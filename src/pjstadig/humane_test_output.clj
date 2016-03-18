(ns pjstadig.humane-test-output
  (:use [clojure.test])
  (:require [clojure.data :as data]
            [clojure.pprint :as pp]))

(defonce activation-body
  (delay
   (when (not (System/getenv "INHUMANE_TEST_OUTPUT"))
     (defmethod report :fail
       [{:keys [type expected actual diffs message] :as event}]
       (with-test-out
         (inc-report-counter :fail)
         (println "\nFAIL in" (testing-vars-str event))
         (when (seq *testing-contexts*) (println (testing-contexts-str)))
         (when message (println message))
         (binding [*out* (pp/get-pretty-writer *out*)]
           (let [diffs (when (and (seq? actual)
                                  (seq actual)
                                  (= 'not (first actual))
                                  (seq (second actual))
                                  (#{'clojure.core/= '=} (first (second actual)))
                                  (< 2 (count (second actual))))
                         (let [a (nth (second actual) 1)
                               more (drop 2 (second actual))]
                           (map vector
                                more
                                (map #(take 2 (data/diff a %)) more))))
                 expected (if (seq diffs)
                            (nth (second actual) 1)
                            expected)
                 print-expected (fn [actual]
                                  (print "expected: ")
                                  (pp/pprint expected)
                                  (print "  actual: ")
                                  (pp/pprint actual))]
             (if (seq diffs)
               (doseq [[actual [a b]] diffs]
                 (print-expected actual)
                 (print "    diff:")
                 (if a
                   (do (print " - ")
                       (pp/pprint a)
                       (print "          + "))
                   (print " + "))
                 (when b
                   (pp/pprint b)))
               (print-expected actual))))))
     ;; this code is just yanked from clojure.pprint
     (defmethod pp/simple-dispatch clojure.lang.IRecord [arec]
       (pp/pprint-logical-block
        :prefix (str "#" (.getName (class arec)) "{") :suffix "}"
        (pp/print-length-loop
         [aseq (seq arec)]
         (when aseq
           (pp/pprint-logical-block
            (pp/write-out (ffirst aseq))
            (.write ^java.io.Writer *out* " ")
            (pp/pprint-newline :linear)
            ;; [pjs] this is kind of ugly, but it is a private var :(
            (.set #'pp/*current-length* 0) ; always print both parts of the [k v] pair
            (pp/write-out (fnext (first aseq))))
           (when (next aseq)
             (.write ^java.io.Writer *out* ", ")
             (pp/pprint-newline :linear)
             (recur (next aseq)))))))
     (prefer-method pp/simple-dispatch
                    clojure.lang.IRecord
                    clojure.lang.IPersistentMap))))

(defn activate! []
  @activation-body)
