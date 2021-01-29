(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (if (empty? a-seq)
    '()
    (rest (reduce (fn [a b] (conj a x b)) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (concat [b] a)) () a-seq))

(defn min-max-element [a-seq]
  (let [mmf (fn [[min_v max_v] a]
        (if (= min_v max_v nil)
          [a a]
          (if (< a min_v)
            [a max_v]
            (if (> a max_v)
              [min_v a]
              [min_v max_v]))))
        f (first a-seq)]
    (reduce mmf [f f] a-seq)))


(defn insert [sorted-seq n]
  (loop [res ()
         seq1 sorted-seq]
    (if (empty? seq1)
      (concat res [n])
      (let [f (first seq1)]
        (if (>= f n)
          (concat res [n] seq1)
          (recur (concat res [f]) (rest seq1)))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (reduce
    (fn [res a]
      (if (contains? res a)
        (disj res a)
        (conj res a)))
    #{}
    a-seq))

(defn minus
  ([x] (minus 0 x))
  ([x y] (- x y)))

(defn count-params
  ([] 0)
  ([x] 1)
  ([x & more]
   (reduce (fn [acc v] (inc acc)) 1 more)))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([xx & more] (fn [x]
                 (reduce
                   (fn [acc v]
                     (and acc (v x)))
                   (xx x)
                   more))))


(defn my-transpose-vector [x]
  (reduce (fn [acc x] (conj acc [x])) [] x))

(defn my-sum-vector-arr [a b]
  (if (not= (count a) (count b))
    (throw (Exception. "wrong dimensions"))
    (loop [res []
           x a
           y (my-transpose-vector b)]
      (if (empty? x)
        res
        (recur (concat res [(concat (first x) (first y))])
               (rest x)
               (rest y))))))

(defn my-transpose-matrix [a-seq more]
  (reduce my-sum-vector-arr (my-transpose-vector a-seq) more))

(defn my-map
  ([f a-seq]
   (reduce (fn [acc v]
             (concat acc [(f v)]))
           '()
           a-seq))
  ([f a-seq & more] (reduce
                      (fn [acc x]
                        (concat acc [(apply f x)]))
                      []
                      (my-transpose-matrix a-seq more))))
;--

(defn sum [a-seq]
  (if (empty? a-seq)
    0
    (+ (first a-seq) (sum (rest a-seq)))))

(defn sum2 [a-seq]
  (let [sum-helper (fn [acc a-seq]
                     (if (empty? a-seq)
                       acc
                       (recur (+ acc (first a-seq))
                              (rest a-seq))))]
    (sum-helper 0 a-seq)))

(defn seq-min [a-seq]
  (reduce min a-seq))

(defn count-elem [elem a-seq]
  (let [counter (fn [count e]
                  (if (= e elem)
                    (inc count)
                    count))]
    (reduce counter 0 a-seq)))


(defn one-or-two
  ([x] 1)
  ([x y] 2))

(defn my-max
  ([x] x)
  ([x y] (if (> x y) x y))
  ([x y & more]
   (reduce my-max (my-max x y) more)))

