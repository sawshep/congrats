(defun one-of (x)
  "Returns a random item of list X"
  (nth (random (length x)) x))

(defparameter *grammar1*
  '((sentence (np vp))
    (np (article adj* noun pp*) (name) (pronoun))
    (vp (verb np pp*))
    (pp* () (pp pp*))
    (adj* () (adj adj*))
    (pp (prep np))
    (prep to in by with on)
    (adj big little blue green smelly)
    (article a the)
    (name pat kim lee terry robin)
    (noun man ball woman table)
    (verb hit took saw liked)
    (foo (article noun))
    (pronoun he she it these those that)))

(defvar *grammar* *grammar1*)

(defun constituents (phrase)
  "Returns the constituents of a grammar structure"
  (rest (assoc phrase *grammar*)))

;;; Kleene star: Represents any number of an item.
(defun kleene-starp (phrase)
  "Returns true if a grammar structure follows the Kleene start pattern"
  (and
    (member nil (constituents phrase))
    (find-if
      #'(lambda (x) (member phrase x))
      (constituents phrase))))

(defun empty-phrasep (phrase)
  "Returns true if a phrase is empty"
  (and (listp phrase) (equal (rest phrase) '(nil))))

(defun generate-tree (phrase)
  (cons phrase
	(let ((choice (one-of (constituents phrase))))
	  (if (atom choice)
	    (list choice)
	    (mapcar #'generate-tree choice)))))

(defun clean-tree (tree)
  "Prunes empty word and phrases from a grammar tree"
  (if (atom tree)
    tree
    (remove-if #'empty-phrasep (mapcar #'clean-tree tree))))

(defun normalize (tree)
  "Flattens a grammar tree into a normal sentence"
  (if (atom tree)
    (list tree)
    (reduce #'append (mapcar #'normalize (rest tree)))))
