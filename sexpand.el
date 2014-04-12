(setq open-delim '("(" "{" "["))
(setq close-delim '(")" "}" "]"))

(defun get-curr-char ()
  (let (pt)
    (setq pt (point))
    (buffer-substring pt (+ pt 1))))
 
(defun expand-in-direction (matching-delims other-delims while-cond movement-fn)
  (let (n flag last-delim-pos)
    (if (member (get-curr-char) other-delims)
      (setq n 0)
      (setq n 1))
    (while (and (not flag) (funcall while-cond))
      (if (member (get-curr-char) other-delims)
	  (setq n (+ n 1)))
      (if (member (get-curr-char) matching-delims)
	  (progn
	    (setq n (- n 1))
	    (setq last-delim-pos (point))))
      (if (and (member (get-curr-char) matching-delims) (= n 0))
	  (setq flag t)
	  (funcall movement-fn)))
    (if (not flag)
	(goto-char last-delim-pos))))

(defun sexpand ()
  (interactive)
  (let (new-beginning new-end pt)    
    (setq pt (point))
    (expand-in-direction close-delim open-delim 
			 (lambda () (< (point) (point-max)))
			 'forward-char)	; go right
    (setq new-end (point))
    (goto-char pt)
    (expand-in-direction open-delim close-delim
			 (lambda () (> (point) (point-min)))
			 'backward-char) ; go left
    (setq new-beginning (point))
    (goto-char (+ new-end 1))
    (push-mark new-beginning)
    (setq mark-active t)))

(provide 'sexpand)
