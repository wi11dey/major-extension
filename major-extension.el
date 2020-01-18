;;; major-extension.el --- summary -*- lexical-binding: t -*-

;; Author: Will Dey
;; Maintainer: Will Dey
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage: https://github.com/wi11dey/major-extension
;; Keywords: keywords

;; This file is not (yet) part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;; Generate README:
;;; Commentary:

;; Does basic regex parsing of `auto-mode-alist' to try and guess an appropriate file extension for the mode MODE. If it cannot figure out an extension, nil is returned.

;;; Code:

(defun major-extension (&optional mode)
  "Does basic regex parsing of `auto-mode-alist' to try and guess an appropriate file extension for the mode MODE. If it cannot figure out an extension, nil is returned."
  (unless mode
    (setq mode major-mode))
  (let (candidates
	(shortest-length most-positive-fixnum)
	(mode-symbol-name (symbol-name mode))
	mode-name)
    (save-match-data
      (dolist (pair auto-mode-alist)
	(when (and (eq (cdr pair) mode)
		   (string-match "\\`\\\\\\.\\(?1:[][?[:alnum:]]*\\)\\\\'" (car pair)))
	  (setq extension (match-string 1 (car pair))
		extension (replace-regexp-in-string "\\[\\(?1:.\\).*?\\]" "\\1" extension)
		extension (replace-regexp-in-string ".\\?" "" extension))
	  (when (<= (length extension) shortest-length)
	    (setq shortest-length (length extension))
	    (push (list extension) candidates))))
      (when candidates
	(when (string-match "\\`\\(?1:.*\\)-mode\\'" mode-symbol-name)
	  (setq mode-name (match-string 1 mode-symbol-name))
	  (dolist (candidate candidates)
	    (setcdr candidate (string-distance (car candidate) mode-name)))
	  (setq candidates (sort candidates
				 (lambda (a b)
				   (< (cdr a) (cdr b))))))
	(caar candidates)))))

(provide 'major-extension)
