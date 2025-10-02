;; Iterator definitions for GCC MicroBlaze machine description files.
;; Copyright (C) 2012-2024 Free Software Foundation, Inc.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

; atomics code iterator
(define_code_iterator any_atomic [plus ior xor and])

; atomics code attribute
(define_code_attr atomic_optab
  [(plus "add") (ior "or") (xor "xor") (and "and")])
