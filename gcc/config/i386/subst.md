;; GCC machine description for AVX512F instructions
;; Copyright (C) 2013 Free Software Foundation, Inc.
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

;; Some iterators for extending subst as much as possible
;; All vectors (Use it for destination)
(define_mode_iterator SUBST_V
  [V16QI
   V16HI V8HI
   V16SI V8SI  V4SI
   V8DI  V4DI  V2DI
   V16SF V8SF  V4SF
   V8DF  V4DF  V2DF])

(define_mode_iterator SUBST_S
  [QI HI SI DI])

(define_subst_attr "mask_name" "mask" "" "_mask")
(define_subst_attr "mask_applied" "mask" "false" "true")
(define_subst_attr "mask_operand2" "mask" "" "%{%3%}%N2")
(define_subst_attr "mask_operand3" "mask" "" "%{%4%}%N3")
(define_subst_attr "mask_operand3_1" "mask" "" "%%{%%4%%}%%N3") ;; for sprintf
(define_subst_attr "mask_operand4" "mask" "" "%{%5%}%N4")
(define_subst_attr "mask_operand6" "mask" "" "%{%7%}%N6")
(define_subst_attr "mask_operand11" "mask" "" "%{%12%}%N11")
(define_subst_attr "mask_operand18" "mask" "" "%{%19%}%N18")
(define_subst_attr "mask_operand19" "mask" "" "%{%20%}%N19")
(define_subst_attr "mask_codefor" "mask" "*" "")
(define_subst_attr "mask_mode512bit_condition" "mask" "1" "(GET_MODE_SIZE (GET_MODE (operands[0])) == 64)")
(define_subst_attr "store_mask_constraint" "mask" "vm" "v")
(define_subst_attr "store_mask_predicate" "mask" "nonimmediate_operand" "register_operand")
(define_subst_attr "mask_prefix" "mask" "vex" "evex")
(define_subst_attr "mask_prefix2" "mask" "maybe_vex" "evex")
(define_subst_attr "mask_prefix3" "mask" "orig,vex" "evex")

(define_subst "mask"
  [(set (match_operand:SUBST_V 0)
        (match_operand:SUBST_V 1))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
        (vec_merge:SUBST_V
	  (match_dup 1)
	  (match_operand:SUBST_V 2 "vector_move_operand" "0C")
	  (match_operand:<avx512fmaskmode> 3 "register_operand" "k")))])

(define_subst_attr "mask_scalar_merge_name" "mask_scalar_merge" "" "_mask")
(define_subst_attr "mask_scalar_merge_operand3" "mask_scalar_merge" "" "%{%3%}")
(define_subst_attr "mask_scalar_merge_operand4" "mask_scalar_merge" "" "%{%4%}")

(define_subst "mask_scalar_merge"
  [(set (match_operand:SUBST_S 0)
        (match_operand:SUBST_S 1))]
  "TARGET_AVX512F"
  [(set (match_dup 0)
        (and:SUBST_S
	  (match_dup 1)
	  (match_operand:SUBST_S 3 "register_operand" "k")))])
