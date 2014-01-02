;; GCC machine description for picochip
;; Copyright (C) 2008-2014 Free Software Foundation, Inc.
;; Contributed by Picochip Ltd (http://www.picochip.com)
;; Maintained by Daniel Towner (dant@picochip.com) and Hariharan
;; Sandanagobalane (hariharan@picochip.com)
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
;; along with GCC; see the file COPYING3.  If not, see
;; <http://www.gnu.org/licenses/>.

(define_predicate "picochip_register_or_immediate_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "immediate_operand")))

(define_predicate "power_of_2_imm_operand"
  (match_code "const_int")
{
  if (GET_CODE (op) == CONST_INT)
    {
      if (exact_log2 (INTVAL (op)) <= 16 && exact_log2 (INTVAL (op)) > 0)
        return 1;
    }

  return 0;
})

;; Limit the comparison operators to a selected subset.
(define_predicate "picochip_supported_comparison_operator"
  (and (match_operand 0 "comparison_operator")
       (match_code "ne,eq,ge,geu,lt,ltu")))
(define_predicate "picochip_peephole_comparison_operator"
  (and (match_operand 0 "comparison_operator")
       (match_code "ne,eq")))

;; Allow selected arithmetic operators to apply a shift to their first
;; operands

(define_predicate "picochip_first_op_shift_operator"
  (match_code "and,plus,minus,ior,xor"))

;; The same as the previous predicate, but only allowing those
;; operators which can accept an immediate.
(define_predicate "picochip_first_op_shift_operator_imm"
  (match_code "plus,minus"))

;; Predicate on a J type integer.
(define_predicate "picochip_J_operand"
  (match_operand 0 "immediate_operand")
  {
    return (CONST_INT == GET_CODE(op) &&
            picochip_const_ok_for_letter_p (INTVAL(op), 'J'));
  })

;; Is the operand suitable for use in a compare?

(define_predicate "picochip_comparison_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "immediate_operand")
            (match_test "picochip_const_ok_for_letter_p(INTVAL(op), 'O')"))))

