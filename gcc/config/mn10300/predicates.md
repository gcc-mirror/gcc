;; Predicate definitions for Matsushita MN10300.
;; Copyright (C) 2005-2019 Free Software Foundation, Inc.
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

;; Return true if the operand is the 1.0f constant.

(define_predicate "const_1f_operand"
  (match_code "const_int,const_double")
{
  return (op == CONST1_RTX (SFmode));
})

;; Return true if OP is a valid call operand.

(define_predicate "call_address_operand"
  (match_code "symbol_ref,reg,unspec")
{
  if (flag_pic)
    return (satisfies_constraint_S (op) || GET_CODE (op) == REG);

  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == REG);
})

(define_predicate "impossible_plus_operand"
  (match_code "plus")
{
  return XEXP (op, 0) == stack_pointer_rtx
      || XEXP (op, 1) == stack_pointer_rtx;
})

(define_predicate "reg_or_am33_const_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_test "TARGET_AM33")
	    (match_operand 0 "immediate_operand"))))

(define_predicate "label_ref_operand"
  (match_code "label_ref"))

(define_special_predicate "int_mode_flags"
  (match_code "reg")
{
  if (REGNO (op) != CC_REG)
    return false;
  if (GET_MODE (op) == CC_FLOATmode)
    return false;
  return GET_MODE_CLASS (GET_MODE (op)) == MODE_CC;
})

(define_predicate "CCZN_comparison_operator"
  (match_code "eq,ne,lt,ge"))

(define_predicate "liw_operand"
  (ior (match_operand 0 "register_operand")
       (match_test "satisfies_constraint_O (op)")))

(define_predicate "mn10300_store_multiple_operation"
  (and (match_code "parallel")
       (match_test "mn10300_store_multiple_regs (op) != 0")))
