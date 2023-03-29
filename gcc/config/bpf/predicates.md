;; Predicate definitions for eBPF.
;; Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

(define_predicate "reg_or_imm_operand"
  (ior (and (match_code "const_int")
            (match_test "IN_RANGE (INTVAL (op), -1 - 0x7fffffff, 0x7fffffff)"))
       (match_operand 0 "register_operand")))

(define_predicate "imm32_operand"
  (ior (and (match_code "const_int")
            (match_test "IN_RANGE (INTVAL (op), 0, 0xffffffff)"))
       (match_code "symbol_ref,label_ref,const")))

(define_predicate "lddw_operand"
  (match_code "symbol_ref,label_ref,const,const_double,const_int"))

(define_predicate "call_operand"
  (match_code "reg,symbol_ref,const_int,const")
{
  if (GET_CODE (op) == CONST)
    {
      op = XEXP (op, 0);

      switch (GET_CODE (op))
	{
	case SYMBOL_REF:
	case LABEL_REF:
	case CONST_INT:
	  return true;
	  break;
	default:
	  break;
	}

      return false;
    }

  return true;
})

(define_predicate "mov_src_operand"
  (ior (match_operand 0 "memory_operand")
       (match_operand 0 "reg_or_imm_operand")
       (match_operand 0 "lddw_operand")))

(define_predicate "register_compare_operator"
  (match_code "eq,ne,geu,gtu,ge,gt"))
