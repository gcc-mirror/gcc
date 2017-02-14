;; Predicate definitions for FT32
;; Copyright (C) 2015-2017 Free Software Foundation, Inc.
;; Contributed by FTDI <support@ftdi.com>

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; Predicates
;; -------------------------------------------------------------------------

;; Nonzero if OP can be source of a simple move operation.

(define_predicate "ft32_general_movsrc_operand"
  (match_code "mem,const_int,reg,subreg,symbol_ref,label_ref,const")
{
  /* Any (MEM LABEL_REF) is OK.  That is a pc-relative load.  */
  if (MEM_P (op) && GET_CODE (XEXP (op, 0)) == LABEL_REF)
    return 1;

  if (MEM_P (op)
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == REG
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST)
    return 1;

  return general_operand (op, mode);
})

(define_predicate "ft32_general_movdst_operand"
  (match_code "mem,const_int,reg,subreg,symbol_ref,label_ref,const")
{
  if (MEM_P (op)
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 0)) == REG
      && CONST_INT_P (XEXP (XEXP (op, 0), 1)))
    return 1;
  if (MEM_P (op) && GET_CODE (XEXP (op, 0)) == SYMBOL_REF)
    return 1;
  return REG_P(op) ||
         (MEM_P(op) && REG_P(XEXP (op, 0)));
})

(define_predicate "reg_indirect"
  (match_code "mem")
{
  return (MEM_P(op) && REG_P(XEXP (op, 0)));
})

;; Nonzero if OP can be an operand to an add/inc/dec instruction.

(define_predicate "ft32_add_operand"
  (ior (match_code "reg")
       (and (match_code "const_int")
            (match_test "IN_RANGE (INTVAL (op), -32768, 32767)"))))

;; Nonzero if OP can be an operand to an sub/dec instruction.

(define_predicate "ft32_sub_operand"
  (ior (match_code "reg")
       (and (match_code "const_int")
            (match_test "IN_RANGE (INTVAL (op), -32767, 32768)"))))


(define_predicate "ft32_rimm_operand"
  (ior (match_code "reg")
       (and (match_code "const_int")
            (match_test "IN_RANGE (INTVAL (op), -512, 511)"))))

(define_predicate "ft32_imm_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), -512, 511)")))

(define_predicate "ft32_bwidth_operand"
  (and (match_code "const_int")
       (match_test "IN_RANGE (INTVAL (op), 1, 16)")))
