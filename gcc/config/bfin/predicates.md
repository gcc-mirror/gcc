;; Predicate definitions for the Blackfin.
;; Copyright (C) 2005-2019 Free Software Foundation, Inc.
;; Contributed by Analog Devices.
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

;; Return nonzero iff OP is one of the integer constants 1 or 2.
(define_predicate "pos_scale_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 1 || INTVAL (op) == 2")))

;; Return nonzero iff OP is one of the integer constants 2 or 4.
(define_predicate "scale_by_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 2 || INTVAL (op) == 4")))

;; Return nonzero if OP is a constant that consists of two parts; lower
;; bits all zero and upper bits all ones.  In this case, we can perform
;; an AND operation with a sequence of two shifts.  Don't return nonzero
;; if the constant would be cheap to load.
(define_predicate "highbits_operand"
  (and (match_code "const_int")
       (match_test "log2constp (-INTVAL (op)) && !satisfies_constraint_Ks7 (op)")))

;; Return nonzero if OP is suitable as a right-hand side operand for an
;; andsi3 operation.
(define_predicate "rhs_andsi3_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "log2constp (~INTVAL (op)) || INTVAL (op) == 255 || INTVAL (op) == 65535"))))

;; Return nonzero if OP is a register or a constant with exactly one bit
;; set.
(define_predicate "regorlog2_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "log2constp (INTVAL (op))"))))

;; Return nonzero if OP is a register or an integer constant.
(define_predicate "reg_or_const_int_operand"
  (ior (match_operand 0 "register_operand")
       (match_code "const_int")))

(define_predicate "const01_operand"
  (and (match_code "const_int")
       (match_test "op == const0_rtx || op == const1_rtx")))

(define_predicate "const1_operand"
  (and (match_code "const_int")
       (match_test "op == const1_rtx")))

(define_predicate "const3_operand"
  (and (match_code "const_int")
       (match_test "INTVAL (op) == 3")))

(define_predicate "vec_shift_operand"
  (ior (and (match_code "const_int")
	    (match_test "INTVAL (op) >= -16 && INTVAL (op) < 15"))
       (match_operand 0 "register_operand")))

;; Like register_operand, but make sure that hard regs have a valid mode.
(define_predicate "valid_reg_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (REGNO (op) < FIRST_PSEUDO_REGISTER)
    return targetm.hard_regno_mode_ok (REGNO (op), mode);
  return 1;
})

;; Return nonzero if OP is a D register.
(define_predicate "d_register_operand"
  (and (match_code "reg")
       (match_test "D_REGNO_P (REGNO (op))")))

(define_predicate "p_register_operand"
  (and (match_code "reg")
       (match_test "P_REGNO_P (REGNO (op))")))

(define_predicate "dp_register_operand"
  (and (match_code "reg")
       (match_test "D_REGNO_P (REGNO (op)) || P_REGNO_P (REGNO (op))")))

;; Return nonzero if OP is a LC register.
(define_predicate "lc_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == REG_LC0 || REGNO (op) == REG_LC1")))

;; Return nonzero if OP is a LT register.
(define_predicate "lt_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == REG_LT0 || REGNO (op) == REG_LT1")))

;; Return nonzero if OP is a LB register.
(define_predicate "lb_register_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == REG_LB0 || REGNO (op) == REG_LB1")))

;; Return nonzero if OP is a register or a 7-bit signed constant.
(define_predicate "reg_or_7bit_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "satisfies_constraint_Ks7 (op)"))))

;; Return nonzero if OP is a register other than DREG and PREG.
(define_predicate "nondp_register_operand"
  (match_operand 0 "register_operand")
{
  unsigned int regno;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  regno = REGNO (op);
  return (regno >= FIRST_PSEUDO_REGISTER || !DP_REGNO_P (regno));
})

;; Return nonzero if OP is a register other than DREG and PREG, or MEM.
(define_predicate "nondp_reg_or_memory_operand"
  (ior (match_operand 0 "nondp_register_operand")
       (match_operand 0 "memory_operand")))

;; Return nonzero if OP is a register or, when negated, a 7-bit signed
;; constant.
(define_predicate "reg_or_neg7bit_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "satisfies_constraint_KN7 (op)"))))

;; Used for secondary reloads, this function returns 1 if OP is of the
;; form (plus (fp) (const_int)).
(define_predicate "fp_plus_const_operand"
  (match_code "plus")
{
  rtx op1, op2;

  op1 = XEXP (op, 0);
  op2 = XEXP (op, 1);
  return (REG_P (op1)
	  && (REGNO (op1) == FRAME_POINTER_REGNUM
	      || REGNO (op1) == STACK_POINTER_REGNUM)
	  && GET_CODE (op2) == CONST_INT);
})

;; Returns 1 if OP is a symbolic operand, i.e. a symbol_ref or a label_ref,
;; possibly with an offset.
(define_predicate "symbolic_operand"
  (ior (match_code "symbol_ref,label_ref")
       (and (match_code "const")
	    (match_test "GET_CODE (XEXP (op,0)) == PLUS
			 && (GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
			     || GET_CODE (XEXP (XEXP (op, 0), 0)) == LABEL_REF)
			 && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT"))))

;; Returns 1 if OP is a plain constant or matched by symbolic_operand.
(define_predicate "symbolic_or_const_operand"
  (ior (match_code "const_int,const_double")
       (match_operand 0 "symbolic_operand")))

;; Returns 1 if OP is a SYMBOL_REF.
(define_predicate "symbol_ref_operand"
  (match_code "symbol_ref"))

;; True for any non-virtual or eliminable register.  Used in places where
;; instantiation of such a register may cause the pattern to not be recognized.
(define_predicate "register_no_elim_operand"
  (match_operand 0 "register_operand")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return !(op == arg_pointer_rtx
	   || op == frame_pointer_rtx
	   || (REGNO (op) >= FIRST_PSEUDO_REGISTER
	       && REGNO (op) <= LAST_VIRTUAL_REGISTER));
})

;; Test for an operator valid in a BImode conditional branch
(define_predicate "bfin_bimode_comparison_operator"
  (match_code "eq,ne"))

;; Test for an operator whose result is accessible with movbisi.
(define_predicate "bfin_direct_comparison_operator"
  (match_code "eq,lt,le,leu,ltu"))

;; The following three are used to compute the addrtype attribute.  They return
;; true if passed a memory address usable for a 16-bit load or store using a
;; P or I register, respectively.  If neither matches, we know we have a
;; 32-bit instruction.
;; We subdivide the P case into normal P registers, and SP/FP.  We can assume
;; that speculative loads through SP and FP are no problem, so this has
;; an effect on the anomaly workaround code.

(define_predicate "mem_p_address_operand"
  (match_code "mem")
{
  if (effective_address_32bit_p (op, mode))
    return 0;
  op = XEXP (op, 0);
  if (GET_CODE (op) == PLUS || GET_RTX_CLASS (GET_CODE (op)) == RTX_AUTOINC)
    op = XEXP (op, 0);
  gcc_assert (REG_P (op));
  return PREG_P (op) && op != stack_pointer_rtx && op != frame_pointer_rtx;
})

(define_predicate "mem_spfp_address_operand"
  (match_code "mem")
{
  if (effective_address_32bit_p (op, mode))
    return 0;
  op = XEXP (op, 0);
  if (GET_CODE (op) == PLUS || GET_RTX_CLASS (GET_CODE (op)) == RTX_AUTOINC)
    op = XEXP (op, 0);
  gcc_assert (REG_P (op));
  return op == stack_pointer_rtx || op == frame_pointer_rtx;
})

(define_predicate "mem_i_address_operand"
  (match_code "mem")
{
  if (effective_address_32bit_p (op, mode))
    return 0;
  op = XEXP (op, 0);
  if (GET_CODE (op) == PLUS || GET_RTX_CLASS (GET_CODE (op)) == RTX_AUTOINC)
    op = XEXP (op, 0);
  gcc_assert (REG_P (op));
  return IREG_P (op);
})

(define_predicate "push_multiple_operation"
  (and (match_code "parallel")
       (match_test "analyze_push_multiple_operation (op)")))

(define_predicate "pop_multiple_operation"
  (and (match_code "parallel")
       (match_test "analyze_pop_multiple_operation (op)")))
