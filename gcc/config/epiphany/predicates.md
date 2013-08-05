;; Predicate definitions for code generation on the EPIPHANY cpu.
;; Copyright (C) 1994-2013 Free Software Foundation, Inc.
;; Contributed by Embecosm on behalf of Adapteva, Inc.
;;
;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Returns true iff OP is a symbol reference that is a valid operand
;; in a jump or call instruction.

(define_predicate "symbolic_operand"
  (match_code "symbol_ref,label_ref,const")
{
  if (GET_CODE (op) == SYMBOL_REF)
    return (!epiphany_is_long_call_p (op)
	    && (!flag_pic || SYMBOL_REF_LOCAL_P (op)));
  if (GET_CODE (op) == LABEL_REF)
    return true;
  if (GET_CODE (op) == CONST)
    {
      op = XEXP (op, 0);
      if (GET_CODE (op) != PLUS || !symbolic_operand (XEXP (op, 0), mode))
	return false;
      /* The idea here is that a 'small' constant offset should be OK.
	 What exactly is considered 'small' is a bit arbitrary.  */
      return satisfies_constraint_L (XEXP (op, 1));
    }
  gcc_unreachable ();
})

;; Acceptable arguments to the call insn.

(define_predicate "call_address_operand"
  (ior (match_code "reg")
       (match_operand 0 "symbolic_operand")))

(define_predicate "call_operand"
  (match_code "mem")
{
  op = XEXP (op, 0);
  return call_address_operand (op, mode);
})

;; general purpose register.
(define_predicate "gpr_operand"
  (match_code "reg,subreg")
{
  int regno;

  if (!register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = XEXP (op, 0);
  regno = REGNO (op);
  return regno >= FIRST_PSEUDO_REGISTER || regno <= 63;
})

(define_special_predicate "any_gpr_operand"
  (match_code "subreg,reg")
{
  return gpr_operand (op, mode);
})

;; register suitable for integer add / sub operations; besides general purpose
;; registers we allow fake hard registers that are eliminated to a real
;; hard register via an offset.
(define_predicate "add_reg_operand"
  (match_code "reg,subreg")
{
  int regno;

  if (!register_operand (op, mode))
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = XEXP (op, 0);
  regno = REGNO (op);
  return (regno >= FIRST_PSEUDO_REGISTER || regno <= 63
	  || regno == FRAME_POINTER_REGNUM
	  || regno == ARG_POINTER_REGNUM);
})

;; Also allows suitable constants
(define_predicate "add_operand"
  (match_code "reg,subreg,const_int,symbol_ref,label_ref,const")
{
  if (GET_CODE (op) == REG || GET_CODE (op) == SUBREG)
    return add_reg_operand (op, mode);
  return satisfies_constraint_L (op) || satisfies_constraint_CnL (op);
})

;; Ordinary 3rd operand for arithmetic operations
(define_predicate "arith_operand"
  (match_code "reg,subreg,const_int,symbol_ref,label_ref,const")
{
  if (GET_CODE (op) == REG || GET_CODE (op) == SUBREG)
    return register_operand (op, mode);
  return satisfies_constraint_L (op);
})

;; Constant integer 3rd operand for arithmetic operations
(define_predicate "arith_int_operand"
  (match_code "const_int,symbol_ref,label_ref,const")
{
  return satisfies_constraint_L (op);
})

;; Return true if OP is an acceptable argument for a single word move source.

(define_predicate "move_src_operand"
  (match_code
   "symbol_ref,label_ref,const,const_int,const_double,reg,subreg,mem,unspec")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF :
    case LABEL_REF :
    case CONST :
      return 1;
    case CONST_INT :
      return immediate_operand (op, mode);
    case CONST_DOUBLE :
      /* SImode constants should always fit into a CONST_INT.  Large
	 unsigned 32-bit constants are represented as negative CONST_INTs.  */
      gcc_assert (GET_MODE (op) != SImode);
      /* We can handle 32-bit floating point constants.  */
      if (mode == SFmode)
	return GET_MODE (op) == SFmode;
      return 0;
    case REG :
      return op != frame_pointer_rtx && register_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	return address_operand (XEXP (SUBREG_REG (op), 0), mode);
      else
	return register_operand (op, mode);
    case MEM :
      return address_operand (XEXP (op, 0), mode);
    case UNSPEC:
      return satisfies_constraint_Sra (op);
    default :
      return 0;
    }
})

;; Return true if OP is an acceptable argument for a double word move source.

(define_predicate "move_double_src_operand"
  (match_code "reg,subreg,mem,const_int,const_double,const_vector")
{
  if (GET_CODE (op) == MEM && misaligned_operand (op, mode)
      && !address_operand (plus_constant (Pmode, XEXP (op, 0), 4), SImode))
    return 0;
  return general_operand (op, mode);
})

;; Return true if OP is an acceptable argument for a move destination.

(define_predicate "move_dest_operand"
  (match_code "reg,subreg,mem")
{
  switch (GET_CODE (op))
    {
    case REG :
      return register_operand (op, mode);
    case SUBREG :
      /* (subreg (mem ...) ...) can occur here if the inner part was once a
	 pseudo-reg and is now a stack slot.  */
      if (GET_CODE (SUBREG_REG (op)) == MEM)
	{
	  return address_operand (XEXP (SUBREG_REG (op), 0), mode);
	}
      else
	{
	  return register_operand (op, mode);
	}
    case MEM :
      if (GET_MODE_SIZE (mode) == 8 && misaligned_operand (op, mode)
	  && !address_operand (plus_constant (Pmode, XEXP (op, 0), 4), SImode))
	return 0;
      return address_operand (XEXP (op, 0), mode);
    default :
      return 0;
    }
})

(define_special_predicate "stacktop_operand"
  (match_code "mem")
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return false;
  return rtx_equal_p (XEXP (op, 0), stack_pointer_rtx);
})

;; Return 1 if OP is a comparison operator valid for the mode of CC.
;; This allows the use of MATCH_OPERATOR to recognize all the branch insns.
;;
;; Some insns only set a few bits in the condition code.  So only allow those
;; comparisons that use the bits that are valid.

(define_predicate "proper_comparison_operator"
  (match_code "eq, ne, le, lt, ge, gt, leu, ltu, geu, gtu, unordered, ordered, uneq, unge, ungt, unle, unlt, ltgt")
{
  enum rtx_code code = GET_CODE (op);
  rtx cc = XEXP (op, 0);

  /* combine can try strange things.  */
  if (!REG_P (cc))
    return 0;
  switch (GET_MODE (cc))
    {
    case CC_Zmode:
    case CC_N_NEmode:
    case CC_FP_EQmode:
      return REGNO (cc) == CC_REGNUM && (code == EQ || code == NE);
    case CC_C_LTUmode:
      return REGNO (cc) == CC_REGNUM && (code == LTU || code == GEU);
    case CC_C_GTUmode:
      return REGNO (cc) == CC_REGNUM && (code == GTU || code == LEU);
    case CC_FPmode:
      return (REGNO (cc) == CCFP_REGNUM
	      && (code == EQ || code == NE || code == LT || code == LE));
    case CC_FP_GTEmode:
      return (REGNO (cc) == CC_REGNUM
	      && (code == EQ || code == NE || code == GT || code == GE
		  || code == UNLE || code == UNLT));
    case CC_FP_ORDmode:
      return REGNO (cc) == CC_REGNUM && (code == ORDERED || code == UNORDERED);
    case CC_FP_UNEQmode:
      return REGNO (cc) == CC_REGNUM && (code == UNEQ || code == LTGT);
    case CCmode:
      return REGNO (cc) == CC_REGNUM;
    /* From combiner.  */
    case QImode: case SImode: case SFmode: case HImode:
    /* From cse.c:dead_libcall_p.  */
    case DFmode:
      return 0;
    default:
      gcc_unreachable ();
    }
})

(define_predicate "addsub_operator"
  (match_code "plus, minus"))

(define_predicate "cc_operand"
  (and (match_code "reg")
       (match_test "REGNO (op) == CC_REGNUM || REGNO (op) == CCFP_REGNUM")))

(define_predicate "const0_operand"
  (match_code "const_int, const_double")
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);
  return op == CONST0_RTX (mode);
})

(define_predicate "const_float_1_operand"
  (match_code "const_double")
{
  return op == CONST1_RTX (mode);
})

(define_predicate "cc_move_operand"
  (and (match_code "reg")
       (ior (match_test "REGNO (op) == CC_REGNUM")
	    (match_test "gpr_operand (op, mode)"))))

(define_predicate "float_operation"
  (match_code "parallel")
{
  /* Most patterns start out with one SET and one CLOBBER, and gain a USE
     or two of FP_NEAREST_REGNUM / FP_TRUNCATE_REGNUM / FP_ANYFP_REGNUM
     after mode switching.  The longer patterns are
     all beyond length 4, and before mode switching, end with a
     CLOBBER of CCFP_REGNUM.  */
  int count = XVECLEN (op, 0);
  bool inserted = MACHINE_FUNCTION (cfun)->control_use_inserted;
  int i;

  if (count == 2
      /* Vector ashift has an extra use for the constant factor required to
	 implement the shift as multiply.  */
      || (count == 3 && GET_CODE (XVECEXP (op, 0, 0)) == SET
	  && GET_CODE (XEXP (XVECEXP (op, 0, 0), 1)) == ASHIFT))
    return !inserted;

  /* combine / recog will pass any old garbage here before checking the
     rest of the insn.  */
  if (count <= 3)
    return false;

  i = 1;
  if (count > 4)
    for (i = 2; i < count; i++)
      {
	rtx x = XVECEXP (op, 0, i);

	if (GET_CODE (x) == CLOBBER)
	  {
	    if (!REG_P (XEXP (x, 0)))
	      return false;
	    if (REGNO (XEXP (x, 0)) == CCFP_REGNUM)
	      {
		if (count == i + 1)
		  return !inserted;
		break;
	    }
	  /* Just an ordinary clobber, keep looking.  */
	}
      else if (GET_CODE (x) == USE
	       || (GET_CODE (x) == SET && i == 2))
	continue;
      else
	return false;
    }
  if (count != i + 3 || !inserted)
    return false;
  for (i = i+1; i < count; i++)
    {
      rtx x = XVECEXP (op, 0, i);

      if (GET_CODE (x) != USE && GET_CODE (x) != CLOBBER)
	return false;
      x = XEXP (x, 0);
      if (!REG_P (x)
	  || (REGNO (x) != FP_NEAREST_REGNUM
	      && REGNO (x) != FP_TRUNCATE_REGNUM
	      && REGNO (x) != FP_ANYFP_REGNUM))
	return false;
    }
  return true;
})

(define_predicate "set_fp_mode_operand"
  (ior (match_test "gpr_operand (op, mode)")
       (and (match_code "const")
	    (match_test "satisfies_constraint_Cfm (op)"))))

(define_predicate "post_modify_address"
  (match_code "post_modify,post_inc,post_dec"))

(define_predicate "post_modify_operand"
  (and (match_code "mem")
       (match_test "post_modify_address (XEXP (op, 0), Pmode)")))

(define_predicate "nonsymbolic_immediate_operand"
  (ior (match_test "immediate_operand (op, mode)")
       (match_code "const_vector"))) /* Is this specific enough?  */

;; Return true if OP is misaligned memory operand
(define_predicate "misaligned_operand"
  (and (match_code "mem")
       (match_test "MEM_ALIGN (op) < GET_MODE_ALIGNMENT (mode)")))
