/* Subroutines for insn-output.c for Pyramid 90x, 9000, and MIServer Series.
   Copyright (C) 1989, 1991, 1997 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Some output-actions in pyr.md need these.  */
#include "config.h"
#include <stdio.h>
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "tree.h"

/*
 * Do FUNCTION_ARG.
 * This cannot be defined as a macro on pyramids, because Pyramid Technology's
 * C compiler dies on (several equivalent definitions of) this macro.
 * The only way around this cc bug was to make this a function.
 * While it would be possible to use a macro version for gcc, it seems
 * more reliable to have a single version of the code.
 */
void *
pyr_function_arg(cum, mode, type, named)
  CUMULATIVE_ARGS cum;
  enum machine_mode mode;
  tree type;
{
  return (void *)(FUNCTION_ARG_HELPER (cum, mode,type,named));
}

/* Do the hard part of PARAM_SAFE_FOR_REG_P.
 * This cannot be defined as a macro on pyramids, because Pyramid Technology's
 * C compiler dies on (several equivalent definitions of) this macro.
 * The only way around this cc bug was to make this a function.
 */
int
inner_param_safe_helper (type)
    tree type;
{
  return (INNER_PARAM_SAFE_HELPER(type));
}


/* Return 1 if OP is a non-indexed operand of mode MODE.
   This is either a register reference, a memory reference,
   or a constant.  In the case of a memory reference, the address
   is checked to make sure it isn't indexed.

   Register and memory references must have mode MODE in order to be valid,
   but some constants have no machine mode and are valid for any mode.

   If MODE is VOIDmode, OP is checked for validity for whatever mode
   it has.

   The main use of this function is as a predicate in match_operand
   expressions in the machine description.

   It is  useful to compare this with general_operand().  They should
   be identical except for one line.

   This function seems necessary because of the non-orthogonality of
   Pyramid insns.
   For any 2-operand insn, and any combination of operand modes,
   if indexing is valid for the isn's second operand, it is invalid
   for the first operand to be indexed. */

extern int volatile_ok;

int
nonindexed_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  register RTX_CODE code = GET_CODE (op);
  int mode_altering_drug = 0;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  /* Don't accept CONST_INT or anything similar
     if the caller wants something floating.  */
  if (GET_MODE (op) == VOIDmode && mode != VOIDmode
      && GET_MODE_CLASS (mode) != MODE_INT)
    return 0;

  if (CONSTANT_P (op))
    return ((GET_MODE (op) == VOIDmode || GET_MODE (op) == mode)
	    && LEGITIMATE_CONSTANT_P (op));

  /* Except for certain constants with VOIDmode, already checked for,
     OP's mode must match MODE if MODE specifies a mode.  */

  if (GET_MODE (op) != mode)
    return 0;

  while (code == SUBREG)
    {
      op = SUBREG_REG (op);
      code = GET_CODE (op);
#if 0
      /* No longer needed, since (SUBREG (MEM...))
	 will load the MEM into a reload reg in the MEM's own mode.  */
      mode_altering_drug = 1;
#endif
    }
  if (code == REG)
    return 1;
  if (code == CONST_DOUBLE)
    return LEGITIMATE_CONSTANT_P (op);
  if (code == MEM)
    {
      register rtx y = XEXP (op, 0);
      if (! volatile_ok && MEM_VOLATILE_P (op))
	return 0;
    GO_IF_NONINDEXED_ADDRESS (y, win);
    }
  return 0;

 win:
  if (mode_altering_drug)
    return ! mode_dependent_address_p (XEXP (op, 0));
  return 1;
}

/* Return non-zero if the rtx OP has an immediate component.  An
   immediate component or additive term equal to zero is rejected
   due to assembler problems.  */

int
has_direct_base (op)
     rtx op;
{
  if ((CONSTANT_ADDRESS_P (op)
       && op != const0_rtx)
      || (GET_CODE (op) == PLUS
	  && ((CONSTANT_ADDRESS_P (XEXP (op, 1))
	       && XEXP (op, 1) != const0_rtx)
	      || (CONSTANT_ADDRESS_P (XEXP (op, 0))
		  && XEXP (op, 0) != const0_rtx))))
    return 1;

  return 0;
}

/* Return zero if the rtx OP has a (scaled) index.  */

int
has_index (op)
     rtx op;
{
  if (GET_CODE (op) == PLUS
      && (GET_CODE (XEXP (op, 0)) == MULT
	  || (GET_CODE (XEXP (op, 1)) == MULT)))
    return 1;
  else
    return 0;
}

int swap_operands;

/* weird_memory_memory -- return 1 if OP1 and OP2 can be compared (or
   exchanged with xchw) with one instruction.  If the operands need to
   be swapped, set the global variable SWAP_OPERANDS.  This function
   silently assumes that both OP0 and OP1 are valid memory references.
   */

int
weird_memory_memory (op0, op1)
     rtx op0, op1;
{
  RTX_CODE code0, code1;

  op0 = XEXP (op0, 0);
  op1 = XEXP (op1, 0);
  code0 = GET_CODE (op0);
  code1 = GET_CODE (op1);

  swap_operands = 0;

  if (code1 == REG || code1 == SUBREG)
    {
      return 1;
    }
  if (code0 == REG || code0 == SUBREG)
    {
      swap_operands = 1;
      return 1;
    }
  if (has_direct_base (op0) && has_direct_base (op1))
    {
      if (has_index (op1))
	{
	  if (has_index (op0))
	    return 0;
	  swap_operands = 1;
	}

      return 1;
    }
  return 0;
}

int
signed_comparison (x, mode)
     rtx x;
     enum machine_mode mode;
{
  return ! TRULY_UNSIGNED_COMPARE_P (GET_CODE (x));
}

extern rtx force_reg ();
rtx test_op0, test_op1;
enum machine_mode test_mode;

/* Sign-extend or zero-extend constant X from FROM_MODE to TO_MODE.  */

rtx
extend_const (x, extop, from_mode, to_mode)
    rtx x;
    RTX_CODE extop;
    enum machine_mode from_mode, to_mode;
{
  int val;
  int negative;
  if (from_mode == to_mode)
    return x;
  if (GET_CODE (x) != CONST_INT)
    abort ();
  val = INTVAL (x);
  negative = val & (1 << (GET_MODE_BITSIZE (from_mode) - 1));
  if (GET_MODE_BITSIZE (from_mode) == HOST_BITS_PER_INT)
    abort ();
  if (negative && extop == SIGN_EXTEND)
    val = val | ((-1) << (GET_MODE_BITSIZE (from_mode)));
  else
    val = val & ~((-1) << (GET_MODE_BITSIZE (from_mode)));
  if (GET_MODE_BITSIZE (to_mode) == HOST_BITS_PER_INT)
    return GEN_INT (val);
  return GEN_INT (val & ~((-1) << (GET_MODE_BITSIZE (to_mode))));
}

rtx
ensure_extended (op, extop, from_mode)
     rtx op;
     RTX_CODE extop;
     enum machine_mode from_mode;
{
  if (GET_CODE (op) == CONST_INT)
    return extend_const (op, extop, from_mode, SImode);
  else
    return force_reg (SImode, gen_rtx (extop, SImode, op));
}

/* Emit rtl for a branch, as well as any delayed (integer) compare insns.
   The compare insn to perform is determined by the global variables
   test_op0 and test_op1.  */

void
extend_and_branch (extop)
     RTX_CODE extop;
{
  rtx op0, op1;
  RTX_CODE code0, code1;

  op0 = test_op0, op1 = test_op1;
  if (op0 == 0)
    return;

  code0 = GET_CODE (op0);
  if (op1 != 0)
    code1 = GET_CODE (op1);
  test_op0 = test_op1 = 0;

  if (op1 == 0)
    {
      op0 = ensure_extended (op0, extop, test_mode);
      emit_insn (gen_rtx (SET, VOIDmode, cc0_rtx, op0));
    }
  else
    {
      if (CONSTANT_P (op0) && CONSTANT_P (op1))
	{
	  op0 = ensure_extended (op0, extop, test_mode);
	  op1 = ensure_extended (op1, extop, test_mode);
	}
      else if (extop == ZERO_EXTEND && test_mode == HImode)
	{
	  /* Pyramids have no unsigned "cmphi" instructions.  We need to
	     zero extend unsigned halfwords into temporary registers. */
	  op0 = ensure_extended (op0, extop, test_mode);
	  op1 = ensure_extended (op1, extop, test_mode);
	}
      else if (CONSTANT_P (op0))
	{
	  op0 = ensure_extended (op0, extop, test_mode);
	  op1 = ensure_extended (op1, extop, test_mode);
	}
      else if (CONSTANT_P (op1))
	{
	  op1 = ensure_extended (op1, extop, test_mode);
	  op0 = ensure_extended (op0, extop, test_mode);
	}
      else if ((code0 == REG || code0 == SUBREG)
	       && (code1 == REG || code1 == SUBREG))
	{
	  /* I could do this case without extension, by using the virtual
	     register address (but that would lose for global regs).  */
	  op0 = ensure_extended (op0, extop, test_mode);
	  op1 = ensure_extended (op1, extop, test_mode);
	}
      else if (code0 == MEM && code1 == MEM)
	{
	  /* Load into a reg if the address combination can't be handled
	     directly.  */
	  if (! weird_memory_memory (op0, op1))
	    op0 = force_reg (test_mode, op0);
	}

      emit_insn (gen_rtx (SET, VOIDmode, cc0_rtx,
			  gen_rtx (COMPARE, VOIDmode, op0, op1)));
    }
}

/* Return non-zero if the two single-word moves with operands[0]
   and operands[1] for the first single-word move, and operands[2]
   and operands[3] for the second single-word move, is possible to
   combine to a double word move.

   The criterion is whether the operands are in consecutive memory cells,
   registers, etc.  */

int
movdi_possible (operands)
     rtx operands[];
{
  int cnst_diff0, cnst_diff1;
  RTX_CODE code0 = GET_CODE (operands[0]);
  RTX_CODE code1 = GET_CODE (operands[1]);

  /* Don't dare to combine (possibly overlapping) memory -> memory moves.  */
  /* It would be possible to detect the cases where we dare, by using
     constant_diff (operands[0], operands[1])!!!  */
  if (code0 == MEM && code1 == MEM)
    return 0;

  cnst_diff0 = consecutive_operands (operands[0], operands[2]);
  if (cnst_diff0 == 0)
    return 0;

  cnst_diff1 = consecutive_operands (operands[1], operands[3]);
  if (cnst_diff1 == 0)
    return 0;

  if (cnst_diff0 & cnst_diff1)
    {
      /* The source and destination operands are consecutive.  */

      /* If the first move writes into the source of the second move,
	 we cannot combine.  */
      if ((code0 == REG
	   && reg_overlap_mentioned_p (operands[0], operands[3]))
	  || (code0 == SUBREG
	      && subreg_overlap_mentioned_p (operands[0], operands[3])))
	  return 0;

      if (cnst_diff0 & 1)
	/* operands[0],[1] has higher addresses than operands[2],[3].  */
	swap_operands = 0;
      else
	/* operands[0],[1] has lower addresses than operands[2],[3].  */
	swap_operands = 1;
      return 1;
    }
  return 0;
}

/* Like reg_overlap_mentioned_p, but accepts a subreg rtx instead
   of a reg.  */

int
subreg_overlap_mentioned_p (subreg, x)
     rtx subreg, x;
{
  rtx reg = SUBREG_REG (subreg);
  int regno = REGNO (reg) + SUBREG_WORD (subreg);
  int endregno = regno + HARD_REGNO_NREGS (regno, GET_MODE (subreg));
  return refers_to_regno_p (regno, endregno, x, 0);
}

/* Return 1 if OP0 is a consecutive operand to OP1, 2 if OP1 is a
   consecutive operand to OP0.

   This function is used to determine if addresses are consecutive,
   and therefore possible to combine to fewer instructions.  */

int
consecutive_operands (op0, op1)
     rtx op0, op1;
{
  RTX_CODE code0, code1;
  int cnst_diff;
  int regno_off0, regno_off1;

  code0 = GET_CODE (op0);
  code1 = GET_CODE (op1);

  regno_off0 = 0;
  if (code0 == SUBREG)
    {
      if (GET_MODE_SIZE (GET_MODE (SUBREG_REG (op0))) <= UNITS_PER_WORD)
	return 0;
      regno_off0 = SUBREG_WORD (op0);
      op0 = SUBREG_REG (op0);
      code0 = REG;
    }

  regno_off1 = 0;
  if (code1 == SUBREG)
    {
      if (GET_MODE_SIZE (GET_MODE (SUBREG_REG (op1))) <= UNITS_PER_WORD)
	return 0;
      regno_off1 = SUBREG_WORD (op1);
      op1 = SUBREG_REG (op1);
      code1 = REG;
    }

  if (code0 != code1)
    return 0;

  switch (code0)
    {
    case CONST_INT:
      /* Cannot permit any symbolic constants, even if the consecutive
	 operand is 0, since a movl really performs sign extension.  */
      if (code1 != CONST_INT)
	return 0;
      if ((INTVAL (op0) == 0 && INTVAL (op1) == 0)
	  || (INTVAL (op0) == -1 && INTVAL (op1) == -1))
	return 3;
      if ((INTVAL (op0) == 0 && INTVAL (op1) > 0)
	  || (INTVAL (op0) == -1 && INTVAL (op1) < 0))
	return 2;
      if ((INTVAL (op1) == 0 && INTVAL (op0) > 0)
	  || (INTVAL (op1) == -1 && INTVAL (op0) < 0))
	return 1;
      break;

    case REG:
      regno_off0 = REGNO (op0) + regno_off0;
      regno_off1 = REGNO (op1) + regno_off1;

      cnst_diff = regno_off0 - regno_off1;
      if (cnst_diff == 1)
	{
	  /* movl with the highest numbered parameter (local) register as
	     source or destination, doesn't wrap to the lowest numbered local
	     (temporary) register.  */

	  if (regno_off0 % 16 != 0)
	    return 1;
	  else
	    return 0;
	}
      else if (cnst_diff == -1)
	{
	  if (regno_off1 % 16 != 0)
	    return 2;
	  else
	    return 0;
	}
      break;

    case MEM:
      op0 = XEXP (op0, 0);
      op1 = XEXP (op1, 0);
      if (GET_CODE (op0) == CONST)
	op0 = XEXP (op0, 0);
      if (GET_CODE (op1) == CONST)
	op1 = XEXP (op1, 0);

      cnst_diff = constant_diff (op0, op1);
      if (cnst_diff)
	{
	  if (cnst_diff == 4)
	    return 1;
	  else if (cnst_diff == -4)
	    return 2;
	}
      break;
    }
  return 0;
}

/* Return the constant difference of the rtx expressions OP0 and OP1,
   or 0 if they don't have a constant difference.

   This function is used to determine if addresses are consecutive,
   and therefore possible to combine to fewer instructions.  */

int
constant_diff (op0, op1)
     rtx op0, op1;
{
  RTX_CODE code0, code1;
  int cnst_diff;

  code0 = GET_CODE (op0);
  code1 = GET_CODE (op1);

  if (code0 != code1)
    {
      if (code0 == PLUS)
	{
	  if (GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && rtx_equal_p (op1, XEXP (op0, 0)))
	    return INTVAL (XEXP (op0, 1));
	}
      else if (code1 == PLUS)
	{
	  if (GET_CODE (XEXP (op1, 1)) == CONST_INT
	      && rtx_equal_p (op0, XEXP (op1, 0)))
	    return -INTVAL (XEXP (op1, 1));
	}
      return 0;
    }

  if (code0 == CONST_INT)
    return INTVAL (op0) - INTVAL (op1);

  if (code0 == PLUS)
    {
      cnst_diff = constant_diff (XEXP (op0, 0), XEXP (op1, 0));
      if (cnst_diff)
	return (rtx_equal_p (XEXP (op0, 1), XEXP (op1, 1)))
	  ? cnst_diff : 0;
      cnst_diff = constant_diff (XEXP (op0, 1), XEXP (op1, 1));
      if (cnst_diff)
	return (rtx_equal_p (XEXP (op0, 0), XEXP (op1, 0)))
	  ? cnst_diff : 0;
    }

  return 0;
}

int
already_sign_extended (insn, from_mode, op)
     rtx insn;
     enum machine_mode from_mode;
     rtx op;
{
  rtx xinsn, xdest, xsrc;

  for (;;)
    {
      insn = PREV_INSN (insn);
      if (insn == 0)
	return 0;
      if (GET_CODE (insn) == NOTE || GET_CODE (insn) == JUMP_INSN)
	continue;
      if (GET_CODE (insn) == CALL_INSN && ! call_used_regs[REGNO (op)])
	continue;
      if (GET_CODE (insn) != INSN)
	return 0;
      xinsn = PATTERN (insn);

      if (GET_CODE (xinsn) != SET)
	return 0;

      xdest = SET_DEST (xinsn);
      xsrc = SET_SRC (xinsn);

      if (GET_CODE (xdest) == SUBREG)
	abort ();

      if ( ! REG_P (xdest))
	continue;

      if (REGNO (op) == REGNO (xdest)
	  && ((GET_CODE (xsrc) == SIGN_EXTEND
	   && GET_MODE (XEXP (xsrc, 0)) == from_mode)
	  || (GET_CODE (xsrc) == MEM
	      && GET_MODE (xsrc) == from_mode)))
	return 1;

      /* The register is modified by another operation.  */
      if (reg_overlap_mentioned_p (xdest, op))
	return 0;
    }
}

char *
output_move_double (operands)
     rtx *operands;
{
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT)
	{
	  /* In an integer, the low-order word is in CONST_DOUBLE_LOW.  */
	  rtx const_op = operands[1];
	  if ((CONST_DOUBLE_HIGH (const_op) == 0
	       && CONST_DOUBLE_LOW (const_op) >= 0)
	      || (CONST_DOUBLE_HIGH (const_op) == -1
		  && CONST_DOUBLE_LOW (const_op) < 0))
	    {
	      operands[1] = GEN_INT (CONST_DOUBLE_LOW (const_op));
	      return "movl %1,%0";
	    }
	  operands[1] = GEN_INT (CONST_DOUBLE_HIGH (const_op));
	  output_asm_insn ("movw %1,%0", operands);
	  operands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	  operands[1] = GEN_INT (CONST_DOUBLE_LOW (const_op));
	  return "movw %1,%0";
	}
      else
	{
	  /* In a real, the low-address word is in CONST_DOUBLE_LOW.  */
	  rtx const_op = operands[1];
	  if ((CONST_DOUBLE_LOW (const_op) == 0
	       && CONST_DOUBLE_HIGH (const_op) >= 0)
	      || (CONST_DOUBLE_LOW (const_op) == -1
		  && CONST_DOUBLE_HIGH (const_op) < 0))
	    {
	      operands[1] = GEN_INT (CONST_DOUBLE_HIGH (const_op));
	      return "movl %1,%0";
	    }
	  operands[1] = GEN_INT (CONST_DOUBLE_LOW (const_op));
	  output_asm_insn ("movw %1,%0", operands);
	  operands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	  operands[1] = GEN_INT (CONST_DOUBLE_HIGH (const_op));
	  return "movw %1,%0";
	}
    }

  return "movl %1,%0";
}

/* Output a shift insns, after having reduced integer arguments to
   avoid as warnings.  */

char *
output_shift (pattern, op2, mod)
     char *pattern;
     rtx op2;
     int mod;
{
  if (GET_CODE (op2) == CONST_INT)
    {
      int cnt = INTVAL (op2) % mod;
      if (cnt == 0)
	{
	  cc_status = cc_prev_status;
	  return "";
	}
      op2 = GEN_INT (cnt);
    }
  return pattern;
}

/* Return non-zero if the code of this rtx pattern is a relop.  */

int
relop (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case EQ:
    case NE:
    case LT:
    case LE:
    case GE:
    case GT:
    case LTU:
    case LEU:
    case GEU:
    case GTU:
      return 1;
    }
  return 0;
}

void
notice_update_cc (EXP, INSN)
     rtx EXP, INSN;
{
  switch (GET_CODE (EXP))
    {
    case SET:
      switch (GET_CODE (SET_DEST (EXP)))
	{
	case CC0:
	  cc_status.mdep = 0;
	  cc_status.flags = 0;
	  cc_status.value1 = 0;
	  cc_status.value2 = SET_SRC (EXP);
	  break;

	case PC:
	  break;

	case REG:
	  switch (GET_CODE (SET_SRC (EXP)))
	    {
	    case CALL:
	      goto call;
	    case MEM:
	      if (GET_MODE (SET_SRC (EXP)) == QImode
		  || GET_MODE (SET_SRC (EXP)) == HImode)
		{
		  cc_status.mdep = 0;
		  cc_status.flags = CC_NO_OVERFLOW;
		  cc_status.value1 = SET_DEST (EXP);
		  cc_status.value2 = SET_SRC (EXP);
		  break;
		}
	      /* else: Fall through.  */
	    case CONST_INT:
	    case SYMBOL_REF:
	    case LABEL_REF:
	    case CONST:
	    case CONST_DOUBLE:
	    case REG:
	      if (cc_status.value1
		  && reg_overlap_mentioned_p (SET_DEST (EXP),
					      cc_status.value1))
		cc_status.value1 = 0;
	      if (cc_status.value2
		  && reg_overlap_mentioned_p (SET_DEST (EXP),
					      cc_status.value2))
		cc_status.value2 = 0;
	      break;

	    case UDIV:
	    case UMOD:
	      cc_status.mdep = CC_VALID_FOR_UNSIGNED;
	      cc_status.flags = CC_NO_OVERFLOW;
	      cc_status.value1 = SET_DEST (EXP);
	      cc_status.value2 = SET_SRC (EXP);
	      break;
	    default:
	      cc_status.mdep = 0;
	      cc_status.flags = CC_NO_OVERFLOW;
	      cc_status.value1 = SET_DEST (EXP);
	      cc_status.value2 = SET_SRC (EXP);
	      break;
	    }
	  break;

	case MEM:
	  switch (GET_CODE (SET_SRC (EXP)))
	    {
	    case REG:
	      if (GET_MODE (SET_SRC (EXP)) == QImode
		  || GET_MODE (SET_SRC (EXP)) == HImode)
		{
		  cc_status.flags = CC_NO_OVERFLOW;
		  cc_status.value1 = SET_DEST (EXP);
		  cc_status.value2 = SET_SRC (EXP);
		  cc_status.mdep = 0;
		  break;
		}
	      /* else: Fall through.  */
	    case CONST_INT:
	    case SYMBOL_REF:
	    case LABEL_REF:
	    case CONST:
	    case CONST_DOUBLE:
	    case MEM:
	      /* Need to forget cc_status about memory positions each
		 time a memory store is made, even if the memory store
		 insns in question doesn't modify the condition codes.  */
	      if (cc_status.value1 &&
		  GET_CODE (cc_status.value1) == MEM)
		cc_status.value1 = 0;
	      if (cc_status.value2 &&
		  GET_CODE (cc_status.value2) == MEM)
		cc_status.value2 = 0;
	      break;
	    case SIGN_EXTEND:
	    case FLOAT_EXTEND:
	    case FLOAT_TRUNCATE:
	    case FLOAT:
	    case FIX:
	      cc_status.flags = CC_NO_OVERFLOW;
	      cc_status.value1 = SET_DEST (EXP);
	      cc_status.value2 = SET_SRC (EXP);
	      cc_status.mdep = 0;
	      break;

	    default:
	      abort ();
	    }
	  break;

	default:
	  abort ();
	}
      break;

    case CALL:
    call:
      CC_STATUS_INIT;
      break;
      /* Do calls preserve the condition codes?  (At least forget
	 cc_status expressions if they refer to registers
	 not preserved across calls.  Also forget expressions
	 about memory contents.)  */
      if (cc_status.value1
	  && (refers_to_regno_p (PYR_TREG (0), PYR_TREG (15),
				 cc_status.value1, 0)
	      || GET_CODE (cc_status.value1) == MEM))
	cc_status.value1 = 0;
      if (cc_status.value2
	  && (refers_to_regno_p (PYR_TREG (0), PYR_TREG (15),
				 cc_status.value2, 0)
	      || GET_CODE (cc_status.value2) == MEM))
	cc_status.value2 = 0;
      break;

    default:
      CC_STATUS_INIT;
    }
}

void
forget_cc_if_dependent (op)
     rtx op;
{
  cc_status = cc_prev_status;
  if (cc_status.value1 && reg_overlap_mentioned_p (op, cc_status.value1))
    cc_status.value1 = 0;
  if (cc_status.value2 && reg_overlap_mentioned_p (op, cc_status.value2))
    cc_status.value2 = 0;
}
