/* Subroutines used for code generation on IBM RS/6000.
   Copyright (C) 1991 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@nyu.edu)

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-flags.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "obstack.h"
#include "tree.h"

extern char *language_string;

#define min(A,B)	((A) < (B) ? (A) : (B))
#define max(A,B)	((A) > (B) ? (A) : (B))

/* Set to non-zero by "fix" operation to indicate that itrunc and
   uitrunc must be defined.  */

int rs6000_trunc_used;

/* Set to non-zero once they have been defined.  */

static int trunc_defined;

/* Save information from a "cmpxx" operation until the branch or scc is
   emitted.  */

rtx rs6000_compare_op0, rs6000_compare_op1;
int rs6000_compare_fp_p;

/* Return non-zero if this function is known to have a null epilogue.  */

int
direct_return ()
{
  return (reload_completed
	  && first_reg_to_save () == 32
	  && first_fp_reg_to_save () == 64
	  && ! regs_ever_live[65]
	  && ! rs6000_pushes_stack ());
}

/* Returns 1 always.  */

int
any_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return 1;
}

/* Return 1 if OP is a constant that can fit in a D field.  */

int
short_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned) (INTVAL (op) + 0x8000) < 0x10000);
}

/* Similar for a unsigned D field.  */

int
u_short_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && (INTVAL (op) & 0xffff0000) == 0);
}

/* Return 1 if OP is a CONST_INT that cannot fit in a signed D field.  */

int
non_short_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned) (INTVAL (op) + 0x8000) >= 0x10000);
}

/* Returns 1 if OP is a register that is not special (i.e., not MQ,
   ctr, or lr).  */

int
gpc_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG || REGNO (op) >= 67 || REGNO (op) < 64));
}

/* Returns 1 if OP is either a pseudo-register or a register denoting a
   CR field.  */

int
cc_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  && (GET_CODE (op) != REG
	      || REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || CR_REGNO_P (REGNO (op))));
}

/* Returns 1 if OP is either a constant integer valid for a D-field or a
   non-special register.  If a register, it must be in the proper mode unless
   MODE is VOIDmode.  */

int
reg_or_short_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return short_cint_operand (op, mode);

  return gpc_reg_operand (op, mode);
}

/* Similar, except check if the negation of the constant would be valid for
   a D-field.  */

int
reg_or_neg_short_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return CONST_OK_FOR_LETTER_P (INTVAL (op), 'P');

  return gpc_reg_operand (op, mode);
}

/* Return 1 if the operand is either a register or an integer whose high-order
   16 bits are zero.  */

int
reg_or_u_short_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT
      && (INTVAL (op) & 0xffff0000) == 0)
    return 1;

  return gpc_reg_operand (op, mode);
}

/* Return 1 is the operand is either a non-special register or ANY
   constant integer.  */

int
reg_or_cint_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
     return GET_CODE (op) == CONST_INT || gpc_reg_operand (op, mode);
}

/* Return 1 if the operand is a CONST_DOUBLE and it can be put into a
   register with one instruction per word.  For SFmode, this means  that
   the low 16-bits are zero.  For DFmode, it means the low 16-bits of
   the first word are zero and the high 16 bits of the second word
   are zero (usually all bits in the low-order word will be zero).

   We only do this if we can safely read CONST_DOUBLE_{LOW,HIGH}.  */

int
easy_fp_constant (op, mode)
     register rtx op;
     register enum machine_mode mode;
{
  rtx low, high;

  if (GET_CODE (op) != CONST_DOUBLE
      || GET_MODE (op) != mode
      || GET_MODE_CLASS (mode) != MODE_FLOAT)
    return 0;

  high = operand_subword (op, 0, 0, mode);
  low = operand_subword (op, 1, 0, mode);

  if (high == 0 || GET_CODE (high) != CONST_INT || (INTVAL (high) & 0xffff))
    return 0;

  return (mode == SFmode
	  || (low != 0 && GET_CODE (low) == CONST_INT
	      && (INTVAL (low) & 0xffff0000) == 0));
}
      
/* Return 1 if the operand is either a floating-point register, a pseudo
   register, or memory.  */

int
fp_reg_or_mem_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (memory_operand (op, mode)
	  || (register_operand (op, mode)
	      && (GET_CODE (op) != REG
		  || REGNO (op) >= FIRST_PSEUDO_REGISTER
		  || FP_REGNO_P (REGNO (op)))));
}

/* Return 1 if the operand is either an easy FP constant (see above) or
   memory.  */

int
mem_or_easy_const_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return memory_operand (op, mode) || easy_fp_constant (op, mode);
}

/* Return 1 if the operand is either a non-special register or an item
   that can be used as the operand of an SI add insn.  */

int
add_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return (reg_or_short_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && (INTVAL (op) & 0xffff) == 0));
}

/* Return 1 if OP is a constant but not a valid add_operand.  */

int
non_add_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (unsigned) (INTVAL (op) + 0x8000) >= 0x10000
	  && (INTVAL (op) & 0xffff) != 0);
}

/* Return 1 if the operand is a non-special register or a constant that
   can be used as the operand of an OR or XOR insn on the RS/6000.  */

int
logical_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (gpc_reg_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && ((INTVAL (op) & 0xffff0000) == 0
		  || (INTVAL (op) & 0xffff) == 0)));
}

/* Return 1 if C is a constant that is not a logical operand (as
   above).  */

int
non_logical_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) & 0xffff0000) != 0
	  && (INTVAL (op) & 0xffff) != 0);
}

/* Return 1 if C is a constant that can be encoded in a mask on the
   RS/6000.  It is if there are no more than two 1->0 or 0->1 transitions.
   Reject all ones and all zeros, since these should have been optimized
   away and confuse the making of MB and ME.  */

int
mask_constant (c)
     register int c;
{
  int i;
  int last_bit_value;
  int transitions = 0;

  if (c == 0 || c == ~0)
    return 0;

  last_bit_value = c & 1;

  for (i = 1; i < 32; i++)
    if (((c >>= 1) & 1) != last_bit_value)
      last_bit_value ^= 1, transitions++;

  return transitions <= 2;
}

/* Return 1 if the operand is a constant that is a mask on the RS/6000. */

int
mask_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == CONST_INT && mask_constant (INTVAL (op));
}

/* Return 1 if the operand is either a non-special register or a
   constant that can be used as the operand of an RS/6000 logical AND insn.  */

int
and_operand (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  return (reg_or_short_operand (op, mode)
	  || logical_operand (op, mode)
	  || mask_operand (op, mode));
}

/* Return 1 if the operand is a constant but not a valid operand for an AND
   insn.  */

int
non_and_cint_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return GET_CODE (op) == CONST_INT && ! and_operand (op, mode);
}

/* Return 1 if the operand is a general register or memory operand.  */

int
reg_or_mem_operand (op, mode)
     register rtx op;
     register enum machine_mode mode;
{
  return gpc_reg_operand (op, mode) || memory_operand (op, mode);
}

/* Return 1 if the operand, used inside a MEM, is a valid first argument
   to CALL.  This is a SYMBOL_REF or a pseudo-register, which will be
   forced to lr.  */

int
call_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  return (GET_CODE (op) == SYMBOL_REF
	  || (GET_CODE (op) == REG && REGNO (op) >= FIRST_PSEUDO_REGISTER));
}

/* Return 1 if this operand is a valid input for a move insn.  */

int
input_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (memory_operand (op, mode))
    return 1;

  /* For floating-point or multi-word mode, only register or memory
     is valid.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      || GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return gpc_reg_operand (op, mode);

  /* The only cases left are integral modes one word or smaller (we
     do not get called for MODE_CC values).  These can be in any
     register.  */
  if (register_operand (op, mode))
    return;

  /* For HImode and QImode, any constant is valid. */
  if ((mode == HImode || mode == QImode)
      && GET_CODE (op) == CONST_INT)
    return 1;

  /* Otherwise, we will be doing this SET with an add, so anything valid
     for an add will be valid.  */
  return add_operand (op, mode);
}

/* Return 1 if OP is a load multiple operation.  It is known to be a
   PARALLEL and the first section will be tested.  */

int
load_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int count = XVECLEN (op, 0);
  int dest_regno;
  rtx src_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || REGNO (SET_DEST (elt)) != dest_regno + i
	  || GET_CODE (SET_SRC (elt)) != MEM
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
	  || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
}

/* Similar, but tests for store multiple.  Here, the second vector element
   is a CLOBBER.  It will be tested later.  */

int
store_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int count = XVECLEN (op, 0) - 1;
  int src_regno;
  rtx dest_addr;
  int i;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, 0)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, 0)), 0);

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i + 1);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != REG
	  || GET_MODE (SET_SRC (elt)) != SImode
	  || REGNO (SET_SRC (elt)) != src_regno + i
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
	  || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
	  || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
	  || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != i * 4)
	return 0;
    }

  return 1;
}

/* Return 1 if OP is a comparison operation that is valid for a branch insn.
   We only check the opcode against the mode of the CC value here.  */

int
branch_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  enum machine_mode cc_mode;

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  cc_mode = GET_MODE (XEXP (op, 0));
  if (GET_MODE_CLASS (cc_mode) != MODE_CC)
    return 0;

  if ((code == GT || code == LT || code == GE || code == LE)
      && cc_mode == CCUNSmode)
    return 0;

  if ((code == GTU || code == LTU || code == GEU || code == LEU)
      && (cc_mode != CCUNSmode))
    return 0;

  return 1;
}

/* Return 1 if OP is a comparison operation that is valid for an scc insn.
   We check the opcode against the mode of the CC value and disallow EQ or
   NE comparisons for integers.  */

int
scc_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  enum machine_mode cc_mode;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_RTX_CLASS (code) != '<')
    return 0;

  cc_mode = GET_MODE (XEXP (op, 0));
  if (GET_MODE_CLASS (cc_mode) != MODE_CC)
    return 0;

  if (code == NE && cc_mode != CCFPmode)
    return 0;

  if ((code == GT || code == LT || code == GE || code == LE)
      && cc_mode == CCUNSmode)
    return 0;

  if ((code == GTU || code == LTU || code == GEU || code == LEU)
      && (cc_mode != CCUNSmode))
    return 0;

  if (cc_mode == CCEQmode && code != EQ && code != NE)
    return 0;

  return 1;
}

/* Return 1 if ANDOP is a mask that has no bits on that are not in the
   mask required to convert the result of a rotate insn into a shift
   left insn of SHIFTOP bits.  Both are known to be CONST_INT.  */

int
includes_lshift_p (shiftop, andop)
     register rtx shiftop;
     register rtx andop;
{
  int shift_mask = (~0 << INTVAL (shiftop));

  return (INTVAL (andop) & ~shift_mask) == 0;
}

/* Similar, but for right shift.  */

int
includes_rshift_p (shiftop, andop)
     register rtx shiftop;
     register rtx andop;
{
  unsigned shift_mask = ~0;

  shift_mask >>= INTVAL (shiftop);

  return (INTVAL (andop) & ~ shift_mask) == 0;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno = true_regnum (in);

  if (regno >= FIRST_PSEUDO_REGISTER)
    regno = -1;

  /* We can place anything into GENERAL_REGS and can put GENERAL_REGS
     into anything.  */
  if (class == GENERAL_REGS || class == BASE_REGS
      || (regno >= 0 && INT_REGNO_P (regno)))
    return NO_REGS;

  /* Constants, memory, and FP registers can go into FP registers.  */
  if ((regno == -1 || FP_REGNO_P (regno))
      && (class == FLOAT_REGS || class == NON_SPECIAL_REGS))
    return NO_REGS;

  /* We can copy among the CR registers.  */
  if ((class == CR_REGS || class == CR0_REGS)
      && regno >= 0 && CR_REGNO_P (regno))
    return NO_REGS;

  /* Otherwise, we need GENERAL_REGS.  */
  return GENERAL_REGS;
}

/* Given a comparison operation, return the bit number in CCR to test.  We
   know this is a valid comparison.  

   SCC_P is 1 if this is for an scc.  That means that %D will have been
   used instead of %C, so the bits will be in different places.

   Return -1 if OP isn't a valid comparison for some reason.  */

int
ccr_bit (op, scc_p)
     register rtx op;
     int scc_p;
{
  enum rtx_code code = GET_CODE (op);
  enum machine_mode cc_mode;
  int cc_regnum;
  int base_bit;

  if (GET_RTX_CLASS (code) != '<')
    return -1;

  cc_mode = GET_MODE (XEXP (op, 0));
  cc_regnum = REGNO (XEXP (op, 0));
  base_bit = 4 * (cc_regnum - 68);

  /* In CCEQmode cases we have made sure that the result is always in the
     third bit of the CR field.  */

  if (cc_mode == CCEQmode)
    return base_bit + 3;

  switch (code)
    {
    case NE:
      return scc_p ? base_bit + 3 : base_bit + 2;
    case EQ:
      return base_bit + 2;
    case GT:  case GTU:
      return base_bit + 1;
    case LT:  case LTU:
      return base_bit;

    case GE:  case GEU:
      /* If floating-point, we will have done a cror to put the bit in the
	 unordered position.  So test that bit.  For integer, this is ! LT
	 unless this is an scc insn.  */
      return cc_mode == CCFPmode || scc_p ? base_bit + 3 : base_bit;

    case LE:  case LEU:
      return cc_mode == CCFPmode || scc_p ? base_bit + 3 : base_bit + 1;

    default:
      abort ();
    }
}

/* Print an operand.  Recognize special options, documented below.  */

void
print_operand (file, x, code)
    FILE *file;
    rtx x;
    char code;
{
  int i;
  int val;

  /* These macros test for integers and extract the low-order bits.  */
#define INT_P(X)  \
((GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST_DOUBLE)	\
 && GET_MODE (X) == VOIDmode)

#define INT_LOWPART(X) \
  (GET_CODE (X) == CONST_INT ? INTVAL (X) : CONST_DOUBLE_LOW (X))

  switch (code)
    {
    case 'h':
      /* If constant, output low-order five bits.  Otherwise,
	 write normally. */
      if (INT_P (x))
	fprintf (file, "%d", INT_LOWPART (x) & 31);
      else
	print_operand (file, x, 0);
      return;

    case 'H':
      /* X must be a constant.  Output the low order 5 bits plus 24.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%H value");

      fprintf (file, "%d", (INT_LOWPART (x) + 24) & 31);
      return;

    case 'b':
      /* Low-order 16 bits of constant, unsigned.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%b value");

      fprintf (file, "%d", INT_LOWPART (x) & 0xffff);
      return;

    case 'w':
      /* If constant, low-order 16 bits of constant, signed.  Otherwise, write
	 normally.  */
      if (INT_P (x))
	fprintf (file, "%d",
		 (INT_LOWPART (x) & 0xffff) - 2 * (INT_LOWPART (x) & 0x8000));
      else
	print_operand (file, x, 0);
      return;

    case 'W':
      /* If constant, low-order 16 bits of constant, unsigned.
	 Otherwise, write normally.  */
      if (INT_P (x))
	fprintf (file, "%d", INT_LOWPART (x) & 0xffff);
      else
	print_operand (file, x, 0);
      return;

    case 'u':
      /* High-order 16 bits of constant.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%u value");

      fprintf (file, "%d", (INT_LOWPART (x) >> 16) & 0xffff);
      return;

    case 's':
      /* Low 5 bits of 32 - value */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%s value");

      fprintf (file, "%d", (32 - INT_LOWPART (x)) & 31);
      return;

    case 'S':
      /* Low 5 bits of 31 - value */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%S value");

      fprintf (file, "%d", (31 - INT_LOWPART (x)) & 31);
      return;

    case 'p':
      /* X is a CONST_INT that is a power of two.  Output the logarithm.  */
      if (! INT_P (x)
	  || (i = exact_log2 (INT_LOWPART (x))) < 0)
	output_operand_lossage ("invalid %%p value");

      fprintf (file, "%d", i);
      return;

    case 'm':
      /* MB value for a mask operand.  */
      if (! mask_operand (x, VOIDmode))
	output_operand_lossage ("invalid %%m value");

      val = INT_LOWPART (x);

      /* If the high bit is set and the low bit is not, the value is zero.
	 If the high bit is zero, the value is the first 1 bit we find from
	 the left.  */
      if (val < 0 && (val & 1) == 0)
	{
	  fprintf (file, "0");
	  return;
	}
      else if (val >= 0)
	{
	  for (i = 1; i < 32; i++)
	    if ((val <<= 1) < 0)
	      break;
	  fprintf (file, "%d", i);
	  return;
	}
	  
      /* Otherwise, look for the first 0 bit from the right.  The result is its
	 number plus 1. We know the low-order bit is one.  */
      for (i = 0; i < 32; i++)
	if (((val >>= 1) & 1) == 0)
	  break;

      /* If we ended in ...01, I would be 0.  The correct value is 31, so
	 we want 31 - i.  */
      fprintf (file, "%d", 31 - i);
      return;

    case 'M':
      /* ME value for a mask operand.  */
      if (! mask_operand (x, VOIDmode))
	output_operand_lossage ("invalid %%m value");

      val = INT_LOWPART (x);

      /* If the low bit is set and the high bit is not, the value is 31.
	 If the low bit is zero, the value is the first 1 bit we find from
	 the right.  */
      if ((val & 1) && val >= 0)
	{
	  fprintf (file, "31");
	  return;
	}
      else if ((val & 1) == 0)
	{
	  for (i = 0; i < 32; i++)
	    if ((val >>= 1) & 1)
	      break;

	  /* If we had ....10, I would be 0.  The result should be
	     30, so we need 30 - i.  */
	  fprintf (file, "%d", 30 - i);
	  return;
	}
	  
      /* Otherwise, look for the first 0 bit from the left.  The result is its
	 number minus 1. We know the high-order bit is one.  */
      for (i = 0; i < 32; i++)
	if ((val <<= 1) >= 0)
	  break;

      fprintf (file, "%d", i);
      return;

    case 'f':
      /* X is a CR register.  Print the shift count needed to move it
	 to the high-order four bits.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%f value");
      else
	fprintf (file, "%d", 4 * (REGNO (x) - 68));
      return;

    case 'F':
      /* Similar, but print the count for the rotate in the opposite
	 direction.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%F value");
      else
	fprintf (file, "%d", 32 - 4 * (REGNO (x) - 68));
      return;

    case 'E':
      /* X is a CR register.  Print the number of the third bit of the CR */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%E value");

      fprintf(file, "%d", 4 * (REGNO (x) - 68) + 3);
      break;

    case 'R':
      /* X is a CR register.  Print the mask for `mtcrf'.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%R value");
      else
	fprintf (file, "%d", 128 >> (REGNO (x) - 68));
      return;

    case 'X':
      if (GET_CODE (x) == MEM
	  && LEGITIMATE_INDEXED_ADDRESS_P (XEXP (x, 0)))
	fprintf (file, "x");
      return;

    case 'U':
      /* Print `u' is this has an auto-increment or auto-decrement.  */
      if (GET_CODE (x) == MEM
	  && (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC))
	fprintf (file, "u");
      return;

    case 'I':
      /* Print `i' is this is a constant, else nothing.  */
      if (INT_P (x))
	fprintf (file, "i");
      return;

    case 'N':
      /* Write the number of elements in the vector times 4.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%N value");

      fprintf (file, "%d", XVECLEN (x, 0) * 4);
      return;

    case 'O':
      /* Similar, but subtract 1 first.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%N value");

      fprintf (file, "%d", (XVECLEN (x, 0) - 1) * 4);
      return;

    case 'P':
      /* The operand must be an indirect memory reference.  The result
	 is the register number. */
      if (GET_CODE (x) != MEM || GET_CODE (XEXP (x, 0)) != REG
	  || REGNO (XEXP (x, 0)) >= 32)
	output_operand_lossage ("invalid %%P value");

      fprintf (file, "%d", REGNO (XEXP (x, 0)));
      return;

    case 'L':
      /* Write second word of DImode or DFmode reference.  Works on register
	 or non-indexed memory only.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%d", REGNO (x) + 1);
      else if (GET_CODE (x) == MEM)
	{
	  /* Handle possible auto-increment.  Since it is pre-increment and
	     we have already done it, we can just use an offset of four.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 4));
	  else
	    output_address (plus_constant (XEXP (x, 0), 4));
	}
      return;
			    
    case 'Y':
      /* Similar, for third word of TImode  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%d", REGNO (x) + 2);
      else if (GET_CODE (x) == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 8));
	  else
	    output_address (plus_constant (XEXP (x, 0), 8));
	}
      return;
			    
    case 'Z':
      /* Similar, for last word of TImode.  */
      if (GET_CODE (x) == REG)
	fprintf (file, "%d", REGNO (x) + 3);
      else if (GET_CODE (x) == MEM)
	{
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (XEXP (XEXP (x, 0), 0), 12));
	  else
	    output_address (plus_constant (XEXP (x, 0), 12));
	}
      return;
			    
    case 't':
      /* Write 12 if this jump operation will branch if true, 4 otherwise. 
	 All floating-point operations except NE branch true and integer
	 EQ, LT, GT, LTU and GTU also branch true.  */
      if (GET_RTX_CLASS (GET_CODE (x)) != '<')
	output_operand_lossage ("invalid %%t value");

      else if ((GET_MODE (XEXP (x, 0)) == CCFPmode
		&& GET_CODE (x) != NE)
	       || GET_CODE (x) == EQ
	       || GET_CODE (x) == LT || GET_CODE (x) == GT
	       || GET_CODE (x) == LTU || GET_CODE (x) == GTU)
	fprintf (file, "12");
      else
	fprintf (file, "4");
      return;
      
    case 'T':
      /* Opposite of 't': write 4 if this jump operation will branch if true,
	 12 otherwise.   */
      if (GET_RTX_CLASS (GET_CODE (x)) != '<')
	output_operand_lossage ("invalid %%t value");

      else if ((GET_MODE (XEXP (x, 0)) == CCFPmode
		&& GET_CODE (x) != NE)
	       || GET_CODE (x) == EQ
	       || GET_CODE (x) == LT || GET_CODE (x) == GT
	       || GET_CODE (x) == LTU || GET_CODE (x) == GTU)
	fprintf (file, "4");
      else
	fprintf (file, "12");
      return;
      
    case 'j':
      /* Write the bit number in CCR for jump.  */
      i = ccr_bit (x, 0);
      if (i == -1)
	output_operand_lossage ("invalid %%j code");
      else
	fprintf (file, "%d", i);
      return;

    case 'J':
      /* Similar, but add one for shift count in rlinm for scc and pass
	 scc flag to `ccr_bit'.  */
      i = ccr_bit (x, 1);
      if (i == -1)
	output_operand_lossage ("invalid %%J code");
      else
	fprintf (file, "%d", i + 1);
      return;

    case 'C':
      /* This is an optional cror needed for LE or GE floating-point
	 comparisons.  Otherwise write nothing.  */
      if ((GET_CODE (x) == LE || GET_CODE (x) == GE)
	  && GET_MODE (XEXP (x, 0)) == CCFPmode)
	{
	  int base_bit = 4 * (REGNO (XEXP (x, 0)) - 68);

	  fprintf (file, "cror %d,%d,%d\n\t", base_bit + 3,
		   base_bit + 2, base_bit + (GET_CODE (x) == GE));
	}
      return;

    case 'D':
      /* Similar, except that this is for an scc, so we must be able to
	 encode the test in a single bit that is one.  We do the above
	 for any LE, GE, GEU, or LEU and invert the bit for NE.  */
      if (GET_CODE (x) == LE || GET_CODE (x) == GE
	  || GET_CODE (x) == LEU || GET_CODE (x) == GEU)
	{
	  int base_bit = 4 * (REGNO (XEXP (x, 0)) - 68);

	  fprintf (file, "cror %d,%d,%d\n\t", base_bit + 3,
		   base_bit + 2,
		   base_bit + (GET_CODE (x) == GE || GET_CODE (x) == GEU));
	}

      else if (GET_CODE (x) == NE)
	{
	  int base_bit = 4 * (REGNO (XEXP (x, 0)) - 68);

	  fprintf (file, "crnor %d,%d,%d\n\t", base_bit + 3,
		   base_bit + 2, base_bit + 2);
	}
      return;

    case 'z':
      /* X is a SYMBOL_REF.  Write out the name preceded by a
	 period and without any trailing data in brackets.  Used for function
	 names.  */
      if (GET_CODE (x) != SYMBOL_REF)
	abort ();

      fprintf (file, ".");
      RS6000_OUTPUT_BASENAME (file, XSTR (x, 0));
      return;

    case 'A':
      /* If X is a constant integer whose low-order 5 bits are zero,
	 write 'l'.  Otherwise, write 'r'.  This is a kludge to fix a bug
	 in the RS/6000 assembler where "sri" with a zero shift count
	 write a trash instruction.  */
      if (GET_CODE (x) == CONST_INT && (INTVAL (x) & 31) == 0)
	fprintf (file, "l");
      else
	fprintf (file, "r");
      return;

    case 0:
      if (GET_CODE (x) == REG)
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (GET_CODE (x) == MEM)
	{
	  /* We need to handle PRE_INC and PRE_DEC here, since we need to
	     know the width from the mode.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC)
	    fprintf (file, "%d(%d)", GET_MODE_SIZE (GET_MODE (x)),
		     REGNO (XEXP (XEXP (x, 0), 0)));
	  else if (GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    fprintf (file, "%d(%d)", - GET_MODE_SIZE (GET_MODE (x)),
		     REGNO (XEXP (XEXP (x, 0), 0)));
	  else
	    output_address (XEXP (x, 0));
	}
      else
	output_addr_const (file, x);
      break;

    default:
      output_operand_lossage ("invalid %%xn code");
    }
}

/* Print the address of an operand.  */

void
print_operand_address (file, x)
     FILE *file;
     register rtx x;
{
  if (GET_CODE (x) == REG)
    fprintf (file, "0(%d)", REGNO (x));
  else if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == CONST)
    {
      output_addr_const (file, x);
      fprintf (file, "(2)");
    }
  else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == REG)
    {
      if (REGNO (XEXP (x, 0)) == 0)
	fprintf (file, "%d,%d", REGNO (XEXP (x, 1)), REGNO (XEXP (x, 0)));
      else
	fprintf (file, "%d,%d", REGNO (XEXP (x, 0)), REGNO (XEXP (x, 1)));
    }
  else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == CONST_INT)
    fprintf (file, "%d(%d)", INTVAL (XEXP (x, 1)), REGNO (XEXP (x, 0)));
  else
    abort ();
}

/* This page contains routines that are used to determine what the function
   prologue and epilogue code will do and write them out.  */

/*  Return the first fixed-point register that is required to be saved. 32 if
    none.  */

int
first_reg_to_save ()
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 13; first_reg <= 31; first_reg++)
    if (regs_ever_live[first_reg])
      break;

  /* If profiling, then we must save/restore every register that contains
     a parameter before/after the .mcount call.  Use registers from 30 down
     to 23 to do this.  Don't use the frame pointer in reg 31.

     For now, save enough room for all of the parameter registers.  */
  if (profile_flag)
    if (first_reg > 23)
      first_reg = 23;

  return first_reg;
}

/* Similar, for FP regs.  */

int
first_fp_reg_to_save ()
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 14 + 32; first_reg <= 63; first_reg++)
    if (regs_ever_live[first_reg])
      break;

  return first_reg;
}

/* Return 1 if we need to save CR.  */

int
must_save_cr ()
{
  return regs_ever_live[70] || regs_ever_live[71] || regs_ever_live[72];
}

/* Compute the size of the save area in the stack, including the space for
   the fixed area.  */

int
rs6000_sa_size ()
{
  int size;
  int i;

  /* We have the six fixed words, plus the size of the register save 
     areas, rounded to a double-word.  */
  size = 6 + (32 - first_reg_to_save ()) + (64 - first_fp_reg_to_save ()) * 2;
  if (size & 1)
    size++;

  return size * 4;
}

/* Return non-zero if this function makes calls.  */

int
rs6000_makes_calls ()
{
  rtx insn;

  for (insn = get_insns (); insn; insn = next_insn (insn))
    if (GET_CODE (insn) == CALL_INSN)
      return 1;

  return 0;
}

/* Return non-zero if this function needs to push space on the stack.  */

int
rs6000_pushes_stack ()
{
  int total_size = (rs6000_sa_size () + get_frame_size ()
		    + current_function_outgoing_args_size);

  /* We need to push the stack if a frame pointer is needed (because the
     stack might be dynamically adjusted), if we are debugging, if the
     total stack size is more than 220 bytes, or if we make calls.  */

  return (frame_pointer_needed || write_symbols != NO_DEBUG
	  || total_size > 220
	  || rs6000_makes_calls ());
}

/* Write function prologue.  */

void
output_prolog (file, size)
     FILE *file;
     int size;
{
  int first_reg = first_reg_to_save ();
  int must_push = rs6000_pushes_stack ();
  int first_fp_reg = first_fp_reg_to_save ();
  int basic_size = rs6000_sa_size ();
  int total_size = (basic_size + size + current_function_outgoing_args_size);

  /* Round size to multiple of 8 bytes.  */
  total_size = (total_size + 7) & ~7;

  /* Write .extern for any function we will call to save and restore fp
     values.  */
  if (first_fp_reg < 62)
    fprintf (file, "\t.extern ._savef%d\n\t.extern ._restf%d\n",
	     first_fp_reg - 32, first_fp_reg - 32);

  /* Write .extern for truncation routines, if needed.  */
  if (rs6000_trunc_used && ! trunc_defined)
    {
      fprintf (file, "\t.extern .itrunc\n\t.extern .uitrunc\n");
      trunc_defined = 1;
    }

  /* If we have to call a function to save fpr's, or if we are doing profiling,
     then we will be using LR.  */
  if (first_fp_reg < 62 || profile_flag)
    regs_ever_live[65] = 1;

  /* If we use the link register, get it into r0.  */
  if (regs_ever_live[65])
    fprintf (file, "\tmflr 0\n");

  /* If we need to save CR, put it into r12.  */
  if (must_save_cr ())
    fprintf (file, "\tmfcr 12\n");

  /* Do any required saving of fpr's.  If only one or two to save, do it
     ourself.  Otherwise, call function.  */
  if (first_fp_reg == 62)
    fprintf (file, "\tstfd 30,-16(1)\n\tstfd 31,-8(1)\n");
  else if (first_fp_reg == 63)
    fprintf (file, "\tstfd 31,-8(1)\n");
  else if (first_fp_reg != 64)
    fprintf (file, "\tbl ._savef%d\n\tcror 15,15,15\n", first_fp_reg - 32);

  /* Now save gpr's.  */
  if (first_reg == 31)
    fprintf (file, "\tst 31,%d(1)\n", -4 - (64 - first_fp_reg) * 8);
  else if (first_reg != 32)
    fprintf (file, "\tstm %d,%d(1)\n", first_reg,
	     - (32 - first_reg) * 4 - (64 - first_fp_reg) * 8);

  /* Save lr if we used it.  */
  if (regs_ever_live[65])
    fprintf (file, "\tst 0,8(1)\n");

  /* Save CR if we use any that must be preserved.  */
  if (must_save_cr ())
    fprintf (file, "\tst 12,4(1)\n");

  /* Update stack and set back pointer.  */
  if (must_push)
    {
      if (total_size < 32767)
	fprintf (file, "\tstu 1,%d(1)\n", - total_size);
      else
	{
	  fprintf (file, "\tcau 0,0,%d\n\toril 0,0,%d\n",
		   (total_size >> 16) & 0xffff, total_size & 0xffff);
	  fprintf (file, "\tsf 12,0,1\n\tst 1,0(12)\n\toril 1,12,0\n");
	}
    }

  /* Set frame pointer, if needed.  */
  if (frame_pointer_needed)
    fprintf (file, "\toril 31,1,0\n");
}

/* Write function epilogue.  */

void
output_epilog (file, size)
     FILE *file;
     int size;
{
  int first_reg = first_reg_to_save ();
  int must_push = rs6000_pushes_stack ();
  int first_fp_reg = first_fp_reg_to_save ();
  int basic_size = rs6000_sa_size ();
  int total_size = (basic_size + size + current_function_outgoing_args_size);
  rtx insn = get_last_insn ();

  /* Round size to multiple of 8 bytes.  */
  total_size = (total_size + 7) & ~7;

  /* If the last insn was a BARRIER, we don't have to write anything except
     the trace table.  */
  if (GET_CODE (insn) == NOTE)
    insn = prev_nonnote_insn (insn);
  if (insn == 0 ||  GET_CODE (insn) != BARRIER)
    {
      /* If we have a frame pointer, a call to alloca,  or a large stack
	 frame, restore the old stack pointer using the backchain.  Otherwise,
	 we know what size to update it with.  */
      if (frame_pointer_needed || current_function_calls_alloca
	  || total_size > 32767)
	fprintf (file, "\tl 1,0(1)\n");
      else if (must_push)
	fprintf (file, "\tai 1,1,%d\n", total_size);

      /* Get the old lr if we saved it.  */
      if (regs_ever_live[65])
	fprintf (file, "\tl 0,8(1)\n");

      /* Get the old cr if we saved it.  */
      if (must_save_cr ())
	fprintf (file, "\tl 12,4(1)\n");

      /* Set LR here to try to overlap restores below.  */
      if (regs_ever_live[65])
	fprintf (file, "\tmtlr 0\n");

      /* Restore gpr's.  */
      if (first_reg == 31)
	fprintf (file, "\tl 31,%d(1)\n", -4 - (64 - first_fp_reg) * 8);
      else if (first_reg != 32)
	fprintf (file, "\tlm %d,%d(1)\n", first_reg,
		 - (32 - first_reg) * 4 - (64 - first_fp_reg) * 8);

      /* Restore fpr's if we can do it without calling a function.  */
      if (first_fp_reg == 62)
	fprintf (file, "\tlfd 30,-16(1)\n\tlfd 31,-8(1)\n");
      else if (first_fp_reg == 63)
	fprintf (file, "\tlfd 31,-8(1)\n");

      /* If we saved cr, restore it here.  Just set cr2, cr3, and cr4.  */
      if (must_save_cr ())
	fprintf (file, "\tmtcrf 0x38,12\n");

      /* If we have to restore more than two FP registers, branch to the
	 restore function.  It will return to our caller.  */
      if (first_fp_reg < 62)
	fprintf (file, "\tb ._restf%d\n\tcror 15,15,15\n", first_fp_reg - 32);
      else
	fprintf (file, "\tbr\n");
    }

  /* Output a traceback table here.  See /usr/include/sys/debug.h for info
     on its format.  */
  {
    char *fname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
    int fixed_parms, float_parms, parm_info;
    int i;

    /* Need label immediately before tbtab, so we can compute its offset
       from the function start.  */
    if (*fname == '*')
      ++fname;
    fprintf (file, "LT..");
    ASM_OUTPUT_LABEL (file, fname);

    /* The .tbtab pseudo-op can only be used for the first eight
       expressions, since it can't handle the possibly variable length
       fields that follow.  However, if you omit the optional fields,
       the assembler outputs zeros for all optional fields anyways, giving each
       variable length field is minimum length (as defined in sys/debug.h).
       Thus we can not use the .tbtab pseudo-op at all.  */

    /* An all-zero word flags the start of the tbtab, for debuggers that have
       to find it by searching forward from the entry point or from the
       current pc.  */
    fprintf (file, "\t.long 0\n");

    /* Tbtab format type.  Use format type 0.  */
    fprintf (file, "\t.byte 0,");

    /* Language type.  Unfortunately, there doesn't seem to be any official way
       to get this info, so we use language_string.  C is 0.  C++ is 9.
       No number defined for Obj-C, but it doesn't have its own
       language_string, so we can't detect it anyways.  */
    if (! strcmp (language_string, "GNU C"))
      i = 0;
    else if (! strcmp (language_string, "GNU F77"))
      i = 1;
    else if (! strcmp (language_string, "GNU Ada"))
      i = 3;
    else if (! strcmp (language_string, "GNU PASCAL"))
      i = 2;
    else if (! strcmp (language_string, "GNU C++"))
      i = 9;
    else
      abort ();
    fprintf (file, "%d,", i);

    /* 8 single bit fields: global linkage (not set for C extern linkage,
       apparently a PL/I convention?), out-of-line epilogue/prologue, offset
       from start of procedure stored in tbtab, internal function, function
       has controlled storage, function has no toc, function uses fp,
       function logs/aborts fp operations.  */
    /* Assume that fp operations are used if any fp reg must be saved.  */
    fprintf (file, "%d,", (1 << 5) | ((first_fp_reg != 64) << 1));

    /* 6 bitfields: function is interrupt handler, name present in proc table,
       function calls alloca, on condition directives (controls stack walks,
       3 bits), saves condition reg, saves link reg.  */
    /* The `function calls alloca' bit seems to be set whenever reg 31 is
       set up as a frame pointer, even when there is no alloca call.  */
    fprintf (file, "%d,",
	     ((1 << 6) | (frame_pointer_needed << 5)
	      | (must_save_cr () << 1) | (regs_ever_live[65])));

    /* 3 bitfields: saves backchain, spare bit, number of fpr saved
       (6 bits).  */
    fprintf (file, "%d,",
	     (must_push << 7) | (64 - first_fp_reg_to_save ()));

    /* 2 bitfields: spare bits (2 bits), number of gpr saved (6 bits).  */
    fprintf (file, "%d,", (32 - first_reg_to_save ()));

    {
      /* Compute the parameter info from the function decl argument list.  */
      tree decl;
      int next_parm_info_bit;

      next_parm_info_bit = 31;
      parm_info = 0;
      fixed_parms = 0;
      float_parms = 0;

      for (decl = DECL_ARGUMENTS (current_function_decl);
	   decl; decl = TREE_CHAIN (decl))
	{
	  rtx parameter = DECL_INCOMING_RTL (decl);
	  enum machine_mode mode = GET_MODE (parameter);

	  if (GET_CODE (parameter) == REG)
	    {
	      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
		{
		  int bits;

		  float_parms++;

		  if (mode == SFmode)
		    bits = 0x2;
		  else if (mode == DFmode)
		    bits = 0x3;
		  else
		    abort ();

		  /* If only one bit will fit, don't or in this entry.  */
		  if (next_parm_info_bit > 0)
		    parm_info |= (bits << (next_parm_info_bit - 1));
		  next_parm_info_bit -= 2;
		}
	      else
		{
		  fixed_parms += ((GET_MODE_SIZE (mode) + (UNITS_PER_WORD - 1))
				  / UNITS_PER_WORD);
		  next_parm_info_bit -= 1;
		}
	    }
	}
    }

    /* Number of fixed point parameters.  */
    /* This is actually the number of words of fixed point parameters; thus
       an 8 byte struct counts as 2; and thus the maximum value is 8.  */
    fprintf (file, "%d,", fixed_parms);

    /* 2 bitfields: number of floating point parameters (7 bits), parameters
       all on stack.  */
    /* This is actually the number of fp registers that hold parameters;
       and thus the maximum value is 13.  */
    /* Set parameters on stack bit if parameters are not in their original
       registers, regardless of whether they are on the stack?  Xlc
       seems to set the bit when not optimizing.  */
    fprintf (file, "%d\n", ((float_parms << 1) | (! optimize)));

    /* Optional fields follow.  Some are variable length.  */

    /* Parameter types, left adjusted bit fields: 0 fixed, 10 single float,
       11 double float.  */
    /* There is an entry for each parameter in a register, in the order that
       they occur in the parameter list.  Any intervening arguments on the
       stack are ignored.  If the list overflows a long (max possible length
       34 bits) then completely leave off all elements that don't fit.  */
    /* Only emit this long if there was at least one parameter.  */
    if (fixed_parms || float_parms)
      fprintf (file, "\t.long %d\n", parm_info);

    /* Offset from start of code to tb table.  */
    fprintf (file, "\t.long LT..");
    RS6000_OUTPUT_BASENAME (file, fname);
    fprintf (file, "-.");
    RS6000_OUTPUT_BASENAME (file, fname);
    fprintf (file, "\n");

    /* Interrupt handler mask.  */
    /* Omit this long, since we never set the interrupt handler bit above.  */

    /* Number of CTL (controlled storage) anchors.  */
    /* Omit this long, since the has_ctl bit is never set above.  */

    /* Displacement into stack of each CTL anchor.  */
    /* Omit this list of longs, because there are no CTL anchors.  */

    /* Length of function name.  */
    fprintf (file, "\t.short %d\n", strlen (fname));

    /* Function name.  */
    assemble_string (fname, strlen (fname));

    /* Register for alloca automatic storage; this is always reg 31.
       Only emit this if the alloca bit was set above.  */
    if (frame_pointer_needed)
      fprintf (file, "\t.byte 31\n");
  }
}

/* Output a TOC entry.  We derive the entry name from what is
   being written.  */

void
output_toc (file, x, labelno)
     FILE *file;
     rtx x;
     int labelno;
{
  char buf[256];
  char *name = buf;
  rtx base = x;
  int offset = 0;

  ASM_OUTPUT_INTERNAL_LABEL (file, "LC", labelno);

  /* Handle FP constants specially.  */
  if (GET_CODE (x) == CONST_DOUBLE
      && GET_MODE (x) == DFmode
      && TARGET_FLOAT_FORMAT == HOST_FLOAT_FORMAT
      && BITS_PER_WORD == HOST_BITS_PER_INT
      && TARGET_FP_IN_TOC)
    {
      fprintf (file, "\t.tc FD_%x_%x[TC],%d,%d\n",
	       CONST_DOUBLE_LOW (x), CONST_DOUBLE_HIGH (x),
	       CONST_DOUBLE_LOW (x), CONST_DOUBLE_HIGH (x));
      return;
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode
	   && TARGET_FP_IN_TOC)
    {
      rtx val = operand_subword (x, 0, 0, SFmode);

      if (val == 0 || GET_CODE (val) != CONST_INT)
	abort ();

      fprintf (file, "\t.tc FS_%x[TC],%d\n", INTVAL (val), INTVAL (val));
      return;
    }

  if (GET_CODE (x) == CONST)
    {
      base = XEXP (XEXP (x, 0), 0);
      offset = INTVAL (XEXP (XEXP (x, 0), 1));
    }
  
  if (GET_CODE (base) == SYMBOL_REF)
    name = XSTR (base, 0);
  else if (GET_CODE (base) == LABEL_REF)
    ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (XEXP (base, 0)));
  else if (GET_CODE (base) == CODE_LABEL)
    ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (base));
  else
    abort ();

  fprintf (file, "\t.tc ");
  RS6000_OUTPUT_BASENAME (file, name);

  if (offset < 0)
    fprintf (file, ".N%d", - offset);
  else if (offset)
    fprintf (file, ".P%d", offset);

  fprintf (file, "[TC],");
  output_addr_const (file, x);
  fprintf (file, "\n");
}

/* Output an assembler pseudo-op to write an ASCII string of N characters
   starting at P to FILE.

   On the RS/6000, we have to do this using the .byte operation and
   write out special characters outside the quoted string.
   Also, the assembler is broken; very long strings are truncated,
   so we must artificially break them up early. */

void
output_ascii (file, p, n)
     FILE *file;
     char *p;
     int n;
{
  char c;
  int i, count_string;
  char *for_string = "\t.byte \"";
  char *for_decimal = "\t.byte ";
  char *to_close = NULL;

  count_string = 0;
  for (i = 0; i < n; i++)
    {
      c = *p++;
      if (c >= ' ' && c < 0177)
	{
	  if (for_string)
	    fputs (for_string, file);
	  putc (c, file);

	  /* Write two quotes to get one.  */
	  if (c == '"')
	    {
	      putc (c, file);
	      ++count_string;
	    }

	  for_string = NULL;
	  for_decimal = "\"\n\t.byte ";
	  to_close = "\"\n";
	  ++count_string;

	  if (count_string >= 512)
	    {
	      fputs (to_close, file);

	      for_string = "\t.byte \"";
	      for_decimal = "\t.byte ";
	      to_close = NULL;
	      count_string = 0;
	    }
	}
      else
	{
	  if (for_decimal)
	    fputs (for_decimal, file);
	  fprintf (file, "%d", c);

	  for_string = "\n\t.byte \"";
	  for_decimal = ", ";
	  to_close = "\n";
	  count_string = 0;
	}
    }

  /* Now close the string if we have written one.  Then end the line.  */
  if (to_close)
    fprintf (file, to_close);
}

/* Generate a unique section name for FILENAME for a section type
   represented by SECTION_DESC.  Output goes into BUF.

   SECTION_DESC can be any string, as long as it is different for each
   possible section type.

   We name the section in the same manner as xlc.  The name begins with an
   underscore followed by the filename (after stripping any leading directory
   names) with the period replaced by the string SECTION_DESC.  If FILENAME
   does not contain a period, SECTION_DESC is appended at the end of the
   name.  */

void
rs6000_gen_section_name (buf, filename, section_desc)
     char **buf;
     char *filename;
     char *section_desc;
{
  char *q, *after_last_slash;
  char *p;
  int len;
  int used_desc = 0;

  after_last_slash = filename;
  for (q = filename; *q; q++)
    if (*q == '/')
      after_last_slash = q + 1;

  len = strlen (filename) + strlen (section_desc) + 2;
  *buf = (char *) permalloc (len);

  p = *buf;
  *p++ = '_';

  for (q = after_last_slash; *q; q++)
    {
      if (*q == '.')
        {
	  strcpy (p, section_desc);
	  p += strlen (section_desc);
	  used_desc = 1;
        }

      else if (isalnum (*q))
        *p++ = *q;
    }

  if (! used_desc)
    strcpy (p, section_desc);
  else
    *p = '\0';
}

/* Write function profiler code. */

void
output_function_profiler (file, labelno)
  FILE *file;
  int labelno;
{
  /* The last used parameter register.  */
  int last_parm_reg;
  int i, j;

  /* Set up a TOC entry for the profiler label.  */
  toc_section ();
  fprintf (file, "LPC..%d:\n\t.tc\tLP..%d[TC],LP..%d\n",
	   labelno, labelno, labelno);
  text_section ();

  /* Figure out last used parameter register.  The proper thing to do is
     to walk incoming args of the function.  A function might have live
     parameter registers even if it has no incoming args.  */

  for (last_parm_reg = 10;
       last_parm_reg > 2 && ! regs_ever_live [last_parm_reg];
       last_parm_reg--)
    ;

  /* Save parameter registers in regs 23-30.  Don't overwrite reg 31, since
     it might be set up as the frame pointer.  */

  for (i = 3, j = 30; i <= last_parm_reg; i++, j--)
    fprintf (file, "\tai %d,%d,0\n", j, i);

  /* Load location address into r3, and call mcount.  */

  fprintf (file, "\tl 3,LPC..%d(2)\n\tbl .mcount\n", labelno);

  /* Restore parameter registers.  */

  for (i = 3, j = 30; i <= last_parm_reg; i++, j--)
    fprintf (file, "\tai %d,%d,0\n", i, j);
}
