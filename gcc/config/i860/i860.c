/* Subroutines for insn-output.c for Intel i860
   Copyright (C) 1989, 1991, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
   2005
   Free Software Foundation, Inc.
   Derived from sparc.c.

   Written by Richard Stallman (rms@ai.mit.edu).

   Hacked substantially by Ron Guilmette (rfg@netcom.com) to cater
   to the whims of the System V Release 4 assembler.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "flags.h"
#include "rtl.h"
#include "tree.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "recog.h"
#include "insn-attr.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "toplev.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"
#include "tree-gimple.h"

static rtx find_addr_reg (rtx);

#ifndef I860_REG_PREFIX
#define I860_REG_PREFIX ""
#endif

const char *i860_reg_prefix = I860_REG_PREFIX;

/* Save information from a "cmpxx" operation until the branch is emitted.  */

rtx i860_compare_op0, i860_compare_op1;

/* Return nonzero if this pattern, can be evaluated safely, even if it
   was not asked for.  */
int
safe_insn_src_p (rtx op, enum machine_mode mode)
{
  /* Just experimenting.  */

  /* No floating point source is safe if it contains an arithmetic
     operation, since that operation may trap.  */
  switch (GET_CODE (op))
    {
    case CONST_INT:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
      return 1;

    case REG:
      return 1;

    case MEM:
      return CONSTANT_ADDRESS_P (XEXP (op, 0));

      /* We never need to negate or complement constants.  */
    case NEG:
      return (mode != SFmode && mode != DFmode);
    case NOT:
    case ZERO_EXTEND:
      return 1;

    case EQ:
    case NE:
    case LT:
    case GT:
    case LE:
    case GE:
    case LTU:
    case GTU:
    case LEU:
    case GEU:
    case MINUS:
    case PLUS:
      return (mode != SFmode && mode != DFmode);
    case AND:
    case IOR:
    case XOR:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if ((GET_CODE (XEXP (op, 0)) == CONST_INT && ! SMALL_INT (XEXP (op, 0)))
	  || (GET_CODE (XEXP (op, 1)) == CONST_INT && ! SMALL_INT (XEXP (op, 1))))
	return 0;
      return 1;

    default:
      return 0;
    }
}

/* Return 1 if REG is clobbered in IN.
   Return 2 if REG is used in IN. 
   Return 3 if REG is both used and clobbered in IN.
   Return 0 if none of the above.  */

static int
reg_clobbered_p (rtx reg, rtx in)
{
  register enum rtx_code code;

  if (in == 0)
    return 0;

  code = GET_CODE (in);

  if (code == SET || code == CLOBBER)
    {
      rtx dest = SET_DEST (in);
      int set = 0;
      int used = 0;

      while (GET_CODE (dest) == STRICT_LOW_PART
	     || GET_CODE (dest) == SUBREG
	     || GET_CODE (dest) == ZERO_EXTRACT)
	dest = XEXP (dest, 0);

      if (dest == reg)
	set = 1;
      else if (GET_CODE (dest) == REG
	       && refers_to_regno_p (REGNO (reg),
				     REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
				     SET_DEST (in), 0))
	{
	  set = 1;
	  /* Anything that sets just part of the register
	     is considered using as well as setting it.
	     But note that a straight SUBREG of a single-word value
	     clobbers the entire value.  */
	  if (dest != SET_DEST (in)
	      && ! (GET_CODE (SET_DEST (in)) == SUBREG
		    || UNITS_PER_WORD >= GET_MODE_SIZE (GET_MODE (dest))))
	    used = 1;
	}

      if (code == SET)
	{
	  if (set)
	    used = refers_to_regno_p (REGNO (reg),
				      REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
				      SET_SRC (in), 0);
	  else
	    used = refers_to_regno_p (REGNO (reg),
				      REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
				      in, 0);
	}

      return set + used * 2;
    }

  if (refers_to_regno_p (REGNO (reg),
			 REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
			 in, 0))
    return 2;
  return 0;
}

/* Return nonzero if OP can be written to without screwing up
   GCC's model of what's going on.  It is assumed that this operand
   appears in the dest position of a SET insn in a conditional
   branch's delay slot.  AFTER is the label to start looking from.  */
int
operand_clobbered_before_used_after (rtx op, rtx after)
{
  /* Just experimenting.  */
  if (GET_CODE (op) == CC0)
    return 1;
  if (GET_CODE (op) == REG)
    {
      rtx insn;

      if (op == stack_pointer_rtx)
	return 0;

      /* Scan forward from the label, to see if the value of OP
	 is clobbered before the first use.  */

      for (insn = NEXT_INSN (after); insn; insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == NOTE)
	    continue;
	  if (GET_CODE (insn) == INSN
	      || GET_CODE (insn) == JUMP_INSN
	      || GET_CODE (insn) == CALL_INSN)
	    {
	      switch (reg_clobbered_p (op, PATTERN (insn)))
		{
		default:
		  return 0;
		case 1:
		  return 1;
		case 0:
		  break;
		}
	    }
	  /* If we reach another label without clobbering OP,
	     then we cannot safely write it here.  */
	  else if (GET_CODE (insn) == CODE_LABEL)
	    return 0;
	  if (GET_CODE (insn) == JUMP_INSN)
	    {
	      if (condjump_p (insn))
		return 0;
	      /* This is a jump insn which has already
		 been mangled.  We can't tell what it does.  */
	      if (GET_CODE (PATTERN (insn)) == PARALLEL)
		return 0;
	      if (! JUMP_LABEL (insn))
		return 0;
	      /* Keep following jumps.  */
	      insn = JUMP_LABEL (insn);
	    }
	}
      return 1;
    }

  /* In both of these cases, the first insn executed
     for this op will be a orh whatever%h,%r0,%r31,
     which is tolerable.  */
  if (GET_CODE (op) == MEM)
    return (CONSTANT_ADDRESS_P (XEXP (op, 0)));

  return 0;
}


/* Return nonzero only if OP is a register of mode MODE,
   or const0_rtx.  */
int
reg_or_0_operand (rtx op, enum machine_mode mode)
{
  return (op == const0_rtx || register_operand (op, mode)
	  || op == CONST0_RTX (mode));
}

/* Return truth value of whether OP can be used as an operands in a three
   address add/subtract insn (such as add %o1,7,%l2) of mode MODE.  */

int
arith_operand (rtx op, enum machine_mode mode)
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SMALL_INT (op)));
}

/* Return 1 if OP is a valid first operand for a logical insn of mode MODE.  */

int
logic_operand (rtx op, enum machine_mode mode)
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && LOGIC_INT (op)));
}

/* Return 1 if OP is a valid first operand for a shift insn of mode MODE.  */

int
shift_operand (rtx op, enum machine_mode mode)
{
  return (register_operand (op, mode)
          || (GET_CODE (op) == CONST_INT));
}

/* Return 1 if OP is a valid first operand for either a logical insn
   or an add insn of mode MODE.  */

int
compare_operand (rtx op, enum machine_mode mode)
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SMALL_INT (op) && LOGIC_INT (op)));
}

/* Return truth value of whether OP can be used as the 5-bit immediate
   operand of a bte or btne insn.  */

int
bte_operand (rtx op, enum machine_mode mode)
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (unsigned) INTVAL (op) < 0x20));
}

/* Return 1 if OP is an indexed memory reference of mode MODE.  */

int
indexed_operand (rtx op, enum machine_mode mode)
{
  return (GET_CODE (op) == MEM && GET_MODE (op) == mode
	  && GET_CODE (XEXP (op, 0)) == PLUS
	  && GET_MODE (XEXP (op, 0)) == SImode
	  && register_operand (XEXP (XEXP (op, 0), 0), SImode)
	  && register_operand (XEXP (XEXP (op, 0), 1), SImode));
}

/* Return 1 if OP is a suitable source operand for a load insn
   with mode MODE.  */

int
load_operand (rtx op, enum machine_mode mode)
{
  return (memory_operand (op, mode) || indexed_operand (op, mode));
}

/* Return truth value of whether OP is an integer which fits the
   range constraining immediate operands in add/subtract insns.  */

int
small_int (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return (GET_CODE (op) == CONST_INT && SMALL_INT (op));
}

/* Return truth value of whether OP is an integer which fits the
   range constraining immediate operands in logic insns.  */

int
logic_int (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return (GET_CODE (op) == CONST_INT && LOGIC_INT (op));
}

/* Test for a valid operand for a call instruction.
   Don't allow the arg pointer register or virtual regs
   since they may change into reg + const, which the patterns
   can't handle yet.  */

int
call_insn_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (op) == MEM
      && (CONSTANT_ADDRESS_P (XEXP (op, 0))
	  || (GET_CODE (XEXP (op, 0)) == REG
	      && XEXP (op, 0) != arg_pointer_rtx
	      && !(REGNO (XEXP (op, 0)) >= FIRST_PSEUDO_REGISTER
		   && REGNO (XEXP (op, 0)) <= LAST_VIRTUAL_REGISTER))))
    return 1;
  return 0;
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

static const char *
singlemove_string (rtx *operands)
{
  if (GET_CODE (operands[0]) == MEM)
    {
      if (GET_CODE (operands[1]) != MEM)
	if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	  {
	    if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
		   && (cc_prev_status.flags & CC_HI_R31_ADJ)
		   && cc_prev_status.mdep == XEXP (operands[0], 0)))
	      {
		CC_STATUS_INIT;
	        output_asm_insn ("orh %h0,%?r0,%?r31", operands);
	      }
	    cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
	    cc_status.mdep = XEXP (operands[0], 0);
	    return "st.l %r1,%L0(%?r31)";
	  }
	else
	  return "st.l %r1,%0";
      else
	abort ();
#if 0
	{
	  rtx xoperands[2];

	  cc_status.flags &= ~CC_F0_IS_0;
	  xoperands[0] = gen_rtx_REG (SFmode, 32);
	  xoperands[1] = operands[1];
	  output_asm_insn (singlemove_string (xoperands), xoperands);
	  xoperands[1] = xoperands[0];
	  xoperands[0] = operands[0];
	  output_asm_insn (singlemove_string (xoperands), xoperands);
	  return "";
	}
#endif
    }
  if (GET_CODE (operands[1]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
		 && (cc_prev_status.flags & CC_HI_R31_ADJ)
		 && cc_prev_status.mdep == XEXP (operands[1], 0)))
	    {
	      CC_STATUS_INIT;
	      output_asm_insn ("orh %h1,%?r0,%?r31", operands);
	    }
	  cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
	  cc_status.mdep = XEXP (operands[1], 0);
	  return "ld.l %L1(%?r31),%0";
	}
      return "ld.l %m1,%0";
    }
 if (GET_CODE (operands[1]) == CONST_INT)
   {
     if (operands[1] == const0_rtx)
      return "mov %?r0,%0";
     if((INTVAL (operands[1]) & 0xffff0000) == 0)
      return "or %L1,%?r0,%0";
     if((INTVAL (operands[1]) & 0xffff8000) == 0xffff8000)
      return "adds %1,%?r0,%0";
     if((INTVAL (operands[1]) & 0x0000ffff) == 0)
      return "orh %H1,%?r0,%0";

     return "orh %H1,%?r0,%0\n\tor %L1,%0,%0";
   }
  return "mov %1,%0";
}

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

const char *
output_move_double (rtx *operands)
{
  enum { REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;
  int highest_first = 0;
  int no_addreg1_decrement = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (operands[0]) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1]))
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP)
    abort ();

  /* If an operand is an unoffsettable memory reference, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (operands[0], 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (operands[1], 0));

/* ??? Perhaps in some cases move double words
   if there is a spare pair of floating regs.  */

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first,
     but if either operand is autodecrementing then we
     do the high-numbered word first.

     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx_REG (SImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adjust_address (operands[0], SImode, 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx_REG (SImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adjust_address (operands[1], SImode, 4);
  else if (optype1 == CNSTOP)
    {
      if (GET_CODE (operands[1]) == CONST_DOUBLE)
	split_double (operands[1], &operands[1], &latehalf[1]);
#if 0
      else if (CONSTANT_P (operands[1]))
	latehalf[1] = const0_rtx;
#else
      else if (CONSTANT_P (operands[1]))
        split_double (operands[1], &operands[1], &latehalf[1]);
#endif
    }
  else
    latehalf[1] = operands[1];

  /* If the first move would clobber the source of the second one,
     do them in the other order.

     RMS says "This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance."

     But it happens on the sparc when loading parameter registers,
     so I am going to define that circumstance, and make it work
     as expected.  */

  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (latehalf[1]))
    {
      CC_STATUS_PARTIAL_INIT;
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	output_asm_insn ("adds 0x4,%0,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("adds 0x4,%0,%0", &addreg1);

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("adds -0x4,%0,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("adds -0x4,%0,%0", &addreg1);

      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }
  else if (optype0 == REGOP && optype1 != REGOP
	   && reg_overlap_mentioned_p (operands[0], operands[1]))
    {
      /* If both halves of dest are used in the src memory address,
	 add the two regs and put them in the low reg (operands[0]).
	 Then it works to load latehalf first.  */
      if (reg_mentioned_p (operands[0], XEXP (operands[1], 0))
	  && reg_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
	{
	  rtx xops[2];
	  xops[0] = latehalf[0];
	  xops[1] = operands[0];
	  output_asm_insn ("adds %1,%0,%1", xops);
	  operands[1] = gen_rtx_MEM (DImode, operands[0]);
	  latehalf[1] = adjust_address (operands[1], SImode, 4);
	  addreg1 = 0;
	  highest_first = 1;
	}
      /* Only one register in the dest is used in the src memory address,
	 and this is the first register of the dest, so we want to do
	 the late half first here also.  */
      else if (! reg_mentioned_p (latehalf[0], XEXP (operands[1], 0)))
	highest_first = 1;
      /* Only one register in the dest is used in the src memory address,
	 and this is the second register of the dest, so we want to do
	 the late half last.  If addreg1 is set, and addreg1 is the same
	 register as latehalf, then we must suppress the trailing decrement,
	 because it would clobber the value just loaded.  */
      else if (addreg1 && reg_mentioned_p (addreg1, latehalf[0]))
	no_addreg1_decrement = 1;
    }

  /* Normal case: do the two words, low-numbered first.
     Overlap case (highest_first set): do high-numbered word first.  */

  if (! highest_first)
    output_asm_insn (singlemove_string (operands), operands);

  CC_STATUS_PARTIAL_INIT;
  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("adds 0x4,%0,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("adds 0x4,%0,%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("adds -0x4,%0,%0", &addreg0);
  if (addreg1 && !no_addreg1_decrement)
    output_asm_insn ("adds -0x4,%0,%0", &addreg1);

  if (highest_first)
    output_asm_insn (singlemove_string (operands), operands);

  return "";
}

const char *
output_fp_move_double (rtx *operands)
{
  /* If the source operand is any sort of zero, use f0 instead.  */

  if (operands[1] == CONST0_RTX (GET_MODE (operands[1])))
    operands[1] = gen_rtx_REG (DFmode, F0_REGNUM);

  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]))
	return "fmov.dd %1,%0";
      if (GET_CODE (operands[1]) == REG)
	{
	  output_asm_insn ("ixfr %1,%0", operands);
	  operands[0] = gen_rtx_REG (VOIDmode, REGNO (operands[0]) + 1);
	  operands[1] = gen_rtx_REG (VOIDmode, REGNO (operands[1]) + 1);
	  return "ixfr %1,%0";
	}
      if (operands[1] == CONST0_RTX (DFmode))
	return "fmov.dd f0,%0";
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
		 && (cc_prev_status.flags & CC_HI_R31_ADJ)
		 && cc_prev_status.mdep == XEXP (operands[1], 0)))
	    {
	      CC_STATUS_INIT;
	      output_asm_insn ("orh %h1,%?r0,%?r31", operands);
	    }
	  cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
	  cc_status.mdep = XEXP (operands[1], 0);
	  return "fld.d %L1(%?r31),%0";
	}
      return "fld.d %1,%0";
    }
  else if (FP_REG_P (operands[1]))
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  output_asm_insn ("fxfr %1,%0", operands);
	  operands[0] = gen_rtx_REG (VOIDmode, REGNO (operands[0]) + 1);
	  operands[1] = gen_rtx_REG (VOIDmode, REGNO (operands[1]) + 1);
	  return "fxfr %1,%0";
	}
      if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
		 && (cc_prev_status.flags & CC_HI_R31_ADJ)
		 && cc_prev_status.mdep == XEXP (operands[0], 0)))
	    {
	      CC_STATUS_INIT;
	      output_asm_insn ("orh %h0,%?r0,%?r31", operands);
	    }
	  cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
	  cc_status.mdep = XEXP (operands[0], 0);
	  return "fst.d %1,%L0(%?r31)";
	}
      return "fst.d %1,%0";
    }
  else
    abort ();
  /* NOTREACHED */
  return NULL;
}

/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.  */

static rtx
find_addr_reg (rtx addr)
{
  while (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	abort ();
    }
  if (GET_CODE (addr) == REG)
    return addr;
  abort ();
  /* NOTREACHED */
  return NULL;
}

/* Return a template for a load instruction with mode MODE and
   arguments from the string ARGS.

  This string is in static storage.  */

static const char *
load_opcode (enum machine_mode mode, const char *args, rtx reg)
{
  static char buf[30];
  const char *opcode;

  switch (mode)
    {
    case QImode:
      opcode = "ld.b";
      break;

    case HImode:
      opcode = "ld.s";
      break;

    case SImode:
    case SFmode:
      if (FP_REG_P (reg))
	opcode = "fld.l";
      else
	opcode = "ld.l";
      break;

    case DImode:
      if (!FP_REG_P (reg))
	abort ();
    case DFmode:
      opcode = "fld.d";
      break;

    default:
      abort ();
    }

  sprintf (buf, "%s %s", opcode, args);
  return buf;
}

/* Return a template for a store instruction with mode MODE and
   arguments from the string ARGS.

   This string is in static storage.  */

static const char *
store_opcode (enum machine_mode mode, const char *args, rtx reg)
{
  static char buf[30];
  const char *opcode;

  switch (mode)
    {
    case QImode:
      opcode = "st.b";
      break;

    case HImode:
      opcode = "st.s";
      break;

    case SImode:
    case SFmode:
      if (FP_REG_P (reg))
	opcode = "fst.l";
      else
	opcode = "st.l";
      break;

    case DImode:
      if (!FP_REG_P (reg))
	abort ();
    case DFmode:
      opcode = "fst.d";
      break;

    default:
      abort ();
    }

  sprintf (buf, "%s %s", opcode, args);
  return buf;
}

/* Output a store-in-memory whose operands are OPERANDS[0,1].
   OPERANDS[0] is a MEM, and OPERANDS[1] is a reg or zero.

   This function returns a template for an insn.
   This is in static storage.

   It may also output some insns directly.
   It may alter the values of operands[0] and operands[1].  */

const char *
output_store (rtx *operands)
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx address = XEXP (operands[0], 0);

  cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
  cc_status.mdep = address;

  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
	 && (cc_prev_status.flags & CC_HI_R31_ADJ)
	 && address == cc_prev_status.mdep))
    {
      CC_STATUS_INIT;
      output_asm_insn ("orh %h0,%?r0,%?r31", operands);
      cc_prev_status.mdep = address;
    }

  /* Store zero in two parts when appropriate.  */
  if (mode == DFmode && operands[1] == CONST0_RTX (DFmode))
    return store_opcode (DFmode, "%r1,%L0(%?r31)", operands[1]);

  /* Code below isn't smart enough to move a doubleword in two parts,
     so use output_move_double to do that in the cases that require it.  */
  if ((mode == DImode || mode == DFmode)
      && ! FP_REG_P (operands[1]))
    return output_move_double (operands);

  return store_opcode (mode, "%r1,%L0(%?r31)", operands[1]);
}

/* Output a load-from-memory whose operands are OPERANDS[0,1].
   OPERANDS[0] is a reg, and OPERANDS[1] is a mem.

   This function returns a template for an insn.
   This is in static storage.

   It may also output some insns directly.
   It may alter the values of operands[0] and operands[1].  */

const char *
output_load (rtx *operands)
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx address = XEXP (operands[1], 0);

  /* We don't bother trying to see if we know %hi(address).
     This is because we are doing a load, and if we know the
     %hi value, we probably also know that value in memory.  */
  cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
  cc_status.mdep = address;

  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
	 && (cc_prev_status.flags & CC_HI_R31_ADJ)
	 && address == cc_prev_status.mdep
	 && cc_prev_status.mdep == cc_status.mdep))
    {
      CC_STATUS_INIT;
      output_asm_insn ("orh %h1,%?r0,%?r31", operands);
      cc_prev_status.mdep = address;
    }

  /* Code below isn't smart enough to move a doubleword in two parts,
     so use output_move_double to do that in the cases that require it.  */
  if ((mode == DImode || mode == DFmode)
      && ! FP_REG_P (operands[0]))
    return output_move_double (operands);

  return load_opcode (mode, "%L1(%?r31),%0", operands[0]);
}

#if 0
/* Load the address specified by OPERANDS[3] into the register
   specified by OPERANDS[0].

   OPERANDS[3] may be the result of a sum, hence it could either be:

   (1) CONST
   (2) REG
   (2) REG + CONST_INT
   (3) REG + REG + CONST_INT
   (4) REG + REG  (special case of 3).

   Note that (3) is not a legitimate address.
   All cases are handled here.  */

void
output_load_address (rtx *operands)
{
  rtx base, offset;

  if (CONSTANT_P (operands[3]))
    {
      output_asm_insn ("mov %3,%0", operands);
      return;
    }

  if (REG_P (operands[3]))
    {
      if (REGNO (operands[0]) != REGNO (operands[3]))
	output_asm_insn ("shl %?r0,%3,%0", operands);
      return;
    }

  if (GET_CODE (operands[3]) != PLUS)
    abort ();

  base = XEXP (operands[3], 0);
  offset = XEXP (operands[3], 1);

  if (GET_CODE (base) == CONST_INT)
    {
      rtx tmp = base;
      base = offset;
      offset = tmp;
    }

  if (GET_CODE (offset) != CONST_INT)
    {
      /* Operand is (PLUS (REG) (REG)).  */
      base = operands[3];
      offset = const0_rtx;
    }

  if (REG_P (base))
    {
      operands[6] = base;
      operands[7] = offset;
      CC_STATUS_PARTIAL_INIT;
      if (SMALL_INT (offset))
	output_asm_insn ("adds %7,%6,%0", operands);
      else
	output_asm_insn ("mov %7,%0\n\tadds %0,%6,%0", operands);
    }
  else if (GET_CODE (base) == PLUS)
    {
      operands[6] = XEXP (base, 0);
      operands[7] = XEXP (base, 1);
      operands[8] = offset;

      CC_STATUS_PARTIAL_INIT;
      if (SMALL_INT (offset))
	output_asm_insn ("adds %6,%7,%0\n\tadds %8,%0,%0", operands);
      else
	output_asm_insn ("mov %8,%0\n\tadds %0,%6,%0\n\tadds %0,%7,%0", operands);
    }
  else
    abort ();
}
#endif

/* Output code to place a size count SIZE in register REG.
   Because block moves are pipelined, we don't include the
   first element in the transfer of SIZE to REG.
   For this, we subtract ALIGN.  (Actually, I think it is not
   right to subtract on this machine, so right now we don't.)  */

static void
output_size_for_block_move (rtx size, rtx reg, rtx align)
{
  rtx xoperands[3];

  xoperands[0] = reg;
  xoperands[1] = size;
  xoperands[2] = align;

#if 1
  cc_status.flags &= ~ CC_KNOW_HI_R31;
  output_asm_insn (singlemove_string (xoperands), xoperands);
#else
  if (GET_CODE (size) == REG)
    output_asm_insn ("sub %2,%1,%0", xoperands);
  else
    {
      xoperands[1] = GEN_INT (INTVAL (size) - INTVAL (align));
      cc_status.flags &= ~ CC_KNOW_HI_R31;
      output_asm_insn ("mov %1,%0", xoperands);
    }
#endif
}

/* Emit code to perform a block move.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.
   OPERANDS[2] is the size.
   OPERANDS[3] is the known safe alignment.
   OPERANDS[4..6] are pseudos we can safely clobber as temps.  */

const char *
output_block_move (rtx *operands)
{
  /* A vector for our computed operands.  Note that load_output_address
     makes use of (and can clobber) up to the 8th element of this vector.  */
  rtx xoperands[10];
#if 0
  rtx zoperands[10];
#endif
  static int movmemsi_label = 0;
  int i;
  rtx temp1 = operands[4];
  rtx alignrtx = operands[3];
  int align = INTVAL (alignrtx);
  int chunk_size;

  xoperands[0] = operands[0];
  xoperands[1] = operands[1];
  xoperands[2] = temp1;

  /* We can't move more than four bytes at a time
     because we have only one register to move them through.  */
  if (align > 4)
    {
      align = 4;
      alignrtx = GEN_INT (4);
    }

  /* Recognize special cases of block moves.  These occur
     when GNU C++ is forced to treat something as BLKmode
     to keep it in memory, when its mode could be represented
     with something smaller.

     We cannot do this for global variables, since we don't know
     what pages they don't cross.  Sigh.  */
  if (GET_CODE (operands[2]) == CONST_INT
      && ! CONSTANT_ADDRESS_P (operands[0])
      && ! CONSTANT_ADDRESS_P (operands[1]))
    {
      int size = INTVAL (operands[2]);
      rtx op0 = xoperands[0];
      rtx op1 = xoperands[1];

      if ((align & 3) == 0 && (size & 3) == 0 && (size >> 2) <= 16)
	{
	  if (memory_address_p (SImode, plus_constant (op0, size))
	      && memory_address_p (SImode, plus_constant (op1, size)))
	    {
	      cc_status.flags &= ~CC_KNOW_HI_R31;
	      for (i = (size>>2)-1; i >= 0; i--)
		{
		  xoperands[0] = plus_constant (op0, i * 4);
		  xoperands[1] = plus_constant (op1, i * 4);
		  output_asm_insn ("ld.l %a1,%?r31\n\tst.l %?r31,%a0",
				   xoperands);
		}
	      return "";
	    }
	}
      else if ((align & 1) == 0 && (size & 1) == 0 && (size >> 1) <= 16)
	{
	  if (memory_address_p (HImode, plus_constant (op0, size))
	      && memory_address_p (HImode, plus_constant (op1, size)))
	    {
	      cc_status.flags &= ~CC_KNOW_HI_R31;
	      for (i = (size>>1)-1; i >= 0; i--)
		{
		  xoperands[0] = plus_constant (op0, i * 2);
		  xoperands[1] = plus_constant (op1, i * 2);
		  output_asm_insn ("ld.s %a1,%?r31\n\tst.s %?r31,%a0",
				   xoperands);
		}
	      return "";
	    }
	}
      else if (size <= 16)
	{
	  if (memory_address_p (QImode, plus_constant (op0, size))
	      && memory_address_p (QImode, plus_constant (op1, size)))
	    {
	      cc_status.flags &= ~CC_KNOW_HI_R31;
	      for (i = size-1; i >= 0; i--)
		{
		  xoperands[0] = plus_constant (op0, i);
		  xoperands[1] = plus_constant (op1, i);
		  output_asm_insn ("ld.b %a1,%?r31\n\tst.b %?r31,%a0",
				   xoperands);
		}
	      return "";
	    }
	}
    }

  /* Since we clobber untold things, nix the condition codes.  */
  CC_STATUS_INIT;

  /* This is the size of the transfer.
     Either use the register which already contains the size,
     or use a free register (used by no operands).  */
  output_size_for_block_move (operands[2], operands[4], alignrtx);

#if 0
  /* Also emit code to decrement the size value by ALIGN.  */
  zoperands[0] = operands[0];
  zoperands[3] = plus_constant (operands[0], align);
  output_load_address (zoperands);
#endif

  /* Generate number for unique label.  */

  xoperands[3] = GEN_INT (movmemsi_label++);

  /* Calculate the size of the chunks we will be trying to move first.  */

#if 0
  if ((align & 3) == 0)
    chunk_size = 4;
  else if ((align & 1) == 0)
    chunk_size = 2;
  else
#endif
    chunk_size = 1;

  /* Copy the increment (negative) to a register for bla insn.  */

  xoperands[4] = GEN_INT (- chunk_size);
  xoperands[5] = operands[5];
  output_asm_insn ("adds %4,%?r0,%5", xoperands);

  /* Predecrement the loop counter.  This happens again also in the `bla'
     instruction which precedes the loop, but we need to have it done
     two times before we enter the loop because of the bizarre semantics
     of the bla instruction.  */

  output_asm_insn ("adds %5,%2,%2", xoperands);

  /* Check for the case where the original count was less than or equal to
     zero.  Avoid going through the loop at all if the original count was
     indeed less than or equal to zero.  Note that we treat the count as
     if it were a signed 32-bit quantity here, rather than an unsigned one,
     even though we really shouldn't.  We have to do this because of the
     semantics of the `ble' instruction, which assume that the count is
     a signed 32-bit value.  Anyway, in practice it won't matter because
     nobody is going to try to do a memcpy() of more than half of the
     entire address space (i.e. 2 gigabytes) anyway.  */

  output_asm_insn ("bc .Le%3", xoperands);

  /* Make available a register which is a temporary.  */

  xoperands[6] = operands[6];

  /* Now the actual loop.
     In xoperands, elements 1 and 0 are the input and output vectors.
     Element 2 is the loop index.  Element 5 is the increment.  */

  output_asm_insn ("subs %1,%5,%1", xoperands);
  output_asm_insn ("bla %5,%2,.Lm%3", xoperands);
  output_asm_insn ("adds %0,%2,%6", xoperands);
  output_asm_insn ("\n.Lm%3:", xoperands);	    /* Label for bla above.  */
  output_asm_insn ("\n.Ls%3:",  xoperands);	    /* Loop start label.  */
  output_asm_insn ("adds %5,%6,%6", xoperands);

  /* NOTE:  The code here which is supposed to handle the cases where the
     sources and destinations are known to start on a 4 or 2 byte boundary
     are currently broken.  They fail to do anything about the overflow
     bytes which might still need to be copied even after we have copied
     some number of words or halfwords.  Thus, for now we use the lowest
     common denominator, i.e. the code which just copies some number of
     totally unaligned individual bytes.  (See the calculation of
     chunk_size above.  */

  if (chunk_size == 4)
    {
      output_asm_insn ("ld.l %2(%1),%?r31", xoperands);
      output_asm_insn ("bla %5,%2,.Ls%3", xoperands);
      output_asm_insn ("st.l %?r31,8(%6)", xoperands);
    }
  else if (chunk_size == 2)
    {
      output_asm_insn ("ld.s %2(%1),%?r31", xoperands);
      output_asm_insn ("bla %5,%2,.Ls%3", xoperands);
      output_asm_insn ("st.s %?r31,4(%6)", xoperands);
    }
  else /* chunk_size == 1 */
    {
      output_asm_insn ("ld.b %2(%1),%?r31", xoperands);
      output_asm_insn ("bla %5,%2,.Ls%3", xoperands);
      output_asm_insn ("st.b %?r31,2(%6)", xoperands);
    }
  output_asm_insn ("\n.Le%3:", xoperands);	    /* Here if count <= 0.  */

  return "";
}

/* Special routine to convert an SFmode value represented as a
   CONST_DOUBLE into its equivalent unsigned long bit pattern.
   We convert the value from a double precision floating-point
   value to single precision first, and thence to a bit-wise
   equivalent unsigned long value.  This routine is used when
   generating an immediate move of an SFmode value directly
   into a general register because the SVR4 assembler doesn't
   grok floating literals in instruction operand contexts.  */

unsigned long
sfmode_constant_to_ulong (rtx x)
{
  REAL_VALUE_TYPE d;
  unsigned long l;

  if (GET_CODE (x) != CONST_DOUBLE || GET_MODE (x) != SFmode)
    abort ();

  REAL_VALUE_FROM_CONST_DOUBLE (d, x);
  REAL_VALUE_TO_TARGET_SINGLE (d, l);
  return l;
}

/* This function generates the assembly code for function entry.

   ASM_FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.

   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.

   NOTE: `frame_lower_bytes' is the count of bytes which will lie
   between the new `fp' value and the new `sp' value after the
   prologue is done.  `frame_upper_bytes' is the count of bytes
   that will lie between the new `fp' and the *old* `sp' value
   after the new `fp' is setup (in the prologue).  The upper
   part of each frame always includes at least 2 words (8 bytes)
   to hold the saved frame pointer and the saved return address.

   The SVR4 ABI for the i860 now requires that the values of the
   stack pointer and frame pointer registers be kept aligned to
   16-byte boundaries at all times.  We obey that restriction here.

   The SVR4 ABI for the i860 is entirely vague when it comes to specifying
   exactly where the "preserved" registers should be saved.  The native
   SVR4 C compiler I now have doesn't help to clarify the requirements
   very much because it is plainly out-of-date and non-ABI-compliant
   (in at least one important way, i.e. how it generates function
   epilogues).

   The native SVR4 C compiler saves the "preserved" registers (i.e.
   r4-r15 and f2-f7) in the lower part of a frame (i.e. at negative
   offsets from the frame pointer).

   Previous versions of GCC also saved the "preserved" registers in the
   "negative" part of the frame, but they saved them using positive
   offsets from the (adjusted) stack pointer (after it had been adjusted
   to allocate space for the new frame).  That's just plain wrong
   because if the current function calls alloca(), the stack pointer
   will get moved, and it will be impossible to restore the registers
   properly again after that.

   Both compilers handled parameter registers (i.e. r16-r27 and f8-f15)
   by copying their values either into various "preserved" registers or
   into stack slots in the lower part of the current frame (as seemed
   appropriate, depending upon subsequent usage of these values).

   Here we want to save the preserved registers at some offset from the
   frame pointer register so as to avoid any possible problems arising
   from calls to alloca().  We can either save them at small positive
   offsets from the frame pointer, or at small negative offsets from
   the frame pointer.  If we save them at small negative offsets from
   the frame pointer (i.e. in the lower part of the frame) then we
   must tell the rest of GCC (via STARTING_FRAME_OFFSET) exactly how
   many bytes of space we plan to use in the lower part of the frame
   for this purpose.  Since other parts of the compiler reference the
   value of STARTING_FRAME_OFFSET long before final() calls this function,
   we would have to go ahead and assume the worst-case storage requirements
   for saving all of the "preserved" registers (and use that number, i.e.
   `80', to define STARTING_FRAME_OFFSET) if we wanted to save them in
   the lower part of the frame.  That could potentially be very wasteful,
   and that wastefulness could really hamper people compiling for embedded
   i860 targets with very tight limits on stack space.  Thus, we choose
   here to save the preserved registers in the upper part of the
   frame, so that we can decide at the very last minute how much (or how
   little) space we must allocate for this purpose.

   To satisfy the needs of the SVR4 ABI "tdesc" scheme, preserved
   registers must always be saved so that the saved values of registers
   with higher numbers are at higher addresses.  We obey that restriction
   here.

   There are two somewhat different ways that you can generate prologues
   here... i.e. pedantically ABI-compliant, and the "other" way.  The
   "other" way is more consistent with what is currently generated by the
   "native" SVR4 C compiler for the i860.  That's important if you want
   to use the current (as of 8/91) incarnation of SVR4 SDB for the i860.
   The SVR4 SDB for the i860 insists on having function prologues be
   non-ABI-compliant!

   To get fully ABI-compliant prologues, define I860_STRICT_ABI_PROLOGUES
   in the i860/sysv4.h file.  (By default this is *not* defined).

   The differences between the ABI-compliant and non-ABI-compliant prologues
   are that (a) the ABI version seems to require the use of *signed*
   (rather than unsigned) adds and subtracts, and (b) the ordering of
   the various steps (e.g. saving preserved registers, saving the
   return address, setting up the new frame pointer value) is different.

   For strict ABI compliance, it seems to be the case that the very last
   thing that is supposed to happen in the prologue is getting the frame
   pointer set to its new value (but only after everything else has
   already been properly setup).  We do that here, but only if the symbol
   I860_STRICT_ABI_PROLOGUES is defined.  */

#ifndef STACK_ALIGNMENT
#define STACK_ALIGNMENT	16
#endif

const char *current_function_original_name;

static int must_preserve_r1;
static unsigned must_preserve_bytes;

static void
i860_output_function_prologue (FILE *asm_file, HOST_WIDE_INT local_bytes)
{
  register HOST_WIDE_INT frame_lower_bytes;
  register HOST_WIDE_INT frame_upper_bytes;
  register HOST_WIDE_INT total_fsize;
  register unsigned preserved_reg_bytes = 0;
  register unsigned i;
  register unsigned preserved_so_far = 0;

  must_preserve_r1 = (optimize < 2 || ! leaf_function_p ());
  must_preserve_bytes = 4 + (must_preserve_r1 ? 4 : 0);

  /* Count registers that need preserving.  Ignore r0.  It never needs
     preserving.  */

  for (i = 1; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (regs_ever_live[i] && ! call_used_regs[i])
        preserved_reg_bytes += 4;
    }

  /* Round-up the frame_lower_bytes so that it's a multiple of 16.  */

  frame_lower_bytes = (local_bytes + STACK_ALIGNMENT - 1) & -STACK_ALIGNMENT;

  /* The upper part of each frame will contain the saved fp,
     the saved r1, and stack slots for all of the other "preserved"
     registers that we find we will need to save & restore.  */

  frame_upper_bytes = must_preserve_bytes + preserved_reg_bytes;

  /* Round-up the frame_upper_bytes so that it's a multiple of 16.  */

  frame_upper_bytes
    = (frame_upper_bytes + STACK_ALIGNMENT - 1) & -STACK_ALIGNMENT;

  total_fsize = frame_upper_bytes + frame_lower_bytes;

#ifndef I860_STRICT_ABI_PROLOGUES

  /* There are two kinds of function prologues.
     You use the "small" version if the total frame size is
     small enough so that it can fit into an immediate 16-bit
     value in one instruction.  Otherwise, you use the "large"
     version of the function prologue.  */

  if (total_fsize > 0x7fff)
    {
      /* Adjust the stack pointer.  The ABI specifies using `adds' for
	 this, but the native C compiler on SVR4 uses `addu'.  */

      fprintf (asm_file, "\taddu -" HOST_WIDE_INT_PRINT_DEC ",%ssp,%ssp\n",
	frame_upper_bytes, i860_reg_prefix, i860_reg_prefix);

      /* Save the old frame pointer.  */

      fprintf (asm_file, "\tst.l %sfp,0(%ssp)\n",
	i860_reg_prefix, i860_reg_prefix);

      /* Setup the new frame pointer.  The ABI specifies that this is to
	 be done after preserving registers (using `adds'), but that's not
	 what the native C compiler on SVR4 does.  */

      fprintf (asm_file, "\taddu 0,%ssp,%sfp\n",
	i860_reg_prefix, i860_reg_prefix);

      /* Get the value of frame_lower_bytes into r31.  */

      fprintf (asm_file, "\torh " HOST_WIDE_INT_PRINT_DEC ",%sr0,%sr31\n",
	frame_lower_bytes >> 16, i860_reg_prefix, i860_reg_prefix);
      fprintf (asm_file, "\tor " HOST_WIDE_INT_PRINT_DEC ",%sr31,%sr31\n",
	frame_lower_bytes & 0xffff, i860_reg_prefix, i860_reg_prefix);

      /* Now re-adjust the stack pointer using the value in r31.
	 The ABI specifies that this is done with `subs' but SDB may
	 prefer `subu'.  */

      fprintf (asm_file, "\tsubu %ssp,%sr31,%ssp\n",
	i860_reg_prefix, i860_reg_prefix, i860_reg_prefix);

      /* Preserve registers.  The ABI specifies that this is to be done
	 before setting up the new frame pointer, but that's not what the
	 native C compiler on SVR4 does.  */

      for (i = 1; i < 32; i++)
        if (regs_ever_live[i] && ! call_used_regs[i])
          fprintf (asm_file, "\tst.l %s%s,%d(%sfp)\n",
	    i860_reg_prefix, reg_names[i],
	    must_preserve_bytes  + (4 * preserved_so_far++),
	    i860_reg_prefix);

      for (i = 32; i < 64; i++)
        if (regs_ever_live[i] && ! call_used_regs[i])
          fprintf (asm_file, "\tfst.l %s%s,%d(%sfp)\n",
	    i860_reg_prefix, reg_names[i],
	    must_preserve_bytes + (4 * preserved_so_far++),
	    i860_reg_prefix);

      /* Save the return address.  */

      if (must_preserve_r1)
        fprintf (asm_file, "\tst.l %sr1,4(%sfp)\n",
	  i860_reg_prefix, i860_reg_prefix);
    }
  else
    {
      /* Adjust the stack pointer.  The ABI specifies using `adds' for this,
	 but the native C compiler on SVR4 uses `addu'.  */

      fprintf (asm_file, "\taddu -" HOST_WIDE_INT_PRINT_DEC ",%ssp,%ssp\n",
	total_fsize, i860_reg_prefix, i860_reg_prefix);

      /* Save the old frame pointer.  */

      fprintf (asm_file, "\tst.l %sfp," HOST_WIDE_INT_PRINT_DEC "(%ssp)\n",
	i860_reg_prefix, frame_lower_bytes, i860_reg_prefix);

      /* Setup the new frame pointer.  The ABI specifies that this is to be
	 done after preserving registers and after saving the return address,
	 (and to do it using `adds'), but that's not what the native C
	 compiler on SVR4 does.  */

      fprintf (asm_file, "\taddu " HOST_WIDE_INT_PRINT_DEC ",%ssp,%sfp\n",
	frame_lower_bytes, i860_reg_prefix, i860_reg_prefix);

      /* Preserve registers.  The ABI specifies that this is to be done
	 before setting up the new frame pointer, but that's not what the
	 native compiler on SVR4 does.  */

      for (i = 1; i < 32; i++)
        if (regs_ever_live[i] && ! call_used_regs[i])
          fprintf (asm_file, "\tst.l %s%s,%d(%sfp)\n",
	    i860_reg_prefix, reg_names[i],
	    must_preserve_bytes + (4 * preserved_so_far++),
	    i860_reg_prefix);

      for (i = 32; i < 64; i++)
        if (regs_ever_live[i] && ! call_used_regs[i])
          fprintf (asm_file, "\tfst.l %s%s,%d(%sfp)\n",
	    i860_reg_prefix, reg_names[i],
	    must_preserve_bytes + (4 * preserved_so_far++),
	    i860_reg_prefix);

      /* Save the return address.  The ABI specifies that this is to be
	 done earlier, and also via an offset from %sp, but the native C
	 compiler on SVR4 does it later (i.e. now) and uses an offset from
	 %fp.  */

      if (must_preserve_r1)
        fprintf (asm_file, "\tst.l %sr1,4(%sfp)\n",
	  i860_reg_prefix, i860_reg_prefix);
    }

#else /* defined(I860_STRICT_ABI_PROLOGUES) */

  /* There are two kinds of function prologues.
     You use the "small" version if the total frame size is
     small enough so that it can fit into an immediate 16-bit
     value in one instruction.  Otherwise, you use the "large"
     version of the function prologue.  */

  if (total_fsize > 0x7fff)
    {
      /* Adjust the stack pointer (thereby allocating a new frame).  */

      fprintf (asm_file, "\tadds -%d,%ssp,%ssp\n",
	frame_upper_bytes, i860_reg_prefix, i860_reg_prefix);

      /* Save the caller's frame pointer.  */

      fprintf (asm_file, "\tst.l %sfp,0(%ssp)\n",
	i860_reg_prefix, i860_reg_prefix);

      /* Save return address.  */

      if (must_preserve_r1)
        fprintf (asm_file, "\tst.l %sr1,4(%ssp)\n",
	  i860_reg_prefix, i860_reg_prefix);

      /* Get the value of frame_lower_bytes into r31 for later use.  */

      fprintf (asm_file, "\torh %d,%sr0,%sr31\n",
	frame_lower_bytes >> 16, i860_reg_prefix, i860_reg_prefix);
      fprintf (asm_file, "\tor %d,%sr31,%sr31\n",
	frame_lower_bytes & 0xffff, i860_reg_prefix, i860_reg_prefix);

      /* Now re-adjust the stack pointer using the value in r31.  */

      fprintf (asm_file, "\tsubs %ssp,%sr31,%ssp\n",
	i860_reg_prefix, i860_reg_prefix, i860_reg_prefix);

      /* Pre-compute value to be used as the new frame pointer.  */

      fprintf (asm_file, "\tadds %ssp,%sr31,%sr31\n",
	i860_reg_prefix, i860_reg_prefix, i860_reg_prefix);

      /* Preserve registers.  */

      for (i = 1; i < 32; i++)
        if (regs_ever_live[i] && ! call_used_regs[i])
          fprintf (asm_file, "\tst.l %s%s,%d(%sr31)\n",
	    i860_reg_prefix, reg_names[i],
	    must_preserve_bytes + (4 * preserved_so_far++),
	    i860_reg_prefix);

      for (i = 32; i < 64; i++)
        if (regs_ever_live[i] && ! call_used_regs[i])
          fprintf (asm_file, "\tfst.l %s%s,%d(%sr31)\n",
	    i860_reg_prefix, reg_names[i],
	    must_preserve_bytes + (4 * preserved_so_far++),
	    i860_reg_prefix);

      /* Actually set the new value of the frame pointer.  */

      fprintf (asm_file, "\tmov %sr31,%sfp\n",
	i860_reg_prefix, i860_reg_prefix);
    }
  else
    {
      /* Adjust the stack pointer.  */

      fprintf (asm_file, "\tadds -%d,%ssp,%ssp\n",
	total_fsize, i860_reg_prefix, i860_reg_prefix);

      /* Save the caller's frame pointer.  */

      fprintf (asm_file, "\tst.l %sfp,%d(%ssp)\n",
	i860_reg_prefix, frame_lower_bytes, i860_reg_prefix);

      /* Save the return address.  */

      if (must_preserve_r1)
        fprintf (asm_file, "\tst.l %sr1,%d(%ssp)\n",
	  i860_reg_prefix, frame_lower_bytes + 4, i860_reg_prefix);

      /* Preserve registers.  */

      for (i = 1; i < 32; i++)
        if (regs_ever_live[i] && ! call_used_regs[i])
          fprintf (asm_file, "\tst.l %s%s,%d(%ssp)\n",
	    i860_reg_prefix, reg_names[i],
	    frame_lower_bytes + must_preserve_bytes + (4 * preserved_so_far++),
	    i860_reg_prefix);

      for (i = 32; i < 64; i++)
        if (regs_ever_live[i] && ! call_used_regs[i])
          fprintf (asm_file, "\tfst.l %s%s,%d(%ssp)\n",
	    i860_reg_prefix, reg_names[i],
	    frame_lower_bytes + must_preserve_bytes + (4 * preserved_so_far++),
	    i860_reg_prefix);

      /* Setup the new frame pointer.  */

      fprintf (asm_file, "\tadds %d,%ssp,%sfp\n",
	frame_lower_bytes, i860_reg_prefix, i860_reg_prefix);
    }
#endif /* defined(I860_STRICT_ABI_PROLOGUES) */

#ifdef ASM_OUTPUT_PROLOGUE_SUFFIX
  ASM_OUTPUT_PROLOGUE_SUFFIX (asm_file);
#endif /* defined(ASM_OUTPUT_PROLOGUE_SUFFIX) */
}

/* This function generates the assembly code for function exit.

   ASM_FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.

   Note that when we go to restore the preserved register values we must
   not try to address their slots by using offsets from the stack pointer.
   That's because the stack pointer may have been moved during the function
   execution due to a call to alloca().  Rather, we must restore all
   preserved registers via offsets from the frame pointer value.

   Note also that when the current frame is being "popped" (by adjusting
   the value of the stack pointer) on function exit, we must (for the
   sake of alloca) set the new value of the stack pointer based upon
   the current value of the frame pointer.  We can't just add what we
   believe to be the (static) frame size to the stack pointer because
   if we did that, and alloca() had been called during this function,
   we would end up returning *without* having fully deallocated all of
   the space grabbed by alloca.  If that happened, and a function
   containing one or more alloca() calls was called over and over again,
   then the stack would grow without limit!

   Finally note that the epilogues generated here are completely ABI
   compliant.  They go out of their way to insure that the value in
   the frame pointer register is never less than the value in the stack
   pointer register.  It's not clear why this relationship needs to be
   maintained at all times, but maintaining it only costs one extra
   instruction, so what the hell.  */

/* This corresponds to a version 4 TDESC structure. Lower numbered
   versions successively omit the last word of the structure. We
   don't try to handle version 5 here.  */

typedef struct TDESC_flags {
	int version:4;
	int reg_packing:1;
	int callable_block:1;
	int reserved:4;
	int fregs:6;	/* fp regs 2-7 */
	int iregs:16;	/* regs 0-15 */
} TDESC_flags;

typedef struct TDESC {
	TDESC_flags flags;
	int integer_reg_offset;		/* same as must_preserve_bytes */
	int floating_point_reg_offset;
	unsigned int positive_frame_size;	/* same as frame_upper_bytes */
	unsigned int negative_frame_size;	/* same as frame_lower_bytes */
} TDESC;

static void
i860_output_function_epilogue (FILE *asm_file, HOST_WIDE_INT local_bytes)
{
  register HOST_WIDE_INT frame_upper_bytes;
  register HOST_WIDE_INT frame_lower_bytes;
  register HOST_WIDE_INT preserved_reg_bytes = 0;
  register unsigned i;
  register unsigned restored_so_far = 0;
  register unsigned int_restored;
  register unsigned mask;
  unsigned intflags=0;
  register TDESC_flags *flags = (TDESC_flags *) &intflags;
#ifdef	OUTPUT_TDESC	/* Output an ABI-compliant TDESC entry */
  const char *long_op = integer_asm_op (4, TRUE);
#endif

  flags->version = 4;
  flags->reg_packing = 1;
  flags->iregs = 8;	/* old fp always gets saved */

  /* Round-up the frame_lower_bytes so that it's a multiple of 16.  */

  frame_lower_bytes = (local_bytes + STACK_ALIGNMENT - 1) & -STACK_ALIGNMENT;

  /* Count the number of registers that were preserved in the prologue.
     Ignore r0.  It is never preserved.  */

  for (i = 1; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (regs_ever_live[i] && ! call_used_regs[i])
        preserved_reg_bytes += 4;
    }

  /* The upper part of each frame will contain only saved fp,
     the saved r1, and stack slots for all of the other "preserved"
     registers that we find we will need to save & restore.  */

  frame_upper_bytes = must_preserve_bytes + preserved_reg_bytes;

  /* Round-up frame_upper_bytes so that t is a multiple of 16.  */

  frame_upper_bytes
    = (frame_upper_bytes + STACK_ALIGNMENT - 1) & -STACK_ALIGNMENT;

  /* Restore all of the "preserved" registers that need restoring.  */

  mask = 2;

  for (i = 1; i < 32; i++, mask<<=1)
    if (regs_ever_live[i] && ! call_used_regs[i]) {
      fprintf (asm_file, "\tld.l %d(%sfp),%s%s\n",
	must_preserve_bytes + (4 * restored_so_far++),
	i860_reg_prefix, i860_reg_prefix, reg_names[i]);
      if (i > 3 && i < 16)
	flags->iregs |= mask;
    }

  int_restored = restored_so_far;
  mask = 1;

  for (i = 32; i < 64; i++) {
    if (regs_ever_live[i] && ! call_used_regs[i]) {
      fprintf (asm_file, "\tfld.l %d(%sfp),%s%s\n",
	must_preserve_bytes + (4 * restored_so_far++),
	i860_reg_prefix, i860_reg_prefix, reg_names[i]);
      if (i > 33 && i < 40)
	flags->fregs |= mask;
    }
    if (i > 33 && i < 40)
      mask<<=1;
  }

  /* Get the value we plan to use to restore the stack pointer into r31.  */

  fprintf (asm_file, "\tadds " HOST_WIDE_INT_PRINT_DEC ",%sfp,%sr31\n",
    frame_upper_bytes, i860_reg_prefix, i860_reg_prefix);

  /* Restore the return address and the old frame pointer.  */

  if (must_preserve_r1) {
    fprintf (asm_file, "\tld.l 4(%sfp),%sr1\n",
      i860_reg_prefix, i860_reg_prefix);
    flags->iregs |= 2;
  }

  fprintf (asm_file, "\tld.l 0(%sfp),%sfp\n",
    i860_reg_prefix, i860_reg_prefix);

  /* Return and restore the old stack pointer value.  */

  fprintf (asm_file, "\tbri %sr1\n\tmov %sr31,%ssp\n",
    i860_reg_prefix, i860_reg_prefix, i860_reg_prefix);

#ifdef	OUTPUT_TDESC	/* Output an ABI-compliant TDESC entry.  */
  if (! frame_lower_bytes) {
    flags->version--;
    if (! frame_upper_bytes) {
      flags->version--;
      if (restored_so_far == int_restored)	/* No FP saves.  */
	flags->version--;
    }
  }
  assemble_name(asm_file,current_function_original_name);
  fputs(".TDESC:\n", asm_file);
  fprintf(asm_file, "%s 0x%0x\n", long_op, intflags);
  fprintf(asm_file, "%s %d\n", long_op,
	int_restored ? must_preserve_bytes : 0);
  if (flags->version > 1) {
    fprintf(asm_file, "%s %d\n", long_op,
	(restored_so_far == int_restored) ? 0 : must_preserve_bytes +
	  (4 * int_restored));
    if (flags->version > 2) {
      fprintf(asm_file, "%s %d\n", long_op, frame_upper_bytes);
      if (flags->version > 3)
	fprintf(asm_file, "%s %d\n", long_op, frame_lower_bytes);
    }
  }
  tdesc_section();
  fprintf(asm_file, "%s ", long_op);
  assemble_name(asm_file, current_function_original_name);
  fprintf(asm_file, "\n%s ", long_op);
  assemble_name(asm_file, current_function_original_name);
  fputs(".TDESC\n", asm_file);
  text_section();
#endif
}


/* Expand a library call to __builtin_saveregs.  */

static rtx
i860_saveregs (void)
{
  rtx fn = gen_rtx_SYMBOL_REF (Pmode, "__builtin_saveregs");
  rtx save = gen_reg_rtx (Pmode);
  rtx valreg = LIBCALL_VALUE (Pmode);
  rtx ret;

  /* The return value register overlaps the first argument register.
     Save and restore it around the call.  */
  emit_move_insn (save, valreg);
  ret = emit_library_call_value (fn, NULL_RTX, 1, Pmode, 0);
  if (GET_CODE (ret) != REG || REGNO (ret) < FIRST_PSEUDO_REGISTER)
    ret = copy_to_reg (ret);
  emit_move_insn (valreg, save);

  return ret;
}

/* Create the va_list data type.
   The SVR4 ABI requires the following structure:
        typedef struct {
            unsigned long  ireg_used;
            unsigned long  freg_used;
            long          *reg_base;
            long          *mem_ptr;
        } va_list;

   Otherwise, this structure is used:
        typedef struct {
            long          *reg_base;
            long          *mem_ptr;
            unsigned long  ireg_used;
            unsigned long  freg_used;
        } va_list;

   The tree representing the va_list declaration is returned.  */

static tree
i860_build_builtin_va_list (void)
{
  tree f_gpr, f_fpr, f_mem, f_sav, record, type_decl;

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);
  type_decl = build_decl (TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_gpr = build_decl (FIELD_DECL, get_identifier ("__ireg_used"),
		      unsigned_type_node);
  f_fpr = build_decl (FIELD_DECL, get_identifier ("__freg_used"),
		      unsigned_type_node);
  f_sav = build_decl (FIELD_DECL, get_identifier ("__reg_base"),
		      ptr_type_node);
  f_mem = build_decl (FIELD_DECL, get_identifier ("__mem_ptr"),
		      ptr_type_node);

  DECL_FIELD_CONTEXT (f_gpr) = record;
  DECL_FIELD_CONTEXT (f_fpr) = record;
  DECL_FIELD_CONTEXT (f_sav) = record;
  DECL_FIELD_CONTEXT (f_mem) = record;

  TREE_CHAIN (record) = type_decl;
  TYPE_NAME (record) = type_decl;

#ifdef I860_SVR4_VA_LIST
  TYPE_FIELDS (record) = f_gpr;
  TREE_CHAIN (f_gpr) = f_fpr;
  TREE_CHAIN (f_fpr) = f_sav;
  TREE_CHAIN (f_sav) = f_mem;
#else
  TYPE_FIELDS (record) = f_sav;
  TREE_CHAIN (f_sav) = f_mem;
  TREE_CHAIN (f_mem) = f_gpr;
  TREE_CHAIN (f_gpr) = f_fpr;
#endif

  layout_type (record);
  return record;
}

/* Initialize the va_list structure.  */

void
i860_va_start (tree valist, rtx nextarg ATTRIBUTE_UNUSED)
{
  tree saveregs, t;
  tree f_gpr, f_fpr, f_mem, f_sav;
  tree gpr, fpr, mem, sav;
  int off = 0;
  saveregs = make_tree (ptr_type_node, expand_builtin_saveregs ());

#ifdef I860_SVR4_VA_LIST
  f_gpr = TYPE_FIELDS (va_list_type_node);
  f_fpr = TREE_CHAIN (f_gpr);
  f_sav = TREE_CHAIN (f_fpr);
  f_mem = TREE_CHAIN (f_sav);
#else
  f_sav = TYPE_FIELDS (va_list_type_node);
  f_mem = TREE_CHAIN (f_sav);
  f_gpr = TREE_CHAIN (f_mem);
  f_fpr = TREE_CHAIN (f_gpr);
#endif

  gpr = build (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr, NULL_TREE);
  fpr = build (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr, NULL_TREE);
  sav = build (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav, NULL_TREE);
  mem = build (COMPONENT_REF, TREE_TYPE (f_mem), valist, f_mem, NULL_TREE);

  /* Initialize the `mem_ptr' field to the address of the first anonymous
     stack argument.  */
  t = make_tree (TREE_TYPE (mem), virtual_incoming_args_rtx);
  off = INTVAL (current_function_arg_offset_rtx);
  off = off < 0 ? 0 : off;
  t = build (PLUS_EXPR, TREE_TYPE (mem), t, build_int_cst (NULL_TREE, off, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (mem), mem, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Initialize the `ireg_used' field.  */
  t = build_int_cst (NULL_TREE,
		     current_function_args_info.ints / UNITS_PER_WORD, 0);
  t = build (MODIFY_EXPR, TREE_TYPE (gpr), gpr, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
     
  /* Initialize the `freg_used' field.  */
  t = build_int_cst (NULL_TREE,
		     current_function_args_info.floats / UNITS_PER_WORD, 0);
  t = build (MODIFY_EXPR, TREE_TYPE (fpr), fpr, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
      
  /* Initialize the `reg_base' field.  */
  t = build (MODIFY_EXPR, TREE_TYPE (sav), sav, saveregs);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

#define NUM_PARM_FREGS	8
#define NUM_PARM_IREGS	12
#ifdef I860_SVR4_VA_LIST
#define FREG_OFFSET 0
#define IREG_OFFSET (NUM_PARM_FREGS * UNITS_PER_WORD)
#else
#define FREG_OFFSET (NUM_PARM_IREGS * UNITS_PER_WORD)
#define IREG_OFFSET 0
#endif

/* Update the VALIST structure as necessary for an
   argument of the given TYPE, and return the argument.  */

static tree
i860_gimplify_va_arg_expr (tree valist, tree type, tree *pre_p, tree *post_p)
{
  tree f_gpr, f_fpr, f_mem, f_sav;
  tree gpr, fpr, mem, sav;
  tree size, t, u, addr, type_ptr;
  tree reg, n_reg, sav_ofs, lim_reg;
  HOST_WIDE_INT isize;
  bool indirect;

#ifdef I860_SVR4_VA_LIST
  f_gpr = TYPE_FIELDS (va_list_type_node);
  f_fpr = TREE_CHAIN (f_gpr);
  f_sav = TREE_CHAIN (f_fpr);
  f_mem = TREE_CHAIN (f_sav);
#else
  f_sav = TYPE_FIELDS (va_list_type_node);
  f_mem = TREE_CHAIN (f_sav);
  f_gpr = TREE_CHAIN (f_mem);
  f_fpr = TREE_CHAIN (f_gpr);
#endif

  gpr = build (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr, NULL_TREE);
  fpr = build (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr, NULL_TREE);
  mem = build (COMPONENT_REF, TREE_TYPE (f_mem), valist, f_mem, NULL_TREE);
  sav = build (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav, NULL_TREE);

  indirect = pass_by_reference (NULL, TYPE_MODE (type), type, false);
  if (indirect)
    type = build_pointer_type (type);
  size = size_in_bytes (type);
  type_ptr = build_pointer_type (type);

  if (AGGREGATE_TYPE_P (type))
    {
      /* Aggregates are passed on the stack.  */
      HOST_WIDE_INT align;

      align = TYPE_ALIGN (type);
      if (align < BITS_PER_WORD)
        align = BITS_PER_WORD;
      align /= BITS_PER_UNIT;

      u = fold_convert (ptr_type_node, size_int (align - 1));
      t = build (PLUS_EXPR, ptr_type_node, mem, u);
      u = fold (build (BIT_NOT_EXPR, ptr_type_node, u));
      t = build (BIT_AND_EXPR, ptr_type_node, t, u);
      addr = get_initialized_tmp_var (t, pre_p, post_p);

      u = fold_convert (ptr_type_node, size);
      t = build (PLUS_EXPR, ptr_type_node, addr, size);
      t = build (MODIFY_EXPR, ptr_type_node, mem, t);
      gimplify_and_add (t, pre_p);
    }
  else
    {
      isize = tree_low_cst (size, 0);

      if (FLOAT_TYPE_P (type) || (INTEGRAL_TYPE_P (type) && isize == 8))
	{
	  /* Floats and long longs are passed in the fp registers.  */
	  reg = fpr;
	  n_reg = size_int (isize / UNITS_PER_WORD);
	  n_reg = fold_convert (unsigned_type_node, n_reg);
	  lim_reg = size_int (NUM_PARM_FREGS - (isize / UNITS_PER_WORD));
	  lim_reg = fold_convert (unsigned_type_node, lim_reg);
	  sav_ofs = size_int (FREG_OFFSET);
	}
      else
	{
	  /* Everything else is passed in general registers.  */
	  reg = gpr;
	  if ((isize + UNITS_PER_WORD - 1) / UNITS_PER_WORD > 1)
	    abort ();
	  n_reg = fold_convert (unsigned_type_node, integer_one_node);
	  lim_reg = size_int (NUM_PARM_IREGS - 1);
	  lim_reg = fold_convert (unsigned_type_node, lim_reg);
	  sav_ofs = size_int (IREG_OFFSET);
	}

      u = build (LE_EXPR, boolean_type_node, reg, lim_reg);
      addr = build (COND_EXPR, ptr_type_node, u, NULL, NULL);

      /* The value was passed in a register, so read it from the register
	 save area initialized by __builtin_saveregs.  */

      sav_ofs = fold_convert (ptr_type_node, sav_ofs);
      sav_ofs = fold (build (PLUS_EXPR, ptr_type_node, sav, sav_ofs));

      u = fold_convert (unsigned_type_node, size_int (UNITS_PER_WORD));
      u = build (MULT_EXPR, unsigned_type_node, reg, u);
      u = fold_convert (ptr_type_node, u);
      u = build (PLUS_EXPR, ptr_type_node, sav_ofs, u);
      COND_EXPR_THEN (addr) = u;

      /* The value was passed in memory, so read it from the overflow area.  */

      t = fold_convert (ptr_type_node, size);
      u = build (POSTINCREMENT_EXPR, ptr_type_node, mem, t);
      COND_EXPR_ELSE (addr) = u;

      /* Increment either the ireg_used or freg_used field.  */

      t = build (PLUS_EXPR, unsigned_type_node, reg, n_reg);
      t = build (MODIFY_EXPR, unsigned_type_node, reg, t);
      gimplify_and_add (t, post_p);
    }

  addr = fold_convert (type_ptr, addr);
  if (indirect)
    addr = build_fold_indirect_ref (addr);
  return build_fold_indirect_ref (addr);
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
i860_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED, int *total)
{
  switch (code)
    {
    case CONST_INT:
      if (INTVAL (x) == 0)
        *total = 0;
      else if (INTVAL (x) < 0x2000 && INTVAL (x) >= -0x2000)
        *total = 1;
      return true;
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = 4;
      return true;
    case CONST_DOUBLE:
      *total = 6;
      return true;
    default:
      return false;
    }
}

static void
i860_internal_label (FILE *stream, const char *prefix, unsigned long labelno)
{
  fprintf (stream, ".%s%ld:\n", prefix, labelno);
}

static void
i860_file_start (void)
{
  output_file_directive (asm_out_file, main_input_filename);
  fprintf (asm_out_file, "\t.version\t\"01.01\"\n");
}

static void
i860_init_libfuncs (void)
{
  set_optab_libfunc (sdiv_optab, SImode, "*.div");
  set_optab_libfunc (udiv_optab, SImode, "*.udiv");
  set_optab_libfunc (smod_optab, SImode, "*.rem");
  set_optab_libfunc (umod_optab, SImode, "*.urem");
}

static rtx
i860_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
		       int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, I860_STRUCT_VALUE_REGNUM);
}

/* Initialize the GCC target structure.  */
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS i860_rtx_costs

#undef  TARGET_ASM_INTERNAL_LABEL
#define TARGET_ASM_INTERNAL_LABEL i860_internal_label

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE i860_output_function_prologue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE i860_output_function_epilogue

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS i860_init_libfuncs

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST i860_build_builtin_va_list
#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR i860_gimplify_va_arg_expr

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX i860_struct_value_rtx

#undef TARGET_EXPAND_BUILTIN_SAVEREGS
#define TARGET_EXPAND_BUILTIN_SAVEREGS i860_saveregs

struct gcc_target targetm = TARGET_INITIALIZER;
