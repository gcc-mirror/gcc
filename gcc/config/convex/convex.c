/* Subroutines for insn-output.c for Convex.
   Copyright (C) 1989,1991 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

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

/* Boolean to keep track of whether the current section is .text or not.
   Used by .align handler in convex.h. */

int current_section_is_text;

/* set_cmp saves the operands of a "cmp" insn, along with the type character
 * to be used in the compare instruction.
 *
 * gen_cmp finds out what comparison is to be performed and outputs the
 * necessary instructions, e.g.
 *    "eq.w a1,a2\;jbra.t L5"
 * for (cmpsi a1 a2) (beq L5)  */
 
static rtx xop0, xop1;
static char typech, regch;

char *
set_cmp (op0, op1, typechr)
     rtx op0, op1;
     char typechr;
{
  xop0 = op0;
  xop1 = op1;
  typech = typechr;
  if (GET_CODE (op0) == REG)
    regch = A_REGNO_P (REGNO (op0)) ? 'a' : 's';
  else if (GET_CODE (op1) == REG)
    regch = A_REGNO_P (REGNO (op1)) ? 'a' : 's';
  else abort ();
  return "";
}

char *
gen_cmp (label, cmpop, tf)
     rtx label;
     char *cmpop;
     char tf;
{
  char buf[80];
  char revop[4];
  rtx ops[3];

  ops[2] = label;

  /* Constant must be first; swap operands if necessary.
     If lt, le, ltu, leu are swapped, change to le, lt, leu, ltu
     and reverse the sense of the jump. */

  if (CONSTANT_P (xop1))
    {
      ops[0] = xop1;
      ops[1] = xop0;
      if (cmpop[0] == 'l')
	{
	  bcopy (cmpop, revop, sizeof revop);
	  revop[1] ^= 'e' ^ 't';
	  tf ^= 't' ^ 'f';
	  cmpop = revop;
	}
    }
  else
    {
      ops[0] = xop0;
      ops[1] = xop1;
    }

  sprintf (buf, "%s.%c %%0,%%1\n\tjbr%c.%c %%l2", cmpop, typech, regch, tf);
  output_asm_insn (buf, ops);
  return "";
}

/* Routines to separate CONST_DOUBLEs into component parts. */

int
const_double_high_int (x)
     rtx x;
{
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    return CONST_DOUBLE_LOW (x);
  else
    return CONST_DOUBLE_HIGH (x);
}

int
const_double_low_int (x)
     rtx x;
{
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    return CONST_DOUBLE_HIGH (x);
  else
    return CONST_DOUBLE_LOW (x);
}

/* Return the number of args in the call insn X. */

static int
call_num_args (x)
     rtx x;
{
  if (GET_CODE (x) == CALL)
    return INTVAL (x->fld[1].rtx);
  if (GET_CODE (x) == SET)
    return call_num_args (SET_SRC (x));
  abort ();
}

/* Scan forward from a call to decide whether we need to reload AP
   from 12(FP) after it.  We need to if there can be a reference to
   arg_pointer_rtx before the next call, which will clobber AP.
   Look forward in the instruction list until encountering a call
   (don't need the load), or a reference to AP (do need it), or
   a jump (don't know, do the load).  */

static int
ap_reload_needed (insn)
     rtx insn;
{
  for (;;)
    {
      insn = NEXT_INSN (insn);
      switch (GET_CODE (insn))
	{
	case JUMP_INSN:
	  /* Basic block ends.  If return, no AP needed, else assume it is. */
	  return GET_CODE (PATTERN (insn)) != RETURN;
	case CALL_INSN:
	  /* A subsequent call.  AP isn't needed unless the call itself
	     requires it.  But zero-arg calls don't clobber AP, so
	     don't terminate the search in that case. */
	  if (reg_mentioned_p (arg_pointer_rtx, PATTERN (insn)))
	    return 1;
	  if (! TARGET_ARGCOUNT && call_num_args (PATTERN (insn)) == 0)
	    break;
	  return 0;
	case BARRIER:
	  /* Barrier, don't need AP. */
	  return 0;
	case INSN:
	  /* Other insn may need AP; if not, keep looking. */
	  if (reg_mentioned_p (arg_pointer_rtx, PATTERN (insn)))
	    return 1;
	}
    }
}

/* Output the insns needed to do a call. */

char *
output_call (insn, address, argcount)
    rtx insn, address, argcount;
{
  int set_ap = TARGET_ARGCOUNT || argcount != const0_rtx;

  /* If AP is used by the call address, evaluate the address into a temp. */
  if (reg_mentioned_p (arg_pointer_rtx, address))
    if (set_ap)
      {
	address = XEXP (address, 0);
	output_asm_insn ("ld.w %0,a1", &address);
	address = gen_rtx (MEM, QImode, gen_rtx (REG, Pmode, 9));
      }

  /* If there are args, point AP to them. */
  if (set_ap)
    output_asm_insn ("mov sp,ap");

  /* If we are passing an arg count, convert it to words and push it. */
  if (TARGET_ARGCOUNT)
    {
      argcount = gen_rtx (CONST_INT, VOIDmode, (INTVAL (argcount) + 3) / 4);
      output_asm_insn ("pshea %a0", &argcount);
    }

  /* The call. */
  output_asm_insn ("calls %0", &address);

  /* If we clobbered AP, reload it if it is live. */
  if (set_ap)
    if (ap_reload_needed (insn))
      output_asm_insn ("ld.w 12(fp),ap");

  /* If we pushed an arg count, pop it and the args. */
  if (TARGET_ARGCOUNT)
    {
      argcount = gen_rtx (CONST_INT, VOIDmode, INTVAL (argcount) * 4 + 4);
      output_asm_insn ("add.w %0,sp", &argcount);
    }
  
  return "";
}
