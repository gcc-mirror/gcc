/* Subroutines for insn-output.c for Matsushita MN10300 series
   Copyright (C) 1996 Free Software Foundation, Inc.
   Contributed by Jeff Law (law@cygnus.com).

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
#include "tree.h"
#include "obstack.h"

void
asm_file_start (file)
     FILE *file;
{
  fprintf (file, "#\tGCC For the Matsushita MN10300\n");
  if (optimize)
    fprintf (file, "# -O%d\n", optimize);
  else
    fprintf (file, "\n\n");
  output_file_directive (file, main_input_filename);
}


int
const_costs (r, c)
     rtx r;
     enum rtx_code c;
{
  switch (c)
    {
    case CONST_INT:
      if (INT_8_BITS (INTVAL (r)))
	return 0;
      else if (INT_16_BITS (INTVAL (r)))
	return 1;
      else
	return 2;
    case CONST_DOUBLE:
      return 8;
    default:
      return 4;
    }
}

/* Print operand X using operand code CODE to assembly language output file
   FILE.  */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  switch (code)
    {
      case 'b':
      case 'B':
	/* These are normal and reversed branches.  */
	switch (code == 'b' ? GET_CODE (x) : reverse_condition (GET_CODE (x)))
	  {
	  case NE:
	    fprintf (file, "ne");
	    break;
	  case EQ:
	    fprintf (file, "eq");
	    break;
	  case GE:
	    fprintf (file, "ge");
	    break;
	  case GT:
	    fprintf (file, "gt");
	    break;
	  case LE:
	    fprintf (file, "le");
	    break;
	  case LT:
	    fprintf (file, "lt");
	    break;
	  case GEU:
	    fprintf (file, "cc");
	    break;
	  case GTU:
	    fprintf (file, "hi");
	    break;
	  case LEU:
	    fprintf (file, "ls");
	    break;
	  case LTU:
	    fprintf (file, "cs");
	    break;
	  default:
	    abort ();
	  }
	break;
      case 'C':
	/* This is used for the operand to a call instruction;
	   if it's a REG, enclose it in parens, else output
	   the operand normally.  */
	if (GET_CODE (x) == REG)
	  {
	    fputc ('(', file);
	    print_operand (file, x, 0);
	    fputc (')', file);
	  }
	else
	  print_operand (file, x, 0);
	break;
     
      default:
	switch (GET_CODE (x))
	  {
	  case MEM:
	    fputc ('(', file);
	    output_address (XEXP (x, 0));
	    fputc (')', file);
	    break;

	  case REG:
	    fprintf (file, "%s", reg_names[REGNO (x)]);
	    break;

	  case SUBREG:
	    fprintf (file, "%s",
		     reg_names[REGNO (SUBREG_REG (x)) + SUBREG_WORD (x)]);
	    break;

	  case CONST_INT:
	  case SYMBOL_REF:
	  case CONST:
	  case LABEL_REF:
	  case CODE_LABEL:
	    print_operand_address (file, x);
	    break;
	  default:
	    abort ();
	  }
	break;
   }
}

/* Output assembly language output for the address ADDR to FILE.  */

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    case REG:
      if (addr == stack_pointer_rtx)
	print_operand_address (file, gen_rtx (PLUS, SImode,
					      stack_pointer_rtx,
					      GEN_INT (0)));
      else
	print_operand (file, addr, 0);
      break;
    case PLUS:
      {
	rtx base, index;
	if (REG_P (XEXP (addr, 0))
	    && REG_OK_FOR_BASE_P (XEXP (addr, 0)))
	  base = XEXP (addr, 0), index = XEXP (addr, 1);
	else if (REG_P (XEXP (addr, 1))
	    && REG_OK_FOR_BASE_P (XEXP (addr, 1)))
	  base = XEXP (addr, 1), index = XEXP (addr, 0);
      	else
	  abort ();
	print_operand (file, index, 0);
	fputc (',', file);
	print_operand (file, base, 0);;
	break;
      }
    case SYMBOL_REF:
      output_addr_const (file, addr);
      break;
    default:
      output_addr_const (file, addr);
      break;
    }
}

void
expand_prologue ()
{
  unsigned int size = get_frame_size ();

  /* And now store all the registers onto the stack with a
     single two byte instruction.  */
  if (regs_ever_live[2] || regs_ever_live[3]
      || regs_ever_live[6] || regs_ever_live[7]
      || frame_pointer_needed)
    emit_insn (gen_store_movm ());

  /* Now put the frame pointer into the frame pointer register.  */
  if (frame_pointer_needed)
    emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);

  /* Allocate stack for this frame.  */
  if (size)
    emit_insn (gen_addsi3 (stack_pointer_rtx,
			   stack_pointer_rtx,
			   GEN_INT (-size)));
}

void
expand_epilogue ()
{
  unsigned int size = get_frame_size ();

  /* Cut back the stack.  */
  if (frame_pointer_needed)
    {
      emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);
      size = 0;
    }
  else if (size > 255)
    {
      emit_insn (gen_addsi3 (stack_pointer_rtx,
			     stack_pointer_rtx,
			     GEN_INT (size)));
      size = 0;
    }

  /* For simplicity, we just movm all the callee saved registers to
     the stack with one instruction.

     ?!? Only save registers which are actually used.  Reduces
     stack requireents and is faster.  */
  if (regs_ever_live[2] || regs_ever_live[3]
      || regs_ever_live[6] || regs_ever_live[7]
      || frame_pointer_needed)
    emit_jump_insn (gen_return_internal_regs (GEN_INT (size)));
  else
    {
      if (size)
	emit_insn (gen_addsi3 (stack_pointer_rtx,
			       stack_pointer_rtx,
			       GEN_INT (size)));
      emit_jump_insn (gen_return_internal ());
    }
}

/* Update the condition code from the insn.  */

void
notice_update_cc (body, insn)
     rtx body;
     rtx insn;
{
  switch (get_attr_cc (insn))
    {
    case CC_NONE:
      /* Insn does not affect CC at all.  */
      break;

    case CC_NONE_0HIT:
      /* Insn does not change CC, but the 0'th operand has been changed.  */
      if (cc_status.value1 != 0
	  && reg_overlap_mentioned_p (recog_operand[0], cc_status.value1))
	cc_status.value1 = 0;
      break;

    case CC_SET_ZN_C0:
      /* Insn sets the Z,N flags of CC to recog_operand[0].
	 V is always set to 0.  C may or may not be set to 0 but that's ok
	 because alter_cond will change tests to use EQ/NE.  */
      CC_STATUS_INIT;
      cc_status.flags |= CC_NO_OVERFLOW | CC_OVERFLOW_UNUSABLE;
      cc_status.value1 = recog_operand[0];
      break;

    case CC_TST:
      /* The insn sets all the condition codes, except v is bogus.  */
      CC_STATUS_INIT;
      cc_status.value1 = recog_operand[0];
      break;

    case CC_COMPARE:
      /* The insn is a compare instruction.  */
      CC_STATUS_INIT;
      cc_status.value1 = SET_SRC (body);
      break;

    case CC_CLOBBER:
      /* Insn doesn't leave CC in a usable state.  */
      CC_STATUS_INIT;
      break;

    default:
      abort ();
    }
}

/* Return true if OP is a valid call operand.  */

int
call_address_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == REG);
}

/* What (if any) secondary registers are needed to move IN with mode
   MODE into a register from in register class CLASS. 

   We might be able to simplify this.  */
enum reg_class
secondary_reload_class (class, mode, in)
     enum reg_class class;
     enum machine_mode mode;
     rtx in;
{
  int regno;

  /* Memory loads less than a full word wide can't have an
     address or stack pointer destination.  They must use
     a data register as an intermediate register.  */
  if (GET_CODE (in) == MEM
      && (mode == QImode || mode == HImode)
      && (class == ADDRESS_REGS || class == SP_REGS))
    return DATA_REGS;

  /* We can't directly load sp + const_int into a data register;
     we must use an address register as an intermediate.  */
  if (class != SP_REGS
      && class != ADDRESS_REGS
      && class != SP_OR_ADDRESS_REGS
      && (in == stack_pointer_rtx
	  || (GET_CODE (in) == PLUS
	      && (XEXP (in, 0) == stack_pointer_rtx
		  || XEXP (in, 1) == stack_pointer_rtx))))
    return ADDRESS_REGS;

  /* Otherwise assume no secondary reloads are needed.  */
  return NO_REGS;
}

int
initial_offset (from, to)
     int from, to;
{
  if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    {
      if (regs_ever_live[2] || regs_ever_live[3]
	  || regs_ever_live[6] || regs_ever_live[7]
	  || frame_pointer_needed)
	return 20;
      else
	return 4;
    }

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      if (regs_ever_live[2] || regs_ever_live[3]
	  || regs_ever_live[6] || regs_ever_live[7]
	  || frame_pointer_needed)
	return get_frame_size () + 20;
      else
	return get_frame_size () + 4;
    }

  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return get_frame_size ();

  abort ();
}
