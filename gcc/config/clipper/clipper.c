/* Subroutines for insn-output.c for Clipper
   Copyright (C) 1987, 1988, 1991, 1997 Free Software Foundation, Inc.
   Contributed by Holger Teutsch (holger@hotbso.rhein-main.de)

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
#include "c-tree.h"
#include "expr.h"
#include "flags.h"
#include "machmode.h"

extern char regs_ever_live[];

extern int frame_pointer_needed;

static int frame_size;

/*
 * compute size of a clipper stack frame where 'lsize' is the required
 * space for local variables.
 */

int
clipper_frame_size (lsize)
     int lsize;
{
  int i,size;				/* total size of frame */
  int save_size;
  save_size = 0;			/* compute size for reg saves */

  for (i = 16; i < 32; i++)
    if (regs_ever_live[i] && !call_used_regs[i])
      save_size += 8;

  for (i = 0; i < 16; i++)
    if (regs_ever_live[i] && !call_used_regs[i])
      save_size += 4;

  size = lsize + save_size;

  size = (size + 7) & ~7;		/* align to 64 Bit */
  return size;
}

/*
 * prologue and epilogue output
 * function is entered with pc pushed, i.e. stack is 32 bit aligned
 *
 * current_function_args_size == 0 means that the current function's args
 * are passed totally in registers i.e fp is not used as ap.
 * If frame_size is also 0 the current function does not push anything and
 * can run with misaligned stack -> subq $4,sp / add $4,sp on entry and exit
 * can be omitted.
 *
 */
void
output_function_prologue (file, lsize)
     FILE *file;
     int lsize;				/* size for locals */
{
  int i, offset;
  int size;

  frame_size = size = clipper_frame_size (lsize);

  if (frame_pointer_needed)
    {
      fputs ("\tpushw  fp,sp\n", file);
      fputs ("\tmovw   sp,fp\n", file);
    }
  else if (size != 0 || current_function_args_size != 0)
    {
      size += 4;			/* keep stack aligned */
      frame_size = size;		/* must push data or access args */
    }

  if (size)
    {
      if (size < 16)
	fprintf (file, "\tsubq   $%d,sp\n", size);
      else
	fprintf (file, "\tsubi   $%d,sp\n", size);

      /* register save slots are relative to sp, because we have small positive
	 displacements and this works whether we have a frame pointer or not */

      offset = 0;
      for (i = 16; i < 32; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    if (offset == 0)
	      fprintf (file, "\tstord  f%d,(sp)\n", i-16);
	    else
	      fprintf (file, "\tstord  f%d,%d(sp)\n", i-16, offset);
	    offset += 8;
	  }

      for (i = 0; i < 16; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    if (offset == 0)
	      fprintf (file, "\tstorw  r%d,(sp)\n", i);
	    else
	      fprintf (file, "\tstorw  r%d,%d(sp)\n", i, offset);
	    offset += 4;
	  }
    }
}

void
output_function_epilogue (file, size)
     FILE *file;
     int size;				/* ignored */
{
  int i, offset;

  if (frame_pointer_needed)
    {
      offset = -frame_size;

      for (i = 16; i < 32; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    fprintf (file, "\tloadd  %d(fp),f%d\n", offset, i-16);
	    offset += 8;
	  }

      for (i = 0; i < 16; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    fprintf (file, "\tloadw  %d(fp),r%d\n", offset, i);
	    offset += 4;
	  }

      fputs ("\tmovw   fp,sp\n\tpopw   sp,fp\n\tret    sp\n",
	     file);
    }

  else					/* no frame pointer */
    {
      offset = 0;

      for (i = 16; i < 32; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    if (offset == 0)
	      fprintf (file, "\tloadd  (sp),f%d\n", i-16);
	    else
	      fprintf (file, "\tloadd  %d(sp),f%d\n", offset, i-16);
	    offset += 8;
	  }

      for (i = 0; i < 16; i++)
	if (regs_ever_live[i] && !call_used_regs[i])
	  {
	    if (offset == 0)
	      fprintf (file, "\tloadw  (sp),r%d\n", i);
	    else
	      fprintf (file, "\tloadw  %d(sp),r%d\n", offset, i);
	    offset += 4;
	  }

      if (frame_size > 0)
	{
	  if (frame_size < 16)
	    fprintf (file, "\taddq   $%d,sp\n", frame_size);
	  else
	    fprintf (file, "\taddi   $%d,sp\n", frame_size);
	}

      fputs ("\tret    sp\n", file);
    }
}

/*
 * blockmove
 *
 * clipper_movstr ()
 */
void
clipper_movstr (operands)
     rtx *operands;
{
  rtx dst,src,cnt,tmp,top,bottom,xops[3];
  int align;
  int fixed;

  extern FILE *asm_out_file;

  dst = operands[0];
  src = operands[1];
  /* don't change this operands[2]; gcc 2.3.3 doesn't honor clobber note */
  align = INTVAL (operands[3]);
  tmp = operands[4];
  cnt = operands[5];

  if (GET_CODE (operands[2]) == CONST_INT) /* fixed size move */
    {
      if ((fixed = INTVAL (operands[2])) <= 0)
	abort ();

      if (fixed <16)
	output_asm_insn ("loadq  %2,%5", operands);
      else
	output_asm_insn ("loadi  %2,%5", operands);
    }
  else
    {
      fixed = 0;
      bottom = (rtx)gen_label_rtx ();	/* need a bottom label */
      xops[0] = cnt; xops[1] = bottom;
      output_asm_insn ("movw   %2,%5", operands); /* count is scratch reg 5 */
      output_asm_insn ("brle   %l1", xops);
    }


  top = (rtx)gen_label_rtx ();		/* top of loop label */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (top));


  xops[0] = src; xops[1] = tmp; xops[2] = dst;

  if (fixed && (align & 0x3) == 0)	/* word aligned move with known size */
    {
      if (fixed >= 4)
	{
	  rtx xops1[2];
	  output_asm_insn(
	    "loadw  %a0,%1\n\taddq   $4,%0\n\tstorw  %1,%a2\n\taddq   $4,%2",
			  xops);

	  xops1[0] = cnt; xops1[1] = top;
	  output_asm_insn ("subq   $4,%0\n\tbrgt   %l1", xops1);
	}

      if (fixed & 0x2)
	{
	  output_asm_insn ("loadh  %a0,%1\n\tstorh  %1,%a2", xops);
	  if (fixed & 0x1)
	    output_asm_insn ("loadb  2%a0,%1\n\tstorb  %1,2%a2", xops);
	}
      else
	if (fixed & 0x1)
	  output_asm_insn ("loadb  %a0,%1\n\tstorb  %1,%a2", xops);
    }
  else
    {
      output_asm_insn(
	  "loadb  %a0,%1\n\taddq   $1,%0\n\tstorb  %1,%a2\n\taddq   $1,%2",
		      xops);

      xops[0] = cnt; xops[1] = top;
      output_asm_insn ("subq   $1,%0\n\tbrgt   %l1", xops);
    }

  if (fixed == 0)
    ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (bottom));
}


print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  rtx op0,op1;

 retry:
  switch (GET_CODE (addr))
    {
    case REG:
      fprintf (file, "(%s)", reg_names[REGNO (addr)]);
      break;

    case PLUS:
      /* can be 'symbol + reg' or 'reg + reg' */

      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);

      if (GET_CODE (op0) == REG && GET_CODE (op1) == REG)
	{
	  fprintf (file, "[%s](%s)",
		   reg_names[REGNO (op0)], reg_names[REGNO (op1)]);
	  break;
	}

      if (GET_CODE (op0) == REG && CONSTANT_ADDRESS_P (op1))
	{
	  output_addr_const (file, op1);
	  fprintf (file, "(%s)", reg_names[REGNO (op0)]);
	  break;
	}

      if (GET_CODE (op1) == REG && CONSTANT_ADDRESS_P (op0))
	{
	  output_addr_const (file, op0);
	  fprintf (file, "(%s)", reg_names[REGNO (op1)]);
	  break;
	}
      abort ();				/* Oh no */

    default:
      output_addr_const (file, addr);
    }
}


char *
rev_cond_name (op)
     rtx op;
{
  switch (GET_CODE (op))
    {
    case EQ:
      return "ne";
    case NE:
      return "eq";
    case LT:
      return "ge";
    case LE:
      return "gt";
    case GT:
      return "le";
    case GE:
      return "lt";
    case LTU:
      return "geu";
    case LEU:
      return "gtu";
    case GTU:
      return "leu";
    case GEU:
      return "ltu";

    default:
      abort ();
    }
}


/* Do what is necessary for `va_start'.  The argument is ignored;
   We fill in an initial va_list.  A pointer to this constructor
   is returned. */


struct rtx_def *
clipper_builtin_saveregs (arglist)
     tree arglist;
{
  extern int current_function_varargs;
  rtx block, addr, argsize, scratch, r0_addr,r1_addr,f0_addr,f1_addr;

  /* Allocate the va_list constructor + save area for r0,r1,f0,f1 */

  block = assign_stack_local (BLKmode,
			      (6 + 6) * UNITS_PER_WORD, 2 * BITS_PER_WORD);

  RTX_UNCHANGING_P (block) = 1;
  RTX_UNCHANGING_P (XEXP (block, 0)) = 1;

  addr = copy_to_reg (XEXP (block, 0));

  f0_addr =  gen_rtx (PLUS, Pmode, addr, GEN_INT (24));
  f1_addr =  gen_rtx (PLUS, Pmode, addr, GEN_INT (32));
  r0_addr =  gen_rtx (PLUS, Pmode, addr, GEN_INT (40));
  r1_addr =  gen_rtx (PLUS, Pmode, addr, GEN_INT (44));


  /* Store float regs  */

  emit_move_insn (gen_rtx (MEM, DFmode, f0_addr), gen_rtx (REG, DFmode, 16));
  emit_move_insn (gen_rtx (MEM, DFmode, f1_addr), gen_rtx (REG, DFmode, 17));

  /* Store int regs  */

  emit_move_insn (gen_rtx (MEM, SImode, r0_addr), gen_rtx (REG, SImode, 0));
  emit_move_insn (gen_rtx (MEM, SImode, r1_addr), gen_rtx (REG, SImode, 1));

  /* Store the arg pointer in the __va_stk member.  */

  emit_move_insn (gen_rtx (MEM, SImode, addr),
		  copy_to_reg (virtual_incoming_args_rtx));
		  

  /* now move addresses of the saved regs into the pointer array */

  scratch = gen_reg_rtx (Pmode);

  emit_move_insn (scratch, r0_addr);
  emit_move_insn (gen_rtx (MEM, SImode,
			   gen_rtx (PLUS, Pmode, addr,
				    GEN_INT (4))),
		  scratch);
		  
  emit_move_insn (scratch, f0_addr);
  emit_move_insn (gen_rtx (MEM, SImode,
			   gen_rtx (PLUS, Pmode, addr,
				    GEN_INT (8))),
		  scratch);
		  
  emit_move_insn (scratch, r1_addr);
  emit_move_insn (gen_rtx (MEM, SImode,
			   gen_rtx (PLUS, Pmode, addr,
				    GEN_INT (12))),
		  scratch);
		  
  emit_move_insn (scratch, f1_addr);
  emit_move_insn (gen_rtx (MEM, SImode,
			   gen_rtx (PLUS, Pmode, addr,
				    GEN_INT (16))),
		  scratch);


  if (current_function_check_memory_usage)
    {
      emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
			 addr, ptr_mode,
			 GEN_INT (5 * GET_MODE_SIZE (SImode)),
			 TYPE_MODE (sizetype),
			 GEN_INT (MEMORY_USE_RW),
			 TYPE_MODE (integer_type_node));

      emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
			 f0_addr, ptr_mode,
			 GEN_INT (GET_MODE_SIZE (DFmode)),
			 TYPE_MODE (sizetype),
			 GEN_INT (MEMORY_USE_RW), 
			 TYPE_MODE (integer_type_node));
      emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
			 f1_addr, ptr_mode,
			 GEN_INT (GET_MODE_SIZE (DFmode)),
			 TYPE_MODE (sizetype),
			 GEN_INT (MEMORY_USE_RW), 
			 TYPE_MODE (integer_type_node));
      emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
			 r0_addr, ptr_mode,
			 GEN_INT (GET_MODE_SIZE (SImode)),
			 TYPE_MODE (sizetype),
			 GEN_INT (MEMORY_USE_RW),
			 TYPE_MODE (integer_type_node));
      emit_library_call (chkr_set_right_libfunc, 1, VOIDmode, 3,
			 r1_addr, ptr_mode,
			 GEN_INT (GET_MODE_SIZE (SImode)),
			 TYPE_MODE (sizetype),
			 GEN_INT (MEMORY_USE_RW),
			 TYPE_MODE (integer_type_node));
    }

  /* Return the address of the va_list constructor, but don't put it in a
     register.  This fails when not optimizing and produces worse code when
     optimizing.  */
  return XEXP (block, 0);
}


/* Return truth value of whether OP can be used as an word register
   operand. Reject (SUBREG:SI (REG:SF )) */

int
int_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) &&
	  (GET_CODE (op) != SUBREG ||
	   GET_MODE_CLASS (GET_MODE (SUBREG_REG (op))) == MODE_INT));
}

/* Return truth value of whether OP can be used as a float register
   operand. Reject (SUBREG:SF (REG:SI )) )) */

int
fp_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) &&
	  (GET_CODE (op) != SUBREG ||
	   GET_MODE_CLASS (GET_MODE (SUBREG_REG (op))) == MODE_FLOAT));
}

