/* Definitions of target machine for GNU compiler.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by James E. Wilson <wilson@cygnus.com> and
   		  David Mosberger <davidm@hpl.hp.com>.

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
#include <ctype.h>
#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
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
#include "except.h"
#include "function.h"
#include "ggc.h"
#include "basic-block.h"

/* This is used for communication between ASM_OUTPUT_LABEL and
   ASM_OUTPUT_LABELREF.  */
int ia64_asm_output_label = 0;

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  */
struct rtx_def * ia64_compare_op0;
struct rtx_def * ia64_compare_op1;

/* Register number where ar.pfs was saved in the prologue, or zero
   if it was not saved.  */

int ia64_arpfs_regno;

/* Register number where rp was saved in the prologue, or zero if it was
   not saved.  */

int ia64_rp_regno;

/* Register number where frame pointer was saved in the prologue, or zero
   if it was not saved.  */

int ia64_fp_regno;

/* Number of input and local registers used.  This is needed for the .regstk
   directive, and also for debugging info.  */

int ia64_input_regs;
int ia64_local_regs;

/* If true, then we must emit a .regstk directive.  */

int ia64_need_regstk;

/* Register names for ia64_expand_prologue.  */
char *ia64_reg_numbers[96] =
{ "r32", "r33", "r34", "r35", "r36", "r37", "r38", "r39",
  "r40", "r41", "r42", "r43", "r44", "r45", "r46", "r47",
  "r48", "r49", "r50", "r51", "r52", "r53", "r54", "r55",
  "r56", "r57", "r58", "r59", "r60", "r61", "r62", "r63",
  "r64", "r65", "r66", "r67", "r68", "r69", "r70", "r71",
  "r72", "r73", "r74", "r75", "r76", "r77", "r78", "r79",
  "r80", "r81", "r82", "r83", "r84", "r85", "r86", "r87",
  "r88", "r89", "r90", "r91", "r92", "r93", "r94", "r95",
  "r96", "r97", "r98", "r99", "r100","r101","r102","r103",
  "r104","r105","r106","r107","r108","r109","r110","r111",
  "r112","r113","r114","r115","r116","r117","r118","r119",
  "r120","r121","r122","r123","r124","r125","r126","r127"};

/* ??? These strings could be shared with REGISTER_NAMES.  */
char *ia64_input_reg_names[8] =
{ "in0",  "in1",  "in2",  "in3",  "in4",  "in5",  "in6",  "in7" };

/* ??? These strings could be shared with REGISTER_NAMES.  */
char *ia64_local_reg_names[80] =
{ "loc0", "loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7",
  "loc8", "loc9", "loc10","loc11","loc12","loc13","loc14","loc15",
  "loc16","loc17","loc18","loc19","loc20","loc21","loc22","loc23",
  "loc24","loc25","loc26","loc27","loc28","loc29","loc30","loc31",
  "loc32","loc33","loc34","loc35","loc36","loc37","loc38","loc39",
  "loc40","loc41","loc42","loc43","loc44","loc45","loc46","loc47",
  "loc48","loc49","loc50","loc51","loc52","loc53","loc54","loc55",
  "loc56","loc57","loc58","loc59","loc60","loc61","loc62","loc63",
  "loc64","loc65","loc66","loc67","loc68","loc69","loc70","loc71",
  "loc72","loc73","loc74","loc75","loc76","loc77","loc78","loc79" };

/* ??? These strings could be shared with REGISTER_NAMES.  */
char *ia64_output_reg_names[8] =
{ "out0", "out1", "out2", "out3", "out4", "out5", "out6", "out7" };

/* String used with the -mfixed-range= option.  */
const char *ia64_fixed_range_string;

/* Variables which are this size or smaller are put in the sdata/sbss
   sections.  */

int ia64_section_threshold;

/* Return 1 if OP is a valid operand for the MEM of a CALL insn.  */

int
call_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return 0;

  return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == REG
	  || (GET_CODE (op) == SUBREG && GET_CODE (XEXP (op, 0)) == REG));
}

/* Return 1 if OP refers to a symbol in the sdata section.  */

int
sdata_symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
      return XSTR (op, 0)[0] == SDATA_NAME_FLAG_CHAR;

    case CONST:
      return (GET_CODE (XEXP (op, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (op, 0), 0)) == SYMBOL_REF
	      && XSTR (XEXP (XEXP (op, 0), 0), 0)[0] == SDATA_NAME_FLAG_CHAR);
    default:
      break;
    }

  return 0;
}

/* Return 1 if OP refers to a symbol.  */

int
symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    default:
      break;
    }
  return 0;
}

/* Return 1 if OP refers to a function.  */

int
function_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == SYMBOL_REF && SYMBOL_REF_FLAG (op))
    return 1;
  else
    return 0;
}

/* Return 1 if OP is setjmp or a similar function.  */

/* ??? This is an unsatisfying solution.  Should rethink.  */

int
setjmp_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  char *name;
  int retval = 0;

  if (GET_CODE (op) != SYMBOL_REF)
    return 0;

  name = XSTR (op, 0);

  /* The following code is borrowed from special_function_p in calls.c.  */

  /* Disregard prefix _, __ or __x.  */
  if (name[0] == '_')
    {
      if (name[1] == '_' && name[2] == 'x')
	name += 3;
      else if (name[1] == '_')
	name += 2;
      else
	name += 1;
    }

  if (name[0] == 's')
    {
      retval
	= ((name[1] == 'e'
	    && (! strcmp (name, "setjmp")
		|| ! strcmp (name, "setjmp_syscall")))
	   || (name[1] == 'i'
	       && ! strcmp (name, "sigsetjmp"))
	   || (name[1] == 'a'
	       && ! strcmp (name, "savectx")));
    }
  else if ((name[0] == 'q' && name[1] == 's'
	    && ! strcmp (name, "qsetjmp"))
	   || (name[0] == 'v' && name[1] == 'f'
	       && ! strcmp (name, "vfork")))
    retval = 1;

  return retval;
}

/* Return 1 if OP is a general operand, but when pic exclude symbolic
   operands.  */

/* ??? If we drop no-pic support, can delete SYMBOL_REF, CONST, and LABEL_REF
   from PREDICATE_CODES.  */

int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (! TARGET_NO_PIC && symbolic_operand (op, mode))
    return 0;

  return general_operand (op, mode);
}

/* Return 1 if OP is a register operand, or zero.  */

int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (op == const0_rtx || register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or a 6 bit immediate operand.  */

int
reg_or_6bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_M (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or an 8 bit immediate operand.  */

int
reg_or_8bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_K (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or an 8 bit adjusted immediate
   operand.  */

int
reg_or_8bit_adjusted_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_L (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or is valid for both an 8 bit
   immediate and an 8 bit adjusted immediate operand.  This is necessary
   because when we emit a compare, we don't know what the condition will be,
   so we need the union of the immediates accepted by GT and LT.  */

int
reg_or_8bit_and_adjusted_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_K (INTVAL (op))
	   && CONST_OK_FOR_L (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or a 14 bit immediate operand.  */

int
reg_or_14bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_I (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || register_operand (op, mode));
}

/* Return 1 if OP is a register operand, or a 22 bit immediate operand.  */

int
reg_or_22bit_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_J (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || register_operand (op, mode));
}

/* Return 1 if OP is a 6 bit immediate operand.  */

int
shift_count_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT && CONST_OK_FOR_M (INTVAL (op)))
	  || GET_CODE (op) == CONSTANT_P_RTX);
}

/* Return 1 if OP is a 5 bit immediate operand.  */

int
shift_32bit_count_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_INT
	   && (INTVAL (op) >= 0 && INTVAL (op) < 32))
	  || GET_CODE (op) == CONSTANT_P_RTX);
}

/* Return 1 if OP is a 2, 4, 8, or 16 immediate operand.  */

int
shladd_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 2 || INTVAL (op) == 4
	      || INTVAL (op) == 8 || INTVAL (op) == 16));
}

/* Return 1 if OP is a -16, -8, -4, -1, 1, 4, 8, or 16 immediate operand. */

int
fetchadd_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT
          && (INTVAL (op) == -16 || INTVAL (op) == -8 ||
              INTVAL (op) == -4  || INTVAL (op) == -1 ||
              INTVAL (op) == 1   || INTVAL (op) == 4  ||
              INTVAL (op) == 8   || INTVAL (op) == 16));
}

/* Return 1 if OP is a floating-point constant zero, one, or a register.  */

int
reg_or_fp01_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((GET_CODE (op) == CONST_DOUBLE && CONST_DOUBLE_OK_FOR_G (op))
	  || GET_CODE (op) == CONSTANT_P_RTX
	  || register_operand (op, mode));
}

/* Return 1 if this is a comparison operator, which accepts an normal 8-bit
   signed immediate operand.  */

int
normal_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && (code == EQ || code == NE 
	      || code == GT || code == LE || code == GTU || code == LEU));
}

/* Return 1 if this is a comparison operator, which accepts an adjusted 8-bit
   signed immediate operand.  */

int
adjusted_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (op);
  return ((mode == VOIDmode || GET_MODE (op) == mode)
	  && (code == LT || code == GE || code == LTU || code == GEU));
}

/* Return 1 if OP is a call returning an HFA.  It is known to be a PARALLEL
   and the first section has already been tested.  */

int
call_multiple_values_operation (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int count = XVECLEN (op, 0) - 2;
  int i;
  int dest_regno;

  /* Perform a quick check so we don't block up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, 0))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != CALL)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, 0)));

  for (i = 1; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i + 2);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_SRC (elt)) != CALL
	  || GET_CODE (SET_DEST (elt)) != REG
	  || REGNO (SET_DEST (elt)) != dest_regno + i)
	return 0;
    }

  return 1;
}

  
/* Structure to be filled in by ia64_compute_frame_size with register
   save masks and offsets for the current function.  */

struct ia64_frame_info
{
  long total_size;		/* # bytes that the entire frame takes up.  */
  long var_size;		/* # bytes that variables take up.  */
  long args_size;		/* # bytes that outgoing arguments take up.  */
  long pretend_size;		/* # bytes that stdarg arguments take up.  */
  long pretend_pad_size;	/* # bytes padding to align stdarg args.  */
  long extra_size;		/* # bytes of extra gunk.  */
  long gr_size;			/* # bytes needed to store general regs.  */
  long fr_size;			/* # bytes needed to store FP regs.  */
  long fr_pad_size;		/* # bytes needed to align FP save area.  */
  long pr_size;			/* # bytes needed to store predicate regs.  */
  long br_size;			/* # bytes needed to store branch regs.  */
  HARD_REG_SET mask;		/* mask of saved registers.  */
  int initialized;		/* != 0 is frame size already calculated.  */
};

/* Current frame information calculated by compute_frame_size.  */
struct ia64_frame_info current_frame_info;

/* Helper function for INITIAL_ELIMINATION_OFFSET.  Return the offset from the
   frame pointer where b0 is saved.  */

int
ia64_rap_fp_offset ()
{
  return - current_frame_info.br_size;
}

/* Returns the number of bytes offset between the frame pointer and the stack
   pointer for the current function.  SIZE is the number of bytes of space
   needed for local variables.  */
unsigned int
ia64_compute_frame_size (size)
     int size;
{
  int total_size;
  int extra_size;
  int gr_size = 0;
  int fr_size = 0;
  int fr_pad_size = 0;
  int pr_size = 0;
  int br_size = 0;
  int pretend_pad_size = 0;
  int tmp;
  int regno;
  HARD_REG_SET mask;

  /* Reload used to round the frame size to STACK_BOUNDARY.  Now we do it
     here.  */
  size = IA64_STACK_ALIGN (size);

  CLEAR_HARD_REG_SET (mask);

  /* Calculate space needed for general registers.  */
  /* We never need to save any of the stacked registers, which are regs
     32 to 127.  */
  for (regno = GR_REG (0); regno <= GR_REG (31); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	SET_HARD_REG_BIT (mask, regno);
	gr_size += 8;
      }

  /* Allocate space to save/restore the unat from.  */
  if (gr_size != 0
      || current_function_varargs || current_function_stdarg)
    gr_size += 8;

  /* Calculate space needed for FP registers.  */
  for (regno = FR_REG (0); regno <= FR_REG (127); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	SET_HARD_REG_BIT (mask, regno);
	fr_size += 16;
      }

  /* Calculate space needed for predicate registers.  */
  for (regno = PR_REG (0); regno <= PR_REG (63); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	SET_HARD_REG_BIT (mask, regno);
	pr_size = 8;
      }

  /* Calculate space needed for branch registers.  */
  for (regno = BR_REG (0); regno <= BR_REG (7); regno++)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	SET_HARD_REG_BIT (mask, regno);
	br_size += 8;
      }

  /* The FR save area needs to be 16-byte aligned.  */
  if (fr_size)
    {
      tmp = (size + fr_size + pr_size + br_size);
      fr_pad_size = IA64_STACK_ALIGN (tmp) - tmp;
    }
  else
    fr_pad_size = 0;

  /* If we have an odd number of words of pretend arguments written to the
     stack, then the FR save area will be unaligned.  We pad below this area
     to keep things 16 byte aligned.  This needs to be kept distinct, to
     avoid confusing it with padding added below the GR save area, which does
     not affect the FR area alignment.  */
  pretend_pad_size = current_function_pretend_args_size % 16;

  /* The 16 bytes is for the scratch area.  */
  tmp = (size + gr_size + fr_pad_size + fr_size + pr_size + br_size
	 + current_function_outgoing_args_size + 16);
  tmp += (current_function_pretend_args_size
	  ? current_function_pretend_args_size - 16
	  : 0) + pretend_pad_size;
  total_size = IA64_STACK_ALIGN (tmp);
  extra_size = total_size - tmp + 16;

  /* If this is a leaf routine (BR_REG (0) is not live), and if there is no
     stack space needed for register saves, then don't allocate the 16 byte
     scratch area.  */
  if (total_size == 16 && ! regs_ever_live[BR_REG (0)])
    {
      total_size = 0;
      extra_size = 0;
    }

  current_frame_info.total_size = total_size;
  current_frame_info.var_size = size;
  current_frame_info.args_size = current_function_outgoing_args_size;
  current_frame_info.pretend_size
    = (current_function_pretend_args_size
       ? current_function_pretend_args_size - 16
       : 0);
  current_frame_info.pretend_pad_size = pretend_pad_size;
  current_frame_info.extra_size = extra_size;
  current_frame_info.gr_size = gr_size;
  current_frame_info.fr_size = fr_size;
  current_frame_info.fr_pad_size = fr_pad_size;
  current_frame_info.pr_size = pr_size;
  current_frame_info.br_size = br_size;
  COPY_HARD_REG_SET (current_frame_info.mask, mask);
  current_frame_info.initialized = reload_completed;

  return total_size;
}

void
save_restore_insns (save_p)
     int save_p;
{
  rtx insn;

  if (current_frame_info.gr_size + current_frame_info.fr_size
      + current_frame_info.br_size + current_frame_info.pr_size)
    {
      rtx tmp_reg = gen_rtx_REG (DImode, GR_REG (2));
      rtx tmp_post_inc = gen_rtx_POST_INC (DImode, tmp_reg);
      rtx tmp2_reg = gen_rtx_REG (DImode, GR_REG (3));
      int offset = (current_frame_info.total_size
		    - (current_frame_info.gr_size + current_frame_info.fr_size
		       + current_frame_info.fr_pad_size
		       + current_frame_info.br_size
		       + current_frame_info.pr_size
		       + current_frame_info.var_size
		       + current_frame_info.pretend_size
		       + current_frame_info.pretend_pad_size));
      rtx offset_rtx;
      int regno;
	
      /* If there is a frame pointer, then we use it instead of the stack
	 pointer, so that the stack pointer does not need to be valid when
	 the epilogue starts.  See EXIT_IGNORE_STACK.  */
      if (frame_pointer_needed)
	offset = offset - current_frame_info.total_size;

      if (CONST_OK_FOR_I (offset))
	offset_rtx = GEN_INT (offset);
      else
	{
	  offset_rtx = tmp_reg;
	  insn = emit_insn (gen_movdi (tmp_reg, GEN_INT (offset)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      insn = emit_insn (gen_adddi3 (tmp_reg,
				    (frame_pointer_needed ? frame_pointer_rtx
				     : stack_pointer_rtx),
				    offset_rtx));
      RTX_FRAME_RELATED_P (insn) = 1;

      /* Must save/restore ar.unat if any GR is spilled/restored.  */
      if (current_frame_info.gr_size != 0
	  || current_function_varargs || current_function_stdarg)
	{
	  rtx mem = gen_rtx_MEM (DImode, tmp_post_inc);
	  if (save_p)
	    {
	      insn = emit_insn (gen_unat_spill (tmp2_reg));
	      RTX_FRAME_RELATED_P (insn) = 1;
	      insn = emit_insn (gen_movdi (mem, tmp2_reg));
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  else
	    {
	      insn = emit_insn (gen_movdi (tmp2_reg, mem));
	      RTX_FRAME_RELATED_P (insn) = 1;
	      /* The restore happens after the last ld8.fill instruction.  */
	    }
	}

      for (regno = GR_REG (0); regno <= GR_REG (127); regno++)
	if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
	  {
	    rtx mem = gen_rtx_MEM (DImode, tmp_post_inc);
	    if (save_p)
	      insn = emit_insn (gen_gr_spill (mem,
					      gen_rtx_REG (DImode, regno)));
	    else
	      insn = emit_insn (gen_gr_restore (gen_rtx_REG (DImode, regno),
						mem));
	    RTX_FRAME_RELATED_P (insn) = 1;
	  }

      /* Now restore the unat register if necessary.  */
      if ((current_frame_info.gr_size != 0
	   || current_function_varargs || current_function_stdarg)
	  && ! save_p)
	emit_insn (gen_unat_restore (tmp2_reg));

      for (regno = FR_REG (0); regno <= FR_REG (127); regno++)
	if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
	  {
	    rtx mem = gen_rtx_MEM (XFmode, tmp_post_inc);
	    if (save_p)
	      insn = emit_insn (gen_fr_spill (mem,
					      gen_rtx_REG (XFmode, regno)));
	    else
	      insn = emit_insn (gen_fr_restore (gen_rtx_REG (XFmode, regno),
						mem));
	    RTX_FRAME_RELATED_P (insn) = 1;
	  }

      /* If one is used, we save/restore all of them.  */
      for (regno = PR_REG (0); regno <= PR_REG (63); regno++)
	if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
	  {
	    rtx mem = gen_rtx_MEM (DImode, tmp_post_inc);
	    if (save_p)
	      {
		insn = emit_insn (gen_pr_spill (tmp2_reg));
		RTX_FRAME_RELATED_P (insn) = 1;
		insn = emit_insn (gen_movdi (mem, tmp2_reg));
		RTX_FRAME_RELATED_P (insn) = 1;
	      }
	    else
	      {
		insn = emit_insn (gen_movdi (tmp2_reg, mem));
		RTX_FRAME_RELATED_P (insn) = 1;
		insn = emit_insn (gen_pr_restore (tmp2_reg));
		RTX_FRAME_RELATED_P (insn) = 1;
	      }
	    break;
	  }
					     
      for (regno = BR_REG (0); regno <= BR_REG (7); regno++)
	if (TEST_HARD_REG_BIT (current_frame_info.mask, regno))
	  {
	    rtx src, dest;

	    if (save_p)
	      {
		src = gen_rtx_REG (DImode, regno);
		dest = gen_rtx_MEM (DImode, tmp_post_inc);
	      }
	    else
	      {
		src = gen_rtx_MEM (DImode, tmp_post_inc);
		dest = gen_rtx_REG (DImode, regno);
	      }

	    insn = emit_insn (gen_movdi (tmp2_reg, src));
	    RTX_FRAME_RELATED_P (insn) = 1;
	    insn = emit_insn (gen_movdi (dest, tmp2_reg));
	    RTX_FRAME_RELATED_P (insn) = 1;
	  }
    }
}


/* Called after register allocation to add any instructions needed for the
   prologue.  Using a prologue insn is favored compared to putting all of the
   instructions in the FUNCTION_PROLOGUE macro, since it allows the scheduler
   to intermix instructions with the saves of the caller saved registers.  In
   some cases, it might be necessary to emit a barrier instruction as the last
   insn to prevent such scheduling.

   Also any insns generated here should have RTX_FRAME_RELATED_P(insn) = 1
   so that the debug info generation code can handle them properly.  */

/* ??? Get inefficient code when the frame size is larger than can fit in an
   adds instruction.  */

/* ??? If this is a leaf function, then fp/rp/ar.pfs should be put in the
   low 32 regs.  */

/* ??? Should not reserve a local register for rp/ar.pfs.  Should
   instead check to see if any local registers are unused, and if so,
   allocate them to rp/ar.pfs in that order.  Not sure what to do about
   fp, we may still need to reserve a local register for it.  */

void
ia64_expand_prologue ()
{
  rtx insn, offset;
  int i, locals, inputs, outputs, rotates;
  int frame_size = ia64_compute_frame_size (get_frame_size ());
  int leaf_function;
  int epilogue_p;
  edge e;

  /* ??? This seems like a leaf_function_p bug.  It calls get_insns which
     returns the first insn of the current sequence, not the first insn
     of the function.  We work around this by pushing to the topmost
     sequence first.  */
  push_topmost_sequence ();
  leaf_function = leaf_function_p ();
  pop_topmost_sequence ();

  /* If there is no epilogue, then we don't need some prologue insns.  We
     need to avoid emitting the dead prologue insns, because flow will complain
     about them.  */
  if (optimize)
    {
      for (e = EXIT_BLOCK_PTR->pred; e ; e = e->pred_next)
	if ((e->flags & EDGE_FAKE) == 0
	    && (e->flags & EDGE_FALLTHRU) != 0)
	  break;
      epilogue_p = (e != NULL);
    }
  else
    epilogue_p = 1;

  /* Find the highest local register used.  */
  /* We have only 80 local registers, because we reserve 8 for the inputs
     and 8 for the outputs.  */

  for (i = LOC_REG (79); i >= LOC_REG (0); i--)
    if (regs_ever_live[i])
      break;
  locals = i - LOC_REG (0) + 1;

  /* Likewise for inputs.  */

  for (i = IN_REG (7); i >= IN_REG (0); i--)
    if (regs_ever_live[i])
      break;
  inputs = i - IN_REG (0) + 1;

#if 0
  /* If the function was declared with syscall_linkage, then we may need to
     preserve all declared input registers, even if they weren't used.
     Currently, syscall_linkage does not have this effect.  */

  if (lookup_attribute ("syscall_linkage",
			TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))
    inputs = MAX (inputs, current_function_args_info.words);
#endif

  /* Likewise for outputs.  */

  for (i = OUT_REG (7); i >= OUT_REG (0); i--)
    if (regs_ever_live[i])
      break;
  outputs = i - OUT_REG (0) + 1;

  /* When -p profiling, we need one output register for the mcount argument.
     Likwise for -a profiling for the bb_init_func argument.  For -ax
     profiling, we need two output registers for the two bb_init_trace_func
     arguments.  */
  if (profile_flag || profile_block_flag == 1)
    outputs = MAX (outputs, 1);
  else if (profile_block_flag == 2)
    outputs = MAX (outputs, 2);

  /* No rotating register support as yet.  */

  rotates = 0;

  /* Allocate two extra locals for saving/restoring rp and ar.pfs.  Also
     allocate one local for use as the frame pointer if frame_pointer_needed
     is true.  */
  /* ??? If this is a leaf function, then we aren't using one of these local
     registers for the RP anymore.  */
  locals += 2 + frame_pointer_needed;

  /* Save these values in global registers for debugging info.  */
  ia64_input_regs = inputs;
  ia64_local_regs = locals;

  /* Set the local, input, and output register names.  We need to do this
     for GNU libc, which creates crti.S/crtn.S by splitting initfini.c in
     half.  If we use in/loc/out register names, then we get assembler errors
     in crtn.S because there is no alloc insn or regstk directive in there.
     We give in/loc/out names to unused registers, to make invalid uses of
     them easy to spot.  */
  if (! TARGET_REG_NAMES)
    {
      for (i = 0; i < 8; i++)
	{
	  if (i < inputs)
	    reg_names[IN_REG (i)] = ia64_reg_numbers[i];
	  else
	    reg_names[IN_REG (i)] = ia64_input_reg_names[i];
	}
      for (i = 0; i < 80; i++)
	{
	  if (i < locals)
	    reg_names[LOC_REG (i)] = ia64_reg_numbers[inputs + i];
	  else
	    reg_names[LOC_REG (i)] = ia64_local_reg_names[i];
	}
      for (i = 0; i < 8; i++)
	{
	  if (i < outputs)
	    reg_names[OUT_REG (i)] = ia64_reg_numbers[inputs + locals + i];
	  else
	    reg_names[OUT_REG (i)] = ia64_output_reg_names[i];
	}
    }

  /* Set the frame pointer register name now that it is known, and the
     local register names are known.  */
  if (frame_pointer_needed)
    {
      reg_names[FRAME_POINTER_REGNUM] 
	= reg_names[LOC_REG (locals - 3)];
      ia64_fp_regno = LOC_REG (inputs + locals - 3);
    }
  else
    ia64_fp_regno = 0;

  /* We don't need an alloc instruction if this is a leaf function, and the
     locals and outputs are both zero sized.  Since we have already allocated
     two locals for rp and ar.pfs, we check for two locals.  */
  /* Leaf functions can use output registers as call-clobbered temporaries.  */
  if (locals == 2 && outputs == 0 && leaf_function)
    {
      /* If there is no alloc, but there are input registers used, then we
	 need a .regstk directive.  */
      if (TARGET_REG_NAMES)
	ia64_need_regstk = 1;
      else
	ia64_need_regstk = 0;

      ia64_arpfs_regno = 0;
      ia64_rp_regno = 0;
    }
  else
    {
      ia64_need_regstk = 0;
      ia64_arpfs_regno = LOC_REG (locals - 1);

      emit_insn (gen_alloc (gen_rtx_REG (DImode, ia64_arpfs_regno),
			    GEN_INT (inputs), GEN_INT (locals),
			    GEN_INT (outputs), GEN_INT (rotates)));

      /* Emit a save of BR_REG (0) if we call other functions.
	 Do this even if this function doesn't return, as EH
         depends on this to be able to unwind the stack.  */
      if (! leaf_function)
	{
	  rtx ia64_rp_reg;

	  ia64_rp_regno = LOC_REG (locals - 2);
	  reg_names[RETURN_ADDRESS_REGNUM] = reg_names[ia64_rp_regno];

	  ia64_rp_reg = gen_rtx_REG (DImode, ia64_rp_regno);
	  insn = emit_move_insn (ia64_rp_reg, gen_rtx_REG (DImode,
							   BR_REG (0)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  if (! epilogue_p)
	    {
	      /* If we don't have an epilogue, then the return value
		 doesn't appear to be needed and the above store will
		 appear dead and will elicit a warning from flow.  */
	      emit_insn (gen_rtx_USE (VOIDmode, ia64_rp_reg));
	    }
	}
      else
	ia64_rp_regno = 0;
    }

  /* Set up frame pointer and stack pointer.  */
  if (frame_pointer_needed)
    {
      insn = emit_insn (gen_movdi (hard_frame_pointer_rtx, stack_pointer_rtx));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  if (frame_size != 0)
    {
      if (CONST_OK_FOR_I (-frame_size))
	offset = GEN_INT (-frame_size);
      else
	{
	  offset = gen_rtx_REG (DImode, GR_REG (2));
	  insn = emit_insn (gen_movdi (offset, GEN_INT (-frame_size)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      /* If there is a frame pointer, then we need to make the stack pointer
	 decrement depend on the frame pointer, so that the stack pointer
	 update won't be moved past fp-relative stores to the frame.  */
      if (frame_pointer_needed)
	insn = emit_insn (gen_prologue_allocate_stack (stack_pointer_rtx,
						       stack_pointer_rtx,
						       offset,
						       hard_frame_pointer_rtx));
      else
	insn = emit_insn (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
				      offset));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Save registers to frame.  */
  save_restore_insns (1);
}

/* Called after register allocation to add any instructions needed for the
   epilogue.  Using a epilogue insn is favored compared to putting all of the
   instructions in the FUNCTION_PROLOGUE macro, since it allows the scheduler
   to intermix instructions with the saves of the caller saved registers.  In
   some cases, it might be necessary to emit a barrier instruction as the last
   insn to prevent such scheduling.  */

void
ia64_expand_epilogue ()
{
  /* Restore registers from frame.  */
  save_restore_insns (0);

  /* ??? The gen_epilogue_deallocate_stack call below does not work.  This
     is mainly because there is no fp+offset addressing mode, so most loads
     from the frame do not actually use the frame pointer; they use a pseudo
     computed from the frame pointer.  The same problem exists with the
     stack pointer when there is no frame pointer.  I think this can be
     fixed only by making the dependency analysis code in sched smarter, so
     that it recognizes references to the frame, and makes succeeding stack
     pointer updates anti-dependent on them.  */
  emit_insn (gen_blockage ());

  if (frame_pointer_needed)
    {
      /* If there is a frame pointer, then we need to make the stack pointer
	 restore depend on the frame pointer, so that the stack pointer
	 restore won't be moved up past fp-relative loads from the frame.  */
      emit_insn (gen_epilogue_deallocate_stack (stack_pointer_rtx,
						hard_frame_pointer_rtx));
    }
  else
    {
      int frame_size = current_frame_info.total_size;
      rtx offset;

      if (frame_size != 0)
	{
	  if (CONST_OK_FOR_I (frame_size))
	    offset = GEN_INT (frame_size);
	  else
	    {
	      offset = gen_rtx_REG (DImode, GR_REG (2));
	      emit_insn (gen_movdi (offset, GEN_INT (frame_size)));
	    }
	  emit_insn (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
				 offset));
	}
    }

  if (ia64_arpfs_regno)
    emit_insn (gen_pfs_restore (gen_rtx_REG (DImode, ia64_arpfs_regno)));

  if (ia64_rp_regno)
    emit_move_insn (gen_rtx_REG (DImode, BR_REG (0)),
		    gen_rtx_REG (DImode, ia64_rp_regno));

  emit_jump_insn (gen_return_internal (gen_rtx_REG (DImode, BR_REG (0))));
}

/* Emit the function prologue.  */

void
ia64_function_prologue (file, size)
     FILE *file;
     int size;
{
  if (ia64_need_regstk)
    fprintf (file, "\t.regstk %d, 0, 0, 0\n", ia64_input_regs);

  /* ??? Emit .body directive.  GNU as ignores .body currently.  */
}

/* Emit the function epilogue.  */

void
ia64_function_epilogue (file, size)
     FILE *file;
     int size;
{
}

/* Return 1 if br.ret can do all the work required to return from a
   function.  */

int
ia64_direct_return ()
{
  return (reload_completed && ! frame_pointer_needed
	  && ia64_compute_frame_size (get_frame_size ()));
}


/* Do any needed setup for a variadic function.  CUM has not been updated
   for the last named argument which has type TYPE and mode MODE.  */
void
ia64_setup_incoming_varargs (cum, int_mode, type, pretend_size, second_time)
     CUMULATIVE_ARGS cum;
     int             int_mode;
     tree            type;
     int *           pretend_size;
     int	     second_time;
{
  /* If this is a stdarg function, then don't save the current argument.  */
  int offset = ! current_function_varargs;

  if (cum.words < MAX_ARGUMENT_SLOTS)
    {
      if (! second_time)
	{
	  int i;
	  int first_reg = GR_ARG_FIRST + cum.words + offset;
	  rtx tmp_reg = gen_rtx_REG (DImode, GR_REG (16));
	  rtx tmp_post_inc = gen_rtx_POST_INC (DImode, tmp_reg);
	  rtx mem = gen_rtx_MEM (DImode, tmp_post_inc);
	  rtx insn;

	  /* We must emit st8.spill insns instead of st8 because we might
	     be saving non-argument registers, and non-argument registers might
	     not contain valid values.  */
	  emit_move_insn (tmp_reg, virtual_incoming_args_rtx);
	  for (i = first_reg; i < GR_ARG_FIRST + 8; i++)
	    {
	      insn = emit_insn (gen_gr_spill (mem, gen_rtx_REG (DImode, i)));
	      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_INC, tmp_reg, 0);
	    }
	}
      *pretend_size = ((MAX_ARGUMENT_SLOTS - cum.words - offset)
		       * UNITS_PER_WORD);
    }
}

/* Check whether TYPE is a homogeneous floating point aggregate.  If
   it is, return the mode of the floating point type that appears
   in all leafs.  If it is not, return VOIDmode.

   An aggregate is a homogeneous floating point aggregate is if all
   fields/elements in it have the same floating point type (e.g,
   SFmode).  128-bit quad-precision floats are excluded.  */

static enum machine_mode
hfa_element_mode (type, nested)
     tree type;
     int nested;
{
  enum machine_mode element_mode = VOIDmode;
  enum machine_mode mode;
  enum tree_code code = TREE_CODE (type);
  int know_element_mode = 0;
  tree t;

  switch (code)
    {
    case VOID_TYPE:	case INTEGER_TYPE:	case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:	case CHAR_TYPE:		case POINTER_TYPE:
    case OFFSET_TYPE:	case REFERENCE_TYPE:	case METHOD_TYPE:
    case FILE_TYPE:	case SET_TYPE:		case LANG_TYPE:
    case FUNCTION_TYPE:
      return VOIDmode;

      /* Fortran complex types are supposed to be HFAs, so we need to handle
	 gcc's COMPLEX_TYPEs as HFAs.  We need to exclude the integral complex
	 types though.  */
    case COMPLEX_TYPE:
      if (GET_MODE_CLASS (TYPE_MODE (type)) == MODE_COMPLEX_FLOAT)
	return mode_for_size (GET_MODE_UNIT_SIZE (TYPE_MODE (type))
			      * BITS_PER_UNIT, MODE_FLOAT, 0);
      else
	return VOIDmode;

    case REAL_TYPE:
      /* We want to return VOIDmode for raw REAL_TYPEs, but the actual
	 mode if this is contained within an aggregate.  */
      if (nested)
	return TYPE_MODE (type);
      else
	return VOIDmode;

    case ARRAY_TYPE:
      return TYPE_MODE (TREE_TYPE (type));

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      for (t = TYPE_FIELDS (type); t; t = TREE_CHAIN (t))
	{
	  if (TREE_CODE (t) != FIELD_DECL)
	    continue;

	  mode = hfa_element_mode (TREE_TYPE (t), 1);
	  if (know_element_mode)
	    {
	      if (mode != element_mode)
		return VOIDmode;
	    }
	  else if (GET_MODE_CLASS (mode) != MODE_FLOAT)
	    return VOIDmode;
	  else
	    {
	      know_element_mode = 1;
	      element_mode = mode;
	    }
	}
      return element_mode;

    default:
      /* If we reach here, we probably have some front-end specific type
	 that the backend doesn't know about.  This can happen via the
	 aggregate_value_p call in init_function_start.  All we can do is
	 ignore unknown tree types.  */
      return VOIDmode;
    }

  return VOIDmode;
}

/* Return rtx for register where argument is passed, or zero if it is passed
   on the stack.  */

/* ??? 128-bit quad-precision floats are always passed in general
   registers.  */

rtx
ia64_function_arg (cum, mode, type, named, incoming)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
     int incoming;
{
  int basereg = (incoming ? GR_ARG_FIRST : AR_ARG_FIRST);
  int words = (((mode == BLKmode ? int_size_in_bytes (type)
		 : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
	       / UNITS_PER_WORD);
  int offset = 0;
  enum machine_mode hfa_mode = VOIDmode;

  /* Arguments larger than 8 bytes start at the next even boundary.  */
  if (words > 1 && (cum->words & 1))
    offset = 1;

  /* If all argument slots are used, then it must go on the stack.  */
  if (cum->words + offset >= MAX_ARGUMENT_SLOTS)
    return 0;

  /* Check for and handle homogeneous FP aggregates.  */
  if (type)
    hfa_mode = hfa_element_mode (type, 0);

  /* Unnamed prototyped hfas are passed as usual.  Named prototyped hfas
     and unprototyped hfas are passed specially.  */
  if (hfa_mode != VOIDmode && (! cum->prototype || named))
    {
      rtx loc[16];
      int i = 0;
      int fp_regs = cum->fp_regs;
      int int_regs = cum->words + offset;
      int hfa_size = GET_MODE_SIZE (hfa_mode);
      int byte_size;
      int args_byte_size;

      /* If prototyped, pass it in FR regs then GR regs.
	 If not prototyped, pass it in both FR and GR regs.

	 If this is an SFmode aggregate, then it is possible to run out of
	 FR regs while GR regs are still left.  In that case, we pass the
	 remaining part in the GR regs.  */

      /* Fill the FP regs.  We do this always.  We stop if we reach the end
	 of the argument, the last FP register, or the last argument slot.  */

      byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
      args_byte_size = int_regs * UNITS_PER_WORD;
      offset = 0;
      for (; (offset < byte_size && fp_regs < MAX_ARGUMENT_SLOTS
	      && args_byte_size < (MAX_ARGUMENT_SLOTS * UNITS_PER_WORD)); i++)
	{
	  loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (hfa_mode, (FR_ARG_FIRST
							      + fp_regs)),
				      GEN_INT (offset));
	  /* ??? Padding for XFmode type?  */
	  offset += hfa_size;
	  args_byte_size += hfa_size;
	  fp_regs++;
	}

      /* If no prototype, then the whole thing must go in GR regs.  */
      if (! cum->prototype)
	offset = 0;
      /* If this is an SFmode aggregate, then we might have some left over
	 that needs to go in GR regs.  */
      else if (byte_size != offset)
	int_regs += offset / UNITS_PER_WORD;

      /* Fill in the GR regs.  We must use DImode here, not the hfa mode.  */

      for (; offset < byte_size && int_regs < MAX_ARGUMENT_SLOTS; i++)
	{
	  enum machine_mode gr_mode = DImode;

	  /* If we have an odd 4 byte hunk because we ran out of FR regs,
	     then this goes in a GR reg left adjusted/little endian, right
	     adjusted/big endian.  */
	  /* ??? Currently this is handled wrong, because 4-byte hunks are
	     always right adjusted/little endian.  */
	  if (offset & 0x4)
	    gr_mode = SImode;
	  /* If we have an even 4 byte hunk because the aggregate is a
	     multiple of 4 bytes in size, then this goes in a GR reg right
	     adjusted/little endian.  */
	  else if (byte_size - offset == 4)
	    gr_mode = SImode;

	  loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (gr_mode, (basereg
							     + int_regs)),
				      GEN_INT (offset));
	  offset += GET_MODE_SIZE (gr_mode);
	  int_regs++;
	}

      /* If we ended up using just one location, just return that one loc.  */
      if (i == 1)
	return XEXP (loc[0], 0);
      else
	return gen_rtx_PARALLEL (mode, gen_rtvec_v (i, loc));
    }

  /* Integral and aggregates go in general registers.  If we have run out of
     FR registers, then FP values must also go in general registers.  This can
     happen when we have a SFmode HFA.  */
  else if (! FLOAT_MODE_P (mode) || cum->fp_regs == MAX_ARGUMENT_SLOTS)
    return gen_rtx_REG (mode, basereg + cum->words + offset);

  /* If there is a prototype, then FP values go in a FR register when
     named, and in a GR registeer when unnamed.  */
  else if (cum->prototype)
    {
      if (! named)
	return gen_rtx_REG (mode, basereg + cum->words + offset);
      else
	return gen_rtx_REG (mode, FR_ARG_FIRST + cum->fp_regs);
    }
  /* If there is no prototype, then FP values go in both FR and GR
     registers.  */
  else
    {
      rtx fp_reg = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (mode, (FR_ARG_FIRST
							  + cum->fp_regs)),
				      const0_rtx);
      rtx gr_reg = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (mode,
						   (basereg + cum->words
						    + offset)),
				      const0_rtx);
						   
      return gen_rtx_PARALLEL (mode, gen_rtvec (2, fp_reg, gr_reg));
    }
}

/* Return number of words, at the beginning of the argument, that must be
   put in registers.  0 is the argument is entirely in registers or entirely
   in memory.  */

int
ia64_function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int words = (((mode == BLKmode ? int_size_in_bytes (type)
		 : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
	       / UNITS_PER_WORD);
  int offset = 0;

  /* Arguments larger than 8 bytes start at the next even boundary.  */
  if (words > 1 && (cum->words & 1))
    offset = 1;

  /* If all argument slots are used, then it must go on the stack.  */
  if (cum->words + offset >= MAX_ARGUMENT_SLOTS)
    return 0;

  /* It doesn't matter whether the argument goes in FR or GR regs.  If
     it fits within the 8 argument slots, then it goes entirely in
     registers.  If it extends past the last argument slot, then the rest
     goes on the stack.  */

  if (words + cum->words + offset <= MAX_ARGUMENT_SLOTS)
    return 0;

  return MAX_ARGUMENT_SLOTS - cum->words - offset;
}

/* Update CUM to point after this argument.  This is patterned after
   ia64_function_arg.  */

void
ia64_function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
{
  int words = (((mode == BLKmode ? int_size_in_bytes (type)
		 : GET_MODE_SIZE (mode)) + UNITS_PER_WORD - 1)
	       / UNITS_PER_WORD);
  int offset = 0;
  enum machine_mode hfa_mode = VOIDmode;

  /* If all arg slots are already full, then there is nothing to do.  */
  if (cum->words >= MAX_ARGUMENT_SLOTS)
    return;

  /* Arguments larger than 8 bytes start at the next even boundary.  */
  if (words > 1 && (cum->words & 1))
    offset = 1;

  cum->words += words + offset;

  /* Check for and handle homogeneous FP aggregates.  */
  if (type)
    hfa_mode = hfa_element_mode (type, 0);

  /* Unnamed prototyped hfas are passed as usual.  Named prototyped hfas
     and unprototyped hfas are passed specially.  */
  if (hfa_mode != VOIDmode && (! cum->prototype || named))
    {
      int fp_regs = cum->fp_regs;
      /* This is the original value of cum->words + offset.  */
      int int_regs = cum->words - words;
      int hfa_size = GET_MODE_SIZE (hfa_mode);
      int byte_size;
      int args_byte_size;

      /* If prototyped, pass it in FR regs then GR regs.
	 If not prototyped, pass it in both FR and GR regs.

	 If this is an SFmode aggregate, then it is possible to run out of
	 FR regs while GR regs are still left.  In that case, we pass the
	 remaining part in the GR regs.  */

      /* Fill the FP regs.  We do this always.  We stop if we reach the end
	 of the argument, the last FP register, or the last argument slot.  */

      byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (type) : GET_MODE_SIZE (mode));
      args_byte_size = int_regs * UNITS_PER_WORD;
      offset = 0;
      for (; (offset < byte_size && fp_regs < MAX_ARGUMENT_SLOTS
	      && args_byte_size < (MAX_ARGUMENT_SLOTS * UNITS_PER_WORD));)
	{
	  /* ??? Padding for XFmode type?  */
	  offset += hfa_size;
	  args_byte_size += hfa_size;
	  fp_regs++;
	}

      cum->fp_regs = fp_regs;
    }

  /* Integral and aggregates go in general registers.  If we have run out of
     FR registers, then FP values must also go in general registers.  This can
     happen when we have a SFmode HFA.  */
  else if (! FLOAT_MODE_P (mode) || cum->fp_regs == MAX_ARGUMENT_SLOTS)
    return;

  /* If there is a prototype, then FP values go in a FR register when
     named, and in a GR registeer when unnamed.  */
  else if (cum->prototype)
    {
      if (! named)
	return;
      else
	/* ??? Complex types should not reach here.  */
	cum->fp_regs += (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT ? 2 : 1);
    }
  /* If there is no prototype, then FP values go in both FR and GR
     registers.  */
  else
    /* ??? Complex types should not reach here.  */
    cum->fp_regs += (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT ? 2 : 1);

  return;
}

/* Implement va_start.  */

void
ia64_va_start (stdarg_p, valist, nextarg)
     int stdarg_p;
     tree valist;
     rtx nextarg;
{
  int arg_words;
  int ofs;

  arg_words = current_function_args_info.words;

  if (stdarg_p)
    ofs = 0;
  else
    ofs = (arg_words >= MAX_ARGUMENT_SLOTS ? -UNITS_PER_WORD : 0);

  nextarg = plus_constant (nextarg, ofs);
  std_expand_builtin_va_start (1, valist, nextarg);
}

/* Implement va_arg.  */

rtx
ia64_va_arg (valist, type)
     tree valist, type;
{
  HOST_WIDE_INT size;
  tree t;

  /* Arguments larger than 8 bytes are 16 byte aligned.  */
  size = int_size_in_bytes (type);
  if (size > UNITS_PER_WORD)
    {
      t = build (PLUS_EXPR, TREE_TYPE (valist), valist,
		 build_int_2 (2 * UNITS_PER_WORD - 1, 0));
      t = build (BIT_AND_EXPR, TREE_TYPE (t), t, 
		 build_int_2 (-2 * UNITS_PER_WORD, -1));
      t = build (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  return std_expand_builtin_va_arg (valist, type);
}

/* Return 1 if function return value returned in memory.  Return 0 if it is
   in a register.  */

int
ia64_return_in_memory (valtype)
     tree valtype;
{
  enum machine_mode mode;
  enum machine_mode hfa_mode;
  int byte_size;

  mode = TYPE_MODE (valtype);
  byte_size = ((mode == BLKmode)
	       ? int_size_in_bytes (valtype) : GET_MODE_SIZE (mode));

  /* Hfa's with up to 8 elements are returned in the FP argument registers.  */

  hfa_mode = hfa_element_mode (valtype, 0);
  if (hfa_mode != VOIDmode)
    {
      int hfa_size = GET_MODE_SIZE (hfa_mode);

      /* ??? Padding for XFmode type?  */
      if (byte_size / hfa_size > MAX_ARGUMENT_SLOTS)
	return 1;
      else
	return 0;
    }

  else if (byte_size > UNITS_PER_WORD * MAX_INT_RETURN_SLOTS)
    return 1;
  else
    return 0;
}

/* Return rtx for register that holds the function return value.  */

rtx
ia64_function_value (valtype, func)
     tree valtype;
     tree func;
{
  enum machine_mode mode;
  enum machine_mode hfa_mode;

  mode = TYPE_MODE (valtype);
  hfa_mode = hfa_element_mode (valtype, 0);

  if (hfa_mode != VOIDmode)
    {
      rtx loc[8];
      int i;
      int hfa_size;
      int byte_size;
      int offset;

      hfa_size = GET_MODE_SIZE (hfa_mode);
      byte_size = ((mode == BLKmode)
		   ? int_size_in_bytes (valtype) : GET_MODE_SIZE (mode));
      offset = 0;
      for (i = 0; offset < byte_size; i++)
	{
	  loc[i] = gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (hfa_mode, FR_ARG_FIRST + i),
				      GEN_INT (offset));
	  /* ??? Padding for XFmode type?  */
	  offset += hfa_size;
	}

      if (i == 1)
	return XEXP (loc[0], 0);
      else
	return gen_rtx_PARALLEL (mode, gen_rtvec_v (i, loc));
    }
  else if (FLOAT_TYPE_P (valtype))
    return gen_rtx_REG (mode, FR_ARG_FIRST);
  else
    return gen_rtx_REG (mode, GR_RET_FIRST);
}

/* Print a memory address as an operand to reference that memory location.  */

/* ??? Do we need this?  It gets used only for 'a' operands.  We could perhaps
   also call this from ia64_print_operand for memory addresses.  */

void
ia64_print_operand_address (stream, address)
     FILE * stream;
     rtx    address;
{
}

/* Print an operand to a assembler instruction.
   B    Work arounds for hardware bugs.
   C	Swap and print a comparison operator.
   D	Print an FP comparison operator.
   E    Print 32 - constant, for SImode shifts as extract.
   F	A floating point constant 0.0 emitted as f0, or 1.0 emitted as f1, or
        a floating point register emitted normally.
   I	Invert a predicate register by adding 1.
   O	Append .acq for volatile load.
   P	Postincrement of a MEM.
   Q	Append .rel for volatile store.
   S	Shift amount for shladd instruction.
   T	Print an 8-bit sign extended number (K) as a 32-bit unsigned number
	for Intel assembler.
   U	Print an 8-bit sign extended number (K) as a 64-bit unsigned number
	for Intel assembler.
   r	Print register name, or constant 0 as r0.  HP compatibility for
	Linux kernel.  */
void
ia64_print_operand (file, x, code)
     FILE * file;
     rtx    x;
     int    code;
{
  switch (code)
    {
      /* XXX Add other codes here.  */
      
    case 0:
      /* Handled below.  */
      break;
      
    case 'B':
      if (TARGET_A_STEP)
	fputs (" ;; nop 0 ;; nop 0 ;;", file);
      return;

    case 'C':
      {
	enum rtx_code c = swap_condition (GET_CODE (x));
	fputs (GET_RTX_NAME (c), file);
	return;
      }

    case 'D':
      fputs (GET_CODE (x) == NE ? "neq" : GET_RTX_NAME (GET_CODE (x)), file);
      return;

    case 'E':
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, 32 - INTVAL (x));
      return;

    case 'F':
      if (x == CONST0_RTX (GET_MODE (x)))
	fputs (reg_names [FR_REG (0)], file);
      else if (x == CONST1_RTX (GET_MODE (x)))
	fputs (reg_names [FR_REG (1)], file);
      else if (GET_CODE (x) == REG)
	fputs (reg_names [REGNO (x)], file);
      else
	abort ();
      return;

    case 'I':
      fputs (reg_names [REGNO (x) + 1], file);
      return;

    case 'O':
      if (MEM_VOLATILE_P (x))
	fputs(".acq", file);
      return;

    case 'P':
      {
	int value;

	if (GET_CODE (XEXP (x, 0)) != POST_INC
	    && GET_CODE (XEXP (x, 0)) != POST_DEC)
	  return;

	fputs (", ", file);

	value = GET_MODE_SIZE (GET_MODE (x));

	/* ??? This is for ldf.fill and stf.spill which use XFmode, but which
	   actually need 16 bytes increments.  Perhaps we can change them
	   to use TFmode instead.  Or don't use POST_DEC/POST_INC for them.
	   Currently, there are no other uses of XFmode, so hacking it here
	   is no problem.  */
	if (value == 12)
	  value = 16;

	if (GET_CODE (XEXP (x, 0)) == POST_DEC)
	  value = -value;
    
	fprintf (file, "%d", value);
	return;
      }

    case 'Q':
      if (MEM_VOLATILE_P (x))
	fputs(".rel", file);
      return;

    case 'S':
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, exact_log2 (INTVAL (x)));
      return;

    case 'T':
      if (! TARGET_GNU_AS && GET_CODE (x) == CONST_INT)
	{
	  fprintf (file, "0x%x", INTVAL (x) & 0xffffffff);
	  return;
	}
      break;

    case 'U':
      if (! TARGET_GNU_AS && GET_CODE (x) == CONST_INT)
	{
	  char *prefix = "0x";
	  if (INTVAL (x) & 0x80000000)
	    {
	      fprintf (file, "0xffffffff");
	      prefix = "";
	    }
	  fprintf (file, "%s%x", prefix, INTVAL (x) & 0xffffffff);
	  return;
	}
      break;
      
    case 'r':
      /* If this operand is the constant zero, write it as zero.  */
      if (GET_CODE (x) == REG)
	fputs (reg_names[REGNO (x)], file);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fputs ("r0", file);
      else
	output_operand_lossage ("invalid %%r value");
      return;

    default:
      output_operand_lossage ("ia64_print_operand: unknown code");
      return;
    }

  switch (GET_CODE (x))
    {
      /* This happens for the spill/restore instructions.  */
    case POST_INC:
      x = XEXP (x, 0);
      /* ... fall through ... */

    case REG:
      fputs (reg_names [REGNO (x)], file);
      break;

    case MEM:
      {
	rtx addr = XEXP (x, 0);
	if (GET_CODE (addr) == POST_INC || GET_CODE (addr) == POST_DEC)
	  addr = XEXP (addr, 0);
	fprintf (file, "[%s]", reg_names [REGNO (addr)]);
	break;
      }
      
    default:
      output_addr_const (file, x);
      break;
    }

  return;
}



/* This function returns the register class required for a secondary
   register when copying between one of the registers in CLASS, and X,
   using MODE.  A return value of NO_REGS means that no secondary register
   is required.  */

enum reg_class
ia64_secondary_reload_class (class, mode, x)
     enum reg_class class;
     enum machine_mode mode;
     rtx x;
{
  int regno = -1;

  if (GET_CODE (x) == REG || GET_CODE (x) == SUBREG)
    regno = true_regnum (x);

  /* ??? This is required because of a bad gcse/cse/global interaction.
     We end up with two pseudos with overlapping lifetimes both of which are
     equiv to the same constant, and both which need to be in BR_REGS.  This
     results in a BR_REGS to BR_REGS copy which doesn't exist.  To reproduce,
     return NO_REGS here, and compile divdi3 in libgcc2.c.  This seems to be
     a cse bug.  cse_basic_block_end changes depending on the path length,
     which means the qty_first_reg check in make_regs_eqv can give different
     answers at different times.  */
  /* ??? At some point I'll probably need a reload_indi pattern to handle
     this.  */
  if (class == BR_REGS && BR_REGNO_P (regno))
    return GR_REGS;

  /* This is needed if a pseudo used as a call_operand gets spilled to a
     stack slot.  */
  if (class == BR_REGS && GET_CODE (x) == MEM)
    return GR_REGS;

  /* This can happen when a paradoxical subreg is an operand to the muldi3
     pattern.  */
  /* ??? This shouldn't be necessary after instruction scheduling is enabled,
     because paradoxical subregs are not accepted by register_operand when
     INSN_SCHEDULING is defined.  Or alternatively, stop the paradoxical subreg
     stupidity in the *_operand functions in recog.c.  */
  if ((class == FR_REGS || class == FR_INT_REGS || class == FR_FP_REGS)
      && GET_CODE (x) == MEM
      && (GET_MODE (x) == SImode || GET_MODE (x) == HImode
	  || GET_MODE (x) == QImode))
    return GR_REGS;

  /* This can happen because of the ior/and/etc patterns that accept FP
     registers as operands.  If the third operand is a constant, then it
     needs to be reloaded into a FP register.  */
  if ((class == FR_REGS || class == FR_INT_REGS || class == FR_FP_REGS)
      && GET_CODE (x) == CONST_INT)
    return GR_REGS;

  /* Moving a integer from an FP register to memory requires a general register
     as an intermediary.  This is not necessary if we are moving a DImode
     subreg of a DFmode value from an FP register to memory, since stfd will
     do the right thing in this case.  */
  if (class == FR_INT_REGS && GET_CODE (x) == MEM && GET_MODE (x) == DImode)
    return GR_REGS;

  /* ??? This happens if we cse/gcse a CCmode value across a call, and the
     function has a nonlocal goto.  This is because global does not allocate
     call crossing pseudos to hard registers when current_function_has_
     nonlocal_goto is true.  This is relatively common for C++ programs that
     use exceptions.  To reproduce, return NO_REGS and compile libstdc++.  */
  if (class == PR_REGS && GET_CODE (x) == MEM)
    return GR_REGS;

  return NO_REGS;
}


/* Emit text to declare externally defined variables and functions, because
   the Intel assembler does not support undefined externals.  */

void
ia64_asm_output_external (file, decl, name)
     FILE *file;
     tree decl;
     char *name;
{
  int save_referenced;

  /* GNU as does not need anything here.  */
  if (TARGET_GNU_AS)
    return;

  /* ??? The Intel assembler creates a reference that needs to be satisfied by
     the linker when we do this, so we need to be careful not to do this for
     builtin functions which have no library equivalent.  Unfortunately, we
     can't tell here whether or not a function will actually be called by
     expand_expr, so we pull in library functions even if we may not need
     them later.  */
  if (! strcmp (name, "__builtin_next_arg")
      || ! strcmp (name, "alloca")
      || ! strcmp (name, "__builtin_constant_p")
      || ! strcmp (name, "__builtin_args_info"))
    return;

  /* assemble_name will set TREE_SYMBOL_REFERENCED, so we must save and
     restore it.  */
  save_referenced = TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl));
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      fprintf (file, "\t%s\t ", TYPE_ASM_OP);
      assemble_name (file, name);
      putc (',', file);
      fprintf (file, TYPE_OPERAND_FMT, "function");
      putc ('\n', file);
    }
  ASM_GLOBALIZE_LABEL (file, name);
  TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)) = save_referenced;
}

/* Parse the -mfixed-range= option string.  */

static void
fix_range (str)
     char *str;
{
  int i, first, last;
  char *dash, *comma;

  /* str must be of the form REG1'-'REG2{,REG1'-'REG} where REG1 and
     REG2 are either register names or register numbers.  The effect
     of this option is to mark the registers in the range from REG1 to
     REG2 as ``fixed'' so they won't be used by the compiler.  This is
     used, e.g., to ensure that kernel mode code doesn't use f32-f127.  */

  while (1)
    {
      dash = strchr (str, '-');
      if (!dash)
	{
	  warning ("value of -mfixed-range must have form REG1-REG2");
	  return;
	}
      *dash = '\0';

      comma = strchr (dash + 1, ',');
      if (comma)
	*comma = '\0';

      first = decode_reg_name (str);
      if (first < 0)
	{
	  warning ("unknown register name: %s", str);
	  return;
	}

      last = decode_reg_name (dash + 1);
      if (last < 0)
	{
	  warning ("unknown register name: %s", dash + 1);
	  return;
	}

      *dash = '-';

      if (first > last)
	{
	  warning ("%s-%s is an empty range", str, dash + 1);
	  return;
	}

      for (i = first; i <= last; ++i)
	fixed_regs[i] = call_used_regs[i] = 1;

      if (!comma)
	break;

      *comma = ',';
      str = comma + 1;
    }
}

/* Called to register all of our global variables with the garbage
   collector.  */

static void
ia64_add_gc_roots ()
{
  ggc_add_rtx_root (&ia64_compare_op0, 1);
  ggc_add_rtx_root (&ia64_compare_op1, 1);
}

/* Handle TARGET_OPTIONS switches.  */

void
ia64_override_options ()
{
  if (ia64_fixed_range_string)
    fix_range (ia64_fixed_range_string);

  ia64_section_threshold = g_switch_set ? g_switch_value : IA64_DEFAULT_GVALUE;

  ia64_add_gc_roots ();
}

/* The following collection of routines emit instruction group stop bits as
   necessary to avoid dependencies.  */

/* Need to track some additional registers as far as serialization is
   concerned so we can properly handle br.call and br.ret.  We could
   make these registers visible to gcc, but since these registers are
   never explicitly used in gcc generated code, it seems wasteful to
   do so (plus it would make the call and return patterns needlessly
   complex).  */
#define REG_GP		(GR_REG (1))
#define REG_RP		(BR_REG (0))
#define REG_AR_PFS	(FIRST_PSEUDO_REGISTER)
#define REG_AR_CFM	(FIRST_PSEUDO_REGISTER + 1)
/* ??? This will eventually need to be a hard register.  */
#define REG_AR_EC	(FIRST_PSEUDO_REGISTER + 2)
/* This is used for volatile asms which may require a stop bit immediately
   before and after them.  */
#define REG_VOLATILE	(FIRST_PSEUDO_REGISTER + 3)
#define NUM_REGS	(FIRST_PSEUDO_REGISTER + 4)

/* For each register, we keep track of how many times it has been
   written in the current instruction group.  If a register is written
   unconditionally (no qualifying predicate), WRITE_COUNT is set to 2
   and FIRST_PRED is ignored.  If a register is written if its
   qualifying predicate P is true, we set WRITE_COUNT to 1 and
   FIRST_PRED to P.  Later on, the same register may be written again
   by the complement of P (P+1 if P is even, P-1, otherwise) and when
   this happens, WRITE_COUNT gets set to 2.  The result of this is
   that whenever an insn attempts to write a register whose
   WRITE_COUNT is two, we need to issue a insn group barrier first.  */
struct reg_write_state
{
  char write_count;
  char written_by_fp;	/* Was register written by a floating-point insn?  */
  short first_pred;	/* 0 means ``no predicate'' */
};

/* Cumulative info for the current instruction group.  */
struct reg_write_state rws_sum[NUM_REGS];
/* Info for the current instruction.  This gets copied to rws_sum after a
   stop bit is emitted.  */
struct reg_write_state rws_insn[NUM_REGS];

/* Misc flags needed to compute RAW/WAW dependencies while we are traversing
   RTL for one instruction.  */
struct reg_flags
{
  unsigned int is_write : 1;	/* Is register being written?  */
  unsigned int is_fp : 1;	/* Is register used as part of an fp op?  */
  unsigned int is_branch : 1;	/* Is register used as part of a branch?  */
};

/* Update *RWS for REGNO, which is being written by the current instruction,
   with predicate PRED, and associated register flags in FLAGS.  */

static void
rws_update (rws, regno, flags, pred)
     struct reg_write_state *rws;
     int regno;
     struct reg_flags flags;
     int pred;
{
  rws[regno].write_count += pred ? 1 : 2;
  rws[regno].written_by_fp |= flags.is_fp;
  rws[regno].first_pred = pred;
}

/* Handle an access to register REGNO of type FLAGS using predicate register
   PRED.  Update rws_insn and rws_sum arrays.  Return 1 if this access creates
   a dependency with an earlier instruction in the same group.  */

static int
rws_access_reg (regno, flags, pred)
     int regno;
     struct reg_flags flags;
     int pred;
{
  int need_barrier = 0;
  int is_predicate_reg;

  if (regno >= NUM_REGS)
    abort ();

  if (flags.is_write)
    {
      /* One insn writes same reg multiple times?  */
      if (rws_insn[regno].write_count > 0)
	abort ();

      /* Update info for current instruction.  */
      rws_update (rws_insn, regno, flags, pred);

      /* ??? This is necessary because predicate regs require two hard
	 registers.  However, this should be using HARD_REGNO_NREGS so that
	 it works for all multi-reg hard registers, instead of only for
	 predicate registers.  */
      is_predicate_reg = REGNO_REG_CLASS (regno) == PR_REGS;
      if (is_predicate_reg)
	rws_update (rws_insn, regno + 1, flags, pred);

      switch (rws_sum[regno].write_count)
	{
	case 0:
	  /* The register has not been written yet.  */
	  rws_update (rws_sum, regno, flags, pred);
	  if (is_predicate_reg)
	    rws_update (rws_sum, regno + 1, flags, pred);
	  break;

	case 1:
	  /* The register has been written via a predicate.  If this is
	     not a complementary predicate, then we need a barrier.  */
	  /* ??? This assumes that P and P+1 are always complementary
	     predicates for P even.  */
	  if ((rws_sum[regno].first_pred ^ 1) != pred)
	    need_barrier = 1;
	  rws_update (rws_sum, regno, flags, pred);
	  if (is_predicate_reg)
	    rws_update (rws_sum, regno + 1, flags, pred);
	  break;

	case 2:
	  /* The register has been unconditionally written already.  We
	     need a barrier.  */
	  need_barrier = 1;
	  break;

	default:
	  abort ();
	}
    }
  else
    {
      if (flags.is_branch)
	{
	  /* Branches have several RAW exceptions that allow to avoid
	     barriers.  */

	  if (REGNO_REG_CLASS (regno) == BR_REGS || regno == REG_AR_PFS)
	    /* RAW dependencies on branch regs are permissible as long
	       as the writer is a non-branch instruction.  Since we
	       never generate code that uses a branch register written
	       by a branch instruction, handling this case is
	       easy.  */
	    /* ??? This assumes that we don't emit br.cloop, br.cexit, br.ctop,
	       br.wexit, br.wtop.  This is true currently.  */
	      return 0;

	  if (REGNO_REG_CLASS (regno) == PR_REGS
	      && ! rws_sum[regno].written_by_fp)
	    /* The predicates of a branch are available within the
	       same insn group as long as the predicate was written by
	       something other than a floating-point instruction.   */
	    return 0;
	}

      switch (rws_sum[regno].write_count)
	{
	case 0:
	  /* The register has not been written yet.  */
	  break;

	case 1:
	  /* The register has been written via a predicate.  If this is
	     not a complementary predicate, then we need a barrier.  */
	  /* ??? This assumes that P and P+1 are always complementary
	     predicates for P even.  */
	  if ((rws_sum[regno].first_pred ^ 1) != pred)
	    need_barrier = 1;
	  break;

	case 2:
	  /* The register has been unconditionally written already.  We
	     need a barrier.  */
	  need_barrier = 1;
	  break;

	default:
	  abort ();
	}
    }

  return need_barrier;
}

/* Handle an access to rtx X of type FLAGS using predicate register PRED.
   Return 1 is this access creates a dependency with an earlier instruction
   in the same group.  */

static int
rtx_needs_barrier (x, flags, pred)
     rtx x;
     struct reg_flags flags;
     int pred;
{
  int i, j;
  int is_complemented = 0;
  int need_barrier = 0;
  const char *format_ptr;
  struct reg_flags new_flags;
  rtx src, dst;
  rtx cond = 0;

  if (! x)
    return 0;

  new_flags = flags;

  switch (GET_CODE (x))
    {
    case SET:
      src = SET_SRC (x);
      switch (GET_CODE (src))
	{
	case CALL:
	  /* We don't need to worry about the result registers that
             get written by subroutine call.  */
	  need_barrier = rtx_needs_barrier (src, flags, pred);
	  return need_barrier;

	case IF_THEN_ELSE:
	  if (SET_DEST (x) == pc_rtx)
	    {
	      /* X is a conditional branch.  */
	      /* ??? This seems redundant, as the caller sets this bit for
		 all JUMP_INSNs.  */
	      new_flags.is_branch = 1;
	      need_barrier = rtx_needs_barrier (src, new_flags, pred);
	      return need_barrier;
	    }
	  else
	    {
	      /* X is a conditional move.  */
	      cond = XEXP (src, 0);
	      if (GET_CODE (cond) == EQ)
		is_complemented = 1;
	      cond = XEXP (cond, 0);
	      if (GET_CODE (cond) != REG
		  && REGNO_REG_CLASS (REGNO (cond)) != PR_REGS)
		abort ();

	      if (XEXP (src, 1) == SET_DEST (x)
		  || XEXP (src, 2) == SET_DEST (x))
		{
		  /* X is a conditional move that conditionally writes the
		     destination.  */

		  /* We need another complement in this case.  */
		  if (XEXP (src, 1) == SET_DEST (x))
		    is_complemented = ! is_complemented;

		  pred = REGNO (cond);
		  if (is_complemented)
		    ++pred;
		}

	      /* ??? If this is a conditional write to the dest, then this
		 instruction does not actually read one source.  This probably
		 doesn't matter, because that source is also the dest.  */
	      /* ??? Multiple writes to predicate registers are allowed
		 if they are all AND type compares, or if they are all OR
		 type compares.  We do not generate such instructions
		 currently.  */
	    }
	  /* ... fall through ... */

	default:
	  if (GET_RTX_CLASS (GET_CODE (src)) == '<'
	       && GET_MODE_CLASS (GET_MODE (XEXP (src, 0))) == MODE_FLOAT)
	    /* Set new_flags.is_fp to 1 so that we know we're dealing
	       with a floating point comparison when processing the
	       destination of the SET.  */
	    new_flags.is_fp = 1;
	  break;
	}
      need_barrier = rtx_needs_barrier (src, flags, pred);
      /* This instruction unconditionally uses a predicate register.  */
      if (cond)
	need_barrier |= rws_access_reg (REGNO (cond), flags, 0);

      dst = SET_DEST (x);
      if (GET_CODE (dst) == ZERO_EXTRACT)
	{
	  need_barrier |= rtx_needs_barrier (XEXP (dst, 1), flags, pred);
	  need_barrier |= rtx_needs_barrier (XEXP (dst, 2), flags, pred);
	  dst = XEXP (dst, 0);
	}
      new_flags.is_write = 1;
      need_barrier |= rtx_needs_barrier (dst, new_flags, pred);
      break;

    case CALL:
      new_flags.is_write = 0;
      /* ??? Why is this here?  It seems unnecessary.  */
      need_barrier |= rws_access_reg (REG_GP, new_flags, pred);
      need_barrier |= rws_access_reg (REG_AR_EC, new_flags, pred);

      /* Avoid multiple register writes, in case this is a pattern with
	 multiple CALL rtx.  This avoids an abort in rws_access_reg.  */
      /* ??? This assumes that no rtx other than CALL/RETURN sets REG_AR_CFM,
	 and that we don't have predicated calls/returns.  */
      if (! rws_insn[REG_AR_CFM].write_count)
	{
	  new_flags.is_write = 1;
	  need_barrier |= rws_access_reg (REG_RP, new_flags, pred);
	  need_barrier |= rws_access_reg (REG_AR_PFS, new_flags, pred);
	  need_barrier |= rws_access_reg (REG_AR_CFM, new_flags, pred);
	}
      break;

    case CLOBBER:
#if 0
    case USE:
      /* We must handle USE here in case it occurs within a PARALLEL.
	 For instance, the mov ar.pfs= instruction has a USE which requires
	 a barrier between it and an immediately preceeding alloc.  */
#endif
      /* Clobber & use are for earlier compiler-phases only.  */
      break;

    case ASM_OPERANDS:
    case ASM_INPUT:
      /* We always emit stop bits for traditional asms.  We emit stop bits
	 for volatile extended asms if TARGET_VOL_ASM_STOP is true.  */
      if (GET_CODE (x) != ASM_OPERANDS
	  || (MEM_VOLATILE_P (x) && TARGET_VOL_ASM_STOP))
	{
	  /* Avoid writing the register multiple times if we have multiple
	     asm outputs.  This avoids an abort in rws_access_reg.  */
	  if (! rws_insn[REG_VOLATILE].write_count)
	    {
	      new_flags.is_write = 1;
	      rws_access_reg (REG_VOLATILE, new_flags, pred);
	    }
	  return 1;
	}

      /* For all ASM_OPERANDS, we must traverse the vector of input operands.
	 We can not just fall through here since then we would be confused
	 by the ASM_INPUT rtx inside ASM_OPERANDS, which do not indicate
	 traditional asms unlike their normal usage.  */

      for (i = ASM_OPERANDS_INPUT_LENGTH (x) - 1; i >= 0; --i)
	if (rtx_needs_barrier (ASM_OPERANDS_INPUT (x, i), flags, pred))
	  need_barrier = 1;
      break;

    case PARALLEL:
      for (i = XVECLEN (x, 0) - 1; i >= 0; --i)
	if (rtx_needs_barrier (XVECEXP (x, 0, i), flags, pred))
	  need_barrier = 1;
      break;

    case SUBREG:
      x = SUBREG_REG (x);
      /* FALLTHRU */
    case REG:
      need_barrier = rws_access_reg (REGNO (x), flags, pred);
      break;

    case MEM:
      /* Find the regs used in memory address computation.  */
      new_flags.is_write = 0;
      need_barrier = rtx_needs_barrier (XEXP (x, 0), new_flags, pred);
      break;

    case CONST_INT:   case CONST_DOUBLE:
    case SYMBOL_REF:  case LABEL_REF:     case CONST:
      break;

      /* Operators with side-effects.  */
    case POST_INC:    case POST_DEC:
      if (GET_CODE (XEXP (x, 0)) != REG)
	abort ();

      new_flags.is_write = 0;
      need_barrier  = rws_access_reg (REGNO (XEXP (x, 0)), new_flags, pred);
      new_flags.is_write = 1;
      need_barrier |= rws_access_reg (REGNO (XEXP (x, 0)), new_flags, pred);
      break;

      /* Handle common unary and binary ops for efficiency.  */
    case COMPARE:  case PLUS:    case MINUS:   case MULT:      case DIV:
    case MOD:      case UDIV:    case UMOD:    case AND:       case IOR:
    case XOR:      case ASHIFT:  case ROTATE:  case ASHIFTRT:  case LSHIFTRT:
    case ROTATERT: case SMIN:    case SMAX:    case UMIN:      case UMAX:
    case NE:       case EQ:      case GE:      case GT:        case LE:
    case LT:       case GEU:     case GTU:     case LEU:       case LTU:
      need_barrier = rtx_needs_barrier (XEXP (x, 0), new_flags, pred);
      need_barrier |= rtx_needs_barrier (XEXP (x, 1), new_flags, pred);
      break;

    case NEG:      case NOT:	        case SIGN_EXTEND:     case ZERO_EXTEND:
    case TRUNCATE: case FLOAT_EXTEND:   case FLOAT_TRUNCATE:  case FLOAT:
    case FIX:      case UNSIGNED_FLOAT: case UNSIGNED_FIX:    case ABS:
    case SQRT:     case FFS:
      need_barrier = rtx_needs_barrier (XEXP (x, 0), flags, pred);
      break;

    case UNSPEC:
      switch (XINT (x, 1))
	{
	  /* ??? For the st8.spill/ld8.fill instructions, we can ignore unat
	     dependencies as long as we don't have both a spill and fill in
	     the same instruction group.  We need to check for that.  */
	case 1: /* st8.spill */
	case 2: /* ld8.fill */
	case 3: /* stf.spill */
	case 4: /* ldf.spill */
	case 8: /* popcnt */
	  need_barrier = rtx_needs_barrier (XVECEXP (x, 0, 0), flags, pred);
	  break;

	case 5: /* mov =pr */
	  /* This reads all predicate registers.  */
	  for (i = PR_REG (1); i < PR_REG (64); i++)
	    need_barrier |= rws_access_reg (i, flags, pred);
	  break;

	case 6:
	case 7:
	  abort ();

	  /* ??? Should track unat reads and writes.  */
	case 9: /* mov =ar.unat */
	case 10: /* mov ar.unat= */
	  break;
        case 11: /* mov ar.ccv= */
          break;
        case 12: /* mf */
          break;
        case 13: /* cmpxchg_acq */
          break;
        case 14: /* val_compare_and_swap */
          break;
        case 15: /* lock_release */
          break;
        case 16: /* lock_test_and_set */
          break;
        case 17: /* _and_fetch */
          break;
        case 18: /* fetch_and_ */
          break;
        case 19: /* fetchadd_acq */
          break;
	default:
	  abort ();
	}
      break;

    case UNSPEC_VOLATILE:
      switch (XINT (x, 1))
	{
	case 0: /* alloc */
	  /* Alloc must always be the first instruction.  Currently, we
	     only emit it at the function start, so we don't need to worry
	     about emitting a stop bit before it.  */
	  need_barrier = rws_access_reg (REG_AR_PFS, flags, pred);

	  new_flags.is_write = 1;
	  need_barrier |= rws_access_reg (REG_AR_CFM, new_flags, pred);
	  return need_barrier;

	case 1: /* blockage */
	case 2: /* insn group barrier */
	  return 0;

	case 3: /* flush_cache */
	  return rtx_needs_barrier (XVECEXP (x, 0, 0), flags, pred);

	case 4: /* mov ar.pfs= */
	  new_flags.is_write = 1;
	  need_barrier = rws_access_reg (REG_AR_PFS, new_flags, pred);
	  break;

	case 6: /* mov pr= */
	  /* This writes all predicate registers.  */
	  new_flags.is_write = 1;
	  /* We need to skip by two, because rws_access_reg always writes
	     to two predicate registers at a time.  */
	  /* ??? Strictly speaking, we shouldn't be counting writes to pr0.  */
	  for (i = PR_REG (0); i < PR_REG (64); i += 2)
	    need_barrier |= rws_access_reg (i, new_flags, pred);
	  break;

	default:
	  abort ();
	}
      break;

    case RETURN:
      new_flags.is_write = 0;
      need_barrier  = rws_access_reg (REG_RP, flags, pred);
      need_barrier |= rws_access_reg (REG_AR_PFS, flags, pred);

      new_flags.is_write = 1;
      need_barrier |= rws_access_reg (REG_AR_EC, new_flags, pred);
      need_barrier |= rws_access_reg (REG_AR_CFM, new_flags, pred);
      break;

    default:
      format_ptr = GET_RTX_FORMAT (GET_CODE (x));
      for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
	switch (format_ptr[i])
	  {
	  case '0':	/* unused field */
	  case 'i':	/* integer */
	  case 'n':	/* note */
	  case 'w':	/* wide integer */
	  case 's':	/* pointer to string */
	  case 'S':	/* optional pointer to string */
	    break;

	  case 'e':
	    if (rtx_needs_barrier (XEXP (x, i), flags, pred))
	      need_barrier = 1;
	    break;

	  case 'E':
	    for (j = XVECLEN (x, i) - 1; j >= 0; --j)
	      if (rtx_needs_barrier (XVECEXP (x, i, j), flags, pred))
		need_barrier = 1;
	    break;

	  default:
	    abort ();
	  }
    }
  return need_barrier;
}

/* INSNS is an chain of instructions.  Scan the chain, and insert stop bits
   as necessary to eliminate dependendencies.  */

static void
emit_insn_group_barriers (insns)
     rtx insns;
{
  int need_barrier = 0;
  int exception_nesting;
  struct reg_flags flags;
  rtx insn, prev_insn;

  memset (rws_sum, 0, sizeof (rws_sum));

  prev_insn = 0;
  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      memset (&flags, 0, sizeof (flags));
      switch (GET_CODE (insn))
	{
	case NOTE:
	  switch (NOTE_LINE_NUMBER (insn))
	    {
	    case NOTE_INSN_EH_REGION_BEG:
	      exception_nesting++;
	      break;

	    case NOTE_INSN_EH_REGION_END:
	      exception_nesting--;
	      break;

	    case NOTE_INSN_EPILOGUE_BEG:
	      break;

	    default:
	      break;
	    }
	  break;

	case JUMP_INSN:
	case CALL_INSN:
	  flags.is_branch = 1;
	case INSN:
	  if (GET_CODE (PATTERN (insn)) == USE)
	    /* Don't care about USE "insns"---those are used to
	       indicate to the optimizer that it shouldn't get rid of
	       certain operations.  */
	    break;
	  else
	    {
	      memset (rws_insn, 0, sizeof (rws_insn));
	      need_barrier = rtx_needs_barrier (PATTERN (insn), flags, 0);

	      /* Check to see if the previous instruction was a volatile
		 asm.  */
	      if (! need_barrier)
		need_barrier = rws_access_reg (REG_VOLATILE, flags, 0);

	      if (need_barrier)
		{
		  /* PREV_INSN null can happen if the very first insn is a
		     volatile asm.  */
		  if (prev_insn)
		    emit_insn_after (gen_insn_group_barrier (), prev_insn);
		  memcpy (rws_sum, rws_insn, sizeof (rws_sum));
		}
	      need_barrier = 0;
	      prev_insn = insn;
	    }
	  break;

	case BARRIER:
	  /* A barrier doesn't imply an instruction group boundary.  */
	  break;

	case CODE_LABEL:
	  /* Leave prev_insn alone so the barrier gets generated in front
	     of the label, if one is needed.  */
	  break;

	default:
	  abort ();
	}
    }
}

/* Perform machine dependent operations on the rtl chain INSNS.  */

void
ia64_reorg (insns)
     rtx insns;
{
  emit_insn_group_barriers (insns);
}

/* Return true if REGNO is used by the epilogue.  */

int
ia64_epilogue_uses (regno)
     int regno;
{
  /* For functions defined with the syscall_linkage attribute, all input
     registers are marked as live at all function exits.  This prevents the
     register allocator from using the input registers, which in turn makes it
     possible to restart a system call after an interrupt without having to
     save/restore the input registers.  */

  if (IN_REGNO_P (regno)
      && (regno < IN_REG (current_function_args_info.words))
      && lookup_attribute ("syscall_linkage",
			   TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))))
    return 1;

  return 0;
}

/* Return true if IDENTIFIER is a valid attribute for TYPE.  */

int
ia64_valid_type_attribute (type, attributes, identifier, args)
     tree type;
     tree attributes ATTRIBUTE_UNUSED;
     tree identifier;
     tree args;
{
  /* We only support an attribute for function calls.  */

  if (TREE_CODE (type) != FUNCTION_TYPE
      && TREE_CODE (type) != METHOD_TYPE)
    return 0;

  /* The "syscall_linkage" attribute says the callee is a system call entry
     point.  This affects ia64_epilogue_uses.  */

  if (is_attribute_p ("syscall_linkage", identifier))
    return args == NULL_TREE;

  return 0;
}

/* For ia64, SYMBOL_REF_FLAG set means that it is a function.

   We add @ to the name if this goes in small data/bss.  We can only put
   a variable in small data/bss if it is defined in this module or a module
   that we are statically linked with.  We can't check the second condition,
   but TREE_STATIC gives us the first one.  */

/* ??? If we had IPA, we could check the second condition.  We could support
   programmer added section attributes if the variable is not defined in this
   module.  */

/* ??? See the v850 port for a cleaner way to do this.  */

/* ??? We could also support own long data here.  Generating movl/add/ld8
   instead of addl,ld8/ld8.  This makes the code bigger, but should make the
   code faster because there is one less load.  This also includes incomplete
   types which can't go in sdata/sbss.  */

/* ??? See select_section.  We must put short own readonly variables in
   sdata/sbss instead of the more natural rodata, because we can't perform
   the DECL_READONLY_SECTION test here.  */

extern struct obstack * saveable_obstack;

void
ia64_encode_section_info (decl)
     tree decl;
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1;
  /* We assume that -fpic is used only to create a shared library (dso).
     With -fpic, no global data can ever be sdata.
     Without -fpic, global common uninitialized data can never be sdata, since
     it can unify with a real definition in a dso.  */
  /* ??? Actually, we can put globals in sdata, as long as we don't use gprel
     to access them.  The linker may then be able to do linker relaxation to
     optimize references to them.  Currently sdata implies use of gprel.  */
  else if (! TARGET_NO_SDATA
	   && TREE_CODE (decl) == VAR_DECL
	   && TREE_STATIC (decl)
	   && ! (TREE_PUBLIC (decl)
		 && (flag_pic
		     || (DECL_COMMON (decl)
			 && (DECL_INITIAL (decl) == 0
			     || DECL_INITIAL (decl) == error_mark_node))))
	   /* Either the variable must be declared without a section attribute,
	      or the section must be sdata or sbss.  */
	   && (DECL_SECTION_NAME (decl) == 0
	       || ! strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
			    ".sdata")
	       || ! strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
			    ".sbss")))
    {
      int size = int_size_in_bytes (TREE_TYPE (decl));
      char *str = XSTR (XEXP (DECL_RTL (decl), 0), 0);
      int reloc;

      /* ??? We should redeclare CTOR_LIST, DTOR_END so that we don't have to
	 special case them here.  Currently we put them in ctor/dtors sections
	 behind the compiler's back.  We should use section attributes
	 instead.  */
      if (! strcmp (str, "__CTOR_LIST__")
	  || ! strcmp (str, "__DTOR_END__"))
	;

      /* If this is an incomplete type with size 0, then we can't put it in
	 sdata because it might be too big when completed.  */
      else if (size > 0 && size <= ia64_section_threshold)
	{
	  int len = strlen (str);
	  char *newstr = obstack_alloc (saveable_obstack, len + 2);

	  strcpy (newstr + 1, str);
	  *newstr = SDATA_NAME_FLAG_CHAR;
	  XSTR (XEXP (DECL_RTL (decl), 0), 0) = newstr;
	}
    }
}

#define def_builtin(name, type, code) \
  builtin_function ((name), (type), (code), BUILT_IN_MD, NULL_PTR)

struct builtin_description
{
  enum insn_code icode;
  const char *name;
  enum ia64_builtins code;
  enum rtx_code comparison;
  unsigned int flag;
};

/* All 32 bit intrinsics that take 2 arguments. */
static struct builtin_description bdesc_2argsi[] =
{
  { CODE_FOR_fetch_and_add_si, "__sync_fetch_and_add_si", IA64_BUILTIN_FETCH_AND_ADD_SI, 0, 0 },
  { CODE_FOR_fetch_and_sub_si, "__sync_fetch_and_sub_si", IA64_BUILTIN_FETCH_AND_SUB_SI, 0, 0 },
  { CODE_FOR_fetch_and_or_si, "__sync_fetch_and_or_si", IA64_BUILTIN_FETCH_AND_OR_SI, 0, 0 },
  { CODE_FOR_fetch_and_and_si, "__sync_fetch_and_and_si", IA64_BUILTIN_FETCH_AND_AND_SI, 0, 0 },
  { CODE_FOR_fetch_and_xor_si, "__sync_fetch_and_xor_si", IA64_BUILTIN_FETCH_AND_XOR_SI, 0, 0 },
  { CODE_FOR_fetch_and_nand_si, "__sync_fetch_and_nand_si", IA64_BUILTIN_FETCH_AND_NAND_SI, 0, 0 },
  { CODE_FOR_add_and_fetch_si, "__sync_add_and_fetch_si", IA64_BUILTIN_ADD_AND_FETCH_SI, 0, 0 },
  { CODE_FOR_sub_and_fetch_si, "__sync_sub_and_fetch_si", IA64_BUILTIN_SUB_AND_FETCH_SI, 0, 0 },
  { CODE_FOR_or_and_fetch_si, "__sync_or_and_fetch_si", IA64_BUILTIN_OR_AND_FETCH_SI, 0, 0 },
  { CODE_FOR_and_and_fetch_si, "__sync_and_and_fetch_si", IA64_BUILTIN_AND_AND_FETCH_SI, 0, 0 },
  { CODE_FOR_xor_and_fetch_si, "__sync_xor_and_fetch_si", IA64_BUILTIN_XOR_AND_FETCH_SI, 0, 0 },
  { CODE_FOR_nand_and_fetch_si, "__sync_nand_and_fetch_si", IA64_BUILTIN_NAND_AND_FETCH_SI, 0, 0 }
};

/* All 64 bit intrinsics that take 2 arguments. */
static struct builtin_description bdesc_2argdi[] =
{
  { CODE_FOR_fetch_and_add_di, "__sync_fetch_and_add_di", IA64_BUILTIN_FETCH_AND_ADD_DI, 0, 0 },
  { CODE_FOR_fetch_and_sub_di, "__sync_fetch_and_sub_di", IA64_BUILTIN_FETCH_AND_SUB_DI, 0, 0 },
  { CODE_FOR_fetch_and_or_di, "__sync_fetch_and_or_di", IA64_BUILTIN_FETCH_AND_OR_DI, 0, 0 },
  { CODE_FOR_fetch_and_and_di, "__sync_fetch_and_and_di", IA64_BUILTIN_FETCH_AND_AND_DI, 0, 0 },
  { CODE_FOR_fetch_and_xor_di, "__sync_fetch_and_xor_di", IA64_BUILTIN_FETCH_AND_XOR_DI, 0, 0 },
  { CODE_FOR_fetch_and_nand_di, "__sync_fetch_and_nand_di", IA64_BUILTIN_FETCH_AND_NAND_DI, 0, 0 },
  { CODE_FOR_add_and_fetch_di, "__sync_add_and_fetch_di", IA64_BUILTIN_ADD_AND_FETCH_DI, 0, 0 },
  { CODE_FOR_sub_and_fetch_di, "__sync_sub_and_fetch_di", IA64_BUILTIN_SUB_AND_FETCH_DI, 0, 0 },
  { CODE_FOR_or_and_fetch_di, "__sync_or_and_fetch_di", IA64_BUILTIN_OR_AND_FETCH_DI, 0, 0 },
  { CODE_FOR_and_and_fetch_di, "__sync_and_and_fetch_di", IA64_BUILTIN_AND_AND_FETCH_DI, 0, 0 },
  { CODE_FOR_xor_and_fetch_di, "__sync_xor_and_fetch_di", IA64_BUILTIN_XOR_AND_FETCH_DI, 0, 0 },
  { CODE_FOR_nand_and_fetch_di, "__sync_nand_and_fetch_di", IA64_BUILTIN_NAND_AND_FETCH_DI, 0, 0 }
};

void
ia64_init_builtins ()
{
  int i;
  struct builtin_description *d;

  tree psi_type_node = build_pointer_type (integer_type_node);
  tree pdi_type_node = build_pointer_type (long_integer_type_node);
  tree endlink = tree_cons (NULL_TREE, void_type_node, NULL_TREE);


  /* __sync_val_compare_and_swap_si, __sync_bool_compare_and_swap_si */
  tree si_ftype_psi_si_si
    = build_function_type (integer_type_node,
                           tree_cons (NULL_TREE, psi_type_node,
                                      tree_cons (NULL_TREE, integer_type_node,
                                                 tree_cons (NULL_TREE, integer_type_node,
                                                            endlink))));

  /* __sync_val_compare_and_swap_di, __sync_bool_compare_and_swap_di */
  tree di_ftype_pdi_di_di
    = build_function_type (long_integer_type_node,
                           tree_cons (NULL_TREE, pdi_type_node,
                                      tree_cons (NULL_TREE, long_integer_type_node,
                                                 tree_cons (NULL_TREE, long_integer_type_node,
                                                            endlink))));
  /* __sync_synchronize */
  tree void_ftype_void
    = build_function_type (void_type_node, endlink);

  /* __sync_lock_test_and_set_si */
  tree si_ftype_psi_si
    = build_function_type (integer_type_node,
                           tree_cons (NULL_TREE, psi_type_node,
                           tree_cons (NULL_TREE, integer_type_node, endlink)));

  /* __sync_lock_test_and_set_di */
  tree di_ftype_pdi_di
    = build_function_type (long_integer_type_node,  
                           tree_cons (NULL_TREE, pdi_type_node,
                           tree_cons (NULL_TREE, long_integer_type_node, endlink)));

  /* __sync_lock_release_si */
  tree void_ftype_psi
    = build_function_type (void_type_node, tree_cons (NULL_TREE, psi_type_node, endlink));

  /* __sync_lock_release_di */
  tree void_ftype_pdi
    = build_function_type (void_type_node, tree_cons (NULL_TREE, pdi_type_node, endlink));

  def_builtin ("__sync_val_compare_and_swap_si", si_ftype_psi_si_si, IA64_BUILTIN_VAL_COMPARE_AND_SWAP_SI);

  def_builtin ("__sync_val_compare_and_swap_di", di_ftype_pdi_di_di, IA64_BUILTIN_VAL_COMPARE_AND_SWAP_DI);

  def_builtin ("__sync_bool_compare_and_swap_si", si_ftype_psi_si_si, IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_SI);

  def_builtin ("__sync_bool_compare_and_swap_di", di_ftype_pdi_di_di, IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_DI);

  def_builtin ("__sync_synchronize", void_ftype_void, IA64_BUILTIN_SYNCHRONIZE);

  def_builtin ("__sync_lock_test_and_set_si", si_ftype_psi_si, IA64_BUILTIN_LOCK_TEST_AND_SET_SI);

  def_builtin ("__sync_lock_test_and_set_di", di_ftype_pdi_di, IA64_BUILTIN_LOCK_TEST_AND_SET_DI);

  def_builtin ("__sync_lock_release_si", void_ftype_psi, IA64_BUILTIN_LOCK_RELEASE_SI);

  def_builtin ("__sync_lock_release_di", void_ftype_pdi, IA64_BUILTIN_LOCK_RELEASE_DI);

  /* Add all builtins that are operations on two args. */
  for (i=0, d = bdesc_2argsi; i < sizeof(bdesc_2argsi) / sizeof *d; i++, d++)
    def_builtin (d->name, si_ftype_psi_si, d->code);
  for (i=0, d = bdesc_2argdi; i < sizeof(bdesc_2argdi) / sizeof *d; i++, d++)
    def_builtin (d->name, di_ftype_pdi_di, d->code);
}

/* Expand fetch_and_op intrinsics.  The basic code sequence is:

     mf
     ldsz return = [ptr];
     tmp = return;
     do {
       oldval = tmp;
       ar.ccv = tmp;
       tmp <op>= value;
       cmpxchgsz.acq tmp = [ptr], tmp
       cmpxchgsz.acq tmp = [ptr], tmp
     } while (tmp != oldval)
*/
void
ia64_expand_fetch_and_op (code, mode, operands)
     enum fetchop_code code;
     enum machine_mode mode;
     rtx operands[];
{
  rtx oldval, newlabel;
  rtx tmp_reg = gen_rtx_REG (mode, GR_REG(0));
  rtx mfreg = gen_rtx_MEM (BLKmode, tmp_reg);
  RTX_UNCHANGING_P (mfreg) = 1;
  emit_insn (gen_mf (mfreg));
  tmp_reg = gen_reg_rtx (mode);
  oldval = gen_reg_rtx (mode);

  if (mode == SImode)
    {
      emit_insn (gen_movsi (operands[0], operands[1]));
      emit_insn (gen_movsi (tmp_reg, operands[0]));
    }
  else
    {
      emit_insn (gen_movdi (operands[0], operands[1]));
      emit_insn (gen_movdi (tmp_reg, operands[0]));
    }

  newlabel = gen_label_rtx ();
  emit_label (newlabel);
  if (mode == SImode)
    {
      emit_insn (gen_movsi (oldval, tmp_reg));
      emit_insn (gen_ccv_restore_si (tmp_reg));
    }
  else
    {
      emit_insn (gen_movdi (oldval, tmp_reg));
      emit_insn (gen_ccv_restore_di (tmp_reg));
    }

  /* Perform the specific operation. */
  switch (code)
  {
  case IA64_ADD_OP:
    {
      rtx reg;
      if (GET_CODE (operands[2]) == CONST_INT)
	reg = gen_reg_rtx (mode);
      else 
        reg = operands[2];
      if (mode == SImode)
	{
	  if (reg != operands[2])
	    emit_insn (gen_movsi (reg, operands[2]));
	  emit_insn (gen_addsi3 (tmp_reg, tmp_reg, reg));
	}
      else
        {
	  if (reg != operands[2])
	    emit_insn (gen_movdi (reg, operands[2]));
	  emit_insn (gen_adddi3 (tmp_reg, tmp_reg, reg));
	}
      break;
    }

  case IA64_SUB_OP:
    if (mode == SImode)
      emit_insn (gen_subsi3 (tmp_reg, tmp_reg, operands[2]));
    else
      emit_insn (gen_subdi3 (tmp_reg, tmp_reg, operands[2]));
    break;

  case IA64_OR_OP:
    emit_insn (gen_iordi3 (tmp_reg, tmp_reg, operands[2]));
    break;

  case IA64_AND_OP:
    emit_insn (gen_anddi3 (tmp_reg, tmp_reg, operands[2]));
    break;

  case IA64_XOR_OP:
    emit_insn (gen_xordi3 (tmp_reg, tmp_reg, operands[2]));
    break;

  case IA64_NAND_OP:
    emit_insn (gen_anddi3 (tmp_reg, tmp_reg, operands[2]));
    if (mode == SImode)
      emit_insn (gen_one_cmplsi2 (tmp_reg, operands[0]));
    else
      emit_insn (gen_one_cmpldi2 (tmp_reg, operands[0]));
    break;

  default:
    break;
  }
    
  if (mode == SImode) 
    emit_insn (gen_cmpxchg_acq_si (tmp_reg, operands[1], tmp_reg));
  else
    emit_insn (gen_cmpxchg_acq_di (tmp_reg, operands[1], tmp_reg));

  emit_cmp_and_jump_insns (tmp_reg, oldval, NE, 0, mode, 1, 0, newlabel);
}

/* Expand op_and_fetch intrinsics.  The basic code sequence is:

     mf
     ldsz return = [ptr];
     do {
       oldval = tmp;
       ar.ccv = tmp;
       return = tmp + value;
       cmpxchgsz.acq tmp = [ptr], return
     } while (tmp != oldval)
*/
void
ia64_expand_op_and_fetch (code, mode, operands)
     enum fetchop_code code;
     enum machine_mode mode;
     rtx operands[];
{
  rtx oldval, newlabel;
  rtx tmp_reg, tmp2_reg = gen_rtx_REG (mode, GR_REG(0));
  rtx mfreg = gen_rtx_MEM (BLKmode, tmp2_reg);
  RTX_UNCHANGING_P (mfreg) = 1;

  emit_insn (gen_mf (mfreg));
  tmp_reg = gen_reg_rtx (mode);
  if (mode == SImode)
    emit_insn (gen_movsi (tmp_reg, operands[1]));
  else
    emit_insn (gen_movdi (tmp_reg, operands[1]));

  newlabel = gen_label_rtx ();
  emit_label (newlabel);
  oldval = gen_reg_rtx (mode);
  if (mode == SImode)
    {
      emit_insn (gen_movsi (oldval, tmp_reg));
      emit_insn (gen_ccv_restore_si (tmp_reg));
    }
  else
    {
      emit_insn (gen_movdi (oldval, tmp_reg));
      emit_insn (gen_ccv_restore_di (tmp_reg));
    }

  /* Perform the specific operation. */
  switch (code)
  {
  case IA64_ADD_OP:
    if (mode == SImode)
      emit_insn (gen_addsi3 (operands[0], tmp_reg, operands[2]));
    else
      emit_insn (gen_adddi3 (operands[0], tmp_reg, operands[2]));
    break;

  case IA64_SUB_OP:
    if (mode == SImode)
      emit_insn (gen_subsi3 (operands[0], tmp_reg, operands[2]));
    else
      emit_insn (gen_subdi3 (operands[0], tmp_reg, operands[2]));
    break;

  case IA64_OR_OP:
    emit_insn (gen_iordi3 (operands[0], tmp_reg, operands[2]));
    break;

  case IA64_AND_OP:
    emit_insn (gen_anddi3 (operands[0], tmp_reg, operands[2]));
    break;

  case IA64_XOR_OP:
    emit_insn (gen_xordi3 (operands[0], tmp_reg, operands[2]));
    break;

  case IA64_NAND_OP:
    emit_insn (gen_anddi3 (operands[0], tmp_reg, operands[2]));
    if (mode == SImode)
      emit_insn (gen_one_cmplsi2 (operands[0], operands[0]));
    else
      emit_insn (gen_one_cmpldi2 (operands[0], operands[0]));
    break;

  default:
    break;
  }
    
  if (mode == SImode) 
    emit_insn (gen_cmpxchg_acq_si (tmp_reg, operands[1], operands[0]));
  else
    emit_insn (gen_cmpxchg_acq_di (tmp_reg, operands[1], operands[0]));

  emit_cmp_and_jump_insns (tmp_reg, oldval, NE, 0, mode, 1, 0, newlabel);
}

/* Expand val_ and bool_compare_and_swap.  For val_ we want:

     ar.ccv = oldval
     mf
     cmpxchgsz.acq ret = [ptr], newval, ar.ccv
     return ret

   For bool_ it's the same except return ret == oldval.
*/
static rtx
ia64_expand_compare_and_swap (icode, arglist, target, boolcode)
     enum insn_code icode;
     tree arglist;
     rtx target;
     int boolcode;
{
  tree arg0, arg1, arg2;
  rtx newlabel, newlabel2, op0, op1, op2, pat;
  enum machine_mode tmode, mode0, mode1, mode2;
 
  arg0 = TREE_VALUE (arglist);
  arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
  op2 = expand_expr (arg2, NULL_RTX, VOIDmode, 0);
  tmode = insn_data[icode].operand[0].mode;
  mode0 = insn_data[icode].operand[1].mode;
  mode1 = insn_data[icode].operand[2].mode;
  mode2 = insn_data[icode].operand[3].mode;

  op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
  RTX_UNCHANGING_P (op0) = 1;
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);
  if (! (*insn_data[icode].operand[3].predicate) (op2, mode2))
    op2 = copy_to_mode_reg (mode2, op2);
  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  pat = GEN_FCN (icode) (target, op0, op1, op2);
  if (! pat)
    return 0;
  emit_insn (pat);
  if (boolcode)
    {
      if (tmode == SImode)
        {
          emit_insn (gen_cmpsi (target, op1));
          emit_insn (gen_seq (gen_lowpart (DImode, target)));
        }
      else
        {
          emit_insn (gen_cmpdi (target, op1));
          emit_insn (gen_seq (target));
        }
    }
  return target;
}

/* Expand all intrinsics that take 2 arguments. */
static rtx
ia64_expand_binop_builtin (icode, arglist, target)
     enum insn_code icode;
     tree arglist;
     rtx target;
{
  rtx pat;
  tree arg0 = TREE_VALUE (arglist);
  tree arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

rtx
ia64_expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget;
     enum machine_mode mode;
     int ignore;
{
  rtx op0, op1, op2, op3, pat;
  rtx tmp_reg;
  rtx newlabel, newlabel2;
  tree arg0, arg1, arg2, arg3;
  tree arglist = TREE_OPERAND (exp, 1);
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  int fcode = DECL_FUNCTION_CODE (fndecl);
  enum machine_mode tmode, mode0, mode1, mode2, mode3;
  enum insn_code icode;
  int boolcode = 0;
  int i;
  struct builtin_description *d;

  switch (fcode)
    {
    case IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_SI:
      return ia64_expand_compare_and_swap (CODE_FOR_val_compare_and_swap_si, arglist, target, 1);
    case IA64_BUILTIN_VAL_COMPARE_AND_SWAP_SI:
      return ia64_expand_compare_and_swap (CODE_FOR_val_compare_and_swap_si, arglist, target, 0);
    case IA64_BUILTIN_BOOL_COMPARE_AND_SWAP_DI:
      return ia64_expand_compare_and_swap (CODE_FOR_val_compare_and_swap_di, arglist, target, 1);
    case IA64_BUILTIN_VAL_COMPARE_AND_SWAP_DI:
      return ia64_expand_compare_and_swap (CODE_FOR_val_compare_and_swap_di, arglist, target, 0);
    case IA64_BUILTIN_SYNCHRONIZE:
      /* Pass a volatile memory operand. */
      tmp_reg = gen_rtx_REG (DImode, GR_REG(0));
      target = gen_rtx_MEM (BLKmode, tmp_reg);
      emit_insn (gen_mf (target));
      return 0;

    case IA64_BUILTIN_LOCK_TEST_AND_SET_SI:
      icode = CODE_FOR_lock_test_and_set_si;
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;
      op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
      RTX_UNCHANGING_P (op0) = 1;
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
        op1 = copy_to_mode_reg (mode1, op1);
      if (target == 0
          || GET_MODE (target) != tmode
          || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
        target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
        return 0;
      emit_insn (pat);
      return target;

    case IA64_BUILTIN_LOCK_TEST_AND_SET_DI:
      icode = CODE_FOR_lock_test_and_set_di;
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;
      op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
      RTX_UNCHANGING_P (op0) = 1; 
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
        op1 = copy_to_mode_reg (mode1, op1);
      if (target == 0
          || GET_MODE (target) != tmode
          || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
        target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
        return 0;
      emit_insn (pat);
      return target;

    case IA64_BUILTIN_LOCK_RELEASE_SI:
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op0 = gen_rtx_MEM (SImode, copy_to_mode_reg (Pmode, op0));
      MEM_VOLATILE_P (op0) = 1;
      emit_insn (gen_movsi (op0, GEN_INT(0)));
      return 0;

    case IA64_BUILTIN_LOCK_RELEASE_DI:
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op0 = gen_rtx_MEM (DImode, copy_to_mode_reg (Pmode, op0));
      MEM_VOLATILE_P (op0) = 1;
      emit_insn (gen_movdi (op0, GEN_INT(0)));
      return 0;

    default:
      break;
    }

  /* Expand all 32 bit intrinsics that take 2 arguments. */
  for (i=0, d = bdesc_2argsi; i < sizeof (bdesc_2argsi) / sizeof *d; i++, d++)
    if (d->code == fcode)
      return ia64_expand_binop_builtin (d->icode, arglist, target);

  /* Expand all 64 bit intrinsics that take 2 arguments. */
  for (i=0, d = bdesc_2argdi; i < sizeof (bdesc_2argdi) / sizeof *d; i++, d++)
    if (d->code == fcode)
      return ia64_expand_binop_builtin (d->icode, arglist, target);

  fail:
    return 0;
}
