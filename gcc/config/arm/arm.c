/* Output routines for GCC for ARM/RISCiX.
   Copyright (C) 1991, 1993, 1994 Free Software Foundation, Inc.
   Contributed by Pieter `Tiggr' Schoenmakers (rcpieter@win.tue.nl)
   	      and Martin Simmons (@harleqn.co.uk).
   More major hacks by Richard Earnshaw (rwe11@cl.cam.ac.uk)

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
#include <string.h>
#include "assert.h"
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
#include "reload.h"
#include "tree.h"
#include "expr.h"

/* The maximum number of insns skipped which will be conditionalised if
   possible.  */
#define MAX_INSNS_SKIPPED  5

/* Some function declarations.  */
extern FILE *asm_out_file;
extern char *output_multi_immediate ();
extern void arm_increase_location ();

HOST_WIDE_INT int_log2 PROTO ((HOST_WIDE_INT));
static int get_prologue_size PROTO ((void));

/*  Define the information needed to generate branch insns.  This is
   stored from the compare operation. */

rtx arm_compare_op0, arm_compare_op1;
int arm_compare_fp;

/* What type of cpu are we compiling for? */
enum processor_type arm_cpu;

/* Waht type of floating point are we compiling for? */
enum floating_point_type arm_fpu;

/* In case of a PRE_INC, POST_INC, PRE_DEC, POST_DEC memory reference, we
   must report the mode of the memory reference from PRINT_OPERAND to
   PRINT_OPERAND_ADDRESS.  */
enum machine_mode output_memory_reference_mode;

/* Nonzero if the prologue must setup `fp'.  */
int current_function_anonymous_args;

/* Location counter of .text segment.  */
int arm_text_location = 0;

/* Set to one if we think that lr is only saved because of subroutine calls,
   but all of these can be `put after' return insns */
int lr_save_eliminated;

/* A hash table is used to store text segment labels and their associated
   offset from the start of the text segment.  */
struct label_offset
{
  char *name;
  int offset;
  struct label_offset *cdr;
};

#define LABEL_HASH_SIZE  257

static struct label_offset *offset_table[LABEL_HASH_SIZE];

/* Set to 1 when a return insn is output, this means that the epilogue
   is not needed. */

static int return_used_this_function;

/* For an explanation of these variables, see final_prescan_insn below.  */
int arm_ccfsm_state;
int arm_current_cc;
rtx arm_target_insn;
int arm_target_label;

/* The condition codes of the ARM, and the inverse function.  */
char *arm_condition_codes[] =
{
  "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
};

#define ARM_INVERSE_CONDITION_CODE(X)  ((X) ^ 1)

/* Return 1 if it is possible to return using a single instruction */

int
use_return_insn ()
{
  int regno;

  if (!reload_completed ||current_function_pretend_args_size
      || current_function_anonymous_args
      || (get_frame_size () && !(TARGET_APCS || frame_pointer_needed)))
    return 0;

  /* Can't be done if any of the FPU regs are pushed, since this also
     requires an insn */
  for (regno = 20; regno < 24; regno++)
    if (regs_ever_live[regno])
      return 0;

  return 1;
}

/* Return TRUE if int I is a valid immediate ARM constant.  */

int
const_ok_for_arm (i)
     HOST_WIDE_INT i;
{
  unsigned HOST_WIDE_INT mask = ~0xFF;

  /* Fast return for 0 and powers of 2 */
  if ((i & (i - 1)) == 0)
    return TRUE;

  do
    {
      if ((i & mask & (unsigned HOST_WIDE_INT) 0xffffffff) == 0)
        return TRUE;
      mask =
	  (mask << 2) | ((mask & (unsigned HOST_WIDE_INT) 0xffffffff)
			 >> (32 - 2)) | ~((unsigned HOST_WIDE_INT) 0xffffffff);
    } while (mask != ~0xFF);

  return FALSE;
}

/* Return true if I is a valid constant for the operation CODE. */
int
const_ok_for_op (i, code, mode)
     HOST_WIDE_INT i;
     enum rtx_code code;
     enum machine_mode mode;
{
  if (const_ok_for_arm (i))
    return 1;

  switch (code)
    {
    case PLUS:
      return const_ok_for_arm (ARM_SIGN_EXTEND (-i));

    case MINUS:		/* Should only occur with (MINUS I reg) => rsb */
    case XOR:
    case IOR:
      return 0;

    case AND:
      return const_ok_for_arm (ARM_SIGN_EXTEND (~i));

    default:
      abort ();
    }
}

/* Emit a sequence of insns to handle a large constant.
   CODE is the code of the operation required, it can be any of SET, PLUS,
   IOR, AND, XOR, MINUS;
   MODE is the mode in which the operation is being performed;
   VAL is the integer to operate on;
   SOURCE is the other operand (a register, or a null-pointer for SET);
   SUBTARGETS means it is safe to create scratch registers if that will
   either produce a simpler sequence, or we will want to cse the values. */

int
arm_split_constant (code, mode, val, target, source, subtargets)
     enum rtx_code code;
     enum machine_mode mode;
     HOST_WIDE_INT val;
     rtx target;
     rtx source;
     int subtargets;
{
  int can_add = 0;
  int can_invert = 0;
  int can_negate = 0;
  int can_negate_initial = 0;
  int can_shift = 0;
  int i;
  int num_bits_set = 0;
  int set_sign_bit_copies = 0;
  int clear_sign_bit_copies = 0;
  int clear_zero_bit_copies = 0;
  int set_zero_bit_copies = 0;
  int insns = 0;
  rtx new_src;
  unsigned HOST_WIDE_INT temp1, temp2;
  unsigned HOST_WIDE_INT remainder = val & 0xffffffff;

  /* find out which operations are safe for a given CODE.  Also do a quick
     check for degenerate cases; these can occur when DImode operations
     are split.  */
  switch (code)
    {
    case SET:
      can_invert = 1;
      can_shift = 1;
      can_negate = 1;
      break;

    case PLUS:
      can_negate = 1;
      can_negate_initial = 1;
      break;

    case IOR:
      if (remainder == 0xffffffff)
	{
	  emit_insn (gen_rtx (SET, VOIDmode, target,
			      GEN_INT (ARM_SIGN_EXTEND (val))));
	  return 1;
	}
      if (remainder == 0)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  emit_insn (gen_rtx (SET, VOIDmode, target, source));
	  return 1;
	}
      break;

    case AND:
      if (remainder == 0)
	{
	  emit_insn (gen_rtx (SET, VOIDmode, target, const0_rtx));
	  return 1;
	}
      if (remainder == 0xffffffff)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  emit_insn (gen_rtx (SET, VOIDmode, target, source));
	  return 1;
	}
      can_invert = 1;
      break;

    case XOR:
      if (remainder == 0)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  emit_insn (gen_rtx (SET, VOIDmode, target, source));
	  return 1;
	}
      if (remainder == 0xffffffff)
	{
	  emit_insn (gen_rtx (SET, VOIDmode, target,
			      gen_rtx (NOT, mode, source)));
	  return 1;
	}

      /* We don't know how to handle this yet below.  */
      abort ();

    case MINUS:
      /* We treat MINUS as (val - source), since (source - val) is always
	 passed as (source + (-val)).  */
      if (remainder == 0)
	{
	  emit_insn (gen_rtx (SET, VOIDmode, target,
			      gen_rtx (NEG, mode, source)));
	  return 1;
	}
      if (const_ok_for_arm (val))
	{
	  emit_insn (gen_rtx (SET, VOIDmode, target, 
			      gen_rtx (MINUS, mode, GEN_INT (val), source)));
	  return 1;
	}
      can_negate = 1;

      break;

    default:
      abort ();
    }

  /* If we can do it in one insn get out quickly */
  if (const_ok_for_arm (val)
      || (can_negate_initial && const_ok_for_arm (-val))
      || (can_invert && const_ok_for_arm (~val)))
    {
      emit_insn (gen_rtx (SET, VOIDmode, target,
			  (source ? gen_rtx (code, mode, source,
					     GEN_INT (val)) : GEN_INT (val))));
      return 1;
    }


  /* Calculate a few attributes that may be useful for specific
     optimizations. */

  for (i = 31; i >= 0; i--)
    {
      if ((remainder & (1 << i)) == 0)
	clear_sign_bit_copies++;
      else
	break;
    }

  for (i = 31; i >= 0; i--)
    {
      if ((remainder & (1 << i)) != 0)
	set_sign_bit_copies++;
      else
	break;
    }

  for (i = 0; i <= 31; i++)
    {
      if ((remainder & (1 << i)) == 0)
	clear_zero_bit_copies++;
      else
	break;
    }

  for (i = 0; i <= 31; i++)
    {
      if ((remainder & (1 << i)) != 0)
	set_zero_bit_copies++;
      else
	break;
    }

  switch (code)
    {
    case SET:
      /* See if we can do this by sign_extending a constant that is known
	 to be negative.  This is a good, way of doing it, since the shift
	 may well merge into a subsequent insn.  */
      if (set_sign_bit_copies > 1)
	{
	  if (const_ok_for_arm
	      (temp1 = ARM_SIGN_EXTEND (remainder 
					<< (set_sign_bit_copies - 1))))
	    {
	      new_src = subtargets ? gen_reg_rtx (mode) : target;
	      emit_insn (gen_rtx (SET, VOIDmode, new_src, GEN_INT (temp1)));
	      emit_insn (gen_ashrsi3 (target, new_src, 
				      GEN_INT (set_sign_bit_copies - 1)));
	      return 2;
	    }
	  /* For an inverted constant, we will need to set the low bits,
	     these will be shifted out of harm's way.  */
	  temp1 |= (1 << (set_sign_bit_copies - 1)) - 1;
	  if (const_ok_for_arm (~temp1))
	    {
	      new_src = subtargets ? gen_reg_rtx (mode) : target;
	      emit_insn (gen_rtx (SET, VOIDmode, new_src, GEN_INT (temp1)));
	      emit_insn (gen_ashrsi3 (target, new_src, 
				      GEN_INT (set_sign_bit_copies - 1)));
	      return 2;
	    }
	}

      /* See if we can generate this by setting the bottom (or the top)
	 16 bits, and then shifting these into the other half of the
	 word.  We only look for the simplest cases, to do more would cost
	 too much.  Be careful, however, not to generate this when the
	 alternative would take fewer insns.  */
      if (val & 0xffff0000)
	{
	  temp1 = remainder & 0xffff0000;
	  temp2 = remainder & 0x0000ffff;

	  /* Overlaps outside this range are best done using other methods. */
	  for (i = 9; i < 24; i++)
	    {
	      if ((((temp2 | (temp2 << i)) & 0xffffffff) == remainder)
		  && ! const_ok_for_arm (temp2))
		{
		  insns
		    = arm_split_constant (code, mode, temp2,
					  (new_src
					   = subtargets ? gen_reg_rtx (mode)
					   : target),
					  source, subtargets);
		  source = new_src;
		  emit_insn (gen_rtx (SET, VOIDmode, target,
				      gen_rtx (IOR, mode,
					       gen_rtx (ASHIFT, mode, source,
							GEN_INT (i)),
					       source)));
		  return insns + 1;
		}
	    }

	  /* Don't duplicate cases already considered. */
	  for (i = 17; i < 24; i++)
	    {
	      if (((temp1 | (temp1 >> i)) == remainder)
		  && ! const_ok_for_arm (temp1))
		{
		  insns
		    = arm_split_constant (code, mode, temp1,
					  (new_src
					   = subtargets ? gen_reg_rtx (mode)
					   : target),
					  source, subtargets);
		  source = new_src;
		  emit_insn (gen_rtx (SET, VOIDmode, target,
				      gen_rtx (IOR, mode,
					       gen_rtx (LSHIFTRT, mode, source,
							GEN_INT (i)),
					       source)));
		  return insns + 1;
		}
	    }
	}
      break;

    case IOR:
    case XOR:
      /* If we have IOR or XOR, and the inverse of the constant can be loaded
	 in a single instruction, and we can find a temporary to put it in,
	 then this can be done in two instructions instead of 3-4.  */
      if (subtargets
	  || (reload_completed && ! reg_mentioned_p (target, source)))
	{
	  if (const_ok_for_arm (ARM_SIGN_EXTEND (~ val)))
	    {
	      rtx sub = subtargets ? gen_reg_rtx (mode) : target;

	      emit_insn (gen_rtx (SET, VOIDmode, sub,
				  GEN_INT (ARM_SIGN_EXTEND (~ val))));
	      emit_insn (gen_rtx (SET, VOIDmode, target, 
				  gen_rtx (code, mode, source, sub)));
	      return 2;
	    }
	}

      if (code == XOR)
	break;

      if (set_sign_bit_copies > 8
	  && (val & (-1 << (32 - set_sign_bit_copies))) == val)
	{
	  rtx sub = subtargets ? gen_reg_rtx (mode) : target;
	  rtx shift = GEN_INT (set_sign_bit_copies);

	  emit_insn (gen_rtx (SET, VOIDmode, sub,
			      gen_rtx (NOT, mode, 
				       gen_rtx (ASHIFT, mode, source, 
						shift))));
	  emit_insn (gen_rtx (SET, VOIDmode, target,
			      gen_rtx (NOT, mode,
				       gen_rtx (LSHIFTRT, mode, sub,
						shift))));
	  return 2;
	}

      if (set_zero_bit_copies > 8
	  && (remainder & ((1 << set_zero_bit_copies) - 1)) == remainder)
	{
	  rtx sub = subtargets ? gen_reg_rtx (mode) : target;
	  rtx shift = GEN_INT (set_zero_bit_copies);
	  
	  emit_insn (gen_rtx (SET, VOIDmode, sub,
			      gen_rtx (NOT, mode,
				       gen_rtx (LSHIFTRT, mode, source,
						shift))));
	  emit_insn (gen_rtx (SET, VOIDmode, target,
			      gen_rtx (NOT, mode,
				       gen_rtx (ASHIFT, mode, sub,
						shift))));
	  return 2;
	}

      if (const_ok_for_arm (temp1 = ARM_SIGN_EXTEND (~ val)))
	{
	  rtx sub = subtargets ? gen_reg_rtx (mode) : target;
	  emit_insn (gen_rtx (SET, VOIDmode, sub,
			      gen_rtx (NOT, mode, source)));
	  source = sub;
	  if (subtargets)
	    sub = gen_reg_rtx (mode);
	  emit_insn (gen_rtx (SET, VOIDmode, sub,
			      gen_rtx (AND, mode, source, GEN_INT (temp1))));
	  emit_insn (gen_rtx (SET, VOIDmode, target,
			      gen_rtx (NOT, mode, sub)));
	  return 3;
	}
      break;

    case AND:
      /* See if two shifts will do 2 or more insn's worth of work.  */
      if (clear_sign_bit_copies >= 16 && clear_sign_bit_copies < 24)
	{
	  HOST_WIDE_INT shift_mask = ((0xffffffff 
				       << (32 - clear_sign_bit_copies))
				      & 0xffffffff);
	  rtx new_source;
	  rtx shift = GEN_INT (clear_sign_bit_copies);

	  if ((remainder | shift_mask) != 0xffffffff)
	    {
	      new_source = subtargets ? gen_reg_rtx (mode) : target;
	      insns = arm_split_constant (AND, mode, remainder | shift_mask,
					  new_source, source, subtargets);
	      source = new_source;
	    }

	  new_source = subtargets ? gen_reg_rtx (mode) : target;
	  emit_insn (gen_ashlsi3 (new_source, source, shift));
	  emit_insn (gen_lshrsi3 (target, new_source, shift));
	  return insns + 2;
	}

      if (clear_zero_bit_copies >= 16 && clear_zero_bit_copies < 24)
	{
	  HOST_WIDE_INT shift_mask = (1 << clear_zero_bit_copies) - 1;
	  rtx new_source;
	  rtx shift = GEN_INT (clear_zero_bit_copies);
	  
	  if ((remainder | shift_mask) != 0xffffffff)
	    {
	      new_source = subtargets ? gen_reg_rtx (mode) : target;
	      insns = arm_split_constant (AND, mode, remainder | shift_mask,
					  new_source, source, subtargets);
	      source = new_source;
	    }

	  new_source = subtargets ? gen_reg_rtx (mode) : target;
	  emit_insn (gen_lshrsi3 (new_source, source, shift));
	  emit_insn (gen_ashlsi3 (target, new_source, shift));
	  return insns + 2;
	}

      break;

    default:
      break;
    }

  for (i = 0; i < 32; i++)
    if (remainder & (1 << i))
      num_bits_set++;

  if (code == AND || (can_invert && num_bits_set > 16))
    remainder = (~remainder) & 0xffffffff;
  else if (code == PLUS && num_bits_set > 16)
    remainder = (-remainder) & 0xffffffff;
  else
    {
      can_invert = 0;
      can_negate = 0;
    }

  /* Now try and find a way of doing the job in either two or three
     instructions.
     We start by looking for the largest block of zeros that are aligned on
     a 2-bit boundary, we then fill up the temps, wrapping around to the
     top of the word when we drop off the bottom.
     In the worst case this code should produce no more than four insns. */
  {
    int best_start = 0;
    int best_consecutive_zeros = 0;

    for (i = 0; i < 32; i += 2)
      {
	int consecutive_zeros = 0;

	if (! (remainder & (3 << i)))
	  {
	    while ((i < 32) && ! (remainder & (3 << i)))
	      {
		consecutive_zeros += 2;
		i += 2;
	      }
	    if (consecutive_zeros > best_consecutive_zeros)
	      {
		best_consecutive_zeros = consecutive_zeros;
		best_start = i - consecutive_zeros;
	      }
	    i -= 2;
	  }
      }

    /* Now start emitting the insns, starting with the one with the highest
       bit set: we do this so that the smallest number will be emitted last;
       this is more likely to be combinable with addressing insns. */
    i = best_start;
    do
      {
	int end;

	if (i <= 0)
	  i += 32;
	if (remainder & (3 << (i - 2)))
	  {
	    end = i - 8;
	    if (end < 0)
	      end += 32;
	    temp1 = remainder & ((0x0ff << end)
				 | ((i < end) ? (0xff >> (32 - end)) : 0));
	    remainder &= ~temp1;

	    if (code == SET)
	      {
		emit_insn (gen_rtx (SET, VOIDmode,
				    new_src = (subtargets ? gen_reg_rtx (mode)
					       : target),
				    GEN_INT (can_invert ? ~temp1 : temp1)));
		can_invert = 0;
		code = PLUS;
	      }
	    else if (code == MINUS)
	      {
		emit_insn (gen_rtx (SET, VOIDmode,
				    new_src = (subtargets ? gen_reg_rtx (mode)
					       : target),
				    gen_rtx (code, mode, GEN_INT (temp1),
					     source)));
		code = PLUS;
	      }
	    else
	      {
		emit_insn (gen_rtx (SET, VOIDmode,
				    new_src = remainder ? (subtargets
							   ? gen_reg_rtx (mode)
							   : target) : target,
				    gen_rtx (code, mode, source,
					     GEN_INT (can_invert ? ~temp1
						      : (can_negate
							 ? -temp1 : temp1)))));
	      }

	    insns++;
	    source = new_src;
	    i -= 6;
	  }
	i -= 2;
      } while (remainder);
  }
  return insns;
}

#define REG_OR_SUBREG_REG(X)						\
  (GET_CODE (X) == REG							\
   || (GET_CODE (X) == SUBREG && GET_CODE (SUBREG_REG (X)) == REG))

#define REG_OR_SUBREG_RTX(X)			\
   (GET_CODE (X) == REG ? (X) : SUBREG_REG (X))

#define ARM_FRAME_RTX(X)				\
  ((X) == frame_pointer_rtx || (X) == stack_pointer_rtx	\
   || (X) == arg_pointer_rtx)

int
arm_rtx_costs (x, code, outer_code)
     rtx x;
     enum rtx_code code, outer_code;
{
  enum machine_mode mode = GET_MODE (x);
  enum rtx_code subcode;
  int extra_cost;

  switch (code)
    {
    case MEM:
      /* Memory costs quite a lot for the first word, but subsequent words
	 load at the equivalent of a single insn each.  */
      return (10 + 4 * ((GET_MODE_SIZE (mode) - 1) / UNITS_PER_WORD)
	      + (CONSTANT_POOL_ADDRESS_P (x) ? 4 : 0));

    case DIV:
    case MOD:
      return 100;

    case ROTATE:
      if (mode == SImode && GET_CODE (XEXP (x, 1)) == REG)
	return 4;
      /* Fall through */
    case ROTATERT:
      if (mode != SImode)
	return 8;
      /* Fall through */
    case ASHIFT: case LSHIFTRT: case ASHIFTRT:
      if (mode == DImode)
	return (8 + (GET_CODE (XEXP (x, 1)) == CONST_INT ? 0 : 8)
		+ ((GET_CODE (XEXP (x, 0)) == REG 
		    || (GET_CODE (XEXP (x, 0)) == SUBREG
			&& GET_CODE (SUBREG_REG (XEXP (x, 0))) == REG))
		   ? 0 : 8));
      return (1 + ((GET_CODE (XEXP (x, 0)) == REG
		    || (GET_CODE (XEXP (x, 0)) == SUBREG
			&& GET_CODE (SUBREG_REG (XEXP (x, 0))) == REG))
		   ? 0 : 4)
	      + ((GET_CODE (XEXP (x, 1)) == REG
		  || (GET_CODE (XEXP (x, 1)) == SUBREG
		      && GET_CODE (SUBREG_REG (XEXP (x, 1))) == REG)
		  || (GET_CODE (XEXP (x, 1)) == CONST_INT))
		 ? 0 : 4));

    case MINUS:
      if (mode == DImode)
	return (4 + (REG_OR_SUBREG_REG (XEXP (x, 1)) ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 0))
		    || (GET_CODE (XEXP (x, 0)) == CONST_INT
		       && const_ok_for_arm (INTVAL (XEXP (x, 0)))))
		   ? 0 : 8));

      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return (2 + ((REG_OR_SUBREG_REG (XEXP (x, 1))
		      || (GET_CODE (XEXP (x, 1)) == CONST_DOUBLE
			  && const_double_rtx_ok_for_fpu (XEXP (x, 1))))
		     ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 0))
		    || (GET_CODE (XEXP (x, 0)) == CONST_DOUBLE
			&& const_double_rtx_ok_for_fpu (XEXP (x, 0))))
		   ? 0 : 8));

      if (((GET_CODE (XEXP (x, 0)) == CONST_INT
	    && const_ok_for_arm (INTVAL (XEXP (x, 0)))
	    && REG_OR_SUBREG_REG (XEXP (x, 1))))
	  || (((subcode = GET_CODE (XEXP (x, 1))) == ASHIFT
	       || subcode == ASHIFTRT || subcode == LSHIFTRT
	       || subcode == ROTATE || subcode == ROTATERT
	       || (subcode == MULT
		   && GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT
		   && ((INTVAL (XEXP (XEXP (x, 1), 1)) &
			(INTVAL (XEXP (XEXP (x, 1), 1)) - 1)) == 0)))
	      && REG_OR_SUBREG_REG (XEXP (XEXP (x, 1), 0))
	      && (REG_OR_SUBREG_REG (XEXP (XEXP (x, 1), 1))
		  || GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT)
	      && REG_OR_SUBREG_REG (XEXP (x, 0))))
	return 1;
      /* Fall through */

    case PLUS: 
      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return (2 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 1))
		    || (GET_CODE (XEXP (x, 1)) == CONST_DOUBLE
			&& const_double_rtx_ok_for_fpu (XEXP (x, 1))))
		   ? 0 : 8));

      /* Fall through */
    case AND: case XOR: case IOR: 
      extra_cost = 0;

      /* Normally the frame registers will be spilt into reg+const during
	 reload, so it is a bad idea to combine them with other instructions,
	 since then they might not be moved outside of loops.  As a compromise
	 we allow integration with ops that have a constant as their second
	 operand.  */
      if ((REG_OR_SUBREG_REG (XEXP (x, 0))
	   && ARM_FRAME_RTX (REG_OR_SUBREG_RTX (XEXP (x, 0)))
	   && GET_CODE (XEXP (x, 1)) != CONST_INT)
	  || (REG_OR_SUBREG_REG (XEXP (x, 0))
	      && ARM_FRAME_RTX (REG_OR_SUBREG_RTX (XEXP (x, 0)))))
	extra_cost = 4;

      if (mode == DImode)
	return (4 + extra_cost + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 1))
		    || (GET_CODE (XEXP (x, 1)) == CONST_INT
			&& const_ok_for_op (INTVAL (XEXP (x, 1)), code, mode)))
		   ? 0 : 8));

      if (REG_OR_SUBREG_REG (XEXP (x, 0)))
	return (1 + (GET_CODE (XEXP (x, 1)) == CONST_INT ? 0 : extra_cost)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 1))
		    || (GET_CODE (XEXP (x, 1)) == CONST_INT
			&& const_ok_for_op (INTVAL (XEXP (x, 1)), code, mode)))
		   ? 0 : 4));

      else if (REG_OR_SUBREG_REG (XEXP (x, 1)))
	return (1 + extra_cost
		+ ((((subcode = GET_CODE (XEXP (x, 0))) == ASHIFT
		     || subcode == LSHIFTRT || subcode == ASHIFTRT
		     || subcode == ROTATE || subcode == ROTATERT
		     || (subcode == MULT
			 && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
			 && ((INTVAL (XEXP (XEXP (x, 0), 1)) &
			      (INTVAL (XEXP (XEXP (x, 0), 1)) - 1)) == 0))
		    && (REG_OR_SUBREG_REG (XEXP (XEXP (x, 0), 0)))
		    && ((REG_OR_SUBREG_REG (XEXP (XEXP (x, 0), 1)))
			|| GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)))
		   ? 0 : 4));

      return 8;

    case MULT:
      if (GET_MODE_CLASS (mode) == MODE_FLOAT
	  || mode == DImode)
	return 30;

      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  HOST_WIDE_INT i = INTVAL (XEXP (x, 1)) & 0xffffffff;
	  int add_cost = const_ok_for_arm (i) ? 4 : 8;
	  int j;
	  
	  /* This will need adjusting for ARM's with fast multiplies */
	  for (j = 0; i && j < 32; j += 2)
	    {
	      i &= ~(3 << j);
	      add_cost += 2;
	    }

	  return add_cost;
	}

      return (30 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 4)
	      + (REG_OR_SUBREG_REG (XEXP (x, 1)) ? 0 : 4));

    case NEG:
      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return 4 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 6);
      /* Fall through */
    case NOT:
      if (mode == DImode)
	return 4 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 4);

      return 1 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 4);

    case IF_THEN_ELSE:
      if (GET_CODE (XEXP (x, 1)) == PC || GET_CODE (XEXP (x, 2)) == PC)
	return 14;
      return 2;

    case COMPARE:
      return 1;

    case ABS:
      return 4 + (mode == DImode ? 4 : 0);

    case SIGN_EXTEND:
      if (GET_MODE (XEXP (x, 0)) == QImode)
	return (4 + (mode == DImode ? 4 : 0)
		+ (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));
      /* Fall through */
    case ZERO_EXTEND:
      switch (GET_MODE (XEXP (x, 0)))
	{
	case QImode:
	  return (1 + (mode == DImode ? 4 : 0)
		  + (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));

	case HImode:
	  return (4 + (mode == DImode ? 4 : 0)
		  + (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));

	case SImode:
	  return (1 + (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));
	}
      abort ();

    default:
      return 99;
    }
}
     
/* This code has been fixed for cross compilation. */

static int fpa_consts_inited = 0;

char *strings_fpa[8] = {
  "0.0",
  "1.0",
  "2.0",
  "3.0",
  "4.0",
  "5.0",
  "0.5",
  "10.0"
  };

static REAL_VALUE_TYPE values_fpa[8];

static void
init_fpa_table ()
{
  int i;
  REAL_VALUE_TYPE r;

  for (i = 0; i < 8; i++)
    {
      r = REAL_VALUE_ATOF (strings_fpa[i], DFmode);
      values_fpa[i] = r;
    }

  fpa_consts_inited = 1;
}

/* Return TRUE if rtx X is a valid immediate FPU constant. */

int
const_double_rtx_ok_for_fpu (x)
     rtx x;
{
  REAL_VALUE_TYPE r;
  int i;
  
  if (!fpa_consts_inited)
    init_fpa_table ();
  
  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  if (REAL_VALUE_MINUS_ZERO (r))
    return 0;

  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (r, values_fpa[i]))
      return 1;

  return 0;
}

/* Return TRUE if rtx X is a valid immediate FPU constant. */

int
neg_const_double_rtx_ok_for_fpu (x)
     rtx x;
{
  REAL_VALUE_TYPE r;
  int i;
  
  if (!fpa_consts_inited)
    init_fpa_table ();
  
  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  r = REAL_VALUE_NEGATE (r);
  if (REAL_VALUE_MINUS_ZERO (r))
    return 0;

  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (r, values_fpa[i]))
      return 1;

  return 0;
}

/* Predicates for `match_operand' and `match_operator'.  */

/* s_register_operand is the same as register_operand, but it doesn't accept
   (SUBREG (MEM)...). */

int
s_register_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
}

/* Only accept reg, subreg(reg), const_int.  */

int
reg_or_int_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return 1;

  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
}

/* Return 1 if OP is an item in memory, given that we are in reload.  */

int
reload_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  int regno = true_regnum (op);

  return (! CONSTANT_P (op)
	  && (regno == -1
	      || (GET_CODE (op) == REG
		  && REGNO (op) >= FIRST_PSEUDO_REGISTER)));
}

/* Return TRUE for valid operands for the rhs of an ARM instruction.  */

int
arm_rhs_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && const_ok_for_arm (INTVAL (op))));
}

/* Return TRUE for valid operands for the rhs of an ARM instruction, or a load.
 */

int
arm_rhsm_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && const_ok_for_arm (INTVAL (op)))
	  || memory_operand (op, mode));
}

/* Return TRUE for valid operands for the rhs of an ARM instruction, or if a
   constant that is valid when negated.  */

int
arm_add_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (const_ok_for_arm (INTVAL (op))
		  || const_ok_for_arm (-INTVAL (op)))));
}

int
arm_not_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (const_ok_for_arm (INTVAL (op))
		  || const_ok_for_arm (~INTVAL (op)))));
}

/* Return TRUE for valid operands for the rhs of an FPU instruction.  */

int
fpu_rhs_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return TRUE;
  else if (GET_CODE (op) == CONST_DOUBLE)
    return (const_double_rtx_ok_for_fpu (op));

  return FALSE;
}

int
fpu_add_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return TRUE;
  else if (GET_CODE (op) == CONST_DOUBLE)
    return (const_double_rtx_ok_for_fpu (op) 
	    || neg_const_double_rtx_ok_for_fpu (op));

  return FALSE;
}

/* Return nonzero if OP is a constant power of two.  */

int
power_of_two_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    {
      HOST_WIDE_INT value = INTVAL(op);
      return value != 0  &&  (value & (value - 1)) == 0;
    }
  return FALSE;
}

/* Return TRUE for a valid operand of a DImode operation.
   Either: REG, CONST_DOUBLE or MEM(DImode_address).
   Note that this disallows MEM(REG+REG), but allows
   MEM(PRE/POST_INC/DEC(REG)).  */

int
di_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (s_register_operand (op, mode))
    return TRUE;

  switch (GET_CODE (op))
    {
    case CONST_DOUBLE:
    case CONST_INT:
      return TRUE;

    case MEM:
      return memory_address_p (DImode, XEXP (op, 0));

    default:
      return FALSE;
    }
}

/* Return TRUE for valid index operands. */

int
index_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (s_register_operand(op, mode)
	  || (immediate_operand (op, mode)
	      && INTVAL (op) < 4096 && INTVAL (op) > -4096));
}

/* Return TRUE for valid shifts by a constant. This also accepts any
   power of two on the (somewhat overly relaxed) assumption that the
   shift operator in this case was a mult. */

int
const_shift_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (power_of_two_operand (op, mode)
	  || (immediate_operand (op, mode)
	      && (INTVAL (op) < 32 && INTVAL (op) > 0)));
}

/* Return TRUE for arithmetic operators which can be combined with a multiply
   (shift).  */

int
shiftable_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (GET_MODE (x) != mode)
    return FALSE;
  else
    {
      enum rtx_code code = GET_CODE (x);

      return (code == PLUS || code == MINUS
	      || code == IOR || code == XOR || code == AND);
    }
}

/* Return TRUE for shift operators. */

int
shift_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (GET_MODE (x) != mode)
    return FALSE;
  else
    {
      enum rtx_code code = GET_CODE (x);

      if (code == MULT)
	return power_of_two_operand (XEXP (x, 1));

      return (code == ASHIFT || code == ASHIFTRT || code == LSHIFTRT
	      || code == ROTATERT);
    }
}

int equality_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  return GET_CODE (x) == EQ || GET_CODE (x) == NE;
}

/* Return TRUE for SMIN SMAX UMIN UMAX operators. */

int
minmax_operator (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);

  if (GET_MODE (x) != mode)
    return FALSE;

  return code == SMIN || code == SMAX || code == UMIN || code == UMAX;
}

/* return TRUE if x is EQ or NE */

/* Return TRUE if this is the condition code register, if we aren't given
   a mode, accept any class CCmode register */

int
cc_register (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    {
      mode = GET_MODE (x);
      if (GET_MODE_CLASS (mode) != MODE_CC)
	return FALSE;
    }

  if (mode == GET_MODE (x) && GET_CODE (x) == REG && REGNO (x) == 24)
    return TRUE;

  return FALSE;
}

/* Return TRUE if this is the condition code register, if we aren't given
   a mode, accept any mode in class CC_MODE that is reversible */

int
reversible_cc_register (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    {
      mode = GET_MODE (x);
      if (GET_MODE_CLASS (mode) != MODE_CC
	  && GET_CODE (x) == REG && REGNO (x) == 24)
	abort ();
      if (GET_MODE_CLASS (mode) != MODE_CC
	  || (! flag_fast_math && ! REVERSIBLE_CC_MODE (mode)))
	return FALSE;
    }

  if (mode == GET_MODE (x) && GET_CODE (x) == REG && REGNO (x) == 24)
    return TRUE;

  return FALSE;
}

enum rtx_code
minmax_code (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);

  if (code == SMAX)
    return GE;
  else if (code == SMIN)
    return LE;
  else if (code == UMIN)
    return LEU;
  else if (code == UMAX)
    return GEU;

  abort ();
}

/* Return 1 if memory locations are adjacent */

int
adjacent_mem_locations (a, b)
     rtx a, b;
{
  int val0 = 0, val1 = 0;
  int reg0, reg1;
  
  if ((GET_CODE (XEXP (a, 0)) == REG
       || (GET_CODE (XEXP (a, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (a, 0), 1)) == CONST_INT))
      && (GET_CODE (XEXP (b, 0)) == REG
	  || (GET_CODE (XEXP (b, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (b, 0), 1)) == CONST_INT)))
    {
      if (GET_CODE (XEXP (a, 0)) == PLUS)
        {
	  reg0 = REGNO (XEXP (XEXP (a, 0), 0));
	  val0 = INTVAL (XEXP (XEXP (a, 0), 1));
        }
      else
	reg0 = REGNO (XEXP (a, 0));
      if (GET_CODE (XEXP (b, 0)) == PLUS)
        {
	  reg1 = REGNO (XEXP (XEXP (b, 0), 0));
	  val1 = INTVAL (XEXP (XEXP (b, 0), 1));
        }
      else
	reg1 = REGNO (XEXP (b, 0));
      return (reg0 == reg1) && ((val1 - val0) == 4 || (val0 - val1) == 4);
    }
  return 0;
}

/* Return 1 if OP is a load multiple operation.  It is known to be
   parallel and the first section will be tested. */

int
load_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode;
{
  HOST_WIDE_INT count = XVECLEN (op, 0);
  int dest_regno;
  rtx src_addr;
  HOST_WIDE_INT i = 1, base = 0;
  rtx elt;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return 0;

  /* Check to see if this might be a write-back */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully */
      if (GET_CODE (SET_DEST (elt)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
          || REGNO (XEXP (SET_SRC (elt), 0)) != REGNO (SET_DEST (elt))
          || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
          || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 2) * 4
          || GET_CODE (XVECEXP (op, 0, count - 1)) != CLOBBER
          || GET_CODE (XEXP (XVECEXP (op, 0, count - 1), 0)) != REG
          || REGNO (XEXP (XVECEXP (op, 0, count - 1), 0))
              != REGNO (SET_DEST (elt)))
        return 0;

      count--;
    }

  /* Perform a quick check so we don't blow up below.  */
  if (count <= i
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != MEM)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, i - 1)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, i - 1)), 0);

  for (; i < count; i++)
    {
      rtx elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_DEST (elt)) != REG
          || GET_MODE (SET_DEST (elt)) != SImode
          || REGNO (SET_DEST (elt)) != dest_regno + i - base
          || GET_CODE (SET_SRC (elt)) != MEM
          || GET_MODE (SET_SRC (elt)) != SImode
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
          || ! rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
          || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != (i - base) * 4)
        return 0;
    }

  return 1;
}

/* Return 1 if OP is a store multiple operation.  It is known to be
   parallel and the first section will be tested. */

int
store_multiple_operation (op, mode)
     rtx op;
     enum machine_mode mode;
{
  HOST_WIDE_INT count = XVECLEN (op, 0);
  int src_regno;
  rtx dest_addr;
  HOST_WIDE_INT i = 1, base = 0;
  rtx elt;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return 0;

  /* Check to see if this might be a write-back */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully */
      if (GET_CODE (SET_DEST (elt)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
          || REGNO (XEXP (SET_SRC (elt), 0)) != REGNO (SET_DEST (elt))
          || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
          || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 2) * 4
          || GET_CODE (XVECEXP (op, 0, count - 1)) != CLOBBER
          || GET_CODE (XEXP (XVECEXP (op, 0, count - 1), 0)) != REG
          || REGNO (XEXP (XVECEXP (op, 0, count - 1), 0))
              != REGNO (SET_DEST (elt)))
        return 0;

      count--;
    }

  /* Perform a quick check so we don't blow up below.  */
  if (count <= i
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != REG)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, i - 1)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, i - 1)), 0);

  for (; i < count; i++)
    {
      elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_SRC (elt)) != REG
          || GET_MODE (SET_SRC (elt)) != SImode
          || REGNO (SET_SRC (elt)) != src_regno + i - base
          || GET_CODE (SET_DEST (elt)) != MEM
          || GET_MODE (SET_DEST (elt)) != SImode
          || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
          || ! rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
          || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != (i - base) * 4)
        return 0;
    }

  return 1;
}

int
multi_register_push (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != PARALLEL
      || (GET_CODE (XVECEXP (op, 0, 0)) != SET)
      || (GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != UNSPEC)
      || (XINT (SET_SRC (XVECEXP (op, 0, 0)), 1) != 2))
    return 0;

  return 1;
}


/* Routines for use with attributes */

int
const_pool_offset (symbol)
     rtx symbol;
{
  return get_pool_offset (symbol) - get_pool_size () - get_prologue_size ();
}

/* Routines for use in generating RTL */

rtx
arm_gen_load_multiple (base_regno, count, from, up, write_back)
     int base_regno;
     int count;
     rtx from;
     int up;
     int write_back;
{
  int i = 0, j;
  rtx result;
  int sign = up ? 1 : -1;

  result = gen_rtx (PARALLEL, VOIDmode,
                    rtvec_alloc (count + (write_back ? 2 : 0)));
  if (write_back)
    {
      XVECEXP (result, 0, 0)
	= gen_rtx (SET, GET_MODE (from), from,
		   plus_constant (from, count * 4 * sign));
      i = 1;
      count++;
    }

  for (j = 0; i < count; i++, j++)
    {
      XVECEXP (result, 0, i)
	= gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, base_regno + j),
		   gen_rtx (MEM, SImode,
			    plus_constant (from, j * 4 * sign)));
    }

  if (write_back)
    XVECEXP (result, 0, i) = gen_rtx (CLOBBER, SImode, from);

  return result;
}

rtx
arm_gen_store_multiple (base_regno, count, to, up, write_back)
     int base_regno;
     int count;
     rtx to;
     int up;
     int write_back;
{
  int i = 0, j;
  rtx result;
  int sign = up ? 1 : -1;

  result = gen_rtx (PARALLEL, VOIDmode,
                    rtvec_alloc (count + (write_back ? 2 : 0)));
  if (write_back)
    {
      XVECEXP (result, 0, 0)
	= gen_rtx (SET, GET_MODE (to), to,
		   plus_constant (to, count * 4 * sign));
      i = 1;
      count++;
    }

  for (j = 0; i < count; i++, j++)
    {
      XVECEXP (result, 0, i)
	= gen_rtx (SET, VOIDmode,
		   gen_rtx (MEM, SImode, plus_constant (to, j * 4 * sign)),
		   gen_rtx (REG, SImode, base_regno + j));
    }

  if (write_back)
    XVECEXP (result, 0, i) = gen_rtx (CLOBBER, SImode, to);

  return result;
}

int
arm_gen_movstrqi (operands)
     rtx *operands;
{
  HOST_WIDE_INT in_words_to_go, out_words_to_go, last_bytes;
  int i, r;
  rtx const_sxteen = gen_rtx (CONST_INT, SImode, 16);
  rtx src, dst;
  rtx st_src, st_dst, end_src, end_dst, fin_src, fin_dst;
  rtx part_bytes_reg = NULL;
  extern int optimize;

  if (GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[3]) != CONST_INT
      || INTVAL (operands[2]) > 64
      || INTVAL (operands[3]) & 3)
    return 0;

  st_dst = XEXP (operands[0], 0);
  st_src = XEXP (operands[1], 0);
  fin_dst = dst = copy_to_mode_reg (SImode, st_dst);
  fin_src = src = copy_to_mode_reg (SImode, st_src);

  in_words_to_go = (INTVAL (operands[2]) + 3) / 4;
  out_words_to_go = INTVAL (operands[2]) / 4;
  last_bytes = INTVAL (operands[2]) & 3;

  if (out_words_to_go != in_words_to_go && ((in_words_to_go - 1) & 3) != 0)
    part_bytes_reg = gen_rtx (REG, SImode, (in_words_to_go - 1) & 3);

  for (i = 0; in_words_to_go >= 2; i+=4)
    {
      emit_insn (arm_gen_load_multiple (0, (in_words_to_go > 4 
					    ? 4 : in_words_to_go),
                                        src, TRUE, TRUE));
      if (out_words_to_go)
	{
	  if (out_words_to_go != 1)
	    emit_insn (arm_gen_store_multiple (0, (out_words_to_go > 4
						   ? 4 : out_words_to_go),
					       dst, TRUE, TRUE));
	  else
	    {
	      emit_move_insn (gen_rtx (MEM, SImode, dst),
			      gen_rtx (REG, SImode, 0));
	      emit_insn (gen_addsi3 (dst, dst, GEN_INT (4)));
	    }
	}

      in_words_to_go -= in_words_to_go < 4 ? in_words_to_go : 4;
      out_words_to_go -= out_words_to_go < 4 ? out_words_to_go : 4;
    }

  /* OUT_WORDS_TO_GO will be zero here if there are byte stores to do.  */
  if (out_words_to_go)
  {
    rtx sreg;

    emit_move_insn (sreg = gen_reg_rtx (SImode), gen_rtx (MEM, SImode, src));
    emit_move_insn (fin_src = gen_reg_rtx (SImode), plus_constant (src, 4));
    emit_move_insn (gen_rtx (MEM, SImode, dst), sreg);
    emit_move_insn (fin_dst = gen_reg_rtx (SImode), plus_constant (dst, 4));
    in_words_to_go--;

    if (in_words_to_go)	/* Sanity check */
      abort ();
  }

  if (in_words_to_go)
    {
      if (in_words_to_go < 0)
	abort ();

      part_bytes_reg = copy_to_mode_reg (SImode, gen_rtx (MEM, SImode, src));
      emit_insn (gen_addsi3 (src, src, GEN_INT (4)));
    }

  if (BYTES_BIG_ENDIAN && last_bytes)
    {
      rtx tmp = gen_reg_rtx (SImode);

      if (part_bytes_reg == NULL)
	abort ();

      /* The bytes we want are in the top end of the word */
      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg,
			      GEN_INT (8 * (4 - last_bytes))));
      part_bytes_reg = tmp;
      
      while (last_bytes)
	{
	  emit_move_insn (gen_rtx (MEM, QImode, 
				   plus_constant (dst, last_bytes - 1)),
			  gen_rtx (SUBREG, QImode, part_bytes_reg, 0));
	  if (--last_bytes)
	    {
	      tmp = gen_reg_rtx (SImode);
	      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg, GEN_INT (8)));
	      part_bytes_reg = tmp;
	    }
	}
	  
    }
  else
    {
      while (last_bytes)
	{
	  if (part_bytes_reg == NULL)
	    abort ();

	  emit_move_insn (gen_rtx (MEM, QImode, dst),
			  gen_rtx (SUBREG, QImode, part_bytes_reg, 0));
	  emit_insn (gen_addsi3 (dst, dst, const1_rtx));
	  if (--last_bytes)
	    {
	      rtx tmp = gen_reg_rtx (SImode);
	      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg, GEN_INT (8)));
	      part_bytes_reg = tmp;
	    }
	}
    }

  return 1;
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 0 in the proper mode.  FP means this is a
   floating point compare: I don't think that it is needed on the arm.  */

rtx
gen_compare_reg (code, x, y, fp)
     enum rtx_code code;
     rtx x, y;
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg = gen_rtx (REG, mode, 24);

  emit_insn (gen_rtx (SET, VOIDmode, cc_reg,
                      gen_rtx (COMPARE, mode, x, y)));

  return cc_reg;
}

void
arm_reload_in_hi (operands)
     rtx *operands;
{
  rtx base = find_replacement (&XEXP (operands[1], 0));

  emit_insn (gen_zero_extendqisi2 (operands[2], gen_rtx (MEM, QImode, base)));
  emit_insn (gen_zero_extendqisi2 (gen_rtx (SUBREG, SImode, operands[0], 0),
				   gen_rtx (MEM, QImode, 
					    plus_constant (base, 1))));
  if (BYTES_BIG_ENDIAN)
    emit_insn (gen_rtx (SET, VOIDmode, gen_rtx (SUBREG, SImode, 
						operands[0], 0),
			gen_rtx (IOR, SImode, 
				 gen_rtx (ASHIFT, SImode,
					  gen_rtx (SUBREG, SImode,
						   operands[0], 0),
					  GEN_INT (8)),
				 operands[2])));
  else
    emit_insn (gen_rtx (SET, VOIDmode, gen_rtx (SUBREG, SImode, 
						operands[0], 0),
			gen_rtx (IOR, SImode, 
				 gen_rtx (ASHIFT, SImode,
					  operands[2],
					  GEN_INT (8)),
				 gen_rtx (SUBREG, SImode, operands[0], 0))));
}

void
arm_reload_out_hi (operands)
     rtx *operands;
{
  rtx base = find_replacement (&XEXP (operands[0], 0));

  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_movqi (gen_rtx (MEM, QImode, plus_constant (base, 1)),
			    gen_rtx (SUBREG, QImode, operands[1], 0)));
      emit_insn (gen_lshrsi3 (operands[2],
			      gen_rtx (SUBREG, SImode, operands[1], 0),
			      GEN_INT (8)));
      emit_insn (gen_movqi (gen_rtx (MEM, QImode, base),
			    gen_rtx (SUBREG, QImode, operands[2], 0)));
    }
  else
    {
      emit_insn (gen_movqi (gen_rtx (MEM, QImode, base),
			    gen_rtx (SUBREG, QImode, operands[1], 0)));
      emit_insn (gen_lshrsi3 (operands[2],
			      gen_rtx (SUBREG, SImode, operands[1], 0),
			      GEN_INT (8)));
      emit_insn (gen_movqi (gen_rtx (MEM, QImode, plus_constant (base, 1)),
			    gen_rtx (SUBREG, QImode, operands[2], 0)));
    }
}

/* Check to see if a branch is forwards or backwards.  Return TRUE if it
   is backwards.  */

int
arm_backwards_branch (from, to)
     int from, to;
{
  return insn_addresses[to] <= insn_addresses[from];
}

/* Check to see if a branch is within the distance that can be done using
   an arithmetic expression. */
int
short_branch (from, to)
     int from, to;
{
  int delta = insn_addresses[from] + 8 - insn_addresses[to];

  return abs (delta) < 980;	/* A small margin for safety */
}

/* Check to see that the insn isn't the target of the conditionalizing
   code */
int
arm_insn_not_targeted (insn)
     rtx insn;
{
  return insn != arm_target_insn;
}


/* Routines to output assembly language.  */

/* If the rtx is the correct value then return the string of the number.
   In this way we can ensure that valid double constants are generated even
   when cross compiling. */
char *
fp_immediate_constant (x)
     rtx x;
{
  REAL_VALUE_TYPE r;
  int i;
  
  if (!fpa_consts_inited)
    init_fpa_table ();
  
  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (r, values_fpa[i]))
      return strings_fpa[i];

  abort ();
}

/* As for fp_immediate_constant, but value is passed directly, not in rtx.  */
static char *
fp_const_from_val (r)
     REAL_VALUE_TYPE *r;
{
  int i;

  if (! fpa_consts_inited)
    init_fpa_table ();

  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (*r, values_fpa[i]))
      return strings_fpa[i];

  abort ();
}

/* Output the operands of a LDM/STM instruction to STREAM.
   MASK is the ARM register set mask of which only bits 0-15 are important.
   INSTR is the possibly suffixed base register.  HAT unequals zero if a hat
   must follow the register list.  */

void
print_multi_reg (stream, instr, mask, hat)
     FILE *stream;
     char *instr;
     int mask, hat;
{
  int i;
  int not_first = FALSE;

  fputc ('\t', stream);
  fprintf (stream, instr, ARM_REG_PREFIX);
  fputs (", {", stream);
  for (i = 0; i < 16; i++)
    if (mask & (1 << i))
      {
	if (not_first)
	  fprintf (stream, ", ");
	fprintf (stream, "%s%s", ARM_REG_PREFIX, reg_names[i]);
	not_first = TRUE;
      }

  fprintf (stream, "}%s\n", hat ? "^" : "");
}

/* Output a 'call' insn. */

char *
output_call (operands)
     rtx *operands;
{
  /* Handle calls to lr using ip (which may be clobbered in subr anyway). */

  if (REGNO (operands[0]) == 14)
    {
      operands[0] = gen_rtx (REG, SImode, 12);
      output_asm_insn ("mov%?\t%0, %|lr", operands);
    }
  output_asm_insn ("mov%?\t%|lr, %|pc", operands);
  output_asm_insn ("mov%?\t%|pc, %0", operands);
  return "";
}

static int
eliminate_lr2ip (x)
     rtx *x;
{
  int something_changed = 0;
  rtx x0 = *x;
  int code = GET_CODE (x0);
  register int i, j;
  register char *fmt;
  
  switch (code)
    {
    case REG:
      if (REGNO (x0) == 14)
        {
	  *x = gen_rtx (REG, SImode, 12);
	  return 1;
        }
      return 0;
    default:
      /* Scan through the sub-elements and change any references there */
      fmt = GET_RTX_FORMAT (code);
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	if (fmt[i] == 'e')
	  something_changed |= eliminate_lr2ip (&XEXP (x0, i));
	else if (fmt[i] == 'E')
	  for (j = 0; j < XVECLEN (x0, i); j++)
	    something_changed |= eliminate_lr2ip (&XVECEXP (x0, i, j));
      return something_changed;
    }
}
  
/* Output a 'call' insn that is a reference in memory. */

char *
output_call_mem (operands)
     rtx *operands;
{
  operands[0] = copy_rtx (operands[0]); /* Be ultra careful */
  /* Handle calls using lr by using ip (which may be clobbered in subr anyway).
   */
  if (eliminate_lr2ip (&operands[0]))
    output_asm_insn ("mov%?\t%|ip, %|lr", operands);

  output_asm_insn ("mov%?\t%|lr, %|pc", operands);
  output_asm_insn ("ldr%?\t%|pc, %0", operands);
  return "";
}


/* Output a move from arm registers to an fpu registers.
   OPERANDS[0] is an fpu register.
   OPERANDS[1] is the first registers of an arm register pair.  */

char *
output_mov_long_double_fpu_from_arm (operands)
     rtx *operands;
{
  int arm_reg0 = REGNO (operands[1]);
  rtx ops[3];

  if (arm_reg0 == 12)
    abort();

  ops[0] = gen_rtx (REG, SImode, arm_reg0);
  ops[1] = gen_rtx (REG, SImode, 1 + arm_reg0);
  ops[2] = gen_rtx (REG, SImode, 2 + arm_reg0);
  
  output_asm_insn ("stm%?fd\t%|sp!, {%0, %1, %2}", ops);
  output_asm_insn ("ldf%?e\t%0, [%|sp], #12", operands);
  return "";
}

/* Output a move from an fpu register to arm registers.
   OPERANDS[0] is the first registers of an arm register pair.
   OPERANDS[1] is an fpu register.  */

char *
output_mov_long_double_arm_from_fpu (operands)
     rtx *operands;
{
  int arm_reg0 = REGNO (operands[0]);
  rtx ops[3];

  if (arm_reg0 == 12)
    abort();

  ops[0] = gen_rtx (REG, SImode, arm_reg0);
  ops[1] = gen_rtx (REG, SImode, 1 + arm_reg0);
  ops[2] = gen_rtx (REG, SImode, 2 + arm_reg0);

  output_asm_insn ("stf%?e\t%1, [%|sp, #-12]!", operands);
  output_asm_insn ("ldm%?fd\t%|sp!, {%0, %1, %2}", ops);
  return "";
}

/* Output a move from arm registers to arm registers of a long double
   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.  */
char *
output_mov_long_double_arm_from_arm (operands)
     rtx *operands;
{
  /* We have to be careful here because the two might overlap */
  int dest_start = REGNO (operands[0]);
  int src_start = REGNO (operands[1]);
  rtx ops[2];
  int i;

  if (dest_start < src_start)
    {
      for (i = 0; i < 3; i++)
	{
	  ops[0] = gen_rtx (REG, SImode, dest_start + i);
	  ops[1] = gen_rtx (REG, SImode, src_start + i);
	  output_asm_insn ("mov%?\t%0, %1", ops);
	}
    }
  else
    {
      for (i = 2; i >= 0; i--)
	{
	  ops[0] = gen_rtx (REG, SImode, dest_start + i);
	  ops[1] = gen_rtx (REG, SImode, src_start + i);
	  output_asm_insn ("mov%?\t%0, %1", ops);
	}
    }

  return "";
}


/* Output a move from arm registers to an fpu registers.
   OPERANDS[0] is an fpu register.
   OPERANDS[1] is the first registers of an arm register pair.  */

char *
output_mov_double_fpu_from_arm (operands)
     rtx *operands;
{
  int arm_reg0 = REGNO (operands[1]);
  rtx ops[2];

  if (arm_reg0 == 12)
    abort();
  ops[0] = gen_rtx (REG, SImode, arm_reg0);
  ops[1] = gen_rtx (REG, SImode, 1 + arm_reg0);
  output_asm_insn ("stm%?fd\t%|sp!, {%0, %1}", ops);
  output_asm_insn ("ldf%?d\t%0, [%|sp], #8", operands);
  return "";
}

/* Output a move from an fpu register to arm registers.
   OPERANDS[0] is the first registers of an arm register pair.
   OPERANDS[1] is an fpu register.  */

char *
output_mov_double_arm_from_fpu (operands)
     rtx *operands;
{
  int arm_reg0 = REGNO (operands[0]);
  rtx ops[2];

  if (arm_reg0 == 12)
    abort();

  ops[0] = gen_rtx (REG, SImode, arm_reg0);
  ops[1] = gen_rtx (REG, SImode, 1 + arm_reg0);
  output_asm_insn ("stf%?d\t%1, [%|sp, #-8]!", operands);
  output_asm_insn ("ldm%?fd\t%|sp!, {%0, %1}", ops);
  return "";
}

/* Output a move between double words.
   It must be REG<-REG, REG<-CONST_DOUBLE, REG<-CONST_INT, REG<-MEM
   or MEM<-REG and all MEMs must be offsettable addresses.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);
  rtx otherops[2];

  if (code0 == REG)
    {
      int reg0 = REGNO (operands[0]);

      otherops[0] = gen_rtx (REG, SImode, 1 + reg0);
      if (code1 == REG)
	{
	  int reg1 = REGNO (operands[1]);
	  if (reg1 == 12)
	    abort();

	  otherops[1] = gen_rtx (REG, SImode, 1 + reg1);

	  /* Ensure the second source is not overwritten */
	  if (reg0 == 1 + reg1)
	    {
	      output_asm_insn("mov%?\t%0, %1", otherops);
	      output_asm_insn("mov%?\t%0, %1", operands);
	    }
	  else
	    {
	      output_asm_insn("mov%?\t%0, %1", operands);
	      output_asm_insn("mov%?\t%0, %1", otherops);
	    }
	}
      else if (code1 == CONST_DOUBLE)
	{
	  otherops[1] = gen_rtx (CONST_INT, VOIDmode,
				 CONST_DOUBLE_HIGH (operands[1]));
	  operands[1] = gen_rtx (CONST_INT, VOIDmode,
				 CONST_DOUBLE_LOW (operands[1]));
	  output_mov_immediate (operands, FALSE, "");
	  output_mov_immediate (otherops, FALSE, "");
	}
      else if (code1 == CONST_INT)
	{
	  otherops[1] = const0_rtx;
	  /* sign extend the intval into the high-order word */
	  /* Note: output_mov_immediate may clobber operands[1], so we
	     put this out first */
	  if (INTVAL (operands[1]) < 0)
	    output_asm_insn ("mvn%?\t%0, %1", otherops);
	  else
	    output_asm_insn ("mov%?\t%0, %1", otherops);
	  output_mov_immediate (operands, FALSE, "");
	}
      else if (code1 == MEM)
	{
	  switch (GET_CODE (XEXP (operands[1], 0)))
	    {
	    case REG:
	      /* Handle the simple case where address is [r, #0] more
		 efficient.  */
	      output_asm_insn ("ldm%?ia\t%m1, %M0", operands);
	      break;
  	    case PRE_INC:
	      output_asm_insn ("add%?\t%m1, %m1, #8", operands);
	      output_asm_insn ("ldm%?ia\t%m1, %M0", operands);
	      break;
	    case PRE_DEC:
	      output_asm_insn ("sub%?\t%m1, %m1, #8", operands);
	      output_asm_insn ("ldm%?ia\t%m1, %M0", operands);
	      break;
	    case POST_INC:
	      output_asm_insn ("ldm%?ia\t%m1!, %M0", operands);
	      break;
	    case POST_DEC:
	      output_asm_insn ("ldm%?ia\t%m1, %M0", operands);
	      output_asm_insn ("sub%?\t%m1, %m1, #8", operands);
	      break;
	    default:
	      otherops[1] = adj_offsettable_operand (operands[1], 4);
	      /* Take care of overlapping base/data reg.  */
	      if (reg_mentioned_p (operands[0], operands[1]))
		{
		  output_asm_insn ("ldr%?\t%0, %1", otherops);
		  output_asm_insn ("ldr%?\t%0, %1", operands);
		}
	      else
		{
		  output_asm_insn ("ldr%?\t%0, %1", operands);
		  output_asm_insn ("ldr%?\t%0, %1", otherops);
		}
	    }
	}
      else abort();  /* Constraints should prevent this */
    }
  else if (code0 == MEM && code1 == REG)
    {
      if (REGNO (operands[1]) == 12)
	abort();
      switch (GET_CODE (XEXP (operands[0], 0)))
        {
	case REG:
	  output_asm_insn ("stm%?ia\t%m0, %M1", operands);
	  break;
        case PRE_INC:
	  output_asm_insn ("add%?\t%m0, %m0, #8", operands);
	  output_asm_insn ("stm%?ia\t%m0, %M1", operands);
	  break;
        case PRE_DEC:
	  output_asm_insn ("sub%?\t%m0, %m0, #8", operands);
	  output_asm_insn ("stm%?ia\t%m0, %M1", operands);
	  break;
        case POST_INC:
	  output_asm_insn ("stm%?ia\t%m0!, %M1", operands);
	  break;
        case POST_DEC:
	  output_asm_insn ("stm%?ia\t%m0, %M1", operands);
	  output_asm_insn ("sub%?\t%m0, %m0, #8", operands);
	  break;
        default:
	  otherops[0] = adj_offsettable_operand (operands[0], 4);
	  otherops[1] = gen_rtx (REG, SImode, 1 + REGNO (operands[1]));
	  output_asm_insn ("str%?\t%1, %0", operands);
	  output_asm_insn ("str%?\t%1, %0", otherops);
	}
    }
  else abort();  /* Constraints should prevent this */

  return "";
}


/* Output an arbitrary MOV reg, #n.
   OPERANDS[0] is a register.  OPERANDS[1] is a const_int.  */

char *
output_mov_immediate (operands)
     rtx *operands;
{
  HOST_WIDE_INT n = INTVAL (operands[1]);
  int n_ones = 0;
  int i;

  /* Try to use one MOV */
  if (const_ok_for_arm (n))
    {
      output_asm_insn ("mov%?\t%0, %1", operands);
      return "";
    }

  /* Try to use one MVN */
  if (const_ok_for_arm (~n))
    {
      operands[1] = GEN_INT (~n);
      output_asm_insn ("mvn%?\t%0, %1", operands);
      return "";
    }

  /* If all else fails, make it out of ORRs or BICs as appropriate. */

  for (i=0; i < 32; i++)
    if (n & 1 << i)
      n_ones++;

  if (n_ones > 16)  /* Shorter to use MVN with BIC in this case. */
    output_multi_immediate(operands, "mvn%?\t%0, %1", "bic%?\t%0, %0, %1", 1,
			   ~n);
  else
    output_multi_immediate(operands, "mov%?\t%0, %1", "orr%?\t%0, %0, %1", 1,
			   n);

  return "";
}


/* Output an ADD r, s, #n where n may be too big for one instruction.  If
   adding zero to one register, output nothing.  */

char *
output_add_immediate (operands)
     rtx *operands;
{
  HOST_WIDE_INT n = INTVAL (operands[2]);

  if (n != 0 || REGNO (operands[0]) != REGNO (operands[1]))
    {
      if (n < 0)
	output_multi_immediate (operands,
				"sub%?\t%0, %1, %2", "sub%?\t%0, %0, %2", 2,
				-n);
      else
	output_multi_immediate (operands,
				"add%?\t%0, %1, %2", "add%?\t%0, %0, %2", 2,
				n);
    }

  return "";
}

/* Output a multiple immediate operation.
   OPERANDS is the vector of operands referred to in the output patterns.
   INSTR1 is the output pattern to use for the first constant.
   INSTR2 is the output pattern to use for subsequent constants.
   IMMED_OP is the index of the constant slot in OPERANDS.
   N is the constant value.  */

char *
output_multi_immediate (operands, instr1, instr2, immed_op, n)
     rtx *operands;
     char *instr1, *instr2;
     int immed_op;
     HOST_WIDE_INT n;
{
#if HOST_BITS_PER_WIDE_INT > 32
  n &= 0xffffffff;
#endif

  if (n == 0)
    {
      operands[immed_op] = const0_rtx;
      output_asm_insn (instr1, operands); /* Quick and easy output */
    }
  else
    {
      int i;
      char *instr = instr1;

      /* Note that n is never zero here (which would give no output) */
      for (i = 0; i < 32; i += 2)
	{
	  if (n & (3 << i))
	    {
	      operands[immed_op] = GEN_INT (n & (255 << i));
	      output_asm_insn (instr, operands);
	      instr = instr2;
	      i += 6;
	    }
	}
    }
  return "";
}


/* Return the appropriate ARM instruction for the operation code.
   The returned result should not be overwritten.  OP is the rtx of the
   operation.  SHIFT_FIRST_ARG is TRUE if the first argument of the operator
   was shifted.  */

char *
arithmetic_instr (op, shift_first_arg)
     rtx op;
     int shift_first_arg;
{
  switch (GET_CODE (op))
    {
    case PLUS:
      return "add";

    case MINUS:
      return shift_first_arg ? "rsb" : "sub";

    case IOR:
      return "orr";

    case XOR:
      return "eor";

    case AND:
      return "and";

    default:
      abort ();
    }
}


/* Ensure valid constant shifts and return the appropriate shift mnemonic
   for the operation code.  The returned result should not be overwritten.
   OP is the rtx code of the shift.
   On exit, *AMOUNTP will be -1 if the shift is by a register, or a constant
   shift. */

static char *
shift_op (op, amountp)
     rtx op;
     HOST_WIDE_INT *amountp;
{
  char *mnem;
  enum rtx_code code = GET_CODE (op);

  if (GET_CODE (XEXP (op, 1)) == REG || GET_CODE (XEXP (op, 1)) == SUBREG)
    *amountp = -1;
  else if (GET_CODE (XEXP (op, 1)) == CONST_INT)
    *amountp = INTVAL (XEXP (op, 1));
  else
    abort ();

  switch (code)
    {
    case ASHIFT:
      mnem = "asl";
      break;

    case ASHIFTRT:
      mnem = "asr";
      break;

    case LSHIFTRT:
      mnem = "lsr";
      break;

    case ROTATERT:
      mnem = "ror";
      break;

    case MULT:
      /* We never have to worry about the amount being other than a
	 power of 2, since this case can never be reloaded from a reg.  */
      if (*amountp != -1)
	*amountp = int_log2 (*amountp);
      else
	abort ();
      return "asl";

    default:
      abort ();
    }

  if (*amountp != -1)
    {
      /* This is not 100% correct, but follows from the desire to merge
	 multiplication by a power of 2 with the recognizer for a
	 shift.  >=32 is not a valid shift for "asl", so we must try and
	 output a shift that produces the correct arithmetical result.
	 Using lsr #32 is idendical except for the fact that the carry bit
	 is not set correctly if we set the flags; but we never use the 
	 carry bit from such an operation, so we can ignore that.  */
      if (code == ROTATERT)
	*amountp &= 31;		/* Rotate is just modulo 32 */
      else if (*amountp != (*amountp & 31))
	{
	  if (code == ASHIFT)
	    mnem = "lsr";
	  *amountp = 32;
	}

      /* Shifts of 0 are no-ops.  */
      if (*amountp == 0)
	return NULL;
    }	  

  return mnem;
}


/* Obtain the shift from the POWER of two. */

HOST_WIDE_INT
int_log2 (power)
     HOST_WIDE_INT power;
{
  HOST_WIDE_INT shift = 0;

  while (((1 << shift) & power) == 0)
    {
      if (shift > 31)
	abort ();
      shift++;
    }

  return shift;
}

/* Output a .ascii pseudo-op, keeping track of lengths.  This is because
   /bin/as is horribly restrictive.  */

void
output_ascii_pseudo_op (stream, p, len)
     FILE *stream;
     unsigned char *p;
     int len;
{
  int i;
  int len_so_far = 1000;
  int chars_so_far = 0;

  for (i = 0; i < len; i++)
    {
      register int c = p[i];

      if (len_so_far > 50)
	{
	  if (chars_so_far)
	    fputs ("\"\n", stream);
	  fputs ("\t.ascii\t\"", stream);
	  len_so_far = 0;
	  arm_increase_location (chars_so_far);
	  chars_so_far = 0;
	}

      if (c == '\"' || c == '\\')
	{
	  putc('\\', stream);
	  len_so_far++;
	}

      if (c >= ' ' && c < 0177)
	{
	  putc (c, stream);
	  len_so_far++;
	}
      else
	{
	  fprintf (stream, "\\%03o", c);
	  len_so_far +=4;
	}

      chars_so_far++;
    }

  fputs ("\"\n", stream);
  arm_increase_location (chars_so_far);
}


/* Try to determine whether a pattern really clobbers the link register.
   This information is useful when peepholing, so that lr need not be pushed
   if we combine a call followed by a return.
   NOTE: This code does not check for side-effect expressions in a SET_SRC:
   such a check should not be needed because these only update an existing
   value within a register; the register must still be set elsewhere within
   the function. */

static int
pattern_really_clobbers_lr (x)
     rtx x;
{
  int i;
  
  switch (GET_CODE (x))
    {
    case SET:
      switch (GET_CODE (SET_DEST (x)))
	{
	case REG:
	  return REGNO (SET_DEST (x)) == 14;

        case SUBREG:
	  if (GET_CODE (XEXP (SET_DEST (x), 0)) == REG)
	    return REGNO (XEXP (SET_DEST (x), 0)) == 14;

	  if (GET_CODE (XEXP (SET_DEST (x), 0)) == MEM)
	    return 0;
	  abort ();

        default:
	  return 0;
        }

    case PARALLEL:
      for (i = 0; i < XVECLEN (x, 0); i++)
	if (pattern_really_clobbers_lr (XVECEXP (x, 0, i)))
	  return 1;
      return 0;

    case CLOBBER:
      switch (GET_CODE (XEXP (x, 0)))
        {
	case REG:
	  return REGNO (XEXP (x, 0)) == 14;

        case SUBREG:
	  if (GET_CODE (XEXP (XEXP (x, 0), 0)) == REG)
	    return REGNO (XEXP (XEXP (x, 0), 0)) == 14;
	  abort ();

        default:
	  return 0;
        }

    case UNSPEC:
      return 1;

    default:
      return 0;
    }
}

static int
function_really_clobbers_lr (first)
     rtx first;
{
  rtx insn, next;
  
  for (insn = first; insn; insn = next_nonnote_insn (insn))
    {
      switch (GET_CODE (insn))
        {
	case BARRIER:
	case NOTE:
	case CODE_LABEL:
	case JUMP_INSN:		/* Jump insns only change the PC (and conds) */
	case INLINE_HEADER:
	  break;

        case INSN:
	  if (pattern_really_clobbers_lr (PATTERN (insn)))
	    return 1;
	  break;

        case CALL_INSN:
	  /* Don't yet know how to handle those calls that are not to a 
	     SYMBOL_REF */
	  if (GET_CODE (PATTERN (insn)) != PARALLEL)
	    abort ();

	  switch (GET_CODE (XVECEXP (PATTERN (insn), 0, 0)))
	    {
	    case CALL:
	      if (GET_CODE (XEXP (XEXP (XVECEXP (PATTERN (insn), 0, 0), 0), 0))
		  != SYMBOL_REF)
		return 1;
	      break;

	    case SET:
	      if (GET_CODE (XEXP (XEXP (SET_SRC (XVECEXP (PATTERN (insn),
							  0, 0)), 0), 0))
		  != SYMBOL_REF)
		return 1;
	      break;

	    default:	/* Don't recognize it, be safe */
	      return 1;
	    }

	  /* A call can be made (by peepholing) not to clobber lr iff it is
	     followed by a return.  There may, however, be a use insn iff
	     we are returning the result of the call. 
	     If we run off the end of the insn chain, then that means the
	     call was at the end of the function.  Unfortunately we don't
	     have a return insn for the peephole to recognize, so we
	     must reject this.  (Can this be fixed by adding our own insn?) */
	  if ((next = next_nonnote_insn (insn)) == NULL)
	    return 1;

	  if (GET_CODE (next) == INSN && GET_CODE (PATTERN (next)) == USE
	      && (GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET)
	      && (REGNO (SET_DEST (XVECEXP (PATTERN (insn), 0, 0)))
		  == REGNO (XEXP (PATTERN (next), 0))))
	    if ((next = next_nonnote_insn (next)) == NULL)
	      return 1;

	  if (GET_CODE (next) == JUMP_INSN
	      && GET_CODE (PATTERN (next)) == RETURN)
	    break;
	  return 1;

        default:
	  abort ();
        }
    }

  /* We have reached the end of the chain so lr was _not_ clobbered */
  return 0;
}

char *
output_return_instruction (operand, really_return)
     rtx operand;
     int really_return;
{
  char instr[100];
  int reg, live_regs = 0;
  int volatile_func = (optimize > 0 
		       && TREE_THIS_VOLATILE (current_function_decl));

  return_used_this_function = 1;

  if (volatile_func)
    {
      rtx ops[2];
      /* If this function was declared non-returning, and we have found a tail 
	 call, then we have to trust that the called function won't return. */
      if (! really_return)
	return "";

      /* Otherwise, trap an attempted return by aborting. */
      ops[0] = operand;
      ops[1] = gen_rtx (SYMBOL_REF, Pmode, "abort");
      output_asm_insn ("bl%d0\t%a1", ops);
      return "";
    }
      
  if (current_function_calls_alloca && ! really_return)
    abort();
    
  for (reg = 0; reg <= 10; reg++)
    if (regs_ever_live[reg] && ! call_used_regs[reg])
      live_regs++;

  if (live_regs || (regs_ever_live[14] && ! lr_save_eliminated))
    live_regs++;

  if (frame_pointer_needed)
    live_regs += 4;

  if (live_regs)
    {
      if (lr_save_eliminated || ! regs_ever_live[14])
        live_regs++;

      if (frame_pointer_needed)
        strcpy (instr, "ldm%?%d0ea\t%|fp, {");
      else
        strcpy (instr, "ldm%?%d0fd\t%|sp!, {");

      for (reg = 0; reg <= 10; reg++)
        if (regs_ever_live[reg] && ! call_used_regs[reg])
          {
	    strcat (instr, "%|");
            strcat (instr, reg_names[reg]);
	    if (--live_regs)
              strcat (instr, ", ");
          }

      if (frame_pointer_needed)
        {
	  strcat (instr, "%|");
          strcat (instr, reg_names[11]);
          strcat (instr, ", ");
	  strcat (instr, "%|");
          strcat (instr, reg_names[13]);
          strcat (instr, ", ");
	  strcat (instr, "%|");
          strcat (instr, really_return ? reg_names[15] : reg_names[14]);
        }
      else
	{
	  strcat (instr, "%|");
	  strcat (instr, really_return ? reg_names[15] : reg_names[14]);
	}
      strcat (instr, (TARGET_6 || !really_return) ? "}" : "}^");
      output_asm_insn (instr, &operand);
    }
  else if (really_return)
    {
      strcpy (instr,
	      TARGET_6 ? "mov%?%d0\t%|pc, lr" : "mov%?%d0s\t%|pc, %|lr");
      output_asm_insn (instr, &operand);
    }

  return "";
}

int
arm_volatile_func ()
{
  return (optimize > 0 && TREE_THIS_VOLATILE (current_function_decl));
}

/* Return the size of the prologue.  It's not too bad if we slightly 
   over-estimate.  */

static int
get_prologue_size ()
{
  return profile_flag ? 12 : 0;
}

/* The amount of stack adjustment that happens here, in output_return and in
   output_epilogue must be exactly the same as was calculated during reload,
   or things will point to the wrong place.  The only time we can safely
   ignore this constraint is when a function has no arguments on the stack,
   no stack frame requirement and no live registers execpt for `lr'.  If we
   can guarantee that by making all function calls into tail calls and that
   lr is not clobbered in any other way, then there is no need to push lr
   onto the stack. */
   
void
output_func_prologue (f, frame_size)
     FILE *f;
     int frame_size;
{
  int reg, live_regs_mask = 0;
  rtx operands[3];
  int volatile_func = (optimize > 0
		       && TREE_THIS_VOLATILE (current_function_decl));

  /* Nonzero if we must stuff some register arguments onto the stack as if
     they were passed there.  */
  int store_arg_regs = 0;

  if (arm_ccfsm_state || arm_target_insn)
    abort ();					/* Sanity check */
  
  return_used_this_function = 0;
  lr_save_eliminated = 0;
  
  fprintf (f, "\t%c args = %d, pretend = %d, frame = %d\n",
	   ARM_COMMENT_CHAR, current_function_args_size,
	   current_function_pretend_args_size, frame_size);
  fprintf (f, "\t%c frame_needed = %d, current_function_anonymous_args = %d\n",
	   ARM_COMMENT_CHAR, frame_pointer_needed,
	   current_function_anonymous_args);

  if (volatile_func)
    fprintf (f, "\t%c Volatile function.\n", ARM_COMMENT_CHAR);

  if (current_function_anonymous_args && current_function_pretend_args_size)
    store_arg_regs = 1;

  for (reg = 0; reg <= 10; reg++)
    if (regs_ever_live[reg] && ! call_used_regs[reg])
      live_regs_mask |= (1 << reg);

  if (frame_pointer_needed)
    live_regs_mask |= 0xD800;
  else if (regs_ever_live[14])
    {
      if (! current_function_args_size
	  && ! function_really_clobbers_lr (get_insns ()))
	lr_save_eliminated = 1;
      else
        live_regs_mask |= 0x4000;
    }

  if (live_regs_mask)
    {
      /* if a di mode load/store multiple is used, and the base register
	 is r3, then r4 can become an ever live register without lr
	 doing so,  in this case we need to push lr as well, or we
	 will fail to get a proper return. */

      live_regs_mask |= 0x4000;
      lr_save_eliminated = 0;

    }

  if (lr_save_eliminated)
    fprintf (f,"\t%c I don't think this function clobbers lr\n",
	     ARM_COMMENT_CHAR);
}


void
output_func_epilogue (f, frame_size)
     FILE *f;
     int frame_size;
{
  int reg, live_regs_mask = 0, code_size = 0;
  /* If we need this then it will always be at lesat this much */
  int floats_offset = 24;
  rtx operands[3];
  int volatile_func = (optimize > 0
		       && TREE_THIS_VOLATILE (current_function_decl));

  if (use_return_insn() && return_used_this_function)
    {
      if (frame_size && !(frame_pointer_needed || TARGET_APCS))
        {
          abort ();
        }
      goto epilogue_done;
    }

  /* A volatile function should never return.  Call abort.  */
  if (volatile_func)
    {
      rtx op = gen_rtx (SYMBOL_REF, Pmode, "abort");
      output_asm_insn ("bl\t%a0", &op);
      code_size = 4;
      goto epilogue_done;
    }

  for (reg = 0; reg <= 10; reg++)
    if (regs_ever_live[reg] && ! call_used_regs[reg])
      {
        live_regs_mask |= (1 << reg);
	floats_offset += 4;
      }

  if (frame_pointer_needed)
    {
      for (reg = 23; reg > 15; reg--)
	if (regs_ever_live[reg] && ! call_used_regs[reg])
	  {
	    fprintf (f, "\tldfe\t%s%s, [%sfp, #-%d]\n", ARM_REG_PREFIX,
		     reg_names[reg], ARM_REG_PREFIX, floats_offset);
	    floats_offset += 12;
	    code_size += 4;
	  }

      live_regs_mask |= 0xA800;
      print_multi_reg (f, "ldmea\t%sfp", live_regs_mask,
		       TARGET_6 ? FALSE : TRUE);
      code_size += 4;
    }
  else
    {
      /* Restore stack pointer if necessary.  */
      if (frame_size)
	{
	  operands[0] = operands[1] = stack_pointer_rtx;
	  operands[2] = gen_rtx (CONST_INT, VOIDmode, frame_size);
	  output_add_immediate (operands);
	}

      for (reg = 16; reg < 24; reg++)
	if (regs_ever_live[reg] && ! call_used_regs[reg])
	  {
	    fprintf (f, "\tldfe\t%s%s, [%ssp], #12\n", ARM_REG_PREFIX,
		     reg_names[reg], ARM_REG_PREFIX);
	    code_size += 4;
	  }
      if (current_function_pretend_args_size == 0 && regs_ever_live[14])
	{
	  print_multi_reg (f, "ldmfd\t%ssp!", live_regs_mask | 0x8000,
			   TARGET_6 ? FALSE : TRUE);
	  code_size += 4;
	}
      else
	{
	  if (live_regs_mask || regs_ever_live[14])
	    {
	      live_regs_mask |= 0x4000;
	      print_multi_reg (f, "ldmfd\t%ssp!", live_regs_mask, FALSE);
	      code_size += 4;
	    }
	  if (current_function_pretend_args_size)
	    {
	      operands[0] = operands[1] = stack_pointer_rtx;
	      operands[2] = gen_rtx (CONST_INT, VOIDmode,
				     current_function_pretend_args_size);
	      output_add_immediate (operands);
	    }
	  fprintf (f,
		   TARGET_6 ? "\tmov\t%spc, %slr\n" : "\tmovs\t%spc, %slr\n",
		   ARM_REG_PREFIX, ARM_REG_PREFIX, f);
	  code_size += 4;
	}
    }

 epilogue_done:

  /* insn_addresses isn't allocated when not optimizing */

  if (optimize > 0)
    arm_increase_location (code_size
			   + insn_addresses[INSN_UID (get_last_insn ())]
			   + get_prologue_size ());

  current_function_anonymous_args = 0;
}

static void
emit_multi_reg_push (mask)
     int mask;
{
  int num_regs = 0;
  int i, j;
  rtx par;

  for (i = 0; i < 16; i++)
    if (mask & (1 << i))
      num_regs++;

  if (num_regs == 0 || num_regs > 16)
    abort ();

  par = gen_rtx (PARALLEL, VOIDmode, rtvec_alloc (num_regs));

  for (i = 0; i < 16; i++)
    {
      if (mask & (1 << i))
	{
	  XVECEXP (par, 0, 0)
	    = gen_rtx (SET, VOIDmode, gen_rtx (MEM, BLKmode,
					       gen_rtx (PRE_DEC, BLKmode,
							stack_pointer_rtx)),
		       gen_rtx (UNSPEC, BLKmode,
				gen_rtvec (1, gen_rtx (REG, SImode, i)),
				2));
	  break;
	}
    }

  for (j = 1, i++; j < num_regs; i++)
    {
      if (mask & (1 << i))
	{
	  XVECEXP (par, 0, j)
	    = gen_rtx (USE, VOIDmode, gen_rtx (REG, SImode, i));
	  j++;
	}
    }
  emit_insn (par);
}

void
arm_expand_prologue ()
{
  int reg;
  rtx amount = GEN_INT (- get_frame_size ());
  rtx push_insn;
  int num_regs;
  int live_regs_mask = 0;
  int store_arg_regs = 0;
  int volatile_func = (optimize > 0
		       && TREE_THIS_VOLATILE (current_function_decl));

  if (current_function_anonymous_args && current_function_pretend_args_size)
    store_arg_regs = 1;

  if (! volatile_func)
    for (reg = 0; reg <= 10; reg++)
      if (regs_ever_live[reg] && ! call_used_regs[reg])
	live_regs_mask |= 1 << reg;

  if (! volatile_func && regs_ever_live[14])
    live_regs_mask |= 0x4000;

  if (frame_pointer_needed)
    {
      live_regs_mask |= 0xD800;
      emit_insn (gen_movsi (gen_rtx (REG, SImode, 12),
			    stack_pointer_rtx));
    }

  if (current_function_pretend_args_size)
    {
      if (store_arg_regs)
	emit_multi_reg_push ((0xf0 >> (current_function_pretend_args_size / 4))
			     & 0xf);
      else
	emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, 
			       GEN_INT (-current_function_pretend_args_size)));
    }

  if (live_regs_mask)
    {
      /* If we have to push any regs, then we must push lr as well, or
	 we won't get a propper return.  */
      live_regs_mask |= 0x4000;
      emit_multi_reg_push (live_regs_mask);
    }
      
  /* For now the integer regs are still pushed in output_func_epilogue ().  */

  if (! volatile_func)
    for (reg = 23; reg > 15; reg--)
      if (regs_ever_live[reg] && ! call_used_regs[reg])
	emit_insn (gen_rtx (SET, VOIDmode, 
			    gen_rtx (MEM, XFmode, 
				     gen_rtx (PRE_DEC, XFmode,
					      stack_pointer_rtx)),
			    gen_rtx (REG, XFmode, reg)));

  if (frame_pointer_needed)
    emit_insn (gen_addsi3 (hard_frame_pointer_rtx, gen_rtx (REG, SImode, 12),
			   (GEN_INT
			    (-(4 + current_function_pretend_args_size)))));

  if (amount != const0_rtx)
    {
      emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, amount));
      emit_insn (gen_rtx (CLOBBER, VOIDmode, 
			  gen_rtx (MEM, BLKmode, stack_pointer_rtx)));
    }

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  */
  if (profile_flag || profile_block_flag)
    emit_insn (gen_blockage ());
}
  

/* If CODE is 'd', then the X is a condition operand and the instruction
   should only be executed if the condition is true.
   if CODE is 'D', then the X is a condition operand and the instruciton
   should only be executed if the condition is false: however, if the mode
   of the comparison is CCFPEmode, then always execute the instruction -- we
   do this because in these circumstances !GE does not necessarily imply LT;
   in these cases the instruction pattern will take care to make sure that
   an instruction containing %d will follow, thereby undoing the effects of
   doing this instrucion unconditionally.
   If CODE is 'N' then X is a floating point operand that must be negated
   before output.
   If CODE is 'B' then output a bitwise inverted value of X (a const int).
   If X is a REG and CODE is `M', output a ldm/stm style multi-reg.  */

void
arm_print_operand (stream, x, code)
     FILE *stream;
     rtx x;
     int code;
{
  switch (code)
    {
    case '@':
      fputc (ARM_COMMENT_CHAR, stream);
      return;

    case '|':
      fputs (ARM_REG_PREFIX, stream);
      return;

    case '?':
      if (arm_ccfsm_state == 3 || arm_ccfsm_state == 4)
	fputs (arm_condition_codes[arm_current_cc], stream);
      return;

    case 'N':
      {
	REAL_VALUE_TYPE r;
	REAL_VALUE_FROM_CONST_DOUBLE (r, x);
	r = REAL_VALUE_NEGATE (r);
	fprintf (stream, "%s", fp_const_from_val (&r));
      }
      return;

    case 'B':
      if (GET_CODE (x) == CONST_INT)
	fprintf (stream,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		 "%d",
#else
		 "%ld",
#endif
		 ARM_SIGN_EXTEND (~ INTVAL (x)));
      else
	{
	  putc ('~', stream);
	  output_addr_const (stream, x);
	}
      return;

    case 'i':
      fprintf (stream, "%s", arithmetic_instr (x, 1));
      return;

    case 'I':
      fprintf (stream, "%s", arithmetic_instr (x, 0));
      return;

    case 'S':
      {
	HOST_WIDE_INT val;
	char *shift = shift_op (x, &val);

	if (shift)
	  {
	    fprintf (stream, ", %s ", shift_op (x, &val));
	    if (val == -1)
	      arm_print_operand (stream, XEXP (x, 1), 0);
	    else
	      fprintf (stream,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		       "#%d",
#else
		       "#%ld",
#endif
		       val);
	  }
      }
      return;

    case 'R':
      if (REGNO (x) > 15)
	abort ();
      fputs (ARM_REG_PREFIX, stream);
      fputs (reg_names[REGNO (x) + 1], stream);
      return;

    case 'm':
      fputs (ARM_REG_PREFIX, stream);
      if (GET_CODE (XEXP (x, 0)) == REG)
	fputs (reg_names[REGNO (XEXP (x, 0))], stream);
      else
	fputs (reg_names[REGNO (XEXP (XEXP (x, 0), 0))], stream);
      return;

    case 'M':
      fprintf (stream, "{%s%s-%s%s}", ARM_REG_PREFIX, reg_names[REGNO (x)],
	       ARM_REG_PREFIX, reg_names[REGNO (x) - 1
					 + ((GET_MODE_SIZE (GET_MODE (x))
					     + GET_MODE_SIZE (SImode) - 1)
					    / GET_MODE_SIZE (SImode))]);
      return;

    case 'd':
      if (x)
        fputs (arm_condition_codes[get_arm_condition_code (x)],
	       stream);
      return;

    case 'D':
      if (x && (flag_fast_math
		|| GET_CODE (x) == EQ || GET_CODE (x) == NE
		|| (GET_MODE (XEXP (x, 0)) != CCFPEmode
		    && (GET_MODE_CLASS (GET_MODE (XEXP (x, 0)))
			!= MODE_FLOAT))))
        fputs (arm_condition_codes[ARM_INVERSE_CONDITION_CODE
				   (get_arm_condition_code (x))],
	       stream);
      return;

    default:
      if (x == 0)
	abort ();

      if (GET_CODE (x) == REG)
	{
	  fputs (ARM_REG_PREFIX, stream);
	  fputs (reg_names[REGNO (x)], stream);
	}
      else if (GET_CODE (x) == MEM)
	{
	  output_memory_reference_mode = GET_MODE (x);
	  output_address (XEXP (x, 0));
	}
      else if (GET_CODE (x) == CONST_DOUBLE)
	fprintf (stream, "#%s", fp_immediate_constant (x));
      else if (GET_CODE (x) == NEG)
	abort (); /* This should never happen now. */
      else
	{
	  fputc ('#', stream);
	  output_addr_const (stream, x);
	}
    }
}

/* Increase the `arm_text_location' by AMOUNT if we're in the text
   segment.  */

void
arm_increase_location (amount)
     int amount;
{
  if (in_text_section ())
    arm_text_location += amount;
}


/* Output a label definition.  If this label is within the .text segment, it
   is stored in OFFSET_TABLE, to be used when building `llc' instructions.
   Maybe GCC remembers names not starting with a `*' for a long time, but this
   is a minority anyway, so we just make a copy.  Do not store the leading `*'
   if the name starts with one.  */

void
arm_asm_output_label (stream, name)
     FILE *stream;
     char *name;
{
  char *real_name, *s;
  struct label_offset *cur;
  int hash = 0;

  assemble_name (stream, name);
  fputs (":\n", stream);
  if (! in_text_section ())
    return;

  if (name[0] == '*')
    {
      real_name = xmalloc (1 + strlen (&name[1]));
      strcpy (real_name, &name[1]);
    }
  else
    {
      real_name = xmalloc (2 + strlen (name));
      strcpy (real_name, "_");
      strcat (real_name, name);
    }
  for (s = real_name; *s; s++)
    hash += *s;

  hash = hash % LABEL_HASH_SIZE;
  cur = (struct label_offset *) xmalloc (sizeof (struct label_offset));
  cur->name = real_name;
  cur->offset = arm_text_location;
  cur->cdr = offset_table[hash];
  offset_table[hash] = cur;
}

/* Load a symbol that is known to be in the text segment into a register.
   This should never be called when not optimizing.  */

char *
output_load_symbol (insn, operands)
     rtx insn;
     rtx *operands;
{
  char *s;
  char *name = XSTR (operands[1], 0);
  struct label_offset *he;
  int hash = 0;
  int offset;
  unsigned int mask, never_mask = 0xffffffff;
  int shift, inst;
  char buffer[100];

  if (optimize == 0 || *name != '*')
    abort ();

  for (s = &name[1]; *s; s++)
    hash += *s;

  hash = hash % LABEL_HASH_SIZE;
  he = offset_table[hash];
  while (he && strcmp (he->name, &name[1]))
    he = he->cdr;
  
  if (!he)
    abort ();
  
  offset = (arm_text_location + insn_addresses[INSN_UID (insn)]
	    + get_prologue_size () + 8 - he->offset);
  if (offset < 0)
    abort ();

  /* When generating the instructions, we never mask out the bits that we
     think will be always zero, then if a mistake has occured somewhere, the
     assembler will spot it and generate an error.  */

  /* If the symbol is word aligned then we might be able to reduce the
     number of loads.  */
  shift = ((offset & 3) == 0) ? 2 : 0;

  /* Clear the bits from NEVER_MASK that will be orred in with the individual
     instructions.  */
  for (; shift < 32; shift += 8)
    {
      mask = 0xff << shift;
      if ((offset & mask) || ((unsigned) offset) > mask)
	never_mask &= ~mask;
    }

  inst = 8;
  mask = 0xff << (shift - 32);

  while (mask && (never_mask & mask) == 0)
    {
      if (inst == 8)
	{
	  strcpy (buffer, "sub%?\t%0, %|pc, #(8 + . -%a1)");
	  if ((never_mask | mask) != 0xffffffff)
	    sprintf (buffer + strlen (buffer), " & 0x%x", mask | never_mask);
	}
      else
	sprintf (buffer, "sub%%?\t%%0, %%0, #(%d + . -%%a1) & 0x%x",
		 inst, mask | never_mask);

      output_asm_insn (buffer, operands);
      mask <<= 8;
      inst -= 4;
    }

  return "";
}

/* Output code resembling an .lcomm directive.  /bin/as doesn't have this
   directive hence this hack, which works by reserving some `.space' in the
   bss segment directly.

   XXX This is a severe hack, which is guaranteed NOT to work since it doesn't
   define STATIC COMMON space but merely STATIC BSS space.  */

void
output_lcomm_directive (stream, name, size, rounded)
     FILE *stream;
     char *name;
     int size, rounded;
{
  fprintf (stream, "\n\t.bss\t%c .lcomm\n", ARM_COMMENT_CHAR);
  assemble_name (stream, name);
  fprintf (stream, ":\t.space\t%d\n", rounded);
  if (in_text_section ())
    fputs ("\n\t.text\n", stream);
  else
    fputs ("\n\t.data\n", stream);
}

/* A finite state machine takes care of noticing whether or not instructions
   can be conditionally executed, and thus decrease execution time and code
   size by deleting branch instructions.  The fsm is controlled by
   final_prescan_insn, and controls the actions of ASM_OUTPUT_OPCODE.  */

/* The state of the fsm controlling condition codes are:
   0: normal, do nothing special
   1: make ASM_OUTPUT_OPCODE not output this instruction
   2: make ASM_OUTPUT_OPCODE not output this instruction
   3: make instructions conditional
   4: make instructions conditional

   State transitions (state->state by whom under condition):
   0 -> 1 final_prescan_insn if the `target' is a label
   0 -> 2 final_prescan_insn if the `target' is an unconditional branch
   1 -> 3 ASM_OUTPUT_OPCODE after not having output the conditional branch
   2 -> 4 ASM_OUTPUT_OPCODE after not having output the conditional branch
   3 -> 0 ASM_OUTPUT_INTERNAL_LABEL if the `target' label is reached
          (the target label has CODE_LABEL_NUMBER equal to arm_target_label).
   4 -> 0 final_prescan_insn if the `target' unconditional branch is reached
          (the target insn is arm_target_insn).

   If the jump clobbers the conditions then we use states 2 and 4.

   A similar thing can be done with conditional return insns.

   XXX In case the `target' is an unconditional branch, this conditionalising
   of the instructions always reduces code size, but not always execution
   time.  But then, I want to reduce the code size to somewhere near what
   /bin/cc produces.  */

/* Returns the index of the ARM condition code string in
   `arm_condition_codes'.  COMPARISON should be an rtx like
   `(eq (...) (...))'.  */

int
get_arm_condition_code (comparison)
     rtx comparison;
{
  switch (GET_CODE (comparison))
    {
    case NE: return (1);
    case EQ: return (0);
    case GE: return (10);
    case GT: return (12);
    case LE: return (13);
    case LT: return (11);
    case GEU: return (2);
    case GTU: return (8);
    case LEU: return (9);
    case LTU: return (3);
    default: abort ();
    }
  /*NOTREACHED*/
  return (42);
}


void
final_prescan_insn (insn, opvec, noperands)
     rtx insn;
     rtx *opvec;
     int noperands;
{
  /* BODY will hold the body of INSN.  */
  register rtx body = PATTERN (insn);

  /* This will be 1 if trying to repeat the trick, and things need to be
     reversed if it appears to fail.  */
  int reverse = 0;

  /* JUMP_CLOBBERS will be one implies that the conditions if a branch is
     taken are clobbered, even if the rtl suggests otherwise.  It also
     means that we have to grub around within the jump expression to find
     out what the conditions are when the jump isn't taken.  */
  int jump_clobbers = 0;
  
  /* If we start with a return insn, we only succeed if we find another one. */
  int seeking_return = 0;
  
  /* START_INSN will hold the insn from where we start looking.  This is the
     first insn after the following code_label if REVERSE is true.  */
  rtx start_insn = insn;

  /* If in state 4, check if the target branch is reached, in order to
     change back to state 0.  */
  if (arm_ccfsm_state == 4)
    {
      if (insn == arm_target_insn)
      {
	arm_target_insn = NULL;
	arm_ccfsm_state = 0;
      }
      return;
    }

  /* If in state 3, it is possible to repeat the trick, if this insn is an
     unconditional branch to a label, and immediately following this branch
     is the previous target label which is only used once, and the label this
     branch jumps to is not too far off.  */
  if (arm_ccfsm_state == 3)
    {
      if (simplejump_p (insn))
	{
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    {
	      /* XXX Isn't this always a barrier?  */
	      start_insn = next_nonnote_insn (start_insn);
	    }
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == arm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    reverse = TRUE;
	  else
	    return;
	}
      else if (GET_CODE (body) == RETURN)
        {
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == arm_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    {
	      reverse = TRUE;
	      seeking_return = 1;
	    }
	  else
	    return;
        }
      else
	return;
    }

  if (arm_ccfsm_state != 0 && !reverse)
    abort ();
  if (GET_CODE (insn) != JUMP_INSN)
    return;

  /* This jump might be paralled with a clobber of the condition codes 
     the jump should always come first */
  if (GET_CODE (body) == PARALLEL && XVECLEN (body, 0) > 0)
    body = XVECEXP (body, 0, 0);

#if 0  
  /* If this is a conditional return then we don't want to know */
  if (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
      && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE
      && (GET_CODE (XEXP (SET_SRC (body), 1)) == RETURN
          || GET_CODE (XEXP (SET_SRC (body), 2)) == RETURN))
    return;
#endif

  if (reverse
      || (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
	  && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE))
    {
      int insns_skipped = 0, fail = FALSE, succeed = FALSE;
      /* Flag which part of the IF_THEN_ELSE is the LABEL_REF.  */
      int then_not_else = TRUE;
      rtx this_insn = start_insn, label = 0;

      if (get_attr_conds (insn) == CONDS_JUMP_CLOB)
	{
	  /* The code below is wrong for these, and I haven't time to
	     fix it now.  So we just do the safe thing and return.  This
	     whole function needs re-writing anyway.  */
	  jump_clobbers = 1;
	  return;
	}
      
      /* Register the insn jumped to.  */
      if (reverse)
        {
	  if (!seeking_return)
	    label = XEXP (SET_SRC (body), 0);
        }
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == LABEL_REF)
	label = XEXP (XEXP (SET_SRC (body), 1), 0);
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == LABEL_REF)
	{
	  label = XEXP (XEXP (SET_SRC (body), 2), 0);
	  then_not_else = FALSE;
	}
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == RETURN)
	seeking_return = 1;
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == RETURN)
        {
	  seeking_return = 1;
	  then_not_else = FALSE;
        }
      else
	abort ();

      /* See how many insns this branch skips, and what kind of insns.  If all
	 insns are okay, and the label or unconditional branch to the same
	 label is not too far away, succeed.  */
      for (insns_skipped = 0;
	   !fail && !succeed && insns_skipped < MAX_INSNS_SKIPPED;
	   insns_skipped++)
	{
	  rtx scanbody;

	  this_insn = next_nonnote_insn (this_insn);
	  if (!this_insn)
	    break;

	  scanbody = PATTERN (this_insn);

	  switch (GET_CODE (this_insn))
	    {
	    case CODE_LABEL:
	      /* Succeed if it is the target label, otherwise fail since
		 control falls in from somewhere else.  */
	      if (this_insn == label)
		{
		  if (jump_clobbers)
		    {
		      arm_ccfsm_state = 2;
		      this_insn = next_nonnote_insn (this_insn);
		    }
		  else
		    arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case BARRIER:
	      /* Succeed if the following insn is the target label.
		 Otherwise fail.  
		 If return insns are used then the last insn in a function 
		 will be a barrier. */
	      this_insn = next_nonnote_insn (this_insn);
	      if (this_insn && this_insn == label)
		{
		  if (jump_clobbers)
		    {
		      arm_ccfsm_state = 2;
		      this_insn = next_nonnote_insn (this_insn);
		    }
		  else
		    arm_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case CALL_INSN:
	      /* The arm 6xx uses full 32 bit addresses so the cc is not 
		 preserved over calls */
	      if (TARGET_6)
		fail = TRUE;
	      break;
	    case JUMP_INSN:
      	      /* If this is an unconditional branch to the same label, succeed.
		 If it is to another label, do nothing.  If it is conditional,
		 fail.  */
	      /* XXX Probably, the test for the SET and the PC are unnecessary. */

	      if (GET_CODE (scanbody) == SET
		  && GET_CODE (SET_DEST (scanbody)) == PC)
		{
		  if (GET_CODE (SET_SRC (scanbody)) == LABEL_REF
		      && XEXP (SET_SRC (scanbody), 0) == label && !reverse)
		    {
		      arm_ccfsm_state = 2;
		      succeed = TRUE;
		    }
		  else if (GET_CODE (SET_SRC (scanbody)) == IF_THEN_ELSE)
		    fail = TRUE;
		}
	      else if (GET_CODE (scanbody) == RETURN
		       && seeking_return)
	        {
		  arm_ccfsm_state = 2;
		  succeed = TRUE;
	        }
	      else if (GET_CODE (scanbody) == PARALLEL)
	        {
		  switch (get_attr_conds (this_insn))
		    {
		    case CONDS_NOCOND:
		      break;
		    default:
		      fail = TRUE;
		      break;
		    }
		}
	      break;

	    case INSN:
	      /* Instructions using or affecting the condition codes make it
		 fail.  */
	      if ((GET_CODE (scanbody) == SET
		   || GET_CODE (scanbody) == PARALLEL)
		  && get_attr_conds (this_insn) != CONDS_NOCOND)
		fail = TRUE;
	      break;

	    default:
	      break;
	    }
	}
      if (succeed)
	{
	  if ((!seeking_return) && (arm_ccfsm_state == 1 || reverse))
	    arm_target_label = CODE_LABEL_NUMBER (label);
	  else if (seeking_return || arm_ccfsm_state == 2)
	    {
	      while (this_insn && GET_CODE (PATTERN (this_insn)) == USE)
	        {
		  this_insn = next_nonnote_insn (this_insn);
		  if (this_insn && (GET_CODE (this_insn) == BARRIER
				    || GET_CODE (this_insn) == CODE_LABEL))
		    abort ();
	        }
	      if (!this_insn)
	        {
		  /* Oh, dear! we ran off the end.. give up */
		  recog (PATTERN (insn), insn, NULL_PTR);
		  arm_ccfsm_state = 0;
		  arm_target_insn = NULL;
		  return;
	        }
	      arm_target_insn = this_insn;
	    }
	  else
	    abort ();
	  if (jump_clobbers)
	    {
	      if (reverse)
		abort ();
	      arm_current_cc = 
		  get_arm_condition_code (XEXP (XEXP (XEXP (SET_SRC (body),
							    0), 0), 1));
	      if (GET_CODE (XEXP (XEXP (SET_SRC (body), 0), 0)) == AND)
		arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	      if (GET_CODE (XEXP (SET_SRC (body), 0)) == NE)
		arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	    }
	  else
	    {
	      /* If REVERSE is true, ARM_CURRENT_CC needs to be inverted from
		 what it was.  */
	      if (!reverse)
		arm_current_cc = get_arm_condition_code (XEXP (SET_SRC (body),
							       0));
	    }

	  if (reverse || then_not_else)
	    arm_current_cc = ARM_INVERSE_CONDITION_CODE (arm_current_cc);
	}
      /* restore recog_operand (getting the attributes of other insns can
	 destroy this array, but final.c assumes that it remains intact
	 accross this call; since the insn has been recognized already we
	 call recog direct). */
      recog (PATTERN (insn), insn, NULL_PTR);
    }
}

/* EOF */
