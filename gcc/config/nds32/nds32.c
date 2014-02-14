/* Subroutines used for code generation of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */



#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"	/* Required by recog.h.  */
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"		/* For DFA state_t.  */
#include "insn-codes.h"		/* For CODE_FOR_xxx.  */
#include "reload.h"		/* For push_reload().  */
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "df.h"
#include "tm_p.h"
#include "tm-constrs.h"
#include "optabs.h"		/* For GEN_FCN.  */
#include "target.h"
#include "target-def.h"
#include "langhooks.h"		/* For add_builtin_function().  */
#include "ggc.h"

/* ------------------------------------------------------------------------ */

/* This file is divided into five parts:

     PART 1: Auxiliary static variable definitions and
             target hook static variable definitions.

     PART 2: Auxiliary static function definitions.

     PART 3: Implement target hook stuff definitions.

     PART 4: Implemet extern function definitions,
             the prototype is in nds32-protos.h.

     PART 5: Initialize target hook structure and definitions.  */

/* ------------------------------------------------------------------------ */

/* PART 1: Auxiliary static variable definitions and
           target hook static variable definitions.  */

/* Refer to nds32.h, there are maximum 73 isr vectors in nds32 architecture.
   0 for reset handler with __attribute__((reset())),
   1-8 for exception handler with __attribute__((exception(1,...,8))),
   and 9-72 for interrupt handler with __attribute__((interrupt(0,...,63))).
   We use an array to record essential information for each vector.  */
static struct nds32_isr_info nds32_isr_vectors[NDS32_N_ISR_VECTORS];

/* Define intrinsic register names.
   Please refer to nds32_intrinsic.h file, the index is corresponding to
   'enum nds32_intrinsic_registers' data type values.
   NOTE that the base value starting from 1024.  */
static const char * const nds32_intrinsic_register_names[] =
{
  "$PSW", "$IPSW", "$ITYPE", "$IPC"
};

/* Defining target-specific uses of __attribute__.  */
static const struct attribute_spec nds32_attribute_table[] =
{
  /* Syntax: { name, min_len, max_len, decl_required, type_required,
               function_type_required, handler, affects_type_identity } */

  /* The interrupt vid: [0-63]+ (actual vector number starts from 9 to 72).  */
  { "interrupt",    1, 64, false, false, false, NULL, false },
  /* The exception vid: [1-8]+  (actual vector number starts from 1 to 8).  */
  { "exception",    1,  8, false, false, false, NULL, false },
  /* Argument is user's interrupt numbers.  The vector number is always 0.  */
  { "reset",        1,  1, false, false, false, NULL, false },

  /* The attributes describing isr nested type.  */
  { "nested",       0,  0, false, false, false, NULL, false },
  { "not_nested",   0,  0, false, false, false, NULL, false },
  { "nested_ready", 0,  0, false, false, false, NULL, false },

  /* The attributes describing isr register save scheme.  */
  { "save_all",     0,  0, false, false, false, NULL, false },
  { "partial_save", 0,  0, false, false, false, NULL, false },

  /* The attributes used by reset attribute.  */
  { "nmi",          1,  1, false, false, false, NULL, false },
  { "warm",         1,  1, false, false, false, NULL, false },

  /* The attribute telling no prologue/epilogue.  */
  { "naked",        0,  0, false, false, false, NULL, false },

  /* The last attribute spec is set to be NULL.  */
  { NULL,           0,  0, false, false, false, NULL, false }
};


/* ------------------------------------------------------------------------ */

/* PART 2: Auxiliary static function definitions.  */

/* Function to save and restore machine-specific function data.  */
static struct machine_function *
nds32_init_machine_status (void)
{
  struct machine_function *machine;
  machine = ggc_alloc_cleared_machine_function ();

  /* Initially assume this function needs prologue/epilogue.  */
  machine->naked_p = 0;

  /* Initially assume this function does NOT use fp_as_gp optimization.  */
  machine->fp_as_gp_p = 0;

  return machine;
}

/* Function to compute stack frame size and
   store into cfun->machine structure.  */
static void
nds32_compute_stack_frame (void)
{
  int r;
  int block_size;

  /* Because nds32_compute_stack_frame() will be called from different place,
     everytime we enter this function, we have to assume this function
     needs prologue/epilogue.  */
  cfun->machine->naked_p = 0;

  /* Get variadic arguments size to prepare pretend arguments and
     push them into stack at prologue.
     Currently, we do not push variadic arguments by ourself.
     We have GCC handle all the works.
     The caller will push all corresponding nameless arguments into stack,
     and the callee is able to retrieve them without problems.
     These variables are still preserved in case one day
     we would like caller passing arguments with registers.  */
  cfun->machine->va_args_size = 0;
  cfun->machine->va_args_first_regno = SP_REGNUM;
  cfun->machine->va_args_last_regno  = SP_REGNUM;

  /* Get local variables, incoming variables, and temporary variables size.
     Note that we need to make sure it is 8-byte alignment because
     there may be no padding bytes if we are using LRA.  */
  cfun->machine->local_size = NDS32_ROUND_UP_DOUBLE_WORD (get_frame_size ());

  /* Get outgoing arguments size.  */
  cfun->machine->out_args_size = crtl->outgoing_args_size;

  /* If $fp value is required to be saved on stack, it needs 4 bytes space.
     Check whether $fp is ever live.  */
  cfun->machine->fp_size = (df_regs_ever_live_p (FP_REGNUM)) ? 4 : 0;

  /* If $gp value is required to be saved on stack, it needs 4 bytes space.
     Check whether we are using PIC code genration.  */
  cfun->machine->gp_size = (flag_pic) ? 4 : 0;

  /* If $lp value is required to be saved on stack, it needs 4 bytes space.
     Check whether $lp is ever live.  */
  cfun->machine->lp_size = (df_regs_ever_live_p (LP_REGNUM)) ? 4 : 0;

  /* Initially there is no padding bytes.  */
  cfun->machine->callee_saved_area_padding_bytes = 0;

  /* Calculate the bytes of saving callee-saved registers on stack.  */
  cfun->machine->callee_saved_regs_size = 0;
  cfun->machine->callee_saved_regs_first_regno = SP_REGNUM;
  cfun->machine->callee_saved_regs_last_regno  = SP_REGNUM;
  /* Currently, there is no need to check $r28~$r31
     because we will save them in another way.  */
  for (r = 0; r < 28; r++)
    {
      if (NDS32_REQUIRED_CALLEE_SAVED_P (r))
	{
	  /* Mark the first required callee-saved register
	     (only need to set it once).
	     If first regno == SP_REGNUM, we can tell that
	     it is the first time to be here.  */
	  if (cfun->machine->callee_saved_regs_first_regno == SP_REGNUM)
	    cfun->machine->callee_saved_regs_first_regno = r;
	  /* Mark the last required callee-saved register.  */
	  cfun->machine->callee_saved_regs_last_regno = r;
	}
    }

  /* Check if this function can omit prologue/epilogue code fragment.
     If there is 'naked' attribute in this function,
     we can set 'naked_p' flag to indicate that
     we do not have to generate prologue/epilogue.
     Or, if all the following conditions succeed,
     we can set this function 'naked_p' as well:
       condition 1: first_regno == last_regno == SP_REGNUM,
                    which means we do not have to save
                    any callee-saved registers.
       condition 2: Both $lp and $fp are NOT live in this function,
                    which means we do not need to save them.
       condition 3: There is no local_size, which means
                    we do not need to adjust $sp.  */
  if (lookup_attribute ("naked", DECL_ATTRIBUTES (current_function_decl))
      || (cfun->machine->callee_saved_regs_first_regno == SP_REGNUM
	  && cfun->machine->callee_saved_regs_last_regno == SP_REGNUM
	  && !df_regs_ever_live_p (FP_REGNUM)
	  && !df_regs_ever_live_p (LP_REGNUM)
	  && cfun->machine->local_size == 0))
    {
      /* Set this function 'naked_p' and
         other functions can check this flag.  */
      cfun->machine->naked_p = 1;

      /* No need to save $fp, $gp, and $lp.
         We should set these value to be zero
         so that nds32_initial_elimination_offset() can work properly.  */
      cfun->machine->fp_size = 0;
      cfun->machine->gp_size = 0;
      cfun->machine->lp_size = 0;

      /* If stack usage computation is required,
         we need to provide the static stack size.  */
      if (flag_stack_usage_info)
	current_function_static_stack_size = 0;

      /* No need to do following adjustment, return immediately.  */
      return;
    }

  /* Adjustment for v3push instructions:
     If we are using v3push (push25/pop25) instructions,
     we need to make sure Rb is $r6 and Re is
     located on $r6, $r8, $r10, or $r14.
     Some results above will be discarded and recomputed.
     Note that it is only available under V3/V3M ISA.  */
  if (TARGET_V3PUSH)
    {
      /* Recompute:
           cfun->machine->fp_size
           cfun->machine->gp_size
           cfun->machine->lp_size
           cfun->machine->callee_saved_regs_first_regno
           cfun->machine->callee_saved_regs_last_regno */

      /* For v3push instructions, $fp, $gp, and $lp are always saved.  */
      cfun->machine->fp_size = 4;
      cfun->machine->gp_size = 4;
      cfun->machine->lp_size = 4;

      /* Remember to set Rb = $r6.  */
      cfun->machine->callee_saved_regs_first_regno = 6;

      if (cfun->machine->callee_saved_regs_last_regno <= 6)
	{
	  /* Re = $r6 */
	  cfun->machine->callee_saved_regs_last_regno = 6;
	}
      else if (cfun->machine->callee_saved_regs_last_regno <= 8)
	{
	  /* Re = $r8 */
	  cfun->machine->callee_saved_regs_last_regno = 8;
	}
      else if (cfun->machine->callee_saved_regs_last_regno <= 10)
	{
	  /* Re = $r10 */
	  cfun->machine->callee_saved_regs_last_regno = 10;
	}
      else if (cfun->machine->callee_saved_regs_last_regno <= 14)
	{
	  /* Re = $r14 */
	  cfun->machine->callee_saved_regs_last_regno = 14;
	}
      else if (cfun->machine->callee_saved_regs_last_regno == SP_REGNUM)
	{
	  /* If last_regno is SP_REGNUM, which means
	     it is never changed, so set it to Re = $r6.  */
	  cfun->machine->callee_saved_regs_last_regno = 6;
	}
      else
	{
	  /* The program flow should not go here.  */
	  gcc_unreachable ();
	}
    }

  /* We have correctly set callee_saved_regs_first_regno
     and callee_saved_regs_last_regno.
     Initially, the callee_saved_regs_size is supposed to be 0.
     As long as callee_saved_regs_last_regno is not SP_REGNUM,
     we can update callee_saved_regs_size with new size.  */
  if (cfun->machine->callee_saved_regs_last_regno != SP_REGNUM)
    {
      /* Compute pushed size of callee-saved registers.  */
      cfun->machine->callee_saved_regs_size
	= 4 * (cfun->machine->callee_saved_regs_last_regno
	       - cfun->machine->callee_saved_regs_first_regno
	       + 1);
    }

  /* Important: We need to make sure that
                (va_args_size + fp_size + gp_size
                 + lp_size + callee_saved_regs_size)
                is 8-byte alignment.
                If it is not, calculate the padding bytes.  */
  block_size = cfun->machine->va_args_size
	       + cfun->machine->fp_size
	       + cfun->machine->gp_size
	       + cfun->machine->lp_size
	       + cfun->machine->callee_saved_regs_size;
  if (!NDS32_DOUBLE_WORD_ALIGN_P (block_size))
    {
      cfun->machine->callee_saved_area_padding_bytes
	= NDS32_ROUND_UP_DOUBLE_WORD (block_size) - block_size;
    }

  /* If stack usage computation is required,
     we need to provide the static stack size.  */
  if (flag_stack_usage_info)
    {
      current_function_static_stack_size
	= NDS32_ROUND_UP_DOUBLE_WORD (block_size)
	  + cfun->machine->local_size
	  + cfun->machine->out_args_size;
    }
}

/* Function to create a parallel rtx pattern
   which presents stack push multiple behavior.
   The overall concept are:
     "push registers to memory",
     "adjust stack pointer".  */
static rtx
nds32_gen_stack_push_multiple (rtx Rb, rtx Re,
			       rtx En4 ATTRIBUTE_UNUSED)
{
  int regno;
  int extra_count;
  int num_use_regs;
  int par_index;
  int offset;

  rtx reg;
  rtx mem;
  rtx push_rtx;
  rtx adjust_sp_rtx;
  rtx parallel_insn;

  /* We need to provide a customized rtx which contains
     necessary information for data analysis,
     so we create a parallel rtx like this:
     (parallel [(set (mem (plus (reg:SI SP_REGNUM) (const_int -32)))
                     (reg:SI Rb))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -28)))
                     (reg:SI Rb+1))
                ...
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -16)))
                     (reg:SI Re))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -12)))
                     (reg:SI FP_REGNUM))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -8)))
                     (reg:SI GP_REGNUM))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -4)))
                     (reg:SI LP_REGNUM))
                (set (reg:SI SP_REGNUM)
                     (plus (reg:SI SP_REGNUM) (const_int -32)))]) */

  /* Calculate the number of registers that will be pushed.  */
  extra_count = 0;
  if (cfun->machine->fp_size)
    extra_count++;
  if (cfun->machine->gp_size)
    extra_count++;
  if (cfun->machine->lp_size)
    extra_count++;
  /* Note that Rb and Re may be SP_REGNUM.  DO NOT count it in.  */
  if (REGNO (Rb) == SP_REGNUM && REGNO (Re) == SP_REGNUM)
    num_use_regs = extra_count;
  else
    num_use_regs = REGNO (Re) - REGNO (Rb) + 1 + extra_count;

  /* In addition to used registers,
     we need one more space for (set sp sp-x) rtx.  */
  parallel_insn = gen_rtx_PARALLEL (VOIDmode,
				    rtvec_alloc (num_use_regs + 1));
  par_index = 0;

  /* Initialize offset and start to create push behavior.  */
  offset = -(num_use_regs * 4);

  /* Create (set mem regX) from Rb, Rb+1 up to Re.  */
  for (regno = REGNO (Rb); regno <= (int) REGNO (Re); regno++)
    {
      /* Rb and Re may be SP_REGNUM.
         We need to break this loop immediately.  */
      if (regno == SP_REGNUM)
        break;

      reg = gen_rtx_REG (SImode, regno);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      push_rtx = gen_rtx_SET (VOIDmode, mem, reg);
      XVECEXP (parallel_insn, 0, par_index) = push_rtx;
      RTX_FRAME_RELATED_P (push_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }

  /* Create (set mem fp), (set mem gp), and (set mem lp) if necessary.  */
  if (cfun->machine->fp_size)
    {
      reg = gen_rtx_REG (SImode, FP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      push_rtx = gen_rtx_SET (VOIDmode, mem, reg);
      XVECEXP (parallel_insn, 0, par_index) = push_rtx;
      RTX_FRAME_RELATED_P (push_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }
  if (cfun->machine->gp_size)
    {
      reg = gen_rtx_REG (SImode, GP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      push_rtx = gen_rtx_SET (VOIDmode, mem, reg);
      XVECEXP (parallel_insn, 0, par_index) = push_rtx;
      RTX_FRAME_RELATED_P (push_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }
  if (cfun->machine->lp_size)
    {
      reg = gen_rtx_REG (SImode, LP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      push_rtx = gen_rtx_SET (VOIDmode, mem, reg);
      XVECEXP (parallel_insn, 0, par_index) = push_rtx;
      RTX_FRAME_RELATED_P (push_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }

  /* Create (set sp sp-x).  */

  /* We need to re-calculate the offset value again for adjustment.  */
  offset = -(num_use_regs * 4);
  adjust_sp_rtx
    = gen_rtx_SET (VOIDmode,
		   stack_pointer_rtx,
		   plus_constant (Pmode, stack_pointer_rtx, offset));
  XVECEXP (parallel_insn, 0, par_index) = adjust_sp_rtx;
  RTX_FRAME_RELATED_P (adjust_sp_rtx) = 1;

  return parallel_insn;
}

/* Function to create a parallel rtx pattern
   which presents stack pop multiple behavior.
   The overall concept are:
     "pop registers from memory",
     "adjust stack pointer".  */
static rtx
nds32_gen_stack_pop_multiple (rtx Rb, rtx Re,
			      rtx En4 ATTRIBUTE_UNUSED)
{
  int regno;
  int extra_count;
  int num_use_regs;
  int par_index;
  int offset;

  rtx reg;
  rtx mem;
  rtx pop_rtx;
  rtx adjust_sp_rtx;
  rtx parallel_insn;

  /* We need to provide a customized rtx which contains
     necessary information for data analysis,
     so we create a parallel rtx like this:
     (parallel [(set (reg:SI Rb)
                     (mem (reg:SI SP_REGNUM)))
                (set (reg:SI Rb+1)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 4))))
                ...
                (set (reg:SI Re)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 16))))
                (set (reg:SI FP_REGNUM)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 20))))
                (set (reg:SI GP_REGNUM)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 24))))
                (set (reg:SI LP_REGNUM)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 28))))
                (set (reg:SI SP_REGNUM)
                     (plus (reg:SI SP_REGNUM) (const_int 32)))]) */

  /* Calculate the number of registers that will be poped.  */
  extra_count = 0;
  if (cfun->machine->fp_size)
    extra_count++;
  if (cfun->machine->gp_size)
    extra_count++;
  if (cfun->machine->lp_size)
    extra_count++;
  /* Note that Rb and Re may be SP_REGNUM.  DO NOT count it in.  */
  if (REGNO (Rb) == SP_REGNUM && REGNO (Re) == SP_REGNUM)
    num_use_regs = extra_count;
  else
    num_use_regs = REGNO (Re) - REGNO (Rb) + 1 + extra_count;

  /* In addition to used registers,
     we need one more space for (set sp sp+x) rtx.  */
  parallel_insn = gen_rtx_PARALLEL (VOIDmode,
				    rtvec_alloc (num_use_regs + 1));
  par_index = 0;

  /* Initialize offset and start to create pop behavior.  */
  offset = 0;

  /* Create (set regX mem) from Rb, Rb+1 up to Re.  */
  for (regno = REGNO (Rb); regno <= (int) REGNO (Re); regno++)
    {
      /* Rb and Re may be SP_REGNUM.
         We need to break this loop immediately.  */
      if (regno == SP_REGNUM)
        break;

      reg = gen_rtx_REG (SImode, regno);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      pop_rtx = gen_rtx_SET (VOIDmode, reg, mem);
      XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
      RTX_FRAME_RELATED_P (pop_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }

  /* Create (set fp mem), (set gp mem), and (set lp mem) if necessary.  */
  if (cfun->machine->fp_size)
    {
      reg = gen_rtx_REG (SImode, FP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      pop_rtx = gen_rtx_SET (VOIDmode, reg, mem);
      XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
      RTX_FRAME_RELATED_P (pop_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }
  if (cfun->machine->gp_size)
    {
      reg = gen_rtx_REG (SImode, GP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      pop_rtx = gen_rtx_SET (VOIDmode, reg, mem);
      XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
      RTX_FRAME_RELATED_P (pop_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }
  if (cfun->machine->lp_size)
    {
      reg = gen_rtx_REG (SImode, LP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      pop_rtx = gen_rtx_SET (VOIDmode, reg, mem);
      XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
      RTX_FRAME_RELATED_P (pop_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }

  /* Create (set sp sp+x).  */

  /* The offset value is already in place.  No need to re-calculate it.  */
  adjust_sp_rtx
    = gen_rtx_SET (VOIDmode,
		   stack_pointer_rtx,
		   plus_constant (Pmode, stack_pointer_rtx, offset));
  XVECEXP (parallel_insn, 0, par_index) = adjust_sp_rtx;
  RTX_FRAME_RELATED_P (adjust_sp_rtx) = 1;

  return parallel_insn;
}

/* Function to create a parallel rtx pattern
   which presents stack v3push behavior.
   The overall concept are:
     "push registers to memory",
     "adjust stack pointer".  */
static rtx
nds32_gen_stack_v3push (rtx Rb,
			rtx Re,
			rtx En4 ATTRIBUTE_UNUSED,
			rtx imm8u)
{
  int regno;
  int num_use_regs;
  int par_index;
  int offset;

  rtx reg;
  rtx mem;
  rtx push_rtx;
  rtx adjust_sp_rtx;
  rtx parallel_insn;

  /* We need to provide a customized rtx which contains
     necessary information for data analysis,
     so we create a parallel rtx like this:
     (parallel [
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -32)))
                     (reg:SI Rb))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -28)))
                     (reg:SI Rb+1))
                ...
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -16)))
                     (reg:SI Re))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -12)))
                     (reg:SI FP_REGNUM))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -8)))
                     (reg:SI GP_REGNUM))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -4)))
                     (reg:SI LP_REGNUM))
                (set (reg:SI SP_REGNUM)
                     (plus (reg:SI SP_REGNUM) (const_int -32-imm8u)))]) */

  /* Calculate the number of registers that will be pushed.
     Since $fp, $gp, and $lp is always pushed with v3push instruction,
     we need to count these three registers.
     Under v3push, Rb is $r6, while Re is $r6, $r8, $r10, or $r14.
     So there is no need to worry about Rb=Re=SP_REGNUM case.  */
  num_use_regs = REGNO (Re) - REGNO (Rb) + 1 + 3;

  /* In addition to used registers,
     we need one more space for (set sp sp-x-imm8u) rtx.  */
  parallel_insn = gen_rtx_PARALLEL (VOIDmode,
				    rtvec_alloc (num_use_regs + 1));
  par_index = 0;

  /* Initialize offset and start to create push behavior.  */
  offset = -(num_use_regs * 4);

  /* Create (set mem regX) from Rb, Rb+1 up to Re.
     Under v3push, Rb is $r6, while Re is $r6, $r8, $r10, or $r14.
     So there is no need to worry about Rb=Re=SP_REGNUM case.  */
  for (regno = REGNO (Rb); regno <= (int) REGNO (Re); regno++)
    {
      reg = gen_rtx_REG (SImode, regno);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      push_rtx = gen_rtx_SET (VOIDmode, mem, reg);
      XVECEXP (parallel_insn, 0, par_index) = push_rtx;
      RTX_FRAME_RELATED_P (push_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }

  /* Create (set mem fp).  */
  reg = gen_rtx_REG (SImode, FP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  push_rtx = gen_rtx_SET (VOIDmode, mem, reg);
  XVECEXP (parallel_insn, 0, par_index) = push_rtx;
  RTX_FRAME_RELATED_P (push_rtx) = 1;
  offset = offset + 4;
  par_index++;
  /* Create (set mem gp).  */
  reg = gen_rtx_REG (SImode, GP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  push_rtx = gen_rtx_SET (VOIDmode, mem, reg);
  XVECEXP (parallel_insn, 0, par_index) = push_rtx;
  RTX_FRAME_RELATED_P (push_rtx) = 1;
  offset = offset + 4;
  par_index++;
  /* Create (set mem lp).  */
  reg = gen_rtx_REG (SImode, LP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  push_rtx = gen_rtx_SET (VOIDmode, mem, reg);
  XVECEXP (parallel_insn, 0, par_index) = push_rtx;
  RTX_FRAME_RELATED_P (push_rtx) = 1;
  offset = offset + 4;
  par_index++;

  /* Create (set sp sp-x-imm8u).  */

  /* We need to re-calculate the offset value again for adjustment.  */
  offset = -(num_use_regs * 4);
  adjust_sp_rtx
    = gen_rtx_SET (VOIDmode,
		   stack_pointer_rtx,
		   plus_constant (Pmode,
				  stack_pointer_rtx,
				  offset - INTVAL (imm8u)));
  XVECEXP (parallel_insn, 0, par_index) = adjust_sp_rtx;
  RTX_FRAME_RELATED_P (adjust_sp_rtx) = 1;

  return parallel_insn;
}

/* Function to create a parallel rtx pattern
   which presents stack v3pop behavior.
   The overall concept are:
     "pop registers from memory",
     "adjust stack pointer".  */
static rtx
nds32_gen_stack_v3pop (rtx Rb,
		       rtx Re,
		       rtx En4 ATTRIBUTE_UNUSED,
		       rtx imm8u)
{
  int regno;
  int num_use_regs;
  int par_index;
  int offset;

  rtx reg;
  rtx mem;
  rtx pop_rtx;
  rtx adjust_sp_rtx;
  rtx parallel_insn;

  /* We need to provide a customized rtx which contains
     necessary information for data analysis,
     so we create a parallel rtx like this:
     (parallel [(set (reg:SI Rb)
                     (mem (reg:SI SP_REGNUM)))
                (set (reg:SI Rb+1)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 4))))
                ...
                (set (reg:SI Re)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 16))))
                (set (reg:SI FP_REGNUM)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 20))))
                (set (reg:SI GP_REGNUM)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 24))))
                (set (reg:SI LP_REGNUM)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 28))))
                (set (reg:SI SP_REGNUM)
                     (plus (reg:SI SP_REGNUM) (const_int 32+imm8u)))]) */

  /* Calculate the number of registers that will be poped.
     Since $fp, $gp, and $lp is always poped with v3pop instruction,
     we need to count these three registers.
     Under v3push, Rb is $r6, while Re is $r6, $r8, $r10, or $r14.
     So there is no need to worry about Rb=Re=SP_REGNUM case.  */
  num_use_regs = REGNO (Re) - REGNO (Rb) + 1 + 3;

  /* In addition to used registers,
     we need one more space for (set sp sp+x+imm8u) rtx.  */
  parallel_insn = gen_rtx_PARALLEL (VOIDmode,
				    rtvec_alloc (num_use_regs + 1));
  par_index = 0;

  /* Initialize offset and start to create pop behavior.  */
  offset = 0;

  /* Create (set regX mem) from Rb, Rb+1 up to Re.
     Under v3pop, Rb is $r6, while Re is $r6, $r8, $r10, or $r14.
     So there is no need to worry about Rb=Re=SP_REGNUM case.  */
  for (regno = REGNO (Rb); regno <= (int) REGNO (Re); regno++)
    {
      reg = gen_rtx_REG (SImode, regno);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      pop_rtx = gen_rtx_SET (VOIDmode, reg, mem);
      XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
      RTX_FRAME_RELATED_P (pop_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }

  /* Create (set fp mem).  */
  reg = gen_rtx_REG (SImode, FP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  pop_rtx = gen_rtx_SET (VOIDmode, reg, mem);
  XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
  RTX_FRAME_RELATED_P (pop_rtx) = 1;
  offset = offset + 4;
  par_index++;
  /* Create (set gp mem).  */
  reg = gen_rtx_REG (SImode, GP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  pop_rtx = gen_rtx_SET (VOIDmode, reg, mem);
  XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
  RTX_FRAME_RELATED_P (pop_rtx) = 1;
  offset = offset + 4;
  par_index++;
  /* Create (set lp mem ).  */
  reg = gen_rtx_REG (SImode, LP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  pop_rtx = gen_rtx_SET (VOIDmode, reg, mem);
  XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
  RTX_FRAME_RELATED_P (pop_rtx) = 1;
  offset = offset + 4;
  par_index++;

  /* Create (set sp sp+x+imm8u).  */

  /* The offset value is already in place.  No need to re-calculate it.  */
  adjust_sp_rtx
    = gen_rtx_SET (VOIDmode,
		   stack_pointer_rtx,
		   plus_constant (Pmode,
				  stack_pointer_rtx,
				  offset + INTVAL (imm8u)));
  XVECEXP (parallel_insn, 0, par_index) = adjust_sp_rtx;
  RTX_FRAME_RELATED_P (adjust_sp_rtx) = 1;

  return parallel_insn;
}

/* A subroutine that checks multiple load and store
   using consecutive registers.
     OP is a parallel rtx we would like to check.
     LOAD_P indicates whether we are checking load operation.
     PAR_INDEX is starting element of parallel rtx.
     FIRST_ELT_REGNO is used to tell starting register number.
     COUNT helps us to check consecutive register numbers.  */
static bool
nds32_consecutive_registers_load_store_p (rtx op,
					  bool load_p,
					  int par_index,
					  int first_elt_regno,
					  int count)
{
  int i;
  int check_regno;
  rtx elt;
  rtx elt_reg;
  rtx elt_mem;

  for (i = 0; i < count; i++)
    {
      /* Pick up each element from parallel rtx.  */
      elt = XVECEXP (op, 0, i + par_index);

      /* If this element is not a 'set' rtx, return false immediately.  */
      if (GET_CODE (elt) != SET)
	return false;

      /* Pick up reg and mem of this element.  */
      elt_reg = load_p ? SET_DEST (elt) : SET_SRC (elt);
      elt_mem = load_p ? SET_SRC (elt) : SET_DEST (elt);

      /* If elt_reg is not a expected reg rtx, return false.  */
      if (GET_CODE (elt_reg) != REG || GET_MODE (elt_reg) != SImode)
	return false;
      /* If elt_mem is not a expected mem rtx, return false.  */
      if (GET_CODE (elt_mem) != MEM || GET_MODE (elt_mem) != SImode)
	return false;

      /* The consecutive registers should be in (Rb,Rb+1...Re) order.  */
      check_regno = first_elt_regno + i;

      /* If the register number is not continuous, return false.  */
      if (REGNO (elt_reg) != (unsigned int) check_regno)
	return false;
    }

  return true;
}

/* A helper function to emit section head template.  */
static void
nds32_emit_section_head_template (char section_name[],
				  char symbol_name[],
				  int align_value,
				  bool object_p)
{
  const char *flags_str;
  const char *type_str;

  flags_str = (object_p) ? "\"a\"" : "\"ax\"";
  type_str = (object_p) ? "@object" : "@function";

  fprintf (asm_out_file, "\t.section\t%s, %s\n", section_name, flags_str);
  fprintf (asm_out_file, "\t.align\t%d\n", align_value);
  fprintf (asm_out_file, "\t.global\t%s\n", symbol_name);
  fprintf (asm_out_file, "\t.type\t%s, %s\n", symbol_name, type_str);
  fprintf (asm_out_file, "%s:\n", symbol_name);
}

/* A helper function to emit section tail template.  */
static void
nds32_emit_section_tail_template (char symbol_name[])
{
  fprintf (asm_out_file, "\t.size\t%s, .-%s\n", symbol_name, symbol_name);
}

/* Function to emit isr jump table section.  */
static void
nds32_emit_isr_jmptbl_section (int vector_id)
{
  char section_name[100];
  char symbol_name[100];

  /* Prepare jmptbl section and symbol name.  */
  snprintf (section_name, sizeof (section_name),
	    ".nds32_jmptbl.%02d", vector_id);
  snprintf (symbol_name, sizeof (symbol_name),
	    "_nds32_jmptbl_%02d", vector_id);

  nds32_emit_section_head_template (section_name, symbol_name, 2, true);
  fprintf (asm_out_file, "\t.word\t%s\n",
			 nds32_isr_vectors[vector_id].func_name);
  nds32_emit_section_tail_template (symbol_name);
}

/* Function to emit isr vector section.  */
static void
nds32_emit_isr_vector_section (int vector_id)
{
  unsigned int vector_number_offset = 0;
  const char *c_str = "CATEGORY";
  const char *sr_str = "SR";
  const char *nt_str = "NT";
  const char *vs_str = "VS";
  char first_level_handler_name[100];
  char section_name[100];
  char symbol_name[100];

  /* Set the vector number offset so that we can calculate
     the value that user specifies in the attribute.
     We also prepare the category string for first level handler name.  */
  switch (nds32_isr_vectors[vector_id].category)
    {
    case NDS32_ISR_INTERRUPT:
      vector_number_offset = 9;
      c_str = "i";
      break;
    case NDS32_ISR_EXCEPTION:
      vector_number_offset = 0;
      c_str = "e";
      break;
    case NDS32_ISR_NONE:
    case NDS32_ISR_RESET:
      /* Normally it should not be here.  */
      gcc_unreachable ();
      break;
    }

  /* Prepare save reg string for first level handler name.  */
  switch (nds32_isr_vectors[vector_id].save_reg)
    {
    case NDS32_SAVE_ALL:
      sr_str = "sa";
      break;
    case NDS32_PARTIAL_SAVE:
      sr_str = "ps";
      break;
    }

  /* Prepare nested type string for first level handler name.  */
  switch (nds32_isr_vectors[vector_id].nested_type)
    {
    case NDS32_NESTED:
      nt_str = "ns";
      break;
    case NDS32_NOT_NESTED:
      nt_str = "nn";
      break;
    case NDS32_NESTED_READY:
      nt_str = "nr";
      break;
    }

  /* Currently we have 4-byte or 16-byte size for each vector.
     If it is 4-byte, the first level handler name has suffix string "_4b".  */
  vs_str = (nds32_isr_vector_size == 4) ? "_4b" : "";

  /* Now we can create first level handler name.  */
  snprintf (first_level_handler_name, sizeof (first_level_handler_name),
	    "_nds32_%s_%s_%s%s", c_str, sr_str, nt_str, vs_str);

  /* Prepare vector section and symbol name.  */
  snprintf (section_name, sizeof (section_name),
	    ".nds32_vector.%02d", vector_id);
  snprintf (symbol_name, sizeof (symbol_name),
	    "_nds32_vector_%02d%s", vector_id, vs_str);


  /* Everything is ready.  We can start emit vector section content.  */
  nds32_emit_section_head_template (section_name, symbol_name,
				    floor_log2 (nds32_isr_vector_size), false);

  /* According to the vector size, the instructions in the
     vector section may be different.  */
  if (nds32_isr_vector_size == 4)
    {
      /* This block is for 4-byte vector size.
         Hardware $VID support is necessary and only one instruction
         is needed in vector section.  */
      fprintf (asm_out_file, "\tj\t%s ! jump to first level handler\n",
			     first_level_handler_name);
    }
  else
    {
      /* This block is for 16-byte vector size.
         There is NO hardware $VID so that we need several instructions
         such as pushing GPRs and preparing software vid at vector section.
         For pushing GPRs, there are four variations for
         16-byte vector content and we have to handle each combination.
         For preparing software vid, note that the vid need to
         be substracted vector_number_offset.  */
      if (TARGET_REDUCED_REGS)
	{
	  if (nds32_isr_vectors[vector_id].save_reg == NDS32_SAVE_ALL)
	    {
	      /* Case of reduced set registers and save_all attribute.  */
	      fprintf (asm_out_file, "\t! reduced set regs + save_all\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r15, [$sp], $r15, 0xf\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r0, [$sp], $r10, 0x0\n");

	    }
	  else
	    {
	      /* Case of reduced set registers and partial_save attribute.  */
	      fprintf (asm_out_file, "\t! reduced set regs + partial_save\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r15, [$sp], $r15, 0x2\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r0, [$sp], $r5, 0x0\n");
	    }
	}
      else
	{
	  if (nds32_isr_vectors[vector_id].save_reg == NDS32_SAVE_ALL)
	    {
	      /* Case of full set registers and save_all attribute.  */
	      fprintf (asm_out_file, "\t! full set regs + save_all\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r0, [$sp], $r27, 0xf\n");
	    }
	  else
	    {
	      /* Case of full set registers and partial_save attribute.  */
	      fprintf (asm_out_file, "\t! full set regs + partial_save\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r15, [$sp], $r27, 0x2\n");
	      fprintf (asm_out_file, "\tsmw.adm\t$r0, [$sp], $r5, 0x0\n");
	    }
	}

      fprintf (asm_out_file, "\tmovi\t$r0, %d ! preparing software vid\n",
			     vector_id - vector_number_offset);
      fprintf (asm_out_file, "\tj\t%s ! jump to first level handler\n",
			     first_level_handler_name);
    }

  nds32_emit_section_tail_template (symbol_name);
}

/* Function to emit isr reset handler content.
   Including all jmptbl/vector references, jmptbl section,
   vector section, nmi handler section, and warm handler section.  */
static void
nds32_emit_isr_reset_content (void)
{
  unsigned int i;
  unsigned int total_n_vectors;
  const char *vs_str;
  char reset_handler_name[100];
  char section_name[100];
  char symbol_name[100];

  total_n_vectors = nds32_isr_vectors[0].total_n_vectors;
  vs_str = (nds32_isr_vector_size == 4) ? "_4b" : "";

  fprintf (asm_out_file, "\t! RESET HANDLER CONTENT - BEGIN !\n");

  /* Create references in .rodata according to total number of vectors.  */
  fprintf (asm_out_file, "\t.section\t.rodata\n");
  fprintf (asm_out_file, "\t.align\t2\n");

  /* Emit jmptbl references.  */
  fprintf (asm_out_file, "\t ! references to jmptbl section entries\n");
  for (i = 0; i < total_n_vectors; i++)
    fprintf (asm_out_file, "\t.word\t_nds32_jmptbl_%02d\n", i);

  /* Emit vector references.  */
  fprintf (asm_out_file, "\t ! references to vector section entries\n");
  for (i = 0; i < total_n_vectors; i++)
    fprintf (asm_out_file, "\t.word\t_nds32_vector_%02d%s\n", i, vs_str);

  /* Emit jmptbl_00 section.  */
  snprintf (section_name, sizeof (section_name), ".nds32_jmptbl.00");
  snprintf (symbol_name, sizeof (symbol_name), "_nds32_jmptbl_00");

  fprintf (asm_out_file, "\t! ....................................\n");
  nds32_emit_section_head_template (section_name, symbol_name, 2, true);
  fprintf (asm_out_file, "\t.word\t%s\n",
			 nds32_isr_vectors[0].func_name);
  nds32_emit_section_tail_template (symbol_name);

  /* Emit vector_00 section.  */
  snprintf (section_name, sizeof (section_name), ".nds32_vector.00");
  snprintf (symbol_name, sizeof (symbol_name), "_nds32_vector_00%s", vs_str);
  snprintf (reset_handler_name, sizeof (reset_handler_name),
	    "_nds32_reset%s", vs_str);

  fprintf (asm_out_file, "\t! ....................................\n");
  nds32_emit_section_head_template (section_name, symbol_name,
				    floor_log2 (nds32_isr_vector_size), false);
  fprintf (asm_out_file, "\tj\t%s ! jump to reset handler\n",
			 reset_handler_name);
  nds32_emit_section_tail_template (symbol_name);

  /* Emit nmi handler section.  */
  snprintf (section_name, sizeof (section_name), ".nds32_nmih");
  snprintf (symbol_name, sizeof (symbol_name), "_nds32_nmih");

  fprintf (asm_out_file, "\t! ....................................\n");
  nds32_emit_section_head_template (section_name, symbol_name, 2, true);
  fprintf (asm_out_file, "\t.word\t%s\n",
			 (strlen (nds32_isr_vectors[0].nmi_name) == 0)
			 ? "0"
			 : nds32_isr_vectors[0].nmi_name);
  nds32_emit_section_tail_template (symbol_name);

  /* Emit warm handler section.  */
  snprintf (section_name, sizeof (section_name), ".nds32_wrh");
  snprintf (symbol_name, sizeof (symbol_name), "_nds32_wrh");

  fprintf (asm_out_file, "\t! ....................................\n");
  nds32_emit_section_head_template (section_name, symbol_name, 2, true);
  fprintf (asm_out_file, "\t.word\t%s\n",
			 (strlen (nds32_isr_vectors[0].warm_name) == 0)
			 ? "0"
			 : nds32_isr_vectors[0].warm_name);
  nds32_emit_section_tail_template (symbol_name);

  fprintf (asm_out_file, "\t! RESET HANDLER CONTENT - END !\n");
}

/* Function for nds32_merge_decl_attributes() and nds32_insert_attributes()
   to check if there are any conflict isr-specific attributes being set.
   We need to check:
     1. Only 'save_all' or 'partial_save' in the attributes.
     2. Only 'nested', 'not_nested', or 'nested_ready' in the attributes.
     3. Only 'interrupt', 'exception', or 'reset' in the attributes.  */
static void
nds32_check_isr_attrs_conflict (tree func_decl, tree func_attrs)
{
  int save_all_p, partial_save_p;
  int nested_p, not_nested_p, nested_ready_p;
  int intr_p, excp_p, reset_p;

  /* Initialize variables.  */
  save_all_p = partial_save_p = 0;
  nested_p = not_nested_p = nested_ready_p = 0;
  intr_p = excp_p = reset_p = 0;

  /* We must check at MOST one attribute to set save-reg.  */
  if (lookup_attribute ("save_all", func_attrs))
    save_all_p = 1;
  if (lookup_attribute ("partial_save", func_attrs))
    partial_save_p = 1;

  if ((save_all_p + partial_save_p) > 1)
    error ("multiple save reg attributes to function %qD", func_decl);

  /* We must check at MOST one attribute to set nested-type.  */
  if (lookup_attribute ("nested", func_attrs))
    nested_p = 1;
  if (lookup_attribute ("not_nested", func_attrs))
    not_nested_p = 1;
  if (lookup_attribute ("nested_ready", func_attrs))
    nested_ready_p = 1;

  if ((nested_p + not_nested_p + nested_ready_p) > 1)
    error ("multiple nested types attributes to function %qD", func_decl);

  /* We must check at MOST one attribute to
     set interrupt/exception/reset.  */
  if (lookup_attribute ("interrupt", func_attrs))
    intr_p = 1;
  if (lookup_attribute ("exception", func_attrs))
    excp_p = 1;
  if (lookup_attribute ("reset", func_attrs))
    reset_p = 1;

  if ((intr_p + excp_p + reset_p) > 1)
    error ("multiple interrupt attributes to function %qD", func_decl);
}

/* Function to construct isr vectors information array.
   We DO NOT HAVE TO check if the attributes are valid
   because those works are supposed to be done on
   nds32_merge_decl_attributes() and nds32_insert_attributes().  */
static void
nds32_construct_isr_vectors_information (tree func_attrs,
					 const char *func_name)
{
  tree save_all, partial_save;
  tree nested, not_nested, nested_ready;
  tree intr, excp, reset;

  save_all     = lookup_attribute ("save_all", func_attrs);
  partial_save = lookup_attribute ("partial_save", func_attrs);

  nested       = lookup_attribute ("nested", func_attrs);
  not_nested   = lookup_attribute ("not_nested", func_attrs);
  nested_ready = lookup_attribute ("nested_ready", func_attrs);

  intr  = lookup_attribute ("interrupt", func_attrs);
  excp  = lookup_attribute ("exception", func_attrs);
  reset = lookup_attribute ("reset", func_attrs);

  /* If there is no interrupt/exception/reset, we can return immediately.  */
  if (!intr && !excp && !reset)
    return;

  /* If we are here, either we have interrupt/exception,
     or reset attribute.  */
  if (intr || excp)
    {
      tree id_list;

      /* Prepare id list so that we can traverse and set vector id.  */
      id_list = (intr) ? (TREE_VALUE (intr)) : (TREE_VALUE (excp));

      while (id_list)
	{
	  tree id;
	  int vector_id;
	  unsigned int vector_number_offset;

	  /* The way to handle interrupt or exception is the same,
	     we just need to take care of actual vector number.
	     For interrupt(0..63), the actual vector number is (9..72).
	     For exception(1..8), the actual vector number is (1..8).  */
	  vector_number_offset = (intr) ? (9) : (0);

	  /* Pick up each vector id value.  */
	  id = TREE_VALUE (id_list);
	  /* Add vector_number_offset to get actual vector number.  */
	  vector_id = TREE_INT_CST_LOW (id) + vector_number_offset;

	  /* Enable corresponding vector and set function name.  */
	  nds32_isr_vectors[vector_id].category = (intr)
						  ? (NDS32_ISR_INTERRUPT)
						  : (NDS32_ISR_EXCEPTION);
	  strcpy (nds32_isr_vectors[vector_id].func_name, func_name);

	  /* Set register saving scheme.  */
	  if (save_all)
	    nds32_isr_vectors[vector_id].save_reg = NDS32_SAVE_ALL;
	  else if (partial_save)
	    nds32_isr_vectors[vector_id].save_reg = NDS32_PARTIAL_SAVE;

	  /* Set nested type.  */
	  if (nested)
	    nds32_isr_vectors[vector_id].nested_type = NDS32_NESTED;
	  else if (not_nested)
	    nds32_isr_vectors[vector_id].nested_type = NDS32_NOT_NESTED;
	  else if (nested_ready)
	    nds32_isr_vectors[vector_id].nested_type = NDS32_NESTED_READY;

	  /* Advance to next id.  */
	  id_list = TREE_CHAIN (id_list);
	}
    }
  else
    {
      tree id_list;
      tree id;
      tree nmi, warm;

      /* Deal with reset attribute.  Its vector number is always 0.  */
      nds32_isr_vectors[0].category = NDS32_ISR_RESET;

      /* Prepare id_list and identify id value so that
         we can set total number of vectors.  */
      id_list = TREE_VALUE (reset);
      id = TREE_VALUE (id_list);

      /* The total vectors = interrupt + exception numbers + reset.
         There are 8 exception and 1 reset in nds32 architecture.  */
      nds32_isr_vectors[0].total_n_vectors = TREE_INT_CST_LOW (id) + 8 + 1;
      strcpy (nds32_isr_vectors[0].func_name, func_name);

      /* Retrieve nmi and warm function.  */
      nmi  = lookup_attribute ("nmi", func_attrs);
      warm = lookup_attribute ("warm", func_attrs);

      if (nmi != NULL_TREE)
	{
	  tree nmi_func_list;
	  tree nmi_func;

	  nmi_func_list = TREE_VALUE (nmi);
	  nmi_func = TREE_VALUE (nmi_func_list);

	  /* Record nmi function name.  */
	  strcpy (nds32_isr_vectors[0].nmi_name,
		  IDENTIFIER_POINTER (nmi_func));
	}

      if (warm != NULL_TREE)
	{
	  tree warm_func_list;
	  tree warm_func;

	  warm_func_list = TREE_VALUE (warm);
	  warm_func = TREE_VALUE (warm_func_list);

	  /* Record warm function name.  */
	  strcpy (nds32_isr_vectors[0].warm_name,
		  IDENTIFIER_POINTER (warm_func));
	}
    }
}

/* Function that may creates more instructions
   for large value on adjusting stack pointer.

   In nds32 target, 'addi' can be used for stack pointer
   adjustment in prologue/epilogue stage.
   However, sometimes there are too many local variables so that
   the adjustment value is not able to be fit in the 'addi' instruction.
   One solution is to move value into a register
   and then use 'add' instruction.
   In practice, we use TA_REGNUM ($r15) to accomplish this purpose.
   Also, we need to return zero for sp adjustment so that
   proglogue/epilogue knows there is no need to create 'addi' instruction.  */
static int
nds32_force_addi_stack_int (int full_value)
{
  int adjust_value;

  rtx tmp_reg;
  rtx sp_adjust_insn;

  if (!satisfies_constraint_Is15 (GEN_INT (full_value)))
    {
      /* The value is not able to fit in single addi instruction.
         Create more instructions of moving value into a register
         and then add stack pointer with it.  */

      /* $r15 is going to be temporary register to hold the value.  */
      tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);

      /* Create one more instruction to move value
         into the temporary register.  */
      emit_move_insn (tmp_reg, GEN_INT (full_value));

      /* Create new 'add' rtx.  */
      sp_adjust_insn = gen_addsi3 (stack_pointer_rtx,
				   stack_pointer_rtx,
				   tmp_reg);
      /* Emit rtx into insn list and receive its transformed insn rtx.  */
      sp_adjust_insn = emit_insn (sp_adjust_insn);

      /* At prologue, we need to tell GCC that this is frame related insn,
         so that we can consider this instruction to output debug information.
         If full_value is NEGATIVE, it means this function
         is invoked by expand_prologue.  */
      if (full_value < 0)
	{
	  /* Because (tmp_reg <- full_value) may be split into two
	     rtl patterns, we can not set its RTX_FRAME_RELATED_P.
	     We need to construct another (sp <- sp + full_value)
	     and then insert it into sp_adjust_insn's reg note to
	     represent a frame related expression.
	     GCC knows how to refer it and output debug information.  */

	  rtx plus_rtx;
	  rtx set_rtx;

	  plus_rtx = plus_constant (Pmode, stack_pointer_rtx, full_value);
	  set_rtx = gen_rtx_SET (VOIDmode, stack_pointer_rtx, plus_rtx);
	  add_reg_note (sp_adjust_insn, REG_FRAME_RELATED_EXPR, set_rtx);

	  RTX_FRAME_RELATED_P (sp_adjust_insn) = 1;
	}

      /* We have used alternative way to adjust stack pointer value.
         Return zero so that prologue/epilogue
         will not generate other instructions.  */
      return 0;
    }
  else
    {
      /* The value is able to fit in addi instruction.
         However, remember to make it to be positive value
         because we want to return 'adjustment' result.  */
      adjust_value = (full_value < 0) ? (-full_value) : (full_value);

      return adjust_value;
    }
}

/* Return true if MODE/TYPE need double word alignment.  */
static bool
nds32_needs_double_word_align (enum machine_mode mode, const_tree type)
{
  unsigned int align;

  /* Pick up the alignment according to the mode or type.  */
  align = NDS32_MODE_TYPE_ALIGN (mode, type);

  return (align > PARM_BOUNDARY);
}

/* Return true if FUNC is a naked function.  */
static bool
nds32_naked_function_p (tree func)
{
  tree t;

  if (TREE_CODE (func) != FUNCTION_DECL)
    abort ();

  t = lookup_attribute ("naked", DECL_ATTRIBUTES (func));

  return (t != NULL_TREE);
}

/* Function that check if 'X' is a valid address register.
   The variable 'STRICT' is very important to
   make decision for register number.

   STRICT : true
     => We are in reload pass or after reload pass.
        The register number should be strictly limited in general registers.

   STRICT : false
     => Before reload pass, we are free to use any register number.  */
static bool
nds32_address_register_rtx_p (rtx x, bool strict)
{
  int regno;

  if (GET_CODE (x) != REG)
    return false;

  regno = REGNO (x);

  if (strict)
    return REGNO_OK_FOR_BASE_P (regno);
  else
    return true;
}

/* Function that check if 'INDEX' is valid to be a index rtx for address.

   OUTER_MODE : Machine mode of outer address rtx.
        INDEX : Check if this rtx is valid to be a index for address.
       STRICT : If it is true, we are in reload pass or after reload pass.  */
static bool
nds32_legitimate_index_p (enum machine_mode outer_mode,
			  rtx index,
			  bool strict)
{
  int regno;
  rtx op0;
  rtx op1;

  switch (GET_CODE (index))
    {
    case REG:
      regno = REGNO (index);
      /* If we are in reload pass or after reload pass,
         we need to limit it to general register.  */
      if (strict)
	return REGNO_OK_FOR_INDEX_P (regno);
      else
	return true;

    case CONST_INT:
      /* The alignment of the integer value is determined by 'outer_mode'.  */
      if (GET_MODE_SIZE (outer_mode) == 1)
	{
	  /* Further check if the value is legal for the 'outer_mode'.  */
	  if (!satisfies_constraint_Is15 (index))
	    return false;

	  /* Pass all test, the value is valid, return true.  */
	  return true;
	}
      if (GET_MODE_SIZE (outer_mode) == 2
	  && NDS32_HALF_WORD_ALIGN_P (INTVAL (index)))
	{
	  /* Further check if the value is legal for the 'outer_mode'.  */
	  if (!satisfies_constraint_Is16 (index))
	    return false;

	  /* Pass all test, the value is valid, return true.  */
	  return true;
	}
      if (GET_MODE_SIZE (outer_mode) == 4
	  && NDS32_SINGLE_WORD_ALIGN_P (INTVAL (index)))
	{
	  /* Further check if the value is legal for the 'outer_mode'.  */
	  if (!satisfies_constraint_Is17 (index))
	    return false;

	  /* Pass all test, the value is valid, return true.  */
	  return true;
	}
      if (GET_MODE_SIZE (outer_mode) == 8
	  && NDS32_SINGLE_WORD_ALIGN_P (INTVAL (index)))
	{
	  /* Further check if the value is legal for the 'outer_mode'.  */
	  if (!satisfies_constraint_Is17 (gen_int_mode (INTVAL (index) + 4,
							SImode)))
	    return false;

	  /* Pass all test, the value is valid, return true.  */
	  return true;
	}

      return false;

    case MULT:
      op0 = XEXP (index, 0);
      op1 = XEXP (index, 1);

      if (REG_P (op0) && CONST_INT_P (op1))
	{
	  int multiplier;
	  multiplier = INTVAL (op1);

	  /* We only allow (mult reg const_int_1)
	     or (mult reg const_int_2) or (mult reg const_int_4).  */
	  if (multiplier != 1 && multiplier != 2 && multiplier != 4)
	    return false;

	  regno = REGNO (op0);
	  /* Limit it in general registers if we are
	     in reload pass or after reload pass.  */
	  if(strict)
	    return REGNO_OK_FOR_INDEX_P (regno);
	  else
	    return true;
	}

      return false;

    case ASHIFT:
      op0 = XEXP (index, 0);
      op1 = XEXP (index, 1);

      if (REG_P (op0) && CONST_INT_P (op1))
	{
	  int sv;
	  /* op1 is already the sv value for use to do left shift.  */
	  sv = INTVAL (op1);

	  /* We only allow (ashift reg const_int_0)
	     or (ashift reg const_int_1) or (ashift reg const_int_2).  */
	  if (sv != 0 && sv != 1 && sv !=2)
	    return false;

	  regno = REGNO (op0);
	  /* Limit it in general registers if we are
	     in reload pass or after reload pass.  */
	  if(strict)
	    return REGNO_OK_FOR_INDEX_P (regno);
	  else
	    return true;
	}

      return false;

    default:
      return false;
    }
}

/* Function to expand builtin function for
   '[(unspec_volatile [(reg)])]'.  */
static rtx
nds32_expand_builtin_null_ftype_reg (enum insn_code icode,
				     tree exp, rtx target)
{
  /* Mapping:
       ops[0] <--> value0 <--> arg0 */
  struct expand_operand ops[1];
  tree arg0;
  rtx value0;

  /* Grab the incoming arguments and extract its rtx.  */
  arg0 = CALL_EXPR_ARG (exp, 0);
  value0 = expand_normal (arg0);

  /* Create operands.  */
  create_input_operand (&ops[0], value0, TYPE_MODE (TREE_TYPE (arg0)));

  /* Emit new instruction.  */
  if (!maybe_expand_insn (icode, 1, ops))
    error ("invalid argument to built-in function");

  return target;
}

/* Function to expand builtin function for
   '[(set (reg) (unspec_volatile [(imm)]))]'.  */
static rtx
nds32_expand_builtin_reg_ftype_imm (enum insn_code icode,
				    tree exp, rtx target)
{
  /* Mapping:
       ops[0] <--> target <--> exp
       ops[1] <--> value0 <--> arg0 */
  struct expand_operand ops[2];
  tree arg0;
  rtx value0;

  /* Grab the incoming arguments and extract its rtx.  */
  arg0 = CALL_EXPR_ARG (exp, 0);
  value0 = expand_normal (arg0);

  /* Create operands.  */
  create_output_operand (&ops[0], target, TYPE_MODE (TREE_TYPE (exp)));
  create_input_operand (&ops[1], value0, TYPE_MODE (TREE_TYPE (arg0)));

  /* Emit new instruction.  */
  if (!maybe_expand_insn (icode, 2, ops))
    error ("invalid argument to built-in function");

  return target;
}

/* Function to expand builtin function for
   '[(unspec_volatile [(reg) (imm)])]' pattern.  */
static rtx
nds32_expand_builtin_null_ftype_reg_imm (enum insn_code icode,
					 tree exp, rtx target)
{
  /* Mapping:
       ops[0] <--> value0 <--> arg0
       ops[1] <--> value1 <--> arg1 */
  struct expand_operand ops[2];
  tree arg0, arg1;
  rtx value0, value1;

  /* Grab the incoming arguments and extract its rtx.  */
  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);
  value0 = expand_normal (arg0);
  value1 = expand_normal (arg1);

  /* Create operands.  */
  create_input_operand (&ops[0], value0, TYPE_MODE (TREE_TYPE (arg0)));
  create_input_operand (&ops[1], value1, TYPE_MODE (TREE_TYPE (arg1)));

  /* Emit new instruction.  */
  if (!maybe_expand_insn (icode, 2, ops))
    error ("invalid argument to built-in function");

  return target;
}

/* A helper function to return character based on byte size.  */
static char
nds32_byte_to_size (int byte)
{
  switch (byte)
    {
    case 4:
      return 'w';
    case 2:
      return 'h';
    case 1:
      return 'b';
    default:
      /* Normally it should not be here.  */
      gcc_unreachable ();
    }
}

/* A helper function to check if this function should contain prologue.  */
static int
nds32_have_prologue_p (void)
{
  int i;

  for (i = 0; i < 28; i++)
    if (NDS32_REQUIRED_CALLEE_SAVED_P (i))
      return 1;

  return (flag_pic
	  || NDS32_REQUIRED_CALLEE_SAVED_P (FP_REGNUM)
	  || NDS32_REQUIRED_CALLEE_SAVED_P (LP_REGNUM));
}

/* ------------------------------------------------------------------------ */

/* PART 3: Implement target hook stuff definitions.  */

/* Register Classes.  */

static unsigned char
nds32_class_max_nregs (reg_class_t rclass ATTRIBUTE_UNUSED,
		       enum machine_mode mode)
{
  /* Return the maximum number of consecutive registers
     needed to represent "mode" in a register of "rclass".  */
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
}

static int
nds32_register_priority (int hard_regno)
{
  /* Encourage to use r0-r7 for LRA when optimize for size.  */
  if (optimize_size && hard_regno < 8)
    return 4;
  return 3;
}


/* Stack Layout and Calling Conventions.  */

/* There are three kinds of pointer concepts using in GCC compiler:

     frame pointer: A pointer to the first location of local variables.
     stack pointer: A pointer to the top of a stack frame.
     argument pointer: A pointer to the incoming arguments.

   In nds32 target calling convention, we are using 8-byte alignment.
   Besides, we would like to have each stack frame of a function includes:

     [Block A]
       1. previous hard frame pointer
       2. return address
       3. callee-saved registers
       4. <padding bytes> (we will calculte in nds32_compute_stack_frame()
                           and save it at
                           cfun->machine->callee_saved_area_padding_bytes)

     [Block B]
       1. local variables
       2. spilling location
       3. <padding bytes> (it will be calculated by GCC itself)
       4. incoming arguments
       5. <padding bytes> (it will be calculated by GCC itself)

     [Block C]
       1. <padding bytes> (it will be calculated by GCC itself)
       2. outgoing arguments

   We 'wrap' these blocks together with
   hard frame pointer ($r28) and stack pointer ($r31).
   By applying the basic frame/stack/argument pointers concept,
   the layout of a stack frame shoule be like this:

                            |    |
       old stack pointer ->  ----
                            |    | \
                            |    |   saved arguments for
                            |    |   vararg functions
                            |    | /
      hard frame pointer ->   --
      & argument pointer    |    | \
                            |    |   previous hardware frame pointer
                            |    |   return address
                            |    |   callee-saved registers
                            |    | /
           frame pointer ->   --
                            |    | \
                            |    |   local variables
                            |    |   and incoming arguments
                            |    | /
                              --
                            |    | \
                            |    |   outgoing
                            |    |   arguments
                            |    | /
           stack pointer ->  ----

  $SFP and $AP are used to represent frame pointer and arguments pointer,
  which will be both eliminated as hard frame pointer.  */

/* -- Eliminating Frame Pointer and Arg Pointer.  */

static bool nds32_can_eliminate (const int from_reg, const int to_reg)
{
  if (from_reg == ARG_POINTER_REGNUM && to_reg == STACK_POINTER_REGNUM)
    return true;

  if (from_reg == ARG_POINTER_REGNUM && to_reg == HARD_FRAME_POINTER_REGNUM)
    return true;

  if (from_reg == FRAME_POINTER_REGNUM && to_reg == STACK_POINTER_REGNUM)
    return true;

  if (from_reg == FRAME_POINTER_REGNUM && to_reg == HARD_FRAME_POINTER_REGNUM)
    return true;

  return false;
}

/* -- Passing Arguments in Registers.  */

static rtx
nds32_function_arg (cumulative_args_t ca, enum machine_mode mode,
		    const_tree type, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (ca);

  /* The last time this hook is called,
     it is called with MODE == VOIDmode.  */
  if (mode == VOIDmode)
    return NULL_RTX;

  /* For nameless arguments, they are passed on the stack.  */
  if (!named)
    return NULL_RTX;

  /* If there are still registers available, return it.  */
  if (NDS32_ARG_PASS_IN_REG_P (cum->reg_offset, mode, type))
    {
      /* Pick up the next available register number.  */
      unsigned int regno;

      regno = NDS32_AVAILABLE_REGNUM_FOR_ARG (cum->reg_offset, mode, type);
      return gen_rtx_REG (mode, regno);
    }
  else
    {
      /* No register available, return NULL_RTX.
         The compiler will use stack to pass argument instead.  */
      return NULL_RTX;
    }
}

static void
nds32_function_arg_advance (cumulative_args_t ca, enum machine_mode mode,
			    const_tree type, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (ca);

  /* Advance next register for use.
     Only named argument could be advanced.  */
  if (named)
    {
      cum->reg_offset
	= NDS32_AVAILABLE_REGNUM_FOR_ARG (cum->reg_offset, mode, type)
	  - NDS32_GPR_ARG_FIRST_REGNUM
	  + NDS32_NEED_N_REGS_FOR_ARG (mode, type);
    }
}

static unsigned int
nds32_function_arg_boundary (enum machine_mode mode, const_tree type)
{
  return (nds32_needs_double_word_align (mode, type)
	  ? NDS32_DOUBLE_WORD_ALIGNMENT
	  : PARM_BOUNDARY);
}

/* -- How Scalar Function Values Are Returned.  */

static rtx
nds32_function_value (const_tree ret_type,
		      const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		      bool outgoing ATTRIBUTE_UNUSED)
{
  enum machine_mode mode;
  int unsignedp;

  mode = TYPE_MODE (ret_type);
  unsignedp = TYPE_UNSIGNED (ret_type);

  mode = promote_mode (ret_type, mode, &unsignedp);

  return gen_rtx_REG (mode, NDS32_GPR_RET_FIRST_REGNUM);
}

static rtx
nds32_libcall_value (enum machine_mode mode,
		     const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, NDS32_GPR_RET_FIRST_REGNUM);
}

static bool
nds32_function_value_regno_p (const unsigned int regno)
{
  return (regno == NDS32_GPR_RET_FIRST_REGNUM);
}

/* -- Function Entry and Exit.  */

/* The content produced from this function
   will be placed before prologue body.  */
static void
nds32_asm_function_prologue (FILE *file,
			     HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  int r;
  const char *func_name;
  tree attrs;
  tree name;

  /* All stack frame information is supposed to be
     already computed when expanding prologue.
     The result is in cfun->machine.
     DO NOT call nds32_compute_stack_frame() here
     because it may corrupt the essential information.  */

  fprintf (file, "\t! BEGIN PROLOGUE\n");
  fprintf (file, "\t!     fp needed: %d\n", frame_pointer_needed);
  fprintf (file, "\t!  pretend_args: %d\n", cfun->machine->va_args_size);
  fprintf (file, "\t!    local_size: %d\n", cfun->machine->local_size);
  fprintf (file, "\t! out_args_size: %d\n", cfun->machine->out_args_size);

  /* Use df_regs_ever_live_p() to detect if the register
     is ever used in the current function.  */
  fprintf (file, "\t! registers ever_live: ");
  for (r = 0; r < 32; r++)
    {
      if (df_regs_ever_live_p (r))
	fprintf (file, "%s, ", reg_names[r]);
    }
  fputc ('\n', file);

  /* Display the attributes of this function.  */
  fprintf (file, "\t! function attributes: ");
  /* GCC build attributes list with reverse order,
     so we use nreverse() to make it looks like
     the order that user specifies.  */
  attrs = nreverse (DECL_ATTRIBUTES (current_function_decl));

  /* If there is no any attribute, print out "None".  */
  if (!attrs)
    fprintf (file, "None");

  /* If there are some attributes, try if we need to
     construct isr vector information.  */
  func_name = IDENTIFIER_POINTER (DECL_NAME (current_function_decl));
  nds32_construct_isr_vectors_information (attrs, func_name);

  /* Display all attributes of this function.  */
  while (attrs)
    {
      name = TREE_PURPOSE (attrs);
      fprintf (file, "%s ", IDENTIFIER_POINTER (name));

      /* Pick up the next attribute.  */
      attrs = TREE_CHAIN (attrs);
    }
  fputc ('\n', file);
}

/* After rtl prologue has been expanded, this function is used.  */
static void
nds32_asm_function_end_prologue (FILE *file)
{
  fprintf (file, "\t! END PROLOGUE\n");

  /* If frame pointer is NOT needed and -mfp-as-gp is issued,
     we can generate special directive: ".omit_fp_begin"
     to guide linker doing fp-as-gp optimization.
     However, for a naked function, which means
     it should not have prologue/epilogue,
     using fp-as-gp still requires saving $fp by push/pop behavior and
     there is no benefit to use fp-as-gp on such small function.
     So we need to make sure this function is NOT naked as well.  */
  if (!frame_pointer_needed
      && !cfun->machine->naked_p
      && cfun->machine->fp_as_gp_p)
    {
      fprintf (file, "\t! ----------------------------------------\n");
      fprintf (file, "\t! Guide linker to do "
		     "link time optimization: fp-as-gp\n");
      fprintf (file, "\t! We add one more instruction to "
		     "initialize $fp near to $gp location.\n");
      fprintf (file, "\t! If linker fails to use fp-as-gp transformation,\n");
      fprintf (file, "\t! this extra instruction should be "
		     "eliminated at link stage.\n");
      fprintf (file, "\t.omit_fp_begin\n");
      fprintf (file, "\tla\t$fp,_FP_BASE_\n");
      fprintf (file, "\t! ----------------------------------------\n");
    }
}

/* Before rtl epilogue has been expanded, this function is used.  */
static void
nds32_asm_function_begin_epilogue (FILE *file)
{
  /* If frame pointer is NOT needed and -mfp-as-gp is issued,
     we can generate special directive: ".omit_fp_end"
     to claim fp-as-gp optimization range.
     However, for a naked function,
     which means it should not have prologue/epilogue,
     using fp-as-gp still requires saving $fp by push/pop behavior and
     there is no benefit to use fp-as-gp on such small function.
     So we need to make sure this function is NOT naked as well.  */
  if (!frame_pointer_needed
      && !cfun->machine->naked_p
      && cfun->machine->fp_as_gp_p)
    {
      fprintf (file, "\t! ----------------------------------------\n");
      fprintf (file, "\t! Claim the range of fp-as-gp "
		     "link time optimization\n");
      fprintf (file, "\t.omit_fp_end\n");
      fprintf (file, "\t! ----------------------------------------\n");
    }

  fprintf (file, "\t! BEGIN EPILOGUE\n");
}

/* The content produced from this function
   will be placed after epilogue body.  */
static void
nds32_asm_function_epilogue (FILE *file,
			     HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  fprintf (file, "\t! END EPILOGUE\n");
}

static void
nds32_asm_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
			   HOST_WIDE_INT delta,
			   HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED,
			   tree function)
{
  int this_regno;

  /* Make sure unwind info is emitted for the thunk if needed.  */
  final_start_function (emit_barrier (), file, 1);

  this_regno = (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function)
		? 1
		: 0);

  if (delta != 0)
    {
      if (satisfies_constraint_Is15 (GEN_INT (delta)))
	{
	  fprintf (file, "\taddi\t$r%d, $r%d, %ld\n",
		   this_regno, this_regno, delta);
	}
      else if (satisfies_constraint_Is20 (GEN_INT (delta)))
	{
	  fprintf (file, "\tmovi\t$ta, %ld\n", delta);
	  fprintf (file, "\tadd\t$r%d, $r%d, $ta\n", this_regno, this_regno);
	}
      else
	{
	  fprintf (file, "\tsethi\t$ta, hi20(%ld)\n", delta);
	  fprintf (file, "\tori\t$ta, $ta, lo12(%ld)\n", delta);
	  fprintf (file, "\tadd\t$r%d, $r%d, $ta\n", this_regno, this_regno);
	}
    }

  fprintf (file, "\tb\t");
  assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
  fprintf (file, "\n");

  final_end_function ();
}

/* -- Permitting tail calls.  */

/* Determine whether we need to enable warning for function return check.  */
static bool
nds32_warn_func_return (tree decl)
{
/* Naked functions are implemented entirely in assembly, including the
   return sequence, so suppress warnings about this.  */
  return !nds32_naked_function_p (decl);
}


/* Implementing the Varargs Macros.  */

static bool
nds32_strict_argument_naming (cumulative_args_t ca ATTRIBUTE_UNUSED)
{
  /* Return true so that all the named arguments for FUNCTION_ARG have named=1.
     If return false, for the variadic function, all named arguments EXCEPT
     the last are treated as named.  */
  return true;
}


/* Trampolines for Nested Functions.  */

static void
nds32_asm_trampoline_template (FILE *f)
{
  if (TARGET_REDUCED_REGS)
    {
      /* Trampoline is not supported on reduced-set registers yet.  */
      sorry ("a nested function is not supported for reduced registers");
    }
  else
    {
      asm_fprintf (f, "\t! Trampoline code template\n");
      asm_fprintf (f, "\t! This code fragment will be copied "
		      "into stack on demand\n");

      asm_fprintf (f, "\tmfusr\t$r16,$pc\n");
      asm_fprintf (f, "\tlwi\t$r15,[$r16 + 20] "
		      "! load nested function address\n");
      asm_fprintf (f, "\tlwi\t$r16,[$r16 + 16] "
		      "! load chain_value\n");
      asm_fprintf (f, "\tjr\t$r15\n");
    }

  /* Preserve space ($pc + 16) for saving chain_value,
     nds32_trampoline_init will fill the value in this slot.  */
  asm_fprintf (f, "\t! space for saving chain_value\n");
  assemble_aligned_integer (UNITS_PER_WORD, const0_rtx);

  /* Preserve space ($pc + 20) for saving nested function address,
     nds32_trampoline_init will fill the value in this slot.  */
  asm_fprintf (f, "\t! space for saving nested function address\n");
  assemble_aligned_integer (UNITS_PER_WORD, const0_rtx);
}

/* Emit RTL insns to initialize the variable parts of a trampoline.  */
static void
nds32_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  int i;

  /* Nested function address.  */
  rtx fnaddr;
  /* The memory rtx that is going to
     be filled with chain_value.  */
  rtx chain_value_mem;
  /* The memory rtx that is going to
     be filled with nested function address.  */
  rtx nested_func_mem;

  /* Start address of trampoline code in stack, for doing cache sync.  */
  rtx sync_cache_addr;
  /* Temporary register for sync instruction.  */
  rtx tmp_reg;
  /* Instruction-cache sync instruction,
     requesting an argument as starting address.  */
  rtx isync_insn;
  /* For convenience reason of doing comparison.  */
  int tramp_align_in_bytes;

  /* Trampoline is not supported on reduced-set registers yet.  */
  if (TARGET_REDUCED_REGS)
    sorry ("a nested function is not supported for reduced registers");

  /* STEP 1: Copy trampoline code template into stack,
             fill up essential data into stack.  */

  /* Extract nested function address rtx.  */
  fnaddr = XEXP (DECL_RTL (fndecl), 0);

  /* m_tramp is memory rtx that is going to be filled with trampoline code.
     We have nds32_asm_trampoline_template() to emit template pattern.  */
  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  /* After copying trampoline code into stack,
     fill chain_value into stack.  */
  chain_value_mem = adjust_address (m_tramp, SImode, 16);
  emit_move_insn (chain_value_mem, chain_value);
  /* After copying trampoline code int stack,
     fill nested function address into stack.  */
  nested_func_mem = adjust_address (m_tramp, SImode, 20);
  emit_move_insn (nested_func_mem, fnaddr);

  /* STEP 2: Sync instruction-cache.  */

  /* We have successfully filled trampoline code into stack.
     However, in order to execute code in stack correctly,
     we must sync instruction cache.  */
  sync_cache_addr = XEXP (m_tramp, 0);
  tmp_reg         = gen_reg_rtx (SImode);
  isync_insn      = gen_unspec_volatile_isync (tmp_reg);

  /* Because nds32_cache_block_size is in bytes,
     we get trampoline alignment in bytes for convenient comparison.  */
  tramp_align_in_bytes = TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT;

  if (tramp_align_in_bytes >= nds32_cache_block_size
      && (tramp_align_in_bytes % nds32_cache_block_size) == 0)
    {
      /* Under this condition, the starting address of trampoline
         must be aligned to the starting address of each cache block
         and we do not have to worry about cross-boundary issue.  */
      for (i = 0;
	   i < (TRAMPOLINE_SIZE + nds32_cache_block_size - 1)
	       / nds32_cache_block_size;
	   i++)
	{
	  emit_move_insn (tmp_reg,
			  plus_constant (Pmode, sync_cache_addr,
					 nds32_cache_block_size * i));
	  emit_insn (isync_insn);
	}
    }
  else if (TRAMPOLINE_SIZE > nds32_cache_block_size)
    {
      /* The starting address of trampoline code
         may not be aligned to the cache block,
         so the trampoline code may be across two cache block.
         We need to sync the last element, which is 4-byte size,
         of trampoline template.  */
      for (i = 0;
	   i < (TRAMPOLINE_SIZE + nds32_cache_block_size - 1)
	       / nds32_cache_block_size;
	   i++)
	{
	  emit_move_insn (tmp_reg,
			  plus_constant (Pmode, sync_cache_addr,
					 nds32_cache_block_size * i));
	  emit_insn (isync_insn);
	}

      /* The last element of trampoline template is 4-byte size.  */
      emit_move_insn (tmp_reg,
		      plus_constant (Pmode, sync_cache_addr,
				     TRAMPOLINE_SIZE - 4));
      emit_insn (isync_insn);
    }
  else
    {
      /* This is the simplest case.
         Because TRAMPOLINE_SIZE is less than or
         equal to nds32_cache_block_size,
         we can just sync start address and
         the last element of trampoline code.  */

      /* Sync starting address of tampoline code.  */
      emit_move_insn (tmp_reg, sync_cache_addr);
      emit_insn (isync_insn);
      /* Sync the last element, which is 4-byte size,
         of trampoline template.  */
      emit_move_insn (tmp_reg,
		      plus_constant (Pmode, sync_cache_addr,
				     TRAMPOLINE_SIZE - 4));
      emit_insn (isync_insn);
    }

  /* Set instruction serialization barrier
     to guarantee the correct operations.  */
  emit_insn (gen_unspec_volatile_isb ());
}


/* Addressing Modes.  */

static bool
nds32_legitimate_address_p (enum machine_mode mode, rtx x, bool strict)
{
  /* For (mem:DI addr) or (mem:DF addr) case,
     we only allow 'addr' to be [reg], [symbol_ref],
                                [const], or [reg + const_int] pattern.  */
  if (mode == DImode || mode == DFmode)
    {
      /* Allow [Reg + const_int] addressing mode.  */
      if (GET_CODE (x) == PLUS)
	{
	  if (nds32_address_register_rtx_p (XEXP (x, 0), strict)
	      && nds32_legitimate_index_p (mode, XEXP (x, 1), strict)
	      && CONST_INT_P (XEXP (x, 1)))
	    return true;

	  else if (nds32_address_register_rtx_p (XEXP (x, 1), strict)
		   && nds32_legitimate_index_p (mode, XEXP (x, 0), strict)
		   && CONST_INT_P (XEXP (x, 0)))
	    return true;
	}

      /* Now check [reg], [symbol_ref], and [const].  */
      if (GET_CODE (x) != REG
	  && GET_CODE (x) != SYMBOL_REF
	  && GET_CODE (x) != CONST)
	return false;
    }

  /* Check if 'x' is a valid address.  */
  switch (GET_CODE (x))
    {
    case REG:
      /* (mem (reg A)) => [Ra] */
      return nds32_address_register_rtx_p (x, strict);

    case SYMBOL_REF:

      if (!TARGET_GP_DIRECT
	  && (reload_completed
	      || reload_in_progress
	      || lra_in_progress))
	return false;

      /* (mem (symbol_ref A)) => [symbol_ref] */
      return !currently_expanding_to_rtl;

    case CONST:

      if (!TARGET_GP_DIRECT
	  && (reload_completed
	      || reload_in_progress
	      || lra_in_progress))
	return false;

      /* (mem (const (...)))
         => [ + const_addr ], where const_addr = symbol_ref + const_int */
      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  rtx plus_op = XEXP (x, 0);

	  rtx op0 = XEXP (plus_op, 0);
	  rtx op1 = XEXP (plus_op, 1);

	  if (GET_CODE (op0) == SYMBOL_REF && CONST_INT_P (op1))
	    return true;
	  else
	    return false;
	}

	return false;

    case POST_MODIFY:
      /* (mem (post_modify (reg) (plus (reg) (reg))))
         => [Ra], Rb */
      /* (mem (post_modify (reg) (plus (reg) (const_int))))
         => [Ra], const_int */
      if (GET_CODE (XEXP (x, 0)) == REG
	  && GET_CODE (XEXP (x, 1)) == PLUS)
	{
	  rtx plus_op = XEXP (x, 1);

	  rtx op0 = XEXP (plus_op, 0);
	  rtx op1 = XEXP (plus_op, 1);

	  if (nds32_address_register_rtx_p (op0, strict)
	      && nds32_legitimate_index_p (mode, op1, strict))
	    return true;
	  else
	    return false;
	}

	return false;

    case POST_INC:
    case POST_DEC:
      /* (mem (post_inc reg)) => [Ra], 1/2/4 */
      /* (mem (post_dec reg)) => [Ra], -1/-2/-4 */
      /* The 1/2/4 or -1/-2/-4 have been displayed in nds32.md.
         We only need to deal with register Ra.  */
      if (nds32_address_register_rtx_p (XEXP (x, 0), strict))
	return true;
      else
	return false;

    case PLUS:
      /* (mem (plus reg const_int))
         => [Ra + imm] */
      /* (mem (plus reg reg))
         => [Ra + Rb] */
      /* (mem (plus (mult reg const_int) reg))
         => [Ra + Rb << sv] */
      if (nds32_address_register_rtx_p (XEXP (x, 0), strict)
	  && nds32_legitimate_index_p (mode, XEXP (x, 1), strict))
	return true;
      else if (nds32_address_register_rtx_p (XEXP (x, 1), strict)
	       && nds32_legitimate_index_p (mode, XEXP (x, 0), strict))
	return true;
      else
	return false;

    case LO_SUM:
      if (!TARGET_GP_DIRECT)
	return true;

    default:
      return false;
    }
}


/* Describing Relative Costs of Operations.  */

static int nds32_register_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
				     reg_class_t from,
				     reg_class_t to)
{
  if (from == HIGH_REGS || to == HIGH_REGS)
    return 6;

  return 2;
}

static int nds32_memory_move_cost (enum machine_mode mode ATTRIBUTE_UNUSED,
				   reg_class_t rclass ATTRIBUTE_UNUSED,
				   bool in ATTRIBUTE_UNUSED)
{
  return 8;
}

/* This target hook describes the relative costs of RTL expressions.
   Return 'true' when all subexpressions of x have been processed.
   Return 'false' to sum the costs of sub-rtx, plus cost of this operation.
   Refer to gcc/rtlanal.c for more information.  */
static bool
nds32_rtx_costs (rtx x,
		 int code,
		 int outer_code,
		 int opno ATTRIBUTE_UNUSED,
		 int *total,
		 bool speed)
{
  /* According to 'speed', goto suitable cost model section.  */
  if (speed)
    goto performance_cost;
  else
    goto size_cost;


performance_cost:
  /* This is section for performance cost model.  */

  /* In gcc/rtl.h, the default value of COSTS_N_INSNS(N) is N*4.
     We treat it as 4-cycle cost for each instruction
     under performance consideration.  */
  switch (code)
    {
    case SET:
      /* For 'SET' rtx, we need to return false
         so that it can recursively calculate costs.  */
      return false;

    case USE:
      /* Used in combine.c as a marker.  */
      *total = 0;
      break;

    case MULT:
      *total = COSTS_N_INSNS (1);
      break;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      *total = COSTS_N_INSNS (7);
      break;

    default:
      *total = COSTS_N_INSNS (1);
      break;
    }

  return true;


size_cost:
  /* This is section for size cost model.  */

  /* In gcc/rtl.h, the default value of COSTS_N_INSNS(N) is N*4.
     We treat it as 4-byte cost for each instruction
     under code size consideration.  */
  switch (code)
    {
    case SET:
      /* For 'SET' rtx, we need to return false
         so that it can recursively calculate costs.  */
      return false;

    case USE:
      /* Used in combine.c as a marker.  */
      *total = 0;
      break;

    case CONST_INT:
      /* All instructions involving constant operation
         need to be considered for cost evaluation.  */
      if (outer_code == SET)
	{
	  /* (set X imm5s), use movi55, 2-byte cost.
	     (set X imm20s), use movi, 4-byte cost.
	     (set X BIG_INT), use sethi/ori, 8-byte cost.  */
	  if (satisfies_constraint_Is05 (x))
	    *total = COSTS_N_INSNS (1) - 2;
	  else if (satisfies_constraint_Is20 (x))
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (2);
	}
      else if (outer_code == PLUS || outer_code == MINUS)
	{
	  /* Possible addi333/subi333 or subi45/addi45, 2-byte cost.
	     General case, cost 1 instruction with 4-byte.  */
	  if (satisfies_constraint_Iu05 (x))
	    *total = COSTS_N_INSNS (1) - 2;
	  else
	    *total = COSTS_N_INSNS (1);
	}
      else if (outer_code == ASHIFT)
	{
	  /* Possible slli333, 2-byte cost.
	     General case, cost 1 instruction with 4-byte.  */
	  if (satisfies_constraint_Iu03 (x))
	    *total = COSTS_N_INSNS (1) - 2;
	  else
	    *total = COSTS_N_INSNS (1);
	}
      else if (outer_code == ASHIFTRT || outer_code == LSHIFTRT)
	{
	  /* Possible srai45 or srli45, 2-byte cost.
	     General case, cost 1 instruction with 4-byte.  */
	  if (satisfies_constraint_Iu05 (x))
	    *total = COSTS_N_INSNS (1) - 2;
	  else
	    *total = COSTS_N_INSNS (1);
	}
      else
	{
	  /* For other cases, simply set it 4-byte cost.  */
	  *total = COSTS_N_INSNS (1);
	}
      break;

    case CONST_DOUBLE:
      /* It requires high part and low part processing, set it 8-byte cost.  */
      *total = COSTS_N_INSNS (2);
      break;

    default:
      /* For other cases, generally we set it 4-byte cost
         and stop resurively traversing.  */
      *total = COSTS_N_INSNS (1);
      break;
    }

  return true;
}

static int nds32_address_cost (rtx address,
			       enum machine_mode mode ATTRIBUTE_UNUSED,
			       addr_space_t as ATTRIBUTE_UNUSED,
			       bool speed)
{
  rtx plus0, plus1;
  enum rtx_code code;

  code = GET_CODE (address);

  /* According to 'speed', goto suitable cost model section.  */
  if (speed)
    goto performance_cost;
  else
    goto size_cost;

performance_cost:
  /* This is section for performance cost model.  */

  /* FALLTHRU, currently we use same cost model as size_cost.  */

size_cost:
  /* This is section for size cost model.  */

  switch (code)
    {
    case POST_MODIFY:
    case POST_INC:
    case POST_DEC:
      /* We encourage that rtx contains
         POST_MODIFY/POST_INC/POST_DEC behavior.  */
      return 0;

    case SYMBOL_REF:
      /* We can have gp-relative load/store for symbol_ref.
         Have it 4-byte cost.  */
      return COSTS_N_INSNS (1);

    case CONST:
      /* It is supposed to be the pattern (const (plus symbol_ref const_int)).
         Have it 4-byte cost.  */
      return COSTS_N_INSNS (1);

    case REG:
      /* Simply return 4-byte costs.  */
      return COSTS_N_INSNS (1);

    case PLUS:
      /* We do not need to check if the address is a legitimate address,
         because this hook is never called with an invalid address.
         But we better check the range of
         const_int value for cost, if it exists.  */
      plus0 = XEXP (address, 0);
      plus1 = XEXP (address, 1);

      if (REG_P (plus0) && CONST_INT_P (plus1))
        {
	  /* If it is possible to be lwi333/swi333 form,
	     make it 2-byte cost.  */
	  if (satisfies_constraint_Iu05 (plus1))
	    return (COSTS_N_INSNS (1) - 2);
	  else
	    return COSTS_N_INSNS (1);
	}

      /* For other 'plus' situation, make it cost 4-byte.  */
      return COSTS_N_INSNS (1);

    default:
      break;
    }

  return COSTS_N_INSNS (4);
}


/* Defining the Output Assembler Language.  */

/* -- The Overall Framework of an Assembler File.  */

static void
nds32_asm_file_start (void)
{
  int i;

  default_file_start ();

  /* Tell assembler which ABI we are using.  */
  fprintf (asm_out_file, "\t! ABI version\n");
  fprintf (asm_out_file, "\t.abi_2\n");

  /* Tell assembler that this asm code is generated by compiler.  */
  fprintf (asm_out_file, "\t! This asm file is generated by compiler\n");
  fprintf (asm_out_file, "\t.flag\tverbatim\n");
  /* Give assembler the size of each vector for interrupt handler.  */
  fprintf (asm_out_file, "\t! This vector size directive is required "
			 "for checking inconsistency on interrupt handler\n");
  fprintf (asm_out_file, "\t.vec_size\t%d\n", nds32_isr_vector_size);

  /* If user enables '-mforce-fp-as-gp' or compiles programs with -Os,
     the compiler may produce 'la $fp,_FP_BASE_' instruction
     at prologue for fp-as-gp optimization.
     We should emit weak reference of _FP_BASE_ to avoid undefined reference
     in case user does not pass '--relax' option to linker.  */
  if (TARGET_FORCE_FP_AS_GP || optimize_size)
    {
      fprintf (asm_out_file, "\t! This weak reference is required to do "
			     "fp-as-gp link time optimization\n");
      fprintf (asm_out_file, "\t.weak\t_FP_BASE_\n");
    }
  /* If user enables '-mex9', we should emit relaxation directive
     to tell linker that this file is allowed to do ex9 optimization.  */
  if (TARGET_EX9)
    {
      fprintf (asm_out_file, "\t! This relaxation directive is required "
			     "to do ex9 link time optimization\n");
      fprintf (asm_out_file, "\t.relax\tex9\n");
    }

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  if (TARGET_ISA_V2)
    fprintf (asm_out_file, "\t! ISA family\t\t: %s\n", "V2");
  if (TARGET_ISA_V3)
    fprintf (asm_out_file, "\t! ISA family\t\t: %s\n", "V3");
  if (TARGET_ISA_V3M)
    fprintf (asm_out_file, "\t! ISA family\t\t: %s\n", "V3M");

  fprintf (asm_out_file, "\t! Endian setting\t: %s\n",
			 ((TARGET_BIG_ENDIAN) ? "big-endian"
					      : "little-endian"));

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  fprintf (asm_out_file, "\t! Use conditional move\t\t: %s\n",
			 ((TARGET_CMOV) ? "Yes"
					: "No"));
  fprintf (asm_out_file, "\t! Use performance extension\t: %s\n",
			 ((TARGET_PERF_EXT) ? "Yes"
					    : "No"));

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  fprintf (asm_out_file, "\t! V3PUSH instructions\t: %s\n",
			 ((TARGET_V3PUSH) ? "Yes"
					  : "No"));
  fprintf (asm_out_file, "\t! 16-bit instructions\t: %s\n",
			 ((TARGET_16_BIT) ? "Yes"
					  : "No"));
  fprintf (asm_out_file, "\t! GP base access\t: %s\n",
			 ((TARGET_GP_DIRECT) ? "Yes"
					     : "No"));
  fprintf (asm_out_file, "\t! Reduced registers set\t: %s\n",
			 ((TARGET_REDUCED_REGS) ? "Yes"
						: "No"));

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  if (optimize_size)
    fprintf (asm_out_file, "\t! Optimization level\t: -Os\n");
  else
    fprintf (asm_out_file, "\t! Optimization level\t: -O%d\n", optimize);

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  fprintf (asm_out_file, "\t! Cache block size\t: %d\n",
			 nds32_cache_block_size);

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  /* Initialize isr vector information array before compiling functions.  */
  for (i = 0; i < NDS32_N_ISR_VECTORS; i++)
    {
      nds32_isr_vectors[i].category = NDS32_ISR_NONE;
      strcpy (nds32_isr_vectors[i].func_name, "");
      nds32_isr_vectors[i].save_reg = NDS32_PARTIAL_SAVE;
      nds32_isr_vectors[i].nested_type = NDS32_NOT_NESTED;
      nds32_isr_vectors[i].total_n_vectors = 0;
      strcpy (nds32_isr_vectors[i].nmi_name, "");
      strcpy (nds32_isr_vectors[i].warm_name, "");
    }
}

static void
nds32_asm_file_end (void)
{
  int i;

  /* If all the vectors are NDS32_ISR_NONE, we can return immediately.  */
  for (i = 0; i < NDS32_N_ISR_VECTORS; i++)
    if (nds32_isr_vectors[i].category != NDS32_ISR_NONE)
      break;

  if (i == NDS32_N_ISR_VECTORS)
    return;

  /* At least one vector is NOT NDS32_ISR_NONE,
     we should output isr vector information.  */
  fprintf (asm_out_file, "\t! ------------------------------------\n");
  fprintf (asm_out_file, "\t! The isr vector information:\n");
  fprintf (asm_out_file, "\t! ------------------------------------\n");

  /* Check reset handler first.  Its vector number is always 0.  */
  if (nds32_isr_vectors[0].category == NDS32_ISR_RESET)
    {
      nds32_emit_isr_reset_content ();
      fprintf (asm_out_file, "\t! ------------------------------------\n");
    }

  /* Check other vectors, starting from vector number 1.  */
  for (i = 1; i < NDS32_N_ISR_VECTORS; i++)
    {
      if (nds32_isr_vectors[i].category == NDS32_ISR_INTERRUPT
	  || nds32_isr_vectors[i].category == NDS32_ISR_EXCEPTION)
	{
	  /* Found one vector which is interupt or exception.
	     Output its jmptbl and vector section content.  */
	  fprintf (asm_out_file, "\t! interrupt/exception vector %02d\n", i);
	  fprintf (asm_out_file, "\t! ------------------------------------\n");
	  nds32_emit_isr_jmptbl_section (i);
	  fprintf (asm_out_file, "\t! ....................................\n");
	  nds32_emit_isr_vector_section (i);
	  fprintf (asm_out_file, "\t! ------------------------------------\n");
	}
    }

  fprintf (asm_out_file, "\t! ------------------------------------\n");
}

/* -- Output and Generation of Labels.  */

static void
nds32_asm_globalize_label (FILE *stream, const char *name)
{
  fputs ("\t.global\t", stream);
  assemble_name (stream, name);
  fputs ("\n", stream);
}

/* -- Output of Assembler Instructions.  */

static void
nds32_print_operand (FILE *stream, rtx x, int code)
{
  int op_value;

  switch (code)
    {
    case 0 :
      /* Do nothing special.  */
      break;

    case 'V':
      /* 'x' is supposed to be CONST_INT, get the value.  */
      gcc_assert (CONST_INT_P (x));
      op_value = INTVAL (x);

      /* According to the Andes architecture,
         the system/user register index range is 0 ~ 1023.
         In order to avoid conflict between user-specified-integer value
         and enum-specified-register value,
         the 'enum nds32_intrinsic_registers' value
         in nds32_intrinsic.h starts from 1024.  */
      if (op_value < 1024 && op_value >= 0)
	{
	  /* If user gives integer value directly (0~1023),
	     we just print out the value.  */
	  fprintf (stream, "%d", op_value);
	}
      else if (op_value < 0
	       || op_value >= ((int) ARRAY_SIZE (nds32_intrinsic_register_names)
			       + 1024))
	{
	  /* The enum index value for array size is out of range.  */
	  error ("intrinsic register index is out of range");
	}
      else
	{
	  /* If user applies normal way with __NDS32_REG_XXX__ enum data,
	     we can print out register name.  Remember to substract 1024.  */
	  fprintf (stream, "%s",
			   nds32_intrinsic_register_names[op_value - 1024]);
	}

      /* No need to handle following process, so return immediately.  */
      return;

    default :
      /* Unknown flag.  */
      output_operand_lossage ("invalid operand output code");
      break;
    }

  switch (GET_CODE (x))
    {
    case LABEL_REF:
    case SYMBOL_REF:
      output_addr_const (stream, x);
      break;

    case REG:
      /* Forbid using static chain register ($r16)
         on reduced-set registers configuration.  */
      if (TARGET_REDUCED_REGS
	  && REGNO (x) == STATIC_CHAIN_REGNUM)
	sorry ("a nested function is not supported for reduced registers");

      /* Normal cases, print out register name.  */
      fputs (reg_names[REGNO (x)], stream);
      break;

    case MEM:
      output_address (XEXP (x, 0));
      break;

    case CODE_LABEL:
    case CONST_INT:
    case CONST:
      output_addr_const (stream, x);
      break;

    default:
      /* Generally, output_addr_const () is able to handle most cases.
         We want to see what CODE could appear,
         so we use gcc_unreachable() to stop it.  */
      debug_rtx (x);
      gcc_unreachable ();
      break;
    }
}

static void
nds32_print_operand_address (FILE *stream, rtx x)
{
  rtx op0, op1;

  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
    case CONST:
      /* [ + symbol_ref] */
      /* [ + const_addr], where const_addr = symbol_ref + const_int */
      fputs ("[ + ", stream);
      output_addr_const (stream, x);
      fputs ("]", stream);
      break;

    case REG:
      /* Forbid using static chain register ($r16)
         on reduced-set registers configuration.  */
      if (TARGET_REDUCED_REGS
	  && REGNO (x) == STATIC_CHAIN_REGNUM)
	sorry ("a nested function is not supported for reduced registers");

      /* [Ra] */
      fprintf (stream, "[%s]", reg_names[REGNO (x)]);
      break;

    case PLUS:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      /* Checking op0, forbid using static chain register ($r16)
         on reduced-set registers configuration.  */
      if (TARGET_REDUCED_REGS
	  && REG_P (op0)
	  && REGNO (op0) == STATIC_CHAIN_REGNUM)
	sorry ("a nested function is not supported for reduced registers");
      /* Checking op1, forbid using static chain register ($r16)
         on reduced-set registers configuration.  */
      if (TARGET_REDUCED_REGS
	  && REG_P (op1)
	  && REGNO (op1) == STATIC_CHAIN_REGNUM)
	sorry ("a nested function is not supported for reduced registers");

      if (REG_P (op0) && CONST_INT_P (op1))
	{
	  /* [Ra + imm] */
	  fprintf (stream, "[%s + (%d)]",
			   reg_names[REGNO (op0)], (int)INTVAL (op1));
	}
      else if (REG_P (op0) && REG_P (op1))
	{
	  /* [Ra + Rb] */
	  fprintf (stream, "[%s + %s]",
			   reg_names[REGNO (op0)], reg_names[REGNO (op1)]);
	}
      else if (GET_CODE (op0) == MULT && REG_P (op1))
	{
	  /* [Ra + Rb << sv]
	     From observation, the pattern looks like:
	     (plus:SI (mult:SI (reg:SI 58)
	                       (const_int 4 [0x4]))
	              (reg/f:SI 57)) */
	  int sv;

	  /* We need to set sv to output shift value.  */
	  if (INTVAL (XEXP (op0, 1)) == 1)
	    sv = 0;
	  else if (INTVAL (XEXP (op0, 1)) == 2)
	    sv = 1;
	  else if (INTVAL (XEXP (op0, 1)) == 4)
	    sv = 2;
	  else
	    gcc_unreachable ();

	  fprintf (stream, "[%s + %s << %d]",
			   reg_names[REGNO (op1)],
			   reg_names[REGNO (XEXP (op0, 0))],
			   sv);
	}
      else
	{
	  /* The control flow is not supposed to be here.  */
	  debug_rtx (x);
	  gcc_unreachable ();
	}

      break;

    case POST_MODIFY:
      /* (post_modify (regA) (plus (regA) (regB)))
         (post_modify (regA) (plus (regA) (const_int)))
         We would like to extract
         regA and regB (or const_int) from plus rtx.  */
      op0 = XEXP (XEXP (x, 1), 0);
      op1 = XEXP (XEXP (x, 1), 1);

      /* Checking op0, forbid using static chain register ($r16)
         on reduced-set registers configuration.  */
      if (TARGET_REDUCED_REGS
	  && REG_P (op0)
	  && REGNO (op0) == STATIC_CHAIN_REGNUM)
	sorry ("a nested function is not supported for reduced registers");
      /* Checking op1, forbid using static chain register ($r16)
         on reduced-set registers configuration.  */
      if (TARGET_REDUCED_REGS
	  && REG_P (op1)
	  && REGNO (op1) == STATIC_CHAIN_REGNUM)
	sorry ("a nested function is not supported for reduced registers");

      if (REG_P (op0) && REG_P (op1))
	{
	  /* [Ra], Rb */
	  fprintf (stream, "[%s], %s",
			   reg_names[REGNO (op0)], reg_names[REGNO (op1)]);
	}
      else if (REG_P (op0) && CONST_INT_P (op1))
	{
	  /* [Ra], imm */
	  fprintf (stream, "[%s], %d",
			   reg_names[REGNO (op0)], (int)INTVAL (op1));
	}
      else
	{
	  /* The control flow is not supposed to be here.  */
	  debug_rtx (x);
	  gcc_unreachable ();
	}

      break;

    case POST_INC:
    case POST_DEC:
      op0 = XEXP (x, 0);

      /* Checking op0, forbid using static chain register ($r16)
         on reduced-set registers configuration.  */
      if (TARGET_REDUCED_REGS
	  && REG_P (op0)
	  && REGNO (op0) == STATIC_CHAIN_REGNUM)
	sorry ("a nested function is not supported for reduced registers");

      if (REG_P (op0))
	{
	  /* "[Ra], 1/2/4" or "[Ra], -1/-2/-4"
	     The 1/2/4 or -1/-2/-4 have been displayed in nds32.md.
	     We only need to deal with register Ra.  */
	  fprintf (stream, "[%s]", reg_names[REGNO (op0)]);
	}
      else
	{
	  /* The control flow is not supposed to be here.  */
	  debug_rtx (x);
	  gcc_unreachable ();
	}

      break;

    default :
      /* Generally, output_addr_const () is able to handle most cases.
         We want to see what CODE could appear,
         so we use gcc_unreachable() to stop it.  */
      debug_rtx (x);
      gcc_unreachable ();
      break;
    }
}


/* Defining target-specific uses of __attribute__.  */

/* Add some checking after merging attributes.  */
static tree
nds32_merge_decl_attributes (tree olddecl, tree newdecl)
{
  tree combined_attrs;

  /* Create combined attributes.  */
  combined_attrs = merge_attributes (DECL_ATTRIBUTES (olddecl),
				     DECL_ATTRIBUTES (newdecl));

  /* Since newdecl is acutally a duplicate of olddecl,
     we can take olddecl for some operations.  */
  if (TREE_CODE (olddecl) == FUNCTION_DECL)
    {
      /* Check isr-specific attributes conflict.  */
      nds32_check_isr_attrs_conflict (olddecl, combined_attrs);
    }

  return combined_attrs;
}

/* Add some checking when inserting attributes.  */
static void
nds32_insert_attributes (tree decl, tree *attributes)
{
  /* For function declaration, we need to check isr-specific attributes:
       1. Call nds32_check_isr_attrs_conflict() to check any conflict.
       2. Check valid integer value for interrupt/exception.
       3. Check valid integer value for reset.
       4. Check valid function for nmi/warm.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      tree func_attrs;
      tree intr, excp, reset;

      /* Pick up function attributes.  */
      func_attrs = *attributes;

      /* 1. Call nds32_check_isr_attrs_conflict() to check any conflict.  */
      nds32_check_isr_attrs_conflict (decl, func_attrs);

      /* Now we are starting to check valid id value
         for interrupt/exception/reset.
         Note that we ONLY check its validity here.
         To construct isr vector information, it is still performed
         by nds32_construct_isr_vectors_information().  */
      intr  = lookup_attribute ("interrupt", func_attrs);
      excp  = lookup_attribute ("exception", func_attrs);
      reset = lookup_attribute ("reset", func_attrs);

      if (intr || excp)
	{
	  /* Deal with interrupt/exception.  */
	  tree id_list;
	  unsigned int lower_bound, upper_bound;

	  /* The way to handle interrupt or exception is the same,
	     we just need to take care of actual vector number.
	     For interrupt(0..63), the actual vector number is (9..72).
	     For exception(1..8), the actual vector number is (1..8).  */
	  lower_bound = (intr) ? (0) : (1);
	  upper_bound = (intr) ? (63) : (8);

	  /* Prepare id list so that we can traverse id value.  */
	  id_list = (intr) ? (TREE_VALUE (intr)) : (TREE_VALUE (excp));

	  /* 2. Check valid integer value for interrupt/exception.  */
	  while (id_list)
	    {
	      tree id;

	      /* Pick up each vector id value.  */
	      id = TREE_VALUE (id_list);
	      /* Issue error if it is not a valid integer value.  */
	      if (TREE_CODE (id) != INTEGER_CST
		  || TREE_INT_CST_LOW (id) < lower_bound
		  || TREE_INT_CST_LOW (id) > upper_bound)
		error ("invalid id value for interrupt/exception attribute");

	      /* Advance to next id.  */
	      id_list = TREE_CHAIN (id_list);
	    }
	}
      else if (reset)
	{
	  /* Deal with reset.  */
	  tree id_list;
	  tree id;
	  tree nmi, warm;
	  unsigned int lower_bound;
	  unsigned int upper_bound;

	  /* Prepare id_list and identify id value so that
	     we can check if total number of vectors is valid.  */
	  id_list = TREE_VALUE (reset);
	  id = TREE_VALUE (id_list);

	  /* The maximum numbers for user's interrupt is 64.  */
	  lower_bound = 0;
	  upper_bound = 64;

	  /* 3. Check valid integer value for reset.  */
	  if (TREE_CODE (id) != INTEGER_CST
	      || TREE_INT_CST_LOW (id) < lower_bound
	      || TREE_INT_CST_LOW (id) > upper_bound)
	    error ("invalid id value for reset attribute");

	  /* 4. Check valid function for nmi/warm.  */
	  nmi  = lookup_attribute ("nmi", func_attrs);
	  warm = lookup_attribute ("warm", func_attrs);

	  if (nmi != NULL_TREE)
	    {
	      tree nmi_func_list;
	      tree nmi_func;

	      nmi_func_list = TREE_VALUE (nmi);
	      nmi_func = TREE_VALUE (nmi_func_list);

	      /* Issue error if it is not a valid nmi function.  */
	      if (TREE_CODE (nmi_func) != IDENTIFIER_NODE)
		error ("invalid nmi function for reset attribute");
	    }

	  if (warm != NULL_TREE)
	    {
	      tree warm_func_list;
	      tree warm_func;

	      warm_func_list = TREE_VALUE (warm);
	      warm_func = TREE_VALUE (warm_func_list);

	      /* Issue error if it is not a valid warm function.  */
	      if (TREE_CODE (warm_func) != IDENTIFIER_NODE)
		error ("invalid warm function for reset attribute");
	    }
	}
      else
	{
	  /* No interrupt, exception, or reset attribute is set.  */
	  return;
	}
    }
}

static bool
nds32_option_pragma_parse (tree args ATTRIBUTE_UNUSED,
			   tree pop_target ATTRIBUTE_UNUSED)
{
  /* Currently, we do not parse any pragma target by ourself,
     so just simply return false.  */
  return false;
}

static void
nds32_option_override (void)
{
  /* After all the command options have been parsed,
     we shall deal with some flags for changing compiler settings.  */

  /* At first, we check if we have to strictly
     set some flags based on ISA family.  */
  if (TARGET_ISA_V2)
    {
      /* Under V2 ISA, we need to strictly disable TARGET_V3PUSH.  */
      target_flags &= ~MASK_V3PUSH;
    }
  if (TARGET_ISA_V3)
    {
      /* Under V3 ISA, currently nothing should be strictly set.  */
    }
  if (TARGET_ISA_V3M)
    {
      /* Under V3M ISA, we need to strictly enable TARGET_REDUCED_REGS.  */
      target_flags |= MASK_REDUCED_REGS;
      /* Under V3M ISA, we need to strictly disable TARGET_PERF_EXT.  */
      target_flags &= ~MASK_PERF_EXT;
    }

  /* See if we are using reduced-set registers:
       $r0~$r5, $r6~$r10, $r15, $r28, $r29, $r30, $r31
     If so, we must forbid using $r11~$r14, $r16~$r27.  */
  if (TARGET_REDUCED_REGS)
    {
      int r;

      /* Prevent register allocator from
         choosing it as doing register allocation.  */
      for (r = 11; r <= 14; r++)
	fixed_regs[r] = call_used_regs[r] = 1;
      for (r = 16; r <= 27; r++)
	fixed_regs[r] = call_used_regs[r] = 1;
    }

  /* See if user explicitly would like to use fp-as-gp optimization.
     If so, we must prevent $fp from being allocated
     during register allocation.  */
  if (TARGET_FORCE_FP_AS_GP)
    fixed_regs[FP_REGNUM] = call_used_regs[FP_REGNUM] = 1;

  if (!TARGET_16_BIT)
    {
      /* Under no 16 bit ISA, we need to strictly disable TARGET_V3PUSH.  */
      target_flags &= ~MASK_V3PUSH;
    }

  /* Currently, we don't support PIC code generation yet.  */
  if (flag_pic)
    sorry ("not support -fpic");
}


/* Miscellaneous Parameters.  */

static void
nds32_init_builtins (void)
{
  tree pointer_type_node  = build_pointer_type (integer_type_node);

  tree void_ftype_void    = build_function_type (void_type_node,
						 void_list_node);

  tree void_ftype_pint    = build_function_type_list (void_type_node,
						      pointer_type_node,
						      NULL_TREE);

  tree int_ftype_int      = build_function_type_list (integer_type_node,
						      integer_type_node,
						      NULL_TREE);

  tree void_ftype_int_int = build_function_type_list (void_type_node,
						      integer_type_node,
						      integer_type_node,
						      NULL_TREE);

  /* Cache.  */
  add_builtin_function ("__builtin_nds32_isync",  void_ftype_pint,
			NDS32_BUILTIN_ISYNC,
			BUILT_IN_MD, NULL, NULL_TREE);
  add_builtin_function ("__builtin_nds32_isb",  void_ftype_void,
			NDS32_BUILTIN_ISB,
			BUILT_IN_MD, NULL, NULL_TREE);

  /* Register Transfer.  */
  add_builtin_function ("__builtin_nds32_mfsr",  int_ftype_int,
			NDS32_BUILTIN_MFSR,
			BUILT_IN_MD, NULL, NULL_TREE);
  add_builtin_function ("__builtin_nds32_mfusr", int_ftype_int,
			NDS32_BUILTIN_MFUSR,
			BUILT_IN_MD, NULL, NULL_TREE);
  add_builtin_function ("__builtin_nds32_mtsr",  void_ftype_int_int,
			NDS32_BUILTIN_MTSR,
			BUILT_IN_MD, NULL, NULL_TREE);
  add_builtin_function ("__builtin_nds32_mtusr", void_ftype_int_int,
			NDS32_BUILTIN_MTUSR,
			BUILT_IN_MD, NULL, NULL_TREE);

  /* Interrupt.  */
  add_builtin_function ("__builtin_nds32_setgie_en",  void_ftype_void,
			NDS32_BUILTIN_SETGIE_EN,
			BUILT_IN_MD, NULL, NULL_TREE);
  add_builtin_function ("__builtin_nds32_setgie_dis", void_ftype_void,
			NDS32_BUILTIN_SETGIE_DIS,
			BUILT_IN_MD, NULL, NULL_TREE);
}

static rtx
nds32_expand_builtin (tree exp,
		      rtx target,
		      rtx subtarget ATTRIBUTE_UNUSED,
		      enum machine_mode mode ATTRIBUTE_UNUSED,
		      int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);

  int fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    /* Cache.  */
    case NDS32_BUILTIN_ISYNC:
      return nds32_expand_builtin_null_ftype_reg
	     (CODE_FOR_unspec_volatile_isync, exp, target);
    case NDS32_BUILTIN_ISB:
      /* Since there are no result and operands for isb instruciton,
         we can simply emit this rtx.  */
      emit_insn (gen_unspec_volatile_isb ());
      return target;

    /* Register Transfer.  */
    case NDS32_BUILTIN_MFSR:
      return nds32_expand_builtin_reg_ftype_imm
	     (CODE_FOR_unspec_volatile_mfsr, exp, target);
    case NDS32_BUILTIN_MFUSR:
      return nds32_expand_builtin_reg_ftype_imm
	     (CODE_FOR_unspec_volatile_mfusr, exp, target);
    case NDS32_BUILTIN_MTSR:
      return nds32_expand_builtin_null_ftype_reg_imm
	     (CODE_FOR_unspec_volatile_mtsr, exp, target);
    case NDS32_BUILTIN_MTUSR:
      return nds32_expand_builtin_null_ftype_reg_imm
	     (CODE_FOR_unspec_volatile_mtusr, exp, target);

    /* Interrupt.  */
    case NDS32_BUILTIN_SETGIE_EN:
      /* Since there are no result and operands for setgie.e instruciton,
         we can simply emit this rtx.  */
      emit_insn (gen_unspec_volatile_setgie_en ());
      return target;
    case NDS32_BUILTIN_SETGIE_DIS:
      /* Since there are no result and operands for setgie.d instruciton,
         we can simply emit this rtx.  */
      emit_insn (gen_unspec_volatile_setgie_dis ());
      return target;

    default:
      gcc_unreachable ();
    }

  return NULL_RTX;
}


/* ------------------------------------------------------------------------ */

/* PART 4: Implemet extern function definitions,
           the prototype is in nds32-protos.h.  */

/* Defining Data Structures for Per-function Information.  */

void
nds32_init_expanders (void)
{
  /* Arrange to initialize and mark the machine per-function status.  */
  init_machine_status = nds32_init_machine_status;
}


/* Register Usage.  */

/* -- How Values Fit in Registers.  */

int
nds32_hard_regno_nregs (int regno ATTRIBUTE_UNUSED,
			enum machine_mode mode)
{
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
}

int
nds32_hard_regno_mode_ok (int regno, enum machine_mode mode)
{
  /* Restrict double-word quantities to even register pairs.  */
  if (HARD_REGNO_NREGS (regno, mode) == 1
      || !((regno) & 1))
    return 1;

  return 0;
}


/* Register Classes.  */

enum reg_class
nds32_regno_reg_class (int regno)
{
  /* Refer to nds32.h for more register class details.  */

  if (regno >= 0 && regno <= 7)
    return LOW_REGS;
  else if (regno >= 8 && regno <= 11)
    return MIDDLE_REGS;
  else if (regno >= 12 && regno <= 14)
    return HIGH_REGS;
  else if (regno == 15)
    return R15_TA_REG;
  else if (regno >= 16 && regno <= 19)
    return MIDDLE_REGS;
  else if (regno >= 20 && regno <= 31)
    return HIGH_REGS;
  else if (regno == 32 || regno == 33)
    return FRAME_REGS;
  else
    return NO_REGS;
}


/* Stack Layout and Calling Conventions.  */

/* -- Basic Stack Layout.  */

rtx
nds32_return_addr_rtx (int count,
		       rtx frameaddr ATTRIBUTE_UNUSED)
{
  /* There is no way to determine the return address
     if frameaddr is the frame that has 'count' steps
     up from current frame.  */
  if (count != 0)
    return NULL_RTX;

  /* If count == 0, it means we are at current frame,
     the return address is $r30 ($lp).  */
  return get_hard_reg_initial_val (Pmode, LP_REGNUM);
}

/* -- Eliminating Frame Pointer and Arg Pointer.  */

HOST_WIDE_INT
nds32_initial_elimination_offset (unsigned int from_reg, unsigned int to_reg)
{
  HOST_WIDE_INT offset;

  /* Compute and setup stack frame size.
     The result will be in cfun->machine.  */
  nds32_compute_stack_frame ();

  /* Remember to consider
     cfun->machine->callee_saved_area_padding_bytes
     when calculating offset.  */
  if (from_reg == ARG_POINTER_REGNUM && to_reg == STACK_POINTER_REGNUM)
    {
      offset = (cfun->machine->fp_size
	        + cfun->machine->gp_size
		+ cfun->machine->lp_size
		+ cfun->machine->callee_saved_regs_size
		+ cfun->machine->callee_saved_area_padding_bytes
		+ cfun->machine->local_size
		+ cfun->machine->out_args_size);
    }
  else if (from_reg == ARG_POINTER_REGNUM
	   && to_reg == HARD_FRAME_POINTER_REGNUM)
    {
      offset = 0;
    }
  else if (from_reg == FRAME_POINTER_REGNUM
	   && to_reg == STACK_POINTER_REGNUM)
    {
      offset = (cfun->machine->local_size + cfun->machine->out_args_size);
    }
  else if (from_reg == FRAME_POINTER_REGNUM
	   && to_reg == HARD_FRAME_POINTER_REGNUM)
    {
      offset = (-1) * (cfun->machine->fp_size
		       + cfun->machine->gp_size
		       + cfun->machine->lp_size
		       + cfun->machine->callee_saved_regs_size
		       + cfun->machine->callee_saved_area_padding_bytes);
    }
  else
    {
      gcc_unreachable ();
    }

  return offset;
}

/* -- Passing Arguments in Registers.  */

void
nds32_init_cumulative_args (CUMULATIVE_ARGS *cum,
			    tree fntype ATTRIBUTE_UNUSED,
			    rtx libname ATTRIBUTE_UNUSED,
			    tree fndecl ATTRIBUTE_UNUSED,
			    int n_named_args ATTRIBUTE_UNUSED)
{
  /* Initial available registers
     (in offset, corresponding to NDS32_GPR_ARG_FIRST_REGNUM)
     for passing arguments.  */
  cum->reg_offset = 0;
}

/* -- Function Entry and Exit.  */

/* Function for normal multiple push prologue.  */
void
nds32_expand_prologue (void)
{
  int fp_adjust;
  int sp_adjust;
  int en4_const;

  rtx Rb, Re;
  rtx push_insn;
  rtx fp_adjust_insn, sp_adjust_insn;

  /* Before computing everything for stack frame size,
     we check if it is still worth to use fp_as_gp optimization.
     If it is, the 'df_regs_ever_live_p (FP_REGNUM)' will be set
     so that $fp will be saved on stack.  */
  cfun->machine->fp_as_gp_p = nds32_fp_as_gp_check_available ();

  /* Compute and setup stack frame size.
     The result will be in cfun->machine.  */
  nds32_compute_stack_frame ();

  /* If the function is 'naked',
     we do not have to generate prologue code fragment.  */
  if (cfun->machine->naked_p)
    return;

  /* Get callee_first_regno and callee_last_regno.  */
  Rb = gen_rtx_REG (SImode, cfun->machine->callee_saved_regs_first_regno);
  Re = gen_rtx_REG (SImode, cfun->machine->callee_saved_regs_last_regno);

  /* push_insn = gen_stack_push_multiple(first_regno, last_regno),
     the pattern 'stack_push_multiple' is implemented in nds32.md.
     For En4 field, we have to calculate its constant value.
     Refer to Andes ISA for more information.  */
  en4_const = 0;
  if (cfun->machine->fp_size)
    en4_const += 8;
  if (cfun->machine->gp_size)
    en4_const += 4;
  if (cfun->machine->lp_size)
    en4_const += 2;

  /* If $fp, $gp, $lp, and all callee-save registers are NOT required
     to be saved, we don't have to create multiple push instruction.
     Otherwise, a multiple push instruction is needed.  */
  if (!(REGNO (Rb) == SP_REGNUM && REGNO (Re) == SP_REGNUM && en4_const == 0))
    {
      /* Create multiple push instruction rtx.  */
      push_insn = nds32_gen_stack_push_multiple (Rb, Re, GEN_INT (en4_const));
      /* Emit rtx into instructions list and receive INSN rtx form.  */
      push_insn = emit_insn (push_insn);

      /* The insn rtx 'push_insn' will change frame layout.
         We need to use RTX_FRAME_RELATED_P so that GCC is able to
         generate CFI (Call Frame Information) stuff.  */
      RTX_FRAME_RELATED_P (push_insn) = 1;
    }

  /* Check frame_pointer_needed to see
     if we shall emit fp adjustment instruction.  */
  if (frame_pointer_needed)
    {
      /* adjust $fp = $sp + ($fp size) + ($gp size) + ($lp size)
                          + (4 * callee-saved-registers)
         Note: No need to adjust
               cfun->machine->callee_saved_area_padding_bytes,
               because, at this point, stack pointer is just
               at the position after push instruction.  */
      fp_adjust = cfun->machine->fp_size
		  + cfun->machine->gp_size
		  + cfun->machine->lp_size
		  + cfun->machine->callee_saved_regs_size;
      fp_adjust_insn = gen_addsi3 (hard_frame_pointer_rtx,
				   stack_pointer_rtx,
				   GEN_INT (fp_adjust));
      /* Emit rtx into instructions list and receive INSN rtx form.  */
      fp_adjust_insn = emit_insn (fp_adjust_insn);
    }

  /* Adjust $sp = $sp - local_size - out_args_size
                      - callee_saved_area_padding_bytes.  */
  sp_adjust = cfun->machine->local_size
	      + cfun->machine->out_args_size
	      + cfun->machine->callee_saved_area_padding_bytes;
  /* sp_adjust value may be out of range of the addi instruction,
     create alternative add behavior with TA_REGNUM if necessary,
     using NEGATIVE value to tell that we are decreasing address.  */
  sp_adjust = nds32_force_addi_stack_int ( (-1) * sp_adjust);
  if (sp_adjust)
    {
      /* Generate sp adjustment instruction if and only if sp_adjust != 0.  */
      sp_adjust_insn = gen_addsi3 (stack_pointer_rtx,
				   stack_pointer_rtx,
				   GEN_INT (-1 * sp_adjust));
      /* Emit rtx into instructions list and receive INSN rtx form.  */
      sp_adjust_insn = emit_insn (sp_adjust_insn);

      /* The insn rtx 'sp_adjust_insn' will change frame layout.
         We need to use RTX_FRAME_RELATED_P so that GCC is able to
         generate CFI (Call Frame Information) stuff.  */
      RTX_FRAME_RELATED_P (sp_adjust_insn) = 1;
    }

  /* Prevent the instruction scheduler from
     moving instructions across the boundary.  */
  emit_insn (gen_blockage ());
}

/* Function for normal multiple pop epilogue.  */
void
nds32_expand_epilogue (void)
{
  int sp_adjust;
  int en4_const;

  rtx Rb, Re;
  rtx pop_insn;
  rtx sp_adjust_insn;

  /* Compute and setup stack frame size.
     The result will be in cfun->machine.  */
  nds32_compute_stack_frame ();

  /* Prevent the instruction scheduler from
     moving instructions across the boundary.  */
  emit_insn (gen_blockage ());

  /* If the function is 'naked', we do not have to generate
     epilogue code fragment BUT 'ret' instruction.  */
  if (cfun->machine->naked_p)
    {
      /* Generate return instruction by using
         unspec_volatile_func_return pattern.
         Make sure this instruction is after gen_blockage().
         NOTE that $lp will become 'live'
         after this instruction has been emitted.  */
      emit_insn (gen_unspec_volatile_func_return ());
      return;
    }

  if (frame_pointer_needed)
    {
      /* adjust $sp = $fp - ($fp size) - ($gp size) - ($lp size)
                          - (4 * callee-saved-registers)
         Note: No need to adjust
               cfun->machine->callee_saved_area_padding_bytes,
               because we want to adjust stack pointer
               to the position for pop instruction.  */
      sp_adjust = cfun->machine->fp_size
		  + cfun->machine->gp_size
		  + cfun->machine->lp_size
		  + cfun->machine->callee_saved_regs_size;
      sp_adjust_insn = gen_addsi3 (stack_pointer_rtx,
				   hard_frame_pointer_rtx,
				   GEN_INT (-1 * sp_adjust));
      /* Emit rtx into instructions list and receive INSN rtx form.  */
      sp_adjust_insn = emit_insn (sp_adjust_insn);
    }
  else
    {
      /* If frame pointer is NOT needed,
         we cannot calculate the sp adjustment from frame pointer.
         Instead, we calculate the adjustment by local_size,
         out_args_size, and callee_saved_area_padding_bytes.
         Notice that such sp adjustment value may be out of range,
         so we have to deal with it as well.  */

      /* Adjust $sp = $sp + local_size + out_args_size
                          + callee_saved_area_padding_bytes.  */
      sp_adjust = cfun->machine->local_size
		  + cfun->machine->out_args_size
		  + cfun->machine->callee_saved_area_padding_bytes;
      /* sp_adjust value may be out of range of the addi instruction,
         create alternative add behavior with TA_REGNUM if necessary,
         using POSITIVE value to tell that we are increasing address.  */
      sp_adjust = nds32_force_addi_stack_int (sp_adjust);
      if (sp_adjust)
	{
	  /* Generate sp adjustment instruction
	     if and only if sp_adjust != 0.  */
	  sp_adjust_insn = gen_addsi3 (stack_pointer_rtx,
				       stack_pointer_rtx,
				       GEN_INT (sp_adjust));
	  /* Emit rtx into instructions list and receive INSN rtx form.  */
	  sp_adjust_insn = emit_insn (sp_adjust_insn);
	}
    }

  /* Get callee_first_regno and callee_last_regno.  */
  Rb = gen_rtx_REG (SImode, cfun->machine->callee_saved_regs_first_regno);
  Re = gen_rtx_REG (SImode, cfun->machine->callee_saved_regs_last_regno);

  /* pop_insn = gen_stack_pop_multiple(first_regno, last_regno),
     the pattern 'stack_pop_multiple' is implementad in nds32.md.
     For En4 field, we have to calculate its constant value.
     Refer to Andes ISA for more information.  */
  en4_const = 0;
  if (cfun->machine->fp_size)
    en4_const += 8;
  if (cfun->machine->gp_size)
    en4_const += 4;
  if (cfun->machine->lp_size)
    en4_const += 2;

  /* If $fp, $gp, $lp, and all callee-save registers are NOT required
     to be saved, we don't have to create multiple pop instruction.
     Otherwise, a multiple pop instruction is needed.  */
  if (!(REGNO (Rb) == SP_REGNUM && REGNO (Re) == SP_REGNUM && en4_const == 0))
    {
      /* Create multiple pop instruction rtx.  */
      pop_insn = nds32_gen_stack_pop_multiple (Rb, Re, GEN_INT (en4_const));
      /* Emit pop instruction.  */
      emit_insn (pop_insn);
    }

  /* Generate return instruction by using
     unspec_volatile_func_return pattern.  */
  emit_insn (gen_unspec_volatile_func_return ());
}

/* Function for v3push prologue.  */
void
nds32_expand_prologue_v3push (void)
{
  int fp_adjust;
  int sp_adjust;

  rtx Rb, Re;
  rtx push_insn;
  rtx fp_adjust_insn, sp_adjust_insn;

  /* Before computing everything for stack frame size,
     we check if it is still worth to use fp_as_gp optimization.
     If it is, the 'df_regs_ever_live_p (FP_REGNUM)' will be set
     so that $fp will be saved on stack.  */
  cfun->machine->fp_as_gp_p = nds32_fp_as_gp_check_available ();

  /* Compute and setup stack frame size.
     The result will be in cfun->machine.  */
  nds32_compute_stack_frame ();

  /* If the function is 'naked',
     we do not have to generate prologue code fragment.  */
  if (cfun->machine->naked_p)
    return;

  /* Get callee_first_regno and callee_last_regno.  */
  Rb = gen_rtx_REG (SImode, cfun->machine->callee_saved_regs_first_regno);
  Re = gen_rtx_REG (SImode, cfun->machine->callee_saved_regs_last_regno);

  /* Calculate sp_adjust first to test if 'push25 Re,imm8u' is available,
     where imm8u has to be 8-byte alignment.  */
  sp_adjust = cfun->machine->local_size
	      + cfun->machine->out_args_size
	      + cfun->machine->callee_saved_area_padding_bytes;

  if (satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
      && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust))
    {
      /* We can use 'push25 Re,imm8u'.  */

      /* push_insn = gen_stack_v3push(last_regno, sp_adjust),
         the pattern 'stack_v3push' is implemented in nds32.md.
         The (const_int 14) means v3push always push { $fp $gp $lp }.  */
      push_insn = nds32_gen_stack_v3push (Rb, Re,
					  GEN_INT (14), GEN_INT (sp_adjust));
      /* emit rtx into instructions list and receive INSN rtx form */
      push_insn = emit_insn (push_insn);

      /* The insn rtx 'push_insn' will change frame layout.
         We need to use RTX_FRAME_RELATED_P so that GCC is able to
         generate CFI (Call Frame Information) stuff.  */
      RTX_FRAME_RELATED_P (push_insn) = 1;

      /* Check frame_pointer_needed to see
         if we shall emit fp adjustment instruction.  */
      if (frame_pointer_needed)
	{
	  /* adjust $fp = $sp   + 4         ($fp size)
	                        + 4         ($gp size)
	                        + 4         ($lp size)
	                        + (4 * n)   (callee-saved registers)
	                        + sp_adjust ('push25 Re,imm8u')
	     Note: Since we use 'push25 Re,imm8u',
	           the position of stack pointer is further
	           changed after push instruction.
	           Hence, we need to take sp_adjust value
	           into consideration.  */
	  fp_adjust = cfun->machine->fp_size
		      + cfun->machine->gp_size
		      + cfun->machine->lp_size
		      + cfun->machine->callee_saved_regs_size
		      + sp_adjust;
	  fp_adjust_insn = gen_addsi3 (hard_frame_pointer_rtx,
				       stack_pointer_rtx,
				       GEN_INT (fp_adjust));
	  /* Emit rtx into instructions list and receive INSN rtx form.  */
	  fp_adjust_insn = emit_insn (fp_adjust_insn);
	}
    }
  else
    {
      /* We have to use 'push25 Re,0' and
         expand one more instruction to adjust $sp later.  */

      /* push_insn = gen_stack_v3push(last_regno, sp_adjust),
         the pattern 'stack_v3push' is implemented in nds32.md.
         The (const_int 14) means v3push always push { $fp $gp $lp }.  */
      push_insn = nds32_gen_stack_v3push (Rb, Re,
					  GEN_INT (14), GEN_INT (0));
      /* Emit rtx into instructions list and receive INSN rtx form.  */
      push_insn = emit_insn (push_insn);

      /* The insn rtx 'push_insn' will change frame layout.
         We need to use RTX_FRAME_RELATED_P so that GCC is able to
         generate CFI (Call Frame Information) stuff.  */
      RTX_FRAME_RELATED_P (push_insn) = 1;

      /* Check frame_pointer_needed to see
         if we shall emit fp adjustment instruction.  */
      if (frame_pointer_needed)
	{
	  /* adjust $fp = $sp + 4        ($fp size)
	                      + 4        ($gp size)
	                      + 4        ($lp size)
	                      + (4 * n)  (callee-saved registers)
	     Note: Since we use 'push25 Re,0',
	           the stack pointer is just at the position
	           after push instruction.
	           No need to take sp_adjust into consideration.  */
	  fp_adjust = cfun->machine->fp_size
		      + cfun->machine->gp_size
		      + cfun->machine->lp_size
		      + cfun->machine->callee_saved_regs_size;
	  fp_adjust_insn = gen_addsi3 (hard_frame_pointer_rtx,
				       stack_pointer_rtx,
				       GEN_INT (fp_adjust));
	  /* Emit rtx into instructions list and receive INSN rtx form.  */
	  fp_adjust_insn = emit_insn (fp_adjust_insn);
	}

      /* Because we use 'push25 Re,0',
         we need to expand one more instruction to adjust $sp.
         However, sp_adjust value may be out of range of the addi instruction,
         create alternative add behavior with TA_REGNUM if necessary,
         using NEGATIVE value to tell that we are decreasing address.  */
      sp_adjust = nds32_force_addi_stack_int ( (-1) * sp_adjust);
      if (sp_adjust)
	{
	  /* Generate sp adjustment instruction
	     if and only if sp_adjust != 0.  */
	  sp_adjust_insn = gen_addsi3 (stack_pointer_rtx,
				       stack_pointer_rtx,
				       GEN_INT (-1 * sp_adjust));
	  /* Emit rtx into instructions list and receive INSN rtx form.  */
	  sp_adjust_insn = emit_insn (sp_adjust_insn);

	  /* The insn rtx 'sp_adjust_insn' will change frame layout.
	     We need to use RTX_FRAME_RELATED_P so that GCC is able to
	     generate CFI (Call Frame Information) stuff.  */
	  RTX_FRAME_RELATED_P (sp_adjust_insn) = 1;
	}
    }

  /* Prevent the instruction scheduler from
     moving instructions across the boundary.  */
  emit_insn (gen_blockage ());
}

/* Function for v3pop epilogue.  */
void
nds32_expand_epilogue_v3pop (void)
{
  int sp_adjust;

  rtx Rb, Re;
  rtx pop_insn;
  rtx sp_adjust_insn;

  /* Compute and setup stack frame size.
     The result will be in cfun->machine.  */
  nds32_compute_stack_frame ();

  /* Prevent the instruction scheduler from
     moving instructions across the boundary.  */
  emit_insn (gen_blockage ());

  /* If the function is 'naked', we do not have to generate
     epilogue code fragment BUT 'ret' instruction.  */
  if (cfun->machine->naked_p)
    {
      /* Generate return instruction by using
         unspec_volatile_func_return pattern.
         Make sure this instruction is after gen_blockage().
         NOTE that $lp will become 'live'
         after this instruction has been emitted.  */
      emit_insn (gen_unspec_volatile_func_return ());
      return;
    }

  /* Get callee_first_regno and callee_last_regno.  */
  Rb = gen_rtx_REG (SImode, cfun->machine->callee_saved_regs_first_regno);
  Re = gen_rtx_REG (SImode, cfun->machine->callee_saved_regs_last_regno);

  /* Calculate sp_adjust first to test if 'pop25 Re,imm8u' is available,
     where imm8u has to be 8-byte alignment.  */
  sp_adjust = cfun->machine->local_size
	      + cfun->machine->out_args_size
	      + cfun->machine->callee_saved_area_padding_bytes;

  /* We have to consider alloca issue as well.
     If the function does call alloca(), the stack pointer is not fixed.
     In that case, we cannot use 'pop25 Re,imm8u' directly.
     We have to caculate stack pointer from frame pointer
     and then use 'pop25 Re,0'.
     Of course, the frame_pointer_needed should be nonzero
     if the function calls alloca().  */
  if (satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
      && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust)
      && !cfun->calls_alloca)
    {
      /* We can use 'pop25 Re,imm8u'.  */

      /* pop_insn = gen_stack_v3pop(last_regno, sp_adjust),
         the pattern 'stack_v3pop' is implementad in nds32.md.
         The (const_int 14) means v3pop always pop { $fp $gp $lp }.  */
      pop_insn = nds32_gen_stack_v3pop (Rb, Re,
					GEN_INT (14), GEN_INT (sp_adjust));

      /* Emit pop instruction.  */
      emit_insn (pop_insn);
    }
  else
    {
      /* We have to use 'pop25 Re,0', and prior to it,
         we must expand one more instruction to adjust $sp.  */

      if (frame_pointer_needed)
	{
	  /* adjust $sp = $fp - 4        ($fp size)
	                      - 4        ($gp size)
	                      - 4        ($lp size)
	                      - (4 * n)  (callee-saved registers)
	     Note: No need to adjust
	           cfun->machine->callee_saved_area_padding_bytes,
	           because we want to adjust stack pointer
	           to the position for pop instruction.  */
	  sp_adjust = cfun->machine->fp_size
		      + cfun->machine->gp_size
		      + cfun->machine->lp_size
		      + cfun->machine->callee_saved_regs_size;
	  sp_adjust_insn = gen_addsi3 (stack_pointer_rtx,
				       hard_frame_pointer_rtx,
				       GEN_INT (-1 * sp_adjust));
	  /* Emit rtx into instructions list and receive INSN rtx form.  */
	  sp_adjust_insn = emit_insn (sp_adjust_insn);
	}
      else
	{
	  /* If frame pointer is NOT needed,
	     we cannot calculate the sp adjustment from frame pointer.
	     Instead, we calculate the adjustment by local_size,
	     out_args_size, and callee_saved_area_padding_bytes.
	     Notice that such sp adjustment value may be out of range,
	     so we have to deal with it as well.  */

	  /* Adjust $sp = $sp + local_size + out_args_size
			      + callee_saved_area_padding_bytes.  */
	  sp_adjust = cfun->machine->local_size
		      + cfun->machine->out_args_size
		      + cfun->machine->callee_saved_area_padding_bytes;
	  /* sp_adjust value may be out of range of the addi instruction,
	     create alternative add behavior with TA_REGNUM if necessary,
	     using POSITIVE value to tell that we are increasing address.  */
	  sp_adjust = nds32_force_addi_stack_int (sp_adjust);
	  if (sp_adjust)
	    {
	      /* Generate sp adjustment instruction
	         if and only if sp_adjust != 0.  */
	      sp_adjust_insn = gen_addsi3 (stack_pointer_rtx,
					   stack_pointer_rtx,
					   GEN_INT (sp_adjust));
	      /* Emit rtx into instructions list and receive INSN rtx form.  */
	      sp_adjust_insn = emit_insn (sp_adjust_insn);
	    }
	}

      /* pop_insn = gen_stack_v3pop(last_regno, sp_adjust),
         the pattern 'stack_v3pop' is implementad in nds32.md.  */
      /* The (const_int 14) means v3pop always pop { $fp $gp $lp }.  */
      pop_insn = nds32_gen_stack_v3pop (Rb, Re,
					GEN_INT (14), GEN_INT (0));

      /* Emit pop instruction.  */
      emit_insn (pop_insn);
    }
}

/* ------------------------------------------------------------------------ */

/* Function to test 333-form for load/store instructions.
   This is auxiliary extern function for auxiliary macro in nds32.h.
   Because it is a little complicated, we use function instead of macro.  */
bool
nds32_ls_333_p (rtx rt, rtx ra, rtx imm, enum machine_mode mode)
{
  if (REGNO_REG_CLASS (REGNO (rt)) == LOW_REGS
      && REGNO_REG_CLASS (REGNO (ra)) == LOW_REGS)
    {
      if (GET_MODE_SIZE (mode) == 4)
	return satisfies_constraint_Iu05 (imm);

      if (GET_MODE_SIZE (mode) == 2)
	return satisfies_constraint_Iu04 (imm);

      if (GET_MODE_SIZE (mode) == 1)
	return satisfies_constraint_Iu03 (imm);
    }

  return false;
}


/* Functions to expand load_multiple and store_multiple.
   They are auxiliary extern functions to help create rtx template.
   Check nds32-multiple.md file for the patterns.  */
rtx
nds32_expand_load_multiple (int base_regno, int count,
			    rtx base_addr, rtx basemem)
{
  int par_index;
  int offset;
  rtx result;
  rtx new_addr, mem, reg;

  /* Create the pattern that is presented in nds32-multiple.md.  */

  result = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  for (par_index = 0; par_index < count; par_index++)
    {
      offset   = par_index * 4;
      /* 4-byte for loading data to each register.  */
      new_addr = plus_constant (Pmode, base_addr, offset);
      mem      = adjust_automodify_address_nv (basemem, SImode,
					       new_addr, offset);
      reg      = gen_rtx_REG (SImode, base_regno + par_index);

      XVECEXP (result, 0, par_index) = gen_rtx_SET (VOIDmode, reg, mem);
    }

  return result;
}

rtx
nds32_expand_store_multiple (int base_regno, int count,
			     rtx base_addr, rtx basemem)
{
  int par_index;
  int offset;
  rtx result;
  rtx new_addr, mem, reg;

  /* Create the pattern that is presented in nds32-multiple.md.  */

  result = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  for (par_index = 0; par_index < count; par_index++)
    {
      offset   = par_index * 4;
      /* 4-byte for storing data to memory.  */
      new_addr = plus_constant (Pmode, base_addr, offset);
      mem      = adjust_automodify_address_nv (basemem, SImode,
					       new_addr, offset);
      reg      = gen_rtx_REG (SImode, base_regno + par_index);

      XVECEXP (result, 0, par_index) = gen_rtx_SET (VOIDmode, mem, reg);
    }

  return result;
}

/* Function to move block memory content by
   using load_multiple and store_multiple.
   This is auxiliary extern function to help create rtx template.
   Check nds32-multiple.md file for the patterns.  */
int
nds32_expand_movmemqi (rtx dstmem, rtx srcmem, rtx total_bytes, rtx alignment)
{
  HOST_WIDE_INT in_words, out_words;
  rtx dst_base_reg, src_base_reg;
  int maximum_bytes;

  /* Because reduced-set regsiters has few registers
     (r0~r5, r6~10, r15, r28~r31, where 'r15' and 'r28~r31'
      cannot be used for register allocation),
     using 8 registers (32 bytes) for moving memory block
     may easily consume all of them.
     It makes register allocation/spilling hard to work.
     So we only allow maximum=4 registers (16 bytes) for
     moving memory block under reduced-set registers.  */
  if (TARGET_REDUCED_REGS)
    maximum_bytes = 16;
  else
    maximum_bytes = 32;

  /* 1. Total_bytes is integer for sure.
     2. Alignment is integer for sure.
     3. Maximum 4 or 8 registers, 4 * 4 = 16 bytes, 8 * 4 = 32 bytes.
     4. Requires (n * 4) block size.
     5. Requires 4-byte alignment.  */
  if (GET_CODE (total_bytes) != CONST_INT
      || GET_CODE (alignment) != CONST_INT
      || INTVAL (total_bytes) > maximum_bytes
      || INTVAL (total_bytes) & 3
      || INTVAL (alignment) & 3)
    return 0;

  dst_base_reg = copy_to_mode_reg (SImode, XEXP (dstmem, 0));
  src_base_reg = copy_to_mode_reg (SImode, XEXP (srcmem, 0));

  out_words = in_words = INTVAL (total_bytes) / UNITS_PER_WORD;

  emit_insn (nds32_expand_load_multiple (0, in_words, src_base_reg, srcmem));
  emit_insn (nds32_expand_store_multiple (0, out_words, dst_base_reg, dstmem));

  /* Successfully create patterns, return 1.  */
  return 1;
}

/* Function to check whether the OP is a valid load/store operation.
   This is a helper function for the predicates:
   'nds32_load_multiple_operation' and 'nds32_store_multiple_operation'
   in predicates.md file.

   The OP is supposed to be a parallel rtx.
   For each element within this parallel rtx:
     (set (reg) (mem addr)) is the form for load operation.
     (set (mem addr) (reg)) is the form for store operation.
   We have to extract reg and mem of every element and
   check if the information is valid for multiple load/store operation.  */
bool
nds32_valid_multiple_load_store (rtx op, bool load_p)
{
  int count;
  int first_elt_regno;
  rtx elt;

  /* Get the counts of elements in the parallel rtx.  */
  count = XVECLEN (op, 0);
  /* Pick up the first element.  */
  elt = XVECEXP (op, 0, 0);

  /* Perform some quick check for the first element in the parallel rtx.  */
  if (GET_CODE (elt) != SET
      || count <= 1
      || count > 8)
    return false;

  /* Pick up regno of first element for further detail checking.
     Note that the form is different between load and store operation.  */
  if (load_p)
    {
      if (GET_CODE (SET_DEST (elt)) != REG
	  || GET_CODE (SET_SRC (elt)) != MEM)
	return false;

      first_elt_regno = REGNO (SET_DEST (elt));
    }
  else
    {
      if (GET_CODE (SET_SRC (elt)) != REG
	  || GET_CODE (SET_DEST (elt)) != MEM)
	return false;

      first_elt_regno = REGNO (SET_SRC (elt));
    }

  /* Perform detail check for each element.
     Refer to nds32-multiple.md for more information
     about following checking.
     The starting element of parallel rtx is index 0.  */
  if (!nds32_consecutive_registers_load_store_p (op, load_p, 0,
						 first_elt_regno,
						 count))
    return false;

  /* Pass all test, this is a valid rtx.  */
  return true;
}

/* Function to check whether the OP is a valid stack push/pop operation.
   For a valid stack operation, it must satisfy following conditions:
     1. Consecutive registers push/pop operations.
     2. Valid $fp/$gp/$lp push/pop operations.
     3. The last element must be stack adjustment rtx.
   See the prologue/epilogue implementation for details.  */
bool
nds32_valid_stack_push_pop (rtx op, bool push_p)
{
  int index;
  int total_count;
  int rest_count;
  int first_regno;
  rtx elt;
  rtx elt_reg;
  rtx elt_mem;
  rtx elt_plus;

  /* Get the counts of elements in the parallel rtx.  */
  total_count = XVECLEN (op, 0);

  /* Perform some quick check for that every element should be 'set'.  */
  for (index = 0; index < total_count; index++)
    {
      elt = XVECEXP (op, 0, index);
      if (GET_CODE (elt) != SET)
        return false;
    }

  /* For push operation, the parallel rtx looks like:
     (parallel [(set (mem (plus (reg:SI SP_REGNUM) (const_int -32)))
                     (reg:SI Rb))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -28)))
                     (reg:SI Rb+1))
                ...
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -16)))
                     (reg:SI Re))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -12)))
                     (reg:SI FP_REGNUM))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -8)))
                     (reg:SI GP_REGNUM))
                (set (mem (plus (reg:SI SP_REGNUM) (const_int -4)))
                     (reg:SI LP_REGNUM))
                (set (reg:SI SP_REGNUM)
                     (plus (reg:SI SP_REGNUM) (const_int -32)))])

     For pop operation, the parallel rtx looks like:
     (parallel [(set (reg:SI Rb)
                     (mem (reg:SI SP_REGNUM)))
                (set (reg:SI Rb+1)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 4))))
                ...
                (set (reg:SI Re)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 16))))
                (set (reg:SI FP_REGNUM)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 20))))
                (set (reg:SI GP_REGNUM)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 24))))
                (set (reg:SI LP_REGNUM)
                     (mem (plus (reg:SI SP_REGNUM) (const_int 28))))
                (set (reg:SI SP_REGNUM)
                     (plus (reg:SI SP_REGNUM) (const_int 32)))]) */

  /* 1. Consecutive registers push/pop operations.
        We need to calculate how many registers should be consecutive.
        The $sp adjustment rtx, $fp push rtx, $gp push rtx,
        and $lp push rtx are excluded.  */

  /* Exclude last $sp adjustment rtx.  */
  rest_count = total_count - 1;
  /* Exclude $fp, $gp, and $lp if they are in the parallel rtx.  */
  if (cfun->machine->fp_size)
    rest_count--;
  if (cfun->machine->gp_size)
    rest_count--;
  if (cfun->machine->lp_size)
    rest_count--;

  if (rest_count > 0)
    {
      elt = XVECEXP (op, 0, 0);
      /* Pick up register element.  */
      elt_reg = push_p ? SET_SRC (elt) : SET_DEST (elt);
      first_regno = REGNO (elt_reg);

      /* The 'push' operation is a kind of store operation.
         The 'pop' operation is a kind of load operation.
         Pass corresponding false/true as second argument (bool load_p).
         The par_index is supposed to start with index 0.  */
      if (!nds32_consecutive_registers_load_store_p (op,
						     !push_p ? true : false,
						     0,
						     first_regno,
						     rest_count))
        return false;
    }

  /* 2. Valid $fp/$gp/$lp push/pop operations.
        Remember to set start index for checking them.  */

  /* The rest_count is the start index for checking $fp/$gp/$lp.  */
  index = rest_count;
  /* If index < 0, this parallel rtx is definitely
     not a valid stack push/pop operation.  */
  if (index < 0)
    return false;

  /* Check $fp/$gp/$lp one by one.
     We use 'push_p' to pick up reg rtx and mem rtx.  */
  if (cfun->machine->fp_size)
    {
      elt = XVECEXP (op, 0, index);
      elt_mem = push_p ? SET_DEST (elt) : SET_SRC (elt);
      elt_reg = push_p ? SET_SRC (elt) : SET_DEST (elt);
      index++;

      if (GET_CODE (elt_mem) != MEM
          || GET_CODE (elt_reg) != REG
          || REGNO (elt_reg) != FP_REGNUM)
        return false;
    }
  if (cfun->machine->gp_size)
    {
      elt = XVECEXP (op, 0, index);
      elt_mem = push_p ? SET_DEST (elt) : SET_SRC (elt);
      elt_reg = push_p ? SET_SRC (elt) : SET_DEST (elt);
      index++;

      if (GET_CODE (elt_mem) != MEM
          || GET_CODE (elt_reg) != REG
          || REGNO (elt_reg) != GP_REGNUM)
        return false;
    }
  if (cfun->machine->lp_size)
    {
      elt = XVECEXP (op, 0, index);
      elt_mem = push_p ? SET_DEST (elt) : SET_SRC (elt);
      elt_reg = push_p ? SET_SRC (elt) : SET_DEST (elt);
      index++;

      if (GET_CODE (elt_mem) != MEM
          || GET_CODE (elt_reg) != REG
          || REGNO (elt_reg) != LP_REGNUM)
        return false;
    }

  /* 3. The last element must be stack adjustment rtx.
        Its form of rtx should be:
          (set (reg:SI SP_REGNUM)
               (plus (reg:SI SP_REGNUM) (const_int X)))
        The X could be positive or negative value.  */

  /* Pick up the last element.  */
  elt = XVECEXP (op, 0, total_count - 1);

  /* Extract its destination and source rtx.  */
  elt_reg  = SET_DEST (elt);
  elt_plus = SET_SRC (elt);

  /* Check this is (set (stack_reg) (plus stack_reg const)) pattern.  */
  if (GET_CODE (elt_reg) != REG
      || GET_CODE (elt_plus) != PLUS
      || REGNO (elt_reg) != SP_REGNUM)
    return false;

  /* Pass all test, this is a valid rtx.  */
  return true;
}

/* Computing the Length of an Insn.
   Modifies the length assigned to instruction INSN.
   LEN is the initially computed length of the insn.  */
int
nds32_adjust_insn_length (rtx insn, int length)
{
  rtx src, dst;

  switch (recog_memoized (insn))
    {
    case CODE_FOR_move_df:
    case CODE_FOR_move_di:
      /* Adjust length of movd44 to 2.  */
      src = XEXP (PATTERN (insn), 1);
      dst = XEXP (PATTERN (insn), 0);

      if (REG_P (src)
	  && REG_P (dst)
	  && (REGNO (src) % 2) == 0
	  && (REGNO (dst) % 2) == 0)
	length = 2;
      break;

    default:
      break;
    }

  return length;
}


/* Function to check if 'bclr' instruction can be used with IVAL.  */
int
nds32_can_use_bclr_p (int ival)
{
  int one_bit_count;

  /* Calculate the number of 1-bit of (~ival), if there is only one 1-bit,
     it means the original ival has only one 0-bit,
     So it is ok to perform 'bclr' operation.  */

  one_bit_count = popcount_hwi ((unsigned HOST_WIDE_INT) (~ival));

  /* 'bclr' is a performance extension instruction.  */
  return (TARGET_PERF_EXT && (one_bit_count == 1));
}

/* Function to check if 'bset' instruction can be used with IVAL.  */
int
nds32_can_use_bset_p (int ival)
{
  int one_bit_count;

  /* Caculate the number of 1-bit of ival, if there is only one 1-bit,
     it is ok to perform 'bset' operation.  */

  one_bit_count = popcount_hwi ((unsigned HOST_WIDE_INT) (ival));

  /* 'bset' is a performance extension instruction.  */
  return (TARGET_PERF_EXT && (one_bit_count == 1));
}

/* Function to check if 'btgl' instruction can be used with IVAL.  */
int
nds32_can_use_btgl_p (int ival)
{
  int one_bit_count;

  /* Caculate the number of 1-bit of ival, if there is only one 1-bit,
     it is ok to perform 'btgl' operation.  */

  one_bit_count = popcount_hwi ((unsigned HOST_WIDE_INT) (ival));

  /* 'btgl' is a performance extension instruction.  */
  return (TARGET_PERF_EXT && (one_bit_count == 1));
}

/* Function to check if 'bitci' instruction can be used with IVAL.  */
int
nds32_can_use_bitci_p (int ival)
{
  /* If we are using V3 ISA, we have 'bitci' instruction.
     Try to see if we can present 'andi' semantic with
     such 'bit-clear-immediate' operation.
     For example, 'andi $r0,$r0,0xfffffffc' can be
     presented with 'bitci $r0,$r0,3'.  */
  return (TARGET_ISA_V3
	  && (ival < 0)
	  && satisfies_constraint_Iu15 (gen_int_mode (~ival, SImode)));
}


/* Return true if is load/store with SYMBOL_REF addressing mode
   and memory mode is SImode.  */
bool
nds32_symbol_load_store_p (rtx insn)
{
  rtx mem_src = NULL_RTX;

  switch (get_attr_type (insn))
    {
    case TYPE_LOAD:
      mem_src = SET_SRC (PATTERN (insn));
      break;
    case TYPE_STORE:
      mem_src = SET_DEST (PATTERN (insn));
      break;
    default:
      break;
    }

  /* Find load/store insn with addressing mode is SYMBOL_REF.  */
  if (mem_src != NULL_RTX)
    {
      if ((GET_CODE (mem_src) == ZERO_EXTEND)
	  || (GET_CODE (mem_src) == SIGN_EXTEND))
	mem_src = XEXP (mem_src, 0);

      if ((GET_CODE (XEXP (mem_src, 0)) == SYMBOL_REF)
	   || (GET_CODE (XEXP (mem_src, 0)) == LO_SUM))
	return true;
    }

  return false;
}

/* Function to determine whether it is worth to do fp_as_gp optimization.
   Return 0: It is NOT worth to do fp_as_gp optimization.
   Return 1: It is APPROXIMATELY worth to do fp_as_gp optimization.
   Note that if it is worth to do fp_as_gp optimization,
   we MUST set FP_REGNUM ever live in this function.  */
int
nds32_fp_as_gp_check_available (void)
{
  /* If there exists ANY of following conditions,
     we DO NOT perform fp_as_gp optimization:
       1. TARGET_FORBID_FP_AS_GP is set
          regardless of the TARGET_FORCE_FP_AS_GP.
       2. User explicitly uses 'naked' attribute.
       3. Not optimize for size.
       4. Need frame pointer.
       5. If $fp is already required to be saved,
          it means $fp is already choosen by register allocator.
          Thus we better not to use it for fp_as_gp optimization.
       6. This function is a vararg function.
          DO NOT apply fp_as_gp optimization on this function
          because it may change and break stack frame.
       7. The epilogue is empty.
          This happens when the function uses exit()
          or its attribute is no_return.
          In that case, compiler will not expand epilogue
          so that we have no chance to output .omit_fp_end directive.  */
  if (TARGET_FORBID_FP_AS_GP
      || lookup_attribute ("naked", DECL_ATTRIBUTES (current_function_decl))
      || !optimize_size
      || frame_pointer_needed
      || NDS32_REQUIRED_CALLEE_SAVED_P (FP_REGNUM)
      || (cfun->stdarg == 1)
      || (find_fallthru_edge (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds) == NULL))
    return 0;

  /* Now we can check the possibility of using fp_as_gp optimization.  */
  if (TARGET_FORCE_FP_AS_GP)
    {
      /* User explicitly issues -mforce-fp-as-gp option.  */
      df_set_regs_ever_live (FP_REGNUM, 1);
      return 1;
    }
  else
    {
      /* In the following we are going to evaluate whether
         it is worth to do fp_as_gp optimization.  */
      int good_gain     = 0;
      int symbol_count  = 0;

      int threshold;
      rtx insn;

      /* We check if there already requires prologue.
         Note that $gp will be saved in prologue for PIC code generation.
         After that, we can set threshold by the existence of prologue.
         Each fp-implied instruction will gain 2-byte code size
         from gp-aware instruction, so we have following heuristics.  */
      if (flag_pic
	  || nds32_have_prologue_p ())
	{
	  /* Have-prologue:
	       Compiler already intends to generate prologue content,
	       so the fp_as_gp optimization will only insert
	       'la $fp,_FP_BASE_' instruction, which will be
	       converted into 4-byte instruction at link time.
	       The threshold is "3" symbol accesses, 2 + 2 + 2 > 4.  */
	  threshold = 3;
	}
      else
	{
	  /* None-prologue:
	       Compiler originally does not generate prologue content,
	       so the fp_as_gp optimization will NOT ONLY insert
	       'la $fp,_FP_BASE' instruction, but also causes
	       push/pop instructions.
	       If we are using v3push (push25/pop25),
	       the threshold is "5" symbol accesses, 5*2 > 4 + 2 + 2;
	       If we are using normal push (smw/lmw),
	       the threshold is "5+2" symbol accesses 7*2 > 4 + 4 + 4.  */
	  threshold = 5 + (TARGET_V3PUSH ? 0 : 2);
	}

      /* We would like to traverse every instruction in this function.
         So we need to have push_topmost_sequence()/pop_topmost_sequence()
         surrounding our for-loop evaluation.  */
      push_topmost_sequence ();
      /* Counting the insn number which the addressing mode is symbol.  */
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	{
	  if (single_set (insn) && nds32_symbol_load_store_p (insn))
	    symbol_count++;

	  if (symbol_count == threshold)
	    {
	      good_gain = 1;
	      break;
	    }
	}
      pop_topmost_sequence ();

      /* Enable fp_as_gp optimization when potential gain is good enough.  */
      if (good_gain)
	{
	  df_set_regs_ever_live (FP_REGNUM, 1);
	  return 1;
	}
    }

  /* By default we return 0.  */
  return 0;
}


/* Function to generate PC relative jump table.
   Refer to nds32.md for more details.

   The following is the sample for the case that diff value
   can be presented in '.short' size.

     addi    $r1, $r1, -(case_lower_bound)
     slti    $ta, $r1, (case_number)
     beqz    $ta, .L_skip_label

     la      $ta, .L35             ! get jump table address
     lh      $r1, [$ta + $r1 << 1] ! load symbol diff from jump table entry
     addi    $ta, $r1, $ta
     jr5     $ta

     ! jump table entry
   L35:
     .short  .L25-.L35
     .short  .L26-.L35
     .short  .L27-.L35
     .short  .L28-.L35
     .short  .L29-.L35
     .short  .L30-.L35
     .short  .L31-.L35
     .short  .L32-.L35
     .short  .L33-.L35
     .short  .L34-.L35 */
const char *
nds32_output_casesi_pc_relative (rtx *operands)
{
  enum machine_mode mode;
  rtx diff_vec;

  diff_vec = PATTERN (NEXT_INSN (operands[1]));

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  /* Step C: "t <-- operands[1]".  */
  output_asm_insn ("la\t$ta, %l1", operands);

  /* Get the mode of each element in the difference vector.  */
  mode = GET_MODE (diff_vec);

  /* Step D: "z <-- (mem (plus (operands[0] << m) t))",
     where m is 0, 1, or 2 to load address-diff value from table.  */
  switch (mode)
    {
    case QImode:
      output_asm_insn ("lb\t%2, [$ta + %0 << 0]", operands);
      break;
    case HImode:
      output_asm_insn ("lh\t%2, [$ta + %0 << 1]", operands);
      break;
    case SImode:
      output_asm_insn ("lw\t%2, [$ta + %0 << 2]", operands);
      break;
    default:
      gcc_unreachable ();
    }

  /* Step E: "t <-- z + t".
     Add table label_ref with address-diff value to
     obtain target case address.  */
  output_asm_insn ("add\t$ta, %2, $ta", operands);

  /* Step F: jump to target with register t.  */
  if (TARGET_16_BIT)
    return "jr5\t$ta";
  else
    return "jr\t$ta";
}

/* Function to generate normal jump table.  */
const char *
nds32_output_casesi (rtx *operands)
{
  /* Step C: "t <-- operands[1]".  */
  output_asm_insn ("la\t$ta, %l1", operands);

  /* Step D: "z <-- (mem (plus (operands[0] << 2) t))".  */
  output_asm_insn ("lw\t%2, [$ta + %0 << 2]", operands);

  /* No need to perform Step E, which is only used for
     pc relative jump table.  */

  /* Step F: jump to target with register z.  */
  if (TARGET_16_BIT)
    return "jr5\t%2";
  else
    return "jr\t%2";
}


/* Function to return memory format.  */
enum nds32_16bit_address_type
nds32_mem_format (rtx op)
{
  enum machine_mode mode_test;
  int val;
  int regno;

  if (!TARGET_16_BIT)
    return ADDRESS_NOT_16BIT_FORMAT;

  mode_test = GET_MODE (op);

  op = XEXP (op, 0);

  /* 45 format.  */
  if (GET_CODE (op) == REG && (mode_test == SImode))
    return ADDRESS_REG;

  /* 333 format for QI/HImode.  */
  if (GET_CODE (op) == REG && (REGNO (op) < R8_REGNUM))
    return ADDRESS_LO_REG_IMM3U;

  /* post_inc 333 format.  */
  if ((GET_CODE (op) == POST_INC) && (mode_test == SImode))
    {
      regno = REGNO(XEXP (op, 0));

      if (regno < 8)
	return ADDRESS_POST_INC_LO_REG_IMM3U;
    }

  /* post_inc 333 format.  */
  if ((GET_CODE (op) == POST_MODIFY)
      && (mode_test == SImode)
      && (REG_P (XEXP (XEXP (op, 1), 0)))
      && (CONST_INT_P (XEXP (XEXP (op, 1), 1))))
    {
      regno = REGNO (XEXP (XEXP (op, 1), 0));
      val = INTVAL (XEXP (XEXP (op, 1), 1));
      if (regno < 8 && val < 32)
	return ADDRESS_POST_INC_LO_REG_IMM3U;
    }

  if ((GET_CODE (op) == PLUS)
      && (GET_CODE (XEXP (op, 0)) == REG)
      && (GET_CODE (XEXP (op, 1)) == CONST_INT))
    {
      val = INTVAL (XEXP (op, 1));

      regno = REGNO(XEXP (op, 0));

      if (regno > 7
	  && regno != SP_REGNUM
	  && regno != FP_REGNUM)
	return ADDRESS_NOT_16BIT_FORMAT;

      switch (mode_test)
	{
	case QImode:
	  /* 333 format.  */
	  if (val >= 0 && val < 8 && regno < 8)
	    return ADDRESS_LO_REG_IMM3U;
	  break;

	case HImode:
	  /* 333 format.  */
	  if (val >= 0 && val < 16 && (val % 2 == 0) && regno < 8)
	    return ADDRESS_LO_REG_IMM3U;
	  break;

	case SImode:
	case SFmode:
	case DFmode:
	  /* fp imply 37 format.  */
	  if ((regno == FP_REGNUM) &&
	      (val >= 0 && val < 512 && (val % 4 == 0)))
	    return ADDRESS_FP_IMM7U;
	  /* sp imply 37 format.  */
	  else if ((regno == SP_REGNUM) &&
		   (val >= 0 && val < 512 && (val % 4 == 0)))
	    return ADDRESS_SP_IMM7U;
	  /* 333 format.  */
	  else if (val >= 0 && val < 32 && (val % 4 == 0) && regno < 8)
	    return ADDRESS_LO_REG_IMM3U;
	  break;

	default:
	  break;
	}
    }

  return ADDRESS_NOT_16BIT_FORMAT;
}

/* Output 16-bit store.  */
const char *
nds32_output_16bit_store (rtx *operands, int byte)
{
  char pattern[100];
  char size;
  rtx code = XEXP (operands[0], 0);

  size = nds32_byte_to_size (byte);

  switch (nds32_mem_format (operands[0]))
    {
    case ADDRESS_REG:
      operands[0] = code;
      output_asm_insn ("swi450\t%1, [%0]", operands);
      break;
    case ADDRESS_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "s%ci333\t%%1, %%0", size);
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_POST_INC_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "s%ci333.bi\t%%1, %%0", size);
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_FP_IMM7U:
      output_asm_insn ("swi37\t%1, %0", operands);
      break;
    case ADDRESS_SP_IMM7U:
      /* Get immediate value and set back to operands[1].  */
      operands[0] = XEXP (code, 1);
      output_asm_insn ("swi37.sp\t%1, [ + (%0)]", operands);
      break;
    default:
      break;
    }

  return "";
}

/* Output 16-bit load.  */
const char *
nds32_output_16bit_load (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code = XEXP (operands[1], 0);

  size = nds32_byte_to_size (byte);

  switch (nds32_mem_format (operands[1]))
    {
    case ADDRESS_REG:
      operands[1] = code;
      output_asm_insn ("lwi450\t%0, [%1]", operands);
      break;
    case ADDRESS_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "l%ci333\t%%0, %%1", size);
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_POST_INC_LO_REG_IMM3U:
      snprintf (pattern, sizeof (pattern), "l%ci333.bi\t%%0, %%1", size);
      output_asm_insn (pattern, operands);
      break;
    case ADDRESS_FP_IMM7U:
      output_asm_insn ("lwi37\t%0, %1", operands);
      break;
    case ADDRESS_SP_IMM7U:
      /* Get immediate value and set back to operands[0].  */
      operands[1] = XEXP (code, 1);
      output_asm_insn ("lwi37.sp\t%0, [ + (%1)]", operands);
      break;
    default:
      break;
    }

  return "";
}

/* Output 32-bit store.  */
const char *
nds32_output_32bit_store (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code = XEXP (operands[0], 0);

  size = nds32_byte_to_size (byte);

  switch (GET_CODE (code))
    {
    case REG:
      /* (mem (reg X))
	 => access location by using register,
	 use "sbi / shi / swi" */
      snprintf (pattern, sizeof (pattern), "s%ci\t%%1, %%0", size);
      break;

    case SYMBOL_REF:
    case CONST:
      /* (mem (symbol_ref X))
	 (mem (const (...)))
	 => access global variables,
	 use "sbi.gp / shi.gp / swi.gp" */
      operands[0] = XEXP (operands[0], 0);
      snprintf (pattern, sizeof (pattern), "s%ci.gp\t%%1, [ + %%0]", size);
      break;

    case POST_INC:
      /* (mem (post_inc reg))
	 => access location by using register which will be post increment,
	 use "sbi.bi / shi.bi / swi.bi" */
      snprintf (pattern, sizeof (pattern),
		"s%ci.bi\t%%1, %%0, %d", size, byte);
      break;

    case POST_DEC:
      /* (mem (post_dec reg))
	 => access location by using register which will be post decrement,
	 use "sbi.bi / shi.bi / swi.bi" */
      snprintf (pattern, sizeof (pattern),
		"s%ci.bi\t%%1, %%0, -%d", size, byte);
      break;

    case POST_MODIFY:
      switch (GET_CODE (XEXP (XEXP (code, 1), 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (post_modify (reg) (plus (reg) (reg))))
	     => access location by using register which will be
	     post modified with reg,
	     use "sb.bi/ sh.bi / sw.bi" */
	  snprintf (pattern, sizeof (pattern), "s%c.bi\t%%1, %%0", size);
	  break;
	case CONST_INT:
	  /* (mem (post_modify (reg) (plus (reg) (const_int))))
	     => access location by using register which will be
	     post modified with const_int,
	     use "sbi.bi/ shi.bi / swi.bi" */
	  snprintf (pattern, sizeof (pattern), "s%ci.bi\t%%1, %%0", size);
	  break;
	default:
	  abort ();
	}
      break;

    case PLUS:
      switch (GET_CODE (XEXP (code, 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (plus reg reg)) or (mem (plus (mult reg const_int) reg))
	     => access location by adding two registers,
	     use "sb / sh / sw" */
	  snprintf (pattern, sizeof (pattern), "s%c\t%%1, %%0", size);
	  break;
	case CONST_INT:
	  /* (mem (plus reg const_int))
	     => access location by adding one register with const_int,
	     use "sbi / shi / swi" */
	  snprintf (pattern, sizeof (pattern), "s%ci\t%%1, %%0", size);
	  break;
	default:
	  abort ();
	}
      break;

    case LO_SUM:
      operands[2] = XEXP (code, 1);
      operands[0] = XEXP (code, 0);
      snprintf (pattern, sizeof (pattern),
		"s%ci\t%%1, [%%0 + lo12(%%2)]", size);
      break;

    default:
      abort ();
    }

  output_asm_insn (pattern, operands);
  return "";
}

/* Output 32-bit load.  */
const char *
nds32_output_32bit_load (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code;

  code = XEXP (operands[1], 0);

  size = nds32_byte_to_size (byte);

  switch (GET_CODE (code))
    {
    case REG:
      /* (mem (reg X))
	 => access location by using register,
	 use "lbi / lhi / lwi" */
      snprintf (pattern, sizeof (pattern), "l%ci\t%%0, %%1", size);
      break;

    case SYMBOL_REF:
    case CONST:
      /* (mem (symbol_ref X))
	 (mem (const (...)))
	 => access global variables,
	 use "lbi.gp / lhi.gp / lwi.gp" */
      operands[1] = XEXP (operands[1], 0);
      snprintf (pattern, sizeof (pattern), "l%ci.gp\t%%0, [ + %%1]", size);
      break;

    case POST_INC:
      /* (mem (post_inc reg))
	 => access location by using register which will be post increment,
	 use "lbi.bi / lhi.bi / lwi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%ci.bi\t%%0, %%1, %d", size, byte);
      break;

    case POST_DEC:
      /* (mem (post_dec reg))
	 => access location by using register which will be post decrement,
	 use "lbi.bi / lhi.bi / lwi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%ci.bi\t%%0, %%1, -%d", size, byte);
      break;

    case POST_MODIFY:
      switch (GET_CODE (XEXP (XEXP (code, 1), 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (post_modify (reg) (plus (reg) (reg))))
	     => access location by using register which will be
	     post modified with reg,
	     use "lb.bi/ lh.bi / lw.bi" */
	  snprintf (pattern, sizeof (pattern), "l%c.bi\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (post_modify (reg) (plus (reg) (const_int))))
	     => access location by using register which will be
	     post modified with const_int,
	     use "lbi.bi/ lhi.bi / lwi.bi" */
	  snprintf (pattern, sizeof (pattern), "l%ci.bi\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case PLUS:
      switch (GET_CODE (XEXP (code, 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (plus reg reg)) or (mem (plus (mult reg const_int) reg))
	     use "lb / lh / lw" */
	  snprintf (pattern, sizeof (pattern), "l%c\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (plus reg const_int))
	     => access location by adding one register with const_int,
	     use "lbi / lhi / lwi" */
	  snprintf (pattern, sizeof (pattern), "l%ci\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case LO_SUM:
      operands[2] = XEXP (code, 1);
      operands[1] = XEXP (code, 0);
      snprintf (pattern, sizeof (pattern),
		"l%ci\t%%0, [%%1 + lo12(%%2)]", size);
      break;

    default:
      abort ();
    }

  output_asm_insn (pattern, operands);
  return "";
}

/* Output 32-bit load with signed extension.  */
const char *
nds32_output_32bit_load_s (rtx *operands, int byte)
{
  char pattern[100];
  unsigned char size;
  rtx code;

  code = XEXP (operands[1], 0);

  size = nds32_byte_to_size (byte);

  switch (GET_CODE (code))
    {
    case REG:
      /* (mem (reg X))
         => access location by using register,
         use "lbsi / lhsi" */
      snprintf (pattern, sizeof (pattern), "l%csi\t%%0, %%1", size);
      break;

    case SYMBOL_REF:
    case CONST:
      /* (mem (symbol_ref X))
         (mem (const (...)))
         => access global variables,
         use "lbsi.gp / lhsi.gp" */
      operands[1] = XEXP (operands[1], 0);
      snprintf (pattern, sizeof (pattern), "l%csi.gp\t%%0, [ + %%1]", size);
      break;

    case POST_INC:
      /* (mem (post_inc reg))
         => access location by using register which will be post increment,
         use "lbsi.bi / lhsi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%csi.bi\t%%0, %%1, %d", size, byte);
      break;

    case POST_DEC:
      /* (mem (post_dec reg))
         => access location by using register which will be post decrement,
         use "lbsi.bi / lhsi.bi" */
      snprintf (pattern, sizeof (pattern),
		"l%csi.bi\t%%0, %%1, -%d", size, byte);
      break;

    case POST_MODIFY:
      switch (GET_CODE (XEXP (XEXP (code, 1), 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (post_modify (reg) (plus (reg) (reg))))
	     => access location by using register which will be
	     post modified with reg,
	     use "lbs.bi/ lhs.bi" */
	  snprintf (pattern, sizeof (pattern), "l%cs.bi\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (post_modify (reg) (plus (reg) (const_int))))
	     => access location by using register which will be
	     post modified with const_int,
	     use "lbsi.bi/ lhsi.bi" */
	  snprintf (pattern, sizeof (pattern), "l%csi.bi\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case PLUS:
      switch (GET_CODE (XEXP (code, 1)))
	{
	case REG:
	case SUBREG:
	  /* (mem (plus reg reg)) or (mem (plus (mult reg const_int) reg))
	     use "lbs / lhs" */
	  snprintf (pattern, sizeof (pattern), "l%cs\t%%0, %%1", size);
	  break;
	case CONST_INT:
	  /* (mem (plus reg const_int))
	     => access location by adding one register with const_int,
	     use "lbsi / lhsi" */
	  snprintf (pattern, sizeof (pattern), "l%csi\t%%0, %%1", size);
	  break;
	default:
	  abort ();
	}
      break;

    case LO_SUM:
      operands[2] = XEXP (code, 1);
      operands[1] = XEXP (code, 0);
      snprintf (pattern, sizeof (pattern),
		"l%csi\t%%0, [%%1 + lo12(%%2)]", size);
      break;

    default:
      abort ();
    }

  output_asm_insn (pattern, operands);
  return "";
}

/* Function to output stack push operation.
   We need to deal with normal stack push multiple or stack v3push.  */
const char *
nds32_output_stack_push (void)
{
  /* A string pattern for output_asm_insn().  */
  char pattern[100];
  /* The operands array which will be used in output_asm_insn().  */
  rtx operands[3];
  /* Pick up callee-saved first regno and last regno for further use.  */
  int rb_regno = cfun->machine->callee_saved_regs_first_regno;
  int re_regno = cfun->machine->callee_saved_regs_last_regno;

  if (TARGET_V3PUSH)
    {
      /* For stack v3push:
           operands[0]: Re
           operands[1]: imm8u */

      /* This variable is to check if 'push25 Re,imm8u' is available.  */
      int sp_adjust;

      /* Set operands[0].  */
      operands[0] = gen_rtx_REG (SImode, re_regno);

      /* Check if we can generate 'push25 Re,imm8u',
         otherwise, generate 'push25 Re,0'.  */
      sp_adjust = cfun->machine->local_size
		  + cfun->machine->out_args_size
		  + cfun->machine->callee_saved_area_padding_bytes;
      if (satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
	  && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust))
	operands[1] = GEN_INT (sp_adjust);
      else
	operands[1] = GEN_INT (0);

      /* Create assembly code pattern.  */
      snprintf (pattern, sizeof (pattern), "push25\t%%0, %%1");
    }
  else
    {
      /* For normal stack push multiple:
         operands[0]: Rb
         operands[1]: Re
         operands[2]: En4 */

      /* This variable is used to check if we only need to generate En4 field.
         As long as Rb==Re=SP_REGNUM, we set this variable to 1.  */
      int push_en4_only_p = 0;

      /* Set operands[0] and operands[1].  */
      operands[0] = gen_rtx_REG (SImode, rb_regno);
      operands[1] = gen_rtx_REG (SImode, re_regno);

      /* 'smw.adm $sp,[$sp],$sp,0' means push nothing.  */
      if (!cfun->machine->fp_size
	  && !cfun->machine->gp_size
	  && !cfun->machine->lp_size
	  && REGNO (operands[0]) == SP_REGNUM
	  && REGNO (operands[1]) == SP_REGNUM)
	{
	  /* No need to generate instruction.  */
	  return "";
	}
      else
	{
	  /* If Rb==Re=SP_REGNUM, we only need to generate En4 field.  */
	  if (REGNO (operands[0]) == SP_REGNUM
	      && REGNO (operands[1]) == SP_REGNUM)
	    push_en4_only_p = 1;

	  /* Create assembly code pattern.
	     We need to handle the form: "Rb, Re, { $fp $gp $lp }".  */
	  snprintf (pattern, sizeof (pattern),
		    "push.s\t%s{%s%s%s }",
		    push_en4_only_p ? "" : "%0, %1, ",
		    cfun->machine->fp_size ? " $fp" : "",
		    cfun->machine->gp_size ? " $gp" : "",
		    cfun->machine->lp_size ? " $lp" : "");
	}
    }

  /* We use output_asm_insn() to output assembly code by ourself.  */
  output_asm_insn (pattern, operands);
  return "";
}

/* Function to output stack pop operation.
   We need to deal with normal stack pop multiple or stack v3pop.  */
const char *
nds32_output_stack_pop (void)
{
  /* A string pattern for output_asm_insn().  */
  char pattern[100];
  /* The operands array which will be used in output_asm_insn().  */
  rtx operands[3];
  /* Pick up callee-saved first regno and last regno for further use.  */
  int rb_regno = cfun->machine->callee_saved_regs_first_regno;
  int re_regno = cfun->machine->callee_saved_regs_last_regno;

  if (TARGET_V3PUSH)
    {
      /* For stack v3pop:
           operands[0]: Re
           operands[1]: imm8u */

      /* This variable is to check if 'pop25 Re,imm8u' is available.  */
      int sp_adjust;

      /* Set operands[0].  */
      operands[0] = gen_rtx_REG (SImode, re_regno);

      /* Check if we can generate 'pop25 Re,imm8u',
         otherwise, generate 'pop25 Re,0'.
         We have to consider alloca issue as well.
         If the function does call alloca(), the stack pointer is not fixed.
         In that case, we cannot use 'pop25 Re,imm8u' directly.
         We have to caculate stack pointer from frame pointer
         and then use 'pop25 Re,0'.  */
      sp_adjust = cfun->machine->local_size
		  + cfun->machine->out_args_size
		  + cfun->machine->callee_saved_area_padding_bytes;
      if (satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
	  && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust)
	  && !cfun->calls_alloca)
	operands[1] = GEN_INT (sp_adjust);
      else
	operands[1] = GEN_INT (0);

      /* Create assembly code pattern.  */
      snprintf (pattern, sizeof (pattern), "pop25\t%%0, %%1");
    }
  else
    {
      /* For normal stack pop multiple:
         operands[0]: Rb
         operands[1]: Re
         operands[2]: En4 */

      /* This variable is used to check if we only need to generate En4 field.
         As long as Rb==Re=SP_REGNUM, we set this variable to 1.  */
      int pop_en4_only_p = 0;

      /* Set operands[0] and operands[1].  */
      operands[0] = gen_rtx_REG (SImode, rb_regno);
      operands[1] = gen_rtx_REG (SImode, re_regno);

      /* 'lmw.bim $sp,[$sp],$sp,0' means pop nothing.  */
      if (!cfun->machine->fp_size
	  && !cfun->machine->gp_size
	  && !cfun->machine->lp_size
	  && REGNO (operands[0]) == SP_REGNUM
	  && REGNO (operands[1]) == SP_REGNUM)
	{
	  /* No need to generate instruction.  */
	  return "";
	}
      else
	{
	  /* If Rb==Re=SP_REGNUM, we only need to generate En4 field.  */
	  if (REGNO (operands[0]) == SP_REGNUM
	      && REGNO (operands[1]) == SP_REGNUM)
	    pop_en4_only_p = 1;

	  /* Create assembly code pattern.
	     We need to handle the form: "Rb, Re, { $fp $gp $lp }".  */
	  snprintf (pattern, sizeof (pattern),
		    "pop.s\t%s{%s%s%s }",
		    pop_en4_only_p ? "" : "%0, %1, ",
		    cfun->machine->fp_size ? " $fp" : "",
		    cfun->machine->gp_size ? " $gp" : "",
		    cfun->machine->lp_size ? " $lp" : "");
	}
    }

  /* We use output_asm_insn() to output assembly code by ourself.  */
  output_asm_insn (pattern, operands);
  return "";
}

/* Return align 2 (log base 2) if the next instruction of LABEL is 4 byte.  */
int
nds32_target_alignment (rtx label)
{
  rtx insn;

  if (optimize_size)
    return 0;

  insn = next_active_insn (label);

  if (insn == 0)
    return 0;
  else if ((get_attr_length (insn) % 4) == 0)
    return 2;
  else
    return 0;
}

/* ------------------------------------------------------------------------ */

/* PART 5: Initialize target hook structure and definitions.  */

/* Controlling the Compilation Driver.  */


/* Run-time Target Specification.  */


/* Defining Data Structures for Per-function Information.  */


/* Storage Layout.  */

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE \
  default_promote_function_mode_always_promote


/* Layout of Source Language Data Types.  */


/* Register Usage.  */

/* -- Basic Characteristics of Registers.  */

/* -- Order of Allocation of Registers.  */

/* -- How Values Fit in Registers.  */

/* -- Handling Leaf Functions.  */

/* -- Registers That Form a Stack.  */


/* Register Classes.  */

#undef TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS nds32_class_max_nregs

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_true

#undef TARGET_REGISTER_PRIORITY
#define TARGET_REGISTER_PRIORITY nds32_register_priority


/* Obsolete Macros for Defining Constraints.  */


/* Stack Layout and Calling Conventions.  */

/* -- Basic Stack Layout.  */

/* -- Exception Handling Support.  */

/* -- Specifying How Stack Checking is Done.  */

/* -- Registers That Address the Stack Frame.  */

/* -- Eliminating Frame Pointer and Arg Pointer.  */

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE nds32_can_eliminate

/* -- Passing Function Arguments on the Stack.  */

/* -- Passing Arguments in Registers.  */

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG nds32_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE nds32_function_arg_advance

#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY nds32_function_arg_boundary

/* -- How Scalar Function Values Are Returned.  */

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE nds32_function_value

#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE nds32_libcall_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P nds32_function_value_regno_p

/* -- How Large Values Are Returned.  */

/* -- Caller-Saves Register Allocation.  */

/* -- Function Entry and Exit.  */

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE nds32_asm_function_prologue

#undef TARGET_ASM_FUNCTION_END_PROLOGUE
#define TARGET_ASM_FUNCTION_END_PROLOGUE nds32_asm_function_end_prologue

#undef  TARGET_ASM_FUNCTION_BEGIN_EPILOGUE
#define TARGET_ASM_FUNCTION_BEGIN_EPILOGUE nds32_asm_function_begin_epilogue

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE nds32_asm_function_epilogue

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK nds32_asm_output_mi_thunk

#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK default_can_output_mi_thunk_no_vcall

/* -- Generating Code for Profiling.  */

/* -- Permitting tail calls.  */

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN nds32_warn_func_return

/* Stack smashing protection.  */


/* Implementing the Varargs Macros.  */

#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING nds32_strict_argument_naming


/* Trampolines for Nested Functions.  */

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE nds32_asm_trampoline_template

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT nds32_trampoline_init


/* Implicit Calls to Library Routines.  */


/* Addressing Modes.  */

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P nds32_legitimate_address_p


/* Anchored Addresses.  */


/* Condition Code Status.  */

/* -- Representation of condition codes using (cc0).  */

/* -- Representation of condition codes using registers.  */

/* -- Macros to control conditional execution.  */


/* Describing Relative Costs of Operations.  */

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST nds32_register_move_cost

#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST nds32_memory_move_cost

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS nds32_rtx_costs

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST nds32_address_cost


/* Adjusting the Instruction Scheduler.  */


/* Dividing the Output into Sections (Texts, Data, . . . ).  */


/* Position Independent Code.  */


/* Defining the Output Assembler Language.  */

/* -- The Overall Framework of an Assembler File.  */

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START nds32_asm_file_start
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END nds32_asm_file_end

/* -- Output of Data.  */

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"

#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

/* -- Output of Uninitialized Variables.  */

/* -- Output and Generation of Labels.  */

#undef TARGET_ASM_GLOBALIZE_LABEL
#define TARGET_ASM_GLOBALIZE_LABEL nds32_asm_globalize_label

/* -- How Initialization Functions Are Handled.  */

/* -- Macros Controlling Initialization Routines.  */

/* -- Output of Assembler Instructions.  */

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND nds32_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS nds32_print_operand_address

/* -- Output of Dispatch Tables.  */

/* -- Assembler Commands for Exception Regions.  */

/* -- Assembler Commands for Alignment.  */


/* Controlling Debugging Information Format.  */

/* -- Macros Affecting All Debugging Formats.  */

/* -- Specific Options for DBX Output.  */

/* -- Open-Ended Hooks for DBX Format.  */

/* -- File Names in DBX Format.  */

/* -- Macros for SDB and DWARF Output.  */

/* -- Macros for VMS Debug Format.  */


/* Cross Compilation and Floating Point.  */


/* Mode Switching Instructions.  */


/* Defining target-specific uses of __attribute__.  */

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE nds32_attribute_table

#undef TARGET_MERGE_DECL_ATTRIBUTES
#define TARGET_MERGE_DECL_ATTRIBUTES nds32_merge_decl_attributes

#undef TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES nds32_insert_attributes

#undef TARGET_OPTION_PRAGMA_PARSE
#define TARGET_OPTION_PRAGMA_PARSE nds32_option_pragma_parse

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE nds32_option_override


/* Emulating TLS.  */


/* Defining coprocessor specifics for MIPS targets.  */


/* Parameters for Precompiled Header Validity Checking.  */


/* C++ ABI parameters.  */


/* Adding support for named address spaces.  */


/* Miscellaneous Parameters.  */

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS nds32_init_builtins

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN nds32_expand_builtin


/* ------------------------------------------------------------------------ */

/* Initialize the GCC target structure.  */

struct gcc_target targetm = TARGET_INITIALIZER;

/* ------------------------------------------------------------------------ */
