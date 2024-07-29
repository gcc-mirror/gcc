/* Subroutines used to generate function prologues and epilogues
   on IBM RS/6000.
   Copyright (C) 1991-2024 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "df.h"
#include "tm_p.h"
#include "ira.h"
#include "print-tree.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "tree-pass.h"
#include "rtx-vector-builder.h"
#include "predict.h"
#include "target.h"
#include "stringpool.h"
#include "attribs.h"
#include "except.h"
#include "langhooks.h"
#include "optabs.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "rs6000-internal.h"

static int rs6000_ra_ever_killed (void);
static void is_altivec_return_reg (rtx, void *);
static bool rs6000_save_toc_in_prologue_p (void);

static rs6000_stack_t stack_info;

/* Set if HARD_FRAM_POINTER_REGNUM is really needed.  */
static bool frame_pointer_needed_indeed = false;

/* Label number of label created for -mrelocatable, to call to so we can
   get the address of the GOT section */
int rs6000_pic_labelno = 0;


#ifndef TARGET_PROFILE_KERNEL
#define TARGET_PROFILE_KERNEL 0
#endif


/* Function to init struct machine_function.
   This will be called, via a pointer variable,
   from push_function_context.  */

struct machine_function *
rs6000_init_machine_status (void)
{
  stack_info.reload_completed = 0;
  return ggc_cleared_alloc<machine_function> ();
}

/* This page contains routines that are used to determine what the
   function prologue and epilogue code will do and write them out.  */

/* Determine whether the REG is really used.  */

bool
save_reg_p (int reg)
{
  if (reg == RS6000_PIC_OFFSET_TABLE_REGNUM && !TARGET_SINGLE_PIC_BASE)
    {
      /* When calling eh_return, we must return true for all the cases
	 where conditional_register_usage marks the PIC offset reg
	 call used or fixed.  */
      if (crtl->calls_eh_return
	  && ((DEFAULT_ABI == ABI_V4 && flag_pic)
	      || (DEFAULT_ABI == ABI_DARWIN && flag_pic)
	      || (TARGET_TOC && TARGET_MINIMAL_TOC)))
	return true;

      /* We need to mark the PIC offset register live for the same
	 conditions as it is set up in rs6000_emit_prologue, or
	 otherwise it won't be saved before we clobber it.  */
      if (TARGET_TOC && TARGET_MINIMAL_TOC
	  && !constant_pool_empty_p ())
	return true;

      if (DEFAULT_ABI == ABI_V4
	  && (flag_pic == 1 || (flag_pic && TARGET_SECURE_PLT))
	  && df_regs_ever_live_p (RS6000_PIC_OFFSET_TABLE_REGNUM))
	return true;

      if (DEFAULT_ABI == ABI_DARWIN
	  && flag_pic && crtl->uses_pic_offset_table)
	return true;
    }

  return !call_used_or_fixed_reg_p (reg) && df_regs_ever_live_p (reg);
}

/* Return the first fixed-point register that is required to be
   saved. 32 if none.  */

int
first_reg_to_save (void)
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 13; first_reg <= 31; first_reg++)
    if (save_reg_p (first_reg))
      break;

  return first_reg;
}

/* Similar, for FP regs.  */

int
first_fp_reg_to_save (void)
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 14 + 32; first_reg <= 63; first_reg++)
    if (save_reg_p (first_reg))
      break;

  return first_reg;
}

/* Similar, for AltiVec regs.  */

static int
first_altivec_reg_to_save (void)
{
  int i;

  /* Stack frame remains as is unless we are in AltiVec ABI.  */
  if (! TARGET_ALTIVEC_ABI)
    return LAST_ALTIVEC_REGNO + 1;

  /* On Darwin, the unwind routines are compiled without
     TARGET_ALTIVEC, and use save_world to save/restore the
     altivec registers when necessary.  */
  if (DEFAULT_ABI == ABI_DARWIN && crtl->calls_eh_return
      && ! TARGET_ALTIVEC)
    return FIRST_ALTIVEC_REGNO + 20;

  /* Find lowest numbered live register.  */
  for (i = FIRST_ALTIVEC_REGNO + 20; i <= LAST_ALTIVEC_REGNO; ++i)
    if (save_reg_p (i))
      break;

  return i;
}

/* Return a 32-bit mask of the AltiVec registers we need to set in
   VRSAVE.  Bit n of the return value is 1 if Vn is live.  The MSB in
   the 32-bit word is 0.  */

static unsigned int
compute_vrsave_mask (void)
{
  unsigned int i, mask = 0;

  /* On Darwin, the unwind routines are compiled without
     TARGET_ALTIVEC, and use save_world to save/restore the
     call-saved altivec registers when necessary.  */
  if (DEFAULT_ABI == ABI_DARWIN && crtl->calls_eh_return
      && ! TARGET_ALTIVEC)
    mask |= 0xFFF;

  /* First, find out if we use _any_ altivec registers.  */
  for (i = FIRST_ALTIVEC_REGNO; i <= LAST_ALTIVEC_REGNO; ++i)
    if (df_regs_ever_live_p (i))
      mask |= ALTIVEC_REG_BIT (i);

  if (mask == 0)
    return mask;

  /* Next, remove the argument registers from the set.  These must
     be in the VRSAVE mask set by the caller, so we don't need to add
     them in again.  More importantly, the mask we compute here is
     used to generate CLOBBERs in the set_vrsave insn, and we do not
     wish the argument registers to die.  */
  for (i = ALTIVEC_ARG_MIN_REG; i < (unsigned) crtl->args.info.vregno; i++)
    mask &= ~ALTIVEC_REG_BIT (i);

  /* Similarly, remove the return value from the set.  */
  {
    bool yes = false;
    diddle_return_value (is_altivec_return_reg, &yes);
    if (yes)
      mask &= ~ALTIVEC_REG_BIT (ALTIVEC_ARG_RETURN);
  }

  return mask;
}

/* For a very restricted set of circumstances, we can cut down the
   size of prologues/epilogues by calling our own save/restore-the-world
   routines.  */

static void
compute_save_world_info (rs6000_stack_t *info)
{
  info->world_save_p = 1;
  info->world_save_p
    = (WORLD_SAVE_P (info)
       && DEFAULT_ABI == ABI_DARWIN
       && !cfun->has_nonlocal_label
       && info->first_fp_reg_save == FIRST_SAVED_FP_REGNO
       && info->first_gp_reg_save == FIRST_SAVED_GP_REGNO
       && info->first_altivec_reg_save == FIRST_SAVED_ALTIVEC_REGNO
       && info->cr_save_p);

  /* This will not work in conjunction with sibcalls.  Make sure there
     are none.  (This check is expensive, but seldom executed.) */
  if (WORLD_SAVE_P (info))
    {
      rtx_insn *insn;
      for (insn = get_last_insn_anywhere (); insn; insn = PREV_INSN (insn))
	if (CALL_P (insn) && SIBLING_CALL_P (insn))
	  {
	    info->world_save_p = 0;
	    break;
	  }
    }

  if (WORLD_SAVE_P (info))
    {
      /* Even if we're not touching VRsave, make sure there's room on the
	 stack for it, if it looks like we're calling SAVE_WORLD, which
	 will attempt to save it. */
      info->vrsave_size  = 4;

      /* If we are going to save the world, we need to save the link register too.  */
      info->lr_save_p = 1;

      /* "Save" the VRsave register too if we're saving the world.  */
      if (info->vrsave_mask == 0)
	info->vrsave_mask = compute_vrsave_mask ();

      /* Because the Darwin register save/restore routines only handle
	 F14 .. F31 and V20 .. V31 as per the ABI, perform a consistency
	 check.  */
      gcc_assert (info->first_fp_reg_save >= FIRST_SAVED_FP_REGNO
		  && (info->first_altivec_reg_save
		      >= FIRST_SAVED_ALTIVEC_REGNO));
    }

  return;
}


static void
is_altivec_return_reg (rtx reg, void *xyes)
{
  bool *yes = (bool *) xyes;
  if (REGNO (reg) == ALTIVEC_ARG_RETURN)
    *yes = true;
}


/* Return whether REG is a global user reg or has been specifed by
   -ffixed-REG.  We should not restore these, and so cannot use
   lmw or out-of-line restore functions if there are any.  We also
   can't save them (well, emit frame notes for them), because frame
   unwinding during exception handling will restore saved registers.  */

static bool
fixed_reg_p (int reg)
{
  /* Ignore fixed_regs[RS6000_PIC_OFFSET_TABLE_REGNUM] when the
     backend sets it, overriding anything the user might have given.  */
  if (reg == RS6000_PIC_OFFSET_TABLE_REGNUM
      && ((DEFAULT_ABI == ABI_V4 && flag_pic)
	  || (DEFAULT_ABI == ABI_DARWIN && flag_pic)
	  || (TARGET_TOC && TARGET_MINIMAL_TOC)))
    return false;

  return fixed_regs[reg];
}

/* Determine the strategy for savings/restoring registers.  */

enum {
  SAVE_MULTIPLE = 0x1,
  SAVE_INLINE_GPRS = 0x2,
  SAVE_INLINE_FPRS = 0x4,
  SAVE_NOINLINE_GPRS_SAVES_LR = 0x8,
  SAVE_NOINLINE_FPRS_SAVES_LR = 0x10,
  SAVE_INLINE_VRS = 0x20,
  REST_MULTIPLE = 0x100,
  REST_INLINE_GPRS = 0x200,
  REST_INLINE_FPRS = 0x400,
  REST_NOINLINE_FPRS_DOESNT_RESTORE_LR = 0x800,
  REST_INLINE_VRS = 0x1000
};

static int
rs6000_savres_strategy (rs6000_stack_t *info,
			bool using_static_chain_p)
{
  int strategy = 0;

  /* Select between in-line and out-of-line save and restore of regs.
     First, all the obvious cases where we don't use out-of-line.  */
  if (crtl->calls_eh_return
      || cfun->machine->ra_need_lr)
    strategy |= (SAVE_INLINE_FPRS | REST_INLINE_FPRS
		 | SAVE_INLINE_GPRS | REST_INLINE_GPRS
		 | SAVE_INLINE_VRS | REST_INLINE_VRS);

  if (info->first_gp_reg_save == 32)
    strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;

  if (info->first_fp_reg_save == 64)
    strategy |= SAVE_INLINE_FPRS | REST_INLINE_FPRS;

  if (info->first_altivec_reg_save == LAST_ALTIVEC_REGNO + 1)
    strategy |= SAVE_INLINE_VRS | REST_INLINE_VRS;

  /* Define cutoff for using out-of-line functions to save registers.  */
  if (DEFAULT_ABI == ABI_V4 || TARGET_ELF)
    {
      if (!optimize_size)
	{
	  strategy |= SAVE_INLINE_FPRS | REST_INLINE_FPRS;
	  strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;
	  strategy |= SAVE_INLINE_VRS | REST_INLINE_VRS;
	}
      else
	{
	  /* Prefer out-of-line restore if it will exit.  */
	  if (info->first_fp_reg_save > 61)
	    strategy |= SAVE_INLINE_FPRS;
	  if (info->first_gp_reg_save > 29)
	    {
	      if (info->first_fp_reg_save == 64)
		strategy |= SAVE_INLINE_GPRS;
	      else
		strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;
	    }
	  if (info->first_altivec_reg_save == LAST_ALTIVEC_REGNO)
	    strategy |= SAVE_INLINE_VRS | REST_INLINE_VRS;
	}
    }
  else if (DEFAULT_ABI == ABI_DARWIN)
    {
      if (info->first_fp_reg_save > 60)
	strategy |= SAVE_INLINE_FPRS | REST_INLINE_FPRS;
      if (info->first_gp_reg_save > 29)
	strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;
      strategy |= SAVE_INLINE_VRS | REST_INLINE_VRS;
    }
  else
    {
      gcc_checking_assert (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2);
      if ((flag_shrink_wrap_separate && optimize_function_for_speed_p (cfun))
	  || info->first_fp_reg_save > 61)
	strategy |= SAVE_INLINE_FPRS | REST_INLINE_FPRS;
      strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;
      strategy |= SAVE_INLINE_VRS | REST_INLINE_VRS;
    }

  /* Don't bother to try to save things out-of-line if r11 is occupied
     by the static chain.  It would require too much fiddling and the
     static chain is rarely used anyway.  FPRs are saved w.r.t the stack
     pointer on Darwin, and AIX uses r1 or r12.  */
  if (using_static_chain_p
      && (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_DARWIN))
    strategy |= ((DEFAULT_ABI == ABI_DARWIN ? 0 : SAVE_INLINE_FPRS)
		 | SAVE_INLINE_GPRS
		 | SAVE_INLINE_VRS);

  /* Don't ever restore fixed regs.  That means we can't use the
     out-of-line register restore functions if a fixed reg is in the
     range of regs restored.   */
  if (!(strategy & REST_INLINE_FPRS))
    for (int i = info->first_fp_reg_save; i < 64; i++)
      if (fixed_regs[i])
	{
	  strategy |= REST_INLINE_FPRS;
	  break;
	}

  /* We can only use the out-of-line routines to restore fprs if we've
     saved all the registers from first_fp_reg_save in the prologue.
     Otherwise, we risk loading garbage.  Of course, if we have saved
     out-of-line then we know we haven't skipped any fprs.  */
  if ((strategy & SAVE_INLINE_FPRS)
      && !(strategy & REST_INLINE_FPRS))
    for (int i = info->first_fp_reg_save; i < 64; i++)
      if (!save_reg_p (i))
	{
	  strategy |= REST_INLINE_FPRS;
	  break;
	}

  /* Similarly, for altivec regs.  */
  if (!(strategy & REST_INLINE_VRS))
    for (int i = info->first_altivec_reg_save; i < LAST_ALTIVEC_REGNO + 1; i++)
      if (fixed_regs[i])
	{
	  strategy |= REST_INLINE_VRS;
	  break;
	}

  if ((strategy & SAVE_INLINE_VRS)
      && !(strategy & REST_INLINE_VRS))
    for (int i = info->first_altivec_reg_save; i < LAST_ALTIVEC_REGNO + 1; i++)
      if (!save_reg_p (i))
	{
	  strategy |= REST_INLINE_VRS;
	  break;
	}

  /* info->lr_save_p isn't yet set if the only reason lr needs to be
     saved is an out-of-line save or restore.  Set up the value for
     the next test (excluding out-of-line gprs).  */
  bool lr_save_p = (info->lr_save_p
		    || !(strategy & SAVE_INLINE_FPRS)
		    || !(strategy & SAVE_INLINE_VRS)
		    || !(strategy & REST_INLINE_FPRS)
		    || !(strategy & REST_INLINE_VRS));

  if (TARGET_MULTIPLE
      && !TARGET_POWERPC64
      && info->first_gp_reg_save < 31
      && !(flag_shrink_wrap
	   && flag_shrink_wrap_separate
	   && optimize_function_for_speed_p (cfun)))
    {
      int count = 0;
      for (int i = info->first_gp_reg_save; i < 32; i++)
	if (save_reg_p (i))
	  count++;

      if (count <= 1)
	/* Don't use store multiple if only one reg needs to be
	   saved.  This can occur for example when the ABI_V4 pic reg
	   (r30) needs to be saved to make calls, but r31 is not
	   used.  */
	strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;
      else
	{
	  /* Prefer store multiple for saves over out-of-line
	     routines, since the store-multiple instruction will
	     always be smaller.  */
	  strategy |= SAVE_INLINE_GPRS | SAVE_MULTIPLE;

	  /* The situation is more complicated with load multiple.
	     We'd prefer to use the out-of-line routines for restores,
	     since the "exit" out-of-line routines can handle the
	     restore of LR and the frame teardown.  However if doesn't
	     make sense to use the out-of-line routine if that is the
	     only reason we'd need to save LR, and we can't use the
	     "exit" out-of-line gpr restore if we have saved some
	     fprs; In those cases it is advantageous to use load
	     multiple when available.  */
	  if (info->first_fp_reg_save != 64 || !lr_save_p)
	    strategy |= REST_INLINE_GPRS | REST_MULTIPLE;
	}
    }

  /* Using the "exit" out-of-line routine does not improve code size
     if using it would require lr to be saved and if only saving one
     or two gprs.  */
  else if (!lr_save_p && info->first_gp_reg_save > 29)
    strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;

  /* Don't ever restore fixed regs.  */
  if ((strategy & (REST_INLINE_GPRS | REST_MULTIPLE)) != REST_INLINE_GPRS)
    for (int i = info->first_gp_reg_save; i < 32; i++)
      if (fixed_reg_p (i))
	{
	  strategy |= REST_INLINE_GPRS;
	  strategy &= ~REST_MULTIPLE;
	  break;
	}

  /* We can only use load multiple or the out-of-line routines to
     restore gprs if we've saved all the registers from
     first_gp_reg_save.  Otherwise, we risk loading garbage.
     Of course, if we have saved out-of-line or used stmw then we know
     we haven't skipped any gprs.  */
  if ((strategy & (SAVE_INLINE_GPRS | SAVE_MULTIPLE)) == SAVE_INLINE_GPRS
      && (strategy & (REST_INLINE_GPRS | REST_MULTIPLE)) != REST_INLINE_GPRS)
    for (int i = info->first_gp_reg_save; i < 32; i++)
      if (!save_reg_p (i))
	{
	  strategy |= REST_INLINE_GPRS;
	  strategy &= ~REST_MULTIPLE;
	  break;
	}

  if (TARGET_ELF && TARGET_64BIT)
    {
      if (!(strategy & SAVE_INLINE_FPRS))
	strategy |= SAVE_NOINLINE_FPRS_SAVES_LR;
      else if (!(strategy & SAVE_INLINE_GPRS)
	       && info->first_fp_reg_save == 64)
	strategy |= SAVE_NOINLINE_GPRS_SAVES_LR;
    }
  else if (TARGET_AIX && !(strategy & REST_INLINE_FPRS))
    strategy |= REST_NOINLINE_FPRS_DOESNT_RESTORE_LR;

  if (TARGET_MACHO && !(strategy & SAVE_INLINE_FPRS))
    strategy |= SAVE_NOINLINE_FPRS_SAVES_LR;

  return strategy;
}

/* Calculate the stack information for the current function.  This is
   complicated by having two separate calling sequences, the AIX calling
   sequence and the V.4 calling sequence.

   AIX (and Darwin/Mac OS X) stack frames look like:
							  32-bit  64-bit
	SP---->	+---------------------------------------+
		| back chain to caller			| 0	  0
		+---------------------------------------+
		| saved CR				| 4       8 (8-11)
		+---------------------------------------+
		| saved LR				| 8       16
		+---------------------------------------+
		| reserved for compilers		| 12      24
		+---------------------------------------+
		| reserved for binders			| 16      32
		+---------------------------------------+
		| saved TOC pointer			| 20      40
		+---------------------------------------+
		| Parameter save area (+padding*) (P)	| 24      48
		+---------------------------------------+
		| Alloca space (A)			| 24+P    etc.
		+---------------------------------------+
		| Local variable space (L)		| 24+P+A
		+---------------------------------------+
		| Float/int conversion temporary (X)	| 24+P+A+L
		+---------------------------------------+
		| Save area for AltiVec registers (W)	| 24+P+A+L+X
		+---------------------------------------+
		| AltiVec alignment padding (Y)		| 24+P+A+L+X+W
		+---------------------------------------+
		| Save area for VRSAVE register (Z)	| 24+P+A+L+X+W+Y
		+---------------------------------------+
		| Save area for GP registers (G)	| 24+P+A+X+L+X+W+Y+Z
		+---------------------------------------+
		| Save area for FP registers (F)	| 24+P+A+X+L+X+W+Y+Z+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|
		+---------------------------------------+

     * If the alloca area is present, the parameter save area is
       padded so that the former starts 16-byte aligned.

   The required alignment for AIX configurations is two words (i.e., 8
   or 16 bytes).

   The ELFv2 ABI is a variant of the AIX ABI.  Stack frames look like:

	SP---->	+---------------------------------------+
		| Back chain to caller			|  0
		+---------------------------------------+
		| Save area for CR			|  8
		+---------------------------------------+
		| Saved LR				|  16
		+---------------------------------------+
		| Saved TOC pointer			|  24
		+---------------------------------------+
		| Parameter save area (+padding*) (P)	|  32
		+---------------------------------------+
		| Alloca space (A)			|  32+P
		+---------------------------------------+
		| Local variable space (L)		|  32+P+A
		+---------------------------------------+
		| Optional ROP hash slot (R)		|  32+P+A+L
		+---------------------------------------+
		| Save area for AltiVec registers (W)	|  32+P+A+L+R
		+---------------------------------------+
		| AltiVec alignment padding (Y)		|  32+P+A+L+R+W
		+---------------------------------------+
		| Save area for GP registers (G)	|  32+P+A+L+R+W+Y
		+---------------------------------------+
		| Save area for FP registers (F)	|  32+P+A+L+R+W+Y+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|  32+P+A+L+R+W+Y+G+F
		+---------------------------------------+

     * If the alloca area is present, the parameter save area is
       padded so that the former starts 16-byte aligned.

   V.4 stack frames look like:

	SP---->	+---------------------------------------+
		| back chain to caller			| 0
		+---------------------------------------+
		| caller's saved LR			| 4
		+---------------------------------------+
		| Parameter save area (+padding*) (P)	| 8
		+---------------------------------------+
		| Alloca space (A)			| 8+P
		+---------------------------------------+
		| Varargs save area (V)			| 8+P+A
		+---------------------------------------+
		| Local variable space (L)		| 8+P+A+V
		+---------------------------------------+
		| Float/int conversion temporary (X)	| 8+P+A+V+L
		+---------------------------------------+
		| Save area for AltiVec registers (W)	| 8+P+A+V+L+X
		+---------------------------------------+
		| AltiVec alignment padding (Y)		| 8+P+A+V+L+X+W
		+---------------------------------------+
		| Save area for VRSAVE register (Z)	| 8+P+A+V+L+X+W+Y
		+---------------------------------------+
		| saved CR (C)				| 8+P+A+V+L+X+W+Y+Z
		+---------------------------------------+
		| Save area for GP registers (G)	| 8+P+A+V+L+X+W+Y+Z+C
		+---------------------------------------+
		| Save area for FP registers (F)	| 8+P+A+V+L+X+W+Y+Z+C+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|
		+---------------------------------------+

     * If the alloca area is present and the required alignment is
       16 bytes, the parameter save area is padded so that the
       alloca area starts 16-byte aligned.

   The required alignment for V.4 is 16 bytes, or 8 bytes if -meabi is
   given.  (But note below and in sysv4.h that we require only 8 and
   may round up the size of our stack frame anyways.  The historical
   reason is early versions of powerpc-linux which didn't properly
   align the stack at program startup.  A happy side-effect is that
   -mno-eabi libraries can be used with -meabi programs.)

   The EABI configuration defaults to the V.4 layout.  However,
   the stack alignment requirements may differ.  If -mno-eabi is not
   given, the required stack alignment is 8 bytes; if -mno-eabi is
   given, the required alignment is 16 bytes.  (But see V.4 comment
   above.)  */

#ifndef ABI_STACK_BOUNDARY
#define ABI_STACK_BOUNDARY STACK_BOUNDARY
#endif

rs6000_stack_t *
rs6000_stack_info (void)
{
  /* We should never be called for thunks, we are not set up for that.  */
  gcc_assert (!cfun->is_thunk);

  rs6000_stack_t *info = &stack_info;
  int reg_size = TARGET_32BIT ? 4 : 8;
  int ehrd_size;
  int ehcr_size;
  int save_align;
  int first_gp;
  HOST_WIDE_INT non_fixed_size;
  bool using_static_chain_p;

  if (reload_completed && info->reload_completed)
    return info;

  memset (info, 0, sizeof (*info));
  info->reload_completed = reload_completed;

  /* Select which calling sequence.  */
  info->abi = DEFAULT_ABI;

  /* Calculate which registers need to be saved & save area size.  */
  info->first_gp_reg_save = first_reg_to_save ();
  /* Assume that we will have to save RS6000_PIC_OFFSET_TABLE_REGNUM,
     even if it currently looks like we won't.  Reload may need it to
     get at a constant; if so, it will have already created a constant
     pool entry for it.  */
  if (((TARGET_TOC && TARGET_MINIMAL_TOC)
       || (flag_pic == 1 && DEFAULT_ABI == ABI_V4)
       || (flag_pic && DEFAULT_ABI == ABI_DARWIN))
      && crtl->uses_const_pool
      && info->first_gp_reg_save > RS6000_PIC_OFFSET_TABLE_REGNUM)
    first_gp = RS6000_PIC_OFFSET_TABLE_REGNUM;
  else
    first_gp = info->first_gp_reg_save;

  info->gp_size = reg_size * (32 - first_gp);

  info->first_fp_reg_save = first_fp_reg_to_save ();
  info->fp_size = 8 * (64 - info->first_fp_reg_save);

  info->first_altivec_reg_save = first_altivec_reg_to_save ();
  info->altivec_size = 16 * (LAST_ALTIVEC_REGNO + 1
				 - info->first_altivec_reg_save);

  /* Does this function call anything (apart from sibling calls)?  */
  info->calls_p = (!crtl->is_leaf || cfun->machine->ra_needs_full_frame);
  info->rop_hash_size = 0;

  /* If we want ROP protection and this function makes a call, indicate
     we need to create a stack slot to save the hashed return address in.  */
  if (rs6000_rop_protect
      && info->calls_p)
    info->rop_hash_size = 8;

  /* Determine if we need to save the condition code registers.  */
  if (save_reg_p (CR2_REGNO)
      || save_reg_p (CR3_REGNO)
      || save_reg_p (CR4_REGNO))
    {
      info->cr_save_p = 1;
      if (DEFAULT_ABI == ABI_V4)
	info->cr_size = reg_size;
    }

  /* If the current function calls __builtin_eh_return, then we need
     to allocate stack space for registers that will hold data for
     the exception handler.  */
  if (crtl->calls_eh_return)
    {
      unsigned int i;
      for (i = 0; EH_RETURN_DATA_REGNO (i) != INVALID_REGNUM; ++i)
	continue;

      ehrd_size = i * UNITS_PER_WORD;
    }
  else
    ehrd_size = 0;

  /* In the ELFv2 ABI, we also need to allocate space for separate
     CR field save areas if the function calls __builtin_eh_return.  */
  if (DEFAULT_ABI == ABI_ELFv2 && crtl->calls_eh_return)
    {
      /* This hard-codes that we have three call-saved CR fields.  */
      ehcr_size = 3 * reg_size;
      /* We do *not* use the regular CR save mechanism.  */
      info->cr_save_p = 0;
    }
  else
    ehcr_size = 0;

  /* Determine various sizes.  */
  info->reg_size     = reg_size;
  info->fixed_size   = RS6000_SAVE_AREA;
  info->vars_size    = RS6000_ALIGN (get_frame_size (), 8);
  if (cfun->calls_alloca)
    info->parm_size  =
      RS6000_ALIGN (crtl->outgoing_args_size + info->fixed_size,
		    STACK_BOUNDARY / BITS_PER_UNIT) - info->fixed_size;
  else
    info->parm_size  = RS6000_ALIGN (crtl->outgoing_args_size,
				     TARGET_ALTIVEC ? 16 : 8);
  if (FRAME_GROWS_DOWNWARD)
    info->vars_size
      += RS6000_ALIGN (info->fixed_size + info->vars_size + info->parm_size,
		       ABI_STACK_BOUNDARY / BITS_PER_UNIT)
	 - (info->fixed_size + info->vars_size + info->parm_size);

  if (TARGET_ALTIVEC_ABI)
    info->vrsave_mask = compute_vrsave_mask ();

  if (TARGET_ALTIVEC_VRSAVE && info->vrsave_mask)
    info->vrsave_size = 4;

  compute_save_world_info (info);

  /* Calculate the offsets.  */
  switch (DEFAULT_ABI)
    {
    case ABI_NONE:
    default:
      gcc_unreachable ();

    case ABI_AIX:
    case ABI_ELFv2:
    case ABI_DARWIN:
      info->fp_save_offset = -info->fp_size;
      info->gp_save_offset = info->fp_save_offset - info->gp_size;

      if (TARGET_ALTIVEC_ABI)
	{
	  info->vrsave_save_offset = info->gp_save_offset - info->vrsave_size;

	  /* Align stack so vector save area is on a quadword boundary.
	     The padding goes above the vectors.  */
	  if (info->altivec_size != 0)
	    info->altivec_padding_size = info->vrsave_save_offset & 0xF;

	  info->altivec_save_offset = info->vrsave_save_offset
				      - info->altivec_padding_size
				      - info->altivec_size;
	  gcc_assert (info->altivec_size == 0
		      || info->altivec_save_offset % 16 == 0);

	  /* Adjust for ROP protection.  */
	  info->rop_hash_save_offset
	    = info->altivec_save_offset - info->rop_hash_size;
	}
      else
	  /* Adjust for ROP protection.  */
	  info->rop_hash_save_offset
	    = info->gp_save_offset - info->rop_hash_size;

      info->ehrd_offset = info->rop_hash_save_offset - ehrd_size;
      info->ehcr_offset = info->ehrd_offset - ehcr_size;
      info->cr_save_offset = reg_size; /* first word when 64-bit.  */
      info->lr_save_offset = 2*reg_size;
      break;

    case ABI_V4:
      info->fp_save_offset = -info->fp_size;
      info->gp_save_offset = info->fp_save_offset - info->gp_size;
      info->cr_save_offset = info->gp_save_offset - info->cr_size;

      if (TARGET_ALTIVEC_ABI)
	{
	  info->vrsave_save_offset = info->cr_save_offset - info->vrsave_size;

	  /* Align stack so vector save area is on a quadword boundary.  */
	  if (info->altivec_size != 0)
	    info->altivec_padding_size = 16 - (-info->vrsave_save_offset % 16);

	  info->altivec_save_offset = info->vrsave_save_offset
				      - info->altivec_padding_size
				      - info->altivec_size;

	  /* Adjust for AltiVec case.  */
	  info->ehrd_offset = info->altivec_save_offset;
	}
      else
	info->ehrd_offset = info->cr_save_offset;

      info->ehrd_offset -= ehrd_size;
      info->lr_save_offset = reg_size;
    }

  save_align = (TARGET_ALTIVEC_ABI || DEFAULT_ABI == ABI_DARWIN) ? 16 : 8;
  info->save_size = RS6000_ALIGN (info->fp_size
				  + info->gp_size
				  + info->altivec_size
				  + info->altivec_padding_size
				  + info->rop_hash_size
				  + ehrd_size
				  + ehcr_size
				  + info->cr_size
				  + info->vrsave_size,
				  save_align);

  non_fixed_size = info->vars_size + info->parm_size + info->save_size;

  info->total_size = RS6000_ALIGN (non_fixed_size + info->fixed_size,
				   ABI_STACK_BOUNDARY / BITS_PER_UNIT);

  /* Determine if we need to save the link register.  */
  if (info->calls_p
      || ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
	  && crtl->profile
	  && !TARGET_PROFILE_KERNEL)
      || (DEFAULT_ABI == ABI_V4 && cfun->calls_alloca)
#ifdef TARGET_RELOCATABLE
      || (DEFAULT_ABI == ABI_V4
	  && (TARGET_RELOCATABLE || flag_pic > 1)
	  && !constant_pool_empty_p ())
#endif
      || rs6000_ra_ever_killed ())
    info->lr_save_p = 1;

  using_static_chain_p = (cfun->static_chain_decl != NULL_TREE
			  && df_regs_ever_live_p (STATIC_CHAIN_REGNUM)
			  && call_used_or_fixed_reg_p (STATIC_CHAIN_REGNUM));
  info->savres_strategy = rs6000_savres_strategy (info, using_static_chain_p);

  if (!(info->savres_strategy & SAVE_INLINE_GPRS)
      || !(info->savres_strategy & SAVE_INLINE_FPRS)
      || !(info->savres_strategy & SAVE_INLINE_VRS)
      || !(info->savres_strategy & REST_INLINE_GPRS)
      || !(info->savres_strategy & REST_INLINE_FPRS)
      || !(info->savres_strategy & REST_INLINE_VRS))
    info->lr_save_p = 1;

  if (info->lr_save_p)
    df_set_regs_ever_live (LR_REGNO, true);

  /* Determine if we need to allocate any stack frame:

     For AIX we need to push the stack if a frame pointer is needed
     (because the stack might be dynamically adjusted), if we are
     debugging, if we make calls, or if the sum of fp_save, gp_save,
     and local variables are more than the space needed to save all
     non-volatile registers: 32-bit: 18*8 + 19*4 = 220 or 64-bit: 18*8
     + 18*8 = 288 (GPR13 reserved).

     For V.4 we don't have the stack cushion that AIX uses, but assume
     that the debugger can handle stackless frames.  */

  if (info->calls_p)
    info->push_p = 1;

  else if (DEFAULT_ABI == ABI_V4)
    info->push_p = non_fixed_size != 0;

  else if (frame_pointer_needed)
    info->push_p = 1;

  else
    info->push_p = non_fixed_size > (TARGET_32BIT ? 220 : 288);

  return info;
}

static void
debug_stack_info (rs6000_stack_t *info)
{
  const char *abi_string;

  if (! info)
    info = rs6000_stack_info ();

  fprintf (stderr, "\nStack information for function %s:\n",
	   ((current_function_decl && DECL_NAME (current_function_decl))
	    ? IDENTIFIER_POINTER (DECL_NAME (current_function_decl))
	    : "<unknown>"));

  switch (info->abi)
    {
    default:		 abi_string = "Unknown";	break;
    case ABI_NONE:	 abi_string = "NONE";		break;
    case ABI_AIX:	 abi_string = "AIX";		break;
    case ABI_ELFv2:	 abi_string = "ELFv2";		break;
    case ABI_DARWIN:	 abi_string = "Darwin";		break;
    case ABI_V4:	 abi_string = "V.4";		break;
    }

  fprintf (stderr, "\tABI                 = %5s\n", abi_string);

  if (TARGET_ALTIVEC_ABI)
    fprintf (stderr, "\tALTIVEC ABI extensions enabled.\n");

  if (info->first_gp_reg_save != 32)
    fprintf (stderr, "\tfirst_gp_reg_save   = %5d\n", info->first_gp_reg_save);

  if (info->first_fp_reg_save != 64)
    fprintf (stderr, "\tfirst_fp_reg_save   = %5d\n", info->first_fp_reg_save);

  if (info->first_altivec_reg_save <= LAST_ALTIVEC_REGNO)
    fprintf (stderr, "\tfirst_altivec_reg_save = %5d\n",
	     info->first_altivec_reg_save);

  if (info->lr_save_p)
    fprintf (stderr, "\tlr_save_p           = %5d\n", info->lr_save_p);

  if (info->cr_save_p)
    fprintf (stderr, "\tcr_save_p           = %5d\n", info->cr_save_p);

  if (info->vrsave_mask)
    fprintf (stderr, "\tvrsave_mask         = 0x%x\n", info->vrsave_mask);

  if (info->push_p)
    fprintf (stderr, "\tpush_p              = %5d\n", info->push_p);

  if (info->calls_p)
    fprintf (stderr, "\tcalls_p             = %5d\n", info->calls_p);

  if (info->gp_size)
    fprintf (stderr, "\tgp_save_offset      = %5d\n", info->gp_save_offset);

  if (info->fp_size)
    fprintf (stderr, "\tfp_save_offset      = %5d\n", info->fp_save_offset);

  if (info->altivec_size)
    fprintf (stderr, "\taltivec_save_offset = %5d\n",
	     info->altivec_save_offset);

  if (info->vrsave_size)
    fprintf (stderr, "\tvrsave_save_offset  = %5d\n",
	     info->vrsave_save_offset);

  if (info->rop_hash_size)
    fprintf (stderr, "\trop_hash_save_offset = %5d\n",
	     info->rop_hash_save_offset);

  if (info->lr_save_p)
    fprintf (stderr, "\tlr_save_offset      = %5d\n", info->lr_save_offset);

  if (info->cr_save_p)
    fprintf (stderr, "\tcr_save_offset      = %5d\n", info->cr_save_offset);

  if (info->varargs_save_offset)
    fprintf (stderr, "\tvarargs_save_offset = %5d\n", info->varargs_save_offset);

  if (info->total_size)
    fprintf (stderr, "\ttotal_size          = " HOST_WIDE_INT_PRINT_DEC"\n",
	     info->total_size);

  if (info->vars_size)
    fprintf (stderr, "\tvars_size           = " HOST_WIDE_INT_PRINT_DEC"\n",
	     info->vars_size);

  if (info->parm_size)
    fprintf (stderr, "\tparm_size           = %5d\n", info->parm_size);

  if (info->fixed_size)
    fprintf (stderr, "\tfixed_size          = %5d\n", info->fixed_size);

  if (info->gp_size)
    fprintf (stderr, "\tgp_size             = %5d\n", info->gp_size);

  if (info->fp_size)
    fprintf (stderr, "\tfp_size             = %5d\n", info->fp_size);

  if (info->altivec_size)
    fprintf (stderr, "\taltivec_size        = %5d\n", info->altivec_size);

  if (info->vrsave_size)
    fprintf (stderr, "\tvrsave_size         = %5d\n", info->vrsave_size);

  if (info->altivec_padding_size)
    fprintf (stderr, "\taltivec_padding_size= %5d\n",
	     info->altivec_padding_size);

  if (info->rop_hash_size)
    fprintf (stderr, "\trop_hash_size       = %5d\n", info->rop_hash_size);

  if (info->cr_size)
    fprintf (stderr, "\tcr_size             = %5d\n", info->cr_size);

  if (info->save_size)
    fprintf (stderr, "\tsave_size           = %5d\n", info->save_size);

  if (info->reg_size != 4)
    fprintf (stderr, "\treg_size            = %5d\n", info->reg_size);

  fprintf (stderr, "\tsave-strategy       =  %04x\n", info->savres_strategy);

  if (info->abi == ABI_DARWIN)
    fprintf (stderr, "\tWORLD_SAVE_P        = %5d\n", WORLD_SAVE_P(info));

  fprintf (stderr, "\n");
}

rtx
rs6000_return_addr (int count, rtx frame)
{
  /* We can't use get_hard_reg_initial_val for LR when count == 0 if LR
     is trashed by the prologue, as it is for PIC on ABI_V4 and Darwin.  */
  if (count != 0
      || ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_DARWIN) && flag_pic))
    {
      cfun->machine->ra_needs_full_frame = 1;

      if (count == 0)
	/* FRAME is set to frame_pointer_rtx by the generic code, but that
	   is good for loading 0(r1) only when !FRAME_GROWS_DOWNWARD.  */
	frame = stack_pointer_rtx;
      rtx prev_frame_addr = memory_address (Pmode, frame);
      rtx prev_frame = copy_to_reg (gen_rtx_MEM (Pmode, prev_frame_addr));
      rtx lr_save_off = plus_constant (Pmode,
				       prev_frame, RETURN_ADDRESS_OFFSET);
      rtx lr_save_addr = memory_address (Pmode, lr_save_off);
      return gen_rtx_MEM (Pmode, lr_save_addr);
    }

  cfun->machine->ra_need_lr = 1;
  return get_hard_reg_initial_val (Pmode, LR_REGNO);
}

/* Helper function for rs6000_function_ok_for_sibcall.  */

bool
rs6000_decl_ok_for_sibcall (tree decl)
{
  /* Sibcalls are always fine for the Darwin ABI.  */
  if (DEFAULT_ABI == ABI_DARWIN)
    return true;

  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    {
      /* A function compiled using the PC-relative addressing model does not
	 use a TOC pointer; nor is it guaranteed to preserve the value of
	 r2 for its caller's TOC.  Such a function may make sibcalls to any
	 function, whether local or external, without restriction based on
	 TOC-save/restore rules.  */
      if (rs6000_pcrel_p ())
	return true;

      /* Otherwise, under the AIX or ELFv2 ABIs we can't allow sibcalls
	 to non-local functions, because the callee may not preserve the
	 TOC pointer, and there's no way to ensure we restore the TOC when
	 we return.  */
      if (!decl || DECL_EXTERNAL (decl) || DECL_WEAK (decl)
	  || !(*targetm.binds_local_p) (decl))
	return false;

      /* A local sibcall from a function that preserves the TOC pointer
	 to a function that does not is invalid for the same reason.  */
      if (rs6000_fndecl_pcrel_p (decl))
	return false;

      return true;
    }

  /*  With the secure-plt SYSV ABI we can't make non-local calls when
      -fpic/PIC because the plt call stubs use r30.  */
  if (DEFAULT_ABI != ABI_V4
      || (TARGET_SECURE_PLT
	  && flag_pic
	  && (!decl || !((*targetm.binds_local_p) (decl)))))
    return false;

  return true;
}

/* Say whether a function is a candidate for sibcall handling or not.  */

bool
rs6000_function_ok_for_sibcall (tree decl, tree exp)
{
  tree fntype;

  /* The sibcall epilogue may clobber the static chain register.
     ??? We could work harder and avoid that, but it's probably
     not worth the hassle in practice.  */
  if (CALL_EXPR_STATIC_CHAIN (exp))
    return false;

  if (decl)
    fntype = TREE_TYPE (decl);
  else
    fntype = TREE_TYPE (TREE_TYPE (CALL_EXPR_FN (exp)));

  /* We can't do it if the called function has more vector parameters
     than the current function; there's nowhere to put the VRsave code.  */
  if (TARGET_ALTIVEC_ABI
      && TARGET_ALTIVEC_VRSAVE
      && !(decl && decl == current_function_decl))
    {
      function_args_iterator args_iter;
      tree type;
      int nvreg = 0;

      /* Functions with vector parameters are required to have a
	 prototype, so the argument type info must be available
	 here.  */
      FOREACH_FUNCTION_ARGS(fntype, type, args_iter)
	if (VECTOR_TYPE_P (type)
	    && ALTIVEC_OR_VSX_VECTOR_MODE (TYPE_MODE (type)))
	  nvreg++;

      FOREACH_FUNCTION_ARGS(TREE_TYPE (current_function_decl), type, args_iter)
	if (VECTOR_TYPE_P (type)
	    && ALTIVEC_OR_VSX_VECTOR_MODE (TYPE_MODE (type)))
	  nvreg--;

      if (nvreg > 0)
	return false;
    }

  if (rs6000_decl_ok_for_sibcall (decl))
    {
      tree attr_list = TYPE_ATTRIBUTES (fntype);

      if (!lookup_attribute ("longcall", attr_list)
	  || lookup_attribute ("shortcall", attr_list))
	return true;
    }

  return false;
}

static int
rs6000_ra_ever_killed (void)
{
  rtx_insn *top;
  rtx reg;
  rtx_insn *insn;

  if (cfun->is_thunk)
    return 0;

  if (cfun->machine->lr_save_state)
    return cfun->machine->lr_save_state - 1;

  /* regs_ever_live has LR marked as used if any sibcalls are present,
     but this should not force saving and restoring in the
     pro/epilogue.  Likewise, reg_set_between_p thinks a sibcall
     clobbers LR, so that is inappropriate.  */

  /* Also, the prologue can generate a store into LR that
     doesn't really count, like this:

        move LR->R0
        bcl to set PIC register
        move LR->R31
        move R0->LR

     When we're called from the epilogue, we need to avoid counting
     this as a store.  */

  push_topmost_sequence ();
  top = get_insns ();
  pop_topmost_sequence ();
  reg = gen_rtx_REG (Pmode, LR_REGNO);

  for (insn = NEXT_INSN (top); insn != NULL_RTX; insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	{
	  if (CALL_P (insn))
	    {
	      if (!SIBLING_CALL_P (insn))
		return 1;
	    }
	  else if (find_regno_note (insn, REG_INC, LR_REGNO))
	    return 1;
	  else if (set_of (reg, insn) != NULL_RTX
		   && !prologue_epilogue_contains (insn))
	    return 1;
    	}
    }
  return 0;
}

/* Emit instructions needed to load the TOC register.
   This is only needed when TARGET_TOC, TARGET_MINIMAL_TOC, and there is
   a constant pool; or for SVR4 -fpic.  */

void
rs6000_emit_load_toc_table (int fromprolog)
{
  rtx dest;
  dest = gen_rtx_REG (Pmode, RS6000_PIC_OFFSET_TABLE_REGNUM);

  if (TARGET_ELF && TARGET_SECURE_PLT && DEFAULT_ABI == ABI_V4 && flag_pic)
    {
      char buf[30];
      rtx lab, tmp1, tmp2, got;

      lab = gen_label_rtx ();
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (lab));
      lab = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
      if (flag_pic == 2)
	{
	  got = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (toc_label_name));
	  need_toc_init = 1;
	}
      else
	got = rs6000_got_sym ();
      tmp1 = tmp2 = dest;
      if (!fromprolog)
	{
	  tmp1 = gen_reg_rtx (Pmode);
	  tmp2 = gen_reg_rtx (Pmode);
	}
      emit_insn (gen_load_toc_v4_PIC_1 (lab));
      emit_move_insn (tmp1, gen_rtx_REG (Pmode, LR_REGNO));
      emit_insn (gen_load_toc_v4_PIC_3b (tmp2, tmp1, got, lab));
      emit_insn (gen_load_toc_v4_PIC_3c (dest, tmp2, got, lab));
    }
  else if (TARGET_ELF && DEFAULT_ABI == ABI_V4 && flag_pic == 1)
    {
      emit_insn (gen_load_toc_v4_pic_si ());
      emit_move_insn (dest, gen_rtx_REG (Pmode, LR_REGNO));
    }
  else if (TARGET_ELF && DEFAULT_ABI == ABI_V4 && flag_pic == 2)
    {
      char buf[30];
      rtx temp0 = (fromprolog
		   ? gen_rtx_REG (Pmode, 0)
		   : gen_reg_rtx (Pmode));

      if (fromprolog)
	{
	  rtx symF, symL;

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
	  symF = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCL", rs6000_pic_labelno);
	  symL = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));

	  emit_insn (gen_load_toc_v4_PIC_1 (symF));
	  emit_move_insn (dest, gen_rtx_REG (Pmode, LR_REGNO));
	  emit_insn (gen_load_toc_v4_PIC_2 (temp0, dest, symL, symF));
	}
      else
	{
	  rtx tocsym, lab;

	  tocsym = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (toc_label_name));
	  need_toc_init = 1;
	  lab = gen_label_rtx ();
	  emit_insn (gen_load_toc_v4_PIC_1b (tocsym, lab));
	  emit_move_insn (dest, gen_rtx_REG (Pmode, LR_REGNO));
	  if (TARGET_LINK_STACK)
	    emit_insn (gen_addsi3 (dest, dest, GEN_INT (4)));
	  emit_move_insn (temp0, gen_rtx_MEM (Pmode, dest));
	}
      emit_insn (gen_addsi3 (dest, temp0, dest));
    }
  else if (TARGET_ELF && !TARGET_AIX && flag_pic == 0 && TARGET_MINIMAL_TOC)
    {
      /* This is for AIX code running in non-PIC ELF32.  */
      rtx realsym = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (toc_label_name));

      need_toc_init = 1;
      emit_insn (gen_elf_high (dest, realsym));
      emit_insn (gen_elf_low (dest, dest, realsym));
    }
  else
    {
      gcc_assert (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2);

      if (TARGET_32BIT)
	emit_insn (gen_load_toc_aix_si (dest));
      else
	emit_insn (gen_load_toc_aix_di (dest));
    }
}

/* Emit instructions to restore the link register after determining where
   its value has been stored.  */

void
rs6000_emit_eh_reg_restore (rtx source, rtx scratch)
{
  rs6000_stack_t *info = rs6000_stack_info ();
  rtx operands[2];

  operands[0] = source;
  operands[1] = scratch;

  if (info->lr_save_p)
    {
      rtx frame_rtx = stack_pointer_rtx;
      HOST_WIDE_INT sp_offset = 0;
      rtx tmp;

      if (frame_pointer_needed
	  || cfun->calls_alloca
	  || info->total_size > 32767)
	{
	  tmp = gen_frame_mem (Pmode, frame_rtx);
	  emit_move_insn (operands[1], tmp);
	  frame_rtx = operands[1];
	}
      else if (info->push_p)
	sp_offset = info->total_size;

      tmp = plus_constant (Pmode, frame_rtx,
			   info->lr_save_offset + sp_offset);
      tmp = gen_frame_mem (Pmode, tmp);
      emit_move_insn (tmp, operands[0]);
    }
  else
    emit_move_insn (gen_rtx_REG (Pmode, LR_REGNO), operands[0]);

  /* Freeze lr_save_p.  We've just emitted rtl that depends on the
     state of lr_save_p so any change from here on would be a bug.  In
     particular, stop rs6000_ra_ever_killed from considering the SET
     of lr we may have added just above.  */ 
  cfun->machine->lr_save_state = info->lr_save_p + 1;
}

/* This returns nonzero if the current function uses the TOC.  This is
   determined by the presence of (use (unspec ... UNSPEC_TOC)), which
   is generated by the ABI_V4 load_toc_* patterns.
   Return 2 instead of 1 if the load_toc_* pattern is in the function
   partition that doesn't start the function.  */
#if TARGET_ELF
int
uses_TOC (void)
{
  rtx_insn *insn;
  int ret = 1;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	{
	  rtx pat = PATTERN (insn);
	  int i;

	  if (GET_CODE (pat) == PARALLEL)
	    for (i = 0; i < XVECLEN (pat, 0); i++)
	      {
		rtx sub = XVECEXP (pat, 0, i);
		if (GET_CODE (sub) == USE)
		  {
		    sub = XEXP (sub, 0);
		    if (GET_CODE (sub) == UNSPEC
			&& XINT (sub, 1) == UNSPEC_TOC)
		      return ret;
		  }
	      }
	}
      else if (crtl->has_bb_partition
	       && NOTE_P (insn)
	       && NOTE_KIND (insn) == NOTE_INSN_SWITCH_TEXT_SECTIONS)
	ret = 2;
    }
  return 0;
}
#endif

/* Issue assembly directives that create a reference to the given DWARF
   FRAME_TABLE_LABEL from the current function section.  */
void
rs6000_aix_asm_output_dwarf_table_ref (char * frame_table_label)
{
  fprintf (asm_out_file, "\t.ref %s\n",
	   (* targetm.strip_name_encoding) (frame_table_label));
}

/* This ties together stack memory (MEM with an alias set of frame_alias_set)
   and the change to the stack pointer.  */

static void
rs6000_emit_stack_tie (rtx fp, bool hard_frame_needed)
{
  rtvec p;
  int i;
  rtx regs[3];

  i = 0;
  regs[i++] = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  if (hard_frame_needed)
    regs[i++] = gen_rtx_REG (Pmode, HARD_FRAME_POINTER_REGNUM);
  if (!(REGNO (fp) == STACK_POINTER_REGNUM
	|| (hard_frame_needed
	    && REGNO (fp) == HARD_FRAME_POINTER_REGNUM)))
    regs[i++] = fp;

  p = rtvec_alloc (i);
  while (--i >= 0)
    {
      rtx mem = gen_frame_mem (BLKmode, regs[i]);
      RTVEC_ELT (p, i) = gen_rtx_SET (mem, const0_rtx);
    }

  emit_insn (gen_stack_tie (gen_rtx_PARALLEL (VOIDmode, p)));
}

/* Allocate SIZE_INT bytes on the stack using a store with update style insn
   and set the appropriate attributes for the generated insn.  Return the
   first insn which adjusts the stack pointer or the last insn before
   the stack adjustment loop. 

   SIZE_INT is used to create the CFI note for the allocation.

   SIZE_RTX is an rtx containing the size of the adjustment.  Note that
   since stacks grow to lower addresses its runtime value is -SIZE_INT.

   ORIG_SP contains the backchain value that must be stored at *sp.  */

static rtx_insn *
rs6000_emit_allocate_stack_1 (HOST_WIDE_INT size_int, rtx orig_sp)
{
  rtx_insn *insn;

  rtx size_rtx = GEN_INT (-size_int);
  if (size_int > 32767)
    {
      rtx tmp_reg = gen_rtx_REG (Pmode, 0);
      /* Need a note here so that try_split doesn't get confused.  */
      if (get_last_insn () == NULL_RTX)
	emit_note (NOTE_INSN_DELETED);
      insn = emit_move_insn (tmp_reg, size_rtx);
      try_split (PATTERN (insn), insn, 0);
      size_rtx = tmp_reg;
    }
  
  if (TARGET_32BIT)
    insn = emit_insn (gen_movsi_update_stack (stack_pointer_rtx,
					      stack_pointer_rtx,
					      size_rtx,
					      orig_sp));
  else
    insn = emit_insn (gen_movdi_update_stack (stack_pointer_rtx,
					      stack_pointer_rtx,
					      size_rtx,
					      orig_sp));
  rtx par = PATTERN (insn);
  gcc_assert (GET_CODE (par) == PARALLEL);
  rtx set = XVECEXP (par, 0, 0);
  gcc_assert (GET_CODE (set) == SET);
  rtx mem = SET_DEST (set);
  gcc_assert (MEM_P (mem));
  MEM_NOTRAP_P (mem) = 1;
  set_mem_alias_set (mem, get_frame_alias_set ());

  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		gen_rtx_SET (stack_pointer_rtx,
			     gen_rtx_PLUS (Pmode,
					   stack_pointer_rtx,
					   GEN_INT (-size_int))));

  /* Emit a blockage to ensure the allocation/probing insns are
     not optimized, combined, removed, etc.  Add REG_STACK_CHECK
     note for similar reasons.  */
  if (flag_stack_clash_protection)
    {
      add_reg_note (insn, REG_STACK_CHECK, const0_rtx);
      emit_insn (gen_blockage ());
    }

  return insn;
}

static HOST_WIDE_INT
get_stack_clash_protection_probe_interval (void)
{
  return (HOST_WIDE_INT_1U
	  << param_stack_clash_protection_probe_interval);
}

static HOST_WIDE_INT
get_stack_clash_protection_guard_size (void)
{
  return (HOST_WIDE_INT_1U
	  << param_stack_clash_protection_guard_size);
}

/* Allocate ORIG_SIZE bytes on the stack and probe the newly
   allocated space every STACK_CLASH_PROTECTION_PROBE_INTERVAL bytes.

   COPY_REG, if non-null, should contain a copy of the original
   stack pointer at exit from this function.

   This is subtly different than the Ada probing in that it tries hard to
   prevent attacks that jump the stack guard.  Thus it is never allowed to
   allocate more than STACK_CLASH_PROTECTION_PROBE_INTERVAL bytes of stack
   space without a suitable probe.  */
static rtx_insn *
rs6000_emit_probe_stack_range_stack_clash (HOST_WIDE_INT orig_size,
					   rtx copy_reg)
{
  rtx orig_sp = copy_reg;

  HOST_WIDE_INT probe_interval = get_stack_clash_protection_probe_interval ();

  /* Round the size down to a multiple of PROBE_INTERVAL.  */
  HOST_WIDE_INT rounded_size = ROUND_DOWN (orig_size, probe_interval);

  /* If explicitly requested,
       or the rounded size is not the same as the original size
       or the rounded size is greater than a page,
     then we will need a copy of the original stack pointer.  */
  if (rounded_size != orig_size
      || rounded_size > probe_interval
      || copy_reg)
    {
      /* If the caller did not request a copy of the incoming stack
	 pointer, then we use r0 to hold the copy.  */
      if (!copy_reg)
	orig_sp = gen_rtx_REG (Pmode, 0);
      emit_move_insn (orig_sp, stack_pointer_rtx);
    }

  /* There's three cases here.

     One is a single probe which is the most common and most efficiently
     implemented as it does not have to have a copy of the original
     stack pointer if there are no residuals.

     Second is unrolled allocation/probes which we use if there's just
     a few of them.  It needs to save the original stack pointer into a
     temporary for use as a source register in the allocation/probe.

     Last is a loop.  This is the most uncommon case and least efficient.  */
  rtx_insn *retval = NULL;
  if (rounded_size == probe_interval)
    {
      retval = rs6000_emit_allocate_stack_1 (probe_interval, stack_pointer_rtx);

      dump_stack_clash_frame_info (PROBE_INLINE, rounded_size != orig_size);
    }
  else if (rounded_size <= 8 * probe_interval)
    {
      /* The ABI requires using the store with update insns to allocate
	 space and store the backchain into the stack

	 So we save the current stack pointer into a temporary, then
	 emit the store-with-update insns to store the saved stack pointer
	 into the right location in each new page.  */
      for (int i = 0; i < rounded_size; i += probe_interval)
	{
	  rtx_insn *insn
	    = rs6000_emit_allocate_stack_1 (probe_interval, orig_sp);

	  /* Save the first stack adjustment in RETVAL.  */
	  if (i == 0)
	    retval = insn;
	}

      dump_stack_clash_frame_info (PROBE_INLINE, rounded_size != orig_size);
    }
  else
    {
      /* Compute the ending address.  */
      rtx end_addr
	= copy_reg ? gen_rtx_REG (Pmode, 0) : gen_rtx_REG (Pmode, 12);
      rtx rs = GEN_INT (-rounded_size);
      rtx_insn *insn = gen_add3_insn (end_addr, stack_pointer_rtx, rs);
      if (insn == NULL)
	{
	  emit_move_insn (end_addr, rs);
	  insn = gen_add3_insn (end_addr, end_addr, stack_pointer_rtx);
	  gcc_assert (insn);
	}
      bool add_note = false;
      if (!NONJUMP_INSN_P (insn) || NEXT_INSN (insn))
	add_note = true;
      else
	{
	  rtx set = single_set (insn);
	  if (set == NULL_RTX
	      || SET_DEST (set) != end_addr
	      || GET_CODE (SET_SRC (set)) != PLUS
	      || XEXP (SET_SRC (set), 0) != stack_pointer_rtx
	      || XEXP (SET_SRC (set), 1) != rs)
	    add_note = true;
	}
      insn = emit_insn (insn);
      /* Describe the effect of INSN to the CFI engine, unless it
	 is a single insn that describes it itself.  */
      if (add_note)
	add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		      gen_rtx_SET (end_addr,
				   gen_rtx_PLUS (Pmode, stack_pointer_rtx,
						 rs)));
      RTX_FRAME_RELATED_P (insn) = 1;

      /* Emit the loop.  */
      if (TARGET_64BIT)
	retval = emit_insn (gen_probe_stack_rangedi (stack_pointer_rtx,
						     stack_pointer_rtx, orig_sp,
						     end_addr));
      else
	retval = emit_insn (gen_probe_stack_rangesi (stack_pointer_rtx,
						     stack_pointer_rtx, orig_sp,
						     end_addr));
      RTX_FRAME_RELATED_P (retval) = 1;
      /* Describe the effect of INSN to the CFI engine.  */
      add_reg_note (retval, REG_FRAME_RELATED_EXPR,
		    gen_rtx_SET (stack_pointer_rtx, end_addr));

      /* Emit a blockage to ensure the allocation/probing insns are
	 not optimized, combined, removed, etc.  Other cases handle this
	 within their call to rs6000_emit_allocate_stack_1.  */
      emit_insn (gen_blockage ());

      dump_stack_clash_frame_info (PROBE_LOOP, rounded_size != orig_size);
    }

  if (orig_size != rounded_size)
    {
      /* Allocate (and implicitly probe) any residual space.   */
      HOST_WIDE_INT residual = orig_size - rounded_size;

      rtx_insn *insn = rs6000_emit_allocate_stack_1 (residual, orig_sp);

      /* If the residual was the only allocation, then we can return the
	 allocating insn.  */
      if (!retval)
	retval = insn;
    }

  return retval;
}

/* Emit the correct code for allocating stack space, as insns.
   If COPY_REG, make sure a copy of the old frame is left there.
   The generated code may use hard register 0 as a temporary.  */

static rtx_insn *
rs6000_emit_allocate_stack (HOST_WIDE_INT size, rtx copy_reg, int copy_off)
{
  rtx_insn *insn;
  rtx stack_reg = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx tmp_reg = gen_rtx_REG (Pmode, 0);
  rtx todec = gen_int_mode (-size, Pmode);

  if (INTVAL (todec) != -size)
    {
      warning (0, "stack frame too large");
      emit_insn (gen_trap ());
      return 0;
    }

  if (crtl->limit_stack)
    {
      if (REG_P (stack_limit_rtx)
	  && REGNO (stack_limit_rtx) > 1
	  && REGNO (stack_limit_rtx) <= 31)
	{
	  rtx_insn *insn
	    = gen_add3_insn (tmp_reg, stack_limit_rtx, GEN_INT (size));
	  gcc_assert (insn);
	  emit_insn (insn);
	  emit_insn (gen_cond_trap (LTU, stack_reg, tmp_reg, const0_rtx));
	}
      else if (SYMBOL_REF_P (stack_limit_rtx)
	       && TARGET_32BIT
	       && DEFAULT_ABI == ABI_V4
	       && !flag_pic)
	{
	  rtx toload = gen_rtx_CONST (VOIDmode,
				      gen_rtx_PLUS (Pmode,
						    stack_limit_rtx,
						    GEN_INT (size)));

	  /* We cannot use r0 with elf_low.  Lamely solve this problem by
	     moving registers around.  */
	  rtx r11_reg = gen_rtx_REG (Pmode, 11);
	  emit_move_insn (tmp_reg, r11_reg);
	  emit_insn (gen_elf_high (r11_reg, toload));
	  emit_insn (gen_elf_low (r11_reg, r11_reg, toload));
	  emit_insn (gen_cond_trap (LTU, stack_reg, r11_reg, const0_rtx));
	  emit_move_insn (r11_reg, tmp_reg);
	}
      else
	warning (0, "stack limit expression is not supported");
    }

  if (flag_stack_clash_protection)
    {
      if (size < get_stack_clash_protection_guard_size ())
	dump_stack_clash_frame_info (NO_PROBE_SMALL_FRAME, true);
      else
	{
	  rtx_insn *insn = rs6000_emit_probe_stack_range_stack_clash (size,
								      copy_reg);

	  /* If we asked for a copy with an offset, then we still need add in
	     the offset.  */
	  if (copy_reg && copy_off)
	    emit_insn (gen_add3_insn (copy_reg, copy_reg, GEN_INT (copy_off)));
	  return insn;
	}
    }

  if (copy_reg)
    {
      if (copy_off != 0)
	emit_insn (gen_add3_insn (copy_reg, stack_reg, GEN_INT (copy_off)));
      else
	emit_move_insn (copy_reg, stack_reg);
    }

  /* Since we didn't use gen_frame_mem to generate the MEM, grab
     it now and set the alias set/attributes. The above gen_*_update
     calls will generate a PARALLEL with the MEM set being the first
     operation. */
  insn = rs6000_emit_allocate_stack_1 (size, stack_reg);
  return insn;
}

#define PROBE_INTERVAL (1 << STACK_CHECK_PROBE_INTERVAL_EXP)

#if PROBE_INTERVAL > 32768
#error Cannot use indexed addressing mode for stack probing
#endif

/* Emit code to probe a range of stack addresses from FIRST to FIRST+SIZE,
   inclusive.  These are offsets from the current stack pointer.  */

static void
rs6000_emit_probe_stack_range (HOST_WIDE_INT first, HOST_WIDE_INT size)
{
  /* See if we have a constant small number of probes to generate.  If so,
     that's the easy case.  */
  if (first + size <= 32768)
    {
      HOST_WIDE_INT i;

      /* Probe at FIRST + N * PROBE_INTERVAL for values of N from 1 until
	 it exceeds SIZE.  If only one probe is needed, this will not
	 generate any code.  Then probe at FIRST + SIZE.  */
      for (i = PROBE_INTERVAL; i < size; i += PROBE_INTERVAL)
	emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
					 -(first + i)));

      emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
				       -(first + size)));
    }

  /* Otherwise, do the same as above, but in a loop.  Note that we must be
     extra careful with variables wrapping around because we might be at
     the very top (or the very bottom) of the address space and we have
     to be able to handle this case properly; in particular, we use an
     equality test for the loop condition.  */
  else
    {
      HOST_WIDE_INT rounded_size;
      rtx r12 = gen_rtx_REG (Pmode, 12);
      rtx r0 = gen_rtx_REG (Pmode, 0);

      /* Sanity check for the addressing mode we're going to use.  */
      gcc_assert (first <= 32768);

      /* Step 1: round SIZE to the previous multiple of the interval.  */

      rounded_size = ROUND_DOWN (size, PROBE_INTERVAL);


      /* Step 2: compute initial and final value of the loop counter.  */

      /* TEST_ADDR = SP + FIRST.  */
      emit_insn (gen_rtx_SET (r12, plus_constant (Pmode, stack_pointer_rtx,
						  -first)));

      /* LAST_ADDR = SP + FIRST + ROUNDED_SIZE.  */
      if (rounded_size > 32768)
	{
	  emit_move_insn (r0, GEN_INT (-rounded_size));
	  emit_insn (gen_rtx_SET (r0, gen_rtx_PLUS (Pmode, r12, r0)));
	}
      else
	emit_insn (gen_rtx_SET (r0, plus_constant (Pmode, r12,
						   -rounded_size)));


      /* Step 3: the loop

	 do
	   {
	     TEST_ADDR = TEST_ADDR + PROBE_INTERVAL
	     probe at TEST_ADDR
	   }
	 while (TEST_ADDR != LAST_ADDR)

	 probes at FIRST + N * PROBE_INTERVAL for values of N from 1
	 until it is equal to ROUNDED_SIZE.  */

      if (TARGET_64BIT)
	emit_insn (gen_probe_stack_rangedi (r12, r12, stack_pointer_rtx, r0));
      else
	emit_insn (gen_probe_stack_rangesi (r12, r12, stack_pointer_rtx, r0));


      /* Step 4: probe at FIRST + SIZE if we cannot assert at compile-time
	 that SIZE is equal to ROUNDED_SIZE.  */

      if (size != rounded_size)
	emit_stack_probe (plus_constant (Pmode, r12, rounded_size - size));
    }
}

/* Probe a range of stack addresses from REG1 to REG2 inclusive.  These are
   addresses, not offsets.  */

static const char *
output_probe_stack_range_1 (rtx reg1, rtx reg2)
{
  static int labelno = 0;
  char loop_lab[32];
  rtx xops[2];

  ASM_GENERATE_INTERNAL_LABEL (loop_lab, "LPSRL", labelno++);

  /* Loop.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_lab);

  /* TEST_ADDR = TEST_ADDR + PROBE_INTERVAL.  */
  xops[0] = reg1;
  xops[1] = GEN_INT (-PROBE_INTERVAL);
  output_asm_insn ("addi %0,%0,%1", xops);

  /* Probe at TEST_ADDR.  */
  xops[1] = gen_rtx_REG (Pmode, 0);
  output_asm_insn ("stw %1,0(%0)", xops);

  /* Test if TEST_ADDR == LAST_ADDR.  */
  xops[1] = reg2;
  if (TARGET_64BIT)
    output_asm_insn ("cmpd 0,%0,%1", xops);
  else
    output_asm_insn ("cmpw 0,%0,%1", xops);

  /* Branch.  */
  fputs ("\tbne 0,", asm_out_file);
  assemble_name_raw (asm_out_file, loop_lab);
  fputc ('\n', asm_out_file);

  return "";
}

/* This function is called when rs6000_frame_related is processing
   SETs within a PARALLEL, and returns whether the REGNO save ought to
   be marked RTX_FRAME_RELATED_P.  The PARALLELs involved are those
   for out-of-line register save functions, store multiple, and the
   Darwin world_save.  They may contain registers that don't really
   need saving.  */

static bool
interesting_frame_related_regno (unsigned int regno)
{
  /* Saves apparently of r0 are actually saving LR.  It doesn't make
     sense to substitute the regno here to test save_reg_p (LR_REGNO).
     We *know* LR needs saving, and dwarf2cfi.cc is able to deduce that
     (set (mem) (r0)) is saving LR from a prior (set (r0) (lr)) marked
     as frame related.  */
  if (regno == 0)
    return true;
  /* If we see CR2 then we are here on a Darwin world save.  Saves of
     CR2 signify the whole CR is being saved.  This is a long-standing
     ABI wart fixed by ELFv2.  As for r0/lr there is no need to check
     that CR needs to be saved.  */
  if (regno == CR2_REGNO)
    return true;
  /* Omit frame info for any user-defined global regs.  If frame info
     is supplied for them, frame unwinding will restore a user reg.
     Also omit frame info for any reg we don't need to save, as that
     bloats frame info and can cause problems with shrink wrapping.
     Since global regs won't be seen as needing to be saved, both of
     these conditions are covered by save_reg_p.  */
  return save_reg_p (regno);
}

/* Probe a range of stack addresses from REG1 to REG3 inclusive.  These are
   addresses, not offsets.

   REG2 contains the backchain that must be stored into *sp at each allocation.

   This is subtly different than the Ada probing above in that it tries hard
   to prevent attacks that jump the stack guard.  Thus, it is never allowed
   to allocate more than PROBE_INTERVAL bytes of stack space without a
   suitable probe.  */

static const char *
output_probe_stack_range_stack_clash (rtx reg1, rtx reg2, rtx reg3)
{
  static int labelno = 0;
  char loop_lab[32];
  rtx xops[3];

  HOST_WIDE_INT probe_interval = get_stack_clash_protection_probe_interval ();

  ASM_GENERATE_INTERNAL_LABEL (loop_lab, "LPSRL", labelno++);

  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_lab);

  /* This allocates and probes.  */
  xops[0] = reg1;
  xops[1] = reg2;
  xops[2] = GEN_INT (-probe_interval);
  if (TARGET_64BIT)
    output_asm_insn ("stdu %1,%2(%0)", xops);
  else
    output_asm_insn ("stwu %1,%2(%0)", xops);

  /* Jump to LOOP_LAB if TEST_ADDR != LAST_ADDR.  */
  xops[0] = reg1;
  xops[1] = reg3;
  if (TARGET_64BIT)
    output_asm_insn ("cmpd 0,%0,%1", xops);
  else
    output_asm_insn ("cmpw 0,%0,%1", xops);

  fputs ("\tbne 0,", asm_out_file);
  assemble_name_raw (asm_out_file, loop_lab);
  fputc ('\n', asm_out_file);

  return "";
}

/* Wrapper around the output_probe_stack_range routines.  */
const char *
output_probe_stack_range (rtx reg1, rtx reg2, rtx reg3)
{
  if (flag_stack_clash_protection)
    return output_probe_stack_range_stack_clash (reg1, reg2, reg3);
  else
    return output_probe_stack_range_1 (reg1, reg3);
}

/* Add to 'insn' a note which is PATTERN (INSN) but with REG replaced
   with (plus:P (reg 1) VAL), and with REG2 replaced with REPL2 if REG2
   is not NULL.  It would be nice if dwarf2out_frame_debug_expr could
   deduce these equivalences by itself so it wasn't necessary to hold
   its hand so much.  Don't be tempted to always supply d2_f_d_e with
   the actual cfa register, ie. r31 when we are using a hard frame
   pointer.  That fails when saving regs off r1, and sched moves the
   r31 setup past the reg saves.  */

static rtx_insn *
rs6000_frame_related (rtx_insn *insn, rtx reg, HOST_WIDE_INT val,
		      rtx reg2, rtx repl2)
{
  rtx repl;

  if (REGNO (reg) == STACK_POINTER_REGNUM)
    {
      gcc_checking_assert (val == 0);
      repl = NULL_RTX;
    }
  else
    repl = gen_rtx_PLUS (Pmode, gen_rtx_REG (Pmode, STACK_POINTER_REGNUM),
			 GEN_INT (val));

  rtx pat = PATTERN (insn);
  if (!repl && !reg2)
    {
      /* No need for any replacement.  Just set RTX_FRAME_RELATED_P.  */
      if (GET_CODE (pat) == PARALLEL)
	for (int i = 0; i < XVECLEN (pat, 0); i++)
	  if (GET_CODE (XVECEXP (pat, 0, i)) == SET)
	    {
	      rtx set = XVECEXP (pat, 0, i);

	      if (!REG_P (SET_SRC (set))
		  || interesting_frame_related_regno (REGNO (SET_SRC (set))))
		RTX_FRAME_RELATED_P (set) = 1;
	    }
      RTX_FRAME_RELATED_P (insn) = 1;
      return insn;
    }

  /* We expect that 'pat' is either a SET or a PARALLEL containing
     SETs (and possibly other stuff).  In a PARALLEL, all the SETs
     are important so they all have to be marked RTX_FRAME_RELATED_P.
     Call simplify_replace_rtx on the SETs rather than the whole insn
     so as to leave the other stuff alone (for example USE of r12).  */

  set_used_flags (pat);
  if (GET_CODE (pat) == SET)
    {
      if (repl)
	pat = simplify_replace_rtx (pat, reg, repl);
      if (reg2)
	pat = simplify_replace_rtx (pat, reg2, repl2);
    }
  else if (GET_CODE (pat) == PARALLEL)
    {
      pat = shallow_copy_rtx (pat);
      XVEC (pat, 0) = shallow_copy_rtvec (XVEC (pat, 0));

      for (int i = 0; i < XVECLEN (pat, 0); i++)
	if (GET_CODE (XVECEXP (pat, 0, i)) == SET)
	  {
	    rtx set = XVECEXP (pat, 0, i);

	    if (repl)
	      set = simplify_replace_rtx (set, reg, repl);
	    if (reg2)
	      set = simplify_replace_rtx (set, reg2, repl2);
	    XVECEXP (pat, 0, i) = set;

	    if (!REG_P (SET_SRC (set))
		|| interesting_frame_related_regno (REGNO (SET_SRC (set))))
	      RTX_FRAME_RELATED_P (set) = 1;
	  }
    }
  else
    gcc_unreachable ();

  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_FRAME_RELATED_EXPR, copy_rtx_if_shared (pat));

  return insn;
}

/* Returns an insn that has a vrsave set operation with the
   appropriate CLOBBERs.  */

static rtx
generate_set_vrsave (rtx reg, rs6000_stack_t *info, int epiloguep)
{
  int nclobs, i;
  rtx insn, clobs[TOTAL_ALTIVEC_REGS + 1];
  rtx vrsave = gen_rtx_REG (SImode, VRSAVE_REGNO);

  clobs[0]
    = gen_rtx_SET (vrsave,
		   gen_rtx_UNSPEC_VOLATILE (SImode,
					    gen_rtvec (2, reg, vrsave),
					    UNSPECV_SET_VRSAVE));

  nclobs = 1;

  /* We need to clobber the registers in the mask so the scheduler
     does not move sets to VRSAVE before sets of AltiVec registers.

     However, if the function receives nonlocal gotos, reload will set
     all call saved registers live.  We will end up with:

     	(set (reg 999) (mem))
	(parallel [ (set (reg vrsave) (unspec blah))
		    (clobber (reg 999))])

     The clobber will cause the store into reg 999 to be dead, and
     flow will attempt to delete an epilogue insn.  In this case, we
     need an unspec use/set of the register.  */

  for (i = FIRST_ALTIVEC_REGNO; i <= LAST_ALTIVEC_REGNO; ++i)
    if (info->vrsave_mask & ALTIVEC_REG_BIT (i))
      {
	if (!epiloguep || call_used_or_fixed_reg_p (i))
	  clobs[nclobs++] = gen_hard_reg_clobber (V4SImode, i);
	else
	  {
	    rtx reg = gen_rtx_REG (V4SImode, i);

	    clobs[nclobs++]
	      = gen_rtx_SET (reg,
			     gen_rtx_UNSPEC (V4SImode,
					     gen_rtvec (1, reg), 27));
	  }
      }

  insn = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (nclobs));

  for (i = 0; i < nclobs; ++i)
    XVECEXP (insn, 0, i) = clobs[i];

  return insn;
}

static rtx
gen_frame_set (rtx reg, rtx frame_reg, int offset, bool store)
{
  rtx addr, mem;

  addr = gen_rtx_PLUS (Pmode, frame_reg, GEN_INT (offset));
  mem = gen_frame_mem (GET_MODE (reg), addr);
  return gen_rtx_SET (store ? mem : reg, store ? reg : mem);
}

static rtx
gen_frame_load (rtx reg, rtx frame_reg, int offset)
{
  return gen_frame_set (reg, frame_reg, offset, false);
}

static rtx
gen_frame_store (rtx reg, rtx frame_reg, int offset)
{
  return gen_frame_set (reg, frame_reg, offset, true);
}

/* Save a register into the frame, and emit RTX_FRAME_RELATED_P notes.
   Save REGNO into [FRAME_REG + OFFSET] in mode MODE.  */

static rtx_insn *
emit_frame_save (rtx frame_reg, machine_mode mode,
		 unsigned int regno, int offset, HOST_WIDE_INT frame_reg_to_sp)
{
  rtx reg;

  /* Some cases that need register indexed addressing.  */
  gcc_checking_assert (!(TARGET_ALTIVEC_ABI && ALTIVEC_VECTOR_MODE (mode))
			 || (TARGET_VSX && ALTIVEC_OR_VSX_VECTOR_MODE (mode)));

  reg = gen_rtx_REG (mode, regno);
  rtx_insn *insn = emit_insn (gen_frame_store (reg, frame_reg, offset));
  return rs6000_frame_related (insn, frame_reg, frame_reg_to_sp,
			       NULL_RTX, NULL_RTX);
}

/* Emit an offset memory reference suitable for a frame store, while
   converting to a valid addressing mode.  */

static rtx
gen_frame_mem_offset (machine_mode mode, rtx reg, int offset)
{
  return gen_frame_mem (mode, gen_rtx_PLUS (Pmode, reg, GEN_INT (offset)));
}

#ifndef TARGET_FIX_AND_CONTINUE
#define TARGET_FIX_AND_CONTINUE 0
#endif

/* It's really GPR 13 or 14, FPR 14 and VR 20.  We need the smallest.  */
#define FIRST_SAVRES_REGISTER FIRST_SAVED_GP_REGNO
#define LAST_SAVRES_REGISTER 31
#define N_SAVRES_REGISTERS (LAST_SAVRES_REGISTER - FIRST_SAVRES_REGISTER + 1)

enum {
  SAVRES_LR = 0x1,
  SAVRES_SAVE = 0x2,
  SAVRES_REG = 0x0c,
  SAVRES_GPR = 0,
  SAVRES_FPR = 4,
  SAVRES_VR  = 8
};

static GTY(()) rtx savres_routine_syms[N_SAVRES_REGISTERS][12];

/* Temporary holding space for an out-of-line register save/restore
   routine name.  */
static char savres_routine_name[30];

/* Return the name for an out-of-line register save/restore routine.
   We are saving/restoring GPRs if GPR is true.  */

static char *
rs6000_savres_routine_name (int regno, int sel)
{
  const char *prefix = "";
  const char *suffix = "";

  /* Different targets are supposed to define
     {SAVE,RESTORE}_FP_{PREFIX,SUFFIX} with the idea that the needed
     routine name could be defined with:

     sprintf (name, "%s%d%s", SAVE_FP_PREFIX, regno, SAVE_FP_SUFFIX)

     This is a nice idea in practice, but in reality, things are
     complicated in several ways:

     - ELF targets have save/restore routines for GPRs.

     - PPC64 ELF targets have routines for save/restore of GPRs that
       differ in what they do with the link register, so having a set
       prefix doesn't work.  (We only use one of the save routines at
       the moment, though.)

     - PPC32 elf targets have "exit" versions of the restore routines
       that restore the link register and can save some extra space.
       These require an extra suffix.  (There are also "tail" versions
       of the restore routines and "GOT" versions of the save routines,
       but we don't generate those at present.  Same problems apply,
       though.)

     We deal with all this by synthesizing our own prefix/suffix and
     using that for the simple sprintf call shown above.  */
  if (DEFAULT_ABI == ABI_V4)
    {
      if (TARGET_64BIT)
	goto aix_names;

      if ((sel & SAVRES_REG) == SAVRES_GPR)
	prefix = (sel & SAVRES_SAVE) ? "_savegpr_" : "_restgpr_";
      else if ((sel & SAVRES_REG) == SAVRES_FPR)
	prefix = (sel & SAVRES_SAVE) ? "_savefpr_" : "_restfpr_";
      else if ((sel & SAVRES_REG) == SAVRES_VR)
	prefix = (sel & SAVRES_SAVE) ? "_savevr_" : "_restvr_";
      else
	abort ();

      if ((sel & SAVRES_LR))
	suffix = "_x";
    }
  else if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    {
#if !defined (POWERPC_LINUX) && !defined (POWERPC_FREEBSD)
      /* No out-of-line save/restore routines for GPRs on AIX.  */
      gcc_assert (!TARGET_AIX || (sel & SAVRES_REG) != SAVRES_GPR);
#endif

    aix_names:
      if ((sel & SAVRES_REG) == SAVRES_GPR)
	prefix = ((sel & SAVRES_SAVE)
		  ? ((sel & SAVRES_LR) ? "_savegpr0_" : "_savegpr1_")
		  : ((sel & SAVRES_LR) ? "_restgpr0_" : "_restgpr1_"));
      else if ((sel & SAVRES_REG) == SAVRES_FPR)
	{
#if defined (POWERPC_LINUX) || defined (POWERPC_FREEBSD)
	  if ((sel & SAVRES_LR))
	    prefix = ((sel & SAVRES_SAVE) ? "_savefpr_" : "_restfpr_");
	  else
#endif
	    {
	      prefix = (sel & SAVRES_SAVE) ? SAVE_FP_PREFIX : RESTORE_FP_PREFIX;
	      suffix = (sel & SAVRES_SAVE) ? SAVE_FP_SUFFIX : RESTORE_FP_SUFFIX;
	    }
	}
      else if ((sel & SAVRES_REG) == SAVRES_VR)
	prefix = (sel & SAVRES_SAVE) ? "_savevr_" : "_restvr_";
      else
	abort ();
    }

   if (DEFAULT_ABI == ABI_DARWIN)
    {
      /* The Darwin approach is (slightly) different, in order to be
	 compatible with code generated by the system toolchain.  There is a
	 single symbol for the start of save sequence, and the code here
	 embeds an offset into that code on the basis of the first register
	 to be saved.  */
      prefix = (sel & SAVRES_SAVE) ? "save" : "rest" ;
      if ((sel & SAVRES_REG) == SAVRES_GPR)
	sprintf (savres_routine_name, "*%sGPR%s%s%.0d ; %s r%d-r31", prefix,
		 ((sel & SAVRES_LR) ? "x" : ""), (regno == 13 ? "" : "+"),
		 (regno - 13) * 4, prefix, regno);
      else if ((sel & SAVRES_REG) == SAVRES_FPR)
	sprintf (savres_routine_name, "*%sFP%s%.0d ; %s f%d-f31", prefix,
		 (regno == 14 ? "" : "+"), (regno - 14) * 4, prefix, regno);
      else if ((sel & SAVRES_REG) == SAVRES_VR)
	sprintf (savres_routine_name, "*%sVEC%s%.0d ; %s v%d-v31", prefix,
		 (regno == 20 ? "" : "+"), (regno - 20) * 8, prefix, regno);
      else
	abort ();
    }
  else
    sprintf (savres_routine_name, "%s%d%s", prefix, regno, suffix);

  return savres_routine_name;
}

/* Return an RTL SYMBOL_REF for an out-of-line register save/restore routine.
   We are saving/restoring GPRs if GPR is true.  */

static rtx
rs6000_savres_routine_sym (rs6000_stack_t *info, int sel)
{
  int regno = ((sel & SAVRES_REG) == SAVRES_GPR
	       ? info->first_gp_reg_save
	       : (sel & SAVRES_REG) == SAVRES_FPR
	       ? info->first_fp_reg_save - 32
	       : (sel & SAVRES_REG) == SAVRES_VR
	       ? info->first_altivec_reg_save - FIRST_ALTIVEC_REGNO
	       : -1);
  rtx sym;
  int select = sel;

  /* Don't generate bogus routine names.  */
  gcc_assert (FIRST_SAVRES_REGISTER <= regno
	      && regno <= LAST_SAVRES_REGISTER
	      && select >= 0 && select <= 12);

  sym = savres_routine_syms[regno-FIRST_SAVRES_REGISTER][select];

  if (sym == NULL)
    {
      char *name;

      name = rs6000_savres_routine_name (regno, sel);

      sym = savres_routine_syms[regno-FIRST_SAVRES_REGISTER][select]
	= gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (name));
      SYMBOL_REF_FLAGS (sym) |= SYMBOL_FLAG_FUNCTION;
    }

  return sym;
}

/* Emit a sequence of insns, including a stack tie if needed, for
   resetting the stack pointer.  If UPDT_REGNO is not 1, then don't
   reset the stack pointer, but move the base of the frame into
   reg UPDT_REGNO for use by out-of-line register restore routines.  */

static rtx
rs6000_emit_stack_reset (rtx frame_reg_rtx, HOST_WIDE_INT frame_off,
			 unsigned updt_regno)
{
  /* If there is nothing to do, don't do anything.  */
  if (frame_off == 0 && REGNO (frame_reg_rtx) == updt_regno)
    return NULL_RTX;

  rtx updt_reg_rtx = gen_rtx_REG (Pmode, updt_regno);

  /* This blockage is needed so that sched doesn't decide to move
     the sp change before the register restores.  */
  if (DEFAULT_ABI == ABI_V4)
    return emit_insn (gen_stack_restore_tie (updt_reg_rtx, frame_reg_rtx,
					     GEN_INT (frame_off)));

  /* If we are restoring registers out-of-line, we will be using the
     "exit" variants of the restore routines, which will reset the
     stack for us.  But we do need to point updt_reg into the
     right place for those routines.  */
  if (frame_off != 0)
    return emit_insn (gen_add3_insn (updt_reg_rtx,
				     frame_reg_rtx, GEN_INT (frame_off)));
  else
    return emit_move_insn (updt_reg_rtx, frame_reg_rtx);

  return NULL_RTX;
}

/* Return the register number used as a pointer by out-of-line
   save/restore functions.  */

static inline unsigned
ptr_regno_for_savres (int sel)
{
  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    return (sel & SAVRES_REG) == SAVRES_FPR || (sel & SAVRES_LR) ? 1 : 12;
  return DEFAULT_ABI == ABI_DARWIN && (sel & SAVRES_REG) == SAVRES_FPR ? 1 : 11;
}

/* Construct a parallel rtx describing the effect of a call to an
   out-of-line register save/restore routine, and emit the insn
   or jump_insn as appropriate.  */

static rtx_insn *
rs6000_emit_savres_rtx (rs6000_stack_t *info,
			rtx frame_reg_rtx, int save_area_offset, int lr_offset,
			machine_mode reg_mode, int sel)
{
  int i;
  int offset, start_reg, end_reg, n_regs, use_reg;
  int reg_size = GET_MODE_SIZE (reg_mode);
  rtx sym;
  rtvec p;
  rtx par;
  rtx_insn *insn;

  offset = 0;
  start_reg = ((sel & SAVRES_REG) == SAVRES_GPR
	       ? info->first_gp_reg_save
	       : (sel & SAVRES_REG) == SAVRES_FPR
	       ? info->first_fp_reg_save
	       : (sel & SAVRES_REG) == SAVRES_VR
	       ? info->first_altivec_reg_save
	       : -1);
  end_reg = ((sel & SAVRES_REG) == SAVRES_GPR
	     ? 32
	     : (sel & SAVRES_REG) == SAVRES_FPR
	     ? 64
	     : (sel & SAVRES_REG) == SAVRES_VR
	     ? LAST_ALTIVEC_REGNO + 1
	     : -1);
  n_regs = end_reg - start_reg;
  p = rtvec_alloc (3 + ((sel & SAVRES_LR) ? 1 : 0)
		   + ((sel & SAVRES_REG) == SAVRES_VR ? 1 : 0)
		   + n_regs);

  if (!(sel & SAVRES_SAVE) && (sel & SAVRES_LR))
    RTVEC_ELT (p, offset++) = ret_rtx;

  RTVEC_ELT (p, offset++) = gen_hard_reg_clobber (Pmode, LR_REGNO);

  sym = rs6000_savres_routine_sym (info, sel);
  RTVEC_ELT (p, offset++) = gen_rtx_USE (VOIDmode, sym);

  use_reg = ptr_regno_for_savres (sel);
  if ((sel & SAVRES_REG) == SAVRES_VR)
    {
      /* Vector regs are saved/restored using [reg+reg] addressing.  */
      RTVEC_ELT (p, offset++) = gen_hard_reg_clobber (Pmode, use_reg);
      RTVEC_ELT (p, offset++)
	= gen_rtx_USE (VOIDmode, gen_rtx_REG (Pmode, 0));
    }
  else
    RTVEC_ELT (p, offset++)
      = gen_rtx_USE (VOIDmode, gen_rtx_REG (Pmode, use_reg));

  for (i = 0; i < end_reg - start_reg; i++)
    RTVEC_ELT (p, i + offset)
      = gen_frame_set (gen_rtx_REG (reg_mode, start_reg + i),
		       frame_reg_rtx, save_area_offset + reg_size * i,
		       (sel & SAVRES_SAVE) != 0);

  if ((sel & SAVRES_SAVE) && (sel & SAVRES_LR))
    RTVEC_ELT (p, i + offset)
      = gen_frame_store (gen_rtx_REG (Pmode, 0), frame_reg_rtx, lr_offset);

  par = gen_rtx_PARALLEL (VOIDmode, p);

  if (!(sel & SAVRES_SAVE) && (sel & SAVRES_LR))
    {
      insn = emit_jump_insn (par);
      JUMP_LABEL (insn) = ret_rtx;
    }
  else
    insn = emit_insn (par);
  return insn;
}

/* Emit prologue code to store CR fields that need to be saved into REG.  This
   function should only be called when moving the non-volatile CRs to REG, it
   is not a general purpose routine to move the entire set of CRs to REG.
   Specifically, gen_prologue_movesi_from_cr() does not contain uses of the
   volatile CRs.  */

static void
rs6000_emit_prologue_move_from_cr (rtx reg)
{
  /* Only the ELFv2 ABI allows storing only selected fields.  */
  if (DEFAULT_ABI == ABI_ELFv2 && TARGET_MFCRF)
    {
      int i, cr_reg[8], count = 0;

      /* Collect CR fields that must be saved.  */
      for (i = 0; i < 8; i++)
	if (save_reg_p (CR0_REGNO + i))
	  cr_reg[count++] = i;

      /* If it's just a single one, use mfcrf.  */
      if (count == 1)
	{
	  rtvec p = rtvec_alloc (1);
	  rtvec r = rtvec_alloc (2);
	  RTVEC_ELT (r, 0) = gen_rtx_REG (CCmode, CR0_REGNO + cr_reg[0]);
	  RTVEC_ELT (r, 1) = GEN_INT (1 << (7 - cr_reg[0]));
	  RTVEC_ELT (p, 0)
	    = gen_rtx_SET (reg,
			   gen_rtx_UNSPEC (SImode, r, UNSPEC_MOVESI_FROM_CR));

	  emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
	  return;
	}

      /* ??? It might be better to handle count == 2 / 3 cases here
	 as well, using logical operations to combine the values.  */
    }

  emit_insn (gen_prologue_movesi_from_cr (reg));
}

/* Return whether the split-stack arg pointer (r12) is used.  */

static bool
split_stack_arg_pointer_used_p (void)
{
  /* If the pseudo holding the arg pointer is no longer a pseudo,
     then the arg pointer is used.  */
  if (cfun->machine->split_stack_arg_pointer != NULL_RTX
      && (!REG_P (cfun->machine->split_stack_arg_pointer)
	  || HARD_REGISTER_P (cfun->machine->split_stack_arg_pointer)))
    return true;

  /* Unfortunately we also need to do some code scanning, since
     r12 may have been substituted for the pseudo.  */
  rtx_insn *insn;
  basic_block bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;
  FOR_BB_INSNS (bb, insn)
    if (NONDEBUG_INSN_P (insn))
      {
	/* A call destroys r12.  */
	if (CALL_P (insn))
	  return false;

	df_ref use;
	FOR_EACH_INSN_USE (use, insn)
	  {
	    rtx x = DF_REF_REG (use);
	    if (REG_P (x) && REGNO (x) == 12)
	      return true;
	  }
	df_ref def;
	FOR_EACH_INSN_DEF (def, insn)
	  {
	    rtx x = DF_REF_REG (def);
	    if (REG_P (x) && REGNO (x) == 12)
	      return false;
	  }
      }
  return bitmap_bit_p (DF_LR_OUT (bb), 12);
}

/* Return whether we need to emit an ELFv2 global entry point prologue.  */

bool
rs6000_global_entry_point_prologue_needed_p (void)
{
  /* Only needed for the ELFv2 ABI.  */
  if (DEFAULT_ABI != ABI_ELFv2)
    return false;

  /* With -msingle-pic-base, we assume the whole program shares the same
     TOC, so no global entry point prologues are needed anywhere.  */
  if (TARGET_SINGLE_PIC_BASE)
    return false;

  /* PC-relative functions never generate a global entry point prologue.  */
  if (rs6000_pcrel_p ())
    return false;

  /* Ensure we have a global entry point for thunks.   ??? We could
     avoid that if the target routine doesn't need a global entry point,
     but we do not know whether this is the case at this point.  */
  if (cfun->is_thunk)
    return true;

  /* For regular functions, rs6000_emit_prologue sets this flag if the
     routine ever uses the TOC pointer.  */
  return cfun->machine->r2_setup_needed;
}

/* Implement TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS.  */
sbitmap
rs6000_get_separate_components (void)
{
  rs6000_stack_t *info = rs6000_stack_info ();

  if (WORLD_SAVE_P (info))
    return NULL;

  gcc_assert (!(info->savres_strategy & SAVE_MULTIPLE)
	      && !(info->savres_strategy & REST_MULTIPLE));

  /* Component 0 is the save/restore of LR (done via GPR0).
     Component 2 is the save of the TOC (GPR2).
     Components 13..31 are the save/restore of GPR13..GPR31.
     Components 46..63 are the save/restore of FPR14..FPR31.  */

  cfun->machine->n_components = 64;

  sbitmap components = sbitmap_alloc (cfun->machine->n_components);
  bitmap_clear (components);

  int reg_size = TARGET_32BIT ? 4 : 8;
  int fp_reg_size = 8;

  /* The GPRs we need saved to the frame.  */
  if ((info->savres_strategy & SAVE_INLINE_GPRS)
      && (info->savres_strategy & REST_INLINE_GPRS))
    {
      int offset = info->gp_save_offset;
      if (info->push_p)
	offset += info->total_size;

      for (unsigned regno = info->first_gp_reg_save; regno < 32; regno++)
	{
	  if (IN_RANGE (offset, -0x8000, 0x7fff)
	      && save_reg_p (regno))
	    bitmap_set_bit (components, regno);

	  offset += reg_size;
	}
    }

  /* Don't mess with the hard frame pointer.  */
  if (frame_pointer_needed)
    bitmap_clear_bit (components, HARD_FRAME_POINTER_REGNUM);

  /* Don't mess with the fixed TOC register.  */
  if ((TARGET_TOC && TARGET_MINIMAL_TOC)
      || (flag_pic == 1 && DEFAULT_ABI == ABI_V4)
      || (flag_pic && DEFAULT_ABI == ABI_DARWIN))
    bitmap_clear_bit (components, RS6000_PIC_OFFSET_TABLE_REGNUM);

  /* The FPRs we need saved to the frame.  */
  if ((info->savres_strategy & SAVE_INLINE_FPRS)
      && (info->savres_strategy & REST_INLINE_FPRS))
    {
      int offset = info->fp_save_offset;
      if (info->push_p)
	offset += info->total_size;

      for (unsigned regno = info->first_fp_reg_save; regno < 64; regno++)
	{
	  if (IN_RANGE (offset, -0x8000, 0x7fff) && save_reg_p (regno))
	    bitmap_set_bit (components, regno);

	  offset += fp_reg_size;
	}
    }

  /* Optimize LR save and restore if we can.  This is component 0.  Any
     out-of-line register save/restore routines need LR.  */
  if (info->lr_save_p
      && !(flag_pic && (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_DARWIN))
      && (info->savres_strategy & SAVE_INLINE_GPRS)
      && (info->savres_strategy & REST_INLINE_GPRS)
      && (info->savres_strategy & SAVE_INLINE_FPRS)
      && (info->savres_strategy & REST_INLINE_FPRS)
      && (info->savres_strategy & SAVE_INLINE_VRS)
      && (info->savres_strategy & REST_INLINE_VRS))
    {
      int offset = info->lr_save_offset;
      if (info->push_p)
	offset += info->total_size;
      if (IN_RANGE (offset, -0x8000, 0x7fff))
	bitmap_set_bit (components, 0);
    }

  /* Optimize saving the TOC.  This is component 2.  */
  if (cfun->machine->save_toc_in_prologue)
    bitmap_set_bit (components, 2);

  return components;
}

/* Implement TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB.  */
sbitmap
rs6000_components_for_bb (basic_block bb)
{
  rs6000_stack_t *info = rs6000_stack_info ();

  bitmap in = DF_LIVE_IN (bb);
  bitmap gen = &DF_LIVE_BB_INFO (bb)->gen;
  bitmap kill = &DF_LIVE_BB_INFO (bb)->kill;

  sbitmap components = sbitmap_alloc (cfun->machine->n_components);
  bitmap_clear (components);

  /* A register is used in a bb if it is in the IN, GEN, or KILL sets.  */

  /* GPRs.  */
  for (unsigned regno = info->first_gp_reg_save; regno < 32; regno++)
    if (bitmap_bit_p (in, regno)
	|| bitmap_bit_p (gen, regno)
	|| bitmap_bit_p (kill, regno))
      bitmap_set_bit (components, regno);

  /* FPRs.  */
  for (unsigned regno = info->first_fp_reg_save; regno < 64; regno++)
    if (bitmap_bit_p (in, regno)
	|| bitmap_bit_p (gen, regno)
	|| bitmap_bit_p (kill, regno))
      bitmap_set_bit (components, regno);

  /* The link register.  */
  if (bitmap_bit_p (in, LR_REGNO)
      || bitmap_bit_p (gen, LR_REGNO)
      || bitmap_bit_p (kill, LR_REGNO))
    bitmap_set_bit (components, 0);

  /* The TOC save.  */
  if (bitmap_bit_p (in, TOC_REGNUM)
      || bitmap_bit_p (gen, TOC_REGNUM)
      || bitmap_bit_p (kill, TOC_REGNUM))
    bitmap_set_bit (components, 2);

  return components;
}

/* Implement TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS.  */
void
rs6000_disqualify_components (sbitmap components, edge e,
			      sbitmap edge_components, bool /*is_prologue*/)
{
  /* Our LR pro/epilogue code moves LR via R0, so R0 had better not be
     live where we want to place that code.  */
  if (bitmap_bit_p (edge_components, 0)
      && bitmap_bit_p (DF_LIVE_IN (e->dest), 0))
    {
      if (dump_file)
	fprintf (dump_file, "Disqualifying LR because GPR0 is live "
		 "on entry to bb %d\n", e->dest->index);
      bitmap_clear_bit (components, 0);
    }
}

/* Implement TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS.  */
void
rs6000_emit_prologue_components (sbitmap components)
{
  rs6000_stack_t *info = rs6000_stack_info ();
  rtx ptr_reg = gen_rtx_REG (Pmode, frame_pointer_needed_indeed
				      ? HARD_FRAME_POINTER_REGNUM
				      : STACK_POINTER_REGNUM);

  machine_mode reg_mode = Pmode;
  int reg_size = TARGET_32BIT ? 4 : 8;
  machine_mode fp_reg_mode = TARGET_HARD_FLOAT ? DFmode : SFmode;
  int fp_reg_size = 8;

  /* Prologue for LR.  */
  if (bitmap_bit_p (components, 0))
    {
      rtx lr = gen_rtx_REG (reg_mode, LR_REGNO);
      rtx reg = gen_rtx_REG (reg_mode, 0);
      rtx_insn *insn = emit_move_insn (reg, lr);
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_REGISTER, gen_rtx_SET (reg, lr));

      int offset = info->lr_save_offset;
      if (info->push_p)
	offset += info->total_size;

      insn = emit_insn (gen_frame_store (reg, ptr_reg, offset));
      RTX_FRAME_RELATED_P (insn) = 1;
      rtx mem = copy_rtx (SET_DEST (single_set (insn)));
      add_reg_note (insn, REG_CFA_OFFSET, gen_rtx_SET (mem, lr));
    }

  /* Prologue for TOC.  */
  if (bitmap_bit_p (components, 2))
    {
      rtx reg = gen_rtx_REG (reg_mode, TOC_REGNUM);
      rtx sp_reg = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
      emit_insn (gen_frame_store (reg, sp_reg, RS6000_TOC_SAVE_SLOT));
    }

  /* Prologue for the GPRs.  */
  int offset = info->gp_save_offset;
  if (info->push_p)
    offset += info->total_size;

  for (int i = info->first_gp_reg_save; i < 32; i++)
    {
      if (bitmap_bit_p (components, i))
	{
	  rtx reg = gen_rtx_REG (reg_mode, i);
	  rtx_insn *insn = emit_insn (gen_frame_store (reg, ptr_reg, offset));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  rtx set = copy_rtx (single_set (insn));
	  add_reg_note (insn, REG_CFA_OFFSET, set);
	}

      offset += reg_size;
    }

  /* Prologue for the FPRs.  */
  offset = info->fp_save_offset;
  if (info->push_p)
    offset += info->total_size;

  for (int i = info->first_fp_reg_save; i < 64; i++)
    {
      if (bitmap_bit_p (components, i))
	{
	  rtx reg = gen_rtx_REG (fp_reg_mode, i);
	  rtx_insn *insn = emit_insn (gen_frame_store (reg, ptr_reg, offset));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  rtx set = copy_rtx (single_set (insn));
	  add_reg_note (insn, REG_CFA_OFFSET, set);
	}

      offset += fp_reg_size;
    }
}

/* Implement TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS.  */
void
rs6000_emit_epilogue_components (sbitmap components)
{
  rs6000_stack_t *info = rs6000_stack_info ();
  rtx ptr_reg = gen_rtx_REG (Pmode, frame_pointer_needed_indeed
				      ? HARD_FRAME_POINTER_REGNUM
				      : STACK_POINTER_REGNUM);

  machine_mode reg_mode = Pmode;
  int reg_size = TARGET_32BIT ? 4 : 8;

  machine_mode fp_reg_mode = TARGET_HARD_FLOAT ? DFmode : SFmode;
  int fp_reg_size = 8;

  /* Epilogue for the FPRs.  */
  int offset = info->fp_save_offset;
  if (info->push_p)
    offset += info->total_size;

  for (int i = info->first_fp_reg_save; i < 64; i++)
    {
      if (bitmap_bit_p (components, i))
	{
	  rtx reg = gen_rtx_REG (fp_reg_mode, i);
	  rtx_insn *insn = emit_insn (gen_frame_load (reg, ptr_reg, offset));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  add_reg_note (insn, REG_CFA_RESTORE, reg);
	}

      offset += fp_reg_size;
    }

  /* Epilogue for the GPRs.  */
  offset = info->gp_save_offset;
  if (info->push_p)
    offset += info->total_size;

  for (int i = info->first_gp_reg_save; i < 32; i++)
    {
      if (bitmap_bit_p (components, i))
	{
	  rtx reg = gen_rtx_REG (reg_mode, i);
	  rtx_insn *insn = emit_insn (gen_frame_load (reg, ptr_reg, offset));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  add_reg_note (insn, REG_CFA_RESTORE, reg);
	}

      offset += reg_size;
    }

  /* Epilogue for LR.  */
  if (bitmap_bit_p (components, 0))
    {
      int offset = info->lr_save_offset;
      if (info->push_p)
	offset += info->total_size;

      rtx reg = gen_rtx_REG (reg_mode, 0);
      rtx_insn *insn = emit_insn (gen_frame_load (reg, ptr_reg, offset));

      rtx lr = gen_rtx_REG (Pmode, LR_REGNO);
      insn = emit_move_insn (lr, reg);
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_RESTORE, lr);
    }
}

/* Implement TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS.  */
void
rs6000_set_handled_components (sbitmap components)
{
  rs6000_stack_t *info = rs6000_stack_info ();

  for (int i = info->first_gp_reg_save; i < 32; i++)
    if (bitmap_bit_p (components, i))
      cfun->machine->gpr_is_wrapped_separately[i] = true;

  for (int i = info->first_fp_reg_save; i < 64; i++)
    if (bitmap_bit_p (components, i))
      cfun->machine->fpr_is_wrapped_separately[i - 32] = true;

  if (bitmap_bit_p (components, 0))
    cfun->machine->lr_is_wrapped_separately = true;

  if (bitmap_bit_p (components, 2))
    cfun->machine->toc_is_wrapped_separately = true;
}

/* VRSAVE is a bit vector representing which AltiVec registers
   are used.  The OS uses this to determine which vector
   registers to save on a context switch.  We need to save
   VRSAVE on the stack frame, add whatever AltiVec registers we
   used in this function, and do the corresponding magic in the
   epilogue.  */
static void
emit_vrsave_prologue (rs6000_stack_t *info, int save_regno,
		      HOST_WIDE_INT frame_off, rtx frame_reg_rtx)
{
  /* Get VRSAVE into a GPR.  */
  rtx reg = gen_rtx_REG (SImode, save_regno);
  rtx vrsave = gen_rtx_REG (SImode, VRSAVE_REGNO);
  if (TARGET_MACHO)
    emit_insn (gen_get_vrsave_internal (reg));
  else
    emit_insn (gen_rtx_SET (reg, vrsave));

  /* Save VRSAVE.  */
  int offset = info->vrsave_save_offset + frame_off;
  emit_insn (gen_frame_store (reg, frame_reg_rtx, offset));

  /* Include the registers in the mask.  */
  emit_insn (gen_iorsi3 (reg, reg, GEN_INT (info->vrsave_mask)));

  emit_insn (generate_set_vrsave (reg, info, 0));
}

/* Set up the arg pointer (r12) for -fsplit-stack code.  If __morestack was
   called, it left the arg pointer to the old stack in r29.  Otherwise, the
   arg pointer is the top of the current frame.  */
static void
emit_split_stack_prologue (rs6000_stack_t *info, rtx_insn *sp_adjust,
			   HOST_WIDE_INT frame_off, rtx frame_reg_rtx)
{
  cfun->machine->split_stack_argp_used = true;

  if (sp_adjust)
    {
      rtx r12 = gen_rtx_REG (Pmode, 12);
      rtx sp_reg_rtx = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
      rtx set_r12 = gen_rtx_SET (r12, sp_reg_rtx);
      emit_insn_before (set_r12, sp_adjust);
    }
  else if (frame_off != 0 || REGNO (frame_reg_rtx) != 12)
    {
      rtx r12 = gen_rtx_REG (Pmode, 12);
      if (frame_off == 0)
	emit_move_insn (r12, frame_reg_rtx);
      else
	emit_insn (gen_add3_insn (r12, frame_reg_rtx, GEN_INT (frame_off)));
    }

  if (info->push_p)
    {
      rtx r12 = gen_rtx_REG (Pmode, 12);
      rtx r29 = gen_rtx_REG (Pmode, 29);
      rtx cr7 = gen_rtx_REG (CCUNSmode, CR7_REGNO);
      rtx not_more = gen_label_rtx ();
      rtx jump;

      jump = gen_rtx_IF_THEN_ELSE (VOIDmode,
				   gen_rtx_GEU (VOIDmode, cr7, const0_rtx),
				   gen_rtx_LABEL_REF (VOIDmode, not_more),
				   pc_rtx);
      jump = emit_jump_insn (gen_rtx_SET (pc_rtx, jump));
      JUMP_LABEL (jump) = not_more;
      LABEL_NUSES (not_more) += 1;
      emit_move_insn (r12, r29);
      emit_label (not_more);
    }
}

/* Emit function prologue as insns.  */

void
rs6000_emit_prologue (void)
{
  rs6000_stack_t *info = rs6000_stack_info ();
  machine_mode reg_mode = Pmode;
  int reg_size = TARGET_32BIT ? 4 : 8;
  machine_mode fp_reg_mode = TARGET_HARD_FLOAT ? DFmode : SFmode;
  int fp_reg_size = 8;
  rtx sp_reg_rtx = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx frame_reg_rtx = sp_reg_rtx;
  unsigned int cr_save_regno;
  rtx cr_save_rtx = NULL_RTX;
  rtx_insn *insn;
  int strategy;
  int using_static_chain_p
    = (cfun->static_chain_decl != NULL_TREE
       && df_regs_ever_live_p (STATIC_CHAIN_REGNUM)
       && call_used_or_fixed_reg_p (STATIC_CHAIN_REGNUM));
  int using_split_stack = (flag_split_stack
                           && (lookup_attribute ("no_split_stack",
                                                 DECL_ATTRIBUTES (cfun->decl))
                               == NULL));

  frame_pointer_needed_indeed
    = frame_pointer_needed && df_regs_ever_live_p (HARD_FRAME_POINTER_REGNUM);

  /* Offset to top of frame for frame_reg and sp respectively.  */
  HOST_WIDE_INT frame_off = 0;
  HOST_WIDE_INT sp_off = 0;
  /* sp_adjust is the stack adjusting instruction, tracked so that the
     insn setting up the split-stack arg pointer can be emitted just
     prior to it, when r12 is not used here for other purposes.  */
  rtx_insn *sp_adjust = 0;

#if CHECKING_P
  /* Track and check usage of r0, r11, r12.  */
  int reg_inuse = using_static_chain_p ? 1 << 11 : 0;
#define START_USE(R) do \
  {						\
    gcc_assert ((reg_inuse & (1 << (R))) == 0);	\
    reg_inuse |= 1 << (R);			\
  } while (0)
#define END_USE(R) do \
  {						\
    gcc_assert ((reg_inuse & (1 << (R))) != 0);	\
    reg_inuse &= ~(1 << (R));			\
  } while (0)
#define NOT_INUSE(R) do \
  {						\
    gcc_assert ((reg_inuse & (1 << (R))) == 0);	\
  } while (0)
#else
#define START_USE(R) do {} while (0)
#define END_USE(R) do {} while (0)
#define NOT_INUSE(R) do {} while (0)
#endif

  if (DEFAULT_ABI == ABI_ELFv2
      && !TARGET_SINGLE_PIC_BASE)
    {
      cfun->machine->r2_setup_needed = df_regs_ever_live_p (TOC_REGNUM);

      /* With -mminimal-toc we may generate an extra use of r2 below.  */
      if (TARGET_TOC && TARGET_MINIMAL_TOC
	  && !constant_pool_empty_p ())
	cfun->machine->r2_setup_needed = true;
    }


  if (flag_stack_usage_info)
    current_function_static_stack_size = info->total_size;

  if (flag_stack_check == STATIC_BUILTIN_STACK_CHECK)
    {
      HOST_WIDE_INT size = info->total_size;

      if (crtl->is_leaf && !cfun->calls_alloca)
	{
	  if (size > PROBE_INTERVAL && size > get_stack_check_protect ())
	    rs6000_emit_probe_stack_range (get_stack_check_protect (),
					   size - get_stack_check_protect ());
	}
      else if (size > 0)
	rs6000_emit_probe_stack_range (get_stack_check_protect (), size);
    }

  if (TARGET_FIX_AND_CONTINUE)
    {
      /* gdb on darwin arranges to forward a function from the old
	 address by modifying the first 5 instructions of the function
	 to branch to the overriding function.  This is necessary to
	 permit function pointers that point to the old function to
	 actually forward to the new function.  */
      emit_insn (gen_nop ());
      emit_insn (gen_nop ());
      emit_insn (gen_nop ());
      emit_insn (gen_nop ());
      emit_insn (gen_nop ());
    }

  /* Handle world saves specially here.  */
  if (WORLD_SAVE_P (info))
    {
      int i, j, sz;
      rtx treg;
      rtvec p;
      rtx reg0;

      /* save_world expects lr in r0. */
      reg0 = gen_rtx_REG (Pmode, 0);
      if (info->lr_save_p)
	{
	  insn = emit_move_insn (reg0,
				 gen_rtx_REG (Pmode, LR_REGNO));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* The SAVE_WORLD and RESTORE_WORLD routines make a number of
	 assumptions about the offsets of various bits of the stack
	 frame.  */
      gcc_assert (info->gp_save_offset == -220
		  && info->fp_save_offset == -144
		  && info->lr_save_offset == 8
		  && info->cr_save_offset == 4
		  && info->push_p
		  && info->lr_save_p
		  && (!crtl->calls_eh_return
		      || info->ehrd_offset == -432)
		  && info->vrsave_save_offset == -224
		  && info->altivec_save_offset == -416);

      treg = gen_rtx_REG (SImode, 11);
      emit_move_insn (treg, GEN_INT (-info->total_size));

      /* SAVE_WORLD takes the caller's LR in R0 and the frame size
	 in R11.  It also clobbers R12, so beware!  */

      /* Preserve CR2 for save_world prologues */
      sz = 5;
      sz += 32 - info->first_gp_reg_save;
      sz += 64 - info->first_fp_reg_save;
      sz += LAST_ALTIVEC_REGNO - info->first_altivec_reg_save + 1;
      p = rtvec_alloc (sz);
      j = 0;
      RTVEC_ELT (p, j++) = gen_hard_reg_clobber (SImode, LR_REGNO);
      RTVEC_ELT (p, j++) = gen_rtx_USE (VOIDmode,
					gen_rtx_SYMBOL_REF (Pmode,
							    "*save_world"));
      /* We do floats first so that the instruction pattern matches
	 properly.  */
      for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	RTVEC_ELT (p, j++)
	  = gen_frame_store (gen_rtx_REG (TARGET_HARD_FLOAT ? DFmode : SFmode,
					  info->first_fp_reg_save + i),
			     frame_reg_rtx,
			     info->fp_save_offset + frame_off + 8 * i);
      for (i = 0; info->first_altivec_reg_save + i <= LAST_ALTIVEC_REGNO; i++)
	RTVEC_ELT (p, j++)
	  = gen_frame_store (gen_rtx_REG (V4SImode,
					  info->first_altivec_reg_save + i),
			     frame_reg_rtx,
			     info->altivec_save_offset + frame_off + 16 * i);
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	RTVEC_ELT (p, j++)
	  = gen_frame_store (gen_rtx_REG (reg_mode, info->first_gp_reg_save + i),
			     frame_reg_rtx,
			     info->gp_save_offset + frame_off + reg_size * i);

      /* CR register traditionally saved as CR2.  */
      RTVEC_ELT (p, j++)
	= gen_frame_store (gen_rtx_REG (SImode, CR2_REGNO),
			   frame_reg_rtx, info->cr_save_offset + frame_off);
      /* Explain about use of R0.  */
      if (info->lr_save_p)
	RTVEC_ELT (p, j++)
	  = gen_frame_store (reg0,
			     frame_reg_rtx, info->lr_save_offset + frame_off);
      /* Explain what happens to the stack pointer.  */
      {
	rtx newval = gen_rtx_PLUS (Pmode, sp_reg_rtx, treg);
	RTVEC_ELT (p, j++) = gen_rtx_SET (sp_reg_rtx, newval);
      }

      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
      rs6000_frame_related (insn, frame_reg_rtx, sp_off - frame_off,
			    treg, GEN_INT (-info->total_size));
      sp_off = frame_off = info->total_size;
    }

  strategy = info->savres_strategy;

  /* For V.4, update stack before we do any saving and set back pointer.  */
  if (! WORLD_SAVE_P (info)
      && info->push_p
      && (DEFAULT_ABI == ABI_V4
	  || crtl->calls_eh_return))
    {
      bool need_r11 = (!(strategy & SAVE_INLINE_FPRS)
		       || !(strategy & SAVE_INLINE_GPRS)
		       || !(strategy & SAVE_INLINE_VRS));
      int ptr_regno = -1;
      rtx ptr_reg = NULL_RTX;
      int ptr_off = 0;

      if (info->total_size < 32767)
	frame_off = info->total_size;
      else if (need_r11)
	ptr_regno = 11;
      else if (info->cr_save_p
	       || info->lr_save_p
	       || info->first_fp_reg_save < 64
	       || info->first_gp_reg_save < 32
	       || info->altivec_size != 0
	       || info->vrsave_size != 0
	       || crtl->calls_eh_return)
	ptr_regno = 12;
      else
	{
	  /* The prologue won't be saving any regs so there is no need
	     to set up a frame register to access any frame save area.
	     We also won't be using frame_off anywhere below, but set
	     the correct value anyway to protect against future
	     changes to this function.  */
	  frame_off = info->total_size;
	}
      if (ptr_regno != -1)
	{
	  /* Set up the frame offset to that needed by the first
	     out-of-line save function.  */
	  START_USE (ptr_regno);
	  ptr_reg = gen_rtx_REG (Pmode, ptr_regno);
	  frame_reg_rtx = ptr_reg;
	  if (!(strategy & SAVE_INLINE_FPRS) && info->fp_size != 0)
	    gcc_checking_assert (info->fp_save_offset + info->fp_size == 0);
	  else if (!(strategy & SAVE_INLINE_GPRS) && info->first_gp_reg_save < 32)
	    ptr_off = info->gp_save_offset + info->gp_size;
	  else if (!(strategy & SAVE_INLINE_VRS) && info->altivec_size != 0)
	    ptr_off = info->altivec_save_offset + info->altivec_size;
	  frame_off = -ptr_off;
	}
      sp_adjust = rs6000_emit_allocate_stack (info->total_size,
					      ptr_reg, ptr_off);
      if (REGNO (frame_reg_rtx) == 12)
	sp_adjust = 0;
      sp_off = info->total_size;
      if (frame_reg_rtx != sp_reg_rtx)
	rs6000_emit_stack_tie (frame_reg_rtx, false);
    }

  /* If we use the link register, get it into r0.  */
  if (!WORLD_SAVE_P (info) && info->lr_save_p
      && !cfun->machine->lr_is_wrapped_separately)
    {
      rtx reg;

      reg = gen_rtx_REG (Pmode, 0);
      START_USE (0);
      insn = emit_move_insn (reg, gen_rtx_REG (Pmode, LR_REGNO));
      RTX_FRAME_RELATED_P (insn) = 1;

      if (!(strategy & (SAVE_NOINLINE_GPRS_SAVES_LR
			| SAVE_NOINLINE_FPRS_SAVES_LR)))
	{
	  insn = emit_insn (gen_frame_store (reg, frame_reg_rtx,
					     info->lr_save_offset + frame_off));
	  rs6000_frame_related (insn, frame_reg_rtx, sp_off - frame_off,
				NULL_RTX, NULL_RTX);
	  END_USE (0);
	}
    }

  /* The ROP hash store must occur before a stack frame is created,
     since the hash operates on r1.  */
  /* NOTE: The hashst isn't needed if we're going to do a sibcall,
     but there's no way to know that here.  Harmless except for
     performance, of course.  */
  if (info->rop_hash_size)
    {
      rtx stack_ptr = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
      rtx addr = gen_rtx_PLUS (Pmode, stack_ptr,
			       GEN_INT (info->rop_hash_save_offset));
      rtx mem = gen_rtx_MEM (Pmode, addr);
      rtx reg0 = gen_rtx_REG (Pmode, 0);
      emit_insn (gen_hashst (mem, reg0));
    }

  /* If we need to save CR, put it into r12 or r11.  Choose r12 except when
     r12 will be needed by out-of-line gpr save.  */
  if (DEFAULT_ABI == ABI_AIX
      && !(strategy & (SAVE_INLINE_GPRS | SAVE_NOINLINE_GPRS_SAVES_LR)))
    cr_save_regno = 11;
  else if (DEFAULT_ABI == ABI_ELFv2)
    cr_save_regno = 11;
  else
    cr_save_regno = 12;
  if (!WORLD_SAVE_P (info)
      && info->cr_save_p
      && REGNO (frame_reg_rtx) != cr_save_regno
      && !(using_static_chain_p && cr_save_regno == 11)
      && !(using_split_stack && cr_save_regno == 12 && sp_adjust))
    {
      cr_save_rtx = gen_rtx_REG (SImode, cr_save_regno);
      START_USE (cr_save_regno);
      rs6000_emit_prologue_move_from_cr (cr_save_rtx);
    }

  /* Do any required saving of fpr's.  If only one or two to save, do
     it ourselves.  Otherwise, call function.  */
  if (!WORLD_SAVE_P (info) && (strategy & SAVE_INLINE_FPRS))
    {
      int offset = info->fp_save_offset + frame_off;
      for (int i = info->first_fp_reg_save; i < 64; i++)
	{
	  if (save_reg_p (i)
	      && !cfun->machine->fpr_is_wrapped_separately[i - 32])
	    emit_frame_save (frame_reg_rtx, fp_reg_mode, i, offset,
			     sp_off - frame_off);

	  offset += fp_reg_size;
	}
    }
  else if (!WORLD_SAVE_P (info) && info->first_fp_reg_save != 64)
    {
      bool lr = (strategy & SAVE_NOINLINE_FPRS_SAVES_LR) != 0;
      int sel = SAVRES_SAVE | SAVRES_FPR | (lr ? SAVRES_LR : 0);
      unsigned ptr_regno = ptr_regno_for_savres (sel);
      rtx ptr_reg = frame_reg_rtx;

      if (REGNO (frame_reg_rtx) == ptr_regno)
	gcc_checking_assert (frame_off == 0);
      else
	{
	  ptr_reg = gen_rtx_REG (Pmode, ptr_regno);
	  NOT_INUSE (ptr_regno);
	  emit_insn (gen_add3_insn (ptr_reg,
				    frame_reg_rtx, GEN_INT (frame_off)));
	}
      insn = rs6000_emit_savres_rtx (info, ptr_reg,
				     info->fp_save_offset,
				     info->lr_save_offset,
				     DFmode, sel);
      rs6000_frame_related (insn, ptr_reg, sp_off,
			    NULL_RTX, NULL_RTX);
      if (lr)
	END_USE (0);
    }

  /* Save GPRs.  This is done as a PARALLEL if we are using
     the store-multiple instructions.  */
  if (!WORLD_SAVE_P (info) && !(strategy & SAVE_INLINE_GPRS))
    {
      bool lr = (strategy & SAVE_NOINLINE_GPRS_SAVES_LR) != 0;
      int sel = SAVRES_SAVE | SAVRES_GPR | (lr ? SAVRES_LR : 0);
      unsigned ptr_regno = ptr_regno_for_savres (sel);
      rtx ptr_reg = frame_reg_rtx;
      bool ptr_set_up = REGNO (ptr_reg) == ptr_regno;
      int end_save = info->gp_save_offset + info->gp_size;
      int ptr_off;

      if (ptr_regno == 12)
	sp_adjust = 0;
      if (!ptr_set_up)
	ptr_reg = gen_rtx_REG (Pmode, ptr_regno);

      /* Need to adjust r11 (r12) if we saved any FPRs.  */
      if (end_save + frame_off != 0)
	{
	  rtx offset = GEN_INT (end_save + frame_off);

	  if (ptr_set_up)
	    frame_off = -end_save;
	  else
	    NOT_INUSE (ptr_regno);
	  emit_insn (gen_add3_insn (ptr_reg, frame_reg_rtx, offset));
	}
      else if (!ptr_set_up)
	{
	  NOT_INUSE (ptr_regno);
	  emit_move_insn (ptr_reg, frame_reg_rtx);
	}
      ptr_off = -end_save;
      insn = rs6000_emit_savres_rtx (info, ptr_reg,
				     info->gp_save_offset + ptr_off,
				     info->lr_save_offset + ptr_off,
				     reg_mode, sel);
      rs6000_frame_related (insn, ptr_reg, sp_off - ptr_off,
			    NULL_RTX, NULL_RTX);
      if (lr)
	END_USE (0);
    }
  else if (!WORLD_SAVE_P (info) && (strategy & SAVE_MULTIPLE))
    {
      rtvec p;
      int i;
      p = rtvec_alloc (32 - info->first_gp_reg_save);
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	RTVEC_ELT (p, i)
	  = gen_frame_store (gen_rtx_REG (reg_mode, info->first_gp_reg_save + i),
			     frame_reg_rtx,
			     info->gp_save_offset + frame_off + reg_size * i);
      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
      rs6000_frame_related (insn, frame_reg_rtx, sp_off - frame_off,
			    NULL_RTX, NULL_RTX);
    }
  else if (!WORLD_SAVE_P (info))
    {
      int offset = info->gp_save_offset + frame_off;
      for (int i = info->first_gp_reg_save; i < 32; i++)
	{
	  if (save_reg_p (i)
	      && !cfun->machine->gpr_is_wrapped_separately[i])
	    emit_frame_save (frame_reg_rtx, reg_mode, i, offset,
			     sp_off - frame_off);

	  offset += reg_size;
	}
    }

  if (crtl->calls_eh_return)
    {
      unsigned int i;
      rtvec p;

      for (i = 0; ; ++i)
	{
	  unsigned int regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;
	}

      p = rtvec_alloc (i);

      for (i = 0; ; ++i)
	{
	  unsigned int regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;

	  rtx set
	    = gen_frame_store (gen_rtx_REG (reg_mode, regno),
			       sp_reg_rtx,
			       info->ehrd_offset + sp_off + reg_size * (int) i);
	  RTVEC_ELT (p, i) = set;
	  RTX_FRAME_RELATED_P (set) = 1;
	}

      insn = emit_insn (gen_blockage ());
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_FRAME_RELATED_EXPR, gen_rtx_PARALLEL (VOIDmode, p));
    }

  /* In AIX ABI we need to make sure r2 is really saved.  */
  if (TARGET_AIX && crtl->calls_eh_return)
    {
      rtx tmp_reg, tmp_reg_si, hi, lo, compare_result, toc_save_done, jump;
      rtx join_insn, note;
      rtx_insn *save_insn;
      long toc_restore_insn;

      tmp_reg = gen_rtx_REG (Pmode, 11);
      tmp_reg_si = gen_rtx_REG (SImode, 11);
      if (using_static_chain_p)
	{
	  START_USE (0);
	  emit_move_insn (gen_rtx_REG (Pmode, 0), tmp_reg);
	}
      else
	START_USE (11);
      emit_move_insn (tmp_reg, gen_rtx_REG (Pmode, LR_REGNO));
      /* Peek at instruction to which this function returns.  If it's
	 restoring r2, then we know we've already saved r2.  We can't
	 unconditionally save r2 because the value we have will already
	 be updated if we arrived at this function via a plt call or
	 toc adjusting stub.  */
      emit_move_insn (tmp_reg_si, gen_rtx_MEM (SImode, tmp_reg));
      toc_restore_insn = ((TARGET_32BIT ? 0x80410000 : 0xE8410000)
			  + RS6000_TOC_SAVE_SLOT);
      hi = gen_int_mode (toc_restore_insn & ~0xffff, SImode);
      emit_insn (gen_xorsi3 (tmp_reg_si, tmp_reg_si, hi));
      compare_result = gen_rtx_REG (CCUNSmode, CR0_REGNO);
      validate_condition_mode (EQ, CCUNSmode);
      lo = gen_int_mode (toc_restore_insn & 0xffff, SImode);
      emit_insn (gen_rtx_SET (compare_result,
			      gen_rtx_COMPARE (CCUNSmode, tmp_reg_si, lo)));
      toc_save_done = gen_label_rtx ();
      jump = gen_rtx_IF_THEN_ELSE (VOIDmode,
				   gen_rtx_EQ (VOIDmode, compare_result,
					       const0_rtx),
				   gen_rtx_LABEL_REF (VOIDmode, toc_save_done),
				   pc_rtx);
      jump = emit_jump_insn (gen_rtx_SET (pc_rtx, jump));
      JUMP_LABEL (jump) = toc_save_done;
      LABEL_NUSES (toc_save_done) += 1;

      save_insn = emit_frame_save (frame_reg_rtx, reg_mode,
				   TOC_REGNUM, frame_off + RS6000_TOC_SAVE_SLOT,
				   sp_off - frame_off);

      emit_label (toc_save_done);

      /* ??? If we leave SAVE_INSN as marked as saving R2, then we'll
	 have a CFG that has different saves along different paths.
	 Move the note to a dummy blockage insn, which describes that
	 R2 is unconditionally saved after the label.  */
      /* ??? An alternate representation might be a special insn pattern
	 containing both the branch and the store.  That might let the
	 code that minimizes the number of DW_CFA_advance opcodes better
	 freedom in placing the annotations.  */
      note = find_reg_note (save_insn, REG_FRAME_RELATED_EXPR, NULL);
      if (note)
	remove_note (save_insn, note);
      else
	note = alloc_reg_note (REG_FRAME_RELATED_EXPR,
			       copy_rtx (PATTERN (save_insn)), NULL_RTX);
      RTX_FRAME_RELATED_P (save_insn) = 0;

      join_insn = emit_insn (gen_blockage ());
      REG_NOTES (join_insn) = note;
      RTX_FRAME_RELATED_P (join_insn) = 1;

      if (using_static_chain_p)
	{
	  emit_move_insn (tmp_reg, gen_rtx_REG (Pmode, 0));
	  END_USE (0);
	}
      else
	END_USE (11);
    }

  /* Save CR if we use any that must be preserved.  */
  if (!WORLD_SAVE_P (info) && info->cr_save_p)
    {
      rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->cr_save_offset + frame_off));
      rtx mem = gen_frame_mem (SImode, addr);

      /* If we didn't copy cr before, do so now using r0.  */
      if (cr_save_rtx == NULL_RTX)
	{
	  START_USE (0);
	  cr_save_rtx = gen_rtx_REG (SImode, 0);
	  rs6000_emit_prologue_move_from_cr (cr_save_rtx);
	}

      /* Saving CR requires a two-instruction sequence: one instruction
	 to move the CR to a general-purpose register, and a second
	 instruction that stores the GPR to memory.

	 We do not emit any DWARF CFI records for the first of these,
	 because we cannot properly represent the fact that CR is saved in
	 a register.  One reason is that we cannot express that multiple
	 CR fields are saved; another reason is that on 64-bit, the size
	 of the CR register in DWARF (4 bytes) differs from the size of
	 a general-purpose register.

	 This means if any intervening instruction were to clobber one of
	 the call-saved CR fields, we'd have incorrect CFI.  To prevent
	 this from happening, we mark the store to memory as a use of
	 those CR fields, which prevents any such instruction from being
	 scheduled in between the two instructions.  */
      rtx crsave_v[9];
      int n_crsave = 0;
      int i;

      crsave_v[n_crsave++] = gen_rtx_SET (mem, cr_save_rtx);
      for (i = 0; i < 8; i++)
	if (save_reg_p (CR0_REGNO + i))
	  crsave_v[n_crsave++]
	    = gen_rtx_USE (VOIDmode, gen_rtx_REG (CCmode, CR0_REGNO + i));

      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode,
					  gen_rtvec_v (n_crsave, crsave_v)));
      END_USE (REGNO (cr_save_rtx));

      /* Now, there's no way that dwarf2out_frame_debug_expr is going to
	 understand '(unspec:SI [(reg:CC 68) ...] UNSPEC_MOVESI_FROM_CR)',
	 so we need to construct a frame expression manually.  */
      RTX_FRAME_RELATED_P (insn) = 1;

      /* Update address to be stack-pointer relative, like
	 rs6000_frame_related would do.  */
      addr = gen_rtx_PLUS (Pmode, gen_rtx_REG (Pmode, STACK_POINTER_REGNUM),
			   GEN_INT (info->cr_save_offset + sp_off));
      mem = gen_frame_mem (SImode, addr);

      if (DEFAULT_ABI == ABI_ELFv2)
	{
	  /* In the ELFv2 ABI we generate separate CFI records for each
	     CR field that was actually saved.  They all point to the
	     same 32-bit stack slot.  */
	  rtx crframe[8];
	  int n_crframe = 0;

	  for (i = 0; i < 8; i++)
	    if (save_reg_p (CR0_REGNO + i))
	      {
		crframe[n_crframe]
		  = gen_rtx_SET (mem, gen_rtx_REG (SImode, CR0_REGNO + i));

		RTX_FRAME_RELATED_P (crframe[n_crframe]) = 1;
		n_crframe++;
	     }

	  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			gen_rtx_PARALLEL (VOIDmode,
					  gen_rtvec_v (n_crframe, crframe)));
	}
      else
	{
	  /* In other ABIs, by convention, we use a single CR regnum to
	     represent the fact that all call-saved CR fields are saved.
	     We use CR2_REGNO to be compatible with gcc-2.95 on Linux.  */
	  rtx set = gen_rtx_SET (mem, gen_rtx_REG (SImode, CR2_REGNO));
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR, set);
	}
    }

  /* In the ELFv2 ABI we need to save all call-saved CR fields into
     *separate* slots if the routine calls __builtin_eh_return, so
     that they can be independently restored by the unwinder.  */
  if (DEFAULT_ABI == ABI_ELFv2 && crtl->calls_eh_return)
    {
      int i, cr_off = info->ehcr_offset;
      rtx crsave;

      /* ??? We might get better performance by using multiple mfocrf
	 instructions.  */
      crsave = gen_rtx_REG (SImode, 0);
      emit_insn (gen_prologue_movesi_from_cr (crsave));

      for (i = 0; i < 8; i++)
	if (!call_used_or_fixed_reg_p (CR0_REGNO + i))
	  {
	    rtvec p = rtvec_alloc (2);
	    RTVEC_ELT (p, 0)
	      = gen_frame_store (crsave, frame_reg_rtx, cr_off + frame_off);
	    RTVEC_ELT (p, 1)
	      = gen_rtx_USE (VOIDmode, gen_rtx_REG (CCmode, CR0_REGNO + i));

	    insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));

	    RTX_FRAME_RELATED_P (insn) = 1;
	    add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			  gen_frame_store (gen_rtx_REG (SImode, CR0_REGNO + i),
					   sp_reg_rtx, cr_off + sp_off));

	    cr_off += reg_size;
	  }
    }

  /* If we are emitting stack probes, but allocate no stack, then
     just note that in the dump file.  */
  if (flag_stack_clash_protection
      && dump_file
      && !info->push_p)
    dump_stack_clash_frame_info (NO_PROBE_NO_FRAME, false);

  /* Update stack and set back pointer unless this is V.4,
     for which it was done previously.  */
  if (!WORLD_SAVE_P (info) && info->push_p
      && !(DEFAULT_ABI == ABI_V4 || crtl->calls_eh_return))
    {
      rtx ptr_reg = NULL;
      int ptr_off = 0;

      /* If saving altivec regs we need to be able to address all save
	 locations using a 16-bit offset.  */
      if ((strategy & SAVE_INLINE_VRS) == 0
	  || (info->altivec_size != 0
	      && (info->altivec_save_offset + info->altivec_size - 16
		  + info->total_size - frame_off) > 32767)
	  || (info->vrsave_size != 0
	      && (info->vrsave_save_offset
		  + info->total_size - frame_off) > 32767))
	{
	  int sel = SAVRES_SAVE | SAVRES_VR;
	  unsigned ptr_regno = ptr_regno_for_savres (sel);

	  if (using_static_chain_p
	      && ptr_regno == STATIC_CHAIN_REGNUM)
	    ptr_regno = 12;
	  if (REGNO (frame_reg_rtx) != ptr_regno)
	    START_USE (ptr_regno);
	  ptr_reg = gen_rtx_REG (Pmode, ptr_regno);
	  frame_reg_rtx = ptr_reg;
	  ptr_off = info->altivec_save_offset + info->altivec_size;
	  frame_off = -ptr_off;
	}
      else if (REGNO (frame_reg_rtx) == 1)
	frame_off = info->total_size;
      sp_adjust = rs6000_emit_allocate_stack (info->total_size,
					      ptr_reg, ptr_off);
      if (REGNO (frame_reg_rtx) == 12)
	sp_adjust = 0;
      sp_off = info->total_size;
      if (frame_reg_rtx != sp_reg_rtx)
	rs6000_emit_stack_tie (frame_reg_rtx, false);
    }

  /* Set frame pointer, if needed.  */
  if (frame_pointer_needed_indeed)
    {
      insn = emit_move_insn (gen_rtx_REG (Pmode, HARD_FRAME_POINTER_REGNUM),
			     sp_reg_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Save AltiVec registers if needed.  Save here because the red zone does
     not always include AltiVec registers.  */
  if (!WORLD_SAVE_P (info)
      && info->altivec_size != 0 && (strategy & SAVE_INLINE_VRS) == 0)
    {
      int end_save = info->altivec_save_offset + info->altivec_size;
      int ptr_off;
      /* Oddly, the vector save/restore functions point r0 at the end
	 of the save area, then use r11 or r12 to load offsets for
	 [reg+reg] addressing.  */
      rtx ptr_reg = gen_rtx_REG (Pmode, 0);
      int scratch_regno = ptr_regno_for_savres (SAVRES_SAVE | SAVRES_VR);
      rtx scratch_reg = gen_rtx_REG (Pmode, scratch_regno);

      gcc_checking_assert (scratch_regno == 11 || scratch_regno == 12);
      NOT_INUSE (0);
      if (scratch_regno == 12)
	sp_adjust = 0;
      if (end_save + frame_off != 0)
	{
	  rtx offset = GEN_INT (end_save + frame_off);

	  emit_insn (gen_add3_insn (ptr_reg, frame_reg_rtx, offset));
	}
      else
	emit_move_insn (ptr_reg, frame_reg_rtx);

      ptr_off = -end_save;
      insn = rs6000_emit_savres_rtx (info, scratch_reg,
				     info->altivec_save_offset + ptr_off,
				     0, V4SImode, SAVRES_SAVE | SAVRES_VR);
      rs6000_frame_related (insn, scratch_reg, sp_off - ptr_off,
			    NULL_RTX, NULL_RTX);
      if (REGNO (frame_reg_rtx) == REGNO (scratch_reg))
	{
	  /* The oddity mentioned above clobbered our frame reg.  */
	  emit_move_insn (frame_reg_rtx, ptr_reg);
	  frame_off = ptr_off;
	}
    }
  else if (!WORLD_SAVE_P (info)
	   && info->altivec_size != 0)
    {
      int i;

      for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	if (info->vrsave_mask & ALTIVEC_REG_BIT (i))
	  {
	    rtx areg, savereg, mem;
	    HOST_WIDE_INT offset;

	    offset = (info->altivec_save_offset + frame_off
		      + 16 * (i - info->first_altivec_reg_save));

	    savereg = gen_rtx_REG (V4SImode, i);

	    if (TARGET_P9_VECTOR && quad_address_offset_p (offset))
	      {
		mem = gen_frame_mem (V4SImode,
				     gen_rtx_PLUS (Pmode, frame_reg_rtx,
						   GEN_INT (offset)));
		insn = emit_insn (gen_rtx_SET (mem, savereg));
		areg = NULL_RTX;
	      }
	    else
	      {
		NOT_INUSE (0);
		areg = gen_rtx_REG (Pmode, 0);
		emit_move_insn (areg, GEN_INT (offset));

		/* AltiVec addressing mode is [reg+reg].  */
		mem = gen_frame_mem (V4SImode,
				     gen_rtx_PLUS (Pmode, frame_reg_rtx, areg));

		/* Rather than emitting a generic move, force use of the stvx
		   instruction, which we always want on ISA 2.07 (power8) systems.
		   In particular we don't want xxpermdi/stxvd2x for little
		   endian.  */
		insn = emit_insn (gen_altivec_stvx_v4si_internal (mem, savereg));
	      }

	    rs6000_frame_related (insn, frame_reg_rtx, sp_off - frame_off,
				  areg, GEN_INT (offset));
	  }
    }

  /* VRSAVE is a bit vector representing which AltiVec registers
     are used.  The OS uses this to determine which vector
     registers to save on a context switch.  We need to save
     VRSAVE on the stack frame, add whatever AltiVec registers we
     used in this function, and do the corresponding magic in the
     epilogue.  */

  if (!WORLD_SAVE_P (info) && info->vrsave_size != 0)
    {
      /* Get VRSAVE into a GPR.  Note that ABI_V4 and ABI_DARWIN might
	 be using r12 as frame_reg_rtx and r11 as the static chain
	 pointer for nested functions.  */
      int save_regno = 12;
      if ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
	  && !using_static_chain_p)
	save_regno = 11;
      else if (using_split_stack || REGNO (frame_reg_rtx) == 12)
	{
	  save_regno = 11;
	  if (using_static_chain_p)
	    save_regno = 0;
	}
      NOT_INUSE (save_regno);

      emit_vrsave_prologue (info, save_regno, frame_off, frame_reg_rtx);
    }

  /* If we are using RS6000_PIC_OFFSET_TABLE_REGNUM, we need to set it up.  */
  if (!TARGET_SINGLE_PIC_BASE
      && ((TARGET_TOC && TARGET_MINIMAL_TOC
	   && !constant_pool_empty_p ())
	  || (DEFAULT_ABI == ABI_V4
	      && (flag_pic == 1 || (flag_pic && TARGET_SECURE_PLT))
	      && df_regs_ever_live_p (RS6000_PIC_OFFSET_TABLE_REGNUM))))
    {
      /* If emit_load_toc_table will use the link register, we need to save
	 it.  We use R12 for this purpose because emit_load_toc_table
	 can use register 0.  This allows us to use a plain 'blr' to return
	 from the procedure more often.  */
      int save_LR_around_toc_setup = (TARGET_ELF
				      && DEFAULT_ABI == ABI_V4
				      && flag_pic
				      && ! info->lr_save_p
				      && EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds) > 0);
      if (save_LR_around_toc_setup)
	{
	  rtx lr = gen_rtx_REG (Pmode, LR_REGNO);
	  rtx tmp = gen_rtx_REG (Pmode, 12);

	  sp_adjust = 0;
	  insn = emit_move_insn (tmp, lr);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  rs6000_emit_load_toc_table (TRUE);

	  insn = emit_move_insn (lr, tmp);
	  add_reg_note (insn, REG_CFA_RESTORE, lr);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      else
	rs6000_emit_load_toc_table (TRUE);
    }

#if TARGET_MACHO
  if (!TARGET_SINGLE_PIC_BASE
      && DEFAULT_ABI == ABI_DARWIN
      && flag_pic && crtl->uses_pic_offset_table)
    {
      rtx lr = gen_rtx_REG (Pmode, LR_REGNO);
      rtx src = gen_rtx_SYMBOL_REF (Pmode, MACHOPIC_FUNCTION_BASE_NAME);

      /* Save and restore LR locally around this call (in R0).  */
      if (!info->lr_save_p)
	emit_move_insn (gen_rtx_REG (Pmode, 0), lr);

      emit_insn (gen_load_macho_picbase (Pmode, src));

      emit_move_insn (gen_rtx_REG (Pmode,
				   RS6000_PIC_OFFSET_TABLE_REGNUM),
		      lr);

      if (!info->lr_save_p)
	emit_move_insn (lr, gen_rtx_REG (Pmode, 0));
    }
#endif

  /* If we need to, save the TOC register after doing the stack setup.
     Do not emit eh frame info for this save.  The unwinder wants info,
     conceptually attached to instructions in this function, about
     register values in the caller of this function.  This R2 may have
     already been changed from the value in the caller.
     We don't attempt to write accurate DWARF EH frame info for R2
     because code emitted by gcc for a (non-pointer) function call
     doesn't save and restore R2.  Instead, R2 is managed out-of-line
     by a linker generated plt call stub when the function resides in
     a shared library.  This behavior is costly to describe in DWARF,
     both in terms of the size of DWARF info and the time taken in the
     unwinder to interpret it.  R2 changes, apart from the
     calls_eh_return case earlier in this function, are handled by
     linux-unwind.h frob_update_context.  */
  if (rs6000_save_toc_in_prologue_p ()
      && !cfun->machine->toc_is_wrapped_separately)
    {
      rtx reg = gen_rtx_REG (reg_mode, TOC_REGNUM);
      emit_insn (gen_frame_store (reg, sp_reg_rtx, RS6000_TOC_SAVE_SLOT));
    }

  /* Set up the arg pointer (r12) for -fsplit-stack code.  */
  if (using_split_stack && split_stack_arg_pointer_used_p ())
    emit_split_stack_prologue (info, sp_adjust, frame_off, frame_reg_rtx);
}

/* Output .extern statements for the save/restore routines we use.  */

static void
rs6000_output_savres_externs (FILE *file)
{
  rs6000_stack_t *info = rs6000_stack_info ();

  if (TARGET_DEBUG_STACK)
    debug_stack_info (info);

  /* Write .extern for any function we will call to save and restore
     fp values.  */
  if (info->first_fp_reg_save < 64
      && !TARGET_MACHO
      && !TARGET_ELF)
    {
      char *name;
      int regno = info->first_fp_reg_save - 32;

      if ((info->savres_strategy & SAVE_INLINE_FPRS) == 0)
	{
	  bool lr = (info->savres_strategy & SAVE_NOINLINE_FPRS_SAVES_LR) != 0;
	  int sel = SAVRES_SAVE | SAVRES_FPR | (lr ? SAVRES_LR : 0);
	  name = rs6000_savres_routine_name (regno, sel);
	  fprintf (file, "\t.extern %s\n", name);
	}
      if ((info->savres_strategy & REST_INLINE_FPRS) == 0)
	{
	  bool lr = (info->savres_strategy
		     & REST_NOINLINE_FPRS_DOESNT_RESTORE_LR) == 0;
	  int sel = SAVRES_FPR | (lr ? SAVRES_LR : 0);
	  name = rs6000_savres_routine_name (regno, sel);
	  fprintf (file, "\t.extern %s\n", name);
	}
    }
}

/* Write function prologue.  */

void
rs6000_output_function_prologue (FILE *file)
{
  if (!cfun->is_thunk)
    {
      rs6000_output_savres_externs (file);
#ifdef USING_ELFOS_H
      const char *curr_machine = rs6000_machine_from_flags ();
      if (rs6000_machine != curr_machine)
	{
	  rs6000_machine = curr_machine;
	  emit_asm_machine ();
	}
#endif
    }

  /* ELFv2 ABI r2 setup code and local entry point.  This must follow
     immediately after the global entry point label.  */
  if (rs6000_global_entry_point_prologue_needed_p ())
    {
      const char *name = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
      (*targetm.asm_out.internal_label) (file, "LCF", rs6000_pic_labelno);

      if (TARGET_CMODEL != CMODEL_LARGE)
	{
	  /* In the small and medium code models, we assume the TOC is less
	     2 GB away from the text section, so it can be computed via the
	     following two-instruction sequence.  */
	  char buf[256];

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
	  fprintf (file, "0:\taddis 2,12,.TOC.-");
	  assemble_name (file, buf);
	  fprintf (file, "@ha\n");
	  fprintf (file, "\taddi 2,2,.TOC.-");
	  assemble_name (file, buf);
	  fprintf (file, "@l\n");
	}
      else
	{
	  /* In the large code model, we allow arbitrary offsets between the
	     TOC and the text section, so we have to load the offset from
	     memory.  The data field is emitted directly before the global
	     entry point in rs6000_elf_declare_function_name.  */
	  char buf[256];

#ifdef HAVE_AS_ENTRY_MARKERS
	  /* If supported by the linker, emit a marker relocation.  If the
	     total code size of the final executable or shared library
	     happens to fit into 2 GB after all, the linker will replace
	     this code sequence with the sequence for the small or medium
	     code model.  */
	  fprintf (file, "\t.reloc .,R_PPC64_ENTRY\n");
#endif
	  fprintf (file, "\tld 2,");
	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCL", rs6000_pic_labelno);
	  assemble_name (file, buf);
	  fprintf (file, "-");
	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
	  assemble_name (file, buf);
	  fprintf (file, "(12)\n");
	  fprintf (file, "\tadd 2,2,12\n");
	}

      unsigned short patch_area_size = crtl->patch_area_size;
      unsigned short patch_area_entry = crtl->patch_area_entry;
      /* Need to emit the patching area.  */
      if (patch_area_size > 0)
	{
	  cfun->machine->global_entry_emitted = true;
	  /* As ELFv2 ABI shows, the allowable bytes between the global
	     and local entry points are 0, 4, 8, 16, 32 and 64 when
	     there is a local entry point.  Considering there are two
	     non-prefixed instructions for global entry point prologue
	     (8 bytes), the count for patchable nops before local entry
	     point would be 2, 6 and 14.  It's possible to support those
	     other counts of nops by not making a local entry point, but
	     we don't have clear use cases for them, so leave them
	     unsupported for now.  */
	  if (patch_area_entry > 0)
	    {
	      if (patch_area_entry != 2
		  && patch_area_entry != 6
		  && patch_area_entry != 14)
		error ("unsupported number of nops before function entry (%u)",
		       patch_area_entry);
	      rs6000_print_patchable_function_entry (file, patch_area_entry,
						     true);
	      patch_area_size -= patch_area_entry;
	    }
	}

      fputs ("\t.localentry\t", file);
      assemble_name (file, name);
      fputs (",.-", file);
      assemble_name (file, name);
      fputs ("\n", file);
      /* Emit the nops after local entry.  */
      if (patch_area_size > 0)
	rs6000_print_patchable_function_entry (file, patch_area_size,
					       patch_area_entry == 0);
    }

  else if (rs6000_pcrel_p ())
    {
      const char *name = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
      /* All functions compiled to use PC-relative addressing will
	 have a .localentry value of 0 or 1.  For now we set it to
	 1 all the time, indicating that the function may clobber
	 the TOC register r2.  Later we may optimize this by setting
	 it to 0 if the function is a leaf and does not clobber r2.  */
      fputs ("\t.localentry\t", file);
      assemble_name (file, name);
      fputs (",1\n", file);
    }

  /* Output -mprofile-kernel code.  This needs to be done here instead of
     in output_function_profile since it must go after the ELFv2 ABI
     local entry point.  */
  if (TARGET_PROFILE_KERNEL && crtl->profile)
    {
      gcc_assert (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2);
      gcc_assert (!TARGET_32BIT);

      asm_fprintf (file, "\tmflr %s\n", reg_names[0]);

      /* In the ELFv2 ABI we have no compiler stack word.  It must be
	 the resposibility of _mcount to preserve the static chain
	 register if required.  */
      if (DEFAULT_ABI != ABI_ELFv2
	  && cfun->static_chain_decl != NULL)
	{
	  asm_fprintf (file, "\tstd %s,24(%s)\n",
		       reg_names[STATIC_CHAIN_REGNUM], reg_names[1]);
	  fprintf (file, "\tbl %s\n", RS6000_MCOUNT);
	  asm_fprintf (file, "\tld %s,24(%s)\n",
		       reg_names[STATIC_CHAIN_REGNUM], reg_names[1]);
	}
      else
	fprintf (file, "\tbl %s\n", RS6000_MCOUNT);
    }

  rs6000_pic_labelno++;
}

/* -mprofile-kernel code calls mcount before the function prolog,
   so a profiled leaf function should stay a leaf function.  */
bool
rs6000_keep_leaf_when_profiled (void)
{
  return TARGET_PROFILE_KERNEL;
}

/* Non-zero if vmx regs are restored before the frame pop, zero if
   we restore after the pop when possible.  */
#define ALWAYS_RESTORE_ALTIVEC_BEFORE_POP 0

/* Restoring cr is a two step process: loading a reg from the frame
   save, then moving the reg to cr.  For ABI_V4 we must let the
   unwinder know that the stack location is no longer valid at or
   before the stack deallocation, but we can't emit a cfa_restore for
   cr at the stack deallocation like we do for other registers.
   The trouble is that it is possible for the move to cr to be
   scheduled after the stack deallocation.  So say exactly where cr
   is located on each of the two insns.  */

static rtx
load_cr_save (int regno, rtx frame_reg_rtx, int offset, bool exit_func)
{
  rtx mem = gen_frame_mem_offset (SImode, frame_reg_rtx, offset);
  rtx reg = gen_rtx_REG (SImode, regno);
  rtx_insn *insn = emit_move_insn (reg, mem);

  if (!exit_func && DEFAULT_ABI == ABI_V4)
    {
      rtx cr = gen_rtx_REG (SImode, CR2_REGNO);
      rtx set = gen_rtx_SET (reg, cr);

      add_reg_note (insn, REG_CFA_REGISTER, set);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  return reg;
}

/* Reload CR from REG.  */

static void
restore_saved_cr (rtx reg, bool using_mfcr_multiple, bool exit_func)
{
  int count = 0;
  int i;

  if (using_mfcr_multiple)
    {
      for (i = 0; i < 8; i++)
	if (save_reg_p (CR0_REGNO + i))
	  count++;
      gcc_assert (count);
    }

  if (using_mfcr_multiple && count > 1)
    {
      rtx_insn *insn;
      rtvec p;
      int ndx;

      p = rtvec_alloc (count);

      ndx = 0;
      for (i = 0; i < 8; i++)
	if (save_reg_p (CR0_REGNO + i))
	  {
	    rtvec r = rtvec_alloc (2);
	    RTVEC_ELT (r, 0) = reg;
	    RTVEC_ELT (r, 1) = GEN_INT (1 << (7-i));
	    RTVEC_ELT (p, ndx) =
	      gen_rtx_SET (gen_rtx_REG (CCmode, CR0_REGNO + i),
			   gen_rtx_UNSPEC (CCmode, r, UNSPEC_MOVESI_TO_CR));
	    ndx++;
	  }
      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
      gcc_assert (ndx == count);

      /* For the ELFv2 ABI we generate a CFA_RESTORE for each
	 CR field separately.  */
      if (!exit_func && DEFAULT_ABI == ABI_ELFv2 && flag_shrink_wrap)
	{
	  for (i = 0; i < 8; i++)
	    if (save_reg_p (CR0_REGNO + i))
	      add_reg_note (insn, REG_CFA_RESTORE,
			    gen_rtx_REG (SImode, CR0_REGNO + i));

	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
  else
    for (i = 0; i < 8; i++)
      if (save_reg_p (CR0_REGNO + i))
	{
	  rtx insn = emit_insn (gen_movsi_to_cr_one
				 (gen_rtx_REG (CCmode, CR0_REGNO + i), reg));

	  /* For the ELFv2 ABI we generate a CFA_RESTORE for each
	     CR field separately, attached to the insn that in fact
	     restores this particular CR field.  */
	  if (!exit_func && DEFAULT_ABI == ABI_ELFv2 && flag_shrink_wrap)
	    {
	      add_reg_note (insn, REG_CFA_RESTORE,
			    gen_rtx_REG (SImode, CR0_REGNO + i));

	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	}

  /* For other ABIs, we just generate a single CFA_RESTORE for CR2.  */
  if (!exit_func && DEFAULT_ABI != ABI_ELFv2
      && (DEFAULT_ABI == ABI_V4 || flag_shrink_wrap))
    {
      rtx_insn *insn = get_last_insn ();
      rtx cr = gen_rtx_REG (SImode, CR2_REGNO);

      add_reg_note (insn, REG_CFA_RESTORE, cr);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

/* Like cr, the move to lr instruction can be scheduled after the
   stack deallocation, but unlike cr, its stack frame save is still
   valid.  So we only need to emit the cfa_restore on the correct
   instruction.  */

static void
load_lr_save (int regno, rtx frame_reg_rtx, int offset)
{
  rtx mem = gen_frame_mem_offset (Pmode, frame_reg_rtx, offset);
  rtx reg = gen_rtx_REG (Pmode, regno);

  emit_move_insn (reg, mem);
}

static void
restore_saved_lr (int regno, bool exit_func)
{
  rtx reg = gen_rtx_REG (Pmode, regno);
  rtx lr = gen_rtx_REG (Pmode, LR_REGNO);
  rtx_insn *insn = emit_move_insn (lr, reg);

  if (!exit_func && flag_shrink_wrap)
    {
      add_reg_note (insn, REG_CFA_RESTORE, lr);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

static rtx
add_crlr_cfa_restore (const rs6000_stack_t *info, rtx cfa_restores)
{
  if (DEFAULT_ABI == ABI_ELFv2)
    {
      int i;
      for (i = 0; i < 8; i++)
	if (save_reg_p (CR0_REGNO + i))
	  {
	    rtx cr = gen_rtx_REG (SImode, CR0_REGNO + i);
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, cr,
					   cfa_restores);
	  }
    }
  else if (info->cr_save_p)
    cfa_restores = alloc_reg_note (REG_CFA_RESTORE,
				   gen_rtx_REG (SImode, CR2_REGNO),
				   cfa_restores);

  if (info->lr_save_p)
    cfa_restores = alloc_reg_note (REG_CFA_RESTORE,
				   gen_rtx_REG (Pmode, LR_REGNO),
				   cfa_restores);
  return cfa_restores;
}

/* Return true if OFFSET from stack pointer can be clobbered by signals.
   V.4 doesn't have any stack cushion, AIX ABIs have 220 or 288 bytes
   below stack pointer not cloberred by signals.  */

static inline bool
offset_below_red_zone_p (HOST_WIDE_INT offset)
{
  return offset < (DEFAULT_ABI == ABI_V4
		   ? 0
		   : TARGET_32BIT ? -220 : -288);
}

/* Append CFA_RESTORES to any existing REG_NOTES on the last insn.  */

static void
emit_cfa_restores (rtx cfa_restores)
{
  rtx_insn *insn = get_last_insn ();
  rtx *loc = &REG_NOTES (insn);

  while (*loc)
    loc = &XEXP (*loc, 1);
  *loc = cfa_restores;
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Emit function epilogue as insns.  */

void
rs6000_emit_epilogue (enum epilogue_type epilogue_type)
{
  HOST_WIDE_INT frame_off = 0;
  rtx sp_reg_rtx = gen_rtx_REG (Pmode, 1);
  rtx frame_reg_rtx = sp_reg_rtx;
  rtx cfa_restores = NULL_RTX;
  rtx insn;
  rtx cr_save_reg = NULL_RTX;
  machine_mode reg_mode = Pmode;
  int reg_size = TARGET_32BIT ? 4 : 8;
  machine_mode fp_reg_mode = TARGET_HARD_FLOAT ? DFmode : SFmode;
  int fp_reg_size = 8;
  int i;
  unsigned ptr_regno;

  rs6000_stack_t *info = rs6000_stack_info ();

  int strategy = info->savres_strategy;
  bool using_load_multiple = !!(strategy & REST_MULTIPLE);
  bool restoring_GPRs_inline = !!(strategy & REST_INLINE_GPRS);
  bool restoring_FPRs_inline = !!(strategy & REST_INLINE_FPRS);
  if (epilogue_type == EPILOGUE_TYPE_SIBCALL)
    {
      restoring_GPRs_inline = true;
      restoring_FPRs_inline = true;
    }

  bool using_mtcr_multiple = (rs6000_tune == PROCESSOR_PPC601
			      || rs6000_tune == PROCESSOR_PPC603
			      || rs6000_tune == PROCESSOR_PPC750
			      || optimize_size);

  /* Restore via the backchain when we have a large frame, since this
     is more efficient than an addis, addi pair.  The second condition
     here will not trigger at the moment;  We don't actually need a
     frame pointer for alloca, but the generic parts of the compiler
     give us one anyway.  */
  bool use_backchain_to_restore_sp
    = (info->total_size + (info->lr_save_p ? info->lr_save_offset : 0) > 32767
       || (cfun->calls_alloca && !frame_pointer_needed));

  bool restore_lr = (info->lr_save_p
		&& (restoring_FPRs_inline
		    || (strategy & REST_NOINLINE_FPRS_DOESNT_RESTORE_LR))
		&& (restoring_GPRs_inline
		    || info->first_fp_reg_save < 64)
		&& !cfun->machine->lr_is_wrapped_separately);


  if (WORLD_SAVE_P (info))
    {
      gcc_assert (epilogue_type != EPILOGUE_TYPE_SIBCALL);

      /* eh_rest_world_r10 will return to the location saved in the LR
	 stack slot (which is not likely to be our caller.)
	 Input: R10 -- stack adjustment.  Clobbers R0, R11, R12, R7, R8.
	 rest_world is similar, except any R10 parameter is ignored.
	 The exception-handling stuff that was here in 2.95 is no
	 longer necessary.  */

      rtvec p;
      p = rtvec_alloc (9
		       + 32 - info->first_gp_reg_save
		       + LAST_ALTIVEC_REGNO + 1 - info->first_altivec_reg_save
		       + 63 + 1 - info->first_fp_reg_save);

      const char *rname;
      switch (epilogue_type)
	{
	case EPILOGUE_TYPE_NORMAL:
	  rname = ggc_strdup ("*rest_world");
	  break;

	case EPILOGUE_TYPE_EH_RETURN:
	  rname = ggc_strdup ("*eh_rest_world_r10");
	  break;

	default:
	  gcc_unreachable ();
	}

      int j = 0;
      RTVEC_ELT (p, j++) = ret_rtx;
      RTVEC_ELT (p, j++)
	= gen_rtx_USE (VOIDmode, gen_rtx_SYMBOL_REF (Pmode, rname));
      /* The instruction pattern requires a clobber here;
	 it is shared with the restVEC helper. */
      RTVEC_ELT (p, j++) = gen_hard_reg_clobber (Pmode, 11);

      {
	/* CR register traditionally saved as CR2.  */
	rtx reg = gen_rtx_REG (SImode, CR2_REGNO);
	RTVEC_ELT (p, j++)
	  = gen_frame_load (reg, frame_reg_rtx, info->cr_save_offset);
	if (flag_shrink_wrap)
	  {
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE,
					   gen_rtx_REG (Pmode, LR_REGNO),
					   cfa_restores);
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	  }
      }

      int i;
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	{
	  rtx reg = gen_rtx_REG (reg_mode, info->first_gp_reg_save + i);
	  RTVEC_ELT (p, j++)
	    = gen_frame_load (reg,
			      frame_reg_rtx, info->gp_save_offset + reg_size * i);
	  if (flag_shrink_wrap
	      && save_reg_p (info->first_gp_reg_save + i))
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	}
      for (i = 0; info->first_altivec_reg_save + i <= LAST_ALTIVEC_REGNO; i++)
	{
	  rtx reg = gen_rtx_REG (V4SImode, info->first_altivec_reg_save + i);
	  RTVEC_ELT (p, j++)
	    = gen_frame_load (reg,
			      frame_reg_rtx, info->altivec_save_offset + 16 * i);
	  if (flag_shrink_wrap
	      && save_reg_p (info->first_altivec_reg_save + i))
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	}
      for (i = 0; info->first_fp_reg_save + i <= 63; i++)
	{
	  rtx reg = gen_rtx_REG (TARGET_HARD_FLOAT ? DFmode : SFmode,
				 info->first_fp_reg_save + i);
	  RTVEC_ELT (p, j++)
	    = gen_frame_load (reg, frame_reg_rtx, info->fp_save_offset + 8 * i);
	  if (flag_shrink_wrap
	      && save_reg_p (info->first_fp_reg_save + i))
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	}
      RTVEC_ELT (p, j++) = gen_hard_reg_clobber (Pmode, 0);
      RTVEC_ELT (p, j++) = gen_hard_reg_clobber (SImode, 12);
      RTVEC_ELT (p, j++) = gen_hard_reg_clobber (SImode, 7);
      RTVEC_ELT (p, j++) = gen_hard_reg_clobber (SImode, 8);
      RTVEC_ELT (p, j++)
	= gen_rtx_USE (VOIDmode, gen_rtx_REG (SImode, 10));
      insn = emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, p));

      if (flag_shrink_wrap)
	{
	  REG_NOTES (insn) = cfa_restores;
	  add_reg_note (insn, REG_CFA_DEF_CFA, sp_reg_rtx);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      return;
    }

  /* frame_reg_rtx + frame_off points to the top of this stack frame.  */
  if (info->push_p)
    frame_off = info->total_size;

  /* Restore AltiVec registers if we must do so before adjusting the
     stack.  */
  if (info->altivec_size != 0
      && (ALWAYS_RESTORE_ALTIVEC_BEFORE_POP
	  || (DEFAULT_ABI != ABI_V4
	      && offset_below_red_zone_p (info->altivec_save_offset))))
    {
      int i;
      int scratch_regno = ptr_regno_for_savres (SAVRES_VR);

      gcc_checking_assert (scratch_regno == 11 || scratch_regno == 12);
      if (use_backchain_to_restore_sp)
	{
	  int frame_regno = 11;

	  if ((strategy & REST_INLINE_VRS) == 0)
	    {
	      /* Of r11 and r12, select the one not clobbered by an
		 out-of-line restore function for the frame register.  */
	      frame_regno = 11 + 12 - scratch_regno;
	    }
	  frame_reg_rtx = gen_rtx_REG (Pmode, frame_regno);
	  emit_move_insn (frame_reg_rtx,
			  gen_rtx_MEM (Pmode, sp_reg_rtx));
	  frame_off = 0;
	}
      else if (frame_pointer_needed)
	frame_reg_rtx = hard_frame_pointer_rtx;

      if ((strategy & REST_INLINE_VRS) == 0)
	{
	  int end_save = info->altivec_save_offset + info->altivec_size;
	  int ptr_off;
	  rtx ptr_reg = gen_rtx_REG (Pmode, 0);
	  rtx scratch_reg = gen_rtx_REG (Pmode, scratch_regno);

	  if (end_save + frame_off != 0)
	    {
	      rtx offset = GEN_INT (end_save + frame_off);

	      emit_insn (gen_add3_insn (ptr_reg, frame_reg_rtx, offset));
	    }
	  else
	    emit_move_insn (ptr_reg, frame_reg_rtx);

	  ptr_off = -end_save;
	  insn = rs6000_emit_savres_rtx (info, scratch_reg,
					 info->altivec_save_offset + ptr_off,
					 0, V4SImode, SAVRES_VR);
	}
      else
	{
	  for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	    if (info->vrsave_mask & ALTIVEC_REG_BIT (i))
	      {
		rtx addr, areg, mem, insn;
		rtx reg = gen_rtx_REG (V4SImode, i);
		HOST_WIDE_INT offset
		  = (info->altivec_save_offset + frame_off
		     + 16 * (i - info->first_altivec_reg_save));

		if (TARGET_P9_VECTOR && quad_address_offset_p (offset))
		  {
		    mem = gen_frame_mem (V4SImode,
					 gen_rtx_PLUS (Pmode, frame_reg_rtx,
						       GEN_INT (offset)));
		    insn = gen_rtx_SET (reg, mem);
		  }
		else
		  {
		    areg = gen_rtx_REG (Pmode, 0);
		    emit_move_insn (areg, GEN_INT (offset));

		    /* AltiVec addressing mode is [reg+reg].  */
		    addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, areg);
		    mem = gen_frame_mem (V4SImode, addr);

		    /* Rather than emitting a generic move, force use of the
		       lvx instruction, which we always want.  In particular we
		       don't want lxvd2x/xxpermdi for little endian.  */
		    insn = gen_altivec_lvx_v4si_internal (reg, mem);
		  }

		(void) emit_insn (insn);
	      }
	}

      for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	if (((strategy & REST_INLINE_VRS) == 0
	     || (info->vrsave_mask & ALTIVEC_REG_BIT (i)) != 0)
	    && (flag_shrink_wrap
		|| (offset_below_red_zone_p
		    (info->altivec_save_offset
		     + 16 * (i - info->first_altivec_reg_save))))
	    && save_reg_p (i))
	  {
	    rtx reg = gen_rtx_REG (V4SImode, i);
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	  }
    }

  /* Restore VRSAVE if we must do so before adjusting the stack.  */
  if (info->vrsave_size != 0
      && (ALWAYS_RESTORE_ALTIVEC_BEFORE_POP
	  || (DEFAULT_ABI != ABI_V4
	      && offset_below_red_zone_p (info->vrsave_save_offset))))
    {
      rtx reg;

      if (frame_reg_rtx == sp_reg_rtx)
	{
	  if (use_backchain_to_restore_sp)
	    {
	      frame_reg_rtx = gen_rtx_REG (Pmode, 11);
	      emit_move_insn (frame_reg_rtx,
			      gen_rtx_MEM (Pmode, sp_reg_rtx));
	      frame_off = 0;
	    }
	  else if (frame_pointer_needed)
	    frame_reg_rtx = hard_frame_pointer_rtx;
	}

      reg = gen_rtx_REG (SImode, 12);
      emit_insn (gen_frame_load (reg, frame_reg_rtx,
				 info->vrsave_save_offset + frame_off));

      emit_insn (generate_set_vrsave (reg, info, 1));
    }

  insn = NULL_RTX;
  /* If we have a large stack frame, restore the old stack pointer
     using the backchain.  */
  if (use_backchain_to_restore_sp)
    {
      if (frame_reg_rtx == sp_reg_rtx)
	{
	  /* Under V.4, don't reset the stack pointer until after we're done
	     loading the saved registers.  */
	  if (DEFAULT_ABI == ABI_V4)
	    frame_reg_rtx = gen_rtx_REG (Pmode, 11);

	  insn = emit_move_insn (frame_reg_rtx,
				 gen_rtx_MEM (Pmode, sp_reg_rtx));
	  frame_off = 0;
	}
      else if (ALWAYS_RESTORE_ALTIVEC_BEFORE_POP
	       && DEFAULT_ABI == ABI_V4)
	/* frame_reg_rtx has been set up by the altivec restore.  */
	;
      else
	{
	  insn = emit_move_insn (sp_reg_rtx, frame_reg_rtx);
	  frame_reg_rtx = sp_reg_rtx;
	}
    }
  /* If we have a frame pointer, we can restore the old stack pointer
     from it.  */
  else if (frame_pointer_needed_indeed)
    {
      frame_reg_rtx = sp_reg_rtx;
      if (DEFAULT_ABI == ABI_V4)
	frame_reg_rtx = gen_rtx_REG (Pmode, 11);
      /* Prevent reordering memory accesses against stack pointer restore.  */
      else if (cfun->calls_alloca
	       || offset_below_red_zone_p (-info->total_size))
	rs6000_emit_stack_tie (frame_reg_rtx, true);

      insn = emit_insn (gen_add3_insn (frame_reg_rtx, hard_frame_pointer_rtx,
				       GEN_INT (info->total_size)));
      frame_off = 0;
    }
  else if (info->push_p
	   && DEFAULT_ABI != ABI_V4
	   && epilogue_type != EPILOGUE_TYPE_EH_RETURN)
    {
      /* Prevent reordering memory accesses against stack pointer restore.  */
      if (cfun->calls_alloca
	  || offset_below_red_zone_p (-info->total_size))
	rs6000_emit_stack_tie (frame_reg_rtx, false);
      insn = emit_insn (gen_add3_insn (sp_reg_rtx, sp_reg_rtx,
				       GEN_INT (info->total_size)));
      frame_off = 0;
    }
  if (insn && frame_reg_rtx == sp_reg_rtx)
    {
      if (cfa_restores)
	{
	  REG_NOTES (insn) = cfa_restores;
	  cfa_restores = NULL_RTX;
	}
      add_reg_note (insn, REG_CFA_DEF_CFA, sp_reg_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Restore AltiVec registers if we have not done so already.  */
  if (!ALWAYS_RESTORE_ALTIVEC_BEFORE_POP
      && info->altivec_size != 0
      && (DEFAULT_ABI == ABI_V4
	  || !offset_below_red_zone_p (info->altivec_save_offset)))
    {
      int i;

      if ((strategy & REST_INLINE_VRS) == 0)
	{
	  int end_save = info->altivec_save_offset + info->altivec_size;
	  int ptr_off;
	  rtx ptr_reg = gen_rtx_REG (Pmode, 0);
	  int scratch_regno = ptr_regno_for_savres (SAVRES_VR);
	  rtx scratch_reg = gen_rtx_REG (Pmode, scratch_regno);

	  if (end_save + frame_off != 0)
	    {
	      rtx offset = GEN_INT (end_save + frame_off);

	      emit_insn (gen_add3_insn (ptr_reg, frame_reg_rtx, offset));
	    }
	  else
	    emit_move_insn (ptr_reg, frame_reg_rtx);

	  ptr_off = -end_save;
	  insn = rs6000_emit_savres_rtx (info, scratch_reg,
					 info->altivec_save_offset + ptr_off,
					 0, V4SImode, SAVRES_VR);
	  if (REGNO (frame_reg_rtx) == REGNO (scratch_reg))
	    {
	      /* Frame reg was clobbered by out-of-line save.  Restore it
		 from ptr_reg, and if we are calling out-of-line gpr or
		 fpr restore set up the correct pointer and offset.  */
	      unsigned newptr_regno = 1;
	      if (!restoring_GPRs_inline)
		{
		  bool lr = info->gp_save_offset + info->gp_size == 0;
		  int sel = SAVRES_GPR | (lr ? SAVRES_LR : 0);
		  newptr_regno = ptr_regno_for_savres (sel);
		  end_save = info->gp_save_offset + info->gp_size;
		}
	      else if (!restoring_FPRs_inline)
		{
		  bool lr = !(strategy & REST_NOINLINE_FPRS_DOESNT_RESTORE_LR);
		  int sel = SAVRES_FPR | (lr ? SAVRES_LR : 0);
		  newptr_regno = ptr_regno_for_savres (sel);
		  end_save = info->fp_save_offset + info->fp_size;
		}

	      if (newptr_regno != 1 && REGNO (frame_reg_rtx) != newptr_regno)
		frame_reg_rtx = gen_rtx_REG (Pmode, newptr_regno);
		
	      if (end_save + ptr_off != 0)
		{
		  rtx offset = GEN_INT (end_save + ptr_off);

		  frame_off = -end_save;
		  if (TARGET_32BIT)
		    emit_insn (gen_addsi3_carry (frame_reg_rtx,
						 ptr_reg, offset));
		  else
		    emit_insn (gen_adddi3_carry (frame_reg_rtx,
						 ptr_reg, offset));
		}
	      else
		{
		  frame_off = ptr_off;
		  emit_move_insn (frame_reg_rtx, ptr_reg);
		}
	    }
	}
      else
	{
	  for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	    if (info->vrsave_mask & ALTIVEC_REG_BIT (i))
	      {
		rtx addr, areg, mem, insn;
		rtx reg = gen_rtx_REG (V4SImode, i);
		HOST_WIDE_INT offset
		  = (info->altivec_save_offset + frame_off
		     + 16 * (i - info->first_altivec_reg_save));

		if (TARGET_P9_VECTOR && quad_address_offset_p (offset))
		  {
		    mem = gen_frame_mem (V4SImode,
					 gen_rtx_PLUS (Pmode, frame_reg_rtx,
						       GEN_INT (offset)));
		    insn = gen_rtx_SET (reg, mem);
		  }
		else
		  {
		    areg = gen_rtx_REG (Pmode, 0);
		    emit_move_insn (areg, GEN_INT (offset));

		    /* AltiVec addressing mode is [reg+reg].  */
		    addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, areg);
		    mem = gen_frame_mem (V4SImode, addr);

		    /* Rather than emitting a generic move, force use of the
		       lvx instruction, which we always want.  In particular we
		       don't want lxvd2x/xxpermdi for little endian.  */
		    insn = gen_altivec_lvx_v4si_internal (reg, mem);
		  }

		(void) emit_insn (insn);
	      }
	}

      for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	if (((strategy & REST_INLINE_VRS) == 0
	     || (info->vrsave_mask & ALTIVEC_REG_BIT (i)) != 0)
	    && (DEFAULT_ABI == ABI_V4 || flag_shrink_wrap)
	    && save_reg_p (i))
	  {
	    rtx reg = gen_rtx_REG (V4SImode, i);
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	  }
    }

  /* Restore VRSAVE if we have not done so already.  */
  if (!ALWAYS_RESTORE_ALTIVEC_BEFORE_POP
      && info->vrsave_size != 0
      && (DEFAULT_ABI == ABI_V4
	  || !offset_below_red_zone_p (info->vrsave_save_offset)))
    {
      rtx reg;

      reg = gen_rtx_REG (SImode, 12);
      emit_insn (gen_frame_load (reg, frame_reg_rtx,
				 info->vrsave_save_offset + frame_off));

      emit_insn (generate_set_vrsave (reg, info, 1));
    }

  /* If we exit by an out-of-line restore function on ABI_V4 then that
     function will deallocate the stack, so we don't need to worry
     about the unwinder restoring cr from an invalid stack frame
     location.  */
  bool exit_func = (!restoring_FPRs_inline
		    || (!restoring_GPRs_inline
			&& info->first_fp_reg_save == 64));

  /* In the ELFv2 ABI we need to restore all call-saved CR fields from
     *separate* slots if the routine calls __builtin_eh_return, so
     that they can be independently restored by the unwinder.  Since
     it is for CR fields restoring, it should be done for any epilogue
     types (not EPILOGUE_TYPE_EH_RETURN specific).  */
  if (DEFAULT_ABI == ABI_ELFv2 && crtl->calls_eh_return)
    {
      int i, cr_off = info->ehcr_offset;

      for (i = 0; i < 8; i++)
	if (!call_used_or_fixed_reg_p (CR0_REGNO + i))
	  {
	    rtx reg = gen_rtx_REG (SImode, 0);
	    emit_insn (gen_frame_load (reg, frame_reg_rtx,
				       cr_off + frame_off));

	    insn = emit_insn (gen_movsi_to_cr_one
				(gen_rtx_REG (CCmode, CR0_REGNO + i), reg));

	    if (!exit_func && flag_shrink_wrap)
	      {
		add_reg_note (insn, REG_CFA_RESTORE,
			      gen_rtx_REG (SImode, CR0_REGNO + i));

		RTX_FRAME_RELATED_P (insn) = 1;
	      }

	    cr_off += reg_size;
	  }
    }

  /* Get the old lr if we saved it.  If we are restoring registers
     out-of-line, then the out-of-line routines can do this for us.  */
  if (restore_lr && restoring_GPRs_inline)
    load_lr_save (0, frame_reg_rtx, info->lr_save_offset + frame_off);

  /* Get the old cr if we saved it.  */
  if (info->cr_save_p)
    {
      unsigned cr_save_regno = 12;

      if (!restoring_GPRs_inline)
	{
	  /* Ensure we don't use the register used by the out-of-line
	     gpr register restore below.  */
	  bool lr = info->gp_save_offset + info->gp_size == 0;
	  int sel = SAVRES_GPR | (lr ? SAVRES_LR : 0);
	  int gpr_ptr_regno = ptr_regno_for_savres (sel);

	  if (gpr_ptr_regno == 12)
	    cr_save_regno = 11;
	  gcc_checking_assert (REGNO (frame_reg_rtx) != cr_save_regno);
	}
      else if (REGNO (frame_reg_rtx) == 12)
	cr_save_regno = 11;

      /* For ELFv2 r12 is already in use as the GEP.  */
      if (DEFAULT_ABI == ABI_ELFv2)
	cr_save_regno = 11;

      cr_save_reg = load_cr_save (cr_save_regno, frame_reg_rtx,
				  info->cr_save_offset + frame_off,
				  exit_func);
    }

  /* Set LR here to try to overlap restores below.  */
  if (restore_lr && restoring_GPRs_inline)
    restore_saved_lr (0, exit_func);

  /* Load exception handler data registers, if needed.  */
  if (epilogue_type == EPILOGUE_TYPE_EH_RETURN)
    {
      unsigned int i, regno;

      if (TARGET_AIX)
	{
	  rtx reg = gen_rtx_REG (reg_mode, 2);
	  emit_insn (gen_frame_load (reg, frame_reg_rtx,
				     frame_off + RS6000_TOC_SAVE_SLOT));
	}

      for (i = 0; ; ++i)
	{
	  rtx mem;

	  regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;

	  mem = gen_frame_mem_offset (reg_mode, frame_reg_rtx,
				      info->ehrd_offset + frame_off
				      + reg_size * (int) i);

	  emit_move_insn (gen_rtx_REG (reg_mode, regno), mem);
	}
    }

  /* Restore GPRs.  This is done as a PARALLEL if we are using
     the load-multiple instructions.  */
  if (!restoring_GPRs_inline)
    {
      /* We are jumping to an out-of-line function.  */
      rtx ptr_reg;
      int end_save = info->gp_save_offset + info->gp_size;
      bool can_use_exit = end_save == 0;
      int sel = SAVRES_GPR | (can_use_exit ? SAVRES_LR : 0);
      int ptr_off;

      /* Emit stack reset code if we need it.  */
      ptr_regno = ptr_regno_for_savres (sel);
      ptr_reg = gen_rtx_REG (Pmode, ptr_regno);
      if (can_use_exit)
	rs6000_emit_stack_reset (frame_reg_rtx, frame_off, ptr_regno);
      else if (end_save + frame_off != 0)
	emit_insn (gen_add3_insn (ptr_reg, frame_reg_rtx,
				  GEN_INT (end_save + frame_off)));
      else if (REGNO (frame_reg_rtx) != ptr_regno)
	emit_move_insn (ptr_reg, frame_reg_rtx);
      if (REGNO (frame_reg_rtx) == ptr_regno)
	frame_off = -end_save;

      if (can_use_exit && info->cr_save_p)
	restore_saved_cr (cr_save_reg, using_mtcr_multiple, true);

      ptr_off = -end_save;
      rs6000_emit_savres_rtx (info, ptr_reg,
			      info->gp_save_offset + ptr_off,
			      info->lr_save_offset + ptr_off,
			      reg_mode, sel);
    }
  else if (using_load_multiple)
    {
      rtvec p;
      p = rtvec_alloc (32 - info->first_gp_reg_save);
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	RTVEC_ELT (p, i)
	  = gen_frame_load (gen_rtx_REG (reg_mode, info->first_gp_reg_save + i),
			    frame_reg_rtx,
			    info->gp_save_offset + frame_off + reg_size * i);
      emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
    }
  else
    {
      int offset = info->gp_save_offset + frame_off;
      for (i = info->first_gp_reg_save; i < 32; i++)
	{
	  if (save_reg_p (i)
	      && !cfun->machine->gpr_is_wrapped_separately[i])
	    {
	      rtx reg = gen_rtx_REG (reg_mode, i);
	      emit_insn (gen_frame_load (reg, frame_reg_rtx, offset));
	    }

	  offset += reg_size;
	}
    }

  if (DEFAULT_ABI == ABI_V4 || flag_shrink_wrap)
    {
      /* If the frame pointer was used then we can't delay emitting
	 a REG_CFA_DEF_CFA note.  This must happen on the insn that
	 restores the frame pointer, r31.  We may have already emitted
	 a REG_CFA_DEF_CFA note, but that's OK;  A duplicate is
	 discarded by dwarf2cfi.cc/dwarf2out.cc, and in any case would
	 be harmless if emitted.  */
      if (frame_pointer_needed_indeed)
	{
	  insn = get_last_insn ();
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, frame_reg_rtx, frame_off));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* Set up cfa_restores.  We always need these when
	 shrink-wrapping.  If not shrink-wrapping then we only need
	 the cfa_restore when the stack location is no longer valid.
	 The cfa_restores must be emitted on or before the insn that
	 invalidates the stack, and of course must not be emitted
	 before the insn that actually does the restore.  The latter
	 is why it is a bad idea to emit the cfa_restores as a group
	 on the last instruction here that actually does a restore:
	 That insn may be reordered with respect to others doing
	 restores.  */
      if (flag_shrink_wrap
	  && !restoring_GPRs_inline
	  && info->first_fp_reg_save == 64)
	cfa_restores = add_crlr_cfa_restore (info, cfa_restores);

      for (i = info->first_gp_reg_save; i < 32; i++)
	if (save_reg_p (i)
	    && !cfun->machine->gpr_is_wrapped_separately[i])
	  {
	    rtx reg = gen_rtx_REG (reg_mode, i);
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	  }
    }

  if (!restoring_GPRs_inline
      && info->first_fp_reg_save == 64)
    {
      /* We are jumping to an out-of-line function.  */
      if (cfa_restores)
	emit_cfa_restores (cfa_restores);
      return;
    }

  if (restore_lr && !restoring_GPRs_inline)
    {
      load_lr_save (0, frame_reg_rtx, info->lr_save_offset + frame_off);
      restore_saved_lr (0, exit_func);
    }

  /* Restore fpr's if we need to do it without calling a function.  */
  if (restoring_FPRs_inline)
    {
      int offset = info->fp_save_offset + frame_off;
      for (i = info->first_fp_reg_save; i < 64; i++)
	{
	  if (save_reg_p (i)
	      && !cfun->machine->fpr_is_wrapped_separately[i - 32])
	    {
	      rtx reg = gen_rtx_REG (fp_reg_mode, i);
	      emit_insn (gen_frame_load (reg, frame_reg_rtx, offset));
	      if (DEFAULT_ABI == ABI_V4 || flag_shrink_wrap)
		cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg,
					       cfa_restores);
	    }

	  offset += fp_reg_size;
	}
    }

  /* If we saved cr, restore it here.  Just those that were used.  */
  if (info->cr_save_p)
    restore_saved_cr (cr_save_reg, using_mtcr_multiple, exit_func);

  /* If this is V.4, unwind the stack pointer after all of the loads
     have been done, or set up r11 if we are restoring fp out of line.  */
  ptr_regno = 1;
  if (!restoring_FPRs_inline)
    {
      bool lr = (strategy & REST_NOINLINE_FPRS_DOESNT_RESTORE_LR) == 0;
      int sel = SAVRES_FPR | (lr ? SAVRES_LR : 0);
      ptr_regno = ptr_regno_for_savres (sel);
    }

  insn = rs6000_emit_stack_reset (frame_reg_rtx, frame_off, ptr_regno);
  if (REGNO (frame_reg_rtx) == ptr_regno)
    frame_off = 0;

  if (insn && restoring_FPRs_inline)
    {
      if (cfa_restores)
	{
	  REG_NOTES (insn) = cfa_restores;
	  cfa_restores = NULL_RTX;
	}
      add_reg_note (insn, REG_CFA_DEF_CFA, sp_reg_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (epilogue_type == EPILOGUE_TYPE_EH_RETURN)
    {
      rtx sa = EH_RETURN_STACKADJ_RTX;
      emit_insn (gen_add3_insn (sp_reg_rtx, sp_reg_rtx, sa));
    }

  /* The ROP hash check must occur after the stack pointer is restored
     (since the hash involves r1), and is not performed for a sibcall.  */
  if (info->rop_hash_size
      && epilogue_type != EPILOGUE_TYPE_SIBCALL)
    {
      rtx stack_ptr = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
      rtx addr = gen_rtx_PLUS (Pmode, stack_ptr,
			       GEN_INT (info->rop_hash_save_offset));
      rtx mem = gen_rtx_MEM (Pmode, addr);
      rtx reg0 = gen_rtx_REG (Pmode, 0);
      emit_insn (gen_hashchk (reg0, mem));
    }

  if (epilogue_type != EPILOGUE_TYPE_SIBCALL && restoring_FPRs_inline)
    {
      if (cfa_restores)
	{
	  /* We can't hang the cfa_restores off a simple return,
	     since the shrink-wrap code sometimes uses an existing
	     return.  This means there might be a path from
	     pre-prologue code to this return, and dwarf2cfi code
	     wants the eh_frame unwinder state to be the same on
	     all paths to any point.  So we need to emit the
	     cfa_restores before the return.  For -m64 we really
	     don't need epilogue cfa_restores at all, except for
	     this irritating dwarf2cfi with shrink-wrap
	     requirement;  The stack red-zone means eh_frame info
	     from the prologue telling the unwinder to restore
	     from the stack is perfectly good right to the end of
	     the function.  */
	  emit_insn (gen_blockage ());
	  emit_cfa_restores (cfa_restores);
	  cfa_restores = NULL_RTX;
	}

      emit_jump_insn (targetm.gen_simple_return ());
    }

  if (epilogue_type != EPILOGUE_TYPE_SIBCALL && !restoring_FPRs_inline)
    {
      bool lr = (strategy & REST_NOINLINE_FPRS_DOESNT_RESTORE_LR) == 0;
      rtvec p = rtvec_alloc (3 + !!lr + 64 - info->first_fp_reg_save);
      int elt = 0;
      RTVEC_ELT (p, elt++) = ret_rtx;
      if (lr)
	RTVEC_ELT (p, elt++) = gen_hard_reg_clobber (Pmode, LR_REGNO);

      /* We have to restore more than two FP registers, so branch to the
	 restore function.  It will return to our caller.  */
      int i;
      int reg;
      rtx sym;

      if (flag_shrink_wrap)
	cfa_restores = add_crlr_cfa_restore (info, cfa_restores);

      sym = rs6000_savres_routine_sym (info, SAVRES_FPR | (lr ? SAVRES_LR : 0));
      RTVEC_ELT (p, elt++) = gen_rtx_USE (VOIDmode, sym);
      reg = (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)? 1 : 11;
      RTVEC_ELT (p, elt++) = gen_rtx_USE (VOIDmode, gen_rtx_REG (Pmode, reg));

      for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	{
	  rtx reg = gen_rtx_REG (DFmode, info->first_fp_reg_save + i);

	  RTVEC_ELT (p, elt++)
	    = gen_frame_load (reg, sp_reg_rtx, info->fp_save_offset + 8 * i);
	  if (flag_shrink_wrap
	      && save_reg_p (info->first_fp_reg_save + i))
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	}

      emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, p));
    }

  if (cfa_restores)
    {
      if (epilogue_type == EPILOGUE_TYPE_SIBCALL)
	/* Ensure the cfa_restores are hung off an insn that won't
	   be reordered above other restores.  */
	emit_insn (gen_blockage ());

      emit_cfa_restores (cfa_restores);
    }
}

#if TARGET_MACHO

/* Generate far-jump branch islands for everything recorded in
   branch_islands.  Invoked immediately after the last instruction of
   the epilogue has been emitted; the branch islands must be appended
   to, and contiguous with, the function body.  Mach-O stubs are
   generated in machopic_output_stub().  */

static void
macho_branch_islands (void)
{
  char tmp_buf[512];

  while (!vec_safe_is_empty (branch_islands))
    {
      branch_island *bi = &branch_islands->last ();
      const char *label = IDENTIFIER_POINTER (bi->label_name);
      const char *name = IDENTIFIER_POINTER (bi->function_name);
      char name_buf[512];
      /* Cheap copy of the details from the Darwin ASM_OUTPUT_LABELREF().  */
      if (name[0] == '*' || name[0] == '&')
	strcpy (name_buf, name+1);
      else
	{
	  name_buf[0] = '_';
	  strcpy (name_buf+1, name);
	}
      strcpy (tmp_buf, "\n");
      strcat (tmp_buf, label);
      if (flag_pic)
	{
	  strcat (tmp_buf, ":\n\tmflr r0\n\tbcl 20,31,");
	  strcat (tmp_buf, label);
	  strcat (tmp_buf, "_pic\n");
	  strcat (tmp_buf, label);
	  strcat (tmp_buf, "_pic:\n\tmflr r11\n");

	  strcat (tmp_buf, "\taddis r11,r11,ha16(");
	  strcat (tmp_buf, name_buf);
	  strcat (tmp_buf, " - ");
	  strcat (tmp_buf, label);
	  strcat (tmp_buf, "_pic)\n");

	  strcat (tmp_buf, "\tmtlr r0\n");

	  strcat (tmp_buf, "\taddi r12,r11,lo16(");
	  strcat (tmp_buf, name_buf);
	  strcat (tmp_buf, " - ");
	  strcat (tmp_buf, label);
	  strcat (tmp_buf, "_pic)\n");

	  strcat (tmp_buf, "\tmtctr r12\n\tbctr\n");
	}
      else
	{
	  strcat (tmp_buf, ":\n\tlis r12,hi16(");
	  strcat (tmp_buf, name_buf);
	  strcat (tmp_buf, ")\n\tori r12,r12,lo16(");
	  strcat (tmp_buf, name_buf);
	  strcat (tmp_buf, ")\n\tmtctr r12\n\tbctr");
	}
      output_asm_insn (tmp_buf, 0);
      branch_islands->pop ();
    }
}
#endif

/* Write function epilogue.  */

void
rs6000_output_function_epilogue (FILE *file)
{
#if TARGET_MACHO
  macho_branch_islands ();

  {
    rtx_insn *insn = get_last_insn ();
    rtx_insn *deleted_debug_label = NULL;

    /* Mach-O doesn't support labels at the end of objects, so if
       it looks like we might want one, take special action.

       First, collect any sequence of deleted debug labels.  */
    while (insn
	   && NOTE_P (insn)
	   && NOTE_KIND (insn) != NOTE_INSN_DELETED_LABEL)
      {
	/* Don't insert a nop for NOTE_INSN_DELETED_DEBUG_LABEL
	   notes only, instead set their CODE_LABEL_NUMBER to -1,
	   otherwise there would be code generation differences
	   in between -g and -g0.  */
	if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_DELETED_DEBUG_LABEL)
	  deleted_debug_label = insn;
	insn = PREV_INSN (insn);
      }

    /* Second, if we have:
       label:
	 barrier
       then this needs to be detected, so skip past the barrier.  */

    if (insn && BARRIER_P (insn))
      insn = PREV_INSN (insn);

    /* Up to now we've only seen notes or barriers.  */
    if (insn)
      {
	if (LABEL_P (insn)
	    || (NOTE_P (insn)
		&& NOTE_KIND (insn) == NOTE_INSN_DELETED_LABEL))
	  /* Trailing label: <barrier>.  */
	  fputs ("\tnop\n", file);
	else
	  {
	    /* Lastly, see if we have a completely empty function body.  */
	    while (insn && ! INSN_P (insn))
	      insn = PREV_INSN (insn);
	    /* If we don't find any insns, we've got an empty function body;
	       I.e. completely empty - without a return or branch.  This is
	       taken as the case where a function body has been removed
	       because it contains an inline __builtin_unreachable().  GCC
	       states that reaching __builtin_unreachable() means UB so we're
	       not obliged to do anything special; however, we want
	       non-zero-sized function bodies.  To meet this, and help the
	       user out, let's trap the case.  */
	    if (insn == NULL)
	      fputs ("\ttrap\n", file);
	  }
      }
    else if (deleted_debug_label)
      for (insn = deleted_debug_label; insn; insn = NEXT_INSN (insn))
	if (NOTE_KIND (insn) == NOTE_INSN_DELETED_DEBUG_LABEL)
	  CODE_LABEL_NUMBER (insn) = -1;
  }
#endif

  /* Output a traceback table here.  See /usr/include/sys/debug.h for info
     on its format.

     We don't output a traceback table if -finhibit-size-directive was
     used.  The documentation for -finhibit-size-directive reads
     ``don't output a @code{.size} assembler directive, or anything
     else that would cause trouble if the function is split in the
     middle, and the two halves are placed at locations far apart in
     memory.''  The traceback table has this property, since it
     includes the offset from the start of the function to the
     traceback table itself.

     System V.4 Powerpc's (and the embedded ABI derived from it) use a
     different traceback table.  */
  if ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
      && ! flag_inhibit_size_directive
      && rs6000_traceback != traceback_none && !cfun->is_thunk)
    {
      const char *fname = NULL;
      const char *language_string = lang_hooks.name;
      int fixed_parms = 0, float_parms = 0, parm_info = 0;
      int i;
      int optional_tbtab;
      rs6000_stack_t *info = rs6000_stack_info ();

      if (rs6000_traceback == traceback_full)
	optional_tbtab = 1;
      else if (rs6000_traceback == traceback_part)
	optional_tbtab = 0;
      else
	optional_tbtab = !optimize_size && !TARGET_ELF;

      if (optional_tbtab)
	{
	  fname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
	  while (*fname == '.')	/* V.4 encodes . in the name */
	    fname++;

	  /* Need label immediately before tbtab, so we can compute
	     its offset from the function start.  */
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LT");
	  ASM_OUTPUT_LABEL (file, fname);
	}

      /* The .tbtab pseudo-op can only be used for the first eight
	 expressions, since it can't handle the possibly variable
	 length fields that follow.  However, if you omit the optional
	 fields, the assembler outputs zeros for all optional fields
	 anyways, giving each variable length field is minimum length
	 (as defined in sys/debug.h).  Thus we cannot use the .tbtab
	 pseudo-op at all.  */

      /* An all-zero word flags the start of the tbtab, for debuggers
	 that have to find it by searching forward from the entry
	 point or from the current pc.  */
      fputs ("\t.long 0\n", file);

      /* Tbtab format type.  Use format type 0.  */
      fputs ("\t.byte 0,", file);

      /* Language type.  Unfortunately, there does not seem to be any
	 official way to discover the language being compiled, so we
	 use language_string.
	 C is 0.  Fortran is 1.  Ada is 3.  Modula-2 is 8.  C++ is 9.
	 Java is 13.  Objective-C is 14.  Objective-C++ isn't assigned
	 a number, so for now use 9.  LTO, Go, D, and JIT aren't assigned
	 numbers either, so for now use 0.  */
      if (lang_GNU_C ()
	  || ! strcmp (language_string, "GNU GIMPLE")
	  || ! strcmp (language_string, "GNU Go")
	  || ! strcmp (language_string, "GNU D")
	  || ! strcmp (language_string, "GNU Rust")
	  || ! strcmp (language_string, "libgccjit"))
	i = 0;
      else if (! strcmp (language_string, "GNU F77")
	       || lang_GNU_Fortran ())
	i = 1;
      else if (! strcmp (language_string, "GNU Ada"))
	i = 3;
      else if (! strcmp (language_string, "GNU Modula-2"))
	i = 8;
      else if (lang_GNU_CXX ()
	       || ! strcmp (language_string, "GNU Objective-C++"))
	i = 9;
      else if (! strcmp (language_string, "GNU Java"))
	i = 13;
      else if (! strcmp (language_string, "GNU Objective-C"))
	i = 14;
      else
	gcc_unreachable ();
      fprintf (file, "%d,", i);

      /* 8 single bit fields: global linkage (not set for C extern linkage,
	 apparently a PL/I convention?), out-of-line epilogue/prologue, offset
	 from start of procedure stored in tbtab, internal function, function
	 has controlled storage, function has no toc, function uses fp,
	 function logs/aborts fp operations.  */
      /* Assume that fp operations are used if any fp reg must be saved.  */
      fprintf (file, "%d,",
	       (optional_tbtab << 5) | ((info->first_fp_reg_save != 64) << 1));

      /* 6 bitfields: function is interrupt handler, name present in
	 proc table, function calls alloca, on condition directives
	 (controls stack walks, 3 bits), saves condition reg, saves
	 link reg.  */
      /* The `function calls alloca' bit seems to be set whenever reg 31 is
	 set up as a frame pointer, even when there is no alloca call.  */
      fprintf (file, "%d,",
	       ((optional_tbtab << 6)
		| ((optional_tbtab & frame_pointer_needed) << 5)
		| (info->cr_save_p << 1)
		| (info->lr_save_p)));

      /* 3 bitfields: saves backchain, fixup code, number of fpr saved
	 (6 bits).  */
      fprintf (file, "%d,",
	       (info->push_p << 7) | (64 - info->first_fp_reg_save));

      /* 2 bitfields: spare bits (2 bits), number of gpr saved (6 bits).  */
      fprintf (file, "%d,", (32 - first_reg_to_save ()));

      if (optional_tbtab)
	{
	  /* Compute the parameter info from the function decl argument
	     list.  */
	  tree decl;
	  int next_parm_info_bit = 31;

	  for (decl = DECL_ARGUMENTS (current_function_decl);
	       decl; decl = DECL_CHAIN (decl))
	    {
	      rtx parameter = DECL_INCOMING_RTL (decl);
	      machine_mode mode = GET_MODE (parameter);

	      if (REG_P (parameter))
		{
		  if (SCALAR_FLOAT_MODE_P (mode))
		    {
		      int bits;

		      float_parms++;

		      switch (mode)
			{
			case E_SFmode:
			case E_SDmode:
			  bits = 0x2;
			  break;

			case E_DFmode:
			case E_DDmode:
			case E_TFmode:
			case E_TDmode:
			case E_IFmode:
			case E_KFmode:
			  bits = 0x3;
			  break;

			default:
			  gcc_unreachable ();
			}

		      /* If only one bit will fit, don't or in this entry.  */
		      if (next_parm_info_bit > 0)
			parm_info |= (bits << (next_parm_info_bit - 1));
		      next_parm_info_bit -= 2;
		    }
		  else
		    {
		      fixed_parms += ((GET_MODE_SIZE (mode)
				       + (UNITS_PER_WORD - 1))
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

      if (optional_tbtab)
	{
	  /* Optional fields follow.  Some are variable length.  */

	  /* Parameter types, left adjusted bit fields: 0 fixed, 10 single
	     float, 11 double float.  */
	  /* There is an entry for each parameter in a register, in the order
	     that they occur in the parameter list.  Any intervening arguments
	     on the stack are ignored.  If the list overflows a long (max
	     possible length 34 bits) then completely leave off all elements
	     that don't fit.  */
	  /* Only emit this long if there was at least one parameter.  */
	  if (fixed_parms || float_parms)
	    fprintf (file, "\t.long %d\n", parm_info);

	  /* Offset from start of code to tb table.  */
	  fputs ("\t.long ", file);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LT");
	  RS6000_OUTPUT_BASENAME (file, fname);
	  putc ('-', file);
	  rs6000_output_function_entry (file, fname);
	  putc ('\n', file);

	  /* Interrupt handler mask.  */
	  /* Omit this long, since we never set the interrupt handler bit
	     above.  */

	  /* Number of CTL (controlled storage) anchors.  */
	  /* Omit this long, since the has_ctl bit is never set above.  */

	  /* Displacement into stack of each CTL anchor.  */
	  /* Omit this list of longs, because there are no CTL anchors.  */

	  /* Length of function name.  */
	  if (*fname == '*')
	    ++fname;
	  fprintf (file, "\t.short %d\n", (int) strlen (fname));

	  /* Function name.  */
	  assemble_string (fname, strlen (fname));

	  /* Register for alloca automatic storage; this is always reg 31.
	     Only emit this if the alloca bit was set above.  */
	  if (frame_pointer_needed)
	    fputs ("\t.byte 31\n", file);

	  fputs ("\t.align 2\n", file);
	}
    }

  /* Arrange to define .LCTOC1 label, if not already done.  */
  if (need_toc_init)
    {
      need_toc_init = 0;
      if (!toc_initialized)
	{
	  switch_to_section (toc_section);
	  switch_to_section (current_function_section ());
	}
    }
}

/* -fsplit-stack support.  */

/* A SYMBOL_REF for __morestack.  */
static GTY(()) rtx morestack_ref;

static rtx
gen_add3_const (rtx rt, rtx ra, long c)
{
  if (TARGET_64BIT)
    return gen_adddi3 (rt, ra, GEN_INT (c));
 else
    return gen_addsi3 (rt, ra, GEN_INT (c));
}

/* Emit -fsplit-stack prologue, which goes before the regular function
   prologue (at local entry point in the case of ELFv2).  */

void
rs6000_expand_split_stack_prologue (void)
{
  rs6000_stack_t *info = rs6000_stack_info ();
  unsigned HOST_WIDE_INT allocate;
  long alloc_hi, alloc_lo;
  rtx r0, r1, r12, lr, ok_label, compare, jump, call_fusage;
  rtx_insn *insn;

  gcc_assert (flag_split_stack && reload_completed);

  if (!info->push_p)
    {
      /* We need the -fsplit-stack prologue for functions that make
	 tail calls.  Tail calls don't count against crtl->is_leaf.
	 Note that we are called inside a sequence.  get_insns will
	 just return that (as yet empty) sequence, so instead we
	 access the function rtl with get_topmost_sequence.  */
      for (insn = get_topmost_sequence ()->first; insn; insn = NEXT_INSN (insn))
	if (CALL_P (insn))
	  break;
      if (!insn)
	return;
    }

  if (global_regs[29])
    {
      error ("%qs uses register r29", "%<-fsplit-stack%>");
      inform (DECL_SOURCE_LOCATION (global_regs_decl[29]),
	      "conflicts with %qD", global_regs_decl[29]);
    }

  allocate = info->total_size;
  if (allocate > (unsigned HOST_WIDE_INT) 1 << 31)
    {
      sorry ("Stack frame larger than 2G is not supported for "
	     "%<-fsplit-stack%>");
      return;
    }
  if (morestack_ref == NULL_RTX)
    {
      morestack_ref = gen_rtx_SYMBOL_REF (Pmode, "__morestack");
      SYMBOL_REF_FLAGS (morestack_ref) |= (SYMBOL_FLAG_LOCAL
					   | SYMBOL_FLAG_FUNCTION);
    }

  r0 = gen_rtx_REG (Pmode, 0);
  r1 = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  r12 = gen_rtx_REG (Pmode, 12);
  emit_insn (gen_load_split_stack_limit (r0));
  /* Always emit two insns here to calculate the requested stack,
     so that the linker can edit them when adjusting size for calling
     non-split-stack code.  */
  alloc_hi = (-allocate + 0x8000) & ~0xffffL;
  alloc_lo = -allocate - alloc_hi;
  if (alloc_hi != 0)
    {
      emit_insn (gen_add3_const (r12, r1, alloc_hi));
      if (alloc_lo != 0)
	emit_insn (gen_add3_const (r12, r12, alloc_lo));
      else
	emit_insn (gen_nop ());
    }
  else
    {
      emit_insn (gen_add3_const (r12, r1, alloc_lo));
      emit_insn (gen_nop ());
    }

  compare = gen_rtx_REG (CCUNSmode, CR7_REGNO);
  emit_insn (gen_rtx_SET (compare, gen_rtx_COMPARE (CCUNSmode, r12, r0)));
  ok_label = gen_label_rtx ();
  jump = gen_rtx_IF_THEN_ELSE (VOIDmode,
			       gen_rtx_GEU (VOIDmode, compare, const0_rtx),
			       gen_rtx_LABEL_REF (VOIDmode, ok_label),
			       pc_rtx);
  insn = emit_jump_insn (gen_rtx_SET (pc_rtx, jump));
  JUMP_LABEL (insn) = ok_label;
  /* Mark the jump as very likely to be taken.  */
  add_reg_br_prob_note (insn, profile_probability::very_likely ());

  lr = gen_rtx_REG (Pmode, LR_REGNO);
  insn = emit_move_insn (r0, lr);
  RTX_FRAME_RELATED_P (insn) = 1;
  insn = emit_insn (gen_frame_store (r0, r1, info->lr_save_offset));
  RTX_FRAME_RELATED_P (insn) = 1;

  insn = emit_call_insn (gen_call (gen_rtx_MEM (SImode, morestack_ref),
				   const0_rtx, const0_rtx));
  call_fusage = NULL_RTX;
  use_reg (&call_fusage, r12);
  /* Say the call uses r0, even though it doesn't, to stop regrename
     from twiddling with the insns saving lr, trashing args for cfun.
     The insns restoring lr are similarly protected by making
     split_stack_return use r0.  */
  use_reg (&call_fusage, r0);
  add_function_usage_to (insn, call_fusage);
  /* Indicate that this function can't jump to non-local gotos.  */
  make_reg_eh_region_note_nothrow_nononlocal (insn);
  emit_insn (gen_frame_load (r0, r1, info->lr_save_offset));
  insn = emit_move_insn (lr, r0);
  add_reg_note (insn, REG_CFA_RESTORE, lr);
  RTX_FRAME_RELATED_P (insn) = 1;
  emit_insn (gen_split_stack_return ());

  emit_label (ok_label);
  LABEL_NUSES (ok_label) = 1;
}

/* We may have to tell the dataflow pass that the split stack prologue
   is initializing a register.  */

void
rs6000_live_on_entry (bitmap regs)
{
  if (flag_split_stack)
    bitmap_set_bit (regs, 12);
}

/* Emit -fsplit-stack dynamic stack allocation space check.  */

void
rs6000_split_stack_space_check (rtx size, rtx label)
{
  rtx sp = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx limit = gen_reg_rtx (Pmode);
  rtx requested = gen_reg_rtx (Pmode);
  rtx cmp = gen_reg_rtx (CCUNSmode);
  rtx jump;

  emit_insn (gen_load_split_stack_limit (limit));
  if (CONST_INT_P (size))
    emit_insn (gen_add3_insn (requested, sp, GEN_INT (-INTVAL (size))));
  else
    {
      size = force_reg (Pmode, size);
      emit_move_insn (requested, gen_rtx_MINUS (Pmode, sp, size));
    }
  emit_insn (gen_rtx_SET (cmp, gen_rtx_COMPARE (CCUNSmode, requested, limit)));
  jump = gen_rtx_IF_THEN_ELSE (VOIDmode,
			       gen_rtx_GEU (VOIDmode, cmp, const0_rtx),
			       gen_rtx_LABEL_REF (VOIDmode, label),
			       pc_rtx);
  jump = emit_jump_insn (gen_rtx_SET (pc_rtx, jump));
  JUMP_LABEL (jump) = label;
}


/* Return whether we need to always update the saved TOC pointer when we update
   the stack pointer.  */

static bool
rs6000_save_toc_in_prologue_p (void)
{
  return (cfun && cfun->machine && cfun->machine->save_toc_in_prologue);
}

#include "gt-rs6000-logue.h"
