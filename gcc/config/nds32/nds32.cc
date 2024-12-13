/* Subroutines used for code generation of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2024 Free Software Foundation, Inc.
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

/* ------------------------------------------------------------------------ */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "tree-pass.h"
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "optabs.h"		/* For GEN_FCN.  */
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "output.h"
#include "explow.h"
#include "expr.h"
#include "tm-constrs.h"
#include "builtins.h"
#include "cpplib.h"
#include "context.h"

/* This file should be included last.  */
#include "target-def.h"

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

/* Define intrinsic register names.
   Please refer to nds32_intrinsic.h file, the index is corresponding to
   'enum nds32_intrinsic_registers' data type values.
   NOTE that the base value starting from 1024.  */
static const char * const nds32_intrinsic_register_names[] =
{
  "$CPU_VER",
  "$ICM_CFG",
  "$DCM_CFG",
  "$MMU_CFG",
  "$MSC_CFG",
  "$MSC_CFG2",
  "$CORE_ID",
  "$FUCOP_EXIST",

  "$PSW",
  "$IPSW",
  "$P_IPSW",
  "$IVB",
  "$EVA",
  "$P_EVA",
  "$ITYPE",
  "$P_ITYPE",

  "$MERR",
  "$IPC",
  "$P_IPC",
  "$OIPC",
  "$P_P0",
  "$P_P1",

  "$INT_MASK",
  "$INT_MASK2",
  "$INT_MASK3",
  "$INT_PEND",
  "$INT_PEND2",
  "$INT_PEND3",
  "$SP_USR",
  "$SP_PRIV",
  "$INT_PRI",
  "$INT_PRI2",
  "$INT_PRI3",
  "$INT_PRI4",
  "$INT_CTRL",
  "$INT_TRIGGER",
  "$INT_TRIGGER2",
  "$INT_GPR_PUSH_DIS",

  "$MMU_CTL",
  "$L1_PPTB",
  "$TLB_VPN",
  "$TLB_DATA",
  "$TLB_MISC",
  "$VLPT_IDX",
  "$ILMB",
  "$DLMB",

  "$CACHE_CTL",
  "$HSMP_SADDR",
  "$HSMP_EADDR",
  "$SDZ_CTL",
  "$N12MISC_CTL",
  "$MISC_CTL",
  "$ECC_MISC",

  "$BPC0",
  "$BPC1",
  "$BPC2",
  "$BPC3",
  "$BPC4",
  "$BPC5",
  "$BPC6",
  "$BPC7",

  "$BPA0",
  "$BPA1",
  "$BPA2",
  "$BPA3",
  "$BPA4",
  "$BPA5",
  "$BPA6",
  "$BPA7",

  "$BPAM0",
  "$BPAM1",
  "$BPAM2",
  "$BPAM3",
  "$BPAM4",
  "$BPAM5",
  "$BPAM6",
  "$BPAM7",

  "$BPV0",
  "$BPV1",
  "$BPV2",
  "$BPV3",
  "$BPV4",
  "$BPV5",
  "$BPV6",
  "$BPV7",

  "$BPCID0",
  "$BPCID1",
  "$BPCID2",
  "$BPCID3",
  "$BPCID4",
  "$BPCID5",
  "$BPCID6",
  "$BPCID7",

  "$EDM_CFG",
  "$EDMSW",
  "$EDM_CTL",
  "$EDM_DTR",
  "$BPMTC",
  "$DIMBR",

  "$TECR0",
  "$TECR1",
  "$PFMC0",
  "$PFMC1",
  "$PFMC2",
  "$PFM_CTL",
  "$PFT_CTL",
  "$HSP_CTL",
  "$SP_BOUND",
  "$SP_BOUND_PRIV",
  "$SP_BASE",
  "$SP_BASE_PRIV",
  "$FUCOP_CTL",
  "$PRUSR_ACC_CTL",

  "$DMA_CFG",
  "$DMA_GCSW",
  "$DMA_CHNSEL",
  "$DMA_ACT",
  "$DMA_SETUP",
  "$DMA_ISADDR",
  "$DMA_ESADDR",
  "$DMA_TCNT",
  "$DMA_STATUS",
  "$DMA_2DSET",
  "$DMA_2DSCTL",
  "$DMA_RCNT",
  "$DMA_HSTATUS",

  "$PC",
  "$SP_USR1",
  "$SP_USR2",
  "$SP_USR3",
  "$SP_PRIV1",
  "$SP_PRIV2",
  "$SP_PRIV3",
  "$BG_REGION",
  "$SFCR",
  "$SIGN",
  "$ISIGN",
  "$P_ISIGN",
  "$IFC_LP",
  "$ITB"
};

/* Define instrinsic cctl names.  */
static const char * const nds32_cctl_names[] =
{
  "L1D_VA_FILLCK",
  "L1D_VA_ULCK",
  "L1I_VA_FILLCK",
  "L1I_VA_ULCK",

  "L1D_IX_WBINVAL",
  "L1D_IX_INVAL",
  "L1D_IX_WB",
  "L1I_IX_INVAL",

  "L1D_VA_INVAL",
  "L1D_VA_WB",
  "L1D_VA_WBINVAL",
  "L1I_VA_INVAL",

  "L1D_IX_RTAG",
  "L1D_IX_RWD",
  "L1I_IX_RTAG",
  "L1I_IX_RWD",

  "L1D_IX_WTAG",
  "L1D_IX_WWD",
  "L1I_IX_WTAG",
  "L1I_IX_WWD"
};

static const char * const nds32_dpref_names[] =
{
  "SRD",
  "MRD",
  "SWR",
  "MWR",
  "PTE",
  "CLWR"
};

/* Defining register allocation order for performance.
   We want to allocate callee-saved registers after others.
   It may be used by nds32_adjust_reg_alloc_order().  */
static const int nds32_reg_alloc_order_for_speed[] =
{
   0,   1,   2,   3,   4,   5,  16,  17,
  18,  19,  20,  21,  22,  23,  24,  25,
  26,  27,   6,   7,   8,   9,  10,  11,
  12,  13,  14,  15
};

/* Defining target-specific uses of __attribute__.  */
TARGET_GNU_ATTRIBUTES (nds32_attribute_table,
{
  /* Syntax: { name, min_len, max_len, decl_required, type_required,
	       function_type_required, affects_type_identity, handler,
	       exclude } */

  /* The interrupt vid: [0-63]+ (actual vector number starts from 9 to 72).  */
  { "interrupt",    1, 64, false, false, false, false, NULL, NULL },
  /* The exception vid: [1-8]+  (actual vector number starts from 1 to 8).  */
  { "exception",    1,  8, false, false, false, false, NULL, NULL },
  /* Argument is user's interrupt numbers.  The vector number is always 0.  */
  { "reset",        1,  1, false, false, false, false, NULL, NULL },

  /* The attributes describing isr nested type.  */
  { "nested",       0,  0, false, false, false, false, NULL, NULL },
  { "not_nested",   0,  0, false, false, false, false, NULL, NULL },
  { "nested_ready", 0,  0, false, false, false, false, NULL, NULL },
  { "critical",     0,  0, false, false, false, false, NULL, NULL },

  /* The attributes describing isr register save scheme.  */
  { "save_all",     0,  0, false, false, false, false, NULL, NULL },
  { "partial_save", 0,  0, false, false, false, false, NULL, NULL },

  /* The attributes used by reset attribute.  */
  { "nmi",          1,  1, false, false, false, false, NULL, NULL },
  { "warm",         1,  1, false, false, false, false, NULL, NULL },

  /* The attributes describing isr security level. */
  { "secure",       1,  1, false, false, false, false, NULL, NULL },

  /* The attribute telling no prologue/epilogue.  */
  { "naked",        0,  0, false, false, false, false, NULL, NULL },

  /* The attribute is used to tell this function to be ROM patch.  */
  { "indirect_call",0,  0, false, false, false, false, NULL, NULL },

  /* FOR BACKWARD COMPATIBILITY,
     this attribute also tells no prologue/epilogue.  */
  { "no_prologue",  0,  0, false, false, false, false, NULL, NULL }
});


/* ------------------------------------------------------------------------ */

/* PART 2: Auxiliary static function definitions.  */

/* Function to save and restore machine-specific function data.  */
static struct machine_function *
nds32_init_machine_status (void)
{
  struct machine_function *machine;
  machine = ggc_cleared_alloc<machine_function> ();

  /* Initially assume this function does not use __builtin_eh_return.  */
  machine->use_eh_return_p = 0;

  /* Initially assume this function needs prologue/epilogue.  */
  machine->naked_p = 0;

  /* Initially assume this function does NOT use fp_as_gp optimization.  */
  machine->fp_as_gp_p = 0;

  /* Initially this function is not under strictly aligned situation.  */
  machine->strict_aligned_p = 0;

  /* Initially this function has no naked and no_prologue attributes.  */
  machine->attr_naked_p = 0;
  machine->attr_no_prologue_p = 0;

  return machine;
}

/* Function to compute stack frame size and
   store into cfun->machine structure.  */
static void
nds32_compute_stack_frame (void)
{
  int r;
  int block_size;
  bool v3pushpop_p;

  /* Because nds32_compute_stack_frame() will be called from different place,
     everytime we enter this function, we have to assume this function
     needs prologue/epilogue.  */
  cfun->machine->naked_p = 0;

  /* We need to mark whether this function has naked and no_prologue
     attribute so that we can distinguish the difference if users applies
     -mret-in-naked-func option.  */
  cfun->machine->attr_naked_p
    = lookup_attribute ("naked", DECL_ATTRIBUTES (current_function_decl))
      ? 1 : 0;
  cfun->machine->attr_no_prologue_p
    = lookup_attribute ("no_prologue", DECL_ATTRIBUTES (current_function_decl))
      ? 1 : 0;

  /* If __builtin_eh_return is used, we better have frame pointer needed
     so that we can easily locate the stack slot of return address.  */
  if (crtl->calls_eh_return)
    {
      frame_pointer_needed = 1;

      /* We need to mark eh data registers that need to be saved
	 in the stack.  */
      cfun->machine->eh_return_data_first_regno = EH_RETURN_DATA_REGNO (0);
      for (r = 0; EH_RETURN_DATA_REGNO (r) != INVALID_REGNUM; r++)
	cfun->machine->eh_return_data_last_regno = r;

      cfun->machine->eh_return_data_regs_size
	= 4 * (cfun->machine->eh_return_data_last_regno
	       - cfun->machine->eh_return_data_first_regno
	       + 1);
      cfun->machine->use_eh_return_p = 1;
    }
  else
    {
      /* Assigning SP_REGNUM to eh_first_regno and eh_last_regno means we
	 do not need to handle __builtin_eh_return case in this function.  */
      cfun->machine->eh_return_data_first_regno = SP_REGNUM;
      cfun->machine->eh_return_data_last_regno  = SP_REGNUM;

      cfun->machine->eh_return_data_regs_size = 0;
      cfun->machine->use_eh_return_p = 0;
    }

  /* Get variadic arguments size to prepare pretend arguments and
     we will push them into stack at prologue by ourself.  */
  cfun->machine->va_args_size = crtl->args.pretend_args_size;
  if (cfun->machine->va_args_size != 0)
    {
      cfun->machine->va_args_first_regno
	= NDS32_GPR_ARG_FIRST_REGNUM
	  + NDS32_MAX_GPR_REGS_FOR_ARGS
	  - (crtl->args.pretend_args_size / UNITS_PER_WORD);
      cfun->machine->va_args_last_regno
	= NDS32_GPR_ARG_FIRST_REGNUM + NDS32_MAX_GPR_REGS_FOR_ARGS - 1;
    }
  else
    {
      cfun->machine->va_args_first_regno = SP_REGNUM;
      cfun->machine->va_args_last_regno  = SP_REGNUM;
    }

  /* Important: We need to make sure that varargs area is 8-byte alignment.  */
  block_size = cfun->machine->va_args_size;
  if (!NDS32_DOUBLE_WORD_ALIGN_P (block_size))
    {
      cfun->machine->va_args_area_padding_bytes
	= NDS32_ROUND_UP_DOUBLE_WORD (block_size) - block_size;
    }

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
  cfun->machine->gp_size =
    (flag_pic && df_regs_ever_live_p (PIC_OFFSET_TABLE_REGNUM)) ? 4 : 0;

  /* If $lp value is required to be saved on stack, it needs 4 bytes space.
     Check whether $lp is ever live.  */
  cfun->machine->lp_size
    = (flag_always_save_lp || df_regs_ever_live_p (LP_REGNUM)) ? 4 : 0;

  /* Initially there is no padding bytes.  */
  cfun->machine->callee_saved_area_gpr_padding_bytes = 0;

  /* Calculate the bytes of saving callee-saved registers on stack.  */
  cfun->machine->callee_saved_gpr_regs_size = 0;
  cfun->machine->callee_saved_first_gpr_regno = SP_REGNUM;
  cfun->machine->callee_saved_last_gpr_regno  = SP_REGNUM;
  cfun->machine->callee_saved_fpr_regs_size = 0;
  cfun->machine->callee_saved_first_fpr_regno = SP_REGNUM;
  cfun->machine->callee_saved_last_fpr_regno  = SP_REGNUM;

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
	  if (cfun->machine->callee_saved_first_gpr_regno == SP_REGNUM)
	    cfun->machine->callee_saved_first_gpr_regno = r;
	  /* Mark the last required callee-saved register.  */
	  cfun->machine->callee_saved_last_gpr_regno = r;
	}
    }

  /* Recording fpu callee-saved register.  */
  if (TARGET_HARD_FLOAT)
    {
      for (r = NDS32_FIRST_FPR_REGNUM; r < NDS32_LAST_FPR_REGNUM; r++)
	{
	  if (NDS32_REQUIRED_CALLEE_SAVED_P (r))
	    {
	      /* Mark the first required callee-saved register.  */
	      if (cfun->machine->callee_saved_first_fpr_regno == SP_REGNUM)
		{
		  /* Make first callee-saved number is even,
		     bacause we use doubleword access, and this way
		     promise 8-byte alignemt.  */
		  if (!NDS32_FPR_REGNO_OK_FOR_DOUBLE (r))
		    cfun->machine->callee_saved_first_fpr_regno = r - 1;
		  else
		    cfun->machine->callee_saved_first_fpr_regno = r;
		}
	      cfun->machine->callee_saved_last_fpr_regno = r;
	    }
	}

      /* Make last callee-saved register number is odd,
	 we hope callee-saved register is even.  */
      int last_fpr = cfun->machine->callee_saved_last_fpr_regno;
      if (NDS32_FPR_REGNO_OK_FOR_DOUBLE (last_fpr))
	cfun->machine->callee_saved_last_fpr_regno++;
    }

  /* Check if this function can omit prologue/epilogue code fragment.
     If there is 'no_prologue'/'naked'/'secure' attribute in this function,
     we can set 'naked_p' flag to indicate that
     we do not have to generate prologue/epilogue.
     Or, if all the following conditions succeed,
     we can set this function 'naked_p' as well:
       condition 1: first_regno == last_regno == SP_REGNUM,
		    which means we do not have to save
		    any callee-saved registers.
       condition 2: Both $lp and $fp are NOT live in this function,
		    which means we do not need to save them and there
		    is no outgoing size.
       condition 3: There is no local_size, which means
		    we do not need to adjust $sp.  */
  if (lookup_attribute ("no_prologue", DECL_ATTRIBUTES (current_function_decl))
      || lookup_attribute ("naked", DECL_ATTRIBUTES (current_function_decl))
      || lookup_attribute ("secure", DECL_ATTRIBUTES (current_function_decl))
      || (cfun->machine->callee_saved_first_gpr_regno == SP_REGNUM
	  && cfun->machine->callee_saved_last_gpr_regno == SP_REGNUM
	  && cfun->machine->callee_saved_first_fpr_regno == SP_REGNUM
	  && cfun->machine->callee_saved_last_fpr_regno == SP_REGNUM
	  && !df_regs_ever_live_p (FP_REGNUM)
	  && !df_regs_ever_live_p (LP_REGNUM)
	  && cfun->machine->local_size == 0
	  && !flag_pic))
    {
      /* Set this function 'naked_p' and other functions can check this flag.
	 Note that in nds32 port, the 'naked_p = 1' JUST means there is no
	 callee-saved, local size, and outgoing size.
	 The varargs space and ret instruction may still present in
	 the prologue/epilogue expanding.  */
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

  v3pushpop_p = NDS32_V3PUSH_AVAILABLE_P;

  /* Adjustment for v3push instructions:
     If we are using v3push (push25/pop25) instructions,
     we need to make sure Rb is $r6 and Re is
     located on $r6, $r8, $r10, or $r14.
     Some results above will be discarded and recomputed.
     Note that it is only available under V3/V3M ISA and we
     DO NOT setup following stuff for isr or variadic function.  */
  if (v3pushpop_p)
    {
      /* Recompute:
	   cfun->machine->fp_size
	   cfun->machine->gp_size
	   cfun->machine->lp_size
	   cfun->machine->callee_saved_first_gpr_regno
	   cfun->machine->callee_saved_last_gpr_regno */

      /* For v3push instructions, $fp, $gp, and $lp are always saved.  */
      cfun->machine->fp_size = 4;
      cfun->machine->gp_size = 4;
      cfun->machine->lp_size = 4;

      /* Remember to set Rb = $r6.  */
      cfun->machine->callee_saved_first_gpr_regno = 6;

      if (cfun->machine->callee_saved_last_gpr_regno <= 6)
	{
	  /* Re = $r6 */
	  cfun->machine->callee_saved_last_gpr_regno = 6;
	}
      else if (cfun->machine->callee_saved_last_gpr_regno <= 8)
	{
	  /* Re = $r8 */
	  cfun->machine->callee_saved_last_gpr_regno = 8;
	}
      else if (cfun->machine->callee_saved_last_gpr_regno <= 10)
	{
	  /* Re = $r10 */
	  cfun->machine->callee_saved_last_gpr_regno = 10;
	}
      else if (cfun->machine->callee_saved_last_gpr_regno <= 14)
	{
	  /* Re = $r14 */
	  cfun->machine->callee_saved_last_gpr_regno = 14;
	}
      else if (cfun->machine->callee_saved_last_gpr_regno == SP_REGNUM)
	{
	  /* If last_regno is SP_REGNUM, which means
	     it is never changed, so set it to Re = $r6.  */
	  cfun->machine->callee_saved_last_gpr_regno = 6;
	}
      else
	{
	  /* The program flow should not go here.  */
	  gcc_unreachable ();
	}
    }

  int sp_adjust = cfun->machine->local_size
		  + cfun->machine->out_args_size
		  + cfun->machine->callee_saved_area_gpr_padding_bytes
		  + cfun->machine->callee_saved_fpr_regs_size;

  if (!v3pushpop_p
      && sp_adjust == 0
      && !frame_pointer_needed)
    {
      block_size = cfun->machine->fp_size
		   + cfun->machine->gp_size
		   + cfun->machine->lp_size;

      if (cfun->machine->callee_saved_last_gpr_regno != SP_REGNUM)
	block_size += (4 * (cfun->machine->callee_saved_last_gpr_regno
			    - cfun->machine->callee_saved_first_gpr_regno
			    + 1));

      if (!NDS32_DOUBLE_WORD_ALIGN_P (block_size))
	{
	  /* $r14 is last callee save register.  */
	  if (cfun->machine->callee_saved_last_gpr_regno
	      < NDS32_LAST_CALLEE_SAVE_GPR_REGNUM)
	    {
	      cfun->machine->callee_saved_last_gpr_regno++;
	    }
	  else if (cfun->machine->callee_saved_first_gpr_regno == SP_REGNUM)
	    {
	      cfun->machine->callee_saved_first_gpr_regno
		= NDS32_FIRST_CALLEE_SAVE_GPR_REGNUM;
	      cfun->machine->callee_saved_last_gpr_regno
		= NDS32_FIRST_CALLEE_SAVE_GPR_REGNUM;
	    }
	}
    }

  /* We have correctly set callee_saved_first_gpr_regno
     and callee_saved_last_gpr_regno.
     Initially, the callee_saved_gpr_regs_size is supposed to be 0.
     As long as callee_saved_last_gpr_regno is not SP_REGNUM,
     we can update callee_saved_gpr_regs_size with new size.  */
  if (cfun->machine->callee_saved_last_gpr_regno != SP_REGNUM)
    {
      /* Compute pushed size of callee-saved registers.  */
      cfun->machine->callee_saved_gpr_regs_size
	= 4 * (cfun->machine->callee_saved_last_gpr_regno
	       - cfun->machine->callee_saved_first_gpr_regno
	       + 1);
    }

  if (TARGET_HARD_FLOAT)
    {
      /* Compute size of callee svaed floating-point registers.  */
      if (cfun->machine->callee_saved_last_fpr_regno != SP_REGNUM)
	{
	  cfun->machine->callee_saved_fpr_regs_size
	   = 4 * (cfun->machine->callee_saved_last_fpr_regno
		  - cfun->machine->callee_saved_first_fpr_regno
		  + 1);
	}
    }

  /* Important: We need to make sure that
		(fp_size + gp_size + lp_size + callee_saved_gpr_regs_size)
		is 8-byte alignment.
		If it is not, calculate the padding bytes.  */
  block_size = cfun->machine->fp_size
	       + cfun->machine->gp_size
	       + cfun->machine->lp_size
	       + cfun->machine->callee_saved_gpr_regs_size;
  if (!NDS32_DOUBLE_WORD_ALIGN_P (block_size))
    {
      cfun->machine->callee_saved_area_gpr_padding_bytes
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
static void
nds32_emit_stack_push_multiple (unsigned Rb, unsigned Re,
				bool save_fp_p, bool save_gp_p, bool save_lp_p,
				bool vaarg_p)
{
  unsigned regno;
  int extra_count;
  int num_use_regs;
  int par_index;
  int offset;

  rtx reg;
  rtx mem;
  rtx push_rtx;
  rtx adjust_sp_rtx;
  rtx parallel_insn;
  rtx dwarf;

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
  if (save_fp_p)
    extra_count++;
  if (save_gp_p)
    extra_count++;
  if (save_lp_p)
    extra_count++;
  /* Note that Rb and Re may be SP_REGNUM.  DO NOT count it in.  */
  if (Rb == SP_REGNUM && Re == SP_REGNUM)
    num_use_regs = extra_count;
  else
    num_use_regs = Re - Rb + 1 + extra_count;

  /* In addition to used registers,
     we need one more space for (set sp sp-x) rtx.  */
  parallel_insn = gen_rtx_PARALLEL (VOIDmode,
				    rtvec_alloc (num_use_regs + 1));
  par_index = 0;

  /* Initialize offset and start to create push behavior.  */
  offset = -(num_use_regs * 4);

  /* Create (set mem regX) from Rb, Rb+1 up to Re.  */
  for (regno = Rb; regno <= Re; regno++)
    {
      /* Rb and Re may be SP_REGNUM.
	 We need to break this loop immediately.  */
      if (regno == SP_REGNUM)
	break;

      reg = gen_rtx_REG (SImode, regno);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      push_rtx = gen_rtx_SET (mem, reg);
      XVECEXP (parallel_insn, 0, par_index) = push_rtx;
      RTX_FRAME_RELATED_P (push_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }

  /* Create (set mem fp), (set mem gp), and (set mem lp) if necessary.  */
  if (save_fp_p)
    {
      reg = gen_rtx_REG (SImode, FP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      push_rtx = gen_rtx_SET (mem, reg);
      XVECEXP (parallel_insn, 0, par_index) = push_rtx;
      RTX_FRAME_RELATED_P (push_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }
  if (save_gp_p)
    {
      reg = gen_rtx_REG (SImode, GP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      push_rtx = gen_rtx_SET (mem, reg);
      XVECEXP (parallel_insn, 0, par_index) = push_rtx;
      RTX_FRAME_RELATED_P (push_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }
  if (save_lp_p)
    {
      reg = gen_rtx_REG (SImode, LP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      push_rtx = gen_rtx_SET (mem, reg);
      XVECEXP (parallel_insn, 0, par_index) = push_rtx;
      RTX_FRAME_RELATED_P (push_rtx) = 1;
      offset = offset + 4;
      par_index++;
    }

  /* Create (set sp sp-x).  */

  /* We need to re-calculate the offset value again for adjustment.  */
  offset = -(num_use_regs * 4);
  adjust_sp_rtx
    = gen_rtx_SET (stack_pointer_rtx,
		   plus_constant (Pmode, stack_pointer_rtx, offset));
  XVECEXP (parallel_insn, 0, par_index) = adjust_sp_rtx;
  RTX_FRAME_RELATED_P (adjust_sp_rtx) = 1;

  parallel_insn = emit_insn (parallel_insn);

  /* The insn rtx 'parallel_insn' will change frame layout.
     We need to use RTX_FRAME_RELATED_P so that GCC is able to
     generate CFI (Call Frame Information) stuff.  */
  RTX_FRAME_RELATED_P (parallel_insn) = 1;

  /* Don't use GCC's logic for CFI info if we are generate a push for VAARG
     since we will not restore those register at epilogue.  */
  if (vaarg_p)
    {
      dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA,
			      copy_rtx (adjust_sp_rtx), NULL_RTX);
      REG_NOTES (parallel_insn) = dwarf;
    }
}

/* Function to create a parallel rtx pattern
   which presents stack pop multiple behavior.
   The overall concept are:
     "pop registers from memory",
     "adjust stack pointer".  */
static void
nds32_emit_stack_pop_multiple (unsigned Rb, unsigned Re,
			       bool save_fp_p, bool save_gp_p, bool save_lp_p)
{
  unsigned regno;
  int extra_count;
  int num_use_regs;
  int par_index;
  int offset;

  rtx reg;
  rtx mem;
  rtx pop_rtx;
  rtx adjust_sp_rtx;
  rtx parallel_insn;
  rtx dwarf = NULL_RTX;

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
  if (save_fp_p)
    extra_count++;
  if (save_gp_p)
    extra_count++;
  if (save_lp_p)
    extra_count++;
  /* Note that Rb and Re may be SP_REGNUM.  DO NOT count it in.  */
  if (Rb == SP_REGNUM && Re == SP_REGNUM)
    num_use_regs = extra_count;
  else
    num_use_regs = Re - Rb + 1 + extra_count;

  /* In addition to used registers,
     we need one more space for (set sp sp+x) rtx.  */
  parallel_insn = gen_rtx_PARALLEL (VOIDmode,
				    rtvec_alloc (num_use_regs + 1));
  par_index = 0;

  /* Initialize offset and start to create pop behavior.  */
  offset = 0;

  /* Create (set regX mem) from Rb, Rb+1 up to Re.  */
  for (regno = Rb; regno <= Re; regno++)
    {
      /* Rb and Re may be SP_REGNUM.
	 We need to break this loop immediately.  */
      if (regno == SP_REGNUM)
	break;

      reg = gen_rtx_REG (SImode, regno);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      pop_rtx = gen_rtx_SET (reg, mem);
      XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
      RTX_FRAME_RELATED_P (pop_rtx) = 1;
      offset = offset + 4;
      par_index++;

      dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);
    }

  /* Create (set fp mem), (set gp mem), and (set lp mem) if necessary.  */
  if (save_fp_p)
    {
      reg = gen_rtx_REG (SImode, FP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      pop_rtx = gen_rtx_SET (reg, mem);
      XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
      RTX_FRAME_RELATED_P (pop_rtx) = 1;
      offset = offset + 4;
      par_index++;

      dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);
    }
  if (save_gp_p)
    {
      reg = gen_rtx_REG (SImode, GP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      pop_rtx = gen_rtx_SET (reg, mem);
      XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
      RTX_FRAME_RELATED_P (pop_rtx) = 1;
      offset = offset + 4;
      par_index++;

      dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);
    }
  if (save_lp_p)
    {
      reg = gen_rtx_REG (SImode, LP_REGNUM);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      pop_rtx = gen_rtx_SET (reg, mem);
      XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
      RTX_FRAME_RELATED_P (pop_rtx) = 1;
      offset = offset + 4;
      par_index++;

      dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);
    }

  /* Create (set sp sp+x).  */

  /* The offset value is already in place.  No need to re-calculate it.  */
  adjust_sp_rtx
    = gen_rtx_SET (stack_pointer_rtx,
		   plus_constant (Pmode, stack_pointer_rtx, offset));
  XVECEXP (parallel_insn, 0, par_index) = adjust_sp_rtx;

  /* Tell gcc we adjust SP in this insn.  */
  dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA, copy_rtx (adjust_sp_rtx), dwarf);

  parallel_insn = emit_insn (parallel_insn);

  /* The insn rtx 'parallel_insn' will change frame layout.
     We need to use RTX_FRAME_RELATED_P so that GCC is able to
     generate CFI (Call Frame Information) stuff.  */
  RTX_FRAME_RELATED_P (parallel_insn) = 1;

  /* Add CFI info by manual.  */
  REG_NOTES (parallel_insn) = dwarf;
}

/* Function to create a parallel rtx pattern
   which presents stack v3push behavior.
   The overall concept are:
     "push registers to memory",
     "adjust stack pointer".  */
static void
nds32_emit_stack_v3push (unsigned Rb,
			 unsigned Re,
			 unsigned imm8u)
{
  unsigned regno;
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
		     (plus (reg:SI SP_REGNUM) (const_int -32-imm8u)))]) */

  /* Calculate the number of registers that will be pushed.
     Since $fp, $gp, and $lp is always pushed with v3push instruction,
     we need to count these three registers.
     Under v3push, Rb is $r6, while Re is $r6, $r8, $r10, or $r14.
     So there is no need to worry about Rb=Re=SP_REGNUM case.  */
  num_use_regs = Re - Rb + 1 + 3;

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
  for (regno = Rb; regno <= Re; regno++)
    {
      reg = gen_rtx_REG (SImode, regno);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      push_rtx = gen_rtx_SET (mem, reg);
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
  push_rtx = gen_rtx_SET (mem, reg);
  XVECEXP (parallel_insn, 0, par_index) = push_rtx;
  RTX_FRAME_RELATED_P (push_rtx) = 1;
  offset = offset + 4;
  par_index++;
  /* Create (set mem gp).  */
  reg = gen_rtx_REG (SImode, GP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  push_rtx = gen_rtx_SET (mem, reg);
  XVECEXP (parallel_insn, 0, par_index) = push_rtx;
  RTX_FRAME_RELATED_P (push_rtx) = 1;
  offset = offset + 4;
  par_index++;
  /* Create (set mem lp).  */
  reg = gen_rtx_REG (SImode, LP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  push_rtx = gen_rtx_SET (mem, reg);
  XVECEXP (parallel_insn, 0, par_index) = push_rtx;
  RTX_FRAME_RELATED_P (push_rtx) = 1;
  offset = offset + 4;
  par_index++;

  /* Create (set sp sp-x-imm8u).  */

  /* We need to re-calculate the offset value again for adjustment.  */
  offset = -(num_use_regs * 4);
  adjust_sp_rtx
    = gen_rtx_SET (stack_pointer_rtx,
		   plus_constant (Pmode,
				  stack_pointer_rtx,
				  offset - imm8u));
  XVECEXP (parallel_insn, 0, par_index) = adjust_sp_rtx;
  RTX_FRAME_RELATED_P (adjust_sp_rtx) = 1;

  parallel_insn = emit_insn (parallel_insn);

  /* The insn rtx 'parallel_insn' will change frame layout.
     We need to use RTX_FRAME_RELATED_P so that GCC is able to
     generate CFI (Call Frame Information) stuff.  */
  RTX_FRAME_RELATED_P (parallel_insn) = 1;
}

/* Function to create a parallel rtx pattern
   which presents stack v3pop behavior.
   The overall concept are:
     "pop registers from memory",
     "adjust stack pointer".  */
static void
nds32_emit_stack_v3pop (unsigned Rb,
			unsigned Re,
			unsigned imm8u)
{
  unsigned regno;
  int num_use_regs;
  int par_index;
  int offset;

  rtx reg;
  rtx mem;
  rtx pop_rtx;
  rtx adjust_sp_rtx;
  rtx parallel_insn;
  rtx dwarf = NULL_RTX;

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
  num_use_regs = Re - Rb + 1 + 3;

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
  for (regno = Rb; regno <= Re; regno++)
    {
      reg = gen_rtx_REG (SImode, regno);
      mem = gen_frame_mem (SImode, plus_constant (Pmode,
						  stack_pointer_rtx,
						  offset));
      pop_rtx = gen_rtx_SET (reg, mem);
      XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
      RTX_FRAME_RELATED_P (pop_rtx) = 1;
      offset = offset + 4;
      par_index++;

      dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);
    }

  /* Create (set fp mem).  */
  reg = gen_rtx_REG (SImode, FP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  pop_rtx = gen_rtx_SET (reg, mem);
  XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
  RTX_FRAME_RELATED_P (pop_rtx) = 1;
  offset = offset + 4;
  par_index++;
  dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);

  /* Create (set gp mem).  */
  reg = gen_rtx_REG (SImode, GP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  pop_rtx = gen_rtx_SET (reg, mem);
  XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
  RTX_FRAME_RELATED_P (pop_rtx) = 1;
  offset = offset + 4;
  par_index++;
  dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);

  /* Create (set lp mem ).  */
  reg = gen_rtx_REG (SImode, LP_REGNUM);
  mem = gen_frame_mem (SImode, plus_constant (Pmode,
					      stack_pointer_rtx,
					      offset));
  pop_rtx = gen_rtx_SET (reg, mem);
  XVECEXP (parallel_insn, 0, par_index) = pop_rtx;
  RTX_FRAME_RELATED_P (pop_rtx) = 1;
  offset = offset + 4;
  par_index++;
  dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);

  /* Create (set sp sp+x+imm8u).  */

  /* The offset value is already in place.  No need to re-calculate it.  */
  adjust_sp_rtx
    = gen_rtx_SET (stack_pointer_rtx,
		   plus_constant (Pmode,
				  stack_pointer_rtx,
				  offset + imm8u));
  XVECEXP (parallel_insn, 0, par_index) = adjust_sp_rtx;

  if (frame_pointer_needed)
    {
      /* (expr_list:REG_CFA_DEF_CFA (plus:SI (reg/f:SI $sp)
					     (const_int 0))
	 mean reset frame pointer to $sp and reset to offset 0.  */
      rtx cfa_adjust_rtx = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					 const0_rtx);
      dwarf = alloc_reg_note (REG_CFA_DEF_CFA, cfa_adjust_rtx, dwarf);
    }
  else
    {
      /* Tell gcc we adjust SP in this insn.  */
      dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA,
			      copy_rtx (adjust_sp_rtx), dwarf);
    }

  parallel_insn = emit_insn (parallel_insn);

  /* The insn rtx 'parallel_insn' will change frame layout.
     We need to use RTX_FRAME_RELATED_P so that GCC is able to
     generate CFI (Call Frame Information) stuff.  */
  RTX_FRAME_RELATED_P (parallel_insn) = 1;

  /* Add CFI info by manual.  */
  REG_NOTES (parallel_insn) = dwarf;
}

static void
nds32_emit_load_gp (void)
{
  rtx got_symbol, pat;

  /* Initial GLOBAL OFFSET TABLE don't do the scheduling.  */
  emit_insn (gen_blockage ());

  got_symbol = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
  /* sethi $gp, _GLOBAL_OFFSET_TABLE_ -8 */
  pat = gen_rtx_UNSPEC (SImode, gen_rtvec (1, got_symbol), UNSPEC_GOTINIT);
  pat = gen_rtx_CONST (SImode, gen_rtx_PLUS (Pmode, pat, GEN_INT (-8)));
  emit_insn (gen_sethi (pic_offset_table_rtx,pat));

  /* ori $gp, $gp, _GLOBAL_OFFSET_TABLE_ -4 */
  pat = gen_rtx_UNSPEC (SImode, gen_rtvec (1, got_symbol), UNSPEC_GOTINIT);
  pat = gen_rtx_CONST (SImode, gen_rtx_PLUS (Pmode, pat, GEN_INT (-4)));
  emit_insn (gen_lo_sum (pic_offset_table_rtx, pic_offset_table_rtx, pat));

  /* add5.pc $gp */
  emit_insn (gen_add_pc (pic_offset_table_rtx, pic_offset_table_rtx));

  /* Initial GLOBAL OFFSET TABLE don't do the scheduling.  */
  emit_insn (gen_blockage ());
}

/* Function that may creates more instructions
   for large value on adjusting stack pointer.

   In nds32 target, 'addi' can be used for stack pointer
   adjustment in prologue/epilogue stage.
   However, sometimes there are too many local variables so that
   the adjustment value is not able to be fit in the 'addi' instruction.
   One solution is to move value into a register
   and then use 'add' instruction.
   In practice, we use TA_REGNUM ($r15) to accomplish this purpose.  */
static void
nds32_emit_adjust_frame (rtx to_reg, rtx from_reg, int adjust_value)
{
  rtx tmp_reg;
  rtx frame_adjust_insn;
  rtx adjust_value_rtx = GEN_INT (adjust_value);

  if (adjust_value == 0)
    return;

  if (!satisfies_constraint_Is15 (adjust_value_rtx))
    {
      /* The value is not able to fit in single addi instruction.
	 Create more instructions of moving value into a register
	 and then add stack pointer with it.  */

      /* $r15 is going to be temporary register to hold the value.  */
      tmp_reg = gen_rtx_REG (SImode, TA_REGNUM);

      /* Create one more instruction to move value
	 into the temporary register.  */
      emit_move_insn (tmp_reg, adjust_value_rtx);

      /* Create new 'add' rtx.  */
      frame_adjust_insn = gen_addsi3 (to_reg,
				      from_reg,
				      tmp_reg);
      /* Emit rtx into insn list and receive its transformed insn rtx.  */
      frame_adjust_insn = emit_insn (frame_adjust_insn);

      /* Because (tmp_reg <- full_value) may be split into two
	 rtl patterns, we cannot set its RTX_FRAME_RELATED_P.
	 We need to construct another (sp <- sp + full_value)
	 and then insert it into sp_adjust_insn's reg note to
	 represent a frame related expression.
	 GCC knows how to refer it and output debug information.  */

      rtx plus_rtx;
      rtx set_rtx;

      plus_rtx = plus_constant (Pmode, from_reg, adjust_value);
      set_rtx = gen_rtx_SET (to_reg, plus_rtx);
      add_reg_note (frame_adjust_insn, REG_FRAME_RELATED_EXPR, set_rtx);
    }
  else
    {
      /* Generate sp adjustment instruction if and only if sp_adjust != 0.  */
      frame_adjust_insn = gen_addsi3 (to_reg,
				      from_reg,
				      adjust_value_rtx);
      /* Emit rtx into instructions list and receive INSN rtx form.  */
      frame_adjust_insn = emit_insn (frame_adjust_insn);
    }

    /* The insn rtx 'sp_adjust_insn' will change frame layout.
       We need to use RTX_FRAME_RELATED_P so that GCC is able to
       generate CFI (Call Frame Information) stuff.  */
    RTX_FRAME_RELATED_P (frame_adjust_insn) = 1;
}

/* Return true if MODE/TYPE need double word alignment.  */
static bool
nds32_needs_double_word_align (machine_mode mode, const_tree type)
{
  unsigned int align;

  /* Pick up the alignment according to the mode or type.  */
  align = NDS32_MODE_TYPE_ALIGN (mode, type);

  return (align > PARM_BOUNDARY);
}

/* Return true if FUNC is a naked function.  */
bool
nds32_naked_function_p (tree func)
{
  /* FOR BACKWARD COMPATIBILITY,
     we need to support 'no_prologue' attribute as well.  */
  tree t_naked;
  tree t_no_prologue;

  if (TREE_CODE (func) != FUNCTION_DECL)
    abort ();

  /* We have to use lookup_attribute() to check attributes.
     Because attr_naked_p and attr_no_prologue_p are set in
     nds32_compute_stack_frame() and the function has not been
     invoked yet.  */
  t_naked       = lookup_attribute ("naked", DECL_ATTRIBUTES (func));
  t_no_prologue = lookup_attribute ("no_prologue", DECL_ATTRIBUTES (func));

  return ((t_naked != NULL_TREE) || (t_no_prologue != NULL_TREE));
}

/* Function that determine whether a load postincrement is a good thing to use
   for a given mode.  */
bool
nds32_use_load_post_increment (machine_mode mode)
{
  return (GET_MODE_SIZE (mode) <= GET_MODE_SIZE(E_DImode));
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
nds32_legitimate_index_p (machine_mode outer_mode,
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
      switch (GET_MODE_SIZE (outer_mode))
	{
	case 1:
	  /* Further check if the value is legal for the 'outer_mode'.  */
	  if (satisfies_constraint_Is15 (index))
	    return true;
	  break;

	case 2:
	  /* Further check if the value is legal for the 'outer_mode'.  */
	  if (satisfies_constraint_Is16 (index))
	    {
	      /* If it is not under strictly aligned situation,
		 we can return true without checking alignment.  */
	      if (!cfun->machine->strict_aligned_p)
		return true;
	      /* Make sure address is half word alignment.  */
	      else if (NDS32_HALF_WORD_ALIGN_P (INTVAL (index)))
		return true;
	    }
	  break;

	case 4:
	  /* Further check if the value is legal for the 'outer_mode'.  */
	  if (satisfies_constraint_Is17 (index))
	    {
	      if ((TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE))
		{
		  if (!satisfies_constraint_Is14 (index))
		    return false;
		}

	      /* If it is not under strictly aligned situation,
		 we can return true without checking alignment.  */
	      if (!cfun->machine->strict_aligned_p)
		return true;
	      /* Make sure address is word alignment.  */
	      else if (NDS32_SINGLE_WORD_ALIGN_P (INTVAL (index)))
		return true;
	    }
	  break;

	case 8:
	  if (satisfies_constraint_Is17 (gen_int_mode (INTVAL (index) + 4,
						       SImode)))
	    {
	      if ((TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE))
		{
		  if (!satisfies_constraint_Is14 (index))
		    return false;
		}

	      /* If it is not under strictly aligned situation,
		 we can return true without checking alignment.  */
	      if (!cfun->machine->strict_aligned_p)
		return true;
	      /* Make sure address is word alignment.
		Currently we do not have 64-bit load/store yet,
		so we will use two 32-bit load/store instructions to do
		memory access and they are single word alignment.  */
	      else if (NDS32_SINGLE_WORD_ALIGN_P (INTVAL (index)))
		return true;
	    }
	  break;

	default:
	  return false;
	}

      return false;

    case MULT:
      op0 = XEXP (index, 0);
      op1 = XEXP (index, 1);

      if (REG_P (op0) && CONST_INT_P (op1))
	{
	  int multiplier;
	  multiplier = INTVAL (op1);

	  /* We only allow (mult reg const_int_1), (mult reg const_int_2),
	     (mult reg const_int_4) or (mult reg const_int_8).  */
	  if (multiplier != 1 && multiplier != 2
	      && multiplier != 4 && multiplier != 8)
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
	     or (ashift reg const_int_1) or (ashift reg const_int_2) or
	     (ashift reg const_int_3).  */
	  if (sv != 0 && sv != 1 && sv !=2 && sv != 3)
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

static void
nds32_register_pass (
  rtl_opt_pass *(*make_pass_func) (gcc::context *),
  enum pass_positioning_ops pass_pos,
  const char *ref_pass_name)
{
  opt_pass *new_opt_pass = make_pass_func (g);

  struct register_pass_info insert_pass =
    {
      new_opt_pass,	/* pass */
      ref_pass_name,	/* reference_pass_name */
      1,		/* ref_pass_instance_number */
      pass_pos		/* po_op */
    };

  register_pass (&insert_pass);
}

/* This function is called from nds32_option_override ().
   All new passes should be registered here.  */
static void
nds32_register_passes (void)
{
  nds32_register_pass (
    make_pass_nds32_fp_as_gp,
    PASS_POS_INSERT_BEFORE,
    "ira");

  nds32_register_pass (
    make_pass_nds32_relax_opt,
    PASS_POS_INSERT_AFTER,
    "mach");
}

/* ------------------------------------------------------------------------ */

/* PART 3: Implement target hook stuff definitions.  */


/* Computing the Length of an Insn.
   Modifies the length assigned to instruction INSN.
   LEN is the initially computed length of the insn.  */
int
nds32_adjust_insn_length (rtx_insn *insn, int length)
{
  int adjust_value = 0;
  switch (recog_memoized (insn))
    {
    case CODE_FOR_call_internal:
    case CODE_FOR_call_value_internal:
      {
	if (NDS32_ALIGN_P ())
	  {
	    rtx_insn *next_insn = next_active_insn (insn);
	    if (next_insn && get_attr_length (next_insn) != 2)
	      adjust_value += 2;
	  }
	/* We need insert a nop after a noretun function call
	   to prevent software breakpoint corrupt the next function. */
	if (find_reg_note (insn, REG_NORETURN, NULL_RTX))
	  {
	    if (TARGET_16_BIT)
	      adjust_value += 2;
	    else
	      adjust_value += 4;
	  }
      }
      return length + adjust_value;

    default:
      return length;
    }
}

/* Storage Layout.  */

/* This function will be called just before expansion into rtl.  */
static void
nds32_expand_to_rtl_hook (void)
{
  /* We need to set strictly aligned situation.
     After that, the memory address checking in nds32_legitimate_address_p()
     will take alignment offset into consideration so that it will not create
     unaligned [base + offset] access during the rtl optimization.  */
  cfun->machine->strict_aligned_p = 1;
}


/* Register Usage.  */

static void
nds32_conditional_register_usage (void)
{
  int regno;

  if (TARGET_LINUX_ABI)
    fixed_regs[TP_REGNUM] = 1;

  if (TARGET_HARD_FLOAT)
    {
      for (regno = NDS32_FIRST_FPR_REGNUM;
	   regno <= NDS32_LAST_FPR_REGNUM; regno++)
	{
	  fixed_regs[regno] = 0;
	  if (regno < NDS32_FIRST_FPR_REGNUM + NDS32_MAX_FPR_REGS_FOR_ARGS)
	    call_used_regs[regno] = 1;
	  else if (regno >= NDS32_FIRST_FPR_REGNUM + 22
		   && regno < NDS32_FIRST_FPR_REGNUM + 48)
	    call_used_regs[regno] = 1;
	  else
	    call_used_regs[regno] = 0;
	}
    }
  else if (TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE)
    {
      for (regno = NDS32_FIRST_FPR_REGNUM;
	   regno <= NDS32_LAST_FPR_REGNUM;
	   regno++)
	fixed_regs[regno] = 0;
    }
}


/* Register Classes.  */

static unsigned char
nds32_class_max_nregs (reg_class_t rclass ATTRIBUTE_UNUSED,
		       machine_mode mode)
{
  /* Return the maximum number of consecutive registers
     needed to represent "mode" in a register of "rclass".  */
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
}

static int
nds32_register_priority (int hard_regno)
{
  /* Encourage to use r0-r7 for LRA when optimize for size.  */
  if (optimize_size)
    {
      if (hard_regno < 8)
	return 4;
      else if (hard_regno < 16)
	return 3;
      else if (hard_regno < 28)
	return 2;
      else
	return 1;
    }
  else
    {
      if (hard_regno > 27)
	return 1;
      else
	return 4;
    }
}

static bool
nds32_can_change_mode_class (machine_mode from,
			     machine_mode to,
			     reg_class_t rclass)
{
  /* Don't spill double-precision register to two singal-precision
     registers  */
  if ((TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE)
       && GET_MODE_SIZE (from) != GET_MODE_SIZE (to))
    {
      return !reg_classes_intersect_p (rclass, FP_REGS);
    }

  return true;
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

static bool
nds32_can_eliminate (const int from_reg, const int to_reg)
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
nds32_function_arg (cumulative_args_t ca, const function_arg_info &arg)
{
  unsigned int regno;
  CUMULATIVE_ARGS *cum = get_cumulative_args (ca);
  tree type = arg.type;
  machine_mode mode = arg.mode;

  /* The last time this hook is called,
     it is called with an end marker.  */
  if (arg.end_marker_p ())
    return NULL_RTX;

  /* For nameless arguments, we need to take care it individually.  */
  if (!arg.named)
    {
      /* If we are under hard float abi, we have arguments passed on the
	 stack and all situation can be handled by GCC itself.  */
      if (TARGET_HARD_FLOAT)
	return NULL_RTX;

      if (NDS32_ARG_PARTIAL_IN_GPR_REG_P (cum->gpr_offset, mode, type))
	{
	  /* If we still have enough registers to pass argument, pick up
	     next available register number.  */
	  regno
	    = NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (cum->gpr_offset, mode, type);
	  return gen_rtx_REG (mode, regno);
	}

      /* No register available, return NULL_RTX.
	 The compiler will use stack to pass argument instead.  */
      return NULL_RTX;
    }

  /* The following is to handle named argument.
     Note that the strategies of TARGET_HARD_FLOAT and !TARGET_HARD_FLOAT
     are different.  */
  if (TARGET_HARD_FLOAT)
    {
      /* For TARGET_HARD_FLOAT calling convention, we use GPR and FPR
	 to pass argument.  We have to further check TYPE and MODE so
	 that we can determine which kind of register we shall use.  */

      /* Note that we need to pass argument entirely in registers under
	 hard float abi.  */
      if (GET_MODE_CLASS (mode) == MODE_FLOAT
	  && NDS32_ARG_ENTIRE_IN_FPR_REG_P (cum->fpr_offset, mode, type))
	{
	  /* Pick up the next available FPR register number.  */
	  regno
	    = NDS32_AVAILABLE_REGNUM_FOR_FPR_ARG (cum->fpr_offset, mode, type);
	  return gen_rtx_REG (mode, regno);
	}
      else if (GET_MODE_CLASS (mode) != MODE_FLOAT
	       && NDS32_ARG_ENTIRE_IN_GPR_REG_P (cum->gpr_offset, mode, type))
	{
	  /* Pick up the next available GPR register number.  */
	  regno
	    = NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (cum->gpr_offset, mode, type);
	  return gen_rtx_REG (mode, regno);
	}
    }
  else
    {
      /* For !TARGET_HARD_FLOAT calling convention, we always use GPR to pass
	 argument.  Since we allow to pass argument partially in registers,
	 we can just return it if there are still registers available.  */
      if (NDS32_ARG_PARTIAL_IN_GPR_REG_P (cum->gpr_offset, mode, type))
	{
	  /* Pick up the next available register number.  */
	  regno
	    = NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (cum->gpr_offset, mode, type);
	  return gen_rtx_REG (mode, regno);
	}

    }

  /* No register available, return NULL_RTX.
     The compiler will use stack to pass argument instead.  */
  return NULL_RTX;
}

static bool
nds32_must_pass_in_stack (const function_arg_info &arg)
{
  /* Return true if a type must be passed in memory.
     If it is NOT using hard float abi, small aggregates can be
     passed in a register even we are calling a variadic function.
     So there is no need to take padding into consideration.  */
  if (TARGET_HARD_FLOAT)
    return must_pass_in_stack_var_size_or_pad (arg);
  else
    return must_pass_in_stack_var_size (arg);
}

static int
nds32_arg_partial_bytes (cumulative_args_t ca, const function_arg_info &arg)
{
  /* Returns the number of bytes at the beginning of an argument that
     must be put in registers.  The value must be zero for arguments that are
     passed entirely in registers or that are entirely pushed on the stack.
     Besides, TARGET_FUNCTION_ARG for these arguments should return the
     first register to be used by the caller for this argument.  */
  unsigned int needed_reg_count;
  unsigned int remaining_reg_count;
  CUMULATIVE_ARGS *cum;

  cum = get_cumulative_args (ca);

  /* Under hard float abi, we better have argument entirely passed in
     registers or pushed on the stack so that we can reduce the complexity
     of dealing with cum->gpr_offset and cum->fpr_offset.  */
  if (TARGET_HARD_FLOAT)
    return 0;

  /* If we have already runned out of argument registers, return zero
     so that the argument will be entirely pushed on the stack.  */
  if (NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (cum->gpr_offset, arg.mode, arg.type)
      >= NDS32_GPR_ARG_FIRST_REGNUM + NDS32_MAX_GPR_REGS_FOR_ARGS)
    return 0;

  /* Calculate how many registers do we need for this argument.  */
  needed_reg_count = NDS32_NEED_N_REGS_FOR_ARG (arg.mode, arg.type);

  /* Calculate how many argument registers have left for passing argument.
     Note that we should count it from next available register number.  */
  remaining_reg_count
    = NDS32_MAX_GPR_REGS_FOR_ARGS
      - (NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (cum->gpr_offset,
					     arg.mode, arg.type)
	 - NDS32_GPR_ARG_FIRST_REGNUM);

  /* Note that we have to return the nubmer of bytes, not registers count.  */
  if (needed_reg_count > remaining_reg_count)
    return remaining_reg_count * UNITS_PER_WORD;

  return 0;
}

static void
nds32_function_arg_advance (cumulative_args_t ca,
			    const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (ca);
  tree type = arg.type;
  machine_mode mode = arg.mode;

  if (arg.named)
    {
      /* We need to further check TYPE and MODE so that we can determine
	 which kind of register we shall advance.  */

      /* Under hard float abi, we may advance FPR registers.  */
      if (TARGET_HARD_FLOAT && GET_MODE_CLASS (mode) == MODE_FLOAT)
	{
	  cum->fpr_offset
	    = NDS32_AVAILABLE_REGNUM_FOR_FPR_ARG (cum->fpr_offset, mode, type)
	      - NDS32_FPR_ARG_FIRST_REGNUM
	      + NDS32_NEED_N_REGS_FOR_ARG (mode, type);
	}
      else
	{
	  cum->gpr_offset
	    = NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (cum->gpr_offset, mode, type)
	      - NDS32_GPR_ARG_FIRST_REGNUM
	      + NDS32_NEED_N_REGS_FOR_ARG (mode, type);
	}
    }
  else
    {
      /* If this nameless argument is NOT under TARGET_HARD_FLOAT,
	 we can advance next register as well so that caller is
	 able to pass arguments in registers and callee must be
	 in charge of pushing all of them into stack.  */
      if (!TARGET_HARD_FLOAT)
	{
	  cum->gpr_offset
	    = NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (cum->gpr_offset, mode, type)
	      - NDS32_GPR_ARG_FIRST_REGNUM
	      + NDS32_NEED_N_REGS_FOR_ARG (mode, type);
	}
    }
}

static unsigned int
nds32_function_arg_boundary (machine_mode mode, const_tree type)
{
  return (nds32_needs_double_word_align (mode, type)
	  ? NDS32_DOUBLE_WORD_ALIGNMENT
	  : PARM_BOUNDARY);
}

bool
nds32_vector_mode_supported_p (machine_mode mode)
{
  if (mode == V4QImode
      || mode == V2HImode)
    return NDS32_EXT_DSP_P ();

  return false;
}

/* -- How Scalar Function Values Are Returned.  */

static rtx
nds32_function_value (const_tree ret_type,
		      const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		      bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode;
  int unsignedp;

  mode = TYPE_MODE (ret_type);
  unsignedp = TYPE_UNSIGNED (ret_type);

  if (INTEGRAL_TYPE_P (ret_type))
    mode = promote_mode (ret_type, mode, &unsignedp);

  if (TARGET_HARD_FLOAT && (mode == SFmode || mode == DFmode))
    return gen_rtx_REG (mode, NDS32_FPR_RET_FIRST_REGNUM);
  else
    return gen_rtx_REG (mode, NDS32_GPR_RET_FIRST_REGNUM);
}

static rtx
nds32_libcall_value (machine_mode mode,
		     const_rtx fun ATTRIBUTE_UNUSED)
{
  if (TARGET_HARD_FLOAT && (mode == SFmode || mode == DFmode))
    return gen_rtx_REG (mode, NDS32_FPR_RET_FIRST_REGNUM);

  return gen_rtx_REG (mode, NDS32_GPR_RET_FIRST_REGNUM);
}

static bool
nds32_function_value_regno_p (const unsigned int regno)
{
  if (regno == NDS32_GPR_RET_FIRST_REGNUM
      || (TARGET_HARD_FLOAT
	  && regno == NDS32_FPR_RET_FIRST_REGNUM))
    return true;

  return false;
}

/* -- How Large Values Are Returned.  */

static bool
nds32_return_in_memory (const_tree type,
			const_tree fntype ATTRIBUTE_UNUSED)
{
  /* Note that int_size_in_bytes can return -1 if the size can vary
     or is larger than an integer.  */
  HOST_WIDE_INT size = int_size_in_bytes (type);

  /* For COMPLEX_TYPE, if the total size cannot be hold within two registers,
     the return value is supposed to be in memory.  We need to be aware of
     that the size may be -1.  */
  if (TREE_CODE (type) == COMPLEX_TYPE)
    if (size < 0 || size > 2 * UNITS_PER_WORD)
      return true;

  /* If it is BLKmode and the total size cannot be hold within two registers,
     the return value is supposed to be in memory.  We need to be aware of
     that the size may be -1.  */
  if (TYPE_MODE (type) == BLKmode)
    if (size < 0 || size > 2 * UNITS_PER_WORD)
      return true;

  /* For other cases, having result in memory is unnecessary.  */
  return false;
}

/* -- Function Entry and Exit.  */

/* The content produced from this function
   will be placed before prologue body.  */
static void
nds32_asm_function_prologue (FILE *file)
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
  for (r = 0; r < 65; r++)
    {
      if (df_regs_ever_live_p (r))
	fprintf (file, "%s, ", reg_names[r]);
    }
  fputc ('\n', file);

  /* Display the attributes of this function.  */
  fprintf (file, "\t! function attributes: ");
  /* Get the attributes tree list.
     Note that GCC builds attributes list with reverse order.  */
  attrs = DECL_ATTRIBUTES (current_function_decl);

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
}

/* Before rtl epilogue has been expanded, this function is used.  */
static void
nds32_asm_function_begin_epilogue (FILE *file)
{
  fprintf (file, "\t! BEGIN EPILOGUE\n");
}

/* The content produced from this function
   will be placed after epilogue body.  */
static void
nds32_asm_function_epilogue (FILE *file)
{
  fprintf (file, "\t! END EPILOGUE\n");
}

static void
nds32_asm_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
			   HOST_WIDE_INT delta,
			   HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED,
			   tree function)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk));
  int this_regno;

  assemble_start_function (thunk, fnname);
  /* Make sure unwind info is emitted for the thunk if needed.  */
  final_start_function (emit_barrier (), file, 1);

  this_regno = (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function)
		? 1
		: 0);

  if (flag_pic)
    {
      fprintf (file, "\tsmw.adm\t$r31, [$r31], $r31, 4\n");
      fprintf (file, "\tsethi\t%s, hi20(_GLOBAL_OFFSET_TABLE_-8)\n",
		      reg_names [PIC_OFFSET_TABLE_REGNUM]);
      fprintf (file, "\tori\t%s, %s, lo12(_GLOBAL_OFFSET_TABLE_-4)\n",
		      reg_names [PIC_OFFSET_TABLE_REGNUM],
		      reg_names [PIC_OFFSET_TABLE_REGNUM]);

      if (TARGET_ISA_V3)
	fprintf (file, "\tadd5.pc\t$gp\n");
      else
	{
	  fprintf (file, "\tmfusr\t$ta, $pc\n");
	  fprintf (file, "\tadd\t%s, $ta, %s\n",
			  reg_names [PIC_OFFSET_TABLE_REGNUM],
			  reg_names [PIC_OFFSET_TABLE_REGNUM]);
	}
    }

  if (delta != 0)
    {
      if (satisfies_constraint_Is15 (GEN_INT (delta)))
	{
	  fprintf (file, "\taddi\t$r%d, $r%d, " HOST_WIDE_INT_PRINT_DEC "\n",
		   this_regno, this_regno, delta);
	}
      else if (satisfies_constraint_Is20 (GEN_INT (delta)))
	{
	  fprintf (file, "\tmovi\t$ta, " HOST_WIDE_INT_PRINT_DEC "\n", delta);
	  fprintf (file, "\tadd\t$r%d, $r%d, $ta\n", this_regno, this_regno);
	}
      else
	{
	  fprintf (file,
		   "\tsethi\t$ta, hi20(" HOST_WIDE_INT_PRINT_DEC ")\n",
		   delta);
	  fprintf (file,
		   "\tori\t$ta, $ta, lo12(" HOST_WIDE_INT_PRINT_DEC ")\n",
		   delta);
	  fprintf (file, "\tadd\t$r%d, $r%d, $ta\n", this_regno, this_regno);
	}
    }

  if (flag_pic)
    {
      fprintf (file, "\tla\t$ta, ");
      assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
      fprintf (file, "@PLT\n");
      fprintf (file, "\t! epilogue\n");
      fprintf (file, "\tlwi.bi\t%s, [%s], 4\n",
	       reg_names[PIC_OFFSET_TABLE_REGNUM],
	       reg_names[STACK_POINTER_REGNUM]);
      fprintf (file, "\tbr\t$ta\n");
    }
  else
    {
      fprintf (file, "\tb\t");
      assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
      fprintf (file, "\n");
    }

  final_end_function ();
  assemble_end_function (thunk, fnname);
}

/* -- Permitting tail calls.  */

/* Return true if it is ok to do sibling call optimization.  */
static bool
nds32_function_ok_for_sibcall (tree decl,
			       tree exp ATTRIBUTE_UNUSED)
{
  /* The DECL is NULL if it is an indirect call.  */

  /* 1. Do not apply sibling call if -mv3push is enabled,
	because pop25 instruction also represents return behavior.
     2. If this function is a isr function, do not apply sibling call
	because it may perform the behavior that user does not expect.
     3. If this function is a variadic function, do not apply sibling call
	because the stack layout may be a mess.
     4. We don't want to apply sibling call optimization for indirect
	sibcall because the pop behavior in epilogue may pollute the
	content of caller-saved regsiter when the register is used for
	indirect sibcall.
     5. In pic mode, it may use some registers for PLT call.  */
  return (!TARGET_V3PUSH
	  && !nds32_isr_function_p (current_function_decl)
	  && (cfun->machine->va_args_size == 0)
	  && decl
	  && !flag_pic);
}

/* Determine whether we need to enable warning for function return check.  */
static bool
nds32_warn_func_return (tree decl)
{
  /* Naked functions are implemented entirely in assembly, including the
     return sequence, so suppress warnings about this.  */
  return !nds32_naked_function_p (decl);
}


/* Implementing the Varargs Macros.  */

static void
nds32_setup_incoming_varargs (cumulative_args_t ca,
			      const function_arg_info &arg,
			      int *pretend_args_size,
			      int second_time ATTRIBUTE_UNUSED)
{
  unsigned int total_args_regs;
  unsigned int num_of_used_regs;
  unsigned int remaining_reg_count;
  CUMULATIVE_ARGS *cum;

  /* If we are under hard float abi, we do not need to set *pretend_args_size.
     So that all nameless arguments are pushed by caller and all situation
     can be handled by GCC itself.  */
  if (TARGET_HARD_FLOAT)
    return;

  /* We are using NDS32_MAX_GPR_REGS_FOR_ARGS registers,
     counting from NDS32_GPR_ARG_FIRST_REGNUM, for saving incoming arguments.
     However, for nameless(anonymous) arguments, we should push them on the
     stack so that all the nameless arguments appear to have been passed
     consecutively in the memory for accessing.  Hence, we need to check and
     exclude the registers that are used for named arguments.  */

  cum = get_cumulative_args (ca);

  /* ARG describes the last argument.
     We need those information to determine the remaining registers
     for varargs.  */
  total_args_regs
    = NDS32_MAX_GPR_REGS_FOR_ARGS + NDS32_GPR_ARG_FIRST_REGNUM;
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl))
      || arg.type != NULL_TREE)
    num_of_used_regs
      = NDS32_AVAILABLE_REGNUM_FOR_GPR_ARG (cum->gpr_offset, arg.mode, arg.type)
        + NDS32_NEED_N_REGS_FOR_ARG (arg.mode, arg.type);
  else
    num_of_used_regs = cum->gpr_offset + NDS32_GPR_ARG_FIRST_REGNUM;

  remaining_reg_count = total_args_regs - num_of_used_regs;
  *pretend_args_size = remaining_reg_count * UNITS_PER_WORD;

  return;
}

static bool
nds32_strict_argument_naming (cumulative_args_t ca ATTRIBUTE_UNUSED)
{
  /* If this hook returns true, the named argument of FUNCTION_ARG is always
     true for named arguments, and false for unnamed arguments.  */
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
nds32_legitimate_address_p (machine_mode mode, rtx x, bool strict,
			    code_helper = ERROR_MARK)
{
  if (TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE)
    {
     /* When using floating-point instructions,
	we don't allow 'addr' to be [symbol_ref], [CONST] pattern.  */
      if ((mode == DFmode || mode == SFmode)
	  && (GET_CODE (x) == SYMBOL_REF
	  || GET_CODE(x) == CONST))
	return false;

      /* Allow [post_modify] addressing mode, when using FPU instructions.  */
      if (GET_CODE (x) == POST_MODIFY
	  && mode == DFmode)
	{
	  if (GET_CODE (XEXP (x, 0)) == REG
	      && GET_CODE (XEXP (x, 1)) == PLUS)
	    {
	      rtx plus_op = XEXP (x, 1);
	      rtx op0 = XEXP (plus_op, 0);
	      rtx op1 = XEXP (plus_op, 1);

	      if (nds32_address_register_rtx_p (op0, strict)
		  && CONST_INT_P (op1))
		{
		  if (satisfies_constraint_Is14 (op1))
		    {
		      /* If it is not under strictly aligned situation,
			 we can return true without checking alignment.  */
		      if (!cfun->machine->strict_aligned_p)
			return true;
		      /* Make sure address is word alignment.
			Currently we do not have 64-bit load/store yet,
			so we will use two 32-bit load/store instructions to do
			memory access and they are single word alignment.  */
		      else if (NDS32_SINGLE_WORD_ALIGN_P (INTVAL (op1)))
			return true;
		    }
		}
	    }
	}
    }

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

      /* Allow [post_inc] and [post_dec] addressing mode.  */
      if (GET_CODE (x) == POST_INC || GET_CODE (x) == POST_DEC)
	{
	  if (nds32_address_register_rtx_p (XEXP (x, 0), strict))
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
      /* (mem (symbol_ref A)) => [symbol_ref] */

      if (flag_pic || SYMBOL_REF_TLS_MODEL (x))
	return false;

      if (TARGET_ICT_MODEL_LARGE && nds32_indirect_call_referenced_p (x))
	return false;

      /* If -mcmodel=large, the 'symbol_ref' is not a valid address
	 during or after LRA/reload phase.  */
      if (TARGET_CMODEL_LARGE
	  && (reload_completed
	      || reload_in_progress
	      || lra_in_progress))
	return false;
      /* If -mcmodel=medium and the symbol references to rodata section,
	 the 'symbol_ref' is not a valid address during or after
	 LRA/reload phase.  */
      if (TARGET_CMODEL_MEDIUM
	  && (NDS32_SYMBOL_REF_RODATA_P (x)
	      || CONSTANT_POOL_ADDRESS_P (x))
	  && (reload_completed
	      || reload_in_progress
	      || lra_in_progress))
	return false;

      return true;

    case CONST:
      /* (mem (const (...)))
	 => [ + const_addr ], where const_addr = symbol_ref + const_int */
      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  rtx plus_op = XEXP (x, 0);

	  rtx op0 = XEXP (plus_op, 0);
	  rtx op1 = XEXP (plus_op, 1);

	  if (GET_CODE (op0) == SYMBOL_REF && CONST_INT_P (op1))
	    {
	      /* Now we see the [ + const_addr ] pattern, but we need
		 some further checking.  */

	      if (flag_pic || SYMBOL_REF_TLS_MODEL (op0))
		return false;

	      /* If -mcmodel=large, the 'const_addr' is not a valid address
		 during or after LRA/reload phase.  */
	      if (TARGET_CMODEL_LARGE
		  && (reload_completed
		      || reload_in_progress
		      || lra_in_progress))
		return false;
	      /* If -mcmodel=medium and the symbol references to rodata section,
		 the 'const_addr' is not a valid address during or after
		 LRA/reload phase.  */
	      if (TARGET_CMODEL_MEDIUM
		  && NDS32_SYMBOL_REF_RODATA_P (op0)
		  && (reload_completed
		      || reload_in_progress
		      || lra_in_progress))
		return false;

	      /* At this point we can make sure 'const_addr' is a
		 valid address.  */
	      return true;
	    }
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
      /* (mem (lo_sum (reg) (symbol_ref))) */
      /* (mem (lo_sum (reg) (const (plus (symbol_ref) (reg)))) */
      /* TLS case: (mem (lo_sum (reg) (const (unspec symbol_ref X)))) */
      /* The LO_SUM is a valid address if and only if we would like to
	 generate 32-bit full address memory access with any of following
	 circumstance:
	   1. -mcmodel=large.
	   2. -mcmodel=medium and the symbol_ref references to rodata.  */
      {
	rtx sym = NULL_RTX;

	if (flag_pic)
	  return false;

	if (!REG_P (XEXP (x, 0)))
	  return false;

	if (GET_CODE (XEXP (x, 1)) == SYMBOL_REF)
	  sym = XEXP (x, 1);
	else if (GET_CODE (XEXP (x, 1)) == CONST)
	  {
	    rtx plus = XEXP(XEXP (x, 1), 0);
	    if (GET_CODE (plus) == PLUS)
	      sym = XEXP (plus, 0);
	    else if (GET_CODE (plus) == UNSPEC)
	      sym = XVECEXP (plus, 0, 0);
	  }
	else
	  return false;

	gcc_assert (GET_CODE (sym) == SYMBOL_REF);

	if (TARGET_ICT_MODEL_LARGE
	    && nds32_indirect_call_referenced_p (sym))
	  return true;

	if (TARGET_CMODEL_LARGE)
	  return true;
	else if (TARGET_CMODEL_MEDIUM
		 && NDS32_SYMBOL_REF_RODATA_P (sym))
	  return true;
	else
	  return false;
      }

    default:
      return false;
    }
}

static rtx
nds32_legitimize_address (rtx x,
			  rtx oldx ATTRIBUTE_UNUSED,
			  machine_mode mode ATTRIBUTE_UNUSED)
{
  if (nds32_tls_referenced_p (x))
    x = nds32_legitimize_tls_address (x);
  else if (flag_pic && SYMBOLIC_CONST_P (x))
    x = nds32_legitimize_pic_address (x);
  else if (TARGET_ICT_MODEL_LARGE && nds32_indirect_call_referenced_p (x))
    x = nds32_legitimize_ict_address (x);

  return x;
}

static bool
nds32_legitimate_constant_p (machine_mode mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_DOUBLE:
      if ((TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE)
	  && (mode == DFmode || mode == SFmode))
	return false;
      break;
    case CONST:
      x = XEXP (x, 0);

      if (GET_CODE (x) == PLUS)
	{
	  if (!CONST_INT_P (XEXP (x, 1)))
	    return false;
	  x = XEXP (x, 0);
	}

      if (GET_CODE (x) == UNSPEC)
	{
	  switch (XINT (x, 1))
	    {
	    case UNSPEC_GOT:
	    case UNSPEC_GOTOFF:
	    case UNSPEC_PLT:
	    case UNSPEC_TLSGD:
	    case UNSPEC_TLSLD:
	    case UNSPEC_TLSIE:
	    case UNSPEC_TLSLE:
	    case UNSPEC_ICT:
	      return false;
	    default:
	      return true;
	    }
	}
      break;
    case SYMBOL_REF:
      /* TLS symbols need a call to resolve in
	 precompute_register_parameters.  */
      if (SYMBOL_REF_TLS_MODEL (x))
	return false;
      break;
    default:
      return true;
    }

  return true;
}

/* Reorgnize the UNSPEC CONST and return its direct symbol.  */
static rtx
nds32_delegitimize_address (rtx x)
{
  x = delegitimize_mem_from_attrs (x);

  if (GET_CODE(x) == CONST)
    {
      rtx inner = XEXP (x, 0);

      /* Handle for GOTOFF.  */
      if (GET_CODE (inner) == PLUS)
	inner = XEXP (inner, 0);

      if (GET_CODE (inner) == UNSPEC)
	{
	  switch (XINT (inner, 1))
	    {
	    case UNSPEC_GOTINIT:
	    case UNSPEC_GOT:
	    case UNSPEC_GOTOFF:
	    case UNSPEC_PLT:
	    case UNSPEC_TLSGD:
	    case UNSPEC_TLSLD:
	    case UNSPEC_TLSIE:
	    case UNSPEC_TLSLE:
	    case UNSPEC_ICT:
	      x = XVECEXP (inner, 0, 0);
	      break;
	    default:
	      break;
	    }
	}
    }
  return x;
}

static machine_mode
nds32_vectorize_preferred_simd_mode (scalar_mode mode)
{
  if (!NDS32_EXT_DSP_P ())
    return word_mode;

  switch (mode)
    {
    case E_QImode:
      return V4QImode;
    case E_HImode:
      return V2HImode;
    default:
      return word_mode;
    }
}

static bool
nds32_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST:
      return !nds32_legitimate_constant_p (mode, x);
    case SYMBOL_REF:
      /* All symbols have to be accessed through gp-relative in PIC mode.  */
      /* We don't want to force symbol as constant pool in .text section,
	 because we use the gp-relatived instruction to load in small
	 or medium model.  */
      if (flag_pic
	  || SYMBOL_REF_TLS_MODEL (x)
	  || TARGET_CMODEL_SMALL
	  || TARGET_CMODEL_MEDIUM)
	return true;
      break;
    case CONST_INT:
    case CONST_DOUBLE:
      if (flag_pic && (lra_in_progress || reload_completed))
	return true;
      break;
    default:
      return false;
    }
  return false;
}


/* Condition Code Status.  */

/* -- Representation of condition codes using registers.  */

static void
nds32_canonicalize_comparison (int *code,
			       rtx *op0 ATTRIBUTE_UNUSED,
			       rtx *op1,
			       bool op0_preserve_value ATTRIBUTE_UNUSED)
{
  /* When the instruction combination pass tries to combine a comparison insn
     with its previous insns, it also transforms the operator in order to
     minimize its constant field.  For example, it tries to transform a
     comparison insn from
       (set (reg:SI 54)
	   (ltu:SI (reg:SI 52)
	       (const_int 10 [0xa])))
     to
       (set (reg:SI 54)
	   (leu:SI (reg:SI 52)
	       (const_int 9 [0x9])))

     However, the nds32 target only provides instructions supporting the LTU
     operation directly, and the implementation of the pattern "cbranchsi4"
     only expands the LTU form.  In order to handle the non-LTU operations
     generated from passes other than the RTL expansion pass, we have to
     implement this hook to revert those changes.  Since we only expand the LTU
     operator in the RTL expansion pass, we might only need to handle the LEU
     case, unless we find other optimization passes perform more aggressive
     transformations.  */

  if (*code == LEU && CONST_INT_P (*op1))
    {
      *op1 = gen_int_mode (INTVAL (*op1) + 1, SImode);
      *code = LTU;
    }
}


/* Describing Relative Costs of Operations.  */

static int
nds32_register_move_cost (machine_mode mode,
			  reg_class_t from,
			  reg_class_t to)
{
  /* In garywolf cpu, FPR to GPR is chaper than other cpu.  */
  if (TARGET_PIPELINE_GRAYWOLF)
    {
      if (GET_MODE_SIZE (mode) == 8)
	{
	  /* DPR to GPR.  */
	  if (from == FP_REGS && to != FP_REGS)
	    return 3;
	  /* GPR to DPR.  */
	  if (from != FP_REGS && to == FP_REGS)
	    return 2;
	}
      else
	{
	  if ((from == FP_REGS && to != FP_REGS)
	      || (from != FP_REGS && to == FP_REGS))
	    return 2;
	}
    }

  if ((from == FP_REGS && to != FP_REGS)
      || (from != FP_REGS && to == FP_REGS))
    return 3;
  else if (from == HIGH_REGS || to == HIGH_REGS)
    return optimize_size ? 6 : 2;
  else
    return 2;
}

static int
nds32_memory_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			reg_class_t rclass ATTRIBUTE_UNUSED,
			bool in ATTRIBUTE_UNUSED)
{
  return 8;
}

/* This target hook describes the relative costs of RTL expressions.
   Return 'true' when all subexpressions of x have been processed.
   Return 'false' to sum the costs of sub-rtx, plus cost of this operation.
   Refer to gcc/rtlanal.cc for more information.  */
static bool
nds32_rtx_costs (rtx x,
		 machine_mode mode,
		 int outer_code,
		 int opno,
		 int *total,
		 bool speed)
{
  return nds32_rtx_costs_impl (x, mode, outer_code, opno, total, speed);
}

static int
nds32_address_cost (rtx address,
		    machine_mode mode,
		    addr_space_t as,
		    bool speed)
{
  return nds32_address_cost_impl (address, mode, as, speed);
}


/* Dividing the Output into Sections (Texts, Data, . . . ).  */

/* If references to a symbol or a constant must be treated differently
   depending on something about the variable or function named by the symbol
   (such as what section it is in), we use this hook to store flags
   in symbol_ref rtx.  */
static void
nds32_encode_section_info (tree decl, rtx rtl, int new_decl_p)
{
  default_encode_section_info (decl, rtl, new_decl_p);

  /* For the memory rtx, if it references to rodata section, we can store
     NDS32_SYMBOL_FLAG_RODATA flag into symbol_ref rtx so that the
     nds32_legitimate_address_p() can determine how to treat such symbol_ref
     based on -mcmodel=X and this information.  */
  if (MEM_P (rtl) && MEM_READONLY_P (rtl))
    {
      rtx addr = XEXP (rtl, 0);

      if (GET_CODE (addr) == SYMBOL_REF)
	{
	  /* For (mem (symbol_ref X)) case.  */
	  SYMBOL_REF_FLAGS (addr) |= NDS32_SYMBOL_FLAG_RODATA;
	}
      else if (GET_CODE (addr) == CONST
	       && GET_CODE (XEXP (addr, 0)) == PLUS)
	{
	  /* For (mem (const (plus (symbol_ref X) (const_int N)))) case.  */
	  rtx plus_op = XEXP (addr, 0);
	  rtx op0 = XEXP (plus_op, 0);
	  rtx op1 = XEXP (plus_op, 1);

	  if (GET_CODE (op0) == SYMBOL_REF && CONST_INT_P (op1))
	    SYMBOL_REF_FLAGS (op0) |= NDS32_SYMBOL_FLAG_RODATA;
	}
    }
}


/* Defining the Output Assembler Language.  */

/* -- The Overall Framework of an Assembler File.  */

static void
nds32_asm_file_start (void)
{
  default_file_start ();

  if (flag_pic)
    fprintf (asm_out_file, "\t.pic\n");

  /* Tell assembler which ABI we are using.  */
  fprintf (asm_out_file, "\t! ABI version\n");
  if (TARGET_HARD_FLOAT)
    fprintf (asm_out_file, "\t.abi_2fp_plus\n");
  else
    fprintf (asm_out_file, "\t.abi_2\n");

  /* Tell assembler that this asm code is generated by compiler.  */
  fprintf (asm_out_file, "\t! This asm file is generated by compiler\n");
  fprintf (asm_out_file, "\t.flag\tverbatim\n");

  /* Insert directive for linker to distinguish object's ict flag.  */
  if (!TARGET_LINUX_ABI)
    {
      if (TARGET_ICT_MODEL_LARGE)
	fprintf (asm_out_file, "\t.ict_model\tlarge\n");
      else
	fprintf (asm_out_file, "\t.ict_model\tsmall\n");
    }

  /* We need to provide the size of each vector for interrupt handler
     under elf toolchain.  */
  if (!TARGET_LINUX_ABI)
    {
      fprintf (asm_out_file, "\t! This vector size directive is required "
			     "for checking inconsistency on interrupt handler\n");
      fprintf (asm_out_file, "\t.vec_size\t%d\n", nds32_isr_vector_size);
    }

  /* If user enables '-mforce-fp-as-gp' or compiles programs with -Os,
     the compiler may produce 'la $fp,_FP_BASE_' instruction
     at prologue for fp-as-gp optimization.
     We should emit weak reference of _FP_BASE_ to avoid undefined reference
     in case user does not pass '--relax' option to linker.  */
  if (!TARGET_LINUX_ABI && (TARGET_FORCE_FP_AS_GP || optimize_size))
    {
      fprintf (asm_out_file, "\t! This weak reference is required to do "
			     "fp-as-gp link time optimization\n");
      fprintf (asm_out_file, "\t.weak\t_FP_BASE_\n");
    }

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  if (TARGET_ISA_V2)
    fprintf (asm_out_file, "\t! ISA family\t\t: %s\n", "V2");
  if (TARGET_ISA_V3)
    fprintf (asm_out_file, "\t! ISA family\t\t: %s\n", "V3");
  if (TARGET_ISA_V3M)
    fprintf (asm_out_file, "\t! ISA family\t\t: %s\n", "V3M");

  switch (nds32_cpu_option)
    {
    case CPU_N6:
      fprintf (asm_out_file, "\t! Pipeline model\t: %s\n", "N6");
      break;

    case CPU_N7:
      fprintf (asm_out_file, "\t! Pipeline model\t: %s\n", "N7");
      break;

    case CPU_N8:
      fprintf (asm_out_file, "\t! Pipeline model\t: %s\n", "N8");
      break;

    case CPU_E8:
      fprintf (asm_out_file, "\t! Pipeline model\t: %s\n", "E8");
      break;

    case CPU_N9:
      fprintf (asm_out_file, "\t! Pipeline model\t: %s\n", "N9");
      break;

    case CPU_N10:
      fprintf (asm_out_file, "\t! Pipeline model\t: %s\n", "N10");
      break;

    case CPU_GRAYWOLF:
      fprintf (asm_out_file, "\t! Pipeline model\t: %s\n", "Graywolf");
      break;

    case CPU_N12:
    case CPU_N13:
      fprintf (asm_out_file, "\t! Pipeline model\t: %s\n", "N13");
      break;

    case CPU_SIMPLE:
      fprintf (asm_out_file, "\t! Pipeline model\t: %s\n", "SIMPLE");
      break;

    default:
      gcc_unreachable ();
    }

  if (TARGET_CMODEL_SMALL)
    fprintf (asm_out_file, "\t! Code model\t\t: %s\n", "SMALL");
  if (TARGET_CMODEL_MEDIUM)
    fprintf (asm_out_file, "\t! Code model\t\t: %s\n", "MEDIUM");
  if (TARGET_CMODEL_LARGE)
    fprintf (asm_out_file, "\t! Code model\t\t: %s\n", "LARGE");

  fprintf (asm_out_file, "\t! Endian setting\t: %s\n",
			 ((TARGET_BIG_ENDIAN) ? "big-endian"
					      : "little-endian"));
  fprintf (asm_out_file, "\t! Use SP floating-point instruction\t: %s\n",
			 ((TARGET_FPU_SINGLE) ? "Yes"
					      : "No"));
  fprintf (asm_out_file, "\t! Use DP floating-point instruction\t: %s\n",
			 ((TARGET_FPU_DOUBLE) ? "Yes"
					      : "No"));
  fprintf (asm_out_file, "\t! ABI version\t\t: %s\n",
			 ((TARGET_HARD_FLOAT) ? "ABI2FP+"
					      : "ABI2"));

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  fprintf (asm_out_file, "\t! Use conditional move\t\t: %s\n",
			 ((TARGET_CMOV) ? "Yes"
					: "No"));
  fprintf (asm_out_file, "\t! Use performance extension\t: %s\n",
			 ((TARGET_EXT_PERF) ? "Yes"
					    : "No"));
  fprintf (asm_out_file, "\t! Use performance extension 2\t: %s\n",
			 ((TARGET_EXT_PERF2) ? "Yes"
					     : "No"));
  fprintf (asm_out_file, "\t! Use string extension\t\t: %s\n",
			 ((TARGET_EXT_STRING) ? "Yes"
					      : "No"));

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  fprintf (asm_out_file, "\t! V3PUSH instructions\t: %s\n",
			 ((TARGET_V3PUSH) ? "Yes"
					  : "No"));
  fprintf (asm_out_file, "\t! 16-bit instructions\t: %s\n",
			 ((TARGET_16_BIT) ? "Yes"
					  : "No"));
  fprintf (asm_out_file, "\t! Reduced registers set\t: %s\n",
			 ((TARGET_REDUCED_REGS) ? "Yes"
						: "No"));

  fprintf (asm_out_file, "\t! Support unaligned access\t\t: %s\n",
			 (flag_unaligned_access ? "Yes"
						: "No"));

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  if (optimize_size)
    fprintf (asm_out_file, "\t! Optimization level\t: -Os\n");
  else if (optimize_fast)
    fprintf (asm_out_file, "\t! Optimization level\t: -Ofast\n");
  else if (optimize_debug)
    fprintf (asm_out_file, "\t! Optimization level\t: -Og\n");
  else
    fprintf (asm_out_file, "\t! Optimization level\t: -O%d\n", optimize);

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  fprintf (asm_out_file, "\t! Cache block size\t: %d\n",
			 nds32_cache_block_size);

  fprintf (asm_out_file, "\t! ------------------------------------\n");

  nds32_asm_file_start_for_isr ();
}

static void
nds32_asm_file_end (void)
{
  nds32_asm_file_end_for_isr ();

  /* The NDS32 Linux stack is mapped non-executable by default, so add a
     .note.GNU-stack section.  */
  if (TARGET_LINUX_ABI)
    file_end_indicate_exec_stack ();

  fprintf (asm_out_file, "\t! ------------------------------------\n");
}

static bool
nds32_asm_output_addr_const_extra (FILE *file, rtx x)
{
  if (GET_CODE (x) == UNSPEC)
    {
      switch (XINT (x, 1))
	{
	case UNSPEC_GOTINIT:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  break;
	case UNSPEC_GOTOFF:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOTOFF", file);
	  break;
	case UNSPEC_GOT:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOT", file);
	  break;
	case UNSPEC_PLT:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@PLT", file);
	  break;
	case UNSPEC_TLSGD:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@TLSDESC", file);
	  break;
	case UNSPEC_TLSLD:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@TLSDESC", file);
	  break;
	case UNSPEC_TLSIE:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@GOTTPOFF", file);
	  break;
	case UNSPEC_TLSLE:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@TPOFF", file);
	  break;
	case UNSPEC_ICT:
	  output_addr_const (file, XVECEXP (x, 0, 0));
	  fputs ("@ICT", file);
	  break;
	default:
	  return false;
	}
      return true;
    }
  else
    return false;
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
  HOST_WIDE_INT op_value = 0;
  HOST_WIDE_INT one_position;
  HOST_WIDE_INT zero_position;
  bool pick_lsb_p = false;
  bool pick_msb_p = false;
  int regno;

  if (CONST_INT_P (x))
    op_value = INTVAL (x);

  switch (code)
    {
    case 0 :
      /* Do nothing special.  */
      break;

    case 'b':
      /* Use exact_log2() to search the 0-bit position.  */
      gcc_assert (CONST_INT_P (x));
      zero_position = exact_log2 (~UINTVAL (x) & GET_MODE_MASK (SImode));
      gcc_assert (zero_position != -1);
      fprintf (stream, HOST_WIDE_INT_PRINT_DEC, zero_position);

      /* No need to handle following process, so return immediately.  */
      return;

    case 'e':
      gcc_assert (MEM_P (x)
		  && GET_CODE (XEXP (x, 0)) == PLUS
		  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT);
      fprintf (stream, HOST_WIDE_INT_PRINT_DEC, INTVAL (XEXP (XEXP (x, 0), 1)));

      /* No need to handle following process, so return immediately.  */
      return;

    case 'v':
      gcc_assert (CONST_INT_P (x)
		  && (INTVAL (x) == 0
		      || INTVAL (x) == 8
		      || INTVAL (x) == 16
		      || INTVAL (x) == 24));
      fprintf (stream, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) / 8);

      /* No need to handle following process, so return immediately.  */
      return;

    case 'B':
      /* Use exact_log2() to search the 1-bit position.  */
      gcc_assert (CONST_INT_P (x));
      one_position = exact_log2 (UINTVAL (x) & GET_MODE_MASK (SImode));
      gcc_assert (one_position != -1);
      fprintf (stream, HOST_WIDE_INT_PRINT_DEC, one_position);

      /* No need to handle following process, so return immediately.  */
      return;

    case 'L':
      /* X is supposed to be REG rtx.  */
      gcc_assert (REG_P (x));
      /* Claim that we are going to pick LSB part of X.  */
      pick_lsb_p = true;
      break;

    case 'H':
      /* X is supposed to be REG rtx.  */
      gcc_assert (REG_P (x));
      /* Claim that we are going to pick MSB part of X.  */
      pick_msb_p = true;
      break;

    case 'V':
      /* 'x' is supposed to be CONST_INT, get the value.  */
      gcc_assert (CONST_INT_P (x));

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
	  fprintf (stream, HOST_WIDE_INT_PRINT_DEC, op_value);
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

    case 'R': /* cctl valck  */
      /* Note the cctl divide to 5 group and share the same name table.  */
      if (op_value < 0 || op_value > 4)
	error ("CCTL intrinsic function subtype out of range");
      fprintf (stream, "%s", nds32_cctl_names[op_value]);
      return;

    case 'T': /* cctl idxwbinv  */
      /* Note the cctl divide to 5 group and share the same name table.  */
      if (op_value < 0 || op_value > 4)
	error ("CCTL intrinsic function subtype out of range");
      fprintf (stream, "%s", nds32_cctl_names[op_value + 4]);
      return;

    case 'U': /* cctl vawbinv  */
      /* Note the cctl divide to 5 group and share the same name table.  */
      if (op_value < 0 || op_value > 4)
	error ("CCTL intrinsic function subtype out of range");
      fprintf (stream, "%s", nds32_cctl_names[op_value + 8]);
      return;

    case 'X': /* cctl idxread  */
      /* Note the cctl divide to 5 group and share the same name table.  */
      if (op_value < 0 || op_value > 4)
	error ("CCTL intrinsic function subtype out of range");
      fprintf (stream, "%s", nds32_cctl_names[op_value + 12]);
      return;

    case 'W': /* cctl idxwitre  */
      /* Note the cctl divide to 5 group and share the same name table.  */
      if (op_value < 0 || op_value > 4)
	error ("CCTL intrinsic function subtype out of range");
      fprintf (stream, "%s", nds32_cctl_names[op_value + 16]);
      return;

    case 'Z': /* dpref  */
      fprintf (stream, "%s", nds32_dpref_names[op_value]);
      return;

    default :
      /* Unknown flag.  */
      output_operand_lossage ("invalid operand output code");
      break;
    }

  switch (GET_CODE (x))
    {
    case LABEL_REF:
      output_addr_const (stream, x);
      break;

    case SYMBOL_REF:
      output_addr_const (stream, x);

      if (!TARGET_LINUX_ABI && nds32_indirect_call_referenced_p (x))
	fprintf (stream, "@ICT");

      break;

    case REG:
      /* Print a Double-precision register name.  */
      if ((GET_MODE (x) == DImode || GET_MODE (x) == DFmode)
	  && NDS32_IS_FPR_REGNUM (REGNO (x)))
	{
	  regno = REGNO (x);
	  if (!NDS32_FPR_REGNO_OK_FOR_DOUBLE (regno))
	    {
	      output_operand_lossage ("invalid operand for code '%c'", code);
	      break;
	    }
	  fprintf (stream, "$fd%d", (regno - NDS32_FIRST_FPR_REGNUM) >> 1);
	  break;
	}

      /* Print LSB or MSB part of register pair if the
	 constraint modifier 'L' or 'H' is specified.  */
      if ((GET_MODE (x) == DImode || GET_MODE (x) == DFmode)
	  && NDS32_IS_GPR_REGNUM (REGNO (x)))
	{
	  if ((pick_lsb_p && WORDS_BIG_ENDIAN)
	      || (pick_msb_p && !WORDS_BIG_ENDIAN))
	    {
	      /* If we would like to print out LSB register under big-endian,
		 or print out MSB register under little-endian, we need to
		 increase register number.  */
	      regno = REGNO (x);
	      regno++;
	      fputs (reg_names[regno], stream);
	      break;
	    }
	}

      /* Forbid using static chain register ($r16)
	 on reduced-set registers configuration.  */
      if (TARGET_REDUCED_REGS
	  && REGNO (x) == STATIC_CHAIN_REGNUM)
	sorry ("a nested function is not supported for reduced registers");

      /* Normal cases, print out register name.  */
      fputs (reg_names[REGNO (x)], stream);
      break;

    case MEM:
      output_address (GET_MODE (x), XEXP (x, 0));
      break;

    case HIGH:
      if (GET_CODE (XEXP (x, 0)) == CONST_DOUBLE)
	{
	  const REAL_VALUE_TYPE *rv;
	  long val;
	  gcc_assert (GET_MODE (x) == SFmode);

	  rv = CONST_DOUBLE_REAL_VALUE (XEXP (x, 0));
	  REAL_VALUE_TO_TARGET_SINGLE (*rv, val);

	  fprintf (stream, "hi20(0x%lx)", val);
	}
      else
	gcc_unreachable ();
      break;

    case CONST_DOUBLE:
      const REAL_VALUE_TYPE *rv;
      long val;
      gcc_assert (GET_MODE (x) == SFmode);

      rv = CONST_DOUBLE_REAL_VALUE (x);
      REAL_VALUE_TO_TARGET_SINGLE (*rv, val);

      fprintf (stream, "0x%lx", val);
      break;

    case CODE_LABEL:
    case CONST_INT:
    case CONST:
      output_addr_const (stream, x);
      break;

    case CONST_VECTOR:
      fprintf (stream, HOST_WIDE_INT_PRINT_HEX, const_vector_to_hwint (x));
      break;

    case LO_SUM:
      /* This is a special case for inline assembly using memory address 'p'.
	 The inline assembly code is expected to use pesudo instruction
	 for the operand.  EX: la  */
      output_addr_const (stream, XEXP(x, 1));
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
nds32_print_operand_address (FILE *stream,
			     machine_mode mode ATTRIBUTE_UNUSED,
			     rtx x)
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

    case LO_SUM:
      /* This is a special case for inline assembly using memory operand 'm'.
	 The inline assembly code is expected to use pesudo instruction
	 for the operand.  EX: [ls].[bhw]  */
      fputs ("[ + ", stream);
      op1 = XEXP (x, 1);
      output_addr_const (stream, op1);
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
	  fprintf (stream, "[%s + (" HOST_WIDE_INT_PRINT_DEC ")]",
			   reg_names[REGNO (op0)], INTVAL (op1));
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
	  else if (INTVAL (XEXP (op0, 1)) == 8)
	    sv = 3;
	  else
	    gcc_unreachable ();

	  fprintf (stream, "[%s + %s << %d]",
			   reg_names[REGNO (op1)],
			   reg_names[REGNO (XEXP (op0, 0))],
			   sv);
	}
      else if (GET_CODE (op0) == ASHIFT && REG_P (op1))
	{
	  /* [Ra + Rb << sv]
	     In normal, ASHIFT can be converted to MULT like above case.
	     But when the address rtx does not go through canonicalize_address
	     defined in fwprop, we'll need this case.  */
	  int sv = INTVAL (XEXP (op0, 1));
	  gcc_assert (sv <= 3 && sv >=0);

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
	  fprintf (stream, "[%s], " HOST_WIDE_INT_PRINT_DEC,
			   reg_names[REGNO (op0)], INTVAL (op1));
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

/* -- Assembler Commands for Exception Regions.  */

static rtx
nds32_dwarf_register_span (rtx reg)
{
  rtx dwarf_high, dwarf_low;
  rtx dwarf_single;
  machine_mode mode;
  int regno;

  mode = GET_MODE (reg);
  regno = REGNO (reg);

  /* We need to adjust dwarf register information for floating-point registers
     rather than using default register number mapping.  */
  if (regno >= NDS32_FIRST_FPR_REGNUM
      && regno <= NDS32_LAST_FPR_REGNUM)
    {
      if (mode == DFmode || mode == SCmode)
	{
	  /* By default, GCC maps increasing register numbers to increasing
	     memory locations, but paired FPRs in NDS32 target are always
	     big-endian, i.e.:

	       fd0 :  fs0   fs1
		     (MSB) (LSB)

	     We must return parallel rtx to represent such layout.  */
	  dwarf_high = gen_rtx_REG (word_mode, regno);
	  dwarf_low = gen_rtx_REG (word_mode, regno + 1);
	  return gen_rtx_PARALLEL (VOIDmode,
				   gen_rtvec (2, dwarf_low, dwarf_high));
	}
      else if (mode == DCmode)
	{
	  rtx dwarf_high_re = gen_rtx_REG (word_mode, regno);
	  rtx dwarf_low_re = gen_rtx_REG (word_mode, regno + 1);
	  rtx dwarf_high_im = gen_rtx_REG (word_mode, regno);
	  rtx dwarf_low_im = gen_rtx_REG (word_mode, regno + 1);
	  return gen_rtx_PARALLEL (VOIDmode,
				   gen_rtvec (4, dwarf_low_re, dwarf_high_re,
						 dwarf_high_im, dwarf_low_im));
	}
      else if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
	{
	  return NULL_RTX;
	}
      else
	{
	  /* We should not be here.  */
	  gcc_unreachable ();
	}
    }

  return NULL_RTX;
}

/* Map internal gcc register numbers to DWARF2 register numbers.  */

unsigned int
nds32_debugger_regno (unsigned int regno)
{
  /* The nds32 port in GDB maintains a mapping between dwarf register
     number and displayed register name.  For backward compatibility to
     previous toolchain, currently our gdb still has four registers
     (d0.l, d0.h, d1.l, and d1.h) between GPR and FPR while compiler
     does not count those four registers in its register number table.
     So we have to add 4 on its register number and then create new
     dwarf information.  Hopefully we can discard such workaround
     in the future.  */
  if (NDS32_IS_FPR_REGNUM (regno))
    return regno + 4;

  return regno;
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
  /* A "indirect_call" function attribute implies "noinline" and "noclone"
     for elf toolchain to support ROM patch mechanism.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && lookup_attribute ("indirect_call", *attributes) != NULL)
    {
      tree new_attrs = *attributes;

      if (TARGET_LINUX_ABI)
	error ("cannot use %<indirect_call%> attribute under linux toolchain");

      if (lookup_attribute ("noinline", new_attrs) == NULL)
	new_attrs = tree_cons (get_identifier ("noinline"), NULL, new_attrs);
      if (lookup_attribute ("noclone", new_attrs) == NULL)
	new_attrs = tree_cons (get_identifier ("noclone"), NULL, new_attrs);

      if (!TREE_PUBLIC (decl))
	error ("%<indirect_call%> attribute cannot apply for static function");

      *attributes = new_attrs;
    }

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

      /* The following code may use attribute arguments.  If there is no
	 argument from source code, it will cause segmentation fault.
	 Therefore, return dircetly and report error message later.  */
      if ((intr && TREE_VALUE (intr) == NULL)
	  || (excp && TREE_VALUE (excp) == NULL)
	  || (reset && TREE_VALUE (reset) == NULL))
	return;

      /* ------------------------------------------------------------- */
      /* FIXME:
	 FOR BACKWARD COMPATIBILITY, we need to support following patterns:

	     __attribute__((interrupt("XXX;YYY;id=ZZZ")))
	     __attribute__((exception("XXX;YYY;id=ZZZ")))
	     __attribute__((reset("vectors=XXX;nmi_func=YYY;warm_func=ZZZ")))

	 If interrupt/exception/reset appears and its argument is a
	 STRING_CST, we will use other functions to parse string in the
	 nds32_construct_isr_vectors_information() and then set necessary
	 isr information in the nds32_isr_vectors[] array.  Here we can
	 just return immediately to avoid new-syntax checking.  */
      if (intr != NULL_TREE
	  && TREE_CODE (TREE_VALUE (TREE_VALUE (intr))) == STRING_CST)
	return;
      if (excp != NULL_TREE
	  && TREE_CODE (TREE_VALUE (TREE_VALUE (excp))) == STRING_CST)
	return;
      if (reset != NULL_TREE
	  && TREE_CODE (TREE_VALUE (TREE_VALUE (reset))) == STRING_CST)
	return;
      /* ------------------------------------------------------------- */

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
		  || wi::ltu_p (wi::to_wide (id), lower_bound)
		  || wi::gtu_p (wi::to_wide (id), upper_bound))
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
	      || wi::ltu_p (wi::to_wide (id), lower_bound)
	      || wi::gtu_p (wi::to_wide (id), upper_bound))
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
      /* If this is ARCH_V3J, we need to enable TARGET_REDUCED_REGS.  */
      if (nds32_arch_option == ARCH_V3J)
	target_flags |= MASK_REDUCED_REGS;
    }
  if (TARGET_ISA_V3M)
    {
      /* Under V3M ISA, we need to strictly enable TARGET_REDUCED_REGS.  */
      target_flags |= MASK_REDUCED_REGS;
      /* Under V3M ISA, we need to strictly disable TARGET_EXT_PERF.  */
      target_flags &= ~MASK_EXT_PERF;
      /* Under V3M ISA, we need to strictly disable TARGET_EXT_PERF2.  */
      target_flags &= ~MASK_EXT_PERF2;
      /* Under V3M ISA, we need to strictly disable TARGET_EXT_STRING.  */
      target_flags &= ~MASK_EXT_STRING;

      if (flag_pic)
	error ("not support %<-fpic%> option for v3m toolchain");
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

  if (TARGET_HARD_FLOAT && !(TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE))
    {
      if (nds32_arch_option == ARCH_V3S || nds32_arch_option == ARCH_V3F)
	error ("Disable FPU ISA, "
	       "the ABI option must be enable %<-mfloat-abi=soft%>");
      else
	error ("%<-mabi=2fp+%> option only support when FPU available, "
	       "must be enable %<-mext-fpu-sp%> or %<-mext-fpu-dp%>");
    }

  nds32_init_rtx_costs ();

  nds32_register_passes ();
}


/* Miscellaneous Parameters.  */

static rtx_insn *
nds32_md_asm_adjust (vec<rtx> &outputs ATTRIBUTE_UNUSED,
		     vec<rtx> &inputs ATTRIBUTE_UNUSED,
		     vec<machine_mode> &input_modes ATTRIBUTE_UNUSED,
		     vec<const char *> &constraints ATTRIBUTE_UNUSED,
		     vec<rtx> &/*uses*/, vec<rtx> &clobbers,
		     HARD_REG_SET &clobbered_regs, location_t /*loc*/)
{
  if (!flag_inline_asm_r15)
    {
      clobbers.safe_push (gen_rtx_REG (SImode, TA_REGNUM));
      SET_HARD_REG_BIT (clobbered_regs, TA_REGNUM);
    }
  return NULL;
}

static void
nds32_init_builtins (void)
{
  nds32_init_builtins_impl ();
}

static tree
nds32_builtin_decl (unsigned code, bool initialize_p)
{
  /* Implement in nds32-intrinsic.cc.  */
  return nds32_builtin_decl_impl (code, initialize_p);
}

static rtx
nds32_expand_builtin (tree exp,
		      rtx target,
		      rtx subtarget,
		      machine_mode mode,
		      int ignore)
{
  return nds32_expand_builtin_impl (exp, target, subtarget, mode, ignore);
}

/* Implement TARGET_INIT_LIBFUNCS.  */
static void
nds32_init_libfuncs (void)
{
  if (TARGET_LINUX_ABI)
    init_sync_libfuncs (UNITS_PER_WORD);
}

/* ------------------------------------------------------------------------ */

/* PART 4: Implemet extern function definitions,
           the prototype is in nds32-protos.h.  */

/* Run-time Target Specification.  */

void
nds32_cpu_cpp_builtins(struct cpp_reader *pfile)
{
#define builtin_define(TXT) cpp_define (pfile, TXT)
#define builtin_assert(TXT) cpp_assert (pfile, TXT)
  builtin_define ("__nds32__");
  builtin_define ("__NDS32__");

  /* We need to provide builtin macro to describe the size of
     each vector for interrupt handler under elf toolchain.  */
  if (!TARGET_LINUX_ABI)
    {
      if (TARGET_ISR_VECTOR_SIZE_4_BYTE)
	builtin_define ("__NDS32_ISR_VECTOR_SIZE_4__");
      else
	builtin_define ("__NDS32_ISR_VECTOR_SIZE_16__");
    }

  if (TARGET_HARD_FLOAT)
    builtin_define ("__NDS32_ABI_2FP_PLUS__");
  else
    builtin_define ("__NDS32_ABI_2__");

  if (TARGET_ISA_V2)
    builtin_define ("__NDS32_ISA_V2__");
  if (TARGET_ISA_V3)
    builtin_define ("__NDS32_ISA_V3__");
  if (TARGET_ISA_V3M)
    builtin_define ("__NDS32_ISA_V3M__");

  if (TARGET_FPU_SINGLE)
    builtin_define ("__NDS32_EXT_FPU_SP__");
  if (TARGET_FPU_DOUBLE)
    builtin_define ("__NDS32_EXT_FPU_DP__");

  if (TARGET_EXT_FPU_FMA)
    builtin_define ("__NDS32_EXT_FPU_FMA__");
  if (NDS32_EXT_FPU_DOT_E)
    builtin_define ("__NDS32_EXT_FPU_DOT_E__");
  if (TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE)
    {
      switch (nds32_fp_regnum)
	{
	case 0:
	case 4:
	  builtin_define ("__NDS32_EXT_FPU_CONFIG_0__");
	  break;
	case 1:
	case 5:
	  builtin_define ("__NDS32_EXT_FPU_CONFIG_1__");
	  break;
	case 2:
	case 6:
	  builtin_define ("__NDS32_EXT_FPU_CONFIG_2__");
	  break;
	case 3:
	case 7:
	  builtin_define ("__NDS32_EXT_FPU_CONFIG_3__");
	  break;
	default:
	  abort ();
	}
    }

  if (TARGET_BIG_ENDIAN)
    builtin_define ("__NDS32_EB__");
  else
    builtin_define ("__NDS32_EL__");

  if (TARGET_REDUCED_REGS)
    builtin_define ("__NDS32_REDUCED_REGS__");
  if (TARGET_CMOV)
    builtin_define ("__NDS32_CMOV__");
  if (TARGET_EXT_PERF)
    builtin_define ("__NDS32_EXT_PERF__");
  if (TARGET_EXT_PERF2)
    builtin_define ("__NDS32_EXT_PERF2__");
  if (TARGET_EXT_STRING)
    builtin_define ("__NDS32_EXT_STRING__");
  if (TARGET_16_BIT)
    builtin_define ("__NDS32_16_BIT__");
  if (TARGET_GP_DIRECT)
    builtin_define ("__NDS32_GP_DIRECT__");
  if (TARGET_VH)
    builtin_define ("__NDS32_VH__");
  if (NDS32_EXT_DSP_P ())
    builtin_define ("__NDS32_EXT_DSP__");

  if (TARGET_BIG_ENDIAN)
    builtin_define ("__big_endian__");

  builtin_assert ("cpu=nds32");
  builtin_assert ("machine=nds32");

  if (TARGET_HARD_FLOAT)
    builtin_define ("__NDS32_ABI_2FP_PLUS");
  else
    builtin_define ("__NDS32_ABI_2");

#undef builtin_define
#undef builtin_assert
}


/* Defining Data Structures for Per-function Information.  */

void
nds32_init_expanders (void)
{
  /* Arrange to initialize and mark the machine per-function status.  */
  init_machine_status = nds32_init_machine_status;
}


/* Register Usage.  */

/* -- Order of Allocation of Registers.  */

void
nds32_adjust_reg_alloc_order (void)
{
  const int nds32_reg_alloc_order[] = REG_ALLOC_ORDER;

  /* Copy the default register allocation order, which is designed
     to optimize for code size.  */
  memcpy(reg_alloc_order, nds32_reg_alloc_order, sizeof (reg_alloc_order));

  /* Adjust few register allocation order when optimizing for speed.  */
  if (!optimize_size)
    {
      memcpy (reg_alloc_order, nds32_reg_alloc_order_for_speed,
	      sizeof (nds32_reg_alloc_order_for_speed));
    }
}

/* -- How Values Fit in Registers.  */

static unsigned
nds32_hard_regno_nregs (unsigned regno ATTRIBUTE_UNUSED,
			machine_mode mode)
{
  return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
nds32_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (regno >= FIRST_PSEUDO_REGISTER)
    return true;

  if ((TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE) && NDS32_IS_FPR_REGNUM (regno))
    {
      if (NDS32_IS_EXT_FPR_REGNUM(regno))
	return (NDS32_FPR_REGNO_OK_FOR_DOUBLE(regno) && (mode == DFmode));
      else if (mode == SFmode || mode == SImode)
	return NDS32_FPR_REGNO_OK_FOR_SINGLE (regno);
      else if (mode == DFmode)
	return NDS32_FPR_REGNO_OK_FOR_DOUBLE (regno);

      return false;
    }

  /* Restrict double-word quantities to even register pairs.  */
  if (regno <= NDS32_LAST_GPR_REGNUM)
    return (targetm.hard_regno_nregs (regno, mode) == 1
	    || !((regno) & 1));

  return false;
}

/* Implement TARGET_MODES_TIEABLE_P.  We can use general registers to
   tie QI/HI/SI modes together.  */

static bool
nds32_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if ((GET_MODE_CLASS (mode1) == MODE_INT
       && GET_MODE_CLASS (mode2) == MODE_INT)
      && GET_MODE_SIZE (mode1) <= UNITS_PER_WORD
      && GET_MODE_SIZE (mode2) <= UNITS_PER_WORD)
    return true;

  if (GET_MODE_SIZE (mode1) == GET_MODE_SIZE (mode2))
    {
      if ((TARGET_FPU_SINGLE && !TARGET_FPU_DOUBLE)
	  && (mode1 == DFmode || mode2 == DFmode))
	return false;
      else
	return true;
    }

  return false;
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
    {
      /* $SFP and $AP is FRAME_REGS in fact, However prevent IRA don't
	 know how to allocate register for $SFP and $AP, just tell IRA they
	 are GENERAL_REGS, and ARM do this hack too.  */
      return GENERAL_REGS;
    }
  else if (regno >= 34 && regno <= 97)
    return FP_REGS;
  else
    return NO_REGS;
}


/* Stack Layout and Calling Conventions.  */

/* -- Basic Stack Layout.  */

rtx
nds32_dynamic_chain_address (rtx frameaddr)
{
  if (TARGET_V3PUSH)
    {
      /* If -mv3push is specified, we push $fp, $gp, and $lp into stack.
         We can access dynamic chain address from stack by [$fp - 12].  */
      return plus_constant (Pmode, frameaddr, -12);
    }
  else
    {
      /* For general case we push $fp and $lp into stack at prologue.
         We can access dynamic chain address from stack by [$fp - 8].  */
      return plus_constant (Pmode, frameaddr, -8);
    }
}

rtx
nds32_return_addr_rtx (int count,
		       rtx frameaddr)
{
  int offset;
  rtx addr;

  if (count != 0)
    {
      /* In nds32 ABI design, we can expect that $lp is always available
         from stack by [$fp - 4] location.  */
      offset = -4;
      addr = plus_constant (Pmode, frameaddr, offset);
      addr = memory_address (Pmode, addr);

      return gen_rtx_MEM (Pmode, addr);
    }

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
     cfun->machine->callee_saved_area_gpr_padding_bytes and
     cfun->machine->eh_return_data_regs_size
     when calculating offset.  */
  if (from_reg == ARG_POINTER_REGNUM && to_reg == STACK_POINTER_REGNUM)
    {
      offset = (cfun->machine->fp_size
		+ cfun->machine->gp_size
		+ cfun->machine->lp_size
		+ cfun->machine->callee_saved_gpr_regs_size
		+ cfun->machine->callee_saved_area_gpr_padding_bytes
		+ cfun->machine->callee_saved_fpr_regs_size
		+ cfun->machine->eh_return_data_regs_size
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
		       + cfun->machine->callee_saved_gpr_regs_size
		       + cfun->machine->callee_saved_area_gpr_padding_bytes
		       + cfun->machine->callee_saved_fpr_regs_size
		       + cfun->machine->eh_return_data_regs_size);
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
  /* Initial available registers.  The values are offset against
     NDS32_GPR_ARG_FIRST_REGNUM and NDS32_FPR_ARG_FIRST_REGNUM
     for passing arguments.  */
  cum->gpr_offset = 0;
  cum->fpr_offset = 0;
}

/* -- Function Entry and Exit.  */

/* Function for normal multiple push prologue.  */
void
nds32_expand_prologue (void)
{
  int fp_adjust;
  int sp_adjust;
  unsigned Rb, Re;

  /* Compute and setup stack frame size.
     The result will be in cfun->machine.  */
  nds32_compute_stack_frame ();

  /* Check frame_pointer_needed again to prevent fp is need after reload.  */
  if (frame_pointer_needed)
    cfun->machine->fp_as_gp_p = false;

  /* If this is a variadic function, first we need to push argument
     registers that hold the unnamed argument value.  */
  if (cfun->machine->va_args_size != 0)
    {
      Rb = cfun->machine->va_args_first_regno;
      Re = cfun->machine->va_args_last_regno;
      /* No need to push $fp, $gp, or $lp.  */
      nds32_emit_stack_push_multiple (Rb, Re, false, false, false, true);

      /* We may also need to adjust stack pointer for padding bytes
         because varargs may cause $sp not 8-byte aligned.  */
      if (cfun->machine->va_args_area_padding_bytes)
	{
	  /* Generate sp adjustment instruction.  */
	  sp_adjust = cfun->machine->va_args_area_padding_bytes;

	  nds32_emit_adjust_frame (stack_pointer_rtx,
				   stack_pointer_rtx,
				   -1 * sp_adjust);
	}
    }

  /* If the function is 'naked',
     we do not have to generate prologue code fragment.  */
  if (cfun->machine->naked_p && !flag_pic)
    return;

  /* Get callee_first_regno and callee_last_regno.  */
  Rb = cfun->machine->callee_saved_first_gpr_regno;
  Re = cfun->machine->callee_saved_last_gpr_regno;

  /* If $fp, $gp, $lp, and all callee-save registers are NOT required
     to be saved, we don't have to create multiple push instruction.
     Otherwise, a multiple push instruction is needed.  */
  if (!(Rb == SP_REGNUM && Re == SP_REGNUM
	&& cfun->machine->fp_size == 0
	&& cfun->machine->gp_size == 0
	&& cfun->machine->lp_size == 0))
    {
      /* Create multiple push instruction rtx.  */
      nds32_emit_stack_push_multiple (
	Rb, Re,
	cfun->machine->fp_size, cfun->machine->gp_size, cfun->machine->lp_size,
	false);
    }

  /* Save eh data registers.  */
  if (cfun->machine->use_eh_return_p)
    {
      Rb = cfun->machine->eh_return_data_first_regno;
      Re = cfun->machine->eh_return_data_last_regno;

      /* No need to push $fp, $gp, or $lp.
	 Also, this is not variadic arguments push.  */
      nds32_emit_stack_push_multiple (Rb, Re, false, false, false, false);
    }

  /* Check frame_pointer_needed to see
     if we shall emit fp adjustment instruction.  */
  if (frame_pointer_needed)
    {
      /* adjust $fp = $sp + ($fp size) + ($gp size) + ($lp size)
			  + (4 * callee-saved-registers)
			  + (4 * exception-handling-data-registers)
	 Note: No need to adjust
	       cfun->machine->callee_saved_area_gpr_padding_bytes,
	       because, at this point, stack pointer is just
	       at the position after push instruction.  */
      fp_adjust = cfun->machine->fp_size
		  + cfun->machine->gp_size
		  + cfun->machine->lp_size
		  + cfun->machine->callee_saved_gpr_regs_size
		  + cfun->machine->eh_return_data_regs_size;

      nds32_emit_adjust_frame (hard_frame_pointer_rtx,
			       stack_pointer_rtx,
			       fp_adjust);
    }

  /* Save fpu registers.  */
  if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
    {
      /* When $sp moved to bottom of stack, we need to check whether
	 the range of offset in the FPU instruction.  */
      int fpr_offset = cfun->machine->local_size
		       + cfun->machine->out_args_size
		       + cfun->machine->callee_saved_fpr_regs_size;

      /* Check FPU instruction offset imm14s.  */
      if (!satisfies_constraint_Is14 (GEN_INT (fpr_offset)))
	{
	  int fpr_space = cfun->machine->callee_saved_area_gpr_padding_bytes
			  + cfun->machine->callee_saved_fpr_regs_size;

	  /* Save fpu registers, need to allocate stack space
	     for fpu callee registers.  And now $sp position
	     on callee saved fpr registers.  */
	  nds32_emit_adjust_frame (stack_pointer_rtx,
				   stack_pointer_rtx,
				   -1 * fpr_space);

	  /* Emit fpu store instruction, using [$sp + offset] store
	     fpu registers.  */
	  nds32_emit_push_fpr_callee_saved (0);

          /* Adjust $sp = $sp - local_size - out_args_size.  */
	  sp_adjust = cfun->machine->local_size
		      + cfun->machine->out_args_size;

	  /* Allocate stack space for local size and out args size.  */
	  nds32_emit_adjust_frame (stack_pointer_rtx,
				   stack_pointer_rtx,
				   -1 * sp_adjust);
	}
      else
	{
	  /* Offset range in Is14, so $sp moved to bottom of stack.  */

          /* Adjust $sp = $sp - local_size - out_args_size
			      - callee_saved_area_gpr_padding_bytes
			      - callee_saved_fpr_regs_size.  */
	  sp_adjust = cfun->machine->local_size
		      + cfun->machine->out_args_size
		      + cfun->machine->callee_saved_area_gpr_padding_bytes
		      + cfun->machine->callee_saved_fpr_regs_size;

	  nds32_emit_adjust_frame (stack_pointer_rtx,
				   stack_pointer_rtx,
				   -1 * sp_adjust);

	  /* Emit fpu store instruction, using [$sp + offset] store
	     fpu registers.  */
	  int fpr_position = cfun->machine->out_args_size
			     + cfun->machine->local_size;
	  nds32_emit_push_fpr_callee_saved (fpr_position);
	}
    }
  else
    {
      /* Adjust $sp = $sp - local_size - out_args_size
			  - callee_saved_area_gpr_padding_bytes.  */
      sp_adjust = cfun->machine->local_size
		  + cfun->machine->out_args_size
		  + cfun->machine->callee_saved_area_gpr_padding_bytes;

      /* sp_adjust value may be out of range of the addi instruction,
	 create alternative add behavior with TA_REGNUM if necessary,
	 using NEGATIVE value to tell that we are decreasing address.  */
      nds32_emit_adjust_frame (stack_pointer_rtx,
			       stack_pointer_rtx,
			       -1 * sp_adjust);
    }

  /* Emit gp setup instructions for -fpic.  */
  if (flag_pic && df_regs_ever_live_p (PIC_OFFSET_TABLE_REGNUM))
    nds32_emit_load_gp ();

  /* If user applies -mno-sched-prolog-epilog option,
     we need to prevent instructions of function body from being
     scheduled with stack adjustment in prologue.  */
  if (!flag_sched_prolog_epilog)
    emit_insn (gen_blockage ());
}

/* Function for normal multiple pop epilogue.  */
void
nds32_expand_epilogue (bool sibcall_p)
{
  int sp_adjust;
  unsigned Rb, Re;

  /* Compute and setup stack frame size.
     The result will be in cfun->machine.  */
  nds32_compute_stack_frame ();

  /* If user applies -mno-sched-prolog-epilog option,
     we need to prevent instructions of function body from being
     scheduled with stack adjustment in epilogue.  */
  if (!flag_sched_prolog_epilog)
    emit_insn (gen_blockage ());

  /* If the function is 'naked', we do not have to generate
     epilogue code fragment BUT 'ret' instruction.
     However, if this function is also a variadic function,
     we need to create adjust stack pointer before 'ret' instruction.  */
  if (cfun->machine->naked_p)
    {
      /* If this is a variadic function, we do not have to restore argument
	 registers but need to adjust stack pointer back to previous stack
	 frame location before return.  */
      if (cfun->machine->va_args_size != 0)
	{
	  /* Generate sp adjustment instruction.
	     We  need to consider padding bytes here.  */
	  sp_adjust = cfun->machine->va_args_size
		      + cfun->machine->va_args_area_padding_bytes;

	  nds32_emit_adjust_frame (stack_pointer_rtx,
				   stack_pointer_rtx,
				   sp_adjust);
	}

      /* Generate return instruction by using 'return_internal' pattern.
	 Make sure this instruction is after gen_blockage().  */
      if (!sibcall_p)
	{
	  /* We need to further check attributes to determine whether
	     there should be return instruction at epilogue.
	     If the attribute naked exists but -mno-ret-in-naked-func
	     is issued, there is NO need to generate return instruction.  */
	  if (cfun->machine->attr_naked_p && !flag_ret_in_naked_func)
	    return;

	  emit_jump_insn (gen_return_internal ());
	}
      return;
    }

  if (frame_pointer_needed)
    {
      /* Restore fpu registers.  */
      if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	{
	  int gpr_padding = cfun->machine->callee_saved_area_gpr_padding_bytes;

	  /* adjust $sp = $fp - ($fp size) - ($gp size) - ($lp size)
			      - (4 * callee-saved-registers)
			      - (4 * exception-handling-data-registers)
			      - (4 * callee-saved-gpr-registers padding byte)
			      - (4 * callee-saved-fpr-registers)
	     Note:  we want to adjust stack pointer
		    to the position for callee-saved fpr register,
		    And restore fpu register use .bi instruction to adjust $sp
		    from callee-saved fpr register to pop instruction.  */
	  sp_adjust = cfun->machine->fp_size
		      + cfun->machine->gp_size
		      + cfun->machine->lp_size
		      + cfun->machine->callee_saved_gpr_regs_size
		      + cfun->machine->eh_return_data_regs_size
		      + cfun->machine->callee_saved_area_gpr_padding_bytes
		      + cfun->machine->callee_saved_fpr_regs_size;

	  nds32_emit_adjust_frame (stack_pointer_rtx,
				   hard_frame_pointer_rtx,
				   -1 * sp_adjust);

	  /* Emit fpu load instruction, using .bi instruction
	     load fpu registers.  */
	  nds32_emit_pop_fpr_callee_saved (gpr_padding);
	}
      else
	{
	  /* adjust $sp = $fp - ($fp size) - ($gp size) - ($lp size)
			      - (4 * callee-saved-registers)
			      - (4 * exception-handling-data-registers)
	     Note: No need to adjust
		   cfun->machine->callee_saved_area_gpr_padding_bytes,
		   because we want to adjust stack pointer
		   to the position for pop instruction.  */
	  sp_adjust = cfun->machine->fp_size
		      + cfun->machine->gp_size
		      + cfun->machine->lp_size
		      + cfun->machine->callee_saved_gpr_regs_size
		      + cfun->machine->eh_return_data_regs_size;

	  nds32_emit_adjust_frame (stack_pointer_rtx,
				   hard_frame_pointer_rtx,
				   -1 * sp_adjust);
	}
    }
  else
    {
      /* Restore fpu registers.  */
      if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	{
	  int gpr_padding = cfun->machine->callee_saved_area_gpr_padding_bytes;

	  /* Adjust $sp = $sp + local_size + out_args_size.  */
	  sp_adjust = cfun->machine->local_size
		      + cfun->machine->out_args_size;

	  nds32_emit_adjust_frame (stack_pointer_rtx,
				   stack_pointer_rtx,
				   sp_adjust);

	  /* Emit fpu load instruction, using .bi instruction
	     load fpu registers, and adjust $sp from callee-saved fpr register
	     to callee-saved gpr register.  */
	  nds32_emit_pop_fpr_callee_saved (gpr_padding);
	}
      else
	{
	  /* If frame pointer is NOT needed,
	     we cannot calculate the sp adjustment from frame pointer.
	     Instead, we calculate the adjustment by local_size,
	     out_args_size, and callee_saved_area_gpr_padding_bytes.
	     Notice that such sp adjustment value may be out of range,
	     so we have to deal with it as well.  */

	  /* Adjust $sp = $sp + local_size + out_args_size
			      + callee_saved_area_gpr_padding_bytes.  */
	  sp_adjust = cfun->machine->local_size
		      + cfun->machine->out_args_size
		      + cfun->machine->callee_saved_area_gpr_padding_bytes;

	  nds32_emit_adjust_frame (stack_pointer_rtx,
				   stack_pointer_rtx,
				   sp_adjust);
	}
    }

  /* Restore eh data registers.  */
  if (cfun->machine->use_eh_return_p)
    {
      Rb = cfun->machine->eh_return_data_first_regno;
      Re = cfun->machine->eh_return_data_last_regno;

      /* No need to pop $fp, $gp, or $lp.  */
      nds32_emit_stack_pop_multiple (Rb, Re, false, false, false);
    }

  /* Get callee_first_regno and callee_last_regno.  */
  Rb = cfun->machine->callee_saved_first_gpr_regno;
  Re = cfun->machine->callee_saved_last_gpr_regno;

  /* If $fp, $gp, $lp, and all callee-save registers are NOT required
     to be saved, we don't have to create multiple pop instruction.
     Otherwise, a multiple pop instruction is needed.  */
  if (!(Rb == SP_REGNUM && Re == SP_REGNUM
	&& cfun->machine->fp_size == 0
	&& cfun->machine->gp_size == 0
	&& cfun->machine->lp_size == 0))
    {
      /* Create multiple pop instruction rtx.  */
      nds32_emit_stack_pop_multiple (
	Rb, Re,
	cfun->machine->fp_size, cfun->machine->gp_size, cfun->machine->lp_size);
    }

  /* If this is a variadic function, we do not have to restore argument
     registers but need to adjust stack pointer back to previous stack
     frame location before return.  */
  if (cfun->machine->va_args_size != 0)
    {
      /* Generate sp adjustment instruction.
	 We need to consider padding bytes here.  */
      sp_adjust = cfun->machine->va_args_size
		  + cfun->machine->va_args_area_padding_bytes;

      nds32_emit_adjust_frame (stack_pointer_rtx,
			       stack_pointer_rtx,
			       sp_adjust);
    }

  /* If this function uses __builtin_eh_return, make stack adjustment
     for exception handler.  */
  if (cfun->machine->use_eh_return_p)
    {
      /* We need to unwind the stack by the offset computed by
	 EH_RETURN_STACKADJ_RTX.  However, at this point the CFA is
	 based on SP.  Ideally we would update the SP and define the
	 CFA along the lines of:

	 SP = SP + EH_RETURN_STACKADJ_RTX
	 (regnote CFA = SP - EH_RETURN_STACKADJ_RTX)

	 However the dwarf emitter only understands a constant
	 register offset.

	 The solution chosen here is to use the otherwise $ta ($r15)
	 as a temporary register to hold the current SP value.  The
	 CFA is described using $ta then SP is modified.  */

      rtx ta_reg;
      rtx insn;

      ta_reg = gen_rtx_REG (SImode, TA_REGNUM);

      insn = emit_move_insn (ta_reg, stack_pointer_rtx);
      add_reg_note (insn, REG_CFA_DEF_CFA, ta_reg);
      RTX_FRAME_RELATED_P (insn) = 1;

      emit_insn (gen_addsi3 (stack_pointer_rtx,
			     stack_pointer_rtx,
			     EH_RETURN_STACKADJ_RTX));

      /* Ensure the assignment to $ta does not get optimized away.  */
      emit_use (ta_reg);
    }

  /* Generate return instruction.  */
  if (!sibcall_p)
    emit_jump_insn (gen_return_internal ());
}

/* Function for v3push prologue.  */
void
nds32_expand_prologue_v3push (void)
{
  int fp_adjust;
  int sp_adjust;
  int fpr_space = 0;
  unsigned Rb, Re;

  /* Compute and setup stack frame size.
     The result will be in cfun->machine.  */
  nds32_compute_stack_frame ();

  if (cfun->machine->callee_saved_gpr_regs_size > 0)
    df_set_regs_ever_live (FP_REGNUM, 1);

  /* Check frame_pointer_needed again to prevent fp is need after reload.  */
  if (frame_pointer_needed)
    cfun->machine->fp_as_gp_p = false;

  /* If the function is 'naked',
     we do not have to generate prologue code fragment.  */
  if (cfun->machine->naked_p && !flag_pic)
    return;

  /* Get callee_first_regno and callee_last_regno.  */
  Rb = cfun->machine->callee_saved_first_gpr_regno;
  Re = cfun->machine->callee_saved_last_gpr_regno;

  /* Calculate sp_adjust first to test if 'push25 Re,imm8u' is available,
     where imm8u has to be 8-byte alignment.  */
  sp_adjust = cfun->machine->local_size
	      + cfun->machine->out_args_size
	      + cfun->machine->callee_saved_area_gpr_padding_bytes
	      + cfun->machine->callee_saved_fpr_regs_size;

  if (satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
      && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust))
    {
      /* We can use 'push25 Re,imm8u'.  */

      /* nds32_emit_stack_v3push(last_regno, sp_adjust),
	 the pattern 'stack_v3push' is implemented in nds32.md.  */
      nds32_emit_stack_v3push (Rb, Re, sp_adjust);

      /* Save fpu registers.  */
      if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	{
	  /* Calculate fpr position.  */
	  int fpr_position = cfun->machine->local_size
			     + cfun->machine->out_args_size;
	  /* Emit fpu store instruction, using [$sp + offset] store
	     fpu registers.  */
	  nds32_emit_push_fpr_callee_saved (fpr_position);
	}

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
		      + cfun->machine->callee_saved_gpr_regs_size
		      + sp_adjust;

	  nds32_emit_adjust_frame (hard_frame_pointer_rtx,
				   stack_pointer_rtx,
				   fp_adjust);
	}
    }
  else
    {
      if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	{
	  /* Calculate fpr space.  */
	  fpr_space = cfun->machine->callee_saved_area_gpr_padding_bytes
		      + cfun->machine->callee_saved_fpr_regs_size;

	  /* We have to use 'push25 Re, fpr_space', to pre-allocate
	     callee saved fpr registers space.  */
	  nds32_emit_stack_v3push (Rb, Re, fpr_space);
	  nds32_emit_push_fpr_callee_saved (0);
	}
      else
	{
	  /* We have to use 'push25 Re,0' and
	     expand one more instruction to adjust $sp later.  */

	  /* nds32_emit_stack_v3push(last_regno, sp_adjust),
	     the pattern 'stack_v3push' is implemented in nds32.md.  */
	  nds32_emit_stack_v3push (Rb, Re, 0);
	}

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
		      + cfun->machine->callee_saved_gpr_regs_size;

	  if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	    {
	      /* We use 'push25 Re, fpr_space', the $sp is
		 on callee saved fpr position, so need to consider
		 fpr space.  */
	      fp_adjust = fp_adjust + fpr_space;
	    }

	  nds32_emit_adjust_frame (hard_frame_pointer_rtx,
				   stack_pointer_rtx,
				   fp_adjust);
	}

      if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	{
	  /* We use 'push25 Re, fpr_space',
	     the $sp is on callee saved fpr position,
	     no need to consider fpr space.  */
	  sp_adjust = sp_adjust - fpr_space;
	}

      /* Because we use 'push25 Re,0',
	 we need to expand one more instruction to adjust $sp.
	 using NEGATIVE value to tell that we are decreasing address.  */
      nds32_emit_adjust_frame (stack_pointer_rtx,
			       stack_pointer_rtx,
			       -1 * sp_adjust);
    }

  /* Emit gp setup instructions for -fpic.  */
  if (flag_pic && df_regs_ever_live_p (PIC_OFFSET_TABLE_REGNUM))
    nds32_emit_load_gp ();

  /* Prevent the instruction scheduler from
     moving instructions across the boundary.  */
  emit_insn (gen_blockage ());
}

/* Function for v3pop epilogue.  */
void
nds32_expand_epilogue_v3pop (bool sibcall_p)
{
  int sp_adjust;
  unsigned Rb, Re;

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
      /* Generate return instruction by using 'return_internal' pattern.
	 Make sure this instruction is after gen_blockage().
	 First we need to check this is a function without sibling call.  */
      if (!sibcall_p)
	{
	  /* We need to further check attributes to determine whether
	     there should be return instruction at epilogue.
	     If the attribute naked exists but -mno-ret-in-naked-func
	     is issued, there is NO need to generate return instruction.  */
	  if (cfun->machine->attr_naked_p && !flag_ret_in_naked_func)
	    return;

	  emit_jump_insn (gen_return_internal ());
	}
      return;
    }

  /* Get callee_first_regno and callee_last_regno.  */
  Rb = cfun->machine->callee_saved_first_gpr_regno;
  Re = cfun->machine->callee_saved_last_gpr_regno;

  /* Calculate sp_adjust first to test if 'pop25 Re,imm8u' is available,
     where imm8u has to be 8-byte alignment.  */
  sp_adjust = cfun->machine->local_size
	      + cfun->machine->out_args_size
	      + cfun->machine->callee_saved_area_gpr_padding_bytes
	      + cfun->machine->callee_saved_fpr_regs_size;

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
      /* Restore fpu registers.  */
      if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	{
	  int fpr_position = cfun->machine->local_size
			     + cfun->machine->out_args_size;
	  /* Emit fpu load instruction, using [$sp + offset] restore
	     fpu registers.  */
	  nds32_emit_v3pop_fpr_callee_saved (fpr_position);
	}

      /* We can use 'pop25 Re,imm8u'.  */

      /* nds32_emit_stack_v3pop(last_regno, sp_adjust),
	 the pattern 'stack_v3pop' is implementad in nds32.md.  */
      nds32_emit_stack_v3pop (Rb, Re, sp_adjust);
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
		   cfun->machine->callee_saved_area_gpr_padding_bytes,
		   because we want to adjust stack pointer
		   to the position for pop instruction.  */
	  sp_adjust = cfun->machine->fp_size
		      + cfun->machine->gp_size
		      + cfun->machine->lp_size
		      + cfun->machine->callee_saved_gpr_regs_size;

	  /* Restore fpu registers.  */
	  if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	    {
	      /* Set $sp to callee saved fpr position, we need to restore
		 fpr registers.  */
	      sp_adjust = sp_adjust
			  + cfun->machine->callee_saved_area_gpr_padding_bytes
			  + cfun->machine->callee_saved_fpr_regs_size;

	      nds32_emit_adjust_frame (stack_pointer_rtx,
				       hard_frame_pointer_rtx,
				       -1 * sp_adjust);

	      /* Emit fpu load instruction, using [$sp + offset] restore
		 fpu registers.  */
	      nds32_emit_v3pop_fpr_callee_saved (0);
	    }
	  else
	    {
	      nds32_emit_adjust_frame (stack_pointer_rtx,
				       hard_frame_pointer_rtx,
				       -1 * sp_adjust);
	    }
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
			      + callee_saved_area_gpr_padding_bytes
			      + callee_saved_fpr_regs_size.  */
	  sp_adjust = cfun->machine->local_size
		      + cfun->machine->out_args_size
		      + cfun->machine->callee_saved_area_gpr_padding_bytes
		      + cfun->machine->callee_saved_fpr_regs_size;

	  /* Restore fpu registers.  */
	  if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	    {
	      /* Set $sp to callee saved fpr position, we need to restore
		 fpr registers.  */
	      sp_adjust = sp_adjust
			  - cfun->machine->callee_saved_area_gpr_padding_bytes
			  - cfun->machine->callee_saved_fpr_regs_size;

	      nds32_emit_adjust_frame (stack_pointer_rtx,
				       stack_pointer_rtx,
				       sp_adjust);

	      /* Emit fpu load instruction, using [$sp + offset] restore
		 fpu registers.  */
	      nds32_emit_v3pop_fpr_callee_saved (0);
	    }
	  else
	    {
	       /* sp_adjust value may be out of range of the addi instruction,
		  create alternative add behavior with TA_REGNUM if necessary,
		  using POSITIVE value to tell that we are increasing
		  address.  */
	      nds32_emit_adjust_frame (stack_pointer_rtx,
				       stack_pointer_rtx,
				       sp_adjust);
	    }
	}

      if (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)
	{
	  /* We have fpr need to restore, so $sp is set on callee saved fpr
	     position.  And we use 'pop25 Re, fpr_space' to adjust $sp.  */
	  int fpr_space = cfun->machine->callee_saved_area_gpr_padding_bytes
			  + cfun->machine->callee_saved_fpr_regs_size;
	  nds32_emit_stack_v3pop (Rb, Re, fpr_space);
	}
      else
	{
	  /* nds32_emit_stack_v3pop(last_regno, sp_adjust),
	     the pattern 'stack_v3pop' is implementad in nds32.md.  */
	  nds32_emit_stack_v3pop (Rb, Re, 0);
	}
    }
  /* Generate return instruction.  */
  emit_jump_insn (gen_pop25return ());
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */
int
nds32_can_use_return_insn (void)
{
  int sp_adjust;

  /* Prior to reloading, we can't tell how many registers must be saved.
     Thus we cannot determine whether this function has null epilogue.  */
  if (!reload_completed)
    return 0;

  /* If attribute 'naked' appears but -mno-ret-in-naked-func is used,
     we cannot use return instruction.  */
  if (cfun->machine->attr_naked_p && !flag_ret_in_naked_func)
    return 0;

  sp_adjust = cfun->machine->local_size
	      + cfun->machine->out_args_size
	      + cfun->machine->callee_saved_area_gpr_padding_bytes
	      + cfun->machine->callee_saved_fpr_regs_size;
  if (!cfun->machine->fp_as_gp_p
      && satisfies_constraint_Iu08 (GEN_INT (sp_adjust))
      && NDS32_DOUBLE_WORD_ALIGN_P (sp_adjust)
      && !cfun->calls_alloca
      && NDS32_V3PUSH_AVAILABLE_P
      && !(TARGET_HARD_FLOAT
	   && (cfun->machine->callee_saved_first_fpr_regno != SP_REGNUM)))
    return 1;

  /* If no stack was created, two conditions must be satisfied:
     1. This is a naked function.
	So there is no callee-saved, local size, or outgoing size.
     2. This is NOT a variadic function.
	So there is no pushing arguement registers into the stack.  */
  return (cfun->machine->naked_p && (cfun->machine->va_args_size == 0));
}

scalar_int_mode
nds32_case_vector_shorten_mode (int min_offset, int max_offset,
				rtx body ATTRIBUTE_UNUSED)
{
  if (min_offset < 0 || max_offset >= 0x2000)
    return SImode;
  else
    {
      /* The jump table maybe need to 2 byte alignment,
	 so reserved 1 byte for check max_offset.  */
      if (max_offset >= 0xff)
	return HImode;
      else
	return QImode;
    }
}

/* ------------------------------------------------------------------------ */

/* Return alignment for the label.  */
int
nds32_target_alignment (rtx_insn *label)
{
  rtx_insn *insn;

  if (!NDS32_ALIGN_P ())
    return 0;

  insn = next_active_insn (label);

  /* Always align to 4 byte when first instruction after label is jump
     instruction since length for that might changed, so let's always align
     it for make sure we don't lose any perfomance here.  */
  if (insn == 0
      || (get_attr_length (insn) == 2
	  && !JUMP_P (insn) && !CALL_P (insn)))
    return 0;
  else
    return 2;
}

/* Return alignment for data.  */
unsigned int
nds32_data_alignment (tree data,
		      unsigned int basic_align)
{
  if ((basic_align < BITS_PER_WORD)
      && (TREE_CODE (data) == ARRAY_TYPE
	 || TREE_CODE (data) == UNION_TYPE
	 || TREE_CODE (data) == RECORD_TYPE))
    return BITS_PER_WORD;
  else
    return basic_align;
}

/* Return alignment for constant value.  */
static HOST_WIDE_INT
nds32_constant_alignment (const_tree constant,
			  HOST_WIDE_INT basic_align)
{
  /* Make string literal and constant for constructor to word align.  */
  if (((TREE_CODE (constant) == STRING_CST
	|| TREE_CODE (constant) == CONSTRUCTOR
	|| TREE_CODE (constant) == UNION_TYPE
	|| TREE_CODE (constant) == RECORD_TYPE
	|| TREE_CODE (constant) == ARRAY_TYPE)
       && basic_align < BITS_PER_WORD))
    return BITS_PER_WORD;
  else
    return basic_align;
}

/* Return alignment for local variable.  */
unsigned int
nds32_local_alignment (tree local ATTRIBUTE_UNUSED,
		       unsigned int basic_align)
{
  bool at_least_align_to_word = false;
  /* Make local array, struct and union at least align to word for make
     sure it can unroll memcpy when initialize by constant.  */
  switch (TREE_CODE (local))
    {
    case ARRAY_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
      at_least_align_to_word = true;
      break;
    default:
      at_least_align_to_word = false;
      break;
    }
  if (at_least_align_to_word
      && (basic_align < BITS_PER_WORD))
    return BITS_PER_WORD;
  else
    return basic_align;
}

bool
nds32_split_double_word_load_store_p(rtx *operands, bool load_p)
{
  rtx mem = load_p ? operands[1] : operands[0];
  /* Do split at split2 if -O0 or schedule 2 not enable.  */
  if (optimize == 0 || !flag_schedule_insns_after_reload)
    return !satisfies_constraint_Da (mem) || MEM_VOLATILE_P (mem);

  /* Split double word load store after copy propgation.  */
  if (current_pass == NULL)
    return false;

  const char *pass_name = current_pass->name;
  if (pass_name && ((strcmp (pass_name, "split3") == 0)
		     || (strcmp (pass_name, "split5") == 0)))
    return !satisfies_constraint_Da (mem) || MEM_VOLATILE_P (mem);

  return false;
}

static bool
nds32_use_blocks_for_constant_p (machine_mode mode,
				 const_rtx x ATTRIBUTE_UNUSED)
{
  if ((TARGET_FPU_SINGLE || TARGET_FPU_DOUBLE)
      && (mode == DFmode || mode == SFmode))
    return true;
  else
    return false;
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

#undef TARGET_EXPAND_TO_RTL_HOOK
#define TARGET_EXPAND_TO_RTL_HOOK nds32_expand_to_rtl_hook

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT nds32_constant_alignment


/* Layout of Source Language Data Types.  */


/* Register Usage.  */

/* -- Basic Characteristics of Registers.  */

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE nds32_conditional_register_usage

/* -- Order of Allocation of Registers.  */

/* -- How Values Fit in Registers.  */

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS nds32_hard_regno_nregs

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK nds32_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P nds32_modes_tieable_p

/* -- Handling Leaf Functions.  */

/* -- Registers That Form a Stack.  */


/* Register Classes.  */

#undef TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS nds32_class_max_nregs

#undef TARGET_REGISTER_PRIORITY
#define TARGET_REGISTER_PRIORITY nds32_register_priority

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS nds32_can_change_mode_class


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

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK nds32_must_pass_in_stack

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES nds32_arg_partial_bytes

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE nds32_function_arg_advance

#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY nds32_function_arg_boundary

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P nds32_vector_mode_supported_p

/* -- How Scalar Function Values Are Returned.  */

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE nds32_function_value

#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE nds32_libcall_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P nds32_function_value_regno_p

/* -- How Large Values Are Returned.  */

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY nds32_return_in_memory

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

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL nds32_function_ok_for_sibcall

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN nds32_warn_func_return

/* Stack smashing protection.  */


/* Implementing the Varargs Macros.  */

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS nds32_setup_incoming_varargs

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

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS nds32_legitimize_address

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P nds32_legitimate_constant_p

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE nds32_vectorize_preferred_simd_mode

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM nds32_cannot_force_const_mem

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS nds32_delegitimize_address


/* Anchored Addresses.  */


/* Condition Code Status.  */

/* -- Representation of condition codes using (cc0).  */

/* -- Representation of condition codes using registers.  */

#undef TARGET_CANONICALIZE_COMPARISON
#define TARGET_CANONICALIZE_COMPARISON nds32_canonicalize_comparison

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

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO nds32_encode_section_info


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

#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA nds32_asm_output_addr_const_extra

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

#undef TARGET_DWARF_REGISTER_SPAN
#define TARGET_DWARF_REGISTER_SPAN nds32_dwarf_register_span

/* -- Assembler Commands for Alignment.  */


/* Controlling Debugging Information Format.  */

/* -- Macros Affecting All Debugging Formats.  */

/* -- Macros for DWARF Output.  */

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

#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS TARGET_LINUX_ABI


/* Defining coprocessor specifics for MIPS targets.  */


/* Parameters for Precompiled Header Validity Checking.  */


/* C++ ABI parameters.  */


/* Adding support for named address spaces.  */


/* Miscellaneous Parameters.  */

#undef TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST nds32_md_asm_adjust

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS nds32_init_builtins

#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL nds32_builtin_decl

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN nds32_expand_builtin

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS nds32_init_libfuncs

#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P nds32_use_blocks_for_constant_p

#undef  TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed

#undef TARGET_DOCUMENTATION_NAME
#define TARGET_DOCUMENTATION_NAME "NDS32"


/* ------------------------------------------------------------------------ */

/* Initialize the GCC target structure.  */

struct gcc_target targetm = TARGET_INITIALIZER;

/* ------------------------------------------------------------------------ */
