/* The fp-as-gp pass of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2020 Free Software Foundation, Inc.
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
#include "hard-reg-set.h"
#include "tm_p.h"
#include "rtl.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "insn-config.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "ira.h"
#include "ira-int.h"
#include "df.h"
#include "tree-core.h"
#include "tree-pass.h"
#include "nds32-protos.h"

/* ------------------------------------------------------------------------ */

/* A helper function to check if this function should contain prologue.  */
static bool
nds32_have_prologue_p (void)
{
  int i;

  for (i = 0; i < 28; i++)
    if (NDS32_REQUIRED_CALLEE_SAVED_P (i))
      return true;

  return (flag_pic
	  || NDS32_REQUIRED_CALLEE_SAVED_P (FP_REGNUM)
	  || NDS32_REQUIRED_CALLEE_SAVED_P (LP_REGNUM));
}

static int
nds32_get_symbol_count (void)
{
  int symbol_count = 0;
  rtx_insn *insn;
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_BB_INSNS (bb, insn)
	{
	  /* Counting the insn number which the addressing mode is symbol.  */
	  if (single_set (insn) && nds32_symbol_load_store_p (insn))
	    {
	      rtx pattern = PATTERN (insn);
	      rtx mem;
	      gcc_assert (GET_CODE (pattern) == SET);
	      if (GET_CODE (SET_SRC (pattern)) == REG )
		mem = SET_DEST (pattern);
	      else
		mem = SET_SRC (pattern);

	      /* We have only lwi37 and swi37 for fp-as-gp optimization,
		 so don't count any other than SImode.
		 MEM for QImode and HImode will wrap by ZERO_EXTEND
		 or SIGN_EXTEND */
	      if (GET_CODE (mem) == MEM)
		symbol_count++;
	    }
	}
    }

  return symbol_count;
}

/* Function to determine whether it is worth to do fp_as_gp optimization.
   Return false: It is NOT worth to do fp_as_gp optimization.
   Return true: It is APPROXIMATELY worth to do fp_as_gp optimization.
   Note that if it is worth to do fp_as_gp optimization,
   we MUST set FP_REGNUM ever live in this function.  */
static bool
nds32_fp_as_gp_check_available (void)
{
  basic_block bb;
  basic_block exit_bb;
  edge_iterator ei;
  edge e;
  bool first_exit_blocks_p;

  /* If there exists ANY of following conditions,
     we DO NOT perform fp_as_gp optimization:
       1. TARGET_FORBID_FP_AS_GP is set
	  regardless of the TARGET_FORCE_FP_AS_GP.
       2. User explicitly uses 'naked'/'no_prologue' attribute.
	  We use nds32_naked_function_p() to help such checking.
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
      || nds32_naked_function_p (current_function_decl)
      || !optimize_size
      || frame_pointer_needed
      || NDS32_REQUIRED_CALLEE_SAVED_P (FP_REGNUM)
      || (cfun->stdarg == 1)
      || (find_fallthru_edge (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds) == NULL))
    return false;

  /* Disable fp_as_gp if there is any infinite loop since the fp may
     reuse in infinite loops by register rename.
     For check infinite loops we should make sure exit_bb is post dominate
     all other basic blocks if there is no infinite loops.  */
  first_exit_blocks_p = true;
  exit_bb = NULL;

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    {
      /* More than one exit block also do not perform fp_as_gp optimization.  */
      if (!first_exit_blocks_p)
	return false;

      exit_bb = e->src;
      first_exit_blocks_p = false;
    }

  /* Not found exit_bb? just abort fp_as_gp!  */
  if (!exit_bb)
    return false;

  /* Each bb should post dominate by exit_bb if there is no infinite loop! */
  FOR_EACH_BB_FN (bb, cfun)
    {
      if (!dominated_by_p (CDI_POST_DOMINATORS,
			   bb,
			   exit_bb))
	return false;
    }

  /* Now we can check the possibility of using fp_as_gp optimization.  */
  if (TARGET_FORCE_FP_AS_GP)
    {
      /* User explicitly issues -mforce-fp-as-gp option.  */
      return true;
    }
  else
    {
      /* In the following we are going to evaluate whether
	 it is worth to do fp_as_gp optimization.  */
      bool good_gain = false;
      int symbol_count;

      int threshold;

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

      symbol_count = nds32_get_symbol_count ();

      if (symbol_count >= threshold)
	good_gain = true;

      /* Enable fp_as_gp optimization when potential gain is good enough.  */
      return good_gain;
    }
}

static unsigned int
nds32_fp_as_gp (void)
{
  bool fp_as_gp_p;
  calculate_dominance_info (CDI_POST_DOMINATORS);
  fp_as_gp_p = nds32_fp_as_gp_check_available ();

  /* Here is a hack to IRA for enable/disable a hard register per function.
     We *MUST* review this way after migrate gcc 4.9! */
  if (fp_as_gp_p) {
    SET_HARD_REG_BIT(this_target_ira_int->x_no_unit_alloc_regs, FP_REGNUM);
    df_set_regs_ever_live (FP_REGNUM, 1);
  } else {
    CLEAR_HARD_REG_BIT(this_target_ira_int->x_no_unit_alloc_regs, FP_REGNUM);
  }

  cfun->machine->fp_as_gp_p = fp_as_gp_p;

  free_dominance_info (CDI_POST_DOMINATORS);
  return 1;
}

const pass_data pass_data_nds32_fp_as_gp =
{
  RTL_PASS,				/* type */
  "fp_as_gp",				/* name */
  OPTGROUP_NONE,			/* optinfo_flags */
  TV_MACH_DEP,				/* tv_id */
  0,					/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
};

class pass_nds32_fp_as_gp : public rtl_opt_pass
{
public:
  pass_nds32_fp_as_gp (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_nds32_fp_as_gp, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *)
  {
    return !TARGET_LINUX_ABI
	   && TARGET_16_BIT
	   && optimize_size;
  }
  unsigned int execute (function *) { return nds32_fp_as_gp (); }
};

rtl_opt_pass *
make_pass_nds32_fp_as_gp (gcc::context *ctxt)
{
  return new pass_nds32_fp_as_gp (ctxt);
}

/* ------------------------------------------------------------------------ */
