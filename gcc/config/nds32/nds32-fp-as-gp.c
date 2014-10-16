/* The fp-as-gp pass of Andes NDS32 cpu for GNU compiler
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

/* ------------------------------------------------------------------------ */

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
#include "hashtab.h"
#include "hash-set.h"
#include "vec.h"
#include "machmode.h"
#include "input.h"
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
#include "builtins.h"

/* ------------------------------------------------------------------------ */

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

/* Return true if is load/store with SYMBOL_REF addressing mode
   and memory mode is SImode.  */
static bool
nds32_symbol_load_store_p (rtx_insn *insn)
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
      rtx_insn *insn;

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

/* ------------------------------------------------------------------------ */
