/* RISC-V cm.popretz optimization pass.
   Copyright (C) 2025 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/*
   This pass combines "li a0, 0" + "cm.popret" into "cm.popretz" instruction
   for the RISC-V Zcmp extension.

   Rationale:
   ---------
   Ideally, cm.popretz should be generated during prologue/epilogue expansion.
   However, as documented in PR113715 [1], this approach causes shrink-wrapping
   analysis to fail, resulting in incorrect code generation.

   To address this issue, we use a dedicated RTL pass to combine these
   instructions later in the compilation pipeline, after shrink-wrapping has
   completed.

   Why not use peephole2?
   ----------------------
   An alternative approach would be to use a peephole2 pattern to perform this
   optimization. However, between "li a0, 0" and "cm.popret", there can be
   STACK_TIE and other instructions that make it difficult to write a robust
   peephole pattern that handles all cases.

   For example, in RV32, when the return value is in DImode but the low part
   (a0) is zero, this pattern is hard to describe effectively in peephole2.
   Using a dedicated pass gives us more flexibility to handle these cases.

   [1] https://gcc.gnu.org/bugzilla/show_bug.cgi?id=113715  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "emit-rtl.h"
#include "dumpfile.h"
#include "tree-pass.h"
#include "insn-config.h"
#include "insn-opinit.h"
#include "recog.h"

namespace {

const pass_data pass_data_combine_popretz =
{
  RTL_PASS, /* type. */
  "popretz", /* name. */
  OPTGROUP_NONE, /* optinfo_flags. */
  TV_MACH_DEP, /* tv_id. */
  0, /* properties_required. */
  0, /* properties_provided. */
  0, /* properties_destroyed. */
  0, /* todo_flags_start. */
  0, /* todo_flags_finish. */
};

class pass_combine_popretz : public rtl_opt_pass
{
public:
  pass_combine_popretz (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_combine_popretz, ctxt)
  {}

  virtual bool gate (function *)
    {
      return TARGET_ZCMP && !frame_pointer_needed;
    }

  virtual unsigned int execute (function *);
}; // class pass_combine_popretz


/* Check if the given instruction code is a cm.popret instruction.
   Returns true if the code corresponds to any variant of gpr_multi_popret
   (for different register bounds and modes).  */
static bool
riscv_popret_insn_p (int code)
{
#define CASE_CODE_FOR_POPRET_(REG_BOUND, MODE) \
  case CODE_FOR_gpr_multi_popret_up_to_##REG_BOUND##_##MODE:
#define CASE_CODE_FOR_POPRET(REG_BOUND) \
  CASE_CODE_FOR_POPRET_(REG_BOUND, si) \
  CASE_CODE_FOR_POPRET_(REG_BOUND, di)
#define ALL_CASE_CODE_FOR_POPRET \
  CASE_CODE_FOR_POPRET(ra) \
  CASE_CODE_FOR_POPRET(s0) \
  CASE_CODE_FOR_POPRET(s1) \
  CASE_CODE_FOR_POPRET(s2) \
  CASE_CODE_FOR_POPRET(s3) \
  CASE_CODE_FOR_POPRET(s4) \
  CASE_CODE_FOR_POPRET(s5) \
  CASE_CODE_FOR_POPRET(s6) \
  CASE_CODE_FOR_POPRET(s7) \
  CASE_CODE_FOR_POPRET(s8) \
  CASE_CODE_FOR_POPRET(s9) \
  CASE_CODE_FOR_POPRET(s11) \

  switch (code)
    {
    ALL_CASE_CODE_FOR_POPRET
      return true;
    default:
      return false;
    }

#undef CASE_CODE_FOR_POPRET_
#undef CASE_CODE_FOR_POPRET
#undef ALL_CASE_CODE_FOR_POPRET
}

/* Convert a cm.popret instruction code to its corresponding cm.popretz code.
   Given an instruction code for gpr_multi_popret, returns the equivalent
   gpr_multi_popretz instruction code. Returns CODE_FOR_nothing if the
   input is not a valid popret instruction.  */
static int
riscv_code_for_popretz (int code)
{
#define CASE_CODE_FOR_POPRETZ_(REG_BOUND, MODE) \
  case CODE_FOR_gpr_multi_popret_up_to_##REG_BOUND##_##MODE: \
    return CODE_FOR_gpr_multi_popretz_up_to_##REG_BOUND##_##MODE;

#define CASE_CODE_FOR_POPRETZ(REG_BOUND) \
  CASE_CODE_FOR_POPRETZ_(REG_BOUND, si) \
  CASE_CODE_FOR_POPRETZ_(REG_BOUND, di)

#define ALL_CASE_CODE_FOR_POPRETZ \
  CASE_CODE_FOR_POPRETZ(ra) \
  CASE_CODE_FOR_POPRETZ(s0) \
  CASE_CODE_FOR_POPRETZ(s1) \
  CASE_CODE_FOR_POPRETZ(s2) \
  CASE_CODE_FOR_POPRETZ(s3) \
  CASE_CODE_FOR_POPRETZ(s4) \
  CASE_CODE_FOR_POPRETZ(s5) \
  CASE_CODE_FOR_POPRETZ(s6) \
  CASE_CODE_FOR_POPRETZ(s7) \
  CASE_CODE_FOR_POPRETZ(s8) \
  CASE_CODE_FOR_POPRETZ(s9) \
  CASE_CODE_FOR_POPRETZ(s11) \

  switch (code)
    {
    ALL_CASE_CODE_FOR_POPRETZ
    default:
      return CODE_FOR_nothing;
    }

#undef CASE_CODE_FOR_POPRETZ_
#undef CASE_CODE_FOR_POPRETZ
#undef ALL_CASE_CODE_FOR_POPRETZ
}

/* Combine "li a0, 0" with "cm.popret" to form "cm.popretz".

   This pass scans basic blocks that precede the exit block, looking for
   the following pattern:
     1. A cm.popret instruction (function epilogue with return)
     2. A (use a0) pseudo-instruction before the cm.popret
     3. A "li a0, 0" instruction (set a0 to zero) before the use

   When this pattern is found AND a0 is not referenced by any other
   instructions between the "li a0, 0" and the (use a0), we can safely
   combine them into a single cm.popretz instruction, which performs
   the same operations more efficiently.

   This is a late RTL pass that runs before branch shortening.  */
unsigned int
pass_combine_popretz::execute (function *fn)
{
  timevar_push (TV_MACH_DEP);
  edge e;
  edge_iterator ei;

  /* Only visit exit block's pred since popret will only appear there.  */
  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (fn)->preds)
    {
      basic_block bb = e->src;
      rtx_insn *popret_insn = BB_END (bb);
      if (!JUMP_P (popret_insn))
        continue;
      int code = recog_memoized (popret_insn);
      if (!riscv_popret_insn_p (code))
        continue;

      rtx_insn *def_a0_insn = NULL;
      rtx_insn *use_a0_insn = NULL;
      rtx a0_reg = NULL;
      /* Scan backwards from popret to find the pattern:
         1. First, find the (use a0) pseudo-instruction
         2. Continue scanning to find "li a0, 0" (set a0 to const0_rtx)
         3. Ensure a0 is not referenced by any instructions between them
         4. Stop at the first definition of a0 (to ensure we have the
            last/most recent def before the use).  */
      for (rtx_insn *def_insn = PREV_INSN (popret_insn);
	   def_insn && def_insn != PREV_INSN (BB_HEAD (bb));
	   def_insn = PREV_INSN (def_insn))
	{
	  if (!INSN_P (def_insn))
	    continue;
	  rtx def_pat = PATTERN (def_insn);
	  if (GET_CODE (def_pat) == USE
	      && REG_P (XEXP (def_pat, 0))
	      && REGNO (XEXP (def_pat, 0)) == A0_REGNUM)
	    {
	      a0_reg = XEXP (def_pat, 0);
	      use_a0_insn = def_insn;
	      continue;
	    }

	  if (use_a0_insn && reg_referenced_p (a0_reg, def_pat))
	    {
	      /* a0 is used by other instruction before its use in popret.  */
	      use_a0_insn = NULL;
	      break;
	    }

	  if (use_a0_insn
	      && GET_CODE (def_pat) == SET
	      && REG_P (SET_DEST (def_pat))
	      && REGNO (SET_DEST (def_pat)) == A0_REGNUM)
	    {
	      if (SET_SRC (def_pat) == CONST0_RTX (GET_MODE (SET_SRC (def_pat))))
	        def_a0_insn = def_insn;
	      /* Stop the search regardless of the value assigned to a0,
	         because we only want to match the last (most recent)
	         definition of a0 before the (use a0).  */
	      break;
	    }
	  }

        /* If we found a def of a0 before its use, and the value is zero,
	   we can replace the popret with popretz.  */
	if (!def_a0_insn || !use_a0_insn)
	  continue;

	int code_for_popretz = riscv_code_for_popretz (code);
	gcc_assert (code_for_popretz != CODE_FOR_nothing);

	/* Extract the stack adjustment value from the popret instruction.
	   The popret pattern is a PARALLEL, and the first element is the
	   stack pointer adjustment: (set sp (plus sp const_int)).  */
	rtx stack_adj_rtx = XVECEXP (PATTERN (popret_insn), 0, 0);
	gcc_assert (GET_CODE (stack_adj_rtx) == SET
		    && REG_P (SET_DEST (stack_adj_rtx))
		    && REGNO (SET_DEST (stack_adj_rtx)) == SP_REGNUM
		    && GET_CODE (SET_SRC (stack_adj_rtx)) == PLUS
		    && CONST_INT_P (XEXP (SET_SRC (stack_adj_rtx), 1)));

	rtx stack_adj_val = XEXP (SET_SRC (stack_adj_rtx), 1);

	/* Generate and insert the popretz instruction at the position of
	   the original popret. emit_insn_after places the new instruction
	   after PREV_INSN(popret_insn).  */
	rtx popretz = GEN_FCN (code_for_popretz) (stack_adj_val);
	emit_insn_after (popretz, PREV_INSN (popret_insn));

	/* Clean up those instructions.  */
	remove_insn (popret_insn);
	remove_insn (use_a0_insn);
	remove_insn (def_a0_insn);
    }

  timevar_pop (TV_MACH_DEP);
  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_combine_popretz (gcc::context *ctxt)
{
  return new pass_combine_popretz (ctxt);
}
