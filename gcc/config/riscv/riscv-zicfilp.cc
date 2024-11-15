/* Branch Target Identification for RISCV architecture.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
   Based on ARM target.

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

#define IN_TARGET_CODE 1

#include "config.h"
#define INCLUDE_STRING
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "expr.h"
#include "emit-rtl.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "dumpfile.h"
#include "rtl-iter.h"
#include "cfgrtl.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "output.h"

/* This pass implements forward-CFI landing pad checks for RISCV. This is
   a security feature similar to BTI (branch target identification) in
   AArch64 and IBT (indirect branch tracking)in X86. A LPAD (landing-pad
   check) instruction is used to guard against the execution of
   instructions which are not the intended target of an indirect branch.

   When forward-CFI is disabled or unimplemented in the CPU, the
   landing-pad check label instructions behave as NOP. When implemented in
   the CPU, and enabled, the destination of an indirect branch must be
   LPAD insn. Otherwise, the CPU reaises an exception.

   In order to enable this mechanism, this pass iterates through the
   control flow of the code and adds appropriate LPAD instructions at the
   beginning of any function that can be called indirectly, and for targets
   of indirect jumps, i.e., jump table targets, non-local goto targets, and
   labels that might be referenced by variables, constant pools, etc
   (NOTE_INSN_DELETED_LABEL). */

namespace {

const pass_data pass_data_insert_landing_pad =
{
  RTL_PASS, /* type. */
  "zisslpcfi", /* name. */
  OPTGROUP_NONE, /* optinfo_flags. */
  TV_MACH_DEP, /* tv_id. */
  0, /* properties_required. */
  0, /* properties_provided. */
  0, /* properties_destroyed. */
  0, /* todo_flags_start. */
  0, /* todo_flags_finish. */
};

static bool
is_interrupt_handler_p (tree type)
{
  return lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type)) != NULL;
}

/* Insert landing-pad check instructions.  This is a late RTL pass that runs
   before branch shortening. */
static unsigned int
rest_of_insert_landing_pad (void)
{
  timevar_push (TV_MACH_DEP);

  struct cgraph_node *c_node;
  rtx lpad_insn;
  rtx_insn *insn;
  basic_block bb;

  bb = 0;
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb));
	   insn = NEXT_INSN (insn))
	{
	  /* If a label is marked to be preserved or can be a non-local goto
	     target, it must be protected with a lpad instruction.  */
	  if (LABEL_P (insn)
	       && (LABEL_PRESERVE_P (insn)
		   || bb->flags & BB_NON_LOCAL_GOTO_TARGET))
	    {
	      emit_insn_before (gen_lpad_align (), insn);
	      emit_insn_after (gen_lpad (const0_rtx), insn);
	      continue;
	    }

	  if (INSN_P (insn) && INSN_CODE (insn) == CODE_FOR_gpr_save)
	    {
	      emit_move_insn (RISCV_CALL_ADDRESS_LPAD (Pmode), const0_rtx);
	      emit_insn_before (gen_lpad_align (), insn);
	      emit_insn_after (gen_lpad (const0_rtx), insn);
	      continue;
	    }

	  if (INSN_P (insn) && INSN_CODE (insn) == CODE_FOR_gpr_restore)
	    emit_move_insn (RISCV_CALL_ADDRESS_LPAD (Pmode), const0_rtx);

	}
    }

  c_node = cgraph_node::get (cfun->decl);
  if (!c_node->only_called_directly_p ()
      && !is_interrupt_handler_p (TREE_TYPE (cfun->decl)))
    {
      bb = ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb;
      insn = BB_HEAD (bb);
      lpad_insn = gen_lpad (const0_rtx);
      emit_insn_before (lpad_insn, insn);
    }

  timevar_pop (TV_MACH_DEP);
  return 0;
}

class pass_insert_landing_pad : public rtl_opt_pass
{
public:
  pass_insert_landing_pad (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_insert_landing_pad, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return is_zicfilp_p ();
    }

  virtual unsigned int execute (function *)
    {
      return rest_of_insert_landing_pad ();
    }

}; // class pass_insert_landing_pad

} // anon namespace

rtl_opt_pass *
make_pass_insert_landing_pad (gcc::context *ctxt)
{
  return new pass_insert_landing_pad (ctxt);
}
