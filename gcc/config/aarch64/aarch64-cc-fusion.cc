// Pass to fuse CC operations with other instructions.
// Copyright (C) 2021-2024 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.
//
// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// This pass looks for sequences of the form:
//
//    A: (set (reg R1) X1)
//    B: ...instructions that might change the value of X1...
//    C: (set (reg CC) X2) // X2 uses R1
//
// and tries to change them to:
//
//    C': [(set (reg CC) X2')
//         (set (reg R1) X1)]
//    B: ...instructions that might change the value of X1...
//
// where X2' is the result of replacing R1 with X1 in X2.
//
// This sequence occurs in SVE code in two important cases:
//
// (a) Sometimes, to deal correctly with overflow, we need to increment
//     an IV after a WHILELO rather than before it.  In this case:
//     - A is a WHILELO,
//     - B includes an IV increment and
//     - C is a separate PTEST.
//
// (b) ACLE code of the form:
//
//       svbool_t ok = svrdffr ();
//       if (svptest_last (pg, ok))
//         ...
//
//     must, for performance reasons, be code-generated as:
//
//       RDFFRS Pok.B, Pg/Z
//       ...branch on flags result...
//
//     without a separate PTEST of Pok.  In this case:
//     - A is an aarch64_rdffr
//     - B includes an aarch64_update_ffrt
//     - C is a separate PTEST
//
// Combine can handle this optimization if B doesn't exist and if A and
// C are in the same BB.  This pass instead handles cases where B does
// exist and cases where A and C are in different BBs of the same EBB.

#define IN_TARGET_CODE 1

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_ARRAY
#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "rtl-ssa.h"
#include "tree-pass.h"

using namespace rtl_ssa;

namespace {
const pass_data pass_data_cc_fusion =
{
  RTL_PASS, // type
  "cc_fusion", // name
  OPTGROUP_NONE, // optinfo_flags
  TV_NONE, // tv_id
  0, // properties_required
  0, // properties_provided
  0, // properties_destroyed
  0, // todo_flags_start
  TODO_df_finish, // todo_flags_finish
};

// Class that represents one run of the pass.
class cc_fusion
{
public:
  cc_fusion ()  : m_parallel () {}
  void execute ();

private:
  rtx optimizable_set (const insn_info *);
  bool parallelize_insns (def_info *, rtx, def_info *, rtx);
  void optimize_cc_setter (def_info *, rtx);

  // A spare PARALLEL rtx, or null if none.
  rtx m_parallel;
};

// See whether INSN is a single_set that we can optimize.  Return the
// set if so, otherwise return null.
rtx
cc_fusion::optimizable_set (const insn_info *insn)
{
  if (!insn->can_be_optimized ()
      || insn->is_asm ()
      || insn->has_volatile_refs ()
      || insn->has_pre_post_modify ())
    return NULL_RTX;

  return single_set (insn->rtl ());
}

// CC_SET is a single_set that sets (only) CC_DEF; OTHER_SET is likewise
// a single_set that sets (only) OTHER_DEF.  CC_SET is known to set the
// CC register and the instruction that contains CC_SET is known to use
// OTHER_DEF.  Try to do CC_SET and OTHER_SET in parallel.
bool
cc_fusion::parallelize_insns (def_info *cc_def, rtx cc_set,
			      def_info *other_def, rtx other_set)
{
  auto attempt = crtl->ssa->new_change_attempt ();

  insn_info *cc_insn = cc_def->insn ();
  insn_info *other_insn = other_def->insn ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "trying to parallelize insn %d and insn %d\n",
	     other_insn->uid (), cc_insn->uid ());

  // Try to substitute OTHER_SET into CC_INSN.
  insn_change_watermark rtl_watermark;
  rtx_insn *cc_rtl = cc_insn->rtl ();
  insn_propagation prop (cc_rtl, SET_DEST (other_set),
			 SET_SRC (other_set));
  if (!prop.apply_to_pattern (&PATTERN (cc_rtl))
      || prop.num_replacements == 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "-- failed to substitute all uses of r%d\n",
		 other_def->regno ());
      return false;
    }

  // Restrict the uses to those outside notes.
  use_array cc_uses = remove_note_accesses (attempt, cc_insn->uses ());
  use_array other_set_uses = remove_note_accesses (attempt,
						   other_insn->uses ());

  // Remove the use of the substituted value.
  access_array_builder uses_builder (attempt);
  uses_builder.reserve (cc_uses.size ());
  for (use_info *use : cc_uses)
    if (use->def () != other_def)
      uses_builder.quick_push (use);
  cc_uses = use_array (uses_builder.finish ());

  // Get the list of uses for the new instruction.
  insn_change cc_change (cc_insn);
  cc_change.new_uses = merge_access_arrays (attempt, other_set_uses, cc_uses);
  if (!cc_change.new_uses.is_valid ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "-- cannot merge uses\n");
      return false;
    }

  // The instruction initially defines just two registers.  recog can add
  // extra clobbers if necessary.
  auto_vec<access_info *, 2> new_defs;
  new_defs.quick_push (cc_def);
  new_defs.quick_push (other_def);
  sort_accesses (new_defs);
  cc_change.new_defs = def_array (access_array (new_defs));

  // Make sure there is somewhere that the new instruction could live.
  auto other_change = insn_change::delete_insn (other_insn);
  insn_change *changes[] = { &other_change, &cc_change };
  cc_change.move_range = cc_insn->ebb ()->insn_range ();
  if (!restrict_movement (cc_change, ignore_changing_insns (changes)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "-- cannot satisfy all definitions and uses\n");
      return false;
    }

  // Tentatively install the new pattern.  By convention, the CC set
  // must be first.
  if (m_parallel)
    {
      XVECEXP (m_parallel, 0, 0) = cc_set;
      XVECEXP (m_parallel, 0, 1) = other_set;
    }
  else
    {
      rtvec vec = gen_rtvec (2, cc_set, other_set);
      m_parallel = gen_rtx_PARALLEL (VOIDmode, vec);
    }
  validate_change (cc_rtl, &PATTERN (cc_rtl), m_parallel, 1);

  // These routines report failures themselves.
  if (!recog (attempt, cc_change, ignore_changing_insns (changes))
      || !changes_are_worthwhile (changes)
      || !crtl->ssa->verify_insn_changes (changes))
    return false;

  remove_reg_equal_equiv_notes (cc_rtl);
  confirm_change_group ();
  crtl->ssa->change_insns (changes);
  m_parallel = NULL_RTX;
  return true;
}

// Try to optimize the instruction that contains CC_DEF, where CC_DEF describes
// a definition of the CC register by CC_SET.
void
cc_fusion::optimize_cc_setter (def_info *cc_def, rtx cc_set)
{
  // Search the registers used by the CC setter for an easily-substitutable
  // def-use chain.
  for (use_info *other_use : cc_def->insn ()->uses ())
    if (def_info *other_def = other_use->def ())
      if (other_use->regno () != CC_REGNUM
	  && other_def->ebb () == cc_def->ebb ())
	if (rtx other_set = optimizable_set (other_def->insn ()))
	  {
	    rtx dest = SET_DEST (other_set);
	    if (REG_P (dest)
		&& REGNO (dest) == other_def->regno ()
		&& REG_NREGS (dest) == 1
		&& parallelize_insns (cc_def, cc_set, other_def, other_set))
	      return;
	  }
}

// Run the pass on the current function.
void
cc_fusion::execute ()
{
  // Initialization.
  calculate_dominance_info (CDI_DOMINATORS);
  df_analyze ();
  crtl->ssa = new rtl_ssa::function_info (cfun);

  // Walk through all instructions that set CC.  Look for a PTEST instruction
  // that we can optimize.
  //
  // ??? The PTEST test isn't needed for correctness, but it ensures that the
  // pass no effect on non-SVE code.
  for (def_info *def : crtl->ssa->reg_defs (CC_REGNUM))
    if (rtx cc_set = optimizable_set (def->insn ()))
      if (REG_P (SET_DEST (cc_set))
	  && REGNO (SET_DEST (cc_set)) == CC_REGNUM
	  && GET_CODE (SET_SRC (cc_set)) == UNSPEC
	  && XINT (SET_SRC (cc_set), 1) == UNSPEC_PTEST)
	optimize_cc_setter (def, cc_set);

  // Finalization.
  crtl->ssa->perform_pending_updates ();
  free_dominance_info (CDI_DOMINATORS);
}

class pass_cc_fusion : public rtl_opt_pass
{
public:
  pass_cc_fusion (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_cc_fusion, ctxt)
  {}

  // opt_pass methods:
  virtual bool gate (function *) { return TARGET_SVE && optimize >= 2; }
  virtual unsigned int execute (function *);
};

unsigned int
pass_cc_fusion::execute (function *)
{
  cc_fusion ().execute ();
  return 0;
}

} // end namespace

// Create a new CC fusion pass instance.

rtl_opt_pass *
make_pass_cc_fusion (gcc::context *ctxt)
{
  return new pass_cc_fusion (ctxt);
}
