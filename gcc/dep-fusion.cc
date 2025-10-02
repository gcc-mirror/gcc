// Dependency fusion reordering pass.
// Copyright (C) 2025 Free Software Foundation, Inc.
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
//
// This pass uses the RTL-SSA representation to detect def-use pairs that are
// macro-op-fusible in the current microarchitecture (using the
// macro_fusion_pair_p () target hook) and place them next to one another, if
// possible.

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_MEMORY
#define INCLUDE_ARRAY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "rtl-ssa.h"
#include "print-rtl.h"
#include "tree-pass.h"
#include "cfgcleanup.h"
#include "target.h"
#include "dbgcnt.h"

namespace {
const pass_data pass_data_dep_fusion =
{
  RTL_PASS, // type
  "dep_fusion", // name
  OPTGROUP_NONE, // optinfo_flags
  TV_NONE, // tv_id
  0, // properties_required
  0, // properties_provided
  0, // properties_destroyed
  0, // todo_flags_start
  TODO_df_finish, // todo_flags_finish
};

class pass_dep_fusion : public rtl_opt_pass
{
public:
  pass_dep_fusion (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_dep_fusion, ctxt)
  {}

  // opt_pass methods:
  opt_pass *clone () final override { return new pass_dep_fusion (m_ctxt); }
  bool gate (function *) final override;
  unsigned int execute (function *) final override;
};

bool
pass_dep_fusion::gate (function *)
{
  return optimize > 0 && flag_dep_fusion;
}

unsigned int
pass_dep_fusion::execute (function *fn)
{
  // If the target has no macro fusion, there is nothing to be done.
  if (!targetm.sched.macro_fusion_pair_p)
    return 0;

  // Initialization.
  calculate_dominance_info (CDI_DOMINATORS);
  df_analyze ();
  crtl->ssa = new rtl_ssa::function_info (fn);

  init_recog_no_volatile ();

  for (rtl_ssa::insn_info *insn = *crtl->ssa->nondebug_insns ().begin ();
       insn;
       insn = insn->next_nondebug_insn ())
    {
      if (!insn->can_be_optimized () || insn->num_defs () != 1)
       continue;

      rtl_ssa::set_info *def = single_set_info (insn);
      if (!def)
       continue;

      rtl_ssa::use_info *use_insn = def->single_nondebug_insn_use ();
      if (!use_insn
	  || !use_insn->insn ()->can_be_optimized ()
	  || !targetm.sched.macro_fusion_pair_p (insn->rtl (),
						 use_insn->insn ()->rtl ()))
       continue;

      auto attempt = crtl->ssa->new_change_attempt ();
      rtl_ssa::insn_change change (use_insn->insn ());

      if (use_insn->insn () != insn->next_any_insn ())
	{
	  if (!can_move_insn_p (use_insn->insn ()))
	    continue;

	  change.move_range = insn;
	  if (!rtl_ssa::restrict_movement (change))
	    continue;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Moved a single-use instruction:\n");
	      dump_insn_slim (dump_file, use_insn->insn ()->rtl ());
	      fprintf (dump_file, "right after its definition:\n");
	      dump_insn_slim (dump_file, insn->rtl ());
	    }
	}

      SCHED_GROUP_P (use_insn->insn ()->rtl ()) = 1;
      confirm_change_group ();
      crtl->ssa->change_insn (change);
    }

  // Finalization.
  if (crtl->ssa->perform_pending_updates ())
    cleanup_cfg (0);

  delete crtl->ssa;

  init_recog ();
  free_dominance_info (CDI_DOMINATORS);
  return 0;
}

} // end namespace

// Create a new dep fusion pass instance.

rtl_opt_pass *
make_pass_dep_fusion (gcc::context *ctxt)
{
  return new pass_dep_fusion (ctxt);
}
