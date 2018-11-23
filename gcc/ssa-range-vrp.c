/* VRP implemented with SSA Ranger.
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "domwalk.h"
#include "ssa-range.h"
#include "tree-ssa-dce.h"

// Return TRUE if NAME can be propagated.,

static bool
argument_ok_to_propagate (tree name)
{
  if (TREE_CODE (name) != SSA_NAME)
    return true;

  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
    {
      // From may_propagate_copy
      if (SSA_NAME_IS_DEFAULT_DEF (name) &&
	  (SSA_NAME_VAR (name) == NULL_TREE ||
	   TREE_CODE (SSA_NAME_VAR (name)) == VAR_DECL))
	return true;
      else
        return false;
    }

  if (virtual_operand_p (name))
    return false;
  return true;
}

static void
rvrp_process_bb_end (ssa_range& ranger, basic_block bb, bitmap touched)
{
  gcond *cond;
  gimple *stmt = gimple_outgoing_range_stmt_p (bb);
  irange r;

  // Look only at conditionals.
  if (stmt && (cond = dyn_cast <gcond *> (stmt)))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "RVRP: Considering BB %d:  ", bb->index);
	  print_gimple_stmt (dump_file, cond, 0, TDF_NONE);
	}
      // CHeck to see if the expression folds.
      if (ranger.range_of_stmt (r, stmt) && !r.varying_p ())
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "      Expression evaluates to range: ");
	      r.dump (dump_file);
	    }
	  // If it folds to a constant and its OK to propagate the args.
	  if (r.singleton_p ())
	    {
	      if (!argument_ok_to_propagate (gimple_cond_lhs (cond)) ||
		  !argument_ok_to_propagate (gimple_cond_rhs (cond)))
		{
		  if (dump_file)
		    fprintf (dump_file, "RVRP: Cannot propagate.\n");
		  return;
		}

	      // If either operand is an ssa_name, set the touched bit for
	      // potential removal later if no uses are left.
	      tree t = ranger.valid_ssa_p (gimple_cond_lhs (cond));
	      if (t)
		bitmap_set_bit (touched, SSA_NAME_VERSION (t));

	      t = ranger.valid_ssa_p (gimple_cond_rhs (cond));
	      if (t)
		bitmap_set_bit (touched, SSA_NAME_VERSION (t));

	      /* Rewrite the condition to either true or false.  */
	      if (r.zero_p ())
		gimple_cond_make_false (cond);
	      else
		gimple_cond_make_true (cond);
	      update_stmt (cond);

	      if (dump_file)
		{
		  fprintf (dump_file, "RVRP: Branch rewritten to: ");
		  print_gimple_stmt (dump_file, cond, 0, TDF_NONE);
		  fprintf (dump_file, "\n");
		}
	    }
	}
      else
	{
	  // The expression doesn't fold, but see if any operand
	  // evaluates to an empty range on one side, indicating the
	  // edge is not executable.

	}
    }
}

static unsigned int
execute_ranger_vrp ()
{
  bool details = dump_file && (dump_flags & TDF_DETAILS);
  // Create a temp ranger and exercise it before running in order to get a
  // listing in the dump file, and to fully exercise the code.
  if (details)
    { 
      global_ranger e;
      e.exercise (dump_file);
    }

  trace_ranger ranger;
  basic_block bb;
  irange r;
  bitmap touched = BITMAP_ALLOC (NULL);

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gphi_iterator gpi = gsi_start_phis (bb); !gsi_end_p (gpi);
	   gsi_next (&gpi))
	{
	  gphi *phi = gpi.phi ();
	  tree phi_def = gimple_phi_result (phi);
	  if (ranger.valid_ssa_p (phi_def) && ranger.range_of_stmt (r, phi))
	    {
	      // bitmap_set_bit (touched, SSA_NAME_VERSION (phi_def));
	    }
	}

      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  tree lhs = gimple_get_lhs (stmt);
	  if (ranger.valid_ssa_p (lhs) && ranger.range_of_stmt (r, stmt))
	    {
	      // bitmap_set_bit (touched, SSA_NAME_VERSION (lhs));
	    }
	}

      rvrp_process_bb_end (ranger, bb, touched);
    }

  // Now delete any statements with 0 uses.
  simple_dce_from_worklist (touched);

  return 0;
}



// This routine uses a ranger to query the arguments of branches at the bottom
// of every block and try to fold them if appropriate.
// A walk of the block is not performed, so no values within the block are
// folded, just the branches.

unsigned int
execute_ranger_vrp_conditional ()
{
  bool details = dump_file && (dump_flags & TDF_DETAILS);
  // Create a temp ranger and exercise it before running in order to get a
  // listing in the dump file, and to fully exercise the code.
  if (details)
  { 
    global_ranger e;
    e.exercise (dump_file);
  }

  global_ranger ranger;
  basic_block bb;
  irange r;
  bitmap touched = BITMAP_ALLOC (NULL);

  FOR_EACH_BB_FN (bb, cfun)
    rvrp_process_bb_end (ranger, bb, touched);

  // Now delete any statements with 0 uses.
  simple_dce_from_worklist (touched);

  return 0;
}

namespace {

const pass_data pass_data_ranger_vrp =
{
  GIMPLE_PASS, /* type */
  "rvrp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_RANGER_VRP, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_verify_all ),
};

class pass_ranger_vrp : public gimple_opt_pass
{
public:
  pass_ranger_vrp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_ranger_vrp, ctxt)
    {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_ranger_vrp (m_ctxt); }
  virtual bool gate (function *)
    {
      return flag_tree_rvrp != 0;
    }
  virtual unsigned int execute (function *)
    { return execute_ranger_vrp (); }

}; // class pass_vrp
} // anon namespace

gimple_opt_pass *
make_pass_ranger_vrp (gcc::context *ctxt)
{
  return new pass_ranger_vrp (ctxt);
}


