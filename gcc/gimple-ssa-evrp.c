/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2019 Free Software Foundation, Inc.

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
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "alloc-pool.h"
#include "domwalk.h"
#include "tree-cfgcleanup.h"
#include "vr-values.h"
#include "gimple-ssa-evrp-analyze.h"

class evrp_folder : public substitute_and_fold_engine
{
 public:
  tree get_value (tree) FINAL OVERRIDE;
  evrp_folder (class vr_values *vr_values_) : vr_values (vr_values_) { }
  bool simplify_stmt_using_ranges (gimple_stmt_iterator *gsi)
    { return vr_values->simplify_stmt_using_ranges (gsi); }
  class vr_values *vr_values;

 private:
  DISABLE_COPY_AND_ASSIGN (evrp_folder);
};

tree
evrp_folder::get_value (tree op)
{
  return vr_values->op_with_constant_singleton_value_range (op);
}

/* evrp_dom_walker visits the basic blocks in the dominance order and set
   the Value Ranges (VR) for SSA_NAMEs in the scope.  Use this VR to
   discover more VRs.  */

class evrp_dom_walker : public dom_walker
{
public:
  evrp_dom_walker ()
    : dom_walker (CDI_DOMINATORS),
      evrp_range_analyzer (true),
      evrp_folder (evrp_range_analyzer.get_vr_values ())
    {
      need_eh_cleanup = BITMAP_ALLOC (NULL);
    }
  ~evrp_dom_walker ()
    {
      BITMAP_FREE (need_eh_cleanup);
    }
  virtual edge before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);
  void cleanup (void);

 private:
  DISABLE_COPY_AND_ASSIGN (evrp_dom_walker);
  bitmap need_eh_cleanup;
  auto_vec<gimple *> stmts_to_fixup;
  auto_vec<gimple *> stmts_to_remove;

  class evrp_range_analyzer evrp_range_analyzer;
  class evrp_folder evrp_folder;
};

edge
evrp_dom_walker::before_dom_children (basic_block bb)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Visiting BB%d\n", bb->index);

  evrp_range_analyzer.enter (bb);

  for (gphi_iterator gpi = gsi_start_phis (bb);
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();
      tree lhs = PHI_RESULT (phi);
      if (virtual_operand_p (lhs))
	continue;

      value_range *vr = evrp_range_analyzer.get_value_range (lhs);
      /* Mark PHIs whose lhs we fully propagate for removal.  */
      tree val = value_range_constant_singleton (vr);
      if (val && may_propagate_copy (lhs, val))
	{
	  stmts_to_remove.safe_push (phi);
	  continue;
	}
    }

  edge taken_edge = NULL;

  /* Visit all other stmts and discover any new VRs possible.  */
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      tree output = NULL_TREE;
      gimple *old_stmt = stmt;
      bool was_noreturn = (is_gimple_call (stmt)
			   && gimple_call_noreturn_p (stmt));

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Visiting stmt ");
	  print_gimple_stmt (dump_file, stmt, 0);
	}

      evrp_range_analyzer.record_ranges_from_stmt (stmt, false);

      if (gcond *cond = dyn_cast <gcond *> (stmt))
	{
	  evrp_range_analyzer.vrp_visit_cond_stmt (cond, &taken_edge);
	  if (taken_edge)
	    {
	      if (taken_edge->flags & EDGE_TRUE_VALUE)
		gimple_cond_make_true (cond);
	      else if (taken_edge->flags & EDGE_FALSE_VALUE)
		gimple_cond_make_false (cond);
	      else
		gcc_unreachable ();
	      update_stmt (stmt);
	    }
	}
      else if (stmt_interesting_for_vrp (stmt))
	{
	  output = get_output_for_vrp (stmt);
	  if (output)
	    {
	      tree val;
	      value_range *vr = evrp_range_analyzer.get_value_range (output);

	      /* Mark stmts whose output we fully propagate for removal.  */
	      if ((val = value_range_constant_singleton (vr))
		  && may_propagate_copy (output, val)
		  && !stmt_could_throw_p (cfun, stmt)
		  && !gimple_has_side_effects (stmt))
		{
		  stmts_to_remove.safe_push (stmt);
		  continue;
		}
	    }
	}

      /* Try folding stmts with the VR discovered.  */
      bool did_replace = evrp_folder.replace_uses_in (stmt);
      if (fold_stmt (&gsi, follow_single_use_edges)
	  || did_replace)
	{
	  stmt = gsi_stmt (gsi);
	  update_stmt (stmt);
	  did_replace = true;
	}
      if (evrp_folder.simplify_stmt_using_ranges (&gsi))
	{
	  stmt = gsi_stmt (gsi);
	  update_stmt (stmt);
	  did_replace = true;
	}

      if (did_replace)
	{
	  /* If we cleaned up EH information from the statement,
	     remove EH edges.  */
	  if (maybe_clean_or_replace_eh_stmt (old_stmt, stmt))
	    bitmap_set_bit (need_eh_cleanup, bb->index);

	  /* If we turned a not noreturn call into a noreturn one
	     schedule it for fixup.  */
	  if (!was_noreturn
	      && is_gimple_call (stmt)
	      && gimple_call_noreturn_p (stmt))
	    stmts_to_fixup.safe_push (stmt);

	  if (gimple_assign_single_p (stmt))
	    {
	      tree rhs = gimple_assign_rhs1 (stmt);
	      if (TREE_CODE (rhs) == ADDR_EXPR)
		recompute_tree_invariant_for_addr_expr (rhs);
	    }
	}
    }

  /* Visit BB successor PHI nodes and replace PHI args.  */
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      for (gphi_iterator gpi = gsi_start_phis (e->dest);
	   !gsi_end_p (gpi); gsi_next (&gpi))
	{
	  gphi *phi = gpi.phi ();
	  use_operand_p use_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, e);
	  tree arg = USE_FROM_PTR (use_p);
	  if (TREE_CODE (arg) != SSA_NAME
	      || virtual_operand_p (arg))
	    continue;
	  value_range *vr = evrp_range_analyzer.get_value_range (arg);
	  tree val = value_range_constant_singleton (vr);
	  if (val && may_propagate_copy (arg, val))
	    propagate_value (use_p, val);
	}
    }
 
  return taken_edge;
}

void
evrp_dom_walker::after_dom_children (basic_block bb)
{
  evrp_range_analyzer.leave (bb);
}

/* Perform any cleanups after the main phase of EVRP has completed.  */

void
evrp_dom_walker::cleanup (void)
{
  if (dump_file)
    {
      fprintf (dump_file, "\nValue ranges after Early VRP:\n\n");
      evrp_range_analyzer.dump_all_value_ranges (dump_file);
      fprintf (dump_file, "\n");
    }

  /* Remove stmts in reverse order to make debug stmt creation possible.  */
  while (! stmts_to_remove.is_empty ())
    {
      gimple *stmt = stmts_to_remove.pop ();
      if (dump_file && dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "Removing dead stmt ");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      if (gimple_code (stmt) == GIMPLE_PHI)
	remove_phi_node (&gsi, true);
      else
	{
	  unlink_stmt_vdef (stmt);
	  gsi_remove (&gsi, true);
	  release_defs (stmt);
	}
    }

  if (!bitmap_empty_p (need_eh_cleanup))
    gimple_purge_all_dead_eh_edges (need_eh_cleanup);

  /* Fixup stmts that became noreturn calls.  This may require splitting
     blocks and thus isn't possible during the dominator walk.  Do this
     in reverse order so we don't inadvertedly remove a stmt we want to
     fixup by visiting a dominating now noreturn call first.  */
  while (!stmts_to_fixup.is_empty ())
    {
      gimple *stmt = stmts_to_fixup.pop ();
      fixup_noreturn_call (stmt);
    }

  evrp_folder.vr_values->cleanup_edges_and_switches ();
}

/* Main entry point for the early vrp pass which is a simplified non-iterative
   version of vrp where basic blocks are visited in dominance order.  Value
   ranges discovered in early vrp will also be used by ipa-vrp.  */

static unsigned int
execute_early_vrp ()
{
  /* Ideally this setup code would move into the ctor for the dominator
     walk.  However, this setup can change the number of blocks which
     invalidates the internal arrays that are set up by the dominator
     walker.  */
  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  scev_initialize ();
  calculate_dominance_info (CDI_DOMINATORS);

  /* Walk stmts in dominance order and propagate VRP.  */
  evrp_dom_walker walker;
  walker.walk (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  walker.cleanup ();

  scev_finalize ();
  loop_optimizer_finalize ();
  return 0;
}

namespace {

const pass_data pass_data_early_vrp =
{
  GIMPLE_PASS, /* type */
  "evrp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_EARLY_VRP, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa | TODO_verify_all ),
};

class pass_early_vrp : public gimple_opt_pass
{
public:
  pass_early_vrp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_early_vrp, ctxt)
    {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_early_vrp (m_ctxt); }
  virtual bool gate (function *)
    {
      return flag_tree_vrp != 0;
    }
  virtual unsigned int execute (function *)
    { return execute_early_vrp (); }

}; // class pass_vrp
} // anon namespace

gimple_opt_pass *
make_pass_early_vrp (gcc::context *ctxt)
{
  return new pass_early_vrp (ctxt);
}

