/* VRP implemented with SSA Ranger.
   Copyright (C) 2018-2019 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop.h"
#include "alloc-pool.h"
#include "range.h"
#include "vr-values.h"
#include "tree-ssa-propagate.h"
#include "dbgcnt.h"

class irange_misc : public range_misc
{
public:
  irange_misc (global_ranger *r) : m_ranger (r) { }
private:
  virtual irange get_irange (tree, gimple *);
  virtual tree singleton (tree, gimple *stmt);
  global_ranger *m_ranger;
};

irange
irange_misc::get_irange (tree op, gimple *stmt)
{
  if (TREE_CODE (op) == INTEGER_CST)
    return irange (op, op);

  irange r;
  m_ranger->range_of_expr (r, op, stmt);
  return r;
}

/* If OP has a value range with a single constant value return that,
   otherwise return NULL_TREE.  This returns OP itself if OP is a
   constant.  */

tree
irange_misc::singleton (tree op, gimple *stmt)
{
  if (is_gimple_min_invariant (op))
    return op;

  if (TREE_CODE (op) != SSA_NAME)
    return NULL_TREE;

  tree t;
  if (get_irange (op, stmt).singleton_p (&t))
    return t;
  return NULL;
}

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

// Fold a constant GIMPLE_ASSIGN if possible.  Return TRUE if successful.

static bool
rvrp_fold_const_assign (gassign *assign, const irange &r)
{
  irange trange;
  // ?? Perhaps we could set the assignment to 0 instead?
  if (r.undefined_p ())
    return false;
  tree rhs;
  gcc_assert (r.singleton_p (&rhs));
  delink_stmt_imm_use (assign);
  gimple_assign_set_rhs_code (assign, SSA_NAME);
  gimple_assign_set_rhs1 (assign, rhs);
  gimple_set_num_ops (assign, 2);
  return true;
}

// Fold a constant GIMPLE_COND if possible.  Return TRUE if successful.

static bool
rvrp_fold_const_cond (gcond *cond, const irange &r)
{
  /* Rewrite the condition to either true or false.  */
  if (r.zero_p ())
    gimple_cond_make_false (cond);
  else
    gimple_cond_make_true (cond);
  return true;
}

// Fold STMT in place if possible.  Return TRUE if folding was successful.

static bool
rvrp_fold (ssa_ranger &ranger, gimple *stmt, bitmap touched)
{
  irange r;
  if (ranger.range_of_stmt (r, stmt) && !r.varying_p ())
    {
      // If it folds to a constant and it's OK to propagate the args.
      // Also, if it folds to [], the statement is unreachable (and
      // the BB for that matter).  Fold the branch enough so we can
      // nuke any possible SSA uses in the conditional.
      if (r.singleton_p () || r.undefined_p ())
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	      fprintf (dump_file, "      Expression evaluates to singleton: ");
	      r.dump (dump_file);
	    }

	  // Verify we can propagate all SSA names in statement.
	  for (unsigned i = 1; i < gimple_num_ops (stmt); ++i)
	    {
	      tree t = gimple_op (stmt, i);
	      if (t && !argument_ok_to_propagate (t))
		return false;
	    }
	  // Potentially mark folded SSA_NAMEs for future deletion.
	  for (unsigned i = 0; i < gimple_num_ops (stmt); ++i)
	    {
	      tree t = gimple_op (stmt, i);
	      if (t && TREE_CODE (t) == SSA_NAME)
		bitmap_set_bit (touched, SSA_NAME_VERSION (t));
	    }

	  bool changed = false;
	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_COND:
	      changed = rvrp_fold_const_cond (dyn_cast <gcond *> (stmt), r);
	      break;
	    case GIMPLE_ASSIGN:
	      changed = rvrp_fold_const_assign (dyn_cast <gassign *> (stmt),
						r);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  if (changed)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "RVRP: %s rewritten to: ",
			   gimple_code (stmt) == GIMPLE_COND
			   ? "Branch" : "Statement");
		  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
		  fprintf (dump_file, "\n");
		}
	      update_stmt (stmt);
	    }
	  else if (dump_file)
	    fprintf (dump_file, "RVRP: Cannot propagate.\n");
	  return changed;
	}
    }
  else
    {
      // The expression doesn't fold, but see if any operand evaluates
      // to an empty range on one side, indicating the edge is not
      // executable.
    }
  return false;
}

// Given a STMT for which we can't totally fold, attempt to simplify
// it in place using known range information.
//
// Returns TRUE if any simplification was done.

static bool
rvrp_simplify (global_ranger &ranger, gimple_stmt_iterator *gsi)
{
  gimple *orig;
  bool details = dump_file && (dump_flags & TDF_DETAILS);
  if (details)
    orig = gimple_copy (gsi_stmt (*gsi));

  irange_misc misc (&ranger);
  if (misc.simplify_stmt_using_ranges (gsi))
    {
      gimple *stmt = gsi_stmt (*gsi);
      if (details)
	{
	  fprintf (dump_file, "RVRP: Simplifying:\t");
	  print_gimple_stmt (dump_file, orig, 0, TDF_SLIM);
	  fprintf (dump_file, "\t\t\t");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      update_stmt (stmt);
      return true;
    }
  return false;
}

// Given a bitmap of TOUCHED ssa names throughout the pass, attempt to
// propagate any uses and possibly remove dead code.

static void
rvrp_final_propagate (global_ranger &ranger, bitmap touched)
{
  if (dump_file && (dump_flags & TDF_DETAILS) && !bitmap_empty_p (touched))
    {
      unsigned i;
      bitmap_iterator bi;

      fprintf (dump_file, "RVRP: Propagating SSAs: ");
      EXECUTE_IF_SET_IN_BITMAP (touched, 0, i, bi)
	{
	  print_generic_expr (dump_file, ssa_name (i));
	  fprintf (dump_file, ", ");
	}
      fprintf (dump_file, "\n");
    }

  propagate_from_worklist (touched);
  simple_dce_from_worklist (touched);
  ranger.export_global_ranges ();
}

// Perform a domwalk and accumulate all blocks into a vector.
class dom_accumulator : public dom_walker
{
public:
  dom_accumulator (cdi_direction dir, vec<basic_block> &blocks);
  virtual ~dom_accumulator () { }
  virtual edge before_dom_children (basic_block);
private:
  vec<basic_block> &m_bbs;
};

dom_accumulator::dom_accumulator (cdi_direction dir, vec<basic_block> &blocks)
    : dom_walker (dir), m_bbs (blocks)
{
  if (dir == CDI_DOMINATORS)
    walk (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  else
    walk (EXIT_BLOCK_PTR_FOR_FN (cfun));
}

edge
dom_accumulator::before_dom_children (basic_block bb)
{
  m_bbs.quick_push (bb);
  return NULL;
}

// Main rvrp engine that can run in a variety of different orders:
// Dominator, post-dominator, forward block order, and backwards block
// order.
class rvrp_engine
{
public:
  rvrp_engine (enum rvrp_order);
  ~rvrp_engine ();

private:
  void run ();
  void visit (basic_block);
  void fold_and_simplify (gimple_stmt_iterator &);

  dom_accumulator *m_dom_accumulator;
  auto_vec<basic_block> m_bbs;
  // This is a pointer instead of a scalar so we can delay
  // construction until after loop info is available.
  trace_ranger *m_ranger;
  auto_bitmap m_touched;
  propagate_cleanups m_cleanups;
  enum rvrp_order m_order;
};

rvrp_engine::rvrp_engine (enum rvrp_order order)
  : m_order (order)
{
  // Loop info must be initialized before the ranger because
  // loop_optimizer_init() may alter the IL while normalizing loops.
  calculate_dominance_info (CDI_DOMINATORS);
  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  scev_initialize ();

  basic_block bb;
  m_bbs.reserve (n_basic_blocks_for_fn (cfun));
  m_ranger = new trace_ranger;

  switch (order)
    {
    case RVRP_ORDER_DOMINATOR:
      m_dom_accumulator = new dom_accumulator (CDI_DOMINATORS, m_bbs);
      break;
    case RVRP_ORDER_POSTDOM:
      calculate_dominance_info (CDI_POST_DOMINATORS);
      m_dom_accumulator = new dom_accumulator (CDI_POST_DOMINATORS, m_bbs);
      break;
    case RVRP_ORDER_FORWARD:
    case RVRP_ORDER_CONDITIONALS:
      FOR_EACH_BB_FN (bb, cfun)
	m_bbs.quick_push (bb);
      m_dom_accumulator = NULL;
      break;
    case RVRP_ORDER_BACKWARD:
      FOR_EACH_BB_REVERSE_FN (bb, cfun)
	m_bbs.quick_push (bb);
      m_dom_accumulator = NULL;
      break;
    default:
      gcc_unreachable ();
    }
  run ();
}

rvrp_engine::~rvrp_engine ()
{
  if (m_dom_accumulator)
    delete m_dom_accumulator;

  if (m_order == RVRP_ORDER_POSTDOM)
    free_dominance_info (CDI_POST_DOMINATORS);

  scev_finalize ();
  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);

  rvrp_final_propagate (*m_ranger, m_touched);
}

void
rvrp_engine::fold_and_simplify (gimple_stmt_iterator &gsi)
{
  bool changed = false;
  irange r;
  gimple *stmt = gsi_stmt (gsi);

  // Only process statements which are a COND expr or have a valid LHS.
  if (gimple_code (stmt) != GIMPLE_COND &&
      !m_ranger->valid_ssa_p (gimple_get_lhs (stmt)))
    return;

  // ?? This is only needed for propagate_mark_stmt_for_cleanup.
  // Can we get away with a shallow copy?
  gimple *old_stmt = gimple_copy (stmt);

  if (!dbg_cnt (rvrp_fold_count))
    return;

  if (m_ranger->range_of_stmt (r, stmt))
    changed = rvrp_fold (*m_ranger, stmt, m_touched);
  if (!changed)
    changed = rvrp_simplify (*m_ranger, &gsi);
  if (changed)
    m_cleanups.record_change (old_stmt, stmt);
}

void
rvrp_engine::visit (basic_block bb)
{
  irange r;
  gimple_stmt_iterator gsi;

  if (dump_file)
    fprintf (dump_file, "RVRP: Considering BB %d.\n", bb->index);

  if (m_order == RVRP_ORDER_CONDITIONALS)
    {
      // Check if there is a conditional at the end of the block and visit it.
      gsi = gsi_outgoing_range_stmt (bb);
      if (!gsi_end_p (gsi))
        fold_and_simplify (gsi);
      return;
    }

  // Process all the PHI nodes.
  for (gphi_iterator gpi = gsi_start_phis (bb); !gsi_end_p (gpi);
       gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();
      tree phi_def = gimple_phi_result (phi);
      if (m_ranger->valid_ssa_p (phi_def))
	m_ranger->range_of_stmt (r, phi);
    }

  // If processing in reverse, walk the IL bottom up.
  if (m_order == RVRP_ORDER_BACKWARD)
    for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
      fold_and_simplify (gsi);
  else
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      fold_and_simplify (gsi);
}

void
rvrp_engine::run ()
{
  bool details = dump_file && (dump_flags & TDF_DETAILS);
  basic_block bb;

  // Create a temp ranger and exercise it before running in order to get a
  // listing in the dump file, and to fully exercise the code.
  if (details)
    {
      fprintf (dump_file, "RVRP: BEFORE rvrp ranger dump.\n");
      global_ranger e;
      e.calculate_and_dump (dump_file);
    }

  for (unsigned i = 0; m_bbs.iterate (i, &bb); ++i)
    visit (bb);
}

static unsigned int
execute_ranger_vrp ()
{
  enum rvrp_order order
    = flag_rvrp_order ? flag_rvrp_order : RVRP_ORDER_DOMINATOR;
  rvrp_engine w (order);
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
