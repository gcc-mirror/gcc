/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2017 Free Software Foundation, Inc.

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

class evrp_folder : public substitute_and_fold_engine
{
 public:
  tree get_value (tree) FINAL OVERRIDE;

  class vr_values *vr_values;
};

tree
evrp_folder::get_value (tree op)
{
  return vr_values->op_with_constant_singleton_value_range (op);
}

class evrp_range_analyzer
{
 public:
  evrp_range_analyzer (void);
  ~evrp_range_analyzer (void) { stack.release (); }

  void enter (basic_block);
  void leave (basic_block);
  void record_ranges_from_stmt (gimple *);

  class vr_values vr_values;

 private:
  DISABLE_COPY_AND_ASSIGN (evrp_range_analyzer);
  void push_value_range (tree var, value_range *vr);
  value_range *pop_value_range (tree var);
  value_range *try_find_new_range (tree, tree op, tree_code code, tree limit);
  void record_ranges_from_incoming_edge (basic_block);
  void record_ranges_from_phis (basic_block);

  /* STACK holds the old VR.  */
  auto_vec<std::pair <tree, value_range*> > stack;

  /* Temporary delegators.  */
  value_range *get_value_range (const_tree op)
    { return vr_values.get_value_range (op); }
  bool update_value_range (const_tree op, value_range *vr)
    { return vr_values.update_value_range (op, vr); }
  void extract_range_from_phi_node (gphi *phi, value_range *vr)
    { vr_values.extract_range_from_phi_node (phi, vr); }
  void adjust_range_with_scev (value_range *vr, struct loop *loop,
                               gimple *stmt, tree var)
    { vr_values.adjust_range_with_scev (vr, loop, stmt, var); }
  void extract_range_from_stmt (gimple *stmt, edge *taken_edge_p,
                                tree *output_p, value_range *vr)
    { vr_values.extract_range_from_stmt (stmt, taken_edge_p, output_p, vr); }
  void set_defs_to_varying (gimple *stmt)
    { return vr_values.set_defs_to_varying (stmt); }
  void set_vr_value (tree name, value_range *vr)
    { vr_values.set_vr_value (name, vr); }
  void extract_range_for_var_from_comparison_expr (tree var,
						   enum tree_code cond_code,
						   tree op, tree limit,
						   value_range *vr_p)
    { vr_values.extract_range_for_var_from_comparison_expr (var, cond_code,
							    op, limit, vr_p); }
};

evrp_range_analyzer::evrp_range_analyzer () : stack (10)
{
  edge e;
  edge_iterator ei;
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      bb->flags &= ~BB_VISITED;
      FOR_EACH_EDGE (e, ei, bb->preds)
	e->flags |= EDGE_EXECUTABLE;
    }
}


/* evrp_dom_walker visits the basic blocks in the dominance order and set
   the Value Ranges (VR) for SSA_NAMEs in the scope.  Use this VR to
   discover more VRs.  */

class evrp_dom_walker : public dom_walker
{
public:
  evrp_dom_walker () : dom_walker (CDI_DOMINATORS)
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

  /* Temporary delegators.  */
  value_range *get_value_range (const_tree op)
    { return evrp_range_analyzer.vr_values.get_value_range (op); }
  tree op_with_constant_singleton_value_range (tree op)
    { return evrp_range_analyzer.vr_values.op_with_constant_singleton_value_range (op); }
  void vrp_visit_cond_stmt (gcond *cond, edge *e)
    { evrp_range_analyzer.vr_values.vrp_visit_cond_stmt (cond, e); }
};

void
evrp_range_analyzer::enter (basic_block bb)
{
  stack.safe_push (std::make_pair (NULL_TREE, (value_range *)NULL));
  record_ranges_from_incoming_edge (bb);
  record_ranges_from_phis (bb);
}

/* Find new range for NAME such that (OP CODE LIMIT) is true.  */
value_range *
evrp_range_analyzer::try_find_new_range (tree name,
				    tree op, tree_code code, tree limit)
{
  value_range vr = VR_INITIALIZER;
  value_range *old_vr = get_value_range (name);

  /* Discover VR when condition is true.  */
  extract_range_for_var_from_comparison_expr (name, code, op,
					      limit, &vr);
  /* If we found any usable VR, set the VR to ssa_name and create a
     PUSH old value in the stack with the old VR.  */
  if (vr.type == VR_RANGE || vr.type == VR_ANTI_RANGE)
    {
      if (old_vr->type == vr.type
	  && vrp_operand_equal_p (old_vr->min, vr.min)
	  && vrp_operand_equal_p (old_vr->max, vr.max))
	return NULL;
      value_range *new_vr = vr_values.vrp_value_range_pool.allocate ();
      *new_vr = vr;
      return new_vr;
    }
  return NULL;
}

/* If BB is reached by a single incoming edge (ignoring loop edges),
   then derive ranges implied by traversing that edge.  */

void
evrp_range_analyzer::record_ranges_from_incoming_edge (basic_block bb)
{
  edge pred_e = single_pred_edge_ignoring_loop_edges (bb, false);
  if (pred_e)
    {
      gimple *stmt = last_stmt (pred_e->src);
      tree op0 = NULL_TREE;

      if (stmt
	  && gimple_code (stmt) == GIMPLE_COND
	  && (op0 = gimple_cond_lhs (stmt))
	  && TREE_CODE (op0) == SSA_NAME
	  && (INTEGRAL_TYPE_P (TREE_TYPE (gimple_cond_lhs (stmt)))
	      || POINTER_TYPE_P (TREE_TYPE (gimple_cond_lhs (stmt)))))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Visiting controlling predicate ");
	      print_gimple_stmt (dump_file, stmt, 0);
	    }
	  /* Entering a new scope.  Try to see if we can find a VR
	     here.  */
	  tree op1 = gimple_cond_rhs (stmt);
	  if (TREE_OVERFLOW_P (op1))
	    op1 = drop_tree_overflow (op1);
	  tree_code code = gimple_cond_code (stmt);

	  auto_vec<assert_info, 8> asserts;
	  register_edge_assert_for (op0, pred_e, code, op0, op1, asserts);
	  if (TREE_CODE (op1) == SSA_NAME)
	    register_edge_assert_for (op1, pred_e, code, op0, op1, asserts);

	  auto_vec<std::pair<tree, value_range *>, 8> vrs;
	  for (unsigned i = 0; i < asserts.length (); ++i)
	    {
	      value_range *vr = try_find_new_range (asserts[i].name,
						    asserts[i].expr,
						    asserts[i].comp_code,
						    asserts[i].val);
	      if (vr)
		vrs.safe_push (std::make_pair (asserts[i].name, vr));
	    }
	  /* Push updated ranges only after finding all of them to avoid
	     ordering issues that can lead to worse ranges.  */
	  for (unsigned i = 0; i < vrs.length (); ++i)
	    push_value_range (vrs[i].first, vrs[i].second);
	}
    }
}

void
evrp_range_analyzer::record_ranges_from_phis (basic_block bb)
{
  /* Visit PHI stmts and discover any new VRs possible.  */
  bool has_unvisited_preds = false;
  edge_iterator ei;
  edge e;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (e->flags & EDGE_EXECUTABLE
	&& !(e->src->flags & BB_VISITED))
      {
	has_unvisited_preds = true;
	break;
      }

  for (gphi_iterator gpi = gsi_start_phis (bb);
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();
      tree lhs = PHI_RESULT (phi);
      if (virtual_operand_p (lhs))
	continue;

      value_range vr_result = VR_INITIALIZER;
      bool interesting = stmt_interesting_for_vrp (phi);
      if (!has_unvisited_preds && interesting)
	extract_range_from_phi_node (phi, &vr_result);
      else
	{
	  set_value_range_to_varying (&vr_result);
	  /* When we have an unvisited executable predecessor we can't
	     use PHI arg ranges which may be still UNDEFINED but have
	     to use VARYING for them.  But we can still resort to
	     SCEV for loop header PHIs.  */
	  struct loop *l;
	  if (interesting
	      && (l = loop_containing_stmt (phi))
	      && l->header == gimple_bb (phi))
	  adjust_range_with_scev (&vr_result, l, phi, lhs);
	}
      update_value_range (lhs, &vr_result);

      /* Set the SSA with the value range.  */
      if (INTEGRAL_TYPE_P (TREE_TYPE (lhs)))
	{
	  if ((vr_result.type == VR_RANGE
	       || vr_result.type == VR_ANTI_RANGE)
	      && (TREE_CODE (vr_result.min) == INTEGER_CST)
	      && (TREE_CODE (vr_result.max) == INTEGER_CST))
	    set_range_info (lhs, vr_result.type,
			    wi::to_wide (vr_result.min),
			    wi::to_wide (vr_result.max));
	}
      else if (POINTER_TYPE_P (TREE_TYPE (lhs))
	       && ((vr_result.type == VR_RANGE
		    && range_includes_zero_p (vr_result.min,
					      vr_result.max) == 0)
		   || (vr_result.type == VR_ANTI_RANGE
		       && range_includes_zero_p (vr_result.min,
						 vr_result.max) == 1)))
	set_ptr_nonnull (lhs);
    }
}

/* Record any ranges created by statement STMT.  */

void
evrp_range_analyzer::record_ranges_from_stmt (gimple *stmt)
{
  tree output = NULL_TREE;

  if (dyn_cast <gcond *> (stmt))
    ;
  else if (stmt_interesting_for_vrp (stmt))
    {
      edge taken_edge;
      value_range vr = VR_INITIALIZER;
      extract_range_from_stmt (stmt, &taken_edge, &output, &vr);
      if (output && (vr.type == VR_RANGE || vr.type == VR_ANTI_RANGE))
	{
	  update_value_range (output, &vr);

	  /* Set the SSA with the value range.  */
	  if (INTEGRAL_TYPE_P (TREE_TYPE (output)))
	    {
	      if ((vr.type == VR_RANGE || vr.type == VR_ANTI_RANGE)
		  && (TREE_CODE (vr.min) == INTEGER_CST)
		  && (TREE_CODE (vr.max) == INTEGER_CST))
		set_range_info (output, vr.type,
				wi::to_wide (vr.min),
				wi::to_wide (vr.max));
	    }
	  else if (POINTER_TYPE_P (TREE_TYPE (output))
		   && ((vr.type == VR_RANGE
			&& range_includes_zero_p (vr.min, vr.max) == 0)
		       || (vr.type == VR_ANTI_RANGE
			   && range_includes_zero_p (vr.min, vr.max) == 1)))
	    set_ptr_nonnull (output);
	}
      else
	set_defs_to_varying (stmt);
    }
  else
    set_defs_to_varying (stmt);

  /* See if we can derive a range for any of STMT's operands.  */
  tree op;
  ssa_op_iter i;
  FOR_EACH_SSA_TREE_OPERAND (op, stmt, i, SSA_OP_USE)
    {
      tree value;
      enum tree_code comp_code;

      /* If OP is used in such a way that we can infer a value
	 range for it, and we don't find a previous assertion for
	 it, create a new assertion location node for OP.  */
      if (infer_value_range (stmt, op, &comp_code, &value))
	{
	  /* If we are able to infer a nonzero value range for OP,
	     then walk backwards through the use-def chain to see if OP
	     was set via a typecast.
	     If so, then we can also infer a nonzero value range
	     for the operand of the NOP_EXPR.  */
	  if (comp_code == NE_EXPR && integer_zerop (value))
	    {
	      tree t = op;
	      gimple *def_stmt = SSA_NAME_DEF_STMT (t);
	      while (is_gimple_assign (def_stmt)
		     && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt))
		     && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME
		     && POINTER_TYPE_P
			  (TREE_TYPE (gimple_assign_rhs1 (def_stmt))))
		{
		  t = gimple_assign_rhs1 (def_stmt);
		  def_stmt = SSA_NAME_DEF_STMT (t);

		  /* Add VR when (T COMP_CODE value) condition is true.  */
		  value_range *op_range
		    = try_find_new_range (t, t, comp_code, value);
		  if (op_range)
		    push_value_range (t, op_range);
		}
	    }
	  /* Add VR when (OP COMP_CODE value) condition is true.  */
	  value_range *op_range = try_find_new_range (op, op,
						      comp_code, value);
	  if (op_range)
	    push_value_range (op, op_range);
	}
    }
}

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

      /* Mark PHIs whose lhs we fully propagate for removal.  */
      tree val = op_with_constant_singleton_value_range (lhs);
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

      evrp_range_analyzer.record_ranges_from_stmt (stmt);

      if (gcond *cond = dyn_cast <gcond *> (stmt))
	{
	  vrp_visit_cond_stmt (cond, &taken_edge);
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
	  value_range vr = VR_INITIALIZER;
	  output = get_output_for_vrp (stmt);
	  if (output)
	    {
	      tree val;
	      vr = *get_value_range (output);

	      /* Mark stmts whose output we fully propagate for removal.  */
	      if ((vr.type == VR_RANGE || vr.type == VR_ANTI_RANGE)
		  && (val = op_with_constant_singleton_value_range (output))
		  && may_propagate_copy (output, val)
		  && !stmt_could_throw_p (stmt)
		  && !gimple_has_side_effects (stmt))
		{
		  stmts_to_remove.safe_push (stmt);
		  continue;
		}
	    }
	}

      /* Try folding stmts with the VR discovered.  */
      class evrp_folder evrp_folder;
      evrp_folder.vr_values = &evrp_range_analyzer.vr_values;
      bool did_replace = evrp_folder.replace_uses_in (stmt);
      if (fold_stmt (&gsi, follow_single_use_edges)
	  || did_replace)
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
	  tree val = op_with_constant_singleton_value_range (arg);
	  if (val && may_propagate_copy (arg, val))
	    propagate_value (use_p, val);
	}
    }
 
  bb->flags |= BB_VISITED;

  return taken_edge;
}

void
evrp_dom_walker::after_dom_children (basic_block bb)
{
  evrp_range_analyzer.leave (bb);
}

/* Restore/pop VRs valid only for BB when we leave BB.  */

void
evrp_range_analyzer::leave (basic_block bb ATTRIBUTE_UNUSED)
{
  gcc_checking_assert (!stack.is_empty ());
  while (stack.last ().first != NULL_TREE)
    pop_value_range (stack.last ().first);
  stack.pop ();
}

/* Push the Value Range of VAR to the stack and update it with new VR.  */

void
evrp_range_analyzer::push_value_range (tree var, value_range *vr)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "pushing new range for ");
      print_generic_expr (dump_file, var);
      fprintf (dump_file, ": ");
      dump_value_range (dump_file, vr);
      fprintf (dump_file, "\n");
    }
  stack.safe_push (std::make_pair (var, get_value_range (var)));
  set_vr_value (var, vr);
}

/* Pop the Value Range from the vrp_stack and update VAR with it.  */

value_range *
evrp_range_analyzer::pop_value_range (tree var)
{
  value_range *vr = stack.last ().second;
  gcc_checking_assert (var == stack.last ().first);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "popping range for ");
      print_generic_expr (dump_file, var);
      fprintf (dump_file, ", restoring ");
      dump_value_range (dump_file, vr);
      fprintf (dump_file, "\n");
    }
  set_vr_value (var, vr);
  stack.pop ();
  return vr;
}

/* Perform any cleanups after the main phase of EVRP has completed.  */

void
evrp_dom_walker::cleanup (void)
{
  if (dump_file)
    {
      fprintf (dump_file, "\nValue ranges after Early VRP:\n\n");
      evrp_range_analyzer.vr_values.dump_all_value_ranges (dump_file);
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

