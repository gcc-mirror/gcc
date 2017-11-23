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
#include "gimple-ssa-evrp-analyze.h"

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
  vr_values = new class vr_values;
}

void
evrp_range_analyzer::enter (basic_block bb)
{
  stack.safe_push (std::make_pair (NULL_TREE, (value_range *)NULL));
  record_ranges_from_incoming_edge (bb);
  record_ranges_from_phis (bb);
  bb->flags |= BB_VISITED;
}

/* Find new range for NAME such that (OP CODE LIMIT) is true.  */
value_range *
evrp_range_analyzer::try_find_new_range (tree name,
				    tree op, tree_code code, tree limit)
{
  value_range vr = VR_INITIALIZER;
  value_range *old_vr = get_value_range (name);

  /* Discover VR when condition is true.  */
  vr_values->extract_range_for_var_from_comparison_expr (name, code, op,
							 limit, &vr);
  /* If we found any usable VR, set the VR to ssa_name and create a
     PUSH old value in the stack with the old VR.  */
  if (vr.type == VR_RANGE || vr.type == VR_ANTI_RANGE)
    {
      if (old_vr->type == vr.type
	  && vrp_operand_equal_p (old_vr->min, vr.min)
	  && vrp_operand_equal_p (old_vr->max, vr.max))
	return NULL;
      value_range *new_vr = vr_values->allocate_value_range ();
      *new_vr = vr;
      return new_vr;
    }
  return NULL;
}

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
	vr_values->extract_range_from_phi_node (phi, &vr_result);
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
	  vr_values->adjust_range_with_scev (&vr_result, l, phi, lhs);
	}
      vr_values->update_value_range (lhs, &vr_result);

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
      vr_values->extract_range_from_stmt (stmt, &taken_edge, &output, &vr);
      if (output
	  && (vr.type == VR_RANGE || vr.type == VR_ANTI_RANGE))
	{
	  vr_values->update_value_range (output, &vr);

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
	vr_values->set_defs_to_varying (stmt);
    }
  else
    vr_values->set_defs_to_varying (stmt);

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
		     && TREE_CODE
			  (gimple_assign_rhs1 (def_stmt)) == SSA_NAME
		     && POINTER_TYPE_P
			  (TREE_TYPE (gimple_assign_rhs1 (def_stmt))))
		{
		  t = gimple_assign_rhs1 (def_stmt);
		  def_stmt = SSA_NAME_DEF_STMT (t);

		  /* Add VR when (T COMP_CODE value) condition is
		     true.  */
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
  vr_values->set_vr_value (var, vr);
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
  vr_values->set_vr_value (var, vr);
  stack.pop ();
  return vr;
}
