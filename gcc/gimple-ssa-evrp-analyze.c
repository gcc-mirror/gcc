/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2020 Free Software Foundation, Inc.

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
#include "dbgcnt.h"
#include "gimple-range.h"
#include "ssa-range.h"

class vr_gori_interface : public trace_gori_compute
{
public:
  vr_gori_interface (range_store *store) : store (store) { }
  virtual bool outgoing_edge_range_p (irange &, edge, tree name,
				      const irange *name_range = NULL);
  bool gori_computable_p (tree name, basic_block);
protected:
  virtual bool refine_range_with_equivalences (irange &, edge, tree name);
private:
  virtual void range_of_ssa_name (irange &r, tree op, gimple * = NULL);
  bool solve_name_at_statement (irange &, tree, gimple *stmt, const irange &);
  bool solve_name_given_equivalence (irange &r, tree name, tree equiv,
				     const irange &equiv_range);
  range_store *store;
};

class trace_vr_gori_interface : public vr_gori_interface
{
public:
  trace_vr_gori_interface (range_store *store) : vr_gori_interface (store) { }
private:
  virtual bool refine_range_with_equivalences (irange &, edge, tree name);
  typedef vr_gori_interface super;
};

evrp_range_analyzer::evrp_range_analyzer (bool update_global_ranges)
  : stack (10), m_update_global_ranges (update_global_ranges)
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
  ranger = new global_ranger;
  gori = new trace_vr_gori_interface (vr_values);
}

evrp_range_analyzer::~evrp_range_analyzer (void)
{
  delete vr_values;
  stack.release ();
  delete ranger;
  delete gori;
}

/* Push an unwinding marker onto the unwinding stack.  */

void
evrp_range_analyzer::push_marker ()
{
  stack.safe_push (std::make_pair (NULL_TREE, (value_range_equiv *)NULL));
}

/* Analyze ranges as we enter basic block BB.  */

void
evrp_range_analyzer::enter (basic_block bb)
{
  if (!optimize)
    return;
  push_marker ();
  record_ranges_from_incoming_edge (bb);
  record_ranges_from_phis (bb);
  bb->flags |= BB_VISITED;
}

/* Find new range for NAME such that (OP CODE LIMIT) is true.  */
value_range_equiv *
evrp_range_analyzer::try_find_new_range (tree name,
					 tree op, tree_code code, tree limit)
{
  value_range_equiv vr;
  const value_range_equiv *old_vr = get_value_range (name);

  /* Discover VR when condition is true.  */
  vr_values->extract_range_for_var_from_comparison_expr (name, code, op,
							 limit, &vr);
  /* If we found any usable VR, set the VR to ssa_name and create a
     PUSH old value in the stack with the old VR.  */
  if (!vr.undefined_p () && !vr.varying_p ())
    {
      if (old_vr->equal_p (vr, /*ignore_equivs=*/true))
	return NULL;
      value_range_equiv *new_vr = vr_values->allocate_value_range_equiv ();
      new_vr->move (&vr);
      return new_vr;
    }
  return NULL;
}

/* For LHS record VR in the SSA info.  */
void
evrp_range_analyzer::set_ssa_range_info (tree lhs, value_range_equiv *vr)
{
  gcc_assert (m_update_global_ranges);

  /* Set the SSA with the value range.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (lhs)))
    {
      if (vr->constant_p ())
	set_range_info (lhs, vr->kind (),
			wi::to_wide (vr->min ()),
			wi::to_wide (vr->max ()));
    }
  else if (POINTER_TYPE_P (TREE_TYPE (lhs))
	   && range_includes_zero_p (vr) == 0)
    set_ptr_nonnull (lhs);
}

/* Return true if all uses of NAME are dominated by STMT or feed STMT
   via a chain of single immediate uses.  */

static bool
all_uses_feed_or_dominated_by_stmt (tree name, gimple *stmt)
{
  use_operand_p use_p, use2_p;
  imm_use_iterator iter;
  basic_block stmt_bb = gimple_bb (stmt);

  FOR_EACH_IMM_USE_FAST (use_p, iter, name)
    {
      gimple *use_stmt = USE_STMT (use_p), *use_stmt2;
      if (use_stmt == stmt
	  || is_gimple_debug (use_stmt)
	  || (gimple_bb (use_stmt) != stmt_bb
	      && dominated_by_p (CDI_DOMINATORS,
				 gimple_bb (use_stmt), stmt_bb)))
	continue;
      while (use_stmt != stmt
	     && is_gimple_assign (use_stmt)
	     && TREE_CODE (gimple_assign_lhs (use_stmt)) == SSA_NAME
	     && single_imm_use (gimple_assign_lhs (use_stmt),
				&use2_p, &use_stmt2))
	use_stmt = use_stmt2;
      if (use_stmt != stmt)
	return false;
    }
  return true;
}

void
vr_gori_interface::range_of_ssa_name (irange &r, tree op,
				      gimple *stmt ATTRIBUTE_UNUSED)
{
  r = *store->get_value_range (op);
  r.normalize_symbolics ();
}

// Return TRUE if NAME is computable by GORI in BB.

bool
vr_gori_interface::gori_computable_p (tree name, basic_block bb)
{
  return m_gori_map.is_export_p (name, bb);
}

// Calculate the range for NAME on edge E and return it in R.
// Return FALSE if unable to compute a range.

bool
vr_gori_interface::outgoing_edge_range_p (irange &r, edge e, tree name,
				const irange *known_range ATTRIBUTE_UNUSED)
{
  if (!gori_compute::outgoing_edge_range_p (r, e, name))
    r.set_varying (TREE_TYPE (name));
  if (!r.singleton_p ())
    refine_range_with_equivalences (r, e, name);
  widest_irange tmp;
  range_of_ssa_name (tmp, name);
  r.intersect (tmp);
  return !r.varying_p ();
}

// R is a known range for NAME on edge E.  Refine it with any
// equivalences NAME may have.

bool
vr_gori_interface::refine_range_with_equivalences (irange &r,
						   edge e, tree name)
{
  widest_irange branch_range, tmp;
  gimple *branch = gimple_outgoing_edge_range_p (branch_range, e);
  if (!branch)
    return false;

  // Solve each equivalence and use them to refine the range.
  bitmap_iterator bi;
  unsigned i;
  bitmap gori_exports = m_gori_map.exports (e->src);
  EXECUTE_IF_SET_IN_BITMAP (gori_exports, 0, i, bi)
    {
      tree equiv = ssa_name (i);
      if (equiv == name)
	continue;
      widest_irange equiv_range;
      if (solve_name_at_statement (equiv_range, equiv, branch, branch_range))
	{
	  gimple *equiv_def = SSA_NAME_DEF_STMT (equiv);
	  if (!equiv_def->bb
	      || !dominated_by_p (CDI_DOMINATORS, e->src, equiv_def->bb))
	    continue;

	  if (solve_name_given_equivalence (tmp, name, equiv, equiv_range))
	    r.intersect (tmp);
	}
    }
  return !r.varying_p ();
}

// Compute a range for NAME as it would appear in STMT and return it
// in R.  LHS is the known range for STMT.

bool
vr_gori_interface::solve_name_at_statement (irange &r,
					    tree name, gimple *stmt,
					    const irange &lhs)
{
  widest_irange name_range;
  range_of_ssa_name (name_range, name, stmt);
  return compute_operand_range (r, stmt, lhs, name, &name_range);
}

bool
vr_gori_interface::solve_name_given_equivalence (irange &r,
						 tree name,
						 tree equiv,
						 const irange &equiv_range)
{
  // Solve NAME in EQUIV = USE(NAME).
  gimple *def = SSA_NAME_DEF_STMT (equiv);
  if (gimple_range_handler (def) && gimple_range_operand1 (def) == name)
    {
      widest_irange op2_range;
      if (tree op2 = gimple_range_operand2 (def))
	range_of_expr (op2_range, op2, def);
      else
	{
	  tree type = TREE_TYPE (gimple_range_operand1 (def));
	  op2_range.set_varying (type);
	}
      return gimple_range_calc_op1 (def, r, equiv_range, op2_range);
    }
  // Solve NAME in NAME = USE(EQUIV).
  def =  SSA_NAME_DEF_STMT (name);
  if (gimple_range_handler (def) && gimple_range_operand1 (def) == equiv)
    {
      widest_irange op2_range;
      if (tree op2 = gimple_range_operand2 (def))
	range_of_expr (op2_range, op2, def);
      else
	{
	  tree type = gimple_expr_type (def);
	  op2_range.set_varying (type);
	}
      return gimple_range_fold (def, r, equiv_range, op2_range);
    }
  return false;
}

bool
trace_vr_gori_interface::refine_range_with_equivalences (irange &r,
							   edge e, tree name)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "refine_range_with_equivalences (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") on edge %d->%d, with range ",
	       e->src->index, e->dest->index);
      r.dump (dump_file);
      fputc ('\n', dump_file);
      indent += bump;
    }
  bool res = super::refine_range_with_equivalences (r, e, name);
  return trailer (idx, "refine_range_with_equivalences", res, name, r);
}

// Class to assert that gori/ranger provide ranges that are at least
// as good as evrp.

class vr_gori_comparison
{
public:
  vr_gori_comparison (const value_range *r_evrp, const irange *r_gori);
  void compare (tree name, edge, vr_values *vr);
private:
  void dump_differences_and_trap () const;
  void dump_differences (FILE *) const;
  void dump_improvements (FILE *) const;
  bool gori_range_is_same () const;
  bool gori_range_is_better () const;
  tree m_name;
  edge m_edge;
  const irange *m_range_evrp;
  const irange *m_range_gori;
  vr_values *m_vr_values;
};

vr_gori_comparison::vr_gori_comparison (const value_range *r_evrp,
					const irange *r_gori)
{
  m_range_evrp = r_evrp;
  m_range_gori = r_gori;
}

void
vr_gori_comparison::compare (tree name, edge e, vr_values *vr)
{
  m_name = name;
  m_edge = e;
  m_vr_values = vr;

  if (gori_range_is_same ())
    return;
  if (gori_range_is_better ())
    {
      if (dump_file)
	dump_improvements (dump_file);
      return;
    }
  dump_differences_and_trap ();
}

bool
vr_gori_comparison::gori_range_is_same () const
{
  // We may be able to normalize a symbolic to a [MIN,MAX] plus
  // or minus the end-points.  Don't count that as a win just yet.
  if (m_range_evrp && m_range_evrp->symbolic_p ())
    return true;
  // Treat UNDEFINED and VARYING as interchangeable.
  widest_irange evrp;
  if (m_range_evrp)
    evrp = *m_range_evrp;
  if (evrp.undefined_p () && m_range_gori->varying_p ())
    return true;
  if (evrp.varying_p () && m_range_gori->undefined_p ())
    return true;
  return evrp == *m_range_gori;
}

bool
vr_gori_comparison::gori_range_is_better () const
{
  if (gori_range_is_same ())
    return false;
  if (!m_range_evrp)
    return true;
  if (!range_has_numeric_bounds_p (m_range_evrp))
    {
      gcc_checking_assert (range_has_numeric_bounds_p (m_range_gori));
      return true;
    }
  widest_irange inter (*m_range_gori);
  inter.intersect (*m_range_evrp);
  return inter == *m_range_gori;
}

void
vr_gori_comparison::dump_differences_and_trap () const
{
  bool dumping = getenv("GORIME") && !strcmp (getenv ("GORIME"), "dump");
  if (dumping)
    {
      FILE *out = fopen ("/tmp/gori-differences", "a");
      fprintf (out, "=========FILE: %s ========\n",
	       main_input_filename ? main_input_filename : "UNKNOWN");
      dump_differences (out);
      fclose (out);
      return;
    }
  dump_differences (stderr);
  gcc_unreachable ();
}

void
vr_gori_comparison::dump_differences (FILE *out) const
{
  fprintf (out, "Different ranges on edge (%d -> %d) for SSA: ",
	   m_edge->src->index, m_edge->dest->index);
  print_generic_stmt (out, m_name, TDF_VOPS|TDF_MEMSYMS);
  fprintf (out, "\tevrp: ");
  m_range_evrp->dump (out);
  fprintf (out, "\n\tgori: ");
  m_range_gori->dump (out);
  fprintf (out, "\n\n");
  dump_bb (out, m_edge->src, 0, TDF_NONE);
  fprintf (out, "\n");
  fprintf (out, "==============================================\n");
  dump_function_to_file (current_function_decl, out, TDF_NONE);

  dump_flags_t save = dump_flags;
  dump_flags |= TDF_GORI;
  m_vr_values->dump_all_value_ranges (out);
  dump_flags = save;
}

void
vr_gori_comparison::dump_improvements (FILE *out) const
{
  if (m_range_evrp && !range_has_numeric_bounds_p (m_range_evrp))
    return;
  if (gori_range_is_better ())
    {
      fprintf (out, "GORI improved: ");
      print_generic_expr (out, m_name);
      fprintf (out, " from: ");
      if (m_range_evrp)
	m_range_evrp->dump (out);
      else
	fprintf (out, "UNDEFINED");
      fprintf (out, " to: ");
      m_range_gori->dump (out);
      fprintf (out, "\n");
    }
}

value_range_equiv *
evrp_range_analyzer::try_find_new_range_for_assert (const assert_info &assert,
						    edge e)
{
  tree name = assert.name;
  widest_irange vr_gori;
  widest_irange vr_ranger;
  widest_irange tmp;
  bitmap_iterator bi;
  bool gori_can_calculate = (assert.gori_computable_p
			     && gori->gori_computable_p (name, e->src));

  if (gori_can_calculate)
    {
      if (!gori->outgoing_edge_range_p (vr_gori, e, name))
	vr_gori.set_varying (TREE_TYPE (name));
    }

  value_range_equiv *vr
    = try_find_new_range (name, assert.expr, assert.comp_code, assert.val);

  ranger->range_on_edge (vr_ranger, e, name);
  if (vr && vr->equiv() && !bitmap_empty_p (vr->equiv ()))
    {
      unsigned x;
      EXECUTE_IF_SET_IN_BITMAP (vr->equiv (), 0, x, bi)
	{
	  if (ssa_name (x) == name)
	    continue;
	  ranger->range_on_edge (tmp, e, ssa_name (x));
	  vr_ranger.intersect (tmp);
	}
    }

//  if (vr && gori_can_calculate)
//    {
//      tmp = vr_ranger;
//      tmp.union_ (*vr);
//      gcc_assert (tmp == *vr);
//    }

  if (gori_can_calculate)
    {
      if (CHECKING_P && dbg_cnt (evrp_find_range))
	{
	  vr_gori_comparison comp (vr, &vr_gori);
	  comp.compare (name, e, vr_values);
	}
      if (!vr)
	{
	  vr = vr_values->allocate_value_range_equiv ();
	  vr->set_varying (TREE_TYPE (name));
	}
      static_cast <irange *> (vr)->intersect (vr_gori);
    }
  return vr;
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

	  auto_vec<std::pair<tree, value_range_equiv *>, 8> vrs;
	  for (unsigned i = 0; i < asserts.length (); ++i)
	    {
	      value_range_equiv *vr
		= try_find_new_range_for_assert (asserts[i], pred_e);
	      if (vr)
		vrs.safe_push (std::make_pair (asserts[i].name, vr));
	    }

	  /* If pred_e is really a fallthru we can record value ranges
	     in SSA names as well.  */
	  bool is_fallthru = assert_unreachable_fallthru_edge_p (pred_e);

	  /* Push updated ranges only after finding all of them to avoid
	     ordering issues that can lead to worse ranges.  */
	  for (unsigned i = 0; i < vrs.length (); ++i)
	    {
	      /* But make sure we do not weaken ranges like when
	         getting first [64, +INF] and then ~[0, 0] from
		 conditions like (s & 0x3cc0) == 0).  */
	      const value_range_equiv *old_vr
		= get_value_range (vrs[i].first);
	      value_range tem (*old_vr);
	      tem.intersect (vrs[i].second);
	      if (tem.equal_p (*old_vr))
		{
		  vr_values->free_value_range (vrs[i].second);
		  continue;
		}
	      push_value_range (vrs[i].first, vrs[i].second);
	      if (is_fallthru
		  && m_update_global_ranges
		  && all_uses_feed_or_dominated_by_stmt (vrs[i].first, stmt))
		{
		  set_ssa_range_info (vrs[i].first, vrs[i].second);
		  maybe_set_nonzero_bits (pred_e, vrs[i].first);
		}
	    }
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

      /* Skips floats and other things we can't represent in a
	 range.  */
      if (!value_range::supports_type_p (TREE_TYPE (lhs)))
	continue;

      value_range_equiv vr_result;
      bool interesting = stmt_interesting_for_vrp (phi);
      if (!has_unvisited_preds && interesting)
	vr_values->extract_range_from_phi_node (phi, &vr_result);
      else
	{
	  vr_result.set_varying (TREE_TYPE (lhs));
	  /* When we have an unvisited executable predecessor we can't
	     use PHI arg ranges which may be still UNDEFINED but have
	     to use VARYING for them.  But we can still resort to
	     SCEV for loop header PHIs.  */
	  class loop *l;
	  if (scev_initialized_p ()
	      && interesting
	      && (l = loop_containing_stmt (phi))
	      && l->header == gimple_bb (phi))
	  vr_values->adjust_range_with_scev (&vr_result, l, phi, lhs);
	}
      vr_values->update_value_range (lhs, &vr_result);

      /* Set the SSA with the value range.  */
      if (m_update_global_ranges)
	set_ssa_range_info (lhs, &vr_result);
    }
}

/* Record ranges from STMT into our VR_VALUES class.  If TEMPORARY is
   true, then this is a temporary equivalence and should be recorded
   into the unwind table.  Othewise record the equivalence into the
   global table.  */

void
evrp_range_analyzer::record_ranges_from_stmt (gimple *stmt, bool temporary)
{
  tree output = NULL_TREE;

  if (!optimize)
    return;

  if (dyn_cast <gcond *> (stmt))
    ;
  else if (stmt_interesting_for_vrp (stmt))
    {
      edge taken_edge;
      value_range_equiv vr;
      vr_values->extract_range_from_stmt (stmt, &taken_edge, &output, &vr);
      if (output)
	{
	  /* Set the SSA with the value range.  There are two cases to
	     consider.  First (the the most common) is we are processing
	     STMT in a context where its resulting range globally holds
	     and thus it can be reflected into the global ranges and need
	     not be unwound as we leave scope.

	     The second case occurs if we are processing a statement in
	     a context where the resulting range must not be reflected
	     into the global tables and must be unwound as we leave
	     the current context.  This happens in jump threading for
	     example.  */
	  if (!temporary)
	    {
	      /* Case one.  We can just update the underlying range
		 information as well as the global information.  */
	      vr_values->update_value_range (output, &vr);
	      if (m_update_global_ranges)
		set_ssa_range_info (output, &vr);
	    }
	  else
	    {
	      /* We're going to need to unwind this range.  We cannot
		 use VR as that's a stack object.  We have to allocate
		 a new range and push the old range onto the stack.  We
		 also have to be very careful about sharing the underlying
		 bitmaps.  Ugh.  */
	      value_range_equiv *new_vr
		= vr_values->allocate_value_range_equiv ();
	      new_vr->set (vr.min (), vr.max (), NULL, vr.kind ());
	      vr.equiv_clear ();
	      push_value_range (output, new_vr);
	    }
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
		  value_range_equiv *op_range
		    = try_find_new_range (t, t, comp_code, value);
		  if (op_range)
		    push_value_range (t, op_range);
		}
	    }
	  /* Add VR when (OP COMP_CODE value) condition is true.  */
	  value_range_equiv *op_range = try_find_new_range (op, op,
							    comp_code, value);
	  if (op_range)
	    push_value_range (op, op_range);
	}
    }
}

/* Unwind recorded ranges to their most recent state.  */

void
evrp_range_analyzer::pop_to_marker (void)
{
  gcc_checking_assert (!stack.is_empty ());
  while (stack.last ().first != NULL_TREE)
    pop_value_range ();
  stack.pop ();
}

/* Restore/pop VRs valid only for BB when we leave BB.  */

void
evrp_range_analyzer::leave (basic_block bb ATTRIBUTE_UNUSED)
{
  if (!optimize)
    return;
  pop_to_marker ();
}


/* Push the Value Range of VAR to the stack and update it with new VR.  */

void
evrp_range_analyzer::push_value_range (tree var, value_range_equiv *vr)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "pushing new range for ");
      print_generic_expr (dump_file, var);
      fprintf (dump_file, ": ");
      dump_value_range (dump_file, vr);
      fprintf (dump_file, "\n");
    }
  value_range_equiv *old_vr = vr_values->swap_vr_value (var, vr);
  stack.safe_push (std::make_pair (var, old_vr));
}

/* Pop a Value Range from the vrp_stack.  */

void
evrp_range_analyzer::pop_value_range ()
{
  std::pair<tree, value_range_equiv *> e = stack.pop ();
  tree var = e.first;
  value_range_equiv *vr = e.second;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "popping range for ");
      print_generic_expr (dump_file, var);
      fprintf (dump_file, ", restoring ");
      dump_value_range (dump_file, vr);
      fprintf (dump_file, "\n");
    }
  /* We saved off a lattice entry, now give it back and release
     the one we popped.  */
  value_range_equiv *popped_vr = vr_values->swap_vr_value (var, vr);
  if (popped_vr)
    vr_values->free_value_range (popped_vr);
}
