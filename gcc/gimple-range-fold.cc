/* Code for GIMPLE range related routines.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "optabs-tree.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "wide-int.h"
#include "fold-const.h"
#include "case-cfn-macros.h"
#include "omp-general.h"
#include "cfgloop.h"
#include "tree-ssa-loop.h"
#include "tree-scalar-evolution.h"
#include "langhooks.h"
#include "vr-values.h"
#include "range.h"
#include "value-query.h"
#include "gimple-range-op.h"
#include "gimple-range.h"
#include "cgraph.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "ipa-utils.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
// Construct a fur_source, and set the m_query field.

fur_source::fur_source (range_query *q)
{
  if (q)
    m_query = q;
  else
    m_query = get_range_query (cfun);
  m_depend_p = false;
}

// Invoke range_of_expr on EXPR.

bool
fur_source::get_operand (vrange &r, tree expr)
{
  return m_query->range_of_expr (r, expr);
}

// Evaluate EXPR for this stmt as a PHI argument on edge E.  Use the current
// range_query to get the range on the edge.

bool
fur_source::get_phi_operand (vrange &r, tree expr, edge e)
{
  return m_query->range_on_edge (r, e, expr);
}

// Default is no relation.

relation_kind
fur_source::query_relation (tree op1 ATTRIBUTE_UNUSED,
			    tree op2 ATTRIBUTE_UNUSED)
{
  return VREL_VARYING;
}

// Default registers nothing.

void
fur_source::register_relation (gimple *s ATTRIBUTE_UNUSED,
			       relation_kind k ATTRIBUTE_UNUSED,
			       tree op1 ATTRIBUTE_UNUSED,
			       tree op2 ATTRIBUTE_UNUSED)
{
}

// Default registers nothing.

void
fur_source::register_relation (edge e ATTRIBUTE_UNUSED,
			       relation_kind k ATTRIBUTE_UNUSED,
			       tree op1 ATTRIBUTE_UNUSED,
			       tree op2 ATTRIBUTE_UNUSED)
{
}

// This version of fur_source will pick a range up off an edge.

class fur_edge : public fur_source
{
public:
  fur_edge (edge e, range_query *q = NULL);
  virtual bool get_operand (vrange &r, tree expr) override;
  virtual bool get_phi_operand (vrange &r, tree expr, edge e) override;
private:
  edge m_edge;
};

// Instantiate an edge based fur_source.

inline
fur_edge::fur_edge (edge e, range_query *q) : fur_source (q)
{
  m_edge = e;
}

// Get the value of EXPR on edge m_edge.

bool
fur_edge::get_operand (vrange &r, tree expr)
{
  return m_query->range_on_edge (r, m_edge, expr);
}

// Evaluate EXPR for this stmt as a PHI argument on edge E.  Use the current
// range_query to get the range on the edge.

bool
fur_edge::get_phi_operand (vrange &r, tree expr, edge e)
{
  // Edge to edge recalculations not supported yet, until we sort it out.
  gcc_checking_assert (e == m_edge);
  return m_query->range_on_edge (r, e, expr);
}

// Instantiate a stmt based fur_source.

fur_stmt::fur_stmt (gimple *s, range_query *q) : fur_source (q)
{
  m_stmt = s;
}

// Retrieve range of EXPR as it occurs as a use on stmt M_STMT.

bool
fur_stmt::get_operand (vrange &r, tree expr)
{
  return m_query->range_of_expr (r, expr, m_stmt);
}

// Evaluate EXPR for this stmt as a PHI argument on edge E.  Use the current
// range_query to get the range on the edge.

bool
fur_stmt::get_phi_operand (vrange &r, tree expr, edge e)
{
  // Pick up the range of expr from edge E.
  fur_edge e_src (e, m_query);
  return e_src.get_operand (r, expr);
}

// Return relation based from m_stmt.

relation_kind
fur_stmt::query_relation (tree op1, tree op2)
{
  return m_query->relation ().query (m_stmt, op1, op2);
}

// Instantiate a stmt based fur_source with a GORI object.


fur_depend::fur_depend (gimple *s, range_query *q)
  : fur_stmt (s, q)
{
  m_depend_p = true;
}

// Register a relation on a stmt if there is an oracle.

void
fur_depend::register_relation (gimple *s, relation_kind k, tree op1, tree op2)
{
  m_query->relation ().record (s, k, op1, op2);
}

// Register a relation on an edge if there is an oracle.

void
fur_depend::register_relation (edge e, relation_kind k, tree op1, tree op2)
{
  m_query->relation ().record (e, k, op1, op2);
}

// This version of fur_source will pick a range up from a list of ranges
// supplied by the caller.

class fur_list : public fur_source
{
public:
  fur_list (vrange &r1, range_query *q = NULL);
  fur_list (vrange &r1, vrange &r2, range_query *q = NULL);
  fur_list (unsigned num, vrange **list, range_query *q = NULL);
  virtual bool get_operand (vrange &r, tree expr) override;
  virtual bool get_phi_operand (vrange &r, tree expr, edge e) override;
private:
  vrange *m_local[2];
  vrange **m_list;
  unsigned m_index;
  unsigned m_limit;
};

// One range supplied for unary operations.

fur_list::fur_list (vrange &r1, range_query *q) : fur_source (q)
{
  m_list = m_local;
  m_index = 0;
  m_limit = 1;
  m_local[0] = &r1;
}

// Two ranges supplied for binary operations.

fur_list::fur_list (vrange &r1, vrange &r2, range_query *q) : fur_source (q)
{
  m_list = m_local;
  m_index = 0;
  m_limit = 2;
  m_local[0] = &r1;
  m_local[1] = &r2;
}

// Arbitrary number of ranges in a vector.

fur_list::fur_list (unsigned num, vrange **list, range_query *q)
  : fur_source (q)
{
  m_list = list;
  m_index = 0;
  m_limit = num;
}

// Get the next operand from the vector, ensure types are compatible.

bool
fur_list::get_operand (vrange &r, tree expr)
{
  // Do not use the vector for non-ssa-names, or if it has been emptied.
  if (TREE_CODE (expr) != SSA_NAME || m_index >= m_limit)
    return m_query->range_of_expr (r, expr);
  r = *m_list[m_index++];
  gcc_checking_assert (range_compatible_p (TREE_TYPE (expr), r.type ()));
  return true;
}

// This will simply pick the next operand from the vector.
bool
fur_list::get_phi_operand (vrange &r, tree expr, edge e ATTRIBUTE_UNUSED)
{
  return get_operand (r, expr);
}

// Fold stmt S into range R using R1 as the first operand.

bool
fold_range (vrange &r, gimple *s, vrange &r1, range_query *q)
{
  fold_using_range f;
  fur_list src (r1, q);
  return f.fold_stmt (r, s, src);
}

// Fold stmt S into range R using R1  and R2 as the first two operands.

bool
fold_range (vrange &r, gimple *s, vrange &r1, vrange &r2, range_query *q)
{
  fold_using_range f;
  fur_list src (r1, r2, q);
  return f.fold_stmt (r, s, src);
}

// Fold stmt S into range R using NUM_ELEMENTS from VECTOR as the initial
// operands encountered.

bool
fold_range (vrange &r, gimple *s, unsigned num_elements, vrange **vector,
	    range_query *q)
{
  fold_using_range f;
  fur_list src (num_elements, vector, q);
  return f.fold_stmt (r, s, src);
}

// Fold stmt S into range R using range query Q.

bool
fold_range (vrange &r, gimple *s, range_query *q)
{
  fold_using_range f;
  fur_stmt src (s, q);
  return f.fold_stmt (r, s, src);
}

// Recalculate stmt S into R using range query Q as if it were on edge ON_EDGE.

bool
fold_range (vrange &r, gimple *s, edge on_edge, range_query *q)
{
  fold_using_range f;
  fur_edge src (on_edge, q);
  return f.fold_stmt (r, s, src);
}

// Calculate op1 on statetemt S with LHS into range R using range query Q
// to resolve any other operands.

bool
op1_range (vrange &r, gimple *s, const vrange &lhs, range_query *q)
{
  gimple_range_op_handler handler (s);
  if (!handler)
    return false;

  fur_stmt src (s, q);

  tree op2_expr = handler.operand2 ();
  if (!op2_expr)
    return handler.calc_op1 (r, lhs);

  Value_Range op2 (TREE_TYPE (op2_expr));
  if (!src.get_operand (op2, op2_expr))
    return false;

  return handler.calc_op1 (r, lhs, op2);
}

// Calculate op1 on statetemt S into range R using range query Q.
// LHS is set to VARYING in this case.

bool
op1_range (vrange &r, gimple *s, range_query *q)
{
  tree lhs_type = gimple_range_type (s);
  if (!lhs_type)
    return false;
  Value_Range lhs_range;
  lhs_range.set_varying (lhs_type);
  return op1_range (r, s, lhs_range, q);
}

// Calculate op2 on statetemt S with LHS into range R using range query Q
// to resolve any other operands.

bool
op2_range (vrange &r, gimple *s, const vrange &lhs, range_query *q)
{

  gimple_range_op_handler handler (s);
  if (!handler)
    return false;

  fur_stmt src (s, q);

  Value_Range op1 (TREE_TYPE (handler.operand1 ()));
  if (!src.get_operand (op1, handler.operand1 ()))
    return false;

  return handler.calc_op2 (r, lhs, op1);
}

// Calculate op2 on statetemt S into range R using range query Q.
// LHS is set to VARYING in this case.

bool
op2_range (vrange &r, gimple *s, range_query *q)
{
  tree lhs_type = gimple_range_type (s);
  if (!lhs_type)
    return false;
  Value_Range lhs_range;
  lhs_range.set_varying (lhs_type);
  return op2_range (r, s, lhs_range, q);
}

// Provide a fur_source which can be used to determine any relations on
// a statement.  It manages the callback from fold_using_ranges to determine
// a relation_trio for a statement.

class fur_relation : public fur_stmt
{
public:
  fur_relation (gimple *s, range_query *q = NULL);
  virtual void register_relation (gimple *stmt, relation_kind k, tree op1,
				  tree op2);
  virtual void register_relation (edge e, relation_kind k, tree op1,
				  tree op2);
  relation_trio trio() const;
private:
  relation_kind def_op1, def_op2, op1_op2;
};

fur_relation::fur_relation (gimple *s, range_query *q) : fur_stmt (s, q)
{
  def_op1 = def_op2 = op1_op2 = VREL_VARYING;
}

// Construct a trio from what is known.

relation_trio
fur_relation::trio () const
{
  return relation_trio (def_op1, def_op2, op1_op2);
}

// Don't support edges, but avoid a compiler warning by providing the routine.

void
fur_relation::register_relation (edge, relation_kind, tree, tree)
{
}

// Register relation K between OP1 and OP2 on STMT.

void
fur_relation::register_relation (gimple *stmt, relation_kind k, tree op1,
				 tree op2)
{
  tree lhs = gimple_get_lhs (stmt);
  tree a1 = NULL_TREE;
  tree a2 = NULL_TREE;
  switch (gimple_code (stmt))
    {
      case GIMPLE_COND:
	a1 = gimple_cond_lhs (stmt);
	a2 = gimple_cond_rhs (stmt);
	break;
      case GIMPLE_ASSIGN:
	a1 = gimple_assign_rhs1 (stmt);
	if (gimple_num_ops (stmt) >= 3)
	  a2 = gimple_assign_rhs2 (stmt);
	break;
      default:
	break;
    }
  // STMT is of the form LHS = A1 op A2, now map the relation to these
  // operands, if possible.
  if (op1 == lhs)
    {
      if (op2 == a1)
	def_op1 = k;
      else if (op2 == a2)
	def_op2 = k;
    }
  else if (op2 == lhs)
    {
      if (op1 == a1)
	def_op1 = relation_swap (k);
      else if (op1 == a2)
	def_op2 = relation_swap (k);
    }
  else
    {
      if (op1 == a1 && op2 == a2)
	op1_op2 = k;
      else if (op2 == a1 && op1 == a2)
	op1_op2 = relation_swap (k);
    }
}

// Return the relation trio for stmt S using query Q.

relation_trio
fold_relations (gimple *s, range_query *q)
{
  fold_using_range f;
  fur_relation src (s, q);
  tree lhs = gimple_range_ssa_p (gimple_get_lhs (s));
  if (lhs)
    {
      Value_Range vr(TREE_TYPE (lhs));
      if (f.fold_stmt (vr, s, src))
	return src.trio ();
    }
  return TRIO_VARYING;
}

// -------------------------------------------------------------------------

// Adjust the range for a pointer difference where the operands came
// from a memchr.
//
// This notices the following sequence:
//
//	def = __builtin_memchr (arg, 0, sz)
//	n = def - arg
//
// The range for N can be narrowed to [0, PTRDIFF_MAX - 1].

static void
adjust_pointer_diff_expr (irange &res, const gimple *diff_stmt)
{
  tree op0 = gimple_assign_rhs1 (diff_stmt);
  tree op1 = gimple_assign_rhs2 (diff_stmt);
  tree op0_ptype = TREE_TYPE (TREE_TYPE (op0));
  tree op1_ptype = TREE_TYPE (TREE_TYPE (op1));
  gimple *call;

  if (TREE_CODE (op0) == SSA_NAME
      && TREE_CODE (op1) == SSA_NAME
      && (call = SSA_NAME_DEF_STMT (op0))
      && is_gimple_call (call)
      && gimple_call_builtin_p (call, BUILT_IN_MEMCHR)
      && TYPE_MODE (op0_ptype) == TYPE_MODE (char_type_node)
      && TYPE_PRECISION (op0_ptype) == TYPE_PRECISION (char_type_node)
      && TYPE_MODE (op1_ptype) == TYPE_MODE (char_type_node)
      && TYPE_PRECISION (op1_ptype) == TYPE_PRECISION (char_type_node)
      && gimple_call_builtin_p (call, BUILT_IN_MEMCHR)
      && vrp_operand_equal_p (op1, gimple_call_arg (call, 0))
      && integer_zerop (gimple_call_arg (call, 1)))
    {
      wide_int maxm1 = irange_val_max (ptrdiff_type_node) - 1;
      res.intersect (int_range<2> (ptrdiff_type_node,
				   wi::zero (TYPE_PRECISION (ptrdiff_type_node)),
				   maxm1));
    }
}

// Adjust the range for an IMAGPART_EXPR.

static void
adjust_imagpart_expr (vrange &res, const gimple *stmt)
{
  tree name = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);

  if (TREE_CODE (name) != SSA_NAME || !SSA_NAME_DEF_STMT (name))
    return;

  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  if (is_gimple_call (def_stmt) && gimple_call_internal_p (def_stmt))
    {
      switch (gimple_call_internal_fn (def_stmt))
	{
	case IFN_ADD_OVERFLOW:
	case IFN_SUB_OVERFLOW:
	case IFN_MUL_OVERFLOW:
	case IFN_UADDC:
	case IFN_USUBC:
	case IFN_ATOMIC_COMPARE_EXCHANGE:
	  {
	    int_range<2> r;
	    r.set_varying (boolean_type_node);
	    tree type = TREE_TYPE (gimple_assign_lhs (stmt));
	    range_cast (r, type);
	    res.intersect (r);
	  }
	default:
	  break;
	}
      return;
    }
  if (is_gimple_assign (def_stmt)
      && gimple_assign_rhs_code (def_stmt) == COMPLEX_CST)
    {
      tree cst = gimple_assign_rhs1 (def_stmt);
      if (TREE_CODE (cst) == COMPLEX_CST
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (cst))) == INTEGER_TYPE)
	{
	  wide_int w = wi::to_wide (TREE_IMAGPART (cst));
	  int_range<1> imag (TREE_TYPE (TREE_IMAGPART (cst)), w, w);
	  res.intersect (imag);
	}
    }
}

// Adjust the range for a REALPART_EXPR.

static void
adjust_realpart_expr (vrange &res, const gimple *stmt)
{
  tree name = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);

  if (TREE_CODE (name) != SSA_NAME)
    return;

  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  if (!SSA_NAME_DEF_STMT (name))
    return;

  if (is_gimple_assign (def_stmt)
      && gimple_assign_rhs_code (def_stmt) == COMPLEX_CST)
    {
      tree cst = gimple_assign_rhs1 (def_stmt);
      if (TREE_CODE (cst) == COMPLEX_CST
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (cst))) == INTEGER_TYPE)
	{
	  wide_int imag = wi::to_wide (TREE_REALPART (cst));
	  int_range<2> tmp (TREE_TYPE (TREE_REALPART (cst)), imag, imag);
	  res.intersect (tmp);
	}
    }
}

// This function looks for situations when walking the use/def chains
// may provide additional contextual range information not exposed on
// this statement.

static void
gimple_range_adjustment (vrange &res, const gimple *stmt)
{
  switch (gimple_expr_code (stmt))
    {
    case POINTER_DIFF_EXPR:
      adjust_pointer_diff_expr (as_a <irange> (res), stmt);
      return;

    case IMAGPART_EXPR:
      adjust_imagpart_expr (res, stmt);
      return;

    case REALPART_EXPR:
      adjust_realpart_expr (res, stmt);
      return;

    default:
      break;
    }
}

// Calculate a range for statement S and return it in R. If NAME is provided it
// represents the SSA_NAME on the LHS of the statement. It is only required
// if there is more than one lhs/output.  If a range cannot
// be calculated, return false.

bool
fold_using_range::fold_stmt (vrange &r, gimple *s, fur_source &src, tree name)
{
  bool res = false;
  // If name and S are specified, make sure it is an LHS of S.
  gcc_checking_assert (!name || !gimple_get_lhs (s) ||
		       name == gimple_get_lhs (s));

  if (!name)
    name = gimple_get_lhs (s);

  // Process addresses.
  if (gimple_code (s) == GIMPLE_ASSIGN
      && gimple_assign_rhs_code (s) == ADDR_EXPR)
    return range_of_address (as_a <prange> (r), s, src);

  gimple_range_op_handler handler (s);
  if (handler)
    res = range_of_range_op (r, handler, src);
  else if (is_a<gphi *>(s))
    res = range_of_phi (r, as_a<gphi *> (s), src);
  else if (is_a<gcall *>(s))
    res = range_of_call (r, as_a<gcall *> (s), src);
  else if (is_a<gassign *> (s) && gimple_assign_rhs_code (s) == COND_EXPR)
    res = range_of_cond_expr (r, as_a<gassign *> (s), src);

  // If the result is varying, check for basic nonnegativeness.
  // Specifically this helps for now with strict enum in cases like
  // g++.dg/warn/pr33738.C.
  bool so_p;
  if (res && r.varying_p () && INTEGRAL_TYPE_P (r.type ())
      && gimple_stmt_nonnegative_warnv_p (s, &so_p))
    r.set_nonnegative (r.type ());

  if (!res)
    {
      // If no name specified or range is unsupported, bail.
      if (!name || !gimple_range_ssa_p (name))
	return false;
      // We don't understand the stmt, so return the global range.
      gimple_range_global (r, name);
      return true;
    }

  if (r.undefined_p ())
    return true;

  // We sometimes get compatible types copied from operands, make sure
  // the correct type is being returned.
  if (name && TREE_TYPE (name) != r.type ())
    {
      gcc_checking_assert (range_compatible_p (r.type (), TREE_TYPE (name)));
      range_cast (r, TREE_TYPE (name));
    }
  return true;
}

// Calculate a range for range_op statement S and return it in R.  If any
// If a range cannot be calculated, return false.

bool
fold_using_range::range_of_range_op (vrange &r,
				     gimple_range_op_handler &handler,
				     fur_source &src)
{
  gcc_checking_assert (handler);
  gimple *s = handler.stmt ();
  tree type = gimple_range_type (s);
  if (!type)
    return false;

  tree lhs = handler.lhs ();
  tree op1 = handler.operand1 ();
  tree op2 = handler.operand2 ();

  // Certain types of builtin functions may have no arguments.
  if (!op1)
    {
      Value_Range r1 (type);
      if (!handler.fold_range (r, type, r1, r1))
	r.set_varying (type);
      return true;
    }

  Value_Range range1 (TREE_TYPE (op1));
  Value_Range range2 (op2 ? TREE_TYPE (op2) : TREE_TYPE (op1));

  if (src.get_operand (range1, op1))
    {
      if (!op2)
	{
	  // Fold range, and register any dependency if available.
	  Value_Range r2 (type);
	  r2.set_varying (type);
	  if (!handler.fold_range (r, type, range1, r2))
	    r.set_varying (type);
	  if (lhs && gimple_range_ssa_p (op1))
	    {
	      if (src.gori_ssa ())
		src.gori_ssa ()->register_dependency (lhs, op1);
	      relation_kind rel;
	      rel = handler.lhs_op1_relation (r, range1, range1);
	      if (rel != VREL_VARYING)
		src.register_relation (s, rel, lhs, op1);
	    }
	}
      else if (src.get_operand (range2, op2))
	{
	  relation_kind rel = src.query_relation (op1, op2);
	  if (dump_file && (dump_flags & TDF_DETAILS) && rel != VREL_VARYING)
	    {
	      fprintf (dump_file, " folding with relation ");
	      print_generic_expr (dump_file, op1, TDF_SLIM);
	      print_relation (dump_file, rel);
	      print_generic_expr (dump_file, op2, TDF_SLIM);
	      fputc ('\n', dump_file);
	    }
	  // Fold range, and register any dependency if available.
	  if (!handler.fold_range (r, type, range1, range2,
				   relation_trio::op1_op2 (rel)))
	    r.set_varying (type);
	  if (irange::supports_p (type))
	    relation_fold_and_or (as_a <irange> (r), s, src, range1, range2);
	  if (lhs)
	    {
	      if (src.gori_ssa ())
		{
		  src.gori_ssa ()->register_dependency (lhs, op1);
		  src.gori_ssa ()->register_dependency (lhs, op2);
		}
	      if (gimple_range_ssa_p (op1))
		{
		  rel = handler.lhs_op1_relation (r, range1, range2, rel);
		  if (rel != VREL_VARYING)
		    src.register_relation (s, rel, lhs, op1);
		}
	      if (gimple_range_ssa_p (op2))
		{
		  rel = handler.lhs_op2_relation (r, range1, range2, rel);
		  if (rel != VREL_VARYING)
		    src.register_relation (s, rel, lhs, op2);
		}
	    }
	  // Check for an existing BB, as we maybe asked to fold an
	  // artificial statement not in the CFG.
	  else if (is_a<gcond *> (s) && gimple_bb (s))
	    {
	      basic_block bb = gimple_bb (s);
	      edge e0 = EDGE_SUCC (bb, 0);
	      edge e1 = EDGE_SUCC (bb, 1);

	      if (!single_pred_p (e0->dest))
		e0 = NULL;
	      if (!single_pred_p (e1->dest))
		e1 = NULL;
	      src.register_outgoing_edges (as_a<gcond *> (s),
					   as_a <irange> (r), e0, e1);
	    }
	}
      else
	r.set_varying (type);
    }
  else
    r.set_varying (type);
  // Make certain range-op adjustments that aren't handled any other way.
  gimple_range_adjustment (r, s);
  return true;
}

// Calculate the range of an assignment containing an ADDR_EXPR.
// Return the range in R.
// If a range cannot be calculated, set it to VARYING and return true.

bool
fold_using_range::range_of_address (prange &r, gimple *stmt, fur_source &src)
{
  gcc_checking_assert (gimple_code (stmt) == GIMPLE_ASSIGN);
  gcc_checking_assert (gimple_assign_rhs_code (stmt) == ADDR_EXPR);

  bool strict_overflow_p;
  tree expr = gimple_assign_rhs1 (stmt);
  poly_int64 bitsize, bitpos;
  tree offset;
  machine_mode mode;
  int unsignedp, reversep, volatilep;
  tree base = get_inner_reference (TREE_OPERAND (expr, 0), &bitsize,
				   &bitpos, &offset, &mode, &unsignedp,
				   &reversep, &volatilep);


  if (base != NULL_TREE
      && TREE_CODE (base) == MEM_REF
      && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME)
    {
      tree ssa = TREE_OPERAND (base, 0);
      tree lhs = gimple_get_lhs (stmt);
      if (lhs && gimple_range_ssa_p (ssa) && src.gori_ssa ())
	src.gori_ssa ()->register_dependency (lhs, ssa);
      src.get_operand (r, ssa);
      range_cast (r, TREE_TYPE (gimple_assign_rhs1 (stmt)));

      poly_offset_int off = 0;
      bool off_cst = false;
      if (offset == NULL_TREE || TREE_CODE (offset) == INTEGER_CST)
	{
	  off = mem_ref_offset (base);
	  if (offset)
	    off += poly_offset_int::from (wi::to_poly_wide (offset),
					  SIGNED);
	  off <<= LOG2_BITS_PER_UNIT;
	  off += bitpos;
	  off_cst = true;
	}
      /* If &X->a is equal to X, the range of X is the result.  */
      if (off_cst && known_eq (off, 0))
	return true;
      else if (flag_delete_null_pointer_checks
	       && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (expr)))
	{
	  /* For -fdelete-null-pointer-checks -fno-wrapv-pointer we don't
	     allow going from non-NULL pointer to NULL.  */
	  if (r.undefined_p ()
	      || !r.contains_p (wi::zero (TYPE_PRECISION (TREE_TYPE (expr)))))
	    {
	      /* We could here instead adjust r by off >> LOG2_BITS_PER_UNIT
		 using POINTER_PLUS_EXPR if off_cst and just fall back to
		 this.  */
	      r.set_nonzero (TREE_TYPE (gimple_assign_rhs1 (stmt)));
	      return true;
	    }
	}
      /* If MEM_REF has a "positive" offset, consider it non-NULL
	 always, for -fdelete-null-pointer-checks also "negative"
	 ones.  Punt for unknown offsets (e.g. variable ones).  */
      if (!TYPE_OVERFLOW_WRAPS (TREE_TYPE (expr))
	  && off_cst
	  && known_ne (off, 0)
	  && (flag_delete_null_pointer_checks || known_gt (off, 0)))
	{
	  r.set_nonzero (TREE_TYPE (gimple_assign_rhs1 (stmt)));
	  return true;
	}
      r.set_varying (TREE_TYPE (gimple_assign_rhs1 (stmt)));
      return true;
    }

  // Handle "= &a".
  if (tree_single_nonzero_warnv_p (expr, &strict_overflow_p))
    {
      r.set_nonzero (TREE_TYPE (gimple_assign_rhs1 (stmt)));
      return true;
    }

  // Otherwise return varying.
  r.set_varying (TREE_TYPE (gimple_assign_rhs1 (stmt)));
  return true;
}

// Calculate a range for phi statement S and return it in R.
// If a range cannot be calculated, return false.

bool
fold_using_range::range_of_phi (vrange &r, gphi *phi, fur_source &src)
{
  tree phi_def = gimple_phi_result (phi);
  tree type = gimple_range_type (phi);
  Value_Range arg_range (type);
  Value_Range equiv_range (type);
  unsigned x;

  if (!type)
    return false;

  // Track if all executable arguments are the same.
  tree single_arg = NULL_TREE;
  bool seen_arg = false;

  relation_oracle *oracle = &(src.query()->relation ());
  // Start with an empty range, unioning in each argument's range.
  r.set_undefined ();
  for (x = 0; x < gimple_phi_num_args (phi); x++)
    {
      tree arg = gimple_phi_arg_def (phi, x);
      // An argument that is the same as the def provides no new range.
      if (arg == phi_def)
	continue;

      edge e = gimple_phi_arg_edge (phi, x);

      // Get the range of the argument on its edge.
      src.get_phi_operand (arg_range, arg, e);

      if (!arg_range.undefined_p ())
	{
	  // Register potential dependencies for stale value tracking.
	  // Likewise, if the incoming PHI argument is equivalent to this
	  // PHI definition, it provides no new info.  Accumulate these ranges
	  // in case all arguments are equivalences.
	  if (oracle->query (e, arg, phi_def) == VREL_EQ)
	    equiv_range.union_(arg_range);
	  else
	    r.union_ (arg_range);

	  if (gimple_range_ssa_p (arg) && src.gori_ssa ())
	    src.gori_ssa ()->register_dependency (phi_def, arg);
	}

      // Track if all arguments are the same.
      if (!seen_arg)
	{
	  seen_arg = true;
	  single_arg = arg;
	}
      else if (single_arg != arg)
	single_arg = NULL_TREE;

      // Once the value reaches varying, stop looking.
      if (r.varying_p () && single_arg == NULL_TREE)
	break;
    }

  // If all arguments were equivalences, use the equivalence ranges as no
  // arguments were processed.
  if (r.undefined_p () && !equiv_range.undefined_p ())
    r = equiv_range;

  // If the PHI boils down to a single effective argument, look at it.
  if (single_arg)
    {
      // Symbolic arguments can be equivalences.
      if (gimple_range_ssa_p (single_arg))
	{
	  // Only allow the equivalence if the PHI definition does not
	  // dominate any incoming edge for SINGLE_ARG.
	  // See PR 108139 and 109462.
	  basic_block bb = gimple_bb (phi);
	  if (!dom_info_available_p (CDI_DOMINATORS))
	    single_arg = NULL;
	  else
	    for (x = 0; x < gimple_phi_num_args (phi); x++)
	      if (gimple_phi_arg_def (phi, x) == single_arg
		  && dominated_by_p (CDI_DOMINATORS,
				      gimple_phi_arg_edge (phi, x)->src,
				      bb))
		{
		  single_arg = NULL;
		  break;
		}
	  if (single_arg)
	    src.register_relation (phi, VREL_EQ, phi_def, single_arg);
	}
      else if (src.get_operand (arg_range, single_arg)
	       && arg_range.singleton_p ())
	{
	  // Numerical arguments that are a constant can be returned as
	  // the constant. This can help fold later cases where even this
	  // constant might have been UNDEFINED via an unreachable edge.
	  r = arg_range;
	  return true;
	}
    }

  // If PHI analysis is available, see if there is an iniital range.
  if (phi_analysis_available_p ()
      && irange::supports_p (TREE_TYPE (phi_def)))
    {
      phi_group *g = (phi_analysis())[phi_def];
      if (g && !(g->range ().varying_p ()))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "PHI GROUP query for ");
	      print_generic_expr (dump_file, phi_def, TDF_SLIM);
	      fprintf (dump_file, " found : ");
	      g->range ().dump (dump_file);
	      fprintf (dump_file, " and adjusted original range from :");
	      r.dump (dump_file);
	    }
	  r.intersect (g->range ());
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " to :");
	      r.dump (dump_file);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  // If SCEV is available, query if this PHI has any known values.
  if (scev_initialized_p ()
      && !POINTER_TYPE_P (TREE_TYPE (phi_def)))
    {
      class loop *l = loop_containing_stmt (phi);
      if (l && loop_outer (l))
	{
	  Value_Range loop_range (type);
	  range_of_ssa_name_with_loop_info (loop_range, phi_def, l, phi, src);
	  if (!loop_range.varying_p ())
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Loops range found for ");
		  print_generic_expr (dump_file, phi_def, TDF_SLIM);
		  fprintf (dump_file, ": ");
		  loop_range.dump (dump_file);
		  fprintf (dump_file, " and calculated range :");
		  r.dump (dump_file);
		  fprintf (dump_file, "\n");
		}
	      r.intersect (loop_range);
	    }
	}
    }

  return true;
}

// Calculate a range for call statement S and return it in R.
// If a range cannot be calculated, return false.

bool
fold_using_range::range_of_call (vrange &r, gcall *call, fur_source &)
{
  tree type = gimple_range_type (call);
  if (!type)
    return false;

  tree lhs = gimple_call_lhs (call);
  bool strict_overflow_p;

  if (gimple_stmt_nonnegative_warnv_p (call, &strict_overflow_p))
    r.set_nonnegative (type);
  else if (gimple_call_nonnull_result_p (call)
	   || gimple_call_nonnull_arg (call))
    r.set_nonzero (type);
  else
    r.set_varying (type);

  tree callee = gimple_call_fndecl (call);
  if (callee
      && useless_type_conversion_p (TREE_TYPE (TREE_TYPE (callee)), type))
    {
      Value_Range val;
      if (ipa_return_value_range (val, callee))
	{
	  r.intersect (val);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Using return value range of ");
	      print_generic_expr (dump_file, callee, TDF_SLIM);
	      fprintf (dump_file, ": ");
	      val.dump (dump_file);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  // If there is an LHS, intersect that with what is known.
  if (lhs)
    {
      Value_Range def (TREE_TYPE (lhs));
      gimple_range_global (def, lhs);
      r.intersect (def);
    }
  return true;
}

// Given COND ? OP1 : OP2 with ranges R1 for OP1 and R2 for OP2, Use gori
// to further resolve R1 and R2 if there are any dependencies between
// OP1 and COND or OP2 and COND.  All values can are to be calculated using SRC
// as the origination source location for operands..
// Effectively, use COND an the edge condition and solve for OP1 on the true
// edge and OP2 on the false edge.

bool
fold_using_range::condexpr_adjust (vrange &r1, vrange &r2, gimple *, tree cond,
				   tree op1, tree op2, fur_source &src)
{
  if (!src.gori () || !src.gori_ssa ())
    return false;

  tree ssa1 = gimple_range_ssa_p (op1);
  tree ssa2 = gimple_range_ssa_p (op2);
  if (!ssa1 && !ssa2)
    return false;
  if (TREE_CODE (cond) != SSA_NAME)
    return false;
  gassign *cond_def = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (cond));
  if (!cond_def
      || TREE_CODE_CLASS (gimple_assign_rhs_code (cond_def)) != tcc_comparison)
    return false;
  tree type = TREE_TYPE (gimple_assign_rhs1 (cond_def));
  if (!range_compatible_p (type, TREE_TYPE (gimple_assign_rhs2 (cond_def))))
    return false;
  range_op_handler hand (gimple_assign_rhs_code (cond_def));
  if (!hand)
    return false;

  tree c1 = gimple_range_ssa_p (gimple_assign_rhs1 (cond_def));
  tree c2 = gimple_range_ssa_p (gimple_assign_rhs2 (cond_def));

  // Only solve if there is one SSA name in the condition.
  if ((!c1 && !c2) || (c1 && c2))
    return false;

  // Pick up the current values of each part of the condition.
  tree rhs1 = gimple_assign_rhs1 (cond_def);
  tree rhs2 = gimple_assign_rhs2 (cond_def);
  Value_Range cl (TREE_TYPE (rhs1));
  Value_Range cr (TREE_TYPE (rhs2));
  src.get_operand (cl, rhs1);
  src.get_operand (cr, rhs2);

  tree cond_name = c1 ? c1 : c2;
  gimple *def_stmt = SSA_NAME_DEF_STMT (cond_name);

  // Evaluate the value of COND_NAME on the true and false edges, using either
  // the op1 or op2 routines based on its location.
  Value_Range cond_true (type), cond_false (type);
  if (c1)
    {
      if (!hand.op1_range (cond_false, type, range_false (), cr))
	return false;
      if (!hand.op1_range (cond_true, type, range_true (), cr))
	return false;
      cond_false.intersect (cl);
      cond_true.intersect (cl);
    }
  else
    {
      if (!hand.op2_range (cond_false, type, range_false (), cl))
	return false;
      if (!hand.op2_range (cond_true, type, range_true (), cl))
	return false;
      cond_false.intersect (cr);
      cond_true.intersect (cr);
    }

   // Now solve for SSA1 or SSA2 if they are in the dependency chain.
   if (ssa1 && src.gori_ssa()->in_chain_p (ssa1, cond_name))
    {
      Value_Range tmp1 (TREE_TYPE (ssa1));
      if (src.gori ()->compute_operand_range (tmp1, def_stmt, cond_true,
	  ssa1, src))
	r1.intersect (tmp1);
    }
  if (ssa2 && src.gori_ssa ()->in_chain_p (ssa2, cond_name))
    {
      Value_Range tmp2 (TREE_TYPE (ssa2));
      if (src.gori ()->compute_operand_range (tmp2, def_stmt, cond_false,
	  ssa2, src))
	r2.intersect (tmp2);
    }
  return true;
}

// Calculate a range for COND_EXPR statement S and return it in R.
// If a range cannot be calculated, return false.

bool
fold_using_range::range_of_cond_expr  (vrange &r, gassign *s, fur_source &src)
{
  tree cond = gimple_assign_rhs1 (s);
  tree op1 = gimple_assign_rhs2 (s);
  tree op2 = gimple_assign_rhs3 (s);

  tree type = gimple_range_type (s);
  if (!type)
    return false;

  Value_Range range1 (TREE_TYPE (op1));
  Value_Range range2 (TREE_TYPE (op2));
  Value_Range cond_range (TREE_TYPE (cond));
  gcc_checking_assert (gimple_assign_rhs_code (s) == COND_EXPR);
  gcc_checking_assert (range_compatible_p (TREE_TYPE (op1), TREE_TYPE (op2)));
  src.get_operand (cond_range, cond);
  src.get_operand (range1, op1);
  src.get_operand (range2, op2);

  // Try to see if there is a dependence between the COND and either operand
  if (condexpr_adjust (range1, range2, s, cond, op1, op2, src))
    if (dump_file && (dump_flags & TDF_DETAILS))
      {
	fprintf (dump_file, "Possible COND_EXPR adjustment. Range op1 : ");
	range1.dump(dump_file);
	fprintf (dump_file, " and Range op2: ");
	range2.dump(dump_file);
	fprintf (dump_file, "\n");
      }

  // If the condition is known, choose the appropriate expression.
  if (cond_range.singleton_p ())
    {
      // False, pick second operand.
      if (cond_range.zero_p ())
	r = range2;
      else
	r = range1;
    }
  else
    {
      r = range1;
      r.union_ (range2);
    }
  gcc_checking_assert (r.undefined_p ()
		       || range_compatible_p (r.type (), type));
  return true;
}

// If SCEV has any information about phi node NAME, return it as a range in R.

void
fold_using_range::range_of_ssa_name_with_loop_info (vrange &r, tree name,
						    class loop *l, gphi *phi,
						    fur_source &src)
{
  gcc_checking_assert (TREE_CODE (name) == SSA_NAME);
  // SCEV currently invokes get_range_query () for values.  If the query
  // being passed in is not the same SCEV will use, do not invoke SCEV.
  // This can be remove if/when SCEV uses a passed in range-query.
  if (src.query () != get_range_query (cfun)
      || !range_of_var_in_loop (r, name, l, phi, src.query ()))
    r.set_varying (TREE_TYPE (name));
}

// -----------------------------------------------------------------------

// Check if an && or || expression can be folded based on relations. ie
//   c_2 = a_6 > b_7
//   c_3 = a_6 < b_7
//   c_4 = c_2 && c_3
// c_2 and c_3 can never be true at the same time,
// Therefore c_4 can always resolve to false based purely on the relations.

void
fold_using_range::relation_fold_and_or (irange& lhs_range, gimple *s,
					fur_source &src, vrange &op1,
					vrange &op2)
{
  // No queries or already folded.
  if (!src.gori () || lhs_range.singleton_p ())
    return;

  // Only care about AND and OR expressions.
  enum tree_code code = gimple_expr_code (s);
  bool is_and = false;
  if (code == BIT_AND_EXPR || code == TRUTH_AND_EXPR)
    is_and = true;
  else if (code != BIT_IOR_EXPR && code != TRUTH_OR_EXPR)
    return;

  gimple_range_op_handler handler (s);
  tree lhs = handler.lhs ();
  tree ssa1 = gimple_range_ssa_p (handler.operand1 ());
  tree ssa2 = gimple_range_ssa_p (handler.operand2 ());

  // Deal with || and && only when there is a full set of symbolics.
  if (!lhs || !ssa1 || !ssa2
      || (TREE_CODE (TREE_TYPE (lhs)) != BOOLEAN_TYPE)
      || (TREE_CODE (TREE_TYPE (ssa1)) != BOOLEAN_TYPE)
      || (TREE_CODE (TREE_TYPE (ssa2)) != BOOLEAN_TYPE))
    return;

  // Now we know its a boolean AND or OR expression with boolean operands.
  // Ideally we search dependencies for common names, and see what pops out.
  // until then, simply try to resolve direct dependencies.

  gimple *ssa1_stmt = SSA_NAME_DEF_STMT (ssa1);
  gimple *ssa2_stmt = SSA_NAME_DEF_STMT (ssa2);

  gimple_range_op_handler handler1 (ssa1_stmt);
  gimple_range_op_handler handler2 (ssa2_stmt);

  // If either handler is not present, no relation can be found.
  if (!handler1 || !handler2)
    return;

  // Both stmts will need to have 2 ssa names in the stmt.
  tree ssa1_dep1 = gimple_range_ssa_p (handler1.operand1 ());
  tree ssa1_dep2 = gimple_range_ssa_p (handler1.operand2 ());
  tree ssa2_dep1 = gimple_range_ssa_p (handler2.operand1 ());
  tree ssa2_dep2 = gimple_range_ssa_p (handler2.operand2 ());

  if (!ssa1_dep1 || !ssa1_dep2 || !ssa2_dep1 || !ssa2_dep2)
    return;

  if (HONOR_NANS (TREE_TYPE (ssa1_dep1)))
    return;

  // Make sure they are the same dependencies, and detect the order of the
  // relationship.
  bool reverse_op2 = true;
  if (ssa1_dep1 == ssa2_dep1 && ssa1_dep2 == ssa2_dep2)
    reverse_op2 = false;
  else if (ssa1_dep1 != ssa2_dep2 || ssa1_dep2 != ssa2_dep1)
    return;

  int_range<2> bool_one = range_true ();
  relation_kind relation1 = handler1.op1_op2_relation (bool_one, op1, op2);
  relation_kind relation2 = handler2.op1_op2_relation (bool_one, op1, op2);
  if (relation1 == VREL_VARYING || relation2 == VREL_VARYING)
    return;

  if (reverse_op2)
    relation2 = relation_negate (relation2);

  // x && y is false if the relation intersection of the true cases is NULL.
  if (is_and && relation_intersect (relation1, relation2) == VREL_UNDEFINED)
    lhs_range = range_false (boolean_type_node);
  // x || y is true if the union of the true cases is NO-RELATION..
  // ie, one or the other being true covers the full range of possibilities.
  else if (!is_and && relation_union (relation1, relation2) == VREL_VARYING)
    lhs_range = bool_one;
  else
    return;

  range_cast (lhs_range, TREE_TYPE (lhs));
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  Relation adjustment: ");
      print_generic_expr (dump_file, ssa1, TDF_SLIM);
      fprintf (dump_file, "  and ");
      print_generic_expr (dump_file, ssa2, TDF_SLIM);
      fprintf (dump_file, "  combine to produce ");
      lhs_range.dump (dump_file);
      fputc ('\n', dump_file);
    }

  return;
}

// Register any outgoing edge relations from a conditional branch.

void
fur_source::register_outgoing_edges (gcond *s, irange &lhs_range,
				     edge e0, edge e1)
{
  int_range<2> e0_range, e1_range;
  tree name;
  basic_block bb = gimple_bb (s);

  gimple_range_op_handler handler (s);
  if (!handler)
    return;

  if (e0)
    {
      // If this edge is never taken, ignore it.
      gcond_edge_range (e0_range, e0);
      e0_range.intersect (lhs_range);
      if (e0_range.undefined_p ())
	e0 = NULL;
    }

  if (e1)
    {
      // If this edge is never taken, ignore it.
      gcond_edge_range (e1_range, e1);
      e1_range.intersect (lhs_range);
      if (e1_range.undefined_p ())
	e1 = NULL;
    }

  if (!e0 && !e1)
    return;

  // First, register the gcond itself.  This will catch statements like
  // if (a_2 < b_5)
  tree ssa1 = gimple_range_ssa_p (handler.operand1 ());
  tree ssa2 = gimple_range_ssa_p (handler.operand2 ());
  Value_Range r1,r2;
  if (ssa1 && ssa2)
    {
      r1.set_varying (TREE_TYPE (ssa1));
      r2.set_varying (TREE_TYPE (ssa2));
      if (e0)
	{
	  relation_kind relation = handler.op1_op2_relation (e0_range, r1, r2);
	  if (relation != VREL_VARYING)
	    register_relation (e0, relation, ssa1, ssa2);
	}
      if (e1)
	{
	  relation_kind relation = handler.op1_op2_relation (e1_range, r1, r2);
	  if (relation != VREL_VARYING)
	    register_relation (e1, relation, ssa1, ssa2);
	}
    }

  // Outgoing relations of GORI exports require a gori engine.
  if (!gori_ssa ())
    return;

  // Now look for other relations in the exports.  This will find stmts
  // leading to the condition such as:
  // c_2 = a_4 < b_7
  // if (c_2)
  FOR_EACH_GORI_EXPORT_NAME (gori_ssa (), bb, name)
    {
      if (TREE_CODE (TREE_TYPE (name)) != BOOLEAN_TYPE)
	continue;
      gimple *stmt = SSA_NAME_DEF_STMT (name);
      gimple_range_op_handler handler (stmt);
      if (!handler)
	continue;
      tree ssa1 = gimple_range_ssa_p (handler.operand1 ());
      tree ssa2 = gimple_range_ssa_p (handler.operand2 ());
      Value_Range r (TREE_TYPE (name));
      if (ssa1 && ssa2)
	{
	  r1.set_varying (TREE_TYPE (ssa1));
	  r2.set_varying (TREE_TYPE (ssa2));
	  if (e0 && gori ()->edge_range_p (r, e0, name, *m_query)
	      && r.singleton_p ())
	    {
	      relation_kind relation = handler.op1_op2_relation (r, r1, r2);
	      if (relation != VREL_VARYING)
		register_relation (e0, relation, ssa1, ssa2);
	    }
	  if (e1 && gori ()->edge_range_p (r, e1, name, *m_query)
	      && r.singleton_p ())
	    {
	      relation_kind relation = handler.op1_op2_relation (r, r1, r2);
	      if (relation != VREL_VARYING)
		register_relation (e1, relation, ssa1, ssa2);
	    }
	}
    }
}
