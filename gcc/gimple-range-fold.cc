/* Code for GIMPLE range related routines.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
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
#include "range-op.h"
#include "gimple-range.h"
// Construct a fur_source, and set the m_query field.

fur_source::fur_source (range_query *q)
{
  if (q)
    m_query = q;
  else if (cfun)
    m_query = get_range_query (cfun);
  else
    m_query = get_global_range_query ();
  m_gori = NULL;
}

// Invoke range_of_expr on EXPR.

bool
fur_source::get_operand (irange &r, tree expr)
{
  return m_query->range_of_expr (r, expr);
}

// Evaluate EXPR for this stmt as a PHI argument on edge E.  Use the current
// range_query to get the range on the edge.

bool
fur_source::get_phi_operand (irange &r, tree expr, edge e)
{
  return m_query->range_on_edge (r, e, expr);
}

// Default is no relation.

relation_kind
fur_source::query_relation (tree op1 ATTRIBUTE_UNUSED,
			    tree op2 ATTRIBUTE_UNUSED)
{
  return VREL_NONE;
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
  virtual bool get_operand (irange &r, tree expr) OVERRIDE;
  virtual bool get_phi_operand (irange &r, tree expr, edge e) OVERRIDE;
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
fur_edge::get_operand (irange &r, tree expr)
{
  return m_query->range_on_edge (r, m_edge, expr);
}

// Evaluate EXPR for this stmt as a PHI argument on edge E.  Use the current
// range_query to get the range on the edge.

bool
fur_edge::get_phi_operand (irange &r, tree expr, edge e)
{
  // Edge to edge recalculations not supoprted yet, until we sort it out.
  gcc_checking_assert (e == m_edge);
  return m_query->range_on_edge (r, e, expr);
}

// Instantiate a stmt based fur_source.

fur_stmt::fur_stmt (gimple *s, range_query *q) : fur_source (q)
{
  m_stmt = s;
}

// Retreive range of EXPR as it occurs as a use on stmt M_STMT.

bool
fur_stmt::get_operand (irange &r, tree expr)
{
  return m_query->range_of_expr (r, expr, m_stmt);
}

// Evaluate EXPR for this stmt as a PHI argument on edge E.  Use the current
// range_query to get the range on the edge.

bool
fur_stmt::get_phi_operand (irange &r, tree expr, edge e)
{
  // Pick up the range of expr from edge E.
  fur_edge e_src (e, m_query);
  return e_src.get_operand (r, expr);
}

// Return relation based from m_stmt.

relation_kind
fur_stmt::query_relation (tree op1, tree op2)
{
  return m_query->query_relation (m_stmt, op1, op2);
}

// Instantiate a stmt based fur_source with a GORI object.


fur_depend::fur_depend (gimple *s, gori_compute *gori, range_query *q)
  : fur_stmt (s, q)
{
  gcc_checking_assert (gori);
  m_gori = gori;
  // Set relations if there is an oracle in the range_query.
  // This will enable registering of relationships as they are discovered.
  m_oracle = q->oracle ();

}

// Register a relation on a stmt if there is an oracle.

void
fur_depend::register_relation (gimple *s, relation_kind k, tree op1, tree op2)
{
  if (m_oracle)
    m_oracle->register_stmt (s, k, op1, op2);
}

// Register a relation on an edge if there is an oracle.

void
fur_depend::register_relation (edge e, relation_kind k, tree op1, tree op2)
{
  if (m_oracle)
    m_oracle->register_edge (e, k, op1, op2);
}

// This version of fur_source will pick a range up from a list of ranges
// supplied by the caller.

class fur_list : public fur_source
{
public:
  fur_list (irange &r1);
  fur_list (irange &r1, irange &r2);
  fur_list (unsigned num, irange *list);
  virtual bool get_operand (irange &r, tree expr) OVERRIDE;
  virtual bool get_phi_operand (irange &r, tree expr, edge e) OVERRIDE;
private:
  int_range_max m_local[2];
  irange *m_list;
  unsigned m_index;
  unsigned m_limit;
};

// One range supplied for unary operations.

fur_list::fur_list (irange &r1) : fur_source (NULL)
{
  m_list = m_local;
  m_index = 0;
  m_limit = 1;
  m_local[0] = r1;
}

// Two ranges supplied for binary operations.

fur_list::fur_list (irange &r1, irange &r2) : fur_source (NULL)
{
  m_list = m_local;
  m_index = 0;
  m_limit = 2;
  m_local[0] = r1;
  m_local[0] = r2;
}

// Arbitrary number of ranges in a vector.

fur_list::fur_list (unsigned num, irange *list) : fur_source (NULL)
{
  m_list = list;
  m_index = 0;
  m_limit = num;
}

// Get the next operand from the vector, ensure types are compatible.

bool
fur_list::get_operand (irange &r, tree expr)
{
  if (m_index >= m_limit)
    return m_query->range_of_expr (r, expr);
  r = m_list[m_index++];
  gcc_checking_assert (range_compatible_p (TREE_TYPE (expr), r.type ()));
  return true;
}

// This will simply pick the next operand from the vector.
bool
fur_list::get_phi_operand (irange &r, tree expr, edge e ATTRIBUTE_UNUSED)
{
  return get_operand (r, expr);
}

// Fold stmt S into range R using R1 as the first operand.

bool
fold_range (irange &r, gimple *s, irange &r1)
{
  fold_using_range f;
  fur_list src (r1);
  return f.fold_stmt (r, s, src);
}

// Fold stmt S into range R using R1  and R2 as the first two operands.

bool
fold_range (irange &r, gimple *s, irange &r1, irange &r2)
{
  fold_using_range f;
  fur_list src (r1, r2);
  return f.fold_stmt (r, s, src);
}

// Fold stmt S into range R using NUM_ELEMENTS from VECTOR as the initial
// operands encountered.

bool
fold_range (irange &r, gimple *s, unsigned num_elements, irange *vector)
{
  fold_using_range f;
  fur_list src (num_elements, vector);
  return f.fold_stmt (r, s, src);
}

// Fold stmt S into range R using range query Q.

bool
fold_range (irange &r, gimple *s, range_query *q)
{
  fold_using_range f;
  fur_stmt src (s, q);
  return f.fold_stmt (r, s, src);
}

// Recalculate stmt S into R using range query Q as if it were on edge ON_EDGE.

bool
fold_range (irange &r, gimple *s, edge on_edge, range_query *q)
{
  fold_using_range f;
  fur_edge src (on_edge, q);
  return f.fold_stmt (r, s, src);
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
      tree max = vrp_val_max (ptrdiff_type_node);
      unsigned prec = TYPE_PRECISION (TREE_TYPE (max));
      wide_int wmaxm1 = wi::to_wide (max, prec) - 1;
      res.intersect (wi::zero (prec), wmaxm1);
    }
}

// Adjust the range for an IMAGPART_EXPR.

static void
adjust_imagpart_expr (irange &res, const gimple *stmt)
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
  if (is_gimple_assign (def_stmt))
    {
      tree cst = gimple_assign_rhs1 (def_stmt);
      if (TREE_CODE (cst) == COMPLEX_CST)
	{
	  wide_int imag = wi::to_wide (TREE_IMAGPART (cst));
	  res.intersect (imag, imag);
	}
    }
}

// Adjust the range for a REALPART_EXPR.

static void
adjust_realpart_expr (irange &res, const gimple *stmt)
{
  tree name = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);

  if (TREE_CODE (name) != SSA_NAME)
    return;

  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  if (!SSA_NAME_DEF_STMT (name))
    return;

  if (is_gimple_assign (def_stmt))
    {
      tree cst = gimple_assign_rhs1 (def_stmt);
      if (TREE_CODE (cst) == COMPLEX_CST)
	{
	  tree imag = TREE_REALPART (cst);
	  int_range<2> tmp (imag, imag);
	  res.intersect (tmp);
	}
    }
}

// This function looks for situations when walking the use/def chains
// may provide additonal contextual range information not exposed on
// this statement.

static void
gimple_range_adjustment (irange &res, const gimple *stmt)
{
  switch (gimple_expr_code (stmt))
    {
    case POINTER_DIFF_EXPR:
      adjust_pointer_diff_expr (res, stmt);
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

// Return the base of the RHS of an assignment.

static tree
gimple_range_base_of_assignment (const gimple *stmt)
{
  gcc_checking_assert (gimple_code (stmt) == GIMPLE_ASSIGN);
  tree op1 = gimple_assign_rhs1 (stmt);
  if (gimple_assign_rhs_code (stmt) == ADDR_EXPR)
    return get_base_address (TREE_OPERAND (op1, 0));
  return op1;
}

// Return the first operand of this statement if it is a valid operand
// supported by ranges, otherwise return NULL_TREE.  Special case is
// &(SSA_NAME expr), return the SSA_NAME instead of the ADDR expr.

tree
gimple_range_operand1 (const gimple *stmt)
{
  gcc_checking_assert (gimple_range_handler (stmt));

  switch (gimple_code (stmt))
    {
      case GIMPLE_COND:
	return gimple_cond_lhs (stmt);
      case GIMPLE_ASSIGN:
	{
	  tree base = gimple_range_base_of_assignment (stmt);
	  if (base && TREE_CODE (base) == MEM_REF)
	    {
	      // If the base address is an SSA_NAME, we return it
	      // here.  This allows processing of the range of that
	      // name, while the rest of the expression is simply
	      // ignored.  The code in range_ops will see the
	      // ADDR_EXPR and do the right thing.
	      tree ssa = TREE_OPERAND (base, 0);
	      if (TREE_CODE (ssa) == SSA_NAME)
		return ssa;
	    }
	  return base;
	}
      default:
	break;
    }
  return NULL;
}

// Return the second operand of statement STMT, otherwise return NULL_TREE.

tree
gimple_range_operand2 (const gimple *stmt)
{
  gcc_checking_assert (gimple_range_handler (stmt));

  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      return gimple_cond_rhs (stmt);
    case GIMPLE_ASSIGN:
      if (gimple_num_ops (stmt) >= 3)
	return gimple_assign_rhs2 (stmt);
    default:
      break;
    }
  return NULL_TREE;
}

// Calculate a range for statement S and return it in R. If NAME is provided it
// represents the SSA_NAME on the LHS of the statement. It is only required
// if there is more than one lhs/output.  If a range cannot
// be calculated, return false.

bool
fold_using_range::fold_stmt (irange &r, gimple *s, fur_source &src, tree name)
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
    return range_of_address (r, s, src);

  if (gimple_range_handler (s))
    res = range_of_range_op (r, s, src);
  else if (is_a<gphi *>(s))
    res = range_of_phi (r, as_a<gphi *> (s), src);
  else if (is_a<gcall *>(s))
    res = range_of_call (r, as_a<gcall *> (s), src);
  else if (is_a<gassign *> (s) && gimple_assign_rhs_code (s) == COND_EXPR)
    res = range_of_cond_expr (r, as_a<gassign *> (s), src);

  if (!res)
    {
      // If no name specified or range is unsupported, bail.
      if (!name || !gimple_range_ssa_p (name))
	return false;
      // We don't understand the stmt, so return the global range.
      r = gimple_range_global (name);
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
fold_using_range::range_of_range_op (irange &r, gimple *s, fur_source &src)
{
  int_range_max range1, range2;
  tree type = gimple_range_type (s);
  if (!type)
    return false;
  range_operator *handler = gimple_range_handler (s);
  gcc_checking_assert (handler);

  tree lhs = gimple_get_lhs (s);
  tree op1 = gimple_range_operand1 (s);
  tree op2 = gimple_range_operand2 (s);

  if (src.get_operand (range1, op1))
    {
      if (!op2)
	{
	  // Fold range, and register any dependency if available.
	  int_range<2> r2 (type);
	  handler->fold_range (r, type, range1, r2);
	  if (lhs && gimple_range_ssa_p (op1))
	    {
	      if (src.gori ())
		src.gori ()->register_dependency (lhs, op1);
	      relation_kind rel;
	      rel = handler->lhs_op1_relation (r, range1, range1);
	      if (rel != VREL_NONE)
		src.register_relation (s, rel, lhs, op1);
	    }
	}
      else if (src.get_operand (range2, op2))
	{
	  relation_kind rel = src.query_relation (op1, op2);
	  if (dump_file && (dump_flags & TDF_DETAILS) && rel != VREL_NONE)
	    {
	      fprintf (dump_file, " folding with relation ");
	      print_relation (dump_file, rel);
	      fputc ('\n', dump_file);
	    }
	  // Fold range, and register any dependency if available.
	  handler->fold_range (r, type, range1, range2, rel);
	  relation_fold_and_or (r, s, src);
	  if (lhs)
	    {
	      if (src.gori ())
		{
		  src.gori ()->register_dependency (lhs, op1);
		  src.gori ()->register_dependency (lhs, op2);
		}
	      if (gimple_range_ssa_p (op1))
		{
		  rel = handler->lhs_op1_relation (r, range1, range2);
		  if (rel != VREL_NONE)
		    src.register_relation (s, rel, lhs, op1);
		}
	      if (gimple_range_ssa_p (op2))
		{
		  rel= handler->lhs_op2_relation (r, range1, range2);
		  if (rel != VREL_NONE)
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
	      src.register_outgoing_edges (as_a<gcond *> (s), r, e0, e1);
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
fold_using_range::range_of_address (irange &r, gimple *stmt, fur_source &src)
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
      if (lhs && gimple_range_ssa_p (ssa) && src.gori ())
	src.gori ()->register_dependency (lhs, ssa);
      gcc_checking_assert (irange::supports_type_p (TREE_TYPE (ssa)));
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
	   if(!range_includes_zero_p (&r))
	    return true;
	}
      /* If MEM_REF has a "positive" offset, consider it non-NULL
	 always, for -fdelete-null-pointer-checks also "negative"
	 ones.  Punt for unknown offsets (e.g. variable ones).  */
      if (!TYPE_OVERFLOW_WRAPS (TREE_TYPE (expr))
	  && off_cst
	  && known_ne (off, 0)
	  && (flag_delete_null_pointer_checks || known_gt (off, 0)))
	{
	  r = range_nonzero (TREE_TYPE (gimple_assign_rhs1 (stmt)));
	  return true;
	}
      r = int_range<2> (TREE_TYPE (gimple_assign_rhs1 (stmt)));
      return true;
    }

  // Handle "= &a".
  if (tree_single_nonzero_warnv_p (expr, &strict_overflow_p))
    {
      r = range_nonzero (TREE_TYPE (gimple_assign_rhs1 (stmt)));
      return true;
    }

  // Otherwise return varying.
  r = int_range<2> (TREE_TYPE (gimple_assign_rhs1 (stmt)));
  return true;
}

// Calculate a range for phi statement S and return it in R.
// If a range cannot be calculated, return false.

bool
fold_using_range::range_of_phi (irange &r, gphi *phi, fur_source &src)
{
  tree phi_def = gimple_phi_result (phi);
  tree type = gimple_range_type (phi);
  int_range_max arg_range;
  unsigned x;

  if (!type)
    return false;

  // Track if all executable arguments are the same.
  tree single_arg = NULL_TREE;
  bool seen_arg = false;

  // Start with an empty range, unioning in each argument's range.
  r.set_undefined ();
  for (x = 0; x < gimple_phi_num_args (phi); x++)
    {
      tree arg = gimple_phi_arg_def (phi, x);
      edge e = gimple_phi_arg_edge (phi, x);

      // Get the range of the argument on its edge.
      src.get_phi_operand (arg_range, arg, e);

      if (!arg_range.undefined_p ())
	{
	  // Register potential dependencies for stale value tracking.
	  r.union_ (arg_range);
	  if (gimple_range_ssa_p (arg) && src.gori ())
	    src.gori ()->register_dependency (phi_def, arg);

	  // Track if all arguments are the same.
	  if (!seen_arg)
	    {
	      seen_arg = true;
	      single_arg = arg;
	    }
	  else if (single_arg != arg)
	    single_arg = NULL_TREE;
	}

      // Once the value reaches varying, stop looking.
      if (r.varying_p () && single_arg == NULL_TREE)
	break;
    }

    // If the PHI boils down to a single effective argument, look at it.
    if (single_arg)
      {
	// Symbolic arguments are equivalences.
	if (gimple_range_ssa_p (single_arg))
	  src.register_relation (phi, EQ_EXPR, phi_def, single_arg);
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

  // If SCEV is available, query if this PHI has any knonwn values.
  if (scev_initialized_p () && !POINTER_TYPE_P (TREE_TYPE (phi_def)))
    {
      value_range loop_range;
      class loop *l = loop_containing_stmt (phi);
      if (l && loop_outer (l))
	{
	  range_of_ssa_name_with_loop_info (loop_range, phi_def, l, phi, src);
	  if (!loop_range.varying_p ())
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "   Loops range found for ");
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
fold_using_range::range_of_call (irange &r, gcall *call, fur_source &src)
{
  tree type = gimple_range_type (call);
  if (!type)
    return false;

  tree lhs = gimple_call_lhs (call);
  bool strict_overflow_p;

  if (range_of_builtin_call (r, call, src))
    ;
  else if (gimple_stmt_nonnegative_warnv_p (call, &strict_overflow_p))
    r.set (build_int_cst (type, 0), TYPE_MAX_VALUE (type));
  else if (gimple_call_nonnull_result_p (call)
	   || gimple_call_nonnull_arg (call))
    r = range_nonzero (type);
  else
    r.set_varying (type);

  // If there is an LHS, intersect that with what is known.
  if (lhs)
    {
      value_range def;
      def = gimple_range_global (lhs);
      r.intersect (def);
    }
  return true;
}

// Return the range of a __builtin_ubsan* in CALL and set it in R.
// CODE is the type of ubsan call (PLUS_EXPR, MINUS_EXPR or
// MULT_EXPR).

void
fold_using_range::range_of_builtin_ubsan_call (irange &r, gcall *call,
					       tree_code code, fur_source &src)
{
  gcc_checking_assert (code == PLUS_EXPR || code == MINUS_EXPR
		       || code == MULT_EXPR);
  tree type = gimple_range_type (call);
  range_operator *op = range_op_handler (code, type);
  gcc_checking_assert (op);
  int_range_max ir0, ir1;
  tree arg0 = gimple_call_arg (call, 0);
  tree arg1 = gimple_call_arg (call, 1);
  src.get_operand (ir0, arg0);
  src.get_operand (ir1, arg1);
  // Check for any relation between arg0 and arg1.
  relation_kind relation = src.query_relation (arg0, arg1);

  bool saved_flag_wrapv = flag_wrapv;
  // Pretend the arithmetic is wrapping.  If there is any overflow,
  // we'll complain, but will actually do wrapping operation.
  flag_wrapv = 1;
  op->fold_range (r, type, ir0, ir1, relation);
  flag_wrapv = saved_flag_wrapv;

  // If for both arguments vrp_valueize returned non-NULL, this should
  // have been already folded and if not, it wasn't folded because of
  // overflow.  Avoid removing the UBSAN_CHECK_* calls in that case.
  if (r.singleton_p ())
    r.set_varying (type);
}

// Return TRUE if we recognize the target character set and return the
// range for lower case and upper case letters.

static bool
get_letter_range (tree type, irange &lowers, irange &uppers)
{
  // ASCII
  int a = lang_hooks.to_target_charset ('a');
  int z = lang_hooks.to_target_charset ('z');
  int A = lang_hooks.to_target_charset ('A');
  int Z = lang_hooks.to_target_charset ('Z');

  if ((z - a == 25) && (Z - A == 25))
    {
      lowers = int_range<2> (build_int_cst (type, a), build_int_cst (type, z));
      uppers = int_range<2> (build_int_cst (type, A), build_int_cst (type, Z));
      return true;
    }
  // Unknown character set.
  return false;
}

// For a builtin in CALL, return a range in R if known and return
// TRUE.  Otherwise return FALSE.

bool
fold_using_range::range_of_builtin_call (irange &r, gcall *call,
					 fur_source &src)
{
  combined_fn func = gimple_call_combined_fn (call);
  if (func == CFN_LAST)
    return false;

  tree type = gimple_range_type (call);
  tree arg;
  int mini, maxi, zerov = 0, prec;
  scalar_int_mode mode;

  switch (func)
    {
    case CFN_BUILT_IN_CONSTANT_P:
      if (cfun->after_inlining)
	{
	  r.set_zero (type);
	  // r.equiv_clear ();
	  return true;
	}
      arg = gimple_call_arg (call, 0);
      if (src.get_operand (r, arg) && r.singleton_p ())
	{
	  r.set (build_one_cst (type), build_one_cst (type));
	  return true;
	}
      break;

    case CFN_BUILT_IN_TOUPPER:
      {
	arg = gimple_call_arg (call, 0);
	// If the argument isn't compatible with the LHS, do nothing.
	if (!range_compatible_p (type, TREE_TYPE (arg)))
	  return false;
	if (!src.get_operand (r, arg))
	  return false;

	int_range<3> lowers;
	int_range<3> uppers;
	if (!get_letter_range (type, lowers, uppers))
	  return false;

	// Return the range passed in without any lower case characters,
	// but including all the upper case ones.
	lowers.invert ();
	r.intersect (lowers);
	r.union_ (uppers);
	return true;
      }

     case CFN_BUILT_IN_TOLOWER:
      {
	arg = gimple_call_arg (call, 0);
	// If the argument isn't compatible with the LHS, do nothing.
	if (!range_compatible_p (type, TREE_TYPE (arg)))
	  return false;
	if (!src.get_operand (r, arg))
	  return false;

	int_range<3> lowers;
	int_range<3> uppers;
	if (!get_letter_range (type, lowers, uppers))
	  return false;

	// Return the range passed in without any upper case characters,
	// but including all the lower case ones.
	uppers.invert ();
	r.intersect (uppers);
	r.union_ (lowers);
	return true;
      }

    CASE_CFN_FFS:
    CASE_CFN_POPCOUNT:
      // __builtin_ffs* and __builtin_popcount* return [0, prec].
      arg = gimple_call_arg (call, 0);
      prec = TYPE_PRECISION (TREE_TYPE (arg));
      mini = 0;
      maxi = prec;
      src.get_operand (r, arg);
      // If arg is non-zero, then ffs or popcount are non-zero.
      if (!range_includes_zero_p (&r))
	mini = 1;
      // If some high bits are known to be zero, decrease the maximum.
      if (!r.undefined_p ())
	{
	  if (TYPE_SIGN (r.type ()) == SIGNED)
	    range_cast (r, unsigned_type_for (r.type ()));
	  wide_int max = r.upper_bound ();
	  maxi = wi::floor_log2 (max) + 1;
	}
      r.set (build_int_cst (type, mini), build_int_cst (type, maxi));
      return true;

    CASE_CFN_PARITY:
      r.set (build_zero_cst (type), build_one_cst (type));
      return true;

    CASE_CFN_CLZ:
      // __builtin_c[lt]z* return [0, prec-1], except when the
      // argument is 0, but that is undefined behavior.
      //
      // For __builtin_c[lt]z* consider argument of 0 always undefined
      // behavior, for internal fns depending on C?Z_DEFINED_VALUE_AT_ZERO.
      arg = gimple_call_arg (call, 0);
      prec = TYPE_PRECISION (TREE_TYPE (arg));
      mini = 0;
      maxi = prec - 1;
      mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (arg));
      if (gimple_call_internal_p (call))
	{
	  if (optab_handler (clz_optab, mode) != CODE_FOR_nothing
	      && CLZ_DEFINED_VALUE_AT_ZERO (mode, zerov) == 2)
	    {
	      // Only handle the single common value.
	      if (zerov == prec)
		maxi = prec;
	      else
		// Magic value to give up, unless we can prove arg is non-zero.
		mini = -2;
	    }
	}

      src.get_operand (r, arg);
      // From clz of minimum we can compute result maximum.
      if (!r.undefined_p ())
	{
	  // From clz of minimum we can compute result maximum.
	  if (wi::gt_p (r.lower_bound (), 0, TYPE_SIGN (r.type ())))
	    {
	      maxi = prec - 1 - wi::floor_log2 (r.lower_bound ());
	      if (mini == -2)
		mini = 0;
	    }
	  else if (!range_includes_zero_p (&r))
	    {
	      mini = 0;
	      maxi = prec - 1;
	    }
	  if (mini == -2)
	    break;
	  // From clz of maximum we can compute result minimum.
	  wide_int max = r.upper_bound ();
	  int newmini = prec - 1 - wi::floor_log2 (max);
	  if (max == 0)
	    {
	      // If CLZ_DEFINED_VALUE_AT_ZERO is 2 with VALUE of prec,
	      // return [prec, prec], otherwise ignore the range.
	      if (maxi == prec)
		mini = prec;
	    }
	  else
	    mini = newmini;
	}
      if (mini == -2)
	break;
      r.set (build_int_cst (type, mini), build_int_cst (type, maxi));
      return true;

    CASE_CFN_CTZ:
      // __builtin_ctz* return [0, prec-1], except for when the
      // argument is 0, but that is undefined behavior.
      //
      // For __builtin_ctz* consider argument of 0 always undefined
      // behavior, for internal fns depending on CTZ_DEFINED_VALUE_AT_ZERO.
      arg = gimple_call_arg (call, 0);
      prec = TYPE_PRECISION (TREE_TYPE (arg));
      mini = 0;
      maxi = prec - 1;
      mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (arg));
      if (gimple_call_internal_p (call))
	{
	  if (optab_handler (ctz_optab, mode) != CODE_FOR_nothing
	      && CTZ_DEFINED_VALUE_AT_ZERO (mode, zerov) == 2)
	    {
	      // Handle only the two common values.
	      if (zerov == -1)
		mini = -1;
	      else if (zerov == prec)
		maxi = prec;
	      else
		// Magic value to give up, unless we can prove arg is non-zero.
		mini = -2;
	    }
	}
      src.get_operand (r, arg);
      if (!r.undefined_p ())
	{
	  // If arg is non-zero, then use [0, prec - 1].
	  if (!range_includes_zero_p (&r))
	    {
	      mini = 0;
	      maxi = prec - 1;
	    }
	  // If some high bits are known to be zero, we can decrease
	  // the maximum.
	  wide_int max = r.upper_bound ();
	  if (max == 0)
	    {
	      // Argument is [0, 0].  If CTZ_DEFINED_VALUE_AT_ZERO
	      // is 2 with value -1 or prec, return [-1, -1] or [prec, prec].
	      // Otherwise ignore the range.
	      if (mini == -1)
		maxi = -1;
	      else if (maxi == prec)
		mini = prec;
	    }
	  // If value at zero is prec and 0 is in the range, we can't lower
	  // the upper bound.  We could create two separate ranges though,
	  // [0,floor_log2(max)][prec,prec] though.
	  else if (maxi != prec)
	    maxi = wi::floor_log2 (max);
	}
      if (mini == -2)
	break;
      r.set (build_int_cst (type, mini), build_int_cst (type, maxi));
      return true;

    CASE_CFN_CLRSB:
      arg = gimple_call_arg (call, 0);
      prec = TYPE_PRECISION (TREE_TYPE (arg));
      r.set (build_int_cst (type, 0), build_int_cst (type, prec - 1));
      return true;
    case CFN_UBSAN_CHECK_ADD:
      range_of_builtin_ubsan_call (r, call, PLUS_EXPR, src);
      return true;
    case CFN_UBSAN_CHECK_SUB:
      range_of_builtin_ubsan_call (r, call, MINUS_EXPR, src);
      return true;
    case CFN_UBSAN_CHECK_MUL:
      range_of_builtin_ubsan_call (r, call, MULT_EXPR, src);
      return true;

    case CFN_GOACC_DIM_SIZE:
    case CFN_GOACC_DIM_POS:
      // Optimizing these two internal functions helps the loop
      // optimizer eliminate outer comparisons.  Size is [1,N]
      // and pos is [0,N-1].
      {
	bool is_pos = func == CFN_GOACC_DIM_POS;
	int axis = oacc_get_ifn_dim_arg (call);
	int size = oacc_get_fn_dim_size (current_function_decl, axis);
	if (!size)
	  // If it's dynamic, the backend might know a hardware limitation.
	  size = targetm.goacc.dim_limit (axis);

	r.set (build_int_cst (type, is_pos ? 0 : 1),
	       size
	       ? build_int_cst (type, size - is_pos) : vrp_val_max (type));
	return true;
      }

    case CFN_BUILT_IN_STRLEN:
      if (tree lhs = gimple_call_lhs (call))
	if (ptrdiff_type_node
	    && (TYPE_PRECISION (ptrdiff_type_node)
		== TYPE_PRECISION (TREE_TYPE (lhs))))
	  {
	    tree type = TREE_TYPE (lhs);
	    tree max = vrp_val_max (ptrdiff_type_node);
	    wide_int wmax
	      = wi::to_wide (max, TYPE_PRECISION (TREE_TYPE (max)));
	    tree range_min = build_zero_cst (type);
	    // To account for the terminating NULL, the maximum length
	    // is one less than the maximum array size, which in turn
	    // is one less than PTRDIFF_MAX (or SIZE_MAX where it's
	    // smaller than the former type).
	    // FIXME: Use max_object_size() - 1 here.
	    tree range_max = wide_int_to_tree (type, wmax - 2);
	    r.set (range_min, range_max);
	    return true;
	  }
      break;
    default:
      break;
    }
  return false;
}


// Calculate a range for COND_EXPR statement S and return it in R.
// If a range cannot be calculated, return false.

bool
fold_using_range::range_of_cond_expr  (irange &r, gassign *s, fur_source &src)
{
  int_range_max cond_range, range1, range2;
  tree cond = gimple_assign_rhs1 (s);
  tree op1 = gimple_assign_rhs2 (s);
  tree op2 = gimple_assign_rhs3 (s);

  tree type = gimple_range_type (s);
  if (!type)
    return false;

  gcc_checking_assert (gimple_assign_rhs_code (s) == COND_EXPR);
  gcc_checking_assert (range_compatible_p (TREE_TYPE (op1), TREE_TYPE (op2)));
  src.get_operand (cond_range, cond);
  src.get_operand (range1, op1);
  src.get_operand (range2, op2);

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
fold_using_range::range_of_ssa_name_with_loop_info (irange &r, tree name,
						    class loop *l, gphi *phi,
						    fur_source &src)
{
  gcc_checking_assert (TREE_CODE (name) == SSA_NAME);
  tree min, max, type = TREE_TYPE (name);
  if (bounds_of_var_in_loop (&min, &max, src.query (), l, phi, name))
    {
      if (TREE_CODE (min) != INTEGER_CST)
	{
	  if (src.query ()->range_of_expr (r, min, phi) && !r.undefined_p ())
	    min = wide_int_to_tree (type, r.lower_bound ());
	  else
	    min = vrp_val_min (type);
	}
      if (TREE_CODE (max) != INTEGER_CST)
	{
	  if (src.query ()->range_of_expr (r, max, phi) && !r.undefined_p ())
	    max = wide_int_to_tree (type, r.upper_bound ());
	  else
	    max = vrp_val_max (type);
	}
      r.set (min, max);
    }
  else
    r.set_varying (type);
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
					fur_source &src)
{
  // No queries or already folded.
  if (!src.gori () || !src.query ()->oracle () || lhs_range.singleton_p ())
    return;

  // Only care about AND and OR expressions.
  enum tree_code code = gimple_expr_code (s);
  bool is_and = false;
  if (code == BIT_AND_EXPR || code == TRUTH_AND_EXPR)
    is_and = true;
  else if (code != BIT_IOR_EXPR && code != TRUTH_OR_EXPR)
    return;

  tree lhs = gimple_get_lhs (s);
  tree ssa1 = gimple_range_ssa_p (gimple_range_operand1 (s));
  tree ssa2 = gimple_range_ssa_p (gimple_range_operand2 (s));

  // Deal with || and && only when there is a full set of symbolics.
  if (!lhs || !ssa1 || !ssa2
      || (TREE_CODE (TREE_TYPE (lhs)) != BOOLEAN_TYPE)
      || (TREE_CODE (TREE_TYPE (ssa1)) != BOOLEAN_TYPE)
      || (TREE_CODE (TREE_TYPE (ssa2)) != BOOLEAN_TYPE))
    return;

  // Now we know its a boolean AND or OR expression with boolean operands.
  // Ideally we search dependencies for common names, and see what pops out.
  // until then, simply try to resolve direct dependencies.

  // Both names will need to have 2 direct dependencies.
  tree ssa1_dep2 = src.gori ()->depend2 (ssa1);
  tree ssa2_dep2 = src.gori ()->depend2 (ssa2);
  if (!ssa1_dep2 || !ssa2_dep2)
    return;

  tree ssa1_dep1 = src.gori ()->depend1 (ssa1);
  tree ssa2_dep1 = src.gori ()->depend1 (ssa2);
  // Make sure they are the same dependencies, and detect the order of the
  // relationship.
  bool reverse_op2 = true;
  if (ssa1_dep1 == ssa2_dep1 && ssa1_dep2 == ssa2_dep2)
    reverse_op2 = false;
  else if (ssa1_dep1 != ssa2_dep2 || ssa1_dep2 != ssa2_dep1)
    return;

  range_operator *handler1 = gimple_range_handler (SSA_NAME_DEF_STMT (ssa1));
  range_operator *handler2 = gimple_range_handler (SSA_NAME_DEF_STMT (ssa2));

  // If either handler is not present, no relation is found.
  if (!handler1 || !handler2)
    return;

  int_range<2> bool_one (boolean_true_node, boolean_true_node);

  relation_kind relation1 = handler1->op1_op2_relation (bool_one);
  relation_kind relation2 = handler2->op1_op2_relation (bool_one);
  if (relation1 == VREL_NONE || relation2 == VREL_NONE)
    return;

  if (reverse_op2)
    relation2 = relation_negate (relation2);

  // x && y is false if the relation intersection of the true cases is NULL.
  if (is_and && relation_intersect (relation1, relation2) == VREL_EMPTY)
    lhs_range = int_range<2> (boolean_false_node, boolean_false_node);
  // x || y is true if the union of the true cases is NO-RELATION..
  // ie, one or the other being true covers the full range of possibilties.
  else if (!is_and && relation_union (relation1, relation2) == VREL_NONE)
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
fur_source::register_outgoing_edges (gcond *s, irange &lhs_range, edge e0, edge e1)
{
  int_range_max r;
  int_range<2> e0_range, e1_range;
  tree name;
  range_operator *handler;
  basic_block bb = gimple_bb (s);

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
  tree ssa1 = gimple_range_ssa_p (gimple_range_operand1 (s));
  tree ssa2 = gimple_range_ssa_p (gimple_range_operand2 (s));
  if (ssa1 && ssa2)
    {
      handler = gimple_range_handler (s);
      gcc_checking_assert (handler);
      if (e0)
	{
	  relation_kind relation = handler->op1_op2_relation (e0_range);
	  if (relation != VREL_NONE)
	    register_relation (e0, relation, ssa1, ssa2);
	}
      if (e1)
	{
	  relation_kind relation = handler->op1_op2_relation (e1_range);
	  if (relation != VREL_NONE)
	    register_relation (e1, relation, ssa1, ssa2);
	}
    }

  // Outgoing relations of GORI exports require a gori engine.
  if (!gori ())
    return;

  // Now look for other relations in the exports.  This will find stmts
  // leading to the condition such as:
  // c_2 = a_4 < b_7
  // if (c_2)
  FOR_EACH_GORI_EXPORT_NAME (*(gori ()), bb, name)
    {
      if (TREE_CODE (TREE_TYPE (name)) != BOOLEAN_TYPE)
	continue;
      gimple *stmt = SSA_NAME_DEF_STMT (name);
      handler = gimple_range_handler (stmt);
      if (!handler)
	continue;
      tree ssa1 = gimple_range_ssa_p (gimple_range_operand1 (stmt));
      tree ssa2 = gimple_range_ssa_p (gimple_range_operand2 (stmt));
      if (ssa1 && ssa2)
	{
	  if (e0 && gori ()->outgoing_edge_range_p (r, e0, name, *m_query)
	      && r.singleton_p ())
	    {
	      relation_kind relation = handler->op1_op2_relation (r);
	      if (relation != VREL_NONE)
		register_relation (e0, relation, ssa1, ssa2);
	    }
	  if (e1 && gori ()->outgoing_edge_range_p (r, e1, name, *m_query)
	      && r.singleton_p ())
	    {
	      relation_kind relation = handler->op1_op2_relation (r);
	      if (relation != VREL_NONE)
		register_relation (e1, relation, ssa1, ssa2);
	    }
	}
    }
}
