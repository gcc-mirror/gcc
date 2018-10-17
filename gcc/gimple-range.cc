/* RIMPLE - Range GIMPLE supoprt functions.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
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
#include "gimple-range.h"
#include "fold-const.h"


// This function returns a basic range for a tree node.  If ranges are not
// supported, the false is returned.
bool
gimple_range_of_expr (irange &r, tree expr)
{
  tree t;
  switch (TREE_CODE (expr))
    {
      case INTEGER_CST:
	r = irange (TREE_TYPE (expr), expr, expr);
	return true;

      case SSA_NAME:
        if (gimple_range_supports_ssa (expr))
	  {
	    r = range_from_ssa (expr);
	    return true;
	  }
	break;

      case ADDR_EXPR:
        {
	  bool ov;
	  t = TREE_TYPE (expr);
	  if (gimple_range_supports_type (t))
	    {
	      // handle &var which can show up in phi arguments
	      if (tree_single_nonzero_warnv_p (expr, &ov))
		range_non_zero (&r, TREE_TYPE (expr));
	      else
		r.set_varying (TREE_TYPE (expr));
	      return true;
	    }
	  break;
	}

      default:
	if (TYPE_P (expr))
	  t = expr;
	else
	  t = TREE_TYPE (expr);
	if (gimple_range_supports_type (t))
	  {
	    // Set to range for this type.
	    r.set_varying (t);
	    return true;
	  }
	break;
    }

  return false;
}

bool
gimple_range_of_stmt (irange &r, grange_op *s)
{
  irange range1, range2;

  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  if (!gimple_range_of_expr (range1, op1))
    return false;
  if (!op2)
    return s->fold (r, range1);

  if (!gimple_range_of_expr (range2, op2))
    return false;
  return s->fold (r, range1, range2);
}

bool
gimple_range_of_stmt (irange &r, gimple *s)
{
  grange_op *rn = dyn_cast<grange_op *>(s);
  if (rn)
    return gimple_range_of_stmt (r, rn);
  return false;
}

// This function will attempt to evaluate the statement G by replacing any
// occurrence of ssa_name NAME with the RANGE_OF_NAME. If it can be
// evaluated, TRUE is returned and the resulting range returned in R. 
bool
gimple_range_of_stmt (irange& r, grange_op *s, tree name,
		      const irange& range_of_name)
{
  irange range1, range2;
  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  if (op1 == name)
    range1 = range_of_name;
  else
    if (!gimple_range_of_expr (range1, op1))
      return false;

  if (!op2)
    return s->fold (r, range1);

  if (op2 == name)
    range2 = range_of_name;
  else
    if (!gimple_range_of_expr (range2, op2))
      return false;

  return s->fold (r, range1, range2);
}

bool
gimple_range_of_stmt (irange& r, gimple *g, tree name,
		      const irange& range_of_name)
{
  grange_op *rn = dyn_cast<grange_op *> (g);
  if (rn)
    return gimple_range_of_stmt (r, rn, name, range_of_name);
  return false;
}
  

/* Evaluate a binary logical expression given true and false ranges for each
   of the operands. Base the result on the value in the LHS.  */
bool
gimple_range_logical_fold (irange& r, const glogical *g, const irange& lhs,
			   const irange& op1_true, const irange& op1_false,
			   const irange& op2_true, const irange& op2_false)
{
  /* If the LHS can be TRUE OR FALSE, then both need to be evaluated and
     combined, otherwise any range restrictions that have been determined
     leading up to this point would be lost.  */
  if (!wi::eq_p (lhs.lower_bound(), lhs.upper_bound()))
    {
      irange r1;
      unsigned prec = TYPE_PRECISION (boolean_type_node);
      irange bool_zero (boolean_type_node, wi::zero (prec), wi::zero (prec));
      irange bool_one (boolean_type_node, wi::one (prec), wi::one (prec));
      if (gimple_range_logical_fold (r1, g, bool_zero, op1_true, op1_false,
				     op2_true, op2_false) &&
	  gimple_range_logical_fold (r, g, bool_one, op1_true, op1_false,
				     op2_true, op2_false))
	{
	  r.union_ (r1);
	  return true;
	}
      return false;

    }

  /* Now combine based on whether the result is TRUE or FALSE.  */
  switch (gimple_expr_code (g))
    {

      /* A logical operation on two ranges is executed with operand ranges that
	 have been determined for both a TRUE and FALSE result..
	 Assuming x_8 is an unsigned char:
		b_1 = x_8 < 20
		b_2 = x_8 > 5
	 if we are looking for the range of x_8, the operand ranges will be:
	 will be: 
	 b_1 TRUE	x_8 = [0, 19]
	 b_1 FALSE  	x_8 = [20, 255]
	 b_2 TRUE 	x_8 = [6, 255]
	 b_2 FALSE	x_8 = [0,5]. */
	       
      /*	c_2 = b_1 && b_2
	 The result of an AND operation with a TRUE result is the intersection
	 of the 2 TRUE ranges, [0,19] intersect [6,255]  ->   [6, 19]. */
      case TRUTH_AND_EXPR:
      case BIT_AND_EXPR:
        if (!lhs.zero_p ())
	  r = range_intersect (op1_true, op2_true);
	else
	  {
	    /* The FALSE side is the union of the other 3 cases.  */
	    irange ff = range_intersect (op1_false, op2_false);
	    irange tf = range_intersect (op1_true, op2_false);
	    irange ft = range_intersect (op1_false, op2_true);
	    r = range_union (ff, tf);
	    r.union_ (ft);
	  }
        break;

      /* 	c_2 = b_1 || b_2
	 An OR operation will only take the FALSE path if both operands are
	 false, so [20, 255] intersect [0, 5] is the union: [0,5][20,255].  */
      case TRUTH_OR_EXPR:
      case BIT_IOR_EXPR:
        if (lhs.zero_p ())
	  r = range_intersect (op1_false, op2_false);
	else
	  {
	    /* The TRUE side of the OR operation will be the union of the other
	       three combinations.  */
	    irange tt = range_intersect (op1_true, op2_true);
	    irange tf = range_intersect (op1_true, op2_false);
	    irange ft = range_intersect (op1_false, op2_true);
	    r = range_union (tt, tf);
	    r.union_ (ft);
	  }
	break;

      default:
        gcc_unreachable ();
    }

  return true;
}


/* Return the range implied by edge E for a branch statement in R.  */
void
gimple_range_outgoing_edge (irange &r, edge e)
{
  gcc_checking_assert (is_a<gcond *> (last_stmt (e->src)));
  if (e->flags & EDGE_TRUE_VALUE)
    r = irange (boolean_type_node, boolean_true_node, boolean_true_node);
  else if (e->flags & EDGE_FALSE_VALUE)
    r = irange (boolean_type_node, boolean_false_node, boolean_false_node);
  else
    gcc_unreachable ();

  return;
}


/* Return the First operand of the statement if it is a valid operand which
   is supported by rimple. Otherwise return NULL_TREE.  */
tree
grange_op::operand1 () const
{
  switch (gimple_code (this))
    {
      case GIMPLE_COND:
        return gimple_cond_lhs (this);
      case GIMPLE_ASSIGN:
        {
	  tree expr = gimple_assign_rhs1 (this);
	  if (gimple_assign_rhs_code (this) == ADDR_EXPR)
	    {
	      // If the base address is an SSA_NAME, return it. 
	      // Or if its range is non-zero, then the pointer is non-zero.
	      bool strict_ov;
	      tree base = get_base_address (TREE_OPERAND (expr, 0));
	      if (base != NULL_TREE && TREE_CODE (base) == MEM_REF)
	        {
		  tree b = TREE_OPERAND (base, 0);
		  if (TREE_CODE (b) == SSA_NAME &&
		      gimple_range_supports_ssa (b))
		    return TREE_OPERAND (base, 0);
		}
	      // Otherwise, check to see if the RHS is always non-null
	      // and return a constant that will equate to a non-zero range.
	      if (tree_single_nonzero_warnv_p (expr, &strict_ov))
		return integer_one_node;
	      // Otherwise this is not something complex we care about. Return
	      // the RHS and allow validate_stmt to have a look.
	    }
	  return expr;
	}
      default:
        break;
    }
  return NULL;
}


/* This method will attempt to resolve a unary expression with value R1 to
   a range.  If the expression can be resolved, true is returned, and the
   range is returned in RES.  */
bool
grange_op::fold (irange &res, const irange& r1) const
{
  irange r2;
  /* Single ssa operations require the LHS type as the second range.  */
  if (lhs ())
    r2.set_varying (TREE_TYPE (lhs ()));
  else
    r2.set_undefined (r1.type ());

  return handler()->fold_range (res, r1, r2);
}


/* This method will attempt to resolve a binary expression with operand
   values R1 tnd R2 to a range.  If the expression can be resolved, true is
   returned, and the range is returned in RES.  */
bool
grange_op::fold (irange &res, const irange& r1, const irange& r2) const
{
  // Make sure this isnt a unary operation being passed a second range.
  return handler()->fold_range (res, r1, r2);
}

/* This method will evaluate a range for the operand of a unary expression
   given a range for the LHS of the expression in LHS_RANGE. If it can be
   evaluated, TRUE is returned and the resulting range returned in RES.  */
bool
grange_op::calc_op1_irange (irange& r, const irange& lhs_range) const
{  
  irange type_range;
  // An empty range is viral, so return an empty range.
  if (lhs_range.undefined_p ())
    {
      r.set_undefined (TREE_TYPE (operand1 ()));
      return true;
    }
  type_range.set_varying (TREE_TYPE (operand1 ()));
  return handler ()->op1_range (r, lhs_range, type_range);
}

/* This method will evaluate a range for operand 1 of a binary expression
   given a range for the LHS in LHS_RANGE and a range for operand 2 in
   OP2_RANGE. If it can be evaluated, TRUE is returned and the resulting
   range returned in RES.  */
bool
grange_op::calc_op1_irange (irange& r, const irange& lhs_range,
			    const irange& op2_range) const
{  
  // Changing the API to say unary ops can be called with the range of the
  // RHS instead of the type if the caller wants.
  // This is more flexible since we can still get the type from the range if
  // that is all we want, but getting an actual range will let us sometimes
  // determine more. see operator_cast::op1_irange.()
  
  // gcc_assert (operand2 () != NULL);
  
  // An empty range is viral, so return an empty range.
  if (op2_range.undefined_p () || lhs_range.undefined_p ())
    {
      r.set_undefined (op2_range.type ());
      return true;
    }
  return handler ()->op1_range (r, lhs_range, op2_range);
}

/* This method will evaluate a range for operand 2 of a binary expression
   given a range for the LHS in LHS_RANGE and a range for operand 1 in
   OP1_RANGE. If it can be evaluated, TRUE is returned and the resulting
   range returned in RES.  */
bool
grange_op::calc_op2_irange (irange& r, const irange& lhs_range,
			     const irange& op1_range) const
{  
  // An empty range is viral, so return an empty range.
  if (op1_range.undefined_p () || lhs_range.undefined_p ())
    {
      r.set_undefined (op1_range.type ());
      return true;
    }
  return handler ()->op2_range (r, lhs_range, op1_range);
}

