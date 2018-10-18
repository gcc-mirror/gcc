/* GIMPLE range support functions.
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


// This function returns a range for a tree node.  Return false if ranges are
// not supported.

bool
gimple_range_of_expr (irange &r, tree expr)
{
  tree type;
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
	  // handle &var which can show up in phi arguments
	  bool ov;
	  type = TREE_TYPE (expr);
	  if (gimple_range_supports_type (type))
	    {
	      if (tree_single_nonzero_warnv_p (expr, &ov))
		range_non_zero (&r, type);
	      else
		r.set_varying (type);
	      return true;
	    }
	  break;
	}

      default:
	if (TYPE_P (expr))
	  type = expr;
	else
	  type = TREE_TYPE (expr);
	if (gimple_range_supports_type (type))
	  {
	    // Set to range for this type.
	    r.set_varying (type);
	    return true;
	  }
	break;
    }

  return false;
}

// Calculate a range for range_op statement S and return it in R.  If a range
// cannot be calculated, return false.  

bool
gimple_range_of_stmt (irange &r, grange_op *s)
{
  irange range1, range2;

  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

  // Calculate a range for operand 1.
  if (!gimple_range_of_expr (range1, op1))
    return false;
  if (!op2)
    return s->fold (r, range1);

  // Calculate a range for operand 2.
  if (!gimple_range_of_expr (range2, op2))
    return false;
  return s->fold (r, range1, range2);
}


// Calculate a range for statement S and return it in R.  If a range cannot
// be calculated, return false.

bool
gimple_range_of_stmt (irange &r, gimple *s)
{
  grange_op *g = dyn_cast<grange_op *>(s);
  if (g)
    return gimple_range_of_stmt (r, g);
  return false;
}

// Calculate a range for range_op statement S by replacing any occurrence of
// ssa_name NAME with the RANGE_OF_NAME. If it can be evaluated, TRUE is 
// returned and the resulting range returned in R. 

bool
gimple_range_of_stmt (irange& r, grange_op *s, tree name,
		      const irange& range_of_name)
{
  irange range1, range2;
  tree op1 = s->operand1 ();
  tree op2 = s->operand2 ();

//  gcc_checking_assert (gimple_range_valid_ssa (name));
//  gcc_checking_assert (TREE_TYPE (name) == range_of_name.type ());
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

// Calculate a range for statement S by replacing any occurrence of
// ssa_name NAME with the RANGE_OF_NAME. If it can be evaluated, TRUE is 
// returned and the resulting range returned in R. 

bool
gimple_range_of_stmt (irange& r, gimple *g, tree name,
		      const irange& range_of_name)
{
  grange_op *rn = dyn_cast<grange_op *> (g);
  if (rn)
    return gimple_range_of_stmt (r, rn, name, range_of_name);
  return false;
}
  
// Return the range forced by edge E for a branch statement in R.
// Note this applies only to edges with PRED blocks ending in statements
// with control flow but no LHS. Such as the true or false edge of a branch
// or the case values on a switch edge.
//
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


// Evaluate a binary logical expression for this statement by combining the
// true and false ranges for each of the operands based on the result value in 
// the LHS. 

bool
glogical::combine (irange& r, const irange& lhs, const irange& op1_true,
		   const irange& op1_false, const irange& op2_true,
		   const irange& op2_false)
{
  // This is not a simple fold of a logical expression, rather it determines
  // ranges which flow through the logical expression.
  // Assuming x_8 is an unsigned char, and relational statements:
  //	      b_1 = x_8 < 20
  //	      b_2 = x_8 > 5
  // consider the logical expression and branch:
  //          c_2 = b_1 && b_2
  //          if (c_2)
  // To determine the range of x_8 on either edge of the branch,
  // one must first determine what the range of x_8 is when the boolean
  // values of b_1 and b_2 are both true and false.
  //    b_1 TRUE      x_8 = [0, 19]
  //    b_1 FALSE     x_8 = [20, 255]
  //    b_2 TRUE      x_8 = [6, 255]
  //    b_2 FALSE     x_8 = [0,5]. 
  //
  // These ranges are then combined based on the expected outcome of the branch
  // The range on the TRUE side of the branch must satisfy 
  //     b_1 == true && b_2 == true
  // in terms of x_8, that means both x_8 == [0, 19] and x_8 = [6, 255]
  // must be true.  The range of x_8 on the true side must be the intersection
  // of both ranges since both must be true.  Thus the range of x_8
  // on the true side is [6, 19]
  // 
  // To determine the ranges on the FALSE side, all 3 combinations of
  // failing ranges must be considered, and combined as any of them can cause
  // the false result.
  
  // If the LHS can be TRUE OR FALSE, then evaluate both a TRUE and FALSE
  // results and combine them.  If we fell back to VARYING any range
  // restrictions that have been discovered up to this point would be lost.  */
  if (!lhs.singleton_p ())
    {
      irange r1;
      irange bool_zero (boolean_type_node, boolean_false_node,
			boolean_false_node);
      irange bool_one (boolean_type_node, boolean_true_node, boolean_true_node);
      if (combine (r1, bool_zero, op1_true, op1_false, op2_true, op2_false) &&
	  combine (r, bool_one, op1_true, op1_false, op2_true, op2_false))
	{
	  r.union_ (r1);
	  return true;
	}
      return false;

    }

  switch (gimple_expr_code (this))
    {
      //  A logical AND combines ranges from 2 boolean conditions.
      //       c_2 = b_1 && b_2
      case TRUTH_AND_EXPR:
      case BIT_AND_EXPR:
        if (!lhs.zero_p ())
	  // The TRUE side is the intersection of the the 2 true ranges.
	  r = range_intersect (op1_true, op2_true);
	else
	  {
	    // The FALSE side is the union of the other 3 cases. 
	    irange ff = range_intersect (op1_false, op2_false);
	    irange tf = range_intersect (op1_true, op2_false);
	    irange ft = range_intersect (op1_false, op2_true);
	    r = range_union (ff, tf);
	    r.union_ (ft);
	  }
        break;

      //  A logical OR combines ranges from 2 boolean conditons.
      // 	c_2 = b_1 || b_2
      case TRUTH_OR_EXPR:
      case BIT_IOR_EXPR:
        if (lhs.zero_p ())
	  // An OR operation will only take the FALSE path if both operands
	  // are false. so [20, 255] intersect [0, 5] is the 
	  // union: [0,5][20,255].  */
	  r = range_intersect (op1_false, op2_false);
	else
	  {
	    // The TRUE side of an OR operation will be the union of the other
	    // three combinations.
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

// Return the First operand of this statement if it is a valid operand 
// supported by ranges, otherwise return NULL_TREE. 
//
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
	      bool strict_ov;
	      tree base = get_base_address (TREE_OPERAND (expr, 0));
	      if (base != NULL_TREE && TREE_CODE (base) == MEM_REF)
	        {
		  // If the base address is an SSA_NAME, return it. 
		  tree b = gimple_range_valid_ssa (TREE_OPERAND (base, 0));
		  if (b)
		    return b;
		}
	      // Otherwise, check to see if the RHS is always non-null
	      // and return a constant that will equate to a non-zero range.
	      if (tree_single_nonzero_warnv_p (expr, &strict_ov))
		return integer_one_node;
	      // Otherwise this is not something complex needing tweaking.
	    }
	  return expr;
	}
      default:
        break;
    }
  return NULL;
}


// Fold this unary statement uusing R1 as operand1's range, returning the
// result in RES.  Return false if the operation fails.

bool
grange_op::fold (irange &res, const irange& r1) const
{
  irange r2;
  // Single ssa operations require the LHS type as the second range.
  if (lhs ())
    r2.set_varying (TREE_TYPE (lhs ()));
  else
    r2.set_undefined (r1.type ());

  return handler()->fold_range (res, r1, r2);
}


// Fold this binary statement using R1 and R2 as the operands ranges,
// returning the result in RES.  Return false if the operation fails.

bool
grange_op::fold (irange &res, const irange& r1, const irange& r2) const
{
  // Make sure this isnt a unary operation being passed a second range.
  return handler()->fold_range (res, r1, r2);
}

// Calculate what we can determine of the range of this unary statement's
// operand if the lhs of the expression has the range LHS_RANGE.  Return false
// if nothing can be determined.

bool
grange_op::calc_op1_irange (irange& r, const irange& lhs_range) const
{  
  irange type_range;
  gcc_checking_assert (gimple_num_ops (this) < 3);
  // An empty range is viral, so return an empty range.
  if (lhs_range.undefined_p ())
    {
      r.set_undefined (TREE_TYPE (operand1 ()));
      return true;
    }
  // Unary operations require the type of the first operand in the second range
  // position.
  type_range.set_varying (TREE_TYPE (operand1 ()));
  return handler ()->op1_range (r, lhs_range, type_range);
}

// Calculate what we can determine of the range of this statement's first 
// operand if the lhs of the expression has the range LHS_RANGE and the second
// operand has the range OP2_RANGE.  Return false if nothing can be determined.

bool
grange_op::calc_op1_irange (irange& r, const irange& lhs_range,
			    const irange& op2_range) const
{  
  // Unary operation are allowed to pass a range in for second operand
  // as there are often additional restrictions beyond the type which can
  // be imposed.  See operator_cast::op1_irange.()
  
  // An empty range is viral, so return an empty range.
  if (op2_range.undefined_p () || lhs_range.undefined_p ())
    {
      r.set_undefined (op2_range.type ());
      return true;
    }
  return handler ()->op1_range (r, lhs_range, op2_range);
}

// Calculate what we can determine of the range of this statement's second
// operand if the lhs of the expression has the range LHS_RANGE and the first
// operand has the range OP1_RANGE.  Return false if nothing can be determined.

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

