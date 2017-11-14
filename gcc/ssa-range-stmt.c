/* On-demand ssa range statement summary.
   Copyright (C) 2017 Free Software Foundation, Inc.
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
#include "ssa-range-stmt.h"

/* The defintion dependency vectors are used to indicate which ssa_names may 
   require range info in order to fully resolve the range. 

   d_7 = a_3 + 2
   d_8 = d_7 * 2
   c_9 = d_8 < b_6
   if (c_9)

   when determining range information for the if, we require knowledge of what
   c_9 is built from.  c_9 can utilize range information from both d_8 and b_6
   d_8 in turn can use info from d_7, and d_7 uses info from a_3.

   Range can also be determined for any of these components by adjusting 
   the operations. ie, on the TRUE side of the branch, we can also view the
   expression as :
   c_9 = (d_7 * 2) < b_6,   --> c_9 = (d_7 < b_6 / 2)
   c_9 = (a_3 + 2) < b_6 / 2   -->  c_9 = (a_3 < (b_6 / 2) - 2)

   The definition dependency map will provide a list of the ssa_names which 
   are used in the contruction of a given value, but only for those which
   we have the ablity to do thes transformations.  Ie, have entries in the
   range_operator_table.

   For the above example, the def_Dep vectors for each name will look like:
   a_3 ->  NULL
   d_7 -> a_3
   d_8 -> a_3, d_7
   c_9 -> d_8, a_3, d_7, b_6
   b_6 -> NULL

   with a quick check of an ssa_name we can determine:
     a) Any name in a branch operand provides the list of names which that
	branch can generate range info for.
     b) which operand to look for a specific name, ie:
     	d_8 < b_6 , if we are looking for range in terms of a_3, the bit for
	a_3 is set in d_8, so thats the operand to follow calling the
	op_adjust routines to get the range adjustment in terms of a_3
     c) Which ssa-name's range info could be useful to refine other ranges.

   We also limit the depth of these to some value from the actual branch so
   we dont look too far back and involve too many names.  

   Names are reprocessed as they may be used at differfent depths from different
   branches, and this ensures we alwasy get at least that depth from each
   branch.  */


irange_operator *
range_stmt::handler () const
{
  return irange_op_handler (code);
}

/* Intialize the state based on the operands to the expression.  */
bool
range_stmt::validate_operands ()
{
  ssa1 = valid_irange_ssa (op1);
  ssa2 = valid_irange_ssa (op2);

  if (!op2)
    {
      if (ssa1 || (TREE_CODE (op1) == INTEGER_CST && !TREE_OVERFLOW (op1)))
	return true;
    }
  else
    {
      if (ssa1 && ssa2 && ((code >= LT_EXPR && code <= NE_EXPR)
			   || logical_expr_p (TREE_TYPE (op1))))
	return true;
      if ((ssa1 || (TREE_CODE (op1) == INTEGER_CST && !TREE_OVERFLOW (op1)))
	  && (ssa2 || (TREE_CODE (op2) == INTEGER_CST && !TREE_OVERFLOW (op2))))
	return true;
    }
  return false;
}

/* Build a range node from a stmt, if it possible.  */
void
range_stmt::from_stmt (gimple *s)
{
  g = NULL;
  switch (gimple_code (s))
    {
      case GIMPLE_COND:
        {
	  gcond *gc = as_a <gcond *> (s);
	  code  = gimple_cond_code (gc);
	  if (irange_op_handler (code))
	    {
	      g = s;
	      op1 = gimple_cond_lhs (gc);
	      op2 = gimple_cond_rhs (gc);
	      if (validate_operands ())
	        g = s;;
	    }
	  break;
	}
      case GIMPLE_ASSIGN:
	{
	  gassign *ga = as_a <gassign *> (s);
	  code = gimple_assign_rhs_code (ga);
	  if (irange_op_handler (code))
	    {
	      op1 = gimple_assign_rhs1 (ga);
	      if (get_gimple_rhs_class (code) == GIMPLE_BINARY_RHS)
		op2 = gimple_assign_rhs2 (ga);
	      else
		op2 = NULL;
	      if (validate_operands ())
	        g = s;
	    }
	  break;
	}

      default:
        break;
    }
}


bool
range_stmt::logical_expr_p (tree type) const
{

  /* Look for boolean and/or condition.  */
  switch (get_code ())
    {
      case TRUTH_AND_EXPR:
      case TRUTH_OR_EXPR:
        return true;

      case BIT_AND_EXPR:
      case BIT_IOR_EXPR:
        if (types_compatible_p (type, boolean_type_node))
	  return true;
	break;

      default:
        break;
    }
  return false;
}

bool
range_stmt::logical_expr (irange& r, const irange& lhs, const irange& op1_true,
			  const irange& op1_false, const irange& op2_true,
			  const irange& op2_false)
{
  gcc_checking_assert (logical_expr_p (TREE_TYPE (op1)));
 
  /* If the LHS can be TRUE OR FALSE, then we cant really tell anything.  */
  if (!wi::eq_p (lhs.lower_bound(), lhs.upper_bound()))
    return false;

  /* Now combine based on the result.  */
  switch (code)
    {

      /* A logical AND of two ranges is executed when we are walking forward
	 with ranges that have been determined.   x_8 is an unsigned char.
	       b_1 = x_8 < 20
	       b_2 = x_8 > 5
	       c_2 = b_1 && b_2
	 if we are looking for the range of x_8, the ranges on each side 
	 will be:   b_1 carries x_8 = [0, 19],   b_2 carries [6, 255]
	 the result of the AND is the intersection of the 2 ranges, [6, 255]. */
      case TRUTH_AND_EXPR:
      case BIT_AND_EXPR:
        if (!lhs.zero_p ())
	  r = irange_intersect (op1_true, op2_true);
	else
	  {
	    irange ff = irange_intersect (op1_false, op2_false);
	    irange tf = irange_intersect (op1_true, op2_false);
	    irange ft = irange_intersect (op1_false, op2_true);
	    r = irange_union (ff, tf);
	    r.union_ (ft);
	  }
        break;

      /* A logical OR of two ranges is executed when we are walking forward with
	 ranges that have been determined.   x_8 is an unsigned char.
	       b_1 = x_8 > 20
	       b_2 = x_8 < 5
	       c_2 = b_1 || b_2
	 if we are looking for the range of x_8, the ranges on each side
	 will be:   b_1 carries x_8 = [21, 255],   b_2 carries [0, 4]
	 the result of the OR is the union_ of the 2 ranges, [0,4][21,255].  */
      case TRUTH_OR_EXPR:
      case BIT_IOR_EXPR:
        if (lhs.zero_p ())
	  r = irange_intersect (op1_false, op2_false);
	else
	  {
	    irange tt = irange_intersect (op1_true, op2_true);
	    irange tf = irange_intersect (op1_true, op2_false);
	    irange ft = irange_intersect (op1_false, op2_true);
	    r = irange_union (tt, tf);
	    r.union_ (ft);
	  }
	break;

      default:
        gcc_unreachable ();
    }

  return true;
}


range_stmt::range_stmt ()
{
  g = NULL;
}

range_stmt::range_stmt (gimple *s)
{
  from_stmt (s);
}

range_stmt&
range_stmt::operator= (gimple *s)
{
  from_stmt (s);
  return *this;
}

/* THis function will attempt to resolve the expression to a constant. 
   If the expression can be resolved, true is returned, and the range is
   returned in RES.
   If one or more SSA_NAME's needs to be resolved to a range first, their
   values are passed in as parameters.  It is an error to attempt to resolve
   an ssa_name with no range.  */

bool
range_stmt::fold (irange &res, const irange& r1) const
{
  irange r2;
  irange_operator *handler = irange_op_handler (code);
  gcc_assert (handler != NULL);

  /* Single ssa operations require the LHS type as the second range.  */
  tree lhs = gimple_get_lhs (g);
  if (lhs)
    r2.set_range_for_type (TREE_TYPE (lhs));
  else
    r2.clear ();
  return handler->fold_range (res, r1, r2);
}

bool
range_stmt::fold (irange &res, const irange& r1, const irange& r2) const
{
  irange_operator *handler = irange_op_handler (code);
  gcc_assert (handler != NULL);

  // Make sure this isnt a unary operation being passsed a second range.
  gcc_assert (op2);
  return handler->fold_range (res, r1, r2);
}


bool
range_stmt::fold (irange &res) const
{
  irange r1, r2;
  
  if (!op2)
    {
      get_operand_range (r1, op1);
      return fold (res, r1);
    }

  get_operand_range (r1, op1);
  get_operand_range (r2, op2);
  return fold (res, r1, r2);
}


bool
range_stmt::fold (irange& res, tree name, const irange& name_range) const
{
  irange r1, r2;

  if (!op2)
    {
      if (ssa1 == name)
	r1 = name_range;
      else
	get_operand_range (r1, op1);
      return fold (res, r1);
    }

  if (ssa1 == name)
    r1 = name_range;
  else
    get_operand_range (r1, op1);

  if (ssa2 == name)
    r2 = name_range;
  else
    get_operand_range (r2, op2);

  return fold (res, r1, r2);
}

bool
range_stmt::op1_irange (irange& r, const irange& lhs_range) const
{  
  irange type_range;
  type_range.set_range_for_type (TREE_TYPE (op1));
  return handler ()->op1_irange (r, lhs_range, type_range);
}

bool
range_stmt::op1_irange (irange& r, const irange& lhs_range,
			const irange& op2_range) const
{  
  gcc_assert (op2 != NULL);
  return handler ()->op1_irange (r, lhs_range, op2_range);
}

bool
range_stmt::op2_irange (irange& r, const irange& lhs_range,
			const irange& op1_range) const
{  
  return handler ()->op2_irange (r, lhs_range, op1_range);
}

void
range_stmt::dump (FILE *f) const
{
  print_gimple_stmt (f, g, 0, 0);

  if (!op2)
    irange_op_handler (code)->dump (f);

  print_generic_expr (f, op1, TDF_SLIM);

  if (op2)
    {
      irange_op_handler (code)->dump (f);
      print_generic_expr (f, op2, TDF_SLIM);
    }

}

bool
get_operand_range (irange& r, tree op)
{
  /* This check allows unary operations to be handled without having to 
     make an explicit check for the existence of a second operand.  */
  if (!op)
    return false;

  if (TREE_CODE (op) == INTEGER_CST)
    {
      r.set_range (TREE_TYPE (op), op, op);
      return true;
    }
  else
    if (TREE_CODE (op) == SSA_NAME)
      {
	/* Eventually we may go look for an on-demand range... */
	r = op;
	return true;
      }

  /* Unary expressions often set the type for the operand, get that range.  */
  if (TYPE_P (op))
    r.set_range_for_type (op);
  else
    r.set_range_for_type (TREE_TYPE (op));
  return true;
}




