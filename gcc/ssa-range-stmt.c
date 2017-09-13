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

#define trace_output ((FILE *)0)


irange_operator *
range_stmt::handler () const
{
  return irange_op_handler (code);
}

#define ACCEPTABLE_SSA(T) ((T) && TREE_CODE (T) == SSA_NAME		\
			   && (INTEGRAL_TYPE_P (TREE_TYPE (T))		\
			       || POINTER_TYPE_P (TREE_TYPE (T))))

/* Intialize the state based on the operands to the expression.  */
enum range_stmt_state
range_stmt::determine_state (tree t1, tree t2)
{
  enum range_stmt_state st = RS_INV;
  ssa1 = ACCEPTABLE_SSA (t1) ? t1 : NULL_TREE;
  ssa2 = ACCEPTABLE_SSA (t2) ? t2 : NULL_TREE;

  if (!t2 || TYPE_P (t2))
    {
      /* Check for unary cases.  */
      if (ssa1)
	st = RS_S;
      else
	if (TREE_CODE (t1) == INTEGER_CST && !TREE_OVERFLOW (t1))
	  st = RS_I;
    }
  else
    {
      /* Binary cases.  */
      if (ssa1)
	{
	  if (ssa2)
	    st = RS_SS;
	  else
	    if (TREE_CODE (t2) == INTEGER_CST && !TREE_OVERFLOW (t2))
	      st = RS_SI;
	}
      else
	if (TREE_CODE (t1) == INTEGER_CST && !TREE_OVERFLOW (t1))
	  {
	    if (ssa2)
	      st = RS_IS;
	    else
	      if (TREE_CODE (t2) == INTEGER_CST && !TREE_OVERFLOW (t2))
		st = RS_II;
	  }
    }
  return st;
}

/* Build a range node from a stmt, if it possible.  */
void
range_stmt::from_stmt (gimple *s)
{
  state = RS_INV;  /* Assume invalid state.  */
  switch (gimple_code (s))
    {
      case GIMPLE_COND:
        {
	  gcond *g = as_a <gcond *> (s);
	  code  = gimple_cond_code (g);
	  if (irange_op_handler (code))
	    {
	      op1 = gimple_cond_lhs (g);
	      op2 = gimple_cond_rhs (g);
	      state = determine_state (op1, op2);
	    }
	  break;
	}
      case GIMPLE_ASSIGN:
	{
	  gassign *g = as_a <gassign *> (s);
	  code = gimple_assign_rhs_code (g);
	  if (irange_op_handler (code))
	    {
	      op1 = gimple_assign_rhs1 (g);
	      if (get_gimple_rhs_class (code) == GIMPLE_BINARY_RHS)
		op2 = gimple_assign_rhs2 (g);
	      else
	        /* Unary operations often need the type, think casting.  */
		if (get_gimple_rhs_class (code) == GIMPLE_UNARY_RHS)
		  op2 = TREE_TYPE (op1);
		else
		  op2 = NULL;
	      state = determine_state (op1, op2);
	    }
	  break;
	}

      default:
        break;
    }
}


bool
range_stmt::combine_range_p (tree type)
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
range_stmt::combine_range (irange& r, const irange& lhs, const irange& op1_true,
			   const irange& op1_false, const irange& op2_true,
			   const irange& op2_false)
{
  gcc_checking_assert (combine_range_p (TREE_TYPE (op1)));
 
  if (trace_output)
    {
      fprintf (dump_file, "\nIn Combine_range for ");
      print_gimple_stmt (dump_file, g, 0 ,0);
    }

  /* If the LHS can be TRUE OR FALSE, then we cant really tell anything.  */
  if (!wi::eq_p (lhs.lower_bound(), lhs.upper_bound()))
    {
      if (trace_output)
	fprintf (dump_file, " : LHS can be true or false, know nothing.\n");
      return false;
    }

  if (trace_output)
    {
      fprintf (dump_file, "  combining following 4 ranges:\n");
      fprintf (dump_file, "op1_true : ");
      op1_true.dump (dump_file);
      fprintf (dump_file, "op1_false : ");
      op1_false.dump (dump_file);
      fprintf (dump_file, "op2_true : ");
      op2_true.dump (dump_file);
      fprintf (dump_file, "op2_false : ");
      op2_false.dump (dump_file);
    }

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

  if (trace_output)
    {
      fprintf (dump_file, "result range is ");
      r.dump (dump_file);
    }
  return true;
}


range_stmt::range_stmt ()
{
  state = RS_INV;
  g = NULL;
}

range_stmt::range_stmt (gimple *s)
{
  from_stmt (s);
  g = s;
}

range_stmt&
range_stmt::operator= (gimple *s)
{
  from_stmt (s);
  g = s;
  return *this;
}

bool
range_stmt::is_relational()
{
  if (code >= TRUTH_ANDIF_EXPR && code <= NE_EXPR)
    return true;
  if (code >= BIT_AND_EXPR && code <= BIT_NOT_EXPR &&
      types_compatible_p (TREE_TYPE (op1), boolean_type_node))
    return true;

  return false;
}

/* THis function will attempt to resolve the expression to a constant. 
   If the expression can be resolved, true is returned, and the range is
   returned in RES.
   If one or more SSA_NAME's needs to be resolved to a range first, their
   values are passed in as parameters.  It is an error to attempt to resolve
   an ssa_name with no range.  */

bool
range_stmt::fold (irange &res, irange* value1, irange* value2) const
{
  bool result = false;
  tree lhs;
  irange r1, r2;
  irange_operator *handler = irange_op_handler (code);
  
  gcc_assert (handler != NULL);

  switch (state)
    {
      case RS_I:
        gcc_assert (!value1 && !value2);
	r1.set_range (TREE_TYPE (operand1 ()), operand1 (), operand1 ());
	r2.clear ();
	result = handler->fold_range (res, r1, r2);
	break;

      case RS_II:
        gcc_assert (!value1 && !value2);
	r1.set_range (TREE_TYPE (operand1 ()), operand1 (), operand1 ());
	r2.set_range (TREE_TYPE (operand2 ()), operand2 (), operand2 ());
	result = handler->fold_range (res, r1, r2);
	break;

      case RS_S:
        {
	  gcc_assert (value1 && !value2);
	  lhs = gimple_get_lhs (g);
	  /* Single ssa operations require the LHS type as the second range.  */
	  if (lhs)
	    r2.set_range_for_type (TREE_TYPE (lhs));
	  else
	    r2.clear ();
	  result = handler->fold_range (res, *value1, r2);
	break;
      }

      case RS_SI:
        gcc_assert (value1 && !value2);
	r2.set_range (TREE_TYPE (operand2 ()), operand2 (), operand2 ());
	result = handler->fold_range(res, *value1, r2);
	break;

      case RS_IS:
	/* Allows calling with (NULL, range) in addition to (range).  */
        /* One param or the other needs to be non-null.  */
        gcc_assert (!value1 != !value2);
	r1.set_range (TREE_TYPE (operand1 ()), operand1 (), operand1 ());
	if (value1)
	  result = handler->fold_range (res, r1, *value1);
	else
	  result = handler->fold_range (res, r1, *value2);
	break;

      case RS_SS:
        gcc_assert (value1 && value2);
	result = handler->fold_range(res, *value1, *value2);
	break;
  
      default:
        gcc_unreachable ();
	break;
    }

  return result;
}


/* Resolve the expression using whatever we know about the global state of
   any SSA_NAMEs involved.  This will ensure we can always get a range,
   if it is possible. 
   Return TRUE if a range is calculated. */
   
bool
range_stmt::fold (irange& r, FILE *trace) const
{
  irange r1, r2;
  irange *r1p = NULL, *r2p = NULL;
  bool res;

  if (trace)
    {
      fprintf (trace, "Calling fold() on : ");
      dump (trace);
    }

  if (ssa1)
    {
      r1 = ssa1;
      r1p = &r1;
      if (trace)
        {
	  fprintf (trace, "  name1 range = ");
	  r1.dump ();
	}
    }

  if (ssa2)
    {
      r2 = ssa2;
      r2p = &r2;
      if (trace)
        {
	  fprintf (trace, "  name2 range = ");
	  r2.dump ();
	}
    }

  res = fold (r, r1p, r2p);
  if (trace)
    {
      fprintf (trace, "\nFold Result range : ");
      r.dump (trace);
      fprintf (trace, "\n");
    }

  return res;
}

bool
range_stmt::op1_irange (irange& r, const irange& lhs, const irange& op2,
		        FILE *trace) const
{  
  bool res;
  if (trace)
    {
      fprintf (trace, "\nCalling op1_irange () on : ");
      dump (trace);
      fprintf (trace, "\n  lhs = ");
      lhs.dump (trace);
      fprintf (trace, "  op2 = ");
      op2.dump (trace);
    }
  res = handler ()->op1_irange (r, lhs, op2);
  if (trace)
    {
      fprintf (trace, "  result of op1_irange: ");
      r.dump (trace);
    }
  return res;
}

bool
range_stmt::op2_irange (irange& r, const irange& lhs, const irange& op1,
		        FILE *trace) const
{  
  bool res;
  if (trace)
    {
      fprintf (trace, "\nCalling op2_irange () on : ");
      dump (trace);
      fprintf (trace, "\n  lhs = ");
      lhs.dump (trace);
      fprintf (trace, "  op1 = ");
      op1.dump (trace);
    }
  res = handler ()->op2_irange (r, lhs, op1);
  if (trace)
    {
      fprintf (trace, "  result of op2_irange: ");
      r.dump (trace);
    }
  return res;
}

void
range_stmt::dump (FILE *f) const
{
  print_gimple_stmt (f, g, 0, 0);
  switch (state)
  {
    case RS_INV:
      fprintf (f, "RS_INV  ");
      return;

    case RS_I:
      fprintf (f, "RS_I  ");
      irange_op_handler (code)->dump (f);
      break;
    case RS_S:
      fprintf (f, "RS_S  ");
      irange_op_handler (code)->dump (f);
      break;
    case RS_II:
      fprintf (f, "RS_II ");
      break;
    case RS_SI:
      fprintf (f, "RS_SI ");
      break;
    case RS_IS:
      fprintf (f, "RS_IS ");
      break;
    case RS_SS:
      fprintf (f, "RS_SS ");
      break;
    default:
      gcc_unreachable ();
  }

  if (op2 && (state == RS_I || state == RS_S))
    {
      irange_op_handler (code)->dump (f);
      fprintf (f, " (");
      print_generic_expr (f, op2, TDF_SLIM);
      fprintf (f, ") ");
    }

  print_generic_expr (stderr, op1, TDF_SLIM);

  if (state != RS_I && state != RS_S)
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
	/* But at least should look for what we currently know. */
	r.set_range_for_type (TREE_TYPE (op));
	return true;
      }

  /* Unary expressions often set the type for the operand, get that range.  */
  if (TYPE_P (op))
    r.set_range_for_type (op);
  else
    r.set_range_for_type (TREE_TYPE (op));
  return true;
}




