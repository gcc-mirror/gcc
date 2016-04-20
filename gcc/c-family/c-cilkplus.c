/* This file contains routines to construct and validate Cilk Plus
   constructs within the C and C++ front ends.

   Copyright (C) 2013-2016 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "c-common.h"

/* Validate the body of a _Cilk_for construct or a <#pragma simd> for
   loop.

   Returns true if there were no errors, false otherwise.  */

bool
c_check_cilk_loop (location_t loc, tree decl)
{
  if (TREE_THIS_VOLATILE (decl))
    {
      error_at (loc, "iteration variable cannot be volatile");
      return false;
    }
  return true;
}

/* Calculate number of iterations of CILK_FOR.  */

tree
cilk_for_number_of_iterations (tree cilk_for)
{
  tree t, v, n1, n2, step, type, init, cond, incr, itype;
  enum tree_code cond_code;
  location_t loc = EXPR_LOCATION (cilk_for);

  init = TREE_VEC_ELT (OMP_FOR_INIT (cilk_for), 0);
  v = TREE_OPERAND (init, 0);
  cond = TREE_VEC_ELT (OMP_FOR_COND (cilk_for), 0);
  incr = TREE_VEC_ELT (OMP_FOR_INCR (cilk_for), 0);
  type = TREE_TYPE (v);

  gcc_assert (TREE_CODE (TREE_TYPE (v)) == INTEGER_TYPE
	      || TREE_CODE (TREE_TYPE (v)) == POINTER_TYPE);
  n1 = TREE_OPERAND (init, 1);
  cond_code = TREE_CODE (cond);
  n2 = TREE_OPERAND (cond, 1);
  switch (cond_code)
    {
    case LT_EXPR:
    case GT_EXPR:
    case NE_EXPR:
      break;
    case LE_EXPR:
      if (POINTER_TYPE_P (TREE_TYPE (n2)))
	n2 = fold_build_pointer_plus_hwi_loc (loc, n2, 1);
      else
	n2 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (n2), n2,
			      build_int_cst (TREE_TYPE (n2), 1));
      cond_code = LT_EXPR;
      break;
    case GE_EXPR:
      if (POINTER_TYPE_P (TREE_TYPE (n2)))
	n2 = fold_build_pointer_plus_hwi_loc (loc, n2, -1);
      else
	n2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (n2), n2,
			      build_int_cst (TREE_TYPE (n2), 1));
      cond_code = GT_EXPR;
      break;
    default:
      gcc_unreachable ();
    }

  step = NULL_TREE;
  switch (TREE_CODE (incr))
    {
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      step = build_int_cst (TREE_TYPE (v), 1);
      break;
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      step = build_int_cst (TREE_TYPE (v), -1);
      break;
    case MODIFY_EXPR:
      t = TREE_OPERAND (incr, 1);
      gcc_assert (TREE_OPERAND (t, 0) == v);
      switch (TREE_CODE (t))
	{
	case PLUS_EXPR:
	  step = TREE_OPERAND (t, 1);
	  break;
	case POINTER_PLUS_EXPR:
	  step = fold_convert (ssizetype, TREE_OPERAND (t, 1));
	  break;
	case MINUS_EXPR:
	  step = TREE_OPERAND (t, 1);
	  step = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (step), step);
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
    default:
      gcc_unreachable ();
    }

  itype = type;
  if (POINTER_TYPE_P (itype))
    itype = signed_type_for (itype);
  if (cond_code == NE_EXPR)
    {
      /* For NE_EXPR, we need to find out if the iterator increases
	 or decreases from whether step is positive or negative.  */
      tree stype = itype;
      if (TYPE_UNSIGNED (stype))
	stype = signed_type_for (stype);
      cond = fold_build2_loc (loc, GE_EXPR, boolean_type_node,
			      fold_convert_loc (loc, stype, step),
			      build_int_cst (stype, 0));
      t = fold_build3_loc (loc, COND_EXPR, itype, cond,
			   build_int_cst (itype, -1),
			   build_int_cst (itype, 1));
    }
  else
    t = build_int_cst (itype, (cond_code == LT_EXPR ? -1 : 1));
  t = fold_build2_loc (loc, PLUS_EXPR, itype,
		       fold_convert_loc (loc, itype, step), t);
  t = fold_build2_loc (loc, PLUS_EXPR, itype, t,
		       fold_convert_loc (loc, itype, n2));
  t = fold_build2_loc (loc, MINUS_EXPR, itype, t,
		       fold_convert_loc (loc, itype, n1));
  if (TYPE_UNSIGNED (itype) && cond_code == GT_EXPR)
    t = fold_build2_loc (loc, TRUNC_DIV_EXPR, itype,
			 fold_build1_loc (loc, NEGATE_EXPR, itype, t),
			 fold_build1_loc (loc, NEGATE_EXPR, itype,
					  fold_convert_loc (loc, itype,
							    step)));
  else if (TYPE_UNSIGNED (itype) && cond_code == NE_EXPR)
    {
      tree t1
	= fold_build2_loc (loc, TRUNC_DIV_EXPR, itype, t,
			   fold_convert_loc (loc, itype, step));
      tree t2
	= fold_build2_loc (loc, TRUNC_DIV_EXPR, itype,
			   fold_build1_loc (loc, NEGATE_EXPR, itype, t),
			   fold_build1_loc (loc, NEGATE_EXPR, itype,
					    fold_convert_loc (loc, itype,
							      step)));
      t = fold_build3_loc (loc, COND_EXPR, itype, cond, t1, t2);
    }
  else
    t = fold_build2_loc (loc, TRUNC_DIV_EXPR, itype, t,
			 fold_convert_loc (loc, itype, step));
  cond = fold_build2_loc (loc, cond_code, boolean_type_node, n1, n2);
  t = fold_build3_loc (loc, COND_EXPR, itype, cond, t,
		       build_int_cst (itype, 0));
  return t;
}
