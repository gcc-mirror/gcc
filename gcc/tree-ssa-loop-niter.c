/* Functions to determine/estimate number of iterations of a loop.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.
   
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "diagnostic.h"
#include "intl.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "ggc.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "tree-data-ref.h"
#include "params.h"
#include "flags.h"
#include "toplev.h"
#include "tree-inline.h"

#define SWAP(X, Y) do { void *tmp = (X); (X) = (Y); (Y) = tmp; } while (0)


/*

   Analysis of number of iterations of an affine exit test.

*/

/* Returns true if ARG is either NULL_TREE or constant zero.  Unlike
   integer_zerop, it does not care about overflow flags.  */

bool
zero_p (tree arg)
{
  if (!arg)
    return true;

  if (TREE_CODE (arg) != INTEGER_CST)
    return false;

  return (TREE_INT_CST_LOW (arg) == 0 && TREE_INT_CST_HIGH (arg) == 0);
}

/* Returns true if ARG a nonzero constant.  Unlike integer_nonzerop, it does
   not care about overflow flags.  */

static bool
nonzero_p (tree arg)
{
  if (!arg)
    return false;

  if (TREE_CODE (arg) != INTEGER_CST)
    return false;

  return (TREE_INT_CST_LOW (arg) != 0 || TREE_INT_CST_HIGH (arg) != 0);
}

/* Returns inverse of X modulo 2^s, where MASK = 2^s-1.  */

static tree
inverse (tree x, tree mask)
{
  tree type = TREE_TYPE (x);
  tree rslt;
  unsigned ctr = tree_floor_log2 (mask);

  if (TYPE_PRECISION (type) <= HOST_BITS_PER_WIDE_INT)
    {
      unsigned HOST_WIDE_INT ix;
      unsigned HOST_WIDE_INT imask;
      unsigned HOST_WIDE_INT irslt = 1;

      gcc_assert (cst_and_fits_in_hwi (x));
      gcc_assert (cst_and_fits_in_hwi (mask));

      ix = int_cst_value (x);
      imask = int_cst_value (mask);

      for (; ctr; ctr--)
	{
	  irslt *= ix;
	  ix *= ix;
	}
      irslt &= imask;

      rslt = build_int_cst_type (type, irslt);
    }
  else
    {
      rslt = build_int_cst_type (type, 1);
      for (; ctr; ctr--)
	{
	  rslt = int_const_binop (MULT_EXPR, rslt, x, 0);
	  x = int_const_binop (MULT_EXPR, x, x, 0);
	}
      rslt = int_const_binop (BIT_AND_EXPR, rslt, mask, 0);
    }

  return rslt;
}

/* Determine the number of iterations according to condition (for staying
   inside loop) which compares two induction variables using comparison
   operator CODE.  The induction variable on left side of the comparison
   has base BASE0 and step STEP0. the right-hand side one has base
   BASE1 and step STEP1.  Both induction variables must have type TYPE,
   which must be an integer or pointer type.  STEP0 and STEP1 must be
   constants (or NULL_TREE, which is interpreted as constant zero).
   
   The results (number of iterations and assumptions as described in
   comments at struct tree_niter_desc in tree-flow.h) are stored to NITER.
   In case we are unable to determine number of iterations, contents of
   this structure is unchanged.  */

static void
number_of_iterations_cond (tree type, tree base0, tree step0,
			   enum tree_code code, tree base1, tree step1,
			   struct tree_niter_desc *niter)
{
  tree step, delta, mmin, mmax;
  tree may_xform, bound, s, d, tmp;
  bool was_sharp = false;
  tree assumption;
  tree assumptions = boolean_true_node;
  tree noloop_assumptions = boolean_false_node;
  tree niter_type, signed_niter_type;
  tree bits;

  /* The meaning of these assumptions is this:
     if !assumptions
       then the rest of information does not have to be valid
     if noloop_assumptions then the loop does not have to roll
       (but it is only conservative approximation, i.e. it only says that
       if !noloop_assumptions, then the loop does not end before the computed
       number of iterations)  */

  /* Make < comparison from > ones.  */
  if (code == GE_EXPR
      || code == GT_EXPR)
    {
      SWAP (base0, base1);
      SWAP (step0, step1);
      code = swap_tree_comparison (code);
    }

  /* We can handle the case when neither of the sides of the comparison is
     invariant, provided that the test is NE_EXPR.  This rarely occurs in
     practice, but it is simple enough to manage.  */
  if (!zero_p (step0) && !zero_p (step1))
    {
      if (code != NE_EXPR)
	return;

      step0 = fold_binary_to_constant (MINUS_EXPR, type, step0, step1);
      step1 = NULL_TREE;
    }

  /* If the result is a constant,  the loop is weird.  More precise handling
     would be possible, but the situation is not common enough to waste time
     on it.  */
  if (zero_p (step0) && zero_p (step1))
    return;

  /* Ignore loops of while (i-- < 10) type.  */
  if (code != NE_EXPR)
    {
      if (step0 && tree_int_cst_sign_bit (step0))
	return;

      if (!zero_p (step1) && !tree_int_cst_sign_bit (step1))
	return;
    }

  if (POINTER_TYPE_P (type))
    {
      /* We assume pointer arithmetic never overflows.  */
      mmin = mmax = NULL_TREE;
    }
  else
    {
      mmin = TYPE_MIN_VALUE (type);
      mmax = TYPE_MAX_VALUE (type);
    }

  /* Some more condition normalization.  We must record some assumptions
     due to overflows.  */

  if (code == LT_EXPR)
    {
      /* We want to take care only of <=; this is easy,
	 as in cases the overflow would make the transformation unsafe the loop
	 does not roll.  Seemingly it would make more sense to want to take
	 care of <, as NE is more similar to it, but the problem is that here
	 the transformation would be more difficult due to possibly infinite
	 loops.  */
      if (zero_p (step0))
	{
	  if (mmax)
	    assumption = fold_build2 (EQ_EXPR, boolean_type_node, base0, mmax);
	  else
	    assumption = boolean_false_node;
	  if (nonzero_p (assumption))
	    goto zero_iter;
	  base0 = fold_build2 (PLUS_EXPR, type, base0,
			       build_int_cst_type (type, 1));
	}
      else
	{
	  if (mmin)
	    assumption = fold_build2 (EQ_EXPR, boolean_type_node, base1, mmin);
	  else
	    assumption = boolean_false_node;
	  if (nonzero_p (assumption))
	    goto zero_iter;
	  base1 = fold_build2 (MINUS_EXPR, type, base1,
			       build_int_cst_type (type, 1));
	}
      noloop_assumptions = assumption;
      code = LE_EXPR;

      /* It will be useful to be able to tell the difference once more in
	 <= -> != reduction.  */
      was_sharp = true;
    }

  /* Take care of trivially infinite loops.  */
  if (code != NE_EXPR)
    {
      if (zero_p (step0)
	  && mmin
	  && operand_equal_p (base0, mmin, 0))
	return;
      if (zero_p (step1)
	  && mmax
	  && operand_equal_p (base1, mmax, 0))
	return;
    }

  /* If we can we want to take care of NE conditions instead of size
     comparisons, as they are much more friendly (most importantly
     this takes care of special handling of loops with step 1).  We can
     do it if we first check that upper bound is greater or equal to
     lower bound, their difference is constant c modulo step and that
     there is not an overflow.  */
  if (code != NE_EXPR)
    {
      if (zero_p (step0))
	step = fold_unary_to_constant (NEGATE_EXPR, type, step1);
      else
	step = step0;
      delta = fold_build2 (MINUS_EXPR, type, base1, base0);
      delta = fold_build2 (FLOOR_MOD_EXPR, type, delta, step);
      may_xform = boolean_false_node;

      if (TREE_CODE (delta) == INTEGER_CST)
	{
	  tmp = fold_binary_to_constant (MINUS_EXPR, type, step,
					 build_int_cst_type (type, 1));
	  if (was_sharp
	      && operand_equal_p (delta, tmp, 0))
	    {
	      /* A special case.  We have transformed condition of type
		 for (i = 0; i < 4; i += 4)
		 into
		 for (i = 0; i <= 3; i += 4)
		 obviously if the test for overflow during that transformation
		 passed, we cannot overflow here.  Most importantly any
		 loop with sharp end condition and step 1 falls into this
		 category, so handling this case specially is definitely
		 worth the troubles.  */
	      may_xform = boolean_true_node;
	    }
	  else if (zero_p (step0))
	    {
	      if (!mmin)
		may_xform = boolean_true_node;
	      else
		{
		  bound = fold_binary_to_constant (PLUS_EXPR, type,
						   mmin, step);
		  bound = fold_binary_to_constant (MINUS_EXPR, type,
						   bound, delta);
		  may_xform = fold_build2 (LE_EXPR, boolean_type_node,
					   bound, base0);
		}
	    }
	  else
	    {
	      if (!mmax)
		may_xform = boolean_true_node;
	      else
		{
		  bound = fold_binary_to_constant (MINUS_EXPR, type,
						   mmax, step);
		  bound = fold_binary_to_constant (PLUS_EXPR, type,
						   bound, delta);
		  may_xform = fold_build2 (LE_EXPR, boolean_type_node,
					   base1, bound);
		}
	    }
	}

      if (!zero_p (may_xform))
	{
	  /* We perform the transformation always provided that it is not
	     completely senseless.  This is OK, as we would need this assumption
	     to determine the number of iterations anyway.  */
	  if (!nonzero_p (may_xform))
	    assumptions = may_xform;

	  if (zero_p (step0))
	    {
	      base0 = fold_build2 (PLUS_EXPR, type, base0, delta);
	      base0 = fold_build2 (MINUS_EXPR, type, base0, step);
	    }
	  else
	    {
	      base1 = fold_build2 (MINUS_EXPR, type, base1, delta);
	      base1 = fold_build2 (PLUS_EXPR, type, base1, step);
	    }

	  assumption = fold_build2 (GT_EXPR, boolean_type_node, base0, base1);
	  noloop_assumptions = fold_build2 (TRUTH_OR_EXPR, boolean_type_node,
					    noloop_assumptions, assumption);
	  code = NE_EXPR;
	}
    }

  /* Count the number of iterations.  */
  niter_type = unsigned_type_for (type);
  signed_niter_type = signed_type_for (type);

  if (code == NE_EXPR)
    {
      /* Everything we do here is just arithmetics modulo size of mode.  This
	 makes us able to do more involved computations of number of iterations
	 than in other cases.  First transform the condition into shape
	 s * i <> c, with s positive.  */
      base1 = fold_build2 (MINUS_EXPR, type, base1, base0);
      base0 = NULL_TREE;
      if (!zero_p (step1))
  	step0 = fold_unary_to_constant (NEGATE_EXPR, type, step1);
      step1 = NULL_TREE;
      if (tree_int_cst_sign_bit (fold_convert (signed_niter_type, step0)))
	{
	  step0 = fold_unary_to_constant (NEGATE_EXPR, type, step0);
	  base1 = fold_build1 (NEGATE_EXPR, type, base1);
	}

      base1 = fold_convert (niter_type, base1);
      step0 = fold_convert (niter_type, step0);

      /* Let nsd (step, size of mode) = d.  If d does not divide c, the loop
	 is infinite.  Otherwise, the number of iterations is
	 (inverse(s/d) * (c/d)) mod (size of mode/d).  */
      bits = num_ending_zeros (step0);
      d = fold_binary_to_constant (LSHIFT_EXPR, niter_type,
				   build_int_cst_type (niter_type, 1), bits);
      s = fold_binary_to_constant (RSHIFT_EXPR, niter_type, step0, bits);

      bound = build_low_bits_mask (niter_type,
				   (TYPE_PRECISION (niter_type)
				    - tree_low_cst (bits, 1)));

      assumption = fold_build2 (FLOOR_MOD_EXPR, niter_type, base1, d);
      assumption = fold_build2 (EQ_EXPR, boolean_type_node,
				assumption,
				build_int_cst (niter_type, 0));
      assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				 assumptions, assumption);

      tmp = fold_build2 (EXACT_DIV_EXPR, niter_type, base1, d);
      tmp = fold_build2 (MULT_EXPR, niter_type, tmp, inverse (s, bound));
      niter->niter = fold_build2 (BIT_AND_EXPR, niter_type, tmp, bound);
    }
  else
    {
      if (zero_p (step1))
	/* Condition in shape a + s * i <= b
	   We must know that b + s does not overflow and a <= b + s and then we
	   can compute number of iterations as (b + s - a) / s.  (It might
	   seem that we in fact could be more clever about testing the b + s
	   overflow condition using some information about b - a mod s,
	   but it was already taken into account during LE -> NE transform).  */
	{
	  if (mmax)
	    {
	      bound = fold_binary_to_constant (MINUS_EXPR, type, mmax, step0);
	      assumption = fold_build2 (LE_EXPR, boolean_type_node,
					base1, bound);
	      assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
					 assumptions, assumption);
	    }

	  step = step0;
	  tmp = fold_build2 (PLUS_EXPR, type, base1, step0);
	  assumption = fold_build2 (GT_EXPR, boolean_type_node, base0, tmp);
	  delta = fold_build2 (PLUS_EXPR, type, base1, step);
	  delta = fold_build2 (MINUS_EXPR, type, delta, base0);
	  delta = fold_convert (niter_type, delta);
	}
      else
	{
	  /* Condition in shape a <= b - s * i
	     We must know that a - s does not overflow and a - s <= b and then
	     we can again compute number of iterations as (b - (a - s)) / s.  */
	  if (mmin)
	    {
	      bound = fold_binary_to_constant (MINUS_EXPR, type, mmin, step1);
	      assumption = fold_build2 (LE_EXPR, boolean_type_node,
					bound, base0);
	      assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
					 assumptions, assumption);
	    }
	  step = fold_build1 (NEGATE_EXPR, type, step1);
	  tmp = fold_build2 (PLUS_EXPR, type, base0, step1);
	  assumption = fold_build2 (GT_EXPR, boolean_type_node, tmp, base1);
	  delta = fold_build2 (MINUS_EXPR, type, base0, step);
	  delta = fold_build2 (MINUS_EXPR, type, base1, delta);
	  delta = fold_convert (niter_type, delta);
	}
      noloop_assumptions = fold_build2 (TRUTH_OR_EXPR, boolean_type_node,
					noloop_assumptions, assumption);
      delta = fold_build2 (FLOOR_DIV_EXPR, niter_type, delta,
			   fold_convert (niter_type, step));
      niter->niter = delta;
    }

  niter->assumptions = assumptions;
  niter->may_be_zero = noloop_assumptions;
  return;

zero_iter:
  niter->assumptions = boolean_true_node;
  niter->may_be_zero = boolean_true_node;
  niter->niter = build_int_cst_type (type, 0);
  return;
}


/* Similar to number_of_iterations_cond, but only handles the special
   case of loops with step 1 or -1.  The meaning of the arguments
   is the same as in number_of_iterations_cond.  The function
   returns true if the special case was recognized, false otherwise.  */

static bool
number_of_iterations_special (tree type, tree base0, tree step0,
			      enum tree_code code, tree base1, tree step1,
			      struct tree_niter_desc *niter)
{
  tree niter_type = unsigned_type_for (type), mmax, mmin;

  /* Make < comparison from > ones.  */
  if (code == GE_EXPR
      || code == GT_EXPR)
    {
      SWAP (base0, base1);
      SWAP (step0, step1);
      code = swap_tree_comparison (code);
    }

  switch (code)
    {
    case NE_EXPR:
      if (zero_p (step0))
	{
	  if (zero_p (step1))
	    return false;
    	  SWAP (base0, base1);
	  SWAP (step0, step1);
	}
      else if (!zero_p (step1))
	return false;

      if (integer_onep (step0))
	{
	  /* for (i = base0; i != base1; i++)  */
	  niter->assumptions = boolean_true_node;
	  niter->may_be_zero = boolean_false_node;
	  niter->niter = fold_build2 (MINUS_EXPR, type, base1, base0);
	  niter->additional_info = boolean_true_node;
	}
      else if (integer_all_onesp (step0))
	{
	  /* for (i = base0; i != base1; i--)  */
	  niter->assumptions = boolean_true_node;
	  niter->may_be_zero = boolean_false_node;
	  niter->niter = fold_build2 (MINUS_EXPR, type, base0, base1);
	}
      else
	return false;

      break;

    case LT_EXPR:
      if ((step0 && integer_onep (step0) && zero_p (step1))
	  || (step1 && integer_all_onesp (step1) && zero_p (step0)))
	{
	  /* for (i = base0; i < base1; i++)
	     
	     or

	     for (i = base1; i > base0; i--).
	     
	     In both cases # of iterations is base1 - base0.  */

	  niter->assumptions = boolean_true_node;
	  niter->may_be_zero = fold_build2 (GT_EXPR, boolean_type_node,
					    base0, base1);
	  niter->niter = fold_build2 (MINUS_EXPR, type, base1, base0);
	}
      else
	return false;
      break;

    case LE_EXPR:
      if (POINTER_TYPE_P (type))
	{
	  /* We assume pointer arithmetic never overflows.  */
	  mmin = mmax = NULL_TREE;
	}
      else
	{
	  mmin = TYPE_MIN_VALUE (type);
	  mmax = TYPE_MAX_VALUE (type);
	}

      if (step0 && integer_onep (step0) && zero_p (step1))
	{
	  /* for (i = base0; i <= base1; i++)  */
	  if (mmax)
	    niter->assumptions = fold_build2 (NE_EXPR, boolean_type_node,
					      base1, mmax);
	  else
	    niter->assumptions = boolean_true_node;
	  base1 = fold_build2 (PLUS_EXPR, type, base1,
			       build_int_cst_type (type, 1));
	}
      else if (step1 && integer_all_onesp (step1) && zero_p (step0))
	{
	  /* for (i = base1; i >= base0; i--)  */
	  if (mmin)
	    niter->assumptions = fold_build2 (NE_EXPR, boolean_type_node,
					      base0, mmin);
	  else
	    niter->assumptions = boolean_true_node;
	  base0 = fold_build2 (MINUS_EXPR, type, base0,
			       build_int_cst_type (type, 1));
	}
      else
	return false;

      niter->may_be_zero = fold_build2 (GT_EXPR, boolean_type_node,
					base0, base1);
      niter->niter = fold_build2 (MINUS_EXPR, type, base1, base0);
      break;

    default:
      gcc_unreachable ();
    }

  niter->niter = fold_convert (niter_type, niter->niter);
  niter->additional_info = boolean_true_node;
  return true;
}

/* Substitute NEW for OLD in EXPR and fold the result.  */

static tree
simplify_replace_tree (tree expr, tree old, tree new)
{
  unsigned i, n;
  tree ret = NULL_TREE, e, se;

  if (!expr)
    return NULL_TREE;

  if (expr == old
      || operand_equal_p (expr, old, 0))
    return unshare_expr (new);

  if (!EXPR_P (expr))
    return expr;

  n = TREE_CODE_LENGTH (TREE_CODE (expr));
  for (i = 0; i < n; i++)
    {
      e = TREE_OPERAND (expr, i);
      se = simplify_replace_tree (e, old, new);
      if (e == se)
	continue;

      if (!ret)
	ret = copy_node (expr);

      TREE_OPERAND (ret, i) = se;
    }

  return (ret ? fold (ret) : expr);
}

/* Expand definitions of ssa names in EXPR as long as they are simple
   enough, and return the new expression.  */

tree
expand_simple_operations (tree expr)
{
  unsigned i, n;
  tree ret = NULL_TREE, e, ee, stmt;
  enum tree_code code = TREE_CODE (expr);

  if (is_gimple_min_invariant (expr))
    return expr;

  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
    {
      n = TREE_CODE_LENGTH (code);
      for (i = 0; i < n; i++)
	{
	  e = TREE_OPERAND (expr, i);
	  ee = expand_simple_operations (e);
	  if (e == ee)
	    continue;

	  if (!ret)
	    ret = copy_node (expr);

	  TREE_OPERAND (ret, i) = ee;
	}

      return (ret ? fold (ret) : expr);
    }

  if (TREE_CODE (expr) != SSA_NAME)
    return expr;

  stmt = SSA_NAME_DEF_STMT (expr);
  if (TREE_CODE (stmt) != MODIFY_EXPR)
    return expr;

  e = TREE_OPERAND (stmt, 1);
  if (/* Casts are simple.  */
      TREE_CODE (e) != NOP_EXPR
      && TREE_CODE (e) != CONVERT_EXPR
      /* Copies are simple.  */
      && TREE_CODE (e) != SSA_NAME
      /* Assignments of invariants are simple.  */
      && !is_gimple_min_invariant (e)
      /* And increments and decrements by a constant are simple.  */
      && !((TREE_CODE (e) == PLUS_EXPR
	    || TREE_CODE (e) == MINUS_EXPR)
	   && is_gimple_min_invariant (TREE_OPERAND (e, 1))))
    return expr;

  return expand_simple_operations (e);
}

/* Tries to simplify EXPR using the condition COND.  Returns the simplified
   expression (or EXPR unchanged, if no simplification was possible).  */

static tree
tree_simplify_using_condition_1 (tree cond, tree expr)
{
  bool changed;
  tree e, te, e0, e1, e2, notcond;
  enum tree_code code = TREE_CODE (expr);

  if (code == INTEGER_CST)
    return expr;

  if (code == TRUTH_OR_EXPR
      || code == TRUTH_AND_EXPR
      || code == COND_EXPR)
    {
      changed = false;

      e0 = tree_simplify_using_condition_1 (cond, TREE_OPERAND (expr, 0));
      if (TREE_OPERAND (expr, 0) != e0)
	changed = true;

      e1 = tree_simplify_using_condition_1 (cond, TREE_OPERAND (expr, 1));
      if (TREE_OPERAND (expr, 1) != e1)
	changed = true;

      if (code == COND_EXPR)
	{
	  e2 = tree_simplify_using_condition_1 (cond, TREE_OPERAND (expr, 2));
	  if (TREE_OPERAND (expr, 2) != e2)
	    changed = true;
	}
      else
	e2 = NULL_TREE;

      if (changed)
	{
	  if (code == COND_EXPR)
	    expr = fold_build3 (code, boolean_type_node, e0, e1, e2);
	  else
	    expr = fold_build2 (code, boolean_type_node, e0, e1);
	}

      return expr;
    }

  /* In case COND is equality, we may be able to simplify EXPR by copy/constant
     propagation, and vice versa.  Fold does not handle this, since it is
     considered too expensive.  */
  if (TREE_CODE (cond) == EQ_EXPR)
    {
      e0 = TREE_OPERAND (cond, 0);
      e1 = TREE_OPERAND (cond, 1);

      /* We know that e0 == e1.  Check whether we cannot simplify expr
	 using this fact.  */
      e = simplify_replace_tree (expr, e0, e1);
      if (zero_p (e) || nonzero_p (e))
	return e;

      e = simplify_replace_tree (expr, e1, e0);
      if (zero_p (e) || nonzero_p (e))
	return e;
    }
  if (TREE_CODE (expr) == EQ_EXPR)
    {
      e0 = TREE_OPERAND (expr, 0);
      e1 = TREE_OPERAND (expr, 1);

      /* If e0 == e1 (EXPR) implies !COND, then EXPR cannot be true.  */
      e = simplify_replace_tree (cond, e0, e1);
      if (zero_p (e))
	return e;
      e = simplify_replace_tree (cond, e1, e0);
      if (zero_p (e))
	return e;
    }
  if (TREE_CODE (expr) == NE_EXPR)
    {
      e0 = TREE_OPERAND (expr, 0);
      e1 = TREE_OPERAND (expr, 1);

      /* If e0 == e1 (!EXPR) implies !COND, then EXPR must be true.  */
      e = simplify_replace_tree (cond, e0, e1);
      if (zero_p (e))
	return boolean_true_node;
      e = simplify_replace_tree (cond, e1, e0);
      if (zero_p (e))
	return boolean_true_node;
    }

  te = expand_simple_operations (expr);

  /* Check whether COND ==> EXPR.  */
  notcond = invert_truthvalue (cond);
  e = fold_binary (TRUTH_OR_EXPR, boolean_type_node, notcond, te);
  if (nonzero_p (e))
    return e;

  /* Check whether COND ==> not EXPR.  */
  e = fold_binary (TRUTH_AND_EXPR, boolean_type_node, cond, te);
  if (e && zero_p (e))
    return e;

  return expr;
}

/* Tries to simplify EXPR using the condition COND.  Returns the simplified
   expression (or EXPR unchanged, if no simplification was possible).
   Wrapper around tree_simplify_using_condition_1 that ensures that chains
   of simple operations in definitions of ssa names in COND are expanded,
   so that things like casts or incrementing the value of the bound before
   the loop do not cause us to fail.  */

static tree
tree_simplify_using_condition (tree cond, tree expr)
{
  cond = expand_simple_operations (cond);

  return tree_simplify_using_condition_1 (cond, expr);
}
     
/* Tries to simplify EXPR using the conditions on entry to LOOP.
   Record the conditions used for simplification to CONDS_USED.
   Returns the simplified expression (or EXPR unchanged, if no
   simplification was possible).*/

static tree
simplify_using_initial_conditions (struct loop *loop, tree expr,
				   tree *conds_used)
{
  edge e;
  basic_block bb;
  tree exp, cond;

  if (TREE_CODE (expr) == INTEGER_CST)
    return expr;

  for (bb = loop->header;
       bb != ENTRY_BLOCK_PTR;
       bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      if (!single_pred_p (bb))
	continue;
      e = single_pred_edge (bb);

      if (!(e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
	continue;

      cond = COND_EXPR_COND (last_stmt (e->src));
      if (e->flags & EDGE_FALSE_VALUE)
	cond = invert_truthvalue (cond);
      exp = tree_simplify_using_condition (cond, expr);

      if (exp != expr)
	*conds_used = fold_build2 (TRUTH_AND_EXPR,
				   boolean_type_node,
				   *conds_used,
				   cond);

      expr = exp;
    }

  return expr;
}

/* Tries to simplify EXPR using the evolutions of the loop invariants
   in the superloops of LOOP.  Returns the simplified expression
   (or EXPR unchanged, if no simplification was possible).  */

static tree
simplify_using_outer_evolutions (struct loop *loop, tree expr)
{
  enum tree_code code = TREE_CODE (expr);
  bool changed;
  tree e, e0, e1, e2;

  if (is_gimple_min_invariant (expr))
    return expr;

  if (code == TRUTH_OR_EXPR
      || code == TRUTH_AND_EXPR
      || code == COND_EXPR)
    {
      changed = false;

      e0 = simplify_using_outer_evolutions (loop, TREE_OPERAND (expr, 0));
      if (TREE_OPERAND (expr, 0) != e0)
	changed = true;

      e1 = simplify_using_outer_evolutions (loop, TREE_OPERAND (expr, 1));
      if (TREE_OPERAND (expr, 1) != e1)
	changed = true;

      if (code == COND_EXPR)
	{
	  e2 = simplify_using_outer_evolutions (loop, TREE_OPERAND (expr, 2));
	  if (TREE_OPERAND (expr, 2) != e2)
	    changed = true;
	}
      else
	e2 = NULL_TREE;

      if (changed)
	{
	  if (code == COND_EXPR)
	    expr = fold_build3 (code, boolean_type_node, e0, e1, e2);
	  else
	    expr = fold_build2 (code, boolean_type_node, e0, e1);
	}

      return expr;
    }

  e = instantiate_parameters (loop, expr);
  if (is_gimple_min_invariant (e))
    return e;

  return expr;
}

/* Stores description of number of iterations of LOOP derived from
   EXIT (an exit edge of the LOOP) in NITER.  Returns true if some
   useful information could be derived (and fields of NITER has
   meaning described in comments at struct tree_niter_desc
   declaration), false otherwise.  If WARN is true and
   -Wunsafe-loop-optimizations was given, warn if the optimizer is going to use
   potentially unsafe assumptions.  */

bool
number_of_iterations_exit (struct loop *loop, edge exit,
			   struct tree_niter_desc *niter,
			   bool warn)
{
  tree stmt, cond, type;
  tree op0, base0, step0;
  tree op1, base1, step1;
  enum tree_code code;

  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, exit->src))
    return false;

  niter->assumptions = boolean_false_node;
  stmt = last_stmt (exit->src);
  if (!stmt || TREE_CODE (stmt) != COND_EXPR)
    return false;

  /* We want the condition for staying inside loop.  */
  cond = COND_EXPR_COND (stmt);
  if (exit->flags & EDGE_TRUE_VALUE)
    cond = invert_truthvalue (cond);

  code = TREE_CODE (cond);
  switch (code)
    {
    case GT_EXPR:
    case GE_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
      break;

    default:
      return false;
    }
  
  op0 = TREE_OPERAND (cond, 0);
  op1 = TREE_OPERAND (cond, 1);
  type = TREE_TYPE (op0);

  if (TREE_CODE (type) != INTEGER_TYPE
      && !POINTER_TYPE_P (type))
    return false;
     
  if (!simple_iv (loop, stmt, op0, &base0, &step0, false))
    return false;
  if (!simple_iv (loop, stmt, op1, &base1, &step1, false))
    return false;

  niter->niter = NULL_TREE;

  /* Handle common special cases first, so that we do not need to use
     generic (and slow) analysis very often.  */
  if (!number_of_iterations_special (type, base0, step0, code, base1, step1,
				     niter))
    {

      number_of_iterations_cond (type, base0, step0, code, base1, step1,
				 niter);

      if (!niter->niter)
	return false;
    }

  if (optimize >= 3)
    {
      niter->assumptions = simplify_using_outer_evolutions (loop,
							    niter->assumptions);
      niter->may_be_zero = simplify_using_outer_evolutions (loop,
							    niter->may_be_zero);
      niter->niter = simplify_using_outer_evolutions (loop, niter->niter);
    }

  niter->additional_info = boolean_true_node;
  niter->assumptions
	  = simplify_using_initial_conditions (loop,
					       niter->assumptions,
					       &niter->additional_info);
  niter->may_be_zero
	  = simplify_using_initial_conditions (loop,
					       niter->may_be_zero,
					       &niter->additional_info);

  if (integer_onep (niter->assumptions))
    return true;

  /* With -funsafe-loop-optimizations we assume that nothing bad can happen.
     But if we can prove that there is overflow or some other source of weird
     behavior, ignore the loop even with -funsafe-loop-optimizations.  */
  if (integer_zerop (niter->assumptions))
    return false;

  if (flag_unsafe_loop_optimizations)
    niter->assumptions = boolean_true_node;

  if (warn)
    {
      const char *wording;
      location_t loc = EXPR_LOCATION (stmt);
  
      /* We can provide a more specific warning if one of the operator is
	 constant and the other advances by +1 or -1.  */
      if (step1 ? !step0 && (integer_onep (step1) || integer_all_onesp (step1))
	  	: step0 && (integer_onep (step0) || integer_all_onesp (step0)))
        wording =
          flag_unsafe_loop_optimizations
          ? N_("assuming that the loop is not infinite")
          : N_("cannot optimize possibly infinite loops");
      else
	wording = 
	  flag_unsafe_loop_optimizations
	  ? N_("assuming that the loop counter does not overflow")
	  : N_("cannot optimize loop, the loop counter may overflow");

      if (LOCATION_LINE (loc) > 0)
	warning (OPT_Wunsafe_loop_optimizations, "%H%s", &loc, gettext (wording));
      else
	warning (OPT_Wunsafe_loop_optimizations, "%s", gettext (wording));
    }

  return flag_unsafe_loop_optimizations;
}

/* Try to determine the number of iterations of LOOP.  If we succeed,
   expression giving number of iterations is returned and *EXIT is
   set to the edge from that the information is obtained.  Otherwise
   chrec_dont_know is returned.  */

tree
find_loop_niter (struct loop *loop, edge *exit)
{
  unsigned n_exits, i;
  edge *exits = get_loop_exit_edges (loop, &n_exits);
  edge ex;
  tree niter = NULL_TREE, aniter;
  struct tree_niter_desc desc;

  *exit = NULL;
  for (i = 0; i < n_exits; i++)
    {
      ex = exits[i];
      if (!just_once_each_iteration_p (loop, ex->src))
	continue;

      if (!number_of_iterations_exit (loop, ex, &desc, false))
	continue;

      if (nonzero_p (desc.may_be_zero))
	{
	  /* We exit in the first iteration through this exit.
	     We won't find anything better.  */
	  niter = build_int_cst_type (unsigned_type_node, 0);
	  *exit = ex;
	  break;
	}

      if (!zero_p (desc.may_be_zero))
	continue;

      aniter = desc.niter;

      if (!niter)
	{
	  /* Nothing recorded yet.  */
	  niter = aniter;
	  *exit = ex;
	  continue;
	}

      /* Prefer constants, the lower the better.  */
      if (TREE_CODE (aniter) != INTEGER_CST)
	continue;

      if (TREE_CODE (niter) != INTEGER_CST)
	{
	  niter = aniter;
	  *exit = ex;
	  continue;
	}

      if (tree_int_cst_lt (aniter, niter))
	{
	  niter = aniter;
	  *exit = ex;
	  continue;
	}
    }
  free (exits);

  return niter ? niter : chrec_dont_know;
}

/*

   Analysis of a number of iterations of a loop by a brute-force evaluation.

*/

/* Bound on the number of iterations we try to evaluate.  */

#define MAX_ITERATIONS_TO_TRACK \
  ((unsigned) PARAM_VALUE (PARAM_MAX_ITERATIONS_TO_TRACK))

/* Returns the loop phi node of LOOP such that ssa name X is derived from its
   result by a chain of operations such that all but exactly one of their
   operands are constants.  */

static tree
chain_of_csts_start (struct loop *loop, tree x)
{
  tree stmt = SSA_NAME_DEF_STMT (x);
  tree use;
  basic_block bb = bb_for_stmt (stmt);

  if (!bb
      || !flow_bb_inside_loop_p (loop, bb))
    return NULL_TREE;
  
  if (TREE_CODE (stmt) == PHI_NODE)
    {
      if (bb == loop->header)
	return stmt;

      return NULL_TREE;
    }

  if (TREE_CODE (stmt) != MODIFY_EXPR)
    return NULL_TREE;

  if (!ZERO_SSA_OPERANDS (stmt, SSA_OP_ALL_VIRTUALS))
    return NULL_TREE;
  if (SINGLE_SSA_DEF_OPERAND (stmt, SSA_OP_DEF) == NULL_DEF_OPERAND_P)
    return NULL_TREE;

  use = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_USE);
  if (use == NULL_USE_OPERAND_P)
    return NULL_TREE;

  return chain_of_csts_start (loop, use);
}

/* Determines whether the expression X is derived from a result of a phi node
   in header of LOOP such that

   * the derivation of X consists only from operations with constants
   * the initial value of the phi node is constant
   * the value of the phi node in the next iteration can be derived from the
     value in the current iteration by a chain of operations with constants.
   
   If such phi node exists, it is returned.  If X is a constant, X is returned
   unchanged.  Otherwise NULL_TREE is returned.  */

static tree
get_base_for (struct loop *loop, tree x)
{
  tree phi, init, next;

  if (is_gimple_min_invariant (x))
    return x;

  phi = chain_of_csts_start (loop, x);
  if (!phi)
    return NULL_TREE;

  init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
  next = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));

  if (TREE_CODE (next) != SSA_NAME)
    return NULL_TREE;

  if (!is_gimple_min_invariant (init))
    return NULL_TREE;

  if (chain_of_csts_start (loop, next) != phi)
    return NULL_TREE;

  return phi;
}

/* Given an expression X, then 
 
   * if BASE is NULL_TREE, X must be a constant and we return X.
   * otherwise X is a SSA name, whose value in the considered loop is derived
     by a chain of operations with constant from a result of a phi node in
     the header of the loop.  Then we return value of X when the value of the
     result of this phi node is given by the constant BASE.  */

static tree
get_val_for (tree x, tree base)
{
  tree stmt, nx, val;
  use_operand_p op;
  ssa_op_iter iter;

  if (!x)
    return base;

  stmt = SSA_NAME_DEF_STMT (x);
  if (TREE_CODE (stmt) == PHI_NODE)
    return base;

  FOR_EACH_SSA_USE_OPERAND (op, stmt, iter, SSA_OP_USE)
    {
      nx = USE_FROM_PTR (op);
      val = get_val_for (nx, base);
      SET_USE (op, val);
      val = fold (TREE_OPERAND (stmt, 1));
      SET_USE (op, nx);
      /* only iterate loop once.  */
      return val;
    }

  /* Should never reach here.  */
  gcc_unreachable();
}

/* Tries to count the number of iterations of LOOP till it exits by EXIT
   by brute force -- i.e. by determining the value of the operands of the
   condition at EXIT in first few iterations of the loop (assuming that
   these values are constant) and determining the first one in that the
   condition is not satisfied.  Returns the constant giving the number
   of the iterations of LOOP if successful, chrec_dont_know otherwise.  */

tree
loop_niter_by_eval (struct loop *loop, edge exit)
{
  tree cond, cnd, acnd;
  tree op[2], val[2], next[2], aval[2], phi[2];
  unsigned i, j;
  enum tree_code cmp;

  cond = last_stmt (exit->src);
  if (!cond || TREE_CODE (cond) != COND_EXPR)
    return chrec_dont_know;

  cnd = COND_EXPR_COND (cond);
  if (exit->flags & EDGE_TRUE_VALUE)
    cnd = invert_truthvalue (cnd);

  cmp = TREE_CODE (cnd);
  switch (cmp)
    {
    case EQ_EXPR:
    case NE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
      for (j = 0; j < 2; j++)
	op[j] = TREE_OPERAND (cnd, j);
      break;

    default:
      return chrec_dont_know;
    }

  for (j = 0; j < 2; j++)
    {
      phi[j] = get_base_for (loop, op[j]);
      if (!phi[j])
	return chrec_dont_know;
    }

  for (j = 0; j < 2; j++)
    {
      if (TREE_CODE (phi[j]) == PHI_NODE)
	{
	  val[j] = PHI_ARG_DEF_FROM_EDGE (phi[j], loop_preheader_edge (loop));
	  next[j] = PHI_ARG_DEF_FROM_EDGE (phi[j], loop_latch_edge (loop));
	}
      else
	{
	  val[j] = phi[j];
	  next[j] = NULL_TREE;
	  op[j] = NULL_TREE;
	}
    }

  for (i = 0; i < MAX_ITERATIONS_TO_TRACK; i++)
    {
      for (j = 0; j < 2; j++)
	aval[j] = get_val_for (op[j], val[j]);

      acnd = fold_binary (cmp, boolean_type_node, aval[0], aval[1]);
      if (acnd && zero_p (acnd))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Proved that loop %d iterates %d times using brute force.\n",
		     loop->num, i);
	  return build_int_cst (unsigned_type_node, i);
	}

      for (j = 0; j < 2; j++)
	val[j] = get_val_for (next[j], val[j]);
    }

  return chrec_dont_know;
}

/* Finds the exit of the LOOP by that the loop exits after a constant
   number of iterations and stores the exit edge to *EXIT.  The constant
   giving the number of iterations of LOOP is returned.  The number of
   iterations is determined using loop_niter_by_eval (i.e. by brute force
   evaluation).  If we are unable to find the exit for that loop_niter_by_eval
   determines the number of iterations, chrec_dont_know is returned.  */

tree
find_loop_niter_by_eval (struct loop *loop, edge *exit)
{
  unsigned n_exits, i;
  edge *exits = get_loop_exit_edges (loop, &n_exits);
  edge ex;
  tree niter = NULL_TREE, aniter;

  *exit = NULL;
  for (i = 0; i < n_exits; i++)
    {
      ex = exits[i];
      if (!just_once_each_iteration_p (loop, ex->src))
	continue;

      aniter = loop_niter_by_eval (loop, ex);
      if (chrec_contains_undetermined (aniter))
	continue;

      if (niter
	  && !tree_int_cst_lt (aniter, niter))
	continue;

      niter = aniter;
      *exit = ex;
    }
  free (exits);

  return niter ? niter : chrec_dont_know;
}

/*

   Analysis of upper bounds on number of iterations of a loop.

*/

/* Records that AT_STMT is executed at most BOUND times in LOOP.  The
   additional condition ADDITIONAL is recorded with the bound.  */

void
record_estimate (struct loop *loop, tree bound, tree additional, tree at_stmt)
{
  struct nb_iter_bound *elt = xmalloc (sizeof (struct nb_iter_bound));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Statements after ");
      print_generic_expr (dump_file, at_stmt, TDF_SLIM);
      fprintf (dump_file, " are executed at most ");
      print_generic_expr (dump_file, bound, TDF_SLIM);
      fprintf (dump_file, " times in loop %d.\n", loop->num);
    }

  elt->bound = bound;
  elt->at_stmt = at_stmt;
  elt->additional = additional;
  elt->next = loop->bounds;
  loop->bounds = elt;
}

/* Initialize LOOP->ESTIMATED_NB_ITERATIONS with the lowest safe
   approximation of the number of iterations for LOOP.  */

static void
compute_estimated_nb_iterations (struct loop *loop)
{
  struct nb_iter_bound *bound;
  
  for (bound = loop->bounds; bound; bound = bound->next)
    if (TREE_CODE (bound->bound) == INTEGER_CST
	/* Update only when there is no previous estimation.  */
	&& (chrec_contains_undetermined (loop->estimated_nb_iterations)
	    /* Or when the current estimation is smaller.  */
	    || tree_int_cst_lt (bound->bound, loop->estimated_nb_iterations)))
      loop->estimated_nb_iterations = bound->bound;
}

/* The following analyzers are extracting informations on the bounds
   of LOOP from the following undefined behaviors:

   - data references should not access elements over the statically
     allocated size,

   - signed variables should not overflow when flag_wrapv is not set.
*/

static void
infer_loop_bounds_from_undefined (struct loop *loop)
{
  unsigned i;
  basic_block bb, *bbs;
  block_stmt_iterator bsi;
  
  bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
        {
	  tree stmt = bsi_stmt (bsi);

	  switch (TREE_CODE (stmt))
	    {
	    case MODIFY_EXPR:
	      {
		tree op0 = TREE_OPERAND (stmt, 0);
		tree op1 = TREE_OPERAND (stmt, 1);

		/* For each array access, analyze its access function
		   and record a bound on the loop iteration domain.  */
		if (TREE_CODE (op1) == ARRAY_REF)
		  analyze_array (stmt, op1, true);

		if (TREE_CODE (op0) == ARRAY_REF)
		  analyze_array (stmt, op0, false);

		/* For each signed type variable in LOOP, analyze its
		   scalar evolution and record a bound of the loop
		   based on the type's ranges.  */
		else if (!flag_wrapv && TREE_CODE (op0) == SSA_NAME)
		  {
		    tree init, step, diff, estimation;
		    tree scev = instantiate_parameters 
		      (loop, analyze_scalar_evolution (loop, op0));
		    tree type = chrec_type (scev);
		    tree utype;

		    if (chrec_contains_undetermined (scev)
			|| TYPE_UNSIGNED (type))
		      break;

		    init = initial_condition_in_loop_num (scev, loop->num);
		    step = evolution_part_in_loop_num (scev, loop->num);

		    if (init == NULL_TREE
			|| step == NULL_TREE
			|| TREE_CODE (init) != INTEGER_CST
			|| TREE_CODE (step) != INTEGER_CST
			|| TYPE_MIN_VALUE (type) == NULL_TREE
			|| TYPE_MAX_VALUE (type) == NULL_TREE)
		      break;

		    utype = unsigned_type_for (type);
		    if (tree_int_cst_lt (step, integer_zero_node))
		      diff = fold_build2 (MINUS_EXPR, utype, init,
					  TYPE_MIN_VALUE (type));
		    else
		      diff = fold_build2 (MINUS_EXPR, utype,
					  TYPE_MAX_VALUE (type), init);

		    estimation = fold_build2 (CEIL_DIV_EXPR, utype, diff,
					      step);
		    record_estimate (loop, estimation, boolean_true_node, stmt);
		  }

		break;
	      }

	    case CALL_EXPR:
	      {
		tree args;

		for (args = TREE_OPERAND (stmt, 1); args;
		     args = TREE_CHAIN (args))
		  if (TREE_CODE (TREE_VALUE (args)) == ARRAY_REF)
		    analyze_array (stmt, TREE_VALUE (args), true);

		break;
	      }

	    default:
	      break;
	    }
	}

      if (chrec_contains_undetermined (loop->estimated_nb_iterations))
	compute_estimated_nb_iterations (loop);
    }

  free (bbs);
}

/* Records estimates on numbers of iterations of LOOP.  */

static void
estimate_numbers_of_iterations_loop (struct loop *loop)
{
  edge *exits;
  tree niter, type;
  unsigned i, n_exits;
  struct tree_niter_desc niter_desc;

  /* Give up if we already have tried to compute an estimation.  */
  if (loop->estimated_nb_iterations == chrec_dont_know
      /* Or when we already have an estimation.  */
      || (loop->estimated_nb_iterations != NULL_TREE
	  && TREE_CODE (loop->estimated_nb_iterations) == INTEGER_CST))
    return;
  else
    loop->estimated_nb_iterations = chrec_dont_know;

  exits = get_loop_exit_edges (loop, &n_exits);
  for (i = 0; i < n_exits; i++)
    {
      if (!number_of_iterations_exit (loop, exits[i], &niter_desc, false))
	continue;

      niter = niter_desc.niter;
      type = TREE_TYPE (niter);
      if (!zero_p (niter_desc.may_be_zero)
	  && !nonzero_p (niter_desc.may_be_zero))
	niter = build3 (COND_EXPR, type, niter_desc.may_be_zero,
			build_int_cst_type (type, 0),
			niter);
      record_estimate (loop, niter,
		       niter_desc.additional_info,
		       last_stmt (exits[i]->src));
    }
  free (exits);
  
  if (chrec_contains_undetermined (loop->estimated_nb_iterations))
    infer_loop_bounds_from_undefined (loop);
}

/* Records estimates on numbers of iterations of LOOPS.  */

void
estimate_numbers_of_iterations (struct loops *loops)
{
  unsigned i;
  struct loop *loop;

  for (i = 1; i < loops->num; i++)
    {
      loop = loops->parray[i];
      if (loop)
	estimate_numbers_of_iterations_loop (loop);
    }
}

/* If A > B, returns -1.  If A == B, returns 0.  If A < B, returns 1.
   If neither of these relations can be proved, returns 2.  */

static int
compare_trees (tree a, tree b)
{
  tree typea = TREE_TYPE (a), typeb = TREE_TYPE (b);
  tree type;

  if (TYPE_PRECISION (typea) > TYPE_PRECISION (typeb))
    type = typea;
  else
    type = typeb;

  a = fold_convert (type, a);
  b = fold_convert (type, b);

  if (nonzero_p (fold_binary (EQ_EXPR, boolean_type_node, a, b)))
    return 0;
  if (nonzero_p (fold_binary (LT_EXPR, boolean_type_node, a, b)))
    return 1;
  if (nonzero_p (fold_binary (GT_EXPR, boolean_type_node, a, b)))
    return -1;

  return 2;
}

/* Returns true if statement S1 dominates statement S2.  */

static bool
stmt_dominates_stmt_p (tree s1, tree s2)
{
  basic_block bb1 = bb_for_stmt (s1), bb2 = bb_for_stmt (s2);

  if (!bb1
      || s1 == s2)
    return true;

  if (bb1 == bb2)
    {
      block_stmt_iterator bsi;

      for (bsi = bsi_start (bb1); bsi_stmt (bsi) != s2; bsi_next (&bsi))
	if (bsi_stmt (bsi) == s1)
	  return true;

      return false;
    }

  return dominated_by_p (CDI_DOMINATORS, bb2, bb1);
}

/* Return true when it is possible to prove that the induction
   variable does not wrap: vary outside the type specified bounds.
   Checks whether BOUND < VALID_NITER that means in the context of iv
   conversion that all the iterations in the loop are safe: not
   producing wraps.

   The statement NITER_BOUND->AT_STMT is executed at most
   NITER_BOUND->BOUND times in the loop.
   
   NITER_BOUND->ADDITIONAL is the additional condition recorded for
   operands of the bound.  This is useful in the following case,
   created by loop header copying:

   i = 0;
   if (n > 0)
     do
       {
         something;
       } while (++i < n)

   If the n > 0 condition is taken into account, the number of iterations of the
   loop can be expressed as n - 1.  If the type of n is signed, the ADDITIONAL
   assumption "n > 0" says us that the value of the number of iterations is at
   most MAX_TYPE - 1 (without this assumption, it might overflow).  */

static bool
proved_non_wrapping_p (tree at_stmt,
		       struct nb_iter_bound *niter_bound, 
		       tree new_type,
		       tree valid_niter)
{
  tree cond;
  tree bound = niter_bound->bound;
  enum tree_code cmp;

  if (TYPE_PRECISION (new_type) > TYPE_PRECISION (TREE_TYPE (bound)))
    bound = fold_convert (unsigned_type_for (new_type), bound);
  else
    valid_niter = fold_convert (TREE_TYPE (bound), valid_niter);

  /* Give up if BOUND was not folded to an INTEGER_CST, as in PR23434.  */
  if (TREE_CODE (bound) != INTEGER_CST)
    return false;

  /* After the statement niter_bound->at_stmt we know that anything is
     executed at most BOUND times.  */
  if (at_stmt && stmt_dominates_stmt_p (niter_bound->at_stmt, at_stmt))
    cmp = GE_EXPR;
  /* Before the statement niter_bound->at_stmt we know that anything
     is executed at most BOUND + 1 times.  */
  else
    cmp = GT_EXPR;

  cond = fold_binary (cmp, boolean_type_node, valid_niter, bound);
  if (nonzero_p (cond))
    return true;

  cond = build2 (cmp, boolean_type_node, valid_niter, bound);
  /* Try taking additional conditions into account.  */
  cond = fold_binary (TRUTH_OR_EXPR, boolean_type_node,
		      invert_truthvalue (niter_bound->additional),
		      cond);

  if (nonzero_p (cond))
    return true;

  return false;
}

/* Checks whether it is correct to count the induction variable BASE +
   STEP * I at AT_STMT in a wider type NEW_TYPE, using the bounds on
   numbers of iterations of a LOOP.  If it is possible, return the
   value of step of the induction variable in the NEW_TYPE, otherwise
   return NULL_TREE.  */

static tree
convert_step_widening (struct loop *loop, tree new_type, tree base, tree step,
		       tree at_stmt)
{
  struct nb_iter_bound *bound;
  tree base_in_new_type, base_plus_step_in_new_type, step_in_new_type;
  tree delta, step_abs;
  tree unsigned_type, valid_niter;

  /* Compute the new step.  For example, {(uchar) 100, +, (uchar) 240}
     is converted to {(uint) 100, +, (uint) 0xfffffff0} in order to
     keep the values of the induction variable unchanged: 100, 84, 68,
     ...

     Another example is: (uint) {(uchar)100, +, (uchar)3} is converted
     to {(uint)100, +, (uint)3}.  

     Before returning the new step, verify that the number of
     iterations is less than DELTA / STEP_ABS (i.e. in the previous
     example (256 - 100) / 3) such that the iv does not wrap (in which
     case the operations are too difficult to be represented and
     handled: the values of the iv should be taken modulo 256 in the
     wider type; this is not implemented).  */
  base_in_new_type = fold_convert (new_type, base);
  base_plus_step_in_new_type = 
    fold_convert (new_type,
		  fold_build2 (PLUS_EXPR, TREE_TYPE (base), base, step));
  step_in_new_type = fold_build2 (MINUS_EXPR, new_type,
				  base_plus_step_in_new_type,
				  base_in_new_type);

  if (TREE_CODE (step_in_new_type) != INTEGER_CST)
    return NULL_TREE;

  switch (compare_trees (base_plus_step_in_new_type, base_in_new_type))
    {
    case -1:
      {
	tree extreme = upper_bound_in_type (new_type, TREE_TYPE (base));
	delta = fold_build2 (MINUS_EXPR, new_type, extreme,
			     base_in_new_type);
	step_abs = step_in_new_type;
	break;
      }

    case 1:
      {
	tree extreme = lower_bound_in_type (new_type, TREE_TYPE (base));
	delta = fold_build2 (MINUS_EXPR, new_type, base_in_new_type,
			     extreme);
	step_abs = fold_build1 (NEGATE_EXPR, new_type, step_in_new_type);
	break;
      }

    case 0:
      return step_in_new_type;

    default:
      return NULL_TREE;
    }

  unsigned_type = unsigned_type_for (new_type);
  delta = fold_convert (unsigned_type, delta);
  step_abs = fold_convert (unsigned_type, step_abs);
  valid_niter = fold_build2 (FLOOR_DIV_EXPR, unsigned_type,
			     delta, step_abs);

  estimate_numbers_of_iterations_loop (loop);
  for (bound = loop->bounds; bound; bound = bound->next)
    if (proved_non_wrapping_p (at_stmt, bound, new_type, valid_niter))
      return step_in_new_type;

  /* Fail when the loop has no bound estimations, or when no bound can
     be used for verifying the conversion.  */
  return NULL_TREE;
}

/* Returns true when VAR is used in pointer arithmetics.  DEPTH is
   used for limiting the search.  */

static bool
used_in_pointer_arithmetic_p (tree var, int depth)
{
  use_operand_p use_p;
  imm_use_iterator iter;

  if (depth == 0
      || TREE_CODE (var) != SSA_NAME
      || !has_single_use (var))
    return false;

  FOR_EACH_IMM_USE_FAST (use_p, iter, var)
    {
      tree stmt = USE_STMT (use_p);

      if (stmt && TREE_CODE (stmt) == MODIFY_EXPR)
	{
	  tree rhs = TREE_OPERAND (stmt, 1);

	  if (TREE_CODE (rhs) == NOP_EXPR
	      || TREE_CODE (rhs) == CONVERT_EXPR)
	    {
	      if (POINTER_TYPE_P (TREE_TYPE (rhs)))
		return true;
	      return false;
	    }
	  else
	    return used_in_pointer_arithmetic_p (TREE_OPERAND (stmt, 0),
						 depth - 1);
	}
    }
  return false;
}

/* Return false only when the induction variable BASE + STEP * I is
   known to not overflow: i.e. when the number of iterations is small
   enough with respect to the step and initial condition in order to
   keep the evolution confined in TYPEs bounds.  Return true when the
   iv is known to overflow or when the property is not computable.

   Initialize INIT_IS_MAX to true when the evolution goes from
   INIT_IS_MAX to LOWER_BOUND_IN_TYPE, false in the contrary case.
   When this property cannot be determined, UNKNOWN_MAX is set to
   true.  */

bool
scev_probably_wraps_p (tree type, tree base, tree step, 
		       tree at_stmt, struct loop *loop,
		       bool *init_is_max, bool *unknown_max)
{
  struct nb_iter_bound *bound;
  tree delta, step_abs;
  tree unsigned_type, valid_niter;
  tree base_plus_step, bpsps;
  int cps, cpsps;

  /* FIXME: The following code will not be used anymore once
     http://gcc.gnu.org/ml/gcc-patches/2005-06/msg02025.html is
     committed.

     If AT_STMT is a cast to unsigned that is later used for
     referencing a memory location, it is followed by a pointer
     conversion just after.  Because pointers do not wrap, the
     sequences that reference the memory do not wrap either.  In the
     following example, sequences corresponding to D_13 and to D_14
     can be proved to not wrap because they are used for computing a
     memory access:
	 
       D.1621_13 = (long unsigned intD.4) D.1620_12;
       D.1622_14 = D.1621_13 * 8;
       D.1623_15 = (doubleD.29 *) D.1622_14;
  */
  if (at_stmt && TREE_CODE (at_stmt) == MODIFY_EXPR)
    {
      tree op0 = TREE_OPERAND (at_stmt, 0);
      tree op1 = TREE_OPERAND (at_stmt, 1);
      tree type_op1 = TREE_TYPE (op1);

      if ((TYPE_UNSIGNED (type_op1)
	   && used_in_pointer_arithmetic_p (op0, 2))
	  || POINTER_TYPE_P (type_op1))
	{
	  *unknown_max = true;
	  return false;
	}
    }

  if (chrec_contains_undetermined (base)
      || chrec_contains_undetermined (step)
      || TREE_CODE (base) == REAL_CST
      || TREE_CODE (step) == REAL_CST)
    {
      *unknown_max = true;
      return true;
    }

  *unknown_max = false;
  base_plus_step = fold_build2 (PLUS_EXPR, type, base, step);
  bpsps = fold_build2 (PLUS_EXPR, type, base_plus_step, step);
  cps = compare_trees (base_plus_step, base);
  cpsps = compare_trees (bpsps, base_plus_step);

  /* Check that the sequence is not wrapping in the first step: it
     should have the same monotonicity for the first two steps.  See
     PR23410.  */
  if (cps != cpsps)
    return true;

  switch (cps)
    {
    case -1:
      {
	tree extreme = upper_bound_in_type (type, TREE_TYPE (base));
	delta = fold_build2 (MINUS_EXPR, type, extreme, base);
	step_abs = step;
	*init_is_max = false;
	break;
      }

    case 1:
      {
	tree extreme = lower_bound_in_type (type, TREE_TYPE (base));
	delta = fold_build2 (MINUS_EXPR, type, base, extreme);
	step_abs = fold_build1 (NEGATE_EXPR, type, step);
	*init_is_max = true;
	break;
      }

    case 0:
      /* This means step is equal to 0.  This should not happen.  It
	 could happen in convert step, but not here.  Safely answer
	 don't know as in the default case.  */

    default:
      *unknown_max = true;
      return true;
    }

  /* If AT_STMT represents a cast operation, we may not be able to
     take advantage of the undefinedness of signed type evolutions.

     implement-c.texi states: "For conversion to a type of width
     N, the value is reduced modulo 2^N to be within range of the
     type;"

     See PR 21959 for a test case.  Essentially, given a cast
     operation
     		unsigned char uc;
		signed char sc;
		...
     		sc = (signed char) uc;
		if (sc < 0)
		  ...

     where uc and sc have the scev {0, +, 1}, we would consider uc to
     wrap around, but not sc, because it is of a signed type.  This
     causes VRP to erroneously fold the predicate above because it
     thinks that sc cannot be negative.  */
  if (at_stmt && TREE_CODE (at_stmt) == MODIFY_EXPR)
    {
      tree rhs = TREE_OPERAND (at_stmt, 1);
      tree outer_t = TREE_TYPE (rhs);

      if (!TYPE_UNSIGNED (outer_t)
	  && (TREE_CODE (rhs) == NOP_EXPR || TREE_CODE (rhs) == CONVERT_EXPR))
	{
	  tree inner_t = TREE_TYPE (TREE_OPERAND (rhs, 0));

	  /* If the inner type is unsigned and its size and/or
	     precision are smaller to that of the outer type, then the
	     expression may wrap around.  */
	  if (TYPE_UNSIGNED (inner_t)
	      && (TYPE_SIZE (inner_t) <= TYPE_SIZE (outer_t)
		  || TYPE_PRECISION (inner_t) <= TYPE_PRECISION (outer_t)))
	    {
	      *unknown_max = true;
	      return true;
	    }
	}
    }

  /* After having set INIT_IS_MAX, we can return false: when not using
     wrapping arithmetic, signed types don't wrap.  */
  if (!flag_wrapv && !TYPE_UNSIGNED (type))
    return false;

  unsigned_type = unsigned_type_for (type);
  delta = fold_convert (unsigned_type, delta);
  step_abs = fold_convert (unsigned_type, step_abs);
  valid_niter = fold_build2 (FLOOR_DIV_EXPR, unsigned_type, delta, step_abs);

  estimate_numbers_of_iterations_loop (loop);
  for (bound = loop->bounds; bound; bound = bound->next)
    if (proved_non_wrapping_p (at_stmt, bound, type, valid_niter))
      return false;

  /* At this point we still don't have a proof that the iv does not
     overflow: give up.  */
  *unknown_max = true;
  return true;
}

/* Return the conversion to NEW_TYPE of the STEP of an induction
   variable BASE + STEP * I at AT_STMT.  When it fails, return
   NULL_TREE.  */

tree
convert_step (struct loop *loop, tree new_type, tree base, tree step,
	      tree at_stmt)
{
  tree base_type;

  if (chrec_contains_undetermined (base)
      || chrec_contains_undetermined (step))
    return NULL_TREE;

  base_type = TREE_TYPE (base);

  /* When not using wrapping arithmetic, signed types don't wrap.  */
  if (!flag_wrapv && !TYPE_UNSIGNED (base_type))
    return fold_convert (new_type, step);

  if (TYPE_PRECISION (new_type) > TYPE_PRECISION (base_type))
    return convert_step_widening (loop, new_type, base, step, at_stmt);

  return fold_convert (new_type, step);
}

/* Frees the information on upper bounds on numbers of iterations of LOOP.  */

static void
free_numbers_of_iterations_estimates_loop (struct loop *loop)
{
  struct nb_iter_bound *bound, *next;
  
  for (bound = loop->bounds; bound; bound = next)
    {
      next = bound->next;
      free (bound);
    }

  loop->bounds = NULL;
}

/* Frees the information on upper bounds on numbers of iterations of LOOPS.  */

void
free_numbers_of_iterations_estimates (struct loops *loops)
{
  unsigned i;
  struct loop *loop;

  for (i = 1; i < loops->num; i++)
    {
      loop = loops->parray[i];
      if (loop)
	free_numbers_of_iterations_estimates_loop (loop);
    }
}

/* Substitute value VAL for ssa name NAME inside expressions held
   at LOOP.  */

void
substitute_in_loop_info (struct loop *loop, tree name, tree val)
{
  struct nb_iter_bound *bound;

  loop->nb_iterations = simplify_replace_tree (loop->nb_iterations, name, val);
  loop->estimated_nb_iterations
	  = simplify_replace_tree (loop->estimated_nb_iterations, name, val);
  for (bound = loop->bounds; bound; bound = bound->next)
    {
      bound->bound = simplify_replace_tree (bound->bound, name, val);
      bound->additional = simplify_replace_tree (bound->additional, name, val);
    }
}
