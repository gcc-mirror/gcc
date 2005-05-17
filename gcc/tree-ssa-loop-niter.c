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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

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
	  rslt = fold_binary_to_constant (MULT_EXPR, type, rslt, x);
	  x = fold_binary_to_constant (MULT_EXPR, type, x, x);
	}
      rslt = fold_binary_to_constant (BIT_AND_EXPR, type, rslt, mask);
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
      delta = build2 (MINUS_EXPR, type, base1, base0);
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
  e = fold_build2 (TRUTH_OR_EXPR, boolean_type_node, notcond, te);
  if (nonzero_p (e))
    return e;

  /* Check whether COND ==> not EXPR.  */
  e = fold_build2 (TRUTH_AND_EXPR, boolean_type_node, cond, te);
  if (zero_p (e))
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
   declaration), false otherwise.  */

bool
number_of_iterations_exit (struct loop *loop, edge exit,
			   struct tree_niter_desc *niter)
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
  return integer_onep (niter->assumptions);
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

      if (!number_of_iterations_exit (loop, ex, &desc))
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

      acnd = fold_build2 (cmp, boolean_type_node, aval[0], aval[1]);
      if (zero_p (acnd))
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

/* Records estimates on numbers of iterations of LOOP.  */

static void
estimate_numbers_of_iterations_loop (struct loop *loop)
{
  edge *exits;
  tree niter, type;
  unsigned i, n_exits;
  struct tree_niter_desc niter_desc;

  exits = get_loop_exit_edges (loop, &n_exits);
  for (i = 0; i < n_exits; i++)
    {
      if (!number_of_iterations_exit (loop, exits[i], &niter_desc))
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
  
  /* Analyzes the bounds of arrays accessed in the loop.  */
  if (loop->estimated_nb_iterations == NULL_TREE)
    {
      varray_type datarefs;
      VARRAY_GENERIC_PTR_INIT (datarefs, 3, "datarefs");
      find_data_references_in_loop (loop, &datarefs);
      free_data_refs (datarefs);
    }
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

  if (nonzero_p (fold_build2 (EQ_EXPR, boolean_type_node, a, b)))
    return 0;
  if (nonzero_p (fold_build2 (LT_EXPR, boolean_type_node, a, b)))
    return 1;
  if (nonzero_p (fold_build2 (GT_EXPR, boolean_type_node, a, b)))
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

/* Checks whether it is correct to count the induction variable BASE + STEP * I
   at AT_STMT in wider TYPE, using the fact that statement OF is executed at
   most BOUND times in the loop.  If it is possible, return the value of step
   of the induction variable in the TYPE, otherwise return NULL_TREE.
   
   ADDITIONAL is the additional condition recorded for operands of the bound.
   This is useful in the following case, created by loop header copying:

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

static tree
can_count_iv_in_wider_type_bound (tree type, tree base, tree step,
				  tree at_stmt,
				  tree bound,
				  tree additional,
				  tree of)
{
  tree inner_type = TREE_TYPE (base), b, bplusstep, new_step, new_step_abs;
  tree valid_niter, extreme, unsigned_type, delta, bound_type;
  tree cond;

  b = fold_convert (type, base);
  bplusstep = fold_convert (type,
			    fold_build2 (PLUS_EXPR, inner_type, base, step));
  new_step = fold_build2 (MINUS_EXPR, type, bplusstep, b);
  if (TREE_CODE (new_step) != INTEGER_CST)
    return NULL_TREE;

  switch (compare_trees (bplusstep, b))
    {
    case -1:
      extreme = upper_bound_in_type (type, inner_type);
      delta = fold_build2 (MINUS_EXPR, type, extreme, b);
      new_step_abs = new_step;
      break;

    case 1:
      extreme = lower_bound_in_type (type, inner_type);
      new_step_abs = fold_build1 (NEGATE_EXPR, type, new_step);
      delta = fold_build2 (MINUS_EXPR, type, b, extreme);
      break;

    case 0:
      return new_step;

    default:
      return NULL_TREE;
    }

  unsigned_type = unsigned_type_for (type);
  delta = fold_convert (unsigned_type, delta);
  new_step_abs = fold_convert (unsigned_type, new_step_abs);
  valid_niter = fold_build2 (FLOOR_DIV_EXPR, unsigned_type,
			     delta, new_step_abs);

  bound_type = TREE_TYPE (bound);
  if (TYPE_PRECISION (type) > TYPE_PRECISION (bound_type))
    bound = fold_convert (unsigned_type, bound);
  else
    valid_niter = fold_convert (bound_type, valid_niter);
    
  if (at_stmt && stmt_dominates_stmt_p (of, at_stmt))
    {
      /* After the statement OF we know that anything is executed at most
	 BOUND times.  */
      cond = fold_build2 (GE_EXPR, boolean_type_node, valid_niter, bound);
    }
  else
    {
      /* Before the statement OF we know that anything is executed at most
	 BOUND + 1 times.  */
      cond = fold_build2 (GT_EXPR, boolean_type_node, valid_niter, bound);
    }

  if (nonzero_p (cond))
    return new_step;

  /* Try taking additional conditions into account.  */
  cond = fold_build2 (TRUTH_OR_EXPR, boolean_type_node,
		      invert_truthvalue (additional),
		      cond);
  if (nonzero_p (cond))
    return new_step;

  return NULL_TREE;
}

/* Checks whether it is correct to count the induction variable BASE + STEP * I
   at AT_STMT in wider TYPE, using the bounds on numbers of iterations of a
   LOOP.  If it is possible, return the value of step of the induction variable
   in the TYPE, otherwise return NULL_TREE.  */

tree
can_count_iv_in_wider_type (struct loop *loop, tree type, tree base, tree step,
			    tree at_stmt)
{
  struct nb_iter_bound *bound;
  tree new_step;

  for (bound = loop->bounds; bound; bound = bound->next)
    {
      new_step = can_count_iv_in_wider_type_bound (type, base, step,
						   at_stmt,
						   bound->bound,
						   bound->additional,
						   bound->at_stmt);

      if (new_step)
	return new_step;
    }

  return NULL_TREE;
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
