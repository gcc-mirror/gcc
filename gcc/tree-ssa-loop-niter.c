/* Functions to determine/estimate number of iterations of a loop.
   Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
   
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

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
#include "gmp.h"

#define SWAP(X, Y) do { affine_iv *tmp = (X); (X) = (Y); (Y) = tmp; } while (0)

/* The maximum number of dominator BBs we search for conditions
   of loop header copies we use for simplifying a conditional
   expression.  */
#define MAX_DOMINATORS_TO_WALK 8

/*

   Analysis of number of iterations of an affine exit test.

*/

/* Bounds on some value, BELOW <= X <= UP.  */

typedef struct
{
  mpz_t below, up;
} bounds;


/* Splits expression EXPR to a variable part VAR and constant OFFSET.  */

static void
split_to_var_and_offset (tree expr, tree *var, mpz_t offset)
{
  tree type = TREE_TYPE (expr);
  tree op0, op1;
  double_int off;
  bool negate = false;

  *var = expr;
  mpz_set_ui (offset, 0);

  switch (TREE_CODE (expr))
    {
    case MINUS_EXPR:
      negate = true;
      /* Fallthru.  */

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
      op0 = TREE_OPERAND (expr, 0);
      op1 = TREE_OPERAND (expr, 1);

      if (TREE_CODE (op1) != INTEGER_CST)
	break;

      *var = op0;
      /* Always sign extend the offset.  */
      off = double_int_sext (tree_to_double_int (op1),
			     TYPE_PRECISION (type));
      mpz_set_double_int (offset, off, false);
      break;

    case INTEGER_CST:
      *var = build_int_cst_type (type, 0);
      off = tree_to_double_int (expr);
      mpz_set_double_int (offset, off, TYPE_UNSIGNED (type));
      break;

    default:
      break;
    }
}

/* Stores estimate on the minimum/maximum value of the expression VAR + OFF
   in TYPE to MIN and MAX.  */

static void
determine_value_range (tree type, tree var, mpz_t off,
		       mpz_t min, mpz_t max)
{
  /* If the expression is a constant, we know its value exactly.  */
  if (integer_zerop (var))
    {
      mpz_set (min, off);
      mpz_set (max, off);
      return;
    }

  /* If the computation may wrap, we know nothing about the value, except for
     the range of the type.  */
  get_type_static_bounds (type, min, max);
  if (!nowrap_type_p (type))
    return;

  /* Since the addition of OFF does not wrap, if OFF is positive, then we may
     add it to MIN, otherwise to MAX.  */
  if (mpz_sgn (off) < 0)
    mpz_add (max, max, off);
  else
    mpz_add (min, min, off);
}

/* Stores the bounds on the difference of the values of the expressions
   (var + X) and (var + Y), computed in TYPE, to BNDS.  */

static void
bound_difference_of_offsetted_base (tree type, mpz_t x, mpz_t y,
				    bounds *bnds)
{
  int rel = mpz_cmp (x, y);
  bool may_wrap = !nowrap_type_p (type);
  mpz_t m;

  /* If X == Y, then the expressions are always equal.
     If X > Y, there are the following possibilities:
       a) neither of var + X and var + Y overflow or underflow, or both of
	  them do.  Then their difference is X - Y.
       b) var + X overflows, and var + Y does not.  Then the values of the
	  expressions are var + X - M and var + Y, where M is the range of
	  the type, and their difference is X - Y - M.
       c) var + Y underflows and var + X does not.  Their difference again
	  is M - X + Y.
       Therefore, if the arithmetics in type does not overflow, then the
       bounds are (X - Y, X - Y), otherwise they are (X - Y - M, X - Y)
     Similarly, if X < Y, the bounds are either (X - Y, X - Y) or
     (X - Y, X - Y + M).  */

  if (rel == 0)
    {
      mpz_set_ui (bnds->below, 0);
      mpz_set_ui (bnds->up, 0);
      return;
    }

  mpz_init (m);
  mpz_set_double_int (m, double_int_mask (TYPE_PRECISION (type)), true);
  mpz_add_ui (m, m, 1);
  mpz_sub (bnds->up, x, y);
  mpz_set (bnds->below, bnds->up);

  if (may_wrap)
    {
      if (rel > 0)
	mpz_sub (bnds->below, bnds->below, m);
      else
	mpz_add (bnds->up, bnds->up, m);
    }

  mpz_clear (m);
}

/* From condition C0 CMP C1 derives information regarding the
   difference of values of VARX + OFFX and VARY + OFFY, computed in TYPE,
   and stores it to BNDS.  */

static void
refine_bounds_using_guard (tree type, tree varx, mpz_t offx,
			   tree vary, mpz_t offy,
			   tree c0, enum tree_code cmp, tree c1,
			   bounds *bnds)
{
  tree varc0, varc1, tmp, ctype;
  mpz_t offc0, offc1, loffx, loffy, bnd;
  bool lbound = false;
  bool no_wrap = nowrap_type_p (type);
  bool x_ok, y_ok;

  switch (cmp)
    {
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
      STRIP_SIGN_NOPS (c0);
      STRIP_SIGN_NOPS (c1);
      ctype = TREE_TYPE (c0);
      if (!useless_type_conversion_p (ctype, type))
	return;

      break;

    case EQ_EXPR:
      /* We could derive quite precise information from EQ_EXPR, however, such
	 a guard is unlikely to appear, so we do not bother with handling
	 it.  */
      return;

    case NE_EXPR:
      /* NE_EXPR comparisons do not contain much of useful information, except for
	 special case of comparing with the bounds of the type.  */
      if (TREE_CODE (c1) != INTEGER_CST
	  || !INTEGRAL_TYPE_P (type))
	return;

      /* Ensure that the condition speaks about an expression in the same type
	 as X and Y.  */
      ctype = TREE_TYPE (c0);
      if (TYPE_PRECISION (ctype) != TYPE_PRECISION (type))
	return;
      c0 = fold_convert (type, c0);
      c1 = fold_convert (type, c1);

      if (TYPE_MIN_VALUE (type)
	  && operand_equal_p (c1, TYPE_MIN_VALUE (type), 0))
	{
	  cmp = GT_EXPR;
	  break;
	}
      if (TYPE_MAX_VALUE (type)
	  && operand_equal_p (c1, TYPE_MAX_VALUE (type), 0))
	{
	  cmp = LT_EXPR;
	  break;
	}

      return;
    default:
      return;
    } 

  mpz_init (offc0);
  mpz_init (offc1);
  split_to_var_and_offset (expand_simple_operations (c0), &varc0, offc0);
  split_to_var_and_offset (expand_simple_operations (c1), &varc1, offc1);

  /* We are only interested in comparisons of expressions based on VARX and
     VARY.  TODO -- we might also be able to derive some bounds from
     expressions containing just one of the variables.  */

  if (operand_equal_p (varx, varc1, 0))
    {
      tmp = varc0; varc0 = varc1; varc1 = tmp;
      mpz_swap (offc0, offc1);
      cmp = swap_tree_comparison (cmp);
    }

  if (!operand_equal_p (varx, varc0, 0)
      || !operand_equal_p (vary, varc1, 0))
    goto end;

  mpz_init_set (loffx, offx);
  mpz_init_set (loffy, offy);

  if (cmp == GT_EXPR || cmp == GE_EXPR)
    {
      tmp = varx; varx = vary; vary = tmp;
      mpz_swap (offc0, offc1);
      mpz_swap (loffx, loffy);
      cmp = swap_tree_comparison (cmp);
      lbound = true;
    }

  /* If there is no overflow, the condition implies that

     (VARX + OFFX) cmp (VARY + OFFY) + (OFFX - OFFY + OFFC1 - OFFC0).

     The overflows and underflows may complicate things a bit; each
     overflow decreases the appropriate offset by M, and underflow
     increases it by M.  The above inequality would not necessarily be
     true if
   
     -- VARX + OFFX underflows and VARX + OFFC0 does not, or
	VARX + OFFC0 overflows, but VARX + OFFX does not.
	This may only happen if OFFX < OFFC0.
     -- VARY + OFFY overflows and VARY + OFFC1 does not, or
	VARY + OFFC1 underflows and VARY + OFFY does not.
	This may only happen if OFFY > OFFC1.  */

  if (no_wrap)
    {
      x_ok = true;
      y_ok = true;
    }
  else
    {
      x_ok = (integer_zerop (varx)
	      || mpz_cmp (loffx, offc0) >= 0);
      y_ok = (integer_zerop (vary)
	      || mpz_cmp (loffy, offc1) <= 0);
    }

  if (x_ok && y_ok)
    {
      mpz_init (bnd);
      mpz_sub (bnd, loffx, loffy);
      mpz_add (bnd, bnd, offc1);
      mpz_sub (bnd, bnd, offc0);

      if (cmp == LT_EXPR)
	mpz_sub_ui (bnd, bnd, 1);

      if (lbound)
	{
	  mpz_neg (bnd, bnd);
	  if (mpz_cmp (bnds->below, bnd) < 0)
	    mpz_set (bnds->below, bnd);
	}
      else
	{
	  if (mpz_cmp (bnd, bnds->up) < 0)
	    mpz_set (bnds->up, bnd);
	}
      mpz_clear (bnd);
    }

  mpz_clear (loffx);
  mpz_clear (loffy);
end:
  mpz_clear (offc0);
  mpz_clear (offc1);
}

/* Stores the bounds on the value of the expression X - Y in LOOP to BNDS.
   The subtraction is considered to be performed in arbitrary precision,
   without overflows.
 
   We do not attempt to be too clever regarding the value ranges of X and
   Y; most of the time, they are just integers or ssa names offsetted by
   integer.  However, we try to use the information contained in the
   comparisons before the loop (usually created by loop header copying).  */

static void
bound_difference (struct loop *loop, tree x, tree y, bounds *bnds)
{
  tree type = TREE_TYPE (x);
  tree varx, vary;
  mpz_t offx, offy;
  mpz_t minx, maxx, miny, maxy;
  int cnt = 0;
  edge e;
  basic_block bb;
  tree cond, c0, c1;
  enum tree_code cmp;

  /* Get rid of unnecessary casts, but preserve the value of
     the expressions.  */
  STRIP_SIGN_NOPS (x);
  STRIP_SIGN_NOPS (y);

  mpz_init (bnds->below);
  mpz_init (bnds->up);
  mpz_init (offx);
  mpz_init (offy);
  split_to_var_and_offset (x, &varx, offx);
  split_to_var_and_offset (y, &vary, offy);

  if (!integer_zerop (varx)
      && operand_equal_p (varx, vary, 0))
    {
      /* Special case VARX == VARY -- we just need to compare the
         offsets.  The matters are a bit more complicated in the
	 case addition of offsets may wrap.  */
      bound_difference_of_offsetted_base (type, offx, offy, bnds);
    }
  else
    {
      /* Otherwise, use the value ranges to determine the initial
	 estimates on below and up.  */
      mpz_init (minx);
      mpz_init (maxx);
      mpz_init (miny);
      mpz_init (maxy);
      determine_value_range (type, varx, offx, minx, maxx);
      determine_value_range (type, vary, offy, miny, maxy);

      mpz_sub (bnds->below, minx, maxy);
      mpz_sub (bnds->up, maxx, miny);
      mpz_clear (minx);
      mpz_clear (maxx);
      mpz_clear (miny);
      mpz_clear (maxy);
    }

  /* If both X and Y are constants, we cannot get any more precise.  */
  if (integer_zerop (varx) && integer_zerop (vary))
    goto end;

  /* Now walk the dominators of the loop header and use the entry
     guards to refine the estimates.  */
  for (bb = loop->header;
       bb != ENTRY_BLOCK_PTR && cnt < MAX_DOMINATORS_TO_WALK;
       bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      if (!single_pred_p (bb))
	continue;
      e = single_pred_edge (bb);

      if (!(e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
	continue;

      cond = COND_EXPR_COND (last_stmt (e->src));
      if (!COMPARISON_CLASS_P (cond))
	continue;
      c0 = TREE_OPERAND (cond, 0);
      cmp = TREE_CODE (cond);
      c1 = TREE_OPERAND (cond, 1);

      if (e->flags & EDGE_FALSE_VALUE)
	cmp = invert_tree_comparison (cmp, false);

      refine_bounds_using_guard (type, varx, offx, vary, offy,
				 c0, cmp, c1, bnds);
      ++cnt;
    }

end:
  mpz_clear (offx);
  mpz_clear (offy);
}

/* Update the bounds in BNDS that restrict the value of X to the bounds
   that restrict the value of X + DELTA.  X can be obtained as a
   difference of two values in TYPE.  */

static void
bounds_add (bounds *bnds, double_int delta, tree type)
{
  mpz_t mdelta, max;

  mpz_init (mdelta);
  mpz_set_double_int (mdelta, delta, false);

  mpz_init (max);
  mpz_set_double_int (max, double_int_mask (TYPE_PRECISION (type)), true);

  mpz_add (bnds->up, bnds->up, mdelta);
  mpz_add (bnds->below, bnds->below, mdelta);

  if (mpz_cmp (bnds->up, max) > 0)
    mpz_set (bnds->up, max);

  mpz_neg (max, max);
  if (mpz_cmp (bnds->below, max) < 0)
    mpz_set (bnds->below, max);

  mpz_clear (mdelta);
  mpz_clear (max);
}

/* Update the bounds in BNDS that restrict the value of X to the bounds
   that restrict the value of -X.  */

static void
bounds_negate (bounds *bnds)
{
  mpz_t tmp;

  mpz_init_set (tmp, bnds->up);
  mpz_neg (bnds->up, bnds->below);
  mpz_neg (bnds->below, tmp);
  mpz_clear (tmp);
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
      rslt = build_int_cst (type, 1);
      for (; ctr; ctr--)
	{
	  rslt = int_const_binop (MULT_EXPR, rslt, x, 0);
	  x = int_const_binop (MULT_EXPR, x, x, 0);
	}
      rslt = int_const_binop (BIT_AND_EXPR, rslt, mask, 0);
    }

  return rslt;
}

/* Derives the upper bound BND on the number of executions of loop with exit
   condition S * i <> C, assuming that this exit is taken.  If
   NO_OVERFLOW is true, then the control variable of the loop does not
   overflow.  If NO_OVERFLOW is true or BNDS.below >= 0, then BNDS.up
   contains the upper bound on the value of C.  */

static void
number_of_iterations_ne_max (mpz_t bnd, bool no_overflow, tree c, tree s,
			     bounds *bnds)
{
  double_int max;
  mpz_t d;

  /* If the control variable does not overflow, the number of iterations is
     at most c / s.  Otherwise it is at most the period of the control
     variable.  */
  if (!no_overflow && !multiple_of_p (TREE_TYPE (c), c, s))
    {
      max = double_int_mask (TYPE_PRECISION (TREE_TYPE (c))
			     - tree_low_cst (num_ending_zeros (s), 1));
      mpz_set_double_int (bnd, max, true);
      return;
    }

  /* Determine the upper bound on C.  */
  if (no_overflow || mpz_sgn (bnds->below) >= 0)
    mpz_set (bnd, bnds->up);
  else if (TREE_CODE (c) == INTEGER_CST)
    mpz_set_double_int (bnd, tree_to_double_int (c), true);
  else
    mpz_set_double_int (bnd, double_int_mask (TYPE_PRECISION (TREE_TYPE (c))),
			true);

  mpz_init (d);
  mpz_set_double_int (d, tree_to_double_int (s), true);
  mpz_fdiv_q (bnd, bnd, d);
  mpz_clear (d);
}

/* Determines number of iterations of loop whose ending condition
   is IV <> FINAL.  TYPE is the type of the iv.  The number of
   iterations is stored to NITER.  EXIT_MUST_BE_TAKEN is true if
   we know that the exit must be taken eventually, i.e., that the IV
   ever reaches the value FINAL (we derived this earlier, and possibly set
   NITER->assumptions to make sure this is the case).  BNDS contains the
   bounds on the difference FINAL - IV->base.  */

static bool
number_of_iterations_ne (tree type, affine_iv *iv, tree final,
			 struct tree_niter_desc *niter, bool exit_must_be_taken,
			 bounds *bnds)
{
  tree niter_type = unsigned_type_for (type);
  tree s, c, d, bits, assumption, tmp, bound;
  mpz_t max;

  niter->control = *iv;
  niter->bound = final;
  niter->cmp = NE_EXPR;

  /* Rearrange the terms so that we get inequality S * i <> C, with S
     positive.  Also cast everything to the unsigned type.  If IV does
     not overflow, BNDS bounds the value of C.  Also, this is the
     case if the computation |FINAL - IV->base| does not overflow, i.e.,
     if BNDS->below in the result is nonnegative.  */
  if (tree_int_cst_sign_bit (iv->step))
    {
      s = fold_convert (niter_type,
			fold_build1 (NEGATE_EXPR, type, iv->step));
      c = fold_build2 (MINUS_EXPR, niter_type,
		       fold_convert (niter_type, iv->base),
		       fold_convert (niter_type, final));
      bounds_negate (bnds);
    }
  else
    {
      s = fold_convert (niter_type, iv->step);
      c = fold_build2 (MINUS_EXPR, niter_type,
		       fold_convert (niter_type, final),
		       fold_convert (niter_type, iv->base));
    }

  mpz_init (max);
  number_of_iterations_ne_max (max, iv->no_overflow, c, s, bnds);
  niter->max = mpz_get_double_int (niter_type, max, false);
  mpz_clear (max);

  /* First the trivial cases -- when the step is 1.  */
  if (integer_onep (s))
    {
      niter->niter = c;
      return true;
    }

  /* Let nsd (step, size of mode) = d.  If d does not divide c, the loop
     is infinite.  Otherwise, the number of iterations is
     (inverse(s/d) * (c/d)) mod (size of mode/d).  */
  bits = num_ending_zeros (s);
  bound = build_low_bits_mask (niter_type,
			       (TYPE_PRECISION (niter_type)
				- tree_low_cst (bits, 1)));

  d = fold_binary_to_constant (LSHIFT_EXPR, niter_type,
			       build_int_cst (niter_type, 1), bits);
  s = fold_binary_to_constant (RSHIFT_EXPR, niter_type, s, bits);

  if (!exit_must_be_taken)
    {
      /* If we cannot assume that the exit is taken eventually, record the
	 assumptions for divisibility of c.  */
      assumption = fold_build2 (FLOOR_MOD_EXPR, niter_type, c, d);
      assumption = fold_build2 (EQ_EXPR, boolean_type_node,
				assumption, build_int_cst (niter_type, 0));
      if (!integer_nonzerop (assumption))
	niter->assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
					  niter->assumptions, assumption);
    }
      
  c = fold_build2 (EXACT_DIV_EXPR, niter_type, c, d);
  tmp = fold_build2 (MULT_EXPR, niter_type, c, inverse (s, bound));
  niter->niter = fold_build2 (BIT_AND_EXPR, niter_type, tmp, bound);
  return true;
}

/* Checks whether we can determine the final value of the control variable
   of the loop with ending condition IV0 < IV1 (computed in TYPE).
   DELTA is the difference IV1->base - IV0->base, STEP is the absolute value
   of the step.  The assumptions necessary to ensure that the computation
   of the final value does not overflow are recorded in NITER.  If we
   find the final value, we adjust DELTA and return TRUE.  Otherwise
   we return false.  BNDS bounds the value of IV1->base - IV0->base,
   and will be updated by the same amount as DELTA.  EXIT_MUST_BE_TAKEN is
   true if we know that the exit must be taken eventually.  */

static bool
number_of_iterations_lt_to_ne (tree type, affine_iv *iv0, affine_iv *iv1,
			       struct tree_niter_desc *niter,
			       tree *delta, tree step,
			       bool exit_must_be_taken, bounds *bnds)
{
  tree niter_type = TREE_TYPE (step);
  tree mod = fold_build2 (FLOOR_MOD_EXPR, niter_type, *delta, step);
  tree tmod;
  mpz_t mmod;
  tree assumption = boolean_true_node, bound, noloop;
  bool ret = false, fv_comp_no_overflow;
  tree type1 = type;
  if (POINTER_TYPE_P (type))
    type1 = sizetype;

  if (TREE_CODE (mod) != INTEGER_CST)
    return false;
  if (integer_nonzerop (mod))
    mod = fold_build2 (MINUS_EXPR, niter_type, step, mod);
  tmod = fold_convert (type1, mod);

  mpz_init (mmod);
  mpz_set_double_int (mmod, tree_to_double_int (mod), true);
  mpz_neg (mmod, mmod);

  /* If the induction variable does not overflow and the exit is taken,
     then the computation of the final value does not overflow.  This is
     also obviously the case if the new final value is equal to the
     current one.  Finally, we postulate this for pointer type variables,
     as the code cannot rely on the object to that the pointer points being
     placed at the end of the address space (and more pragmatically,
     TYPE_{MIN,MAX}_VALUE is not defined for pointers).  */
  if (integer_zerop (mod) || POINTER_TYPE_P (type))
    fv_comp_no_overflow = true;
  else if (!exit_must_be_taken)
    fv_comp_no_overflow = false;
  else
    fv_comp_no_overflow =
	    (iv0->no_overflow && integer_nonzerop (iv0->step))
	    || (iv1->no_overflow && integer_nonzerop (iv1->step));

  if (integer_nonzerop (iv0->step))
    {
      /* The final value of the iv is iv1->base + MOD, assuming that this
	 computation does not overflow, and that
	 iv0->base <= iv1->base + MOD.  */
      if (!fv_comp_no_overflow)
	{
	  bound = fold_build2 (MINUS_EXPR, type1,
			       TYPE_MAX_VALUE (type1), tmod);
	  assumption = fold_build2 (LE_EXPR, boolean_type_node,
				    iv1->base, bound);
	  if (integer_zerop (assumption))
	    goto end;
	}
      if (mpz_cmp (mmod, bnds->below) < 0)
	noloop = boolean_false_node;
      else if (POINTER_TYPE_P (type))
	noloop = fold_build2 (GT_EXPR, boolean_type_node,
			      iv0->base,
			      fold_build2 (POINTER_PLUS_EXPR, type,
					   iv1->base, tmod));
      else
	noloop = fold_build2 (GT_EXPR, boolean_type_node,
			      iv0->base,
			      fold_build2 (PLUS_EXPR, type1,
					   iv1->base, tmod));
    }
  else
    {
      /* The final value of the iv is iv0->base - MOD, assuming that this
	 computation does not overflow, and that
	 iv0->base - MOD <= iv1->base. */
      if (!fv_comp_no_overflow)
	{
	  bound = fold_build2 (PLUS_EXPR, type1,
			       TYPE_MIN_VALUE (type1), tmod);
	  assumption = fold_build2 (GE_EXPR, boolean_type_node,
				    iv0->base, bound);
	  if (integer_zerop (assumption))
	    goto end;
	}
      if (mpz_cmp (mmod, bnds->below) < 0)
	noloop = boolean_false_node;
      else if (POINTER_TYPE_P (type))
	noloop = fold_build2 (GT_EXPR, boolean_type_node,
			      fold_build2 (POINTER_PLUS_EXPR, type,
					   iv0->base,
					   fold_build1 (NEGATE_EXPR,
							type1, tmod)),
			      iv1->base);
      else
	noloop = fold_build2 (GT_EXPR, boolean_type_node,
			      fold_build2 (MINUS_EXPR, type1,
					   iv0->base, tmod),
			      iv1->base);
    }

  if (!integer_nonzerop (assumption))
    niter->assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				      niter->assumptions,
				      assumption);
  if (!integer_zerop (noloop))
    niter->may_be_zero = fold_build2 (TRUTH_OR_EXPR, boolean_type_node,
				      niter->may_be_zero,
				      noloop);
  bounds_add (bnds, tree_to_double_int (mod), type);
  *delta = fold_build2 (PLUS_EXPR, niter_type, *delta, mod);

  ret = true;
end:
  mpz_clear (mmod);
  return ret;
}

/* Add assertions to NITER that ensure that the control variable of the loop
   with ending condition IV0 < IV1 does not overflow.  Types of IV0 and IV1
   are TYPE.  Returns false if we can prove that there is an overflow, true
   otherwise.  STEP is the absolute value of the step.  */

static bool
assert_no_overflow_lt (tree type, affine_iv *iv0, affine_iv *iv1,
		       struct tree_niter_desc *niter, tree step)
{
  tree bound, d, assumption, diff;
  tree niter_type = TREE_TYPE (step);

  if (integer_nonzerop (iv0->step))
    {
      /* for (i = iv0->base; i < iv1->base; i += iv0->step) */
      if (iv0->no_overflow)
	return true;

      /* If iv0->base is a constant, we can determine the last value before
	 overflow precisely; otherwise we conservatively assume
	 MAX - STEP + 1.  */

      if (TREE_CODE (iv0->base) == INTEGER_CST)
	{
	  d = fold_build2 (MINUS_EXPR, niter_type,
			   fold_convert (niter_type, TYPE_MAX_VALUE (type)),
			   fold_convert (niter_type, iv0->base));
	  diff = fold_build2 (FLOOR_MOD_EXPR, niter_type, d, step);
	}
      else
	diff = fold_build2 (MINUS_EXPR, niter_type, step,
			    build_int_cst (niter_type, 1));
      bound = fold_build2 (MINUS_EXPR, type,
			   TYPE_MAX_VALUE (type), fold_convert (type, diff));
      assumption = fold_build2 (LE_EXPR, boolean_type_node,
				iv1->base, bound);
    }
  else
    {
      /* for (i = iv1->base; i > iv0->base; i += iv1->step) */
      if (iv1->no_overflow)
	return true;

      if (TREE_CODE (iv1->base) == INTEGER_CST)
	{
	  d = fold_build2 (MINUS_EXPR, niter_type,
			   fold_convert (niter_type, iv1->base),
			   fold_convert (niter_type, TYPE_MIN_VALUE (type)));
	  diff = fold_build2 (FLOOR_MOD_EXPR, niter_type, d, step);
	}
      else
	diff = fold_build2 (MINUS_EXPR, niter_type, step,
			    build_int_cst (niter_type, 1));
      bound = fold_build2 (PLUS_EXPR, type,
			   TYPE_MIN_VALUE (type), fold_convert (type, diff));
      assumption = fold_build2 (GE_EXPR, boolean_type_node,
				iv0->base, bound);
    }

  if (integer_zerop (assumption))
    return false;
  if (!integer_nonzerop (assumption))
    niter->assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				      niter->assumptions, assumption);
    
  iv0->no_overflow = true;
  iv1->no_overflow = true;
  return true;
}

/* Add an assumption to NITER that a loop whose ending condition
   is IV0 < IV1 rolls.  TYPE is the type of the control iv.  BNDS
   bounds the value of IV1->base - IV0->base.  */

static void
assert_loop_rolls_lt (tree type, affine_iv *iv0, affine_iv *iv1,
		      struct tree_niter_desc *niter, bounds *bnds)
{
  tree assumption = boolean_true_node, bound, diff;
  tree mbz, mbzl, mbzr, type1;
  bool rolls_p, no_overflow_p;
  double_int dstep;
  mpz_t mstep, max;

  /* We are going to compute the number of iterations as
     (iv1->base - iv0->base + step - 1) / step, computed in the unsigned
     variant of TYPE.  This formula only works if 
     
     -step + 1 <= (iv1->base - iv0->base) <= MAX - step + 1
   
     (where MAX is the maximum value of the unsigned variant of TYPE, and
     the computations in this formula are performed in full precision
     (without overflows).

     Usually, for loops with exit condition iv0->base + step * i < iv1->base,
     we have a condition of form iv0->base - step < iv1->base before the loop,
     and for loops iv0->base < iv1->base - step * i the condition
     iv0->base < iv1->base + step, due to loop header copying, which enable us
     to prove the lower bound.
     
     The upper bound is more complicated.  Unless the expressions for initial
     and final value themselves contain enough information, we usually cannot
     derive it from the context.  */

  /* First check whether the answer does not follow from the bounds we gathered
     before.  */
  if (integer_nonzerop (iv0->step))
    dstep = tree_to_double_int (iv0->step);
  else
    {
      dstep = double_int_sext (tree_to_double_int (iv1->step),
			       TYPE_PRECISION (type));
      dstep = double_int_neg (dstep);
    }

  mpz_init (mstep);
  mpz_set_double_int (mstep, dstep, true);
  mpz_neg (mstep, mstep);
  mpz_add_ui (mstep, mstep, 1);

  rolls_p = mpz_cmp (mstep, bnds->below) <= 0;

  mpz_init (max);
  mpz_set_double_int (max, double_int_mask (TYPE_PRECISION (type)), true);
  mpz_add (max, max, mstep);
  no_overflow_p = (mpz_cmp (bnds->up, max) <= 0
		   /* For pointers, only values lying inside a single object
		      can be compared or manipulated by pointer arithmetics.
		      Gcc in general does not allow or handle objects larger
		      than half of the address space, hence the upper bound
		      is satisfied for pointers.  */
		   || POINTER_TYPE_P (type));
  mpz_clear (mstep);
  mpz_clear (max);

  if (rolls_p && no_overflow_p)
    return;
  
  type1 = type;
  if (POINTER_TYPE_P (type))
    type1 = sizetype;

  /* Now the hard part; we must formulate the assumption(s) as expressions, and
     we must be careful not to introduce overflow.  */

  if (integer_nonzerop (iv0->step))
    {
      diff = fold_build2 (MINUS_EXPR, type1,
			  iv0->step, build_int_cst (type1, 1));

      /* We need to know that iv0->base >= MIN + iv0->step - 1.  Since
	 0 address never belongs to any object, we can assume this for
	 pointers.  */
      if (!POINTER_TYPE_P (type))
	{
	  bound = fold_build2 (PLUS_EXPR, type1,
			       TYPE_MIN_VALUE (type), diff);
	  assumption = fold_build2 (GE_EXPR, boolean_type_node,
				    iv0->base, bound);
	}

      /* And then we can compute iv0->base - diff, and compare it with
	 iv1->base.  */      
      mbzl = fold_build2 (MINUS_EXPR, type1, 
			  fold_convert (type1, iv0->base), diff);
      mbzr = fold_convert (type1, iv1->base);
    }
  else
    {
      diff = fold_build2 (PLUS_EXPR, type1,
			  iv1->step, build_int_cst (type1, 1));

      if (!POINTER_TYPE_P (type))
	{
	  bound = fold_build2 (PLUS_EXPR, type1,
			       TYPE_MAX_VALUE (type), diff);
	  assumption = fold_build2 (LE_EXPR, boolean_type_node,
				    iv1->base, bound);
	}

      mbzl = fold_convert (type1, iv0->base);
      mbzr = fold_build2 (MINUS_EXPR, type1,
			  fold_convert (type1, iv1->base), diff);
    }

  if (!integer_nonzerop (assumption))
    niter->assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				      niter->assumptions, assumption);
  if (!rolls_p)
    {
      mbz = fold_build2 (GT_EXPR, boolean_type_node, mbzl, mbzr);
      niter->may_be_zero = fold_build2 (TRUTH_OR_EXPR, boolean_type_node,
					niter->may_be_zero, mbz);
    }
}

/* Determines number of iterations of loop whose ending condition
   is IV0 < IV1.  TYPE is the type of the iv.  The number of
   iterations is stored to NITER.  BNDS bounds the difference
   IV1->base - IV0->base.  EXIT_MUST_BE_TAKEN is true if we know
   that the exit must be taken eventually.  */

static bool
number_of_iterations_lt (tree type, affine_iv *iv0, affine_iv *iv1,
			 struct tree_niter_desc *niter,
			 bool exit_must_be_taken, bounds *bnds)
{
  tree niter_type = unsigned_type_for (type);
  tree delta, step, s;
  mpz_t mstep, tmp;

  if (integer_nonzerop (iv0->step))
    {
      niter->control = *iv0;
      niter->cmp = LT_EXPR;
      niter->bound = iv1->base;
    }
  else
    {
      niter->control = *iv1;
      niter->cmp = GT_EXPR;
      niter->bound = iv0->base;
    }

  delta = fold_build2 (MINUS_EXPR, niter_type,
		       fold_convert (niter_type, iv1->base),
		       fold_convert (niter_type, iv0->base));

  /* First handle the special case that the step is +-1.  */
  if ((integer_onep (iv0->step) && integer_zerop (iv1->step))
      || (integer_all_onesp (iv1->step) && integer_zerop (iv0->step)))
    {
      /* for (i = iv0->base; i < iv1->base; i++)

	 or

	 for (i = iv1->base; i > iv0->base; i--).
	     
	 In both cases # of iterations is iv1->base - iv0->base, assuming that
	 iv1->base >= iv0->base.

         First try to derive a lower bound on the value of
	 iv1->base - iv0->base, computed in full precision.  If the difference
	 is nonnegative, we are done, otherwise we must record the
	 condition.  */

      if (mpz_sgn (bnds->below) < 0)
	niter->may_be_zero = fold_build2 (LT_EXPR, boolean_type_node,
					  iv1->base, iv0->base);
      niter->niter = delta;
      niter->max = mpz_get_double_int (niter_type, bnds->up, false);
      return true;
    }

  if (integer_nonzerop (iv0->step))
    step = fold_convert (niter_type, iv0->step);
  else
    step = fold_convert (niter_type,
			 fold_build1 (NEGATE_EXPR, type, iv1->step));

  /* If we can determine the final value of the control iv exactly, we can
     transform the condition to != comparison.  In particular, this will be
     the case if DELTA is constant.  */
  if (number_of_iterations_lt_to_ne (type, iv0, iv1, niter, &delta, step,
				     exit_must_be_taken, bnds))
    {
      affine_iv zps;

      zps.base = build_int_cst (niter_type, 0);
      zps.step = step;
      /* number_of_iterations_lt_to_ne will add assumptions that ensure that
	 zps does not overflow.  */
      zps.no_overflow = true;

      return number_of_iterations_ne (type, &zps, delta, niter, true, bnds);
    }

  /* Make sure that the control iv does not overflow.  */
  if (!assert_no_overflow_lt (type, iv0, iv1, niter, step))
    return false;

  /* We determine the number of iterations as (delta + step - 1) / step.  For
     this to work, we must know that iv1->base >= iv0->base - step + 1,
     otherwise the loop does not roll.  */
  assert_loop_rolls_lt (type, iv0, iv1, niter, bnds);

  s = fold_build2 (MINUS_EXPR, niter_type,
		   step, build_int_cst (niter_type, 1));
  delta = fold_build2 (PLUS_EXPR, niter_type, delta, s);
  niter->niter = fold_build2 (FLOOR_DIV_EXPR, niter_type, delta, step);

  mpz_init (mstep);
  mpz_init (tmp);
  mpz_set_double_int (mstep, tree_to_double_int (step), true);
  mpz_add (tmp, bnds->up, mstep);
  mpz_sub_ui (tmp, tmp, 1);
  mpz_fdiv_q (tmp, tmp, mstep);
  niter->max = mpz_get_double_int (niter_type, tmp, false);
  mpz_clear (mstep);
  mpz_clear (tmp);

  return true;
}

/* Determines number of iterations of loop whose ending condition
   is IV0 <= IV1.  TYPE is the type of the iv.  The number of
   iterations is stored to NITER.  EXIT_MUST_BE_TAKEN is true if
   we know that this condition must eventually become false (we derived this
   earlier, and possibly set NITER->assumptions to make sure this
   is the case).  BNDS bounds the difference IV1->base - IV0->base.  */

static bool
number_of_iterations_le (tree type, affine_iv *iv0, affine_iv *iv1,
			 struct tree_niter_desc *niter, bool exit_must_be_taken,
			 bounds *bnds)
{
  tree assumption;
  tree type1 = type;
  if (POINTER_TYPE_P (type))
    type1 = sizetype;

  /* Say that IV0 is the control variable.  Then IV0 <= IV1 iff
     IV0 < IV1 + 1, assuming that IV1 is not equal to the greatest
     value of the type.  This we must know anyway, since if it is
     equal to this value, the loop rolls forever.  We do not check
     this condition for pointer type ivs, as the code cannot rely on 
     the object to that the pointer points being placed at the end of
     the address space (and more pragmatically, TYPE_{MIN,MAX}_VALUE is
     not defined for pointers).  */

  if (!exit_must_be_taken && !POINTER_TYPE_P (type))
    {
      if (integer_nonzerop (iv0->step))
	assumption = fold_build2 (NE_EXPR, boolean_type_node,
				  iv1->base, TYPE_MAX_VALUE (type));
      else
	assumption = fold_build2 (NE_EXPR, boolean_type_node,
				  iv0->base, TYPE_MIN_VALUE (type));

      if (integer_zerop (assumption))
	return false;
      if (!integer_nonzerop (assumption))
	niter->assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
					  niter->assumptions, assumption);
    }

  if (integer_nonzerop (iv0->step))
    {
      if (POINTER_TYPE_P (type))
	iv1->base = fold_build2 (POINTER_PLUS_EXPR, type, iv1->base,
				 build_int_cst (type1, 1));
      else
	iv1->base = fold_build2 (PLUS_EXPR, type1, iv1->base,
				 build_int_cst (type1, 1));
    }
  else if (POINTER_TYPE_P (type))
    iv0->base = fold_build2 (POINTER_PLUS_EXPR, type, iv0->base,
			     fold_build1 (NEGATE_EXPR, type1,
					  build_int_cst (type1, 1)));
  else
    iv0->base = fold_build2 (MINUS_EXPR, type1,
			     iv0->base, build_int_cst (type1, 1));

  bounds_add (bnds, double_int_one, type1);

  return number_of_iterations_lt (type, iv0, iv1, niter, exit_must_be_taken,
				  bnds);
}

/* Dumps description of affine induction variable IV to FILE.  */

static void
dump_affine_iv (FILE *file, affine_iv *iv)
{
  if (!integer_zerop (iv->step))
    fprintf (file, "[");

  print_generic_expr (dump_file, iv->base, TDF_SLIM);

  if (!integer_zerop (iv->step))
    {
      fprintf (file, ", + , ");
      print_generic_expr (dump_file, iv->step, TDF_SLIM);
      fprintf (file, "]%s", iv->no_overflow ? "(no_overflow)" : "");
    }
}

/* Determine the number of iterations according to condition (for staying
   inside loop) which compares two induction variables using comparison
   operator CODE.  The induction variable on left side of the comparison
   is IV0, the right-hand side is IV1.  Both induction variables must have
   type TYPE, which must be an integer or pointer type.  The steps of the
   ivs must be constants (or NULL_TREE, which is interpreted as constant zero).

   LOOP is the loop whose number of iterations we are determining.

   ONLY_EXIT is true if we are sure this is the only way the loop could be
   exited (including possibly non-returning function calls, exceptions, etc.)
   -- in this case we can use the information whether the control induction
   variables can overflow or not in a more efficient way.
   
   The results (number of iterations and assumptions as described in
   comments at struct tree_niter_desc in tree-flow.h) are stored to NITER.
   Returns false if it fails to determine number of iterations, true if it
   was determined (possibly with some assumptions).  */

static bool
number_of_iterations_cond (struct loop *loop,
			   tree type, affine_iv *iv0, enum tree_code code,
			   affine_iv *iv1, struct tree_niter_desc *niter,
			   bool only_exit)
{
  bool exit_must_be_taken = false, ret;
  bounds bnds;

  /* The meaning of these assumptions is this:
     if !assumptions
       then the rest of information does not have to be valid
     if may_be_zero then the loop does not roll, even if
       niter != 0.  */
  niter->assumptions = boolean_true_node;
  niter->may_be_zero = boolean_false_node;
  niter->niter = NULL_TREE;
  niter->max = double_int_zero;

  niter->bound = NULL_TREE;
  niter->cmp = ERROR_MARK;

  /* Make < comparison from > ones, and for NE_EXPR comparisons, ensure that
     the control variable is on lhs.  */
  if (code == GE_EXPR || code == GT_EXPR
      || (code == NE_EXPR && integer_zerop (iv0->step)))
    {
      SWAP (iv0, iv1);
      code = swap_tree_comparison (code);
    }

  if (POINTER_TYPE_P (type))
    {
      /* Comparison of pointers is undefined unless both iv0 and iv1 point
	 to the same object.  If they do, the control variable cannot wrap
	 (as wrap around the bounds of memory will never return a pointer
	 that would be guaranteed to point to the same object, even if we
	 avoid undefined behavior by casting to size_t and back).  */
      iv0->no_overflow = true;
      iv1->no_overflow = true;
    }

  /* If the control induction variable does not overflow and the only exit
     from the loop is the one that we analyze, we know it must be taken
     eventually.  */
  if (only_exit)
    {
      if (!integer_zerop (iv0->step) && iv0->no_overflow)
	exit_must_be_taken = true;
      else if (!integer_zerop (iv1->step) && iv1->no_overflow)
	exit_must_be_taken = true;
    }

  /* We can handle the case when neither of the sides of the comparison is
     invariant, provided that the test is NE_EXPR.  This rarely occurs in
     practice, but it is simple enough to manage.  */
  if (!integer_zerop (iv0->step) && !integer_zerop (iv1->step))
    {
      if (code != NE_EXPR)
	return false;

      iv0->step = fold_binary_to_constant (MINUS_EXPR, type,
					   iv0->step, iv1->step);
      iv0->no_overflow = false;
      iv1->step = build_int_cst (type, 0);
      iv1->no_overflow = true;
    }

  /* If the result of the comparison is a constant,  the loop is weird.  More
     precise handling would be possible, but the situation is not common enough
     to waste time on it.  */
  if (integer_zerop (iv0->step) && integer_zerop (iv1->step))
    return false;

  /* Ignore loops of while (i-- < 10) type.  */
  if (code != NE_EXPR)
    {
      if (iv0->step && tree_int_cst_sign_bit (iv0->step))
	return false;

      if (!integer_zerop (iv1->step) && !tree_int_cst_sign_bit (iv1->step))
	return false;
    }

  /* If the loop exits immediately, there is nothing to do.  */
  if (integer_zerop (fold_build2 (code, boolean_type_node, iv0->base, iv1->base)))
    {
      niter->niter = build_int_cst (unsigned_type_for (type), 0);
      niter->max = double_int_zero;
      return true;
    }
	  
  /* OK, now we know we have a senseful loop.  Handle several cases, depending
     on what comparison operator is used.  */
  bound_difference (loop, iv1->base, iv0->base, &bnds);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file,
	       "Analyzing # of iterations of loop %d\n", loop->num);

      fprintf (dump_file, "  exit condition ");
      dump_affine_iv (dump_file, iv0);
      fprintf (dump_file, " %s ",
	       code == NE_EXPR ? "!="
	       : code == LT_EXPR ? "<"
	       : "<=");
      dump_affine_iv (dump_file, iv1);
      fprintf (dump_file, "\n");

      fprintf (dump_file, "  bounds on difference of bases: ");
      mpz_out_str (dump_file, 10, bnds.below);
      fprintf (dump_file, " ... ");
      mpz_out_str (dump_file, 10, bnds.up);
      fprintf (dump_file, "\n");
    }

  switch (code)
    {
    case NE_EXPR:
      gcc_assert (integer_zerop (iv1->step));
      ret = number_of_iterations_ne (type, iv0, iv1->base, niter,
				     exit_must_be_taken, &bnds);
      break;

    case LT_EXPR:
      ret = number_of_iterations_lt (type, iv0, iv1, niter, exit_must_be_taken,
				     &bnds);
      break;

    case LE_EXPR:
      ret = number_of_iterations_le (type, iv0, iv1, niter, exit_must_be_taken,
				     &bnds);
      break;

    default:
      gcc_unreachable ();
    }

  mpz_clear (bnds.up);
  mpz_clear (bnds.below);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (ret)
	{
	  fprintf (dump_file, "  result:\n");
	  if (!integer_nonzerop (niter->assumptions))
	    {
	      fprintf (dump_file, "    under assumptions ");
	      print_generic_expr (dump_file, niter->assumptions, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }

	  if (!integer_zerop (niter->may_be_zero))
	    {
	      fprintf (dump_file, "    zero if ");
	      print_generic_expr (dump_file, niter->may_be_zero, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }

	  fprintf (dump_file, "    # of iterations ");
	  print_generic_expr (dump_file, niter->niter, TDF_SLIM);
	  fprintf (dump_file, ", bounded by ");
	  dump_double_int (dump_file, niter->max, true);
	  fprintf (dump_file, "\n");
	}
      else
	fprintf (dump_file, "  failed\n\n");
    }
  return ret;
}

/* Substitute NEW for OLD in EXPR and fold the result.  */

static tree
simplify_replace_tree (tree expr, tree old, tree new_tree)
{
  unsigned i, n;
  tree ret = NULL_TREE, e, se;

  if (!expr)
    return NULL_TREE;

  if (expr == old
      || operand_equal_p (expr, old, 0))
    return unshare_expr (new_tree);

  if (!EXPR_P (expr) && !GIMPLE_STMT_P (expr))
    return expr;

  n = TREE_OPERAND_LENGTH (expr);
  for (i = 0; i < n; i++)
    {
      e = TREE_OPERAND (expr, i);
      se = simplify_replace_tree (e, old, new_tree);
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
  enum tree_code code;

  if (expr == NULL_TREE)
    return expr;

  if (is_gimple_min_invariant (expr))
    return expr;

  code = TREE_CODE (expr);
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
    {
      n = TREE_OPERAND_LENGTH (expr);
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

      if (!ret)
	return expr;

      fold_defer_overflow_warnings ();
      ret = fold (ret);
      fold_undefer_and_ignore_overflow_warnings ();
      return ret;
    }

  if (TREE_CODE (expr) != SSA_NAME)
    return expr;

  stmt = SSA_NAME_DEF_STMT (expr);
  if (TREE_CODE (stmt) == PHI_NODE)
    {
      basic_block src, dest;

      if (PHI_NUM_ARGS (stmt) != 1)
	return expr;
      e = PHI_ARG_DEF (stmt, 0);

      /* Avoid propagating through loop exit phi nodes, which
	 could break loop-closed SSA form restrictions.  */
      dest = bb_for_stmt (stmt);
      src = single_pred (dest);
      if (TREE_CODE (e) == SSA_NAME
	  && src->loop_father != dest->loop_father)
	return expr;

      return expand_simple_operations (e);
    }
  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return expr;

  e = GIMPLE_STMT_OPERAND (stmt, 1);
  if (/* Casts are simple.  */
      TREE_CODE (e) != NOP_EXPR
      && TREE_CODE (e) != CONVERT_EXPR
      /* Copies are simple.  */
      && TREE_CODE (e) != SSA_NAME
      /* Assignments of invariants are simple.  */
      && !is_gimple_min_invariant (e)
      /* And increments and decrements by a constant are simple.  */
      && !((TREE_CODE (e) == PLUS_EXPR
	    || TREE_CODE (e) == MINUS_EXPR
	    || TREE_CODE (e) == POINTER_PLUS_EXPR)
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
      if (integer_zerop (e) || integer_nonzerop (e))
	return e;

      e = simplify_replace_tree (expr, e1, e0);
      if (integer_zerop (e) || integer_nonzerop (e))
	return e;
    }
  if (TREE_CODE (expr) == EQ_EXPR)
    {
      e0 = TREE_OPERAND (expr, 0);
      e1 = TREE_OPERAND (expr, 1);

      /* If e0 == e1 (EXPR) implies !COND, then EXPR cannot be true.  */
      e = simplify_replace_tree (cond, e0, e1);
      if (integer_zerop (e))
	return e;
      e = simplify_replace_tree (cond, e1, e0);
      if (integer_zerop (e))
	return e;
    }
  if (TREE_CODE (expr) == NE_EXPR)
    {
      e0 = TREE_OPERAND (expr, 0);
      e1 = TREE_OPERAND (expr, 1);

      /* If e0 == e1 (!EXPR) implies !COND, then EXPR must be true.  */
      e = simplify_replace_tree (cond, e0, e1);
      if (integer_zerop (e))
	return boolean_true_node;
      e = simplify_replace_tree (cond, e1, e0);
      if (integer_zerop (e))
	return boolean_true_node;
    }

  te = expand_simple_operations (expr);

  /* Check whether COND ==> EXPR.  */
  notcond = invert_truthvalue (cond);
  e = fold_binary (TRUTH_OR_EXPR, boolean_type_node, notcond, te);
  if (e && integer_nonzerop (e))
    return e;

  /* Check whether COND ==> not EXPR.  */
  e = fold_binary (TRUTH_AND_EXPR, boolean_type_node, cond, te);
  if (e && integer_zerop (e))
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
   Returns the simplified expression (or EXPR unchanged, if no
   simplification was possible).*/

static tree
simplify_using_initial_conditions (struct loop *loop, tree expr)
{
  edge e;
  basic_block bb;
  tree cond;
  int cnt = 0;

  if (TREE_CODE (expr) == INTEGER_CST)
    return expr;

  /* Limit walking the dominators to avoid quadraticness in
     the number of BBs times the number of loops in degenerate
     cases.  */
  for (bb = loop->header;
       bb != ENTRY_BLOCK_PTR && cnt < MAX_DOMINATORS_TO_WALK;
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
      expr = tree_simplify_using_condition (cond, expr);
      ++cnt;
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

/* Returns true if EXIT is the only possible exit from LOOP.  */

bool
loop_only_exit_p (const struct loop *loop, const_edge exit)
{
  basic_block *body;
  block_stmt_iterator bsi;
  unsigned i;
  tree call;

  if (exit != single_exit (loop))
    return false;

  body = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      for (bsi = bsi_start (body[0]); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  call = get_call_expr_in (bsi_stmt (bsi));
	  if (call && TREE_SIDE_EFFECTS (call))
	    {
	      free (body);
	      return false;
	    }
	}
    }

  free (body);
  return true;
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
  tree op0, op1;
  enum tree_code code;
  affine_iv iv0, iv1;

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
     
  if (!simple_iv (loop, stmt, op0, &iv0, false))
    return false;
  if (!simple_iv (loop, stmt, op1, &iv1, false))
    return false;

  /* We don't want to see undefined signed overflow warnings while
     computing the number of iterations.  */
  fold_defer_overflow_warnings ();

  iv0.base = expand_simple_operations (iv0.base);
  iv1.base = expand_simple_operations (iv1.base);
  if (!number_of_iterations_cond (loop, type, &iv0, code, &iv1, niter,
				  loop_only_exit_p (loop, exit)))
    {
      fold_undefer_and_ignore_overflow_warnings ();
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

  niter->assumptions
	  = simplify_using_initial_conditions (loop,
					       niter->assumptions);
  niter->may_be_zero
	  = simplify_using_initial_conditions (loop,
					       niter->may_be_zero);

  fold_undefer_and_ignore_overflow_warnings ();

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
      if (!integer_zerop (iv1.step)
	  ? (integer_zerop (iv0.step)
	     && (integer_onep (iv1.step) || integer_all_onesp (iv1.step)))
	  : (integer_onep (iv0.step) || integer_all_onesp (iv0.step)))
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
  unsigned i;
  VEC (edge, heap) *exits = get_loop_exit_edges (loop);
  edge ex;
  tree niter = NULL_TREE, aniter;
  struct tree_niter_desc desc;

  *exit = NULL;
  for (i = 0; VEC_iterate (edge, exits, i, ex); i++)
    {
      if (!just_once_each_iteration_p (loop, ex->src))
	continue;

      if (!number_of_iterations_exit (loop, ex, &desc, false))
	continue;

      if (integer_nonzerop (desc.may_be_zero))
	{
	  /* We exit in the first iteration through this exit.
	     We won't find anything better.  */
	  niter = build_int_cst (unsigned_type_node, 0);
	  *exit = ex;
	  break;
	}

      if (!integer_zerop (desc.may_be_zero))
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
  VEC_free (edge, heap, exits);

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

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
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
 
   * if X is NULL_TREE, we return the constant BASE.
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

  gcc_assert (is_gimple_min_invariant (base));

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
      val = fold (GIMPLE_STMT_OPERAND (stmt, 1));
      SET_USE (op, nx);
      /* only iterate loop once.  */
      return val;
    }

  /* Should never reach here.  */
  gcc_unreachable ();
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

  /* Don't issue signed overflow warnings.  */
  fold_defer_overflow_warnings ();

  for (i = 0; i < MAX_ITERATIONS_TO_TRACK; i++)
    {
      for (j = 0; j < 2; j++)
	aval[j] = get_val_for (op[j], val[j]);

      acnd = fold_binary (cmp, boolean_type_node, aval[0], aval[1]);
      if (acnd && integer_zerop (acnd))
	{
	  fold_undefer_and_ignore_overflow_warnings ();
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Proved that loop %d iterates %d times using brute force.\n",
		     loop->num, i);
	  return build_int_cst (unsigned_type_node, i);
	}

      for (j = 0; j < 2; j++)
	{
	  val[j] = get_val_for (next[j], val[j]);
	  if (!is_gimple_min_invariant (val[j]))
	    {
	      fold_undefer_and_ignore_overflow_warnings ();
	      return chrec_dont_know;
	    }
	}
    }

  fold_undefer_and_ignore_overflow_warnings ();

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
  unsigned i;
  VEC (edge, heap) *exits = get_loop_exit_edges (loop);
  edge ex;
  tree niter = NULL_TREE, aniter;

  *exit = NULL;
  for (i = 0; VEC_iterate (edge, exits, i, ex); i++)
    {
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
  VEC_free (edge, heap, exits);

  return niter ? niter : chrec_dont_know;
}

/*

   Analysis of upper bounds on number of iterations of a loop.

*/

/* Returns a constant upper bound on the value of expression VAL.  VAL
   is considered to be unsigned.  If its type is signed, its value must
   be nonnegative.  */
 
static double_int
derive_constant_upper_bound (const_tree val)
{
  tree type = TREE_TYPE (val);
  tree op0, op1, subtype, maxt;
  double_int bnd, max, mmax, cst;
  tree stmt;

  if (INTEGRAL_TYPE_P (type))
    maxt = TYPE_MAX_VALUE (type);
  else
    maxt = upper_bound_in_type (type, type);

  max = tree_to_double_int (maxt);

  switch (TREE_CODE (val))
    {
    case INTEGER_CST:
      return tree_to_double_int (val);

    case NOP_EXPR:
    case CONVERT_EXPR:
      op0 = TREE_OPERAND (val, 0);
      subtype = TREE_TYPE (op0);
      if (!TYPE_UNSIGNED (subtype)
	  /* If TYPE is also signed, the fact that VAL is nonnegative implies
	     that OP0 is nonnegative.  */
	  && TYPE_UNSIGNED (type)
	  && !tree_expr_nonnegative_p (op0))
	{
	  /* If we cannot prove that the casted expression is nonnegative,
	     we cannot establish more useful upper bound than the precision
	     of the type gives us.  */
	  return max;
	}

      /* We now know that op0 is an nonnegative value.  Try deriving an upper
	 bound for it.  */
      bnd = derive_constant_upper_bound (op0);

      /* If the bound does not fit in TYPE, max. value of TYPE could be
	 attained.  */
      if (double_int_ucmp (max, bnd) < 0)
	return max;

      return bnd;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      op0 = TREE_OPERAND (val, 0);
      op1 = TREE_OPERAND (val, 1);

      if (TREE_CODE (op1) != INTEGER_CST
	  || !tree_expr_nonnegative_p (op0))
	return max;

      /* Canonicalize to OP0 - CST.  Consider CST to be signed, in order to
	 choose the most logical way how to treat this constant regardless
	 of the signedness of the type.  */
      cst = tree_to_double_int (op1);
      cst = double_int_sext (cst, TYPE_PRECISION (type));
      if (TREE_CODE (val) == PLUS_EXPR)
	cst = double_int_neg (cst);

      bnd = derive_constant_upper_bound (op0);

      if (double_int_negative_p (cst))
	{
	  cst = double_int_neg (cst);
	  /* Avoid CST == 0x80000...  */
	  if (double_int_negative_p (cst))
	    return max;;

	  /* OP0 + CST.  We need to check that
	     BND <= MAX (type) - CST.  */

	  mmax = double_int_add (max, double_int_neg (cst));
	  if (double_int_ucmp (bnd, mmax) > 0)
	    return max;

	  return double_int_add (bnd, cst);
	}
      else
	{
	  /* OP0 - CST, where CST >= 0.

	     If TYPE is signed, we have already verified that OP0 >= 0, and we
	     know that the result is nonnegative.  This implies that
	     VAL <= BND - CST.

	     If TYPE is unsigned, we must additionally know that OP0 >= CST,
	     otherwise the operation underflows.
	   */

	  /* This should only happen if the type is unsigned; however, for
	     buggy programs that use overflowing signed arithmetics even with
	     -fno-wrapv, this condition may also be true for signed values.  */
	  if (double_int_ucmp (bnd, cst) < 0)
	    return max;

	  if (TYPE_UNSIGNED (type))
	    {
	      tree tem = fold_binary (GE_EXPR, boolean_type_node, op0,
				      double_int_to_tree (type, cst));
	      if (!tem || integer_nonzerop (tem))
		return max;
	    }

	  bnd = double_int_add (bnd, double_int_neg (cst));
	}

      return bnd;

    case FLOOR_DIV_EXPR:
    case EXACT_DIV_EXPR:
      op0 = TREE_OPERAND (val, 0);
      op1 = TREE_OPERAND (val, 1);
      if (TREE_CODE (op1) != INTEGER_CST
	  || tree_int_cst_sign_bit (op1))
	return max;

      bnd = derive_constant_upper_bound (op0);
      return double_int_udiv (bnd, tree_to_double_int (op1), FLOOR_DIV_EXPR);

    case BIT_AND_EXPR:
      op1 = TREE_OPERAND (val, 1);
      if (TREE_CODE (op1) != INTEGER_CST
	  || tree_int_cst_sign_bit (op1))
	return max;
      return tree_to_double_int (op1);

    case SSA_NAME:
      stmt = SSA_NAME_DEF_STMT (val);
      if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT
	  || GIMPLE_STMT_OPERAND (stmt, 0) != val)
	return max;
      return derive_constant_upper_bound (GIMPLE_STMT_OPERAND (stmt, 1));

    default: 
      return max;
    }
}

/* Records that every statement in LOOP is executed I_BOUND times.
   REALISTIC is true if I_BOUND is expected to be close the the real number
   of iterations.  UPPER is true if we are sure the loop iterates at most
   I_BOUND times.  */

static void
record_niter_bound (struct loop *loop, double_int i_bound, bool realistic,
		    bool upper)
{
  /* Update the bounds only when there is no previous estimation, or when the current
     estimation is smaller.  */
  if (upper
      && (!loop->any_upper_bound
	  || double_int_ucmp (i_bound, loop->nb_iterations_upper_bound) < 0))
    {
      loop->any_upper_bound = true;
      loop->nb_iterations_upper_bound = i_bound;
    }
  if (realistic
      && (!loop->any_estimate
	  || double_int_ucmp (i_bound, loop->nb_iterations_estimate) < 0))
    {
      loop->any_estimate = true;
      loop->nb_iterations_estimate = i_bound;
    }
}

/* Records that AT_STMT is executed at most BOUND + 1 times in LOOP.  IS_EXIT
   is true if the loop is exited immediately after STMT, and this exit
   is taken at last when the STMT is executed BOUND + 1 times.
   REALISTIC is true if BOUND is expected to be close the the real number
   of iterations.  UPPER is true if we are sure the loop iterates at most
   BOUND times.  I_BOUND is an unsigned double_int upper estimate on BOUND.  */

static void
record_estimate (struct loop *loop, tree bound, double_int i_bound,
		 tree at_stmt, bool is_exit, bool realistic, bool upper)
{
  double_int delta;
  edge exit;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Statement %s", is_exit ? "(exit)" : "");
      print_generic_expr (dump_file, at_stmt, TDF_SLIM);
      fprintf (dump_file, " is %sexecuted at most ",
	       upper ? "" : "probably ");
      print_generic_expr (dump_file, bound, TDF_SLIM);
      fprintf (dump_file, " (bounded by ");
      dump_double_int (dump_file, i_bound, true);
      fprintf (dump_file, ") + 1 times in loop %d.\n", loop->num);
    }

  /* If the I_BOUND is just an estimate of BOUND, it rarely is close to the
     real number of iterations.  */
  if (TREE_CODE (bound) != INTEGER_CST)
    realistic = false;
  if (!upper && !realistic)
    return;

  /* If we have a guaranteed upper bound, record it in the appropriate
     list.  */
  if (upper)
    {
      struct nb_iter_bound *elt = GGC_NEW (struct nb_iter_bound);

      elt->bound = i_bound;
      elt->stmt = at_stmt;
      elt->is_exit = is_exit;
      elt->next = loop->bounds;
      loop->bounds = elt;
    }

  /* Update the number of iteration estimates according to the bound.
     If at_stmt is an exit, then every statement in the loop is
     executed at most BOUND + 1 times.  If it is not an exit, then
     some of the statements before it could be executed BOUND + 2
     times, if an exit of LOOP is before stmt.  */
  exit = single_exit (loop);
  if (is_exit
      || (exit != NULL
	  && dominated_by_p (CDI_DOMINATORS,
			     exit->src, bb_for_stmt (at_stmt))))
    delta = double_int_one;
  else
    delta = double_int_two;
  i_bound = double_int_add (i_bound, delta);

  /* If an overflow occurred, ignore the result.  */
  if (double_int_ucmp (i_bound, delta) < 0)
    return;

  record_niter_bound (loop, i_bound, realistic, upper);
}

/* Record the estimate on number of iterations of LOOP based on the fact that
   the induction variable BASE + STEP * i evaluated in STMT does not wrap and
   its values belong to the range <LOW, HIGH>.  REALISTIC is true if the
   estimated number of iterations is expected to be close to the real one.
   UPPER is true if we are sure the induction variable does not wrap.  */

static void
record_nonwrapping_iv (struct loop *loop, tree base, tree step, tree stmt,
		       tree low, tree high, bool realistic, bool upper)
{
  tree niter_bound, extreme, delta;
  tree type = TREE_TYPE (base), unsigned_type;
  double_int max;

  if (TREE_CODE (step) != INTEGER_CST || integer_zerop (step))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Induction variable (");
      print_generic_expr (dump_file, TREE_TYPE (base), TDF_SLIM);
      fprintf (dump_file, ") ");
      print_generic_expr (dump_file, base, TDF_SLIM);
      fprintf (dump_file, " + ");
      print_generic_expr (dump_file, step, TDF_SLIM);
      fprintf (dump_file, " * iteration does not wrap in statement ");
      print_generic_expr (dump_file, stmt, TDF_SLIM);
      fprintf (dump_file, " in loop %d.\n", loop->num);
    }

  unsigned_type = unsigned_type_for (type);
  base = fold_convert (unsigned_type, base);
  step = fold_convert (unsigned_type, step);

  if (tree_int_cst_sign_bit (step))
    {
      extreme = fold_convert (unsigned_type, low);
      if (TREE_CODE (base) != INTEGER_CST)
	base = fold_convert (unsigned_type, high);
      delta = fold_build2 (MINUS_EXPR, unsigned_type, base, extreme);
      step = fold_build1 (NEGATE_EXPR, unsigned_type, step);
    }
  else
    {
      extreme = fold_convert (unsigned_type, high);
      if (TREE_CODE (base) != INTEGER_CST)
	base = fold_convert (unsigned_type, low);
      delta = fold_build2 (MINUS_EXPR, unsigned_type, extreme, base);
    }

  /* STMT is executed at most NITER_BOUND + 1 times, since otherwise the value
     would get out of the range.  */
  niter_bound = fold_build2 (FLOOR_DIV_EXPR, unsigned_type, delta, step);
  max = derive_constant_upper_bound (niter_bound);
  record_estimate (loop, niter_bound, max, stmt, false, realistic, upper);
}

/* Returns true if REF is a reference to an array at the end of a dynamically
   allocated structure.  If this is the case, the array may be allocated larger
   than its upper bound implies.  */

static bool
array_at_struct_end_p (tree ref)
{
  tree base = get_base_address (ref);
  tree parent, field;

  /* Unless the reference is through a pointer, the size of the array matches
     its declaration.  */
  if (!base || !INDIRECT_REF_P (base))
    return false;
  
  for (;handled_component_p (ref); ref = parent)
    {
      parent = TREE_OPERAND (ref, 0);

      if (TREE_CODE (ref) == COMPONENT_REF)
	{
	  /* All fields of a union are at its end.  */
	  if (TREE_CODE (TREE_TYPE (parent)) == UNION_TYPE)
	    continue;

	  /* Unless the field is at the end of the struct, we are done.  */
	  field = TREE_OPERAND (ref, 1);
	  if (TREE_CHAIN (field))
	    return false;
	}

      /* The other options are ARRAY_REF, ARRAY_RANGE_REF, VIEW_CONVERT_EXPR.
	 In all these cases, we might be accessing the last element, and
	 although in practice this will probably never happen, it is legal for
	 the indices of this last element to exceed the bounds of the array.
	 Therefore, continue checking.  */
    }

  gcc_assert (INDIRECT_REF_P (ref));
  return true;
}

/* Determine information about number of iterations a LOOP from the index
   IDX of a data reference accessed in STMT.  RELIABLE is true if STMT is
   guaranteed to be executed in every iteration of LOOP.  Callback for
   for_each_index.  */

struct ilb_data
{
  struct loop *loop;
  tree stmt;
  bool reliable;
};

static bool
idx_infer_loop_bounds (tree base, tree *idx, void *dta)
{
  struct ilb_data *data = (struct ilb_data *) dta;
  tree ev, init, step;
  tree low, high, type, next;
  bool sign, upper = data->reliable, at_end = false;
  struct loop *loop = data->loop;

  if (TREE_CODE (base) != ARRAY_REF)
    return true;

  /* For arrays at the end of the structure, we are not guaranteed that they
     do not really extend over their declared size.  However, for arrays of
     size greater than one, this is unlikely to be intended.  */
  if (array_at_struct_end_p (base))
    {
      at_end = true;
      upper = false;
    }

  ev = instantiate_parameters (loop, analyze_scalar_evolution (loop, *idx));
  init = initial_condition (ev);
  step = evolution_part_in_loop_num (ev, loop->num);

  if (!init
      || !step
      || TREE_CODE (step) != INTEGER_CST
      || integer_zerop (step)
      || tree_contains_chrecs (init, NULL)
      || chrec_contains_symbols_defined_in_loop (init, loop->num))
    return true;

  low = array_ref_low_bound (base);
  high = array_ref_up_bound (base);
  
  /* The case of nonconstant bounds could be handled, but it would be
     complicated.  */
  if (TREE_CODE (low) != INTEGER_CST
      || !high
      || TREE_CODE (high) != INTEGER_CST)
    return true;
  sign = tree_int_cst_sign_bit (step);
  type = TREE_TYPE (step);

  /* The array of length 1 at the end of a structure most likely extends
     beyond its bounds.  */
  if (at_end
      && operand_equal_p (low, high, 0))
    return true;

  /* In case the relevant bound of the array does not fit in type, or
     it does, but bound + step (in type) still belongs into the range of the
     array, the index may wrap and still stay within the range of the array
     (consider e.g. if the array is indexed by the full range of
     unsigned char).

     To make things simpler, we require both bounds to fit into type, although
     there are cases where this would not be strictly necessary.  */
  if (!int_fits_type_p (high, type)
      || !int_fits_type_p (low, type))
    return true;
  low = fold_convert (type, low);
  high = fold_convert (type, high);

  if (sign)
    next = fold_binary (PLUS_EXPR, type, low, step);
  else
    next = fold_binary (PLUS_EXPR, type, high, step);
  
  if (tree_int_cst_compare (low, next) <= 0
      && tree_int_cst_compare (next, high) <= 0)
    return true;

  record_nonwrapping_iv (loop, init, step, data->stmt, low, high, true, upper);
  return true;
}

/* Determine information about number of iterations a LOOP from the bounds
   of arrays in the data reference REF accessed in STMT.  RELIABLE is true if
   STMT is guaranteed to be executed in every iteration of LOOP.*/

static void
infer_loop_bounds_from_ref (struct loop *loop, tree stmt, tree ref,
			    bool reliable)
{
  struct ilb_data data;

  data.loop = loop;
  data.stmt = stmt;
  data.reliable = reliable;
  for_each_index (&ref, idx_infer_loop_bounds, &data);
}

/* Determine information about number of iterations of a LOOP from the way
   arrays are used in STMT.  RELIABLE is true if STMT is guaranteed to be
   executed in every iteration of LOOP.  */

static void
infer_loop_bounds_from_array (struct loop *loop, tree stmt, bool reliable)
{
  tree call;

  if (TREE_CODE (stmt) == GIMPLE_MODIFY_STMT)
    {
      tree op0 = GIMPLE_STMT_OPERAND (stmt, 0);
      tree op1 = GIMPLE_STMT_OPERAND (stmt, 1);

      /* For each memory access, analyze its access function
	 and record a bound on the loop iteration domain.  */
      if (REFERENCE_CLASS_P (op0))
	infer_loop_bounds_from_ref (loop, stmt, op0, reliable);

      if (REFERENCE_CLASS_P (op1))
	infer_loop_bounds_from_ref (loop, stmt, op1, reliable);
    }
  
  
  call = get_call_expr_in (stmt);
  if (call)
    {
      tree arg;
      call_expr_arg_iterator iter;

      FOR_EACH_CALL_EXPR_ARG (arg, iter, call)
	if (REFERENCE_CLASS_P (arg))
	  infer_loop_bounds_from_ref (loop, stmt, arg, reliable);
    }
}

/* Determine information about number of iterations of a LOOP from the fact
   that signed arithmetics in STMT does not overflow.  */

static void
infer_loop_bounds_from_signedness (struct loop *loop, tree stmt)
{
  tree def, base, step, scev, type, low, high;

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return;

  def = GIMPLE_STMT_OPERAND (stmt, 0);

  if (TREE_CODE (def) != SSA_NAME)
    return;

  type = TREE_TYPE (def);
  if (!INTEGRAL_TYPE_P (type)
      || !TYPE_OVERFLOW_UNDEFINED (type))
    return;

  scev = instantiate_parameters (loop, analyze_scalar_evolution (loop, def));
  if (chrec_contains_undetermined (scev))
    return;

  base = initial_condition_in_loop_num (scev, loop->num);
  step = evolution_part_in_loop_num (scev, loop->num);

  if (!base || !step
      || TREE_CODE (step) != INTEGER_CST
      || tree_contains_chrecs (base, NULL)
      || chrec_contains_symbols_defined_in_loop (base, loop->num))
    return;

  low = lower_bound_in_type (type, type);
  high = upper_bound_in_type (type, type);

  record_nonwrapping_iv (loop, base, step, stmt, low, high, false, true);
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
  basic_block *bbs;
  block_stmt_iterator bsi;
  basic_block bb;
  bool reliable;
  
  bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];

      /* If BB is not executed in each iteration of the loop, we cannot
	 use the operations in it to infer reliable upper bound on the
	 # of iterations of the loop.  However, we can use it as a guess.  */
      reliable = dominated_by_p (CDI_DOMINATORS, loop->latch, bb);

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  tree stmt = bsi_stmt (bsi);

	  infer_loop_bounds_from_array (loop, stmt, reliable);

	  if (reliable)
	    infer_loop_bounds_from_signedness (loop, stmt);
  	}

    }

  free (bbs);
}

/* Converts VAL to double_int.  */

static double_int
gcov_type_to_double_int (gcov_type val)
{
  double_int ret;

  ret.low = (unsigned HOST_WIDE_INT) val;
  /* If HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_WIDEST_INT, avoid shifting by
     the size of type.  */
  val >>= HOST_BITS_PER_WIDE_INT - 1;
  val >>= 1;
  ret.high = (unsigned HOST_WIDE_INT) val;

  return ret;
}

/* Records estimates on numbers of iterations of LOOP.  */

void
estimate_numbers_of_iterations_loop (struct loop *loop)
{
  VEC (edge, heap) *exits;
  tree niter, type;
  unsigned i;
  struct tree_niter_desc niter_desc;
  edge ex;
  double_int bound;

  /* Give up if we already have tried to compute an estimation.  */
  if (loop->estimate_state != EST_NOT_COMPUTED)
    return;
  loop->estimate_state = EST_AVAILABLE;
  loop->any_upper_bound = false;
  loop->any_estimate = false;

  exits = get_loop_exit_edges (loop);
  for (i = 0; VEC_iterate (edge, exits, i, ex); i++)
    {
      if (!number_of_iterations_exit (loop, ex, &niter_desc, false))
	continue;

      niter = niter_desc.niter;
      type = TREE_TYPE (niter);
      if (TREE_CODE (niter_desc.may_be_zero) != INTEGER_CST)
	niter = build3 (COND_EXPR, type, niter_desc.may_be_zero,
			build_int_cst (type, 0),
			niter);
      record_estimate (loop, niter, niter_desc.max,
		       last_stmt (ex->src),
		       true, true, true);
    }
  VEC_free (edge, heap, exits);
  
  infer_loop_bounds_from_undefined (loop);

  /* If we have a measured profile, use it to estimate the number of
     iterations.  */
  if (loop->header->count != 0)
    {
      gcov_type nit = expected_loop_iterations_unbounded (loop) + 1;
      bound = gcov_type_to_double_int (nit);
      record_niter_bound (loop, bound, true, false);
    }

  /* If an upper bound is smaller than the realistic estimate of the
     number of iterations, use the upper bound instead.  */
  if (loop->any_upper_bound
      && loop->any_estimate
      && double_int_ucmp (loop->nb_iterations_upper_bound,
			  loop->nb_iterations_estimate) < 0)
    loop->nb_iterations_estimate = loop->nb_iterations_upper_bound;
}

/* Records estimates on numbers of iterations of loops.  */

void
estimate_numbers_of_iterations (void)
{
  loop_iterator li;
  struct loop *loop;

  /* We don't want to issue signed overflow warnings while getting
     loop iteration estimates.  */
  fold_defer_overflow_warnings ();

  FOR_EACH_LOOP (li, loop, 0)
    {
      estimate_numbers_of_iterations_loop (loop);
    }

  fold_undefer_and_ignore_overflow_warnings ();
}

/* Returns true if statement S1 dominates statement S2.  */

bool
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

/* Returns true when we can prove that the number of executions of
   STMT in the loop is at most NITER, according to the bound on
   the number of executions of the statement NITER_BOUND->stmt recorded in
   NITER_BOUND.  If STMT is NULL, we must prove this bound for all
   statements in the loop.  */

static bool
n_of_executions_at_most (tree stmt,
			 struct nb_iter_bound *niter_bound, 
			 tree niter)
{
  double_int bound = niter_bound->bound;
  tree nit_type = TREE_TYPE (niter), e;
  enum tree_code cmp;

  gcc_assert (TYPE_UNSIGNED (nit_type));

  /* If the bound does not even fit into NIT_TYPE, it cannot tell us that
     the number of iterations is small.  */
  if (!double_int_fits_to_tree_p (nit_type, bound))
    return false;

  /* We know that NITER_BOUND->stmt is executed at most NITER_BOUND->bound + 1
     times.  This means that:
     
     -- if NITER_BOUND->is_exit is true, then everything before
        NITER_BOUND->stmt is executed at most NITER_BOUND->bound + 1
	times, and everything after it at most NITER_BOUND->bound times.

     -- If NITER_BOUND->is_exit is false, then if we can prove that when STMT
	is executed, then NITER_BOUND->stmt is executed as well in the same
	iteration (we conclude that if both statements belong to the same
	basic block, or if STMT is after NITER_BOUND->stmt), then STMT
	is executed at most NITER_BOUND->bound + 1 times.  Otherwise STMT is
	executed at most NITER_BOUND->bound + 2 times.  */

  if (niter_bound->is_exit)
    {
      if (stmt
	  && stmt != niter_bound->stmt
	  && stmt_dominates_stmt_p (niter_bound->stmt, stmt))
	cmp = GE_EXPR;
      else
	cmp = GT_EXPR;
    }
  else
    {
      if (!stmt
	  || (bb_for_stmt (stmt) != bb_for_stmt (niter_bound->stmt)
	      && !stmt_dominates_stmt_p (niter_bound->stmt, stmt)))
	{
	  bound = double_int_add (bound, double_int_one);
	  if (double_int_zero_p (bound)
	      || !double_int_fits_to_tree_p (nit_type, bound))
	    return false;
	}
      cmp = GT_EXPR;
    }

  e = fold_binary (cmp, boolean_type_node,
		   niter, double_int_to_tree (nit_type, bound));
  return e && integer_nonzerop (e);
}

/* Returns true if the arithmetics in TYPE can be assumed not to wrap.  */

bool
nowrap_type_p (tree type)
{
  if (INTEGRAL_TYPE_P (type)
      && TYPE_OVERFLOW_UNDEFINED (type))
    return true;

  if (POINTER_TYPE_P (type))
    return true;

  return false;
}

/* Return false only when the induction variable BASE + STEP * I is
   known to not overflow: i.e. when the number of iterations is small
   enough with respect to the step and initial condition in order to
   keep the evolution confined in TYPEs bounds.  Return true when the
   iv is known to overflow or when the property is not computable.
 
   USE_OVERFLOW_SEMANTICS is true if this function should assume that
   the rules for overflow of the given language apply (e.g., that signed
   arithmetics in C does not overflow).  */

bool
scev_probably_wraps_p (tree base, tree step, 
		       tree at_stmt, struct loop *loop,
		       bool use_overflow_semantics)
{
  struct nb_iter_bound *bound;
  tree delta, step_abs;
  tree unsigned_type, valid_niter;
  tree type = TREE_TYPE (step);

  /* FIXME: We really need something like
     http://gcc.gnu.org/ml/gcc-patches/2005-06/msg02025.html.

     We used to test for the following situation that frequently appears
     during address arithmetics:
	 
       D.1621_13 = (long unsigned intD.4) D.1620_12;
       D.1622_14 = D.1621_13 * 8;
       D.1623_15 = (doubleD.29 *) D.1622_14;

     And derived that the sequence corresponding to D_14
     can be proved to not wrap because it is used for computing a
     memory access; however, this is not really the case -- for example,
     if D_12 = (unsigned char) [254,+,1], then D_14 has values
     2032, 2040, 0, 8, ..., but the code is still legal.  */

  if (chrec_contains_undetermined (base)
      || chrec_contains_undetermined (step))
    return true;

  if (integer_zerop (step))
    return false;

  /* If we can use the fact that signed and pointer arithmetics does not
     wrap, we are done.  */
  if (use_overflow_semantics && nowrap_type_p (type))
    return false;

  /* To be able to use estimates on number of iterations of the loop,
     we must have an upper bound on the absolute value of the step.  */
  if (TREE_CODE (step) != INTEGER_CST)
    return true;

  /* Don't issue signed overflow warnings.  */
  fold_defer_overflow_warnings ();

  /* Otherwise, compute the number of iterations before we reach the
     bound of the type, and verify that the loop is exited before this
     occurs.  */
  unsigned_type = unsigned_type_for (type);
  base = fold_convert (unsigned_type, base);

  if (tree_int_cst_sign_bit (step))
    {
      tree extreme = fold_convert (unsigned_type,
				   lower_bound_in_type (type, type));
      delta = fold_build2 (MINUS_EXPR, unsigned_type, base, extreme);
      step_abs = fold_build1 (NEGATE_EXPR, unsigned_type,
			      fold_convert (unsigned_type, step));
    }
  else
    {
      tree extreme = fold_convert (unsigned_type,
				   upper_bound_in_type (type, type));
      delta = fold_build2 (MINUS_EXPR, unsigned_type, extreme, base);
      step_abs = fold_convert (unsigned_type, step);
    }

  valid_niter = fold_build2 (FLOOR_DIV_EXPR, unsigned_type, delta, step_abs);

  estimate_numbers_of_iterations_loop (loop);
  for (bound = loop->bounds; bound; bound = bound->next)
    {
      if (n_of_executions_at_most (at_stmt, bound, valid_niter))
	{
	  fold_undefer_and_ignore_overflow_warnings ();
	  return false;
	}
    }

  fold_undefer_and_ignore_overflow_warnings ();

  /* At this point we still don't have a proof that the iv does not
     overflow: give up.  */
  return true;
}

/* Frees the information on upper bounds on numbers of iterations of LOOP.  */

void
free_numbers_of_iterations_estimates_loop (struct loop *loop)
{
  struct nb_iter_bound *bound, *next;

  loop->nb_iterations = NULL;
  loop->estimate_state = EST_NOT_COMPUTED;
  for (bound = loop->bounds; bound; bound = next)
    {
      next = bound->next;
      ggc_free (bound);
    }

  loop->bounds = NULL;
}

/* Frees the information on upper bounds on numbers of iterations of loops.  */

void
free_numbers_of_iterations_estimates (void)
{
  loop_iterator li;
  struct loop *loop;

  FOR_EACH_LOOP (li, loop, 0)
    {
      free_numbers_of_iterations_estimates_loop (loop);
    }
}

/* Substitute value VAL for ssa name NAME inside expressions held
   at LOOP.  */

void
substitute_in_loop_info (struct loop *loop, tree name, tree val)
{
  loop->nb_iterations = simplify_replace_tree (loop->nb_iterations, name, val);
}
