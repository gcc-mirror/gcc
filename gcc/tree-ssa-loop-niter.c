/* Functions to determine/estimate number of iterations of a loop.
   Copyright (C) 2004-2013 Free Software Foundation, Inc.

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
#include "tm_p.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "intl.h"
#include "tree-flow.h"
#include "dumpfile.h"
#include "cfgloop.h"
#include "ggc.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "tree-data-ref.h"
#include "params.h"
#include "flags.h"
#include "diagnostic-core.h"
#include "tree-inline.h"
#include "tree-pass.h"

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
      off = tree_to_double_int (op1);
      off = off.sext (TYPE_PRECISION (type));
      mpz_set_double_int (offset, off, false);
      if (negate)
	mpz_neg (offset, offset);
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
  mpz_set_double_int (m, double_int::mask (TYPE_PRECISION (type)), true);
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
  tree c0, c1;
  gimple cond;
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

      cond = last_stmt (e->src);
      c0 = gimple_cond_lhs (cond);
      cmp = gimple_cond_code (cond);
      c1 = gimple_cond_rhs (cond);

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
  mpz_set_double_int (max, double_int::mask (TYPE_PRECISION (type)), true);

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
	  rslt = int_const_binop (MULT_EXPR, rslt, x);
	  x = int_const_binop (MULT_EXPR, x, x);
	}
      rslt = int_const_binop (BIT_AND_EXPR, rslt, mask);
    }

  return rslt;
}

/* Derives the upper bound BND on the number of executions of loop with exit
   condition S * i <> C.  If NO_OVERFLOW is true, then the control variable of
   the loop does not overflow.  EXIT_MUST_BE_TAKEN is true if we are guaranteed
   that the loop ends through this exit, i.e., the induction variable ever
   reaches the value of C.  
   
   The value C is equal to final - base, where final and base are the final and
   initial value of the actual induction variable in the analysed loop.  BNDS
   bounds the value of this difference when computed in signed type with
   unbounded range, while the computation of C is performed in an unsigned
   type with the range matching the range of the type of the induction variable.
   In particular, BNDS.up contains an upper bound on C in the following cases:
   -- if the iv must reach its final value without overflow, i.e., if
      NO_OVERFLOW && EXIT_MUST_BE_TAKEN is true, or
   -- if final >= base, which we know to hold when BNDS.below >= 0.  */

static void
number_of_iterations_ne_max (mpz_t bnd, bool no_overflow, tree c, tree s,
			     bounds *bnds, bool exit_must_be_taken)
{
  double_int max;
  mpz_t d;
  tree type = TREE_TYPE (c);
  bool bnds_u_valid = ((no_overflow && exit_must_be_taken)
		       || mpz_sgn (bnds->below) >= 0);

  if (integer_onep (s)
      || (TREE_CODE (c) == INTEGER_CST
	  && TREE_CODE (s) == INTEGER_CST
	  && tree_to_double_int (c).mod (tree_to_double_int (s),
					 TYPE_UNSIGNED (type),
					 EXACT_DIV_EXPR).is_zero ())
      || (TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (c))
	  && multiple_of_p (type, c, s)))
    {
      /* If C is an exact multiple of S, then its value will be reached before
	 the induction variable overflows (unless the loop is exited in some
	 other way before).  Note that the actual induction variable in the
	 loop (which ranges from base to final instead of from 0 to C) may
	 overflow, in which case BNDS.up will not be giving a correct upper
	 bound on C; thus, BNDS_U_VALID had to be computed in advance.  */
      no_overflow = true;
      exit_must_be_taken = true;
    }

  /* If the induction variable can overflow, the number of iterations is at
     most the period of the control variable (or infinite, but in that case
     the whole # of iterations analysis will fail).  */
  if (!no_overflow)
    {
      max = double_int::mask (TYPE_PRECISION (type)
			      - tree_low_cst (num_ending_zeros (s), 1));
      mpz_set_double_int (bnd, max, true);
      return;
    }

  /* Now we know that the induction variable does not overflow, so the loop
     iterates at most (range of type / S) times.  */
  mpz_set_double_int (bnd, double_int::mask (TYPE_PRECISION (type)), true);

  /* If the induction variable is guaranteed to reach the value of C before
     overflow, ... */
  if (exit_must_be_taken)
    {
      /* ... then we can strengthen this to C / S, and possibly we can use
	 the upper bound on C given by BNDS.  */
      if (TREE_CODE (c) == INTEGER_CST)
	mpz_set_double_int (bnd, tree_to_double_int (c), true);
      else if (bnds_u_valid)
	mpz_set (bnd, bnds->up);
    }

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
  number_of_iterations_ne_max (max, iv->no_overflow, c, s, bnds,
			       exit_must_be_taken);
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
			      fold_build_pointer_plus (iv1->base, tmod));
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
			      fold_build_pointer_plus (iv0->base,
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
     the computations in this formula are performed in full precision,
     i.e., without overflows).

     Usually, for loops with exit condition iv0->base + step * i < iv1->base,
     we have a condition of the form iv0->base - step < iv1->base before the loop,
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
      dstep = tree_to_double_int (iv1->step).sext (TYPE_PRECISION (type));
      dstep = -dstep;
    }

  mpz_init (mstep);
  mpz_set_double_int (mstep, dstep, true);
  mpz_neg (mstep, mstep);
  mpz_add_ui (mstep, mstep, 1);

  rolls_p = mpz_cmp (mstep, bnds->below) <= 0;

  mpz_init (max);
  mpz_set_double_int (max, double_int::mask (TYPE_PRECISION (type)), true);
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
	iv1->base = fold_build_pointer_plus_hwi (iv1->base, 1);
      else
	iv1->base = fold_build2 (PLUS_EXPR, type1, iv1->base,
				 build_int_cst (type1, 1));
    }
  else if (POINTER_TYPE_P (type))
    iv0->base = fold_build_pointer_plus_hwi (iv0->base, -1);
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

   if EVERY_ITERATION is true, we know the test is executed on every iteration.

   The results (number of iterations and assumptions as described in
   comments at struct tree_niter_desc in tree-flow.h) are stored to NITER.
   Returns false if it fails to determine number of iterations, true if it
   was determined (possibly with some assumptions).  */

static bool
number_of_iterations_cond (struct loop *loop,
			   tree type, affine_iv *iv0, enum tree_code code,
			   affine_iv *iv1, struct tree_niter_desc *niter,
			   bool only_exit, bool every_iteration)
{
  bool exit_must_be_taken = false, ret;
  bounds bnds;

  /* If the test is not executed every iteration, wrapping may make the test
     to pass again. 
     TODO: the overflow case can be still used as unreliable estimate of upper
     bound.  But we have no API to pass it down to number of iterations code
     and, at present, it will not use it anyway.  */
  if (!every_iteration
      && (!iv0->no_overflow || !iv1->no_overflow
	  || code == NE_EXPR || code == EQ_EXPR))
    return false;

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
      tree step_type = POINTER_TYPE_P (type) ? sizetype : type;
      if (code != NE_EXPR)
	return false;

      iv0->step = fold_binary_to_constant (MINUS_EXPR, step_type,
					   iv0->step, iv1->step);
      iv0->no_overflow = false;
      iv1->step = build_int_cst (step_type, 0);
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
  tree tem = fold_binary (code, boolean_type_node, iv0->base, iv1->base);
  if (tem && integer_zerop (tem))
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

  /* Do not bother to replace constants.  */
  if (CONSTANT_CLASS_P (old))
    return expr;

  if (expr == old
      || operand_equal_p (expr, old, 0))
    return unshare_expr (new_tree);

  if (!EXPR_P (expr))
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
  tree ret = NULL_TREE, e, ee, e1;
  enum tree_code code;
  gimple stmt;

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
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      basic_block src, dest;

      if (gimple_phi_num_args (stmt) != 1)
	return expr;
      e = PHI_ARG_DEF (stmt, 0);

      /* Avoid propagating through loop exit phi nodes, which
	 could break loop-closed SSA form restrictions.  */
      dest = gimple_bb (stmt);
      src = single_pred (dest);
      if (TREE_CODE (e) == SSA_NAME
	  && src->loop_father != dest->loop_father)
	return expr;

      return expand_simple_operations (e);
    }
  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return expr;

  /* Avoid expanding to expressions that contain SSA names that need
     to take part in abnormal coalescing.  */
  ssa_op_iter iter;
  FOR_EACH_SSA_TREE_OPERAND (e, stmt, iter, SSA_OP_USE)
    if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (e))
      return expr;

  e = gimple_assign_rhs1 (stmt);
  code = gimple_assign_rhs_code (stmt);
  if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS)
    {
      if (is_gimple_min_invariant (e))
	return e;

      if (code == SSA_NAME)
	return expand_simple_operations (e);

      return expr;
    }

  switch (code)
    {
    CASE_CONVERT:
      /* Casts are simple.  */
      ee = expand_simple_operations (e);
      return fold_build1 (code, TREE_TYPE (expr), ee);

    case PLUS_EXPR:
    case MINUS_EXPR:
    case POINTER_PLUS_EXPR:
      /* And increments and decrements by a constant are simple.  */
      e1 = gimple_assign_rhs2 (stmt);
      if (!is_gimple_min_invariant (e1))
	return expr;

      ee = expand_simple_operations (e);
      return fold_build2 (code, TREE_TYPE (expr), ee, e1);

    default:
      return expr;
    }
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
  gimple stmt;
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

      stmt = last_stmt (e->src);
      cond = fold_build2 (gimple_cond_code (stmt),
			  boolean_type_node,
			  gimple_cond_lhs (stmt),
			  gimple_cond_rhs (stmt));
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
  gimple_stmt_iterator bsi;
  unsigned i;
  gimple call;

  if (exit != single_exit (loop))
    return false;

  body = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    {
      for (bsi = gsi_start_bb (body[i]); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  call = gsi_stmt (bsi);
	  if (gimple_code (call) != GIMPLE_CALL)
	    continue;

	  if (gimple_has_side_effects (call))
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
   potentially unsafe assumptions.  
   When EVERY_ITERATION is true, only tests that are known to be executed
   every iteration are considered (i.e. only test that alone bounds the loop). 
 */

bool
number_of_iterations_exit (struct loop *loop, edge exit,
			   struct tree_niter_desc *niter,
			   bool warn, bool every_iteration)
{
  gimple stmt;
  tree type;
  tree op0, op1;
  enum tree_code code;
  affine_iv iv0, iv1;
  bool safe;

  safe = dominated_by_p (CDI_DOMINATORS, loop->latch, exit->src);

  if (every_iteration && !safe)
    return false;

  niter->assumptions = boolean_false_node;
  stmt = last_stmt (exit->src);
  if (!stmt || gimple_code (stmt) != GIMPLE_COND)
    return false;

  /* We want the condition for staying inside loop.  */
  code = gimple_cond_code (stmt);
  if (exit->flags & EDGE_TRUE_VALUE)
    code = invert_tree_comparison (code, false);

  switch (code)
    {
    case GT_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case NE_EXPR:
      break;

    default:
      return false;
    }

  op0 = gimple_cond_lhs (stmt);
  op1 = gimple_cond_rhs (stmt);
  type = TREE_TYPE (op0);

  if (TREE_CODE (type) != INTEGER_TYPE
      && !POINTER_TYPE_P (type))
    return false;

  if (!simple_iv (loop, loop_containing_stmt (stmt), op0, &iv0, false))
    return false;
  if (!simple_iv (loop, loop_containing_stmt (stmt), op1, &iv1, false))
    return false;

  /* We don't want to see undefined signed overflow warnings while
     computing the number of iterations.  */
  fold_defer_overflow_warnings ();

  iv0.base = expand_simple_operations (iv0.base);
  iv1.base = expand_simple_operations (iv1.base);
  if (!number_of_iterations_cond (loop, type, &iv0, code, &iv1, niter,
				  loop_only_exit_p (loop, exit), safe))
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

  /* If NITER has simplified into a constant, update MAX.  */
  if (TREE_CODE (niter->niter) == INTEGER_CST)
    niter->max = tree_to_double_int (niter->niter);

  if (integer_onep (niter->assumptions))
    return true;

  /* With -funsafe-loop-optimizations we assume that nothing bad can happen.
     But if we can prove that there is overflow or some other source of weird
     behavior, ignore the loop even with -funsafe-loop-optimizations.  */
  if (integer_zerop (niter->assumptions) || !single_exit (loop))
    return false;

  if (flag_unsafe_loop_optimizations)
    niter->assumptions = boolean_true_node;

  if (warn)
    {
      const char *wording;
      location_t loc = gimple_location (stmt);

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

      warning_at ((LOCATION_LINE (loc) > 0) ? loc : input_location,
		  OPT_Wunsafe_loop_optimizations, "%s", gettext (wording));
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
  vec<edge> exits = get_loop_exit_edges (loop);
  edge ex;
  tree niter = NULL_TREE, aniter;
  struct tree_niter_desc desc;

  *exit = NULL;
  FOR_EACH_VEC_ELT (exits, i, ex)
    {
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
  exits.release ();

  return niter ? niter : chrec_dont_know;
}

/* Return true if loop is known to have bounded number of iterations.  */

bool
finite_loop_p (struct loop *loop)
{
  double_int nit;
  int flags;

  if (flag_unsafe_loop_optimizations)
    return true;
  flags = flags_from_decl_or_type (current_function_decl);
  if ((flags & (ECF_CONST|ECF_PURE)) && !(flags & ECF_LOOPING_CONST_OR_PURE))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Found loop %i to be finite: it is within pure or const function.\n",
		 loop->num);
      return true;
    }

  if (loop->any_upper_bound
      || max_loop_iterations (loop, &nit))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Found loop %i to be finite: upper bound found.\n",
		 loop->num);
      return true;
    }
  return false;
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

static gimple
chain_of_csts_start (struct loop *loop, tree x)
{
  gimple stmt = SSA_NAME_DEF_STMT (x);
  tree use;
  basic_block bb = gimple_bb (stmt);
  enum tree_code code;

  if (!bb
      || !flow_bb_inside_loop_p (loop, bb))
    return NULL;

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      if (bb == loop->header)
	return stmt;

      return NULL;
    }

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return NULL;

  code = gimple_assign_rhs_code (stmt);
  if (gimple_references_memory_p (stmt)
      || TREE_CODE_CLASS (code) == tcc_reference
      || (code == ADDR_EXPR
	  && !is_gimple_min_invariant (gimple_assign_rhs1 (stmt))))
    return NULL;

  use = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_USE);
  if (use == NULL_TREE)
    return NULL;

  return chain_of_csts_start (loop, use);
}

/* Determines whether the expression X is derived from a result of a phi node
   in header of LOOP such that

   * the derivation of X consists only from operations with constants
   * the initial value of the phi node is constant
   * the value of the phi node in the next iteration can be derived from the
     value in the current iteration by a chain of operations with constants.

   If such phi node exists, it is returned, otherwise NULL is returned.  */

static gimple
get_base_for (struct loop *loop, tree x)
{
  gimple phi;
  tree init, next;

  if (is_gimple_min_invariant (x))
    return NULL;

  phi = chain_of_csts_start (loop, x);
  if (!phi)
    return NULL;

  init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
  next = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));

  if (TREE_CODE (next) != SSA_NAME)
    return NULL;

  if (!is_gimple_min_invariant (init))
    return NULL;

  if (chain_of_csts_start (loop, next) != phi)
    return NULL;

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
  gimple stmt;

  gcc_assert (is_gimple_min_invariant (base));

  if (!x)
    return base;

  stmt = SSA_NAME_DEF_STMT (x);
  if (gimple_code (stmt) == GIMPLE_PHI)
    return base;

  gcc_assert (is_gimple_assign (stmt));

  /* STMT must be either an assignment of a single SSA name or an
     expression involving an SSA name and a constant.  Try to fold that
     expression using the value for the SSA name.  */
  if (gimple_assign_ssa_name_copy_p (stmt))
    return get_val_for (gimple_assign_rhs1 (stmt), base);
  else if (gimple_assign_rhs_class (stmt) == GIMPLE_UNARY_RHS
	   && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    {
      return fold_build1 (gimple_assign_rhs_code (stmt),
			  gimple_expr_type (stmt),
			  get_val_for (gimple_assign_rhs1 (stmt), base));
    }
  else if (gimple_assign_rhs_class (stmt) == GIMPLE_BINARY_RHS)
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      if (TREE_CODE (rhs1) == SSA_NAME)
	rhs1 = get_val_for (rhs1, base);
      else if (TREE_CODE (rhs2) == SSA_NAME)
	rhs2 = get_val_for (rhs2, base);
      else
	gcc_unreachable ();
      return fold_build2 (gimple_assign_rhs_code (stmt),
			  gimple_expr_type (stmt), rhs1, rhs2);
    }
  else
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
  tree acnd;
  tree op[2], val[2], next[2], aval[2];
  gimple phi, cond;
  unsigned i, j;
  enum tree_code cmp;

  cond = last_stmt (exit->src);
  if (!cond || gimple_code (cond) != GIMPLE_COND)
    return chrec_dont_know;

  cmp = gimple_cond_code (cond);
  if (exit->flags & EDGE_TRUE_VALUE)
    cmp = invert_tree_comparison (cmp, false);

  switch (cmp)
    {
    case EQ_EXPR:
    case NE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
      op[0] = gimple_cond_lhs (cond);
      op[1] = gimple_cond_rhs (cond);
      break;

    default:
      return chrec_dont_know;
    }

  for (j = 0; j < 2; j++)
    {
      if (is_gimple_min_invariant (op[j]))
	{
	  val[j] = op[j];
	  next[j] = NULL_TREE;
	  op[j] = NULL_TREE;
	}
      else
	{
	  phi = get_base_for (loop, op[j]);
	  if (!phi)
	    return chrec_dont_know;
	  val[j] = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
	  next[j] = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));
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
  vec<edge> exits = get_loop_exit_edges (loop);
  edge ex;
  tree niter = NULL_TREE, aniter;

  *exit = NULL;

  /* Loops with multiple exits are expensive to handle and less important.  */
  if (!flag_expensive_optimizations
      && exits.length () > 1)
    {
      exits.release ();
      return chrec_dont_know;
    }

  FOR_EACH_VEC_ELT (exits, i, ex)
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
  exits.release ();

  return niter ? niter : chrec_dont_know;
}

/*

   Analysis of upper bounds on number of iterations of a loop.

*/

static double_int derive_constant_upper_bound_ops (tree, tree,
						   enum tree_code, tree);

/* Returns a constant upper bound on the value of the right-hand side of
   an assignment statement STMT.  */

static double_int
derive_constant_upper_bound_assign (gimple stmt)
{
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);

  return derive_constant_upper_bound_ops (TREE_TYPE (gimple_assign_lhs (stmt)),
					  op0, code, op1);
}

/* Returns a constant upper bound on the value of expression VAL.  VAL
   is considered to be unsigned.  If its type is signed, its value must
   be nonnegative.  */

static double_int
derive_constant_upper_bound (tree val)
{
  enum tree_code code;
  tree op0, op1;

  extract_ops_from_tree (val, &code, &op0, &op1);
  return derive_constant_upper_bound_ops (TREE_TYPE (val), op0, code, op1);
}

/* Returns a constant upper bound on the value of expression OP0 CODE OP1,
   whose type is TYPE.  The expression is considered to be unsigned.  If
   its type is signed, its value must be nonnegative.  */

static double_int
derive_constant_upper_bound_ops (tree type, tree op0,
				 enum tree_code code, tree op1)
{
  tree subtype, maxt;
  double_int bnd, max, mmax, cst;
  gimple stmt;

  if (INTEGRAL_TYPE_P (type))
    maxt = TYPE_MAX_VALUE (type);
  else
    maxt = upper_bound_in_type (type, type);

  max = tree_to_double_int (maxt);

  switch (code)
    {
    case INTEGER_CST:
      return tree_to_double_int (op0);

    CASE_CONVERT:
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
      if (max.ult (bnd))
	return max;

      return bnd;

    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      if (TREE_CODE (op1) != INTEGER_CST
	  || !tree_expr_nonnegative_p (op0))
	return max;

      /* Canonicalize to OP0 - CST.  Consider CST to be signed, in order to
	 choose the most logical way how to treat this constant regardless
	 of the signedness of the type.  */
      cst = tree_to_double_int (op1);
      cst = cst.sext (TYPE_PRECISION (type));
      if (code != MINUS_EXPR)
	cst = -cst;

      bnd = derive_constant_upper_bound (op0);

      if (cst.is_negative ())
	{
	  cst = -cst;
	  /* Avoid CST == 0x80000...  */
	  if (cst.is_negative ())
	    return max;;

	  /* OP0 + CST.  We need to check that
	     BND <= MAX (type) - CST.  */

	  mmax -= cst;
	  if (bnd.ugt (mmax))
	    return max;

	  return bnd + cst;
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
	  if (bnd.ult (cst))
	    return max;

	  if (TYPE_UNSIGNED (type))
	    {
	      tree tem = fold_binary (GE_EXPR, boolean_type_node, op0,
				      double_int_to_tree (type, cst));
	      if (!tem || integer_nonzerop (tem))
		return max;
	    }

	  bnd -= cst;
	}

      return bnd;

    case FLOOR_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (TREE_CODE (op1) != INTEGER_CST
	  || tree_int_cst_sign_bit (op1))
	return max;

      bnd = derive_constant_upper_bound (op0);
      return bnd.udiv (tree_to_double_int (op1), FLOOR_DIV_EXPR);

    case BIT_AND_EXPR:
      if (TREE_CODE (op1) != INTEGER_CST
	  || tree_int_cst_sign_bit (op1))
	return max;
      return tree_to_double_int (op1);

    case SSA_NAME:
      stmt = SSA_NAME_DEF_STMT (op0);
      if (gimple_code (stmt) != GIMPLE_ASSIGN
	  || gimple_assign_lhs (stmt) != op0)
	return max;
      return derive_constant_upper_bound_assign (stmt);

    default:
      return max;
    }
}

/* Records that every statement in LOOP is executed I_BOUND times.
   REALISTIC is true if I_BOUND is expected to be close to the real number
   of iterations.  UPPER is true if we are sure the loop iterates at most
   I_BOUND times.  */

void
record_niter_bound (struct loop *loop, double_int i_bound, bool realistic,
		    bool upper)
{
  /* Update the bounds only when there is no previous estimation, or when the
     current estimation is smaller.  */
  if (upper
      && (!loop->any_upper_bound
	  || i_bound.ult (loop->nb_iterations_upper_bound)))
    {
      loop->any_upper_bound = true;
      loop->nb_iterations_upper_bound = i_bound;
    }
  if (realistic
      && (!loop->any_estimate
	  || i_bound.ult (loop->nb_iterations_estimate)))
    {
      loop->any_estimate = true;
      loop->nb_iterations_estimate = i_bound;
    }

  /* If an upper bound is smaller than the realistic estimate of the
     number of iterations, use the upper bound instead.  */
  if (loop->any_upper_bound
      && loop->any_estimate
      && loop->nb_iterations_upper_bound.ult (loop->nb_iterations_estimate))
    loop->nb_iterations_estimate = loop->nb_iterations_upper_bound;
}

/* Emit a -Waggressive-loop-optimizations warning if needed.  */

static void
do_warn_aggressive_loop_optimizations (struct loop *loop,
				       double_int i_bound, gimple stmt)
{
  /* Don't warn if the loop doesn't have known constant bound.  */
  if (!loop->nb_iterations
      || TREE_CODE (loop->nb_iterations) != INTEGER_CST
      || !warn_aggressive_loop_optimizations
      /* To avoid warning multiple times for the same loop,
	 only start warning when we preserve loops.  */
      || (cfun->curr_properties & PROP_loops) == 0
      /* Only warn once per loop.  */
      || loop->warned_aggressive_loop_optimizations
      /* Only warn if undefined behavior gives us lower estimate than the
	 known constant bound.  */
      || i_bound.ucmp (tree_to_double_int (loop->nb_iterations)) >= 0
      /* And undefined behavior happens unconditionally.  */
      || !dominated_by_p (CDI_DOMINATORS, loop->latch, gimple_bb (stmt)))
    return;

  edge e = single_exit (loop);
  if (e == NULL)
    return;

  gimple estmt = last_stmt (e->src);
  if (warning_at (gimple_location (stmt), OPT_Waggressive_loop_optimizations,
		  "iteration %E invokes undefined behavior",
		  double_int_to_tree (TREE_TYPE (loop->nb_iterations),
				      i_bound)))
    inform (gimple_location (estmt), "containing loop");
  loop->warned_aggressive_loop_optimizations = true;
}

/* Records that AT_STMT is executed at most BOUND + 1 times in LOOP.  IS_EXIT
   is true if the loop is exited immediately after STMT, and this exit
   is taken at last when the STMT is executed BOUND + 1 times.
   REALISTIC is true if BOUND is expected to be close to the real number
   of iterations.  UPPER is true if we are sure the loop iterates at most
   BOUND times.  I_BOUND is an unsigned double_int upper estimate on BOUND.  */

static void
record_estimate (struct loop *loop, tree bound, double_int i_bound,
		 gimple at_stmt, bool is_exit, bool realistic, bool upper)
{
  double_int delta;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Statement %s", is_exit ? "(exit)" : "");
      print_gimple_stmt (dump_file, at_stmt, 0, TDF_SLIM);
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
  else
    gcc_checking_assert (i_bound == tree_to_double_int (bound));
  if (!upper && !realistic)
    return;

  /* If we have a guaranteed upper bound, record it in the appropriate
     list, unless this is an !is_exit bound (i.e. undefined behavior in
     at_stmt) in a loop with known constant number of iterations.  */
  if (upper
      && (is_exit
	  || loop->nb_iterations == NULL_TREE
	  || TREE_CODE (loop->nb_iterations) != INTEGER_CST))
    {
      struct nb_iter_bound *elt = ggc_alloc_nb_iter_bound ();

      elt->bound = i_bound;
      elt->stmt = at_stmt;
      elt->is_exit = is_exit;
      elt->next = loop->bounds;
      loop->bounds = elt;
    }

  /* If statement is executed on every path to the loop latch, we can directly
     infer the upper bound on the # of iterations of the loop.  */
  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, gimple_bb (at_stmt)))
    return;

  /* Update the number of iteration estimates according to the bound.
     If at_stmt is an exit then the loop latch is executed at most BOUND times,
     otherwise it can be executed BOUND + 1 times.  We will lower the estimate
     later if such statement must be executed on last iteration  */
  if (is_exit)
    delta = double_int_zero;
  else
    delta = double_int_one;
  i_bound += delta;

  /* If an overflow occurred, ignore the result.  */
  if (i_bound.ult (delta))
    return;

  if (upper && !is_exit)
    do_warn_aggressive_loop_optimizations (loop, i_bound, at_stmt);
  record_niter_bound (loop, i_bound, realistic, upper);
}

/* Record the estimate on number of iterations of LOOP based on the fact that
   the induction variable BASE + STEP * i evaluated in STMT does not wrap and
   its values belong to the range <LOW, HIGH>.  REALISTIC is true if the
   estimated number of iterations is expected to be close to the real one.
   UPPER is true if we are sure the induction variable does not wrap.  */

static void
record_nonwrapping_iv (struct loop *loop, tree base, tree step, gimple stmt,
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
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
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

/* Determine information about number of iterations a LOOP from the index
   IDX of a data reference accessed in STMT.  RELIABLE is true if STMT is
   guaranteed to be executed in every iteration of LOOP.  Callback for
   for_each_index.  */

struct ilb_data
{
  struct loop *loop;
  gimple stmt;
};

static bool
idx_infer_loop_bounds (tree base, tree *idx, void *dta)
{
  struct ilb_data *data = (struct ilb_data *) dta;
  tree ev, init, step;
  tree low, high, type, next;
  bool sign, upper = true, at_end = false;
  struct loop *loop = data->loop;
  bool reliable = true;

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

  struct loop *dloop = loop_containing_stmt (data->stmt);
  if (!dloop)
    return true;

  ev = analyze_scalar_evolution (dloop, *idx);
  ev = instantiate_parameters (loop, ev);
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

  /* If access is not executed on every iteration, we must ensure that overlow may
     not make the access valid later.  */
  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, gimple_bb (data->stmt))
      && scev_probably_wraps_p (initial_condition_in_loop_num (ev, loop->num),
				step, data->stmt, loop, true))
    reliable = false;

  record_nonwrapping_iv (loop, init, step, data->stmt, low, high, reliable, upper);
  return true;
}

/* Determine information about number of iterations a LOOP from the bounds
   of arrays in the data reference REF accessed in STMT.  RELIABLE is true if
   STMT is guaranteed to be executed in every iteration of LOOP.*/

static void
infer_loop_bounds_from_ref (struct loop *loop, gimple stmt, tree ref)
{
  struct ilb_data data;

  data.loop = loop;
  data.stmt = stmt;
  for_each_index (&ref, idx_infer_loop_bounds, &data);
}

/* Determine information about number of iterations of a LOOP from the way
   arrays are used in STMT.  RELIABLE is true if STMT is guaranteed to be
   executed in every iteration of LOOP.  */

static void
infer_loop_bounds_from_array (struct loop *loop, gimple stmt)
{
  if (is_gimple_assign (stmt))
    {
      tree op0 = gimple_assign_lhs (stmt);
      tree op1 = gimple_assign_rhs1 (stmt);

      /* For each memory access, analyze its access function
	 and record a bound on the loop iteration domain.  */
      if (REFERENCE_CLASS_P (op0))
	infer_loop_bounds_from_ref (loop, stmt, op0);

      if (REFERENCE_CLASS_P (op1))
	infer_loop_bounds_from_ref (loop, stmt, op1);
    }
  else if (is_gimple_call (stmt))
    {
      tree arg, lhs;
      unsigned i, n = gimple_call_num_args (stmt);

      lhs = gimple_call_lhs (stmt);
      if (lhs && REFERENCE_CLASS_P (lhs))
	infer_loop_bounds_from_ref (loop, stmt, lhs);

      for (i = 0; i < n; i++)
	{
	  arg = gimple_call_arg (stmt, i);
	  if (REFERENCE_CLASS_P (arg))
	    infer_loop_bounds_from_ref (loop, stmt, arg);
	}
    }
}

/* Determine information about number of iterations of a LOOP from the fact
   that pointer arithmetics in STMT does not overflow.  */

static void
infer_loop_bounds_from_pointer_arith (struct loop *loop, gimple stmt)
{
  tree def, base, step, scev, type, low, high;
  tree var, ptr;

  if (!is_gimple_assign (stmt)
      || gimple_assign_rhs_code (stmt) != POINTER_PLUS_EXPR)
    return;

  def = gimple_assign_lhs (stmt);
  if (TREE_CODE (def) != SSA_NAME)
    return;

  type = TREE_TYPE (def);
  if (!nowrap_type_p (type))
    return;

  ptr = gimple_assign_rhs1 (stmt);
  if (!expr_invariant_in_loop_p (loop, ptr))
    return;

  var = gimple_assign_rhs2 (stmt);
  if (TYPE_PRECISION (type) != TYPE_PRECISION (TREE_TYPE (var)))
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

  /* In C, pointer arithmetic p + 1 cannot use a NULL pointer, and p - 1 cannot
     produce a NULL pointer.  The contrary would mean NULL points to an object,
     while NULL is supposed to compare unequal with the address of all objects.
     Furthermore, p + 1 cannot produce a NULL pointer and p - 1 cannot use a
     NULL pointer since that would mean wrapping, which we assume here not to
     happen.  So, we can exclude NULL from the valid range of pointer
     arithmetic.  */
  if (flag_delete_null_pointer_checks && int_cst_value (low) == 0)
    low = build_int_cstu (TREE_TYPE (low), TYPE_ALIGN_UNIT (TREE_TYPE (type)));

  record_nonwrapping_iv (loop, base, step, stmt, low, high, false, true);
}

/* Determine information about number of iterations of a LOOP from the fact
   that signed arithmetics in STMT does not overflow.  */

static void
infer_loop_bounds_from_signedness (struct loop *loop, gimple stmt)
{
  tree def, base, step, scev, type, low, high;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return;

  def = gimple_assign_lhs (stmt);

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
  gimple_stmt_iterator bsi;
  basic_block bb;
  bool reliable;

  bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];

      /* If BB is not executed in each iteration of the loop, we cannot
	 use the operations in it to infer reliable upper bound on the
	 # of iterations of the loop.  However, we can use it as a guess. 
	 Reliable guesses come only from array bounds.  */
      reliable = dominated_by_p (CDI_DOMINATORS, loop->latch, bb);

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple stmt = gsi_stmt (bsi);

	  infer_loop_bounds_from_array (loop, stmt);

	  if (reliable)
            {
              infer_loop_bounds_from_signedness (loop, stmt);
              infer_loop_bounds_from_pointer_arith (loop, stmt);
            }
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

/* Compare double ints, callback for qsort.  */

int
double_int_cmp (const void *p1, const void *p2)
{
  const double_int *d1 = (const double_int *)p1;
  const double_int *d2 = (const double_int *)p2;
  if (*d1 == *d2)
    return 0;
  if (d1->ult (*d2))
    return -1;
  return 1;
}

/* Return index of BOUND in BOUNDS array sorted in increasing order.
   Lookup by binary search.  */

int
bound_index (vec<double_int> bounds, double_int bound)
{
  unsigned int end = bounds.length ();
  unsigned int begin = 0;

  /* Find a matching index by means of a binary search.  */
  while (begin != end)
    {
      unsigned int middle = (begin + end) / 2;
      double_int index = bounds[middle];

      if (index == bound)
	return middle;
      else if (index.ult (bound))
	begin = middle + 1;
      else
	end = middle;
    }
  gcc_unreachable ();
}

/* We recorded loop bounds only for statements dominating loop latch (and thus
   executed each loop iteration).  If there are any bounds on statements not
   dominating the loop latch we can improve the estimate by walking the loop
   body and seeing if every path from loop header to loop latch contains
   some bounded statement.  */

static void
discover_iteration_bound_by_body_walk (struct loop *loop)
{
  pointer_map_t *bb_bounds;
  struct nb_iter_bound *elt;
  vec<double_int> bounds = vNULL;
  vec<vec<basic_block> > queues = vNULL;
  vec<basic_block> queue = vNULL;
  ptrdiff_t queue_index;
  ptrdiff_t latch_index = 0;
  pointer_map_t *block_priority;

  /* Discover what bounds may interest us.  */
  for (elt = loop->bounds; elt; elt = elt->next)
    {
      double_int bound = elt->bound;

      /* Exit terminates loop at given iteration, while non-exits produce undefined
	 effect on the next iteration.  */
      if (!elt->is_exit)
	{
	  bound += double_int_one;
	  /* If an overflow occurred, ignore the result.  */
	  if (bound.is_zero ())
	    continue;
	}

      if (!loop->any_upper_bound
	  || bound.ult (loop->nb_iterations_upper_bound))
        bounds.safe_push (bound);
    }

  /* Exit early if there is nothing to do.  */
  if (!bounds.exists ())
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, " Trying to walk loop body to reduce the bound.\n");

  /* Sort the bounds in decreasing order.  */
  qsort (bounds.address (), bounds.length (),
	 sizeof (double_int), double_int_cmp);

  /* For every basic block record the lowest bound that is guaranteed to
     terminate the loop.  */

  bb_bounds = pointer_map_create ();
  for (elt = loop->bounds; elt; elt = elt->next)
    {
      double_int bound = elt->bound;
      if (!elt->is_exit)
	{
	  bound += double_int_one;
	  /* If an overflow occurred, ignore the result.  */
	  if (bound.is_zero ())
	    continue;
	}

      if (!loop->any_upper_bound
	  || bound.ult (loop->nb_iterations_upper_bound))
	{
	  ptrdiff_t index = bound_index (bounds, bound);
	  void **entry = pointer_map_contains (bb_bounds,
					       gimple_bb (elt->stmt));
	  if (!entry)
	    *pointer_map_insert (bb_bounds,
				 gimple_bb (elt->stmt)) = (void *)index;
	  else if ((ptrdiff_t)*entry > index)
	    *entry = (void *)index;
	}
    }

  block_priority = pointer_map_create ();

  /* Perform shortest path discovery loop->header ... loop->latch.

     The "distance" is given by the smallest loop bound of basic block
     present in the path and we look for path with largest smallest bound
     on it.

     To avoid the need for fibonacci heap on double ints we simply compress
     double ints into indexes to BOUNDS array and then represent the queue
     as arrays of queues for every index.
     Index of BOUNDS.length() means that the execution of given BB has
     no bounds determined.

     VISITED is a pointer map translating basic block into smallest index
     it was inserted into the priority queue with.  */
  latch_index = -1;

  /* Start walk in loop header with index set to infinite bound.  */
  queue_index = bounds.length ();
  queues.safe_grow_cleared (queue_index + 1);
  queue.safe_push (loop->header);
  queues[queue_index] = queue;
  *pointer_map_insert (block_priority, loop->header) = (void *)queue_index;

  for (; queue_index >= 0; queue_index--)
    {
      if (latch_index < queue_index)
	{
	  while (queues[queue_index].length ())
	    {
	      basic_block bb;
	      ptrdiff_t bound_index = queue_index;
	      void **entry;
              edge e;
              edge_iterator ei;

	      queue = queues[queue_index];
	      bb = queue.pop ();

	      /* OK, we later inserted the BB with lower priority, skip it.  */
	      if ((ptrdiff_t)*pointer_map_contains (block_priority, bb) > queue_index)
		continue;

	      /* See if we can improve the bound.  */
	      entry = pointer_map_contains (bb_bounds, bb);
	      if (entry && (ptrdiff_t)*entry < bound_index)
		bound_index = (ptrdiff_t)*entry;

	      /* Insert succesors into the queue, watch for latch edge
		 and record greatest index we saw.  */
	      FOR_EACH_EDGE (e, ei, bb->succs)
		{
		  bool insert = false;
		  void **entry;

		  if (loop_exit_edge_p (loop, e))
		    continue;

		  if (e == loop_latch_edge (loop)
		      && latch_index < bound_index)
		    latch_index = bound_index;
		  else if (!(entry = pointer_map_contains (block_priority, e->dest)))
		    {
		      insert = true;
		      *pointer_map_insert (block_priority, e->dest) = (void *)bound_index;
		    }
		  else if ((ptrdiff_t)*entry < bound_index)
		    {
		      insert = true;
		      *entry = (void *)bound_index;
		    }
		    
		  if (insert)
		    queues[bound_index].safe_push (e->dest);
		}
	    }
	}
      queues[queue_index].release ();
    }

  gcc_assert (latch_index >= 0);
  if ((unsigned)latch_index < bounds.length ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Found better loop bound ");
	  dump_double_int (dump_file, bounds[latch_index], true);
	  fprintf (dump_file, "\n");
	}
      record_niter_bound (loop, bounds[latch_index], false, true);
    }

  queues.release ();
  bounds.release ();
  pointer_map_destroy (bb_bounds);
  pointer_map_destroy (block_priority);
}

/* See if every path cross the loop goes through a statement that is known
   to not execute at the last iteration. In that case we can decrese iteration
   count by 1.  */

static void
maybe_lower_iteration_bound (struct loop *loop)
{
  pointer_set_t *not_executed_last_iteration = NULL;
  struct nb_iter_bound *elt;
  bool found_exit = false;
  vec<basic_block> queue = vNULL;
  bitmap visited;

  /* Collect all statements with interesting (i.e. lower than
     nb_iterations_upper_bound) bound on them. 

     TODO: Due to the way record_estimate choose estimates to store, the bounds
     will be always nb_iterations_upper_bound-1.  We can change this to record
     also statements not dominating the loop latch and update the walk bellow
     to the shortest path algorthm.  */
  for (elt = loop->bounds; elt; elt = elt->next)
    {
      if (!elt->is_exit
	  && elt->bound.ult (loop->nb_iterations_upper_bound))
	{
	  if (!not_executed_last_iteration)
	    not_executed_last_iteration = pointer_set_create ();
	  pointer_set_insert (not_executed_last_iteration, elt->stmt);
	}
    }
  if (!not_executed_last_iteration)
    return;

  /* Start DFS walk in the loop header and see if we can reach the
     loop latch or any of the exits (including statements with side
     effects that may terminate the loop otherwise) without visiting
     any of the statements known to have undefined effect on the last
     iteration.  */
  queue.safe_push (loop->header);
  visited = BITMAP_ALLOC (NULL);
  bitmap_set_bit (visited, loop->header->index);
  found_exit = false;

  do
    {
      basic_block bb = queue.pop ();
      gimple_stmt_iterator gsi;
      bool stmt_found = false;

      /* Loop for possible exits and statements bounding the execution.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  if (pointer_set_contains (not_executed_last_iteration, stmt))
	    {
	      stmt_found = true;
	      break;
	    }
	  if (gimple_has_side_effects (stmt))
	    {
	      found_exit = true;
	      break;
	    }
	}
      if (found_exit)
	break;

      /* If no bounding statement is found, continue the walk.  */
      if (!stmt_found)
	{
          edge e;
          edge_iterator ei;

          FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if (loop_exit_edge_p (loop, e)
		  || e == loop_latch_edge (loop))
		{
		  found_exit = true;
		  break;
		}
	      if (bitmap_set_bit (visited, e->dest->index))
		queue.safe_push (e->dest);
	    }
	}
    }
  while (queue.length () && !found_exit);

  /* If every path through the loop reach bounding statement before exit,
     then we know the last iteration of the loop will have undefined effect
     and we can decrease number of iterations.  */
    
  if (!found_exit)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Reducing loop iteration estimate by 1; "
		 "undefined statement must be executed at the last iteration.\n");
      record_niter_bound (loop, loop->nb_iterations_upper_bound - double_int_one,
			  false, true);
    }
  BITMAP_FREE (visited);
  queue.release ();
  pointer_set_destroy (not_executed_last_iteration);
}

/* Records estimates on numbers of iterations of LOOP.  If USE_UNDEFINED_P
   is true also use estimates derived from undefined behavior.  */

void
estimate_numbers_of_iterations_loop (struct loop *loop)
{
  vec<edge> exits;
  tree niter, type;
  unsigned i;
  struct tree_niter_desc niter_desc;
  edge ex;
  double_int bound;
  edge likely_exit;

  /* Give up if we already have tried to compute an estimation.  */
  if (loop->estimate_state != EST_NOT_COMPUTED)
    return;

  loop->estimate_state = EST_AVAILABLE;
  /* Force estimate compuation but leave any existing upper bound in place.  */
  loop->any_estimate = false;

  /* Ensure that loop->nb_iterations is computed if possible.  If it turns out
     to be constant, we avoid undefined behavior implied bounds and instead
     diagnose those loops with -Waggressive-loop-optimizations.  */
  number_of_latch_executions (loop);

  exits = get_loop_exit_edges (loop);
  likely_exit = single_likely_exit (loop);
  FOR_EACH_VEC_ELT (exits, i, ex)
    {
      if (!number_of_iterations_exit (loop, ex, &niter_desc, false, false))
	continue;

      niter = niter_desc.niter;
      type = TREE_TYPE (niter);
      if (TREE_CODE (niter_desc.may_be_zero) != INTEGER_CST)
	niter = build3 (COND_EXPR, type, niter_desc.may_be_zero,
			build_int_cst (type, 0),
			niter);
      record_estimate (loop, niter, niter_desc.max,
		       last_stmt (ex->src),
		       true, ex == likely_exit, true);
    }
  exits.release ();

  if (flag_aggressive_loop_optimizations)
    infer_loop_bounds_from_undefined (loop);

  discover_iteration_bound_by_body_walk (loop);

  maybe_lower_iteration_bound (loop);

  /* If we have a measured profile, use it to estimate the number of
     iterations.  */
  if (loop->header->count != 0)
    {
      gcov_type nit = expected_loop_iterations_unbounded (loop) + 1;
      bound = gcov_type_to_double_int (nit);
      record_niter_bound (loop, bound, true, false);
    }

  /* If we know the exact number of iterations of this loop, try to
     not break code with undefined behavior by not recording smaller
     maximum number of iterations.  */
  if (loop->nb_iterations
      && TREE_CODE (loop->nb_iterations) == INTEGER_CST)
    {
      loop->any_upper_bound = true;
      loop->nb_iterations_upper_bound
	= tree_to_double_int (loop->nb_iterations);
    }
}

/* Sets NIT to the estimated number of executions of the latch of the
   LOOP.  If CONSERVATIVE is true, we must be sure that NIT is at least as
   large as the number of iterations.  If we have no reliable estimate,
   the function returns false, otherwise returns true.  */

bool
estimated_loop_iterations (struct loop *loop, double_int *nit)
{
  /* When SCEV information is available, try to update loop iterations
     estimate.  Otherwise just return whatever we recorded earlier.  */
  if (scev_initialized_p ())
    estimate_numbers_of_iterations_loop (loop);

  /* Even if the bound is not recorded, possibly we can derrive one from
     profile.  */
  if (!loop->any_estimate)
    {
      if (loop->header->count)
	{
          *nit = gcov_type_to_double_int
		   (expected_loop_iterations_unbounded (loop) + 1);
	  return true;
	}
      return false;
    }

  *nit = loop->nb_iterations_estimate;
  return true;
}

/* Sets NIT to an upper bound for the maximum number of executions of the
   latch of the LOOP.  If we have no reliable estimate, the function returns
   false, otherwise returns true.  */

bool
max_loop_iterations (struct loop *loop, double_int *nit)
{
  /* When SCEV information is available, try to update loop iterations
     estimate.  Otherwise just return whatever we recorded earlier.  */
  if (scev_initialized_p ())
    estimate_numbers_of_iterations_loop (loop);
  if (!loop->any_upper_bound)
    return false;

  *nit = loop->nb_iterations_upper_bound;
  return true;
}

/* Similar to estimated_loop_iterations, but returns the estimate only
   if it fits to HOST_WIDE_INT.  If this is not the case, or the estimate
   on the number of iterations of LOOP could not be derived, returns -1.  */

HOST_WIDE_INT
estimated_loop_iterations_int (struct loop *loop)
{
  double_int nit;
  HOST_WIDE_INT hwi_nit;

  if (!estimated_loop_iterations (loop, &nit))
    return -1;

  if (!nit.fits_shwi ())
    return -1;
  hwi_nit = nit.to_shwi ();

  return hwi_nit < 0 ? -1 : hwi_nit;
}

/* Similar to max_loop_iterations, but returns the estimate only
   if it fits to HOST_WIDE_INT.  If this is not the case, or the estimate
   on the number of iterations of LOOP could not be derived, returns -1.  */

HOST_WIDE_INT
max_loop_iterations_int (struct loop *loop)
{
  double_int nit;
  HOST_WIDE_INT hwi_nit;

  if (!max_loop_iterations (loop, &nit))
    return -1;

  if (!nit.fits_shwi ())
    return -1;
  hwi_nit = nit.to_shwi ();

  return hwi_nit < 0 ? -1 : hwi_nit;
}

/* Returns an upper bound on the number of executions of statements
   in the LOOP.  For statements before the loop exit, this exceeds
   the number of execution of the latch by one.  */

HOST_WIDE_INT
max_stmt_executions_int (struct loop *loop)
{
  HOST_WIDE_INT nit = max_loop_iterations_int (loop);
  HOST_WIDE_INT snit;

  if (nit == -1)
    return -1;

  snit = (HOST_WIDE_INT) ((unsigned HOST_WIDE_INT) nit + 1);

  /* If the computation overflows, return -1.  */
  return snit < 0 ? -1 : snit;
}

/* Returns an estimate for the number of executions of statements
   in the LOOP.  For statements before the loop exit, this exceeds
   the number of execution of the latch by one.  */

HOST_WIDE_INT
estimated_stmt_executions_int (struct loop *loop)
{
  HOST_WIDE_INT nit = estimated_loop_iterations_int (loop);
  HOST_WIDE_INT snit;

  if (nit == -1)
    return -1;

  snit = (HOST_WIDE_INT) ((unsigned HOST_WIDE_INT) nit + 1);

  /* If the computation overflows, return -1.  */
  return snit < 0 ? -1 : snit;
}

/* Sets NIT to the estimated maximum number of executions of the latch of the
   LOOP, plus one.  If we have no reliable estimate, the function returns
   false, otherwise returns true.  */

bool
max_stmt_executions (struct loop *loop, double_int *nit)
{
  double_int nit_minus_one;

  if (!max_loop_iterations (loop, nit))
    return false;

  nit_minus_one = *nit;

  *nit += double_int_one;

  return (*nit).ugt (nit_minus_one);
}

/* Sets NIT to the estimated number of executions of the latch of the
   LOOP, plus one.  If we have no reliable estimate, the function returns
   false, otherwise returns true.  */

bool
estimated_stmt_executions (struct loop *loop, double_int *nit)
{
  double_int nit_minus_one;

  if (!estimated_loop_iterations (loop, nit))
    return false;

  nit_minus_one = *nit;

  *nit += double_int_one;

  return (*nit).ugt (nit_minus_one);
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
stmt_dominates_stmt_p (gimple s1, gimple s2)
{
  basic_block bb1 = gimple_bb (s1), bb2 = gimple_bb (s2);

  if (!bb1
      || s1 == s2)
    return true;

  if (bb1 == bb2)
    {
      gimple_stmt_iterator bsi;

      if (gimple_code (s2) == GIMPLE_PHI)
	return false;

      if (gimple_code (s1) == GIMPLE_PHI)
	return true;

      for (bsi = gsi_start_bb (bb1); gsi_stmt (bsi) != s2; gsi_next (&bsi))
	if (gsi_stmt (bsi) == s1)
	  return true;

      return false;
    }

  return dominated_by_p (CDI_DOMINATORS, bb2, bb1);
}

/* Returns true when we can prove that the number of executions of
   STMT in the loop is at most NITER, according to the bound on
   the number of executions of the statement NITER_BOUND->stmt recorded in
   NITER_BOUND and fact that NITER_BOUND->stmt dominate STMT.

   ??? This code can become quite a CPU hog - we can have many bounds,
   and large basic block forcing stmt_dominates_stmt_p to be queried
   many times on a large basic blocks, so the whole thing is O(n^2)
   for scev_probably_wraps_p invocation (that can be done n times).

   It would make more sense (and give better answers) to remember BB
   bounds computed by discover_iteration_bound_by_body_walk.  */

static bool
n_of_executions_at_most (gimple stmt,
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

     -- if NITER_BOUND->is_exit is true, then everything after
	it at most NITER_BOUND->bound times.

     -- If NITER_BOUND->is_exit is false, then if we can prove that when STMT
	is executed, then NITER_BOUND->stmt is executed as well in the same
	iteration then STMT is executed at most NITER_BOUND->bound + 1 times. 

	If we can determine that NITER_BOUND->stmt is always executed
	after STMT, then STMT is executed at most NITER_BOUND->bound + 2 times.
	We conclude that if both statements belong to the same
	basic block and STMT is before NITER_BOUND->stmt and there are no
	statements with side effects in between.  */

  if (niter_bound->is_exit)
    {
      if (stmt == niter_bound->stmt
	  || !stmt_dominates_stmt_p (niter_bound->stmt, stmt))
	return false;
      cmp = GE_EXPR;
    }
  else
    {
      if (!stmt_dominates_stmt_p (niter_bound->stmt, stmt))
	{
          gimple_stmt_iterator bsi;
	  if (gimple_bb (stmt) != gimple_bb (niter_bound->stmt)
	      || gimple_code (stmt) == GIMPLE_PHI
	      || gimple_code (niter_bound->stmt) == GIMPLE_PHI)
	    return false;

	  /* By stmt_dominates_stmt_p we already know that STMT appears
	     before NITER_BOUND->STMT.  Still need to test that the loop
	     can not be terinated by a side effect in between.  */
	  for (bsi = gsi_for_stmt (stmt); gsi_stmt (bsi) != niter_bound->stmt;
	       gsi_next (&bsi))
	    if (gimple_has_side_effects (gsi_stmt (bsi)))
	       return false;
	  bound += double_int_one;
	  if (bound.is_zero ()
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
		       gimple at_stmt, struct loop *loop,
		       bool use_overflow_semantics)
{
  tree delta, step_abs;
  tree unsigned_type, valid_niter;
  tree type = TREE_TYPE (step);
  tree e;
  double_int niter;
  struct nb_iter_bound *bound;

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
  if (use_overflow_semantics && nowrap_type_p (TREE_TYPE (base)))
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

  if (max_loop_iterations (loop, &niter)
      && double_int_fits_to_tree_p (TREE_TYPE (valid_niter), niter)
      && (e = fold_binary (GT_EXPR, boolean_type_node, valid_niter,
			   double_int_to_tree (TREE_TYPE (valid_niter),
					       niter))) != NULL
      && integer_nonzerop (e))
    {
      fold_undefer_and_ignore_overflow_warnings ();
      return false;
    }
  if (at_stmt)
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
