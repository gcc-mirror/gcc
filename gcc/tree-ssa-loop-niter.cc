/* Functions to determine/estimate number of iterations of a loop.
   Copyright (C) 2004-2024 Free Software Foundation, Inc.

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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "stor-layout.h"
#include "fold-const.h"
#include "calls.h"
#include "intl.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "tree-dfa.h"
#include "internal-fn.h"
#include "gimple-range.h"
#include "sreal.h"


/* The maximum number of dominator BBs we search for conditions
   of loop header copies we use for simplifying a conditional
   expression.  */
#define MAX_DOMINATORS_TO_WALK 8

/*

   Analysis of number of iterations of an affine exit test.

*/

/* Bounds on some value, BELOW <= X <= UP.  */

struct bounds
{
  mpz_t below, up;
};

/* Splits expression EXPR to a variable part VAR and constant OFFSET.  */

static void
split_to_var_and_offset (tree expr, tree *var, mpz_t offset)
{
  tree type = TREE_TYPE (expr);
  tree op0, op1;
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
      wi::to_mpz (wi::to_wide (op1), offset, SIGNED);
      if (negate)
	mpz_neg (offset, offset);
      break;

    case INTEGER_CST:
      *var = build_int_cst_type (type, 0);
      wi::to_mpz (wi::to_wide (expr), offset, TYPE_SIGN (type));
      break;

    default:
      break;
    }
}

/* From condition C0 CMP C1 derives information regarding the value range
   of VAR, which is of TYPE.  Results are stored in to BELOW and UP.  */

static void
refine_value_range_using_guard (tree type, tree var,
				tree c0, enum tree_code cmp, tree c1,
				mpz_t below, mpz_t up)
{
  tree varc0, varc1, ctype;
  mpz_t offc0, offc1;
  mpz_t mint, maxt, minc1, maxc1;
  bool no_wrap = nowrap_type_p (type);
  bool c0_ok, c1_ok;
  signop sgn = TYPE_SIGN (type);

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
      /* We could derive quite precise information from EQ_EXPR, however,
	 such a guard is unlikely to appear, so we do not bother with
	 handling it.  */
      return;

    case NE_EXPR:
      /* NE_EXPR comparisons do not contain much of useful information,
	 except for cases of comparing with bounds.  */
      if (TREE_CODE (c1) != INTEGER_CST
	  || !INTEGRAL_TYPE_P (type))
	return;

      /* Ensure that the condition speaks about an expression in the same
	 type as X and Y.  */
      ctype = TREE_TYPE (c0);
      if (TYPE_PRECISION (ctype) != TYPE_PRECISION (type))
	return;
      c0 = fold_convert (type, c0);
      c1 = fold_convert (type, c1);

      if (operand_equal_p (var, c0, 0))
	{
	  /* Case of comparing VAR with its below/up bounds.  */
	  auto_mpz valc1;
	  wi::to_mpz (wi::to_wide (c1), valc1, TYPE_SIGN (type));
	  if (mpz_cmp (valc1, below) == 0)
	    cmp = GT_EXPR;
	  if (mpz_cmp (valc1, up) == 0)
	    cmp = LT_EXPR;
	}
      else
	{
	  /* Case of comparing with the bounds of the type.  */
	  wide_int min = wi::min_value (type);
	  wide_int max = wi::max_value (type);

	  if (wi::to_wide (c1) == min)
	    cmp = GT_EXPR;
	  if (wi::to_wide (c1) == max)
	    cmp = LT_EXPR;
	}

      /* Quick return if no useful information.  */
      if (cmp == NE_EXPR)
	return;

      break;

    default:
      return;
    }

  mpz_init (offc0);
  mpz_init (offc1);
  split_to_var_and_offset (expand_simple_operations (c0), &varc0, offc0);
  split_to_var_and_offset (expand_simple_operations (c1), &varc1, offc1);

  /* We are only interested in comparisons of expressions based on VAR.  */
  if (operand_equal_p (var, varc1, 0))
    {
      std::swap (varc0, varc1);
      mpz_swap (offc0, offc1);
      cmp = swap_tree_comparison (cmp);
    }
  else if (!operand_equal_p (var, varc0, 0))
    {
      mpz_clear (offc0);
      mpz_clear (offc1);
      return;
    }

  mpz_init (mint);
  mpz_init (maxt);
  get_type_static_bounds (type, mint, maxt);
  mpz_init (minc1);
  mpz_init (maxc1);
  Value_Range r (TREE_TYPE (varc1));
  /* Setup range information for varc1.  */
  if (integer_zerop (varc1))
    {
      wi::to_mpz (0, minc1, TYPE_SIGN (type));
      wi::to_mpz (0, maxc1, TYPE_SIGN (type));
    }
  else if (TREE_CODE (varc1) == SSA_NAME
	   && INTEGRAL_TYPE_P (type)
	   && get_range_query (cfun)->range_of_expr (r, varc1)
	   && !r.undefined_p ()
	   && !r.varying_p ())
    {
      gcc_assert (wi::le_p (r.lower_bound (), r.upper_bound (), sgn));
      wi::to_mpz (r.lower_bound (), minc1, sgn);
      wi::to_mpz (r.upper_bound (), maxc1, sgn);
    }
  else
    {
      mpz_set (minc1, mint);
      mpz_set (maxc1, maxt);
    }

  /* Compute valid range information for varc1 + offc1.  Note nothing
     useful can be derived if it overflows or underflows.  Overflow or
     underflow could happen when:

       offc1 > 0 && varc1 + offc1 > MAX_VAL (type)
       offc1 < 0 && varc1 + offc1 < MIN_VAL (type).  */
  mpz_add (minc1, minc1, offc1);
  mpz_add (maxc1, maxc1, offc1);
  c1_ok = (no_wrap
	   || mpz_sgn (offc1) == 0
	   || (mpz_sgn (offc1) < 0 && mpz_cmp (minc1, mint) >= 0)
	   || (mpz_sgn (offc1) > 0 && mpz_cmp (maxc1, maxt) <= 0));
  if (!c1_ok)
    goto end;

  if (mpz_cmp (minc1, mint) < 0)
    mpz_set (minc1, mint);
  if (mpz_cmp (maxc1, maxt) > 0)
    mpz_set (maxc1, maxt);

  if (cmp == LT_EXPR)
    {
      cmp = LE_EXPR;
      mpz_sub_ui (maxc1, maxc1, 1);
    }
  if (cmp == GT_EXPR)
    {
      cmp = GE_EXPR;
      mpz_add_ui (minc1, minc1, 1);
    }

  /* Compute range information for varc0.  If there is no overflow,
     the condition implied that

       (varc0) cmp (varc1 + offc1 - offc0)

     We can possibly improve the upper bound of varc0 if cmp is LE_EXPR,
     or the below bound if cmp is GE_EXPR.

     To prove there is no overflow/underflow, we need to check below
     four cases:
       1) cmp == LE_EXPR && offc0 > 0

	    (varc0 + offc0) doesn't overflow
	    && (varc1 + offc1 - offc0) doesn't underflow

       2) cmp == LE_EXPR && offc0 < 0

	    (varc0 + offc0) doesn't underflow
	    && (varc1 + offc1 - offc0) doesn't overfloe

	  In this case, (varc0 + offc0) will never underflow if we can
	  prove (varc1 + offc1 - offc0) doesn't overflow.

       3) cmp == GE_EXPR && offc0 < 0

	    (varc0 + offc0) doesn't underflow
	    && (varc1 + offc1 - offc0) doesn't overflow

       4) cmp == GE_EXPR && offc0 > 0

	    (varc0 + offc0) doesn't overflow
	    && (varc1 + offc1 - offc0) doesn't underflow

	  In this case, (varc0 + offc0) will never overflow if we can
	  prove (varc1 + offc1 - offc0) doesn't underflow.

     Note we only handle case 2 and 4 in below code.  */

  mpz_sub (minc1, minc1, offc0);
  mpz_sub (maxc1, maxc1, offc0);
  c0_ok = (no_wrap
	   || mpz_sgn (offc0) == 0
	   || (cmp == LE_EXPR
	       && mpz_sgn (offc0) < 0 && mpz_cmp (maxc1, maxt) <= 0)
	   || (cmp == GE_EXPR
	       && mpz_sgn (offc0) > 0 && mpz_cmp (minc1, mint) >= 0));
  if (!c0_ok)
    goto end;

  if (cmp == LE_EXPR)
    {
      if (mpz_cmp (up, maxc1) > 0)
	mpz_set (up, maxc1);
    }
  else
    {
      if (mpz_cmp (below, minc1) < 0)
	mpz_set (below, minc1);
    }

end:
  mpz_clear (mint);
  mpz_clear (maxt);
  mpz_clear (minc1);
  mpz_clear (maxc1);
  mpz_clear (offc0);
  mpz_clear (offc1);
}

/* Stores estimate on the minimum/maximum value of the expression VAR + OFF
   in TYPE to MIN and MAX.  */

static void
determine_value_range (class loop *loop, tree type, tree var, mpz_t off,
		       mpz_t min, mpz_t max)
{
  int cnt = 0;
  mpz_t minm, maxm;
  basic_block bb;
  wide_int minv, maxv;
  enum value_range_kind rtype = VR_VARYING;

  /* If the expression is a constant, we know its value exactly.  */
  if (integer_zerop (var))
    {
      mpz_set (min, off);
      mpz_set (max, off);
      return;
    }

  get_type_static_bounds (type, min, max);

  /* See if we have some range info from VRP.  */
  if (TREE_CODE (var) == SSA_NAME && INTEGRAL_TYPE_P (type))
    {
      edge e = loop_preheader_edge (loop);
      signop sgn = TYPE_SIGN (type);
      gphi_iterator gsi;

      /* Either for VAR itself...  */
      Value_Range var_range (TREE_TYPE (var));
      get_range_query (cfun)->range_of_expr (var_range, var);
      if (var_range.varying_p () || var_range.undefined_p ())
	rtype = VR_VARYING;
      else
	rtype = VR_RANGE;
      if (!var_range.undefined_p ())
	{
	  minv = var_range.lower_bound ();
	  maxv = var_range.upper_bound ();
	}

      /* Or for PHI results in loop->header where VAR is used as
	 PHI argument from the loop preheader edge.  */
      Value_Range phi_range (TREE_TYPE (var));
      for (gsi = gsi_start_phis (loop->header); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  if (PHI_ARG_DEF_FROM_EDGE (phi, e) == var
	      && get_range_query (cfun)->range_of_expr (phi_range,
						    gimple_phi_result (phi))
	      && !phi_range.varying_p ()
	      && !phi_range.undefined_p ())
	    {
	      if (rtype != VR_RANGE)
		{
		  rtype = VR_RANGE;
		  minv = phi_range.lower_bound ();
		  maxv = phi_range.upper_bound ();
		}
	      else
		{
		  minv = wi::max (minv, phi_range.lower_bound (), sgn);
		  maxv = wi::min (maxv, phi_range.upper_bound (), sgn);
		  /* If the PHI result range are inconsistent with
		     the VAR range, give up on looking at the PHI
		     results.  This can happen if VR_UNDEFINED is
		     involved.  */
		  if (wi::gt_p (minv, maxv, sgn))
		    {
		      Value_Range vr (TREE_TYPE (var));
		      get_range_query (cfun)->range_of_expr (vr, var);
		      if (vr.varying_p () || vr.undefined_p ())
			rtype = VR_VARYING;
		      else
			rtype = VR_RANGE;
		      if (!vr.undefined_p ())
			{
			  minv = vr.lower_bound ();
			  maxv = vr.upper_bound ();
			}
		      break;
		    }
		}
	    }
	}
      mpz_init (minm);
      mpz_init (maxm);
      if (rtype != VR_RANGE)
	{
	  mpz_set (minm, min);
	  mpz_set (maxm, max);
	}
      else
	{
	  gcc_assert (wi::le_p (minv, maxv, sgn));
	  wi::to_mpz (minv, minm, sgn);
	  wi::to_mpz (maxv, maxm, sgn);
	}
      /* Now walk the dominators of the loop header and use the entry
	 guards to refine the estimates.  */
      for (bb = loop->header;
	   bb != ENTRY_BLOCK_PTR_FOR_FN (cfun) && cnt < MAX_DOMINATORS_TO_WALK;
	   bb = get_immediate_dominator (CDI_DOMINATORS, bb))
	{
	  edge e;
	  tree c0, c1;
	  enum tree_code cmp;

	  if (!single_pred_p (bb))
	    continue;
	  e = single_pred_edge (bb);

	  if (!(e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
	    continue;

	  gcond *cond = as_a <gcond *> (*gsi_last_bb (e->src));
	  c0 = gimple_cond_lhs (cond);
	  cmp = gimple_cond_code (cond);
	  c1 = gimple_cond_rhs (cond);

	  if (e->flags & EDGE_FALSE_VALUE)
	    cmp = invert_tree_comparison (cmp, false);

	  refine_value_range_using_guard (type, var, c0, cmp, c1, minm, maxm);
	  ++cnt;
	}

      mpz_add (minm, minm, off);
      mpz_add (maxm, maxm, off);
      /* If the computation may not wrap or off is zero, then this
	 is always fine.  If off is negative and minv + off isn't
	 smaller than type's minimum, or off is positive and
	 maxv + off isn't bigger than type's maximum, use the more
	 precise range too.  */
      if (nowrap_type_p (type)
	  || mpz_sgn (off) == 0
	  || (mpz_sgn (off) < 0 && mpz_cmp (minm, min) >= 0)
	  || (mpz_sgn (off) > 0 && mpz_cmp (maxm, max) <= 0))
	{
	  mpz_set (min, minm);
	  mpz_set (max, maxm);
	  mpz_clear (minm);
	  mpz_clear (maxm);
	  return;
	}
      mpz_clear (minm);
      mpz_clear (maxm);
    }

  /* If the computation may wrap, we know nothing about the value, except for
     the range of the type.  */
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

  auto_mpz m;
  wi::to_mpz (wi::minus_one (TYPE_PRECISION (type)), m, UNSIGNED);
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
  tree varc0, varc1, ctype;
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
      std::swap (varc0, varc1);
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
      std::swap (varx, vary);
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
bound_difference (class loop *loop, tree x, tree y, bounds *bnds)
{
  tree type = TREE_TYPE (x);
  tree varx, vary;
  mpz_t offx, offy;
  int cnt = 0;
  edge e;
  basic_block bb;
  tree c0, c1;
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
      auto_mpz minx, maxx, miny, maxy;
      determine_value_range (loop, type, varx, offx, minx, maxx);
      determine_value_range (loop, type, vary, offy, miny, maxy);

      mpz_sub (bnds->below, minx, maxy);
      mpz_sub (bnds->up, maxx, miny);
    }

  /* If both X and Y are constants, we cannot get any more precise.  */
  if (integer_zerop (varx) && integer_zerop (vary))
    goto end;

  /* Now walk the dominators of the loop header and use the entry
     guards to refine the estimates.  */
  for (bb = loop->header;
       bb != ENTRY_BLOCK_PTR_FOR_FN (cfun) && cnt < MAX_DOMINATORS_TO_WALK;
       bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      if (!single_pred_p (bb))
	continue;
      e = single_pred_edge (bb);

      if (!(e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
	continue;

      gcond *cond = as_a <gcond *> (*gsi_last_bb (e->src));
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
bounds_add (bounds *bnds, const widest_int &delta, tree type)
{
  mpz_t mdelta, max;

  mpz_init (mdelta);
  wi::to_mpz (delta, mdelta, SIGNED);

  mpz_init (max);
  wi::to_mpz (wi::minus_one (TYPE_PRECISION (type)), max, UNSIGNED);

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
  widest_int max;
  mpz_t d;
  tree type = TREE_TYPE (c);
  bool bnds_u_valid = ((no_overflow && exit_must_be_taken)
		       || mpz_sgn (bnds->below) >= 0);

  if (integer_onep (s)
      || (TREE_CODE (c) == INTEGER_CST
	  && TREE_CODE (s) == INTEGER_CST
	  && wi::mod_trunc (wi::to_wide (c), wi::to_wide (s),
			    TYPE_SIGN (type)) == 0)
      || (TYPE_OVERFLOW_UNDEFINED (type)
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
      max = wi::mask <widest_int> (TYPE_PRECISION (type)
				   - wi::ctz (wi::to_wide (s)), false);
      wi::to_mpz (max, bnd, UNSIGNED);
      return;
    }

  /* Now we know that the induction variable does not overflow, so the loop
     iterates at most (range of type / S) times.  */
  wi::to_mpz (wi::minus_one (TYPE_PRECISION (type)), bnd, UNSIGNED);

  /* If the induction variable is guaranteed to reach the value of C before
     overflow, ... */
  if (exit_must_be_taken)
    {
      /* ... then we can strengthen this to C / S, and possibly we can use
	 the upper bound on C given by BNDS.  */
      if (TREE_CODE (c) == INTEGER_CST)
	wi::to_mpz (wi::to_wide (c), bnd, UNSIGNED);
      else if (bnds_u_valid)
	mpz_set (bnd, bnds->up);
    }

  mpz_init (d);
  wi::to_mpz (wi::to_wide (s), d, UNSIGNED);
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
number_of_iterations_ne (class loop *loop, tree type, affine_iv *iv,
			 tree final, class tree_niter_desc *niter,
			 bool exit_must_be_taken, bounds *bnds)
{
  tree niter_type = unsigned_type_for (type);
  tree s, c, d, bits, assumption, tmp, bound;

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

  auto_mpz max;
  number_of_iterations_ne_max (max, iv->no_overflow, c, s, bnds,
			       exit_must_be_taken);
  niter->max = widest_int::from (wi::from_mpz (niter_type, max, false),
				 TYPE_SIGN (niter_type));

  /* Compute no-overflow information for the control iv.  This can be
     proven when below two conditions are satisfied:

       1) IV evaluates toward FINAL at beginning, i.e:
	    base <= FINAL ; step > 0
	    base >= FINAL ; step < 0

       2) |FINAL - base| is an exact multiple of step.

     Unfortunately, it's hard to prove above conditions after pass loop-ch
     because loop with exit condition (IV != FINAL) usually will be guarded
     by initial-condition (IV.base - IV.step != FINAL).  In this case, we
     can alternatively try to prove below conditions:

       1') IV evaluates toward FINAL at beginning, i.e:
	    new_base = base - step < FINAL ; step > 0
					     && base - step doesn't underflow
	    new_base = base - step > FINAL ; step < 0
					     && base - step doesn't overflow

     Please refer to PR34114 as an example of loop-ch's impact.

     Note, for NE_EXPR, base equals to FINAL is a special case, in
     which the loop exits immediately, and the iv does not overflow.

     Also note, we prove condition 2) by checking base and final seperately
     along with condition 1) or 1').  Since we ensure the difference
     computation of c does not wrap with cond below and the adjusted s
     will fit a signed type as well as an unsigned we can safely do
     this using the type of the IV if it is not pointer typed.  */
  tree mtype = type;
  if (POINTER_TYPE_P (type))
    mtype = niter_type;
  if (!niter->control.no_overflow
      && (integer_onep (s)
	  || (multiple_of_p (mtype, fold_convert (mtype, iv->base),
			     fold_convert (mtype, s), false)
	      && multiple_of_p (mtype, fold_convert (mtype, final),
				fold_convert (mtype, s), false))))
    {
      tree t, cond, relaxed_cond = boolean_false_node;

      if (tree_int_cst_sign_bit (iv->step))
	{
	  cond = fold_build2 (GE_EXPR, boolean_type_node, iv->base, final);
	  if (TREE_CODE (type) == INTEGER_TYPE)
	    {
	      /* Only when base - step doesn't overflow.  */
	      t = TYPE_MAX_VALUE (type);
	      t = fold_build2 (PLUS_EXPR, type, t, iv->step);
	      t = fold_build2 (GE_EXPR, boolean_type_node, t, iv->base);
	      if (integer_nonzerop (t))
		{
		  t = fold_build2 (MINUS_EXPR, type, iv->base, iv->step);
		  relaxed_cond = fold_build2 (GT_EXPR, boolean_type_node, t,
					      final);
		}
	    }
	}
      else
	{
	  cond = fold_build2 (LE_EXPR, boolean_type_node, iv->base, final);
	  if (TREE_CODE (type) == INTEGER_TYPE)
	    {
	      /* Only when base - step doesn't underflow.  */
	      t = TYPE_MIN_VALUE (type);
	      t = fold_build2 (PLUS_EXPR, type, t, iv->step);
	      t = fold_build2 (LE_EXPR, boolean_type_node, t, iv->base);
	      if (integer_nonzerop (t))
		{
		  t = fold_build2 (MINUS_EXPR, type, iv->base, iv->step);
		  relaxed_cond = fold_build2 (LT_EXPR, boolean_type_node, t,
					      final);
		}
	    }
	}

      t = simplify_using_initial_conditions (loop, cond);
      if (!t || !integer_onep (t))
	t = simplify_using_initial_conditions (loop, relaxed_cond);

      if (t && integer_onep (t))
	{
	  niter->control.no_overflow = true;
	  niter->niter = fold_build2 (EXACT_DIV_EXPR, niter_type, c, s);
	  return true;
	}
    }

  /* Let nsd (step, size of mode) = d.  If d does not divide c, the loop
     is infinite.  Otherwise, the number of iterations is
     (inverse(s/d) * (c/d)) mod (size of mode/d).  */
  bits = num_ending_zeros (s);
  bound = build_low_bits_mask (niter_type,
			       (TYPE_PRECISION (niter_type)
				- tree_to_uhwi (bits)));

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
  if (integer_onep (s))
    {
      niter->niter = c;
    }
  else
    {
      tmp = fold_build2 (MULT_EXPR, niter_type, c, inverse (s, bound));
      niter->niter = fold_build2 (BIT_AND_EXPR, niter_type, tmp, bound);
    }
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
			       class tree_niter_desc *niter,
			       tree *delta, tree step,
			       bool exit_must_be_taken, bounds *bnds)
{
  tree niter_type = TREE_TYPE (step);
  tree mod = fold_build2 (FLOOR_MOD_EXPR, niter_type, *delta, step);
  tree tmod;
  tree assumption = boolean_true_node, bound, noloop;
  bool fv_comp_no_overflow;
  tree type1 = type;
  if (POINTER_TYPE_P (type))
    type1 = sizetype;

  if (TREE_CODE (mod) != INTEGER_CST)
    return false;
  if (integer_nonzerop (mod))
    mod = fold_build2 (MINUS_EXPR, niter_type, step, mod);
  tmod = fold_convert (type1, mod);

  auto_mpz mmod;
  wi::to_mpz (wi::to_wide (mod), mmod, UNSIGNED);
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
	    return false;
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
	    return false;
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
  bounds_add (bnds, wi::to_widest (mod), type);
  *delta = fold_build2 (PLUS_EXPR, niter_type, *delta, mod);

  return true;
}

/* Add assertions to NITER that ensure that the control variable of the loop
   with ending condition IV0 < IV1 does not overflow.  Types of IV0 and IV1
   are TYPE.  Returns false if we can prove that there is an overflow, true
   otherwise.  STEP is the absolute value of the step.  */

static bool
assert_no_overflow_lt (tree type, affine_iv *iv0, affine_iv *iv1,
		       class tree_niter_desc *niter, tree step)
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
		      class tree_niter_desc *niter, bounds *bnds)
{
  tree assumption = boolean_true_node, bound, diff;
  tree mbz, mbzl, mbzr, type1;
  bool rolls_p, no_overflow_p;
  widest_int dstep;
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
    dstep = wi::to_widest (iv0->step);
  else
    {
      dstep = wi::sext (wi::to_widest (iv1->step), TYPE_PRECISION (type));
      dstep = -dstep;
    }

  mpz_init (mstep);
  wi::to_mpz (dstep, mstep, UNSIGNED);
  mpz_neg (mstep, mstep);
  mpz_add_ui (mstep, mstep, 1);

  rolls_p = mpz_cmp (mstep, bnds->below) <= 0;

  mpz_init (max);
  wi::to_mpz (wi::minus_one (TYPE_PRECISION (type)), max, UNSIGNED);
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
   is IV0 < IV1 which likes:  {base, -C} < n,  or n < {base, C}.
   The number of iterations is stored to NITER.  */

static bool
number_of_iterations_until_wrap (class loop *loop, tree type, affine_iv *iv0,
				 affine_iv *iv1, class tree_niter_desc *niter)
{
  tree niter_type = unsigned_type_for (type);
  tree step, num, assumptions, may_be_zero, span;
  wide_int high, low, max, min;

  may_be_zero = fold_build2 (LE_EXPR, boolean_type_node, iv1->base, iv0->base);
  if (integer_onep (may_be_zero))
    return false;

  int prec = TYPE_PRECISION (type);
  signop sgn = TYPE_SIGN (type);
  min = wi::min_value (prec, sgn);
  max = wi::max_value (prec, sgn);

  /* n < {base, C}. */
  if (integer_zerop (iv0->step) && !tree_int_cst_sign_bit (iv1->step))
    {
      step = iv1->step;
      /* MIN + C - 1 <= n.  */
      tree last = wide_int_to_tree (type, min + wi::to_wide (step) - 1);
      assumptions = fold_build2 (LE_EXPR, boolean_type_node, last, iv0->base);
      if (integer_zerop (assumptions))
	return false;

      num = fold_build2 (MINUS_EXPR, niter_type,
			 wide_int_to_tree (niter_type, max),
			 fold_convert (niter_type, iv1->base));

      /* When base has the form iv + 1, if we know iv >= n, then iv + 1 < n
	 only when iv + 1 overflows, i.e. when iv == TYPE_VALUE_MAX.  */
      if (sgn == UNSIGNED
	  && integer_onep (step)
	  && TREE_CODE (iv1->base) == PLUS_EXPR
	  && integer_onep (TREE_OPERAND (iv1->base, 1)))
	{
	  tree cond = fold_build2 (GE_EXPR, boolean_type_node,
				   TREE_OPERAND (iv1->base, 0), iv0->base);
	  cond = simplify_using_initial_conditions (loop, cond);
	  if (integer_onep (cond))
	    may_be_zero = fold_build2 (EQ_EXPR, boolean_type_node,
				       TREE_OPERAND (iv1->base, 0),
				       TYPE_MAX_VALUE (type));
	}

      high = max;
      if (TREE_CODE (iv1->base) == INTEGER_CST)
	low = wi::to_wide (iv1->base) - 1;
      else if (TREE_CODE (iv0->base) == INTEGER_CST)
	low = wi::to_wide (iv0->base);
      else
	low = min;
    }
  /* {base, -C} < n.  */
  else if (tree_int_cst_sign_bit (iv0->step) && integer_zerop (iv1->step))
    {
      step = fold_build1 (NEGATE_EXPR, TREE_TYPE (iv0->step), iv0->step);
      /* MAX - C + 1 >= n.  */
      tree last = wide_int_to_tree (type, max - wi::to_wide (step) + 1);
      assumptions = fold_build2 (GE_EXPR, boolean_type_node, last, iv1->base);
      if (integer_zerop (assumptions))
	return false;

      num = fold_build2 (MINUS_EXPR, niter_type,
			 fold_convert (niter_type, iv0->base),
			 wide_int_to_tree (niter_type, min));
      low = min;
      if (TREE_CODE (iv0->base) == INTEGER_CST)
	high = wi::to_wide (iv0->base) + 1;
      else if (TREE_CODE (iv1->base) == INTEGER_CST)
	high = wi::to_wide (iv1->base);
      else
	high = max;
    }
  else
    return false;

  /* (delta + step - 1) / step */
  step = fold_convert (niter_type, step);
  num = fold_build2 (PLUS_EXPR, niter_type, num, step);
  niter->niter = fold_build2 (FLOOR_DIV_EXPR, niter_type, num, step);

  widest_int delta, s;
  delta = widest_int::from (high, sgn) - widest_int::from (low, sgn);
  s = wi::to_widest (step);
  delta = delta + s - 1;
  niter->max = wi::udiv_floor (delta, s);

  niter->may_be_zero = may_be_zero;

  if (!integer_nonzerop (assumptions))
    niter->assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				      niter->assumptions, assumptions);

  niter->control.no_overflow = false;

  /* Update bound and exit condition as:
     bound = niter * STEP + (IVbase - STEP).
     { IVbase - STEP, +, STEP } != bound
     Here, biasing IVbase by 1 step makes 'bound' be the value before wrap.
     */
  tree base_type = TREE_TYPE (niter->control.base);
  if (POINTER_TYPE_P (base_type))
    {
      tree utype = unsigned_type_for (base_type);
      niter->control.base
	= fold_build2 (MINUS_EXPR, utype,
		       fold_convert (utype, niter->control.base),
		       fold_convert (utype, niter->control.step));
      niter->control.base = fold_convert (base_type, niter->control.base);
    }
  else
    niter->control.base
      = fold_build2 (MINUS_EXPR, base_type, niter->control.base,
		     niter->control.step);

  span = fold_build2 (MULT_EXPR, niter_type, niter->niter,
		      fold_convert (niter_type, niter->control.step));
  niter->bound = fold_build2 (PLUS_EXPR, niter_type, span,
			      fold_convert (niter_type, niter->control.base));
  niter->bound = fold_convert (type, niter->bound);
  niter->cmp = NE_EXPR;

  return true;
}

/* Determines number of iterations of loop whose ending condition
   is IV0 < IV1.  TYPE is the type of the iv.  The number of
   iterations is stored to NITER.  BNDS bounds the difference
   IV1->base - IV0->base.  EXIT_MUST_BE_TAKEN is true if we know
   that the exit must be taken eventually.  */

static bool
number_of_iterations_lt (class loop *loop, tree type, affine_iv *iv0,
			 affine_iv *iv1, class tree_niter_desc *niter,
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

  /* {base, -C} < n,  or n < {base, C} */
  if (tree_int_cst_sign_bit (iv0->step)
      || (!integer_zerop (iv1->step) && !tree_int_cst_sign_bit (iv1->step)))
    return number_of_iterations_until_wrap (loop, type, iv0, iv1, niter);

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
      niter->max = widest_int::from (wi::from_mpz (niter_type, bnds->up, false),
				     TYPE_SIGN (niter_type));
      niter->control.no_overflow = true;
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

      return number_of_iterations_ne (loop, type, &zps,
				      delta, niter, true, bnds);
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
  wi::to_mpz (wi::to_wide (step), mstep, UNSIGNED);
  mpz_add (tmp, bnds->up, mstep);
  mpz_sub_ui (tmp, tmp, 1);
  mpz_fdiv_q (tmp, tmp, mstep);
  niter->max = widest_int::from (wi::from_mpz (niter_type, tmp, false),
				 TYPE_SIGN (niter_type));
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
number_of_iterations_le (class loop *loop, tree type, affine_iv *iv0,
			 affine_iv *iv1, class tree_niter_desc *niter,
			 bool exit_must_be_taken, bounds *bnds)
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

  bounds_add (bnds, 1, type1);

  return number_of_iterations_lt (loop, type, iv0, iv1, niter, exit_must_be_taken,
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
   comments at class tree_niter_desc in tree-ssa-loop.h) are stored to NITER.
   Returns false if it fails to determine number of iterations, true if it
   was determined (possibly with some assumptions).  */

static bool
number_of_iterations_cond (class loop *loop,
			   tree type, affine_iv *iv0, enum tree_code code,
			   affine_iv *iv1, class tree_niter_desc *niter,
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
  niter->max = 0;
  niter->bound = NULL_TREE;
  niter->cmp = ERROR_MARK;

  /* Make < comparison from > ones, and for NE_EXPR comparisons, ensure that
     the control variable is on lhs.  */
  if (code == GE_EXPR || code == GT_EXPR
      || (code == NE_EXPR && integer_zerop (iv0->step)))
    {
      std::swap (iv0, iv1);
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

  /* We can handle cases which neither of the sides of the comparison is
     invariant:

       {iv0.base, iv0.step} cmp_code {iv1.base, iv1.step}
     as if:
       {iv0.base, iv0.step - iv1.step} cmp_code {iv1.base, 0}

     provided that either below condition is satisfied:

       a) the test is NE_EXPR;
       b) iv0 and iv1 do not overflow and iv0.step - iv1.step is of
	  the same sign and of less or equal magnitude than iv0.step

     This rarely occurs in practice, but it is simple enough to manage.  */
  if (!integer_zerop (iv0->step) && !integer_zerop (iv1->step))
    {
      tree step_type = POINTER_TYPE_P (type) ? sizetype : type;
      tree step = fold_binary_to_constant (MINUS_EXPR, step_type,
					   iv0->step, iv1->step);

      /* For code other than NE_EXPR we have to ensure moving the evolution
	 of IV1 to that of IV0 does not introduce overflow.  */
      if (TREE_CODE (step) != INTEGER_CST
	  || !iv0->no_overflow || !iv1->no_overflow)
	{
	  if (code != NE_EXPR)
	    return false;
	  iv0->no_overflow = false;
	}
      /* If the new step of IV0 has changed sign or is of greater
	 magnitude then we do not know whether IV0 does overflow
	 and thus the transform is not valid for code other than NE_EXPR.  */
      else if (tree_int_cst_sign_bit (step) != tree_int_cst_sign_bit (iv0->step)
	       || wi::gtu_p (wi::abs (wi::to_widest (step)),
			     wi::abs (wi::to_widest (iv0->step))))
	{
	  if (POINTER_TYPE_P (type) && code != NE_EXPR)
	    /* For relational pointer compares we have further guarantees
	       that the pointers always point to the same object (or one
	       after it) and that objects do not cross the zero page.  So
	       not only is the transform always valid for relational
	       pointer compares, we also know the resulting IV does not
	       overflow.  */
	    ;
	  else if (code != NE_EXPR)
	    return false;
	  else
	    iv0->no_overflow = false;
	}

      iv0->step = step;
      iv1->step = build_int_cst (step_type, 0);
      iv1->no_overflow = true;
    }

  /* If the result of the comparison is a constant,  the loop is weird.  More
     precise handling would be possible, but the situation is not common enough
     to waste time on it.  */
  if (integer_zerop (iv0->step) && integer_zerop (iv1->step))
    return false;

  /* If the loop exits immediately, there is nothing to do.  */
  tree tem = fold_binary (code, boolean_type_node, iv0->base, iv1->base);
  if (tem && integer_zerop (tem))
    {
      if (!every_iteration)
	return false;
      niter->niter = build_int_cst (unsigned_type_for (type), 0);
      niter->max = 0;
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
      ret = number_of_iterations_ne (loop, type, iv0, iv1->base, niter,
				     exit_must_be_taken, &bnds);
      break;

    case LT_EXPR:
      ret = number_of_iterations_lt (loop, type, iv0, iv1, niter,
				     exit_must_be_taken, &bnds);
      break;

    case LE_EXPR:
      ret = number_of_iterations_le (loop, type, iv0, iv1, niter,
				     exit_must_be_taken, &bnds);
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
	  print_decu (niter->max, dump_file);
	  fprintf (dump_file, "\n");
	}
      else
	fprintf (dump_file, "  failed\n\n");
    }
  return ret;
}

/* Return an expression that computes the popcount of src.  */

static tree
build_popcount_expr (tree src)
{
  tree fn;
  bool use_ifn = false;
  int prec = TYPE_PRECISION (TREE_TYPE (src));
  int i_prec = TYPE_PRECISION (integer_type_node);
  int li_prec = TYPE_PRECISION (long_integer_type_node);
  int lli_prec = TYPE_PRECISION (long_long_integer_type_node);

  tree utype = unsigned_type_for (TREE_TYPE (src));
  src = fold_convert (utype, src);

  if (direct_internal_fn_supported_p (IFN_POPCOUNT, utype, OPTIMIZE_FOR_BOTH))
    use_ifn = true;
  else if (prec <= i_prec)
    fn = builtin_decl_implicit (BUILT_IN_POPCOUNT);
  else if (prec == li_prec)
    fn = builtin_decl_implicit (BUILT_IN_POPCOUNTL);
  else if (prec == lli_prec || prec == 2 * lli_prec)
    fn = builtin_decl_implicit (BUILT_IN_POPCOUNTLL);
  else
    return NULL_TREE;

  tree call;
  if (use_ifn)
      call = build_call_expr_internal_loc (UNKNOWN_LOCATION, IFN_POPCOUNT,
					   integer_type_node, 1, src);
  else if (prec == 2 * lli_prec)
    {
      tree src1 = fold_convert (long_long_unsigned_type_node,
				fold_build2 (RSHIFT_EXPR, TREE_TYPE (src),
					     unshare_expr (src),
					     build_int_cst (integer_type_node,
							    lli_prec)));
      tree src2 = fold_convert (long_long_unsigned_type_node, src);
      tree call1 = build_call_expr (fn, 1, src1);
      tree call2 = build_call_expr (fn, 1, src2);
      call = fold_build2 (PLUS_EXPR, integer_type_node, call1, call2);
    }
  else
    {
      if (prec < i_prec)
	src = fold_convert (unsigned_type_node, src);

      call = build_call_expr (fn, 1, src);
    }

  return call;
}

/* Utility function to check if OP is defined by a stmt
   that is a val - 1.  */

static bool
ssa_defined_by_minus_one_stmt_p (tree op, tree val)
{
  gimple *stmt;
  return (TREE_CODE (op) == SSA_NAME
	  && (stmt = SSA_NAME_DEF_STMT (op))
	  && is_gimple_assign (stmt)
	  && (gimple_assign_rhs_code (stmt) == PLUS_EXPR)
	  && val == gimple_assign_rhs1 (stmt)
	  && integer_minus_onep (gimple_assign_rhs2 (stmt)));
}

/* See comment below for number_of_iterations_bitcount.
   For popcount, we have:

   modify:
   _1 = iv_1 + -1
   iv_2 = iv_1 & _1

   test:
   if (iv != 0)

   modification count:
   popcount (src)

 */

static bool
number_of_iterations_popcount (loop_p loop, edge exit,
			       enum tree_code code,
			       class tree_niter_desc *niter)
{
  bool modify_before_test = true;
  HOST_WIDE_INT max;

  /* Check that condition for staying inside the loop is like
     if (iv != 0).  */
  gcond *cond_stmt = safe_dyn_cast <gcond *> (*gsi_last_bb (exit->src));
  if (!cond_stmt
      || code != NE_EXPR
      || !integer_zerop (gimple_cond_rhs (cond_stmt))
      || TREE_CODE (gimple_cond_lhs (cond_stmt)) != SSA_NAME)
    return false;

  tree iv_2 = gimple_cond_lhs (cond_stmt);
  gimple *iv_2_stmt = SSA_NAME_DEF_STMT (iv_2);

  /* If the test comes before the iv modification, then these will actually be
     iv_1 and a phi node.  */
  if (gimple_code (iv_2_stmt) == GIMPLE_PHI
      && gimple_bb (iv_2_stmt) == loop->header
      && gimple_phi_num_args (iv_2_stmt) == 2
      && (TREE_CODE (gimple_phi_arg_def (iv_2_stmt,
					 loop_latch_edge (loop)->dest_idx))
	  == SSA_NAME))
    {
      /* iv_2 is actually one of the inputs to the phi.  */
      iv_2 = gimple_phi_arg_def (iv_2_stmt, loop_latch_edge (loop)->dest_idx);
      iv_2_stmt = SSA_NAME_DEF_STMT (iv_2);
      modify_before_test = false;
    }

  /* Make sure iv_2_stmt is an and stmt (iv_2 = _1 & iv_1).  */
  if (!is_gimple_assign (iv_2_stmt)
      || gimple_assign_rhs_code (iv_2_stmt) != BIT_AND_EXPR)
    return false;

  tree iv_1 = gimple_assign_rhs1 (iv_2_stmt);
  tree _1 = gimple_assign_rhs2 (iv_2_stmt);

  /* Check that _1 is defined by (_1 = iv_1 + -1).
     Also make sure that _1 is the same in and_stmt and _1 defining stmt.
     Also canonicalize if _1 and _b11 are revrsed.  */
  if (ssa_defined_by_minus_one_stmt_p (iv_1, _1))
    std::swap (iv_1, _1);
  else if (ssa_defined_by_minus_one_stmt_p (_1, iv_1))
    ;
  else
    return false;

  /* Check the recurrence.  */
  gimple *phi = SSA_NAME_DEF_STMT (iv_1);
  if (gimple_code (phi) != GIMPLE_PHI
      || (gimple_bb (phi) != loop_latch_edge (loop)->dest)
      || (iv_2 != gimple_phi_arg_def (phi, loop_latch_edge (loop)->dest_idx)))
    return false;

  /* We found a match.  */
  tree src = gimple_phi_arg_def (phi, loop_preheader_edge (loop)->dest_idx);
  int src_precision = TYPE_PRECISION (TREE_TYPE (src));

  /* Get the corresponding popcount builtin.  */
  tree expr = build_popcount_expr (src);

  if (!expr)
    return false;

  max = src_precision;

  tree may_be_zero = boolean_false_node;

  if (modify_before_test)
    {
      expr = fold_build2 (MINUS_EXPR, integer_type_node, expr,
			  integer_one_node);
      max = max - 1;
      may_be_zero = fold_build2 (EQ_EXPR, boolean_type_node, src,
				      build_zero_cst (TREE_TYPE (src)));
    }

  expr = fold_convert (unsigned_type_node, expr);

  niter->assumptions = boolean_true_node;
  niter->may_be_zero = simplify_using_initial_conditions (loop, may_be_zero);
  niter->niter = simplify_using_initial_conditions(loop, expr);

  if (TREE_CODE (niter->niter) == INTEGER_CST)
    niter->max = tree_to_uhwi (niter->niter);
  else
    niter->max = max;

  niter->bound = NULL_TREE;
  niter->cmp = ERROR_MARK;
  return true;
}

/* Return an expression that counts the leading/trailing zeroes of src.

   If define_at_zero is true, then the built expression will be defined to
   return the precision of src when src == 0 (using either a conditional
   expression or a suitable internal function).
   Otherwise, we can elide the conditional expression and let src = 0 invoke
   undefined behaviour.  */

static tree
build_cltz_expr (tree src, bool leading, bool define_at_zero)
{
  tree fn;
  internal_fn ifn = leading ? IFN_CLZ : IFN_CTZ;
  bool use_ifn = false;
  int prec = TYPE_PRECISION (TREE_TYPE (src));
  int i_prec = TYPE_PRECISION (integer_type_node);
  int li_prec = TYPE_PRECISION (long_integer_type_node);
  int lli_prec = TYPE_PRECISION (long_long_integer_type_node);

  tree utype = unsigned_type_for (TREE_TYPE (src));
  src = fold_convert (utype, src);

  if (direct_internal_fn_supported_p (ifn, utype, OPTIMIZE_FOR_BOTH))
    use_ifn = true;
  else if (prec <= i_prec)
    fn = leading ? builtin_decl_implicit (BUILT_IN_CLZ)
		 : builtin_decl_implicit (BUILT_IN_CTZ);
  else if (prec == li_prec)
    fn = leading ? builtin_decl_implicit (BUILT_IN_CLZL)
		 : builtin_decl_implicit (BUILT_IN_CTZL);
  else if (prec == lli_prec || prec == 2 * lli_prec)
    fn = leading ? builtin_decl_implicit (BUILT_IN_CLZLL)
		 : builtin_decl_implicit (BUILT_IN_CTZLL);
  else
    return NULL_TREE;

  tree call;
  if (use_ifn)
    {
      int val;
      int optab_defined_at_zero
	= (leading
	   ? CLZ_DEFINED_VALUE_AT_ZERO (SCALAR_INT_TYPE_MODE (utype), val)
	   : CTZ_DEFINED_VALUE_AT_ZERO (SCALAR_INT_TYPE_MODE (utype), val));
      tree arg2 = NULL_TREE;
      if (define_at_zero && optab_defined_at_zero == 2 && val == prec)
	arg2 = build_int_cst (integer_type_node, val);
      call = build_call_expr_internal_loc (UNKNOWN_LOCATION, ifn,
					   integer_type_node, arg2 ? 2 : 1,
					   src, arg2);
      if (define_at_zero && arg2 == NULL_TREE)
	{
	  tree is_zero = fold_build2 (NE_EXPR, boolean_type_node, src,
				      build_zero_cst (TREE_TYPE (src)));
	  call = fold_build3 (COND_EXPR, integer_type_node, is_zero, call,
			      build_int_cst (integer_type_node, prec));
	}
    }
  else if (prec == 2 * lli_prec)
    {
      tree src1 = fold_convert (long_long_unsigned_type_node,
				fold_build2 (RSHIFT_EXPR, TREE_TYPE (src),
					     unshare_expr (src),
					     build_int_cst (integer_type_node,
							    lli_prec)));
      tree src2 = fold_convert (long_long_unsigned_type_node, src);
      /* We count the zeroes in src1, and add the number in src2 when src1
	 is 0.  */
      if (!leading)
	std::swap (src1, src2);
      tree call1 = build_call_expr (fn, 1, src1);
      tree call2 = build_call_expr (fn, 1, src2);
      if (define_at_zero)
	{
	  tree is_zero2 = fold_build2 (NE_EXPR, boolean_type_node, src2,
				       build_zero_cst (TREE_TYPE (src2)));
	  call2 = fold_build3 (COND_EXPR, integer_type_node, is_zero2, call2,
			       build_int_cst (integer_type_node, lli_prec));
	}
      tree is_zero1 = fold_build2 (NE_EXPR, boolean_type_node, src1,
				   build_zero_cst (TREE_TYPE (src1)));
      call = fold_build3 (COND_EXPR, integer_type_node, is_zero1, call1,
			  fold_build2 (PLUS_EXPR, integer_type_node, call2,
				       build_int_cst (integer_type_node,
						      lli_prec)));
    }
  else
    {
      if (prec < i_prec)
	src = fold_convert (unsigned_type_node, src);

      call = build_call_expr (fn, 1, src);
      if (leading && prec < i_prec)
	call = fold_build2 (MINUS_EXPR, integer_type_node, call,
			    build_int_cst (integer_type_node, i_prec - prec));
      if (define_at_zero)
	{
	  tree is_zero = fold_build2 (NE_EXPR, boolean_type_node, src,
				      build_zero_cst (TREE_TYPE (src)));
	  call = fold_build3 (COND_EXPR, integer_type_node, is_zero, call,
			      build_int_cst (integer_type_node, prec));
	}
    }

  return call;
}

/* See comment below for number_of_iterations_bitcount.
   For c[lt]z, we have:

   modify:
   iv_2 = iv_1 << 1 OR iv_1 >> 1

   test:
   if (iv & 1 << (prec-1)) OR (iv & 1)

   modification count:
   src precision - c[lt]z (src)

 */

static bool
number_of_iterations_cltz (loop_p loop, edge exit,
			       enum tree_code code,
			       class tree_niter_desc *niter)
{
  bool modify_before_test = true;
  HOST_WIDE_INT max;
  int checked_bit;
  tree iv_2;

  /* Check that condition for staying inside the loop is like
     if (iv == 0).  */
  gcond *cond_stmt = safe_dyn_cast <gcond *> (*gsi_last_bb (exit->src));
  if (!cond_stmt
      || (code != EQ_EXPR && code != GE_EXPR)
      || !integer_zerop (gimple_cond_rhs (cond_stmt))
      || TREE_CODE (gimple_cond_lhs (cond_stmt)) != SSA_NAME)
    return false;

  if (code == EQ_EXPR)
    {
      /* Make sure we check a bitwise and with a suitable constant */
      gimple *and_stmt = SSA_NAME_DEF_STMT (gimple_cond_lhs (cond_stmt));
      if (!is_gimple_assign (and_stmt)
	  || gimple_assign_rhs_code (and_stmt) != BIT_AND_EXPR
	  || !integer_pow2p (gimple_assign_rhs2 (and_stmt))
	  || TREE_CODE (gimple_assign_rhs1 (and_stmt)) != SSA_NAME)
	return false;

      checked_bit = tree_log2 (gimple_assign_rhs2 (and_stmt));

      iv_2 = gimple_assign_rhs1 (and_stmt);
    }
  else
    {
      /* We have a GE_EXPR - a signed comparison with zero is equivalent to
	 testing the leading bit, so check for this pattern too.  */

      iv_2 = gimple_cond_lhs (cond_stmt);
      tree test_value_type = TREE_TYPE (iv_2);

      if (TYPE_UNSIGNED (test_value_type))
	return false;

      gimple *test_value_stmt = SSA_NAME_DEF_STMT (iv_2);

      if (is_gimple_assign (test_value_stmt)
	  && gimple_assign_rhs_code (test_value_stmt) == NOP_EXPR)
	{
	  /* If the test value comes from a NOP_EXPR, then we need to unwrap
	     this.  We conservatively require that both types have the same
	     precision.  */
	  iv_2 = gimple_assign_rhs1 (test_value_stmt);
	  tree rhs_type = TREE_TYPE (iv_2);
	  if (TREE_CODE (iv_2) != SSA_NAME
	      || TREE_CODE (rhs_type) != INTEGER_TYPE
	      || (TYPE_PRECISION (rhs_type)
		  != TYPE_PRECISION (test_value_type)))
	    return false;
	}

      checked_bit = TYPE_PRECISION (test_value_type) - 1;
    }

  gimple *iv_2_stmt = SSA_NAME_DEF_STMT (iv_2);

  /* If the test comes before the iv modification, then these will actually be
     iv_1 and a phi node.  */
  if (gimple_code (iv_2_stmt) == GIMPLE_PHI
      && gimple_bb (iv_2_stmt) == loop->header
      && gimple_phi_num_args (iv_2_stmt) == 2
      && (TREE_CODE (gimple_phi_arg_def (iv_2_stmt,
					 loop_latch_edge (loop)->dest_idx))
	  == SSA_NAME))
    {
      /* iv_2 is actually one of the inputs to the phi.  */
      iv_2 = gimple_phi_arg_def (iv_2_stmt, loop_latch_edge (loop)->dest_idx);
      iv_2_stmt = SSA_NAME_DEF_STMT (iv_2);
      modify_before_test = false;
    }

  /* Make sure iv_2_stmt is a logical shift by one stmt:
     iv_2 = iv_1 {<<|>>} 1  */
  if (!is_gimple_assign (iv_2_stmt)
      || (gimple_assign_rhs_code (iv_2_stmt) != LSHIFT_EXPR
	  && (gimple_assign_rhs_code (iv_2_stmt) != RSHIFT_EXPR
	      || !TYPE_UNSIGNED (TREE_TYPE (gimple_assign_lhs (iv_2_stmt)))))
      || !integer_onep (gimple_assign_rhs2 (iv_2_stmt)))
    return false;

  bool left_shift = (gimple_assign_rhs_code (iv_2_stmt) == LSHIFT_EXPR);

  tree iv_1 = gimple_assign_rhs1 (iv_2_stmt);

  /* Check the recurrence.  */
  gimple *phi = SSA_NAME_DEF_STMT (iv_1);
  if (gimple_code (phi) != GIMPLE_PHI
      || (gimple_bb (phi) != loop_latch_edge (loop)->dest)
      || (iv_2 != gimple_phi_arg_def (phi, loop_latch_edge (loop)->dest_idx)))
    return false;

  /* We found a match.  */
  tree src = gimple_phi_arg_def (phi, loop_preheader_edge (loop)->dest_idx);
  int src_precision = TYPE_PRECISION (TREE_TYPE (src));

  /* Apply any needed preprocessing to src.  */
  int num_ignored_bits;
  if (left_shift)
    num_ignored_bits = src_precision - checked_bit - 1;
  else
    num_ignored_bits = checked_bit;

  if (modify_before_test)
    num_ignored_bits++;

  if (num_ignored_bits != 0)
    src = fold_build2 (left_shift ? LSHIFT_EXPR : RSHIFT_EXPR,
		       TREE_TYPE (src), src,
		       build_int_cst (integer_type_node, num_ignored_bits));

  /* Get the corresponding c[lt]z builtin.  */
  tree expr = build_cltz_expr (src, left_shift, false);

  if (!expr)
    return false;

  max = src_precision - num_ignored_bits - 1;

  expr = fold_convert (unsigned_type_node, expr);

  tree assumptions = fold_build2 (NE_EXPR, boolean_type_node, src,
				  build_zero_cst (TREE_TYPE (src)));

  niter->assumptions = simplify_using_initial_conditions (loop, assumptions);
  niter->may_be_zero = boolean_false_node;
  niter->niter = simplify_using_initial_conditions (loop, expr);

  if (TREE_CODE (niter->niter) == INTEGER_CST)
    niter->max = tree_to_uhwi (niter->niter);
  else
    niter->max = max;

  niter->bound = NULL_TREE;
  niter->cmp = ERROR_MARK;

  return true;
}

/* See comment below for number_of_iterations_bitcount.
   For c[lt]z complement, we have:

   modify:
   iv_2 = iv_1 >> 1 OR iv_1 << 1

   test:
   if (iv != 0)

   modification count:
   src precision - c[lt]z (src)

 */

static bool
number_of_iterations_cltz_complement (loop_p loop, edge exit,
			       enum tree_code code,
			       class tree_niter_desc *niter)
{
  bool modify_before_test = true;
  HOST_WIDE_INT max;

  /* Check that condition for staying inside the loop is like
     if (iv != 0).  */
  gcond *cond_stmt = safe_dyn_cast <gcond *> (*gsi_last_bb (exit->src));
  if (!cond_stmt
      || code != NE_EXPR
      || !integer_zerop (gimple_cond_rhs (cond_stmt))
      || TREE_CODE (gimple_cond_lhs (cond_stmt)) != SSA_NAME)
    return false;

  tree iv_2 = gimple_cond_lhs (cond_stmt);
  gimple *iv_2_stmt = SSA_NAME_DEF_STMT (iv_2);

  /* If the test comes before the iv modification, then these will actually be
     iv_1 and a phi node.  */
  if (gimple_code (iv_2_stmt) == GIMPLE_PHI
      && gimple_bb (iv_2_stmt) == loop->header
      && gimple_phi_num_args (iv_2_stmt) == 2
      && (TREE_CODE (gimple_phi_arg_def (iv_2_stmt,
					 loop_latch_edge (loop)->dest_idx))
	  == SSA_NAME))
    {
      /* iv_2 is actually one of the inputs to the phi.  */
      iv_2 = gimple_phi_arg_def (iv_2_stmt, loop_latch_edge (loop)->dest_idx);
      iv_2_stmt = SSA_NAME_DEF_STMT (iv_2);
      modify_before_test = false;
    }

  /* Make sure iv_2_stmt is a logical shift by one stmt:
     iv_2 = iv_1 {>>|<<} 1  */
  if (!is_gimple_assign (iv_2_stmt)
      || (gimple_assign_rhs_code (iv_2_stmt) != LSHIFT_EXPR
	  && (gimple_assign_rhs_code (iv_2_stmt) != RSHIFT_EXPR
	      || !TYPE_UNSIGNED (TREE_TYPE (gimple_assign_lhs (iv_2_stmt)))))
      || !integer_onep (gimple_assign_rhs2 (iv_2_stmt)))
    return false;

  bool left_shift = (gimple_assign_rhs_code (iv_2_stmt) == LSHIFT_EXPR);

  tree iv_1 = gimple_assign_rhs1 (iv_2_stmt);

  /* Check the recurrence.  */
  gimple *phi = SSA_NAME_DEF_STMT (iv_1);
  if (gimple_code (phi) != GIMPLE_PHI
      || (gimple_bb (phi) != loop_latch_edge (loop)->dest)
      || (iv_2 != gimple_phi_arg_def (phi, loop_latch_edge (loop)->dest_idx)))
    return false;

  /* We found a match.  */
  tree src = gimple_phi_arg_def (phi, loop_preheader_edge (loop)->dest_idx);
  int src_precision = TYPE_PRECISION (TREE_TYPE (src));

  /* Get the corresponding c[lt]z builtin.  */
  tree expr = build_cltz_expr (src, !left_shift, true);

  if (!expr)
    return false;

  expr = fold_build2 (MINUS_EXPR, integer_type_node,
		      build_int_cst (integer_type_node, src_precision),
		      expr);

  max = src_precision;

  tree may_be_zero = boolean_false_node;

  if (modify_before_test)
    {
      expr = fold_build2 (MINUS_EXPR, integer_type_node, expr,
			  integer_one_node);
      max = max - 1;
      may_be_zero = fold_build2 (EQ_EXPR, boolean_type_node, src,
				      build_zero_cst (TREE_TYPE (src)));
    }

  expr = fold_convert (unsigned_type_node, expr);

  niter->assumptions = boolean_true_node;
  niter->may_be_zero = simplify_using_initial_conditions (loop, may_be_zero);
  niter->niter = simplify_using_initial_conditions (loop, expr);

  if (TREE_CODE (niter->niter) == INTEGER_CST)
    niter->max = tree_to_uhwi (niter->niter);
  else
    niter->max = max;

  niter->bound = NULL_TREE;
  niter->cmp = ERROR_MARK;
  return true;
}

/* See if LOOP contains a bit counting idiom. The idiom consists of two parts:
   1. A modification to the induction variabler;.
   2. A test to determine whether or not to exit the loop.

   These can come in either order - i.e.:

   <bb 3>
   iv_1 = PHI <src(2), iv_2(4)>
   if (test (iv_1))
     goto <bb 4>
   else
     goto <bb 5>

   <bb 4>
   iv_2 = modify (iv_1)
   goto <bb 3>

   OR

   <bb 3>
   iv_1 = PHI <src(2), iv_2(4)>
   iv_2 = modify (iv_1)

   <bb 4>
   if (test (iv_2))
     goto <bb 3>
   else
     goto <bb 5>

   The second form can be generated by copying the loop header out of the loop.

   In the first case, the number of latch executions will be equal to the
   number of induction variable modifications required before the test fails.

   In the second case (modify_before_test), if we assume that the number of
   modifications required before the test fails is nonzero, then the number of
   latch executions will be one less than this number.

   If we recognise the pattern, then we update niter accordingly, and return
   true.  */

static bool
number_of_iterations_bitcount (loop_p loop, edge exit,
			       enum tree_code code,
			       class tree_niter_desc *niter)
{
  return (number_of_iterations_popcount (loop, exit, code, niter)
	  || number_of_iterations_cltz (loop, exit, code, niter)
	  || number_of_iterations_cltz_complement (loop, exit, code, niter));
}

/* Substitute NEW_TREE for OLD in EXPR and fold the result.
   If VALUEIZE is non-NULL then OLD and NEW_TREE are ignored and instead
   all SSA names are replaced with the result of calling the VALUEIZE
   function with the SSA name as argument.  */

tree
simplify_replace_tree (tree expr, tree old, tree new_tree,
		       tree (*valueize) (tree, void*), void *context,
		       bool do_fold)
{
  unsigned i, n;
  tree ret = NULL_TREE, e, se;

  if (!expr)
    return NULL_TREE;

  /* Do not bother to replace constants.  */
  if (CONSTANT_CLASS_P (expr))
    return expr;

  if (valueize)
    {
      if (TREE_CODE (expr) == SSA_NAME)
	{
	  new_tree = valueize (expr, context);
	  if (new_tree != expr)
	    return new_tree;
	}
    }
  else if (expr == old
	   || operand_equal_p (expr, old, 0))
    return unshare_expr (new_tree);

  if (!EXPR_P (expr))
    return expr;

  n = TREE_OPERAND_LENGTH (expr);
  for (i = 0; i < n; i++)
    {
      e = TREE_OPERAND (expr, i);
      se = simplify_replace_tree (e, old, new_tree, valueize, context, do_fold);
      if (e == se)
	continue;

      if (!ret)
	ret = copy_node (expr);

      TREE_OPERAND (ret, i) = se;
    }

  return (ret ? (do_fold ? fold (ret) : ret) : expr);
}

/* Expand definitions of ssa names in EXPR as long as they are simple
   enough, and return the new expression.  If STOP is specified, stop
   expanding if EXPR equals to it.  */

static tree
expand_simple_operations (tree expr, tree stop, hash_map<tree, tree> &cache)
{
  unsigned i, n;
  tree ret = NULL_TREE, e, ee, e1;
  enum tree_code code;
  gimple *stmt;

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
	  if (!e)
	    continue;
	  /* SCEV analysis feeds us with a proper expression
	     graph matching the SSA graph.  Avoid turning it
	     into a tree here, thus handle tree sharing
	     properly.
	     ???  The SSA walk below still turns the SSA graph
	     into a tree but until we find a testcase do not
	     introduce additional tree sharing here.  */
	  bool existed_p;
	  tree &cee = cache.get_or_insert (e, &existed_p);
	  if (existed_p)
	    ee = cee;
	  else
	    {
	      cee = e;
	      ee = expand_simple_operations (e, stop, cache);
	      if (ee != e)
		*cache.get (e) = ee;
	    }
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

  /* Stop if it's not ssa name or the one we don't want to expand.  */
  if (TREE_CODE (expr) != SSA_NAME || expr == stop)
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

      return expand_simple_operations (e, stop, cache);
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
	return expand_simple_operations (e, stop, cache);
      else if (code == ADDR_EXPR)
	{
	  poly_int64 offset;
	  tree base = get_addr_base_and_unit_offset (TREE_OPERAND (e, 0),
						     &offset);
	  if (base
	      && TREE_CODE (base) == MEM_REF)
	    {
	      ee = expand_simple_operations (TREE_OPERAND (base, 0), stop,
					     cache);
	      return fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (expr), ee,
				  wide_int_to_tree (sizetype,
						    mem_ref_offset (base)
						    + offset));
	    }
	}

      return expr;
    }

  switch (code)
    {
    CASE_CONVERT:
      /* Casts are simple.  */
      ee = expand_simple_operations (e, stop, cache);
      return fold_build1 (code, TREE_TYPE (expr), ee);

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
      if (ANY_INTEGRAL_TYPE_P (TREE_TYPE (expr))
	  && TYPE_OVERFLOW_TRAPS (TREE_TYPE (expr)))
	return expr;
      /* Fallthru.  */
    case POINTER_PLUS_EXPR:
      /* And increments and decrements by a constant are simple.  */
      e1 = gimple_assign_rhs2 (stmt);
      if (!is_gimple_min_invariant (e1))
	return expr;

      ee = expand_simple_operations (e, stop, cache);
      return fold_build2 (code, TREE_TYPE (expr), ee, e1);

    default:
      return expr;
    }
}

tree
expand_simple_operations (tree expr, tree stop)
{
  hash_map<tree, tree> cache;
  return expand_simple_operations (expr, stop, cache);
}

/* Tries to simplify EXPR using the condition COND.  Returns the simplified
   expression (or EXPR unchanged, if no simplification was possible).  */

static tree
tree_simplify_using_condition_1 (tree cond, tree expr)
{
  bool changed;
  tree e, e0, e1, e2, notcond;
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

  /* Check whether COND ==> EXPR.  */
  notcond = invert_truthvalue (cond);
  e = fold_binary (TRUTH_OR_EXPR, boolean_type_node, notcond, expr);
  if (e && integer_nonzerop (e))
    return e;

  /* Check whether COND ==> not EXPR.  */
  e = fold_binary (TRUTH_AND_EXPR, boolean_type_node, cond, expr);
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
   simplification was possible).  */

tree
simplify_using_initial_conditions (class loop *loop, tree expr)
{
  edge e;
  basic_block bb;
  tree cond, expanded, backup;
  int cnt = 0;

  if (TREE_CODE (expr) == INTEGER_CST)
    return expr;

  backup = expanded = expand_simple_operations (expr);

  /* Limit walking the dominators to avoid quadraticness in
     the number of BBs times the number of loops in degenerate
     cases.  */
  for (bb = loop->header;
       bb != ENTRY_BLOCK_PTR_FOR_FN (cfun) && cnt < MAX_DOMINATORS_TO_WALK;
       bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      if (!single_pred_p (bb))
	continue;
      e = single_pred_edge (bb);

      if (!(e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
	continue;

      gcond *stmt = as_a <gcond *> (*gsi_last_bb (e->src));
      cond = fold_build2 (gimple_cond_code (stmt),
			  boolean_type_node,
			  gimple_cond_lhs (stmt),
			  gimple_cond_rhs (stmt));
      if (e->flags & EDGE_FALSE_VALUE)
	cond = invert_truthvalue (cond);
      expanded = tree_simplify_using_condition (cond, expanded);
      /* Break if EXPR is simplified to const values.  */
      if (expanded
	  && (integer_zerop (expanded) || integer_nonzerop (expanded)))
	return expanded;

      ++cnt;
    }

  /* Return the original expression if no simplification is done.  */
  return operand_equal_p (backup, expanded, 0) ? expr : expanded;
}

/* Tries to simplify EXPR using the evolutions of the loop invariants
   in the superloops of LOOP.  Returns the simplified expression
   (or EXPR unchanged, if no simplification was possible).  */

static tree
simplify_using_outer_evolutions (class loop *loop, tree expr)
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
loop_only_exit_p (const class loop *loop, basic_block *body, const_edge exit)
{
  gimple_stmt_iterator bsi;
  unsigned i;

  if (exit != single_exit (loop))
    return false;

  for (i = 0; i < loop->num_nodes; i++)
    for (bsi = gsi_start_bb (body[i]); !gsi_end_p (bsi); gsi_next (&bsi))
      if (stmt_can_terminate_bb_p (gsi_stmt (bsi)))
	return false;

  return true;
}

/* Stores description of number of iterations of LOOP derived from
   EXIT (an exit edge of the LOOP) in NITER.  Returns true if some useful
   information could be derived (and fields of NITER have meaning described
   in comments at class tree_niter_desc declaration), false otherwise.
   When EVERY_ITERATION is true, only tests that are known to be executed
   every iteration are considered (i.e. only test that alone bounds the loop).
   If AT_STMT is not NULL, this function stores LOOP's condition statement in
   it when returning true.  */

bool
number_of_iterations_exit_assumptions (class loop *loop, edge exit,
				       class tree_niter_desc *niter,
				       gcond **at_stmt, bool every_iteration,
				       basic_block *body)
{
  tree type;
  tree op0, op1;
  enum tree_code code;
  affine_iv iv0, iv1;
  bool safe;

  /* The condition at a fake exit (if it exists) does not control its
     execution.  */
  if (exit->flags & EDGE_FAKE)
    return false;

  /* Nothing to analyze if the loop is known to be infinite.  */
  if (loop_constraint_set_p (loop, LOOP_C_INFINITE))
    return false;

  safe = dominated_by_p (CDI_DOMINATORS, loop->latch, exit->src);

  if (every_iteration && !safe)
    return false;

  niter->assumptions = boolean_false_node;
  niter->control.base = NULL_TREE;
  niter->control.step = NULL_TREE;
  niter->control.no_overflow = false;
  gcond *stmt = safe_dyn_cast <gcond *> (*gsi_last_bb (exit->src));
  if (!stmt)
    return false;

  if (at_stmt)
    *at_stmt = stmt;

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

    case EQ_EXPR:
      return number_of_iterations_cltz (loop, exit, code, niter);

    default:
      return false;
    }

  op0 = gimple_cond_lhs (stmt);
  op1 = gimple_cond_rhs (stmt);
  type = TREE_TYPE (op0);

  if (TREE_CODE (type) != INTEGER_TYPE
      && !POINTER_TYPE_P (type))
    return false;

  tree iv0_niters = NULL_TREE;
  if (!simple_iv_with_niters (loop, loop_containing_stmt (stmt),
			      op0, &iv0, safe ? &iv0_niters : NULL, false))
    return number_of_iterations_bitcount (loop, exit, code, niter);
  tree iv1_niters = NULL_TREE;
  if (!simple_iv_with_niters (loop, loop_containing_stmt (stmt),
			      op1, &iv1, safe ? &iv1_niters : NULL, false))
    return false;
  /* Give up on complicated case.  */
  if (iv0_niters && iv1_niters)
    return false;

  /* We don't want to see undefined signed overflow warnings while
     computing the number of iterations.  */
  fold_defer_overflow_warnings ();

  iv0.base = expand_simple_operations (iv0.base);
  iv1.base = expand_simple_operations (iv1.base);
  bool body_from_caller = true;
  if (!body)
    {
      body = get_loop_body (loop);
      body_from_caller = false;
    }
  bool only_exit_p = loop_only_exit_p (loop, body, exit);
  if (!body_from_caller)
    free (body);
  if (!number_of_iterations_cond (loop, type, &iv0, code, &iv1, niter,
				  only_exit_p, safe))
    {
      fold_undefer_and_ignore_overflow_warnings ();
      return false;
    }

  /* Incorporate additional assumption implied by control iv.  */
  tree iv_niters = iv0_niters ? iv0_niters : iv1_niters;
  if (iv_niters)
    {
      tree assumption = fold_build2 (LE_EXPR, boolean_type_node, niter->niter,
				     fold_convert (TREE_TYPE (niter->niter),
						   iv_niters));

      if (!integer_nonzerop (assumption))
	niter->assumptions = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
					  niter->assumptions, assumption);

      /* Refine upper bound if possible.  */
      if (TREE_CODE (iv_niters) == INTEGER_CST
	  && niter->max > wi::to_widest (iv_niters))
	niter->max = wi::to_widest (iv_niters);
    }

  /* There is no assumptions if the loop is known to be finite.  */
  if (!integer_zerop (niter->assumptions)
      && loop_constraint_set_p (loop, LOOP_C_FINITE))
    niter->assumptions = boolean_true_node;

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
    niter->max = wi::to_widest (niter->niter);

  return (!integer_zerop (niter->assumptions));
}

/* Like number_of_iterations_exit_assumptions, but return TRUE only if
   the niter information holds unconditionally.  */

bool
number_of_iterations_exit (class loop *loop, edge exit,
			   class tree_niter_desc *niter,
			   bool warn, bool every_iteration,
			   basic_block *body)
{
  gcond *stmt;
  if (!number_of_iterations_exit_assumptions (loop, exit, niter,
					      &stmt, every_iteration, body))
    return false;

  if (integer_nonzerop (niter->assumptions))
    return true;

  if (warn && dump_enabled_p ())
    dump_printf_loc (MSG_MISSED_OPTIMIZATION, stmt,
		     "missed loop optimization: niters analysis ends up "
		     "with assumptions.\n");

  return false;
}

/* Try to determine the number of iterations of LOOP.  If we succeed,
   expression giving number of iterations is returned and *EXIT is
   set to the edge from that the information is obtained.  Otherwise
   chrec_dont_know is returned.  */

tree
find_loop_niter (class loop *loop, edge *exit)
{
  unsigned i;
  auto_vec<edge> exits = get_loop_exit_edges (loop);
  edge ex;
  tree niter = NULL_TREE, aniter;
  class tree_niter_desc desc;

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

  return niter ? niter : chrec_dont_know;
}

/* Return true if loop is known to have bounded number of iterations.  */

bool
finite_loop_p (class loop *loop)
{
  widest_int nit;
  int flags;

  if (loop->finite_p)
    {
      unsigned i;
      auto_vec<edge> exits = get_loop_exit_edges (loop);
      edge ex;

      /* If the loop has a normal exit, we can assume it will terminate.  */
      FOR_EACH_VEC_ELT (exits, i, ex)
	if (!(ex->flags & (EDGE_EH | EDGE_ABNORMAL | EDGE_FAKE)))
	  {
	    if (dump_file)
	      fprintf (dump_file, "Assume loop %i to be finite: it has an exit "
		       "and -ffinite-loops is on or loop was "
		       "previously finite.\n",
		       loop->num);
	    return true;
	  }
    }

  flags = flags_from_decl_or_type (current_function_decl);
  if ((flags & (ECF_CONST|ECF_PURE)) && !(flags & ECF_LOOPING_CONST_OR_PURE))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Found loop %i to be finite: it is within "
		 "pure or const function.\n",
		 loop->num);
      loop->finite_p = true;
      return true;
    }

  if (loop->any_upper_bound
      /* Loop with no normal exit will not pass max_loop_iterations.  */
      || (!loop->finite_p && max_loop_iterations (loop, &nit)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Found loop %i to be finite: upper bound found.\n",
		 loop->num);
      loop->finite_p = true;
      return true;
    }

  return false;
}

/*

   Analysis of a number of iterations of a loop by a brute-force evaluation.

*/

/* Bound on the number of iterations we try to evaluate.  */

#define MAX_ITERATIONS_TO_TRACK \
  ((unsigned) param_max_iterations_to_track)

/* Returns the loop phi node of LOOP such that ssa name X is derived from its
   result by a chain of operations such that all but exactly one of their
   operands are constants.  */

static gphi *
chain_of_csts_start (class loop *loop, tree x)
{
  gimple *stmt = SSA_NAME_DEF_STMT (x);
  tree use;
  basic_block bb = gimple_bb (stmt);
  enum tree_code code;

  if (!bb
      || !flow_bb_inside_loop_p (loop, bb))
    return NULL;

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      if (bb == loop->header)
	return as_a <gphi *> (stmt);

      return NULL;
    }

  if (gimple_code (stmt) != GIMPLE_ASSIGN
      || gimple_assign_rhs_class (stmt) == GIMPLE_TERNARY_RHS)
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
     value in the current iteration by a chain of operations with constants,
     or is also a constant

   If such phi node exists, it is returned, otherwise NULL is returned.  */

static gphi *
get_base_for (class loop *loop, tree x)
{
  gphi *phi;
  tree init, next;

  if (is_gimple_min_invariant (x))
    return NULL;

  phi = chain_of_csts_start (loop, x);
  if (!phi)
    return NULL;

  init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
  next = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));

  if (!is_gimple_min_invariant (init))
    return NULL;

  if (TREE_CODE (next) == SSA_NAME
      && chain_of_csts_start (loop, next) != phi)
    return NULL;

  return phi;
}

/* Given an expression X, then

   * if X is NULL_TREE, we return the constant BASE.
   * if X is a constant, we return the constant X.
   * otherwise X is a SSA name, whose value in the considered loop is derived
     by a chain of operations with constant from a result of a phi node in
     the header of the loop.  Then we return value of X when the value of the
     result of this phi node is given by the constant BASE.  */

static tree
get_val_for (tree x, tree base)
{
  gimple *stmt;

  gcc_checking_assert (is_gimple_min_invariant (base));

  if (!x)
    return base;
  else if (is_gimple_min_invariant (x))
    return x;

  stmt = SSA_NAME_DEF_STMT (x);
  if (gimple_code (stmt) == GIMPLE_PHI)
    return base;

  gcc_checking_assert (is_gimple_assign (stmt));

  /* STMT must be either an assignment of a single SSA name or an
     expression involving an SSA name and a constant.  Try to fold that
     expression using the value for the SSA name.  */
  if (gimple_assign_ssa_name_copy_p (stmt))
    return get_val_for (gimple_assign_rhs1 (stmt), base);
  else if (gimple_assign_rhs_class (stmt) == GIMPLE_UNARY_RHS
	   && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    return fold_build1 (gimple_assign_rhs_code (stmt),
			TREE_TYPE (gimple_assign_lhs (stmt)),
			get_val_for (gimple_assign_rhs1 (stmt), base));
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
			  TREE_TYPE (gimple_assign_lhs (stmt)), rhs1, rhs2);
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
loop_niter_by_eval (class loop *loop, edge exit)
{
  tree acnd;
  tree op[2], val[2], next[2], aval[2];
  gphi *phi;
  unsigned i, j;
  enum tree_code cmp;

  gcond *cond = safe_dyn_cast <gcond *> (*gsi_last_bb (exit->src));
  if (!cond)
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
	  aval[j] = val[j];
	  val[j] = get_val_for (next[j], val[j]);
	  if (!is_gimple_min_invariant (val[j]))
	    {
	      fold_undefer_and_ignore_overflow_warnings ();
	      return chrec_dont_know;
	    }
	}

      /* If the next iteration would use the same base values
	 as the current one, there is no point looping further,
	 all following iterations will be the same as this one.  */
      if (val[0] == aval[0] && val[1] == aval[1])
	break;
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
find_loop_niter_by_eval (class loop *loop, edge *exit)
{
  unsigned i;
  auto_vec<edge> exits = get_loop_exit_edges (loop);
  edge ex;
  tree niter = NULL_TREE, aniter;

  *exit = NULL;

  /* Loops with multiple exits are expensive to handle and less important.  */
  if (!flag_expensive_optimizations
      && exits.length () > 1)
    return chrec_dont_know;

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

  return niter ? niter : chrec_dont_know;
}

/*

   Analysis of upper bounds on number of iterations of a loop.

*/

static widest_int derive_constant_upper_bound_ops (tree, tree,
						   enum tree_code, tree);

/* Returns a constant upper bound on the value of the right-hand side of
   an assignment statement STMT.  */

static widest_int
derive_constant_upper_bound_assign (gimple *stmt)
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

static widest_int
derive_constant_upper_bound (tree val)
{
  enum tree_code code;
  tree op0, op1, op2;

  extract_ops_from_tree (val, &code, &op0, &op1, &op2);
  return derive_constant_upper_bound_ops (TREE_TYPE (val), op0, code, op1);
}

/* Returns a constant upper bound on the value of expression OP0 CODE OP1,
   whose type is TYPE.  The expression is considered to be unsigned.  If
   its type is signed, its value must be nonnegative.  */

static widest_int
derive_constant_upper_bound_ops (tree type, tree op0,
				 enum tree_code code, tree op1)
{
  tree subtype, maxt;
  widest_int bnd, max, cst;
  gimple *stmt;

  if (INTEGRAL_TYPE_P (type))
    maxt = TYPE_MAX_VALUE (type);
  else
    maxt = upper_bound_in_type (type, type);

  max = wi::to_widest (maxt);

  switch (code)
    {
    case INTEGER_CST:
      return wi::to_widest (op0);

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
      if (wi::ltu_p (max, bnd))
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
      cst = wi::sext (wi::to_widest (op1), TYPE_PRECISION (type));
      if (code != MINUS_EXPR)
	cst = -cst;

      bnd = derive_constant_upper_bound (op0);

      if (wi::neg_p (cst))
	{
	  cst = -cst;
	  /* Avoid CST == 0x80000...  */
	  if (wi::neg_p (cst))
	    return max;

	  /* OP0 + CST.  We need to check that
	     BND <= MAX (type) - CST.  */

	  widest_int mmax = max - cst;
	  if (wi::leu_p (bnd, mmax))
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
	  if (wi::ltu_p (bnd, cst))
	    return max;

	  if (TYPE_UNSIGNED (type))
	    {
	      tree tem = fold_binary (GE_EXPR, boolean_type_node, op0,
				      wide_int_to_tree (type, cst));
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
      return wi::udiv_floor (bnd, wi::to_widest (op1));

    case BIT_AND_EXPR:
      if (TREE_CODE (op1) != INTEGER_CST
	  || tree_int_cst_sign_bit (op1))
	return max;
      return wi::to_widest (op1);

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

/* Emit a -Waggressive-loop-optimizations warning if needed.  */

static void
do_warn_aggressive_loop_optimizations (class loop *loop,
				       widest_int i_bound, gimple *stmt)
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
      || wi::cmpu (i_bound, wi::to_widest (loop->nb_iterations)) >= 0
      /* And undefined behavior happens unconditionally.  */
      || !dominated_by_p (CDI_DOMINATORS, loop->latch, gimple_bb (stmt)))
    return;

  edge e = single_exit (loop);
  if (e == NULL)
    return;

  gimple *estmt = last_nondebug_stmt (e->src);
  char buf[WIDE_INT_PRINT_BUFFER_SIZE], *p;
  unsigned len;
  if (print_dec_buf_size (i_bound, TYPE_SIGN (TREE_TYPE (loop->nb_iterations)),
			  &len))
    p = XALLOCAVEC (char, len);
  else
    p = buf;
  print_dec (i_bound, p, TYPE_SIGN (TREE_TYPE (loop->nb_iterations)));
  auto_diagnostic_group d;
  if (warning_at (gimple_location (stmt), OPT_Waggressive_loop_optimizations,
		  "iteration %s invokes undefined behavior", p))
    inform (gimple_location (estmt), "within this loop");
  loop->warned_aggressive_loop_optimizations = true;
}

/* Records that AT_STMT is executed at most BOUND + 1 times in LOOP.  IS_EXIT
   is true if the loop is exited immediately after STMT, and this exit
   is taken at last when the STMT is executed BOUND + 1 times.
   REALISTIC is true if BOUND is expected to be close to the real number
   of iterations.  UPPER is true if we are sure the loop iterates at most
   BOUND times.  I_BOUND is a widest_int upper estimate on BOUND.  */

static void
record_estimate (class loop *loop, tree bound, const widest_int &i_bound,
		 gimple *at_stmt, bool is_exit, bool realistic, bool upper)
{
  widest_int delta;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Statement %s", is_exit ? "(exit)" : "");
      print_gimple_stmt (dump_file, at_stmt, 0, TDF_SLIM);
      fprintf (dump_file, " is %sexecuted at most ",
	       upper ? "" : "probably ");
      print_generic_expr (dump_file, bound, TDF_SLIM);
      fprintf (dump_file, " (bounded by ");
      print_decu (i_bound, dump_file);
      fprintf (dump_file, ") + 1 times in loop %d.\n", loop->num);
    }

  /* If the I_BOUND is just an estimate of BOUND, it rarely is close to the
     real number of iterations.  */
  if (TREE_CODE (bound) != INTEGER_CST)
    realistic = false;
  else
    gcc_checking_assert (i_bound == wi::to_widest (bound));

  if (wi::min_precision (i_bound, SIGNED) > bound_wide_int ().get_precision ())
    return;

  /* If we have a guaranteed upper bound, record it in the appropriate
     list, unless this is an !is_exit bound (i.e. undefined behavior in
     at_stmt) in a loop with known constant number of iterations.  */
  if (upper
      && (is_exit
	  || loop->nb_iterations == NULL_TREE
	  || TREE_CODE (loop->nb_iterations) != INTEGER_CST))
    {
      class nb_iter_bound *elt = ggc_alloc<nb_iter_bound> ();

      elt->bound = bound_wide_int::from (i_bound, SIGNED);
      elt->stmt = at_stmt;
      elt->is_exit = is_exit;
      elt->next = loop->bounds;
      loop->bounds = elt;
    }

  /* If statement is executed on every path to the loop latch, we can directly
     infer the upper bound on the # of iterations of the loop.  */
  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, gimple_bb (at_stmt)))
    upper = false;

  /* Update the number of iteration estimates according to the bound.
     If at_stmt is an exit then the loop latch is executed at most BOUND times,
     otherwise it can be executed BOUND + 1 times.  We will lower the estimate
     later if such statement must be executed on last iteration  */
  if (is_exit)
    delta = 0;
  else
    delta = 1;
  widest_int new_i_bound = i_bound + delta;

  /* If an overflow occurred, ignore the result.  */
  if (wi::ltu_p (new_i_bound, delta))
    return;

  if (upper && !is_exit)
    do_warn_aggressive_loop_optimizations (loop, new_i_bound, at_stmt);
  record_niter_bound (loop, new_i_bound, realistic, upper);
}

/* Records the control iv analyzed in NITER for LOOP if the iv is valid
   and doesn't overflow.  */

static void
record_control_iv (class loop *loop, class tree_niter_desc *niter)
{
  struct control_iv *iv;

  if (!niter->control.base || !niter->control.step)
    return;

  if (!integer_onep (niter->assumptions) || !niter->control.no_overflow)
    return;

  iv = ggc_alloc<control_iv> ();
  iv->base = niter->control.base;
  iv->step = niter->control.step;
  iv->next = loop->control_ivs;
  loop->control_ivs = iv;

  return;
}

/* This function returns TRUE if below conditions are satisfied:
     1) VAR is SSA variable.
     2) VAR is an IV:{base, step} in its defining loop.
     3) IV doesn't overflow.
     4) Both base and step are integer constants.
     5) Base is the MIN/MAX value depends on IS_MIN.
   Store value of base to INIT correspondingly.  */

static bool
get_cst_init_from_scev (tree var, wide_int *init, bool is_min)
{
  if (TREE_CODE (var) != SSA_NAME)
    return false;

  gimple *def_stmt = SSA_NAME_DEF_STMT (var);
  class loop *loop = loop_containing_stmt (def_stmt);

  if (loop == NULL)
    return false;

  affine_iv iv;
  if (!simple_iv (loop, loop, var, &iv, false))
    return false;

  if (!iv.no_overflow)
    return false;

  if (TREE_CODE (iv.base) != INTEGER_CST || TREE_CODE (iv.step) != INTEGER_CST)
    return false;

  if (is_min == tree_int_cst_sign_bit (iv.step))
    return false;

  *init = wi::to_wide (iv.base);
  return true;
}

/* Record the estimate on number of iterations of LOOP based on the fact that
   the induction variable BASE + STEP * i evaluated in STMT does not wrap and
   its values belong to the range <LOW, HIGH>.  REALISTIC is true if the
   estimated number of iterations is expected to be close to the real one.
   UPPER is true if we are sure the induction variable does not wrap.  */

static void
record_nonwrapping_iv (class loop *loop, tree base, tree step, gimple *stmt,
		       tree low, tree high, bool realistic, bool upper)
{
  tree niter_bound, extreme, delta;
  tree type = TREE_TYPE (base), unsigned_type;
  tree orig_base = base;

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
      wide_int max;
      Value_Range base_range (TREE_TYPE (orig_base));
      if (get_range_query (cfun)->range_of_expr (base_range, orig_base)
	  && !base_range.undefined_p ())
	max = base_range.upper_bound ();
      extreme = fold_convert (unsigned_type, low);
      if (TREE_CODE (orig_base) == SSA_NAME
	  && TREE_CODE (high) == INTEGER_CST
	  && INTEGRAL_TYPE_P (TREE_TYPE (orig_base))
	  && ((!base_range.varying_p ()
	       && !base_range.undefined_p ())
	      || get_cst_init_from_scev (orig_base, &max, false))
	  && wi::gts_p (wi::to_wide (high), max))
	base = wide_int_to_tree (unsigned_type, max);
      else if (TREE_CODE (base) != INTEGER_CST
	       && dominated_by_p (CDI_DOMINATORS,
				  loop->latch, gimple_bb (stmt)))
	base = fold_convert (unsigned_type, high);
      delta = fold_build2 (MINUS_EXPR, unsigned_type, base, extreme);
      step = fold_build1 (NEGATE_EXPR, unsigned_type, step);
    }
  else
    {
      wide_int min;
      Value_Range base_range (TREE_TYPE (orig_base));
      if (get_range_query (cfun)->range_of_expr (base_range, orig_base)
	  && !base_range.undefined_p ())
	min = base_range.lower_bound ();
      extreme = fold_convert (unsigned_type, high);
      if (TREE_CODE (orig_base) == SSA_NAME
	  && TREE_CODE (low) == INTEGER_CST
	  && INTEGRAL_TYPE_P (TREE_TYPE (orig_base))
	  && ((!base_range.varying_p ()
	       && !base_range.undefined_p ())
	      || get_cst_init_from_scev (orig_base, &min, true))
	  && wi::gts_p (min, wi::to_wide (low)))
	base = wide_int_to_tree (unsigned_type, min);
      else if (TREE_CODE (base) != INTEGER_CST
	       && dominated_by_p (CDI_DOMINATORS,
				  loop->latch, gimple_bb (stmt)))
	base = fold_convert (unsigned_type, low);
      delta = fold_build2 (MINUS_EXPR, unsigned_type, extreme, base);
    }

  /* STMT is executed at most NITER_BOUND + 1 times, since otherwise the value
     would get out of the range.  */
  niter_bound = fold_build2 (FLOOR_DIV_EXPR, unsigned_type, delta, step);
  widest_int max = derive_constant_upper_bound (niter_bound);
  record_estimate (loop, niter_bound, max, stmt, false, realistic, upper);
}

/* Determine information about number of iterations a LOOP from the index
   IDX of a data reference accessed in STMT.  RELIABLE is true if STMT is
   guaranteed to be executed in every iteration of LOOP.  Callback for
   for_each_index.  */

struct ilb_data
{
  class loop *loop;
  gimple *stmt;
};

static bool
idx_infer_loop_bounds (tree base, tree *idx, void *dta)
{
  struct ilb_data *data = (struct ilb_data *) dta;
  tree ev, init, step;
  tree low, high, type, next;
  bool sign, upper = true, has_flexible_size = false;
  class loop *loop = data->loop;

  if (TREE_CODE (base) != ARRAY_REF)
    return true;

  /* For arrays that might have flexible sizes, it is not guaranteed that they
     do not really extend over their declared size.  */ 
  if (array_ref_flexible_size_p (base))
    {
      has_flexible_size = true;
      upper = false;
    }

  class loop *dloop = loop_containing_stmt (data->stmt);
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

  /* The array that might have flexible size most likely extends
     beyond its bounds.  */
  if (has_flexible_size
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

  /* If access is not executed on every iteration, we must ensure that overlow
     may not make the access valid later.  */
  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, gimple_bb (data->stmt)))
    {
      if (scev_probably_wraps_p (NULL_TREE,
				 initial_condition_in_loop_num (ev, loop->num),
				 step, data->stmt, loop, true))
	upper = false;
    }
  else
    record_nonwrapping_chrec (ev);

  record_nonwrapping_iv (loop, init, step, data->stmt, low, high, false, upper);
  return true;
}

/* Determine information about number of iterations a LOOP from the bounds
   of arrays in the data reference REF accessed in STMT.  RELIABLE is true if
   STMT is guaranteed to be executed in every iteration of LOOP.*/

static void
infer_loop_bounds_from_ref (class loop *loop, gimple *stmt, tree ref)
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
infer_loop_bounds_from_array (class loop *loop, gimple *stmt)
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
infer_loop_bounds_from_pointer_arith (class loop *loop, gimple *stmt)
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

  class loop *uloop = loop_containing_stmt (stmt);
  scev = instantiate_parameters (loop, analyze_scalar_evolution (uloop, def));
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

  record_nonwrapping_chrec (scev);
  record_nonwrapping_iv (loop, base, step, stmt, low, high, false, true);
}

/* Determine information about number of iterations of a LOOP from the fact
   that signed arithmetics in STMT does not overflow.  */

static void
infer_loop_bounds_from_signedness (class loop *loop, gimple *stmt)
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
  Value_Range r (TREE_TYPE (def));
  get_range_query (cfun)->range_of_expr (r, def);
  if (!r.varying_p () && !r.undefined_p ())
    {
      low = wide_int_to_tree (type, r.lower_bound ());
      high = wide_int_to_tree (type, r.upper_bound ());
    }

  record_nonwrapping_chrec (scev);
  record_nonwrapping_iv (loop, base, step, stmt, low, high, false, true);
}

/* The following analyzers are extracting informations on the bounds
   of LOOP from the following undefined behaviors:

   - data references should not access elements over the statically
     allocated size,

   - signed variables should not overflow when flag_wrapv is not set.
*/

static void
infer_loop_bounds_from_undefined (class loop *loop, basic_block *bbs)
{
  unsigned i;
  gimple_stmt_iterator bsi;
  basic_block bb;
  bool reliable;

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
	  gimple *stmt = gsi_stmt (bsi);

	  infer_loop_bounds_from_array (loop, stmt);

	  if (reliable)
            {
              infer_loop_bounds_from_signedness (loop, stmt);
              infer_loop_bounds_from_pointer_arith (loop, stmt);
            }
  	}

    }
}

/* Compare wide ints, callback for qsort.  */

static int
wide_int_cmp (const void *p1, const void *p2)
{
  const bound_wide_int *d1 = (const bound_wide_int *) p1;
  const bound_wide_int *d2 = (const bound_wide_int *) p2;
  return wi::cmpu (*d1, *d2);
}

/* Return index of BOUND in BOUNDS array sorted in increasing order.
   Lookup by binary search.  */

static int
bound_index (const vec<bound_wide_int> &bounds, const bound_wide_int &bound)
{
  unsigned int end = bounds.length ();
  unsigned int begin = 0;

  /* Find a matching index by means of a binary search.  */
  while (begin != end)
    {
      unsigned int middle = (begin + end) / 2;
      bound_wide_int index = bounds[middle];

      if (index == bound)
	return middle;
      else if (wi::ltu_p (index, bound))
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
discover_iteration_bound_by_body_walk (class loop *loop)
{
  class nb_iter_bound *elt;
  auto_vec<bound_wide_int> bounds;
  vec<vec<basic_block> > queues = vNULL;
  vec<basic_block> queue = vNULL;
  ptrdiff_t queue_index;
  ptrdiff_t latch_index = 0;

  /* Discover what bounds may interest us.  */
  for (elt = loop->bounds; elt; elt = elt->next)
    {
      bound_wide_int bound = elt->bound;

      /* Exit terminates loop at given iteration, while non-exits produce undefined
	 effect on the next iteration.  */
      if (!elt->is_exit)
	{
	  bound += 1;
	  /* If an overflow occurred, ignore the result.  */
	  if (bound == 0)
	    continue;
	}

      if (!loop->any_upper_bound
	  || wi::ltu_p (bound, loop->nb_iterations_upper_bound))
        bounds.safe_push (bound);
    }

  /* Exit early if there is nothing to do.  */
  if (!bounds.exists ())
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, " Trying to walk loop body to reduce the bound.\n");

  /* Sort the bounds in decreasing order.  */
  bounds.qsort (wide_int_cmp);

  /* For every basic block record the lowest bound that is guaranteed to
     terminate the loop.  */

  hash_map<basic_block, ptrdiff_t> bb_bounds;
  for (elt = loop->bounds; elt; elt = elt->next)
    {
      bound_wide_int bound = elt->bound;
      if (!elt->is_exit)
	{
	  bound += 1;
	  /* If an overflow occurred, ignore the result.  */
	  if (bound == 0)
	    continue;
	}

      if (!loop->any_upper_bound
	  || wi::ltu_p (bound, loop->nb_iterations_upper_bound))
	{
	  ptrdiff_t index = bound_index (bounds, bound);
	  ptrdiff_t *entry = bb_bounds.get (gimple_bb (elt->stmt));
	  if (!entry)
	    bb_bounds.put (gimple_bb (elt->stmt), index);
	  else if ((ptrdiff_t)*entry > index)
	    *entry = index;
	}
    }

  hash_map<basic_block, ptrdiff_t> block_priority;

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
  queues.safe_grow_cleared (queue_index + 1, true);
  queue.safe_push (loop->header);
  queues[queue_index] = queue;
  block_priority.put (loop->header, queue_index);

  for (; queue_index >= 0; queue_index--)
    {
      if (latch_index < queue_index)
	{
	  while (queues[queue_index].length ())
	    {
	      basic_block bb;
	      ptrdiff_t bound_index = queue_index;
              edge e;
              edge_iterator ei;

	      queue = queues[queue_index];
	      bb = queue.pop ();

	      /* OK, we later inserted the BB with lower priority, skip it.  */
	      if (*block_priority.get (bb) > queue_index)
		continue;

	      /* See if we can improve the bound.  */
	      ptrdiff_t *entry = bb_bounds.get (bb);
	      if (entry && *entry < bound_index)
		bound_index = *entry;

	      /* Insert succesors into the queue, watch for latch edge
		 and record greatest index we saw.  */
	      FOR_EACH_EDGE (e, ei, bb->succs)
		{
		  bool insert = false;

		  if (loop_exit_edge_p (loop, e))
		    continue;

		  if (e == loop_latch_edge (loop)
		      && latch_index < bound_index)
		    latch_index = bound_index;
		  else if (!(entry = block_priority.get (e->dest)))
		    {
		      insert = true;
		      block_priority.put (e->dest, bound_index);
		    }
		  else if (*entry < bound_index)
		    {
		      insert = true;
		      *entry = bound_index;
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
	  print_decu (bounds[latch_index], dump_file);
	  fprintf (dump_file, "\n");
	}
      record_niter_bound (loop, widest_int::from (bounds[latch_index],
						  SIGNED), false, true);
    }

  queues.release ();
}

/* See if every path cross the loop goes through a statement that is known
   to not execute at the last iteration. In that case we can decrese iteration
   count by 1.  */

static void
maybe_lower_iteration_bound (class loop *loop)
{
  hash_set<gimple *> *not_executed_last_iteration = NULL;
  class nb_iter_bound *elt;
  bool found_exit = false;
  auto_vec<basic_block> queue;
  bitmap visited;

  /* Collect all statements with interesting (i.e. lower than
     nb_iterations_upper_bound) bound on them. 

     TODO: Due to the way record_estimate choose estimates to store, the bounds
     will be always nb_iterations_upper_bound-1.  We can change this to record
     also statements not dominating the loop latch and update the walk bellow
     to the shortest path algorithm.  */
  for (elt = loop->bounds; elt; elt = elt->next)
    {
      if (!elt->is_exit
	  && wi::ltu_p (elt->bound, loop->nb_iterations_upper_bound))
	{
	  if (!not_executed_last_iteration)
	    not_executed_last_iteration = new hash_set<gimple *>;
	  not_executed_last_iteration->add (elt->stmt);
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
	  gimple *stmt = gsi_stmt (gsi);
	  if (not_executed_last_iteration->contains (stmt))
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
      record_niter_bound (loop, widest_int::from (loop->nb_iterations_upper_bound,
						  SIGNED) - 1,
			  false, true);
    }

  BITMAP_FREE (visited);
  delete not_executed_last_iteration;
}

/* Get expected upper bound for number of loop iterations for
   BUILT_IN_EXPECT_WITH_PROBABILITY for a condition COND.  */

static tree
get_upper_bound_based_on_builtin_expr_with_prob (gcond *cond)
{
  if (cond == NULL)
    return NULL_TREE;

  tree lhs = gimple_cond_lhs (cond);
  if (TREE_CODE (lhs) != SSA_NAME)
    return NULL_TREE;

  gimple *stmt = SSA_NAME_DEF_STMT (gimple_cond_lhs (cond));
  gcall *def = dyn_cast<gcall *> (stmt);
  if (def == NULL)
    return NULL_TREE;

  tree decl = gimple_call_fndecl (def);
  if (!decl
      || !fndecl_built_in_p (decl, BUILT_IN_EXPECT_WITH_PROBABILITY)
      || gimple_call_num_args (stmt) != 3)
    return NULL_TREE;

  tree c = gimple_call_arg (def, 1);
  tree condt = TREE_TYPE (lhs);
  tree res = fold_build2 (gimple_cond_code (cond),
			  condt, c,
			  gimple_cond_rhs (cond));
  if (TREE_CODE (res) != INTEGER_CST)
    return NULL_TREE;


  tree prob = gimple_call_arg (def, 2);
  tree t = TREE_TYPE (prob);
  tree one
    = build_real_from_int_cst (t,
			       integer_one_node);
  if (integer_zerop (res))
    prob = fold_build2 (MINUS_EXPR, t, one, prob);
  tree r = fold_build2 (RDIV_EXPR, t, one, prob);
  if (TREE_CODE (r) != REAL_CST)
    return NULL_TREE;

  HOST_WIDE_INT probi
    = real_to_integer (TREE_REAL_CST_PTR (r));
  return build_int_cst (condt, probi);
}

/* Records estimates on numbers of iterations of LOOP.  If USE_UNDEFINED_P
   is true also use estimates derived from undefined behavior.  */

void
estimate_numbers_of_iterations (class loop *loop)
{
  tree niter, type;
  unsigned i;
  class tree_niter_desc niter_desc;
  edge ex;
  widest_int bound;
  edge likely_exit;

  /* Give up if we already have tried to compute an estimation.  */
  if (loop->estimate_state != EST_NOT_COMPUTED)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Estimating # of iterations of loop %d\n", loop->num);

  loop->estimate_state = EST_AVAILABLE;

  sreal nit;
  bool reliable;

  /* If we have a measured profile, use it to estimate the number of
     iterations.  Normally this is recorded by branch_prob right after
     reading the profile.  In case we however found a new loop, record the
     information here.

     Explicitly check for profile status so we do not report
     wrong prediction hitrates for guessed loop iterations heuristics.
     Do not recompute already recorded bounds - we ought to be better on
     updating iteration bounds than updating profile in general and thus
     recomputing iteration bounds later in the compilation process will just
     introduce random roundoff errors.  */
  if (!loop->any_estimate
      && expected_loop_iterations_by_profile (loop, &nit, &reliable)
      && reliable)
    {
      bound = nit.to_nearest_int ();
      record_niter_bound (loop, bound, true, false);
    }

  /* Ensure that loop->nb_iterations is computed if possible.  If it turns out
     to be constant, we avoid undefined behavior implied bounds and instead
     diagnose those loops with -Waggressive-loop-optimizations.  */
  number_of_latch_executions (loop);

  basic_block *body = get_loop_body (loop);
  auto_vec<edge> exits = get_loop_exit_edges (loop, body);
  likely_exit = single_likely_exit (loop, exits);
  FOR_EACH_VEC_ELT (exits, i, ex)
    {
      if (ex == likely_exit)
	{
	  gimple *stmt = *gsi_last_bb (ex->src);
	  if (stmt != NULL)
	    {
	      gcond *cond = dyn_cast<gcond *> (stmt);
	      tree niter_bound
		= get_upper_bound_based_on_builtin_expr_with_prob (cond);
	      if (niter_bound != NULL_TREE)
		{
		  widest_int max = derive_constant_upper_bound (niter_bound);
		  record_estimate (loop, niter_bound, max, cond,
				   true, true, false);
		}
	    }
	}

      if (!number_of_iterations_exit (loop, ex, &niter_desc,
				      false, false, body))
	continue;

      niter = niter_desc.niter;
      type = TREE_TYPE (niter);
      if (TREE_CODE (niter_desc.may_be_zero) != INTEGER_CST)
	niter = build3 (COND_EXPR, type, niter_desc.may_be_zero,
			build_int_cst (type, 0),
			niter);
      record_estimate (loop, niter, niter_desc.max,
		       last_nondebug_stmt (ex->src),
		       true, ex == likely_exit, true);
      record_control_iv (loop, &niter_desc);
    }

  if (flag_aggressive_loop_optimizations)
    infer_loop_bounds_from_undefined (loop, body);
  free (body);

  discover_iteration_bound_by_body_walk (loop);

  maybe_lower_iteration_bound (loop);

  /* If we know the exact number of iterations of this loop, try to
     not break code with undefined behavior by not recording smaller
     maximum number of iterations.  */
  if (loop->nb_iterations
      && TREE_CODE (loop->nb_iterations) == INTEGER_CST
      && (wi::min_precision (wi::to_widest (loop->nb_iterations), SIGNED)
	  <= bound_wide_int ().get_precision ()))
    {
      loop->any_upper_bound = true;
      loop->nb_iterations_upper_bound
	= bound_wide_int::from (wi::to_widest (loop->nb_iterations), SIGNED);
    }
}

/* Sets NIT to the estimated number of executions of the latch of the
   LOOP.  If CONSERVATIVE is true, we must be sure that NIT is at least as
   large as the number of iterations.  If we have no reliable estimate,
   the function returns false, otherwise returns true.  */

bool
estimated_loop_iterations (class loop *loop, widest_int *nit)
{
  /* When SCEV information is available, try to update loop iterations
     estimate.  Otherwise just return whatever we recorded earlier.  */
  if (scev_initialized_p ())
    estimate_numbers_of_iterations (loop);

  return (get_estimated_loop_iterations (loop, nit));
}

/* Similar to estimated_loop_iterations, but returns the estimate only
   if it fits to HOST_WIDE_INT.  If this is not the case, or the estimate
   on the number of iterations of LOOP could not be derived, returns -1.  */

HOST_WIDE_INT
estimated_loop_iterations_int (class loop *loop)
{
  widest_int nit;
  HOST_WIDE_INT hwi_nit;

  if (!estimated_loop_iterations (loop, &nit))
    return -1;

  if (!wi::fits_shwi_p (nit))
    return -1;
  hwi_nit = nit.to_shwi ();

  return hwi_nit < 0 ? -1 : hwi_nit;
}


/* Sets NIT to an upper bound for the maximum number of executions of the
   latch of the LOOP.  If we have no reliable estimate, the function returns
   false, otherwise returns true.  */

bool
max_loop_iterations (class loop *loop, widest_int *nit)
{
  /* When SCEV information is available, try to update loop iterations
     estimate.  Otherwise just return whatever we recorded earlier.  */
  if (scev_initialized_p ())
    estimate_numbers_of_iterations (loop);

  return get_max_loop_iterations (loop, nit);
}

/* Similar to max_loop_iterations, but returns the estimate only
   if it fits to HOST_WIDE_INT.  If this is not the case, or the estimate
   on the number of iterations of LOOP could not be derived, returns -1.  */

HOST_WIDE_INT
max_loop_iterations_int (class loop *loop)
{
  widest_int nit;
  HOST_WIDE_INT hwi_nit;

  if (!max_loop_iterations (loop, &nit))
    return -1;

  if (!wi::fits_shwi_p (nit))
    return -1;
  hwi_nit = nit.to_shwi ();

  return hwi_nit < 0 ? -1 : hwi_nit;
}

/* Sets NIT to an likely upper bound for the maximum number of executions of the
   latch of the LOOP.  If we have no reliable estimate, the function returns
   false, otherwise returns true.  */

bool
likely_max_loop_iterations (class loop *loop, widest_int *nit)
{
  /* When SCEV information is available, try to update loop iterations
     estimate.  Otherwise just return whatever we recorded earlier.  */
  if (scev_initialized_p ())
    estimate_numbers_of_iterations (loop);

  return get_likely_max_loop_iterations (loop, nit);
}

/* Similar to max_loop_iterations, but returns the estimate only
   if it fits to HOST_WIDE_INT.  If this is not the case, or the estimate
   on the number of iterations of LOOP could not be derived, returns -1.  */

HOST_WIDE_INT
likely_max_loop_iterations_int (class loop *loop)
{
  widest_int nit;
  HOST_WIDE_INT hwi_nit;

  if (!likely_max_loop_iterations (loop, &nit))
    return -1;

  if (!wi::fits_shwi_p (nit))
    return -1;
  hwi_nit = nit.to_shwi ();

  return hwi_nit < 0 ? -1 : hwi_nit;
}

/* Returns an estimate for the number of executions of statements
   in the LOOP.  For statements before the loop exit, this exceeds
   the number of execution of the latch by one.  */

HOST_WIDE_INT
estimated_stmt_executions_int (class loop *loop)
{
  HOST_WIDE_INT nit = estimated_loop_iterations_int (loop);
  HOST_WIDE_INT snit;

  if (nit == -1)
    return -1;

  snit = (HOST_WIDE_INT) ((unsigned HOST_WIDE_INT) nit + 1);

  /* If the computation overflows, return -1.  */
  return snit < 0 ? -1 : snit;
}

/* Sets NIT to the maximum number of executions of the latch of the
   LOOP, plus one.  If we have no reliable estimate, the function returns
   false, otherwise returns true.  */

bool
max_stmt_executions (class loop *loop, widest_int *nit)
{
  widest_int nit_minus_one;

  if (!max_loop_iterations (loop, nit))
    return false;

  nit_minus_one = *nit;

  *nit += 1;

  return wi::gtu_p (*nit, nit_minus_one);
}

/* Sets NIT to the estimated maximum number of executions of the latch of the
   LOOP, plus one.  If we have no likely estimate, the function returns
   false, otherwise returns true.  */

bool
likely_max_stmt_executions (class loop *loop, widest_int *nit)
{
  widest_int nit_minus_one;

  if (!likely_max_loop_iterations (loop, nit))
    return false;

  nit_minus_one = *nit;

  *nit += 1;

  return wi::gtu_p (*nit, nit_minus_one);
}

/* Sets NIT to the estimated number of executions of the latch of the
   LOOP, plus one.  If we have no reliable estimate, the function returns
   false, otherwise returns true.  */

bool
estimated_stmt_executions (class loop *loop, widest_int *nit)
{
  widest_int nit_minus_one;

  if (!estimated_loop_iterations (loop, nit))
    return false;

  nit_minus_one = *nit;

  *nit += 1;

  return wi::gtu_p (*nit, nit_minus_one);
}

/* Records estimates on numbers of iterations of loops.  */

void
estimate_numbers_of_iterations (function *fn)
{
  /* We don't want to issue signed overflow warnings while getting
     loop iteration estimates.  */
  fold_defer_overflow_warnings ();

  for (auto loop : loops_list (fn, 0))
    estimate_numbers_of_iterations (loop);

  fold_undefer_and_ignore_overflow_warnings ();
}

/* Returns true if statement S1 dominates statement S2.  */

bool
stmt_dominates_stmt_p (gimple *s1, gimple *s2)
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
n_of_executions_at_most (gimple *stmt,
			 class nb_iter_bound *niter_bound,
			 tree niter)
{
  widest_int bound = widest_int::from (niter_bound->bound, SIGNED);
  tree nit_type = TREE_TYPE (niter), e;
  enum tree_code cmp;

  gcc_assert (TYPE_UNSIGNED (nit_type));

  /* If the bound does not even fit into NIT_TYPE, it cannot tell us that
     the number of iterations is small.  */
  if (!wi::fits_to_tree_p (bound, nit_type))
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
	     cannot be terinated by a side effect in between.  */
	  for (bsi = gsi_for_stmt (stmt); gsi_stmt (bsi) != niter_bound->stmt;
	       gsi_next (&bsi))
	    if (gimple_has_side_effects (gsi_stmt (bsi)))
	       return false;
	  bound += 1;
	  if (bound == 0
	      || !wi::fits_to_tree_p (bound, nit_type))
	    return false;
	}
      cmp = GT_EXPR;
    }

  e = fold_binary (cmp, boolean_type_node,
		   niter, wide_int_to_tree (nit_type, bound));
  return e && integer_nonzerop (e);
}

/* Returns true if the arithmetics in TYPE can be assumed not to wrap.  */

bool
nowrap_type_p (tree type)
{
  if (ANY_INTEGRAL_TYPE_P (type)
      && TYPE_OVERFLOW_UNDEFINED (type))
    return true;

  if (POINTER_TYPE_P (type))
    return true;

  return false;
}

/* Return true if we can prove LOOP is exited before evolution of induction
   variable {BASE, STEP} overflows with respect to its type bound.  */

static bool
loop_exits_before_overflow (tree base, tree step,
			    gimple *at_stmt, class loop *loop)
{
  widest_int niter;
  struct control_iv *civ;
  class nb_iter_bound *bound;
  tree e, delta, step_abs, unsigned_base;
  tree type = TREE_TYPE (step);
  tree unsigned_type, valid_niter;

  /* Don't issue signed overflow warnings.  */
  fold_defer_overflow_warnings ();

  /* Compute the number of iterations before we reach the bound of the
     type, and verify that the loop is exited before this occurs.  */
  unsigned_type = unsigned_type_for (type);
  unsigned_base = fold_convert (unsigned_type, base);

  if (tree_int_cst_sign_bit (step))
    {
      tree extreme = fold_convert (unsigned_type,
				   lower_bound_in_type (type, type));
      delta = fold_build2 (MINUS_EXPR, unsigned_type, unsigned_base, extreme);
      step_abs = fold_build1 (NEGATE_EXPR, unsigned_type,
			      fold_convert (unsigned_type, step));
    }
  else
    {
      tree extreme = fold_convert (unsigned_type,
				   upper_bound_in_type (type, type));
      delta = fold_build2 (MINUS_EXPR, unsigned_type, extreme, unsigned_base);
      step_abs = fold_convert (unsigned_type, step);
    }

  valid_niter = fold_build2 (FLOOR_DIV_EXPR, unsigned_type, delta, step_abs);

  estimate_numbers_of_iterations (loop);

  if (max_loop_iterations (loop, &niter)
      && wi::fits_to_tree_p (niter, TREE_TYPE (valid_niter))
      && (e = fold_binary (GT_EXPR, boolean_type_node, valid_niter,
			   wide_int_to_tree (TREE_TYPE (valid_niter),
					     niter))) != NULL
      && integer_nonzerop (e))
    {
      fold_undefer_and_ignore_overflow_warnings ();
      return true;
    }
  if (at_stmt)
    for (bound = loop->bounds; bound; bound = bound->next)
      {
	if (n_of_executions_at_most (at_stmt, bound, valid_niter))
	  {
	    fold_undefer_and_ignore_overflow_warnings ();
	    return true;
	  }
      }
  fold_undefer_and_ignore_overflow_warnings ();

  /* Try to prove loop is exited before {base, step} overflows with the
     help of analyzed loop control IV.  This is done only for IVs with
     constant step because otherwise we don't have the information.  */
  if (TREE_CODE (step) == INTEGER_CST)
    {
      for (civ = loop->control_ivs; civ; civ = civ->next)
	{
	  enum tree_code code;
	  tree civ_type = TREE_TYPE (civ->step);

	  /* Have to consider type difference because operand_equal_p ignores
	     that for constants.  */
	  if (TYPE_UNSIGNED (type) != TYPE_UNSIGNED (civ_type)
	      || element_precision (type) != element_precision (civ_type))
	    continue;

	  /* Only consider control IV with same step.  */
	  if (!operand_equal_p (step, civ->step, 0))
	    continue;

	  /* Done proving if this is a no-overflow control IV.  */
	  if (operand_equal_p (base, civ->base, 0))
	    return true;

	  /* Control IV is recorded after expanding simple operations,
	     Here we expand base and compare it too.  */
	  tree expanded_base = expand_simple_operations (base);
	  if (operand_equal_p (expanded_base, civ->base, 0))
	    return true;

	  /* If this is a before stepping control IV, in other words, we have

	       {civ_base, step} = {base + step, step}

	     Because civ {base + step, step} doesn't overflow during loop
	     iterations, {base, step} will not overflow if we can prove the
	     operation "base + step" does not overflow.  Specifically, we try
	     to prove below conditions are satisfied:

	       base <= UPPER_BOUND (type) - step  ;;step > 0
	       base >= LOWER_BOUND (type) - step  ;;step < 0

	     by proving the reverse conditions are false using loop's initial
	     condition.  */
	  if (POINTER_TYPE_P (TREE_TYPE (base)))
	    code = POINTER_PLUS_EXPR;
	  else
	    code = PLUS_EXPR;

	  tree stepped = fold_build2 (code, TREE_TYPE (base), base, step);
	  tree expanded_stepped = fold_build2 (code, TREE_TYPE (base),
					       expanded_base, step);
	  if (operand_equal_p (stepped, civ->base, 0)
	      || operand_equal_p (expanded_stepped, civ->base, 0))
	    {
	      tree extreme;

	      if (tree_int_cst_sign_bit (step))
		{
		  code = LT_EXPR;
		  extreme = lower_bound_in_type (type, type);
		}
	      else
		{
		  code = GT_EXPR;
		  extreme = upper_bound_in_type (type, type);
		}
	      extreme = fold_build2 (MINUS_EXPR, type, extreme, step);
	      e = fold_build2 (code, boolean_type_node, base, extreme);
	      e = simplify_using_initial_conditions (loop, e);
	      if (integer_zerop (e))
		return true;
	    }
        }
    }

  return false;
}

/* VAR is scev variable whose evolution part is constant STEP, this function
   proves that VAR can't overflow by using value range info.  If VAR's value
   range is [MIN, MAX], it can be proven by:
     MAX + step doesn't overflow    ; if step > 0
   or
     MIN + step doesn't underflow   ; if step < 0.

   We can only do this if var is computed in every loop iteration, i.e, var's
   definition has to dominate loop latch.  Consider below example:

     {
       unsigned int i;

       <bb 3>:

       <bb 4>:
       # RANGE [0, 4294967294] NONZERO 65535
       # i_21 = PHI <0(3), i_18(9)>
       if (i_21 != 0)
	 goto <bb 6>;
       else
	 goto <bb 8>;

       <bb 6>:
       # RANGE [0, 65533] NONZERO 65535
       _6 = i_21 + 4294967295;
       # RANGE [0, 65533] NONZERO 65535
       _7 = (long unsigned int) _6;
       # RANGE [0, 524264] NONZERO 524280
       _8 = _7 * 8;
       # PT = nonlocal escaped
       _9 = a_14 + _8;
       *_9 = 0;

       <bb 8>:
       # RANGE [1, 65535] NONZERO 65535
       i_18 = i_21 + 1;
       if (i_18 >= 65535)
	 goto <bb 10>;
       else
	 goto <bb 9>;

       <bb 9>:
       goto <bb 4>;

       <bb 10>:
       return;
     }

   VAR _6 doesn't overflow only with pre-condition (i_21 != 0), here we
   can't use _6 to prove no-overlfow for _7.  In fact, var _7 takes value
   sequence (4294967295, 0, 1, ..., 65533) in loop life time, rather than
   (4294967295, 4294967296, ...).  */

static bool
scev_var_range_cant_overflow (tree var, tree step, class loop *loop)
{
  tree type;
  wide_int minv, maxv, diff, step_wi;

  if (TREE_CODE (step) != INTEGER_CST || !INTEGRAL_TYPE_P (TREE_TYPE (var)))
    return false;

  /* Check if VAR evaluates in every loop iteration.  It's not the case
     if VAR is default definition or does not dominate loop's latch.  */
  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (var));
  if (!def_bb || !dominated_by_p (CDI_DOMINATORS, loop->latch, def_bb))
    return false;

  Value_Range r (TREE_TYPE (var));
  get_range_query (cfun)->range_of_expr (r, var);
  if (r.varying_p () || r.undefined_p ())
    return false;

  /* VAR is a scev whose evolution part is STEP and value range info
     is [MIN, MAX], we can prove its no-overflowness by conditions:

       type_MAX - MAX >= step   ; if step > 0
       MIN - type_MIN >= |step| ; if step < 0.

     Or VAR must take value outside of value range, which is not true.  */
  step_wi = wi::to_wide (step);
  type = TREE_TYPE (var);
  if (tree_int_cst_sign_bit (step))
    {
      diff = r.lower_bound () - wi::to_wide (lower_bound_in_type (type, type));
      step_wi = - step_wi;
    }
  else
    diff = wi::to_wide (upper_bound_in_type (type, type)) - r.upper_bound ();

  return (wi::geu_p (diff, step_wi));
}

/* Return false only when the induction variable BASE + STEP * I is
   known to not overflow: i.e. when the number of iterations is small
   enough with respect to the step and initial condition in order to
   keep the evolution confined in TYPEs bounds.  Return true when the
   iv is known to overflow or when the property is not computable.

   USE_OVERFLOW_SEMANTICS is true if this function should assume that
   the rules for overflow of the given language apply (e.g., that signed
   arithmetics in C does not overflow).

   If VAR is a ssa variable, this function also returns false if VAR can
   be proven not overflow with value range info.  */

bool
scev_probably_wraps_p (tree var, tree base, tree step,
		       gimple *at_stmt, class loop *loop,
		       bool use_overflow_semantics)
{
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

  /* Check if var can be proven not overflow with value range info.  */
  if (var && TREE_CODE (var) == SSA_NAME
      && scev_var_range_cant_overflow (var, step, loop))
    return false;

  if (loop_exits_before_overflow (base, step, at_stmt, loop))
    return false;

  /* Check the nonwrapping flag, which may be set by niter analysis (e.g., the
     above loop exits before overflow).  */
  if (var && nonwrapping_chrec_p (analyze_scalar_evolution (loop, var)))
    return false;

  /* At this point we still don't have a proof that the iv does not
     overflow: give up.  */
  return true;
}

/* Frees the information on upper bounds on numbers of iterations of LOOP.  */

void
free_numbers_of_iterations_estimates (class loop *loop)
{
  struct control_iv *civ;
  class nb_iter_bound *bound;

  loop->nb_iterations = NULL;
  loop->estimate_state = EST_NOT_COMPUTED;
  for (bound = loop->bounds; bound;)
    {
      class nb_iter_bound *next = bound->next;
      ggc_free (bound);
      bound = next;
    }
  loop->bounds = NULL;

  for (civ = loop->control_ivs; civ;)
    {
      struct control_iv *next = civ->next;
      ggc_free (civ);
      civ = next;
    }
  loop->control_ivs = NULL;
}

/* Frees the information on upper bounds on numbers of iterations of loops.  */

void
free_numbers_of_iterations_estimates (function *fn)
{
  for (auto loop : loops_list (fn, 0))
    free_numbers_of_iterations_estimates (loop);
}

/* Substitute value VAL for ssa name NAME inside expressions held
   at LOOP.  */

void
substitute_in_loop_info (class loop *loop, tree name, tree val)
{
  loop->nb_iterations = simplify_replace_tree (loop->nb_iterations, name, val);
}
