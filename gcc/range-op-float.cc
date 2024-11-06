/* Floating point range operators.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

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

#define INCLUDE_MEMORY
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
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "value-relation.h"
#include "range-op.h"
#include "range-op-mixed.h"

// Default definitions for floating point operators.

bool
range_operator::fold_range (frange &r, tree type,
				  const frange &op1, const frange &op2,
				  relation_trio trio) const
{
  if (empty_range_varying (r, type, op1, op2))
    return true;
  if (op1.known_isnan () || op2.known_isnan ())
    {
      r.set_nan (type);
      return true;
    }

  rv_fold (r, type,
	   op1.lower_bound (), op1.upper_bound (),
	   op2.lower_bound (), op2.upper_bound (), trio.op1_op2 ());

  if (r.known_isnan ())
    return true;
  if (op1.maybe_isnan () || op2.maybe_isnan ())
    r.update_nan ();

  // If the result has overflowed and flag_trapping_math, folding this
  // operation could elide an overflow or division by zero exception.
  // Avoid returning a singleton +-INF, to keep the propagators (DOM
  // and substitute_and_fold_engine) from folding.  See PR107608.
  if (flag_trapping_math
      && MODE_HAS_INFINITIES (TYPE_MODE (type))
      && r.known_isinf () && !op1.known_isinf () && !op2.known_isinf ())
    {
      REAL_VALUE_TYPE inf = r.lower_bound ();
      if (real_isneg (&inf))
	{
	  REAL_VALUE_TYPE min = real_min_representable (type);
	  r.set (type, inf, min);
	}
      else
	{
	  REAL_VALUE_TYPE max = real_max_representable (type);
	  r.set (type, max, inf);
	}
    }

  r.flush_denormals_to_zero ();

  return true;
}

// For a given operation, fold two sets of ranges into [lb, ub].
// MAYBE_NAN is set to TRUE if, in addition to any result in LB or
// UB, the final range has the possibility of a NAN.
void
range_operator::rv_fold (frange &r, tree type,
			 const REAL_VALUE_TYPE &,
			 const REAL_VALUE_TYPE &,
			 const REAL_VALUE_TYPE &,
			 const REAL_VALUE_TYPE &, relation_kind) const
{
  r.set (type, dconstninf, dconstinf, nan_state (true));
}

bool
range_operator::fold_range (irange &r ATTRIBUTE_UNUSED,
				  tree type ATTRIBUTE_UNUSED,
				  const frange &lh ATTRIBUTE_UNUSED,
				  const irange &rh ATTRIBUTE_UNUSED,
				  relation_trio) const
{
  return false;
}

bool
range_operator::fold_range (irange &r ATTRIBUTE_UNUSED,
				  tree type ATTRIBUTE_UNUSED,
				  const frange &lh ATTRIBUTE_UNUSED,
				  const frange &rh ATTRIBUTE_UNUSED,
				  relation_trio) const
{
  return false;
}

bool
range_operator::fold_range (frange &r ATTRIBUTE_UNUSED,
			    tree type ATTRIBUTE_UNUSED,
			    const irange &lh ATTRIBUTE_UNUSED,
			    const irange &rh ATTRIBUTE_UNUSED,
			    relation_trio) const
{
  return false;
}

bool
range_operator::op1_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const frange &lhs ATTRIBUTE_UNUSED,
				 const frange &op2 ATTRIBUTE_UNUSED,
				 relation_trio) const
{
  return false;
}

bool
range_operator::op1_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const irange &lhs ATTRIBUTE_UNUSED,
				 const frange &op2 ATTRIBUTE_UNUSED,
				 relation_trio) const
{
  return false;
}

bool
range_operator::op2_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const frange &lhs ATTRIBUTE_UNUSED,
				 const frange &op1 ATTRIBUTE_UNUSED,
				 relation_trio) const
{
  return false;
}

bool
range_operator::op2_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const irange &lhs ATTRIBUTE_UNUSED,
				 const frange &op1 ATTRIBUTE_UNUSED,
				 relation_trio) const
{
  return false;
}

relation_kind
range_operator::lhs_op1_relation (const frange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator::lhs_op1_relation (const irange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator::lhs_op2_relation (const irange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator::lhs_op2_relation (const frange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator::op1_op2_relation (const irange &,
				  const frange &,
				  const frange &) const
{
  return VREL_VARYING;
}


relation_kind
range_operator::op1_op2_relation (const frange &,
				  const frange &,
				  const frange &) const
{
  return VREL_VARYING;
}

// Return TRUE if OP1 and OP2 may be a NAN.

static inline bool
maybe_isnan (const frange &op1, const frange &op2)
{
  return op1.maybe_isnan () || op2.maybe_isnan ();
}

// Floating point version of relop_early_resolve that takes NANs into
// account.
//
// For relation opcodes, first try to see if the supplied relation
// forces a true or false result, and return that.
// Then check for undefined operands.  If none of this applies,
// return false.
//
// TRIO are the relations between operands as they appear in the IL.
// MY_REL is the relation that corresponds to the operator being
// folded.  For example, when attempting to fold x_3 == y_5, MY_REL is
// VREL_EQ, and if the statement is dominated by x_3 > y_5, then
// TRIO.op1_op2() is VREL_GT.

static inline bool
frelop_early_resolve (irange &r, tree type,
		      const frange &op1, const frange &op2,
		      relation_trio trio, relation_kind my_rel)
{
  relation_kind rel = trio.op1_op2 ();

  // If known relation is a complete subset of this relation, always
  // return true.  However, avoid doing this when NAN is a possibility
  // as we'll incorrectly fold conditions:
  //
  //   if (x_3 >= y_5)
  //     ;
  //   else
  //     ;; With NANs the relation here is basically VREL_UNLT, so we
  //     ;; can't fold the following:
  //     if (x_3 < y_5)
  if (!maybe_isnan (op1, op2) && relation_union (rel, my_rel) == my_rel)
    {
      r = range_true (type);
      return true;
    }

  // If known relation has no subset of this relation, always false.
  if (relation_intersect (rel, my_rel) == VREL_UNDEFINED)
    {
      r = range_false (type);
      return true;
    }

  // If either operand is undefined, return VARYING.
  if (empty_range_varying (r, type, op1, op2))
    return true;

  return false;
}

// Set VALUE to its next real value, or INF if the operation overflows.

void
frange_nextafter (enum machine_mode mode,
		  REAL_VALUE_TYPE &value,
		  const REAL_VALUE_TYPE &inf)
{
  if (MODE_COMPOSITE_P (mode)
      && (real_isdenormal (&value, mode) || real_iszero (&value)))
    {
      // IBM extended denormals only have DFmode precision.
      REAL_VALUE_TYPE tmp, tmp2;
      real_convert (&tmp2, DFmode, &value);
      real_nextafter (&tmp, REAL_MODE_FORMAT (DFmode), &tmp2, &inf);
      real_convert (&value, mode, &tmp);
    }
  else
    {
      REAL_VALUE_TYPE tmp;
      real_nextafter (&tmp, REAL_MODE_FORMAT (mode), &value, &inf);
      value = tmp;
    }
}

// Like real_arithmetic, but round the result to INF if the operation
// produced inexact results.
//
// ?? There is still one problematic case, i387.  With
// -fexcess-precision=standard we perform most SF/DFmode arithmetic in
// XFmode (long_double_type_node), so that case is OK.  But without
// -mfpmath=sse, all the SF/DFmode computations are in XFmode
// precision (64-bit mantissa) and only occasionally rounded to
// SF/DFmode (when storing into memory from the 387 stack).  Maybe
// this is ok as well though it is just occasionally more precise. ??

void
frange_arithmetic (enum tree_code code, tree type,
		   REAL_VALUE_TYPE &result,
		   const REAL_VALUE_TYPE &op1,
		   const REAL_VALUE_TYPE &op2,
		   const REAL_VALUE_TYPE &inf)
{
  REAL_VALUE_TYPE value;
  enum machine_mode mode = TYPE_MODE (type);
  bool mode_composite = MODE_COMPOSITE_P (mode);

  bool inexact = real_arithmetic (&value, code, &op1, &op2);
  real_convert (&result, mode, &value);

  /* When rounding towards negative infinity, x + (-x) and
     x - x is -0 rather than +0 real_arithmetic computes.
     So, when we are looking for lower bound (inf is negative),
     use -0 rather than +0.  */
  if (flag_rounding_math
      && (code == PLUS_EXPR || code == MINUS_EXPR)
      && !inexact
      && real_iszero (&result)
      && !real_isneg (&result)
      && real_isneg (&inf))
    {
      REAL_VALUE_TYPE op2a = op2;
      if (code == PLUS_EXPR)
	op2a.sign ^= 1;
      if (real_isneg (&op1) == real_isneg (&op2a) && real_equal (&op1, &op2a))
	result.sign = 1;
    }

  // Be extra careful if there may be discrepancies between the
  // compile and runtime results.
  bool round = false;
  if (mode_composite)
    round = true;
  else
    {
      bool low = real_isneg (&inf);
      round = (low ? !real_less (&result, &value)
		   : !real_less (&value, &result));
      if (real_isinf (&result, !low)
	  && !real_isinf (&value)
	  && !flag_rounding_math)
	{
	  // Use just [+INF, +INF] rather than [MAX, +INF]
	  // even if value is larger than MAX and rounds to
	  // nearest to +INF.  Similarly just [-INF, -INF]
	  // rather than [-INF, +MAX] even if value is smaller
	  // than -MAX and rounds to nearest to -INF.
	  // Unless INEXACT is true, in that case we need some
	  // extra buffer.
	  if (!inexact)
	    round = false;
	  else
	    {
	      REAL_VALUE_TYPE tmp = result, tmp2;
	      frange_nextafter (mode, tmp, inf);
	      // TMP is at this point the maximum representable
	      // number.
	      real_arithmetic (&tmp2, MINUS_EXPR, &value, &tmp);
	      if (real_isneg (&tmp2) != low
		  && (REAL_EXP (&tmp2) - REAL_EXP (&tmp)
		      >= 2 - REAL_MODE_FORMAT (mode)->p))
		round = false;
	    }
	}
    }
  if (round && (inexact || !real_identical (&result, &value)))
    {
      if (mode_composite
	  && (real_isdenormal (&result, mode) || real_iszero (&result)))
	{
	  // IBM extended denormals only have DFmode precision.
	  REAL_VALUE_TYPE tmp, tmp2;
	  real_convert (&tmp2, DFmode, &value);
	  real_nextafter (&tmp, REAL_MODE_FORMAT (DFmode), &tmp2, &inf);
	  real_convert (&result, mode, &tmp);
	}
      else
	frange_nextafter (mode, result, inf);
    }
  if (mode_composite)
    switch (code)
      {
      case PLUS_EXPR:
      case MINUS_EXPR:
	// ibm-ldouble-format documents 1ulp for + and -.
	frange_nextafter (mode, result, inf);
	break;
      case MULT_EXPR:
	// ibm-ldouble-format documents 2ulps for *.
	frange_nextafter (mode, result, inf);
	frange_nextafter (mode, result, inf);
	break;
      case RDIV_EXPR:
	// ibm-ldouble-format documents 3ulps for /.
	frange_nextafter (mode, result, inf);
	frange_nextafter (mode, result, inf);
	frange_nextafter (mode, result, inf);
	break;
      default:
	break;
      }
}

// Crop R to [-INF, MAX] where MAX is the maximum representable number
// for TYPE.

static inline void
frange_drop_inf (frange &r, tree type)
{
  REAL_VALUE_TYPE max = real_max_representable (type);
  frange tmp (type, r.lower_bound (), max);
  r.intersect (tmp);
}

// Crop R to [MIN, +INF] where MIN is the minimum representable number
// for TYPE.

static inline void
frange_drop_ninf (frange &r, tree type)
{
  REAL_VALUE_TYPE min = real_min_representable (type);
  frange tmp (type, min, r.upper_bound ());
  r.intersect (tmp);
}

// Crop R to [MIN, MAX] where MAX is the maximum representable number
// for TYPE and MIN the minimum representable number for TYPE.

static inline void
frange_drop_infs (frange &r, tree type)
{
  REAL_VALUE_TYPE max = real_max_representable (type);
  REAL_VALUE_TYPE min = real_min_representable (type);
  frange tmp (type, min, max);
  r.intersect (tmp);
}

// If zero is in R, make sure both -0.0 and +0.0 are in the range.

static inline void
frange_add_zeros (frange &r, tree type)
{
  if (r.undefined_p () || r.known_isnan ())
    return;

  if (HONOR_SIGNED_ZEROS (type)
      && (real_iszero (&r.lower_bound ()) || real_iszero (&r.upper_bound ())))
    {
      frange zero;
      zero.set_zero (type);
      r.union_ (zero);
    }
}

// Build a range that is <= VAL and store it in R.  Return TRUE if
// further changes may be needed for R, or FALSE if R is in its final
// form.

static bool
build_le (frange &r, tree type, const frange &val)
{
  gcc_checking_assert (!val.known_isnan ());

  REAL_VALUE_TYPE ninf = frange_val_min (type);
  r.set (type, ninf, val.upper_bound ());

  // Add both zeros if there's the possibility of zero equality.
  frange_add_zeros (r, type);

  return true;
}

// Build a range that is < VAL and store it in R.  Return TRUE if
// further changes may be needed for R, or FALSE if R is in its final
// form.

static bool
build_lt (frange &r, tree type, const frange &val)
{
  gcc_checking_assert (!val.known_isnan ());

  // < -INF is outside the range.
  if (real_isinf (&val.upper_bound (), 1))
    {
      if (HONOR_NANS (type))
	r.set_nan (type);
      else
	r.set_undefined ();
      return false;
    }

  REAL_VALUE_TYPE ninf = frange_val_min (type);
  REAL_VALUE_TYPE prev = val.upper_bound ();
  machine_mode mode = TYPE_MODE (type);
  // Default to the conservatively correct closed ranges for
  // MODE_COMPOSITE_P, otherwise use nextafter.  Note that for
  // !HONOR_INFINITIES, nextafter will yield -INF, but frange::set()
  // will crop the range appropriately.
  if (!MODE_COMPOSITE_P (mode))
    frange_nextafter (mode, prev, ninf);
  r.set (type, ninf, prev);
  return true;
}

// Build a range that is >= VAL and store it in R.  Return TRUE if
// further changes may be needed for R, or FALSE if R is in its final
// form.

static bool
build_ge (frange &r, tree type, const frange &val)
{
  gcc_checking_assert (!val.known_isnan ());

  REAL_VALUE_TYPE inf = frange_val_max (type);
  r.set (type, val.lower_bound (), inf);

  // Add both zeros if there's the possibility of zero equality.
  frange_add_zeros (r, type);

  return true;
}

// Build a range that is > VAL and store it in R.  Return TRUE if
// further changes may be needed for R, or FALSE if R is in its final
// form.

static bool
build_gt (frange &r, tree type, const frange &val)
{
  gcc_checking_assert (!val.known_isnan ());

  // > +INF is outside the range.
  if (real_isinf (&val.lower_bound (), 0))
    {
      if (HONOR_NANS (type))
	r.set_nan (type);
      else
	r.set_undefined ();
      return false;
    }

  REAL_VALUE_TYPE inf = frange_val_max (type);
  REAL_VALUE_TYPE next = val.lower_bound ();
  machine_mode mode = TYPE_MODE (type);
  // Default to the conservatively correct closed ranges for
  // MODE_COMPOSITE_P, otherwise use nextafter.  Note that for
  // !HONOR_INFINITIES, nextafter will yield +INF, but frange::set()
  // will crop the range appropriately.
  if (!MODE_COMPOSITE_P (mode))
    frange_nextafter (mode, next, inf);
  r.set (type, next, inf);
  return true;
}


bool
operator_identity::fold_range (frange &r, tree, const frange &op1,
			       const frange &, relation_trio) const
{
  r = op1;
  return true;
}

bool
operator_identity::op1_range (frange &r, tree, const frange &lhs,
			      const frange &, relation_trio) const
{
  r = lhs;
  return true;
}

bool
operator_cst::fold_range (frange &r, tree, const frange &op1,
			  const frange &, relation_trio) const
{
  r = op1;
  return true;
}

bool
operator_equal::op2_range (frange &r, tree type,
			   const irange &lhs, const frange &op1,
			   relation_trio rel) const
{
  return op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
}

bool
operator_equal::fold_range (irange &r, tree type,
			    const frange &op1, const frange &op2,
			    relation_trio rel) const
{
  if (frelop_early_resolve (r, type, op1, op2, rel, VREL_EQ))
    return true;

  if (op1.known_isnan () || op2.known_isnan ())
    r = range_false (type);
  // We can be sure the values are always equal or not if both ranges
  // consist of a single value, and then compare them.
  else if (op1.singleton_p () && op2.singleton_p ())
    {
      if (op1 == op2)
	r = range_true (type);
      // If one operand is -0.0 and other 0.0, they are still equal.
      else if (real_iszero (&op1.lower_bound ())
	       && real_iszero (&op2.lower_bound ()))
	r = range_true (type);
      else
	r = range_false (type);
    }
  else if (real_iszero (&op1.lower_bound ())
	   && real_iszero (&op1.upper_bound ())
	   && real_iszero (&op2.lower_bound ())
	   && real_iszero (&op2.upper_bound ())
	   && !maybe_isnan (op1, op2))
    // [-0.0, 0.0] == [-0.0, 0.0] or similar.
    r = range_true (type);
  else
    {
      // If ranges do not intersect, we know the range is not equal,
      // otherwise we don't know anything for sure.
      frange tmp = op1;
      tmp.intersect (op2);
      if (tmp.undefined_p ())
	{
	  // If one range is [whatever, -0.0] and another
	  // [0.0, whatever2], we don't know anything either,
	  // because -0.0 == 0.0.
	  if ((real_iszero (&op1.upper_bound ())
	       && real_iszero (&op2.lower_bound ()))
	      || (real_iszero (&op1.lower_bound ())
		  && real_iszero (&op2.upper_bound ())))
	    r = range_true_and_false (type);
	  else
	    r = range_false (type);
	}
      else
	r = range_true_and_false (type);
    }
  return true;
}

bool
operator_equal::op1_range (frange &r, tree type,
			    const irange &lhs,
			    const frange &op2,
			    relation_trio trio) const
{
  relation_kind rel = trio.op1_op2 ();
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // The TRUE side of x == NAN is unreachable.
      if (op2.known_isnan ())
	r.set_undefined ();
      else
	{
	  // If it's true, the result is the same as OP2.
	  r = op2;
	  // Add both zeros if there's the possibility of zero equality.
	  frange_add_zeros (r, type);
	  // The TRUE side of op1 == op2 implies op1 is !NAN.
	  r.clear_nan ();
	}
      break;

    case BRS_FALSE:
      // The FALSE side of op1 == op1 implies op1 is a NAN.
      if (rel == VREL_EQ)
	r.set_nan (type);
      // On the FALSE side of x == NAN, we know nothing about x.
      else if (op2.known_isnan ())
	r.set_varying (type);
      // If the result is false, the only time we know anything is
      // if OP2 is a constant.
      else if (op2.singleton_p ()
	       || (!op2.maybe_isnan () && op2.zero_p ()))
	{
	  REAL_VALUE_TYPE tmp = op2.lower_bound ();
	  r.set (type, tmp, tmp, VR_ANTI_RANGE);
	}
      else
	r.set_varying (type);
      break;

    default:
      break;
    }
  return true;
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_equal::op1_op2_relation (const irange &lhs, const frange &,
				  const frange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 == op2 indicates NE_EXPR.
  if (lhs.zero_p ())
    return VREL_NE;

  // TRUE = op1 == op2 indicates EQ_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_EQ;
  return VREL_VARYING;
}

bool
operator_not_equal::fold_range (irange &r, tree type,
				const frange &op1, const frange &op2,
				relation_trio trio) const
{
  relation_kind rel = trio.op1_op2 ();

  // VREL_NE & NE_EXPR is always true, even with NANs.
  if (rel == VREL_NE)
    {
      r = range_true (type);
      return true;
    }
  if (rel == VREL_EQ && maybe_isnan (op1, op2))
    {
      // Avoid frelop_early_resolve() below as it could fold to FALSE
      // without regards to NANs.  This would be incorrect if trying
      // to fold x_5 != x_5 without prior knowledge of NANs.
    }
  else if (frelop_early_resolve (r, type, op1, op2, trio, VREL_NE))
    return true;

  // x != NAN is always TRUE.
  if (op1.known_isnan () || op2.known_isnan ())
    r = range_true (type);
  // We can be sure the values are always equal or not if both ranges
  // consist of a single value, and then compare them.
  else if (op1.singleton_p () && op2.singleton_p ())
    {
      if (op1 == op2)
	r = range_false (type);
      // If one operand is -0.0 and other 0.0, they are still equal.
      else if (real_iszero (&op1.lower_bound ())
	       && real_iszero (&op2.lower_bound ()))
	r = range_false (type);
      else
	r = range_true (type);
    }
  else if (real_iszero (&op1.lower_bound ())
	   && real_iszero (&op1.upper_bound ())
	   && real_iszero (&op2.lower_bound ())
	   && real_iszero (&op2.upper_bound ())
	   && !maybe_isnan (op1, op2))
    // [-0.0, 0.0] != [-0.0, 0.0] or similar.
    r = range_false (type);
  else
    {
      // If ranges do not intersect, we know the range is not equal,
      // otherwise we don't know anything for sure.
      frange tmp = op1;
      tmp.intersect (op2);
      if (tmp.undefined_p ())
	{
	  // If one range is [whatever, -0.0] and another
	  // [0.0, whatever2], we don't know anything either,
	  // because -0.0 == 0.0.
	  if ((real_iszero (&op1.upper_bound ())
	       && real_iszero (&op2.lower_bound ()))
	      || (real_iszero (&op1.lower_bound ())
		  && real_iszero (&op2.upper_bound ())))
	    r = range_true_and_false (type);
	  else
	    r = range_true (type);
	}
      else
	r = range_true_and_false (type);
    }
  return true;
}

bool
operator_not_equal::op1_range (frange &r, tree type,
			       const irange &lhs,
			       const frange &op2,
			       relation_trio trio) const
{
  relation_kind rel = trio.op1_op2 ();
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // If the result is true, the only time we know anything is if
      // OP2 is a constant.
      if (op2.singleton_p ())
	{
	  // This is correct even if op1 is NAN, because the following
	  // range would be ~[tmp, tmp] with the NAN property set to
	  // maybe (VARYING).
	  REAL_VALUE_TYPE tmp = op2.lower_bound ();
	  r.set (type, tmp, tmp, VR_ANTI_RANGE);
	}
      // The TRUE side of op1 != op1 implies op1 is NAN.
      else if (rel == VREL_EQ)
	r.set_nan (type);
      else
	r.set_varying (type);
      break;

    case BRS_FALSE:
      // The FALSE side of x != NAN is impossible.
      if (op2.known_isnan ())
	r.set_undefined ();
      else
	{
	  // If it's false, the result is the same as OP2.
	  r = op2;
	  // Add both zeros if there's the possibility of zero equality.
	  frange_add_zeros (r, type);
	  // The FALSE side of op1 != op2 implies op1 is !NAN.
	  r.clear_nan ();
	}
      break;

    default:
      break;
    }
  return true;
}

bool
operator_not_equal::op2_range (frange &r, tree type,
			       const irange &lhs,
			       const frange &op1,
			       relation_trio trio) const
{
  return op1_range (r, type, lhs, op1, trio);
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_not_equal::op1_op2_relation (const irange &lhs, const frange &,
				      const frange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 != op2  indicates EQ_EXPR.
  if (lhs.zero_p ())
    return VREL_EQ;

  // TRUE = op1 != op2  indicates NE_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_NE;
  return VREL_VARYING;
}

bool
operator_lt::fold_range (irange &r, tree type,
			 const frange &op1, const frange &op2,
			 relation_trio trio) const
{
  if (frelop_early_resolve (r, type, op1, op2, trio, VREL_LT))
    return true;

  if (op1.known_isnan ()
      || op2.known_isnan ()
      || !real_less (&op1.lower_bound (), &op2.upper_bound ()))
    r = range_false (type);
  else if (!maybe_isnan (op1, op2)
	   && real_less (&op1.upper_bound (), &op2.lower_bound ()))
    r = range_true (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_lt::op1_range (frange &r,
			tree type,
			const irange &lhs,
			const frange &op2,
			relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // The TRUE side of x < NAN is unreachable.
      if (op2.known_isnan ())
	r.set_undefined ();
      else if (op2.undefined_p ())
	return false;
      else if (build_lt (r, type, op2))
	{
	  r.clear_nan ();
	  // x < y implies x is not +INF.
	  frange_drop_inf (r, type);
	}
      break;

    case BRS_FALSE:
      // On the FALSE side of x < NAN, we know nothing about x.
      if (op2.maybe_isnan ())
	r.set_varying (type);
      else
	build_ge (r, type, op2);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_lt::op2_range (frange &r,
			tree type,
			const irange &lhs,
			const frange &op1,
			relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // The TRUE side of NAN < x is unreachable.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (op1.undefined_p ())
	return false;
      else if (build_gt (r, type, op1))
	{
	  r.clear_nan ();
	  // x < y implies y is not -INF.
	  frange_drop_ninf (r, type);
	}
      break;

    case BRS_FALSE:
      // On the FALSE side of NAN < x, we know nothing about x.
      if (op1.maybe_isnan ())
	r.set_varying (type);
      else
	build_le (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}


// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_lt::op1_op2_relation (const irange &lhs, const frange &,
			       const frange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 < op2 indicates GE_EXPR.
  if (lhs.zero_p ())
    return VREL_GE;

  // TRUE = op1 < op2 indicates LT_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_LT;
  return VREL_VARYING;
}

bool
operator_le::fold_range (irange &r, tree type,
			 const frange &op1, const frange &op2,
			 relation_trio rel) const
{
  if (frelop_early_resolve (r, type, op1, op2, rel, VREL_LE))
    return true;

  if (op1.known_isnan ()
      || op2.known_isnan ()
      || !real_compare (LE_EXPR, &op1.lower_bound (), &op2.upper_bound ()))
    r = range_false (type);
  else if (!maybe_isnan (op1, op2)
	   && real_compare (LE_EXPR, &op1.upper_bound (), &op2.lower_bound ()))
    r = range_true (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_le::op1_range (frange &r,
			tree type,
			const irange &lhs,
			const frange &op2,
			relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // The TRUE side of x <= NAN is unreachable.
      if (op2.known_isnan ())
	r.set_undefined ();
      else if (op2.undefined_p ())
	return false;
      else if (build_le (r, type, op2))
	r.clear_nan ();
      break;

    case BRS_FALSE:
      // On the FALSE side of x <= NAN, we know nothing about x.
      if (op2.maybe_isnan ())
	r.set_varying (type);
      else
	build_gt (r, type, op2);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_le::op2_range (frange &r,
			tree type,
			const irange &lhs,
			const frange &op1,
			relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // The TRUE side of NAN <= x is unreachable.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (op1.undefined_p ())
	return false;
      else if (build_ge (r, type, op1))
	r.clear_nan ();
      break;

    case BRS_FALSE:
      // On the FALSE side of NAN <= x, we know nothing about x.
      if (op1.maybe_isnan ())
	r.set_varying (type);
      else if (op1.undefined_p ())
	return false;
      else
	build_lt (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_le::op1_op2_relation (const irange &lhs, const frange &,
			       const frange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 <= op2 indicates GT_EXPR.
  if (lhs.zero_p ())
    return VREL_GT;

  // TRUE = op1 <= op2 indicates LE_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_LE;
  return VREL_VARYING;
}

bool
operator_gt::fold_range (irange &r, tree type,
			 const frange &op1, const frange &op2,
			 relation_trio trio) const
{
  if (frelop_early_resolve (r, type, op1, op2, trio, VREL_GT))
    return true;

  if (op1.known_isnan ()
      || op2.known_isnan ()
      || !real_compare (GT_EXPR, &op1.upper_bound (), &op2.lower_bound ()))
    r = range_false (type);
  else if (!maybe_isnan (op1, op2)
	   && real_compare (GT_EXPR, &op1.lower_bound (), &op2.upper_bound ()))
    r = range_true (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_gt::op1_range (frange &r,
			tree type,
			const irange &lhs,
			const frange &op2,
			relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // The TRUE side of x > NAN is unreachable.
      if (op2.known_isnan ())
	r.set_undefined ();
      else if (op2.undefined_p ())
	return false;
      else if (build_gt (r, type, op2))
	{
	  r.clear_nan ();
	  // x > y implies x is not -INF.
	  frange_drop_ninf (r, type);
	}
      break;

    case BRS_FALSE:
      // On the FALSE side of x > NAN, we know nothing about x.
      if (op2.maybe_isnan ())
	r.set_varying (type);
      else if (op2.undefined_p ())
	return false;
      else
	build_le (r, type, op2);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_gt::op2_range (frange &r,
			tree type,
			const irange &lhs,
			const frange &op1,
			relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // The TRUE side of NAN > x is unreachable.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (op1.undefined_p ())
	return false;
      else if (build_lt (r, type, op1))
	{
	  r.clear_nan ();
	  // x > y implies y is not +INF.
	  frange_drop_inf (r, type);
	}
      break;

    case BRS_FALSE:
      // On The FALSE side of NAN > x, we know nothing about x.
      if (op1.maybe_isnan ())
	r.set_varying (type);
      else if (op1.undefined_p ())
	return false;
      else
	build_ge (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_gt::op1_op2_relation (const irange &lhs, const frange &,
			       const frange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 > op2 indicates LE_EXPR.
  if (lhs.zero_p ())
    return VREL_LE;

  // TRUE = op1 > op2 indicates GT_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_GT;
  return VREL_VARYING;
}

bool
operator_ge::fold_range (irange &r, tree type,
			 const frange &op1, const frange &op2,
			 relation_trio rel) const
{
  if (frelop_early_resolve (r, type, op1, op2, rel, VREL_GE))
    return true;

  if (op1.known_isnan ()
      || op2.known_isnan ()
      || !real_compare (GE_EXPR, &op1.upper_bound (), &op2.lower_bound ()))
    r = range_false (type);
  else if (!maybe_isnan (op1, op2)
	   && real_compare (GE_EXPR, &op1.lower_bound (), &op2.upper_bound ()))
    r = range_true (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_ge::op1_range (frange &r,
			tree type,
			const irange &lhs,
			const frange &op2,
			relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // The TRUE side of x >= NAN is unreachable.
      if (op2.known_isnan ())
	r.set_undefined ();
      else if (op2.undefined_p ())
	return false;
      else if (build_ge (r, type, op2))
	r.clear_nan ();
      break;

    case BRS_FALSE:
      // On the FALSE side of x >= NAN, we know nothing about x.
      if (op2.maybe_isnan ())
	r.set_varying (type);
      else if (op2.undefined_p ())
	return false;
      else
	build_lt (r, type, op2);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_ge::op2_range (frange &r, tree type,
			const irange &lhs,
			const frange &op1,
			relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // The TRUE side of NAN >= x is unreachable.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (op1.undefined_p ())
	return false;
      else if (build_le (r, type, op1))
	r.clear_nan ();
      break;

    case BRS_FALSE:
      // On the FALSE side of NAN >= x, we know nothing about x.
      if (op1.maybe_isnan ())
	r.set_varying (type);
      else if (op1.undefined_p ())
	return false;
      else
	build_gt (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

// Check if the LHS range indicates a relation between OP1 and OP2.

relation_kind
operator_ge::op1_op2_relation (const irange &lhs, const frange &,
			       const frange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 >= op2 indicates LT_EXPR.
  if (lhs.zero_p ())
    return VREL_LT;

  // TRUE = op1 >= op2 indicates GE_EXPR.
  if (!contains_zero_p (lhs))
    return VREL_GE;
  return VREL_VARYING;
}

// UNORDERED_EXPR comparison.

class foperator_unordered : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio rel = TRIO_VARYING) const final override
  {
    return op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
  }
} fop_unordered;

bool
foperator_unordered::fold_range (irange &r, tree type,
				 const frange &op1, const frange &op2,
				 relation_trio) const
{
  // UNORDERED is TRUE if either operand is a NAN.
  if (op1.known_isnan () || op2.known_isnan ())
    r = range_true (type);
  // UNORDERED is FALSE if neither operand is a NAN.
  else if (!op1.maybe_isnan () && !op2.maybe_isnan ())
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
foperator_unordered::op1_range (frange &r, tree type,
				const irange &lhs,
				const frange &op2,
				relation_trio trio) const
{
  relation_kind rel = trio.op1_op2 ();
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // Since at least one operand must be NAN, if one of them is
      // not, the other must be.
      if (rel == VREL_EQ || !op2.maybe_isnan ())
	r.set_nan (type);
      else
	r.set_varying (type);
      break;

    case BRS_FALSE:
      // A false UNORDERED means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
      else
	{
	  r.set_varying (type);
	  r.clear_nan ();
	}
      break;

    default:
      break;
    }
  return true;
}

// ORDERED_EXPR comparison.

class foperator_ordered : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio rel = TRIO_VARYING) const final override
  {
    return op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
  }
} fop_ordered;

bool
foperator_ordered::fold_range (irange &r, tree type,
			       const frange &op1, const frange &op2,
			       relation_trio) const
{
  if (op1.known_isnan () || op2.known_isnan ())
    r = range_false (type);
  else if (!op1.maybe_isnan () && !op2.maybe_isnan ())
    r = range_true (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
foperator_ordered::op1_range (frange &r, tree type,
			      const irange &lhs,
			      const frange &op2,
			      relation_trio trio) const
{
  relation_kind rel = trio.op1_op2 ();
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // The TRUE side of ORDERED means both operands are !NAN, so
      // it's impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
      else
	{
	  r.set_varying (type);
	  r.clear_nan ();
	}
      break;

    case BRS_FALSE:
      // The FALSE side of op1 ORDERED op1 implies op1 is NAN.
      if (rel == VREL_EQ)
	r.set_nan (type);
      else
	r.set_varying (type);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_negate::fold_range (frange &r, tree type,
			     const frange &op1, const frange &op2,
			     relation_trio) const
{
  if (empty_range_varying (r, type, op1, op2))
    return true;
  if (op1.known_isnan ())
    {
      bool sign;
      if (op1.nan_signbit_p (sign))
	r.set_nan (type, !sign);
      else
	r.set_nan (type);
      return true;
    }

  REAL_VALUE_TYPE lh_lb = op1.lower_bound ();
  REAL_VALUE_TYPE lh_ub = op1.upper_bound ();
  lh_lb = real_value_negate (&lh_lb);
  lh_ub = real_value_negate (&lh_ub);
  r.set (type, lh_ub, lh_lb);
  if (op1.maybe_isnan ())
    {
      bool sign;
      if (op1.nan_signbit_p (sign))
	r.update_nan (!sign);
      else
	r.update_nan ();
    }
  else
    r.clear_nan ();
  return true;
}

bool
operator_negate::op1_range (frange &r, tree type,
			    const frange &lhs, const frange &op2,
			    relation_trio rel) const
{
  return fold_range (r, type, lhs, op2, rel);
}

bool
operator_abs::fold_range (frange &r, tree type,
			  const frange &op1, const frange &op2,
			  relation_trio) const
{
  if (empty_range_varying (r, type, op1, op2))
    return true;
  if (op1.known_isnan ())
    {
      r.set_nan (type, /*sign=*/false);
      return true;
    }

  const REAL_VALUE_TYPE lh_lb = op1.lower_bound ();
  const REAL_VALUE_TYPE lh_ub = op1.upper_bound ();
  // Handle the easy case where everything is positive.
  if (real_compare (GE_EXPR, &lh_lb, &dconst0)
      && !real_iszero (&lh_lb, /*sign=*/true)
      && !op1.maybe_isnan (/*sign=*/true))
    {
      r = op1;
      return true;
    }

  REAL_VALUE_TYPE min = real_value_abs (&lh_lb);
  REAL_VALUE_TYPE max = real_value_abs (&lh_ub);
  // If the range contains zero then we know that the minimum value in the
  // range will be zero.
  if (real_compare (LE_EXPR, &lh_lb, &dconst0)
      && real_compare (GE_EXPR, &lh_ub, &dconst0))
    {
      if (real_compare (GT_EXPR, &min, &max))
	max = min;
      min = dconst0;
    }
  else
    {
      // If the range was reversed, swap MIN and MAX.
      if (real_compare (GT_EXPR, &min, &max))
	std::swap (min, max);
    }

  r.set (type, min, max);
  if (op1.maybe_isnan ())
    r.update_nan (/*sign=*/false);
  else
    r.clear_nan ();
  return true;
}

bool
operator_abs::op1_range (frange &r, tree type,
			 const frange &lhs, const frange &op2,
			 relation_trio) const
{
  if (empty_range_varying (r, type, lhs, op2))
    return true;
  if (lhs.known_isnan ())
    {
      r.set_nan (type);
      return true;
    }

  // Start with the positives because negatives are an impossible result.
  frange positives (type, dconst0, frange_val_max (type));
  positives.update_nan (/*sign=*/false);
  positives.intersect (lhs);
  r = positives;
  // Add -NAN if relevant.
  if (r.maybe_isnan ())
    {
      frange neg_nan;
      neg_nan.set_nan (type, true);
      r.union_ (neg_nan);
    }
  if (r.known_isnan () || r.undefined_p ())
    return true;
  // Then add the negative of each pair:
  // ABS(op1) = [5,20] would yield op1 => [-20,-5][5,20].
  frange negatives (type, real_value_negate (&positives.upper_bound ()),
		    real_value_negate (&positives.lower_bound ()));
  negatives.clear_nan ();
  r.union_ (negatives);
  return true;
}

class foperator_unordered_lt : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio trio = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_true (type);
	return true;
      }
    frange op1_no_nan = op1;
    frange op2_no_nan = op2;
    if (op1.maybe_isnan ())
      op1_no_nan.clear_nan ();
    if (op2.maybe_isnan ())
      op2_no_nan.clear_nan ();
    if (!range_op_handler (LT_EXPR).fold_range (r, type, op1_no_nan,
						op2_no_nan, trio))
      return false;
    // The result is the same as the ordered version when the
    // comparison is true or when the operands cannot be NANs.
    if (!maybe_isnan (op1, op2) || r == range_true (type))
      return true;
    else
      {
	r = range_true_and_false (type);
	return true;
      }
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs,
		  const frange &op2,
		  relation_trio trio) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs,
		  const frange &op1,
		  relation_trio trio) const final override;
} fop_unordered_lt;

bool
foperator_unordered_lt::op1_range (frange &r, tree type,
				   const irange &lhs,
				   const frange &op2,
				   relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      if (op2.maybe_isnan ())
	r.set_varying (type);
      else if (op2.undefined_p ())
	return false;
      else
	build_lt (r, type, op2);
      break;

    case BRS_FALSE:
      // A false UNORDERED_LT means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
      else if (op2.undefined_p ())
	return false;
      else if (build_ge (r, type, op2))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

bool
foperator_unordered_lt::op2_range (frange &r, tree type,
				   const irange &lhs,
				   const frange &op1,
				   relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      if (op1.maybe_isnan ())
	r.set_varying (type);
      else if (op1.undefined_p ())
	return false;
      else
	build_gt (r, type, op1);
      break;

    case BRS_FALSE:
      // A false UNORDERED_LT means both operands are !NAN, so it's
      // impossible for op1 to be a NAN.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (op1.undefined_p ())
	return false;
      else if (build_le (r, type, op1))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

class foperator_unordered_le : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio trio = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_true (type);
	return true;
      }
    frange op1_no_nan = op1;
    frange op2_no_nan = op2;
    if (op1.maybe_isnan ())
      op1_no_nan.clear_nan ();
    if (op2.maybe_isnan ())
      op2_no_nan.clear_nan ();
    if (!range_op_handler (LE_EXPR).fold_range (r, type, op1_no_nan,
						op2_no_nan, trio))
      return false;
    // The result is the same as the ordered version when the
    // comparison is true or when the operands cannot be NANs.
    if (!maybe_isnan (op1, op2) || r == range_true (type))
      return true;
    else
      {
	r = range_true_and_false (type);
	return true;
      }
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;
} fop_unordered_le;

bool
foperator_unordered_le::op1_range (frange &r, tree type,
				   const irange &lhs, const frange &op2,
				   relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      if (op2.maybe_isnan ())
	r.set_varying (type);
      else if (op2.undefined_p ())
	return false;
      else
	build_le (r, type, op2);
      break;

    case BRS_FALSE:
      // A false UNORDERED_LE means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
      else if (build_gt (r, type, op2))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

bool
foperator_unordered_le::op2_range (frange &r,
				   tree type,
				   const irange &lhs,
				   const frange &op1,
				   relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      if (op1.maybe_isnan ())
	r.set_varying (type);
      else if (op1.undefined_p ())
	return false;
      else
	build_ge (r, type, op1);
      break;

    case BRS_FALSE:
      // A false UNORDERED_LE means both operands are !NAN, so it's
      // impossible for op1 to be a NAN.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (op1.undefined_p ())
	return false;
      else if (build_lt (r, type, op1))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

class foperator_unordered_gt : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio trio = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_true (type);
	return true;
      }
    frange op1_no_nan = op1;
    frange op2_no_nan = op2;
    if (op1.maybe_isnan ())
      op1_no_nan.clear_nan ();
    if (op2.maybe_isnan ())
      op2_no_nan.clear_nan ();
    if (!range_op_handler (GT_EXPR).fold_range (r, type, op1_no_nan,
						op2_no_nan, trio))
      return false;
    // The result is the same as the ordered version when the
    // comparison is true or when the operands cannot be NANs.
    if (!maybe_isnan (op1, op2) || r == range_true (type))
      return true;
    else
      {
	r = range_true_and_false (type);
	return true;
      }
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;
} fop_unordered_gt;

bool
foperator_unordered_gt::op1_range (frange &r,
			 tree type,
			 const irange &lhs,
			 const frange &op2,
			 relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      if (op2.maybe_isnan ())
	r.set_varying (type);
      else if (op2.undefined_p ())
	return false;
      else
	build_gt (r, type, op2);
      break;

    case BRS_FALSE:
      // A false UNORDERED_GT means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
      else if (op2.undefined_p ())
	return false;
      else if (build_le (r, type, op2))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

bool
foperator_unordered_gt::op2_range (frange &r,
				   tree type,
				   const irange &lhs,
				   const frange &op1,
				   relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      if (op1.maybe_isnan ())
	r.set_varying (type);
      else if (op1.undefined_p ())
	return false;
      else
	build_lt (r, type, op1);
      break;

    case BRS_FALSE:
      // A false UNORDERED_GT means both operands are !NAN, so it's
      // impossible for op1 to be a NAN.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (op1.undefined_p ())
	return false;
      else if (build_ge (r, type, op1))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

class foperator_unordered_ge : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio trio = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_true (type);
	return true;
      }
    frange op1_no_nan = op1;
    frange op2_no_nan = op2;
    if (op1.maybe_isnan ())
      op1_no_nan.clear_nan ();
    if (op2.maybe_isnan ())
      op2_no_nan.clear_nan ();
    if (!range_op_handler (GE_EXPR).fold_range (r, type, op1_no_nan,
						op2_no_nan, trio))
      return false;
    // The result is the same as the ordered version when the
    // comparison is true or when the operands cannot be NANs.
    if (!maybe_isnan (op1, op2) || r == range_true (type))
      return true;
    else
      {
	r = range_true_and_false (type);
	return true;
      }
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;
} fop_unordered_ge;

bool
foperator_unordered_ge::op1_range (frange &r,
				   tree type,
				   const irange &lhs,
				   const frange &op2,
				   relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      if (op2.maybe_isnan ())
	r.set_varying (type);
      else if (op2.undefined_p ())
	return false;
      else
	build_ge (r, type, op2);
      break;

    case BRS_FALSE:
      // A false UNORDERED_GE means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
      else if (op2.undefined_p ())
	return false;
      else if (build_lt (r, type, op2))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

bool
foperator_unordered_ge::op2_range (frange &r, tree type,
				   const irange &lhs,
				   const frange &op1,
				   relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      if (op1.maybe_isnan ())
	r.set_varying (type);
      else if (op1.undefined_p ())
	return false;
      else
	build_le (r, type, op1);
      break;

    case BRS_FALSE:
      // A false UNORDERED_GE means both operands are !NAN, so it's
      // impossible for op1 to be a NAN.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (op1.undefined_p ())
	return false;
      else if (build_gt (r, type, op1))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

class foperator_unordered_equal : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio trio = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_true (type);
	return true;
      }
    frange op1_no_nan = op1;
    frange op2_no_nan = op2;
    if (op1.maybe_isnan ())
      op1_no_nan.clear_nan ();
    if (op2.maybe_isnan ())
      op2_no_nan.clear_nan ();
    if (!range_op_handler (EQ_EXPR).fold_range (r, type, op1_no_nan,
						op2_no_nan, trio))
      return false;
    // The result is the same as the ordered version when the
    // comparison is true or when the operands cannot be NANs.
    if (!maybe_isnan (op1, op2) || r == range_true (type))
      return true;
    else
      {
	r = range_true_and_false (type);
	return true;
      }
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio rel = TRIO_VARYING) const final override
  {
    return op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
  }
} fop_unordered_equal;

bool
foperator_unordered_equal::op1_range (frange &r, tree type,
				      const irange &lhs,
				      const frange &op2,
				      relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // If it's true, the result is the same as OP2 plus a NAN.
      r = op2;
      // Add both zeros if there's the possibility of zero equality.
      frange_add_zeros (r, type);
      // Add the possibility of a NAN.
      r.update_nan ();
      break;

    case BRS_FALSE:
      // A false UNORDERED_EQ means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
      else
	{
	  // The false side indicates !NAN and not equal.  We can at least
	  // represent !NAN.
	  r.set_varying (type);
	  r.clear_nan ();
	}
      break;

    default:
      break;
    }
  return true;
}

class foperator_ltgt : public range_operator
{
  using range_operator::fold_range;
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio trio = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_false (type);
	return true;
      }
    frange op1_no_nan = op1;
    frange op2_no_nan = op2;
    if (op1.maybe_isnan ())
      op1_no_nan.clear_nan ();
    if (op2.maybe_isnan ())
      op2_no_nan.clear_nan ();
    if (!range_op_handler (NE_EXPR).fold_range (r, type, op1_no_nan,
						op2_no_nan, trio))
      return false;
    // The result is the same as the ordered version when the
    // comparison is true or when the operands cannot be NANs.
    if (!maybe_isnan (op1, op2) || r == range_false (type))
      return true;
    else
      {
	r = range_true_and_false (type);
	return true;
      }
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio rel = TRIO_VARYING) const final override
  {
    return op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
  }
} fop_ltgt;

bool
foperator_ltgt::op1_range (frange &r, tree type,
			   const irange &lhs,
			   const frange &op2,
			   relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // A true LTGT means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
      else
	{
	  // The true side indicates !NAN and not equal.  We can at least
	  // represent !NAN.
	  r.set_varying (type);
	  r.clear_nan ();
	}
      break;

    case BRS_FALSE:
      // If it's false, the result is the same as OP2 plus a NAN.
      r = op2;
      // Add both zeros if there's the possibility of zero equality.
      frange_add_zeros (r, type);
      // Add the possibility of a NAN.
      r.update_nan ();
      break;

    default:
      break;
    }
  return true;
}

// Final tweaks for float binary op op1_range/op2_range.
// Return TRUE if the operation is performed and a valid range is available.

static bool
float_binary_op_range_finish (bool ret, frange &r, tree type,
			      const frange &lhs, bool div_op2 = false)
{
  if (!ret)
    return false;

  // If we get a known NAN from reverse op, it means either that
  // the other operand was known NAN (in that case we know nothing),
  // or the reverse operation introduced a known NAN.
  // Say for lhs = op1 * op2 if lhs is [-0, +0] and op2 is too,
  // 0 / 0 is known NAN.  Just punt in that case.
  // If NANs aren't honored, we get for 0 / 0 UNDEFINED, so punt as well.
  // Or if lhs is a known NAN, we also don't know anything.
  if (r.known_isnan () || lhs.known_isnan () || r.undefined_p ())
    {
      r.set_varying (type);
      return true;
    }

  // If lhs isn't NAN, then neither operand could be NAN,
  // even if the reverse operation does introduce a maybe_nan.
  if (!lhs.maybe_isnan ())
    {
      r.clear_nan ();
      if (div_op2
	  ? !(real_compare (LE_EXPR, &lhs.lower_bound (), &dconst0)
	      && real_compare (GE_EXPR, &lhs.upper_bound (), &dconst0))
	  : !(real_isinf (&lhs.lower_bound ())
	      || real_isinf (&lhs.upper_bound ())))
	// For reverse + or - or * or op1 of /, if result is finite, then
	// r must be finite too, as X + INF or X - INF or X * INF or
	// INF / X is always +-INF or NAN.  For op2 of /, if result is
	// non-zero and not NAN, r must be finite, as X / INF is always
	// 0 or NAN.
	frange_drop_infs (r, type);
    }
  // If lhs is a maybe or known NAN, the operand could be
  // NAN.
  else
    r.update_nan ();
  return true;
}

// True if [lb, ub] is [+-0, +-0].
static bool
zero_p (const REAL_VALUE_TYPE &lb, const REAL_VALUE_TYPE &ub)
{
  return real_iszero (&lb) && real_iszero (&ub);
}

// True if +0 or -0 is in [lb, ub] range.
static bool
contains_zero_p (const REAL_VALUE_TYPE &lb, const REAL_VALUE_TYPE &ub)
{
  return (real_compare (LE_EXPR, &lb, &dconst0)
	  && real_compare (GE_EXPR, &ub, &dconst0));
}

// True if [lb, ub] is [-INF, -INF] or [+INF, +INF].
static bool
singleton_inf_p (const REAL_VALUE_TYPE &lb, const REAL_VALUE_TYPE &ub)
{
  return real_isinf (&lb) && real_isinf (&ub, real_isneg (&lb));
}

// Return -1 if binary op result must have sign bit set,
// 1 if binary op result must have sign bit clear,
// 0 otherwise.
// Sign bit of binary op result is exclusive or of the
// operand's sign bits.
static int
signbit_known_p (const REAL_VALUE_TYPE &lh_lb, const REAL_VALUE_TYPE &lh_ub,
		 const REAL_VALUE_TYPE &rh_lb, const REAL_VALUE_TYPE &rh_ub)
{
  if (real_isneg (&lh_lb) == real_isneg (&lh_ub)
      && real_isneg (&rh_lb) == real_isneg (&rh_ub))
    {
      if (real_isneg (&lh_lb) == real_isneg (&rh_ub))
	return 1;
      else
	return -1;
    }
  return 0;
}

// Set [lb, ub] to [-0, -0], [-0, +0] or [+0, +0] depending on
// signbit_known.
static void
zero_range (REAL_VALUE_TYPE &lb, REAL_VALUE_TYPE &ub, int signbit_known)
{
  ub = lb = dconst0;
  if (signbit_known <= 0)
    lb = dconstm0;
  if (signbit_known < 0)
    ub = lb;
}

// Set [lb, ub] to [-INF, -INF], [-INF, +INF] or [+INF, +INF] depending on
// signbit_known.
static void
inf_range (REAL_VALUE_TYPE &lb, REAL_VALUE_TYPE &ub, int signbit_known)
{
  if (signbit_known > 0)
    ub = lb = dconstinf;
  else if (signbit_known < 0)
    ub = lb = dconstninf;
  else
    {
      lb = dconstninf;
      ub = dconstinf;
    }
}

// Set [lb, ub] to [-INF, -0], [-INF, +INF] or [+0, +INF] depending on
// signbit_known.
static void
zero_to_inf_range (REAL_VALUE_TYPE &lb, REAL_VALUE_TYPE &ub, int signbit_known)
{
  if (signbit_known > 0)
    {
      lb = dconst0;
      ub = dconstinf;
    }
  else if (signbit_known < 0)
    {
      lb = dconstninf;
      ub = dconstm0;
    }
  else
    {
      lb = dconstninf;
      ub = dconstinf;
    }
}

/* Extend the LHS range by 1ulp in each direction.  For op1_range
   or op2_range of binary operations just computing the inverse
   operation on ranges isn't sufficient.  Consider e.g.
   [1., 1.] = op1 + [1., 1.].  op1's range is not [0., 0.], but
   [-0x1.0p-54, 0x1.0p-53] (when not -frounding-math), any value for
   which adding 1. to it results in 1. after rounding to nearest.
   So, for op1_range/op2_range extend the lhs range by 1ulp (or 0.5ulp)
   in each direction.  See PR109008 for more details.  */

static frange
float_widen_lhs_range (tree type, const frange &lhs)
{
  frange ret = lhs;
  if (lhs.known_isnan ())
    return ret;
  REAL_VALUE_TYPE lb = lhs.lower_bound ();
  REAL_VALUE_TYPE ub = lhs.upper_bound ();
  if (real_isfinite (&lb))
    {
      frange_nextafter (TYPE_MODE (type), lb, dconstninf);
      if (real_isinf (&lb))
	{
	  /* For -DBL_MAX, instead of -Inf use
	     nexttoward (-DBL_MAX, -LDBL_MAX) in a hypothetical
	     wider type with the same mantissa precision but larger
	     exponent range; it is outside of range of double values,
	     but makes it clear it is just one ulp larger rather than
	     infinite amount larger.  */
	  lb = dconstm1;
	  SET_REAL_EXP (&lb, FLOAT_MODE_FORMAT (TYPE_MODE (type))->emax + 1);
	}
      if (!flag_rounding_math && !MODE_COMPOSITE_P (TYPE_MODE (type)))
	{
	  /* If not -frounding-math nor IBM double double, actually widen
	     just by 0.5ulp rather than 1ulp.  */
	  REAL_VALUE_TYPE tem;
	  real_arithmetic (&tem, PLUS_EXPR, &lhs.lower_bound (), &lb);
	  real_arithmetic (&lb, RDIV_EXPR, &tem, &dconst2);
	}
    }
  if (real_isfinite (&ub))
    {
      frange_nextafter (TYPE_MODE (type), ub, dconstinf);
      if (real_isinf (&ub))
	{
	  /* For DBL_MAX similarly.  */
	  ub = dconst1;
	  SET_REAL_EXP (&ub, FLOAT_MODE_FORMAT (TYPE_MODE (type))->emax + 1);
	}
      if (!flag_rounding_math && !MODE_COMPOSITE_P (TYPE_MODE (type)))
	{
	  /* If not -frounding-math nor IBM double double, actually widen
	     just by 0.5ulp rather than 1ulp.  */
	  REAL_VALUE_TYPE tem;
	  real_arithmetic (&tem, PLUS_EXPR, &lhs.upper_bound (), &ub);
	  real_arithmetic (&ub, RDIV_EXPR, &tem, &dconst2);
	}
    }
  /* Temporarily disable -ffinite-math-only, so that frange::set doesn't
     reduce the range back to real_min_representable (type) as lower bound
     or real_max_representable (type) as upper bound.  */
  bool save_flag_finite_math_only = flag_finite_math_only;
  flag_finite_math_only = false;
  ret.set (type, lb, ub, lhs.get_nan_state ());
  flag_finite_math_only = save_flag_finite_math_only;
  return ret;
}

bool
operator_plus::op1_range (frange &r, tree type, const frange &lhs,
			  const frange &op2, relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  range_op_handler minus (MINUS_EXPR);
  if (!minus)
    return false;
  frange wlhs = float_widen_lhs_range (type, lhs);
  return float_binary_op_range_finish (minus.fold_range (r, type, wlhs, op2),
				       r, type, wlhs);
}

bool
operator_plus::op2_range (frange &r, tree type,
			  const frange &lhs, const frange &op1,
			  relation_trio) const
{
  return op1_range (r, type, lhs, op1);
}

void
operator_plus::rv_fold (frange &r, tree type,
			const REAL_VALUE_TYPE &lh_lb,
			const REAL_VALUE_TYPE &lh_ub,
			const REAL_VALUE_TYPE &rh_lb,
			const REAL_VALUE_TYPE &rh_ub,
			relation_kind) const
{
  REAL_VALUE_TYPE lb, ub;
  bool maybe_nan = false;

  frange_arithmetic (PLUS_EXPR, type, lb, lh_lb, rh_lb, dconstninf);
  frange_arithmetic (PLUS_EXPR, type, ub, lh_ub, rh_ub, dconstinf);

  // [-INF] + [+INF] = NAN
  if (real_isinf (&lh_lb, true) && real_isinf (&rh_ub, false))
    maybe_nan = true;
  // [+INF] + [-INF] = NAN
  else if (real_isinf (&lh_ub, false) && real_isinf (&rh_lb, true))
    maybe_nan = true;

  // Handle possible NANs by saturating to the appropriate INF if only
  // one end is a NAN.  If both ends are a NAN, just return a NAN.
  bool lb_nan = real_isnan (&lb);
  bool ub_nan = real_isnan (&ub);
  if (lb_nan && ub_nan)
    {
      r.set_nan (type);
      return;
    }
  if (lb_nan)
    lb = dconstninf;
  else if (ub_nan)
    ub = dconstinf;
  r.set (type, lb, ub, nan_state (maybe_nan));
}


bool
operator_minus::op1_range (frange &r, tree type,
			   const frange &lhs, const frange &op2,
			   relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  frange wlhs = float_widen_lhs_range (type, lhs);
  return float_binary_op_range_finish (
	      range_op_handler (PLUS_EXPR).fold_range (r, type, wlhs, op2),
	      r, type, wlhs);
}

bool
operator_minus::op2_range (frange &r, tree type,
			   const frange &lhs, const frange &op1,
			   relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  frange wlhs = float_widen_lhs_range (type, lhs);
  return float_binary_op_range_finish (fold_range (r, type, op1, wlhs),
				       r, type, wlhs);
}

void
operator_minus::rv_fold (frange &r, tree type,
			 const REAL_VALUE_TYPE &lh_lb,
			 const REAL_VALUE_TYPE &lh_ub,
			 const REAL_VALUE_TYPE &rh_lb,
			 const REAL_VALUE_TYPE &rh_ub,
			 relation_kind) const
{
  REAL_VALUE_TYPE lb, ub;
  bool maybe_nan = false;

  frange_arithmetic (MINUS_EXPR, type, lb, lh_lb, rh_ub, dconstninf);
  frange_arithmetic (MINUS_EXPR, type, ub, lh_ub, rh_lb, dconstinf);

  // [+INF] - [+INF] = NAN
  if (real_isinf (&lh_ub, false) && real_isinf (&rh_ub, false))
    maybe_nan = true;
  // [-INF] - [-INF] = NAN
  else if (real_isinf (&lh_lb, true) && real_isinf (&rh_lb, true))
    maybe_nan = true;

  // Handle possible NANs by saturating to the appropriate INF if only
  // one end is a NAN.  If both ends are a NAN, just return a NAN.
  bool lb_nan = real_isnan (&lb);
  bool ub_nan = real_isnan (&ub);
  if (lb_nan && ub_nan)
    {
      r.set_nan (type);
      return;
    }
  if (lb_nan)
    lb = dconstninf;
  else if (ub_nan)
    ub = dconstinf;
  r.set (type, lb, ub, nan_state (maybe_nan));
}


// Given CP[0] to CP[3] floating point values rounded to -INF,
// set LB to the smallest of them (treating -0 as smaller to +0).
// Given CP[4] to CP[7] floating point values rounded to +INF,
// set UB to the largest of them (treating -0 as smaller to +0).

static void
find_range (REAL_VALUE_TYPE &lb, REAL_VALUE_TYPE &ub,
	    const REAL_VALUE_TYPE (&cp)[8])
{
  lb = cp[0];
  ub = cp[4];
  for (int i = 1; i < 4; ++i)
    {
      if (real_less (&cp[i], &lb)
	  || (real_iszero (&lb) && real_isnegzero (&cp[i])))
	lb = cp[i];
      if (real_less (&ub, &cp[i + 4])
	  || (real_isnegzero (&ub) && real_iszero (&cp[i + 4])))
	ub = cp[i + 4];
    }
}


bool
operator_mult::op1_range (frange &r, tree type,
			  const frange &lhs, const frange &op2,
			  relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  range_op_handler rdiv (RDIV_EXPR);
  if (!rdiv)
    return false;
  frange wlhs = float_widen_lhs_range (type, lhs);
  bool ret = rdiv.fold_range (r, type, wlhs, op2);
  if (ret == false)
    return false;
  if (wlhs.known_isnan () || op2.known_isnan () || op2.undefined_p ())
    return float_binary_op_range_finish (ret, r, type, wlhs);
  const REAL_VALUE_TYPE &lhs_lb = wlhs.lower_bound ();
  const REAL_VALUE_TYPE &lhs_ub = wlhs.upper_bound ();
  const REAL_VALUE_TYPE &op2_lb = op2.lower_bound ();
  const REAL_VALUE_TYPE &op2_ub = op2.upper_bound ();
  if ((contains_zero_p (lhs_lb, lhs_ub) && contains_zero_p (op2_lb, op2_ub))
      || ((real_isinf (&lhs_lb) || real_isinf (&lhs_ub))
	  && (real_isinf (&op2_lb) || real_isinf (&op2_ub))))
    {
      // If both lhs and op2 could be zeros or both could be infinities,
      // we don't know anything about op1 except maybe for the sign
      // and perhaps if it can be NAN or not.
      REAL_VALUE_TYPE lb, ub;
      int signbit_known = signbit_known_p (lhs_lb, lhs_ub, op2_lb, op2_ub);
      zero_to_inf_range (lb, ub, signbit_known);
      r.set (type, lb, ub);
    }
  // Otherwise, if op2 is a singleton INF and lhs doesn't include INF,
  // or if lhs must be zero and op2 doesn't include zero, it would be
  // UNDEFINED, while rdiv.fold_range computes a zero or singleton INF
  // range.  Those are supersets of UNDEFINED, so let's keep that way.
  return float_binary_op_range_finish (ret, r, type, wlhs);
}

bool
operator_mult::op2_range (frange &r, tree type,
			  const frange &lhs, const frange &op1,
			  relation_trio) const
{
  return op1_range (r, type, lhs, op1);
}

void
operator_mult::rv_fold (frange &r, tree type,
			const REAL_VALUE_TYPE &lh_lb,
			const REAL_VALUE_TYPE &lh_ub,
			const REAL_VALUE_TYPE &rh_lb,
			const REAL_VALUE_TYPE &rh_ub,
			relation_kind kind) const
{
  bool is_square
    = (kind == VREL_EQ
       && real_equal (&lh_lb, &rh_lb)
       && real_equal (&lh_ub, &rh_ub)
       && real_isneg (&lh_lb) == real_isneg (&rh_lb)
       && real_isneg (&lh_ub) == real_isneg (&rh_ub));
  REAL_VALUE_TYPE lb, ub;
  bool maybe_nan = false;
  // x * x never produces a new NAN and we only multiply the same
  // values, so the 0 * INF problematic cases never appear there.
  if (!is_square)
    {
      // [+-0, +-0] * [+INF,+INF] (or [-INF,-INF] or swapped is a known NAN.
      if ((zero_p (lh_lb, lh_ub) && singleton_inf_p (rh_lb, rh_ub))
	  || (zero_p (rh_lb, rh_ub) && singleton_inf_p (lh_lb, lh_ub)))
	{
	  r.set_nan (type);
	  return;
	}

      // Otherwise, if one range includes zero and the other ends with +-INF,
      // it is a maybe NAN.
      if ((contains_zero_p (lh_lb, lh_ub)
	   && (real_isinf (&rh_lb) || real_isinf (&rh_ub)))
	  || (contains_zero_p (rh_lb, rh_ub)
	      && (real_isinf (&lh_lb) || real_isinf (&lh_ub))))
	{
	  maybe_nan = true;

	  int signbit_known = signbit_known_p (lh_lb, lh_ub, rh_lb, rh_ub);

	  // If one of the ranges that includes INF is singleton
	  // and the other range includes zero, the resulting
	  // range is INF and NAN, because the 0 * INF boundary
	  // case will be NAN, but already nextafter (0, 1) * INF
	  // is INF.
	  if (singleton_inf_p (lh_lb, lh_ub)
	      || singleton_inf_p (rh_lb, rh_ub))
	    {
	      inf_range (lb, ub, signbit_known);
	      r.set (type, lb, ub, nan_state (true));
	      return;
	    }

	  // If one of the multiplicands must be zero, the resulting
	  // range is +-0 and NAN.
	  if (zero_p (lh_lb, lh_ub) || zero_p (rh_lb, rh_ub))
	    {
	      zero_range (lb, ub, signbit_known);
	      r.set (type, lb, ub, nan_state (true));
	      return;
	    }

	  // Otherwise one of the multiplicands could be
	  // [0.0, nextafter (0.0, 1.0)] and the [DBL_MAX, INF]
	  // or similarly with different signs.  0.0 * DBL_MAX
	  // is still 0.0, nextafter (0.0, 1.0) * INF is still INF,
	  // so if the signs are always the same or always different,
	  // result is [+0.0, +INF] or [-INF, -0.0], otherwise VARYING.
	  zero_to_inf_range (lb, ub, signbit_known);
	  r.set (type, lb, ub, nan_state (true));
	  return;
	}
    }

  REAL_VALUE_TYPE cp[8];
  // Do a cross-product.  At this point none of the multiplications
  // should produce a NAN.
  frange_arithmetic (MULT_EXPR, type, cp[0], lh_lb, rh_lb, dconstninf);
  frange_arithmetic (MULT_EXPR, type, cp[4], lh_lb, rh_lb, dconstinf);
  if (is_square)
    {
      // For x * x we can just do max (lh_lb * lh_lb, lh_ub * lh_ub)
      // as maximum and -0.0 as minimum if 0.0 is in the range,
      // otherwise min (lh_lb * lh_lb, lh_ub * lh_ub).
      // -0.0 rather than 0.0 because VREL_EQ doesn't prove that
      // x and y are bitwise equal, just that they compare equal.
      if (contains_zero_p (lh_lb, lh_ub))
	{
	  if (real_isneg (&lh_lb) == real_isneg (&lh_ub))
	    cp[1] = dconst0;
	  else
	    cp[1] = dconstm0;
	}
      else
	cp[1] = cp[0];
      cp[2] = cp[0];
      cp[5] = cp[4];
      cp[6] = cp[4];
    }
  else
    {
      frange_arithmetic (MULT_EXPR, type, cp[1], lh_lb, rh_ub, dconstninf);
      frange_arithmetic (MULT_EXPR, type, cp[5], lh_lb, rh_ub, dconstinf);
      frange_arithmetic (MULT_EXPR, type, cp[2], lh_ub, rh_lb, dconstninf);
      frange_arithmetic (MULT_EXPR, type, cp[6], lh_ub, rh_lb, dconstinf);
    }
  frange_arithmetic (MULT_EXPR, type, cp[3], lh_ub, rh_ub, dconstninf);
  frange_arithmetic (MULT_EXPR, type, cp[7], lh_ub, rh_ub, dconstinf);

  find_range (lb, ub, cp);

  gcc_checking_assert (!real_isnan (&lb));
  gcc_checking_assert (!real_isnan (&ub));
  r.set (type, lb, ub, nan_state (maybe_nan));
}


class foperator_div : public range_operator
{
  using range_operator::op1_range;
  using range_operator::op2_range;
public:
  virtual bool op1_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op2,
			  relation_trio = TRIO_VARYING) const final override
  {
    if (lhs.undefined_p ())
      return false;
    frange wlhs = float_widen_lhs_range (type, lhs);
    bool ret = range_op_handler (MULT_EXPR).fold_range (r, type, wlhs, op2);
    if (!ret)
      return ret;
    if (wlhs.known_isnan () || op2.known_isnan () || op2.undefined_p ())
      return float_binary_op_range_finish (ret, r, type, wlhs);
    const REAL_VALUE_TYPE &lhs_lb = wlhs.lower_bound ();
    const REAL_VALUE_TYPE &lhs_ub = wlhs.upper_bound ();
    const REAL_VALUE_TYPE &op2_lb = op2.lower_bound ();
    const REAL_VALUE_TYPE &op2_ub = op2.upper_bound ();
    if ((contains_zero_p (lhs_lb, lhs_ub)
	 && (real_isinf (&op2_lb) || real_isinf (&op2_ub)))
	|| ((contains_zero_p (op2_lb, op2_ub))
	    && (real_isinf (&lhs_lb) || real_isinf (&lhs_ub))))
      {
	// If both lhs could be zero and op2 infinity or vice versa,
	// we don't know anything about op1 except maybe for the sign
	// and perhaps if it can be NAN or not.
	REAL_VALUE_TYPE lb, ub;
	int signbit_known = signbit_known_p (lhs_lb, lhs_ub, op2_lb, op2_ub);
	zero_to_inf_range (lb, ub, signbit_known);
	r.set (type, lb, ub);
      }
    return float_binary_op_range_finish (ret, r, type, wlhs);
  }
  virtual bool op2_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op1,
			  relation_trio = TRIO_VARYING) const final override
  {
    if (lhs.undefined_p ())
      return false;
    frange wlhs = float_widen_lhs_range (type, lhs);
    bool ret = fold_range (r, type, op1, wlhs);
    if (!ret)
      return ret;
    if (wlhs.known_isnan () || op1.known_isnan () || op1.undefined_p ())
      return float_binary_op_range_finish (ret, r, type, wlhs, true);
    const REAL_VALUE_TYPE &lhs_lb = wlhs.lower_bound ();
    const REAL_VALUE_TYPE &lhs_ub = wlhs.upper_bound ();
    const REAL_VALUE_TYPE &op1_lb = op1.lower_bound ();
    const REAL_VALUE_TYPE &op1_ub = op1.upper_bound ();
    if ((contains_zero_p (lhs_lb, lhs_ub) && contains_zero_p (op1_lb, op1_ub))
	|| ((real_isinf (&lhs_lb) || real_isinf (&lhs_ub))
	    && (real_isinf (&op1_lb) || real_isinf (&op1_ub))))
      {
	// If both lhs and op1 could be zeros or both could be infinities,
	// we don't know anything about op2 except maybe for the sign
	// and perhaps if it can be NAN or not.
	REAL_VALUE_TYPE lb, ub;
	int signbit_known = signbit_known_p (lhs_lb, lhs_ub, op1_lb, op1_ub);
	zero_to_inf_range (lb, ub, signbit_known);
	r.set (type, lb, ub);
      }
    return float_binary_op_range_finish (ret, r, type, wlhs, true);
  }
private:
  void rv_fold (frange &r, tree type,
		const REAL_VALUE_TYPE &lh_lb,
		const REAL_VALUE_TYPE &lh_ub,
		const REAL_VALUE_TYPE &rh_lb,
		const REAL_VALUE_TYPE &rh_ub,
		relation_kind) const final override
  {
    // +-0.0 / +-0.0 or +-INF / +-INF is a known NAN.
    if ((zero_p (lh_lb, lh_ub) && zero_p (rh_lb, rh_ub))
	|| (singleton_inf_p (lh_lb, lh_ub) && singleton_inf_p (rh_lb, rh_ub)))
      {
	r.set_nan (type);
	return;
      }

    REAL_VALUE_TYPE lb, ub;
    bool maybe_nan = false;
    // If +-0.0 is in both ranges, it is a maybe NAN.
    if (contains_zero_p (lh_lb, lh_ub) && contains_zero_p (rh_lb, rh_ub))
      maybe_nan = true;
    // If +-INF is in both ranges, it is a maybe NAN.
    else if ((real_isinf (&lh_lb) || real_isinf (&lh_ub))
	     && (real_isinf (&rh_lb) || real_isinf (&rh_ub)))
      maybe_nan = true;

    int signbit_known = signbit_known_p (lh_lb, lh_ub, rh_lb, rh_ub);

    // If dividend must be zero, the range is just +-0
    // (including if the divisor is +-INF).
    // If divisor must be +-INF, the range is just +-0
    // (including if the dividend is zero).
    if (zero_p (lh_lb, lh_ub) || singleton_inf_p (rh_lb, rh_ub))
      {
	zero_range (lb, ub, signbit_known);
	r.set (type, lb, ub, nan_state (maybe_nan));
	return;
      }

    // If divisor must be zero, the range is just +-INF
    // (including if the dividend is +-INF).
    // If dividend must be +-INF, the range is just +-INF
    // (including if the dividend is zero).
    if (zero_p (rh_lb, rh_ub) || singleton_inf_p (lh_lb, lh_ub))
      {
	inf_range (lb, ub, signbit_known);
	r.set (type, lb, ub, nan_state (maybe_nan));
	return;
      }

    // Otherwise if both operands may be zero, divisor could be
    // nextafter(0.0, +-1.0) and dividend +-0.0
    // in which case result is going to INF or vice versa and
    // result +0.0.  So, all we can say for that case is if the
    // signs of divisor and dividend are always the same we have
    // [+0.0, +INF], if they are always different we have
    // [-INF, -0.0].  If they vary, VARYING.
    // If both may be +-INF, divisor could be INF and dividend FLT_MAX,
    // in which case result is going to INF or vice versa and
    // result +0.0.  So, all we can say for that case is if the
    // signs of divisor and dividend are always the same we have
    // [+0.0, +INF], if they are always different we have
    // [-INF, -0.0].  If they vary, VARYING.
    if (maybe_nan)
      {
	zero_to_inf_range (lb, ub, signbit_known);
	r.set (type, lb, ub, nan_state (maybe_nan));
	return;
      }

    REAL_VALUE_TYPE cp[8];
    // Do a cross-division.  At this point none of the divisions should
    // produce a NAN.
    frange_arithmetic (RDIV_EXPR, type, cp[0], lh_lb, rh_lb, dconstninf);
    frange_arithmetic (RDIV_EXPR, type, cp[1], lh_lb, rh_ub, dconstninf);
    frange_arithmetic (RDIV_EXPR, type, cp[2], lh_ub, rh_lb, dconstninf);
    frange_arithmetic (RDIV_EXPR, type, cp[3], lh_ub, rh_ub, dconstninf);
    frange_arithmetic (RDIV_EXPR, type, cp[4], lh_lb, rh_lb, dconstinf);
    frange_arithmetic (RDIV_EXPR, type, cp[5], lh_lb, rh_ub, dconstinf);
    frange_arithmetic (RDIV_EXPR, type, cp[6], lh_ub, rh_lb, dconstinf);
    frange_arithmetic (RDIV_EXPR, type, cp[7], lh_ub, rh_ub, dconstinf);

    find_range (lb, ub, cp);

    // If divisor may be zero (but is not known to be only zero),
    // and dividend can't be zero, the range can go up to -INF or +INF
    // depending on the signs.
    if (contains_zero_p (rh_lb, rh_ub))
      {
	if (signbit_known <= 0)
	  real_inf (&lb, true);
	if (signbit_known >= 0)
	  real_inf (&ub, false);
      }

    gcc_checking_assert (!real_isnan (&lb));
    gcc_checking_assert (!real_isnan (&ub));
    r.set (type, lb, ub, nan_state (maybe_nan));
  }
} fop_div;


// Initialize any float operators to the primary table

void
range_op_table::initialize_float_ops ()
{
  set (UNLE_EXPR, fop_unordered_le);
  set (UNLT_EXPR, fop_unordered_lt);
  set (UNGE_EXPR, fop_unordered_ge);
  set (UNGT_EXPR, fop_unordered_gt);
  set (UNEQ_EXPR, fop_unordered_equal);
  set (ORDERED_EXPR, fop_ordered);
  set (UNORDERED_EXPR, fop_unordered);
  set (LTGT_EXPR, fop_ltgt);
  set (RDIV_EXPR, fop_div);
}

#if CHECKING_P
#include "selftest.h"

namespace selftest
{

// Build an frange from string endpoints.

static inline frange
frange_float (const char *lb, const char *ub, tree type = float_type_node)
{
  REAL_VALUE_TYPE min, max;
  gcc_assert (real_from_string (&min, lb) == 0);
  gcc_assert (real_from_string (&max, ub) == 0);
  return frange (type, min, max);
}

void
range_op_float_tests ()
{
  frange r, r0, r1;
  frange trange (float_type_node);

  // negate([-5, +10]) => [-10, 5]
  r0 = frange_float ("-5", "10");
  range_op_handler (NEGATE_EXPR).fold_range (r, float_type_node, r0, trange);
  ASSERT_EQ (r, frange_float ("-10", "5"));

  // negate([0, 1] -NAN) => [-1, -0] +NAN
  r0 = frange_float ("0", "1");
  r0.update_nan (true);
  range_op_handler (NEGATE_EXPR).fold_range (r, float_type_node, r0, trange);
  r1 = frange_float ("-1", "-0");
  r1.update_nan (false);
  ASSERT_EQ (r, r1);

  // [-INF,+INF] + [-INF,+INF] could be a NAN.
  range_op_handler plus (PLUS_EXPR);
  r0.set_varying (float_type_node);
  r1.set_varying (float_type_node);
  r0.clear_nan ();
  r1.clear_nan ();
  plus.fold_range (r, float_type_node, r0, r1);
  if (HONOR_NANS (float_type_node))
    ASSERT_TRUE (r.maybe_isnan ());
}

} // namespace selftest

#endif // CHECKING_P
