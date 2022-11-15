/* Floating point range operators.
   Copyright (C) 2022 Free Software Foundation, Inc.
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

// Default definitions for floating point operators.

bool
range_operator_float::fold_range (frange &r, tree type,
				  const frange &op1, const frange &op2,
				  relation_trio trio) const
{
  if (empty_range_varying (r, type, op1, op2))
    return true;
  if (op1.known_isnan () || op2.known_isnan ())
    {
      r.set_nan (op1.type ());
      return true;
    }

  REAL_VALUE_TYPE lb, ub;
  bool maybe_nan;
  rv_fold (lb, ub, maybe_nan, type,
	   op1.lower_bound (), op1.upper_bound (),
	   op2.lower_bound (), op2.upper_bound (), trio.op1_op2 ());

  // Handle possible NANs by saturating to the appropriate INF if only
  // one end is a NAN.  If both ends are a NAN, just return a NAN.
  bool lb_nan = real_isnan (&lb);
  bool ub_nan = real_isnan (&ub);
  if (lb_nan && ub_nan)
    {
      r.set_nan (type);
      return true;
    }
  if (lb_nan)
    lb = dconstninf;
  else if (ub_nan)
    ub = dconstinf;

  r.set (type, lb, ub);

  if (lb_nan || ub_nan || maybe_nan)
    // Keep the default NAN (with a varying sign) set by the setter.
    ;
  else if (!op1.maybe_isnan () && !op2.maybe_isnan ())
    r.clear_nan ();

  return true;
}

// For a given operation, fold two sets of ranges into [lb, ub].
// MAYBE_NAN is set to TRUE if, in addition to any result in LB or
// UB, the final range has the possiblity of a NAN.
void
range_operator_float::rv_fold (REAL_VALUE_TYPE &lb,
			       REAL_VALUE_TYPE &ub,
			       bool &maybe_nan,
			       tree type ATTRIBUTE_UNUSED,
			       const REAL_VALUE_TYPE &lh_lb ATTRIBUTE_UNUSED,
			       const REAL_VALUE_TYPE &lh_ub ATTRIBUTE_UNUSED,
			       const REAL_VALUE_TYPE &rh_lb ATTRIBUTE_UNUSED,
			       const REAL_VALUE_TYPE &rh_ub ATTRIBUTE_UNUSED,
			       relation_kind) const
{
  lb = dconstninf;
  ub = dconstinf;
  maybe_nan = true;
}

bool
range_operator_float::fold_range (irange &r ATTRIBUTE_UNUSED,
				  tree type ATTRIBUTE_UNUSED,
				  const frange &lh ATTRIBUTE_UNUSED,
				  const irange &rh ATTRIBUTE_UNUSED,
				  relation_trio) const
{
  return false;
}

bool
range_operator_float::fold_range (irange &r ATTRIBUTE_UNUSED,
				  tree type ATTRIBUTE_UNUSED,
				  const frange &lh ATTRIBUTE_UNUSED,
				  const frange &rh ATTRIBUTE_UNUSED,
				  relation_trio) const
{
  return false;
}

bool
range_operator_float::op1_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const frange &lhs ATTRIBUTE_UNUSED,
				 const frange &op2 ATTRIBUTE_UNUSED,
				 relation_trio) const
{
  return false;
}

bool
range_operator_float::op1_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const irange &lhs ATTRIBUTE_UNUSED,
				 const frange &op2 ATTRIBUTE_UNUSED,
				 relation_trio) const
{
  return false;
}

bool
range_operator_float::op2_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const frange &lhs ATTRIBUTE_UNUSED,
				 const frange &op1 ATTRIBUTE_UNUSED,
				 relation_trio) const
{
  return false;
}

bool
range_operator_float::op2_range (frange &r ATTRIBUTE_UNUSED,
				 tree type ATTRIBUTE_UNUSED,
				 const irange &lhs ATTRIBUTE_UNUSED,
				 const frange &op1 ATTRIBUTE_UNUSED,
				 relation_trio) const
{
  return false;
}

relation_kind
range_operator_float::lhs_op1_relation (const frange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator_float::lhs_op1_relation (const irange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator_float::lhs_op2_relation (const irange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator_float::lhs_op2_relation (const frange &lhs ATTRIBUTE_UNUSED,
					const frange &op1 ATTRIBUTE_UNUSED,
					const frange &op2 ATTRIBUTE_UNUSED,
					relation_kind) const
{
  return VREL_VARYING;
}

relation_kind
range_operator_float::op1_op2_relation (const irange &lhs ATTRIBUTE_UNUSED) const
{
  return VREL_VARYING;
}

relation_kind
range_operator_float::op1_op2_relation (const frange &lhs ATTRIBUTE_UNUSED) const
{
  return VREL_VARYING;
}

// Return TRUE if OP1 and OP2 may be a NAN.

static inline bool
maybe_isnan (const frange &op1, const frange &op2)
{
  return op1.maybe_isnan () || op2.maybe_isnan ();
}

// Floating version of relop_early_resolve that takes into account NAN
// and -ffinite-math-only.

inline bool
frelop_early_resolve (irange &r, tree type,
		      const frange &op1, const frange &op2,
		      relation_trio rel, relation_kind my_rel)
{
  // If either operand is undefined, return VARYING.
  if (empty_range_varying (r, type, op1, op2))
    return true;

  // We can fold relations from the oracle when we know both operands
  // are free of NANs, or when -ffinite-math-only.
  return (!maybe_isnan (op1, op2)
	  && relop_early_resolve (r, type, op1, op2, rel, my_rel));
}

// Set VALUE to its next real value, or INF if the operation overflows.

inline void
frange_nextafter (enum machine_mode mode,
		  REAL_VALUE_TYPE &value,
		  const REAL_VALUE_TYPE &inf)
{
  const real_format *fmt = REAL_MODE_FORMAT (mode);
  REAL_VALUE_TYPE tmp;
  real_nextafter (&tmp, fmt, &value, &inf);
  value = tmp;
}

// Like real_arithmetic, but round the result to INF if the operation
// produced inexact results.
//
// ?? There is still one problematic case, i387.  With
// -fexcess-precision=standard we perform most SF/DFmode arithmetic in
// XFmode (long_double_type_node), so that case is OK.  But without
// -mfpmath=sse, all the SF/DFmode computations are in XFmode
// precision (64-bit mantissa) and only occassionally rounded to
// SF/DFmode (when storing into memory from the 387 stack).  Maybe
// this is ok as well though it is just occassionally more precise. ??

static void
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

  // Be extra careful if there may be discrepancies between the
  // compile and runtime results.
  if ((mode_composite || (real_isneg (&inf) ? real_less (&result, &value)
			  : !real_less (&value, &result)))
      && (inexact || !real_identical (&result, &value)))
    {
      if (mode_composite)
	{
	  if (real_isdenormal (&result, mode)
	      || real_iszero (&result))
	    {
	      // IBM extended denormals only have DFmode precision.
	      REAL_VALUE_TYPE tmp;
	      real_convert (&tmp, DFmode, &value);
	      frange_nextafter (DFmode, tmp, inf);
	      real_convert (&result, mode, &tmp);
	      return;
	    }
	}
      frange_nextafter (mode, result, inf);
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


class foperator_identity : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
public:
  bool fold_range (frange &r, tree type ATTRIBUTE_UNUSED,
		   const frange &op1, const frange &op2 ATTRIBUTE_UNUSED,
		   relation_trio = TRIO_VARYING) const final override
  {
    r = op1;
    return true;
  }
  bool op1_range (frange &r, tree type ATTRIBUTE_UNUSED,
		  const frange &lhs, const frange &op2 ATTRIBUTE_UNUSED,
		  relation_trio = TRIO_VARYING) const final override
  {
    r = lhs;
    return true;
  }
public:
} fop_identity;

class foperator_equal : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
  using range_operator_float::op1_op2_relation;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return equal_op1_op2_relation (lhs);
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
} fop_equal;

bool
foperator_equal::fold_range (irange &r, tree type,
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
      else
	r = range_false (type);
    }
  else if (!maybe_isnan (op1, op2))
    {
      // If ranges do not intersect, we know the range is not equal,
      // otherwise we don't know anything for sure.
      frange tmp = op1;
      tmp.intersect (op2);
      if (tmp.undefined_p ())
	r = range_false (type);
      else
	r = range_true_and_false (type);
    }
  else
    r = range_true_and_false (type);
  return true;
}

bool
foperator_equal::op1_range (frange &r, tree type,
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

class foperator_not_equal : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op1_op2_relation;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return not_equal_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
} fop_not_equal;

bool
foperator_not_equal::fold_range (irange &r, tree type,
				 const frange &op1, const frange &op2,
				 relation_trio rel) const
{
  if (frelop_early_resolve (r, type, op1, op2, rel, VREL_NE))
    return true;

  // x != NAN is always TRUE.
  if (op1.known_isnan () || op2.known_isnan ())
    r = range_true (type);
  // We can be sure the values are always equal or not if both ranges
  // consist of a single value, and then compare them.
  else if (op1.singleton_p () && op2.singleton_p ())
    {
      if (op1 != op2)
	r = range_true (type);
      else
	r = range_false (type);
    }
  else if (!maybe_isnan (op1, op2))
    {
      // If ranges do not intersect, we know the range is not equal,
      // otherwise we don't know anything for sure.
      frange tmp = op1;
      tmp.intersect (op2);
      if (tmp.undefined_p ())
	r = range_true (type);
      else
	r = range_true_and_false (type);
    }
  else
    r = range_true_and_false (type);
  return true;
}

bool
foperator_not_equal::op1_range (frange &r, tree type,
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

class foperator_lt : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
  using range_operator_float::op1_op2_relation;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return lt_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;
} fop_lt;

bool
foperator_lt::fold_range (irange &r, tree type,
			  const frange &op1, const frange &op2,
			  relation_trio rel) const
{
  if (frelop_early_resolve (r, type, op1, op2, rel, VREL_LT))
    return true;

  if (op1.known_isnan () || op2.known_isnan ())
    r = range_false (type);
  else if (!maybe_isnan (op1, op2))
    {
      if (real_less (&op1.upper_bound (), &op2.lower_bound ()))
	r = range_true (type);
      else if (!real_less (&op1.lower_bound (), &op2.upper_bound ()))
	r = range_false (type);
      else
	r = range_true_and_false (type);
    }
  else
    r = range_true_and_false (type);
  return true;
}

bool
foperator_lt::op1_range (frange &r,
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
      else if (build_lt (r, type, op2))
	{
	  r.clear_nan ();
	  // x < y implies x is not +INF.
	  frange_drop_inf (r, type);
	}
      break;

    case BRS_FALSE:
      // On the FALSE side of x < NAN, we know nothing about x.
      if (op2.known_isnan ())
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
foperator_lt::op2_range (frange &r,
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
      else if (build_gt (r, type, op1))
	{
	  r.clear_nan ();
	  // x < y implies y is not -INF.
	  frange_drop_ninf (r, type);
	}
      break;

    case BRS_FALSE:
      // On the FALSE side of NAN < x, we know nothing about x.
      if (op1.known_isnan ())
	r.set_varying (type);
      else
	build_le (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

class foperator_le : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
  using range_operator_float::op1_op2_relation;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio rel = TRIO_VARYING) const final override;
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return le_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio rel = TRIO_VARYING) const final override;
} fop_le;

bool
foperator_le::fold_range (irange &r, tree type,
			  const frange &op1, const frange &op2,
			  relation_trio rel) const
{
  if (frelop_early_resolve (r, type, op1, op2, rel, VREL_LE))
    return true;

  if (op1.known_isnan () || op2.known_isnan ())
    r = range_false (type);
  else if (!maybe_isnan (op1, op2))
    {
      if (real_compare (LE_EXPR, &op1.upper_bound (), &op2.lower_bound ()))
	r = range_true (type);
      else if (!real_compare (LE_EXPR, &op1.lower_bound (), &op2.upper_bound ()))
	r = range_false (type);
      else
	r = range_true_and_false (type);
    }
  else
    r = range_true_and_false (type);
  return true;
}

bool
foperator_le::op1_range (frange &r,
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
      else if (build_le (r, type, op2))
	r.clear_nan ();
      break;

    case BRS_FALSE:
      // On the FALSE side of x <= NAN, we know nothing about x.
      if (op2.known_isnan ())
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
foperator_le::op2_range (frange &r,
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
      else if (build_ge (r, type, op1))
	r.clear_nan ();
      break;

    case BRS_FALSE:
      // On the FALSE side of NAN <= x, we know nothing about x.
      if (op1.known_isnan ())
	r.set_varying (type);
      else
	build_lt (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

class foperator_gt : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
  using range_operator_float::op1_op2_relation;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return gt_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;
} fop_gt;

bool
foperator_gt::fold_range (irange &r, tree type,
			  const frange &op1, const frange &op2,
			  relation_trio rel) const
{
  if (frelop_early_resolve (r, type, op1, op2, rel, VREL_GT))
    return true;

  if (op1.known_isnan () || op2.known_isnan ())
    r = range_false (type);
  else if (!maybe_isnan (op1, op2))
    {
      if (real_compare (GT_EXPR, &op1.lower_bound (), &op2.upper_bound ()))
	r = range_true (type);
      else if (!real_compare (GT_EXPR, &op1.upper_bound (), &op2.lower_bound ()))
	r = range_false (type);
      else
	r = range_true_and_false (type);
    }
  else
    r = range_true_and_false (type);
  return true;
}

bool
foperator_gt::op1_range (frange &r,
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
      else if (build_gt (r, type, op2))
	{
	  r.clear_nan ();
	  // x > y implies x is not -INF.
	  frange_drop_ninf (r, type);
	}
      break;

    case BRS_FALSE:
      // On the FALSE side of x > NAN, we know nothing about x.
      if (op2.known_isnan ())
	r.set_varying (type);
      else
	build_le (r, type, op2);
      break;

    default:
      break;
    }
  return true;
}

bool
foperator_gt::op2_range (frange &r,
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
      else if (build_lt (r, type, op1))
	{
	  r.clear_nan ();
	  // x > y implies y is not +INF.
	  frange_drop_inf (r, type);
	}
      break;

    case BRS_FALSE:
      // On The FALSE side of NAN > x, we know nothing about x.
      if (op1.known_isnan ())
	r.set_varying (type);
      else
	build_ge (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

class foperator_ge : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
  using range_operator_float::op1_op2_relation;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override;
  relation_kind op1_op2_relation (const irange &lhs) const final override
  {
    return ge_op1_op2_relation (lhs);
  }
  bool op1_range (frange &r, tree type,
		  const irange &lhs, const frange &op2,
		  relation_trio = TRIO_VARYING) const final override;
  bool op2_range (frange &r, tree type,
		  const irange &lhs, const frange &op1,
		  relation_trio = TRIO_VARYING) const final override;
} fop_ge;

bool
foperator_ge::fold_range (irange &r, tree type,
			  const frange &op1, const frange &op2,
			  relation_trio rel) const
{
  if (frelop_early_resolve (r, type, op1, op2, rel, VREL_GE))
    return true;

  if (op1.known_isnan () || op2.known_isnan ())
    r = range_false (type);
  else if (!maybe_isnan (op1, op2))
    {
      if (real_compare (GE_EXPR, &op1.lower_bound (), &op2.upper_bound ()))
	r = range_true (type);
      else if (!real_compare (GE_EXPR, &op1.upper_bound (), &op2.lower_bound ()))
	r = range_false (type);
      else
	r = range_true_and_false (type);
    }
  else
    r = range_true_and_false (type);
  return true;
}

bool
foperator_ge::op1_range (frange &r,
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
      else if (build_ge (r, type, op2))
	r.clear_nan ();
      break;

    case BRS_FALSE:
      // On the FALSE side of x >= NAN, we know nothing about x.
      if (op2.known_isnan ())
	r.set_varying (type);
      else
	build_lt (r, type, op2);
      break;

    default:
      break;
    }
  return true;
}

bool
foperator_ge::op2_range (frange &r, tree type,
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
      else if (build_le (r, type, op1))
	r.clear_nan ();
      break;

    case BRS_FALSE:
      // On the FALSE side of NAN >= x, we know nothing about x.
      if (op1.known_isnan ())
	r.set_varying (type);
      else
	build_gt (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

// UNORDERED_EXPR comparison.

class foperator_unordered : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
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

class foperator_ordered : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
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

class foperator_negate : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
public:
  bool fold_range (frange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio = TRIO_VARYING) const final override
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
  bool op1_range (frange &r, tree type,
		  const frange &lhs, const frange &op2,
		  relation_trio rel = TRIO_VARYING) const final override
  {
    return fold_range (r, type, lhs, op2, rel);
  }
} fop_negate;

class foperator_abs : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
public:
  bool fold_range (frange &r, tree type,
		   const frange &op1, const frange &,
		   relation_trio = TRIO_VARYING) const final override;
  bool op1_range (frange &r, tree type,
		  const frange &lhs, const frange &op2,
		  relation_trio rel = TRIO_VARYING) const final override;
} fop_abs;

bool
foperator_abs::fold_range (frange &r, tree type,
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
foperator_abs::op1_range (frange &r, tree type,
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
  if (r.known_isnan ())
    return true;
  // Then add the negative of each pair:
  // ABS(op1) = [5,20] would yield op1 => [-20,-5][5,20].
  frange negatives (type, real_value_negate (&positives.upper_bound ()),
		    real_value_negate (&positives.lower_bound ()));
  negatives.clear_nan ();
  r.union_ (negatives);
  return true;
}

class foperator_unordered_lt : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio rel = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_true (type);
	return true;
      }
    if (!fop_lt.fold_range (r, type, op1, op2, rel))
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
      if (op2.known_isnan ())
	r.set_varying (type);
      else
	build_lt (r, type, op2);
      break;

    case BRS_FALSE:
      // A false UNORDERED_LT means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
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
      if (op1.known_isnan ())
	r.set_varying (type);
      else
	build_gt (r, type, op1);
      break;

    case BRS_FALSE:
      // A false UNORDERED_LT means both operands are !NAN, so it's
      // impossible for op1 to be a NAN.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (build_le (r, type, op1))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

class foperator_unordered_le : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio rel = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_true (type);
	return true;
      }
    if (!fop_le.fold_range (r, type, op1, op2, rel))
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
      if (op2.known_isnan ())
	r.set_varying (type);
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
      if (op1.known_isnan ())
	r.set_varying (type);
      else
	build_ge (r, type, op1);
      break;

    case BRS_FALSE:
      // A false UNORDERED_LE means both operands are !NAN, so it's
      // impossible for op1 to be a NAN.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (build_lt (r, type, op1))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

class foperator_unordered_gt : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio rel = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_true (type);
	return true;
      }
    if (!fop_gt.fold_range (r, type, op1, op2, rel))
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
      if (op2.known_isnan ())
	r.set_varying (type);
      else
	build_gt (r, type, op2);
      break;

    case BRS_FALSE:
      // A false UNORDERED_GT means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
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
      if (op1.known_isnan ())
	r.set_varying (type);
      else
	build_lt (r, type, op1);
      break;

    case BRS_FALSE:
      // A false UNORDERED_GT means both operands are !NAN, so it's
      // impossible for op1 to be a NAN.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (build_ge (r, type, op1))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

class foperator_unordered_ge : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio rel = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_true (type);
	return true;
      }
    if (!fop_ge.fold_range (r, type, op1, op2, rel))
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
      if (op2.known_isnan ())
	r.set_varying (type);
      else
	build_ge (r, type, op2);
      break;

    case BRS_FALSE:
      // A false UNORDERED_GE means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
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
      if (op1.known_isnan ())
	r.set_varying (type);
      else
	build_le (r, type, op1);
      break;

    case BRS_FALSE:
      // A false UNORDERED_GE means both operands are !NAN, so it's
      // impossible for op1 to be a NAN.
      if (op1.known_isnan ())
	r.set_undefined ();
      else if (build_gt (r, type, op1))
	r.clear_nan ();
      break;

    default:
      break;
    }
  return true;
}

class foperator_unordered_equal : public range_operator_float
{
  using range_operator_float::fold_range;
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
public:
  bool fold_range (irange &r, tree type,
		   const frange &op1, const frange &op2,
		   relation_trio rel = TRIO_VARYING) const final override
  {
    if (op1.known_isnan () || op2.known_isnan ())
      {
	r = range_true (type);
	return true;
      }
    if (!fop_equal.fold_range (r, type, op1, op2, rel))
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
      // Add the posibility of a NAN.
      r.update_nan ();
      break;

    case BRS_FALSE:
      // A false UNORDERED_EQ means both operands are !NAN, so it's
      // impossible for op2 to be a NAN.
      if (op2.known_isnan ())
	r.set_undefined ();
      else
	{
	  // The false side indictates !NAN and not equal.  We can at least
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

// Final tweaks for float binary op op1_range/op2_range.
// Return TRUE if the operation is performed and a valid range is available.

static bool
float_binary_op_range_finish (bool ret, frange &r, tree type,
			      const frange &lhs)
{
  if (!ret)
    return false;

  // If we get a known NAN from reverse op, it means either that
  // the other operand was known NAN (in that case we know nothing),
  // or the reverse operation introduced a known NAN.
  // Say for lhs = op1 * op2 if lhs is [-0, +0] and op2 is too,
  // 0 / 0 is known NAN.  Just punt in that case.
  // Or if lhs is a known NAN, we also don't know anything.
  if (r.known_isnan () || lhs.known_isnan ())
    {
      r.set_varying (type);
      return true;
    }

  // If lhs isn't NAN, then neither operand could be NAN,
  // even if the reverse operation does introduce a maybe_nan.
  if (!lhs.maybe_isnan ())
    r.clear_nan ();
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
    lb = real_value_negate (&dconst0);
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
      ub = real_value_negate (&dconst0);
    }
  else
    {
      lb = dconstninf;
      ub = dconstinf;
    }
}

class foperator_plus : public range_operator_float
{
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
public:
  virtual bool op1_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op2,
			  relation_trio = TRIO_VARYING) const final override
  {
    if (lhs.undefined_p ())
      return false;
    range_op_handler minus (MINUS_EXPR, type);
    if (!minus)
      return false;
    return float_binary_op_range_finish (minus.fold_range (r, type, lhs, op2),
					 r, type, lhs);
  }
  virtual bool op2_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op1,
			  relation_trio = TRIO_VARYING) const final override
  {
    return op1_range (r, type, lhs, op1);
  }
private:
  void rv_fold (REAL_VALUE_TYPE &lb, REAL_VALUE_TYPE &ub, bool &maybe_nan,
		tree type,
		const REAL_VALUE_TYPE &lh_lb,
		const REAL_VALUE_TYPE &lh_ub,
		const REAL_VALUE_TYPE &rh_lb,
		const REAL_VALUE_TYPE &rh_ub,
		relation_kind) const final override
  {
    frange_arithmetic (PLUS_EXPR, type, lb, lh_lb, rh_lb, dconstninf);
    frange_arithmetic (PLUS_EXPR, type, ub, lh_ub, rh_ub, dconstinf);

    // [-INF] + [+INF] = NAN
    if (real_isinf (&lh_lb, true) && real_isinf (&rh_ub, false))
      maybe_nan = true;
    // [+INF] + [-INF] = NAN
    else if (real_isinf (&lh_ub, false) && real_isinf (&rh_lb, true))
      maybe_nan = true;
    else
      maybe_nan = false;
  }
} fop_plus;


class foperator_minus : public range_operator_float
{
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
public:
  virtual bool op1_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op2,
			  relation_trio = TRIO_VARYING) const final override
  {
    if (lhs.undefined_p ())
      return false;
    return float_binary_op_range_finish (fop_plus.fold_range (r, type, lhs,
							      op2),
					 r, type, lhs);
  }
  virtual bool op2_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op1,
			  relation_trio = TRIO_VARYING) const final override
  {
    if (lhs.undefined_p ())
      return false;
    return float_binary_op_range_finish (fold_range (r, type, op1, lhs),
					 r, type, lhs);
  }
private:
  void rv_fold (REAL_VALUE_TYPE &lb, REAL_VALUE_TYPE &ub, bool &maybe_nan,
		tree type,
		const REAL_VALUE_TYPE &lh_lb,
		const REAL_VALUE_TYPE &lh_ub,
		const REAL_VALUE_TYPE &rh_lb,
		const REAL_VALUE_TYPE &rh_ub,
		relation_kind) const final override
  {
    frange_arithmetic (MINUS_EXPR, type, lb, lh_lb, rh_ub, dconstninf);
    frange_arithmetic (MINUS_EXPR, type, ub, lh_ub, rh_lb, dconstinf);

    // [+INF] - [+INF] = NAN
    if (real_isinf (&lh_ub, false) && real_isinf (&rh_ub, false))
      maybe_nan = true;
    // [-INF] - [-INF] = NAN
    else if (real_isinf (&lh_lb, true) && real_isinf (&rh_lb, true))
      maybe_nan = true;
    else
      maybe_nan = false;
  }
} fop_minus;


class foperator_mult_div_base : public range_operator_float
{
protected:
  // Given CP[0] to CP[3] floating point values rounded to -INF,
  // set LB to the smallest of them (treating -0 as smaller to +0).
  // Given CP[4] to CP[7] floating point values rounded to +INF,
  // set UB to the largest of them (treating -0 as smaller to +0).
  static void find_range (REAL_VALUE_TYPE &lb, REAL_VALUE_TYPE &ub,
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
};


class foperator_mult : public foperator_mult_div_base
{
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
public:
  virtual bool op1_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op2,
			  relation_trio = TRIO_VARYING) const final override
  {
    if (lhs.undefined_p ())
      return false;
    range_op_handler rdiv (RDIV_EXPR, type);
    if (!rdiv)
      return false;
    return float_binary_op_range_finish (rdiv.fold_range (r, type, lhs, op2),
					 r, type, lhs);
  }
  virtual bool op2_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op1,
			  relation_trio = TRIO_VARYING) const final override
  {
    return op1_range (r, type, lhs, op1);
  }
private:
  void rv_fold (REAL_VALUE_TYPE &lb, REAL_VALUE_TYPE &ub, bool &maybe_nan,
		tree type,
		const REAL_VALUE_TYPE &lh_lb,
		const REAL_VALUE_TYPE &lh_ub,
		const REAL_VALUE_TYPE &rh_lb,
		const REAL_VALUE_TYPE &rh_ub,
		relation_kind kind) const final override
  {
    bool is_square
      = (kind == VREL_EQ
	 && real_equal (&lh_lb, &rh_lb)
	 && real_equal (&lh_ub, &rh_ub)
	 && real_isneg (&lh_lb) == real_isneg (&rh_lb)
	 && real_isneg (&lh_ub) == real_isneg (&rh_ub));

    maybe_nan = false;
    // x * x never produces a new NAN and we only multiply the same
    // values, so the 0 * INF problematic cases never appear there.
    if (!is_square)
      {
	// [+-0, +-0] * [+INF,+INF] (or [-INF,-INF] or swapped is a known NAN.
	if ((zero_p (lh_lb, lh_ub) && singleton_inf_p (rh_lb, rh_ub))
	    || (zero_p (rh_lb, rh_ub) && singleton_inf_p (lh_lb, lh_ub)))
	  {
	    real_nan (&lb, "", 0, TYPE_MODE (type));
	    ub = lb;
	    maybe_nan = true;
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
	      return inf_range (lb, ub, signbit_known);

	    // If one of the multiplicands must be zero, the resulting
	    // range is +-0 and NAN.
	    if (zero_p (lh_lb, lh_ub) || zero_p (rh_lb, rh_ub))
	      return zero_range (lb, ub, signbit_known);

	    // Otherwise one of the multiplicands could be
	    // [0.0, nextafter (0.0, 1.0)] and the [DBL_MAX, INF]
	    // or similarly with different signs.  0.0 * DBL_MAX
	    // is still 0.0, nextafter (0.0, 1.0) * INF is still INF,
	    // so if the signs are always the same or always different,
	    // result is [+0.0, +INF] or [-INF, -0.0], otherwise VARYING.
	    return zero_to_inf_range (lb, ub, signbit_known);
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
	      cp[1] = real_value_negate (&dconst0);
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
  }
} fop_mult;


class foperator_div : public foperator_mult_div_base
{
  using range_operator_float::op1_range;
  using range_operator_float::op2_range;
public:
  virtual bool op1_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op2,
			  relation_trio = TRIO_VARYING) const final override
  {
    if (lhs.undefined_p ())
      return false;
    return float_binary_op_range_finish (fop_mult.fold_range (r, type, lhs,
							      op2),
					 r, type, lhs);
  }
  virtual bool op2_range (frange &r, tree type,
			  const frange &lhs,
			  const frange &op1,
			  relation_trio = TRIO_VARYING) const final override
  {
    if (lhs.undefined_p ())
      return false;
    return float_binary_op_range_finish (fold_range (r, type, op1, lhs),
					 r, type, lhs);
  }
private:
  void rv_fold (REAL_VALUE_TYPE &lb, REAL_VALUE_TYPE &ub, bool &maybe_nan,
		tree type,
		const REAL_VALUE_TYPE &lh_lb,
		const REAL_VALUE_TYPE &lh_ub,
		const REAL_VALUE_TYPE &rh_lb,
		const REAL_VALUE_TYPE &rh_ub,
		relation_kind) const final override
  {
    // +-0.0 / +-0.0 or +-INF / +-INF is a known NAN.
    if ((zero_p (lh_lb, lh_ub) && zero_p (rh_lb, rh_ub))
	|| (singleton_inf_p (lh_lb, lh_ub) || singleton_inf_p (rh_lb, rh_ub)))
      {
	real_nan (&lb, "", 0, TYPE_MODE (type));
	ub = lb;
	maybe_nan = true;
	return;
      }

    // If +-0.0 is in both ranges, it is a maybe NAN.
    if (contains_zero_p (lh_lb, lh_ub) && contains_zero_p (rh_lb, rh_ub))
      maybe_nan = true;
    // If +-INF is in both ranges, it is a maybe NAN.
    else if ((real_isinf (&lh_lb) || real_isinf (&lh_ub))
	     && (real_isinf (&rh_lb) || real_isinf (&rh_ub)))
      maybe_nan = true;
    else
      maybe_nan = false;

    int signbit_known = signbit_known_p (lh_lb, lh_ub, rh_lb, rh_ub);

    // If dividend must be zero, the range is just +-0
    // (including if the divisor is +-INF).
    // If divisor must be +-INF, the range is just +-0
    // (including if the dividend is zero).
    if (zero_p (lh_lb, lh_ub) || singleton_inf_p (rh_lb, rh_ub))
      return zero_range (lb, ub, signbit_known);

    // If divisor must be zero, the range is just +-INF
    // (including if the dividend is +-INF).
    // If dividend must be +-INF, the range is just +-INF
    // (including if the dividend is zero).
    if (zero_p (rh_lb, rh_ub) || singleton_inf_p (lh_lb, lh_ub))
      return inf_range (lb, ub, signbit_known);

    // Otherwise if both operands may be zero, divisor could be
    // nextafter(0.0, +-1.0) and dividend +-0.0
    // in which case result is going to INF or vice versa and
    // result +0.0.  So, all we can say for that case is if the
    // signs of divisor and dividend are always the same we have
    // [+0.0, +INF], if they are always different we have
    // [-INF, -0.0].  If they vary, VARING.
    // If both may be +-INF, divisor could be INF and dividend FLT_MAX,
    // in which case result is going to INF or vice versa and
    // result +0.0.  So, all we can say for that case is if the
    // signs of divisor and dividend are always the same we have
    // [+0.0, +INF], if they are always different we have
    // [-INF, -0.0].  If they vary, VARYING.
    if (maybe_nan)
      return zero_to_inf_range (lb, ub, signbit_known);

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
  }
} fop_div;

// Instantiate a range_op_table for floating point operations.
static floating_op_table global_floating_table;

// Pointer to the float table so the dispatch code can access it.
floating_op_table *floating_tree_table = &global_floating_table;

floating_op_table::floating_op_table ()
{
  set (SSA_NAME, fop_identity);
  set (PAREN_EXPR, fop_identity);
  set (OBJ_TYPE_REF, fop_identity);
  set (REAL_CST, fop_identity);

  // All the relational operators are expected to work, because the
  // calculation of ranges on outgoing edges expect the handlers to be
  // present.
  set (EQ_EXPR, fop_equal);
  set (NE_EXPR, fop_not_equal);
  set (LT_EXPR, fop_lt);
  set (LE_EXPR, fop_le);
  set (GT_EXPR, fop_gt);
  set (GE_EXPR, fop_ge);
  set (UNLE_EXPR, fop_unordered_le);
  set (UNLT_EXPR, fop_unordered_lt);
  set (UNGE_EXPR, fop_unordered_ge);
  set (UNGT_EXPR, fop_unordered_gt);
  set (UNEQ_EXPR, fop_unordered_equal);
  set (ORDERED_EXPR, fop_ordered);
  set (UNORDERED_EXPR, fop_unordered);

  set (ABS_EXPR, fop_abs);
  set (NEGATE_EXPR, fop_negate);
  set (PLUS_EXPR, fop_plus);
  set (MINUS_EXPR, fop_minus);
  set (MULT_EXPR, fop_mult);
  set (RDIV_EXPR, fop_div);
}

// Return a pointer to the range_operator_float instance, if there is
// one associated with tree_code CODE.

range_operator_float *
floating_op_table::operator[] (enum tree_code code)
{
  return m_range_tree[code];
}

// Add OP to the handler table for CODE.

void
floating_op_table::set (enum tree_code code, range_operator_float &op)
{
  gcc_checking_assert (m_range_tree[code] == NULL);
  m_range_tree[code] = &op;
}

#if CHECKING_P
#include "selftest.h"

namespace selftest
{

// Build an frange from string endpoints.

inline frange
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
  fop_negate.fold_range (r, float_type_node, r0, trange);
  ASSERT_EQ (r, frange_float ("-10", "5"));

  // negate([0, 1] -NAN) => [-1, -0] +NAN
  r0 = frange_float ("0", "1");
  r0.update_nan (true);
  fop_negate.fold_range (r, float_type_node, r0, trange);
  r1 = frange_float ("-1", "-0");
  r1.update_nan (false);
  ASSERT_EQ (r, r1);

  // [-INF,+INF] + [-INF,+INF] could be a NAN.
  range_op_handler plus (PLUS_EXPR, float_type_node);
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
