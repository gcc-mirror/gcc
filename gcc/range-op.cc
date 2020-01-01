/* Code for range operators.
   Copyright (C) 2017-2020 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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
#include "range-op.h"

// Return the upper limit for a type.

static inline wide_int
max_limit (const_tree type)
{
  return wi::max_value (TYPE_PRECISION (type) , TYPE_SIGN (type));
}

// Return the lower limit for a type.

static inline wide_int
min_limit (const_tree type)
{
  return wi::min_value (TYPE_PRECISION (type) , TYPE_SIGN (type));
}

// If the range of either op1 or op2 is undefined, set the result to
// undefined and return TRUE.

inline bool
empty_range_check (value_range &r,
		   const value_range &op1, const value_range & op2)
{
  if (op1.undefined_p () || op2.undefined_p ())
    {
      r.set_undefined ();
      return true;
    }
  else
    return false;
}

// Return TRUE if shifting by OP is undefined behavior, and set R to
// the appropriate range.

static inline bool
undefined_shift_range_check (value_range &r, tree type, const value_range op)
{
  if (op.undefined_p ())
    {
      r = value_range ();
      return true;
    }

  // Shifting by any values outside [0..prec-1], gets undefined
  // behavior from the shift operation.  We cannot even trust
  // SHIFT_COUNT_TRUNCATED at this stage, because that applies to rtl
  // shifts, and the operation at the tree level may be widened.
  if (wi::lt_p (op.lower_bound (), 0, TYPE_SIGN (op.type ()))
      || wi::ge_p (op.upper_bound (),
		   TYPE_PRECISION (type), TYPE_SIGN (op.type ())))
    {
      r = value_range (type);
      return true;
    }
  return false;
}

// Return TRUE if 0 is within [WMIN, WMAX].

static inline bool
wi_includes_zero_p (tree type, const wide_int &wmin, const wide_int &wmax)
{
  signop sign = TYPE_SIGN (type);
  return wi::le_p (wmin, 0, sign) && wi::ge_p (wmax, 0, sign);
}

// Return TRUE if [WMIN, WMAX] is the singleton 0.

static inline bool
wi_zero_p (tree type, const wide_int &wmin, const wide_int &wmax)
{
  unsigned prec = TYPE_PRECISION (type);
  return wmin == wmax && wi::eq_p (wmin, wi::zero (prec));
}

// Default wide_int fold operation returns [MIN, MAX].

void
range_operator::wi_fold (value_range &r, tree type,
			 const wide_int &lh_lb ATTRIBUTE_UNUSED,
			 const wide_int &lh_ub ATTRIBUTE_UNUSED,
			 const wide_int &rh_lb ATTRIBUTE_UNUSED,
			 const wide_int &rh_ub ATTRIBUTE_UNUSED) const
{
  gcc_checking_assert (value_range::supports_type_p (type));
  r = value_range (type);
}

// The default for fold is to break all ranges into sub-ranges and
// invoke the wi_fold method on each sub-range pair.

bool
range_operator::fold_range (value_range &r, tree type,
			    const value_range &lh,
			    const value_range &rh) const
{
  gcc_checking_assert (value_range::supports_type_p (type));
  if (empty_range_check (r, lh, rh))
    return true;

  value_range tmp;
  r.set_undefined ();
  for (unsigned x = 0; x < lh.num_pairs (); ++x)
    for (unsigned y = 0; y < rh.num_pairs (); ++y)
      {
	wide_int lh_lb = lh.lower_bound (x);
	wide_int lh_ub = lh.upper_bound (x);
	wide_int rh_lb = rh.lower_bound (y);
	wide_int rh_ub = rh.upper_bound (y);
	wi_fold (tmp, type, lh_lb, lh_ub, rh_lb, rh_ub);
	r.union_ (tmp);
	if (r.varying_p ())
	  return true;
      }
  return true;
}

// The default for op1_range is to return false.

bool
range_operator::op1_range (value_range &r ATTRIBUTE_UNUSED,
			   tree type ATTRIBUTE_UNUSED,
			   const value_range &lhs ATTRIBUTE_UNUSED,
			   const value_range &op2 ATTRIBUTE_UNUSED) const
{
  return false;
}

// The default for op2_range is to return false.

bool
range_operator::op2_range (value_range &r ATTRIBUTE_UNUSED,
			   tree type ATTRIBUTE_UNUSED,
			   const value_range &lhs ATTRIBUTE_UNUSED,
			   const value_range &op1 ATTRIBUTE_UNUSED) const
{
  return false;
}


// Create and return a range from a pair of wide-ints that are known
// to have overflowed (or underflowed).

static void
value_range_from_overflowed_bounds (value_range &r, tree type,
				    const wide_int &wmin,
				    const wide_int &wmax)
{
  const signop sgn = TYPE_SIGN (type);
  const unsigned int prec = TYPE_PRECISION (type);

  wide_int tmin = wide_int::from (wmin, prec, sgn);
  wide_int tmax = wide_int::from (wmax, prec, sgn);

  bool covers = false;
  wide_int tem = tmin;
  tmin = tmax + 1;
  if (wi::cmp (tmin, tmax, sgn) < 0)
    covers = true;
  tmax = tem - 1;
  if (wi::cmp (tmax, tem, sgn) > 0)
    covers = true;

  // If the anti-range would cover nothing, drop to varying.
  // Likewise if the anti-range bounds are outside of the types
  // values.
  if (covers || wi::cmp (tmin, tmax, sgn) > 0)
    r = value_range (type);
  else
    r = value_range (type, tmin, tmax, VR_ANTI_RANGE);
}

// Create and return a range from a pair of wide-ints.  MIN_OVF and
// MAX_OVF describe any overflow that might have occurred while
// calculating WMIN and WMAX respectively.

static void
value_range_with_overflow (value_range &r, tree type,
			   const wide_int &wmin, const wide_int &wmax,
			   wi::overflow_type min_ovf = wi::OVF_NONE,
			   wi::overflow_type max_ovf = wi::OVF_NONE)
{
  const signop sgn = TYPE_SIGN (type);
  const unsigned int prec = TYPE_PRECISION (type);
  const bool overflow_wraps = TYPE_OVERFLOW_WRAPS (type);

  // For one bit precision if max != min, then the range covers all
  // values.
  if (prec == 1 && wi::ne_p (wmax, wmin))
    {
      r = value_range (type);
      return;
    }

  if (overflow_wraps)
    {
      // If overflow wraps, truncate the values and adjust the range,
      // kind, and bounds appropriately.
      if ((min_ovf != wi::OVF_NONE) == (max_ovf != wi::OVF_NONE))
	{
	  wide_int tmin = wide_int::from (wmin, prec, sgn);
	  wide_int tmax = wide_int::from (wmax, prec, sgn);
	  // If the limits are swapped, we wrapped around and cover
	  // the entire range.
	  if (wi::gt_p (tmin, tmax, sgn))
	    r = value_range (type);
	  else
	    // No overflow or both overflow or underflow.  The range
	    // kind stays normal.
	    r = value_range (type, tmin, tmax);
	  return;
	}

      if ((min_ovf == wi::OVF_UNDERFLOW && max_ovf == wi::OVF_NONE)
	  || (max_ovf == wi::OVF_OVERFLOW && min_ovf == wi::OVF_NONE))
	value_range_from_overflowed_bounds (r, type, wmin, wmax);
      else
	// Other underflow and/or overflow, drop to VR_VARYING.
	r = value_range (type);
    }
  else
    {
      // If overflow does not wrap, saturate to [MIN, MAX].
      wide_int new_lb, new_ub;
      if (min_ovf == wi::OVF_UNDERFLOW)
	new_lb = wi::min_value (prec, sgn);
      else if (min_ovf == wi::OVF_OVERFLOW)
	new_lb = wi::max_value (prec, sgn);
      else
        new_lb = wmin;

      if (max_ovf == wi::OVF_UNDERFLOW)
	new_ub = wi::min_value (prec, sgn);
      else if (max_ovf == wi::OVF_OVERFLOW)
	new_ub = wi::max_value (prec, sgn);
      else
        new_ub = wmax;

      r = value_range (type, new_lb, new_ub);
    }
}

// Create and return a range from a pair of wide-ints.  Canonicalize
// the case where the bounds are swapped.  In which case, we transform
// [10,5] into [MIN,5][10,MAX].

static inline void
create_possibly_reversed_range (value_range &r, tree type,
				const wide_int &new_lb, const wide_int &new_ub)
{
  signop s = TYPE_SIGN (type);
  // If the bounds are swapped, treat the result as if an overflow occured.
  if (wi::gt_p (new_lb, new_ub, s))
    value_range_from_overflowed_bounds (r, type, new_lb, new_ub);
  else
    // Otherwise its just a normal range.
    r = value_range (type, new_lb, new_ub);
}

// Return a value_range instance that is a boolean TRUE.

static inline value_range
range_true (tree type)
{
  unsigned prec = TYPE_PRECISION (type);
  return value_range (type, wi::one (prec), wi::one (prec));
}

// Return a value_range instance that is a boolean FALSE.

static inline value_range
range_false (tree type)
{
  unsigned prec = TYPE_PRECISION (type);
  return value_range (type, wi::zero (prec), wi::zero (prec));
}

// Return a value_range that covers both true and false.

static inline value_range
range_true_and_false (tree type)
{
  unsigned prec = TYPE_PRECISION (type);
  return value_range (type, wi::zero (prec), wi::one (prec));
}

enum bool_range_state { BRS_FALSE, BRS_TRUE, BRS_EMPTY, BRS_FULL };

// Return the summary information about boolean range LHS.  Return an
// "interesting" range in R.  For EMPTY or FULL, return the equivalent
// range for TYPE, for BRS_TRUE and BRS false, return the negation of
// the bool range.

static bool_range_state
get_bool_state (value_range &r, const value_range &lhs, tree val_type)
{
  // If there is no result, then this is unexecutable.
  if (lhs.undefined_p ())
    {
      r.set_undefined ();
      return BRS_EMPTY;
    }

  // If the bounds aren't the same, then it's not a constant.
  if (!wi::eq_p (lhs.upper_bound (), lhs.lower_bound ()))
    {
      r.set_varying (val_type);
      return BRS_FULL;
    }

  if (lhs.zero_p ())
    return BRS_FALSE;

  return BRS_TRUE;
}


class operator_equal : public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &val) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &val) const;
} op_equal;

bool
operator_equal::fold_range (value_range &r, tree type,
			    const value_range &op1,
			    const value_range &op2) const
{
  if (empty_range_check (r, op1, op2))
    return true;

  // We can be sure the values are always equal or not if both ranges
  // consist of a single value, and then compare them.
  if (wi::eq_p (op1.lower_bound (), op1.upper_bound ())
      && wi::eq_p (op2.lower_bound (), op2.upper_bound ()))
    {
      if (wi::eq_p (op1.lower_bound (), op2.upper_bound()))
	r = range_true (type);
      else
	r = range_false (type);
    }
  else
    {
      // If ranges do not intersect, we know the range is not equal,
      // otherwise we don't know anything for sure.
      r = op1;
      r.intersect (op2);
      if (r.undefined_p ())
	r = range_false (type);
      else
	r = range_true_and_false (type);
    }
  return true;
}

bool
operator_equal::op1_range (value_range &r, tree type,
			   const value_range &lhs,
			   const value_range &op2) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_FALSE:
      // If the result is false, the only time we know anything is
      // if OP2 is a constant.
      if (wi::eq_p (op2.lower_bound(), op2.upper_bound()))
	{
	  r = op2;
	  r.invert ();
	}
      else
	r.set_varying (type);
      break;

    case BRS_TRUE:
      // If it's true, the result is the same as OP2.
      r = op2;
      break;

    default:
      break;
    }
  return true;
}

bool
operator_equal::op2_range (value_range &r, tree type,
			   const value_range &lhs,
			   const value_range &op1) const
{
  return operator_equal::op1_range (r, type, lhs, op1);
}


class operator_not_equal : public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
} op_not_equal;

bool
operator_not_equal::fold_range (value_range &r, tree type,
				const value_range &op1,
				const value_range &op2) const
{
  if (empty_range_check (r, op1, op2))
    return true;

  // We can be sure the values are always equal or not if both ranges
  // consist of a single value, and then compare them.
  if (wi::eq_p (op1.lower_bound (), op1.upper_bound ())
      && wi::eq_p (op2.lower_bound (), op2.upper_bound ()))
    {
      if (wi::ne_p (op1.lower_bound (), op2.upper_bound()))
	r = range_true (type);
      else
	r = range_false (type);
    }
  else
    {
      // If ranges do not intersect, we know the range is not equal,
      // otherwise we don't know anything for sure.
      r = op1;
      r.intersect (op2);
      if (r.undefined_p ())
	r = range_true (type);
      else
	r = range_true_and_false (type);
    }
  return true;
}

bool
operator_not_equal::op1_range (value_range &r, tree type,
			       const value_range &lhs,
			       const value_range &op2) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // If the result is true, the only time we know anything is if
      // OP2 is a constant.
      if (wi::eq_p (op2.lower_bound(), op2.upper_bound()))
	{
	  r = op2;
	  r.invert ();
	}
      else
	r.set_varying (type);
      break;

    case BRS_FALSE:
      // If its true, the result is the same as OP2.
      r = op2;
      break;

    default:
      break;
    }
  return true;
}


bool
operator_not_equal::op2_range (value_range &r, tree type,
			       const value_range &lhs,
			       const value_range &op1) const
{
  return operator_not_equal::op1_range (r, type, lhs, op1);
}

// (X < VAL) produces the range of [MIN, VAL - 1].

static void
build_lt (value_range &r, tree type, const wide_int &val)
{
  wi::overflow_type ov;
  wide_int lim = wi::sub (val, 1, TYPE_SIGN (type), &ov);

  // If val - 1 underflows, check if X < MIN, which is an empty range.
  if (ov)
    r.set_undefined ();
  else
    r = value_range (type, min_limit (type), lim);
}

// (X <= VAL) produces the range of [MIN, VAL].

static void
build_le (value_range &r, tree type, const wide_int &val)
{
  r = value_range (type, min_limit (type), val);
}

// (X > VAL) produces the range of [VAL + 1, MAX].

static void
build_gt (value_range &r, tree type, const wide_int &val)
{
  wi::overflow_type ov;
  wide_int lim = wi::add (val, 1, TYPE_SIGN (type), &ov);
  // If val + 1 overflows, check is for X > MAX, which is an empty range.
  if (ov)
    r.set_undefined ();
  else
    r = value_range (type, lim, max_limit (type));
}

// (X >= val) produces the range of [VAL, MAX].

static void
build_ge (value_range &r, tree type, const wide_int &val)
{
  r = value_range (type, val, max_limit (type));
}


class operator_lt :  public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
} op_lt;

bool
operator_lt::fold_range (value_range &r, tree type,
			 const value_range &op1,
			 const value_range &op2) const
{
  if (empty_range_check (r, op1, op2))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::lt_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_true (type);
  else if (!wi::lt_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_lt::op1_range (value_range &r, tree type,
			const value_range &lhs,
			const value_range &op2) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_lt (r, type, op2.upper_bound ());
      break;

    case BRS_FALSE:
      build_ge (r, type, op2.lower_bound ());
      break;

    default:
      break;
    }
  return true;
}

bool
operator_lt::op2_range (value_range &r, tree type,
			const value_range &lhs,
			const value_range &op1) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_FALSE:
      build_le (r, type, op1.upper_bound ());
      break;

    case BRS_TRUE:
      build_gt (r, type, op1.lower_bound ());
      break;

    default:
      break;
    }
  return true;
}


class operator_le :  public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
} op_le;

bool
operator_le::fold_range (value_range &r, tree type,
			 const value_range &op1,
			 const value_range &op2) const
{
  if (empty_range_check (r, op1, op2))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::le_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_true (type);
  else if (!wi::le_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_le::op1_range (value_range &r, tree type,
			const value_range &lhs,
			const value_range &op2) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_le (r, type, op2.upper_bound ());
      break;

    case BRS_FALSE:
      build_gt (r, type, op2.lower_bound ());
      break;

    default:
      break;
    }
  return true;
}

bool
operator_le::op2_range (value_range &r, tree type,
			const value_range &lhs,
			const value_range &op1) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_FALSE:
      build_lt (r, type, op1.upper_bound ());
      break;

    case BRS_TRUE:
      build_ge (r, type, op1.lower_bound ());
      break;

    default:
      break;
    }
  return true;
}


class operator_gt :  public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
} op_gt;

bool
operator_gt::fold_range (value_range &r, tree type,
			 const value_range &op1, const value_range &op2) const
{
  if (empty_range_check (r, op1, op2))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::gt_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_true (type);
  else if (!wi::gt_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_gt::op1_range (value_range &r, tree type,
			const value_range &lhs, const value_range &op2) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_gt (r, type, op2.lower_bound ());
      break;

    case BRS_FALSE:
      build_le (r, type, op2.upper_bound ());
      break;

    default:
      break;
    }
  return true;
}

bool
operator_gt::op2_range (value_range &r, tree type,
			const value_range &lhs,
			const value_range &op1) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_FALSE:
      build_ge (r, type, op1.lower_bound ());
      break;

    case BRS_TRUE:
      build_lt (r, type, op1.upper_bound ());
      break;

    default:
      break;
    }
  return true;
}


class operator_ge :  public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
} op_ge;

bool
operator_ge::fold_range (value_range &r, tree type,
			 const value_range &op1,
			 const value_range &op2) const
{
  if (empty_range_check (r, op1, op2))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::ge_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_true (type);
  else if (!wi::ge_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);
  return true;
}

bool
operator_ge::op1_range (value_range &r, tree type,
			const value_range &lhs,
			const value_range &op2) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_ge (r, type, op2.lower_bound ());
      break;

    case BRS_FALSE:
      build_lt (r, type, op2.upper_bound ());
      break;

    default:
      break;
    }
  return true;
}

bool
operator_ge::op2_range (value_range &r, tree type,
			const value_range &lhs,
			const value_range &op1) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_FALSE:
      build_gt (r, type, op1.lower_bound ());
      break;

    case BRS_TRUE:
      build_le (r, type, op1.upper_bound ());
      break;

    default:
      break;
    }
  return true;
}


class operator_plus : public range_operator
{
public:
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
} op_plus;

void
operator_plus::wi_fold (value_range &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const
{
  wi::overflow_type ov_lb, ov_ub;
  signop s = TYPE_SIGN (type);
  wide_int new_lb = wi::add (lh_lb, rh_lb, s, &ov_lb);
  wide_int new_ub = wi::add (lh_ub, rh_ub, s, &ov_ub);
  value_range_with_overflow (r, type, new_lb, new_ub, ov_lb, ov_ub);
}

bool
operator_plus::op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const
{
  return range_op_handler (MINUS_EXPR, type)->fold_range (r, type, lhs, op2);
}

bool
operator_plus::op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const
{
  return range_op_handler (MINUS_EXPR, type)->fold_range (r, type, lhs, op1);
}


class operator_minus : public range_operator
{
public:
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
} op_minus;

void 
operator_minus::wi_fold (value_range &r, tree type,
			 const wide_int &lh_lb, const wide_int &lh_ub,
			 const wide_int &rh_lb, const wide_int &rh_ub) const
{
  wi::overflow_type ov_lb, ov_ub;
  signop s = TYPE_SIGN (type);
  wide_int new_lb = wi::sub (lh_lb, rh_ub, s, &ov_lb);
  wide_int new_ub = wi::sub (lh_ub, rh_lb, s, &ov_ub);
  value_range_with_overflow (r, type, new_lb, new_ub, ov_lb, ov_ub);
}

bool
operator_minus::op1_range (value_range &r, tree type,
			   const value_range &lhs,
			   const value_range &op2) const
{
  return range_op_handler (PLUS_EXPR, type)->fold_range (r, type, lhs, op2);
}

bool
operator_minus::op2_range (value_range &r, tree type,
			   const value_range &lhs,
			   const value_range &op1) const
{
  return fold_range (r, type, op1, lhs);
}


class operator_min : public range_operator
{
public:
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
} op_min;

void
operator_min::wi_fold (value_range &r, tree type,
		       const wide_int &lh_lb, const wide_int &lh_ub,
		       const wide_int &rh_lb, const wide_int &rh_ub) const
{
  signop s = TYPE_SIGN (type);
  wide_int new_lb = wi::min (lh_lb, rh_lb, s);
  wide_int new_ub = wi::min (lh_ub, rh_ub, s);
  value_range_with_overflow (r, type, new_lb, new_ub);
}


class operator_max : public range_operator
{
public:
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
} op_max;

void
operator_max::wi_fold (value_range &r, tree type,
		       const wide_int &lh_lb, const wide_int &lh_ub,
		       const wide_int &rh_lb, const wide_int &rh_ub) const
{
  signop s = TYPE_SIGN (type);
  wide_int new_lb = wi::max (lh_lb, rh_lb, s);
  wide_int new_ub = wi::max (lh_ub, rh_ub, s);
  value_range_with_overflow (r, type, new_lb, new_ub);
}


class cross_product_operator : public range_operator
{
public:
  // Perform an operation between two wide-ints and place the result
  // in R.  Return true if the operation overflowed.
  virtual bool wi_op_overflows (wide_int &r,
				tree type,
				const wide_int &,
				const wide_int &) const = 0;

  // Calculate the cross product of two sets of sub-ranges and return it.
  void wi_cross_product (value_range &r, tree type,
			 const wide_int &lh_lb,
			 const wide_int &lh_ub,
			 const wide_int &rh_lb,
			 const wide_int &rh_ub) const;
};

// Calculate the cross product of two sets of ranges and return it.
//
// Multiplications, divisions and shifts are a bit tricky to handle,
// depending on the mix of signs we have in the two ranges, we need to
// operate on different values to get the minimum and maximum values
// for the new range.  One approach is to figure out all the
// variations of range combinations and do the operations.
//
// However, this involves several calls to compare_values and it is
// pretty convoluted.  It's simpler to do the 4 operations (MIN0 OP
// MIN1, MIN0 OP MAX1, MAX0 OP MIN1 and MAX0 OP MAX0 OP MAX1) and then
// figure the smallest and largest values to form the new range.

void
cross_product_operator::wi_cross_product (value_range &r, tree type,
					  const wide_int &lh_lb,
					  const wide_int &lh_ub,
					  const wide_int &rh_lb,
					  const wide_int &rh_ub) const
{
  wide_int cp1, cp2, cp3, cp4;
  // Default to varying.
  r = value_range (type);

  // Compute the 4 cross operations, bailing if we get an overflow we
  // can't handle.
  if (wi_op_overflows (cp1, type, lh_lb, rh_lb))
    return;
  if (wi::eq_p (lh_lb, lh_ub))
    cp3 = cp1;
  else if (wi_op_overflows (cp3, type, lh_ub, rh_lb))
    return;
  if (wi::eq_p (rh_lb, rh_ub))
    cp2 = cp1;
  else if (wi_op_overflows (cp2, type, lh_lb, rh_ub))
    return;
  if (wi::eq_p (lh_lb, lh_ub))
    cp4 = cp2;
  else if (wi_op_overflows (cp4, type, lh_ub, rh_ub))
    return;

  // Order pairs.
  signop sign = TYPE_SIGN (type);
  if (wi::gt_p (cp1, cp2, sign))
    std::swap (cp1, cp2);
  if (wi::gt_p (cp3, cp4, sign))
    std::swap (cp3, cp4);

  // Choose min and max from the ordered pairs.
  wide_int res_lb = wi::min (cp1, cp3, sign);
  wide_int res_ub = wi::max (cp2, cp4, sign);
  value_range_with_overflow (r, type, res_lb, res_ub);
}


class operator_mult : public cross_product_operator
{
public:
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
  virtual bool wi_op_overflows (wide_int &res, tree type,
				const wide_int &w0, const wide_int &w1) const;
} op_mult;

bool
operator_mult::wi_op_overflows (wide_int &res, tree type,
				const wide_int &w0, const wide_int &w1) const
{
  wi::overflow_type overflow = wi::OVF_NONE;
  signop sign = TYPE_SIGN (type);
  res = wi::mul (w0, w1, sign, &overflow);
   if (overflow && TYPE_OVERFLOW_UNDEFINED (type))
     {
       // For multiplication, the sign of the overflow is given
       // by the comparison of the signs of the operands.
       if (sign == UNSIGNED || w0.sign_mask () == w1.sign_mask ())
	 res = wi::max_value (w0.get_precision (), sign);
       else
	 res = wi::min_value (w0.get_precision (), sign);
       return false;
     }
   return overflow;
}

void 
operator_mult::wi_fold (value_range &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const
{
  if (TYPE_OVERFLOW_UNDEFINED (type))
    {
      wi_cross_product (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
      return;
    }

  // Multiply the ranges when overflow wraps.  This is basically fancy
  // code so we don't drop to varying with an unsigned
  // [-3,-1]*[-3,-1].
  //
  // This test requires 2*prec bits if both operands are signed and
  // 2*prec + 2 bits if either is not.  Therefore, extend the values
  // using the sign of the result to PREC2.  From here on out,
  // everthing is just signed math no matter what the input types
  // were.

  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);
  widest2_int min0 = widest2_int::from (lh_lb, sign);
  widest2_int max0 = widest2_int::from (lh_ub, sign);
  widest2_int min1 = widest2_int::from (rh_lb, sign);
  widest2_int max1 = widest2_int::from (rh_ub, sign);
  widest2_int sizem1 = wi::mask <widest2_int> (prec, false);
  widest2_int size = sizem1 + 1;

  // Canonicalize the intervals.
  if (sign == UNSIGNED)
    {
      if (wi::ltu_p (size, min0 + max0))
	{
	  min0 -= size;
	  max0 -= size;
	}
      if (wi::ltu_p (size, min1 + max1))
	{
	  min1 -= size;
	  max1 -= size;
	}
    }

  // Sort the 4 products so that min is in prod0 and max is in
  // prod3.
  widest2_int prod0 = min0 * min1;
  widest2_int prod1 = min0 * max1;
  widest2_int prod2 = max0 * min1;
  widest2_int prod3 = max0 * max1;

  // min0min1 > max0max1
  if (prod0 > prod3)
    std::swap (prod0, prod3);

  // min0max1 > max0min1
  if (prod1 > prod2)
    std::swap (prod1, prod2);

  if (prod0 > prod1)
    std::swap (prod0, prod1);

  if (prod2 > prod3)
    std::swap (prod2, prod3);

  // diff = max - min
  prod2 = prod3 - prod0;
  if (wi::geu_p (prod2, sizem1))
    // The range covers all values.
    r = value_range (type);
  else
    {
      wide_int new_lb = wide_int::from (prod0, prec, sign);
      wide_int new_ub = wide_int::from (prod3, prec, sign);
      create_possibly_reversed_range (r, type, new_lb, new_ub);
    }
}


class operator_div : public cross_product_operator
{
public:
  operator_div (enum tree_code c)  { code = c; }
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
  virtual bool wi_op_overflows (wide_int &res, tree type,
				const wide_int &, const wide_int &) const;
private:
  enum tree_code code;
};

bool
operator_div::wi_op_overflows (wide_int &res, tree type,
			       const wide_int &w0, const wide_int &w1) const
{
  if (w1 == 0)
    return true;

  wi::overflow_type overflow = wi::OVF_NONE;
  signop sign = TYPE_SIGN (type);

  switch (code)
    {
    case EXACT_DIV_EXPR:
      // EXACT_DIV_EXPR is implemented as TRUNC_DIV_EXPR in
      // operator_exact_divide.  No need to handle it here.
      gcc_unreachable ();
      break;
    case TRUNC_DIV_EXPR:
      res = wi::div_trunc (w0, w1, sign, &overflow);
      break;
    case FLOOR_DIV_EXPR:
      res = wi::div_floor (w0, w1, sign, &overflow);
      break;
    case ROUND_DIV_EXPR:
      res = wi::div_round (w0, w1, sign, &overflow);
      break;
    case CEIL_DIV_EXPR:
      res = wi::div_ceil (w0, w1, sign, &overflow);
      break;
    default:
      gcc_unreachable ();
    }

  if (overflow && TYPE_OVERFLOW_UNDEFINED (type))
    {
      // For division, the only case is -INF / -1 = +INF.
      res = wi::max_value (w0.get_precision (), sign);
      return false;
    }
  return overflow;
}

void
operator_div::wi_fold (value_range &r, tree type,
		       const wide_int &lh_lb, const wide_int &lh_ub,
		       const wide_int &rh_lb, const wide_int &rh_ub) const
{
  // If we know we will divide by zero, return undefined.
  if (rh_lb == 0 && rh_ub == 0)
    {
      r = value_range ();
      return;
    }

  const wide_int dividend_min = lh_lb;
  const wide_int dividend_max = lh_ub;
  const wide_int divisor_min = rh_lb;
  const wide_int divisor_max = rh_ub;
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);
  wide_int extra_min, extra_max;

  // If we know we won't divide by zero, just do the division.
  if (!wi_includes_zero_p (type, divisor_min, divisor_max))
    {
      wi_cross_product (r, type, dividend_min, dividend_max,
		       divisor_min, divisor_max);
      return;
    }

  // If flag_non_call_exceptions, we must not eliminate a division by zero.
  if (cfun->can_throw_non_call_exceptions)
    {
      r = value_range (type);
      return;
    }

  // If we're definitely dividing by zero, there's nothing to do.
  if (wi_zero_p (type, divisor_min, divisor_max))
    {
      r = value_range ();
      return;
    }

  // Perform the division in 2 parts, [LB, -1] and [1, UB], which will
  // skip any division by zero.

  // First divide by the negative numbers, if any.
  if (wi::neg_p (divisor_min, sign))
    wi_cross_product (r, type, dividend_min, dividend_max,
		      divisor_min, wi::minus_one (prec));
  else
    r = value_range ();

  // Then divide by the non-zero positive numbers, if any.
  if (wi::gt_p (divisor_max, wi::zero (prec), sign))
    {
      value_range tmp;
      wi_cross_product (tmp, type, dividend_min, dividend_max,
			wi::one (prec), divisor_max);
      r.union_ (tmp);
    }
  // We shouldn't still have undefined here.
  gcc_checking_assert (!r.undefined_p ());
}

operator_div op_trunc_div (TRUNC_DIV_EXPR);
operator_div op_floor_div (FLOOR_DIV_EXPR);
operator_div op_round_div (ROUND_DIV_EXPR);
operator_div op_ceil_div (CEIL_DIV_EXPR);


class operator_exact_divide : public operator_div
{
public:
  operator_exact_divide () : operator_div (TRUNC_DIV_EXPR) { }
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;

} op_exact_div;

bool
operator_exact_divide::op1_range (value_range &r, tree type,
				  const value_range &lhs,
				  const value_range &op2) const
{
  tree offset;
  // [2, 4] = op1 / [3,3]   since its exact divide, no need to worry about
  // remainders in the endpoints, so op1 = [2,4] * [3,3] = [6,12].
  // We wont bother trying to enumerate all the in between stuff :-P
  // TRUE accuraacy is [6,6][9,9][12,12].  This is unlikely to matter most of
  // the time however.
  // If op2 is a multiple of 2, we would be able to set some non-zero bits.
  if (op2.singleton_p (&offset)
      && !integer_zerop (offset))
    return range_op_handler (MULT_EXPR, type)->fold_range (r, type, lhs, op2);
  return false;
}


class operator_lshift : public cross_product_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;

  virtual void wi_fold (value_range &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const;
  virtual bool wi_op_overflows (wide_int &res,
				tree type,
				const wide_int &,
				const wide_int &) const;
} op_lshift;

bool
operator_lshift::fold_range (value_range &r, tree type,
			     const value_range &op1,
			     const value_range &op2) const
{
  if (undefined_shift_range_check (r, type, op2))
    return true;

  // Transform left shifts by constants into multiplies.
  if (op2.singleton_p ())
    {
      unsigned shift = op2.lower_bound ().to_uhwi ();
      wide_int tmp = wi::set_bit_in_zero (shift, TYPE_PRECISION (type));
      value_range mult (type, tmp, tmp);

      // Force wrapping multiplication.
      bool saved_flag_wrapv = flag_wrapv;
      bool saved_flag_wrapv_pointer = flag_wrapv_pointer;
      flag_wrapv = 1;
      flag_wrapv_pointer = 1;
      bool b = range_op_handler (MULT_EXPR, type)->fold_range (r, type, op1,
							       mult);
      flag_wrapv = saved_flag_wrapv;
      flag_wrapv_pointer = saved_flag_wrapv_pointer;
      return b;
    }
  else
    // Otherwise, invoke the generic fold routine.
    return range_operator::fold_range (r, type, op1, op2);
}

void
operator_lshift::wi_fold (value_range &r, tree type,
			  const wide_int &lh_lb, const wide_int &lh_ub,
			  const wide_int &rh_lb, const wide_int &rh_ub) const
{
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);
  int overflow_pos = sign == SIGNED ? prec - 1 : prec;
  int bound_shift = overflow_pos - rh_ub.to_shwi ();
  // If bound_shift == HOST_BITS_PER_WIDE_INT, the llshift can
  // overflow.  However, for that to happen, rh.max needs to be zero,
  // which means rh is a singleton range of zero, which means it
  // should be handled by the lshift fold_range above.
  wide_int bound = wi::set_bit_in_zero (bound_shift, prec);
  wide_int complement = ~(bound - 1);
  wide_int low_bound, high_bound;
  bool in_bounds = false;

  if (sign == UNSIGNED)
    {
      low_bound = bound;
      high_bound = complement;
      if (wi::ltu_p (lh_ub, low_bound))
	{
	  // [5, 6] << [1, 2] == [10, 24].
	  // We're shifting out only zeroes, the value increases
	  // monotonically.
	  in_bounds = true;
	}
      else if (wi::ltu_p (high_bound, lh_lb))
	{
	  // [0xffffff00, 0xffffffff] << [1, 2]
	  // == [0xfffffc00, 0xfffffffe].
	  // We're shifting out only ones, the value decreases
	  // monotonically.
	  in_bounds = true;
	}
    }
  else
    {
      // [-1, 1] << [1, 2] == [-4, 4]
      low_bound = complement;
      high_bound = bound;
      if (wi::lts_p (lh_ub, high_bound)
	  && wi::lts_p (low_bound, lh_lb))
	{
	  // For non-negative numbers, we're shifting out only zeroes,
	  // the value increases monotonically.  For negative numbers,
	  // we're shifting out only ones, the value decreases
	  // monotonically.
	  in_bounds = true;
	}
    }

  if (in_bounds)
    wi_cross_product (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
  else
   r = value_range (type);
}

bool
operator_lshift::wi_op_overflows (wide_int &res, tree type,
				  const wide_int &w0, const wide_int &w1) const
{
  signop sign = TYPE_SIGN (type);
  if (wi::neg_p (w1))
    {
      // It's unclear from the C standard whether shifts can overflow.
      // The following code ignores overflow; perhaps a C standard
      // interpretation ruling is needed.
      res = wi::rshift (w0, -w1, sign);
    }
  else
    res = wi::lshift (w0, w1);
  return false;
}


class operator_rshift : public cross_product_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
  virtual bool wi_op_overflows (wide_int &res,
				tree type,
				const wide_int &w0,
				const wide_int &w1) const;
} op_rshift;

bool
operator_rshift::wi_op_overflows (wide_int &res,
				  tree type,
				  const wide_int &w0,
				  const wide_int &w1) const
{
  signop sign = TYPE_SIGN (type);
  if (wi::neg_p (w1))
    res = wi::lshift (w0, -w1);
  else
    {
      // It's unclear from the C standard whether shifts can overflow.
      // The following code ignores overflow; perhaps a C standard
      // interpretation ruling is needed.
      res = wi::rshift (w0, w1, sign);
    }
  return false;
}

bool
operator_rshift::fold_range (value_range &r, tree type,
			     const value_range &op1,
			     const value_range &op2) const
{
  // Invoke the generic fold routine if not undefined..
  if (undefined_shift_range_check (r, type, op2))
    return true;

  return range_operator::fold_range (r, type, op1, op2);
}

void
operator_rshift::wi_fold (value_range &r, tree type,
			  const wide_int &lh_lb, const wide_int &lh_ub,
			  const wide_int &rh_lb, const wide_int &rh_ub) const
{
  wi_cross_product (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
}


class operator_cast: public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;

} op_convert;

bool
operator_cast::fold_range (value_range &r, tree type ATTRIBUTE_UNUSED,
			   const value_range &lh,
			   const value_range &rh) const
{
  if (empty_range_check (r, lh, rh))
    return true;
  
  tree inner = lh.type ();
  tree outer = rh.type ();
  gcc_checking_assert (rh.varying_p ());
  gcc_checking_assert (types_compatible_p (outer, type));
  signop inner_sign = TYPE_SIGN (inner);
  signop outer_sign = TYPE_SIGN (outer);
  unsigned inner_prec = TYPE_PRECISION (inner);
  unsigned outer_prec = TYPE_PRECISION (outer);

  // Start with an empty range and add subranges.
  r = value_range ();
  for (unsigned x = 0; x < lh.num_pairs (); ++x)
    {
      wide_int lh_lb = lh.lower_bound (x);
      wide_int lh_ub = lh.upper_bound (x);

      // If the conversion is not truncating we can convert the min
      // and max values and canonicalize the resulting range.
      // Otherwise, we can do the conversion if the size of the range
      // is less than what the precision of the target type can
      // represent.
      if (outer_prec >= inner_prec
	  || wi::rshift (wi::sub (lh_ub, lh_lb),
			 wi::uhwi (outer_prec, inner_prec),
			 inner_sign) == 0)
	{
	  wide_int min = wide_int::from (lh_lb, outer_prec, inner_sign);
	  wide_int max = wide_int::from (lh_ub, outer_prec, inner_sign);
	  if (!wi::eq_p (min, wi::min_value (outer_prec, outer_sign))
	      || !wi::eq_p (max, wi::max_value (outer_prec, outer_sign)))
	    {
	      value_range tmp;
	      create_possibly_reversed_range (tmp, type, min, max);
	      r.union_ (tmp);
	      continue;
	    }
	}
      r = value_range (type);
      break;
    }
  return true;
}

bool
operator_cast::op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const
{
  tree lhs_type = lhs.type ();
  value_range tmp;
  gcc_checking_assert (types_compatible_p (op2.type(), type));

  // If the precision of the LHS is smaller than the precision of the
  // RHS, then there would be truncation of the value on the RHS, and
  // so we can tell nothing about it.
  if (TYPE_PRECISION (lhs_type) < TYPE_PRECISION (type))
    {
      // If we've been passed an actual value for the RHS rather than
      // the type, see if it fits the LHS, and if so, then we can allow
      // it.
      fold_range (r, lhs_type, op2, value_range (lhs_type));
      fold_range (tmp, type, r, value_range (type));
      if (tmp == op2)
        {
	  // We know the value of the RHS fits in the LHS type, so
	  // convert the LHS and remove any values that arent in OP2.
	  fold_range (r, type, lhs, value_range (type));
	  r.intersect (op2);
	  return true;
	}
      // Special case if the LHS is a boolean.  A 0 means the RHS is
      // zero, and a 1 means the RHS is non-zero.
      if (TREE_CODE (lhs_type) == BOOLEAN_TYPE)
	{
	  // If the LHS is unknown, the result is whatever op2 already is.
	  if (!lhs.singleton_p ())
	    {
	      r = op2;
	      return true;
	    }
	  // Boolean casts are weird in GCC. It's actually an implied
	  // mask with 0x01, so all that is known is whether the
	  // rightmost bit is 0 or 1, which implies the only value
	  // *not* in the RHS is 0 or -1.
	  unsigned prec = TYPE_PRECISION (type);
	  if (lhs.zero_p ())
	    r = value_range (type, wi::minus_one (prec), wi::minus_one (prec),
			     VR_ANTI_RANGE);
	  else
	    r = value_range (type, wi::zero (prec), wi::zero (prec),
			     VR_ANTI_RANGE);
	  // And intersect it with what we know about op2.
	  r.intersect (op2);
	}
      else
	// Otherwise we'll have to assume it's whatever we know about op2.
	r = op2;
      return true;
    }

  // If the LHS precision is greater than the rhs precision, the LHS
  // range is restricted to the range of the RHS by this
  // assignment.
  if (TYPE_PRECISION (lhs_type) > TYPE_PRECISION (type))
    {
      // Cast the range of the RHS to the type of the LHS.
      fold_range (tmp, lhs_type, value_range (type), value_range (lhs_type));
      // Intersect this with the LHS range will produce the range, which
      // will be cast to the RHS type before returning.
      tmp.intersect (lhs);
    }
  else
    tmp = lhs;

  // Cast the calculated range to the type of the RHS.
  fold_range (r, type, tmp, value_range (type));
  return true;
}


class operator_logical_and : public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &lh,
			   const value_range &rh) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
} op_logical_and;


bool
operator_logical_and::fold_range (value_range &r, tree type,
				  const value_range &lh,
				  const value_range &rh) const
{
  if (empty_range_check (r, lh, rh))
    return true;

  // 0 && anything is 0.
  if ((wi::eq_p (lh.lower_bound (), 0) && wi::eq_p (lh.upper_bound (), 0))
      || (wi::eq_p (lh.lower_bound (), 0) && wi::eq_p (rh.upper_bound (), 0)))
    r = range_false (type);
  else if (lh.contains_p (build_zero_cst (lh.type ()))
	   || rh.contains_p (build_zero_cst (rh.type ())))
    // To reach this point, there must be a logical 1 on each side, and
    // the only remaining question is whether there is a zero or not.
    r = range_true_and_false (type);
  else
    r = range_true (type);
  return true;
}

bool
operator_logical_and::op1_range (value_range &r, tree type,
				 const value_range &lhs,
				 const value_range &op2 ATTRIBUTE_UNUSED) const
{
   switch (get_bool_state (r, lhs, type))
     {
     case BRS_TRUE:
       // A true result means both sides of the AND must be true.
       r = range_true (type);
       break;
     default:
       // Any other result means only one side has to be false, the
       // other side can be anything. So we cannott be sure of any
       // result here.
       r = range_true_and_false (type);
       break;
     }
  return true;
}

bool
operator_logical_and::op2_range (value_range &r, tree type,
				 const value_range &lhs,
				 const value_range &op1) const
{
  return operator_logical_and::op1_range (r, type, lhs, op1);
}


class operator_bitwise_and : public range_operator
{
public:
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
} op_bitwise_and;

// Optimize BIT_AND_EXPR and BIT_IOR_EXPR in terms of a mask if
// possible.  Basically, see if we can optimize:
//
//	[LB, UB] op Z
//   into:
//	[LB op Z, UB op Z]
//
// If the optimization was successful, accumulate the range in R and
// return TRUE.

static bool
wi_optimize_and_or (value_range &r,
		    enum tree_code code,
		    tree type,
		    const wide_int &lh_lb, const wide_int &lh_ub,
		    const wide_int &rh_lb, const wide_int &rh_ub)
{
  // Calculate the singleton mask among the ranges, if any.
  wide_int lower_bound, upper_bound, mask;
  if (wi::eq_p (rh_lb, rh_ub))
    {
      mask = rh_lb;
      lower_bound = lh_lb;
      upper_bound = lh_ub;
    }
  else if (wi::eq_p (lh_lb, lh_ub))
    {
      mask = lh_lb;
      lower_bound = rh_lb;
      upper_bound = rh_ub;
    }
  else
    return false;

  // If Z is a constant which (for op | its bitwise not) has n
  // consecutive least significant bits cleared followed by m 1
  // consecutive bits set immediately above it and either
  // m + n == precision, or (x >> (m + n)) == (y >> (m + n)).
  //
  // The least significant n bits of all the values in the range are
  // cleared or set, the m bits above it are preserved and any bits
  // above these are required to be the same for all values in the
  // range.
  wide_int w = mask;
  int m = 0, n = 0;
  if (code == BIT_IOR_EXPR)
    w = ~w;
  if (wi::eq_p (w, 0))
    n = w.get_precision ();
  else
    {
      n = wi::ctz (w);
      w = ~(w | wi::mask (n, false, w.get_precision ()));
      if (wi::eq_p (w, 0))
	m = w.get_precision () - n;
      else
	m = wi::ctz (w) - n;
    }
  wide_int new_mask = wi::mask (m + n, true, w.get_precision ());
  if ((new_mask & lower_bound) != (new_mask & upper_bound))
    return false;

  wide_int res_lb, res_ub;
  if (code == BIT_AND_EXPR)
    {
      res_lb = wi::bit_and (lower_bound, mask);
      res_ub = wi::bit_and (upper_bound, mask);
    }
  else if (code == BIT_IOR_EXPR)
    {
      res_lb = wi::bit_or (lower_bound, mask);
      res_ub = wi::bit_or (upper_bound, mask);
    }
  else
    gcc_unreachable ();
  value_range_with_overflow (r, type, res_lb, res_ub);
  return true;
}

// For range [LB, UB] compute two wide_int bit masks.
//
// In the MAYBE_NONZERO bit mask, if some bit is unset, it means that
// for all numbers in the range the bit is 0, otherwise it might be 0
// or 1.
//
// In the MUSTBE_NONZERO bit mask, if some bit is set, it means that
// for all numbers in the range the bit is 1, otherwise it might be 0
// or 1.

void
wi_set_zero_nonzero_bits (tree type,
			  const wide_int &lb, const wide_int &ub,
			  wide_int &maybe_nonzero,
			  wide_int &mustbe_nonzero)
{
  signop sign = TYPE_SIGN (type);

  if (wi::eq_p (lb, ub))
    maybe_nonzero = mustbe_nonzero = lb;
  else if (wi::ge_p (lb, 0, sign) || wi::lt_p (ub, 0, sign))
    {
      wide_int xor_mask = lb ^ ub;
      maybe_nonzero = lb | ub;
      mustbe_nonzero = lb & ub;
      if (xor_mask != 0)
	{
	  wide_int mask = wi::mask (wi::floor_log2 (xor_mask), false,
				    maybe_nonzero.get_precision ());
	  maybe_nonzero = maybe_nonzero | mask;
	  mustbe_nonzero = wi::bit_and_not (mustbe_nonzero, mask);
	}
    }
  else
    {
      maybe_nonzero = wi::minus_one (lb.get_precision ());
      mustbe_nonzero = wi::zero (lb.get_precision ());
    }
}

void
operator_bitwise_and::wi_fold (value_range &r, tree type,
			       const wide_int &lh_lb,
			       const wide_int &lh_ub,
			       const wide_int &rh_lb,
			       const wide_int &rh_ub) const
{
  if (wi_optimize_and_or (r, BIT_AND_EXPR, type, lh_lb, lh_ub, rh_lb, rh_ub))
    return;

  wide_int maybe_nonzero_lh, mustbe_nonzero_lh;
  wide_int maybe_nonzero_rh, mustbe_nonzero_rh;
  wi_set_zero_nonzero_bits (type, lh_lb, lh_ub,
			    maybe_nonzero_lh, mustbe_nonzero_lh);
  wi_set_zero_nonzero_bits (type, rh_lb, rh_ub,
			    maybe_nonzero_rh, mustbe_nonzero_rh);

  wide_int new_lb = mustbe_nonzero_lh & mustbe_nonzero_rh;
  wide_int new_ub = maybe_nonzero_lh & maybe_nonzero_rh;
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);
  // If both input ranges contain only negative values, we can
  // truncate the result range maximum to the minimum of the
  // input range maxima.
  if (wi::lt_p (lh_ub, 0, sign) && wi::lt_p (rh_ub, 0, sign))
    {
      new_ub = wi::min (new_ub, lh_ub, sign);
      new_ub = wi::min (new_ub, rh_ub, sign);
    }
  // If either input range contains only non-negative values
  // we can truncate the result range maximum to the respective
  // maximum of the input range.
  if (wi::ge_p (lh_lb, 0, sign))
    new_ub = wi::min (new_ub, lh_ub, sign);
  if (wi::ge_p (rh_lb, 0, sign))
    new_ub = wi::min (new_ub, rh_ub, sign);
  // PR68217: In case of signed & sign-bit-CST should
  // result in [-INF, 0] instead of [-INF, INF].
  if (wi::gt_p (new_lb, new_ub, sign))
    {
      wide_int sign_bit = wi::set_bit_in_zero (prec - 1, prec);
      if (sign == SIGNED
	  && ((wi::eq_p (lh_lb, lh_ub)
	       && !wi::cmps (lh_lb, sign_bit))
	      || (wi::eq_p (rh_lb, rh_ub)
		  && !wi::cmps (rh_lb, sign_bit))))
	{
	  new_lb = wi::min_value (prec, sign);
	  new_ub = wi::zero (prec);
	}
    }
  // If the limits got swapped around, return varying.
  if (wi::gt_p (new_lb, new_ub,sign))
    r = value_range (type);
  else
    value_range_with_overflow (r, type, new_lb, new_ub);
}

bool
operator_bitwise_and::op1_range (value_range &r, tree type,
				 const value_range &lhs,
				 const value_range &op2) const
{
  // If this is really a logical wi_fold, call that.
  if (types_compatible_p (type, boolean_type_node))
    return op_logical_and.op1_range (r, type, lhs, op2);

  // For now do nothing with bitwise AND of value_range's.
  r.set_varying (type);
  return true;
}

bool
operator_bitwise_and::op2_range (value_range &r, tree type,
				 const value_range &lhs,
				 const value_range &op1) const
{
  return operator_bitwise_and::op1_range (r, type, lhs, op1);
}


class operator_logical_or : public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &lh,
			   const value_range &rh) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
} op_logical_or;

bool
operator_logical_or::fold_range (value_range &r, tree type ATTRIBUTE_UNUSED,
				 const value_range &lh,
				 const value_range &rh) const
{
  if (empty_range_check (r, lh, rh))
    return true;

  r = lh;
  r.union_ (rh);
  return true;
}

bool
operator_logical_or::op1_range (value_range &r, tree type,
				const value_range &lhs,
				const value_range &op2 ATTRIBUTE_UNUSED) const
{
   switch (get_bool_state (r, lhs, type))
     {
     case BRS_FALSE:
       // A false result means both sides of the OR must be false.
       r = range_false (type);
       break;
     default:
       // Any other result means only one side has to be true, the
       // other side can be anything. so we can't be sure of any result
       // here.
       r = range_true_and_false (type);
       break;
    }
  return true;
}

bool
operator_logical_or::op2_range (value_range &r, tree type,
				const value_range &lhs,
				const value_range &op1) const
{
  return operator_logical_or::op1_range (r, type, lhs, op1);
}


class operator_bitwise_or : public range_operator
{
public:
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
  virtual bool op2_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op1) const;
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
} op_bitwise_or;

void
operator_bitwise_or::wi_fold (value_range &r, tree type,
			      const wide_int &lh_lb,
			      const wide_int &lh_ub,
			      const wide_int &rh_lb,
			      const wide_int &rh_ub) const
{
  if (wi_optimize_and_or (r, BIT_IOR_EXPR, type, lh_lb, lh_ub, rh_lb, rh_ub))
    return;

  wide_int maybe_nonzero_lh, mustbe_nonzero_lh;
  wide_int maybe_nonzero_rh, mustbe_nonzero_rh;
  wi_set_zero_nonzero_bits (type, lh_lb, lh_ub,
			    maybe_nonzero_lh, mustbe_nonzero_lh);
  wi_set_zero_nonzero_bits (type, rh_lb, rh_ub,
			    maybe_nonzero_rh, mustbe_nonzero_rh);
  wide_int new_lb = mustbe_nonzero_lh | mustbe_nonzero_rh;
  wide_int new_ub = maybe_nonzero_lh | maybe_nonzero_rh;
  signop sign = TYPE_SIGN (type);
  // If the input ranges contain only positive values we can
  // truncate the minimum of the result range to the maximum
  // of the input range minima.
  if (wi::ge_p (lh_lb, 0, sign)
      && wi::ge_p (rh_lb, 0, sign))
    {
      new_lb = wi::max (new_lb, lh_lb, sign);
      new_lb = wi::max (new_lb, rh_lb, sign);
    }
  // If either input range contains only negative values
  // we can truncate the minimum of the result range to the
  // respective minimum range.
  if (wi::lt_p (lh_ub, 0, sign))
    new_lb = wi::max (new_lb, lh_lb, sign);
  if (wi::lt_p (rh_ub, 0, sign))
    new_lb = wi::max (new_lb, rh_lb, sign);
  // If the limits got swapped around, return varying.
  if (wi::gt_p (new_lb, new_ub,sign))
    r = value_range (type);
  else
    value_range_with_overflow (r, type, new_lb, new_ub);
}

bool
operator_bitwise_or::op1_range (value_range &r, tree type,
				const value_range &lhs,
				const value_range &op2) const
{
  // If this is really a logical wi_fold, call that.
  if (types_compatible_p (type, boolean_type_node))
    return op_logical_or.op1_range (r, type, lhs, op2);

  // For now do nothing with bitwise OR of value_range's.
  r.set_varying (type);
  return true;
}

bool
operator_bitwise_or::op2_range (value_range &r, tree type,
				const value_range &lhs,
				const value_range &op1) const
{
  return operator_bitwise_or::op1_range (r, type, lhs, op1);
}


class operator_bitwise_xor : public range_operator
{
public:
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
} op_bitwise_xor;

void
operator_bitwise_xor::wi_fold (value_range &r, tree type,
			       const wide_int &lh_lb,
			       const wide_int &lh_ub,
			       const wide_int &rh_lb,
			       const wide_int &rh_ub) const
{
  signop sign = TYPE_SIGN (type);
  wide_int maybe_nonzero_lh, mustbe_nonzero_lh;
  wide_int maybe_nonzero_rh, mustbe_nonzero_rh;
  wi_set_zero_nonzero_bits (type, lh_lb, lh_ub,
			    maybe_nonzero_lh, mustbe_nonzero_lh);
  wi_set_zero_nonzero_bits (type, rh_lb, rh_ub,
			    maybe_nonzero_rh, mustbe_nonzero_rh);

  wide_int result_zero_bits = ((mustbe_nonzero_lh & mustbe_nonzero_rh)
			       | ~(maybe_nonzero_lh | maybe_nonzero_rh));
  wide_int result_one_bits
    = (wi::bit_and_not (mustbe_nonzero_lh, maybe_nonzero_rh)
       | wi::bit_and_not (mustbe_nonzero_rh, maybe_nonzero_lh));
  wide_int new_ub = ~result_zero_bits;
  wide_int new_lb = result_one_bits;

  // If the range has all positive or all negative values, the result
  // is better than VARYING.
  if (wi::lt_p (new_lb, 0, sign) || wi::ge_p (new_ub, 0, sign))
    value_range_with_overflow (r, type, new_lb, new_ub);
  else
    r = value_range (type);
}


class operator_trunc_mod : public range_operator
{
public:
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
} op_trunc_mod;

void
operator_trunc_mod::wi_fold (value_range &r, tree type,
			     const wide_int &lh_lb,
			     const wide_int &lh_ub,
			     const wide_int &rh_lb,
			     const wide_int &rh_ub) const
{
  wide_int new_lb, new_ub, tmp;
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);

  // Mod 0 is undefined.  Return undefined.
  if (wi_zero_p (type, rh_lb, rh_ub))
    {
      r = value_range ();
      return;
    }

  // ABS (A % B) < ABS (B) and either 0 <= A % B <= A or A <= A % B <= 0.
  new_ub = rh_ub - 1;
  if (sign == SIGNED)
    {
      tmp = -1 - rh_lb;
      new_ub = wi::smax (new_ub, tmp);
    }

  if (sign == UNSIGNED)
    new_lb = wi::zero (prec);
  else
    {
      new_lb = -new_ub;
      tmp = lh_lb;
      if (wi::gts_p (tmp, 0))
	tmp = wi::zero (prec);
      new_lb = wi::smax (new_lb, tmp);
    }
  tmp = lh_ub;
  if (sign == SIGNED && wi::neg_p (tmp))
    tmp = wi::zero (prec);
  new_ub = wi::min (new_ub, tmp, sign);

  value_range_with_overflow (r, type, new_lb, new_ub);
}


class operator_logical_not : public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &lh,
			   const value_range &rh) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
} op_logical_not;

// Folding a logical NOT, oddly enough, involves doing nothing on the
// forward pass through.  During the initial walk backwards, the
// logical NOT reversed the desired outcome on the way back, so on the
// way forward all we do is pass the range forward.
//
// 	b_2 = x_1 < 20
// 	b_3 = !b_2
// 	if (b_3)
//  to determine the TRUE branch, walking  backward
//       if (b_3)		if ([1,1])
//       b_3 = !b_2		[1,1] = ![0,0]
// 	 b_2 = x_1 < 20		[0,0] = x_1 < 20,   false, so x_1 == [20, 255]
//   which is the result we are looking for.. so.. pass it through.

bool
operator_logical_not::fold_range (value_range &r, tree type,
				  const value_range &lh,
				  const value_range &rh ATTRIBUTE_UNUSED) const
{
  if (empty_range_check (r, lh, rh))
    return true;

  if (lh.varying_p () || lh.undefined_p ())
    r = lh;
  else
    {
      r = lh;
      r.invert ();
    }
  gcc_checking_assert (lh.type() == type);
  return true;
}

bool
operator_logical_not::op1_range (value_range &r,
				 tree type ATTRIBUTE_UNUSED,
				 const value_range &lhs,
				 const value_range &op2 ATTRIBUTE_UNUSED) const
{
  r = lhs;
  if (!lhs.varying_p () && !lhs.undefined_p ())
    r.invert ();
  return true;
}


class operator_bitwise_not : public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &lh,
			   const value_range &rh) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
} op_bitwise_not;

bool
operator_bitwise_not::fold_range (value_range &r, tree type,
				  const value_range &lh,
				  const value_range &rh) const
{
  if (empty_range_check (r, lh, rh))
    return true;

  // ~X is simply -1 - X.
  value_range minusone (type, wi::minus_one (TYPE_PRECISION (type)),
			wi::minus_one (TYPE_PRECISION (type)));
  return range_op_handler (MINUS_EXPR, type)->fold_range (r, type, minusone,
							  lh);
}

bool
operator_bitwise_not::op1_range (value_range &r, tree type,
				 const value_range &lhs,
				 const value_range &op2) const
{
  // ~X is -1 - X and since bitwise NOT is involutary...do it again.
  return fold_range (r, type, lhs, op2);
}


class operator_cst : public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
} op_integer_cst;

bool
operator_cst::fold_range (value_range &r, tree type ATTRIBUTE_UNUSED,
			  const value_range &lh,
			  const value_range &rh ATTRIBUTE_UNUSED) const
{
  r = lh;
  return true;
}


class operator_identity : public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
} op_identity;

bool
operator_identity::fold_range (value_range &r, tree type ATTRIBUTE_UNUSED,
			       const value_range &lh,
			       const value_range &rh ATTRIBUTE_UNUSED) const
{
  r = lh;
  return true;
}

bool
operator_identity::op1_range (value_range &r, tree type ATTRIBUTE_UNUSED,
			      const value_range &lhs,
			      const value_range &op2 ATTRIBUTE_UNUSED) const
{
  r = lhs;
  return true;
}


class operator_abs : public range_operator
{
 public:
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
} op_abs;

void
operator_abs::wi_fold (value_range &r, tree type,
		       const wide_int &lh_lb, const wide_int &lh_ub,
		       const wide_int &rh_lb ATTRIBUTE_UNUSED,
		       const wide_int &rh_ub ATTRIBUTE_UNUSED) const
{
  wide_int min, max;
  signop sign = TYPE_SIGN (type);
  unsigned prec = TYPE_PRECISION (type);

  // Pass through LH for the easy cases.
  if (sign == UNSIGNED || wi::ge_p (lh_lb, 0, sign))
    {
      r = value_range (type, lh_lb, lh_ub);
      return;
    }

  // -TYPE_MIN_VALUE = TYPE_MIN_VALUE with flag_wrapv so we can't get
  // a useful range.
  wide_int min_value = wi::min_value (prec, sign);
  wide_int max_value = wi::max_value (prec, sign);
  if (!TYPE_OVERFLOW_UNDEFINED (type) && wi::eq_p (lh_lb, min_value))
    {
      r = value_range (type);
      return;
    }

  // ABS_EXPR may flip the range around, if the original range
  // included negative values.
  if (wi::eq_p (lh_lb, min_value))
    min = max_value;
  else
    min = wi::abs (lh_lb);
  if (wi::eq_p (lh_ub, min_value))
    max = max_value;
  else
    max = wi::abs (lh_ub);

  // If the range contains zero then we know that the minimum value in the
  // range will be zero.
  if (wi::le_p (lh_lb, 0, sign) && wi::ge_p (lh_ub, 0, sign))
    {
      if (wi::gt_p (min, max, sign))
	max = min;
      min = wi::zero (prec);
    }
  else
    {
      // If the range was reversed, swap MIN and MAX.
      if (wi::gt_p (min, max, sign))
	std::swap (min, max);
    }

  // If the new range has its limits swapped around (MIN > MAX), then
  // the operation caused one of them to wrap around.  The only thing
  // we know is that the result is positive.
  if (wi::gt_p (min, max, sign))
    {
      min = wi::zero (prec);
      max = max_value;
    }
  r = value_range (type, min, max);
}

bool
operator_abs::op1_range (value_range &r, tree type,
			 const value_range &lhs,
			 const value_range &op2) const
{
  if (empty_range_check (r, lhs, op2))
    return true;
  if (TYPE_UNSIGNED (type))
    {
      r = lhs;
      return true;
    }
  // Start with the positives because negatives are an impossible result.
  value_range positives = range_positives (type);
  positives.intersect (lhs);
  r = positives;
  // Then add the negative of each pair:
  // ABS(op1) = [5,20] would yield op1 => [-20,-5][5,20].
  for (unsigned i = 0; i < positives.num_pairs (); ++i)
    r.union_ (value_range (type,
			   -positives.upper_bound (i),
			   -positives.lower_bound (i)));
  return true;
}


class operator_absu : public range_operator
{
 public:
  virtual void wi_fold (value_range &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const;
} op_absu;

void
operator_absu::wi_fold (value_range &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb ATTRIBUTE_UNUSED,
			const wide_int &rh_ub ATTRIBUTE_UNUSED) const
{
  wide_int new_lb, new_ub;

  // Pass through VR0 the easy cases.
  if (wi::ges_p (lh_lb, 0))
    {
      new_lb = lh_lb;
      new_ub = lh_ub;
    }
  else
    {
      new_lb = wi::abs (lh_lb);
      new_ub = wi::abs (lh_ub);

      // If the range contains zero then we know that the minimum
      // value in the range will be zero.
      if (wi::ges_p (lh_ub, 0))
	{
	  if (wi::gtu_p (new_lb, new_ub))
	    new_ub = new_lb;
	  new_lb = wi::zero (TYPE_PRECISION (type));
	}
      else
	std::swap (new_lb, new_ub);
    }

  gcc_checking_assert (TYPE_UNSIGNED (type));
  r = value_range (type, new_lb, new_ub);
}


class operator_negate : public range_operator
{
 public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
} op_negate;

bool
operator_negate::fold_range (value_range &r, tree type,
			     const value_range &lh,
			     const value_range &rh) const
{
  if (empty_range_check (r, lh, rh))
    return true;
  // -X is simply 0 - X.
  return range_op_handler (MINUS_EXPR, type)->fold_range (r, type,
							  range_zero (type),
							  lh);
}

bool
operator_negate::op1_range (value_range &r, tree type,
			    const value_range &lhs,
			    const value_range &op2) const
{
  // NEGATE is involutory.
  return fold_range (r, type, lhs, op2);
}


class operator_addr_expr : public range_operator
{
public:
  virtual bool fold_range (value_range &r, tree type,
			   const value_range &op1,
			   const value_range &op2) const;
  virtual bool op1_range (value_range &r, tree type,
			  const value_range &lhs,
			  const value_range &op2) const;
} op_addr;

bool
operator_addr_expr::fold_range (value_range &r, tree type,
				const value_range &lh,
				const value_range &rh) const
{
  if (empty_range_check (r, lh, rh))
    return true;

  // Return a non-null pointer of the LHS type (passed in op2).
  if (lh.zero_p ())
    r = range_zero (type);
  else if (!lh.contains_p (build_zero_cst (lh.type ())))
    r = range_nonzero (type);
  else
    r = value_range (type);
  return true;
}

bool
operator_addr_expr::op1_range (value_range &r, tree type,
			       const value_range &lhs,
			       const value_range &op2) const
{
  return operator_addr_expr::fold_range (r, type, lhs, op2);
}


class pointer_plus_operator : public range_operator
{
public:
  virtual void wi_fold (value_range &r, tree type,
		        const wide_int &lh_lb,
		        const wide_int &lh_ub,
		        const wide_int &rh_lb,
		        const wide_int &rh_ub) const;
} op_pointer_plus;

void
pointer_plus_operator::wi_fold (value_range &r, tree type,
				const wide_int &lh_lb,
				const wide_int &lh_ub,
				const wide_int &rh_lb,
				const wide_int &rh_ub) const
{
  // For pointer types, we are really only interested in asserting
  // whether the expression evaluates to non-NULL.
  //
  // With -fno-delete-null-pointer-checks we need to be more
  // conservative.  As some object might reside at address 0,
  // then some offset could be added to it and the same offset
  // subtracted again and the result would be NULL.
  // E.g.
  // static int a[12]; where &a[0] is NULL and
  // ptr = &a[6];
  // ptr -= 6;
  // ptr will be NULL here, even when there is POINTER_PLUS_EXPR
  // where the first range doesn't include zero and the second one
  // doesn't either.  As the second operand is sizetype (unsigned),
  // consider all ranges where the MSB could be set as possible
  // subtractions where the result might be NULL.
  if ((!wi_includes_zero_p (type, lh_lb, lh_ub)
       || !wi_includes_zero_p (type, rh_lb, rh_ub))
      && !TYPE_OVERFLOW_WRAPS (type)
      && (flag_delete_null_pointer_checks
	  || !wi::sign_mask (rh_ub)))
    r = range_nonzero (type);
  else if (lh_lb == lh_ub && lh_lb == 0
	   && rh_lb == rh_ub && rh_lb == 0)
    r = range_zero (type);
  else
   r = value_range (type);
}


class pointer_min_max_operator : public range_operator
{
public:
  virtual void wi_fold (value_range & r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const;
} op_ptr_min_max;

void
pointer_min_max_operator::wi_fold (value_range &r, tree type,
				   const wide_int &lh_lb,
				   const wide_int &lh_ub,
				   const wide_int &rh_lb,
				   const wide_int &rh_ub) const
{
  // For MIN/MAX expressions with pointers, we only care about
  // nullness.  If both are non null, then the result is nonnull.
  // If both are null, then the result is null.  Otherwise they
  // are varying.
  if (!wi_includes_zero_p (type, lh_lb, lh_ub)
      && !wi_includes_zero_p (type, rh_lb, rh_ub))
    r = range_nonzero (type);
  else if (wi_zero_p (type, lh_lb, lh_ub) && wi_zero_p (type, rh_lb, rh_ub))
    r = range_zero (type);
  else
    r = value_range (type);
}


class pointer_and_operator : public range_operator
{
public:
  virtual void wi_fold (value_range &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const;
} op_pointer_and;

void
pointer_and_operator::wi_fold (value_range &r, tree type,
			       const wide_int &lh_lb,
			       const wide_int &lh_ub,
			       const wide_int &rh_lb ATTRIBUTE_UNUSED,
			       const wide_int &rh_ub ATTRIBUTE_UNUSED) const
{
  // For pointer types, we are really only interested in asserting
  // whether the expression evaluates to non-NULL.
  if (wi_zero_p (type, lh_lb, lh_ub) || wi_zero_p (type, lh_lb, lh_ub))
    r = range_zero (type);
  else 
    r = value_range (type);
}


class pointer_or_operator : public range_operator
{
public:
  virtual void wi_fold (value_range &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const;
} op_pointer_or;

void
pointer_or_operator::wi_fold (value_range &r, tree type,
			      const wide_int &lh_lb,
			      const wide_int &lh_ub,
			      const wide_int &rh_lb,
			      const wide_int &rh_ub) const
{
  // For pointer types, we are really only interested in asserting
  // whether the expression evaluates to non-NULL.
  if (!wi_includes_zero_p (type, lh_lb, lh_ub)
      && !wi_includes_zero_p (type, rh_lb, rh_ub))
    r = range_nonzero (type);
  else if (wi_zero_p (type, lh_lb, lh_ub) && wi_zero_p (type, rh_lb, rh_ub))
    r = range_zero (type);
  else
    r = value_range (type);
}

// This implements the range operator tables as local objects in this file.

class range_op_table
{
public:
  inline range_operator *operator[] (enum tree_code code);
protected:
  void set (enum tree_code code, range_operator &op);
private:
  range_operator *m_range_tree[MAX_TREE_CODES];
};

// Return a pointer to the range_operator instance, if there is one
// associated with tree_code CODE.

range_operator *
range_op_table::operator[] (enum tree_code code)
{
  gcc_checking_assert (code > 0 && code < MAX_TREE_CODES);
  return m_range_tree[code];
}

// Add OP to the handler table for CODE.

void
range_op_table::set (enum tree_code code, range_operator &op)
{
  gcc_checking_assert (m_range_tree[code] == NULL);
  m_range_tree[code] = &op;
}

// Instantiate a range op table for integral operations.

class integral_table : public range_op_table
{
public:
  integral_table ();
} integral_tree_table;

integral_table::integral_table ()
{
  set (EQ_EXPR, op_equal);
  set (NE_EXPR, op_not_equal);
  set (LT_EXPR, op_lt);
  set (LE_EXPR, op_le);
  set (GT_EXPR, op_gt);
  set (GE_EXPR, op_ge);
  set (PLUS_EXPR, op_plus);
  set (MINUS_EXPR, op_minus);
  set (MIN_EXPR, op_min);
  set (MAX_EXPR, op_max);
  set (MULT_EXPR, op_mult);
  set (TRUNC_DIV_EXPR, op_trunc_div);
  set (FLOOR_DIV_EXPR, op_floor_div);
  set (ROUND_DIV_EXPR, op_round_div);
  set (CEIL_DIV_EXPR, op_ceil_div);
  set (EXACT_DIV_EXPR, op_exact_div);
  set (LSHIFT_EXPR, op_lshift);
  set (RSHIFT_EXPR, op_rshift);
  set (NOP_EXPR, op_convert);
  set (CONVERT_EXPR, op_convert);
  set (TRUTH_AND_EXPR, op_logical_and);
  set (BIT_AND_EXPR, op_bitwise_and);
  set (TRUTH_OR_EXPR, op_logical_or);
  set (BIT_IOR_EXPR, op_bitwise_or);
  set (BIT_XOR_EXPR, op_bitwise_xor);
  set (TRUNC_MOD_EXPR, op_trunc_mod);
  set (TRUTH_NOT_EXPR, op_logical_not);
  set (BIT_NOT_EXPR, op_bitwise_not);
  set (INTEGER_CST, op_integer_cst);
  set (SSA_NAME, op_identity);
  set (PAREN_EXPR, op_identity);
  set (OBJ_TYPE_REF, op_identity);
  set (ABS_EXPR, op_abs);
  set (ABSU_EXPR, op_absu);
  set (NEGATE_EXPR, op_negate);
  set (ADDR_EXPR, op_addr);
}

// Instantiate a range op table for pointer operations.

class pointer_table : public range_op_table
{
public:
  pointer_table ();
} pointer_tree_table;

pointer_table::pointer_table ()
{
  set (BIT_AND_EXPR, op_pointer_and);
  set (BIT_IOR_EXPR, op_pointer_or);
  set (MIN_EXPR, op_ptr_min_max);
  set (MAX_EXPR, op_ptr_min_max);
  set (POINTER_PLUS_EXPR, op_pointer_plus);

  set (EQ_EXPR, op_equal);
  set (NE_EXPR, op_not_equal);
  set (LT_EXPR, op_lt);
  set (LE_EXPR, op_le);
  set (GT_EXPR, op_gt);
  set (GE_EXPR, op_ge);
  set (SSA_NAME, op_identity);
  set (ADDR_EXPR, op_addr);
  set (NOP_EXPR, op_convert);
  set (CONVERT_EXPR, op_convert);

  set (BIT_NOT_EXPR, op_bitwise_not);
  set (BIT_XOR_EXPR, op_bitwise_xor);
}

// The tables are hidden and accessed via a simple extern function.

range_operator *
range_op_handler (enum tree_code code, tree type)
{
  // First check if there is apointer specialization.
  if (POINTER_TYPE_P (type))
    return pointer_tree_table[code];
  return integral_tree_table[code];
}

// Cast the range in R to TYPE.

void
range_cast (value_range &r, tree type)
{
  value_range tmp = r;
  range_operator *op = range_op_handler (CONVERT_EXPR, type);
  // Call op_convert, if it fails, the result is varying.
  if (!op->fold_range (r, type, tmp, value_range (type)))
    r = value_range (type);
}

#if CHECKING_P
#include "selftest.h"
#include "stor-layout.h"

namespace selftest
{
#define INT(N) build_int_cst (integer_type_node, (N))
#define UINT(N) build_int_cstu (unsigned_type_node, (N))
#define INT16(N) build_int_cst (short_integer_type_node, (N))
#define UINT16(N) build_int_cstu (short_unsigned_type_node, (N))
#define INT64(N) build_int_cstu (long_long_integer_type_node, (N))
#define UINT64(N) build_int_cstu (long_long_unsigned_type_node, (N))
#define UINT128(N) build_int_cstu (u128_type, (N))
#define UCHAR(N) build_int_cstu (unsigned_char_type_node, (N))
#define SCHAR(N) build_int_cst (signed_char_type_node, (N))

// Run all of the selftests within this file.

void
range_tests ()
{
  tree u128_type = build_nonstandard_integer_type (128, /*unsigned=*/1);
  value_range i1, i2, i3;
  value_range r0, r1, rold;

  // Test that NOT(255) is [0..254] in 8-bit land.
  value_range not_255 (UCHAR (255), UCHAR (255), VR_ANTI_RANGE);
  ASSERT_TRUE (not_255 == value_range (UCHAR (0), UCHAR (254)));

  // Test that NOT(0) is [1..255] in 8-bit land.
  value_range not_zero = range_nonzero (unsigned_char_type_node);
  ASSERT_TRUE (not_zero == value_range (UCHAR (1), UCHAR (255)));

  // Check that [0,127][0x..ffffff80,0x..ffffff]
  //  => ~[128, 0x..ffffff7f].
  r0 = value_range (UINT128 (0), UINT128 (127));
  tree high = build_minus_one_cst (u128_type);
  // low = -1 - 127 => 0x..ffffff80.
  tree low = fold_build2 (MINUS_EXPR, u128_type, high, UINT128(127));
  r1 = value_range (low, high); // [0x..ffffff80, 0x..ffffffff]
  // r0 = [0,127][0x..ffffff80,0x..fffffff].
  r0.union_ (r1);
  // r1 = [128, 0x..ffffff7f].
  r1 = value_range (UINT128(128),
			 fold_build2 (MINUS_EXPR, u128_type,
				      build_minus_one_cst (u128_type),
				      UINT128(128)));
  r0.invert ();
  ASSERT_TRUE (r0 == r1);

  r0.set_varying (integer_type_node);
  tree minint = wide_int_to_tree (integer_type_node, r0.lower_bound ());
  tree maxint = wide_int_to_tree (integer_type_node, r0.upper_bound ());

  r0.set_varying (short_integer_type_node);
  tree minshort = wide_int_to_tree (short_integer_type_node, r0.lower_bound ());
  tree maxshort = wide_int_to_tree (short_integer_type_node, r0.upper_bound ());

  r0.set_varying (unsigned_type_node);
  tree maxuint = wide_int_to_tree (unsigned_type_node, r0.upper_bound ());

  // Check that ~[0,5] => [6,MAX] for unsigned int.
  r0 = value_range (UINT (0), UINT (5));
  r0.invert ();
  ASSERT_TRUE (r0 == value_range (UINT(6), maxuint));

  // Check that ~[10,MAX] => [0,9] for unsigned int.
  r0 = value_range (UINT(10), maxuint);
  r0.invert ();
  ASSERT_TRUE (r0 == value_range (UINT (0), UINT (9)));

  // Check that ~[0,5] => [6,MAX] for unsigned 128-bit numbers.
  r0 = value_range (UINT128 (0), UINT128 (5), VR_ANTI_RANGE);
  r1 = value_range (UINT128(6), build_minus_one_cst (u128_type));
  ASSERT_TRUE (r0 == r1);

  // Check that [~5] is really [-MIN,4][6,MAX].
  r0 = value_range (INT (5), INT (5), VR_ANTI_RANGE);
  r1 = value_range (minint, INT (4));
  r1.union_ (value_range (INT (6), maxint));
  ASSERT_FALSE (r1.undefined_p ());
  ASSERT_TRUE (r0 == r1);

  r1 = value_range (INT (5), INT (5));
  value_range r2 (r1);
  ASSERT_TRUE (r1 == r2);

  r1 = value_range (INT (5), INT (10));

  r1 = value_range (integer_type_node,
	       wi::to_wide (INT (5)), wi::to_wide (INT (10)));
  ASSERT_TRUE (r1.contains_p (INT (7)));

  r1 = value_range (SCHAR (0), SCHAR (20));
  ASSERT_TRUE (r1.contains_p (SCHAR(15)));
  ASSERT_FALSE (r1.contains_p (SCHAR(300)));

  // If a range is in any way outside of the range for the converted
  // to range, default to the range for the new type.
  if (TYPE_PRECISION (TREE_TYPE (maxint))
      > TYPE_PRECISION (short_integer_type_node))
    {
      r1 = value_range (integer_zero_node, maxint);
      range_cast (r1, short_integer_type_node);
      ASSERT_TRUE (r1.lower_bound () == wi::to_wide (minshort)
		   && r1.upper_bound() == wi::to_wide (maxshort));
    }

  // (unsigned char)[-5,-1] => [251,255].
  r0 = rold = value_range (SCHAR (-5), SCHAR (-1));
  range_cast (r0, unsigned_char_type_node);
  ASSERT_TRUE (r0 == value_range (UCHAR (251), UCHAR (255)));
  range_cast (r0, signed_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // (signed char)[15, 150] => [-128,-106][15,127].
  r0 = rold = value_range (UCHAR (15), UCHAR (150));
  range_cast (r0, signed_char_type_node);
  r1 = value_range (SCHAR (15), SCHAR (127));
  r2 = value_range (SCHAR (-128), SCHAR (-106));
  r1.union_ (r2);
  ASSERT_TRUE (r1 == r0);
  range_cast (r0, unsigned_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // (unsigned char)[-5, 5] => [0,5][251,255].
  r0 = rold = value_range (SCHAR (-5), SCHAR (5));
  range_cast (r0, unsigned_char_type_node);
  r1 = value_range (UCHAR (251), UCHAR (255));
  r2 = value_range (UCHAR (0), UCHAR (5));
  r1.union_ (r2);
  ASSERT_TRUE (r0 == r1);
  range_cast (r0, signed_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // (unsigned char)[-5,5] => [0,5][251,255].
  r0 = value_range (INT (-5), INT (5));
  range_cast (r0, unsigned_char_type_node);
  r1 = value_range (UCHAR (0), UCHAR (5));
  r1.union_ (value_range (UCHAR (251), UCHAR (255)));
  ASSERT_TRUE (r0 == r1);

  // (unsigned char)[5U,1974U] => [0,255].
  r0 = value_range (UINT (5), UINT (1974));
  range_cast (r0, unsigned_char_type_node);
  ASSERT_TRUE (r0 == value_range (UCHAR (0), UCHAR (255)));
  range_cast (r0, integer_type_node);
  // Going to a wider range should not sign extend.
  ASSERT_TRUE (r0 == value_range (INT (0), INT (255)));

  // (unsigned char)[-350,15] => [0,255].
  r0 = value_range (INT (-350), INT (15));
  range_cast (r0, unsigned_char_type_node);
  ASSERT_TRUE (r0 == (value_range
		      (TYPE_MIN_VALUE (unsigned_char_type_node),
		       TYPE_MAX_VALUE (unsigned_char_type_node))));

  // Casting [-120,20] from signed char to unsigned short.
  // => [0, 20][0xff88, 0xffff].
  r0 = value_range (SCHAR (-120), SCHAR (20));
  range_cast (r0, short_unsigned_type_node);
  r1 = value_range (UINT16 (0), UINT16 (20));
  r2 = value_range (UINT16 (0xff88), UINT16 (0xffff));
  r1.union_ (r2);
  ASSERT_TRUE (r0 == r1);
  // A truncating cast back to signed char will work because [-120, 20]
  // is representable in signed char.
  range_cast (r0, signed_char_type_node);
  ASSERT_TRUE (r0 == value_range (SCHAR (-120), SCHAR (20)));

  // unsigned char -> signed short
  //	(signed short)[(unsigned char)25, (unsigned char)250]
  // => [(signed short)25, (signed short)250]
  r0 = rold = value_range (UCHAR (25), UCHAR (250));
  range_cast (r0, short_integer_type_node);
  r1 = value_range (INT16 (25), INT16 (250));
  ASSERT_TRUE (r0 == r1);
  range_cast (r0, unsigned_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // Test casting a wider signed [-MIN,MAX] to a nar`rower unsigned.
  r0 = value_range (TYPE_MIN_VALUE (long_long_integer_type_node),
	       TYPE_MAX_VALUE (long_long_integer_type_node));
  range_cast (r0, short_unsigned_type_node);
  r1 = value_range (TYPE_MIN_VALUE (short_unsigned_type_node),
	       TYPE_MAX_VALUE (short_unsigned_type_node));
  ASSERT_TRUE (r0 == r1);

  // NOT([10,20]) ==> [-MIN,9][21,MAX].
  r0 = r1 = value_range (INT (10), INT (20));
  r2 = value_range (minint, INT(9));
  r2.union_ (value_range (INT(21), maxint));
  ASSERT_FALSE (r2.undefined_p ());
  r1.invert ();
  ASSERT_TRUE (r1 == r2);
  // Test that NOT(NOT(x)) == x.
  r2.invert ();
  ASSERT_TRUE (r0 == r2);

  // Test that booleans and their inverse work as expected.
  r0 = range_zero (boolean_type_node);
  ASSERT_TRUE (r0 == value_range (build_zero_cst (boolean_type_node),
				       build_zero_cst (boolean_type_node)));
  r0.invert ();
  ASSERT_TRUE (r0 == value_range (build_one_cst (boolean_type_node),
				       build_one_cst (boolean_type_node)));

  // Casting NONZERO to a narrower type will wrap/overflow so
  // it's just the entire range for the narrower type.
  //
  // "NOT 0 at signed 32-bits" ==> [-MIN_32,-1][1, +MAX_32].  This is
  // is outside of the range of a smaller range, return the full
  // smaller range.
  if (TYPE_PRECISION (integer_type_node)
      > TYPE_PRECISION (short_integer_type_node))
    {
      r0 = range_nonzero (integer_type_node);
      range_cast (r0, short_integer_type_node);
      r1 = value_range (TYPE_MIN_VALUE (short_integer_type_node),
			     TYPE_MAX_VALUE (short_integer_type_node));
      ASSERT_TRUE (r0 == r1);
    }

  // Casting NONZERO from a narrower signed to a wider signed.
  //
  // NONZERO signed 16-bits is [-MIN_16,-1][1, +MAX_16].
  // Converting this to 32-bits signed is [-MIN_16,-1][1, +MAX_16].
  r0 = range_nonzero (short_integer_type_node);
  range_cast (r0, integer_type_node);
  r1 = value_range (INT (-32768), INT (-1));
  r2 = value_range (INT (1), INT (32767));
  r1.union_ (r2);
  ASSERT_TRUE (r0 == r1);

  // Make sure NULL and non-NULL of pointer types work, and that
  // inverses of them are consistent.
  tree voidp = build_pointer_type (void_type_node);
  r0 = range_zero (voidp);
  r1 = r0;
  r0.invert ();
  r0.invert ();
  ASSERT_TRUE (r0 == r1);

  // [10,20] U [15, 30] => [10, 30].
  r0 = value_range (INT (10), INT (20));
  r1 = value_range (INT (15), INT (30));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == value_range (INT (10), INT (30)));

  // [15,40] U [] => [15,40].
  r0 = value_range (INT (15), INT (40));
  r1.set_undefined ();
  r0.union_ (r1);
  ASSERT_TRUE (r0 == value_range (INT (15), INT (40)));

  // [10,20] U [10,10] => [10,20].
  r0 = value_range (INT (10), INT (20));
  r1 = value_range (INT (10), INT (10));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == value_range (INT (10), INT (20)));

  // [10,20] U [9,9] => [9,20].
  r0 = value_range (INT (10), INT (20));
  r1 = value_range (INT (9), INT (9));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == value_range (INT (9), INT (20)));

  // [10,20] ^ [15,30] => [15,20].
  r0 = value_range (INT (10), INT (20));
  r1 = value_range (INT (15), INT (30));
  r0.intersect (r1);
  ASSERT_TRUE (r0 == value_range (INT (15), INT (20)));

  // Test the internal sanity of wide_int's wrt HWIs.
  ASSERT_TRUE (wi::max_value (TYPE_PRECISION (boolean_type_node),
			      TYPE_SIGN (boolean_type_node))
	       == wi::uhwi (1, TYPE_PRECISION (boolean_type_node)));

  // Test zero_p().
  r0 = value_range (INT (0), INT (0));
  ASSERT_TRUE (r0.zero_p ());

  // Test nonzero_p().
  r0 = value_range (INT (0), INT (0));
  r0.invert ();
  ASSERT_TRUE (r0.nonzero_p ());
}

} // namespace selftest

#endif // CHECKING_P
