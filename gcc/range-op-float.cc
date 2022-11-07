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
range_operator_float::fold_range (frange &r ATTRIBUTE_UNUSED,
				  tree type ATTRIBUTE_UNUSED,
				  const frange &lh ATTRIBUTE_UNUSED,
				  const frange &rh ATTRIBUTE_UNUSED,
				  relation_trio) const
{
  return false;
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
  // We only support closed intervals.
  REAL_VALUE_TYPE ninf = frange_val_min (type);
  r.set (type, ninf, val.upper_bound ());
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

  // We only support closed intervals.
  REAL_VALUE_TYPE inf = frange_val_max (type);
  r.set (type, val.lower_bound (), inf);
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
  r.union_ (frange (type,
		    real_value_negate (&positives.upper_bound ()),
		    real_value_negate (&positives.lower_bound ())));
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
}

} // namespace selftest

#endif // CHECKING_P
