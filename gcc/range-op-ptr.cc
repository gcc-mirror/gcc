/* Code for range operators.
   Copyright (C) 2017-2025 Free Software Foundation, Inc.
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
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "value-relation.h"
#include "range-op.h"
#include "tree-ssa-ccp.h"
#include "range-op-mixed.h"

bool
range_operator::fold_range (prange &, tree, const prange &, const prange &,
			    relation_trio) const
{
  return false;
}

bool
range_operator::fold_range (prange &, tree, const prange &, const irange &,
			    relation_trio) const
{
  return false;
}

bool
range_operator::fold_range (irange &, tree, const prange &, const prange &,
			    relation_trio) const
{
  return false;
}

bool
range_operator::fold_range (prange &, tree, const irange &, const prange &,
			    relation_trio) const
{
  return false;
}

bool
range_operator::fold_range (irange &, tree, const prange &, const irange &,
			    relation_trio) const
{
  return false;
}

bool
range_operator::op1_op2_relation_effect (prange &, tree,
					 const prange &,
					 const prange &,
					 relation_kind) const
{
  return false;
}

bool
range_operator::op1_op2_relation_effect (prange &, tree,
					 const prange &,
					 const irange &,
					 relation_kind) const
{
  return false;
}

bool
range_operator::op1_op2_relation_effect (irange &, tree,
					 const prange &,
					 const prange &,
					 relation_kind) const
{
  return false;
}

bool
range_operator::op1_op2_relation_effect (prange &, tree,
					 const irange &,
					 const prange &,
					 relation_kind) const
{
  return false;
}

bool
range_operator::op1_op2_relation_effect (irange &, tree,
					 const prange &,
					 const irange &,
					 relation_kind) const
{
  return false;
}

bool
range_operator::op1_range (prange &, tree,
			   const prange &lhs ATTRIBUTE_UNUSED,
			   const prange &op2 ATTRIBUTE_UNUSED,
			   relation_trio) const
{
  return false;
}

bool
range_operator::op1_range (prange &, tree,
			   const irange &lhs ATTRIBUTE_UNUSED,
			   const prange &op2 ATTRIBUTE_UNUSED,
			   relation_trio) const
{
  return false;
}

bool
range_operator::op1_range (prange &, tree,
			   const prange &lhs ATTRIBUTE_UNUSED,
			   const irange &op2 ATTRIBUTE_UNUSED,
			   relation_trio) const
{
  return false;
}

bool
range_operator::op1_range (irange &, tree,
			   const prange &lhs ATTRIBUTE_UNUSED,
			   const irange &op2 ATTRIBUTE_UNUSED,
			   relation_trio) const
{
  return false;
}

bool
range_operator::op2_range (prange &, tree,
			   const irange &lhs ATTRIBUTE_UNUSED,
			   const prange &op1 ATTRIBUTE_UNUSED,
			   relation_trio) const
{
  return false;
}

bool
range_operator::op2_range (irange &, tree,
			   const prange &lhs ATTRIBUTE_UNUSED,
			   const prange &op1 ATTRIBUTE_UNUSED,
			   relation_trio) const
{
  return false;
}

relation_kind
range_operator::op1_op2_relation (const irange &lhs ATTRIBUTE_UNUSED,
				  const prange &op1 ATTRIBUTE_UNUSED,
				  const prange &op2 ATTRIBUTE_UNUSED) const
{
  return VREL_VARYING;
}

relation_kind
range_operator::lhs_op1_relation (const prange &lhs ATTRIBUTE_UNUSED,
				  const irange &op1 ATTRIBUTE_UNUSED,
				  const irange &op2 ATTRIBUTE_UNUSED,
				  relation_kind rel ATTRIBUTE_UNUSED) const
{
  return VREL_VARYING;
}

relation_kind
range_operator::lhs_op1_relation (const irange &lhs ATTRIBUTE_UNUSED,
				  const prange &op1 ATTRIBUTE_UNUSED,
				  const prange &op2 ATTRIBUTE_UNUSED,
				  relation_kind rel ATTRIBUTE_UNUSED) const
{
  return VREL_VARYING;
}

relation_kind
range_operator::lhs_op1_relation (const prange &lhs ATTRIBUTE_UNUSED,
				  const prange &op1 ATTRIBUTE_UNUSED,
				  const prange &op2 ATTRIBUTE_UNUSED,
				  relation_kind rel ATTRIBUTE_UNUSED) const
{
  return VREL_VARYING;
}

void
range_operator::update_bitmask (irange &,
				const prange &,
				const prange &) const
{
}

// Return the upper limit for a type.

static inline wide_int
max_limit (const_tree type)
{
  return wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
}

// Return the lower limit for a type.

static inline wide_int
min_limit (const_tree type)
{
  return wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
}

// Build a range that is < VAL and store it in R.

static void
build_lt (prange &r, tree type, const prange &val)
{
  wi::overflow_type ov;
  wide_int lim = wi::sub (val.upper_bound (), 1, UNSIGNED, &ov);

  // If val - 1 underflows, check if X < MIN, which is an empty range.
  if (ov)
    r.set_undefined ();
  else
    r.set (type, min_limit (type), lim);
}

// Build a range that is <= VAL and store it in R.

static void
build_le (prange &r, tree type, const prange &val)
{
  r.set (type, min_limit (type), val.upper_bound ());
}

// Build a range that is > VAL and store it in R.

static void
build_gt (prange &r, tree type, const prange &val)
{
  wi::overflow_type ov;
  wide_int lim = wi::add (val.lower_bound (), 1, UNSIGNED, &ov);

  // If val + 1 overflows, check is for X > MAX, which is an empty range.
  if (ov)
    r.set_undefined ();
  else
    r.set (type, lim, max_limit (type));

}

// Build a range that is >= VAL and store it in R.

static void
build_ge (prange &r, tree type, const prange &val)
{
  r.set (type, val.lower_bound (), max_limit (type));
}

class pointer_plus_operator : public range_operator
{
  using range_operator::update_bitmask;
  using range_operator::fold_range;
  using range_operator::op2_range;
public:
  virtual bool fold_range (prange &r, tree type,
			   const prange &op1,
			   const irange &op2,
			   relation_trio) const final override;
  virtual bool op2_range (irange &r, tree type,
			  const prange &lhs,
			  const prange &op1,
			  relation_trio = TRIO_VARYING) const final override;
  void update_bitmask (prange &r, const prange &lh, const irange &rh) const
    { update_known_bitmask (r, POINTER_PLUS_EXPR, lh, rh); }
} op_pointer_plus;

bool
pointer_plus_operator::fold_range (prange &r, tree type,
				   const prange &op1,
				   const irange &op2,
				   relation_trio) const
{
  if (empty_range_varying (r, type, op1, op2))
    return true;

  const wide_int lh_lb = op1.lower_bound ();
  const wide_int lh_ub = op1.upper_bound ();
  const wide_int rh_lb = op2.lower_bound ();
  const wide_int rh_ub = op2.upper_bound ();

  // Check for [0,0] + const, and simply return the const.
  if (lh_lb == 0 && lh_ub == 0 && rh_lb == rh_ub)
    {
      r.set (type, rh_lb, rh_lb);
      return true;
    }

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
    r.set_nonzero (type);
  else if (lh_lb == lh_ub && lh_lb == 0
	   && rh_lb == rh_ub && rh_lb == 0)
    r.set_zero (type);
  else
   r.set_varying (type);

  update_known_bitmask (r, POINTER_PLUS_EXPR, op1, op2);
  return true;
}

bool
pointer_plus_operator::op2_range (irange &r, tree type,
				  const prange &lhs ATTRIBUTE_UNUSED,
				  const prange &op1 ATTRIBUTE_UNUSED,
				  relation_trio trio) const
{
  relation_kind rel = trio.lhs_op1 ();
  r.set_varying (type);

  // If the LHS and OP1 are equal, the op2 must be zero.
  if (rel == VREL_EQ)
    r.set_zero (type);
  // If the LHS and OP1 are not equal, the offset must be non-zero.
  else if (rel == VREL_NE)
    r.set_nonzero (type);
  else
    return false;
  return true;
}

bool
operator_bitwise_or::fold_range (prange &r, tree type,
				 const prange &op1,
				 const prange &op2,
				 relation_trio) const
{
  // For pointer types, we are really only interested in asserting
  // whether the expression evaluates to non-NULL.
  if (!range_includes_zero_p (op1) || !range_includes_zero_p (op2))
    r.set_nonzero (type);
  else if (op1.zero_p () && op2.zero_p ())
    r.set_zero (type);
  else
    r.set_varying (type);

  update_known_bitmask (r, BIT_IOR_EXPR, op1, op2);
  return true;
}


class operator_pointer_diff : public range_operator
{
  using range_operator::fold_range;
  using range_operator::update_bitmask;
  using range_operator::op1_op2_relation_effect;
  virtual bool fold_range (irange &r, tree type,
			   const prange &op1,
			   const prange &op2,
			   relation_trio trio) const final override;
  virtual bool op1_op2_relation_effect (irange &lhs_range,
					tree type,
					const prange &op1_range,
					const prange &op2_range,
					relation_kind rel) const final override;
  void update_bitmask (irange &r,
		       const prange &lh, const prange &rh) const final override
  { update_known_bitmask (r, POINTER_DIFF_EXPR, lh, rh); }
} op_pointer_diff;

bool
operator_pointer_diff::fold_range (irange &r, tree type,
				   const prange &op1,
				   const prange &op2,
				   relation_trio trio) const
{
  gcc_checking_assert (r.supports_type_p (type));

  r.set_varying (type);
  relation_kind rel = trio.op1_op2 ();
  op1_op2_relation_effect (r, type, op1, op2, rel);
  update_bitmask (r, op1, op2);
  return true;
}

bool
operator_pointer_diff::op1_op2_relation_effect (irange &lhs_range, tree type,
						const prange &op1_range,
						const prange &op2_range,
						relation_kind rel) const
{
  int_range<2> op1, op2, tmp;
  range_op_handler cast (CONVERT_EXPR);

  if (!cast.fold_range (op1, type, op1_range, tmp)
      || !cast.fold_range (op2, type, op2_range, tmp))
    return false;

  return minus_op1_op2_relation_effect (lhs_range, type, op1, op2, rel);
}

bool
operator_identity::fold_range (prange &r, tree type ATTRIBUTE_UNUSED,
			       const prange &lh ATTRIBUTE_UNUSED,
			       const prange &rh ATTRIBUTE_UNUSED,
			       relation_trio) const
{
  r = lh;
  return true;
}

relation_kind
operator_identity::lhs_op1_relation (const prange &lhs,
				     const prange &op1 ATTRIBUTE_UNUSED,
				     const prange &op2 ATTRIBUTE_UNUSED,
				     relation_kind) const
{
  if (lhs.undefined_p ())
    return VREL_VARYING;
  // Simply a copy, so they are equivalent.
  return VREL_EQ;
}

bool
operator_identity::op1_range (prange &r, tree type ATTRIBUTE_UNUSED,
			      const prange &lhs,
			      const prange &op2 ATTRIBUTE_UNUSED,
			      relation_trio) const
{
  r = lhs;
  return true;
}

bool
operator_cst::fold_range (prange &r, tree type ATTRIBUTE_UNUSED,
			  const prange &lh,
			  const prange & ATTRIBUTE_UNUSED,
			  relation_trio) const
{
  r = lh;
  return true;
}

// Cast between pointers.

bool
operator_cast::fold_range (prange &r, tree type,
			   const prange &inner,
			   const prange &outer,
			   relation_trio) const
{
  if (empty_range_varying (r, type, inner, outer))
    return true;

  r.set (type, inner.lower_bound (), inner.upper_bound ());
  r.update_bitmask (inner.get_bitmask ());
  return true;
}

// Cast a pointer to an integer.

bool
operator_cast::fold_range (irange &r, tree type,
			   const prange &inner,
			   const irange &outer,
			   relation_trio) const
{
  if (empty_range_varying (r, type, inner, outer))
    return true;

  // Represent INNER as an integer of the same size, and then cast it
  // to the resulting integer type.
  tree pointer_uint_type = make_unsigned_type (TYPE_PRECISION (inner.type ()));
  r.set (pointer_uint_type, inner.lower_bound (), inner.upper_bound ());
  r.update_bitmask (inner.get_bitmask ());
  range_cast (r, type);
  return true;
}

// Cast an integer to a pointer.

bool
operator_cast::fold_range (prange &r, tree type,
			   const irange &inner,
			   const prange &outer,
			   relation_trio) const
{
  if (empty_range_varying (r, type, inner, outer))
    return true;

  // Cast INNER to an integer of the same size as the pointer we want,
  // and then copy the bounds to the resulting pointer range.
  int_range<2> tmp = inner;
  tree pointer_uint_type = make_unsigned_type (TYPE_PRECISION (type));
  range_cast (tmp, pointer_uint_type);
  r.set (type, tmp.lower_bound (), tmp.upper_bound ());
  r.update_bitmask (tmp.get_bitmask ());
  return true;
}

bool
operator_cast::op1_range (prange &r, tree type,
			  const prange &lhs,
			  const prange &op2,
			  relation_trio trio) const
{
  if (lhs.undefined_p ())
    return false;
  gcc_checking_assert (types_compatible_p (op2.type(), type));

  // Conversion from other pointers or a constant (including 0/NULL)
  // are straightforward.
  if (POINTER_TYPE_P (lhs.type ())
      || (lhs.singleton_p ()
	  && TYPE_PRECISION (lhs.type ()) >= TYPE_PRECISION (type)))
    fold_range (r, type, lhs, op2, trio);
  else
    {
      // If the LHS is not a pointer nor a singleton, then it is
      // either VARYING or non-zero.
      if (!lhs.undefined_p () && !range_includes_zero_p (lhs))
	r.set_nonzero (type);
      else
	r.set_varying (type);
    }
  r.intersect (op2);
  return true;
}

bool
operator_cast::op1_range (irange &r, tree type,
			  const prange &lhs,
			  const irange &op2,
			  relation_trio trio) const
{
  if (lhs.undefined_p ())
    return false;
  gcc_checking_assert (types_compatible_p (op2.type(), type));

  // Conversion from other pointers or a constant (including 0/NULL)
  // are straightforward.
  if (POINTER_TYPE_P (lhs.type ())
      || (lhs.singleton_p ()
	  && TYPE_PRECISION (lhs.type ()) >= TYPE_PRECISION (type)))
    fold_range (r, type, lhs, op2, trio);
  else
    {
      // If the LHS is not a pointer nor a singleton, then it is
      // either VARYING or non-zero.
      if (!lhs.undefined_p () && !range_includes_zero_p (lhs))
	r.set_nonzero (type);
      else
	r.set_varying (type);
    }
  r.intersect (op2);
  return true;
}

bool
operator_cast::op1_range (prange &r, tree type,
			  const irange &lhs,
			  const prange &op2,
			  relation_trio trio) const
{
  if (lhs.undefined_p ())
    return false;
  gcc_checking_assert (types_compatible_p (op2.type(), type));

  // Conversion from other pointers or a constant (including 0/NULL)
  // are straightforward.
  if (POINTER_TYPE_P (lhs.type ())
      || (lhs.singleton_p ()
	  && TYPE_PRECISION (lhs.type ()) >= TYPE_PRECISION (type)))
    fold_range (r, type, lhs, op2, trio);
  else
    {
      // If the LHS is not a pointer nor a singleton, then it is
      // either VARYING or non-zero.
      if (!lhs.undefined_p () && !range_includes_zero_p (lhs))
	r.set_nonzero (type);
      else
	r.set_varying (type);
    }
  r.intersect (op2);
  return true;
}

relation_kind
operator_cast::lhs_op1_relation (const prange &lhs,
				 const prange &op1,
				 const prange &op2 ATTRIBUTE_UNUSED,
				 relation_kind) const
{
  if (lhs.undefined_p () || op1.undefined_p ())
    return VREL_VARYING;
  unsigned lhs_prec = TYPE_PRECISION (lhs.type ());
  unsigned op1_prec = TYPE_PRECISION (op1.type ());
  // If the result gets sign extended into a larger type check first if this
  // qualifies as a partial equivalence.
  if (TYPE_SIGN (op1.type ()) == SIGNED && lhs_prec > op1_prec)
    {
      // If the result is sign extended, and the LHS is larger than op1,
      // check if op1's range can be negative as the sign extension will
      // cause the upper bits to be 1 instead of 0, invalidating the PE.
      int_range<3> negs = range_negatives (op1.type ());
      negs.intersect (op1);
      if (!negs.undefined_p ())
	return VREL_VARYING;
    }

  unsigned prec = MIN (lhs_prec, op1_prec);
  return bits_to_pe (prec);
}

relation_kind
operator_cast::lhs_op1_relation (const prange &lhs,
				 const irange &op1,
				 const irange &op2 ATTRIBUTE_UNUSED,
				 relation_kind) const
{
  if (lhs.undefined_p () || op1.undefined_p ())
    return VREL_VARYING;
  unsigned lhs_prec = TYPE_PRECISION (lhs.type ());
  unsigned op1_prec = TYPE_PRECISION (op1.type ());
  // If the result gets sign extended into a larger type check first if this
  // qualifies as a partial equivalence.
  if (TYPE_SIGN (op1.type ()) == SIGNED && lhs_prec > op1_prec)
    {
      // If the result is sign extended, and the LHS is larger than op1,
      // check if op1's range can be negative as the sign extension will
      // cause the upper bits to be 1 instead of 0, invalidating the PE.
      int_range<3> negs = range_negatives (op1.type ());
      negs.intersect (op1);
      if (!negs.undefined_p ())
	return VREL_VARYING;
    }

  unsigned prec = MIN (lhs_prec, op1_prec);
  return bits_to_pe (prec);
}

relation_kind
operator_cast::lhs_op1_relation (const irange &lhs,
				 const prange &op1,
				 const prange &op2 ATTRIBUTE_UNUSED,
				 relation_kind) const
{
  if (lhs.undefined_p () || op1.undefined_p ())
    return VREL_VARYING;
  unsigned lhs_prec = TYPE_PRECISION (lhs.type ());
  unsigned op1_prec = TYPE_PRECISION (op1.type ());
  // If the result gets sign extended into a larger type check first if this
  // qualifies as a partial equivalence.
  if (TYPE_SIGN (op1.type ()) == SIGNED && lhs_prec > op1_prec)
    {
      // If the result is sign extended, and the LHS is larger than op1,
      // check if op1's range can be negative as the sign extension will
      // cause the upper bits to be 1 instead of 0, invalidating the PE.
      int_range<3> negs = range_negatives (op1.type ());
      negs.intersect (op1);
      if (!negs.undefined_p ())
	return VREL_VARYING;
    }

  unsigned prec = MIN (lhs_prec, op1_prec);
  return bits_to_pe (prec);
}

bool
operator_min::fold_range (prange &r, tree type,
			  const prange &op1,
			  const prange &op2,
			  relation_trio) const
{
  // For MIN/MAX expressions with pointers, we only care about
  // nullness.  If both are non null, then the result is nonnull.
  // If both are null, then the result is null.  Otherwise they
  // are varying.
  if (!range_includes_zero_p (op1)
      && !range_includes_zero_p (op2))
    r.set_nonzero (type);
  else if (op1.zero_p () && op2.zero_p ())
    r.set_zero (type);
  else
    r.set_varying (type);

  update_known_bitmask (r, MIN_EXPR, op1, op2);
  return true;
}

bool
operator_max::fold_range (prange &r, tree type,
			  const prange &op1,
			  const prange &op2,
			  relation_trio) const
{
  // For MIN/MAX expressions with pointers, we only care about
  // nullness.  If both are non null, then the result is nonnull.
  // If both are null, then the result is null.  Otherwise they
  // are varying.
  if (!range_includes_zero_p (op1)
      && !range_includes_zero_p (op2))
    r.set_nonzero (type);
  else if (op1.zero_p () && op2.zero_p ())
    r.set_zero (type);
  else
    r.set_varying (type);

  update_known_bitmask (r, MAX_EXPR, op1, op2);
  return true;
}

bool
operator_addr_expr::op1_range (prange &r, tree type,
			       const prange &lhs,
			       const prange &op2,
			       relation_trio) const
{
  if (empty_range_varying (r, type, lhs, op2))
    return true;

  // Return a non-null pointer of the LHS type (passed in op2), but only
  // if we cant overflow, eitherwise a no-zero offset could wrap to zero.
  // See PR 111009.
  if (!lhs.undefined_p ()
      && !range_includes_zero_p (lhs)
      && TYPE_OVERFLOW_UNDEFINED (type))
    r.set_nonzero (type);
  else
    r.set_varying (type);
  return true;
}

bool
operator_bitwise_and::fold_range (prange &r, tree type,
				  const prange &op1,
				  const prange &op2 ATTRIBUTE_UNUSED,
				  relation_trio) const
{
  // For pointer types, we are really only interested in asserting
  // whether the expression evaluates to non-NULL.
  if (op1.zero_p () || op2.zero_p ())
    r.set_zero (type);
  else
    r.set_varying (type);

  update_known_bitmask (r, BIT_AND_EXPR, op1, op2);
  return true;
}

bool
operator_equal::fold_range (irange &r, tree type,
			    const prange &op1,
			    const prange &op2,
			    relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_EQ))
    return true;

  // We can be sure the values are always equal or not if both ranges
  // consist of a single value, and then compare them.
  bool op1_const = wi::eq_p (op1.lower_bound (), op1.upper_bound ());
  bool op2_const = wi::eq_p (op2.lower_bound (), op2.upper_bound ());
  if (op1_const && op2_const)
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
      prange tmp = op1;
      tmp.intersect (op2);
      if (tmp.undefined_p ())
	r = range_false (type);
      // Check if a constant cannot satisfy the bitmask requirements.
      else if (op2_const && !op1.get_bitmask ().member_p (op2.lower_bound ()))
	 r = range_false (type);
      else if (op1_const && !op2.get_bitmask ().member_p (op1.lower_bound ()))
	 r = range_false (type);
      else
	r = range_true_and_false (type);
    }

  //update_known_bitmask (r, EQ_EXPR, op1, op2);
  return true;
}

bool
operator_equal::op1_range (prange &r, tree type,
			   const irange &lhs,
			   const prange &op2,
			   relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // If it's true, the result is the same as OP2.
      r = op2;
      break;

    case BRS_FALSE:
      // If the result is false, the only time we know anything is
      // if OP2 is a constant.
      if (!op2.undefined_p ()
	  && wi::eq_p (op2.lower_bound(), op2.upper_bound()))
	{
	  r = op2;
	  r.invert ();
	}
      else
	r.set_varying (type);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_equal::op2_range (prange &r, tree type,
			   const irange &lhs,
			   const prange &op1,
			   relation_trio rel) const
{
  return operator_equal::op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
}

relation_kind
operator_equal::op1_op2_relation (const irange &lhs, const prange &,
				  const prange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 == op2 indicates NE_EXPR.
  if (lhs.zero_p ())
    return VREL_NE;

  // TRUE = op1 == op2 indicates EQ_EXPR.
  if (!range_includes_zero_p (lhs))
    return VREL_EQ;
  return VREL_VARYING;
}

bool
operator_not_equal::fold_range (irange &r, tree type,
				const prange &op1,
				const prange &op2,
				relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_NE))
    return true;

  // We can be sure the values are always equal or not if both ranges
  // consist of a single value, and then compare them.
  bool op1_const = wi::eq_p (op1.lower_bound (), op1.upper_bound ());
  bool op2_const = wi::eq_p (op2.lower_bound (), op2.upper_bound ());
  if (op1_const && op2_const)
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
      prange tmp = op1;
      tmp.intersect (op2);
      if (tmp.undefined_p ())
	r = range_true (type);
      // Check if a constant cannot satisfy the bitmask requirements.
      else if (op2_const && !op1.get_bitmask ().member_p (op2.lower_bound ()))
	 r = range_true (type);
      else if (op1_const && !op2.get_bitmask ().member_p (op1.lower_bound ()))
	 r = range_true (type);
      else
	r = range_true_and_false (type);
    }

  //update_known_bitmask (r, NE_EXPR, op1, op2);
  return true;
}

bool
operator_not_equal::op1_range (prange &r, tree type,
			       const irange &lhs,
			       const prange &op2,
			       relation_trio) const
{
  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      // If the result is true, the only time we know anything is if
      // OP2 is a constant.
      if (!op2.undefined_p ()
	  && wi::eq_p (op2.lower_bound(), op2.upper_bound()))
	{
	  r = op2;
	  r.invert ();
	}
      else
	r.set_varying (type);
      break;

    case BRS_FALSE:
      // If it's false, the result is the same as OP2.
      r = op2;
      break;

    default:
      break;
    }
  return true;
}


bool
operator_not_equal::op2_range (prange &r, tree type,
			       const irange &lhs,
			       const prange &op1,
			       relation_trio rel) const
{
  return operator_not_equal::op1_range (r, type, lhs, op1, rel.swap_op1_op2 ());
}

relation_kind
operator_not_equal::op1_op2_relation (const irange &lhs, const prange &,
				      const prange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 != op2  indicates EQ_EXPR.
  if (lhs.zero_p ())
    return VREL_EQ;

  // TRUE = op1 != op2  indicates NE_EXPR.
  if (!range_includes_zero_p (lhs))
    return VREL_NE;
  return VREL_VARYING;
}

bool
operator_lt::fold_range (irange &r, tree type,
			 const prange &op1,
			 const prange &op2,
			 relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_LT))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::lt_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_true (type);
  else if (!wi::lt_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_false (type);
  // Use nonzero bits to determine if < 0 is false.
  else if (op2.zero_p () && !wi::neg_p (op1.get_nonzero_bits (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);

  //update_known_bitmask (r, LT_EXPR, op1, op2);
  return true;
}

bool
operator_lt::op1_range (prange &r, tree type,
			const irange &lhs,
			const prange &op2,
			relation_trio) const
{
  if (op2.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_lt (r, type, op2);
      break;

    case BRS_FALSE:
      build_ge (r, type, op2);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_lt::op2_range (prange &r, tree type,
			const irange &lhs,
			const prange &op1,
			relation_trio) const
{
  if (op1.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_gt (r, type, op1);
      break;

    case BRS_FALSE:
      build_le (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

relation_kind
operator_lt::op1_op2_relation (const irange &lhs, const prange &,
			       const prange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 < op2 indicates GE_EXPR.
  if (lhs.zero_p ())
    return VREL_GE;

  // TRUE = op1 < op2 indicates LT_EXPR.
  if (!range_includes_zero_p (lhs))
    return VREL_LT;
  return VREL_VARYING;
}

bool
operator_le::fold_range (irange &r, tree type,
			 const prange &op1,
			 const prange &op2,
			 relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_LE))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::le_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_true (type);
  else if (!wi::le_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);

  //update_known_bitmask (r, LE_EXPR, op1, op2);
  return true;
}

bool
operator_le::op1_range (prange &r, tree type,
			const irange &lhs,
			const prange &op2,
			relation_trio) const
{
  if (op2.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_le (r, type, op2);
      break;

    case BRS_FALSE:
      build_gt (r, type, op2);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_le::op2_range (prange &r, tree type,
			const irange &lhs,
			const prange &op1,
			relation_trio) const
{
  if (op1.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_ge (r, type, op1);
      break;

    case BRS_FALSE:
      build_lt (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

relation_kind
operator_le::op1_op2_relation (const irange &lhs, const prange &,
			       const prange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 <= op2 indicates GT_EXPR.
  if (lhs.zero_p ())
    return VREL_GT;

  // TRUE = op1 <= op2 indicates LE_EXPR.
  if (!range_includes_zero_p (lhs))
    return VREL_LE;
  return VREL_VARYING;
}

bool
operator_gt::fold_range (irange &r, tree type,
			 const prange &op1, const prange &op2,
			 relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_GT))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::gt_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_true (type);
  else if (!wi::gt_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);

  //update_known_bitmask (r, GT_EXPR, op1, op2);
  return true;
}

bool
operator_gt::op1_range (prange &r, tree type,
			const irange &lhs, const prange &op2,
			relation_trio) const
{
  if (op2.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_gt (r, type, op2);
      break;

    case BRS_FALSE:
      build_le (r, type, op2);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_gt::op2_range (prange &r, tree type,
			const irange &lhs,
			const prange &op1,
			relation_trio) const
{
  if (op1.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_lt (r, type, op1);
      break;

    case BRS_FALSE:
      build_ge (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

relation_kind
operator_gt::op1_op2_relation (const irange &lhs, const prange &,
			       const prange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 > op2 indicates LE_EXPR.
  if (lhs.zero_p ())
    return VREL_LE;

  // TRUE = op1 > op2 indicates GT_EXPR.
  if (!range_includes_zero_p (lhs))
    return VREL_GT;
  return VREL_VARYING;
}

bool
operator_ge::fold_range (irange &r, tree type,
			 const prange &op1,
			 const prange &op2,
			 relation_trio rel) const
{
  if (relop_early_resolve (r, type, op1, op2, rel, VREL_GE))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::ge_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_true (type);
  else if (!wi::ge_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_false (type);
  else
    r = range_true_and_false (type);

  //update_known_bitmask (r, GE_EXPR, op1, op2);
  return true;
}

bool
operator_ge::op1_range (prange &r, tree type,
			const irange &lhs,
			const prange &op2,
			relation_trio) const
{
  if (op2.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_ge (r, type, op2);
      break;

    case BRS_FALSE:
      build_lt (r, type, op2);
      break;

    default:
      break;
    }
  return true;
}

bool
operator_ge::op2_range (prange &r, tree type,
			const irange &lhs,
			const prange &op1,
			relation_trio) const
{
  if (op1.undefined_p ())
    return false;

  switch (get_bool_state (r, lhs, type))
    {
    case BRS_TRUE:
      build_le (r, type, op1);
      break;

    case BRS_FALSE:
      build_gt (r, type, op1);
      break;

    default:
      break;
    }
  return true;
}

relation_kind
operator_ge::op1_op2_relation (const irange &lhs, const prange &,
			       const prange &) const
{
  if (lhs.undefined_p ())
    return VREL_UNDEFINED;

  // FALSE = op1 >= op2 indicates LT_EXPR.
  if (lhs.zero_p ())
    return VREL_LT;

  // TRUE = op1 >= op2 indicates GE_EXPR.
  if (!range_includes_zero_p (lhs))
    return VREL_GE;
  return VREL_VARYING;
}

// Initialize any pointer operators to the primary table

void
range_op_table::initialize_pointer_ops ()
{
  set (POINTER_PLUS_EXPR, op_pointer_plus);
  set (POINTER_DIFF_EXPR, op_pointer_diff);
}
