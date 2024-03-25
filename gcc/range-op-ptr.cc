/* Code for range operators.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

class pointer_plus_operator : public range_operator
{
  using range_operator::op2_range;
public:
  virtual void wi_fold (irange &r, tree type,
			const wide_int &lh_lb,
			const wide_int &lh_ub,
			const wide_int &rh_lb,
			const wide_int &rh_ub) const;
  virtual bool op2_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op1,
			  relation_trio = TRIO_VARYING) const;
  void update_bitmask (irange &r, const irange &lh, const irange &rh) const
    { update_known_bitmask (r, POINTER_PLUS_EXPR, lh, rh); }
} op_pointer_plus;

void
pointer_plus_operator::wi_fold (irange &r, tree type,
				const wide_int &lh_lb,
				const wide_int &lh_ub,
				const wide_int &rh_lb,
				const wide_int &rh_ub) const
{
  // Check for [0,0] + const, and simply return the const.
  if (lh_lb == 0 && lh_ub == 0 && rh_lb == rh_ub)
    {
      r.set (type, rh_lb, rh_lb);
      return;
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
    r = range_nonzero (type);
  else if (lh_lb == lh_ub && lh_lb == 0
	   && rh_lb == rh_ub && rh_lb == 0)
    r = range_zero (type);
  else
   r.set_varying (type);
}

bool
pointer_plus_operator::op2_range (irange &r, tree type,
				  const irange &lhs ATTRIBUTE_UNUSED,
				  const irange &op1 ATTRIBUTE_UNUSED,
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

class pointer_min_max_operator : public range_operator
{
public:
  virtual void wi_fold (irange & r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const;
} op_ptr_min_max;

void
pointer_min_max_operator::wi_fold (irange &r, tree type,
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
    r.set_varying (type);
}

class pointer_and_operator : public range_operator
{
public:
  virtual void wi_fold (irange &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const;
} op_pointer_and;

void
pointer_and_operator::wi_fold (irange &r, tree type,
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
    r.set_varying (type);
}


class pointer_or_operator : public range_operator
{
public:
  using range_operator::op1_range;
  using range_operator::op2_range;
  virtual bool op1_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op2,
			  relation_trio rel = TRIO_VARYING) const;
  virtual bool op2_range (irange &r, tree type,
			  const irange &lhs,
			  const irange &op1,
			  relation_trio rel = TRIO_VARYING) const;
  virtual void wi_fold (irange &r, tree type,
			const wide_int &lh_lb, const wide_int &lh_ub,
			const wide_int &rh_lb, const wide_int &rh_ub) const;
} op_pointer_or;

bool
pointer_or_operator::op1_range (irange &r, tree type,
				const irange &lhs,
				const irange &op2 ATTRIBUTE_UNUSED,
				relation_trio) const
{
  if (lhs.undefined_p ())
    return false;
  if (lhs.zero_p ())
    {
      r.set_zero (type);
      return true;
    }
  r.set_varying (type);
  return true;
}

bool
pointer_or_operator::op2_range (irange &r, tree type,
				const irange &lhs,
				const irange &op1,
				relation_trio) const
{
  return pointer_or_operator::op1_range (r, type, lhs, op1);
}

void
pointer_or_operator::wi_fold (irange &r, tree type,
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
    r.set_varying (type);
}

class operator_pointer_diff : public range_operator
{
  virtual bool op1_op2_relation_effect (irange &lhs_range,
					tree type,
					const irange &op1_range,
					const irange &op2_range,
					relation_kind rel) const;
  void update_bitmask (irange &r, const irange &lh, const irange &rh) const
    { update_known_bitmask (r, POINTER_DIFF_EXPR, lh, rh); }
} op_pointer_diff;

bool
operator_pointer_diff::op1_op2_relation_effect (irange &lhs_range, tree type,
						const irange &op1_range,
						const irange &op2_range,
						relation_kind rel) const
{
  return minus_op1_op2_relation_effect (lhs_range, type, op1_range, op2_range,
					rel);
}

// ----------------------------------------------------------------------
// Hybrid operators for the 4 operations which integer and pointers share,
// but which have different implementations.  Simply check the type in
// the call and choose the appropriate method.
// Once there is a PRANGE signature, simply add the appropriate
// prototypes in the rmixed range class, and remove these hybrid classes.

class hybrid_and_operator : public operator_bitwise_and
{
public:
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::lhs_op1_relation;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const final override
    {
      if (INTEGRAL_TYPE_P (type))
	return operator_bitwise_and::op1_range (r, type, lhs, op2, rel);
      else
	return false;
    }
  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio rel = TRIO_VARYING) const final override
    {
      if (INTEGRAL_TYPE_P (type))
	return operator_bitwise_and::op2_range (r, type, lhs, op1, rel);
      else
	return false;
    }
  relation_kind lhs_op1_relation (const irange &lhs,
				  const irange &op1, const irange &op2,
				  relation_kind rel) const final override
    {
      if (!lhs.undefined_p () && INTEGRAL_TYPE_P (lhs.type ()))
	return operator_bitwise_and::lhs_op1_relation (lhs, op1, op2, rel);
      else
	return VREL_VARYING;
    }
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override
    {
      if (!r.undefined_p () && INTEGRAL_TYPE_P (r.type ()))
	operator_bitwise_and::update_bitmask (r, lh, rh);
    }

  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const final override
    {
      if (INTEGRAL_TYPE_P (type))
	return operator_bitwise_and::wi_fold (r, type, lh_lb, lh_ub,
					      rh_lb, rh_ub);
      else
	return op_pointer_and.wi_fold (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
    }
} op_hybrid_and;

// Temporary class which dispatches routines to either the INT version or
// the pointer version depending on the type.  Once PRANGE is a range
// class, we can remove the hybrid.

class hybrid_or_operator : public operator_bitwise_or
{
public:
  using range_operator::op1_range;
  using range_operator::op2_range;
  using range_operator::lhs_op1_relation;
  bool op1_range (irange &r, tree type,
		  const irange &lhs, const irange &op2,
		  relation_trio rel = TRIO_VARYING) const final override
    {
      if (INTEGRAL_TYPE_P (type))
	return operator_bitwise_or::op1_range (r, type, lhs, op2, rel);
      else
	return op_pointer_or.op1_range (r, type, lhs, op2, rel);
    }
  bool op2_range (irange &r, tree type,
		  const irange &lhs, const irange &op1,
		  relation_trio rel = TRIO_VARYING) const final override
    {
      if (INTEGRAL_TYPE_P (type))
	return operator_bitwise_or::op2_range (r, type, lhs, op1, rel);
      else
	return op_pointer_or.op2_range (r, type, lhs, op1, rel);
    }
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override
    {
      if (!r.undefined_p () && INTEGRAL_TYPE_P (r.type ()))
	operator_bitwise_or::update_bitmask (r, lh, rh);
    }

  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const final override
    {
      if (INTEGRAL_TYPE_P (type))
	return operator_bitwise_or::wi_fold (r, type, lh_lb, lh_ub,
					      rh_lb, rh_ub);
      else
	return op_pointer_or.wi_fold (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
    }
} op_hybrid_or;

// Temporary class which dispatches routines to either the INT version or
// the pointer version depending on the type.  Once PRANGE is a range
// class, we can remove the hybrid.

class hybrid_min_operator : public operator_min
{
public:
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override
    {
      if (!r.undefined_p () && INTEGRAL_TYPE_P (r.type ()))
	operator_min::update_bitmask (r, lh, rh);
    }

  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const final override
    {
      if (INTEGRAL_TYPE_P (type))
	return operator_min::wi_fold (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
      else
	return op_ptr_min_max.wi_fold (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
    }
} op_hybrid_min;

class hybrid_max_operator : public operator_max
{
public:
  void update_bitmask (irange &r, const irange &lh,
		       const irange &rh) const final override
    {
      if (!r.undefined_p () && INTEGRAL_TYPE_P (r.type ()))
	operator_max::update_bitmask (r, lh, rh);
    }

  void wi_fold (irange &r, tree type, const wide_int &lh_lb,
		const wide_int &lh_ub, const wide_int &rh_lb,
		const wide_int &rh_ub) const final override
    {
      if (INTEGRAL_TYPE_P (type))
	return operator_max::wi_fold (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
      else
	return op_ptr_min_max.wi_fold (r, type, lh_lb, lh_ub, rh_lb, rh_ub);
    }
} op_hybrid_max;

// Initialize any pointer operators to the primary table

void
range_op_table::initialize_pointer_ops ()
{
  set (POINTER_PLUS_EXPR, op_pointer_plus);
  set (POINTER_DIFF_EXPR, op_pointer_diff);
  set (BIT_AND_EXPR, op_hybrid_and);
  set (BIT_IOR_EXPR, op_hybrid_or);
  set (MIN_EXPR, op_hybrid_min);
  set (MAX_EXPR, op_hybrid_max);
}
