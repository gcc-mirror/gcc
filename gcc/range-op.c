/* Code for range operators.
   Copyright (C) 2017-2019 Free Software Foundation, Inc.
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
#include "range.h"
#include "range-op.h"
#include "wide-int-range.h"


/* Defaults for all operations are to return NULL. This means no
   additional range info is available beyond that of the type.  */

bool
range_operator::fold_range (irange& r ATTRIBUTE_UNUSED,
			   const irange& op1 ATTRIBUTE_UNUSED,
			   const irange& op2 ATTRIBUTE_UNUSED) const
{
  return false;
}

bool
range_operator::op1_range (irange& r ATTRIBUTE_UNUSED,
			   const irange& lhs ATTRIBUTE_UNUSED,
			   const irange& op2 ATTRIBUTE_UNUSED) const
{
  return false;
}

bool
range_operator::op2_range (irange& r ATTRIBUTE_UNUSED,
			   const irange& lhs ATTRIBUTE_UNUSED,
			   const irange& op1 ATTRIBUTE_UNUSED) const
{
  return false;
}

// This class is used to create a range_operator for a tree code
// and automatically register it with the operator table.
// Simply inherit from this class and overload whatever routines are required.
// This provides registration as well as default debug dumping for the 
// tree code and calling op_rr() to resolve fold_range.

class trange_operator : public range_operator
{
public:
  trange_operator (enum tree_code c);
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
protected:
  tree_code code;
};


// This implements the range operator table as a local object in this file.

class range_op_table
{
public:
  inline range_operator *operator[] (enum tree_code code);
  void register_operator (enum tree_code code, range_operator *);
private:
  range_operator *m_range_tree[MAX_TREE_CODES];
} range_tree;

// Return a pointer tto the range_operator instance, if there is one, 
// associated with tree_code CODE.

range_operator *
range_op_table::operator[] (enum tree_code code)
{
  gcc_assert (code > 0 && code < MAX_TREE_CODES);
  return m_range_tree[code];
}

// Add OP to the handler table for CODE.

void
range_op_table::register_operator (enum tree_code code, range_operator *op)
{
  gcc_checking_assert (m_range_tree[code] == NULL && op != NULL);
  m_range_tree[code] = op;
}

/* The table is hidden and accessed via a simple extern function.  */

range_operator *
range_op_handler (enum tree_code code)
{
  return range_tree[code];
}
// -------------------------------------------------------------------------

// Auxillary routine to return the upper limit for a type.

inline wide_int
max_limit (const_tree type)
{
  return wi::max_value (TYPE_PRECISION (type) , TYPE_SIGN (type));
}

// Auxillary routine to return the lower limit for a type.

inline wide_int
min_limit (const_tree type)
{
  return wi::min_value (TYPE_PRECISION (type) , TYPE_SIGN (type));
}

// If the range of either operand is undefined, set the result to
// undefined and return true.  This is a common routine to most
// functions as undefined is a viral condition, so if either operand
// is undefined, so is the result.

inline bool
empty_range_check (irange& r, const irange& op1, const irange & op2, tree type)
{
  if (op1.undefined_p () || op2.undefined_p ())
    {
      r.set_undefined (type);
      return true;
    }
  else
    return false;
}

/* Given newly calculated lbound and ubound, examine their respective
   overflow bits to determine how to add [lbound, ubound] into range
   R.  */

static void
accumulate_range (irange &r,
		  const wide_int &lb, wi::overflow_type ov_lb,
		  const wide_int &ub, wi::overflow_type ov_ub,
		  bool overflow_wraps = false)
{
  value_range_kind kind;
  wide_int min = lb, max = ub;
  tree type = r.type ();
  adjust_range_for_overflow (kind, min, max, type, ov_lb, ov_ub,
			     overflow_wraps);
  if (kind == VR_VARYING)
    {
      r.set_varying (type);
      return;
    }
  irange tmp (kind, type, min, max);
  r.union_ (tmp);
  return;
}

static void
accumulate_range (irange &r, const wide_int &lb, const wide_int &ub)
{
  accumulate_range (r, lb, wi::OVF_NONE, ub, wi::OVF_NONE, false);
}

/* Like accumulate_range, but canonicalize the case where the bounds
   are swapped and overflow may wrap.  In which case we transform
   [10,5] into [MIN,5][10,MAX].

   I am not happy with this.  This is basically a workaround for the
   fact that VRP has cripple ranges and wide_int_range_mult_wrapping
   may return swapped ranges, so they must be canonicalized into some
   anti range crap.  I'm hoping this will go away when everything is
   an irange.  */

static inline void
accumulate_range_and_canonicalize (signop s,
				   irange &r,
				   const wide_int &new_lb,
				   const wide_int &new_ub)
{
  /* If the bounds are swapped, set the overflow bit to force
     add_to_range to create the range correctly.  This is essentially
     what set_and_canonicalize_value_range was doing.  */
  wi::overflow_type overflow;
  bool overflow_wraps = TYPE_OVERFLOW_WRAPS (r.type ());
  if (wi::gt_p (new_lb, new_ub, s))
    {
      overflow = wi::OVF_OVERFLOW;
      overflow_wraps = true;
    }
  else
    overflow = wi::OVF_NONE;

  accumulate_range (r, new_lb, wi::OVF_NONE, new_ub, overflow, overflow_wraps);
}

/*  -----------------------------------------------------------------------  */

/* irange wrapper for wide_int_range_multiplicative_op.  */

static bool
irange_multiplicative_op (enum tree_code code, signop s, irange& r,
			  const wide_int& lh_lb, const wide_int& lh_ub,
			  const wide_int& rh_lb, const wide_int& rh_ub)
{
  wide_int new_lb, new_ub;
  bool overflow_undefined = TYPE_OVERFLOW_UNDEFINED (r.type ());
  unsigned prec = TYPE_PRECISION (r.type ());
  if (wide_int_range_multiplicative_op (new_lb, new_ub,
					code, s, prec,
					lh_lb, lh_ub, rh_lb, rh_ub,
					overflow_undefined))
    {
      accumulate_range_and_canonicalize (s, r, new_lb, new_ub);
      return true;
    }
  return false;
}

/* For an operation on pointers, this specifies whether the resulting
   operation is null, non-null, or unknown.  */
enum wide_int_range_nullness {
  WIDE_INT_RANGE_UNKNOWN = 0,
  WIDE_INT_RANGE_NULL,
  WIDE_INT_RANGE_NONNULL
};

/* FIXME: This was slated to be in wide-int-range.cc, but Richi is
   unconvinced we need to abstract this out.  To be discussed later.

   See discussion here:
   https://gcc.gnu.org/ml/gcc-patches/2018-09/msg00171.html
*/
/* Given a binary operator (CODE) on two pointer ranges, return if the
   result will be zero, non-zero, or unknown.  */

static enum wide_int_range_nullness
wide_int_range_pointer (enum tree_code code,
			signop sign,
			const wide_int &vr0_min,
			const wide_int &vr0_max,
			const wide_int &vr1_min,
			const wide_int &vr1_max,
			bool overflow_wraps)
{
  unsigned prec = vr0_min.get_precision ();
  if (code == MIN_EXPR || code == MAX_EXPR)
    {
      /* For MIN/MAX expressions with pointers, we only care about
	 nullness, if both are non null, then the result is nonnull.
	 If both are null, then the result is null. Otherwise they
	 are varying.  */
      if (!wide_int_range_includes_zero_p (vr0_min, vr0_max, sign)
	  && !wide_int_range_includes_zero_p (vr1_min, vr1_max, sign))
	return WIDE_INT_RANGE_NONNULL;
      else if (wide_int_range_zero_p (vr0_min, vr0_max, prec)
	       && wide_int_range_zero_p (vr1_min, vr1_max, prec))
	return WIDE_INT_RANGE_NULL;
      else
	return WIDE_INT_RANGE_UNKNOWN;
    }
  else if (code == POINTER_PLUS_EXPR)
    {
      /* For pointer types, we are really only interested in asserting
	 whether the expression evaluates to non-NULL.

	 With -fno-delete-null-pointer-checks we need to be more
	 conservative.  As some object might reside at address 0,
	 then some offset could be added to it and the same offset
	 subtracted again and the result would be NULL.
	 E.g.
	 static int a[12]; where &a[0] is NULL and
	 ptr = &a[6];
	 ptr -= 6;
	 ptr will be NULL here, even when there is POINTER_PLUS_EXPR
	 where the first range doesn't include zero and the second one
	 doesn't either.  As the second operand is sizetype (unsigned),
	 consider all ranges where the MSB could be set as possible
	 subtractions where the result might be NULL.  */
      if ((!wide_int_range_includes_zero_p (vr0_min, vr0_max, sign)
	   || !wide_int_range_includes_zero_p (vr1_min, vr1_max, sign))
	  && !overflow_wraps
	  && (flag_delete_null_pointer_checks
	      || !wi::sign_mask (vr1_max)))
	return WIDE_INT_RANGE_NONNULL;
      else if (wide_int_range_zero_p (vr0_min, vr0_max, prec)
	       && wide_int_range_zero_p (vr1_min, vr1_max, prec))
	return WIDE_INT_RANGE_NULL;
      else
	return WIDE_INT_RANGE_UNKNOWN;
    }
  else if (code == BIT_AND_EXPR)
    {
      /* For pointer types, we are really only interested in asserting
	 whether the expression evaluates to non-NULL.  */
      if (!wide_int_range_includes_zero_p (vr0_min, vr0_max, sign)
	  && !wide_int_range_includes_zero_p (vr1_min, vr1_max, sign))
	return WIDE_INT_RANGE_NONNULL;
      else if (wide_int_range_zero_p (vr0_min, vr0_max, prec)
	       || wide_int_range_zero_p (vr1_min, vr1_max, prec))
	return WIDE_INT_RANGE_NULL;
      else
	return WIDE_INT_RANGE_UNKNOWN;
    }
  else
    return WIDE_INT_RANGE_UNKNOWN;
}

/* irange wrapper for wide_int_range_pointer.  */

static void
irange_pointer_optimization (enum tree_code code, signop s, irange &r,
			     const wide_int &lh_lb, const wide_int &lh_ub,
			     const wide_int &rh_lb, const wide_int &rh_ub)
{
  tree type = r.type ();
  wide_int_range_nullness n;
  n = wide_int_range_pointer (code, s, lh_lb, lh_ub, rh_lb, rh_ub,
			      TYPE_OVERFLOW_WRAPS (type));
  if (n == WIDE_INT_RANGE_UNKNOWN)
    r.set_varying (type);
  else if (n == WIDE_INT_RANGE_NULL)
    r.union_ (range_zero (type));
  else if (n == WIDE_INT_RANGE_NONNULL)
    r.union_ (range_nonzero (type));
  else
    gcc_unreachable ();
}

/* The result of a bitwise [lh_lb, lh_ub] .AND. [rh_lb, rh_ub] has
   already been calculated in range R.  See if the operation was a
   masking operation, and optimize it further.  */

static void
irange_adjust_bit_and_mask (irange &r, signop s,
			    const wide_int &lh_lb, const wide_int &lh_ub,
			    const wide_int &rh_lb, const wide_int &rh_ub)
{
  /* FIXME: This optimization causes us to miscompare with VRP.
     Disable for now, and then contribute it independently
     upstream.  */
  return;

  /* If the resulting range contains 0, AND the least significant bit
     of mask is not set, we can improve the range by making the least
     significant bit set the minimum, and adding in the 0.

     For example, A & 0x3C returns a range of [0,60].  We can improve
     this to [0,0][4, 60].  */
  wide_int mask, lower_bound, upper_bound;
  int tz;
  tree type = r.type ();
  irange zero = range_zero (type);
  bool range_contains_zero = !range_intersect (r, zero).undefined_p ();
  if (range_contains_zero
      && wide_int_range_get_mask_and_bounds (mask,
					     lower_bound,
					     upper_bound,
					     lh_lb, lh_ub,
					     rh_lb, rh_ub)
      && (tz = wi::ctz (mask)) != 0)
    {
      unsigned prec = TYPE_PRECISION (type);
      irange negatives, positives;
      wide_int lb, ub;
      if (s == SIGNED)
	{
	  positives = range_positives (type);
	  negatives = range_negatives (type);
	  positives.intersect (r);
	  negatives.intersect (r);
	}
      else
	{
	  positives = r;
	  negatives.set_undefined (type);
	}
      if (!positives.undefined_p ())
	{
	  // Mask out the positive numbers that can't happen.
	  lb = wi::shifted_mask (tz, 1, false, prec);
	  ub = positives.upper_bound();
	  if (wi::le_p (lb, ub, s))
	    positives.intersect (irange (type, lb, ub));
	}
      if (!negatives.undefined_p ())
	{
	  // Mask out the negatives numbers that can't happen.
	  lb = wi::min_value (prec, s);
	  ub = wi::shifted_mask (0, tz, true, prec);
	  negatives.intersect (irange (type, lb, ub));
	}
      r = positives;
      r.union_ (negatives);
      r.union_ (zero);
    }
}

/* Perform an operation CODE on pairs of ranges, and store the result
   in R.

   If the operation is a binary op, perform:
   [lh_lb, lh_ub] .CODE. [rh_lb, rh_ub]

   If the operation is a unary op, the rh_* bounds are unused.

   Return FALSE to stop processing remaining subranges.  In this case,
   the result is assumed to span the entire domain (range_for_type).  */

static bool
op_wi (enum tree_code code, irange &r, tree rh_type,
       const wide_int &lh_lb, const wide_int lh_ub,
       const wide_int &rh_lb, const wide_int &rh_ub)
{
  wide_int new_lb, new_ub, tmp;
  wi::overflow_type ov_lb, ov_ub;
  tree type = r.type ();
  signop s = TYPE_SIGN (type);

  if (POINTER_TYPE_P (type))
    {
      irange_pointer_optimization (code, s, r, lh_lb, lh_ub, rh_lb, rh_ub);
      return true;
    }

  switch (code)
    {
    case PLUS_EXPR:
      wide_int_binop (new_lb, code, lh_lb, rh_lb, s, &ov_lb);
      wide_int_binop (new_ub, code, lh_ub, rh_ub, s, &ov_ub);
      accumulate_range (r, new_lb, ov_lb, new_ub, ov_ub,
			TYPE_OVERFLOW_WRAPS (type));
      return true;

    case MINUS_EXPR:
      wide_int_binop (new_lb, code, lh_lb, rh_ub, s, &ov_lb);
      wide_int_binop (new_ub, code, lh_ub, rh_lb, s, &ov_ub);
      accumulate_range (r, new_lb, ov_lb, new_ub, ov_ub,
			TYPE_OVERFLOW_WRAPS (type));
      return true;

    case MAX_EXPR:
    case MIN_EXPR:
      if (wide_int_range_min_max (new_lb, new_ub, code, s,
				  TYPE_PRECISION (type),
				  lh_lb, lh_ub, rh_lb, rh_ub))
	{
	  accumulate_range (r, new_lb, new_ub);
	  return true;
	}
      r.set_varying (type);
      return false;

    case MULT_EXPR:
      if (irange_multiplicative_op (code, s, r, lh_lb, lh_ub, rh_lb, rh_ub))
	return true;
      r.set_varying (type);
      return false;

    case RSHIFT_EXPR:
      if (!wide_int_range_shift_undefined_p (TYPE_SIGN (rh_type),
					     TYPE_PRECISION (type),
					     rh_lb, rh_ub)
	  && irange_multiplicative_op (code, s, r,
				       lh_lb, lh_ub, rh_lb, rh_ub))
	return true;
      r.set_varying (type);
      return false;

    case LSHIFT_EXPR:
      if (!wide_int_range_shift_undefined_p (TYPE_SIGN (rh_type),
					     TYPE_PRECISION (type),
					     rh_lb, rh_ub)
	  && wide_int_range_lshift (new_lb, new_ub, s, TYPE_PRECISION (type),
				    lh_lb, lh_ub, rh_lb, rh_ub,
				    TYPE_OVERFLOW_UNDEFINED (type)))
	{
	  accumulate_range_and_canonicalize (s, r, new_lb, new_ub);
	  return true;
	}
      r.set_varying (type);
      return false;

    case BIT_AND_EXPR:
      {
	/* For pointer types, we are really only interested in asserting
	   whether the expression evaluates to non-NULL.  */
	wide_int may_be_nonzero_lh, must_be_nonzero_lh;
	wide_int may_be_nonzero_rh, must_be_nonzero_rh;
	wide_int_range_set_zero_nonzero_bits (s, lh_lb, lh_ub,
					      may_be_nonzero_lh,
					      must_be_nonzero_lh);
	wide_int_range_set_zero_nonzero_bits (s, rh_lb, rh_ub,
					      may_be_nonzero_rh,
					      must_be_nonzero_rh);
	if (wide_int_range_bit_and (new_lb, new_ub, s, TYPE_PRECISION (type),
				    lh_lb, lh_ub,
				    rh_lb, rh_ub,
				    must_be_nonzero_lh,
				    may_be_nonzero_lh,
				    must_be_nonzero_rh,
				    may_be_nonzero_rh))
	  {
	    // For AND, calculate each subrange separately, and then union
	    // the results.
	    irange tmp;
	    tmp.set_undefined (r.type ());
	    accumulate_range (tmp, new_lb, new_ub);
	    irange_adjust_bit_and_mask (tmp, s, lh_lb, lh_ub, rh_lb, rh_ub);
	    r.union_ (tmp);
	    return true;
	  }
	r.set_varying (type);
	return false;
      }

    case BIT_IOR_EXPR:
      {
	wide_int may_be_nonzero_lh, must_be_nonzero_lh;
	wide_int may_be_nonzero_rh, must_be_nonzero_rh;
	wide_int_range_set_zero_nonzero_bits (s, lh_lb, lh_ub,
					      may_be_nonzero_lh,
					      must_be_nonzero_lh);
	wide_int_range_set_zero_nonzero_bits (s, rh_lb, rh_ub,
					      may_be_nonzero_rh,
					      must_be_nonzero_rh);
	if (wide_int_range_bit_ior (new_lb, new_ub, s,
				    lh_lb, lh_ub,
				    rh_lb, rh_ub,
				    must_be_nonzero_lh,
				    may_be_nonzero_lh,
				    must_be_nonzero_rh,
				    may_be_nonzero_rh))
	  {
	    accumulate_range (r, new_lb, new_ub);
	    return true;
	  }
	r.set_varying (type);
	return false;
      }

    case BIT_XOR_EXPR:
      {
	wide_int may_be_nonzero_lh, must_be_nonzero_lh;
	wide_int may_be_nonzero_rh, must_be_nonzero_rh;
	wide_int_range_set_zero_nonzero_bits (s, lh_lb, lh_ub,
					      may_be_nonzero_lh,
					      must_be_nonzero_lh);
	wide_int_range_set_zero_nonzero_bits (s, rh_lb, rh_ub,
					      may_be_nonzero_rh,
					      must_be_nonzero_rh);
	if (wide_int_range_bit_xor (new_lb, new_ub, s, TYPE_PRECISION (type),
				    must_be_nonzero_lh,
				    may_be_nonzero_lh,
				    must_be_nonzero_rh,
				    may_be_nonzero_rh))
	  {
	    accumulate_range (r, new_lb, new_ub);
	    return true;
	  }
	r.set_varying (type);
	return false;
      }

    case TRUNC_MOD_EXPR:
      if (wide_int_range_zero_p (rh_lb, rh_ub, TYPE_PRECISION (type)))
	{
	  /* An empty range means undefined.  */
	  return false;
	}
      wide_int_range_trunc_mod (new_lb, new_ub, s, TYPE_PRECISION (type),
				lh_lb, lh_ub, rh_lb, rh_ub);
      accumulate_range (r, new_lb, new_ub);
      return true;

    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case CEIL_DIV_EXPR:
      {
	/* If we know we will divide by zero, return an empty range,
	   which will be interpreted as undefined.  */
	if (rh_lb == 0 && rh_ub == 0)
	  return false;

	wide_int extra_min, extra_max;
	bool extra_range_p;
	if (wide_int_range_div (new_lb, new_ub, code, s,
				TYPE_PRECISION (type),
				lh_lb, lh_ub,
				rh_lb, rh_ub,
				TYPE_OVERFLOW_UNDEFINED (type),
				extra_range_p, extra_min, extra_max))
	  {
	    accumulate_range (r, new_lb, new_ub);
	    if (extra_range_p)
	      accumulate_range (r, extra_min, extra_max);
	    return true;
	  }
	r.set_varying (type);
	return false;
      }

    case ABS_EXPR:
      if (wide_int_range_abs (new_lb, new_ub,
			      TYPE_SIGN (r.type ()),
			      TYPE_PRECISION (r.type ()),
			      lh_lb, lh_ub,
			      TYPE_OVERFLOW_UNDEFINED (r.type ())))
	{
	  r.union_ (irange (r.type (), new_lb, new_ub));
	  return true;
	}
      r.set_varying (type);
      return false;

    case ABSU_EXPR:
      wide_int_range_absu (new_lb, new_ub, TYPE_PRECISION (r.type ()),
			   lh_lb, lh_ub);
      r.union_ (irange (unsigned_type_for (r.type ()), new_lb, new_ub));
      return true;

    default:
      return false;
    }
}

/* Perform an operation between 2 ranges.  */

static bool
op_rr (enum tree_code code, irange &r, const irange &lh, const irange &rh)
{
  bool res = false;
  tree type = lh.type ();
  // Clear and set result type.
  r.set_undefined (type);

  if (lh.undefined_p () || rh.undefined_p ())
    return true;

  for (unsigned x = 0; x < lh.num_pairs (); ++x)
    for (unsigned y = 0; y < rh.num_pairs (); ++y)
      {
	wide_int lh_lb = lh.lower_bound (x);
	wide_int lh_ub = lh.upper_bound (x);
	wide_int rh_lb = rh.lower_bound (y);
	wide_int rh_ub = rh.upper_bound (y);
	tree type = rh.type ();
	res = op_wi (code, r, type, lh_lb, lh_ub, rh_lb, rh_ub);
	if (!res)
	  return false;
      }

  return res && !r.varying_p ();
}

/* Perform a unary operation on a range.  TYPE is the type of the
   resulting operation (and thus R).  */

static bool
op_rr_unary (enum tree_code code, irange &r, const irange &lh, tree type)
{
  bool res = false;
  // Clear and set result type.
  r.set_undefined (type);

  if (lh.undefined_p ())
    return true;

  for (unsigned x = 0; x < lh.num_pairs (); ++x)
    {
      wide_int lower_bound = lh.lower_bound (x);
      wide_int upper_bound = lh.upper_bound (x);
      res = op_wi (code, r, type,
		   lower_bound, upper_bound, lower_bound, upper_bound);
      if (!res)
	return false;
    }
  return res && !r.varying_p ();
}

/*  -----------------------------------------------------------------------  */

// Construct and register the range_op handler for treee code C.

trange_operator::trange_operator (enum tree_code c)
{
  code = c;
  range_tree.register_operator (c, this);
}

// Dump the name of this operation.

void 
trange_operator::dump (FILE *f) const
{
  fprintf (f," %s ", get_tree_code_name (code));
}

// Perform the default fold operation of LH OP RH, and return it in R.

bool
trange_operator::fold_range (irange& r, const irange& lh,
			     const irange& rh) const
{
  if (empty_range_check (r, lh, rh, lh.type ()))
    return true;

  return op_rr (code, r, lh, rh);
}

/*  -----------------------------------------------------------------------  */

// Return an irange instance that is a boolean TRUE.

static irange
range_true ()
{
  unsigned prec = TYPE_PRECISION (boolean_type_node);
  return irange (boolean_type_node, wi::one (prec), wi::one (prec));
}

// Return an irange instance that is a boolean FALSE.

static irange
range_false ()
{
  unsigned prec = TYPE_PRECISION (boolean_type_node);
  return irange (boolean_type_node, wi::zero (prec), wi::zero (prec));
}


enum bool_range_state { BRS_FALSE, BRS_TRUE, BRS_EMPTY, BRS_FULL };

/* Return the summary information about boolean range LHS.
   Return an "interesting" range in R.  
   for EMPTY or FULL, return the equivilent range for TYPE,
   for BRS_TRUE and BRS false, return the negatiuon of the bool range.  */
static bool_range_state
get_bool_state (irange& r, const irange& lhs, tree val_type)
{
  /* If there is no result, then this is unexectuable, so no range. */
  if (lhs.undefined_p ())
    {
      r.set_undefined (val_type);
      return BRS_EMPTY;
    }

  // if the bounds arent the same, then its not a constant.  */
  if (!wi::eq_p (lhs.upper_bound (), lhs.lower_bound ()))
    {
      r.set_varying (val_type);
      return BRS_FULL;
    }

  if (lhs.zero_p ())
    return BRS_FALSE;

  return BRS_TRUE;
}


class operator_equal : public trange_operator
{
public:
  operator_equal () : trange_operator (EQ_EXPR) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			       const irange& val) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			       const irange& val) const;
} op_equal;

/* Fold comparison of the 2 ranges.  */
bool
operator_equal::fold_range (irange& r, const irange& op1,
			    const irange& op2) const
{
  if (empty_range_check (r, op1, op2, boolean_type_node))
    return true;

  /* We can be sure the values are always equal or not if both ranges
     consist of a single value, and then compare them.  */
  if (wi::eq_p (op1.lower_bound (), op1.upper_bound ())
      && wi::eq_p (op2.lower_bound (), op2.upper_bound ()))
    {
      if (wi::eq_p (op1.lower_bound (), op2.upper_bound()))
	r = range_true ();
      else
	r = range_false ();
    }
  else
    {
      /* If ranges do not intersect, we know the range is not equal, otherwise
         we don;t really know anything for sure.  */
      r = range_intersect (op1, op2);
      if (r.undefined_p ())
	r = range_false ();
      else
	r.set_varying (boolean_type_node);
    }

  return true;
}

bool
operator_equal::op1_range (irange& r, const irange& lhs,
			    const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.type ()))
    {
      case BRS_FALSE:
        /* If the result is false, the only time we know anything is if OP2 is
	   a constant.  */
	if (wi::eq_p (op2.lower_bound(), op2.upper_bound()))
	  r = range_invert (op2);
	else
	  r.set_varying (op2.type ());
	break;

      case BRS_TRUE:
        /* If its true, the result is the same as OP2.  */
        r = op2;
	break;

      default:
        break;
    }
  return true;
}


bool
operator_equal::op2_range (irange& r, const irange& lhs,
			    const irange& op1) const
{
  return operator_equal::op1_range (r, lhs, op1);
}


/*  -----------------------------------------------------------------------  */

/* Range operator for def = op1 != op2. */

class operator_not_equal : public trange_operator
{
public:
  operator_not_equal () : trange_operator (NE_EXPR) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_not_equal;

/* Fold comparison of the 2 ranges.  */
bool
operator_not_equal::fold_range (irange& r, const irange& op1,
				const irange& op2) const
{
  if (empty_range_check (r, op1, op2, boolean_type_node))
    return true;

  /* We can be sure the values are always equal or not if both ranges
     consist of a single value, and then compare them.  */
  if (wi::eq_p (op1.lower_bound (), op1.upper_bound ())
      && wi::eq_p (op2.lower_bound (), op2.upper_bound ()))
    {
      if (wi::ne_p (op1.lower_bound (), op2.upper_bound()))
	r = range_true ();
      else
	r = range_false ();
    }
  else
    {
      /* If ranges do not intersect, we know the range is not equal, otherwise
         we don;t really know anything for sure.  */
      r = range_intersect (op1, op2);
      if (r.undefined_p ())
	r = range_true ();
      else
	r.set_varying (boolean_type_node);
    }

  return true;
}

/* Calculate the range of op1 being == to VAL based on LHS.  */
bool
operator_not_equal::op1_range (irange& r, const irange& lhs,
				const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.type ()))
    {
      case BRS_TRUE:
        /* If the result is true, the only time we know anything is if OP2 is
	   a constant.  */
	if (wi::eq_p (op2.lower_bound(), op2.upper_bound()))
	  r = range_invert (op2);
	else
	  r.set_varying (op2.type ());
	break;

      case BRS_FALSE:
        /* If its true, the result is the same as OP2.  */
        r = op2;
	break;

      default:
        break;
    }
  return true;
}


bool
operator_not_equal::op2_range (irange& r, const irange& lhs,
				const irange& op1) const
{
  return operator_not_equal::op1_range (r, lhs, op1);
}


/*  -----------------------------------------------------------------------  */


/* (X < VAL) produces the a range of [MIN, VAL - 1]  */
static void
build_lt (irange& r, tree type, const wide_int& val)
{
  wi::overflow_type ov;
  wide_int lim = wi::sub (val, 1, TYPE_SIGN (type), &ov);

  /* If val - 1 underflows, check is X < MIN, which is an empty range.  */
  if (ov)
    r.set_undefined (type);
  else
    r = irange (type, min_limit (type), lim);
}

/* (X <= VAL) produces the a range of [MIN, VAL]  */
static void
build_le (irange& r, tree type, const wide_int& val)
{
  r = irange (type, min_limit (type), val);
}

/* (X > VAL) produces the a range of [VAL + 1, MAX]  */
static void
build_gt (irange& r, tree type, const wide_int& val)
{
  wi::overflow_type ov;
  wide_int lim = wi::add (val, 1, TYPE_SIGN (type), &ov);
  /* If val + 1 overflows, check is for X > MAX , which is an empty range.  */
  if (ov)
    r.set_undefined (type);
  else
    r = irange (type, lim, max_limit (type));
}

/* (X >= val) produces the a range of [VAL, MAX]  */
static void
build_ge (irange& r, tree type, const wide_int& val)
{
  r = irange (type, val, max_limit (type));
}



class operator_lt :  public trange_operator
{
public:
  operator_lt () : trange_operator (LT_EXPR) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_lt;

bool
operator_lt::fold_range (irange& r, const irange& op1, const irange& op2) const
{
  if (empty_range_check (r, op1, op2, boolean_type_node))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::lt_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_true ();
  else
    if (!wi::lt_p (op1.lower_bound (), op2.upper_bound (), sign))
      r = range_false ();
    else 
      r.set_varying (boolean_type_node);
  return true;
}


bool
operator_lt::op1_range (irange& r, const irange& lhs, const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.type ()))
    {
      case BRS_TRUE:
	build_lt (r, op2.type (), op2.upper_bound ());
	break;

      case BRS_FALSE:
	build_ge (r, op2.type (), op2.lower_bound ());
	break;

      default:
        break;
    }
  return true;
}


bool
operator_lt::op2_range (irange& r, const irange& lhs, const irange& op1) const
{
  switch (get_bool_state (r, lhs, op1.type ()))
    {
      case BRS_FALSE:
	build_le (r, op1.type (), op1.upper_bound ());
	break;

      case BRS_TRUE:
	build_gt (r, op1.type (), op1.lower_bound ());
	break;

      default:
        break;
    }
  return true;

}

class operator_le :  public trange_operator
{
public:
  operator_le () : trange_operator (LE_EXPR) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_le;

bool
operator_le::fold_range (irange& r, const irange& op1, const irange& op2) const
{
  if (empty_range_check (r, op1, op2, boolean_type_node))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::le_p (op1.upper_bound (), op2.lower_bound (), sign))
    r = range_true ();
  else
    if (!wi::le_p (op1.lower_bound (), op2.upper_bound (), sign))
      r = range_false ();
    else 
      r.set_varying (boolean_type_node);
  return true;
}

bool
operator_le::op1_range (irange& r, const irange& lhs, const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.type ()))
    {
      case BRS_TRUE:
	build_le (r, op2.type (), op2.upper_bound ());
	break;

      case BRS_FALSE:
	build_gt (r, op2.type (), op2.lower_bound ());
	break;

      default:
        break;
    }
  return true;
}


bool
operator_le::op2_range (irange& r, const irange& lhs, const irange& op1) const
{
  switch (get_bool_state (r, lhs, op1.type ()))
    {
      case BRS_FALSE:
	build_lt (r, op1.type (), op1.upper_bound ());
	break;

      case BRS_TRUE:
	build_ge (r, op1.type (), op1.lower_bound ());
	break;

      default:
        break;
    }
  return true;

}


class operator_gt :  public trange_operator
{
public:
  operator_gt () : trange_operator (GT_EXPR) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_gt;

bool
operator_gt::fold_range (irange& r, const irange& op1, const irange& op2) const
{
  if (empty_range_check (r, op1, op2, boolean_type_node))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::gt_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_true ();
  else
    if (!wi::gt_p (op1.upper_bound (), op2.lower_bound (), sign))
      r = range_false ();
    else 
      r.set_varying (boolean_type_node);

  return true;
}

bool
operator_gt::op1_range (irange& r, const irange& lhs, const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.type ()))
    {
      case BRS_TRUE:
	build_gt (r, op2.type (), op2.lower_bound ());
	break;

      case BRS_FALSE:
	build_le (r, op2.type (), op2.upper_bound ());
	break;

      default:
        break;
    }
  return true;
}


bool
operator_gt::op2_range (irange& r, const irange& lhs, const irange& op1) const
{
  switch (get_bool_state (r, lhs, op1.type ()))
    {
      case BRS_FALSE:
	build_ge (r, op1.type (), op1.lower_bound ());
	break;

      case BRS_TRUE:
	build_lt (r, op1.type (), op1.upper_bound ());
	break;

      default:
        break;
    }
  return true;

}


class operator_ge :  public trange_operator
{
public:
  operator_ge () : trange_operator (GE_EXPR) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_ge;

bool
operator_ge::fold_range (irange& r, const irange& op1, const irange& op2) const
{
  if (empty_range_check (r, op1, op2, boolean_type_node))
    return true;

  signop sign = TYPE_SIGN (op1.type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.type ()));

  if (wi::ge_p (op1.lower_bound (), op2.upper_bound (), sign))
    r = range_true ();
  else
    if (!wi::ge_p (op1.upper_bound (), op2.lower_bound (), sign))
      r = range_false ();
    else 
      r.set_varying (boolean_type_node);

  return true;
} 

bool
operator_ge::op1_range (irange& r, const irange& lhs, const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.type ()))
    {
      case BRS_TRUE:
	build_ge (r, op2.type (), op2.lower_bound ());
	break;

      case BRS_FALSE:
	build_lt (r, op2.type (), op2.upper_bound ());
	break;

      default:
        break;
    }
  return true;
}


bool
operator_ge::op2_range (irange& r, const irange& lhs, const irange& op1) const
{
  switch (get_bool_state (r, lhs, op1.type ()))
    {
      case BRS_FALSE:
	build_gt (r, op1.type (), op1.lower_bound ());
	break;

      case BRS_TRUE:
	build_le (r, op1.type (), op1.upper_bound ());
	break;

      default:
        break;
    }
  return true;

}


class operator_plus : public trange_operator
{
public:
  operator_plus () : trange_operator (PLUS_EXPR) { }
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;

} op_plus;

/* Adjust irange to be in terms of op1. 
   Given [range] = op1 + val,  op1 = [range] - val.  */
bool
operator_plus::op1_range (irange& r, const irange& lhs,
			   const irange& op2) const
{
  return op_rr (MINUS_EXPR, r, lhs, op2);
}

bool
operator_plus::op2_range (irange& r, const irange& lhs,
			   const irange& op1) const
{
  return op_rr (MINUS_EXPR, r, lhs, op1);
}


class operator_minus : public trange_operator
{
public:
  operator_minus () : trange_operator (MINUS_EXPR) { }
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_minus;

/* Adjust irange to be in terms of op1. 
   Given lhs = op1 - op2,  op1 = lhs + op2.  */
bool
operator_minus::op1_range (irange& r, const irange& lhs,
			    const irange& op2) const
{
  return op_rr (PLUS_EXPR, r, lhs, op2);
}

/* Adjust irange to be in terms of op2. 
   Given lhs = op1 - op2,  -op2 = lhs - op1, therefore op2 = op1 - lhs.  */
bool
operator_minus::op2_range (irange& r, const irange& lhs,
			   const irange& op1) const
{
  return op_rr (MINUS_EXPR, r, op1 ,lhs);
}


trange_operator op_mult (MULT_EXPR);
trange_operator op_trunc_div (TRUNC_DIV_EXPR);
trange_operator op_floor_div(FLOOR_DIV_EXPR);
trange_operator op_round_div (ROUND_DIV_EXPR);
trange_operator op_ceil_div (CEIL_DIV_EXPR);
trange_operator op_pointer_plus (POINTER_PLUS_EXPR);
trange_operator op_max (MAX_EXPR);
trange_operator op_min (MIN_EXPR);

class operator_exact_divide : public trange_operator
{
public:
  operator_exact_divide () : trange_operator (EXACT_DIV_EXPR) { }
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;

} op_exact_div;


// Adjust irange to be in terms of op1. 
bool
operator_exact_divide::op1_range (irange& r,
				   const irange& lhs,
				   const irange& op2) const
{
  tree offset;
  // [2, 4] = op1 / [3,3]   since its exact divide, no need to worry about
  // remainders in the endpoints, so op1 = [2,4] * [3,3] = [6,12].
  // We wont bother trying to enumerate all the in between stuff :-P
  // TRUE accuraacy is [6,6][9,9][12,12].  This is unlikely to matter most of
  // the time however.  
  // If op2 is a multiple of 2, we would be able to set some non-zero bits.
  if (op2.singleton_p (&offset) && op_rr (MULT_EXPR, r, lhs, op2)
      && !integer_zerop (offset))
    return true;
  return false;
}


class operator_shift : public trange_operator
{
public:
  operator_shift (enum tree_code c) : trange_operator (c) { }
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
};

operator_shift op_lshift (LSHIFT_EXPR);
operator_shift op_rshift (RSHIFT_EXPR);

bool
operator_shift::op1_range (irange& r, const irange& lhs,
			    const irange& op2) const
{
  tree type = lhs.type ();
  wide_int w2;
  if (empty_range_check (r, lhs, op2, type))
    return true;

  /* FIXME: Andrew: You probably want to use
     wide_int_range_shift_undefined_p() here instead of these checks.
     But I'm not going to touch anything since this entire function
     isn't doing anything but keeping notes to entertain you in your
     sleepless nights.  */

  // Negative shifts are undefined, as well as shift >= precision
  if (wi::lt_p (op2.lower_bound (), 0, TYPE_SIGN (op2.type ())))
    return false;
  if (wi::ge_p (op2.upper_bound (), TYPE_PRECISION (type), UNSIGNED))
    return false;

  return false;
#if 0
  // Check if the calculation can be done without overflows.
  // and if so, adjust the bounds to allow for 1's that may have been shifted
  // out.
  wide_int mask;
  if (code == LSHIFT_EXPR)
    {
      res = op_rr (RSHIFT_EXPR, r, lhs, w2);
      if (res)
        {
	  mask = wi::mask (op, true, r.get_precision ());
	}
    }
  else
    {
      res = op_rr (LSHIFT_EXPR, r, lhs, w2);
    }

  return res;
#endif
}


/*  ----------------------------------------------------------------------  */

class operator_cast: public trange_operator
{
public:
  operator_cast (enum tree_code code) : trange_operator (code) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;

};

operator_cast op_nop (NOP_EXPR);
operator_cast op_convert (CONVERT_EXPR);


/* Return the range of lh converted to the type of rh:
   r = (type_of(rh)) lh.  */

bool
operator_cast::fold_range (irange& r, const irange& lh, const irange& rh) const
{
  if (empty_range_check (r, lh, rh, rh.type ()))
    return true;

  if (lh.type () != rh.type ())
    {
      /* Handle conversion so they become the same type.  */
      r = lh;
      r.cast (rh.type ());
      r.intersect (rh);
    }
  else
    /* If they are the same type, the result should be the intersection of
       the two ranges.  */
    r = range_intersect (lh, rh);
  return true;
}

bool
operator_cast::op1_range (irange& r, const irange& lhs,
			   const irange& op2) const
{
  tree lhs_type = lhs.type ();
  tree op2_type = op2.type ();
  irange op_type;

 
  /* If the precision of the LHS is smaller than the precision of the RHS,
     then there would be truncation of the value on the RHS, and so we can tell
     nothing about it.  */
  if (TYPE_PRECISION (lhs_type) < TYPE_PRECISION (op2_type))
    {
      /* If we've been passed an actual value for the RHS rather than the type
	 see if it fits the LHS, and if so, then we can allow it.  */
      r = op2;
      r.cast (lhs_type);
      r.cast (op2_type);
      if (r == op2)
        {
	  /* We know the value of the RHS fits in the LHS type, so convert the
	     left hand side and remove any values that arent in OP2.  */
	  r = lhs;
	  r.cast (op2_type);
	  r.intersect (op2);
	  return true;
	}
      /* Special case if the LHS is a boolean.  A 0 means the RHS is zero,
	 and a 1 means the RHS is non-zero.  */
      else if (TREE_CODE (lhs_type) == BOOLEAN_TYPE)
	{
	  /* If the LHS is unknown, the result is whatever op2 already is.  */
	  if (!lhs.singleton_p ())
	    {
	      r = op2;
	      return true;
	    }
	  /* Boolean casts are weird in GCC. It's actually an implied mask with
	      0x01, so all that is known is whether the rightmost bit is 0 or 1,
	      which implies the only value *not* in the RHS is 0 or -1.  */
	  unsigned prec = TYPE_PRECISION (op2_type);
	  if (lhs.zero_p ())
	    r = irange (VR_ANTI_RANGE, op2_type,
			wi::minus_one (prec), wi::minus_one (prec));
	  else
	    r = irange (VR_ANTI_RANGE, op2_type,
			wi::zero (prec), wi::zero (prec));
	  /* And intersect it with what we know about op2.  */
	  r.intersect (op2);
	  return true;
	}
      /* Otherwise we'll have to assume it's whatever we know about op2.  */
      r = op2;
      return true;
    }

  /* If the LHS precision is greater than the rhs precision, the LHS range
     is resticted to the range of the RHS by this assignment.  */
  if (TYPE_PRECISION (lhs_type) > TYPE_PRECISION (op2_type))
    {
      /* Cast the range of the RHS to the type of the LHS. */
      op_type.set_varying (op2_type);
      op_type.cast (lhs_type);

      /* Intersect this with the LHS range will produce the RHS range.  */
      r = range_intersect (lhs, op_type);
    }
  else
    r = lhs;

  /* Cast the calculated range to the type of the RHS.  */
  r.cast (op2.type ());

  return true;
}

/*  ----------------------------------------------------------------------  */

// Bitwise and logical ops. 

class operator_logical_and : public trange_operator
{
public:
  operator_logical_and () : trange_operator (TRUTH_AND_EXPR) { }
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_logical_and;


bool
operator_logical_and::fold_range (irange& r, const irange& lh,
				  const irange& rh) const
{
  if (empty_range_check (r, lh, rh, boolean_type_node))
    return true;

  // 0 && anything is 0
  if ((wi::eq_p (lh.lower_bound (), 0) && wi::eq_p (lh.upper_bound (), 0))
      || (wi::eq_p (lh.lower_bound (), 0) && wi::eq_p (rh.upper_bound (), 0)))
    {
      r = range_false ();
      return true;
    }

  // To reach this point, there must be a logical 1 on each side, and the only
  // remaining question is whether there is a zero or not.

  if (lh.contains_p (build_zero_cst (lh.type ()))
      || rh.contains_p (build_zero_cst (rh.type ())))
    r.set_varying (boolean_type_node);
  else
    r = range_true ();
  
  return true;
}



bool
operator_logical_and::op1_range (irange& r, const irange& lhs,
				  const irange& op2) const
{
   switch (get_bool_state (r, lhs, op2.type ()))
     {
       /* A true result means both sides of the AND must be true.  */
       case BRS_TRUE:
         r = range_true ();
	 break;
     
       /* Any other result means only one side has to be false, the other
	  side can be anything. SO we cant be sure of any result here.  */
      default:
        r.set_varying (boolean_type_node);
	break;
    }
  return true;
}

bool
operator_logical_and::op2_range (irange& r, const irange& lhs,
				  const irange& op1) const
{
  return operator_logical_and::op1_range (r, lhs, op1);
}

class operator_bitwise_and : public trange_operator
{
public:
  operator_bitwise_and () : trange_operator (BIT_AND_EXPR) { }
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_bitwise_and;

bool
operator_bitwise_and::op1_range (irange& r, const irange& lhs,
				  const irange& op2) const
{
  /* If this is really a logical operation, call that.  */
  if (types_compatible_p (lhs.type (), boolean_type_node))
    return op_logical_and.op1_range (r, lhs, op2);

  /* For now do nothing with bitwise AND of iranges, just return the type. */
  r.set_varying (lhs.type ());
  return true;
}

bool
operator_bitwise_and::op2_range (irange& r, const irange& lhs,
				  const irange& op1) const
{
  return operator_bitwise_and::op1_range (r, lhs, op1);
}


class operator_logical_or : public trange_operator
{
public:
  operator_logical_or () : trange_operator (TRUTH_OR_EXPR) { }
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_logical_or;


bool
operator_logical_or::fold_range (irange& r, const irange& lh,
				  const irange& rh) const
{
  if (empty_range_check (r, lh, rh, boolean_type_node))
    return true;

  r = range_union (lh, rh);
  return true;
}

bool
operator_logical_or::op1_range (irange& r, const irange& lhs,
				  const irange& op2) const
{
   switch (get_bool_state (r, lhs, op2.type ()))
     {
       /* A false result means both sides of the OR must be false.  */
       case BRS_FALSE:
         r = range_false ();
	 break;
     
       /* Any other result means only one side has to be true, the other
	  side can be anything. SO we cant be sure of any result here.  */
      default:
        r.set_varying (boolean_type_node);
	break;
    }
  return true;
}

bool
operator_logical_or::op2_range (irange& r, const irange& lhs,
				  const irange& op1) const
{
  return operator_logical_or::op1_range (r, lhs, op1);
}

class operator_bitwise_or : public trange_operator
{
public:
  operator_bitwise_or () : trange_operator (BIT_IOR_EXPR) { }
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_bitwise_or;

bool
operator_bitwise_or::op1_range (irange& r, const irange& lhs,
				  const irange& op2) const
{
  /* If this is really a logical operation, call that.  */
  if (types_compatible_p (lhs.type (), boolean_type_node))
    return op_logical_or.op1_range (r, lhs, op2);

  /* For now do nothing with bitwise OR of iranges, just return the type. */
  r.set_varying (lhs.type ());
  return true;
}

bool
operator_bitwise_or::op2_range (irange& r, const irange& lhs,
				  const irange& op1) const
{
  return operator_bitwise_or::op1_range (r, lhs, op1);
}

class operator_bitwise_xor : public trange_operator
{
public:
  operator_bitwise_xor (): trange_operator (BIT_XOR_EXPR) { }
  /* FIXME: Andrew can implement the op1_range and op2_range variants
     when he returns from leave :-P.  */
} op_bitwise_xor;

class operator_trunc_mod : public trange_operator
{
public:
  operator_trunc_mod (): trange_operator (TRUNC_MOD_EXPR) { }
  /* FIXME: Andrew can implement the op1_range and op2_range variants
     when he returns from leave :-P.  */
} op_trunc_mod;

class operator_logical_not : public trange_operator
{
public:
  operator_logical_not () : trange_operator (TRUTH_NOT_EXPR) { }
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
} op_logical_not;


/* Folding a logical NOT, oddly enough, involves doing nothing on the
   forward pass thru.  During the initial walk backwards, the logical NOT
   reversed the desired outcome on the way back, so on the way forward all
   we do is pass the range forward.
	b_2 = x_1 < 20
	b_3 = !b_2
	if (b_3)
    to determine the TRUE branch, walking  backward
         if (b_3)		if ([1,1])
         b_3 = !b_2		[1,1] = ![0,0]
	 b_2 = x_1 < 20		[0,0] = x_1 < 20,   false, so x_1 == [20, 255]
     which is te result we are looking for.. so.. pass it thru.  */
      
bool
operator_logical_not::fold_range (irange& r, const irange& lh,
				  const irange& rh ATTRIBUTE_UNUSED) const
{
  if (empty_range_check (r, lh, rh, boolean_type_node))
    return true;

  if (lh.varying_p () || lh.undefined_p ())
    r = lh;
  else
    r = range_invert (lh);
  return true;
}

bool
operator_logical_not::op1_range (irange& r, const irange& lhs,
				  const irange& op2 ATTRIBUTE_UNUSED) const
{
  if (lhs.varying_p () || lhs.undefined_p ())
    r = lhs;
  else
    r = range_invert (lhs);
  return true;
}


class operator_bitwise_not : public trange_operator
{
public:
  operator_bitwise_not () : trange_operator (BIT_NOT_EXPR) { }
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
} op_bitwise_not;

bool
operator_bitwise_not::fold_range (irange& r, const irange& lh,
				  const irange& rh) const
{
  tree type = lh.type ();
  if (empty_range_check (r, lh, rh, type))
    return true;

  // ~X is simply -1 - X.
  irange minusone (type,
		   wi::minus_one (TYPE_PRECISION (type)),
		   wi::minus_one (TYPE_PRECISION (type)));
  return op_rr (MINUS_EXPR, r, minusone, lh);
}

bool
operator_bitwise_not::op1_range (irange& r, const irange& lhs,
				  const irange& op2 ATTRIBUTE_UNUSED) const
{
  tree type = lhs.type ();

  // ~X is -1 - X and since bitwise NOT is involutary...do it again.
  irange minusone (type,
		   wi::minus_one (TYPE_PRECISION (type)),
		   wi::minus_one (TYPE_PRECISION (type)));
  return op_rr (MINUS_EXPR, r, minusone, lhs);
}


/*  ----------------------------------------------------------------------  */


class operator_cst : public trange_operator
{
public:
  operator_cst () : trange_operator (INTEGER_CST) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
} op_integer_cst;


bool
operator_cst::fold_range (irange& r, const irange& lh,
			  const irange& rh ATTRIBUTE_UNUSED) const
{
  r = lh;
  return true;
}


class operator_ssa_name : public trange_operator
{
public:
  operator_ssa_name () : trange_operator (SSA_NAME) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
} op_ssa_name;

bool
operator_ssa_name::fold_range (irange& r, const irange& lh,
			  const irange& rh ATTRIBUTE_UNUSED) const
{
  r = lh;
  return true;
}

bool
operator_ssa_name::op1_range (irange& r, const irange& lhs,
			       const irange& op2 ATTRIBUTE_UNUSED) const
{
  r = lhs;
  return true;
}


// Unary identity function.
class operator_identity : public trange_operator
{
 public:
  operator_identity (enum tree_code c) : trange_operator (c) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange &op2 ATTRIBUTE_UNUSED) const
  { r = op1; return false; }
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange &op2 ATTRIBUTE_UNUSED) const
  { r = lhs; return false; }
} op_paren (PAREN_EXPR), op_obj_type_ref (OBJ_TYPE_REF);

class operator_abs : public trange_operator
{
 public:
  operator_abs (enum tree_code code) : trange_operator (code) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r,
			   const irange& lhs, const irange& op2) const;
} op_abs (ABS_EXPR), op_absu (ABSU_EXPR);

bool
operator_abs::fold_range (irange &r,
			  const irange &lh, const irange& rh) const
{
  tree type = rh.type ();
  if (empty_range_check (r, lh, rh, type))
    return true;

  return op_rr_unary (code, r, lh, type);
}

bool
operator_abs::op1_range (irange& r,
			  const irange& lhs, const irange& op2) const
{
  // FIXME: ?? Andrew TODO ??
  if (code == ABSU_EXPR)
    return false;

  tree type = lhs.type ();
  if (empty_range_check (r, lhs, op2, type))
    return true;
  if (TYPE_UNSIGNED (type))
    {
      r = lhs;
      return true;
    }
  // Start with the positives because negatives are an impossible result.
  irange positives = range_positives (type);
  positives.intersect (lhs);
  r = positives;
  // Then add the negative of each pair:
  // ABS(op1) = [5,20] would yield op1 => [-20,-5][5,20].
  for (unsigned i = 0; i < positives.num_pairs (); ++i)
    r.union_ (irange (type,
		      -positives.upper_bound (i),
		      -positives.lower_bound (i)));
  return true;
}

class operator_negate : public trange_operator
{
 public:
  operator_negate () : trange_operator (NEGATE_EXPR) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const
    { return fold_range (r, lhs, op2); } // NEGATE is involutory :-P.
} op_negate;

/* Return the negated range of lh with the type of rh.  */

bool
operator_negate::fold_range (irange &r,
			     const irange &lh, const irange& rh) const
{
  tree type = rh.type ();
  if (empty_range_check (r, lh, rh, type))
    return true;
  // -X is simply 0 - X.
  return op_rr (MINUS_EXPR, r, range_zero (type), lh);
}

// Disable for now for VRP parity.
#if 0
class operator_min_max : public trange_operator
{
public:
  operator_min_max (tree_code c) : trange_operator (c) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_min (MIN_EXPR), op_max (MAX_EXPR);


bool
operator_min_max::fold_range (irange& r, const irange& lh,
			      const irange& rh) const
{
  wide_int lb, ub;
  wi::overflow_type ov;
  tree type = lh.type ();

  if (empty_range_check (r, lh, rh, type))
    return true;

  // Start with the union of both ranges  
  r = range_union (lh, rh);

  // For pointer types we are concerned with NULL and NON-NULL.
  // Min max result in this case is a strict union.
  if (POINTER_TYPE_P (type))
    return true;

  // Intersect the union with the max/min values of both to get a set
  // of values.  This allows MIN ([1,5][20,30] , [0,4][18,60]) to
  // produce [0,5][18,30] rather than [0,30]
  wide_int_binop (lb, code, lh.lower_bound (), rh.lower_bound (),
		  TYPE_SIGN (type), &ov);
  wide_int_binop (ub, code, lh.upper_bound (), rh.upper_bound (),
		  TYPE_SIGN (type), &ov);
  r.intersect (irange (type, lb, ub));
  return true;
}

bool
operator_min_max::op1_range (irange& r, const irange& lhs,
			      const irange& op2) const
{
  if (empty_range_check (r, lhs, op2, lhs.type ()))
    return true;

  if (POINTER_TYPE_P (lhs.type ()))
    return false;
  
  // Until this can be examined closer...  Im not convinces this is right
  return false;

  wide_int lb = lhs.lower_bound ();
  wide_int ub = lhs.upper_bound ();
  if (code == MIN_EXPR)
    {
      // If the upper bound is set by the other operand, we have no idea
      // what this upper could be, otherwise it HAS to be the upper bound.
      if (wi::eq_p (lhs.upper_bound (), op2.upper_bound ()))
	ub = max_limit (lhs.type ());
    }
  else
    {
      // this operand.   Otherwise, it could be any value to MAX_TYPE as the
      // upper bound comes from the other operand.
      if (wi::eq_p (lhs.lower_bound (), op2.lower_bound ()))
	lb = min_limit (lhs.type ());
    }

  r = irange (lhs.type (), lb, ub);
  return true;
}

bool
operator_min_max::op2_range (irange& r, const irange& lhs,
			      const irange& op1) const
{
  return operator_min_max::op1_range (r, lhs, op1);
}
#endif // if 0

class operator_addr_expr : public trange_operator
{
public:
  operator_addr_expr () : trange_operator (ADDR_EXPR) { }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
} op_addr;

bool
operator_addr_expr::fold_range (irange& r, const irange& lh,
			  const irange& rh) const
{
  if (empty_range_check (r, lh, rh, rh.type ()))
    return true;

  // Return a non-null pointer of the LHS type (passed in op2)
  if (lh.zero_p ())
    r = range_zero (rh.type ());
  else
    if (!lh.contains_p (build_zero_cst (lh.type ())))
      r = range_nonzero (rh.type ());
    else
      return false;
  return true;
}

// The same functionality for fold() applies to op1_range...
// effectively copying the non-nullness.
bool
operator_addr_expr::op1_range (irange& r, const irange& lhs,
			        const irange& op2) const
{
  return operator_addr_expr::fold_range (r, lhs, op2);
}

