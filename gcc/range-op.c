/* Code for range operators.
   Copyright (C) 2017 Free Software Foundation, Inc.
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

inline wide_int
max_limit (const_tree type)
{
  return wi::max_value (TYPE_PRECISION (type) , TYPE_SIGN (type));
}

inline wide_int
min_limit (const_tree type)
{
  return wi::min_value (TYPE_PRECISION (type) , TYPE_SIGN (type));
}

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
   overflow bits to determine how to add [lbound, ubound] into range R.

   This is the irange implementation of set_value_range_with_overflow,
   which we can't share because the VRP implementation deals with
   yucky anti ranges.

   FIXME: Add a note to set_value_range_with_overflow to keep
   these two functions in sync.  */

static void
accumulate_range (irange& r,
		  const wide_int &lb, wi::overflow_type ov_lb,
		  const wide_int &ub, wi::overflow_type ov_ub,
		  bool overflow_wraps = false)
{
  tree type = r.type ();

  /* For one bit precision if max < min, then the swapped
     range covers all values.  */
  if (TYPE_PRECISION (type) == 1 && wi::lt_p (ub, lb, TYPE_SIGN (type)))
    {
      r.set_varying (type);
      return;
    }

  if (overflow_wraps)
    {
      // No overflow or both overflow or underflow.  The range can be
      // used as is.
      if (ov_lb == ov_ub)
	r.union_ (irange (type, lb, ub));
      // Underflow on the lower bound.
      else if (ov_lb == wi::OVF_UNDERFLOW && ov_ub == wi::OVF_NONE)
	{
	  r.union_ (irange (type, min_limit (type), ub));
	  r.union_ (irange (type, lb, max_limit (type)));
	}
      // Overflow on the upper bound.
      else if (ov_lb == wi::OVF_NONE && ov_ub == wi::OVF_OVERFLOW)
	{
	  r.union_ (irange (type, lb, max_limit (type)));
	  r.union_ (irange (type, min_limit (type), ub));
	}
      else
	r.set_varying (type);
    }
  else
    {
      // If overflow does not wrap, saturate to the types min/max value.
      wide_int new_lb, new_ub;
      if (ov_lb == wi::OVF_UNDERFLOW)
	new_lb = min_limit (type);
      else if (ov_lb == wi::OVF_OVERFLOW)
	new_lb = max_limit (type);
      else
	new_lb = lb;

      if (ov_ub == wi::OVF_UNDERFLOW)
	new_ub = min_limit (type);
      else if (ov_ub == wi::OVF_OVERFLOW)
	new_ub = max_limit (type);
      else
	new_ub = ub;
      r.union_ (irange (type, new_lb, new_ub));
    }
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

/*  ------------------------------------------------------------------------  */

/* Defaults for all remaining operations are to return NULL. This means no
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

/*  -----------------------------------------------------------------------  */

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


class operator_equal : public range_operator
{
public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			       const irange& val) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			       const irange& val) const;
} op_equal;

void 
operator_equal::dump (FILE *f) const
{
  fprintf (f, " == ");
}

static irange
range_true ()
{
  unsigned prec = TYPE_PRECISION (boolean_type_node);
  return irange (boolean_type_node, wi::one (prec), wi::one (prec));
}

static irange
range_false ()
{
  unsigned prec = TYPE_PRECISION (boolean_type_node);
  return irange (boolean_type_node, wi::zero (prec), wi::zero (prec));
}

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

class operator_not_equal : public range_operator
{
public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_not_equal;

void 
operator_not_equal::dump (FILE *f) const
{
  fprintf (f, " != ");
}

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



class operator_lt :  public range_operator
{
public:
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;

  virtual void dump (FILE *f) const;
} op_lt;

void 
operator_lt::dump (FILE *f) const
{
  fprintf (f, " < ");
}

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

class operator_le :  public range_operator
{
public:
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;

  virtual void dump (FILE *f) const;
} op_le;

void 
operator_le::dump (FILE *f) const
{
  fprintf (f, " <= ");
}

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


class operator_gt :  public range_operator
{
public:
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;

  virtual void dump (FILE *f) const;
} op_gt;

void 
operator_gt::dump (FILE *f) const
{
  fprintf (f, " > ");
}

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


class operator_ge :  public range_operator
{
public:
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;

  virtual void dump (FILE *f) const;
} op_ge;

void 
operator_ge::dump (FILE *f) const
{
  fprintf (f, " >= ");
}

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
			const wide_int &vr1_max)
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
	 whether the expression evaluates to non-NULL.  */
      if (!wide_int_range_includes_zero_p (vr0_min, vr0_max, sign)
	  || !wide_int_range_includes_zero_p (vr1_min, vr1_max, sign))
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
  n = wide_int_range_pointer (code, s, lh_lb, lh_ub, rh_lb, rh_ub);
  if (n == WIDE_INT_RANGE_UNKNOWN)
    r.set_varying (type);
  else if (n == WIDE_INT_RANGE_NULL)
    {
      irange zero;
      range_zero (&zero, type);
      r.union_ (zero);
    }
  else if (n == WIDE_INT_RANGE_NONNULL)
    {
      irange nzero;
      range_non_zero (&nzero, type);
      r.union_ (nzero);
    }
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
  /* If the resulting range contains 0, AND the least significant bit
     of mask is not set, we can improve the range by making the least
     significant bit set the minimum, and adding in the 0.

     For example, A & 0x3C returns a range of [0,60].  We can improve
     this to [0,0][4, 60].  */
  wide_int mask, lower_bound, upper_bound;
  int tz;
  tree type = r.type ();
  irange zero;
  range_zero (&zero, type);
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
	  range_positives (&positives, type);
	  range_negatives (&negatives, type);
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
	    accumulate_range (r, new_lb, new_ub);
	    irange_adjust_bit_and_mask (r, s, lh_lb, lh_ub, rh_lb, rh_ub);
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

    default:
      return false;
    }
}

/* Perform an operation between a constant and a range.  */

static bool
op_ir (enum tree_code code, irange &r, const wide_int &lh, const irange &rh)
{
  r.set_undefined (r.type ());
  for (unsigned x = 0; x < rh.num_pairs () ; x++)
    if (!op_wi (code, r, rh.type (),
		lh, lh, rh.lower_bound (x), rh.upper_bound (x)))
      return false;
  return true;
}

/* Perform an operation between a range and a constant.  */

static bool
op_ri (enum tree_code code, irange &r, tree rh_type,
       const irange &lh, const wide_int &rh)
{
  r.set_undefined (r.type ());
  for (unsigned x = 0; x < lh.num_pairs () ; x++)
    if (!op_wi (code, r, rh_type,
		lh.lower_bound (x), lh.upper_bound (x), rh, rh))
      return false;
  return true;
}

/* Perform an operation between 2 ranges.  */

static bool
op_rr (enum tree_code code, irange& r, const irange& lh, const irange& rh)
{
  bool res = false;
  tree type = lh.type ();
  // Clear and set result type.
  r.set_undefined (type);

  if (lh.undefined_p () || rh.undefined_p ())
    return true;

  /* Try constant cases first to see if we do anything special with them. */
  if (wi::eq_p (lh.upper_bound (), lh.lower_bound ()))
    res = op_ir (code, r, lh.upper_bound (), rh);

  if (!res && wi::eq_p (rh.upper_bound (), rh.lower_bound ()))
    res = op_ri (code, r, rh.type (), lh, rh.upper_bound ());

  if (!res)
    {
      for (unsigned x = 0; x < lh.num_pairs (); ++x)
	for (unsigned y = 0; y < rh.num_pairs (); ++y)
	  {
	    res = op_wi (code, r, rh.type (),
			 lh.lower_bound (x), lh.upper_bound (x),
			 rh.lower_bound (y), rh.upper_bound (y));
	    if (!res)
	      return false;
	  }
    }

  return res && !r.varying_p ();
}

/* Perform a unary operation on a range.  */

static bool
op_rr_unary (enum tree_code code, irange &r, const irange &lh)
{
  bool res = false;
  tree type = lh.type ();
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

class basic_operator : public range_operator
{
private:
  enum tree_code code;
public:
  basic_operator (enum tree_code c);
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
};

basic_operator::basic_operator (enum tree_code c)
{
  code = c;
}

void 
basic_operator::dump (FILE *f) const
{
  fprintf (f," %s ", get_tree_code_name (code));
}

bool
basic_operator::fold_range (irange& r, const irange& lh, const irange& rh) const
{
  if (empty_range_check (r, lh, rh, lh.type ()))
    return true;

  return op_rr (code, r, lh, rh);
}


class operator_plus : public basic_operator
{
public:
  operator_plus (): basic_operator (PLUS_EXPR) { }
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


class operator_minus : public basic_operator
{
public:
  operator_minus () : basic_operator (MINUS_EXPR) { }
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


basic_operator op_mult (MULT_EXPR);
basic_operator op_trunc_div (TRUNC_DIV_EXPR);
basic_operator op_floor_div(FLOOR_DIV_EXPR);
basic_operator op_round_div (ROUND_DIV_EXPR);
basic_operator op_ceil_div (CEIL_DIV_EXPR);

class operator_exact_divide : public basic_operator
{
public:
  operator_exact_divide () : basic_operator (EXACT_DIV_EXPR) { }
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;

} op_exact_div;


// Adjust irange to be in terms of op1. 
bool
operator_exact_divide::op1_range (irange& r,
				   const irange& lhs,
				   const irange& op2) const
{
  wide_int offset;
  // [2, 4] = op1 / [3,3]   since its exact divide, no need to worry about
  // remainders in the endpoints, so op1 = [2,4] * [3,3] = [6,12].
  // We wont bother trying to enumerate all the in between stuff :-P
  // TRUE accuraacy is [6,6][9,9][12,12].  This is unlikely to matter most of
  // the time however.  
  // If op2 is a multiple of 2, we would be able to set some non-zero bits.
  if (op2.singleton_p (offset) && op_rr (MULT_EXPR, r, lhs, op2)
      && wi::ne_p (offset, 0))
    return true;
  return false;
}


class operator_shift : public basic_operator
{
public:
  operator_shift (enum tree_code c) : basic_operator (c) { }
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

class operator_cast: public range_operator
{
public:
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;

} op_cast;

/* Unary operators put the range for the type of the expression as op2.  */

void
operator_cast::dump (FILE *f) const
{
  fprintf (f, "(cast)");
}

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
      // If we've been passed an actual value for the RHS rather than the type
      // see if it fits the LHS, and if so, then we can allow it.
      r = op2;
      r.cast (lhs_type);
      r.cast (op2_type);
      if (r == op2)
        {
	  // we know the value of the RHS fits in the LHS type, so convert the
	  // left hand side and remove any values that arent in OP2.
	  r = lhs;
	  r.cast (op2_type);
	  r.intersect (op2);
	}
      else
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

class operator_logical_and : public range_operator
{
public:
  void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_logical_and;

void 
operator_logical_and::dump (FILE *f) const
{
  fprintf (f, " && ");
}


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

  if (lh.contains_p (wi::zero (TYPE_PRECISION (lh.type ())))
      || rh.contains_p (wi::zero (TYPE_PRECISION (rh.type ()))))
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

class operator_bitwise_and : public basic_operator
{
public:
  operator_bitwise_and () : basic_operator (BIT_AND_EXPR) { }
  virtual void dump (FILE *f) const { fprintf (f, " & "); }
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


class operator_logical_or : public range_operator
{
public:
  void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_logical_or;

void 
operator_logical_or::dump (FILE *f) const
{
  fprintf (f, " || ");
}


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

class operator_bitwise_or : public basic_operator
{
public:
  operator_bitwise_or () : basic_operator (BIT_IOR_EXPR) { }
  virtual void dump (FILE *f) const { fprintf (f, " | "); }
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

class operator_bitwise_xor : public basic_operator
{
public:
  operator_bitwise_xor (): basic_operator (BIT_XOR_EXPR) { }
  /* FIXME: Andrew can implement the op1_range and op2_range variants
     when he returns from leave :-P.  */
} op_bitwise_xor;

class operator_trunc_mod : public basic_operator
{
public:
  operator_trunc_mod (): basic_operator (TRUNC_MOD_EXPR) { }
  /* FIXME: Andrew can implement the op1_range and op2_range variants
     when he returns from leave :-P.  */
} op_trunc_mod;

class operator_logical_not : public range_operator
{
public:
  void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
} op_logical_not;

void 
operator_logical_not::dump (FILE *f) const
{
  fprintf (f, " ! ");
}

/* Folding a logical NOT, oddly enough, invlves doing nothing on the
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


class operator_bitwise_not : public range_operator
{
public:
  void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
} op_bitwise_not;

void 
operator_bitwise_not::dump (FILE *f) const
{
  fprintf (f, " ~ ");
}

bool
operator_bitwise_not::fold_range (irange& r, const irange& lh,
				  const irange& rh) const
{
  tree type = lh.type ();
  if (empty_range_check (r, lh, rh, type))
    return true;

  /* If this is a boolean not, call the logical version.  */
  if (types_compatible_p (type, boolean_type_node))
    return op_logical_not.fold_range (r, lh, rh);

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
  /* If this is a boolean not, call the logical version.  */
  if (types_compatible_p (type, boolean_type_node))
    return op_logical_not.op1_range (r, lhs, op2);

  // ~X is -1 - X and since bitwise NOT is involutary...do it again.
  irange minusone (type,
		   wi::minus_one (TYPE_PRECISION (type)),
		   wi::minus_one (TYPE_PRECISION (type)));
  return op_rr (MINUS_EXPR, r, minusone, lhs);
}


/*  ----------------------------------------------------------------------  */


class operator_cst : public range_operator
{
public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
} op_integer_cst;

void 
operator_cst::dump (FILE *f) const
{
  fprintf (f, " const ");
}

bool
operator_cst::fold_range (irange& r, const irange& lh,
			  const irange& rh ATTRIBUTE_UNUSED) const
{
  r = lh;
  return true;
}



class operator_ssa_name : public range_operator
{
public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
} op_ssa_name;

void 
operator_ssa_name::dump (FILE *f) const
{
  fprintf (f, " SSA_NAME ");
}

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


class operator_pointer_plus : public range_operator
{
public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
} op_pointer_plus;

void 
operator_pointer_plus::dump (FILE *f) const
{
  fprintf (f, " POINTER_PLUS ");
}

bool
operator_pointer_plus::fold_range (irange& r, const irange& lh,
				   const irange& rh) const
{
  if (empty_range_check (r, lh, rh, lh.type ()))
    return true;

  // For pointer types we are mostly concerned with NULL and NON-NULL.
  // If either side is non-null, the result will be non-null.
  if (!lh.contains_p (wi::zero (TYPE_PRECISION (lh.type ())))
      || !rh.contains_p (wi::zero (TYPE_PRECISION (rh.type ()))))
    range_non_zero (&r, lh.type ());
  else
    {
      // Can't perform a union since the RH is a different type from the LH.
      if (lh.zero_p () && rh.zero_p ())
        r = lh;
      else
	r.set_varying (lh.type ());
    }
  return true;
}

// Unary identity function.
class operator_identity : public range_operator
{
  tree_code code;
 public:
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange &op2 ATTRIBUTE_UNUSED) const
  { r = op1; return false; }
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange &op2 ATTRIBUTE_UNUSED) const
  { r = lhs; return false; }
} op_identity;

void
operator_identity::dump (FILE *f) const
{
  if (code == PAREN_EXPR)
    fprintf (f, " PAREN ");
  else if (code == OBJ_TYPE_REF)
    fprintf (f, " OBJ_TYPE ");
  else
    gcc_unreachable ();
}

class operator_abs : public range_operator
{
 public:
  virtual void dump (FILE *f) const { fprintf (f, " ABS_EXPR "); }
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r,
			   const irange& lhs, const irange& op2) const;
} op_abs;

bool
operator_abs::fold_range (irange &r,
			  const irange &lh, const irange& rh) const
{
  tree type = rh.type ();
  if (empty_range_check (r, lh, rh, type))
    return true;

  return op_rr_unary (ABS_EXPR, r, lh);
}

bool
operator_abs::op1_range (irange& r,
			  const irange& lhs, const irange& op2) const
{
  tree type = lhs.type ();
  if (empty_range_check (r, lhs, op2, type))
    return true;
  if (TYPE_UNSIGNED (type))
    {
      r = lhs;
      return true;
    }
  // Start with the positives because negatives are an impossible result.
  irange positives;
  range_positives (&positives, type);
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

class operator_negate : public range_operator
{
  tree_code code;
 public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const
    { return fold_range (r, lhs, op2); } // NEGATE is involutory :-P.
} op_negate;

void
operator_negate::dump (FILE *f) const
{
  fprintf (f, " - ");
}

/* Return the negated range of lh with the type of rh.  */

bool
operator_negate::fold_range (irange &r,
			     const irange &lh, const irange& rh) const
{
  tree type = rh.type ();
  if (empty_range_check (r, lh, rh, type))
    return true;
  // -X is simply 0 - X.
  irange zero;
  range_zero (&zero, type);
  return op_rr (MINUS_EXPR, r, zero, lh);
}

class operator_min_max : public range_operator
{
  tree_code code;
public:
  operator_min_max (tree_code c);
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_range (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_min (MIN_EXPR), op_max (MAX_EXPR);

operator_min_max::operator_min_max (tree_code c)
{
  gcc_assert (c == MIN_EXPR || c == MAX_EXPR);
  code = c;
}

void 
operator_min_max::dump (FILE *f) const
{
  fprintf (f," %s ", get_tree_code_name (code));
}

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

  // intersect the union with the max/min values of both to get a set of values.
  // This allows   MIN  ([1,5][20,30] , [0,4][18,60])  to produce 
  // [0,5][18,30]  rather than [0,30]
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




class operator_addr_expr : public range_operator
{
public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_range (irange& r, const irange& lhs,
			   const irange& op2) const;
} op_addr;

void 
operator_addr_expr::dump (FILE *f) const
{
  fprintf (f, " ADDR_EXPR ");
}

bool
operator_addr_expr::fold_range (irange& r, const irange& lh,
			  const irange& rh) const
{
  if (empty_range_check (r, lh, rh, rh.type ()))
    return true;

  // Return a non-null pointer of the LHS type (passed in op2)
  if (lh.zero_p ())
    range_zero (&r, rh.type ());
  else
    if (!lh.contains_p (wi::zero (TYPE_PRECISION (lh.type ()))))
      range_non_zero (&r, rh.type ());
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
/*  ----------------------------------------------------------------------  */

/* Create the range operator table as a local object in this file, and the
   constructor should be automatically called before it is used.  */

class range_op_table
{
public:
  range_op_table ();
  inline range_operator *operator[] (enum tree_code code);
private:
  range_operator *m_range_tree[MAX_TREE_CODES];
} range_tree;

range_operator *range_op_table::operator[] (enum tree_code code)
{
  gcc_assert (code > 0 && code < MAX_TREE_CODES);
  return m_range_tree[code];
}

range_op_table::range_op_table ()
{
  m_range_tree[LT_EXPR] = &op_lt;
  m_range_tree[LE_EXPR] = &op_le;
  m_range_tree[GT_EXPR] = &op_gt;
  m_range_tree[GE_EXPR] = &op_ge;
  m_range_tree[NE_EXPR] = &op_not_equal;
  m_range_tree[EQ_EXPR] = &op_equal;

  m_range_tree[PLUS_EXPR] = &op_plus;
  m_range_tree[MINUS_EXPR] = &op_minus;
  m_range_tree[MULT_EXPR] = &op_mult;
  m_range_tree[TRUNC_DIV_EXPR] = &op_trunc_div;
  m_range_tree[FLOOR_DIV_EXPR] = &op_floor_div;
  m_range_tree[ROUND_DIV_EXPR] = &op_round_div;
  m_range_tree[CEIL_DIV_EXPR] = &op_ceil_div;
  m_range_tree[EXACT_DIV_EXPR] = &op_exact_div;
  
  m_range_tree[NOP_EXPR] = &op_cast;
  m_range_tree[CONVERT_EXPR] = &op_cast;

  m_range_tree[TRUTH_AND_EXPR] = &op_logical_and;
  m_range_tree[TRUTH_OR_EXPR] = &op_logical_or;
  m_range_tree[TRUTH_NOT_EXPR] = &op_logical_not;

  m_range_tree[BIT_AND_EXPR] = &op_bitwise_and;
  m_range_tree[BIT_IOR_EXPR] = &op_bitwise_or;
  m_range_tree[BIT_XOR_EXPR] = &op_bitwise_xor;
  m_range_tree[BIT_NOT_EXPR] = &op_bitwise_not;

  m_range_tree[INTEGER_CST] = &op_integer_cst;
  m_range_tree[SSA_NAME] = &op_ssa_name;

  m_range_tree[ABS_EXPR] = &op_abs;
  m_range_tree[MIN_EXPR] = &op_min;
  m_range_tree[MAX_EXPR] = &op_max;
  m_range_tree[NEGATE_EXPR] = &op_negate;
  m_range_tree[PAREN_EXPR] = &op_identity;
  m_range_tree[OBJ_TYPE_REF] = &op_identity;
  m_range_tree[POINTER_PLUS_EXPR] = &op_pointer_plus;
  m_range_tree[TRUNC_MOD_EXPR] = &op_trunc_mod;

  m_range_tree[LSHIFT_EXPR] = &op_lshift;
  m_range_tree[RSHIFT_EXPR] = &op_rshift;

  m_range_tree[ADDR_EXPR] = &op_addr;
}

/* The table is hidden and accessed via a simple extern function.  */

range_operator *
range_op_handler (enum tree_code code)
{
  return range_tree[code];
}

//   GIMPLE extensions

// Return the First operand of this statement if it is a valid operand 
// supported by ranges, otherwise return NULL_TREE. 

tree
grange_op::operand1 () const
{
  switch (gimple_code (this))
    {
      case GIMPLE_COND:
        return gimple_cond_lhs (this);
      case GIMPLE_ASSIGN:
        {
	  tree expr = gimple_assign_rhs1 (this);
	  if (gimple_assign_rhs_code (this) == ADDR_EXPR)
	    {
	      // If the base address is an SSA_NAME, we return it here.
	      // This allows processing of the range of that name, while the
	      // rest of the expression is simply ignored.  The code in
	      // range_ops will see the ADDR_EXPR and do the right thing.
	      tree base = get_base_address (TREE_OPERAND (expr, 0));
	      if (base != NULL_TREE && TREE_CODE (base) == MEM_REF)
	        {
		  // If the base address is an SSA_NAME, return it. 
		  tree b = TREE_OPERAND (base, 0);
		  if (TREE_CODE (b) == SSA_NAME)
		    return b;
		}
	    }
	  return expr;
	}
      default:
        break;
    }
  return NULL;
}


// Fold this unary statement uusing R1 as operand1's range, returning the
// result in RES.  Return false if the operation fails.

bool
grange_op::fold (irange &res, const irange &r1) const
{
  irange r2;
  // Single ssa operations require the LHS type as the second range.
  if (lhs ())
    r2.set_varying (TREE_TYPE (lhs ()));
  else
    r2.set_undefined (r1.type ());

  return handler()->fold_range (res, r1, r2);
}


// Fold this binary statement using R1 and R2 as the operands ranges,
// returning the result in RES.  Return false if the operation fails.

bool
grange_op::fold (irange &res, const irange &r1, const irange &r2) const
{
  // Make sure this isnt a unary operation being passed a second range.
  return handler()->fold_range (res, r1, r2);
}

// Calculate what we can determine of the range of this unary statement's
// operand if the lhs of the expression has the range LHS_RANGE.  Return false
// if nothing can be determined.

bool
grange_op::calc_op1_irange (irange &r, const irange &lhs_range) const
{  
  irange type_range;
  gcc_checking_assert (gimple_num_ops (this) < 3);
  // An empty range is viral, so return an empty range.
  if (lhs_range.undefined_p ())
    {
      r.set_undefined (TREE_TYPE (operand1 ()));
      return true;
    }
  // Unary operations require the type of the first operand in the second range
  // position.
  type_range.set_varying (TREE_TYPE (operand1 ()));
  return handler ()->op1_range (r, lhs_range, type_range);
}

// Calculate what we can determine of the range of this statement's first 
// operand if the lhs of the expression has the range LHS_RANGE and the second
// operand has the range OP2_RANGE.  Return false if nothing can be determined.

bool
grange_op::calc_op1_irange (irange &r, const irange &lhs_range,
			    const irange &op2_range) const
{  
  // Unary operation are allowed to pass a range in for second operand
  // as there are often additional restrictions beyond the type which can
  // be imposed.  See operator_cast::op1_irange.()
  
  // An empty range is viral, so return an empty range.
  if (op2_range.undefined_p () || lhs_range.undefined_p ())
    {
      r.set_undefined (op2_range.type ());
      return true;
    }
  return handler ()->op1_range (r, lhs_range, op2_range);
}

// Calculate what we can determine of the range of this statement's second
// operand if the lhs of the expression has the range LHS_RANGE and the first
// operand has the range OP1_RANGE.  Return false if nothing can be determined.

bool
grange_op::calc_op2_irange (irange &r, const irange &lhs_range,
			    const irange &op1_range) const
{  
  // An empty range is viral, so return an empty range.
  if (op1_range.undefined_p () || lhs_range.undefined_p ())
    {
      r.set_undefined (op1_range.type ());
      return true;
    }
  return handler ()->op2_range (r, lhs_range, op1_range);
}


