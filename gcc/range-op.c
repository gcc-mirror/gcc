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
  if (op1.empty_p () || op2.empty_p ())
    {
      r.clear (type);
      return true;
    }
  else
    return false;
}

/* Given newly calculated lbound and ubound, examine their respective
   overflow bits to determine how to add [lbound,ubound] into range R.

   This is the irange implementation of set_value_range_with_overflow,
   which we can't share because the VRP implementation deals with
   yucky anti ranges.

   FIXME: Add a note to set_value_range_with_overflow to keep
   these two functions in sync.  */

static void
accumulate_range (irange& r,
		  const wide_int &lb, wi::overflow_type ov_lb,
		  const wide_int &ub, wi::overflow_type ov_ub,
		  bool overflow_wraps)
{
  tree type = r.get_type ();
  if (overflow_wraps)
    {
      // No overflow or both overflow or underflow.  The range can be
      // used as is.
      if (ov_lb == ov_ub)
	r.union_ (lb, ub);
      // Underflow on the lower bound.
      else if (ov_lb == wi::OVF_UNDERFLOW && ov_ub == wi::OVF_NONE)
	{
	  r.union_ (min_limit (type), ub);
	  r.union_ (lb, max_limit (type));
	}
      // Overflow on the upper bound.
      else if (ov_lb == wi::OVF_NONE && ov_ub == wi::OVF_OVERFLOW)
	{
	  r.union_ (lb, max_limit (type));
	  r.union_ (min_limit (type), ub);
	}
      else
	r.set_range_for_type (type);
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

      if (wi::gt_p (new_lb, new_ub, TYPE_SIGN (type)))
	{
	  // FIXME: We should not be wrapping here, especially on
	  // !TYPE_OVERFLOW_WRAPS.
	  gcc_unreachable ();
	  r.set_range_for_type (type);
	}
      else
	r.union_ (new_lb, new_ub);
    }
  if (ov_lb || ov_ub)
    r.set_overflow ();
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
accumulate_range_with_possible_swap (signop s,
				     irange &r,
				     const wide_int &new_lb,
				     const wide_int &new_ub)
{
  /* If the bounds are swapped, set the overflow bit to force
     add_to_range to create the range correctly.  This is essentially
     what set_and_canonicalize_value_range was doing.  */
  wi::overflow_type overflow;
  bool overflow_wraps = TYPE_OVERFLOW_WRAPS (r.get_type ());
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
irange_operator::fold_range (irange& r ATTRIBUTE_UNUSED,
			     const irange& op1 ATTRIBUTE_UNUSED,
			     const irange& op2 ATTRIBUTE_UNUSED) const
{
  return false;
}


bool
irange_operator::op1_irange (irange& r ATTRIBUTE_UNUSED,
			     const irange& lhs ATTRIBUTE_UNUSED,
			     const irange& op2 ATTRIBUTE_UNUSED) const
{
  return false;
}

bool
irange_operator::op2_irange (irange& r ATTRIBUTE_UNUSED,
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
  if (lhs.empty_p ())
    {
      r.clear (val_type);
      return BRS_EMPTY;
    }

  // if the bounds arent the same, then its not a constant.  */
  if (!wi::eq_p (lhs.upper_bound (), lhs.lower_bound ()))
    {
      r.set_range_for_type (val_type);
      return BRS_FULL;
    }

  if (lhs.zero_p ())
    return BRS_FALSE;

  return BRS_TRUE;
}


class operator_equal : public irange_operator
{
public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			       const irange& val) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
			       const irange& val) const;
} op_equal;

void 
operator_equal::dump (FILE *f) const
{
  fprintf (f, " == ");
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
	r.set_range (boolean_type_node, 1, 1);
      else
	r.set_range (boolean_type_node, 0, 0);
    }
  else
    {
      /* If ranges do not intersect, we know the range is not equal, otherwise
         we don;t really know anything for sure.  */
      r = irange_intersect (op1, op2);
      if (r.empty_p ())
	r.set_range (boolean_type_node, 0, 0);
      else
	r.set_range_for_type (boolean_type_node);
    }

  return true;
}

bool
operator_equal::op1_irange (irange& r, const irange& lhs,
			    const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.get_type ()))
    {
      case BRS_FALSE:
        /* If the result is false, the only time we know anything is if OP2 is
	   a constant.  */
	if (wi::eq_p (op2.lower_bound(), op2.upper_bound()))
	  r = irange_invert (op2);
	else
	  r.set_range_for_type (op2.get_type ());
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
operator_equal::op2_irange (irange& r, const irange& lhs,
			    const irange& op1) const
{
  return operator_equal::op1_irange (r, lhs, op1);
}


/*  -----------------------------------------------------------------------  */

/* Range operator for def = op1 != op2. */

class operator_not_equal : public irange_operator
{
public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
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
	r.set_range (boolean_type_node, 1, 1);
      else
	r.set_range (boolean_type_node, 0, 0);
    }
  else
    {
      /* If ranges do not intersect, we know the range is not equal, otherwise
         we don;t really know anything for sure.  */
      r = irange_intersect (op1, op2);
      if (r.empty_p ())
	r.set_range (boolean_type_node, 1, 1);
      else
	r.set_range_for_type (boolean_type_node);
    }

  return true;
}

/* Calculate the range of op1 being == to VAL based on LHS.  */
bool
operator_not_equal::op1_irange (irange& r, const irange& lhs,
				const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.get_type ()))
    {
      case BRS_TRUE:
        /* If the result is true, the only time we know anything is if OP2 is
	   a constant.  */
	if (wi::eq_p (op2.lower_bound(), op2.upper_bound()))
	  r = irange_invert (op2);
	else
	  r.set_range_for_type (op2.get_type ());
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
operator_not_equal::op2_irange (irange& r, const irange& lhs,
				const irange& op1) const
{
  return operator_not_equal::op1_irange (r, lhs, op1);
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
    r.clear (type);
  else
    r.set_range (type, min_limit (type), lim);
}

/* (X <= VAL) produces the a range of [MIN, VAL]  */
static void
build_le (irange& r, tree type, const wide_int& val)
{
  r.set_range (type, min_limit (type), val);
}

/* (X > VAL) produces the a range of [VAL + 1, MAX]  */
static void
build_gt (irange& r, tree type, const wide_int& val)
{
  wi::overflow_type ov;
  wide_int lim = wi::add (val, 1, TYPE_SIGN (type), &ov);
  /* If val + 1 overflows, check is for X > MAX , which is an empty range.  */
  if (ov)
    r.clear (type);
  else
    r.set_range (type, lim, max_limit (type));
}

/* (X >= val) produces the a range of [VAL, MAX]  */
static void
build_ge (irange& r, tree type, const wide_int& val)
{
  r.set_range (type, val, max_limit (type));
}



class operator_lt :  public irange_operator
{
public:
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
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

  signop sign = TYPE_SIGN (op1.get_type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.get_type ()));

  if (wi::lt_p (op1.upper_bound (), op2.lower_bound (), sign))
    r.set_range (boolean_type_node, 1, 1);
  else
    if (!wi::lt_p (op1.lower_bound (), op2.upper_bound (), sign))
      r.set_range (boolean_type_node, 0 ,0);
    else 
      r.set_range_for_type (boolean_type_node);
  return true;
}


bool
operator_lt::op1_irange (irange& r, const irange& lhs, const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.get_type ()))
    {
      case BRS_TRUE:
	build_lt (r, op2.get_type (), op2.upper_bound ());
	break;

      case BRS_FALSE:
	build_ge (r, op2.get_type (), op2.lower_bound ());
	break;

      default:
        break;
    }
  return true;
}


bool
operator_lt::op2_irange (irange& r, const irange& lhs, const irange& op1) const
{
  switch (get_bool_state (r, lhs, op1.get_type ()))
    {
      case BRS_FALSE:
	build_le (r, op1.get_type (), op1.upper_bound ());
	break;

      case BRS_TRUE:
	build_gt (r, op1.get_type (), op1.lower_bound ());
	break;

      default:
        break;
    }
  return true;

}

class operator_le :  public irange_operator
{
public:
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
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

  signop sign = TYPE_SIGN (op1.get_type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.get_type ()));

  if (wi::le_p (op1.upper_bound (), op2.lower_bound (), sign))
    r.set_range (boolean_type_node, 1, 1);
  else
    if (!wi::le_p (op1.lower_bound (), op2.upper_bound (), sign))
      r.set_range (boolean_type_node, 0 ,0);
    else 
      r.set_range_for_type (boolean_type_node);
  return true;
}

bool
operator_le::op1_irange (irange& r, const irange& lhs, const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.get_type ()))
    {
      case BRS_TRUE:
	build_le (r, op2.get_type (), op2.upper_bound ());
	break;

      case BRS_FALSE:
	build_gt (r, op2.get_type (), op2.lower_bound ());
	break;

      default:
        break;
    }
  return true;
}


bool
operator_le::op2_irange (irange& r, const irange& lhs, const irange& op1) const
{
  switch (get_bool_state (r, lhs, op1.get_type ()))
    {
      case BRS_FALSE:
	build_lt (r, op1.get_type (), op1.upper_bound ());
	break;

      case BRS_TRUE:
	build_ge (r, op1.get_type (), op1.lower_bound ());
	break;

      default:
        break;
    }
  return true;

}


class operator_gt :  public irange_operator
{
public:
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
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

  signop sign = TYPE_SIGN (op1.get_type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.get_type ()));

  if (wi::gt_p (op1.lower_bound (), op2.upper_bound (), sign))
    r.set_range (boolean_type_node, 1, 1);
  else
    if (!wi::gt_p (op1.upper_bound (), op2.lower_bound (), sign))
    r.set_range (boolean_type_node, 0, 0);
    else 
      r.set_range_for_type (boolean_type_node);

  return true;
}

bool
operator_gt::op1_irange (irange& r, const irange& lhs, const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.get_type ()))
    {
      case BRS_TRUE:
	build_gt (r, op2.get_type (), op2.lower_bound ());
	break;

      case BRS_FALSE:
	build_le (r, op2.get_type (), op2.upper_bound ());
	break;

      default:
        break;
    }
  return true;
}


bool
operator_gt::op2_irange (irange& r, const irange& lhs, const irange& op1) const
{
  switch (get_bool_state (r, lhs, op1.get_type ()))
    {
      case BRS_FALSE:
	build_ge (r, op1.get_type (), op1.lower_bound ());
	break;

      case BRS_TRUE:
	build_lt (r, op1.get_type (), op1.upper_bound ());
	break;

      default:
        break;
    }
  return true;

}


class operator_ge :  public irange_operator
{
public:
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
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

  signop sign = TYPE_SIGN (op1.get_type ());
  gcc_checking_assert (sign == TYPE_SIGN (op2.get_type ()));

  if (wi::ge_p (op1.lower_bound (), op2.upper_bound (), sign))
    r.set_range (boolean_type_node, 1 , 1);
  else
    if (!wi::ge_p (op1.upper_bound (), op2.lower_bound (), sign))
      r.set_range (boolean_type_node, 0 , 0);
    else 
      r.set_range_for_type (boolean_type_node);

  return true;
} 

bool
operator_ge::op1_irange (irange& r, const irange& lhs, const irange& op2) const
{
  switch (get_bool_state (r, lhs, op2.get_type ()))
    {
      case BRS_TRUE:
	build_ge (r, op2.get_type (), op2.lower_bound ());
	break;

      case BRS_FALSE:
	build_lt (r, op2.get_type (), op2.upper_bound ());
	break;

      default:
        break;
    }
  return true;
}


bool
operator_ge::op2_irange (irange& r, const irange& lhs, const irange& op1) const
{
  switch (get_bool_state (r, lhs, op1.get_type ()))
    {
      case BRS_FALSE:
	build_gt (r, op1.get_type (), op1.lower_bound ());
	break;

      case BRS_TRUE:
	build_le (r, op1.get_type (), op1.upper_bound ());
	break;

      default:
        break;
    }
  return true;

}



/*  -----------------------------------------------------------------------  */

/*  -----------------------------------------------------------------------  */

static bool
do_cross_product_irange (enum tree_code code, signop s, irange& r,
			 const wide_int& lh_lb, const wide_int& lh_ub,
			 const wide_int& rh_lb, const wide_int& rh_ub)
{
  wide_int new_lb, new_ub;
  bool overflow_undefined = TYPE_OVERFLOW_UNDEFINED (r.get_type ());
  bool overflow_wraps = TYPE_OVERFLOW_WRAPS (r.get_type ());
  unsigned prec = TYPE_PRECISION (r.get_type ());
  if (wide_int_range_multiplicative_op (new_lb, new_ub,
					code, s, prec,
					lh_lb, lh_ub, rh_lb, rh_ub,
					overflow_undefined, overflow_wraps))
    {
      /* ?? Should we only call this when overflow wraps and
	 MULT_EXPR, otherwise call r.union_()?  Because that's the
	 only case where the ranges can be swapped.  Is it worth
	 it?  */
      accumulate_range_with_possible_swap (s, r, new_lb, new_ub);
      return true;
    }
  return false;
}

static bool
op_wi (enum tree_code code, signop s, irange& r, const wide_int& lh_lb,
       const wide_int lh_ub, const wide_int& rh_lb, const wide_int &rh_ub) 
{
  wide_int new_lb, new_ub, tmp;
  wi::overflow_type ov_lb, ov_ub;

  switch (code)
    {
    case PLUS_EXPR:
      wide_int_binop (new_lb, code, lh_lb, rh_lb, s, &ov_lb);
      wide_int_binop (new_ub, code, lh_ub, rh_ub, s, &ov_ub);
      accumulate_range (r, new_lb, ov_lb, new_ub, ov_ub,
			TYPE_OVERFLOW_WRAPS (r.get_type ()));
      return true;

    case MINUS_EXPR:
      wide_int_binop (new_lb, code, lh_lb, rh_ub, s, &ov_lb);
      wide_int_binop (new_ub, code, lh_ub, rh_lb, s, &ov_ub);
      accumulate_range (r, new_lb, ov_lb, new_ub, ov_ub,
			TYPE_OVERFLOW_WRAPS (r.get_type ()));
      return true;

    case RSHIFT_EXPR:
    case MULT_EXPR:
      return do_cross_product_irange (code, s, r, lh_lb, lh_ub, rh_lb, rh_ub);

    case LSHIFT_EXPR:
      {
	tree type = r.get_type ();
        int prec = TYPE_PRECISION (type);
	if (!wide_int_range_shift_undefined_p (s, prec, rh_lb, rh_ub)
	    && wide_int_range_lshift (new_lb, new_ub,
				      s, prec, lh_lb, lh_ub, rh_lb, rh_ub,
				      TYPE_OVERFLOW_UNDEFINED (type),
				      TYPE_OVERFLOW_WRAPS (type)))
	  {
	    accumulate_range_with_possible_swap (s, r, new_lb, new_ub);
	    return true;
	  }
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
	if (wide_int_range_bit_xor (new_lb, new_ub, s,
				    TYPE_PRECISION (r.get_type ()),
				    must_be_nonzero_lh,
				    may_be_nonzero_lh,
				    must_be_nonzero_rh,
				    may_be_nonzero_rh))
	  {
	    accumulate_range (r, new_lb, wi::OVF_NONE, new_ub, wi::OVF_NONE,
			      false);
	    return true;
	  }
	return false;
      }

    case TRUNC_MOD_EXPR:
      // Make sure we're not dividing by zero.
      if (wi::lt_p (0, rh_lb, s) || wi::lt_p (rh_ub, 0, s))
	{
	  wide_int_range_trunc_mod (new_lb, new_ub, s,
				    TYPE_PRECISION (r.get_type ()),
				    lh_lb, lh_ub, rh_lb, rh_ub);
	  accumulate_range (r, new_lb, wi::OVF_NONE, new_ub, wi::OVF_NONE,
			    false);
	  return true;
	}
      // Division by zero is undefined.
      return false;

    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case CEIL_DIV_EXPR:
      // if zero is not a possible result, do the operation
      if (wi::lt_p (0, rh_lb, s) || wi::lt_p (rh_ub, 0, s))
	return do_cross_product_irange (code, s, r, lh_lb, lh_ub, rh_lb, rh_ub);
      
      // If divide by zero is allowed, do nothing.
      if (cfun->can_throw_non_call_exceptions)
        return false;
      
      // Perform the division in 2 parts,[LB, -1] and [1, UB]
      // skipping that section if that bound is actually 0.
      if (wi::ne_p (rh_lb, 0))
        {
	  tmp = wi::minus_one (rh_lb.get_precision ());
	  if (!do_cross_product_irange (code, s, r, lh_lb, lh_ub, rh_lb, tmp))
	    return false;
	}
      if (wi::ne_p (rh_ub, 0))
        {
	  tmp = wi::one (rh_ub.get_precision ());
	  if (!do_cross_product_irange (code, s, r, lh_lb, lh_ub, tmp, rh_ub))
	    return false;
	}
      // If nothing was done at all, the dividsor must be [0,0] and 
      // non-call exceptions must be true.  simply calculate no range
      // this allows division by ranges which have multiple sub-ranges to 
      // continue if one of the subranges is [0,0]
      return true;

    default:
      break;
    }

  return false;
}

/* Perform an operation between a constant and a range.  */
static bool
op_ir (enum tree_code code, irange& r, const wide_int& lh, const irange& rh)
{
  unsigned x;
  signop s = TYPE_SIGN (r.get_type ());
  r.clear (r.get_type ());
  for (x = 0; x < rh.num_pairs () ; x++)
    {
      if (!op_wi (code, s, r, lh, lh, rh.lower_bound (x), rh.upper_bound (x)))
        return false;
    }
  return true;
}

/* Perform an operation between a range and a constant.  */
static bool
op_ri (enum tree_code code, irange& r, const irange& lh, const wide_int& rh)
{
  unsigned x;
  signop s = TYPE_SIGN (r.get_type ());

  r.clear (r.get_type ());
  for (x = 0; x < lh.num_pairs () ; x++)
    {
      if (!op_wi (code, s, r, lh.lower_bound (x), lh.upper_bound (x), rh, rh))
        return false;
    }
  return true;
}

/* Perform an operation between 2 ranges.  */
static bool
op_rr (enum tree_code code, irange& r, const irange& lh, const irange& rh)
{
  bool res = false;
  tree type = lh.get_type ();
  // Clear and set result type.
  r.clear (type);

  if (lh.empty_p () || rh.empty_p ())
    return true;

  if (lh.overflow_p() || rh.overflow_p ())
    return false;

  /* Try constant cases first to see if we do anything special with them. */
  if (wi::eq_p (lh.upper_bound (), lh.lower_bound ()))
    res = op_ir (code, r, lh.upper_bound (), rh);

  if (!res && wi::eq_p (rh.upper_bound (), rh.lower_bound ()))
    res = op_ri (code, r, lh, rh.upper_bound ());

  if (!res)
    {
      signop s = TYPE_SIGN (type);
      for (unsigned x = 0; x < lh.num_pairs (); ++x)
	for (unsigned y = 0; y < rh.num_pairs (); ++y)
	  {
	    res = op_wi (code, s, r,
			 lh.lower_bound (x), lh.upper_bound (x),
			 rh.lower_bound (y), rh.upper_bound (y));
	    if (!res)
	      return false;
	  }
    }

  return res && !r.range_for_type_p ();
}


class basic_operator : public irange_operator
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
  if (empty_range_check (r, lh, rh, lh.get_type ()))
    return true;

  return op_rr (code, r, lh, rh);
}


class operator_plus : public basic_operator
{
public:
  operator_plus (): basic_operator (PLUS_EXPR) { }
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const;

} op_plus;


/* Adjust irange to be in terms of op1. 
   Given [range] = op1 + val,  op1 = [range] - val.  */
bool
operator_plus::op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const
{
  return op_rr (MINUS_EXPR, r, lhs, op2);
}

bool
operator_plus::op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const
{
  return op_rr (MINUS_EXPR, r, lhs, op1);
}


class operator_minus : public basic_operator
{
public:
  operator_minus () : basic_operator (MINUS_EXPR) { }
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_minus;

/* Adjust irange to be in terms of op1. 
   Given lhs = op1 - op2,  op1 = lhs + op2.  */
bool
operator_minus::op1_irange (irange& r, const irange& lhs,
			    const irange& op2) const
{
  return op_rr (PLUS_EXPR, r, lhs, op2);
}

/* Adjust irange to be in terms of op2. 
   Given lhs = op1 - op2,  -op2 = lhs - op1, therefore op2 = op1 - lhs.  */
bool
operator_minus::op2_irange (irange& r, const irange& lhs,
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
  virtual bool fold_range (irange& r, const irange& op1,
                           const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;

} op_exact_div;


bool operator_exact_divide::fold_range (irange& r, const irange& op1,
					const irange& op2) const
{
  bool res = basic_operator::fold_range (r , op1, op2);
  // Unless 0 is in the dividend, the result cannot contain 0.
  if (!op1.contains_p (0))
    {
      if (!res)
	{
	  // any (range without 0) EXACT_DIV (anything) results in ~[0,0]
	  r.set_range (op1.get_type (), 0, 0, irange::INVERSE);
	  return true;
	}
      else
	// if 0 is in the current result, remove it.
	if (r.contains_p (0))
	  r.intersect (irange (r.get_type (), 0, 0, irange::INVERSE));
    }
  return res;
}
// Adjust irange to be in terms of op1. 
bool
operator_exact_divide::op1_irange (irange& r,
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
  if (op2.singleton_p (offset) && op_rr (MULT_EXPR, r, lhs, op2) &&
      !r.overflow_p () && wi::ne_p (offset, 0))   
    return true;
  return false;
}


class operator_shift : public basic_operator
{
public:
  operator_shift (enum tree_code c) : basic_operator (c) { }
  virtual bool fold_range (irange& r, const irange& op1,
                           const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
};

operator_shift op_lshift (LSHIFT_EXPR);
operator_shift op_rshift (RSHIFT_EXPR);

bool
operator_shift::fold_range (irange& r, const irange& op1,
                           const irange& op2) const 
{
  tree t1 = op1.get_type ();
  if (empty_range_check (r, op1, op2, t1))
    return true;
  
  // Negative shifts are undefined, as well as shift >= precision
  if (wi::lt_p (op2.lower_bound (), 0, TYPE_SIGN (op2.get_type ())))
    return false;
  if (wi::ge_p (op2.upper_bound (), TYPE_PRECISION (t1), UNSIGNED))
    return false;

  return basic_operator::fold_range (r, op1, op2);
}

bool
operator_shift::op1_irange (irange& r, const irange& lhs,
			    const irange& op2) const
{
  tree type = lhs.get_type ();
  wide_int w2;
  if (empty_range_check (r, lhs, op2, type))
    return true;

  // Negative shifts are undefined, as well as shift >= precision
  if (wi::lt_p (op2.lower_bound (), 0, TYPE_SIGN (op2.get_type ())))
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

class operator_cast: public irange_operator
{
public:
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
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
  if (empty_range_check (r, lh, rh, rh.get_type ()))
    return true;

  if (lh.get_type () != rh.get_type ())
    {
      /* Handle conversion so they become the same type.  */
      r = lh;
      r.cast (rh.get_type ());
      r.intersect (rh);
    }
  else
    /* If they are the same type, the result should be the intersection of
       the two ranges.  */
    r = irange_intersect (lh, rh);
  return true;
}

bool
operator_cast::op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const
{
  tree lhs_type = lhs.get_type ();
  tree op2_type = op2.get_type ();
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
	  r = lhs;
	  r.cast (op2_type);
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
      op_type.set_range_for_type (op2_type);
      op_type.cast (lhs_type);

      /* Intersect this with the LHS range will produce the RHS range.  */
      r = irange_intersect (lhs, op_type);
    }
  else
    r = lhs;

  /* Cast the calculated range to the type of the RHS.  */
  r.cast (op2.get_type ());

  return true;
}

/*  ----------------------------------------------------------------------  */

// Bitwise and logical ops. 

class operator_logical_and : public irange_operator
{
public:
  void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
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
      r.set_range (boolean_type_node, boolean_false_node, boolean_false_node);
      return true;
    }

  // To reach this point, there must be a logical 1 on each side, and the only
  // remaining question is whether there is a zero or not.

  if (lh.contains_p (0) || rh.contains_p (0))
    r.set_range (boolean_type_node, boolean_false_node, boolean_true_node);
  else
    r.set_range (boolean_type_node, boolean_true_node, boolean_true_node);
  
  return true;
}



bool
operator_logical_and::op1_irange (irange& r, const irange& lhs,
				  const irange& op2) const
{
   switch (get_bool_state (r, lhs, op2.get_type ()))
     {
       /* A true result means both sides of the AND must be true.  */
       case BRS_TRUE:
         r.set_range (boolean_type_node, 1, 1);
	 break;
     
       /* Any other result means only one side has to be false, the other
	  side can be anything. SO we cant be sure of any result here.  */
      default:
        r.set_range_for_type (boolean_type_node);
	break;
    }
  return true;
}

bool
operator_logical_and::op2_irange (irange& r, const irange& lhs,
				  const irange& op1) const
{
  return operator_logical_and::op1_irange (r, lhs, op1);
}

class operator_bitwise_and : public irange_operator
{
  bool apply_mask_to_pair (irange& r, const wide_int& lb, const wide_int& ub,
			   const wide_int& mask) const;
  bool apply_mask_to_range (irange &r, const irange& val,
			    const wide_int& mask) const;
public:
  void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_bitwise_and;

void 
operator_bitwise_and::dump (FILE *f) const
{
  fprintf (f, " & ");
}

bool 
operator_bitwise_and::apply_mask_to_pair (irange& r, const wide_int& lb, 
					  const wide_int& ub,
					  const wide_int& mask) const
{
  wide_int new_lb, new_ub;
  // FIXME: remove me and remove add back the cast to wide-int-range.cc
  extern bool wide_int_range_can_optimize_bit_op
    (tree_code code,
     const wide_int &lb, const wide_int &ub,
     const wide_int &mask);
  if (wide_int_range_can_optimize_bit_op (BIT_AND_EXPR, lb, ub, mask))
    {
      new_lb = lb & mask;
      new_ub = ub & mask;
    }
  else
    {
      wide_int may_be_nonzero;
      wide_int must_be_nonzero;

      wide_int_range_set_zero_nonzero_bits (UNSIGNED, lb, ub, may_be_nonzero,
					    must_be_nonzero);

      new_lb = must_be_nonzero & mask;
      new_ub = may_be_nonzero & mask;
      // Truncate the result range maximum to maximum of the input range.
      new_ub = wi::min (new_ub, ub, UNSIGNED);
      new_ub = wi::min (new_ub, mask, UNSIGNED);
    }

  r.union_ (new_lb, new_ub);
  return true;
}

bool
operator_bitwise_and::apply_mask_to_range (irange &r, const irange& val,
					   const wide_int& mask) const
{
  tree type = const_cast <tree> (val.get_type ());

  if (val.empty_p ())
   {
     r.clear (type);
     return true;
   }

  // If the mask is zero, zero is the only possible result. 
  if (wi::eq_p (mask, 0))
    {
      r.set_range (type, mask, mask);
      return true;
    }
  // FIXME   This following approach is flawed when the sign bit is set
  // void boo (char t);
  // int foo(char c) {
  //   char t = c & 0xf0;
  //   boo (t);
  // }
  // This is because the unsigned range we find when applying the mask is
  // [16, 240], and when we cast back to a signed value we get a ange of
  // [-128, -16][0, 0][16, 127] char
  // We lose the information that the signed value SHOULD be [16, 112]
  // as the unsigned range alone goes from 16-240.  we need to treat 
  // each subrange seperately, doing all the positives and then all the
  // negatives, and maiking sure 0 isnt in any of the ranges. we'll
  // add that in later.
  //  Also note this is currently only working on the range as a whole, not
  //  for each subrange.. which is also flawed :-P
  //
  //  Perhaps revert to the striaght VRP version until we think this through
  //  better and do a more complete job.

  // Process the entire range as if it were unsigned, then convert back at 
  // the end if need be.
  signop sign = TYPE_SIGN (type);
  irange tmp = val;
  if (sign == SIGNED)
    {
      type = unsigned_type_for (type);
      tmp.cast (type);
    }
    
  r.clear (type);
  for (unsigned i = 0; i < tmp.num_pairs (); i++)
    apply_mask_to_pair (r, tmp.lower_bound (i), tmp.upper_bound (i), mask);

  // If nothing intersects with mask, then jhe result has to be 0. 
  if (r.empty_p ())
    {
      r.set_range (val.get_type (), 0, 0);
      return true;
    }

  // If the resulting range contains 0, AND the least significant bit of
  // mask is not set, we can improve the range by making the least significant
  // bit set the minimum, and adding in 0.
  // ie,   a & 0x1C  returns a range of [0,28], we can improve this to
  // [0,0][12, 28] since we have subranges now.
  int tz;
  if (r.contains_p (0) && (tz = wi::ctz (mask)) != 0)
    {
      wide_int lb = wi::set_bit_in_zero (tz, mask.get_precision ());
      wide_int ub = r.upper_bound();
      r.intersect (lb, ub);
      r.union_ (irange (r.get_type (), 0, 0));
    }

  // If we converted from signed, convert back. 
  if (sign == SIGNED)
    r.cast (val.get_type ());

  return true;
}

/* A bitwise AND of two ranges is executed when we are walking forward with
   ranges that have been determined.   x_8 is an unsigned char.
	 b_1 = x_8 < 20
	 b_2 = x_8 > 5
	 c_2 = b_1 && b_2
   if we are looking for the range of x_8, the ranges on each side of the AND
   will be:   b_1 carries x_8 = [0, 19],   b_2 carries [6, 255]
   the result of the AND is the intersection of the 2 ranges, [6, 255]. */
bool
operator_bitwise_and::fold_range (irange& r, const irange& lh,
				  const irange& rh) const
{
  if (empty_range_check (r, lh, rh, lh.get_type ()))
    return true;

  wide_int w;
  /* If this is really a logical operation, call that.  */
  if (types_compatible_p (lh.get_type (), boolean_type_node))
    return op_logical_and.fold_range (r, lh, rh);

  if (lh.singleton_p (w))
    return apply_mask_to_range (r, rh, w);
  else
    if (rh.singleton_p (w))
      return apply_mask_to_range (r, lh, w);

  tree type = lh.get_type ();
  // For pointers we only care about NULL and NONNULL
  if (POINTER_TYPE_P (type))
    {
      // If either side is a 0, the result will be 0.
      if (lh.zero_p () || rh.zero_p ())
        r = lh;
      else
	r = irange_union (lh, rh);
    }
  else
    {
      // To be safe, make the range largest to smallest, and include 0.
      wide_int ub = wi::max (lh.upper_bound (), rh.upper_bound(),
			     TYPE_SIGN (lh.get_type ()));
      wide_int lb = wi::min (lh.lower_bound (), rh.lower_bound(),
			     TYPE_SIGN (lh.get_type ()));
      lb = wi::min (lb, 0, TYPE_SIGN (lh.get_type ()));
      ub = wi::max (ub, 0, TYPE_SIGN (lh.get_type ()));
      r.set_range (lh.get_type (), lb, ub);
    }
  return true;
}

bool
operator_bitwise_and::op1_irange (irange& r, const irange& lhs,
				  const irange& op2) const
{
  /* If this is really a logical operation, call that.  */
  if (types_compatible_p (lhs.get_type (), boolean_type_node))
    return op_logical_and.op1_irange (r, lhs, op2);

  /* For now do nothing with bitwise AND of iranges, just return the type. */
  r.set_range_for_type (lhs.get_type ());
  return true;
}

bool
operator_bitwise_and::op2_irange (irange& r, const irange& lhs,
				  const irange& op1) const
{
  return operator_bitwise_and::op1_irange (r, lhs, op1);
}


class operator_logical_or : public irange_operator
{
public:
  void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
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

  r = irange_union (lh, rh);
  return true;
}

bool
operator_logical_or::op1_irange (irange& r, const irange& lhs,
				  const irange& op2) const
{
   switch (get_bool_state (r, lhs, op2.get_type ()))
     {
       /* A false result means both sides of the OR must be false.  */
       case BRS_FALSE:
         r.set_range (boolean_type_node, 0 , 0);
	 break;
     
       /* Any other result means only one side has to be true, the other
	  side can be anything. SO we cant be sure of any result here.  */
      default:
        r.set_range_for_type (boolean_type_node);
	break;
    }
  return true;
}

bool
operator_logical_or::op2_irange (irange& r, const irange& lhs,
				  const irange& op1) const
{
  return operator_logical_or::op1_irange (r, lhs, op1);
}

class operator_bitwise_or : public irange_operator
{
public:
  void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const;
} op_bitwise_or;

void 
operator_bitwise_or::dump (FILE *f) const
{
  fprintf (f, " | ");
}


/* A bitwise AND of two ranges is executed when we are walking forward with
   ranges that have been determined.   x_8 is an unsigned char.
	 b_1 = x_8 < 20
	 b_2 = x_8 > 5
	 c_2 = b_1 && b_2
   if we are looking for the range of x_8, the ranges on each side of the AND
   will be:   b_1 carries x_8 = [0, 19],   b_2 carries [6, 255]
   the result of the AND is the intersection of the 2 ranges, [6, 255]. */
bool
operator_bitwise_or::fold_range (irange& r, const irange& lh,
				  const irange& rh) const
{
  if (empty_range_check (r, lh, rh, lh.get_type ()))
    return true;

  /* If this is really a logical operation, call that.  */
  if (types_compatible_p (lh.get_type (), boolean_type_node))
    return op_logical_or.fold_range (r, lh, rh);

  /* For now do nothing with bitwise OR of iranges, just return the type. */
  r.set_range_for_type (lh.get_type ());
  return true;
}

bool
operator_bitwise_or::op1_irange (irange& r, const irange& lhs,
				  const irange& op2) const
{
  /* If this is really a logical operation, call that.  */
  if (types_compatible_p (lhs.get_type (), boolean_type_node))
    return op_logical_or.op1_irange (r, lhs, op2);

  /* For now do nothing with bitwise OR of iranges, just return the type. */
  r.set_range_for_type (lhs.get_type ());
  return true;
}

bool
operator_bitwise_or::op2_irange (irange& r, const irange& lhs,
				  const irange& op1) const
{
  return operator_bitwise_or::op1_irange (r, lhs, op1);
}

class operator_bitwise_xor : public basic_operator
{
public:
  operator_bitwise_xor (): basic_operator (BIT_XOR_EXPR) { }
  /* FIXME: Andrew can implement the op1_irange and op2_irange variants
     when he returns from leave :-P.  */
} op_bitwise_xor;

class operator_trunc_mod : public basic_operator
{
public:
  operator_trunc_mod (): basic_operator (TRUNC_MOD_EXPR) { }
  /* FIXME: Andrew can implement the op1_irange and op2_irange variants
     when he returns from leave :-P.  */
} op_trunc_mod;

class operator_logical_not : public irange_operator
{
public:
  void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
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

  if (lh.range_for_type_p () || lh.empty_p ())
    r = lh;
  else
    r = irange_invert (lh);
  return true;
}

bool
operator_logical_not::op1_irange (irange& r, const irange& lhs,
				  const irange& op2 ATTRIBUTE_UNUSED) const
{
  if (lhs.range_for_type_p () || lhs.empty_p ())
    r = lhs;
  else
    r = irange_invert (lhs);
  return true;
}


class operator_bitwise_not : public irange_operator
{
public:
  void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& lh, const irange& rh) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
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
  tree type = lh.get_type ();
  if (empty_range_check (r, lh, rh, type))
    return true;

  /* If this is a boolean not, call the logical version.  */
  if (types_compatible_p (type, boolean_type_node))
    return op_logical_not.fold_range (r, lh, rh);

  // ~X is simply -1 - X.
  irange minusone (type, -1, -1);
  return op_rr (MINUS_EXPR, r, minusone, lh);
}

bool
operator_bitwise_not::op1_irange (irange& r, const irange& lhs,
				  const irange& op2 ATTRIBUTE_UNUSED) const
{
  tree type = lhs.get_type ();
  /* If this is a boolean not, call the logical version.  */
  if (types_compatible_p (type, boolean_type_node))
    return op_logical_not.op1_irange (r, lhs, op2);

  // ~X is -1 - X and since bitwise NOT is involutary...do it again.
  irange minusone (type, -1, -1);
  return op_rr (MINUS_EXPR, r, minusone, lhs);
}


/*  ----------------------------------------------------------------------  */


class operator_cst : public irange_operator
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



class operator_ssa_name : public irange_operator
{
public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
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
operator_ssa_name::op1_irange (irange& r, const irange& lhs,
			       const irange& op2 ATTRIBUTE_UNUSED) const
{
  r = lhs;
  return true;
}


class operator_pointer_plus : public irange_operator
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
  if (empty_range_check (r, lh, rh, lh.get_type ()))
    return true;

  // For pointer types we are mostly concerned with NULL and NON-NULL.
  // If either side is non-null, the result will be non-null.
  if (!lh.contains_p (0) || !rh.contains_p (0))
    r.set_range (lh.get_type (), 0, 0, irange::INVERSE);
  else
    {
      // Can't perform a union since the RH is a different type from the LH.
      if (lh.zero_p () && rh.zero_p ())
        r = lh;
      else
	r.set_range_for_type (lh.get_type ());
    }
  return true;
}

// Unary identity function.
class operator_identity : public irange_operator
{
  tree_code code;
 public:
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange &op2 ATTRIBUTE_UNUSED) const
  { r = op1; return false; }
  virtual bool op1_irange (irange& r, const irange& lhs,
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

class operator_abs : public irange_operator
{
  tree_code code;
 public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;

 private:
  bool protected_negate (wide_int &, tree, const wide_int &) const;
} op_abs;

void
operator_abs::dump (FILE *f) const
{
  fprintf (f, " ABS ");
}

/* Negate that can handle flipping MIN/MAX extremes.

   X is the value to negate with a type of TYPE.
   RES is where to put the result.

   Returns FALSE if the value being negated is MIN and signed overflow
   is allowed, in which case negation is undefined.  */

bool
operator_abs::protected_negate (wide_int &res, tree type,
				const wide_int &x) const
{
  if (x == wi::to_wide (TYPE_MIN_VALUE (type)))
    {
      // -TYPE_MIN_VALUE = TYPE_MIN_VALUE with flag_wrapv so we can't
      // get a useful range.  */
      if (!TYPE_OVERFLOW_UNDEFINED (type))
	return false;
      res = wi::to_wide (TYPE_MAX_VALUE (type));
    }
  else
    res = -x;
  return true;
}

bool
operator_abs::fold_range (irange& r, const irange& lh, const irange& rh) const
{
  tree type = rh.get_type ();
  if (empty_range_check (r, lh, rh, type))
    return true;
  if (TYPE_UNSIGNED (type))
    {
      r = lh;
      return true;
    }

  // To calculate the ABS of a range, flip the sign bit on all the
  // negative parts of the range, and then swap the upper/lower
  // bounds.  The positive parts remain as is.  For example:
  // ABS([-50, -20][6, 10]) => [20,50][6,10] => [6,10][20,50]
  // ABS([-50,5][80,90]) => [1,50][0,5][80,90] => [0,50][80,90].
  irange negatives, positives;
  range_positives (&positives, type);
  range_negatives (&negatives, type);
  positives.intersect (lh);
  negatives.intersect (lh);
  for (unsigned i = 0; i < negatives.num_pairs (); ++i)
    {
      wide_int lb, ub;
      if (!protected_negate (lb, type, negatives.lower_bound (i))
	  || !protected_negate (ub, type, negatives.upper_bound (i)))
	return false;
      positives.union_ (ub, lb);
    }
  r = positives;
  return true;
}

bool
operator_abs::op1_irange (irange& r, const irange& lhs, const irange& op2) const
{
  tree type = lhs.get_type ();
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
    r.union_ (-positives.upper_bound (i), -positives.lower_bound (i));
  return true;
}

class operator_negate : public irange_operator
{
  tree_code code;
 public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
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
  tree type = rh.get_type ();
  if (empty_range_check (r, lh, rh, type))
    return true;
  // -X is simply 0 - X.
  irange zero (type, 0, 0);
  return op_rr (MINUS_EXPR, r, zero, lh);
}

class operator_min_max : public irange_operator
{
  tree_code code;
public:
  operator_min_max (tree_code c);
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
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
  tree type = lh.get_type ();

  if (empty_range_check (r, lh, rh, type))
    return true;

  // Start with the union of both ranges  
  r = irange_union (lh, rh);

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
operator_min_max::op1_irange (irange& r, const irange& lhs,
			      const irange& op2) const
{
  if (empty_range_check (r, lhs, op2, lhs.get_type ()))
    return true;

  if (POINTER_TYPE_P (lhs.get_type ()))
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
	ub = max_limit (lhs.get_type ());
    }
  else
    {
      // this operand.   Otherwise, it could be any value to MAX_TYPE as the
      // upper bound comes from the other operand.
      if (wi::eq_p (lhs.lower_bound (), op2.lower_bound ()))
	lb = min_limit (lhs.get_type ());
    }

  r. set_range (lhs.get_type (), lb, ub);
  return true;
}

bool
operator_min_max::op2_irange (irange& r, const irange& lhs,
			      const irange& op1) const
{
  return operator_min_max::op1_irange (r, lhs, op1);
}




class operator_addr_expr : public irange_operator
{
public:
  virtual void dump (FILE *f) const;

  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
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
  if (empty_range_check (r, lh, rh, rh.get_type ()))
    return true;

  // Return a non-null pointer of the LHS type (passed in op2)
  if (lh.zero_p ())
    r.set_range (rh.get_type (), 0, 0);
  else
    if (!lh.contains_p (0))
      r.set_range (rh.get_type (), 0, 0, irange::INVERSE);
    else
      return false;
  return true;
}

// The same functionality for fold() applies to op1_range...
// effectively copying the non-nullness.
bool
operator_addr_expr::op1_irange (irange& r, const irange& lhs,
			        const irange& op2) const
{
  return operator_addr_expr::fold_range (r, lhs, op2);
}
/*  ----------------------------------------------------------------------  */

/* Create the irange operator table as a local object in this file, and the
   constructor should be automatically called before it is used.  */

class irange_op_table
{
  irange_operator *irange_tree[MAX_TREE_CODES];
public:
  irange_op_table ();
  inline irange_operator *operator[] (enum tree_code code);
} irange_tree;

irange_operator *irange_op_table::operator[] (enum tree_code code)
{
  gcc_assert (code > 0 && code < MAX_TREE_CODES);
  return irange_tree[code];
}

irange_op_table::irange_op_table ()
{
  irange_tree[LT_EXPR] = &op_lt;
  irange_tree[LE_EXPR] = &op_le;
  irange_tree[GT_EXPR] = &op_gt;
  irange_tree[GE_EXPR] = &op_ge;
  irange_tree[NE_EXPR] = &op_not_equal;
  irange_tree[EQ_EXPR] = &op_equal;

  irange_tree[PLUS_EXPR] = &op_plus;
  irange_tree[MINUS_EXPR] = &op_minus;
  irange_tree[MULT_EXPR] = &op_mult;
  irange_tree[TRUNC_DIV_EXPR] = &op_trunc_div;
  irange_tree[FLOOR_DIV_EXPR] = &op_floor_div;
  irange_tree[ROUND_DIV_EXPR] = &op_round_div;
  irange_tree[CEIL_DIV_EXPR] = &op_ceil_div;
  irange_tree[EXACT_DIV_EXPR] = &op_exact_div;
  
  irange_tree[NOP_EXPR] = &op_cast;
  irange_tree[CONVERT_EXPR] = &op_cast;

  irange_tree[TRUTH_AND_EXPR] = &op_logical_and;
  irange_tree[TRUTH_OR_EXPR] = &op_logical_or;
  irange_tree[TRUTH_NOT_EXPR] = &op_logical_not;

  irange_tree[BIT_AND_EXPR] = &op_bitwise_and;
  irange_tree[BIT_IOR_EXPR] = &op_bitwise_or;
  irange_tree[BIT_XOR_EXPR] = &op_bitwise_xor;
  irange_tree[BIT_NOT_EXPR] = &op_bitwise_not;

  irange_tree[INTEGER_CST] = &op_integer_cst;
  irange_tree[SSA_NAME] = &op_ssa_name;

  irange_tree[ABS_EXPR] = &op_abs;
  irange_tree[MIN_EXPR] = &op_min;
  irange_tree[MAX_EXPR] = &op_max;
  irange_tree[NEGATE_EXPR] = &op_negate;
  irange_tree[PAREN_EXPR] = &op_identity;
  irange_tree[OBJ_TYPE_REF] = &op_identity;
  irange_tree[POINTER_PLUS_EXPR] = &op_pointer_plus;
  irange_tree[TRUNC_MOD_EXPR] = &op_trunc_mod;

  irange_tree[LSHIFT_EXPR] = &op_lshift;
  irange_tree[RSHIFT_EXPR] = &op_rshift;

  irange_tree[ADDR_EXPR] = &op_addr;
}

/* The table is hidden and accessed via a simple extern function.  */

irange_operator *
irange_op_handler (enum tree_code code)
{
  return irange_tree[code];
}

static bool
irange_from_value_range (irange &r, const value_range& vr)
{
  wide_int w1, w2;
  tree type = TREE_TYPE (vr.min);
  wi::overflow_type ov;

  if (TREE_CODE (vr.min) != INTEGER_CST || TREE_CODE (vr.max) != INTEGER_CST)
    return false;

  if (vr.type != VR_RANGE || vr.type != VR_ANTI_RANGE)
    return false;

  w1 = wi::to_wide (vr.min);
  w2 = wi::to_wide (vr.max);
  if (vr.type == VR_RANGE)
    r.set_range (type, w1, w2);
  else
    {
      w1 = wi::sub (w1, 1, TYPE_SIGN (type), &ov);
      w2 = wi::add (w2, 1, TYPE_SIGN (type), &ov);
      r.set_range (type, min_limit (type), w1);
      r.union_ (irange (type, w2, max_limit (type)));
    }

  return true;
}

static bool
value_range_from_irange (value_range& vr, const irange& r)
{
  wi::overflow_type ov;
  if (r.overflow_p ())
    return false;

  tree type = r.get_type ();
  wide_int w1 = r.lower_bound();
  wide_int w2 = r.upper_bound();
  wide_int min = min_limit (type);
  wide_int max = max_limit (type);

  // check for anti range
  if (w1 == min && w2 == max)
    {
      if (r.num_pairs () != 2)
	return false;
      vr.type = VR_ANTI_RANGE;
      w1 = wi::add (r.upper_bound (0), 1, TYPE_SIGN (type), &ov);
      w2 = wi::sub (r.lower_bound (1), 1, TYPE_SIGN (type), &ov);
    }
  else
    vr.type = VR_RANGE;

  vr.min = wide_int_to_tree (type, w1);
  vr.max = wide_int_to_tree (type, w2);
  return true;
}

// Fold constant value ranges by using the range_ops code.
bool fold_value_range (value_range& vr, enum tree_code code,
		       value_range& vr0, value_range& vr1)
{
  irange res, v0, v1;
  if (!irange_op_handler (code))
    return false;

  if (!irange_from_value_range (v0, vr0) || !irange_from_value_range (v1, vr1))
    return false;

  if (irange_op_handler (code)->fold_range (res, v0, v1))
    if (value_range_from_irange (vr, res))
      return true;
  
  vr.type = VR_VARYING;
  return true; 
}


