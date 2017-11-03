/* Code for range operators.
   Copyright (C) 2017 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

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


/* Given newly calcuclated lbound and ubound, examine the overflow bits to 
   determine where the various ranges belong.  */
static void
add_to_range (irange& r, wide_int& lb, bool ov_lb, wide_int& ub, bool ov_ub)
{
  if (ov_lb)
    {
      if (ov_ub)
        {
	  /* Both overflow, so result is all overflow.  */
	  if (TYPE_OVERFLOW_WRAPS (r.get_type ()))
	    r.union_ (lb, ub);
	}
      else
	{
	  /* lbound overflow, ubound doesn't, must be underflow.  */
	  r.union_ (min_limit (r.get_type ()), ub);
	  if (TYPE_OVERFLOW_WRAPS (r.get_type ()))
	    r.union_ (lb, max_limit (r.get_type ()));
	}
      if (!TYPE_OVERFLOW_WRAPS (r.get_type ()))
	r.set_overflow ();
    }
  else
    if (ov_ub)
      {
	/* Ubound overflow, lboudn doesnt, must be an overflow.  */
	r.union_ (lb, max_limit (r.get_type ()));
	if (TYPE_OVERFLOW_WRAPS (r.get_type ()))
	  r.union_ (min_limit (r.get_type ()), ub);
	if (!TYPE_OVERFLOW_WRAPS (r.get_type ()))
	  r.set_overflow ();
      }
    else
      r.union_ (lb, ub);
}

/* Handle the first addition to a range while handling the overflow bits.  */
static void
set_range (const_tree type, irange& r, wide_int& lb, bool ov_lb, wide_int& ub,
	   bool ov_ub)
{
  r.clear (type);
  add_to_range (r, lb, ov_lb, ub, ov_ub);
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
get_bool_state (irange& r, const irange& lhs, const_tree val_type)
{
  /* If there is no result, then this is unexectuable, so no range. */
  if (lhs.empty_p ())
    {
      r.clear (val_type);
      return BRS_EMPTY;
    }

  // there are multiple types of boolean nodes.  for instance
  // gcc_assert (lhs.valid_p () && lhs.get_type () == boolean_type_node);

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
build_lt (irange& r, const_tree type, const wide_int& val)
{
  bool ov;
  wide_int lim = wi::sub (val, 1, TYPE_SIGN (type), &ov);

  /* If val - 1 underflows, check is X < MIN, which is an empty range.  */
  if (ov)
    r.clear (type);
  else
    r.set_range (type, min_limit (type), lim);
}

/* (X <= VAL) produces the a range of [MIN, VAL]  */
static void
build_le (irange& r, const_tree type, const wide_int& val)
{
  r.set_range (type, min_limit (type), val);
}

/* (X > VAL) produces the a range of [VAL + 1, MAX]  */
static void
build_gt (irange& r, const_tree type, const wide_int& val)
{
  bool ov;
  wide_int lim = wi::add (val, 1, TYPE_SIGN (type), &ov);
  /* If val + 1 overflows, check is for X > MAX , which is an empty range.  */
  if (ov)
    r.clear (type);
  else
    r.set_range (type, lim, max_limit (type));
}

/* (X >= val) produces the a range of [VAL, MAX]  */
static void
build_ge (irange& r, const_tree type, const wide_int& val)
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
/* Range operator for     def = op1 + op2. */

enum opm_mode { OPM_ADD, OPM_SUB, OPM_MUL, OPM_DIV, OPM_EXACTDIV };

/* Perform an operation between a constant and a range.  */
static bool
op_ir (opm_mode mode, irange& r, const wide_int& lh, const irange& rh)
{
  bool ov_lb, ov_ub;
  unsigned x;
  wide_int lb, ub, new_lb, new_ub;
  const_tree type = rh.get_type ();
  signop s = TYPE_SIGN (type);

  r.clear (type);
  for (x = 0; x < rh.num_pairs () ; x++)
    {
      lb = rh.lower_bound (x);
      ub = rh.upper_bound (x);
      switch (mode)
        {
        case OPM_ADD:
	  new_lb = wi::add (lh, lb, s, &ov_lb);
	  new_ub = wi::add (lh, ub, s, &ov_ub);
	  break;

        case OPM_SUB:
	  new_lb = wi::sub (lh, ub, s, &ov_lb);
	  new_ub = wi::sub (lh, lb, s, &ov_ub);
	  break;

        case OPM_MUL:
	  new_lb = wi::mul (lh, lb, s, &ov_lb);
	  new_ub = wi::mul (lh, ub, s, &ov_ub);
	  // Multiply by large numbers can cause multiple overflows.
	  if (ov_lb || ov_ub)
	    return false;
	  // Multiply by a negative number can cause bound reversals.
	  if (wi::gt_p (new_lb, new_ub, s))
	    {
	      wide_int tmp = new_lb;
	      new_lb = new_ub;
	      new_ub = tmp;
	    }
	  break;

        case OPM_DIV:
	  // 0 / anything is [0,0]
	  if (wi::eq_p (lh, 0))
	    {
	      new_lb = lh;
	      new_ub = lh;
	      ov_lb = ov_ub = false;
	    }
	  else
	    return false;
	  break;

	default:
	  gcc_unreachable ();
	}
      add_to_range (r, new_lb, ov_lb, new_ub, ov_ub);
    }
  return true;
}

/* Perform an operation between a range and a constant.  */
static bool
op_ri (opm_mode mode, irange& r, const irange& lh, const wide_int& rh)
{
  bool ov_lb, ov_ub;
  unsigned x;
  wide_int lb, ub, new_lb, new_ub;
  const_tree type = lh.get_type ();
  signop s = TYPE_SIGN (type);

  r.clear (type);
  for (x = 0; x < lh.num_pairs () ; x++)
    {
      lb = lh.lower_bound (x);
      ub = lh.upper_bound (x);
      switch (mode)
        {
	case OPM_ADD:
	  new_lb = wi::add (lb, rh, s, &ov_lb);
	  new_ub = wi::add (ub, rh, s, &ov_ub);
	  break;

	case OPM_SUB:
	  new_lb = wi::sub (lb, rh, s, &ov_lb);
	  new_ub = wi::sub (ub, rh, s, &ov_ub);
	  break;

        case OPM_MUL:
	  // Multiply is symetrical
	  return op_ir (mode, r, rh, lh);

        case OPM_DIV:
	  // anything / [0,0] traps, so abort.
	  if (wi::eq_p (rh, 0))
	    return false;
	  new_lb = wi::div_trunc (lb, rh, s, &ov_lb);
	  new_ub = wi::div_trunc (ub, rh, s, &ov_ub);
	  // The sign of the divisor can reverse the bounds.
	  if (wi::gt_p (new_lb, new_ub, s))
	    {
	      wide_int tmp = new_lb;
	      new_lb = new_ub;
	      new_ub = tmp;
	    }
	  break;
	default:
	  gcc_unreachable ();
	}
      add_to_range (r, new_lb, ov_lb, new_ub, ov_ub);
    }
  return true;
}

/* Perform an operation between 2 ranges.  */
static bool
op_rr (opm_mode mode, irange& r, const irange& lh, const irange& rh)
{
  bool res = false;
  if (lh.empty_p () || rh.empty_p ())
    {
      r.clear (lh.get_type ());
      return true;
    }

  // There is a bug in 128 bit operations which dont set the overflow
  // bits properly. PR 82547.  This can cause an ICE because we dont
  // properly handle the overflow, resulting in ranges with LB and UB reversed.
  if (TYPE_PRECISION (lh.get_type ()) > HOST_BITS_PER_WIDE_INT)
    return false;

  /* Try constant cases first to see if we do anything special with them. */
  if (wi::eq_p (lh.upper_bound (), lh.lower_bound ()))
    res = op_ir (mode, r, lh.upper_bound (), rh);
  else
    if (wi::eq_p (rh.upper_bound (), rh.lower_bound ()))
      res = op_ri (mode, r, lh, rh.upper_bound ());\

  if (!res)
    {
      wide_int lb, ub, new_lb, new_ub;
      bool ov_lb, ov_ub;
      const_tree type = lh.get_type ();
      signop s = TYPE_SIGN (type);

      switch (mode)
	{
	case OPM_ADD:
	  /* Add the 2 lower and upper bounds togther and see what we get.  */
	  new_lb = wi::add (lh.lower_bound (), rh.lower_bound (), s, &ov_lb);
	  new_ub = wi::add (lh.upper_bound (), rh.upper_bound (), s, &ov_ub);
	  break;

	case OPM_SUB:
	  /* New possible range is [lb1-ub2, ub1-lb2].  */
	  new_lb = wi::sub (lh.lower_bound (), rh.upper_bound (), s, &ov_lb);
	  new_ub = wi::sub (lh.upper_bound (), rh.lower_bound (), s, &ov_ub);
	  break;

	case OPM_MUL:
	  // Changing signs makes it complicated. we have to break any ranges
	  // which cross 0 into sub ranges with same signs.  punt for now.
	  ov_lb = ov_ub = true;
	  break;

	case OPM_DIV:
	  ov_lb = ov_ub = true;
	  break;

	case OPM_EXACTDIV:
	  // any range without 0 EXACT_DIV anything results in ~[0,0]
	  if (!lh.contains_p (0))
	    {
	      r.set_range (type, 0, 0, irange::INVERSE);
	      return true;
	    }
	  return false;
	default:
	  gcc_unreachable ();
	}

      /* If both overflow, we can't be sure of the final range. */
      if (ov_lb && ov_ub)
	r.set_range_for_type (type);
      else
	set_range (type, r, new_lb, ov_lb, new_ub, ov_ub);
      res = true;
    }
  return res;
}



class operator_plus : public irange_operator
{
public:
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const;

} op_plus;

void 
operator_plus::dump (FILE *f) const
{
  fprintf (f, " + ");
}


bool
operator_plus::fold_range (irange& r, const irange& lh, const irange& rh) const
{
  return op_rr (OPM_ADD, r, lh, rh);
}


/* Adjust irange to be in terms of op1. 
   Given [range] = op1 + val,  op1 = [range] - val.  */
bool
operator_plus::op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const
{
  return op_rr (OPM_SUB, r, lhs, op2);
}

bool
operator_plus::op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const
{
  return op_rr (OPM_SUB, r, lhs, op1);
}


class operator_minus : public irange_operator
{
public:
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const;

} op_minus;

void 
operator_minus::dump (FILE *f) const
{
  fprintf (f, " - ");
}


bool
operator_minus::fold_range (irange& r, const irange& lh, const irange& rh) const
{
  return op_rr (OPM_SUB, r, lh, rh);
}


/* Adjust irange to be in terms of op1. 
   Given lhs = op1 - op2,  op1 = lhs + op2.  */
bool
operator_minus::op1_irange (irange& r, const irange& lhs,
			    const irange& op2) const
{
  return op_rr (OPM_ADD, r, lhs, op2);
}

/* Adjust irange to be in terms of op2. 
   Given lhs = op1 - op2,  -op2 = lhs - op1, therefore op2 = op1 - lhs.  */
bool
operator_minus::op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const
{
  return op_rr (OPM_SUB, r, op1 ,lhs);
}


class operator_multiply : public irange_operator
{
public:
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const;

} op_multiply;

void 
operator_multiply::dump (FILE *f) const
{
  fprintf (f, " * ");
}


bool
operator_multiply::fold_range (irange& r, const irange& lh,
			      const irange& rh) const
{
  return op_rr (OPM_MUL, r, lh, rh);
}


/* Adjust irange to be in terms of op1. 
   Given [range] = op1 + val,  op1 = [range] - val.  */
bool
operator_multiply::op1_irange (irange& r, const irange& lhs,
			       const irange& op2) const
{
  return op_rr (OPM_DIV, r, lhs, op2);
}

bool
operator_multiply::op2_irange (irange& r, const irange& lhs,
			       const irange& op1) const
{
  return op_rr (OPM_DIV, r, lhs, op1);
}


class operator_divide : public irange_operator
{
public:
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const;

} op_divide;

void 
operator_divide::dump (FILE *f) const
{
  fprintf (f, " / ");
}


bool
operator_divide::fold_range (irange& r, const irange& lh,
			     const irange& rh) const
{
  return op_rr (OPM_DIV, r, lh, rh);
}


// Adjust irange to be in terms of op1. 
bool
operator_divide::op1_irange (irange& r ATTRIBUTE_UNUSED,
			     const irange& lhs ATTRIBUTE_UNUSED,
			     const irange& op2 ATTRIBUTE_UNUSED) const
{
  // 	a_6 = foo ()
  // 	a_7 = a_6 / 4
  // if a_7 is [4,4], a_6 is actually [16,19] so ranges are trickier to
  // calulate.  punt for now.
  return false;
}

bool
operator_divide::op2_irange (irange& r ATTRIBUTE_UNUSED,
			     const irange& lhs ATTRIBUTE_UNUSED,
			     const irange& op1 ATTRIBUTE_UNUSED) const
{
  return false;
}


class operator_exact_divide : public irange_operator
{
public:
  virtual void dump (FILE *f) const;
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;

} op_exact_divide;

void 
operator_exact_divide::dump (FILE *f) const
{
  fprintf (f, " exact_div ");
}


bool
operator_exact_divide::fold_range (irange& r, const irange& lh,
			     const irange& rh) const
{
  return op_rr (OPM_EXACTDIV, r, lh, rh);
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

/* Return the range of lh converted to the type of rh.  */
bool
operator_cast::fold_range (irange& r, const irange& lh, const irange& rh) const
{
  if (lh.get_type () != rh.get_type ())
    {
      /* Handle conversion so they become the same type.  */
      r = lh;
      r.cast (rh.get_type ());
    }
  /* If they are the same type, the result should be the intersection of
     the two ranges.  */
  r = irange_intersect (lh, rh);
  return true;
}

bool
operator_cast::op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const
{
  const_tree lhs_type = lhs.get_type ();
  const_tree op2_type = op2.get_type ();
  irange op_type;

  /* if the precision of the LHS is smaller than the precision of the RHS,
     then there would be truncation of the value on the RHS, and so we can tell
     nothing about it.  */
  if (TYPE_PRECISION (lhs_type) < TYPE_PRECISION (op2_type))
    {
      r.set_range_for_type (op2_type);
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
  
  // Empty ranges are viral.
  if (lh.empty_p () || rh.empty_p ())
    {
      r.clear (boolean_type_node);
      return true;
    }

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
  /* If this is really a logical operation, call that.  */
  if (types_compatible_p (const_cast <tree> (lh.get_type ()),
			  boolean_type_node))
    return op_logical_and.fold_range (r, lh, rh);

  /* For now do nothing with bitwise AND of iranges, just return the type. */
  r.set_range_for_type (lh.get_type ());
  return true;
}

bool
operator_bitwise_and::op1_irange (irange& r, const irange& lhs,
				  const irange& op2) const
{
  /* If this is really a logical operation, call that.  */
  if (types_compatible_p (const_cast <tree> (lhs.get_type ()),
			  boolean_type_node))
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
  /* If this is really a logical operation, call that.  */
  if (types_compatible_p (const_cast <tree> (lh.get_type ()),
			  boolean_type_node))
    return op_logical_or.fold_range (r, lh, rh);

  /* For now do nothing with bitwise AND of iranges, just return the type. */
  r.set_range_for_type (lh.get_type ());
  return true;
}

bool
operator_bitwise_or::op1_irange (irange& r, const irange& lhs,
				  const irange& op2) const
{
  /* If this is really a logical operation, call that.  */
  if (types_compatible_p (const_cast <tree> (lhs.get_type ()),
			  boolean_type_node))
    return op_logical_or.op1_irange (r, lhs, op2);

  /* For now do nothing with bitwise AND of iranges, just return the type. */
  r.set_range_for_type (lhs.get_type ());
  return true;
}

bool
operator_bitwise_or::op2_irange (irange& r, const irange& lhs,
				  const irange& op1) const
{
  return operator_bitwise_or::op1_irange (r, lhs, op1);
}



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
  r = lh;
  return true;
}

bool
operator_logical_not::op1_irange (irange& r, const irange& lhs,
				  const irange& op2 ATTRIBUTE_UNUSED) const
{
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
  /* If this is a boolean not, call the logical version.  */
  if (types_compatible_p (const_cast <tree> (lh.get_type ()),
			  boolean_type_node))
    return op_logical_not.fold_range (r, lh, rh);

  /* Not sure how to logical not a range, add bitpattern support.  */
  r.set_range_for_type (lh.get_type ());
  return true;
}

bool
operator_bitwise_not::op1_irange (irange& r, const irange& lhs,
				  const irange& op2 ATTRIBUTE_UNUSED) const
{
  /* If this is a boolean not, call the logical version.  */
  if (types_compatible_p (const_cast <tree> (lhs.get_type ()),
			  boolean_type_node))
    return op_logical_not.op1_irange (r, lhs, op2);

  /* Not sure how to logical not a range, add bitpattern support.  */
  r.set_range_for_type (lhs.get_type ());
  return true;
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
  irange_tree[MULT_EXPR] = &op_multiply;
  irange_tree[TRUNC_DIV_EXPR] = &op_divide;
  irange_tree[EXACT_DIV_EXPR] = &op_exact_divide;
  
  irange_tree[NOP_EXPR] = &op_cast;
  irange_tree[CONVERT_EXPR] = &op_cast;

  irange_tree[TRUTH_AND_EXPR] = &op_logical_and;
  irange_tree[TRUTH_OR_EXPR] = &op_logical_or;
  irange_tree[TRUTH_NOT_EXPR] = &op_logical_not;

  irange_tree[BIT_AND_EXPR] = &op_bitwise_and;
  irange_tree[BIT_IOR_EXPR] = &op_bitwise_or;
  irange_tree[BIT_NOT_EXPR] = &op_bitwise_not;

  irange_tree[INTEGER_CST] = &op_integer_cst;
  irange_tree[SSA_NAME] = &op_ssa_name;
}

/* The table is hidden and accessed via a simple extern function.  */

irange_operator *
irange_op_handler (enum tree_code code)
{
  return irange_tree[code];
}


