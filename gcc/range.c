/* SSA range analysis implementation. -*- C++ -*-
   Copyright (C) 2017 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "ssa.h"
#include "range.h"
#include "selftest.h"
#include "wide-int-range.h"

// Common code between the alternate irange implementations.

// Return TRUE if two types are compatible for range operations.

static bool
range_compatible_p (tree t1, tree t2)
{
  if (POINTER_TYPE_P (t1) && POINTER_TYPE_P (t2))
    return true;

  // FORTRAN has different precision booleans that trigger a false
  // from types_compatible_p.
  if (TREE_CODE (t1) == BOOLEAN_TYPE && TREE_CODE (t2) == BOOLEAN_TYPE)
      return true;

  return types_compatible_p (t1, t2);
}

bool
irange::operator== (const irange &r) const
{
  /* Special case UNDEFINED, because the value_range_base
     implementation may not have a type for a freshly initialized
     UNDEFINED.  */
  if (undefined_p ())
    return r.undefined_p ();

  if (num_pairs () != r.num_pairs ()
      || !range_compatible_p (type (), r.type ()))
    return false;

  for (unsigned p = 0; p < num_pairs (); p++)
    if (wi::ne_p (lower_bound (p), r.lower_bound (p))
	|| wi::ne_p (upper_bound (p), r.upper_bound (p)))
      return false;

  return true;
}

bool
irange::operator!= (const irange &r) const
{
  return !(*this == r);
}

// Set range from an SSA_NAME's available range.  If there is no
// available range, build a range for its entire domain.

irange
range_from_ssa (tree ssa)
{
  tree type = TREE_TYPE (ssa);
  gcc_checking_assert (irange::supports_type_p (type));
  if (!SSA_NAME_RANGE_INFO (ssa) || POINTER_TYPE_P (type))
    return irange (type);
  wide_int min, max;
  enum value_range_kind kind = get_range_info (ssa, &min, &max);
  return value_range_to_irange (type, kind, min, max);
}

// This function returns a range for tree node EXPR in R.  Return
// false if ranges are not supported.

bool
get_tree_range (irange &r, tree expr)
{
  tree type;
  switch (TREE_CODE (expr))
    {
      case INTEGER_CST:
        if (!TREE_OVERFLOW_P (expr))
	  r = irange (expr, expr);
	else
	  // If we encounter an overflow, simply punt and drop to varying
	  // since we hvae no idea how it will be used.
	  r.set_varying (TREE_TYPE (expr));
	return true;

      case SSA_NAME:
        if (irange::supports_ssa_p (expr))
	  {
	    r = range_from_ssa (expr);
	    return true;
	  }
	break;

      case ADDR_EXPR:
        {
	  // handle &var which can show up in phi arguments
	  bool ov;
	  type = TREE_TYPE (expr);
	  if (irange::supports_type_p (type))
	    {
	      if (tree_single_nonzero_warnv_p (expr, &ov))
		r = range_nonzero (type);
	      else
		r.set_varying (type);
	      return true;
	    }
	  break;
	}

      default:
	if (TYPE_P (expr))
	  type = expr;
	else
	  type = TREE_TYPE (expr);
	if (irange::supports_type_p (type))
	  {
	    // Set to range for this type.
	    r.set_varying (type);
	    return true;
	  }
	break;
    }

  return false;
}

irange
range_intersect (const irange &r1, const irange &r2)
{
  irange tmp (r1);
  tmp.intersect (r2);
  return tmp;
}

irange
range_invert (const irange &r1)
{
  irange tmp (r1);
  tmp.invert ();
  return tmp;
}

irange
range_union (const irange &r1, const irange &r2)
{
  irange tmp (r1);
  tmp.union_ (r2);
  return tmp;
}

irange
range_zero (tree type)
{
  return irange (build_zero_cst (type), build_zero_cst (type));
}

irange
range_nonzero (tree type)
{
  return irange (IRANGE_INVERSE,
		 build_zero_cst (type), build_zero_cst (type));
}

irange
range_positives (tree type)
{
  unsigned prec = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  return irange (type, wi::zero (prec), wi::max_value (prec, sign));
}

irange
range_negatives (tree type)
{
  unsigned prec = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  irange r;
  if (sign == UNSIGNED)
    r.set_undefined (type);
  else
    r = irange (type, wi::min_value (prec, sign), wi::minus_one (prec));
  return r;
}

#if !IRANGE_WITH_VALUE_RANGE
// Standalone irange implementation.

// Subtract 1 from X and set OVERFLOW if the operation overflows.

static wide_int inline
subtract_one (const wide_int &x, tree type, wi::overflow_type &overflow)
{
  // A signed 1-bit bit-field, has a range of [-1,0] so subtracting +1
  // overflows, since +1 is unrepresentable.  This is why we have an
  // addition of -1 here.
  if (TYPE_SIGN (type) == SIGNED)
    return wi::add (x, -1 , SIGNED, &overflow);
  else
    return wi::sub (x, 1, UNSIGNED, &overflow);
}

// Set range from wide ints.

void
irange::init (tree type, const wide_int &lbound, const wide_int &ubound,
	      irange_kind rt)
{
  gcc_checking_assert (irange::supports_type_p (type));
  gcc_checking_assert (TYPE_PRECISION (type) == lbound.get_precision ());
  gcc_checking_assert (lbound.get_precision () == ubound.get_precision ());
  m_type = type;
  gcc_checking_assert (wi::le_p (lbound, ubound, TYPE_SIGN (type)));
  if (rt == IRANGE_INVERSE)
    {
      // Calculate INVERSE([I,J]) as [-MIN, I-1][J+1, +MAX].
      wi::overflow_type ovf;
      m_nitems = 0;
      wide_int min = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
      wide_int max = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));

      // If we will overflow, don't bother.  This will handle unsigned
      // underflow which doesn't set the overflow bit.
      if (lbound != min)
	{
	  m_bounds[m_nitems++] = min;
	  m_bounds[m_nitems++] = subtract_one (lbound, type, ovf);
	  if (ovf)
	    m_nitems = 0;
	}
      // If we will overflow, don't bother.  This will handle unsigned
      // overflow which doesn't set the overflow bit.
      if (ubound != max)
	{
	  m_bounds[m_nitems++] = wi::add (ubound, 1, TYPE_SIGN (type), &ovf);
	  if (ovf)
	    m_nitems--;
	  else
	    m_bounds[m_nitems++] = max;
	}
      // If we get here with N==0, it means we tried to calculate the
      // inverse of [-MIN, +MAX] which is actually the empty set, and
      // N==0 maps nicely to the empty set.
    }
  else
    {
      m_bounds[0] = lbound;
      m_bounds[1] = ubound;
      m_nitems = 2;
    }
}

irange::irange (tree type)
{
  set_varying (type);
}

irange::irange (irange_kind rt, tree type,
		const wide_int &lbound, const wide_int &ubound)
{
  init (type, lbound, ubound, rt);
}

irange::irange (tree type, const wide_int &lbound, const wide_int &ubound)
{
  init (type, lbound, ubound, IRANGE_PLAIN);
}

irange::irange (irange_kind rt, tree lbound, tree ubound)
{
  tree type = TREE_TYPE (lbound);
  init (type, wi::to_wide (lbound), wi::to_wide (ubound), rt);
}

irange::irange (tree lbound, tree ubound)
{
  tree type = TREE_TYPE (lbound);
  init (type, wi::to_wide (lbound), wi::to_wide (ubound), IRANGE_PLAIN);
}

// Mark pair [i, j] to empty.  This is done by building a non-sensical pair.

void
irange_storage::set_empty_pair (unsigned i, unsigned j, tree type)
{
  unsigned precision = trailing_bounds[0].get_precision ();
  if (precision == 1 && TYPE_SIGN (type) == SIGNED)
    {
      // For stupid ass signed 1-bit types, we can't use [1, 0] as a
      // nonsensical pair, since it maps to [-1, 0] which is valid.
      // In this case, use [0, 1] which is invalid in this brain-dead world.
      trailing_bounds[i] = wi::zero (precision);
      trailing_bounds[j] = wi::one (precision);
    }
  else
    {
      // For almost all types, we mark empty ranges with a nonsensical [1, 0] range.
      trailing_bounds[i] = wi::one (precision);
      trailing_bounds[j] = wi::zero (precision);
    }
}

irange::irange (tree type, const irange_storage *storage)
{
  m_type = type;
  m_nitems = 0;
  unsigned i = 0;
  unsigned precision = wi::get_precision (storage->trailing_bounds[0]);
  gcc_checking_assert (precision == TYPE_PRECISION (type));
  while (i < m_max_pairs * 2)
    {
      if (storage->empty_pair_p (i, i + 1, type))
	break;
      m_bounds[i] = storage->trailing_bounds[i];
      m_bounds[i + 1] = storage->trailing_bounds[i + 1];
      i += 2;
    }
  m_nitems = i;
}

// Set range from the full domain of TYPE.

void
irange::set_varying (tree type)
{
  wide_int min = wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type));
  wide_int max = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
  init (type, min, max);
}

bool
irange::valid_p () const
{
  if (m_type == NULL
      || m_nitems % 2
      || m_nitems > m_max_pairs * 2)
    return false;

  if (undefined_p ())
    return true;

  // Check that the bounds are in the right order.
  // So for [a,b][c,d][e,f] we must have: a <= b < c <= d < e <= f.
  if (wi::gt_p (lower_bound (0), upper_bound (0), TYPE_SIGN (m_type)))
    return false;
  for (unsigned p = 1; p < num_pairs (); ++p)
    {
      if (wi::le_p (lower_bound (p), upper_bound (p - 1), TYPE_SIGN (m_type)))
	return false;
      if (wi::gt_p (lower_bound (p), upper_bound (p), TYPE_SIGN (m_type)))
	return false;
    }
  return true;
}

void
irange::check () const
{
  gcc_assert (valid_p ());
}

// Convert the current range in place into a range of type NEW_TYPE.

void
irange::cast (tree new_type)
{
  // If the expression involves a pointer, we are only interested in
  // determining if it evaluates to NULL [0, 0] or non-NULL (~[0, 0]).
  if (POINTER_TYPE_P (new_type) || POINTER_TYPE_P (m_type))
    {
      if (!contains_p (wi::zero (TYPE_PRECISION (m_type))))
	{
	  // Don't use range_nonzero because it will recurse into cast().
	  unsigned prec = TYPE_PRECISION (new_type);
	  irange nz (IRANGE_INVERSE, new_type,
		     wi::zero (prec), wi::zero (prec));
	  *this = nz;
	}
      else if (zero_p ())
	*this = range_zero (new_type);
      else
	set_varying (new_type);
      return;
    }

  // If nothing changed, this is a simple type conversion between two
  // variants of the same type.
  bool sign_change = TYPE_SIGN (new_type) != TYPE_SIGN (m_type);
  unsigned old_precision = TYPE_PRECISION (m_type);
  unsigned new_precision = TYPE_PRECISION (new_type);
  signop old_sign = TYPE_SIGN (m_type);
  signop new_sign = TYPE_SIGN (new_type);
  if (undefined_p () || (!sign_change && old_precision == new_precision))
    {
      m_type = new_type;
      return;
    }

  wide_int orig_lowest = lower_bound ();
  wide_int orig_highest = upper_bound ();
  wide_int new_type_min = wi::min_value (new_precision, new_sign);
  wide_int new_type_max = wi::max_value (new_precision, new_sign);
  unsigned n = m_nitems;
  for (unsigned i = 0; i < n; i += 2)
    {
      // If this sub-range doesn't fit in the new range type, bail.
      wide_int new_min, new_max;
      if (!wide_int_range_convert (new_min, new_max,
				   old_sign, old_precision,
				   new_sign, new_precision,
				   m_bounds[i], m_bounds[i + 1]))
	{
	  m_type = new_type;
	  m_nitems = 2;
	  m_bounds[0] = new_type_min;
	  m_bounds[1] = new_type_max;
	  return;
	}
      // If the new bounds are in the right order, we can do a
      // straight up conversion.
      if (wi::le_p (new_min, new_max, new_sign))
	{
	  m_bounds[i] = new_min;
	  m_bounds[i + 1] = new_max;
	}
      // Otherwise, the bounds have wrapped and we must handle them
      // specially as [-MIN,Y][X,MAX].
      else
	{
	  /* For one bit precision, the swapped range covers all values.  */
	  if (TYPE_PRECISION (new_type) == 1)
	    {
	      set_varying (new_type);
	      return;
	    }
	  // If we're about to go over the maximum number of ranges,
	  // convert to something conservative and cast again.
	  if (m_nitems >= m_max_pairs * 2)
	    {
	      m_nitems = 2;
	      m_bounds[0] = orig_lowest;
	      m_bounds[1] = orig_highest;
	      cast (new_type);
	      return;
	    }
	  // Handle wrapping of [X,Y] as [-MIN,Y][X,MAX].
	  m_bounds[i] = new_type_min;
	  m_bounds[i + 1] = new_max;
	  // If we're about to construct [-MIN, MAX], no sense
	  // calculating anything else.
	  if (m_bounds[i + 1] == new_type_max)
	    {
	      m_nitems = 2;
	      m_type = new_type;
	      m_bounds[0] = new_type_min;
	      m_bounds[1] = new_type_max;
	      return;
	    }
	  m_bounds[m_nitems++] = new_min;
	  m_bounds[m_nitems++] = new_type_max;
	}
    }
  m_type = new_type;
  canonicalize ();
}

// Return TRUE if the current range contains wide-int ELEMENT.

bool
irange::contains_p (const wide_int &element) const
{
  for (unsigned p = 0; p < num_pairs (); ++p)
    if (wi::ge_p (element, lower_bound (p), TYPE_SIGN (m_type))
	&& wi::le_p (element, upper_bound (p), TYPE_SIGN (m_type)))
      return true;
  return false;
}

// Return TRUE if the current range contains tree ELEMENT.

bool
irange::contains_p (tree element) const
{
  return contains_p (wi::to_wide (element));
}

// Remove PAIR.

void
irange::remove_pair (unsigned pair)
{
  unsigned i = pair * 2;
  unsigned j = i + 1;
  gcc_checking_assert (i < m_nitems && i < j);
  unsigned dst = i;
  unsigned ndeleted = j - i + 1;
  for (++j; j < m_nitems; ++j)
    m_bounds[dst++] = m_bounds[j];
  m_nitems -= ndeleted;
}

// Canonicalize the current range.

void
irange::canonicalize ()
{
  if (undefined_p ())
    return;

  // Fix any out of order ranges: [10,20][-5,5] into [-5,5][10,20].
  signop sign = TYPE_SIGN (m_type);
  for (unsigned p = 0; p < num_pairs (); ++p)
    for (unsigned q = p; q < num_pairs (); ++q)
      if (wi::gt_p (lower_bound (p), lower_bound (q), sign))
	{
	  wide_int t1 = lower_bound (p);
	  wide_int t2 = upper_bound (p);
	  set_lower_bound (p, lower_bound (q));
	  set_upper_bound (p, upper_bound (q));
	  set_lower_bound (q, t1);
	  set_upper_bound (q, t2);
	}
  // Merge sub-ranges when appropriate.
  for (unsigned p = 0; p < num_pairs () - 1; )
    {
      // Merge edges that touch:
      // [9,10][11,20] => [9,20]
      // [9,10][10,20] => [9,20].
      wi::overflow_type ovf;
      if (upper_bound (p) == lower_bound (p + 1)
	  || (wi::add (upper_bound (p), 1, sign, &ovf) == lower_bound (p + 1)
	      && !ovf))
	{
	  set_upper_bound (p, upper_bound (p + 1));
	  remove_pair (p + 1);
	}
      // Merge pairs that bleed into each other:
      // [10,20][11,18] => [10,20]
      // [10,20][15,30] => [10,30]
      else if (wi::le_p (lower_bound (p), lower_bound (p + 1), sign)
	       && wi::ge_p (upper_bound (p), lower_bound (p + 1), sign))
	{
	  set_upper_bound (p, wi::max (upper_bound (p), upper_bound (p + 1), sign));
	  remove_pair (p + 1);
	}
      else
	++p;
    }
  if (flag_checking)
    check ();
}

// THIS = THIS U R

void
irange::union_ (const irange &r)
{
  gcc_checking_assert (range_compatible_p (m_type, r.m_type));

  if (undefined_p ())
    {
      *this = r;
      return;
    }
  else if (r.undefined_p ())
    return;

  // Do not worry about merging and such by reserving twice as many
  // pairs as needed, and then simply sort the 2 ranges into this
  // intermediate form.
  //
  // The intermediate result will have the property that the beginning
  // of each range is <= the beginning of the next range.  There may
  // be overlapping ranges at this point.  I.e. this would be valid
  // [-20, 10], [-10, 0], [0, 20], [40, 90] as it satisfies this
  // contraint : -20 < -10 < 0 < 40 When the range is rebuilt into r,
  // the merge is performed.
  //
  // [Xi,Yi]..[Xn,Yn]  U  [Xj,Yj]..[Xm,Ym]   -->  [Xk,Yk]..[Xp,Yp]
  signop sign = TYPE_SIGN (m_type);
  wide_int res[m_max_pairs * 4];
  wide_int u1 ;
  wi::overflow_type ovf;
  unsigned i = 0, j = 0, k = 0;

  while (i < m_nitems && j < r.m_nitems)
    {
      // lower of Xi and Xj is the lowest point.
      if (wi::le_p (m_bounds[i], r.m_bounds[j], sign))
        {
	  res[k++] = m_bounds[i];
	  res[k++] = m_bounds[i + 1];
	  i += 2;
	}
      else
        {
	  res[k++] = r.m_bounds[j];
	  res[k++] = r.m_bounds[j + 1];
	  j += 2;
	}
    }
  for ( ; i < m_nitems; i += 2)
    {
      res[k++] = m_bounds[i];
      res[k++] = m_bounds[i + 1];
    }
  for ( ; j < r.m_nitems; j += 2)
    {
      res[k++] = r.m_bounds[j];
      res[k++] = r.m_bounds[j + 1];
    }

  // Now normalize the vector removing any overlaps.
  i = 2;
  int prec = TYPE_PRECISION (m_type);
  wide_int max_val = wi::max_value (prec, sign);
  for (j = 2; j < k ; j += 2)
    {
      if (res[i - 1] == max_val)
        break;
      u1 = wi::add (res[i - 1], 1, sign, &ovf);

      // Overflow indicates we are at MAX already.
      // A wide int bug requires the previous max_val check
      // trigger: gcc.c-torture/compile/pr80443.c  with -O3
      if (ovf)
        break;

      // Current upper+1 is >= lower bound next pair, then we merge ranges.
      if (wi::ge_p (u1, res[j], sign))
        {
	  // New upper bounds is greater of current or the next one.
	  if (wi::gt_p (res[j + 1], res [i - 1], sign))
	    res [i - 1] = res[j + 1];
	}
      else
        {
	  // This is a new distinct range, but no point in copying it
	  // if it is already in the right place.
	  if (i != j)
	    {
	      res[i++] = res[j];
	      res[i++] = res[j + 1];
	    }
	  else
	    i += 2;

	}
    }
  
  // At this point, the vector should have i ranges, none
  // overlapping. Now it simply needs to be copied, and if there are
  // too many ranges, merge some.  We wont do any analysis as to what
  // the "best" merges are, simply combine the final ranges into one.
  if (i > m_max_pairs * 2)
    {
      res[m_max_pairs * 2 - 1] = res[i - 1];
      i = m_max_pairs * 2;
    }

  for (j = 0; j < i ; j++)
    m_bounds[j] = res [j];
  m_nitems = i;

  if (flag_checking)
    check ();
}

// THIS = THIS ^ [X,Y].

void
irange::intersect (const wide_int &x, const wide_int &y)
{
  unsigned pos = 0;

  for (unsigned i = 0; i < m_nitems; i += 2)
    {
      signop sign = TYPE_SIGN (m_type);
      wide_int newlo = wi::max (m_bounds[i], x, sign);
      wide_int newhi = wi::min (m_bounds[i + 1], y, sign);
      if (wi::gt_p (newlo, newhi, sign))
	{
	  // If the new sub-range doesn't make sense, it's an
	  // impossible range and must be kept out of the result.
	}
      else
	{
	  m_bounds[pos++] = newlo;
	  m_bounds[pos++] = newhi;
	}
    }
  m_nitems = pos;
  if (flag_checking)
    check ();
}

// THIS = THIS ^ R.

void
irange::intersect (const irange &r)
{
  gcc_checking_assert (range_compatible_p (m_type, r.m_type));
  irange orig_range (*this);

  // Intersection with an empty range is an empty range.
  set_undefined ();
  if (orig_range.undefined_p () || r.undefined_p ())
    return;

  // The general algorithm is as follows.
  //
  // Intersect each sub-range of R with all of ORIG_RANGE one at a time, and
  // join/union the results of these intersections together.  I.e:
  //
  //   [10,20][30,40][50,60] ^ [15,25][38,51][55,70]
  //
  // Step 1: [10,20][30,40][50,60] ^ [15,25] => [15,20]
  // Step 2: [10,20][30,40][50,60] ^ [38,51] => [38,40]
  // Step 3: [10,20][30,40][50,60] ^ [55,70] => [55,60]
  // Final:  [15,20] U [38,40] U [55,60] => [15,20][38,40][55,60]
  for (unsigned i = 0; i < r.m_nitems; i += 2)
    {
      irange tmp (orig_range);
      tmp.intersect (r.m_bounds[i], r.m_bounds[i + 1]);
      union_ (tmp);
    }
  // There is no check here because the calls to union_ above would
  // have verified sanity.
}

// Set THIS to the inverse of its range.

void
irange::invert ()
{
  // We always need one more set of bounds to represent an inverse, so
  // if we're at the limit, we can't properly represent things.
  //
  // For instance, to represent the inverse of a 2 sub-range set
  // [5, 10][20, 30], we would need a 3 sub-range set
  // [-MIN, 4][11, 19][31, MAX].
  //
  // In this case, return the most conservative thing.
  //
  // However, if any of the extremes of the range are -MIN/+MAX, we
  // know we will not need an extra bound.  For example:
  //
  // 	INVERT([-MIN,20][30,40]) => [21,29][41,+MAX]
  // 	INVERT([-MIN,20][30,MAX]) => [21,29]
  wide_int min = wi::min_value (TYPE_PRECISION (m_type), TYPE_SIGN (m_type));
  wide_int max = wi::max_value (TYPE_PRECISION (m_type), TYPE_SIGN (m_type));
  if (m_nitems == m_max_pairs * 2
      && m_bounds[0] != min
      && m_bounds[m_nitems] != max)
    {
      m_bounds[1] = max;
      m_nitems = 2;
      return;
    }

  // The inverse of the empty set is the entire domain.
  if (undefined_p ())
    {
      set_varying (m_type);
      return;
    }

  // The algorithm is as follows.  To calculate INVERT ([a,b][c,d]), we
  // generate [-MIN, a-1][b+1, c-1][d+1, MAX].
  //
  // If there is an over/underflow in the calculation for any
  // sub-range, we eliminate that subrange.  This allows us to easily
  // calculate INVERT([-MIN, 5]) with: [-MIN, -MIN-1][6, MAX].  And since
  // we eliminate the underflow, only [6, MAX] remains.

  unsigned i = 0;
  wi::overflow_type ovf;

  // Construct leftmost range.
  irange orig_range (*this);
  m_nitems = 0;
  // If this is going to underflow on the MINUS 1, don't even bother
  // checking.  This also handles subtracting one from an unsigned 0,
  // which doesn't set the underflow bit.
  if (min != orig_range.m_bounds[i])
    {
      m_bounds[m_nitems++] = min;
      m_bounds[m_nitems++] = subtract_one (orig_range.m_bounds[i],
					   m_type, ovf);
      if (ovf)
	m_nitems = 0;
    }
  i++;
  // Construct middle ranges if applicable.
  if (orig_range.m_nitems > 2)
    {
      unsigned j = i;
      for (; j < (unsigned) (orig_range.m_nitems - 2); j += 2)
	{
	  // The middle ranges cannot have MAX/MIN, so there's no need
	  // to check for unsigned overflow on the +1 and -1 here.
	  m_bounds[m_nitems++]
	    = wi::add (orig_range.m_bounds[j], 1, TYPE_SIGN (m_type), &ovf);
	  m_bounds[m_nitems++]
	    = subtract_one (orig_range.m_bounds[j + 1], m_type, ovf);
	  if (ovf)
	    m_nitems -= 2;
	}
      i = j;
    }
  // Construct rightmost range.
  //
  // However, if this will overflow on the PLUS 1, don't even bother.
  // This also handles adding one to an unsigned MAX, which doesn't
  // set the overflow bit.
  if (max != orig_range.m_bounds[i])
    {
      m_bounds[m_nitems++]
	= wi::add (orig_range.m_bounds[i], 1, TYPE_SIGN (m_type), &ovf);
      m_bounds[m_nitems++] = max;
      if (ovf)
	m_nitems -= 2;
    }

  if (flag_checking)
    check ();
}

// Dump the current range onto BUFFER.

void
irange::dump (pretty_printer *buffer) const
{
  wide_int min = wi::min_value (TYPE_PRECISION (m_type), TYPE_SIGN (m_type));
  wide_int max = wi::max_value (TYPE_PRECISION (m_type), TYPE_SIGN (m_type));
  if (POINTER_TYPE_P (m_type) && nonzero_p ())
    pp_string (buffer, "[ non-zero pointer ]");
  else
    for (unsigned i = 0; i < m_nitems; ++i)
      {
	if (i % 2 == 0)
	  pp_character (buffer, '[');

	// Wide ints may be sign extended to the full extent of the
	// underlying HWI storage, even if the precision we care about
	// is smaller.  Chop off the excess bits for prettier output.
	signop sign = TYPE_UNSIGNED (m_type) ? UNSIGNED : SIGNED;
	widest_int val = widest_int::from (m_bounds[i], sign);
	val &= wi::mask<widest_int> (m_bounds[i].get_precision (), false);

	if (i == 0
	    && INTEGRAL_TYPE_P (m_type)
	    && !TYPE_UNSIGNED (m_type)
	    && m_bounds[i] == min
	    && TYPE_PRECISION (m_type) != 1)
	  pp_string (buffer, "-INF");
	else if (i + 1 == m_nitems
	    && INTEGRAL_TYPE_P (m_type)
	    && !TYPE_UNSIGNED (m_type)
	    && m_bounds[i] == max
	    && TYPE_PRECISION (m_type) != 1)
	  pp_string (buffer, "+INF");
	else
	  {
	    if (val > 0xffff)
	      print_hex (val, pp_buffer (buffer)->digit_buffer);
	    else
	      print_dec (m_bounds[i], pp_buffer (buffer)->digit_buffer, sign);
	    pp_string (buffer, pp_buffer (buffer)->digit_buffer);
	  }
	if (i % 2 == 0)
	  pp_string (buffer, ", ");
	else
	  pp_character (buffer, ']');
      }
  if (undefined_p ())
    pp_string (buffer, "[]");

  pp_character (buffer, ' ');
  dump_generic_node (buffer, m_type, 0, TDF_NONE, false);
  pp_flush (buffer);
}

// Dump the current range onto FILE F.

void
irange::dump (FILE *f) const
{
  pretty_printer buffer;
  buffer.buffer->stream = f;
  dump (&buffer);
}

// Like above but dump to STDERR.
//
// You'd think we could have a default parameter for dump(FILE),
// but gdb currently doesn't do default parameters gracefully-- or at
// all, and since this is a function we need to be callable from the
// debugger...

void
irange::dump () const
{
  dump (stderr);
}

// Initialize the current irange_storage to the irange in IR.

void
irange_storage::set (const irange &ir)
{
  unsigned precision = TYPE_PRECISION (ir.type ());
  trailing_bounds.set_precision (precision);
  unsigned i;
  for (i = 0; i < ir.num_pairs () * 2; ++i)
    trailing_bounds[i] = ir.m_bounds[i];

  // Clear the remaining empty ranges.
  for (; i < irange::m_max_pairs * 2; i += 2)
    set_empty_pair (i, i + 1, ir.type ());
}

// Update a previously initialized irange_storage to NEW_RANGE, iff the
// precision of the present range is the same as the precision of
// the new range.  Return TRUE if update was successful.

bool
irange_storage::update (const irange &new_range)
{
  if (trailing_bounds.get_precision () == TYPE_PRECISION (new_range.type ()))
    {
      set (new_range);
      return true;
    }
  return false;
}

// Return TRUE if range contains exactly one element and set RESULT to it.

bool
irange::singleton_p (tree *result) const
{
  if (num_pairs () == 1 && lower_bound (0) == upper_bound (0))
    {
      if (result)
	*result = wide_int_to_tree (type (), lower_bound ());
      return true;
    }
  return false;
}

// Convert irange  to a value_range_kind.

value_range_base
irange_to_value_range (const irange &r)
{
  value_range_base vr;
  if (r.varying_p ())
    {
      vr.set_varying (r.type ());
      return vr;
    }
  if (r.undefined_p ())
    {
      vr.set_undefined (r.type ());
      return vr;
    }
  tree type = r.type ();
  unsigned int precision = TYPE_PRECISION (type);
  // Represent non-zero correctly.
  if (TYPE_UNSIGNED (type)
      && r.num_pairs () == 1
      && r.lower_bound () == wi::uhwi (1, precision)
      && r.upper_bound () == wi::max_value (precision, UNSIGNED)
      // Do not get confused by booleans.
      && TYPE_PRECISION (type) != 1)
    vr = value_range (VR_ANTI_RANGE,
		      build_int_cst (type, 0), build_int_cst (type, 0));
  // Represent anti-ranges.
  else if ((r.num_pairs () == 2
	    || r.num_pairs () == 3)
	   // Do not get confused by booleans.
	   && TYPE_PRECISION (type) != 1
	   && r.lower_bound () == wi::min_value (precision, TYPE_SIGN (type))
	   && r.upper_bound () == wi::max_value (precision, TYPE_SIGN (type)))
    {
      irange tmp = r;
      if (r.num_pairs () == 3)
	{
	  // Hack to make up for the fact that we can compute finer
	  // grained ranges that VRP can only approximate with an
	  // anti-range.  Attempt to reconstruct sub-ranges of the form:
	  //
	  //	[0, 94][96, 127][0xff80, 0xffff] => ~[95,95]
	  //	[0, 1][3, 0x7fffffff][0xff..80000000, 0xff..ff] => ~[2, 2].
	  //
	  // Merge the last two bounds.
	  tmp = irange (type, r.lower_bound (0), r.upper_bound (0));
	  tmp.union_ (irange (type, r.lower_bound (1), r.upper_bound ()));
	}
      tmp = range_invert (tmp);
      vr = value_range (VR_ANTI_RANGE,
			wide_int_to_tree (type, tmp.lower_bound ()),
			wide_int_to_tree (type, tmp.upper_bound ()));
    }
  else
    vr = value_range (VR_RANGE,
		      wide_int_to_tree (type, r.lower_bound ()),
		      wide_int_to_tree (type, r.upper_bound ()));
  return vr;
}

// Convert a value_range to an irange and store it in R.

irange
value_range_to_irange (tree type, enum value_range_kind kind,
		       const wide_int &min, const wide_int &max)
{
  gcc_checking_assert (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type));
  irange r;
  if (kind == VR_VARYING || kind == VR_UNDEFINED)
    r.set_varying (type);
  else
    r = irange (kind == VR_ANTI_RANGE ? IRANGE_INVERSE : IRANGE_PLAIN,
		type, min, max);
  return r;
}

// Same as above, but takes an entire value_range instead of piecemeal.

irange
value_range_to_irange (tree type, const value_range_base &vr)
{
  irange r;
  if (vr.varying_p () || vr.undefined_p ())
    r.set_varying (type);
  else
    r = value_range_to_irange (TREE_TYPE (vr.min ()), vr.kind (),
			       wi::to_wide (vr.min ()),
			       wi::to_wide (vr.max ()));
  return r;
}
#endif // !IRANGE_WITH_VALUE_RANGE

#if CHECKING_P
#include "stor-layout.h"

// Ideally this should go in namespace selftest, but irange_tests
// needs to be a friend of class irange so it can access
// irange::m_max_pairs.

#define INT(N) build_int_cst (integer_type_node, (N))
#define UINT(N) build_int_cstu (unsigned_type_node, (N))
#define INT16(N) build_int_cst (short_integer_type_node, (N))
#define UINT16(N) build_int_cstu (short_unsigned_type_node, (N))
#define INT64(N) build_int_cstu (long_long_integer_type_node, (N))
#define UINT64(N) build_int_cstu (long_long_unsigned_type_node, (N))
#define UINT128(N) build_int_cstu (u128_type, (N))
#define UCHAR(N) build_int_cstu (unsigned_char_type_node, (N))
#define SCHAR(N) build_int_cst (signed_char_type_node, (N))

#define RANGE3(A,B,C,D,E,F)		\
( i1 = irange (INT (A), INT (B)),	\
  i2 = irange (INT (C), INT (D)),	\
  i3 = irange (INT (E), INT (F)),	\
  i1.union_ (i2),			\
  i1.union_ (i3),			\
  i1 )

// Run all of the selftests within this file.

void
irange_tests ()
{
  tree u128_type = build_nonstandard_integer_type (128, /*unsigned=*/1);
  irange i1, i2, i3;
  irange r0, r1, rold;

  // Test that NOT(255) is [0..254] in 8-bit land.
  irange not_255 (IRANGE_INVERSE, UCHAR (255), UCHAR (255));
  ASSERT_TRUE (not_255 == irange (UCHAR (0), UCHAR (254)));

  // Test that NOT(0) is [1..255] in 8-bit land.
  irange not_zero = range_nonzero (unsigned_char_type_node);
  ASSERT_TRUE (not_zero == irange (UCHAR (1), UCHAR (255)));

  // Check that [0,127][0x..ffffff80,0x..ffffff]
  //  => ~[128, 0x..ffffff7f].
  r0 = irange (UINT128 (0), UINT128 (127));
  tree high = build_minus_one_cst (u128_type);
  // low = -1 - 127 => 0x..ffffff80.
  tree low = fold_build2 (MINUS_EXPR, u128_type, high, UINT128(127));
  r1 = irange (low, high); // [0x..ffffff80, 0x..ffffffff]
  // r0 = [0,127][0x..ffffff80,0x..fffffff].
  r0.union_ (r1);
  // r1 = [128, 0x..ffffff7f].
  r1 = irange (UINT128(128),
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
  r0 = irange (UINT (0), UINT (5));
  r0.invert ();
  ASSERT_TRUE (r0 == irange (UINT(6), maxuint));

  // Check that ~[10,MAX] => [0,9] for unsigned int.
  r0 = irange (IRANGE_PLAIN, UINT(10), maxuint);
  r0.invert ();
  ASSERT_TRUE (r0 == irange (UINT (0), UINT (9)));

  // Check that ~[0,5] => [6,MAX] for unsigned 128-bit numbers.
  r0 = irange (IRANGE_INVERSE, UINT128 (0), UINT128 (5));
  r1 = irange (UINT128(6), build_minus_one_cst (u128_type));
  ASSERT_TRUE (r0 == r1);

  // Check that [~5] is really [-MIN,4][6,MAX].
  r0 = irange (IRANGE_INVERSE, INT (5), INT (5));
  r1 = irange (minint, INT (4));
  r1.union_ (irange (INT (6), maxint));
  ASSERT_FALSE (r1.undefined_p ());
  ASSERT_TRUE (r0 == r1);

  r1 = irange (INT (5), INT (5));
  r1.check ();
  irange r2 (r1);
  ASSERT_TRUE (r1 == r2);

  r1 = irange (INT (5), INT (10));
  r1.check ();

  r1 = irange (integer_type_node,
	       wi::to_wide (INT (5)), wi::to_wide (INT (10)));
  r1.check ();
  ASSERT_TRUE (r1.contains_p (INT (7)));

  r1 = irange (SCHAR (0), SCHAR (20));
  ASSERT_TRUE (r1.contains_p (SCHAR(15)));
  ASSERT_FALSE (r1.contains_p (SCHAR(300)));

  // If a range is in any way outside of the range for the converted
  // to range, default to the range for the new type.
  r1 = irange (integer_zero_node, maxint);
  r1.cast (short_integer_type_node);
  ASSERT_TRUE (r1.lower_bound () == wi::to_wide (minshort)
	       && r1.upper_bound() == wi::to_wide (maxshort));

  // (unsigned char)[-5,-1] => [251,255].
  r0 = rold = irange (SCHAR (-5), SCHAR (-1));
  r0.cast (unsigned_char_type_node);
  ASSERT_TRUE (r0 == irange (UCHAR (251), UCHAR (255)));
  r0.cast (signed_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // (signed char)[15, 150] => [-128,-106][15,127].
  r0 = rold = irange (UCHAR (15), UCHAR (150));
  r0.cast (signed_char_type_node);
  r1 = irange (SCHAR (15), SCHAR (127));
  r2 = irange (SCHAR (-128), SCHAR (-106));
  r1.union_ (r2);
  ASSERT_TRUE (r1 == r0);
  r0.cast (unsigned_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // (unsigned char)[-5, 5] => [0,5][251,255].
  r0 = rold = irange (SCHAR (-5), SCHAR (5));
  r0.cast (unsigned_char_type_node);
  r1 = irange (UCHAR (251), UCHAR (255));
  r2 = irange (UCHAR (0), UCHAR (5));
  r1.union_ (r2);
  ASSERT_TRUE (r0 == r1);
  r0.cast (signed_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // (unsigned char)[-5,5] => [0,5][251,255].
  r0 = irange (INT (-5), INT (5));
  r0.cast (unsigned_char_type_node);
  r1 = irange (UCHAR (0), UCHAR (5));
  r1.union_ (irange (UCHAR (251), UCHAR (255)));
  ASSERT_TRUE (r0 == r1);

  // (unsigned char)[5U,1974U] => [0,255].
  r0 = irange (UINT (5), UINT (1974));
  r0.cast (unsigned_char_type_node);
  ASSERT_TRUE (r0 == irange (UCHAR (0), UCHAR (255)));
  r0.cast (integer_type_node);
  // Going to a wider range should not sign extend.
  ASSERT_TRUE (r0 == irange (INT (0), INT (255)));

  // (unsigned char)[-350,15] => [0,255].
  r0 = irange (INT (-350), INT (15));
  r0.cast (unsigned_char_type_node);
  ASSERT_TRUE (r0 == irange (TYPE_MIN_VALUE (unsigned_char_type_node),
			     TYPE_MAX_VALUE (unsigned_char_type_node)));

  // Casting [-120,20] from signed char to unsigned short.
  // => [0, 20][0xff88, 0xffff].
  r0 = irange (SCHAR (-120), SCHAR (20));
  r0.cast (short_unsigned_type_node);
  r1 = irange (UINT16 (0), UINT16 (20));
  r2 = irange (UINT16 (0xff88), UINT16 (0xffff));
  r1.union_ (r2);
  ASSERT_TRUE (r0 == r1);
  // A truncating cast back to signed char will work because [-120, 20]
  // is representable in signed char.
  r0.cast (signed_char_type_node);
  ASSERT_TRUE (r0 == irange (SCHAR (-120), SCHAR (20)));

  // unsigned char -> signed short
  //	(signed short)[(unsigned char)25, (unsigned char)250]
  // => [(signed short)25, (signed short)250]
  r0 = rold = irange (UCHAR (25), UCHAR (250));
  r0.cast (short_integer_type_node);
  r1 = irange (INT16 (25), INT16 (250));
  ASSERT_TRUE (r0 == r1);
  r0.cast (unsigned_char_type_node);
  ASSERT_TRUE (r0 == rold);

  // Test casting a wider signed [-MIN,MAX] to a nar`rower unsigned.
  r0 = irange (TYPE_MIN_VALUE (long_long_integer_type_node),
	       TYPE_MAX_VALUE (long_long_integer_type_node));
  r0.cast (short_unsigned_type_node);
  r1 = irange (TYPE_MIN_VALUE (short_unsigned_type_node),
	       TYPE_MAX_VALUE (short_unsigned_type_node));
  ASSERT_TRUE (r0 == r1);

  // Test that casting a range with MAX_PAIRS that changes sign is
  // done conservatively.
  //
  //    (unsigned short)[-5,5][20,30][40,50]...
  // => (unsigned short)[-5,50]
  // => [0,50][65531,65535]
  r0 = irange (INT16 (-5), INT16 (5));
  gcc_assert (irange::m_max_pairs * 2 * 10 + 10 < 32767);
  unsigned i;
  for (i = 2; i < irange::m_max_pairs * 2; i += 2)
    {
      r1 = irange (INT16 (i * 10), INT16 (i * 10 + 10));
      r0.union_ (r1);
    }
  r0.cast(short_unsigned_type_node);
  r1 = irange (UINT16 (0), UINT16 ((i - 2) * 10 + 10));
  r2 = irange (UINT16 (65531), UINT16 (65535));
  r1.union_ (r2);
  ASSERT_TRUE (r0 == r1);

  // NOT([10,20]) ==> [-MIN,9][21,MAX].
  r0 = r1 = irange (INT (10), INT (20));
  r2 = irange (minint, INT(9));
  r2.union_ (irange (INT(21), maxint));
  ASSERT_FALSE (r2.undefined_p ());
  r1.invert ();
  ASSERT_TRUE (r1 == r2);
  // Test that NOT(NOT(x)) == x.
  r2.invert ();
  ASSERT_TRUE (r0 == r2);

  // NOT(-MIN,+MAX) is the empty set and should return false.
  r0 = irange (minint, maxint);
  r0.invert ();
  ASSERT_TRUE (r0.undefined_p ());
  r1.set_undefined ();
  ASSERT_TRUE (r0 == r1);

  // Test that booleans and their inverse work as expected.
  r0 = range_zero (boolean_type_node);
  ASSERT_TRUE (r0 == irange (build_zero_cst (boolean_type_node),
			     build_zero_cst (boolean_type_node)));
  r0.invert();
  ASSERT_TRUE (r0 == irange (build_one_cst (boolean_type_node),
			     build_one_cst (boolean_type_node)));

  // Casting NONZERO to a narrower type will wrap/overflow so
  // it's just the entire range for the narrower type.
  //
  // "NOT 0 at signed 32-bits" ==> [-MIN_32,-1][1, +MAX_32].  This is
  // is outside of the range of a smaller range, return the full
  // smaller range.
  r0 = range_nonzero (integer_type_node);
  r0.cast (short_integer_type_node);
  r1 = irange (TYPE_MIN_VALUE (short_integer_type_node),
	       TYPE_MAX_VALUE (short_integer_type_node));
  ASSERT_TRUE (r0 == r1);

  // Casting NONZERO from a narrower signed to a wider signed.
  //
  // NONZERO signed 16-bits is [-MIN_16,-1][1, +MAX_16].
  // Converting this to 32-bits signed is [-MIN_16,-1][1, +MAX_16].
  r0 = range_nonzero (short_integer_type_node);
  r0.cast (integer_type_node);
  r1 = irange (INT (-32768), INT (-1));
  r2 = irange (INT (1), INT (32767));
  r1.union_ (r2);
  ASSERT_TRUE (r0 == r1);

  if (irange::m_max_pairs > 2)
    {
      // ([10,20] U [5,8]) U [1,3] ==> [1,3][5,8][10,20].
      r0 = irange (INT (10), INT (20));
      r1 = irange (INT (5), INT (8));
      r0.union_ (r1);
      r1 = irange (INT (1), INT (3));
      r0.union_ (r1);
      ASSERT_TRUE (r0 == RANGE3 (1, 3, 5, 8, 10, 20));

      // [1,3][5,8][10,20] U [-5,0] => [-5,3][5,8][10,20].
      r1 = irange (INT (-5), INT (0));
      r0.union_ (r1);
      ASSERT_TRUE (r0 == RANGE3 (-5, 3, 5, 8, 10, 20));
    }

  // [10,20] U [30,40] ==> [10,20][30,40].
  r0 = irange (INT (10), INT (20));
  r1 = irange (INT (30), INT (40));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == range_union (irange (INT (10), INT (20)),
				   irange (INT (30), INT (40))));
  if (irange::m_max_pairs > 2)
    {
      // [10,20][30,40] U [50,60] ==> [10,20][30,40][50,60].
      r1 = irange (INT (50), INT (60));
      r0.union_ (r1);
      ASSERT_TRUE (r0 == RANGE3 (10, 20, 30, 40, 50, 60));
      // [10,20][30,40][50,60] U [70, 80] ==> [10,20][30,40][50,60][70,80].
      r1 = irange (INT (70), INT (80));
      r0.union_ (r1);

      r2 = RANGE3 (10, 20, 30, 40, 50, 60);
      r2.union_ (irange (INT (70), INT (80)));
      ASSERT_TRUE (r0 == r2);
    }

  // Make sure NULL and non-NULL of pointer types work, and that
  // inverses of them are consistent.
  tree voidp = build_pointer_type (void_type_node);
  r0 = range_zero (voidp);
  r1 = r0;
  r0.invert ();
  r0.invert ();
  ASSERT_TRUE (r0 == r1);

  if (irange::m_max_pairs > 2)
    {
      // [10,20][30,40][50,60] U [6,35] => [6,40][50,60].
      r0 = RANGE3 (10, 20, 30, 40, 50, 60);
      r1 = irange (INT (6), INT (35));
      r0.union_ (r1);
      ASSERT_TRUE (r0 == range_union (irange (INT (6), INT (40)),
				      irange (INT (50), INT (60))));

      // [10,20][30,40][50,60] U [6,60] => [6,60] */
      r0 = RANGE3 (10, 20, 30, 40, 50, 60);
      r1 = irange (INT (6), INT (60));
      r0.union_ (r1);
      ASSERT_TRUE (r0 == irange (INT (6), INT (60)));

      // [10,20][30,40][50,60] U [6,70] => [6,70].
      r0 = RANGE3 (10, 20, 30, 40, 50, 60);
      r1 = irange (INT (6), INT (70));
      r0.union_ (r1);
      ASSERT_TRUE (r0 == irange (INT (6), INT (70)));

      // [10,20][30,40][50,60] U [35,70] => [10,20][30,70].
      r0 = RANGE3 (10, 20, 30, 40, 50, 60);
      r1 = irange (INT (35), INT (70));
      r0.union_ (r1);
      ASSERT_TRUE (r0 == range_union (irange (INT (10), INT (20)),
				       irange (INT (30), INT (70))));
    }

  // [10,20][30,40] U [25,70] => [10,70].
  r0 = range_union (irange (INT (10), INT (20)),
		     irange (INT (30), INT (40)));
  r1 = irange (INT (25), INT (70));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == range_union (irange (INT (10), INT (20)),
				   irange (INT (25), INT (70))));

  if (irange::m_max_pairs > 2)
    {
      // [10,20][30,40][50,60] U [15,35] => [10,40][50,60].
      r0 = RANGE3 (10, 20, 30, 40, 50, 60);
      r1 = irange (INT (15), INT (35));
      r0.union_ (r1);
      ASSERT_TRUE (r0 == range_union (irange (INT (10), INT (40)),
				       irange (INT (50), INT (60))));
    }

  // [10,20] U [15, 30] => [10, 30].
  r0 = irange (INT (10), INT (20));
  r1 = irange (INT (15), INT (30));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == irange (INT (10), INT (30)));

  // [10,20] U [25,25] => [10,20][25,25].
  r0 = irange (INT (10), INT (20));
  r1 = irange (INT (25), INT (25));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == range_union (irange (INT (10), INT (20)),
				   irange (INT (25), INT (25))));

  if (irange::m_max_pairs > 2)
    {
      // [10,20][30,40][50,60] U [35,35] => [10,20][30,40][50,60].
      r0 = RANGE3 (10, 20, 30, 40, 50, 60);
      r1 = irange (INT (35), INT (35));
      r0.union_ (r1);
      ASSERT_TRUE (r0 == RANGE3 (10, 20, 30, 40, 50, 60));
    }

  // [15,40] U [] => [15,40].
  r0 = irange (INT (15), INT (40));
  r1.set_undefined ();
  r0.union_ (r1);
  ASSERT_TRUE (r0 == irange (INT (15), INT (40)));

  // [10,20] U [10,10] => [10,20].
  r0 = irange (INT (10), INT (20));
  r1 = irange (INT (10), INT (10));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == irange (INT (10), INT (20)));

  // [10,20] U [9,9] => [9,20].
  r0 = irange (INT (10), INT (20));
  r1 = irange (INT (9), INT (9));
  r0.union_ (r1);
  ASSERT_TRUE (r0 == irange (INT (9), INT (20)));

  if (irange::m_max_pairs > 2)
    {
      // [10,10][12,12][20,100] ^ [15,200].
      r0 = RANGE3 (10, 10, 12, 12, 20, 100);
      r1 = irange (INT (15), INT (200));
      r0.intersect (r1);
      ASSERT_TRUE (r0 == irange (INT (20), INT (100)));

      // [10,20][30,40][50,60] ^ [15,25][38,51][55,70]
      // => [15,20][38,40][50,51][55,60]
      r0 = RANGE3 (10, 20, 30, 40, 50, 60);
      r1 = RANGE3 (15, 25, 38, 51, 55, 70);
      r0.intersect (r1);
      if (irange::m_max_pairs == 3)
	{
	  // When pairs==3, we don't have enough space, so
	  //  conservatively handle things.  Thus, the ...[50,60].
	  ASSERT_TRUE (r0 == RANGE3 (15, 20, 38, 40, 50, 60));
	}
      else
	{
	  r2 = RANGE3 (15, 20, 38, 40, 50, 51);
	  r2.union_ (irange (INT (55), INT (60)));
	  ASSERT_TRUE (r0 == r2);
	}

      // [15,20][30,40][50,60] ^ [15,35][40,90][100,200]
      // => [15,20][30,35][40,60]
      r0 = RANGE3 (15, 20, 30, 40, 50, 60);
      r1 = RANGE3 (15, 35, 40, 90, 100, 200);
      r0.intersect (r1);
      if (irange::m_max_pairs == 3)
	{
	  // When pairs==3, we don't have enough space, so
	  // conservatively handle things.
	  ASSERT_TRUE (r0 == RANGE3 (15, 20, 30, 35, 40, 60));
	}
      else
	{
	  r2 = RANGE3 (15, 20, 30, 35, 40, 40);
	  r2.union_ (irange (INT (50), INT (60)));
	  ASSERT_TRUE (r0 == r2);
	}

      // Test cases where a union inserts a sub-range inside a larger
      // range.
      //
      // [8,10][135,255] U [14,14] => [8,10][14,14][135,255]
      r0 = range_union (irange (INT (8), INT (10)),
			 irange (INT (135), INT (255)));
      r1 = irange (INT (14), INT (14));
      r0.union_ (r1);
      ASSERT_TRUE (r0 == RANGE3 (8, 10, 14, 14, 135, 255));
    }

  // [10,20] ^ [15,30] => [15,20].
  r0 = irange (INT (10), INT (20));
  r1 = irange (INT (15), INT (30));
  r0.intersect (r1);
  ASSERT_TRUE (r0 == irange (INT (15), INT (20)));

  // [10,20][30,40] ^ [40,50] => [40,40].
  r0 = range_union (irange (INT (10), INT (20)),
		     irange (INT (30), INT (40)));
  r1 = irange (INT (40), INT (50));
  r0.intersect (r1);
  ASSERT_TRUE (r0 == irange (INT (40), INT (40)));

  // Test non-destructive intersection.
  r0 = rold = irange (INT (10), INT (20));
  ASSERT_FALSE (range_intersect (r0, irange (INT (15),
					     INT (30))).undefined_p ());
  ASSERT_TRUE (r0 == rold);

  // Test the internal sanity of wide_int's wrt HWIs.
  ASSERT_TRUE (wi::max_value (TYPE_PRECISION (boolean_type_node),
			      TYPE_SIGN (boolean_type_node))
	       == wi::uhwi (1, TYPE_PRECISION (boolean_type_node)));

  // Test irange_storage.
  r0 = irange (INT (5), INT (10));
  irange_storage *stow = irange_storage::alloc (r0);
  r1 = irange (integer_type_node, stow);
  ASSERT_TRUE (r0 == r1);

  // Test irange_storage with signed 1-bit fields.
  tree s1bit_type = make_signed_type (1);
  r0 = irange (build_int_cst (s1bit_type, -1), build_int_cst (s1bit_type, 0));
  stow = irange_storage::alloc (r0);
  r1 = irange (s1bit_type, stow);
  ASSERT_TRUE (r0 == r1);

  // Test zero_p().
  r0 = irange (INT (0), INT (0));
  ASSERT_TRUE (r0.zero_p ());

  // Test nonzero_p().
  r0 = irange (INT (0), INT (0));
  r0.invert ();
  ASSERT_TRUE (r0.nonzero_p ());

  // Test irange / value_range conversion functions.
  r0 = irange (IRANGE_INVERSE, INT (10), INT (20));
  value_range_base vr = irange_to_value_range (r0);
  ASSERT_TRUE (vr.kind () == VR_ANTI_RANGE);
  ASSERT_TRUE (wi::eq_p (10, wi::to_wide (vr.min ()))
	       && wi::eq_p (20, wi::to_wide (vr.max ())));
  r1 = value_range_to_irange (integer_type_node, vr);
  ASSERT_TRUE (r0 == r1);
}

#endif // CHECKING_P
