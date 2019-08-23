/* High resolution range class.
   Copyright (C) 2017-2019 Free Software Foundation, Inc.
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
#include "wide-int-range.h"

// Common code between the alternate irange implementations.

// Return TRUE if two types are compatible for range operations.

static bool
range_compatible_p (tree t1, tree t2)
{
  if (POINTER_TYPE_P (t1) && POINTER_TYPE_P (t2))
    return true;

  return types_compatible_p (t1, t2);
}

bool
irange::operator== (const irange &r) const
{
  // Special case this because a freshly initialized range may be
  // typeless.
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
  return irange (VR_ANTI_RANGE,
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
    r.set_undefined ();
  else
    r = irange (type, wi::min_value (prec, sign), wi::minus_one (prec));
  return r;
}

#if USE_IRANGE
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
	      value_range_kind rt)
{
  if (rt == VR_UNDEFINED)
    {
      set_undefined ();
      return;
    }
  if (rt == VR_VARYING)
    {
      set_varying (type);
      return;
    }
  gcc_checking_assert (irange::supports_type_p (type));
  gcc_checking_assert (TYPE_PRECISION (type) == lbound.get_precision ());
  gcc_checking_assert (lbound.get_precision () == ubound.get_precision ());
  m_type = type;
  gcc_checking_assert (wi::le_p (lbound, ubound, TYPE_SIGN (type)));
  if (rt == VR_ANTI_RANGE)
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

irange::irange (value_range_kind rt, tree type,
		const wide_int &lbound, const wide_int &ubound)
{
  init (type, lbound, ubound, rt);
}

irange::irange (tree type, const wide_int &lbound, const wide_int &ubound)
{
  init (type, lbound, ubound, VR_RANGE);
}

irange::irange (value_range_kind rt, tree lbound, tree ubound)
{
  gcc_checking_assert (rt == VR_RANGE || rt == VR_ANTI_RANGE);
  tree type = TREE_TYPE (lbound);
  init (type, wi::to_wide (lbound), wi::to_wide (ubound), rt);
}

irange::irange (tree lbound, tree ubound)
{
  tree type = TREE_TYPE (lbound);
  init (type, wi::to_wide (lbound), wi::to_wide (ubound), VR_RANGE);
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
  if (undefined_p ())
    {
      *this = r;
      return;
    }
  else if (r.undefined_p ())
    return;

  gcc_checking_assert (range_compatible_p (m_type, r.m_type));

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
  if (undefined_p ())
    return;

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
  irange orig_range (*this);

  // Intersection with an empty range is an empty range.
  if (orig_range.undefined_p () || r.undefined_p ())
    {
      set_undefined ();
      return;
    }

  gcc_checking_assert (range_compatible_p (m_type, r.m_type));

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
  set_undefined ();
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
  if (undefined_p ())
    return;

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
  if (undefined_p ())
    {
      pp_string (buffer, "[]");
      pp_flush (buffer);
      return;
    }

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
irange_storage::set (const irange &ir, tree type)
{
  if (type)
    gcc_checking_assert (ir.undefined_p ()
			 || types_compatible_p (type, ir.type ()));
  else
    type = ir.type ();
  unsigned precision = TYPE_PRECISION (type);
  trailing_bounds.set_precision (precision);
  unsigned i;
  for (i = 0; i < ir.num_pairs () * 2; ++i)
    trailing_bounds[i] = ir.m_bounds[i];

  // Clear the remaining empty ranges.
  for (; i < irange::m_max_pairs * 2; i += 2)
    set_empty_pair (i, i + 1, type);
}

// Update a previously initialized irange_storage to NEW_RANGE, iff the
// precision of the present range is the same as the precision of
// the new range.  Return TRUE if update was successful.

bool
irange_storage::update (const irange &new_range, tree type)
{
  if (trailing_bounds.get_precision () == TYPE_PRECISION (type))
    {
      set (new_range, type);
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

// Convert irange to a value_range.

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
      vr.set_undefined ();
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

// Convert a value_range into an irange.

static irange
value_range_to_irange (const value_range_base &vr)
{
  if (vr.varying_p ())
    return irange (vr.type ());
  if (vr.undefined_p ())
    {
      irange r;
      r.set_undefined ();
      return r;
    }
  return irange (vr.kind (), vr.min (), vr.max ());
}

irange::irange (const value_range_base &vr)
{
  *this = value_range_to_irange (vr);
}

#endif // USE_IRANGE
