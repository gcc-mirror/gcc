/* Header file for range analysis.
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

#ifndef GCC_RANGE_H
#define GCC_RANGE_H

class irange_storage;

/* This is a class for working with ranges, currently integer ones.
   With it you can specify a range of [5,10] (5 through 10 inclusive),
   or even ranges including multi-part ranges [-10,5][30,40][50,60].
   This last one specifies the union of the different sub-ranges.

   Inverse ranges are represented as an actual range.  For instance,
   the inverse of 0 is [-MIN,-1][1,+MAX] for a signed integer.

   Methods are provided for intersecting and uniting ranges, as well
   as converting between them.  In performing any of these operations,
   when no efficient way can be computed, we may default to a more
   conservative range.

   For example, the inverse of [5,10][15,20][30,40] is actually
   [-MIN,4][11,14][21,29][41,+MAX].  If this cannot be efficiently or
   quickly computed, we may opt to represent the inverse as
   [-MIN,4][41,+MAX] which is an equivalent conservative
   representation.

   This class is not meant to live in long term storage (GC).
   Consequently, there are no GTY markers.  For long term storage, use
   the irange_storage class described later.  */
class irange
{
  friend class irange_storage;
 public:
  /* Maximum number of pairs of ranges allowed.  */
  static const unsigned int max_pairs = 3;

 private:
  /* Number of items in bounds[].  */
  unsigned char nitems;
  /* Whether or not a set operation overflowed.  */
  bool overflow;
  /* The type of the range.  */
  tree type;
  /* The pairs of sub-ranges in the range.  */
  wide_int bounds[max_pairs * 2];

  void insert (const wide_int &x, const wide_int &y, unsigned pos);
  void prepend (const wide_int &x, const wide_int &y);
  void append (const wide_int &x, const wide_int &y);
  void remove (unsigned i, unsigned j);
  void canonicalize ();

 public:
  /* When constructing a range, this specifies wether this is a
     regular range, or the inverse of a range.  */
  enum kind { PLAIN, INVERSE };
  irange () { type = NULL_TREE; nitems = 0; }
  explicit irange (tree t) { set_range (t); }
  irange (tree typ, const wide_int &lbound, const wide_int &ubound,
	  kind rt = PLAIN)
    { set_range (typ, lbound, ubound, rt); }
  irange (tree typ, tree lbound, tree ubound,
	  kind rt = PLAIN)
    { set_range (typ, wi::to_wide (lbound), wi::to_wide (ubound), rt); }
  irange (const irange &);
  irange (const irange_storage *stor, tree typ) { set_range (stor, typ); }
  irange (tree t, int x, int y, kind k = PLAIN)
						    { set_range (t, x, y, k); }

  void set_range (const irange_storage *, tree);
  void set_range (tree);
  void set_range (tree, const wide_int &lbound, const wide_int &ubound,
		  kind rt = PLAIN);
  void set_range (tree typ, tree lbound, tree ubound,
		  kind rt = PLAIN)
    { set_range (typ, wi::to_wide (lbound), wi::to_wide (ubound), rt);  }
  void set_range (tree t, int x, int y, kind rt = PLAIN);
  void set_range_for_type (tree);

  bool overflow_p () const { return overflow && !TYPE_OVERFLOW_WRAPS (type); }
  void set_overflow () { overflow = true; }
  void clear_overflow () { overflow = false; }

  unsigned num_pairs () const { return nitems / 2; }
  /* Returns the lower bound of PAIR.  */
  wide_int lower_bound (unsigned pair = 0) const
    {
      gcc_assert (nitems != 0 && pair <= num_pairs ());
      return bounds[pair * 2];
    }
  /* Returns the uppermost bound.  */
  wide_int upper_bound () const
    {
      gcc_assert (nitems != 0);
      return bounds[nitems - 1];
    }
  wide_int upper_bound (unsigned pair) const;

  /* Remove a sub-range from a range.  PAIR is the zero-based
     sub-range to remove.  */
  void remove_pair (unsigned pair) { remove (pair * 2, pair * 2 + 1); }
  void clear () { nitems = 0; }
  void clear (tree t) { type = t; nitems = 0; overflow = false; }
  bool empty_p () const { return !nitems; }
  bool range_for_type_p () const;
  bool simple_range_p () const { return nitems == 2; }
  bool zero_p () const { return *this == irange (type, 0, 0); }
  bool non_zero_p () const
    {
      irange nz;
      nz.set_range (type, 0, 0, INVERSE);
      return *this == nz;
    }
  inline bool singleton_p (wide_int &) const;

  void dump () const;
  void dump (pretty_printer *pp) const;
  void dump (FILE *) const;

  bool valid_p () const;
  void cast (tree type);
  bool contains_p (const wide_int &element) const;
  bool contains_p (tree) const;
  bool contains_p (int) const;

  tree get_type () const { return type; }

  void intersect_mask (const wide_int& mask);

  irange& operator= (const irange &r);
  irange& operator= (tree t);

  bool operator== (const irange &r) const;
  bool operator!= (const irange &r) const { return !(*this == r); }

  irange &union_ (const wide_int &x, const wide_int &y);
  irange &union_ (const irange &r);
  irange &intersect (const wide_int &x, const wide_int &y);
  irange &intersect (const irange &r);
  irange &invert ();
};

/* Return TRUE if range contains exactly one element.  If so, set ELEM
   to said element.  */

inline bool
irange::singleton_p (wide_int &elem) const
{
  if (num_pairs () == 1 && bounds[0] == bounds[1])
    {
      elem = bounds[0];
      return true;
    }
  return false;
}

/* Return R1 U R2.  */
static inline
irange irange_union (const irange &r1, const irange &r2)
{
  return irange (r1).union_ (r2);
}

/* Return R1 ^ R2.  */
static inline
irange irange_intersect (const irange &r1, const irange &r2)
{
  return irange (r1).intersect (r2);
}

/* Return the inverse range of R1.  */
static inline
irange irange_invert (const irange &r1)
{
  return irange (r1).invert ();
}

void range_zero (irange *r, tree type);
void range_one (irange *r, tree type);
bool range_non_zero (irange *r, tree type);
static inline void
range_positives (irange *r, tree type)
{
  r->set_range (type, build_int_cst (type, 0), TYPE_MAX_VALUE (type));
}
static inline void
range_negatives (irange *r, tree type)
{
  r->set_range (type, TYPE_MIN_VALUE (type), build_int_cst (type, -1));
}
enum value_range_type irange_to_value_range (wide_int &, wide_int &);

/* An irange is inefficient when it comes to memory, so this class is
   used to store iranges in memory (off of an SSA_NAME likely).  It is
   a variable length structure that contains the sub-range pairs as
   well as the non-zero bitmask.  The number of entries are
   irnage::max_pairs * 2 + 1 (to accomodate the non-zero bits).

   To store an irange class X into this memory efficient irange_storage
   class use:

	irange X;
	irange_storage *stow = irange_storage::ggc_alloc_init (X);
   or
	irange_storage *stow = irange_storage::ggc_alloc (precision);
	stow->set_irange (X);

   To convert it back to an irange use:

	tree type = ...;
	irange X (stow, type);
   or
	if (SSA_NAME_RANGE_INFO (ssa)) {
	  irange X (ssa);
	  ...
	}
   or
	irange x;
	stow->extract_irange (x, TYPE);

   To get at the nonzero bits use:

	irange_storage *stow = ...;
	stow->set_nonzero_bits();
	stow->get_nonzero_bits();
*/

class GTY((variable_size)) irange_storage
{
  friend class irange;
 public:
  /* These are the pair of subranges for the irange.  The last
     wide_int allocated is a mask representing which bits in an
     integer are known to be non-zero.  */
  trailing_wide_ints<irange::max_pairs * 2 + 1> trailing_bounds;

  void set_irange (const irange &);
  /* Returns the size of an irange_storage with PRECISION.  */
  static size_t size (unsigned precision)
  { return sizeof (irange_storage)
      /* There is a +1 for the non-zero bits field.  */
      + trailing_wide_ints<irange::max_pairs * 2 + 1>::extra_size (precision);
  }
  /* Allocate GC memory for an irange_storage with PRECISION.

     Note: The precision is set, but the irange_storage returned is
     otherwise uninitialized.  The caller must still call
     stow->set_irange().  */
  static irange_storage *ggc_alloc (unsigned precision)
  { irange_storage *stow = static_cast<irange_storage *> (ggc_internal_alloc
							  (size (precision)));
    stow->trailing_bounds.set_precision (precision);
    stow->set_nonzero_bits (wi::shwi (-1, precision));
    return stow;
  }
  /* Like irange_storage::ggc_alloc (), but initialize the storage to
     the range in IR.  */
  static irange_storage *ggc_alloc_init (const irange &ir)
  {
    unsigned precision = TYPE_PRECISION (ir.type);
    irange_storage *stow = static_cast<irange_storage *> (ggc_internal_alloc
							  (size (precision)));
    stow->set_irange (ir);
    stow->set_nonzero_bits (wi::shwi (-1, precision));
    return stow;
  }
  /* Extract the current range onto OUTPUT with a type of TYP.
     Returns the range.  */
  inline irange &extract_irange (irange &output, tree typ);
  /* Set the nonzero bit mask to WI.  */
  void set_nonzero_bits (const wide_int &wi)
  { trailing_bounds[irange::max_pairs * 2] = wi; }
  /* Return the nonzero bits in the range.  */
  wide_int get_nonzero_bits (void)
  { return trailing_bounds[irange::max_pairs * 2]; }
};

/* Extract the range in THIS and store it in OUTPUT with a type of TYP.
   Returns OUTPUT.  */

inline irange &
irange_storage::extract_irange (irange &output, tree typ)
{
  output.set_range (this, typ);
  return output;
}

// ----------------------------------------------------------------------

/* Return T if it is a valid type for irange to operator on. 
   Otherwise return NULL_TREE.  */
static inline
tree valid_irange_type (tree t)
{
  if (t && (INTEGRAL_TYPE_P (t) || POINTER_TYPE_P (t)))
    return t;
  return NULL_TREE;
}

/* Return T if it is an SSA_NAME and a valid type for irange to operator on. 
   Otherwise return NULL_TREE.  */
static inline
tree valid_irange_ssa (tree t)
{
  if (t && TREE_CODE (t) == SSA_NAME && !SSA_NAME_IS_VIRTUAL_OPERAND (t)
      && valid_irange_type (TREE_TYPE (t)))
    return t;
  return NULL_TREE;
}

/* Convert a value_range to an irange and store it in R.
   TYPE is the SSA type.
   KIND, MIN, and MAX are as in a value_range.  */

static inline void
value_range_to_irange (irange &r,
		       tree type, enum value_range_type kind,
		       const wide_int &min, const wide_int &max)
{
  gcc_assert (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type));
  if (kind == VR_VARYING || POINTER_TYPE_P (type))
    r.set_range_for_type (type);
  else
    r.set_range (type, min, max,
		 kind == VR_ANTI_RANGE ? irange::INVERSE : irange::PLAIN);
}

#endif // GCC_RANGE_H
