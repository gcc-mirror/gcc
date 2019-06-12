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

// This is a class for working with ranges, currently integer ones.
// With it you can specify a range of [5,10] or even ranges including
// multi-part ranges [-10,5][30,40][50,60].
//
// Inverse ranges are represented as an actual range.  For instance,
// the inverse of 0 is [-MIN,-1][1,+MAX] for a signed integer.
//
// Methods are provided for intersecting and uniting ranges, as well
// as converting between them.  In performing any of these operations,
// when no efficient way can be computed, we may default to a more
// conservative range.
//
// For example, the inverse of [5,10][15,20][30,40] is actually
// [-MIN,4][11,14][21,29][41,+MAX].  If this cannot be efficiently or
// quickly computed, we may opt to represent the inverse as
// [-MIN,4][41,+MAX] which is an equivalent conservative
// representation.
//
// This class is not meant to live in long term storage.
// Consequently, there are no GTY markers.  For long term storage, use
// the irange_storage class described later.

enum irange_kind { IRANGE_PLAIN, IRANGE_INVERSE };

class irange
{
  friend class irange_storage;
  friend void irange_tests ();

 public:
  irange ();
  irange (tree type);
  irange (irange_kind, tree type, const wide_int &, const wide_int &);
  irange (tree type, const wide_int &, const wide_int &);
  irange (irange_kind, tree, tree);
  irange (tree, tree);
  irange (tree type, const irange_storage *);

  static bool supports_type_p (tree type);
  static bool supports_ssa_p (tree ssa);
  static bool supports_p (tree expr);

  void set_varying (tree);
  void set_undefined (tree = NULL);

  unsigned num_pairs () const;
  wide_int lower_bound (unsigned pair = 0) const;
  wide_int upper_bound () const;
  wide_int upper_bound (unsigned pair) const;

  tree type () const { return m_type; }

  void cast (tree type);

  bool varying_p () const;
  bool undefined_p () const;
  bool zero_p () const;
  bool nonzero_p () const;
  // These two take a tree instead of a wide_int, for API
  // compatibility with value_range.
  bool singleton_p (tree * = NULL) const;
  bool contains_p (tree) const;

  bool operator== (const irange &r) const;
  bool operator!= (const irange &r) const;

  void union_ (const irange &r);
  void intersect (const irange &r);
  void invert ();

  void dump () const;
  void dump (FILE *) const;

private:
  void init (tree type, const wide_int &, const wide_int &,
	     irange_kind = IRANGE_PLAIN);
  void canonicalize ();
  void set_lower_bound (unsigned pair, const wide_int &);
  void set_upper_bound (unsigned pair, const wide_int &);
  void remove_pair (unsigned pair);
  void intersect (const wide_int &x, const wide_int &y);
  bool contains_p (const wide_int &element) const;
  void dump (pretty_printer *pp) const;
  bool valid_p () const;

  tree m_type;
  unsigned char m_nitems;
  static const unsigned int m_max_pairs = 3;
  wide_int m_bounds[m_max_pairs * 2];
}; // class irange

irange range_zero (tree type);
irange range_nonzero (tree type);
irange range_intersect (const irange &, const irange &);
irange range_union (const irange &, const irange &);
irange range_invert (const irange &);
irange range_from_ssa (tree ssa);
irange range_positives (tree type);
irange range_negatives (tree type);
irange value_range_to_irange (tree type, const value_range_base &);
irange value_range_to_irange (tree type, enum value_range_kind kind,
			      const wide_int &, const wide_int &);
value_range_base irange_to_value_range (const irange &);

// Extract a range from a tree node.
bool get_tree_range (irange &r, tree expr);

inline
irange::irange () : m_type (NULL), m_nitems (0)
{
}

inline wide_int
irange::lower_bound (unsigned pair) const
{
  return m_bounds[pair * 2];
}

inline wide_int
irange::upper_bound () const
{
  return m_bounds[m_nitems - 1];
}

inline wide_int
irange::upper_bound (unsigned pair) const
{
  return m_bounds[pair * 2 + 1];
}

inline void
irange::set_lower_bound (unsigned pair, const wide_int &i)
{
  m_bounds[pair * 2 ] = i;
}

inline void
irange::set_upper_bound (unsigned pair, const wide_int &i)
{
  m_bounds[pair * 2 + 1] = i;
}

inline unsigned
irange::num_pairs () const
{
  return m_nitems / 2;
}

inline void
irange::set_undefined (tree type)
{
  if (type)
    m_type = type;
  m_nitems = 0;
}

inline bool
irange::undefined_p () const
{
  return !m_nitems;
}

// Return true if TYPE is a valid type for irange to operate on.
// Otherwise return FALSE.

inline bool
irange::supports_type_p (tree type)
{
  if (type && (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)))
    return type;
  return NULL;
}

// Return true if SSA is a valid ssa_name for irange to operate on.
// Otherwise return FALSE.

inline bool
irange::supports_ssa_p (tree ssa)
{
  if (!SSA_NAME_IS_VIRTUAL_OPERAND (ssa))
    return supports_type_p (TREE_TYPE (ssa));
 return false;
}

// Return true if EXPR is a valid tree expression for irange to operate on.
// Otherwise return FALSE.

inline bool
irange::supports_p (tree expr)
{
  if (TYPE_P (expr))
    return supports_type_p (expr);
  else if (TREE_CODE (expr) == SSA_NAME)
    return supports_ssa_p (expr);
  return supports_type_p (TREE_TYPE (expr));
}

inline bool
irange::zero_p () const
{
  wide_int z = wi::zero (TYPE_PRECISION (m_type));
  return (m_nitems == 2 && m_bounds[0] == z && m_bounds[1] == z);
}

inline bool
irange::nonzero_p () const
{
  unsigned prec = TYPE_PRECISION (m_type);
  return *this == irange (IRANGE_INVERSE, m_type,
			  wi::zero (prec), wi::zero (prec));
}

// Return true if this range is the full range for its type.

inline bool
irange::varying_p () const
{
  return (m_nitems == 2 &&
	  m_bounds[0] == wi::min_value (TYPE_PRECISION (m_type),
					TYPE_SIGN (m_type)) &&
	  m_bounds[1] == wi::max_value (TYPE_PRECISION (m_type),
					TYPE_SIGN (m_type)));
}

// An irange is memory inefficient, so this class is used to store
// them in memory.  It is a variable length structure that contains
// the sub-range pairs as well as the non-zero bitmask.  The number of
// entries are m_max_pairs * 2 + 1 (to accomodate the non-zero bits).
//
// To store an irange class X into an irange_storage use:
//
// 	irange X = ...;
// 	irange_storage *stow = irange_storage::alloc (X);
//
// To convert it back into an irange use:
//
// 	tree type = ...;
// 	irange X (type, stow);
//
// To get at the nonzero bits:
//
// 	irange_storage *stow = ...;
// 	stow->set_nonzero_bits();
// 	stow->get_nonzero_bits();

class GTY((variable_size)) irange_storage
{
  friend class irange;

 public:
  static irange_storage *alloc (const irange &);
  bool update (const irange &);

 private:
  static size_t size (unsigned precision);
  void set (const irange &);
  bool empty_pair_p (unsigned, unsigned, tree) const;
  void set_empty_pair (unsigned, unsigned, tree);
  void set_nonzero_bits (const wide_int &);
  wide_int get_nonzero_bits ();

  // The last wide_int in this field is a mask representing which bits in
  // an integer are known to be non-zero.
  trailing_wide_ints<irange::m_max_pairs * 2 + 1> trailing_bounds;
};

// Return the nonzero bits in the range.

inline wide_int
irange_storage::get_nonzero_bits ()
{
  return trailing_bounds[irange::m_max_pairs * 2];
}

// Set the nonzero bit mask to WI.
inline void
irange_storage::set_nonzero_bits (const wide_int &wi)
{
  trailing_bounds[irange::m_max_pairs * 2] = wi;
}

// Returns the size of an irange_storage with PRECISION.

inline size_t
irange_storage::size (unsigned precision)
{
  return sizeof (irange_storage)
    /* There is a +1 for the non-zero bits field.  */
    + trailing_wide_ints<irange::m_max_pairs * 2 + 1>::extra_size (precision);
}

// Allocate GC memory for an irange_storage with PRECISION and
// initialize it to IR.

inline irange_storage *
irange_storage::alloc (const irange &ir)
{
  unsigned precision = TYPE_PRECISION (ir.m_type);
  irange_storage *stow = static_cast<irange_storage *> (ggc_internal_alloc
							(size (precision)));
  stow->set (ir);
  stow->set_nonzero_bits (wi::shwi (-1, precision));
  return stow;
}

// Return TRUE if pair [i, j] is marked as empty.

inline bool
irange_storage::empty_pair_p (unsigned i, unsigned j, tree type) const
{
  unsigned precision = wi::get_precision (trailing_bounds[0]);
  if (precision == 1 && TYPE_SIGN (type) == SIGNED)
    return (trailing_bounds[i] == wi::zero (precision)
	    && trailing_bounds[j] == wi::one (precision));
  return (trailing_bounds[i] == wi::one (precision)
	  && trailing_bounds[j] == wi::zero (precision));
}

#endif // GCC_RANGE_H
