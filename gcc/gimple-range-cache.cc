/* Gimple ranger SSA cache implementation.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.
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
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "gimple-range.h"
#include "value-range-storage.h"
#include "tree-cfg.h"
#include "target.h"
#include "attribs.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "cfganal.h"

#define DEBUG_RANGE_CACHE (dump_file					\
			   && (param_ranger_debug & RANGER_DEBUG_CACHE))

// This class represents the API into a cache of ranges for an SSA_NAME.
// Routines must be implemented to set, get, and query if a value is set.

class ssa_block_ranges
{
public:
  ssa_block_ranges (tree t) : m_type (t) { }
  virtual bool set_bb_range (const_basic_block bb, const vrange &r) = 0;
  virtual bool get_bb_range (vrange &r, const_basic_block bb) = 0;
  virtual bool bb_range_p (const_basic_block bb) = 0;

  void dump(FILE *f);
private:
  tree m_type;
};

// Print the list of known ranges for file F in a nice format.

void
ssa_block_ranges::dump (FILE *f)
{
  basic_block bb;
  value_range r (m_type);

  FOR_EACH_BB_FN (bb, cfun)
    if (get_bb_range (r, bb))
      {
	fprintf (f, "BB%d  -> ", bb->index);
	r.dump (f);
	fprintf (f, "\n");
      }
}

// This class implements the range cache as a linear vector, indexed by BB.
// It caches a varying and undefined range which are used instead of
// allocating new ones each time.

class sbr_vector : public ssa_block_ranges
{
public:
  sbr_vector (tree t, vrange_allocator *allocator, bool zero_p = true);

  virtual bool set_bb_range (const_basic_block bb, const vrange &r) override;
  virtual bool get_bb_range (vrange &r, const_basic_block bb) override;
  virtual bool bb_range_p (const_basic_block bb) override;
protected:
  vrange_storage **m_tab;	// Non growing vector.
  int m_tab_size;
  vrange_storage *m_varying;
  vrange_storage *m_undefined;
  tree m_type;
  vrange_allocator *m_range_allocator;
  bool m_zero_p;
  void grow ();
};


// Initialize a block cache for an ssa_name of type T.

sbr_vector::sbr_vector (tree t, vrange_allocator *allocator, bool zero_p)
  : ssa_block_ranges (t)
{
  gcc_checking_assert (TYPE_P (t));
  m_type = t;
  m_zero_p = zero_p;
  m_range_allocator = allocator;
  m_tab_size = last_basic_block_for_fn (cfun) + 1;
  m_tab = static_cast <vrange_storage **>
    (allocator->alloc (m_tab_size * sizeof (vrange_storage *)));
  if (zero_p)
    memset (m_tab, 0, m_tab_size * sizeof (vrange *));

  // Create the cached type range.
  m_varying = m_range_allocator->clone_varying (t);
  m_undefined = m_range_allocator->clone_undefined (t);
}

// Grow the vector when the CFG has increased in size.

void
sbr_vector::grow ()
{
  int curr_bb_size = last_basic_block_for_fn (cfun);
  gcc_checking_assert (curr_bb_size > m_tab_size);

  // Increase the max of a)128, b)needed increase * 2, c)10% of current_size.
  int inc = MAX ((curr_bb_size - m_tab_size) * 2, 128);
  inc = MAX (inc, curr_bb_size / 10);
  int new_size = inc + curr_bb_size;

  // Allocate new memory, copy the old vector and clear the new space.
  vrange_storage **t = static_cast <vrange_storage **>
    (m_range_allocator->alloc (new_size * sizeof (vrange_storage *)));
  memcpy (t, m_tab, m_tab_size * sizeof (vrange_storage *));
  if (m_zero_p)
    memset (t + m_tab_size, 0, (new_size - m_tab_size) * sizeof (vrange_storage *));

  m_tab = t;
  m_tab_size = new_size;
}

// Set the range for block BB to be R.

bool
sbr_vector::set_bb_range (const_basic_block bb, const vrange &r)
{
  vrange_storage *m;
  if (bb->index >= m_tab_size)
    grow ();
  if (r.varying_p ())
    m = m_varying;
  else if (r.undefined_p ())
    m = m_undefined;
  else
    m = m_range_allocator->clone (r);
  m_tab[bb->index] = m;
  return true;
}

// Return the range associated with block BB in R.  Return false if
// there is no range.

bool
sbr_vector::get_bb_range (vrange &r, const_basic_block bb)
{
  if (bb->index >= m_tab_size)
    return false;
  vrange_storage *m = m_tab[bb->index];
  if (m)
    {
      m->get_vrange (r, m_type);
      return true;
    }
  return false;
}

// Return true if a range is present.

bool
sbr_vector::bb_range_p (const_basic_block bb)
{
  if (bb->index < m_tab_size)
    return m_tab[bb->index] != NULL;
  return false;
}

// Like an sbr_vector, except it uses a bitmap to manage whetehr  vale is set
// or not rather than cleared memory.

class sbr_lazy_vector : public sbr_vector
{
public:
  sbr_lazy_vector (tree t, vrange_allocator *allocator, bitmap_obstack *bm);

  virtual bool set_bb_range (const_basic_block bb, const vrange &r) override;
  virtual bool get_bb_range (vrange &r, const_basic_block bb) override;
  virtual bool bb_range_p (const_basic_block bb) override;
protected:
  bitmap m_has_value;
};

sbr_lazy_vector::sbr_lazy_vector (tree t, vrange_allocator *allocator,
				  bitmap_obstack *bm)
  : sbr_vector (t, allocator, false)
{
  m_has_value = BITMAP_ALLOC (bm);
}

bool
sbr_lazy_vector::set_bb_range (const_basic_block bb, const vrange &r)
{
  sbr_vector::set_bb_range (bb, r);
  bitmap_set_bit (m_has_value, bb->index);
  return true;
}

bool
sbr_lazy_vector::get_bb_range (vrange &r, const_basic_block bb)
{
  if (bitmap_bit_p (m_has_value, bb->index))
    return sbr_vector::get_bb_range (r, bb);
  return false;
}

bool
sbr_lazy_vector::bb_range_p (const_basic_block bb)
{
  return bitmap_bit_p (m_has_value, bb->index);
}

// This class implements the on entry cache via a sparse bitmap.
// It uses the quad bit routines to access 4 bits at a time.
// A value of 0 (the default) means there is no entry, and a value of
// 1 thru SBR_NUM represents an element in the m_range vector.
// Varying is given the first value (1) and pre-cached.
// SBR_NUM + 1 represents the value of UNDEFINED, and is never stored.
// SBR_NUM is the number of values that can be cached.
// Indexes are 1..SBR_NUM and are stored locally at m_range[0..SBR_NUM-1]

#define SBR_NUM		14
#define SBR_UNDEF	SBR_NUM + 1
#define SBR_VARYING	1

class sbr_sparse_bitmap : public ssa_block_ranges
{
public:
  sbr_sparse_bitmap (tree t, vrange_allocator *allocator, bitmap_obstack *bm);
  virtual bool set_bb_range (const_basic_block bb, const vrange &r) override;
  virtual bool get_bb_range (vrange &r, const_basic_block bb) override;
  virtual bool bb_range_p (const_basic_block bb) override;
private:
  void bitmap_set_quad (bitmap head, int quad, int quad_value);
  int bitmap_get_quad (const_bitmap head, int quad);
  vrange_allocator *m_range_allocator;
  vrange_storage *m_range[SBR_NUM];
  bitmap_head bitvec;
  tree m_type;
};

// Initialize a block cache for an ssa_name of type T.

sbr_sparse_bitmap::sbr_sparse_bitmap (tree t, vrange_allocator *allocator,
				      bitmap_obstack *bm)
  : ssa_block_ranges (t)
{
  gcc_checking_assert (TYPE_P (t));
  m_type = t;
  bitmap_initialize (&bitvec, bm);
  bitmap_tree_view (&bitvec);
  m_range_allocator = allocator;
  // Pre-cache varying.
  m_range[0] = m_range_allocator->clone_varying (t);
  // Pre-cache zero and non-zero values for pointers.
  if (POINTER_TYPE_P (t))
    {
      prange nonzero;
      nonzero.set_nonzero (t);
      m_range[1] = m_range_allocator->clone (nonzero);
      prange zero;
      zero.set_zero (t);
      m_range[2] = m_range_allocator->clone (zero);
    }
  else
    m_range[1] = m_range[2] = NULL;
  // Clear SBR_NUM entries.
  for (int x = 3; x < SBR_NUM; x++)
    m_range[x] = 0;
}

// Set 4 bit values in a sparse bitmap. This allows a bitmap to
// function as a sparse array of 4 bit values.
// QUAD is the index, QUAD_VALUE is the 4 bit value to set.

inline void
sbr_sparse_bitmap::bitmap_set_quad (bitmap head, int quad, int quad_value)
{
  bitmap_set_aligned_chunk (head, quad, 4, (BITMAP_WORD) quad_value);
}

// Get a 4 bit value from a sparse bitmap. This allows a bitmap to
// function as a sparse array of 4 bit values.
// QUAD is the index.
inline int
sbr_sparse_bitmap::bitmap_get_quad (const_bitmap head, int quad)
{
  return (int) bitmap_get_aligned_chunk (head, quad, 4);
}

// Set the range on entry to basic block BB to R.

bool
sbr_sparse_bitmap::set_bb_range (const_basic_block bb, const vrange &r)
{
  if (r.undefined_p ())
    {
      bitmap_set_quad (&bitvec, bb->index, SBR_UNDEF);
      return true;
    }

  // Loop thru the values to see if R is already present.
  for (int x = 0; x < SBR_NUM; x++)
    if (!m_range[x] || m_range[x]->equal_p (r))
      {
	if (!m_range[x])
	  m_range[x] = m_range_allocator->clone (r);
	bitmap_set_quad (&bitvec, bb->index, x + 1);
	return true;
      }
  // All values are taken, default to VARYING.
  bitmap_set_quad (&bitvec, bb->index, SBR_VARYING);
  return false;
}

// Return the range associated with block BB in R.  Return false if
// there is no range.

bool
sbr_sparse_bitmap::get_bb_range (vrange &r, const_basic_block bb)
{
  int value = bitmap_get_quad (&bitvec, bb->index);

  if (!value)
    return false;

  gcc_checking_assert (value <= SBR_UNDEF);
  if (value == SBR_UNDEF)
    r.set_undefined ();
  else
    m_range[value - 1]->get_vrange (r, m_type);
  return true;
}

// Return true if a range is present.

bool
sbr_sparse_bitmap::bb_range_p (const_basic_block bb)
{
  return (bitmap_get_quad (&bitvec, bb->index) != 0);
}

// -------------------------------------------------------------------------

// Initialize the block cache.

block_range_cache::block_range_cache ()
{
  bitmap_obstack_initialize (&m_bitmaps);
  m_ssa_ranges.create (0);
  m_ssa_ranges.safe_grow_cleared (num_ssa_names);
  m_range_allocator = new vrange_allocator;
}

// Remove any m_block_caches which have been created.

block_range_cache::~block_range_cache ()
{
  delete m_range_allocator;
  // Release the vector itself.
  m_ssa_ranges.release ();
  bitmap_obstack_release (&m_bitmaps);
}

// Set the range for NAME on entry to block BB to R.
// If it has not been accessed yet, allocate it first.

bool
block_range_cache::set_bb_range (tree name, const_basic_block bb,
				 const vrange &r)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_ssa_ranges.length ())
    m_ssa_ranges.safe_grow_cleared (num_ssa_names);

  if (!m_ssa_ranges[v])
    {
      // Use sparse bitmap representation if there are too many basic blocks.
      if (last_basic_block_for_fn (cfun) > param_vrp_sparse_threshold)
	{
	  void *r = m_range_allocator->alloc (sizeof (sbr_sparse_bitmap));
	  m_ssa_ranges[v] = new (r) sbr_sparse_bitmap (TREE_TYPE (name),
						       m_range_allocator,
						       &m_bitmaps);
	}
      else if (last_basic_block_for_fn (cfun) < param_vrp_vector_threshold)
	{
	  // For small CFGs use the basic vector implemntation.
	  void *r = m_range_allocator->alloc (sizeof (sbr_vector));
	  m_ssa_ranges[v] = new (r) sbr_vector (TREE_TYPE (name),
						m_range_allocator);
	}
      else
	{
	  // Otherwise use the sparse vector implementation.
	  void *r = m_range_allocator->alloc (sizeof (sbr_lazy_vector));
	  m_ssa_ranges[v] = new (r) sbr_lazy_vector (TREE_TYPE (name),
						     m_range_allocator,
						     &m_bitmaps);
	}
    }
  return m_ssa_ranges[v]->set_bb_range (bb, r);
}


// Return a pointer to the ssa_block_cache for NAME.  If it has not been
// accessed yet, return NULL.

inline ssa_block_ranges *
block_range_cache::query_block_ranges (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_ssa_ranges.length () || !m_ssa_ranges[v])
    return NULL;
  return m_ssa_ranges[v];
}



// Return the range for NAME on entry to BB in R.  Return true if there
// is one.

bool
block_range_cache::get_bb_range (vrange &r, tree name, const_basic_block bb)
{
  ssa_block_ranges *ptr = query_block_ranges (name);
  if (ptr)
    return ptr->get_bb_range (r, bb);
  return false;
}

// Return true if NAME has a range set in block BB.

bool
block_range_cache::bb_range_p (tree name, const_basic_block bb)
{
  ssa_block_ranges *ptr = query_block_ranges (name);
  if (ptr)
    return ptr->bb_range_p (bb);
  return false;
}

// Print all known block caches to file F.

void
block_range_cache::dump (FILE *f)
{
  unsigned x;
  for (x = 1; x < m_ssa_ranges.length (); ++x)
    {
      if (m_ssa_ranges[x])
	{
	  fprintf (f, " Ranges for ");
	  print_generic_expr (f, ssa_name (x), TDF_NONE);
	  fprintf (f, ":\n");
	  m_ssa_ranges[x]->dump (f);
	  fprintf (f, "\n");
	}
    }
}

// Print all known ranges on entry to block BB to file F.

void
block_range_cache::dump (FILE *f, basic_block bb, bool print_varying)
{
  unsigned x;
  bool summarize_varying = false;
  for (x = 1; x < m_ssa_ranges.length (); ++x)
    {
      if (!m_ssa_ranges[x])
	continue;

      if (!gimple_range_ssa_p (ssa_name (x)))
	continue;

      value_range r (TREE_TYPE (ssa_name (x)));
      if (m_ssa_ranges[x]->get_bb_range (r, bb))
	{
	  if (!print_varying && r.varying_p ())
	    {
	      summarize_varying = true;
	      continue;
	    }
	  print_generic_expr (f, ssa_name (x), TDF_NONE);
	  fprintf (f, "\t");
	  r.dump(f);
	  fprintf (f, "\n");
	}
    }
  // If there were any varying entries, lump them all together.
  if (summarize_varying)
    {
      fprintf (f, "VARYING_P on entry : ");
      for (x = 1; x < m_ssa_ranges.length (); ++x)
	{
	  if (!m_ssa_ranges[x])
	    continue;

	  if (!gimple_range_ssa_p (ssa_name (x)))
	    continue;

	  value_range r (TREE_TYPE (ssa_name (x)));
	  if (m_ssa_ranges[x]->get_bb_range (r, bb))
	    {
	      if (r.varying_p ())
		{
		  print_generic_expr (f, ssa_name (x), TDF_NONE);
		  fprintf (f, "  ");
		}
	    }
	}
      fprintf (f, "\n");
    }
}

// -------------------------------------------------------------------------

// Initialize an ssa cache.

ssa_cache::ssa_cache ()
{
  m_tab.create (0);
  m_range_allocator = new vrange_allocator;
}

// Deconstruct an ssa cache.

ssa_cache::~ssa_cache ()
{
  m_tab.release ();
  delete m_range_allocator;
}

// Enable a query to evaluate staements/ramnges based on picking up ranges
// from just an ssa-cache.

bool
ssa_cache::range_of_expr (vrange &r, tree expr, gimple *stmt)
{
  if (!gimple_range_ssa_p (expr))
    return get_tree_range (r, expr, stmt);

  if (!get_range (r, expr))
    gimple_range_global (r, expr, cfun);
  return true;
}

// Return TRUE if the global range of NAME has a cache entry.

bool
ssa_cache::has_range (tree name) const
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    return false;
  return m_tab[v] != NULL;
}

// Retrieve the global range of NAME from cache memory if it exists.
// Return the value in R.

bool
ssa_cache::get_range (vrange &r, tree name) const
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    return false;

  vrange_storage *stow = m_tab[v];
  if (!stow)
    return false;
  stow->get_vrange (r, TREE_TYPE (name));
  return true;
}

// Set the range for NAME to R in the ssa cache.
// Return TRUE if there was already a range set, otherwise false.

bool
ssa_cache::set_range (tree name, const vrange &r)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    m_tab.safe_grow_cleared (num_ssa_names + 1);

  vrange_storage *m = m_tab[v];
  if (m && m->fits_p (r))
    m->set_vrange (r);
  else
    m_tab[v] = m_range_allocator->clone (r);
  return m != NULL;
}

// If NAME has a range, intersect it with R, otherwise set it to R.
// Return TRUE if the range is new or changes.

bool
ssa_cache::merge_range (tree name, const vrange &r)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    m_tab.safe_grow_cleared (num_ssa_names + 1);

  vrange_storage *m = m_tab[v];
  // Check if this is a new value.
  if (!m)
    m_tab[v] = m_range_allocator->clone (r);
  else
    {
      value_range curr (TREE_TYPE (name));
      m->get_vrange (curr, TREE_TYPE (name));
      // If there is no change, return false.
      if (!curr.intersect (r))
	return false;

      if (m->fits_p (curr))
	m->set_vrange (curr);
      else
	m_tab[v] = m_range_allocator->clone (curr);
    }
  return true;
}

// Set the range for NAME to R in the ssa cache.

void
ssa_cache::clear_range (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    return;
  m_tab[v] = NULL;
}

// Clear the ssa cache.

void
ssa_cache::clear ()
{
  if (m_tab.address ())
    memset (m_tab.address(), 0, m_tab.length () * sizeof (vrange *));
}

// Dump the contents of the ssa cache to F.

void
ssa_cache::dump (FILE *f)
{
  for (unsigned x = 1; x < num_ssa_names; x++)
    {
      if (!gimple_range_ssa_p (ssa_name (x)))
	continue;
      value_range r (TREE_TYPE (ssa_name (x)));
      // Dump all non-varying ranges.
      if (get_range (r, ssa_name (x)) && !r.varying_p ())
	{
	  print_generic_expr (f, ssa_name (x), TDF_NONE);
	  fprintf (f, "  : ");
	  r.dump (f);
	  fprintf (f, "\n");
	}
    }

}

// Construct an ssa_lazy_cache. If OB is specified, us it, otherwise use
// a local bitmap obstack.

ssa_lazy_cache::ssa_lazy_cache (bitmap_obstack *ob)
{
  if (!ob)
    {
      bitmap_obstack_initialize (&m_bitmaps);
      m_ob = &m_bitmaps;
    }
  else
    m_ob = ob;
  active_p = BITMAP_ALLOC (m_ob);
}

// Destruct an sa_lazy_cache.  Free the bitmap if it came from a different
// obstack, or release the obstack if it was a local one.

ssa_lazy_cache::~ssa_lazy_cache ()
{
  if (m_ob == &m_bitmaps)
    bitmap_obstack_release (&m_bitmaps);
  else
    BITMAP_FREE (active_p);
}

// Return true if NAME has an active range in the cache.

bool
ssa_lazy_cache::has_range (tree name) const
{
  return bitmap_bit_p (active_p, SSA_NAME_VERSION (name));
}

// Set range of NAME to R in a lazy cache.  Return FALSE if it did not already
// have a range.

bool
ssa_lazy_cache::set_range (tree name, const vrange &r)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!bitmap_set_bit (active_p, v))
    {
      // There is already an entry, simply set it.
      gcc_checking_assert (v < m_tab.length ());
      return ssa_cache::set_range (name, r);
    }
  if (v >= m_tab.length ())
    m_tab.safe_grow (num_ssa_names + 1);
  m_tab[v] = m_range_allocator->clone (r);
  return false;
}

// If NAME has a range, intersect it with R, otherwise set it to R.
// Return TRUE if the range is new or changes.

bool
ssa_lazy_cache::merge_range (tree name, const vrange &r)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (!bitmap_set_bit (active_p, v))
    {
      // There is already an entry, simply merge it.
      gcc_checking_assert (v < m_tab.length ());
      return ssa_cache::merge_range (name, r);
    }
  if (v >= m_tab.length ())
    m_tab.safe_grow (num_ssa_names + 1);
  m_tab[v] = m_range_allocator->clone (r);
  return true;
}

// Merge all elements of CACHE with this cache.
// Any names in CACHE that are not in this one are added.
// Any names in both are merged via merge_range..

void
ssa_lazy_cache::merge (const ssa_lazy_cache &cache)
{
  unsigned x;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (cache.active_p, 0, x, bi)
    {
      tree name = ssa_name (x);
      value_range r(TREE_TYPE (name));
      cache.get_range (r, name);
      merge_range (ssa_name (x), r);
    }
}

// Return TRUE if NAME has a range, and return it in R.

bool
ssa_lazy_cache::get_range (vrange &r, tree name) const
{
  if (!bitmap_bit_p (active_p, SSA_NAME_VERSION (name)))
    return false;
  return ssa_cache::get_range (r, name);
}

// Remove NAME from the active range list.

void
ssa_lazy_cache::clear_range (tree name)
{
  bitmap_clear_bit (active_p, SSA_NAME_VERSION (name));
}

// Remove all ranges from the active range list.

void
ssa_lazy_cache::clear ()
{
  bitmap_clear (active_p);
}

// --------------------------------------------------------------------------


// This class will manage the timestamps for each ssa_name.
// When a value is calculated, the timestamp is set to the current time.
// Current time is then incremented.  Any dependencies will already have
// been calculated, and will thus have older timestamps.
// If one of those values is ever calculated again, it will get a newer
// timestamp, and the "current_p" check will fail.

class temporal_cache
{
public:
  temporal_cache ();
  ~temporal_cache ();
  bool current_p (tree name, tree dep1, tree dep2) const;
  void set_timestamp (tree name);
  void set_always_current (tree name, bool value);
  bool always_current_p (tree name) const;
private:
  int temporal_value (unsigned ssa) const;
  int m_current_time;
  vec <int> m_timestamp;
};

inline
temporal_cache::temporal_cache ()
{
  m_current_time = 1;
  m_timestamp.create (0);
  m_timestamp.safe_grow_cleared (num_ssa_names);
}

inline
temporal_cache::~temporal_cache ()
{
  m_timestamp.release ();
}

// Return the timestamp value for SSA, or 0 if there isn't one.

inline int
temporal_cache::temporal_value (unsigned ssa) const
{
  if (ssa >= m_timestamp.length ())
    return 0;
  return abs (m_timestamp[ssa]);
}

// Return TRUE if the timestamp for NAME is newer than any of its dependents.
// Up to 2 dependencies can be checked.

bool
temporal_cache::current_p (tree name, tree dep1, tree dep2) const
{
  if (always_current_p (name))
    return true;

  // Any non-registered dependencies will have a value of 0 and thus be older.
  // Return true if time is newer than either dependent.
  int ts = temporal_value (SSA_NAME_VERSION (name));
  if (dep1 && ts < temporal_value (SSA_NAME_VERSION (dep1)))
    return false;
  if (dep2 && ts < temporal_value (SSA_NAME_VERSION (dep2)))
    return false;

  return true;
}

// This increments the global timer and sets the timestamp for NAME.

inline void
temporal_cache::set_timestamp (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_timestamp.length ())
    m_timestamp.safe_grow_cleared (num_ssa_names + 20);
  m_timestamp[v] = ++m_current_time;
}

// Set the timestamp to 0, marking it as "always up to date".

inline void
temporal_cache::set_always_current (tree name, bool value)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_timestamp.length ())
    m_timestamp.safe_grow_cleared (num_ssa_names + 20);

  int ts = abs (m_timestamp[v]);
  // If this does not have a timestamp, create one.
  if (ts == 0)
    ts = ++m_current_time;
  m_timestamp[v] = value ? -ts : ts;
}

// Return true if NAME is always current.

inline bool
temporal_cache::always_current_p (tree name) const
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_timestamp.length ())
    return false;
  return m_timestamp[v] <= 0;
}

// --------------------------------------------------------------------------

// This class provides an abstraction of a list of blocks to be updated
// by the cache.  It is currently a stack but could be changed.  It also
// maintains a list of blocks which have failed propagation, and does not
// enter any of those blocks into the list.

// A vector over the BBs is maintained, and an entry of 0 means it is not in
// a list.  Otherwise, the entry is the next block in the list. -1 terminates
// the list.  m_head points to the top of the list, -1 if the list is empty.

class update_list
{
public:
  update_list ();
  ~update_list ();
  void add (basic_block bb);
  basic_block pop ();
  inline bool empty_p () { return m_update_head == -1; }
  inline void clear_failures () { bitmap_clear (m_propfail); }
  inline void propagation_failed (basic_block bb)
				  { bitmap_set_bit (m_propfail, bb->index); }
private:
  vec<int> m_update_list;
  int m_update_head;
  bitmap m_propfail;
  bitmap_obstack m_bitmaps;
};

// Create an update list.

update_list::update_list ()
{
  m_update_list.create (0);
  m_update_list.safe_grow_cleared (last_basic_block_for_fn (cfun) + 64);
  m_update_head = -1;
  bitmap_obstack_initialize (&m_bitmaps);
  m_propfail = BITMAP_ALLOC (&m_bitmaps);
}

// Destroy an update list.

update_list::~update_list ()
{
  m_update_list.release ();
  bitmap_obstack_release (&m_bitmaps);
}

// Add BB to the list of blocks to update, unless it's already in the list.

void
update_list::add (basic_block bb)
{
  int i = bb->index;
  // If propagation has failed for BB, or its already in the list, don't
  // add it again.
  if ((unsigned)i >= m_update_list.length ())
    m_update_list.safe_grow_cleared (i + 64);
  if (!m_update_list[i] && !bitmap_bit_p (m_propfail, i))
    {
      if (empty_p ())
	{
	  m_update_head = i;
	  m_update_list[i] = -1;
	}
      else
	{
	  gcc_checking_assert (m_update_head > 0);
	  m_update_list[i] = m_update_head;
	  m_update_head = i;
	}
    }
}

// Remove a block from the list.

basic_block
update_list::pop ()
{
  gcc_checking_assert (!empty_p ());
  basic_block bb = BASIC_BLOCK_FOR_FN (cfun, m_update_head);
  int pop = m_update_head;
  m_update_head = m_update_list[pop];
  m_update_list[pop] = 0;
  return bb;
}

// --------------------------------------------------------------------------

ranger_cache::ranger_cache (int not_executable_flag, bool use_imm_uses)
{
  m_workback = vNULL;
  m_temporal = new temporal_cache;

  // If DOM info is available, spawn an oracle as well.
  create_relation_oracle ();
  create_infer_oracle (use_imm_uses);
  create_gori (not_executable_flag, param_vrp_switch_limit);

  unsigned x, lim = last_basic_block_for_fn (cfun);
  // Calculate outgoing range info upfront.  This will fully populate the
  // m_maybe_variant bitmap which will help eliminate processing of names
  // which never have their ranges adjusted.
  for (x = 0; x < lim ; x++)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, x);
      if (bb)
	gori_ssa ()->exports (bb);
    }
  m_update = new update_list ();
}

ranger_cache::~ranger_cache ()
{
  delete m_update;
  destroy_infer_oracle ();
  destroy_relation_oracle ();
  delete m_temporal;
  m_workback.release ();
}

// Dump the global caches to file F.  if GORI_DUMP is true, dump the
// gori map as well.

void
ranger_cache::dump (FILE *f)
{
  fprintf (f, "Non-varying global ranges:\n");
  fprintf (f, "=========================:\n");
  m_globals.dump (f);
  fprintf (f, "\n");
}

// Dump the caches for basic block BB to file F.

void
ranger_cache::dump_bb (FILE *f, basic_block bb)
{
  gori_ssa ()->dump (f, bb, false);
  m_on_entry.dump (f, bb);
  m_relation->dump (f, bb);
}

// Get the global range for NAME, and return in R.  Return false if the
// global range is not set, and return the legacy global value in R.

bool
ranger_cache::get_global_range (vrange &r, tree name) const
{
  if (m_globals.get_range (r, name))
    return true;
  gimple_range_global (r, name);
  return false;
}

// Get the global range for NAME, and return in R.  Return false if the
// global range is not set, and R will contain the legacy global value.
// CURRENT_P is set to true if the value was in cache and not stale.
// Otherwise, set CURRENT_P to false and mark as it always current.
// If the global cache did not have a value, initialize it as well.
// After this call, the global cache will have a value.

bool
ranger_cache::get_global_range (vrange &r, tree name, bool &current_p)
{
  bool had_global = get_global_range (r, name);

  // If there was a global value, set current flag, otherwise set a value.
  current_p = false;
  if (had_global)
    current_p = r.singleton_p ()
		|| m_temporal->current_p (name, gori_ssa ()->depend1 (name),
					  gori_ssa ()->depend2 (name));
  else
    {
      // If no global value has been set and value is VARYING, fold the stmt
      // using just global ranges to get a better initial value.
      // After inlining we tend to decide some things are constant, so
      // so not do this evaluation after inlining.
      if (r.varying_p () && !cfun->after_inlining)
	{
	  gimple *s = SSA_NAME_DEF_STMT (name);
	  // Do not process PHIs as SCEV may be in use and it can
	  // spawn cyclic lookups.
	  if (gimple_get_lhs (s) == name && !is_a<gphi *> (s))
	    {
	      if (!fold_range (r, s, get_global_range_query ()))
		gimple_range_global (r, name);
	    }
	}
      m_globals.set_range (name, r);
    }

  // If the existing value was not current, mark it as always current.
  if (!current_p)
    m_temporal->set_always_current (name, true);
  return had_global;
}

//  Set the global range of NAME to R and give it a timestamp.

void
ranger_cache::set_global_range (tree name, const vrange &r, bool changed)
{
  // Setting a range always clears the always_current flag.
  m_temporal->set_always_current (name, false);
  if (!changed)
    {
      // If there are dependencies, make sure this is not out of date.
      if (!m_temporal->current_p (name, gori_ssa ()->depend1 (name),
				 gori_ssa ()->depend2 (name)))
	m_temporal->set_timestamp (name);
      return;
    }
  if (m_globals.set_range (name, r))
    {
      // If there was already a range set, propagate the new value.
      basic_block bb = gimple_bb (SSA_NAME_DEF_STMT (name));
      if (!bb)
	bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

      if (DEBUG_RANGE_CACHE)
	fprintf (dump_file, "   GLOBAL :");

      propagate_updated_value (name, bb);
    }
  // Constants no longer need to tracked.  Any further refinement has to be
  // undefined. Propagation works better with constants. PR 100512.
  // Pointers which resolve to non-zero also do not need
  // tracking in the cache as they will never change.  See PR 98866.
  // Timestamp must always be updated, or dependent calculations may
  // not include this latest value. PR 100774.

  if (r.singleton_p ()
      || (POINTER_TYPE_P (TREE_TYPE (name)) && r.nonzero_p ()))
    gori_ssa ()->set_range_invariant (name);
  m_temporal->set_timestamp (name);
}

//  Provide lookup for the gori-computes class to access the best known range
//  of an ssa_name in any given basic block.  Note, this does no additional
//  lookups, just accesses the data that is already known.

// Get the range of NAME when the def occurs in block BB.  If BB is NULL
// get the best global value available.

void
ranger_cache::range_of_def (vrange &r, tree name, basic_block bb)
{
  gcc_checking_assert (gimple_range_ssa_p (name));
  gcc_checking_assert (!bb || bb == gimple_bb (SSA_NAME_DEF_STMT (name)));

  // Pick up the best global range available.
  if (!m_globals.get_range (r, name))
    {
      // If that fails, try to calculate the range using just global values.
      gimple *s = SSA_NAME_DEF_STMT (name);
      if (gimple_get_lhs (s) == name)
	fold_range (r, s, get_global_range_query ());
      else
	gimple_range_global (r, name);
    }
}

// Get the range of NAME as it occurs on entry to block BB.  Use MODE for
// lookups.

void
ranger_cache::entry_range (vrange &r, tree name, basic_block bb,
			   enum rfd_mode mode)
{
  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
    {
      gimple_range_global (r, name);
      return;
    }

  // Look for the on-entry value of name in BB from the cache.
  // Otherwise pick up the best available global value.
  if (!m_on_entry.get_bb_range (r, name, bb))
    if (!range_from_dom (r, name, bb, mode))
      range_of_def (r, name);
}

// Get the range of NAME as it occurs on exit from block BB.  Use MODE for
// lookups.

void
ranger_cache::exit_range (vrange &r, tree name, basic_block bb,
			  enum rfd_mode mode)
{
  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
    {
      gimple_range_global (r, name);
      return;
    }

  gimple *s = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = gimple_bb (s);
  if (def_bb == bb)
    range_of_def (r, name, bb);
  else
    entry_range (r, name, bb, mode);
}

// Get the range of NAME on edge E using MODE, return the result in R.
// Always returns a range and true.

bool
ranger_cache::edge_range (vrange &r, edge e, tree name, enum rfd_mode mode)
{
  exit_range (r, name, e->src, mode);
  // If this is not an abnormal edge, check for inferred ranges on exit.
  if ((e->flags & (EDGE_EH | EDGE_ABNORMAL)) == 0)
    infer_oracle ().maybe_adjust_range (r, name, e->src);
  value_range er (TREE_TYPE (name));
  if (gori ().edge_range_p (er, e, name, *this))
    r.intersect (er);
  return true;
}



// Implement range_of_expr.

bool
ranger_cache::range_of_expr (vrange &r, tree name, gimple *stmt)
{
  if (!gimple_range_ssa_p (name))
    {
      get_tree_range (r, name, stmt);
      return true;
    }

  basic_block bb = gimple_bb (stmt);
  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = gimple_bb (def_stmt);

  if (bb == def_bb)
    range_of_def (r, name, bb);
  else
    entry_range (r, name, bb, RFD_NONE);
  return true;
}


// Implement range_on_edge.  Always return the best available range using
// the current cache values.

bool
ranger_cache::range_on_edge (vrange &r, edge e, tree expr)
{
  if (gimple_range_ssa_p (expr))
    return edge_range (r, e, expr, RFD_NONE);
  return get_tree_range (r, expr, NULL);
}

// Return a static range for NAME on entry to basic block BB in R.  If
// calc is true, fill any cache entries required between BB and the
// def block for NAME.  Otherwise, return false if the cache is empty.

bool
ranger_cache::block_range (vrange &r, basic_block bb, tree name, bool calc)
{
  gcc_checking_assert (gimple_range_ssa_p (name));

  // If there are no range calculations anywhere in the IL, global range
  // applies everywhere, so don't bother caching it.
  if (!gori ().has_edge_range_p (name))
    return false;

  if (calc)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      basic_block def_bb = NULL;
      if (def_stmt)
	def_bb = gimple_bb (def_stmt);
      if (!def_bb)
	{
	  // If we get to the entry block, this better be a default def
	  // or range_on_entry was called for a block not dominated by
	  // the def.  But it could be also SSA_NAME defined by a statement
	  // not yet in the IL (such as queued edge insertion), in that case
	  // just punt.
	  if (!SSA_NAME_IS_DEFAULT_DEF (name))
	    return false;
	  def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
	}

      // There is no range on entry for the definition block.
      if (def_bb == bb)
	return false;

      // Otherwise, go figure out what is known in predecessor blocks.
      fill_block_cache (name, bb, def_bb);
      gcc_checking_assert (m_on_entry.bb_range_p (name, bb));
    }
  return m_on_entry.get_bb_range (r, name, bb);
}

// If there is anything in the propagation update_list, continue
// processing NAME until the list of blocks is empty.

void
ranger_cache::propagate_cache (tree name)
{
  basic_block bb;
  edge_iterator ei;
  edge e;
  tree type = TREE_TYPE (name);
  value_range new_range (type);
  value_range current_range (type);
  value_range e_range (type);

  // Process each block by seeing if its calculated range on entry is
  // the same as its cached value. If there is a difference, update
  // the cache to reflect the new value, and check to see if any
  // successors have cache entries which may need to be checked for
  // updates.

  while (!m_update->empty_p ())
    {
      bb = m_update->pop ();
      gcc_checking_assert (m_on_entry.bb_range_p (name, bb));
      m_on_entry.get_bb_range (current_range, name, bb);

      if (DEBUG_RANGE_CACHE)
	{
	  fprintf (dump_file, "FWD visiting block %d for ", bb->index);
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fprintf (dump_file, "  starting range : ");
	  current_range.dump (dump_file);
	  fprintf (dump_file, "\n");
	}

      // Calculate the "new" range on entry by unioning the pred edges.
      new_range.set_undefined ();
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  edge_range (e_range, e, name, RFD_READ_ONLY);
	  if (DEBUG_RANGE_CACHE)
	    {
	      fprintf (dump_file, "   edge %d->%d :", e->src->index, bb->index);
	      e_range.dump (dump_file);
	      fprintf (dump_file, "\n");
	    }
	  new_range.union_ (e_range);
	  if (new_range.varying_p ())
	    break;
	}

      // If the range on entry has changed, update it.
      if (new_range != current_range)
	{
	  bool ok_p = m_on_entry.set_bb_range (name, bb, new_range);
	  // If the cache couldn't set the value, mark it as failed.
	  if (!ok_p)
	    m_update->propagation_failed (bb);
	  if (DEBUG_RANGE_CACHE)
	    {
	      if (!ok_p)
		{
		  fprintf (dump_file, "   Cache failure to store value:");
		  print_generic_expr (dump_file, name, TDF_SLIM);
		  fprintf (dump_file, "  ");
		}
	      else
		{
		  fprintf (dump_file, "      Updating range to ");
		  new_range.dump (dump_file);
		}
	      fprintf (dump_file, "\n      Updating blocks :");
	    }
	  // Mark each successor that has a range to re-check its range
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (m_on_entry.bb_range_p (name, e->dest))
	      {
		if (DEBUG_RANGE_CACHE)
		  fprintf (dump_file, " bb%d",e->dest->index);
		m_update->add (e->dest);
	      }
	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "\n");
	}
    }
  if (DEBUG_RANGE_CACHE)
    {
      fprintf (dump_file, "DONE visiting blocks for ");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, "\n");
    }
  m_update->clear_failures ();
}

// Check to see if an update to the value for NAME in BB has any effect
// on values already in the on-entry cache for successor blocks.
// If it does, update them.  Don't visit any blocks which don't have a cache
// entry.

void
ranger_cache::propagate_updated_value (tree name, basic_block bb)
{
  edge e;
  edge_iterator ei;

  // The update work list should be empty at this point.
  gcc_checking_assert (m_update->empty_p ());
  gcc_checking_assert (bb);

  if (DEBUG_RANGE_CACHE)
    {
      fprintf (dump_file, " UPDATE cache for ");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, " in BB %d : successors : ", bb->index);
    }
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      // Only update active cache entries.
      if (m_on_entry.bb_range_p (name, e->dest))
	{
	  m_update->add (e->dest);
	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, " UPDATE: bb%d", e->dest->index);
	}
    }
    if (!m_update->empty_p ())
      {
	if (DEBUG_RANGE_CACHE)
	  fprintf (dump_file, "\n");
	propagate_cache (name);
      }
    else
      {
	if (DEBUG_RANGE_CACHE)
	  fprintf (dump_file, "  : No updates!\n");
      }
}

// Make sure that the range-on-entry cache for NAME is set for block BB.
// Work back through the CFG to DEF_BB ensuring the range is calculated
// on the block/edges leading back to that point.

void
ranger_cache::fill_block_cache (tree name, basic_block bb, basic_block def_bb)
{
  edge_iterator ei;
  edge e;
  tree type = TREE_TYPE (name);
  value_range block_result (type);
  value_range undefined (type);

  // At this point we shouldn't be looking at the def, entry block.
  gcc_checking_assert (bb != def_bb && bb != ENTRY_BLOCK_PTR_FOR_FN (cfun));
  unsigned start_length = m_workback.length ();

  // If the block cache is set, then we've already visited this block.
  if (m_on_entry.bb_range_p (name, bb))
    return;

  if (DEBUG_RANGE_CACHE)
    {
      fprintf (dump_file, "\n");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, " : ");
    }

  // Check if a dominators can supply the range.
  if (range_from_dom (block_result, name, bb, RFD_FILL))
    {
      if (DEBUG_RANGE_CACHE)
	{
	  fprintf (dump_file, "Filled from dominator! :  ");
	  block_result.dump (dump_file);
	  fprintf (dump_file, "\n");
	}
      // See if any equivalences can refine it.
      // PR 109462, like 108139 below, a one way equivalence introduced
      // by a PHI node can also be through the definition side.  Disallow it.
      tree equiv_name;
      relation_kind rel;
      int prec = TYPE_PRECISION (type);
      // If there are too many basic blocks, do not attempt to process
      // equivalencies.
      if (last_basic_block_for_fn (cfun) > param_vrp_sparse_threshold)
	{
	  m_on_entry.set_bb_range (name, bb, block_result);
	  gcc_checking_assert (m_workback.length () == start_length);
	  return;
	}
      FOR_EACH_PARTIAL_AND_FULL_EQUIV (m_relation, bb, name, equiv_name, rel)
	{
	  basic_block equiv_bb = gimple_bb (SSA_NAME_DEF_STMT (equiv_name));

	  // Ignore partial equivs that are smaller than this object.
	  if (rel != VREL_EQ && prec > pe_to_bits (rel))
	    continue;

	  // Check if the equiv has any ranges calculated.
	  if (!gori ().has_edge_range_p (equiv_name))
	    continue;

	  // Check if the equiv definition dominates this block
	  if (equiv_bb == bb ||
	      (equiv_bb && !dominated_by_p (CDI_DOMINATORS, bb, equiv_bb)))
	    continue;

	  if (DEBUG_RANGE_CACHE)
	    {
	      if (rel == VREL_EQ)
		fprintf (dump_file, "Checking Equivalence (");
	      else
		fprintf (dump_file, "Checking Partial equiv (");
	      print_relation (dump_file, rel);
	      fprintf (dump_file, ") ");
	      print_generic_expr (dump_file, equiv_name, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }
	  value_range equiv_range (TREE_TYPE (equiv_name));
	  if (range_from_dom (equiv_range, equiv_name, bb, RFD_READ_ONLY))
	    {
	      if (rel != VREL_EQ)
		range_cast (equiv_range, type);
	      else
		adjust_equivalence_range (equiv_range);

	      if (block_result.intersect (equiv_range))
		{
		  if (DEBUG_RANGE_CACHE)
		    {
		      if (rel == VREL_EQ)
			fprintf (dump_file, "Equivalence update! :  ");
		      else
			fprintf (dump_file, "Partial equiv update! :  ");
		      print_generic_expr (dump_file, equiv_name, TDF_SLIM);
		      fprintf (dump_file, " has range  :  ");
		      equiv_range.dump (dump_file);
		      fprintf (dump_file, " refining range to :");
		      block_result.dump (dump_file);
		      fprintf (dump_file, "\n");
		    }
		}
	    }
	}

      m_on_entry.set_bb_range (name, bb, block_result);
      gcc_checking_assert (m_workback.length () == start_length);
      return;
    }

  // Visit each block back to the DEF.  Initialize each one to UNDEFINED.
  // m_visited at the end will contain all the blocks that we needed to set
  // the range_on_entry cache for.
  m_workback.safe_push (bb);
  undefined.set_undefined ();
  m_on_entry.set_bb_range (name, bb, undefined);
  gcc_checking_assert (m_update->empty_p ());

  while (m_workback.length () > start_length)
    {
      basic_block node = m_workback.pop ();
      if (DEBUG_RANGE_CACHE)
	{
	  fprintf (dump_file, "BACK visiting block %d for ", node->index);
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}

      FOR_EACH_EDGE (e, ei, node->preds)
	{
	  basic_block pred = e->src;
	  value_range r (TREE_TYPE (name));

	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "  %d->%d ",e->src->index, e->dest->index);

	  // If the pred block is the def block add this BB to update list.
	  if (pred == def_bb)
	    {
	      m_update->add (node);
	      continue;
	    }

	  // If the pred is entry but NOT def, then it is used before
	  // defined, it'll get set to [] and no need to update it.
	  if (pred == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	    {
	      if (DEBUG_RANGE_CACHE)
		fprintf (dump_file, "entry: bail.");
	      continue;
	    }

	  // Regardless of whether we have visited pred or not, if the
	  // pred has inferred ranges, revisit this block.
	  // Don't search the DOM tree.
	  if (infer_oracle ().has_range_p (pred, name))
	    {
	      if (DEBUG_RANGE_CACHE)
		fprintf (dump_file, "Inferred range: update ");
	      m_update->add (node);
	    }

	  // If the pred block already has a range, or if it can contribute
	  // something new. Ie, the edge generates a range of some sort.
	  if (m_on_entry.get_bb_range (r, name, pred))
	    {
	      if (DEBUG_RANGE_CACHE)
		{
		  fprintf (dump_file, "has cache, ");
		  r.dump (dump_file);
		  fprintf (dump_file, ", ");
		}
	      if (!r.undefined_p () || gori ().has_edge_range_p (name, e))
		{
		  m_update->add (node);
		  if (DEBUG_RANGE_CACHE)
		    fprintf (dump_file, "update. ");
		}
	      continue;
	    }

	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "pushing undefined pred block.\n");
	  // If the pred hasn't been visited (has no range), add it to
	  // the list.
	  gcc_checking_assert (!m_on_entry.bb_range_p (name, pred));
	  m_on_entry.set_bb_range (name, pred, undefined);
	  m_workback.safe_push (pred);
	}
    }

  if (DEBUG_RANGE_CACHE)
    fprintf (dump_file, "\n");

  // Now fill in the marked blocks with values.
  propagate_cache (name);
  if (DEBUG_RANGE_CACHE)
    fprintf (dump_file, "  Propagation update done.\n");
}

// Resolve the range of BB if the dominators range is R by calculating incoming
// edges to this block.  All lead back to the dominator so should be cheap.
// The range for BB is set and returned in R.

void
ranger_cache::resolve_dom (vrange &r, tree name, basic_block bb)
{
  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (name));
  basic_block dom_bb = get_immediate_dominator (CDI_DOMINATORS, bb);

  // if it doesn't already have a value, store the incoming range.
  if (!m_on_entry.bb_range_p (name, dom_bb) && def_bb != dom_bb)
    {
      // If the range can't be store, don't try to accumulate
      // the range in PREV_BB due to excessive recalculations.
      if (!m_on_entry.set_bb_range (name, dom_bb, r))
	return;
    }
  // With the dominator set, we should be able to cheaply query
  // each incoming edge now and accumulate the results.
  r.set_undefined ();
  edge e;
  edge_iterator ei;
  value_range er (TREE_TYPE (name));
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      // If the predecessor is dominated by this block, then there is a back
      // edge, and won't provide anything useful.  We'll actually end up with
      // VARYING as we will not resolve this node.
      if (dominated_by_p (CDI_DOMINATORS, e->src, bb))
	continue;
      edge_range (er, e, name, RFD_READ_ONLY);
      r.union_ (er);
    }
  // Set the cache in PREV_BB so it is not calculated again.
  m_on_entry.set_bb_range (name, bb, r);
}

// Get the range of NAME from dominators of BB and return it in R.  Search the
// dominator tree based on MODE.

bool
ranger_cache::range_from_dom (vrange &r, tree name, basic_block start_bb,
			      enum rfd_mode mode)
{
  if (mode == RFD_NONE || !dom_info_available_p (CDI_DOMINATORS))
    return false;

  // Search back to the definition block or entry block.
  basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (name));
  if (def_bb == NULL)
    def_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  basic_block bb;
  basic_block prev_bb = start_bb;

  // Track any inferred ranges seen.
  value_range infer (TREE_TYPE (name));
  infer.set_varying (TREE_TYPE (name));

  // Range on entry to the DEF block should not be queried.
  gcc_checking_assert (start_bb != def_bb);
  unsigned start_limit = m_workback.length ();

  // Default value is global range.
  get_global_range (r, name);

  // The dominator of EXIT_BLOCK doesn't seem to be set, so at least handle
  // the common single exit cases.
  if (start_bb == EXIT_BLOCK_PTR_FOR_FN (cfun) && single_pred_p (start_bb))
    bb = single_pred_edge (start_bb)->src;
  else
    bb = get_immediate_dominator (CDI_DOMINATORS, start_bb);

  // Search until a value is found, pushing blocks which may need calculating.
  for ( ; bb; prev_bb = bb, bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      // Accumulate any block exit inferred ranges.
      infer_oracle ().maybe_adjust_range (infer, name, bb);

      // This block has an outgoing range.
      if (gori ().has_edge_range_p (name, bb))
	m_workback.safe_push (prev_bb);
      else
	{
	  // Normally join blocks don't carry any new range information on
	  // incoming edges.  If the first incoming edge to this block does
	  // generate a range, calculate the ranges if all incoming edges
	  // are also dominated by the dominator.  (Avoids backedges which
	  // will break the rule of moving only upward in the dominator tree).
	  // If the first pred does not generate a range, then we will be
	  // using the dominator range anyway, so that's all the check needed.
	  if (EDGE_COUNT (prev_bb->preds) > 1
	      && gori ().has_edge_range_p (name, EDGE_PRED (prev_bb, 0)->src))
	    {
	      edge e;
	      edge_iterator ei;
	      bool all_dom = true;
	      FOR_EACH_EDGE (e, ei, prev_bb->preds)
		if (e->src != bb
		    && !dominated_by_p (CDI_DOMINATORS, e->src, bb))
		  {
		    all_dom = false;
		    break;
		  }
	      if (all_dom)
		m_workback.safe_push (prev_bb);
	    }
	}

      if (def_bb == bb)
	break;

      if (m_on_entry.get_bb_range (r, name, bb))
	break;
    }

  if (DEBUG_RANGE_CACHE)
    {
      fprintf (dump_file, "CACHE: BB %d DOM query for ", start_bb->index);
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ", found ");
      r.dump (dump_file);
      if (bb)
	fprintf (dump_file, " at BB%d\n", bb->index);
      else
	fprintf (dump_file, " at function top\n");
    }

  // Now process any blocks wit incoming edges that nay have adjustments.
  while (m_workback.length () > start_limit)
    {
      value_range er (TREE_TYPE (name));
      prev_bb = m_workback.pop ();
      if (!single_pred_p (prev_bb))
	{
	  // Non single pred means we need to cache a value in the dominator
	  // so we can cheaply calculate incoming edges to this block, and
	  // then store the resulting value.  If processing mode is not
	  // RFD_FILL, then the cache cant be stored to, so don't try.
	  // Otherwise this becomes a quadratic timed calculation.
	  if (mode == RFD_FILL)
	    resolve_dom (r, name, prev_bb);
	  continue;
	}

      edge e = single_pred_edge (prev_bb);
      bb = e->src;
      if (gori ().edge_range_p (er, e, name, *this))
	{
	  r.intersect (er);
	  // If this is a normal edge, apply any inferred ranges.
	  if ((e->flags & (EDGE_EH | EDGE_ABNORMAL)) == 0)
	    infer_oracle ().maybe_adjust_range (r, name, bb);

	  if (DEBUG_RANGE_CACHE)
	    {
	      fprintf (dump_file, "CACHE: Adjusted edge range for %d->%d : ",
		       bb->index, prev_bb->index);
	      r.dump (dump_file);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  // Apply non-null if appropriate.
  if (!has_abnormal_call_or_eh_pred_edge_p (start_bb))
    r.intersect (infer);

  if (DEBUG_RANGE_CACHE)
    {
      fprintf (dump_file, "CACHE: Range for DOM returns : ");
      r.dump (dump_file);
      fprintf (dump_file, "\n");
    }
  return true;
}

// This routine will register an inferred value in block BB, and possibly
// update the on-entry cache if appropriate.

void
ranger_cache::register_inferred_value (const vrange &ir, tree name,
				       basic_block bb)
{
  value_range r (TREE_TYPE (name));
  if (!m_on_entry.get_bb_range (r, name, bb))
    exit_range (r, name, bb, RFD_READ_ONLY);
  if (r.intersect (ir))
    {
      m_on_entry.set_bb_range (name, bb, r);
      // If this range was invariant before, remove invariant.
      if (!gori ().has_edge_range_p (name))
	gori_ssa ()->set_range_invariant (name, false);
    }
}

// This routine is used during a block walk to adjust any inferred ranges
// of operands on stmt S.

void
ranger_cache::apply_inferred_ranges (gimple *s)
{
  bool update = true;

  basic_block bb = gimple_bb (s);
  gimple_infer_range infer(s);
  if (infer.num () == 0)
    return;

  // Do not update the on-entry cache for block ending stmts.
  if (stmt_ends_bb_p (s))
    {
      edge_iterator ei;
      edge e;
      FOR_EACH_EDGE (e, ei, gimple_bb (s)->succs)
	if (!(e->flags & (EDGE_ABNORMAL|EDGE_EH)))
	  break;
      if (e == NULL)
	update = false;
    }

  infer_oracle ().add_ranges (s, infer);
  if (update)
    for (unsigned x = 0; x < infer.num (); x++)
      register_inferred_value (infer.range (x), infer.name (x), bb);
}
