/* Gimple ranger SSA cache implementation.
   Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

// During contructor, allocate the vector of ssa_names.

non_null_ref::non_null_ref ()
{
  m_nn.create (0);
  m_nn.safe_grow_cleared (num_ssa_names);
  bitmap_obstack_initialize (&m_bitmaps);
}

// Free any bitmaps which were allocated,a swell as the vector itself.

non_null_ref::~non_null_ref ()
{
  bitmap_obstack_release (&m_bitmaps);
  m_nn.release ();
}

// Return true if NAME has a non-null dereference in block bb.  If this is the
// first query for NAME, calculate the summary first.
// If SEARCH_DOM is true, the search the dominator tree as well.

bool
non_null_ref::non_null_deref_p (tree name, basic_block bb, bool search_dom)
{
  if (!POINTER_TYPE_P (TREE_TYPE (name)))
    return false;

  unsigned v = SSA_NAME_VERSION (name);
  if (!m_nn[v])
    process_name (name);

  if (bitmap_bit_p (m_nn[v], bb->index))
    return true;

  // See if any dominator has set non-zero.
  if (search_dom && dom_info_available_p (CDI_DOMINATORS))
    {
      // Search back to the Def block, or the top, whichever is closer.
      basic_block def_bb = gimple_bb (SSA_NAME_DEF_STMT (name));
      basic_block def_dom = def_bb
			    ? get_immediate_dominator (CDI_DOMINATORS, def_bb)
			    : NULL;
      for ( ;
	    bb && bb != def_dom;
	    bb = get_immediate_dominator (CDI_DOMINATORS, bb))
	if (bitmap_bit_p (m_nn[v], bb->index))
	  return true;
    }
  return false;
}

// Allocate an populate the bitmap for NAME.  An ON bit for a block
// index indicates there is a non-null reference in that block.  In
// order to populate the bitmap, a quick run of all the immediate uses
// are made and the statement checked to see if a non-null dereference
// is made on that statement.

void
non_null_ref::process_name (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  use_operand_p use_p;
  imm_use_iterator iter;
  bitmap b;

  // Only tracked for pointers.
  if (!POINTER_TYPE_P (TREE_TYPE (name)))
    return;

  // Already processed if a bitmap has been allocated.
  if (m_nn[v])
    return;

  b = BITMAP_ALLOC (&m_bitmaps);

  // Loop over each immediate use and see if it implies a non-null value.
  FOR_EACH_IMM_USE_FAST (use_p, iter, name)
    {
      gimple *s = USE_STMT (use_p);
      unsigned index = gimple_bb (s)->index;

      // If bit is already set for this block, dont bother looking again.
      if (bitmap_bit_p (b, index))
	continue;

      // If we can infer a nonnull range, then set the bit for this BB
      if (!SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name)
	  && infer_nonnull_range (s, name))
	bitmap_set_bit (b, index);
    }

  m_nn[v] = b;
}

// -------------------------------------------------------------------------

// This class represents the API into a cache of ranges for an SSA_NAME.
// Routines must be implemented to set, get, and query if a value is set.

class ssa_block_ranges
{
public:
  virtual void set_bb_range (const basic_block bb, const irange &r) = 0;
  virtual bool get_bb_range (irange &r, const basic_block bb) = 0;
  virtual bool bb_range_p (const basic_block bb) = 0;

  void dump(FILE *f);
};

// Print the list of known ranges for file F in a nice format.

void
ssa_block_ranges::dump (FILE *f)
{
  basic_block bb;
  int_range_max r;

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
  sbr_vector (tree t, irange_allocator *allocator);

  virtual void set_bb_range (const basic_block bb, const irange &r) OVERRIDE;
  virtual bool get_bb_range (irange &r, const basic_block bb) OVERRIDE;
  virtual bool bb_range_p (const basic_block bb) OVERRIDE;
protected:
  irange **m_tab;	// Non growing vector.
  int m_tab_size;
  int_range<2> m_varying;
  int_range<2> m_undefined;
  tree m_type;
  irange_allocator *m_irange_allocator;
};


// Initialize a block cache for an ssa_name of type T.

sbr_vector::sbr_vector (tree t, irange_allocator *allocator)
{
  gcc_checking_assert (TYPE_P (t));
  m_type = t;
  m_irange_allocator = allocator;
  m_tab_size = last_basic_block_for_fn (cfun) + 1;
  m_tab = (irange **)allocator->get_memory (m_tab_size * sizeof (irange *));
  memset (m_tab, 0, m_tab_size * sizeof (irange *));

  // Create the cached type range.
  m_varying.set_varying (t);
  m_undefined.set_undefined ();
}

// Set the range for block BB to be R.

void
sbr_vector::set_bb_range (const basic_block bb, const irange &r)
{
  irange *m;
  gcc_checking_assert (bb->index < m_tab_size);
  if (r.varying_p ())
    m = &m_varying;
  else if (r.undefined_p ())
    m = &m_undefined;
  else
    m = m_irange_allocator->allocate (r);
  m_tab[bb->index] = m;
}

// Return the range associated with block BB in R.  Return false if
// there is no range.

bool
sbr_vector::get_bb_range (irange &r, const basic_block bb)
{
  gcc_checking_assert (bb->index < m_tab_size);
  irange *m = m_tab[bb->index];
  if (m)
    {
      r = *m;
      return true;
    }
  return false;
}

// Return true if a range is present.

bool
sbr_vector::bb_range_p (const basic_block bb)
{
  gcc_checking_assert (bb->index < m_tab_size);
  return m_tab[bb->index] != NULL;
}

// -------------------------------------------------------------------------

// Initialize the block cache.

block_range_cache::block_range_cache ()
{
  m_ssa_ranges.create (0);
  m_ssa_ranges.safe_grow_cleared (num_ssa_names);
  m_irange_allocator = new irange_allocator;
}

// Remove any m_block_caches which have been created.

block_range_cache::~block_range_cache ()
{
  delete m_irange_allocator;
  // Release the vector itself.
  m_ssa_ranges.release ();
}

// Set the range for NAME on entry to block BB to R.
// If it has not been // accessed yet, allocate it first.

void
block_range_cache::set_bb_range (tree name, const basic_block bb,
				 const irange &r)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_ssa_ranges.length ())
    m_ssa_ranges.safe_grow_cleared (num_ssa_names + 1);

  if (!m_ssa_ranges[v])
    {
      void *r = m_irange_allocator->get_memory (sizeof (sbr_vector));
      m_ssa_ranges[v] = new (r) sbr_vector (TREE_TYPE (name),
					    m_irange_allocator);
    }
  m_ssa_ranges[v]->set_bb_range (bb, r);
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
block_range_cache::get_bb_range (irange &r, tree name, const basic_block bb)
{
  ssa_block_ranges *ptr = query_block_ranges (name);
  if (ptr)
    return ptr->get_bb_range (r, bb);
  return false;
}

// Return true if NAME has a range set in block BB.

bool
block_range_cache::bb_range_p (tree name, const basic_block bb)
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
  for (x = 0; x < m_ssa_ranges.length (); ++x)
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

// Print all known ranges on entry to blobk BB to file F.

void
block_range_cache::dump (FILE *f, basic_block bb, bool print_varying)
{
  unsigned x;
  int_range_max r;
  bool summarize_varying = false;
  for (x = 1; x < m_ssa_ranges.length (); ++x)
    {
      if (!gimple_range_ssa_p (ssa_name (x)))
	continue;
      if (m_ssa_ranges[x] && m_ssa_ranges[x]->get_bb_range (r, bb))
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
      for (x = 1; x < num_ssa_names; ++x)
	{
	  if (!gimple_range_ssa_p (ssa_name (x)))
	    continue;
	  if (m_ssa_ranges[x] && m_ssa_ranges[x]->get_bb_range (r, bb))
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

// Initialize a global cache.

ssa_global_cache::ssa_global_cache ()
{
  m_tab.create (0);
  m_tab.safe_grow_cleared (num_ssa_names);
  m_irange_allocator = new irange_allocator;
}

// Deconstruct a global cache.

ssa_global_cache::~ssa_global_cache ()
{
  m_tab.release ();
  delete m_irange_allocator;
}

// Retrieve the global range of NAME from cache memory if it exists. 
// Return the value in R.

bool
ssa_global_cache::get_global_range (irange &r, tree name) const
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    return false;

  irange *stow = m_tab[v];
  if (!stow)
    return false;
  r = *stow;
  return true;
}

// Set the range for NAME to R in the global cache.
// Return TRUE if there was already a range set, otherwise false.

bool
ssa_global_cache::set_global_range (tree name, const irange &r)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    m_tab.safe_grow_cleared (num_ssa_names + 1);

  irange *m = m_tab[v];
  if (m && m->fits_p (r))
    *m = r;
  else
    m_tab[v] = m_irange_allocator->allocate (r);
  return m != NULL;
}

// Set the range for NAME to R in the glonbal cache.

void
ssa_global_cache::clear_global_range (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  if (v >= m_tab.length ())
    m_tab.safe_grow_cleared (num_ssa_names + 1);
  m_tab[v] = NULL;
}

// Clear the global cache.

void
ssa_global_cache::clear ()
{
  memset (m_tab.address(), 0, m_tab.length () * sizeof (irange *));
}

// Dump the contents of the global cache to F.

void
ssa_global_cache::dump (FILE *f)
{
  unsigned x;
  int_range_max r;
  fprintf (f, "Non-varying global ranges:\n");
  fprintf (f, "=========================:\n");
  for ( x = 1; x < num_ssa_names; x++)
    if (gimple_range_ssa_p (ssa_name (x)) &&
	get_global_range (r, ssa_name (x))  && !r.varying_p ())
      {
	print_generic_expr (f, ssa_name (x), TDF_NONE);
	fprintf (f, "  : ");
	r.dump (f);
	fprintf (f, "\n");
      }
  fputc ('\n', f);
}

// --------------------------------------------------------------------------


// This struct provides a timestamp for a global range calculation.
// it contains the time counter, as well as a limited number of ssa-names
// that it is dependent upon.  If the timestamp for any of the dependent names
// Are newer, then this range could need updating.

struct range_timestamp
{
  unsigned time;
  unsigned ssa1;
  unsigned ssa2;
};

// This class will manage the timestamps for each ssa_name.
// When a value is calcualted, its timestamp is set to the current time.
// The ssanames it is dependent on have already been calculated, so they will
// have older times.  If one fo those values is ever calculated again, it
// will get a newer timestamp, and the "current_p" check will fail.

class temporal_cache
{
public:
  temporal_cache ();
  ~temporal_cache ();
  bool current_p (tree name) const;
  void set_timestamp (tree name);
  void set_dependency (tree name, tree dep);
  void set_always_current (tree name);
private:
  unsigned temporal_value (unsigned ssa) const;
  const range_timestamp *get_timestamp (unsigned ssa) const;
  range_timestamp *get_timestamp (unsigned ssa);

  unsigned m_current_time;
  vec <range_timestamp> m_timestamp;
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

// Return a pointer to the timetamp for ssa-name at index SSA, if there is
// one, otherwise return NULL.

inline const range_timestamp *
temporal_cache::get_timestamp (unsigned ssa) const
{
  if (ssa >= m_timestamp.length ())
    return NULL;
  return &(m_timestamp[ssa]);
}

// Return a reference to the timetamp for ssa-name at index SSA.  If the index
// is past the end of the vector, extend the vector.

inline range_timestamp *
temporal_cache::get_timestamp (unsigned ssa)
{
  if (ssa >= m_timestamp.length ())
    m_timestamp.safe_grow_cleared (num_ssa_names + 20);
  return &(m_timestamp[ssa]);
}

// This routine will fill NAME's next operand slot with DEP if DEP is a valid
// SSA_NAME and there is a free slot.

inline void
temporal_cache::set_dependency (tree name, tree dep)
{
  if (dep && TREE_CODE (dep) == SSA_NAME)
    {
      gcc_checking_assert (get_timestamp (SSA_NAME_VERSION (name)));
      range_timestamp& ts = *(get_timestamp (SSA_NAME_VERSION (name)));
      if (!ts.ssa1)
	ts.ssa1 = SSA_NAME_VERSION (dep);
      else if (!ts.ssa2 && ts.ssa1 != SSA_NAME_VERSION (name))
	ts.ssa2 = SSA_NAME_VERSION (dep);
    }
}

// Return the timestamp value for SSA, or 0 if there isnt one.
inline unsigned
temporal_cache::temporal_value (unsigned ssa) const
{
  const range_timestamp *ts = get_timestamp (ssa);
  return ts ? ts->time : 0;
}

// Return TRUE if the timestampe for NAME is newer than any of its dependents.

bool
temporal_cache::current_p (tree name) const
{
  const range_timestamp *ts = get_timestamp (SSA_NAME_VERSION (name));
  if (!ts || ts->time == 0)
    return true;
  // Any non-registered dependencies will have a value of 0 and thus be older.
  // Return true if time is newer than either dependent.
  return ts->time > temporal_value (ts->ssa1)
	 && ts->time > temporal_value (ts->ssa2);
}

// This increments the global timer and sets the timestamp for NAME.

inline void
temporal_cache::set_timestamp (tree name)
{
  gcc_checking_assert (get_timestamp (SSA_NAME_VERSION (name)));
  get_timestamp (SSA_NAME_VERSION (name))->time = ++m_current_time;
}

// Set the timestamp to 0, marking it as "always up to date".

inline void
temporal_cache::set_always_current (tree name)
{
  gcc_checking_assert (get_timestamp (SSA_NAME_VERSION (name)));
  get_timestamp (SSA_NAME_VERSION (name))->time = 0;
}


// --------------------------------------------------------------------------

ranger_cache::ranger_cache (gimple_ranger &q) : query (q)
{
  m_workback.create (0);
  m_workback.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_update_list.create (0);
  m_update_list.safe_grow_cleared (last_basic_block_for_fn (cfun));
  m_update_list.truncate (0);
  m_poor_value_list.create (0);
  m_poor_value_list.safe_grow_cleared (20);
  m_poor_value_list.truncate (0);
  m_temporal = new temporal_cache;
}

ranger_cache::~ranger_cache ()
{
  delete m_temporal;
  m_poor_value_list.release ();
  m_workback.release ();
  m_update_list.release ();
}

// Dump the global caches to file F.  if GORI_DUMP is true, dump the
// gori map as well.

void
ranger_cache::dump (FILE *f, bool gori_dump)
{
  m_globals.dump (f);
  if (gori_dump)
    {
      fprintf (f, "\nDUMPING GORI MAP\n");
      gori_compute::dump (f);
    }
  fprintf (f, "\n");
}

// Dump the caches for basic block BB to file F.

void
ranger_cache::dump (FILE *f, basic_block bb)
{
  m_on_entry.dump (f, bb);
}

// Get the global range for NAME, and return in R.  Return false if the
// global range is not set.

bool
ranger_cache::get_global_range (irange &r, tree name) const
{
  return m_globals.get_global_range (r, name);
}

// Get the global range for NAME, and return in R if the value is not stale.
// If the range is set, but is stale, mark it current and return false.
// If it is not set pick up the legacy global value, mark it current, and
// return false.
// Note there is always a value returned in R. The return value indicates
// whether that value is an up-to-date calculated value or not..

bool
ranger_cache::get_non_stale_global_range (irange &r, tree name)
{
  if (m_globals.get_global_range (r, name))
    {
      if (m_temporal->current_p (name))
	return true;
    }
  else
    {
      // Global has never been accessed, so pickup the legacy global value.
      r = gimple_range_global (name);
      m_globals.set_global_range (name, r);
    }
  // After a stale check failure, mark the value as always current until a
  // new one is set.
  m_temporal->set_always_current (name);
  return false;
}
//  Set the global range of NAME to R.

void
ranger_cache::set_global_range (tree name, const irange &r)
{
  if (m_globals.set_global_range (name, r))
    {
      // If there was already a range set, propagate the new value.
      basic_block bb = gimple_bb (SSA_NAME_DEF_STMT (name));
      if (!bb)
	bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);

      if (DEBUG_RANGE_CACHE)
	fprintf (dump_file, "   GLOBAL :");

      propagate_updated_value (name, bb);
    }
  // Mark the value as up-to-date.
  m_temporal->set_timestamp (name);
}

// Register a dependency on DEP to name.  If the timestamp for DEP is ever
// greateer than the timestamp for NAME, then it is newer and NAMEs value
// becomes stale.

void
ranger_cache::register_dependency (tree name, tree dep)
{
  m_temporal->set_dependency (name, dep);
}

// Push a request for a new lookup in block BB of name.  Return true if
// the request is actually made (ie, isn't a duplicate).

bool
ranger_cache::push_poor_value (basic_block bb, tree name)
{
  if (m_poor_value_list.length ())
    {
      // Don't push anything else to the same block.  If there are multiple 
      // things required, another request will come during a later evaluation
      // and this prevents oscillation building uneccessary depth.
      if ((m_poor_value_list.last ()).bb == bb)
	return false;
    }

  struct update_record rec;
  rec.bb = bb;
  rec.calc = name;
  m_poor_value_list.safe_push (rec);
  return true;
}

//  Provide lookup for the gori-computes class to access the best known range
//  of an ssa_name in any given basic block.  Note, this does no additonal
//  lookups, just accesses the data that is already known.

void
ranger_cache::ssa_range_in_bb (irange &r, tree name, basic_block bb)
{
  gimple *s = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = ((s && gimple_bb (s)) ? gimple_bb (s) :
					       ENTRY_BLOCK_PTR_FOR_FN (cfun));
  if (bb == def_bb)
    {
      // NAME is defined in this block, so request its current value
      if (!m_globals.get_global_range (r, name))
	{
	  // If it doesn't have a value calculated, it means it's a
	  // "poor" value being used in some calculation.  Queue it up
	  // as a poor value to be improved later.
	  r = gimple_range_global (name);
	  if (push_poor_value (bb, name))
	    {
	      if (DEBUG_RANGE_CACHE)
		{
		  fprintf (dump_file,
			   "*CACHE* no global def in bb %d for ", bb->index);
		  print_generic_expr (dump_file, name, TDF_SLIM);
		  fprintf (dump_file, " depth : %d\n",
			   m_poor_value_list.length ());
		}
	    }
	 }
    }
  // Look for the on-entry value of name in BB from the cache.
  else if (!m_on_entry.get_bb_range (r, name, bb))
    {
      // If it has no entry but should, then mark this as a poor value.
      // Its not a poor value if it does not have *any* edge ranges,
      // Then global range is as good as it gets.
      if (has_edge_range_p (name) && push_poor_value (bb, name))
	{
	  if (DEBUG_RANGE_CACHE)
	    {
	      fprintf (dump_file,
		       "*CACHE* no on entry range in bb %d for ", bb->index);
	      print_generic_expr (dump_file, name, TDF_SLIM);
	      fprintf (dump_file, " depth : %d\n", m_poor_value_list.length ());
	    }
	}
      // Try to pick up any known global value as a best guess for now.
      if (!m_globals.get_global_range (r, name))
	r = gimple_range_global (name);
    }

  // Check if pointers have any non-null dereferences.  Non-call
  // exceptions mean we could throw in the middle of the block, so just
  // punt for now on those.
  if (r.varying_p () && m_non_null.non_null_deref_p (name, bb, false) &&
      !cfun->can_throw_non_call_exceptions)
    r = range_nonzero (TREE_TYPE (name));
}

// Return a static range for NAME on entry to basic block BB in R.  If
// calc is true, fill any cache entries required between BB and the
// def block for NAME.  Otherwise, return false if the cache is empty.

bool
ranger_cache::block_range (irange &r, basic_block bb, tree name, bool calc)
{
  gcc_checking_assert (gimple_range_ssa_p (name));

  // If there are no range calculations anywhere in the IL, global range
  // applies everywhere, so don't bother caching it.
  if (!has_edge_range_p (name))
    return false;

  if (calc)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      basic_block def_bb = NULL;
      if (def_stmt)
	def_bb = gimple_bb (def_stmt);;
      if (!def_bb)
	{
	  // If we get to the entry block, this better be a default def
	  // or range_on_entry was called for a block not dominated by
	  // the def.  
	  gcc_checking_assert (SSA_NAME_IS_DEFAULT_DEF (name));
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

// Add BB to the list of blocks to update, unless it's already in the list.

void
ranger_cache::add_to_update (basic_block bb)
{
  if (!m_update_list.contains (bb))
    m_update_list.quick_push (bb);
}

// If there is anything in the propagation update_list, continue
// processing NAME until the list of blocks is empty.

void
ranger_cache::propagate_cache (tree name)
{
  basic_block bb;
  edge_iterator ei;
  edge e;
  int_range_max new_range;
  int_range_max current_range;
  int_range_max e_range;

  // Process each block by seeing if its calculated range on entry is
  // the same as its cached value. If there is a difference, update
  // the cache to reflect the new value, and check to see if any
  // successors have cache entries which may need to be checked for
  // updates.

  while (m_update_list.length () > 0)
    {
      bb = m_update_list.pop ();
      gcc_checking_assert (m_on_entry.bb_range_p (name, bb));
      m_on_entry.get_bb_range (current_range, name, bb);

      // Calculate the "new" range on entry by unioning the pred edges.
      new_range.set_undefined ();
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "   edge %d->%d :", e->src->index, bb->index);
	  // Get whatever range we can for this edge.
	  if (!outgoing_edge_range_p (e_range, e, name))
	    {
	      ssa_range_in_bb (e_range, name, e->src);
	      if (DEBUG_RANGE_CACHE)
		{
		  fprintf (dump_file, "No outgoing edge range, picked up ");
		  e_range.dump(dump_file);
		  fprintf (dump_file, "\n");
		}
	    }
	  else
	    {
	      if (DEBUG_RANGE_CACHE)
		{
		  fprintf (dump_file, "outgoing range :");
		  e_range.dump(dump_file);
		  fprintf (dump_file, "\n");
		}
	    }
	  new_range.union_ (e_range);
	  if (new_range.varying_p ())
	    break;
	}

      if (DEBUG_RANGE_CACHE)
	{
	  fprintf (dump_file, "FWD visiting block %d for ", bb->index);
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fprintf (dump_file, "  starting range : ");
	  current_range.dump (dump_file);
	  fprintf (dump_file, "\n");
	}

      // If the range on entry has changed, update it.
      if (new_range != current_range)
	{
	  if (DEBUG_RANGE_CACHE) 
	    {
	      fprintf (dump_file, "      Updating range to ");
	      new_range.dump (dump_file);
	      fprintf (dump_file, "\n      Updating blocks :");
	    }
	  m_on_entry.set_bb_range (name, bb, new_range);
	  // Mark each successor that has a range to re-check its range
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (m_on_entry.bb_range_p (name, e->dest))
	      {
		if (DEBUG_RANGE_CACHE) 
		  fprintf (dump_file, " bb%d",e->dest->index);
		add_to_update (e->dest);
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
}

// Check to see if an update to the value for NAME in BB has any effect
// on values already in the on-entry cache for successor blocks.
// If it does, update them.  Don't visit any blocks which dont have a cache
// entry.

void
ranger_cache::propagate_updated_value (tree name, basic_block bb)
{
  edge e;
  edge_iterator ei;

  // The update work list should be empty at this point.
  gcc_checking_assert (m_update_list.length () == 0);
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
	  add_to_update (e->dest);
	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, " UPDATE: bb%d", e->dest->index);
	}
    }
    if (m_update_list.length () != 0)
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
  int_range_max block_result;
  int_range_max undefined;
  unsigned poor_list_start = m_poor_value_list.length ();  

  // At this point we shouldn't be looking at the def, entry or exit block.
  gcc_checking_assert (bb != def_bb && bb != ENTRY_BLOCK_PTR_FOR_FN (cfun) &&
		       bb != EXIT_BLOCK_PTR_FOR_FN (cfun));

  // If the block cache is set, then we've already visited this block.
  if (m_on_entry.bb_range_p (name, bb))
    return;

  // Visit each block back to the DEF.  Initialize each one to UNDEFINED.
  // m_visited at the end will contain all the blocks that we needed to set
  // the range_on_entry cache for.
  m_workback.truncate (0);
  m_workback.quick_push (bb);
  undefined.set_undefined ();
  m_on_entry.set_bb_range (name, bb, undefined);
  gcc_checking_assert (m_update_list.length () == 0);

  if (DEBUG_RANGE_CACHE)
    {
      fprintf (dump_file, "\n");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, " : ");
    }

  while (m_workback.length () > 0)
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
	  int_range_max r;

	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "  %d->%d ",e->src->index, e->dest->index);

	  // If the pred block is the def block add this BB to update list.
	  if (pred == def_bb)
	    {
	      add_to_update (node);
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
	  // pred has a non-null reference, revisit this block.
	  // Don't search the DOM tree.
	  if (m_non_null.non_null_deref_p (name, pred, false))
	    {
	      if (DEBUG_RANGE_CACHE)
		fprintf (dump_file, "nonnull: update ");
	      add_to_update (node);
	    }

	  // If the pred block already has a range, or if it can contribute
	  // something new. Ie, the edge generates a range of some sort.
	  if (m_on_entry.get_bb_range (r, name, pred))
	    {
	      if (DEBUG_RANGE_CACHE)
		fprintf (dump_file, "has cache, ");
	      if (!r.undefined_p () || has_edge_range_p (name, e))
		{
		  add_to_update (node);
		  if (DEBUG_RANGE_CACHE)
		    fprintf (dump_file, "update. ");
		}
	      continue;
	    }

	  if (DEBUG_RANGE_CACHE)
	    fprintf (dump_file, "pushing undefined pred block. ");
	  // If the pred hasn't been visited (has no range), add it to
	  // the list.
	  gcc_checking_assert (!m_on_entry.bb_range_p (name, pred));
	  m_on_entry.set_bb_range (name, pred, undefined);
	  m_workback.quick_push (pred);
	}
    }

  if (DEBUG_RANGE_CACHE)
    fprintf (dump_file, "\n");

  // Now fill in the marked blocks with values.
  propagate_cache (name);
  if (DEBUG_RANGE_CACHE)
    fprintf (dump_file, "  Propagation update done.\n");

  // Now that the cache has been updated, check to see if there were any 
  // SSA_NAMES used in filling the cache which were "poor values".
  // Evaluate them, and inject any new values into the propagation
  // list, and see if it improves any on-entry values.
  if (poor_list_start !=  m_poor_value_list.length ())
    {
      gcc_checking_assert (poor_list_start < m_poor_value_list.length ());
      while (poor_list_start < m_poor_value_list.length ())
	{
	  // Find a range for this unresolved value.   
	  // Note, this may spawn new cache filling cycles, but by the time it
	  // is finished, the work vectors will all be back to the same state
	  // as before the call.  The update record vector will always be
	  // returned to the current state upon return.
	  struct update_record rec = m_poor_value_list.pop ();
	  basic_block calc_bb = rec.bb;
	  int_range_max tmp;

	  if (DEBUG_RANGE_CACHE)
	    {
	      fprintf (dump_file, "(%d:%d)Calculating ",
		       m_poor_value_list.length () + 1, poor_list_start);
	      print_generic_expr (dump_file, name, TDF_SLIM);
	      fprintf (dump_file, " used POOR VALUE for ");
	      print_generic_expr (dump_file, rec.calc, TDF_SLIM);
	      fprintf (dump_file, " in bb%d, trying to improve:\n",
		       calc_bb->index);
	    }

	  // Calculate a range at the exit from the block so the caches feeding
	  // this block will be filled, and we'll get a "better" value.
	  query.range_on_exit (tmp, calc_bb, rec.calc);
	  
	  // Then ask for NAME to be re-evaluated on outgoing edges and 
	  // use any new values.
	  propagate_updated_value (name, calc_bb);
	}
    }
}

