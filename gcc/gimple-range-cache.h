/* Header file for gimple ranger SSA cache.
   Copyright (C) 2017-2021 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

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

#ifndef GCC_SSA_RANGE_CACHE_H
#define GCC_SSA_RANGE_CACHE_H

#include "gimple-range-gori.h" 

// Class used to track non-null references of an SSA name.  A vector
// of bitmaps indexed by SSA name is maintained.  When indexed by
// basic block, an on-bit indicates there is a non-null dereference
// for that SSA in that block.

class non_null_ref
{
public:
  non_null_ref ();
  ~non_null_ref ();
  bool non_null_deref_p (tree name, basic_block bb);
private:
  vec <bitmap> m_nn;
  void process_name (tree name);
  bitmap_obstack m_bitmaps;
};

// This class manages a vector of pointers to ssa_block ranges.  It
// provides the basis for the "range on entry" cache for all
// SSA names.

class block_range_cache
{
public:
  block_range_cache ();
  ~block_range_cache ();

  void set_bb_range (tree name, const basic_block bb, const irange &r);
  void set_bb_varying (tree name, const basic_block bb);
  bool get_bb_range (irange &r, tree name, const basic_block bb);
  bool bb_range_p (tree name, const basic_block bb);

  void dump (FILE *f);
  void dump (FILE *f, basic_block bb, bool print_varying = true);
private:
  vec<class ssa_block_ranges *> m_ssa_ranges;
  ssa_block_ranges &get_block_ranges (tree name);
  ssa_block_ranges *query_block_ranges (tree name);
  irange_allocator *m_irange_allocator;
};

// This global cache is used with the range engine as markers for what
// has been visited during this incarnation.  Once the ranger evaluates
// a name, it is typically not re-evaluated again.

class ssa_global_cache
{
public:
  ssa_global_cache ();
  ~ssa_global_cache ();
  bool get_global_range (irange &r, tree name) const;
  bool set_global_range (tree name, const irange &r);
  void clear_global_range (tree name);
  void clear ();
  void dump (FILE *f = stderr);
private:
  vec<irange *> m_tab;
  class irange_allocator *m_irange_allocator;
};

// This class provides all the caches a global ranger may need, and makes 
// them available for gori-computes to query so outgoing edges can be
// properly calculated.

class ranger_cache : public gori_compute
{
public:
  ranger_cache (class gimple_ranger &q);
  ~ranger_cache ();

  virtual void ssa_range_in_bb (irange &r, tree name, basic_block bb);
  bool block_range (irange &r, basic_block bb, tree name, bool calc = true);

  bool get_global_range (irange &r, tree name) const;
  bool get_non_stale_global_range (irange &r, tree name);
  void set_global_range (tree name, const irange &r);
  void register_dependency (tree name, tree dep);

  non_null_ref m_non_null;

  void dump (FILE *f, bool dump_gori = true);
  void dump (FILE *f, basic_block bb);
private:
  ssa_global_cache m_globals;
  block_range_cache m_on_entry;
  class temporal_cache *m_temporal;
  void add_to_update (basic_block bb);
  void fill_block_cache (tree name, basic_block bb, basic_block def_bb);
  void propagate_cache (tree name);

  void propagate_updated_value (tree name, basic_block bb);

  vec<basic_block> m_workback;
  vec<basic_block> m_update_list;

  // Iterative "poor value" calculations.
  struct update_record
  {
    basic_block bb;	// Block which value needs to be calculated in.
    tree calc;		// SSA_NAME which needs its value calculated.
  };
  bool push_poor_value (basic_block bb, tree name);
  vec<update_record> m_poor_value_list;
  class gimple_ranger &query;
};

#endif // GCC_SSA_RANGE_CACHE_H
