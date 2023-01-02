/* Header file for gimple ranger SSA cache.
   Copyright (C) 2017-2023 Free Software Foundation, Inc.
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
#include "gimple-range-infer.h"

// This class manages a vector of pointers to ssa_block ranges.  It
// provides the basis for the "range on entry" cache for all
// SSA names.

class block_range_cache
{
public:
  block_range_cache ();
  ~block_range_cache ();

  bool set_bb_range (tree name, const_basic_block bb, const vrange &v);
  bool get_bb_range (vrange &v, tree name, const_basic_block bb);
  bool bb_range_p (tree name, const_basic_block bb);

  void dump (FILE *f);
  void dump (FILE *f, basic_block bb, bool print_varying = true);
private:
  vec<class ssa_block_ranges *> m_ssa_ranges;
  ssa_block_ranges &get_block_ranges (tree name);
  ssa_block_ranges *query_block_ranges (tree name);
  class vrange_allocator *m_range_allocator;
  bitmap_obstack m_bitmaps;
};

// This global cache is used with the range engine as markers for what
// has been visited during this incarnation.  Once the ranger evaluates
// a name, it is typically not re-evaluated again.

class ssa_global_cache
{
public:
  ssa_global_cache ();
  ~ssa_global_cache ();
  bool get_global_range (vrange &r, tree name) const;
  bool set_global_range (tree name, const vrange &r);
  void clear_global_range (tree name);
  void clear ();
  void dump (FILE *f = stderr);
private:
  vec<vrange *> m_tab;
  vrange_allocator *m_range_allocator;
};

// This class provides all the caches a global ranger may need, and makes 
// them available for gori-computes to query so outgoing edges can be
// properly calculated.

class ranger_cache : public range_query
{
public:
  ranger_cache (int not_executable_flag, bool use_imm_uses);
  ~ranger_cache ();

  bool range_of_expr (vrange &r, tree name, gimple *stmt) final override;
  bool range_on_edge (vrange &r, edge e, tree expr) final override;
  bool block_range (vrange &r, basic_block bb, tree name, bool calc = true);

  bool get_global_range (vrange &r, tree name) const;
  bool get_global_range (vrange &r, tree name, bool &current_p);
  void set_global_range (tree name, const vrange &r);

  void propagate_updated_value (tree name, basic_block bb);

  void register_inferred_value (const vrange &r, tree name, basic_block bb);
  void apply_inferred_ranges (gimple *s);
  gori_compute m_gori;
  infer_range_manager m_exit;

  void dump_bb (FILE *f, basic_block bb);
  virtual void dump (FILE *f) override;
private:
  ssa_global_cache m_globals;
  block_range_cache m_on_entry;
  class temporal_cache *m_temporal;
  void fill_block_cache (tree name, basic_block bb, basic_block def_bb);
  void propagate_cache (tree name);

  enum rfd_mode
    {
      RFD_NONE,		// Only look at current block cache.
      RFD_READ_ONLY,	// Scan DOM tree, do not write to cache.
      RFD_FILL		// Scan DOM tree, updating important nodes.
    };
  bool range_from_dom (vrange &r, tree name, basic_block bb, enum rfd_mode);
  void resolve_dom (vrange &r, tree name, basic_block bb);
  void range_of_def (vrange &r, tree name, basic_block bb = NULL);
  void entry_range (vrange &r, tree expr, basic_block bb, enum rfd_mode);
  void exit_range (vrange &r, tree expr, basic_block bb, enum rfd_mode);
  bool edge_range (vrange &r, edge e, tree name, enum rfd_mode);

  vec<basic_block> m_workback;
  class update_list *m_update;
};

#endif // GCC_SSA_RANGE_CACHE_H
