/* Header file for jump threading path solver.
   Copyright (C) 2021 Free Software Foundation, Inc.
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

#ifndef GCC_TREE_SSA_THREADSOLVER_H
#define GCC_TREE_SSA_THREADSOLVER_H

// This class is a basic block path solver.  Given a set of BBs
// indicating a path through the CFG, range_of_expr and range_of_stmt
// will calculate the range of an SSA or STMT as if the BBs in the
// path would have been executed in order.
//
// Only SSA names passed in IMPORTS are precomputed, and can be
// queried.
//
// Note that the blocks are in reverse order, thus the exit block is
// path[0].

class path_range_query : public range_query
{
public:
  path_range_query (class gimple_ranger &ranger);
  virtual ~path_range_query ();
  void precompute_ranges (const vec<basic_block> &path,
			  const bitmap_head *imports);
  bool range_of_expr (irange &r, tree name, gimple * = NULL) override;
  bool range_of_stmt (irange &r, gimple *, tree name = NULL) override;
  void dump (FILE *) override;
  void debug ();

private:
  // Cache manipulation.
  void set_cache (const irange &r, tree name);
  bool get_cache (irange &r, tree name);
  void clear_cache (tree name);

  // Methods to precompute ranges for the given path.
  bool range_defined_in_block (irange &, tree name, basic_block bb);
  void precompute_ranges_in_block (basic_block bb);
  void ssa_range_in_phi (irange &r, gphi *phi);

  // Path navigation.
  void set_path (const vec<basic_block> &);
  basic_block entry_bb () { return (*m_path)[m_path->length () - 1]; }
  basic_block exit_bb ()  { return (*m_path)[0]; }
  basic_block curr_bb ()  { return (*m_path)[m_pos]; }
  basic_block prev_bb ()  { return (*m_path)[m_pos + 1]; }
  basic_block next_bb ()  { return (*m_path)[m_pos - 1]; }
  bool at_entry ()	  { return m_pos == m_path->length () - 1; }
  bool at_exit ()	  { return m_pos == 0; }
  void move_next ()	  { --m_pos; }

  // Range cache for SSA names.
  ssa_global_cache *m_cache;

  // Set for each SSA that has an active entry in the cache.
  bitmap m_has_cache_entry;

  // Path being analyzed.
  const vec<basic_block> *m_path;

  // Current path position.
  unsigned m_pos;

  const bitmap_head *m_imports;
  gimple_ranger &m_ranger;
};

#endif // GCC_TREE_SSA_THREADSOLVER_H
