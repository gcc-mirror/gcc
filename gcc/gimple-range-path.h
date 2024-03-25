/* Header file for jump threading path solver.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.
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
// Note that the blocks are in reverse order, thus the exit block is
// path[0].

class path_range_query : public range_query
{
public:
  path_range_query (class gimple_ranger &ranger,
		    const vec<basic_block> &path,
		    const bitmap_head *dependencies = NULL,
		    bool resolve = true);
  path_range_query (gimple_ranger &ranger, bool resolve = true);
  virtual ~path_range_query ();
  void reset_path (const vec<basic_block> &, const bitmap_head *dependencies);
  bool range_of_expr (vrange &r, tree name, gimple * = NULL) override;
  bool range_of_stmt (vrange &r, gimple *, tree name = NULL) override;
  bool unreachable_path_p ();
  void dump (FILE *) override;
  void debug ();

private:
  bool internal_range_of_expr (vrange &r, tree name, gimple *);
  void compute_ranges (const bitmap_head *dependencies);
  void compute_exit_dependencies (bitmap_head *dependencies);
  bool defined_outside_path (tree name);
  void range_on_path_entry (vrange &r, tree name);
  path_oracle *get_path_oracle () { return (path_oracle *)m_oracle; }

  // Cache manipulation.
  bool get_cache (vrange &r, tree name);

  // Methods to compute ranges for the given path.
  bool range_defined_in_block (vrange &, tree name, basic_block bb);
  void compute_ranges_in_block (basic_block bb);
  void compute_ranges_in_phis (basic_block bb);
  void adjust_for_non_null_uses (basic_block bb);
  void ssa_range_in_phi (vrange &r, gphi *phi);
  void compute_outgoing_relations (basic_block bb, basic_block next);
  void compute_phi_relations (basic_block bb, basic_block prev);
  void maybe_register_phi_relation (gphi *, edge e);
  bool add_to_exit_dependencies (tree name, bitmap dependencies);
  bool exit_dependency_p (tree name);
  bool ssa_defined_in_bb (tree name, basic_block bb);
  bool relations_may_be_invalidated (edge);

  // Path navigation.
  basic_block entry_bb () { return m_path[m_path.length () - 1]; }
  basic_block exit_bb ()  { return m_path[0]; }
  basic_block curr_bb ()  { return m_path[m_pos]; }
  basic_block prev_bb ()  { return m_path[m_pos + 1]; }
  basic_block next_bb ()  { return m_path[m_pos - 1]; }
  bool at_entry ()	  { return m_pos == m_path.length () - 1; }
  bool at_exit ()	  { return m_pos == 0; }
  void move_next ()	  { --m_pos; }

  // Range cache for SSA names.
  ssa_lazy_cache m_cache;

  // Path being analyzed.
  auto_vec<basic_block> m_path;

  // This is a list of SSA names that may have relevant context
  // information for solving the final conditional along the path.
  // Ranges for these SSA names are pre-calculated and cached during a
  // top-down traversal of the path, and are then used to answer
  // questions at the path exit.
  auto_bitmap m_exit_dependencies;

  // A ranger used to resolve ranges for SSA names whose values come
  // from outside the path.
  gimple_ranger &m_ranger;

  // Current path position.
  unsigned m_pos;

  // Use ranger to resolve anything not known on entry.
  bool m_resolve;

  // Set if there were any undefined expressions while pre-calculating path.
  bool m_undefined_path;
};

#endif // GCC_TREE_SSA_THREADSOLVER_H
