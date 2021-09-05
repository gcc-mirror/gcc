/* Basic block path solver.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cfganal.h"
#include "value-range.h"
#include "gimple-range.h"
#include "tree-pretty-print.h"
#include "gimple-range-path.h"
#include "ssa.h"

// Internal construct to help facilitate debugging of solver.
#define DEBUG_SOLVER (0 && dump_file)

path_range_query::path_range_query (gimple_ranger &ranger)
  : m_ranger (ranger)
{
  m_cache = new ssa_global_cache;
  m_has_cache_entry = BITMAP_ALLOC (NULL);
  m_path = NULL;
}

path_range_query::~path_range_query ()
{
  BITMAP_FREE (m_has_cache_entry);
  delete m_cache;
}

// Mark cache entry for NAME as unused.

void
path_range_query::clear_cache (tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  bitmap_clear_bit (m_has_cache_entry, v);
}

// If NAME has a cache entry, return it in R, and return TRUE.

inline bool
path_range_query::get_cache (irange &r, tree name)
{
  if (!gimple_range_ssa_p (name))
    return get_global_range_query ()->range_of_expr (r, name);

  unsigned v = SSA_NAME_VERSION (name);
  if (bitmap_bit_p (m_has_cache_entry, v))
    return m_cache->get_global_range (r, name);

  return false;
}

// Set the cache entry for NAME to R.

void
path_range_query::set_cache (const irange &r, tree name)
{
  unsigned v = SSA_NAME_VERSION (name);
  bitmap_set_bit (m_has_cache_entry, v);
  m_cache->set_global_range (name, r);
}

void
path_range_query::dump (FILE *dump_file)
{
  if (m_path->is_empty ())
    return;

  unsigned i;
  bitmap_iterator bi;
  extern void dump_ranger (FILE *, const vec<basic_block> &);

  fprintf (dump_file, "Path is (length=%d):\n", m_path->length ());
  dump_ranger (dump_file, *m_path);

  fprintf (dump_file, "Imports:\n");
  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  m_cache->dump (dump_file);
}

void
path_range_query::debug ()
{
  dump (stderr);
}

// Return the range of NAME at the end of the path being analyzed.

bool
path_range_query::internal_range_of_expr (irange &r, tree name, gimple *stmt)
{
  if (!irange::supports_type_p (TREE_TYPE (name)))
    return false;

  if (get_cache (r, name))
    return true;


  basic_block bb = stmt ? gimple_bb (stmt) : exit_bb ();
  if (stmt && range_defined_in_block (r, name, bb))
    {
      set_cache (r, name);
      return true;
    }

  r.set_varying (TREE_TYPE (name));
  return true;
}

bool
path_range_query::range_of_expr (irange &r, tree name, gimple *stmt)
{
  if (internal_range_of_expr (r, name, stmt))
    {
      if (r.undefined_p ())
	m_undefined_path = true;

      return true;
    }
  return false;
}

bool
path_range_query::unreachable_path_p ()
{
  return m_undefined_path;
}

// Return the range of STMT at the end of the path being analyzed.

bool
path_range_query::range_of_stmt (irange &r, gimple *stmt, tree)
{
  tree type = gimple_range_type (stmt);

  if (!irange::supports_type_p (type))
    return false;

  if (!fold_range (r, stmt, this))
    r.set_varying (type);

  return true;
}

// Initialize the current path to PATH.  The current block is set to
// the entry block to the path.
//
// Note that the blocks are in reverse order, so the exit block is
// path[0].

void
path_range_query::set_path (const vec<basic_block> &path)
{
  gcc_checking_assert (path.length () > 1);
  m_path = &path;
  m_pos = m_path->length () - 1;
  bitmap_clear (m_has_cache_entry);
}

// Return the range of the result of PHI in R.

void
path_range_query::ssa_range_in_phi (irange &r, gphi *phi)
{
  tree name = gimple_phi_result (phi);
  basic_block bb = gimple_bb (phi);

  // We experimented with querying ranger's range_on_entry here, but
  // the performance penalty was too high, for hardly any improvements.
  if (at_entry ())
    {
      // Try fold just in case we can resolve simple things like PHI <5(99), 6(88)>.
      if (!fold_range (r, phi, this))
	r.set_varying (TREE_TYPE (name));

      return;
    }

  basic_block prev = prev_bb ();
  edge e_in = find_edge (prev, bb);
  unsigned nargs = gimple_phi_num_args (phi);

  for (size_t i = 0; i < nargs; ++i)
    if (e_in == gimple_phi_arg_edge (phi, i))
      {
	tree arg = gimple_phi_arg_def (phi, i);

	if (!get_cache (r, arg))
	  r.set_varying (TREE_TYPE (name));

	return;
      }
  gcc_unreachable ();
}

// If NAME is defined in BB, set R to the range of NAME, and return
// TRUE.  Otherwise, return FALSE.

bool
path_range_query::range_defined_in_block (irange &r, tree name, basic_block bb)
{
  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  basic_block def_bb = gimple_bb (def_stmt);

  if (def_bb != bb)
    return false;

  if (gimple_code (def_stmt) == GIMPLE_PHI)
    ssa_range_in_phi (r, as_a<gphi *> (def_stmt));
  else if (!range_of_stmt (r, def_stmt, name))
    r.set_varying (TREE_TYPE (name));

  if (bb)
    m_non_null.adjust_range (r, name, bb);

  if (DEBUG_SOLVER && (bb || !r.varying_p ()))
    {
      fprintf (dump_file, "range_defined_in_block (BB%d) for ", bb ? bb->index : -1);
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, " is ");
      r.dump (dump_file);
      fprintf (dump_file, "\n");
    }

  // We may have an artificial statement not in the IL.
  if (!bb && r.varying_p ())
    return false;

  return true;
}

// Precompute ranges defined in the current block, or ranges
// that are exported on an edge to the next block.

void
path_range_query::precompute_ranges_in_block (basic_block bb)
{
  bitmap_iterator bi;
  int_range_max r, cached_range;
  unsigned i;

  // Force recalculation of any names in the cache that are defined in
  // this block.  This can happen on interdependent SSA/phis in loops.
  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      basic_block def_bb = gimple_bb (def_stmt);

      if (def_bb == bb)
	clear_cache (name);
    }

  // Solve imports defined in this block.
  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);

      if (range_defined_in_block (r, name, bb))
	set_cache (r, name);
    }

  if (at_exit ())
    return;

  // Solve imports that are exported to the next block.
  edge e = find_edge (bb, next_bb ());
  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);
      gori_compute &g = m_ranger.gori ();
      bitmap exports = g.exports (bb);

      if (bitmap_bit_p (exports, i))
	{
	  if (g.outgoing_edge_range_p (r, e, name, *this))
	    {
	      if (get_cache (cached_range, name))
		r.intersect (cached_range);

	      set_cache (r, name);
	      if (DEBUG_SOLVER)
		{
		  fprintf (dump_file, "outgoing_edge_range_p for ");
		  print_generic_expr (dump_file, name, TDF_SLIM);
		  fprintf (dump_file, " on edge %d->%d ",
			   e->src->index, e->dest->index);
		  fprintf (dump_file, "is ");
		  r.dump (dump_file);
		  fprintf (dump_file, "\n");
		}
	    }
	}
    }
}

// Adjust all pointer imports in BB with non-null information.

void
path_range_query::adjust_for_non_null_uses (basic_block bb)
{
  int_range_max r;
  bitmap_iterator bi;
  unsigned i;

  EXECUTE_IF_SET_IN_BITMAP (m_imports, 0, i, bi)
    {
      tree name = ssa_name (i);

      if (!POINTER_TYPE_P (TREE_TYPE (name)))
	continue;

      if (get_cache (r, name))
	{
	  if (r.nonzero_p ())
	    continue;
	}
      else
	r.set_varying (TREE_TYPE (name));

      if (m_non_null.adjust_range (r, name, bb))
	set_cache (r, name);
    }
}

// Precompute the ranges for IMPORTS along PATH.
//
// IMPORTS are the set of SSA names, any of which could potentially
// change the value of the final conditional in PATH.

void
path_range_query::precompute_ranges (const vec<basic_block> &path,
				     const bitmap_head *imports)
{
  set_path (path);
  m_imports = imports;
  m_undefined_path = false;

  if (DEBUG_SOLVER)
    {
      fprintf (dump_file, "\npath_range_query: precompute_ranges for path: ");
      for (unsigned i = path.length (); i > 0; --i)
	{
	  basic_block bb = path[i - 1];
	  fprintf (dump_file, "BB %d", bb->index);
	  if (i > 1)
	    fprintf (dump_file, ", ");
	}
      fprintf (dump_file, "\n");
    }

  while (1)
    {
      basic_block bb = curr_bb ();

      precompute_ranges_in_block (bb);
      adjust_for_non_null_uses (bb);

      if (at_exit ())
	break;

      move_next ();
    }

  if (DEBUG_SOLVER)
    dump (dump_file);
}
