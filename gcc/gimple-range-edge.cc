/* Gimple range edge functionality.
   Copyright (C) 2020-2024 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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


#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "gimple-range.h"
#include "value-range-storage.h"

// If there is a range control statement at the end of block BB, return it.
// Otherwise return NULL.

gimple *
gimple_outgoing_range_stmt_p (basic_block bb)
{
  gimple_stmt_iterator gsi = gsi_last_nondebug_bb (bb);
  if (!gsi_end_p (gsi))
    {
      gimple *s = gsi_stmt (gsi);
      if (is_a<gcond *> (s) && gimple_range_op_handler::supported_p (s))
	return gsi_stmt (gsi);
      if (is_a <gswitch *> (s))
	return gsi_stmt (gsi);
    }
  return NULL;
}

// Return a TRUE or FALSE range representing the edge value of a GCOND.

void
gcond_edge_range (irange &r, edge e)
{
  gcc_checking_assert (e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE));
  if (e->flags & EDGE_TRUE_VALUE)
    r = range_true ();
  else
    r = range_false ();
}

// Construct a gimple_outgoing_range object.  No memory is allocated.

gimple_outgoing_range::gimple_outgoing_range (int max_sw_edges)
{
  m_edge_table = NULL;
  m_range_allocator = NULL;
  m_max_edges = max_sw_edges;
}

// Destruct an edge object, disposing of any memory allocated.

gimple_outgoing_range::~gimple_outgoing_range ()
{
  if (m_edge_table)
    delete m_edge_table;
  if (m_range_allocator)
    delete m_range_allocator;
}

// Set a new switch limit.

void
gimple_outgoing_range::set_switch_limit (int max_sw_edges)
{
  m_max_edges = max_sw_edges;
}

// Get a range for a switch edge E from statement S and return it in R.
// Use a cached value if it exists, or calculate it if not.

bool
gimple_outgoing_range::switch_edge_range (irange &r, gswitch *sw, edge e)
{
  // ADA currently has cases where the index is 64 bits and the case
  // arguments are 32 bit, causing a trap when we create a case_range.
  // Until this is resolved (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=87798)
  // punt on switches where the labels don't match the argument.
  if (gimple_switch_num_labels (sw) > 1 &&
      TYPE_PRECISION (TREE_TYPE (CASE_LOW (gimple_switch_label (sw, 1)))) !=
      TYPE_PRECISION (TREE_TYPE (gimple_switch_index (sw))))
    return false;

  if (!m_edge_table)
    m_edge_table = new hash_map<edge, vrange_storage *> (n_edges_for_fn (cfun));
  if (!m_range_allocator)
    m_range_allocator = new vrange_allocator;

   vrange_storage **val = m_edge_table->get (e);
   if (!val)
     {
       calc_switch_ranges (sw);
       val = m_edge_table->get (e);
       gcc_checking_assert (val);
     }
   (*val)->get_vrange (r, TREE_TYPE (gimple_switch_index (sw)));
  return true;
}


// Calculate all switch edges from SW and cache them in the hash table.

void
gimple_outgoing_range::calc_switch_ranges (gswitch *sw)
{
  bool existed;
  unsigned x, lim;
  lim = gimple_switch_num_labels (sw);
  tree type = TREE_TYPE (gimple_switch_index (sw));
  edge default_edge = gimple_switch_default_edge (cfun, sw);

  // This should be the first call into this switch.
  //
  // Allocate an int_range_max for the default range case, start with
  // varying and intersect each other case from it.
  int_range_max default_range (type);

  for (x = 1; x < lim; x++)
    {
      edge e = gimple_switch_edge (cfun, sw, x);

      // If this edge is the same as the default edge, do nothing else.
      if (e == default_edge)
	continue;

      wide_int low = wi::to_wide (CASE_LOW (gimple_switch_label (sw, x)));
      wide_int high;
      tree tree_high = CASE_HIGH (gimple_switch_label (sw, x));
      if (tree_high)
	high = wi::to_wide (tree_high);
      else
	high = low;

      // Remove the case range from the default case.
      int_range_max def_range (type, low, high);
      range_cast (def_range, type);
      // If all possible values are taken, set default_range to UNDEFINED.
      if (def_range.varying_p ())
	default_range.set_undefined ();
      else
	{
	  def_range.invert ();
	  default_range.intersect (def_range);
	}

      // Create/union this case with anything on else on the edge.
      int_range_max case_range (type, low, high);
      range_cast (case_range, type);
      vrange_storage *&slot = m_edge_table->get_or_insert (e, &existed);
      if (existed)
	{
	  // If this doesn't change the value, move on.
	  int_range_max tmp;
	  slot->get_vrange (tmp, type);
	  if (!case_range.union_ (tmp))
	   continue;
	  if (slot->fits_p (case_range))
	    {
	      slot->set_vrange (case_range);
	      continue;
	    }
	}
      // If there was an existing range and it doesn't fit, we lose the memory.
      // It'll get reclaimed when the obstack is freed.  This seems less
      // intrusive than allocating max ranges for each case.
      slot = m_range_allocator->clone (case_range);
    }

  vrange_storage *&slot = m_edge_table->get_or_insert (default_edge, &existed);
  // This should be the first call into this switch.
  gcc_checking_assert (!existed);
  slot = m_range_allocator->clone (default_range);
}


// Calculate the range forced on on edge E by control flow, return it
// in R.  Return the statement which defines the range, otherwise
// return NULL

gimple *
gimple_outgoing_range::edge_range_p (irange &r, edge e)
{
  if (single_succ_p (e->src))
    return NULL;

  // Determine if there is an outgoing edge.
  gimple *s = gimple_outgoing_range_stmt_p (e->src);
  if (!s)
    return NULL;

  if (is_a<gcond *> (s))
    {
      gcond_edge_range (r, e);
      return s;
    }

  // Only process switches if it within the size limit.
  if (m_max_edges == 0 || (EDGE_COUNT (e->src->succs) > (unsigned)m_max_edges))
    return NULL;

  gcc_checking_assert (is_a<gswitch *> (s));
  gswitch *sw = as_a<gswitch *> (s);

  // Switches can only be integers.
  if (switch_edge_range (as_a <irange> (r), sw, e))
    return s;

  return NULL;
}
