/* Header file for the gimple ranger.
   Copyright (C) 2017-2020 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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

#ifndef GCC_GIMPLE_RANGER_H
#define GCC_GIMPLE_RANGER_H

#include "gimple-range-stmt.h"
#include "gimple-range-gori.h"
#include "gimple-range-cfg.h"
#include "gimple-range-cache.h"


// This is the basic range generator interface.
//
// This base class provides all the API entry points, but only provides
// functionality at the statement level.  Ie, it can calculate ranges on
// statements, but does no additonal lookup.
//
// All the range_of_* methods will return a range if the types is
// supported by the range engine.  It may be the full range for the
// type, AKA varying_p or it may be a refined range.  If the range
// type is not supported, then false is returned.  Non-statement
// related methods return whatever the current global value is.

class global_ranger : public gimple_ranger
{
public:
  global_ranger ();
  ~global_ranger ();
  virtual void range_on_entry (irange &r, basic_block bb, tree name);
  virtual void range_on_exit (irange &r, basic_block bb, tree name);
  virtual bool range_of_stmt (irange &r, gimple *s, tree name = NULL_TREE);
  virtual void range_on_edge (irange &r, edge e, tree name);

  void export_global_ranges ();

  void dump (FILE *f);
  void calculate_and_dump (FILE *f);
protected:
  virtual void range_of_ssa_name (irange &r, tree name, gimple *s = NULL);
  bool range_from_import (irange &r, tree name, irange &import_range);
  ssa_global_cache m_globals;
private:
  typedef gimple_ranger super;
  bool non_null_deref_p (tree name, basic_block bb);
  bool block_range (irange &r, basic_block bb, tree name, bool calc = true);
  void dump_block (FILE *f, basic_block bb);

  void add_to_update (basic_block bb);
  bool edge_range (irange &r, edge e, tree name);
  void fill_block_cache (tree name, basic_block bb, basic_block def_bb);
  void iterative_cache_update (tree name);

  block_range_cache m_on_entry;
  non_null_ref m_non_null;
  vec<basic_block> m_workback;
  vec<basic_block> m_update_list;
};


// A global ranger that uses SCEV/loop (if available) to refine PHI results.

class loop_ranger : public global_ranger
{
public:
  loop_ranger ();
  ~loop_ranger ();
  virtual void range_on_edge (irange &r, edge e, tree name);
  virtual bool range_of_phi (irange &r, gphi *phi);
  virtual bool range_of_stmt (irange &r, gimple *stmt, tree name);

private:
  typedef global_ranger super;
  bool range_with_loop_info (irange &r, tree name);
  void range_of_ssa_name_with_loop_info (irange &, tree, class loop *,
					 gphi *);

  class vr_values *m_vr_values;
};

class trace_ranger : public loop_ranger
{
public:
  trace_ranger();

  virtual bool range_of_stmt (irange &r, gimple *s, tree name = NULL_TREE);
  virtual void range_on_edge (irange &r, edge e, tree name);
  virtual void range_on_entry (irange &r, basic_block bb, tree name);
  virtual void range_on_exit (irange &r, basic_block bb, tree name);

  // Calculate a range on edge E only if it is defined by E.
  virtual bool outgoing_edge_range_p (irange &r, edge e, tree name,
				      const irange *name_range = NULL);
protected:
  virtual void range_of_ssa_name (irange &r, tree name, gimple *s = NULL);
private:
  typedef loop_ranger super;
  static const unsigned bump = 2;
  unsigned indent;
  unsigned trace_count;		// Current trace index count.

  bool dumping (unsigned counter, bool trailing = false);
  bool trailer (unsigned counter, const char *caller, bool result, tree name,
		const irange &r);
};



// Like global_ranger::range_of_expr (), but make an on-the-fly
// ranger.  If SSA, as seen from STMT, has a known range, set it in R
// and return TRUE.
//
// NOTE: There is overhead involved with this function, so it should
// only be used for lightweight queries.  It is mostly meant for range
// queries that don't need caching in subsequent calls.

static inline bool
on_demand_get_range_on_stmt (irange &r, tree ssa, gimple *stmt)
{
  if (!cfun->cfg)
    return false;
  loop_ranger ranger;
  bool ret;
  ret = ranger.range_of_expr (r, ssa, stmt);
  if (ret && r.varying_p ())
    return false;
  return ret;
}
#endif // GCC_GIMPLE_RANGER_H
