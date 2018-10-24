/* Header file for ssa-range generator.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
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

#ifndef GCC_SSA_RANGE_H
#define GCC_SSA_RANGE_H

#include "ssa-range-bb.h"

// This class utilizes the basic block GORI map and is used to query the range
// of SSA_NAMEs across multiple basic blocks and edges.  It builds a cache
// of range on entry to blocks.  ALL work is done on-demand so it is relatively
// lightweight until used.
// 
// There is a global ssa-name table implemented within path_ranger via 
// a set of private global_ssa_name routines.  These are here until such
// time that there is a global irange table for real.  

class path_ranger : public block_ranger
{
public:
  path_ranger ();
  ~path_ranger ();

  virtual bool range_of_expr (irange &r, tree op, gimple *s = NULL);
  virtual bool range_on_entry (irange &r, basic_block bb, tree name);

  virtual bool range_of_call (irange &r, gcall *call);
  virtual bool range_of_phi (irange &r, gphi *phi);
  virtual bool range_of_range_op  (irange &r, grange_op *s);

  void dump (FILE *f);
  void exercise (FILE *f);   /* do a full mapping pass, dump if provided.  */

private:
  void dump_global_ssa_range (FILE *f);
  bool has_global_ssa_range (irange &r, tree name);
  bool get_global_ssa_range (irange &r, tree name);
  void set_global_ssa_range (tree name, const irange&r);
  void clear_global_ssa_range (tree name);
  bool adjust_global_value (bool valid, irange &r, tree name);
  bool check_global_value (irange &r, tree name);

  class block_range_cache *m_block_cache;
  class ssa_global_cache *m_globals;
  class non_null_ref *m_non_null;
  bool non_null_deref_in_block (irange &r, tree name, basic_block bb);
  void fill_block_cache (tree name, basic_block bb, basic_block def_bb);
};


// Like path_ranger::path_range_on_stmt(), but make an on-the-fly ranger.
// Return TRUE if SSA as seen from within STMT has a known range the is not
// varying.  Set this range in R.
//
// NOTE: There is a small overhead involved with this function, so it
// should only be used for very lightweight or unrelated range
// queries.  This function is mostly meant for range queries that
// don't need caching in subsequent calls.  */

static inline bool
on_demand_get_range_on_stmt (irange &r, tree ssa, gimple *stmt)
{
  path_ranger ranger;
  bool ret;
  ret = ranger.range_of_expr (r, ssa, stmt);
  if (ret && r.varying_p ())
    return false;
  return ret;
}

#endif /* GCC_SSA_RANGE_H */
