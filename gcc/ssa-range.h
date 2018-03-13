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

class ssa_block_ranges
{
private:
  vec<irange_storage *> tab;
  irange_storage *type_range;
  const_tree type;
public:
  ssa_block_ranges (tree t);
  ~ssa_block_ranges ();

  void set_bb_range (const basic_block bb, const irange &r);
  void set_bb_range_for_type (const basic_block bb);
  bool get_bb_range (irange& r, const basic_block bb);
  bool bb_range_p (const basic_block bb);

  void dump(FILE *f);
};


class block_range_cache
{
private:
  vec<ssa_block_ranges *> ssa_ranges;
public:
  block_range_cache ();
  ~block_range_cache ();
  ssa_block_ranges& operator[] (tree name);

  void dump (FILE *f);
};

/* This class utilizes the basic block GORI map and is used to query the range
   of SSA_NAMEs across multiple basic blocks and edges.  */
class path_ranger : public block_ranger
{
private:
  block_range_cache block_cache;

  void range_for_bb (irange &r, tree name, basic_block bb, basic_block def_bb);
  void determine_block (tree name, basic_block bb, basic_block def_bb);
  bool path_range_reverse (irange &r, tree name, const vec<basic_block> &);
  bool path_fold_stmt (irange &r, range_stmt &rn, basic_block bb,
		       edge e = NULL);
public:
  enum path_range_direction { FORWARD, REVERSE };
  path_ranger ();

  /* What is the known range of name from its DEF point to edge E.  */
  bool path_range_edge (irange& r, tree name, edge e);
  bool path_range_entry (irange& r, tree name, basic_block bb);
  bool path_range_stmt (irange& r, tree name, gimple *g);
  bool path_range (irange &r, tree name, const vec<basic_block> &bbs,
		   enum path_range_direction, edge start_edge = NULL);
  // Evaluate expression within a BB as much as possible.
  bool path_range_of_def (irange& r, gimple *g);
  bool path_range_of_def (irange& r, gimple *g, edge e);

  void dump (FILE *f);
  void exercise (FILE *f);   /* do a full mapping pass, dump if provided.  */
};

#endif /* GCC_SSA_RANGE_H */
