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

#ifndef GCC_SSA_RANGE_BB_H
#define GCC_SSA_RANGE_BB_H

#include "ssa-range-stmt.h"

/* This is the primary interface class for the range generator at the basic
   block level. It allows the client to query a range for an ssa-name within
   a basic block, either on an outgoing edge, or on an individual statement. 
   
   It is lightweight to declare, but for each basic block that is queried, it
   will scan some or all of the statements in the block to determine what
   ssa-names can have range information generated for them.  THis will
   prevent constantly rescanning a block.
   
   THis is primarily of use to the path_ranger found in ssa-range.[ch] which
   builds on top of this to find ranges across the CFG.  */

class block_ranger
{
  class gori_map *gori; 	/* Generates Outgoing Range Info.  */
  irange bool_zero;		/* Bolean zero cached.  */
  irange bool_one;		/* Bolean true cached.  */
  bool process_logical (range_stmt stmt, irange& r, tree name,
			const irange& lhs);
  bool get_range_from_stmt (range_stmt stmt, irange& r, tree name,
			    const irange& lhs);
protected:
  virtual bool get_operand_range (irange& r, tree op, gimple *s = NULL);
  gimple *ssa_name_same_bb_p (tree name, basic_block bb);
public:
  block_ranger ();
  ~block_ranger ();

  /* True if NAME Generates range info on one or more outgoing edges of BB.  */
  bool range_p (basic_block bb, tree name);
  /* What is the static calculated range of NAME on outgoing edge E.  */
  bool range_on_edge (irange& r, tree name, edge e);
  /* Evaluate statement G.  */
  bool range_of_stmt (irange& r, gimple *g);
  /* Evaluate statement G assuming entry only via edge E */
  bool range_of_stmt (irange& r, gimple *g, edge e);
  /* Evaluate statement G if NAME has RANGE_FOR_NAME.  */
  bool range_of_stmt (irange& r, gimple *g, tree name,
		     const irange& range_for_name);
  tree single_import (tree name);
  void dump (FILE *f);
  void exercise (FILE *f);
};


// If NAME is defined in block BB, return the gimple statement pointer, 
// otherwise return NULL>
inline gimple *
block_ranger::ssa_name_same_bb_p (tree name, basic_block bb)
{
  gimple *g = SSA_NAME_DEF_STMT (name);
  if (!g || gimple_bb (g) != bb)
   return NULL;
  return g;
}

#endif /* GCC_SSA_RANGE_BB_H */
