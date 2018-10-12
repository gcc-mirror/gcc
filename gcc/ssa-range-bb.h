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

#include "gimple-range.h"

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
public:
  block_ranger ();
  ~block_ranger ();

  /* True if NAME Generates range info on one or more outgoing edges of BB.  */
  bool range_p (basic_block bb, tree name);
  /* What is the static calculated range of NAME on outgoing edge E.  */
  bool range_on_edge (irange& r, tree name, edge e);
  /* Evaluate statement G assuming entry only via edge E */
  bool range_of_stmt (irange& r, gimple *g, edge e);
  tree single_import (tree name);
  void dump (FILE *f);
  void exercise (FILE *f);
protected:
  virtual bool get_operand_range (irange& r, tree op, gimple *s);
private:
  class gori_map *m_gori; 	/* Generates Outgoing Range Info.  */
  irange m_bool_zero;		/* Bolean zero cached.  */
  irange m_bool_one;		/* Bolean true cached.  */
  bool process_logical (glogical *s, irange& r, tree name,
			const irange& lhs);
  bool get_range_from_stmt (gimple *s, irange& r, tree name,
			    const irange& lhs);
};

#endif /* GCC_SSA_RANGE_BB_H */
