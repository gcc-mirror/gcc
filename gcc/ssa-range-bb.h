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

class block_ranger : public gimple_range
{
public:
  block_ranger ();
  ~block_ranger ();

  // If a range for name is defined by edge E, return it.
  virtual bool range_on_edge_p (irange &r, edge e, tree name);
  // Evaluate the range for name on stmt S if the lhs has range LHS.
  bool compute_operand_range (irange &r, gimple *s, tree name,
			      const irange &lhs);

  bool range_p (basic_block bb, tree name);
  tree single_import (tree name);
  void dump (FILE *f);
  void exercise (FILE *f);
private:
  class gori_map *m_gori; 	/* Generates Outgoing Range Info.  */
  irange m_bool_zero;		/* Bolean zero cached.  */
  irange m_bool_one;		/* Bolean true cached.  */
  bool process_logical (glogical *s, irange &r, tree name,
			const irange &lhs);
  bool get_range_thru_op1 (grange_op *s, irange &r, tree name,
			   const irange &lhs);
  bool get_range_thru_op2 (grange_op *s, irange &r, tree name,
			   const irange &lhs);
  bool get_range_thru_op1_and_op2 (grange_op *s, irange &r, tree name,
				   const irange &lhs);
};

#endif /* GCC_SSA_RANGE_BB_H */
