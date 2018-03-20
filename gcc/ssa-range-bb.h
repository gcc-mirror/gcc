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

#include "range.h"
#include "range-op.h"
#include "ssa-range-stmt.h"

/* This is the primary interface class for the range generator at the basic
   block level. It allows the client to query a range for an ssa-name within
   a basic block, either on an outgoing edge, or on an individual statement.  */

class block_ranger
{
  class gori_map *gori; 	/* Generates Outgoing Range Info.  */
  irange bool_zero;
  irange bool_one;
  bool logical_expr_p (tree_code code, tree type) const;
  bool eval_logical (irange& r, range_stmt &stmt, const irange& lhs,
		     const irange& op1_true, const irange& op1_false,
		     const irange& op2_true, const irange& op2_false) const;
  bool process_logical (range_stmt& stmt, irange& r, tree name,
			const irange& lhs);
  bool get_range (range_stmt& stmt, irange& r, tree name, const irange& lhs);
  bool get_range_from_stmt (gimple *stmt, irange& r, tree name,
			    const irange& lhs);
public:
  block_ranger ();
  ~block_ranger ();

  /* True if NAME Generates range info on one or more outgoing edges of BB.  */
  bool range_p (basic_block bb, tree name);
  /* What is the static calculated range of NAME on outgoing edge E.  */
  bool range_on_edge (irange& r, tree name, edge e);
  /* What range does NAME have on entry to statement g.  */
  bool range_on_stmt (irange& r, tree name, gimple *g);
  /* What infomation does stmt g provide about the lhs.  */
  bool range_of_def (irange& r, gimple *g);
  /* What does g provide about the lhs if NAME has RANGE_FOR_NAME.  */
  bool range_of_def (irange& r, gimple *g, tree name,
		     const irange& range_for_name);

  tree single_import (tree name);
  void dump (FILE *f);
  void exercise (FILE *f);
};

#endif /* GCC_SSA_RANGE_BB_H */
