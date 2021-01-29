/* Header file for gimple range GORI structures.
   Copyright (C) 2017-2021 Free Software Foundation, Inc.
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

#ifndef GCC_GIMPLE_RANGE_GORI_H
#define GCC_GIMPLE_RANGE_GORI_H


// This class is used to determine which SSA_NAMES can have ranges
// calculated for them on outgoing edges from basic blocks.  This represents
// ONLY the effect of the basic block edge->src on a range.
//
// There are 2 primary entry points:
//
// has_edge_range_p (tree name, edge e)
//   returns true if the outgoing edge *may* be able to produce range
//   information for ssa_name NAME on edge E.
//   FALSE is returned if this edge does not affect the range of NAME.
//   if no edge is specified, return TRUE if name may have a value calculated
//   on *ANY* edge that has been seen.  FALSE indicates that the global value
//   is applicable everywhere that has been processed.
//
// outgoing_edge_range_p (irange &range, edge e, tree name)
//   Actually does the calculation of RANGE for name on E
//   This represents application of whatever static range effect edge E
//   may have on NAME, not any cumulative effect.

// There are also some internal APIs
//
// ssa_range_in_bb ()  is an internal routine which is used to start any
// calculation chain using SSA_NAMES which come from outside the block. ie
//      a_2 = b_4 - 8
//      if (a_2 < 30)
// on the true edge, a_2 is known to be [0, 29]
// b_4 can be calculated as [8, 37]
// during this calculation, b_4 is considered an "import" and ssa_range_in_bb
// is queried for a starting range which is used in the calculation.
// A default value of VARYING provides the raw static info for the edge.
//
// If there is any known range for b_4 coming into this block, it can refine
// the results.  This allows for cascading results to be propogated.
// if b_4 is [100, 200] on entry to the block, feeds into the calculation
// of a_2 = [92, 192], and finally on the true edge the range would be 
// an empty range [] because it is not possible for the true edge to be taken.
//
// expr_range_in_bb is simply a wrapper which calls ssa_range_in_bb for 
// SSA_NAMES and otherwise simply calculates the range of the expression.
//
// The remaining routines are internal use only.

class gori_compute 
{
public:
  gori_compute ();
  ~gori_compute ();
  bool outgoing_edge_range_p (irange &r, edge e, tree name);
  bool has_edge_range_p (tree name, edge e = NULL);
  void set_range_invariant (tree name);
  void dump (FILE *f);
protected:
  virtual void ssa_range_in_bb (irange &r, tree name, basic_block bb);
  virtual bool compute_operand_range (irange &r, gimple *stmt,
				      const irange &lhs, tree name);

  void expr_range_in_bb (irange &r, tree expr, basic_block bb);
  bool compute_logical_operands (irange &r, gimple *stmt,
				 const irange &lhs,
				 tree name);
  void compute_logical_operands_in_chain (class tf_range &range,
					  gimple *stmt, const irange &lhs,
					  tree name, tree op,
					  bool op_in_chain);
  bool optimize_logical_operands (tf_range &range, gimple *stmt,
				  const irange &lhs, tree name, tree op);
  bool logical_combine (irange &r, enum tree_code code, const irange &lhs,
			const class tf_range &op1_range,
			const class tf_range &op2_range);
  int_range<2> m_bool_zero;           // Boolean false cached.
  int_range<2> m_bool_one;            // Boolean true cached.

private:
  bool compute_operand_range_switch (irange &r, gswitch *stmt,
				     const irange &lhs, tree name);
  bool compute_name_range_op (irange &r, gimple *stmt, const irange &lhs,
			      tree name);
  bool compute_operand1_range (irange &r, gimple *stmt, const irange &lhs,
			       tree name);
  bool compute_operand2_range (irange &r, gimple *stmt, const irange &lhs,
			       tree name);
  bool compute_operand1_and_operand2_range (irange &r, gimple *stmt,
					    const irange &lhs, tree name);

  class gori_map *m_gori_map;
  outgoing_range outgoing;	// Edge values for COND_EXPR & SWITCH_EXPR.
};


// This class adds a cache to gori_computes for logical expressions.
//       bool result = x && y
// requires calcuation of both X and Y for both true and false results.
// There are 4 combinations [0,0][0,0] [0,0][1,1] [1,1][0,0] and [1,1][1,1].
// Note that each pair of possible results for X and Y are used twice, and
// the calcuation of those results are the same each time.
//
// The cache simply checks if a stmt is cachable, and if so, saves both the
// true and false results for the next time the query is made.
//
// This is used to speed up long chains of logical operations which
// quickly become exponential.

class gori_compute_cache : public gori_compute
{
public:
  gori_compute_cache ();
  ~gori_compute_cache ();
protected:
  virtual bool compute_operand_range (irange &r, gimple *stmt,
				      const irange &lhs, tree name);
private:
  void cache_stmt (gimple *);
  typedef gori_compute super;
  class logical_stmt_cache *m_cache;
};

#endif // GCC_GIMPLE_RANGE_GORI_H
