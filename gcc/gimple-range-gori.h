/* Header file for gimple range GORI structures.
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

#ifndef GCC_GIMPLE_RANGE_GORI_H
#define GCC_GIMPLE_RANGE_GORI_H


// This class utilizes a GORI map to determine which SSA_NAMES can
// have ranges calculated for them on outgoing edges from basic
// blocks.

class gori_compute 
{
public:
  gori_compute ();
  ~gori_compute ();
  bool outgoing_edge_range_p (irange &r, edge e, tree name);
  bool has_edge_range_p (edge e, tree name);
  void dump (FILE *f);
protected:
  virtual void ssa_range_in_bb (irange &r, tree name, basic_block bb) = 0;
  virtual bool compute_operand_range (irange &r, gimple *stmt,
				      const irange &lhs, tree name);

  void expr_range_in_bb (irange &r, tree expr, basic_block bb);
  bool compute_logical_operands (irange &r, gimple *stmt,
				 const irange &lhs,
				 tree name);
  void compute_logical_operands_in_chain (class tf_range &range,
					  gimple *stmt, const irange &lhs,
					  tree name, tree op, bool op_in_chain);
  bool optimize_logical_operands (tf_range &range, gimple *stmt,
				  const irange &lhs, tree name, tree op);

  bool logical_combine (irange &r, enum tree_code code, const irange &lhs,
			const class tf_range &op1_range,
			const class tf_range &op2_range);
  int_range<1> m_bool_zero;           // Boolean false cached.
  int_range<1> m_bool_one;            // Boolean true cached.

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
};

class gori_compute_cache : public gori_compute
{
public:
  gori_compute_cache ();
  ~gori_compute_cache ();
protected:
  virtual bool compute_operand_range (irange &r, gimple *stmt,
				      const irange &lhs, tree name);
private:
  void cache_comparison (gimple *);
  void cache_comparison_with_int (gimple *, enum tree_code,
				  tree op1, tree op2);
  void cache_comparison_with_ssa (gimple *, enum tree_code,
				  tree op1, tree op2);
  typedef gori_compute super;
  class logical_stmt_cache *m_cache;
};

#endif // GCC_GIMPLE_RANGE_GORI_H
