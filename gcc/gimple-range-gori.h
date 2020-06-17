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

/* RANGE_DEF_CHAIN is used to determine what SSA names in a block can
   have range information calculated for them, and what the
   dependencies on each other are.

   Information for a basic block is calculated once and stored.  It is
   only calculated the first time a query is made, so if no queries
   are made, there is little overhead.

   The def_chain bitmap is indexed by SSA_NAME_VERSION.  Bits are set
   within this bitmap to indicate SSA names that are defined in the
   SAME block and used to calculate this SSA name.

   One import is maintained per def-chain.  An IMPORT is defined as an
   SSA name in the def chain which occurs outside the basic block. A
   change in the value of this SSA name can change the value of any
   name in the chain.

   If there is more than one import, or an ssa_name originates WITHIN
   the same basic block, but is defined by a statement that the range
   engine does not know how to calculate, then there is no import for
   the entire chain.

    <bb 2> :
      _1 = x_4(D) + -2;
      _2 = _1 * 4;
      j_7 = foo ();
      q_5 = _2 + 3;
      if (q_5 <= 13)

    _1  : (import : x_4(D))  :x_4(D)
    _2  : (import : x_4(D))  :_1  x_4(D)
    q_5  : (import : x_4(D))  :_1  _2  x_4(D)

    This dump indicates the bits set in the def_chain vector and their
    import, as well as demonstrates the def_chain bits for the related
    ssa_names.

    Checking the chain for _2 indicates that _1 and x_4 are used in
    its evaluation, and with x_4 being an import.

    For the purpose of defining an import, PHI node defintions are
    considered imports as they don't really reside in the block, but
    are accumulators of values from incoming edges.

    Def chains also only include statements which are valid gimple
    so a def chain will only span statements for which the range
    engine implements operations for.  */


class range_def_chain
{
public:
  range_def_chain ();
  ~range_def_chain ();
  tree terminal_name (tree name);
  bool has_def_chain (tree name);
  bitmap get_def_chain (tree name);
  bool in_chain_p (tree name, tree def);
private:
  vec<bitmap> m_def_chain;	// SSA_NAME : def chain components.
  vec<tree> m_terminal;	        // SSA_NAME : chain terminal name.
  tree build_def_chain (tree name, bitmap result, basic_block bb);
};


/* GORI_MAP is used to accumulate what SSA names in a block can
   generate range information, and provides tools for the block ranger
   to enable it to efficiently calculate these ranges.

   GORI stands for "Generates Outgoing Range Information."

   It utilizes the range_def_chain class to contruct def_chains.
   Information for a basic block is calculated once and stored.  It is
   only calculated the first time a query is made.  If no queries are
   made, there is little overhead.

   2 bitmaps are maintained for each basic block:

   m_outgoing  : a set bit indicates a range can be generated for a name.
   m_incoming  : a set bit means a this name come from outside the
	         block and is used in the calculation of some outgoing
	         range.

   Generally speaking, the m_outgoing vector is the union of the
   entire def_chain of all SSA names used in the last statement of the
   block which generate ranges.  The m_incoming vector is the union of
   all the terminal names of those def chains.  They act as a one-stop
   summary for the block.  */

class gori_map : public range_def_chain
{
public:
  gori_map ();
  ~gori_map ();

  bool is_export_p (tree name, basic_block bb);
  bool def_chain_in_export_p (tree name, basic_block bb);
  bool is_import_p (tree name, basic_block bb);

  void dump (FILE *f);
  void dump (FILE *f, basic_block bb);
private:
  bitmap_obstack m_bitmaps;
  vec<bitmap> m_outgoing;	// BB: Outgoing ranges calculatable on edges
  vec<bitmap> m_incoming;	// BB: block imports
  void maybe_add_gori (tree name, basic_block bb);
  void calculate_gori (basic_block bb);
  bitmap imports (basic_block bb);
public:
  // FIXME: Temporarily set as public.
  bitmap exports (basic_block bb);
};

// Generic object to return a range for an SSA.
class range_store
{
public:
  virtual bool range_of_expr (irange &r, tree expr, gimple *stmt = NULL) = 0;
  virtual const class value_range_equiv *get_value_range (const_tree expr,
							  gimple *stmt = NULL);
};

// This class utilizes a GORI map to determine which SSA_NAMES can
// have ranges calculated for them on outgoing edges from basic
// blocks.

class gori_compute : public range_store
{
public:
  gori_compute ();
  /*  Destructor is virtual to silence:

      warning: deleting object of polymorphic class type ‘vr_values’
      which has non-virtual destructor might cause undefined
      behavior.  */
  virtual ~gori_compute ();
  virtual bool range_of_expr (irange &r, tree expr, gimple *stmt = NULL);
  virtual bool outgoing_edge_range_p (irange &r, edge e, tree name,
				      const irange *name_range = NULL);
protected:
  virtual void range_of_ssa_name (irange &r, tree name, gimple *stmt = NULL);
  virtual bool compute_operand_range (irange &r, gimple *stmt,
				      const irange &lhs,
				      tree name,
				      const irange *name_range = NULL);
  bool has_edge_range_p (edge e, tree name);
  virtual bool compute_logical_operands (irange &r, gimple *stmt,
					 const irange &lhs,
					 tree name, const irange *name_range);
  void compute_logical_operands_in_chain (class tf_range &range,
					  gimple *stmt, const irange &lhs,
					  tree name,
					  const irange *name_range,
					  tree op, bool op_in_chain);
  bool optimize_logical_operands (tf_range &range,
				  gimple *stmt, const irange &lhs,
				  tree name, const irange *name_range,
				  tree op);

  bool logical_combine (irange &r, enum tree_code code,
			const irange &lhs,
			const class tf_range &op1_range,
			const class tf_range &op2_range);
  int_range<1> m_bool_zero;           // Boolean false cached.
  int_range<1> m_bool_one;            // Boolean true cached.

  gori_map m_gori_map;
private:
  void get_tree_range (irange &, tree expr, tree name,
		       const irange *range_of_name);
  bool compute_operand_range_switch (irange &r, gswitch *stmt,
				     const irange &lhs,
				     tree name, const irange *name_range);
  bool compute_name_range_op (irange &r, gimple *stmt,
			      const irange &lhs,
			      tree name, const irange *name_range);
  bool compute_operand1_range (irange &r, gimple *stmt,
			       const irange &lhs,
			       tree name, const irange *name_range);
  bool compute_operand2_range (irange &r, gimple *stmt,
			       const irange &lhs,
			       tree name, const irange *name_range);
  bool compute_operand1_and_operand2_range
				(irange &r, gimple *stmt,
				 const irange &lhs,
				 tree name, const irange *name_range);
};

class gori_compute_cache : public gori_compute
{
public:
  gori_compute_cache ();
  ~gori_compute_cache ();
protected:
  virtual bool compute_operand_range (irange &r, gimple *stmt,
				      const irange &lhs,
				      tree name,
				      const irange *name_range = NULL);
private:
  void cache_comparison (gimple *);
  void cache_comparison_with_int (gimple *, enum tree_code,
				  tree op1, tree op2);
  void cache_comparison_with_ssa (gimple *, enum tree_code,
				  tree op1, tree op2);
  typedef gori_compute super;
  class logical_stmt_cache *m_cache;
};

class trace_gori_compute : public gori_compute_cache
{
public:
  trace_gori_compute ();
  virtual bool range_of_expr (irange &r, tree expr, gimple *stmt = NULL);
  virtual bool outgoing_edge_range_p (irange &r, edge e, tree name,
				      const irange *name_range = NULL);
protected:
  virtual void range_of_ssa_name (irange &r, tree name, gimple *stmt = NULL);
  virtual bool compute_operand_range (irange &r, gimple *stmt,
				      const irange &lhs,
				      tree name,
				      const irange *name_range = NULL);
  virtual bool compute_logical_operands (irange &r, gimple *stmt,
					 const irange &lhs,
					 tree name, const irange *name_range);
private:
  typedef gori_compute_cache super;
protected:
  static const unsigned bump = 2;
  unsigned indent;
  unsigned trace_count;		// Current trace index count.

  bool dumping (unsigned counter, bool trailing = false);
  bool trailer (unsigned counter, const char *caller, bool result, tree name,
		const irange &r);
};

#endif // GCC_GIMPLE_RANGE_GORI_H
