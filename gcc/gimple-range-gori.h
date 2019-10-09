/* Header file for GIMPLE range GORI structures.
   Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

#ifndef GCC_GIMPLE_RANGE_GORI_H
#define GCC_GIMPLE_RANGE_GORI_H

/* RANGE_DEF_CHAIN is used to determine what ssa-names in a block can have range
   information calculated for them, and what the dependencies on each other are.

   Information for a basic block is calculated once and stored.  It is only
   calculated the first time a query is made, so if no queries are made, there
   is little overhead.
   
   The def_chain bitmap is indexed by ssa_name version.  Bits are set within
   this bitmap to indicate ssa_names that are defined in the SAME block and used
   to calculate this ssa_name.

   One import is maintained per def-chain.  An IMPORT is defined as an ssa-name
   in the def chain which occurs outside the basic block. A change in the value
   of this ssa-name can change the value of any name in the chain. 

   If there is more than one import, or an ssa-Name originates WITHIN the same 
   basic block but is defined by a statement that the range engine does not
   know how to calculate, then there is no import for the entire chain.

    <bb 2> :
      _1 = x_4(D) + -2;
      _2 = _1 * 4;
      j_7 = foo ();
      q_5 = _2 + 3;
      if (q_5 <= 13)

    _1  : (import : x_4(D))  :x_4(D)
    _2  : (import : x_4(D))  :_1  x_4(D)
    q_5  : (import : x_4(D))  :_1  _2  x_4(D)

    This dump indicates the bits set in the def_chain vector and ther import, as
    well as demonstrates the def_chain bits for the related ssa_names.

    Checking the chain for _2 indicates that _1 and x_4 are used in its
    evaluation, and with x_4 being an import.

    For the purpose of defining an import, PHI node defintions  are considered
    imports as the dont really reside in the block, but rather are
    accumulators of values from incoming edges.
    
    Def chains also only include statements which are valid gimple's so
    a def chain will only span statements for which the range engine
    implements operations.  */


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


/* GORI_MAP is used to accumulate what ssa-names in a block can generate range
   information, and provides tools for the block ranger to enable it to
   efficiently calculate these ranges.
   GORI stands for "Generates Outgoing Range Information."

   It utilizes the range_def_chain class to contruct def_chains.
   information for a basic block is calculated once and stored. It is only
   calculated the first time a query is made, so if no queries are made, there
   is little overhead.
   
   2 bitmaps are maintained for each basic block:
   m_outgoing  : a set bit indicates a range can be generated for a name.
   m_incoming  : a set bit means a this name come from outside the block and is
	       used in the calculation of some outgoing range.
   
   Generally speaking, the m_outgoing vector is the union of the entire
   def_chain of all ssa-names used in the last statement of the block which
   generate ranges.  The m_incoming vector is the union of all the terminal
   names of those def chains.  They act as a one-stop summary for the block.  */

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
  vec<bitmap> m_outgoing;	// BB: Outgoing ranges generated.
  vec<bitmap> m_incoming;	// BB: ranges coming in.
  void maybe_add_gori (tree name, basic_block bb);
  void calculate_gori (basic_block bb);
  bitmap imports (basic_block bb);
  bitmap exports (basic_block bb);
};

// This class utilizes a GORI map to determine which SSA_NAMES can have ranges
// calculated for them on outgoing edges from basic blocks.

class gori_compute 
{
public:
  gori_compute ();
  ~gori_compute ();
  bool range_of_expr (value_range_base &r, tree expr, gimple *s = NULL);
  virtual bool outgoing_edge_range_p (value_range_base &r, edge e, tree name,
				      value_range_base *name_range = NULL);
protected:
  virtual void range_of_ssa_name (value_range_base &, tree name,
				  gimple *s = NULL);
  bool has_edge_range_p (edge e, tree name);
  gori_map m_gori_map;
private:
  value_range_base get_tree_range (tree expr, tree name,
				   value_range_base *range_of_name);
  bool compute_operand_range (value_range_base &r, gimple *s,
			      const value_range_base &lhs,
			      tree name, value_range_base *name_range = NULL);
  bool compute_operand_range_switch (value_range_base &r, gswitch *s,
				     const value_range_base &lhs,
				     tree name, value_range_base *name_range);
  bool compute_name_range_op (value_range_base &r, gimple *s,
			      const value_range_base &lhs,
			      tree name, value_range_base *name_range);
  bool compute_operand_range_op (value_range_base &r, gimple *stmt,
				 const value_range_base &lhs,
				 tree name, value_range_base *name_range);
  bool compute_operand1_range (value_range_base &r, gimple *s,
			       const value_range_base &lhs,
			       tree name, value_range_base *name_range);
  bool compute_operand2_range (value_range_base &r, gimple *s,
			       const value_range_base &lhs,
			       tree name, value_range_base *name_range);
  bool compute_operand1_and_operand2_range (value_range_base &r, gimple *s, 
					    const value_range_base &lhs,
					    tree name,
					    value_range_base *name_range);
  bool compute_logical_operands (value_range_base &r, gimple *s,
				 const value_range_base &lhs,
				 tree name, value_range_base *name_range);
  bool logical_combine (value_range_base &r, enum tree_code code,
			const value_range_base &lhs,
		        const value_range_base &op1_true,
			const value_range_base &op1_false,
		        const value_range_base &op2_true,
			const value_range_base &op2_false);
  value_range_base m_bool_zero;           /* Boolean zero cached.  */
  value_range_base m_bool_one;            /* Boolean true cached.  */
};

#endif // GCC_GIMPLE_RANGE_GORI_H
