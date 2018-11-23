/* Header file for SSA range interface.
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

#include "range.h"
#include "range-op.h"

extern gimple *gimple_outgoing_range_stmt_p (basic_block bb);
extern gimple *gimple_outgoing_edge_range_p (irange &r, edge e);
extern bool get_tree_range (irange &r, tree expr);

// This is the basic range generator interface. 
//
// This base class provides all the API entry points, but only provides 
// functionality at the statement level.  Ie, it can calculate ranges on 
// statements, but does no additonal lookup.
//
// ALL the range_of_* methods will return a range if the types is supported
// by the range engine. It may be the full range for the type, AKA varying_p
// or it may be a refined range.
//
// range_of_expr simply picks whatever is in GCC's global range table, or
//               calculates a constant range as approriate.
// range_on_edge will only provide a range if name occurs on the
//               last stmt in the predecessor edge
// range_of_stmt will calculate a result using the global range table.
//               (via range_of_expr)
// range_on_entry calculates the union of ranges on incoming edges.
// 
// outgoing_edge_range_p will only return a range if the edge specified 
// defines a range for the specified name.  Otherwise false is returned.
//
// If the object is created with the support_calculations flag set to TRUE,
// it enables building defintion chains which will allow the calculation
// of ssa-names found within these chains via class gori_compute.
// 
// It is lightweight to declare in this case, but for each basic block that is
// queried, it will scan some or all of the statements in the block to
// determine what ssa-names can have range information generated for them and
// cache this information.  
//
// termninal_name () provides the "input" name to the chain of statements for
// name that can change the calculation of its range.  This is always outside 
// the current basic block.  ie, in the above example, b_6 is the
// "terminal_name" returned for both a_2 and c_3 since a change in the
// range of b_6 to calculate their results may produce a different range.
 
class ssa_range
{
  public:
  ssa_range (bool support_calculations);
  ~ssa_range ();

  static bool supports_type_p (tree type);
  static bool supports_ssa_p (tree ssa);
  static bool supports_const_p (tree c);
  static bool supports_p (tree expr);
  static tree valid_ssa_p (tree exp);

  virtual bool range_of_expr (irange &r, tree expr, gimple *s = NULL);
  virtual bool range_of_expr (irange &r, tree expr, edge e);
  virtual bool range_on_edge (irange &r, edge e, tree name);
  virtual bool range_on_entry (irange &r, basic_block bb, tree name);
  virtual bool range_on_exit (irange &r, basic_block bb, tree name);
  virtual bool range_of_stmt (irange &r, gimple *s, tree name = NULL_TREE);

  // Calculate a range on edge E only if it is defined by E.
  virtual bool outgoing_edge_range_p (irange &r, edge e, tree name,
				      irange *name_range = NULL);

  
  bool range_of_stmt_with_range (irange &r, gimple *s, tree name,
				 const irange &name_range);
  bool reevaluate_range_of_name (irange &r, tree name, gimple *s);
  bool reevaluate_range_of_name (irange &r, tree name, edge on_edge);

  // Defintion chain
  tree terminal_name (tree name);

  void dump (FILE *f);
  void exercise (FILE *f);
protected:
  // Calculate the range for NAME if the result of statement S is the range LHS.
  class gori_compute *m_gori; 	/* Generates Outgoing Range Info.  */

  // Calculate a range for a kind of gimple statement .
  bool range_of_range_op_core (irange &r, grange_op *s, bool valid,
			       irange &range1, irange &range2);
  bool range_of_range_op  (irange &r, grange_op *s);
  bool range_of_range_op  (irange &r, grange_op *s, tree name,
			   const irange &name_range);
  bool range_of_range_op  (irange &r, grange_op *s, gimple *eval_from);
  bool range_of_range_op  (irange &r, grange_op *s, edge on_edge);

  bool range_of_phi (irange &r, gphi *phi, tree name = NULL_TREE,
		     const irange *name_range = NULL, gimple *eval_from = NULL,
		     edge on_edge = NULL);

  bool range_of_call (irange &r, gcall *call, tree name = NULL_TREE,
		      const irange *name_range = NULL,
		      gimple *eval_from = NULL, edge on_edge = NULL);

  bool range_of_cond_expr (irange &r, gassign* call, edge on_edge);
  bool range_of_cond_expr (irange &r, gassign* call, tree name = NULL_TREE,
			   const irange *name_range = NULL,
			   gimple *eval_from = NULL);


  bool range_p (basic_block bb, tree name);
};

  

// This class utilizes the gori summary to query the range
// of SSA_NAMEs across multiple basic blocks and edges.  It builds a cache
// of range on entry to blocks.  All work is done on-demand so it is relatively
// lightweight until used.
// 
// There is a global ssa-name table maintained via a set of private 
// global_ssa_name routines.  This is used to avoid recalculating ranges a
// second time.

class global_ranger : public ssa_range
{
public:
  global_ranger ();
  ~global_ranger ();

  virtual bool range_of_expr (irange &r, tree op, gimple *s = NULL);
  virtual bool range_of_expr (irange &r, tree expr, edge e);
  virtual bool range_on_entry (irange &r, basic_block bb, tree name);
  virtual bool range_of_stmt (irange &r, gimple *s, tree name = NULL_TREE);

  void dump (FILE *f);
  void exercise (FILE *f);   /* Calculate all stmts and dump */

private:
  void dump_global_ssa_range (FILE *f);
  bool has_global_ssa_range (irange &r, tree name);
  bool get_global_ssa_range (irange &r, tree name);
  void set_global_ssa_range (tree name, const irange&r);
  void clear_global_ssa_range (tree name);

  class block_range_cache *m_block_cache;
  class ssa_global_cache *m_globals;
  class non_null_ref *m_non_null;
  bool non_null_deref_in_block (irange &r, tree name, basic_block bb);
  void fill_block_cache (tree name, basic_block bb, basic_block def_bb);
  bool maybe_propagate_on_edge (tree name, edge e);

  vec<basic_block> m_workback;
  vec<basic_block> m_workfwd;
  sbitmap m_visited;

};


// Like global_ranger::path_range_on_stmt(), but make an on-the-fly ranger.
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
  global_ranger ranger;
  bool ret;
  ret = ranger.range_of_expr (r, ssa, stmt);
  if (ret && r.varying_p ())
    return false;
  return ret;
}

// This function return true if type TYPE is supported by ranges.

inline bool
ssa_range::supports_type_p (tree type)
{
  // Only support irange at the moment.
  return irange::supports_type_p (type);
}


// This function return true if SSA is a non-virtual SSA_NAME with a type
// supported by ranges.

inline bool
ssa_range::supports_ssa_p (tree ssa)
{
  if (!SSA_NAME_IS_VIRTUAL_OPERAND (ssa))
    return supports_type_p (TREE_TYPE (ssa));
 return false;
}

// This function returns EXP if EXP is an ssa_name and is supported by ranges.
// Otherwise it returns NULL_TREE

inline tree
ssa_range::valid_ssa_p (tree exp)
{
  if (exp && TREE_CODE (exp) == SSA_NAME && supports_ssa_p (exp))
    return exp;
  return NULL_TREE;
}

// This function returns TRUE if constant c is supported by ranges.

inline bool
ssa_range::supports_const_p (tree c)
{
  if (!TREE_OVERFLOW (c))
    return supports_type_p (TREE_TYPE (c));
  return false;

}

// This function returns true if expr is supported by ranges.

inline bool
ssa_range::supports_p (tree expr)
{
  if (TYPE_P (expr))
    return supports_type_p (expr);
  else if (TREE_CODE (expr) == SSA_NAME)
    return supports_ssa_p (expr);
  // Constant overflows are rejected.
  else if (CONSTANT_CLASS_P (expr))
    return supports_const_p (expr);

  return supports_type_p (TREE_TYPE (expr));
}

#endif // GCC_SSA_RANGE_H
