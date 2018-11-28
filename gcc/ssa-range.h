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
#include "ssa-range-gori.h"

extern gimple *gimple_outgoing_range_stmt_p (basic_block bb);
extern gimple *gimple_outgoing_edge_range_p (irange &r, edge e);
extern bool get_tree_range (irange &r, tree expr);

// This is the basic range generator interface. 
//
// This base class provides all the API entry points, but only provides 
// functionality at the statement level.  Ie, it can calculate ranges on 
// statements, but does no additonal lookup.
//
// All the range_of_* methods will return a range if the types is supported
// by the range engine. It may be the full range for the type, AKA varying_p
// or it may be a refined range.
// If the range type is not supported, then false is returned.
// The basic ssa_ranger class will also return false for range_on_entry and
// range_on_exit as those make no sense at the statement level.
//
// outgoing_edge_range_p will only return a range if the edge specified 
// defines a range for the specified name.  Otherwise false is returned.
//

class ssa_ranger
{
  public:
  ssa_ranger ();
  ~ssa_ranger ();

  static bool supports_type_p (tree type);
  static bool supports_ssa_p (tree ssa);
  static bool supports_const_p (tree c);
  static bool supports_p (tree expr);
  static tree valid_ssa_p (tree exp);

  virtual bool range_of_expr (irange &r, tree expr, gimple *s = NULL);
  virtual bool range_of_expr (irange &r, tree expr, edge e);
  virtual bool range_of_stmt (irange &r, gimple *s, tree name = NULL_TREE);
  virtual bool range_of_stmt_with_range (irange &r, gimple *s, tree name,
					 const irange &name_range);
  virtual bool range_on_edge (irange &r, edge e, tree name);
  virtual bool range_on_entry (irange &r, basic_block bb, tree name);
  virtual bool range_on_exit (irange &r, basic_block bb, tree name);

  // Calculate a range on edge E only if it is defined by E.
  virtual bool outgoing_edge_range_p (irange &r, edge e, tree name,
				      irange *name_range = NULL);

  
protected:
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
};

// THe gori_ranger works at the block level. GORI stands for Generates Outgoing
// Range Info, and utilizes the engine from ssa-range-gori.h to buil;d
// defintion chains on demand which enables the calculation of ranges for 
// various ssa names that are related to those that actually generate ranges.
//
// It is lightweight to declare in this case, but for each basic block that is
// queried, it will scan some or all of the statements in the block to
// determine what ssa-names can have range information generated for them and
// cache this information.  
//
// terminal_name () provides the "input" name to the chain of statements for
// name that can change the calculation of its range.  This is usually outside 
// the basic block the name is defind in..  ie,
// bb4:
//   a_2 = b_6 + 5
//   c_3 = (a_2 < 25)
// in this small example, b_6 is the
// "terminal_name" returned for both a_2 and c_3 since a change in the
// range of b_6 to calculate their results may produce a different range.

class gori_ranger : public ssa_ranger
{
  public:
  gori_ranger ();
  ~gori_ranger ();

  virtual bool range_on_edge (irange &r, edge e, tree name);
  virtual bool range_on_entry (irange &r, basic_block bb, tree name);
  virtual bool range_on_exit (irange &r, basic_block bb, tree name);

  virtual bool outgoing_edge_range_p (irange &r, edge e, tree name,
				      irange *name_range = NULL);
  
  tree terminal_name (tree name);

  void dump (FILE *f);
protected:
  // Calculate the range for NAME if the result of statement S is the range LHS.
  gori_compute m_gori; 	  /* Generates Outgoing Range Info.  */
  bool reevaluate_definition (irange &r, tree name, edge e,
			      irange *block_range);

  // Do we need these any more?
  bool reevaluate_range_of_name (irange &r, tree name, gimple *s);
  bool reevaluate_range_of_name (irange &r, tree name, edge on_edge);
};


// This class utilizes the gori summary to query the range
// of SSA_NAMEs across multiple basic blocks and edges.  It builds a cache
// of range on entry to blocks.  All work is done on-demand so it is relatively
// lightweight until used.
// 
// There is a global ssa-name table maintained via a set of private 
// global_ssa_name routines.

class global_ranger : public gori_ranger
{
public:
  global_ranger ();
  ~global_ranger ();

  virtual bool range_of_expr (irange &r, tree op, gimple *s = NULL);
  virtual bool range_of_expr (irange &r, tree expr, edge e);
  virtual bool range_on_entry (irange &r, basic_block bb, tree name);
  virtual bool range_of_stmt (irange &r, gimple *s, tree name = NULL_TREE);

  void dump (FILE *f);
  void calculate_and_dump (FILE *f);   /* Calculate all stmts and dump */

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
  void maybe_propagate_block (tree name, basic_block bb);
  void iterative_cache_update (tree name);

  vec<basic_block> m_workback;
  vec<basic_block> m_update_list;
};


class trace_ranger : public global_ranger
{
public:
  trace_ranger();

  virtual bool range_of_expr (irange &r, tree expr, gimple *s = NULL);
  virtual bool range_of_expr (irange &r, tree expr, edge e);
  virtual bool range_on_edge (irange &r, edge e, tree name);
  virtual bool range_on_entry (irange &r, basic_block bb, tree name);
  virtual bool range_on_exit (irange &r, basic_block bb, tree name);
  virtual bool range_of_stmt (irange &r, gimple *s, tree name = NULL_TREE);

  // Calculate a range on edge E only if it is defined by E.
  virtual bool outgoing_edge_range_p (irange &r, edge e, tree name,
				      irange *name_range = NULL);
private:
  typedef global_ranger super;  // Inherited from class for easy changing.
  static const unsigned bump = 2;
  unsigned indent;
  unsigned counter;

  bool dumping ();
  bool trailer (const char *caller, bool result, tree name, const irange &r);
};

  

// Like global_ranger::range_of_expr (), but make an on-the-fly ranger.
// Return TRUE if SSA as seen from within STMT has a known range the is not
// varying.  Set this range in R.
//
// NOTE: There is overhead involved with this function, so it
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
ssa_ranger::supports_type_p (tree type)
{
  // Only support irange at the moment.
  return irange::supports_type_p (type);
}


// This function return true if SSA is a non-virtual SSA_NAME with a type
// supported by ranges.

inline bool
ssa_ranger::supports_ssa_p (tree ssa)
{
  if (!SSA_NAME_IS_VIRTUAL_OPERAND (ssa))
    return supports_type_p (TREE_TYPE (ssa));
 return false;
}

// This function returns TRUE if constant c is supported by ranges.

inline bool
ssa_ranger::supports_const_p (tree c)
{
  if (!TREE_OVERFLOW (c))
    return supports_type_p (TREE_TYPE (c));
  return false;

}

// This function returns true if expr is supported by ranges.

inline bool
ssa_ranger::supports_p (tree expr)
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

// This function returns EXP if EXP is an ssa_name and is supported by ranges.
// Otherwise it returns NULL_TREE

inline tree
ssa_ranger::valid_ssa_p (tree exp)
{
  if (exp && TREE_CODE (exp) == SSA_NAME && supports_ssa_p (exp))
    return exp;
  return NULL_TREE;
}


#endif // GCC_SSA_RANGE_H
