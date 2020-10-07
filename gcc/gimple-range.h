/* Header file for the GIMPLE range interface.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#ifndef GCC_GIMPLE_RANGE_STMT_H
#define GCC_GIMPLE_RANGE_STMT_H


#include "range.h"
#include "range-op.h"
#include "gimple-range-edge.h"
#include "gimple-range-gori.h"
#include "gimple-range-cache.h"
#include "value-query.h"

// This is the basic range generator interface.
//
// This base class provides all the API entry points, but only provides
// functionality at the statement level.  Ie, it can calculate ranges on
// statements, but does no additonal lookup.
//
// All the range_of_* methods will return a range if the types is
// supported by the range engine.  It may be the full range for the
// type, AKA varying_p or it may be a refined range.  If the range
// type is not supported, then false is returned.  Non-statement
// related methods return whatever the current global value is.


class gimple_ranger : public range_query
{
public:
  gimple_ranger () : m_cache (*this) { }
  virtual bool range_of_stmt (irange &r, gimple *, tree name = NULL) OVERRIDE;
  virtual bool range_of_expr (irange &r, tree name, gimple * = NULL) OVERRIDE;
  virtual bool range_on_edge (irange &r, edge e, tree name) OVERRIDE;
  virtual void range_on_entry (irange &r, basic_block bb, tree name);
  virtual void range_on_exit (irange &r, basic_block bb, tree name);
  void export_global_ranges ();
  void dump (FILE *f);
protected:
  bool calc_stmt (irange &r, gimple *s, tree name = NULL_TREE);
  bool range_of_range_op (irange &r, gimple *s);
  bool range_of_call (irange &r, gcall *call);
  bool range_of_cond_expr (irange &r, gassign* cond);
  ranger_cache m_cache;
private:
  bool range_of_phi (irange &r, gphi *phi);
  bool range_of_non_trivial_assignment (irange &r, gimple *s);
  bool range_of_builtin_call (irange &r, gcall *call);
  void range_of_builtin_ubsan_call (irange &r, gcall *call, tree_code code);
  bool range_with_loop_info (irange &r, tree name);
  void range_of_ssa_name_with_loop_info (irange &, tree, class loop *,
					 gphi *);
};

// Calculate a basic range for a tree expression.
extern bool get_tree_range (irange &r, tree expr);

// These routines provide a GIMPLE interface to the range-ops code.
extern tree gimple_range_operand1 (const gimple *s);
extern tree gimple_range_operand2 (const gimple *s);
extern tree gimple_range_base_of_assignment (const gimple *s);
extern bool gimple_range_fold (irange &res, const gimple *s,
			       const irange &r1);
extern bool gimple_range_fold (irange &res, const gimple *s,
			       const irange &r1,
			       const irange &r2);
extern bool gimple_range_calc_op1 (irange &r, const gimple *s,
				   const irange &lhs_range);
extern bool gimple_range_calc_op1 (irange &r, const gimple *s,
				   const irange &lhs_range,
				   const irange &op2_range);
extern bool gimple_range_calc_op2 (irange &r, const gimple *s,
				   const irange &lhs_range,
				   const irange &op1_range);


// Return the range_operator pointer for this statement.  This routine
// can also be used to gate whether a routine is range-ops enabled.

static inline range_operator *
gimple_range_handler (const gimple *s)
{
  if ((gimple_code (s) == GIMPLE_ASSIGN) || (gimple_code (s) == GIMPLE_COND))
    return range_op_handler (gimple_expr_code (s), gimple_expr_type (s));
  return NULL;
}

// Return EXP if it is an SSA_NAME with a type supported by gimple ranges.

static inline tree
gimple_range_ssa_p (tree exp)
{
  if (exp && TREE_CODE (exp) == SSA_NAME &&
      !SSA_NAME_IS_VIRTUAL_OPERAND (exp) &&
      irange::supports_type_p (TREE_TYPE (exp)))
    return exp;
  return NULL_TREE;
}

// Return the legacy GCC global range for NAME if it has one, otherwise
// return VARYING.

static inline value_range
gimple_range_global (tree name)
{
  gcc_checking_assert (gimple_range_ssa_p (name));
  tree type = TREE_TYPE (name);
#if 0
  // Reenable picking up global ranges when we are OK failing tests that look
  // for builtin_unreachable in the code, like
  // RUNTESTFLAGS=dg.exp=pr61034.C check-g++
  // pre-optimizations (inlining) set a global range which causes the ranger
  // to remove the condition which leads to builtin_unreachable.
  if (!POINTER_TYPE_P (type) && SSA_NAME_RANGE_INFO (name))
    {
      // Return a range from an SSA_NAME's available range.
      wide_int min, max;
      enum value_range_kind kind = get_range_info (name, &min, &max);
      return value_range (type, min, max, kind);
    }
#endif
 // Otherwise return range for the type.
 return value_range (type);
}


// This class overloads the ranger routines to provide tracing facilties
// Entry and exit values to each of the APIs is placed in the dumpfile.

class trace_ranger : public gimple_ranger
{
public:
  trace_ranger ();
  virtual bool range_of_stmt (irange &r, gimple *s, tree name = NULL_TREE);
  virtual bool range_of_expr (irange &r, tree name, gimple *s = NULL);
  virtual bool range_on_edge (irange &r, edge e, tree name);
  virtual void range_on_entry (irange &r, basic_block bb, tree name);
  virtual void range_on_exit (irange &r, basic_block bb, tree name);
private:
  static const unsigned bump = 2;
  unsigned indent;
  unsigned trace_count;		// Current trace index count.

  bool dumping (unsigned counter, bool trailing = false);
  bool trailer (unsigned counter, const char *caller, bool result, tree name,
		const irange &r);
};

// Flag to enable debugging the various internal Caches.
#define DEBUG_RANGE_CACHE (dump_file && (flag_evrp_mode & EVRP_MODE_DEBUG))

#endif // GCC_GIMPLE_RANGE_STMT_H
