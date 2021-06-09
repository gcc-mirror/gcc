/* Header file for the GIMPLE range interface.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
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
#include "value-query.h"
#include "gimple-range-edge.h"
#include "gimple-range-gori.h"
#include "gimple-range-cache.h"

// This file is the main include point for gimple ranges.
// There are two fold_range routines of interest:
//   bool fold_range (irange &r, gimple *s, range_query *q)
//   bool fold_range (irange &r, gimple *s, edge on_edge, range_query *q)
// These routines will fold stmt S into the result irange R.
// Any ssa_names on the stmt will be calculated using the range_query
// parameter via a call to range_of_expr.
// If no range_query is provided, current global range info will be used.
// The second variation specifies an edge, and stmt S is recalculated as if
// it appeared on that edge.


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
  inline gori_compute &gori ()  { return m_cache.m_gori; }
  virtual void dump (FILE *f) OVERRIDE;
  void dump_bb (FILE *f, basic_block bb);
protected:
  bool fold_range_internal (irange &r, gimple *s, tree name);
  ranger_cache m_cache;
};

// Source of all operands for fold_using_range and gori_compute.
// It abstracts out the source of an operand so it can come from a stmt or
// and edge or anywhere a derived classof fur_source wants.

class fur_source
{
public:
  virtual bool get_operand (irange &r, tree expr);
  virtual bool get_phi_operand (irange &r, tree expr, edge e);
  virtual void register_dependency (tree lhs, tree rhs);
  virtual range_query *query ();
};

// fur_stmt is the specification for drawing an operand from range_query Q
// via a range_of_Expr call on stmt S.

class fur_stmt : public fur_source
{
public:
  fur_stmt (gimple *s, range_query *q = NULL);
  virtual bool get_operand (irange &r, tree expr) OVERRIDE;
  virtual bool get_phi_operand (irange &r, tree expr, edge e) OVERRIDE;
  virtual range_query *query () OVERRIDE;
private:
  range_query *m_query;
  gimple *m_stmt;
};


// Fold stmt S into range R using range query Q.
bool fold_range (irange &r, gimple *s, range_query *q = NULL);
// Recalculate stmt S into R using range query Q as if it were on edge ON_EDGE.
bool fold_range (irange &r, gimple *s, edge on_edge, range_query *q = NULL);
// These routines allow you to specify the operands to use when folding.
// Any excess queries will be drawn from the current range_query.
bool fold_range (irange &r, gimple *s, irange &r1);
bool fold_range (irange &r, gimple *s, irange &r1, irange &r2);
bool fold_range (irange &r, gimple *s, unsigned num_elements, irange *vector);

// This class uses ranges to fold a gimple statement producinf a range for
// the LHS.  The source of all operands is supplied via the fur_source class
// which provides a range_query as well as a source location and any other
// required information.

class fold_using_range
{
public:
  bool fold_stmt (irange &r, gimple *s, class fur_source &src,
		  tree name = NULL_TREE);
protected:
  bool range_of_range_op (irange &r, gimple *s, fur_source &src);
  bool range_of_call (irange &r, gcall *call, fur_source &src);
  bool range_of_cond_expr (irange &r, gassign* cond, fur_source &src);
  bool range_of_address (irange &r, gimple *s, fur_source &src);
  bool range_of_builtin_call (irange &r, gcall *call, fur_source &src);
  void range_of_builtin_ubsan_call (irange &r, gcall *call, tree_code code,
				    fur_source &src);
  bool range_of_phi (irange &r, gphi *phi, fur_source &src);
  void range_of_ssa_name_with_loop_info (irange &, tree, class loop *, gphi *,
					 fur_source &src);
};


// These routines provide a GIMPLE interface to the range-ops code.
extern tree gimple_range_operand1 (const gimple *s);
extern tree gimple_range_operand2 (const gimple *s);
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
  if (const gassign *ass = dyn_cast<const gassign *> (s))
    return range_op_handler (gimple_assign_rhs_code (ass),
			     TREE_TYPE (gimple_assign_lhs (ass)));
  if (const gcond *cond = dyn_cast<const gcond *> (s))
    return range_op_handler (gimple_cond_code (cond),
			     TREE_TYPE (gimple_cond_lhs (cond)));
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

// Return true if TYPE1 and TYPE2 are compatible range types.

static inline bool
range_compatible_p (tree type1, tree type2)
{
  // types_compatible_p requires conversion in both directions to be useless.
  // GIMPLE only requires a cast one way in order to be compatible.
  // Ranges really only need the sign and precision to be the same.
  return (TYPE_PRECISION (type1) == TYPE_PRECISION (type2)
	  && TYPE_SIGN (type1) == TYPE_SIGN (type2));
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
#define DEBUG_RANGE_CACHE (dump_file && (param_evrp_mode & EVRP_MODE_DEBUG))

extern gimple_ranger *enable_ranger (struct function *);
extern void disable_ranger (struct function *);

#endif // GCC_GIMPLE_RANGE_STMT_H
