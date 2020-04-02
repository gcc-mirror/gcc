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

// If BB ends with a range generating stmt, return its GSI.
extern gimple_stmt_iterator gsi_outgoing_range_stmt (basic_block bb);
// If BB ends with a range generating stmt, return that stmt.
extern gimple *gimple_outgoing_range_stmt_p (basic_block bb);
// If edge E has a constant range, return it and the range generating
// statement.  for conditonals its TRUE/FALSE, for switches its the
// possible cases.
extern gimple *gimple_outgoing_edge_range_p (irange &r, edge e);

// These routines provide a GIMPLE interface to the range-ops code.
extern tree gimple_range_operand1 (const gimple *s);
extern tree gimple_range_operand2 (const gimple *s);
extern bool gimple_range_fold (const gimple *s, irange &res,
			       const irange &r1);
extern bool gimple_range_fold (const gimple *s, irange &res,
			       const irange &r1,
			       const irange &r2);
extern bool gimple_range_calc_op1 (const gimple *s, irange &r,
				   const irange &lhs_range);
extern bool gimple_range_calc_op1 (const gimple *s, irange &r,
				   const irange &lhs_range,
				   const irange &op2_range);
extern bool gimple_range_calc_op2 (const gimple *s, irange &r,
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
  if (!POINTER_TYPE_P (type) && SSA_NAME_RANGE_INFO (name))
    {
      // Return a range from an SSA_NAME's available range.
      wide_int min, max;
      enum value_range_kind kind = get_range_info (name, &min, &max);
      return value_range (type, min, max, kind);
    }
 // Otherwise return range for the type.
 return value_range (type);
}

#endif // GCC_GIMPLE_RANGE_STMT_H
