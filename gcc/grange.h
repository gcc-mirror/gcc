/* Header file for grange gimple statement implementation.
   Copyright (C) 2019 Free Software Foundation, Inc.
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

#ifndef GCC_GRANGE_H
#define GCC_GRANGE_H

#include "range.h"
#include "range-op.h"

// Gimple statement which supports range_op operations.
// This can map to gimple assign or cond statements, so quick access to the
// operands is provided so the user does not need to know which it is.
// Also provides access to the range operator interface taking care of 
// filling in unary operand requirements from the statement.
//
// usage is typical gimple statement style:
// foo (gimple *s)
// {
//   grange gr = s;
//   if (!gr)
//     return false;  // NULL means this stmt cannot generate ranges.
//   < ...decide on some ranges... >
//   /* And call the range fold routine for this statement.  */
//   return gimple_range_fold (gr, res_range, op1_range, op2_range);

class GTY((tag("GCC_SSA_RANGE_OPERATOR")))
  grange: public gimple
{
  // Adds no new fields, adds invariant 
  // stmt->code == GIMPLE_ASSIGN || stmt->code == GIMPLE_COND
  // and there is a range_operator handler for gimple_expr_code().
};

template <>
template <>
inline bool
is_a_helper <const grange *>::test (const gimple *gs)
{ 
  // Supported statement kind and there is a handler for the expression code.
  if (dyn_cast<const gassign *> (gs) || dyn_cast<const gcond *>(gs))
    {
      enum tree_code c = gimple_expr_code (gs);
      tree expr_type = gimple_expr_type (gs);
      return range_op_handler (c, expr_type);
    }
  return false;
}

template <>
template <>
inline bool
is_a_helper <grange *>::test (gimple *gs)
{ 
  // Supported statement kind and there is a handler for the expression code.
  // Supported statement kind and there is a handler for the expression code.
  if (dyn_cast<gassign *> (gs) || dyn_cast<gcond *>(gs))
    {
      enum tree_code c = gimple_expr_code (gs);
      tree expr_type = gimple_expr_type (gs);
      return range_op_handler (c, expr_type);
    }
  return false;
}

// Return the LHS of this statement. If there isn't a LHS return NULL_TREE.

static inline tree
gimple_range_lhs (const grange *s)
{
  if (gimple_code (s) == GIMPLE_ASSIGN)
    return gimple_assign_lhs (s);
  return NULL_TREE;
}


// Return the second operand of this statement, otherwise return NULL_TREE.

static inline tree
gimple_range_operand2 (const grange *s)
{
  if (gimple_code (s) == GIMPLE_COND)
    return gimple_cond_rhs (s);

  // At this point it must be an assignemnt statement.
  if (gimple_num_ops (s) >= 3)
    return gimple_assign_rhs2 (s);
  return NULL_TREE;
}



extern tree gimple_range_operand1 (const grange *s);
extern bool gimple_range_fold (const grange *s, irange &res,
			       const irange &r1);
extern bool gimple_range_fold (const grange *s, irange &res,
			       const irange &r1, const irange &r2);
extern bool gimple_range_calc_op1 (const grange *s, irange &r,
				   const irange &lhs_range);
extern bool gimple_range_calc_op1 (const grange *s, irange &r,
				   const irange &lhs_range,
				   const irange &op2_range);
extern bool gimple_range_calc_op2 (const grange *s, irange &r,
				   const irange &lhs_range,
				   const irange &op1_range);


extern gimple_stmt_iterator gsi_outgoing_range_stmt (basic_block bb);
extern gimple *gimple_outgoing_range_stmt_p (basic_block bb);
extern gimple *gimple_outgoing_edge_range_p (irange &r, edge e);

static inline tree
valid_range_ssa_p (tree exp)
{
  if (exp && TREE_CODE (exp) == SSA_NAME && irange::supports_ssa_p (exp))
    return exp;
  return NULL_TREE;
}

static inline irange
ssa_name_range (tree name)
{
  gcc_checking_assert (irange::supports_ssa_p (name));
  tree type = TREE_TYPE (name);
  if (!POINTER_TYPE_P (type) && SSA_NAME_RANGE_INFO (name))
    {
      // Return a range from an SSA_NAME's available range.  
      wide_int min, max;
      enum value_range_kind kind = get_range_info (name, &min, &max);
      return irange (kind, type, min, max);
    }
 // Otherwise return range for the type.
 return irange (type);
}


#endif // GCC_GRANGE_H
