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
//   grange_op gr = s;
//   if (!gr)
//     return false;  // NULL means this stmt cannot generate ranges.
//   < ...decide on some ranges... >
//   /* And call the range fold routine for this statement.  */
//   return gr->fold (res_range, op1_range, op2_range);

class GTY((tag("GCC_SSA_RANGE_OPERATOR")))
  grange_op: public gimple
{
public:
  // Adds no new fields, adds invariant 
  // stmt->code == GIMPLE_ASSIGN || stmt->code == GIMPLE_COND
  // and there is a range_operator handler for gimple_expr_code().
  tree lhs () const;
  tree operand1 () const;
  tree operand2 () const;

  bool fold (irange &res, const irange &r1) const;
  bool calc_op1_irange (irange &r, const irange &lhs_range) const;

  bool fold (irange &res, const irange &r1, const irange &r2) const;
  bool calc_op1_irange (irange &r, const irange &lhs_range,
		   const irange &op2_range) const;
  bool calc_op2_irange (irange &r, const irange &lhs_range,
		   const irange &op1_range) const;
private:
  // Range-op IL agnostic range calculations handler.
  class range_operator *handler() const;
  // IL contextual range information adjustments handler.
  class grange_adjust *grange_adjust_handler () const;
};

// Return the LHS of this statement. If there isn't a LHS return NULL_TREE.

inline tree
grange_op::lhs () const
{
  if (gimple_code (this) == GIMPLE_ASSIGN)
    return gimple_assign_lhs (this);
  return NULL_TREE;
}

// Return the second operand of this statement, otherwise return NULL_TREE.

inline tree
grange_op::operand2 () const
{
  if (gimple_code (this) == GIMPLE_COND)
    return gimple_cond_rhs (this);

  // At this point it must be an assignemnt statement.
  if (gimple_num_ops (this) >= 3)
    return gimple_assign_rhs2 (this);
  return NULL_TREE;
}

template <>
template <>
inline bool
is_a_helper <const grange_op *>::test (const gimple *gs)
{ 
  extern grange_adjust *gimple_range_adjust_handler (enum tree_code c);
  // Supported statement kind and there is a handler for the expression code.
  if (dyn_cast<const gassign *> (gs) || dyn_cast<const gcond *>(gs))
    {
      enum tree_code c = gimple_expr_code (gs);
      tree expr_type = gimple_expr_type (gs);
      return range_op_handler (c, expr_type) || gimple_range_adjust_handler (c);
    }
  return false;
}

template <>
template <>
inline bool
is_a_helper <grange_op *>::test (gimple *gs)
{ 
  extern grange_adjust *gimple_range_adjust_handler (enum tree_code c);
  // Supported statement kind and there is a handler for the expression code.
  // Supported statement kind and there is a handler for the expression code.
  if (dyn_cast<gassign *> (gs) || dyn_cast<gcond *>(gs))
    {
      enum tree_code c = gimple_expr_code (gs);
      tree expr_type = gimple_expr_type (gs);
      return range_op_handler (c, expr_type) || gimple_range_adjust_handler (c);
    }
  return false;
}

#endif // GCC_GRANGE_H
