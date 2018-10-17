/* Header file for RIMPLE classes.  Range GIMPLE.
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

#ifndef GCC_RIMPLE_H
#define GCC_RIMPLE_H

#include "range.h"
#include "range-op.h"

/* These classes derive from the various GIMPLE classes and provide a
   similar experience.  The is_a::test helper functions determine suitability
   of a gimple statement for range processing.  Any gimple statment which 
   has no bearing on range calculations will return NULL.

   ONe a gimple statement has been successfully cast to a rimple statement,
   various range specific queries are available.
*/


// This function return true if T is a type supported by the GCC range system.

inline bool
gimple_range_supports_type (tree type)
{
  // Only support irange at the moment.
  return valid_irange_type (type);
}

// This function return true if T is a non-virtual SSA_NAME with a type
// supported by the GCC range system.

inline bool
gimple_range_supports_ssa (tree ssa)
{
  if (!SSA_NAME_IS_VIRTUAL_OPERAND (ssa))
    return gimple_range_supports_type (TREE_TYPE (ssa));
 return false;
}

// This function returns TRUE if constant c is valid for range processing.
//
inline bool
gimple_range_supports_const (tree c)
{
  if (!TREE_OVERFLOW (c))
    return gimple_range_supports_type (TREE_TYPE (c));
  return false;

}

// This function returns TRUE id expr is supported by the gimple range system.

inline bool
gimple_range_supports (tree expr)
{
  if (TYPE_P (expr))
    return gimple_range_supports_type (expr);
  else if (TREE_CODE (expr) == SSA_NAME)
    return gimple_range_supports_ssa (expr);
  // Constant overflows are rejected.
  else if (CONSTANT_CLASS_P (expr))
    return gimple_range_supports_const (expr);

  return gimple_range_supports_type (TREE_TYPE (expr));
}

// Rimple version of an assignment statement which is a logical operation
// requiring special processing. This is specifically either a && or ||
// operation. When calcuating ranges through these statements, there is
// some special processing required. 
class GTY((tag("GCC_GLOGICAL")))
  glogical : public gassign
{
  // Adds no new fields, adds invariant stmt->code == GIMPLE_ASSIGN and 
  // expr_code is an AND or OR on boolean compatiable operands.
};

extern bool
gimple_range_logical_fold (irange& r, const glogical *g, const irange& lhs,
			   const irange& op1_true, const irange& op1_false,
			   const irange& op2_true, const irange& op2_false);
inline bool
gimple_range_logical_fold (irange& r, const gimple *gs, const irange& lhs,
			   const irange& op1_true, const irange& op1_false,
			   const irange& op2_true, const irange& op2_false)
{
  const glogical *s = GIMPLE_CHECK2<const glogical *> (gs);
  return gimple_range_logical_fold (r, s, lhs, op1_true, op1_false, op2_true,
				    op2_false);
}  


class GTY((tag("GCC_GIMPLE_RANGE_OPERATOR")))
  grange_op: public gimple
{
public:
  tree lhs () const;
  tree operand1 () const;
  tree operand2 () const;
  tree ssa_operand1 () const;
  tree ssa_operand2 () const;

  bool fold (irange& res, const irange& r1) const;
  bool calc_op1_irange (irange& r, const irange& lhs_range) const;

  bool fold (irange& res, const irange& r1, const irange& r2) const;
  bool calc_op1_irange (irange& r, const irange& lhs_range,
		   const irange& op2_range) const;
  bool calc_op2_irange (irange& r, const irange& lhs_range,
		   const irange& op1_range) const;
private:
  class range_operator *handler() const;
};


// Return the LHS, of this node. If there isn't a LHS return NULL_TREE.
inline tree
grange_op::lhs () const
{
  if (gimple_code (this) == GIMPLE_ASSIGN)
    return gimple_assign_lhs (this);
  return NULL_TREE;
}

/* Return the second operand of the statement, if there is one.  Otherwise
   return NULL_TREE*/
inline tree
grange_op::operand2 () const
{
  if (gimple_code (this) == GIMPLE_COND)
    return gimple_cond_rhs (this);

  gcc_checking_assert (gimple_code (this) == GIMPLE_ASSIGN);

  if (gimple_num_ops (this) >= 3)
    return gimple_assign_rhs2 (this);
  return NULL_TREE;
}


/* Return the first operand of the statement, if it is a valid SSA_NAME which
   is supported rimple. Otherwise return NULL_TREE.  */
inline tree
grange_op::ssa_operand1() const
{
  tree op1 = operand1 ();
  if (op1 && TREE_CODE (op1) == SSA_NAME)
    return op1;
  return NULL_TREE;
}

/* Return the second operand of the statement, if it is a valid SSA_NAME which
   is supported by rimple. Otherwise return NULL_TREE.  */
inline tree
grange_op::ssa_operand2 () const
{
  tree op2 = operand2 ();
  if (op2 && TREE_CODE (op2) == SSA_NAME)
    return op2;
  return NULL_TREE;
}

inline range_operator *
grange_op::handler () const
{
  return range_op_handler (gimple_expr_code (this));
}

// Static ranges from an edge that arent related to ssa_names. conditional
// branches and switch operations.
extern void gimple_range_outgoing_edge (irange &r, edge e);
// Return a basic range for a tree expr.
extern bool gimple_range_of_expr (irange &r, tree expr);
// Return a basic range for a gimple statement
extern bool gimple_range_of_stmt (irange &r, gimple *s);
extern bool gimple_range_of_stmt (irange &r, grange_op *s);
// Return a basic range for a statement if NAME has a range
extern bool gimple_range_of_stmt (irange &r, gimple *s, tree name,
				  const irange &nr);
extern bool gimple_range_of_stmt (irange &r, grange_op *s,
				  tree name, const irange &nr);


template <>
template <>
inline bool
is_a_helper <grange_op *>::test (gimple *gs)
{ 
  if (dyn_cast<gassign *> (gs) || dyn_cast<gcond *>(gs))
    if (range_op_handler (gimple_expr_code (gs)))
      return true;
  return false;
}

template <>
template <>
inline bool
is_a_helper <grange_op *>::test (glogical *gs ATTRIBUTE_UNUSED)
{ 
  return true;
}

inline bool
glogical_is_a_helper (const gimple *gs)
{ 
  /* Look for boolean and/or condition.  */
  if (gimple_code (gs) == GIMPLE_ASSIGN)
    switch (gimple_expr_code (gs))
      {
	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	  return true;

	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	  // Bitwise operations on single bits are logical too.
	  if (types_compatible_p (TREE_TYPE (gimple_assign_rhs1 (gs)),
				  boolean_type_node))
	    return true;
	  break;

	default:
	  break;
      }
  return false;
}

template <>
template <>
inline bool
is_a_helper <glogical *>::test (gimple *gs)
{ 
  return glogical_is_a_helper (gs);
}

template <>
template <>
inline bool
is_a_helper <const glogical *>::test (const gimple *gs)
{ 
  return glogical_is_a_helper (gs);
}
#endif // GCC_RIMPLE_H
