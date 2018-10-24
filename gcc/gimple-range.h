/* Header file for gimple range interface.
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

#ifndef GCC_GIMPLE_RANGE_H
#define GCC_GIMPLE_RANGE_H

#include "range.h"
#include "range-op.h"

// Gimple statement which supports range_op operations.
// This can map to multiple gimple statement kinds, so quick access to the
// operands is provided so the user does not need to know what they are.
// Also provides access to the range operator interface.

class GTY((tag("GCC_GIMPLE_RANGE_OPERATOR")))
  grange_op: public gimple
{
public:
  // Adds no new fields, adds invariant 
  // stmt->code == GIMPLE_ASSIGN || stmt->code == GIMPLE_COND
  // and thre is a range_operator handler for the gimple_expr_code.
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
  class range_operator *handler() const;
};

// Gimple Range version of an assignment statement which is a logical operation
// requiring special processing. This is specifically either a && or ||
// operation. 

class GTY((tag("GCC_GLOGICAL")))
  glogical : public grange_op
{
  public:
  // Adds no new fields, adds invariant stmt->code == GIMPLE_ASSIGN and 
  // expr_code is an AND or OR on boolean compatiable operands.
  bool combine (irange &r, const irange &lhs, const irange &op1_true,
	        const irange &op1_false, const irange &op2_true,
	        const irange &op2_false);
};


class gimple_range
{
  public:
  static bool supports_type_p (tree type);
  static bool supports_ssa_p (tree ssa);
  static bool supports_const_p (tree c);
  static bool supports_p (tree expr);
  static tree valid_ssa_p (tree exp);

  // Calculate a range for a tree expr, originating on optional stmt S.
  virtual bool range_of_expr (irange &r, tree expr, gimple *s = NULL);

  // Calculate a range for a gimple statement which support range operations.
  bool range_of_stmt (irange &r, gimple *s);

  // Calculate the range for NAME on edge E.
  bool range_on_edge (irange &r, edge e, tree name);

  // Calculate the range for NAME on entry to block BB.
  virtual bool range_on_entry (irange &r, basic_block bb, tree name);

  // Calculate the range for NAME at the end of block BB
  bool range_on_exit (irange &r, basic_block bb, tree name);

  // Calculate a range for a kind of gimple statement .
  virtual bool range_of_range_op  (irange &r, grange_op *s);
  virtual bool range_of_phi (irange &r, gphi *phi);
  virtual bool range_of_call (irange &r, gcall *call);

  // Calculate a range on edge E only if it is defined by E.
  virtual bool outgoing_edge_range_p (irange &r, edge e, tree name = NULL);
  // Calculate the range for NAME if the result of statement S is the range LHS.
  virtual bool compute_operand_range (irange &r, gimple *s, tree name,
				      const irange &lhs);
};

// This function return true if type TYPE is supported by ranges.

inline bool
gimple_range::supports_type_p (tree type)
{
  // Only support irange at the moment.
  return irange::supports_type_p (type);
}


// This function return true if SSA is a non-virtual SSA_NAME with a type
// supported by ranges.

inline bool
gimple_range::supports_ssa_p (tree ssa)
{
  if (!SSA_NAME_IS_VIRTUAL_OPERAND (ssa))
    return supports_type_p (TREE_TYPE (ssa));
 return false;
}

// This function returns EXP if EXP is an ssa_name and is supported by ranges.
// Otherwise it returns NULL_TREE

inline tree
gimple_range::valid_ssa_p (tree exp)
{
  if (exp && TREE_CODE (exp) == SSA_NAME && supports_ssa_p (exp))
    return exp;
  return NULL_TREE;
}

// This function returns TRUE if constant c is supported by ranges.

inline bool
gimple_range::supports_const_p (tree c)
{
  if (!TREE_OVERFLOW (c))
    return supports_type_p (TREE_TYPE (c));
  return false;

}

// This function returns true if expr is supported by ranges.

inline bool
gimple_range::supports_p (tree expr)
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


// Return the LHS, of this statement. If there isn't a LHS return NULL_TREE.

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

// Return the range_operator pointer for this statement.

inline range_operator *
grange_op::handler () const
{
  return range_op_handler (gimple_expr_code (this));
}


template <>
template <>
inline bool
is_a_helper <const grange_op *>::test (const gimple *gs)
{ 
  // Supported statement kind and there is a handler for the expression code.
  if (dyn_cast<const gassign *> (gs) || dyn_cast<const gcond *>(gs))
     return range_op_handler (gimple_expr_code (gs));
  return false;
}

template <>
template <>
inline bool
is_a_helper <grange_op *>::test (gimple *gs)
{ 
  // Supported statement kind and there is a handler for the expression code.
  if (dyn_cast<gassign *> (gs) || dyn_cast<gcond *>(gs))
     return range_op_handler (gimple_expr_code (gs));
  return false;
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
#endif // GCC_GIMPLE_RANGE_H
