/* Header file for ssa-range generator stmt summary.
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

#ifndef GCC_SSA_RANGE_STMT_H
#define GCC_SSA_RANGE_STMT_H

#include "range.h"
#include "range-op.h"

/* This class is used to summarize gimple statements that are supported by the
   irange_operator class, and provide an interface to range operations on
   the statement.

   The contents of the statement which may be of interest to range operations 
   are made available with simple queries.

   Current support is for unary and binary statements.  The expression or its
   operands can be resolved to a range if 2 of the 3 operands have ranges
   supplied for them. 
   
   This class is intended to be a lightweight overlay of a 'gimple *'
   pointer, and can be passed by value. It can be created from a gimple stmt
   and converted back to a gimple statement by default.  */

class range_stmt
{
private:
  gimple *g;
  void validate_stmt (gimple *s);
  class irange_operator *handler() const;
  tree_code get_code () const;
public:
  range_stmt ();
  range_stmt (gimple *stmt);
  range_stmt &operator= (gimple *stmt);

  bool valid () const;
  operator gimple *() const;

  tree operand1 () const;
  tree operand2 () const;
  tree ssa_operand1 () const;
  tree ssa_operand2 () const;

  bool fold (irange& res, const irange& r1) const;
  bool op1_irange (irange& r, const irange& lhs_range) const;

  bool fold (irange& res, const irange& r1, const irange& r2) const;
  bool op1_irange (irange& r, const irange& lhs_range,
		   const irange& op2_range) const;
  bool op2_irange (irange& r, const irange& lhs_range,
		   const irange& op1_range) const;

  bool logical_expr_p () const;
  bool fold_logical (irange& r, const irange& lhs, const irange& op1_true,
		     const irange& op1_false, const irange& op2_true,
		     const irange& op2_false) const;

  void dump (FILE *f) const;
};


/* Initialize a range statement to invalid.  */
inline
range_stmt::range_stmt ()
{
  g = NULL;
}

/* Initialize a range statement from gimple statement S.  */
inline 
range_stmt::range_stmt (gimple *s)
{
  validate_stmt (s);
}

/* Initialize a range statement from gimple statement S.  */
inline range_stmt&
range_stmt::operator= (gimple *s)
{
  validate_stmt (s);
  return *this;
}

/* Return true is this is a valid range summary.  */
inline bool
range_stmt::valid () const
{
  return g != NULL;
}

inline 
range_stmt::operator gimple *() const
{
  return g;
}

inline tree_code
range_stmt::get_code () const
{
  return gimple_expr_code (g);
}

/* Return the second operand of the statement, if there is one.  Otherwise
   return NULL_TREE*/
inline tree
range_stmt::operand2 () const
{
  switch (gimple_code (g))
    {
      case GIMPLE_COND:
        return gimple_cond_rhs (g);
      case GIMPLE_ASSIGN:
        if (gimple_num_ops (g) >= 3)
	  return gimple_assign_rhs2 (g);
	else
	  return NULL_TREE;
      default:
        break;
    }
  return NULL_TREE;
}

/* Return the first operand of the statement, if it is a valid SSA_NAME which
   is supported by class irange. Otherwise return NULL_TREE.  */
inline tree
range_stmt::ssa_operand1() const
{
  tree op1 = operand1 ();
  /* Validate statement has confirmed this SSA_NAME is a valid kind.  */
  if (op1 && TREE_CODE (op1) == SSA_NAME)
    return op1;
  return NULL_TREE;
}

/* Return the second operand of the statement, if it is a valid SSA_NAME which
   is supported by class irange. Otherwise return NULL_TREE.  */
inline tree
range_stmt::ssa_operand2 () const
{
  tree op2 = operand2 ();
  /* Validate statement has confirmed this SSA_NAME is a valid kind.  */
  if (op2 && TREE_CODE (op2) == SSA_NAME)
    return op2;
  return NULL_TREE;
}

inline irange_operator *
range_stmt::handler () const
{
  return irange_op_handler (get_code ());
}

#endif /* GCC_SSA_RANGE_STMT_H */
