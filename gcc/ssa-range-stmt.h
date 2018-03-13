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

/* Return a the range for a single tree object.  */
bool get_operand_range (irange& r, tree op);

/* This class is used summarize expressions that are supported by the
   irange_operator class, and provide an interface to the operators on
   the expression.  

   The contents of the stmt are abstracted away from gimple (or any other IL)

   The expression or its operands can be resolved to a range if 2 of the
   3 operands have ranges supplied for them. 
   
   This class is not meant to be cached in any way, just utilized within the
   range engine to communicate required components of an expression.  */


class range_stmt
{
private:
  tree_code code;
  tree lhs;
  tree op1, op2;
  tree ssa1, ssa2;
  tree_code validate_operands ();
  void from_stmt (gimple *s);
  class irange_operator *handler() const;
public:
  range_stmt ();
  range_stmt (gimple *stmt);
  range_stmt &operator= (gimple *stmt);

  bool valid () const;
  tree_code get_code () const;

  tree operand1 () const;
  tree operand2 () const;

  tree ssa_operand1 () const;
  tree ssa_operand2 () const;

  bool fold (irange& res) const;
  bool fold (irange& res, tree name, const irange& name_range) const;
  bool fold (irange& res, const irange& r1) const;
  bool fold (irange& res, const irange& r1, const irange& r2) const;
  bool op1_irange (irange& r, const irange& lhs_range) const;
  bool op1_irange (irange& r, const irange& lhs_range,
		   const irange& op2_range) const;
  bool op2_irange (irange& r, const irange& lhs_range,
		   const irange& op1_range) const;

  void dump (FILE *f) const;
};


/* Initialize a range statement to invalid.  */
inline
range_stmt::range_stmt ()
{
  code = ERROR_MARK;
}

/* Initialize a range statement from gimple statement S.  */
inline 
range_stmt::range_stmt (gimple *s)
{
  from_stmt (s);
}

/* Initialize a range statement from gimple statement S.  */
inline range_stmt&
range_stmt::operator= (gimple *s)
{
  from_stmt (s);
  return *this;
}

/* Return true is this is a valid range summary.  */
inline bool
range_stmt::valid () const
{
  return code != ERROR_MARK;
}

inline tree_code
range_stmt::get_code () const
{
  return code;
}

inline tree
range_stmt::operand1 () const
{
  return op1;
}

inline tree
range_stmt::operand2 () const
{
  return op2;
}

inline tree
range_stmt::ssa_operand1() const
{
  return ssa1;
}

inline tree
range_stmt::ssa_operand2 () const
{
  return ssa2;
}

inline irange_operator *
range_stmt::handler () const
{
  return irange_op_handler (code);
}

#endif /* GCC_SSA_RANGE_STMT_H */
