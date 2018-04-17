/* Header file for range operator class.
   Copyright (C) 2017 Free Software Foundation, Inc.
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

#ifndef GCC_RANGE_OP_H
#define GCC_RANGE_OP_H

/* This class is implmented for each kind of operator that is supported by 
   the range generator.  It serves dual purposes. 

   1 - Generates range information for the specific operation between
       the 4 possible combinations of integers and ranges. 
       This provides the ability to fold ranges for an expression.

   2 - Performs range algebra on the expression such that a range can be
       adjusted in terms of one of the operands:
         def = op1 + op2
       Given a range for def, we can possibly adjust the range so its in
       terms of either operand.
       op1_adjust (def_range, op2) will adjust the range in place so its
       in terms of op1.  since op1 = def - op2, it will subtract op2 from
       each element of the range.

   3 - Creates a range for an operand based on whether the result is 0 or
       non-zero.  Tihs is mostly for logical true false, but can serve other
       purposes.   
         ie   0 = op1 - op2 implies op2 has the same range as op1.

*/



class irange_operator
{
public:
  virtual void dump (FILE *f) const = 0;

  /* Set a range based on this operation between 2 operands.
     Return the TRUE if a valid range is created.  */
  virtual bool fold_range (irange& r, const irange& op1,
			   const irange& op2) const;

  /* Set the range for op? in the general case. R is the range for the LHS
     of the expression, VAL is the range for the other operand, and
     the result is returned in R.
     ie   [range] = op1 + VAL
     This is re-formed as  new_range = [range] - VAL.
     Return TRUE if the operation could be performd and the range is valid.  */
  virtual bool op1_irange (irange& r, const irange& lhs,
			   const irange& op2) const;
  virtual bool op2_irange (irange& r, const irange& lhs,
			   const irange& op1) const;
};

extern irange_operator *irange_op_handler(enum tree_code code);

#endif /* GCC_RANGE_OP_H */
