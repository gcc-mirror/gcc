/* Header file for mixed range operator class.
   Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

#ifndef GCC_RANGE_OP_MIXED_H
#define GCC_RANGE_OP_MIXED_H

enum bool_range_state { BRS_FALSE, BRS_TRUE, BRS_EMPTY, BRS_FULL };
bool_range_state get_bool_state (vrange &r, const vrange &lhs, tree val_type);

// If the range of either op1 or op2 is undefined, set the result to
// varying and return TRUE.  If the caller truly cares about a result,
// they should pass in a varying if it has an undefined that it wants
// treated as a varying.

inline bool
empty_range_varying (vrange &r, tree type,
		     const vrange &op1, const vrange & op2)
{
  if (op1.undefined_p () || op2.undefined_p ())
    {
      r.set_varying (type);
      return true;
    }
  else
    return false;
}

// For relation opcodes, first try to see if the supplied relation
// forces a true or false result, and return that.
// Then check for undefined operands.  If none of this applies,
// return false.

inline bool
relop_early_resolve (irange &r, tree type, const vrange &op1,
		     const vrange &op2, relation_trio trio,
		     relation_kind my_rel)
{
  relation_kind rel = trio.op1_op2 ();
  // If known relation is a complete subset of this relation, always true.
  if (relation_union (rel, my_rel) == my_rel)
    {
      r = range_true (type);
      return true;
    }

  // If known relation has no subset of this relation, always false.
  if (relation_intersect (rel, my_rel) == VREL_UNDEFINED)
    {
      r = range_false (type);
      return true;
    }

  // If either operand is undefined, return VARYING.
  if (empty_range_varying (r, type, op1, op2))
    return true;

  return false;
}

#endif // GCC_RANGE_OP_MIXED_H
