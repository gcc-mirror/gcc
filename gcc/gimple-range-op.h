/* Header file for the GIMPLE range-op interface.
   Copyright (C) 2022-2023 Free Software Foundation, Inc.
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

#ifndef GCC_GIMPLE_RANGE_OP_H
#define GCC_GIMPLE_RANGE_OP_H

#include "range-op.h"


class gimple_range_op_handler : public range_op_handler
{
public:
  static bool supported_p (gimple *s);
  gimple_range_op_handler (gimple *s);
  inline gimple *stmt () const { return m_stmt; }
  inline tree lhs () const { return gimple_get_lhs (m_stmt); }
  tree operand1 () const { gcc_checking_assert (m_operator); return m_op1; }
  tree operand2 () const { gcc_checking_assert (m_operator); return m_op2; }
  bool calc_op1 (vrange &r, const vrange &lhs_range);
  bool calc_op1 (vrange &r, const vrange &lhs_range, const vrange &op2_range,
		 relation_trio = TRIO_VARYING);
  bool calc_op2 (vrange &r, const vrange &lhs_range, const vrange &op1_range,
		 relation_trio = TRIO_VARYING);
private:
  void maybe_builtin_call ();
  void maybe_non_standard ();
  gimple *m_stmt;
  tree m_op1, m_op2;
};

// Given stmt S, fill VEC, up to VEC_SIZE elements, with relevant ssa-names
// on the statement.  For efficiency, it is an error to not pass in enough
// elements for the vector.  Return the number of ssa-names.

unsigned gimple_range_ssa_names (tree *vec, unsigned vec_size, gimple *stmt);

#endif // GCC_GIMPLE_RANGE_OP_H
