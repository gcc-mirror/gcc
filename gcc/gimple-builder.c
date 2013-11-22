/* Functions for high level gimple building routines.
   Copyright (C) 2013 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "stringpool.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "tree-ssanames.h"


/* Return the expression type to use based on the CODE and type of
   the given operand OP.  If the expression CODE is a comparison,
   the returned type is boolean_type_node.  Otherwise, it returns
   the type of OP.  */

static tree
get_expr_type (enum tree_code code, tree op)
{
  return (TREE_CODE_CLASS (code) == tcc_comparison)
	 ? boolean_type_node
	 : TREE_TYPE (op);
}


/* Build a new gimple assignment.  The LHS of the assignment is a new
   temporary whose type matches the given expression.  MODE indicates
   whether the LHS should be an SSA or a normal temporary.  CODE is
   the expression code for the RHS.  OP1 is the first operand and VAL
   is an integer value to be used as the second operand.  */

gimple
build_assign (enum tree_code code, tree op1, int val, tree lhs)
{
  tree op2 = build_int_cst (TREE_TYPE (op1), val);
  if (lhs == NULL_TREE)
    lhs = make_ssa_name (get_expr_type (code, op1), NULL);
  return gimple_build_assign_with_ops (code, lhs, op1, op2);
}

gimple
build_assign (enum tree_code code, gimple g, int val, tree lhs )
{
  return build_assign (code, gimple_assign_lhs (g), val, lhs);
}


/* Build and return a new GIMPLE assignment.  The new assignment will
   have the opcode CODE and operands OP1 and OP2.  The type of the
   expression on the RHS is inferred to be the type of OP1.

   The LHS of the statement will be an SSA name or a GIMPLE temporary
   in normal form depending on the type of builder invoking this
   function.  */

gimple
build_assign (enum tree_code code, tree op1, tree op2, tree lhs)
{
  if (lhs == NULL_TREE)
    lhs = make_ssa_name (get_expr_type (code, op1), NULL);
  return gimple_build_assign_with_ops (code, lhs, op1, op2);
}

gimple
build_assign (enum tree_code code, gimple op1, tree op2, tree lhs)
{
  return build_assign (code, gimple_assign_lhs (op1), op2, lhs);
}

gimple
build_assign (enum tree_code code, tree op1, gimple op2, tree lhs)
{
  return build_assign (code, op1, gimple_assign_lhs (op2), lhs);
}

gimple
build_assign (enum tree_code code, gimple op1, gimple op2, tree lhs)
{
  return build_assign (code, gimple_assign_lhs (op1), gimple_assign_lhs (op2),
                       lhs);
}


/* Create and return a type cast assignment. This creates a NOP_EXPR
   that converts OP to TO_TYPE.  */

gimple
build_type_cast (tree to_type, tree op, tree lhs)
{
  if (lhs == NULL_TREE)
    lhs = make_ssa_name (to_type, NULL);
  return gimple_build_assign_with_ops (NOP_EXPR, lhs, op, NULL_TREE);
}

gimple
build_type_cast (tree to_type, gimple op, tree lhs)
{
  return build_type_cast (to_type, gimple_assign_lhs (op), lhs);
}



