/* SSA range statement summary.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

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
#include "backend.h"
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "ssa-range-stmt.h"


/* This routine will return what  is globally known about the range for an
   operand of any kind.  */
bool
get_operand_range (irange& r, tree op)
{
  /* This check allows unary operations to be handled without having to 
     make an explicit check for the existence of a second operand.  */
  if (!op)
    return false;

  if (TREE_CODE (op) == INTEGER_CST)
    r.set_range (TREE_TYPE (op), op, op);
  else
    if (TREE_CODE (op) == SSA_NAME)
      r = op;
    else
      if (TYPE_P (op))
	r.set_range_for_type (op);
      else
        /* Default to range for the type of the expression.   */
	r.set_range_for_type (TREE_TYPE (op));

  return true;
}


/* Initialize the SSA operands and validate that all operands of this
   expression are operable on iranges. 
   Return ERROR_MARK if they are not, otherwise the current tree code.  */
tree_code
range_stmt::validate_operands ()
{
  ssa1 = valid_irange_ssa (op1);
  ssa2 = valid_irange_ssa (op2);

  if (ssa1 || (TREE_CODE (op1) == INTEGER_CST && !TREE_OVERFLOW (op1))) 
   {
     if (!op2)
       return code;
     if (ssa2 || (TREE_CODE (op2) == INTEGER_CST && !TREE_OVERFLOW (op2)))
       return code;
   }
  return ERROR_MARK;
}

/* Build a range node from a stmt, if it possible.  */
void
range_stmt::from_stmt (gimple *s)
{
  switch (gimple_code (s))
    {
      case GIMPLE_COND:
        {
	  gcond *gc = as_a <gcond *> (s);
	  code  = gimple_cond_code (gc);
	  if (irange_op_handler (code))
	    {
	      lhs = gimple_get_lhs (s);
	      op1 = gimple_cond_lhs (gc);
	      op2 = gimple_cond_rhs (gc);
	      code = validate_operands ();
	      return;
	    }
	  break;
	}
      case GIMPLE_ASSIGN:
	{
	  gassign *ga = as_a <gassign *> (s);
	  code = gimple_assign_rhs_code (ga);
	  if (irange_op_handler (code))
	    {
	      lhs = gimple_get_lhs (s);
	      op1 = gimple_assign_rhs1 (ga);
	      if (get_gimple_rhs_class (code) == GIMPLE_BINARY_RHS)
		op2 = gimple_assign_rhs2 (ga);
	      else
		op2 = NULL;
	      code = validate_operands ();
	      return;
	    }
	  break;
	}

      default:
        break;
    }
  code = ERROR_MARK;
  return;
}

/* This method will attempt to resolve a unary expression with value R1 to
   a range.  If the expression can be resolved, true is returned, and the
   range is returned in RES.  */

bool
range_stmt::fold (irange &res, const irange& r1) const
{
  irange r2;
  irange_operator *handler = irange_op_handler (code);
  gcc_assert (handler != NULL);

  /* Single ssa operations require the LHS type as the second range.  */
  if (lhs)
    r2.set_range_for_type (TREE_TYPE (lhs));
  else
    r2.clear ();
  return handler->fold_range (res, r1, r2);
}

/* This method will attempt to resolve a binary expression with operand
   values R1 tnd R2 to a range.  If the expression can be resolved, true is
   returned, and the range is returned in RES.  */

bool
range_stmt::fold (irange &res, const irange& r1, const irange& r2) const
{
  irange_operator *handler = irange_op_handler (code);
  gcc_assert (handler != NULL);

  // Make sure this isnt a unary operation being passed a second range.
  gcc_assert (op2);
  return handler->fold_range (res, r1, r2);
}

/* This method will attempt to evaluate the epression using whatever is
   globally known about the operands.  If it can be evaluated, TRUE is returned
   and the range is returned in RES.  */

bool
range_stmt::fold (irange &res) const
{
  irange r1, r2;
  
  if (!op2)
    {
      get_operand_range (r1, op1);
      return fold (res, r1);
    }

  get_operand_range (r1, op1);
  get_operand_range (r2, op2);
  return fold (res, r1, r2);
}

/* This method will attempt to evaluate the expression by replacing any
   occurrence of ssa_name NAME with the range NAME_RANGE. If it can be
   evaluated, TRUE is returned and the resulting range returned in RES.  */
bool
range_stmt::fold (irange& res, tree name, const irange& name_range) const
{
  irange r1, r2;

  if (!op2)
    {
      if (ssa1 == name)
	r1 = name_range;
      else
	get_operand_range (r1, op1);
      return fold (res, r1);
    }

  if (ssa1 == name)
    r1 = name_range;
  else
    get_operand_range (r1, op1);

  if (ssa2 == name)
    r2 = name_range;
  else
    get_operand_range (r2, op2);

  return fold (res, r1, r2);
}

/* This method will evaluate a range for the operand of a unary expression
   given a range for the LHS of the expression in LHS_RANGE. If it can be
   evaluated, TRUE is returned and the resulting range returned in RES.  */
bool
range_stmt::op1_irange (irange& r, const irange& lhs_range) const
{  
  irange type_range;
  type_range.set_range_for_type (TREE_TYPE (op1));
  return handler ()->op1_irange (r, lhs_range, type_range);
}

/* This method will evaluate a range for operand 1 of a binary expression
   given a range for the LHS in LHS_RANGE and a range for operand 2 in
   OP2_RANGE. If it can be evaluated, TRUE is returned and the resulting
   range returned in RES.  */
bool
range_stmt::op1_irange (irange& r, const irange& lhs_range,
			const irange& op2_range) const
{  
  gcc_assert (op2 != NULL);
  return handler ()->op1_irange (r, lhs_range, op2_range);
}

/* This method will evaluate a range for operand 2 of a binary expression
   given a range for the LHS in LHS_RANGE and a range for operand 1 in
   OP1_RANGE. If it can be evaluated, TRUE is returned and the resulting
   range returned in RES.  */
bool
range_stmt::op2_irange (irange& r, const irange& lhs_range,
			const irange& op1_range) const
{  
  return handler ()->op2_irange (r, lhs_range, op1_range);
}

/* This method will dump the internal state of the statement summary.  */
void
range_stmt::dump (FILE *f) const
{
  if (lhs)
    {
      print_generic_expr (f, lhs, TDF_SLIM);
      fprintf (f, " = ");
    }

  if (!op2)
    irange_op_handler (code)->dump (f);

  print_generic_expr (f, op1, TDF_SLIM);

  if (op2)
    {
      irange_op_handler (code)->dump (f);
      print_generic_expr (f, op2, TDF_SLIM);
    }

}



