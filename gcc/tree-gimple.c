/* Functions to analyze and validate GIMPLE trees.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>
   Rewritten by Jason Merrill <jason@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "ggc.h"
#include "tm.h"
#include "tree.h"
#include "tree-gimple.h"
#include "output.h"
#include "rtl.h"
#include "expr.h"
#include "bitmap.h"

/* GCC GIMPLE structure

   Inspired by the SIMPLE C grammar at

	http://www-acaps.cs.mcgill.ca/info/McCAT/McCAT.html

   function	: FUNCTION_DECL
			DECL_SAVED_TREE -> compound-stmt

   compound-stmt: STATEMENT_LIST
			members -> stmt

   stmt		: block
		| if-stmt
		| switch-stmt
		| goto-stmt
		| return-stmt
		| resx-stmt
		| label-stmt
		| try-stmt
		| modify-stmt
		| call-stmt

   block	: BIND_EXPR
			BIND_EXPR_VARS -> chain of DECLs
			BIND_EXPR_BLOCK -> BLOCK
			BIND_EXPR_BODY -> compound-stmt

   if-stmt	: COND_EXPR
			op0 -> condition
			op1 -> compound-stmt
			op2 -> compound-stmt

   switch-stmt	: SWITCH_EXPR
			op0 -> val
			op1 -> NULL
			op2 -> TREE_VEC of CASE_LABEL_EXPRs
			    The CASE_LABEL_EXPRs are sorted by CASE_LOW,
			    and default is last.

   goto-stmt	: GOTO_EXPR
			op0 -> LABEL_DECL | val

   return-stmt	: RETURN_EXPR
			op0 -> return-value

   return-value	: NULL
		| RESULT_DECL
		| MODIFY_EXPR
			op0 -> RESULT_DECL
			op1 -> lhs

   resx-stmt	: RESX_EXPR

   label-stmt	: LABEL_EXPR
			op0 -> LABEL_DECL

   try-stmt	: TRY_CATCH_EXPR
			op0 -> compound-stmt
			op1 -> handler
		| TRY_FINALLY_EXPR
			op0 -> compound-stmt
			op1 -> compound-stmt

   handler	: catch-seq
		| EH_FILTER_EXPR
		| compound-stmt

   catch-seq	: STATEMENT_LIST
			members -> CATCH_EXPR

   modify-stmt	: MODIFY_EXPR
			op0 -> lhs
			op1 -> rhs

   call-stmt	: CALL_EXPR
			op0 -> val | OBJ_TYPE_REF
			op1 -> call-arg-list

   call-arg-list: TREE_LIST
			members -> lhs

   addr-expr-arg: ID
		| compref

   lhs		: addr-expr-arg
		| '*' val
		| bitfieldref

   min-lval	: ID
		| '*' val

   bitfieldref	: BIT_FIELD_REF
			op0 -> inner-compref
			op1 -> CONST
			op2 -> var

   compref	: inner-compref
		| REALPART_EXPR
			op0 -> inner-compref
		| IMAGPART_EXPR
			op0 -> inner-compref

   inner-compref: min-lval
		| COMPONENT_REF
			op0 -> inner-compref
			op1 -> FIELD_DECL
			op2 -> val
		| ARRAY_REF
			op0 -> inner-compref
			op1 -> val
			op2 -> val
			op3 -> val
		| ARRAY_RANGE_REF
			op0 -> inner-compref
			op1 -> val
			op2 -> val
			op3 -> val
		| VIEW_CONVERT_EXPR
			op0 -> inner-compref

   condition	: val
		| val RELOP val

   val		: ID
		| CONST

   rhs		: lhs
		| CONST
		| '&' addr-expr-arg
		| call_expr
		| UNOP val
		| val BINOP val
		| val RELOP val
*/

static inline bool is_gimple_id (tree);

/* Validation of GIMPLE expressions.  */

/* Return true if T is a GIMPLE RHS.  */

bool
is_gimple_rhs (tree t)
{
  enum tree_code code = TREE_CODE (t);

  switch (TREE_CODE_CLASS (code))
    {
    case '1':
    case '2':
    case '<':
      return true;

    default:
      break;
    }

  switch (code)
    {
    case TRUTH_NOT_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case ADDR_EXPR:
    case CALL_EXPR:
    case CONSTRUCTOR:
    case COMPLEX_EXPR:
      /* FIXME lower VA_ARG_EXPR.  */
    case VA_ARG_EXPR:
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    case OBJ_TYPE_REF:
      return true;

    default:
      break;
    }

  return is_gimple_lvalue (t) || is_gimple_val (t);
}

/* Returns true if T is a valid CONSTRUCTOR component in GIMPLE, either
   a val or another CONSTRUCTOR.  */

bool
is_gimple_constructor_elt (tree t)
{
  return (is_gimple_val (t)
	  || TREE_CODE (t) == CONSTRUCTOR);
}

/*  Return true if T is a valid LHS for a GIMPLE assignment expression.  */

bool
is_gimple_lvalue (tree t)
{
  return (is_gimple_addr_expr_arg (t)
	  || TREE_CODE (t) == INDIRECT_REF
	  /* These are complex lvalues, but don't have addresses, so they
	     go here.  */
	  || TREE_CODE (t) == BIT_FIELD_REF);
}

/*  Return true if T is a GIMPLE condition.  */

bool
is_gimple_condexpr (tree t)
{
  return (is_gimple_val (t)
	  || TREE_CODE_CLASS (TREE_CODE (t)) == '<');
}

/*  Return true if T is a valid operand for ADDR_EXPR.  */

bool
is_gimple_addr_expr_arg (tree t)
{
  return (is_gimple_id (t)
	  || TREE_CODE (t) == ARRAY_REF
	  || TREE_CODE (t) == ARRAY_RANGE_REF
	  || TREE_CODE (t) == COMPONENT_REF
	  || TREE_CODE (t) == REALPART_EXPR
	  || TREE_CODE (t) == IMAGPART_EXPR
	  || TREE_CODE (t) == INDIRECT_REF);
}

/* Return true if T is function invariant.  Or rather a restricted
   form of function invariant.  */

bool
is_gimple_min_invariant (tree t)
{
  switch (TREE_CODE (t))
    {
    case ADDR_EXPR:
      return TREE_INVARIANT (t);

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
      return !TREE_OVERFLOW (t);

    default:
      return false;
    }
}

/* Return true if T looks like a valid GIMPLE statement.  */

bool
is_gimple_stmt (tree t)
{
  enum tree_code code = TREE_CODE (t);

  if (IS_EMPTY_STMT (t))
    return 1;

  switch (code)
    {
    case BIND_EXPR:
    case COND_EXPR:
      /* These are only valid if they're void.  */
      return TREE_TYPE (t) == NULL || VOID_TYPE_P (TREE_TYPE (t));

    case SWITCH_EXPR:
    case GOTO_EXPR:
    case RETURN_EXPR:
    case LABEL_EXPR:
    case CASE_LABEL_EXPR:
    case TRY_CATCH_EXPR:
    case TRY_FINALLY_EXPR:
    case EH_FILTER_EXPR:
    case CATCH_EXPR:
    case ASM_EXPR:
    case RESX_EXPR:
    case PHI_NODE:
    case STATEMENT_LIST:
      /* These are always void.  */
      return true;

    case VA_ARG_EXPR:
      /* FIXME this should be lowered.  */
      return true;

    case CALL_EXPR:
    case MODIFY_EXPR:
      /* These are valid regardless of their type.  */
      return true;

    default:
      return false;
    }
}

/* Return true if T is a variable.  */

bool
is_gimple_variable (tree t)
{
  return (TREE_CODE (t) == VAR_DECL
	  || TREE_CODE (t) == PARM_DECL
	  || TREE_CODE (t) == RESULT_DECL
	  || TREE_CODE (t) == SSA_NAME);
}

/*  Return true if T is a GIMPLE identifier (something with an address).  */

static inline bool
is_gimple_id (tree t)
{
  return (is_gimple_variable (t)
	  || TREE_CODE (t) == FUNCTION_DECL
	  || TREE_CODE (t) == LABEL_DECL
	  /* Allow string constants, since they are addressable.  */
	  || TREE_CODE (t) == STRING_CST);
}

/* Return true if TYPE is a suitable type for a scalar register variable.  */

bool
is_gimple_reg_type (tree type)
{
  return (!AGGREGATE_TYPE_P (type)
          && TREE_CODE (type) != COMPLEX_TYPE);
}


/* Return true if T is a scalar register variable.  */

bool
is_gimple_reg (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    t = SSA_NAME_VAR (t);

  return (is_gimple_variable (t)
	  && is_gimple_reg_type (TREE_TYPE (t))
	  /* A volatile decl is not acceptable because we can't reuse it as
	     needed.  We need to copy it into a temp first.  */
	  && ! TREE_THIS_VOLATILE (t)
	  && ! TREE_ADDRESSABLE (t)
	  && ! needs_to_live_in_memory (t));
}

/* Return true if T is a GIMPLE variable whose address is not needed.  */

bool
is_gimple_non_addressable (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    t = SSA_NAME_VAR (t);

  return (is_gimple_variable (t)
	  && ! TREE_ADDRESSABLE (t)
	  && ! needs_to_live_in_memory (t));
}

/* Return true if T is a GIMPLE rvalue, i.e. an identifier or a constant.  */

bool
is_gimple_val (tree t)
{
  /* Make loads from volatiles and memory vars explicit.  */
  if (is_gimple_variable (t)
      && is_gimple_reg_type (TREE_TYPE (t))
      && !is_gimple_reg (t))
    return false;

  /* FIXME make these decls.  That can happen only when we expose the
     entire landing-pad construct at the tree level.  */
  if (TREE_CODE (t) == EXC_PTR_EXPR || TREE_CODE (t) == FILTER_EXPR)
    return 1;

  return (is_gimple_variable (t) || is_gimple_min_invariant (t));
}


/* Return true if T is a GIMPLE minimal lvalue.  */

bool
is_gimple_min_lval (tree t)
{
  return (is_gimple_id (t)
	  || TREE_CODE (t) == INDIRECT_REF);
}

/* Return true if T is a typecast operation.  */

bool
is_gimple_cast (tree t)
{
  return (TREE_CODE (t) == NOP_EXPR
	  || TREE_CODE (t) == CONVERT_EXPR
          || TREE_CODE (t) == FIX_TRUNC_EXPR
          || TREE_CODE (t) == FIX_CEIL_EXPR
          || TREE_CODE (t) == FIX_FLOOR_EXPR
          || TREE_CODE (t) == FIX_ROUND_EXPR);
}

/* Return true if T is a valid op0 of a CALL_EXPR.  */

bool
is_gimple_call_addr (tree t)
{
  return (TREE_CODE (t) == OBJ_TYPE_REF
	  || is_gimple_val (t));
}

/* If T makes a function call, return the corresponding CALL_EXPR operand.
   Otherwise, return NULL_TREE.  */

tree
get_call_expr_in (tree t)
{
  if (TREE_CODE (t) == MODIFY_EXPR)
    t = TREE_OPERAND (t, 1);
  if (TREE_CODE (t) == CALL_EXPR)
    return t;
  return NULL_TREE;
}

/* Given a memory reference expression, return the base address.  Note that,
   in contrast with get_base_var, this will not recurse inside INDIRECT_REF
   expressions.  Therefore, given the reference PTR->FIELD, this function
   will return *PTR.  Whereas get_base_var would've returned PTR.  */

tree
get_base_address (tree t)
{
  while (TREE_CODE (t) == REALPART_EXPR || TREE_CODE (t) == IMAGPART_EXPR
	 || handled_component_p (t))
    t = TREE_OPERAND (t, 0);
  
  if (SSA_VAR_P (t)
      || TREE_CODE (t) == STRING_CST
      || TREE_CODE (t) == CONSTRUCTOR
      || TREE_CODE (t) == INDIRECT_REF)
    return t;
  else
    return NULL_TREE;
}

void
recalculate_side_effects (tree t)
{
  enum tree_code code = TREE_CODE (t);
  int fro = first_rtl_op (code);
  int i;

  switch (TREE_CODE_CLASS (code))
    {
    case 'e':
      switch (code)
	{
	case INIT_EXPR:
	case MODIFY_EXPR:
	case VA_ARG_EXPR:
	case RTL_EXPR:
	case PREDECREMENT_EXPR:
	case PREINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	case POSTINCREMENT_EXPR:
	  /* All of these have side-effects, no matter what their
	     operands are.  */
	  return;

	default:
	  break;
	}
      /* Fall through.  */

    case '<':  /* a comparison expression */
    case '1':  /* a unary arithmetic expression */
    case '2':  /* a binary arithmetic expression */
    case 'r':  /* a reference */
      TREE_SIDE_EFFECTS (t) = TREE_THIS_VOLATILE (t);
      for (i = 0; i < fro; ++i)
	{
	  tree op = TREE_OPERAND (t, i);
	  if (op && TREE_SIDE_EFFECTS (op))
	    TREE_SIDE_EFFECTS (t) = 1;
	}
      break;
   }
}
