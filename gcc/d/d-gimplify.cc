/* D-specific tree lowering bits; see also gimple.cc.
   Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "dmd/globals.h"

#include "tree.h"
#include "gimple-expr.h"
#include "gimplify.h"

#include "d-tree.h"


/* Return TRUE if an operand OP of a given TYPE being copied has no data.
   The middle-end does a similar check with zero sized types.  */

static bool
empty_modify_p (tree type, tree op)
{
  tree_code code = TREE_CODE (op);
  switch (code)
    {
    case COMPOUND_EXPR:
      return empty_modify_p (type, TREE_OPERAND (op, 1));

    case CONSTRUCTOR:
      /* Non-empty construcors are valid.  */
      if (CONSTRUCTOR_NELTS (op) != 0 || TREE_CLOBBER_P (op))
	return false;
      break;

    case CALL_EXPR:
      /* Leave nrvo alone because it isn't a copy.  */
      if (CALL_EXPR_RETURN_SLOT_OPT (op))
	return false;
      break;

    default:
      /* If the operand doesn't have a simple form.  */
      if (!is_gimple_lvalue (op) && !INDIRECT_REF_P (op))
	return false;
      break;
    }

  return empty_aggregate_p (type);
}

/* Return TRUE if EXPR is a COMPONENT_REF to a bit-field declaration.  */

static bool
bit_field_ref (const_tree expr)
{
  if (TREE_CODE (expr) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (expr, 1)))
    return true;

  return false;
}

/* Gimplify assignment from an INIT_EXPR or MODIFY_EXPR.  */

static gimplify_status
d_gimplify_modify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  tree op0 = TREE_OPERAND (*expr_p, 0);
  tree op1 = TREE_OPERAND (*expr_p, 1);

  if (error_operand_p (op0) || error_operand_p (op1))
    return GS_UNHANDLED;

  /* Remove any copies of empty aggregates.  */
  if (empty_modify_p (TREE_TYPE (op0), op1))
    {
      gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
		     is_gimple_lvalue, fb_lvalue);

      if (TREE_SIDE_EFFECTS (op1))
	gimplify_and_add (op1, pre_p);

      *expr_p = TREE_OPERAND (*expr_p, 0);
      return GS_OK;
    }

  /* If the back end isn't clever enough to know that the lhs and rhs
     types are the same, add an explicit conversion.  */
  if ((AGGREGATE_TYPE_P (TREE_TYPE (op0)) || AGGREGATE_TYPE_P (TREE_TYPE (op1)))
      && !useless_type_conversion_p (TREE_TYPE (op1), TREE_TYPE (op0)))
    {
      TREE_OPERAND (*expr_p, 1) = build1 (VIEW_CONVERT_EXPR,
					  TREE_TYPE (op0), op1);
      return GS_OK;
    }

  /* Same as above, but for bit-field assignments.  */
  if ((bit_field_ref (op0) || bit_field_ref (op1))
      && TREE_TYPE (op0) != TREE_TYPE (op1))
    {
      TREE_OPERAND (*expr_p, 1) = convert (TREE_TYPE (op0), op1);
      return GS_OK;
    }

  return GS_UNHANDLED;
}

/* Gimplify an ADDR_EXPR node.  */

static gimplify_status
d_gimplify_addr_expr (tree *expr_p)
{
  tree op0 = TREE_OPERAND (*expr_p, 0);
  /* Constructors are not lvalues, so make them one.  */
  if (TREE_CODE (op0) == CONSTRUCTOR)
    {
      TREE_OPERAND (*expr_p, 0) = force_target_expr (op0);
      return GS_OK;
    }

  return GS_UNHANDLED;
}

/* Gimplify a CALL_EXPR node.  */

static gimplify_status
d_gimplify_call_expr (tree *expr_p, gimple_seq *pre_p)
{
  /* Strictly evaluate all arguments from left to right.  */
  int nargs = call_expr_nargs (*expr_p);
  location_t loc = EXPR_LOC_OR_LOC (*expr_p, input_location);

  /* No need to enforce evaluation order if only one argument.  */
  if (nargs < 2)
    return GS_UNHANDLED;

  /* Or if all arguments are already free of side-effects.  */
  bool has_side_effects = false;
  for (int i = 0; i < nargs; i++)
    {
      if (TREE_SIDE_EFFECTS (CALL_EXPR_ARG (*expr_p, i)))
	{
	  has_side_effects = true;
	  break;
	}
    }

  if (!has_side_effects)
    return GS_UNHANDLED;

  /* Evaluate the callee before calling it.  */
  tree new_call_fn = CALL_EXPR_FN (*expr_p);

  if (gimplify_expr (&new_call_fn, pre_p, NULL,
		     is_gimple_call_addr, fb_rvalue) == GS_ERROR)
    return GS_ERROR;

  CALL_EXPR_FN (*expr_p) = new_call_fn;

  /* Leave the last argument for gimplify_call_expr.  */
  for (int i = 0; i < nargs - 1; i++)
    {
      tree new_arg = CALL_EXPR_ARG (*expr_p, i);

      /* If argument has a side-effect, gimplify_arg will handle it.  */
      if (gimplify_arg (&new_arg, pre_p, loc) == GS_ERROR)
	return GS_ERROR;

      /* Even if an argument itself doesn't have any side-effects, it
	 might be altered by another argument in the list.  */
      if (new_arg == CALL_EXPR_ARG (*expr_p, i)
	  && !really_constant_p (new_arg))
	new_arg = get_formal_tmp_var (new_arg, pre_p);

      CALL_EXPR_ARG (*expr_p, i) = new_arg;
    }

  return GS_OK;
}

/* Gimplify an UNSIGNED_RSHIFT_EXPR node.  */

static gimplify_status
d_gimplify_unsigned_rshift_expr (tree *expr_p)
{
  /* Convert op0 to an unsigned type.  */
  tree op0 = TREE_OPERAND (*expr_p, 0);
  tree op1 = TREE_OPERAND (*expr_p, 1);
  tree type = d_unsigned_type (TREE_TYPE (op0));

  *expr_p = convert (TREE_TYPE (*expr_p),
		     build2 (RSHIFT_EXPR, type, convert (type, op0), op1));
  return GS_OK;
}

/* Gimplify an unary expression node.  */

static gimplify_status
d_gimplify_unary_expr (tree *expr_p)
{
  tree op0 = TREE_OPERAND (*expr_p, 0);

  if (error_operand_p (op0))
    return GS_UNHANDLED;

  /* Front end doesn't know that bit-field types are really different
     from basic types, add an explicit conversion in unary expressions.  */
  if (bit_field_ref (op0) && TREE_TYPE (op0) != TREE_TYPE (*expr_p))
    {
      TREE_OPERAND (*expr_p, 0) = convert (TREE_TYPE (*expr_p), op0);
      return GS_OK;
    }

  return GS_UNHANDLED;
}

/* Gimplify a binary expression node.  */

static gimplify_status
d_gimplify_binary_expr (tree *expr_p)
{
  tree op0 = TREE_OPERAND (*expr_p, 0);
  tree op1 = TREE_OPERAND (*expr_p, 1);

  if (error_operand_p (op0) || error_operand_p (op1))
    return GS_UNHANDLED;

  /* Front end doesn't know that bit-field types are really different
     from basic types, add an explicit conversion in binary expressions.  */
  if (bit_field_ref (op0) || bit_field_ref (op1))
    {
      if (TREE_TYPE (op0) != TREE_TYPE (*expr_p))
	TREE_OPERAND (*expr_p, 0) = convert (TREE_TYPE (*expr_p), op0);

      if (TREE_TYPE (op1) != TREE_TYPE (*expr_p))
	TREE_OPERAND (*expr_p, 1) = convert (TREE_TYPE (*expr_p), op1);

      return GS_OK;
    }

  return GS_UNHANDLED;
}

/* Implements the lang_hooks.gimplify_expr routine for language D.
   Do gimplification of D specific expression trees in EXPR_P.  */

int
d_gimplify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  switch (TREE_CODE (*expr_p))
    {
    case INIT_EXPR:
    case MODIFY_EXPR:
      return d_gimplify_modify_expr (expr_p, pre_p, post_p);

    case ADDR_EXPR:
      return d_gimplify_addr_expr (expr_p);

    case CALL_EXPR:
      return d_gimplify_call_expr (expr_p, pre_p);

    case UNSIGNED_RSHIFT_EXPR:
      return d_gimplify_unsigned_rshift_expr (expr_p);

    case FLOAT_MOD_EXPR:
      gcc_unreachable ();

    default:
      if (UNARY_CLASS_P (*expr_p) && !CONVERT_EXPR_P (*expr_p))
	return d_gimplify_unary_expr (expr_p);
      if (BINARY_CLASS_P (*expr_p))
	return d_gimplify_binary_expr (expr_p);
      break;
    }

  return GS_UNHANDLED;
}
