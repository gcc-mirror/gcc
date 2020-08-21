/* D-specific tree lowering bits; see also gimple.c.
   Copyright (C) 2020 Free Software Foundation, Inc.

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

/* Implements the lang_hooks.gimplify_expr routine for language D.
   Do gimplification of D specific expression trees in EXPR_P.  */

int
d_gimplify_expr (tree *expr_p, gimple_seq *pre_p,
		 gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree_code code = TREE_CODE (*expr_p);
  enum gimplify_status ret = GS_UNHANDLED;
  tree op0, op1;
  tree type;

  switch (code)
    {
    case INIT_EXPR:
    case MODIFY_EXPR:
      op0 = TREE_OPERAND (*expr_p, 0);
      op1 = TREE_OPERAND (*expr_p, 1);

      if (!error_operand_p (op0) && !error_operand_p (op1)
	  && (AGGREGATE_TYPE_P (TREE_TYPE (op0))
	      || AGGREGATE_TYPE_P (TREE_TYPE (op1)))
	  && !useless_type_conversion_p (TREE_TYPE (op1), TREE_TYPE (op0)))
	{
	  /* If the back end isn't clever enough to know that the lhs and rhs
	     types are the same, add an explicit conversion.  */
	  TREE_OPERAND (*expr_p, 1) = build1 (VIEW_CONVERT_EXPR,
					      TREE_TYPE (op0), op1);
	  ret = GS_OK;
	}
      else if (empty_modify_p (TREE_TYPE (op0), op1))
	{
	  /* Remove any copies of empty aggregates.  */
	  gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
			 is_gimple_lvalue, fb_lvalue);

	  if (TREE_SIDE_EFFECTS (op1))
	    gimplify_and_add (op1, pre_p);

	  *expr_p = TREE_OPERAND (*expr_p, 0);
	  ret = GS_OK;
	}
      break;

    case ADDR_EXPR:
      op0 = TREE_OPERAND (*expr_p, 0);
      /* Constructors are not lvalues, so make them one.  */
      if (TREE_CODE (op0) == CONSTRUCTOR)
	{
	  TREE_OPERAND (*expr_p, 0) = force_target_expr (op0);
	  ret = GS_OK;
	}
      break;

    case CALL_EXPR:
      if (CALL_EXPR_ARGS_ORDERED (*expr_p))
	{
	  /* Strictly evaluate all arguments from left to right.  */
	  int nargs = call_expr_nargs (*expr_p);
	  location_t loc = EXPR_LOC_OR_LOC (*expr_p, input_location);

	  /* No need to enforce evaluation order if only one argument.  */
	  if (nargs < 2)
	    break;

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
	    break;

	  /* Leave the last argument for gimplify_call_expr.  */
	  for (int i = 0; i < nargs - 1; i++)
	    {
	      tree new_arg = CALL_EXPR_ARG (*expr_p, i);

	      /* If argument has a side-effect, gimplify_arg will handle it.  */
	      if (gimplify_arg (&new_arg, pre_p, loc) == GS_ERROR)
		ret = GS_ERROR;

	      /* Even if an argument itself doesn't have any side-effects, it
		 might be altered by another argument in the list.  */
	      if (new_arg == CALL_EXPR_ARG (*expr_p, i)
		  && !really_constant_p (new_arg))
		new_arg = get_formal_tmp_var (new_arg, pre_p);

	      CALL_EXPR_ARG (*expr_p, i) = new_arg;
	    }

	  if (ret != GS_ERROR)
	    ret = GS_OK;
	}
      break;

    case UNSIGNED_RSHIFT_EXPR:
      /* Convert op0 to an unsigned type.  */
      op0 = TREE_OPERAND (*expr_p, 0);
      op1 = TREE_OPERAND (*expr_p, 1);

      type = d_unsigned_type (TREE_TYPE (op0));

      *expr_p = convert (TREE_TYPE (*expr_p),
			 build2 (RSHIFT_EXPR, type, convert (type, op0), op1));
      ret = GS_OK;
      break;

    case FLOAT_MOD_EXPR:
      gcc_unreachable ();

    default:
      break;
    }

  return ret;
}
