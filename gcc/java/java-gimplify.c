/* Java(TM) language-specific gimplification routines.
   Copyright (C) 2003, 2004, 2006, 2007, 2007, 2008
   Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>. 

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "java-tree.h"
#include "tree-dump.h"
#include "gimple.h"
#include "toplev.h"

static tree java_gimplify_block (tree);
static enum gimplify_status java_gimplify_modify_expr (tree *);
static enum gimplify_status java_gimplify_self_mod_expr (tree *, gimple_seq *,
							 gimple_seq *);

static void dump_java_tree (enum tree_dump_index, tree);

/* Convert a Java tree to GENERIC.  */

void
java_genericize (tree fndecl)
{
  dump_java_tree (TDI_original, fndecl);

  /* Genericize with the gimplifier.  */
  gimplify_function_tree (fndecl);

  dump_function (TDI_generic, fndecl);
}

/* Gimplify a Java tree.  */

int
java_gimplify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  enum tree_code code = TREE_CODE (*expr_p);

  switch (code)
    {
    case BLOCK:
      *expr_p = java_gimplify_block (*expr_p);
      break;

    case VAR_DECL:
      *expr_p = java_replace_reference (*expr_p, /* want_lvalue */ false);
      return GS_UNHANDLED;

    case MODIFY_EXPR:
      return java_gimplify_modify_expr (expr_p);

    case SAVE_EXPR:
      /* Note that we can see <save_expr NULL> if the save_expr was
	 already handled by gimplify_save_expr.  */
      if (TREE_OPERAND (*expr_p, 0) != NULL_TREE
	  && TREE_CODE (TREE_OPERAND (*expr_p, 0)) == VAR_DECL)
	TREE_OPERAND (*expr_p, 0) 
	  = java_replace_reference (TREE_OPERAND (*expr_p, 0), 
			       /* want_lvalue */ false);
      return GS_UNHANDLED;

    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
      return java_gimplify_self_mod_expr (expr_p, pre_p, post_p);
      
    /* These should already be lowered before we get here.  */
    case URSHIFT_EXPR:
    case COMPARE_EXPR:
    case COMPARE_L_EXPR:
    case COMPARE_G_EXPR:
      gcc_unreachable ();

    default:
      /* Java insists on strict left-to-right evaluation of expressions.
	 A problem may arise if a variable used in the LHS of a binary
	 operation is altered by an assignment to that value in the RHS
	 before we've performed the operation.  So, we always copy every
	 LHS to a temporary variable.  

	 FIXME: Are there any other cases where we should do this?
	 Parameter lists, maybe?  Or perhaps that's unnecessary because
	 the front end already generates SAVE_EXPRs.  */

      if (TREE_CODE_CLASS (code) == tcc_binary
	  || TREE_CODE_CLASS (code) == tcc_comparison)
	{
	  enum gimplify_status stat 
	    = gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
			     is_gimple_formal_tmp_var, fb_rvalue);
	  if (stat == GS_ERROR)
	    return stat;
	}

      return GS_UNHANDLED;
    }

  return GS_OK;
}

static enum gimplify_status
java_gimplify_modify_expr (tree *modify_expr_p)
{
  tree modify_expr = *modify_expr_p;
  tree lhs = TREE_OPERAND (modify_expr, 0);
  tree rhs = TREE_OPERAND (modify_expr, 1);
  tree lhs_type = TREE_TYPE (lhs);

  /* This is specific to the bytecode compiler.  If a variable has
     LOCAL_SLOT_P set, replace an assignment to it with an assignment
     to the corresponding variable that holds all its aliases.  */
  if (TREE_CODE (lhs) == VAR_DECL
      && DECL_LANG_SPECIFIC (lhs)
      && LOCAL_SLOT_P (lhs)
      && TREE_CODE (lhs_type) == POINTER_TYPE)
    {
      tree new_lhs = java_replace_reference (lhs, /* want_lvalue */ true);
      tree new_rhs = build1 (NOP_EXPR, TREE_TYPE (new_lhs), rhs);
      modify_expr = build2 (MODIFY_EXPR, TREE_TYPE (new_lhs),
			    new_lhs, new_rhs);
      modify_expr = build1 (NOP_EXPR, lhs_type, modify_expr);
    }
  else if (lhs_type != TREE_TYPE (rhs))
    /* Fix up type mismatches to make legal GIMPLE.  These are
       generated in several places, in particular null pointer
       assignment and subclass assignment.  */
    TREE_OPERAND (modify_expr, 1) = convert (lhs_type, rhs);

  *modify_expr_p = modify_expr;
  return GS_UNHANDLED;
}

/*  Special case handling for volatiles: we need to generate a barrier
    between the reading and the writing.  */

static enum gimplify_status
java_gimplify_self_mod_expr (tree *expr_p, gimple_seq *pre_p ATTRIBUTE_UNUSED, 
			     gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree lhs = TREE_OPERAND (*expr_p, 0);

  if (TREE_CODE (lhs) == COMPONENT_REF
      && TREE_THIS_VOLATILE (TREE_OPERAND (lhs, 1)))
    TREE_THIS_VOLATILE (lhs) = 1;

  return GS_UNHANDLED;
}

    
/* Gimplify BLOCK into a BIND_EXPR.  */

static tree
java_gimplify_block (tree java_block)
{
  tree decls = BLOCK_VARS (java_block);
  tree body = BLOCK_EXPR_BODY (java_block);
  gimple outer = gimple_current_bind_expr ();
  tree block;

  /* Don't bother with empty blocks.  */
  if (! body)
    return build_empty_stmt ();

  if (IS_EMPTY_STMT (body))
    return body;

  /* Make a proper block.  Java blocks are unsuitable for BIND_EXPR
     because they use BLOCK_SUBBLOCKS for another purpose.  */
  block = make_node (BLOCK);
  BLOCK_VARS (block) = decls;

  /* The TREE_USED flag on a block determines whether the debug output
     routines generate info for the variables in that block.  */
  TREE_USED (block) = 1;

  if (outer != NULL)
    {
      tree b = gimple_bind_block (outer);
      BLOCK_SUBBLOCKS (b) = chainon (BLOCK_SUBBLOCKS (b), block);
    }
  BLOCK_EXPR_BODY (java_block) = NULL_TREE;

  return build3 (BIND_EXPR, TREE_TYPE (java_block), decls, body, block);
}

/* Dump a tree of some kind.  This is a convenience wrapper for the
   dump_* functions in tree-dump.c.  */
static void
dump_java_tree (enum tree_dump_index phase, tree t)
{
  FILE *stream;
  int flags;

  stream = dump_begin (phase, &flags);
  flags |= TDF_SLIM;
  if (stream)
    {
      dump_node (t, flags, stream);
      dump_end (phase, stream);
    }
}
