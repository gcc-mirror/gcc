/* Java(TM) language-specific gimplification routines.

   Copyright (C) 2003, 2004 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA. 

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
#include "tree-simple.h"
#include "toplev.h"

static tree java_gimplify_case_expr (tree);
static tree java_gimplify_default_expr (tree);
static tree java_gimplify_block (tree);
static tree java_gimplify_new_array_init (tree);
static tree java_gimplify_try_expr (tree);

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
java_gimplify_expr (tree *expr_p, tree *pre_p ATTRIBUTE_UNUSED,
		    tree *post_p ATTRIBUTE_UNUSED)
{
  char code_class = TREE_CODE_CLASS(TREE_CODE (*expr_p));

  /* Java insists on strict left-to-right evaluation of expressions.
     A problem may arise if a variable used in the LHS of a binary
     operation is altered by an assignment to that value in the RHS
     before we've performed the operation.  So, we always copy every
     LHS to a temporary variable.  

     FIXME: Are there any other cases where we should do this?
     Parameter lists, maybe?  Or perhaps that's unnecessary because
     the front end already generates SAVE_EXPRs.  */
  if (code_class == '2')
    {
      tree lhs = TREE_OPERAND (*expr_p, 0);
      enum gimplify_status stat 
	= gimplify_expr (&lhs, pre_p, post_p, is_gimple_tmp_var, fb_rvalue);
      if (stat == GS_ERROR)
	return stat;
      TREE_OPERAND (*expr_p, 0) = lhs;
    }

  switch (TREE_CODE (*expr_p))
    {
    case BLOCK:
      *expr_p = java_gimplify_block (*expr_p);
      break;

    case EXPR_WITH_FILE_LOCATION:
      input_location.file = EXPR_WFL_FILENAME (*expr_p);
      input_location.line = EXPR_WFL_LINENO (*expr_p);
      *expr_p = EXPR_WFL_NODE (*expr_p);
      annotate_with_locus (*expr_p, input_location);
      break;

    case CASE_EXPR:
      *expr_p = java_gimplify_case_expr (*expr_p);
      break;

    case DEFAULT_EXPR:
      *expr_p = java_gimplify_default_expr (*expr_p);
      break;

    case NEW_ARRAY_INIT:
      *expr_p = java_gimplify_new_array_init (*expr_p);
      break;

    case TRY_EXPR:
      *expr_p = java_gimplify_try_expr (*expr_p);
      break;

    case JAVA_CATCH_EXPR:
      *expr_p = TREE_OPERAND (*expr_p, 0);
      break;

    case JAVA_EXC_OBJ_EXPR:
      *expr_p = build_exception_object_ref (TREE_TYPE (*expr_p));
      break;

    /* These should already be lowered before we get here.  */
    case URSHIFT_EXPR:
    case COMPARE_EXPR:
    case COMPARE_L_EXPR:
    case COMPARE_G_EXPR:
    case UNARY_PLUS_EXPR:
    case NEW_ARRAY_EXPR:
    case NEW_ANONYMOUS_ARRAY_EXPR:
    case NEW_CLASS_EXPR:
    case THIS_EXPR:
    case SYNCHRONIZED_EXPR:
    case CONDITIONAL_EXPR:
    case INSTANCEOF_EXPR:
    case CLASS_LITERAL:
      abort ();

    default:
      return GS_UNHANDLED;
    }

  return GS_OK;
}

static tree
java_gimplify_case_expr (tree expr)
{
  tree label = create_artificial_label ();
  return build (CASE_LABEL_EXPR, void_type_node,
		TREE_OPERAND (expr, 0), NULL_TREE, label);
}

static tree
java_gimplify_default_expr (tree expr ATTRIBUTE_UNUSED)
{
  tree label = create_artificial_label ();
  return build (CASE_LABEL_EXPR, void_type_node, NULL_TREE, NULL_TREE, label);
}

/* Gimplify BLOCK into a BIND_EXPR.  */

static tree
java_gimplify_block (tree java_block)
{
  tree decls = BLOCK_VARS (java_block);
  tree body = BLOCK_EXPR_BODY (java_block);
  tree outer = gimple_current_bind_expr ();
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
  if (outer != NULL_TREE)
    {
      outer = BIND_EXPR_BLOCK (outer);
      BLOCK_SUBBLOCKS (outer) = chainon (BLOCK_SUBBLOCKS (outer), block);
    }

  return build (BIND_EXPR, TREE_TYPE (java_block), decls, body, block);
}

/* Gimplify a NEW_ARRAY_INIT node into array/element assignments.  */

static tree
java_gimplify_new_array_init (tree exp)
{
  tree array_type = TREE_TYPE (TREE_TYPE (exp));
  tree data_field = lookup_field (&array_type, get_identifier ("data"));
  tree element_type = TYPE_ARRAY_ELEMENT (array_type);
  HOST_WIDE_INT ilength = java_array_type_length (array_type);
  tree length = build_int_2 (ilength, 0);
  tree init = TREE_OPERAND (exp, 0);
  tree values = CONSTRUCTOR_ELTS (init);

  tree array_ptr_type = build_pointer_type (array_type);
  tree block = build (BLOCK, array_ptr_type);
  tree tmp = build_decl (VAR_DECL, get_identifier ("<tmp>"), array_ptr_type);
  tree array = build_decl (VAR_DECL, get_identifier ("<array>"), array_ptr_type);
  tree body = build (MODIFY_EXPR, array_ptr_type, tmp,
		     build_new_array (element_type, length));

  int index = 0;

  DECL_CONTEXT (array) = current_function_decl;
  DECL_CONTEXT (tmp) = current_function_decl;

  /* FIXME: try to allocate array statically?  */
  while (values != NULL_TREE)
    {
      /* FIXME: Should use build_java_arrayaccess here, but avoid
	 bounds checking.  */
      tree lhs = build (COMPONENT_REF, TREE_TYPE (data_field),    
			build_java_indirect_ref (array_type, tmp, 0),
			data_field);
      tree assignment = build (MODIFY_EXPR, element_type,
  			       build (ARRAY_REF, element_type, lhs,
				      build_int_2 (index++, 0)),
			       TREE_VALUE (values));
      body = build (COMPOUND_EXPR, element_type, body, assignment);
      values = TREE_CHAIN (values);
    }

  body = build (COMPOUND_EXPR, array_ptr_type, body,
		build (MODIFY_EXPR, array_ptr_type, array, tmp));
  TREE_CHAIN (tmp) = array;
  BLOCK_VARS (block) = tmp;
  BLOCK_EXPR_BODY (block) = body;
  return java_gimplify_block (block);
}

static tree
java_gimplify_try_expr (tree try_expr)
{
  tree body = TREE_OPERAND (try_expr, 0);
  tree handler = TREE_OPERAND (try_expr, 1);
  tree catch = NULL_TREE;

  /* Build a CATCH_EXPR for each handler.  */
  while (handler)
    {
      tree java_catch = TREE_OPERAND (handler, 0);
      tree catch_type = TREE_TYPE (TREE_TYPE (BLOCK_EXPR_DECLS (java_catch)));
      tree expr = build (CATCH_EXPR, void_type_node,
			 prepare_eh_table_type (catch_type),
			 handler);
      if (catch)
	catch = build (COMPOUND_EXPR, void_type_node, catch, expr);
      else
	catch = expr;
      handler = TREE_CHAIN (handler);
    }
  return build (TRY_CATCH_EXPR, void_type_node, body, catch);
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
