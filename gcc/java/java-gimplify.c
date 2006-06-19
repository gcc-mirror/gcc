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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. 

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
#include "tree-gimple.h"
#include "toplev.h"

static tree java_gimplify_labeled_block_expr (tree);
static tree java_gimplify_exit_block_expr (tree);
static tree java_gimplify_case_expr (tree);
static tree java_gimplify_default_expr (tree);
static tree java_gimplify_block (tree);
static tree java_gimplify_new_array_init (tree);
static tree java_gimplify_try_expr (tree);
static enum gimplify_status java_gimplify_modify_expr (tree*, tree*, tree *);
static enum gimplify_status java_gimplify_component_ref (tree*, tree*, tree *);
static enum gimplify_status java_gimplify_self_mod_expr (tree*, tree*, tree *);

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
  enum tree_code code = TREE_CODE (*expr_p);

  switch (code)
    {
    case BLOCK:
      *expr_p = java_gimplify_block (*expr_p);
      break;

    case EXPR_WITH_FILE_LOCATION:
#ifdef USE_MAPPED_LOCATION
      input_location = EXPR_LOCATION (*expr_p);
#else
      input_location.file = EXPR_WFL_FILENAME (*expr_p);
      input_location.line = EXPR_WFL_LINENO (*expr_p);
#endif
      *expr_p = EXPR_WFL_NODE (*expr_p);
      if (EXPR_P (*expr_p))
	SET_EXPR_LOCATION (*expr_p, input_location);
      break;

    case LABELED_BLOCK_EXPR:
      *expr_p = java_gimplify_labeled_block_expr (*expr_p);
      break;

    case EXIT_BLOCK_EXPR:
      *expr_p = java_gimplify_exit_block_expr (*expr_p);
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

    case VAR_DECL:
      *expr_p = java_replace_reference (*expr_p, /* want_lvalue */ false);
      return GS_UNHANDLED;

    case MODIFY_EXPR:
      return java_gimplify_modify_expr (expr_p, pre_p, post_p);

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
    case UNARY_PLUS_EXPR:
    case NEW_ARRAY_EXPR:
    case NEW_ANONYMOUS_ARRAY_EXPR:
    case NEW_CLASS_EXPR:
    case THIS_EXPR:
    case SYNCHRONIZED_EXPR:
    case CONDITIONAL_EXPR:
    case INSTANCEOF_EXPR:
    case CLASS_LITERAL:
      gcc_unreachable ();

    case COMPONENT_REF:
      return java_gimplify_component_ref (expr_p, pre_p, post_p);

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

/* Gimplify a LABELED_BLOCK_EXPR into a LABEL_EXPR following
   a (possibly empty) body.  */

static tree
java_gimplify_labeled_block_expr (tree expr)
{
  tree body = LABELED_BLOCK_BODY (expr);
  tree label = LABELED_BLOCK_LABEL (expr);
  tree t;

  DECL_CONTEXT (label) = current_function_decl;
  t = build1 (LABEL_EXPR, void_type_node, label);
  if (body != NULL_TREE)
    t = build2 (COMPOUND_EXPR, void_type_node, body, t);
  return t;
}

/* Gimplify a EXIT_BLOCK_EXPR into a GOTO_EXPR.  */

static tree
java_gimplify_exit_block_expr (tree expr)
{
  tree labeled_block = EXIT_BLOCK_LABELED_BLOCK (expr);
  tree label;

  /* First operand must be a LABELED_BLOCK_EXPR, which should
     already be lowered (or partially lowered) when we get here.  */
  gcc_assert (TREE_CODE (labeled_block) == LABELED_BLOCK_EXPR);

  label = LABELED_BLOCK_LABEL (labeled_block);
  return build1 (GOTO_EXPR, void_type_node, label);
}



static enum gimplify_status
java_gimplify_component_ref (tree *expr_p, tree *pre_p, tree *post_p)
{
  if (CLASS_FROM_SOURCE_P (output_class)
      && TREE_THIS_VOLATILE (TREE_OPERAND (*expr_p, 1))
      && ! TREE_THIS_VOLATILE (*expr_p))
  {
    enum gimplify_status stat;
    tree sync_expr;

    /* Special handling for volatile fields.  

    A load has "acquire" semantics, implying that you can't move up
    later operations.  A store has "release" semantics meaning that
    earlier operations cannot be delayed past it.  

    This logic only handles loads: stores are handled in
    java_gimplify_modify_expr().

    We gimplify this COMPONENT_REF, put the result in a tmp_var, and then
    return a COMPOUND_EXPR of the form {__sync_synchronize(); tmp_var}.  
    This forces __sync_synchronize() to be placed immediately after
    loading from the volatile field.

    */
  
    TREE_THIS_VOLATILE (*expr_p) = 1;
    *expr_p = java_modify_addr_for_volatile (*expr_p);
    stat = gimplify_expr (expr_p, pre_p, post_p,
			  is_gimple_formal_tmp_var, fb_rvalue);
    if (stat == GS_ERROR)
      return stat;

    sync_expr 
      = build3 (CALL_EXPR, void_type_node,
		build_address_of (built_in_decls[BUILT_IN_SYNCHRONIZE]),
		NULL_TREE, NULL_TREE);
    TREE_SIDE_EFFECTS (sync_expr) = 1;
    *expr_p = build2 (COMPOUND_EXPR, TREE_TYPE (*expr_p),
		      sync_expr, *expr_p);
    TREE_SIDE_EFFECTS (*expr_p) = 1;
  }

  return GS_UNHANDLED;
}
  

static enum gimplify_status
java_gimplify_modify_expr (tree *modify_expr_p, tree *pre_p, tree *post_p)
{
  tree modify_expr = *modify_expr_p;
  tree lhs = TREE_OPERAND (modify_expr, 0);
  tree rhs = TREE_OPERAND (modify_expr, 1);
  tree lhs_type = TREE_TYPE (lhs);

  if (CLASS_FROM_SOURCE_P (output_class)
      && TREE_CODE (lhs) == COMPONENT_REF
      && TREE_THIS_VOLATILE (TREE_OPERAND (lhs, 1)))
    {
      /* Special handling for volatile fields.  

      A load has "acquire" semantics, implying that you can't move up
      later operations.  A store has "release" semantics meaning that
      earlier operations cannot be delayed past it.  

      This logic only handles stores; loads are handled in
      java_gimplify_component_ref().

      We gimplify the rhs, put the result in a tmp_var, and then return
      a MODIFY_EXPR with an rhs of the form {__sync_synchronize(); tmp_var}.
      This forces __sync_synchronize() to be placed after evaluating
      the rhs and immediately before storing to the volatile field.

      */
  
      enum gimplify_status stat;
      tree sync_expr 
	= build3 (CALL_EXPR, void_type_node,
		  build_address_of (built_in_decls[BUILT_IN_SYNCHRONIZE]),
		  NULL_TREE, NULL_TREE);
      TREE_SIDE_EFFECTS (sync_expr) = 1;

      stat = gimplify_expr (&rhs, pre_p, post_p,
			    is_gimple_formal_tmp_var, fb_rvalue);
      if (stat == GS_ERROR)
	return stat;

      rhs = build2 (COMPOUND_EXPR, TREE_TYPE (rhs),
		    sync_expr, rhs);
      TREE_SIDE_EFFECTS (rhs) = 1;
      TREE_THIS_VOLATILE (lhs) = 1;
      lhs = java_modify_addr_for_volatile (lhs);
      TREE_OPERAND (modify_expr, 0) = lhs;
      TREE_OPERAND (modify_expr, 1) = rhs;
    }

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
java_gimplify_self_mod_expr (tree *expr_p, tree *pre_p ATTRIBUTE_UNUSED, 
			     tree *post_p ATTRIBUTE_UNUSED)
{
  tree lhs = TREE_OPERAND (*expr_p, 0);

  if (TREE_CODE (lhs) == COMPONENT_REF
      && TREE_THIS_VOLATILE (TREE_OPERAND (lhs, 1)))
    TREE_THIS_VOLATILE (lhs) = 1;

  return GS_UNHANDLED;
}

    
static tree
java_gimplify_case_expr (tree expr)
{
  tree label = create_artificial_label ();
  return build3 (CASE_LABEL_EXPR, void_type_node,
		 TREE_OPERAND (expr, 0), NULL_TREE, label);
}

static tree
java_gimplify_default_expr (tree expr ATTRIBUTE_UNUSED)
{
  tree label = create_artificial_label ();
  return build3 (CASE_LABEL_EXPR, void_type_node, NULL_TREE, NULL_TREE, label);
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

  /* The TREE_USED flag on a block determines whether the debug output
     routines generate info for the variables in that block.  */
  TREE_USED (block) = 1;

  if (outer != NULL_TREE)
    {
      outer = BIND_EXPR_BLOCK (outer);
      BLOCK_SUBBLOCKS (outer) = chainon (BLOCK_SUBBLOCKS (outer), block);
    }
  BLOCK_EXPR_BODY (java_block) = NULL_TREE;

  return build3 (BIND_EXPR, TREE_TYPE (java_block), decls, body, block);
}

/* Gimplify a NEW_ARRAY_INIT node into array/element assignments.  */

static tree
java_gimplify_new_array_init (tree exp)
{
  tree array_type = TREE_TYPE (TREE_TYPE (exp));
  tree data_field = lookup_field (&array_type, get_identifier ("data"));
  tree element_type = TYPE_ARRAY_ELEMENT (array_type);
  HOST_WIDE_INT ilength = java_array_type_length (array_type);
  tree length = build_int_cst (NULL_TREE, ilength);
  tree init = TREE_OPERAND (exp, 0);
  tree value;
  unsigned HOST_WIDE_INT cnt;

  tree array_ptr_type = build_pointer_type (array_type);
  tree tmp = create_tmp_var (array_ptr_type, "array");
  tree body = build2 (MODIFY_EXPR, array_ptr_type, tmp,
		      build_new_array (element_type, length));

  int index = 0;

  /* FIXME: try to allocate array statically?  */
  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (init), cnt, value)
    {
      /* FIXME: Should use build_java_arrayaccess here, but avoid
	 bounds checking.  */
      tree lhs = build3 (COMPONENT_REF, TREE_TYPE (data_field),    
			 build_java_indirect_ref (array_type, tmp, 0),
			 data_field, NULL_TREE);
      tree assignment = build2 (MODIFY_EXPR, element_type,
				build4 (ARRAY_REF, element_type, lhs,
					build_int_cst (NULL_TREE, index++),
					NULL_TREE, NULL_TREE),
				value);
      body = build2 (COMPOUND_EXPR, element_type, body, assignment);
    }

  return build2 (COMPOUND_EXPR, array_ptr_type, body, tmp);
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
      tree expr = build2 (CATCH_EXPR, void_type_node,
			  prepare_eh_table_type (catch_type),
			  handler);
      if (catch)
	catch = build2 (COMPOUND_EXPR, void_type_node, catch, expr);
      else
	catch = expr;
      handler = TREE_CHAIN (handler);
    }
  return build2 (TRY_CATCH_EXPR, void_type_node, body, catch);
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
