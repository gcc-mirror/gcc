/* This file is part of the Intel(R) Cilk(TM) Plus support
   This file contains routines to handle Cilk Plus specific
   routines for the C++ Compiler.
   Copyright (C) 2013-2017 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "tree-iterator.h"
#include "cilk.h"
#include "c-family/c-common.h"

/* Return TRUE if T is a FUNCTION_DECL for a type-conversion operator.  */

static bool
is_conversion_operator_function_decl_p (tree t)
{
  if (TREE_CODE (t) != FUNCTION_DECL)
    return false;

  return DECL_NAME (t) && IDENTIFIER_TYPENAME_P (DECL_NAME (t));
}

/* Recursively traverse EXP to search for a CILK_SPAWN_STMT subtree.
   Return the CILK_SPAWN_STMT subtree if found; otherwise, the last subtree
   searched.  */

static tree
find_spawn (tree exp)
{
  /* Happens with C++ TARGET_EXPR.  */
  if (exp == NULL_TREE)
    return exp;

  if (cilk_ignorable_spawn_rhs_op (exp))
    return find_spawn (TREE_OPERAND (exp, 0));

  switch (TREE_CODE (exp))
    {
    case AGGR_INIT_EXPR:
      {
	/* Check for initialization via a constructor call that represents
	   implicit conversion.  */
	if (AGGR_INIT_VIA_CTOR_P (exp) && aggr_init_expr_nargs (exp) == 2)
	  return find_spawn (AGGR_INIT_EXPR_ARG (exp, 1));

	/* Check for initialization via a call to a type-conversion
	   operator.  */
	tree fn = AGGR_INIT_EXPR_FN (exp);
	if (TREE_CODE (fn) == ADDR_EXPR
	    && is_conversion_operator_function_decl_p (TREE_OPERAND (fn, 0))
	    && aggr_init_expr_nargs (exp) == 1)
	  return find_spawn (AGGR_INIT_EXPR_ARG (exp, 0));
      }
      break;

    case CALL_EXPR:
      {
	/* Check for a call to a type-conversion operator.  */
	tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
	if (is_conversion_operator_function_decl_p (fndecl)
	    && call_expr_nargs (exp) == 1)
	  return find_spawn (CALL_EXPR_ARG (exp, 0));
      }
      break;

    case TARGET_EXPR:
      return find_spawn (TARGET_EXPR_INITIAL (exp));

    case CLEANUP_POINT_EXPR:
    case COMPOUND_EXPR:
    case EXPR_STMT:
      return find_spawn (TREE_OPERAND (exp, 0));

    default:
      break;
    }

    return exp;
}

/* Return true if *EXP0 is a recognized form of spawn.  Recognized forms
   are, after conversion to void, a call expression at outer level or an
   assignment at outer level with the right hand side being a spawned call.
   In addition to this, it also unwraps the CILK_SPAWN_STMT cover from the
   CALL_EXPR that is being spawned.

   Note that `=' in C++ may turn into a CALL_EXPR rather than a
   MODIFY_EXPR.  */

bool
cilk_cp_detect_spawn_and_unwrap (tree *exp0)
{
  tree exp = *exp0;

  if (!TREE_SIDE_EFFECTS (exp))
    return false;

  /* Strip off any conversion to void.  It does not affect whether spawn
     is supported here.  */
  if (TREE_CODE (exp) == CONVERT_EXPR && VOID_TYPE_P (TREE_TYPE (exp)))
    exp = TREE_OPERAND (exp, 0);

  if (TREE_CODE (exp) == MODIFY_EXPR || TREE_CODE (exp) == INIT_EXPR)
    exp = TREE_OPERAND (exp, 1);

  exp = find_spawn (exp);
  if (exp == NULL_TREE)
    return false;

  /* Now we should have a CALL_EXPR with a CILK_SPAWN_STMT wrapper around
     it, or return false.  */
  return cilk_recognize_spawn (exp, exp0);
}

/* Callback for cp_walk_tree to validate the body of a pragma simd loop
   or _cilk_for loop.

   This function is passed in as a function pointer to walk_tree.  *TP is
   the current tree pointer, *WALK_SUBTREES is set to 0 by this function if
   recursing into TP's subtrees is unnecessary. *DATA is a bool variable that
   is set to false if an error has occurred.  */

static tree
cpp_validate_cilk_plus_loop_aux (tree *tp, int *walk_subtrees, void *data)
{
  bool *valid = (bool *) data;

  if (!tp || !*tp)
    return NULL_TREE;

  location_t loc = EXPR_LOCATION (*tp);
  if (TREE_CODE (*tp) == THROW_EXPR)
    {
      error_at (loc, "throw expressions are not allowed inside loops "
		"marked with pragma simd");
      *walk_subtrees = 0;
      *valid = false;
    }
  else if (TREE_CODE (*tp) == TRY_BLOCK)
    {
      error_at (loc, "try statements are not allowed inside loops marked "
		"with #pragma simd");
      *valid = false;
      *walk_subtrees = 0;
    }
  return NULL_TREE;
}  


/* Walks through all the subtrees of BODY using walk_tree to make sure
   invalid statements/expressions are not found inside BODY.  Returns
   false if any invalid statements are found.  */

bool
cpp_validate_cilk_plus_loop (tree body)
{
  bool valid = true;
  cp_walk_tree (&body, cpp_validate_cilk_plus_loop_aux,
		(void *) &valid, NULL);
  return valid;
}

/* Sets the EXCEPTION bit (0x10) in the FRAME.flags field.  */

static tree
set_cilk_except_flag (tree frame)
{
  tree flags = cilk_dot (frame, CILK_TI_FRAME_FLAGS, 0);

  flags = build2 (MODIFY_EXPR, void_type_node, flags,
		  build2 (BIT_IOR_EXPR, TREE_TYPE (flags), flags,
			  build_int_cst (TREE_TYPE (flags),
					 CILK_FRAME_EXCEPTING)));
  return flags;
}

/* Sets the frame.EXCEPT_DATA field to the head of the exception pointer.  */

static tree
set_cilk_except_data (tree frame)
{
  tree except_data = cilk_dot (frame, CILK_TI_FRAME_EXCEPTION, 0);
  tree uresume_fn = builtin_decl_implicit (BUILT_IN_EH_POINTER);
  tree ret_expr;
  uresume_fn  = build_call_expr (uresume_fn, 1,
				 build_int_cst (integer_type_node, 0));
  ret_expr = build2 (MODIFY_EXPR, void_type_node, except_data, uresume_fn);
  return ret_expr;
}

/* Installs BODY into function FNDECL with appropriate exception handling
   code.  WD holds information of wrapper function used to pass into the
   outlining function, cilk_outline.  */

void
cilk_install_body_with_frame_cleanup (tree fndecl, tree orig_body, void *wd)
{
  tree frame = make_cilk_frame (fndecl);
  tree dtor = create_cilk_function_exit (frame, false, false);
  add_local_decl (cfun, frame);

  cfun->language = ggc_cleared_alloc<language_function> ();
  
  location_t loc = EXPR_LOCATION (orig_body);
  tree list = alloc_stmt_list ();
  DECL_SAVED_TREE (fndecl) = list;
  tree fptr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (frame)), frame);
  tree body = cilk_install_body_pedigree_operations (fptr);
  gcc_assert (TREE_CODE (body) == STATEMENT_LIST);
  tree detach_expr = build_call_expr (cilk_detach_fndecl, 1, fptr);
  append_to_statement_list (detach_expr, &body);
  cilk_outline (fndecl, &orig_body, (struct wrapper_data *) wd);
  append_to_statement_list (orig_body, &body);
  if (flag_exceptions)
    {
      tree except_flag = set_cilk_except_flag (frame);
      tree except_data = set_cilk_except_data (frame);
      tree catch_list = alloc_stmt_list ();
      append_to_statement_list (except_flag, &catch_list);
      append_to_statement_list (except_data, &catch_list);
      body = create_try_catch_expr (body, catch_list);
    }
  append_to_statement_list (build_stmt (loc, TRY_FINALLY_EXPR, body, dtor),
			    &list);
}
