/* This file is part of the Intel(R) Cilk(TM) Plus support
   This file contains routines to handle Cilk Plus specific
   routines for the C++ Compiler.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.
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
#include "diagnostic-core.h"
#include "tree-iterator.h"
#include "tree-inline.h"  /* for copy_tree_body_r.  */
#include "ggc.h"
#include "cilk.h"

/* Callback for cp_walk_tree to validate the body of a pragma simd loop
   or _cilk_for loop.

   This function is passed in as a function pointer to walk_tree.  *TP is
   the current tree pointer, *WALK_SUBTREES is set to 0 by this function if
   recursing into TP's subtrees is unnecessary. *DATA is a bool variable that
   is set to false if an error has occured.  */

static tree
cpp_validate_cilk_plus_loop_aux (tree *tp, int *walk_subtrees, void *data)
{
  bool *valid = (bool *) data;
  location_t loc = EXPR_HAS_LOCATION (*tp) ? EXPR_LOCATION (*tp) :
    UNKNOWN_LOCATION;

  if (!tp || !*tp)
    return NULL_TREE;

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

  cfun->language = ggc_alloc_cleared_language_function ();
  
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

