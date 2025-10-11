/* Management of ranges in the Algol 68 front-end.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"
#include "tree-nested.h"

#include "a68.h"

/* Many Algol 68 constructions introduce a new range of definitions.  This is
   the case of clauses and of routine definitions.  The stack of ranges at any
   point in the program determines the "nest" of the constructions declared in
   the program.  This nest carries a record of all the declarations forming the
   environment in which that construct is to be interpreted.

   This file contains a manager of ranges of which we allocate one for each
   range inducing construct.  The top-level range corresponds to the primal
   environment.

   The ranges are used by the lowering code in order to create GCC tree BLOCK
   nodes, and also to keep track of the set of declarations and of statements
   being added by the current serial clause.  */

struct GTY (()) range
{
  /* Whether this range entry doesn't introduce a lexical frame.  Declarations
     and decl_exprs get added to the nearst enclosing range that is not
     frameless.  */
  bool frameless;

  /* A chain of _DECL nodes for all variables, constants, functions,
     and typedef types.  These are in the reverse of the order supplied.  */
  tree names;

  /* A statements list of DECL_EXPR nodes for all the declarations in the
     range.  These are prepended to the statements list when the range is
     closed.  */
  tree decl_exprs;

  /* The context of the range, either a function declaration or a translation
     unit.  */
  tree context;

  /* The range below this one.  */
  struct range *next;

  /* Statement list.  */
  tree stmt_list;

  /* List of blocks to which the block created for this range is the
     superblock.  */
  tree blocks;

  /* Mode associated with the range.  For serial clauses, this is the mode of
     the value yielded by the clause.  */
  MOID_T *mode;

  /* If not TREE_NULL, then the range corresponds to a function, which can be
     either nested or defined at top-level.  */
  tree fndecl;
  bool top_level_function;

  /* The following fields are used by ranges introduced by serial
     clauses.  */
  bool save_restore_stack;
  bool has_completers;
  tree clause_result_decl;
  tree clause_exit_label_decl;
  tree clause_stack_save_decl;
};

/* Global and current ranges.  */

static GTY (()) struct range *global_range;
static GTY (()) struct range *current_range;

/* Create a new range and push it in the list.  */

static struct range *
new_range (void)
{
  struct range *range = ggc_alloc<struct range> ();

  range->frameless = false;
  range->names = NULL;
  range->decl_exprs = alloc_stmt_list ();
  range->context = NULL;
  range->next = NULL;
  range->blocks = NULL_TREE;
  range->stmt_list = alloc_stmt_list ();
  range->fndecl = NULL_TREE;
  range->top_level_function = false;
  range->save_restore_stack = false;
  range->has_completers = false;
  range->clause_result_decl = NULL_TREE;
  range->clause_exit_label_decl = NULL_TREE;
  range->clause_stack_save_decl = NULL_TREE;
  range->mode = NO_MOID;
  return range;
}

/* Push a new frameless range.  */

void
a68_push_stmt_list (MOID_T *mode)
{
  a68_push_range (mode);
  current_range->frameless = true;
}

/* Pop a frameless range.  */

tree
a68_pop_stmt_list (void)
{
  /* This will result into a stmt list.  */
  tree res = a68_pop_range ();
  gcc_assert (TREE_CODE (res) == STATEMENT_LIST);
  return res;
}

/* Push a new range.  */

void
a68_push_range (MOID_T *mode)
{
  struct range *range = new_range ();
  if (current_range)
    range->context = current_range->context;
  range->next = current_range;
  range->mode = mode;
  current_range = range;
}

/* Pop a range, with a finalizer.

   Return a BIND_EXPR, a statement list or a TRY_FINALLY_EXPR.  */

tree
a68_pop_range_with_finalizer (tree finalizer)
{
  tree range = a68_pop_range ();
  return fold_build2 (TRY_FINALLY_EXPR, TREE_TYPE (range),
		      range, finalizer);
}

/* Pop a range.  Return either a BIND_EXPR or a statements list.  */

tree
a68_pop_range (void)
{
  struct range *range = current_range;
  current_range = range->next;
  tree type = (range->mode == NULL ? void_type_node : CTYPE (range->mode));

  /* If TYPE is a pointer type and the last expression in the statement list is
     a variable of the type pointed by TYPE then take its address.  */
  tree_stmt_iterator i = tsi_last (range->stmt_list);
  if (POINTER_TYPE_P (type) && TREE_TYPE (type) == TREE_TYPE (tsi_stmt (i)))
    {
      append_to_statement_list_force (a68_consolidate_ref (range->mode, tsi_stmt (i)),
				      &range->stmt_list);
      tsi_delink (&i);
    }

  tree clause = NULL_TREE;
  if (range->frameless)
    clause = range->stmt_list;
  else
    {
      /* Create a block and set its declarations and supercontext.  */
      tree block = make_node (BLOCK);
      BLOCK_VARS (block) = range->names;
      BLOCK_SUBBLOCKS (block) = range->blocks;

      /* In each subblock, record that this is its superior.  */
      for (tree t = range->blocks; t; t = BLOCK_CHAIN (t))
	BLOCK_SUPERCONTEXT (t) = block;

      if (range->fndecl)
	{
	  BLOCK_SUPERCONTEXT (block) = range->fndecl;
	  DECL_INITIAL (range->fndecl) = block;
	}
      else
	{
	  current_range->blocks
	    = block_chainon (current_range->blocks, block);
	}

      TREE_USED (block) = true;

      /* Create a BIND if the range contains declarations.  Otherwise just
	 use the statements list.  */
      clause = range->stmt_list;
      if (range->names != NULL_TREE)
	{
	  clause = build3 (BIND_EXPR,
			   type,
			   range->names,
			   range->stmt_list,
			   block);
	  TREE_SIDE_EFFECTS (clause) = 1;
	  BIND_EXPR_VARS (clause) = BLOCK_VARS (block);
	}

      /* Prepend the decl_exprs to the range's statements list.  */
      tree_stmt_iterator q = tsi_start (range->stmt_list);
      tsi_link_before (&q, range->decl_exprs, TSI_SAME_STMT);
    }

  /* Set the type of the stmt_list.  */
  TREE_TYPE (range->stmt_list) = type;
  TREE_SIDE_EFFECTS (range->stmt_list) = 1;

  return clause;
}

/* Add a new expression to the current range.  */

void
a68_add_stmt (tree exp)
{
  if (exp == void_node)
    /* This may result from a mode declaration.  */
    return;
  gcc_assert (current_range != NULL);
  append_to_statement_list_force (exp,
				  &current_range->stmt_list);
}

/* Add a new declaration to the current range.  */

void
a68_add_decl (tree decl)
{
  gcc_assert (current_range != NULL);
  struct range *range = current_range;

  /* Search for the right frame where to add the declaration.  */
  while (range->frameless)
    {
      gcc_assert (range->next != NULL);
      range = range->next;
    }

  tree n = range->names;
  while (n != decl && n != NULL)
    n = TREE_CHAIN (n);
  if (n != decl)
    {
      if (decl != current_function_decl)
	DECL_CONTEXT (decl) = range->context;
      /* Note this list needs to be in reverse order for compatibility with
	 GCC.  */
      TREE_CHAIN (decl) = range->names;
      range->names = decl;
    }
}

/* Add a new declaration expr to the current range.  */

void
a68_add_decl_expr (tree decl_expr)
{
  gcc_assert (current_range != NULL);
  struct range *range = current_range;

  /* Search for the right frame where to add the declaration expr.  */
  while (range->frameless)
    {
      gcc_assert (range->next != NULL);
      range = range->next;
    }

  append_to_statement_list_force (decl_expr, &range->decl_exprs);
}

/* Add a completer in the current range.  */

void
a68_add_completer (void)
{
  struct range *range = current_range;

  /* The last statement in the statements list is either a single unit or a
     labeled unit, i.e a COMPOUND_EXPR whose first expression is a label and
     second expression is the unit.  Consolidate the unit within the labeled
     unit to a ref.  */
  tree_stmt_iterator i = tsi_last (range->stmt_list);
  tree last_expr = tsi_stmt (i);

  if (TREE_CODE (last_expr) == COMPOUND_EXPR
      && TREE_CODE (TREE_OPERAND (last_expr, 0)) == LABEL_EXPR)
    {
      TREE_OPERAND (last_expr, 1) = a68_consolidate_ref (range->mode,
							 TREE_OPERAND (last_expr, 1));
      TREE_TYPE (last_expr) = TREE_TYPE (TREE_OPERAND (last_expr, 1));
    }
  else
    last_expr = a68_consolidate_ref (range->mode, last_expr);

  /* Now assign the labeled unit to the clause result decl then jump to the end
     of the serial clause.  */
  append_to_statement_list_force (fold_build2 (MODIFY_EXPR,
					       void_type_node,
					       range->clause_result_decl,
					       last_expr),
				  &range->stmt_list);
  tsi_delink (&i);
  append_to_statement_list_force (fold_build1 (GOTO_EXPR, void_type_node,
					       range->clause_exit_label_decl),
				  &range->stmt_list);
  range->has_completers = true;
}

/* Get the context of the current range.  */

tree
a68_range_context (void)
{
  gcc_assert (current_range != NULL);
  return current_range->context;
}

/* Get the list of declarations in the current range.  */

tree
a68_range_names (void)
{
  struct range *range = current_range;

  while (range->frameless && range->next != NULL)
    range = range->next;

  if (range != NULL)
    return range->names;
  else
    return NULL_TREE;
}

/* Get the statements list of the current range.  */

tree
a68_range_stmt_list (void)
{
  gcc_assert (current_range != NULL);
  return current_range->stmt_list;
}

/* Push a range for a function.  */

void
a68_push_function_range (tree fndecl, tree result_type,
			 bool top_level)
{
  a68_push_range (NULL /* VOID */);
  current_range->fndecl = fndecl;
  current_range->top_level_function = top_level;
  current_range->context = fndecl;

  /* Setup the result declaration.  */
  tree resdecl = build_decl (UNKNOWN_LOCATION,
			     RESULT_DECL,
			     get_identifier ("resdecl%"),
			     result_type);
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_IGNORED_P (resdecl) = 1;
  DECL_CONTEXT (resdecl) = fndecl;
  DECL_RESULT (fndecl) = resdecl;
  rest_of_decl_compilation (fndecl, 1, 0);
  make_decl_rtl (fndecl);
  allocate_struct_function (fndecl, false);

  /* Let GCC know the current scope is this function.  */
  current_function_decl = fndecl;
}

/* Pop a range for a function.  */

void
a68_pop_function_range (tree body)
{
  tree fndecl = current_range->fndecl;
  bool top_level = current_range->top_level_function;

  if (TREE_TYPE (DECL_RESULT (fndecl)) == void_type_node)
    {
      a68_add_stmt (body);
    }
  else
    {
      /* Append the return statement.
	 Note that this does the copy of the returned value.  */
      tree return_stmt = fold_build1 (RETURN_EXPR,
				      void_type_node,
				      fold_build2 (MODIFY_EXPR,
						   TREE_TYPE (DECL_RESULT (fndecl)),
						   DECL_RESULT (fndecl),
						   a68_low_dup (body, true /* use_heap */)));
      a68_add_stmt (return_stmt);
    }

  /* Set the body of the function.  */
  DECL_SAVED_TREE (fndecl) = a68_pop_range ();

  /* Output the GENERIC tree for the function..  */
  dump_function (TDI_original, fndecl);
  /* This compiles the function all the way to assembler language output.
     Nested functions are finalized when the containing top-level function is
     finalized.  */
  if (top_level || a68_in_global_range ())
    cgraph_node::finalize_function (fndecl, true);
  else
    /* Register this function with cgraph just far enough to get it added to
       our parent's nested function list.  */
    (void) cgraph_node::get_create (fndecl);

  /* Let GCC know the current scope has changed.  */
  current_function_decl = NULL_TREE;
  for (struct range *r = current_range; r; r = r->next)
    {
      if (r->fndecl != NULL_TREE)
	current_function_decl = r->fndecl;
    }
}

/* Push a range for a serial clause.

    label1:                 BIND_EXPR_BODY (STATEMENT_LIST (
     expr1;                  label1,
     expr2                   expr1,
   exit label2:              clause_result = expr2,
     expr3;                  goto exit_label,
     expr4                   label2,
   exit label3:              expr3,
     expr5                   clause_result = expr4,
                             goto exit_label,
                             label3,
                             clause_result = expr5,
                             exit_label,
			     clause_result)) */

void
a68_push_serial_clause_range (MOID_T *clause_mode,
			      bool save_restore_stack)
{
  /* Get the type of the enclosing clause.  */
  tree clause_type = CTYPE (clause_mode);

  /* If the serial clause has declarations that involve dynamic allocation, and
     the environ it establishes is local, then save the stack pointer.  */
  if (save_restore_stack)
    {
      a68_push_range (clause_mode);
      current_range->save_restore_stack = true;

      tree outer_clause_result_decl = build_decl (UNKNOWN_LOCATION,
						  VAR_DECL,
						  NULL, /* Set below.  */
						  clause_type);
      char *outer_clause_result_name = xasprintf ("outer_clause_result%d%%",
						  DECL_UID (outer_clause_result_decl));
      DECL_NAME (outer_clause_result_decl) = get_identifier (outer_clause_result_name);
      free (outer_clause_result_name);
      current_range->clause_result_decl = outer_clause_result_decl;
      a68_add_decl (outer_clause_result_decl);

      /* Variable used to save the stack pointer.  */
      tree stack_save_decl = build_decl (UNKNOWN_LOCATION,
					 VAR_DECL,
					 get_identifier ("stack_save%"),
					 build_pointer_type (char_type_node));
      current_range->clause_stack_save_decl = stack_save_decl;
      a68_add_decl (stack_save_decl);
      a68_add_stmt (fold_build1 (DECL_EXPR,
				 TREE_TYPE (stack_save_decl),
				 stack_save_decl));

      /* Save stack pointer.  */
      tree call = builtin_decl_implicit (BUILT_IN_STACK_SAVE);
      call = build_call_expr_loc (UNKNOWN_LOCATION, call, 0);
      a68_add_stmt (fold_build2 (MODIFY_EXPR, void_type_node,
				 stack_save_decl, call));
    }

  /* Push a new range.  */
  a68_push_range (clause_mode);
  current_range->save_restore_stack = save_restore_stack;

  /* Create a decl for clause_result with the right type and add it to the
     block's declaration list.  */
  tree clause_result_decl = build_decl (UNKNOWN_LOCATION,
					VAR_DECL,
					NULL, /* Set below.  */
					clause_type);
  char *clause_result_name = xasprintf ("clause_result%d%%", DECL_UID (clause_result_decl));
  DECL_NAME (clause_result_decl) = get_identifier (clause_result_name);
  //  free (clause_result_name);
  DECL_INITIAL (clause_result_decl) = a68_get_skip_tree (clause_mode);
  DECL_CONTEXT (clause_result_decl) = current_range->context;
  current_range->clause_result_decl = clause_result_decl;

  /* Create a decl for the clause's exit label.  */
  tree clause_exit_label_decl = build_decl (UNKNOWN_LOCATION,
					    LABEL_DECL,
					    NULL, /* Set below.  */
					    void_type_node);
  char *exit_label_name = xasprintf ("clause_exit_label%d%%", DECL_UID (clause_exit_label_decl));
  DECL_NAME (clause_exit_label_decl) = get_identifier (exit_label_name);
  free (exit_label_name);
  DECL_CONTEXT (clause_exit_label_decl) = current_range->context;
  current_range->clause_exit_label_decl = clause_exit_label_decl;
}

/* Pop a range for a serial clause and return the resulting bind
   expression.  */

tree
a68_pop_serial_clause_range (void)
{
  struct range *range = current_range;
  MOID_T *clause_mode = range->mode;
  tree clause_type = CTYPE (clause_mode);

  /* The last expression in the statements list is either a single unit or a
     labeled unit.  Consolidate it to a ref if required by the mode of the
     serial clause.  */
  {
    tree_stmt_iterator si = tsi_last (range->stmt_list);
    tree last_expr = tsi_stmt (si);
    if (TREE_CODE (last_expr) == COMPOUND_EXPR
	&& TREE_CODE (TREE_OPERAND (last_expr, 0)) == LABEL_EXPR)
      {
	TREE_OPERAND (last_expr, 1) = a68_consolidate_ref (range->mode,
							   TREE_OPERAND (last_expr, 1));
	TREE_TYPE (last_expr) = TREE_TYPE (TREE_OPERAND (last_expr, 1));
      }
    else
      last_expr = a68_consolidate_ref (range->mode, last_expr);
    a68_add_stmt (last_expr);
    tsi_delink (&si);
  }

  /* If the serial clause has completers, we have to make use of the
     clause_result% and clause_exit_label% mechanism to assure the statements
     list has a single exit at the end.  */
  if (range->has_completers)
    {
      /* First prepend EXPR_DECL expressions for clause_result% and
	 clause_exit_label% */
      {
	tree_stmt_iterator si = tsi_start (range->stmt_list);
	tsi_link_before (&si,
			 fold_build1 (DECL_EXPR,
				      TREE_TYPE (range->clause_result_decl),
				      range->clause_result_decl),
			 TSI_CONTINUE_LINKING);
	tsi_link_before (&si,
			 fold_build1 (DECL_EXPR,
				      TREE_TYPE (range->clause_exit_label_decl),
				      range->clause_exit_label_decl),
			 TSI_CONTINUE_LINKING);
      }

      /* Then turn the last expression in stmt_list to an assignment to
	 clause_result_decl%, but don't bother if it has been voided.  */
      if (clause_type != a68_void_type)
	{
	  tree_stmt_iterator si = tsi_last (range->stmt_list);
	  tree last_expr = tsi_stmt (si);

	  a68_add_stmt (build2 (MODIFY_EXPR,
				clause_type,
				range->clause_result_decl,
				last_expr));
	  tsi_delink (&si);
	}

      a68_add_decl (range->clause_result_decl);
      a68_add_decl (range->clause_exit_label_decl);

      /* Finally append the exit label and last expression with
	 result_decl.  */
      a68_add_stmt (build1 (LABEL_EXPR, void_type_node, range->clause_exit_label_decl));
      a68_add_stmt (build1 (NON_LVALUE_EXPR, clause_type, range->clause_result_decl));
    }

  /* Check that the type of the last statement in the statements list is the
     same than the type corresponding to the clause mode.  */
  {
    tree_stmt_iterator si = tsi_last (range->stmt_list);
    if (TREE_TYPE (tsi_stmt (si)) != clause_type
	/* But NIL can appear in a context expecting VOID with no widening.  */
	&& !(clause_type == a68_void_type
	     && POINTER_TYPE_P (TREE_TYPE (tsi_stmt (si)))
	     && TREE_CODE (tsi_stmt (si)) == INTEGER_CST
	     && tree_to_shwi (tsi_stmt (si)) == 0)
	/* And any row type is valid when M_ROWS is expected.  */
	&& !(A68_ROWS_TYPE_P (clause_type)
	     && A68_ROWS_TYPE_P (TREE_TYPE (tsi_stmt (si))))
	/* Do not rely on comparing pointer types, as the equality fails in
	   that case.  We need a better way of comparing types, either using
	   TYPE_CANONICAL or caching.  */
	&& !(POINTER_TYPE_P (TREE_TYPE (tsi_stmt (si))) && POINTER_TYPE_P (clause_type)))
      {
	printf ("last statement:\n");
	debug_tree (tsi_stmt (si));
	printf ("expected type:\n");
	debug_tree (clause_type);
	gcc_unreachable ();
      }
  }

  /* If the serial clause has declarations that involve dynamic allocation, and
     the environ it establishes is local, then restore the stack pointer.  */
  if (range->save_restore_stack)
    {
      /* Turn last expression of inner clause into a modify statement.  This
	 may involve a copy.  This can be omitted if the serial clause yields
	 void.  */
      if (clause_type != a68_void_type)
	{
	  tree_stmt_iterator si = tsi_last (range->stmt_list);
	  tree last_expr = tsi_stmt (si);

	  a68_add_stmt (build2 (MODIFY_EXPR,
				clause_type,
				range->next->clause_result_decl,
				a68_low_dup (last_expr)));
	  tsi_delink (&si);
	}

      /* Finish inner clause, restoring stack pointer on finalizing.  */
      tree restore_sp = builtin_decl_implicit (BUILT_IN_STACK_RESTORE);
      restore_sp = build_call_expr_loc (UNKNOWN_LOCATION, restore_sp, 1,
					current_range->next->clause_stack_save_decl);
      a68_add_stmt (a68_pop_range_with_finalizer (restore_sp));
      /* The result value is now in clause_result_decl.  */
      a68_add_stmt (build1 (NON_LVALUE_EXPR, clause_type,
			    current_range->clause_result_decl));
    }

  return a68_pop_range ();
}

/* Whether the current range is the global range.  */

bool
a68_in_global_range (void)
{
  return current_range == global_range;
}

/* Initialize ranges.  */

void
a68_init_ranges (void)
{
  global_range = new_range ();
  global_range->context = build_translation_unit_decl (NULL);
  current_range = global_range;
}

#include "gt-algol68-a68-low-ranges.h"
