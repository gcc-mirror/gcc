/* Code translation -- generate GCC trees from gfc_code.
   Copyright (C) 2002, 2003, 2004, 2005, 2006 Free Software Foundation,
   Inc.
   Contributed by Paul Brook

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tree-gimple.h"
#include "ggc.h"
#include "toplev.h"
#include "defaults.h"
#include "real.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-array.h"
#include "trans-types.h"
#include "trans-const.h"

/* Naming convention for backend interface code:

   gfc_trans_*	translate gfc_code into STMT trees.

   gfc_conv_*	expression conversion

   gfc_get_*	get a backend tree representation of a decl or type  */

static gfc_file *gfc_current_backend_file;


/* Advance along TREE_CHAIN n times.  */

tree
gfc_advance_chain (tree t, int n)
{
  for (; n > 0; n--)
    {
      gcc_assert (t != NULL_TREE);
      t = TREE_CHAIN (t);
    }
  return t;
}


/* Wrap a node in a TREE_LIST node and add it to the end of a list.  */

tree
gfc_chainon_list (tree list, tree add)
{
  tree l;

  l = tree_cons (NULL_TREE, add, NULL_TREE);

  return chainon (list, l);
}


/* Strip off a legitimate source ending from the input
   string NAME of length LEN.  */

static inline void
remove_suffix (char *name, int len)
{
  int i;

  for (i = 2; i < 8 && len > i; i++)
    {
      if (name[len - i] == '.')
	{
	  name[len - i] = '\0';
	  break;
	}
    }
}


/* Creates a variable declaration with a given TYPE.  */

tree
gfc_create_var_np (tree type, const char *prefix)
{
  return create_tmp_var_raw (type, prefix);
}


/* Like above, but also adds it to the current scope.  */

tree
gfc_create_var (tree type, const char *prefix)
{
  tree tmp;

  tmp = gfc_create_var_np (type, prefix);

  pushdecl (tmp);

  return tmp;
}


/* If the an expression is not constant, evaluate it now.  We assign the
   result of the expression to an artificially created variable VAR, and
   return a pointer to the VAR_DECL node for this variable.  */

tree
gfc_evaluate_now (tree expr, stmtblock_t * pblock)
{
  tree var;

  if (CONSTANT_CLASS_P (expr))
    return expr;

  var = gfc_create_var (TREE_TYPE (expr), NULL);
  gfc_add_modify_expr (pblock, var, expr);

  return var;
}


/* Build a MODIFY_EXPR node and add it to a given statement block PBLOCK.
   A MODIFY_EXPR is an assignment: LHS <- RHS.  */

void
gfc_add_modify_expr (stmtblock_t * pblock, tree lhs, tree rhs)
{
  tree tmp;

#ifdef ENABLE_CHECKING
  /* Make sure that the types of the rhs and the lhs are the same
     for scalar assignments.  We should probably have something
     similar for aggregates, but right now removing that check just
     breaks everything.  */
  gcc_assert (TREE_TYPE (rhs) == TREE_TYPE (lhs)
	      || AGGREGATE_TYPE_P (TREE_TYPE (lhs)));
#endif

  tmp = fold_build2 (MODIFY_EXPR, void_type_node, lhs, rhs);
  gfc_add_expr_to_block (pblock, tmp);
}


/* Create a new scope/binding level and initialize a block.  Care must be
   taken when translating expressions as any temporaries will be placed in
   the innermost scope.  */

void
gfc_start_block (stmtblock_t * block)
{
  /* Start a new binding level.  */
  pushlevel (0);
  block->has_scope = 1;

  /* The block is empty.  */
  block->head = NULL_TREE;
}


/* Initialize a block without creating a new scope.  */

void
gfc_init_block (stmtblock_t * block)
{
  block->head = NULL_TREE;
  block->has_scope = 0;
}


/* Sometimes we create a scope but it turns out that we don't actually
   need it.  This function merges the scope of BLOCK with its parent.
   Only variable decls will be merged, you still need to add the code.  */

void
gfc_merge_block_scope (stmtblock_t * block)
{
  tree decl;
  tree next;

  gcc_assert (block->has_scope);
  block->has_scope = 0;

  /* Remember the decls in this scope.  */
  decl = getdecls ();
  poplevel (0, 0, 0);

  /* Add them to the parent scope.  */
  while (decl != NULL_TREE)
    {
      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = NULL_TREE;

      pushdecl (decl);
      decl = next;
    }
}


/* Finish a scope containing a block of statements.  */

tree
gfc_finish_block (stmtblock_t * stmtblock)
{
  tree decl;
  tree expr;
  tree block;

  expr = stmtblock->head;
  if (!expr)
    expr = build_empty_stmt ();

  stmtblock->head = NULL_TREE;

  if (stmtblock->has_scope)
    {
      decl = getdecls ();

      if (decl)
	{
	  block = poplevel (1, 0, 0);
	  expr = build3_v (BIND_EXPR, decl, expr, block);
	}
      else
	poplevel (0, 0, 0);
    }

  return expr;
}


/* Build an ADDR_EXPR and cast the result to TYPE.  If TYPE is NULL, the
   natural type is used.  */

tree
gfc_build_addr_expr (tree type, tree t)
{
  tree base_type = TREE_TYPE (t);
  tree natural_type;

  if (type && POINTER_TYPE_P (type)
      && TREE_CODE (base_type) == ARRAY_TYPE
      && TYPE_MAIN_VARIANT (TREE_TYPE (type))
	 == TYPE_MAIN_VARIANT (TREE_TYPE (base_type)))
    natural_type = type;
  else
    natural_type = build_pointer_type (base_type);

  if (TREE_CODE (t) == INDIRECT_REF)
    {
      if (!type)
	type = natural_type;
      t = TREE_OPERAND (t, 0);
      natural_type = TREE_TYPE (t);
    }
  else
    {
      if (DECL_P (t))
        TREE_ADDRESSABLE (t) = 1;
      t = build1 (ADDR_EXPR, natural_type, t);
    }

  if (type && natural_type != type)
    t = convert (type, t);

  return t;
}


/* Build an ARRAY_REF with its natural type.  */

tree
gfc_build_array_ref (tree base, tree offset)
{
  tree type = TREE_TYPE (base);
  gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
  type = TREE_TYPE (type);

  if (DECL_P (base))
    TREE_ADDRESSABLE (base) = 1;

  return build4 (ARRAY_REF, type, base, offset, NULL_TREE, NULL_TREE);
}


/* Generate a runtime error if COND is true.  */

void
gfc_trans_runtime_check (tree cond, tree msg, stmtblock_t * pblock)
{
  stmtblock_t block;
  tree body;
  tree tmp;
  tree args;

  if (integer_zerop (cond))
    return;

  /* The code to generate the error.  */
  gfc_start_block (&block);

  gcc_assert (TREE_CODE (msg) == STRING_CST);

  TREE_USED (msg) = 1;

  tmp = gfc_build_addr_expr (pchar_type_node, msg);
  args = gfc_chainon_list (NULL_TREE, tmp);

  tmp = gfc_build_addr_expr (pchar_type_node, gfc_strconst_current_filename);
  args = gfc_chainon_list (args, tmp);

  tmp = build_int_cst (NULL_TREE, input_line);
  args = gfc_chainon_list (args, tmp);

  tmp = build_function_call_expr (gfor_fndecl_runtime_error, args);
  gfc_add_expr_to_block (&block, tmp);

  body = gfc_finish_block (&block);

  if (integer_onep (cond))
    {
      gfc_add_expr_to_block (pblock, body);
    }
  else
    {
      /* Tell the compiler that this isn't likely.  */
      tmp = gfc_chainon_list (NULL_TREE, cond);
      tmp = gfc_chainon_list (tmp, integer_zero_node);
      cond = build_function_call_expr (built_in_decls[BUILT_IN_EXPECT], tmp);

      tmp = build3_v (COND_EXPR, cond, body, build_empty_stmt ());
      gfc_add_expr_to_block (pblock, tmp);
    }
}


/* Add a statement to a block.  */

void
gfc_add_expr_to_block (stmtblock_t * block, tree expr)
{
  gcc_assert (block);

  if (expr == NULL_TREE || IS_EMPTY_STMT (expr))
    return;

  if (block->head)
    {
      if (TREE_CODE (block->head) != STATEMENT_LIST)
	{
	  tree tmp;

	  tmp = block->head;
	  block->head = NULL_TREE;
	  append_to_statement_list (tmp, &block->head);
	}
      append_to_statement_list (expr, &block->head);
    }
  else
    /* Don't bother creating a list if we only have a single statement.  */
    block->head = expr;
}


/* Add a block the end of a block.  */

void
gfc_add_block_to_block (stmtblock_t * block, stmtblock_t * append)
{
  gcc_assert (append);
  gcc_assert (!append->has_scope);

  gfc_add_expr_to_block (block, append->head);
  append->head = NULL_TREE;
}


/* Get the current locus.  The structure may not be complete, and should
   only be used with gfc_set_backend_locus.  */

void
gfc_get_backend_locus (locus * loc)
{
  loc->lb = gfc_getmem (sizeof (gfc_linebuf));    
#ifdef USE_MAPPED_LOCATION
  loc->lb->location = input_location;
#else
  loc->lb->linenum = input_line;
#endif
  loc->lb->file = gfc_current_backend_file;
}


/* Set the current locus.  */

void
gfc_set_backend_locus (locus * loc)
{
  gfc_current_backend_file = loc->lb->file;
#ifdef USE_MAPPED_LOCATION
  input_location = loc->lb->location;
#else
  input_line = loc->lb->linenum;
  input_filename = loc->lb->file->filename;
#endif
}


/* Translate an executable statement.  */

tree
gfc_trans_code (gfc_code * code)
{
  stmtblock_t block;
  tree res;

  if (!code)
    return build_empty_stmt ();

  gfc_start_block (&block);

  /* Translate statements one by one to GIMPLE trees until we reach
     the end of this gfc_code branch.  */
  for (; code; code = code->next)
    {
      if (code->here != 0)
	{
	  res = gfc_trans_label_here (code);
	  gfc_add_expr_to_block (&block, res);
	}

      switch (code->op)
	{
	case EXEC_NOP:
	  res = NULL_TREE;
	  break;

	case EXEC_ASSIGN:
	  res = gfc_trans_assign (code);
	  break;

        case EXEC_LABEL_ASSIGN:
          res = gfc_trans_label_assign (code);
          break;

	case EXEC_POINTER_ASSIGN:
	  res = gfc_trans_pointer_assign (code);
	  break;

	case EXEC_CONTINUE:
	  res = NULL_TREE;
	  break;

	case EXEC_CYCLE:
	  res = gfc_trans_cycle (code);
	  break;

	case EXEC_EXIT:
	  res = gfc_trans_exit (code);
	  break;

	case EXEC_GOTO:
	  res = gfc_trans_goto (code);
	  break;

	case EXEC_ENTRY:
	  res = gfc_trans_entry (code);
	  break;

	case EXEC_PAUSE:
	  res = gfc_trans_pause (code);
	  break;

	case EXEC_STOP:
	  res = gfc_trans_stop (code);
	  break;

	case EXEC_CALL:
	  res = gfc_trans_call (code);
	  break;

	case EXEC_RETURN:
	  res = gfc_trans_return (code);
	  break;

	case EXEC_IF:
	  res = gfc_trans_if (code);
	  break;

	case EXEC_ARITHMETIC_IF:
	  res = gfc_trans_arithmetic_if (code);
	  break;

	case EXEC_DO:
	  res = gfc_trans_do (code);
	  break;

	case EXEC_DO_WHILE:
	  res = gfc_trans_do_while (code);
	  break;

	case EXEC_SELECT:
	  res = gfc_trans_select (code);
	  break;

	case EXEC_FLUSH:
	  res = gfc_trans_flush (code);
	  break;

	case EXEC_FORALL:
	  res = gfc_trans_forall (code);
	  break;

	case EXEC_WHERE:
	  res = gfc_trans_where (code);
	  break;

	case EXEC_ALLOCATE:
	  res = gfc_trans_allocate (code);
	  break;

	case EXEC_DEALLOCATE:
	  res = gfc_trans_deallocate (code);
	  break;

	case EXEC_OPEN:
	  res = gfc_trans_open (code);
	  break;

	case EXEC_CLOSE:
	  res = gfc_trans_close (code);
	  break;

	case EXEC_READ:
	  res = gfc_trans_read (code);
	  break;

	case EXEC_WRITE:
	  res = gfc_trans_write (code);
	  break;

	case EXEC_IOLENGTH:
	  res = gfc_trans_iolength (code);
	  break;

	case EXEC_BACKSPACE:
	  res = gfc_trans_backspace (code);
	  break;

	case EXEC_ENDFILE:
	  res = gfc_trans_endfile (code);
	  break;

	case EXEC_INQUIRE:
	  res = gfc_trans_inquire (code);
	  break;

	case EXEC_REWIND:
	  res = gfc_trans_rewind (code);
	  break;

	case EXEC_TRANSFER:
	  res = gfc_trans_transfer (code);
	  break;

	case EXEC_DT_END:
	  res = gfc_trans_dt_end (code);
	  break;

	default:
	  internal_error ("gfc_trans_code(): Bad statement code");
	}

      gfc_set_backend_locus (&code->loc);

      if (res != NULL_TREE && ! IS_EMPTY_STMT (res))
	{
	  if (TREE_CODE (res) == STATEMENT_LIST)
	    annotate_all_with_locus (&res, input_location);
	  else
	    SET_EXPR_LOCATION (res, input_location);
	    
	  /* Add the new statement to the block.  */
	  gfc_add_expr_to_block (&block, res);
	}
    }

  /* Return the finished block.  */
  return gfc_finish_block (&block);
}


/* This function is called after a complete program unit has been parsed
   and resolved.  */

void
gfc_generate_code (gfc_namespace * ns)
{
  if (ns->is_block_data)
    {
      gfc_generate_block_data (ns);
      return;
    }

  gfc_generate_function_code (ns);
}


/* This function is called after a complete module has been parsed
   and resolved.  */

void
gfc_generate_module_code (gfc_namespace * ns)
{
  gfc_namespace *n;

  gfc_generate_module_vars (ns);

  /* We need to generate all module function prototypes first, to allow
     sibling calls.  */
  for (n = ns->contained; n; n = n->sibling)
    {
      if (!n->proc_name)
        continue;

      gfc_create_function_decl (n);
    }

  for (n = ns->contained; n; n = n->sibling)
    {
      if (!n->proc_name)
        continue;

      gfc_generate_function_code (n);
    }
}

