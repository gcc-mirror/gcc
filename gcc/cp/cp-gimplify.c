/* C++-specific tree lowering bits; see also c-gimplify.c and tree-gimple.c.

   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Jason Merrill <jason@redhat.com>

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "cp-tree.h"
#include "c-common.h"
#include "toplev.h"
#include "tree-gimple.h"
#include "hashtab.h"
#include "pointer-set.h"

/* Genericize a TRY_BLOCK.  */

static void
genericize_try_block (tree *stmt_p)
{
  tree body = TRY_STMTS (*stmt_p);
  tree cleanup = TRY_HANDLERS (*stmt_p);

  gimplify_stmt (&body);

  if (CLEANUP_P (*stmt_p))
    /* A cleanup is an expression, so it doesn't need to be genericized.  */;
  else
    gimplify_stmt (&cleanup);

  *stmt_p = build2 (TRY_CATCH_EXPR, void_type_node, body, cleanup);
}

/* Genericize a HANDLER by converting to a CATCH_EXPR.  */

static void
genericize_catch_block (tree *stmt_p)
{
  tree type = HANDLER_TYPE (*stmt_p);
  tree body = HANDLER_BODY (*stmt_p);

  gimplify_stmt (&body);

  /* FIXME should the caught type go in TREE_TYPE?  */
  *stmt_p = build2 (CATCH_EXPR, void_type_node, type, body);
}

/* Genericize an EH_SPEC_BLOCK by converting it to a
   TRY_CATCH_EXPR/EH_FILTER_EXPR pair.  */

static void
genericize_eh_spec_block (tree *stmt_p)
{
  tree body = EH_SPEC_STMTS (*stmt_p);
  tree allowed = EH_SPEC_RAISES (*stmt_p);
  tree failure = build_call (call_unexpected_node,
			     tree_cons (NULL_TREE, build_exc_ptr (),
					NULL_TREE));
  gimplify_stmt (&body);

  *stmt_p = gimple_build_eh_filter (body, allowed, failure);
}

/* Genericize an IF_STMT by turning it into a COND_EXPR.  */

static void
gimplify_if_stmt (tree *stmt_p)
{
  tree stmt, cond, then_, else_;

  stmt = *stmt_p;
  cond = IF_COND (stmt);
  then_ = THEN_CLAUSE (stmt);
  else_ = ELSE_CLAUSE (stmt);

  if (!then_)
    then_ = build_empty_stmt ();
  if (!else_)
    else_ = build_empty_stmt ();

  if (integer_nonzerop (cond) && !TREE_SIDE_EFFECTS (else_))
    stmt = then_;
  else if (integer_zerop (cond) && !TREE_SIDE_EFFECTS (then_))
    stmt = else_;
  else
    stmt = build3 (COND_EXPR, void_type_node, cond, then_, else_);
  *stmt_p = stmt;
}

/* Gimplify initialization from an AGGR_INIT_EXPR.  */

static void
cp_gimplify_init_expr (tree *expr_p, tree *pre_p, tree *post_p)
{
  tree from = TREE_OPERAND (*expr_p, 1);
  tree to = TREE_OPERAND (*expr_p, 0);
  tree sub;

  /* If we are initializing something from a TARGET_EXPR, strip the
     TARGET_EXPR and initialize it directly.  */
  /* What about code that pulls out the temp and uses it elsewhere?  I
     think that such code never uses the TARGET_EXPR as an initializer.  If
     I'm wrong, we'll abort because the temp won't have any RTL.  In that
     case, I guess we'll need to replace references somehow.  */
  if (TREE_CODE (from) == TARGET_EXPR)
    from = TARGET_EXPR_INITIAL (from);
  if (TREE_CODE (from) == CLEANUP_POINT_EXPR)
    from = TREE_OPERAND (from, 0);

  /* Look through any COMPOUND_EXPRs.  */
  sub = expr_last (from);

  /* If we are initializing from an AGGR_INIT_EXPR, drop the INIT_EXPR and
     replace the slot operand with our target.

     Should we add a target parm to gimplify_expr instead?  No, as in this
     case we want to replace the INIT_EXPR.  */
  if (TREE_CODE (sub) == AGGR_INIT_EXPR)
    {
      gimplify_expr (&to, pre_p, post_p, is_gimple_lvalue, fb_lvalue);
      TREE_OPERAND (sub, 2) = to;
      *expr_p = from;

      /* The initialization is now a side-effect, so the container can
         become void.  */
      if (from != sub)
	TREE_TYPE (from) = void_type_node;
    }
}

/* Gimplify a MUST_NOT_THROW_EXPR.  */

static void
gimplify_must_not_throw_expr (tree *expr_p, tree *pre_p)
{
  tree stmt = *expr_p;
  tree temp = voidify_wrapper_expr (stmt, NULL);
  tree body = TREE_OPERAND (stmt, 0);

  gimplify_stmt (&body);

  stmt = gimple_build_eh_filter (body, NULL_TREE,
				 build_call (terminate_node, NULL_TREE));

  if (temp)
    {
      append_to_statement_list (stmt, pre_p);
      *expr_p = temp;
    }
  else
    *expr_p = stmt;
}

/* Do C++-specific gimplification.  Args are as for gimplify_expr.  */

int
cp_gimplify_expr (tree *expr_p, tree *pre_p, tree *post_p)
{
  int saved_stmts_are_full_exprs_p = 0;
  enum tree_code code = TREE_CODE (*expr_p);
  enum gimplify_status ret;

  if (STATEMENT_CODE_P (code))
    {
      saved_stmts_are_full_exprs_p = stmts_are_full_exprs_p ();
      current_stmt_tree ()->stmts_are_full_exprs_p
	= STMT_IS_FULL_EXPR_P (*expr_p);
    }

  switch (code)
    {
    case PTRMEM_CST:
      *expr_p = cplus_expand_constant (*expr_p);
      ret = GS_OK;
      break;

    case AGGR_INIT_EXPR:
      simplify_aggr_init_expr (expr_p);
      ret = GS_OK;
      break;

    case THROW_EXPR:
      /* FIXME communicate throw type to backend, probably by moving
	 THROW_EXPR into ../tree.def.  */
      *expr_p = TREE_OPERAND (*expr_p, 0);
      ret = GS_OK;
      break;

    case MUST_NOT_THROW_EXPR:
      gimplify_must_not_throw_expr (expr_p, pre_p);
      ret = GS_OK;
      break;

    case INIT_EXPR:
    case MODIFY_EXPR:
      cp_gimplify_init_expr (expr_p, pre_p, post_p);
      ret = GS_OK;
      break;

    case EMPTY_CLASS_EXPR:
      /* We create an INTEGER_CST with RECORD_TYPE and value zero.  */
      *expr_p = build_int_cst (TREE_TYPE (*expr_p), 0);
      ret = GS_OK;
      break;

    case BASELINK:
      *expr_p = BASELINK_FUNCTIONS (*expr_p);
      ret = GS_OK;
      break;

    case TRY_BLOCK:
      genericize_try_block (expr_p);
      ret = GS_OK;
      break;

    case HANDLER:
      genericize_catch_block (expr_p);
      ret = GS_OK;
      break;

    case EH_SPEC_BLOCK:
      genericize_eh_spec_block (expr_p);
      ret = GS_OK;
      break;

    case USING_STMT:
      /* Just ignore for now.  Eventually we will want to pass this on to
	 the debugger.  */
      *expr_p = build_empty_stmt ();
      ret = GS_ALL_DONE;
      break;

    case IF_STMT:
      gimplify_if_stmt (expr_p);
      ret = GS_OK;
      break;

    default:
      ret = c_gimplify_expr (expr_p, pre_p, post_p);
      break;
    }

  /* Restore saved state.  */
  if (STATEMENT_CODE_P (code))
    current_stmt_tree ()->stmts_are_full_exprs_p
      = saved_stmts_are_full_exprs_p;

  return ret;
}

static inline bool
is_invisiref_parm (tree t)
{
  return ((TREE_CODE (t) == PARM_DECL || TREE_CODE (t) == RESULT_DECL)
	  && DECL_BY_REFERENCE (t));
}

/* Perform any pre-gimplification lowering of C++ front end trees to
   GENERIC.  */

static tree
cp_genericize_r (tree *stmt_p, int *walk_subtrees, void *data)
{
  tree stmt = *stmt_p;
  struct pointer_set_t *p_set = (struct pointer_set_t*) data;

  if (is_invisiref_parm (stmt))
    {
      *stmt_p = convert_from_reference (stmt);
      *walk_subtrees = 0;
      return NULL;
    }

  /* Other than invisiref parms, don't walk the same tree twice.  */
  if (pointer_set_contains (p_set, stmt))
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  if (TREE_CODE (stmt) == ADDR_EXPR
      && is_invisiref_parm (TREE_OPERAND (stmt, 0)))
    {
      *stmt_p = convert (TREE_TYPE (stmt), TREE_OPERAND (stmt, 0));
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (stmt) == RETURN_EXPR
	   && TREE_OPERAND (stmt, 0)
	   && is_invisiref_parm (TREE_OPERAND (stmt, 0)))
    /* Don't dereference an invisiref RESULT_DECL inside a RETURN_EXPR.  */
    *walk_subtrees = 0;
  else if (IS_TYPE_OR_DECL_P (stmt))
    *walk_subtrees = 0;

  /* Due to the way voidify_wrapper_expr is written, we don't get a chance
     to lower this construct before scanning it, so we need to lower these
     before doing anything else.  */
  else if (TREE_CODE (stmt) == CLEANUP_STMT)
    *stmt_p = build2 (CLEANUP_EH_ONLY (stmt) ? TRY_CATCH_EXPR
					     : TRY_FINALLY_EXPR,
		      void_type_node,
		      CLEANUP_BODY (stmt),
		      CLEANUP_EXPR (stmt));

  pointer_set_insert (p_set, *stmt_p);
  
  return NULL;
}

void
cp_genericize (tree fndecl)
{
  tree t;
  struct pointer_set_t *p_set;

  /* Fix up the types of parms passed by invisible reference.  */
  for (t = DECL_ARGUMENTS (fndecl); t; t = TREE_CHAIN (t))
    if (TREE_ADDRESSABLE (TREE_TYPE (t)))
      {
	/* If a function's arguments are copied to create a thunk,
	   then DECL_BY_REFERENCE will be set -- but the type of the
	   argument will be a pointer type, so we will never get
	   here.  */
	gcc_assert (!DECL_BY_REFERENCE (t));
	gcc_assert (DECL_ARG_TYPE (t) != TREE_TYPE (t));
	TREE_TYPE (t) = DECL_ARG_TYPE (t);
	DECL_BY_REFERENCE (t) = 1;
	TREE_ADDRESSABLE (t) = 0;
	relayout_decl (t);
      }

  /* Do the same for the return value.  */
  if (TREE_ADDRESSABLE (TREE_TYPE (DECL_RESULT (fndecl))))
    {
      t = DECL_RESULT (fndecl);
      TREE_TYPE (t) = build_reference_type (TREE_TYPE (t));
      DECL_BY_REFERENCE (t) = 1;
      TREE_ADDRESSABLE (t) = 0;
      relayout_decl (t);
    }

  /* If we're a clone, the body is already GIMPLE.  */
  if (DECL_CLONED_FUNCTION_P (fndecl))
    return;

  /* We do want to see every occurrence of the parms, so we can't just use
     walk_tree's hash functionality.  */
  p_set = pointer_set_create ();
  walk_tree (&DECL_SAVED_TREE (fndecl), cp_genericize_r, p_set, NULL);
  pointer_set_destroy (p_set);

  /* Do everything else.  */
  c_genericize (fndecl);
}
