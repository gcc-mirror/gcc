/* C++-specific tree lowering bits; see also c-gimplify.c and tree-gimple.c.

   Copyright (C) 2002-2014 Free Software Foundation, Inc.
   Contributed by Jason Merrill <jason@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stor-layout.h"
#include "cp-tree.h"
#include "c-family/c-common.h"
#include "tree-iterator.h"
#include "pointer-set.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "hashtab.h"
#include "flags.h"
#include "splay-tree.h"
#include "target.h"
#include "c-family/c-ubsan.h"
#include "cilk.h"

/* Forward declarations.  */

static tree cp_genericize_r (tree *, int *, void *);
static void cp_genericize_tree (tree*);

/* Local declarations.  */

enum bc_t { bc_break = 0, bc_continue = 1 };

/* Stack of labels which are targets for "break" or "continue",
   linked through TREE_CHAIN.  */
static tree bc_label[2];

/* Begin a scope which can be exited by a break or continue statement.  BC
   indicates which.

   Just creates a label with location LOCATION and pushes it into the current
   context.  */

static tree
begin_bc_block (enum bc_t bc, location_t location)
{
  tree label = create_artificial_label (location);
  DECL_CHAIN (label) = bc_label[bc];
  bc_label[bc] = label;
  return label;
}

/* Finish a scope which can be exited by a break or continue statement.
   LABEL was returned from the most recent call to begin_bc_block.  BLOCK is
   an expression for the contents of the scope.

   If we saw a break (or continue) in the scope, append a LABEL_EXPR to
   BLOCK.  Otherwise, just forget the label.  */

static void
finish_bc_block (tree *block, enum bc_t bc, tree label)
{
  gcc_assert (label == bc_label[bc]);

  if (TREE_USED (label))
    append_to_statement_list (build1 (LABEL_EXPR, void_type_node, label),
			      block);

  bc_label[bc] = DECL_CHAIN (label);
  DECL_CHAIN (label) = NULL_TREE;
}

/* Get the LABEL_EXPR to represent a break or continue statement
   in the current block scope.  BC indicates which.  */

static tree
get_bc_label (enum bc_t bc)
{
  tree label = bc_label[bc];

  /* Mark the label used for finish_bc_block.  */
  TREE_USED (label) = 1;
  return label;
}

/* Genericize a TRY_BLOCK.  */

static void
genericize_try_block (tree *stmt_p)
{
  tree body = TRY_STMTS (*stmt_p);
  tree cleanup = TRY_HANDLERS (*stmt_p);

  *stmt_p = build2 (TRY_CATCH_EXPR, void_type_node, body, cleanup);
}

/* Genericize a HANDLER by converting to a CATCH_EXPR.  */

static void
genericize_catch_block (tree *stmt_p)
{
  tree type = HANDLER_TYPE (*stmt_p);
  tree body = HANDLER_BODY (*stmt_p);

  /* FIXME should the caught type go in TREE_TYPE?  */
  *stmt_p = build2 (CATCH_EXPR, void_type_node, type, body);
}

/* A terser interface for building a representation of an exception
   specification.  */

static tree
build_gimple_eh_filter_tree (tree body, tree allowed, tree failure)
{
  tree t;

  /* FIXME should the allowed types go in TREE_TYPE?  */
  t = build2 (EH_FILTER_EXPR, void_type_node, allowed, NULL_TREE);
  append_to_statement_list (failure, &EH_FILTER_FAILURE (t));

  t = build2 (TRY_CATCH_EXPR, void_type_node, NULL_TREE, t);
  append_to_statement_list (body, &TREE_OPERAND (t, 0));

  return t;
}

/* Genericize an EH_SPEC_BLOCK by converting it to a
   TRY_CATCH_EXPR/EH_FILTER_EXPR pair.  */

static void
genericize_eh_spec_block (tree *stmt_p)
{
  tree body = EH_SPEC_STMTS (*stmt_p);
  tree allowed = EH_SPEC_RAISES (*stmt_p);
  tree failure = build_call_n (call_unexpected_node, 1, build_exc_ptr ());

  *stmt_p = build_gimple_eh_filter_tree (body, allowed, failure);
  TREE_NO_WARNING (*stmt_p) = true;
  TREE_NO_WARNING (TREE_OPERAND (*stmt_p, 1)) = true;
}

/* Genericize an IF_STMT by turning it into a COND_EXPR.  */

static void
genericize_if_stmt (tree *stmt_p)
{
  tree stmt, cond, then_, else_;
  location_t locus = EXPR_LOCATION (*stmt_p);

  stmt = *stmt_p;
  cond = IF_COND (stmt);
  then_ = THEN_CLAUSE (stmt);
  else_ = ELSE_CLAUSE (stmt);

  if (!then_)
    then_ = build_empty_stmt (locus);
  if (!else_)
    else_ = build_empty_stmt (locus);

  if (integer_nonzerop (cond) && !TREE_SIDE_EFFECTS (else_))
    stmt = then_;
  else if (integer_zerop (cond) && !TREE_SIDE_EFFECTS (then_))
    stmt = else_;
  else
    stmt = build3 (COND_EXPR, void_type_node, cond, then_, else_);
  if (CAN_HAVE_LOCATION_P (stmt) && !EXPR_HAS_LOCATION (stmt))
    SET_EXPR_LOCATION (stmt, locus);
  *stmt_p = stmt;
}

/* Build a generic representation of one of the C loop forms.  COND is the
   loop condition or NULL_TREE.  BODY is the (possibly compound) statement
   controlled by the loop.  INCR is the increment expression of a for-loop,
   or NULL_TREE.  COND_IS_FIRST indicates whether the condition is
   evaluated before the loop body as in while and for loops, or after the
   loop body as in do-while loops.  */

static void
genericize_cp_loop (tree *stmt_p, location_t start_locus, tree cond, tree body,
		    tree incr, bool cond_is_first, int *walk_subtrees,
		    void *data)
{
  tree blab, clab;
  tree entry = NULL, exit = NULL, t;
  tree stmt_list = NULL;

  blab = begin_bc_block (bc_break, start_locus);
  clab = begin_bc_block (bc_continue, start_locus);

  if (incr && EXPR_P (incr))
    SET_EXPR_LOCATION (incr, start_locus);

  cp_walk_tree (&cond, cp_genericize_r, data, NULL);
  cp_walk_tree (&body, cp_genericize_r, data, NULL);
  cp_walk_tree (&incr, cp_genericize_r, data, NULL);
  *walk_subtrees = 0;

  /* If condition is zero don't generate a loop construct.  */
  if (cond && integer_zerop (cond))
    {
      if (cond_is_first)
	{
	  t = build1_loc (start_locus, GOTO_EXPR, void_type_node,
			  get_bc_label (bc_break));
	  append_to_statement_list (t, &stmt_list);
	}
    }
  else
    {
      /* Expand to gotos, just like c_finish_loop.  TODO: Use LOOP_EXPR.  */
      tree top = build1 (LABEL_EXPR, void_type_node,
			 create_artificial_label (start_locus));

      /* If we have an exit condition, then we build an IF with gotos either
	 out of the loop, or to the top of it.  If there's no exit condition,
	 then we just build a jump back to the top.  */
      exit = build1 (GOTO_EXPR, void_type_node, LABEL_EXPR_LABEL (top));

      if (cond && !integer_nonzerop (cond))
	{
	  /* Canonicalize the loop condition to the end.  This means
	     generating a branch to the loop condition.  Reuse the
	     continue label, if possible.  */
	  if (cond_is_first)
	    {
	      if (incr)
		{
		  entry = build1 (LABEL_EXPR, void_type_node,
				  create_artificial_label (start_locus));
		  t = build1_loc (start_locus, GOTO_EXPR, void_type_node,
				  LABEL_EXPR_LABEL (entry));
		}
	      else
		t = build1_loc (start_locus, GOTO_EXPR, void_type_node,
				get_bc_label (bc_continue));
	      append_to_statement_list (t, &stmt_list);
	    }

	  t = build1 (GOTO_EXPR, void_type_node, get_bc_label (bc_break));
	  exit = fold_build3_loc (start_locus,
				  COND_EXPR, void_type_node, cond, exit, t);
	}

      append_to_statement_list (top, &stmt_list);
    }

  append_to_statement_list (body, &stmt_list);
  finish_bc_block (&stmt_list, bc_continue, clab);
  append_to_statement_list (incr, &stmt_list);
  append_to_statement_list (entry, &stmt_list);
  append_to_statement_list (exit, &stmt_list);
  finish_bc_block (&stmt_list, bc_break, blab);

  if (stmt_list == NULL_TREE)
    stmt_list = build1 (NOP_EXPR, void_type_node, integer_zero_node);

  *stmt_p = stmt_list;
}

/* Genericize a FOR_STMT node *STMT_P.  */

static void
genericize_for_stmt (tree *stmt_p, int *walk_subtrees, void *data)
{
  tree stmt = *stmt_p;
  tree expr = NULL;
  tree loop;
  tree init = FOR_INIT_STMT (stmt);

  if (init)
    {
      cp_walk_tree (&init, cp_genericize_r, data, NULL);
      append_to_statement_list (init, &expr);
    }

  genericize_cp_loop (&loop, EXPR_LOCATION (stmt), FOR_COND (stmt),
		      FOR_BODY (stmt), FOR_EXPR (stmt), 1, walk_subtrees, data);
  append_to_statement_list (loop, &expr);
  *stmt_p = expr;
}

/* Genericize a WHILE_STMT node *STMT_P.  */

static void
genericize_while_stmt (tree *stmt_p, int *walk_subtrees, void *data)
{
  tree stmt = *stmt_p;
  genericize_cp_loop (stmt_p, EXPR_LOCATION (stmt), WHILE_COND (stmt),
		      WHILE_BODY (stmt), NULL_TREE, 1, walk_subtrees, data);
}

/* Genericize a DO_STMT node *STMT_P.  */

static void
genericize_do_stmt (tree *stmt_p, int *walk_subtrees, void *data)
{
  tree stmt = *stmt_p;
  genericize_cp_loop (stmt_p, EXPR_LOCATION (stmt), DO_COND (stmt),
		      DO_BODY (stmt), NULL_TREE, 0, walk_subtrees, data);
}

/* Genericize a SWITCH_STMT node *STMT_P by turning it into a SWITCH_EXPR.  */

static void
genericize_switch_stmt (tree *stmt_p, int *walk_subtrees, void *data)
{
  tree stmt = *stmt_p;
  tree break_block, body, cond, type;
  location_t stmt_locus = EXPR_LOCATION (stmt);

  break_block = begin_bc_block (bc_break, stmt_locus);

  body = SWITCH_STMT_BODY (stmt);
  if (!body)
    body = build_empty_stmt (stmt_locus);
  cond = SWITCH_STMT_COND (stmt);
  type = SWITCH_STMT_TYPE (stmt);

  cp_walk_tree (&body, cp_genericize_r, data, NULL);
  cp_walk_tree (&cond, cp_genericize_r, data, NULL);
  cp_walk_tree (&type, cp_genericize_r, data, NULL);
  *walk_subtrees = 0;

  *stmt_p = build3_loc (stmt_locus, SWITCH_EXPR, type, cond, body, NULL_TREE);
  finish_bc_block (stmt_p, bc_break, break_block);
}

/* Genericize a CONTINUE_STMT node *STMT_P.  */

static void
genericize_continue_stmt (tree *stmt_p)
{
  tree stmt_list = NULL;
  tree pred = build_predict_expr (PRED_CONTINUE, NOT_TAKEN);
  tree label = get_bc_label (bc_continue);
  location_t location = EXPR_LOCATION (*stmt_p);
  tree jump = build1_loc (location, GOTO_EXPR, void_type_node, label);
  append_to_statement_list (pred, &stmt_list);
  append_to_statement_list (jump, &stmt_list);
  *stmt_p = stmt_list;
}

/* Genericize a BREAK_STMT node *STMT_P.  */

static void
genericize_break_stmt (tree *stmt_p)
{
  tree label = get_bc_label (bc_break);
  location_t location = EXPR_LOCATION (*stmt_p);
  *stmt_p = build1_loc (location, GOTO_EXPR, void_type_node, label);
}

/* Genericize a OMP_FOR node *STMT_P.  */

static void
genericize_omp_for_stmt (tree *stmt_p, int *walk_subtrees, void *data)
{
  tree stmt = *stmt_p;
  location_t locus = EXPR_LOCATION (stmt);
  tree clab = begin_bc_block (bc_continue, locus);

  cp_walk_tree (&OMP_FOR_BODY (stmt), cp_genericize_r, data, NULL);
  cp_walk_tree (&OMP_FOR_CLAUSES (stmt), cp_genericize_r, data, NULL);
  cp_walk_tree (&OMP_FOR_INIT (stmt), cp_genericize_r, data, NULL);
  cp_walk_tree (&OMP_FOR_COND (stmt), cp_genericize_r, data, NULL);
  cp_walk_tree (&OMP_FOR_INCR (stmt), cp_genericize_r, data, NULL);
  cp_walk_tree (&OMP_FOR_PRE_BODY (stmt), cp_genericize_r, data, NULL);
  *walk_subtrees = 0;

  finish_bc_block (&OMP_FOR_BODY (stmt), bc_continue, clab);
}

/* Hook into the middle of gimplifying an OMP_FOR node.  */

static enum gimplify_status
cp_gimplify_omp_for (tree *expr_p, gimple_seq *pre_p)
{
  tree for_stmt = *expr_p;
  gimple_seq seq = NULL;

  /* Protect ourselves from recursion.  */
  if (OMP_FOR_GIMPLIFYING_P (for_stmt))
    return GS_UNHANDLED;
  OMP_FOR_GIMPLIFYING_P (for_stmt) = 1;

  gimplify_and_add (for_stmt, &seq);
  gimple_seq_add_seq (pre_p, seq);

  OMP_FOR_GIMPLIFYING_P (for_stmt) = 0;

  return GS_ALL_DONE;
}

/*  Gimplify an EXPR_STMT node.  */

static void
gimplify_expr_stmt (tree *stmt_p)
{
  tree stmt = EXPR_STMT_EXPR (*stmt_p);

  if (stmt == error_mark_node)
    stmt = NULL;

  /* Gimplification of a statement expression will nullify the
     statement if all its side effects are moved to *PRE_P and *POST_P.

     In this case we will not want to emit the gimplified statement.
     However, we may still want to emit a warning, so we do that before
     gimplification.  */
  if (stmt && warn_unused_value)
    {
      if (!TREE_SIDE_EFFECTS (stmt))
	{
	  if (!IS_EMPTY_STMT (stmt)
	      && !VOID_TYPE_P (TREE_TYPE (stmt))
	      && !TREE_NO_WARNING (stmt))
	    warning (OPT_Wunused_value, "statement with no effect");
	}
      else
	warn_if_unused_value (stmt, input_location);
    }

  if (stmt == NULL_TREE)
    stmt = alloc_stmt_list ();

  *stmt_p = stmt;
}

/* Gimplify initialization from an AGGR_INIT_EXPR.  */

static void
cp_gimplify_init_expr (tree *expr_p)
{
  tree from = TREE_OPERAND (*expr_p, 1);
  tree to = TREE_OPERAND (*expr_p, 0);
  tree t;

  /* What about code that pulls out the temp and uses it elsewhere?  I
     think that such code never uses the TARGET_EXPR as an initializer.  If
     I'm wrong, we'll abort because the temp won't have any RTL.  In that
     case, I guess we'll need to replace references somehow.  */
  if (TREE_CODE (from) == TARGET_EXPR)
    from = TARGET_EXPR_INITIAL (from);

  /* Look through any COMPOUND_EXPRs, since build_compound_expr pushes them
     inside the TARGET_EXPR.  */
  for (t = from; t; )
    {
      tree sub = TREE_CODE (t) == COMPOUND_EXPR ? TREE_OPERAND (t, 0) : t;

      /* If we are initializing from an AGGR_INIT_EXPR, drop the INIT_EXPR and
	 replace the slot operand with our target.

	 Should we add a target parm to gimplify_expr instead?  No, as in this
	 case we want to replace the INIT_EXPR.  */
      if (TREE_CODE (sub) == AGGR_INIT_EXPR
	  || TREE_CODE (sub) == VEC_INIT_EXPR)
	{
	  if (TREE_CODE (sub) == AGGR_INIT_EXPR)
	    AGGR_INIT_EXPR_SLOT (sub) = to;
	  else
	    VEC_INIT_EXPR_SLOT (sub) = to;
	  *expr_p = from;

	  /* The initialization is now a side-effect, so the container can
	     become void.  */
	  if (from != sub)
	    TREE_TYPE (from) = void_type_node;
	}

      if (t == sub)
	break;
      else
	t = TREE_OPERAND (t, 1);
    }

}

/* Gimplify a MUST_NOT_THROW_EXPR.  */

static enum gimplify_status
gimplify_must_not_throw_expr (tree *expr_p, gimple_seq *pre_p)
{
  tree stmt = *expr_p;
  tree temp = voidify_wrapper_expr (stmt, NULL);
  tree body = TREE_OPERAND (stmt, 0);
  gimple_seq try_ = NULL;
  gimple_seq catch_ = NULL;
  gimple mnt;

  gimplify_and_add (body, &try_);
  mnt = gimple_build_eh_must_not_throw (terminate_node);
  gimple_seq_add_stmt_without_update (&catch_, mnt);
  mnt = gimple_build_try (try_, catch_, GIMPLE_TRY_CATCH);

  gimple_seq_add_stmt_without_update (pre_p, mnt);
  if (temp)
    {
      *expr_p = temp;
      return GS_OK;
    }

  *expr_p = NULL;
  return GS_ALL_DONE;
}

/* Do C++-specific gimplification.  Args are as for gimplify_expr.  */

int
cp_gimplify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
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

    case VEC_INIT_EXPR:
      {
	location_t loc = input_location;
	tree init = VEC_INIT_EXPR_INIT (*expr_p);
	int from_array = (init && TREE_CODE (TREE_TYPE (init)) == ARRAY_TYPE);
	gcc_assert (EXPR_HAS_LOCATION (*expr_p));
	input_location = EXPR_LOCATION (*expr_p);
	*expr_p = build_vec_init (VEC_INIT_EXPR_SLOT (*expr_p), NULL_TREE,
				  init, VEC_INIT_EXPR_VALUE_INIT (*expr_p),
				  from_array,
				  tf_warning_or_error);
	cp_genericize_tree (expr_p);
	ret = GS_OK;
	input_location = loc;
      }
      break;

    case THROW_EXPR:
      /* FIXME communicate throw type to back end, probably by moving
	 THROW_EXPR into ../tree.def.  */
      *expr_p = TREE_OPERAND (*expr_p, 0);
      ret = GS_OK;
      break;

    case MUST_NOT_THROW_EXPR:
      ret = gimplify_must_not_throw_expr (expr_p, pre_p);
      break;

      /* We used to do this for MODIFY_EXPR as well, but that's unsafe; the
	 LHS of an assignment might also be involved in the RHS, as in bug
	 25979.  */
    case INIT_EXPR:
      if (fn_contains_cilk_spawn_p (cfun)
	  && cilk_detect_spawn_and_unwrap (expr_p)
	  && !seen_error ())
	return (enum gimplify_status) gimplify_cilk_spawn (expr_p);
      cp_gimplify_init_expr (expr_p);
      if (TREE_CODE (*expr_p) != INIT_EXPR)
	return GS_OK;
      /* Otherwise fall through.  */
    case MODIFY_EXPR:
      {
	if (fn_contains_cilk_spawn_p (cfun)
	    && cilk_detect_spawn_and_unwrap (expr_p)
	    && !seen_error ())
	  return (enum gimplify_status) gimplify_cilk_spawn (expr_p);

	/* If the back end isn't clever enough to know that the lhs and rhs
	   types are the same, add an explicit conversion.  */
	tree op0 = TREE_OPERAND (*expr_p, 0);
	tree op1 = TREE_OPERAND (*expr_p, 1);

	if (!error_operand_p (op0)
	    && !error_operand_p (op1)
	    && (TYPE_STRUCTURAL_EQUALITY_P (TREE_TYPE (op0))
		|| TYPE_STRUCTURAL_EQUALITY_P (TREE_TYPE (op1)))
	    && !useless_type_conversion_p (TREE_TYPE (op1), TREE_TYPE (op0)))
	  TREE_OPERAND (*expr_p, 1) = build1 (VIEW_CONVERT_EXPR,
					      TREE_TYPE (op0), op1);

	else if ((is_gimple_lvalue (op1) || INDIRECT_REF_P (op1)
		  || (TREE_CODE (op1) == CONSTRUCTOR
		      && CONSTRUCTOR_NELTS (op1) == 0
		      && !TREE_CLOBBER_P (op1))
		  || (TREE_CODE (op1) == CALL_EXPR
		      && !CALL_EXPR_RETURN_SLOT_OPT (op1)))
		 && is_really_empty_class (TREE_TYPE (op0)))
	  {
	    /* Remove any copies of empty classes.  We check that the RHS
	       has a simple form so that TARGET_EXPRs and non-empty
	       CONSTRUCTORs get reduced properly, and we leave the return
	       slot optimization alone because it isn't a copy (FIXME so it
	       shouldn't be represented as one).

	       Also drop volatile variables on the RHS to avoid infinite
	       recursion from gimplify_expr trying to load the value.  */
	    if (!TREE_SIDE_EFFECTS (op1)
		|| (DECL_P (op1) && TREE_THIS_VOLATILE (op1)))
	      *expr_p = op0;
	    else if (TREE_CODE (op1) == MEM_REF
		     && TREE_THIS_VOLATILE (op1))
	      {
		/* Similarly for volatile MEM_REFs on the RHS.  */
		if (!TREE_SIDE_EFFECTS (TREE_OPERAND (op1, 0)))
		  *expr_p = op0;
		else
		  *expr_p = build2 (COMPOUND_EXPR, TREE_TYPE (*expr_p),
				    TREE_OPERAND (op1, 0), op0);
	      }
	    else
	      *expr_p = build2 (COMPOUND_EXPR, TREE_TYPE (*expr_p),
				op0, op1);
	  }
      }
      ret = GS_OK;
      break;

    case EMPTY_CLASS_EXPR:
      /* We create an empty CONSTRUCTOR with RECORD_TYPE.  */
      *expr_p = build_constructor (TREE_TYPE (*expr_p), NULL);
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
      gcc_unreachable ();

    case FOR_STMT:
    case WHILE_STMT:
    case DO_STMT:
    case SWITCH_STMT:
    case CONTINUE_STMT:
    case BREAK_STMT:
      gcc_unreachable ();

    case OMP_FOR:
    case OMP_SIMD:
    case OMP_DISTRIBUTE:
      ret = cp_gimplify_omp_for (expr_p, pre_p);
      break;

    case EXPR_STMT:
      gimplify_expr_stmt (expr_p);
      ret = GS_OK;
      break;

    case UNARY_PLUS_EXPR:
      {
	tree arg = TREE_OPERAND (*expr_p, 0);
	tree type = TREE_TYPE (*expr_p);
	*expr_p = (TREE_TYPE (arg) != type) ? fold_convert (type, arg)
					    : arg;
	ret = GS_OK;
      }
      break;

    case CILK_SPAWN_STMT:
      gcc_assert 
	(fn_contains_cilk_spawn_p (cfun) 
	 && cilk_detect_spawn_and_unwrap (expr_p));

      /* If errors are seen, then just process it as a CALL_EXPR.  */
      if (!seen_error ())
	return (enum gimplify_status) gimplify_cilk_spawn (expr_p);
      
    case CALL_EXPR:
      if (fn_contains_cilk_spawn_p (cfun)
	  && cilk_detect_spawn_and_unwrap (expr_p)
	  && !seen_error ())
	return (enum gimplify_status) gimplify_cilk_spawn (expr_p);

    default:
      ret = (enum gimplify_status) c_gimplify_expr (expr_p, pre_p, post_p);
      break;
    }

  /* Restore saved state.  */
  if (STATEMENT_CODE_P (code))
    current_stmt_tree ()->stmts_are_full_exprs_p
      = saved_stmts_are_full_exprs_p;

  return ret;
}

static inline bool
is_invisiref_parm (const_tree t)
{
  return ((TREE_CODE (t) == PARM_DECL || TREE_CODE (t) == RESULT_DECL)
	  && DECL_BY_REFERENCE (t));
}

/* Return true if the uid in both int tree maps are equal.  */

int
cxx_int_tree_map_eq (const void *va, const void *vb)
{
  const struct cxx_int_tree_map *a = (const struct cxx_int_tree_map *) va;
  const struct cxx_int_tree_map *b = (const struct cxx_int_tree_map *) vb;
  return (a->uid == b->uid);
}

/* Hash a UID in a cxx_int_tree_map.  */

unsigned int
cxx_int_tree_map_hash (const void *item)
{
  return ((const struct cxx_int_tree_map *)item)->uid;
}

/* A stable comparison routine for use with splay trees and DECLs.  */

static int
splay_tree_compare_decl_uid (splay_tree_key xa, splay_tree_key xb)
{
  tree a = (tree) xa;
  tree b = (tree) xb;

  return DECL_UID (a) - DECL_UID (b);
}

/* OpenMP context during genericization.  */

struct cp_genericize_omp_taskreg
{
  bool is_parallel;
  bool default_shared;
  struct cp_genericize_omp_taskreg *outer;
  splay_tree variables;
};

/* Return true if genericization should try to determine if
   DECL is firstprivate or shared within task regions.  */

static bool
omp_var_to_track (tree decl)
{
  tree type = TREE_TYPE (decl);
  if (is_invisiref_parm (decl))
    type = TREE_TYPE (type);
  while (TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);
  if (type == error_mark_node || !CLASS_TYPE_P (type))
    return false;
  if (VAR_P (decl) && DECL_THREAD_LOCAL_P (decl))
    return false;
  if (cxx_omp_predetermined_sharing (decl) != OMP_CLAUSE_DEFAULT_UNSPECIFIED)
    return false;
  return true;
}

/* Note DECL use in OpenMP region OMP_CTX during genericization.  */

static void
omp_cxx_notice_variable (struct cp_genericize_omp_taskreg *omp_ctx, tree decl)
{
  splay_tree_node n = splay_tree_lookup (omp_ctx->variables,
					 (splay_tree_key) decl);
  if (n == NULL)
    {
      int flags = OMP_CLAUSE_DEFAULT_SHARED;
      if (omp_ctx->outer)
	omp_cxx_notice_variable (omp_ctx->outer, decl);
      if (!omp_ctx->default_shared)
	{
	  struct cp_genericize_omp_taskreg *octx;

	  for (octx = omp_ctx->outer; octx; octx = octx->outer)
	    {
	      n = splay_tree_lookup (octx->variables, (splay_tree_key) decl);
	      if (n && n->value != OMP_CLAUSE_DEFAULT_SHARED)
		{
		  flags = OMP_CLAUSE_DEFAULT_FIRSTPRIVATE;
		  break;
		}
	      if (octx->is_parallel)
		break;
	    }
	  if (octx == NULL
	      && (TREE_CODE (decl) == PARM_DECL
		  || (!(TREE_STATIC (decl) || DECL_EXTERNAL (decl))
		      && DECL_CONTEXT (decl) == current_function_decl)))
	    flags = OMP_CLAUSE_DEFAULT_FIRSTPRIVATE;
	  if (flags == OMP_CLAUSE_DEFAULT_FIRSTPRIVATE)
	    {
	      /* DECL is implicitly determined firstprivate in
		 the current task construct.  Ensure copy ctor and
		 dtor are instantiated, because during gimplification
		 it will be already too late.  */
	      tree type = TREE_TYPE (decl);
	      if (is_invisiref_parm (decl))
		type = TREE_TYPE (type);
	      while (TREE_CODE (type) == ARRAY_TYPE)
		type = TREE_TYPE (type);
	      get_copy_ctor (type, tf_none);
	      get_dtor (type, tf_none);
	    }
	}
      splay_tree_insert (omp_ctx->variables, (splay_tree_key) decl, flags);
    }
}

/* Genericization context.  */

struct cp_genericize_data
{
  struct pointer_set_t *p_set;
  vec<tree> bind_expr_stack;
  struct cp_genericize_omp_taskreg *omp_ctx;
};

/* Perform any pre-gimplification lowering of C++ front end trees to
   GENERIC.  */

static tree
cp_genericize_r (tree *stmt_p, int *walk_subtrees, void *data)
{
  tree stmt = *stmt_p;
  struct cp_genericize_data *wtd = (struct cp_genericize_data *) data;
  struct pointer_set_t *p_set = wtd->p_set;

  /* If in an OpenMP context, note var uses.  */
  if (__builtin_expect (wtd->omp_ctx != NULL, 0)
      && (VAR_P (stmt)
	  || TREE_CODE (stmt) == PARM_DECL
	  || TREE_CODE (stmt) == RESULT_DECL)
      && omp_var_to_track (stmt))
    omp_cxx_notice_variable (wtd->omp_ctx, stmt);

  if (is_invisiref_parm (stmt)
      /* Don't dereference parms in a thunk, pass the references through. */
      && !(DECL_THUNK_P (current_function_decl)
	   && TREE_CODE (stmt) == PARM_DECL))
    {
      *stmt_p = convert_from_reference (stmt);
      *walk_subtrees = 0;
      return NULL;
    }

  /* Map block scope extern declarations to visible declarations with the
     same name and type in outer scopes if any.  */
  if (cp_function_chain->extern_decl_map
      && VAR_OR_FUNCTION_DECL_P (stmt)
      && DECL_EXTERNAL (stmt))
    {
      struct cxx_int_tree_map *h, in;
      in.uid = DECL_UID (stmt);
      h = (struct cxx_int_tree_map *)
	  htab_find_with_hash (cp_function_chain->extern_decl_map,
			       &in, in.uid);
      if (h)
	{
	  *stmt_p = h->to;
	  *walk_subtrees = 0;
	  return NULL;
	}
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
      /* If in an OpenMP context, note var uses.  */
      if (__builtin_expect (wtd->omp_ctx != NULL, 0)
	  && omp_var_to_track (TREE_OPERAND (stmt, 0)))
	omp_cxx_notice_variable (wtd->omp_ctx, TREE_OPERAND (stmt, 0));
      *stmt_p = convert (TREE_TYPE (stmt), TREE_OPERAND (stmt, 0));
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (stmt) == RETURN_EXPR
	   && TREE_OPERAND (stmt, 0)
	   && is_invisiref_parm (TREE_OPERAND (stmt, 0)))
    /* Don't dereference an invisiref RESULT_DECL inside a RETURN_EXPR.  */
    *walk_subtrees = 0;
  else if (TREE_CODE (stmt) == OMP_CLAUSE)
    switch (OMP_CLAUSE_CODE (stmt))
      {
      case OMP_CLAUSE_LASTPRIVATE:
	/* Don't dereference an invisiref in OpenMP clauses.  */
	if (is_invisiref_parm (OMP_CLAUSE_DECL (stmt)))
	  {
	    *walk_subtrees = 0;
	    if (OMP_CLAUSE_LASTPRIVATE_STMT (stmt))
	      cp_walk_tree (&OMP_CLAUSE_LASTPRIVATE_STMT (stmt),
			    cp_genericize_r, data, NULL);
	  }
	break;
      case OMP_CLAUSE_PRIVATE:
	/* Don't dereference an invisiref in OpenMP clauses.  */
	if (is_invisiref_parm (OMP_CLAUSE_DECL (stmt)))
	  *walk_subtrees = 0;
	else if (wtd->omp_ctx != NULL)
	  {
	    /* Private clause doesn't cause any references to the
	       var in outer contexts, avoid calling
	       omp_cxx_notice_variable for it.  */
	    struct cp_genericize_omp_taskreg *old = wtd->omp_ctx;
	    wtd->omp_ctx = NULL;
	    cp_walk_tree (&OMP_CLAUSE_DECL (stmt), cp_genericize_r,
			  data, NULL);
	    wtd->omp_ctx = old;
	    *walk_subtrees = 0;
	  }
	break;
      case OMP_CLAUSE_SHARED:
      case OMP_CLAUSE_FIRSTPRIVATE:
      case OMP_CLAUSE_COPYIN:
      case OMP_CLAUSE_COPYPRIVATE:
	/* Don't dereference an invisiref in OpenMP clauses.  */
	if (is_invisiref_parm (OMP_CLAUSE_DECL (stmt)))
	  *walk_subtrees = 0;
	break;
      case OMP_CLAUSE_REDUCTION:
	/* Don't dereference an invisiref in reduction clause's
	   OMP_CLAUSE_DECL either.  OMP_CLAUSE_REDUCTION_{INIT,MERGE}
	   still needs to be genericized.  */
	if (is_invisiref_parm (OMP_CLAUSE_DECL (stmt)))
	  {
	    *walk_subtrees = 0;
	    if (OMP_CLAUSE_REDUCTION_INIT (stmt))
	      cp_walk_tree (&OMP_CLAUSE_REDUCTION_INIT (stmt),
			    cp_genericize_r, data, NULL);
	    if (OMP_CLAUSE_REDUCTION_MERGE (stmt))
	      cp_walk_tree (&OMP_CLAUSE_REDUCTION_MERGE (stmt),
			    cp_genericize_r, data, NULL);
	  }
	break;
      default:
	break;
      }
  else if (IS_TYPE_OR_DECL_P (stmt))
    *walk_subtrees = 0;

  /* Due to the way voidify_wrapper_expr is written, we don't get a chance
     to lower this construct before scanning it, so we need to lower these
     before doing anything else.  */
  else if (TREE_CODE (stmt) == CLEANUP_STMT)
    *stmt_p = build2_loc (EXPR_LOCATION (stmt),
			  CLEANUP_EH_ONLY (stmt) ? TRY_CATCH_EXPR
						 : TRY_FINALLY_EXPR,
			  void_type_node,
			  CLEANUP_BODY (stmt),
			  CLEANUP_EXPR (stmt));

  else if (TREE_CODE (stmt) == IF_STMT)
    {
      genericize_if_stmt (stmt_p);
      /* *stmt_p has changed, tail recurse to handle it again.  */
      return cp_genericize_r (stmt_p, walk_subtrees, data);
    }

  /* COND_EXPR might have incompatible types in branches if one or both
     arms are bitfields.  Fix it up now.  */
  else if (TREE_CODE (stmt) == COND_EXPR)
    {
      tree type_left
	= (TREE_OPERAND (stmt, 1)
	   ? is_bitfield_expr_with_lowered_type (TREE_OPERAND (stmt, 1))
	   : NULL_TREE);
      tree type_right
	= (TREE_OPERAND (stmt, 2)
	   ? is_bitfield_expr_with_lowered_type (TREE_OPERAND (stmt, 2))
	   : NULL_TREE);
      if (type_left
	  && !useless_type_conversion_p (TREE_TYPE (stmt),
					 TREE_TYPE (TREE_OPERAND (stmt, 1))))
	{
	  TREE_OPERAND (stmt, 1)
	    = fold_convert (type_left, TREE_OPERAND (stmt, 1));
	  gcc_assert (useless_type_conversion_p (TREE_TYPE (stmt),
						 type_left));
	}
      if (type_right
	  && !useless_type_conversion_p (TREE_TYPE (stmt),
					 TREE_TYPE (TREE_OPERAND (stmt, 2))))
	{
	  TREE_OPERAND (stmt, 2)
	    = fold_convert (type_right, TREE_OPERAND (stmt, 2));
	  gcc_assert (useless_type_conversion_p (TREE_TYPE (stmt),
						 type_right));
	}
    }

  else if (TREE_CODE (stmt) == BIND_EXPR)
    {
      if (__builtin_expect (wtd->omp_ctx != NULL, 0))
	{
	  tree decl;
	  for (decl = BIND_EXPR_VARS (stmt); decl; decl = DECL_CHAIN (decl))
	    if (VAR_P (decl)
		&& !DECL_EXTERNAL (decl)
		&& omp_var_to_track (decl))
	      {
		splay_tree_node n
		  = splay_tree_lookup (wtd->omp_ctx->variables,
				       (splay_tree_key) decl);
		if (n == NULL)
		  splay_tree_insert (wtd->omp_ctx->variables,
				     (splay_tree_key) decl,
				     TREE_STATIC (decl)
				     ? OMP_CLAUSE_DEFAULT_SHARED
				     : OMP_CLAUSE_DEFAULT_PRIVATE);
	      }
	}
      wtd->bind_expr_stack.safe_push (stmt);
      cp_walk_tree (&BIND_EXPR_BODY (stmt),
		    cp_genericize_r, data, NULL);
      wtd->bind_expr_stack.pop ();
    }

  else if (TREE_CODE (stmt) == USING_STMT)
    {
      tree block = NULL_TREE;

      /* Get the innermost inclosing GIMPLE_BIND that has a non NULL
         BLOCK, and append an IMPORTED_DECL to its
	 BLOCK_VARS chained list.  */
      if (wtd->bind_expr_stack.exists ())
	{
	  int i;
	  for (i = wtd->bind_expr_stack.length () - 1; i >= 0; i--)
	    if ((block = BIND_EXPR_BLOCK (wtd->bind_expr_stack[i])))
	      break;
	}
      if (block)
	{
	  tree using_directive;
	  gcc_assert (TREE_OPERAND (stmt, 0));

	  using_directive = make_node (IMPORTED_DECL);
	  TREE_TYPE (using_directive) = void_type_node;

	  IMPORTED_DECL_ASSOCIATED_DECL (using_directive)
	    = TREE_OPERAND (stmt, 0);
	  DECL_CHAIN (using_directive) = BLOCK_VARS (block);
	  BLOCK_VARS (block) = using_directive;
	}
      /* The USING_STMT won't appear in GENERIC.  */
      *stmt_p = build1 (NOP_EXPR, void_type_node, integer_zero_node);
      *walk_subtrees = 0;
    }

  else if (TREE_CODE (stmt) == DECL_EXPR
	   && TREE_CODE (DECL_EXPR_DECL (stmt)) == USING_DECL)
    {
      /* Using decls inside DECL_EXPRs are just dropped on the floor.  */
      *stmt_p = build1 (NOP_EXPR, void_type_node, integer_zero_node);
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (stmt) == OMP_PARALLEL || TREE_CODE (stmt) == OMP_TASK)
    {
      struct cp_genericize_omp_taskreg omp_ctx;
      tree c, decl;
      splay_tree_node n;

      *walk_subtrees = 0;
      cp_walk_tree (&OMP_CLAUSES (stmt), cp_genericize_r, data, NULL);
      omp_ctx.is_parallel = TREE_CODE (stmt) == OMP_PARALLEL;
      omp_ctx.default_shared = omp_ctx.is_parallel;
      omp_ctx.outer = wtd->omp_ctx;
      omp_ctx.variables = splay_tree_new (splay_tree_compare_decl_uid, 0, 0);
      wtd->omp_ctx = &omp_ctx;
      for (c = OMP_CLAUSES (stmt); c; c = OMP_CLAUSE_CHAIN (c))
	switch (OMP_CLAUSE_CODE (c))
	  {
	  case OMP_CLAUSE_SHARED:
	  case OMP_CLAUSE_PRIVATE:
	  case OMP_CLAUSE_FIRSTPRIVATE:
	  case OMP_CLAUSE_LASTPRIVATE:
	    decl = OMP_CLAUSE_DECL (c);
	    if (decl == error_mark_node || !omp_var_to_track (decl))
	      break;
	    n = splay_tree_lookup (omp_ctx.variables, (splay_tree_key) decl);
	    if (n != NULL)
	      break;
	    splay_tree_insert (omp_ctx.variables, (splay_tree_key) decl,
			       OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
			       ? OMP_CLAUSE_DEFAULT_SHARED
			       : OMP_CLAUSE_DEFAULT_PRIVATE);
	    if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_PRIVATE
		&& omp_ctx.outer)
	      omp_cxx_notice_variable (omp_ctx.outer, decl);
	    break;
	  case OMP_CLAUSE_DEFAULT:
	    if (OMP_CLAUSE_DEFAULT_KIND (c) == OMP_CLAUSE_DEFAULT_SHARED)
	      omp_ctx.default_shared = true;
	  default:
	    break;
	  }
      cp_walk_tree (&OMP_BODY (stmt), cp_genericize_r, data, NULL);
      wtd->omp_ctx = omp_ctx.outer;
      splay_tree_delete (omp_ctx.variables);
    }
  else if (TREE_CODE (stmt) == CONVERT_EXPR)
    gcc_assert (!CONVERT_EXPR_VBASE_PATH (stmt));
  else if (TREE_CODE (stmt) == FOR_STMT)
    genericize_for_stmt (stmt_p, walk_subtrees, data);
  else if (TREE_CODE (stmt) == WHILE_STMT)
    genericize_while_stmt (stmt_p, walk_subtrees, data);
  else if (TREE_CODE (stmt) == DO_STMT)
    genericize_do_stmt (stmt_p, walk_subtrees, data);
  else if (TREE_CODE (stmt) == SWITCH_STMT)
    genericize_switch_stmt (stmt_p, walk_subtrees, data);
  else if (TREE_CODE (stmt) == CONTINUE_STMT)
    genericize_continue_stmt (stmt_p);
  else if (TREE_CODE (stmt) == BREAK_STMT)
    genericize_break_stmt (stmt_p);
  else if (TREE_CODE (stmt) == OMP_FOR
	   || TREE_CODE (stmt) == OMP_SIMD
	   || TREE_CODE (stmt) == OMP_DISTRIBUTE)
    genericize_omp_for_stmt (stmt_p, walk_subtrees, data);
  else if (TREE_CODE (stmt) == SIZEOF_EXPR)
    {
      if (SIZEOF_EXPR_TYPE_P (stmt))
	*stmt_p
	  = cxx_sizeof_or_alignof_type (TREE_TYPE (TREE_OPERAND (stmt, 0)),
					SIZEOF_EXPR, false);
      else if (TYPE_P (TREE_OPERAND (stmt, 0)))
	*stmt_p = cxx_sizeof_or_alignof_type (TREE_OPERAND (stmt, 0),
					      SIZEOF_EXPR, false);
      else
	*stmt_p = cxx_sizeof_or_alignof_expr (TREE_OPERAND (stmt, 0),
					      SIZEOF_EXPR, false);
      if (*stmt_p == error_mark_node)
	*stmt_p = size_one_node;
      return NULL;
    }    

  pointer_set_insert (p_set, *stmt_p);

  return NULL;
}

/* Lower C++ front end trees to GENERIC in T_P.  */

static void
cp_genericize_tree (tree* t_p)
{
  struct cp_genericize_data wtd;

  wtd.p_set = pointer_set_create ();
  wtd.bind_expr_stack.create (0);
  wtd.omp_ctx = NULL;
  cp_walk_tree (t_p, cp_genericize_r, &wtd, NULL);
  pointer_set_destroy (wtd.p_set);
  wtd.bind_expr_stack.release ();
}

/* If a function that should end with a return in non-void
   function doesn't obviously end with return, add ubsan
   instrmentation code to verify it at runtime.  */

static void
cp_ubsan_maybe_instrument_return (tree fndecl)
{
  if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (fndecl)))
      || DECL_CONSTRUCTOR_P (fndecl)
      || DECL_DESTRUCTOR_P (fndecl)
      || !targetm.warn_func_return (fndecl))
    return;

  tree t = DECL_SAVED_TREE (fndecl);
  while (t)
    {
      switch (TREE_CODE (t))
	{
	case BIND_EXPR:
	  t = BIND_EXPR_BODY (t);
	  continue;
	case TRY_FINALLY_EXPR:
	  t = TREE_OPERAND (t, 0);
	  continue;
	case STATEMENT_LIST:
	  {
	    tree_stmt_iterator i = tsi_last (t);
	    if (!tsi_end_p (i))
	      {
		t = tsi_stmt (i);
		continue;
	      }
	  }
	  break;
	case RETURN_EXPR:
	  return;
	default:
	  break;
	}
      break;
    }
  if (t == NULL_TREE)
    return;
  t = DECL_SAVED_TREE (fndecl);
  if (TREE_CODE (t) == BIND_EXPR
      && TREE_CODE (BIND_EXPR_BODY (t)) == STATEMENT_LIST)
    {
      tree_stmt_iterator i = tsi_last (BIND_EXPR_BODY (t));
      t = ubsan_instrument_return (DECL_SOURCE_LOCATION (fndecl));
      tsi_link_after (&i, t, TSI_NEW_STMT);
    }
}

void
cp_genericize (tree fndecl)
{
  tree t;

  /* Fix up the types of parms passed by invisible reference.  */
  for (t = DECL_ARGUMENTS (fndecl); t; t = DECL_CHAIN (t))
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
      if (DECL_NAME (t))
	{
	  /* Adjust DECL_VALUE_EXPR of the original var.  */
	  tree outer = outer_curly_brace_block (current_function_decl);
	  tree var;

	  if (outer)
	    for (var = BLOCK_VARS (outer); var; var = DECL_CHAIN (var))
	      if (DECL_NAME (t) == DECL_NAME (var)
		  && DECL_HAS_VALUE_EXPR_P (var)
		  && DECL_VALUE_EXPR (var) == t)
		{
		  tree val = convert_from_reference (t);
		  SET_DECL_VALUE_EXPR (var, val);
		  break;
		}
	}
    }

  /* If we're a clone, the body is already GIMPLE.  */
  if (DECL_CLONED_FUNCTION_P (fndecl))
    return;

  /* Expand all the array notations here.  */
  if (flag_enable_cilkplus 
      && contains_array_notation_expr (DECL_SAVED_TREE (fndecl)))
    DECL_SAVED_TREE (fndecl) = 
      expand_array_notation_exprs (DECL_SAVED_TREE (fndecl));

  /* We do want to see every occurrence of the parms, so we can't just use
     walk_tree's hash functionality.  */
  cp_genericize_tree (&DECL_SAVED_TREE (fndecl));

  if (flag_sanitize & SANITIZE_RETURN)
    cp_ubsan_maybe_instrument_return (fndecl);

  /* Do everything else.  */
  c_genericize (fndecl);

  gcc_assert (bc_label[bc_break] == NULL);
  gcc_assert (bc_label[bc_continue] == NULL);
}

/* Build code to apply FN to each member of ARG1 and ARG2.  FN may be
   NULL if there is in fact nothing to do.  ARG2 may be null if FN
   actually only takes one argument.  */

static tree
cxx_omp_clause_apply_fn (tree fn, tree arg1, tree arg2)
{
  tree defparm, parm, t;
  int i = 0;
  int nargs;
  tree *argarray;

  if (fn == NULL)
    return NULL;

  nargs = list_length (DECL_ARGUMENTS (fn));
  argarray = XALLOCAVEC (tree, nargs);

  defparm = TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (fn)));
  if (arg2)
    defparm = TREE_CHAIN (defparm);

  if (TREE_CODE (TREE_TYPE (arg1)) == ARRAY_TYPE)
    {
      tree inner_type = TREE_TYPE (arg1);
      tree start1, end1, p1;
      tree start2 = NULL, p2 = NULL;
      tree ret = NULL, lab;

      start1 = arg1;
      start2 = arg2;
      do
	{
	  inner_type = TREE_TYPE (inner_type);
	  start1 = build4 (ARRAY_REF, inner_type, start1,
			   size_zero_node, NULL, NULL);
	  if (arg2)
	    start2 = build4 (ARRAY_REF, inner_type, start2,
			     size_zero_node, NULL, NULL);
	}
      while (TREE_CODE (inner_type) == ARRAY_TYPE);
      start1 = build_fold_addr_expr_loc (input_location, start1);
      if (arg2)
	start2 = build_fold_addr_expr_loc (input_location, start2);

      end1 = TYPE_SIZE_UNIT (TREE_TYPE (arg1));
      end1 = fold_build_pointer_plus (start1, end1);

      p1 = create_tmp_var (TREE_TYPE (start1), NULL);
      t = build2 (MODIFY_EXPR, TREE_TYPE (p1), p1, start1);
      append_to_statement_list (t, &ret);

      if (arg2)
	{
	  p2 = create_tmp_var (TREE_TYPE (start2), NULL);
	  t = build2 (MODIFY_EXPR, TREE_TYPE (p2), p2, start2);
	  append_to_statement_list (t, &ret);
	}

      lab = create_artificial_label (input_location);
      t = build1 (LABEL_EXPR, void_type_node, lab);
      append_to_statement_list (t, &ret);

      argarray[i++] = p1;
      if (arg2)
	argarray[i++] = p2;
      /* Handle default arguments.  */
      for (parm = defparm; parm && parm != void_list_node;
	   parm = TREE_CHAIN (parm), i++)
	argarray[i] = convert_default_arg (TREE_VALUE (parm),
					   TREE_PURPOSE (parm), fn, i,
					   tf_warning_or_error);
      t = build_call_a (fn, i, argarray);
      t = fold_convert (void_type_node, t);
      t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
      append_to_statement_list (t, &ret);

      t = fold_build_pointer_plus (p1, TYPE_SIZE_UNIT (inner_type));
      t = build2 (MODIFY_EXPR, TREE_TYPE (p1), p1, t);
      append_to_statement_list (t, &ret);

      if (arg2)
	{
	  t = fold_build_pointer_plus (p2, TYPE_SIZE_UNIT (inner_type));
	  t = build2 (MODIFY_EXPR, TREE_TYPE (p2), p2, t);
	  append_to_statement_list (t, &ret);
	}

      t = build2 (NE_EXPR, boolean_type_node, p1, end1);
      t = build3 (COND_EXPR, void_type_node, t, build_and_jump (&lab), NULL);
      append_to_statement_list (t, &ret);

      return ret;
    }
  else
    {
      argarray[i++] = build_fold_addr_expr_loc (input_location, arg1);
      if (arg2)
	argarray[i++] = build_fold_addr_expr_loc (input_location, arg2);
      /* Handle default arguments.  */
      for (parm = defparm; parm && parm != void_list_node;
	   parm = TREE_CHAIN (parm), i++)
	argarray[i] = convert_default_arg (TREE_VALUE (parm),
					   TREE_PURPOSE (parm),
					   fn, i, tf_warning_or_error);
      t = build_call_a (fn, i, argarray);
      t = fold_convert (void_type_node, t);
      return fold_build_cleanup_point_expr (TREE_TYPE (t), t);
    }
}

/* Return code to initialize DECL with its default constructor, or
   NULL if there's nothing to do.  */

tree
cxx_omp_clause_default_ctor (tree clause, tree decl, tree /*outer*/)
{
  tree info = CP_OMP_CLAUSE_INFO (clause);
  tree ret = NULL;

  if (info)
    ret = cxx_omp_clause_apply_fn (TREE_VEC_ELT (info, 0), decl, NULL);

  return ret;
}

/* Return code to initialize DST with a copy constructor from SRC.  */

tree
cxx_omp_clause_copy_ctor (tree clause, tree dst, tree src)
{
  tree info = CP_OMP_CLAUSE_INFO (clause);
  tree ret = NULL;

  if (info)
    ret = cxx_omp_clause_apply_fn (TREE_VEC_ELT (info, 0), dst, src);
  if (ret == NULL)
    ret = build2 (MODIFY_EXPR, TREE_TYPE (dst), dst, src);

  return ret;
}

/* Similarly, except use an assignment operator instead.  */

tree
cxx_omp_clause_assign_op (tree clause, tree dst, tree src)
{
  tree info = CP_OMP_CLAUSE_INFO (clause);
  tree ret = NULL;

  if (info)
    ret = cxx_omp_clause_apply_fn (TREE_VEC_ELT (info, 2), dst, src);
  if (ret == NULL)
    ret = build2 (MODIFY_EXPR, TREE_TYPE (dst), dst, src);

  return ret;
}

/* Return code to destroy DECL.  */

tree
cxx_omp_clause_dtor (tree clause, tree decl)
{
  tree info = CP_OMP_CLAUSE_INFO (clause);
  tree ret = NULL;

  if (info)
    ret = cxx_omp_clause_apply_fn (TREE_VEC_ELT (info, 1), decl, NULL);

  return ret;
}

/* True if OpenMP should privatize what this DECL points to rather
   than the DECL itself.  */

bool
cxx_omp_privatize_by_reference (const_tree decl)
{
  return (TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE
	  || is_invisiref_parm (decl));
}

/* Return true if DECL is const qualified var having no mutable member.  */
bool
cxx_omp_const_qual_no_mutable (tree decl)
{
  tree type = TREE_TYPE (decl);
  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      if (!is_invisiref_parm (decl))
	return false;
      type = TREE_TYPE (type);

      if (TREE_CODE (decl) == RESULT_DECL && DECL_NAME (decl))
	{
	  /* NVR doesn't preserve const qualification of the
	     variable's type.  */
	  tree outer = outer_curly_brace_block (current_function_decl);
	  tree var;

	  if (outer)
	    for (var = BLOCK_VARS (outer); var; var = DECL_CHAIN (var))
	      if (DECL_NAME (decl) == DECL_NAME (var)
		  && (TYPE_MAIN_VARIANT (type)
		      == TYPE_MAIN_VARIANT (TREE_TYPE (var))))
		{
		  if (TYPE_READONLY (TREE_TYPE (var)))
		    type = TREE_TYPE (var);
		  break;
		}
	}
    }

  if (type == error_mark_node)
    return false;

  /* Variables with const-qualified type having no mutable member
     are predetermined shared.  */
  if (TYPE_READONLY (type) && !cp_has_mutable_p (type))
    return true;

  return false;
}

/* True if OpenMP sharing attribute of DECL is predetermined.  */

enum omp_clause_default_kind
cxx_omp_predetermined_sharing (tree decl)
{
  /* Static data members are predetermined shared.  */
  if (TREE_STATIC (decl))
    {
      tree ctx = CP_DECL_CONTEXT (decl);
      if (TYPE_P (ctx) && MAYBE_CLASS_TYPE_P (ctx))
	return OMP_CLAUSE_DEFAULT_SHARED;
    }

  /* Const qualified vars having no mutable member are predetermined
     shared.  */
  if (cxx_omp_const_qual_no_mutable (decl))
    return OMP_CLAUSE_DEFAULT_SHARED;

  return OMP_CLAUSE_DEFAULT_UNSPECIFIED;
}

/* Finalize an implicitly determined clause.  */

void
cxx_omp_finish_clause (tree c)
{
  tree decl, inner_type;
  bool make_shared = false;

  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_FIRSTPRIVATE)
    return;

  decl = OMP_CLAUSE_DECL (c);
  decl = require_complete_type (decl);
  inner_type = TREE_TYPE (decl);
  if (decl == error_mark_node)
    make_shared = true;
  else if (TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE)
    {
      if (is_invisiref_parm (decl))
	inner_type = TREE_TYPE (inner_type);
      else
	{
	  error ("%qE implicitly determined as %<firstprivate%> has reference type",
		 decl);
	  make_shared = true;
	}
    }

  /* We're interested in the base element, not arrays.  */
  while (TREE_CODE (inner_type) == ARRAY_TYPE)
    inner_type = TREE_TYPE (inner_type);

  /* Check for special function availability by building a call to one.
     Save the results, because later we won't be in the right context
     for making these queries.  */
  if (!make_shared
      && CLASS_TYPE_P (inner_type)
      && cxx_omp_create_clause_info (c, inner_type, false, true, false, true))
    make_shared = true;

  if (make_shared)
    OMP_CLAUSE_CODE (c) = OMP_CLAUSE_SHARED;
}
