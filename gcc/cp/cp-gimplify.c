/* C++-specific tree lowering bits; see also c-gimplify.c and tree-gimple.c.

   Copyright (C) 2002-2017 Free Software Foundation, Inc.
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
#include "target.h"
#include "basic-block.h"
#include "cp-tree.h"
#include "gimple.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "gimplify.h"
#include "c-family/c-ubsan.h"
#include "cilk.h"
#include "cp-cilkplus.h"

/* Forward declarations.  */

static tree cp_genericize_r (tree *, int *, void *);
static tree cp_fold_r (tree *, int *, void *);
static void cp_genericize_tree (tree*, bool);
static tree cp_fold (tree);

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
  if (bc == bc_break)
    LABEL_DECL_BREAK (label) = true;
  else
    LABEL_DECL_CONTINUE (label) = true;
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

/* This function is a wrapper for cilk_gimplify_call_params_in_spawned_fn.
   *EXPR_P can be a CALL_EXPR, INIT_EXPR, MODIFY_EXPR, AGGR_INIT_EXPR or
   TARGET_EXPR.  *PRE_P and *POST_P are gimple sequences from the caller
   of gimplify_cilk_spawn.  */

static void
cilk_cp_gimplify_call_params_in_spawned_fn (tree *expr_p, gimple_seq *pre_p,
					    gimple_seq *post_p)
{
  int ii = 0;

  cilk_gimplify_call_params_in_spawned_fn (expr_p, pre_p);
  if (TREE_CODE (*expr_p) == AGGR_INIT_EXPR)
    for (ii = 0; ii < aggr_init_expr_nargs (*expr_p); ii++)
      gimplify_expr (&AGGR_INIT_EXPR_ARG (*expr_p, ii), pre_p, post_p,
		     is_gimple_reg, fb_rvalue);
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
  if (!EXPR_HAS_LOCATION (stmt))
    protected_set_expr_location (stmt, locus);
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
  tree exit = NULL;
  tree stmt_list = NULL;

  blab = begin_bc_block (bc_break, start_locus);
  clab = begin_bc_block (bc_continue, start_locus);

  protected_set_expr_location (incr, start_locus);

  cp_walk_tree (&cond, cp_genericize_r, data, NULL);
  cp_walk_tree (&body, cp_genericize_r, data, NULL);
  cp_walk_tree (&incr, cp_genericize_r, data, NULL);
  *walk_subtrees = 0;

  if (cond && TREE_CODE (cond) != INTEGER_CST)
    {
      /* If COND is constant, don't bother building an exit.  If it's false,
	 we won't build a loop.  If it's true, any exits are in the body.  */
      location_t cloc = EXPR_LOC_OR_LOC (cond, start_locus);
      exit = build1_loc (cloc, GOTO_EXPR, void_type_node,
			 get_bc_label (bc_break));
      exit = fold_build3_loc (cloc, COND_EXPR, void_type_node, cond,
			      build_empty_stmt (cloc), exit);
    }

  if (exit && cond_is_first)
    append_to_statement_list (exit, &stmt_list);
  append_to_statement_list (body, &stmt_list);
  finish_bc_block (&stmt_list, bc_continue, clab);
  append_to_statement_list (incr, &stmt_list);
  if (exit && !cond_is_first)
    append_to_statement_list (exit, &stmt_list);

  if (!stmt_list)
    stmt_list = build_empty_stmt (start_locus);

  tree loop;
  if (cond && integer_zerop (cond))
    {
      if (cond_is_first)
	loop = fold_build3_loc (start_locus, COND_EXPR,
				void_type_node, cond, stmt_list,
				build_empty_stmt (start_locus));
      else
	loop = stmt_list;
    }
  else
    {
      location_t loc = start_locus;
      if (!cond || integer_nonzerop (cond))
	loc = EXPR_LOCATION (expr_first (body));
      if (loc == UNKNOWN_LOCATION)
	loc = start_locus;
      loop = build1_loc (loc, LOOP_EXPR, void_type_node, stmt_list);
    }

  stmt_list = NULL;
  append_to_statement_list (loop, &stmt_list);
  finish_bc_block (&stmt_list, bc_break, blab);
  if (!stmt_list)
    stmt_list = build_empty_stmt (start_locus);

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
  if (expr == NULL_TREE)
    expr = loop;
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
  append_to_statement_list_force (pred, &stmt_list);
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
  if (TREE_CODE (stmt) != OMP_TASKLOOP)
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

      /* Handle aggregate NSDMI.  */
      replace_placeholders (sub, to);

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
  gimple *mnt;

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

/* Return TRUE if an operand (OP) of a given TYPE being copied is
   really just an empty class copy.

   Check that the operand has a simple form so that TARGET_EXPRs and
   non-empty CONSTRUCTORs get reduced properly, and we leave the
   return slot optimization alone because it isn't a copy.  */

static bool
simple_empty_class_p (tree type, tree op)
{
  return
    ((TREE_CODE (op) == COMPOUND_EXPR
      && simple_empty_class_p (type, TREE_OPERAND (op, 1)))
     || TREE_CODE (op) == EMPTY_CLASS_EXPR
     || is_gimple_lvalue (op)
     || INDIRECT_REF_P (op)
     || (TREE_CODE (op) == CONSTRUCTOR
	 && CONSTRUCTOR_NELTS (op) == 0
	 && !TREE_CLOBBER_P (op))
     || (TREE_CODE (op) == CALL_EXPR
	 && !CALL_EXPR_RETURN_SLOT_OPT (op)))
    && is_really_empty_class (type);
}

/* Returns true if evaluating E as an lvalue has side-effects;
   specifically, a volatile lvalue has TREE_SIDE_EFFECTS, but it doesn't really
   have side-effects until there is a read or write through it.  */

static bool
lvalue_has_side_effects (tree e)
{
  if (!TREE_SIDE_EFFECTS (e))
    return false;
  while (handled_component_p (e))
    {
      if (TREE_CODE (e) == ARRAY_REF
	  && TREE_SIDE_EFFECTS (TREE_OPERAND (e, 1)))
	return true;
      e = TREE_OPERAND (e, 0);
    }
  if (DECL_P (e))
    /* Just naming a variable has no side-effects.  */
    return false;
  else if (INDIRECT_REF_P (e))
    /* Similarly, indirection has no side-effects.  */
    return TREE_SIDE_EFFECTS (TREE_OPERAND (e, 0));
  else
    /* For anything else, trust TREE_SIDE_EFFECTS.  */
    return TREE_SIDE_EFFECTS (e);
}

/* Do C++-specific gimplification.  Args are as for gimplify_expr.  */

int
cp_gimplify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  int saved_stmts_are_full_exprs_p = 0;
  location_t loc = EXPR_LOC_OR_LOC (*expr_p, input_location);
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
	hash_set<tree> pset;
	cp_walk_tree (expr_p, cp_fold_r, &pset, NULL);
	cp_genericize_tree (expr_p, false);
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
      if (fn_contains_cilk_spawn_p (cfun))
	{
	  if (cilk_cp_detect_spawn_and_unwrap (expr_p))
	    {
	      cilk_cp_gimplify_call_params_in_spawned_fn (expr_p,
							  pre_p, post_p);
	      return (enum gimplify_status) gimplify_cilk_spawn (expr_p);
	    }
	  if (seen_error () && contains_cilk_spawn_stmt (*expr_p))
	    return GS_ERROR;
	}

      cp_gimplify_init_expr (expr_p);
      if (TREE_CODE (*expr_p) != INIT_EXPR)
	return GS_OK;
      /* Fall through.  */
    case MODIFY_EXPR:
    modify_expr_case:
      {
	if (fn_contains_cilk_spawn_p (cfun)
	    && cilk_cp_detect_spawn_and_unwrap (expr_p)
	    && !seen_error ())
	  {
	    cilk_cp_gimplify_call_params_in_spawned_fn (expr_p, pre_p, post_p);
	    return (enum gimplify_status) gimplify_cilk_spawn (expr_p);
	  }
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

	else if (simple_empty_class_p (TREE_TYPE (op0), op1))
	  {
	    /* Remove any copies of empty classes.  Also drop volatile
	       variables on the RHS to avoid infinite recursion from
	       gimplify_expr trying to load the value.  */
	    if (TREE_SIDE_EFFECTS (op1))
	      {
		if (TREE_THIS_VOLATILE (op1)
		    && (REFERENCE_CLASS_P (op1) || DECL_P (op1)))
		  op1 = build_fold_addr_expr (op1);

		gimplify_and_add (op1, pre_p);
	      }
	    gimplify_expr (&TREE_OPERAND (*expr_p, 0), pre_p, post_p,
			   is_gimple_lvalue, fb_lvalue);
	    *expr_p = TREE_OPERAND (*expr_p, 0);
	  }
	/* P0145 says that the RHS is sequenced before the LHS.
	   gimplify_modify_expr gimplifies the RHS before the LHS, but that
	   isn't quite strong enough in two cases:

	   1) gimplify.c wants to leave a CALL_EXPR on the RHS, which would
	   mean it's evaluated after the LHS.

	   2) the value calculation of the RHS is also sequenced before the
	   LHS, so for scalar assignment we need to preevaluate if the
	   RHS could be affected by LHS side-effects even if it has no
	   side-effects of its own.  We don't need this for classes because
	   class assignment takes its RHS by reference.  */
       else if (flag_strong_eval_order > 1
                && TREE_CODE (*expr_p) == MODIFY_EXPR
                && lvalue_has_side_effects (op0)
		&& (TREE_CODE (op1) == CALL_EXPR
		    || (SCALAR_TYPE_P (TREE_TYPE (op1))
			&& !TREE_CONSTANT (op1))))
	 TREE_OPERAND (*expr_p, 1) = get_formal_tmp_var (op1, pre_p);
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
    case OMP_TASKLOOP:
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
      gcc_assert(fn_contains_cilk_spawn_p (cfun)
		 && cilk_cp_detect_spawn_and_unwrap (expr_p));

      if (!seen_error ())
	{
	  cilk_cp_gimplify_call_params_in_spawned_fn (expr_p, pre_p, post_p);
	  return (enum gimplify_status) gimplify_cilk_spawn (expr_p);
	}
      return GS_ERROR;

    case CALL_EXPR:
      if (fn_contains_cilk_spawn_p (cfun)
	  && cilk_cp_detect_spawn_and_unwrap (expr_p)
	  && !seen_error ())
	{
	  cilk_cp_gimplify_call_params_in_spawned_fn (expr_p, pre_p, post_p);
	  return (enum gimplify_status) gimplify_cilk_spawn (expr_p);
	}
      ret = GS_OK;
      if (!CALL_EXPR_FN (*expr_p))
	/* Internal function call.  */;
      else if (CALL_EXPR_REVERSE_ARGS (*expr_p))
	{
	  /* This is a call to a (compound) assignment operator that used
	     the operator syntax; gimplify the RHS first.  */
	  gcc_assert (call_expr_nargs (*expr_p) == 2);
	  gcc_assert (!CALL_EXPR_ORDERED_ARGS (*expr_p));
	  enum gimplify_status t
	    = gimplify_arg (&CALL_EXPR_ARG (*expr_p, 1), pre_p, loc);
	  if (t == GS_ERROR)
	    ret = GS_ERROR;
	}
      else if (CALL_EXPR_ORDERED_ARGS (*expr_p))
	{
	  /* Leave the last argument for gimplify_call_expr, to avoid problems
	     with __builtin_va_arg_pack().  */
	  int nargs = call_expr_nargs (*expr_p) - 1;
	  for (int i = 0; i < nargs; ++i)
	    {
	      enum gimplify_status t
		= gimplify_arg (&CALL_EXPR_ARG (*expr_p, i), pre_p, loc);
	      if (t == GS_ERROR)
		ret = GS_ERROR;
	    }
	}
      else if (flag_strong_eval_order
	       && !CALL_EXPR_OPERATOR_SYNTAX (*expr_p))
	{
	  /* If flag_strong_eval_order, evaluate the object argument first.  */
	  tree fntype = TREE_TYPE (CALL_EXPR_FN (*expr_p));
	  if (POINTER_TYPE_P (fntype))
	    fntype = TREE_TYPE (fntype);
	  if (TREE_CODE (fntype) == METHOD_TYPE)
	    {
	      enum gimplify_status t
		= gimplify_arg (&CALL_EXPR_ARG (*expr_p, 0), pre_p, loc);
	      if (t == GS_ERROR)
		ret = GS_ERROR;
	    }
	}
      break;

    case RETURN_EXPR:
      if (TREE_OPERAND (*expr_p, 0)
	  && (TREE_CODE (TREE_OPERAND (*expr_p, 0)) == INIT_EXPR
	      || TREE_CODE (TREE_OPERAND (*expr_p, 0)) == MODIFY_EXPR))
	{
	  expr_p = &TREE_OPERAND (*expr_p, 0);
	  code = TREE_CODE (*expr_p);
	  /* Avoid going through the INIT_EXPR case, which can
	     degrade INIT_EXPRs into AGGR_INIT_EXPRs.  */
	  goto modify_expr_case;
	}
      /* Fall through.  */

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

bool
cxx_int_tree_map_hasher::equal (cxx_int_tree_map *a, cxx_int_tree_map *b)
{
  return (a->uid == b->uid);
}

/* Hash a UID in a cxx_int_tree_map.  */

unsigned int
cxx_int_tree_map_hasher::hash (cxx_int_tree_map *item)
{
  return item->uid;
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
  if (VAR_P (decl) && CP_DECL_THREAD_LOCAL_P (decl))
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
  hash_set<tree> *p_set;
  vec<tree> bind_expr_stack;
  struct cp_genericize_omp_taskreg *omp_ctx;
  tree try_block;
  bool no_sanitize_p;
  bool handle_invisiref_parm_p;
};

/* Perform any pre-gimplification folding of C++ front end trees to
   GENERIC.
   Note:  The folding of none-omp cases is something to move into
     the middle-end.  As for now we have most foldings only on GENERIC
     in fold-const, we need to perform this before transformation to
     GIMPLE-form.  */

static tree
cp_fold_r (tree *stmt_p, int *walk_subtrees, void *data)
{
  tree stmt;
  enum tree_code code;

  *stmt_p = stmt = cp_fold (*stmt_p);

  if (((hash_set<tree> *) data)->add (stmt))
    {
      /* Don't walk subtrees of stmts we've already walked once, otherwise
	 we can have exponential complexity with e.g. lots of nested
	 SAVE_EXPRs or TARGET_EXPRs.  cp_fold uses a cache and will return
	 always the same tree, which the first time cp_fold_r has been
	 called on it had the subtrees walked.  */
      *walk_subtrees = 0;
      return NULL;
    }

  code = TREE_CODE (stmt);
  if (code == OMP_FOR || code == OMP_SIMD || code == OMP_DISTRIBUTE
      || code == OMP_TASKLOOP || code == CILK_FOR || code == CILK_SIMD
      || code == OACC_LOOP)
    {
      tree x;
      int i, n;

      cp_walk_tree (&OMP_FOR_BODY (stmt), cp_fold_r, data, NULL);
      cp_walk_tree (&OMP_FOR_CLAUSES (stmt), cp_fold_r, data, NULL);
      cp_walk_tree (&OMP_FOR_INIT (stmt), cp_fold_r, data, NULL);
      x = OMP_FOR_COND (stmt);
      if (x && TREE_CODE_CLASS (TREE_CODE (x)) == tcc_comparison)
	{
	  cp_walk_tree (&TREE_OPERAND (x, 0), cp_fold_r, data, NULL);
	  cp_walk_tree (&TREE_OPERAND (x, 1), cp_fold_r, data, NULL);
	}
      else if (x && TREE_CODE (x) == TREE_VEC)
	{
	  n = TREE_VEC_LENGTH (x);
	  for (i = 0; i < n; i++)
	    {
	      tree o = TREE_VEC_ELT (x, i);
	      if (o && TREE_CODE_CLASS (TREE_CODE (o)) == tcc_comparison)
		cp_walk_tree (&TREE_OPERAND (o, 1), cp_fold_r, data, NULL);
	    }
	}
      x = OMP_FOR_INCR (stmt);
      if (x && TREE_CODE (x) == TREE_VEC)
	{
	  n = TREE_VEC_LENGTH (x);
	  for (i = 0; i < n; i++)
	    {
	      tree o = TREE_VEC_ELT (x, i);
	      if (o && TREE_CODE (o) == MODIFY_EXPR)
		o = TREE_OPERAND (o, 1);
	      if (o && (TREE_CODE (o) == PLUS_EXPR || TREE_CODE (o) == MINUS_EXPR
			|| TREE_CODE (o) == POINTER_PLUS_EXPR))
		{
		  cp_walk_tree (&TREE_OPERAND (o, 0), cp_fold_r, data, NULL);
		  cp_walk_tree (&TREE_OPERAND (o, 1), cp_fold_r, data, NULL);
		}
	    }
	}
      cp_walk_tree (&OMP_FOR_PRE_BODY (stmt), cp_fold_r, data, NULL);
      *walk_subtrees = 0;
    }

  return NULL;
}

/* Fold ALL the trees!  FIXME we should be able to remove this, but
   apparently that still causes optimization regressions.  */

void
cp_fold_function (tree fndecl)
{
  hash_set<tree> pset;
  cp_walk_tree (&DECL_SAVED_TREE (fndecl), cp_fold_r, &pset, NULL);
}

/* Perform any pre-gimplification lowering of C++ front end trees to
   GENERIC.  */

static tree
cp_genericize_r (tree *stmt_p, int *walk_subtrees, void *data)
{
  tree stmt = *stmt_p;
  struct cp_genericize_data *wtd = (struct cp_genericize_data *) data;
  hash_set<tree> *p_set = wtd->p_set;

  /* If in an OpenMP context, note var uses.  */
  if (__builtin_expect (wtd->omp_ctx != NULL, 0)
      && (VAR_P (stmt)
	  || TREE_CODE (stmt) == PARM_DECL
	  || TREE_CODE (stmt) == RESULT_DECL)
      && omp_var_to_track (stmt))
    omp_cxx_notice_variable (wtd->omp_ctx, stmt);

  /* Dereference invisible reference parms.  */
  if (wtd->handle_invisiref_parm_p && is_invisiref_parm (stmt))
    {
      *stmt_p = convert_from_reference (stmt);
      p_set->add (*stmt_p);
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
      h = cp_function_chain->extern_decl_map->find_with_hash (&in, in.uid);
      if (h)
	{
	  *stmt_p = h->to;
	  *walk_subtrees = 0;
	  return NULL;
	}
    }

  if (TREE_CODE (stmt) == INTEGER_CST
      && TREE_CODE (TREE_TYPE (stmt)) == REFERENCE_TYPE
      && (flag_sanitize & (SANITIZE_NULL | SANITIZE_ALIGNMENT))
      && !wtd->no_sanitize_p)
    {
      ubsan_maybe_instrument_reference (stmt_p);
      if (*stmt_p != stmt)
	{
	  *walk_subtrees = 0;
	  return NULL_TREE;
	}
    }

  /* Other than invisiref parms, don't walk the same tree twice.  */
  if (p_set->contains (stmt))
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
      *stmt_p = fold_convert (TREE_TYPE (stmt), TREE_OPERAND (stmt, 0));
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
      if (flag_sanitize
	  & (SANITIZE_NULL | SANITIZE_ALIGNMENT | SANITIZE_VPTR))
	{
	  /* The point here is to not sanitize static initializers.  */
	  bool no_sanitize_p = wtd->no_sanitize_p;
	  wtd->no_sanitize_p = true;
	  for (tree decl = BIND_EXPR_VARS (stmt);
	       decl;
	       decl = DECL_CHAIN (decl))
	    if (VAR_P (decl)
		&& TREE_STATIC (decl)
		&& DECL_INITIAL (decl))
	      cp_walk_tree (&DECL_INITIAL (decl), cp_genericize_r, data, NULL);
	  wtd->no_sanitize_p = no_sanitize_p;
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
  else if (TREE_CODE (stmt) == DECL_EXPR)
    {
      tree d = DECL_EXPR_DECL (stmt);
      if (VAR_P (d))
	gcc_assert (CP_DECL_THREAD_LOCAL_P (d) == DECL_THREAD_LOCAL_P (d));
    }
  else if (TREE_CODE (stmt) == OMP_PARALLEL
	   || TREE_CODE (stmt) == OMP_TASK
	   || TREE_CODE (stmt) == OMP_TASKLOOP)
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
      if (TREE_CODE (stmt) == OMP_TASKLOOP)
	genericize_omp_for_stmt (stmt_p, walk_subtrees, data);
      else
	cp_walk_tree (&OMP_BODY (stmt), cp_genericize_r, data, NULL);
      wtd->omp_ctx = omp_ctx.outer;
      splay_tree_delete (omp_ctx.variables);
    }
  else if (TREE_CODE (stmt) == TRY_BLOCK)
    {
      *walk_subtrees = 0;
      tree try_block = wtd->try_block;
      wtd->try_block = stmt;
      cp_walk_tree (&TRY_STMTS (stmt), cp_genericize_r, data, NULL);
      wtd->try_block = try_block;
      cp_walk_tree (&TRY_HANDLERS (stmt), cp_genericize_r, data, NULL);
    }
  else if (TREE_CODE (stmt) == MUST_NOT_THROW_EXPR)
    {
      /* MUST_NOT_THROW_COND might be something else with TM.  */
      if (MUST_NOT_THROW_COND (stmt) == NULL_TREE)
	{
	  *walk_subtrees = 0;
	  tree try_block = wtd->try_block;
	  wtd->try_block = stmt;
	  cp_walk_tree (&TREE_OPERAND (stmt, 0), cp_genericize_r, data, NULL);
	  wtd->try_block = try_block;
	}
    }
  else if (TREE_CODE (stmt) == THROW_EXPR)
    {
      location_t loc = location_of (stmt);
      if (TREE_NO_WARNING (stmt))
	/* Never mind.  */;
      else if (wtd->try_block)
	{
	  if (TREE_CODE (wtd->try_block) == MUST_NOT_THROW_EXPR
	      && warning_at (loc, OPT_Wterminate,
			     "throw will always call terminate()")
	      && cxx_dialect >= cxx11
	      && DECL_DESTRUCTOR_P (current_function_decl))
	    inform (loc, "in C++11 destructors default to noexcept");
	}
      else
	{
	  if (warn_cxx11_compat && cxx_dialect < cxx11
	      && DECL_DESTRUCTOR_P (current_function_decl)
	      && (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (current_function_decl))
		  == NULL_TREE)
	      && (get_defaulted_eh_spec (current_function_decl)
		  == empty_except_spec))
	    warning_at (loc, OPT_Wc__11_compat,
			"in C++11 this throw will terminate because "
			"destructors default to noexcept");
	}
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
  else if (TREE_CODE (stmt) == PTRMEM_CST)
    {
      /* By the time we get here we're handing off to the back end, so we don't
	 need or want to preserve PTRMEM_CST anymore.  */
      *stmt_p = cplus_expand_constant (stmt);
      *walk_subtrees = 0;
    }
  else if (TREE_CODE (stmt) == MEM_REF)
    {
      /* For MEM_REF, make sure not to sanitize the second operand even
         if it has reference type.  It is just an offset with a type
	 holding other information.  There is no other processing we
	 need to do for INTEGER_CSTs, so just ignore the second argument
	 unconditionally.  */
      cp_walk_tree (&TREE_OPERAND (stmt, 0), cp_genericize_r, data, NULL);
      *walk_subtrees = 0;
    }
  else if ((flag_sanitize
	    & (SANITIZE_NULL | SANITIZE_ALIGNMENT | SANITIZE_VPTR))
	   && !wtd->no_sanitize_p)
    {
      if ((flag_sanitize & (SANITIZE_NULL | SANITIZE_ALIGNMENT))
	  && TREE_CODE (stmt) == NOP_EXPR
	  && TREE_CODE (TREE_TYPE (stmt)) == REFERENCE_TYPE)
	ubsan_maybe_instrument_reference (stmt_p);
      else if (TREE_CODE (stmt) == CALL_EXPR)
	{
	  tree fn = CALL_EXPR_FN (stmt);
	  if (fn != NULL_TREE
	      && !error_operand_p (fn)
	      && POINTER_TYPE_P (TREE_TYPE (fn))
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (fn))) == METHOD_TYPE)
	    {
	      bool is_ctor
		= TREE_CODE (fn) == ADDR_EXPR
		  && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
		  && DECL_CONSTRUCTOR_P (TREE_OPERAND (fn, 0));
	      if (flag_sanitize & (SANITIZE_NULL | SANITIZE_ALIGNMENT))
		ubsan_maybe_instrument_member_call (stmt, is_ctor);
	      if ((flag_sanitize & SANITIZE_VPTR) && !is_ctor)
		cp_ubsan_maybe_instrument_member_call (stmt);
	    }
	}
    }

  p_set->add (*stmt_p);

  return NULL;
}

/* Lower C++ front end trees to GENERIC in T_P.  */

static void
cp_genericize_tree (tree* t_p, bool handle_invisiref_parm_p)
{
  struct cp_genericize_data wtd;

  wtd.p_set = new hash_set<tree>;
  wtd.bind_expr_stack.create (0);
  wtd.omp_ctx = NULL;
  wtd.try_block = NULL_TREE;
  wtd.no_sanitize_p = false;
  wtd.handle_invisiref_parm_p = handle_invisiref_parm_p;
  cp_walk_tree (t_p, cp_genericize_r, &wtd, NULL);
  delete wtd.p_set;
  wtd.bind_expr_stack.release ();
  if (flag_sanitize & SANITIZE_VPTR)
    cp_ubsan_instrument_member_accesses (t_p);
}

/* If a function that should end with a return in non-void
   function doesn't obviously end with return, add ubsan
   instrumentation code to verify it at runtime.  */

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
  tree *p = &DECL_SAVED_TREE (fndecl);
  if (TREE_CODE (*p) == BIND_EXPR)
    p = &BIND_EXPR_BODY (*p);
  t = ubsan_instrument_return (DECL_SOURCE_LOCATION (fndecl));
  append_to_statement_list (t, p);
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
	      if (VAR_P (var)
		  && DECL_NAME (t) == DECL_NAME (var)
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

  /* Allow cp_genericize calls to be nested.  */
  tree save_bc_label[2];
  save_bc_label[bc_break] = bc_label[bc_break];
  save_bc_label[bc_continue] = bc_label[bc_continue];
  bc_label[bc_break] = NULL_TREE;
  bc_label[bc_continue] = NULL_TREE;

  /* Expand all the array notations here.  */
  if (flag_cilkplus 
      && contains_array_notation_expr (DECL_SAVED_TREE (fndecl)))
    DECL_SAVED_TREE (fndecl)
      = expand_array_notation_exprs (DECL_SAVED_TREE (fndecl));

  /* We do want to see every occurrence of the parms, so we can't just use
     walk_tree's hash functionality.  */
  cp_genericize_tree (&DECL_SAVED_TREE (fndecl), true);

  if (flag_sanitize & SANITIZE_RETURN
      && do_ubsan_in_current_function ())
    cp_ubsan_maybe_instrument_return (fndecl);

  /* Do everything else.  */
  c_genericize (fndecl);

  gcc_assert (bc_label[bc_break] == NULL);
  gcc_assert (bc_label[bc_continue] == NULL);
  bc_label[bc_break] = save_bc_label[bc_break];
  bc_label[bc_continue] = save_bc_label[bc_continue];
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

      p1 = create_tmp_var (TREE_TYPE (start1));
      t = build2 (MODIFY_EXPR, TREE_TYPE (p1), p1, start1);
      append_to_statement_list (t, &ret);

      if (arg2)
	{
	  p2 = create_tmp_var (TREE_TYPE (start2));
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
	      if (VAR_P (var)
		  && DECL_NAME (decl) == DECL_NAME (var)
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
cxx_omp_finish_clause (tree c, gimple_seq *)
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
    inner_type = TREE_TYPE (inner_type);

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
    {
      OMP_CLAUSE_CODE (c) = OMP_CLAUSE_SHARED;
      OMP_CLAUSE_SHARED_FIRSTPRIVATE (c) = 0;
      OMP_CLAUSE_SHARED_READONLY (c) = 0;
    }
}

/* Return true if DECL's DECL_VALUE_EXPR (if any) should be
   disregarded in OpenMP construct, because it is going to be
   remapped during OpenMP lowering.  SHARED is true if DECL
   is going to be shared, false if it is going to be privatized.  */

bool
cxx_omp_disregard_value_expr (tree decl, bool shared)
{
  return !shared
	 && VAR_P (decl)
	 && DECL_HAS_VALUE_EXPR_P (decl)
	 && DECL_ARTIFICIAL (decl)
	 && DECL_LANG_SPECIFIC (decl)
	 && DECL_OMP_PRIVATIZED_MEMBER (decl);
}

/* Perform folding on expression X.  */

tree
cp_fully_fold (tree x)
{
  if (processing_template_decl)
    return x;
  /* FIXME cp_fold ought to be a superset of maybe_constant_value so we don't
     have to call both.  */
  if (cxx_dialect >= cxx11)
    x = maybe_constant_value (x);
  return cp_fold (x);
}

/* Fold expression X which is used as an rvalue if RVAL is true.  */

static tree
cp_fold_maybe_rvalue (tree x, bool rval)
{
  while (true)
    {
      x = cp_fold (x);
      if (rval && DECL_P (x)
	  && TREE_CODE (TREE_TYPE (x)) != REFERENCE_TYPE)
	{
	  tree v = decl_constant_value (x);
	  if (v != x && v != error_mark_node)
	    {
	      x = v;
	      continue;
	    }
	}
      break;
    }
  return x;
}

/* Fold expression X which is used as an rvalue.  */

static tree
cp_fold_rvalue (tree x)
{
  return cp_fold_maybe_rvalue (x, true);
}

/* c-common interface to cp_fold.  If IN_INIT, this is in a static initializer
   and certain changes are made to the folding done.  Or should be (FIXME).  We
   never touch maybe_const, as it is only used for the C front-end
   C_MAYBE_CONST_EXPR.  */

tree
c_fully_fold (tree x, bool /*in_init*/, bool */*maybe_const*/)
{
  /* c_fully_fold is only used on rvalues, and we need to fold CONST_DECL to
     INTEGER_CST.  */
  return cp_fold_rvalue (x);
}

static GTY((deletable)) hash_map<tree, tree> *fold_cache;

/* Dispose of the whole FOLD_CACHE.  */

void
clear_fold_cache (void)
{
  if (fold_cache != NULL)
    fold_cache->empty ();
}

/*  This function tries to fold an expression X.
    To avoid combinatorial explosion, folding results are kept in fold_cache.
    If we are processing a template or X is invalid, we don't fold at all.
    For performance reasons we don't cache expressions representing a
    declaration or constant.
    Function returns X or its folded variant.  */

static tree
cp_fold (tree x)
{
  tree op0, op1, op2, op3;
  tree org_x = x, r = NULL_TREE;
  enum tree_code code;
  location_t loc;
  bool rval_ops = true;

  if (!x || x == error_mark_node)
    return x;

  if (processing_template_decl
      || (EXPR_P (x) && (!TREE_TYPE (x) || TREE_TYPE (x) == error_mark_node)))
    return x;

  /* Don't bother to cache DECLs or constants.  */
  if (DECL_P (x) || CONSTANT_CLASS_P (x))
    return x;

  if (fold_cache == NULL)
    fold_cache = hash_map<tree, tree>::create_ggc (101);

  if (tree *cached = fold_cache->get (x))
    return *cached;

  code = TREE_CODE (x);
  switch (code)
    {
    case CLEANUP_POINT_EXPR:
      /* Strip CLEANUP_POINT_EXPR if the expression doesn't have side
	 effects.  */
      r = cp_fold_rvalue (TREE_OPERAND (x, 0));
      if (!TREE_SIDE_EFFECTS (r))
	x = r;
      break;

    case SIZEOF_EXPR:
      x = fold_sizeof_expr (x);
      break;

    case VIEW_CONVERT_EXPR:
      rval_ops = false;
      /* FALLTHRU */
    case CONVERT_EXPR:
    case NOP_EXPR:
    case NON_LVALUE_EXPR:

      if (VOID_TYPE_P (TREE_TYPE (x)))
	return x;

      loc = EXPR_LOCATION (x);
      op0 = cp_fold_maybe_rvalue (TREE_OPERAND (x, 0), rval_ops);

      if (code == CONVERT_EXPR
	  && SCALAR_TYPE_P (TREE_TYPE (x))
	  && op0 != void_node)
	/* During parsing we used convert_to_*_nofold; re-convert now using the
	   folding variants, since fold() doesn't do those transformations.  */
	x = fold (convert (TREE_TYPE (x), op0));
      else if (op0 != TREE_OPERAND (x, 0))
	{
	  if (op0 == error_mark_node)
	    x = error_mark_node;
	  else
	    x = fold_build1_loc (loc, code, TREE_TYPE (x), op0);
	}
      else
	x = fold (x);

      /* Conversion of an out-of-range value has implementation-defined
	 behavior; the language considers it different from arithmetic
	 overflow, which is undefined.  */
      if (TREE_CODE (op0) == INTEGER_CST
	  && TREE_OVERFLOW_P (x) && !TREE_OVERFLOW_P (op0))
	TREE_OVERFLOW (x) = false;

      break;

    case INDIRECT_REF:
      /* We don't need the decltype(auto) obfuscation anymore.  */
      if (REF_PARENTHESIZED_P (x))
	{
	  tree p = maybe_undo_parenthesized_ref (x);
	  return cp_fold (p);
	}
      goto unary;

    case ADDR_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      rval_ops = false;
      /* FALLTHRU */
    case CONJ_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
    unary:

      loc = EXPR_LOCATION (x);
      op0 = cp_fold_maybe_rvalue (TREE_OPERAND (x, 0), rval_ops);

      if (op0 != TREE_OPERAND (x, 0))
	{
	  if (op0 == error_mark_node)
	    x = error_mark_node;
	  else
	    {
	      x = fold_build1_loc (loc, code, TREE_TYPE (x), op0);
	      if (code == INDIRECT_REF
		  && (INDIRECT_REF_P (x) || TREE_CODE (x) == MEM_REF))
		{
		  TREE_READONLY (x) = TREE_READONLY (org_x);
		  TREE_SIDE_EFFECTS (x) = TREE_SIDE_EFFECTS (org_x);
		  TREE_THIS_VOLATILE (x) = TREE_THIS_VOLATILE (org_x);
		}
	    }
	}
      else
	x = fold (x);

      gcc_assert (TREE_CODE (x) != COND_EXPR
		  || !VOID_TYPE_P (TREE_TYPE (TREE_OPERAND (x, 0))));
      break;

    case UNARY_PLUS_EXPR:
      op0 = cp_fold_rvalue (TREE_OPERAND (x, 0));
      if (op0 == error_mark_node)
	x = error_mark_node;
      else
	x = fold_convert (TREE_TYPE (x), op0);
      break;

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case INIT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case COMPOUND_EXPR:
    case MODIFY_EXPR:
      rval_ops = false;
      /* FALLTHRU */
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR: case LE_EXPR:
    case GT_EXPR: case GE_EXPR:
    case EQ_EXPR: case NE_EXPR:
    case UNORDERED_EXPR: case ORDERED_EXPR:
    case UNLT_EXPR: case UNLE_EXPR:
    case UNGT_EXPR: case UNGE_EXPR:
    case UNEQ_EXPR: case LTGT_EXPR:
    case RANGE_EXPR: case COMPLEX_EXPR:

      loc = EXPR_LOCATION (x);
      op0 = cp_fold_maybe_rvalue (TREE_OPERAND (x, 0), rval_ops);
      op1 = cp_fold_rvalue (TREE_OPERAND (x, 1));

      if (op0 != TREE_OPERAND (x, 0) || op1 != TREE_OPERAND (x, 1))
	{
	  if (op0 == error_mark_node || op1 == error_mark_node)
	    x = error_mark_node;
	  else
	    x = fold_build2_loc (loc, code, TREE_TYPE (x), op0, op1);
	}
      else
	x = fold (x);

      if (TREE_NO_WARNING (org_x)
	  && warn_nonnull_compare
	  && COMPARISON_CLASS_P (org_x))
	{
	  if (x == error_mark_node || TREE_CODE (x) == INTEGER_CST)
	    ;
	  else if (COMPARISON_CLASS_P (x))
	    TREE_NO_WARNING (x) = 1;
	  /* Otherwise give up on optimizing these, let GIMPLE folders
	     optimize those later on.  */
	  else if (op0 != TREE_OPERAND (org_x, 0)
		   || op1 != TREE_OPERAND (org_x, 1))
	    {
	      x = build2_loc (loc, code, TREE_TYPE (org_x), op0, op1);
	      TREE_NO_WARNING (x) = 1;
	    }
	  else
	    x = org_x;
	}
      break;

    case VEC_COND_EXPR:
    case COND_EXPR:

      /* Don't bother folding a void condition, since it can't produce a
	 constant value.  Also, some statement-level uses of COND_EXPR leave
	 one of the branches NULL, so folding would crash.  */
      if (VOID_TYPE_P (TREE_TYPE (x)))
	return x;

      loc = EXPR_LOCATION (x);
      op0 = cp_fold_rvalue (TREE_OPERAND (x, 0));
      op1 = cp_fold (TREE_OPERAND (x, 1));
      op2 = cp_fold (TREE_OPERAND (x, 2));

      if (TREE_CODE (TREE_TYPE (x)) == BOOLEAN_TYPE)
	{
	  warning_sentinel s (warn_int_in_bool_context);
	  if (!VOID_TYPE_P (TREE_TYPE (op1)))
	    op1 = cp_truthvalue_conversion (op1);
	  if (!VOID_TYPE_P (TREE_TYPE (op2)))
	    op2 = cp_truthvalue_conversion (op2);
	}

      if (op0 != TREE_OPERAND (x, 0)
	  || op1 != TREE_OPERAND (x, 1)
	  || op2 != TREE_OPERAND (x, 2))
	{
	  if (op0 == error_mark_node
	      || op1 == error_mark_node
	      || op2 == error_mark_node)
	    x = error_mark_node;
	  else
	    x = fold_build3_loc (loc, code, TREE_TYPE (x), op0, op1, op2);
	}
      else
	x = fold (x);

      /* A COND_EXPR might have incompatible types in branches if one or both
	 arms are bitfields.  If folding exposed such a branch, fix it up.  */
      if (TREE_CODE (x) != code
	  && !useless_type_conversion_p (TREE_TYPE (org_x), TREE_TYPE (x)))
	x = fold_convert (TREE_TYPE (org_x), x);

      break;

    case CALL_EXPR:
      {
	int i, m, sv = optimize, nw = sv, changed = 0;
	tree callee = get_callee_fndecl (x);

	/* Some built-in function calls will be evaluated at compile-time in
	   fold ().  Set optimize to 1 when folding __builtin_constant_p inside
	   a constexpr function so that fold_builtin_1 doesn't fold it to 0.  */
	if (callee && DECL_BUILT_IN (callee) && !optimize
	    && DECL_IS_BUILTIN_CONSTANT_P (callee)
	    && current_function_decl
	    && DECL_DECLARED_CONSTEXPR_P (current_function_decl))
	  nw = 1;

	x = copy_node (x);

	m = call_expr_nargs (x);
	for (i = 0; i < m; i++)
	  {
	    r = cp_fold (CALL_EXPR_ARG (x, i));
	    if (r != CALL_EXPR_ARG (x, i))
	      {
		if (r == error_mark_node)
		  {
		    x = error_mark_node;
		    break;
		  }
		changed = 1;
	      }
	    CALL_EXPR_ARG (x, i) = r;
	  }
	if (x == error_mark_node)
	  break;

	optimize = nw;
	r = fold (x);
	optimize = sv;

	if (TREE_CODE (r) != CALL_EXPR)
	  {
	    x = cp_fold (r);
	    break;
	  }

	optimize = nw;

	/* Invoke maybe_constant_value for functions declared
	   constexpr and not called with AGGR_INIT_EXPRs.
	   TODO:
	   Do constexpr expansion of expressions where the call itself is not
	   constant, but the call followed by an INDIRECT_REF is.  */
	if (callee && DECL_DECLARED_CONSTEXPR_P (callee)
	    && !flag_no_inline)
	  r = maybe_constant_value (x);
	optimize = sv;

        if (TREE_CODE (r) != CALL_EXPR)
	  {
	    if (DECL_CONSTRUCTOR_P (callee))
	      {
		loc = EXPR_LOCATION (x);
		tree s = build_fold_indirect_ref_loc (loc,
						      CALL_EXPR_ARG (x, 0));
		r = build2_loc (loc, INIT_EXPR, TREE_TYPE (s), s, r);
	      }
	    x = r;
	    break;
	  }

	if (!changed)
	  x = org_x;
	break;
      }

    case CONSTRUCTOR:
      {
	unsigned i;
	constructor_elt *p;
	vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (x);
	vec<constructor_elt, va_gc> *nelts = NULL;
	FOR_EACH_VEC_SAFE_ELT (elts, i, p)
	  {
	    tree op = cp_fold (p->value);
	    if (op != p->value)
	      {
		if (op == error_mark_node)
		  {
		    x = error_mark_node;
		    vec_free (nelts);
		    break;
		  }
		if (nelts == NULL)
		  nelts = elts->copy ();
		(*nelts)[i].value = op;
	      }
	  }
	if (nelts)
	  x = build_constructor (TREE_TYPE (x), nelts);
	break;
      }
    case TREE_VEC:
      {
	bool changed = false;
	vec<tree, va_gc> *vec = make_tree_vector ();
	int i, n = TREE_VEC_LENGTH (x);
	vec_safe_reserve (vec, n);

	for (i = 0; i < n; i++)
	  {
	    tree op = cp_fold (TREE_VEC_ELT (x, i));
	    vec->quick_push (op);
	    if (op != TREE_VEC_ELT (x, i))
	      changed = true;
	  }

	if (changed)
	  {
	    r = copy_node (x);
	    for (i = 0; i < n; i++)
	      TREE_VEC_ELT (r, i) = (*vec)[i];
	    x = r;
	  }

	release_tree_vector (vec);
      }

      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:

      loc = EXPR_LOCATION (x);
      op0 = cp_fold (TREE_OPERAND (x, 0));
      op1 = cp_fold (TREE_OPERAND (x, 1));
      op2 = cp_fold (TREE_OPERAND (x, 2));
      op3 = cp_fold (TREE_OPERAND (x, 3));

      if (op0 != TREE_OPERAND (x, 0)
	  || op1 != TREE_OPERAND (x, 1)
	  || op2 != TREE_OPERAND (x, 2)
	  || op3 != TREE_OPERAND (x, 3))
	{
	  if (op0 == error_mark_node
	      || op1 == error_mark_node
	      || op2 == error_mark_node
	      || op3 == error_mark_node)
	    x = error_mark_node;
	  else
	    {
	      x = build4_loc (loc, code, TREE_TYPE (x), op0, op1, op2, op3);
	      TREE_READONLY (x) = TREE_READONLY (org_x);
	      TREE_SIDE_EFFECTS (x) = TREE_SIDE_EFFECTS (org_x);
	      TREE_THIS_VOLATILE (x) = TREE_THIS_VOLATILE (org_x);
	    }
	}

      x = fold (x);
      break;

    default:
      return org_x;
    }

  fold_cache->put (org_x, x);
  /* Prevent that we try to fold an already folded result again.  */
  if (x != org_x)
    fold_cache->put (x, x);

  return x;
}

#include "gt-cp-cp-gimplify.h"
