/* C++-specific tree lowering bits; see also c-gimplify.cc and gimple.cc.

   Copyright (C) 2002-2023 Free Software Foundation, Inc.
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
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "gcc-rich-location.h"
#include "memmodel.h"
#include "tm_p.h"
#include "output.h"
#include "file-prefix-map.h"
#include "cgraph.h"
#include "omp-general.h"
#include "opts.h"

/* Flags for cp_fold and cp_fold_r.  */

enum fold_flags {
  ff_none = 0,
  /* Whether we're being called from cp_fold_function.  */
  ff_genericize = 1 << 0,
  /* Whether we're folding a point where we know we're
     definitely not in a manifestly constant-evaluated
     context.  */
  ff_mce_false = 1 << 1,
};

using fold_flags_t = int;

struct cp_fold_data
{
  hash_set<tree> pset;
  fold_flags_t flags;
  cp_fold_data (fold_flags_t flags): flags (flags) {}
};

/* Forward declarations.  */

static tree cp_genericize_r (tree *, int *, void *);
static tree cp_fold_r (tree *, int *, void *);
static void cp_genericize_tree (tree*, bool);
static tree cp_fold (tree, fold_flags_t);

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
  tree failure = build_call_n (call_unexpected_fn, 1, build_exc_ptr ());

  *stmt_p = build_gimple_eh_filter_tree (body, allowed, failure);
  suppress_warning (*stmt_p);
  suppress_warning (TREE_OPERAND (*stmt_p, 1));
}

/* Return the first non-compound statement in STMT.  */

tree
first_stmt (tree stmt)
{
  switch (TREE_CODE (stmt))
    {
    case STATEMENT_LIST:
      if (tree_statement_list_node *p = STATEMENT_LIST_HEAD (stmt))
	return first_stmt (p->stmt);
      return void_node;

    case BIND_EXPR:
      return first_stmt (BIND_EXPR_BODY (stmt));

    default:
      return stmt;
    }
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

  if (then_ && else_)
    {
      tree ft = first_stmt (then_);
      tree fe = first_stmt (else_);
      br_predictor pr;
      if (TREE_CODE (ft) == PREDICT_EXPR
	  && TREE_CODE (fe) == PREDICT_EXPR
	  && (pr = PREDICT_EXPR_PREDICTOR (ft)) == PREDICT_EXPR_PREDICTOR (fe)
	  && (pr == PRED_HOT_LABEL || pr == PRED_COLD_LABEL))
	{
	  gcc_rich_location richloc (EXPR_LOC_OR_LOC (ft, locus));
	  richloc.add_range (EXPR_LOC_OR_LOC (fe, locus));
	  warning_at (&richloc, OPT_Wattributes,
		      "both branches of %<if%> statement marked as %qs",
		      pr == PRED_HOT_LABEL ? "likely" : "unlikely");
	}
    }

  if (!then_)
    then_ = build_empty_stmt (locus);
  if (!else_)
    else_ = build_empty_stmt (locus);

  /* consteval if has been verified not to have the then_/else_ blocks
     entered by gotos/case labels from elsewhere, and as then_ block
     can contain unfolded immediate function calls, we have to discard
     the then_ block regardless of whether else_ has side-effects or not.  */
  if (IF_STMT_CONSTEVAL_P (stmt))
    {
      if (block_may_fallthru (then_))
	stmt = build3 (COND_EXPR, void_type_node, boolean_false_node,
		       void_node, else_);
      else
	stmt = else_;
    }
  else if (IF_STMT_CONSTEXPR_P (stmt))
    stmt = integer_nonzerop (cond) ? then_ : else_;
  /* ??? This optimization doesn't seem to belong here, but removing it
     causes -Wreturn-type regressions (e.g. 107310).  */
  else if (integer_nonzerop (cond) && !TREE_SIDE_EFFECTS (else_))
    stmt = then_;
  else if (integer_zerop (cond) && !TREE_SIDE_EFFECTS (then_))
    stmt = else_;
  else
    stmt = build3 (COND_EXPR, void_type_node, cond, then_, else_);
  protected_set_expr_location_if_unset (stmt, locus);
  *stmt_p = stmt;
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
	      && !warning_suppressed_p (stmt, OPT_Wunused_value))
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

  if (TREE_CODE (from) == TARGET_EXPR)
    if (tree init = TARGET_EXPR_INITIAL (from))
      {
	/* Make sure that we expected to elide this temporary.  But also allow
	   gimplify_modify_expr_rhs to elide temporaries of trivial type.  */
	gcc_checking_assert (TARGET_EXPR_ELIDING_P (from)
			     || !TREE_ADDRESSABLE (TREE_TYPE (from)));
	if (target_expr_needs_replace (from))
	  {
	    /* If this was changed by cp_genericize_target_expr, we need to
	       walk into it to replace uses of the slot.  */
	    replace_decl (&init, TARGET_EXPR_SLOT (from), to);
	    *expr_p = init;
	    return;
	  }
	else
	  from = init;
      }

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
  mnt = gimple_build_eh_must_not_throw (terminate_fn);
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

bool
simple_empty_class_p (tree type, tree op, tree_code code)
{
  if (TREE_CODE (op) == COMPOUND_EXPR)
    return simple_empty_class_p (type, TREE_OPERAND (op, 1), code);
  if (SIMPLE_TARGET_EXPR_P (op)
      && TYPE_HAS_TRIVIAL_DESTRUCTOR (type))
    /* The TARGET_EXPR is itself a simple copy, look through it.  */
    return simple_empty_class_p (type, TARGET_EXPR_INITIAL (op), code);

  if (TREE_CODE (op) == PARM_DECL
      && TREE_ADDRESSABLE (TREE_TYPE (op)))
    {
      tree fn = DECL_CONTEXT (op);
      if (DECL_THUNK_P (fn)
	  || lambda_static_thunk_p (fn))
	/* In a thunk, we pass through invisible reference parms, so this isn't
	   actually a copy.  */
	return false;
    }

  return
    (TREE_CODE (op) == EMPTY_CLASS_EXPR
     || code == MODIFY_EXPR
     || is_gimple_lvalue (op)
     || INDIRECT_REF_P (op)
     || (TREE_CODE (op) == CONSTRUCTOR
	 && CONSTRUCTOR_NELTS (op) == 0)
     || (TREE_CODE (op) == CALL_EXPR
	 && !CALL_EXPR_RETURN_SLOT_OPT (op)))
    && !TREE_CLOBBER_P (op)
    && is_really_empty_class (type, /*ignore_vptr*/true);
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

/* Gimplify *EXPR_P as rvalue into an expression that can't be modified
   by expressions with side-effects in other operands.  */

static enum gimplify_status
gimplify_to_rvalue (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p,
		    bool (*gimple_test_f) (tree))
{
  enum gimplify_status t
    = gimplify_expr (expr_p, pre_p, post_p, gimple_test_f, fb_rvalue);
  if (t == GS_ERROR)
    return GS_ERROR;
  else if (is_gimple_variable (*expr_p) && TREE_CODE (*expr_p) != SSA_NAME)
    *expr_p = get_initialized_tmp_var (*expr_p, pre_p);
  return t;
}

/* Like gimplify_arg, but if ORDERED is set (which should be set if
   any of the arguments this argument is sequenced before has
   TREE_SIDE_EFFECTS set, make sure expressions with is_gimple_reg_type type
   are gimplified into SSA_NAME or a fresh temporary and for
   non-is_gimple_reg_type we don't optimize away TARGET_EXPRs.  */

static enum gimplify_status
cp_gimplify_arg (tree *arg_p, gimple_seq *pre_p, location_t call_location,
		 bool ordered)
{
  enum gimplify_status t;
  if (ordered
      && !is_gimple_reg_type (TREE_TYPE (*arg_p))
      && TREE_CODE (*arg_p) == TARGET_EXPR)
    {
      /* gimplify_arg would strip away the TARGET_EXPR, but
	 that can mean we don't copy the argument and some following
	 argument with side-effect could modify it.  */
      protected_set_expr_location (*arg_p, call_location);
      return gimplify_expr (arg_p, pre_p, NULL, is_gimple_lvalue, fb_either);
    }
  else
    {
      t = gimplify_arg (arg_p, pre_p, call_location);
      if (t == GS_ERROR)
	return GS_ERROR;
      else if (ordered
	       && is_gimple_reg_type (TREE_TYPE (*arg_p))
	       && is_gimple_variable (*arg_p)
	       && TREE_CODE (*arg_p) != SSA_NAME
	       /* No need to force references into register, references
		  can't be modified.  */
	       && !TYPE_REF_P (TREE_TYPE (*arg_p))
	       /* And this can't be modified either.  */
	       && *arg_p != current_class_ptr)
	*arg_p = get_initialized_tmp_var (*arg_p, pre_p);
      return t;
    }

}

/* Do C++-specific gimplification.  Args are as for gimplify_expr.  */

int
cp_gimplify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  int saved_stmts_are_full_exprs_p = 0;
  location_t loc = cp_expr_loc_or_input_loc (*expr_p);
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
	*expr_p = expand_vec_init_expr (NULL_TREE, *expr_p,
					tf_warning_or_error);

	cp_fold_data data (ff_genericize | ff_mce_false);
	cp_walk_tree (expr_p, cp_fold_r, &data, NULL);
	cp_genericize_tree (expr_p, false);
	copy_if_shared (expr_p);
	ret = GS_OK;
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
      cp_gimplify_init_expr (expr_p);
      if (TREE_CODE (*expr_p) != INIT_EXPR)
	return GS_OK;
      /* Fall through.  */
    case MODIFY_EXPR:
    modify_expr_case:
      {
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

	else if (simple_empty_class_p (TREE_TYPE (op0), op1, code))
	  {
	    while (TREE_CODE (op1) == TARGET_EXPR)
	      /* We're disconnecting the initializer from its target,
		 don't create a temporary.  */
	      op1 = TARGET_EXPR_INITIAL (op1);

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
	    if (code == RETURN_EXPR && REFERENCE_CLASS_P (*expr_p))
	      /* Avoid 'return *<retval>;'  */
	      *expr_p = TREE_OPERAND (*expr_p, 0);
	  }
	/* P0145 says that the RHS is sequenced before the LHS.
	   gimplify_modify_expr gimplifies the RHS before the LHS, but that
	   isn't quite strong enough in two cases:

	   1) gimplify.cc wants to leave a CALL_EXPR on the RHS, which would
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
	 TREE_OPERAND (*expr_p, 1) = get_initialized_tmp_var (op1, pre_p);
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
    case OMP_LOOP:
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

    case CALL_EXPR:
      ret = GS_OK;
      if (flag_strong_eval_order == 2
	  && CALL_EXPR_FN (*expr_p)
	  && !CALL_EXPR_OPERATOR_SYNTAX (*expr_p)
	  && cp_get_callee_fndecl_nofold (*expr_p) == NULL_TREE)
	{
	  tree fnptrtype = TREE_TYPE (CALL_EXPR_FN (*expr_p));
	  enum gimplify_status t
	    = gimplify_to_rvalue (&CALL_EXPR_FN (*expr_p), pre_p, NULL,
				  is_gimple_call_addr);
	  if (t == GS_ERROR)
	    ret = GS_ERROR;
	  /* GIMPLE considers most pointer conversion useless, but for
	     calls we actually care about the exact function pointer type.  */
	  else if (TREE_TYPE (CALL_EXPR_FN (*expr_p)) != fnptrtype)
	    CALL_EXPR_FN (*expr_p)
	      = build1 (NOP_EXPR, fnptrtype, CALL_EXPR_FN (*expr_p));
	}
      if (!CALL_EXPR_FN (*expr_p))
	/* Internal function call.  */;
      else if (CALL_EXPR_REVERSE_ARGS (*expr_p))
	{
	  /* This is a call to a (compound) assignment operator that used
	     the operator syntax; gimplify the RHS first.  */
	  gcc_assert (call_expr_nargs (*expr_p) == 2);
	  gcc_assert (!CALL_EXPR_ORDERED_ARGS (*expr_p));
	  enum gimplify_status t
	    = cp_gimplify_arg (&CALL_EXPR_ARG (*expr_p, 1), pre_p, loc,
			       TREE_SIDE_EFFECTS (CALL_EXPR_ARG (*expr_p, 0)));
	  if (t == GS_ERROR)
	    ret = GS_ERROR;
	}
      else if (CALL_EXPR_ORDERED_ARGS (*expr_p))
	{
	  /* Leave the last argument for gimplify_call_expr, to avoid problems
	     with __builtin_va_arg_pack().  */
	  int nargs = call_expr_nargs (*expr_p) - 1;
	  int last_side_effects_arg = -1;
	  for (int i = nargs; i > 0; --i)
	    if (TREE_SIDE_EFFECTS (CALL_EXPR_ARG (*expr_p, i)))
	      {
		last_side_effects_arg = i;
		break;
	      }
	  for (int i = 0; i < nargs; ++i)
	    {
	      enum gimplify_status t
		= cp_gimplify_arg (&CALL_EXPR_ARG (*expr_p, i), pre_p, loc,
				   i < last_side_effects_arg);
	      if (t == GS_ERROR)
		ret = GS_ERROR;
	    }
	}
      else if (flag_strong_eval_order
	       && !CALL_EXPR_OPERATOR_SYNTAX (*expr_p))
	{
	  /* If flag_strong_eval_order, evaluate the object argument first.  */
	  tree fntype = TREE_TYPE (CALL_EXPR_FN (*expr_p));
	  if (INDIRECT_TYPE_P (fntype))
	    fntype = TREE_TYPE (fntype);
	  if (TREE_CODE (fntype) == METHOD_TYPE)
	    {
	      int nargs = call_expr_nargs (*expr_p);
	      bool side_effects = false;
	      for (int i = 1; i < nargs; ++i)
		if (TREE_SIDE_EFFECTS (CALL_EXPR_ARG (*expr_p, i)))
		  {
		    side_effects = true;
		    break;
		  }
	      enum gimplify_status t
		= cp_gimplify_arg (&CALL_EXPR_ARG (*expr_p, 0), pre_p, loc,
				   side_effects);
	      if (t == GS_ERROR)
		ret = GS_ERROR;
	    }
	}
      if (ret != GS_ERROR)
	{
	  tree decl = cp_get_callee_fndecl_nofold (*expr_p);
	  if (decl && fndecl_built_in_p (decl, BUILT_IN_FRONTEND))
	    switch (DECL_FE_FUNCTION_CODE (decl))
	      {
	      case CP_BUILT_IN_IS_CONSTANT_EVALUATED:
		*expr_p = boolean_false_node;
		break;
	      case CP_BUILT_IN_SOURCE_LOCATION:
		*expr_p
		  = fold_builtin_source_location (*expr_p);
		break;
	      case CP_BUILT_IN_IS_CORRESPONDING_MEMBER:
		*expr_p
		  = fold_builtin_is_corresponding_member
			(EXPR_LOCATION (*expr_p), call_expr_nargs (*expr_p),
			 &CALL_EXPR_ARG (*expr_p, 0));
		break;
	      case CP_BUILT_IN_IS_POINTER_INTERCONVERTIBLE_WITH_CLASS:
		*expr_p
		  = fold_builtin_is_pointer_inverconvertible_with_class
			(EXPR_LOCATION (*expr_p), call_expr_nargs (*expr_p),
			 &CALL_EXPR_ARG (*expr_p, 0));
		break;
	      default:
		break;
	      }
	}
      break;

    case TARGET_EXPR:
      /* A TARGET_EXPR that expresses direct-initialization should have been
	 elided by cp_gimplify_init_expr.  */
      gcc_checking_assert (!TARGET_EXPR_DIRECT_INIT_P (*expr_p));
      /* Likewise, but allow extra temps of trivial type so that
	 gimplify_init_ctor_preeval can materialize subobjects of a CONSTRUCTOR
	 on the rhs of an assignment, as in constexpr-aggr1.C.  */
      gcc_checking_assert (!TARGET_EXPR_ELIDING_P (*expr_p)
			   || !TREE_ADDRESSABLE (TREE_TYPE (*expr_p)));
      ret = GS_UNHANDLED;
      break;

    case PTRMEM_CST:
      *expr_p = cplus_expand_constant (*expr_p);
      if (TREE_CODE (*expr_p) == PTRMEM_CST)
	ret = GS_ERROR;
      else
	ret = GS_OK;
      break;

    case RETURN_EXPR:
      if (TREE_OPERAND (*expr_p, 0)
	  && (TREE_CODE (TREE_OPERAND (*expr_p, 0)) == INIT_EXPR
	      || TREE_CODE (TREE_OPERAND (*expr_p, 0)) == MODIFY_EXPR))
	{
	  expr_p = &TREE_OPERAND (*expr_p, 0);
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
  else if (TYPE_REF_P (type))
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
	      else if (TYPE_REF_P (type))
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

/* True if any of the element initializers in CTOR are TARGET_EXPRs that are
   not expected to elide, e.g. because unsafe_copy_elision_p is true.  */

static bool
any_non_eliding_target_exprs (tree ctor)
{
  for (const constructor_elt &e : *CONSTRUCTOR_ELTS (ctor))
    {
      if (TREE_CODE (e.value) == TARGET_EXPR
	  && !TARGET_EXPR_ELIDING_P (e.value))
	return true;
    }
  return false;
}

/* If we might need to clean up a partially constructed object, break down the
   CONSTRUCTOR with split_nonconstant_init.  Also expand VEC_INIT_EXPR at this
   point.  If initializing TO with FROM is non-trivial, overwrite *REPLACE with
   the result.  */

static void
cp_genericize_init (tree *replace, tree from, tree to)
{
  tree init = NULL_TREE;
  if (TREE_CODE (from) == VEC_INIT_EXPR)
    init = expand_vec_init_expr (to, from, tf_warning_or_error);
  else if (TREE_CODE (from) == CONSTRUCTOR
	   && TREE_SIDE_EFFECTS (from)
	   && ((flag_exceptions
		&& TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (from)))
	       || any_non_eliding_target_exprs (from)))
    {
      to = cp_stabilize_reference (to);
      replace_placeholders (from, to);
      init = split_nonconstant_init (to, from);
    }

  if (init)
    {
      if (*replace == from)
	/* Make cp_gimplify_init_expr call replace_decl on this
	   TARGET_EXPR_INITIAL.  */
	init = fold_convert (void_type_node, init);
      *replace = init;
    }
}

/* For an INIT_EXPR, replace the INIT_EXPR itself.  */

static void
cp_genericize_init_expr (tree *stmt_p)
{
  iloc_sentinel ils = EXPR_LOCATION (*stmt_p);
  tree to = TREE_OPERAND (*stmt_p, 0);
  tree from = TREE_OPERAND (*stmt_p, 1);
  if (SIMPLE_TARGET_EXPR_P (from)
      /* Return gets confused if we clobber its INIT_EXPR this soon.  */
      && TREE_CODE (to) != RESULT_DECL)
    from = TARGET_EXPR_INITIAL (from);
  cp_genericize_init (stmt_p, from, to);
}

/* For a TARGET_EXPR, change the TARGET_EXPR_INITIAL.  We will need to use
   replace_decl later when we know what we're initializing.  */

static void
cp_genericize_target_expr (tree *stmt_p)
{
  iloc_sentinel ils = EXPR_LOCATION (*stmt_p);
  tree slot = TARGET_EXPR_SLOT (*stmt_p);
  cp_genericize_init (&TARGET_EXPR_INITIAL (*stmt_p),
		      TARGET_EXPR_INITIAL (*stmt_p), slot);
  gcc_assert (!DECL_INITIAL (slot));
}

/* Similar to if (target_expr_needs_replace) replace_decl, but TP is the
   TARGET_EXPR_INITIAL, and this also updates *_SLOT.  We need this extra
   replacement when cp_folding TARGET_EXPR to preserve the invariant that
   AGGR_INIT_EXPR_SLOT agrees with the enclosing TARGET_EXPR_SLOT.  */

bool
maybe_replace_decl (tree *tp, tree decl, tree replacement)
{
  if (!*tp || !VOID_TYPE_P (TREE_TYPE (*tp)))
    return false;
  tree t = *tp;
  while (TREE_CODE (t) == COMPOUND_EXPR)
    t = TREE_OPERAND (t, 1);
  if (TREE_CODE (t) == AGGR_INIT_EXPR)
    replace_decl (&AGGR_INIT_EXPR_SLOT (t), decl, replacement);
  else if (TREE_CODE (t) == VEC_INIT_EXPR)
    replace_decl (&VEC_INIT_EXPR_SLOT (t), decl, replacement);
  else
    replace_decl (tp, decl, replacement);
  return true;
}

/* Genericization context.  */

struct cp_genericize_data
{
  hash_set<tree> *p_set;
  auto_vec<tree> bind_expr_stack;
  struct cp_genericize_omp_taskreg *omp_ctx;
  tree try_block;
  bool no_sanitize_p;
  bool handle_invisiref_parm_p;
};

/* Perform any pre-gimplification folding of C++ front end trees to
   GENERIC.
   Note:  The folding of non-omp cases is something to move into
     the middle-end.  As for now we have most foldings only on GENERIC
     in fold-const, we need to perform this before transformation to
     GIMPLE-form.  */

static tree
cp_fold_r (tree *stmt_p, int *walk_subtrees, void *data_)
{
  cp_fold_data *data = (cp_fold_data*)data_;
  tree stmt = *stmt_p;
  enum tree_code code = TREE_CODE (stmt);

  switch (code)
    {
    case PTRMEM_CST:
      if (TREE_CODE (PTRMEM_CST_MEMBER (stmt)) == FUNCTION_DECL
	  && DECL_IMMEDIATE_FUNCTION_P (PTRMEM_CST_MEMBER (stmt)))
	{
	  if (!data->pset.add (stmt))
	    error_at (PTRMEM_CST_LOCATION (stmt),
		      "taking address of an immediate function %qD",
		      PTRMEM_CST_MEMBER (stmt));
	  stmt = *stmt_p = build_zero_cst (TREE_TYPE (stmt));
	  break;
	}
      break;

    case ADDR_EXPR:
      if (TREE_CODE (TREE_OPERAND (stmt, 0)) == FUNCTION_DECL
	  && DECL_IMMEDIATE_FUNCTION_P (TREE_OPERAND (stmt, 0)))
	{
	  error_at (EXPR_LOCATION (stmt),
		    "taking address of an immediate function %qD",
		    TREE_OPERAND (stmt, 0));
	  stmt = *stmt_p = build_zero_cst (TREE_TYPE (stmt));
	  break;
	}
      break;

    default:
      break;
    }

  *stmt_p = stmt = cp_fold (*stmt_p, data->flags);

  if (data->pset.add (stmt))
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
  switch (code)
    {
      tree x;
      int i, n;
    case OMP_FOR:
    case OMP_SIMD:
    case OMP_DISTRIBUTE:
    case OMP_LOOP:
    case OMP_TASKLOOP:
    case OACC_LOOP:
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
      return NULL;

    case IF_STMT:
      if (IF_STMT_CONSTEVAL_P (stmt))
	{
	  /* Don't walk THEN_CLAUSE (stmt) for consteval if.  IF_COND is always
	     boolean_false_node.  */
	  cp_walk_tree (&ELSE_CLAUSE (stmt), cp_fold_r, data, NULL);
	  cp_walk_tree (&IF_SCOPE (stmt), cp_fold_r, data, NULL);
	  *walk_subtrees = 0;
	  return NULL;
	}
      break;

      /* cp_genericize_{init,target}_expr are only for genericize time; they're
	 here rather than in cp_genericize to avoid problems with the invisible
	 reference transition.  */
    case INIT_EXPR:
      if (data->flags & ff_genericize)
	cp_genericize_init_expr (stmt_p);
      break;

    case TARGET_EXPR:
      if (data->flags & ff_genericize)
	cp_genericize_target_expr (stmt_p);

      /* Folding might replace e.g. a COND_EXPR with a TARGET_EXPR; in
	 that case, strip it in favor of this one.  */
      if (tree &init = TARGET_EXPR_INITIAL (stmt))
	{
	  cp_walk_tree (&init, cp_fold_r, data, NULL);
	  cp_walk_tree (&TARGET_EXPR_CLEANUP (stmt), cp_fold_r, data, NULL);
	  *walk_subtrees = 0;
	  if (TREE_CODE (init) == TARGET_EXPR)
	    {
	      tree sub = TARGET_EXPR_INITIAL (init);
	      maybe_replace_decl (&sub, TARGET_EXPR_SLOT (init),
				  TARGET_EXPR_SLOT (stmt));
	      init = sub;
	    }
	}
      break;

    default:
      break;
    }

  return NULL;
}

/* Fold ALL the trees!  FIXME we should be able to remove this, but
   apparently that still causes optimization regressions.  */

void
cp_fold_function (tree fndecl)
{
  /* By now all manifestly-constant-evaluated expressions will have
     been constant-evaluated already if possible, so we can safely
     pass ff_mce_false.  */
  cp_fold_data data (ff_genericize | ff_mce_false);
  cp_walk_tree (&DECL_SAVED_TREE (fndecl), cp_fold_r, &data, NULL);
}

/* Turn SPACESHIP_EXPR EXPR into GENERIC.  */

static tree genericize_spaceship (tree expr)
{
  iloc_sentinel s (cp_expr_location (expr));
  tree type = TREE_TYPE (expr);
  tree op0 = TREE_OPERAND (expr, 0);
  tree op1 = TREE_OPERAND (expr, 1);
  return genericize_spaceship (input_location, type, op0, op1);
}

/* If EXPR involves an anonymous VLA type, prepend a DECL_EXPR for that type
   to trigger gimplify_type_sizes; otherwise a cast to pointer-to-VLA confuses
   the middle-end (c++/88256).  If EXPR is a DECL, use add_stmt and return
   NULL_TREE; otherwise return a COMPOUND_STMT of the DECL_EXPR and EXPR.  */

tree
predeclare_vla (tree expr)
{
  tree type = TREE_TYPE (expr);
  if (type == error_mark_node)
    return expr;
  if (is_typedef_decl (expr))
    type = DECL_ORIGINAL_TYPE (expr);

  /* We need to strip pointers for gimplify_type_sizes.  */
  tree vla = type;
  while (POINTER_TYPE_P (vla))
    {
      if (TYPE_NAME (vla))
	return expr;
      vla = TREE_TYPE (vla);
    }
  if (vla == type || TYPE_NAME (vla)
      || !variably_modified_type_p (vla, NULL_TREE))
    return expr;

  tree decl = build_decl (input_location, TYPE_DECL, NULL_TREE, vla);
  DECL_ARTIFICIAL (decl) = 1;
  TYPE_NAME (vla) = decl;
  tree dexp = build_stmt (input_location, DECL_EXPR, decl);
  if (DECL_P (expr))
    {
      add_stmt (dexp);
      return NULL_TREE;
    }
  else
    {
      expr = build2 (COMPOUND_EXPR, type, dexp, expr);
      return expr;
    }
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
  if (UNLIKELY (wtd->omp_ctx != NULL)
      && (VAR_P (stmt)
	  || TREE_CODE (stmt) == PARM_DECL
	  || TREE_CODE (stmt) == RESULT_DECL)
      && omp_var_to_track (stmt))
    omp_cxx_notice_variable (wtd->omp_ctx, stmt);

  /* Don't dereference parms in a thunk, pass the references through. */
  if ((TREE_CODE (stmt) == CALL_EXPR && call_from_lambda_thunk_p (stmt))
      || (TREE_CODE (stmt) == AGGR_INIT_EXPR && AGGR_INIT_FROM_THUNK_P (stmt)))
    {
      *walk_subtrees = 0;
      return NULL;
    }

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
  if (VAR_OR_FUNCTION_DECL_P (stmt) && DECL_LOCAL_DECL_P (stmt))
    if (tree alias = DECL_LOCAL_DECL_ALIAS (stmt))
      {
	if (alias != error_mark_node)
	  {
	    *stmt_p = alias;
	    TREE_USED (alias) |= TREE_USED (stmt);
	  }
	*walk_subtrees = 0;
	return NULL;
      }

  if (TREE_CODE (stmt) == INTEGER_CST
      && TYPE_REF_P (TREE_TYPE (stmt))
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

  switch (TREE_CODE (stmt))
    {
    case ADDR_EXPR:
      if (is_invisiref_parm (TREE_OPERAND (stmt, 0)))
	{
	  /* If in an OpenMP context, note var uses.  */
	  if (UNLIKELY (wtd->omp_ctx != NULL)
	      && omp_var_to_track (TREE_OPERAND (stmt, 0)))
	    omp_cxx_notice_variable (wtd->omp_ctx, TREE_OPERAND (stmt, 0));
	  *stmt_p = fold_convert (TREE_TYPE (stmt), TREE_OPERAND (stmt, 0));
	  *walk_subtrees = 0;
	}
      break;

    case RETURN_EXPR:
      if (TREE_OPERAND (stmt, 0) && is_invisiref_parm (TREE_OPERAND (stmt, 0)))
	/* Don't dereference an invisiref RESULT_DECL inside a RETURN_EXPR.  */
	*walk_subtrees = 0;
      break;

    case OMP_CLAUSE:
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
	case OMP_CLAUSE_INCLUSIVE:
	case OMP_CLAUSE_EXCLUSIVE:
	  /* Don't dereference an invisiref in OpenMP clauses.  */
	  if (is_invisiref_parm (OMP_CLAUSE_DECL (stmt)))
	    *walk_subtrees = 0;
	  break;
	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
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
      break;

    /* Due to the way voidify_wrapper_expr is written, we don't get a chance
       to lower this construct before scanning it, so we need to lower these
       before doing anything else.  */
    case CLEANUP_STMT:
      *stmt_p = build2_loc (EXPR_LOCATION (stmt),
			    CLEANUP_EH_ONLY (stmt) ? TRY_CATCH_EXPR
						   : TRY_FINALLY_EXPR,
			    void_type_node,
			    CLEANUP_BODY (stmt),
			    CLEANUP_EXPR (stmt));
      break;

    case IF_STMT:
      genericize_if_stmt (stmt_p);
      /* *stmt_p has changed, tail recurse to handle it again.  */
      return cp_genericize_r (stmt_p, walk_subtrees, data);

    /* COND_EXPR might have incompatible types in branches if one or both
       arms are bitfields.  Fix it up now.  */
    case COND_EXPR:
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
      break;

    case BIND_EXPR:
      if (UNLIKELY (wtd->omp_ctx != NULL))
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
      if (sanitize_flags_p (SANITIZE_NULL | SANITIZE_ALIGNMENT | SANITIZE_VPTR))
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
      break;

    case ASSERTION_STMT:
    case PRECONDITION_STMT:
    case POSTCONDITION_STMT:
      {
	if (tree check = build_contract_check (stmt))
	  {
	    *stmt_p = check;
	    return cp_genericize_r (stmt_p, walk_subtrees, data);
	  }

	/* If we didn't build a check, replace it with void_node so we don't
	   leak contracts into GENERIC.  */
	*stmt_p = void_node;
	*walk_subtrees = 0;
      }
      break;

    case USING_STMT:
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
	    tree decl = TREE_OPERAND (stmt, 0);
	    gcc_assert (decl);

	    if (undeduced_auto_decl (decl))
	      /* Omit from the GENERIC, the back-end can't handle it.  */;
	    else
	      {
		tree using_directive = make_node (IMPORTED_DECL);
		TREE_TYPE (using_directive) = void_type_node;
		DECL_CONTEXT (using_directive) = current_function_decl;
		DECL_SOURCE_LOCATION (using_directive)
		  = cp_expr_loc_or_input_loc (stmt);

		IMPORTED_DECL_ASSOCIATED_DECL (using_directive) = decl;
		DECL_CHAIN (using_directive) = BLOCK_VARS (block);
		BLOCK_VARS (block) = using_directive;
	      }
	  }
	/* The USING_STMT won't appear in GENERIC.  */
	*stmt_p = build1 (NOP_EXPR, void_type_node, integer_zero_node);
	*walk_subtrees = 0;
      }
      break;

    case DECL_EXPR:
      if (TREE_CODE (DECL_EXPR_DECL (stmt)) == USING_DECL)
	{
	  /* Using decls inside DECL_EXPRs are just dropped on the floor.  */
	  *stmt_p = build1 (NOP_EXPR, void_type_node, integer_zero_node);
	  *walk_subtrees = 0;
	}
      else
	{
	  tree d = DECL_EXPR_DECL (stmt);
	  if (VAR_P (d))
	    gcc_assert (CP_DECL_THREAD_LOCAL_P (d) == DECL_THREAD_LOCAL_P (d));
	}
      break;

    case OMP_PARALLEL:
    case OMP_TASK:
    case OMP_TASKLOOP:
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
	      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_PRIVATE && omp_ctx.outer)
		omp_cxx_notice_variable (omp_ctx.outer, decl);
	      break;
	    case OMP_CLAUSE_DEFAULT:
	      if (OMP_CLAUSE_DEFAULT_KIND (c) == OMP_CLAUSE_DEFAULT_SHARED)
		omp_ctx.default_shared = true;
	    default:
	      break;
	    }
	if (TREE_CODE (stmt) == OMP_TASKLOOP)
	  c_genericize_control_stmt (stmt_p, walk_subtrees, data,
				     cp_genericize_r, cp_walk_subtrees);
	else
	  cp_walk_tree (&OMP_BODY (stmt), cp_genericize_r, data, NULL);
	wtd->omp_ctx = omp_ctx.outer;
	splay_tree_delete (omp_ctx.variables);
      }
      break;

    case OMP_TARGET:
      cfun->has_omp_target = true;
      break;

    case TRY_BLOCK:
      {
        *walk_subtrees = 0;
        tree try_block = wtd->try_block;
        wtd->try_block = stmt;
        cp_walk_tree (&TRY_STMTS (stmt), cp_genericize_r, data, NULL);
        wtd->try_block = try_block;
        cp_walk_tree (&TRY_HANDLERS (stmt), cp_genericize_r, data, NULL);
      }
      break;

    case MUST_NOT_THROW_EXPR:
      /* MUST_NOT_THROW_COND might be something else with TM.  */
      if (MUST_NOT_THROW_COND (stmt) == NULL_TREE)
	{
	  *walk_subtrees = 0;
	  tree try_block = wtd->try_block;
	  wtd->try_block = stmt;
	  cp_walk_tree (&TREE_OPERAND (stmt, 0), cp_genericize_r, data, NULL);
	  wtd->try_block = try_block;
	}
      break;

    case THROW_EXPR:
      {
	location_t loc = location_of (stmt);
	if (warning_suppressed_p (stmt /* What warning? */))
	  /* Never mind.  */;
	else if (wtd->try_block)
	  {
	    if (TREE_CODE (wtd->try_block) == MUST_NOT_THROW_EXPR)
	      {
		auto_diagnostic_group d;
		if (warning_at (loc, OPT_Wterminate,
				"%<throw%> will always call %<terminate%>")
		    && cxx_dialect >= cxx11
		    && DECL_DESTRUCTOR_P (current_function_decl))
		  inform (loc, "in C++11 destructors default to %<noexcept%>");
	      }
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
			  "in C++11 this %<throw%> will call %<terminate%> "
			  "because destructors default to %<noexcept%>");
	  }
      }
      break;

    case CONVERT_EXPR:
      gcc_checking_assert (!AGGREGATE_TYPE_P (TREE_TYPE (stmt)));
      gcc_assert (!CONVERT_EXPR_VBASE_PATH (stmt));
      break;

    case SPACESHIP_EXPR:
      *stmt_p = genericize_spaceship (*stmt_p);
      break;

    case PTRMEM_CST:
      /* By the time we get here we're handing off to the back end, so we don't
	 need or want to preserve PTRMEM_CST anymore.  */
      *stmt_p = cplus_expand_constant (stmt);
      *walk_subtrees = 0;
      break;

    case MEM_REF:
      /* For MEM_REF, make sure not to sanitize the second operand even
	 if it has reference type.  It is just an offset with a type
	 holding other information.  There is no other processing we
	 need to do for INTEGER_CSTs, so just ignore the second argument
	 unconditionally.  */
      cp_walk_tree (&TREE_OPERAND (stmt, 0), cp_genericize_r, data, NULL);
      *walk_subtrees = 0;
      break;

    case NOP_EXPR:
      *stmt_p = predeclare_vla (*stmt_p);
      if (!wtd->no_sanitize_p
	  && sanitize_flags_p (SANITIZE_NULL | SANITIZE_ALIGNMENT)
	  && TYPE_REF_P (TREE_TYPE (stmt)))
	ubsan_maybe_instrument_reference (stmt_p);
      break;

    case CALL_EXPR:
      /* Evaluate function concept checks instead of treating them as
	 normal functions.  */
      if (concept_check_p (stmt))
	{
	  *stmt_p = evaluate_concept_check (stmt);
	  * walk_subtrees = 0;
	  break;
	}

      if (!wtd->no_sanitize_p
	  && sanitize_flags_p ((SANITIZE_NULL
				| SANITIZE_ALIGNMENT | SANITIZE_VPTR)))
	{
	  tree fn = CALL_EXPR_FN (stmt);
	  if (fn != NULL_TREE
	      && !error_operand_p (fn)
	      && INDIRECT_TYPE_P (TREE_TYPE (fn))
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (fn))) == METHOD_TYPE)
	    {
	      bool is_ctor
		= TREE_CODE (fn) == ADDR_EXPR
		  && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
		  && DECL_CONSTRUCTOR_P (TREE_OPERAND (fn, 0));
	      if (sanitize_flags_p (SANITIZE_NULL | SANITIZE_ALIGNMENT))
		ubsan_maybe_instrument_member_call (stmt, is_ctor);
	      if (sanitize_flags_p (SANITIZE_VPTR) && !is_ctor)
		cp_ubsan_maybe_instrument_member_call (stmt);
	    }
	  else if (fn == NULL_TREE
		   && CALL_EXPR_IFN (stmt) == IFN_UBSAN_NULL
		   && TREE_CODE (CALL_EXPR_ARG (stmt, 0)) == INTEGER_CST
		   && TYPE_REF_P (TREE_TYPE (CALL_EXPR_ARG (stmt, 0))))
	    *walk_subtrees = 0;
	}
      /* Fall through.  */
    case AGGR_INIT_EXPR:
      /* For calls to a multi-versioned function, overload resolution
	 returns the function with the highest target priority, that is,
	 the version that will checked for dispatching first.  If this
	 version is inlinable, a direct call to this version can be made
	 otherwise the call should go through the dispatcher.  */
      {
	tree fn = cp_get_callee_fndecl_nofold (stmt);
	if (fn && DECL_FUNCTION_VERSIONED (fn)
	    && (current_function_decl == NULL
		|| !targetm.target_option.can_inline_p (current_function_decl,
							fn)))
	  if (tree dis = get_function_version_dispatcher (fn))
	    {
	      mark_versions_used (dis);
	      dis = build_address (dis);
	      if (TREE_CODE (stmt) == CALL_EXPR)
		CALL_EXPR_FN (stmt) = dis;
	      else
		AGGR_INIT_EXPR_FN (stmt) = dis;
	    }
      }
      break;

    case TARGET_EXPR:
      if (TARGET_EXPR_INITIAL (stmt)
	  && TREE_CODE (TARGET_EXPR_INITIAL (stmt)) == CONSTRUCTOR
	  && CONSTRUCTOR_PLACEHOLDER_BOUNDARY (TARGET_EXPR_INITIAL (stmt)))
	TARGET_EXPR_NO_ELIDE (stmt) = 1;
      break;

    case TEMPLATE_ID_EXPR:
      gcc_assert (concept_check_p (stmt));
      /* Emit the value of the concept check.  */
      *stmt_p = evaluate_concept_check (stmt);
      walk_subtrees = 0;
      break;

    case OMP_DISTRIBUTE:
      /* Need to explicitly instantiate copy ctors on class iterators of
	 composite distribute parallel for.  */
      if (OMP_FOR_INIT (*stmt_p) == NULL_TREE)
	{
	  tree *data[4] = { NULL, NULL, NULL, NULL };
	  tree inner = walk_tree (&OMP_FOR_BODY (*stmt_p),
				  find_combined_omp_for, data, NULL);
	  if (inner != NULL_TREE
	      && TREE_CODE (inner) == OMP_FOR)
	    {
	      for (int i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (inner)); i++)
		if (OMP_FOR_ORIG_DECLS (inner)
		    && TREE_CODE (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner),
				  i)) == TREE_LIST
		    && TREE_PURPOSE (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner),
				     i)))
		  {
		    tree orig = TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner), i);
		    /* Class iterators aren't allowed on OMP_SIMD, so the only
		       case we need to solve is distribute parallel for.  */
		    gcc_assert (TREE_CODE (inner) == OMP_FOR
				&& data[1]);
		    tree orig_decl = TREE_PURPOSE (orig);
		    tree c, cl = NULL_TREE;
		    for (c = OMP_FOR_CLAUSES (inner);
			 c; c = OMP_CLAUSE_CHAIN (c))
		      if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_PRIVATE
			   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE)
			  && OMP_CLAUSE_DECL (c) == orig_decl)
			{
			  cl = c;
			  break;
			}
		    if (cl == NULL_TREE)
		      {
			for (c = OMP_PARALLEL_CLAUSES (*data[1]);
			     c; c = OMP_CLAUSE_CHAIN (c))
			  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_PRIVATE
			      && OMP_CLAUSE_DECL (c) == orig_decl)
			    {
			      cl = c;
			      break;
			    }
		      }
		    if (cl)
		      {
			orig_decl = require_complete_type (orig_decl);
			tree inner_type = TREE_TYPE (orig_decl);
			if (orig_decl == error_mark_node)
			  continue;
			if (TYPE_REF_P (TREE_TYPE (orig_decl)))
			  inner_type = TREE_TYPE (inner_type);

			while (TREE_CODE (inner_type) == ARRAY_TYPE)
			  inner_type = TREE_TYPE (inner_type);
			get_copy_ctor (inner_type, tf_warning_or_error);
		      }
		}
	    }
	}
      /* FALLTHRU */

    case FOR_STMT:
    case WHILE_STMT:
    case DO_STMT:
    case SWITCH_STMT:
    case CONTINUE_STMT:
    case BREAK_STMT:
    case OMP_FOR:
    case OMP_SIMD:
    case OMP_LOOP:
    case OACC_LOOP:
    case STATEMENT_LIST:
      /* These cases are handled by shared code.  */
      c_genericize_control_stmt (stmt_p, walk_subtrees, data,
				 cp_genericize_r, cp_walk_subtrees);
      break;

    case BIT_CAST_EXPR:
      *stmt_p = build1_loc (EXPR_LOCATION (stmt), VIEW_CONVERT_EXPR,
			    TREE_TYPE (stmt), TREE_OPERAND (stmt, 0));
      break;

    default:
      if (IS_TYPE_OR_DECL_P (stmt))
	*walk_subtrees = 0;
      break;
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
  if (sanitize_flags_p (SANITIZE_VPTR))
    cp_ubsan_instrument_member_accesses (t_p);
}

/* If a function that should end with a return in non-void
   function doesn't obviously end with return, add ubsan
   instrumentation code to verify it at runtime.  If -fsanitize=return
   is not enabled, instrument __builtin_unreachable.  */

static void
cp_maybe_instrument_return (tree fndecl)
{
  if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (fndecl)))
      || DECL_CONSTRUCTOR_P (fndecl)
      || DECL_DESTRUCTOR_P (fndecl)
      || !targetm.warn_func_return (fndecl))
    return;

  if (!sanitize_flags_p (SANITIZE_RETURN, fndecl)
      /* Don't add __builtin_unreachable () if not optimizing, it will not
	 improve any optimizations in that case, just break UB code.
	 Don't add it if -fsanitize=unreachable -fno-sanitize=return either,
	 UBSan covers this with ubsan_instrument_return above where sufficient
	 information is provided, while the __builtin_unreachable () below
	 if return sanitization is disabled will just result in hard to
	 understand runtime error without location.  */
      && ((!optimize && !flag_unreachable_traps)
	  || sanitize_flags_p (SANITIZE_UNREACHABLE, fndecl)))
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
	case CLEANUP_POINT_EXPR:
	  t = TREE_OPERAND (t, 0);
	  continue;
	case STATEMENT_LIST:
	  {
	    tree_stmt_iterator i = tsi_last (t);
	    while (!tsi_end_p (i))
	      {
		tree p = tsi_stmt (i);
		if (TREE_CODE (p) != DEBUG_BEGIN_STMT)
		  break;
		tsi_prev (&i);
	      }
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

  location_t loc = DECL_SOURCE_LOCATION (fndecl);
  if (sanitize_flags_p (SANITIZE_RETURN, fndecl))
    t = ubsan_instrument_return (loc);
  else
    t = build_builtin_unreachable (BUILTINS_LOCATION);

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
  bc_state_t save_state;
  save_bc_state (&save_state);

  /* We do want to see every occurrence of the parms, so we can't just use
     walk_tree's hash functionality.  */
  cp_genericize_tree (&DECL_SAVED_TREE (fndecl), true);

  cp_maybe_instrument_return (fndecl);

  /* Do everything else.  */
  c_genericize (fndecl);
  restore_bc_state (&save_state);
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

  bool is_method = TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE;
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
					   TREE_PURPOSE (parm), fn,
					   i - is_method, tf_warning_or_error);
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
					   TREE_PURPOSE (parm), fn,
					   i - is_method, tf_warning_or_error);
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
  return (TYPE_REF_P (TREE_TYPE (decl))
	  || is_invisiref_parm (decl));
}

/* Return true if DECL is const qualified var having no mutable member.  */
bool
cxx_omp_const_qual_no_mutable (tree decl)
{
  tree type = TREE_TYPE (decl);
  if (TYPE_REF_P (type))
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

/* OMP_CLAUSE_DEFAULT_UNSPECIFIED unless OpenMP sharing attribute
   of DECL is predetermined.  */

enum omp_clause_default_kind
cxx_omp_predetermined_sharing_1 (tree decl)
{
  /* Static data members are predetermined shared.  */
  if (TREE_STATIC (decl))
    {
      tree ctx = CP_DECL_CONTEXT (decl);
      if (TYPE_P (ctx) && MAYBE_CLASS_TYPE_P (ctx))
	return OMP_CLAUSE_DEFAULT_SHARED;

      if (c_omp_predefined_variable (decl))
	return OMP_CLAUSE_DEFAULT_SHARED;
    }

  /* this may not be specified in data-sharing clauses, still we need
     to predetermined it firstprivate.  */
  if (decl == current_class_ptr)
    return OMP_CLAUSE_DEFAULT_FIRSTPRIVATE;

  return OMP_CLAUSE_DEFAULT_UNSPECIFIED;
}

/* Likewise, but also include the artificial vars.  We don't want to
   disallow the artificial vars being mentioned in explicit clauses,
   as we use artificial vars e.g. for loop constructs with random
   access iterators other than pointers, but during gimplification
   we want to treat them as predetermined.  */

enum omp_clause_default_kind
cxx_omp_predetermined_sharing (tree decl)
{
  enum omp_clause_default_kind ret = cxx_omp_predetermined_sharing_1 (decl);
  if (ret != OMP_CLAUSE_DEFAULT_UNSPECIFIED)
    return ret;

  /* Predetermine artificial variables holding integral values, those
     are usually result of gimplify_one_sizepos or SAVE_EXPR
     gimplification.  */
  if (VAR_P (decl)
      && DECL_ARTIFICIAL (decl)
      && INTEGRAL_TYPE_P (TREE_TYPE (decl))
      && !(DECL_LANG_SPECIFIC (decl)
	   && DECL_OMP_PRIVATIZED_MEMBER (decl)))
    return OMP_CLAUSE_DEFAULT_SHARED;

  /* Similarly for typeinfo symbols.  */
  if (VAR_P (decl) && DECL_ARTIFICIAL (decl) && DECL_TINFO_P (decl))
    return OMP_CLAUSE_DEFAULT_SHARED;

  return OMP_CLAUSE_DEFAULT_UNSPECIFIED;
}

enum omp_clause_defaultmap_kind
cxx_omp_predetermined_mapping (tree decl)
{
  /* Predetermine artificial variables holding integral values, those
     are usually result of gimplify_one_sizepos or SAVE_EXPR
     gimplification.  */
  if (VAR_P (decl)
      && DECL_ARTIFICIAL (decl)
      && INTEGRAL_TYPE_P (TREE_TYPE (decl))
      && !(DECL_LANG_SPECIFIC (decl)
	   && DECL_OMP_PRIVATIZED_MEMBER (decl)))
    return OMP_CLAUSE_DEFAULTMAP_FIRSTPRIVATE;

  if (c_omp_predefined_variable (decl))
    return OMP_CLAUSE_DEFAULTMAP_TO;

  return OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED;
}

/* Finalize an implicitly determined clause.  */

void
cxx_omp_finish_clause (tree c, gimple_seq *, bool /* openacc */)
{
  tree decl, inner_type;
  bool make_shared = false;

  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_FIRSTPRIVATE
      && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_PRIVATE
      && (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_LASTPRIVATE
	  || !OMP_CLAUSE_LASTPRIVATE_LOOP_IV (c)))
    return;

  decl = OMP_CLAUSE_DECL (c);
  decl = require_complete_type (decl);
  inner_type = TREE_TYPE (decl);
  if (decl == error_mark_node)
    make_shared = true;
  else if (TYPE_REF_P (TREE_TYPE (decl)))
    inner_type = TREE_TYPE (inner_type);

  /* We're interested in the base element, not arrays.  */
  while (TREE_CODE (inner_type) == ARRAY_TYPE)
    inner_type = TREE_TYPE (inner_type);

  /* Check for special function availability by building a call to one.
     Save the results, because later we won't be in the right context
     for making these queries.  */
  bool first = OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE;
  bool last = OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE;
  if (!make_shared
      && CLASS_TYPE_P (inner_type)
      && cxx_omp_create_clause_info (c, inner_type, !first, first, last,
				     true))
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
  if (shared)
    return false;
  if (VAR_P (decl)
      && DECL_HAS_VALUE_EXPR_P (decl)
      && DECL_ARTIFICIAL (decl)
      && DECL_LANG_SPECIFIC (decl)
      && DECL_OMP_PRIVATIZED_MEMBER (decl))
    return true;
  if (VAR_P (decl) && DECL_CONTEXT (decl) && is_capture_proxy (decl))
    return true;
  return false;
}

/* Fold expression X which is used as an rvalue if RVAL is true.  */

static tree
cp_fold_maybe_rvalue (tree x, bool rval, fold_flags_t flags)
{
  while (true)
    {
      x = cp_fold (x, flags);
      if (rval)
	x = mark_rvalue_use (x);
      if (rval && DECL_P (x)
	  && !TYPE_REF_P (TREE_TYPE (x)))
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

tree
cp_fold_maybe_rvalue (tree x, bool rval)
{
  return cp_fold_maybe_rvalue (x, rval, ff_none);
}

/* Fold expression X which is used as an rvalue.  */

static tree
cp_fold_rvalue (tree x, fold_flags_t flags)
{
  return cp_fold_maybe_rvalue (x, true, flags);
}

tree
cp_fold_rvalue (tree x)
{
  return cp_fold_rvalue (x, ff_none);
}

/* Perform folding on expression X.  */

static tree
cp_fully_fold (tree x, mce_value manifestly_const_eval)
{
  if (processing_template_decl)
    return x;
  /* FIXME cp_fold ought to be a superset of maybe_constant_value so we don't
     have to call both.  */
  if (cxx_dialect >= cxx11)
    {
      x = maybe_constant_value (x, /*decl=*/NULL_TREE, manifestly_const_eval);
      /* Sometimes we are given a CONSTRUCTOR but the call above wraps it into
	 a TARGET_EXPR; undo that here.  */
      if (TREE_CODE (x) == TARGET_EXPR)
	x = TARGET_EXPR_INITIAL (x);
      else if (TREE_CODE (x) == VIEW_CONVERT_EXPR
	       && TREE_CODE (TREE_OPERAND (x, 0)) == CONSTRUCTOR
	       && TREE_TYPE (TREE_OPERAND (x, 0)) == TREE_TYPE (x))
	x = TREE_OPERAND (x, 0);
    }
  fold_flags_t flags = ff_none;
  if (manifestly_const_eval == mce_false)
    flags |= ff_mce_false;
  return cp_fold_rvalue (x, flags);
}

tree
cp_fully_fold (tree x)
{
  return cp_fully_fold (x, mce_unknown);
}

/* Likewise, but also fold recursively, which cp_fully_fold doesn't perform
   in some cases.  */

tree
cp_fully_fold_init (tree x)
{
  if (processing_template_decl)
    return x;
  x = cp_fully_fold (x, mce_false);
  cp_fold_data data (ff_mce_false);
  cp_walk_tree (&x, cp_fold_r, &data, NULL);
  return x;
}

/* c-common interface to cp_fold.  If IN_INIT, this is in a static initializer
   and certain changes are made to the folding done.  Or should be (FIXME).  We
   never touch maybe_const, as it is only used for the C front-end
   C_MAYBE_CONST_EXPR.  */

tree
c_fully_fold (tree x, bool /*in_init*/, bool */*maybe_const*/, bool lval)
{
  return cp_fold_maybe_rvalue (x, !lval);
}

static GTY((deletable)) hash_map<tree, tree> *fold_caches[2];

/* Subroutine of cp_fold.  Returns which fold cache to use according
   to the given flags.  We need multiple caches since the result of
   folding may depend on which flags are used.  */

static hash_map<tree, tree> *&
get_fold_cache (fold_flags_t flags)
{
  if (flags & ff_mce_false)
    return fold_caches[1];
  else
    return fold_caches[0];
}

/* Dispose of the whole FOLD_CACHE.  */

void
clear_fold_cache (void)
{
  for (auto& fold_cache : fold_caches)
    if (fold_cache != NULL)
      fold_cache->empty ();
}

/*  This function tries to fold an expression X.
    To avoid combinatorial explosion, folding results are kept in fold_cache.
    If X is invalid, we don't fold at all.
    For performance reasons we don't cache expressions representing a
    declaration or constant.
    Function returns X or its folded variant.  */

static tree
cp_fold (tree x, fold_flags_t flags)
{
  tree op0, op1, op2, op3;
  tree org_x = x, r = NULL_TREE;
  enum tree_code code;
  location_t loc;
  bool rval_ops = true;

  if (!x || x == error_mark_node)
    return x;

  if (EXPR_P (x) && (!TREE_TYPE (x) || TREE_TYPE (x) == error_mark_node))
    return x;

  /* Don't bother to cache DECLs or constants.  */
  if (DECL_P (x) || CONSTANT_CLASS_P (x))
    return x;

  auto& fold_cache = get_fold_cache (flags);
  if (fold_cache == NULL)
    fold_cache = hash_map<tree, tree>::create_ggc (101);

  if (tree *cached = fold_cache->get (x))
    return *cached;

  uid_sensitive_constexpr_evaluation_checker c;

  code = TREE_CODE (x);
  switch (code)
    {
    case CLEANUP_POINT_EXPR:
      /* Strip CLEANUP_POINT_EXPR if the expression doesn't have side
	 effects.  */
      r = cp_fold_rvalue (TREE_OPERAND (x, 0), flags);
      if (!TREE_SIDE_EFFECTS (r))
	x = r;
      break;

    case SIZEOF_EXPR:
      x = fold_sizeof_expr (x);
      break;

    case VIEW_CONVERT_EXPR:
      rval_ops = false;
      /* FALLTHRU */
    case NON_LVALUE_EXPR:
    CASE_CONVERT:

      if (VOID_TYPE_P (TREE_TYPE (x)))
	{
	  /* This is just to make sure we don't end up with casts to
	     void from error_mark_node.  If we just return x, then
	     cp_fold_r might fold the operand into error_mark_node and
	     leave the conversion in the IR.  STRIP_USELESS_TYPE_CONVERSION
	     during gimplification doesn't like such casts.
	     Don't create a new tree if op0 != TREE_OPERAND (x, 0), the
	     folding of the operand should be in the caches and if in cp_fold_r
	     it will modify it in place.  */
	  op0 = cp_fold (TREE_OPERAND (x, 0), flags);
	  if (op0 == error_mark_node)
	    x = error_mark_node;
	  break;
	}

      loc = EXPR_LOCATION (x);
      op0 = cp_fold_maybe_rvalue (TREE_OPERAND (x, 0), rval_ops, flags);

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

    case EXCESS_PRECISION_EXPR:
      op0 = cp_fold_maybe_rvalue (TREE_OPERAND (x, 0), rval_ops, flags);
      x = fold_convert_loc (EXPR_LOCATION (x), TREE_TYPE (x), op0);
      break;

    case INDIRECT_REF:
      /* We don't need the decltype(auto) obfuscation anymore.  */
      if (REF_PARENTHESIZED_P (x))
	{
	  tree p = maybe_undo_parenthesized_ref (x);
	  if (p != x)
	    return cp_fold (p, flags);
	}
      goto unary;

    case ADDR_EXPR:
      loc = EXPR_LOCATION (x);
      op0 = cp_fold_maybe_rvalue (TREE_OPERAND (x, 0), false, flags);

      /* Cope with user tricks that amount to offsetof.  */
      if (op0 != error_mark_node
	  && !FUNC_OR_METHOD_TYPE_P (TREE_TYPE (op0)))
	{
	  tree val = get_base_address (op0);
	  if (val
	      && INDIRECT_REF_P (val)
	      && COMPLETE_TYPE_P (TREE_TYPE (val))
	      && TREE_CONSTANT (TREE_OPERAND (val, 0)))
	    {
	      val = TREE_OPERAND (val, 0);
	      STRIP_NOPS (val);
	      val = maybe_constant_value (val);
	      if (TREE_CODE (val) == INTEGER_CST)
		return fold_offsetof (op0, TREE_TYPE (x));
	    }
	}
      goto finish_unary;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      rval_ops = false;
      /* FALLTHRU */
    case CONJ_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case ABSU_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
    unary:

      loc = EXPR_LOCATION (x);
      op0 = cp_fold_maybe_rvalue (TREE_OPERAND (x, 0), rval_ops, flags);

    finish_unary:
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
      op0 = cp_fold_rvalue (TREE_OPERAND (x, 0), flags);
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
    case POINTER_DIFF_EXPR:
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
      op0 = cp_fold_maybe_rvalue (TREE_OPERAND (x, 0), rval_ops, flags);
      op1 = cp_fold_rvalue (TREE_OPERAND (x, 1), flags);

      /* decltype(nullptr) has only one value, so optimize away all comparisons
	 with that type right away, keeping them in the IL causes troubles for
	 various optimizations.  */
      if (COMPARISON_CLASS_P (org_x)
	  && TREE_CODE (TREE_TYPE (op0)) == NULLPTR_TYPE
	  && TREE_CODE (TREE_TYPE (op1)) == NULLPTR_TYPE)
	{
	  switch (code)
	    {
	    case EQ_EXPR:
	      x = constant_boolean_node (true, TREE_TYPE (x));
	      break;
	    case NE_EXPR:
	      x = constant_boolean_node (false, TREE_TYPE (x));
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  return omit_two_operands_loc (loc, TREE_TYPE (x), x,
					op0, op1);
	}

      if (op0 != TREE_OPERAND (x, 0) || op1 != TREE_OPERAND (x, 1))
	{
	  if (op0 == error_mark_node || op1 == error_mark_node)
	    x = error_mark_node;
	  else
	    x = fold_build2_loc (loc, code, TREE_TYPE (x), op0, op1);
	}
      else
	x = fold (x);

      /* This is only needed for -Wnonnull-compare and only if
	 TREE_NO_WARNING (org_x), but to avoid that option affecting code
	 generation, we do it always.  */
      if (COMPARISON_CLASS_P (org_x))
	{
	  if (x == error_mark_node || TREE_CODE (x) == INTEGER_CST)
	    ;
	  else if (COMPARISON_CLASS_P (x))
	    {
	      if (warn_nonnull_compare
		  && warning_suppressed_p (org_x, OPT_Wnonnull_compare))
		suppress_warning (x, OPT_Wnonnull_compare);
	    }
	  /* Otherwise give up on optimizing these, let GIMPLE folders
	     optimize those later on.  */
	  else if (op0 != TREE_OPERAND (org_x, 0)
		   || op1 != TREE_OPERAND (org_x, 1))
	    {
	      x = build2_loc (loc, code, TREE_TYPE (org_x), op0, op1);
	      if (warn_nonnull_compare
		  && warning_suppressed_p (org_x, OPT_Wnonnull_compare))
		suppress_warning (x, OPT_Wnonnull_compare);
	    }
	  else
	    x = org_x;
	}

      break;

    case VEC_COND_EXPR:
    case COND_EXPR:
      loc = EXPR_LOCATION (x);
      op0 = cp_fold_rvalue (TREE_OPERAND (x, 0), flags);
      op1 = cp_fold (TREE_OPERAND (x, 1), flags);
      op2 = cp_fold (TREE_OPERAND (x, 2), flags);

      if (TREE_CODE (TREE_TYPE (x)) == BOOLEAN_TYPE)
	{
	  warning_sentinel s (warn_int_in_bool_context);
	  if (!VOID_TYPE_P (TREE_TYPE (op1)))
	    op1 = cp_truthvalue_conversion (op1, tf_warning_or_error);
	  if (!VOID_TYPE_P (TREE_TYPE (op2)))
	    op2 = cp_truthvalue_conversion (op2, tf_warning_or_error);
	}
      else if (VOID_TYPE_P (TREE_TYPE (x)))
	{
	  if (TREE_CODE (op0) == INTEGER_CST)
	    {
	      /* If the condition is constant, fold can fold away
		 the COND_EXPR.  If some statement-level uses of COND_EXPR
		 have one of the branches NULL, avoid folding crash.  */
	      if (!op1)
		op1 = build_empty_stmt (loc);
	      if (!op2)
		op2 = build_empty_stmt (loc);
	    }
	  else
	    {
	      /* Otherwise, don't bother folding a void condition, since
		 it can't produce a constant value.  */
	      if (op0 != TREE_OPERAND (x, 0)
		  || op1 != TREE_OPERAND (x, 1)
		  || op2 != TREE_OPERAND (x, 2))
		x = build3_loc (loc, code, TREE_TYPE (x), op0, op1, op2);
	      break;
	    }
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
	  && x != error_mark_node
	  && !useless_type_conversion_p (TREE_TYPE (org_x), TREE_TYPE (x)))
	x = fold_convert (TREE_TYPE (org_x), x);

      break;

    case CALL_EXPR:
      {
	tree callee = get_callee_fndecl (x);

	/* "Inline" calls to std::move/forward and other cast-like functions
	   by simply folding them into a corresponding cast to their return
	   type.  This is cheaper than relying on the middle end to do so, and
	   also means we avoid generating useless debug info for them at all.

	   At this point the argument has already been converted into a
	   reference, so it suffices to use a NOP_EXPR to express the
	   cast.  */
	if ((OPTION_SET_P (flag_fold_simple_inlines)
	     ? flag_fold_simple_inlines
	     : !flag_no_inline)
	    && call_expr_nargs (x) == 1
	    && decl_in_std_namespace_p (callee)
	    && DECL_NAME (callee) != NULL_TREE
	    && (id_equal (DECL_NAME (callee), "move")
		|| id_equal (DECL_NAME (callee), "forward")
		|| id_equal (DECL_NAME (callee), "addressof")
		/* This addressof equivalent is used heavily in libstdc++.  */
		|| id_equal (DECL_NAME (callee), "__addressof")
		|| id_equal (DECL_NAME (callee), "as_const")))
	  {
	    r = CALL_EXPR_ARG (x, 0);
	    /* Check that the return and argument types are sane before
	       folding.  */
	    if (INDIRECT_TYPE_P (TREE_TYPE (x))
		&& INDIRECT_TYPE_P (TREE_TYPE (r)))
	      {
		if (!same_type_p (TREE_TYPE (x), TREE_TYPE (r)))
		  r = build_nop (TREE_TYPE (x), r);
		x = cp_fold (r, flags);
		break;
	      }
	  }

	int sv = optimize, nw = sv;

	/* Some built-in function calls will be evaluated at compile-time in
	   fold ().  Set optimize to 1 when folding __builtin_constant_p inside
	   a constexpr function so that fold_builtin_1 doesn't fold it to 0.  */
	if (callee && fndecl_built_in_p (callee) && !optimize
	    && DECL_IS_BUILTIN_CONSTANT_P (callee)
	    && current_function_decl
	    && DECL_DECLARED_CONSTEXPR_P (current_function_decl))
	  nw = 1;

	if (callee && fndecl_built_in_p (callee, BUILT_IN_FRONTEND))
	  {
	    iloc_sentinel ils (EXPR_LOCATION (x));
	    switch (DECL_FE_FUNCTION_CODE (callee))
	      {
	      case CP_BUILT_IN_IS_CONSTANT_EVALUATED:
		/* Defer folding __builtin_is_constant_evaluated unless
		   we know this isn't a manifestly constant-evaluated
		   context.  */
		if (flags & ff_mce_false)
		  x = boolean_false_node;
		break;
	      case CP_BUILT_IN_SOURCE_LOCATION:
		x = fold_builtin_source_location (x);
		break;
	      case CP_BUILT_IN_IS_CORRESPONDING_MEMBER:
	        x = fold_builtin_is_corresponding_member
			(EXPR_LOCATION (x), call_expr_nargs (x),
			 &CALL_EXPR_ARG (x, 0));
		break;
	      case CP_BUILT_IN_IS_POINTER_INTERCONVERTIBLE_WITH_CLASS:
                x = fold_builtin_is_pointer_inverconvertible_with_class
			(EXPR_LOCATION (x), call_expr_nargs (x),
			 &CALL_EXPR_ARG (x, 0));
		break;
	      default:
		break;
	      }
	    break;
	  }

	if (callee
	    && fndecl_built_in_p (callee, CP_BUILT_IN_SOURCE_LOCATION,
				  BUILT_IN_FRONTEND))
	  {
	    x = fold_builtin_source_location (x);
	    break;
	  }

	bool changed = false;
	int m = call_expr_nargs (x);
	for (int i = 0; i < m; i++)
	  {
	    r = cp_fold (CALL_EXPR_ARG (x, i), flags);
	    if (r != CALL_EXPR_ARG (x, i))
	      {
		if (r == error_mark_node)
		  {
		    x = error_mark_node;
		    break;
		  }
		if (!changed)
		  x = copy_node (x);
		CALL_EXPR_ARG (x, i) = r;
		changed = true;
	      }
	  }
	if (x == error_mark_node)
	  break;

	optimize = nw;
	r = fold (x);
	optimize = sv;

	if (TREE_CODE (r) != CALL_EXPR)
	  {
	    x = cp_fold (r, flags);
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
	  {
	    mce_value manifestly_const_eval = mce_unknown;
	    if (flags & ff_mce_false)
	      /* Allow folding __builtin_is_constant_evaluated to false during
		 constexpr evaluation of this call.  */
	      manifestly_const_eval = mce_false;
	    r = maybe_constant_value (x, /*decl=*/NULL_TREE,
				      manifestly_const_eval);
	  }
	optimize = sv;

        if (TREE_CODE (r) != CALL_EXPR)
	  {
	    if (DECL_CONSTRUCTOR_P (callee))
	      {
		loc = EXPR_LOCATION (x);
		tree s = build_fold_indirect_ref_loc (loc,
						      CALL_EXPR_ARG (x, 0));
		r = cp_build_init_expr (s, r);
	      }
	    x = r;
	    break;
	  }

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
	    tree op = cp_fold (p->value, flags);
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
	  {
	    x = build_constructor (TREE_TYPE (x), nelts);
	    CONSTRUCTOR_PLACEHOLDER_BOUNDARY (x)
	      = CONSTRUCTOR_PLACEHOLDER_BOUNDARY (org_x);
	    CONSTRUCTOR_MUTABLE_POISON (x)
	      = CONSTRUCTOR_MUTABLE_POISON (org_x);
	  }
	if (VECTOR_TYPE_P (TREE_TYPE (x)))
	  x = fold (x);
	break;
      }
    case TREE_VEC:
      {
	bool changed = false;
	int n = TREE_VEC_LENGTH (x);

	for (int i = 0; i < n; i++)
	  {
	    tree op = cp_fold (TREE_VEC_ELT (x, i), flags);
	    if (op != TREE_VEC_ELT (x, i))
	      {
		if (!changed)
		  x = copy_node (x);
		TREE_VEC_ELT (x, i) = op;
		changed = true;
	      }
	  }
      }

      break;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:

      loc = EXPR_LOCATION (x);
      op0 = cp_fold (TREE_OPERAND (x, 0), flags);
      op1 = cp_fold (TREE_OPERAND (x, 1), flags);
      op2 = cp_fold (TREE_OPERAND (x, 2), flags);
      op3 = cp_fold (TREE_OPERAND (x, 3), flags);

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

    case SAVE_EXPR:
      /* A SAVE_EXPR might contain e.g. (0 * i) + (0 * j), which, after
	 folding, evaluates to an invariant.  In that case no need to wrap
	 this folded tree with a SAVE_EXPR.  */
      r = cp_fold (TREE_OPERAND (x, 0), flags);
      if (tree_invariant_p (r))
	x = r;
      break;

    case REQUIRES_EXPR:
      x = evaluate_requires_expr (x);
      break;

    default:
      return org_x;
    }

  if (EXPR_P (x) && TREE_CODE (x) == code)
    {
      TREE_THIS_VOLATILE (x) = TREE_THIS_VOLATILE (org_x);
      copy_warning (x, org_x);
    }

  if (!c.evaluation_restricted_p ())
    {
      fold_cache->put (org_x, x);
      /* Prevent that we try to fold an already folded result again.  */
      if (x != org_x)
	fold_cache->put (x, x);
    }

  return x;
}

/* Look up "hot", "cold", "likely" or "unlikely" in attribute list LIST.  */

tree
lookup_hotness_attribute (tree list)
{
  for (; list; list = TREE_CHAIN (list))
    {
      tree name = get_attribute_name (list);
      if ((is_attribute_p ("hot", name)
	   || is_attribute_p ("cold", name)
	   || is_attribute_p ("likely", name)
	   || is_attribute_p ("unlikely", name))
	  && is_attribute_namespace_p ("", list))
	break;
    }
  return list;
}

/* Remove "hot", "cold", "likely" and "unlikely" attributes from LIST.  */

static tree
remove_hotness_attribute (tree list)
{
  for (tree *p = &list; *p; )
    {
      tree l = *p;
      tree name = get_attribute_name (l);
      if ((is_attribute_p ("hot", name)
	   || is_attribute_p ("cold", name)
	   || is_attribute_p ("likely", name)
	   || is_attribute_p ("unlikely", name))
	  && is_attribute_namespace_p ("", l))
	{
	  *p = TREE_CHAIN (l);
	  continue;
	}
      p = &TREE_CHAIN (l);
    }
  return list;
}

/* If [[likely]] or [[unlikely]] appear on this statement, turn it into a
   PREDICT_EXPR.  */

tree
process_stmt_hotness_attribute (tree std_attrs, location_t attrs_loc)
{
  if (std_attrs == error_mark_node)
    return std_attrs;
  if (tree attr = lookup_hotness_attribute (std_attrs))
    {
      tree name = get_attribute_name (attr);
      bool hot = (is_attribute_p ("hot", name)
		  || is_attribute_p ("likely", name));
      tree pred = build_predict_expr (hot ? PRED_HOT_LABEL : PRED_COLD_LABEL,
				      hot ? TAKEN : NOT_TAKEN);
      SET_EXPR_LOCATION (pred, attrs_loc);
      add_stmt (pred);
      if (tree other = lookup_hotness_attribute (TREE_CHAIN (attr)))
	warning (OPT_Wattributes, "ignoring attribute %qE after earlier %qE",
		 get_attribute_name (other), name);
      std_attrs = remove_hotness_attribute (std_attrs);
    }
  return std_attrs;
}

/* Build IFN_ASSUME internal call for assume condition ARG.  */

tree
build_assume_call (location_t loc, tree arg)
{
  if (!processing_template_decl)
    arg = fold_build_cleanup_point_expr (TREE_TYPE (arg), arg);
  return build_call_expr_internal_loc (loc, IFN_ASSUME, void_type_node,
				       1, arg);
}

/* If [[assume (cond)]] appears on this statement, handle it.  */

tree
process_stmt_assume_attribute (tree std_attrs, tree statement,
			       location_t attrs_loc)
{
  if (std_attrs == error_mark_node)
    return std_attrs;
  tree attr = lookup_attribute ("gnu", "assume", std_attrs);
  if (!attr)
    return std_attrs;
  /* The next token after the assume attribute is not ';'.  */
  if (statement)
    {
      warning_at (attrs_loc, OPT_Wattributes,
		  "%<assume%> attribute not followed by %<;%>");
      attr = NULL_TREE;
    }
  for (; attr; attr = lookup_attribute ("gnu", "assume", TREE_CHAIN (attr)))
    {
      tree args = TREE_VALUE (attr);
      if (args && PACK_EXPANSION_P (args))
	{
	  auto_diagnostic_group d;
	  error_at (attrs_loc, "pack expansion of %qE attribute",
		    get_attribute_name (attr));
	  if (cxx_dialect >= cxx17)
	    inform (attrs_loc, "use fold expression in the attribute "
			       "argument instead");
	  continue;
	}
      int nargs = list_length (args);
      if (nargs != 1)
	{
	  auto_diagnostic_group d;
	  error_at (attrs_loc, "wrong number of arguments specified for "
			       "%qE attribute", get_attribute_name (attr));
	  inform (attrs_loc, "expected %i, found %i", 1, nargs);
	}
      else
	{
	  tree arg = TREE_VALUE (args);
	  if (!type_dependent_expression_p (arg))
	    arg = contextual_conv_bool (arg, tf_warning_or_error);
	  if (error_operand_p (arg))
	    continue;
	  finish_expr_stmt (build_assume_call (attrs_loc, arg));
	}
    }
  return remove_attribute ("gnu", "assume", std_attrs);
}

/* Return the type std::source_location::__impl after performing
   verification on it.  */

tree
get_source_location_impl_type ()
{
  tree name = get_identifier ("source_location");
  tree decl = lookup_qualified_name (std_node, name);
  if (TREE_CODE (decl) != TYPE_DECL)
    {
      auto_diagnostic_group d;
      if (decl == error_mark_node || TREE_CODE (decl) == TREE_LIST)
	qualified_name_lookup_error (std_node, name, decl, input_location);
      else
	error ("%qD is not a type", decl);
      return error_mark_node;
    }
  name = get_identifier ("__impl");
  tree type = TREE_TYPE (decl);
  decl = lookup_qualified_name (type, name);
  if (TREE_CODE (decl) != TYPE_DECL)
    {
      auto_diagnostic_group d;
      if (decl == error_mark_node || TREE_CODE (decl) == TREE_LIST)
	qualified_name_lookup_error (type, name, decl, input_location);
      else
	error ("%qD is not a type", decl);
      return error_mark_node;
    }
  type = TREE_TYPE (decl);
  if (TREE_CODE (type) != RECORD_TYPE)
    {
      error ("%qD is not a class type", decl);
      return error_mark_node;
    }

  int cnt = 0;
  for (tree field = TYPE_FIELDS (type);
       (field = next_aggregate_field (field)) != NULL_TREE;
       field = DECL_CHAIN (field))
    {
      if (DECL_NAME (field) != NULL_TREE)
	{
	  const char *n = IDENTIFIER_POINTER (DECL_NAME (field));
	  if (strcmp (n, "_M_file_name") == 0
	      || strcmp (n, "_M_function_name") == 0)
	    {
	      if (TREE_TYPE (field) != const_string_type_node)
		{
		  error ("%qD does not have %<const char *%> type", field);
		  return error_mark_node;
		}
	      cnt++;
	      continue;
	    }
	  else if (strcmp (n, "_M_line") == 0 || strcmp (n, "_M_column") == 0)
	    {
	      if (TREE_CODE (TREE_TYPE (field)) != INTEGER_TYPE)
		{
		  error ("%qD does not have integral type", field);
		  return error_mark_node;
		}
	      cnt++;
	      continue;
	    }
	}
      cnt = 0;
      break;
    }
  if (cnt != 4)
    {
      error ("%<std::source_location::__impl%> does not contain only "
	     "non-static data members %<_M_file_name%>, "
	     "%<_M_function_name%>, %<_M_line%> and %<_M_column%>");
      return error_mark_node;
    }
  return build_qualified_type (type, TYPE_QUAL_CONST);
}

/* Type for source_location_table hash_set.  */
struct GTY((for_user)) source_location_table_entry {
  location_t loc;
  unsigned uid;
  tree var;
};

/* Traits class for function start hash maps below.  */

struct source_location_table_entry_hash
  : ggc_remove <source_location_table_entry>
{
  typedef source_location_table_entry value_type;
  typedef source_location_table_entry compare_type;

  static hashval_t
  hash (const source_location_table_entry &ref)
  {
    inchash::hash hstate (0);
    hstate.add_int (ref.loc);
    hstate.add_int (ref.uid);
    return hstate.end ();
  }

  static bool
  equal (const source_location_table_entry &ref1,
	 const source_location_table_entry &ref2)
  {
    return ref1.loc == ref2.loc && ref1.uid == ref2.uid;
  }

  static void
  mark_deleted (source_location_table_entry &ref)
  {
    ref.loc = UNKNOWN_LOCATION;
    ref.uid = -1U;
    ref.var = NULL_TREE;
  }

  static const bool empty_zero_p = true;

  static void
  mark_empty (source_location_table_entry &ref)
  {
    ref.loc = UNKNOWN_LOCATION;
    ref.uid = 0;
    ref.var = NULL_TREE;
  }

  static bool
  is_deleted (const source_location_table_entry &ref)
  {
    return (ref.loc == UNKNOWN_LOCATION
	    && ref.uid == -1U
	    && ref.var == NULL_TREE);
  }

  static bool
  is_empty (const source_location_table_entry &ref)
  {
    return (ref.loc == UNKNOWN_LOCATION
	    && ref.uid == 0
	    && ref.var == NULL_TREE);
  }

  static void
  pch_nx (source_location_table_entry &p)
  {
    extern void gt_pch_nx (source_location_table_entry &);
    gt_pch_nx (p);
  }

  static void
  pch_nx (source_location_table_entry &p, gt_pointer_operator op, void *cookie)
  {
    extern void gt_pch_nx (source_location_table_entry *, gt_pointer_operator,
			   void *);
    gt_pch_nx (&p, op, cookie);
  }
};

static GTY(()) hash_table <source_location_table_entry_hash>
  *source_location_table;
static GTY(()) unsigned int source_location_id;

/* Fold the __builtin_source_location () call T.  */

tree
fold_builtin_source_location (const_tree t)
{
  gcc_assert (TREE_CODE (t) == CALL_EXPR);
  /* TREE_TYPE (t) is const std::source_location::__impl*  */
  tree source_location_impl = TREE_TYPE (TREE_TYPE (t));
  if (source_location_impl == error_mark_node)
    return build_zero_cst (const_ptr_type_node);
  gcc_assert (CLASS_TYPE_P (source_location_impl)
	      && id_equal (TYPE_IDENTIFIER (source_location_impl), "__impl"));

  location_t loc = EXPR_LOCATION (t);
  if (source_location_table == NULL)
    source_location_table
      = hash_table <source_location_table_entry_hash>::create_ggc (64);
  const line_map_ordinary *map;
  source_location_table_entry entry;
  entry.loc
    = linemap_resolve_location (line_table, loc, LRK_MACRO_EXPANSION_POINT,
				&map);
  entry.uid = current_function_decl ? DECL_UID (current_function_decl) : -1;
  entry.var = error_mark_node;
  source_location_table_entry *entryp
    = source_location_table->find_slot (entry, INSERT);
  tree var;
  if (entryp->var)
    var = entryp->var;
  else
    {
      char tmp_name[32];
      ASM_GENERATE_INTERNAL_LABEL (tmp_name, "Lsrc_loc", source_location_id++);
      var = build_decl (loc, VAR_DECL, get_identifier (tmp_name),
			source_location_impl);
      TREE_STATIC (var) = 1;
      TREE_PUBLIC (var) = 0;
      DECL_ARTIFICIAL (var) = 1;
      DECL_IGNORED_P (var) = 1;
      DECL_EXTERNAL (var) = 0;
      DECL_DECLARED_CONSTEXPR_P (var) = 1;
      DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (var) = 1;
      layout_decl (var, 0);

      vec<constructor_elt, va_gc> *v = NULL;
      vec_alloc (v, 4);
      for (tree field = TYPE_FIELDS (source_location_impl);
	   (field = next_aggregate_field (field)) != NULL_TREE;
	   field = DECL_CHAIN (field))
	{
	  const char *n = IDENTIFIER_POINTER (DECL_NAME (field));
	  tree val = NULL_TREE;
	  if (strcmp (n, "_M_file_name") == 0)
	    {
	      if (const char *fname = LOCATION_FILE (loc))
		{
		  fname = remap_macro_filename (fname);
		  val = build_string_literal (fname);
		}
	      else
		val = build_string_literal ("");
	    }
	  else if (strcmp (n, "_M_function_name") == 0)
	    {
	      const char *name = "";

	      if (current_function_decl)
		name = cxx_printable_name (current_function_decl, 2);

	      val = build_string_literal (name);
	    }
	  else if (strcmp (n, "_M_line") == 0)
	    val = build_int_cst (TREE_TYPE (field), LOCATION_LINE (loc));
	  else if (strcmp (n, "_M_column") == 0)
	    val = build_int_cst (TREE_TYPE (field), LOCATION_COLUMN (loc));
	  else
	    gcc_unreachable ();
	  CONSTRUCTOR_APPEND_ELT (v, field, val);
	}

      tree ctor = build_constructor (source_location_impl, v);
      TREE_CONSTANT (ctor) = 1;
      TREE_STATIC (ctor) = 1;
      DECL_INITIAL (var) = ctor;
      varpool_node::finalize_decl (var);
      *entryp = entry;
      entryp->var = var;
    }

  return build_fold_addr_expr_with_type_loc (loc, var, TREE_TYPE (t));
}

#include "gt-cp-cp-gimplify.h"
