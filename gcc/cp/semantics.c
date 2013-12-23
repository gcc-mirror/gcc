/* Perform the semantic phase of parsing, i.e., the process of
   building tree structure, checking semantic consistency, and
   building RTL.  These routines are used both during actual parsing
   and during the instantiation of template functions.

   Copyright (C) 1998-2013 Free Software Foundation, Inc.
   Written by Mark Mitchell (mmitchell@usa.net) based on code found
   formerly in parse.y and pt.c.

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
#include "tm.h"
#include "tree.h"
#include "stmt.h"
#include "varasm.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "cp-tree.h"
#include "c-family/c-common.h"
#include "c-family/c-objc.h"
#include "tree-inline.h"
#include "intl.h"
#include "toplev.h"
#include "flags.h"
#include "timevar.h"
#include "diagnostic.h"
#include "cgraph.h"
#include "tree-iterator.h"
#include "target.h"
#include "pointer-set.h"
#include "hash-table.h"
#include "gimplify.h"
#include "bitmap.h"
#include "omp-low.h"

static bool verify_constant (tree, bool, bool *, bool *);
#define VERIFY_CONSTANT(X)						\
do {									\
  if (verify_constant ((X), allow_non_constant, non_constant_p, overflow_p)) \
    return t;								\
 } while (0)

/* There routines provide a modular interface to perform many parsing
   operations.  They may therefore be used during actual parsing, or
   during template instantiation, which may be regarded as a
   degenerate form of parsing.  */

static tree maybe_convert_cond (tree);
static tree finalize_nrv_r (tree *, int *, void *);
static tree capture_decltype (tree);


/* Deferred Access Checking Overview
   ---------------------------------

   Most C++ expressions and declarations require access checking
   to be performed during parsing.  However, in several cases,
   this has to be treated differently.

   For member declarations, access checking has to be deferred
   until more information about the declaration is known.  For
   example:

     class A {
	 typedef int X;
       public:
	 X f();
     };

     A::X A::f();
     A::X g();

   When we are parsing the function return type `A::X', we don't
   really know if this is allowed until we parse the function name.

   Furthermore, some contexts require that access checking is
   never performed at all.  These include class heads, and template
   instantiations.

   Typical use of access checking functions is described here:

   1. When we enter a context that requires certain access checking
      mode, the function `push_deferring_access_checks' is called with
      DEFERRING argument specifying the desired mode.  Access checking
      may be performed immediately (dk_no_deferred), deferred
      (dk_deferred), or not performed (dk_no_check).

   2. When a declaration such as a type, or a variable, is encountered,
      the function `perform_or_defer_access_check' is called.  It
      maintains a vector of all deferred checks.

   3. The global `current_class_type' or `current_function_decl' is then
      setup by the parser.  `enforce_access' relies on these information
      to check access.

   4. Upon exiting the context mentioned in step 1,
      `perform_deferred_access_checks' is called to check all declaration
      stored in the vector. `pop_deferring_access_checks' is then
      called to restore the previous access checking mode.

      In case of parsing error, we simply call `pop_deferring_access_checks'
      without `perform_deferred_access_checks'.  */

typedef struct GTY(()) deferred_access {
  /* A vector representing name-lookups for which we have deferred
     checking access controls.  We cannot check the accessibility of
     names used in a decl-specifier-seq until we know what is being
     declared because code like:

       class A {
	 class B {};
	 B* f();
       }

       A::B* A::f() { return 0; }

     is valid, even though `A::B' is not generally accessible.  */
  vec<deferred_access_check, va_gc> * GTY(()) deferred_access_checks;

  /* The current mode of access checks.  */
  enum deferring_kind deferring_access_checks_kind;

} deferred_access;

/* Data for deferred access checking.  */
static GTY(()) vec<deferred_access, va_gc> *deferred_access_stack;
static GTY(()) unsigned deferred_access_no_check;

/* Save the current deferred access states and start deferred
   access checking iff DEFER_P is true.  */

void
push_deferring_access_checks (deferring_kind deferring)
{
  /* For context like template instantiation, access checking
     disabling applies to all nested context.  */
  if (deferred_access_no_check || deferring == dk_no_check)
    deferred_access_no_check++;
  else
    {
      deferred_access e = {NULL, deferring};
      vec_safe_push (deferred_access_stack, e);
    }
}

/* Save the current deferred access states and start deferred access
   checking, continuing the set of deferred checks in CHECKS.  */

void
reopen_deferring_access_checks (vec<deferred_access_check, va_gc> * checks)
{
  push_deferring_access_checks (dk_deferred);
  if (!deferred_access_no_check)
    deferred_access_stack->last().deferred_access_checks = checks;
}

/* Resume deferring access checks again after we stopped doing
   this previously.  */

void
resume_deferring_access_checks (void)
{
  if (!deferred_access_no_check)
    deferred_access_stack->last().deferring_access_checks_kind = dk_deferred;
}

/* Stop deferring access checks.  */

void
stop_deferring_access_checks (void)
{
  if (!deferred_access_no_check)
    deferred_access_stack->last().deferring_access_checks_kind = dk_no_deferred;
}

/* Discard the current deferred access checks and restore the
   previous states.  */

void
pop_deferring_access_checks (void)
{
  if (deferred_access_no_check)
    deferred_access_no_check--;
  else
    deferred_access_stack->pop ();
}

/* Returns a TREE_LIST representing the deferred checks.
   The TREE_PURPOSE of each node is the type through which the
   access occurred; the TREE_VALUE is the declaration named.
   */

vec<deferred_access_check, va_gc> *
get_deferred_access_checks (void)
{
  if (deferred_access_no_check)
    return NULL;
  else
    return (deferred_access_stack->last().deferred_access_checks);
}

/* Take current deferred checks and combine with the
   previous states if we also defer checks previously.
   Otherwise perform checks now.  */

void
pop_to_parent_deferring_access_checks (void)
{
  if (deferred_access_no_check)
    deferred_access_no_check--;
  else
    {
      vec<deferred_access_check, va_gc> *checks;
      deferred_access *ptr;

      checks = (deferred_access_stack->last ().deferred_access_checks);

      deferred_access_stack->pop ();
      ptr = &deferred_access_stack->last ();
      if (ptr->deferring_access_checks_kind == dk_no_deferred)
	{
	  /* Check access.  */
	  perform_access_checks (checks, tf_warning_or_error);
	}
      else
	{
	  /* Merge with parent.  */
	  int i, j;
	  deferred_access_check *chk, *probe;

	  FOR_EACH_VEC_SAFE_ELT (checks, i, chk)
	    {
	      FOR_EACH_VEC_SAFE_ELT (ptr->deferred_access_checks, j, probe)
		{
		  if (probe->binfo == chk->binfo &&
		      probe->decl == chk->decl &&
		      probe->diag_decl == chk->diag_decl)
		    goto found;
		}
	      /* Insert into parent's checks.  */
	      vec_safe_push (ptr->deferred_access_checks, *chk);
	    found:;
	    }
	}
    }
}

/* Perform the access checks in CHECKS.  The TREE_PURPOSE of each node
   is the BINFO indicating the qualifying scope used to access the
   DECL node stored in the TREE_VALUE of the node.  If CHECKS is empty
   or we aren't in SFINAE context or all the checks succeed return TRUE,
   otherwise FALSE.  */

bool
perform_access_checks (vec<deferred_access_check, va_gc> *checks,
		       tsubst_flags_t complain)
{
  int i;
  deferred_access_check *chk;
  location_t loc = input_location;
  bool ok = true;

  if (!checks)
    return true;

  FOR_EACH_VEC_SAFE_ELT (checks, i, chk)
    {
      input_location = chk->loc;
      ok &= enforce_access (chk->binfo, chk->decl, chk->diag_decl, complain);
    }

  input_location = loc;
  return (complain & tf_error) ? true : ok;
}

/* Perform the deferred access checks.

   After performing the checks, we still have to keep the list
   `deferred_access_stack->deferred_access_checks' since we may want
   to check access for them again later in a different context.
   For example:

     class A {
       typedef int X;
       static X a;
     };
     A::X A::a, x;	// No error for `A::a', error for `x'

   We have to perform deferred access of `A::X', first with `A::a',
   next with `x'.  Return value like perform_access_checks above.  */

bool
perform_deferred_access_checks (tsubst_flags_t complain)
{
  return perform_access_checks (get_deferred_access_checks (), complain);
}

/* Defer checking the accessibility of DECL, when looked up in
   BINFO. DIAG_DECL is the declaration to use to print diagnostics.
   Return value like perform_access_checks above.  */

bool
perform_or_defer_access_check (tree binfo, tree decl, tree diag_decl,
			       tsubst_flags_t complain)
{
  int i;
  deferred_access *ptr;
  deferred_access_check *chk;


  /* Exit if we are in a context that no access checking is performed.
     */
  if (deferred_access_no_check)
    return true;

  gcc_assert (TREE_CODE (binfo) == TREE_BINFO);

  ptr = &deferred_access_stack->last ();

  /* If we are not supposed to defer access checks, just check now.  */
  if (ptr->deferring_access_checks_kind == dk_no_deferred)
    {
      bool ok = enforce_access (binfo, decl, diag_decl, complain);
      return (complain & tf_error) ? true : ok;
    }

  /* See if we are already going to perform this check.  */
  FOR_EACH_VEC_SAFE_ELT (ptr->deferred_access_checks, i, chk)
    {
      if (chk->decl == decl && chk->binfo == binfo &&
	  chk->diag_decl == diag_decl)
	{
	  return true;
	}
    }
  /* If not, record the check.  */
  deferred_access_check new_access = {binfo, decl, diag_decl, input_location};
  vec_safe_push (ptr->deferred_access_checks, new_access);

  return true;
}

/* Returns nonzero if the current statement is a full expression,
   i.e. temporaries created during that statement should be destroyed
   at the end of the statement.  */

int
stmts_are_full_exprs_p (void)
{
  return current_stmt_tree ()->stmts_are_full_exprs_p;
}

/* T is a statement.  Add it to the statement-tree.  This is the C++
   version.  The C/ObjC frontends have a slightly different version of
   this function.  */

tree
add_stmt (tree t)
{
  enum tree_code code = TREE_CODE (t);

  if (EXPR_P (t) && code != LABEL_EXPR)
    {
      if (!EXPR_HAS_LOCATION (t))
	SET_EXPR_LOCATION (t, input_location);

      /* When we expand a statement-tree, we must know whether or not the
	 statements are full-expressions.  We record that fact here.  */
      STMT_IS_FULL_EXPR_P (t) = stmts_are_full_exprs_p ();
    }

  /* Add T to the statement-tree.  Non-side-effect statements need to be
     recorded during statement expressions.  */
  gcc_checking_assert (!stmt_list_stack->is_empty ());
  append_to_statement_list_force (t, &cur_stmt_list);

  return t;
}

/* Returns the stmt_tree to which statements are currently being added.  */

stmt_tree
current_stmt_tree (void)
{
  return (cfun
	  ? &cfun->language->base.x_stmt_tree
	  : &scope_chain->x_stmt_tree);
}

/* If statements are full expressions, wrap STMT in a CLEANUP_POINT_EXPR.  */

static tree
maybe_cleanup_point_expr (tree expr)
{
  if (!processing_template_decl && stmts_are_full_exprs_p ())
    expr = fold_build_cleanup_point_expr (TREE_TYPE (expr), expr);
  return expr;
}

/* Like maybe_cleanup_point_expr except have the type of the new expression be
   void so we don't need to create a temporary variable to hold the inner
   expression.  The reason why we do this is because the original type might be
   an aggregate and we cannot create a temporary variable for that type.  */

tree
maybe_cleanup_point_expr_void (tree expr)
{
  if (!processing_template_decl && stmts_are_full_exprs_p ())
    expr = fold_build_cleanup_point_expr (void_type_node, expr);
  return expr;
}



/* Create a declaration statement for the declaration given by the DECL.  */

void
add_decl_expr (tree decl)
{
  tree r = build_stmt (input_location, DECL_EXPR, decl);
  if (DECL_INITIAL (decl)
      || (DECL_SIZE (decl) && TREE_SIDE_EFFECTS (DECL_SIZE (decl))))
    r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);
}

/* Finish a scope.  */

tree
do_poplevel (tree stmt_list)
{
  tree block = NULL;

  if (stmts_are_full_exprs_p ())
    block = poplevel (kept_level_p (), 1, 0);

  stmt_list = pop_stmt_list (stmt_list);

  if (!processing_template_decl)
    {
      stmt_list = c_build_bind_expr (input_location, block, stmt_list);
      /* ??? See c_end_compound_stmt re statement expressions.  */
    }

  return stmt_list;
}

/* Begin a new scope.  */

static tree
do_pushlevel (scope_kind sk)
{
  tree ret = push_stmt_list ();
  if (stmts_are_full_exprs_p ())
    begin_scope (sk, NULL);
  return ret;
}

/* Queue a cleanup.  CLEANUP is an expression/statement to be executed
   when the current scope is exited.  EH_ONLY is true when this is not
   meant to apply to normal control flow transfer.  */

void
push_cleanup (tree decl, tree cleanup, bool eh_only)
{
  tree stmt = build_stmt (input_location, CLEANUP_STMT, NULL, cleanup, decl);
  CLEANUP_EH_ONLY (stmt) = eh_only;
  add_stmt (stmt);
  CLEANUP_BODY (stmt) = push_stmt_list ();
}

/* Begin a conditional that might contain a declaration.  When generating
   normal code, we want the declaration to appear before the statement
   containing the conditional.  When generating template code, we want the
   conditional to be rendered as the raw DECL_EXPR.  */

static void
begin_cond (tree *cond_p)
{
  if (processing_template_decl)
    *cond_p = push_stmt_list ();
}

/* Finish such a conditional.  */

static void
finish_cond (tree *cond_p, tree expr)
{
  if (processing_template_decl)
    {
      tree cond = pop_stmt_list (*cond_p);

      if (expr == NULL_TREE)
	/* Empty condition in 'for'.  */
	gcc_assert (empty_expr_stmt_p (cond));
      else if (check_for_bare_parameter_packs (expr))
        expr = error_mark_node;
      else if (!empty_expr_stmt_p (cond))
	expr = build2 (COMPOUND_EXPR, TREE_TYPE (expr), cond, expr);
    }
  *cond_p = expr;
}

/* If *COND_P specifies a conditional with a declaration, transform the
   loop such that
	    while (A x = 42) { }
	    for (; A x = 42;) { }
   becomes
	    while (true) { A x = 42; if (!x) break; }
	    for (;;) { A x = 42; if (!x) break; }
   The statement list for BODY will be empty if the conditional did
   not declare anything.  */

static void
simplify_loop_decl_cond (tree *cond_p, tree body)
{
  tree cond, if_stmt;

  if (!TREE_SIDE_EFFECTS (body))
    return;

  cond = *cond_p;
  *cond_p = boolean_true_node;

  if_stmt = begin_if_stmt ();
  cond = cp_build_unary_op (TRUTH_NOT_EXPR, cond, 0, tf_warning_or_error);
  finish_if_stmt_cond (cond, if_stmt);
  finish_break_stmt ();
  finish_then_clause (if_stmt);
  finish_if_stmt (if_stmt);
}

/* Finish a goto-statement.  */

tree
finish_goto_stmt (tree destination)
{
  if (identifier_p (destination))
    destination = lookup_label (destination);

  /* We warn about unused labels with -Wunused.  That means we have to
     mark the used labels as used.  */
  if (TREE_CODE (destination) == LABEL_DECL)
    TREE_USED (destination) = 1;
  else
    {
      destination = mark_rvalue_use (destination);
      if (!processing_template_decl)
	{
	  destination = cp_convert (ptr_type_node, destination,
				    tf_warning_or_error);
	  if (error_operand_p (destination))
	    return NULL_TREE;
	  destination
	    = fold_build_cleanup_point_expr (TREE_TYPE (destination),
					     destination);
	}
    }

  check_goto (destination);

  return add_stmt (build_stmt (input_location, GOTO_EXPR, destination));
}

/* COND is the condition-expression for an if, while, etc.,
   statement.  Convert it to a boolean value, if appropriate.
   In addition, verify sequence points if -Wsequence-point is enabled.  */

static tree
maybe_convert_cond (tree cond)
{
  /* Empty conditions remain empty.  */
  if (!cond)
    return NULL_TREE;

  /* Wait until we instantiate templates before doing conversion.  */
  if (processing_template_decl)
    return cond;

  if (warn_sequence_point)
    verify_sequence_points (cond);

  /* Do the conversion.  */
  cond = convert_from_reference (cond);

  if (TREE_CODE (cond) == MODIFY_EXPR
      && !TREE_NO_WARNING (cond)
      && warn_parentheses)
    {
      warning (OPT_Wparentheses,
	       "suggest parentheses around assignment used as truth value");
      TREE_NO_WARNING (cond) = 1;
    }

  return condition_conversion (cond);
}

/* Finish an expression-statement, whose EXPRESSION is as indicated.  */

tree
finish_expr_stmt (tree expr)
{
  tree r = NULL_TREE;

  if (expr != NULL_TREE)
    {
      if (!processing_template_decl)
	{
	  if (warn_sequence_point)
	    verify_sequence_points (expr);
	  expr = convert_to_void (expr, ICV_STATEMENT, tf_warning_or_error);
	}
      else if (!type_dependent_expression_p (expr))
	convert_to_void (build_non_dependent_expr (expr), ICV_STATEMENT, 
                         tf_warning_or_error);

      if (check_for_bare_parameter_packs (expr))
        expr = error_mark_node;

      /* Simplification of inner statement expressions, compound exprs,
	 etc can result in us already having an EXPR_STMT.  */
      if (TREE_CODE (expr) != CLEANUP_POINT_EXPR)
	{
	  if (TREE_CODE (expr) != EXPR_STMT)
	    expr = build_stmt (input_location, EXPR_STMT, expr);
	  expr = maybe_cleanup_point_expr_void (expr);
	}

      r = add_stmt (expr);
    }

  return r;
}


/* Begin an if-statement.  Returns a newly created IF_STMT if
   appropriate.  */

tree
begin_if_stmt (void)
{
  tree r, scope;
  scope = do_pushlevel (sk_cond);
  r = build_stmt (input_location, IF_STMT, NULL_TREE,
		  NULL_TREE, NULL_TREE, scope);
  begin_cond (&IF_COND (r));
  return r;
}

/* Process the COND of an if-statement, which may be given by
   IF_STMT.  */

void
finish_if_stmt_cond (tree cond, tree if_stmt)
{
  finish_cond (&IF_COND (if_stmt), maybe_convert_cond (cond));
  add_stmt (if_stmt);
  THEN_CLAUSE (if_stmt) = push_stmt_list ();
}

/* Finish the then-clause of an if-statement, which may be given by
   IF_STMT.  */

tree
finish_then_clause (tree if_stmt)
{
  THEN_CLAUSE (if_stmt) = pop_stmt_list (THEN_CLAUSE (if_stmt));
  return if_stmt;
}

/* Begin the else-clause of an if-statement.  */

void
begin_else_clause (tree if_stmt)
{
  ELSE_CLAUSE (if_stmt) = push_stmt_list ();
}

/* Finish the else-clause of an if-statement, which may be given by
   IF_STMT.  */

void
finish_else_clause (tree if_stmt)
{
  ELSE_CLAUSE (if_stmt) = pop_stmt_list (ELSE_CLAUSE (if_stmt));
}

/* Finish an if-statement.  */

void
finish_if_stmt (tree if_stmt)
{
  tree scope = IF_SCOPE (if_stmt);
  IF_SCOPE (if_stmt) = NULL;
  add_stmt (do_poplevel (scope));
}

/* Begin a while-statement.  Returns a newly created WHILE_STMT if
   appropriate.  */

tree
begin_while_stmt (void)
{
  tree r;
  r = build_stmt (input_location, WHILE_STMT, NULL_TREE, NULL_TREE);
  add_stmt (r);
  WHILE_BODY (r) = do_pushlevel (sk_block);
  begin_cond (&WHILE_COND (r));
  return r;
}

/* Process the COND of a while-statement, which may be given by
   WHILE_STMT.  */

void
finish_while_stmt_cond (tree cond, tree while_stmt, bool ivdep)
{
  finish_cond (&WHILE_COND (while_stmt), maybe_convert_cond (cond));
  if (ivdep && cond != error_mark_node)
    WHILE_COND (while_stmt) = build2 (ANNOTATE_EXPR,
				      TREE_TYPE (WHILE_COND (while_stmt)),
				      WHILE_COND (while_stmt),
				      build_int_cst (integer_type_node,
						     annot_expr_ivdep_kind));
  simplify_loop_decl_cond (&WHILE_COND (while_stmt), WHILE_BODY (while_stmt));
}

/* Finish a while-statement, which may be given by WHILE_STMT.  */

void
finish_while_stmt (tree while_stmt)
{
  WHILE_BODY (while_stmt) = do_poplevel (WHILE_BODY (while_stmt));
}

/* Begin a do-statement.  Returns a newly created DO_STMT if
   appropriate.  */

tree
begin_do_stmt (void)
{
  tree r = build_stmt (input_location, DO_STMT, NULL_TREE, NULL_TREE);
  add_stmt (r);
  DO_BODY (r) = push_stmt_list ();
  return r;
}

/* Finish the body of a do-statement, which may be given by DO_STMT.  */

void
finish_do_body (tree do_stmt)
{
  tree body = DO_BODY (do_stmt) = pop_stmt_list (DO_BODY (do_stmt));

  if (TREE_CODE (body) == STATEMENT_LIST && STATEMENT_LIST_TAIL (body))
    body = STATEMENT_LIST_TAIL (body)->stmt;

  if (IS_EMPTY_STMT (body))
    warning (OPT_Wempty_body,
            "suggest explicit braces around empty body in %<do%> statement");
}

/* Finish a do-statement, which may be given by DO_STMT, and whose
   COND is as indicated.  */

void
finish_do_stmt (tree cond, tree do_stmt, bool ivdep)
{
  cond = maybe_convert_cond (cond);
  if (ivdep && cond != error_mark_node)
    cond = build2 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
		   build_int_cst (integer_type_node, annot_expr_ivdep_kind));
  DO_COND (do_stmt) = cond;
}

/* Finish a return-statement.  The EXPRESSION returned, if any, is as
   indicated.  */

tree
finish_return_stmt (tree expr)
{
  tree r;
  bool no_warning;

  expr = check_return_expr (expr, &no_warning);

  if (error_operand_p (expr)
      || (flag_openmp && !check_omp_return ()))
    return error_mark_node;
  if (!processing_template_decl)
    {
      if (warn_sequence_point)
	verify_sequence_points (expr);
      
      if (DECL_DESTRUCTOR_P (current_function_decl)
	  || (DECL_CONSTRUCTOR_P (current_function_decl)
	      && targetm.cxx.cdtor_returns_this ()))
	{
	  /* Similarly, all destructors must run destructors for
	     base-classes before returning.  So, all returns in a
	     destructor get sent to the DTOR_LABEL; finish_function emits
	     code to return a value there.  */
	  return finish_goto_stmt (cdtor_label);
	}
    }

  r = build_stmt (input_location, RETURN_EXPR, expr);
  TREE_NO_WARNING (r) |= no_warning;
  r = maybe_cleanup_point_expr_void (r);
  r = add_stmt (r);

  return r;
}

/* Begin the scope of a for-statement or a range-for-statement.
   Both the returned trees are to be used in a call to
   begin_for_stmt or begin_range_for_stmt.  */

tree
begin_for_scope (tree *init)
{
  tree scope = NULL_TREE;
  if (flag_new_for_scope > 0)
    scope = do_pushlevel (sk_for);

  if (processing_template_decl)
    *init = push_stmt_list ();
  else
    *init = NULL_TREE;

  return scope;
}

/* Begin a for-statement.  Returns a new FOR_STMT.
   SCOPE and INIT should be the return of begin_for_scope,
   or both NULL_TREE  */

tree
begin_for_stmt (tree scope, tree init)
{
  tree r;

  r = build_stmt (input_location, FOR_STMT, NULL_TREE, NULL_TREE,
		  NULL_TREE, NULL_TREE, NULL_TREE);

  if (scope == NULL_TREE)
    {
      gcc_assert (!init || !(flag_new_for_scope > 0));
      if (!init)
	scope = begin_for_scope (&init);
    }
  FOR_INIT_STMT (r) = init;
  FOR_SCOPE (r) = scope;

  return r;
}

/* Finish the for-init-statement of a for-statement, which may be
   given by FOR_STMT.  */

void
finish_for_init_stmt (tree for_stmt)
{
  if (processing_template_decl)
    FOR_INIT_STMT (for_stmt) = pop_stmt_list (FOR_INIT_STMT (for_stmt));
  add_stmt (for_stmt);
  FOR_BODY (for_stmt) = do_pushlevel (sk_block);
  begin_cond (&FOR_COND (for_stmt));
}

/* Finish the COND of a for-statement, which may be given by
   FOR_STMT.  */

void
finish_for_cond (tree cond, tree for_stmt, bool ivdep)
{
  finish_cond (&FOR_COND (for_stmt), maybe_convert_cond (cond));
  if (ivdep && cond != error_mark_node)
    FOR_COND (for_stmt) = build2 (ANNOTATE_EXPR,
				  TREE_TYPE (FOR_COND (for_stmt)),
				  FOR_COND (for_stmt),
				  build_int_cst (integer_type_node,
						 annot_expr_ivdep_kind));
  simplify_loop_decl_cond (&FOR_COND (for_stmt), FOR_BODY (for_stmt));
}

/* Finish the increment-EXPRESSION in a for-statement, which may be
   given by FOR_STMT.  */

void
finish_for_expr (tree expr, tree for_stmt)
{
  if (!expr)
    return;
  /* If EXPR is an overloaded function, issue an error; there is no
     context available to use to perform overload resolution.  */
  if (type_unknown_p (expr))
    {
      cxx_incomplete_type_error (expr, TREE_TYPE (expr));
      expr = error_mark_node;
    }
  if (!processing_template_decl)
    {
      if (warn_sequence_point)
	verify_sequence_points (expr);
      expr = convert_to_void (expr, ICV_THIRD_IN_FOR,
                              tf_warning_or_error);
    }
  else if (!type_dependent_expression_p (expr))
    convert_to_void (build_non_dependent_expr (expr), ICV_THIRD_IN_FOR,
                     tf_warning_or_error);
  expr = maybe_cleanup_point_expr_void (expr);
  if (check_for_bare_parameter_packs (expr))
    expr = error_mark_node;
  FOR_EXPR (for_stmt) = expr;
}

/* Finish the body of a for-statement, which may be given by
   FOR_STMT.  The increment-EXPR for the loop must be
   provided.
   It can also finish RANGE_FOR_STMT. */

void
finish_for_stmt (tree for_stmt)
{
  if (TREE_CODE (for_stmt) == RANGE_FOR_STMT)
    RANGE_FOR_BODY (for_stmt) = do_poplevel (RANGE_FOR_BODY (for_stmt));
  else
    FOR_BODY (for_stmt) = do_poplevel (FOR_BODY (for_stmt));

  /* Pop the scope for the body of the loop.  */
  if (flag_new_for_scope > 0)
    {
      tree scope;
      tree *scope_ptr = (TREE_CODE (for_stmt) == RANGE_FOR_STMT
			 ? &RANGE_FOR_SCOPE (for_stmt)
			 : &FOR_SCOPE (for_stmt));
      scope = *scope_ptr;
      *scope_ptr = NULL;
      add_stmt (do_poplevel (scope));
    }
}

/* Begin a range-for-statement.  Returns a new RANGE_FOR_STMT.
   SCOPE and INIT should be the return of begin_for_scope,
   or both NULL_TREE  .
   To finish it call finish_for_stmt(). */

tree
begin_range_for_stmt (tree scope, tree init)
{
  tree r;

  r = build_stmt (input_location, RANGE_FOR_STMT,
		  NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE);

  if (scope == NULL_TREE)
    {
      gcc_assert (!init || !(flag_new_for_scope > 0));
      if (!init)
	scope = begin_for_scope (&init);
    }

  /* RANGE_FOR_STMTs do not use nor save the init tree, so we
     pop it now.  */
  if (init)
    pop_stmt_list (init);
  RANGE_FOR_SCOPE (r) = scope;

  return r;
}

/* Finish the head of a range-based for statement, which may
   be given by RANGE_FOR_STMT. DECL must be the declaration
   and EXPR must be the loop expression. */

void
finish_range_for_decl (tree range_for_stmt, tree decl, tree expr)
{
  RANGE_FOR_DECL (range_for_stmt) = decl;
  RANGE_FOR_EXPR (range_for_stmt) = expr;
  add_stmt (range_for_stmt);
  RANGE_FOR_BODY (range_for_stmt) = do_pushlevel (sk_block);
}

/* Finish a break-statement.  */

tree
finish_break_stmt (void)
{
  /* In switch statements break is sometimes stylistically used after
     a return statement.  This can lead to spurious warnings about
     control reaching the end of a non-void function when it is
     inlined.  Note that we are calling block_may_fallthru with
     language specific tree nodes; this works because
     block_may_fallthru returns true when given something it does not
     understand.  */
  if (!block_may_fallthru (cur_stmt_list))
    return void_zero_node;
  return add_stmt (build_stmt (input_location, BREAK_STMT));
}

/* Finish a continue-statement.  */

tree
finish_continue_stmt (void)
{
  return add_stmt (build_stmt (input_location, CONTINUE_STMT));
}

/* Begin a switch-statement.  Returns a new SWITCH_STMT if
   appropriate.  */

tree
begin_switch_stmt (void)
{
  tree r, scope;

  scope = do_pushlevel (sk_cond);
  r = build_stmt (input_location, SWITCH_STMT, NULL_TREE, NULL_TREE, NULL_TREE, scope);

  begin_cond (&SWITCH_STMT_COND (r));

  return r;
}

/* Finish the cond of a switch-statement.  */

void
finish_switch_cond (tree cond, tree switch_stmt)
{
  tree orig_type = NULL;
  if (!processing_template_decl)
    {
      /* Convert the condition to an integer or enumeration type.  */
      cond = build_expr_type_conversion (WANT_INT | WANT_ENUM, cond, true);
      if (cond == NULL_TREE)
	{
	  error ("switch quantity not an integer");
	  cond = error_mark_node;
	}
      orig_type = TREE_TYPE (cond);
      if (cond != error_mark_node)
	{
	  /* [stmt.switch]

	     Integral promotions are performed.  */
	  cond = perform_integral_promotions (cond);
	  cond = maybe_cleanup_point_expr (cond);
	}
    }
  if (check_for_bare_parameter_packs (cond))
    cond = error_mark_node;
  else if (!processing_template_decl && warn_sequence_point)
    verify_sequence_points (cond);

  finish_cond (&SWITCH_STMT_COND (switch_stmt), cond);
  SWITCH_STMT_TYPE (switch_stmt) = orig_type;
  add_stmt (switch_stmt);
  push_switch (switch_stmt);
  SWITCH_STMT_BODY (switch_stmt) = push_stmt_list ();
}

/* Finish the body of a switch-statement, which may be given by
   SWITCH_STMT.  The COND to switch on is indicated.  */

void
finish_switch_stmt (tree switch_stmt)
{
  tree scope;

  SWITCH_STMT_BODY (switch_stmt) =
    pop_stmt_list (SWITCH_STMT_BODY (switch_stmt));
  pop_switch ();

  scope = SWITCH_STMT_SCOPE (switch_stmt);
  SWITCH_STMT_SCOPE (switch_stmt) = NULL;
  add_stmt (do_poplevel (scope));
}

/* Begin a try-block.  Returns a newly-created TRY_BLOCK if
   appropriate.  */

tree
begin_try_block (void)
{
  tree r = build_stmt (input_location, TRY_BLOCK, NULL_TREE, NULL_TREE);
  add_stmt (r);
  TRY_STMTS (r) = push_stmt_list ();
  return r;
}

/* Likewise, for a function-try-block.  The block returned in
   *COMPOUND_STMT is an artificial outer scope, containing the
   function-try-block.  */

tree
begin_function_try_block (tree *compound_stmt)
{
  tree r;
  /* This outer scope does not exist in the C++ standard, but we need
     a place to put __FUNCTION__ and similar variables.  */
  *compound_stmt = begin_compound_stmt (0);
  r = begin_try_block ();
  FN_TRY_BLOCK_P (r) = 1;
  return r;
}

/* Finish a try-block, which may be given by TRY_BLOCK.  */

void
finish_try_block (tree try_block)
{
  TRY_STMTS (try_block) = pop_stmt_list (TRY_STMTS (try_block));
  TRY_HANDLERS (try_block) = push_stmt_list ();
}

/* Finish the body of a cleanup try-block, which may be given by
   TRY_BLOCK.  */

void
finish_cleanup_try_block (tree try_block)
{
  TRY_STMTS (try_block) = pop_stmt_list (TRY_STMTS (try_block));
}

/* Finish an implicitly generated try-block, with a cleanup is given
   by CLEANUP.  */

void
finish_cleanup (tree cleanup, tree try_block)
{
  TRY_HANDLERS (try_block) = cleanup;
  CLEANUP_P (try_block) = 1;
}

/* Likewise, for a function-try-block.  */

void
finish_function_try_block (tree try_block)
{
  finish_try_block (try_block);
  /* FIXME : something queer about CTOR_INITIALIZER somehow following
     the try block, but moving it inside.  */
  in_function_try_handler = 1;
}

/* Finish a handler-sequence for a try-block, which may be given by
   TRY_BLOCK.  */

void
finish_handler_sequence (tree try_block)
{
  TRY_HANDLERS (try_block) = pop_stmt_list (TRY_HANDLERS (try_block));
  check_handlers (TRY_HANDLERS (try_block));
}

/* Finish the handler-seq for a function-try-block, given by
   TRY_BLOCK.  COMPOUND_STMT is the outer block created by
   begin_function_try_block.  */

void
finish_function_handler_sequence (tree try_block, tree compound_stmt)
{
  in_function_try_handler = 0;
  finish_handler_sequence (try_block);
  finish_compound_stmt (compound_stmt);
}

/* Begin a handler.  Returns a HANDLER if appropriate.  */

tree
begin_handler (void)
{
  tree r;

  r = build_stmt (input_location, HANDLER, NULL_TREE, NULL_TREE);
  add_stmt (r);

  /* Create a binding level for the eh_info and the exception object
     cleanup.  */
  HANDLER_BODY (r) = do_pushlevel (sk_catch);

  return r;
}

/* Finish the handler-parameters for a handler, which may be given by
   HANDLER.  DECL is the declaration for the catch parameter, or NULL
   if this is a `catch (...)' clause.  */

void
finish_handler_parms (tree decl, tree handler)
{
  tree type = NULL_TREE;
  if (processing_template_decl)
    {
      if (decl)
	{
	  decl = pushdecl (decl);
	  decl = push_template_decl (decl);
	  HANDLER_PARMS (handler) = decl;
	  type = TREE_TYPE (decl);
	}
    }
  else
    type = expand_start_catch_block (decl);
  HANDLER_TYPE (handler) = type;
}

/* Finish a handler, which may be given by HANDLER.  The BLOCKs are
   the return value from the matching call to finish_handler_parms.  */

void
finish_handler (tree handler)
{
  if (!processing_template_decl)
    expand_end_catch_block ();
  HANDLER_BODY (handler) = do_poplevel (HANDLER_BODY (handler));
}

/* Begin a compound statement.  FLAGS contains some bits that control the
   behavior and context.  If BCS_NO_SCOPE is set, the compound statement
   does not define a scope.  If BCS_FN_BODY is set, this is the outermost
   block of a function.  If BCS_TRY_BLOCK is set, this is the block
   created on behalf of a TRY statement.  Returns a token to be passed to
   finish_compound_stmt.  */

tree
begin_compound_stmt (unsigned int flags)
{
  tree r;

  if (flags & BCS_NO_SCOPE)
    {
      r = push_stmt_list ();
      STATEMENT_LIST_NO_SCOPE (r) = 1;

      /* Normally, we try hard to keep the BLOCK for a statement-expression.
	 But, if it's a statement-expression with a scopeless block, there's
	 nothing to keep, and we don't want to accidentally keep a block
	 *inside* the scopeless block.  */
      keep_next_level (false);
    }
  else
    r = do_pushlevel (flags & BCS_TRY_BLOCK ? sk_try : sk_block);

  /* When processing a template, we need to remember where the braces were,
     so that we can set up identical scopes when instantiating the template
     later.  BIND_EXPR is a handy candidate for this.
     Note that do_poplevel won't create a BIND_EXPR itself here (and thus
     result in nested BIND_EXPRs), since we don't build BLOCK nodes when
     processing templates.  */
  if (processing_template_decl)
    {
      r = build3 (BIND_EXPR, NULL, NULL, r, NULL);
      BIND_EXPR_TRY_BLOCK (r) = (flags & BCS_TRY_BLOCK) != 0;
      BIND_EXPR_BODY_BLOCK (r) = (flags & BCS_FN_BODY) != 0;
      TREE_SIDE_EFFECTS (r) = 1;
    }

  return r;
}

/* Finish a compound-statement, which is given by STMT.  */

void
finish_compound_stmt (tree stmt)
{
  if (TREE_CODE (stmt) == BIND_EXPR)
    {
      tree body = do_poplevel (BIND_EXPR_BODY (stmt));
      /* If the STATEMENT_LIST is empty and this BIND_EXPR isn't special,
	 discard the BIND_EXPR so it can be merged with the containing
	 STATEMENT_LIST.  */
      if (TREE_CODE (body) == STATEMENT_LIST
	  && STATEMENT_LIST_HEAD (body) == NULL
	  && !BIND_EXPR_BODY_BLOCK (stmt)
	  && !BIND_EXPR_TRY_BLOCK (stmt))
	stmt = body;
      else
	BIND_EXPR_BODY (stmt) = body;
    }
  else if (STATEMENT_LIST_NO_SCOPE (stmt))
    stmt = pop_stmt_list (stmt);
  else
    {
      /* Destroy any ObjC "super" receivers that may have been
	 created.  */
      objc_clear_super_receiver ();

      stmt = do_poplevel (stmt);
    }

  /* ??? See c_end_compound_stmt wrt statement expressions.  */
  add_stmt (stmt);
}

/* Finish an asm-statement, whose components are a STRING, some
   OUTPUT_OPERANDS, some INPUT_OPERANDS, some CLOBBERS and some
   LABELS.  Also note whether the asm-statement should be
   considered volatile.  */

tree
finish_asm_stmt (int volatile_p, tree string, tree output_operands,
		 tree input_operands, tree clobbers, tree labels)
{
  tree r;
  tree t;
  int ninputs = list_length (input_operands);
  int noutputs = list_length (output_operands);

  if (!processing_template_decl)
    {
      const char *constraint;
      const char **oconstraints;
      bool allows_mem, allows_reg, is_inout;
      tree operand;
      int i;

      oconstraints = XALLOCAVEC (const char *, noutputs);

      string = resolve_asm_operand_names (string, output_operands,
					  input_operands, labels);

      for (i = 0, t = output_operands; t; t = TREE_CHAIN (t), ++i)
	{
	  operand = TREE_VALUE (t);

	  /* ??? Really, this should not be here.  Users should be using a
	     proper lvalue, dammit.  But there's a long history of using
	     casts in the output operands.  In cases like longlong.h, this
	     becomes a primitive form of typechecking -- if the cast can be
	     removed, then the output operand had a type of the proper width;
	     otherwise we'll get an error.  Gross, but ...  */
	  STRIP_NOPS (operand);

	  operand = mark_lvalue_use (operand);

	  if (!lvalue_or_else (operand, lv_asm, tf_warning_or_error))
	    operand = error_mark_node;

	  if (operand != error_mark_node
	      && (TREE_READONLY (operand)
		  || CP_TYPE_CONST_P (TREE_TYPE (operand))
		  /* Functions are not modifiable, even though they are
		     lvalues.  */
		  || TREE_CODE (TREE_TYPE (operand)) == FUNCTION_TYPE
		  || TREE_CODE (TREE_TYPE (operand)) == METHOD_TYPE
		  /* If it's an aggregate and any field is const, then it is
		     effectively const.  */
		  || (CLASS_TYPE_P (TREE_TYPE (operand))
		      && C_TYPE_FIELDS_READONLY (TREE_TYPE (operand)))))
	    cxx_readonly_error (operand, lv_asm);

	  constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
	  oconstraints[i] = constraint;

	  if (parse_output_constraint (&constraint, i, ninputs, noutputs,
				       &allows_mem, &allows_reg, &is_inout))
	    {
	      /* If the operand is going to end up in memory,
		 mark it addressable.  */
	      if (!allows_reg && !cxx_mark_addressable (operand))
		operand = error_mark_node;
	    }
	  else
	    operand = error_mark_node;

	  TREE_VALUE (t) = operand;
	}

      for (i = 0, t = input_operands; t; ++i, t = TREE_CHAIN (t))
	{
	  constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
	  bool constraint_parsed
	    = parse_input_constraint (&constraint, i, ninputs, noutputs, 0,   
				      oconstraints, &allows_mem, &allows_reg);
	  /* If the operand is going to end up in memory, don't call
	     decay_conversion.  */
	  if (constraint_parsed && !allows_reg && allows_mem)
	    operand = mark_lvalue_use (TREE_VALUE (t));
	  else
	    operand = decay_conversion (TREE_VALUE (t), tf_warning_or_error);

	  /* If the type of the operand hasn't been determined (e.g.,
	     because it involves an overloaded function), then issue
	     an error message.  There's no context available to
	     resolve the overloading.  */
	  if (TREE_TYPE (operand) == unknown_type_node)
	    {
	      error ("type of asm operand %qE could not be determined",
		     TREE_VALUE (t));
	      operand = error_mark_node;
	    }

	  if (constraint_parsed)
	    {
	      /* If the operand is going to end up in memory,
		 mark it addressable.  */
	      if (!allows_reg && allows_mem)
		{
		  /* Strip the nops as we allow this case.  FIXME, this really
		     should be rejected or made deprecated.  */
		  STRIP_NOPS (operand);
		  if (!cxx_mark_addressable (operand))
		    operand = error_mark_node;
		}
	      else if (!allows_reg && !allows_mem)
		{
		  /* If constraint allows neither register nor memory,
		     try harder to get a constant.  */
		  tree constop = maybe_constant_value (operand);
		  if (TREE_CONSTANT (constop))
		    operand = constop;
		}
	    }
	  else
	    operand = error_mark_node;

	  TREE_VALUE (t) = operand;
	}
    }

  r = build_stmt (input_location, ASM_EXPR, string,
		  output_operands, input_operands,
		  clobbers, labels);
  ASM_VOLATILE_P (r) = volatile_p || noutputs == 0;
  r = maybe_cleanup_point_expr_void (r);
  return add_stmt (r);
}

/* Finish a label with the indicated NAME.  Returns the new label.  */

tree
finish_label_stmt (tree name)
{
  tree decl = define_label (input_location, name);

  if (decl == error_mark_node)
    return error_mark_node;

  add_stmt (build_stmt (input_location, LABEL_EXPR, decl));

  return decl;
}

/* Finish a series of declarations for local labels.  G++ allows users
   to declare "local" labels, i.e., labels with scope.  This extension
   is useful when writing code involving statement-expressions.  */

void
finish_label_decl (tree name)
{
  if (!at_function_scope_p ())
    {
      error ("__label__ declarations are only allowed in function scopes");
      return;
    }

  add_decl_expr (declare_local_label (name));
}

/* When DECL goes out of scope, make sure that CLEANUP is executed.  */

void
finish_decl_cleanup (tree decl, tree cleanup)
{
  push_cleanup (decl, cleanup, false);
}

/* If the current scope exits with an exception, run CLEANUP.  */

void
finish_eh_cleanup (tree cleanup)
{
  push_cleanup (NULL, cleanup, true);
}

/* The MEM_INITS is a list of mem-initializers, in reverse of the
   order they were written by the user.  Each node is as for
   emit_mem_initializers.  */

void
finish_mem_initializers (tree mem_inits)
{
  /* Reorder the MEM_INITS so that they are in the order they appeared
     in the source program.  */
  mem_inits = nreverse (mem_inits);

  if (processing_template_decl)
    {
      tree mem;

      for (mem = mem_inits; mem; mem = TREE_CHAIN (mem))
        {
          /* If the TREE_PURPOSE is a TYPE_PACK_EXPANSION, skip the
             check for bare parameter packs in the TREE_VALUE, because
             any parameter packs in the TREE_VALUE have already been
             bound as part of the TREE_PURPOSE.  See
             make_pack_expansion for more information.  */
          if (TREE_CODE (TREE_PURPOSE (mem)) != TYPE_PACK_EXPANSION
              && check_for_bare_parameter_packs (TREE_VALUE (mem)))
            TREE_VALUE (mem) = error_mark_node;
        }

      add_stmt (build_min_nt_loc (UNKNOWN_LOCATION,
				  CTOR_INITIALIZER, mem_inits));
    }
  else
    emit_mem_initializers (mem_inits);
}

/* Obfuscate EXPR if it looks like an id-expression or member access so
   that the call to finish_decltype in do_auto_deduction will give the
   right result.  */

tree
force_paren_expr (tree expr)
{
  /* This is only needed for decltype(auto) in C++14.  */
  if (cxx_dialect < cxx1y)
    return expr;

  if (!DECL_P (expr) && TREE_CODE (expr) != COMPONENT_REF
      && TREE_CODE (expr) != SCOPE_REF)
    return expr;

  if (processing_template_decl)
    expr = build1 (PAREN_EXPR, TREE_TYPE (expr), expr);
  else
    {
      cp_lvalue_kind kind = lvalue_kind (expr);
      if ((kind & ~clk_class) != clk_none)
	{
	  tree type = unlowered_expr_type (expr);
	  bool rval = !!(kind & clk_rvalueref);
	  type = cp_build_reference_type (type, rval);
	  expr = build_static_cast (type, expr, tf_warning_or_error);
	}
    }

  return expr;
}

/* Finish a parenthesized expression EXPR.  */

tree
finish_parenthesized_expr (tree expr)
{
  if (EXPR_P (expr))
    /* This inhibits warnings in c_common_truthvalue_conversion.  */
    TREE_NO_WARNING (expr) = 1;

  if (TREE_CODE (expr) == OFFSET_REF
      || TREE_CODE (expr) == SCOPE_REF)
    /* [expr.unary.op]/3 The qualified id of a pointer-to-member must not be
       enclosed in parentheses.  */
    PTRMEM_OK_P (expr) = 0;

  if (TREE_CODE (expr) == STRING_CST)
    PAREN_STRING_LITERAL_P (expr) = 1;

  expr = force_paren_expr (expr);

  return expr;
}

/* Finish a reference to a non-static data member (DECL) that is not
   preceded by `.' or `->'.  */

tree
finish_non_static_data_member (tree decl, tree object, tree qualifying_scope)
{
  gcc_assert (TREE_CODE (decl) == FIELD_DECL);

  if (!object)
    {
      tree scope = qualifying_scope;
      if (scope == NULL_TREE)
	scope = context_for_name_lookup (decl);
      object = maybe_dummy_object (scope, NULL);
    }

  object = maybe_resolve_dummy (object);
  if (object == error_mark_node)
    return error_mark_node;

  /* DR 613: Can use non-static data members without an associated
     object in sizeof/decltype/alignof.  */
  if (is_dummy_object (object) && cp_unevaluated_operand == 0
      && (!processing_template_decl || !current_class_ref))
    {
      if (current_function_decl
	  && DECL_STATIC_FUNCTION_P (current_function_decl))
	error ("invalid use of member %q+D in static member function", decl);
      else
	error ("invalid use of non-static data member %q+D", decl);
      error ("from this location");

      return error_mark_node;
    }

  if (current_class_ptr)
    TREE_USED (current_class_ptr) = 1;
  if (processing_template_decl && !qualifying_scope)
    {
      tree type = TREE_TYPE (decl);

      if (TREE_CODE (type) == REFERENCE_TYPE)
	/* Quals on the object don't matter.  */;
      else if (PACK_EXPANSION_P (type))
	/* Don't bother trying to represent this.  */
	type = NULL_TREE;
      else
	{
	  /* Set the cv qualifiers.  */
	  int quals = cp_type_quals (TREE_TYPE (object));

	  if (DECL_MUTABLE_P (decl))
	    quals &= ~TYPE_QUAL_CONST;

	  quals |= cp_type_quals (TREE_TYPE (decl));
	  type = cp_build_qualified_type (type, quals);
	}

      return (convert_from_reference
	      (build_min (COMPONENT_REF, type, object, decl, NULL_TREE)));
    }
  /* If PROCESSING_TEMPLATE_DECL is nonzero here, then
     QUALIFYING_SCOPE is also non-null.  Wrap this in a SCOPE_REF
     for now.  */
  else if (processing_template_decl)
    return build_qualified_name (TREE_TYPE (decl),
				 qualifying_scope,
				 decl,
				 /*template_p=*/false);
  else
    {
      tree access_type = TREE_TYPE (object);

      perform_or_defer_access_check (TYPE_BINFO (access_type), decl,
				     decl, tf_warning_or_error);

      /* If the data member was named `C::M', convert `*this' to `C'
	 first.  */
      if (qualifying_scope)
	{
	  tree binfo = NULL_TREE;
	  object = build_scoped_ref (object, qualifying_scope,
				     &binfo);
	}

      return build_class_member_access_expr (object, decl,
					     /*access_path=*/NULL_TREE,
					     /*preserve_reference=*/false,
					     tf_warning_or_error);
    }
}

/* If we are currently parsing a template and we encountered a typedef
   TYPEDEF_DECL that is being accessed though CONTEXT, this function
   adds the typedef to a list tied to the current template.
   At template instantiation time, that list is walked and access check
   performed for each typedef.
   LOCATION is the location of the usage point of TYPEDEF_DECL.  */

void
add_typedef_to_current_template_for_access_check (tree typedef_decl,
                                                  tree context,
						  location_t location)
{
    tree template_info = NULL;
    tree cs = current_scope ();

    if (!is_typedef_decl (typedef_decl)
	|| !context
	|| !CLASS_TYPE_P (context)
	|| !cs)
      return;

    if (CLASS_TYPE_P (cs) || TREE_CODE (cs) == FUNCTION_DECL)
      template_info = get_template_info (cs);

    if (template_info
	&& TI_TEMPLATE (template_info)
	&& !currently_open_class (context))
      append_type_to_template_for_access_check (cs, typedef_decl,
						context, location);
}

/* DECL was the declaration to which a qualified-id resolved.  Issue
   an error message if it is not accessible.  If OBJECT_TYPE is
   non-NULL, we have just seen `x->' or `x.' and OBJECT_TYPE is the
   type of `*x', or `x', respectively.  If the DECL was named as
   `A::B' then NESTED_NAME_SPECIFIER is `A'.  */

void
check_accessibility_of_qualified_id (tree decl,
				     tree object_type,
				     tree nested_name_specifier)
{
  tree scope;
  tree qualifying_type = NULL_TREE;

  /* If we are parsing a template declaration and if decl is a typedef,
     add it to a list tied to the template.
     At template instantiation time, that list will be walked and
     access check performed.  */
  add_typedef_to_current_template_for_access_check (decl,
						    nested_name_specifier
						    ? nested_name_specifier
						    : DECL_CONTEXT (decl),
						    input_location);

  /* If we're not checking, return immediately.  */
  if (deferred_access_no_check)
    return;

  /* Determine the SCOPE of DECL.  */
  scope = context_for_name_lookup (decl);
  /* If the SCOPE is not a type, then DECL is not a member.  */
  if (!TYPE_P (scope))
    return;
  /* Compute the scope through which DECL is being accessed.  */
  if (object_type
      /* OBJECT_TYPE might not be a class type; consider:

	   class A { typedef int I; };
	   I *p;
	   p->A::I::~I();

	 In this case, we will have "A::I" as the DECL, but "I" as the
	 OBJECT_TYPE.  */
      && CLASS_TYPE_P (object_type)
      && DERIVED_FROM_P (scope, object_type))
    /* If we are processing a `->' or `.' expression, use the type of the
       left-hand side.  */
    qualifying_type = object_type;
  else if (nested_name_specifier)
    {
      /* If the reference is to a non-static member of the
	 current class, treat it as if it were referenced through
	 `this'.  */
      if (DECL_NONSTATIC_MEMBER_P (decl)
	  && current_class_ptr
	  && DERIVED_FROM_P (scope, current_class_type))
	qualifying_type = current_class_type;
      /* Otherwise, use the type indicated by the
	 nested-name-specifier.  */
      else
	qualifying_type = nested_name_specifier;
    }
  else
    /* Otherwise, the name must be from the current class or one of
       its bases.  */
    qualifying_type = currently_open_derived_class (scope);

  if (qualifying_type 
      /* It is possible for qualifying type to be a TEMPLATE_TYPE_PARM
	 or similar in a default argument value.  */
      && CLASS_TYPE_P (qualifying_type)
      && !dependent_type_p (qualifying_type))
    perform_or_defer_access_check (TYPE_BINFO (qualifying_type), decl,
				   decl, tf_warning_or_error);
}

/* EXPR is the result of a qualified-id.  The QUALIFYING_CLASS was the
   class named to the left of the "::" operator.  DONE is true if this
   expression is a complete postfix-expression; it is false if this
   expression is followed by '->', '[', '(', etc.  ADDRESS_P is true
   iff this expression is the operand of '&'.  TEMPLATE_P is true iff
   the qualified-id was of the form "A::template B".  TEMPLATE_ARG_P
   is true iff this qualified name appears as a template argument.  */

tree
finish_qualified_id_expr (tree qualifying_class,
			  tree expr,
			  bool done,
			  bool address_p,
			  bool template_p,
			  bool template_arg_p,
			  tsubst_flags_t complain)
{
  gcc_assert (TYPE_P (qualifying_class));

  if (error_operand_p (expr))
    return error_mark_node;

  if ((DECL_P (expr) || BASELINK_P (expr))
      && !mark_used (expr, complain))
    return error_mark_node;

  if (template_p)
    check_template_keyword (expr);

  /* If EXPR occurs as the operand of '&', use special handling that
     permits a pointer-to-member.  */
  if (address_p && done)
    {
      if (TREE_CODE (expr) == SCOPE_REF)
	expr = TREE_OPERAND (expr, 1);
      expr = build_offset_ref (qualifying_class, expr,
			       /*address_p=*/true, complain);
      return expr;
    }

  /* No need to check access within an enum.  */
  if (TREE_CODE (qualifying_class) == ENUMERAL_TYPE)
    return expr;

  /* Within the scope of a class, turn references to non-static
     members into expression of the form "this->...".  */
  if (template_arg_p)
    /* But, within a template argument, we do not want make the
       transformation, as there is no "this" pointer.  */
    ;
  else if (TREE_CODE (expr) == FIELD_DECL)
    {
      push_deferring_access_checks (dk_no_check);
      expr = finish_non_static_data_member (expr, NULL_TREE,
					    qualifying_class);
      pop_deferring_access_checks ();
    }
  else if (BASELINK_P (expr) && !processing_template_decl)
    {
      /* See if any of the functions are non-static members.  */
      /* If so, the expression may be relative to 'this'.  */
      if (!shared_member_p (expr)
	  && current_class_ptr
	  && DERIVED_FROM_P (qualifying_class,
			     current_nonlambda_class_type ()))
	expr = (build_class_member_access_expr
		(maybe_dummy_object (qualifying_class, NULL),
		 expr,
		 BASELINK_ACCESS_BINFO (expr),
		 /*preserve_reference=*/false,
		 complain));
      else if (done)
	/* The expression is a qualified name whose address is not
	   being taken.  */
	expr = build_offset_ref (qualifying_class, expr, /*address_p=*/false,
				 complain);
    }
  else if (BASELINK_P (expr))
    ;
  else
    {
      /* In a template, return a SCOPE_REF for most qualified-ids
	 so that we can check access at instantiation time.  But if
	 we're looking at a member of the current instantiation, we
	 know we have access and building up the SCOPE_REF confuses
	 non-type template argument handling.  */
      if (processing_template_decl
	  && !currently_open_class (qualifying_class))
	expr = build_qualified_name (TREE_TYPE (expr),
				     qualifying_class, expr,
				     template_p);

      expr = convert_from_reference (expr);
    }

  return expr;
}

/* Begin a statement-expression.  The value returned must be passed to
   finish_stmt_expr.  */

tree
begin_stmt_expr (void)
{
  return push_stmt_list ();
}

/* Process the final expression of a statement expression. EXPR can be
   NULL, if the final expression is empty.  Return a STATEMENT_LIST
   containing all the statements in the statement-expression, or
   ERROR_MARK_NODE if there was an error.  */

tree
finish_stmt_expr_expr (tree expr, tree stmt_expr)
{
  if (error_operand_p (expr))
    {
      /* The type of the statement-expression is the type of the last
         expression.  */
      TREE_TYPE (stmt_expr) = error_mark_node;
      return error_mark_node;
    }

  /* If the last statement does not have "void" type, then the value
     of the last statement is the value of the entire expression.  */
  if (expr)
    {
      tree type = TREE_TYPE (expr);

      if (processing_template_decl)
	{
	  expr = build_stmt (input_location, EXPR_STMT, expr);
	  expr = add_stmt (expr);
	  /* Mark the last statement so that we can recognize it as such at
	     template-instantiation time.  */
	  EXPR_STMT_STMT_EXPR_RESULT (expr) = 1;
	}
      else if (VOID_TYPE_P (type))
	{
	  /* Just treat this like an ordinary statement.  */
	  expr = finish_expr_stmt (expr);
	}
      else
	{
	  /* It actually has a value we need to deal with.  First, force it
	     to be an rvalue so that we won't need to build up a copy
	     constructor call later when we try to assign it to something.  */
	  expr = force_rvalue (expr, tf_warning_or_error);
	  if (error_operand_p (expr))
	    return error_mark_node;

	  /* Update for array-to-pointer decay.  */
	  type = TREE_TYPE (expr);

	  /* Wrap it in a CLEANUP_POINT_EXPR and add it to the list like a
	     normal statement, but don't convert to void or actually add
	     the EXPR_STMT.  */
	  if (TREE_CODE (expr) != CLEANUP_POINT_EXPR)
	    expr = maybe_cleanup_point_expr (expr);
	  add_stmt (expr);
	}

      /* The type of the statement-expression is the type of the last
	 expression.  */
      TREE_TYPE (stmt_expr) = type;
    }

  return stmt_expr;
}

/* Finish a statement-expression.  EXPR should be the value returned
   by the previous begin_stmt_expr.  Returns an expression
   representing the statement-expression.  */

tree
finish_stmt_expr (tree stmt_expr, bool has_no_scope)
{
  tree type;
  tree result;

  if (error_operand_p (stmt_expr))
    {
      pop_stmt_list (stmt_expr);
      return error_mark_node;
    }

  gcc_assert (TREE_CODE (stmt_expr) == STATEMENT_LIST);

  type = TREE_TYPE (stmt_expr);
  result = pop_stmt_list (stmt_expr);
  TREE_TYPE (result) = type;

  if (processing_template_decl)
    {
      result = build_min (STMT_EXPR, type, result);
      TREE_SIDE_EFFECTS (result) = 1;
      STMT_EXPR_NO_SCOPE (result) = has_no_scope;
    }
  else if (CLASS_TYPE_P (type))
    {
      /* Wrap the statement-expression in a TARGET_EXPR so that the
	 temporary object created by the final expression is destroyed at
	 the end of the full-expression containing the
	 statement-expression.  */
      result = force_target_expr (type, result, tf_warning_or_error);
    }

  return result;
}

/* Returns the expression which provides the value of STMT_EXPR.  */

tree
stmt_expr_value_expr (tree stmt_expr)
{
  tree t = STMT_EXPR_STMT (stmt_expr);

  if (TREE_CODE (t) == BIND_EXPR)
    t = BIND_EXPR_BODY (t);

  if (TREE_CODE (t) == STATEMENT_LIST && STATEMENT_LIST_TAIL (t))
    t = STATEMENT_LIST_TAIL (t)->stmt;

  if (TREE_CODE (t) == EXPR_STMT)
    t = EXPR_STMT_EXPR (t);

  return t;
}

/* Return TRUE iff EXPR_STMT is an empty list of
   expression statements.  */

bool
empty_expr_stmt_p (tree expr_stmt)
{
  tree body = NULL_TREE;

  if (expr_stmt == void_zero_node)
    return true;

  if (expr_stmt)
    {
      if (TREE_CODE (expr_stmt) == EXPR_STMT)
	body = EXPR_STMT_EXPR (expr_stmt);
      else if (TREE_CODE (expr_stmt) == STATEMENT_LIST)
	body = expr_stmt;
    }

  if (body)
    {
      if (TREE_CODE (body) == STATEMENT_LIST)
	return tsi_end_p (tsi_start (body));
      else
	return empty_expr_stmt_p (body);
    }
  return false;
}

/* Perform Koenig lookup.  FN is the postfix-expression representing
   the function (or functions) to call; ARGS are the arguments to the
   call; if INCLUDE_STD then the `std' namespace is automatically
   considered an associated namespace (used in range-based for loops).
   Returns the functions to be considered by overload resolution.  */

tree
perform_koenig_lookup (tree fn, vec<tree, va_gc> *args, bool include_std,
		       tsubst_flags_t complain)
{
  tree identifier = NULL_TREE;
  tree functions = NULL_TREE;
  tree tmpl_args = NULL_TREE;
  bool template_id = false;

  if (TREE_CODE (fn) == TEMPLATE_ID_EXPR)
    {
      /* Use a separate flag to handle null args.  */
      template_id = true;
      tmpl_args = TREE_OPERAND (fn, 1);
      fn = TREE_OPERAND (fn, 0);
    }

  /* Find the name of the overloaded function.  */
  if (identifier_p (fn))
    identifier = fn;
  else if (is_overloaded_fn (fn))
    {
      functions = fn;
      identifier = DECL_NAME (get_first_fn (functions));
    }
  else if (DECL_P (fn))
    {
      functions = fn;
      identifier = DECL_NAME (fn);
    }

  /* A call to a namespace-scope function using an unqualified name.

     Do Koenig lookup -- unless any of the arguments are
     type-dependent.  */
  if (!any_type_dependent_arguments_p (args)
      && !any_dependent_template_arguments_p (tmpl_args))
    {
      fn = lookup_arg_dependent (identifier, functions, args, include_std);
      if (!fn)
	{
	  /* The unqualified name could not be resolved.  */
	  if (complain)
	    fn = unqualified_fn_lookup_error (identifier);
	  else
	    fn = identifier;
	}
    }

  if (fn && template_id)
    fn = build2 (TEMPLATE_ID_EXPR, unknown_type_node, fn, tmpl_args);
  
  return fn;
}

/* Generate an expression for `FN (ARGS)'.  This may change the
   contents of ARGS.

   If DISALLOW_VIRTUAL is true, the call to FN will be not generated
   as a virtual call, even if FN is virtual.  (This flag is set when
   encountering an expression where the function name is explicitly
   qualified.  For example a call to `X::f' never generates a virtual
   call.)

   Returns code for the call.  */

tree
finish_call_expr (tree fn, vec<tree, va_gc> **args, bool disallow_virtual,
		  bool koenig_p, tsubst_flags_t complain)
{
  tree result;
  tree orig_fn;
  vec<tree, va_gc> *orig_args = NULL;

  if (fn == error_mark_node)
    return error_mark_node;

  gcc_assert (!TYPE_P (fn));

  orig_fn = fn;

  if (processing_template_decl)
    {
      /* If the call expression is dependent, build a CALL_EXPR node
	 with no type; type_dependent_expression_p recognizes
	 expressions with no type as being dependent.  */
      if (type_dependent_expression_p (fn)
	  || any_type_dependent_arguments_p (*args)
	  /* For a non-static member function that doesn't have an
	     explicit object argument, we need to specifically
	     test the type dependency of the "this" pointer because it
	     is not included in *ARGS even though it is considered to
	     be part of the list of arguments.  Note that this is
	     related to CWG issues 515 and 1005.  */
	  || (TREE_CODE (fn) != COMPONENT_REF
	      && non_static_member_function_p (fn)
	      && current_class_ref
	      && type_dependent_expression_p (current_class_ref)))
	{
	  result = build_nt_call_vec (fn, *args);
	  SET_EXPR_LOCATION (result, EXPR_LOC_OR_LOC (fn, input_location));
	  KOENIG_LOOKUP_P (result) = koenig_p;
	  if (cfun)
	    {
	      do
		{
		  tree fndecl = OVL_CURRENT (fn);
		  if (TREE_CODE (fndecl) != FUNCTION_DECL
		      || !TREE_THIS_VOLATILE (fndecl))
		    break;
		  fn = OVL_NEXT (fn);
		}
	      while (fn);
	      if (!fn)
		current_function_returns_abnormally = 1;
	    }
	  return result;
	}
      orig_args = make_tree_vector_copy (*args);
      if (!BASELINK_P (fn)
	  && TREE_CODE (fn) != PSEUDO_DTOR_EXPR
	  && TREE_TYPE (fn) != unknown_type_node)
	fn = build_non_dependent_expr (fn);
      make_args_non_dependent (*args);
    }

  if (TREE_CODE (fn) == COMPONENT_REF)
    {
      tree member = TREE_OPERAND (fn, 1);
      if (BASELINK_P (member))
	{
	  tree object = TREE_OPERAND (fn, 0);
	  return build_new_method_call (object, member,
					args, NULL_TREE,
                                        (disallow_virtual
                                         ? LOOKUP_NORMAL | LOOKUP_NONVIRTUAL
					 : LOOKUP_NORMAL),
					/*fn_p=*/NULL,
					complain);
	}
    }

  /* Per 13.3.1.1, '(&f)(...)' is the same as '(f)(...)'.  */
  if (TREE_CODE (fn) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (fn, 0)) == OVERLOAD)
    fn = TREE_OPERAND (fn, 0);

  if (is_overloaded_fn (fn))
    fn = baselink_for_fns (fn);

  result = NULL_TREE;
  if (BASELINK_P (fn))
    {
      tree object;

      /* A call to a member function.  From [over.call.func]:

	   If the keyword this is in scope and refers to the class of
	   that member function, or a derived class thereof, then the
	   function call is transformed into a qualified function call
	   using (*this) as the postfix-expression to the left of the
	   . operator.... [Otherwise] a contrived object of type T
	   becomes the implied object argument.

	In this situation:

	  struct A { void f(); };
	  struct B : public A {};
	  struct C : public A { void g() { B::f(); }};

	"the class of that member function" refers to `A'.  But 11.2
	[class.access.base] says that we need to convert 'this' to B* as
	part of the access, so we pass 'B' to maybe_dummy_object.  */

      object = maybe_dummy_object (BINFO_TYPE (BASELINK_ACCESS_BINFO (fn)),
				   NULL);

      if (processing_template_decl)
	{
	  if (type_dependent_expression_p (object))
	    {
	      tree ret = build_nt_call_vec (orig_fn, orig_args);
	      release_tree_vector (orig_args);
	      return ret;
	    }
	  object = build_non_dependent_expr (object);
	}

      result = build_new_method_call (object, fn, args, NULL_TREE,
				      (disallow_virtual
				       ? LOOKUP_NORMAL|LOOKUP_NONVIRTUAL
				       : LOOKUP_NORMAL),
				      /*fn_p=*/NULL,
				      complain);
    }
  else if (is_overloaded_fn (fn))
    {
      /* If the function is an overloaded builtin, resolve it.  */
      if (TREE_CODE (fn) == FUNCTION_DECL
	  && (DECL_BUILT_IN_CLASS (fn) == BUILT_IN_NORMAL
	      || DECL_BUILT_IN_CLASS (fn) == BUILT_IN_MD))
	result = resolve_overloaded_builtin (input_location, fn, *args);

      if (!result)
	{
	  if (warn_sizeof_pointer_memaccess
	      && !vec_safe_is_empty (*args)
	      && !processing_template_decl)
	    {
	      location_t sizeof_arg_loc[3];
	      tree sizeof_arg[3];
	      unsigned int i;
	      for (i = 0; i < 3; i++)
		{
		  tree t;

		  sizeof_arg_loc[i] = UNKNOWN_LOCATION;
		  sizeof_arg[i] = NULL_TREE;
		  if (i >= (*args)->length ())
		    continue;
		  t = (**args)[i];
		  if (TREE_CODE (t) != SIZEOF_EXPR)
		    continue;
		  if (SIZEOF_EXPR_TYPE_P (t))
		    sizeof_arg[i] = TREE_TYPE (TREE_OPERAND (t, 0));
		  else
		    sizeof_arg[i] = TREE_OPERAND (t, 0);
		  sizeof_arg_loc[i] = EXPR_LOCATION (t);
		}
	      sizeof_pointer_memaccess_warning
		(sizeof_arg_loc, fn, *args,
		 sizeof_arg, same_type_ignoring_top_level_qualifiers_p);
	    }

	  /* A call to a namespace-scope function.  */
	  result = build_new_function_call (fn, args, koenig_p, complain);
	}
    }
  else if (TREE_CODE (fn) == PSEUDO_DTOR_EXPR)
    {
      if (!vec_safe_is_empty (*args))
	error ("arguments to destructor are not allowed");
      /* Mark the pseudo-destructor call as having side-effects so
	 that we do not issue warnings about its use.  */
      result = build1 (NOP_EXPR,
		       void_type_node,
		       TREE_OPERAND (fn, 0));
      TREE_SIDE_EFFECTS (result) = 1;
    }
  else if (CLASS_TYPE_P (TREE_TYPE (fn)))
    /* If the "function" is really an object of class type, it might
       have an overloaded `operator ()'.  */
    result = build_op_call (fn, args, complain);

  if (!result)
    /* A call where the function is unknown.  */
    result = cp_build_function_call_vec (fn, args, complain);

  if (processing_template_decl && result != error_mark_node)
    {
      if (INDIRECT_REF_P (result))
	result = TREE_OPERAND (result, 0);
      result = build_call_vec (TREE_TYPE (result), orig_fn, orig_args);
      SET_EXPR_LOCATION (result, input_location);
      KOENIG_LOOKUP_P (result) = koenig_p;
      release_tree_vector (orig_args);
      result = convert_from_reference (result);
    }

  if (koenig_p)
    {
      /* Free garbage OVERLOADs from arg-dependent lookup.  */
      tree next = NULL_TREE;
      for (fn = orig_fn;
	   fn && TREE_CODE (fn) == OVERLOAD && OVL_ARG_DEPENDENT (fn);
	   fn = next)
	{
	  if (processing_template_decl)
	    /* In a template, we'll re-use them at instantiation time.  */
	    OVL_ARG_DEPENDENT (fn) = false;
	  else
	    {
	      next = OVL_CHAIN (fn);
	      ggc_free (fn);
	    }
	}
    }

  return result;
}

/* Finish a call to a postfix increment or decrement or EXPR.  (Which
   is indicated by CODE, which should be POSTINCREMENT_EXPR or
   POSTDECREMENT_EXPR.)  */

tree
finish_increment_expr (tree expr, enum tree_code code)
{
  return build_x_unary_op (input_location, code, expr, tf_warning_or_error);
}

/* Finish a use of `this'.  Returns an expression for `this'.  */

tree
finish_this_expr (void)
{
  tree result;

  if (current_class_ptr)
    {
      tree type = TREE_TYPE (current_class_ref);

      /* In a lambda expression, 'this' refers to the captured 'this'.  */
      if (LAMBDA_TYPE_P (type))
        result = lambda_expr_this_capture (CLASSTYPE_LAMBDA_EXPR (type));
      else
        result = current_class_ptr;
    }
  else if (current_function_decl
	   && DECL_STATIC_FUNCTION_P (current_function_decl))
    {
      error ("%<this%> is unavailable for static member functions");
      result = error_mark_node;
    }
  else
    {
      if (current_function_decl)
	error ("invalid use of %<this%> in non-member function");
      else
	error ("invalid use of %<this%> at top level");
      result = error_mark_node;
    }

  /* The keyword 'this' is a prvalue expression.  */
  result = rvalue (result);

  return result;
}

/* Finish a pseudo-destructor expression.  If SCOPE is NULL, the
   expression was of the form `OBJECT.~DESTRUCTOR' where DESTRUCTOR is
   the TYPE for the type given.  If SCOPE is non-NULL, the expression
   was of the form `OBJECT.SCOPE::~DESTRUCTOR'.  */

tree
finish_pseudo_destructor_expr (tree object, tree scope, tree destructor,
			       location_t loc)
{
  if (object == error_mark_node || destructor == error_mark_node)
    return error_mark_node;

  gcc_assert (TYPE_P (destructor));

  if (!processing_template_decl)
    {
      if (scope == error_mark_node)
	{
	  error_at (loc, "invalid qualifying scope in pseudo-destructor name");
	  return error_mark_node;
	}
      if (is_auto (destructor))
	destructor = TREE_TYPE (object);
      if (scope && TYPE_P (scope) && !check_dtor_name (scope, destructor))
	{
	  error_at (loc,
		    "qualified type %qT does not match destructor name ~%qT",
		    scope, destructor);
	  return error_mark_node;
	}


      /* [expr.pseudo] says both:

	   The type designated by the pseudo-destructor-name shall be
	   the same as the object type.

	 and:

	   The cv-unqualified versions of the object type and of the
	   type designated by the pseudo-destructor-name shall be the
	   same type.

	 We implement the more generous second sentence, since that is
	 what most other compilers do.  */
      if (!same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (object),
						      destructor))
	{
	  error_at (loc, "%qE is not of type %qT", object, destructor);
	  return error_mark_node;
	}
    }

  return build3_loc (loc, PSEUDO_DTOR_EXPR, void_type_node, object,
		     scope, destructor);
}

/* Finish an expression of the form CODE EXPR.  */

tree
finish_unary_op_expr (location_t loc, enum tree_code code, tree expr,
		      tsubst_flags_t complain)
{
  tree result = build_x_unary_op (loc, code, expr, complain);
  if ((complain & tf_warning)
      && TREE_OVERFLOW_P (result) && !TREE_OVERFLOW_P (expr))
    overflow_warning (input_location, result);

  return result;
}

/* Finish a compound-literal expression.  TYPE is the type to which
   the CONSTRUCTOR in COMPOUND_LITERAL is being cast.  */

tree
finish_compound_literal (tree type, tree compound_literal,
			 tsubst_flags_t complain)
{
  if (type == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      compound_literal
	= finish_compound_literal (TREE_TYPE (type), compound_literal,
				   complain);
      return cp_build_c_cast (type, compound_literal, complain);
    }

  if (!TYPE_OBJ_P (type))
    {
      if (complain & tf_error)
	error ("compound literal of non-object type %qT", type);
      return error_mark_node;
    }

  if (processing_template_decl)
    {
      TREE_TYPE (compound_literal) = type;
      /* Mark the expression as a compound literal.  */
      TREE_HAS_CONSTRUCTOR (compound_literal) = 1;
      return compound_literal;
    }

  type = complete_type (type);

  if (TYPE_NON_AGGREGATE_CLASS (type))
    {
      /* Trying to deal with a CONSTRUCTOR instead of a TREE_LIST
	 everywhere that deals with function arguments would be a pain, so
	 just wrap it in a TREE_LIST.  The parser set a flag so we know
	 that it came from T{} rather than T({}).  */
      CONSTRUCTOR_IS_DIRECT_INIT (compound_literal) = 1;
      compound_literal = build_tree_list (NULL_TREE, compound_literal);
      return build_functional_cast (type, compound_literal, complain);
    }

  if (TREE_CODE (type) == ARRAY_TYPE
      && check_array_initializer (NULL_TREE, type, compound_literal))
    return error_mark_node;
  compound_literal = reshape_init (type, compound_literal, complain);
  if (SCALAR_TYPE_P (type)
      && !BRACE_ENCLOSED_INITIALIZER_P (compound_literal)
      && (complain & tf_warning_or_error))
    check_narrowing (type, compound_literal);
  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == NULL_TREE)
    {
      cp_complete_array_type_or_error (&type, compound_literal,
				       false, complain);
      if (type == error_mark_node)
	return error_mark_node;
    }
  compound_literal = digest_init (type, compound_literal, complain);
  if (TREE_CODE (compound_literal) == CONSTRUCTOR)
    TREE_HAS_CONSTRUCTOR (compound_literal) = true;
  /* Put static/constant array temporaries in static variables, but always
     represent class temporaries with TARGET_EXPR so we elide copies.  */
  if ((!at_function_scope_p () || CP_TYPE_CONST_P (type))
      && TREE_CODE (type) == ARRAY_TYPE
      && !TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
      && !cp_unevaluated_operand
      && initializer_constant_valid_p (compound_literal, type))
    {
      tree decl = create_temporary_var (type);
      DECL_INITIAL (decl) = compound_literal;
      TREE_STATIC (decl) = 1;
      if (literal_type_p (type) && CP_TYPE_CONST_NON_VOLATILE_P (type))
	{
	  /* 5.19 says that a constant expression can include an
	     lvalue-rvalue conversion applied to "a glvalue of literal type
	     that refers to a non-volatile temporary object initialized
	     with a constant expression".  Rather than try to communicate
	     that this VAR_DECL is a temporary, just mark it constexpr.  */
	  DECL_DECLARED_CONSTEXPR_P (decl) = true;
	  DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl) = true;
	  TREE_CONSTANT (decl) = true;
	}
      cp_apply_type_quals_to_decl (cp_type_quals (type), decl);
      decl = pushdecl_top_level (decl);
      DECL_NAME (decl) = make_anon_name ();
      SET_DECL_ASSEMBLER_NAME (decl, DECL_NAME (decl));
      /* Make sure the destructor is callable.  */
      tree clean = cxx_maybe_build_cleanup (decl, complain);
      if (clean == error_mark_node)
	return error_mark_node;
      return decl;
    }
  else
    return get_target_expr_sfinae (compound_literal, complain);
}

/* Return the declaration for the function-name variable indicated by
   ID.  */

tree
finish_fname (tree id)
{
  tree decl;

  decl = fname_decl (input_location, C_RID_CODE (id), id);
  if (processing_template_decl && current_function_decl)
    decl = DECL_NAME (decl);
  return decl;
}

/* Finish a translation unit.  */

void
finish_translation_unit (void)
{
  /* In case there were missing closebraces,
     get us back to the global binding level.  */
  pop_everything ();
  while (current_namespace != global_namespace)
    pop_namespace ();

  /* Do file scope __FUNCTION__ et al.  */
  finish_fname_decls ();
}

/* Finish a template type parameter, specified as AGGR IDENTIFIER.
   Returns the parameter.  */

tree
finish_template_type_parm (tree aggr, tree identifier)
{
  if (aggr != class_type_node)
    {
      permerror (input_location, "template type parameters must use the keyword %<class%> or %<typename%>");
      aggr = class_type_node;
    }

  return build_tree_list (aggr, identifier);
}

/* Finish a template template parameter, specified as AGGR IDENTIFIER.
   Returns the parameter.  */

tree
finish_template_template_parm (tree aggr, tree identifier)
{
  tree decl = build_decl (input_location,
			  TYPE_DECL, identifier, NULL_TREE);
  tree tmpl = build_lang_decl (TEMPLATE_DECL, identifier, NULL_TREE);
  DECL_TEMPLATE_PARMS (tmpl) = current_template_parms;
  DECL_TEMPLATE_RESULT (tmpl) = decl;
  DECL_ARTIFICIAL (decl) = 1;
  end_template_decl ();

  gcc_assert (DECL_TEMPLATE_PARMS (tmpl));

  check_default_tmpl_args (decl, DECL_TEMPLATE_PARMS (tmpl), 
			   /*is_primary=*/true, /*is_partial=*/false,
			   /*is_friend=*/0);

  return finish_template_type_parm (aggr, tmpl);
}

/* ARGUMENT is the default-argument value for a template template
   parameter.  If ARGUMENT is invalid, issue error messages and return
   the ERROR_MARK_NODE.  Otherwise, ARGUMENT itself is returned.  */

tree
check_template_template_default_arg (tree argument)
{
  if (TREE_CODE (argument) != TEMPLATE_DECL
      && TREE_CODE (argument) != TEMPLATE_TEMPLATE_PARM
      && TREE_CODE (argument) != UNBOUND_CLASS_TEMPLATE)
    {
      if (TREE_CODE (argument) == TYPE_DECL)
	error ("invalid use of type %qT as a default value for a template "
	       "template-parameter", TREE_TYPE (argument));
      else
	error ("invalid default argument for a template template parameter");
      return error_mark_node;
    }

  return argument;
}

/* Begin a class definition, as indicated by T.  */

tree
begin_class_definition (tree t)
{
  if (error_operand_p (t) || error_operand_p (TYPE_MAIN_DECL (t)))
    return error_mark_node;

  if (processing_template_parmlist)
    {
      error ("definition of %q#T inside template parameter list", t);
      return error_mark_node;
    }

  /* According to the C++ ABI, decimal classes defined in ISO/IEC TR 24733
     are passed the same as decimal scalar types.  */
  if (TREE_CODE (t) == RECORD_TYPE
      && !processing_template_decl)
    {
      tree ns = TYPE_CONTEXT (t);
      if (ns && TREE_CODE (ns) == NAMESPACE_DECL
	  && DECL_CONTEXT (ns) == std_node
	  && DECL_NAME (ns)
	  && !strcmp (IDENTIFIER_POINTER (DECL_NAME (ns)), "decimal"))
	{
	  const char *n = TYPE_NAME_STRING (t);
	  if ((strcmp (n, "decimal32") == 0)
	      || (strcmp (n, "decimal64") == 0)
	      || (strcmp (n, "decimal128") == 0))
	    TYPE_TRANSPARENT_AGGR (t) = 1;
	}
    }

  /* A non-implicit typename comes from code like:

       template <typename T> struct A {
	 template <typename U> struct A<T>::B ...

     This is erroneous.  */
  else if (TREE_CODE (t) == TYPENAME_TYPE)
    {
      error ("invalid definition of qualified type %qT", t);
      t = error_mark_node;
    }

  if (t == error_mark_node || ! MAYBE_CLASS_TYPE_P (t))
    {
      t = make_class_type (RECORD_TYPE);
      pushtag (make_anon_name (), t, /*tag_scope=*/ts_current);
    }

  if (TYPE_BEING_DEFINED (t))
    {
      t = make_class_type (TREE_CODE (t));
      pushtag (TYPE_IDENTIFIER (t), t, /*tag_scope=*/ts_current);
    }
  maybe_process_partial_specialization (t);
  pushclass (t);
  TYPE_BEING_DEFINED (t) = 1;

  if (flag_pack_struct)
    {
      tree v;
      TYPE_PACKED (t) = 1;
      /* Even though the type is being defined for the first time
	 here, there might have been a forward declaration, so there
	 might be cv-qualified variants of T.  */
      for (v = TYPE_NEXT_VARIANT (t); v; v = TYPE_NEXT_VARIANT (v))
	TYPE_PACKED (v) = 1;
    }
  /* Reset the interface data, at the earliest possible
     moment, as it might have been set via a class foo;
     before.  */
  if (! TYPE_ANONYMOUS_P (t))
    {
      struct c_fileinfo *finfo = \
	get_fileinfo (LOCATION_FILE (input_location));
      CLASSTYPE_INTERFACE_ONLY (t) = finfo->interface_only;
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X
	(t, finfo->interface_unknown);
    }
  reset_specialization();

  /* Make a declaration for this class in its own scope.  */
  build_self_reference ();

  return t;
}

/* Finish the member declaration given by DECL.  */

void
finish_member_declaration (tree decl)
{
  if (decl == error_mark_node || decl == NULL_TREE)
    return;

  if (decl == void_type_node)
    /* The COMPONENT was a friend, not a member, and so there's
       nothing for us to do.  */
    return;

  /* We should see only one DECL at a time.  */
  gcc_assert (DECL_CHAIN (decl) == NULL_TREE);

  /* Set up access control for DECL.  */
  TREE_PRIVATE (decl)
    = (current_access_specifier == access_private_node);
  TREE_PROTECTED (decl)
    = (current_access_specifier == access_protected_node);
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    {
      TREE_PRIVATE (DECL_TEMPLATE_RESULT (decl)) = TREE_PRIVATE (decl);
      TREE_PROTECTED (DECL_TEMPLATE_RESULT (decl)) = TREE_PROTECTED (decl);
    }

  /* Mark the DECL as a member of the current class, unless it's
     a member of an enumeration.  */
  if (TREE_CODE (decl) != CONST_DECL)
    DECL_CONTEXT (decl) = current_class_type;

  /* Check for bare parameter packs in the member variable declaration.  */
  if (TREE_CODE (decl) == FIELD_DECL)
    {
      if (check_for_bare_parameter_packs (TREE_TYPE (decl)))
        TREE_TYPE (decl) = error_mark_node;
      if (check_for_bare_parameter_packs (DECL_ATTRIBUTES (decl)))
        DECL_ATTRIBUTES (decl) = NULL_TREE;
    }

  /* [dcl.link]

     A C language linkage is ignored for the names of class members
     and the member function type of class member functions.  */
  if (DECL_LANG_SPECIFIC (decl) && DECL_LANGUAGE (decl) == lang_c)
    SET_DECL_LANGUAGE (decl, lang_cplusplus);

  /* Put functions on the TYPE_METHODS list and everything else on the
     TYPE_FIELDS list.  Note that these are built up in reverse order.
     We reverse them (to obtain declaration order) in finish_struct.  */
  if (DECL_DECLARES_FUNCTION_P (decl))
    {
      /* We also need to add this function to the
	 CLASSTYPE_METHOD_VEC.  */
      if (add_method (current_class_type, decl, NULL_TREE))
	{
	  DECL_CHAIN (decl) = TYPE_METHODS (current_class_type);
	  TYPE_METHODS (current_class_type) = decl;

	  maybe_add_class_template_decl_list (current_class_type, decl,
					      /*friend_p=*/0);
	}
    }
  /* Enter the DECL into the scope of the class, if the class
     isn't a closure (whose fields are supposed to be unnamed).  */
  else if (CLASSTYPE_LAMBDA_EXPR (current_class_type)
	   || pushdecl_class_level (decl))
    {
      if (TREE_CODE (decl) == USING_DECL)
	{
	  /* For now, ignore class-scope USING_DECLS, so that
	     debugging backends do not see them. */
	  DECL_IGNORED_P (decl) = 1;
	}

      /* All TYPE_DECLs go at the end of TYPE_FIELDS.  Ordinary fields
	 go at the beginning.  The reason is that lookup_field_1
	 searches the list in order, and we want a field name to
	 override a type name so that the "struct stat hack" will
	 work.  In particular:

	   struct S { enum E { }; int E } s;
	   s.E = 3;

	 is valid.  In addition, the FIELD_DECLs must be maintained in
	 declaration order so that class layout works as expected.
	 However, we don't need that order until class layout, so we
	 save a little time by putting FIELD_DECLs on in reverse order
	 here, and then reversing them in finish_struct_1.  (We could
	 also keep a pointer to the correct insertion points in the
	 list.)  */

      if (TREE_CODE (decl) == TYPE_DECL)
	TYPE_FIELDS (current_class_type)
	  = chainon (TYPE_FIELDS (current_class_type), decl);
      else
	{
	  DECL_CHAIN (decl) = TYPE_FIELDS (current_class_type);
	  TYPE_FIELDS (current_class_type) = decl;
	}

      maybe_add_class_template_decl_list (current_class_type, decl,
					  /*friend_p=*/0);
    }

  if (pch_file)
    note_decl_for_pch (decl);
}

/* DECL has been declared while we are building a PCH file.  Perform
   actions that we might normally undertake lazily, but which can be
   performed now so that they do not have to be performed in
   translation units which include the PCH file.  */

void
note_decl_for_pch (tree decl)
{
  gcc_assert (pch_file);

  /* There's a good chance that we'll have to mangle names at some
     point, even if only for emission in debugging information.  */
  if (VAR_OR_FUNCTION_DECL_P (decl)
      && !processing_template_decl)
    mangle_decl (decl);
}

/* Finish processing a complete template declaration.  The PARMS are
   the template parameters.  */

void
finish_template_decl (tree parms)
{
  if (parms)
    end_template_decl ();
  else
    end_specialization ();
}

/* Finish processing a template-id (which names a type) of the form
   NAME < ARGS >.  Return the TYPE_DECL for the type named by the
   template-id.  If ENTERING_SCOPE is nonzero we are about to enter
   the scope of template-id indicated.  */

tree
finish_template_type (tree name, tree args, int entering_scope)
{
  tree type;

  type = lookup_template_class (name, args,
				NULL_TREE, NULL_TREE, entering_scope,
				tf_warning_or_error | tf_user);
  if (type == error_mark_node)
    return type;
  else if (CLASS_TYPE_P (type) && !alias_type_or_template_p (type))
    return TYPE_STUB_DECL (type);
  else
    return TYPE_NAME (type);
}

/* Finish processing a BASE_CLASS with the indicated ACCESS_SPECIFIER.
   Return a TREE_LIST containing the ACCESS_SPECIFIER and the
   BASE_CLASS, or NULL_TREE if an error occurred.  The
   ACCESS_SPECIFIER is one of
   access_{default,public,protected_private}_node.  For a virtual base
   we set TREE_TYPE.  */

tree
finish_base_specifier (tree base, tree access, bool virtual_p)
{
  tree result;

  if (base == error_mark_node)
    {
      error ("invalid base-class specification");
      result = NULL_TREE;
    }
  else if (! MAYBE_CLASS_TYPE_P (base))
    {
      error ("%qT is not a class type", base);
      result = NULL_TREE;
    }
  else
    {
      if (cp_type_quals (base) != 0)
	{
	  /* DR 484: Can a base-specifier name a cv-qualified
	     class type?  */
	  base = TYPE_MAIN_VARIANT (base);
	}
      result = build_tree_list (access, base);
      if (virtual_p)
	TREE_TYPE (result) = integer_type_node;
    }

  return result;
}

/* If FNS is a member function, a set of member functions, or a
   template-id referring to one or more member functions, return a
   BASELINK for FNS, incorporating the current access context.
   Otherwise, return FNS unchanged.  */

tree
baselink_for_fns (tree fns)
{
  tree scope;
  tree cl;

  if (BASELINK_P (fns) 
      || error_operand_p (fns))
    return fns;

  scope = ovl_scope (fns);
  if (!CLASS_TYPE_P (scope))
    return fns;

  cl = currently_open_derived_class (scope);
  if (!cl)
    cl = scope;
  cl = TYPE_BINFO (cl);
  return build_baselink (cl, cl, fns, /*optype=*/NULL_TREE);
}

/* Returns true iff DECL is a variable from a function outside
   the current one.  */

static bool
outer_var_p (tree decl)
{
  return ((VAR_P (decl) || TREE_CODE (decl) == PARM_DECL)
	  && DECL_FUNCTION_SCOPE_P (decl)
	  && (DECL_CONTEXT (decl) != current_function_decl
	      || parsing_nsdmi ()));
}

/* As above, but also checks that DECL is automatic.  */

static bool
outer_automatic_var_p (tree decl)
{
  return (outer_var_p (decl)
	  && !TREE_STATIC (decl));
}

/* ID_EXPRESSION is a representation of parsed, but unprocessed,
   id-expression.  (See cp_parser_id_expression for details.)  SCOPE,
   if non-NULL, is the type or namespace used to explicitly qualify
   ID_EXPRESSION.  DECL is the entity to which that name has been
   resolved.

   *CONSTANT_EXPRESSION_P is true if we are presently parsing a
   constant-expression.  In that case, *NON_CONSTANT_EXPRESSION_P will
   be set to true if this expression isn't permitted in a
   constant-expression, but it is otherwise not set by this function.
   *ALLOW_NON_CONSTANT_EXPRESSION_P is true if we are parsing a
   constant-expression, but a non-constant expression is also
   permissible.

   DONE is true if this expression is a complete postfix-expression;
   it is false if this expression is followed by '->', '[', '(', etc.
   ADDRESS_P is true iff this expression is the operand of '&'.
   TEMPLATE_P is true iff the qualified-id was of the form
   "A::template B".  TEMPLATE_ARG_P is true iff this qualified name
   appears as a template argument.

   If an error occurs, and it is the kind of error that might cause
   the parser to abort a tentative parse, *ERROR_MSG is filled in.  It
   is the caller's responsibility to issue the message.  *ERROR_MSG
   will be a string with static storage duration, so the caller need
   not "free" it.

   Return an expression for the entity, after issuing appropriate
   diagnostics.  This function is also responsible for transforming a
   reference to a non-static member into a COMPONENT_REF that makes
   the use of "this" explicit.

   Upon return, *IDK will be filled in appropriately.  */
tree
finish_id_expression (tree id_expression,
		      tree decl,
		      tree scope,
		      cp_id_kind *idk,
		      bool integral_constant_expression_p,
		      bool allow_non_integral_constant_expression_p,
		      bool *non_integral_constant_expression_p,
		      bool template_p,
		      bool done,
		      bool address_p,
		      bool template_arg_p,
		      const char **error_msg,
		      location_t location)
{
  decl = strip_using_decl (decl);

  /* Initialize the output parameters.  */
  *idk = CP_ID_KIND_NONE;
  *error_msg = NULL;

  if (id_expression == error_mark_node)
    return error_mark_node;
  /* If we have a template-id, then no further lookup is
     required.  If the template-id was for a template-class, we
     will sometimes have a TYPE_DECL at this point.  */
  else if (TREE_CODE (decl) == TEMPLATE_ID_EXPR
	   || TREE_CODE (decl) == TYPE_DECL)
    ;
  /* Look up the name.  */
  else
    {
      if (decl == error_mark_node)
	{
	  /* Name lookup failed.  */
	  if (scope
	      && (!TYPE_P (scope)
		  || (!dependent_type_p (scope)
		      && !(identifier_p (id_expression)
			   && IDENTIFIER_TYPENAME_P (id_expression)
			   && dependent_type_p (TREE_TYPE (id_expression))))))
	    {
	      /* If the qualifying type is non-dependent (and the name
		 does not name a conversion operator to a dependent
		 type), issue an error.  */
	      qualified_name_lookup_error (scope, id_expression, decl, location);
	      return error_mark_node;
	    }
	  else if (!scope)
	    {
	      /* It may be resolved via Koenig lookup.  */
	      *idk = CP_ID_KIND_UNQUALIFIED;
	      return id_expression;
	    }
	  else
	    decl = id_expression;
	}
      /* If DECL is a variable that would be out of scope under
	 ANSI/ISO rules, but in scope in the ARM, name lookup
	 will succeed.  Issue a diagnostic here.  */
      else
	decl = check_for_out_of_scope_variable (decl);

      /* Remember that the name was used in the definition of
	 the current class so that we can check later to see if
	 the meaning would have been different after the class
	 was entirely defined.  */
      if (!scope && decl != error_mark_node && identifier_p (id_expression))
	maybe_note_name_used_in_class (id_expression, decl);

      /* Disallow uses of local variables from containing functions, except
	 within lambda-expressions.  */
      if (!outer_var_p (decl))
	/* OK */;
      else if (TREE_STATIC (decl)
	       /* It's not a use (3.2) if we're in an unevaluated context.  */
	       || cp_unevaluated_operand)
	{
	  if (processing_template_decl)
	    /* For a use of an outer static/unevaluated var, return the id
	       so that we'll look it up again in the instantiation.  */
	    return id_expression;
	}
      else
	{
	  tree context = DECL_CONTEXT (decl);
	  tree containing_function = current_function_decl;
	  tree lambda_stack = NULL_TREE;
	  tree lambda_expr = NULL_TREE;
	  tree initializer = convert_from_reference (decl);

	  /* Mark it as used now even if the use is ill-formed.  */
	  mark_used (decl);

	  /* Core issue 696: "[At the July 2009 meeting] the CWG expressed
	     support for an approach in which a reference to a local
	     [constant] automatic variable in a nested class or lambda body
	     would enter the expression as an rvalue, which would reduce
	     the complexity of the problem"

	     FIXME update for final resolution of core issue 696.  */
	  if (decl_constant_var_p (decl))
	    {
	      if (processing_template_decl)
		/* In a template, the constant value may not be in a usable
		   form, so look it up again at instantiation time.  */
		return id_expression;
	      else
		return integral_constant_value (decl);
	    }

	  if (parsing_nsdmi ())
	    containing_function = NULL_TREE;
	  /* If we are in a lambda function, we can move out until we hit
	     1. the context,
	     2. a non-lambda function, or
	     3. a non-default capturing lambda function.  */
	  else while (context != containing_function
		      && LAMBDA_FUNCTION_P (containing_function))
	    {
	      lambda_expr = CLASSTYPE_LAMBDA_EXPR
		(DECL_CONTEXT (containing_function));

	      if (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda_expr)
		  == CPLD_NONE)
		break;

	      lambda_stack = tree_cons (NULL_TREE,
					lambda_expr,
					lambda_stack);

	      containing_function
		= decl_function_context (containing_function);
	    }

	  if (lambda_expr && TREE_CODE (decl) == VAR_DECL
	      && DECL_ANON_UNION_VAR_P (decl))
	    {
	      error ("cannot capture member %qD of anonymous union", decl);
	      return error_mark_node;
	    }
	  if (context == containing_function)
	    {
	      decl = add_default_capture (lambda_stack,
					  /*id=*/DECL_NAME (decl),
					  initializer);
	    }
	  else if (lambda_expr)
	    {
	      error ("%qD is not captured", decl);
	      return error_mark_node;
	    }
	  else
	    {
	      error (VAR_P (decl)
		     ? G_("use of local variable with automatic storage from containing function")
		     : G_("use of parameter from containing function"));
	      inform (input_location, "%q+#D declared here", decl);
	      return error_mark_node;
	    }
	}

      /* Also disallow uses of function parameters outside the function
	 body, except inside an unevaluated context (i.e. decltype).  */
      if (TREE_CODE (decl) == PARM_DECL
	  && DECL_CONTEXT (decl) == NULL_TREE
	  && !cp_unevaluated_operand)
	{
	  error ("use of parameter %qD outside function body", decl);
	  return error_mark_node;
	}
    }

  /* If we didn't find anything, or what we found was a type,
     then this wasn't really an id-expression.  */
  if (TREE_CODE (decl) == TEMPLATE_DECL
      && !DECL_FUNCTION_TEMPLATE_P (decl))
    {
      *error_msg = "missing template arguments";
      return error_mark_node;
    }
  else if (TREE_CODE (decl) == TYPE_DECL
	   || TREE_CODE (decl) == NAMESPACE_DECL)
    {
      *error_msg = "expected primary-expression";
      return error_mark_node;
    }

  /* If the name resolved to a template parameter, there is no
     need to look it up again later.  */
  if ((TREE_CODE (decl) == CONST_DECL && DECL_TEMPLATE_PARM_P (decl))
      || TREE_CODE (decl) == TEMPLATE_PARM_INDEX)
    {
      tree r;

      *idk = CP_ID_KIND_NONE;
      if (TREE_CODE (decl) == TEMPLATE_PARM_INDEX)
	decl = TEMPLATE_PARM_DECL (decl);
      r = convert_from_reference (DECL_INITIAL (decl));

      if (integral_constant_expression_p
	  && !dependent_type_p (TREE_TYPE (decl))
	  && !(INTEGRAL_OR_ENUMERATION_TYPE_P (TREE_TYPE (r))))
	{
	  if (!allow_non_integral_constant_expression_p)
	    error ("template parameter %qD of type %qT is not allowed in "
		   "an integral constant expression because it is not of "
		   "integral or enumeration type", decl, TREE_TYPE (decl));
	  *non_integral_constant_expression_p = true;
	}
      return r;
    }
  else
    {
      bool dependent_p;

      /* If the declaration was explicitly qualified indicate
	 that.  The semantics of `A::f(3)' are different than
	 `f(3)' if `f' is virtual.  */
      *idk = (scope
	      ? CP_ID_KIND_QUALIFIED
	      : (TREE_CODE (decl) == TEMPLATE_ID_EXPR
		 ? CP_ID_KIND_TEMPLATE_ID
		 : CP_ID_KIND_UNQUALIFIED));


      /* [temp.dep.expr]

	 An id-expression is type-dependent if it contains an
	 identifier that was declared with a dependent type.

	 The standard is not very specific about an id-expression that
	 names a set of overloaded functions.  What if some of them
	 have dependent types and some of them do not?  Presumably,
	 such a name should be treated as a dependent name.  */
      /* Assume the name is not dependent.  */
      dependent_p = false;
      if (!processing_template_decl)
	/* No names are dependent outside a template.  */
	;
      else if (TREE_CODE (decl) == CONST_DECL)
	/* We don't want to treat enumerators as dependent.  */
	;
      /* A template-id where the name of the template was not resolved
	 is definitely dependent.  */
      else if (TREE_CODE (decl) == TEMPLATE_ID_EXPR
	       && (identifier_p (TREE_OPERAND (decl, 0))))
	dependent_p = true;
      /* For anything except an overloaded function, just check its
	 type.  */
      else if (!is_overloaded_fn (decl))
	dependent_p
	  = dependent_type_p (TREE_TYPE (decl));
      /* For a set of overloaded functions, check each of the
	 functions.  */
      else
	{
	  tree fns = decl;

	  if (BASELINK_P (fns))
	    fns = BASELINK_FUNCTIONS (fns);

	  /* For a template-id, check to see if the template
	     arguments are dependent.  */
	  if (TREE_CODE (fns) == TEMPLATE_ID_EXPR)
	    {
	      tree args = TREE_OPERAND (fns, 1);
	      dependent_p = any_dependent_template_arguments_p (args);
	      /* The functions are those referred to by the
		 template-id.  */
	      fns = TREE_OPERAND (fns, 0);
	    }

	  /* If there are no dependent template arguments, go through
	     the overloaded functions.  */
	  while (fns && !dependent_p)
	    {
	      tree fn = OVL_CURRENT (fns);

	      /* Member functions of dependent classes are
		 dependent.  */
	      if (TREE_CODE (fn) == FUNCTION_DECL
		  && type_dependent_expression_p (fn))
		dependent_p = true;
	      else if (TREE_CODE (fn) == TEMPLATE_DECL
		       && dependent_template_p (fn))
		dependent_p = true;

	      fns = OVL_NEXT (fns);
	    }
	}

      /* If the name was dependent on a template parameter, we will
	 resolve the name at instantiation time.  */
      if (dependent_p)
	{
	  /* Create a SCOPE_REF for qualified names, if the scope is
	     dependent.  */
	  if (scope)
	    {
	      if (TYPE_P (scope))
		{
		  if (address_p && done)
		    decl = finish_qualified_id_expr (scope, decl,
						     done, address_p,
						     template_p,
						     template_arg_p,
						     tf_warning_or_error);
		  else
		    {
		      tree type = NULL_TREE;
		      if (DECL_P (decl) && !dependent_scope_p (scope))
			type = TREE_TYPE (decl);
		      decl = build_qualified_name (type,
						   scope,
						   id_expression,
						   template_p);
		    }
		}
	      if (TREE_TYPE (decl))
		decl = convert_from_reference (decl);
	      return decl;
	    }
	  /* A TEMPLATE_ID already contains all the information we
	     need.  */
	  if (TREE_CODE (id_expression) == TEMPLATE_ID_EXPR)
	    return id_expression;
	  *idk = CP_ID_KIND_UNQUALIFIED_DEPENDENT;
	  /* If we found a variable, then name lookup during the
	     instantiation will always resolve to the same VAR_DECL
	     (or an instantiation thereof).  */
	  if (VAR_P (decl)
	      || TREE_CODE (decl) == PARM_DECL)
	    {
	      mark_used (decl);
	      return convert_from_reference (decl);
	    }
	  /* The same is true for FIELD_DECL, but we also need to
	     make sure that the syntax is correct.  */
	  else if (TREE_CODE (decl) == FIELD_DECL)
	    {
	      /* Since SCOPE is NULL here, this is an unqualified name.
		 Access checking has been performed during name lookup
		 already.  Turn off checking to avoid duplicate errors.  */
	      push_deferring_access_checks (dk_no_check);
	      decl = finish_non_static_data_member
		       (decl, NULL_TREE,
			/*qualifying_scope=*/NULL_TREE);
	      pop_deferring_access_checks ();
	      return decl;
	    }
	  return id_expression;
	}

      if (TREE_CODE (decl) == NAMESPACE_DECL)
	{
	  error ("use of namespace %qD as expression", decl);
	  return error_mark_node;
	}
      else if (DECL_CLASS_TEMPLATE_P (decl))
	{
	  error ("use of class template %qT as expression", decl);
	  return error_mark_node;
	}
      else if (TREE_CODE (decl) == TREE_LIST)
	{
	  /* Ambiguous reference to base members.  */
	  error ("request for member %qD is ambiguous in "
		 "multiple inheritance lattice", id_expression);
	  print_candidates (decl);
	  return error_mark_node;
	}

      /* Mark variable-like entities as used.  Functions are similarly
	 marked either below or after overload resolution.  */
      if ((VAR_P (decl)
	   || TREE_CODE (decl) == PARM_DECL
	   || TREE_CODE (decl) == CONST_DECL
	   || TREE_CODE (decl) == RESULT_DECL)
	  && !mark_used (decl))
	return error_mark_node;

      /* Only certain kinds of names are allowed in constant
	 expression.  Template parameters have already
	 been handled above.  */
      if (! error_operand_p (decl)
	  && integral_constant_expression_p
	  && ! decl_constant_var_p (decl)
	  && TREE_CODE (decl) != CONST_DECL
	  && ! builtin_valid_in_constant_expr_p (decl))
	{
	  if (!allow_non_integral_constant_expression_p)
	    {
	      error ("%qD cannot appear in a constant-expression", decl);
	      return error_mark_node;
	    }
	  *non_integral_constant_expression_p = true;
	}

      tree wrap;
      if (VAR_P (decl)
	  && !cp_unevaluated_operand
	  && DECL_THREAD_LOCAL_P (decl)
	  && (wrap = get_tls_wrapper_fn (decl)))
	{
	  /* Replace an evaluated use of the thread_local variable with
	     a call to its wrapper.  */
	  decl = build_cxx_call (wrap, 0, NULL, tf_warning_or_error);
	}
      else if (scope)
	{
	  decl = (adjust_result_of_qualified_name_lookup
		  (decl, scope, current_nonlambda_class_type()));

	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    mark_used (decl);

	  if (TYPE_P (scope))
	    decl = finish_qualified_id_expr (scope,
					     decl,
					     done,
					     address_p,
					     template_p,
					     template_arg_p,
					     tf_warning_or_error);
	  else
	    decl = convert_from_reference (decl);
	}
      else if (TREE_CODE (decl) == FIELD_DECL)
	{
	  /* Since SCOPE is NULL here, this is an unqualified name.
	     Access checking has been performed during name lookup
	     already.  Turn off checking to avoid duplicate errors.  */
	  push_deferring_access_checks (dk_no_check);
	  decl = finish_non_static_data_member (decl, NULL_TREE,
						/*qualifying_scope=*/NULL_TREE);
	  pop_deferring_access_checks ();
	}
      else if (is_overloaded_fn (decl))
	{
	  tree first_fn;

	  first_fn = get_first_fn (decl);
	  if (TREE_CODE (first_fn) == TEMPLATE_DECL)
	    first_fn = DECL_TEMPLATE_RESULT (first_fn);

	  if (!really_overloaded_fn (decl)
	      && !mark_used (first_fn))
	    return error_mark_node;

	  if (!template_arg_p
	      && TREE_CODE (first_fn) == FUNCTION_DECL
	      && DECL_FUNCTION_MEMBER_P (first_fn)
	      && !shared_member_p (decl))
	    {
	      /* A set of member functions.  */
	      decl = maybe_dummy_object (DECL_CONTEXT (first_fn), 0);
	      return finish_class_member_access_expr (decl, id_expression,
						      /*template_p=*/false,
						      tf_warning_or_error);
	    }

	  decl = baselink_for_fns (decl);
	}
      else
	{
	  if (DECL_P (decl) && DECL_NONLOCAL (decl)
	      && DECL_CLASS_SCOPE_P (decl))
	    {
	      tree context = context_for_name_lookup (decl); 
	      if (context != current_class_type)
		{
		  tree path = currently_open_derived_class (context);
		  perform_or_defer_access_check (TYPE_BINFO (path),
						 decl, decl,
						 tf_warning_or_error);
		}
	    }

	  decl = convert_from_reference (decl);
	}
    }

  /* Handle references (c++/56130).  */
  tree t = REFERENCE_REF_P (decl) ? TREE_OPERAND (decl, 0) : decl;
  if (TREE_DEPRECATED (t))
    warn_deprecated_use (t, NULL_TREE);

  return decl;
}

/* Implement the __typeof keyword: Return the type of EXPR, suitable for
   use as a type-specifier.  */

tree
finish_typeof (tree expr)
{
  tree type;

  if (type_dependent_expression_p (expr))
    {
      type = cxx_make_type (TYPEOF_TYPE);
      TYPEOF_TYPE_EXPR (type) = expr;
      SET_TYPE_STRUCTURAL_EQUALITY (type);

      return type;
    }

  expr = mark_type_use (expr);

  type = unlowered_expr_type (expr);

  if (!type || type == unknown_type_node)
    {
      error ("type of %qE is unknown", expr);
      return error_mark_node;
    }

  return type;
}

/* Implement the __underlying_type keyword: Return the underlying
   type of TYPE, suitable for use as a type-specifier.  */

tree
finish_underlying_type (tree type)
{
  tree underlying_type;

  if (processing_template_decl)
    {
      underlying_type = cxx_make_type (UNDERLYING_TYPE);
      UNDERLYING_TYPE_TYPE (underlying_type) = type;
      SET_TYPE_STRUCTURAL_EQUALITY (underlying_type);

      return underlying_type;
    }

  complete_type (type);

  if (TREE_CODE (type) != ENUMERAL_TYPE)
    {
      error ("%qT is not an enumeration type", type);
      return error_mark_node;
    }

  underlying_type = ENUM_UNDERLYING_TYPE (type);

  /* Fixup necessary in this case because ENUM_UNDERLYING_TYPE
     includes TYPE_MIN_VALUE and TYPE_MAX_VALUE information.
     See finish_enum_value_list for details.  */
  if (!ENUM_FIXED_UNDERLYING_TYPE_P (type))
    underlying_type
      = c_common_type_for_mode (TYPE_MODE (underlying_type),
				TYPE_UNSIGNED (underlying_type));

  return underlying_type;
}

/* Implement the __direct_bases keyword: Return the direct base classes
   of type */

tree
calculate_direct_bases (tree type)
{
  vec<tree, va_gc> *vector = make_tree_vector();
  tree bases_vec = NULL_TREE;
  vec<tree, va_gc> *base_binfos;
  tree binfo;
  unsigned i;

  complete_type (type);

  if (!NON_UNION_CLASS_TYPE_P (type))
    return make_tree_vec (0);

  base_binfos = BINFO_BASE_BINFOS (TYPE_BINFO (type));

  /* Virtual bases are initialized first */
  for (i = 0; base_binfos->iterate (i, &binfo); i++)
    {
      if (BINFO_VIRTUAL_P (binfo))
       {
         vec_safe_push (vector, binfo);
       }
    }

  /* Now non-virtuals */
  for (i = 0; base_binfos->iterate (i, &binfo); i++)
    {
      if (!BINFO_VIRTUAL_P (binfo))
       {
         vec_safe_push (vector, binfo);
       }
    }


  bases_vec = make_tree_vec (vector->length ());

  for (i = 0; i < vector->length (); ++i)
    {
      TREE_VEC_ELT (bases_vec, i) = BINFO_TYPE ((*vector)[i]);
    }
  return bases_vec;
}

/* Implement the __bases keyword: Return the base classes
   of type */

/* Find morally non-virtual base classes by walking binfo hierarchy */
/* Virtual base classes are handled separately in finish_bases */

static tree
dfs_calculate_bases_pre (tree binfo, void * /*data_*/)
{
  /* Don't walk bases of virtual bases */
  return BINFO_VIRTUAL_P (binfo) ? dfs_skip_bases : NULL_TREE;
}

static tree
dfs_calculate_bases_post (tree binfo, void *data_)
{
  vec<tree, va_gc> **data = ((vec<tree, va_gc> **) data_);
  if (!BINFO_VIRTUAL_P (binfo))
    {
      vec_safe_push (*data, BINFO_TYPE (binfo));
    }
  return NULL_TREE;
}

/* Calculates the morally non-virtual base classes of a class */
static vec<tree, va_gc> *
calculate_bases_helper (tree type)
{
  vec<tree, va_gc> *vector = make_tree_vector();

  /* Now add non-virtual base classes in order of construction */
  dfs_walk_all (TYPE_BINFO (type),
                dfs_calculate_bases_pre, dfs_calculate_bases_post, &vector);
  return vector;
}

tree
calculate_bases (tree type)
{
  vec<tree, va_gc> *vector = make_tree_vector();
  tree bases_vec = NULL_TREE;
  unsigned i;
  vec<tree, va_gc> *vbases;
  vec<tree, va_gc> *nonvbases;
  tree binfo;

  complete_type (type);

  if (!NON_UNION_CLASS_TYPE_P (type))
    return make_tree_vec (0);

  /* First go through virtual base classes */
  for (vbases = CLASSTYPE_VBASECLASSES (type), i = 0;
       vec_safe_iterate (vbases, i, &binfo); i++)
    {
      vec<tree, va_gc> *vbase_bases;
      vbase_bases = calculate_bases_helper (BINFO_TYPE (binfo));
      vec_safe_splice (vector, vbase_bases);
      release_tree_vector (vbase_bases);
    }

  /* Now for the non-virtual bases */
  nonvbases = calculate_bases_helper (type);
  vec_safe_splice (vector, nonvbases);
  release_tree_vector (nonvbases);

  /* Last element is entire class, so don't copy */
  bases_vec = make_tree_vec (vector->length () - 1);

  for (i = 0; i < vector->length () - 1; ++i)
    {
      TREE_VEC_ELT (bases_vec, i) = (*vector)[i];
    }
  release_tree_vector (vector);
  return bases_vec;
}

tree
finish_bases (tree type, bool direct)
{
  tree bases = NULL_TREE;

  if (!processing_template_decl)
    {
      /* Parameter packs can only be used in templates */
      error ("Parameter pack __bases only valid in template declaration");
      return error_mark_node;
    }

  bases = cxx_make_type (BASES);
  BASES_TYPE (bases) = type;
  BASES_DIRECT (bases) = direct;
  SET_TYPE_STRUCTURAL_EQUALITY (bases);

  return bases;
}

/* Perform C++-specific checks for __builtin_offsetof before calling
   fold_offsetof.  */

tree
finish_offsetof (tree expr)
{
  if (TREE_CODE (expr) == PSEUDO_DTOR_EXPR)
    {
      error ("cannot apply %<offsetof%> to destructor %<~%T%>",
	      TREE_OPERAND (expr, 2));
      return error_mark_node;
    }
  if (TREE_CODE (TREE_TYPE (expr)) == FUNCTION_TYPE
      || TREE_CODE (TREE_TYPE (expr)) == METHOD_TYPE
      || TREE_TYPE (expr) == unknown_type_node)
    {
      if (INDIRECT_REF_P (expr))
	error ("second operand of %<offsetof%> is neither a single "
	       "identifier nor a sequence of member accesses and "
	       "array references");
      else
	{
	  if (TREE_CODE (expr) == COMPONENT_REF
	      || TREE_CODE (expr) == COMPOUND_EXPR)
	    expr = TREE_OPERAND (expr, 1);
	  error ("cannot apply %<offsetof%> to member function %qD", expr);
	}
      return error_mark_node;
    }
  if (REFERENCE_REF_P (expr))
    expr = TREE_OPERAND (expr, 0);
  if (TREE_CODE (expr) == COMPONENT_REF)
    {
      tree object = TREE_OPERAND (expr, 0);
      if (!complete_type_or_else (TREE_TYPE (object), object))
	return error_mark_node;
    }
  return fold_offsetof (expr);
}

/* Replace the AGGR_INIT_EXPR at *TP with an equivalent CALL_EXPR.  This
   function is broken out from the above for the benefit of the tree-ssa
   project.  */

void
simplify_aggr_init_expr (tree *tp)
{
  tree aggr_init_expr = *tp;

  /* Form an appropriate CALL_EXPR.  */
  tree fn = AGGR_INIT_EXPR_FN (aggr_init_expr);
  tree slot = AGGR_INIT_EXPR_SLOT (aggr_init_expr);
  tree type = TREE_TYPE (slot);

  tree call_expr;
  enum style_t { ctor, arg, pcc } style;

  if (AGGR_INIT_VIA_CTOR_P (aggr_init_expr))
    style = ctor;
#ifdef PCC_STATIC_STRUCT_RETURN
  else if (1)
    style = pcc;
#endif
  else
    {
      gcc_assert (TREE_ADDRESSABLE (type));
      style = arg;
    }

  call_expr = build_call_array_loc (input_location,
				    TREE_TYPE (TREE_TYPE (TREE_TYPE (fn))),
				    fn,
				    aggr_init_expr_nargs (aggr_init_expr),
				    AGGR_INIT_EXPR_ARGP (aggr_init_expr));
  TREE_NOTHROW (call_expr) = TREE_NOTHROW (aggr_init_expr);

  if (style == ctor)
    {
      /* Replace the first argument to the ctor with the address of the
	 slot.  */
      cxx_mark_addressable (slot);
      CALL_EXPR_ARG (call_expr, 0) =
	build1 (ADDR_EXPR, build_pointer_type (type), slot);
    }
  else if (style == arg)
    {
      /* Just mark it addressable here, and leave the rest to
	 expand_call{,_inline}.  */
      cxx_mark_addressable (slot);
      CALL_EXPR_RETURN_SLOT_OPT (call_expr) = true;
      call_expr = build2 (INIT_EXPR, TREE_TYPE (call_expr), slot, call_expr);
    }
  else if (style == pcc)
    {
      /* If we're using the non-reentrant PCC calling convention, then we
	 need to copy the returned value out of the static buffer into the
	 SLOT.  */
      push_deferring_access_checks (dk_no_check);
      call_expr = build_aggr_init (slot, call_expr,
				   DIRECT_BIND | LOOKUP_ONLYCONVERTING,
                                   tf_warning_or_error);
      pop_deferring_access_checks ();
      call_expr = build2 (COMPOUND_EXPR, TREE_TYPE (slot), call_expr, slot);
    }

  if (AGGR_INIT_ZERO_FIRST (aggr_init_expr))
    {
      tree init = build_zero_init (type, NULL_TREE,
				   /*static_storage_p=*/false);
      init = build2 (INIT_EXPR, void_type_node, slot, init);
      call_expr = build2 (COMPOUND_EXPR, TREE_TYPE (call_expr),
			  init, call_expr);
    }

  *tp = call_expr;
}

/* Emit all thunks to FN that should be emitted when FN is emitted.  */

void
emit_associated_thunks (tree fn)
{
  /* When we use vcall offsets, we emit thunks with the virtual
     functions to which they thunk. The whole point of vcall offsets
     is so that you can know statically the entire set of thunks that
     will ever be needed for a given virtual function, thereby
     enabling you to output all the thunks with the function itself.  */
  if (DECL_VIRTUAL_P (fn)
      /* Do not emit thunks for extern template instantiations.  */
      && ! DECL_REALLY_EXTERN (fn))
    {
      tree thunk;

      for (thunk = DECL_THUNKS (fn); thunk; thunk = DECL_CHAIN (thunk))
	{
	  if (!THUNK_ALIAS (thunk))
	    {
	      use_thunk (thunk, /*emit_p=*/1);
	      if (DECL_RESULT_THUNK_P (thunk))
		{
		  tree probe;

		  for (probe = DECL_THUNKS (thunk);
		       probe; probe = DECL_CHAIN (probe))
		    use_thunk (probe, /*emit_p=*/1);
		}
	    }
	  else
	    gcc_assert (!DECL_THUNKS (thunk));
	}
    }
}

/* Returns true iff FUN is an instantiation of a constexpr function
   template.  */

static inline bool
is_instantiation_of_constexpr (tree fun)
{
  return (DECL_TEMPLOID_INSTANTIATION (fun)
	  && DECL_DECLARED_CONSTEXPR_P (DECL_TEMPLATE_RESULT
					(DECL_TI_TEMPLATE (fun))));
}

/* Generate RTL for FN.  */

bool
expand_or_defer_fn_1 (tree fn)
{
  /* When the parser calls us after finishing the body of a template
     function, we don't really want to expand the body.  */
  if (processing_template_decl)
    {
      /* Normally, collection only occurs in rest_of_compilation.  So,
	 if we don't collect here, we never collect junk generated
	 during the processing of templates until we hit a
	 non-template function.  It's not safe to do this inside a
	 nested class, though, as the parser may have local state that
	 is not a GC root.  */
      if (!function_depth)
	ggc_collect ();
      return false;
    }

  gcc_assert (DECL_SAVED_TREE (fn));

  /* We make a decision about linkage for these functions at the end
     of the compilation.  Until that point, we do not want the back
     end to output them -- but we do want it to see the bodies of
     these functions so that it can inline them as appropriate.  */
  if (DECL_DECLARED_INLINE_P (fn) || DECL_IMPLICIT_INSTANTIATION (fn))
    {
      if (DECL_INTERFACE_KNOWN (fn))
	/* We've already made a decision as to how this function will
	   be handled.  */;
      else if (!at_eof)
	{
	  DECL_EXTERNAL (fn) = 1;
	  DECL_NOT_REALLY_EXTERN (fn) = 1;
	  note_vague_linkage_fn (fn);
	  /* A non-template inline function with external linkage will
	     always be COMDAT.  As we must eventually determine the
	     linkage of all functions, and as that causes writes to
	     the data mapped in from the PCH file, it's advantageous
	     to mark the functions at this point.  */
	  if (!DECL_IMPLICIT_INSTANTIATION (fn))
	    {
	      /* This function must have external linkage, as
		 otherwise DECL_INTERFACE_KNOWN would have been
		 set.  */
	      gcc_assert (TREE_PUBLIC (fn));
	      comdat_linkage (fn);
	      DECL_INTERFACE_KNOWN (fn) = 1;
	    }
	}
      else
	import_export_decl (fn);

      /* If the user wants us to keep all inline functions, then mark
	 this function as needed so that finish_file will make sure to
	 output it later.  Similarly, all dllexport'd functions must
	 be emitted; there may be callers in other DLLs.  */
      if ((flag_keep_inline_functions
	   && DECL_DECLARED_INLINE_P (fn)
	   && !DECL_REALLY_EXTERN (fn))
	  || (flag_keep_inline_dllexport
	      && lookup_attribute ("dllexport", DECL_ATTRIBUTES (fn))))
	{
	  mark_needed (fn);
	  DECL_EXTERNAL (fn) = 0;
	}
    }

  /* If this is a constructor or destructor body, we have to clone
     it.  */
  if (maybe_clone_body (fn))
    {
      /* We don't want to process FN again, so pretend we've written
	 it out, even though we haven't.  */
      TREE_ASM_WRITTEN (fn) = 1;
      /* If this is an instantiation of a constexpr function, keep
	 DECL_SAVED_TREE for explain_invalid_constexpr_fn.  */
      if (!is_instantiation_of_constexpr (fn))
	DECL_SAVED_TREE (fn) = NULL_TREE;
      return false;
    }

  /* There's no reason to do any of the work here if we're only doing
     semantic analysis; this code just generates RTL.  */
  if (flag_syntax_only)
    return false;

  return true;
}

void
expand_or_defer_fn (tree fn)
{
  if (expand_or_defer_fn_1 (fn))
    {
      function_depth++;

      /* Expand or defer, at the whim of the compilation unit manager.  */
      cgraph_finalize_function (fn, function_depth > 1);
      emit_associated_thunks (fn);

      function_depth--;
    }
}

struct nrv_data
{
  tree var;
  tree result;
  hash_table <pointer_hash <tree_node> > visited;
};

/* Helper function for walk_tree, used by finalize_nrv below.  */

static tree
finalize_nrv_r (tree* tp, int* walk_subtrees, void* data)
{
  struct nrv_data *dp = (struct nrv_data *)data;
  tree_node **slot;

  /* No need to walk into types.  There wouldn't be any need to walk into
     non-statements, except that we have to consider STMT_EXPRs.  */
  if (TYPE_P (*tp))
    *walk_subtrees = 0;
  /* Change all returns to just refer to the RESULT_DECL; this is a nop,
     but differs from using NULL_TREE in that it indicates that we care
     about the value of the RESULT_DECL.  */
  else if (TREE_CODE (*tp) == RETURN_EXPR)
    TREE_OPERAND (*tp, 0) = dp->result;
  /* Change all cleanups for the NRV to only run when an exception is
     thrown.  */
  else if (TREE_CODE (*tp) == CLEANUP_STMT
	   && CLEANUP_DECL (*tp) == dp->var)
    CLEANUP_EH_ONLY (*tp) = 1;
  /* Replace the DECL_EXPR for the NRV with an initialization of the
     RESULT_DECL, if needed.  */
  else if (TREE_CODE (*tp) == DECL_EXPR
	   && DECL_EXPR_DECL (*tp) == dp->var)
    {
      tree init;
      if (DECL_INITIAL (dp->var)
	  && DECL_INITIAL (dp->var) != error_mark_node)
	init = build2 (INIT_EXPR, void_type_node, dp->result,
		       DECL_INITIAL (dp->var));
      else
	init = build_empty_stmt (EXPR_LOCATION (*tp));
      DECL_INITIAL (dp->var) = NULL_TREE;
      SET_EXPR_LOCATION (init, EXPR_LOCATION (*tp));
      *tp = init;
    }
  /* And replace all uses of the NRV with the RESULT_DECL.  */
  else if (*tp == dp->var)
    *tp = dp->result;

  /* Avoid walking into the same tree more than once.  Unfortunately, we
     can't just use walk_tree_without duplicates because it would only call
     us for the first occurrence of dp->var in the function body.  */
  slot = dp->visited.find_slot (*tp, INSERT);
  if (*slot)
    *walk_subtrees = 0;
  else
    *slot = *tp;

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Called from finish_function to implement the named return value
   optimization by overriding all the RETURN_EXPRs and pertinent
   CLEANUP_STMTs and replacing all occurrences of VAR with RESULT, the
   RESULT_DECL for the function.  */

void
finalize_nrv (tree *tp, tree var, tree result)
{
  struct nrv_data data;

  /* Copy name from VAR to RESULT.  */
  DECL_NAME (result) = DECL_NAME (var);
  /* Don't forget that we take its address.  */
  TREE_ADDRESSABLE (result) = TREE_ADDRESSABLE (var);
  /* Finally set DECL_VALUE_EXPR to avoid assigning
     a stack slot at -O0 for the original var and debug info
     uses RESULT location for VAR.  */
  SET_DECL_VALUE_EXPR (var, result);
  DECL_HAS_VALUE_EXPR_P (var) = 1;

  data.var = var;
  data.result = result;
  data.visited.create (37);
  cp_walk_tree (tp, finalize_nrv_r, &data, 0);
  data.visited.dispose ();
}

/* Create CP_OMP_CLAUSE_INFO for clause C.  Returns true if it is invalid.  */

bool
cxx_omp_create_clause_info (tree c, tree type, bool need_default_ctor,
			    bool need_copy_ctor, bool need_copy_assignment,
			    bool need_dtor)
{
  int save_errorcount = errorcount;
  tree info, t;

  /* Always allocate 3 elements for simplicity.  These are the
     function decls for the ctor, dtor, and assignment op.
     This layout is known to the three lang hooks,
     cxx_omp_clause_default_init, cxx_omp_clause_copy_init,
     and cxx_omp_clause_assign_op.  */
  info = make_tree_vec (3);
  CP_OMP_CLAUSE_INFO (c) = info;

  if (need_default_ctor || need_copy_ctor)
    {
      if (need_default_ctor)
	t = get_default_ctor (type);
      else
	t = get_copy_ctor (type, tf_warning_or_error);

      if (t && !trivial_fn_p (t))
	TREE_VEC_ELT (info, 0) = t;
    }

  if (need_dtor && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
    TREE_VEC_ELT (info, 1) = get_dtor (type, tf_warning_or_error);

  if (need_copy_assignment)
    {
      t = get_copy_assign (type);

      if (t && !trivial_fn_p (t))
	TREE_VEC_ELT (info, 2) = t;
    }

  return errorcount != save_errorcount;
}

/* Helper function for handle_omp_array_sections.  Called recursively
   to handle multiple array-section-subscripts.  C is the clause,
   T current expression (initially OMP_CLAUSE_DECL), which is either
   a TREE_LIST for array-section-subscript (TREE_PURPOSE is low-bound
   expression if specified, TREE_VALUE length expression if specified,
   TREE_CHAIN is what it has been specified after, or some decl.
   TYPES vector is populated with array section types, MAYBE_ZERO_LEN
   set to true if any of the array-section-subscript could have length
   of zero (explicit or implicit), FIRST_NON_ONE is the index of the
   first array-section-subscript which is known not to have length
   of one.  Given say:
   map(a[:b][2:1][:c][:2][:d][e:f][2:5])
   FIRST_NON_ONE will be 3, array-section-subscript [:b], [2:1] and [:c]
   all are or may have length of 1, array-section-subscript [:2] is the
   first one knonwn not to have length 1.  For array-section-subscript
   <= FIRST_NON_ONE we diagnose non-contiguous arrays if low bound isn't
   0 or length isn't the array domain max + 1, for > FIRST_NON_ONE we
   can if MAYBE_ZERO_LEN is false.  MAYBE_ZERO_LEN will be true in the above
   case though, as some lengths could be zero.  */

static tree
handle_omp_array_sections_1 (tree c, tree t, vec<tree> &types,
			     bool &maybe_zero_len, unsigned int &first_non_one)
{
  tree ret, low_bound, length, type;
  if (TREE_CODE (t) != TREE_LIST)
    {
      if (error_operand_p (t))
	return error_mark_node;
      if (type_dependent_expression_p (t))
	return NULL_TREE;
      if (TREE_CODE (t) != VAR_DECL && TREE_CODE (t) != PARM_DECL)
	{
	  if (processing_template_decl)
	    return NULL_TREE;
	  if (DECL_P (t))
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%qD is not a variable in %qs clause", t,
		      omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	  else
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%qE is not a variable in %qs clause", t,
		      omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	  return error_mark_node;
	}
      else if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND
	       && TREE_CODE (t) == VAR_DECL && DECL_THREAD_LOCAL_P (t))
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "%qD is threadprivate variable in %qs clause", t,
		    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	  return error_mark_node;
	}
      t = convert_from_reference (t);
      return t;
    }

  ret = handle_omp_array_sections_1 (c, TREE_CHAIN (t), types,
				     maybe_zero_len, first_non_one);
  if (ret == error_mark_node || ret == NULL_TREE)
    return ret;

  type = TREE_TYPE (ret);
  low_bound = TREE_PURPOSE (t);
  length = TREE_VALUE (t);
  if ((low_bound && type_dependent_expression_p (low_bound))
      || (length && type_dependent_expression_p (length)))
    return NULL_TREE;

  if (low_bound == error_mark_node || length == error_mark_node)
    return error_mark_node;

  if (low_bound && !INTEGRAL_TYPE_P (TREE_TYPE (low_bound)))
    {
      error_at (OMP_CLAUSE_LOCATION (c),
		"low bound %qE of array section does not have integral type",
		low_bound);
      return error_mark_node;
    }
  if (length && !INTEGRAL_TYPE_P (TREE_TYPE (length)))
    {
      error_at (OMP_CLAUSE_LOCATION (c),
		"length %qE of array section does not have integral type",
		length);
      return error_mark_node;
    }
  if (low_bound
      && TREE_CODE (low_bound) == INTEGER_CST
      && TYPE_PRECISION (TREE_TYPE (low_bound))
	 > TYPE_PRECISION (sizetype))
    low_bound = fold_convert (sizetype, low_bound);
  if (length
      && TREE_CODE (length) == INTEGER_CST
      && TYPE_PRECISION (TREE_TYPE (length))
	 > TYPE_PRECISION (sizetype))
    length = fold_convert (sizetype, length);
  if (low_bound == NULL_TREE)
    low_bound = integer_zero_node;

  if (length != NULL_TREE)
    {
      if (!integer_nonzerop (length))
	maybe_zero_len = true;
      if (first_non_one == types.length ()
	  && (TREE_CODE (length) != INTEGER_CST || integer_onep (length)))
	first_non_one++;
    }
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (length == NULL_TREE
	  && (TYPE_DOMAIN (type) == NULL_TREE
	      || TYPE_MAX_VALUE (TYPE_DOMAIN (type)) == NULL_TREE))
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "for unknown bound array type length expression must "
		    "be specified");
	  return error_mark_node;
	}
      if (TREE_CODE (low_bound) == INTEGER_CST
	  && tree_int_cst_sgn (low_bound) == -1)
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "negative low bound in array section in %qs clause",
		    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	  return error_mark_node;
	}
      if (length != NULL_TREE
	  && TREE_CODE (length) == INTEGER_CST
	  && tree_int_cst_sgn (length) == -1)
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "negative length in array section in %qs clause",
		    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	  return error_mark_node;
	}
      if (TYPE_DOMAIN (type)
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (type))
	  && TREE_CODE (TYPE_MAX_VALUE (TYPE_DOMAIN (type)))
			== INTEGER_CST)
	{
	  tree size = size_binop (PLUS_EXPR,
				  TYPE_MAX_VALUE (TYPE_DOMAIN (type)),
				  size_one_node);
	  if (TREE_CODE (low_bound) == INTEGER_CST)
	    {
	      if (tree_int_cst_lt (size, low_bound))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "low bound %qE above array section size "
			    "in %qs clause", low_bound,
			    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		  return error_mark_node;
		}
	      if (tree_int_cst_equal (size, low_bound))
		maybe_zero_len = true;
	      else if (length == NULL_TREE
		       && first_non_one == types.length ()
		       && tree_int_cst_equal
			    (TYPE_MAX_VALUE (TYPE_DOMAIN (type)),
			     low_bound))
		first_non_one++;
	    }
	  else if (length == NULL_TREE)
	    {
	      maybe_zero_len = true;
	      if (first_non_one == types.length ())
		first_non_one++;
	    }
	  if (length && TREE_CODE (length) == INTEGER_CST)
	    {
	      if (tree_int_cst_lt (size, length))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "length %qE above array section size "
			    "in %qs clause", length,
			    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		  return error_mark_node;
		}
	      if (TREE_CODE (low_bound) == INTEGER_CST)
		{
		  tree lbpluslen
		    = size_binop (PLUS_EXPR,
				  fold_convert (sizetype, low_bound),
				  fold_convert (sizetype, length));
		  if (TREE_CODE (lbpluslen) == INTEGER_CST
		      && tree_int_cst_lt (size, lbpluslen))
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"high bound %qE above array section size "
				"in %qs clause", lbpluslen,
				omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		      return error_mark_node;
		    }
		}
	    }
	}
      else if (length == NULL_TREE)
	{
	  maybe_zero_len = true;
	  if (first_non_one == types.length ())
	    first_non_one++;
	}

      /* For [lb:] we will need to evaluate lb more than once.  */
      if (length == NULL_TREE && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND)
	{
	  tree lb = cp_save_expr (low_bound);
	  if (lb != low_bound)
	    {
	      TREE_PURPOSE (t) = lb;
	      low_bound = lb;
	    }
	}
    }
  else if (TREE_CODE (type) == POINTER_TYPE)
    {
      if (length == NULL_TREE)
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "for pointer type length expression must be specified");
	  return error_mark_node;
	}
      /* If there is a pointer type anywhere but in the very first
	 array-section-subscript, the array section can't be contiguous.  */
      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND
	  && TREE_CODE (TREE_CHAIN (t)) == TREE_LIST)
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "array section is not contiguous in %qs clause",
		    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	  return error_mark_node;
	}
    }
  else
    {
      error_at (OMP_CLAUSE_LOCATION (c),
		"%qE does not have pointer or array type", ret);
      return error_mark_node;
    }
  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND)
    types.safe_push (TREE_TYPE (ret));
  /* We will need to evaluate lb more than once.  */
  tree lb = cp_save_expr (low_bound);
  if (lb != low_bound)
    {
      TREE_PURPOSE (t) = lb;
      low_bound = lb;
    }
  ret = grok_array_decl (OMP_CLAUSE_LOCATION (c), ret, low_bound, false);
  return ret;
}

/* Handle array sections for clause C.  */

static bool
handle_omp_array_sections (tree c)
{
  bool maybe_zero_len = false;
  unsigned int first_non_one = 0;
  auto_vec<tree> types;
  tree first = handle_omp_array_sections_1 (c, OMP_CLAUSE_DECL (c), types,
					    maybe_zero_len, first_non_one);
  if (first == error_mark_node)
    return true;
  if (first == NULL_TREE)
    return false;
  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND)
    {
      tree t = OMP_CLAUSE_DECL (c);
      tree tem = NULL_TREE;
      if (processing_template_decl)
	return false;
      /* Need to evaluate side effects in the length expressions
	 if any.  */
      while (TREE_CODE (t) == TREE_LIST)
	{
	  if (TREE_VALUE (t) && TREE_SIDE_EFFECTS (TREE_VALUE (t)))
	    {
	      if (tem == NULL_TREE)
		tem = TREE_VALUE (t);
	      else
		tem = build2 (COMPOUND_EXPR, TREE_TYPE (tem),
			      TREE_VALUE (t), tem);
	    }
	  t = TREE_CHAIN (t);
	}
      if (tem)
	first = build2 (COMPOUND_EXPR, TREE_TYPE (first), tem, first);
      OMP_CLAUSE_DECL (c) = first;
    }
  else
    {
      unsigned int num = types.length (), i;
      tree t, side_effects = NULL_TREE, size = NULL_TREE;
      tree condition = NULL_TREE;

      if (int_size_in_bytes (TREE_TYPE (first)) <= 0)
	maybe_zero_len = true;
      if (processing_template_decl && maybe_zero_len)
	return false;

      for (i = num, t = OMP_CLAUSE_DECL (c); i > 0;
	   t = TREE_CHAIN (t))
	{
	  tree low_bound = TREE_PURPOSE (t);
	  tree length = TREE_VALUE (t);

	  i--;
	  if (low_bound
	      && TREE_CODE (low_bound) == INTEGER_CST
	      && TYPE_PRECISION (TREE_TYPE (low_bound))
		 > TYPE_PRECISION (sizetype))
	    low_bound = fold_convert (sizetype, low_bound);
	  if (length
	      && TREE_CODE (length) == INTEGER_CST
	      && TYPE_PRECISION (TREE_TYPE (length))
		 > TYPE_PRECISION (sizetype))
	    length = fold_convert (sizetype, length);
	  if (low_bound == NULL_TREE)
	    low_bound = integer_zero_node;
	  if (!maybe_zero_len && i > first_non_one)
	    {
	      if (integer_nonzerop (low_bound))
		goto do_warn_noncontiguous;
	      if (length != NULL_TREE
		  && TREE_CODE (length) == INTEGER_CST
		  && TYPE_DOMAIN (types[i])
		  && TYPE_MAX_VALUE (TYPE_DOMAIN (types[i]))
		  && TREE_CODE (TYPE_MAX_VALUE (TYPE_DOMAIN (types[i])))
		     == INTEGER_CST)
		{
		  tree size;
		  size = size_binop (PLUS_EXPR,
				     TYPE_MAX_VALUE (TYPE_DOMAIN (types[i])),
				     size_one_node);
		  if (!tree_int_cst_equal (length, size))
		    {
		     do_warn_noncontiguous:
		      error_at (OMP_CLAUSE_LOCATION (c),
				"array section is not contiguous in %qs "
				"clause",
				omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		      return true;
		    }
		}
	      if (!processing_template_decl
		  && length != NULL_TREE
		  && TREE_SIDE_EFFECTS (length))
		{
		  if (side_effects == NULL_TREE)
		    side_effects = length;
		  else
		    side_effects = build2 (COMPOUND_EXPR,
					   TREE_TYPE (side_effects),
					   length, side_effects);
		}
	    }
	  else if (processing_template_decl)
	    continue;
	  else
	    {
	      tree l;

	      if (i > first_non_one && length && integer_nonzerop (length))
		continue;
	      if (length)
		l = fold_convert (sizetype, length);
	      else
		{
		  l = size_binop (PLUS_EXPR,
				  TYPE_MAX_VALUE (TYPE_DOMAIN (types[i])),
				  size_one_node);
		  l = size_binop (MINUS_EXPR, l,
				  fold_convert (sizetype, low_bound));
		}
	      if (i > first_non_one)
		{
		  l = fold_build2 (NE_EXPR, boolean_type_node, l,
				   size_zero_node);
		  if (condition == NULL_TREE)
		    condition = l;
		  else
		    condition = fold_build2 (BIT_AND_EXPR, boolean_type_node,
					     l, condition);
		}
	      else if (size == NULL_TREE)
		{
		  size = size_in_bytes (TREE_TYPE (types[i]));
		  size = size_binop (MULT_EXPR, size, l);
		  if (condition)
		    size = fold_build3 (COND_EXPR, sizetype, condition,
					size, size_zero_node);
		}
	      else
		size = size_binop (MULT_EXPR, size, l);
	    }
	}
      if (!processing_template_decl)
	{
	  if (side_effects)
	    size = build2 (COMPOUND_EXPR, sizetype, side_effects, size);
	  OMP_CLAUSE_DECL (c) = first;
	  OMP_CLAUSE_SIZE (c) = size;
	  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP)
	    return false;
	  tree c2 = build_omp_clause (OMP_CLAUSE_LOCATION (c),
				      OMP_CLAUSE_MAP);
	  OMP_CLAUSE_MAP_KIND (c2) = OMP_CLAUSE_MAP_POINTER;
	  if (!cxx_mark_addressable (t))
	    return false;
	  OMP_CLAUSE_DECL (c2) = t;
	  t = build_fold_addr_expr (first);
	  t = fold_convert_loc (OMP_CLAUSE_LOCATION (c),
				ptrdiff_type_node, t);
	  tree ptr = OMP_CLAUSE_DECL (c2);
	  ptr = convert_from_reference (ptr);
	  if (!POINTER_TYPE_P (TREE_TYPE (ptr)))
	    ptr = build_fold_addr_expr (ptr);
	  t = fold_build2_loc (OMP_CLAUSE_LOCATION (c), MINUS_EXPR,
			       ptrdiff_type_node, t,
			       fold_convert_loc (OMP_CLAUSE_LOCATION (c),
						 ptrdiff_type_node, ptr));
	  OMP_CLAUSE_SIZE (c2) = t;
	  OMP_CLAUSE_CHAIN (c2) = OMP_CLAUSE_CHAIN (c);
	  OMP_CLAUSE_CHAIN (c) = c2;
	  ptr = OMP_CLAUSE_DECL (c2);
	  if (TREE_CODE (TREE_TYPE (ptr)) == REFERENCE_TYPE
	      && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (ptr))))
	    {
	      tree c3 = build_omp_clause (OMP_CLAUSE_LOCATION (c),
					  OMP_CLAUSE_MAP);
	      OMP_CLAUSE_MAP_KIND (c3) = OMP_CLAUSE_MAP_POINTER;
	      OMP_CLAUSE_DECL (c3) = ptr;
	      OMP_CLAUSE_DECL (c2) = convert_from_reference (ptr);
	      OMP_CLAUSE_SIZE (c3) = size_zero_node;
	      OMP_CLAUSE_CHAIN (c3) = OMP_CLAUSE_CHAIN (c2);
	      OMP_CLAUSE_CHAIN (c2) = c3;
	    }
	}
    }
  return false;
}

/* Return identifier to look up for omp declare reduction.  */

tree
omp_reduction_id (enum tree_code reduction_code, tree reduction_id, tree type)
{
  const char *p = NULL;
  const char *m = NULL;
  switch (reduction_code)
    {
    case PLUS_EXPR:
    case MULT_EXPR:
    case MINUS_EXPR:
    case BIT_AND_EXPR:
    case BIT_XOR_EXPR:
    case BIT_IOR_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      reduction_id = ansi_opname (reduction_code);
      break;
    case MIN_EXPR:
      p = "min";
      break;
    case MAX_EXPR:
      p = "max";
      break;
    default:
      break;
    }

  if (p == NULL)
    {
      if (TREE_CODE (reduction_id) != IDENTIFIER_NODE)
	return error_mark_node;
      p = IDENTIFIER_POINTER (reduction_id);
    }

  if (type != NULL_TREE)
    m = mangle_type_string (TYPE_MAIN_VARIANT (type));

  const char prefix[] = "omp declare reduction ";
  size_t lenp = sizeof (prefix);
  if (strncmp (p, prefix, lenp - 1) == 0)
    lenp = 1;
  size_t len = strlen (p);
  size_t lenm = m ? strlen (m) + 1 : 0;
  char *name = XALLOCAVEC (char, lenp + len + lenm);
  if (lenp > 1)
    memcpy (name, prefix, lenp - 1);
  memcpy (name + lenp - 1, p, len + 1);
  if (m)
    {
      name[lenp + len - 1] = '~';
      memcpy (name + lenp + len, m, lenm);
    }
  return get_identifier (name);
}

/* Lookup OpenMP UDR ID for TYPE, return the corresponding artificial
   FUNCTION_DECL or NULL_TREE if not found.  */

static tree
omp_reduction_lookup (location_t loc, tree id, tree type, tree *baselinkp,
		      vec<tree> *ambiguousp)
{
  tree orig_id = id;
  tree baselink = NULL_TREE;
  if (identifier_p (id))
    {
      cp_id_kind idk;
      bool nonint_cst_expression_p;
      const char *error_msg;
      id = omp_reduction_id (ERROR_MARK, id, type);
      tree decl = lookup_name (id);
      if (decl == NULL_TREE)
	decl = error_mark_node;
      id = finish_id_expression (id, decl, NULL_TREE, &idk, false, true,
				 &nonint_cst_expression_p, false, true, false,
				 false, &error_msg, loc);
      if (idk == CP_ID_KIND_UNQUALIFIED
	  && identifier_p (id))
	{
	  vec<tree, va_gc> *args = NULL;
	  vec_safe_push (args, build_reference_type (type));
	  id = perform_koenig_lookup (id, args, false, tf_none);
	}
    }
  else if (TREE_CODE (id) == SCOPE_REF)
    id = lookup_qualified_name (TREE_OPERAND (id, 0),
				omp_reduction_id (ERROR_MARK,
						  TREE_OPERAND (id, 1),
						  type),
				false, false);
  tree fns = id;
  if (id && is_overloaded_fn (id))
    id = get_fns (id);
  for (; id; id = OVL_NEXT (id))
    {
      tree fndecl = OVL_CURRENT (id);
      if (TREE_CODE (fndecl) == FUNCTION_DECL)
	{
	  tree argtype = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
	  if (same_type_p (TREE_TYPE (argtype), type))
	    break;
	}
    }
  if (id && BASELINK_P (fns))
    {
      if (baselinkp)
	*baselinkp = fns;
      else
	baselink = fns;
    }
  if (id == NULL_TREE && CLASS_TYPE_P (type) && TYPE_BINFO (type))
    {
      vec<tree> ambiguous = vNULL;
      tree binfo = TYPE_BINFO (type), base_binfo, ret = NULL_TREE;
      unsigned int ix;
      if (ambiguousp == NULL)
	ambiguousp = &ambiguous;
      for (ix = 0; BINFO_BASE_ITERATE (binfo, ix, base_binfo); ix++)
	{
	  id = omp_reduction_lookup (loc, orig_id, BINFO_TYPE (base_binfo),
				     baselinkp ? baselinkp : &baselink,
				     ambiguousp);
	  if (id == NULL_TREE)
	    continue;
	  if (!ambiguousp->is_empty ())
	    ambiguousp->safe_push (id);
	  else if (ret != NULL_TREE)
	    {
	      ambiguousp->safe_push (ret);
	      ambiguousp->safe_push (id);
	      ret = NULL_TREE;
	    }
	  else
	    ret = id;
	}
      if (ambiguousp != &ambiguous)
	return ret;
      if (!ambiguous.is_empty ())
	{
	  const char *str = _("candidates are:");
	  unsigned int idx;
	  tree udr;
	  error_at (loc, "user defined reduction lookup is ambiguous");
	  FOR_EACH_VEC_ELT (ambiguous, idx, udr)
	    {
	      inform (DECL_SOURCE_LOCATION (udr), "%s %#D", str, udr);
	      if (idx == 0)
		str = get_spaces (str);
	    }
	  ambiguous.release ();
	  ret = error_mark_node;
	  baselink = NULL_TREE;
	}
      id = ret;
    }
  if (id && baselink)
    perform_or_defer_access_check (BASELINK_BINFO (baselink),
				   id, id, tf_warning_or_error);
  return id;
}

/* Helper function for cp_parser_omp_declare_reduction_exprs
   and tsubst_omp_udr.
   Remove CLEANUP_STMT for data (omp_priv variable).
   Also append INIT_EXPR for DECL_INITIAL of omp_priv after its
   DECL_EXPR.  */

tree
cp_remove_omp_priv_cleanup_stmt (tree *tp, int *walk_subtrees, void *data)
{
  if (TYPE_P (*tp))
    *walk_subtrees = 0;
  else if (TREE_CODE (*tp) == CLEANUP_STMT && CLEANUP_DECL (*tp) == (tree) data)
    *tp = CLEANUP_BODY (*tp);
  else if (TREE_CODE (*tp) == DECL_EXPR)
    {
      tree decl = DECL_EXPR_DECL (*tp);
      if (!processing_template_decl
	  && decl == (tree) data
	  && DECL_INITIAL (decl)
	  && DECL_INITIAL (decl) != error_mark_node)
	{
	  tree list = NULL_TREE;
	  append_to_statement_list_force (*tp, &list);
	  tree init_expr = build2 (INIT_EXPR, void_type_node,
				   decl, DECL_INITIAL (decl));
	  DECL_INITIAL (decl) = NULL_TREE;
	  append_to_statement_list_force (init_expr, &list);
	  *tp = list;
	}
    }
  return NULL_TREE;
}

/* Data passed from cp_check_omp_declare_reduction to
   cp_check_omp_declare_reduction_r.  */

struct cp_check_omp_declare_reduction_data
{
  location_t loc;
  tree stmts[7];
  bool combiner_p;
};

/* Helper function for cp_check_omp_declare_reduction, called via
   cp_walk_tree.  */

static tree
cp_check_omp_declare_reduction_r (tree *tp, int *, void *data)
{
  struct cp_check_omp_declare_reduction_data *udr_data
    = (struct cp_check_omp_declare_reduction_data *) data;
  if (SSA_VAR_P (*tp)
      && !DECL_ARTIFICIAL (*tp)
      && *tp != DECL_EXPR_DECL (udr_data->stmts[udr_data->combiner_p ? 0 : 3])
      && *tp != DECL_EXPR_DECL (udr_data->stmts[udr_data->combiner_p ? 1 : 4]))
    {
      location_t loc = udr_data->loc;
      if (udr_data->combiner_p)
	error_at (loc, "%<#pragma omp declare reduction%> combiner refers to "
		       "variable %qD which is not %<omp_out%> nor %<omp_in%>",
		  *tp);
      else
	error_at (loc, "%<#pragma omp declare reduction%> initializer refers "
		       "to variable %qD which is not %<omp_priv%> nor "
		       "%<omp_orig%>",
		  *tp);
      return *tp;
    }
  return NULL_TREE;
}

/* Diagnose violation of OpenMP #pragma omp declare reduction restrictions.  */

void
cp_check_omp_declare_reduction (tree udr)
{
  tree type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (udr)));
  gcc_assert (TREE_CODE (type) == REFERENCE_TYPE);
  type = TREE_TYPE (type);
  int i;
  location_t loc = DECL_SOURCE_LOCATION (udr);

  if (type == error_mark_node)
    return;
  if (ARITHMETIC_TYPE_P (type))
    {
      static enum tree_code predef_codes[]
	= { PLUS_EXPR, MULT_EXPR, MINUS_EXPR, BIT_AND_EXPR, BIT_XOR_EXPR,
	    BIT_IOR_EXPR, TRUTH_ANDIF_EXPR, TRUTH_ORIF_EXPR };
      for (i = 0; i < 8; i++)
	{
	  tree id = omp_reduction_id (predef_codes[i], NULL_TREE, NULL_TREE);
	  const char *n1 = IDENTIFIER_POINTER (DECL_NAME (udr));
	  const char *n2 = IDENTIFIER_POINTER (id);
	  if (strncmp (n1, n2, IDENTIFIER_LENGTH (id)) == 0
	      && (n1[IDENTIFIER_LENGTH (id)] == '~'
		  || n1[IDENTIFIER_LENGTH (id)] == '\0'))
	    break;
	}

      if (i == 8
	  && TREE_CODE (type) != COMPLEX_EXPR)
	{
	  const char prefix_minmax[] = "omp declare reduction m";
	  size_t prefix_size = sizeof (prefix_minmax) - 1;
	  const char *n = IDENTIFIER_POINTER (DECL_NAME (udr));
	  if (strncmp (IDENTIFIER_POINTER (DECL_NAME (udr)),
		       prefix_minmax, prefix_size) == 0
	      && ((n[prefix_size] == 'i' && n[prefix_size + 1] == 'n')
		  || (n[prefix_size] == 'a' && n[prefix_size + 1] == 'x'))
	      && (n[prefix_size + 2] == '~' || n[prefix_size + 2] == '\0'))
	    i = 0;
	}
      if (i < 8)
	{
	  error_at (loc, "predeclared arithmetic type %qT in "
			 "%<#pragma omp declare reduction%>", type);
	  return;
	}
    }
  else if (TREE_CODE (type) == FUNCTION_TYPE
	   || TREE_CODE (type) == METHOD_TYPE
	   || TREE_CODE (type) == ARRAY_TYPE)
    {
      error_at (loc, "function or array type %qT in "
		     "%<#pragma omp declare reduction%>", type);
      return;
    }
  else if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      error_at (loc, "reference type %qT in %<#pragma omp declare reduction%>",
		type);
      return;
    }
  else if (TYPE_QUALS_NO_ADDR_SPACE (type))
    {
      error_at (loc, "const, volatile or __restrict qualified type %qT in "
		     "%<#pragma omp declare reduction%>", type);
      return;
    }

  tree body = DECL_SAVED_TREE (udr);
  if (body == NULL_TREE || TREE_CODE (body) != STATEMENT_LIST)
    return;

  tree_stmt_iterator tsi;
  struct cp_check_omp_declare_reduction_data data;
  memset (data.stmts, 0, sizeof data.stmts);
  for (i = 0, tsi = tsi_start (body);
       i < 7 && !tsi_end_p (tsi);
       i++, tsi_next (&tsi))
    data.stmts[i] = tsi_stmt (tsi);
  data.loc = loc;
  gcc_assert (tsi_end_p (tsi));
  if (i >= 3)
    {
      gcc_assert (TREE_CODE (data.stmts[0]) == DECL_EXPR
		  && TREE_CODE (data.stmts[1]) == DECL_EXPR);
      if (TREE_NO_WARNING (DECL_EXPR_DECL (data.stmts[0])))
	return;
      data.combiner_p = true;
      if (cp_walk_tree (&data.stmts[2], cp_check_omp_declare_reduction_r,
			&data, NULL))
	TREE_NO_WARNING (DECL_EXPR_DECL (data.stmts[0])) = 1;
    }
  if (i >= 6)
    {
      gcc_assert (TREE_CODE (data.stmts[3]) == DECL_EXPR
		  && TREE_CODE (data.stmts[4]) == DECL_EXPR);
      data.combiner_p = false;
      if (cp_walk_tree (&data.stmts[5], cp_check_omp_declare_reduction_r,
			&data, NULL)
	  || cp_walk_tree (&DECL_INITIAL (DECL_EXPR_DECL (data.stmts[3])),
			   cp_check_omp_declare_reduction_r, &data, NULL))
	TREE_NO_WARNING (DECL_EXPR_DECL (data.stmts[0])) = 1;
      if (i == 7)
	gcc_assert (TREE_CODE (data.stmts[6]) == DECL_EXPR);
    }
}

/* Helper function of finish_omp_clauses.  Clone STMT as if we were making
   an inline call.  But, remap
   the OMP_DECL1 VAR_DECL (omp_out resp. omp_orig) to PLACEHOLDER
   and OMP_DECL2 VAR_DECL (omp_in resp. omp_priv) to DECL.  */

static tree
clone_omp_udr (tree stmt, tree omp_decl1, tree omp_decl2,
	       tree decl, tree placeholder)
{
  copy_body_data id;
  struct pointer_map_t *decl_map = pointer_map_create ();

  *pointer_map_insert (decl_map, omp_decl1) = placeholder;
  *pointer_map_insert (decl_map, omp_decl2) = decl;
  memset (&id, 0, sizeof (id));
  id.src_fn = DECL_CONTEXT (omp_decl1);
  id.dst_fn = current_function_decl;
  id.src_cfun = DECL_STRUCT_FUNCTION (id.src_fn);
  id.decl_map = decl_map;

  id.copy_decl = copy_decl_no_change;
  id.transform_call_graph_edges = CB_CGE_DUPLICATE;
  id.transform_new_cfg = true;
  id.transform_return_to_modify = false;
  id.transform_lang_insert_block = NULL;
  id.eh_lp_nr = 0;
  walk_tree (&stmt, copy_tree_body_r, &id, NULL);
  pointer_map_destroy (decl_map);
  return stmt;
}

/* Helper function of finish_omp_clauses, called via cp_walk_tree.
   Find OMP_CLAUSE_PLACEHOLDER (passed in DATA) in *TP.  */

static tree
find_omp_placeholder_r (tree *tp, int *, void *data)
{
  if (*tp == (tree) data)
    return *tp;
  return NULL_TREE;
}

/* Helper function of finish_omp_clauses.  Handle OMP_CLAUSE_REDUCTION C.
   Return true if there is some error and the clause should be removed.  */

static bool
finish_omp_reduction_clause (tree c, bool *need_default_ctor, bool *need_dtor)
{
  tree t = OMP_CLAUSE_DECL (c);
  bool predefined = false;
  tree type = TREE_TYPE (t);
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);
  if (ARITHMETIC_TYPE_P (type))
    switch (OMP_CLAUSE_REDUCTION_CODE (c))
      {
      case PLUS_EXPR:
      case MULT_EXPR:
      case MINUS_EXPR:
	predefined = true;
	break;
      case MIN_EXPR:
      case MAX_EXPR:
	if (TREE_CODE (type) == COMPLEX_TYPE)
	  break;
	predefined = true;
	break;
      case BIT_AND_EXPR:
      case BIT_IOR_EXPR:
      case BIT_XOR_EXPR:
      case TRUTH_ANDIF_EXPR:
      case TRUTH_ORIF_EXPR:
	if (FLOAT_TYPE_P (type))
	  break;
	predefined = true;
	break;
      default:
	break;
      }
  else if (TREE_CODE (type) == ARRAY_TYPE || TYPE_READONLY (type))
    {
      error ("%qE has invalid type for %<reduction%>", t);
      return true;
    }
  else if (!processing_template_decl)
    {
      t = require_complete_type (t);
      if (t == error_mark_node)
	return true;
      OMP_CLAUSE_DECL (c) = t;
    }

  if (predefined)
    {
      OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = NULL_TREE;
      return false;
    }
  else if (processing_template_decl)
    return false;

  tree id = OMP_CLAUSE_REDUCTION_PLACEHOLDER (c);

  type = TYPE_MAIN_VARIANT (TREE_TYPE (t));
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);
  OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = NULL_TREE;
  if (id == NULL_TREE)
    id = omp_reduction_id (OMP_CLAUSE_REDUCTION_CODE (c),
			   NULL_TREE, NULL_TREE);
  id = omp_reduction_lookup (OMP_CLAUSE_LOCATION (c), id, type, NULL, NULL);
  if (id)
    {
      if (id == error_mark_node)
	return true;
      id = OVL_CURRENT (id);
      mark_used (id);
      tree body = DECL_SAVED_TREE (id);
      if (TREE_CODE (body) == STATEMENT_LIST)
	{
	  tree_stmt_iterator tsi;
	  tree placeholder = NULL_TREE;
	  int i;
	  tree stmts[7];
	  tree atype = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (id)));
	  atype = TREE_TYPE (atype);
	  bool need_static_cast = !same_type_p (type, atype);
	  memset (stmts, 0, sizeof stmts);
	  for (i = 0, tsi = tsi_start (body);
	       i < 7 && !tsi_end_p (tsi);
	       i++, tsi_next (&tsi))
	    stmts[i] = tsi_stmt (tsi);
	  gcc_assert (tsi_end_p (tsi));

	  if (i >= 3)
	    {
	      gcc_assert (TREE_CODE (stmts[0]) == DECL_EXPR
			  && TREE_CODE (stmts[1]) == DECL_EXPR);
	      placeholder = build_lang_decl (VAR_DECL, NULL_TREE, type);
	      DECL_ARTIFICIAL (placeholder) = 1;
	      DECL_IGNORED_P (placeholder) = 1;
	      OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = placeholder;
	      if (TREE_ADDRESSABLE (DECL_EXPR_DECL (stmts[0])))
		cxx_mark_addressable (placeholder);
	      if (TREE_ADDRESSABLE (DECL_EXPR_DECL (stmts[1]))
		  && TREE_CODE (TREE_TYPE (OMP_CLAUSE_DECL (c)))
		     != REFERENCE_TYPE)
		cxx_mark_addressable (OMP_CLAUSE_DECL (c));
	      tree omp_out = placeholder;
	      tree omp_in = convert_from_reference (OMP_CLAUSE_DECL (c));
	      if (need_static_cast)
		{
		  tree rtype = build_reference_type (atype);
		  omp_out = build_static_cast (rtype, omp_out,
					       tf_warning_or_error);
		  omp_in = build_static_cast (rtype, omp_in,
					      tf_warning_or_error);
		  if (omp_out == error_mark_node || omp_in == error_mark_node)
		    return true;
		  omp_out = convert_from_reference (omp_out);
		  omp_in = convert_from_reference (omp_in);
		}
	      OMP_CLAUSE_REDUCTION_MERGE (c)
		= clone_omp_udr (stmts[2], DECL_EXPR_DECL (stmts[0]),
				 DECL_EXPR_DECL (stmts[1]), omp_in, omp_out);
	    }
	  if (i >= 6)
	    {
	      gcc_assert (TREE_CODE (stmts[3]) == DECL_EXPR
			  && TREE_CODE (stmts[4]) == DECL_EXPR);
	      if (TREE_ADDRESSABLE (DECL_EXPR_DECL (stmts[3])))
		cxx_mark_addressable (OMP_CLAUSE_DECL (c));
	      if (TREE_ADDRESSABLE (DECL_EXPR_DECL (stmts[4])))
		cxx_mark_addressable (placeholder);
	      tree omp_priv = convert_from_reference (OMP_CLAUSE_DECL (c));
	      tree omp_orig = placeholder;
	      if (need_static_cast)
		{
		  if (i == 7)
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"user defined reduction with constructor "
				"initializer for base class %qT", atype);
		      return true;
		    }
		  tree rtype = build_reference_type (atype);
		  omp_priv = build_static_cast (rtype, omp_priv,
						tf_warning_or_error);
		  omp_orig = build_static_cast (rtype, omp_orig,
						tf_warning_or_error);
		  if (omp_priv == error_mark_node
		      || omp_orig == error_mark_node)
		    return true;
		  omp_priv = convert_from_reference (omp_priv);
		  omp_orig = convert_from_reference (omp_orig);
		}
	      if (i == 6)
		*need_default_ctor = true;
	      OMP_CLAUSE_REDUCTION_INIT (c)
		= clone_omp_udr (stmts[5], DECL_EXPR_DECL (stmts[4]),
				 DECL_EXPR_DECL (stmts[3]),
				 omp_priv, omp_orig);
	      if (cp_walk_tree (&OMP_CLAUSE_REDUCTION_INIT (c),
				find_omp_placeholder_r, placeholder, NULL))
		OMP_CLAUSE_REDUCTION_OMP_ORIG_REF (c) = 1;
	    }
	  else if (i >= 3)
	    {
	      if (CLASS_TYPE_P (type) && !pod_type_p (type))
		*need_default_ctor = true;
	      else
		{
		  tree init;
		  tree v = convert_from_reference (t);
		  if (AGGREGATE_TYPE_P (TREE_TYPE (v)))
		    init = build_constructor (TREE_TYPE (v), NULL);
		  else
		    init = fold_convert (TREE_TYPE (v), integer_zero_node);
		  OMP_CLAUSE_REDUCTION_INIT (c)
		    = build2 (INIT_EXPR, TREE_TYPE (v), v, init);
		}
	    }
	}
    }
  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
    *need_dtor = true;
  else
    {
      error ("user defined reduction not found for %qD", t);
      return true;
    }
  return false;
}

/* For all elements of CLAUSES, validate them vs OpenMP constraints.
   Remove any elements from the list that are invalid.  */

tree
finish_omp_clauses (tree clauses)
{
  bitmap_head generic_head, firstprivate_head, lastprivate_head;
  bitmap_head aligned_head;
  tree c, t, *pc = &clauses;
  bool branch_seen = false;
  bool copyprivate_seen = false;

  bitmap_obstack_initialize (NULL);
  bitmap_initialize (&generic_head, &bitmap_default_obstack);
  bitmap_initialize (&firstprivate_head, &bitmap_default_obstack);
  bitmap_initialize (&lastprivate_head, &bitmap_default_obstack);
  bitmap_initialize (&aligned_head, &bitmap_default_obstack);

  for (pc = &clauses, c = clauses; c ; c = *pc)
    {
      bool remove = false;

      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_SHARED:
	  goto check_dup_generic;
	case OMP_CLAUSE_PRIVATE:
	  goto check_dup_generic;
	case OMP_CLAUSE_REDUCTION:
	  goto check_dup_generic;
	case OMP_CLAUSE_COPYPRIVATE:
	  copyprivate_seen = true;
	  goto check_dup_generic;
	case OMP_CLAUSE_COPYIN:
	  goto check_dup_generic;
	case OMP_CLAUSE_LINEAR:
	  t = OMP_CLAUSE_DECL (c);
	  if (!type_dependent_expression_p (t)
	      && !INTEGRAL_TYPE_P (TREE_TYPE (t))
	      && TREE_CODE (TREE_TYPE (t)) != POINTER_TYPE)
	    {
	      error ("linear clause applied to non-integral non-pointer "
		     "variable with %qT type", TREE_TYPE (t));
	      remove = true;
	      break;
	    }
	  t = OMP_CLAUSE_LINEAR_STEP (c);
	  if (t == NULL_TREE)
	    t = integer_one_node;
	  if (t == error_mark_node)
	    {
	      remove = true;
	      break;
	    }
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error ("linear step expression must be integral");
	      remove = true;
	      break;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  if (TREE_CODE (OMP_CLAUSE_DECL (c)) == PARM_DECL)
		    t = maybe_constant_value (t);
		  t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
		  if (TREE_CODE (TREE_TYPE (OMP_CLAUSE_DECL (c)))
		      == POINTER_TYPE)
		    {
		      t = pointer_int_sum (OMP_CLAUSE_LOCATION (c), PLUS_EXPR,
					   OMP_CLAUSE_DECL (c), t);
		      t = fold_build2_loc (OMP_CLAUSE_LOCATION (c),
					   MINUS_EXPR, sizetype, t,
					   OMP_CLAUSE_DECL (c));
		      if (t == error_mark_node)
			{
			  remove = true;
			  break;
			}
		    }
		}
	      OMP_CLAUSE_LINEAR_STEP (c) = t;
	    }
	  goto check_dup_generic;
	check_dup_generic:
	  t = OMP_CLAUSE_DECL (c);
	  if (!VAR_P (t) && TREE_CODE (t) != PARM_DECL)
	    {
	      if (processing_template_decl)
		break;
	      if (DECL_P (t))
		error ("%qD is not a variable in clause %qs", t,
		       omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      else
		error ("%qE is not a variable in clause %qs", t,
		       omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  else if (bitmap_bit_p (&generic_head, DECL_UID (t))
		   || bitmap_bit_p (&firstprivate_head, DECL_UID (t))
		   || bitmap_bit_p (&lastprivate_head, DECL_UID (t)))
	    {
	      error ("%qD appears more than once in data clauses", t);
	      remove = true;
	    }
	  else
	    bitmap_set_bit (&generic_head, DECL_UID (t));
	  break;

	case OMP_CLAUSE_FIRSTPRIVATE:
	  t = OMP_CLAUSE_DECL (c);
	  if (!VAR_P (t) && TREE_CODE (t) != PARM_DECL)
	    {
	      if (processing_template_decl)
		break;
	      if (DECL_P (t))
		error ("%qD is not a variable in clause %<firstprivate%>", t);
	      else
		error ("%qE is not a variable in clause %<firstprivate%>", t);
	      remove = true;
	    }
	  else if (bitmap_bit_p (&generic_head, DECL_UID (t))
		   || bitmap_bit_p (&firstprivate_head, DECL_UID (t)))
	    {
	      error ("%qD appears more than once in data clauses", t);
	      remove = true;
	    }
	  else
	    bitmap_set_bit (&firstprivate_head, DECL_UID (t));
	  break;

	case OMP_CLAUSE_LASTPRIVATE:
	  t = OMP_CLAUSE_DECL (c);
	  if (!VAR_P (t) && TREE_CODE (t) != PARM_DECL)
	    {
	      if (processing_template_decl)
		break;
	      if (DECL_P (t))
		error ("%qD is not a variable in clause %<lastprivate%>", t);
	      else
		error ("%qE is not a variable in clause %<lastprivate%>", t);
	      remove = true;
	    }
	  else if (bitmap_bit_p (&generic_head, DECL_UID (t))
		   || bitmap_bit_p (&lastprivate_head, DECL_UID (t)))
	    {
	      error ("%qD appears more than once in data clauses", t);
	      remove = true;
	    }
	  else
	    bitmap_set_bit (&lastprivate_head, DECL_UID (t));
	  break;

	case OMP_CLAUSE_IF:
	  t = OMP_CLAUSE_IF_EXPR (c);
	  t = maybe_convert_cond (t);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!processing_template_decl)
	    t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	  OMP_CLAUSE_IF_EXPR (c) = t;
	  break;

	case OMP_CLAUSE_FINAL:
	  t = OMP_CLAUSE_FINAL_EXPR (c);
	  t = maybe_convert_cond (t);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!processing_template_decl)
	    t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	  OMP_CLAUSE_FINAL_EXPR (c) = t;
	  break;

	case OMP_CLAUSE_NUM_THREADS:
	  t = OMP_CLAUSE_NUM_THREADS_EXPR (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error ("num_threads expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	      OMP_CLAUSE_NUM_THREADS_EXPR (c) = t;
	    }
	  break;

	case OMP_CLAUSE_SCHEDULE:
	  t = OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (c);
	  if (t == NULL)
	    ;
	  else if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error ("schedule chunk size expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	      OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (c) = t;
	    }
	  break;

	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_SAFELEN:
	  t = OMP_CLAUSE_OPERAND (c, 0);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error ("%qs length expression must be integral",
		     omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      t = maybe_constant_value (t);
	      if (!processing_template_decl)
		{
		  if (TREE_CODE (t) != INTEGER_CST
		      || tree_int_cst_sgn (t) != 1)
		    {
		      error ("%qs length expression must be positive constant"
			     " integer expression",
			     omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		      remove = true;
		    }
		}
	      OMP_CLAUSE_OPERAND (c, 0) = t;
	    }
	  break;

	case OMP_CLAUSE_NUM_TEAMS:
	  t = OMP_CLAUSE_NUM_TEAMS_EXPR (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error ("%<num_teams%> expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	      OMP_CLAUSE_NUM_TEAMS_EXPR (c) = t;
	    }
	  break;

	case OMP_CLAUSE_THREAD_LIMIT:
	  t = OMP_CLAUSE_THREAD_LIMIT_EXPR (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error ("%<thread_limit%> expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	      OMP_CLAUSE_THREAD_LIMIT_EXPR (c) = t;
	    }
	  break;

	case OMP_CLAUSE_DEVICE:
	  t = OMP_CLAUSE_DEVICE_ID (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error ("%<device%> id must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	      OMP_CLAUSE_DEVICE_ID (c) = t;
	    }
	  break;

	case OMP_CLAUSE_DIST_SCHEDULE:
	  t = OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (c);
	  if (t == NULL)
	    ;
	  else if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error ("%<dist_schedule%> chunk size expression must be "
		     "integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	      OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (c) = t;
	    }
	  break;

	case OMP_CLAUSE_ALIGNED:
	  t = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (t) != VAR_DECL && TREE_CODE (t) != PARM_DECL)
	    {
	      if (processing_template_decl)
		break;
	      if (DECL_P (t))
		error ("%qD is not a variable in %<aligned%> clause", t);
	      else
		error ("%qE is not a variable in %<aligned%> clause", t);
	      remove = true;
	    }
	  else if (!type_dependent_expression_p (t)
		   && TREE_CODE (TREE_TYPE (t)) != POINTER_TYPE
		   && TREE_CODE (TREE_TYPE (t)) != ARRAY_TYPE
		   && (TREE_CODE (TREE_TYPE (t)) != REFERENCE_TYPE
		       || (!POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (t)))
			   && (TREE_CODE (TREE_TYPE (TREE_TYPE (t)))
			       != ARRAY_TYPE))))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qE in %<aligned%> clause is neither a pointer nor "
			"an array nor a reference to pointer or array", t);
	      remove = true;
	    }
	  else if (bitmap_bit_p (&aligned_head, DECL_UID (t)))
	    {
	      error ("%qD appears more than once in %<aligned%> clauses", t);
	      remove = true;
	    }
	  else
	    bitmap_set_bit (&aligned_head, DECL_UID (t));
	  t = OMP_CLAUSE_ALIGNED_ALIGNMENT (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (t == NULL_TREE)
	    break;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error ("%<aligned%> clause alignment expression must "
		     "be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      t = maybe_constant_value (t);
	      if (!processing_template_decl)
		{
		  if (TREE_CODE (t) != INTEGER_CST
		      || tree_int_cst_sgn (t) != 1)
		    {
		      error ("%<aligned%> clause alignment expression must be "
			     "positive constant integer expression");
		      remove = true;
		    }
		}
	      OMP_CLAUSE_ALIGNED_ALIGNMENT (c) = t;
	    }
	  break;

	case OMP_CLAUSE_DEPEND:
	  t = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (t) == TREE_LIST)
	    {
	      if (handle_omp_array_sections (c))
		remove = true;
	      break;
	    }
	  if (t == error_mark_node)
	    remove = true;
	  else if (TREE_CODE (t) != VAR_DECL && TREE_CODE (t) != PARM_DECL)
	    {
	      if (processing_template_decl)
		break;
	      if (DECL_P (t))
		error ("%qD is not a variable in %<depend%> clause", t);
	      else
		error ("%qE is not a variable in %<depend%> clause", t);
	      remove = true;
	    }
	  else if (!processing_template_decl
		   && !cxx_mark_addressable (t))
	    remove = true;
	  break;

	case OMP_CLAUSE_MAP:
	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_FROM:
	  t = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (t) == TREE_LIST)
	    {
	      if (handle_omp_array_sections (c))
		remove = true;
	      else
		{
		  t = OMP_CLAUSE_DECL (c);
		  if (!cp_omp_mappable_type (TREE_TYPE (t)))
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"array section does not have mappable type "
				"in %qs clause",
				omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		      remove = true;
		    }
		}
	      break;
	    }
	  if (t == error_mark_node)
	    remove = true;
	  else if (TREE_CODE (t) != VAR_DECL && TREE_CODE (t) != PARM_DECL)
	    {
	      if (processing_template_decl)
		break;
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		  && OMP_CLAUSE_MAP_KIND (c) == OMP_CLAUSE_MAP_POINTER)
		break;
	      if (DECL_P (t))
		error ("%qD is not a variable in %qs clause", t,
		       omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      else
		error ("%qE is not a variable in %qs clause", t,
		       omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  else if (TREE_CODE (t) == VAR_DECL && DECL_THREAD_LOCAL_P (t))
	    {
	      error ("%qD is threadprivate variable in %qs clause", t,
		     omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  else if (!processing_template_decl
		   && TREE_CODE (TREE_TYPE (t)) != REFERENCE_TYPE
		   && !cxx_mark_addressable (t))
	    remove = true;
	  else if (!(OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		     && OMP_CLAUSE_MAP_KIND (c) == OMP_CLAUSE_MAP_POINTER)
		   && !cp_omp_mappable_type ((TREE_CODE (TREE_TYPE (t))
					      == REFERENCE_TYPE)
					     ? TREE_TYPE (TREE_TYPE (t))
					     : TREE_TYPE (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qD does not have a mappable type in %qs clause", t,
			omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  else if (bitmap_bit_p (&generic_head, DECL_UID (t)))
	    {
	      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP)
		error ("%qD appears more than once in motion clauses", t);
	      else
		error ("%qD appears more than once in map clauses", t);
	      remove = true;
	    }
	  else
	    bitmap_set_bit (&generic_head, DECL_UID (t));
	  break;

	case OMP_CLAUSE_UNIFORM:
	  t = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (t) != PARM_DECL)
	    {
	      if (processing_template_decl)
		break;
	      if (DECL_P (t))
		error ("%qD is not an argument in %<uniform%> clause", t);
	      else
		error ("%qE is not an argument in %<uniform%> clause", t);
	      remove = true;
	      break;
	    }
	  goto check_dup_generic;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_PARALLEL:
	case OMP_CLAUSE_FOR:
	case OMP_CLAUSE_SECTIONS:
	case OMP_CLAUSE_TASKGROUP:
	case OMP_CLAUSE_PROC_BIND:
	  break;

	case OMP_CLAUSE_INBRANCH:
	case OMP_CLAUSE_NOTINBRANCH:
	  if (branch_seen)
	    {
	      error ("%<inbranch%> clause is incompatible with "
		     "%<notinbranch%>");
	      remove = true;
	    }
	  branch_seen = true;
	  break;

	default:
	  gcc_unreachable ();
	}

      if (remove)
	*pc = OMP_CLAUSE_CHAIN (c);
      else
	pc = &OMP_CLAUSE_CHAIN (c);
    }

  for (pc = &clauses, c = clauses; c ; c = *pc)
    {
      enum omp_clause_code c_kind = OMP_CLAUSE_CODE (c);
      bool remove = false;
      bool need_complete_non_reference = false;
      bool need_default_ctor = false;
      bool need_copy_ctor = false;
      bool need_copy_assignment = false;
      bool need_implicitly_determined = false;
      bool need_dtor = false;
      tree type, inner_type;

      switch (c_kind)
	{
	case OMP_CLAUSE_SHARED:
	  need_implicitly_determined = true;
	  break;
	case OMP_CLAUSE_PRIVATE:
	  need_complete_non_reference = true;
	  need_default_ctor = true;
	  need_dtor = true;
	  need_implicitly_determined = true;
	  break;
	case OMP_CLAUSE_FIRSTPRIVATE:
	  need_complete_non_reference = true;
	  need_copy_ctor = true;
	  need_dtor = true;
	  need_implicitly_determined = true;
	  break;
	case OMP_CLAUSE_LASTPRIVATE:
	  need_complete_non_reference = true;
	  need_copy_assignment = true;
	  need_implicitly_determined = true;
	  break;
	case OMP_CLAUSE_REDUCTION:
	  need_implicitly_determined = true;
	  break;
	case OMP_CLAUSE_COPYPRIVATE:
	  need_copy_assignment = true;
	  break;
	case OMP_CLAUSE_COPYIN:
	  need_copy_assignment = true;
	  break;
	case OMP_CLAUSE_NOWAIT:
	  if (copyprivate_seen)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<nowait%> clause must not be used together "
			"with %<copyprivate%>");
	      *pc = OMP_CLAUSE_CHAIN (c);
	      continue;
	    }
	  /* FALLTHRU */
	default:
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	}

      t = OMP_CLAUSE_DECL (c);
      if (processing_template_decl
	  && !VAR_P (t) && TREE_CODE (t) != PARM_DECL)
	{
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	}

      switch (c_kind)
	{
	case OMP_CLAUSE_LASTPRIVATE:
	  if (!bitmap_bit_p (&firstprivate_head, DECL_UID (t)))
	    {
	      need_default_ctor = true;
	      need_dtor = true;
	    }
	  break;

	case OMP_CLAUSE_REDUCTION:
	  if (finish_omp_reduction_clause (c, &need_default_ctor,
					   &need_dtor))
	    remove = true;
	  else
	    t = OMP_CLAUSE_DECL (c);
	  break;

	case OMP_CLAUSE_COPYIN:
	  if (!VAR_P (t) || !DECL_THREAD_LOCAL_P (t))
	    {
	      error ("%qE must be %<threadprivate%> for %<copyin%>", t);
	      remove = true;
	    }
	  break;

	default:
	  break;
	}

      if (need_complete_non_reference || need_copy_assignment)
	{
	  t = require_complete_type (t);
	  if (t == error_mark_node)
	    remove = true;
	  else if (TREE_CODE (TREE_TYPE (t)) == REFERENCE_TYPE
		   && need_complete_non_reference)
	    {
	      error ("%qE has reference type for %qs", t,
		     omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	}
      if (need_implicitly_determined)
	{
	  const char *share_name = NULL;

	  if (VAR_P (t) && DECL_THREAD_LOCAL_P (t))
	    share_name = "threadprivate";
	  else switch (cxx_omp_predetermined_sharing (t))
	    {
	    case OMP_CLAUSE_DEFAULT_UNSPECIFIED:
	      break;
	    case OMP_CLAUSE_DEFAULT_SHARED:
	      /* const vars may be specified in firstprivate clause.  */
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
		  && cxx_omp_const_qual_no_mutable (t))
		break;
	      share_name = "shared";
	      break;
	    case OMP_CLAUSE_DEFAULT_PRIVATE:
	      share_name = "private";
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  if (share_name)
	    {
	      error ("%qE is predetermined %qs for %qs",
		     t, share_name, omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	}

      /* We're interested in the base element, not arrays.  */
      inner_type = type = TREE_TYPE (t);
      while (TREE_CODE (inner_type) == ARRAY_TYPE)
	inner_type = TREE_TYPE (inner_type);

      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	  && TREE_CODE (inner_type) == REFERENCE_TYPE)
	inner_type = TREE_TYPE (inner_type);

      /* Check for special function availability by building a call to one.
	 Save the results, because later we won't be in the right context
	 for making these queries.  */
      if (CLASS_TYPE_P (inner_type)
	  && COMPLETE_TYPE_P (inner_type)
	  && (need_default_ctor || need_copy_ctor
	      || need_copy_assignment || need_dtor)
	  && !type_dependent_expression_p (t)
	  && cxx_omp_create_clause_info (c, inner_type, need_default_ctor,
					 need_copy_ctor, need_copy_assignment,
					 need_dtor))
	remove = true;

      if (remove)
	*pc = OMP_CLAUSE_CHAIN (c);
      else
	pc = &OMP_CLAUSE_CHAIN (c);
    }

  bitmap_obstack_release (NULL);
  return clauses;
}

/* For all variables in the tree_list VARS, mark them as thread local.  */

void
finish_omp_threadprivate (tree vars)
{
  tree t;

  /* Mark every variable in VARS to be assigned thread local storage.  */
  for (t = vars; t; t = TREE_CHAIN (t))
    {
      tree v = TREE_PURPOSE (t);

      if (error_operand_p (v))
	;
      else if (!VAR_P (v))
	error ("%<threadprivate%> %qD is not file, namespace "
	       "or block scope variable", v);
      /* If V had already been marked threadprivate, it doesn't matter
	 whether it had been used prior to this point.  */
      else if (TREE_USED (v)
	  && (DECL_LANG_SPECIFIC (v) == NULL
	      || !CP_DECL_THREADPRIVATE_P (v)))
	error ("%qE declared %<threadprivate%> after first use", v);
      else if (! TREE_STATIC (v) && ! DECL_EXTERNAL (v))
	error ("automatic variable %qE cannot be %<threadprivate%>", v);
      else if (! COMPLETE_TYPE_P (complete_type (TREE_TYPE (v))))
	error ("%<threadprivate%> %qE has incomplete type", v);
      else if (TREE_STATIC (v) && TYPE_P (CP_DECL_CONTEXT (v))
	       && CP_DECL_CONTEXT (v) != current_class_type)
	error ("%<threadprivate%> %qE directive not "
	       "in %qT definition", v, CP_DECL_CONTEXT (v));
      else
	{
	  /* Allocate a LANG_SPECIFIC structure for V, if needed.  */
	  if (DECL_LANG_SPECIFIC (v) == NULL)
	    {
	      retrofit_lang_decl (v);

	      /* Make sure that DECL_DISCRIMINATOR_P continues to be true
		 after the allocation of the lang_decl structure.  */
	      if (DECL_DISCRIMINATOR_P (v))
		DECL_LANG_SPECIFIC (v)->u.base.u2sel = 1;
	    }

	  if (! DECL_THREAD_LOCAL_P (v))
	    {
	      DECL_TLS_MODEL (v) = decl_default_tls_model (v);
	      /* If rtl has been already set for this var, call
		 make_decl_rtl once again, so that encode_section_info
		 has a chance to look at the new decl flags.  */
	      if (DECL_RTL_SET_P (v))
		make_decl_rtl (v);
	    }
	  CP_DECL_THREADPRIVATE_P (v) = 1;
	}
    }
}

/* Build an OpenMP structured block.  */

tree
begin_omp_structured_block (void)
{
  return do_pushlevel (sk_omp);
}

tree
finish_omp_structured_block (tree block)
{
  return do_poplevel (block);
}

/* Similarly, except force the retention of the BLOCK.  */

tree
begin_omp_parallel (void)
{
  keep_next_level (true);
  return begin_omp_structured_block ();
}

tree
finish_omp_parallel (tree clauses, tree body)
{
  tree stmt;

  body = finish_omp_structured_block (body);

  stmt = make_node (OMP_PARALLEL);
  TREE_TYPE (stmt) = void_type_node;
  OMP_PARALLEL_CLAUSES (stmt) = clauses;
  OMP_PARALLEL_BODY (stmt) = body;

  return add_stmt (stmt);
}

tree
begin_omp_task (void)
{
  keep_next_level (true);
  return begin_omp_structured_block ();
}

tree
finish_omp_task (tree clauses, tree body)
{
  tree stmt;

  body = finish_omp_structured_block (body);

  stmt = make_node (OMP_TASK);
  TREE_TYPE (stmt) = void_type_node;
  OMP_TASK_CLAUSES (stmt) = clauses;
  OMP_TASK_BODY (stmt) = body;

  return add_stmt (stmt);
}

/* Helper function for finish_omp_for.  Convert Ith random access iterator
   into integral iterator.  Return FALSE if successful.  */

static bool
handle_omp_for_class_iterator (int i, location_t locus, tree declv, tree initv,
			       tree condv, tree incrv, tree *body,
			       tree *pre_body, tree clauses)
{
  tree diff, iter_init, iter_incr = NULL, last;
  tree incr_var = NULL, orig_pre_body, orig_body, c;
  tree decl = TREE_VEC_ELT (declv, i);
  tree init = TREE_VEC_ELT (initv, i);
  tree cond = TREE_VEC_ELT (condv, i);
  tree incr = TREE_VEC_ELT (incrv, i);
  tree iter = decl;
  location_t elocus = locus;

  if (init && EXPR_HAS_LOCATION (init))
    elocus = EXPR_LOCATION (init);

  switch (TREE_CODE (cond))
    {
    case GT_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
      if (TREE_OPERAND (cond, 1) == iter)
	cond = build2 (swap_tree_comparison (TREE_CODE (cond)),
		       TREE_TYPE (cond), iter, TREE_OPERAND (cond, 0));
      if (TREE_OPERAND (cond, 0) != iter)
	cond = error_mark_node;
      else
	{
	  tree tem = build_x_binary_op (EXPR_LOCATION (cond),
					TREE_CODE (cond),
					iter, ERROR_MARK,
					TREE_OPERAND (cond, 1), ERROR_MARK,
					NULL, tf_warning_or_error);
	  if (error_operand_p (tem))
	    return true;
	}
      break;
    default:
      cond = error_mark_node;
      break;
    }
  if (cond == error_mark_node)
    {
      error_at (elocus, "invalid controlling predicate");
      return true;
    }
  diff = build_x_binary_op (elocus, MINUS_EXPR, TREE_OPERAND (cond, 1),
			    ERROR_MARK, iter, ERROR_MARK, NULL,
			    tf_warning_or_error);
  if (error_operand_p (diff))
    return true;
  if (TREE_CODE (TREE_TYPE (diff)) != INTEGER_TYPE)
    {
      error_at (elocus, "difference between %qE and %qD does not have integer type",
		TREE_OPERAND (cond, 1), iter);
      return true;
    }

  switch (TREE_CODE (incr))
    {
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      if (TREE_OPERAND (incr, 0) != iter)
	{
	  incr = error_mark_node;
	  break;
	}
      iter_incr = build_x_unary_op (EXPR_LOCATION (incr),
				    TREE_CODE (incr), iter,
				    tf_warning_or_error);
      if (error_operand_p (iter_incr))
	return true;
      else if (TREE_CODE (incr) == PREINCREMENT_EXPR
	       || TREE_CODE (incr) == POSTINCREMENT_EXPR)
	incr = integer_one_node;
      else
	incr = integer_minus_one_node;
      break;
    case MODIFY_EXPR:
      if (TREE_OPERAND (incr, 0) != iter)
	incr = error_mark_node;
      else if (TREE_CODE (TREE_OPERAND (incr, 1)) == PLUS_EXPR
	       || TREE_CODE (TREE_OPERAND (incr, 1)) == MINUS_EXPR)
	{
	  tree rhs = TREE_OPERAND (incr, 1);
	  if (TREE_OPERAND (rhs, 0) == iter)
	    {
	      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (rhs, 1)))
		  != INTEGER_TYPE)
		incr = error_mark_node;
	      else
		{
		  iter_incr = build_x_modify_expr (EXPR_LOCATION (rhs),
						   iter, TREE_CODE (rhs),
						   TREE_OPERAND (rhs, 1),
						   tf_warning_or_error);
		  if (error_operand_p (iter_incr))
		    return true;
		  incr = TREE_OPERAND (rhs, 1);
		  incr = cp_convert (TREE_TYPE (diff), incr,
				     tf_warning_or_error);
		  if (TREE_CODE (rhs) == MINUS_EXPR)
		    {
		      incr = build1 (NEGATE_EXPR, TREE_TYPE (diff), incr);
		      incr = fold_if_not_in_template (incr);
		    }
		  if (TREE_CODE (incr) != INTEGER_CST
		      && (TREE_CODE (incr) != NOP_EXPR
			  || (TREE_CODE (TREE_OPERAND (incr, 0))
			      != INTEGER_CST)))
		    iter_incr = NULL;
		}
	    }
	  else if (TREE_OPERAND (rhs, 1) == iter)
	    {
	      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (rhs, 0))) != INTEGER_TYPE
		  || TREE_CODE (rhs) != PLUS_EXPR)
		incr = error_mark_node;
	      else
		{
		  iter_incr = build_x_binary_op (EXPR_LOCATION (rhs),
						 PLUS_EXPR,
						 TREE_OPERAND (rhs, 0),
						 ERROR_MARK, iter,
						 ERROR_MARK, NULL,
						 tf_warning_or_error);
		  if (error_operand_p (iter_incr))
		    return true;
		  iter_incr = build_x_modify_expr (EXPR_LOCATION (rhs),
						   iter, NOP_EXPR,
						   iter_incr,
						   tf_warning_or_error);
		  if (error_operand_p (iter_incr))
		    return true;
		  incr = TREE_OPERAND (rhs, 0);
		  iter_incr = NULL;
		}
	    }
	  else
	    incr = error_mark_node;
	}
      else
	incr = error_mark_node;
      break;
    default:
      incr = error_mark_node;
      break;
    }

  if (incr == error_mark_node)
    {
      error_at (elocus, "invalid increment expression");
      return true;
    }

  incr = cp_convert (TREE_TYPE (diff), incr, tf_warning_or_error);
  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	&& OMP_CLAUSE_DECL (c) == iter)
      break;

  decl = create_temporary_var (TREE_TYPE (diff));
  pushdecl (decl);
  add_decl_expr (decl);
  last = create_temporary_var (TREE_TYPE (diff));
  pushdecl (last);
  add_decl_expr (last);
  if (c && iter_incr == NULL)
    {
      incr_var = create_temporary_var (TREE_TYPE (diff));
      pushdecl (incr_var);
      add_decl_expr (incr_var);
    }
  gcc_assert (stmts_are_full_exprs_p ());

  orig_pre_body = *pre_body;
  *pre_body = push_stmt_list ();
  if (orig_pre_body)
    add_stmt (orig_pre_body);
  if (init != NULL)
    finish_expr_stmt (build_x_modify_expr (elocus,
					   iter, NOP_EXPR, init,
					   tf_warning_or_error));
  init = build_int_cst (TREE_TYPE (diff), 0);
  if (c && iter_incr == NULL)
    {
      finish_expr_stmt (build_x_modify_expr (elocus,
					     incr_var, NOP_EXPR,
					     incr, tf_warning_or_error));
      incr = incr_var;
      iter_incr = build_x_modify_expr (elocus,
				       iter, PLUS_EXPR, incr,
				       tf_warning_or_error);
    }
  finish_expr_stmt (build_x_modify_expr (elocus,
					 last, NOP_EXPR, init,
					 tf_warning_or_error));
  *pre_body = pop_stmt_list (*pre_body);

  cond = cp_build_binary_op (elocus,
			     TREE_CODE (cond), decl, diff,
			     tf_warning_or_error);
  incr = build_modify_expr (elocus, decl, NULL_TREE, PLUS_EXPR,
			    elocus, incr, NULL_TREE);

  orig_body = *body;
  *body = push_stmt_list ();
  iter_init = build2 (MINUS_EXPR, TREE_TYPE (diff), decl, last);
  iter_init = build_x_modify_expr (elocus,
				   iter, PLUS_EXPR, iter_init,
				   tf_warning_or_error);
  iter_init = build1 (NOP_EXPR, void_type_node, iter_init);
  finish_expr_stmt (iter_init);
  finish_expr_stmt (build_x_modify_expr (elocus,
					 last, NOP_EXPR, decl,
					 tf_warning_or_error));
  add_stmt (orig_body);
  *body = pop_stmt_list (*body);

  if (c)
    {
      OMP_CLAUSE_LASTPRIVATE_STMT (c) = push_stmt_list ();
      finish_expr_stmt (iter_incr);
      OMP_CLAUSE_LASTPRIVATE_STMT (c)
	= pop_stmt_list (OMP_CLAUSE_LASTPRIVATE_STMT (c));
    }

  TREE_VEC_ELT (declv, i) = decl;
  TREE_VEC_ELT (initv, i) = init;
  TREE_VEC_ELT (condv, i) = cond;
  TREE_VEC_ELT (incrv, i) = incr;

  return false;
}

/* Build and validate an OMP_FOR statement.  CLAUSES, BODY, COND, INCR
   are directly for their associated operands in the statement.  DECL
   and INIT are a combo; if DECL is NULL then INIT ought to be a
   MODIFY_EXPR, and the DECL should be extracted.  PRE_BODY are
   optional statements that need to go before the loop into its
   sk_omp scope.  */

tree
finish_omp_for (location_t locus, enum tree_code code, tree declv, tree initv,
		tree condv, tree incrv, tree body, tree pre_body, tree clauses)
{
  tree omp_for = NULL, orig_incr = NULL;
  tree decl, init, cond, incr;
  location_t elocus;
  int i;

  gcc_assert (TREE_VEC_LENGTH (declv) == TREE_VEC_LENGTH (initv));
  gcc_assert (TREE_VEC_LENGTH (declv) == TREE_VEC_LENGTH (condv));
  gcc_assert (TREE_VEC_LENGTH (declv) == TREE_VEC_LENGTH (incrv));
  for (i = 0; i < TREE_VEC_LENGTH (declv); i++)
    {
      decl = TREE_VEC_ELT (declv, i);
      init = TREE_VEC_ELT (initv, i);
      cond = TREE_VEC_ELT (condv, i);
      incr = TREE_VEC_ELT (incrv, i);
      elocus = locus;

      if (decl == NULL)
	{
	  if (init != NULL)
	    switch (TREE_CODE (init))
	      {
	      case MODIFY_EXPR:
		decl = TREE_OPERAND (init, 0);
		init = TREE_OPERAND (init, 1);
		break;
	      case MODOP_EXPR:
		if (TREE_CODE (TREE_OPERAND (init, 1)) == NOP_EXPR)
		  {
		    decl = TREE_OPERAND (init, 0);
		    init = TREE_OPERAND (init, 2);
		  }
		break;
	      default:
		break;
	      }

	  if (decl == NULL)
	    {
	      error_at (locus,
			"expected iteration declaration or initialization");
	      return NULL;
	    }
	}

      if (init && EXPR_HAS_LOCATION (init))
	elocus = EXPR_LOCATION (init);

      if (cond == NULL)
	{
	  error_at (elocus, "missing controlling predicate");
	  return NULL;
	}

      if (incr == NULL)
	{
	  error_at (elocus, "missing increment expression");
	  return NULL;
	}

      TREE_VEC_ELT (declv, i) = decl;
      TREE_VEC_ELT (initv, i) = init;
    }

  if (dependent_omp_for_p (declv, initv, condv, incrv))
    {
      tree stmt;

      stmt = make_node (code);

      for (i = 0; i < TREE_VEC_LENGTH (declv); i++)
	{
	  /* This is really just a place-holder.  We'll be decomposing this
	     again and going through the cp_build_modify_expr path below when
	     we instantiate the thing.  */
	  TREE_VEC_ELT (initv, i)
	    = build2 (MODIFY_EXPR, void_type_node, TREE_VEC_ELT (declv, i),
		      TREE_VEC_ELT (initv, i));
	}

      TREE_TYPE (stmt) = void_type_node;
      OMP_FOR_INIT (stmt) = initv;
      OMP_FOR_COND (stmt) = condv;
      OMP_FOR_INCR (stmt) = incrv;
      OMP_FOR_BODY (stmt) = body;
      OMP_FOR_PRE_BODY (stmt) = pre_body;
      OMP_FOR_CLAUSES (stmt) = clauses;

      SET_EXPR_LOCATION (stmt, locus);
      return add_stmt (stmt);
    }

  if (processing_template_decl)
    orig_incr = make_tree_vec (TREE_VEC_LENGTH (incrv));

  for (i = 0; i < TREE_VEC_LENGTH (declv); )
    {
      decl = TREE_VEC_ELT (declv, i);
      init = TREE_VEC_ELT (initv, i);
      cond = TREE_VEC_ELT (condv, i);
      incr = TREE_VEC_ELT (incrv, i);
      if (orig_incr)
	TREE_VEC_ELT (orig_incr, i) = incr;
      elocus = locus;

      if (init && EXPR_HAS_LOCATION (init))
	elocus = EXPR_LOCATION (init);

      if (!DECL_P (decl))
	{
	  error_at (elocus, "expected iteration declaration or initialization");
	  return NULL;
	}

      if (incr && TREE_CODE (incr) == MODOP_EXPR)
	{
	  if (orig_incr)
	    TREE_VEC_ELT (orig_incr, i) = incr;
	  incr = cp_build_modify_expr (TREE_OPERAND (incr, 0),
				       TREE_CODE (TREE_OPERAND (incr, 1)),
				       TREE_OPERAND (incr, 2),
				       tf_warning_or_error);
	}

      if (CLASS_TYPE_P (TREE_TYPE (decl)))
	{
	  if (code == OMP_SIMD)
	    {
	      error_at (elocus, "%<#pragma omp simd%> used with class "
				"iteration variable %qE", decl);
	      return NULL;
	    }
	  if (handle_omp_for_class_iterator (i, locus, declv, initv, condv,
					     incrv, &body, &pre_body, clauses))
	    return NULL;
	  continue;
	}

      if (!INTEGRAL_TYPE_P (TREE_TYPE (decl))
	  && !TYPE_PTR_P (TREE_TYPE (decl)))
	{
	  error_at (elocus, "invalid type for iteration variable %qE", decl);
	  return NULL;
	}

      if (!processing_template_decl)
	{
	  init = fold_build_cleanup_point_expr (TREE_TYPE (init), init);
	  init = cp_build_modify_expr (decl, NOP_EXPR, init, tf_warning_or_error);
	}
      else
	init = build2 (MODIFY_EXPR, void_type_node, decl, init);
      if (cond
	  && TREE_SIDE_EFFECTS (cond)
	  && COMPARISON_CLASS_P (cond)
	  && !processing_template_decl)
	{
	  tree t = TREE_OPERAND (cond, 0);
	  if (TREE_SIDE_EFFECTS (t)
	      && t != decl
	      && (TREE_CODE (t) != NOP_EXPR
		  || TREE_OPERAND (t, 0) != decl))
	    TREE_OPERAND (cond, 0)
	      = fold_build_cleanup_point_expr (TREE_TYPE (t), t);

	  t = TREE_OPERAND (cond, 1);
	  if (TREE_SIDE_EFFECTS (t)
	      && t != decl
	      && (TREE_CODE (t) != NOP_EXPR
		  || TREE_OPERAND (t, 0) != decl))
	    TREE_OPERAND (cond, 1)
	      = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	}
      if (decl == error_mark_node || init == error_mark_node)
	return NULL;

      TREE_VEC_ELT (declv, i) = decl;
      TREE_VEC_ELT (initv, i) = init;
      TREE_VEC_ELT (condv, i) = cond;
      TREE_VEC_ELT (incrv, i) = incr;
      i++;
    }

  if (IS_EMPTY_STMT (pre_body))
    pre_body = NULL;

  omp_for = c_finish_omp_for (locus, code, declv, initv, condv, incrv,
			      body, pre_body);

  if (omp_for == NULL)
    return NULL;

  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INCR (omp_for)); i++)
    {
      decl = TREE_OPERAND (TREE_VEC_ELT (OMP_FOR_INIT (omp_for), i), 0);
      incr = TREE_VEC_ELT (OMP_FOR_INCR (omp_for), i);

      if (TREE_CODE (incr) != MODIFY_EXPR)
	continue;

      if (TREE_SIDE_EFFECTS (TREE_OPERAND (incr, 1))
	  && BINARY_CLASS_P (TREE_OPERAND (incr, 1))
	  && !processing_template_decl)
	{
	  tree t = TREE_OPERAND (TREE_OPERAND (incr, 1), 0);
	  if (TREE_SIDE_EFFECTS (t)
	      && t != decl
	      && (TREE_CODE (t) != NOP_EXPR
		  || TREE_OPERAND (t, 0) != decl))
	    TREE_OPERAND (TREE_OPERAND (incr, 1), 0)
	      = fold_build_cleanup_point_expr (TREE_TYPE (t), t);

	  t = TREE_OPERAND (TREE_OPERAND (incr, 1), 1);
	  if (TREE_SIDE_EFFECTS (t)
	      && t != decl
	      && (TREE_CODE (t) != NOP_EXPR
		  || TREE_OPERAND (t, 0) != decl))
	    TREE_OPERAND (TREE_OPERAND (incr, 1), 1)
	      = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	}

      if (orig_incr)
	TREE_VEC_ELT (OMP_FOR_INCR (omp_for), i) = TREE_VEC_ELT (orig_incr, i);
    }
  if (omp_for != NULL)
    OMP_FOR_CLAUSES (omp_for) = clauses;
  return omp_for;
}

void
finish_omp_atomic (enum tree_code code, enum tree_code opcode, tree lhs,
		   tree rhs, tree v, tree lhs1, tree rhs1, bool seq_cst)
{
  tree orig_lhs;
  tree orig_rhs;
  tree orig_v;
  tree orig_lhs1;
  tree orig_rhs1;
  bool dependent_p;
  tree stmt;

  orig_lhs = lhs;
  orig_rhs = rhs;
  orig_v = v;
  orig_lhs1 = lhs1;
  orig_rhs1 = rhs1;
  dependent_p = false;
  stmt = NULL_TREE;

  /* Even in a template, we can detect invalid uses of the atomic
     pragma if neither LHS nor RHS is type-dependent.  */
  if (processing_template_decl)
    {
      dependent_p = (type_dependent_expression_p (lhs)
		     || (rhs && type_dependent_expression_p (rhs))
		     || (v && type_dependent_expression_p (v))
		     || (lhs1 && type_dependent_expression_p (lhs1))
		     || (rhs1 && type_dependent_expression_p (rhs1)));
      if (!dependent_p)
	{
	  lhs = build_non_dependent_expr (lhs);
	  if (rhs)
	    rhs = build_non_dependent_expr (rhs);
	  if (v)
	    v = build_non_dependent_expr (v);
	  if (lhs1)
	    lhs1 = build_non_dependent_expr (lhs1);
	  if (rhs1)
	    rhs1 = build_non_dependent_expr (rhs1);
	}
    }
  if (!dependent_p)
    {
      bool swapped = false;
      if (rhs1 && cp_tree_equal (lhs, rhs))
	{
	  tree tem = rhs;
	  rhs = rhs1;
	  rhs1 = tem;
	  swapped = !commutative_tree_code (opcode);
	}
      if (rhs1 && !cp_tree_equal (lhs, rhs1))
	{
	  if (code == OMP_ATOMIC)
	    error ("%<#pragma omp atomic update%> uses two different "
		   "expressions for memory");
	  else
	    error ("%<#pragma omp atomic capture%> uses two different "
		   "expressions for memory");
	  return;
	}
      if (lhs1 && !cp_tree_equal (lhs, lhs1))
	{
	  if (code == OMP_ATOMIC)
	    error ("%<#pragma omp atomic update%> uses two different "
		   "expressions for memory");
	  else
	    error ("%<#pragma omp atomic capture%> uses two different "
		   "expressions for memory");
	  return;
	}
      stmt = c_finish_omp_atomic (input_location, code, opcode, lhs, rhs,
				  v, lhs1, rhs1, swapped, seq_cst);
      if (stmt == error_mark_node)
	return;
    }
  if (processing_template_decl)
    {
      if (code == OMP_ATOMIC_READ)
	{
	  stmt = build_min_nt_loc (EXPR_LOCATION (orig_lhs),
				   OMP_ATOMIC_READ, orig_lhs);
	  OMP_ATOMIC_SEQ_CST (stmt) = seq_cst;
	  stmt = build2 (MODIFY_EXPR, void_type_node, orig_v, stmt);
	}
      else
	{
	  if (opcode == NOP_EXPR)
	    stmt = build2 (MODIFY_EXPR, void_type_node, orig_lhs, orig_rhs);
	  else 
	    stmt = build2 (opcode, void_type_node, orig_lhs, orig_rhs);
	  if (orig_rhs1)
	    stmt = build_min_nt_loc (EXPR_LOCATION (orig_rhs1),
				     COMPOUND_EXPR, orig_rhs1, stmt);
	  if (code != OMP_ATOMIC)
	    {
	      stmt = build_min_nt_loc (EXPR_LOCATION (orig_lhs1),
				       code, orig_lhs1, stmt);
	      OMP_ATOMIC_SEQ_CST (stmt) = seq_cst;
	      stmt = build2 (MODIFY_EXPR, void_type_node, orig_v, stmt);
	    }
	}
      stmt = build2 (OMP_ATOMIC, void_type_node, integer_zero_node, stmt);
      OMP_ATOMIC_SEQ_CST (stmt) = seq_cst;
    }
  finish_expr_stmt (stmt);
}

void
finish_omp_barrier (void)
{
  tree fn = builtin_decl_explicit (BUILT_IN_GOMP_BARRIER);
  vec<tree, va_gc> *vec = make_tree_vector ();
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  release_tree_vector (vec);
  finish_expr_stmt (stmt);
}

void
finish_omp_flush (void)
{
  tree fn = builtin_decl_explicit (BUILT_IN_SYNC_SYNCHRONIZE);
  vec<tree, va_gc> *vec = make_tree_vector ();
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  release_tree_vector (vec);
  finish_expr_stmt (stmt);
}

void
finish_omp_taskwait (void)
{
  tree fn = builtin_decl_explicit (BUILT_IN_GOMP_TASKWAIT);
  vec<tree, va_gc> *vec = make_tree_vector ();
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  release_tree_vector (vec);
  finish_expr_stmt (stmt);
}

void
finish_omp_taskyield (void)
{
  tree fn = builtin_decl_explicit (BUILT_IN_GOMP_TASKYIELD);
  vec<tree, va_gc> *vec = make_tree_vector ();
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  release_tree_vector (vec);
  finish_expr_stmt (stmt);
}

void
finish_omp_cancel (tree clauses)
{
  tree fn = builtin_decl_explicit (BUILT_IN_GOMP_CANCEL);
  int mask = 0;
  if (find_omp_clause (clauses, OMP_CLAUSE_PARALLEL))
    mask = 1;
  else if (find_omp_clause (clauses, OMP_CLAUSE_FOR))
    mask = 2;
  else if (find_omp_clause (clauses, OMP_CLAUSE_SECTIONS))
    mask = 4;
  else if (find_omp_clause (clauses, OMP_CLAUSE_TASKGROUP))
    mask = 8;
  else
    {
      error ("%<#pragma omp cancel must specify one of "
	     "%<parallel%>, %<for%>, %<sections%> or %<taskgroup%> clauses");
      return;
    }
  vec<tree, va_gc> *vec = make_tree_vector ();
  tree ifc = find_omp_clause (clauses, OMP_CLAUSE_IF);
  if (ifc != NULL_TREE)
    {
      tree type = TREE_TYPE (OMP_CLAUSE_IF_EXPR (ifc));
      ifc = fold_build2_loc (OMP_CLAUSE_LOCATION (ifc), NE_EXPR,
			     boolean_type_node, OMP_CLAUSE_IF_EXPR (ifc),
			     build_zero_cst (type));
    }
  else
    ifc = boolean_true_node;
  vec->quick_push (build_int_cst (integer_type_node, mask));
  vec->quick_push (ifc);
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  release_tree_vector (vec);
  finish_expr_stmt (stmt);
}

void
finish_omp_cancellation_point (tree clauses)
{
  tree fn = builtin_decl_explicit (BUILT_IN_GOMP_CANCELLATION_POINT);
  int mask = 0;
  if (find_omp_clause (clauses, OMP_CLAUSE_PARALLEL))
    mask = 1;
  else if (find_omp_clause (clauses, OMP_CLAUSE_FOR))
    mask = 2;
  else if (find_omp_clause (clauses, OMP_CLAUSE_SECTIONS))
    mask = 4;
  else if (find_omp_clause (clauses, OMP_CLAUSE_TASKGROUP))
    mask = 8;
  else
    {
      error ("%<#pragma omp cancellation point must specify one of "
	     "%<parallel%>, %<for%>, %<sections%> or %<taskgroup%> clauses");
      return;
    }
  vec<tree, va_gc> *vec
    = make_tree_vector_single (build_int_cst (integer_type_node, mask));
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  release_tree_vector (vec);
  finish_expr_stmt (stmt);
}

/* Begin a __transaction_atomic or __transaction_relaxed statement.
   If PCOMPOUND is non-null, this is for a function-transaction-block, and we
   should create an extra compound stmt.  */

tree
begin_transaction_stmt (location_t loc, tree *pcompound, int flags)
{
  tree r;

  if (pcompound)
    *pcompound = begin_compound_stmt (0);

  r = build_stmt (loc, TRANSACTION_EXPR, NULL_TREE);

  /* Only add the statement to the function if support enabled.  */
  if (flag_tm)
    add_stmt (r);
  else
    error_at (loc, ((flags & TM_STMT_ATTR_RELAXED) != 0
		    ? G_("%<__transaction_relaxed%> without "
			 "transactional memory support enabled")
		    : G_("%<__transaction_atomic%> without "
			 "transactional memory support enabled")));

  TRANSACTION_EXPR_BODY (r) = push_stmt_list ();
  TREE_SIDE_EFFECTS (r) = 1;
  return r;
}

/* End a __transaction_atomic or __transaction_relaxed statement.
   If COMPOUND_STMT is non-null, this is for a function-transaction-block,
   and we should end the compound.  If NOEX is non-NULL, we wrap the body in
   a MUST_NOT_THROW_EXPR with NOEX as condition.  */

void
finish_transaction_stmt (tree stmt, tree compound_stmt, int flags, tree noex)
{
  TRANSACTION_EXPR_BODY (stmt) = pop_stmt_list (TRANSACTION_EXPR_BODY (stmt));
  TRANSACTION_EXPR_OUTER (stmt) = (flags & TM_STMT_ATTR_OUTER) != 0;
  TRANSACTION_EXPR_RELAXED (stmt) = (flags & TM_STMT_ATTR_RELAXED) != 0;
  TRANSACTION_EXPR_IS_STMT (stmt) = 1;

  /* noexcept specifications are not allowed for function transactions.  */
  gcc_assert (!(noex && compound_stmt));
  if (noex)
    {
      tree body = build_must_not_throw_expr (TRANSACTION_EXPR_BODY (stmt),
					     noex);
      /* This may not be true when the STATEMENT_LIST is empty.  */
      if (EXPR_P (body))
        SET_EXPR_LOCATION (body, EXPR_LOCATION (TRANSACTION_EXPR_BODY (stmt)));
      TREE_SIDE_EFFECTS (body) = 1;
      TRANSACTION_EXPR_BODY (stmt) = body;
    }

  if (compound_stmt)
    finish_compound_stmt (compound_stmt);
}

/* Build a __transaction_atomic or __transaction_relaxed expression.  If
   NOEX is non-NULL, we wrap the body in a MUST_NOT_THROW_EXPR with NOEX as
   condition.  */

tree
build_transaction_expr (location_t loc, tree expr, int flags, tree noex)
{
  tree ret;
  if (noex)
    {
      expr = build_must_not_throw_expr (expr, noex);
      if (EXPR_P (expr))
	SET_EXPR_LOCATION (expr, loc);
      TREE_SIDE_EFFECTS (expr) = 1;
    }
  ret = build1 (TRANSACTION_EXPR, TREE_TYPE (expr), expr);
  if (flags & TM_STMT_ATTR_RELAXED)
	TRANSACTION_EXPR_RELAXED (ret) = 1;
  TREE_SIDE_EFFECTS (ret) = 1;
  SET_EXPR_LOCATION (ret, loc);
  return ret;
}

void
init_cp_semantics (void)
{
}

/* Build a STATIC_ASSERT for a static assertion with the condition
   CONDITION and the message text MESSAGE.  LOCATION is the location
   of the static assertion in the source code.  When MEMBER_P, this
   static assertion is a member of a class.  */
void 
finish_static_assert (tree condition, tree message, location_t location, 
                      bool member_p)
{
  if (message == NULL_TREE
      || message == error_mark_node
      || condition == NULL_TREE
      || condition == error_mark_node)
    return;

  if (check_for_bare_parameter_packs (condition))
    condition = error_mark_node;

  if (type_dependent_expression_p (condition) 
      || value_dependent_expression_p (condition))
    {
      /* We're in a template; build a STATIC_ASSERT and put it in
         the right place. */
      tree assertion;

      assertion = make_node (STATIC_ASSERT);
      STATIC_ASSERT_CONDITION (assertion) = condition;
      STATIC_ASSERT_MESSAGE (assertion) = message;
      STATIC_ASSERT_SOURCE_LOCATION (assertion) = location;

      if (member_p)
        maybe_add_class_template_decl_list (current_class_type, 
                                            assertion,
                                            /*friend_p=*/0);
      else
        add_stmt (assertion);

      return;
    }

  /* Fold the expression and convert it to a boolean value. */
  condition = fold_non_dependent_expr (condition);
  condition = cp_convert (boolean_type_node, condition, tf_warning_or_error);
  condition = maybe_constant_value (condition);

  if (TREE_CODE (condition) == INTEGER_CST && !integer_zerop (condition))
    /* Do nothing; the condition is satisfied. */
    ;
  else 
    {
      location_t saved_loc = input_location;

      input_location = location;
      if (TREE_CODE (condition) == INTEGER_CST 
          && integer_zerop (condition))
        /* Report the error. */
        error ("static assertion failed: %s", TREE_STRING_POINTER (message));
      else if (condition && condition != error_mark_node)
	{
	  error ("non-constant condition for static assertion");
	  cxx_constant_value (condition);
	}
      input_location = saved_loc;
    }
}

/* Implements the C++0x decltype keyword. Returns the type of EXPR,
   suitable for use as a type-specifier.

   ID_EXPRESSION_OR_MEMBER_ACCESS_P is true when EXPR was parsed as an
   id-expression or a class member access, FALSE when it was parsed as
   a full expression.  */

tree
finish_decltype_type (tree expr, bool id_expression_or_member_access_p,
		      tsubst_flags_t complain)
{
  tree type = NULL_TREE;

  if (!expr || error_operand_p (expr))
    return error_mark_node;

  if (TYPE_P (expr)
      || TREE_CODE (expr) == TYPE_DECL
      || (TREE_CODE (expr) == BIT_NOT_EXPR
	  && TYPE_P (TREE_OPERAND (expr, 0))))
    {
      if (complain & tf_error)
	error ("argument to decltype must be an expression");
      return error_mark_node;
    }

  /* Depending on the resolution of DR 1172, we may later need to distinguish
     instantiation-dependent but not type-dependent expressions so that, say,
     A<decltype(sizeof(T))>::U doesn't require 'typename'.  */
  if (instantiation_dependent_expression_p (expr))
    {
      type = cxx_make_type (DECLTYPE_TYPE);
      DECLTYPE_TYPE_EXPR (type) = expr;
      DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (type)
        = id_expression_or_member_access_p;
      SET_TYPE_STRUCTURAL_EQUALITY (type);

      return type;
    }

  /* The type denoted by decltype(e) is defined as follows:  */

  expr = resolve_nondeduced_context (expr);

  if (invalid_nonstatic_memfn_p (expr, complain))
    return error_mark_node;

  if (type_unknown_p (expr))
    {
      if (complain & tf_error)
	error ("decltype cannot resolve address of overloaded function");
      return error_mark_node;
    }

  /* To get the size of a static data member declared as an array of
     unknown bound, we need to instantiate it.  */
  if (VAR_P (expr)
      && VAR_HAD_UNKNOWN_BOUND (expr)
      && DECL_TEMPLATE_INSTANTIATION (expr))
    instantiate_decl (expr, /*defer_ok*/true, /*expl_inst_mem*/false);

  if (id_expression_or_member_access_p)
    {
      /* If e is an id-expression or a class member access (5.2.5
         [expr.ref]), decltype(e) is defined as the type of the entity
         named by e. If there is no such entity, or e names a set of
         overloaded functions, the program is ill-formed.  */
      if (identifier_p (expr))
        expr = lookup_name (expr);

      if (INDIRECT_REF_P (expr))
        /* This can happen when the expression is, e.g., "a.b". Just
           look at the underlying operand.  */
        expr = TREE_OPERAND (expr, 0);

      if (TREE_CODE (expr) == OFFSET_REF
          || TREE_CODE (expr) == MEMBER_REF
	  || TREE_CODE (expr) == SCOPE_REF)
        /* We're only interested in the field itself. If it is a
           BASELINK, we will need to see through it in the next
           step.  */
        expr = TREE_OPERAND (expr, 1);

      if (BASELINK_P (expr))
        /* See through BASELINK nodes to the underlying function.  */
        expr = BASELINK_FUNCTIONS (expr);

      switch (TREE_CODE (expr))
        {
        case FIELD_DECL:
          if (DECL_BIT_FIELD_TYPE (expr))
            {
              type = DECL_BIT_FIELD_TYPE (expr);
              break;
            }
          /* Fall through for fields that aren't bitfields.  */

        case FUNCTION_DECL:
        case VAR_DECL:
        case CONST_DECL:
        case PARM_DECL:
        case RESULT_DECL:
        case TEMPLATE_PARM_INDEX:
	  expr = mark_type_use (expr);
          type = TREE_TYPE (expr);
          break;

        case ERROR_MARK:
          type = error_mark_node;
          break;

        case COMPONENT_REF:
	case COMPOUND_EXPR:
	  mark_type_use (expr);
          type = is_bitfield_expr_with_lowered_type (expr);
          if (!type)
            type = TREE_TYPE (TREE_OPERAND (expr, 1));
          break;

        case BIT_FIELD_REF:
          gcc_unreachable ();

        case INTEGER_CST:
	case PTRMEM_CST:
          /* We can get here when the id-expression refers to an
             enumerator or non-type template parameter.  */
          type = TREE_TYPE (expr);
          break;

        default:
	  /* Handle instantiated template non-type arguments.  */
	  type = TREE_TYPE (expr);
          break;
        }
    }
  else
    {
      /* Within a lambda-expression:

	 Every occurrence of decltype((x)) where x is a possibly
	 parenthesized id-expression that names an entity of
	 automatic storage duration is treated as if x were
	 transformed into an access to a corresponding data member
	 of the closure type that would have been declared if x
	 were a use of the denoted entity.  */
      if (outer_automatic_var_p (expr)
	  && current_function_decl
	  && LAMBDA_FUNCTION_P (current_function_decl))
	type = capture_decltype (expr);
      else if (error_operand_p (expr))
	type = error_mark_node;
      else if (expr == current_class_ptr)
	/* If the expression is just "this", we want the
	   cv-unqualified pointer for the "this" type.  */
	type = TYPE_MAIN_VARIANT (TREE_TYPE (expr));
      else
	{
	  /* Otherwise, where T is the type of e, if e is an lvalue,
	     decltype(e) is defined as T&; if an xvalue, T&&; otherwise, T. */
	  cp_lvalue_kind clk = lvalue_kind (expr);
	  type = unlowered_expr_type (expr);
	  gcc_assert (TREE_CODE (type) != REFERENCE_TYPE);

	  /* For vector types, pick a non-opaque variant.  */
	  if (TREE_CODE (type) == VECTOR_TYPE)
	    type = strip_typedefs (type);

	  if (clk != clk_none && !(clk & clk_class))
	    type = cp_build_reference_type (type, (clk & clk_rvalueref));
	}
    }

  if (cxx_dialect >= cxx1y && array_of_runtime_bound_p (type))
    {
      if (complain & tf_warning_or_error)
	pedwarn (input_location, OPT_Wvla,
		 "taking decltype of array of runtime bound");
      else
	return error_mark_node;
    }

  return type;
}

/* Called from trait_expr_value to evaluate either __has_nothrow_assign or 
   __has_nothrow_copy, depending on assign_p.  */

static bool
classtype_has_nothrow_assign_or_copy_p (tree type, bool assign_p)
{
  tree fns;

  if (assign_p)
    {
      int ix;
      ix = lookup_fnfields_1 (type, ansi_assopname (NOP_EXPR));
      if (ix < 0)
	return false;
      fns = (*CLASSTYPE_METHOD_VEC (type))[ix];
    } 
  else if (TYPE_HAS_COPY_CTOR (type))
    {
      /* If construction of the copy constructor was postponed, create
	 it now.  */
      if (CLASSTYPE_LAZY_COPY_CTOR (type))
	lazily_declare_fn (sfk_copy_constructor, type);
      if (CLASSTYPE_LAZY_MOVE_CTOR (type))
	lazily_declare_fn (sfk_move_constructor, type);
      fns = CLASSTYPE_CONSTRUCTORS (type);
    }
  else
    return false;

  for (; fns; fns = OVL_NEXT (fns))
    {
      tree fn = OVL_CURRENT (fns);
 
      if (assign_p)
	{
	  if (copy_fn_p (fn) == 0)
	    continue;
	}
      else if (copy_fn_p (fn) <= 0)
	continue;

      maybe_instantiate_noexcept (fn);
      if (!TYPE_NOTHROW_P (TREE_TYPE (fn)))
	return false;
    }

  return true;
}

/* Actually evaluates the trait.  */

static bool
trait_expr_value (cp_trait_kind kind, tree type1, tree type2)
{
  enum tree_code type_code1;
  tree t;

  type_code1 = TREE_CODE (type1);

  switch (kind)
    {
    case CPTK_HAS_NOTHROW_ASSIGN:
      type1 = strip_array_types (type1);
      return (!CP_TYPE_CONST_P (type1) && type_code1 != REFERENCE_TYPE
	      && (trait_expr_value (CPTK_HAS_TRIVIAL_ASSIGN, type1, type2)
		  || (CLASS_TYPE_P (type1)
		      && classtype_has_nothrow_assign_or_copy_p (type1,
								 true))));

    case CPTK_HAS_TRIVIAL_ASSIGN:
      /* ??? The standard seems to be missing the "or array of such a class
	 type" wording for this trait.  */
      type1 = strip_array_types (type1);
      return (!CP_TYPE_CONST_P (type1) && type_code1 != REFERENCE_TYPE
	      && (trivial_type_p (type1)
		    || (CLASS_TYPE_P (type1)
			&& TYPE_HAS_TRIVIAL_COPY_ASSIGN (type1))));

    case CPTK_HAS_NOTHROW_CONSTRUCTOR:
      type1 = strip_array_types (type1);
      return (trait_expr_value (CPTK_HAS_TRIVIAL_CONSTRUCTOR, type1, type2) 
	      || (CLASS_TYPE_P (type1)
		  && (t = locate_ctor (type1))
		  && (maybe_instantiate_noexcept (t),
		      TYPE_NOTHROW_P (TREE_TYPE (t)))));

    case CPTK_HAS_TRIVIAL_CONSTRUCTOR:
      type1 = strip_array_types (type1);
      return (trivial_type_p (type1)
	      || (CLASS_TYPE_P (type1) && TYPE_HAS_TRIVIAL_DFLT (type1)));

    case CPTK_HAS_NOTHROW_COPY:
      type1 = strip_array_types (type1);
      return (trait_expr_value (CPTK_HAS_TRIVIAL_COPY, type1, type2)
	      || (CLASS_TYPE_P (type1)
		  && classtype_has_nothrow_assign_or_copy_p (type1, false)));

    case CPTK_HAS_TRIVIAL_COPY:
      /* ??? The standard seems to be missing the "or array of such a class
	 type" wording for this trait.  */
      type1 = strip_array_types (type1);
      return (trivial_type_p (type1) || type_code1 == REFERENCE_TYPE
	      || (CLASS_TYPE_P (type1) && TYPE_HAS_TRIVIAL_COPY_CTOR (type1)));

    case CPTK_HAS_TRIVIAL_DESTRUCTOR:
      type1 = strip_array_types (type1);
      return (trivial_type_p (type1) || type_code1 == REFERENCE_TYPE
	      || (CLASS_TYPE_P (type1)
		  && TYPE_HAS_TRIVIAL_DESTRUCTOR (type1)));

    case CPTK_HAS_VIRTUAL_DESTRUCTOR:
      return type_has_virtual_destructor (type1);

    case CPTK_IS_ABSTRACT:
      return (ABSTRACT_CLASS_TYPE_P (type1));

    case CPTK_IS_BASE_OF:
      return (NON_UNION_CLASS_TYPE_P (type1) && NON_UNION_CLASS_TYPE_P (type2)
	      && DERIVED_FROM_P (type1, type2));

    case CPTK_IS_CLASS:
      return (NON_UNION_CLASS_TYPE_P (type1));

    case CPTK_IS_CONVERTIBLE_TO:
      /* TODO  */
      return false;

    case CPTK_IS_EMPTY:
      return (NON_UNION_CLASS_TYPE_P (type1) && CLASSTYPE_EMPTY_P (type1));

    case CPTK_IS_ENUM:
      return (type_code1 == ENUMERAL_TYPE);

    case CPTK_IS_FINAL:
      return (CLASS_TYPE_P (type1) && CLASSTYPE_FINAL (type1));

    case CPTK_IS_LITERAL_TYPE:
      return (literal_type_p (type1));

    case CPTK_IS_POD:
      return (pod_type_p (type1));

    case CPTK_IS_POLYMORPHIC:
      return (CLASS_TYPE_P (type1) && TYPE_POLYMORPHIC_P (type1));

    case CPTK_IS_STD_LAYOUT:
      return (std_layout_type_p (type1));

    case CPTK_IS_TRIVIAL:
      return (trivial_type_p (type1));

    case CPTK_IS_UNION:
      return (type_code1 == UNION_TYPE);

    default:
      gcc_unreachable ();
      return false;
    }
}

/* If TYPE is an array of unknown bound, or (possibly cv-qualified)
   void, or a complete type, returns it, otherwise NULL_TREE.  */

static tree
check_trait_type (tree type)
{
  if (TREE_CODE (type) == ARRAY_TYPE && !TYPE_DOMAIN (type)
      && COMPLETE_TYPE_P (TREE_TYPE (type)))
    return type;

  if (VOID_TYPE_P (type))
    return type;

  return complete_type_or_else (strip_array_types (type), NULL_TREE);
}

/* Process a trait expression.  */

tree
finish_trait_expr (cp_trait_kind kind, tree type1, tree type2)
{
  gcc_assert (kind == CPTK_HAS_NOTHROW_ASSIGN
	      || kind == CPTK_HAS_NOTHROW_CONSTRUCTOR
	      || kind == CPTK_HAS_NOTHROW_COPY
	      || kind == CPTK_HAS_TRIVIAL_ASSIGN
	      || kind == CPTK_HAS_TRIVIAL_CONSTRUCTOR
	      || kind == CPTK_HAS_TRIVIAL_COPY
	      || kind == CPTK_HAS_TRIVIAL_DESTRUCTOR
	      || kind == CPTK_HAS_VIRTUAL_DESTRUCTOR	      
	      || kind == CPTK_IS_ABSTRACT
	      || kind == CPTK_IS_BASE_OF
	      || kind == CPTK_IS_CLASS
	      || kind == CPTK_IS_CONVERTIBLE_TO
	      || kind == CPTK_IS_EMPTY
	      || kind == CPTK_IS_ENUM
	      || kind == CPTK_IS_FINAL
	      || kind == CPTK_IS_LITERAL_TYPE
	      || kind == CPTK_IS_POD
	      || kind == CPTK_IS_POLYMORPHIC
	      || kind == CPTK_IS_STD_LAYOUT
	      || kind == CPTK_IS_TRIVIAL
	      || kind == CPTK_IS_UNION);

  if (kind == CPTK_IS_CONVERTIBLE_TO)
    {
      sorry ("__is_convertible_to");
      return error_mark_node;
    }

  if (type1 == error_mark_node
      || ((kind == CPTK_IS_BASE_OF || kind == CPTK_IS_CONVERTIBLE_TO)
	  && type2 == error_mark_node))
    return error_mark_node;

  if (processing_template_decl)
    {
      tree trait_expr = make_node (TRAIT_EXPR);
      TREE_TYPE (trait_expr) = boolean_type_node;
      TRAIT_EXPR_TYPE1 (trait_expr) = type1;
      TRAIT_EXPR_TYPE2 (trait_expr) = type2;
      TRAIT_EXPR_KIND (trait_expr) = kind;
      return trait_expr;
    }

  switch (kind)
    {
    case CPTK_HAS_NOTHROW_ASSIGN:
    case CPTK_HAS_TRIVIAL_ASSIGN:
    case CPTK_HAS_NOTHROW_CONSTRUCTOR:
    case CPTK_HAS_TRIVIAL_CONSTRUCTOR:
    case CPTK_HAS_NOTHROW_COPY:
    case CPTK_HAS_TRIVIAL_COPY:
    case CPTK_HAS_TRIVIAL_DESTRUCTOR:
    case CPTK_HAS_VIRTUAL_DESTRUCTOR:
    case CPTK_IS_ABSTRACT:
    case CPTK_IS_EMPTY:
    case CPTK_IS_FINAL:
    case CPTK_IS_LITERAL_TYPE:
    case CPTK_IS_POD:
    case CPTK_IS_POLYMORPHIC:
    case CPTK_IS_STD_LAYOUT:
    case CPTK_IS_TRIVIAL:
      if (!check_trait_type (type1))
	return error_mark_node;
      break;

    case CPTK_IS_BASE_OF:
      if (NON_UNION_CLASS_TYPE_P (type1) && NON_UNION_CLASS_TYPE_P (type2)
	  && !same_type_ignoring_top_level_qualifiers_p (type1, type2)
	  && !complete_type_or_else (type2, NULL_TREE))
	/* We already issued an error.  */
	return error_mark_node;
      break;

    case CPTK_IS_CLASS:
    case CPTK_IS_ENUM:
    case CPTK_IS_UNION:
      break;
    
    case CPTK_IS_CONVERTIBLE_TO:
    default:
      gcc_unreachable ();
    }

  return (trait_expr_value (kind, type1, type2)
	  ? boolean_true_node : boolean_false_node);
}

/* Do-nothing variants of functions to handle pragma FLOAT_CONST_DECIMAL64,
   which is ignored for C++.  */

void
set_float_const_decimal64 (void)
{
}

void
clear_float_const_decimal64 (void)
{
}

bool
float_const_decimal64_p (void)
{
  return 0;
}


/* Return true if T is a literal type.   */

bool
literal_type_p (tree t)
{
  if (SCALAR_TYPE_P (t)
      || TREE_CODE (t) == VECTOR_TYPE
      || TREE_CODE (t) == REFERENCE_TYPE)
    return true;
  if (CLASS_TYPE_P (t))
    {
      t = complete_type (t);
      gcc_assert (COMPLETE_TYPE_P (t) || errorcount);
      return CLASSTYPE_LITERAL_P (t);
    }
  if (TREE_CODE (t) == ARRAY_TYPE)
    return literal_type_p (strip_array_types (t));
  return false;
}

/* If DECL is a variable declared `constexpr', require its type
   be literal.  Return the DECL if OK, otherwise NULL.  */

tree
ensure_literal_type_for_constexpr_object (tree decl)
{
  tree type = TREE_TYPE (decl);
  if (VAR_P (decl) && DECL_DECLARED_CONSTEXPR_P (decl)
      && !processing_template_decl)
    {
      if (CLASS_TYPE_P (type) && !COMPLETE_TYPE_P (complete_type (type)))
	/* Don't complain here, we'll complain about incompleteness
	   when we try to initialize the variable.  */;
      else if (!literal_type_p (type))
	{
	  error ("the type %qT of constexpr variable %qD is not literal",
		 type, decl);
	  explain_non_literal_class (type);
	  return NULL;
	}
    }
  return decl;
}

/* Representation of entries in the constexpr function definition table.  */

typedef struct GTY(()) constexpr_fundef {
  tree decl;
  tree body;
} constexpr_fundef;

/* This table holds all constexpr function definitions seen in
   the current translation unit.  */

static GTY ((param_is (constexpr_fundef))) htab_t constexpr_fundef_table;

/* Utility function used for managing the constexpr function table.
   Return true if the entries pointed to by P and Q are for the
   same constexpr function.  */

static inline int
constexpr_fundef_equal (const void *p, const void *q)
{
  const constexpr_fundef *lhs = (const constexpr_fundef *) p;
  const constexpr_fundef *rhs = (const constexpr_fundef *) q;
  return lhs->decl == rhs->decl;
}

/* Utility function used for managing the constexpr function table.
   Return a hash value for the entry pointed to by Q.  */

static inline hashval_t
constexpr_fundef_hash (const void *p)
{
  const constexpr_fundef *fundef = (const constexpr_fundef *) p;
  return DECL_UID (fundef->decl);
}

/* Return a previously saved definition of function FUN.   */

static constexpr_fundef *
retrieve_constexpr_fundef (tree fun)
{
  constexpr_fundef fundef = { NULL, NULL };
  if (constexpr_fundef_table == NULL)
    return NULL;

  fundef.decl = fun;
  return (constexpr_fundef *) htab_find (constexpr_fundef_table, &fundef);
}

/* Check whether the parameter and return types of FUN are valid for a
   constexpr function, and complain if COMPLAIN.  */

static bool
is_valid_constexpr_fn (tree fun, bool complain)
{
  tree parm = FUNCTION_FIRST_USER_PARM (fun);
  bool ret = true;
  for (; parm != NULL; parm = TREE_CHAIN (parm))
    if (!literal_type_p (TREE_TYPE (parm)))
      {
	ret = false;
	if (complain)
	  {
	    error ("invalid type for parameter %d of constexpr "
		   "function %q+#D", DECL_PARM_INDEX (parm), fun);
	    explain_non_literal_class (TREE_TYPE (parm));
	  }
      }

  if (!DECL_CONSTRUCTOR_P (fun))
    {
      tree rettype = TREE_TYPE (TREE_TYPE (fun));
      if (!literal_type_p (rettype))
	{
	  ret = false;
	  if (complain)
	    {
	      error ("invalid return type %qT of constexpr function %q+D",
		     rettype, fun);
	      explain_non_literal_class (rettype);
	    }
	}

      if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fun)
	  && !CLASSTYPE_LITERAL_P (DECL_CONTEXT (fun)))
	{
	  ret = false;
	  if (complain)
	    {
	      error ("enclosing class of constexpr non-static member "
		     "function %q+#D is not a literal type", fun);
	      explain_non_literal_class (DECL_CONTEXT (fun));
	    }
	}
    }
  else if (CLASSTYPE_VBASECLASSES (DECL_CONTEXT (fun)))
    {
      ret = false;
      if (complain)
	error ("%q#T has virtual base classes", DECL_CONTEXT (fun));
    }

  return ret;
}

/* Subroutine of build_data_member_initialization.  MEMBER is a COMPONENT_REF
   for a member of an anonymous aggregate, INIT is the initializer for that
   member, and VEC_OUTER is the vector of constructor elements for the class
   whose constructor we are processing.  Add the initializer to the vector
   and return true to indicate success.  */

static bool
build_anon_member_initialization (tree member, tree init,
				  vec<constructor_elt, va_gc> **vec_outer)
{
  /* MEMBER presents the relevant fields from the inside out, but we need
     to build up the initializer from the outside in so that we can reuse
     previously built CONSTRUCTORs if this is, say, the second field in an
     anonymous struct.  So we use a vec as a stack.  */
  auto_vec<tree, 2> fields;
  do
    {
      fields.safe_push (TREE_OPERAND (member, 1));
      member = TREE_OPERAND (member, 0);
    }
  while (ANON_AGGR_TYPE_P (TREE_TYPE (member)));

  /* VEC has the constructor elements vector for the context of FIELD.
     If FIELD is an anonymous aggregate, we will push inside it.  */
  vec<constructor_elt, va_gc> **vec = vec_outer;
  tree field;
  while (field = fields.pop(),
	 ANON_AGGR_TYPE_P (TREE_TYPE (field)))
    {
      tree ctor;
      /* If there is already an outer constructor entry for the anonymous
	 aggregate FIELD, use it; otherwise, insert one.  */
      if (vec_safe_is_empty (*vec)
	  || (*vec)->last().index != field)
	{
	  ctor = build_constructor (TREE_TYPE (field), NULL);
	  CONSTRUCTOR_APPEND_ELT (*vec, field, ctor);
	}
      else
	ctor = (*vec)->last().value;
      vec = &CONSTRUCTOR_ELTS (ctor);
    }

  /* Now we're at the innermost field, the one that isn't an anonymous
     aggregate.  Add its initializer to the CONSTRUCTOR and we're done.  */
  gcc_assert (fields.is_empty());
  CONSTRUCTOR_APPEND_ELT (*vec, field, init);

  return true;
}

/* Subroutine of  build_constexpr_constructor_member_initializers.
   The expression tree T represents a data member initialization
   in a (constexpr) constructor definition.  Build a pairing of
   the data member with its initializer, and prepend that pair
   to the existing initialization pair INITS.  */

static bool
build_data_member_initialization (tree t, vec<constructor_elt, va_gc> **vec)
{
  tree member, init;
  if (TREE_CODE (t) == CLEANUP_POINT_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == EXPR_STMT)
    t = TREE_OPERAND (t, 0);
  if (t == error_mark_node)
    return false;
  if (TREE_CODE (t) == STATEMENT_LIST)
    {
      tree_stmt_iterator i;
      for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
	{
	  if (! build_data_member_initialization (tsi_stmt (i), vec))
	    return false;
	}
      return true;
    }
  if (TREE_CODE (t) == CLEANUP_STMT)
    {
      /* We can't see a CLEANUP_STMT in a constructor for a literal class,
	 but we can in a constexpr constructor for a non-literal class.  Just
	 ignore it; either all the initialization will be constant, in which
	 case the cleanup can't run, or it can't be constexpr.
	 Still recurse into CLEANUP_BODY.  */
      return build_data_member_initialization (CLEANUP_BODY (t), vec);
    }
  if (TREE_CODE (t) == CONVERT_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == INIT_EXPR
      || TREE_CODE (t) == MODIFY_EXPR)
    {
      member = TREE_OPERAND (t, 0);
      init = break_out_target_exprs (TREE_OPERAND (t, 1));
    }
  else if (TREE_CODE (t) == CALL_EXPR)
    {
      member = CALL_EXPR_ARG (t, 0);
      /* We don't use build_cplus_new here because it complains about
	 abstract bases.  Leaving the call unwrapped means that it has the
	 wrong type, but cxx_eval_constant_expression doesn't care.  */
      init = break_out_target_exprs (t);
    }
  else if (TREE_CODE (t) == DECL_EXPR)
    /* Declaring a temporary, don't add it to the CONSTRUCTOR.  */
    return true;
  else
    gcc_unreachable ();
  if (INDIRECT_REF_P (member))
    member = TREE_OPERAND (member, 0);
  if (TREE_CODE (member) == NOP_EXPR)
    {
      tree op = member;
      STRIP_NOPS (op);
      if (TREE_CODE (op) == ADDR_EXPR)
	{
	  gcc_assert (same_type_ignoring_top_level_qualifiers_p
		      (TREE_TYPE (TREE_TYPE (op)),
		       TREE_TYPE (TREE_TYPE (member))));
	  /* Initializing a cv-qualified member; we need to look through
	     the const_cast.  */
	  member = op;
	}
      else if (op == current_class_ptr
	       && (same_type_ignoring_top_level_qualifiers_p
		   (TREE_TYPE (TREE_TYPE (member)),
		    current_class_type)))
	/* Delegating constructor.  */
	member = op;
      else
	{
	  /* This is an initializer for an empty base; keep it for now so
	     we can check it in cxx_eval_bare_aggregate.  */
	  gcc_assert (is_empty_class (TREE_TYPE (TREE_TYPE (member))));
	}
    }
  if (TREE_CODE (member) == ADDR_EXPR)
    member = TREE_OPERAND (member, 0);
  if (TREE_CODE (member) == COMPONENT_REF)
    {
      tree aggr = TREE_OPERAND (member, 0);
      if (TREE_CODE (aggr) != COMPONENT_REF)
	/* Normal member initialization.  */
	member = TREE_OPERAND (member, 1);
      else if (ANON_AGGR_TYPE_P (TREE_TYPE (aggr)))
	/* Initializing a member of an anonymous union.  */
	return build_anon_member_initialization (member, init, vec);
      else
	/* We're initializing a vtable pointer in a base.  Leave it as
	   COMPONENT_REF so we remember the path to get to the vfield.  */
	gcc_assert (TREE_TYPE (member) == vtbl_ptr_type_node);
    }

  CONSTRUCTOR_APPEND_ELT (*vec, member, init);
  return true;
}

/* Make sure that there are no statements after LAST in the constructor
   body represented by LIST.  */

bool
check_constexpr_ctor_body (tree last, tree list)
{
  bool ok = true;
  if (TREE_CODE (list) == STATEMENT_LIST)
    {
      tree_stmt_iterator i = tsi_last (list);
      for (; !tsi_end_p (i); tsi_prev (&i))
	{
	  tree t = tsi_stmt (i);
	  if (t == last)
	    break;
	  if (TREE_CODE (t) == BIND_EXPR)
	    {
	      if (BIND_EXPR_VARS (t))
		{
		  ok = false;
		  break;
		}
	      if (!check_constexpr_ctor_body (last, BIND_EXPR_BODY (t)))
		return false;
	      else
		continue;
	    }
	  /* We currently allow typedefs and static_assert.
	     FIXME allow them in the standard, too.  */
	  if (TREE_CODE (t) != STATIC_ASSERT)
	    {
	      ok = false;
	      break;
	    }
	}
    }
  else if (list != last
	   && TREE_CODE (list) != STATIC_ASSERT)
    ok = false;
  if (!ok)
    {
      error ("constexpr constructor does not have empty body");
      DECL_DECLARED_CONSTEXPR_P (current_function_decl) = false;
    }
  return ok;
}

/* V is a vector of constructor elements built up for the base and member
   initializers of a constructor for TYPE.  They need to be in increasing
   offset order, which they might not be yet if TYPE has a primary base
   which is not first in the base-clause or a vptr and at least one base
   all of which are non-primary.  */

static vec<constructor_elt, va_gc> *
sort_constexpr_mem_initializers (tree type, vec<constructor_elt, va_gc> *v)
{
  tree pri = CLASSTYPE_PRIMARY_BINFO (type);
  tree field_type;
  constructor_elt elt;
  int i;

  if (pri)
    field_type = BINFO_TYPE (pri);
  else if (TYPE_CONTAINS_VPTR_P (type))
    field_type = vtbl_ptr_type_node;
  else
    return v;

  /* Find the element for the primary base or vptr and move it to the
     beginning of the vec.  */
  vec<constructor_elt, va_gc> &vref = *v;
  for (i = 0; ; ++i)
    if (TREE_TYPE (vref[i].index) == field_type)
      break;

  if (i > 0)
    {
      elt = vref[i];
      for (; i > 0; --i)
	vref[i] = vref[i-1];
      vref[0] = elt;
    }

  return v;
}

/* Build compile-time evalable representations of member-initializer list
   for a constexpr constructor.  */

static tree
build_constexpr_constructor_member_initializers (tree type, tree body)
{
  vec<constructor_elt, va_gc> *vec = NULL;
  bool ok = true;
  if (TREE_CODE (body) == MUST_NOT_THROW_EXPR
      || TREE_CODE (body) == EH_SPEC_BLOCK)
    body = TREE_OPERAND (body, 0);
  if (TREE_CODE (body) == STATEMENT_LIST)
    body = STATEMENT_LIST_HEAD (body)->stmt;
  body = BIND_EXPR_BODY (body);
  if (TREE_CODE (body) == CLEANUP_POINT_EXPR)
    {
      body = TREE_OPERAND (body, 0);
      if (TREE_CODE (body) == EXPR_STMT)
	body = TREE_OPERAND (body, 0);
      if (TREE_CODE (body) == INIT_EXPR
	  && (same_type_ignoring_top_level_qualifiers_p
	      (TREE_TYPE (TREE_OPERAND (body, 0)),
	       current_class_type)))
	{
	  /* Trivial copy.  */
	  return TREE_OPERAND (body, 1);
	}
      ok = build_data_member_initialization (body, &vec);
    }
  else if (TREE_CODE (body) == STATEMENT_LIST)
    {
      tree_stmt_iterator i;
      for (i = tsi_start (body); !tsi_end_p (i); tsi_next (&i))
	{
	  ok = build_data_member_initialization (tsi_stmt (i), &vec);
	  if (!ok)
	    break;
	}
    }
  else if (TREE_CODE (body) == TRY_BLOCK)
    {
      error ("body of %<constexpr%> constructor cannot be "
	     "a function-try-block");
      return error_mark_node;
    }
  else if (EXPR_P (body))
    ok = build_data_member_initialization (body, &vec);
  else
    gcc_assert (errorcount > 0);
  if (ok)
    {
      if (vec_safe_length (vec) > 0)
	{
	  /* In a delegating constructor, return the target.  */
	  constructor_elt *ce = &(*vec)[0];
	  if (ce->index == current_class_ptr)
	    {
	      body = ce->value;
	      vec_free (vec);
	      return body;
	    }
	}
      vec = sort_constexpr_mem_initializers (type, vec);
      return build_constructor (type, vec);
    }
  else
    return error_mark_node;
}

/* Subroutine of register_constexpr_fundef.  BODY is the body of a function
   declared to be constexpr, or a sub-statement thereof.  Returns the
   return value if suitable, error_mark_node for a statement not allowed in
   a constexpr function, or NULL_TREE if no return value was found.  */

static tree
constexpr_fn_retval (tree body)
{
  switch (TREE_CODE (body))
    {
    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	tree expr = NULL_TREE;
	for (i = tsi_start (body); !tsi_end_p (i); tsi_next (&i))
	  {
	    tree s = constexpr_fn_retval (tsi_stmt (i));
	    if (s == error_mark_node)
	      return error_mark_node;
	    else if (s == NULL_TREE)
	      /* Keep iterating.  */;
	    else if (expr)
	      /* Multiple return statements.  */
	      return error_mark_node;
	    else
	      expr = s;
	  }
	return expr;
      }

    case RETURN_EXPR:
      return break_out_target_exprs (TREE_OPERAND (body, 0));

    case DECL_EXPR:
      if (TREE_CODE (DECL_EXPR_DECL (body)) == USING_DECL)
	return NULL_TREE;
      return error_mark_node;

    case CLEANUP_POINT_EXPR:
      return constexpr_fn_retval (TREE_OPERAND (body, 0));

    case USING_STMT:
      return NULL_TREE;

    default:
      return error_mark_node;
    }
}

/* Subroutine of register_constexpr_fundef.  BODY is the DECL_SAVED_TREE of
   FUN; do the necessary transformations to turn it into a single expression
   that we can store in the hash table.  */

static tree
massage_constexpr_body (tree fun, tree body)
{
  if (DECL_CONSTRUCTOR_P (fun))
    body = build_constexpr_constructor_member_initializers
      (DECL_CONTEXT (fun), body);
  else
    {
      if (TREE_CODE (body) == EH_SPEC_BLOCK)
        body = EH_SPEC_STMTS (body);
      if (TREE_CODE (body) == MUST_NOT_THROW_EXPR)
	body = TREE_OPERAND (body, 0);
      if (TREE_CODE (body) == BIND_EXPR)
	body = BIND_EXPR_BODY (body);
      body = constexpr_fn_retval (body);
    }
  return body;
}

/* FUN is a constexpr constructor with massaged body BODY.  Return true
   if some bases/fields are uninitialized, and complain if COMPLAIN.  */

static bool
cx_check_missing_mem_inits (tree fun, tree body, bool complain)
{
  bool bad;
  tree field;
  unsigned i, nelts;
  tree ctype;

  if (TREE_CODE (body) != CONSTRUCTOR)
    return false;

  nelts = CONSTRUCTOR_NELTS (body);
  ctype = DECL_CONTEXT (fun);
  field = TYPE_FIELDS (ctype);

  if (TREE_CODE (ctype) == UNION_TYPE)
    {
      if (nelts == 0 && next_initializable_field (field))
	{
	  if (complain)
	    error ("%<constexpr%> constructor for union %qT must "
		   "initialize exactly one non-static data member", ctype);
	  return true;
	}
      return false;
    }

  bad = false;
  for (i = 0; i <= nelts; ++i)
    {
      tree index;
      if (i == nelts)
	index = NULL_TREE;
      else
	{
	  index = CONSTRUCTOR_ELT (body, i)->index;
	  /* Skip base and vtable inits.  */
	  if (TREE_CODE (index) != FIELD_DECL
	      || DECL_ARTIFICIAL (index))
	    continue;
	}
      for (; field != index; field = DECL_CHAIN (field))
	{
	  tree ftype;
	  if (TREE_CODE (field) != FIELD_DECL
	      || (DECL_C_BIT_FIELD (field) && !DECL_NAME (field))
	      || DECL_ARTIFICIAL (field))
	    continue;
	  ftype = strip_array_types (TREE_TYPE (field));
	  if (type_has_constexpr_default_constructor (ftype))
	    {
	      /* It's OK to skip a member with a trivial constexpr ctor.
	         A constexpr ctor that isn't trivial should have been
	         added in by now.  */
	      gcc_checking_assert (!TYPE_HAS_COMPLEX_DFLT (ftype)
				   || errorcount != 0);
	      continue;
	    }
	  if (!complain)
	    return true;
	  error ("uninitialized member %qD in %<constexpr%> constructor",
		 field);
	  bad = true;
	}
      if (field == NULL_TREE)
	break;
      field = DECL_CHAIN (field);
    }

  return bad;
}

/* We are processing the definition of the constexpr function FUN.
   Check that its BODY fulfills the propriate requirements and
   enter it in the constexpr function definition table.
   For constructor BODY is actually the TREE_LIST of the
   member-initializer list.  */

tree
register_constexpr_fundef (tree fun, tree body)
{
  constexpr_fundef entry;
  constexpr_fundef **slot;

  if (!is_valid_constexpr_fn (fun, !DECL_GENERATED_P (fun)))
    return NULL;

  body = massage_constexpr_body (fun, body);
  if (body == NULL_TREE || body == error_mark_node)
    {
      if (!DECL_CONSTRUCTOR_P (fun))
	error ("body of constexpr function %qD not a return-statement", fun);
      return NULL;
    }

  if (!potential_rvalue_constant_expression (body))
    {
      if (!DECL_GENERATED_P (fun))
	require_potential_rvalue_constant_expression (body);
      return NULL;
    }

  if (DECL_CONSTRUCTOR_P (fun)
      && cx_check_missing_mem_inits (fun, body, !DECL_GENERATED_P (fun)))
    return NULL;

  /* Create the constexpr function table if necessary.  */
  if (constexpr_fundef_table == NULL)
    constexpr_fundef_table = htab_create_ggc (101,
                                              constexpr_fundef_hash,
                                              constexpr_fundef_equal,
                                              ggc_free);
  entry.decl = fun;
  entry.body = body;
  slot = (constexpr_fundef **)
    htab_find_slot (constexpr_fundef_table, &entry, INSERT);

  gcc_assert (*slot == NULL);
  *slot = ggc_alloc_constexpr_fundef ();
  **slot = entry;

  return fun;
}

/* FUN is a non-constexpr function called in a context that requires a
   constant expression.  If it comes from a constexpr template, explain why
   the instantiation isn't constexpr.  */

void
explain_invalid_constexpr_fn (tree fun)
{
  static struct pointer_set_t *diagnosed;
  tree body;
  location_t save_loc;
  /* Only diagnose defaulted functions or instantiations.  */
  if (!DECL_DEFAULTED_FN (fun)
      && !is_instantiation_of_constexpr (fun))
    return;
  if (diagnosed == NULL)
    diagnosed = pointer_set_create ();
  if (pointer_set_insert (diagnosed, fun) != 0)
    /* Already explained.  */
    return;

  save_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (fun);
  inform (0, "%q+D is not usable as a constexpr function because:", fun);
  /* First check the declaration.  */
  if (is_valid_constexpr_fn (fun, true))
    {
      /* Then if it's OK, the body.  */
      if (DECL_DEFAULTED_FN (fun))
	explain_implicit_non_constexpr (fun);
      else
	{
	  body = massage_constexpr_body (fun, DECL_SAVED_TREE (fun));
	  require_potential_rvalue_constant_expression (body);
	  if (DECL_CONSTRUCTOR_P (fun))
	    cx_check_missing_mem_inits (fun, body, true);
	}
    }
  input_location = save_loc;
}

/* Objects of this type represent calls to constexpr functions
   along with the bindings of parameters to their arguments, for
   the purpose of compile time evaluation.  */

typedef struct GTY(()) constexpr_call {
  /* Description of the constexpr function definition.  */
  constexpr_fundef *fundef;
  /* Parameter bindings environment.  A TREE_LIST where each TREE_PURPOSE
     is a parameter _DECL and the TREE_VALUE is the value of the parameter.
     Note: This arrangement is made to accommodate the use of
     iterative_hash_template_arg (see pt.c).  If you change this
     representation, also change the hash calculation in
     cxx_eval_call_expression.  */
  tree bindings;
  /* Result of the call.
       NULL means the call is being evaluated.
       error_mark_node means that the evaluation was erroneous;
       otherwise, the actuall value of the call.  */
  tree result;
  /* The hash of this call; we remember it here to avoid having to
     recalculate it when expanding the hash table.  */
  hashval_t hash;
} constexpr_call;

/* A table of all constexpr calls that have been evaluated by the
   compiler in this translation unit.  */

static GTY ((param_is (constexpr_call))) htab_t constexpr_call_table;

static tree cxx_eval_constant_expression (const constexpr_call *, tree,
					  bool, bool, bool *, bool *);

/* Compute a hash value for a constexpr call representation.  */

static hashval_t
constexpr_call_hash (const void *p)
{
  const constexpr_call *info = (const constexpr_call *) p;
  return info->hash;
}

/* Return 1 if the objects pointed to by P and Q represent calls
   to the same constexpr function with the same arguments.
   Otherwise, return 0.  */

static int
constexpr_call_equal (const void *p, const void *q)
{
  const constexpr_call *lhs = (const constexpr_call *) p;
  const constexpr_call *rhs = (const constexpr_call *) q;
  tree lhs_bindings;
  tree rhs_bindings;
  if (lhs == rhs)
    return 1;
  if (!constexpr_fundef_equal (lhs->fundef, rhs->fundef))
    return 0;
  lhs_bindings = lhs->bindings;
  rhs_bindings = rhs->bindings;
  while (lhs_bindings != NULL && rhs_bindings != NULL)
    {
      tree lhs_arg = TREE_VALUE (lhs_bindings);
      tree rhs_arg = TREE_VALUE (rhs_bindings);
      gcc_assert (TREE_TYPE (lhs_arg) == TREE_TYPE (rhs_arg));
      if (!cp_tree_equal (lhs_arg, rhs_arg))
        return 0;
      lhs_bindings = TREE_CHAIN (lhs_bindings);
      rhs_bindings = TREE_CHAIN (rhs_bindings);
    }
  return lhs_bindings == rhs_bindings;
}

/* Initialize the constexpr call table, if needed.  */

static void
maybe_initialize_constexpr_call_table (void)
{
  if (constexpr_call_table == NULL)
    constexpr_call_table = htab_create_ggc (101,
                                            constexpr_call_hash,
                                            constexpr_call_equal,
                                            ggc_free);
}

/* Return true if T designates the implied `this' parameter.  */

static inline bool
is_this_parameter (tree t)
{
  return t == current_class_ptr;
}

/* We have an expression tree T that represents a call, either CALL_EXPR
   or AGGR_INIT_EXPR.  If the call is lexically to a named function,
   retrun the _DECL for that function.  */

static tree
get_function_named_in_call (tree t)
{
  tree fun = NULL;
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      fun = CALL_EXPR_FN (t);
      break;

    case AGGR_INIT_EXPR:
      fun = AGGR_INIT_EXPR_FN (t);
      break;

    default:
      gcc_unreachable();
      break;
    }
  if (TREE_CODE (fun) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (fun, 0)) == FUNCTION_DECL)
    fun = TREE_OPERAND (fun, 0);
  return fun;
}

/* We have an expression tree T that represents a call, either CALL_EXPR
   or AGGR_INIT_EXPR.  Return the Nth argument.  */

static inline tree
get_nth_callarg (tree t, int n)
{
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      return CALL_EXPR_ARG (t, n);

    case AGGR_INIT_EXPR:
      return AGGR_INIT_EXPR_ARG (t, n);

    default:
      gcc_unreachable ();
      return NULL;
    }
}

/* Look up the binding of the function parameter T in a constexpr
   function call context CALL.  */

static tree
lookup_parameter_binding (const constexpr_call *call, tree t)
{
  tree b = purpose_member (t, call->bindings);
  return TREE_VALUE (b);
}

/* Attempt to evaluate T which represents a call to a builtin function.
   We assume here that all builtin functions evaluate to scalar types
   represented by _CST nodes.  */

static tree
cxx_eval_builtin_function_call (const constexpr_call *call, tree t,
				bool allow_non_constant, bool addr,
				bool *non_constant_p, bool *overflow_p)
{
  const int nargs = call_expr_nargs (t);
  tree *args = (tree *) alloca (nargs * sizeof (tree));
  tree new_call;
  int i;
  for (i = 0; i < nargs; ++i)
    {
      args[i] = cxx_eval_constant_expression (call, CALL_EXPR_ARG (t, i),
					      allow_non_constant, addr,
					      non_constant_p, overflow_p);
      if (allow_non_constant && *non_constant_p)
	return t;
    }
  if (*non_constant_p)
    return t;
  new_call = build_call_array_loc (EXPR_LOCATION (t), TREE_TYPE (t),
                                   CALL_EXPR_FN (t), nargs, args);
  new_call = fold (new_call);
  VERIFY_CONSTANT (new_call);
  return new_call;
}

/* TEMP is the constant value of a temporary object of type TYPE.  Adjust
   the type of the value to match.  */

static tree
adjust_temp_type (tree type, tree temp)
{
  if (TREE_TYPE (temp) == type)
    return temp;
  /* Avoid wrapping an aggregate value in a NOP_EXPR.  */
  if (TREE_CODE (temp) == CONSTRUCTOR)
    return build_constructor (type, CONSTRUCTOR_ELTS (temp));
  gcc_assert (scalarish_type_p (type));
  return cp_fold_convert (type, temp);
}

/* Subroutine of cxx_eval_call_expression.
   We are processing a call expression (either CALL_EXPR or
   AGGR_INIT_EXPR) in the call context of OLD_CALL.  Evaluate
   all arguments and bind their values to correspondings
   parameters, making up the NEW_CALL context.  */

static void
cxx_bind_parameters_in_call (const constexpr_call *old_call, tree t,
                             constexpr_call *new_call,
			     bool allow_non_constant,
			     bool *non_constant_p, bool *overflow_p)
{
  const int nargs = call_expr_nargs (t);
  tree fun = new_call->fundef->decl;
  tree parms = DECL_ARGUMENTS (fun);
  int i;
  for (i = 0; i < nargs; ++i)
    {
      tree x, arg;
      tree type = parms ? TREE_TYPE (parms) : void_type_node;
      /* For member function, the first argument is a pointer to the implied
         object.  And for an object construction, don't bind `this' before
         it is fully constructed.  */
      if (i == 0 && DECL_CONSTRUCTOR_P (fun))
        goto next;
      x = get_nth_callarg (t, i);
      if (parms && DECL_BY_REFERENCE (parms))
	{
	  /* cp_genericize made this a reference for argument passing, but
	     we don't want to treat it like one for constexpr evaluation.  */
	  gcc_assert (TREE_CODE (type) == REFERENCE_TYPE);
	  gcc_assert (TREE_CODE (TREE_TYPE (x)) == REFERENCE_TYPE);
	  type = TREE_TYPE (type);
	  x = convert_from_reference (x);
	}
      arg = cxx_eval_constant_expression (old_call, x, allow_non_constant,
					  TREE_CODE (type) == REFERENCE_TYPE,
					  non_constant_p, overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (*non_constant_p && allow_non_constant)
	return;
      /* Just discard ellipsis args after checking their constantitude.  */
      if (!parms)
	continue;
      if (*non_constant_p)
	/* Don't try to adjust the type of non-constant args.  */
	goto next;

      /* Make sure the binding has the same type as the parm.  */
      if (TREE_CODE (type) != REFERENCE_TYPE)
	arg = adjust_temp_type (type, arg);
      new_call->bindings = tree_cons (parms, arg, new_call->bindings);
    next:
      parms = TREE_CHAIN (parms);
    }
}

/* Variables and functions to manage constexpr call expansion context.
   These do not need to be marked for PCH or GC.  */

/* FIXME remember and print actual constant arguments.  */
static vec<tree> call_stack = vNULL;
static int call_stack_tick;
static int last_cx_error_tick;

static bool
push_cx_call_context (tree call)
{
  ++call_stack_tick;
  if (!EXPR_HAS_LOCATION (call))
    SET_EXPR_LOCATION (call, input_location);
  call_stack.safe_push (call);
  if (call_stack.length () > (unsigned) max_constexpr_depth)
    return false;
  return true;
}

static void
pop_cx_call_context (void)
{
  ++call_stack_tick;
  call_stack.pop ();
}

vec<tree> 
cx_error_context (void)
{
  vec<tree> r = vNULL;
  if (call_stack_tick != last_cx_error_tick
      && !call_stack.is_empty ())
    r = call_stack;
  last_cx_error_tick = call_stack_tick;
  return r;
}

/* Subroutine of cxx_eval_constant_expression.
   Evaluate the call expression tree T in the context of OLD_CALL expression
   evaluation.  */

static tree
cxx_eval_call_expression (const constexpr_call *old_call, tree t,
			  bool allow_non_constant, bool addr,
			  bool *non_constant_p, bool *overflow_p)
{
  location_t loc = EXPR_LOC_OR_LOC (t, input_location);
  tree fun = get_function_named_in_call (t);
  tree result;
  constexpr_call new_call = { NULL, NULL, NULL, 0 };
  constexpr_call **slot;
  constexpr_call *entry;
  bool depth_ok;

  if (TREE_CODE (fun) != FUNCTION_DECL)
    {
      /* Might be a constexpr function pointer.  */
      fun = cxx_eval_constant_expression (old_call, fun, allow_non_constant,
					  /*addr*/false, non_constant_p, overflow_p);
      if (TREE_CODE (fun) == ADDR_EXPR)
	fun = TREE_OPERAND (fun, 0);
    }
  if (TREE_CODE (fun) != FUNCTION_DECL)
    {
      if (!allow_non_constant && !*non_constant_p)
	error_at (loc, "expression %qE does not designate a constexpr "
		  "function", fun);
      *non_constant_p = true;
      return t;
    }
  if (DECL_CLONED_FUNCTION_P (fun))
    fun = DECL_CLONED_FUNCTION (fun);
  if (is_builtin_fn (fun))
    return cxx_eval_builtin_function_call (old_call, t, allow_non_constant,
					   addr, non_constant_p, overflow_p);
  if (!DECL_DECLARED_CONSTEXPR_P (fun))
    {
      if (!allow_non_constant)
	{
	  error_at (loc, "call to non-constexpr function %qD", fun);
	  explain_invalid_constexpr_fn (fun);
	}
      *non_constant_p = true;
      return t;
    }

  /* Shortcut trivial constructor/op=.  */
  if (trivial_fn_p (fun))
    {
      if (call_expr_nargs (t) == 2)
	{
	  tree arg = convert_from_reference (get_nth_callarg (t, 1));
	  return cxx_eval_constant_expression (old_call, arg, allow_non_constant,
					       addr, non_constant_p, overflow_p);
	}
      else if (TREE_CODE (t) == AGGR_INIT_EXPR
	       && AGGR_INIT_ZERO_FIRST (t))
	return build_zero_init (DECL_CONTEXT (fun), NULL_TREE, false);
    }

  /* If in direct recursive call, optimize definition search.  */
  if (old_call != NULL && old_call->fundef->decl == fun)
    new_call.fundef = old_call->fundef;
  else
    {
      new_call.fundef = retrieve_constexpr_fundef (fun);
      if (new_call.fundef == NULL || new_call.fundef->body == NULL)
        {
	  if (!allow_non_constant)
	    {
	      if (DECL_INITIAL (fun))
		{
		  /* The definition of fun was somehow unsuitable.  */
		  error_at (loc, "%qD called in a constant expression", fun);
		  explain_invalid_constexpr_fn (fun);
		}
	      else
		error_at (loc, "%qD used before its definition", fun);
	    }
	  *non_constant_p = true;
          return t;
        }
    }
  cxx_bind_parameters_in_call (old_call, t, &new_call,
			       allow_non_constant, non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;

  depth_ok = push_cx_call_context (t);

  new_call.hash
    = iterative_hash_template_arg (new_call.bindings,
				   constexpr_fundef_hash (new_call.fundef));

  /* If we have seen this call before, we are done.  */
  maybe_initialize_constexpr_call_table ();
  slot = (constexpr_call **)
    htab_find_slot (constexpr_call_table, &new_call, INSERT);
  entry = *slot;
  if (entry == NULL)
    {
      /* We need to keep a pointer to the entry, not just the slot, as the
	 slot can move in the call to cxx_eval_builtin_function_call.  */
      *slot = entry = ggc_alloc_constexpr_call ();
      *entry = new_call;
    }
  /* Calls which are in progress have their result set to NULL
     so that we can detect circular dependencies.  */
  else if (entry->result == NULL)
    {
      if (!allow_non_constant)
	error ("call has circular dependency");
      *non_constant_p = true;
      entry->result = result = error_mark_node;
    }

  if (!depth_ok)
    {
      if (!allow_non_constant)
	error ("constexpr evaluation depth exceeds maximum of %d (use "
	       "-fconstexpr-depth= to increase the maximum)",
	       max_constexpr_depth);
      *non_constant_p = true;
      entry->result = result = error_mark_node;
    }
  else
    {
      result = entry->result;
      if (!result || result == error_mark_node)
	result = (cxx_eval_constant_expression
		  (&new_call, new_call.fundef->body,
		   allow_non_constant, addr,
		   non_constant_p, overflow_p));
      if (result == error_mark_node)
	*non_constant_p = true;
      if (*non_constant_p)
	entry->result = result = error_mark_node;
      else
	{
	  /* If this was a call to initialize an object, set the type of
	     the CONSTRUCTOR to the type of that object.  */
	  if (DECL_CONSTRUCTOR_P (fun))
	    {
	      tree ob_arg = get_nth_callarg (t, 0);
	      STRIP_NOPS (ob_arg);
	      gcc_assert (TYPE_PTR_P (TREE_TYPE (ob_arg))
			  && CLASS_TYPE_P (TREE_TYPE (TREE_TYPE (ob_arg))));
	      result = adjust_temp_type (TREE_TYPE (TREE_TYPE (ob_arg)),
					 result);
	    }
	  entry->result = result;
	}
    }

  pop_cx_call_context ();
  return unshare_expr (result);
}

/* FIXME speed this up, it's taking 16% of compile time on sieve testcase.  */

bool
reduced_constant_expression_p (tree t)
{
  if (TREE_CODE (t) == PTRMEM_CST)
    /* Even if we can't lower this yet, it's constant.  */
    return true;
  /* FIXME are we calling this too much?  */
  return initializer_constant_valid_p (t, TREE_TYPE (t)) != NULL_TREE;
}

/* Some expressions may have constant operands but are not constant
   themselves, such as 1/0.  Call this function (or rather, the macro
   following it) to check for that condition.

   We only call this in places that require an arithmetic constant, not in
   places where we might have a non-constant expression that can be a
   component of a constant expression, such as the address of a constexpr
   variable that might be dereferenced later.  */

static bool
verify_constant (tree t, bool allow_non_constant, bool *non_constant_p,
		 bool *overflow_p)
{
  if (!*non_constant_p && !reduced_constant_expression_p (t))
    {
      if (!allow_non_constant)
	error ("%q+E is not a constant expression", t);
      *non_constant_p = true;
    }
  if (TREE_OVERFLOW_P (t))
    {
      if (!allow_non_constant)
	{
	  permerror (input_location, "overflow in constant expression");
	  /* If we're being permissive (and are in an enforcing
	     context), ignore the overflow.  */
	  if (flag_permissive)
	    return *non_constant_p;
	}
      *overflow_p = true;
    }
  return *non_constant_p;
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce the unary expression tree T to a compile time value.
   If successful, return the value.  Otherwise issue a diagnostic
   and return error_mark_node.  */

static tree
cxx_eval_unary_expression (const constexpr_call *call, tree t,
			   bool allow_non_constant, bool addr,
			   bool *non_constant_p, bool *overflow_p)
{
  tree r;
  tree orig_arg = TREE_OPERAND (t, 0);
  tree arg = cxx_eval_constant_expression (call, orig_arg, allow_non_constant,
					   addr, non_constant_p, overflow_p);
  VERIFY_CONSTANT (arg);
  if (arg == orig_arg)
    return t;
  r = fold_build1 (TREE_CODE (t), TREE_TYPE (t), arg);
  VERIFY_CONSTANT (r);
  return r;
}

/* Subroutine of cxx_eval_constant_expression.
   Like cxx_eval_unary_expression, except for binary expressions.  */

static tree
cxx_eval_binary_expression (const constexpr_call *call, tree t,
			    bool allow_non_constant, bool addr,
			    bool *non_constant_p, bool *overflow_p)
{
  tree r;
  tree orig_lhs = TREE_OPERAND (t, 0);
  tree orig_rhs = TREE_OPERAND (t, 1);
  tree lhs, rhs;
  lhs = cxx_eval_constant_expression (call, orig_lhs,
				      allow_non_constant, addr,
				      non_constant_p, overflow_p);
  VERIFY_CONSTANT (lhs);
  rhs = cxx_eval_constant_expression (call, orig_rhs,
				      allow_non_constant, addr,
				      non_constant_p, overflow_p);
  VERIFY_CONSTANT (rhs);
  if (lhs == orig_lhs && rhs == orig_rhs)
    return t;
  r = fold_build2 (TREE_CODE (t), TREE_TYPE (t), lhs, rhs);
  VERIFY_CONSTANT (r);
  return r;
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to evaluate condition expressions.  Dead branches are not
   looked into.  */

static tree
cxx_eval_conditional_expression (const constexpr_call *call, tree t,
				 bool allow_non_constant, bool addr,
				 bool *non_constant_p, bool *overflow_p)
{
  tree val = cxx_eval_constant_expression (call, TREE_OPERAND (t, 0),
					   allow_non_constant, addr,
					   non_constant_p, overflow_p);
  VERIFY_CONSTANT (val);
  /* Don't VERIFY_CONSTANT the other operands.  */
  if (integer_zerop (val))
    return cxx_eval_constant_expression (call, TREE_OPERAND (t, 2),
					 allow_non_constant, addr,
					 non_constant_p, overflow_p);
  return cxx_eval_constant_expression (call, TREE_OPERAND (t, 1),
				       allow_non_constant, addr,
				       non_constant_p, overflow_p);
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a reference to an array slot.  */

static tree
cxx_eval_array_reference (const constexpr_call *call, tree t,
			  bool allow_non_constant, bool addr,
			  bool *non_constant_p, bool *overflow_p)
{
  tree oldary = TREE_OPERAND (t, 0);
  tree ary = cxx_eval_constant_expression (call, oldary,
					   allow_non_constant, addr,
					   non_constant_p, overflow_p);
  tree index, oldidx;
  HOST_WIDE_INT i;
  tree elem_type;
  unsigned len, elem_nchars = 1;
  if (*non_constant_p)
    return t;
  oldidx = TREE_OPERAND (t, 1);
  index = cxx_eval_constant_expression (call, oldidx,
					allow_non_constant, false,
					non_constant_p, overflow_p);
  VERIFY_CONSTANT (index);
  if (addr && ary == oldary && index == oldidx)
    return t;
  else if (addr)
    return build4 (ARRAY_REF, TREE_TYPE (t), ary, index, NULL, NULL);
  elem_type = TREE_TYPE (TREE_TYPE (ary));
  if (TREE_CODE (ary) == CONSTRUCTOR)
    len = CONSTRUCTOR_NELTS (ary);
  else if (TREE_CODE (ary) == STRING_CST)
    {
      elem_nchars = (TYPE_PRECISION (elem_type)
		     / TYPE_PRECISION (char_type_node));
      len = (unsigned) TREE_STRING_LENGTH (ary) / elem_nchars;
    }
  else
    {
      /* We can't do anything with other tree codes, so use
	 VERIFY_CONSTANT to complain and fail.  */
      VERIFY_CONSTANT (ary);
      gcc_unreachable ();
    }
  if (compare_tree_int (index, len) >= 0)
    {
      if (tree_int_cst_lt (index, array_type_nelts_top (TREE_TYPE (ary))))
	{
	  /* If it's within the array bounds but doesn't have an explicit
	     initializer, it's value-initialized.  */
	  tree val = build_value_init (elem_type, tf_warning_or_error);
	  return cxx_eval_constant_expression (call, val,
					       allow_non_constant, addr,
					       non_constant_p, overflow_p);
	}

      if (!allow_non_constant)
	error ("array subscript out of bound");
      *non_constant_p = true;
      return t;
    }
  else if (tree_int_cst_lt (index, integer_zero_node))
    {
      if (!allow_non_constant)
	error ("negative array subscript");
      *non_constant_p = true;
      return t;
    }
  i = tree_to_shwi (index);
  if (TREE_CODE (ary) == CONSTRUCTOR)
    return (*CONSTRUCTOR_ELTS (ary))[i].value;
  else if (elem_nchars == 1)
    return build_int_cst (cv_unqualified (TREE_TYPE (TREE_TYPE (ary))),
			  TREE_STRING_POINTER (ary)[i]);
  else
    {
      tree type = cv_unqualified (TREE_TYPE (TREE_TYPE (ary)));
      return native_interpret_expr (type, (const unsigned char *)
					  TREE_STRING_POINTER (ary)
					  + i * elem_nchars, elem_nchars);
    }
  /* Don't VERIFY_CONSTANT here.  */
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a field access of a value of class type.  */

static tree
cxx_eval_component_reference (const constexpr_call *call, tree t,
			      bool allow_non_constant, bool addr,
			      bool *non_constant_p, bool *overflow_p)
{
  unsigned HOST_WIDE_INT i;
  tree field;
  tree value;
  tree part = TREE_OPERAND (t, 1);
  tree orig_whole = TREE_OPERAND (t, 0);
  tree whole = cxx_eval_constant_expression (call, orig_whole,
					     allow_non_constant, addr,
					     non_constant_p, overflow_p);
  if (whole == orig_whole)
    return t;
  if (addr)
    return fold_build3 (COMPONENT_REF, TREE_TYPE (t),
			whole, part, NULL_TREE);
  /* Don't VERIFY_CONSTANT here; we only want to check that we got a
     CONSTRUCTOR.  */
  if (!*non_constant_p && TREE_CODE (whole) != CONSTRUCTOR)
    {
      if (!allow_non_constant)
	error ("%qE is not a constant expression", orig_whole);
      *non_constant_p = true;
    }
  if (DECL_MUTABLE_P (part))
    {
      if (!allow_non_constant)
	error ("mutable %qD is not usable in a constant expression", part);
      *non_constant_p = true;
    }
  if (*non_constant_p)
    return t;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (whole), i, field, value)
    {
      if (field == part)
        return value;
    }
  if (TREE_CODE (TREE_TYPE (whole)) == UNION_TYPE
      && CONSTRUCTOR_NELTS (whole) > 0)
    {
      /* DR 1188 says we don't have to deal with this.  */
      if (!allow_non_constant)
	error ("accessing %qD member instead of initialized %qD member in "
	       "constant expression", part, CONSTRUCTOR_ELT (whole, 0)->index);
      *non_constant_p = true;
      return t;
    }

  /* If there's no explicit init for this field, it's value-initialized.  */
  value = build_value_init (TREE_TYPE (t), tf_warning_or_error);
  return cxx_eval_constant_expression (call, value,
				       allow_non_constant, addr,
				       non_constant_p, overflow_p);
}

/* Subroutine of cxx_eval_constant_expression.
   Attempt to reduce a field access of a value of class type that is
   expressed as a BIT_FIELD_REF.  */

static tree
cxx_eval_bit_field_ref (const constexpr_call *call, tree t,
			bool allow_non_constant, bool addr,
			bool *non_constant_p, bool *overflow_p)
{
  tree orig_whole = TREE_OPERAND (t, 0);
  tree retval, fldval, utype, mask;
  bool fld_seen = false;
  HOST_WIDE_INT istart, isize;
  tree whole = cxx_eval_constant_expression (call, orig_whole,
					     allow_non_constant, addr,
					     non_constant_p, overflow_p);
  tree start, field, value;
  unsigned HOST_WIDE_INT i;

  if (whole == orig_whole)
    return t;
  /* Don't VERIFY_CONSTANT here; we only want to check that we got a
     CONSTRUCTOR.  */
  if (!*non_constant_p
      && TREE_CODE (whole) != VECTOR_CST
      && TREE_CODE (whole) != CONSTRUCTOR)
    {
      if (!allow_non_constant)
	error ("%qE is not a constant expression", orig_whole);
      *non_constant_p = true;
    }
  if (*non_constant_p)
    return t;

  if (TREE_CODE (whole) == VECTOR_CST)
    return fold_ternary (BIT_FIELD_REF, TREE_TYPE (t), whole,
			 TREE_OPERAND (t, 1), TREE_OPERAND (t, 2));

  start = TREE_OPERAND (t, 2);
  istart = tree_to_shwi (start);
  isize = tree_to_shwi (TREE_OPERAND (t, 1));
  utype = TREE_TYPE (t);
  if (!TYPE_UNSIGNED (utype))
    utype = build_nonstandard_integer_type (TYPE_PRECISION (utype), 1);
  retval = build_int_cst (utype, 0);
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (whole), i, field, value)
    {
      tree bitpos = bit_position (field);
      if (bitpos == start && DECL_SIZE (field) == TREE_OPERAND (t, 1))
	return value;
      if (TREE_CODE (TREE_TYPE (field)) == INTEGER_TYPE
	  && TREE_CODE (value) == INTEGER_CST
	  && tree_fits_shwi_p (bitpos)
	  && tree_fits_shwi_p (DECL_SIZE (field)))
	{
	  HOST_WIDE_INT bit = tree_to_shwi (bitpos);
	  HOST_WIDE_INT sz = tree_to_shwi (DECL_SIZE (field));
	  HOST_WIDE_INT shift;
	  if (bit >= istart && bit + sz <= istart + isize)
	    {
	      fldval = fold_convert (utype, value);
	      mask = build_int_cst_type (utype, -1);
	      mask = fold_build2 (LSHIFT_EXPR, utype, mask,
				  size_int (TYPE_PRECISION (utype) - sz));
	      mask = fold_build2 (RSHIFT_EXPR, utype, mask,
				  size_int (TYPE_PRECISION (utype) - sz));
	      fldval = fold_build2 (BIT_AND_EXPR, utype, fldval, mask);
	      shift = bit - istart;
	      if (BYTES_BIG_ENDIAN)
		shift = TYPE_PRECISION (utype) - shift - sz;
	      fldval = fold_build2 (LSHIFT_EXPR, utype, fldval,
				    size_int (shift));
	      retval = fold_build2 (BIT_IOR_EXPR, utype, retval, fldval);
	      fld_seen = true;
	    }
	}
    }
  if (fld_seen)
    return fold_convert (TREE_TYPE (t), retval);
  gcc_unreachable ();
  return error_mark_node;
}

/* Subroutine of cxx_eval_constant_expression.
   Evaluate a short-circuited logical expression T in the context
   of a given constexpr CALL.  BAILOUT_VALUE is the value for
   early return.  CONTINUE_VALUE is used here purely for
   sanity check purposes.  */

static tree
cxx_eval_logical_expression (const constexpr_call *call, tree t,
                             tree bailout_value, tree continue_value,
			     bool allow_non_constant, bool addr,
			     bool *non_constant_p, bool *overflow_p)
{
  tree r;
  tree lhs = cxx_eval_constant_expression (call, TREE_OPERAND (t, 0),
					   allow_non_constant, addr,
					   non_constant_p, overflow_p);
  VERIFY_CONSTANT (lhs);
  if (tree_int_cst_equal (lhs, bailout_value))
    return lhs;
  gcc_assert (tree_int_cst_equal (lhs, continue_value));
  r = cxx_eval_constant_expression (call, TREE_OPERAND (t, 1),
				    allow_non_constant, addr, non_constant_p, overflow_p);
  VERIFY_CONSTANT (r);
  return r;
}

/* REF is a COMPONENT_REF designating a particular field.  V is a vector of
   CONSTRUCTOR elements to initialize (part of) an object containing that
   field.  Return a pointer to the constructor_elt corresponding to the
   initialization of the field.  */

static constructor_elt *
base_field_constructor_elt (vec<constructor_elt, va_gc> *v, tree ref)
{
  tree aggr = TREE_OPERAND (ref, 0);
  tree field = TREE_OPERAND (ref, 1);
  HOST_WIDE_INT i;
  constructor_elt *ce;

  gcc_assert (TREE_CODE (ref) == COMPONENT_REF);

  if (TREE_CODE (aggr) == COMPONENT_REF)
    {
      constructor_elt *base_ce
	= base_field_constructor_elt (v, aggr);
      v = CONSTRUCTOR_ELTS (base_ce->value);
    }

  for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
    if (ce->index == field)
      return ce;

  gcc_unreachable ();
  return NULL;
}

/* Subroutine of cxx_eval_constant_expression.
   The expression tree T denotes a C-style array or a C-style
   aggregate.  Reduce it to a constant expression.  */

static tree
cxx_eval_bare_aggregate (const constexpr_call *call, tree t,
			 bool allow_non_constant, bool addr,
			 bool *non_constant_p, bool *overflow_p)
{
  vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (t);
  vec<constructor_elt, va_gc> *n;
  vec_alloc (n, vec_safe_length (v));
  constructor_elt *ce;
  HOST_WIDE_INT i;
  bool changed = false;
  gcc_assert (!BRACE_ENCLOSED_INITIALIZER_P (t));
  for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
    {
      tree elt = cxx_eval_constant_expression (call, ce->value,
					       allow_non_constant, addr,
					       non_constant_p, overflow_p);
      /* Don't VERIFY_CONSTANT here.  */
      if (allow_non_constant && *non_constant_p)
	goto fail;
      if (elt != ce->value)
	changed = true;
      if (ce->index && TREE_CODE (ce->index) == COMPONENT_REF)
	{
	  /* This is an initialization of a vfield inside a base
	     subaggregate that we already initialized; push this
	     initialization into the previous initialization.  */
	  constructor_elt *inner = base_field_constructor_elt (n, ce->index);
	  inner->value = elt;
	}
      else if (ce->index && TREE_CODE (ce->index) == NOP_EXPR)
	{
	  /* This is an initializer for an empty base; now that we've
	     checked that it's constant, we can ignore it.  */
	  gcc_assert (is_empty_class (TREE_TYPE (TREE_TYPE (ce->index))));
	}
      else
	CONSTRUCTOR_APPEND_ELT (n, ce->index, elt);
    }
  if (*non_constant_p || !changed)
    {
    fail:
      vec_free (n);
      return t;
    }
  t = build_constructor (TREE_TYPE (t), n);
  TREE_CONSTANT (t) = true;
  if (TREE_CODE (TREE_TYPE (t)) == VECTOR_TYPE)
    t = fold (t);
  return t;
}

/* Subroutine of cxx_eval_constant_expression.
   The expression tree T is a VEC_INIT_EXPR which denotes the desired
   initialization of a non-static data member of array type.  Reduce it to a
   CONSTRUCTOR.

   Note that apart from value-initialization (when VALUE_INIT is true),
   this is only intended to support value-initialization and the
   initializations done by defaulted constructors for classes with
   non-static data members of array type.  In this case, VEC_INIT_EXPR_INIT
   will either be NULL_TREE for the default constructor, or a COMPONENT_REF
   for the copy/move constructor.  */

static tree
cxx_eval_vec_init_1 (const constexpr_call *call, tree atype, tree init,
		     bool value_init, bool allow_non_constant, bool addr,
		     bool *non_constant_p, bool *overflow_p)
{
  tree elttype = TREE_TYPE (atype);
  int max = tree_to_shwi (array_type_nelts (atype));
  vec<constructor_elt, va_gc> *n;
  vec_alloc (n, max + 1);
  bool pre_init = false;
  int i;

  /* For the default constructor, build up a call to the default
     constructor of the element type.  We only need to handle class types
     here, as for a constructor to be constexpr, all members must be
     initialized, which for a defaulted default constructor means they must
     be of a class type with a constexpr default constructor.  */
  if (TREE_CODE (elttype) == ARRAY_TYPE)
    /* We only do this at the lowest level.  */;
  else if (value_init)
    {
      init = build_value_init (elttype, tf_warning_or_error);
      init = cxx_eval_constant_expression
	    (call, init, allow_non_constant, addr, non_constant_p, overflow_p);
      pre_init = true;
    }
  else if (!init)
    {
      vec<tree, va_gc> *argvec = make_tree_vector ();
      init = build_special_member_call (NULL_TREE, complete_ctor_identifier,
					&argvec, elttype, LOOKUP_NORMAL,
					tf_warning_or_error);
      release_tree_vector (argvec);
      init = cxx_eval_constant_expression (call, init, allow_non_constant,
					   addr, non_constant_p, overflow_p);
      pre_init = true;
    }

  if (*non_constant_p && !allow_non_constant)
    goto fail;

  for (i = 0; i <= max; ++i)
    {
      tree idx = build_int_cst (size_type_node, i);
      tree eltinit;
      if (TREE_CODE (elttype) == ARRAY_TYPE)
	{
	  /* A multidimensional array; recurse.  */
	  if (value_init || init == NULL_TREE)
	    eltinit = NULL_TREE;
	  else
	    eltinit = cp_build_array_ref (input_location, init, idx,
					  tf_warning_or_error);
	  eltinit = cxx_eval_vec_init_1 (call, elttype, eltinit, value_init,
					 allow_non_constant, addr,
					 non_constant_p, overflow_p);
	}
      else if (pre_init)
	{
	  /* Initializing an element using value or default initialization
	     we just pre-built above.  */
	  if (i == 0)
	    eltinit = init;
	  else
	    eltinit = unshare_expr (init);
	}
      else
	{
	  /* Copying an element.  */
	  vec<tree, va_gc> *argvec;
	  gcc_assert (same_type_ignoring_top_level_qualifiers_p
		      (atype, TREE_TYPE (init)));
	  eltinit = cp_build_array_ref (input_location, init, idx,
					tf_warning_or_error);
	  if (!real_lvalue_p (init))
	    eltinit = move (eltinit);
	  argvec = make_tree_vector ();
	  argvec->quick_push (eltinit);
	  eltinit = (build_special_member_call
		     (NULL_TREE, complete_ctor_identifier, &argvec,
		      elttype, LOOKUP_NORMAL, tf_warning_or_error));
	  release_tree_vector (argvec);
	  eltinit = cxx_eval_constant_expression
	    (call, eltinit, allow_non_constant, addr, non_constant_p, overflow_p);
	}
      if (*non_constant_p && !allow_non_constant)
	goto fail;
      CONSTRUCTOR_APPEND_ELT (n, idx, eltinit);
    }

  if (!*non_constant_p)
    {
      init = build_constructor (atype, n);
      TREE_CONSTANT (init) = true;
      return init;
    }

 fail:
  vec_free (n);
  return init;
}

static tree
cxx_eval_vec_init (const constexpr_call *call, tree t,
		   bool allow_non_constant, bool addr,
		   bool *non_constant_p, bool *overflow_p)
{
  tree atype = TREE_TYPE (t);
  tree init = VEC_INIT_EXPR_INIT (t);
  tree r = cxx_eval_vec_init_1 (call, atype, init,
				VEC_INIT_EXPR_VALUE_INIT (t),
				allow_non_constant, addr, non_constant_p, overflow_p);
  if (*non_constant_p)
    return t;
  else
    return r;
}

/* A less strict version of fold_indirect_ref_1, which requires cv-quals to
   match.  We want to be less strict for simple *& folding; if we have a
   non-const temporary that we access through a const pointer, that should
   work.  We handle this here rather than change fold_indirect_ref_1
   because we're dealing with things like ADDR_EXPR of INTEGER_CST which
   don't really make sense outside of constant expression evaluation.  Also
   we want to allow folding to COMPONENT_REF, which could cause trouble
   with TBAA in fold_indirect_ref_1.

   Try to keep this function synced with fold_indirect_ref_1.  */

static tree
cxx_fold_indirect_ref (location_t loc, tree type, tree op0, bool *empty_base)
{
  tree sub, subtype;

  sub = op0;
  STRIP_NOPS (sub);
  subtype = TREE_TYPE (sub);
  if (!POINTER_TYPE_P (subtype))
    return NULL_TREE;

  if (TREE_CODE (sub) == ADDR_EXPR)
    {
      tree op = TREE_OPERAND (sub, 0);
      tree optype = TREE_TYPE (op);

      /* *&CONST_DECL -> to the value of the const decl.  */
      if (TREE_CODE (op) == CONST_DECL)
	return DECL_INITIAL (op);
      /* *&p => p;  make sure to handle *&"str"[cst] here.  */
      if (same_type_ignoring_top_level_qualifiers_p (optype, type))
	{
	  tree fop = fold_read_from_constant_string (op);
	  if (fop)
	    return fop;
	  else
	    return op;
	}
      /* *(foo *)&fooarray => fooarray[0] */
      else if (TREE_CODE (optype) == ARRAY_TYPE
	       && (same_type_ignoring_top_level_qualifiers_p
		   (type, TREE_TYPE (optype))))
	{
	  tree type_domain = TYPE_DOMAIN (optype);
	  tree min_val = size_zero_node;
	  if (type_domain && TYPE_MIN_VALUE (type_domain))
	    min_val = TYPE_MIN_VALUE (type_domain);
	  return build4_loc (loc, ARRAY_REF, type, op, min_val,
			     NULL_TREE, NULL_TREE);
	}
      /* *(foo *)&complexfoo => __real__ complexfoo */
      else if (TREE_CODE (optype) == COMPLEX_TYPE
	       && (same_type_ignoring_top_level_qualifiers_p
		   (type, TREE_TYPE (optype))))
	return fold_build1_loc (loc, REALPART_EXPR, type, op);
      /* *(foo *)&vectorfoo => BIT_FIELD_REF<vectorfoo,...> */
      else if (TREE_CODE (optype) == VECTOR_TYPE
	       && (same_type_ignoring_top_level_qualifiers_p
		   (type, TREE_TYPE (optype))))
	{
	  tree part_width = TYPE_SIZE (type);
	  tree index = bitsize_int (0);
	  return fold_build3_loc (loc, BIT_FIELD_REF, type, op, part_width, index);
	}
      /* Also handle conversion to an empty base class, which
	 is represented with a NOP_EXPR.  */
      else if (is_empty_class (type)
	       && CLASS_TYPE_P (optype)
	       && DERIVED_FROM_P (type, optype))
	{
	  *empty_base = true;
	  return op;
	}
      /* *(foo *)&struct_with_foo_field => COMPONENT_REF */
      else if (RECORD_OR_UNION_TYPE_P (optype))
	{
	  tree field = TYPE_FIELDS (optype);
	  for (; field; field = DECL_CHAIN (field))
	    if (TREE_CODE (field) == FIELD_DECL
		&& integer_zerop (byte_position (field))
		&& (same_type_ignoring_top_level_qualifiers_p
		    (TREE_TYPE (field), type)))
	      {
		return fold_build3 (COMPONENT_REF, type, op, field, NULL_TREE);
		break;
	      }
	}
    }
  else if (TREE_CODE (sub) == POINTER_PLUS_EXPR
	   && TREE_CODE (TREE_OPERAND (sub, 1)) == INTEGER_CST)
    {
      tree op00 = TREE_OPERAND (sub, 0);
      tree op01 = TREE_OPERAND (sub, 1);

      STRIP_NOPS (op00);
      if (TREE_CODE (op00) == ADDR_EXPR)
	{
	  tree op00type;
	  op00 = TREE_OPERAND (op00, 0);
	  op00type = TREE_TYPE (op00);

	  /* ((foo*)&vectorfoo)[1] => BIT_FIELD_REF<vectorfoo,...> */
	  if (TREE_CODE (op00type) == VECTOR_TYPE
	      && (same_type_ignoring_top_level_qualifiers_p
		  (type, TREE_TYPE (op00type))))
	    {
	      HOST_WIDE_INT offset = tree_to_shwi (op01);
	      tree part_width = TYPE_SIZE (type);
	      unsigned HOST_WIDE_INT part_widthi = tree_to_shwi (part_width)/BITS_PER_UNIT;
	      unsigned HOST_WIDE_INT indexi = offset * BITS_PER_UNIT;
	      tree index = bitsize_int (indexi);

	      if (offset / part_widthi < TYPE_VECTOR_SUBPARTS (op00type))
		return fold_build3_loc (loc,
					BIT_FIELD_REF, type, op00,
					part_width, index);

	    }
	  /* ((foo*)&complexfoo)[1] => __imag__ complexfoo */
	  else if (TREE_CODE (op00type) == COMPLEX_TYPE
		   && (same_type_ignoring_top_level_qualifiers_p
		       (type, TREE_TYPE (op00type))))
	    {
	      tree size = TYPE_SIZE_UNIT (type);
	      if (tree_int_cst_equal (size, op01))
		return fold_build1_loc (loc, IMAGPART_EXPR, type, op00);
	    }
	  /* ((foo *)&fooarray)[1] => fooarray[1] */
	  else if (TREE_CODE (op00type) == ARRAY_TYPE
		   && (same_type_ignoring_top_level_qualifiers_p
		       (type, TREE_TYPE (op00type))))
	    {
	      tree type_domain = TYPE_DOMAIN (op00type);
	      tree min_val = size_zero_node;
	      if (type_domain && TYPE_MIN_VALUE (type_domain))
		min_val = TYPE_MIN_VALUE (type_domain);
	      op01 = size_binop_loc (loc, EXACT_DIV_EXPR, op01,
				     TYPE_SIZE_UNIT (type));
	      op01 = size_binop_loc (loc, PLUS_EXPR, op01, min_val);
	      return build4_loc (loc, ARRAY_REF, type, op00, op01,
				 NULL_TREE, NULL_TREE);
	    }
	  /* Also handle conversion to an empty base class, which
	     is represented with a NOP_EXPR.  */
	  else if (is_empty_class (type)
		   && CLASS_TYPE_P (op00type)
		   && DERIVED_FROM_P (type, op00type))
	    {
	      *empty_base = true;
	      return op00;
	    }
	  /* ((foo *)&struct_with_foo_field)[1] => COMPONENT_REF */
	  else if (RECORD_OR_UNION_TYPE_P (op00type))
	    {
	      tree field = TYPE_FIELDS (op00type);
	      for (; field; field = DECL_CHAIN (field))
		if (TREE_CODE (field) == FIELD_DECL
		    && tree_int_cst_equal (byte_position (field), op01)
		    && (same_type_ignoring_top_level_qualifiers_p
			(TREE_TYPE (field), type)))
		  {
		    return fold_build3 (COMPONENT_REF, type, op00,
				     field, NULL_TREE);
		    break;
		  }
	    }
	}
    }
  /* *(foo *)fooarrptr => (*fooarrptr)[0] */
  else if (TREE_CODE (TREE_TYPE (subtype)) == ARRAY_TYPE
	   && (same_type_ignoring_top_level_qualifiers_p
	       (type, TREE_TYPE (TREE_TYPE (subtype)))))
    {
      tree type_domain;
      tree min_val = size_zero_node;
      tree newsub = cxx_fold_indirect_ref (loc, TREE_TYPE (subtype), sub, NULL);
      if (newsub)
	sub = newsub;
      else
	sub = build1_loc (loc, INDIRECT_REF, TREE_TYPE (subtype), sub);
      type_domain = TYPE_DOMAIN (TREE_TYPE (sub));
      if (type_domain && TYPE_MIN_VALUE (type_domain))
	min_val = TYPE_MIN_VALUE (type_domain);
      return build4_loc (loc, ARRAY_REF, type, sub, min_val, NULL_TREE,
			 NULL_TREE);
    }

  return NULL_TREE;
}

static tree
cxx_eval_indirect_ref (const constexpr_call *call, tree t,
		       bool allow_non_constant, bool addr,
		       bool *non_constant_p, bool *overflow_p)
{
  tree orig_op0 = TREE_OPERAND (t, 0);
  tree op0 = cxx_eval_constant_expression (call, orig_op0, allow_non_constant,
					   /*addr*/false, non_constant_p, overflow_p);
  bool empty_base = false;
  tree r;

  /* Don't VERIFY_CONSTANT here.  */
  if (*non_constant_p)
    return t;

  r = cxx_fold_indirect_ref (EXPR_LOCATION (t), TREE_TYPE (t), op0,
			     &empty_base);

  if (r)
    r = cxx_eval_constant_expression (call, r, allow_non_constant,
				      addr, non_constant_p, overflow_p);
  else
    {
      tree sub = op0;
      STRIP_NOPS (sub);
      if (TREE_CODE (sub) == ADDR_EXPR)
	{
	  /* We couldn't fold to a constant value.  Make sure it's not
	     something we should have been able to fold.  */
	  gcc_assert (!same_type_ignoring_top_level_qualifiers_p
		      (TREE_TYPE (TREE_TYPE (sub)), TREE_TYPE (t)));
	  /* DR 1188 says we don't have to deal with this.  */
	  if (!allow_non_constant)
	    error ("accessing value of %qE through a %qT glvalue in a "
		   "constant expression", build_fold_indirect_ref (sub),
		   TREE_TYPE (t));
	  *non_constant_p = true;
	  return t;
	}
    }

  /* If we're pulling out the value of an empty base, make sure
     that the whole object is constant and then return an empty
     CONSTRUCTOR.  */
  if (empty_base)
    {
      VERIFY_CONSTANT (r);
      r = build_constructor (TREE_TYPE (t), NULL);
      TREE_CONSTANT (r) = true;
    }

  if (r == NULL_TREE)
    {
      if (addr && op0 != orig_op0)
	return build1 (INDIRECT_REF, TREE_TYPE (t), op0);
      if (!addr)
	VERIFY_CONSTANT (t);
      return t;
    }
  return r;
}

/* Complain about R, a VAR_DECL, not being usable in a constant expression.
   Shared between potential_constant_expression and
   cxx_eval_constant_expression.  */

static void
non_const_var_error (tree r)
{
  tree type = TREE_TYPE (r);
  error ("the value of %qD is not usable in a constant "
	 "expression", r);
  /* Avoid error cascade.  */
  if (DECL_INITIAL (r) == error_mark_node)
    return;
  if (DECL_DECLARED_CONSTEXPR_P (r))
    inform (DECL_SOURCE_LOCATION (r),
	    "%qD used in its own initializer", r);
  else if (INTEGRAL_OR_ENUMERATION_TYPE_P (type))
    {
      if (!CP_TYPE_CONST_P (type))
	inform (DECL_SOURCE_LOCATION (r),
		"%q#D is not const", r);
      else if (CP_TYPE_VOLATILE_P (type))
	inform (DECL_SOURCE_LOCATION (r),
		"%q#D is volatile", r);
      else if (!DECL_INITIAL (r)
	       || !TREE_CONSTANT (DECL_INITIAL (r)))
	inform (DECL_SOURCE_LOCATION (r),
		"%qD was not initialized with a constant "
		"expression", r);
      else
	gcc_unreachable ();
    }
  else
    {
      if (cxx_dialect >= cxx11 && !DECL_DECLARED_CONSTEXPR_P (r))
	inform (DECL_SOURCE_LOCATION (r),
		"%qD was not declared %<constexpr%>", r);
      else
	inform (DECL_SOURCE_LOCATION (r),
		"%qD does not have integral or enumeration type",
		r);
    }
}

/* Subroutine of cxx_eval_constant_expression.
   Like cxx_eval_unary_expression, except for trinary expressions.  */

static tree
cxx_eval_trinary_expression (const constexpr_call *call, tree t,
			     bool allow_non_constant, bool addr,
			     bool *non_constant_p, bool *overflow_p)
{
  int i;
  tree args[3];
  tree val;

  for (i = 0; i < 3; i++)
    {
      args[i] = cxx_eval_constant_expression (call, TREE_OPERAND (t, i),
					      allow_non_constant, addr,
					      non_constant_p, overflow_p);
      VERIFY_CONSTANT (args[i]);
    }

  val = fold_ternary_loc (EXPR_LOCATION (t), TREE_CODE (t), TREE_TYPE (t),
			  args[0], args[1], args[2]);
  if (val == NULL_TREE)
    return t;
  VERIFY_CONSTANT (val);
  return val;
}

/* Attempt to reduce the expression T to a constant value.
   On failure, issue diagnostic and return error_mark_node.  */
/* FIXME unify with c_fully_fold */

static tree
cxx_eval_constant_expression (const constexpr_call *call, tree t,
			      bool allow_non_constant, bool addr,
			      bool *non_constant_p, bool *overflow_p)
{
  tree r = t;

  if (t == error_mark_node)
    {
      *non_constant_p = true;
      return t;
    }
  if (CONSTANT_CLASS_P (t))
    {
      if (TREE_CODE (t) == PTRMEM_CST)
	t = cplus_expand_constant (t);
      else if (TREE_OVERFLOW (t) && (!flag_permissive || allow_non_constant))
	*overflow_p = true;
      return t;
    }
  if (TREE_CODE (t) != NOP_EXPR
      && reduced_constant_expression_p (t))
    return fold (t);

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
      if (addr)
	return t;
      /* else fall through. */
    case CONST_DECL:
      r = integral_constant_value (t);
      if (TREE_CODE (r) == TARGET_EXPR
	  && TREE_CODE (TARGET_EXPR_INITIAL (r)) == CONSTRUCTOR)
	r = TARGET_EXPR_INITIAL (r);
      if (DECL_P (r))
	{
	  if (!allow_non_constant)
	    non_const_var_error (r);
	  *non_constant_p = true;
	}
      break;

    case FUNCTION_DECL:
    case TEMPLATE_DECL:
    case LABEL_DECL:
      return t;

    case PARM_DECL:
      if (call && DECL_CONTEXT (t) == call->fundef->decl)
	{
	  if (DECL_ARTIFICIAL (t) && DECL_CONSTRUCTOR_P (DECL_CONTEXT (t)))
	    {
	      if (!allow_non_constant)
		sorry ("use of the value of the object being constructed "
		       "in a constant expression");
	      *non_constant_p = true;
	    }
	  else
	    r = lookup_parameter_binding (call, t);
	}
      else if (addr)
	/* Defer in case this is only used for its type.  */;
      else
	{
	  if (!allow_non_constant)
	    error ("%qE is not a constant expression", t);
	  *non_constant_p = true;
	}
      break;

    case CALL_EXPR:
    case AGGR_INIT_EXPR:
      r = cxx_eval_call_expression (call, t, allow_non_constant, addr,
				    non_constant_p, overflow_p);
      break;

    case TARGET_EXPR:
      if (!literal_type_p (TREE_TYPE (t)))
	{
	  if (!allow_non_constant)
	    {
	      error ("temporary of non-literal type %qT in a "
		     "constant expression", TREE_TYPE (t));
	      explain_non_literal_class (TREE_TYPE (t));
	    }
	  *non_constant_p = true;
	  break;
	}
      /* else fall through.  */
    case INIT_EXPR:
      /* Pass false for 'addr' because these codes indicate
	 initialization of a temporary.  */
      r = cxx_eval_constant_expression (call, TREE_OPERAND (t, 1),
					allow_non_constant, false,
					non_constant_p, overflow_p);
      if (!*non_constant_p)
	/* Adjust the type of the result to the type of the temporary.  */
	r = adjust_temp_type (TREE_TYPE (t), r);
      break;

    case SCOPE_REF:
      r = cxx_eval_constant_expression (call, TREE_OPERAND (t, 1),
					allow_non_constant, addr,
					non_constant_p, overflow_p);
      break;

    case RETURN_EXPR:
    case NON_LVALUE_EXPR:
    case TRY_CATCH_EXPR:
    case CLEANUP_POINT_EXPR:
    case MUST_NOT_THROW_EXPR:
    case SAVE_EXPR:
      r = cxx_eval_constant_expression (call, TREE_OPERAND (t, 0),
					allow_non_constant, addr,
					non_constant_p, overflow_p);
      break;

      /* These differ from cxx_eval_unary_expression in that this doesn't
	 check for a constant operand or result; an address can be
	 constant without its operand being, and vice versa.  */
    case INDIRECT_REF:
      r = cxx_eval_indirect_ref (call, t, allow_non_constant, addr,
				 non_constant_p, overflow_p);
      break;

    case ADDR_EXPR:
      {
	tree oldop = TREE_OPERAND (t, 0);
	tree op = cxx_eval_constant_expression (call, oldop,
						allow_non_constant,
						/*addr*/true,
						non_constant_p, overflow_p);
	/* Don't VERIFY_CONSTANT here.  */
	if (*non_constant_p)
	  return t;
	/* This function does more aggressive folding than fold itself.  */
	r = build_fold_addr_expr_with_type (op, TREE_TYPE (t));
	if (TREE_CODE (r) == ADDR_EXPR && TREE_OPERAND (r, 0) == oldop)
	  return t;
	break;
      }

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case CONJ_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
      r = cxx_eval_unary_expression (call, t, allow_non_constant, addr,
				     non_constant_p, overflow_p);
      break;

    case SIZEOF_EXPR:
      if (SIZEOF_EXPR_TYPE_P (t))
	r = cxx_sizeof_or_alignof_type (TREE_TYPE (TREE_OPERAND (t, 0)),
					SIZEOF_EXPR, false);
      else if (TYPE_P (TREE_OPERAND (t, 0)))
	r = cxx_sizeof_or_alignof_type (TREE_OPERAND (t, 0), SIZEOF_EXPR,
					false);
      else
	r = cxx_sizeof_or_alignof_expr (TREE_OPERAND (t, 0), SIZEOF_EXPR,
					false);
      if (r == error_mark_node)
	r = size_one_node;
      VERIFY_CONSTANT (r);
      break;

    case COMPOUND_EXPR:
      {
	/* check_return_expr sometimes wraps a TARGET_EXPR in a
	   COMPOUND_EXPR; don't get confused.  Also handle EMPTY_CLASS_EXPR
	   introduced by build_call_a.  */
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	STRIP_NOPS (op1);
	if ((TREE_CODE (op0) == TARGET_EXPR && op1 == TARGET_EXPR_SLOT (op0))
	    || TREE_CODE (op1) == EMPTY_CLASS_EXPR)
	  r = cxx_eval_constant_expression (call, op0, allow_non_constant,
					    addr, non_constant_p, overflow_p);
	else
	  {
	    /* Check that the LHS is constant and then discard it.  */
	    cxx_eval_constant_expression (call, op0, allow_non_constant,
					  false, non_constant_p, overflow_p);
	    op1 = TREE_OPERAND (t, 1);
	    r = cxx_eval_constant_expression (call, op1, allow_non_constant,
					      addr, non_constant_p, overflow_p);
	  }
      }
      break;

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
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case RANGE_EXPR:
    case COMPLEX_EXPR:
      r = cxx_eval_binary_expression (call, t, allow_non_constant, addr,
				      non_constant_p, overflow_p);
      break;

      /* fold can introduce non-IF versions of these; still treat them as
	 short-circuiting.  */
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      r = cxx_eval_logical_expression (call, t, boolean_false_node,
				       boolean_true_node,
				       allow_non_constant, addr,
				       non_constant_p, overflow_p);
      break;

    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      r = cxx_eval_logical_expression (call, t, boolean_true_node,
				       boolean_false_node,
				       allow_non_constant, addr,
				       non_constant_p, overflow_p);
      break;

    case ARRAY_REF:
      r = cxx_eval_array_reference (call, t, allow_non_constant, addr,
				    non_constant_p, overflow_p);
      break;

    case COMPONENT_REF:
      if (is_overloaded_fn (t))
	{
	  /* We can only get here in checking mode via 
	     build_non_dependent_expr,  because any expression that
	     calls or takes the address of the function will have
	     pulled a FUNCTION_DECL out of the COMPONENT_REF.  */
	  gcc_checking_assert (allow_non_constant);
	  *non_constant_p = true;
	  return t;
	}
      r = cxx_eval_component_reference (call, t, allow_non_constant, addr,
					non_constant_p, overflow_p);
      break;

    case BIT_FIELD_REF:
      r = cxx_eval_bit_field_ref (call, t, allow_non_constant, addr,
				  non_constant_p, overflow_p);
      break;

    case COND_EXPR:
    case VEC_COND_EXPR:
      r = cxx_eval_conditional_expression (call, t, allow_non_constant, addr,
					   non_constant_p, overflow_p);
      break;

    case CONSTRUCTOR:
      r = cxx_eval_bare_aggregate (call, t, allow_non_constant, addr,
				   non_constant_p, overflow_p);
      break;

    case VEC_INIT_EXPR:
      /* We can get this in a defaulted constructor for a class with a
	 non-static data member of array type.  Either the initializer will
	 be NULL, meaning default-initialization, or it will be an lvalue
	 or xvalue of the same type, meaning direct-initialization from the
	 corresponding member.  */
      r = cxx_eval_vec_init (call, t, allow_non_constant, addr,
			     non_constant_p, overflow_p);
      break;

    case FMA_EXPR:
    case VEC_PERM_EXPR:
      r = cxx_eval_trinary_expression (call, t, allow_non_constant, addr,
				       non_constant_p, overflow_p);
      break;

    case CONVERT_EXPR:
    case VIEW_CONVERT_EXPR:
    case NOP_EXPR:
      {
	tree oldop = TREE_OPERAND (t, 0);
	tree op = cxx_eval_constant_expression (call, oldop,
						allow_non_constant, addr,
						non_constant_p, overflow_p);
	if (*non_constant_p)
	  return t;
	if (POINTER_TYPE_P (TREE_TYPE (t))
	    && TREE_CODE (op) == INTEGER_CST
	    && !integer_zerop (op))
	  {
	    if (!allow_non_constant)
	      error_at (EXPR_LOC_OR_LOC (t, input_location),
			"reinterpret_cast from integer to pointer");
	    *non_constant_p = true;
	    return t;
	  }
	if (op == oldop)
	  /* We didn't fold at the top so we could check for ptr-int
	     conversion.  */
	  return fold (t);
	r = fold_build1 (TREE_CODE (t), TREE_TYPE (t), op);
	/* Conversion of an out-of-range value has implementation-defined
	   behavior; the language considers it different from arithmetic
	   overflow, which is undefined.  */
	if (TREE_OVERFLOW_P (r) && !TREE_OVERFLOW_P (op))
	  TREE_OVERFLOW (r) = false;
      }
      break;

    case EMPTY_CLASS_EXPR:
      /* This is good enough for a function argument that might not get
	 used, and they can't do anything with it, so just return it.  */
      return t;

    case LAMBDA_EXPR:
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
    case THROW_EXPR:
    case MODIFY_EXPR:
    case MODOP_EXPR:
      /* GCC internal stuff.  */
    case VA_ARG_EXPR:
    case OBJ_TYPE_REF:
    case WITH_CLEANUP_EXPR:
    case STATEMENT_LIST:
    case BIND_EXPR:
    case NON_DEPENDENT_EXPR:
    case BASELINK:
    case EXPR_STMT:
    case OFFSET_REF:
      if (!allow_non_constant)
        error_at (EXPR_LOC_OR_LOC (t, input_location),
		  "expression %qE is not a constant-expression", t);
      *non_constant_p = true;
      break;

    default:
      internal_error ("unexpected expression %qE of kind %s", t,
		      get_tree_code_name (TREE_CODE (t)));
      *non_constant_p = true;
      break;
    }

  if (r == error_mark_node)
    *non_constant_p = true;

  if (*non_constant_p)
    return t;
  else
    return r;
}

static tree
cxx_eval_outermost_constant_expr (tree t, bool allow_non_constant)
{
  bool non_constant_p = false;
  bool overflow_p = false;
  tree r = cxx_eval_constant_expression (NULL, t, allow_non_constant,
					 false, &non_constant_p, &overflow_p);

  verify_constant (r, allow_non_constant, &non_constant_p, &overflow_p);

  if (TREE_CODE (t) != CONSTRUCTOR
      && cp_has_mutable_p (TREE_TYPE (t)))
    {
      /* We allow a mutable type if the original expression was a
	 CONSTRUCTOR so that we can do aggregate initialization of
	 constexpr variables.  */
      if (!allow_non_constant)
	error ("%qT cannot be the type of a complete constant expression "
	       "because it has mutable sub-objects", TREE_TYPE (t));
      non_constant_p = true;
    }

  /* Technically we should check this for all subexpressions, but that
     runs into problems with our internal representation of pointer
     subtraction and the 5.19 rules are still in flux.  */
  if (CONVERT_EXPR_CODE_P (TREE_CODE (r))
      && ARITHMETIC_TYPE_P (TREE_TYPE (r))
      && TREE_CODE (TREE_OPERAND (r, 0)) == ADDR_EXPR)
    {
      if (!allow_non_constant)
	error ("conversion from pointer type %qT "
	       "to arithmetic type %qT in a constant-expression",
	       TREE_TYPE (TREE_OPERAND (r, 0)), TREE_TYPE (r));
      non_constant_p = true;
    }

  if (!non_constant_p && overflow_p)
    non_constant_p = true;

  if (non_constant_p && !allow_non_constant)
    return error_mark_node;
  else if (non_constant_p && TREE_CONSTANT (r))
    {
      /* This isn't actually constant, so unset TREE_CONSTANT.  */
      if (EXPR_P (r))
	r = copy_node (r);
      else if (TREE_CODE (r) == CONSTRUCTOR)
	r = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (r), r);
      else
	r = build_nop (TREE_TYPE (r), r);
      TREE_CONSTANT (r) = false;
    }
  else if (non_constant_p || r == t)
    return t;

  if (TREE_CODE (r) == CONSTRUCTOR && CLASS_TYPE_P (TREE_TYPE (r)))
    {
      if (TREE_CODE (t) == TARGET_EXPR
	  && TARGET_EXPR_INITIAL (t) == r)
	return t;
      else
	{
	  r = get_target_expr (r);
	  TREE_CONSTANT (r) = true;
	  return r;
	}
    }
  else
    return r;
}

/* Returns true if T is a valid subexpression of a constant expression,
   even if it isn't itself a constant expression.  */

bool
is_sub_constant_expr (tree t)
{
  bool non_constant_p = false;
  bool overflow_p = false;
  cxx_eval_constant_expression (NULL, t, true, false, &non_constant_p,
				&overflow_p);
  return !non_constant_p && !overflow_p;
}

/* If T represents a constant expression returns its reduced value.
   Otherwise return error_mark_node.  If T is dependent, then
   return NULL.  */

tree
cxx_constant_value (tree t)
{
  return cxx_eval_outermost_constant_expr (t, false);
}

/* If T is a constant expression, returns its reduced value.
   Otherwise, if T does not have TREE_CONSTANT set, returns T.
   Otherwise, returns a version of T without TREE_CONSTANT.  */

tree
maybe_constant_value (tree t)
{
  tree r;

  if (instantiation_dependent_expression_p (t)
      || type_unknown_p (t)
      || BRACE_ENCLOSED_INITIALIZER_P (t)
      || !potential_constant_expression (t))
    {
      if (TREE_OVERFLOW_P (t))
	{
	  t = build_nop (TREE_TYPE (t), t);
	  TREE_CONSTANT (t) = false;
	}
      return t;
    }

  r = cxx_eval_outermost_constant_expr (t, true);
#ifdef ENABLE_CHECKING
  /* cp_tree_equal looks through NOPs, so allow them.  */
  gcc_assert (r == t
	      || CONVERT_EXPR_P (t)
	      || (TREE_CONSTANT (t) && !TREE_CONSTANT (r))
	      || !cp_tree_equal (r, t));
#endif
  return r;
}

/* Like maybe_constant_value, but returns a CONSTRUCTOR directly, rather
   than wrapped in a TARGET_EXPR.  */

tree
maybe_constant_init (tree t)
{
  t = maybe_constant_value (t);
  if (TREE_CODE (t) == TARGET_EXPR)
    {
      tree init = TARGET_EXPR_INITIAL (t);
      if (TREE_CODE (init) == CONSTRUCTOR)
	t = init;
    }
  return t;
}

#if 0
/* FIXME see ADDR_EXPR section in potential_constant_expression_1.  */
/* Return true if the object referred to by REF has automatic or thread
   local storage.  */

enum { ck_ok, ck_bad, ck_unknown };
static int
check_automatic_or_tls (tree ref)
{
  enum machine_mode mode;
  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  int volatilep = 0, unsignedp = 0;
  tree decl = get_inner_reference (ref, &bitsize, &bitpos, &offset,
				   &mode, &unsignedp, &volatilep, false);
  duration_kind dk;

  /* If there isn't a decl in the middle, we don't know the linkage here,
     and this isn't a constant expression anyway.  */
  if (!DECL_P (decl))
    return ck_unknown;
  dk = decl_storage_duration (decl);
  return (dk == dk_auto || dk == dk_thread) ? ck_bad : ck_ok;
}
#endif

/* Return true if T denotes a potentially constant expression.  Issue
   diagnostic as appropriate under control of FLAGS.  If WANT_RVAL is true,
   an lvalue-rvalue conversion is implied.

   C++0x [expr.const] used to say

   6 An expression is a potential constant expression if it is
     a constant expression where all occurrences of function
     parameters are replaced by arbitrary constant expressions
     of the appropriate type.

   2  A conditional expression is a constant expression unless it
      involves one of the following as a potentially evaluated
      subexpression (3.2), but subexpressions of logical AND (5.14),
      logical OR (5.15), and conditional (5.16) operations that are
      not evaluated are not considered.   */

static bool
potential_constant_expression_1 (tree t, bool want_rval, tsubst_flags_t flags)
{
  enum { any = false, rval = true };
  int i;
  tree tmp;

  if (t == error_mark_node)
    return false;
  if (t == NULL_TREE)
    return true;
  if (TREE_THIS_VOLATILE (t))
    {
      if (flags & tf_error)
        error ("expression %qE has side-effects", t);
      return false;
    }
  if (CONSTANT_CLASS_P (t))
    return true;

  switch (TREE_CODE (t))
    {
    case FUNCTION_DECL:
    case BASELINK:
    case TEMPLATE_DECL:
    case OVERLOAD:
    case TEMPLATE_ID_EXPR:
    case LABEL_DECL:
    case LABEL_EXPR:
    case CONST_DECL:
    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
    case OFFSETOF_EXPR:
    case NOEXCEPT_EXPR:
    case TEMPLATE_PARM_INDEX:
    case TRAIT_EXPR:
    case IDENTIFIER_NODE:
    case USERDEF_LITERAL:
      /* We can see a FIELD_DECL in a pointer-to-member expression.  */
    case FIELD_DECL:
    case PARM_DECL:
    case USING_DECL:
      return true;

    case AGGR_INIT_EXPR:
    case CALL_EXPR:
      /* -- an invocation of a function other than a constexpr function
            or a constexpr constructor.  */
      {
        tree fun = get_function_named_in_call (t);
        const int nargs = call_expr_nargs (t);
	i = 0;

	if (is_overloaded_fn (fun))
	  {
	    if (TREE_CODE (fun) == FUNCTION_DECL)
	      {
		if (builtin_valid_in_constant_expr_p (fun))
		  return true;
		if (!DECL_DECLARED_CONSTEXPR_P (fun)
		    /* Allow any built-in function; if the expansion
		       isn't constant, we'll deal with that then.  */
		    && !is_builtin_fn (fun))
		  {
		    if (flags & tf_error)
		      {
			error_at (EXPR_LOC_OR_LOC (t, input_location),
				  "call to non-constexpr function %qD", fun);
			explain_invalid_constexpr_fn (fun);
		      }
		    return false;
		  }
		/* A call to a non-static member function takes the address
		   of the object as the first argument.  But in a constant
		   expression the address will be folded away, so look
		   through it now.  */
		if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fun)
		    && !DECL_CONSTRUCTOR_P (fun))
		  {
		    tree x = get_nth_callarg (t, 0);
		    if (is_this_parameter (x))
		      {
			if (DECL_CONTEXT (x) == NULL_TREE
			    || DECL_CONSTRUCTOR_P (DECL_CONTEXT (x)))
			  {
			    if (flags & tf_error)
			      sorry ("calling a member function of the "
				     "object being constructed in a constant "
				     "expression");
			    return false;
			  }
			/* Otherwise OK.  */;
		      }
		    else if (!potential_constant_expression_1 (x, rval, flags))
		      return false;
		    i = 1;
		  }
	      }
	    else
	      {
		if (!potential_constant_expression_1 (fun, true, flags))
		  return false;
		fun = get_first_fn (fun);
	      }
	    /* Skip initial arguments to base constructors.  */
	    if (DECL_BASE_CONSTRUCTOR_P (fun))
	      i = num_artificial_parms_for (fun);
	    fun = DECL_ORIGIN (fun);
	  }
	else
          {
	    if (potential_constant_expression_1 (fun, rval, flags))
	      /* Might end up being a constant function pointer.  */;
	    else
	      return false;
          }
        for (; i < nargs; ++i)
          {
            tree x = get_nth_callarg (t, i);
	    if (!potential_constant_expression_1 (x, rval, flags))
	      return false;
          }
        return true;
      }

    case NON_LVALUE_EXPR:
      /* -- an lvalue-to-rvalue conversion (4.1) unless it is applied to
            -- an lvalue of integral type that refers to a non-volatile
               const variable or static data member initialized with
               constant expressions, or

            -- an lvalue of literal type that refers to non-volatile
               object defined with constexpr, or that refers to a
               sub-object of such an object;  */
      return potential_constant_expression_1 (TREE_OPERAND (t, 0), rval, flags);

    case VAR_DECL:
      if (want_rval && !decl_constant_var_p (t)
	  && !dependent_type_p (TREE_TYPE (t)))
        {
          if (flags & tf_error)
            non_const_var_error (t);
          return false;
        }
      return true;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case VIEW_CONVERT_EXPR:
      /* -- a reinterpret_cast.  FIXME not implemented, and this rule
	 may change to something more specific to type-punning (DR 1312).  */
      {
        tree from = TREE_OPERAND (t, 0);
	if (POINTER_TYPE_P (TREE_TYPE (t))
	    && TREE_CODE (from) == INTEGER_CST
	    && !integer_zerop (from))
	  {
	    if (flags & tf_error)
	      error_at (EXPR_LOC_OR_LOC (t, input_location),
			"reinterpret_cast from integer to pointer");
	    return false;
	  }
        return (potential_constant_expression_1
		(from, TREE_CODE (t) != VIEW_CONVERT_EXPR, flags));
      }

    case ADDR_EXPR:
      /* -- a unary operator & that is applied to an lvalue that
            designates an object with thread or automatic storage
            duration;  */
      t = TREE_OPERAND (t, 0);
#if 0
      /* FIXME adjust when issue 1197 is fully resolved.  For now don't do
         any checking here, as we might dereference the pointer later.  If
         we remove this code, also remove check_automatic_or_tls.  */
      i = check_automatic_or_tls (t);
      if (i == ck_ok)
	return true;
      if (i == ck_bad)
        {
          if (flags & tf_error)
            error ("address-of an object %qE with thread local or "
                   "automatic storage is not a constant expression", t);
          return false;
        }
#endif
      return potential_constant_expression_1 (t, any, flags);

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARROW_EXPR:
    case OFFSET_REF:
      /* -- a class member access unless its postfix-expression is
            of literal type or of pointer to literal type.  */
      /* This test would be redundant, as it follows from the
	 postfix-expression being a potential constant expression.  */
      return potential_constant_expression_1 (TREE_OPERAND (t, 0),
					      want_rval, flags);

    case EXPR_PACK_EXPANSION:
      return potential_constant_expression_1 (PACK_EXPANSION_PATTERN (t),
					      want_rval, flags);

    case INDIRECT_REF:
      {
        tree x = TREE_OPERAND (t, 0);
        STRIP_NOPS (x);
        if (is_this_parameter (x))
	  {
	    if (DECL_CONTEXT (x)
		&& !DECL_DECLARED_CONSTEXPR_P (DECL_CONTEXT (x)))
	      {
		if (flags & tf_error)
		  error ("use of %<this%> in a constant expression");
		return false;
	      }
	    if (want_rval && DECL_CONTEXT (x)
		&& DECL_CONSTRUCTOR_P (DECL_CONTEXT (x)))
	      {
		if (flags & tf_error)
		  sorry ("use of the value of the object being constructed "
			 "in a constant expression");
		return false;
	      }
	    return true;
	  }
	return potential_constant_expression_1 (x, rval, flags);
      }

    case LAMBDA_EXPR:
    case DYNAMIC_CAST_EXPR:
    case PSEUDO_DTOR_EXPR:
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
    case THROW_EXPR:
    case MODIFY_EXPR:
    case MODOP_EXPR:
    case OMP_ATOMIC:
    case OMP_ATOMIC_READ:
    case OMP_ATOMIC_CAPTURE_OLD:
    case OMP_ATOMIC_CAPTURE_NEW:
      /* GCC internal stuff.  */
    case VA_ARG_EXPR:
    case OBJ_TYPE_REF:
    case WITH_CLEANUP_EXPR:
    case CLEANUP_POINT_EXPR:
    case MUST_NOT_THROW_EXPR:
    case TRY_CATCH_EXPR:
    case STATEMENT_LIST:
      /* Don't bother trying to define a subset of statement-expressions to
	 be constant-expressions, at least for now.  */
    case STMT_EXPR:
    case EXPR_STMT:
    case BIND_EXPR:
    case TRANSACTION_EXPR:
    case IF_STMT:
    case DO_STMT:
    case FOR_STMT:
    case WHILE_STMT:
      if (flags & tf_error)
        error ("expression %qE is not a constant-expression", t);
      return false;

    case TYPEID_EXPR:
      /* -- a typeid expression whose operand is of polymorphic
            class type;  */
      {
        tree e = TREE_OPERAND (t, 0);
        if (!TYPE_P (e) && !type_dependent_expression_p (e)
	    && TYPE_POLYMORPHIC_P (TREE_TYPE (e)))
          {
            if (flags & tf_error)
              error ("typeid-expression is not a constant expression "
                     "because %qE is of polymorphic type", e);
            return false;
          }
        return true;
      }

    case MINUS_EXPR:
      /* -- a subtraction where both operands are pointers.   */
      if (TYPE_PTR_P (TREE_OPERAND (t, 0))
          && TYPE_PTR_P (TREE_OPERAND (t, 1)))
        {
          if (flags & tf_error)
            error ("difference of two pointer expressions is not "
                   "a constant expression");
          return false;
        }
      want_rval = true;
      goto binary;

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      /* -- a relational or equality operator where at least
            one of the operands is a pointer.  */
      if (TYPE_PTR_P (TREE_OPERAND (t, 0))
          || TYPE_PTR_P (TREE_OPERAND (t, 1)))
        {
          if (flags & tf_error)
            error ("pointer comparison expression is not a "
                   "constant expression");
          return false;
        }
      want_rval = true;
      goto binary;

    case BIT_NOT_EXPR:
      /* A destructor.  */
      if (TYPE_P (TREE_OPERAND (t, 0)))
	return true;
      /* else fall through.  */

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case CONJ_EXPR:
    case SAVE_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case TRUTH_NOT_EXPR:
    case FIXED_CONVERT_EXPR:
    case UNARY_PLUS_EXPR:
      return potential_constant_expression_1 (TREE_OPERAND (t, 0), rval,
					      flags);

    case CAST_EXPR:
    case CONST_CAST_EXPR:
    case STATIC_CAST_EXPR:
    case REINTERPRET_CAST_EXPR:
    case IMPLICIT_CONV_EXPR:
      if (cxx_dialect < cxx11
	  && !dependent_type_p (TREE_TYPE (t))
	  && !INTEGRAL_OR_ENUMERATION_TYPE_P (TREE_TYPE (t)))
	/* In C++98, a conversion to non-integral type can't be part of a
	   constant expression.  */
	{
	  if (flags & tf_error)
	    error ("cast to non-integral type %qT in a constant expression",
		   TREE_TYPE (t));
	  return false;
	}

      return (potential_constant_expression_1
	      (TREE_OPERAND (t, 0),
	       TREE_CODE (TREE_TYPE (t)) != REFERENCE_TYPE, flags));

    case PAREN_EXPR:
    case NON_DEPENDENT_EXPR:
      /* For convenience.  */
    case RETURN_EXPR:
      return potential_constant_expression_1 (TREE_OPERAND (t, 0),
					      want_rval, flags);

    case SCOPE_REF:
      return potential_constant_expression_1 (TREE_OPERAND (t, 1),
					      want_rval, flags);

    case TARGET_EXPR:
      if (!literal_type_p (TREE_TYPE (t)))
	{
	  if (flags & tf_error)
	    {
	      error ("temporary of non-literal type %qT in a "
		     "constant expression", TREE_TYPE (t));
	      explain_non_literal_class (TREE_TYPE (t));
	    }
	  return false;
	}
    case INIT_EXPR:
      return potential_constant_expression_1 (TREE_OPERAND (t, 1),
					      rval, flags);

    case CONSTRUCTOR:
      {
        vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (t);
        constructor_elt *ce;
        for (i = 0; vec_safe_iterate (v, i, &ce); ++i)
	  if (!potential_constant_expression_1 (ce->value, want_rval, flags))
	    return false;
	return true;
      }

    case TREE_LIST:
      {
	gcc_assert (TREE_PURPOSE (t) == NULL_TREE
		    || DECL_P (TREE_PURPOSE (t)));
	if (!potential_constant_expression_1 (TREE_VALUE (t), want_rval,
					      flags))
	  return false;
	if (TREE_CHAIN (t) == NULL_TREE)
	  return true;
	return potential_constant_expression_1 (TREE_CHAIN (t), want_rval,
						flags);
      }

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
      {
	tree denom = TREE_OPERAND (t, 1);
	if (!potential_constant_expression_1 (denom, rval, flags))
	  return false;
	/* We can't call cxx_eval_outermost_constant_expr on an expression
	   that hasn't been through fold_non_dependent_expr yet.  */
	if (!processing_template_decl)
	  denom = cxx_eval_outermost_constant_expr (denom, true);
	if (integer_zerop (denom))
	  {
	    if (flags & tf_error)
	      error ("division by zero is not a constant-expression");
	    return false;
	  }
	else
	  {
	    want_rval = true;
	    return potential_constant_expression_1 (TREE_OPERAND (t, 0),
						    want_rval, flags);
	  }
      }

    case COMPOUND_EXPR:
      {
	/* check_return_expr sometimes wraps a TARGET_EXPR in a
	   COMPOUND_EXPR; don't get confused.  Also handle EMPTY_CLASS_EXPR
	   introduced by build_call_a.  */
	tree op0 = TREE_OPERAND (t, 0);
	tree op1 = TREE_OPERAND (t, 1);
	STRIP_NOPS (op1);
	if ((TREE_CODE (op0) == TARGET_EXPR && op1 == TARGET_EXPR_SLOT (op0))
	    || TREE_CODE (op1) == EMPTY_CLASS_EXPR)
	  return potential_constant_expression_1 (op0, want_rval, flags);
	else
	  goto binary;
      }

      /* If the first operand is the non-short-circuit constant, look at
	 the second operand; otherwise we only care about the first one for
	 potentiality.  */
    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      tmp = boolean_true_node;
      goto truth;
    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      tmp = boolean_false_node;
    truth:
      {
	tree op = TREE_OPERAND (t, 0);
	if (!potential_constant_expression_1 (op, rval, flags))
	  return false;
	if (!processing_template_decl)
	  op = cxx_eval_outermost_constant_expr (op, true);
	if (tree_int_cst_equal (op, tmp))
	  return potential_constant_expression_1 (TREE_OPERAND (t, 1), rval, flags);
	else
	  return true;
      }

    case PLUS_EXPR:
    case MULT_EXPR:
    case POINTER_PLUS_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case RANGE_EXPR:
    case COMPLEX_EXPR:
      want_rval = true;
      /* Fall through.  */
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case MEMBER_REF:
    case DOTSTAR_EXPR:
    binary:
      for (i = 0; i < 2; ++i)
	if (!potential_constant_expression_1 (TREE_OPERAND (t, i),
					      want_rval, flags))
	  return false;
      return true;

    case CILK_SYNC_STMT:
    case CILK_SPAWN_STMT:
    case ARRAY_NOTATION_REF:
      return false;

    case FMA_EXPR:
    case VEC_PERM_EXPR:
     for (i = 0; i < 3; ++i)
      if (!potential_constant_expression_1 (TREE_OPERAND (t, i),
					    true, flags))
	return false;
     return true;

    case COND_EXPR:
    case VEC_COND_EXPR:
      /* If the condition is a known constant, we know which of the legs we
	 care about; otherwise we only require that the condition and
	 either of the legs be potentially constant.  */
      tmp = TREE_OPERAND (t, 0);
      if (!potential_constant_expression_1 (tmp, rval, flags))
	return false;
      if (!processing_template_decl)
	tmp = cxx_eval_outermost_constant_expr (tmp, true);
      if (integer_zerop (tmp))
	return potential_constant_expression_1 (TREE_OPERAND (t, 2),
						want_rval, flags);
      else if (TREE_CODE (tmp) == INTEGER_CST)
	return potential_constant_expression_1 (TREE_OPERAND (t, 1),
						want_rval, flags);
      for (i = 1; i < 3; ++i)
	if (potential_constant_expression_1 (TREE_OPERAND (t, i),
					     want_rval, tf_none))
	  return true;
      if (flags & tf_error)
        error ("expression %qE is not a constant-expression", t);
      return false;

    case VEC_INIT_EXPR:
      if (VEC_INIT_EXPR_IS_CONSTEXPR (t))
	return true;
      if (flags & tf_error)
	{
	  error ("non-constant array initialization");
	  diagnose_non_constexpr_vec_init (t);
	}
      return false;

    default:
      if (objc_is_property_ref (t))
	return false;

      sorry ("unexpected AST of kind %s", get_tree_code_name (TREE_CODE (t)));
      gcc_unreachable();
      return false;
    }
}

/* The main entry point to the above.  */

bool
potential_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, false, tf_none);
}

/* As above, but require a constant rvalue.  */

bool
potential_rvalue_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, true, tf_none);
}

/* Like above, but complain about non-constant expressions.  */

bool
require_potential_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, false, tf_warning_or_error);
}

/* Cross product of the above.  */

bool
require_potential_rvalue_constant_expression (tree t)
{
  return potential_constant_expression_1 (t, true, tf_warning_or_error);
}

/* Insert the deduced return type for an auto function.  */

void
apply_deduced_return_type (tree fco, tree return_type)
{
  tree result;

  if (return_type == error_mark_node)
    return;

  if (LAMBDA_FUNCTION_P (fco))
    {
      tree lambda = CLASSTYPE_LAMBDA_EXPR (current_class_type);
      LAMBDA_EXPR_RETURN_TYPE (lambda) = return_type;
    }

  if (DECL_CONV_FN_P (fco))
    DECL_NAME (fco) = mangle_conv_op_name_for_type (return_type);

  TREE_TYPE (fco) = change_return_type (return_type, TREE_TYPE (fco));

  result = DECL_RESULT (fco);
  if (result == NULL_TREE)
    return;
  if (TREE_TYPE (result) == return_type)
    return;

  /* We already have a DECL_RESULT from start_preparsed_function.
     Now we need to redo the work it and allocate_struct_function
     did to reflect the new type.  */
  gcc_assert (current_function_decl == fco);
  result = build_decl (input_location, RESULT_DECL, NULL_TREE,
		       TYPE_MAIN_VARIANT (return_type));
  DECL_ARTIFICIAL (result) = 1;
  DECL_IGNORED_P (result) = 1;
  cp_apply_type_quals_to_decl (cp_type_quals (return_type),
                               result);

  DECL_RESULT (fco) = result;

  if (!processing_template_decl)
    {
      bool aggr = aggregate_value_p (result, fco);
#ifdef PCC_STATIC_STRUCT_RETURN
      cfun->returns_pcc_struct = aggr;
#endif
      cfun->returns_struct = aggr;
    }

}

/* DECL is a local variable or parameter from the surrounding scope of a
   lambda-expression.  Returns the decltype for a use of the capture field
   for DECL even if it hasn't been captured yet.  */

static tree
capture_decltype (tree decl)
{
  tree lam = CLASSTYPE_LAMBDA_EXPR (DECL_CONTEXT (current_function_decl));
  /* FIXME do lookup instead of list walk? */
  tree cap = value_member (decl, LAMBDA_EXPR_CAPTURE_LIST (lam));
  tree type;

  if (cap)
    type = TREE_TYPE (TREE_PURPOSE (cap));
  else
    switch (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lam))
      {
      case CPLD_NONE:
	error ("%qD is not captured", decl);
	return error_mark_node;

      case CPLD_COPY:
	type = TREE_TYPE (decl);
	if (TREE_CODE (type) == REFERENCE_TYPE
	    && TREE_CODE (TREE_TYPE (type)) != FUNCTION_TYPE)
	  type = TREE_TYPE (type);
	break;

      case CPLD_REFERENCE:
	type = TREE_TYPE (decl);
	if (TREE_CODE (type) != REFERENCE_TYPE)
	  type = build_reference_type (TREE_TYPE (decl));
	break;

      default:
	gcc_unreachable ();
      }

  if (TREE_CODE (type) != REFERENCE_TYPE)
    {
      if (!LAMBDA_EXPR_MUTABLE_P (lam))
	type = cp_build_qualified_type (type, (cp_type_quals (type)
					       |TYPE_QUAL_CONST));
      type = build_reference_type (type);
    }
  return type;
}

#include "gt-cp-semantics.h"
