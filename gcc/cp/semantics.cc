/* Perform the semantic phase of parsing, i.e., the process of
   building tree structure, checking semantic consistency, and
   building RTL.  These routines are used both during actual parsing
   and during the instantiation of template functions.

   Copyright (C) 1998-2025 Free Software Foundation, Inc.
   Written by Mark Mitchell (mmitchell@usa.net) based on code found
   formerly in parse.y and pt.cc.

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
#include "target.h"
#include "bitmap.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "cgraph.h"
#include "stmt.h"
#include "varasm.h"
#include "stor-layout.h"
#include "c-family/c-objc.h"
#include "tree-inline.h"
#include "intl.h"
#include "tree-iterator.h"
#include "omp-general.h"
#include "convert.h"
#include "stringpool.h"
#include "attribs.h"
#include "gomp-constants.h"
#include "predict.h"
#include "memmodel.h"

/* There routines provide a modular interface to perform many parsing
   operations.  They may therefore be used during actual parsing, or
   during template instantiation, which may be regarded as a
   degenerate form of parsing.  */

static tree maybe_convert_cond (tree);
static tree finalize_nrv_r (tree *, int *, void *);

/* Used for OpenMP non-static data member privatization.  */

static hash_map<tree, tree> *omp_private_member_map;
static vec<tree> omp_private_member_vec;
static bool omp_private_member_ignore_next;


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

struct GTY(()) deferred_access {
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
  vec<deferred_access_check, va_gc> *deferred_access_checks;

  /* The current mode of access checks.  */
  enum deferring_kind deferring_access_checks_kind;
};

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

/* Called from enforce_access.  A class has attempted (but failed) to access
   DECL.  It is already established that a baseclass of that class,
   PARENT_BINFO, has private access to DECL.  Examine certain special cases
   to find a decl that accurately describes the source of the problem.  If
   none of the special cases apply, simply return DECL as the source of the
   problem.  */

static tree
get_class_access_diagnostic_decl (tree parent_binfo, tree decl)
{
  /* When a class is denied access to a decl in a baseclass, most of the
     time it is because the decl itself was declared as private at the point
     of declaration.

     However, in C++, there are (at least) two situations in which a decl
     can be private even though it was not originally defined as such.
     These two situations only apply if a baseclass had private access to
     DECL (this function is only called if that is the case).  */

  /* We should first check whether the reason the parent had private access
     to DECL was simply because DECL was created and declared as private in
     the parent.  If it was, then DECL is definitively the source of the
     problem.  */
  if (SAME_BINFO_TYPE_P (context_for_name_lookup (decl),
			 BINFO_TYPE (parent_binfo)))
    return decl;

  /* 1.  If the "using" keyword is used to inherit DECL within the parent,
     this may cause DECL to be private, so we should return the using
     statement as the source of the problem.

     Scan the fields of PARENT_BINFO and see if there are any using decls.  If
     there are, see if they inherit DECL.  If they do, that's where DECL must
     have been declared private.  */

  for (tree parent_field = TYPE_FIELDS (BINFO_TYPE (parent_binfo));
       parent_field;
       parent_field = DECL_CHAIN (parent_field))
    /* Not necessary, but also check TREE_PRIVATE for the sake of
       eliminating obviously non-relevant using decls.  */
    if (TREE_CODE (parent_field) == USING_DECL
	&& TREE_PRIVATE (parent_field))
      {
	tree decl_stripped = strip_using_decl (parent_field);

	/* The using statement might be overloaded.  If so, we need to
	   check all of the overloads.  */
	for (ovl_iterator iter (decl_stripped); iter; ++iter)
	  /* If equal, the using statement inherits DECL, and so is the
	     source of the access failure, so return it.  */
	  if (*iter == decl)
	    return parent_field;
      }

  /* 2.  If DECL was privately inherited by the parent class, then DECL will
     be inaccessible, even though it may originally have been accessible to
     deriving classes.  In that case, the fault lies with the parent, since it
     used a private inheritance, so we return the parent as the source of the
     problem.

     Since this is the last check, we just assume it's true.  At worst, it
     will simply point to the class that failed to give access, which is
     technically true.  */
  return TYPE_NAME (BINFO_TYPE (parent_binfo));
}

/* If the current scope isn't allowed to access DECL along
   BASETYPE_PATH, give an error, or if we're parsing a function or class
   template, defer the access check to be performed at instantiation time.
   The most derived class in BASETYPE_PATH is the one used to qualify DECL.
   DIAG_DECL is the declaration to use in the error diagnostic.  */

static bool
enforce_access (tree basetype_path, tree decl, tree diag_decl,
		tsubst_flags_t complain, access_failure_info *afi = NULL)
{
  gcc_assert (TREE_CODE (basetype_path) == TREE_BINFO);

  if (flag_new_inheriting_ctors
      && DECL_INHERITED_CTOR (decl))
    {
      /* 7.3.3/18: The additional constructors are accessible if they would be
	 accessible when used to construct an object of the corresponding base
	 class.  */
      decl = strip_inheriting_ctors (decl);
      basetype_path = lookup_base (basetype_path, DECL_CONTEXT (decl),
				   ba_any, NULL, complain);
    }

  tree cs = current_scope ();
  if (in_template_context
      && (CLASS_TYPE_P (cs) || TREE_CODE (cs) == FUNCTION_DECL))
    if (tree template_info = get_template_info (cs))
      {
	/* When parsing a function or class template, we in general need to
	   defer access checks until template instantiation time, since a friend
	   declaration may grant access only to a particular specialization of
	   the template.  */

	if (accessible_p (basetype_path, decl, /*consider_local_p=*/true))
	  /* But if the member is deemed accessible at parse time, then we can
	     assume it'll be accessible at instantiation time.  */
	  return true;

	/* Access of a dependent decl should be rechecked after tsubst'ing
	   into the user of the decl, rather than explicitly deferring the
	   check here.  */
	gcc_assert (!uses_template_parms (decl));
	if (TREE_CODE (decl) == FIELD_DECL)
	  gcc_assert (!uses_template_parms (DECL_CONTEXT (decl)));

	/* Defer this access check until instantiation time.  */
	deferred_access_check access_check;
	access_check.binfo = basetype_path;
	access_check.decl = decl;
	access_check.diag_decl = diag_decl;
	access_check.loc = input_location;
	vec_safe_push (TI_DEFERRED_ACCESS_CHECKS (template_info), access_check);
	return true;
      }

  if (!accessible_p (basetype_path, decl, /*consider_local_p=*/true))
    {
      if (flag_new_inheriting_ctors)
	diag_decl = strip_inheriting_ctors (diag_decl);
      if (complain & tf_error)
	{
	  access_kind access_failure_reason = ak_none;

	  /* By default, using the decl as the source of the problem will
	     usually give correct results.  */
	  tree diag_location = diag_decl;

	  /* However, if a parent of BASETYPE_PATH had private access to decl,
	     then it actually might be the case that the source of the problem
	     is not DECL.  */
	  tree parent_binfo = get_parent_with_private_access (decl,
							      basetype_path);

	  /* So if a parent did have private access, then we need to do
	     special checks to obtain the best diagnostic location decl.  */
	  if (parent_binfo != NULL_TREE)
	    {
	      diag_location = get_class_access_diagnostic_decl (parent_binfo,
								diag_decl);

	      /* We also at this point know that the reason access failed was
		 because decl was private.  */
	      access_failure_reason = ak_private;
	    }

	  /* Finally, generate an error message.  */
	  complain_about_access (decl, diag_decl, diag_location, true,
				 access_failure_reason);
	}
      if (afi)
	afi->record_access_failure (basetype_path, decl, diag_decl);
      return false;
    }

  return true;
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
   Return value like perform_access_checks above.
   If non-NULL, report failures to AFI.  */

bool
perform_or_defer_access_check (tree binfo, tree decl, tree diag_decl,
			       tsubst_flags_t complain,
			       access_failure_info *afi)
{
  int i;
  deferred_access *ptr;
  deferred_access_check *chk;

  /* Exit if we are in a context that no access checking is performed.  */
  if (deferred_access_no_check)
    return true;

  gcc_assert (TREE_CODE (binfo) == TREE_BINFO);

  ptr = &deferred_access_stack->last ();

  /* If we are not supposed to defer access checks, just check now.  */
  if (ptr->deferring_access_checks_kind == dk_no_deferred)
    {
      bool ok = enforce_access (binfo, decl, diag_decl, complain, afi);
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
      if (STATEMENT_CODE_P (TREE_CODE (t)))
	STMT_IS_FULL_EXPR_P (t) = stmts_are_full_exprs_p ();
    }

  if (code == LABEL_EXPR || code == CASE_LABEL_EXPR)
    STATEMENT_LIST_HAS_LABEL (cur_stmt_list) = 1;

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
  tree r = build_stmt (DECL_SOURCE_LOCATION (decl), DECL_EXPR, decl);
  if (DECL_INITIAL (decl)
      || (DECL_SIZE (decl) && TREE_SIDE_EFFECTS (DECL_SIZE (decl))))
    r = maybe_cleanup_point_expr_void (r);
  add_stmt (r);
}

/* Set EXPR_LOCATION on one cleanup T to LOC.  */

static void
set_one_cleanup_loc (tree t, location_t loc)
{
  if (!t)
    return;
  if (TREE_CODE (t) != POSTCONDITION_STMT)
    protected_set_expr_location (t, loc);
  /* Avoid locus differences for C++ cdtor calls depending on whether
     cdtor_returns_this: a conversion to void is added to discard the return
     value, and this conversion ends up carrying the location, and when it
     gets discarded, the location is lost.  So hold it in the call as well.  */
  if (TREE_CODE (t) == NOP_EXPR
      && TREE_TYPE (t) == void_type_node
      && TREE_CODE (TREE_OPERAND (t, 0)) == CALL_EXPR)
    protected_set_expr_location (TREE_OPERAND (t, 0), loc);
}

/* Set EXPR_LOCATION of the cleanups of any CLEANUP_STMT in STMTS to LOC.  */

static void
set_cleanup_locs (tree stmts, location_t loc)
{
  if (TREE_CODE (stmts) == CLEANUP_STMT)
    {
      set_one_cleanup_loc (CLEANUP_EXPR (stmts), loc);
      set_cleanup_locs (CLEANUP_BODY (stmts), loc);
    }
  else if (TREE_CODE (stmts) == STATEMENT_LIST)
    for (tree stmt : tsi_range (stmts))
      set_cleanup_locs (stmt, loc);
}

/* True iff the innermost block scope is a try block.  */

static bool
at_try_scope ()
{
  cp_binding_level *b = current_binding_level;
  while (b && b->kind == sk_cleanup)
    b = b->level_chain;
  return b && b->kind == sk_try;
}

/* Finish a scope.  */

tree
do_poplevel (tree stmt_list)
{
  tree block = NULL;

  bool was_try = at_try_scope ();

  if (stmts_are_full_exprs_p ())
    block = poplevel (kept_level_p (), 1, 0);

  /* This needs to come after poplevel merges sk_cleanup statement_lists.  */
  maybe_splice_retval_cleanup (stmt_list, was_try);

  stmt_list = pop_stmt_list (stmt_list);

  /* input_location is the last token of the scope, usually a }.  */
  set_cleanup_locs (stmt_list, input_location);

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
   meant to apply to normal control flow transfer.  DECL is the VAR_DECL
   being cleaned up, if any, or null for temporaries or subobjects.  */

void
push_cleanup (tree decl, tree cleanup, bool eh_only)
{
  tree stmt = build_stmt (input_location, CLEANUP_STMT, NULL, cleanup, decl);
  CLEANUP_EH_ONLY (stmt) = eh_only;
  add_stmt (stmt);
  CLEANUP_BODY (stmt) = push_stmt_list ();
}

/* Simple infinite loop tracking for -Wreturn-type.  We keep a stack of all
   the current loops, represented by 'NULL_TREE' if we've seen a possible
   exit, and 'error_mark_node' if not.  This is currently used only to
   suppress the warning about a function with no return statements, and
   therefore we don't bother noting returns as possible exits.  We also
   don't bother with gotos.  */

static void
begin_maybe_infinite_loop (tree cond)
{
  /* Only track this while parsing a function, not during instantiation.  */
  if (!cfun || (DECL_TEMPLATE_INSTANTIATION (current_function_decl)
		&& !processing_template_decl))
    return;
  bool maybe_infinite = true;
  if (cond)
    {
      cond = fold_non_dependent_expr (cond);
      maybe_infinite = integer_nonzerop (cond);
    }
  vec_safe_push (cp_function_chain->infinite_loops,
		 maybe_infinite ? error_mark_node : NULL_TREE);

}

/* A break is a possible exit for the current loop.  */

void
break_maybe_infinite_loop (void)
{
  if (!cfun)
    return;
  cp_function_chain->infinite_loops->last() = NULL_TREE;
}

/* If we reach the end of the loop without seeing a possible exit, we have
   an infinite loop.  */

static void
end_maybe_infinite_loop (tree cond)
{
  if (!cfun || (DECL_TEMPLATE_INSTANTIATION (current_function_decl)
		&& !processing_template_decl))
    return;
  tree current = cp_function_chain->infinite_loops->pop();
  if (current != NULL_TREE)
    {
      cond = fold_non_dependent_expr (cond);
      if (integer_nonzerop (cond))
	current_function_infinite_loop = 1;
    }
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

/* If loop condition specifies a conditional with a declaration,
   such as
	    while (A x = 42) { }
	    for (; A x = 42;) { }
   move the *BODY_P statements as a BIND_EXPR into {FOR,WHILE}_COND_PREP
   and if there are any CLEANUP_STMT at the end, remember their count in
   {FOR,WHILE}_COND_CLEANUP.
   genericize_c_loop will then handle it appropriately.  In particular,
   the {FOR,WHILE}_COND, {FOR,WHILE}_BODY, if used continue label and
   FOR_EXPR will be appended into the {FOR,WHILE}_COND_PREP BIND_EXPR,
   but it can't be done too early because only the actual body should
   bind BREAK_STMT and CONTINUE_STMT to the inner loop.
   The statement list for *BODY will be empty if the conditional did
   not declare anything.  */

static void
adjust_loop_decl_cond (tree *body_p, tree *prep_p, tree *cleanup_p)
{
  if (!TREE_SIDE_EFFECTS (*body_p))
    return;

  gcc_assert (!processing_template_decl);
  *prep_p = *body_p;
  if (*prep_p != cur_stmt_list)
    {
      /* There can be just one CLEANUP_STMT, or there could be multiple
	 nested CLEANUP_STMTs, e.g. for structured bindings used as
	 condition.  */
      gcc_assert (stmt_list_stack->length () > 1);
      for (unsigned i = stmt_list_stack->length () - 2; ; --i)
	{
	  tree t = (*stmt_list_stack)[i];
	  tree_stmt_iterator last = tsi_last (t);
	  gcc_assert (tsi_one_before_end_p (last)
		      && TREE_CODE (tsi_stmt (last)) == CLEANUP_STMT
		      && (CLEANUP_BODY (tsi_stmt (last))
			  == (*stmt_list_stack)[i + 1])
		      && !CLEANUP_EH_ONLY (tsi_stmt (last)));
	  if (t == *prep_p)
	    {
	      *cleanup_p = build_int_cst (long_unsigned_type_node,
					  stmt_list_stack->length () - 1 - i);
	      break;
	    }
	  gcc_assert (i >= 1);
	}
    }
  current_binding_level->keep = true;
  tree_stmt_iterator iter = tsi_last (cur_stmt_list);
  /* Temporarily store in {FOR,WHILE}_BODY the last statement of
     the innnermost statement list or NULL if it has no statement.
     This is used in finish_loop_cond_prep to find out the splitting
     point and then {FOR,WHILE}_BODY will be changed to the actual
     body.  */
  if (tsi_end_p (iter))
    *body_p = NULL_TREE;
  else
    *body_p = tsi_stmt (iter);
}

/* Finalize {FOR,WHILE}_{BODY,COND_PREP} after the loop body.
   The above function initialized *BODY_P to the last statement
   in *PREP_P at that point.
   Call do_poplevel on *PREP_P and move everything after that
   former last statement into *BODY_P.  genericize_c_loop
   will later put those parts back together.
   CLEANUP is {FOR,WHILE}_COND_CLEANUP.  */

static void
finish_loop_cond_prep (tree *body_p, tree *prep_p, tree cleanup)
{
  *prep_p = do_poplevel (*prep_p);
  gcc_assert (TREE_CODE (*prep_p) == BIND_EXPR);
  if (BIND_EXPR_BODY (*prep_p) == *body_p)
    {
      gcc_assert (cleanup == NULL_TREE);
      *body_p = build_empty_stmt (input_location);
      return;
    }
  tree stmt_list = BIND_EXPR_BODY (*prep_p);
  gcc_assert (TREE_CODE (stmt_list) == STATEMENT_LIST);
  if (cleanup)
    {
      tree_stmt_iterator iter = tsi_last (stmt_list);
      gcc_assert (TREE_CODE (tsi_stmt (iter)) == CLEANUP_STMT);
      for (unsigned depth = tree_to_uhwi (cleanup); depth > 1; --depth)
	{
	  gcc_assert (TREE_CODE (CLEANUP_BODY (tsi_stmt (iter)))
		      == STATEMENT_LIST);
	  iter = tsi_last (CLEANUP_BODY (tsi_stmt (iter)));
	  gcc_assert (TREE_CODE (tsi_stmt (iter)) == CLEANUP_STMT);
	}
      if (*body_p == NULL_TREE)
	{
	  *body_p = CLEANUP_BODY (tsi_stmt (iter));
	  CLEANUP_BODY (tsi_stmt (iter)) = build_empty_stmt (input_location);
	  return;
	}
      stmt_list = CLEANUP_BODY (tsi_stmt (iter));
    }
  tree_stmt_iterator iter = tsi_start (stmt_list);
  while (tsi_stmt (iter) != *body_p)
    tsi_next (&iter);
  *body_p = tsi_split_stmt_list (input_location, iter);
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

  add_stmt (build_predict_expr (PRED_GOTO, NOT_TAKEN));
  return add_stmt (build_stmt (input_location, GOTO_EXPR, destination));
}

/* Returns true if T corresponds to an assignment operator expression.  */

static bool
is_assignment_op_expr_p (tree t)
{
  if (t == NULL_TREE)
    return false;

  if (TREE_CODE (t) == MODIFY_EXPR
      || (TREE_CODE (t) == MODOP_EXPR
	  && TREE_CODE (TREE_OPERAND (t, 1)) == NOP_EXPR))
    return true;

  tree call = extract_call_expr (t);
  if (call == NULL_TREE
      || call == error_mark_node
      || !CALL_EXPR_OPERATOR_SYNTAX (call))
    return false;

  tree fndecl = cp_get_callee_fndecl_nofold (call);
  return fndecl != NULL_TREE
    && DECL_ASSIGNMENT_OPERATOR_P (fndecl)
    && DECL_OVERLOADED_OPERATOR_IS (fndecl, NOP_EXPR);
}

/* Return true if TYPE is a class type that is convertible to
   and assignable from bool.  */

static GTY((deletable)) hash_map<tree, bool> *boolish_class_type_p_cache;

static bool
boolish_class_type_p (tree type)
{
  type = TYPE_MAIN_VARIANT (type);
  if (!CLASS_TYPE_P (type) || !COMPLETE_TYPE_P (type))
    return false;

  if (bool *r = hash_map_safe_get (boolish_class_type_p_cache, type))
    return *r;

  tree ops;
  bool has_bool_assignment = false;
  bool has_bool_conversion = false;

  ops = lookup_fnfields (type, assign_op_identifier, /*protect=*/0, tf_none);
  for (tree op : ovl_range (BASELINK_FUNCTIONS (ops)))
    {
      op = STRIP_TEMPLATE (op);
      if (TREE_CODE (op) != FUNCTION_DECL)
	continue;
      tree parm = DECL_CHAIN (DECL_ARGUMENTS (op));
      tree parm_type = non_reference (TREE_TYPE (parm));
      if (TREE_CODE (parm_type) == BOOLEAN_TYPE)
	{
	  has_bool_assignment = true;
	  break;
	}
    }

  if (has_bool_assignment)
    {
      ops = lookup_conversions (type);
      for (; ops; ops = TREE_CHAIN (ops))
	{
	  tree op = TREE_VALUE (ops);
	  if (!DECL_NONCONVERTING_P (op)
	      && TREE_CODE (DECL_CONV_FN_TYPE (op)) == BOOLEAN_TYPE)
	    {
	      has_bool_conversion = true;
	      break;
	    }
	}
    }

  bool boolish = has_bool_assignment && has_bool_conversion;
  hash_map_safe_put<true> (boolish_class_type_p_cache, type, boolish);
  return boolish;
}


/* Maybe warn about an unparenthesized 'a = b' (appearing in a
   boolean context where 'a == b' might have been intended).
   NESTED_P is true if T is the RHS of another assignment.  */

void
maybe_warn_unparenthesized_assignment (tree t, bool nested_p,
				       tsubst_flags_t complain)
{
  tree type = TREE_TYPE (t);
  t = STRIP_REFERENCE_REF (t);

  if ((complain & tf_warning)
      && warn_parentheses
      && is_assignment_op_expr_p (t)
      /* A parenthesized expression would've had this warning
	 suppressed by finish_parenthesized_expr.  */
      && !warning_suppressed_p (t, OPT_Wparentheses)
      /* In c = a = b, don't warn if a has type bool or bool-like class.  */
      && (!nested_p
	  || (TREE_CODE (type) != BOOLEAN_TYPE
	      && !boolish_class_type_p (type))))
    {
      warning_at (cp_expr_loc_or_input_loc (t), OPT_Wparentheses,
		  "suggest parentheses around assignment used as truth value");
      suppress_warning (t, OPT_Wparentheses);
    }
}

/* Helper class for saving/restoring ANNOTATE_EXPRs.  For a tree node t, users
   can construct one of these like so:

     annotate_saver s (&t);

   and t will be updated to have any annotations removed.  The user can then
   transform t, and later restore the ANNOTATE_EXPRs with:

     t = s.restore (t).

   The intent is to ensure that any ANNOTATE_EXPRs remain the outermost
   expressions following any operations on t.  */

class annotate_saver {
  /* The chain of saved annotations, if there were any.  Otherwise null.  */
  tree m_annotations;

  /* If M_ANNOTATIONS is non-null, then M_INNER points to TREE_OPERAND (A, 0)
     for the innermost annotation A.  */
  tree *m_inner;

public:
  annotate_saver (tree *);
  tree restore (tree);
};

/* If *COND is an ANNOTATE_EXPR, walk through the chain of annotations, and set
   *COND equal to the first non-ANNOTATE_EXPR (saving a pointer to the
   original chain of annotations for later use in restore).  */

annotate_saver::annotate_saver (tree *cond) : m_annotations (nullptr)
{
  tree *t = cond;
  while (TREE_CODE (*t) == ANNOTATE_EXPR)
    t = &TREE_OPERAND (*t, 0);

  if (t != cond)
    {
      m_annotations = *cond;
      *cond = *t;
      m_inner = t;
    }
}

/* If we didn't strip any annotations on construction, return NEW_INNER
   unmodified.  Otherwise, wrap the saved annotations around NEW_INNER (updating
   the types and flags of the annotations if needed) and return the resulting
   expression.  */

tree
annotate_saver::restore (tree new_inner)
{
  if (!m_annotations)
    return new_inner;

  /* If the type of the inner expression changed, we need to update the types
     of all the ANNOTATE_EXPRs.  We may need to update the flags too, but we
     assume they only change if the type of the inner expression changes.
     The flag update logic assumes that the other operands to the
     ANNOTATE_EXPRs are always INTEGER_CSTs.  */
  if (TREE_TYPE (new_inner) != TREE_TYPE (*m_inner))
    {
      const bool new_readonly
	= TREE_READONLY (new_inner) || CONSTANT_CLASS_P (new_inner);

      for (tree c = m_annotations; c != *m_inner; c = TREE_OPERAND (c, 0))
	{
	  gcc_checking_assert (TREE_CODE (c) == ANNOTATE_EXPR
			       && TREE_CODE (TREE_OPERAND (c, 1)) == INTEGER_CST
			       && TREE_CODE (TREE_OPERAND (c, 2)) == INTEGER_CST);
	  TREE_TYPE (c) = TREE_TYPE (new_inner);
	  TREE_SIDE_EFFECTS (c) = TREE_SIDE_EFFECTS (new_inner);
	  TREE_READONLY (c) = new_readonly;
	}
    }

  *m_inner = new_inner;
  return m_annotations;
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
  if (type_dependent_expression_p (cond))
    return cond;

  /* Strip any ANNOTATE_EXPRs from COND.  */
  annotate_saver annotations (&cond);

  /* For structured binding used in condition, the conversion needs to be
     evaluated before the individual variables are initialized in the
     std::tuple_{size,elemenet} case.  cp_finish_decomp saved the conversion
     result in a TARGET_EXPR, pick it up from there.  */
  if (DECL_DECOMPOSITION_P (cond)
      && DECL_DECOMP_IS_BASE (cond)
      && DECL_DECOMP_BASE (cond)
      && TREE_CODE (DECL_DECOMP_BASE (cond)) == TARGET_EXPR)
    cond = TARGET_EXPR_SLOT (DECL_DECOMP_BASE (cond));

  if (warn_sequence_point && !processing_template_decl)
    verify_sequence_points (cond);

  maybe_warn_unparenthesized_assignment (cond, /*nested_p=*/false,
					 tf_warning_or_error);

  /* Do the conversion.  */
  cond = convert_from_reference (cond);
  cond = condition_conversion (cond);

  /* Restore any ANNOTATE_EXPRs around COND.  */
  return annotations.restore (cond);
}

/* Finish an expression-statement, whose EXPRESSION is as indicated.  */

tree
finish_expr_stmt (tree expr)
{
  tree r = NULL_TREE;
  location_t loc = EXPR_LOCATION (expr);

  if (expr != NULL_TREE)
    {
      /* If we ran into a problem, make sure we complained.  */
      gcc_assert (expr != error_mark_node || seen_error ());

      if (!processing_template_decl)
	{
	  if (warn_sequence_point)
	    verify_sequence_points (expr);
	  expr = convert_to_void (expr, ICV_STATEMENT, tf_warning_or_error);
	}
      else if (!type_dependent_expression_p (expr))
	convert_to_void (expr, ICV_STATEMENT, tf_warning_or_error);

      if (check_for_bare_parameter_packs (expr))
        expr = error_mark_node;

      /* Simplification of inner statement expressions, compound exprs,
	 etc can result in us already having an EXPR_STMT.  */
      if (TREE_CODE (expr) != CLEANUP_POINT_EXPR)
	{
	  if (TREE_CODE (expr) != EXPR_STMT)
	    expr = build_stmt (loc, EXPR_STMT, expr);
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
  current_binding_level->this_entity = r;
  begin_cond (&IF_COND (r));
  return r;
}

/* Returns true if FN, a CALL_EXPR, is a call to
   std::is_constant_evaluated or __builtin_is_constant_evaluated.  */

static bool
is_std_constant_evaluated_p (tree fn)
{
  /* std::is_constant_evaluated takes no arguments.  */
  if (call_expr_nargs (fn) != 0)
    return false;

  tree fndecl = cp_get_callee_fndecl_nofold (fn);
  if (fndecl == NULL_TREE)
    return false;

  if (fndecl_built_in_p (fndecl, CP_BUILT_IN_IS_CONSTANT_EVALUATED,
			 BUILT_IN_FRONTEND))
    return true;

  if (!decl_in_std_namespace_p (fndecl))
    return false;

  tree name = DECL_NAME (fndecl);
  return name && id_equal (name, "is_constant_evaluated");
}

/* Callback function for maybe_warn_for_constant_evaluated that looks
   for calls to std::is_constant_evaluated in TP.  */

static tree
find_std_constant_evaluated_r (tree *tp, int *walk_subtrees, void *)
{
  tree t = *tp;

  if (TYPE_P (t) || TREE_CONSTANT (t))
    {
      *walk_subtrees = false;
      return NULL_TREE;
    }

  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      if (is_std_constant_evaluated_p (t))
	return t;
      break;
    case EXPR_STMT:
      /* Don't warn in statement expressions.  */
      *walk_subtrees = false;
      return NULL_TREE;
    default:
      break;
    }

  return NULL_TREE;
}

/* In certain contexts, std::is_constant_evaluated() is always true (for
   instance, in a consteval function or in a constexpr if), or always false
   (e.g., in a non-constexpr non-consteval function) so give the user a clue.  */

static void
maybe_warn_for_constant_evaluated (tree cond, bool constexpr_if,
				   bool trivial_infinite)
{
  if (!warn_tautological_compare)
    return;

  /* Suppress warning for std::is_constant_evaluated if the conditional
     comes from a macro.  */
  if (from_macro_expansion_at (EXPR_LOCATION (cond)))
    return;

  cond = cp_walk_tree_without_duplicates (&cond, find_std_constant_evaluated_r,
					  NULL);
  if (cond)
    {
      if (constexpr_if)
	warning_at (EXPR_LOCATION (cond), OPT_Wtautological_compare,
		    "%<std::is_constant_evaluated%> always evaluates to "
		    "true in %<if constexpr%>");
      else if (trivial_infinite)
	{
	  auto_diagnostic_group d;
	  if (warning_at (EXPR_LOCATION (cond), OPT_Wtautological_compare,
			  "%<std::is_constant_evaluated%> evaluates to "
			  "true when checking if trivially empty iteration "
			  "statement is trivial infinite loop")
	      && !maybe_constexpr_fn (current_function_decl))
	    inform (EXPR_LOCATION (cond),
		    "and evaluates to false when actually evaluating "
		    "the condition in non-%<constexpr%> function");
	}
      else if (!maybe_constexpr_fn (current_function_decl))
	warning_at (EXPR_LOCATION (cond), OPT_Wtautological_compare,
		    "%<std::is_constant_evaluated%> always evaluates to "
		    "false in a non-%<constexpr%> function");
      else if (DECL_IMMEDIATE_FUNCTION_P (current_function_decl))
	warning_at (EXPR_LOCATION (cond), OPT_Wtautological_compare,
		    "%<std::is_constant_evaluated%> always evaluates to "
		    "true in a %<consteval%> function");
    }
}

/* Process the COND of an if-statement, which may be given by
   IF_STMT.  */

tree
finish_if_stmt_cond (tree orig_cond, tree if_stmt)
{
  tree cond = maybe_convert_cond (orig_cond);
  maybe_warn_for_constant_evaluated (cond, IF_STMT_CONSTEXPR_P (if_stmt),
				     /*trivial_infinite=*/false);
  if (IF_STMT_CONSTEXPR_P (if_stmt)
      && !type_dependent_expression_p (cond)
      && require_constant_expression (cond)
      && !instantiation_dependent_expression_p (cond)
      /* Wait until instantiation time, since only then COND has been
	 converted to bool.  */
      && TYPE_MAIN_VARIANT (TREE_TYPE (cond)) == boolean_type_node)
    {
      cond = instantiate_non_dependent_expr (cond);
      cond = cxx_constant_value (cond);
    }
  else if (processing_template_decl)
    cond = orig_cond;
  finish_cond (&IF_COND (if_stmt), cond);
  add_stmt (if_stmt);
  THEN_CLAUSE (if_stmt) = push_stmt_list ();
  return cond;
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

/* Callback for cp_walk_tree to mark all {VAR,PARM}_DECLs in a tree as
   read.  */

static tree
maybe_mark_exp_read_r (tree *tp, int *, void *)
{
  tree t = *tp;
  if (VAR_P (t) || TREE_CODE (t) == PARM_DECL)
    mark_exp_read (t);
  return NULL_TREE;
}

/* Finish an if-statement.  */

void
finish_if_stmt (tree if_stmt)
{
  tree scope = IF_SCOPE (if_stmt);
  IF_SCOPE (if_stmt) = NULL;
  if (IF_STMT_CONSTEXPR_P (if_stmt))
    {
      /* Prevent various -Wunused warnings.  We might not instantiate
	 either of these branches, so we would not mark the variables
	 used in that branch as read.  */
      cp_walk_tree_without_duplicates (&THEN_CLAUSE (if_stmt),
				       maybe_mark_exp_read_r, NULL);
      cp_walk_tree_without_duplicates (&ELSE_CLAUSE (if_stmt),
				       maybe_mark_exp_read_r, NULL);
    }
  add_stmt (do_poplevel (scope));
}

/* Determine if iteration statement with *CONDP condition and
   loop BODY is trivially empty iteration statement or even
   trivial infinite loop.  In the latter case for -ffinite-loops
   add ANNOTATE_EXPR to mark the loop as maybe validly infinite.
   Also, emit -Wtautological-compare warning for std::is_constant_evaluated ()
   calls in the condition when needed.  */

static void
finish_loop_cond (tree *condp, tree body)
{
  if (TREE_CODE (*condp) == INTEGER_CST)
    return;
  bool trivially_empty = expr_first (body) == NULL_TREE;
  bool trivial_infinite = false;
  if (trivially_empty)
    {
      tree c = fold_non_dependent_expr (*condp, tf_none,
					/*manifestly_const_eval=*/true);
      trivial_infinite = c && integer_nonzerop (c);
    }
  if (warn_tautological_compare)
    {
      tree cond = *condp;
      while (TREE_CODE (cond) == ANNOTATE_EXPR)
	cond = TREE_OPERAND (cond, 0);
      if (trivial_infinite
	  && !DECL_IMMEDIATE_FUNCTION_P (current_function_decl))
	maybe_warn_for_constant_evaluated (cond, /*constexpr_if=*/false,
					   /*trivial_infinite=*/true);
      else if (!trivially_empty
	       || !processing_template_decl
	       || DECL_IMMEDIATE_FUNCTION_P (current_function_decl))
	maybe_warn_for_constant_evaluated (cond, /*constexpr_if=*/false,
					   /*trivial_infinite=*/false);
    }
  if (trivial_infinite && flag_finite_loops && !processing_template_decl)
    *condp = build3 (ANNOTATE_EXPR, TREE_TYPE (*condp), *condp,
		     build_int_cst (integer_type_node,
				    annot_expr_maybe_infinite_kind),
		     integer_zero_node);
}

/* Begin a while-statement.  Returns a newly created WHILE_STMT if
   appropriate.  */

tree
begin_while_stmt (void)
{
  tree r;
  r = build_stmt (input_location, WHILE_STMT, NULL_TREE, NULL_TREE, NULL_TREE,
		  NULL_TREE, NULL_TREE);
  add_stmt (r);
  WHILE_BODY (r) = do_pushlevel (sk_block);
  begin_cond (&WHILE_COND (r));
  return r;
}

/* Process the COND of a while-statement, which may be given by
   WHILE_STMT.  */

void
finish_while_stmt_cond (tree cond, tree while_stmt, bool ivdep,
			tree unroll, bool novector)
{
  cond = maybe_convert_cond (cond);
  finish_cond (&WHILE_COND (while_stmt), cond);
  begin_maybe_infinite_loop (cond);
  if (ivdep && cond != error_mark_node)
    WHILE_COND (while_stmt) = build3 (ANNOTATE_EXPR,
				      TREE_TYPE (WHILE_COND (while_stmt)),
				      WHILE_COND (while_stmt),
				      build_int_cst (integer_type_node,
						     annot_expr_ivdep_kind),
				      integer_zero_node);
  if (unroll && cond != error_mark_node)
    WHILE_COND (while_stmt) = build3 (ANNOTATE_EXPR,
				      TREE_TYPE (WHILE_COND (while_stmt)),
				      WHILE_COND (while_stmt),
				      build_int_cst (integer_type_node,
						     annot_expr_unroll_kind),
				      unroll);
  if (novector && cond != error_mark_node)
    WHILE_COND (while_stmt) = build3 (ANNOTATE_EXPR,
				      TREE_TYPE (WHILE_COND (while_stmt)),
				      WHILE_COND (while_stmt),
				      build_int_cst (integer_type_node,
						     annot_expr_no_vector_kind),
				      integer_zero_node);
  adjust_loop_decl_cond (&WHILE_BODY (while_stmt),
			 &WHILE_COND_PREP (while_stmt),
			 &WHILE_COND_CLEANUP (while_stmt));
}

/* Finish a while-statement, which may be given by WHILE_STMT.  */

void
finish_while_stmt (tree while_stmt)
{
  end_maybe_infinite_loop (boolean_true_node);
  if (WHILE_COND_PREP (while_stmt))
    finish_loop_cond_prep (&WHILE_BODY (while_stmt),
			   &WHILE_COND_PREP (while_stmt),
			   WHILE_COND_CLEANUP (while_stmt));
  else
    WHILE_BODY (while_stmt) = do_poplevel (WHILE_BODY (while_stmt));
  finish_loop_cond (&WHILE_COND (while_stmt), WHILE_BODY (while_stmt));
}

/* Begin a do-statement.  Returns a newly created DO_STMT if
   appropriate.  */

tree
begin_do_stmt (void)
{
  tree r = build_stmt (input_location, DO_STMT, NULL_TREE, NULL_TREE,
		       NULL_TREE);
  begin_maybe_infinite_loop (boolean_true_node);
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
finish_do_stmt (tree cond, tree do_stmt, bool ivdep, tree unroll,
		bool novector)
{
  cond = maybe_convert_cond (cond);
  end_maybe_infinite_loop (cond);
  /* Unlike other iteration statements, the condition may not contain
     a declaration, so we don't call finish_cond which checks for
     unexpanded parameter packs.  */
  if (check_for_bare_parameter_packs (cond))
    cond = error_mark_node;
  if (ivdep && cond != error_mark_node)
    cond = build3 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
		   build_int_cst (integer_type_node, annot_expr_ivdep_kind),
		   integer_zero_node);
  if (unroll && cond != error_mark_node)
    cond = build3 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
		   build_int_cst (integer_type_node, annot_expr_unroll_kind),
		   unroll);
  if (novector && cond != error_mark_node)
    cond = build3 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
		   build_int_cst (integer_type_node, annot_expr_no_vector_kind),
		   integer_zero_node);
  DO_COND (do_stmt) = cond;
  tree do_body = DO_BODY (do_stmt);
  if (CONVERT_EXPR_P (do_body)
      && integer_zerop (TREE_OPERAND (do_body, 0))
      && VOID_TYPE_P (TREE_TYPE (do_body)))
    do_body = NULL_TREE;
  finish_loop_cond (&DO_COND (do_stmt), do_body);
}

/* Finish a return-statement.  The EXPRESSION returned, if any, is as
   indicated.  */

tree
finish_return_stmt (tree expr)
{
  tree r;
  bool no_warning;
  bool dangling;

  expr = check_return_expr (expr, &no_warning, &dangling);

  if (error_operand_p (expr)
      || (flag_openmp && !check_omp_return ()))
    {
      /* Suppress -Wreturn-type for this function.  */
      if (warn_return_type)
	suppress_warning (current_function_decl, OPT_Wreturn_type);
      return error_mark_node;
    }

  if (!processing_template_decl)
    {
      if (warn_sequence_point)
	verify_sequence_points (expr);
    }

  r = build_stmt (input_location, RETURN_EXPR, expr);
  RETURN_EXPR_LOCAL_ADDR_P (r) = dangling;
  if (no_warning)
    suppress_warning (r, OPT_Wreturn_type);
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
  tree scope = do_pushlevel (sk_for);

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
		  NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE,
		  NULL_TREE, NULL_TREE);

  if (scope == NULL_TREE)
    {
      gcc_assert (!init);
      scope = begin_for_scope (&init);
    }

  FOR_INIT_STMT (r) = init;
  FOR_SCOPE (r) = scope;

  return r;
}

/* Finish the init-statement of a for-statement, which may be
   given by FOR_STMT.  */

void
finish_init_stmt (tree for_stmt)
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
finish_for_cond (tree cond, tree for_stmt, bool ivdep, tree unroll,
		 bool novector)
{
  cond = maybe_convert_cond (cond);
  finish_cond (&FOR_COND (for_stmt), cond);
  begin_maybe_infinite_loop (cond);
  if (ivdep && cond != error_mark_node)
    FOR_COND (for_stmt) = build3 (ANNOTATE_EXPR,
				  TREE_TYPE (FOR_COND (for_stmt)),
				  FOR_COND (for_stmt),
				  build_int_cst (integer_type_node,
						 annot_expr_ivdep_kind),
				  integer_zero_node);
  if (unroll && cond != error_mark_node)
    FOR_COND (for_stmt) = build3 (ANNOTATE_EXPR,
				  TREE_TYPE (FOR_COND (for_stmt)),
				  FOR_COND (for_stmt),
				  build_int_cst (integer_type_node,
						 annot_expr_unroll_kind),
				  unroll);
  if (novector && cond && cond != error_mark_node)
    FOR_COND (for_stmt) = build3 (ANNOTATE_EXPR,
				  TREE_TYPE (FOR_COND (for_stmt)),
				  FOR_COND (for_stmt),
				  build_int_cst (integer_type_node,
						 annot_expr_no_vector_kind),
				  integer_zero_node);
  adjust_loop_decl_cond (&FOR_BODY (for_stmt), &FOR_COND_PREP (for_stmt),
			 &FOR_COND_CLEANUP (for_stmt));
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
    convert_to_void (expr, ICV_THIRD_IN_FOR, tf_warning_or_error);
  expr = maybe_cleanup_point_expr_void (expr);
  if (check_for_bare_parameter_packs (expr))
    expr = error_mark_node;
  FOR_EXPR (for_stmt) = expr;
}

/* During parsing of the body, range for uses "__for_{range,begin,end} "
   decl names to make those unaccessible by code in the body.
   Find those decls and store into RANGE_FOR_DECL array, so that they
   can be changed later to ones with underscore instead of space, so that
   it can be inspected in the debugger.  */

void
find_range_for_decls (tree range_for_decl[3])
{
  gcc_assert (CPTI_FOR_BEGIN__IDENTIFIER == CPTI_FOR_RANGE__IDENTIFIER + 1
	      && CPTI_FOR_END__IDENTIFIER == CPTI_FOR_RANGE__IDENTIFIER + 2
	      && CPTI_FOR_RANGE_IDENTIFIER == CPTI_FOR_RANGE__IDENTIFIER + 3
	      && CPTI_FOR_BEGIN_IDENTIFIER == CPTI_FOR_BEGIN__IDENTIFIER + 3
	      && CPTI_FOR_END_IDENTIFIER == CPTI_FOR_END__IDENTIFIER + 3);
  for (int i = 0; i < 3; i++)
    {
      tree id = cp_global_trees[CPTI_FOR_RANGE__IDENTIFIER + i];
      if (IDENTIFIER_BINDING (id)
	  && IDENTIFIER_BINDING (id)->scope == current_binding_level)
	{
	  range_for_decl[i] = IDENTIFIER_BINDING (id)->value;
	  gcc_assert (VAR_P (range_for_decl[i])
		      && DECL_ARTIFICIAL (range_for_decl[i]));
	}
    }
}

/* Finish the body of a for-statement, which may be given by
   FOR_STMT.  The increment-EXPR for the loop must be
   provided.
   It can also finish RANGE_FOR_STMT. */

void
finish_for_stmt (tree for_stmt)
{
  end_maybe_infinite_loop (boolean_true_node);

  if (TREE_CODE (for_stmt) == RANGE_FOR_STMT)
    RANGE_FOR_BODY (for_stmt) = do_poplevel (RANGE_FOR_BODY (for_stmt));
  else
    {
      if (FOR_COND_PREP (for_stmt))
	finish_loop_cond_prep (&FOR_BODY (for_stmt),
			       &FOR_COND_PREP (for_stmt),
			       FOR_COND_CLEANUP (for_stmt));
      else
	FOR_BODY (for_stmt) = do_poplevel (FOR_BODY (for_stmt));
      if (FOR_COND (for_stmt))
	finish_loop_cond (&FOR_COND (for_stmt),
			  FOR_EXPR (for_stmt) ? integer_one_node
					      : FOR_BODY (for_stmt));
    }

  /* Pop the scope for the body of the loop.  */
  tree *scope_ptr = (TREE_CODE (for_stmt) == RANGE_FOR_STMT
		     ? &RANGE_FOR_SCOPE (for_stmt)
		     : &FOR_SCOPE (for_stmt));
  tree scope = *scope_ptr;
  *scope_ptr = NULL;

  /* During parsing of the body, range for uses "__for_{range,begin,end} "
     decl names to make those unaccessible by code in the body.
     Change it to ones with underscore instead of space, so that it can
     be inspected in the debugger.  */
  tree range_for_decl[3] = { NULL_TREE, NULL_TREE, NULL_TREE };
  find_range_for_decls (range_for_decl);

  add_stmt (do_poplevel (scope));

  /* If we're being called from build_vec_init, don't mess with the names of
     the variables for an enclosing range-for.  */
  if (!stmts_are_full_exprs_p ())
    return;

  for (int i = 0; i < 3; i++)
    if (range_for_decl[i])
      DECL_NAME (range_for_decl[i])
	= cp_global_trees[CPTI_FOR_RANGE_IDENTIFIER + i];
}

/* Begin a range-for-statement.  Returns a new RANGE_FOR_STMT.
   SCOPE and INIT should be the return of begin_for_scope,
   or both NULL_TREE  .
   To finish it call finish_for_stmt(). */

tree
begin_range_for_stmt (tree scope, tree init)
{
  begin_maybe_infinite_loop (boolean_false_node);

  tree r = build_stmt (input_location, RANGE_FOR_STMT, NULL_TREE, NULL_TREE,
		       NULL_TREE, NULL_TREE, NULL_TREE, NULL_TREE);

  if (scope == NULL_TREE)
    {
      gcc_assert (!init);
      scope = begin_for_scope (&init);
    }

  /* Since C++20, RANGE_FOR_STMTs can use the init tree, so save it.  */
  RANGE_FOR_INIT_STMT (r) = init;
  RANGE_FOR_SCOPE (r) = scope;

  return r;
}

/* Finish the head of a range-based for statement, which may
   be given by RANGE_FOR_STMT.  DECL must be the declaration
   and EXPR must be the loop expression. */

void
finish_range_for_decl (tree range_for_stmt, tree decl, tree expr)
{
  if (processing_template_decl)
    RANGE_FOR_INIT_STMT (range_for_stmt)
      = pop_stmt_list (RANGE_FOR_INIT_STMT (range_for_stmt));
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
    return void_node;
  note_break_stmt ();
  return add_stmt (build_stmt (input_location, BREAK_STMT, NULL_TREE));
}

/* Finish a continue-statement.  */

tree
finish_continue_stmt (void)
{
  return add_stmt (build_stmt (input_location, CONTINUE_STMT, NULL_TREE));
}

/* Begin a switch-statement.  Returns a new SWITCH_STMT if
   appropriate.  */

tree
begin_switch_stmt (void)
{
  tree r, scope;

  scope = do_pushlevel (sk_cond);
  r = build_stmt (input_location, SWITCH_STMT, NULL_TREE, NULL_TREE, NULL_TREE,
		  scope, NULL_TREE);

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
      tree orig_cond = cond;
      /* For structured binding used in condition, the conversion needs to be
	 evaluated before the individual variables are initialized in the
	 std::tuple_{size,elemenet} case.  cp_finish_decomp saved the
	 conversion result in a TARGET_EXPR, pick it up from there.  */
      if (DECL_DECOMPOSITION_P (cond)
	  && DECL_DECOMP_IS_BASE (cond)
	  && DECL_DECOMP_BASE (cond)
	  && TREE_CODE (DECL_DECOMP_BASE (cond)) == TARGET_EXPR)
	cond = TARGET_EXPR_SLOT (DECL_DECOMP_BASE (cond));
      cond = build_expr_type_conversion (WANT_INT | WANT_ENUM, cond, true);
      if (cond == NULL_TREE)
	{
	  error_at (cp_expr_loc_or_input_loc (orig_cond),
		    "switch quantity not an integer");
	  cond = error_mark_node;
	}
      /* We want unlowered type here to handle enum bit-fields.  */
      orig_type = unlowered_expr_type (cond);
      if (TREE_CODE (orig_type) != ENUMERAL_TYPE)
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
  current_binding_level->artificial = 1;
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
    {
      type = expand_start_catch_block (decl);
      if (warn_catch_value
	  && type != NULL_TREE
	  && type != error_mark_node
	  && !TYPE_REF_P (TREE_TYPE (decl)))
	{
	  tree orig_type = TREE_TYPE (decl);
	  if (CLASS_TYPE_P (orig_type))
	    {
	      if (TYPE_POLYMORPHIC_P (orig_type))
		warning_at (DECL_SOURCE_LOCATION (decl),
			    OPT_Wcatch_value_,
			    "catching polymorphic type %q#T by value",
			    orig_type);
	      else if (warn_catch_value > 1)
		warning_at (DECL_SOURCE_LOCATION (decl),
			    OPT_Wcatch_value_,
			    "catching type %q#T by value", orig_type);
	    }
	  else if (warn_catch_value > 2)
	    warning_at (DECL_SOURCE_LOCATION (decl),
			OPT_Wcatch_value_,
			"catching non-reference type %q#T", orig_type);
	}
    }
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
    {
      scope_kind sk = sk_block;
      if (flags & BCS_TRY_BLOCK)
	sk = sk_try;
      else if (flags & BCS_TRANSACTION)
	sk = sk_transaction;
      else if (flags & BCS_STMT_EXPR)
	sk = sk_stmt_expr;
      r = do_pushlevel (sk);
    }

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

/* Finish an asm string literal, which can be a string literal
   or parenthesized constant expression.  Extract the string literal
   from the latter.  */

tree
finish_asm_string_expression (location_t loc, tree string)
{
  if (string == error_mark_node
      || TREE_CODE (string) == STRING_CST
      || processing_template_decl)
    return string;
  string = cxx_constant_value (string, tf_error);
  if (TREE_CODE (string) == STRING_CST)
    string = build1_loc (loc, PAREN_EXPR, TREE_TYPE (string),
			 string);
  cexpr_str cstr (string);
  if (!cstr.type_check (loc))
    return error_mark_node;
  if (!cstr.extract (loc, string))
    string = error_mark_node;
  return string;
}

/* Finish an asm-statement, whose components are a STRING, some
   OUTPUT_OPERANDS, some INPUT_OPERANDS, some CLOBBERS and some
   LABELS.  Also note whether the asm-statement should be
   considered volatile, and whether it is asm inline.  TOPLEV_P
   is true if finishing namespace scope extended asm.  */

tree
finish_asm_stmt (location_t loc, int volatile_p, tree string,
		 tree output_operands, tree input_operands, tree clobbers,
		 tree labels, bool inline_p, bool toplev_p)
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

      string = finish_asm_string_expression (cp_expr_loc_or_loc (string, loc),
					     string);
      if (string == error_mark_node)
	return error_mark_node;
      for (int i = 0; i < 2; ++i)
	for (t = i ? input_operands : output_operands; t; t = TREE_CHAIN (t))
	  {
	    tree s = TREE_VALUE (TREE_PURPOSE (t));
	    s = finish_asm_string_expression (cp_expr_loc_or_loc (s, loc), s);
	    if (s == error_mark_node)
	      return error_mark_node;
	    TREE_VALUE (TREE_PURPOSE (t)) = s;
	  }
      for (t = clobbers; t; t = TREE_CHAIN (t))
	{
	  tree s = TREE_VALUE (t);
	  s = finish_asm_string_expression (cp_expr_loc_or_loc (s, loc), s);
	  TREE_VALUE (t) = s;
	}

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
		  || FUNC_OR_METHOD_TYPE_P (TREE_TYPE (operand))
		  /* If it's an aggregate and any field is const, then it is
		     effectively const.  */
		  || (CLASS_TYPE_P (TREE_TYPE (operand))
		      && C_TYPE_FIELDS_READONLY (TREE_TYPE (operand)))))
	    cxx_readonly_error (loc, operand, lv_asm);

	  tree *op = &operand;
	  while (TREE_CODE (*op) == COMPOUND_EXPR)
	    op = &TREE_OPERAND (*op, 1);
	  switch (TREE_CODE (*op))
	    {
	    case PREINCREMENT_EXPR:
	    case PREDECREMENT_EXPR:
	    case MODIFY_EXPR:
	      *op = genericize_compound_lvalue (*op);
	      op = &TREE_OPERAND (*op, 1);
	      break;
	    default:
	      break;
	    }

	  constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
	  oconstraints[i] = constraint;

	  if (parse_output_constraint (&constraint, i, ninputs, noutputs,
				       &allows_mem, &allows_reg, &is_inout))
	    {
	      /* If the operand is going to end up in memory,
		 mark it addressable.  */
	      if (!allows_reg && !cxx_mark_addressable (*op))
		operand = error_mark_node;
	      if (allows_reg && toplev_p)
		{
		  error_at (loc, "constraint allows registers outside of "
				 "a function");
		  operand = error_mark_node;
		}
	    }
	  else
	    operand = error_mark_node;

	  if (toplev_p && operand != error_mark_node)
	    {
	      if (TREE_SIDE_EFFECTS (operand))
		{
		  error_at (loc, "side-effects in output operand outside "
				 "of a function");
		  operand = error_mark_node;
		}
	      else
		{
		  tree addr
		    = cp_build_addr_expr (operand, tf_warning_or_error);
		  if (addr == error_mark_node)
		    operand = error_mark_node;
		  else
		    {
		      addr = maybe_constant_value (addr);
		      if (!initializer_constant_valid_p (addr,
							 TREE_TYPE (addr)))
			{
			  error_at (loc, "output operand outside of a "
					 "function is not constant");
			  operand = error_mark_node;
			}
		      else
			operand = build_fold_indirect_ref (addr);
		    }
		}
	    }
	  else if (operand != error_mark_node && strstr (constraint, "-"))
	    {
	      error_at (loc, "%<-%> modifier used inside of a function");
	      operand = error_mark_node;
	    }

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
	      error_at (loc,
			"type of %<asm%> operand %qE could not be determined",
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

		  tree *op = &operand;
		  while (TREE_CODE (*op) == COMPOUND_EXPR)
		    op = &TREE_OPERAND (*op, 1);
		  switch (TREE_CODE (*op))
		    {
		    case PREINCREMENT_EXPR:
		    case PREDECREMENT_EXPR:
		    case MODIFY_EXPR:
		      *op = genericize_compound_lvalue (*op);
		      op = &TREE_OPERAND (*op, 1);
		      break;
		    default:
		      break;
		    }

		  if (!cxx_mark_addressable (*op))
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
	      if (allows_reg && toplev_p)
		{
		  error_at (loc, "constraint allows registers outside of "
				 "a function");
		  operand = error_mark_node;
		}
	      if (constraint[0] == ':' && operand != error_mark_node)
		{
		  tree t = operand;
		  STRIP_NOPS (t);
		  if (TREE_CODE (t) != ADDR_EXPR
		      || !(TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL
			   || (VAR_P (TREE_OPERAND (t, 0))
			       && is_global_var (TREE_OPERAND (t, 0)))))
		    {
		      error_at (loc, "%<:%> constraint operand is not address "
				"of a function or non-automatic variable");
		      operand = error_mark_node;
		    }
		}
	    }
	  else
	    operand = error_mark_node;

	  if (toplev_p && operand != error_mark_node)
	    {
	      if (TREE_SIDE_EFFECTS (operand))
		{
		  error_at (loc, "side-effects in input operand outside "
				 "of a function");
		  operand = error_mark_node;
		}
	      else if (allows_mem && lvalue_or_else (operand, lv_asm, tf_none))
		{
		  tree addr = cp_build_addr_expr (operand, tf_warning_or_error);
		  if (addr == error_mark_node)
		    operand = error_mark_node;
		  else
		    {
		      addr = maybe_constant_value (addr);
		      if (!initializer_constant_valid_p (addr,
							 TREE_TYPE (addr)))
			{
			  error_at (loc, "input operand outside of a "
					 "function is not constant");
			  operand = error_mark_node;
			}
		      else
			operand = build_fold_indirect_ref (addr);
		    }
		}
	      else
		{
		  operand = maybe_constant_value (operand);
		  if (!initializer_constant_valid_p (operand,
						     TREE_TYPE (operand)))
		    {
		      error_at (loc, "input operand outside of a "
				     "function is not constant");
		      operand = error_mark_node;
		    }
		}
	    }
	  else if (operand != error_mark_node && strstr (constraint, "-"))
	    {
	      error_at (loc, "%<-%> modifier used inside of a function");
	      operand = error_mark_node;
	    }

	  TREE_VALUE (t) = operand;
	}
    }

  r = build_stmt (loc, ASM_EXPR, string,
		  output_operands, input_operands,
		  clobbers, labels);
  ASM_VOLATILE_P (r) = volatile_p || noutputs == 0;
  ASM_INLINE_P (r) = inline_p;
  if (toplev_p)
    {
      symtab->finalize_toplevel_asm (r);
      return r;
    }
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
      error ("%<__label__%> declarations are only allowed in function scopes");
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
   right result.  If EVEN_UNEVAL, do this even in unevaluated context.  */

tree
force_paren_expr (tree expr, bool even_uneval /* = false */)
{
  /* This is only needed for decltype(auto) in C++14.  */
  if (cxx_dialect < cxx14)
    return expr;

  /* If we're in unevaluated context, we can't be deducing a
     return/initializer type, so we don't need to mess with this.  */
  if (cp_unevaluated_operand && !even_uneval)
    return expr;

  if (TREE_CODE (expr) == COMPONENT_REF
      || TREE_CODE (expr) == SCOPE_REF
      || REFERENCE_REF_P (expr))
    REF_PARENTHESIZED_P (expr) = true;
  else if (DECL_P (tree_strip_any_location_wrapper (expr)))
    {
      location_t loc = cp_expr_location (expr);
      const tree_code code = processing_template_decl ? PAREN_EXPR
						      : VIEW_CONVERT_EXPR;
      expr = build1_loc (loc, code, TREE_TYPE (expr), expr);
      REF_PARENTHESIZED_P (expr) = true;
    }
  return expr;
}

/* If T is an id-expression obfuscated by force_paren_expr, undo the
   obfuscation and return the underlying id-expression.  Otherwise
   return T.  */

tree
maybe_undo_parenthesized_ref (tree t)
{
  if (cxx_dialect < cxx14)
    return t;

  if ((TREE_CODE (t) == PAREN_EXPR || TREE_CODE (t) == VIEW_CONVERT_EXPR)
      && REF_PARENTHESIZED_P (t))
    t = TREE_OPERAND (t, 0);

  return t;
}

/* Finish a parenthesized expression EXPR.  */

cp_expr
finish_parenthesized_expr (cp_expr expr)
{
  if (EXPR_P (expr))
    {
      /* This inhibits warnings in maybe_warn_unparenthesized_assignment
	 and c_common_truthvalue_conversion.  */
      suppress_warning (STRIP_REFERENCE_REF (*expr), OPT_Wparentheses);
      /* And maybe_warn_sizeof_array_div.  */
      suppress_warning (STRIP_REFERENCE_REF (*expr), OPT_Wsizeof_array_div);
    }

  if (TREE_CODE (expr) == OFFSET_REF
      || TREE_CODE (expr) == SCOPE_REF)
    /* [expr.unary.op]/3 The qualified id of a pointer-to-member must not be
       enclosed in parentheses.  */
    PTRMEM_OK_P (expr) = 0;

  tree stripped_expr = tree_strip_any_location_wrapper (expr);
  if (TREE_CODE (stripped_expr) == STRING_CST)
    PAREN_STRING_LITERAL_P (stripped_expr) = 1;
  else if (TREE_CODE (stripped_expr) == PACK_INDEX_EXPR)
    PACK_INDEX_PARENTHESIZED_P (stripped_expr) = true;

  expr = cp_expr (force_paren_expr (expr), expr.get_location ());

  return expr;
}

/* Finish a reference to a non-static data member (DECL) that is not
   preceded by `.' or `->'.  */

tree
finish_non_static_data_member (tree decl, tree object, tree qualifying_scope,
			       tsubst_flags_t complain /* = tf_warning_or_error */)
{
  gcc_assert (TREE_CODE (decl) == FIELD_DECL);
  bool try_omp_private = !object && omp_private_member_map;
  tree ret;

  if (!object)
    {
      tree scope = qualifying_scope;
      if (scope == NULL_TREE)
	{
	  scope = context_for_name_lookup (decl);
	  if (!TYPE_P (scope))
	    {
	      /* Can happen during error recovery (c++/85014).  */
	      gcc_assert (seen_error ());
	      return error_mark_node;
	    }
	}
      object = maybe_dummy_object (scope, NULL);
    }

  object = maybe_resolve_dummy (object, true);
  if (object == error_mark_node)
    return error_mark_node;

  /* DR 613/850: Can use non-static data members without an associated
     object in sizeof/decltype/alignof.  */
  if (is_dummy_object (object)
      && !cp_unevaluated_operand
      && (!processing_template_decl || !current_class_ref))
    {
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  if (current_function_decl
	      && DECL_STATIC_FUNCTION_P (current_function_decl))
	    error ("invalid use of member %qD in static member function", decl);
	  else if (current_function_decl
		   && processing_contract_condition
		   && DECL_CONSTRUCTOR_P (current_function_decl))
	    error ("invalid use of member %qD in constructor %<pre%> contract", decl);
	  else if (current_function_decl
		   && processing_contract_condition
		   && DECL_DESTRUCTOR_P (current_function_decl))
	    error ("invalid use of member %qD in destructor %<post%> contract", decl);
	  else
	    error ("invalid use of non-static data member %qD", decl);
	  inform (DECL_SOURCE_LOCATION (decl), "declared here");
	}

      return error_mark_node;
    }

  if (current_class_ptr)
    TREE_USED (current_class_ptr) = 1;
  if (processing_template_decl)
    {
      tree type = TREE_TYPE (decl);

      if (TYPE_REF_P (type))
	/* Quals on the object don't matter.  */;
      else if (PACK_EXPANSION_P (type))
	/* Don't bother trying to represent this.  */
	type = NULL_TREE;
      else if (WILDCARD_TYPE_P (TREE_TYPE (object)))
	/* We don't know what the eventual quals will be, so punt until
	   instantiation time.

	   This can happen when called from build_capture_proxy for an explicit
	   object lambda.  It's a bit marginal to call this function in that
	   case, since this function is for references to members of 'this',
	   but the deduced type is required to be derived from the closure
	   type, so it works.  */
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

      if (qualifying_scope)
	/* Wrap this in a SCOPE_REF for now.  */
	ret = build_qualified_name (type, qualifying_scope, decl,
				    /*template_p=*/false);
      else
	ret = (convert_from_reference
	       (build_min (COMPONENT_REF, type, object, decl, NULL_TREE)));
    }
  /* If PROCESSING_TEMPLATE_DECL is nonzero here, then
     QUALIFYING_SCOPE is also non-null.  */
  else
    {
      tree access_type = TREE_TYPE (object);

      if (!perform_or_defer_access_check (TYPE_BINFO (access_type), decl,
					  decl, complain))
	return error_mark_node;

      /* If the data member was named `C::M', convert `*this' to `C'
	 first.  */
      if (qualifying_scope)
	{
	  tree binfo = NULL_TREE;
	  object = build_scoped_ref (object, qualifying_scope,
				     &binfo);
	}

      ret = build_class_member_access_expr (object, decl,
					    /*access_path=*/NULL_TREE,
					    /*preserve_reference=*/false,
					    complain);
    }
  if (try_omp_private)
    {
      tree *v = omp_private_member_map->get (decl);
      if (v)
	ret = convert_from_reference (*v);
    }
  return ret;
}

/* DECL was the declaration to which a qualified-id resolved.  Issue
   an error message if it is not accessible.  If OBJECT_TYPE is
   non-NULL, we have just seen `x->' or `x.' and OBJECT_TYPE is the
   type of `*x', or `x', respectively.  If the DECL was named as
   `A::B' then NESTED_NAME_SPECIFIER is `A'.  Return value is like
   perform_access_checks above.  */

bool
check_accessibility_of_qualified_id (tree decl,
				     tree object_type,
				     tree nested_name_specifier,
				     tsubst_flags_t complain)
{
  /* If we're not checking, return immediately.  */
  if (deferred_access_no_check)
    return true;

  /* Determine the SCOPE of DECL.  */
  tree scope = context_for_name_lookup (decl);
  /* If the SCOPE is not a type, then DECL is not a member.  */
  if (!TYPE_P (scope)
      /* If SCOPE is dependent then we can't perform this access check now,
	 and since we'll perform this access check again after substitution
	 there's no need to explicitly defer it.  */
      || dependent_type_p (scope))
    return true;

  tree qualifying_type = NULL_TREE;
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
    {
      /* If we are processing a `->' or `.' expression, use the type of the
	 left-hand side.  */
      if (tree open = currently_open_class (object_type))
	qualifying_type = open;
      else
	qualifying_type = object_type;
    }
  else if (nested_name_specifier)
    {
      /* If the reference is to a non-static member of the
	 current class, treat it as if it were referenced through
	 `this'.  */
      if (DECL_NONSTATIC_MEMBER_P (decl)
	  && current_class_ptr)
	if (tree current = current_nonlambda_class_type ())
	  {
	    if (dependent_type_p (current))
	    /* In general we can't know whether this access goes through
	       `this' until instantiation time.  Punt now, or else we might
	       create a deferred access check that's not relative to `this'
	       when it ought to be.  We'll check this access again after
	       substitution, e.g. from tsubst_qualified_id.  */
	      return true;

	    if (DERIVED_FROM_P (scope, current))
	      qualifying_type = current;
	  }
      /* Otherwise, use the type indicated by the
	 nested-name-specifier.  */
      if (!qualifying_type)
	qualifying_type = nested_name_specifier;
    }
  else
    /* Otherwise, the name must be from the current class or one of
       its bases.  */
    qualifying_type = currently_open_derived_class (scope);

  if (qualifying_type
      /* It is possible for qualifying type to be a TEMPLATE_TYPE_PARM
	 or similar in a default argument value.  */
      && CLASS_TYPE_P (qualifying_type))
    return perform_or_defer_access_check (TYPE_BINFO (qualifying_type), decl,
					  decl, complain);

  return true;
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

  if (DECL_P (expr)
      /* Functions are marked after overload resolution; avoid redundant
	 warnings.  */
      && TREE_CODE (expr) != FUNCTION_DECL
      && !mark_used (expr, complain))
    return error_mark_node;

  if (template_p)
    {
      if (TREE_CODE (expr) == UNBOUND_CLASS_TEMPLATE)
	{
	  /* cp_parser_lookup_name thought we were looking for a type,
	     but we're actually looking for a declaration.  */
	  qualifying_class = TYPE_CONTEXT (expr);
	  expr = TYPE_IDENTIFIER (expr);
	}
      else
	check_template_keyword (expr);
    }

  /* If EXPR occurs as the operand of '&', use special handling that
     permits a pointer-to-member.  */
  if (address_p && done
      && TREE_CODE (qualifying_class) != ENUMERAL_TYPE)
    {
      if (TREE_CODE (expr) == SCOPE_REF)
	expr = TREE_OPERAND (expr, 1);
      expr = build_offset_ref (qualifying_class, expr,
			       /*address_p=*/true, complain);
      return expr;
    }

  /* No need to check access within an enum.  */
  if (TREE_CODE (qualifying_class) == ENUMERAL_TYPE
      && TREE_CODE (expr) != IDENTIFIER_NODE)
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
					    qualifying_class, complain);
      pop_deferring_access_checks ();
    }
  else if (BASELINK_P (expr))
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
  else if (!template_p
	   && TREE_CODE (expr) == TEMPLATE_DECL
	   && !DECL_FUNCTION_TEMPLATE_P (expr))
    {
      if (complain & tf_error)
	error ("%qE missing template arguments", expr);
      return error_mark_node;
    }
  else
    {
      /* In a template, return a SCOPE_REF for most qualified-ids
	 so that we can check access at instantiation time.  But if
	 we're looking at a member of the current instantiation, we
	 know we have access and building up the SCOPE_REF confuses
	 non-type template argument handling.  */
      if (processing_template_decl
	  && (!currently_open_class (qualifying_class)
	      || TREE_CODE (expr) == IDENTIFIER_NODE
	      || TREE_CODE (expr) == TEMPLATE_ID_EXPR
	      || TREE_CODE (expr) == BIT_NOT_EXPR))
	expr = build_qualified_name (TREE_TYPE (expr),
				     qualifying_class, expr,
				     template_p);
      else if (tree wrap = maybe_get_tls_wrapper_call (expr))
	expr = wrap;

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

      if (type && type_unknown_p (type))
	{
	  error ("a statement expression is an insufficient context"
		 " for overload resolution");
	  TREE_TYPE (stmt_expr) = error_mark_node;
	  return error_mark_node;
	}
      else if (processing_template_decl)
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

	  /* This TARGET_EXPR will initialize the outer one added by
	     finish_stmt_expr.  */
	  set_target_expr_eliding (expr);

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

  if (expr_stmt == void_node)
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

/* Perform Koenig lookup.  FN_EXPR is the postfix-expression representing
   the function (or functions) to call; ARGS are the arguments to the
   call.  Returns the functions to be considered by overload resolution.  */

cp_expr
perform_koenig_lookup (cp_expr fn_expr, vec<tree, va_gc> *args,
		       tsubst_flags_t complain)
{
  tree identifier = NULL_TREE;
  tree functions = NULL_TREE;
  tree tmpl_args = NULL_TREE;
  bool template_id = false;
  location_t loc = fn_expr.get_location ();
  tree fn = fn_expr.get_value ();

  STRIP_ANY_LOCATION_WRAPPER (fn);

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
  else
    {
      functions = fn;
      identifier = OVL_NAME (functions);
    }

  /* A call to a namespace-scope function using an unqualified name.

     Do Koenig lookup -- unless any of the arguments are
     type-dependent.  */
  if (!any_type_dependent_arguments_p (args)
      && !any_dependent_template_arguments_p (tmpl_args))
    {
      fn = lookup_arg_dependent (identifier, functions, args);
      if (!fn)
	{
	  /* The unqualified name could not be resolved.  */
	  if (complain & tf_error)
	    fn = unqualified_fn_lookup_error (cp_expr (identifier, loc));
	  else
	    fn = identifier;
	}
    }

  if (fn && template_id && fn != error_mark_node)
    fn = build2 (TEMPLATE_ID_EXPR, unknown_type_node, fn, tmpl_args);

  return cp_expr (fn, loc);
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
  vec<tree, va_gc> *orig_args = *args;

  if (fn == error_mark_node)
    return error_mark_node;

  gcc_assert (!TYPE_P (fn));

  /* If FN may be a FUNCTION_DECL obfuscated by force_paren_expr, undo
     it so that we can tell this is a call to a known function.  */
  fn = maybe_undo_parenthesized_ref (fn);

  STRIP_ANY_LOCATION_WRAPPER (fn);

  orig_fn = fn;

  if (processing_template_decl)
    {
      /* If FN is a local extern declaration (or set thereof) in a template,
	 look it up again at instantiation time.  */
      if (is_overloaded_fn (fn))
	{
	  tree ifn = get_first_fn (fn);
	  if (TREE_CODE (ifn) == FUNCTION_DECL
	      && dependent_local_decl_p (ifn))
	    orig_fn = DECL_NAME (ifn);
	}

      /* If the call expression is dependent, build a CALL_EXPR node
	 with no type; type_dependent_expression_p recognizes
	 expressions with no type as being dependent.  */
      if (type_dependent_expression_p (fn)
	  || any_type_dependent_arguments_p (*args))
	{
	  result = build_min_nt_call_vec (orig_fn, *args);
	  SET_EXPR_LOCATION (result, cp_expr_loc_or_input_loc (fn));
	  KOENIG_LOOKUP_P (result) = koenig_p;
	  /* Disable the std::move warnings since this call was dependent
	     (c++/89780, c++/107363).  This also suppresses the
	     -Wredundant-move warning.  */
	  suppress_warning (result, OPT_Wpessimizing_move);

	  if (cfun && cp_function_chain && !cp_unevaluated_operand)
	    {
	      bool abnormal = true;
	      for (lkp_iterator iter (maybe_get_fns (fn)); iter; ++iter)
		{
		  tree fndecl = STRIP_TEMPLATE (*iter);
		  if (TREE_CODE (fndecl) != FUNCTION_DECL
		      || !TREE_THIS_VOLATILE (fndecl))
		    {
		      abnormal = false;
		      break;
		    }
		}
	      /* FIXME: Stop warning about falling off end of non-void
		 function.   But this is wrong.  Even if we only see
		 no-return fns at this point, we could select a
		 future-defined return fn during instantiation.  Or
		 vice-versa.  */
	      if (abnormal)
		current_function_returns_abnormally = 1;
	    }
	  if (TREE_CODE (fn) == COMPONENT_REF)
	    maybe_generic_this_capture (TREE_OPERAND (fn, 0),
					TREE_OPERAND (fn, 1));
	  return result;
	}
      orig_args = make_tree_vector_copy (*args);
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

      if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (get_first_fn (fn)))
	{
	  /* A constructor call always uses a dummy object.  (This constructor
	     call which has the form A::A () is actually invalid and we are
	     going to reject it later in build_new_method_call.)  */
	  object = build_dummy_object (BINFO_TYPE (BASELINK_ACCESS_BINFO (fn)));
	}
      else
	object = maybe_dummy_object (BINFO_TYPE (BASELINK_ACCESS_BINFO (fn)),
				     NULL);

      result = build_new_method_call (object, fn, args, NULL_TREE,
				      (disallow_virtual
				       ? LOOKUP_NORMAL|LOOKUP_NONVIRTUAL
				       : LOOKUP_NORMAL),
				      /*fn_p=*/NULL,
				      complain);
    }
  else if (concept_check_p (fn))
    {
      error_at (EXPR_LOC_OR_LOC (fn, input_location),
		"cannot call a concept as a function");
      return error_mark_node;
    }
  else if (is_overloaded_fn (fn))
    {
      /* If the function is an overloaded builtin, resolve it.  */
      if (TREE_CODE (fn) == FUNCTION_DECL
	  && (DECL_BUILT_IN_CLASS (fn) == BUILT_IN_NORMAL
	      || DECL_BUILT_IN_CLASS (fn) == BUILT_IN_MD))
	result = resolve_overloaded_builtin (input_location, fn, *args,
					     complain & tf_error);

      if (!result)
	{
	  tree alloc_size_attr = NULL_TREE;
	  if (warn_calloc_transposed_args
	      && TREE_CODE (fn) == FUNCTION_DECL
	      && (alloc_size_attr
		  = lookup_attribute ("alloc_size",
				      TYPE_ATTRIBUTES (TREE_TYPE (fn)))))
	    if (TREE_VALUE (alloc_size_attr) == NULL_TREE
		|| TREE_CHAIN (TREE_VALUE (alloc_size_attr)) == NULL_TREE)
	      alloc_size_attr = NULL_TREE;
	  if ((warn_sizeof_pointer_memaccess || alloc_size_attr)
	      && (complain & tf_warning)
	      && !vec_safe_is_empty (*args)
	      && !processing_template_decl)
	    {
	      location_t sizeof_arg_loc[6];
	      tree sizeof_arg[6];
	      unsigned int i;
	      for (i = 0; i < (alloc_size_attr ? 6 : 3); i++)
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
	      if (warn_sizeof_pointer_memaccess)
		{
		  auto same_p = same_type_ignoring_top_level_qualifiers_p;
		  sizeof_pointer_memaccess_warning (sizeof_arg_loc, fn, *args,
						    sizeof_arg, same_p);
		}
	      if (alloc_size_attr)
		warn_for_calloc (sizeof_arg_loc, fn, *args, sizeof_arg,
				 alloc_size_attr);
	    }

	  if ((complain & tf_warning)
	      && TREE_CODE (fn) == FUNCTION_DECL
	      && fndecl_built_in_p (fn, BUILT_IN_MEMSET)
	      && vec_safe_length (*args) == 3
	      && !any_type_dependent_arguments_p (*args))
	    {
	      tree arg0 = (*orig_args)[0];
	      tree arg1 = (*orig_args)[1];
	      tree arg2 = (*orig_args)[2];
	      int literal_mask = ((literal_integer_zerop (arg1) << 1)
				  | (literal_integer_zerop (arg2) << 2));
	      warn_for_memset (input_location, arg0, arg2, literal_mask);
	    }

	  /* A call to a namespace-scope function.  */
	  result = build_new_function_call (fn, args, complain);
	}
    }
  else if (TREE_CODE (fn) == PSEUDO_DTOR_EXPR)
    {
      if (!vec_safe_is_empty (*args))
	error ("arguments to destructor are not allowed");
      /* C++20/DR: If the postfix-expression names a pseudo-destructor (in
	 which case the postfix-expression is a possibly-parenthesized class
	 member access), the function call destroys the object of scalar type
	 denoted by the object expression of the class member access.  */
      tree ob = TREE_OPERAND (fn, 0);
      if (obvalue_p (ob))
	result = build_trivial_dtor_call (ob, true);
      else
	/* No location to clobber.  */
	result = convert_to_void (ob, ICV_STATEMENT, complain);
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

      /* Prune all but the selected function from the original overload
	 set so that we can avoid some duplicate work at instantiation time.  */
      if (TREE_CODE (result) == CALL_EXPR
	  && really_overloaded_fn (orig_fn))
	{
	  tree sel_fn = CALL_EXPR_FN (result);
	  if (TREE_CODE (sel_fn) == COMPONENT_REF)
	    {
	      /* The non-dependent result of build_new_method_call.  */
	      sel_fn = TREE_OPERAND (sel_fn, 1);
	      gcc_assert (BASELINK_P (sel_fn));
	    }
	  else if (TREE_CODE (sel_fn) == ADDR_EXPR)
	    /* Our original callee wasn't wrapped in an ADDR_EXPR,
	       so strip this ADDR_EXPR added by build_over_call.  */
	    sel_fn = TREE_OPERAND (sel_fn, 0);
	  orig_fn = sel_fn;
	}

      tree r = build_call_vec (TREE_TYPE (result), orig_fn, orig_args);
      SET_EXPR_LOCATION (r, input_location);
      KOENIG_LOOKUP_P (r) = koenig_p;
      TREE_NO_WARNING (r) = TREE_NO_WARNING (result);
      release_tree_vector (orig_args);
      result = convert_from_reference (r);
    }

  return result;
}

/* Finish a call to a postfix increment or decrement or EXPR.  (Which
   is indicated by CODE, which should be POSTINCREMENT_EXPR or
   POSTDECREMENT_EXPR.)  */

cp_expr
finish_increment_expr (cp_expr expr, enum tree_code code)
{
  /* input_location holds the location of the trailing operator token.
     Build a location of the form:
       expr++
       ~~~~^~
     with the caret at the operator token, ranging from the start
     of EXPR to the end of the operator token.  */
  location_t combined_loc = make_location (input_location,
					   expr.get_start (),
					   get_finish (input_location));
  cp_expr result = build_x_unary_op (combined_loc, code, expr,
				     NULL_TREE, tf_warning_or_error);
  /* TODO: build_x_unary_op doesn't honor the location, so set it here.  */
  result.set_location (combined_loc);
  return result;
}

/* Finish a use of `this'.  Returns an expression for `this'.  */

tree
finish_this_expr (void)
{
  tree result = NULL_TREE;

  if (current_class_ptr)
    {
      tree type = TREE_TYPE (current_class_ref);

      /* In a lambda expression, 'this' refers to the captured 'this'.  */
      if (LAMBDA_TYPE_P (type))
        result = lambda_expr_this_capture (CLASSTYPE_LAMBDA_EXPR (type), true);
      else
        result = current_class_ptr;
    }

  if (result)
    /* The keyword 'this' is a prvalue expression.  */
    return rvalue (result);

  tree fn = current_nonlambda_function ();
  if (fn && DECL_XOBJ_MEMBER_FUNCTION_P (fn))
    {
      auto_diagnostic_group d;
      error ("%<this%> is unavailable for explicit object member "
	     "functions");
      tree xobj_parm = DECL_ARGUMENTS (fn);
      gcc_assert (xobj_parm);
      tree parm_name = DECL_NAME (xobj_parm);

      static tree remembered_fn = NULL_TREE;
      /* Only output this diagnostic once per function.  */
      if (remembered_fn == fn)
	/* Early escape.  */;
      else if (parm_name)
	inform (DECL_SOURCE_LOCATION (xobj_parm),
		"use explicit object parameter %qs instead",
		IDENTIFIER_POINTER (parm_name));
      else
	inform (DECL_SOURCE_LOCATION (xobj_parm),
		"name the explicit object parameter");

      remembered_fn = fn;
    }
  else if (fn && DECL_STATIC_FUNCTION_P (fn))
    error ("%<this%> is unavailable for static member functions");
  else if (fn && processing_contract_condition && DECL_CONSTRUCTOR_P (fn))
    error ("invalid use of %<this%> before it is valid");
  else if (fn && processing_contract_condition && DECL_DESTRUCTOR_P (fn))
    error ("invalid use of %<this%> after it is valid");
  else if (fn)
    error ("invalid use of %<this%> in non-member function");
  else
    error ("invalid use of %<this%> at top level");
  return error_mark_node;
}

/* Finish a pseudo-destructor expression.  If SCOPE is NULL, the
   expression was of the form `OBJECT.~DESTRUCTOR' where DESTRUCTOR is
   the TYPE for the type given.  If SCOPE is non-NULL, the expression
   was of the form `OBJECT.SCOPE::~DESTRUCTOR'.  */

tree
finish_pseudo_destructor_expr (tree object, tree scope, tree destructor,
			       location_t loc, tsubst_flags_t complain)
{
  if (object == error_mark_node || destructor == error_mark_node)
    return error_mark_node;

  gcc_assert (TYPE_P (destructor));

  if (!processing_template_decl)
    {
      if (scope == error_mark_node)
	{
	  if (complain & tf_error)
	    error_at (loc, "invalid qualifying scope in pseudo-destructor name");
	  return error_mark_node;
	}
      if (is_auto (destructor))
	destructor = TREE_TYPE (object);
      if (scope && TYPE_P (scope) && !check_dtor_name (scope, destructor))
	{
	  if (complain & tf_error)
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
	  if (complain & tf_error)
	    error_at (loc, "%qE is not of type %qT", object, destructor);
	  return error_mark_node;
	}
    }

  tree type = (type_dependent_expression_p (object)
	       ? NULL_TREE : void_type_node);

  return build3_loc (loc, PSEUDO_DTOR_EXPR, type, object,
		     scope, destructor);
}

/* Finish an expression of the form CODE EXPR.  */

cp_expr
finish_unary_op_expr (location_t op_loc, enum tree_code code, cp_expr expr,
		      tsubst_flags_t complain)
{
  /* Build a location of the form:
       ++expr
       ^~~~~~
     with the caret at the operator token, ranging from the start
     of the operator token to the end of EXPR.  */
  location_t combined_loc = make_location (op_loc,
					   op_loc, expr.get_finish ());
  cp_expr result = build_x_unary_op (combined_loc, code, expr,
				     NULL_TREE, complain);
  /* TODO: build_x_unary_op doesn't always honor the location.  */
  result.set_location (combined_loc);

  if (result == error_mark_node)
    return result;

  if (!(complain & tf_warning))
    return result;

  tree result_ovl = result;
  tree expr_ovl = expr;

  if (!processing_template_decl)
    expr_ovl = cp_fully_fold (expr_ovl);

  if (!CONSTANT_CLASS_P (expr_ovl)
      || TREE_OVERFLOW_P (expr_ovl))
    return result;

  if (!processing_template_decl)
    result_ovl = cp_fully_fold (result_ovl);

  if (CONSTANT_CLASS_P (result_ovl) && TREE_OVERFLOW_P (result_ovl))
    overflow_warning (combined_loc, result_ovl);

  return result;
}

/* Return true if CONSTRUCTOR EXPR after pack expansion could have no
   elements.  */

static bool
maybe_zero_constructor_nelts (tree expr)
{
  if (CONSTRUCTOR_NELTS (expr) == 0)
    return true;
  if (!processing_template_decl)
    return false;
  for (constructor_elt &elt : CONSTRUCTOR_ELTS (expr))
    if (!PACK_EXPANSION_P (elt.value))
      return false;
  return true;
}

/* Finish a compound-literal expression or C++11 functional cast with aggregate
   initializer.  TYPE is the type to which the CONSTRUCTOR in COMPOUND_LITERAL
   is being cast.  */

tree
finish_compound_literal (tree type, tree compound_literal,
			 tsubst_flags_t complain,
			 fcl_t fcl_context)
{
  if (type == error_mark_node)
    return error_mark_node;

  if (TYPE_REF_P (type))
    {
      compound_literal
	= finish_compound_literal (TREE_TYPE (type), compound_literal,
				   complain, fcl_context);
      /* The prvalue is then used to direct-initialize the reference.  */
      tree r = (perform_implicit_conversion_flags
		(type, compound_literal, complain, LOOKUP_NORMAL));
      return convert_from_reference (r);
    }

  if (!TYPE_OBJ_P (type))
    {
      /* DR2351 */
      if (VOID_TYPE_P (type) && CONSTRUCTOR_NELTS (compound_literal) == 0)
	{
	  if (!processing_template_decl)
	    return void_node;
	  TREE_TYPE (compound_literal) = type;
	  TREE_HAS_CONSTRUCTOR (compound_literal) = 1;
	  CONSTRUCTOR_IS_DEPENDENT (compound_literal) = 0;
	  return compound_literal;
	}
      else if (VOID_TYPE_P (type)
	       && processing_template_decl
	       && maybe_zero_constructor_nelts (compound_literal))
	/* If there are only packs in compound_literal, it could
	   be void{} after pack expansion.  */;
      else
	{
	  if (complain & tf_error)
	    error ("compound literal of non-object type %qT", type);
	  return error_mark_node;
	}
    }

  if (template_placeholder_p (type))
    {
      type = do_auto_deduction (type, compound_literal, type, complain,
				adc_variable_type);
      if (type == error_mark_node)
	return error_mark_node;
    }
  /* C++23 auto{x}.  */
  else if (is_auto (type)
	   && !AUTO_IS_DECLTYPE (type)
	   && CONSTRUCTOR_NELTS (compound_literal) == 1)
    {
      if (is_constrained_auto (type))
	{
	  if (complain & tf_error)
	    error ("%<auto{x}%> cannot be constrained");
	  return error_mark_node;
	}
      else if (cxx_dialect < cxx23)
	pedwarn (input_location, OPT_Wc__23_extensions,
		 "%<auto{x}%> only available with "
		 "%<-std=c++23%> or %<-std=gnu++23%>");
      type = do_auto_deduction (type, compound_literal, type, complain,
				adc_variable_type);
      if (type == error_mark_node)
	return error_mark_node;
    }

  /* Used to hold a copy of the compound literal in a template.  */
  tree orig_cl = NULL_TREE;

  if (processing_template_decl)
    {
      const bool dependent_p
	= (instantiation_dependent_expression_p (compound_literal)
	   || dependent_type_p (type));
      if (dependent_p)
	/* We're about to return, no need to copy.  */
	orig_cl = compound_literal;
      else
	/* We're going to need a copy.  */
	orig_cl = unshare_constructor (compound_literal);
      TREE_TYPE (orig_cl) = type;
      /* Mark the expression as a compound literal.  */
      TREE_HAS_CONSTRUCTOR (orig_cl) = 1;
      /* And as instantiation-dependent.  */
      CONSTRUCTOR_IS_DEPENDENT (orig_cl) = dependent_p;
      if (fcl_context == fcl_c99)
	CONSTRUCTOR_C99_COMPOUND_LITERAL (orig_cl) = 1;
      /* If the compound literal is dependent, we're done for now.  */
      if (dependent_p)
	return orig_cl;
      /* Otherwise, do go on to e.g. check narrowing.  */
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
      return build_functional_cast (input_location, type,
				    compound_literal, complain);
    }

  if (TREE_CODE (type) == ARRAY_TYPE
      && check_array_initializer (NULL_TREE, type, compound_literal))
    return error_mark_node;
  compound_literal = reshape_init (type, compound_literal, complain);
  if (SCALAR_TYPE_P (type)
      && !BRACE_ENCLOSED_INITIALIZER_P (compound_literal)
      && !check_narrowing (type, compound_literal, complain))
    return error_mark_node;
  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == NULL_TREE)
    {
      cp_complete_array_type_or_error (&type, compound_literal,
				       false, complain);
      if (type == error_mark_node)
	return error_mark_node;
    }
  compound_literal = digest_init_flags (type, compound_literal,
					LOOKUP_NORMAL | LOOKUP_NO_NARROWING,
					complain);
  if (compound_literal == error_mark_node)
    return error_mark_node;

  /* If we're in a template, return the original compound literal.  */
  if (orig_cl)
    return orig_cl;

  if (TREE_CODE (compound_literal) == CONSTRUCTOR)
    {
      TREE_HAS_CONSTRUCTOR (compound_literal) = true;
      if (fcl_context == fcl_c99)
	CONSTRUCTOR_C99_COMPOUND_LITERAL (compound_literal) = 1;
    }

  /* Put static/constant array temporaries in static variables.  */
  /* FIXME all C99 compound literals should be variables rather than C++
     temporaries, unless they are used as an aggregate initializer.  */
  if ((!at_function_scope_p () || CP_TYPE_CONST_P (type))
      && fcl_context == fcl_c99
      && TREE_CODE (type) == ARRAY_TYPE
      && !TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
      && initializer_constant_valid_p (compound_literal, type))
    {
      tree decl = create_temporary_var (type);
      DECL_CONTEXT (decl) = NULL_TREE;
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

  /* Represent other compound literals with TARGET_EXPR so we produce
     a prvalue, and can elide copies.  */
  if (!VECTOR_TYPE_P (type)
      && (TREE_CODE (compound_literal) == CONSTRUCTOR
	  || TREE_CODE (compound_literal) == VEC_INIT_EXPR))
    {
      /* The CONSTRUCTOR is now an initializer, not a compound literal.  */
      if (TREE_CODE (compound_literal) == CONSTRUCTOR)
	TREE_HAS_CONSTRUCTOR (compound_literal) = false;
      compound_literal = get_target_expr (compound_literal, complain);
    }
  else
    /* For e.g. int{42} just make sure it's a prvalue.  */
    compound_literal = rvalue (compound_literal);

  return compound_literal;
}

/* Return the declaration for the function-name variable indicated by
   ID.  */

tree
finish_fname (tree id)
{
  tree decl;

  decl = fname_decl (input_location, C_RID_CODE (id), id);
  if (processing_template_decl && current_function_decl
      && decl != error_mark_node)
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

  if (vec_safe_length (scope_chain->omp_declare_target_attribute))
    {
      cp_omp_declare_target_attr
	a = scope_chain->omp_declare_target_attribute->pop ();
      if (!errorcount)
	error ("%qs without corresponding %qs",
	       a.device_type >= 0 ? "#pragma omp begin declare target"
				  : "#pragma omp declare target",
	       "#pragma omp end declare target");
      vec_safe_truncate (scope_chain->omp_declare_target_attribute, 0);
    }
  if (vec_safe_length (scope_chain->omp_begin_assumes))
    {
      if (!errorcount)
	error ("%qs without corresponding %qs",
	       "#pragma omp begin assumes", "#pragma omp end assumes");
      vec_safe_truncate (scope_chain->omp_begin_assumes, 0);
    }
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

  /* Associate the constraints with the underlying declaration,
     not the template.  */
  tree constr = current_template_constraints ();
  set_constraints (decl, constr);

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
	{
	  if (tree t = maybe_get_template_decl_from_type_decl (argument))
	    if (TREE_CODE (t) == TEMPLATE_DECL)
	      return t;
	  error ("invalid use of type %qT as a default value for a template "
		 "template-parameter", TREE_TYPE (argument));
	}
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

  if (processing_template_parmlist && !LAMBDA_TYPE_P (t))
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
	  && id_equal (DECL_NAME (ns), "decimal"))
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
      pushtag (make_anon_name (), t);
    }

  if (TYPE_BEING_DEFINED (t))
    {
      t = make_class_type (TREE_CODE (t));
      pushtag (TYPE_IDENTIFIER (t), t);
    }

  if (modules_p ())
    {
      if (!module_may_redeclare (TYPE_NAME (t)))
	return error_mark_node;
      set_instantiating_module (TYPE_NAME (t));
      set_defining_module (TYPE_NAME (t));
    }

  maybe_process_partial_specialization (t);
  pushclass (t);
  TYPE_BEING_DEFINED (t) = 1;
  class_binding_level->defining_class_p = 1;

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
  if (! TYPE_UNNAMED_P (t))
    {
      struct c_fileinfo *finfo = \
	get_fileinfo (LOCATION_FILE (input_location));
      CLASSTYPE_INTERFACE_ONLY (t) = finfo->interface_only;
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X
	(t, finfo->interface_unknown);
    }
  reset_specialization ();

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

  /* Don't add decls after definition.  */
  gcc_assert (TYPE_BEING_DEFINED (current_class_type)
	      /* We can add lambda types when late parsing default
		 arguments.  */
	      || LAMBDA_TYPE_P (TREE_TYPE (decl)));

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

  /* Remember the single FIELD_DECL an anonymous aggregate type is used for.  */
  if (TREE_CODE (decl) == FIELD_DECL
      && ANON_AGGR_TYPE_P (TREE_TYPE (decl)))
    {
      gcc_assert (!ANON_AGGR_TYPE_FIELD (TYPE_MAIN_VARIANT (TREE_TYPE (decl))));
      ANON_AGGR_TYPE_FIELD (TYPE_MAIN_VARIANT (TREE_TYPE (decl))) = decl;
    }

  if (TREE_CODE (decl) == USING_DECL)
    /* Avoid debug info for class-scope USING_DECLS for now, we'll
       call cp_emit_debug_info_for_using later. */
    DECL_IGNORED_P (decl) = 1;

  /* Check for bare parameter packs in the non-static data member
     declaration.  */
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
  if (DECL_LANG_SPECIFIC (decl))
    SET_DECL_LANGUAGE (decl, lang_cplusplus);

  bool add = false;

  /* Functions and non-functions are added differently.  */
  if (DECL_DECLARES_FUNCTION_P (decl))
    add = add_method (current_class_type, decl, false);
  /* Enter the DECL into the scope of the class, if the class
     isn't a closure (whose fields are supposed to be unnamed).  */
  else if (CLASSTYPE_LAMBDA_EXPR (current_class_type)
	   || maybe_push_used_methods (decl)
	   || pushdecl_class_level (decl))
    add = true;

  if (add)
    {
      /* All TYPE_DECLs go at the end of TYPE_FIELDS.  Ordinary fields
	 go at the beginning.  The reason is that
	 legacy_nonfn_member_lookup searches the list in order, and we
	 want a field name to override a type name so that the "struct
	 stat hack" will work.  In particular:

	   struct S { enum E { }; static const int E = 5; int ary[S::E]; } s;

	 is valid.  */

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

// Returns the template type of the class scope being entered. If we're
// entering a constrained class scope. TYPE is the class template
// scope being entered and we may need to match the intended type with
// a constrained specialization. For example:
//
//    template<Object T>
//      struct S { void f(); }; #1
//
//    template<Object T>
//      void S<T>::f() { }      #2
//
// We check, in #2, that S<T> refers precisely to the type declared by
// #1 (i.e., that the constraints match). Note that the following should
// be an error since there is no specialization of S<T> that is
// unconstrained, but this is not diagnosed here.
//
//    template<typename T>
//      void S<T>::f() { }
//
// We cannot diagnose this problem here since this function also matches
// qualified template names that are not part of a definition. For example:
//
//    template<Integral T, Floating_point U>
//      typename pair<T, U>::first_type void f(T, U);
//
// Here, it is unlikely that there is a partial specialization of
// pair constrained for Integral and Floating_point arguments.
//
// The general rule is: if a constrained specialization with matching
// constraints is found return that type. Also note that if TYPE is not a
// class-type (e.g. a typename type), then no fixup is needed.

static tree
fixup_template_type (tree type)
{
  // Find the template parameter list at the a depth appropriate to
  // the scope we're trying to enter.
  tree parms = current_template_parms;
  int depth = template_class_depth (type);
  for (int n = current_template_depth; n > depth && parms; --n)
    parms = TREE_CHAIN (parms);
  if (!parms)
    return type;
  tree cur_reqs = TEMPLATE_PARMS_CONSTRAINTS (parms);
  tree cur_constr = build_constraints (cur_reqs, NULL_TREE);

  // Search for a specialization whose type and constraints match.
  tree tmpl = CLASSTYPE_TI_TEMPLATE (type);
  tree specs = DECL_TEMPLATE_SPECIALIZATIONS (tmpl);
  while (specs)
    {
      tree spec_constr = get_constraints (TREE_VALUE (specs));

      // If the type and constraints match a specialization, then we
      // are entering that type.
      if (same_type_p (type, TREE_TYPE (specs))
	  && equivalent_constraints (cur_constr, spec_constr))
        return TREE_TYPE (specs);
      specs = TREE_CHAIN (specs);
    }

  // If no specialization matches, then must return the type
  // previously found.
  return type;
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
				NULL_TREE, NULL_TREE,
				tf_warning_or_error | tf_user);
  if (entering_scope)
    type = adjust_type_for_entering_scope (type);

  /* If we might be entering the scope of a partial specialization,
     find the one with the right constraints.  */
  if (flag_concepts
      && entering_scope
      && CLASS_TYPE_P (type)
      && CLASSTYPE_TEMPLATE_INFO (type)
      && dependent_type_p (type)
      && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (type)))
    type = fixup_template_type (type);

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
  tree access_path = TYPE_BINFO (cl);
  tree conv_path = (cl == scope ? access_path
		    : lookup_base (cl, scope, ba_any, NULL, tf_none));
  return build_baselink (conv_path, access_path, fns, /*optype=*/NULL_TREE);
}

/* Returns true iff DECL is a variable from a function outside
   the current one.  */

static bool
outer_var_p (tree decl)
{
  /* These should have been stripped or otherwise handled by the caller.  */
  gcc_checking_assert (!REFERENCE_REF_P (decl));

  return ((VAR_P (decl) || TREE_CODE (decl) == PARM_DECL)
	  && DECL_FUNCTION_SCOPE_P (decl)
	  /* Don't get confused by temporaries.  */
	  && DECL_NAME (decl)
	  && (DECL_CONTEXT (decl) != current_function_decl
	      || parsing_nsdmi ()));
}

/* As above, but also checks that DECL is automatic.  */

bool
outer_automatic_var_p (tree decl)
{
  return (outer_var_p (decl)
	  && !TREE_STATIC (decl));
}

/* DECL satisfies outer_automatic_var_p.  Possibly complain about it or
   rewrite it for lambda capture.

   If ODR_USE is true, we're being called from mark_use, and we complain about
   use of constant variables.  If ODR_USE is false, we're being called for the
   id-expression, and we do lambda capture.  */

tree
process_outer_var_ref (tree decl, tsubst_flags_t complain, bool odr_use)
{
  if (cp_unevaluated_operand)
    {
      tree type = TREE_TYPE (decl);
      if (!dependent_type_p (type)
	  && variably_modified_type_p (type, NULL_TREE))
	/* VLAs are used even in unevaluated context.  */;
      else
	/* It's not a use (3.2) if we're in an unevaluated context.  */
	return decl;
    }
  if (decl == error_mark_node)
    return decl;

  tree context = DECL_CONTEXT (decl);
  tree containing_function = current_function_decl;
  tree lambda_stack = NULL_TREE;
  tree lambda_expr = NULL_TREE;
  tree initializer = convert_from_reference (decl);
  tree var = strip_normal_capture_proxy (decl);

  /* Mark it as used now even if the use is ill-formed.  */
  if (!mark_used (decl, complain))
    return error_mark_node;

  if (parsing_nsdmi ())
    containing_function = NULL_TREE;

  if (containing_function && LAMBDA_FUNCTION_P (containing_function))
    {
      /* Check whether we've already built a proxy.  */
      tree d = retrieve_local_specialization (var);

      if (d && d != decl && is_capture_proxy (d))
	{
	  if (DECL_CONTEXT (d) == containing_function)
	    /* We already have an inner proxy.  */
	    return d;
	  else
	    /* We need to capture an outer proxy.  */
	    return process_outer_var_ref (d, complain, odr_use);
	}
    }

  /* If we are in a lambda function, we can move out until we hit
     1. the context,
     2. a non-lambda function, or
     3. a non-default capturing lambda function.  */
  while (context != containing_function
	 /* containing_function can be null with invalid generic lambdas.  */
	 && containing_function
	 && LAMBDA_FUNCTION_P (containing_function))
    {
      tree closure = DECL_CONTEXT (containing_function);
      lambda_expr = CLASSTYPE_LAMBDA_EXPR (closure);

      if (TYPE_CLASS_SCOPE_P (closure))
	/* A lambda in an NSDMI (c++/64496).  */
	break;

      if (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda_expr) == CPLD_NONE)
	break;

      lambda_stack = tree_cons (NULL_TREE, lambda_expr, lambda_stack);

      containing_function = decl_function_context (containing_function);
    }

  /* In a lambda within a template, wait until instantiation time to implicitly
     capture a parameter pack.  We want to wait because we don't know if we're
     capturing the whole pack or a single element, and it's OK to wait because
     find_parameter_packs_r walks into the lambda body.  */
  if (context == containing_function
      && DECL_PACK_P (decl))
    return decl;

  if (lambda_expr && VAR_P (decl) && DECL_ANON_UNION_VAR_P (decl))
    {
      if (complain & tf_error)
	error ("cannot capture member %qD of anonymous union", decl);
      return error_mark_node;
    }
  /* Do lambda capture when processing the id-expression, not when
     odr-using a variable.  */
  if (!odr_use && context == containing_function)
    decl = add_default_capture (lambda_stack,
				/*id=*/DECL_NAME (decl), initializer);
  /* Only an odr-use of an outer automatic variable causes an
     error, and a constant variable can decay to a prvalue
     constant without odr-use.  So don't complain yet.  */
  else if (!odr_use && decl_constant_var_p (var))
    return var;
  else if (lambda_expr)
    {
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  error ("%qD is not captured", decl);
	  tree closure = LAMBDA_EXPR_CLOSURE (lambda_expr);
	  if (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda_expr) == CPLD_NONE)
	    inform (location_of (closure),
		    "the lambda has no capture-default");
	  else if (TYPE_CLASS_SCOPE_P (closure))
	    inform (UNKNOWN_LOCATION, "lambda in local class %q+T cannot "
		    "capture variables from the enclosing context",
		    TYPE_CONTEXT (closure));
	  inform (DECL_SOURCE_LOCATION (decl), "%q#D declared here", decl);
	}
      return error_mark_node;
    }
  else if (processing_contract_condition && (TREE_CODE (decl) == PARM_DECL))
    /* Use of a parameter in a contract condition is fine.  */
    return decl;
  else
    {
      if (complain & tf_error)
	{
	  auto_diagnostic_group d;
	  error (VAR_P (decl)
		 ? G_("use of local variable with automatic storage from "
		      "containing function")
		 : G_("use of parameter from containing function"));
	  inform (DECL_SOURCE_LOCATION (decl), "%q#D declared here", decl);
	}
      return error_mark_node;
    }
  return decl;
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
static cp_expr
finish_id_expression_1 (tree id_expression,
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
		  || (!dependentish_scope_p (scope)
		      && !(identifier_p (id_expression)
			   && IDENTIFIER_CONV_OP_P (id_expression)
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

      /* Remember that the name was used in the definition of
	 the current class so that we can check later to see if
	 the meaning would have been different after the class
	 was entirely defined.  */
      if (!scope && decl != error_mark_node && identifier_p (id_expression))
	maybe_note_name_used_in_class (id_expression, decl);

      /* A use in unevaluated operand might not be instantiated appropriately
	 if tsubst_copy builds a dummy parm, or if we never instantiate a
	 generic lambda, so mark it now.  */
      if (processing_template_decl && cp_unevaluated_operand)
	mark_type_use (decl);

      /* Disallow uses of local variables from containing functions, except
	 within lambda-expressions.  */
      if (outer_automatic_var_p (decl))
	{
	  decl = process_outer_var_ref (decl, tf_warning_or_error);
	  if (decl == error_mark_node)
	    return error_mark_node;
	}

      /* Also disallow uses of function parameters outside the function
	 body, except inside an unevaluated context (i.e. decltype).  */
      if (TREE_CODE (decl) == PARM_DECL
	  && DECL_CONTEXT (decl) == NULL_TREE
	  && !CONSTRAINT_VAR_P (decl)
	  && !cp_unevaluated_operand
	  && !processing_contract_condition
	  && !processing_omp_trait_property_expr)
	{
	  *error_msg = G_("use of parameter outside function body");
	  return error_mark_node;
	}
    }

  /* If we didn't find anything, or what we found was a type,
     then this wasn't really an id-expression.  */
  if (TREE_CODE (decl) == TEMPLATE_DECL
      && !DECL_FUNCTION_TEMPLATE_P (decl))
    {
      *error_msg = G_("missing template arguments");
      return error_mark_node;
    }
  else if (TREE_CODE (decl) == TYPE_DECL
	   || TREE_CODE (decl) == NAMESPACE_DECL)
    {
      *error_msg = G_("expected primary-expression");
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
      r = DECL_INITIAL (decl);
      if (CLASS_TYPE_P (TREE_TYPE (r)) && !CP_TYPE_CONST_P (TREE_TYPE (r)))
	{
	  /* If the entity is a template parameter object for a template
	     parameter of type T, the type of the expression is const T.  */
	  tree ctype = TREE_TYPE (r);
	  ctype = cp_build_qualified_type (ctype, (cp_type_quals (ctype)
						   | TYPE_QUAL_CONST));
	  r = build1 (VIEW_CONVERT_EXPR, ctype, r);
	}
      r = convert_from_reference (r);
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
  else if (TREE_CODE (decl) == UNBOUND_CLASS_TEMPLATE)
    {
      gcc_checking_assert (scope);
      *idk = CP_ID_KIND_QUALIFIED;
      cp_warn_deprecated_use_scopes (scope);
      decl = finish_qualified_id_expr (scope, decl, done, address_p,
				       template_p, template_arg_p,
				       tf_warning_or_error);
    }
  else
    {
      if (TREE_CODE (decl) == TEMPLATE_ID_EXPR
	  && variable_template_p (TREE_OPERAND (decl, 0))
	  && !concept_check_p (decl))
	/* Try resolving this variable TEMPLATE_ID_EXPR (which is always
	   considered type-dependent) now, so that the dependence test that
	   follows gives us the right answer: if it represents a non-dependent
	   variable template-id then finish_template_variable will yield the
	   corresponding non-dependent VAR_DECL.  */
	decl = finish_template_variable (decl);

      bool dependent_p = type_dependent_expression_p (decl);

      /* If the declaration was explicitly qualified indicate
	 that.  The semantics of `A::f(3)' are different than
	 `f(3)' if `f' is virtual.  */
      *idk = (scope
	      ? CP_ID_KIND_QUALIFIED
	      : (TREE_CODE (decl) == TEMPLATE_ID_EXPR
		 ? CP_ID_KIND_TEMPLATE_ID
		 : (dependent_p
		    ? CP_ID_KIND_UNQUALIFIED_DEPENDENT
		    : CP_ID_KIND_UNQUALIFIED)));

      if (dependent_p
	  && !scope
	  && DECL_P (decl)
	  && any_dependent_type_attributes_p (DECL_ATTRIBUTES (decl)))
	/* Dependent type attributes on the decl mean that the TREE_TYPE is
	   wrong, so just return the identifier.  */
	return id_expression;

      if (DECL_CLASS_TEMPLATE_P (decl))
	{
	  error ("use of class template %qT as expression", decl);
	  return error_mark_node;
	}

      if (TREE_CODE (decl) == TREE_LIST)
	{
	  /* Ambiguous reference to base members.  */
	  auto_diagnostic_group d;
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
	  && !dependent_p
	  && integral_constant_expression_p
	  && !decl_constant_var_p (decl)
	  && TREE_CODE (decl) != CONST_DECL
	  && !builtin_valid_in_constant_expr_p (decl)
	  && !concept_check_p (decl))
	{
	  if (!allow_non_integral_constant_expression_p)
	    {
	      error ("%qD cannot appear in a constant-expression", decl);
	      return error_mark_node;
	    }
	  *non_integral_constant_expression_p = true;
	}

      if (tree wrap = maybe_get_tls_wrapper_call (decl))
	/* Replace an evaluated use of the thread_local variable with
	   a call to its wrapper.  */
	decl = wrap;
      else if (concept_check_p (decl))
	{
	  /* Nothing more to do. All of the analysis for concept checks
	     is done by build_conept_id, called from the parser.  */
	}
      else if (scope)
	{
	  if (TREE_CODE (decl) == SCOPE_REF)
	    {
	      gcc_assert (same_type_p (scope, TREE_OPERAND (decl, 0)));
	      decl = TREE_OPERAND (decl, 1);
	    }

	  decl = (adjust_result_of_qualified_name_lookup
		  (decl, scope, current_nonlambda_class_type()));

	  cp_warn_deprecated_use_scopes (scope);

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
	  /* We only need to look at the first function,
	     because all the fns share the attribute we're
	     concerned with (all member fns or all non-members).  */
	  tree first_fn = get_first_fn (decl);
	  first_fn = STRIP_TEMPLATE (first_fn);

	  if (!template_arg_p
	      && (TREE_CODE (first_fn) == USING_DECL
		  || (TREE_CODE (first_fn) == FUNCTION_DECL
		      && DECL_FUNCTION_MEMBER_P (first_fn)
		      && !shared_member_p (decl))))
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
		  if (!path)
		    /* PATH can be null for using an enum of an unrelated
		       class; we checked its access in lookup_using_decl.

		       ??? Should this case make a clone instead, like
		       handle_using_decl?  */
		    gcc_assert (TREE_CODE (decl) == CONST_DECL);
		  else
		    perform_or_defer_access_check (TYPE_BINFO (path),
						   decl, decl,
						   tf_warning_or_error);
		}
	    }

	  decl = convert_from_reference (decl);
	}
    }

  return cp_expr (decl, location);
}

/* As per finish_id_expression_1, but adding a wrapper node
   around the result if needed to express LOCATION.  */

cp_expr
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
  cp_expr result
    = finish_id_expression_1 (id_expression, decl, scope, idk,
			      integral_constant_expression_p,
			      allow_non_integral_constant_expression_p,
			      non_integral_constant_expression_p,
			      template_p, done, address_p, template_arg_p,
			      error_msg, location);
  return result.maybe_add_location_wrapper ();
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
  if (!complete_type_or_else (type, NULL_TREE))
    return error_mark_node;

  if (TREE_CODE (type) != ENUMERAL_TYPE)
    {
      error ("%qT is not an enumeration type", type);
      return error_mark_node;
    }

  tree underlying_type = ENUM_UNDERLYING_TYPE (type);

  /* Fixup necessary in this case because ENUM_UNDERLYING_TYPE
     includes TYPE_MIN_VALUE and TYPE_MAX_VALUE information.
     See finish_enum_value_list for details.  */
  if (!ENUM_FIXED_UNDERLYING_TYPE_P (type))
    underlying_type
      = c_common_type_for_mode (TYPE_MODE (underlying_type),
				TYPE_UNSIGNED (underlying_type));

  return underlying_type;
}

/* Implement the __type_pack_element keyword: Return the type
   at index IDX within TYPES.  */

static tree
finish_type_pack_element (tree idx, tree types, tsubst_flags_t complain)
{
  idx = maybe_constant_value (idx, NULL_TREE, mce_true);
  if (TREE_CODE (idx) != INTEGER_CST || !INTEGRAL_TYPE_P (TREE_TYPE (idx)))
    {
      if (complain & tf_error)
	error ("pack index is not an integral constant");
      return error_mark_node;
    }
  if (tree_int_cst_sgn (idx) < 0)
    {
      if (complain & tf_error)
	error ("pack index is negative");
      return error_mark_node;
    }
  if (wi::to_widest (idx) >= TREE_VEC_LENGTH (types))
    {
      if (complain & tf_error)
	error ("pack index is out of range");
      return error_mark_node;
    }
  return TREE_VEC_ELT (types, tree_to_shwi (idx));
}

/* In a pack-index T...[N], return the element at index IDX within TYPES.
   PARENTHESIZED_P is true iff the pack index was wrapped in ().  */

tree
pack_index_element (tree idx, tree types, bool parenthesized_p,
		    tsubst_flags_t complain)
{
  tree r = finish_type_pack_element (idx, types, complain);
  if (parenthesized_p)
    /* For the benefit of decltype(auto).  */
    r = force_paren_expr (r);
  return r;
}

/* Implement the __direct_bases keyword: Return the direct base classes
   of type.  */

tree
calculate_direct_bases (tree type, tsubst_flags_t complain)
{
  if (!complete_type_or_maybe_complain (type, NULL_TREE, complain)
      || !NON_UNION_CLASS_TYPE_P (type))
    return make_tree_vec (0);

  releasing_vec vector;
  vec<tree, va_gc> *base_binfos = BINFO_BASE_BINFOS (TYPE_BINFO (type));
  tree binfo;
  unsigned i;

  /* Virtual bases are initialized first */
  for (i = 0; base_binfos->iterate (i, &binfo); i++)
    if (BINFO_VIRTUAL_P (binfo))
      vec_safe_push (vector, binfo);

  /* Now non-virtuals */
  for (i = 0; base_binfos->iterate (i, &binfo); i++)
    if (!BINFO_VIRTUAL_P (binfo))
      vec_safe_push (vector, binfo);

  tree bases_vec = make_tree_vec (vector->length ());

  for (i = 0; i < vector->length (); ++i)
    TREE_VEC_ELT (bases_vec, i) = BINFO_TYPE ((*vector)[i]);

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
    vec_safe_push (*data, BINFO_TYPE (binfo));
  return NULL_TREE;
}

/* Calculates the morally non-virtual base classes of a class */
static vec<tree, va_gc> *
calculate_bases_helper (tree type)
{
  vec<tree, va_gc> *vector = make_tree_vector ();

  /* Now add non-virtual base classes in order of construction */
  if (TYPE_BINFO (type))
    dfs_walk_all (TYPE_BINFO (type),
		  dfs_calculate_bases_pre, dfs_calculate_bases_post, &vector);
  return vector;
}

tree
calculate_bases (tree type, tsubst_flags_t complain)
{
  if (!complete_type_or_maybe_complain (type, NULL_TREE, complain)
      || !NON_UNION_CLASS_TYPE_P (type))
    return make_tree_vec (0);

  releasing_vec vector;
  tree bases_vec = NULL_TREE;
  unsigned i;
  vec<tree, va_gc> *vbases;
  tree binfo;

  /* First go through virtual base classes */
  for (vbases = CLASSTYPE_VBASECLASSES (type), i = 0;
       vec_safe_iterate (vbases, i, &binfo); i++)
    {
      releasing_vec vbase_bases
	= calculate_bases_helper (BINFO_TYPE (binfo));
      vec_safe_splice (vector, vbase_bases);
    }

  /* Now for the non-virtual bases */
  releasing_vec nonvbases = calculate_bases_helper (type);
  vec_safe_splice (vector, nonvbases);

  /* Note that during error recovery vector->length can even be zero.  */
  if (vector->length () > 1)
    {
      /* Last element is entire class, so don't copy */
      bases_vec = make_tree_vec (vector->length () - 1);

      for (i = 0; i < vector->length () - 1; ++i)
	TREE_VEC_ELT (bases_vec, i) = (*vector)[i];
    }
  else
    bases_vec = make_tree_vec (0);

  return bases_vec;
}

tree
finish_bases (tree type, bool direct)
{
  tree bases = NULL_TREE;

  if (!processing_template_decl)
    {
      /* Parameter packs can only be used in templates */
      error ("parameter pack %<__bases%> only valid in template declaration");
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
finish_offsetof (tree object_ptr, tree expr, location_t loc)
{
  /* If we're processing a template, we can't finish the semantics yet.
     Otherwise we can fold the entire expression now.  */
  if (processing_template_decl)
    {
      expr = build2 (OFFSETOF_EXPR, size_type_node, expr, object_ptr);
      SET_EXPR_LOCATION (expr, loc);
      return expr;
    }

  if (expr == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (expr) == PSEUDO_DTOR_EXPR)
    {
      error ("cannot apply %<offsetof%> to destructor %<~%T%>",
	      TREE_OPERAND (expr, 2));
      return error_mark_node;
    }
  if (FUNC_OR_METHOD_TYPE_P (TREE_TYPE (expr))
      || TREE_TYPE (expr) == unknown_type_node)
    {
      while (TREE_CODE (expr) == COMPONENT_REF
	     || TREE_CODE (expr) == COMPOUND_EXPR)
	expr = TREE_OPERAND (expr, 1);

      if (DECL_P (expr))
	{
	  auto_diagnostic_group d;
	  error ("cannot apply %<offsetof%> to member function %qD", expr);
	  inform (DECL_SOURCE_LOCATION (expr), "declared here");
	}
      else
	error ("cannot apply %<offsetof%> to member function");
      return error_mark_node;
    }
  if (TREE_CODE (expr) == CONST_DECL)
    {
      error ("cannot apply %<offsetof%> to an enumerator %qD", expr);
      return error_mark_node;
    }
  if (REFERENCE_REF_P (expr))
    expr = TREE_OPERAND (expr, 0);
  if (!complete_type_or_else (TREE_TYPE (TREE_TYPE (object_ptr)), object_ptr))
    return error_mark_node;
  if (warn_invalid_offsetof
      && CLASS_TYPE_P (TREE_TYPE (TREE_TYPE (object_ptr)))
      && CLASSTYPE_NON_STD_LAYOUT (TREE_TYPE (TREE_TYPE (object_ptr)))
      && cp_unevaluated_operand == 0)
    warning_at (loc, OPT_Winvalid_offsetof, "%<offsetof%> within "
		"non-standard-layout type %qT is conditionally-supported",
		TREE_TYPE (TREE_TYPE (object_ptr)));
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
  CALL_FROM_THUNK_P (call_expr) = AGGR_INIT_FROM_THUNK_P (aggr_init_expr);
  CALL_EXPR_OPERATOR_SYNTAX (call_expr)
    = CALL_EXPR_OPERATOR_SYNTAX (aggr_init_expr);
  CALL_EXPR_ORDERED_ARGS (call_expr) = CALL_EXPR_ORDERED_ARGS (aggr_init_expr);
  CALL_EXPR_REVERSE_ARGS (call_expr) = CALL_EXPR_REVERSE_ARGS (aggr_init_expr);
  CALL_EXPR_MUST_TAIL_CALL (call_expr) = AGGR_INIT_EXPR_MUST_TAIL (aggr_init_expr);

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
      call_expr = cp_build_init_expr (slot, call_expr);
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
      init = cp_build_init_expr (slot, init);
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
      && ! DECL_REALLY_EXTERN (fn)
      /* Do not emit thunks for tentative decls, those will be processed
	 again at_eof if really needed.  */
      && (DECL_INTERFACE_KNOWN (fn) || !DECL_DEFER_OUTPUT (fn)))
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
      else if (!at_eof
	       || DECL_IMMEDIATE_FUNCTION_P (fn)
	       || DECL_OMP_DECLARE_REDUCTION_P (fn))
	tentative_decl_linkage (fn);
      else
	import_export_decl (fn);

      /* If the user wants us to keep all inline functions, then mark
	 this function as needed so that finish_file will make sure to
	 output it later.  Similarly, all dllexport'd functions must
	 be emitted; there may be callers in other DLLs.  */
      if (DECL_DECLARED_INLINE_P (fn)
	  && !DECL_REALLY_EXTERN (fn)
	  && !DECL_IMMEDIATE_FUNCTION_P (fn)
	  && !DECL_OMP_DECLARE_REDUCTION_P (fn)
	  && (flag_keep_inline_functions
	      || (flag_keep_inline_dllexport
		  && lookup_attribute ("dllexport", DECL_ATTRIBUTES (fn)))))
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
      /* If this is a constexpr function we still need the body to be
	 able to evaluate it.  Similarly, with modules we only stream
	 the maybe-in-charge cdtor and regenerate the clones from it on
	 demand, so we also need to keep the body.  Otherwise we don't
	 need it anymore.  */
      if (!DECL_DECLARED_CONSTEXPR_P (fn)
	  && !(module_maybe_has_cmi_p () && vague_linkage_p (fn)))
	DECL_SAVED_TREE (fn) = void_node;
      return false;
    }

  /* There's no reason to do any of the work here if we're only doing
     semantic analysis; this code just generates RTL.  */
  if (flag_syntax_only)
    {
      /* Pretend that this function has been written out so that we don't try
	 to expand it again.  */
      TREE_ASM_WRITTEN (fn) = 1;
      return false;
    }

  if (DECL_OMP_DECLARE_REDUCTION_P (fn))
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
      cgraph_node::finalize_function (fn, function_depth > 1);
      emit_associated_thunks (fn);

      function_depth--;

      if (DECL_IMMEDIATE_FUNCTION_P (fn))
	{
	  if (cgraph_node *node = cgraph_node::get (fn))
	    {
	      node->body_removed = true;
	      node->analyzed = false;
	      node->definition = false;
	      node->force_output = false;
	    }
	}
    }
}

class nrv_data
{
public:
  nrv_data () : visited (37) {}

  tree var;
  tree result;
  hash_set<tree> visited;
  bool simple;
  bool in_nrv_cleanup;
};

/* Helper function for walk_tree, used by finalize_nrv below.  */

static tree
finalize_nrv_r (tree* tp, int* walk_subtrees, void* data)
{
  class nrv_data *dp = (class nrv_data *)data;

  /* No need to walk into types.  There wouldn't be any need to walk into
     non-statements, except that we have to consider STMT_EXPRs.  */
  if (TYPE_P (*tp))
    *walk_subtrees = 0;

  /* Replace all uses of the NRV with the RESULT_DECL.  */
  else if (*tp == dp->var)
    *tp = dp->result;

  /* Avoid walking into the same tree more than once.  Unfortunately, we
     can't just use walk_tree_without duplicates because it would only call
     us for the first occurrence of dp->var in the function body.  */
  else if (dp->visited.add (*tp))
    *walk_subtrees = 0;

  /* If there's a label, we might need to destroy the NRV on goto (92407).  */
  else if (TREE_CODE (*tp) == LABEL_EXPR && !dp->in_nrv_cleanup)
    dp->simple = false;
  /* Change NRV returns to just refer to the RESULT_DECL; this is a nop,
     but differs from using NULL_TREE in that it indicates that we care
     about the value of the RESULT_DECL.  But preserve anything appended
     by check_return_expr.  */
  else if (TREE_CODE (*tp) == RETURN_EXPR)
    {
      tree *p = &TREE_OPERAND (*tp, 0);
      while (TREE_CODE (*p) == COMPOUND_EXPR)
	p = &TREE_OPERAND (*p, 0);
      if (TREE_CODE (*p) == INIT_EXPR
	  && INIT_EXPR_NRV_P (*p))
	*p = dp->result;
    }
  /* Change all cleanups for the NRV to only run when not returning.  */
  else if (TREE_CODE (*tp) == CLEANUP_STMT
	   && CLEANUP_DECL (*tp) == dp->var)
    {
      dp->in_nrv_cleanup = true;
      cp_walk_tree (&CLEANUP_BODY (*tp), finalize_nrv_r, data, 0);
      dp->in_nrv_cleanup = false;
      cp_walk_tree (&CLEANUP_EXPR (*tp), finalize_nrv_r, data, 0);
      *walk_subtrees = 0;

      if (dp->simple)
	/* For a simple NRV, just run it on the EH path.  */
	CLEANUP_EH_ONLY (*tp) = true;
      else
	{
	  /* Not simple, we need to check current_retval_sentinel to decide
	     whether to run it.  If it's set, we're returning normally and
	     don't want to destroy the NRV.  If the sentinel is not set, we're
	     leaving scope some other way, either by flowing off the end of its
	     scope or throwing an exception.  */
	  tree cond = build3 (COND_EXPR, void_type_node,
			      current_retval_sentinel,
			      void_node, CLEANUP_EXPR (*tp));
	  CLEANUP_EXPR (*tp) = cond;
	}

      /* If a cleanup might throw, we need to clear current_retval_sentinel on
	 the exception path, both so the check above succeeds and so an outer
	 cleanup added by maybe_splice_retval_cleanup doesn't run.  */
      if (cp_function_chain->throwing_cleanup)
	{
	  tree clear = build2 (MODIFY_EXPR, boolean_type_node,
			       current_retval_sentinel,
			       boolean_false_node);
	  if (dp->simple)
	    {
	      /* We're already only on the EH path, just prepend it.  */
	      tree &exp = CLEANUP_EXPR (*tp);
	      exp = build2 (COMPOUND_EXPR, void_type_node, clear, exp);
	    }
	  else
	    {
	      /* The cleanup runs on both normal and EH paths, we need another
		 CLEANUP_STMT to clear the flag only on the EH path.  */
	      tree &bod = CLEANUP_BODY (*tp);
	      bod = build_stmt (EXPR_LOCATION (*tp), CLEANUP_STMT,
				bod, clear, current_retval_sentinel);
	      CLEANUP_EH_ONLY (bod) = true;
	    }
	}
    }
  /* Disable maybe_splice_retval_cleanup within the NRV cleanup scope, we don't
     want to destroy the retval before the variable goes out of scope.  */
  else if (TREE_CODE (*tp) == CLEANUP_STMT
	   && dp->in_nrv_cleanup
	   && CLEANUP_DECL (*tp) == dp->result)
    CLEANUP_EXPR (*tp) = void_node;
  /* Replace the DECL_EXPR for the NRV with an initialization of the
     RESULT_DECL, if needed.  */
  else if (TREE_CODE (*tp) == DECL_EXPR
	   && DECL_EXPR_DECL (*tp) == dp->var)
    {
      tree init;
      if (DECL_INITIAL (dp->var)
	  && DECL_INITIAL (dp->var) != error_mark_node)
	init = cp_build_init_expr (dp->result,
		       DECL_INITIAL (dp->var));
      else
	init = build_empty_stmt (EXPR_LOCATION (*tp));
      DECL_INITIAL (dp->var) = NULL_TREE;
      SET_EXPR_LOCATION (init, EXPR_LOCATION (*tp));
      *tp = init;
    }

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Called from finish_function to implement the named return value
   optimization by overriding all the RETURN_EXPRs and pertinent
   CLEANUP_STMTs and replacing all occurrences of VAR with RESULT, the
   RESULT_DECL for the function.  */

void
finalize_nrv (tree fndecl, tree var)
{
  class nrv_data data;
  tree result = DECL_RESULT (fndecl);

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
  data.in_nrv_cleanup = false;

  /* This is simpler for variables declared in the outer scope of
     the function so we know that their lifetime always ends with a
     return; see g++.dg/opt/nrv6.C.  */
  tree outer = outer_curly_brace_block (fndecl);
  data.simple = chain_member (var, BLOCK_VARS (outer));

  cp_walk_tree (&DECL_SAVED_TREE (fndecl), finalize_nrv_r, &data, 0);
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

/* If DECL is DECL_OMP_PRIVATIZED_MEMBER, return corresponding
   FIELD_DECL, otherwise return DECL itself.  */

static tree
omp_clause_decl_field (tree decl)
{
  if (VAR_P (decl)
      && DECL_HAS_VALUE_EXPR_P (decl)
      && DECL_ARTIFICIAL (decl)
      && DECL_LANG_SPECIFIC (decl)
      && DECL_OMP_PRIVATIZED_MEMBER (decl))
    {
      tree f = DECL_VALUE_EXPR (decl);
      if (INDIRECT_REF_P (f))
	f = TREE_OPERAND (f, 0);
      if (TREE_CODE (f) == COMPONENT_REF)
	{
	  f = TREE_OPERAND (f, 1);
	  gcc_assert (TREE_CODE (f) == FIELD_DECL);
	  return f;
	}
    }
  return NULL_TREE;
}

/* Adjust DECL if needed for printing using %qE.  */

static tree
omp_clause_printable_decl (tree decl)
{
  tree t = omp_clause_decl_field (decl);
  if (t)
    return t;
  return decl;
}

/* For a FIELD_DECL F and corresponding DECL_OMP_PRIVATIZED_MEMBER
   VAR_DECL T that doesn't need a DECL_EXPR added, record it for
   privatization.  */

static void
omp_note_field_privatization (tree f, tree t)
{
  if (!omp_private_member_map)
    omp_private_member_map = new hash_map<tree, tree>;
  tree &v = omp_private_member_map->get_or_insert (f);
  if (v == NULL_TREE)
    {
      v = t;
      omp_private_member_vec.safe_push (f);
      /* Signal that we don't want to create DECL_EXPR for this dummy var.  */
      omp_private_member_vec.safe_push (integer_zero_node);
    }
}

/* Privatize FIELD_DECL T, return corresponding DECL_OMP_PRIVATIZED_MEMBER
   dummy VAR_DECL.  */

tree
omp_privatize_field (tree t, bool shared)
{
  tree m = finish_non_static_data_member (t, NULL_TREE, NULL_TREE);
  if (m == error_mark_node)
    return error_mark_node;
  if (!omp_private_member_map && !shared)
    omp_private_member_map = new hash_map<tree, tree>;
  if (TYPE_REF_P (TREE_TYPE (t)))
    {
      gcc_assert (INDIRECT_REF_P (m));
      m = TREE_OPERAND (m, 0);
    }
  tree vb = NULL_TREE;
  tree &v = shared ? vb : omp_private_member_map->get_or_insert (t);
  if (v == NULL_TREE)
    {
      v = create_temporary_var (TREE_TYPE (m));
      retrofit_lang_decl (v);
      DECL_OMP_PRIVATIZED_MEMBER (v) = 1;
      SET_DECL_VALUE_EXPR (v, m);
      DECL_HAS_VALUE_EXPR_P (v) = 1;
      if (!shared)
	omp_private_member_vec.safe_push (t);
    }
  return v;
}

/* C++ specialisation of the c_omp_address_inspector class.  */

class cp_omp_address_inspector : public c_omp_address_inspector
{
public:
  cp_omp_address_inspector (location_t loc, tree t)
    : c_omp_address_inspector (loc, t)
    {
    }

  ~cp_omp_address_inspector ()
    {
    }

  bool processing_template_decl_p ()
    {
      return processing_template_decl;
    }

  void emit_unmappable_type_notes (tree t)
    {
      if (TREE_TYPE (t) != error_mark_node
	  && !COMPLETE_TYPE_P (TREE_TYPE (t)))
	cxx_incomplete_type_inform (TREE_TYPE (t));
    }

  tree convert_from_reference (tree x)
    {
      return ::convert_from_reference (x);
    }

  tree build_array_ref (location_t loc, tree arr, tree idx)
    {
      return ::build_array_ref (loc, arr, idx);
    }

  bool check_clause (tree clause)
    {
      if (TREE_CODE (orig) == COMPONENT_REF
	  && invalid_nonstatic_memfn_p (EXPR_LOCATION (orig), orig,
					tf_warning_or_error))
	return false;
      if (!c_omp_address_inspector::check_clause (clause))
	return false;
      return true;
    }
};

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
   first one known not to have length 1.  For array-section-subscript
   <= FIRST_NON_ONE we diagnose non-contiguous arrays if low bound isn't
   0 or length isn't the array domain max + 1, for > FIRST_NON_ONE we
   can if MAYBE_ZERO_LEN is false.  MAYBE_ZERO_LEN will be true in the above
   case though, as some lengths could be zero.  */

static tree
handle_omp_array_sections_1 (tree c, tree t, vec<tree> &types,
			     bool &maybe_zero_len, unsigned int &first_non_one,
			     enum c_omp_region_type ort)
{
  tree ret, low_bound, length, type;
  bool openacc = (ort & C_ORT_ACC) != 0;
  if (TREE_CODE (t) != OMP_ARRAY_SECTION)
    {
      if (error_operand_p (t))
	return error_mark_node;

      cp_omp_address_inspector ai (OMP_CLAUSE_LOCATION (c), t);
      tree t_refto = ai.maybe_unconvert_ref (t);

      if (!ai.check_clause (c))
	return error_mark_node;
      else if (ai.component_access_p ()
	       && (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_TO
		   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FROM))
	t = ai.get_root_term (true);
      else
	t = ai.unconverted_ref_origin ();
      if (t == error_mark_node)
	return error_mark_node;
      ret = t_refto;
      if (TREE_CODE (t) == FIELD_DECL)
	ret = finish_non_static_data_member (t, NULL_TREE, NULL_TREE);
      else if (!VAR_P (t)
	       && (openacc || !EXPR_P (t))
	       && TREE_CODE (t) != PARM_DECL)
	{
	  if (processing_template_decl && TREE_CODE (t) != OVERLOAD)
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
      else if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_AFFINITY
	       && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND
	       && VAR_P (t) && CP_DECL_THREAD_LOCAL_P (t))
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "%qD is threadprivate variable in %qs clause", t,
		    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	  return error_mark_node;
	}
      if (type_dependent_expression_p (ret))
	return NULL_TREE;
      ret = convert_from_reference (ret);
      return ret;
    }

  if ((ort & C_ORT_OMP_DECLARE_SIMD) == C_ORT_OMP
      && (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	  || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION
	  || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_TASK_REDUCTION)
      && TREE_CODE (TREE_OPERAND (t, 0)) == FIELD_DECL)
    TREE_OPERAND (t, 0) = omp_privatize_field (TREE_OPERAND (t, 0), false);
  ret = handle_omp_array_sections_1 (c, TREE_OPERAND (t, 0), types,
				     maybe_zero_len, first_non_one, ort);
  if (ret == error_mark_node || ret == NULL_TREE)
    return ret;

  type = TREE_TYPE (ret);
  low_bound = TREE_OPERAND (t, 1);
  length = TREE_OPERAND (t, 2);
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
  if (low_bound)
    low_bound = mark_rvalue_use (low_bound);
  if (length)
    length = mark_rvalue_use (length);
  /* We need to reduce to real constant-values for checks below.  */
  if (length)
    length = fold_simple (length);
  if (low_bound)
    low_bound = fold_simple (low_bound);
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

  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
      && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH
	  || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_DETACH))
    {
      if (length != integer_one_node)
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "expected single pointer in %qs clause",
		    user_omp_clause_code_name (c, openacc));
	  return error_mark_node;
	}
    }
  if (length != NULL_TREE)
    {
      if (!integer_nonzerop (length))
	{
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_AFFINITY
	      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
	      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION
	      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_TASK_REDUCTION)
	    {
	      if (integer_zerop (length))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "zero length array section in %qs clause",
			    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		  return error_mark_node;
		}
	    }
	  else
	    maybe_zero_len = true;
	}
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
	  tree size
	    = fold_convert (sizetype, TYPE_MAX_VALUE (TYPE_DOMAIN (type)));
	  size = size_binop (PLUS_EXPR, size, size_one_node);
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
		{
		  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_AFFINITY
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_TASK_REDUCTION)
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"zero length array section in %qs clause",
				omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		      return error_mark_node;
		    }
		  maybe_zero_len = true;
		}
	      else if (length == NULL_TREE
		       && first_non_one == types.length ()
		       && tree_int_cst_equal
			    (TYPE_MAX_VALUE (TYPE_DOMAIN (type)),
			     low_bound))
		first_non_one++;
	    }
	  else if (length == NULL_TREE)
	    {
	      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_AFFINITY
		  && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND
		  && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_REDUCTION
		  && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_IN_REDUCTION
		  && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_TASK_REDUCTION)
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
	  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_AFFINITY
	      && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND
	      && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_REDUCTION
	      && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_IN_REDUCTION
	      && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_TASK_REDUCTION)
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
	      TREE_OPERAND (t, 1) = lb;
	      low_bound = lb;
	    }
	}
    }
  else if (TYPE_PTR_P (type))
    {
      if (length == NULL_TREE)
	{
	  if (TREE_CODE (ret) == PARM_DECL && DECL_ARRAY_PARAMETER_P (ret))
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "for array function parameter length expression "
		      "must be specified");
	  else
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "for pointer type length expression must be specified");
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
      /* If there is a pointer type anywhere but in the very first
	 array-section-subscript, the array section could be non-contiguous.  */
      if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_AFFINITY
	  && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_DEPEND
	  && TREE_CODE (TREE_OPERAND (t, 0)) == OMP_ARRAY_SECTION)
	{
	  /* If any prior dimension has a non-one length, then deem this
	     array section as non-contiguous.  */
	  for (tree d = TREE_OPERAND (t, 0); TREE_CODE (d) == OMP_ARRAY_SECTION;
	       d = TREE_OPERAND (d, 0))
	    {
	      tree d_length = TREE_OPERAND (d, 2);
	      if (d_length == NULL_TREE || !integer_onep (d_length))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "array section is not contiguous in %qs clause",
			    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		  return error_mark_node;
		}
	    }
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
      TREE_OPERAND (t, 1) = lb;
      low_bound = lb;
    }
  /* Temporarily disable -fstrong-eval-order for array reductions.
     The SAVE_EXPR and COMPOUND_EXPR added if low_bound has side-effects
     is something the middle-end can't cope with and more importantly,
     it needs to be the actual base variable that is privatized, not some
     temporary assigned previous value of it.  That, together with OpenMP
     saying how many times the side-effects are evaluated is unspecified,
     makes int *a, *b; ... reduction(+:a[a = b, 3:10]) really unspecified.  */
  warning_sentinel s (flag_strong_eval_order,
		      OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_TASK_REDUCTION);
  ret = grok_array_decl (OMP_CLAUSE_LOCATION (c), ret, low_bound, NULL,
			 tf_warning_or_error);
  return ret;
}

/* Handle array sections for clause C.  */

static bool
handle_omp_array_sections (tree &c, enum c_omp_region_type ort)
{
  bool maybe_zero_len = false;
  unsigned int first_non_one = 0;
  auto_vec<tree, 10> types;
  tree *tp = &OMP_CLAUSE_DECL (c);
  if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
       || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_AFFINITY)
      && TREE_CODE (*tp) == TREE_LIST
      && TREE_PURPOSE (*tp)
      && TREE_CODE (TREE_PURPOSE (*tp)) == TREE_VEC)
    tp = &TREE_VALUE (*tp);
  tree first = handle_omp_array_sections_1 (c, *tp, types,
					    maybe_zero_len, first_non_one,
					    ort);
  if (first == error_mark_node)
    return true;
  if (first == NULL_TREE)
    return false;
  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_AFFINITY)
    {
      tree t = *tp;
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
      *tp = first;
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
	   t = TREE_OPERAND (t, 0))
	{
	  gcc_assert (TREE_CODE (t) == OMP_ARRAY_SECTION);

	  tree low_bound = TREE_OPERAND (t, 1);
	  tree length = TREE_OPERAND (t, 2);

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

	      if (i > first_non_one
		  && ((length && integer_nonzerop (length))
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_TASK_REDUCTION))
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
		  tree eltype = TREE_TYPE (types[num - 1]);
		  while (TREE_CODE (eltype) == ARRAY_TYPE)
		    eltype = TREE_TYPE (eltype);
		  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_TASK_REDUCTION)
		    size = size_binop (EXACT_DIV_EXPR, size,
				       size_in_bytes (eltype));
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
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION
	      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_TASK_REDUCTION)
	    {
	      size = size_binop (MINUS_EXPR, size, size_one_node);
	      size = save_expr (size);
	      tree index_type = build_index_type (size);
	      tree eltype = TREE_TYPE (first);
	      while (TREE_CODE (eltype) == ARRAY_TYPE)
		eltype = TREE_TYPE (eltype);
	      tree type = build_array_type (eltype, index_type);
	      tree ptype = build_pointer_type (eltype);
	      if (TYPE_REF_P (TREE_TYPE (t))
		  && INDIRECT_TYPE_P (TREE_TYPE (TREE_TYPE (t))))
		t = convert_from_reference (t);
	      else if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
		t = build_fold_addr_expr (t);
	      tree t2 = build_fold_addr_expr (first);
	      t2 = fold_convert_loc (OMP_CLAUSE_LOCATION (c),
				     ptrdiff_type_node, t2);
	      t2 = fold_build2_loc (OMP_CLAUSE_LOCATION (c), MINUS_EXPR,
				    ptrdiff_type_node, t2,
				    fold_convert_loc (OMP_CLAUSE_LOCATION (c),
						      ptrdiff_type_node, t));
	      if (tree_fits_shwi_p (t2))
		t = build2 (MEM_REF, type, t,
			    build_int_cst (ptype, tree_to_shwi (t2)));
	      else
		{
		  t2 = fold_convert_loc (OMP_CLAUSE_LOCATION (c),
					 sizetype, t2);
		  t = build2_loc (OMP_CLAUSE_LOCATION (c), POINTER_PLUS_EXPR,
				  TREE_TYPE (t), t, t2);
		  t = build2 (MEM_REF, type, t, build_int_cst (ptype, 0));
		}
	      OMP_CLAUSE_DECL (c) = t;
	      return false;
	    }
	  OMP_CLAUSE_DECL (c) = first;
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_HAS_DEVICE_ADDR)
	    return false;
	  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP
	      || (OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_ATTACH
		  && OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_DETACH
		  && OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_FORCE_DETACH))
	    OMP_CLAUSE_SIZE (c) = size;
	  if (TREE_CODE (t) == FIELD_DECL)
	    t = finish_non_static_data_member (t, NULL_TREE, NULL_TREE);

	  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP)
	    return false;

	  if (TREE_CODE (first) == INDIRECT_REF)
	    {
	      /* Detect and skip adding extra nodes for pointer-to-member
		 mappings.  These are unsupported for now.  */
	      tree tmp = TREE_OPERAND (first, 0);

	      if (TREE_CODE (tmp) == NON_LVALUE_EXPR)
		tmp = TREE_OPERAND (tmp, 0);

	      if (TREE_CODE (tmp) == INDIRECT_REF)
		tmp = TREE_OPERAND (tmp, 0);

	      if (TREE_CODE (tmp) == POINTER_PLUS_EXPR)
		{
		  tree offset = TREE_OPERAND (tmp, 1);
		  STRIP_NOPS (offset);
		  if (TYPE_PTRMEM_P (TREE_TYPE (offset)))
		    {
		      sorry_at (OMP_CLAUSE_LOCATION (c),
				"pointer-to-member mapping %qE not supported",
				OMP_CLAUSE_DECL (c));
		      return true;
		    }
		}
	    }

	  /* FIRST represents the first item of data that we are mapping.
	     E.g. if we're mapping an array, FIRST might resemble
	     "foo.bar.myarray[0]".  */

	  auto_vec<omp_addr_token *, 10> addr_tokens;

	  if (!omp_parse_expr (addr_tokens, first))
	    return true;

	  cp_omp_address_inspector ai (OMP_CLAUSE_LOCATION (c), t);

	  tree nc = ai.expand_map_clause (c, first, addr_tokens, ort);
	  if (nc != error_mark_node)
	    {
	      using namespace omp_addr_tokenizer;

	      if (ai.maybe_zero_length_array_section (c))
		OMP_CLAUSE_MAP_MAYBE_ZERO_LENGTH_ARRAY_SECTION (c) = 1;

	      /* !!! If we're accessing a base decl via chained access
		 methods (e.g. multiple indirections), duplicate clause
		 detection won't work properly.  Skip it in that case.  */
	      if ((addr_tokens[0]->type == STRUCTURE_BASE
		   || addr_tokens[0]->type == ARRAY_BASE)
		  && addr_tokens[0]->u.structure_base_kind == BASE_DECL
		  && addr_tokens[1]->type == ACCESS_METHOD
		  && omp_access_chain_p (addr_tokens, 1))
		c = nc;

	      return false;
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
      reduction_id = ovl_op_identifier (false, reduction_code);
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
	  id = perform_koenig_lookup (id, args, tf_none);
	}
    }
  else if (TREE_CODE (id) == SCOPE_REF)
    id = lookup_qualified_name (TREE_OPERAND (id, 0),
				omp_reduction_id (ERROR_MARK,
						  TREE_OPERAND (id, 1),
						  type),
				LOOK_want::NORMAL, false);
  tree fns = id;
  id = NULL_TREE;
  if (fns && is_overloaded_fn (fns))
    {
      for (lkp_iterator iter (get_fns (fns)); iter; ++iter)
	{
	  tree fndecl = *iter;
	  if (TREE_CODE (fndecl) == FUNCTION_DECL)
	    {
	      tree argtype = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
	      if (same_type_p (TREE_TYPE (argtype), type))
		{
		  id = fndecl;
		  break;
		}
	    }
	}

      if (id && BASELINK_P (fns))
	{
	  if (baselinkp)
	    *baselinkp = fns;
	  else
	    baselink = fns;
	}
    }

  if (!id && CLASS_TYPE_P (type) && TYPE_BINFO (type))
    {
      auto_vec<tree> ambiguous;
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
	  auto_diagnostic_group d;
	  const char *str = _("candidates are:");
	  unsigned int idx;
	  tree udr;
	  error_at (loc, "user defined reduction lookup is ambiguous");
	  FOR_EACH_VEC_ELT (ambiguous, idx, udr)
	    {
	      inform (DECL_SOURCE_LOCATION (udr), "%s %#qD", str, udr);
	      if (idx == 0)
		str = get_spaces (str);
	    }
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

bool
cp_check_omp_declare_reduction (tree udr)
{
  tree type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (udr)));
  gcc_assert (TYPE_REF_P (type));
  type = TREE_TYPE (type);
  int i;
  location_t loc = DECL_SOURCE_LOCATION (udr);

  if (type == error_mark_node)
    return false;
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
	  return false;
	}
    }
  else if (FUNC_OR_METHOD_TYPE_P (type)
	   || TREE_CODE (type) == ARRAY_TYPE)
    {
      error_at (loc, "function or array type %qT in "
		     "%<#pragma omp declare reduction%>", type);
      return false;
    }
  else if (TYPE_REF_P (type))
    {
      error_at (loc, "reference type %qT in %<#pragma omp declare reduction%>",
		type);
      return false;
    }
  else if (TYPE_QUALS_NO_ADDR_SPACE (type))
    {
      error_at (loc, "%<const%>, %<volatile%> or %<__restrict%>-qualified "
		"type %qT in %<#pragma omp declare reduction%>", type);
      return false;
    }

  tree body = DECL_SAVED_TREE (udr);
  if (body == NULL_TREE || TREE_CODE (body) != STATEMENT_LIST)
    return true;

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
      if (warning_suppressed_p (DECL_EXPR_DECL (data.stmts[0]) /* What warning? */))
	return true;
      data.combiner_p = true;
      if (cp_walk_tree (&data.stmts[2], cp_check_omp_declare_reduction_r,
			&data, NULL))
	suppress_warning (DECL_EXPR_DECL (data.stmts[0]) /* What warning? */);
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
	suppress_warning (DECL_EXPR_DECL (data.stmts[0])  /* Wat warning? */);
      if (i == 7)
	gcc_assert (TREE_CODE (data.stmts[6]) == DECL_EXPR);
    }
  return true;
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
  hash_map<tree, tree> decl_map;

  decl_map.put (omp_decl1, placeholder);
  decl_map.put (omp_decl2, decl);
  memset (&id, 0, sizeof (id));
  id.src_fn = DECL_CONTEXT (omp_decl1);
  id.dst_fn = current_function_decl;
  id.src_cfun = DECL_STRUCT_FUNCTION (id.src_fn);
  id.decl_map = &decl_map;

  id.copy_decl = copy_decl_no_change;
  id.transform_call_graph_edges = CB_CGE_DUPLICATE;
  id.transform_new_cfg = true;
  id.transform_return_to_modify = false;
  id.eh_lp_nr = 0;
  walk_tree (&stmt, copy_tree_body_r, &id, NULL);
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
  if (TREE_CODE (t) == TREE_LIST)
    {
      gcc_assert (processing_template_decl);
      return false;
    }
  tree type = TREE_TYPE (t);
  if (TREE_CODE (t) == MEM_REF)
    type = TREE_TYPE (type);
  if (TYPE_REF_P (type))
    type = TREE_TYPE (type);
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree oatype = type;
      gcc_assert (TREE_CODE (t) != MEM_REF);
      while (TREE_CODE (type) == ARRAY_TYPE)
	type = TREE_TYPE (type);
      if (!processing_template_decl)
	{
	  t = require_complete_type (t);
	  if (t == error_mark_node
	      || !complete_type_or_else (oatype, NULL_TREE))
	    return true;
	  tree size = size_binop (EXACT_DIV_EXPR, TYPE_SIZE_UNIT (oatype),
				  TYPE_SIZE_UNIT (type));
	  if (integer_zerop (size))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qE in %<reduction%> clause is a zero size array",
			omp_clause_printable_decl (t));
	      return true;
	    }
	  size = size_binop (MINUS_EXPR, size, size_one_node);
	  size = save_expr (size);
	  tree index_type = build_index_type (size);
	  tree atype = build_array_type (type, index_type);
	  tree ptype = build_pointer_type (type);
	  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	    t = build_fold_addr_expr (t);
	  t = build2 (MEM_REF, atype, t, build_int_cst (ptype, 0));
	  OMP_CLAUSE_DECL (c) = t;
	}
    }
  if (type == error_mark_node)
    return true;
  else if (ARITHMETIC_TYPE_P (type))
    switch (OMP_CLAUSE_REDUCTION_CODE (c))
      {
      case PLUS_EXPR:
      case MULT_EXPR:
      case MINUS_EXPR:
      case TRUTH_ANDIF_EXPR:
      case TRUTH_ORIF_EXPR:
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
	if (FLOAT_TYPE_P (type) || TREE_CODE (type) == COMPLEX_TYPE)
	  break;
	predefined = true;
	break;
      default:
	break;
      }
  else if (TYPE_READONLY (type))
    {
      error_at (OMP_CLAUSE_LOCATION (c),
		"%qE has const type for %<reduction%>",
		omp_clause_printable_decl (t));
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
    {
      if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) == error_mark_node)
	return true;
      return false;
    }

  tree id = OMP_CLAUSE_REDUCTION_PLACEHOLDER (c);

  type = TYPE_MAIN_VARIANT (type);
  OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = NULL_TREE;
  if (id == NULL_TREE)
    id = omp_reduction_id (OMP_CLAUSE_REDUCTION_CODE (c),
			   NULL_TREE, NULL_TREE);
  id = omp_reduction_lookup (OMP_CLAUSE_LOCATION (c), id, type, NULL, NULL);
  if (id)
    {
      if (id == error_mark_node)
	return true;
      mark_used (id);
      tree body = DECL_SAVED_TREE (id);
      if (!body)
	return true;
      if (TREE_CODE (body) == STATEMENT_LIST)
	{
	  tree_stmt_iterator tsi;
	  tree placeholder = NULL_TREE, decl_placeholder = NULL_TREE;
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
	      if (TREE_CODE (t) == MEM_REF)
		{
		  decl_placeholder = build_lang_decl (VAR_DECL, NULL_TREE,
						      type);
		  DECL_ARTIFICIAL (decl_placeholder) = 1;
		  DECL_IGNORED_P (decl_placeholder) = 1;
		  OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (c) = decl_placeholder;
		}
	      if (TREE_ADDRESSABLE (DECL_EXPR_DECL (stmts[0])))
		cxx_mark_addressable (placeholder);
	      if (TREE_ADDRESSABLE (DECL_EXPR_DECL (stmts[1]))
		  && (decl_placeholder
		      || !TYPE_REF_P (TREE_TYPE (OMP_CLAUSE_DECL (c)))))
		cxx_mark_addressable (decl_placeholder ? decl_placeholder
				      : OMP_CLAUSE_DECL (c));
	      tree omp_out = placeholder;
	      tree omp_in = decl_placeholder ? decl_placeholder
			    : convert_from_reference (OMP_CLAUSE_DECL (c));
	      if (need_static_cast)
		{
		  tree rtype = build_reference_type (atype);
		  omp_out = build_static_cast (input_location,
					       rtype, omp_out,
					       tf_warning_or_error);
		  omp_in = build_static_cast (input_location,
					      rtype, omp_in,
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
	      if (TREE_ADDRESSABLE (DECL_EXPR_DECL (stmts[3]))
		  && (decl_placeholder
		      || !TYPE_REF_P (TREE_TYPE (OMP_CLAUSE_DECL (c)))))
		cxx_mark_addressable (decl_placeholder ? decl_placeholder
				      : OMP_CLAUSE_DECL (c));
	      if (TREE_ADDRESSABLE (DECL_EXPR_DECL (stmts[4])))
		cxx_mark_addressable (placeholder);
	      tree omp_priv = decl_placeholder ? decl_placeholder
			      : convert_from_reference (OMP_CLAUSE_DECL (c));
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
		  omp_priv = build_static_cast (input_location,
						rtype, omp_priv,
						tf_warning_or_error);
		  omp_orig = build_static_cast (input_location,
						rtype, omp_orig,
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
		  tree v = decl_placeholder ? decl_placeholder
			   : convert_from_reference (t);
		  if (AGGREGATE_TYPE_P (TREE_TYPE (v)))
		    init = build_constructor (TREE_TYPE (v), NULL);
		  else
		    init = fold_convert (TREE_TYPE (v), integer_zero_node);
		  OMP_CLAUSE_REDUCTION_INIT (c)
		    = cp_build_init_expr (v, init);
		}
	    }
	}
    }
  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (c))
    *need_dtor = true;
  else
    {
      error_at (OMP_CLAUSE_LOCATION (c),
		"user defined reduction not found for %qE",
		omp_clause_printable_decl (t));
      return true;
    }
  if (TREE_CODE (OMP_CLAUSE_DECL (c)) == MEM_REF)
    gcc_assert (TYPE_SIZE_UNIT (type)
		&& TREE_CODE (TYPE_SIZE_UNIT (type)) == INTEGER_CST);
  return false;
}

/* Called from finish_struct_1.  linear(this) or linear(this:step)
   clauses might not be finalized yet because the class has been incomplete
   when parsing #pragma omp declare simd methods.  Fix those up now.  */

void
finish_omp_declare_simd_methods (tree t)
{
  if (processing_template_decl)
    return;

  for (tree x = TYPE_FIELDS (t); x; x = DECL_CHAIN (x))
    {
      if (TREE_CODE (x) == USING_DECL
	  || !DECL_IOBJ_MEMBER_FUNCTION_P (x))
	continue;
      tree ods = lookup_attribute ("omp declare simd", DECL_ATTRIBUTES (x));
      if (!ods || !TREE_VALUE (ods))
	continue;
      for (tree c = TREE_VALUE (TREE_VALUE (ods)); c; c = OMP_CLAUSE_CHAIN (c))
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
	    && integer_zerop (OMP_CLAUSE_DECL (c))
	    && OMP_CLAUSE_LINEAR_STEP (c)
	    && TYPE_PTR_P (TREE_TYPE (OMP_CLAUSE_LINEAR_STEP (c))))
	  {
	    tree s = OMP_CLAUSE_LINEAR_STEP (c);
	    s = fold_convert_loc (OMP_CLAUSE_LOCATION (c), sizetype, s);
	    s = fold_build2_loc (OMP_CLAUSE_LOCATION (c), MULT_EXPR,
				 sizetype, s, TYPE_SIZE_UNIT (t));
	    OMP_CLAUSE_LINEAR_STEP (c) = s;
	  }
    }
}

/* Adjust sink depend/doacross clause to take into account pointer offsets.

   Return TRUE if there was a problem processing the offset, and the
   whole clause should be removed.  */

static bool
cp_finish_omp_clause_doacross_sink (tree sink_clause)
{
  tree t = OMP_CLAUSE_DECL (sink_clause);
  gcc_assert (TREE_CODE (t) == TREE_LIST);

  /* Make sure we don't adjust things twice for templates.  */
  if (processing_template_decl)
    return false;

  for (; t; t = TREE_CHAIN (t))
    {
      tree decl = TREE_VALUE (t);
      if (TYPE_PTR_P (TREE_TYPE (decl)))
	{
	  tree offset = TREE_PURPOSE (t);
	  bool neg = wi::neg_p (wi::to_wide (offset));
	  offset = fold_unary (ABS_EXPR, TREE_TYPE (offset), offset);
	  decl = mark_rvalue_use (decl);
	  decl = convert_from_reference (decl);
	  tree t2 = pointer_int_sum (OMP_CLAUSE_LOCATION (sink_clause),
				     neg ? MINUS_EXPR : PLUS_EXPR,
				     decl, offset);
	  t2 = fold_build2_loc (OMP_CLAUSE_LOCATION (sink_clause),
				MINUS_EXPR, sizetype,
				fold_convert (sizetype, t2),
				fold_convert (sizetype, decl));
	  if (t2 == error_mark_node)
	    return true;
	  TREE_PURPOSE (t) = t2;
	}
    }
  return false;
}

/* Finish OpenMP iterators ITER.  Return true if they are errorneous
   and clauses containing them should be removed.  */

static bool
cp_omp_finish_iterators (tree iter)
{
  bool ret = false;
  for (tree it = iter; it; it = TREE_CHAIN (it))
    {
      tree var = TREE_VEC_ELT (it, 0);
      tree begin = TREE_VEC_ELT (it, 1);
      tree end = TREE_VEC_ELT (it, 2);
      tree step = TREE_VEC_ELT (it, 3);
      tree orig_step;
      tree type = TREE_TYPE (var);
      location_t loc = DECL_SOURCE_LOCATION (var);
      if (type == error_mark_node)
	{
	  ret = true;
	  continue;
	}
      if (type_dependent_expression_p (var))
	continue;
      if (!INTEGRAL_TYPE_P (type) && !POINTER_TYPE_P (type))
	{
	  error_at (loc, "iterator %qD has neither integral nor pointer type",
		    var);
	  ret = true;
	  continue;
	}
      else if (TYPE_READONLY (type))
	{
	  error_at (loc, "iterator %qD has const qualified type", var);
	  ret = true;
	  continue;
	}
      if (type_dependent_expression_p (begin)
	  || type_dependent_expression_p (end)
	  || type_dependent_expression_p (step))
	continue;
      else if (error_operand_p (step))
	{
	  ret = true;
	  continue;
	}
      else if (!INTEGRAL_TYPE_P (TREE_TYPE (step)))
	{
	  error_at (EXPR_LOC_OR_LOC (step, loc),
		    "iterator step with non-integral type");
	  ret = true;
	  continue;
	}

      begin = mark_rvalue_use (begin);
      end = mark_rvalue_use (end);
      step = mark_rvalue_use (step);
      begin = cp_build_c_cast (input_location, type, begin,
			       tf_warning_or_error);
      end = cp_build_c_cast (input_location, type, end,
			     tf_warning_or_error);
      orig_step = step;
      if (!processing_template_decl)
	step = orig_step = save_expr (step);
      tree stype = POINTER_TYPE_P (type) ? sizetype : type;
      step = cp_build_c_cast (input_location, stype, step,
			      tf_warning_or_error);
      if (POINTER_TYPE_P (type) && !processing_template_decl)
	{
	  begin = save_expr (begin);
	  step = pointer_int_sum (loc, PLUS_EXPR, begin, step);
	  step = fold_build2_loc (loc, MINUS_EXPR, sizetype,
				  fold_convert (sizetype, step),
				  fold_convert (sizetype, begin));
	  step = fold_convert (ssizetype, step);
	}
      if (!processing_template_decl)
	{
	  begin = maybe_constant_value (begin);
	  end = maybe_constant_value (end);
	  step = maybe_constant_value (step);
	  orig_step = maybe_constant_value (orig_step);
	}
      if (integer_zerop (step))
	{
	  error_at (loc, "iterator %qD has zero step", var);
	  ret = true;
	  continue;
	}

      if (begin == error_mark_node
	  || end == error_mark_node
	  || step == error_mark_node
	  || orig_step == error_mark_node)
	{
	  ret = true;
	  continue;
	}

      if (!processing_template_decl)
	{
	  begin = fold_build_cleanup_point_expr (TREE_TYPE (begin), begin);
	  end = fold_build_cleanup_point_expr (TREE_TYPE (end), end);
	  step = fold_build_cleanup_point_expr (TREE_TYPE (step), step);
	  orig_step = fold_build_cleanup_point_expr (TREE_TYPE (orig_step),
						     orig_step);
	}
      hash_set<tree> pset;
      tree it2;
      for (it2 = TREE_CHAIN (it); it2; it2 = TREE_CHAIN (it2))
	{
	  tree var2 = TREE_VEC_ELT (it2, 0);
	  tree begin2 = TREE_VEC_ELT (it2, 1);
	  tree end2 = TREE_VEC_ELT (it2, 2);
	  tree step2 = TREE_VEC_ELT (it2, 3);
	  location_t loc2 = DECL_SOURCE_LOCATION (var2);
	  if (cp_walk_tree (&begin2, find_omp_placeholder_r, var, &pset))
	    {
	      error_at (EXPR_LOC_OR_LOC (begin2, loc2),
			"begin expression refers to outer iterator %qD", var);
	      break;
	    }
	  else if (cp_walk_tree (&end2, find_omp_placeholder_r, var, &pset))
	    {
	      error_at (EXPR_LOC_OR_LOC (end2, loc2),
			"end expression refers to outer iterator %qD", var);
	      break;
	    }
	  else if (cp_walk_tree (&step2, find_omp_placeholder_r, var, &pset))
	    {
	      error_at (EXPR_LOC_OR_LOC (step2, loc2),
			"step expression refers to outer iterator %qD", var);
	      break;
	    }
	}
      if (it2)
	{
	  ret = true;
	  continue;
	}
      TREE_VEC_ELT (it, 1) = begin;
      TREE_VEC_ELT (it, 2) = end;
      if (processing_template_decl)
	TREE_VEC_ELT (it, 3) = orig_step;
      else
	{
	  TREE_VEC_ELT (it, 3) = step;
	  TREE_VEC_ELT (it, 4) = orig_step;
	}
    }
  return ret;
}

/* Ensure that pointers are used in OpenACC attach and detach clauses.
   Return true if an error has been detected.  */

static bool
cp_oacc_check_attachments (tree c)
{
  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP)
    return false;

  /* OpenACC attach / detach clauses must be pointers.  */
  if (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH
      || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_DETACH)
    {
      tree t = OMP_CLAUSE_DECL (c);
      tree type;

      while (TREE_CODE (t) == OMP_ARRAY_SECTION)
	t = TREE_OPERAND (t, 0);

      type = TREE_TYPE (t);

      if (TREE_CODE (type) == REFERENCE_TYPE)
	type = TREE_TYPE (type);

      if (TREE_CODE (type) != POINTER_TYPE)
	{
	  error_at (OMP_CLAUSE_LOCATION (c), "expected pointer in %qs clause",
		    user_omp_clause_code_name (c, true));
	  return true;
	}
    }

  return false;
}

/* Update OMP_CLAUSE_INIT_PREFER_TYPE in case template substitution
   happened.  */

tree
cp_finish_omp_init_prefer_type (tree pref_type)
{
  if (processing_template_decl
      || pref_type == NULL_TREE
      || TREE_CODE (pref_type) != TREE_LIST)
    return pref_type;

  tree t = TREE_PURPOSE (pref_type);
  char *str = const_cast<char *> (TREE_STRING_POINTER (t));
  tree fr_list = TREE_VALUE (pref_type);
  int len = TREE_VEC_LENGTH (fr_list);
  int cnt = 0;

  while (str[0] == (char) GOMP_INTEROP_IFR_SEPARATOR)
    {
      str++;
      if (str[0] == (char) GOMP_INTEROP_IFR_UNKNOWN)
	{
	  /* Assume a no or a single 'fr'.  */
	  gcc_checking_assert (str[1] == (char) GOMP_INTEROP_IFR_SEPARATOR);
	  location_t loc = UNKNOWN_LOCATION;
	  tree value = TREE_VEC_ELT (fr_list, cnt);
	  if (value != NULL_TREE && value != error_mark_node)
	    {
	      loc = EXPR_LOCATION (value);
	      if (value && TREE_CODE (value) == NOP_EXPR)
		value = TREE_OPERAND (value, 0);
	      value = cp_fully_fold (value);
	    }
	  if (value != NULL_TREE && value != error_mark_node)
	    {
	      if (TREE_CODE (value) != INTEGER_CST
		  || !tree_fits_shwi_p (value))
		error_at (loc,
			  "expected string literal or "
			  "constant integer expression instead of %qE", value);
	      else
		{
		  HOST_WIDE_INT n = tree_to_shwi (value);
		  if (n < 1 || n > GOMP_INTEROP_IFR_LAST)
		    {
		      warning_at (loc, OPT_Wopenmp,
				  "unknown foreign runtime identifier %qwd", n);
		      n = GOMP_INTEROP_IFR_UNKNOWN;
		    }
		  str[0] = (char) n;
		}
	    }
	  str++;
	}
      else if (str[0] != (char) GOMP_INTEROP_IFR_SEPARATOR)
	{
	  /* Assume a no or a single 'fr'.  */
	  gcc_checking_assert (str[1] == (char) GOMP_INTEROP_IFR_SEPARATOR);
	  str++;
	}
      str++;
      while (str[0] != '\0')
	str += strlen (str) + 1;
      str++;
      cnt++;
      if (cnt >= len)
	break;
    }
  return t;
}

/* For all elements of CLAUSES, validate them vs OpenMP constraints.
   Remove any elements from the list that are invalid.  */

tree
finish_omp_clauses (tree clauses, enum c_omp_region_type ort)
{
  bitmap_head generic_head, firstprivate_head, lastprivate_head;
  bitmap_head aligned_head, map_head, map_field_head, map_firstprivate_head;
  bitmap_head oacc_reduction_head, is_on_device_head;
  tree c, t, *pc;
  tree safelen = NULL_TREE;
  bool openacc = (ort & C_ORT_ACC) != 0;
  bool branch_seen = false;
  bool copyprivate_seen = false;
  bool ordered_seen = false;
  bool order_seen = false;
  bool schedule_seen = false;
  bool oacc_async = false;
  bool indir_component_ref_p = false;
  tree last_iterators = NULL_TREE;
  bool last_iterators_remove = false;
  /* 1 if normal/task reduction has been seen, -1 if inscan reduction
     has been seen, -2 if mixed inscan/normal reduction diagnosed.  */
  int reduction_seen = 0;
  bool allocate_seen = false;
  tree detach_seen = NULL_TREE;
  bool mergeable_seen = false;
  bool implicit_moved = false;
  bool target_in_reduction_seen = false;
  bool num_tasks_seen = false;
  bool partial_seen = false;
  bool init_seen = false;
  bool init_use_destroy_seen = false;
  tree init_no_targetsync_clause = NULL_TREE;
  tree depend_clause = NULL_TREE;

  bitmap_obstack_initialize (NULL);
  bitmap_initialize (&generic_head, &bitmap_default_obstack);
  bitmap_initialize (&firstprivate_head, &bitmap_default_obstack);
  bitmap_initialize (&lastprivate_head, &bitmap_default_obstack);
  bitmap_initialize (&aligned_head, &bitmap_default_obstack);
  /* If ort == C_ORT_OMP_DECLARE_SIMD used as uniform_head instead.  */
  bitmap_initialize (&map_head, &bitmap_default_obstack);
  bitmap_initialize (&map_field_head, &bitmap_default_obstack);
  bitmap_initialize (&map_firstprivate_head, &bitmap_default_obstack);
  /* If ort == C_ORT_OMP used as nontemporal_head or use_device_xxx_head
     instead and for ort == C_ORT_OMP_TARGET used as in_reduction_head.  */
  bitmap_initialize (&oacc_reduction_head, &bitmap_default_obstack);
  bitmap_initialize (&is_on_device_head, &bitmap_default_obstack);

  if (openacc)
    for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_ASYNC)
	{
	  oacc_async = true;
	  break;
	}

  tree *grp_start_p = NULL, grp_sentinel = NULL_TREE;

  for (pc = &clauses, c = clauses; c ; c = *pc)
    {
      bool remove = false;
      bool field_ok = false;

      /* We've reached the end of a list of expanded nodes.  Reset the group
	 start pointer.  */
      if (c == grp_sentinel)
	grp_start_p = NULL;

      switch (OMP_CLAUSE_CODE (c))
	{
	case OMP_CLAUSE_SHARED:
	  field_ok = ((ort & C_ORT_OMP_DECLARE_SIMD) == C_ORT_OMP);
	  goto check_dup_generic;
	case OMP_CLAUSE_PRIVATE:
	  field_ok = ((ort & C_ORT_OMP_DECLARE_SIMD) == C_ORT_OMP);
	  goto check_dup_generic;
	case OMP_CLAUSE_REDUCTION:
	  if (reduction_seen == 0)
	    reduction_seen = OMP_CLAUSE_REDUCTION_INSCAN (c) ? -1 : 1;
	  else if (reduction_seen != -2
		   && reduction_seen != (OMP_CLAUSE_REDUCTION_INSCAN (c)
					 ? -1 : 1))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<inscan%> and non-%<inscan%> %<reduction%> clauses "
			"on the same construct");
	      reduction_seen = -2;
	    }
	  /* FALLTHRU */
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	  field_ok = ((ort & C_ORT_OMP_DECLARE_SIMD) == C_ORT_OMP);
	  t = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (t) == OMP_ARRAY_SECTION)
	    {
	      if (handle_omp_array_sections (c, ort))
		{
		  remove = true;
		  break;
		}
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
		  && OMP_CLAUSE_REDUCTION_INSCAN (c))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<inscan%> %<reduction%> clause with array "
			    "section");
		  remove = true;
		  break;
		}
	      if (TREE_CODE (t) == OMP_ARRAY_SECTION)
		{
		  while (TREE_CODE (t) == OMP_ARRAY_SECTION)
		    t = TREE_OPERAND (t, 0);
		}
	      else
		{
		  gcc_assert (TREE_CODE (t) == MEM_REF);
		  t = TREE_OPERAND (t, 0);
		  if (TREE_CODE (t) == POINTER_PLUS_EXPR)
		    t = TREE_OPERAND (t, 0);
		  if (TREE_CODE (t) == ADDR_EXPR
		      || INDIRECT_REF_P (t))
		    t = TREE_OPERAND (t, 0);
		}
	      tree n = omp_clause_decl_field (t);
	      if (n)
		t = n;
	      goto check_dup_generic_t;
	    }
	  if (oacc_async)
	    cxx_mark_addressable (t);
	  goto check_dup_generic;
	case OMP_CLAUSE_COPYPRIVATE:
	  copyprivate_seen = true;
	  field_ok = ((ort & C_ORT_OMP_DECLARE_SIMD) == C_ORT_OMP);
	  goto check_dup_generic;
	case OMP_CLAUSE_COPYIN:
	  goto check_dup_generic;
	case OMP_CLAUSE_LINEAR:
	  field_ok = ((ort & C_ORT_OMP_DECLARE_SIMD) == C_ORT_OMP);
	  t = OMP_CLAUSE_DECL (c);
	  if (ort != C_ORT_OMP_DECLARE_SIMD
	      && OMP_CLAUSE_LINEAR_KIND (c) != OMP_CLAUSE_LINEAR_DEFAULT)
	    {
	      if (OMP_CLAUSE_LINEAR_OLD_LINEAR_MODIFIER (c))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "modifier should not be specified in %<linear%> "
			    "clause on %<simd%> or %<for%> constructs when "
			    "not using OpenMP 5.2 modifiers");
		  OMP_CLAUSE_LINEAR_KIND (c) = OMP_CLAUSE_LINEAR_DEFAULT;
		}
	      else if (OMP_CLAUSE_LINEAR_KIND (c) != OMP_CLAUSE_LINEAR_VAL)
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "modifier other than %<val%> specified in "
			    "%<linear%> clause on %<simd%> or %<for%> "
			    "constructs when using OpenMP 5.2 modifiers");
		  OMP_CLAUSE_LINEAR_KIND (c) = OMP_CLAUSE_LINEAR_DEFAULT;
		}
	    }
	  if ((VAR_P (t) || TREE_CODE (t) == PARM_DECL)
	      && !type_dependent_expression_p (t))
	    {
	      tree type = TREE_TYPE (t);
	      if ((OMP_CLAUSE_LINEAR_KIND (c) == OMP_CLAUSE_LINEAR_REF
		   || OMP_CLAUSE_LINEAR_KIND (c) == OMP_CLAUSE_LINEAR_UVAL)
		  && !TYPE_REF_P (type))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "linear clause with %qs modifier applied to "
			    "non-reference variable with %qT type",
			    OMP_CLAUSE_LINEAR_KIND (c) == OMP_CLAUSE_LINEAR_REF
			    ? "ref" : "uval", TREE_TYPE (t));
		  remove = true;
		  break;
		}
	      if (TYPE_REF_P (type))
		type = TREE_TYPE (type);
	      if (OMP_CLAUSE_LINEAR_KIND (c) != OMP_CLAUSE_LINEAR_REF)
		{
		  if (!INTEGRAL_TYPE_P (type)
		      && !TYPE_PTR_P (type))
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"linear clause applied to non-integral "
				"non-pointer variable with %qT type",
				TREE_TYPE (t));
		      remove = true;
		      break;
		    }
		}
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
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t))
		   && (ort != C_ORT_OMP_DECLARE_SIMD
		       || TREE_CODE (t) != PARM_DECL
		       || !TYPE_REF_P (TREE_TYPE (t))
		       || !INTEGRAL_TYPE_P (TREE_TYPE (TREE_TYPE (t)))))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"linear step expression must be integral");
	      remove = true;
	      break;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (ort == C_ORT_OMP_DECLARE_SIMD && TREE_CODE (t) == PARM_DECL)
		{
		  OMP_CLAUSE_LINEAR_VARIABLE_STRIDE (c) = 1;
		  goto check_dup_generic;
		}
	      if (!processing_template_decl
		  && (VAR_P (OMP_CLAUSE_DECL (c))
		      || TREE_CODE (OMP_CLAUSE_DECL (c)) == PARM_DECL))
		{
		  if (ort == C_ORT_OMP_DECLARE_SIMD)
		    {
		      t = maybe_constant_value (t);
		      if (TREE_CODE (t) != INTEGER_CST)
			{
			  error_at (OMP_CLAUSE_LOCATION (c),
				    "%<linear%> clause step %qE is neither "
				     "constant nor a parameter", t);
			  remove = true;
			  break;
			}
		    }
		  t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
		  tree type = TREE_TYPE (OMP_CLAUSE_DECL (c));
		  if (TYPE_REF_P (type))
		    type = TREE_TYPE (type);
		  if (OMP_CLAUSE_LINEAR_KIND (c) == OMP_CLAUSE_LINEAR_REF)
		    {
		      type = build_pointer_type (type);
		      tree d = fold_convert (type, OMP_CLAUSE_DECL (c));
		      t = pointer_int_sum (OMP_CLAUSE_LOCATION (c), PLUS_EXPR,
					   d, t);
		      t = fold_build2_loc (OMP_CLAUSE_LOCATION (c),
					   MINUS_EXPR, sizetype,
					   fold_convert (sizetype, t),
					   fold_convert (sizetype, d));
		      if (t == error_mark_node)
			{
			  remove = true;
			  break;
			}
		    }
		  else if (TYPE_PTR_P (type)
			   /* Can't multiply the step yet if *this
			      is still incomplete type.  */
			   && (ort != C_ORT_OMP_DECLARE_SIMD
			       || TREE_CODE (OMP_CLAUSE_DECL (c)) != PARM_DECL
			       || !DECL_ARTIFICIAL (OMP_CLAUSE_DECL (c))
			       || DECL_NAME (OMP_CLAUSE_DECL (c))
				  != this_identifier
			       || !TYPE_BEING_DEFINED (TREE_TYPE (type))))
		    {
		      tree d = convert_from_reference (OMP_CLAUSE_DECL (c));
		      t = pointer_int_sum (OMP_CLAUSE_LOCATION (c), PLUS_EXPR,
					   d, t);
		      t = fold_build2_loc (OMP_CLAUSE_LOCATION (c),
					   MINUS_EXPR, sizetype,
					   fold_convert (sizetype, t),
					   fold_convert (sizetype, d));
		      if (t == error_mark_node)
			{
			  remove = true;
			  break;
			}
		    }
		  else
		    t = fold_convert (type, t);
		}
	      OMP_CLAUSE_LINEAR_STEP (c) = t;
	    }
	  goto check_dup_generic;
	check_dup_generic:
	  t = omp_clause_decl_field (OMP_CLAUSE_DECL (c));
	  if (t)
	    {
	      if (!remove && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_SHARED)
		omp_note_field_privatization (t, OMP_CLAUSE_DECL (c));
	    }
	  else
	    t = OMP_CLAUSE_DECL (c);
	check_dup_generic_t:
	  if (t == current_class_ptr
	      && ((ort != C_ORT_OMP_DECLARE_SIMD && !openacc)
		  || (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_LINEAR
		      && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_UNIFORM)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<this%> allowed in OpenMP only in %<declare simd%>"
			" clauses");
	      remove = true;
	      break;
	    }
	  if (!VAR_P (t) && TREE_CODE (t) != PARM_DECL
	      && (!field_ok || TREE_CODE (t) != FIELD_DECL))
	    {
	      if (processing_template_decl && TREE_CODE (t) != OVERLOAD)
		break;
	      if (DECL_P (t))
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is not a variable in clause %qs", t,
			  omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qE is not a variable in clause %qs", t,
			  omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  else if ((openacc
		    && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION)
		   || (ort == C_ORT_OMP
		       && (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_USE_DEVICE_PTR
			   || (OMP_CLAUSE_CODE (c)
			       == OMP_CLAUSE_USE_DEVICE_ADDR)))
		   || (ort == C_ORT_OMP_TARGET
		       && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION))
	    {
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION
		  && (bitmap_bit_p (&generic_head, DECL_UID (t))
		      || bitmap_bit_p (&firstprivate_head, DECL_UID (t))))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD appears more than once in data-sharing "
			    "clauses", t);
		  remove = true;
		  break;
		}
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION)
		target_in_reduction_seen = true;
	      if (bitmap_bit_p (&oacc_reduction_head, DECL_UID (t)))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    openacc
			    ? "%qD appears more than once in reduction clauses"
			    : "%qD appears more than once in data clauses",
			    t);
		  remove = true;
		}
	      else
		bitmap_set_bit (&oacc_reduction_head, DECL_UID (t));
	    }
	  else if (bitmap_bit_p (&generic_head, DECL_UID (t))
		   || bitmap_bit_p (&firstprivate_head, DECL_UID (t))
		   || bitmap_bit_p (&lastprivate_head, DECL_UID (t))
		   || bitmap_bit_p (&map_firstprivate_head, DECL_UID (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qD appears more than once in data clauses", t);
	      remove = true;
	    }
	  else if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_PRIVATE
		    || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_HAS_DEVICE_ADDR
		    || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IS_DEVICE_PTR)
		   && bitmap_bit_p (&map_head, DECL_UID (t)))
	    {
	      if (openacc)
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD appears more than once in data clauses", t);
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD appears both in data and map clauses", t);
	      remove = true;
	    }
	  else
	    bitmap_set_bit (&generic_head, DECL_UID (t));
	  if (!field_ok)
	    break;
	handle_field_decl:
	  if (!remove
	      && TREE_CODE (t) == FIELD_DECL
	      && t == OMP_CLAUSE_DECL (c))
	    {
	      OMP_CLAUSE_DECL (c)
		= omp_privatize_field (t, (OMP_CLAUSE_CODE (c)
					   == OMP_CLAUSE_SHARED));
	      if (OMP_CLAUSE_DECL (c) == error_mark_node)
		remove = true;
	    }
	  break;

	case OMP_CLAUSE_FIRSTPRIVATE:
	  if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c) && !implicit_moved)
	    {
	    move_implicit:
	      implicit_moved = true;
	      /* Move firstprivate and map clauses with
		 OMP_CLAUSE_{FIRSTPRIVATE,MAP}_IMPLICIT set to the end of
		 clauses chain.  */
	      tree cl1 = NULL_TREE, cl2 = NULL_TREE;
	      tree *pc1 = pc, *pc2 = &cl1, *pc3 = &cl2;
	      while (*pc1)
		if (OMP_CLAUSE_CODE (*pc1) == OMP_CLAUSE_FIRSTPRIVATE
		    && OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (*pc1))
		  {
		    *pc3 = *pc1;
		    pc3 = &OMP_CLAUSE_CHAIN (*pc3);
		    *pc1 = OMP_CLAUSE_CHAIN (*pc1);
		  }
		else if (OMP_CLAUSE_CODE (*pc1) == OMP_CLAUSE_MAP
			 && OMP_CLAUSE_MAP_IMPLICIT (*pc1))
		  {
		    *pc2 = *pc1;
		    pc2 = &OMP_CLAUSE_CHAIN (*pc2);
		    *pc1 = OMP_CLAUSE_CHAIN (*pc1);
		  }
		else
		  pc1 = &OMP_CLAUSE_CHAIN (*pc1);
	      *pc3 = NULL;
	      *pc2 = cl2;
	      *pc1 = cl1;
	      continue;
	    }
	  t = omp_clause_decl_field (OMP_CLAUSE_DECL (c));
	  if (t)
	    omp_note_field_privatization (t, OMP_CLAUSE_DECL (c));
	  else
	    t = OMP_CLAUSE_DECL (c);
	  if (!openacc && t == current_class_ptr)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<this%> allowed in OpenMP only in %<declare simd%>"
			" clauses");
	      remove = true;
	      break;
	    }
	  if (!VAR_P (t) && TREE_CODE (t) != PARM_DECL
	      && ((ort & C_ORT_OMP_DECLARE_SIMD) != C_ORT_OMP
		  || TREE_CODE (t) != FIELD_DECL))
	    {
	      if (processing_template_decl && TREE_CODE (t) != OVERLOAD)
		break;
	      if (DECL_P (t))
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is not a variable in clause %<firstprivate%>",
			  t);
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qE is not a variable in clause %<firstprivate%>",
			  t);
	      remove = true;
	    }
	  else if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c)
		   && !OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT_TARGET (c)
		   && bitmap_bit_p (&map_firstprivate_head, DECL_UID (t)))
	    remove = true;
	  else if (bitmap_bit_p (&generic_head, DECL_UID (t))
		   || bitmap_bit_p (&firstprivate_head, DECL_UID (t))
		   || bitmap_bit_p (&map_firstprivate_head, DECL_UID (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qD appears more than once in data clauses", t);
	      remove = true;
	    }
	  else if (bitmap_bit_p (&map_head, DECL_UID (t))
		   || bitmap_bit_p (&map_field_head, DECL_UID (t)))
	    {
	      if (openacc)
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD appears more than once in data clauses", t);
	      else if (OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT (c)
		       && !OMP_CLAUSE_FIRSTPRIVATE_IMPLICIT_TARGET (c))
		/* Silently drop the clause.  */;
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD appears both in data and map clauses", t);
	      remove = true;
	    }
	  else
	    bitmap_set_bit (&firstprivate_head, DECL_UID (t));
	  goto handle_field_decl;

	case OMP_CLAUSE_LASTPRIVATE:
	  t = omp_clause_decl_field (OMP_CLAUSE_DECL (c));
	  if (t)
	    omp_note_field_privatization (t, OMP_CLAUSE_DECL (c));
	  else
	    t = OMP_CLAUSE_DECL (c);
	  if (!openacc && t == current_class_ptr)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<this%> allowed in OpenMP only in %<declare simd%>"
			" clauses");
	      remove = true;
	      break;
	    }
	  if (!VAR_P (t) && TREE_CODE (t) != PARM_DECL
	      && ((ort & C_ORT_OMP_DECLARE_SIMD) != C_ORT_OMP
		  || TREE_CODE (t) != FIELD_DECL))
	    {
	      if (processing_template_decl && TREE_CODE (t) != OVERLOAD)
		break;
	      if (DECL_P (t))
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is not a variable in clause %<lastprivate%>",
			  t);
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qE is not a variable in clause %<lastprivate%>",
			  t);
	      remove = true;
	    }
	  else if (bitmap_bit_p (&generic_head, DECL_UID (t))
		   || bitmap_bit_p (&lastprivate_head, DECL_UID (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qD appears more than once in data clauses", t);
	      remove = true;
	    }
	  else
	    bitmap_set_bit (&lastprivate_head, DECL_UID (t));
	  goto handle_field_decl;

	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_SELF:
	  t = OMP_CLAUSE_OPERAND (c, 0);
	  t = maybe_convert_cond (t);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!processing_template_decl)
	    t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	  OMP_CLAUSE_OPERAND (c, 0) = t;
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

	case OMP_CLAUSE_NOCONTEXT:
	  t = OMP_CLAUSE_NOCONTEXT_EXPR (c);
	  t = maybe_convert_cond (t);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!processing_template_decl)
	    t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	  OMP_CLAUSE_NOCONTEXT_EXPR (c) = t;
	  break;

	case OMP_CLAUSE_NOVARIANTS:
	  t = OMP_CLAUSE_NOVARIANTS_EXPR (c);
	  t = maybe_convert_cond (t);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!processing_template_decl)
	    t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	  OMP_CLAUSE_NOVARIANTS_EXPR (c) = t;
	  break;

	case OMP_CLAUSE_GANG:
	  /* Operand 1 is the gang static: argument.  */
	  t = OMP_CLAUSE_OPERAND (c, 1);
	  if (t != NULL_TREE)
	    {
	      if (t == error_mark_node)
		remove = true;
	      else if (!type_dependent_expression_p (t)
		       && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<gang%> static expression must be integral");
		  remove = true;
		}
	      else
		{
		  t = mark_rvalue_use (t);
		  if (!processing_template_decl)
		    {
		      t = maybe_constant_value (t);
		      if (TREE_CODE (t) == INTEGER_CST
			  && tree_int_cst_sgn (t) != 1
			  && t != integer_minus_one_node)
			{
			  warning_at (OMP_CLAUSE_LOCATION (c), 0,
				      "%<gang%> static value must be "
				      "positive");
			  t = integer_one_node;
			}
		      t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
		    }
		}
	      OMP_CLAUSE_OPERAND (c, 1) = t;
	    }
	  /* Check operand 0, the num argument.  */
	  /* FALLTHRU */

	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	  if (OMP_CLAUSE_OPERAND (c, 0) == NULL_TREE)
	    break;
	  /* FALLTHRU */

	case OMP_CLAUSE_NUM_TASKS:
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_NUM_TASKS)
	    num_tasks_seen = true;
	  /* FALLTHRU */

	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	  t = OMP_CLAUSE_OPERAND (c, 0);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	     switch (OMP_CLAUSE_CODE (c))
		{
		case OMP_CLAUSE_GANG:
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<gang%> num expression must be integral"); break;
		case OMP_CLAUSE_VECTOR:
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<vector%> length expression must be integral");
		  break;
		case OMP_CLAUSE_WORKER:
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<worker%> num expression must be integral");
		  break;
		default:
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qs expression must be integral",
			    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		}
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  t = maybe_constant_value (t);
		  if (TREE_CODE (t) == INTEGER_CST
		      && tree_int_cst_sgn (t) != 1)
		    {
		      switch (OMP_CLAUSE_CODE (c))
			{
			case OMP_CLAUSE_GANG:
			  warning_at (OMP_CLAUSE_LOCATION (c), 0,
				      "%<gang%> num value must be positive");
			  break;
			case OMP_CLAUSE_VECTOR:
			  warning_at (OMP_CLAUSE_LOCATION (c), 0,
				      "%<vector%> length value must be "
				      "positive");
			  break;
			case OMP_CLAUSE_WORKER:
			  warning_at (OMP_CLAUSE_LOCATION (c), 0,
				      "%<worker%> num value must be "
				      "positive");
			  break;
			default:
			  warning_at (OMP_CLAUSE_LOCATION (c),
				      (flag_openmp || flag_openmp_simd)
				      ? OPT_Wopenmp : 0,
				      "%qs value must be positive",
				      omp_clause_code_name
				      [OMP_CLAUSE_CODE (c)]);
			}
		      t = integer_one_node;
		    }
		  t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
		}
	      OMP_CLAUSE_OPERAND (c, 0) = t;
	    }
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_NUM_TEAMS
	      && OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c)
	      && !remove)
	    {
	      t = OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c);
	      if (t == error_mark_node)
		remove = true;
	      else if (!type_dependent_expression_p (t)
		       && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qs expression must be integral",
			    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		  remove = true;
		}
	      else
		{
		  t = mark_rvalue_use (t);
		  if (!processing_template_decl)
		    {
		      t = maybe_constant_value (t);
		      if (TREE_CODE (t) == INTEGER_CST
			  && tree_int_cst_sgn (t) != 1)
			{
			  warning_at (OMP_CLAUSE_LOCATION (c), OPT_Wopenmp,
				      "%qs value must be positive",
				      omp_clause_code_name
				      [OMP_CLAUSE_CODE (c)]);
			  t = NULL_TREE;
			}
		      else
			t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
		      tree upper = OMP_CLAUSE_NUM_TEAMS_UPPER_EXPR (c);
		      if (t
			  && TREE_CODE (t) == INTEGER_CST
			  && TREE_CODE (upper) == INTEGER_CST
			  && tree_int_cst_lt (upper, t))
			{
			  warning_at (OMP_CLAUSE_LOCATION (c), OPT_Wopenmp,
				      "%<num_teams%> lower bound %qE bigger "
				      "than upper bound %qE", t, upper);
			  t = NULL_TREE;
			}
		    }
		  OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c) = t;
		}
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
	      error_at (OMP_CLAUSE_LOCATION (c),
			"schedule chunk size expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  t = maybe_constant_value (t);
		  if (TREE_CODE (t) == INTEGER_CST
		      && tree_int_cst_sgn (t) != 1)
		  {
		    warning_at (OMP_CLAUSE_LOCATION (c), OPT_Wopenmp,
			      "chunk size value must be positive");
		    t = integer_one_node;
		  }
		  t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
		}
	      OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (c) = t;
	    }
	  if (!remove)
	    schedule_seen = true;
	  break;

	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_SAFELEN:
	  t = OMP_CLAUSE_OPERAND (c, 0);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qs length expression must be integral",
			omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  t = maybe_constant_value (t);
		  if (TREE_CODE (t) != INTEGER_CST
		      || tree_int_cst_sgn (t) != 1)
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"%qs length expression must be positive "
				"constant integer expression",
				omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		      remove = true;
		    }
		}
	      OMP_CLAUSE_OPERAND (c, 0) = t;
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SAFELEN)
		safelen = c;
	    }
	  break;

	case OMP_CLAUSE_ASYNC:
	  t = OMP_CLAUSE_ASYNC_EXPR (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<async%> expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	      OMP_CLAUSE_ASYNC_EXPR (c) = t;
	    }
	  break;

	case OMP_CLAUSE_WAIT:
	  t = OMP_CLAUSE_WAIT_EXPR (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!processing_template_decl)
	    t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	  OMP_CLAUSE_WAIT_EXPR (c) = t;
	  break;

	case OMP_CLAUSE_THREAD_LIMIT:
	  t = OMP_CLAUSE_THREAD_LIMIT_EXPR (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<thread_limit%> expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  t = maybe_constant_value (t);
		  if (TREE_CODE (t) == INTEGER_CST
		      && tree_int_cst_sgn (t) != 1)
		    {
		      warning_at (OMP_CLAUSE_LOCATION (c), OPT_Wopenmp,
				  "%<thread_limit%> value must be positive");
		      t = integer_one_node;
		    }
		  t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
		}
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
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<device%> id must be integral");
	      remove = true;
	    }
	  else if (OMP_CLAUSE_DEVICE_ANCESTOR (c)
		   && TREE_CODE (t) == INTEGER_CST
		   && !integer_onep (t))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"the %<device%> clause expression must evaluate to "
			"%<1%>");
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
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<dist_schedule%> chunk size expression must be "
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
	  if (t == current_class_ptr && ort != C_ORT_OMP_DECLARE_SIMD)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<this%> allowed in OpenMP only in %<declare simd%>"
			" clauses");
	      remove = true;
	      break;
	    }
	  if (!VAR_P (t) && TREE_CODE (t) != PARM_DECL)
	    {
	      if (processing_template_decl && TREE_CODE (t) != OVERLOAD)
		break;
	      if (DECL_P (t))
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is not a variable in %<aligned%> clause", t);
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qE is not a variable in %<aligned%> clause", t);
	      remove = true;
	    }
	  else if (!type_dependent_expression_p (t)
		   && !TYPE_PTR_P (TREE_TYPE (t))
		   && TREE_CODE (TREE_TYPE (t)) != ARRAY_TYPE
		   && (!TYPE_REF_P (TREE_TYPE (t))
		       || (!INDIRECT_TYPE_P (TREE_TYPE (TREE_TYPE (t)))
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
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qD appears more than once in %<aligned%> clauses",
			t);
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
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<aligned%> clause alignment expression must "
			"be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  t = maybe_constant_value (t);
		  if (TREE_CODE (t) != INTEGER_CST
		      || tree_int_cst_sgn (t) != 1)
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"%<aligned%> clause alignment expression must "
				"be positive constant integer expression");
		      remove = true;
		    }
		}
	      OMP_CLAUSE_ALIGNED_ALIGNMENT (c) = t;
	    }
	  break;

	case OMP_CLAUSE_NONTEMPORAL:
	  t = OMP_CLAUSE_DECL (c);
	  if (!VAR_P (t) && TREE_CODE (t) != PARM_DECL)
	    {
	      if (processing_template_decl && TREE_CODE (t) != OVERLOAD)
		break;
	      if (DECL_P (t))
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is not a variable in %<nontemporal%> clause",
			  t);
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qE is not a variable in %<nontemporal%> clause",
			  t);
	      remove = true;
	    }
	  else if (bitmap_bit_p (&oacc_reduction_head, DECL_UID (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qD appears more than once in %<nontemporal%> "
			"clauses", t);
	      remove = true;
	    }
	  else
	    bitmap_set_bit (&oacc_reduction_head, DECL_UID (t));
	  break;

	case OMP_CLAUSE_ALLOCATE:
	  t = omp_clause_decl_field (OMP_CLAUSE_DECL (c));
	  if (t)
	    omp_note_field_privatization (t, OMP_CLAUSE_DECL (c));
	  else
	    t = OMP_CLAUSE_DECL (c);
	  if (t == current_class_ptr)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<this%> not allowed in %<allocate%> clause");
	      remove = true;
	      break;
	    }
	  if (!VAR_P (t)
	      && TREE_CODE (t) != PARM_DECL
	      && TREE_CODE (t) != FIELD_DECL)
	    {
	      if (processing_template_decl && TREE_CODE (t) != OVERLOAD)
		break;
	      if (DECL_P (t))
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is not a variable in %<allocate%> clause", t);
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qE is not a variable in %<allocate%> clause", t);
	      remove = true;
	    }
	  else if (bitmap_bit_p (&aligned_head, DECL_UID (t)))
	    {
	      warning_at (OMP_CLAUSE_LOCATION (c), OPT_Wopenmp,
			"%qD appears more than once in %<allocate%> clauses",
			t);
	      remove = true;
	    }
	  else
	    {
	      bitmap_set_bit (&aligned_head, DECL_UID (t));
	      allocate_seen = true;
	    }
	  tree allocator, align;
	  align = OMP_CLAUSE_ALLOCATE_ALIGN (c);
	  if (error_operand_p (align))
	    {
	      remove = true;
	      break;
	    }
	  if (align)
	    {
	      if (!type_dependent_expression_p (align)
		  && !INTEGRAL_TYPE_P (TREE_TYPE (align)))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<allocate%> clause %<align%> modifier "
			    "argument needs to be positive constant "
			    "power of two integer expression");
		  remove = true;
		}
	      else
		{
		  align = mark_rvalue_use (align);
		  if (!processing_template_decl)
		    {
		      align = maybe_constant_value (align);
		      if (TREE_CODE (align) != INTEGER_CST
			  || !tree_fits_uhwi_p (align)
			  || !integer_pow2p (align))
			{
			  error_at (OMP_CLAUSE_LOCATION (c),
				    "%<allocate%> clause %<align%> modifier "
				    "argument needs to be positive constant "
				    "power of two integer expression");
			  remove = true;
			}
		    }
		}
	      OMP_CLAUSE_ALLOCATE_ALIGN (c) = align;
	    }
	  allocator = OMP_CLAUSE_ALLOCATE_ALLOCATOR (c);
	  if (error_operand_p (allocator))
	    {
	      remove = true;
	      break;
	    }
	  if (allocator == NULL_TREE)
	    goto handle_field_decl;
	  tree allocatort;
	  allocatort = TYPE_MAIN_VARIANT (TREE_TYPE (allocator));
	  if (!type_dependent_expression_p (allocator)
	      && (TREE_CODE (allocatort) != ENUMERAL_TYPE
		  || TYPE_NAME (allocatort) == NULL_TREE
		  || TREE_CODE (TYPE_NAME (allocatort)) != TYPE_DECL
		  || (DECL_NAME (TYPE_NAME (allocatort))
		      != get_identifier ("omp_allocator_handle_t"))
		  || (TYPE_CONTEXT (allocatort)
		      != DECL_CONTEXT (global_namespace))))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<allocate%> clause allocator expression has "
			"type %qT rather than %<omp_allocator_handle_t%>",
			TREE_TYPE (allocator));
	      remove = true;
	      break;
	    }
	  else
	    {
	      allocator = mark_rvalue_use (allocator);
	      if (!processing_template_decl)
		allocator = maybe_constant_value (allocator);
	      OMP_CLAUSE_ALLOCATE_ALLOCATOR (c) = allocator;
	    }
	  goto handle_field_decl;

	case OMP_CLAUSE_DOACROSS:
	  t = OMP_CLAUSE_DECL (c);
	  if (t == NULL_TREE)
	    break;
	  if (OMP_CLAUSE_DOACROSS_KIND (c) == OMP_CLAUSE_DOACROSS_SINK)
	    {
	      if (cp_finish_omp_clause_doacross_sink (c))
		remove = true;
	      break;
	    }
	  gcc_unreachable ();
	case OMP_CLAUSE_DEPEND:
	  depend_clause = c;
	  /* FALLTHRU */
	case OMP_CLAUSE_AFFINITY:
	  t = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (t) == TREE_LIST
	      && TREE_PURPOSE (t)
	      && TREE_CODE (TREE_PURPOSE (t)) == TREE_VEC)
	    {
	      if (TREE_PURPOSE (t) != last_iterators)
		last_iterators_remove
		  = cp_omp_finish_iterators (TREE_PURPOSE (t));
	      last_iterators = TREE_PURPOSE (t);
	      t = TREE_VALUE (t);
	      if (last_iterators_remove)
		t = error_mark_node;
	    }
	  else
	    last_iterators = NULL_TREE;

	  if (TREE_CODE (t) == OMP_ARRAY_SECTION)
	    {
	      if (handle_omp_array_sections (c, ort))
		remove = true;
	      else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
		       && (OMP_CLAUSE_DEPEND_KIND (c)
			   == OMP_CLAUSE_DEPEND_DEPOBJ))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<depend%> clause with %<depobj%> dependence "
			    "type on array section");
		  remove = true;
		}
	      break;
	    }
	  if (t == error_mark_node)
	    remove = true;
	  else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
		   && t == ridpointers[RID_OMP_ALL_MEMORY])
	    {
	      if (OMP_CLAUSE_DEPEND_KIND (c) != OMP_CLAUSE_DEPEND_OUT
		  && OMP_CLAUSE_DEPEND_KIND (c) != OMP_CLAUSE_DEPEND_INOUT)
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<omp_all_memory%> used with %<depend%> kind "
			    "other than %<out%> or %<inout%>");
		  remove = true;
		}
	      if (processing_template_decl)
		break;
	    }
	  else if (processing_template_decl && TREE_CODE (t) != OVERLOAD)
	    break;
	  else if (!lvalue_p (t))
	    {
	      if (DECL_P (t))
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is not lvalue expression nor array section "
			  "in %qs clause", t,
			  omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qE is not lvalue expression nor array section "
			  "in %qs clause", t,
			  omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  else if (TREE_CODE (t) == COMPONENT_REF
		   && TREE_CODE (TREE_OPERAND (t, 1)) == FIELD_DECL
		   && DECL_BIT_FIELD (TREE_OPERAND (t, 1)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"bit-field %qE in %qs clause", t,
			omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
		   && OMP_CLAUSE_DEPEND_KIND (c) == OMP_CLAUSE_DEPEND_DEPOBJ)
	    {
	      if (!c_omp_depend_t_p (TYPE_REF_P (TREE_TYPE (t))
				     ? TREE_TYPE (TREE_TYPE (t))
				     : TREE_TYPE (t)))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qE does not have %<omp_depend_t%> type in "
			    "%<depend%> clause with %<depobj%> dependence "
			    "type", t);
		  remove = true;
		}
	    }
	  else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND
		   && c_omp_depend_t_p (TYPE_REF_P (TREE_TYPE (t))
					? TREE_TYPE (TREE_TYPE (t))
					: TREE_TYPE (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qE should not have %<omp_depend_t%> type in "
			"%<depend%> clause with dependence type other than "
			"%<depobj%>", t);
	      remove = true;
	    }
	  if (!remove)
	    {
	      if (t == ridpointers[RID_OMP_ALL_MEMORY])
		t = null_pointer_node;
	      else
		{
		  tree addr = cp_build_addr_expr (t, tf_warning_or_error);
		  if (addr == error_mark_node)
		    {
		      remove = true;
		      break;
		    }
		  t = cp_build_indirect_ref (OMP_CLAUSE_LOCATION (c),
					     addr, RO_UNARY_STAR,
					     tf_warning_or_error);
		  if (t == error_mark_node)
		    {
		      remove = true;
		      break;
		    }
		}
	      if (TREE_CODE (OMP_CLAUSE_DECL (c)) == TREE_LIST
		  && TREE_PURPOSE (OMP_CLAUSE_DECL (c))
		  && (TREE_CODE (TREE_PURPOSE (OMP_CLAUSE_DECL (c)))
		      == TREE_VEC))
		TREE_VALUE (OMP_CLAUSE_DECL (c)) = t;
	      else
		OMP_CLAUSE_DECL (c) = t;
	    }
	  break;
	case OMP_CLAUSE_DETACH:
	  t = OMP_CLAUSE_DECL (c);
	  if (detach_seen)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"too many %qs clauses on a task construct",
			"detach");
	      remove = true;
	      break;
	    }
	  else if (error_operand_p (t))
	    {
	      remove = true;
	      break;
	    }
	  else
	    {
	      tree type = TYPE_MAIN_VARIANT (TREE_TYPE (t));
	      if (!type_dependent_expression_p (t)
		  && (!INTEGRAL_TYPE_P (type)
		      || TREE_CODE (type) != ENUMERAL_TYPE
		      || TYPE_NAME (type) == NULL_TREE
		      || (DECL_NAME (TYPE_NAME (type))
			  != get_identifier ("omp_event_handle_t"))))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<detach%> clause event handle "
			    "has type %qT rather than "
			    "%<omp_event_handle_t%>",
			    type);
		  remove = true;
		}
	      detach_seen = c;
	      cxx_mark_addressable (t);
	    }
	  break;

	case OMP_CLAUSE_MAP:
	  if (OMP_CLAUSE_MAP_IMPLICIT (c) && !implicit_moved)
	    goto move_implicit;
	  /* FALLTHRU */
	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_FROM:
	case OMP_CLAUSE__CACHE_:
	  {
	    using namespace omp_addr_tokenizer;
	    auto_vec<omp_addr_token *, 10> addr_tokens;

	    t = OMP_CLAUSE_DECL (c);
	    if (TREE_CODE (t) == OMP_ARRAY_SECTION)
	      {
		grp_start_p = pc;
		grp_sentinel = OMP_CLAUSE_CHAIN (c);

		if (handle_omp_array_sections (c, ort))
		  remove = true;
		else
		  {
		    t = OMP_CLAUSE_DECL (c);
		    if (TREE_CODE (t) != OMP_ARRAY_SECTION
			&& !type_dependent_expression_p (t)
			&& !omp_mappable_type (TREE_TYPE (t)))
		      {
			auto_diagnostic_group d;
			error_at (OMP_CLAUSE_LOCATION (c),
				  "array section does not have mappable type "
				  "in %qs clause",
				  omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
			if (TREE_TYPE (t) != error_mark_node
			    && !COMPLETE_TYPE_P (TREE_TYPE (t)))
			  cxx_incomplete_type_inform (TREE_TYPE (t));
			remove = true;
		      }
		    while (TREE_CODE (t) == ARRAY_REF)
		      t = TREE_OPERAND (t, 0);

		    if (type_dependent_expression_p (t))
		      break;

		    cp_omp_address_inspector ai (OMP_CLAUSE_LOCATION (c), t);

		    if (!ai.map_supported_p ()
			|| !omp_parse_expr (addr_tokens, t))
		      {
			sorry_at (OMP_CLAUSE_LOCATION (c),
				  "unsupported map expression %qE",
				  OMP_CLAUSE_DECL (c));
			remove = true;
			break;
		      }

		    /* This check is to determine if this will be the only map
		       node created for this clause.  Otherwise, we'll check
		       the following FIRSTPRIVATE_POINTER,
		       FIRSTPRIVATE_REFERENCE or ATTACH_DETACH node on the next
		       iteration(s) of the loop.   */
		    if (addr_tokens.length () >= 4
			&& addr_tokens[0]->type == STRUCTURE_BASE
			&& addr_tokens[0]->u.structure_base_kind == BASE_DECL
			&& addr_tokens[1]->type == ACCESS_METHOD
			&& addr_tokens[2]->type == COMPONENT_SELECTOR
			&& addr_tokens[3]->type == ACCESS_METHOD
			&& (addr_tokens[3]->u.access_kind == ACCESS_DIRECT
			    || (addr_tokens[3]->u.access_kind
				== ACCESS_INDEXED_ARRAY)))
		      {
			tree rt = addr_tokens[1]->expr;

			gcc_assert (DECL_P (rt));

			if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
			    && OMP_CLAUSE_MAP_IMPLICIT (c)
			    && (bitmap_bit_p (&map_head, DECL_UID (rt))
				|| bitmap_bit_p (&map_field_head, DECL_UID (rt))
				|| bitmap_bit_p (&map_firstprivate_head,
						 DECL_UID (rt))))
			  {
			    remove = true;
			    break;
			  }
			if (bitmap_bit_p (&map_field_head, DECL_UID (rt)))
			  break;
			if (bitmap_bit_p (&map_head, DECL_UID (rt)))
			  {
			    if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP)
			      error_at (OMP_CLAUSE_LOCATION (c),
					"%qD appears more than once in motion"
					" clauses", rt);
			    else if (openacc)
			      error_at (OMP_CLAUSE_LOCATION (c),
					"%qD appears more than once in data"
					" clauses", rt);
			    else
			      error_at (OMP_CLAUSE_LOCATION (c),
					"%qD appears more than once in map"
					" clauses", rt);
			    remove = true;
			  }
			else
			  {
			    bitmap_set_bit (&map_head, DECL_UID (rt));
			    bitmap_set_bit (&map_field_head, DECL_UID (rt));
			  }
		      }
		  }
		if (cp_oacc_check_attachments (c))
		  remove = true;
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		    && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH
			|| OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_DETACH)
		    && !OMP_CLAUSE_SIZE (c))
		  /* In this case, we have a single array element which is a
		     pointer, and we already set OMP_CLAUSE_SIZE in
		     handle_omp_array_sections above.  For attach/detach
		     clauses, reset the OMP_CLAUSE_SIZE (representing a bias)
		     to zero here.  */
		  OMP_CLAUSE_SIZE (c) = size_zero_node;
		break;
	      }
	    else if (type_dependent_expression_p (t))
	      break;
	    else if (!omp_parse_expr (addr_tokens, t))
	      {
		sorry_at (OMP_CLAUSE_LOCATION (c),
			  "unsupported map expression %qE",
			  OMP_CLAUSE_DECL (c));
		remove = true;
		break;
	      }
	    if (t == error_mark_node)
	      {
		remove = true;
		break;
	      }
	    /* OpenACC attach / detach clauses must be pointers.  */
	    if (cp_oacc_check_attachments (c))
	      {
		remove = true;
		break;
	      }
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		&& (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH
		    || OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_DETACH)
		&& !OMP_CLAUSE_SIZE (c))
	      /* For attach/detach clauses, set OMP_CLAUSE_SIZE (representing a
		 bias) to zero here, so it is not set erroneously to the
		 pointer size later on in gimplify.cc.  */
	      OMP_CLAUSE_SIZE (c) = size_zero_node;

	    cp_omp_address_inspector ai (OMP_CLAUSE_LOCATION (c), t);

	    if (!ai.check_clause (c))
	      {
		remove = true;
		break;
	      }

	    if (!ai.map_supported_p ())
	      {
		sorry_at (OMP_CLAUSE_LOCATION (c),
			  "unsupported map expression %qE",
			  OMP_CLAUSE_DECL (c));
		remove = true;
		break;
	      }

	    gcc_assert ((addr_tokens[0]->type == ARRAY_BASE
			 || addr_tokens[0]->type == STRUCTURE_BASE)
			&& addr_tokens[1]->type == ACCESS_METHOD);

	    t = addr_tokens[1]->expr;

	    /* This is used to prevent cxx_mark_addressable from being called
	       on 'this' for expressions like 'this->a', i.e. typical member
	       accesses.  */
	    indir_component_ref_p
	      = (addr_tokens[0]->type == STRUCTURE_BASE
		 && addr_tokens[1]->u.access_kind != ACCESS_DIRECT);

	    if (addr_tokens[0]->u.structure_base_kind != BASE_DECL)
	      goto skip_decl_checks;

	    /* For OpenMP, we can access a struct "t" and "t.d" on the same
	       mapping.  OpenACC allows multiple fields of the same structure
	       to be written.  */
	    if (addr_tokens[0]->type == STRUCTURE_BASE
		&& (bitmap_bit_p (&map_field_head, DECL_UID (t))
		    || (!openacc && bitmap_bit_p (&map_head, DECL_UID (t)))))
	      goto skip_decl_checks;

	    if (!processing_template_decl && TREE_CODE (t) == FIELD_DECL)
	      {
		OMP_CLAUSE_DECL (c)
		  = finish_non_static_data_member (t, NULL_TREE, NULL_TREE);
		break;
	      }
	    if (!VAR_P (t) && TREE_CODE (t) != PARM_DECL)
	      {
		if (processing_template_decl && TREE_CODE (t) != OVERLOAD)
		  break;
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		    && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
			|| OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ALWAYS_POINTER
			|| OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_ATTACH_DETACH
			|| (!openacc && EXPR_P (t))))
		  break;
		if (DECL_P (t))
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD is not a variable in %qs clause", t,
			    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		else
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qE is not a variable in %qs clause", t,
			    omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		remove = true;
	      }
	    else if (VAR_P (t) && CP_DECL_THREAD_LOCAL_P (t))
	      {
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is threadprivate variable in %qs clause", t,
			  omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		remove = true;
	      }
	    else if (!processing_template_decl
		     && !TYPE_REF_P (TREE_TYPE (t))
		     && (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP
			 || (OMP_CLAUSE_MAP_KIND (c)
			     != GOMP_MAP_FIRSTPRIVATE_POINTER))
		     && !indir_component_ref_p
		     && (t != current_class_ptr
			 || OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP
			 || OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_ATTACH_DETACH)
		     && !cxx_mark_addressable (t))
	      remove = true;
	    else if (!(OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		       && (OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_POINTER
			   || (OMP_CLAUSE_MAP_KIND (c)
			       == GOMP_MAP_FIRSTPRIVATE_POINTER)
			   || (OMP_CLAUSE_MAP_KIND (c)
			       == GOMP_MAP_ATTACH_DETACH)))
		     && t == OMP_CLAUSE_DECL (c)
		     && !type_dependent_expression_p (t)
		     && !omp_mappable_type (TYPE_REF_P (TREE_TYPE (t))
					    ? TREE_TYPE (TREE_TYPE (t))
					    : TREE_TYPE (t)))
	      {
		auto_diagnostic_group d;
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD does not have a mappable type in %qs clause", t,
			  omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		if (TREE_TYPE (t) != error_mark_node
		    && !COMPLETE_TYPE_P (TREE_TYPE (t)))
		  cxx_incomplete_type_inform (TREE_TYPE (t));
		remove = true;
	      }
	    else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		     && OMP_CLAUSE_MAP_KIND (c) == GOMP_MAP_FORCE_DEVICEPTR
		     && !type_dependent_expression_p (t)
		     && !INDIRECT_TYPE_P (TREE_TYPE (t)))
	      {
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is not a pointer variable", t);
		remove = true;
	      }
	    else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		     && OMP_CLAUSE_MAP_IMPLICIT (c)
		     && (bitmap_bit_p (&map_head, DECL_UID (t))
			 || bitmap_bit_p (&map_field_head, DECL_UID (t))
			 || bitmap_bit_p (&map_firstprivate_head,
					  DECL_UID (t))))
	      remove = true;
	    else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		     && (OMP_CLAUSE_MAP_KIND (c)
			 == GOMP_MAP_FIRSTPRIVATE_POINTER))
	      {
		if (bitmap_bit_p (&generic_head, DECL_UID (t))
		    || bitmap_bit_p (&firstprivate_head, DECL_UID (t))
		    || bitmap_bit_p (&map_firstprivate_head, DECL_UID (t)))
		  {
		    error_at (OMP_CLAUSE_LOCATION (c),
			      "%qD appears more than once in data clauses", t);
		    remove = true;
		  }
		else if (bitmap_bit_p (&map_head, DECL_UID (t))
			 && !bitmap_bit_p (&map_field_head, DECL_UID (t))
			 && openacc)
		  {
		    error_at (OMP_CLAUSE_LOCATION (c),
			      "%qD appears more than once in data clauses", t);
		    remove = true;
		  }
		else
		  bitmap_set_bit (&map_firstprivate_head, DECL_UID (t));
	      }
	    else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		     && (OMP_CLAUSE_MAP_KIND (c)
			 == GOMP_MAP_FIRSTPRIVATE_REFERENCE))
	      bitmap_set_bit (&map_firstprivate_head, DECL_UID (t));
	    else if (bitmap_bit_p (&map_head, DECL_UID (t))
		     && !bitmap_bit_p (&map_field_head, DECL_UID (t))
		     && ort != C_ORT_OMP
		     && ort != C_ORT_OMP_EXIT_DATA)
	      {
		if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP)
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD appears more than once in motion clauses", t);
		else if (openacc)
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD appears more than once in data clauses", t);
		else
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD appears more than once in map clauses", t);
		remove = true;
	      }
	    else if (openacc && bitmap_bit_p (&generic_head, DECL_UID (t)))
	      {
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD appears more than once in data clauses", t);
		remove = true;
	      }
	    else if (bitmap_bit_p (&firstprivate_head, DECL_UID (t))
		     || bitmap_bit_p (&is_on_device_head, DECL_UID (t)))
	      {
		if (openacc)
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD appears more than once in data clauses", t);
		else
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD appears both in data and map clauses", t);
		remove = true;
	      }
	    else if (!omp_access_chain_p (addr_tokens, 1))
	      {
		bitmap_set_bit (&map_head, DECL_UID (t));

		tree decl = OMP_CLAUSE_DECL (c);
		if (t != decl
		    && (TREE_CODE (decl) == COMPONENT_REF
			|| (INDIRECT_REF_P (decl)
			    && (TREE_CODE (TREE_OPERAND (decl, 0))
				== COMPONENT_REF)
			    && TYPE_REF_P (TREE_TYPE (TREE_OPERAND (decl,
								    0))))))
		  bitmap_set_bit (&map_field_head, DECL_UID (t));
	      }

	  skip_decl_checks:
	    /* If we call ai.expand_map_clause in handle_omp_array_sections,
	       the containing loop (here) iterates through the new nodes
	       created by that expansion.  Avoid expanding those again (just
	       by checking the node type).  */
	    if (!remove
		&& !processing_template_decl
		&& ort != C_ORT_DECLARE_SIMD
		&& (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP
		    || (OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_FIRSTPRIVATE_POINTER
			&& (OMP_CLAUSE_MAP_KIND (c)
			    != GOMP_MAP_FIRSTPRIVATE_REFERENCE)
			&& OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_ALWAYS_POINTER
			&& OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_ATTACH_DETACH
			&& OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_ATTACH
			&& OMP_CLAUSE_MAP_KIND (c) != GOMP_MAP_DETACH)))
	      {
		grp_start_p = pc;
		grp_sentinel = OMP_CLAUSE_CHAIN (c);
		tree nc = ai.expand_map_clause (c, OMP_CLAUSE_DECL (c),
						addr_tokens, ort);
		if (nc != error_mark_node)
		  c = nc;
	      }
	  }
	  break;

	case OMP_CLAUSE_ENTER:
	case OMP_CLAUSE_LINK:
	  t = OMP_CLAUSE_DECL (c);
	  const char *cname;
	  cname = omp_clause_code_name[OMP_CLAUSE_CODE (c)];
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_ENTER
	      && OMP_CLAUSE_ENTER_TO (c))
	    cname = "to";
	  if (TREE_CODE (t) == FUNCTION_DECL
	      && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_ENTER)
	    ;
	  else if (!VAR_P (t))
	    {
	      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_ENTER)
		{
		  if (TREE_CODE (t) == TEMPLATE_ID_EXPR)
		    error_at (OMP_CLAUSE_LOCATION (c),
			      "template %qE in clause %qs", t, cname);
		  else if (really_overloaded_fn (t))
		    error_at (OMP_CLAUSE_LOCATION (c),
			      "overloaded function name %qE in clause %qs", t,
			      cname);
		  else
		    error_at (OMP_CLAUSE_LOCATION (c),
			      "%qE is neither a variable nor a function name "
			      "in clause %qs", t, cname);
		}
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qE is not a variable in clause %qs", t, cname);
	      remove = true;
	    }
	  else if (DECL_THREAD_LOCAL_P (t))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qD is threadprivate variable in %qs clause", t,
			cname);
	      remove = true;
	    }
	  else if (!omp_mappable_type (TREE_TYPE (t)))
	    {
	      auto_diagnostic_group d;
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qD does not have a mappable type in %qs clause", t,
			cname);
	      if (TREE_TYPE (t) != error_mark_node
		  && !COMPLETE_TYPE_P (TREE_TYPE (t)))
		cxx_incomplete_type_inform (TREE_TYPE (t));
	      remove = true;
	    }
	  if (remove)
	    break;
	  if (bitmap_bit_p (&generic_head, DECL_UID (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qE appears more than once on the same "
			"%<declare target%> directive", t);
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
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is not an argument in %<uniform%> clause", t);
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qE is not an argument in %<uniform%> clause", t);
	      remove = true;
	      break;
	    }
	  /* map_head bitmap is used as uniform_head if declare_simd.  */
	  bitmap_set_bit (&map_head, DECL_UID (t));
	  goto check_dup_generic;

	case OMP_CLAUSE_GRAINSIZE:
	  t = OMP_CLAUSE_GRAINSIZE_EXPR (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<grainsize%> expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  t = maybe_constant_value (t);
		  if (TREE_CODE (t) == INTEGER_CST
		      && tree_int_cst_sgn (t) != 1)
		    {
		      warning_at (OMP_CLAUSE_LOCATION (c), OPT_Wopenmp,
				  "%<grainsize%> value must be positive");
		      t = integer_one_node;
		    }
		  t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
		}
	      OMP_CLAUSE_GRAINSIZE_EXPR (c) = t;
	    }
	  break;

	case OMP_CLAUSE_PRIORITY:
	  t = OMP_CLAUSE_PRIORITY_EXPR (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<priority%> expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  t = maybe_constant_value (t);
		  if (TREE_CODE (t) == INTEGER_CST
		      && tree_int_cst_sgn (t) == -1)
		    {
		      warning_at (OMP_CLAUSE_LOCATION (c), OPT_Wopenmp,
				  "%<priority%> value must be non-negative");
		      t = integer_one_node;
		    }
		  t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
		}
	      OMP_CLAUSE_PRIORITY_EXPR (c) = t;
	    }
	  break;

	case OMP_CLAUSE_HINT:
	  t = OMP_CLAUSE_HINT_EXPR (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<hint%> expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  t = maybe_constant_value (t);
		  if (TREE_CODE (t) != INTEGER_CST)
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"%<hint%> expression must be constant integer "
				"expression");
		      remove = true;
		    }
		}
	      OMP_CLAUSE_HINT_EXPR (c) = t;
	    }
	  break;

	case OMP_CLAUSE_FILTER:
	  t = OMP_CLAUSE_FILTER_EXPR (c);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<filter%> expression must be integral");
	      remove = true;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  t = maybe_constant_value (t);
		  t = fold_build_cleanup_point_expr (TREE_TYPE (t), t);
		}
	      OMP_CLAUSE_FILTER_EXPR (c) = t;
	    }
	  break;

	case OMP_CLAUSE_IS_DEVICE_PTR:
	case OMP_CLAUSE_USE_DEVICE_PTR:
	  field_ok = (ort & C_ORT_OMP_DECLARE_SIMD) == C_ORT_OMP;
	  t = OMP_CLAUSE_DECL (c);
	  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IS_DEVICE_PTR)
	    bitmap_set_bit (&is_on_device_head, DECL_UID (t));
	  if (!type_dependent_expression_p (t))
	    {
	      tree type = TREE_TYPE (t);
	      if (!TYPE_PTR_P (type)
		  && (!TYPE_REF_P (type) || !TYPE_PTR_P (TREE_TYPE (type))))
		{
		  if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_USE_DEVICE_PTR
		      && ort == C_ORT_OMP)
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"%qs variable is neither a pointer "
				"nor reference to pointer",
				omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		      remove = true;
		    }
		  else if (TREE_CODE (type) != ARRAY_TYPE
			   && (!TYPE_REF_P (type)
			       || TREE_CODE (TREE_TYPE (type)) != ARRAY_TYPE))
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"%qs variable is neither a pointer, nor an "
				"array nor reference to pointer or array",
				omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
		      remove = true;
		    }
		}
	    }
	  goto check_dup_generic;

	case OMP_CLAUSE_HAS_DEVICE_ADDR:
	  t = OMP_CLAUSE_DECL (c);
	  if (TREE_CODE (t) == OMP_ARRAY_SECTION)
	    {
	      if (handle_omp_array_sections (c, ort))
		remove = true;
	      else
		{
		  t = OMP_CLAUSE_DECL (c);
		  while (TREE_CODE (t) == OMP_ARRAY_SECTION)
		    t = TREE_OPERAND (t, 0);
		  while (INDIRECT_REF_P (t)
			 || TREE_CODE (t) == ARRAY_REF)
		    t = TREE_OPERAND (t, 0);
		}
	    }
	  if (VAR_P (t) || TREE_CODE (t) == PARM_DECL)
	    {
	      bitmap_set_bit (&is_on_device_head, DECL_UID (t));
	      if (!processing_template_decl
		  && !cxx_mark_addressable (t))
		remove = true;
	    }
	  goto check_dup_generic_t;

	case OMP_CLAUSE_USE_DEVICE_ADDR:
	  field_ok = true;
	  t = OMP_CLAUSE_DECL (c);
	  if (!processing_template_decl
	      && (VAR_P (t) || TREE_CODE (t) == PARM_DECL)
	      && !TYPE_REF_P (TREE_TYPE (t))
	      && !cxx_mark_addressable (t))
	    remove = true;
	  goto check_dup_generic;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_PARALLEL:
	case OMP_CLAUSE_FOR:
	case OMP_CLAUSE_SECTIONS:
	case OMP_CLAUSE_TASKGROUP:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_DEVICE_TYPE:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_BIND:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE_IF_PRESENT:
	case OMP_CLAUSE_FINALIZE:
	case OMP_CLAUSE_NOHOST:
	case OMP_CLAUSE_INDIRECT:
	  break;

	case OMP_CLAUSE_MERGEABLE:
	  mergeable_seen = true;
	  break;

	case OMP_CLAUSE_TILE:
	  for (tree list = OMP_CLAUSE_TILE_LIST (c); !remove && list;
	       list = TREE_CHAIN (list))
	    {
	      t = TREE_VALUE (list);

	      if (t == error_mark_node)
		remove = true;
	      else if (!type_dependent_expression_p (t)
		       && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<tile%> argument needs integral type");
		  remove = true;
		}
	      else
		{
		  t = mark_rvalue_use (t);
		  if (!processing_template_decl)
		    {
		      /* Zero is used to indicate '*', we permit you
			 to get there via an ICE of value zero.  */
		      t = maybe_constant_value (t);
		      if (!tree_fits_shwi_p (t)
			  || tree_to_shwi (t) < 0)
			{
			  error_at (OMP_CLAUSE_LOCATION (c),
				    "%<tile%> argument needs positive "
				    "integral constant");
			  remove = true;
			}
		    }
		}

		/* Update list item.  */
	      TREE_VALUE (list) = t;
	    }
	  break;

	case OMP_CLAUSE_SIZES:
	  for (tree list = OMP_CLAUSE_SIZES_LIST (c);
	       !remove && list; list = TREE_CHAIN (list))
	    {
	      t = TREE_VALUE (list);

	      if (t == error_mark_node)
		t = integer_one_node;
	      else if (!type_dependent_expression_p (t)
		       && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%<sizes%> argument needs positive integral "
			    "constant");
		  t = integer_one_node;
		}
	      else
		{
		  t = mark_rvalue_use (t);
		  if (!processing_template_decl)
		    {
		      t = maybe_constant_value (t);
		      HOST_WIDE_INT n;
		      if (!tree_fits_shwi_p (t)
			  || !INTEGRAL_TYPE_P (TREE_TYPE (t))
			  || (n = tree_to_shwi (t)) <= 0
			  || (int)n != n)
			{
			  error_at (OMP_CLAUSE_LOCATION (c),
				    "%<sizes%> argument needs positive "
				    "integral constant");
			  t = integer_one_node;
			}
		    }
		}

	      /* Update list item.  */
	      TREE_VALUE (list) = t;
	    }
	  break;

	case OMP_CLAUSE_ORDERED:
	  ordered_seen = true;
	  break;

	case OMP_CLAUSE_ORDER:
	  if (order_seen)
	    remove = true;
	  else
	    order_seen = true;
	  break;

	case OMP_CLAUSE_INBRANCH:
	case OMP_CLAUSE_NOTINBRANCH:
	  if (branch_seen)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<inbranch%> clause is incompatible with "
			"%<notinbranch%>");
	      remove = true;
	    }
	  branch_seen = true;
	  break;

	case OMP_CLAUSE_INCLUSIVE:
	case OMP_CLAUSE_EXCLUSIVE:
	  t = omp_clause_decl_field (OMP_CLAUSE_DECL (c));
	  if (!t)
	    t = OMP_CLAUSE_DECL (c);
	  if (t == current_class_ptr)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<this%> allowed in OpenMP only in %<declare simd%>"
			" clauses");
	      remove = true;
	      break;
	    }
	  if (!VAR_P (t)
	      && TREE_CODE (t) != PARM_DECL
	      && TREE_CODE (t) != FIELD_DECL)
	    {
	      if (processing_template_decl && TREE_CODE (t) != OVERLOAD)
		break;
	      if (DECL_P (t))
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qD is not a variable in clause %qs", t,
			  omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      else
		error_at (OMP_CLAUSE_LOCATION (c),
			  "%qE is not a variable in clause %qs", t,
			  omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  break;

	case OMP_CLAUSE_FULL:
	  break;

	case OMP_CLAUSE_PARTIAL:
	  partial_seen = true;
	  t = OMP_CLAUSE_PARTIAL_EXPR (c);
	  if (!t)
	    break;

	  if (t == error_mark_node)
	    t = NULL_TREE;
	  else if (!type_dependent_expression_p (t)
		   && !INTEGRAL_TYPE_P (TREE_TYPE (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<partial%> argument needs positive constant "
			"integer expression");
	      t = NULL_TREE;
	    }
	  else
	    {
	      t = mark_rvalue_use (t);
	      if (!processing_template_decl)
		{
		  t = maybe_constant_value (t);

		  HOST_WIDE_INT n;
		  if (!INTEGRAL_TYPE_P (TREE_TYPE (t))
		      || !tree_fits_shwi_p (t)
		      || (n = tree_to_shwi (t)) <= 0
		      || (int)n != n)
		    {
		      error_at (OMP_CLAUSE_LOCATION (c),
				"%<partial%> argument needs positive "
				"constant integer expression");
		      t = NULL_TREE;
		    }
		}
	    }

	  OMP_CLAUSE_PARTIAL_EXPR (c) = t;
	  break;
	case OMP_CLAUSE_INIT:
	  init_seen = true;
	  OMP_CLAUSE_INIT_PREFER_TYPE (c)
	    = cp_finish_omp_init_prefer_type (OMP_CLAUSE_INIT_PREFER_TYPE (c));
	  if (!OMP_CLAUSE_INIT_TARGETSYNC (c))
	    init_no_targetsync_clause = c;
	  /* FALLTHRU */
	case OMP_CLAUSE_DESTROY:
	case OMP_CLAUSE_USE:
	  init_use_destroy_seen = true;
	  t = OMP_CLAUSE_DECL (c);
	  if (bitmap_bit_p (&generic_head, DECL_UID (t)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qD appears more than once in action clauses", t);
	      remove = true;
	      break;
	    }
	  bitmap_set_bit (&generic_head, DECL_UID (t));
	  /* FALLTHRU */
	case OMP_CLAUSE_INTEROP:
	  if (!processing_template_decl)
	    {
	      if (/* (ort == C_ORT_OMP_INTEROP [uncomment for depobj init]
		      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_INTEROP) &&  */
		  !c_omp_interop_t_p (TREE_TYPE (OMP_CLAUSE_DECL (c))))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD must be of %<omp_interop_t%>",
			    OMP_CLAUSE_DECL (c));
		  remove = true;
		}
	      else if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_INIT
			|| OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DESTROY)
		       && TREE_READONLY (OMP_CLAUSE_DECL (c)))
		{
		  error_at (OMP_CLAUSE_LOCATION (c),
			    "%qD shall not be const", OMP_CLAUSE_DECL (c));
		  remove = true;
		}
	    }
	  pc = &OMP_CLAUSE_CHAIN (c);
	  break;
	default:
	  gcc_unreachable ();
	}

      if (remove)
	{
	  if (grp_start_p)
	    {
	      /* If we found a clause to remove, we want to remove the whole
		 expanded group, otherwise gimplify
		 (omp_resolve_clause_dependencies) can get confused.  */
	      *grp_start_p = grp_sentinel;
	      pc = grp_start_p;
	      grp_start_p = NULL;
	    }
	  else
	    *pc = OMP_CLAUSE_CHAIN (c);
	}
      else
	pc = &OMP_CLAUSE_CHAIN (c);
    }

  if (reduction_seen < 0 && (ordered_seen || schedule_seen))
    reduction_seen = -2;

  for (pc = &clauses, c = clauses; c ; c = *pc)
    {
      enum omp_clause_code c_kind = OMP_CLAUSE_CODE (c);
      bool remove = false;
      bool need_complete_type = false;
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
	  need_complete_type = true;
	  need_default_ctor = true;
	  need_dtor = true;
	  need_implicitly_determined = true;
	  break;
	case OMP_CLAUSE_FIRSTPRIVATE:
	  need_complete_type = true;
	  need_copy_ctor = true;
	  need_dtor = true;
	  need_implicitly_determined = true;
	  break;
	case OMP_CLAUSE_LASTPRIVATE:
	  need_complete_type = true;
	  need_copy_assignment = true;
	  need_implicitly_determined = true;
	  break;
	case OMP_CLAUSE_REDUCTION:
	  if (reduction_seen == -2)
	    OMP_CLAUSE_REDUCTION_INSCAN (c) = 0;
	  if (OMP_CLAUSE_REDUCTION_INSCAN (c))
	    need_copy_assignment = true;
	  need_implicitly_determined = true;
	  break;
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	case OMP_CLAUSE_INCLUSIVE:
	case OMP_CLAUSE_EXCLUSIVE:
	  need_implicitly_determined = true;
	  break;
	case OMP_CLAUSE_LINEAR:
	  if (ort != C_ORT_OMP_DECLARE_SIMD)
	    need_implicitly_determined = true;
	  else if (OMP_CLAUSE_LINEAR_VARIABLE_STRIDE (c)
		   && !bitmap_bit_p (&map_head,
				     DECL_UID (OMP_CLAUSE_LINEAR_STEP (c))))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<linear%> clause step is a parameter %qD not "
			"specified in %<uniform%> clause",
			OMP_CLAUSE_LINEAR_STEP (c));
	      *pc = OMP_CLAUSE_CHAIN (c);
	      continue;
	    }
	  break;
	case OMP_CLAUSE_COPYPRIVATE:
	  need_copy_assignment = true;
	  break;
	case OMP_CLAUSE_COPYIN:
	  need_copy_assignment = true;
	  break;
	case OMP_CLAUSE_SIMDLEN:
	  if (safelen
	      && !processing_template_decl
	      && tree_int_cst_lt (OMP_CLAUSE_SAFELEN_EXPR (safelen),
				  OMP_CLAUSE_SIMDLEN_EXPR (c)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<simdlen%> clause value is bigger than "
			"%<safelen%> clause value");
	      OMP_CLAUSE_SIMDLEN_EXPR (c)
		= OMP_CLAUSE_SAFELEN_EXPR (safelen);
	    }
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	case OMP_CLAUSE_SCHEDULE:
	  if (ordered_seen
	      && (OMP_CLAUSE_SCHEDULE_KIND (c)
		  & OMP_CLAUSE_SCHEDULE_NONMONOTONIC))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<nonmonotonic%> schedule modifier specified "
			"together with %<ordered%> clause");
	      OMP_CLAUSE_SCHEDULE_KIND (c)
		= (enum omp_clause_schedule_kind)
		  (OMP_CLAUSE_SCHEDULE_KIND (c)
		   & ~OMP_CLAUSE_SCHEDULE_NONMONOTONIC);
	    }
	  if (reduction_seen == -2)
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%qs clause specified together with %<inscan%> "
		      "%<reduction%> clause", "schedule");
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	case OMP_CLAUSE_NOGROUP:
	  if (reduction_seen)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<nogroup%> clause must not be used together with "
			"%<reduction%> clause");
	      *pc = OMP_CLAUSE_CHAIN (c);
	      continue;
	    }
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	case OMP_CLAUSE_GRAINSIZE:
	  if (num_tasks_seen)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<grainsize%> clause must not be used together with "
			"%<num_tasks%> clause");
	      *pc = OMP_CLAUSE_CHAIN (c);
	      continue;
	    }
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	case OMP_CLAUSE_ORDERED:
	  if (reduction_seen == -2)
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%qs clause specified together with %<inscan%> "
		      "%<reduction%> clause", "ordered");
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	case OMP_CLAUSE_ORDER:
	  if (ordered_seen)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<order%> clause must not be used together "
			"with %<ordered%> clause");
	      *pc = OMP_CLAUSE_CHAIN (c);
	      continue;
	    }
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	case OMP_CLAUSE_DETACH:
	  if (mergeable_seen)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<detach%> clause must not be used together with "
			"%<mergeable%> clause");
	      *pc = OMP_CLAUSE_CHAIN (c);
	      continue;
	    }
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	case OMP_CLAUSE_MAP:
	  if (target_in_reduction_seen && !processing_template_decl)
	    {
	      t = OMP_CLAUSE_DECL (c);
	      while (handled_component_p (t)
		     || INDIRECT_REF_P (t)
		     || TREE_CODE (t) == ADDR_EXPR
		     || TREE_CODE (t) == MEM_REF
		     || TREE_CODE (t) == NON_LVALUE_EXPR)
		t = TREE_OPERAND (t, 0);
	      if (DECL_P (t)
		  && bitmap_bit_p (&oacc_reduction_head, DECL_UID (t)))
		OMP_CLAUSE_MAP_IN_REDUCTION (c) = 1;
	    }
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	case OMP_CLAUSE_FULL:
	  if (partial_seen)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<full%> clause must not be used together "
			"with %<partial%> clause");
	      *pc = OMP_CLAUSE_CHAIN (c);
	      continue;
	    }
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	case OMP_CLAUSE_NOWAIT:
	  if (copyprivate_seen)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<nowait%> clause must not be used together "
			"with %<copyprivate%> clause");
	      *pc = OMP_CLAUSE_CHAIN (c);
	      continue;
	    }
	  /* FALLTHRU */
	default:
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	}

      t = OMP_CLAUSE_DECL (c);
      switch (c_kind)
	{
	case OMP_CLAUSE_LASTPRIVATE:
	  if (DECL_P (t)
	      && !bitmap_bit_p (&firstprivate_head, DECL_UID (t)))
	    {
	      need_default_ctor = true;
	      need_dtor = true;
	    }
	  break;

	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	  if (allocate_seen)
	    {
	      if (TREE_CODE (t) == MEM_REF)
		{
		  t = TREE_OPERAND (t, 0);
		  if (TREE_CODE (t) == POINTER_PLUS_EXPR)
		    t = TREE_OPERAND (t, 0);
		  if (TREE_CODE (t) == ADDR_EXPR
		      || INDIRECT_REF_P (t))
		    t = TREE_OPERAND (t, 0);
		  if (DECL_P (t))
		    bitmap_clear_bit (&aligned_head, DECL_UID (t));
		}
	      else if (TREE_CODE (t) == OMP_ARRAY_SECTION)
		{
		  while (TREE_CODE (t) == OMP_ARRAY_SECTION)
		    t = TREE_OPERAND (t, 0);
		  if (DECL_P (t))
		    bitmap_clear_bit (&aligned_head, DECL_UID (t));
		  t = OMP_CLAUSE_DECL (c);
		}
	      else if (DECL_P (t))
		bitmap_clear_bit (&aligned_head, DECL_UID (t));
	      t = OMP_CLAUSE_DECL (c);
	    }
	  if (processing_template_decl
	      && !VAR_P (t) && TREE_CODE (t) != PARM_DECL)
	    break;
	  if (finish_omp_reduction_clause (c, &need_default_ctor,
					   &need_dtor))
	    remove = true;
	  else
	    t = OMP_CLAUSE_DECL (c);
	  break;

	case OMP_CLAUSE_COPYIN:
	  if (processing_template_decl
	      && !VAR_P (t) && TREE_CODE (t) != PARM_DECL)
	    break;
	  if (!VAR_P (t) || !CP_DECL_THREAD_LOCAL_P (t))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qE must be %<threadprivate%> for %<copyin%>", t);
	      remove = true;
	    }
	  break;

	default:
	  break;
	}

      if (processing_template_decl
	  && !VAR_P (t) && TREE_CODE (t) != PARM_DECL)
	{
	  pc = &OMP_CLAUSE_CHAIN (c);
	  continue;
	}

      if (need_complete_type || need_copy_assignment)
	{
	  t = require_complete_type (t);
	  if (t == error_mark_node)
	    remove = true;
	  else if (!processing_template_decl
		   && TYPE_REF_P (TREE_TYPE (t))
		   && !complete_type_or_else (TREE_TYPE (TREE_TYPE (t)), t))
	    remove = true;
	}
      if (need_implicitly_determined)
	{
	  const char *share_name = NULL;

	  if (allocate_seen
	      && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_SHARED
	      && DECL_P (t))
	    bitmap_clear_bit (&aligned_head, DECL_UID (t));

	  if (VAR_P (t) && CP_DECL_THREAD_LOCAL_P (t))
	    share_name = "threadprivate";
	  else switch (cxx_omp_predetermined_sharing_1 (t))
	    {
	    case OMP_CLAUSE_DEFAULT_UNSPECIFIED:
	      break;
	    case OMP_CLAUSE_DEFAULT_SHARED:
	      if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
		   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE)
		  && c_omp_predefined_variable (t))
		/* The __func__ variable and similar function-local predefined
		   variables may be listed in a shared or firstprivate
		   clause.  */
		break;
	      if (VAR_P (t)
		  && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
		  && TREE_STATIC (t)
		  && cxx_omp_const_qual_no_mutable (t))
		{
		  tree ctx = CP_DECL_CONTEXT (t);
		  /* const qualified static data members without mutable
		     member may be specified in firstprivate clause.  */
		  if (TYPE_P (ctx) && MAYBE_CLASS_TYPE_P (ctx))
		    break;
		}
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
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%qE is predetermined %qs for %qs",
			omp_clause_printable_decl (t), share_name,
			omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	      remove = true;
	    }
	  else if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_SHARED
		   && OMP_CLAUSE_CODE (c) != OMP_CLAUSE_FIRSTPRIVATE
		   && cxx_omp_const_qual_no_mutable (t))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"%<const%> qualified %qE without %<mutable%> member "
			"may appear only in %<shared%> or %<firstprivate%> "
			"clauses", omp_clause_printable_decl (t));
	      remove = true;
	    }
	}

      if (detach_seen
	  && (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED
	      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_PRIVATE
	      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
	      || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE)
	  && OMP_CLAUSE_DECL (c) == OMP_CLAUSE_DECL (detach_seen))
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "the event handle of a %<detach%> clause "
		    "should not be in a data-sharing clause");
	  remove = true;
	}

      /* We're interested in the base element, not arrays.  */
      inner_type = type = TREE_TYPE (t);
      if ((need_complete_type
	   || need_copy_assignment
	   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
	   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IN_REDUCTION
	   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_TASK_REDUCTION)
	  && TYPE_REF_P (inner_type))
	inner_type = TREE_TYPE (inner_type);
      while (TREE_CODE (inner_type) == ARRAY_TYPE)
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

      if (!remove
	  && c_kind == OMP_CLAUSE_SHARED
	  && processing_template_decl)
	{
	  t = omp_clause_decl_field (OMP_CLAUSE_DECL (c));
	  if (t)
	    OMP_CLAUSE_DECL (c) = t;
	}

      if (remove)
	*pc = OMP_CLAUSE_CHAIN (c);
      else
	pc = &OMP_CLAUSE_CHAIN (c);
    }

  if (allocate_seen)
    for (pc = &clauses, c = clauses; c ; c = *pc)
      {
	bool remove = false;
	if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_ALLOCATE
	    && !OMP_CLAUSE_ALLOCATE_COMBINED (c)
	    && DECL_P (OMP_CLAUSE_DECL (c))
	    && bitmap_bit_p (&aligned_head, DECL_UID (OMP_CLAUSE_DECL (c))))
	  {
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%qD specified in %<allocate%> clause but not in "
		      "an explicit privatization clause", OMP_CLAUSE_DECL (c));
	    remove = true;
	  }
	if (remove)
	  *pc = OMP_CLAUSE_CHAIN (c);
	else
	  pc = &OMP_CLAUSE_CHAIN (c);
      }

  if (ort == C_ORT_OMP_INTEROP
      && depend_clause
      && (!init_use_destroy_seen
	  || (init_seen && init_no_targetsync_clause)))
    {
      error_at (OMP_CLAUSE_LOCATION (depend_clause),
		"%<depend%> clause requires action clauses with "
		"%<targetsync%> interop-type");
      if (init_no_targetsync_clause)
	inform (OMP_CLAUSE_LOCATION (init_no_targetsync_clause),
		"%<init%> clause lacks the %<targetsync%> modifier");
    }

  bitmap_obstack_release (NULL);
  return clauses;
}

/* Start processing OpenMP clauses that can include any
   privatization clauses for non-static data members.  */

tree
push_omp_privatization_clauses (bool ignore_next)
{
  if (omp_private_member_ignore_next)
    {
      omp_private_member_ignore_next = ignore_next;
      return NULL_TREE;
    }
  omp_private_member_ignore_next = ignore_next;
  if (omp_private_member_map)
    omp_private_member_vec.safe_push (error_mark_node);
  return push_stmt_list ();
}

/* Revert remapping of any non-static data members since
   the last push_omp_privatization_clauses () call.  */

void
pop_omp_privatization_clauses (tree stmt)
{
  if (stmt == NULL_TREE)
    return;
  stmt = pop_stmt_list (stmt);
  if (omp_private_member_map)
    {
      while (!omp_private_member_vec.is_empty ())
	{
	  tree t = omp_private_member_vec.pop ();
	  if (t == error_mark_node)
	    {
	      add_stmt (stmt);
	      return;
	    }
	  bool no_decl_expr = t == integer_zero_node;
	  if (no_decl_expr)
	    t = omp_private_member_vec.pop ();
	  tree *v = omp_private_member_map->get (t);
	  gcc_assert (v);
	  if (!no_decl_expr)
	    add_decl_expr (*v);
	  omp_private_member_map->remove (t);
	}
      delete omp_private_member_map;
      omp_private_member_map = NULL;
    }
  add_stmt (stmt);
}

/* Remember OpenMP privatization clauses mapping and clear it.
   Used for lambdas.  */

void
save_omp_privatization_clauses (vec<tree> &save)
{
  save = vNULL;
  if (omp_private_member_ignore_next)
    save.safe_push (integer_one_node);
  omp_private_member_ignore_next = false;
  if (!omp_private_member_map)
    return;

  while (!omp_private_member_vec.is_empty ())
    {
      tree t = omp_private_member_vec.pop ();
      if (t == error_mark_node)
	{
	  save.safe_push (t);
	  continue;
	}
      tree n = t;
      if (t == integer_zero_node)
	t = omp_private_member_vec.pop ();
      tree *v = omp_private_member_map->get (t);
      gcc_assert (v);
      save.safe_push (*v);
      save.safe_push (t);
      if (n != t)
	save.safe_push (n);
    }
  delete omp_private_member_map;
  omp_private_member_map = NULL;
}

/* Restore OpenMP privatization clauses mapping saved by the
   above function.  */

void
restore_omp_privatization_clauses (vec<tree> &save)
{
  gcc_assert (omp_private_member_vec.is_empty ());
  omp_private_member_ignore_next = false;
  if (save.is_empty ())
    return;
  if (save.length () == 1 && save[0] == integer_one_node)
    {
      omp_private_member_ignore_next = true;
      save.release ();
      return;
    }

  omp_private_member_map = new hash_map <tree, tree>;
  while (!save.is_empty ())
    {
      tree t = save.pop ();
      tree n = t;
      if (t != error_mark_node)
	{
	  if (t == integer_one_node)
	    {
	      omp_private_member_ignore_next = true;
	      gcc_assert (save.is_empty ());
	      break;
	    }
	  if (t == integer_zero_node)
	    t = save.pop ();
	  tree &v = omp_private_member_map->get_or_insert (t);
	  v = save.pop ();
	}
      omp_private_member_vec.safe_push (t);
      if (n != t)
	omp_private_member_vec.safe_push (n);
    }
  save.release ();
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
      location_t loc = EXPR_LOCATION (TREE_VALUE (t));

      if (error_operand_p (v))
	;
      else if (!VAR_P (v))
	error_at (loc, "%<threadprivate%> %qD is not file, namespace "
		       "or block scope variable", v);
      /* If V had already been marked threadprivate, it doesn't matter
	 whether it had been used prior to this point.  */
      else if (TREE_USED (v)
	  && (DECL_LANG_SPECIFIC (v) == NULL
	      || !CP_DECL_THREADPRIVATE_P (v)))
	error_at (loc, "%qE declared %<threadprivate%> after first use", v);
      else if (! TREE_STATIC (v) && ! DECL_EXTERNAL (v))
	error_at (loc, "automatic variable %qE cannot be %<threadprivate%>", v);
      else if (! COMPLETE_TYPE_P (complete_type (TREE_TYPE (v))))
	error_at (loc, "%<threadprivate%> %qE has incomplete type", v);
      else if (TREE_STATIC (v) && TYPE_P (CP_DECL_CONTEXT (v))
	       && CP_DECL_CONTEXT (v) != current_class_type)
	error_at (loc, "%<threadprivate%> %qE directive not "
		       "in %qT definition", v, CP_DECL_CONTEXT (v));
      else
	{
	  /* Allocate a LANG_SPECIFIC structure for V, if needed.  */
	  if (DECL_LANG_SPECIFIC (v) == NULL)
	    retrofit_lang_decl (v);

	  if (! CP_DECL_THREAD_LOCAL_P (v))
	    {
	      CP_DECL_THREAD_LOCAL_P (v) = true;
	      set_decl_tls_model (v, decl_default_tls_model (v));
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

/* Generate OACC_DATA, with CLAUSES and BLOCK as its compound
   statement.  */

tree
finish_oacc_data (tree clauses, tree block)
{
  tree stmt;

  block = finish_omp_structured_block (block);

  stmt = make_node (OACC_DATA);
  TREE_TYPE (stmt) = void_type_node;
  OACC_DATA_CLAUSES (stmt) = clauses;
  OACC_DATA_BODY (stmt) = block;

  return add_stmt (stmt);
}

/* Generate OACC_HOST_DATA, with CLAUSES and BLOCK as its compound
   statement.  */

tree
finish_oacc_host_data (tree clauses, tree block)
{
  tree stmt;

  block = finish_omp_structured_block (block);

  stmt = make_node (OACC_HOST_DATA);
  TREE_TYPE (stmt) = void_type_node;
  OACC_HOST_DATA_CLAUSES (stmt) = clauses;
  OACC_HOST_DATA_BODY (stmt) = block;

  return add_stmt (stmt);
}

/* Generate OMP construct CODE, with BODY and CLAUSES as its compound
   statement.  */

tree
finish_omp_construct (enum tree_code code, tree body, tree clauses)
{
  body = finish_omp_structured_block (body);

  tree stmt = make_node (code);
  TREE_TYPE (stmt) = void_type_node;
  OMP_BODY (stmt) = body;
  OMP_CLAUSES (stmt) = clauses;

  return add_stmt (stmt);
}

/* Used to walk OpenMP target directive body.  */

struct omp_target_walk_data
{
  /* Holds the 'this' expression found in current function.  */
  tree current_object;

  /* True if the 'this' expression was accessed in the target body.  */
  bool this_expr_accessed;

  /* For non-static functions, record which pointer-typed members were
     accessed, and the whole expression.  */
  hash_map<tree, tree> ptr_members_accessed;

  /* Record which lambda objects were accessed in target body.  */
  hash_set<tree> lambda_objects_accessed;

  /* For lambda functions, the __closure object expression of the current
     function, and the set of captured variables accessed in target body.  */
  tree current_closure;
  hash_set<tree> closure_vars_accessed;

  /* Local variables declared inside a BIND_EXPR, used to filter out such
     variables when recording lambda_objects_accessed.  */
  hash_set<tree> local_decls;
};

/* Helper function of finish_omp_target_clauses, called via
   cp_walk_tree_without_duplicates.  Traverse body of OpenMP target
   directive *TP, and fill out omp_target_walk_data passed in *PTR.  */

static tree
finish_omp_target_clauses_r (tree *tp, int *walk_subtrees, void *ptr)
{
  tree t = *tp;
  struct omp_target_walk_data *data = (struct omp_target_walk_data *) ptr;
  tree current_object = data->current_object;
  tree current_closure = data->current_closure;

  /* References inside of these expression codes shouldn't incur any
     form of mapping, so return early.  */
  if (TREE_CODE (t) == SIZEOF_EXPR
      || TREE_CODE (t) == ALIGNOF_EXPR)
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  if (TREE_CODE (t) == OMP_CLAUSE)
    return NULL_TREE;

  if (current_object)
    {
      tree this_expr = TREE_OPERAND (current_object, 0);

      if (operand_equal_p (t, this_expr))
	{
	  data->this_expr_accessed = true;
	  *walk_subtrees = 0;
	  return NULL_TREE;
	}

      if (TREE_CODE (t) == COMPONENT_REF
	  && POINTER_TYPE_P (TREE_TYPE (t))
	  && operand_equal_p (TREE_OPERAND (t, 0), current_object)
	  && TREE_CODE (TREE_OPERAND (t, 1)) == FIELD_DECL)
	{
	  data->this_expr_accessed = true;
	  tree fld = TREE_OPERAND (t, 1);
	  if (data->ptr_members_accessed.get (fld) == NULL)
	    {
	      if (TREE_CODE (TREE_TYPE (t)) == REFERENCE_TYPE)
		t = convert_from_reference (t);
	      data->ptr_members_accessed.put (fld, t);
	    }
	  *walk_subtrees = 0;
	  return NULL_TREE;
	}
    }

  /* When the current_function_decl is a lambda function, the closure object
     argument's type seems to not yet have fields layed out, so a recording
     of DECL_VALUE_EXPRs during the target body walk seems the only way to
     find them.  */
  if (current_closure
      && (VAR_P (t)
	  || TREE_CODE (t) == PARM_DECL
	  || TREE_CODE (t) == RESULT_DECL)
      && DECL_HAS_VALUE_EXPR_P (t)
      && TREE_CODE (DECL_VALUE_EXPR (t)) == COMPONENT_REF
      && operand_equal_p (current_closure,
			  TREE_OPERAND (DECL_VALUE_EXPR (t), 0)))
    {
      if (!data->closure_vars_accessed.contains (t))
	data->closure_vars_accessed.add (t);
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  if (TREE_CODE (t) == BIND_EXPR)
    {
      if (tree block = BIND_EXPR_BLOCK (t))
	for (tree var = BLOCK_VARS (block); var; var = DECL_CHAIN (var))
	  if (!data->local_decls.contains (var))
	    data->local_decls.add (var);
      return NULL_TREE;
    }

  if (TREE_TYPE (t) && LAMBDA_TYPE_P (TREE_TYPE (t)))
    {
      tree lt = TREE_TYPE (t);
      gcc_assert (CLASS_TYPE_P (lt));

      if (!data->lambda_objects_accessed.contains (t)
	  /* Do not prepare to create target maps for locally declared
	     lambdas or anonymous ones.  */
	  && !data->local_decls.contains (t)
	  && TREE_CODE (t) != TARGET_EXPR)
	data->lambda_objects_accessed.add (t);
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  return NULL_TREE;
}

/* Helper function for finish_omp_target, and also from tsubst_expr.
   Create additional clauses for mapping of non-static members, lambda objects,
   etc.  */

void
finish_omp_target_clauses (location_t loc, tree body, tree *clauses_ptr)
{
  omp_target_walk_data data;
  data.this_expr_accessed = false;
  data.current_object = NULL_TREE;

  if (DECL_NONSTATIC_MEMBER_P (current_function_decl) && current_class_ptr)
    if (tree ct = current_nonlambda_class_type ())
      {
	tree object = maybe_dummy_object (ct, NULL);
	object = maybe_resolve_dummy (object, true);
	data.current_object = object;
      }

  if (DECL_LAMBDA_FUNCTION_P (current_function_decl))
    {
      tree closure = DECL_ARGUMENTS (current_function_decl);
      data.current_closure = build_indirect_ref (loc, closure, RO_UNARY_STAR);
    }
  else
    data.current_closure = NULL_TREE;

  cp_walk_tree_without_duplicates (&body, finish_omp_target_clauses_r, &data);

  auto_vec<tree, 16> new_clauses;

  tree omp_target_this_expr = NULL_TREE;
  tree *explicit_this_deref_map = NULL;
  if (data.this_expr_accessed)
    {
      omp_target_this_expr = TREE_OPERAND (data.current_object, 0);

      /* See if explicit user-specified map(this[:]) clause already exists.
	 If not, we create an implicit map(tofrom:this[:1]) clause.  */
      for (tree *cp = clauses_ptr; *cp; cp = &OMP_CLAUSE_CHAIN (*cp))
	if (OMP_CLAUSE_CODE (*cp) == OMP_CLAUSE_MAP
	    && (TREE_CODE (OMP_CLAUSE_DECL (*cp)) == INDIRECT_REF
		|| TREE_CODE (OMP_CLAUSE_DECL (*cp)) == MEM_REF)
	    && operand_equal_p (TREE_OPERAND (OMP_CLAUSE_DECL (*cp), 0),
				omp_target_this_expr))
	  {
	    explicit_this_deref_map = cp;
	    break;
	  }
    }

  if (DECL_LAMBDA_FUNCTION_P (current_function_decl)
      && (data.this_expr_accessed
	  || !data.closure_vars_accessed.is_empty ()))
    {
      /* For lambda functions, we need to first create a copy of the
	 __closure object.  */
      tree closure = DECL_ARGUMENTS (current_function_decl);
      tree c = build_omp_clause (loc, OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_TO);
      OMP_CLAUSE_DECL (c)
	= build_indirect_ref (loc, closure, RO_UNARY_STAR);
      OMP_CLAUSE_SIZE (c)
	= TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (closure)));
      new_clauses.safe_push (c);

      tree closure_obj = OMP_CLAUSE_DECL (c);
      tree closure_type = TREE_TYPE (closure_obj);

      gcc_assert (LAMBDA_TYPE_P (closure_type)
		  && CLASS_TYPE_P (closure_type));

      tree c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_FIRSTPRIVATE_POINTER);
      OMP_CLAUSE_DECL (c2) = closure;
      OMP_CLAUSE_SIZE (c2) = size_zero_node;
      new_clauses.safe_push (c2);
    }

  if (data.this_expr_accessed)
    {
      /* If the this-expr was accessed, create a map(*this) clause.  */
      enum gomp_map_kind kind = GOMP_MAP_TOFROM;
      if (explicit_this_deref_map)
	{
	  tree this_map = *explicit_this_deref_map;
	  tree nc = OMP_CLAUSE_CHAIN (this_map);
	  gcc_assert (nc != NULL_TREE
		      && OMP_CLAUSE_CODE (nc) == OMP_CLAUSE_MAP
		      && (OMP_CLAUSE_MAP_KIND (nc)
			  == GOMP_MAP_FIRSTPRIVATE_POINTER));
	  kind = OMP_CLAUSE_MAP_KIND (this_map);
	  /* Remove the original 'map(*this) map(firstprivate_ptr:this)'
	     two-map sequence away from the chain.  */
	  *explicit_this_deref_map = OMP_CLAUSE_CHAIN (nc);
	}
      tree c = build_omp_clause (loc, OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (c, kind);
      OMP_CLAUSE_DECL (c)
	= build_indirect_ref (loc, omp_target_this_expr, RO_UNARY_STAR);
      OMP_CLAUSE_SIZE (c)
	= TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (omp_target_this_expr)));
      new_clauses.safe_push (c);

      /* If we're in a lambda function, the this-pointer will actually be
	 '__closure->this', a mapped member of __closure, hence always_pointer.
	 Otherwise it's a firstprivate pointer.  */
      enum gomp_map_kind ptr_kind
	= (DECL_LAMBDA_FUNCTION_P (current_function_decl)
	   ? GOMP_MAP_ALWAYS_POINTER
	   : GOMP_MAP_FIRSTPRIVATE_POINTER);
      c = build_omp_clause (loc, OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (c, ptr_kind);
      OMP_CLAUSE_DECL (c) = omp_target_this_expr;
      OMP_CLAUSE_SIZE (c) = size_zero_node;
      new_clauses.safe_push (c);
    }

  if (DECL_LAMBDA_FUNCTION_P (current_function_decl))
    {
      if (omp_target_this_expr)
	{
	  STRIP_NOPS (omp_target_this_expr);
	  gcc_assert (DECL_HAS_VALUE_EXPR_P (omp_target_this_expr));
	  omp_target_this_expr = DECL_VALUE_EXPR (omp_target_this_expr);
	}

      for (hash_set<tree>::iterator i = data.closure_vars_accessed.begin ();
	   i != data.closure_vars_accessed.end (); ++i)
	{
	  tree orig_decl = *i;
	  tree closure_expr = DECL_VALUE_EXPR (orig_decl);

	  if (TREE_CODE (TREE_TYPE (orig_decl)) == POINTER_TYPE
	      || TREE_CODE (TREE_TYPE (orig_decl)) == REFERENCE_TYPE)
	    {
	      /* this-pointer is processed above, outside this loop.  */
	      if (omp_target_this_expr
		  && operand_equal_p (closure_expr, omp_target_this_expr))
		continue;

	      bool ptr_p = TREE_CODE (TREE_TYPE (orig_decl)) == POINTER_TYPE;
	      enum gomp_map_kind kind, ptr_kind, nc_kind;
	      tree size;

	      if (ptr_p)
		{
		  /* For pointers, default mapped as zero-length array
		     section.  */
		  kind = GOMP_MAP_ALLOC;
		  nc_kind = GOMP_MAP_FIRSTPRIVATE_POINTER;
		  ptr_kind = GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION;
		  size = size_zero_node;
		}
	      else
		{
		  /* For references, default mapped as appearing on map
		     clause.  */
		  kind = GOMP_MAP_TOFROM;
		  nc_kind = GOMP_MAP_FIRSTPRIVATE_REFERENCE;
		  ptr_kind = GOMP_MAP_ALWAYS_POINTER;
		  size = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (closure_expr)));
		}

	      for (tree *p = clauses_ptr; *p; p = &OMP_CLAUSE_CHAIN (*p))
		if (OMP_CLAUSE_CODE (*p) == OMP_CLAUSE_MAP
		    && (TREE_CODE (OMP_CLAUSE_DECL (*p)) == INDIRECT_REF
			|| TREE_CODE (OMP_CLAUSE_DECL (*p)) == MEM_REF)
		    && operand_equal_p (TREE_OPERAND (OMP_CLAUSE_DECL (*p), 0),
					orig_decl))
		  {
		    /* If this was already specified by user as a map,
		       save the user specified map kind, delete the
		       "map(*ptr/ref), map(firstprivate ptr/ref)" sequence,
		       and insert our own sequence:
		       "map(*__closure->ptr/ref), map(<ptr_kind>:__closure->ref"
		    */
		    tree nc = OMP_CLAUSE_CHAIN (*p);
		    gcc_assert (nc != NULL_TREE
				&& OMP_CLAUSE_CODE (nc) == OMP_CLAUSE_MAP
				&& OMP_CLAUSE_MAP_KIND (nc) == nc_kind);
		    /* Update with user specified kind and size.  */
		    kind = OMP_CLAUSE_MAP_KIND (*p);
		    size = OMP_CLAUSE_SIZE (*p);
		    *p = OMP_CLAUSE_CHAIN (nc);
		    break;
		  }

	      tree c = build_omp_clause (loc, OMP_CLAUSE_MAP);
	      OMP_CLAUSE_SET_MAP_KIND (c, kind);
	      OMP_CLAUSE_DECL (c)
		= build_indirect_ref (loc, closure_expr, RO_UNARY_STAR);
	      OMP_CLAUSE_SIZE (c) = size;
	      if (ptr_p)
		OMP_CLAUSE_MAP_MAYBE_ZERO_LENGTH_ARRAY_SECTION (c) = 1;
	      new_clauses.safe_push (c);

	      c = build_omp_clause (loc, OMP_CLAUSE_MAP);
	      OMP_CLAUSE_SET_MAP_KIND (c, ptr_kind);
	      OMP_CLAUSE_DECL (c) = closure_expr;
	      OMP_CLAUSE_SIZE (c) = size_zero_node;
	      new_clauses.safe_push (c);
	    }
	}
    }

  if (!data.ptr_members_accessed.is_empty ())
    for (hash_map<tree, tree>::iterator i = data.ptr_members_accessed.begin ();
	 i != data.ptr_members_accessed.end (); ++i)
      {
	/* For each referenced member that is of pointer or reference-to-pointer
	   type, create the equivalent of map(alloc:this->ptr[:0]).  */
	tree field_decl = (*i).first;
	tree ptr_member = (*i).second;

	for (tree c = *clauses_ptr; c; c = OMP_CLAUSE_CHAIN (c))
	  {
	    if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP)
	      continue;
	    /* If map(this->ptr[:N]) already exists, avoid creating another
	       such map.  */
	    tree decl = OMP_CLAUSE_DECL (c);
	    if ((TREE_CODE (decl) == INDIRECT_REF
		 || TREE_CODE (decl) == MEM_REF)
		&& operand_equal_p (TREE_OPERAND (decl, 0), ptr_member))
	      goto next_ptr_member;
	  }

	if (!cxx_mark_addressable (ptr_member))
	  gcc_unreachable ();

	if (TREE_CODE (TREE_TYPE (field_decl)) == REFERENCE_TYPE)
	  {
	    /* For reference to pointers, we need to map the referenced
	       pointer first for things to be correct.  */
	    tree ptr_member_type = TREE_TYPE (ptr_member);

	    /* Map pointer target as zero-length array section.  */
	    tree c = build_omp_clause (loc, OMP_CLAUSE_MAP);
	    OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_ALLOC);
	    OMP_CLAUSE_DECL (c)
	      = build1 (INDIRECT_REF, TREE_TYPE (ptr_member_type), ptr_member);
	    OMP_CLAUSE_SIZE (c) = size_zero_node;
	    OMP_CLAUSE_MAP_MAYBE_ZERO_LENGTH_ARRAY_SECTION (c) = 1;

	    /* Map pointer to zero-length array section.  */
	    tree c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	    OMP_CLAUSE_SET_MAP_KIND
	      (c2, GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION);
	    OMP_CLAUSE_DECL (c2) = ptr_member;
	    OMP_CLAUSE_SIZE (c2) = size_zero_node;

	    /* Attach reference-to-pointer field to pointer.  */
	    tree c3 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	    OMP_CLAUSE_SET_MAP_KIND (c3, GOMP_MAP_ATTACH);
	    OMP_CLAUSE_DECL (c3) = TREE_OPERAND (ptr_member, 0);
	    OMP_CLAUSE_SIZE (c3) = size_zero_node;

	    new_clauses.safe_push (c);
	    new_clauses.safe_push (c2);
	    new_clauses.safe_push (c3);
	  }
	else if (TREE_CODE (TREE_TYPE (field_decl)) == POINTER_TYPE)
	  {
	    /* Map pointer target as zero-length array section.  */
	    tree c = build_omp_clause (loc, OMP_CLAUSE_MAP);
	    OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_ALLOC);
	    OMP_CLAUSE_DECL (c) = build_indirect_ref (loc, ptr_member,
						      RO_UNARY_STAR);
	    OMP_CLAUSE_SIZE (c) = size_zero_node;
	    OMP_CLAUSE_MAP_MAYBE_ZERO_LENGTH_ARRAY_SECTION (c) = 1;

	    /* Attach zero-length array section to pointer.  */
	    tree c2 = build_omp_clause (loc, OMP_CLAUSE_MAP);
	    OMP_CLAUSE_SET_MAP_KIND
	      (c2, GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION);
	    OMP_CLAUSE_DECL (c2) = ptr_member;
	    OMP_CLAUSE_SIZE (c2) = size_zero_node;

	    new_clauses.safe_push (c);
	    new_clauses.safe_push (c2);
	  }
	else
	  gcc_unreachable ();

      next_ptr_member:
	;
      }

  for (hash_set<tree>::iterator i = data.lambda_objects_accessed.begin ();
       i != data.lambda_objects_accessed.end (); ++i)
    {
      tree lobj = *i;
      if (TREE_CODE (lobj) == TARGET_EXPR)
	lobj = TARGET_EXPR_SLOT (lobj);

      tree lt = TREE_TYPE (lobj);
      gcc_assert (LAMBDA_TYPE_P (lt) && CLASS_TYPE_P (lt));

      tree lc = build_omp_clause (loc, OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (lc, GOMP_MAP_TO);
      OMP_CLAUSE_DECL (lc) = lobj;
      OMP_CLAUSE_SIZE (lc) = TYPE_SIZE_UNIT (lt);
      new_clauses.safe_push (lc);

      for (tree fld = TYPE_FIELDS (lt); fld; fld = DECL_CHAIN (fld))
	{
	  if (TREE_CODE (TREE_TYPE (fld)) == POINTER_TYPE)
	    {
	      tree exp = build3 (COMPONENT_REF, TREE_TYPE (fld),
				 lobj, fld, NULL_TREE);
	      tree c = build_omp_clause (loc, OMP_CLAUSE_MAP);
	      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_ALLOC);
	      OMP_CLAUSE_DECL (c)
		= build_indirect_ref (loc, exp, RO_UNARY_STAR);
	      OMP_CLAUSE_SIZE (c) = size_zero_node;
	      OMP_CLAUSE_MAP_MAYBE_ZERO_LENGTH_ARRAY_SECTION (c) = 1;
	      new_clauses.safe_push (c);

	      c = build_omp_clause (loc, OMP_CLAUSE_MAP);
	      OMP_CLAUSE_SET_MAP_KIND
		(c, GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION);
	      OMP_CLAUSE_DECL (c) = exp;
	      OMP_CLAUSE_SIZE (c) = size_zero_node;
	      new_clauses.safe_push (c);
	    }
	  else if (TREE_CODE (TREE_TYPE (fld)) == REFERENCE_TYPE)
	    {
	      tree exp = build3 (COMPONENT_REF, TREE_TYPE (fld),
				 lobj, fld, NULL_TREE);
	      tree c = build_omp_clause (loc, OMP_CLAUSE_MAP);
	      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_TOFROM);
	      OMP_CLAUSE_DECL (c)
		= build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (exp)), exp);
	      OMP_CLAUSE_SIZE (c)
		= TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (exp)));
	      new_clauses.safe_push (c);

	      c = build_omp_clause (loc, OMP_CLAUSE_MAP);
	      OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_ALWAYS_POINTER);
	      OMP_CLAUSE_DECL (c) = exp;
	      OMP_CLAUSE_SIZE (c) = size_zero_node;
	      new_clauses.safe_push (c);
	    }
	}
    }

  tree c = *clauses_ptr;
  for (int i = new_clauses.length () - 1; i >= 0; i--)
    {
      OMP_CLAUSE_CHAIN (new_clauses[i]) = c;
      c = new_clauses[i];
    }
  *clauses_ptr = c;
}

/* Called from cp_parser_omp_target.  Create additional implicit clauses for
   OpenMP target directives, and do sanity checks.  */

tree
finish_omp_target (location_t loc, tree clauses, tree body, bool combined_p)
{
  if (!processing_template_decl)
    finish_omp_target_clauses (loc, body, &clauses);

  tree stmt = make_node (OMP_TARGET);
  TREE_TYPE (stmt) = void_type_node;
  OMP_TARGET_CLAUSES (stmt) = clauses;
  OMP_TARGET_BODY (stmt) = body;
  OMP_TARGET_COMBINED (stmt) = combined_p;
  SET_EXPR_LOCATION (stmt, loc);

  tree c = clauses;
  while (c)
    {
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP)
	switch (OMP_CLAUSE_MAP_KIND (c))
	  {
	  case GOMP_MAP_TO:
	  case GOMP_MAP_ALWAYS_TO:
	  case GOMP_MAP_PRESENT_TO:
	  case GOMP_MAP_ALWAYS_PRESENT_TO:
	  case GOMP_MAP_FROM:
	  case GOMP_MAP_ALWAYS_FROM:
	  case GOMP_MAP_PRESENT_FROM:
	  case GOMP_MAP_ALWAYS_PRESENT_FROM:
	  case GOMP_MAP_TOFROM:
	  case GOMP_MAP_ALWAYS_TOFROM:
	  case GOMP_MAP_PRESENT_TOFROM:
	  case GOMP_MAP_ALWAYS_PRESENT_TOFROM:
	  case GOMP_MAP_ALLOC:
	  case GOMP_MAP_PRESENT_ALLOC:
	  case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  case GOMP_MAP_FIRSTPRIVATE_REFERENCE:
	  case GOMP_MAP_ALWAYS_POINTER:
	  case GOMP_MAP_ATTACH_DETACH:
	  case GOMP_MAP_ATTACH:
	  case GOMP_MAP_ATTACH_ZERO_LENGTH_ARRAY_SECTION:
	  case GOMP_MAP_POINTER:
	  case GOMP_MAP_POINTER_TO_ZERO_LENGTH_ARRAY_SECTION:
	    break;
	  default:
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%<#pragma omp target%> with map-type other "
		      "than %<to%>, %<from%>, %<tofrom%> or %<alloc%> "
		      "on %<map%> clause");
	    break;
	  }
      c = OMP_CLAUSE_CHAIN (c);
    }
  return add_stmt (stmt);
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
handle_omp_for_class_iterator (int i, location_t locus, enum tree_code code,
			       tree declv, tree orig_declv, tree initv,
			       tree condv, tree incrv, tree *body,
			       tree *pre_body, tree &clauses,
			       int collapse, int ordered)
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
    case NE_EXPR:
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
					NULL_TREE, NULL, tf_warning_or_error);
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
  diff = build_x_binary_op (elocus, MINUS_EXPR,
			    TREE_OPERAND (cond, 1), ERROR_MARK,
			    iter, ERROR_MARK,
			    NULL_TREE, NULL, tf_warning_or_error);
  diff = cp_fully_fold (diff);
  if (error_operand_p (diff))
    return true;
  if (TREE_CODE (TREE_TYPE (diff)) != INTEGER_TYPE)
    {
      error_at (elocus, "difference between %qE and %qD does not have integer type",
		TREE_OPERAND (cond, 1), iter);
      return true;
    }
  if (!c_omp_check_loop_iv_exprs (locus, code, orig_declv, i,
				  TREE_VEC_ELT (declv, i), NULL_TREE,
				  cond, cp_walk_subtrees))
    return true;

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
				    NULL_TREE, tf_warning_or_error);
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
						   NULL_TREE,
						   tf_warning_or_error);
		  if (error_operand_p (iter_incr))
		    return true;
		  incr = TREE_OPERAND (rhs, 1);
		  incr = cp_convert (TREE_TYPE (diff), incr,
				     tf_warning_or_error);
		  if (TREE_CODE (rhs) == MINUS_EXPR)
		    {
		      incr = build1 (NEGATE_EXPR, TREE_TYPE (diff), incr);
		      incr = fold_simple (incr);
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
						 ERROR_MARK, NULL_TREE, NULL,
						 tf_warning_or_error);
		  if (error_operand_p (iter_incr))
		    return true;
		  iter_incr = build_x_modify_expr (EXPR_LOCATION (rhs),
						   iter, NOP_EXPR,
						   iter_incr, NULL_TREE,
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
  incr = cp_fully_fold (incr);
  tree loop_iv_seen = NULL_TREE;
  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	&& OMP_CLAUSE_DECL (c) == iter)
      {
	if (code == OMP_TASKLOOP || code == OMP_LOOP)
	  {
	    loop_iv_seen = c;
	    OMP_CLAUSE_LASTPRIVATE_LOOP_IV (c) = 1;
	  }
	break;
      }
    else if ((code == OMP_TASKLOOP || code == OMP_LOOP)
	     && OMP_CLAUSE_CODE (c) == OMP_CLAUSE_PRIVATE
	     && OMP_CLAUSE_DECL (c) == iter)
      {
	loop_iv_seen = c;
	if (code == OMP_TASKLOOP)
	  OMP_CLAUSE_PRIVATE_TASKLOOP_IV (c) = 1;
      }

  decl = create_temporary_var (TREE_TYPE (diff));
  pushdecl (decl);
  add_decl_expr (decl);
  last = create_temporary_var (TREE_TYPE (diff));
  pushdecl (last);
  add_decl_expr (last);
  if (c && iter_incr == NULL && TREE_CODE (incr) != INTEGER_CST
      && (!ordered || (i < collapse && collapse > 1)))
    {
      incr_var = create_temporary_var (TREE_TYPE (diff));
      pushdecl (incr_var);
      add_decl_expr (incr_var);
    }
  gcc_assert (stmts_are_full_exprs_p ());
  tree diffvar = NULL_TREE;
  if (code == OMP_TASKLOOP)
    {
      if (!loop_iv_seen)
	{
	  tree ivc = build_omp_clause (locus, OMP_CLAUSE_FIRSTPRIVATE);
	  OMP_CLAUSE_DECL (ivc) = iter;
	  cxx_omp_finish_clause (ivc, NULL, false);
	  OMP_CLAUSE_CHAIN (ivc) = clauses;
	  clauses = ivc;
	}
      tree lvc = build_omp_clause (locus, OMP_CLAUSE_FIRSTPRIVATE);
      OMP_CLAUSE_DECL (lvc) = last;
      OMP_CLAUSE_CHAIN (lvc) = clauses;
      clauses = lvc;
      diffvar = create_temporary_var (TREE_TYPE (diff));
      pushdecl (diffvar);
      add_decl_expr (diffvar);
    }
  else if (code == OMP_LOOP)
    {
      if (!loop_iv_seen)
	{
	  /* While iterators on the loop construct are predetermined
	     lastprivate, if the decl is not declared inside of the
	     loop, OMP_CLAUSE_LASTPRIVATE should have been added
	     already.  */
	  loop_iv_seen = build_omp_clause (locus, OMP_CLAUSE_FIRSTPRIVATE);
	  OMP_CLAUSE_DECL (loop_iv_seen) = iter;
	  OMP_CLAUSE_CHAIN (loop_iv_seen) = clauses;
	  clauses = loop_iv_seen;
	}
      else if (OMP_CLAUSE_CODE (loop_iv_seen) == OMP_CLAUSE_PRIVATE)
	{
	  OMP_CLAUSE_PRIVATE_DEBUG (loop_iv_seen) = 0;
	  OMP_CLAUSE_PRIVATE_OUTER_REF (loop_iv_seen) = 0;
	  OMP_CLAUSE_CODE (loop_iv_seen) = OMP_CLAUSE_FIRSTPRIVATE;
	}
      if (OMP_CLAUSE_CODE (loop_iv_seen) == OMP_CLAUSE_FIRSTPRIVATE)
	cxx_omp_finish_clause (loop_iv_seen, NULL, false);
    }

  orig_pre_body = *pre_body;
  *pre_body = push_stmt_list ();
  if (orig_pre_body)
    add_stmt (orig_pre_body);
  if (init != NULL)
    finish_expr_stmt (build_x_modify_expr (elocus,
					   iter, NOP_EXPR, init,
					   NULL_TREE, tf_warning_or_error));
  init = build_int_cst (TREE_TYPE (diff), 0);
  if (c && iter_incr == NULL
      && (!ordered || (i < collapse && collapse > 1)))
    {
      if (incr_var)
	{
	  finish_expr_stmt (build_x_modify_expr (elocus,
						 incr_var, NOP_EXPR,
						 incr, NULL_TREE,
						 tf_warning_or_error));
	  incr = incr_var;
	}
      iter_incr = build_x_modify_expr (elocus,
				       iter, PLUS_EXPR, incr,
				       NULL_TREE, tf_warning_or_error);
    }
  if (c && ordered && i < collapse && collapse > 1)
    iter_incr = incr;
  finish_expr_stmt (build_x_modify_expr (elocus,
					 last, NOP_EXPR, init,
					 NULL_TREE, tf_warning_or_error));
  if (diffvar)
    {
      finish_expr_stmt (build_x_modify_expr (elocus,
					     diffvar, NOP_EXPR,
					     diff, NULL_TREE, tf_warning_or_error));
      diff = diffvar;
    }
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
				   NULL_TREE, tf_warning_or_error);
  if (iter_init != error_mark_node)
    iter_init = build1 (NOP_EXPR, void_type_node, iter_init);
  finish_expr_stmt (iter_init);
  finish_expr_stmt (build_x_modify_expr (elocus,
					 last, NOP_EXPR, decl,
					 NULL_TREE, tf_warning_or_error));
  add_stmt (orig_body);
  *body = pop_stmt_list (*body);

  if (c)
    {
      OMP_CLAUSE_LASTPRIVATE_STMT (c) = push_stmt_list ();
      if (!ordered)
	finish_expr_stmt (iter_incr);
      else
	{
	  iter_init = decl;
	  if (i < collapse && collapse > 1 && !error_operand_p (iter_incr))
	    iter_init = build2 (PLUS_EXPR, TREE_TYPE (diff),
				iter_init, iter_incr);
	  iter_init = build2 (MINUS_EXPR, TREE_TYPE (diff), iter_init, last);
	  iter_init = build_x_modify_expr (elocus,
					   iter, PLUS_EXPR, iter_init,
					   NULL_TREE, tf_warning_or_error);
	  if (iter_init != error_mark_node)
	    iter_init = build1 (NOP_EXPR, void_type_node, iter_init);
	  finish_expr_stmt (iter_init);
	}
      OMP_CLAUSE_LASTPRIVATE_STMT (c)
	= pop_stmt_list (OMP_CLAUSE_LASTPRIVATE_STMT (c));
    }

  if (TREE_CODE (TREE_VEC_ELT (orig_declv, i)) == TREE_LIST)
    {
      tree t = TREE_VEC_ELT (orig_declv, i);
      gcc_assert (TREE_PURPOSE (t) == NULL_TREE
		  && TREE_VALUE (t) == NULL_TREE
		  && TREE_CODE (TREE_CHAIN (t)) == TREE_VEC);
      TREE_PURPOSE (t) = TREE_VEC_ELT (declv, i);
      TREE_VALUE (t) = last;
    }
  else
    TREE_VEC_ELT (orig_declv, i)
      = tree_cons (TREE_VEC_ELT (declv, i), last, NULL_TREE);
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
finish_omp_for (location_t locus, enum tree_code code, tree declv,
		tree orig_declv, tree initv, tree condv, tree incrv,
		tree body, tree pre_body, vec<tree> *orig_inits, tree clauses)
{
  tree omp_for = NULL, orig_incr = NULL;
  tree decl = NULL, init, cond, incr;
  location_t elocus;
  int i;
  int collapse = 1;
  int ordered = 0;
  auto_vec<location_t> init_locv;

  gcc_assert (TREE_VEC_LENGTH (declv) == TREE_VEC_LENGTH (initv));
  gcc_assert (TREE_VEC_LENGTH (declv) == TREE_VEC_LENGTH (condv));
  gcc_assert (TREE_VEC_LENGTH (declv) == TREE_VEC_LENGTH (incrv));
  if (TREE_VEC_LENGTH (declv) > 1)
    {
      if (tree ti = omp_find_clause (clauses, OMP_CLAUSE_TILE))
	collapse = list_length (OMP_CLAUSE_TILE_LIST (ti));
      else
	{
	  if (tree co = omp_find_clause (clauses, OMP_CLAUSE_COLLAPSE))
	    collapse = tree_to_shwi (OMP_CLAUSE_COLLAPSE_EXPR (co));
	  else if (tree si = omp_find_clause (clauses, OMP_CLAUSE_SIZES))
	    collapse = list_length (OMP_CLAUSE_SIZES_LIST (si));
	  if (collapse != TREE_VEC_LENGTH (declv))
	    ordered = TREE_VEC_LENGTH (declv);
	}
    }
  for (i = 0; i < TREE_VEC_LENGTH (declv); i++)
    {
      decl = TREE_VEC_ELT (declv, i);
      init = TREE_VEC_ELT (initv, i);
      cond = TREE_VEC_ELT (condv, i);
      incr = TREE_VEC_ELT (incrv, i);
      elocus = locus;

      if (decl == global_namespace)
	{
	  gcc_assert (init == NULL_TREE && cond == NULL_TREE && incr == NULL_TREE);
	  TREE_VEC_ELT (declv, i) = NULL_TREE;
	  init_locv.safe_push (UNKNOWN_LOCATION);
	  continue;
	}
      /* We are going to throw out the init's original MODIFY_EXPR or
	 MODOP_EXPR below.  Save its location so we can use it when
	 reconstructing the expression farther down.  Alternatively, if the
	 initializer is a binding of the iteration variable, save
	 that location.  Any of these locations in the initialization clause
	 for the current nested loop are better than using the argument locus,
	 that points to the "for" of the outermost loop in the nest.  */
      if (init && EXPR_HAS_LOCATION (init))
	elocus = EXPR_LOCATION (init);
      else if (decl && INDIRECT_REF_P (decl) && EXPR_HAS_LOCATION (decl))
	/* This can happen for class iterators.  */
	elocus = EXPR_LOCATION (decl);
      else if (decl && DECL_P (decl))
	{
	  if (DECL_SOURCE_LOCATION (decl) != UNKNOWN_LOCATION)
	    elocus = DECL_SOURCE_LOCATION (decl);
	  else if (DECL_INITIAL (decl)
		   && EXPR_HAS_LOCATION (DECL_INITIAL (decl)))
	    elocus = EXPR_LOCATION (DECL_INITIAL (decl));
	}
      init_locv.safe_push (elocus);

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

      if (cond == global_namespace)
	continue;

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

  if (orig_inits)
    {
      bool fail = false;
      tree orig_init;
      FOR_EACH_VEC_ELT (*orig_inits, i, orig_init)
	if (orig_init
	    && !c_omp_check_loop_iv_exprs (locus, code,
					   orig_declv ? orig_declv : declv, i,
					   TREE_VEC_ELT (declv, i), orig_init,
					   NULL_TREE, cp_walk_subtrees))
	  fail = true;
      if (fail)
	return NULL;
    }

  if (dependent_omp_for_p (declv, initv, condv, incrv, body))
    {
      tree stmt;

      stmt = make_node (code);

      for (i = 0; i < TREE_VEC_LENGTH (declv); i++)
	{
	  if (TREE_VEC_ELT (declv, i) == NULL_TREE)
	    continue;
	  /* This is really just a place-holder.  We'll be decomposing this
	     again and going through the cp_build_modify_expr path below when
	     we instantiate the thing.  */
	  TREE_VEC_ELT (initv, i)
	    = build2_loc (init_locv[i], MODIFY_EXPR, void_type_node,
			  TREE_VEC_ELT (declv, i), TREE_VEC_ELT (initv, i));
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

  if (!orig_declv)
    orig_declv = copy_node (declv);

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
      elocus = init_locv[i];

      if (decl == NULL_TREE)
	{
	  i++;
	  continue;
	}

      if (!DECL_P (decl))
	{
	  error_at (elocus, "expected iteration declaration or initialization");
	  return NULL;
	}

      if (incr && TREE_CODE (incr) == MODOP_EXPR)
	{
	  if (orig_incr)
	    TREE_VEC_ELT (orig_incr, i) = incr;
	  incr = cp_build_modify_expr (elocus, TREE_OPERAND (incr, 0),
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
	  if (handle_omp_for_class_iterator (i, locus, code, declv, orig_declv,
					     initv, condv, incrv, &body,
					     &pre_body, clauses,
					     collapse, ordered))
	    return NULL;
	  continue;
	}

      if (!INTEGRAL_TYPE_P (TREE_TYPE (decl))
	  && !TYPE_PTR_P (TREE_TYPE (decl)))
	{
	  error_at (elocus, "invalid type for iteration variable %qE", decl);
	  return NULL;
	}

      if (!processing_template_decl && TREE_CODE (init) != TREE_VEC)
	init = cp_build_modify_expr (elocus, decl, NOP_EXPR, init,
				     tf_warning_or_error);
      else
	init = build2_loc (elocus, MODIFY_EXPR, void_type_node, decl, init);
      if (decl == error_mark_node || init == error_mark_node)
	return NULL;

      TREE_VEC_ELT (declv, i) = decl;
      TREE_VEC_ELT (initv, i) = init;
      TREE_VEC_ELT (condv, i) = cond;
      TREE_VEC_ELT (incrv, i) = incr;
      i++;
    }

  if (pre_body && IS_EMPTY_STMT (pre_body))
    pre_body = NULL;

  omp_for = c_finish_omp_for (locus, code, declv, orig_declv, initv, condv,
			      incrv, body, pre_body,
			      !processing_template_decl);

  /* Check for iterators appearing in lb, b or incr expressions.  */
  if (omp_for && !c_omp_check_loop_iv (omp_for, orig_declv, cp_walk_subtrees))
    omp_for = NULL_TREE;

  if (omp_for == NULL)
    return NULL;

  add_stmt (omp_for);

  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INCR (omp_for)); i++)
    {
      init = TREE_VEC_ELT (OMP_FOR_INIT (omp_for), i);
      if (init == NULL_TREE)
	continue;
      decl = TREE_OPERAND (init, 0);
      cond = TREE_VEC_ELT (OMP_FOR_COND (omp_for), i);
      incr = TREE_VEC_ELT (OMP_FOR_INCR (omp_for), i);

      if (!processing_template_decl)
	{
	  if (TREE_CODE (TREE_OPERAND (init, 1)) == TREE_VEC)
	    {
	      tree t = TREE_VEC_ELT (TREE_OPERAND (init, 1), 1);
	      TREE_VEC_ELT (TREE_OPERAND (init, 1), 1)
		= fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	      t = TREE_VEC_ELT (TREE_OPERAND (init, 1), 2);
	      TREE_VEC_ELT (TREE_OPERAND (init, 1), 2)
		= fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	    }
	  else
	    {
	      tree t = TREE_OPERAND (init, 1);
	      TREE_OPERAND (init, 1)
		= fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	    }
	  if (TREE_CODE (TREE_OPERAND (cond, 1)) == TREE_VEC)
	    {
	      tree t = TREE_VEC_ELT (TREE_OPERAND (cond, 1), 1);
	      TREE_VEC_ELT (TREE_OPERAND (cond, 1), 1)
		= fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	      t = TREE_VEC_ELT (TREE_OPERAND (cond, 1), 2);
	      TREE_VEC_ELT (TREE_OPERAND (cond, 1), 2)
		= fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	    }
	  else
	    {
	      tree t = TREE_OPERAND (cond, 1);
	      TREE_OPERAND (cond, 1)
		= fold_build_cleanup_point_expr (TREE_TYPE (t), t);
	    }
	}

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
  OMP_FOR_CLAUSES (omp_for) = clauses;

  /* For simd loops with non-static data member iterators, we could have added
     OMP_CLAUSE_LINEAR clauses without OMP_CLAUSE_LINEAR_STEP.  As we know the
     step at this point, fill it in.  */
  if (code == OMP_SIMD && !processing_template_decl
      && TREE_VEC_LENGTH (OMP_FOR_INCR (omp_for)) == 1)
    for (tree c = omp_find_clause (clauses, OMP_CLAUSE_LINEAR); c;
	 c = omp_find_clause (OMP_CLAUSE_CHAIN (c), OMP_CLAUSE_LINEAR))
      if (OMP_CLAUSE_LINEAR_STEP (c) == NULL_TREE)
	{
	  decl = TREE_OPERAND (TREE_VEC_ELT (OMP_FOR_INIT (omp_for), 0), 0);
	  gcc_assert (decl == OMP_CLAUSE_DECL (c));
	  incr = TREE_VEC_ELT (OMP_FOR_INCR (omp_for), 0);
	  tree step, stept;
	  switch (TREE_CODE (incr))
	    {
	    case PREINCREMENT_EXPR:
	    case POSTINCREMENT_EXPR:
	      /* c_omp_for_incr_canonicalize_ptr() should have been
		 called to massage things appropriately.  */
	      gcc_assert (!INDIRECT_TYPE_P (TREE_TYPE (decl)));
	      OMP_CLAUSE_LINEAR_STEP (c) = build_int_cst (TREE_TYPE (decl), 1);
	      break;
	    case PREDECREMENT_EXPR:
	    case POSTDECREMENT_EXPR:
	      /* c_omp_for_incr_canonicalize_ptr() should have been
		 called to massage things appropriately.  */
	      gcc_assert (!INDIRECT_TYPE_P (TREE_TYPE (decl)));
	      OMP_CLAUSE_LINEAR_STEP (c)
		= build_int_cst (TREE_TYPE (decl), -1);
	      break;
	    case MODIFY_EXPR:
	      gcc_assert (TREE_OPERAND (incr, 0) == decl);
	      incr = TREE_OPERAND (incr, 1);
	      switch (TREE_CODE (incr))
		{
		case PLUS_EXPR:
		  if (TREE_OPERAND (incr, 1) == decl)
		    step = TREE_OPERAND (incr, 0);
		  else
		    step = TREE_OPERAND (incr, 1);
		  break;
		case MINUS_EXPR:
		case POINTER_PLUS_EXPR:
		  gcc_assert (TREE_OPERAND (incr, 0) == decl);
		  step = TREE_OPERAND (incr, 1);
		  break;
		default:
		  gcc_unreachable ();
		}
	      stept = TREE_TYPE (decl);
	      if (INDIRECT_TYPE_P (stept))
		stept = sizetype;
	      step = fold_convert (stept, step);
	      if (TREE_CODE (incr) == MINUS_EXPR)
		step = fold_build1 (NEGATE_EXPR, stept, step);
	      OMP_CLAUSE_LINEAR_STEP (c) = step;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}
  /* Override saved methods on OMP_LOOP's OMP_CLAUSE_LASTPRIVATE_LOOP_IV
     clauses, we need copy ctor for those rather than default ctor,
     plus as for other lastprivates assignment op and dtor.  */
  if (code == OMP_LOOP && !processing_template_decl)
    for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
	  && OMP_CLAUSE_LASTPRIVATE_LOOP_IV (c)
	  && cxx_omp_create_clause_info (c, TREE_TYPE (OMP_CLAUSE_DECL (c)),
					 false, true, true, true))
	CP_OMP_CLAUSE_INFO (c) = NULL_TREE;

  return omp_for;
}

/* Code walker for finish_omp_for_block: extract binding of DP->var
   from its current block and move it to a new BIND_EXPR DP->b
   surrounding the body of DP->omp_for.  */

struct fofb_data {
  tree var;
  tree b;
  tree omp_for;
};

static tree
finish_omp_for_block_walker (tree *tp, int *walk_subtrees, void *dp)
{
  struct fofb_data *fofb = (struct fofb_data *)dp;
  if (TREE_CODE (*tp) == BIND_EXPR)
    for (tree *p = &BIND_EXPR_VARS (*tp); *p; p = &DECL_CHAIN (*p))
      {
	if (*p == fofb->var)
	  {
	    *p = DECL_CHAIN (*p);
	    if (fofb->b == NULL_TREE)
	      {
		fofb->b = make_node (BLOCK);
		fofb->b = build3 (BIND_EXPR, void_type_node, NULL_TREE,
			    OMP_FOR_BODY (fofb->omp_for), fofb->b);
		TREE_SIDE_EFFECTS (fofb->b) = 1;
		OMP_FOR_BODY (fofb->omp_for) = fofb->b;
	      }
	    DECL_CHAIN (fofb->var) = BIND_EXPR_VARS (fofb->b);
	    BIND_EXPR_VARS (fofb->b) = fofb->var;
	    BLOCK_VARS (BIND_EXPR_BLOCK (fofb->b)) = fofb->var;
	    BLOCK_VARS (BIND_EXPR_BLOCK (*tp)) = BIND_EXPR_VARS (*tp);
	    return *tp;
	  }
      }
  if (TREE_CODE (*tp) != BIND_EXPR && TREE_CODE (*tp) != STATEMENT_LIST)
    *walk_subtrees = false;
  return NULL_TREE;
}

/* Fix up range for decls.  Those decls were pushed into BIND's
   BIND_EXPR_VARS, or that of a nested BIND_EXPR inside its body,
   and need to be moved into a new BIND_EXPR surrounding OMP_FOR's body
   so that processing of combined loop directives can find them.  */
tree
finish_omp_for_block (tree bind, tree omp_for)
{
  if (omp_for == NULL_TREE
      || !OMP_FOR_ORIG_DECLS (omp_for)
      || bind == NULL_TREE)
    return bind;
  struct fofb_data fofb;
  fofb.b = NULL_TREE;
  fofb.omp_for = omp_for;
  for (int i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (omp_for)); i++)
    if (TREE_VEC_ELT (OMP_FOR_INIT (omp_for), i)
	&& (TREE_CODE (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (omp_for), i))
	    == TREE_LIST)
	&& TREE_CHAIN (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (omp_for), i)))
      {
	tree v = TREE_CHAIN (TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (omp_for), i));
	for (int j = 2; j < TREE_VEC_LENGTH (v); j++)
	  {
	    fofb.var = TREE_VEC_ELT (v, j);
	    cp_walk_tree (&bind, finish_omp_for_block_walker,
			  (void *)&fofb, NULL);
	  }
      }
  return bind;
}

void
finish_omp_atomic (location_t loc, enum tree_code code, enum tree_code opcode,
		   tree lhs, tree rhs, tree v, tree lhs1, tree rhs1, tree r,
		   tree clauses, enum omp_memory_order mo, bool weak)
{
  tree orig_lhs;
  tree orig_rhs;
  tree orig_v;
  tree orig_lhs1;
  tree orig_rhs1;
  tree orig_r;
  bool dependent_p;
  tree stmt;

  orig_lhs = lhs;
  orig_rhs = rhs;
  orig_v = v;
  orig_lhs1 = lhs1;
  orig_rhs1 = rhs1;
  orig_r = r;
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
		     || (rhs1 && type_dependent_expression_p (rhs1))
		     || (r
			 && r != void_list_node
			 && type_dependent_expression_p (r)));
      if (clauses)
	{
	  gcc_assert (TREE_CODE (clauses) == OMP_CLAUSE
		      && OMP_CLAUSE_CODE (clauses) == OMP_CLAUSE_HINT
		      && OMP_CLAUSE_CHAIN (clauses) == NULL_TREE);
	  if (type_dependent_expression_p (OMP_CLAUSE_HINT_EXPR (clauses))
	      || TREE_CODE (OMP_CLAUSE_HINT_EXPR (clauses)) != INTEGER_CST)
	    dependent_p = true;
	}
    }
  if (!dependent_p)
    {
      bool swapped = false;
      if (rhs1 && opcode != COND_EXPR && cp_tree_equal (lhs, rhs))
	{
	  std::swap (rhs, rhs1);
	  swapped = !commutative_tree_code (opcode);
	}
      if (rhs1 && opcode != COND_EXPR && !cp_tree_equal (lhs, rhs1))
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
      stmt = c_finish_omp_atomic (loc, code, opcode, lhs, rhs,
				  v, lhs1, rhs1, r, swapped, mo, weak,
				  processing_template_decl != 0);
      if (stmt == error_mark_node)
	return;
    }
  if (processing_template_decl)
    {
      if (code == OMP_ATOMIC_READ)
	{
	  stmt = build_min_nt_loc (loc, OMP_ATOMIC_READ, orig_lhs);
	  OMP_ATOMIC_MEMORY_ORDER (stmt) = mo;
	  stmt = build2 (MODIFY_EXPR, void_type_node, orig_v, stmt);
	}
      else
	{
	  if (opcode == NOP_EXPR)
	    stmt = build2 (MODIFY_EXPR, void_type_node, orig_lhs, orig_rhs);
	  else if (opcode == COND_EXPR)
	    {
	      stmt = build2 (EQ_EXPR, boolean_type_node, orig_lhs, orig_rhs);
	      if (orig_r)
		stmt = build2 (MODIFY_EXPR, boolean_type_node, orig_r,
			       stmt);
	      stmt = build3 (COND_EXPR, void_type_node, stmt, orig_rhs1,
			     orig_lhs);
	      orig_rhs1 = NULL_TREE;
	    }
	  else
	    stmt = build2 (opcode, void_type_node, orig_lhs, orig_rhs);
	  if (orig_rhs1)
	    stmt = build_min_nt_loc (EXPR_LOCATION (orig_rhs1),
				     COMPOUND_EXPR, orig_rhs1, stmt);
	  if (code != OMP_ATOMIC)
	    {
	      stmt = build_min_nt_loc (loc, code, orig_lhs1, stmt);
	      OMP_ATOMIC_MEMORY_ORDER (stmt) = mo;
	      OMP_ATOMIC_WEAK (stmt) = weak;
	      stmt = build2 (MODIFY_EXPR, void_type_node, orig_v, stmt);
	    }
	}
      stmt = build2 (OMP_ATOMIC, void_type_node,
		     clauses ? clauses : integer_zero_node, stmt);
      OMP_ATOMIC_MEMORY_ORDER (stmt) = mo;
      OMP_ATOMIC_WEAK (stmt) = weak;
      SET_EXPR_LOCATION (stmt, loc);
    }

  /* Avoid -Wunused-value warnings here, the whole construct has side-effects
     and even if it might be wrapped from fold-const.cc or c-omp.cc wrapped
     in some tree that appears to be unused, the value is not unused.  */
  warning_sentinel w (warn_unused_value);
  finish_expr_stmt (stmt);
}

void
finish_omp_barrier (void)
{
  tree fn = builtin_decl_explicit (BUILT_IN_GOMP_BARRIER);
  releasing_vec vec;
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  finish_expr_stmt (stmt);
}

void
finish_omp_depobj (location_t loc, tree depobj,
		   enum omp_clause_depend_kind kind, tree clause)
{
  if (!error_operand_p (depobj) && !type_dependent_expression_p (depobj))
    {
      if (!lvalue_p (depobj))
	{
	  error_at (EXPR_LOC_OR_LOC (depobj, loc),
		    "%<depobj%> expression is not lvalue expression");
	  depobj = error_mark_node;
	}
    }

  if (processing_template_decl)
    {
      if (clause == NULL_TREE)
	clause = build_int_cst (integer_type_node, kind);
      add_stmt (build_min_nt_loc (loc, OMP_DEPOBJ, depobj, clause));
      return;
    }

  if (!error_operand_p (depobj))
    {
      tree addr = cp_build_addr_expr (depobj, tf_warning_or_error);
      if (addr == error_mark_node)
	depobj = error_mark_node;
      else
	depobj = cp_build_indirect_ref (loc, addr, RO_UNARY_STAR,
					tf_warning_or_error);
    }

  c_finish_omp_depobj (loc, depobj, kind, clause);
}

void
finish_omp_flush (int mo)
{
  tree fn = builtin_decl_explicit (BUILT_IN_SYNC_SYNCHRONIZE);
  releasing_vec vec;
  if (mo != MEMMODEL_LAST && mo != MEMMODEL_SEQ_CST)
    {
      fn = builtin_decl_explicit (BUILT_IN_ATOMIC_THREAD_FENCE);
      vec->quick_push (build_int_cst (integer_type_node, mo));
    }
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  finish_expr_stmt (stmt);
}

void
finish_omp_taskwait (void)
{
  tree fn = builtin_decl_explicit (BUILT_IN_GOMP_TASKWAIT);
  releasing_vec vec;
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  finish_expr_stmt (stmt);
}

void
finish_omp_taskyield (void)
{
  tree fn = builtin_decl_explicit (BUILT_IN_GOMP_TASKYIELD);
  releasing_vec vec;
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  finish_expr_stmt (stmt);
}

void
finish_omp_cancel (tree clauses)
{
  tree fn = builtin_decl_explicit (BUILT_IN_GOMP_CANCEL);
  int mask = 0;
  if (omp_find_clause (clauses, OMP_CLAUSE_PARALLEL))
    mask = 1;
  else if (omp_find_clause (clauses, OMP_CLAUSE_FOR))
    mask = 2;
  else if (omp_find_clause (clauses, OMP_CLAUSE_SECTIONS))
    mask = 4;
  else if (omp_find_clause (clauses, OMP_CLAUSE_TASKGROUP))
    mask = 8;
  else
    {
      error ("%<#pragma omp cancel%> must specify one of "
	     "%<parallel%>, %<for%>, %<sections%> or %<taskgroup%> clauses");
      return;
    }
  releasing_vec vec;
  tree ifc = omp_find_clause (clauses, OMP_CLAUSE_IF);
  if (ifc != NULL_TREE)
    {
      if (OMP_CLAUSE_IF_MODIFIER (ifc) != ERROR_MARK
	  && OMP_CLAUSE_IF_MODIFIER (ifc) != VOID_CST)
	error_at (OMP_CLAUSE_LOCATION (ifc),
		  "expected %<cancel%> %<if%> clause modifier");
      else
	{
	  tree ifc2 = omp_find_clause (OMP_CLAUSE_CHAIN (ifc), OMP_CLAUSE_IF);
	  if (ifc2 != NULL_TREE)
	    {
	      gcc_assert (OMP_CLAUSE_IF_MODIFIER (ifc) == VOID_CST
			  && OMP_CLAUSE_IF_MODIFIER (ifc2) != ERROR_MARK
			  && OMP_CLAUSE_IF_MODIFIER (ifc2) != VOID_CST);
	      error_at (OMP_CLAUSE_LOCATION (ifc2),
			"expected %<cancel%> %<if%> clause modifier");
	    }
	}

      if (!processing_template_decl)
	ifc = maybe_convert_cond (OMP_CLAUSE_IF_EXPR (ifc));
      else
	ifc = build_x_binary_op (OMP_CLAUSE_LOCATION (ifc), NE_EXPR,
				 OMP_CLAUSE_IF_EXPR (ifc), ERROR_MARK,
				 integer_zero_node, ERROR_MARK,
				 NULL_TREE, NULL, tf_warning_or_error);
    }
  else
    ifc = boolean_true_node;
  vec->quick_push (build_int_cst (integer_type_node, mask));
  vec->quick_push (ifc);
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
  finish_expr_stmt (stmt);
}

void
finish_omp_cancellation_point (tree clauses)
{
  tree fn = builtin_decl_explicit (BUILT_IN_GOMP_CANCELLATION_POINT);
  int mask = 0;
  if (omp_find_clause (clauses, OMP_CLAUSE_PARALLEL))
    mask = 1;
  else if (omp_find_clause (clauses, OMP_CLAUSE_FOR))
    mask = 2;
  else if (omp_find_clause (clauses, OMP_CLAUSE_SECTIONS))
    mask = 4;
  else if (omp_find_clause (clauses, OMP_CLAUSE_TASKGROUP))
    mask = 8;
  else
    {
      error ("%<#pragma omp cancellation point%> must specify one of "
	     "%<parallel%>, %<for%>, %<sections%> or %<taskgroup%> clauses");
      return;
    }
  releasing_vec vec
    = make_tree_vector_single (build_int_cst (integer_type_node, mask));
  tree stmt = finish_call_expr (fn, &vec, false, false, tf_warning_or_error);
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
      protected_set_expr_location
	(body, EXPR_LOCATION (TRANSACTION_EXPR_BODY (stmt)));
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
      protected_set_expr_location (expr, loc);
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


/* Get constant string at LOCATION. Returns true if successful,
   otherwise false.  */

bool
cexpr_str::type_check (location_t location)
{
  tsubst_flags_t complain = tf_warning_or_error;

  if (message == NULL_TREE
      || message == error_mark_node
      || check_for_bare_parameter_packs (message))
    return false;

  if (TREE_CODE (message) != STRING_CST
      && !type_dependent_expression_p (message))
    {
      message_sz
	= finish_class_member_access_expr (message,
					   get_identifier ("size"),
					   false, complain);
      if (message_sz != error_mark_node)
	message_data
	  = finish_class_member_access_expr (message,
					     get_identifier ("data"),
					     false, complain);
      if (message_sz == error_mark_node || message_data == error_mark_node)
	{
	  error_at (location, "constexpr string must be a string "
		    "literal or object with %<size%> and "
		    "%<data%> members");
	  return false;
	}
      releasing_vec size_args, data_args;
      message_sz = finish_call_expr (message_sz, &size_args, false, false,
				     complain);
      message_data = finish_call_expr (message_data, &data_args, false, false,
				       complain);
      if (message_sz == error_mark_node || message_data == error_mark_node)
	return false;
      message_sz = build_converted_constant_expr (size_type_node, message_sz,
						       complain);
      if (message_sz == error_mark_node)
	{
	  error_at (location, "constexpr string %<size()%> "
		    "must be implicitly convertible to "
		    "%<std::size_t%>");
	  return false;
	}
      message_data = build_converted_constant_expr (const_string_type_node,
							 message_data, complain);
      if (message_data == error_mark_node)
	{
	  error_at (location, "constexpr string %<data()%> "
		    "must be implicitly convertible to "
		    "%<const char*%>");
	  return false;
	}
    }
  return true;
}

/* Extract constant string at LOCATON into output string STR.
   Returns true if successful, otherwise false.  */

bool
cexpr_str::extract (location_t location, tree &str)
{
  const char *msg;
  int len;
  if (!extract (location, msg, len))
    return false;
  str = build_string (len, msg);
  return true;
}

/* Extract constant string at LOCATION into output string MSG with LEN.
   Returns true if successful, otherwise false.  */

bool
cexpr_str::extract (location_t location, const char * & msg, int &len)
{
  tsubst_flags_t complain = tf_warning_or_error;

  msg = NULL;
  if (message_sz && message_data)
    {
      tree msz = cxx_constant_value (message_sz, NULL_TREE, complain);
      if (!tree_fits_uhwi_p (msz))
	{
	  error_at (location,
		    "constexpr string %<size()%> "
		    "must be a constant expression");
	  return false;
	}
      else if ((unsigned HOST_WIDE_INT) (int) tree_to_uhwi (msz)
	       != tree_to_uhwi (msz))
	{
	  error_at (location,
		    "constexpr string message %<size()%> "
		    "%qE too large", msz);
	  return false;
	}
      len = tree_to_uhwi (msz);
      tree data = maybe_constant_value (message_data, NULL_TREE,
					mce_true);
      if (!reduced_constant_expression_p (data))
	data = NULL_TREE;
      if (len)
	{
	  if (data)
	    msg = c_getstr (data);
	  if (msg == NULL)
	    buf = XNEWVEC (char, len);
	  for (int i = 0; i < len; ++i)
	    {
	      tree t = message_data;
	      if (i)
		t = build2 (POINTER_PLUS_EXPR,
			    TREE_TYPE (message_data), message_data,
			    size_int (i));
	      t = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (t)), t);
	      tree t2 = cxx_constant_value (t, NULL_TREE, complain);
	      if (!tree_fits_shwi_p (t2))
		{
		  error_at (location,
			    "constexpr string %<data()[%d]%> "
			    "must be a constant expression", i);
		  return false;
		}
	      if (msg == NULL)
		buf[i] = tree_to_shwi (t2);
	      /* If c_getstr worked, just verify the first and
		 last characters using constant evaluation.  */
	      else if (len > 2 && i == 0)
		i = len - 2;
	    }
	  if (msg == NULL)
	    msg = buf;
	}
      else if (!data)
	{
	  /* We don't have any function to test whether some
	     expression is a core constant expression.  So, instead
	     test whether (message.data (), 0) is a constant
	     expression.  */
	  data = build2 (COMPOUND_EXPR, integer_type_node,
			 message_data, integer_zero_node);
	  tree t = cxx_constant_value (data, NULL_TREE, complain);
	  if (!integer_zerop (t))
	    {
	      error_at (location,
			"constexpr string %<data()%> "
			"must be a core constant expression");
	      return false;
	    }
	}
    }
  else
    {
      tree eltype = TREE_TYPE (TREE_TYPE (message));
      int sz = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (eltype));
      msg = TREE_STRING_POINTER (message);
      len = TREE_STRING_LENGTH (message) / sz - 1;
    }

  return true;
}

/* Build a STATIC_ASSERT for a static assertion with the condition
   CONDITION and the message text MESSAGE.  LOCATION is the location
   of the static assertion in the source code.  When MEMBER_P, this
   static assertion is a member of a class.  If SHOW_EXPR_P is true,
   print the condition (because it was instantiation-dependent).  */

void
finish_static_assert (tree condition, tree message, location_t location,
		      bool member_p, bool show_expr_p)
{
  tsubst_flags_t complain = tf_warning_or_error;

  if (condition == NULL_TREE
      || condition == error_mark_node)
    return;

  if (check_for_bare_parameter_packs (condition))
    return;

  cexpr_str cstr(message);
  if (!cstr.type_check (location))
    return;

  /* Save the condition in case it was a concept check.  */
  tree orig_condition = condition;

  if (instantiation_dependent_expression_p (condition)
      || instantiation_dependent_expression_p (message))
    {
      /* We're in a template; build a STATIC_ASSERT and put it in
         the right place. */
    defer:
      tree assertion = make_node (STATIC_ASSERT);
      STATIC_ASSERT_CONDITION (assertion) = orig_condition;
      STATIC_ASSERT_MESSAGE (assertion) = cstr.message;
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
  condition = contextual_conv_bool (condition, complain);
  condition = fold_non_dependent_expr (condition, complain,
				       /*manifestly_const_eval=*/true);

  if (TREE_CODE (condition) == INTEGER_CST && !integer_zerop (condition))
    /* Do nothing; the condition is satisfied. */
    ;
  else
    {
      iloc_sentinel ils (location);

      if (integer_zerop (condition))
	{
	  /* CWG2518: static_assert failure in a template is not IFNDR.  */
	  if (processing_template_decl)
	    goto defer;

	  int len;
	  const char *msg = NULL;
	  if (!cstr.extract (location, msg, len))
	    return;

	  /* See if we can find which clause was failing (for logical AND).  */
	  tree bad = find_failing_clause (NULL, orig_condition);
	  /* If not, or its location is unusable, fall back to the previous
	     location.  */
	  location_t cloc = cp_expr_loc_or_loc (bad, location);

	  auto_diagnostic_group d;

	  /* Report the error. */
	  if (len == 0)
	    error_at (cloc, "static assertion failed");
	  else
	    error_at (cloc, "static assertion failed: %.*s", len, msg);

	  diagnose_failing_condition (bad, cloc, show_expr_p);
	}
      else if (condition && condition != error_mark_node)
	{
	  error ("non-constant condition for static assertion");
	  if (require_rvalue_constant_expression (condition))
	    cxx_constant_value (condition);
	}
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
	error ("argument to %<decltype%> must be an expression");
      return error_mark_node;
    }

  /* decltype is an unevaluated context.  */
  cp_unevaluated u;

  processing_template_decl_sentinel ptds (/*reset=*/false);

  /* Depending on the resolution of DR 1172, we may later need to distinguish
     instantiation-dependent but not type-dependent expressions so that, say,
     A<decltype(sizeof(T))>::U doesn't require 'typename'.  */
  if (instantiation_dependent_uneval_expression_p (expr))
    {
    dependent:
      type = cxx_make_type (DECLTYPE_TYPE);
      DECLTYPE_TYPE_EXPR (type) = expr;
      DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (type)
        = id_expression_or_member_access_p;
      SET_TYPE_STRUCTURAL_EQUALITY (type);

      return type;
    }
  else if (processing_template_decl)
    {
      expr = instantiate_non_dependent_expr (expr, complain|tf_decltype);
      if (expr == error_mark_node)
	return error_mark_node;
      /* Keep processing_template_decl cleared for the rest of the function
	 (for sake of the call to lvalue_kind below, which handles templated
	 and non-templated COND_EXPR differently).  */
      processing_template_decl = 0;
    }

  /* The type denoted by decltype(e) is defined as follows:  */

  expr = resolve_nondeduced_context (expr, complain);
  if (!mark_single_function (expr, complain))
    return error_mark_node;

  if (invalid_nonstatic_memfn_p (input_location, expr, complain))
    return error_mark_node;

  if (type_unknown_p (expr))
    {
      if (complain & tf_error)
	error ("%<decltype%> cannot resolve address of overloaded function");
      return error_mark_node;
    }

  if (id_expression_or_member_access_p)
    {
      /* If e is an id-expression or a class member access (5.2.5
         [expr.ref]), decltype(e) is defined as the type of the entity
         named by e. If there is no such entity, or e names a set of
         overloaded functions, the program is ill-formed.  */
      if (identifier_p (expr))
        expr = lookup_name (expr);

      if (INDIRECT_REF_P (expr)
	  || TREE_CODE (expr) == VIEW_CONVERT_EXPR)
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

      /* decltype of a decomposition name drops references in the tuple case
	 (unlike decltype of a normal variable) and keeps cv-qualifiers from
	 the containing object in the other cases (unlike decltype of a member
	 access expression).  */
      if (DECL_DECOMPOSITION_P (expr))
	{
	  if (ptds.saved)
	    {
	      gcc_checking_assert (DECL_HAS_VALUE_EXPR_P (expr));
	      /* DECL_HAS_VALUE_EXPR_P is always set if
		 processing_template_decl.  If lookup_decomp_type
		 returns non-NULL, it is the tuple case.  */
	      if (tree ret = lookup_decomp_type (expr))
		return ret;
	    }
	  if (DECL_HAS_VALUE_EXPR_P (expr))
	    /* Expr is an array or struct subobject proxy, handle
	       bit-fields properly.  */
	    return unlowered_expr_type (expr);
	  else
	    /* Expr is a reference variable for the tuple case.  */
	    return lookup_decomp_type (expr);
	}

      switch (TREE_CODE (expr))
        {
        case FIELD_DECL:
          if (DECL_BIT_FIELD_TYPE (expr))
            {
              type = DECL_BIT_FIELD_TYPE (expr);
              break;
            }
          /* Fall through for fields that aren't bitfields.  */
	  gcc_fallthrough ();

        case VAR_DECL:
	  if (is_capture_proxy (expr))
	    {
	      if (is_normal_capture_proxy (expr))
		{
		  expr = DECL_CAPTURED_VARIABLE (expr);
		  type = TREE_TYPE (expr);
		}
	      else
		{
		  expr = DECL_VALUE_EXPR (expr);
		  gcc_assert (TREE_CODE (expr) == COMPONENT_REF);
		  expr = TREE_OPERAND (expr, 1);
		  type = TREE_TYPE (expr);
		}
	      break;
	    }
	  /* Fall through for variables that aren't capture proxies.  */
	  gcc_fallthrough ();

	case FUNCTION_DECL:
        case CONST_DECL:
        case PARM_DECL:
        case RESULT_DECL:
        case TEMPLATE_PARM_INDEX:
	  expr = mark_type_use (expr);
          type = TREE_TYPE (expr);
	  if (VAR_P (expr) && DECL_NTTP_OBJECT_P (expr))
	    {
	      /* decltype of an NTTP object is the type of the template
		 parameter, which is the object type modulo cv-quals.  */
	      int quals = cp_type_quals (type);
	      gcc_checking_assert (quals & TYPE_QUAL_CONST);
	      type = cv_unqualified (type);
	    }
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
      if (outer_automatic_var_p (STRIP_REFERENCE_REF (expr))
	  && current_function_decl
	  && LAMBDA_FUNCTION_P (current_function_decl))
	{
	  /* [expr.prim.id.unqual]/3: If naming the entity from outside of an
	     unevaluated operand within S would refer to an entity captured by
	     copy in some intervening lambda-expression, then let E be the
	     innermost such lambda-expression.

	     If there is such a lambda-expression and if P is in E's function
	     parameter scope but not its parameter-declaration-clause, then the
	     type of the expression is the type of a class member access
	     expression naming the non-static data member that would be declared
	     for such a capture in the object parameter of the function call
	     operator of E."  */
	  /* FIXME: This transformation needs to happen for all uses of an outer
	     local variable inside decltype, not just decltype((x)) (PR83167).
	     And we don't handle nested lambdas properly, where we need to
	     consider the outer lambdas as well (PR112926). */
	  tree decl = STRIP_REFERENCE_REF (expr);
	  tree lam = CLASSTYPE_LAMBDA_EXPR (DECL_CONTEXT (current_function_decl));
	  tree cap = lookup_name (DECL_NAME (decl), LOOK_where::BLOCK,
				  LOOK_want::HIDDEN_LAMBDA);

	  if (cap && is_capture_proxy (cap))
	    type = TREE_TYPE (cap);
	  else if (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lam) == CPLD_COPY)
	    {
	      type = TREE_TYPE (decl);
	      if (TYPE_REF_P (type)
		  && TREE_CODE (TREE_TYPE (type)) != FUNCTION_TYPE)
		type = TREE_TYPE (type);
	    }

	  if (type && !TYPE_REF_P (type))
	    {
	      tree obtype = TREE_TYPE (DECL_ARGUMENTS (current_function_decl));
	      if (WILDCARD_TYPE_P (non_reference (obtype)))
		/* We don't know what the eventual obtype quals will be.  */
		goto dependent;
	      auto direct_type = [](tree t){
		  if (INDIRECT_TYPE_P (t))
		    return TREE_TYPE (t);
		  return t;
	       };
	      int const quals = cp_type_quals (type)
			      | cp_type_quals (direct_type (obtype));
	      type = cp_build_qualified_type (type, quals);
	      type = build_reference_type (type);
	    }
	}
      else if (error_operand_p (expr))
	type = error_mark_node;
      else if (expr == current_class_ptr)
	/* If the expression is just "this", we want the
	   cv-unqualified pointer for the "this" type.  */
	type = TYPE_MAIN_VARIANT (TREE_TYPE (expr));

      if (!type)
	{
	  /* Otherwise, where T is the type of e, if e is an lvalue,
	     decltype(e) is defined as T&; if an xvalue, T&&; otherwise, T. */
	  cp_lvalue_kind clk = lvalue_kind (expr);
	  type = unlowered_expr_type (expr);
	  gcc_assert (!TYPE_REF_P (type));

	  /* For vector types, pick a non-opaque variant.  */
	  if (VECTOR_TYPE_P (type))
	    type = strip_typedefs (type);

	  if (clk != clk_none && !(clk & clk_class))
	    type = cp_build_reference_type (type, (clk & clk_rvalueref));
	}
    }

  return type;
}

/* Called from trait_expr_value to evaluate either __has_nothrow_assign or
   __has_nothrow_copy, depending on assign_p.  Returns true iff all
   the copy {ctor,assign} fns are nothrow.  */

static bool
classtype_has_nothrow_assign_or_copy_p (tree type, bool assign_p)
{
  tree fns = NULL_TREE;

  if (assign_p || TYPE_HAS_COPY_CTOR (type))
    fns = get_class_binding (type, assign_p ? assign_op_identifier
			     : ctor_identifier);

  bool saw_copy = false;
  for (ovl_iterator iter (fns); iter; ++iter)
    {
      tree fn = *iter;

      if (copy_fn_p (fn) > 0)
	{
	  saw_copy = true;
	  if (!maybe_instantiate_noexcept (fn)
	      || !TYPE_NOTHROW_P (TREE_TYPE (fn)))
	    return false;
	}
    }

  return saw_copy;
}

/* Return true if DERIVED is pointer interconvertible base of BASE.  */

static bool
pointer_interconvertible_base_of_p (tree base, tree derived)
{
  if (base == error_mark_node || derived == error_mark_node)
    return false;
  base = TYPE_MAIN_VARIANT (base);
  derived = TYPE_MAIN_VARIANT (derived);
  if (!NON_UNION_CLASS_TYPE_P (base)
      || !NON_UNION_CLASS_TYPE_P (derived))
    return false;

  if (same_type_p (base, derived))
    return true;

  if (!std_layout_type_p (derived))
    return false;

  return uniquely_derived_from_p (base, derived);
}

/* Helper function for fold_builtin_is_pointer_inverconvertible_with_class,
   return true if MEMBERTYPE is the type of the first non-static data member
   of TYPE or for unions of any members.  */
static bool
first_nonstatic_data_member_p (tree type, tree membertype)
{
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      if (TREE_CODE (field) != FIELD_DECL)
	continue;
      if (DECL_FIELD_IS_BASE (field) && is_empty_field (field))
	continue;
      if (DECL_FIELD_IS_BASE (field))
	return first_nonstatic_data_member_p (TREE_TYPE (field), membertype);
      if (ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	{
	  if ((TREE_CODE (TREE_TYPE (field)) == UNION_TYPE
	       || std_layout_type_p (TREE_TYPE (field)))
	      && first_nonstatic_data_member_p (TREE_TYPE (field), membertype))
	    return true;
	}
      else if (same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (field),
							  membertype))
	return true;
      if (TREE_CODE (type) != UNION_TYPE)
	return false;
    }
  return false;
}

/* Fold __builtin_is_pointer_interconvertible_with_class call.  */

tree
fold_builtin_is_pointer_inverconvertible_with_class (location_t loc, int nargs,
						     tree *args)
{
  /* Unless users call the builtin directly, the following 3 checks should be
     ensured from std::is_pointer_interconvertible_with_class function
     template.  */
  if (nargs != 1)
    {
      error_at (loc, "%<__builtin_is_pointer_interconvertible_with_class%> "
		     "needs a single argument");
      return boolean_false_node;
    }
  tree arg = args[0];
  if (error_operand_p (arg))
    return boolean_false_node;
  if (!TYPE_PTRMEM_P (TREE_TYPE (arg)))
    {
      error_at (loc, "%<__builtin_is_pointer_interconvertible_with_class%> "
		     "argument is not pointer to member");
      return boolean_false_node;
    }

  if (!TYPE_PTRDATAMEM_P (TREE_TYPE (arg)))
    return boolean_false_node;

  tree membertype = TREE_TYPE (TREE_TYPE (arg));
  tree basetype = TYPE_OFFSET_BASETYPE (TREE_TYPE (arg));
  if (!complete_type_or_else (basetype, NULL_TREE))
    return boolean_false_node;

  if (TREE_CODE (basetype) != UNION_TYPE
      && !std_layout_type_p (basetype))
    return boolean_false_node;

  if (!first_nonstatic_data_member_p (basetype, membertype))
    return boolean_false_node;

  if (TREE_CODE (arg) == PTRMEM_CST)
    arg = cplus_expand_constant (arg);

  if (integer_nonzerop (arg))
    return boolean_false_node;
  if (integer_zerop (arg))
    return boolean_true_node;

  return fold_build2 (EQ_EXPR, boolean_type_node, arg,
		      build_zero_cst (TREE_TYPE (arg)));
}

/* Helper function for is_corresponding_member_aggr.  Return true if
   MEMBERTYPE pointer-to-data-member ARG can be found in anonymous
   union or structure BASETYPE.  */

static bool
is_corresponding_member_union (tree basetype, tree membertype, tree arg)
{
  for (tree field = TYPE_FIELDS (basetype); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) != FIELD_DECL || DECL_BIT_FIELD_TYPE (field))
      continue;
    else if (same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (field),
							membertype))
      {
	if (TREE_CODE (arg) != INTEGER_CST
	    || tree_int_cst_equal (arg, byte_position (field)))
	  return true;
      }
    else if (ANON_AGGR_TYPE_P (TREE_TYPE (field)))
      {
	tree narg = arg;
	if (TREE_CODE (basetype) != UNION_TYPE
	    && TREE_CODE (narg) == INTEGER_CST)
	  narg = size_binop (MINUS_EXPR, arg, byte_position (field));
	if (is_corresponding_member_union (TREE_TYPE (field),
					   membertype, narg))
	  return true;
      }
  return false;
}

/* Helper function for fold_builtin_is_corresponding_member call.
   Return boolean_false_node if MEMBERTYPE1 BASETYPE1::*ARG1 and
   MEMBERTYPE2 BASETYPE2::*ARG2 aren't corresponding members,
   boolean_true_node if they are corresponding members, or for
   non-constant ARG2 the highest member offset for corresponding
   members.  */

static tree
is_corresponding_member_aggr (location_t loc, tree basetype1, tree membertype1,
			      tree arg1, tree basetype2, tree membertype2,
			      tree arg2)
{
  tree field1 = TYPE_FIELDS (basetype1);
  tree field2 = TYPE_FIELDS (basetype2);
  tree ret = boolean_false_node;
  while (1)
    {
      bool r = next_common_initial_sequence (field1, field2);
      if (field1 == NULL_TREE || field2 == NULL_TREE)
	break;
      if (r
	  && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (field1),
							membertype1)
	  && same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (field2),
							membertype2))
	{
	  tree pos = byte_position (field1);
	  if (TREE_CODE (arg1) == INTEGER_CST
	      && tree_int_cst_equal (arg1, pos))
	    {
	      if (TREE_CODE (arg2) == INTEGER_CST)
		return boolean_true_node;
	      return pos;
	    }
	  else if (TREE_CODE (arg1) != INTEGER_CST)
	    ret = pos;
	}
      else if (ANON_AGGR_TYPE_P (TREE_TYPE (field1))
	       && ANON_AGGR_TYPE_P (TREE_TYPE (field2)))
	{
	  if ((!lookup_attribute ("no_unique_address",
				  DECL_ATTRIBUTES (field1)))
	      != !lookup_attribute ("no_unique_address",
				    DECL_ATTRIBUTES (field2)))
	    break;
	  if (!tree_int_cst_equal (bit_position (field1),
				   bit_position (field2)))
	    break;
	  bool overlap = true;
	  tree pos = byte_position (field1);
	  if (TREE_CODE (arg1) == INTEGER_CST)
	    {
	      tree off1 = fold_convert (sizetype, arg1);
	      tree sz1 = TYPE_SIZE_UNIT (TREE_TYPE (field1));
	      if (tree_int_cst_lt (off1, pos)
		  || tree_int_cst_le (size_binop (PLUS_EXPR, pos, sz1), off1))
		overlap = false;
	    }
	  if (TREE_CODE (arg2) == INTEGER_CST)
	    {
	      tree off2 = fold_convert (sizetype, arg2);
	      tree sz2 = TYPE_SIZE_UNIT (TREE_TYPE (field2));
	      if (tree_int_cst_lt (off2, pos)
		  || tree_int_cst_le (size_binop (PLUS_EXPR, pos, sz2), off2))
		overlap = false;
	    }
	  if (overlap
	      && NON_UNION_CLASS_TYPE_P (TREE_TYPE (field1))
	      && NON_UNION_CLASS_TYPE_P (TREE_TYPE (field2)))
	    {
	      tree narg1 = arg1;
	      if (TREE_CODE (arg1) == INTEGER_CST)
		narg1 = size_binop (MINUS_EXPR,
				    fold_convert (sizetype, arg1), pos);
	      tree narg2 = arg2;
	      if (TREE_CODE (arg2) == INTEGER_CST)
		narg2 = size_binop (MINUS_EXPR,
				    fold_convert (sizetype, arg2), pos);
	      tree t1 = TREE_TYPE (field1);
	      tree t2 = TREE_TYPE (field2);
	      tree nret = is_corresponding_member_aggr (loc, t1, membertype1,
							narg1, t2, membertype2,
							narg2);
	      if (nret != boolean_false_node)
		{
		  if (nret == boolean_true_node)
		    return nret;
		  if (TREE_CODE (arg1) == INTEGER_CST)
		    return size_binop (PLUS_EXPR, nret, pos);
		  ret = size_binop (PLUS_EXPR, nret, pos);
		}
	    }
	  else if (overlap
		   && TREE_CODE (TREE_TYPE (field1)) == UNION_TYPE
		   && TREE_CODE (TREE_TYPE (field2)) == UNION_TYPE)
	    {
	      tree narg1 = arg1;
	      if (TREE_CODE (arg1) == INTEGER_CST)
		narg1 = size_binop (MINUS_EXPR,
				    fold_convert (sizetype, arg1), pos);
	      tree narg2 = arg2;
	      if (TREE_CODE (arg2) == INTEGER_CST)
		narg2 = size_binop (MINUS_EXPR,
				    fold_convert (sizetype, arg2), pos);
	      if (is_corresponding_member_union (TREE_TYPE (field1),
						 membertype1, narg1)
		  && is_corresponding_member_union (TREE_TYPE (field2),
						    membertype2, narg2))
		{
		  sorry_at (loc, "%<__builtin_is_corresponding_member%> "
				 "not well defined for anonymous unions");
		  return boolean_false_node;
		}
	    }
	}
      if (!r)
	break;
      field1 = DECL_CHAIN (field1);
      field2 = DECL_CHAIN (field2);
    }
  return ret;
}

/* Fold __builtin_is_corresponding_member call.  */

tree
fold_builtin_is_corresponding_member (location_t loc, int nargs,
				      tree *args)
{
  /* Unless users call the builtin directly, the following 3 checks should be
     ensured from std::is_corresponding_member function template.  */
  if (nargs != 2)
    {
      error_at (loc, "%<__builtin_is_corresponding_member%> "
		     "needs two arguments");
      return boolean_false_node;
    }
  tree arg1 = args[0];
  tree arg2 = args[1];
  if (error_operand_p (arg1) || error_operand_p (arg2))
    return boolean_false_node;
  if (!TYPE_PTRMEM_P (TREE_TYPE (arg1))
      || !TYPE_PTRMEM_P (TREE_TYPE (arg2)))
    {
      error_at (loc, "%<__builtin_is_corresponding_member%> "
		     "argument is not pointer to member");
      return boolean_false_node;
    }

  if (!TYPE_PTRDATAMEM_P (TREE_TYPE (arg1))
      || !TYPE_PTRDATAMEM_P (TREE_TYPE (arg2)))
    return boolean_false_node;

  tree membertype1 = TREE_TYPE (TREE_TYPE (arg1));
  tree basetype1 = TYPE_OFFSET_BASETYPE (TREE_TYPE (arg1));
  if (!complete_type_or_else (basetype1, NULL_TREE))
    return boolean_false_node;

  tree membertype2 = TREE_TYPE (TREE_TYPE (arg2));
  tree basetype2 = TYPE_OFFSET_BASETYPE (TREE_TYPE (arg2));
  if (!complete_type_or_else (basetype2, NULL_TREE))
    return boolean_false_node;

  if (!NON_UNION_CLASS_TYPE_P (basetype1)
      || !NON_UNION_CLASS_TYPE_P (basetype2)
      || !std_layout_type_p (basetype1)
      || !std_layout_type_p (basetype2))
    return boolean_false_node;

  /* If the member types aren't layout compatible, then they
     can't be corresponding members.  */
  if (!layout_compatible_type_p (membertype1, membertype2))
    return boolean_false_node;

  if (TREE_CODE (arg1) == PTRMEM_CST)
    arg1 = cplus_expand_constant (arg1);
  if (TREE_CODE (arg2) == PTRMEM_CST)
    arg2 = cplus_expand_constant (arg2);

  if (null_member_pointer_value_p (arg1)
      || null_member_pointer_value_p (arg2))
    return boolean_false_node;

  if (TREE_CODE (arg1) == INTEGER_CST
      && TREE_CODE (arg2) == INTEGER_CST
      && !tree_int_cst_equal (arg1, arg2))
    return boolean_false_node;

  if (TREE_CODE (arg2) == INTEGER_CST
      && TREE_CODE (arg1) != INTEGER_CST)
    {
      std::swap (arg1, arg2);
      std::swap (membertype1, membertype2);
      std::swap (basetype1, basetype2);
    }

  tree ret = is_corresponding_member_aggr (loc, basetype1, membertype1, arg1,
					   basetype2, membertype2, arg2);
  if (TREE_TYPE (ret) == boolean_type_node)
    return ret;
  /* If both arg1 and arg2 are INTEGER_CSTs, is_corresponding_member_aggr
     already returns boolean_{true,false}_node whether those particular
     members are corresponding members or not.  Otherwise, if only
     one of them is INTEGER_CST (canonicalized to first being INTEGER_CST
     above), it returns boolean_false_node if it is certainly not a
     corresponding member and otherwise we need to do a runtime check that
     those two OFFSET_TYPE offsets are equal.
     If neither of the operands is INTEGER_CST, is_corresponding_member_aggr
     returns the largest offset at which the members would be corresponding
     members, so perform arg1 <= ret && arg1 == arg2 runtime check.  */
  gcc_assert (TREE_CODE (arg2) != INTEGER_CST);
  if (TREE_CODE (arg1) == INTEGER_CST)
    return fold_build2 (EQ_EXPR, boolean_type_node, arg1,
			fold_convert (TREE_TYPE (arg1), arg2));
  ret = fold_build2 (LE_EXPR, boolean_type_node,
		     fold_convert (pointer_sized_int_node, arg1),
		     fold_convert (pointer_sized_int_node, ret));
  return fold_build2 (TRUTH_AND_EXPR, boolean_type_node, ret,
		      fold_build2 (EQ_EXPR, boolean_type_node, arg1,
				   fold_convert (TREE_TYPE (arg1), arg2)));
}

/* [basic.types] 8.  True iff TYPE is an object type.  */

static bool
object_type_p (const_tree type)
{
  return (TREE_CODE (type) != FUNCTION_TYPE
          && !TYPE_REF_P (type)
          && !VOID_TYPE_P (type));
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

    case CPTK_HAS_NOTHROW_CONSTRUCTOR:
      type1 = strip_array_types (type1);
      return (trait_expr_value (CPTK_HAS_TRIVIAL_CONSTRUCTOR, type1, type2)
	      || (CLASS_TYPE_P (type1)
		  && (t = locate_ctor (type1))
		  && maybe_instantiate_noexcept (t)
		  && TYPE_NOTHROW_P (TREE_TYPE (t))));

    case CPTK_HAS_NOTHROW_COPY:
      type1 = strip_array_types (type1);
      return (trait_expr_value (CPTK_HAS_TRIVIAL_COPY, type1, type2)
	      || (CLASS_TYPE_P (type1)
		  && classtype_has_nothrow_assign_or_copy_p (type1, false)));

    case CPTK_HAS_TRIVIAL_ASSIGN:
      /* ??? The standard seems to be missing the "or array of such a class
	 type" wording for this trait.  */
      type1 = strip_array_types (type1);
      return (!CP_TYPE_CONST_P (type1) && type_code1 != REFERENCE_TYPE
	      && (trivial_type_p (type1)
		    || (CLASS_TYPE_P (type1)
			&& TYPE_HAS_TRIVIAL_COPY_ASSIGN (type1))));

    case CPTK_HAS_TRIVIAL_CONSTRUCTOR:
      type1 = strip_array_types (type1);
      return (trivial_type_p (type1)
	      || (CLASS_TYPE_P (type1) && TYPE_HAS_TRIVIAL_DFLT (type1)));

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

    case CPTK_HAS_UNIQUE_OBJ_REPRESENTATIONS:
      return type_has_unique_obj_representations (type1);

    case CPTK_HAS_VIRTUAL_DESTRUCTOR:
      return type_has_virtual_destructor (type1);

    case CPTK_IS_ABSTRACT:
      return ABSTRACT_CLASS_TYPE_P (type1);

    case CPTK_IS_AGGREGATE:
      return CP_AGGREGATE_TYPE_P (type1);

    case CPTK_IS_ARRAY:
      return (type_code1 == ARRAY_TYPE
	      /* We don't want to report T[0] as being an array type.
		 This is for compatibility with an implementation of
		 std::is_array by template argument deduction, because
		 compute_array_index_type_loc rejects a zero-size array
		 in SFINAE context.  */
	      && !(TYPE_SIZE (type1) && integer_zerop (TYPE_SIZE (type1))));

    case CPTK_IS_ASSIGNABLE:
      return is_xible (MODIFY_EXPR, type1, type2);

    case CPTK_IS_BASE_OF:
      return (NON_UNION_CLASS_TYPE_P (type1) && NON_UNION_CLASS_TYPE_P (type2)
	      && (same_type_ignoring_top_level_qualifiers_p (type1, type2)
		  || DERIVED_FROM_P (type1, type2)));

    case CPTK_IS_BOUNDED_ARRAY:
      return (type_code1 == ARRAY_TYPE
	      && TYPE_DOMAIN (type1)
	      /* We don't want to report T[0] as being a bounded array type.
		 This is for compatibility with an implementation of
		 std::is_bounded_array by template argument deduction, because
		 compute_array_index_type_loc rejects a zero-size array
		 in SFINAE context.  */
	      && !(TYPE_SIZE (type1) && integer_zerop (TYPE_SIZE (type1))));

    case CPTK_IS_CLASS:
      return NON_UNION_CLASS_TYPE_P (type1);

    case CPTK_IS_CONST:
      return CP_TYPE_CONST_P (type1);

    case CPTK_IS_CONSTRUCTIBLE:
      return is_xible (INIT_EXPR, type1, type2);

    case CPTK_IS_CONVERTIBLE:
      return is_convertible (type1, type2);

    case CPTK_IS_EMPTY:
      return NON_UNION_CLASS_TYPE_P (type1) && CLASSTYPE_EMPTY_P (type1);

    case CPTK_IS_ENUM:
      return type_code1 == ENUMERAL_TYPE;

    case CPTK_IS_FINAL:
      return CLASS_TYPE_P (type1) && CLASSTYPE_FINAL (type1);

    case CPTK_IS_FUNCTION:
      return type_code1 == FUNCTION_TYPE;

    case CPTK_IS_INVOCABLE:
      return !error_operand_p (build_invoke (type1, type2, tf_none));

    case CPTK_IS_LAYOUT_COMPATIBLE:
      return layout_compatible_type_p (type1, type2);

    case CPTK_IS_LITERAL_TYPE:
      return literal_type_p (type1);

    case CPTK_IS_MEMBER_FUNCTION_POINTER:
      return TYPE_PTRMEMFUNC_P (type1);

    case CPTK_IS_MEMBER_OBJECT_POINTER:
      return TYPE_PTRDATAMEM_P (type1);

    case CPTK_IS_MEMBER_POINTER:
      return TYPE_PTRMEM_P (type1);

    case CPTK_IS_NOTHROW_ASSIGNABLE:
      return is_nothrow_xible (MODIFY_EXPR, type1, type2);

    case CPTK_IS_NOTHROW_CONSTRUCTIBLE:
      return is_nothrow_xible (INIT_EXPR, type1, type2);

    case CPTK_IS_NOTHROW_CONVERTIBLE:
      return is_nothrow_convertible (type1, type2);

    case CPTK_IS_NOTHROW_INVOCABLE:
      return expr_noexcept_p (build_invoke (type1, type2, tf_none), tf_none);

    case CPTK_IS_OBJECT:
      return object_type_p (type1);

    case CPTK_IS_POINTER_INTERCONVERTIBLE_BASE_OF:
      return pointer_interconvertible_base_of_p (type1, type2);

    case CPTK_IS_POD:
      return pod_type_p (type1);

    case CPTK_IS_POINTER:
      return TYPE_PTR_P (type1);

    case CPTK_IS_POLYMORPHIC:
      return CLASS_TYPE_P (type1) && TYPE_POLYMORPHIC_P (type1);

    case CPTK_IS_REFERENCE:
      return type_code1 == REFERENCE_TYPE;

    case CPTK_IS_SAME:
      return same_type_p (type1, type2);

    case CPTK_IS_SCOPED_ENUM:
      return SCOPED_ENUM_P (type1);

    case CPTK_IS_STD_LAYOUT:
      return std_layout_type_p (type1);

    case CPTK_IS_TRIVIAL:
      return trivial_type_p (type1);

    case CPTK_IS_TRIVIALLY_ASSIGNABLE:
      return is_trivially_xible (MODIFY_EXPR, type1, type2);

    case CPTK_IS_TRIVIALLY_CONSTRUCTIBLE:
      return is_trivially_xible (INIT_EXPR, type1, type2);

    case CPTK_IS_TRIVIALLY_COPYABLE:
      return trivially_copyable_p (type1);

    case CPTK_IS_UNBOUNDED_ARRAY:
      return array_of_unknown_bound_p (type1);

    case CPTK_IS_UNION:
      return type_code1 == UNION_TYPE;

    case CPTK_IS_VIRTUAL_BASE_OF:
      return (NON_UNION_CLASS_TYPE_P (type1) && NON_UNION_CLASS_TYPE_P (type2)
	      && lookup_base (type2, type1, ba_require_virtual,
			      NULL, tf_none) != NULL_TREE);

    case CPTK_IS_VOLATILE:
      return CP_TYPE_VOLATILE_P (type1);

    case CPTK_REF_CONSTRUCTS_FROM_TEMPORARY:
      return ref_xes_from_temporary (type1, type2, /*direct_init=*/true);

    case CPTK_REF_CONVERTS_FROM_TEMPORARY:
      return ref_xes_from_temporary (type1, type2, /*direct_init=*/false);

    case CPTK_IS_DEDUCIBLE:
      return type_targs_deducible_from (type1, type2);

    /* __array_rank is handled in finish_trait_expr. */
    case CPTK_RANK:
      gcc_unreachable ();

#define DEFTRAIT_TYPE(CODE, NAME, ARITY) \
    case CPTK_##CODE:
#include "cp-trait.def"
#undef DEFTRAIT_TYPE
      /* Type-yielding traits are handled in finish_trait_type.  */
      break;
    }

  gcc_unreachable ();
}

/* Returns true if TYPE meets the requirements for the specified KIND,
   false otherwise.

   When KIND == 1, TYPE must be an array of unknown bound,
   or (possibly cv-qualified) void, or a complete type.

   When KIND == 2, TYPE must be a complete type, or array of complete type,
   or (possibly cv-qualified) void.

   When KIND == 3:
   If TYPE is a non-union class type, it must be complete.

   When KIND == 4:
   If TYPE is a class type, it must be complete.  */

static bool
check_trait_type (tree type, int kind = 1)
{
  if (type == NULL_TREE)
    return true;

  if (TREE_CODE (type) == TREE_VEC)
    {
      for (tree arg : tree_vec_range (type))
	if (!check_trait_type (arg, kind))
	  return false;
      return true;
    }

  if (kind == 1 && TREE_CODE (type) == ARRAY_TYPE && !TYPE_DOMAIN (type))
    return true; // Array of unknown bound. Don't care about completeness.

  if (kind == 3 && !NON_UNION_CLASS_TYPE_P (type))
    return true; // Not a non-union class type. Don't care about completeness.

  if (kind == 4 && TREE_CODE (type) == ARRAY_TYPE)
    return true; // Not a class type. Don't care about completeness.

  if (VOID_TYPE_P (type))
    return true;

  type = complete_type (strip_array_types (type));
  if (!COMPLETE_TYPE_P (type)
      && cxx_incomplete_type_diagnostic (NULL_TREE, type, DK_PERMERROR)
      && !flag_permissive)
    return false;
  return true;
}

/* True iff the conversion (if any) would be a direct reference
   binding, not requiring complete types.  This is LWG2939.  */

static bool
same_type_ref_bind_p (cp_trait_kind kind, tree type1, tree type2)
{
  tree from, to;
  switch (kind)
    {
      /* These put the target type first.  */
    case CPTK_IS_CONSTRUCTIBLE:
    case CPTK_IS_NOTHROW_CONSTRUCTIBLE:
    case CPTK_IS_TRIVIALLY_CONSTRUCTIBLE:
    case CPTK_IS_INVOCABLE:
    case CPTK_IS_NOTHROW_INVOCABLE:
    case CPTK_REF_CONSTRUCTS_FROM_TEMPORARY:
    case CPTK_REF_CONVERTS_FROM_TEMPORARY:
      to = type1;
      from = type2;
      break;

      /* These put it second.  */
    case CPTK_IS_CONVERTIBLE:
    case CPTK_IS_NOTHROW_CONVERTIBLE:
      to = type2;
      from = type1;
      break;

    default:
      gcc_unreachable ();
    }

  if (TREE_CODE (to) != REFERENCE_TYPE || !from)
    return false;
  if (TREE_CODE (from) == TREE_VEC && TREE_VEC_LENGTH (from) == 1)
    from = TREE_VEC_ELT (from, 0);
  return (TYPE_P (from)
	  && (same_type_ignoring_top_level_qualifiers_p
	      (non_reference (to), non_reference (from))));
}

/* [defns.referenceable] True iff TYPE is a referenceable type.  */

static bool
referenceable_type_p (const_tree type)
{
  return (TYPE_REF_P (type)
	  || object_type_p (type)
	  || (FUNC_OR_METHOD_TYPE_P (type)
	      && (type_memfn_quals (type) == TYPE_UNQUALIFIED
		  && type_memfn_rqual (type) == REF_QUAL_NONE)));
}

/* Process a trait expression.  */

tree
finish_trait_expr (location_t loc, cp_trait_kind kind, tree type1, tree type2)
{
  if (type1 == error_mark_node
      || type2 == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      tree trait_expr = make_node (TRAIT_EXPR);
      if (kind == CPTK_RANK)
	TREE_TYPE (trait_expr) = size_type_node;
      else
	TREE_TYPE (trait_expr) = boolean_type_node;
      TRAIT_EXPR_TYPE1 (trait_expr) = type1;
      TRAIT_EXPR_TYPE2 (trait_expr) = type2;
      TRAIT_EXPR_KIND (trait_expr) = kind;
      TRAIT_EXPR_LOCATION (trait_expr) = loc;
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
      if (!check_trait_type (type1))
	return error_mark_node;
      break;

    case CPTK_IS_LITERAL_TYPE:
    case CPTK_IS_POD:
    case CPTK_IS_STD_LAYOUT:
    case CPTK_IS_TRIVIAL:
    case CPTK_IS_TRIVIALLY_COPYABLE:
    case CPTK_HAS_UNIQUE_OBJ_REPRESENTATIONS:
      if (!check_trait_type (type1, /* kind = */ 2))
	return error_mark_node;
      break;

    case CPTK_IS_ABSTRACT:
    case CPTK_IS_EMPTY:
    case CPTK_IS_POLYMORPHIC:
    case CPTK_HAS_VIRTUAL_DESTRUCTOR:
      if (!check_trait_type (type1, /* kind = */ 3))
	return error_mark_node;
      break;

    /* N.B. std::is_aggregate is kind=2 but we don't need a complete element
       type to know whether an array is an aggregate, so use kind=4 here.  */
    case CPTK_IS_AGGREGATE:
    case CPTK_IS_FINAL:
      if (!check_trait_type (type1, /* kind = */ 4))
	return error_mark_node;
      break;

    case CPTK_IS_CONSTRUCTIBLE:
    case CPTK_IS_CONVERTIBLE:
    case CPTK_IS_INVOCABLE:
    case CPTK_IS_NOTHROW_CONSTRUCTIBLE:
    case CPTK_IS_NOTHROW_CONVERTIBLE:
    case CPTK_IS_NOTHROW_INVOCABLE:
    case CPTK_IS_TRIVIALLY_CONSTRUCTIBLE:
    case CPTK_REF_CONSTRUCTS_FROM_TEMPORARY:
    case CPTK_REF_CONVERTS_FROM_TEMPORARY:
      /* Don't check completeness for direct reference binding.  */;
      if (same_type_ref_bind_p (kind, type1, type2))
	break;
      gcc_fallthrough ();

    case CPTK_IS_ASSIGNABLE:
    case CPTK_IS_NOTHROW_ASSIGNABLE:
    case CPTK_IS_TRIVIALLY_ASSIGNABLE:
      if (!check_trait_type (type1)
	  || !check_trait_type (type2))
	return error_mark_node;
      break;

    case CPTK_IS_BASE_OF:
    case CPTK_IS_POINTER_INTERCONVERTIBLE_BASE_OF:
      if (NON_UNION_CLASS_TYPE_P (type1) && NON_UNION_CLASS_TYPE_P (type2)
	  && !same_type_ignoring_top_level_qualifiers_p (type1, type2)
	  && !complete_type_or_else (type2, NULL_TREE))
	/* We already issued an error.  */
	return error_mark_node;
      break;

    case CPTK_IS_VIRTUAL_BASE_OF:
      if (NON_UNION_CLASS_TYPE_P (type1) && NON_UNION_CLASS_TYPE_P (type2)
	  && !complete_type_or_else (type2, NULL_TREE))
	/* We already issued an error.  */
	return error_mark_node;
      break;

    case CPTK_IS_ARRAY:
    case CPTK_IS_BOUNDED_ARRAY:
    case CPTK_IS_CLASS:
    case CPTK_IS_CONST:
    case CPTK_IS_ENUM:
    case CPTK_IS_FUNCTION:
    case CPTK_IS_MEMBER_FUNCTION_POINTER:
    case CPTK_IS_MEMBER_OBJECT_POINTER:
    case CPTK_IS_MEMBER_POINTER:
    case CPTK_IS_OBJECT:
    case CPTK_IS_POINTER:
    case CPTK_IS_REFERENCE:
    case CPTK_IS_SAME:
    case CPTK_IS_SCOPED_ENUM:
    case CPTK_IS_UNBOUNDED_ARRAY:
    case CPTK_IS_UNION:
    case CPTK_IS_VOLATILE:
    case CPTK_RANK:
      break;

    case CPTK_IS_LAYOUT_COMPATIBLE:
      if (!array_of_unknown_bound_p (type1)
	  && TREE_CODE (type1) != VOID_TYPE
	  && !complete_type_or_else (type1, NULL_TREE))
	/* We already issued an error.  */
	return error_mark_node;
      if (!array_of_unknown_bound_p (type2)
	  && TREE_CODE (type2) != VOID_TYPE
	  && !complete_type_or_else (type2, NULL_TREE))
	/* We already issued an error.  */
	return error_mark_node;
      break;

    case CPTK_IS_DEDUCIBLE:
      if (!DECL_TYPE_TEMPLATE_P (type1))
	{
	  error ("%qD is not a class or alias template", type1);
	  return error_mark_node;
	}
      break;

#define DEFTRAIT_TYPE(CODE, NAME, ARITY) \
    case CPTK_##CODE:
#include "cp-trait.def"
#undef DEFTRAIT_TYPE
      /* Type-yielding traits are handled in finish_trait_type.  */
      gcc_unreachable ();
    }

  tree val;
  if (kind == CPTK_RANK)
    {
      size_t rank = 0;
      for (; TREE_CODE (type1) == ARRAY_TYPE; type1 = TREE_TYPE (type1))
	++rank;
      val = build_int_cst (size_type_node, rank);
    }
  else
    val = (trait_expr_value (kind, type1, type2)
	   ? boolean_true_node : boolean_false_node);

  return maybe_wrap_with_location (val, loc);
}

/* Process a trait type.  */

tree
finish_trait_type (cp_trait_kind kind, tree type1, tree type2,
		   tsubst_flags_t complain)
{
  if (type1 == error_mark_node
      || type2 == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      tree type = cxx_make_type (TRAIT_TYPE);
      TRAIT_TYPE_TYPE1 (type) = type1;
      TRAIT_TYPE_TYPE2 (type) = type2;
      TRAIT_TYPE_KIND_RAW (type) = build_int_cstu (integer_type_node, kind);
      /* These traits are intended to be used in the definition of the ::type
	 member of the corresponding standard library type trait and aren't
	 mangleable (and thus won't appear directly in template signatures),
	 so structural equality should suffice.  */
      SET_TYPE_STRUCTURAL_EQUALITY (type);
      return type;
    }

  switch (kind)
    {
    case CPTK_ADD_LVALUE_REFERENCE:
      /* [meta.trans.ref].  */
      if (referenceable_type_p (type1))
	return cp_build_reference_type (type1, /*rval=*/false);
      return type1;

    case CPTK_ADD_POINTER:
      /* [meta.trans.ptr].  */
      if (VOID_TYPE_P (type1) || referenceable_type_p (type1))
	{
	  if (TYPE_REF_P (type1))
	    type1 = TREE_TYPE (type1);
	  return build_pointer_type (type1);
	}
      return type1;

    case CPTK_ADD_RVALUE_REFERENCE:
      /* [meta.trans.ref].  */
      if (referenceable_type_p (type1))
	return cp_build_reference_type (type1, /*rval=*/true);
      return type1;

    case CPTK_DECAY:
      if (TYPE_REF_P (type1))
	type1 = TREE_TYPE (type1);

      if (TREE_CODE (type1) == ARRAY_TYPE)
	return finish_trait_type (CPTK_ADD_POINTER, TREE_TYPE (type1), type2,
				  complain);
      else if (TREE_CODE (type1) == FUNCTION_TYPE)
	return finish_trait_type (CPTK_ADD_POINTER, type1, type2, complain);
      else
	return cv_unqualified (type1);

    case CPTK_REMOVE_ALL_EXTENTS:
      return strip_array_types (type1);

    case CPTK_REMOVE_CV:
      return cv_unqualified (type1);

    case CPTK_REMOVE_CVREF:
      if (TYPE_REF_P (type1))
	type1 = TREE_TYPE (type1);
      return cv_unqualified (type1);

    case CPTK_REMOVE_EXTENT:
      if (TREE_CODE (type1) == ARRAY_TYPE)
	type1 = TREE_TYPE (type1);
      return type1;

    case CPTK_REMOVE_POINTER:
      if (TYPE_PTR_P (type1))
	type1 = TREE_TYPE (type1);
      return type1;

    case CPTK_REMOVE_REFERENCE:
      if (TYPE_REF_P (type1))
	type1 = TREE_TYPE (type1);
      return type1;

    case CPTK_TYPE_PACK_ELEMENT:
      return finish_type_pack_element (type1, type2, complain);

    case CPTK_UNDERLYING_TYPE:
      return finish_underlying_type (type1);

#define DEFTRAIT_EXPR(CODE, NAME, ARITY) \
    case CPTK_##CODE:
#include "cp-trait.def"
#undef DEFTRAIT_EXPR
      /* Expression-yielding traits are handled in finish_trait_expr.  */
    case CPTK_BASES:
    case CPTK_DIRECT_BASES:
      /* BASES and DIRECT_BASES are handled in finish_bases.  */
      break;
    }

  gcc_unreachable ();
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


/* Return true if T designates the implied `this' parameter.  */

bool
is_this_parameter (tree t)
{
  if (!DECL_P (t) || DECL_NAME (t) != this_identifier)
    return false;
  gcc_assert (TREE_CODE (t) == PARM_DECL
	      || (VAR_P (t) && DECL_HAS_VALUE_EXPR_P (t))
	      || (cp_binding_oracle && VAR_P (t)));
  return true;
}

/* As above, or a C++23 explicit object parameter.  */

bool
is_object_parameter (tree t)
{
  if (is_this_parameter (t))
    return true;
  if (TREE_CODE (t) != PARM_DECL)
    return false;
  tree ctx = DECL_CONTEXT (t);
  return (ctx && DECL_XOBJ_MEMBER_FUNCTION_P (ctx)
	  && t == DECL_ARGUMENTS (ctx));
}

/* Insert the deduced return type for an auto function.  */

void
apply_deduced_return_type (tree fco, tree return_type)
{
  tree result;

  if (return_type == error_mark_node)
    return;

  if (DECL_CONV_FN_P (fco))
    DECL_NAME (fco) = make_conv_op_name (return_type);

  TREE_TYPE (fco) = change_return_type (return_type, TREE_TYPE (fco));

  maybe_update_postconditions (fco);

  /* Apply the type to the result object.  */

  result = DECL_RESULT (fco);
  if (result == NULL_TREE)
    return;
  if (TREE_TYPE (result) == return_type)
    return;

  if (!processing_template_decl && !VOID_TYPE_P (return_type)
      && !complete_type_or_else (return_type, NULL_TREE))
    return;

  /* We already have a DECL_RESULT from start_preparsed_function.
     Now we need to redo the work it and allocate_struct_function
     did to reflect the new type.  */
  result = build_decl (DECL_SOURCE_LOCATION (result), RESULT_DECL, NULL_TREE,
		       TYPE_MAIN_VARIANT (return_type));
  DECL_ARTIFICIAL (result) = 1;
  DECL_IGNORED_P (result) = 1;
  cp_apply_type_quals_to_decl (cp_type_quals (return_type),
                               result);
  DECL_RESULT (fco) = result;

  if (!processing_template_decl)
    if (function *fun = DECL_STRUCT_FUNCTION (fco))
      {
	bool aggr = aggregate_value_p (result, fco);
#ifdef PCC_STATIC_STRUCT_RETURN
	fun->returns_pcc_struct = aggr;
#endif
	fun->returns_struct = aggr;
      }
}

/* Build a unary fold expression of EXPR over OP. If IS_RIGHT is true,
   this is a right unary fold. Otherwise it is a left unary fold. */

static tree
finish_unary_fold_expr (location_t loc, tree expr, int op, tree_code dir)
{
  /* Build a pack expansion (assuming expr has pack type).  */
  if (!uses_parameter_packs (expr))
    {
      error_at (location_of (expr), "operand of fold expression has no "
		"unexpanded parameter packs");
      return error_mark_node;
    }
  tree pack = make_pack_expansion (expr);

  /* Build the fold expression.  */
  tree code = build_int_cstu (integer_type_node, abs (op));
  tree fold = build_min_nt_loc (loc, dir, code, pack);
  FOLD_EXPR_MODIFY_P (fold) = (op < 0);
  TREE_TYPE (fold) = build_dependent_operator_type (NULL_TREE,
						    FOLD_EXPR_OP (fold),
						    FOLD_EXPR_MODIFY_P (fold));
  return fold;
}

tree
finish_left_unary_fold_expr (location_t loc, tree expr, int op)
{
  return finish_unary_fold_expr (loc, expr, op, UNARY_LEFT_FOLD_EXPR);
}

tree
finish_right_unary_fold_expr (location_t loc, tree expr, int op)
{
  return finish_unary_fold_expr (loc, expr, op, UNARY_RIGHT_FOLD_EXPR);
}

/* Build a binary fold expression over EXPR1 and EXPR2. The
   associativity of the fold is determined by EXPR1 and EXPR2 (whichever
   has an unexpanded parameter pack). */

static tree
finish_binary_fold_expr (location_t loc, tree pack, tree init,
			 int op, tree_code dir)
{
  pack = make_pack_expansion (pack);
  tree code = build_int_cstu (integer_type_node, abs (op));
  tree fold = build_min_nt_loc (loc, dir, code, pack, init);
  FOLD_EXPR_MODIFY_P (fold) = (op < 0);
  TREE_TYPE (fold) = build_dependent_operator_type (NULL_TREE,
						    FOLD_EXPR_OP (fold),
						    FOLD_EXPR_MODIFY_P (fold));
  return fold;
}

tree
finish_binary_fold_expr (location_t loc, tree expr1, tree expr2, int op)
{
  // Determine which expr has an unexpanded parameter pack and
  // set the pack and initial term.
  bool pack1 = uses_parameter_packs (expr1);
  bool pack2 = uses_parameter_packs (expr2);
  if (pack1 && !pack2)
    return finish_binary_fold_expr (loc, expr1, expr2, op, BINARY_RIGHT_FOLD_EXPR);
  else if (pack2 && !pack1)
    return finish_binary_fold_expr (loc, expr2, expr1, op, BINARY_LEFT_FOLD_EXPR);
  else
    {
      if (pack1)
        error ("both arguments in binary fold have unexpanded parameter packs");
      else
        error ("no unexpanded parameter packs in binary fold");
    }
  return error_mark_node;
}

/* Finish __builtin_launder (arg).  */

tree
finish_builtin_launder (location_t loc, tree arg, tsubst_flags_t complain)
{
  tree orig_arg = arg;
  if (!type_dependent_expression_p (arg))
    arg = decay_conversion (arg, complain);
  if (error_operand_p (arg))
    return error_mark_node;
  if (!type_dependent_expression_p (arg) && !TYPE_PTROB_P (TREE_TYPE (arg)))
    {
      error_at (loc, "type %qT of argument to %<__builtin_launder%> "
		"is not a pointer to object type", TREE_TYPE (arg));
      return error_mark_node;
    }
  if (processing_template_decl)
    arg = orig_arg;
  return build_call_expr_internal_loc (loc, IFN_LAUNDER,
				       TREE_TYPE (arg), 1, arg);
}

/* Finish __builtin_convertvector (arg, type).  */

tree
cp_build_vec_convert (tree arg, location_t loc, tree type,
		      tsubst_flags_t complain)
{
  if (error_operand_p (type))
    return error_mark_node;
  if (error_operand_p (arg))
    return error_mark_node;

  tree ret = NULL_TREE;
  if (!type_dependent_expression_p (arg) && !dependent_type_p (type))
    ret = c_build_vec_convert (cp_expr_loc_or_input_loc (arg),
			       decay_conversion (arg, complain),
			       loc, type, (complain & tf_error) != 0);

  if (!processing_template_decl)
    return ret;

  return build_call_expr_internal_loc (loc, IFN_VEC_CONVERT, type, 1, arg);
}

/* Finish __builtin_bit_cast (type, arg).  */

tree
cp_build_bit_cast (location_t loc, tree type, tree arg,
		   tsubst_flags_t complain)
{
  if (error_operand_p (type))
    return error_mark_node;
  if (!dependent_type_p (type))
    {
      if (!complete_type_or_maybe_complain (type, NULL_TREE, complain))
	return error_mark_node;
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  /* std::bit_cast for destination ARRAY_TYPE is not possible,
	     as functions may not return an array, so don't bother trying
	     to support this (and then deal with VLAs etc.).  */
	  error_at (loc, "%<__builtin_bit_cast%> destination type %qT "
			 "is an array type", type);
	  return error_mark_node;
	}
      if (!trivially_copyable_p (type))
	{
	  error_at (loc, "%<__builtin_bit_cast%> destination type %qT "
			 "is not trivially copyable", type);
	  return error_mark_node;
	}
    }

  if (error_operand_p (arg))
    return error_mark_node;

  if (!type_dependent_expression_p (arg))
    {
      if (TREE_CODE (TREE_TYPE (arg)) == ARRAY_TYPE)
	{
	  /* Don't perform array-to-pointer conversion.  */
	  arg = mark_rvalue_use (arg, loc, true);
	  if (!complete_type_or_maybe_complain (TREE_TYPE (arg), arg, complain))
	    return error_mark_node;
	}
      else
	arg = decay_conversion (arg, complain);

      if (error_operand_p (arg))
	return error_mark_node;

      if (!trivially_copyable_p (TREE_TYPE (arg)))
	{
	  error_at (cp_expr_loc_or_loc (arg, loc),
		    "%<__builtin_bit_cast%> source type %qT "
		    "is not trivially copyable", TREE_TYPE (arg));
	  return error_mark_node;
	}
      if (!dependent_type_p (type)
	  && !cp_tree_equal (TYPE_SIZE_UNIT (type),
			     TYPE_SIZE_UNIT (TREE_TYPE (arg))))
	{
	  error_at (loc, "%<__builtin_bit_cast%> source size %qE "
			 "not equal to destination type size %qE",
			 TYPE_SIZE_UNIT (TREE_TYPE (arg)),
			 TYPE_SIZE_UNIT (type));
	  return error_mark_node;
	}
    }

  tree ret = build_min (BIT_CAST_EXPR, type, arg);
  SET_EXPR_LOCATION (ret, loc);

  if (!processing_template_decl && CLASS_TYPE_P (type))
    ret = get_target_expr (ret, complain);

  return ret;
}

/* Diagnose invalid #pragma GCC unroll argument and adjust
   it if needed.  */

tree
cp_check_pragma_unroll (location_t loc, tree unroll)
{
  HOST_WIDE_INT lunroll = 0;
  if (type_dependent_expression_p (unroll))
    ;
  else if (!INTEGRAL_TYPE_P (TREE_TYPE (unroll))
	   || (!value_dependent_expression_p (unroll)
	       && (!tree_fits_shwi_p (unroll)
		   || (lunroll = tree_to_shwi (unroll)) < 0
		   || lunroll >= USHRT_MAX)))
    {
      error_at (loc, "%<#pragma GCC unroll%> requires an"
		" assignment-expression that evaluates to a non-negative"
		" integral constant less than %u", USHRT_MAX);
      unroll = integer_one_node;
    }
  else if (TREE_CODE (unroll) == INTEGER_CST)
    {
      unroll = fold_convert (integer_type_node, unroll);
      if (integer_zerop (unroll))
	unroll = integer_one_node;
    }
  return unroll;
}

#include "gt-cp-semantics.h"
