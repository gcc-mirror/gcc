/* Perform the semantic phase of parsing, i.e., the process of
   building tree structure, checking semantic consistency, and
   building RTL.  These routines are used both during actual parsing
   and during the instantiation of template functions. 

   Copyright (C) 1998, 1999, 2000, 2001, 2002,
   2003, 2004 Free Software Foundation, Inc.
   Written by Mark Mitchell (mmitchell@usa.net) based on code found
   formerly in parse.y and pt.c.  

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
   
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
#include "tree-inline.h"
#include "except.h"
#include "lex.h"
#include "toplev.h"
#include "flags.h"
#include "rtl.h"
#include "expr.h"
#include "output.h"
#include "timevar.h"
#include "debug.h"
#include "cgraph.h"

/* There routines provide a modular interface to perform many parsing
   operations.  They may therefore be used during actual parsing, or
   during template instantiation, which may be regarded as a
   degenerate form of parsing.  Since the current g++ parser is
   lacking in several respects, and will be reimplemented, we are
   attempting to move most code that is not directly related to
   parsing into this file; that will make implementing the new parser
   much easier since it will be able to make use of these routines.  */

static tree maybe_convert_cond (tree);
static tree simplify_aggr_init_exprs_r (tree *, int *, void *);
static void emit_associated_thunks (tree);
static void genrtl_try_block (tree);
static void genrtl_eh_spec_block (tree);
static void genrtl_handler (tree);
static void cp_expand_stmt (tree);


/* Finish processing the COND, the SUBSTMT condition for STMT.  */

#define FINISH_COND(COND, STMT, SUBSTMT) 		\
  do {							\
    if (last_tree != (STMT))				\
      {							\
        RECHAIN_STMTS (STMT, SUBSTMT);			\
        if (!processing_template_decl)			\
          {						\
	    (COND) = build_tree_list (SUBSTMT, COND);	\
	    (SUBSTMT) = (COND);				\
          }						\
      }							\
    else						\
      (SUBSTMT) = (COND);				\
  } while (0)

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
      maintains a TREE_LIST of all deferred checks.

   3. The global `current_class_type' or `current_function_decl' is then
      setup by the parser.  `enforce_access' relies on these information
      to check access.

   4. Upon exiting the context mentioned in step 1,
      `perform_deferred_access_checks' is called to check all declaration
      stored in the TREE_LIST.   `pop_deferring_access_checks' is then
      called to restore the previous access checking mode.

      In case of parsing error, we simply call `pop_deferring_access_checks'
      without `perform_deferred_access_checks'.  */

/* Data for deferred access checking.  */
static GTY(()) deferred_access *deferred_access_stack;
static GTY(()) deferred_access *deferred_access_free_list;

/* Save the current deferred access states and start deferred
   access checking iff DEFER_P is true.  */

void
push_deferring_access_checks (deferring_kind deferring)
{
  deferred_access *d;

  /* For context like template instantiation, access checking
     disabling applies to all nested context.  */
  if (deferred_access_stack
      && deferred_access_stack->deferring_access_checks_kind == dk_no_check)
    deferring = dk_no_check;

  /* Recycle previously used free store if available.  */
  if (deferred_access_free_list)
    {
      d = deferred_access_free_list;
      deferred_access_free_list = d->next;
    }
  else
    d = ggc_alloc (sizeof (deferred_access));

  d->next = deferred_access_stack;
  d->deferred_access_checks = NULL_TREE;
  d->deferring_access_checks_kind = deferring;
  deferred_access_stack = d;
}

/* Resume deferring access checks again after we stopped doing
   this previously.  */

void
resume_deferring_access_checks (void)
{
  if (deferred_access_stack->deferring_access_checks_kind == dk_no_deferred)
    deferred_access_stack->deferring_access_checks_kind = dk_deferred;
}

/* Stop deferring access checks.  */

void
stop_deferring_access_checks (void)
{
  if (deferred_access_stack->deferring_access_checks_kind == dk_deferred)
    deferred_access_stack->deferring_access_checks_kind = dk_no_deferred;
}

/* Discard the current deferred access checks and restore the
   previous states.  */

void
pop_deferring_access_checks (void)
{
  deferred_access *d = deferred_access_stack;
  deferred_access_stack = d->next;

  /* Remove references to access checks TREE_LIST.  */
  d->deferred_access_checks = NULL_TREE;

  /* Store in free list for later use.  */
  d->next = deferred_access_free_list;
  deferred_access_free_list = d;
}

/* Returns a TREE_LIST representing the deferred checks.  
   The TREE_PURPOSE of each node is the type through which the 
   access occurred; the TREE_VALUE is the declaration named.
   */

tree
get_deferred_access_checks (void)
{
  return deferred_access_stack->deferred_access_checks;
}

/* Take current deferred checks and combine with the
   previous states if we also defer checks previously.
   Otherwise perform checks now.  */

void
pop_to_parent_deferring_access_checks (void)
{
  tree deferred_check = get_deferred_access_checks ();
  deferred_access *d1 = deferred_access_stack;
  deferred_access *d2 = deferred_access_stack->next;
  deferred_access *d3 = deferred_access_stack->next->next;

  /* Temporary swap the order of the top two states, just to make
     sure the garbage collector will not reclaim the memory during 
     processing below.  */
  deferred_access_stack = d2;
  d2->next = d1;
  d1->next = d3;

  for ( ; deferred_check; deferred_check = TREE_CHAIN (deferred_check))
    /* Perform deferred check if required.  */
    perform_or_defer_access_check (TREE_PURPOSE (deferred_check), 
				   TREE_VALUE (deferred_check));

  deferred_access_stack = d1;
  d1->next = d2;
  d2->next = d3;
  pop_deferring_access_checks ();
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
   next with `x'.  */

void
perform_deferred_access_checks (void)
{
  tree deferred_check;
  for (deferred_check = deferred_access_stack->deferred_access_checks;
       deferred_check;
       deferred_check = TREE_CHAIN (deferred_check))
    /* Check access.  */
    enforce_access (TREE_PURPOSE (deferred_check), 
		    TREE_VALUE (deferred_check));
}

/* Defer checking the accessibility of DECL, when looked up in
   BINFO.  */

void
perform_or_defer_access_check (tree binfo, tree decl)
{
  tree check;

  my_friendly_assert (TREE_CODE (binfo) == TREE_VEC, 20030623);
  
  /* If we are not supposed to defer access checks, just check now.  */
  if (deferred_access_stack->deferring_access_checks_kind == dk_no_deferred)
    {
      enforce_access (binfo, decl);
      return;
    }
  /* Exit if we are in a context that no access checking is performed.  */
  else if (deferred_access_stack->deferring_access_checks_kind == dk_no_check)
    return;

  /* See if we are already going to perform this check.  */
  for (check = deferred_access_stack->deferred_access_checks;
       check;
       check = TREE_CHAIN (check))
    if (TREE_VALUE (check) == decl && TREE_PURPOSE (check) == binfo)
      return;
  /* If not, record the check.  */
  deferred_access_stack->deferred_access_checks
    = tree_cons (binfo, decl,
		 deferred_access_stack->deferred_access_checks);
}

/* Returns nonzero if the current statement is a full expression,
   i.e. temporaries created during that statement should be destroyed
   at the end of the statement.  */

int
stmts_are_full_exprs_p (void)
{
  return current_stmt_tree ()->stmts_are_full_exprs_p;
}

/* Returns the stmt_tree (if any) to which statements are currently
   being added.  If there is no active statement-tree, NULL is
   returned.  */

stmt_tree
current_stmt_tree (void)
{
  return (cfun 
	  ? &cfun->language->base.x_stmt_tree 
	  : &scope_chain->x_stmt_tree);
}

/* Nonzero if TYPE is an anonymous union or struct type.  We have to use a
   flag for this because "A union for which objects or pointers are
   declared is not an anonymous union" [class.union].  */

int
anon_aggr_type_p (tree node)
{
  return ANON_AGGR_TYPE_P (node);
}

/* Finish a scope.  */

tree
do_poplevel (void)
{
  tree block = NULL_TREE;

  if (stmts_are_full_exprs_p ())
    {
      tree scope_stmts = NULL_TREE;

      block = poplevel (kept_level_p (), 1, 0);
      if (!processing_template_decl)
	{
	  /* This needs to come after the poplevel so that partial scopes
	     are properly nested.  */
	  scope_stmts = add_scope_stmt (/*begin_p=*/0, /*partial_p=*/0);
	  if (block)
	    {
	      SCOPE_STMT_BLOCK (TREE_PURPOSE (scope_stmts)) = block;
	      SCOPE_STMT_BLOCK (TREE_VALUE (scope_stmts)) = block;
	    }
	}
    }

  return block;
}

/* Begin a new scope.  */ 

void
do_pushlevel (scope_kind sk)
{
  if (stmts_are_full_exprs_p ())
    {
      if (!processing_template_decl)
	add_scope_stmt (/*begin_p=*/1, /*partial_p=*/0);
      begin_scope (sk, NULL);
    }
}

/* Finish a goto-statement.  */

tree
finish_goto_stmt (tree destination)
{
  if (TREE_CODE (destination) == IDENTIFIER_NODE)
    destination = lookup_label (destination);

  /* We warn about unused labels with -Wunused.  That means we have to
     mark the used labels as used.  */
  if (TREE_CODE (destination) == LABEL_DECL)
    TREE_USED (destination) = 1;
  else
    {
      /* The DESTINATION is being used as an rvalue.  */
      if (!processing_template_decl)
	destination = decay_conversion (destination);
      /* We don't inline calls to functions with computed gotos.
	 Those functions are typically up to some funny business,
	 and may be depending on the labels being at particular
	 addresses, or some such.  */
      DECL_UNINLINABLE (current_function_decl) = 1;
    }
  
  check_goto (destination);

  return add_stmt (build_stmt (GOTO_STMT, destination));
}

/* COND is the condition-expression for an if, while, etc.,
   statement.  Convert it to a boolean value, if appropriate.  */

static tree
maybe_convert_cond (tree cond)
{
  /* Empty conditions remain empty.  */
  if (!cond)
    return NULL_TREE;

  /* Wait until we instantiate templates before doing conversion.  */
  if (processing_template_decl)
    return cond;

  /* Do the conversion.  */
  cond = convert_from_reference (cond);
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
	expr = convert_to_void (expr, "statement");
      else if (!type_dependent_expression_p (expr))
	convert_to_void (build_non_dependent_expr (expr), "statement");
      
      r = add_stmt (build_stmt (EXPR_STMT, expr));
    }

  finish_stmt ();

  return r;
}


/* Begin an if-statement.  Returns a newly created IF_STMT if
   appropriate.  */

tree
begin_if_stmt (void)
{
  tree r;
  do_pushlevel (sk_block);
  r = build_stmt (IF_STMT, NULL_TREE, NULL_TREE, NULL_TREE);
  add_stmt (r);
  return r;
}

/* Process the COND of an if-statement, which may be given by
   IF_STMT.  */

void 
finish_if_stmt_cond (tree cond, tree if_stmt)
{
  cond = maybe_convert_cond (cond);
  FINISH_COND (cond, if_stmt, IF_COND (if_stmt));
}

/* Finish the then-clause of an if-statement, which may be given by
   IF_STMT.  */

tree
finish_then_clause (tree if_stmt)
{
  RECHAIN_STMTS (if_stmt, THEN_CLAUSE (if_stmt));
  return if_stmt;
}

/* Begin the else-clause of an if-statement.  */

void 
begin_else_clause (void)
{
}

/* Finish the else-clause of an if-statement, which may be given by
   IF_STMT.  */

void
finish_else_clause (tree if_stmt)
{
  RECHAIN_STMTS (if_stmt, ELSE_CLAUSE (if_stmt));
}

/* Finish an if-statement.  */

void 
finish_if_stmt (void)
{
  finish_stmt ();
  do_poplevel ();
}

/* Begin a while-statement.  Returns a newly created WHILE_STMT if
   appropriate.  */

tree
begin_while_stmt (void)
{
  tree r;
  r = build_stmt (WHILE_STMT, NULL_TREE, NULL_TREE);
  add_stmt (r);
  do_pushlevel (sk_block);
  return r;
}

/* Process the COND of a while-statement, which may be given by
   WHILE_STMT.  */

void 
finish_while_stmt_cond (tree cond, tree while_stmt)
{
  cond = maybe_convert_cond (cond);
  if (processing_template_decl)
    /* Don't mess with condition decls in a template.  */
    FINISH_COND (cond, while_stmt, WHILE_COND (while_stmt));
  else if (getdecls () == NULL_TREE)
    /* It was a simple condition; install it.  */
    WHILE_COND (while_stmt) = cond;
  else
    {
      /* If there was a declaration in the condition, we can't leave it
	 there; transform
	    while (A x = 42) { }
	 to
	    while (true) { A x = 42; if (!x) break; }  */
      tree if_stmt;
      WHILE_COND (while_stmt) = boolean_true_node;

      if_stmt = begin_if_stmt ();
      cond = build_unary_op (TRUTH_NOT_EXPR, cond, 0);
      finish_if_stmt_cond (cond, if_stmt);
      finish_break_stmt ();
      finish_then_clause (if_stmt);
      finish_if_stmt ();
    }
}

/* Finish a while-statement, which may be given by WHILE_STMT.  */

void 
finish_while_stmt (tree while_stmt)
{
  do_poplevel ();
  RECHAIN_STMTS (while_stmt, WHILE_BODY (while_stmt));
  finish_stmt ();
}

/* Begin a do-statement.  Returns a newly created DO_STMT if
   appropriate.  */

tree
begin_do_stmt (void)
{
  tree r = build_stmt (DO_STMT, NULL_TREE, NULL_TREE);
  add_stmt (r);
  return r;
}

/* Finish the body of a do-statement, which may be given by DO_STMT.  */

void
finish_do_body (tree do_stmt)
{
  RECHAIN_STMTS (do_stmt, DO_BODY (do_stmt));
}

/* Finish a do-statement, which may be given by DO_STMT, and whose
   COND is as indicated.  */

void
finish_do_stmt (tree cond, tree do_stmt)
{
  cond = maybe_convert_cond (cond);
  DO_COND (do_stmt) = cond;
  finish_stmt ();
}

/* Finish a return-statement.  The EXPRESSION returned, if any, is as
   indicated.  */

tree
finish_return_stmt (tree expr)
{
  tree r;

  expr = check_return_expr (expr);
  if (!processing_template_decl)
    {
      if (DECL_DESTRUCTOR_P (current_function_decl))
	{
	  /* Similarly, all destructors must run destructors for
	     base-classes before returning.  So, all returns in a
	     destructor get sent to the DTOR_LABEL; finish_function emits
	     code to return a value there.  */
	  return finish_goto_stmt (dtor_label);
	}
    }
  r = add_stmt (build_stmt (RETURN_STMT, expr));
  finish_stmt ();

  return r;
}

/* Begin a for-statement.  Returns a new FOR_STMT if appropriate.  */

tree
begin_for_stmt (void)
{
  tree r;

  r = build_stmt (FOR_STMT, NULL_TREE, NULL_TREE, 
		  NULL_TREE, NULL_TREE);
  NEW_FOR_SCOPE_P (r) = flag_new_for_scope > 0;
  if (NEW_FOR_SCOPE_P (r))
    do_pushlevel (sk_for);
  add_stmt (r);

  return r;
}

/* Finish the for-init-statement of a for-statement, which may be
   given by FOR_STMT.  */

void
finish_for_init_stmt (tree for_stmt)
{
  if (last_tree != for_stmt)
    RECHAIN_STMTS (for_stmt, FOR_INIT_STMT (for_stmt));
  do_pushlevel (sk_block);
}

/* Finish the COND of a for-statement, which may be given by
   FOR_STMT.  */

void
finish_for_cond (tree cond, tree for_stmt)
{
  cond = maybe_convert_cond (cond);
  if (processing_template_decl)
    /* Don't mess with condition decls in a template.  */
    FINISH_COND (cond, for_stmt, FOR_COND (for_stmt));
  else if (getdecls () == NULL_TREE)
    /* It was a simple condition; install it.  */
    FOR_COND (for_stmt) = cond;
  else
    {
      /* If there was a declaration in the condition, we can't leave it
	 there; transform
	    for (; A x = 42;) { }
	 to
	    for (;;) { A x = 42; if (!x) break; }  */
      tree if_stmt;
      FOR_COND (for_stmt) = NULL_TREE;

      if_stmt = begin_if_stmt ();
      cond = build_unary_op (TRUTH_NOT_EXPR, cond, 0);
      finish_if_stmt_cond (cond, if_stmt);
      finish_break_stmt ();
      finish_then_clause (if_stmt);
      finish_if_stmt ();
    }
}

/* Finish the increment-EXPRESSION in a for-statement, which may be
   given by FOR_STMT.  */

void
finish_for_expr (tree expr, tree for_stmt)
{
  /* If EXPR is an overloaded function, issue an error; there is no
     context available to use to perform overload resolution.  */
  if (expr && type_unknown_p (expr))
    {
      cxx_incomplete_type_error (expr, TREE_TYPE (expr));
      expr = error_mark_node;
    }
  FOR_EXPR (for_stmt) = expr;
}

/* Finish the body of a for-statement, which may be given by
   FOR_STMT.  The increment-EXPR for the loop must be
   provided.  */

void
finish_for_stmt (tree for_stmt)
{
  /* Pop the scope for the body of the loop.  */
  do_poplevel ();
  RECHAIN_STMTS (for_stmt, FOR_BODY (for_stmt));
  if (NEW_FOR_SCOPE_P (for_stmt))
    do_poplevel ();
  finish_stmt (); 
}

/* Finish a break-statement.  */

tree
finish_break_stmt (void)
{
  return add_stmt (build_break_stmt ());
}

/* Finish a continue-statement.  */

tree
finish_continue_stmt (void)
{
  return add_stmt (build_continue_stmt ());
}

/* Begin a switch-statement.  Returns a new SWITCH_STMT if
   appropriate.  */

tree
begin_switch_stmt (void)
{
  tree r;
  do_pushlevel (sk_block);
  r = build_stmt (SWITCH_STMT, NULL_TREE, NULL_TREE, NULL_TREE);
  add_stmt (r);
  return r;
}

/* Finish the cond of a switch-statement.  */

void
finish_switch_cond (tree cond, tree switch_stmt)
{
  tree orig_type = NULL;
  if (!processing_template_decl)
    {
      tree index;

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
	  cond = fold (build1 (CLEANUP_POINT_EXPR, TREE_TYPE (cond), cond));
	}

      if (cond != error_mark_node)
	{
	  index = get_unwidened (cond, NULL_TREE);
	  /* We can't strip a conversion from a signed type to an unsigned,
	     because if we did, int_fits_type_p would do the wrong thing
	     when checking case values for being in range,
	     and it's too hard to do the right thing.  */
	  if (TREE_UNSIGNED (TREE_TYPE (cond))
	      == TREE_UNSIGNED (TREE_TYPE (index)))
	    cond = index;
	}
    }
  FINISH_COND (cond, switch_stmt, SWITCH_COND (switch_stmt));
  SWITCH_TYPE (switch_stmt) = orig_type;
  push_switch (switch_stmt);
}

/* Finish the body of a switch-statement, which may be given by
   SWITCH_STMT.  The COND to switch on is indicated.  */

void
finish_switch_stmt (tree switch_stmt)
{
  RECHAIN_STMTS (switch_stmt, SWITCH_BODY (switch_stmt));
  pop_switch (); 
  finish_stmt ();
  do_poplevel ();
}

/* Generate the RTL for T, which is a TRY_BLOCK.  */

static void 
genrtl_try_block (tree t)
{
  if (CLEANUP_P (t))
    {
      expand_eh_region_start ();
      expand_stmt (TRY_STMTS (t));
      expand_eh_region_end_cleanup (TRY_HANDLERS (t));
    }
  else
    {
      if (!FN_TRY_BLOCK_P (t)) 
	emit_line_note (input_location);

      expand_eh_region_start ();
      expand_stmt (TRY_STMTS (t));

      if (FN_TRY_BLOCK_P (t))
	{
	  expand_start_all_catch ();
	  in_function_try_handler = 1;
	  expand_stmt (TRY_HANDLERS (t));
	  in_function_try_handler = 0;
	  expand_end_all_catch ();
	}
      else 
	{
	  expand_start_all_catch ();  
	  expand_stmt (TRY_HANDLERS (t));
	  expand_end_all_catch ();
	}
    }
}

/* Generate the RTL for T, which is an EH_SPEC_BLOCK.  */

static void 
genrtl_eh_spec_block (tree t)
{
  expand_eh_region_start ();
  expand_stmt (EH_SPEC_STMTS (t));
  expand_eh_region_end_allowed (EH_SPEC_RAISES (t),
				build_call (call_unexpected_node,
					    tree_cons (NULL_TREE,
						       build_exc_ptr (),
						       NULL_TREE)));
}

/* Begin a try-block.  Returns a newly-created TRY_BLOCK if
   appropriate.  */

tree
begin_try_block (void)
{
  tree r = build_stmt (TRY_BLOCK, NULL_TREE, NULL_TREE);
  add_stmt (r);
  return r;
}

/* Likewise, for a function-try-block.  */

tree
begin_function_try_block (void)
{
  tree r = build_stmt (TRY_BLOCK, NULL_TREE, NULL_TREE);
  FN_TRY_BLOCK_P (r) = 1;
  add_stmt (r);
  return r;
}

/* Finish a try-block, which may be given by TRY_BLOCK.  */

void
finish_try_block (tree try_block)
{
  RECHAIN_STMTS (try_block, TRY_STMTS (try_block));
}

/* Finish the body of a cleanup try-block, which may be given by
   TRY_BLOCK.  */

void
finish_cleanup_try_block (tree try_block)
{
  RECHAIN_STMTS (try_block, TRY_STMTS (try_block));
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
  if (TREE_CHAIN (try_block) 
      && TREE_CODE (TREE_CHAIN (try_block)) == CTOR_INITIALIZER)
    {
      /* Chain the compound statement after the CTOR_INITIALIZER.  */
      TREE_CHAIN (TREE_CHAIN (try_block)) = last_tree;
      /* And make the CTOR_INITIALIZER the body of the try-block.  */
      RECHAIN_STMTS (try_block, TRY_STMTS (try_block));
    }
  else
    RECHAIN_STMTS (try_block, TRY_STMTS (try_block));
  in_function_try_handler = 1;
}

/* Finish a handler-sequence for a try-block, which may be given by
   TRY_BLOCK.  */

void
finish_handler_sequence (tree try_block)
{
  RECHAIN_STMTS (try_block, TRY_HANDLERS (try_block));
  check_handlers (TRY_HANDLERS (try_block));
}

/* Likewise, for a function-try-block.  */

void
finish_function_handler_sequence (tree try_block)
{
  in_function_try_handler = 0;
  RECHAIN_STMTS (try_block, TRY_HANDLERS (try_block));
  check_handlers (TRY_HANDLERS (try_block));
}

/* Generate the RTL for T, which is a HANDLER.  */

static void
genrtl_handler (tree t)
{
  genrtl_do_pushlevel ();
  if (!processing_template_decl)
    expand_start_catch (HANDLER_TYPE (t));
  expand_stmt (HANDLER_BODY (t));
  if (!processing_template_decl)
    expand_end_catch ();
}

/* Begin a handler.  Returns a HANDLER if appropriate.  */

tree
begin_handler (void)
{
  tree r;
  r = build_stmt (HANDLER, NULL_TREE, NULL_TREE);
  add_stmt (r);
  /* Create a binding level for the eh_info and the exception object
     cleanup.  */
  do_pushlevel (sk_catch);
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
	  add_decl_stmt (decl);
	  RECHAIN_STMTS (handler, HANDLER_PARMS (handler));
	  type = TREE_TYPE (decl);
	}
    }
  else
    type = expand_start_catch_block (decl);

  HANDLER_TYPE (handler) = type;
  if (!processing_template_decl && type)
    mark_used (eh_type_info (type));
}

/* Finish a handler, which may be given by HANDLER.  The BLOCKs are
   the return value from the matching call to finish_handler_parms.  */

void
finish_handler (tree handler)
{
  if (!processing_template_decl)
    expand_end_catch_block ();
  do_poplevel ();
  RECHAIN_STMTS (handler, HANDLER_BODY (handler));
}

/* Begin a compound-statement.  If HAS_NO_SCOPE is true, the
   compound-statement does not define a scope.  Returns a new
   COMPOUND_STMT.  */

tree
begin_compound_stmt (bool has_no_scope)
{
  tree r; 
  int is_try = 0;

  r = build_stmt (COMPOUND_STMT, NULL_TREE);

  if (last_tree && TREE_CODE (last_tree) == TRY_BLOCK)
    is_try = 1;

  add_stmt (r);
  if (has_no_scope)
    COMPOUND_STMT_NO_SCOPE (r) = 1;

  last_expr_type = NULL_TREE;

  if (!has_no_scope)
    do_pushlevel (is_try ? sk_try : sk_block);
  else
    /* Normally, we try hard to keep the BLOCK for a
       statement-expression.  But, if it's a statement-expression with
       a scopeless block, there's nothing to keep, and we don't want
       to accidentally keep a block *inside* the scopeless block.  */ 
    keep_next_level (false);

  return r;
}

/* Finish a compound-statement, which is given by COMPOUND_STMT.  */

tree
finish_compound_stmt (tree compound_stmt)
{
  tree r;
  tree t;

  if (COMPOUND_STMT_NO_SCOPE (compound_stmt))
    r = NULL_TREE;
  else
    r = do_poplevel ();

  RECHAIN_STMTS (compound_stmt, COMPOUND_BODY (compound_stmt));

  /* When we call finish_stmt we will lose LAST_EXPR_TYPE.  But, since
     the precise purpose of that variable is store the type of the
     last expression statement within the last compound statement, we
     preserve the value.  */
  t = last_expr_type;
  finish_stmt ();
  last_expr_type = t;

  return r;
}

/* Finish an asm-statement, whose components are a CV_QUALIFIER, a
   STRING, some OUTPUT_OPERANDS, some INPUT_OPERANDS, and some
   CLOBBERS.  */

tree
finish_asm_stmt (tree cv_qualifier, 
                 tree string, 
                 tree output_operands,
		 tree input_operands, 
                 tree clobbers)
{
  tree r;
  tree t;

  if (cv_qualifier != NULL_TREE
      && cv_qualifier != ridpointers[(int) RID_VOLATILE])
    {
      warning ("%s qualifier ignored on asm",
		  IDENTIFIER_POINTER (cv_qualifier));
      cv_qualifier = NULL_TREE;
    }

  if (!processing_template_decl)
    {
      int i;
      int ninputs;
      int noutputs;

      for (t = input_operands; t; t = TREE_CHAIN (t))
	{
	  tree converted_operand 
	    = decay_conversion (TREE_VALUE (t)); 
	  
	  /* If the type of the operand hasn't been determined (e.g.,
	     because it involves an overloaded function), then issue
	     an error message.  There's no context available to
	     resolve the overloading.  */
	  if (TREE_TYPE (converted_operand) == unknown_type_node)
	    {
	      error ("type of asm operand `%E' could not be determined", 
			TREE_VALUE (t));
	      converted_operand = error_mark_node;
	    }
	  TREE_VALUE (t) = converted_operand;
	}

      ninputs = list_length (input_operands);
      noutputs = list_length (output_operands);

      for (i = 0, t = output_operands; t; t = TREE_CHAIN (t), ++i)
	{
	  bool allows_mem;
	  bool allows_reg;
	  bool is_inout;
	  const char *constraint;
	  tree operand;

	  constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (t)));
	  operand = TREE_VALUE (t);

	  if (!parse_output_constraint (&constraint,
					i, ninputs, noutputs,
					&allows_mem,
					&allows_reg,
					&is_inout))
	    {
	      /* By marking this operand as erroneous, we will not try
		 to process this operand again in expand_asm_operands.  */
	      TREE_VALUE (t) = error_mark_node;
	      continue;
	    }

	  /* If the operand is a DECL that is going to end up in
	     memory, assume it is addressable.  This is a bit more
	     conservative than it would ideally be; the exact test is
	     buried deep in expand_asm_operands and depends on the
	     DECL_RTL for the OPERAND -- which we don't have at this
	     point.  */
	  if (!allows_reg && DECL_P (operand))
	    cxx_mark_addressable (operand);
	}
    }

  r = build_stmt (ASM_STMT, cv_qualifier, string,
		  output_operands, input_operands,
		  clobbers);
  return add_stmt (r);
}

/* Finish a label with the indicated NAME.  */

tree
finish_label_stmt (tree name)
{
  tree decl = define_label (input_location, name);
  return add_stmt (build_stmt (LABEL_STMT, decl));
}

/* Finish a series of declarations for local labels.  G++ allows users
   to declare "local" labels, i.e., labels with scope.  This extension
   is useful when writing code involving statement-expressions.  */

void
finish_label_decl (tree name)
{
  tree decl = declare_local_label (name);
  add_decl_stmt (decl);
}

/* When DECL goes out of scope, make sure that CLEANUP is executed.  */

void 
finish_decl_cleanup (tree decl, tree cleanup)
{
  add_stmt (build_stmt (CLEANUP_STMT, decl, cleanup));
}

/* If the current scope exits with an exception, run CLEANUP.  */

void
finish_eh_cleanup (tree cleanup)
{
  tree r = build_stmt (CLEANUP_STMT, NULL_TREE, cleanup);
  CLEANUP_EH_ONLY (r) = 1;
  add_stmt (r);
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
    add_stmt (build_min_nt (CTOR_INITIALIZER, mem_inits));
  else
    emit_mem_initializers (mem_inits);
}

/* Returns the stack of SCOPE_STMTs for the current function.  */

tree *
current_scope_stmt_stack (void)
{
  return &cfun->language->base.x_scope_stmt_stack;
}

/* Finish a parenthesized expression EXPR.  */

tree
finish_parenthesized_expr (tree expr)
{
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (TREE_CODE (expr))))
    /* This inhibits warnings in c_common_truthvalue_conversion.  */
    C_SET_EXP_ORIGINAL_CODE (expr, ERROR_MARK); 

  if (TREE_CODE (expr) == OFFSET_REF)
    /* [expr.unary.op]/3 The qualified id of a pointer-to-member must not be
       enclosed in parentheses.  */
    PTRMEM_OK_P (expr) = 0;
  return expr;
}

/* Finish a reference to a non-static data member (DECL) that is not
   preceded by `.' or `->'.  */

tree
finish_non_static_data_member (tree decl, tree object, tree qualifying_scope)
{
  my_friendly_assert (TREE_CODE (decl) == FIELD_DECL, 20020909);

  if (!object)
    {
      if (current_function_decl 
	  && DECL_STATIC_FUNCTION_P (current_function_decl))
	cp_error_at ("invalid use of member `%D' in static member function",
		     decl);
      else
	cp_error_at ("invalid use of non-static data member `%D'", decl);
      error ("from this location");

      return error_mark_node;
    }
  TREE_USED (current_class_ptr) = 1;
  if (processing_template_decl && !qualifying_scope)
    {
      tree type = TREE_TYPE (decl);

      if (TREE_CODE (type) == REFERENCE_TYPE)
	type = TREE_TYPE (type);
      else
	{
	  /* Set the cv qualifiers.  */
	  int quals = cp_type_quals (TREE_TYPE (current_class_ref));
	  
	  if (DECL_MUTABLE_P (decl))
	    quals &= ~TYPE_QUAL_CONST;

	  quals |= cp_type_quals (TREE_TYPE (decl));
	  type = cp_build_qualified_type (type, quals);
	}
      
      return build_min (COMPONENT_REF, type, object, decl);
    }
  else
    {
      tree access_type = TREE_TYPE (object);
      tree lookup_context = context_for_name_lookup (decl);
      
      while (!DERIVED_FROM_P (lookup_context, access_type))
	{
	  access_type = TYPE_CONTEXT (access_type);
	  while (access_type && DECL_P (access_type))
	    access_type = DECL_CONTEXT (access_type);

	  if (!access_type)
	    {
	      cp_error_at ("object missing in reference to `%D'", decl);
	      error ("from this location");
	      return error_mark_node;
	    }
	}

      /* If PROCESSING_TEMPLATE_DECL is nonzero here, then
	 QUALIFYING_SCOPE is also non-null.  Wrap this in a SCOPE_REF
	 for now.  */
      if (processing_template_decl)
	return build_min (SCOPE_REF, TREE_TYPE (decl),
			  qualifying_scope, DECL_NAME (decl));

      perform_or_defer_access_check (TYPE_BINFO (access_type), decl);

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
					     /*preserve_reference=*/false);
    }
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

  if (qualifying_type)
    perform_or_defer_access_check (TYPE_BINFO (qualifying_type), decl);
}

/* EXPR is the result of a qualified-id.  The QUALIFYING_CLASS was the
   class named to the left of the "::" operator.  DONE is true if this
   expression is a complete postfix-expression; it is false if this
   expression is followed by '->', '[', '(', etc.  ADDRESS_P is true
   iff this expression is the operand of '&'.  */

tree
finish_qualified_id_expr (tree qualifying_class, tree expr, bool done,
			  bool address_p)
{
  if (error_operand_p (expr))
    return error_mark_node;

  /* If EXPR occurs as the operand of '&', use special handling that
     permits a pointer-to-member.  */
  if (address_p && done)
    {
      if (TREE_CODE (expr) == SCOPE_REF)
	expr = TREE_OPERAND (expr, 1);
      expr = build_offset_ref (qualifying_class, expr, 
			       /*address_p=*/true);
      return expr;
    }

  if (TREE_CODE (expr) == FIELD_DECL)
    expr = finish_non_static_data_member (expr, current_class_ref,
					  qualifying_class);
  else if (BASELINK_P (expr) && !processing_template_decl)
    {
      tree fn;
      tree fns;

      /* See if any of the functions are non-static members.  */
      fns = BASELINK_FUNCTIONS (expr);
      if (TREE_CODE (fns) == TEMPLATE_ID_EXPR)
	fns = TREE_OPERAND (fns, 0);
      for (fn = fns; fn; fn = OVL_NEXT (fn))
	if (DECL_NONSTATIC_MEMBER_FUNCTION_P (fn))
	  break;
      /* If so, the expression may be relative to the current
	 class.  */
      if (fn && current_class_type 
	  && DERIVED_FROM_P (qualifying_class, current_class_type))
	expr = (build_class_member_access_expr 
		(maybe_dummy_object (qualifying_class, NULL),
		 expr,
		 BASELINK_ACCESS_BINFO (expr),
		 /*preserve_reference=*/false));
      else if (done)
	/* The expression is a qualified name whose address is not
	   being taken.  */
	expr = build_offset_ref (qualifying_class, expr, /*address_p=*/false);
    }

  return expr;
}

/* Begin a statement-expression.  The value returned must be passed to
   finish_stmt_expr.  */

tree 
begin_stmt_expr (void)
{
  /* If we're outside a function, we won't have a statement-tree to
     work with.  But, if we see a statement-expression we need to
     create one.  */
  if (! cfun && !last_tree)
    begin_stmt_tree (&scope_chain->x_saved_tree);

  last_expr_type = NULL_TREE;
  
  keep_next_level (true);

  return last_tree; 
}

/* Process the final expression of a statement expression. EXPR can be
   NULL, if the final expression is empty.  Build up a TARGET_EXPR so
   that the result value can be safely returned to the enclosing
   expression.  */

tree
finish_stmt_expr_expr (tree expr)
{
  tree result = NULL_TREE;
  tree type = void_type_node;

  if (expr)
    {
      type = TREE_TYPE (expr);
      
      if (!processing_template_decl && !VOID_TYPE_P (TREE_TYPE (expr)))
	{
	  if (TREE_CODE (type) == ARRAY_TYPE
	      || TREE_CODE (type) == FUNCTION_TYPE)
	    expr = decay_conversion (expr);

	  expr = convert_from_reference (expr);
	  expr = require_complete_type (expr);

	  /* Build a TARGET_EXPR for this aggregate.  finish_stmt_expr
	     will then pull it apart so the lifetime of the target is
	     within the scope of the expression containing this statement
	     expression.  */
	  if (TREE_CODE (expr) == TARGET_EXPR)
	    ;
	  else if (!IS_AGGR_TYPE (type) || TYPE_HAS_TRIVIAL_INIT_REF (type))
	    expr = build_target_expr_with_type (expr, type);
	  else
	    {
	      /* Copy construct.  */
	      expr = build_special_member_call
		(NULL_TREE, complete_ctor_identifier,
		 build_tree_list (NULL_TREE, expr),
		 TYPE_BINFO (type), LOOKUP_NORMAL);
	      expr = build_cplus_new (type, expr);
	      my_friendly_assert (TREE_CODE (expr) == TARGET_EXPR, 20030729);
	    }
	}

      if (expr != error_mark_node)
	{
	  result = build_stmt (EXPR_STMT, expr);
	  add_stmt (result);
	}
    }
  
  finish_stmt ();

  /* Remember the last expression so that finish_stmt_expr can pull it
     apart.  */
  last_expr_type = result ? result : void_type_node;
  
  return result;
}

/* Finish a statement-expression.  EXPR should be the value returned
   by the previous begin_stmt_expr.  Returns an expression
   representing the statement-expression.  */

tree 
finish_stmt_expr (tree rtl_expr, bool has_no_scope)
{
  tree result;
  tree result_stmt = last_expr_type;
  tree type;
  
  if (!last_expr_type)
    type = void_type_node;
  else
    {
      if (result_stmt == void_type_node)
	{
	  type = void_type_node;
	  result_stmt = NULL_TREE;
	}
      else
	type = TREE_TYPE (EXPR_STMT_EXPR (result_stmt));
    }
  
  result = build_min (STMT_EXPR, type, last_tree);
  TREE_SIDE_EFFECTS (result) = 1;
  STMT_EXPR_NO_SCOPE (result) = has_no_scope;
  
  last_expr_type = NULL_TREE;
  
  /* Remove the compound statement from the tree structure; it is
     now saved in the STMT_EXPR.  */
  last_tree = rtl_expr;
  TREE_CHAIN (last_tree) = NULL_TREE;

  /* If we created a statement-tree for this statement-expression,
     remove it now.  */ 
  if (! cfun
      && TREE_CHAIN (scope_chain->x_saved_tree) == NULL_TREE)
    finish_stmt_tree (&scope_chain->x_saved_tree);

  if (processing_template_decl)
    return result;

  if (!VOID_TYPE_P (type))
    {
      /* Pull out the TARGET_EXPR that is the final expression. Put
	 the target's init_expr as the final expression and then put
	 the statement expression itself as the target's init
	 expr. Finally, return the target expression.  */
      tree last_expr = EXPR_STMT_EXPR (result_stmt);
      
      my_friendly_assert (TREE_CODE (last_expr) == TARGET_EXPR, 20030729);
      EXPR_STMT_EXPR (result_stmt) = TREE_OPERAND (last_expr, 1);
      TREE_OPERAND (last_expr, 1) = result;
      result = last_expr;
    }
  return result;
}

/* Perform Koenig lookup.  FN is the postfix-expression representing
   the function (or functions) to call; ARGS are the arguments to the
   call.  Returns the functions to be considered by overload
   resolution.  */

tree
perform_koenig_lookup (tree fn, tree args)
{
  tree identifier = NULL_TREE;
  tree functions = NULL_TREE;

  /* Find the name of the overloaded function.  */
  if (TREE_CODE (fn) == IDENTIFIER_NODE)
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
  if (!any_type_dependent_arguments_p (args))
    {
      fn = lookup_arg_dependent (identifier, functions, args);
      if (!fn)
	/* The unqualified name could not be resolved.  */
	fn = unqualified_fn_lookup_error (identifier);
    }
  else
    fn = identifier;

  return fn;
}

/* Generate an expression for `FN (ARGS)'.

   If DISALLOW_VIRTUAL is true, the call to FN will be not generated
   as a virtual call, even if FN is virtual.  (This flag is set when
   encountering an expression where the function name is explicitly
   qualified.  For example a call to `X::f' never generates a virtual
   call.)

   Returns code for the call.  */

tree 
finish_call_expr (tree fn, tree args, bool disallow_virtual, bool koenig_p)
{
  tree result;
  tree orig_fn;
  tree orig_args;

  if (fn == error_mark_node || args == error_mark_node)
    return error_mark_node;

  /* ARGS should be a list of arguments.  */
  my_friendly_assert (!args || TREE_CODE (args) == TREE_LIST,
		      20020712);

  orig_fn = fn;
  orig_args = args;

  if (processing_template_decl)
    {
      if (type_dependent_expression_p (fn)
	  || any_type_dependent_arguments_p (args))
	{
	  result = build_nt (CALL_EXPR, fn, args);
	  KOENIG_LOOKUP_P (result) = koenig_p;
	  return result;
	}
      if (!BASELINK_P (fn)
	  && TREE_CODE (fn) != PSEUDO_DTOR_EXPR
	  && TREE_TYPE (fn) != unknown_type_node)
	fn = build_non_dependent_expr (fn);
      args = build_non_dependent_args (orig_args);
    }

  /* A reference to a member function will appear as an overloaded
     function (rather than a BASELINK) if an unqualified name was used
     to refer to it.  */
  if (!BASELINK_P (fn) && is_overloaded_fn (fn))
    {
      tree f = fn;

      if (TREE_CODE (f) == TEMPLATE_ID_EXPR)
	f = TREE_OPERAND (f, 0);
      f = get_first_fn (f);
      if (DECL_FUNCTION_MEMBER_P (f))
	{
	  tree type = currently_open_derived_class (DECL_CONTEXT (f));
	  if (!type)
	    type = DECL_CONTEXT (f);
	  fn = build_baselink (TYPE_BINFO (type),
			       TYPE_BINFO (type),
			       fn, /*optype=*/NULL_TREE);
	}
    }

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

        This paragraph is unclear about this situation:

	  struct A { void f(); };
	  struct B : public A {};
	  struct C : public A { void g() { B::f(); }};

	In particular, for `B::f', this paragraph does not make clear
	whether "the class of that member function" refers to `A' or 
	to `B'.  We believe it refers to `B'.  */
      if (current_class_type 
	  && DERIVED_FROM_P (BINFO_TYPE (BASELINK_ACCESS_BINFO (fn)),
			     current_class_type)
	  && current_class_ref)
	object = maybe_dummy_object (BINFO_TYPE (BASELINK_ACCESS_BINFO (fn)),
				     NULL);
      else
	{
	  tree representative_fn;

	  representative_fn = BASELINK_FUNCTIONS (fn);
	  if (TREE_CODE (representative_fn) == TEMPLATE_ID_EXPR)
	    representative_fn = TREE_OPERAND (representative_fn, 0);
	  representative_fn = get_first_fn (representative_fn);
	  object = build_dummy_object (DECL_CONTEXT (representative_fn));
	}

      if (processing_template_decl)
	{
	  if (type_dependent_expression_p (object))
	    return build_nt (CALL_EXPR, orig_fn, orig_args);
	  object = build_non_dependent_expr (object);
	}

      result = build_new_method_call (object, fn, args, NULL_TREE,
				      (disallow_virtual 
				       ? LOOKUP_NONVIRTUAL : 0));
    }
  else if (is_overloaded_fn (fn))
    /* A call to a namespace-scope function.  */
    result = build_new_function_call (fn, args);
  else if (TREE_CODE (fn) == PSEUDO_DTOR_EXPR)
    {
      if (args)
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
    result = build_new_op (CALL_EXPR, LOOKUP_NORMAL, fn, args, NULL_TREE,
			   /*overloaded_p=*/NULL);
  if (!result)
    /* A call where the function is unknown.  */
    result = build_function_call (fn, args);

  if (processing_template_decl)
    {
      result = build (CALL_EXPR, TREE_TYPE (result), orig_fn, orig_args);
      KOENIG_LOOKUP_P (result) = koenig_p;
    }
  return result;
}

/* Finish a call to a postfix increment or decrement or EXPR.  (Which
   is indicated by CODE, which should be POSTINCREMENT_EXPR or
   POSTDECREMENT_EXPR.)  */

tree 
finish_increment_expr (tree expr, enum tree_code code)
{
  return build_x_unary_op (code, expr);  
}

/* Finish a use of `this'.  Returns an expression for `this'.  */

tree 
finish_this_expr (void)
{
  tree result;

  if (current_class_ptr)
    {
      result = current_class_ptr;
    }
  else if (current_function_decl
	   && DECL_STATIC_FUNCTION_P (current_function_decl))
    {
      error ("`this' is unavailable for static member functions");
      result = error_mark_node;
    }
  else
    {
      if (current_function_decl)
	error ("invalid use of `this' in non-member function");
      else
	error ("invalid use of `this' at top level");
      result = error_mark_node;
    }

  return result;
}

/* Finish a pseudo-destructor expression.  If SCOPE is NULL, the
   expression was of the form `OBJECT.~DESTRUCTOR' where DESTRUCTOR is
   the TYPE for the type given.  If SCOPE is non-NULL, the expression
   was of the form `OBJECT.SCOPE::~DESTRUCTOR'.  */

tree 
finish_pseudo_destructor_expr (tree object, tree scope, tree destructor)
{
  if (destructor == error_mark_node)
    return error_mark_node;

  my_friendly_assert (TYPE_P (destructor), 20010905);

  if (!processing_template_decl)
    {
      if (scope == error_mark_node)
	{
	  error ("invalid qualifying scope in pseudo-destructor name");
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
	  error ("`%E' is not of type `%T'", object, destructor);
	  return error_mark_node;
	}
    }

  return build (PSEUDO_DTOR_EXPR, void_type_node, object, scope, destructor);
}

/* Finish an expression of the form CODE EXPR.  */

tree
finish_unary_op_expr (enum tree_code code, tree expr)
{
  tree result = build_x_unary_op (code, expr);
  /* Inside a template, build_x_unary_op does not fold the
     expression. So check whether the result is folded before
     setting TREE_NEGATED_INT.  */
  if (code == NEGATE_EXPR && TREE_CODE (expr) == INTEGER_CST
      && TREE_CODE (result) == INTEGER_CST
      && !TREE_UNSIGNED (TREE_TYPE (result))
      && INT_CST_LT (result, integer_zero_node))
    TREE_NEGATED_INT (result) = 1;
  overflow_warning (result);
  return result;
}

/* Finish a compound-literal expression.  TYPE is the type to which
   the INITIALIZER_LIST is being cast.  */

tree
finish_compound_literal (tree type, tree initializer_list)
{
  tree compound_literal;

  /* Build a CONSTRUCTOR for the INITIALIZER_LIST.  */
  compound_literal = build_constructor (NULL_TREE, initializer_list);
  /* Mark it as a compound-literal.  */
  TREE_HAS_CONSTRUCTOR (compound_literal) = 1;
  if (processing_template_decl)
    TREE_TYPE (compound_literal) = type;
  else
    {
      /* Check the initialization.  */
      compound_literal = digest_init (type, compound_literal, NULL);
      /* If the TYPE was an array type with an unknown bound, then we can
	 figure out the dimension now.  For example, something like:

	   `(int []) { 2, 3 }'

	 implies that the array has two elements.  */
      if (TREE_CODE (type) == ARRAY_TYPE && !COMPLETE_TYPE_P (type))
	complete_array_type (type, compound_literal, 1);
    }

  return compound_literal;
}

/* Return the declaration for the function-name variable indicated by
   ID.  */

tree
finish_fname (tree id)
{
  tree decl;
  
  decl = fname_decl (C_RID_CODE (id), id);
  if (processing_template_decl)
    decl = DECL_NAME (decl);
  return decl;
}

/* Begin a function definition declared with DECL_SPECS, ATTRIBUTES,
   and DECLARATOR.  Returns nonzero if the function-declaration is
   valid.  */

int
begin_function_definition (tree decl_specs, tree attributes, tree declarator)
{
  if (!start_function (decl_specs, declarator, attributes, SF_DEFAULT))
    return 0;

  /* The things we're about to see are not directly qualified by any
     template headers we've seen thus far.  */
  reset_specialization ();

  return 1;
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
      pedwarn ("template type parameters must use the keyword `class' or `typename'");
      aggr = class_type_node;
    }

  return build_tree_list (aggr, identifier);
}

/* Finish a template template parameter, specified as AGGR IDENTIFIER.
   Returns the parameter.  */

tree 
finish_template_template_parm (tree aggr, tree identifier)
{
  tree decl = build_decl (TYPE_DECL, identifier, NULL_TREE);
  tree tmpl = build_lang_decl (TEMPLATE_DECL, identifier, NULL_TREE);
  DECL_TEMPLATE_PARMS (tmpl) = current_template_parms;
  DECL_TEMPLATE_RESULT (tmpl) = decl;
  DECL_ARTIFICIAL (decl) = 1;
  end_template_decl ();

  my_friendly_assert (DECL_TEMPLATE_PARMS (tmpl), 20010110);

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
	  tree t = TREE_TYPE (argument);

	  /* Try to emit a slightly smarter error message if we detect
	     that the user is using a template instantiation.  */
	  if (CLASSTYPE_TEMPLATE_INFO (t) 
	      && CLASSTYPE_TEMPLATE_INSTANTIATION (t))
	    error ("invalid use of type `%T' as a default value for a "
	           "template template-parameter", t);
	  else
	    error ("invalid use of `%D' as a default value for a template "
	           "template-parameter", argument);
	}
      else
	error ("invalid default argument for a template template parameter");
      return error_mark_node;
    }

  return argument;
}

/* Finish a parameter list, indicated by PARMS.  If ELLIPSIS is
   nonzero, the parameter list was terminated by a `...'.  */

tree
finish_parmlist (tree parms, int ellipsis)
{
  if (parms)
    {
      /* We mark the PARMS as a parmlist so that declarator processing can
         disambiguate certain constructs.  */
      TREE_PARMLIST (parms) = 1;
      /* We do not append void_list_node here, but leave it to grokparms
         to do that.  */
      PARMLIST_ELLIPSIS_P (parms) = ellipsis;
    }
  return parms;
}

/* Begin a class definition, as indicated by T.  */

tree
begin_class_definition (tree t)
{
  if (t == error_mark_node)
    return error_mark_node;

  if (processing_template_parmlist)
    {
      error ("definition of `%#T' inside template parameter list", t);
      return error_mark_node;
    }
  /* A non-implicit typename comes from code like:

       template <typename T> struct A {
         template <typename U> struct A<T>::B ...

     This is erroneous.  */
  else if (TREE_CODE (t) == TYPENAME_TYPE)
    {
      error ("invalid definition of qualified type `%T'", t);
      t = error_mark_node;
    }

  if (t == error_mark_node || ! IS_AGGR_TYPE (t))
    {
      t = make_aggr_type (RECORD_TYPE);
      pushtag (make_anon_name (), t, 0);
    }

  /* If this type was already complete, and we see another definition,
     that's an error.  */
  if (COMPLETE_TYPE_P (t))
    {
      error ("redefinition of `%#T'", t);
      cp_error_at ("previous definition of `%#T'", t);
      return error_mark_node;
    }

  /* Update the location of the decl.  */
  DECL_SOURCE_LOCATION (TYPE_NAME (t)) = input_location;
  
  if (TYPE_BEING_DEFINED (t))
    {
      t = make_aggr_type (TREE_CODE (t));
      pushtag (TYPE_IDENTIFIER (t), t, 0);
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
      CLASSTYPE_INTERFACE_ONLY (t) = interface_only;
      SET_CLASSTYPE_INTERFACE_UNKNOWN_X
	(t, interface_unknown);
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
  my_friendly_assert (TREE_CHAIN (decl) == NULL_TREE, 0);

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

  /* Mark the DECL as a member of the current class.  */
  DECL_CONTEXT (decl) = current_class_type;

  /* [dcl.link]

     A C language linkage is ignored for the names of class members
     and the member function type of class member functions.  */
  if (DECL_LANG_SPECIFIC (decl) && DECL_LANGUAGE (decl) == lang_c)
    SET_DECL_LANGUAGE (decl, lang_cplusplus);

  /* Put functions on the TYPE_METHODS list and everything else on the
     TYPE_FIELDS list.  Note that these are built up in reverse order.
     We reverse them (to obtain declaration order) in finish_struct.  */
  if (TREE_CODE (decl) == FUNCTION_DECL 
      || DECL_FUNCTION_TEMPLATE_P (decl))
    {
      /* We also need to add this function to the
	 CLASSTYPE_METHOD_VEC.  */
      add_method (current_class_type, decl, /*error_p=*/0);

      TREE_CHAIN (decl) = TYPE_METHODS (current_class_type);
      TYPE_METHODS (current_class_type) = decl;

      maybe_add_class_template_decl_list (current_class_type, decl, 
					  /*friend_p=*/0);
    }
  /* Enter the DECL into the scope of the class.  */
  else if ((TREE_CODE (decl) == USING_DECL && TREE_TYPE (decl))
	   || pushdecl_class_level (decl))
    {
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
	  TREE_CHAIN (decl) = TYPE_FIELDS (current_class_type);
	  TYPE_FIELDS (current_class_type) = decl;
	}

      maybe_add_class_template_decl_list (current_class_type, decl, 
					  /*friend_p=*/0);
    }
}

/* Finish processing the declaration of a member class template
   TYPES whose template parameters are given by PARMS.  */

tree
finish_member_class_template (tree types)
{
  tree t;

  /* If there are declared, but undefined, partial specializations
     mixed in with the typespecs they will not yet have passed through
     maybe_process_partial_specialization, so we do that here.  */
  for (t = types; t != NULL_TREE; t = TREE_CHAIN (t))
    if (IS_AGGR_TYPE_CODE (TREE_CODE (TREE_VALUE (t))))
      maybe_process_partial_specialization (TREE_VALUE (t));

  grok_x_components (types);
  if (TYPE_CONTEXT (TREE_VALUE (types)) != current_class_type)
    /* The component was in fact a friend declaration.  We avoid
       finish_member_template_decl performing certain checks by
       unsetting TYPES.  */
    types = NULL_TREE;
  
  finish_member_template_decl (types);

  /* As with other component type declarations, we do
     not store the new DECL on the list of
     component_decls.  */
  return NULL_TREE;
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
  tree decl;

  decl = lookup_template_class (name, args,
				NULL_TREE, NULL_TREE, entering_scope,
				tf_error | tf_warning | tf_user);
  if (decl != error_mark_node)
    decl = TYPE_STUB_DECL (decl);

  return decl;
}

/* Finish processing a BASE_CLASS with the indicated ACCESS_SPECIFIER.
   Return a TREE_LIST containing the ACCESS_SPECIFIER and the
   BASE_CLASS, or NULL_TREE if an error occurred.  The
   ACCESS_SPECIFIER is one of
   access_{default,public,protected_private}[_virtual]_node.*/

tree 
finish_base_specifier (tree base, tree access, bool virtual_p)
{
  tree result;

  if (base == error_mark_node)
    {
      error ("invalid base-class specification");
      result = NULL_TREE;
    }
  else if (! is_aggr_type (base, 1))
    result = NULL_TREE;
  else
    {
      if (cp_type_quals (base) != 0)
        {
          error ("base class `%T' has cv qualifiers", base);
          base = TYPE_MAIN_VARIANT (base);
        }
      result = build_tree_list (access, base);
      TREE_VIA_VIRTUAL (result) = virtual_p;
    }

  return result;
}

/* Called when multiple declarators are processed.  If that is not
   permitted in this context, an error is issued.  */

void
check_multiple_declarators (void)
{
  /* [temp]
     
     In a template-declaration, explicit specialization, or explicit
     instantiation the init-declarator-list in the declaration shall
     contain at most one declarator.  

     We don't just use PROCESSING_TEMPLATE_DECL for the first
     condition since that would disallow the perfectly valid code, 
     like `template <class T> struct S { int i, j; };'.  */
  if (at_function_scope_p ())
    /* It's OK to write `template <class T> void f() { int i, j;}'.  */
    return;
     
  if (PROCESSING_REAL_TEMPLATE_DECL_P () 
      || processing_explicit_instantiation
      || processing_specialization)
    error ("multiple declarators in template declaration");
}

/* Issue a diagnostic that NAME cannot be found in SCOPE.  */

void
qualified_name_lookup_error (tree scope, tree name)
{
  if (TYPE_P (scope))
    {
      if (!COMPLETE_TYPE_P (scope))
	error ("incomplete type `%T' used in nested name specifier", scope);
      else
	error ("`%D' is not a member of `%T'", name, scope);
    }
  else if (scope != global_namespace)
    error ("`%D' is not a member of `%D'", name, scope);
  else
    error ("`::%D' has not been declared", name);
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
		      tree *qualifying_class,
		      bool integral_constant_expression_p,
		      bool allow_non_integral_constant_expression_p,
		      bool *non_integral_constant_expression_p,
		      const char **error_msg)
{
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
		      && !(TREE_CODE (id_expression) == IDENTIFIER_NODE
			   && IDENTIFIER_TYPENAME_P (id_expression)
			   && dependent_type_p (TREE_TYPE (id_expression))))))
	    {
	      /* If the qualifying type is non-dependent (and the name
		 does not name a conversion operator to a dependent
		 type), issue an error.  */
	      qualified_name_lookup_error (scope, id_expression);
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
      if (!scope && decl != error_mark_node)
	maybe_note_name_used_in_class (id_expression, decl);
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
      *idk = CP_ID_KIND_NONE;
      if (TREE_CODE (decl) == TEMPLATE_PARM_INDEX)
	decl = TEMPLATE_PARM_DECL (decl);
      if (integral_constant_expression_p 
	  && !dependent_type_p (TREE_TYPE (decl))
	  && !INTEGRAL_OR_ENUMERATION_TYPE_P (TREE_TYPE (decl))) 
	{
	  if (!allow_non_integral_constant_expression_p)
	    error ("template parameter `%D' of type `%T' is not allowed in "
		   "an integral constant expression because it is not of "
		   "integral or enumeration type", decl, TREE_TYPE (decl));
	  *non_integral_constant_expression_p = true;
	}
      return DECL_INITIAL (decl);
    }
  /* Similarly, we resolve enumeration constants to their 
     underlying values.  */
  else if (TREE_CODE (decl) == CONST_DECL)
    {
      *idk = CP_ID_KIND_NONE;
      if (!processing_template_decl)
	return DECL_INITIAL (decl);
      return decl;
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
      /* A template-id where the name of the template was not resolved
	 is definitely dependent.  */
      else if (TREE_CODE (decl) == TEMPLATE_ID_EXPR
	       && (TREE_CODE (TREE_OPERAND (decl, 0)) 
		   == IDENTIFIER_NODE))
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
		*qualifying_class = scope;
	      /* Since this name was dependent, the expression isn't
		 constant -- yet.  No error is issued because it might
		 be constant when things are instantiated.  */
	      if (integral_constant_expression_p)
		*non_integral_constant_expression_p = true;
	      if (TYPE_P (scope) && dependent_type_p (scope))
		return build_nt (SCOPE_REF, scope, id_expression);
	      else if (TYPE_P (scope) && DECL_P (decl))
		return build (SCOPE_REF, TREE_TYPE (decl), scope,
			      id_expression);
	      else
		return decl;
	    }
	  /* A TEMPLATE_ID already contains all the information we
	     need.  */
	  if (TREE_CODE (id_expression) == TEMPLATE_ID_EXPR)
	    return id_expression;
	  /* Since this name was dependent, the expression isn't
	     constant -- yet.  No error is issued because it might be
	     constant when things are instantiated.  */
	  if (integral_constant_expression_p)
	    *non_integral_constant_expression_p = true;
	  *idk = CP_ID_KIND_UNQUALIFIED_DEPENDENT;
	  /* If we found a variable, then name lookup during the
	     instantiation will always resolve to the same VAR_DECL
	     (or an instantiation thereof).  */
	  if (TREE_CODE (decl) == VAR_DECL
	      || TREE_CODE (decl) == PARM_DECL)
	    return decl;
	  return id_expression;
	}

      /* Only certain kinds of names are allowed in constant
       expression.  Enumerators and template parameters 
       have already been handled above.  */
      if (integral_constant_expression_p)
	{
	    /* Const variables or static data members of integral or
	      enumeration types initialized with constant expressions
	      are OK.  */
	  if (TREE_CODE (decl) == VAR_DECL
	      && CP_TYPE_CONST_P (TREE_TYPE (decl))
	      && INTEGRAL_OR_ENUMERATION_TYPE_P (TREE_TYPE (decl))
	      && DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl))
	    ;
	  else
	    {
	      if (!allow_non_integral_constant_expression_p)
		{
		  error ("`%D' cannot appear in a constant-expression", decl);
		  return error_mark_node;
		}
	      *non_integral_constant_expression_p = true;
	    }
	}
      
      if (TREE_CODE (decl) == NAMESPACE_DECL)
	{
	  error ("use of namespace `%D' as expression", decl);
	  return error_mark_node;
	}
      else if (DECL_CLASS_TEMPLATE_P (decl))
	{
	  error ("use of class template `%T' as expression", decl);
	  return error_mark_node;
	}
      else if (TREE_CODE (decl) == TREE_LIST)
	{
	  /* Ambiguous reference to base members.  */
	  error ("request for member `%D' is ambiguous in "
		 "multiple inheritance lattice", id_expression);
	  print_candidates (decl);
	  return error_mark_node;
	}

      /* Mark variable-like entities as used.  Functions are similarly
	 marked either below or after overload resolution.  */
      if (TREE_CODE (decl) == VAR_DECL
	  || TREE_CODE (decl) == PARM_DECL
	  || TREE_CODE (decl) == RESULT_DECL)
	mark_used (decl);

      if (scope)
	{
	  decl = (adjust_result_of_qualified_name_lookup 
		  (decl, scope, current_class_type));

	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    mark_used (decl);

	  if (TREE_CODE (decl) == FIELD_DECL || BASELINK_P (decl))
	    *qualifying_class = scope;
	  else if (!processing_template_decl)
	    decl = convert_from_reference (decl);
	  else if (TYPE_P (scope))
	    decl = build (SCOPE_REF, TREE_TYPE (decl), scope, decl);
	}
      else if (TREE_CODE (decl) == FIELD_DECL)
	decl = finish_non_static_data_member (decl, current_class_ref,
					      /*qualifying_scope=*/NULL_TREE);
      else if (is_overloaded_fn (decl))
	{
	  tree first_fn = OVL_CURRENT (decl);

	  if (TREE_CODE (first_fn) == TEMPLATE_DECL)
	    first_fn = DECL_TEMPLATE_RESULT (first_fn);

	  if (!really_overloaded_fn (decl))
	    mark_used (first_fn);

	  if (TREE_CODE (first_fn) == FUNCTION_DECL
	      && DECL_FUNCTION_MEMBER_P (first_fn))
	    {
	      /* A set of member functions.  */
	      decl = maybe_dummy_object (DECL_CONTEXT (first_fn), 0);
	      return finish_class_member_access_expr (decl, id_expression);
	    }
	}
      else
	{
	  if (TREE_CODE (decl) == VAR_DECL
	      || TREE_CODE (decl) == PARM_DECL
	      || TREE_CODE (decl) == RESULT_DECL)
	    {
	      tree context = decl_function_context (decl);
	      
	      if (context != NULL_TREE && context != current_function_decl
		  && ! TREE_STATIC (decl))
		{
		  error ("use of %s from containing function",
			 (TREE_CODE (decl) == VAR_DECL
			  ? "`auto' variable" : "parameter"));
		  cp_error_at ("  `%#D' declared here", decl);
		  return error_mark_node;
		}
	    }
	  
	  if (DECL_P (decl) && DECL_NONLOCAL (decl)
	      && DECL_CLASS_SCOPE_P (decl)
	      && DECL_CONTEXT (decl) != current_class_type)
	    {
	      tree path;
	      
	      path = currently_open_derived_class (DECL_CONTEXT (decl));
	      perform_or_defer_access_check (TYPE_BINFO (path), decl);
	    }
	  
	  if (! processing_template_decl)
	    decl = convert_from_reference (decl);
	}
      
      /* Resolve references to variables of anonymous unions
	 into COMPONENT_REFs.  */
      if (TREE_CODE (decl) == ALIAS_DECL)
	decl = DECL_INITIAL (decl);
    }

  if (TREE_DEPRECATED (decl))
    warn_deprecated_use (decl);

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
      type = make_aggr_type (TYPEOF_TYPE);
      TYPE_FIELDS (type) = expr;

      return type;
    }

  type = TREE_TYPE (expr);

  if (!type || type == unknown_type_node)
    {
      error ("type of `%E' is unknown", expr);
      return error_mark_node;
    }

  return type;
}

/* Generate RTL for the statement T, and its substatements, and any
   other statements at its nesting level.  */

static void
cp_expand_stmt (tree t)
{
  switch (TREE_CODE (t))
    {
    case TRY_BLOCK:
      genrtl_try_block (t);
      break;

    case EH_SPEC_BLOCK:
      genrtl_eh_spec_block (t);
      break;

    case HANDLER:
      genrtl_handler (t);
      break;

    case USING_STMT:
      break;
    
    default:
      abort ();
      break;
    }
}

/* Called from expand_body via walk_tree.  Replace all AGGR_INIT_EXPRs
   will equivalent CALL_EXPRs.  */

static tree
simplify_aggr_init_exprs_r (tree* tp, 
                            int* walk_subtrees,
                            void* data ATTRIBUTE_UNUSED)
{
  /* We don't need to walk into types; there's nothing in a type that
     needs simplification.  (And, furthermore, there are places we
     actively don't want to go.  For example, we don't want to wander
     into the default arguments for a FUNCTION_DECL that appears in a
     CALL_EXPR.)  */
  if (TYPE_P (*tp))
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }
  /* Only AGGR_INIT_EXPRs are interesting.  */
  else if (TREE_CODE (*tp) != AGGR_INIT_EXPR)
    return NULL_TREE;

  simplify_aggr_init_expr (tp);

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Replace the AGGR_INIT_EXPR at *TP with an equivalent CALL_EXPR.  This
   function is broken out from the above for the benefit of the tree-ssa
   project.  */

void
simplify_aggr_init_expr (tree *tp)
{
  tree aggr_init_expr = *tp;

  /* Form an appropriate CALL_EXPR.  */
  tree fn = TREE_OPERAND (aggr_init_expr, 0);
  tree args = TREE_OPERAND (aggr_init_expr, 1);
  tree slot = TREE_OPERAND (aggr_init_expr, 2);
  tree type = TREE_TYPE (aggr_init_expr);

  tree call_expr;
  enum style_t { ctor, arg, pcc } style;

  if (AGGR_INIT_VIA_CTOR_P (aggr_init_expr))
    style = ctor;
#ifdef PCC_STATIC_STRUCT_RETURN
  else if (1)
    style = pcc;
#endif
  else if (TREE_ADDRESSABLE (type))
    style = arg;
  else
    /* We shouldn't build an AGGR_INIT_EXPR if we don't need any special
       handling.  See build_cplus_new.  */
    abort ();

  if (style == ctor || style == arg)
    {
      /* Pass the address of the slot.  If this is a constructor, we
	 replace the first argument; otherwise, we tack on a new one.  */
      tree addr;

      if (style == ctor)
	args = TREE_CHAIN (args);

      cxx_mark_addressable (slot);
      addr = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (slot)), slot);
      if (style == arg)
	{
	  /* The return type might have different cv-quals from the slot.  */
	  tree fntype = TREE_TYPE (TREE_TYPE (fn));
#ifdef ENABLE_CHECKING
	  if (TREE_CODE (fntype) != FUNCTION_TYPE
	      && TREE_CODE (fntype) != METHOD_TYPE)
	    abort ();
#endif
	  addr = convert (build_pointer_type (TREE_TYPE (fntype)), addr);
	}

      args = tree_cons (NULL_TREE, addr, args);
    }

  call_expr = build (CALL_EXPR, 
		     TREE_TYPE (TREE_TYPE (TREE_TYPE (fn))),
		     fn, args, NULL_TREE);

  if (style == arg)
    /* Tell the backend that we've added our return slot to the argument
       list.  */
    CALL_EXPR_HAS_RETURN_SLOT_ADDR (call_expr) = 1;
  else if (style == pcc)
    {
      /* If we're using the non-reentrant PCC calling convention, then we
	 need to copy the returned value out of the static buffer into the
	 SLOT.  */
      push_deferring_access_checks (dk_no_check);
      call_expr = build_aggr_init (slot, call_expr,
				   DIRECT_BIND | LOOKUP_ONLYCONVERTING);
      pop_deferring_access_checks ();
    }

  /* We want to use the value of the initialized location as the
     result.  */
  call_expr = build (COMPOUND_EXPR, type,
		     call_expr, slot);

  /* Replace the AGGR_INIT_EXPR with the CALL_EXPR.  */
  TREE_CHAIN (call_expr) = TREE_CHAIN (aggr_init_expr);
  *tp = call_expr;
}

/* Emit all thunks to FN that should be emitted when FN is emitted.  */

static void
emit_associated_thunks (tree fn)
{
  /* When we use vcall offsets, we emit thunks with the virtual
     functions to which they thunk. The whole point of vcall offsets
     is so that you can know statically the entire set of thunks that
     will ever be needed for a given virtual function, thereby
     enabling you to output all the thunks with the function itself.  */
  if (DECL_VIRTUAL_P (fn))
    {
      tree thunk;
      
      for (thunk = DECL_THUNKS (fn); thunk; thunk = TREE_CHAIN (thunk))
	{
	  if (!THUNK_ALIAS (thunk))
	    {
	      use_thunk (thunk, /*emit_p=*/1);
	      if (DECL_RESULT_THUNK_P (thunk))
		{
		  tree probe;
		  
		  for (probe = DECL_THUNKS (thunk);
		       probe; probe = TREE_CHAIN (probe))
		    use_thunk (probe, /*emit_p=*/1);
		}
	    }
	  else
	    my_friendly_assert (!DECL_THUNKS (thunk), 20031023);
	}
    }
}

/* Generate RTL for FN.  */

void
expand_body (tree fn)
{
  tree saved_function;
  
  /* Compute the appropriate object-file linkage for inline
     functions.  */
  if (DECL_DECLARED_INLINE_P (fn))
    import_export_decl (fn);

  /* If FN is external, then there's no point in generating RTL for
     it.  This situation can arise with an inline function under
     `-fexternal-templates'; we instantiate the function, even though
     we're not planning on emitting it, in case we get a chance to
     inline it.  */
  if (DECL_EXTERNAL (fn))
    return;

  /* ??? When is this needed?  */
  saved_function = current_function_decl;

  /* Emit any thunks that should be emitted at the same time as FN.  */
  emit_associated_thunks (fn);

  timevar_push (TV_INTEGRATION);
  optimize_function (fn);
  timevar_pop (TV_INTEGRATION);

  tree_rest_of_compilation (fn, function_depth > 1);

  current_function_decl = saved_function;

  extract_interface_info ();

  /* If this function is marked with the constructor attribute, add it
     to the list of functions to be called along with constructors
     from static duration objects.  */
  if (DECL_STATIC_CONSTRUCTOR (fn))
    static_ctors = tree_cons (NULL_TREE, fn, static_ctors);

  /* If this function is marked with the destructor attribute, add it
     to the list of functions to be called along with destructors from
     static duration objects.  */
  if (DECL_STATIC_DESTRUCTOR (fn))
    static_dtors = tree_cons (NULL_TREE, fn, static_dtors);

  if (DECL_CLONED_FUNCTION_P (fn))
    {
      /* If this is a clone, go through the other clones now and mark
         their parameters used.  We have to do that here, as we don't
         know whether any particular clone will be expanded, and
         therefore cannot pick one arbitrarily.  */ 
      tree probe;

      for (probe = TREE_CHAIN (DECL_CLONED_FUNCTION (fn));
	   probe && DECL_CLONED_FUNCTION_P (probe);
	   probe = TREE_CHAIN (probe))
	{
	  tree parms;

	  for (parms = DECL_ARGUMENTS (probe);
	       parms; parms = TREE_CHAIN (parms))
	    TREE_USED (parms) = 1;
	}
    }
}

/* Generate RTL for FN.  */

void
expand_or_defer_fn (tree fn)
{
  /* When the parser calls us after finishing the body of a template
     function, we don't really want to expand the body.  */
  if (processing_template_decl)
    {
      /* Normally, collection only occurs in rest_of_compilation.  So,
	 if we don't collect here, we never collect junk generated
	 during the processing of templates until we hit a
	 non-template function.  */
      ggc_collect ();
      return;
    }

  /* Replace AGGR_INIT_EXPRs with appropriate CALL_EXPRs.  */
  walk_tree_without_duplicates (&DECL_SAVED_TREE (fn),
				simplify_aggr_init_exprs_r,
				NULL);

  /* If this is a constructor or destructor body, we have to clone
     it.  */
  if (maybe_clone_body (fn))
    {
      /* We don't want to process FN again, so pretend we've written
	 it out, even though we haven't.  */
      TREE_ASM_WRITTEN (fn) = 1;
      return;
    }

  /* There's no reason to do any of the work here if we're only doing
     semantic analysis; this code just generates RTL.  */
  if (flag_syntax_only)
    return;

  /* Compute the appropriate object-file linkage for inline functions.  */
  if (DECL_DECLARED_INLINE_P (fn))
    import_export_decl (fn);

  function_depth++;

  /* Expand or defer, at the whim of the compilation unit manager.  */
  cgraph_finalize_function (fn, function_depth > 1);

  function_depth--;
}

/* Helper function for walk_tree, used by finish_function to override all
   the RETURN_STMTs and pertinent CLEANUP_STMTs for the named return
   value optimization.  */

tree
nullify_returns_r (tree* tp, int* walk_subtrees, void* data)
{
  tree nrv = (tree) data;

  /* No need to walk into types.  There wouldn't be any need to walk into
     non-statements, except that we have to consider STMT_EXPRs.  */
  if (TYPE_P (*tp))
    *walk_subtrees = 0;
  else if (TREE_CODE (*tp) == RETURN_STMT)
    RETURN_STMT_EXPR (*tp) = NULL_TREE;
  else if (TREE_CODE (*tp) == CLEANUP_STMT
	   && CLEANUP_DECL (*tp) == nrv)
    CLEANUP_EH_ONLY (*tp) = 1;

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Start generating the RTL for FN.  */

void
cxx_expand_function_start (void)
{
  /* Give our named return value the same RTL as our RESULT_DECL.  */
  if (current_function_return_value)
    COPY_DECL_RTL (DECL_RESULT (cfun->decl), current_function_return_value);
}

/* Perform initialization related to this module.  */

void
init_cp_semantics (void)
{
  lang_expand_stmt = cp_expand_stmt;
}

#include "gt-cp-semantics.h"
