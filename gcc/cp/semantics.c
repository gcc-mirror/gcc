/* Perform the semantic phase of parsing, i.e., the process of
   building tree structure, checking semantic consistency, and
   building RTL.  These routines are used both during actual parsing
   and during the instantiation of template functions. 

   Copyright (C) 1998, 1999 Free Software Foundation, Inc.
   Written by Mark Mitchell (mmitchell@usa.net) based on code found
   formerly in parse.y and pt.c.  

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   GNU CC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "except.h"
#include "lex.h"
#include "toplev.h"

/* There routines provide a modular interface to perform many parsing
   operations.  They may therefore be used during actual parsing, or
   during template instantiation, which may be regarded as a
   degenerate form of parsing.  Since the current g++ parser is
   lacking in several respects, and will be reimplemented, we are
   attempting to move most code that is not directly related to
   parsing into this file; that will make implementing the new parser
   much easier since it will be able to make use of these routines.  */

static void expand_stmts PROTO((tree));
static void do_pushlevel PROTO((void));
static tree do_poplevel PROTO((void));

/* Non-zero if we should generate RTL for functions that we process.
   When this is zero, we just accumulate tree structure, without
   interacting with the back end.  */
int expanding_p = 1;

/* Non-zero if we should treat statements as full expressions.  In
   particular, this variable is no-zero if at the end of a statement
   we should destroy any temporaries created during that statement.
   Similarly, if, at the end of a block, we should destroy any local
   variables in this block.  Normally, this variable is non-zero,
   since those are the normal semantics of C++.

   However, in order to represent aggregate initialization code as
   tree structure, we use statement-expressions.  The statements
   within the statement expression should not result in cleanups being
   run until the entire enclosing statement is complete.  */
int stmts_are_full_exprs_p = 1;

/* The type of the last expression-statement we have seen.  This is
   required because the type of a statement-expression is the type of
   the last expression statement.  */
tree last_expr_type;

/* When parsing a template, LAST_TREE contains the last statement
   parsed.  These are chained together through the TREE_CHAIN field,
   but often need to be re-organized since the parse is performed
   bottom-up.  This macro makes LAST_TREE the indicated SUBSTMT of
   STMT.  */

#define RECHAIN_STMTS(stmt, substmt, last)	\
  do {						\
    substmt = last;			        \
    TREE_CHAIN (stmt) = NULL_TREE;		\
    last_tree = stmt;				\
  } while (0)

#define RECHAIN_STMTS_FROM_LAST(stmt, substmt)	\
  RECHAIN_STMTS (stmt, substmt, last_tree)

#define RECHAIN_STMTS_FROM_CHAIN(stmt, substmt)	\
  RECHAIN_STMTS (stmt, substmt, TREE_CHAIN (stmt))

/* Finish an expression-statement, whose EXPRESSION is as indicated.  */

void 
finish_expr_stmt (expr)
     tree expr;
{
  if (expr != NULL_TREE)
    {
      if (building_stmt_tree ())
	add_tree (build_min_nt (EXPR_STMT, expr));
      else
	{
	  emit_line_note (input_filename, lineno);
	  /* Do default conversion if safe and possibly important,
	     in case within ({...}).  */
	  if ((TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE
	       && lvalue_p (expr))
	      || TREE_CODE (TREE_TYPE (expr)) == FUNCTION_TYPE)
	    expr = default_conversion (expr);

	  if (stmts_are_full_exprs_p)
	    expand_start_target_temps ();
	    
	  cplus_expand_expr_stmt (expr);

	  if (stmts_are_full_exprs_p)
	    {
	      expand_end_target_temps ();
	      clear_momentary ();
	    }
	}
    }

  finish_stmt ();

  /* This was an expression-statement, so we save the type of the
     expression.  */
  last_expr_type = expr ? TREE_TYPE (expr) : NULL_TREE;
}

/* Begin an if-statement.  Returns a newly created IF_STMT if
   appropriate.  */

tree
begin_if_stmt ()
{
  tree r;

  if (building_stmt_tree ())
    {
      r = build_min_nt (IF_STMT, NULL_TREE, NULL_TREE, NULL_TREE);
      add_tree (r);
    }
  else
    r = NULL_TREE;

  do_pushlevel ();

  return r;
}

/* Process the COND of an if-statement, which may be given by
   IF_STMT.  */

void 
finish_if_stmt_cond (cond, if_stmt)
     tree cond;
     tree if_stmt;
{
  if (building_stmt_tree ())
    {
      if (last_tree != if_stmt)
	RECHAIN_STMTS_FROM_LAST (if_stmt, IF_COND (if_stmt));
      else
	IF_COND (if_stmt) = copy_to_permanent (cond);
    }
  else
    {
      emit_line_note (input_filename, lineno);
      expand_start_cond (condition_conversion (cond), 0);
    }
}

/* Finish the then-clause of an if-statement, which may be given by
   IF_STMT.  */

tree
finish_then_clause (if_stmt)
     tree if_stmt;
{
  if (building_stmt_tree ())
    {
      RECHAIN_STMTS_FROM_CHAIN (if_stmt, 
				THEN_CLAUSE (if_stmt));
      last_tree = if_stmt;
      return if_stmt;
    }
  else
    return NULL_TREE;
}

/* Begin the else-clause of an if-statement.  */

void 
begin_else_clause ()
{
  if (!building_stmt_tree ())
    expand_start_else ();
}

/* Finish the else-clause of an if-statement, which may be given by
   IF_STMT.  */

void
finish_else_clause (if_stmt)
     tree if_stmt;
{
  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_CHAIN (if_stmt, ELSE_CLAUSE (if_stmt));
}

/* Finsh an if-statement.  */

void 
finish_if_stmt ()
{
  if (!building_stmt_tree ())
    expand_end_cond ();

  do_poplevel ();
  finish_stmt ();
}

/* Begin a while-statement.  Returns a newly created WHILE_STMT if
   appropriate.  */

tree
begin_while_stmt ()
{
  tree r;

  if (building_stmt_tree ())
    {
      r = build_min_nt (WHILE_STMT, NULL_TREE, NULL_TREE);
      add_tree (r);
    }
  else
    {
      emit_nop ();
      emit_line_note (input_filename, lineno);
      expand_start_loop (1); 
      r = NULL_TREE;
    }

  do_pushlevel ();

  return r;
}

/* Process the COND of an if-statement, which may be given by
   WHILE_STMT.  */

void 
finish_while_stmt_cond (cond, while_stmt)
     tree cond;
     tree while_stmt;
{
  if (building_stmt_tree ())
    {
      if (last_tree != while_stmt)
	RECHAIN_STMTS_FROM_LAST (while_stmt, WHILE_COND (while_stmt)); 
      else
	TREE_OPERAND (while_stmt, 0) = copy_to_permanent (cond);
    }
  else
    {
      emit_line_note (input_filename, lineno);
      expand_exit_loop_if_false (0, condition_conversion (cond));
    }

  /* If COND wasn't a declaration, clear out the
     block we made for it and start a new one here so the
     optimization in expand_end_loop will work.  */
  if (getdecls () == NULL_TREE)
    {
      do_poplevel ();
      do_pushlevel ();
    }
}

/* Finish a while-statement, which may be given by WHILE_STMT.  */

void 
finish_while_stmt (while_stmt)
     tree while_stmt;
{
  do_poplevel ();

  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_CHAIN (while_stmt, WHILE_BODY (while_stmt));
  else
    expand_end_loop ();
  finish_stmt ();
}

/* Begin a do-statement.  Returns a newly created DO_STMT if
   appropriate.  */

tree
begin_do_stmt ()
{
  if (building_stmt_tree ())
    {
      tree r = build_min_nt (DO_STMT, NULL_TREE, NULL_TREE);
      add_tree (r);
      return r;
    }
  else
    {
      emit_nop ();
      emit_line_note (input_filename, lineno);
      expand_start_loop_continue_elsewhere (1);
      return NULL_TREE;
    }
}

/* Finish the body of a do-statement, which may be given by DO_STMT.  */

void
finish_do_body (do_stmt)
     tree do_stmt;
{
  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_CHAIN (do_stmt, DO_BODY (do_stmt));
  else
    expand_loop_continue_here ();
}

/* Finish a do-statement, which may be given by DO_STMT, and whose
   COND is as indicated.  */

void
finish_do_stmt (cond, do_stmt)
     tree cond;
     tree do_stmt;
{
  if (building_stmt_tree ())
    DO_COND (do_stmt) = copy_to_permanent (cond);
  else
    {
      emit_line_note (input_filename, lineno);
      expand_exit_loop_if_false (0, condition_conversion (cond));
      expand_end_loop ();
    }

  clear_momentary ();
  finish_stmt ();
}

/* Finish a return-statement.  The EXPRESSION returned, if any, is as
   indicated.  */

void
finish_return_stmt (expr)
     tree expr;
{
  if (building_stmt_tree ())
    add_tree (build_min_nt (RETURN_STMT, expr));
  else
    {
      emit_line_note (input_filename, lineno);
      c_expand_return (expr);
    }

  finish_stmt ();
}

/* Begin a for-statement.  Returns a new FOR_STMT if appropriate.  */

tree
begin_for_stmt ()
{
  tree r;

  if (building_stmt_tree ())
    {
      r = build_min_nt (FOR_STMT, NULL_TREE, NULL_TREE, 
			NULL_TREE, NULL_TREE);
      add_tree (r);
    }
  else
    r = NULL_TREE;

  if (flag_new_for_scope > 0)
    {
      do_pushlevel ();
      note_level_for_for ();
    }

  return r;
}

/* Finish the for-init-statement of a for-statement, which may be
   given by FOR_STMT.  */

void
finish_for_init_stmt (for_stmt)
     tree for_stmt;
{
  if (building_stmt_tree ())
    {
      if (last_tree != for_stmt)
	RECHAIN_STMTS_FROM_CHAIN (for_stmt, FOR_INIT_STMT (for_stmt));
    }
  else
    {
      emit_nop ();
      emit_line_note (input_filename, lineno);
      expand_start_loop_continue_elsewhere (1); 
    }

  do_pushlevel ();
}

/* Finish the COND of a for-statement, which may be given by
   FOR_STMT.  */

void
finish_for_cond (cond, for_stmt)
     tree cond;
     tree for_stmt;
{
  if (building_stmt_tree ())
    {
      if (last_tree != for_stmt)
	RECHAIN_STMTS_FROM_LAST (for_stmt, FOR_COND (for_stmt));
      else
	FOR_COND (for_stmt) = copy_to_permanent (cond);
    }
  else
    {
      emit_line_note (input_filename, lineno);
      if (cond)
	expand_exit_loop_if_false (0, condition_conversion (cond));
    }
  
  /* If the cond wasn't a declaration, clear out the
     block we made for it and start a new one here so the
     optimization in expand_end_loop will work.  */
  if (getdecls () == NULL_TREE)
    {
      do_poplevel ();
      do_pushlevel ();
    }  
}

/* Finish the increment-EXPRESSION in a for-statement, which may be
   given by FOR_STMT.  */

void
finish_for_expr (expr, for_stmt)
     tree expr;
     tree for_stmt;
{
  if (building_stmt_tree ())
    FOR_EXPR (for_stmt) = copy_to_permanent (expr);

  /* Don't let the tree nodes for EXPR be discarded
     by clear_momentary during the parsing of the next stmt.  */
  push_momentary ();
}

/* Finish the body of a for-statement, which may be given by
   FOR_STMT.  The increment-EXPR for the loop must be
   provided.  */

void
finish_for_stmt (expr, for_stmt)
     tree expr;
     tree for_stmt;
{
  /* Pop the scope for the body of the loop.  */
  do_poplevel ();

  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_CHAIN (for_stmt, FOR_BODY (for_stmt));
  else
    {
      emit_line_note (input_filename, lineno);
      expand_loop_continue_here ();
      if (expr) 
	finish_expr_stmt (expr);
      expand_end_loop ();
    }

  pop_momentary ();

  if (flag_new_for_scope > 0)
    do_poplevel ();

  finish_stmt (); 
}

/* Finish a break-statement.  */

void
finish_break_stmt ()
{
  emit_line_note (input_filename, lineno);
  if (building_stmt_tree ())
    add_tree (build_min_nt (BREAK_STMT));
  else if ( ! expand_exit_something ())
    cp_error ("break statement not within loop or switch");
}

/* Finish a continue-statement.  */

void
finish_continue_stmt ()
{
  emit_line_note (input_filename, lineno);
  if (building_stmt_tree ())
    add_tree (build_min_nt (CONTINUE_STMT));
  else if (! expand_continue_loop (0))
    cp_error ("continue statement not within a loop");   
}

/* Begin a switch-statement.  */

void
begin_switch_stmt ()
{
  do_pushlevel ();
}

/* Finish the cond of a switch-statement.  Returns a new
   SWITCH_STMT if appropriate.  */ 

tree
finish_switch_cond (cond)
     tree cond;
{
  tree r;

  if (building_stmt_tree ())
    {
      r = build_min_nt (SWITCH_STMT, cond, NULL_TREE);
      add_tree (r);
    }
  else if (cond != error_mark_node)
    {
      emit_line_note (input_filename, lineno);
      c_expand_start_case (cond);
      r = NULL_TREE;
    }
  else
    {
      /* The code is in error, but we don't want expand_end_case to
         crash. */
      c_expand_start_case (boolean_false_node);
      r = NULL_TREE;
    }

  push_switch ();

  /* Don't let the tree nodes for COND be discarded by
     clear_momentary during the parsing of the next stmt.  */
  push_momentary ();

  return r;
}

/* Finish the body of a switch-statement, which may be given by
   SWITCH_STMT.  The COND to switch on is indicated.  */

void
finish_switch_stmt (cond, switch_stmt)
     tree cond;
     tree switch_stmt;
{
  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_CHAIN (switch_stmt, SWITCH_BODY (switch_stmt));
  else
    expand_end_case (cond);
  pop_momentary ();
  pop_switch (); 
  do_poplevel ();
  finish_stmt ();
}

/* Finish a case-label.  */

void 
finish_case_label (low_value, high_value)
     tree low_value;
     tree high_value;
{
  if (building_stmt_tree ())
    {
      add_tree (build_min_nt (CASE_LABEL, low_value, high_value));
      return;
    }

  do_case (low_value, high_value);
}

/* Finish a goto-statement.  */

void
finish_goto_stmt (destination)
     tree destination;
{
  if (TREE_CODE (destination) == IDENTIFIER_NODE)
    destination = lookup_label (destination);

  if (building_stmt_tree ())
    add_tree (build_min_nt (GOTO_STMT, destination));
  else
    {
      emit_line_note (input_filename, lineno);

      if (TREE_CODE (destination) == LABEL_DECL)
	{
	  TREE_USED (destination) = 1;
	  expand_goto (destination); 
	}
      else
	expand_computed_goto (destination);
    }
}

/* Begin a try-block.  Returns a newly-created TRY_BLOCK if
   appropriate.  */

tree
begin_try_block ()
{
  if (building_stmt_tree ())
    {
      tree r = build_min_nt (TRY_BLOCK, NULL_TREE,
			     NULL_TREE);
      add_tree (r);
      return r;
    }
  else
    {
      emit_line_note (input_filename, lineno);
      expand_start_try_stmts ();
      return NULL_TREE;
    }
}

/* Likewise, for a function-try-block.  */

tree
begin_function_try_block ()
{
  if (building_stmt_tree ())
    {
      tree r = build_min_nt (TRY_BLOCK, NULL_TREE,
			     NULL_TREE);
      add_tree (r);
      return r;
    }
  else
    {
      if (! current_function_parms_stored)
	store_parm_decls ();
      expand_start_early_try_stmts ();
      return NULL_TREE;
    }
}

/* Finish a try-block, which may be given by TRY_BLOCK.  */

void
finish_try_block (try_block)
     tree try_block;
{
  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_LAST (try_block, TRY_STMTS (try_block));
  else
    expand_start_all_catch ();  
}

/* Finish an implicitly generated try-block, with a cleanup is given
   by CLEANUP.  */

void
finish_cleanup (cleanup, try_block)
     tree cleanup;
     tree try_block;
{
  if (building_stmt_tree ()) 
    {
      TRY_HANDLERS (try_block) = copy_to_permanent (cleanup);
      CLEANUP_P (try_block) = 1;
    }
  else
    expand_eh_region_end (protect_with_terminate (cleanup));
}

/* Likewise, for a function-try-block.  */

void
finish_function_try_block (try_block)
     tree try_block; 
{
  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_LAST (try_block, TRY_STMTS (try_block));
  else
    {
      end_protect_partials ();
      expand_start_all_catch ();
      in_function_try_handler = 1;
    }
}

/* Finish a handler-sequence for a try-block, which may be given by
   TRY_BLOCK.  */

void
finish_handler_sequence (try_block)
     tree try_block;
{
  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_CHAIN (try_block, TRY_HANDLERS (try_block));
  else
    expand_end_all_catch ();
}

/* Likewise, for a function-try-block.  */

void
finish_function_handler_sequence (try_block)
     tree try_block;
{
  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_CHAIN (try_block, TRY_HANDLERS (try_block));
  else
    {
      in_function_try_handler = 0;
      expand_end_all_catch ();
    }
}

/* Begin a handler.  Returns a HANDLER if appropriate.  */

tree
begin_handler ()
{
  tree r;

  if (building_stmt_tree ())
    {
      r = build_min_nt (HANDLER, NULL_TREE, NULL_TREE);
      add_tree (r);
    }
  else
    r = NULL_TREE;

  do_pushlevel ();

  return r;
}

/* Finish the handler-parameters for a handler, which may be given by
   HANDLER.  */

void
finish_handler_parms (handler)
     tree handler;
{
  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_CHAIN (handler, HANDLER_PARMS (handler));
}

/* Finish a handler, which may be given by HANDLER.  */

void
finish_handler (handler)
     tree handler;
{
  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_CHAIN (handler, HANDLER_BODY (handler));
  else
    expand_end_catch_block ();

  do_poplevel ();
}

/* Begin a compound-statement.  If HAS_NO_SCOPE is non-zero, the
   compound-statement does not define a scope.  Returns a new
   COMPOUND_STMT if appropriate.  */

tree
begin_compound_stmt (has_no_scope)
     int has_no_scope;
{
  tree r; 

  if (building_stmt_tree ())
    {
      r = build_min_nt (COMPOUND_STMT, NULL_TREE);
      add_tree (r);
      if (has_no_scope)
	COMPOUND_STMT_NO_SCOPE (r) = 1;
    }
  else
    r = NULL_TREE;

  last_expr_type = NULL_TREE;

  if (!has_no_scope)
    do_pushlevel ();
  else
    /* Normally, we try hard to keep the BLOCK for a
       statement-expression.  But, if it's a statement-expression with
       a scopeless block, there's nothing to keep, and we don't want
       to accidentally keep a block *inside* the scopeless block.  */ 
    keep_next_level (0);

  return r;
}


/* Finish a compound-statement, which may be given by COMPOUND_STMT.
   If HAS_NO_SCOPE is non-zero, the compound statement does not define
   a scope.  */

tree
finish_compound_stmt (has_no_scope, compound_stmt)
     int has_no_scope;
     tree compound_stmt;
{
  tree r;
  tree t;

  if (!has_no_scope)
    r = do_poplevel ();
  else
    r = NULL_TREE;

  if (building_stmt_tree ())
    RECHAIN_STMTS_FROM_CHAIN (compound_stmt, 
			      COMPOUND_BODY (compound_stmt));

  /* When we call finish_stmt we will lost LAST_EXPR_TYPE.  But, since
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

void
finish_asm_stmt (cv_qualifier, string, output_operands,
		 input_operands, clobbers)
     tree cv_qualifier;
     tree string;
     tree output_operands;
     tree input_operands;
     tree clobbers;
{
  if (TREE_CHAIN (string))
    {
      if (building_stmt_tree ())
	/* We need to build the combined string on the permanent
	   obstack so that we can use it during instantiations.  */
	push_permanent_obstack ();

      string = combine_strings (string);

      if (building_stmt_tree ())
	pop_obstacks ();
    }

  if (cv_qualifier != NULL_TREE
      && cv_qualifier != ridpointers[(int) RID_VOLATILE])
    {
      cp_warning ("%s qualifier ignored on asm",
		  IDENTIFIER_POINTER (cv_qualifier));
      cv_qualifier = NULL_TREE;
    }

  if (building_stmt_tree ())
    {
      tree r = build_min_nt (ASM_STMT, cv_qualifier, string,
			     output_operands, input_operands,
			     clobbers);
      add_tree (r);
    }
  else
    {
      emit_line_note (input_filename, lineno);
      if (output_operands != NULL_TREE || input_operands != NULL_TREE
	    || clobbers != NULL_TREE)
	{
	  tree t;

	  for (t = input_operands; t; t = TREE_CHAIN (t))
	    TREE_VALUE (t) = decay_conversion (TREE_VALUE (t));

	  c_expand_asm_operands (string, output_operands,
				 input_operands, 
				 clobbers,
				 cv_qualifier != NULL_TREE,
				 input_filename, lineno);
	}
      else
	expand_asm (string);
      
      finish_stmt ();
    }
}

/* Finish a label with the indicated NAME.  */

void
finish_label_stmt (name)
     tree name;
{
  tree decl = define_label (input_filename, lineno, name);

  if (building_stmt_tree ())
    add_tree (build_min_nt (LABEL_STMT, decl));
  else if (decl)
    expand_label (decl);
}

/* Finish a series of declarations for local labels.  G++ allows users
   to declare "local" labels, i.e., labels with scope.  This extension
   is useful when writing code involving statement-expressions.  */

void
finish_label_decl (name)
     tree name;
{
  tree decl = declare_local_label (name);
  if (building_stmt_tree ())
    add_decl_stmt (decl);
}

/* Create a declaration statement for the declaration given by the
   DECL.  */

void
add_decl_stmt (decl)
     tree decl;
{
  tree decl_stmt;

  /* We need the type to last until instantiation time.  */
  TREE_TYPE (decl) = copy_to_permanent (TREE_TYPE (decl));
  decl_stmt = build_min_nt (DECL_STMT, decl);
  add_tree (decl_stmt);
}

/* We're in a constructor, and have just constructed a a subobject of
   *THIS.  CLEANUP is code to run if an exception is thrown before the
   end of the current function is reached.   */

void 
finish_subobject (cleanup)
     tree cleanup;
{
  if (building_stmt_tree ())
    {
      tree r = build_min_nt (SUBOBJECT, cleanup);
      add_tree (r);
    }
  else
    add_partial_entry (cleanup);
}

/* Bind a name and initialization to the return value of
   the current function.  */

void
finish_named_return_value (return_id, init)
     tree return_id, init;
{
  tree decl = DECL_RESULT (current_function_decl);

  if (pedantic)
    /* Give this error as many times as there are occurrences,
       so that users can use Emacs compilation buffers to find
       and fix all such places.  */
    pedwarn ("ANSI C++ does not permit named return values");

  if (return_id != NULL_TREE)
    {
      if (DECL_NAME (decl) == NULL_TREE)
	{
	  DECL_NAME (decl) = return_id;
	  DECL_ASSEMBLER_NAME (decl) = return_id;
	}
      else
	{
	  cp_error ("return identifier `%D' already in place", return_id);
	  return;
	}
    }

  /* Can't let this happen for constructors.  */
  if (DECL_CONSTRUCTOR_P (current_function_decl))
    {
      error ("can't redefine default return value for constructors");
      return;
    }

  /* If we have a named return value, put that in our scope as well.  */
  if (DECL_NAME (decl) != NULL_TREE)
    {
      /* Let `cp_finish_decl' know that this initializer is ok.  */
      DECL_INITIAL (decl) = init;
      pushdecl (decl);

      if (building_stmt_tree ())
	add_tree (build_min_nt (RETURN_INIT, return_id,
				copy_to_permanent (init)));
      else
	{
	  cp_finish_decl (decl, init, NULL_TREE, 0, 0);
	  store_return_init (decl);
	}
    }
}

/* Cache the value of this class's main virtual function table pointer
   in a register variable.  This will save one indirection if a
   more than one virtual function call is made this function.  */

void
setup_vtbl_ptr ()
{
  extern tree base_init_expr;

  if (base_init_expr == 0
      && DECL_CONSTRUCTOR_P (current_function_decl))
    {
      if (building_stmt_tree ())
	add_tree (build_min_nt
		  (CTOR_INITIALIZER,
		   current_member_init_list, current_base_init_list));
      else
	emit_base_init (current_class_type);
    }

  /* Always keep the BLOCK node associated with the outermost pair of
     curley braces of a function.  These are needed for correct
     operation of dwarfout.c.  */
  keep_next_level (1);
}

/* Begin a new scope.  */

static void
do_pushlevel ()
{
  if (!building_stmt_tree ())
    {
      emit_line_note (input_filename, lineno);
      clear_last_expr ();
    }
  push_momentary ();
  if (stmts_are_full_exprs_p)
    pushlevel (0);
  if (!building_stmt_tree () && stmts_are_full_exprs_p)
    expand_start_bindings (0);
}

/* Finish a scope.  */

static tree
do_poplevel ()
{
  tree t;

  if (!building_stmt_tree () && stmts_are_full_exprs_p)
    expand_end_bindings (getdecls (), kept_level_p (), 0);
  if (stmts_are_full_exprs_p)
    t = poplevel (kept_level_p (), 1, 0);
  else
    t = NULL_TREE;
  pop_momentary ();
  return t;
}

/* Finish a parenthesized expression EXPR.  */

tree
finish_parenthesized_expr (expr)
     tree expr;
{
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (TREE_CODE (expr))))
    /* This inhibits warnings in truthvalue_conversion.  */
    C_SET_EXP_ORIGINAL_CODE (expr, ERROR_MARK); 

  return expr;
}

/* Begin a statement-expression.  The value returned must be passed to
   finish_stmt_expr.  */

tree 
begin_stmt_expr ()
{
  keep_next_level (1);
  /* If we're building a statement tree, then the upcoming compound
     statement will be chained onto the tree structure, starting at
     last_tree.  We return last_tree so that we can later unhook the
     compound statement.  */
  return building_stmt_tree () ? last_tree : expand_start_stmt_expr(); 
}

/* Finish a statement-expression.  RTL_EXPR should be the value
   returned by the previous begin_stmt_expr; EXPR is the
   statement-expression.  Returns an expression representing the
   statement-expression.  */

tree 
finish_stmt_expr (rtl_expr, expr)
     tree rtl_expr;
     tree expr;
{
  tree result;

  if (!building_stmt_tree ())
    {
      rtl_expr = expand_end_stmt_expr (rtl_expr);
      /* The statements have side effects, so the group does.  */
      TREE_SIDE_EFFECTS (rtl_expr) = 1;
    }

  if (building_stmt_tree ())
    {
      /* If the last thing in the statement-expression was not an
	 expression-statement, then it has type `void'.  */
      if (!last_expr_type)
	last_expr_type = void_type_node;
      result = build_min (STMT_EXPR, last_expr_type, last_tree);
      TREE_SIDE_EFFECTS (result) = 1;
      
      /* Remove the compound statement from the tree structure; it is
	 now saved in the STMT_EXPR.  */
      last_tree = rtl_expr;
      TREE_CHAIN (last_tree) = NULL_TREE;
    }
  else if (expr && TREE_CODE (expr) == BLOCK)
    {
      result = build (BIND_EXPR, TREE_TYPE (rtl_expr),
		      NULL_TREE, rtl_expr, expr);
      delete_block (expr);
    }
  else
    result = rtl_expr;

  if (expr && TREE_CODE (expr) == BLOCK)
    /* Remove the block from the tree at this point.  It gets put back
       at the proper place when the STMT_EXPR or BIND_EXPR is
       expanded.  */
    delete_block (expr);

  return result;
}

/* Finish a call to FN with ARGS.  Returns a representation of the
   call.  */

tree 
finish_call_expr (fn, args, koenig)
     tree fn;
     tree args;
     int koenig;
{
  tree result;

  if (koenig)
    {
      if (TREE_CODE (fn) == BIT_NOT_EXPR)
	fn = build_x_unary_op (BIT_NOT_EXPR, TREE_OPERAND (fn, 0));
      else if (TREE_CODE (fn) != TEMPLATE_ID_EXPR)
	fn = do_identifier (fn, 2, args);
    }
  result = build_x_function_call (fn, args, current_class_ref);

  if (TREE_CODE (result) == CALL_EXPR
      && (! TREE_TYPE (result)
          || TREE_CODE (TREE_TYPE (result)) != VOID_TYPE))
    result = require_complete_type (result);

  return result;
}

/* Finish a call to a postfix increment or decrement or EXPR.  (Which
   is indicated by CODE, which should be POSTINCREMENT_EXPR or
   POSTDECREMENT_EXPR.)  */

tree 
finish_increment_expr (expr, code)
     tree expr;
     enum tree_code code;
{
  /* If we get an OFFSET_REF, turn it into what it really means (e.g.,
     a COMPONENT_REF).  This way if we've got, say, a reference to a
     static member that's being operated on, we don't end up trying to
     find a member operator for the class it's in.  */

  if (TREE_CODE (expr) == OFFSET_REF)
    expr = resolve_offset_ref (expr);
  return build_x_unary_op (code, expr);  
}

/* Finish a use of `this'.  Returns an expression for `this'.  */

tree 
finish_this_expr ()
{
  tree result;

  if (current_class_ptr)
    {
#ifdef WARNING_ABOUT_CCD
      TREE_USED (current_class_ptr) = 1;
#endif
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

/* Finish a member function call using OBJECT and ARGS as arguments to
   FN.  Returns an expression for the call.  */

tree 
finish_object_call_expr (fn, object, args)
     tree fn;
     tree object;
     tree args;
{
#if 0
  /* This is a future direction of this code, but because
     build_x_function_call cannot always undo what is done in
     build_component_ref entirely yet, we cannot do this.  */

  tree real_fn = build_component_ref (object, fn, NULL_TREE, 1);
  return finish_call_expr (real_fn, args);
#else
  if (TREE_CODE (fn) == TYPE_DECL)
    {
      if (processing_template_decl)
	/* This can happen on code like:

	   class X;
	   template <class T> void f(T t) {
	     t.X();
	   }  

	   We just grab the underlying IDENTIFIER.  */
	fn = DECL_NAME (fn);
      else
	{
	  cp_error ("calling type `%T' like a method", fn);
	  return error_mark_node;
	}
    }

  return build_method_call (object, fn, args, NULL_TREE, LOOKUP_NORMAL);
#endif
}

/* Finish a qualified member function call using OBJECT and ARGS as
   arguments to FN.  Returns an expressino for the call.  */

tree 
finish_qualified_object_call_expr (fn, object, args)
     tree fn;
     tree object;
     tree args;
{
  return build_scoped_method_call (object, TREE_OPERAND (fn, 0),
				   TREE_OPERAND (fn, 1), args);
}

/* Finish a pseudo-destructor call expression of OBJECT, with SCOPE
   being the scope, if any, of DESTRUCTOR.  Returns an expression for
   the call.  */

tree 
finish_pseudo_destructor_call_expr (object, scope, destructor)
     tree object;
     tree scope;
     tree destructor;
{
  if (processing_template_decl)
    return build_min_nt (PSEUDO_DTOR_EXPR, object, scope, destructor);

  if (scope && scope != destructor)
    cp_error ("destructor specifier `%T::~%T()' must have matching names", 
	      scope, destructor);

  if ((scope == NULL_TREE || IDENTIFIER_GLOBAL_VALUE (destructor))
      && (TREE_CODE (TREE_TYPE (object)) !=
	  TREE_CODE (TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (destructor)))))
    cp_error ("`%E' is not of type `%T'", object, destructor);

  return cp_convert (void_type_node, object);
}

/* Finish a call to a globally qualified member function FN using
   ARGS.  Returns an expression for the call.  */

tree 
finish_qualified_call_expr (fn, args)
     tree fn;
     tree args;
{
  if (processing_template_decl)
    return build_min_nt (CALL_EXPR, copy_to_permanent (fn), args,
			 NULL_TREE);
  else
    return build_member_call (TREE_OPERAND (fn, 0),
			      TREE_OPERAND (fn, 1),
			      args);
}

/* Finish an expression taking the address of LABEL.  Returns an
   expression for the address.  */

tree 
finish_label_address_expr (label)
     tree label;
{
  tree result;

  label = lookup_label (label);
  if (label == NULL_TREE)
    result = null_pointer_node;
  else
    {
      TREE_USED (label) = 1;
      result = build1 (ADDR_EXPR, ptr_type_node, label);
      TREE_CONSTANT (result) = 1;
    }

  return result;
}

/* Finish an expression of the form CODE EXPR.  */

tree
finish_unary_op_expr (code, expr)
     enum tree_code code;
     tree expr;
{
  tree result = build_x_unary_op (code, expr);
  if (code == NEGATE_EXPR && TREE_CODE (expr) == INTEGER_CST)
    TREE_NEGATED_INT (result) = 1;
  overflow_warning (result);
  return result;
}

/* Finish an id-expression.  */

tree
finish_id_expr (expr)
     tree expr;
{
  if (TREE_CODE (expr) == IDENTIFIER_NODE)
    expr = do_identifier (expr, 1, NULL_TREE);

  return expr;
}

/* Begin a new-placement.  */

int
begin_new_placement ()
{
  /* The arguments to a placement new might be passed to a
     deallocation function, in the event that the allocation throws an
     exception.  Since we don't expand exception handlers until the
     end of a function, we must make sure the arguments stay around
     that long.  */
  return suspend_momentary ();
}

/* Finish a new-placement.  The ARGS are the placement arguments.  The
   COOKIE is the value returned by the previous call to
   begin_new_placement.  */

tree
finish_new_placement (args, cookie)
     tree args;
     int cookie;
{
  resume_momentary (cookie);
  return args;
}

/* Begin a function defniition declared with DECL_SPECS and
   DECLARATOR.  Returns non-zero if the function-declaration is
   legal.  */

int
begin_function_definition (decl_specs, declarator)
     tree decl_specs;
     tree declarator;
{
  tree specs;
  tree attrs;
  split_specs_attrs (decl_specs, &specs, &attrs);
  if (!start_function (specs, declarator, attrs, 0))
    return 0;
  
  reinit_parse_for_function ();
  /* The things we're about to see are not directly qualified by any
     template headers we've seen thus far.  */
  reset_specialization ();

  return 1;
}

/* Begin a constructor declarator of the form `SCOPE::NAME'.  Returns
   a SCOPE_REF.  */

tree 
begin_constructor_declarator (scope, name)
     tree scope;
     tree name;
{
  tree result = build_parse_node (SCOPE_REF, scope, name);
  enter_scope_of (result);
  return result;
}

/* Finish an init-declarator.  Returns a DECL.  */

tree
finish_declarator (declarator, declspecs, attributes,
		   prefix_attributes, initialized)
     tree declarator;
     tree declspecs;
     tree attributes;
     tree prefix_attributes;
     int initialized;
{
  return start_decl (declarator, declspecs, initialized, attributes,
		     prefix_attributes); 
}

/* Finish a translation unit.  */

void 
finish_translation_unit ()
{
  /* In case there were missing closebraces,
     get us back to the global binding level.  */
  while (! toplevel_bindings_p ())
    poplevel (0, 0, 0);
  while (current_namespace != global_namespace)
    pop_namespace ();
  finish_file ();
}

/* Finish a template type parameter, specified as AGGR IDENTIFIER.
   Returns the parameter.  */

tree 
finish_template_type_parm (aggr, identifier)
     tree aggr;
     tree identifier;
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
finish_template_template_parm (aggr, identifier)
     tree aggr;
     tree identifier;
{
  tree decl = build_decl (TYPE_DECL, identifier, NULL_TREE);
  tree tmpl = build_lang_decl (TEMPLATE_DECL, identifier, NULL_TREE);
  DECL_TEMPLATE_PARMS (tmpl) = current_template_parms;
  DECL_TEMPLATE_RESULT (tmpl) = decl;
  SET_DECL_ARTIFICIAL (decl);
  end_template_decl ();

  return finish_template_type_parm (aggr, tmpl);
}

/* Finish a parameter list, indicated by PARMS.  If ELLIPSIS is
   non-zero, the parameter list was terminated by a `...'.  */

tree
finish_parmlist (parms, ellipsis)
     tree parms;
     int ellipsis;
{
  if (!ellipsis)
    chainon (parms, void_list_node);
  /* We mark the PARMS as a parmlist so that declarator processing can
     disambiguate certain constructs.  */
  if (parms != NULL_TREE)
    TREE_PARMLIST (parms) = 1;

  return parms;
}

/* Begin a class definition, as indicated by T.  */

tree
begin_class_definition (t)
     tree t;
{
  push_permanent_obstack ();

  if (t == error_mark_node
      || ! IS_AGGR_TYPE (t))
    {
      t = make_lang_type (RECORD_TYPE);
      pushtag (make_anon_name (), t, 0);
    }

  /* In a definition of a member class template, we will get here with an
     implicit typename, a TYPENAME_TYPE with a type.  */
  if (TREE_CODE (t) == TYPENAME_TYPE)
    t = TREE_TYPE (t);
  
  /* If we generated a partial instantiation of this type, but now
     we're seeing a real definition, we're actually looking at a
     partial specialization.  Consider:

       template <class T, class U>
       struct Y {};

       template <class T>
       struct X {};

       template <class T, class U>
       void f()
       {
	 typename X<Y<T, U> >::A a;
       }

       template <class T, class U>
       struct X<Y<T, U> >
       {
       };

     We have to undo the effects of the previous partial
     instantiation.  */
  if (PARTIAL_INSTANTIATION_P (t))
    {
      if (!pedantic) 
	{
	  /* Unfortunately, when we're not in pedantic mode, we
	     attempt to actually fill in some of the fields of the
	     partial instantiation, in order to support the implicit
	     typename extension.  Clear those fields now, in
	     preparation for the definition here.  The fields cleared
	     here must match those set in instantiate_class_template.
	     Look for a comment mentioning begin_class_definition
	     there.  */
	  TYPE_BINFO_BASETYPES (t) = NULL_TREE;
	  TYPE_FIELDS (t) = NULL_TREE;
	  TYPE_METHODS (t) = NULL_TREE;
	  CLASSTYPE_TAGS (t) = NULL_TREE;
	  TYPE_SIZE (t) = NULL_TREE;
	}

      /* This isn't a partial instantiation any more.  */
      PARTIAL_INSTANTIATION_P (t) = 0;
    }
  /* If this type was already complete, and we see another definition,
     that's an error.  */
  else if (TYPE_SIZE (t))
    duplicate_tag_error (t);

  /* Update the location of the decl.  */
  DECL_SOURCE_FILE (TYPE_NAME (t)) = input_filename;
  DECL_SOURCE_LINE (TYPE_NAME (t)) = lineno;
  
  if (TYPE_BEING_DEFINED (t))
    {
      t = make_lang_type (TREE_CODE (t));
      pushtag (TYPE_IDENTIFIER (t), t, 0);
    }
  maybe_process_partial_specialization (t);
  pushclass (t, 1);
  TYPE_BEING_DEFINED (t) = 1;
  /* Reset the interface data, at the earliest possible
     moment, as it might have been set via a class foo;
     before.  */
  {
    tree name = TYPE_IDENTIFIER (t);
    
    if (! ANON_AGGRNAME_P (name))
      {
	CLASSTYPE_INTERFACE_ONLY (t) = interface_only;
	SET_CLASSTYPE_INTERFACE_UNKNOWN_X
	  (t, interface_unknown);
      }
    
    /* Only leave this bit clear if we know this
       class is part of an interface-only specification.  */
    if (! CLASSTYPE_INTERFACE_KNOWN (t)
	|| ! CLASSTYPE_INTERFACE_ONLY (t))
      CLASSTYPE_VTABLE_NEEDS_WRITING (t) = 1;
  }
#if 0
  tmp = TYPE_IDENTIFIER ($<ttype>0);
  if (tmp && IDENTIFIER_TEMPLATE (tmp))
    overload_template_name (tmp, 1);
#endif
  reset_specialization();
  
  /* In case this is a local class within a template
     function, we save the current tree structure so
     that we can get it back later.  */
  begin_tree ();

  /* Make a declaration for this class in its own scope.  */
  build_self_reference ();

  return t;
}

/* Finish the member declaration given by DECL.  */

void
finish_member_declaration (decl)
     tree decl;
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
      TREE_PRIVATE (DECL_RESULT (decl)) = TREE_PRIVATE (decl);
      TREE_PROTECTED (DECL_RESULT (decl)) = TREE_PROTECTED (decl);
    }

  /* Mark the DECL as a member of the current class.  */
  if (TREE_CODE (decl) == FUNCTION_DECL 
      || DECL_FUNCTION_TEMPLATE_P (decl))
    /* Historically, DECL_CONTEXT was not set for a FUNCTION_DECL in
       finish_struct.  Presumably it is already set as the function is
       parsed.  Perhaps DECL_CLASS_CONTEXT is already set, too?  */
    DECL_CLASS_CONTEXT (decl) = current_class_type;
  else
    DECL_CONTEXT (decl) = current_class_type;

  /* Put functions on the TYPE_METHODS list and everything else on the
     TYPE_FIELDS list.  Note that these are built up in reverse order.
     We reverse them (to obtain declaration order) in finish_struct.  */
  if (TREE_CODE (decl) == FUNCTION_DECL 
      || DECL_FUNCTION_TEMPLATE_P (decl))
    {
      /* We also need to add this function to the
	 CLASSTYPE_METHOD_VEC.  */
      add_method (current_class_type, 0, decl);

      TREE_CHAIN (decl) = TYPE_METHODS (current_class_type);
      TYPE_METHODS (current_class_type) = decl;
    }
  else
    {
      /* All TYPE_DECLs go at the end of TYPE_FIELDS.  Ordinary fields
	 go at the beginning.  The reason is that lookup_field_1
	 searches the list in order, and we want a field name to
	 override a type name so that the "struct stat hack" will
	 work.  In particular:

	   struct S { enum E { }; int E } s;
	   s.E = 3;

	 is legal.  In addition, the FIELD_DECLs must be maintained in
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

      /* Enter the DECL into the scope of the class.  */
      if (TREE_CODE (decl) != USING_DECL)
	pushdecl_class_level (decl);
    }
}

/* Finish a class definition T with the indicate ATTRIBUTES.  If SEMI,
   the definition is immediately followed by a semicolon.  Returns the
   type.  */

tree
finish_class_definition (t, attributes, semi, pop_scope_p)
     tree t;
     tree attributes;
     int semi;
     int pop_scope_p;
{
  /* finish_struct nukes this anyway; if finish_exception does too,
     then it can go.  */
  if (semi)
    note_got_semicolon (t);

  /* If we got any attributes in class_head, xref_tag will stick them in
     TREE_TYPE of the type.  Grab them now.  */
  attributes = chainon (TREE_TYPE (t), attributes);
  TREE_TYPE (t) = NULL_TREE;

  if (TREE_CODE (t) == ENUMERAL_TYPE)
    ;
  else
    {
      t = finish_struct (t, attributes);
      if (semi) 
	note_got_semicolon (t);
    }

  pop_obstacks ();

  if (! semi)
    check_for_missing_semicolon (t); 
  if (pop_scope_p)
    pop_scope (CP_DECL_CONTEXT (TYPE_MAIN_DECL (t)));
  if (current_scope () == current_function_decl)
    do_pending_defargs ();

  return t;
}

/* Finish processing the default argument expressions cached during
   the processing of a class definition.  */

void
begin_inline_definitions ()
{
  if (pending_inlines 
      && current_scope () == current_function_decl)
    do_pending_inlines ();
}

/* Finish processing the inline function definitions cached during the
   processing of a class definition.  */

void
finish_inline_definitions ()
{
  if (current_class_type == NULL_TREE)
    clear_inline_text_obstack (); 
  
  /* Undo the begin_tree in begin_class_definition.  */
  end_tree ();
}

/* Finish processing the declaration of a member class template
   TYPES whose template parameters are given by PARMS.  */

tree
finish_member_class_template (types)
     tree types;
{
  tree t;

  /* If there are declared, but undefined, partial specializations
     mixed in with the typespecs they will not yet have passed through
     maybe_process_partial_specialization, so we do that here.  */
  for (t = types; t != NULL_TREE; t = TREE_CHAIN (t))
    if (IS_AGGR_TYPE_CODE (TREE_CODE (TREE_VALUE (t))))
      maybe_process_partial_specialization (TREE_VALUE (t));

  note_list_got_semicolon (types);
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

/* Finish processsing a complete template declaration.  The PARMS are
   the template parameters.  */

void
finish_template_decl (parms)
     tree parms;
{
  if (parms)
    end_template_decl ();
  else
    end_specialization ();
}

/* Finish processing a a template-id (which names a type) of the form
   NAME < ARGS >.  Return the TYPE_DECL for the type named by the
   template-id.  If ENTERING_SCOPE is non-zero we are about to enter
   the scope of template-id indicated.  */

tree
finish_template_type (name, args, entering_scope)
     tree name;
     tree args;
     int entering_scope;
{
  tree decl;

  decl = lookup_template_class (name, args,
				NULL_TREE, NULL_TREE, entering_scope);
  if (decl != error_mark_node)
    decl = TYPE_STUB_DECL (decl);

  return decl;
}

/* SR is a SCOPE_REF node.  Enter the scope of SR, whether it is a
   namespace scope or a class scope.  */

void
enter_scope_of (sr)
     tree sr;
{
  tree scope = TREE_OPERAND (sr, 0);

  if (TREE_CODE (scope) == NAMESPACE_DECL)
    {
      push_decl_namespace (scope);
      TREE_COMPLEXITY (sr) = -1;
    }
  else if (scope != current_class_type)
    {
      if (TREE_CODE (scope) == TYPENAME_TYPE)
	{
	  /* In a declarator for a template class member, the scope will
	     get here as an implicit typename, a TYPENAME_TYPE with a type.  */
	  scope = TREE_TYPE (scope);
	  TREE_OPERAND (sr, 0) = scope;
	}
      push_nested_class (scope, 3);
      TREE_COMPLEXITY (sr) = current_class_depth;
    }
}

/* Finish processing a BASE_CLASS with the indicated ACCESS_SPECIFIER.
   Return a TREE_LIST containing the ACCESS_SPECIFIER and the
   BASE_CLASS, or NULL_TREE if an error occurred.  The
   ACCESSS_SPECIFIER is one of
   access_{default,public,protected_private}[_virtual]_node.*/

tree 
finish_base_specifier (access_specifier, base_class)
     tree access_specifier;
     tree base_class;
{
  tree type;
  tree result;

  if (base_class == NULL_TREE)
    {
      error ("invalid base class");
      type = error_mark_node;
    }
  else
    type = TREE_TYPE (base_class);

  if (! is_aggr_type (type, 1))
    result = NULL_TREE;
  else
    result = build_tree_list (access_specifier, type);

  return result;
}

/* Called when multiple declarators are processed.  If that is not
   premitted in this context, an error is issued.  */

void
check_multiple_declarators ()
{
  /* [temp]
     
     In a template-declaration, explicit specialization, or explicit
     instantiation the init-declarator-list in the declaration shall
     contain at most one declarator.  

     We don't just use PROCESSING_TEMPLATE_DECL for the first
     condition since that would disallow the perfectly legal code, 
     like `template <class T> struct S { int i, j; };'.  */
  tree scope = current_scope ();

  if (scope && TREE_CODE (scope) == FUNCTION_DECL)
    /* It's OK to write `template <class T> void f() { int i, j;}'.  */
    return;
     
  if (PROCESSING_REAL_TEMPLATE_DECL_P () 
      || processing_explicit_instantiation
      || processing_specialization)
    cp_error ("multiple declarators in template declaration");
}

tree
finish_typeof (expr)
     tree expr;
{
  if (processing_template_decl)
    {
      tree t;

      push_permanent_obstack ();
      t = make_lang_type (TYPEOF_TYPE);
      TYPE_FIELDS (t) = expr;
      pop_obstacks ();

      return t;
    }

  return TREE_TYPE (expr);
}

/* Create an empty statement tree for FN.  */

void
begin_stmt_tree (fn)
     tree fn;
{
  /* We create a trivial EXPR_STMT so that last_tree is never NULL in
     what follows.  We remove the extraneous statement in
     finish_stmt_tree.  */
  DECL_SAVED_TREE (fn) = build_nt (EXPR_STMT, void_zero_node);
  last_tree = DECL_SAVED_TREE (fn);
  last_expr_type = NULL_TREE;
}

/* Finish the statement tree for FN.  */

void
finish_stmt_tree (fn)
     tree fn;
{
  DECL_SAVED_TREE (fn) = TREE_CHAIN (DECL_SAVED_TREE (fn));
}

/* Generate RTL for the chain of statements T.  */

static void 
expand_stmts (t)
     tree t;
{
  while (t)
    {
      expand_stmt (t);
      t = TREE_CHAIN (t);
    }
}

/* Generate RTL for the statement T, and its substatements.  */

tree
expand_stmt (t)
     tree t;
{
  if (t == NULL_TREE || t == error_mark_node)
    return NULL_TREE;

  switch (TREE_CODE (t))
    {
    case RETURN_STMT:
      lineno = STMT_LINENO (t);
      finish_return_stmt (RETURN_EXPR (t));
      break;

    case EXPR_STMT:
      lineno = STMT_LINENO (t);
      finish_expr_stmt (EXPR_STMT_EXPR (t));
      break;

    case DECL_STMT:
      {
	tree decl;
	int i = suspend_momentary ();

	lineno = STMT_LINENO (t);
	emit_line_note (input_filename, lineno);
	decl = DECL_STMT_DECL (t);
	if (TREE_CODE (decl) == LABEL_DECL)
	  finish_label_decl (DECL_NAME (decl));
	else
	  {
	    /* We need to clear DECL_CONTEXT so that maybe_push_decl
	       will push it into the current scope.  */
	    if (DECL_CONTEXT (decl) == current_function_decl)
	      DECL_CONTEXT (decl) = NULL_TREE;
	    /* If we marked this variable as dead when we processed it
	       before, we must undo that now.  The variable has been
	       resuscitated.  */
	    if (TREE_CODE (decl) == VAR_DECL)
	      DECL_DEAD_FOR_LOCAL (decl) = 0;
	    maybe_push_decl (decl);
	    if (TREE_CODE (decl) == VAR_DECL && !TREE_STATIC (decl))
	      {
		maybe_inject_for_scope_var (decl);
		initialize_local_var (decl, DECL_INITIAL (decl), 0);
	      }
	  }
	resume_momentary (i);
      }
      break;

    case FOR_STMT:
      {
	tree tmp;

	lineno = STMT_LINENO (t);
	begin_for_stmt ();
	for (tmp = FOR_INIT_STMT (t); tmp; tmp = TREE_CHAIN (tmp))
	  expand_stmt (tmp);
	finish_for_init_stmt (NULL_TREE);
	finish_for_cond (FOR_COND (t), NULL_TREE);
	tmp = FOR_EXPR (t);
	finish_for_expr (tmp, NULL_TREE);
	expand_stmt (FOR_BODY (t));
	finish_for_stmt (tmp, NULL_TREE);
      }
      break;

    case WHILE_STMT:
      {
	lineno = STMT_LINENO (t);
	begin_while_stmt ();
	finish_while_stmt_cond (WHILE_COND (t), NULL_TREE);
	expand_stmt (WHILE_BODY (t));
	finish_while_stmt (NULL_TREE);
      }
      break;

    case DO_STMT:
      {
	lineno = STMT_LINENO (t);
	begin_do_stmt ();
	expand_stmt (DO_BODY (t));
	finish_do_body (NULL_TREE);
	finish_do_stmt (DO_COND (t), NULL_TREE);
      }
      break;

    case IF_STMT:
      lineno = STMT_LINENO (t);
      begin_if_stmt ();
      finish_if_stmt_cond (IF_COND (t), NULL_TREE);
      if (THEN_CLAUSE (t))
	{
	  expand_stmt (THEN_CLAUSE (t));
	  finish_then_clause (NULL_TREE);
	}
      if (ELSE_CLAUSE (t))
	{
	  begin_else_clause ();
	  expand_stmt (ELSE_CLAUSE (t));
	  finish_else_clause (NULL_TREE);
	}
      finish_if_stmt ();
      break;

    case COMPOUND_STMT:
      lineno = STMT_LINENO (t);
      begin_compound_stmt (COMPOUND_STMT_NO_SCOPE (t));
      expand_stmts (COMPOUND_BODY (t));
      return finish_compound_stmt (COMPOUND_STMT_NO_SCOPE (t), 
				   NULL_TREE);

    case BREAK_STMT:
      lineno = STMT_LINENO (t);
      finish_break_stmt ();
      break;

    case CONTINUE_STMT:
      lineno = STMT_LINENO (t);
      finish_continue_stmt ();
      break;

    case SWITCH_STMT:
      lineno = STMT_LINENO (t);
      begin_switch_stmt ();
      finish_switch_cond (SWITCH_COND (t));
      if (TREE_OPERAND (t, 1))
	expand_stmt (SWITCH_BODY (t));
      finish_switch_stmt (SWITCH_COND (t), NULL_TREE);
      break;

    case CASE_LABEL:
      finish_case_label (CASE_LOW (t), CASE_HIGH (t));
      break;

    case LABEL_STMT:
      lineno = STMT_LINENO (t);
      finish_label_stmt (DECL_NAME (LABEL_STMT_LABEL (t)));
      break;

    case GOTO_STMT:
      lineno = STMT_LINENO (t);
      if (TREE_CODE (GOTO_DESTINATION (t)) == LABEL_DECL)
	finish_goto_stmt (DECL_NAME (GOTO_DESTINATION (t)));
      else
	finish_goto_stmt (GOTO_DESTINATION (t));
      break;

    case ASM_STMT:
      lineno = STMT_LINENO (t);
      finish_asm_stmt (ASM_CV_QUAL (t), ASM_STRING (t), ASM_OUTPUTS
		       (t), ASM_INPUTS (t), ASM_CLOBBERS (t));
      break;

    case TRY_BLOCK:
      lineno = STMT_LINENO (t);
      if (CLEANUP_P (t))
	{
	  expand_eh_region_start ();
	  expand_stmt (TRY_STMTS (t));
	  finish_cleanup (TRY_HANDLERS (t), NULL_TREE);
	}
      else
	{
	  begin_try_block ();
	  expand_stmt (TRY_STMTS (t));
	  finish_try_block (NULL_TREE);
	  expand_stmts (TRY_HANDLERS (t));
	  finish_handler_sequence (NULL_TREE);
	}
      break;

    case HANDLER:
      lineno = STMT_LINENO (t);
      begin_handler ();
      if (HANDLER_PARMS (t))
	expand_start_catch_block (DECL_STMT_DECL (HANDLER_PARMS (t)));
      else
	expand_start_catch_block (NULL_TREE);
      finish_handler_parms (NULL_TREE);
      expand_stmt (HANDLER_BODY (t));
      finish_handler (NULL_TREE);
      break;

    case SUBOBJECT:
      lineno = STMT_LINENO (t);
      finish_subobject (SUBOBJECT_CLEANUP (t));
      break;

    default:
      my_friendly_abort (19990810);
      break;
    }

  return NULL_TREE;
}

/* Generate RTL for FN.  */

void
expand_body (fn)
     tree fn;
{
  int saved_expanding_p;
  int nested; 
  tree t;
  tree try_block;

  /* Remember whether we're already processing a function definition
     so that we can tell finish_function.  */
  nested = in_function_p ();

  /* Let the compiler know that now is the time to really generate
     actualy RTL.  */
  saved_expanding_p = expanding_p;
  expanding_p = 1;

  start_function (NULL_TREE, fn, NULL_TREE, 1);
  store_parm_decls ();

  /* There are a few things that we do not handle recursively.  For
     example, a function try-block is handled differently from an
     ordinary try-block, so we must handle it here.  */
  t = DECL_SAVED_TREE (fn);
  try_block = NULL_TREE;
  if (t && TREE_CODE (t) == TRY_BLOCK)
    {
      try_block = t;
      begin_function_try_block ();
      t = TRY_STMTS (try_block);
    }

  if (t && TREE_CODE (t) == RETURN_INIT)
    {
      /* Clear this out so that finish_named_return_value can set it
	 again.  */
      DECL_NAME (DECL_RESULT (fn)) = NULL_TREE;
      finish_named_return_value (TREE_OPERAND (t, 0), TREE_OPERAND (t, 1));
      t = TREE_CHAIN (t);
    }

  if (t && TREE_CODE (t) == CTOR_INITIALIZER)
    {
      current_member_init_list = TREE_OPERAND (t, 0);
      current_base_init_list = TREE_OPERAND (t, 1);
      t = TREE_CHAIN (t);
    }

  /* If this is a constructor, we need to initialize our members and
     base-classes.  */
  setup_vtbl_ptr ();

  /* Expand the body.  */
  expand_stmt (t);

  /* If there was a function try-block, expand the handlers.  */
  if (try_block)
    {
      finish_function_try_block (NULL_TREE);
      {
	tree handler = TRY_HANDLERS (try_block);
	for (; handler; handler = TREE_CHAIN (handler))
	  expand_stmt (handler);
      }
      finish_function_handler_sequence (NULL_TREE);
    }

  finish_function (lineno, 0, nested);

  /* Restore EXPANDING_P.  */
  expanding_p = saved_expanding_p;
}
