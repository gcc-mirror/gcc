/* Perform the semantic phase of parsing, i.e., the process of
   building tree structure, checking semantic consistency, and
   building RTL.  These routines are used both during actual parsing
   and during the instantiation of template functions. 

   Copyright (C) 1998 Free Software Foundation, Inc.
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
#include <stdio.h>
#include "tree.h"
#include "cp-tree.h"
#include "except.h"
#include "lex.h"

/* There routines provide a modular interface to perform many parsing
   operations.  They may therefore be used during actual parsing, or
   during template instantiation, which may be regarded as a
   degenerate form of parsing.  Since the current g++ parser is
   lacking in several respects, and will be reimplemented, we are
   attempting to move most code that is not directly related to
   parsing into this file; that will make implementing the new parser
   much easier since it will be able to make use of these routines.  */

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
  if (!processing_template_decl)
    {
      emit_line_note (input_filename, lineno);
      /* Do default conversion if safe and possibly important,
	 in case within ({...}).  */
      if ((TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE
	   && lvalue_p (expr))
	  || TREE_CODE (TREE_TYPE (expr)) == FUNCTION_TYPE)
	expr = default_conversion (expr);
    }
  
  cplus_expand_expr_stmt (expr);
  clear_momentary ();
  finish_stmt ();
}

/* Begin an if-statement.  Returns a newly created IF_STMT if
   appropriate.  */

tree
begin_if_stmt ()
{
  tree r;

  if (processing_template_decl)
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
  if (processing_template_decl)
    {
      if (last_tree != if_stmt)
	RECHAIN_STMTS_FROM_LAST (if_stmt, IF_COND (if_stmt));
      else
	IF_COND (if_stmt) = cond;
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
  if (processing_template_decl)
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
  if (!processing_template_decl)
    expand_start_else ();
}

/* Finish the else-clause of an if-statement, which may be given by
   IF_STMT.  */

void
finish_else_clause (if_stmt)
     tree if_stmt;
{
  if (processing_template_decl)
    RECHAIN_STMTS_FROM_CHAIN (if_stmt, ELSE_CLAUSE (if_stmt));
}

/* Finsh an if-statement.  */

void 
finish_if_stmt ()
{
  if (!processing_template_decl)
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

  if (processing_template_decl)
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
  if (processing_template_decl)
    {
      if (last_tree != while_stmt)
	RECHAIN_STMTS_FROM_LAST (while_stmt, 
				      WHILE_COND (while_stmt)); 
      else
	TREE_OPERAND (while_stmt, 0) = cond;
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

  if (processing_template_decl)
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
  if (processing_template_decl)
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
  if (processing_template_decl)
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
  if (processing_template_decl)
    DO_COND (do_stmt) = cond;
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
  emit_line_note (input_filename, lineno);
  c_expand_return (expr);
  finish_stmt ();
}

/* Begin a for-statement.  Returns a new FOR_STMT if appropriate.  */

tree
begin_for_stmt ()
{
  tree r;

  if (processing_template_decl)
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
  if (processing_template_decl)
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
  if (processing_template_decl)
    {
      if (last_tree != for_stmt)
	RECHAIN_STMTS_FROM_LAST (for_stmt, FOR_COND (for_stmt));
      else
	FOR_COND (for_stmt) = cond;
    }
  else
    {
      emit_line_note (input_filename, lineno);
      if (cond) 
	expand_exit_loop_if_false (0, cond);
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
  if (processing_template_decl)
    FOR_EXPR (for_stmt) = expr;

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

  if (processing_template_decl)
    RECHAIN_STMTS_FROM_CHAIN (for_stmt, FOR_BODY (for_stmt));
  else
    {
      emit_line_note (input_filename, lineno);
      expand_loop_continue_here ();
      if (expr) 
	cplus_expand_expr_stmt (expr);
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
  if (processing_template_decl)
    add_tree (build_min_nt (BREAK_STMT));
  else if ( ! expand_exit_something ())
    cp_error ("break statement not within loop or switch");
}

/* Finish a continue-statement.  */

void
finish_continue_stmt ()
{
  emit_line_note (input_filename, lineno);
  if (processing_template_decl)
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

  if (processing_template_decl)
    {
      r = build_min_nt (SWITCH_STMT, cond, NULL_TREE);
      add_tree (r);
    }
  else
    {
      emit_line_note (input_filename, lineno);
      c_expand_start_case (cond);
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
  if (processing_template_decl)
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
  do_case (low_value, high_value);
}


/* Finish a goto-statement.  */

void
finish_goto_stmt (destination)
     tree destination;
{
  if (processing_template_decl)
    add_tree (build_min_nt (GOTO_STMT, destination));
  else
    {
      emit_line_note (input_filename, lineno);

      if (TREE_CODE (destination) == IDENTIFIER_NODE)
	{
	  tree decl = lookup_label (destination);
	  TREE_USED (decl) = 1;
	  expand_goto (decl); 
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
  if (processing_template_decl)
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

/* Finish a try-block, which may be given by TRY_BLOCK.  */

void
finish_try_block (try_block)
     tree try_block;
{
  if (processing_template_decl)
    RECHAIN_STMTS_FROM_LAST (try_block, TRY_STMTS (try_block));
  else
    expand_start_all_catch ();  
}

/* Finish a handler-sequence for a try-block, which may be given by
   TRY_BLOCK.  */

void
finish_handler_sequence (try_block)
     tree try_block;
{
  if (processing_template_decl)
    RECHAIN_STMTS_FROM_CHAIN (try_block, TRY_HANDLERS (try_block));
  else
    expand_end_all_catch ();
}

/* Begin a handler.  Returns a HANDLER if appropriate.  */

tree
begin_handler ()
{
  tree r;

  if (processing_template_decl)
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
  if (processing_template_decl)
    RECHAIN_STMTS_FROM_CHAIN (handler, HANDLER_PARMS (handler));
}

/* Finish a handler, which may be given by HANDLER.  */

void
finish_handler (handler)
     tree handler;
{
  if (processing_template_decl)
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

  if (processing_template_decl)
    {
      r = build_min_nt (COMPOUND_STMT, NULL_TREE);
      add_tree (r);
      if (has_no_scope)
	COMPOUND_STMT_NO_SCOPE (r) = 1;
    }
  else
    r = NULL_TREE;

  if (!has_no_scope)
    do_pushlevel ();

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

  if (!has_no_scope)
    r = do_poplevel ();
  else
    r = NULL_TREE;

  if (processing_template_decl)
    RECHAIN_STMTS_FROM_CHAIN (compound_stmt, 
			      COMPOUND_BODY (compound_stmt));

  finish_stmt ();

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
    string = combine_strings (string);

  if (processing_template_decl)
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
	  if (cv_qualifier != NULL_TREE
	      && cv_qualifier != ridpointers[(int) RID_VOLATILE])
	    cp_warning ("%s qualifier ignored on asm",
			IDENTIFIER_POINTER (cv_qualifier));
	    
	  c_expand_asm_operands (string, output_operands,
				 input_operands, 
				 clobbers,
				 cv_qualifier 
				 == ridpointers[(int) RID_VOLATILE],
				 input_filename, lineno);
	}
      else
	{
	  if (cv_qualifier != NULL_TREE)
	    cp_warning ("%s qualifier ignored on asm",
			IDENTIFIER_POINTER (cv_qualifier));
	  expand_asm (string);
	}

      finish_stmt ();
    }
}
