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

/* Begin a statement-expression.  Returns a new RTL_EXPR if
   appropriate. */

tree 
begin_stmt_expr ()
{
  keep_next_level ();
  return processing_template_decl ? NULL_TREE : expand_start_stmt_expr(); 
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

  if (!processing_template_decl)
    {
      rtl_expr = expand_end_stmt_expr (rtl_expr);
      /* The statements have side effects, so the group does.  */
      TREE_SIDE_EFFECTS (rtl_expr) = 1;
    }

  if (TREE_CODE (expr) == BLOCK)
    {
      /* Make a BIND_EXPR for the BLOCK already made.  */
      if (processing_template_decl)
	result = build (BIND_EXPR, NULL_TREE,
			NULL_TREE, last_tree, expr);
      else
	result = build (BIND_EXPR, TREE_TYPE (rtl_expr),
			NULL_TREE, rtl_expr, expr);
      
      /* Remove the block from the tree at this point.
	 It gets put back at the proper place
	 when the BIND_EXPR is expanded.  */
      delete_block (expr);
    }
  else
    result = expr;
  
  return result;
}

/* Finish a call to FN with ARGS.  Returns a representation of the
   call.  */

tree 
finish_call_expr (fn, args)
     tree fn;
     tree args;
{
  tree result = build_x_function_call (fn, args, current_class_ref);

  if (TREE_CODE (result) == CALL_EXPR
      && TREE_TYPE (result) != void_type_node)
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
  if (IS_SIGNATURE (TREE_OPERAND (fn, 0)))
    {
      warning ("signature name in scope resolution ignored");
      return finish_object_call_expr (TREE_OPERAND (fn, 1), object, args);
    }
  else
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
finish_globally_qualified_member_call_expr (fn, args)
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

  if (scope != current_class_type)
    {
      push_nested_class (scope, 3);
      TREE_COMPLEXITY (result) = current_class_depth;
    }

  return result;
}

/* Finish a template type parameter, specified as AGGR IDENTIFIER.
   Returns the parameter.  */

tree 
finish_template_type_parm (aggr, identifier)
     tree aggr;
     tree identifier;
{
  if (aggr == signature_type_node)
    sorry ("signature as template type parameter");
  else if (aggr != class_type_node)
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
