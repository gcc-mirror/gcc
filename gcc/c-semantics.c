/* This file contains the definitions and documentation for the common
   tree codes used in the GNU C and C++ compilers (see c-common.def
   for the standard codes).  
   Copyright (C) 2000 Free Software Foundation, Inc.  Written by
   Benjamin Chelf (chelf@codesourcery.com).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "function.h"
#include "splay-tree.h"
#include "varray.h"
#include "c-common.h"
#include "except.h"
#include "toplev.h"
#include "flags.h"
#include "ggc.h"
#include "rtl.h"
#include "output.h"
#include "timevar.h"

/* Build a generic statement based on the given type of node and
   arguments. Similar to `build_nt', except that we set
   TREE_COMPLEXITY to be the current line number.  */

tree
build_stmt VPARAMS ((enum tree_code code, ...))
{
#ifndef ANSI_PROTOTYPES
  enum tree_code code;
#endif
  va_list p;
  register tree t;
  register int length;
  register int i;

  VA_START (p, code);

#ifndef ANSI_PROTOTYPES
  code = va_arg (p, enum tree_code);
#endif

  t = make_node (code);
  length = TREE_CODE_LENGTH (code);
  TREE_COMPLEXITY (t) = lineno;

  for (i = 0; i < length; i++)
    TREE_OPERAND (t, i) = va_arg (p, tree);

  va_end (p);
  return t;
}

/* Some statements, like for-statements or if-statements, require a
   condition.  This condition can be a declaration.  If T is such a
   declaration it is processed, and an expression appropriate to use
   as the condition is returned.  Otherwise, T itself is returned.  */

tree
expand_cond (t)
     tree t;
{
  if (t && TREE_CODE (t) == TREE_LIST)
    {
      expand_stmt (TREE_PURPOSE (t));
      return TREE_VALUE (t);
    }
  else 
    return t;
}

/* Create RTL for the local static variable DECL.  */

void
make_rtl_for_local_static (decl)
     tree decl;
{
  const char *asmspec = NULL;

  /* If we inlined this variable, we could see it's declaration
     again.  */
  if (DECL_RTL (decl))
    return;

  if (DECL_ASSEMBLER_NAME (decl) != DECL_NAME (decl))
    {
      /* The only way this situaton can occur is if the
	 user specified a name for this DECL using the
	 `attribute' syntax.  */
      asmspec = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      DECL_ASSEMBLER_NAME (decl) = DECL_NAME (decl);
    }

  rest_of_decl_compilation (decl, asmspec, /*top_level=*/0, /*at_end=*/0);
}

/* Let the back-end know about DECL.  */

void
emit_local_var (decl)
     tree decl;
{
  /* Create RTL for this variable.  */
  if (!DECL_RTL (decl))
    {
      if (DECL_ASSEMBLER_NAME (decl) != DECL_NAME (decl))
	/* The user must have specified an assembler name for this
	   variable.  Set that up now.  */
	rest_of_decl_compilation
	  (decl, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)),
	   /*top_level=*/0, /*at_end=*/0);
      else
	expand_decl (decl);
    }

  /* Actually do the initialization.  */
  if (stmts_are_full_exprs_p ())
    expand_start_target_temps ();

  expand_decl_init (decl);

  if (stmts_are_full_exprs_p ())
    expand_end_target_temps ();
}

/* Helper for generating the RTL at the beginning of a scope. */

void
genrtl_do_pushlevel ()
{
  emit_line_note (input_filename, lineno);
  clear_last_expr ();
}

/* Helper for generating the RTL. */

void
genrtl_clear_out_block ()
{
  /* If COND wasn't a declaration, clear out the
     block we made for it and start a new one here so the
     optimization in expand_end_loop will work.  */
  if (getdecls () == NULL_TREE)
    genrtl_do_pushlevel ();
}

/* Generate the RTL for DESTINATION, which is a GOTO_STMT. */

void
genrtl_goto_stmt (destination)
     tree destination;
{
  if (TREE_CODE (destination) == IDENTIFIER_NODE)
    abort ();
  
  /* We warn about unused labels with -Wunused.  That means we have to
     mark the used labels as used.  */
  if (TREE_CODE (destination) == LABEL_DECL)
    TREE_USED (destination) = 1;
  
  emit_line_note (input_filename, lineno);
  
  if (TREE_CODE (destination) == LABEL_DECL)
    {
      label_rtx (destination);
      expand_goto (destination); 
    }
  else
    expand_computed_goto (destination);
}

/* Generate the RTL for EXPR, which is an EXPR_STMT. */

void 
genrtl_expr_stmt (expr)
     tree expr;
{
  if (expr != NULL_TREE)
    {
      emit_line_note (input_filename, lineno);
      
      if (stmts_are_full_exprs_p ())
	expand_start_target_temps ();
      
      lang_expand_expr_stmt (expr);
      
      if (stmts_are_full_exprs_p ())
	expand_end_target_temps ();
    }
}

/* Generate the RTL for T, which is a DECL_STMT. */

void
genrtl_decl_stmt (t)
     tree t;
{
  tree decl;
  emit_line_note (input_filename, lineno);
  decl = DECL_STMT_DECL (t);
  /* If this is a declaration for an automatic local
     variable, initialize it.  Note that we might also see a
     declaration for a namespace-scope object (declared with
     `extern').  We don't have to handle the initialization
     of those objects here; they can only be declarations,
     rather than definitions.  */
  if (TREE_CODE (decl) == VAR_DECL 
      && !TREE_STATIC (decl)
      && !DECL_EXTERNAL (decl))
    {
      /* Let the back-end know about this variable.  */
      if (!anon_aggr_type_p (TREE_TYPE (decl)))
	emit_local_var (decl);
      else
	expand_anon_union_decl (decl, NULL_TREE, 
				DECL_ANON_UNION_ELEMS (decl));
    }
  else if (TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl))
    {
      if (DECL_ARTIFICIAL (decl) && ! TREE_USED (decl))
	/* Do not emit unused decls. This is not just an
	   optimization. We really do not want to emit
	   __PRETTY_FUNCTION__ etc, if they're never used.  */
	DECL_IGNORED_P (decl) = 1;
      else
	make_rtl_for_local_static (decl);
    }
}

/* Generate the RTL for T, which is an IF_STMT. */

void
genrtl_if_stmt (t)
     tree t;
{
  tree cond;
  genrtl_do_pushlevel ();
  cond = expand_cond (IF_COND (t));
  emit_line_note (input_filename, lineno);
  expand_start_cond (cond, 0);
  if (THEN_CLAUSE (t))
    expand_stmt (THEN_CLAUSE (t));
  if (ELSE_CLAUSE (t))
    {
      expand_start_else ();
      expand_stmt (ELSE_CLAUSE (t));
    }
  expand_end_cond ();
}

/* Generate the RTL for T, which is a WHILE_STMT. */

void
genrtl_while_stmt (t)
     tree t;
{
  tree cond;
  emit_nop ();
  emit_line_note (input_filename, lineno);
  expand_start_loop (1); 
  genrtl_do_pushlevel ();

  cond = expand_cond (WHILE_COND (t));
  emit_line_note (input_filename, lineno);
  expand_exit_loop_if_false (0, cond);
  genrtl_clear_out_block ();
  
  expand_stmt (WHILE_BODY (t));

  expand_end_loop ();
}

/* Generate the RTL for T, which is a DO_STMT. */

void
genrtl_do_stmt (t)
     tree t;
{
  tree cond;
  emit_nop ();
  emit_line_note (input_filename, lineno);
  expand_start_loop_continue_elsewhere (1);

  expand_stmt (DO_BODY (t));

  expand_loop_continue_here ();

  cond = expand_cond (DO_COND (t));
  emit_line_note (input_filename, lineno);
  expand_exit_loop_if_false (0, cond);
  expand_end_loop ();
}

/* Build the node for a return statement and return it. */

tree
build_return_stmt (expr)
     tree expr;
{
  return (build_stmt (RETURN_STMT, expr));
}

/* Generate the RTL for EXPR, which is a RETURN_STMT. */

void
genrtl_return_stmt (expr)
     tree expr;
{
  emit_line_note (input_filename, lineno);
  c_expand_return (expr);
}

/* Generate the RTL for T, which is a FOR_STMT. */

void
genrtl_for_stmt (t)
     tree t;
{
  tree tmp;
  tree cond;
  if (NEW_FOR_SCOPE_P (t))
    genrtl_do_pushlevel ();

  expand_stmt (FOR_INIT_STMT (t));

  emit_nop ();
  emit_line_note (input_filename, lineno);
  expand_start_loop_continue_elsewhere (1); 
  genrtl_do_pushlevel ();
  cond = expand_cond (FOR_COND (t));
  emit_line_note (input_filename, lineno);
  if (cond)
    expand_exit_loop_if_false (0, cond);
  genrtl_clear_out_block ();
  tmp = FOR_EXPR (t);

  expand_stmt (FOR_BODY (t));

  emit_line_note (input_filename, lineno);
  expand_loop_continue_here ();
  if (tmp) 
    genrtl_expr_stmt (tmp);
  expand_end_loop ();
}

/* Build a break statement node and return it. */

tree
build_break_stmt ()
{
  return (build_stmt (BREAK_STMT));
}

/* Generate the RTL for a BREAK_STMT. */

void
genrtl_break_stmt ()
{
  emit_line_note (input_filename, lineno);
  if ( ! expand_exit_something ())
    error ("break statement not within loop or switch");
}

/* Build a continue statement node and return it. */

tree
build_continue_stmt ()
{
  return (build_stmt (CONTINUE_STMT));
}

/* Generate the RTL for a CONTINUE_STMT. */

void
genrtl_continue_stmt ()
{
  emit_line_note (input_filename, lineno);
  if (! expand_continue_loop (0))
    error ("continue statement not within a loop");   
}

/* Generate the RTL for T, which is a SCOPE_STMT. */

void
genrtl_scope_stmt (t)
     tree t;
{
  if (!SCOPE_NO_CLEANUPS_P (t))
    {
      if (SCOPE_BEGIN_P (t))
	expand_start_bindings_and_block (2 * SCOPE_NULLIFIED_P (t),
					 SCOPE_STMT_BLOCK (t));
      else if (SCOPE_END_P (t))
	expand_end_bindings (NULL_TREE, !SCOPE_NULLIFIED_P (t), 0);
    }
  else if (!SCOPE_NULLIFIED_P (t))
    {
      rtx note = emit_note (NULL,
			    (SCOPE_BEGIN_P (t) 
			     ? NOTE_INSN_BLOCK_BEG
			     : NOTE_INSN_BLOCK_END));
      NOTE_BLOCK (note) = SCOPE_STMT_BLOCK (t);
    }
}

/* Generate the RTL for T, which is a SWITCH_STMT. */

void
genrtl_switch_stmt (t)
     tree t;
{
  tree cond;
  genrtl_do_pushlevel ();
 
  cond = expand_cond (SWITCH_COND (t));
  if (cond != error_mark_node)
    {
      emit_line_note (input_filename, lineno);
      c_expand_start_case (cond);
    }
  else
    /* The code is in error, but we don't want expand_end_case to
       crash. */
    c_expand_start_case (boolean_false_node);

  expand_stmt (SWITCH_BODY (t));

  expand_end_case (cond);
}

/* Create a CASE_LABEL tree node and return it. */

tree
build_case_label (low_value, high_value)
     tree low_value;
     tree high_value;
{
  return build_stmt (CASE_LABEL, low_value, high_value);
}


/* Generate the RTL for a CASE_LABEL. */

void 
genrtl_case_label (low_value, high_value)
     tree low_value;
     tree high_value;
{
  do_case (low_value, high_value);
}

/* Generate the RTL for T, which is a COMPOUND_STMT. */

void
genrtl_compound_stmt (t)
    tree t;
{
  /* If this is the outermost block of the function, declare the
     variables __FUNCTION__, __PRETTY_FUNCTION__, and so forth.  */
  if (cfun
      && !current_function_name_declared () 
      && !COMPOUND_STMT_NO_SCOPE (t))
    {
      set_current_function_name_declared (1);
      declare_function_name ();
    } 

  expand_stmt (COMPOUND_BODY (t));
}

/* Generate the RTL for an ASM_STMT. */

void
genrtl_asm_stmt (cv_qualifier, string, output_operands,
		 input_operands, clobbers)
     tree cv_qualifier;
     tree string;
     tree output_operands;
     tree input_operands;
     tree clobbers;
{
  if (TREE_CHAIN (string))
    string = combine_strings (string);

  if (cv_qualifier != NULL_TREE
      && cv_qualifier != ridpointers[(int) RID_VOLATILE])
    {
      warning ("%s qualifier ignored on asm",
	       IDENTIFIER_POINTER (cv_qualifier));
      cv_qualifier = NULL_TREE;
    }

  emit_line_note (input_filename, lineno);
  if (output_operands != NULL_TREE || input_operands != NULL_TREE
      || clobbers != NULL_TREE)
      c_expand_asm_operands (string, output_operands,
			     input_operands, 
			     clobbers,
			     cv_qualifier != NULL_TREE,
			     input_filename, lineno);
  else
    expand_asm (string);
}

/* Generate the RTL for a DECL_CLEANUP. */

void 
genrtl_decl_cleanup (decl, cleanup)
     tree decl;
     tree cleanup;
{
  if (!decl || (DECL_SIZE (decl) && TREE_TYPE (decl) != error_mark_node))
    expand_decl_cleanup (decl, cleanup);
}

/* Generate the RTL for the statement T, its substatements, and any
   other statements at its nesting level. */

tree
expand_stmt (t)
     tree t;
{
  tree rval;
  rval = lang_expand_stmt (t);
  return rval;
}

