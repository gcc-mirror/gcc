/* Language-specific hook definitions for C front end.
   Copyright (C) 1991, 1995, 1997, 1998,
   1999, 2000, 2001 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "tree.h"
#include "tree-inline.h"
#include "function.h"
#include "input.h"
#include "toplev.h"
#include "diagnostic.h"
#include "output.h"
#include "flags.h"
#include "ggc.h"
#include "rtl.h"
#include "expr.h"
#include "c-tree.h"
#include "c-lex.h"
#include "cpplib.h"
#include "insn-config.h"
#include "integrate.h"
#include "varray.h"
#include "langhooks.h"
#include "langhooks-def.h"

static int c_tree_printer PARAMS ((output_buffer *));
static int c_missing_noreturn_ok_p PARAMS ((tree));
static const char *c_init PARAMS ((const char *));
static void c_init_options PARAMS ((void));
static void c_post_options PARAMS ((void));
static int c_disregard_inline_limits PARAMS ((tree));
static int c_cannot_inline_tree_fn PARAMS ((tree *));

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU C"
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT c_init
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS c_init_options
#undef LANG_HOOKS_DECODE_OPTION
#define LANG_HOOKS_DECODE_OPTION c_decode_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS c_post_options
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET c_common_get_alias_set
#undef LANG_HOOKS_PRINT_IDENTIFIER
#define LANG_HOOKS_PRINT_IDENTIFIER c_print_identifier
#undef LANG_HOOKS_SET_YYDEBUG
#define LANG_HOOKS_SET_YYDEBUG c_set_yydebug

#undef LANG_HOOKS_TREE_INLINING_CANNOT_INLINE_TREE_FN
#define LANG_HOOKS_TREE_INLINING_CANNOT_INLINE_TREE_FN \
  c_cannot_inline_tree_fn
#undef LANG_HOOKS_TREE_INLINING_DISREGARD_INLINE_LIMITS
#define LANG_HOOKS_TREE_INLINING_DISREGARD_INLINE_LIMITS \
  c_disregard_inline_limits
#undef LANG_HOOKS_TREE_INLINING_ANON_AGGR_TYPE_P
#define LANG_HOOKS_TREE_INLINING_ANON_AGGR_TYPE_P \
  anon_aggr_type_p

/* Each front end provides its own.  */
const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

static varray_type deferred_fns;

/* Post-switch processing.  */
static void
c_post_options ()
{
  cpp_post_options (parse_in);

  /* Use tree inlining if possible.  Function instrumentation is only
     done in the RTL level, so we disable tree inlining.  */
  if (! flag_instrument_function_entry_exit)
    {
      if (!flag_no_inline)
	{
	  flag_inline_trees = 1;
	  flag_no_inline = 1;
	}
      if (flag_inline_functions)
	{
	  flag_inline_trees = 2;
	  flag_inline_functions = 0;
	}
    }
}

static void
c_init_options ()
{
  parse_in = cpp_create_reader (CLK_GNUC89);

  /* Mark as "unspecified".  */
  flag_bounds_check = -1;
}

static const char *
c_init (filename)
     const char *filename;
{
  c_init_decl_processing ();

  filename = c_common_lang_init (filename);

  add_c_tree_codes ();

  /* If still unspecified, make it match -std=c99
     (allowing for -pedantic-errors).  */
  if (mesg_implicit_function_declaration < 0)
    {
      if (flag_isoc99)
	mesg_implicit_function_declaration = flag_pedantic_errors ? 2 : 1;
      else
	mesg_implicit_function_declaration = 0;
    }

  save_lang_status = &push_c_function_context;
  restore_lang_status = &pop_c_function_context;
  mark_lang_status = &mark_c_function_context;
  lang_expand_expr = &c_expand_expr;
  lang_safe_from_p = &c_safe_from_p;
  diagnostic_format_decoder (global_dc) = &c_tree_printer;
  lang_expand_decl_stmt = &c_expand_decl_stmt;
  lang_missing_noreturn_ok_p = &c_missing_noreturn_ok_p;

  VARRAY_TREE_INIT (deferred_fns, 32, "deferred_fns");
  ggc_add_tree_varray_root (&deferred_fns, 1);

  return filename;
}

/* Used by c-lex.c, but only for objc.  */

tree
lookup_interface (arg)
     tree arg ATTRIBUTE_UNUSED;
{
  return 0;
}

tree
is_class_name (arg)
    tree arg ATTRIBUTE_UNUSED;
{
  return 0;
}

void
maybe_objc_check_decl (decl)
     tree decl ATTRIBUTE_UNUSED;
{
}

int
maybe_objc_comptypes (lhs, rhs, reflexive)
     tree lhs ATTRIBUTE_UNUSED;
     tree rhs ATTRIBUTE_UNUSED;
     int reflexive ATTRIBUTE_UNUSED;
{
  return -1;
}

tree
maybe_building_objc_message_expr ()
{
  return 0;
}

int
recognize_objc_keyword ()
{
  return 0;
}

/* Used by c-typeck.c (build_external_ref), but only for objc.  */

tree
lookup_objc_ivar (id)
     tree id ATTRIBUTE_UNUSED;
{
  return 0;
}

#if !defined(ASM_OUTPUT_CONSTRUCTOR) || !defined(ASM_OUTPUT_DESTRUCTOR)
extern tree static_ctors;
extern tree static_dtors;

static tree start_cdtor		PARAMS ((int));
static void finish_cdtor	PARAMS ((tree));

static tree
start_cdtor (method_type)
     int method_type;
{
  tree fnname = get_file_function_name (method_type);
  tree void_list_node_1 = build_tree_list (NULL_TREE, void_type_node);
  tree body;

  start_function (void_list_node_1,
		  build_nt (CALL_EXPR, fnname,
			    tree_cons (NULL_TREE, NULL_TREE, void_list_node_1),
			    NULL_TREE),
		  NULL_TREE);
  store_parm_decls ();

  current_function_cannot_inline
    = "static constructors and destructors cannot be inlined";

  body = c_begin_compound_stmt ();

  pushlevel (0);
  clear_last_expr ();
  add_scope_stmt (/*begin_p=*/1, /*partial_p=*/0);

  return body;
}

static void
finish_cdtor (body)
     tree body;
{
  tree scope;
  tree block;

  scope = add_scope_stmt (/*begin_p=*/0, /*partial_p=*/0);
  block = poplevel (0, 0, 0);
  SCOPE_STMT_BLOCK (TREE_PURPOSE (scope)) = block;
  SCOPE_STMT_BLOCK (TREE_VALUE (scope)) = block;

  RECHAIN_STMTS (body, COMPOUND_BODY (body));

  finish_function (0);
}
#endif

/* Register a function tree, so that its optimization and conversion
   to RTL is only done at the end of the compilation.  */

int
defer_fn (fn)
     tree fn;
{
  VARRAY_PUSH_TREE (deferred_fns, fn);

  return 1;
}

/* Called at end of parsing, but before end-of-file processing.  */

void
finish_file ()
{
  unsigned int i;
  bool reconsider;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (deferred_fns); i++)
    {
      tree decl = VARRAY_TREE (deferred_fns, i);

      if (! TREE_ASM_WRITTEN (decl) && TREE_PUBLIC (decl))
	{
	  c_expand_deferred_function (decl);
	  VARRAY_TREE (deferred_fns, i) = NULL;
	}
    }

  do
    {
      reconsider = false;
      for (i = 0; i < VARRAY_ACTIVE_SIZE (deferred_fns); i++)
	{
	  tree decl = VARRAY_TREE (deferred_fns, i);

	  if (decl
	      && ! TREE_ASM_WRITTEN (decl)
	      && (flag_keep_inline_functions
		  || TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))))
	    {
	      c_expand_deferred_function (decl);
	      VARRAY_TREE (deferred_fns, i) = NULL;
	      reconsider = true;
	    }
	}
    } while (reconsider);

  VARRAY_FREE (deferred_fns);

#ifndef ASM_OUTPUT_CONSTRUCTOR
  if (static_ctors)
    {
      tree body = start_cdtor ('I');

      for (; static_ctors; static_ctors = TREE_CHAIN (static_ctors))
	c_expand_expr_stmt (build_function_call (TREE_VALUE (static_ctors),
						 NULL_TREE));

      finish_cdtor (body);
    }
#endif
#ifndef ASM_OUTPUT_DESTRUCTOR
  if (static_dtors)
    {
      tree body = start_cdtor ('D');

      for (; static_dtors; static_dtors = TREE_CHAIN (static_dtors))
	c_expand_expr_stmt (build_function_call (TREE_VALUE (static_dtors),
						 NULL_TREE));

      finish_cdtor (body);
    }
#endif

  if (back_end_hook)
    (*back_end_hook) (getdecls ());
  
  {
    int flags;
    FILE *stream = dump_begin (TDI_all, &flags);

    if (stream)
      {
	dump_node (getdecls (), flags & ~TDF_SLIM, stream);
	dump_end (TDI_all, stream);
      }
  }
}

/* Called during diagnostic message formatting process to print a
   source-level entity onto BUFFER.  The meaning of the format specifiers
   is as follows:
   %D: a general decl,
   %F: a function declaration,
   %T: a type.

   These format specifiers form a subset of the format specifiers set used
   by the C++ front-end.
   Please notice when called, the `%' part was already skipped by the
   diagnostic machinery.  */
static int
c_tree_printer (buffer)
     output_buffer *buffer;
{
  tree t = va_arg (output_buffer_format_args (buffer), tree);

  switch (*output_buffer_text_cursor (buffer))
    {
    case 'D':
    case 'F':
    case 'T':
      {
        const char *n = DECL_NAME (t)
          ? (*decl_printable_name) (t, 2)
          : "({anonymous})";
        output_add_string (buffer, n);
      }
      return 1;

    default:
      return 0;
    }
}

static int
c_missing_noreturn_ok_p (decl)
     tree decl;
{
  /* A missing noreturn is not ok for freestanding implementations and
     ok for the `main' function in hosted implementations.  */
  return flag_hosted && MAIN_NAME_P (DECL_ASSEMBLER_NAME (decl));
}

/* We want to inline `extern inline' functions even if this would
   violate inlining limits.  Some glibc and linux constructs depend on
   such functions always being inlined when optimizing.  */

static int
c_disregard_inline_limits (fn)
     tree fn;
{
  return DECL_DECLARED_INLINE_P (fn) && DECL_EXTERNAL (fn);
}

static tree inline_forbidden_p PARAMS ((tree *, int *, void *));

static tree
inline_forbidden_p (nodep, walk_subtrees, fn)
     tree *nodep;
     int *walk_subtrees ATTRIBUTE_UNUSED;
     void *fn;
{
  tree node = *nodep;
  tree t;

  switch (TREE_CODE (node))
    {
    case CALL_EXPR:
      t = get_callee_fndecl (node);

      if (! t)
	break;

      /* We cannot inline functions that call setjmp.  */
      if (setjmp_call_p (t))
	return node;

      switch (DECL_FUNCTION_CODE (t))
	{
	  /* We cannot inline functions that take a variable number of
	     arguments.  */
	case BUILT_IN_VARARGS_START:
	case BUILT_IN_STDARG_START:
#if 0
	  /* Functions that need information about the address of the
             caller can't (shouldn't?) be inlined.  */
	case BUILT_IN_RETURN_ADDRESS:
#endif
	  return node;

	default:
	  break;
	}

      break;

    case DECL_STMT:
      /* We cannot inline functions that contain other functions.  */
      if (TREE_CODE (TREE_OPERAND (node, 0)) == FUNCTION_DECL
	  && DECL_INITIAL (TREE_OPERAND (node, 0)))
	return node;
      break;

    case GOTO_STMT:
    case GOTO_EXPR:
      t = TREE_OPERAND (node, 0);

      /* We will not inline a function which uses computed goto.  The
	 addresses of its local labels, which may be tucked into
	 global storage, are of course not constant across
	 instantiations, which causes unexpected behaviour.  */
      if (TREE_CODE (t) != LABEL_DECL)
	return node;

      /* We cannot inline a nested function that jumps to a nonlocal
         label.  */
      if (TREE_CODE (t) == LABEL_DECL
	  && DECL_CONTEXT (t) && DECL_CONTEXT (t) != fn)
	return node;

      break;

    default:
      break;
    }

  return NULL_TREE;
}

static int
c_cannot_inline_tree_fn (fnp)
     tree *fnp;
{
  tree fn = *fnp;
  tree t;

  if (! function_attribute_inlinable_p (fn))
    {
      DECL_UNINLINABLE (fn) = 1;
      return 1;
    }

  /* If a function has pending sizes, we must not defer its
     compilation, and we can't inline it as a tree.  */
  if (fn == current_function_decl)
    {
      t = get_pending_sizes ();
      put_pending_sizes (t);

      if (t)
	{
	  DECL_UNINLINABLE (fn) = 1;
	  return 1;
	}
    }

  if (DECL_CONTEXT (fn))
    {
      /* If a nested function has pending sizes, we may have already
         saved them.  */
      if (DECL_LANG_SPECIFIC (fn)->pending_sizes)
	{
	  DECL_UNINLINABLE (fn) = 1;
	  return 1;
	}
    }
  else
    {
      /* We rely on the fact that this function is called upfront,
	 just before we start expanding a function.  If FN is active
	 (i.e., it's the current_function_decl or a parent thereof),
	 we have to walk FN's saved tree.  Otherwise, we can safely
	 assume we have done it before and, if we didn't mark it as
	 uninlinable (in which case we wouldn't have been called), it
	 is inlinable.  Unfortunately, this strategy doesn't work for
	 nested functions, because they're only expanded as part of
	 their enclosing functions, so the inlinability test comes in
	 late.  */
      t = current_function_decl;

      while (t && t != fn)
	t = DECL_CONTEXT (t);
      if (! t)
	return 0;
    }
    
  if (walk_tree (&DECL_SAVED_TREE (fn), inline_forbidden_p, fn, NULL))
    {
      DECL_UNINLINABLE (fn) = 1;
      return 1;
    }

  return 0;
}
