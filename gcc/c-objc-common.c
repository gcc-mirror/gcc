/* Some code common to C and ObjC front ends.
   Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "insn-config.h"
#include "integrate.h"
#include "expr.h"
#include "c-tree.h"
#include "function.h"
#include "flags.h"
#include "toplev.h"
#include "diagnostic.h"
#include "tree-inline.h"
#include "varray.h"
#include "ggc.h"
#include "langhooks.h"
#include "tree-mudflap.h"
#include "target.h"
#include "cgraph.h"

static bool c_tree_printer (pretty_printer *, text_info *);

bool
c_missing_noreturn_ok_p (tree decl)
{
  /* A missing noreturn is not ok for freestanding implementations and
     ok for the `main' function in hosted implementations.  */
  return flag_hosted && MAIN_NAME_P (DECL_ASSEMBLER_NAME (decl));
}

/* We want to inline `extern inline' functions even if this would
   violate inlining limits.  Some glibc and linux constructs depend on
   such functions always being inlined when optimizing.  */

int
c_disregard_inline_limits (tree fn)
{
  if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (fn)) != NULL)
    return 1;

  return (!flag_really_no_inline && DECL_DECLARED_INLINE_P (fn)
	  && DECL_EXTERNAL (fn));
}

int
c_cannot_inline_tree_fn (tree *fnp)
{
  tree fn = *fnp;
  tree t;
  bool do_warning = (warn_inline
		     && DECL_INLINE (fn)
		     && DECL_DECLARED_INLINE_P (fn)
		     && !DECL_IN_SYSTEM_HEADER (fn));

  if (flag_really_no_inline
      && lookup_attribute ("always_inline", DECL_ATTRIBUTES (fn)) == NULL)
    {
      if (do_warning)
	warning ("%Jfunction '%F' can never be inlined because it "
		 "is suppressed using -fno-inline", fn, fn);
      goto cannot_inline;
    }

  /* Don't auto-inline anything that might not be bound within
     this unit of translation.  */
  if (!DECL_DECLARED_INLINE_P (fn) && !targetm.binds_local_p (fn))
    {
      if (do_warning)
	warning ("%Jfunction '%F' can never be inlined because it might not "
		 "be bound within this unit of translation", fn, fn);
      goto cannot_inline;
    }

  if (! function_attribute_inlinable_p (fn))
    {
      if (do_warning)
	warning ("%Jfunction '%F' can never be inlined because it uses "
		 "attributes conflicting with inlining", fn, fn);
      goto cannot_inline;
    }

  /* If a function has pending sizes, we must not defer its
     compilation, and we can't inline it as a tree.  */
  if (fn == current_function_decl)
    {
      t = get_pending_sizes ();
      put_pending_sizes (t);

      if (t)
	{
	  if (do_warning)
	    warning ("%Jfunction '%F' can never be inlined because it has "
		     "pending sizes", fn, fn);
	  goto cannot_inline;
	}
    }

  if (! DECL_FILE_SCOPE_P (fn))
    {
      /* If a nested function has pending sizes, we may have already
         saved them.  */
      if (DECL_LANG_SPECIFIC (fn)->pending_sizes)
	{
	  if (do_warning)
	    warning ("%Jnested function '%F' can never be inlined because it "
		     "has possibly saved pending sizes", fn, fn);
	  goto cannot_inline;
	}
    }

  return 0;

 cannot_inline:
  DECL_UNINLINABLE (fn) = 1;
  return 1;
}

/* Called from check_global_declarations.  */

bool
c_warn_unused_global_decl (tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (decl))
    return false;
  if (DECL_IN_SYSTEM_HEADER (decl))
    return false;

  return true;
}

/* Initialization common to C and Objective-C front ends.  */
bool
c_objc_common_init (void)
{
  static const enum tree_code stmt_codes[] = {
    c_common_stmt_codes
  };

  INIT_STATEMENT_CODES (stmt_codes);

  c_init_decl_processing ();

  if (c_common_init () == false)
    return false;

  /* These were not defined in the Objective-C front end, but I'm
     putting them here anyway.  The diagnostic format decoder might
     want an enhanced ObjC implementation.  */
  diagnostic_format_decoder (global_dc) = &c_tree_printer;

  /* If still unspecified, make it match -std=c99
     (allowing for -pedantic-errors).  */
  if (mesg_implicit_function_declaration < 0)
    {
      if (flag_isoc99)
	mesg_implicit_function_declaration = flag_pedantic_errors ? 2 : 1;
      else
	mesg_implicit_function_declaration = 0;
    }

  return true;
}

/* Synthesize a function which calls all the global ctors or global dtors
   in this file.  */
static void
build_cdtor (int method_type, tree cdtors)
{
  tree fnname = get_file_function_name (method_type);
  tree body;
  tree scope;
  tree block;

  start_function (void_list_node,
		  build_nt (CALL_EXPR, fnname,
			    tree_cons (NULL_TREE, NULL_TREE, void_list_node),
			    NULL_TREE),
		  NULL_TREE);
  store_parm_decls ();

  body = c_begin_compound_stmt ();
  add_scope_stmt (/*begin_p=*/1, /*partial_p=*/0);

  for (; cdtors; cdtors = TREE_CHAIN (cdtors))
    add_stmt (build_stmt (EXPR_STMT,
			  build_function_call (TREE_VALUE (cdtors), 0)));

  scope = add_scope_stmt (/*begin_p=*/0, /*partial_p=*/0);

  block = make_node (BLOCK);
  SCOPE_STMT_BLOCK (TREE_PURPOSE (scope)) = block;
  SCOPE_STMT_BLOCK (TREE_VALUE (scope)) = block;

  RECHAIN_STMTS (body, COMPOUND_BODY (body));

  finish_function ();
}

/* Called at end of parsing, but before end-of-file processing.  */

void
c_objc_common_finish_file (void)
{
  if (pch_file)
    c_common_write_pch ();

  if (static_ctors)
    {
      build_cdtor ('I', static_ctors);
      static_ctors = 0;
    }
  if (static_dtors)
    {
      build_cdtor ('D', static_dtors);
      static_dtors = 0;
    }

  cgraph_finalize_compilation_unit ();
  cgraph_optimize ();

  if (flag_mudflap)
    mudflap_finish_file ();
}

/* Called during diagnostic message formatting process to print a
   source-level entity onto BUFFER.  The meaning of the format specifiers
   is as follows:
   %D: a general decl,
   %E: An expression,
   %F: a function declaration,
   %T: a type.

   These format specifiers form a subset of the format specifiers set used
   by the C++ front-end.
   Please notice when called, the `%' part was already skipped by the
   diagnostic machinery.  */
static bool
c_tree_printer (pretty_printer *pp, text_info *text)
{
  tree t = va_arg (*text->args_ptr, tree);
  const char *n = "({anonymous})";

  switch (*text->format_spec)
    {
    case 'D':
    case 'F':
      if (DECL_NAME (t))
	n = lang_hooks.decl_printable_name (t, 2);
      break;

    case 'T':
      if (TREE_CODE (t) == TYPE_DECL)
	{
	  if (DECL_NAME (t))
	    n = lang_hooks.decl_printable_name (t, 2);
	}
      else
	{
	  t = TYPE_NAME (t);
	  if (t)
	    n = IDENTIFIER_POINTER (t);
	}
      break;

    case 'E':
      if (TREE_CODE (t) == IDENTIFIER_NODE)
	n = IDENTIFIER_POINTER (t);
      else
        return false;
      break;

    default:
      return false;
    }

  pp_string (pp, n);
  return true;
}

tree
c_objc_common_truthvalue_conversion (tree expr)
{
 retry:
  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case ARRAY_TYPE:
      expr = default_conversion (expr);
      if (TREE_CODE (TREE_TYPE (expr)) != ARRAY_TYPE)
	goto retry;

      error ("used array that cannot be converted to pointer where scalar is required");
      return error_mark_node;

    case RECORD_TYPE:
      error ("used struct type value where scalar is required");
      return error_mark_node;

    case UNION_TYPE:
      error ("used union type value where scalar is required");
      return error_mark_node;
    default:
      break;
    }

  return c_common_truthvalue_conversion (expr);
}

