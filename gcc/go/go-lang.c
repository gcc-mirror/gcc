/* go-lang.c -- Go frontend gcc interface.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.

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
#include "ansidecl.h"
#include "coretypes.h"
#include "opts.h"
#include "tree.h"
#include "gimple.h"
#include "ggc.h"
#include "toplev.h"
#include "debug.h"
#include "options.h"
#include "flags.h"
#include "convert.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "except.h"
#include "target.h"

#include <mpfr.h>

#include "go-c.h"

/* Language-dependent contents of a type.  */

struct GTY(()) lang_type
{
  char dummy;
};

/* Language-dependent contents of a decl.  */

struct GTY(()) lang_decl
{
  char dummy;
};

/* Language-dependent contents of an identifier.  This must include a
   tree_identifier.  */

struct GTY(()) lang_identifier
{
  struct tree_identifier common;
};

/* The resulting tree type.  */

union GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
	   chain_next ("(union lang_tree_node *) TREE_CHAIN (&%h.generic)")))
lang_tree_node
{
  union tree_node GTY((tag ("0"),
		       desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY((tag ("1"))) identifier;
};

/* We don't use language_function.  */

struct GTY(()) language_function
{
  int dummy;
};

/* Language hooks.  */

static bool
go_langhook_init (void)
{
  build_common_tree_nodes (false);

  /* The sizetype may be "unsigned long" or "unsigned long long".  */
  if (TYPE_MODE (long_unsigned_type_node) == ptr_mode)
    size_type_node = long_unsigned_type_node;
  else if (TYPE_MODE (long_long_unsigned_type_node) == ptr_mode)
    size_type_node = long_long_unsigned_type_node;
  else
    size_type_node = long_unsigned_type_node;
  set_sizetype (size_type_node);

  build_common_tree_nodes_2 (0);

  /* We must create the gogo IR after calling build_common_tree_nodes
     (because Gogo::define_builtin_function_trees refers indirectly
     to, e.g., unsigned_char_type_node) but before calling
     build_common_builtin_nodes (because it calls, indirectly,
     go_type_for_size).  */
  go_create_gogo (INT_TYPE_SIZE, FLOAT_TYPE_SIZE, POINTER_SIZE);

  build_common_builtin_nodes ();

  /* I don't know why this is not done by any of the above.  */
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may
     eventually be controllable by a command line option.  */
  mpfr_set_default_prec (128);

  /* Go uses exceptions.  */
  using_eh_for_cleanups ();

  return true;
}

/* The option mask.  */

static unsigned int
go_langhook_option_lang_mask (void)
{
  return CL_Go;
}

/* Initialize the options structure.  */

static void
go_langhook_init_options_struct (struct gcc_options *opts)
{
  /* Go says that signed overflow is precisely defined.  */
  opts->x_flag_wrapv = 1;

  /* We default to using strict aliasing, since Go pointers are safe.
     This is turned off for code that imports the "unsafe" package,
     because using unsafe.pointer violates C style aliasing
     requirements.  */
  opts->x_flag_strict_aliasing = 1;

  /* Default to avoiding range issues for complex multiply and
     divide.  */
  opts->x_flag_complex_method = 2;

  /* The builtin math functions should not set errno.  */
  opts->x_flag_errno_math = 0;

  /* By default assume that floating point math does not trap.  */
  opts->x_flag_trapping_math = 0;

  /* We turn on stack splitting if we can.  */
  if (targetm.supports_split_stack (false, opts))
    opts->x_flag_split_stack = 1;

  /* Exceptions are used to handle recovering from panics.  */
  opts->x_flag_exceptions = 1;
  opts->x_flag_non_call_exceptions = 1;
}

/* Handle Go specific options.  Return 0 if we didn't do anything.  */

static bool
go_langhook_handle_option (
    size_t scode,
    const char *arg,
    int value ATTRIBUTE_UNUSED,
    int kind ATTRIBUTE_UNUSED,
    location_t loc ATTRIBUTE_UNUSED,
    const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  enum opt_code code = (enum opt_code) scode;
  bool ret = true;

  switch (code)
    {
    case OPT_I:
    case OPT_L:
      /* For the compiler, we currently handle -I and -L exactly the
	 same way: they give us a directory to search for import
	 statements.  */
      go_add_search_path (arg);
      break;

    case OPT_fgo_dump_:
      ret = go_enable_dump (arg) ? true : false;
      break;

    case OPT_fgo_prefix_:
      go_set_prefix (arg);
      break;

    default:
      /* Just return 1 to indicate that the option is valid.  */
      break;
    }

  return ret;
}

/* Run after parsing options.  */

static bool
go_langhook_post_options (const char **pfilename ATTRIBUTE_UNUSED)
{
  gcc_assert (num_in_fnames > 0);

  if (flag_excess_precision_cmdline == EXCESS_PRECISION_DEFAULT)
    flag_excess_precision_cmdline = EXCESS_PRECISION_STANDARD;

  /* Returning false means that the backend should be used.  */
  return false;
}

static void
go_langhook_parse_file (void)
{
  go_parse_input_files (in_fnames, num_in_fnames, flag_syntax_only,
			go_require_return_statement);
}

static tree
go_langhook_type_for_size (unsigned int bits, int unsignedp)
{
  return go_type_for_size (bits, unsignedp);
}

static tree
go_langhook_type_for_mode (enum machine_mode mode, int unsignedp)
{
  return go_type_for_mode (mode, unsignedp);
}

/* Record a builtin function.  We just ignore builtin functions.  */

static tree
go_langhook_builtin_function (tree decl)
{
  return decl;
}

static int
go_langhook_global_bindings_p (void)
{
  return current_function_decl == NULL ? 1 : 0;
}

/* Push a declaration into the current binding level.  We can't
   usefully implement this since we don't want to convert from tree
   back to one of our internal data structures.  I think the only way
   this is used is to record a decl which is to be returned by
   getdecls, and we could implement it for that purpose if
   necessary.  */

static tree
go_langhook_pushdecl (tree decl ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This hook is used to get the current list of declarations as trees.
   We don't support that; instead we use the write_globals hook.  This
   can't simply crash because it is called by -gstabs.  */

static tree
go_langhook_getdecls (void)
{
  return NULL;
}

/* Write out globals.  */

static void
go_langhook_write_globals (void)
{
  go_write_globals ();
}

/* Go specific gimplification.  We need to gimplify
   CALL_EXPR_STATIC_CHAIN, because the gimplifier doesn't handle
   it.  */

static int
go_langhook_gimplify_expr (tree *expr_p, gimple_seq *pre_p, gimple_seq *post_p)
{
  if (TREE_CODE (*expr_p) == CALL_EXPR
      && CALL_EXPR_STATIC_CHAIN (*expr_p) != NULL_TREE)
    gimplify_expr (&CALL_EXPR_STATIC_CHAIN (*expr_p), pre_p, post_p,
		   is_gimple_val, fb_rvalue);
  return GS_UNHANDLED;
}

/* Return a decl for the exception personality function.  The function
   itself is implemented in libgo/runtime/go-unwind.c.  */

static tree
go_langhook_eh_personality (void)
{
  static tree personality_decl;
  if (personality_decl == NULL_TREE)
    {
      personality_decl = build_personality_function ("gccgo");
      go_preserve_from_gc (personality_decl);
    }
  return personality_decl;
}

/* Functions called directly by the generic backend.  */

tree
convert (tree type, tree expr)
{
  if (type == error_mark_node
      || expr == error_mark_node
      || TREE_TYPE (expr) == error_mark_node)
    return error_mark_node;

  if (type == TREE_TYPE (expr))
    return expr;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold_convert (type, expr);

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case BOOLEAN_TYPE:
      return fold_convert (type, expr);
    case INTEGER_TYPE:
      return fold (convert_to_integer (type, expr));
    case POINTER_TYPE:
      return fold (convert_to_pointer (type, expr));
    case REAL_TYPE:
      return fold (convert_to_real (type, expr));
    case COMPLEX_TYPE:
      return fold (convert_to_complex (type, expr));
    default:
      break;
    }

  gcc_unreachable ();
}

/* FIXME: This is a hack to preserve trees that we create from the
   garbage collector.  */

static GTY(()) tree go_gc_root;

void
go_preserve_from_gc (tree t)
{
  go_gc_root = tree_cons (NULL_TREE, t, go_gc_root);
}

/* Convert an identifier for use in an error message.  */

const char *
go_localize_identifier (const char *ident)
{
  return identifier_to_locale (ident);
}

#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_OPTION_LANG_MASK
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#undef LANG_HOOKS_PUSHDECL
#undef LANG_HOOKS_GETDECLS
#undef LANG_HOOKS_WRITE_GLOBALS
#undef LANG_HOOKS_GIMPLIFY_EXPR
#undef LANG_HOOKS_EH_PERSONALITY

#define LANG_HOOKS_NAME			"GNU Go"
#define LANG_HOOKS_INIT			go_langhook_init
#define LANG_HOOKS_OPTION_LANG_MASK	go_langhook_option_lang_mask
#define LANG_HOOKS_INIT_OPTIONS_STRUCT	go_langhook_init_options_struct
#define LANG_HOOKS_HANDLE_OPTION	go_langhook_handle_option
#define LANG_HOOKS_POST_OPTIONS		go_langhook_post_options
#define LANG_HOOKS_PARSE_FILE		go_langhook_parse_file
#define LANG_HOOKS_TYPE_FOR_MODE	go_langhook_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE	go_langhook_type_for_size
#define LANG_HOOKS_BUILTIN_FUNCTION	go_langhook_builtin_function
#define LANG_HOOKS_GLOBAL_BINDINGS_P	go_langhook_global_bindings_p
#define LANG_HOOKS_PUSHDECL		go_langhook_pushdecl
#define LANG_HOOKS_GETDECLS		go_langhook_getdecls
#define LANG_HOOKS_WRITE_GLOBALS	go_langhook_write_globals
#define LANG_HOOKS_GIMPLIFY_EXPR	go_langhook_gimplify_expr
#define LANG_HOOKS_EH_PERSONALITY	go_langhook_eh_personality

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-go-go-lang.h"
#include "gtype-go.h"
