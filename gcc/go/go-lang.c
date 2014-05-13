/* go-lang.c -- Go frontend gcc interface.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.

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
#include "basic-block.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "stor-layout.h"
#include "toplev.h"
#include "debug.h"
#include "options.h"
#include "flags.h"
#include "convert.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "target.h"
#include "common/common-target.h"

#include <mpfr.h>

#include "go-c.h"

/* Language-dependent contents of a type.  */

struct GTY(()) lang_type
{
  char dummy;
};

/* Language-dependent contents of a decl.  */

struct GTY((variable_size)) lang_decl
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
	   chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
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

/* Option information we need to pass to go_create_gogo.  */

static const char *go_pkgpath = NULL;
static const char *go_prefix = NULL;
static const char *go_relative_import_path = NULL;

/* Language hooks.  */

static bool
go_langhook_init (void)
{
  build_common_tree_nodes (false, false);

  /* I don't know why this has to be done explicitly.  */
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  /* We must create the gogo IR after calling build_common_tree_nodes
     (because Gogo::define_builtin_function_trees refers indirectly
     to, e.g., unsigned_char_type_node) but before calling
     build_common_builtin_nodes (because it calls, indirectly,
     go_type_for_size).  */
  go_create_gogo (INT_TYPE_SIZE, POINTER_SIZE, go_pkgpath, go_prefix,
		  go_relative_import_path, go_check_divide_zero,
		  go_check_divide_overflow);

  build_common_builtin_nodes ();

  /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may
     eventually be controllable by a command line option.  */
  mpfr_set_default_prec (256);

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
  opts->frontend_set_flag_errno_math = true;

  /* We turn on stack splitting if we can.  */
  if (targetm_common.supports_split_stack (false, opts))
    opts->x_flag_split_stack = 1;

  /* Exceptions are used to handle recovering from panics.  */
  opts->x_flag_exceptions = 1;
  opts->x_flag_non_call_exceptions = 1;
}

/* Infrastructure for a vector of char * pointers.  */

typedef const char *go_char_p;

/* The list of directories to search after all the Go specific
   directories have been searched.  */

static vec<go_char_p> go_search_dirs;

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
      go_add_search_path (arg);
      break;

    case OPT_L:
      /* A -L option is assumed to come from the compiler driver.
	 This is a system directory.  We search the following
	 directories, if they exist, before this one:
	   dir/go/VERSION
	   dir/go/VERSION/MACHINE
	 This is like include/c++.  */
      {
	static const char dir_separator_str[] = { DIR_SEPARATOR, 0 };
	size_t len;
	char *p;
	struct stat st;

	len = strlen (arg);
	p = XALLOCAVEC (char,
			(len + sizeof "go" + sizeof DEFAULT_TARGET_VERSION
			 + sizeof DEFAULT_TARGET_MACHINE + 3));
	strcpy (p, arg);
	if (len > 0 && !IS_DIR_SEPARATOR (p[len - 1]))
	  strcat (p, dir_separator_str);
	strcat (p, "go");
	strcat (p, dir_separator_str);
	strcat (p, DEFAULT_TARGET_VERSION);
	if (stat (p, &st) == 0 && S_ISDIR (st.st_mode))
	  {
	    go_add_search_path (p);
	    strcat (p, dir_separator_str);
	    strcat (p, DEFAULT_TARGET_MACHINE);
	    if (stat (p, &st) == 0 && S_ISDIR (st.st_mode))
	      go_add_search_path (p);
	  }

	/* Search ARG too, but only after we've searched to Go
	   specific directories for all -L arguments.  */
	go_search_dirs.safe_push (arg);
      }
      break;

    case OPT_fgo_dump_:
      ret = go_enable_dump (arg) ? true : false;
      break;

    case OPT_fgo_optimize_:
      ret = go_enable_optimize (arg) ? true : false;
      break;

    case OPT_fgo_pkgpath_:
      go_pkgpath = arg;
      break;

    case OPT_fgo_prefix_:
      go_prefix = arg;
      break;

    case OPT_fgo_relative_import_path_:
      go_relative_import_path = arg;
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
  unsigned int ix;
  const char *dir;

  gcc_assert (num_in_fnames > 0);

  FOR_EACH_VEC_ELT (go_search_dirs, ix, dir)
    go_add_search_path (dir);
  go_search_dirs.release ();

  if (flag_excess_precision_cmdline == EXCESS_PRECISION_DEFAULT)
    flag_excess_precision_cmdline = EXCESS_PRECISION_STANDARD;

  /* Tail call optimizations can confuse uses of runtime.Callers.  */
  if (!global_options_set.x_flag_optimize_sibling_calls)
    global_options.x_flag_optimize_sibling_calls = 0;

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
  tree type;
  if (unsignedp)
    {
      if (bits == INT_TYPE_SIZE)
        type = unsigned_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        type = unsigned_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        type = short_unsigned_type_node;
      else if (bits == LONG_TYPE_SIZE)
        type = long_unsigned_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        type = long_long_unsigned_type_node;
      else
        type = make_unsigned_type(bits);
    }
  else
    {
      if (bits == INT_TYPE_SIZE)
        type = integer_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        type = signed_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        type = short_integer_type_node;
      else if (bits == LONG_TYPE_SIZE)
        type = long_integer_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        type = long_long_integer_type_node;
      else
        type = make_signed_type(bits);
    }
  return type;
}

static tree
go_langhook_type_for_mode (enum machine_mode mode, int unsignedp)
{
  tree type;
  /* Go has no vector types.  Build them here.  FIXME: It does not
     make sense for the middle-end to ask the frontend for a type
     which the frontend does not support.  However, at least for now
     it is required.  See PR 46805.  */
  if (VECTOR_MODE_P (mode))
    {
      tree inner;

      inner = go_langhook_type_for_mode (GET_MODE_INNER (mode), unsignedp);
      if (inner != NULL_TREE)
	return build_vector_type_for_mode (inner, mode);
      return NULL_TREE;
    }

  // FIXME: This static_cast should be in machmode.h.
  enum mode_class mc = static_cast<enum mode_class>(GET_MODE_CLASS(mode));
  if (mc == MODE_INT)
    return go_langhook_type_for_size(GET_MODE_BITSIZE(mode), unsignedp);
  else if (mc == MODE_FLOAT)
    {
      switch (GET_MODE_BITSIZE (mode))
	{
	case 32:
	  return float_type_node;
	case 64:
	  return double_type_node;
	default:
	  // We have to check for long double in order to support
	  // i386 excess precision.
	  if (mode == TYPE_MODE(long_double_type_node))
	    return long_double_type_node;
	}
    }
  else if (mc == MODE_COMPLEX_FLOAT)
    {
      switch (GET_MODE_BITSIZE (mode))
	{
	case 64:
	  return complex_float_type_node;
	case 128:
	  return complex_double_type_node;
	default:
	  // We have to check for long double in order to support
	  // i386 excess precision.
	  if (mode == TYPE_MODE(complex_long_double_type_node))
	    return complex_long_double_type_node;
	}
    }

#if HOST_BITS_PER_WIDE_INT >= 64
  /* The middle-end and some backends rely on TImode being supported
     for 64-bit HWI.  */
  if (mode == TImode)
    {
      type = build_nonstandard_integer_type (GET_MODE_BITSIZE (TImode),
					     unsignedp);
      if (type && TYPE_MODE (type) == TImode)
	return type;
    }
#endif
  return NULL_TREE;
}

/* Record a builtin function.  We just ignore builtin functions.  */

static tree
go_langhook_builtin_function (tree decl)
{
  return decl;
}

/* Return true if we are in the global binding level.  */

static bool
go_langhook_global_bindings_p (void)
{
  return current_function_decl == NULL_TREE;
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
