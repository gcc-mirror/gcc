/* jit.c -- Dummy "frontend" for use during JIT-compilation.
   Copyright (C) 2013-2021 Free Software Foundation, Inc.

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
#include "coretypes.h"
#include "jit-playback.h"
#include "stor-layout.h"
#include "debug.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "diagnostic.h"
#include "options.h"
#include "stringpool.h"
#include "attribs.h"

#include <mpfr.h>

/* Attribute handling.  */

static tree handle_noreturn_attribute (tree *, tree, tree, int, bool *);
static tree handle_leaf_attribute (tree *, tree, tree, int, bool *);
static tree handle_const_attribute (tree *, tree, tree, int, bool *);
static tree handle_malloc_attribute (tree *, tree, tree, int, bool *);
static tree handle_pure_attribute (tree *, tree, tree, int, bool *);
static tree handle_novops_attribute (tree *, tree, tree, int, bool *);
static tree handle_nonnull_attribute (tree *, tree, tree, int, bool *);
static tree handle_nothrow_attribute (tree *, tree, tree, int, bool *);
static tree handle_sentinel_attribute (tree *, tree, tree, int, bool *);
static tree handle_type_generic_attribute (tree *, tree, tree, int, bool *);
static tree handle_transaction_pure_attribute (tree *, tree, tree, int, bool *);
static tree handle_returns_twice_attribute (tree *, tree, tree, int, bool *);
static tree handle_patchable_function_entry_attribute (tree *, tree, tree,
						       int, bool *);
static tree ignore_attribute (tree *, tree, tree, int, bool *);

static tree handle_format_attribute (tree *, tree, tree, int, bool *);
static tree handle_fnspec_attribute (tree *, tree, tree, int, bool *);
static tree handle_format_arg_attribute (tree *, tree, tree, int, bool *);

/* Helper to define attribute exclusions.  */
#define ATTR_EXCL(name, function, type, variable)	\
  { name, function, type, variable }

/* Define attributes that are mutually exclusive with one another.  */
static const struct attribute_spec::exclusions attr_noreturn_exclusions[] =
{
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL ("alloc_align", true, true, true),
  ATTR_EXCL ("alloc_size", true, true, true),
  ATTR_EXCL ("const", true, true, true),
  ATTR_EXCL ("malloc", true, true, true),
  ATTR_EXCL ("pure", true, true, true),
  ATTR_EXCL ("returns_twice", true, true, true),
  ATTR_EXCL ("warn_unused_result", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_returns_twice_exclusions[] =
{
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_const_pure_exclusions[] =
{
  ATTR_EXCL ("const", true, true, true),
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL ("pure", true, true, true),
  ATTR_EXCL (NULL, false, false, false)
};

/* Table of machine-independent attributes supported in libgccjit.  */
const struct attribute_spec jit_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "noreturn",               0, 0, true,  false, false, false,
			      handle_noreturn_attribute,
			      attr_noreturn_exclusions },
  { "leaf",		      0, 0, true,  false, false, false,
			      handle_leaf_attribute, NULL },
  /* The same comments as for noreturn attributes apply to const ones.  */
  { "const",                  0, 0, true,  false, false, false,
			      handle_const_attribute,
			      attr_const_pure_exclusions },
  { "malloc",                 0, 0, true,  false, false, false,
			      handle_malloc_attribute, NULL },
  { "pure",                   0, 0, true,  false, false, false,
			      handle_pure_attribute,
			      attr_const_pure_exclusions },
  { "no vops",                0, 0, true,  false, false, false,
			      handle_novops_attribute, NULL },
  { "nonnull",                0, -1, false, true, true, false,
			      handle_nonnull_attribute, NULL },
  { "nothrow",                0, 0, true,  false, false, false,
			      handle_nothrow_attribute, NULL },
  { "patchable_function_entry", 1, 2, true, false, false, false,
			      handle_patchable_function_entry_attribute,
			      NULL },
  { "returns_twice",          0, 0, true,  false, false, false,
			      handle_returns_twice_attribute,
			      attr_returns_twice_exclusions },
  { "sentinel",               0, 1, false, true, true, false,
			      handle_sentinel_attribute, NULL },
  { "type generic",           0, 0, false, true, true, false,
			      handle_type_generic_attribute, NULL },
  { "fn spec",	 	      1, 1, false, true, true, false,
			      handle_fnspec_attribute, NULL },
  { "transaction_pure",	      0, 0, false, true, true, false,
			      handle_transaction_pure_attribute, NULL },
  /* For internal use only.  The leading '*' both prevents its usage in
     source code and signals that it may be overridden by machine tables.  */
  { "*tm regparm",            0, 0, false, true, true, false,
			      ignore_attribute, NULL },
  { NULL,                     0, 0, false, false, false, false, NULL, NULL }
};

/* Give the specifications for the format attributes, used by C and all
   descendants.  */

const struct attribute_spec jit_format_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "format",                 3, 3, false, true,  true, false,
			      handle_format_attribute, NULL },
  { "format_arg",             1, 1, false, true,  true, false,
			      handle_format_arg_attribute, NULL },
  { NULL,                     0, 0, false, false, false, false, NULL, NULL }
};

/* Attribute handlers.  */

/* Handle a "noreturn" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noreturn_attribute (tree *node, tree ARG_UNUSED (name),
			   tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			   bool * ARG_UNUSED (no_add_attrs))
{
  tree type = TREE_TYPE (*node);

  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_THIS_VOLATILE (*node) = 1;
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    TREE_TYPE (*node)
      = build_pointer_type
	(build_type_variant (TREE_TYPE (type),
			     TYPE_READONLY (TREE_TYPE (type)), 1));
  else
    gcc_unreachable ();

  return NULL_TREE;
}

/* Handle a "leaf" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_leaf_attribute (tree *node, tree name,
		       tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  if (!TREE_PUBLIC (*node))
    {
      warning (OPT_Wattributes, "%qE attribute has no effect on unit local functions", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "const" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_const_attribute (tree *node, tree ARG_UNUSED (name),
			tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			bool * ARG_UNUSED (no_add_attrs))
{
  if (TREE_CODE (*node) != FUNCTION_DECL
      || !fndecl_built_in_p (*node))
    inform (UNKNOWN_LOCATION, "%s:%s: %E: %E", __FILE__, __func__, *node, name);

  tree type = TREE_TYPE (*node);

  /* See FIXME comment on noreturn in c_common_attribute_table.  */
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_READONLY (*node) = 1;
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    TREE_TYPE (*node)
      = build_pointer_type
	(build_type_variant (TREE_TYPE (type), 1,
			     TREE_THIS_VOLATILE (TREE_TYPE (type))));
  else
    gcc_unreachable ();

  return NULL_TREE;
}


/* Handle a "malloc" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_malloc_attribute (tree *node, tree ARG_UNUSED (name),
			 tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			 bool * ARG_UNUSED (no_add_attrs))
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (*node))))
    DECL_IS_MALLOC (*node) = 1;
  else
    gcc_unreachable ();

  return NULL_TREE;
}


/* Handle a "pure" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_pure_attribute (tree *node, tree ARG_UNUSED (name),
		       tree ARG_UNUSED (args), int ARG_UNUSED (flags),
		       bool * ARG_UNUSED (no_add_attrs))
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_PURE_P (*node) = 1;
  else
    gcc_unreachable ();

  return NULL_TREE;
}


/* Handle a "no vops" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_novops_attribute (tree *node, tree ARG_UNUSED (name),
			 tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			 bool *ARG_UNUSED (no_add_attrs))
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);
  DECL_IS_NOVOPS (*node) = 1;
  return NULL_TREE;
}


/* Helper for nonnull attribute handling; fetch the operand number
   from the attribute argument list.  */

static bool
get_nonnull_operand (tree arg_num_expr, unsigned HOST_WIDE_INT *valp)
{
  /* Verify the arg number is a constant.  */
  if (!tree_fits_uhwi_p (arg_num_expr))
    return false;

  *valp = TREE_INT_CST_LOW (arg_num_expr);
  return true;
}

/* Handle the "nonnull" attribute.  */

static tree
handle_nonnull_attribute (tree *node, tree ARG_UNUSED (name),
			  tree args, int ARG_UNUSED (flags),
			  bool * ARG_UNUSED (no_add_attrs))
{
  tree type = *node;

  /* If no arguments are specified, all pointer arguments should be
     non-null.  Verify a full prototype is given so that the arguments
     will have the correct types when we actually check them later.
     Avoid diagnosing type-generic built-ins since those have no
     prototype.  */
  if (!args)
    {
      gcc_assert (prototype_p (type)
		  || !TYPE_ATTRIBUTES (type)
		  || lookup_attribute ("type generic", TYPE_ATTRIBUTES (type)));

      return NULL_TREE;
    }

  /* Argument list specified.  Verify that each argument number references
     a pointer argument.  */
  for (; args; args = TREE_CHAIN (args))
    {
      tree argument;
      unsigned HOST_WIDE_INT arg_num = 0, ck_num;

      if (!get_nonnull_operand (TREE_VALUE (args), &arg_num))
	gcc_unreachable ();

      argument = TYPE_ARG_TYPES (type);
      if (argument)
	{
	  for (ck_num = 1; ; ck_num++)
	    {
	      if (!argument || ck_num == arg_num)
		break;
	      argument = TREE_CHAIN (argument);
	    }

	  gcc_assert (argument
		      && TREE_CODE (TREE_VALUE (argument)) == POINTER_TYPE);
	}
    }

  return NULL_TREE;
}


/* Handle a "nothrow" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_nothrow_attribute (tree *node, tree ARG_UNUSED (name),
			  tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			  bool * ARG_UNUSED (no_add_attrs))
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_NOTHROW (*node) = 1;
  else
    gcc_unreachable ();

  return NULL_TREE;
}


/* Handle a "sentinel" attribute.  */

static tree
handle_sentinel_attribute (tree *node, tree ARG_UNUSED (name), tree args,
			   int ARG_UNUSED (flags),
			   bool * ARG_UNUSED (no_add_attrs))
{
  gcc_assert (stdarg_p (*node));

  if (args)
    {
      tree position = TREE_VALUE (args);
      gcc_assert (TREE_CODE (position) == INTEGER_CST);
      if (tree_int_cst_lt (position, integer_zero_node))
	gcc_unreachable ();
    }

  return NULL_TREE;
}

/* Handle a "type_generic" attribute.  */

static tree
handle_type_generic_attribute (tree *node, tree ARG_UNUSED (name),
			       tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			       bool * ARG_UNUSED (no_add_attrs))
{
  /* Ensure we have a function type.  */
  gcc_assert (TREE_CODE (*node) == FUNCTION_TYPE);

  /* Ensure we have a variadic function.  */
  gcc_assert (!prototype_p (*node) || stdarg_p (*node));

  return NULL_TREE;
}

/* Handle a "transaction_pure" attribute.  */

static tree
handle_transaction_pure_attribute (tree *node, tree ARG_UNUSED (name),
				   tree ARG_UNUSED (args),
				   int ARG_UNUSED (flags),
				   bool * ARG_UNUSED (no_add_attrs))
{
  /* Ensure we have a function type.  */
  gcc_assert (TREE_CODE (*node) == FUNCTION_TYPE);

  return NULL_TREE;
}

/* Handle a "returns_twice" attribute.  */

static tree
handle_returns_twice_attribute (tree *node, tree ARG_UNUSED (name),
				tree ARG_UNUSED (args),
				int ARG_UNUSED (flags),
				bool * ARG_UNUSED (no_add_attrs))
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);

  DECL_IS_RETURNS_TWICE (*node) = 1;

  return NULL_TREE;
}

static tree
handle_patchable_function_entry_attribute (tree *, tree, tree, int, bool *)
{
  /* Nothing to be done here.  */
  return NULL_TREE;
}

/* Ignore the given attribute.  Used when this attribute may be usefully
   overridden by the target, but is not used generically.  */

static tree
ignore_attribute (tree * ARG_UNUSED (node), tree ARG_UNUSED (name),
		  tree ARG_UNUSED (args), int ARG_UNUSED (flags),
		  bool *no_add_attrs)
{
  *no_add_attrs = true;
  return NULL_TREE;
}

/* Handle a "format" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_format_attribute (tree * ARG_UNUSED (node), tree ARG_UNUSED (name),
			 tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			 bool *no_add_attrs)
{
  *no_add_attrs = true;
  return NULL_TREE;
}


/* Handle a "format_arg" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_format_arg_attribute (tree * ARG_UNUSED (node), tree ARG_UNUSED (name),
			     tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			     bool *no_add_attrs)
{
  *no_add_attrs = true;
  return NULL_TREE;
}


/* Handle a "fn spec" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_fnspec_attribute (tree *node ATTRIBUTE_UNUSED, tree ARG_UNUSED (name),
			 tree args, int ARG_UNUSED (flags),
			 bool *no_add_attrs ATTRIBUTE_UNUSED)
{
  gcc_assert (args
	      && TREE_CODE (TREE_VALUE (args)) == STRING_CST
	      && !TREE_CHAIN (args));
  return NULL_TREE;
}

/* (end of attribute-handling).  */

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

/* GC-marking callback for use from jit_root_tab.

   If there's an active playback context, call its marking method
   so that it can mark any pointers it references.  */

static void my_ggc_walker (void *)
{
  if (gcc::jit::active_playback_ctxt)
    gcc::jit::active_playback_ctxt->gt_ggc_mx ();
}

const char *dummy;

struct ggc_root_tab jit_root_tab[] =
  {
    {
      &dummy, 1, 0, my_ggc_walker, NULL
    },
    LAST_GGC_ROOT_TAB
  };

/* JIT-specific implementation of diagnostic callbacks.  */

/* Implementation of "begin_diagnostic".  */

static void
jit_begin_diagnostic (diagnostic_context */*context*/,
		      diagnostic_info */*diagnostic*/)
{
  gcc_assert (gcc::jit::active_playback_ctxt);
  JIT_LOG_SCOPE (gcc::jit::active_playback_ctxt->get_logger ());

  /* No-op (apart from logging); the real error-handling is done in the
     "end_diagnostic" hook.  */
}

/* Implementation of "end_diagnostic".  */

static void
jit_end_diagnostic (diagnostic_context *context,
		    diagnostic_info *diagnostic,
		    diagnostic_t)
{
  gcc_assert (gcc::jit::active_playback_ctxt);
  JIT_LOG_SCOPE (gcc::jit::active_playback_ctxt->get_logger ());

  /* Delegate to the playback context (and thence to the
     recording context).  */
  gcc::jit::active_playback_ctxt->add_diagnostic (context, diagnostic);
}

/* Language hooks.  */

static bool
jit_langhook_init (void)
{
  gcc_assert (gcc::jit::active_playback_ctxt);
  JIT_LOG_SCOPE (gcc::jit::active_playback_ctxt->get_logger ());

  static bool registered_root_tab = false;
  if (!registered_root_tab)
    {
      ggc_register_root_tab (jit_root_tab);
      registered_root_tab = true;
    }

  gcc_assert (global_dc);
  global_dc->begin_diagnostic = jit_begin_diagnostic;
  global_dc->end_diagnostic = jit_end_diagnostic;

  build_common_tree_nodes (false);

  /* I don't know why this has to be done explicitly.  */
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  build_common_builtin_nodes ();

  /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may
     eventually be controllable by a command line option.  */
  mpfr_set_default_prec (256);

  return true;
}

static void
jit_langhook_parse_file (void)
{
  /* Replay the activity by the client, recorded on the context.  */
  gcc_assert (gcc::jit::active_playback_ctxt);
  gcc::jit::active_playback_ctxt->replay ();
}

static tree
jit_langhook_type_for_mode (machine_mode mode, int unsignedp)
{
  /* Build any vector types here (see PR 46805).  */
  if (VECTOR_MODE_P (mode))
    {
      tree inner;

      inner = jit_langhook_type_for_mode (GET_MODE_INNER (mode), unsignedp);
      if (inner != NULL_TREE)
	return build_vector_type_for_mode (inner, mode);
      return NULL_TREE;
    }

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;
  if (mode == TYPE_MODE (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (mode == TYPE_MODE (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (mode == TYPE_MODE (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (mode == TYPE_MODE (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;

  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;

  if (COMPLEX_MODE_P (mode))
    {
      if (mode == TYPE_MODE (complex_float_type_node))
	return complex_float_type_node;
      if (mode == TYPE_MODE (complex_double_type_node))
	return complex_double_type_node;
      if (mode == TYPE_MODE (complex_long_double_type_node))
	return complex_long_double_type_node;
      if (mode == TYPE_MODE (complex_integer_type_node) && !unsignedp)
	return complex_integer_type_node;
    }

  /* gcc_unreachable */
  return NULL;
}

/* Record a builtin function.  We just ignore builtin functions.  */

static tree
jit_langhook_builtin_function (tree decl)
{
  return decl;
}

static bool
jit_langhook_global_bindings_p (void)
{
  return true;
}

static tree
jit_langhook_pushdecl (tree decl ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

static tree
jit_langhook_getdecls (void)
{
  return NULL;
}

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME		"libgccjit"

#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT		jit_langhook_init

#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE		jit_langhook_parse_file

#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE	jit_langhook_type_for_mode

#undef LANG_HOOKS_BUILTIN_FUNCTION
#define LANG_HOOKS_BUILTIN_FUNCTION	jit_langhook_builtin_function

#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#define LANG_HOOKS_GLOBAL_BINDINGS_P	jit_langhook_global_bindings_p

#undef LANG_HOOKS_PUSHDECL
#define LANG_HOOKS_PUSHDECL		jit_langhook_pushdecl

#undef LANG_HOOKS_GETDECLS
#define LANG_HOOKS_GETDECLS		jit_langhook_getdecls

/* Attribute hooks.  */
#undef LANG_HOOKS_COMMON_ATTRIBUTE_TABLE
#define LANG_HOOKS_COMMON_ATTRIBUTE_TABLE jit_attribute_table
#undef LANG_HOOKS_FORMAT_ATTRIBUTE_TABLE
#define LANG_HOOKS_FORMAT_ATTRIBUTE_TABLE jit_format_attribute_table

#undef  LANG_HOOKS_DEEP_UNSHARING
#define LANG_HOOKS_DEEP_UNSHARING	true

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-jit-dummy-frontend.h"
#include "gtype-jit.h"
