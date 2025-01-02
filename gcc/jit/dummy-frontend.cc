/* jit.c -- Dummy "frontend" for use during JIT-compilation.
   Copyright (C) 2013-2025 Free Software Foundation, Inc.

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
#include "target.h"
#include "jit-playback.h"
#include "stor-layout.h"
#include "debug.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "diagnostic.h"
#include "options.h"
#include "stringpool.h"
#include "attribs.h"
#include "cgraph.h"
#include "target.h"
#include "diagnostic-format-text.h"
#include "make-unique.h"
#include "print-tree.h"

#include <mpfr.h>
#include <unordered_map>

using namespace gcc::jit;

/* Attribute handling.  */

static tree handle_alias_attribute (tree *, tree, tree, int, bool *);
static tree handle_always_inline_attribute (tree *, tree, tree, int,
					    bool *);
static tree handle_cold_attribute (tree *, tree, tree, int, bool *);
static tree handle_const_attribute (tree *, tree, tree, int, bool *);
static tree handle_fnspec_attribute (tree *, tree, tree, int, bool *);
static tree handle_format_arg_attribute (tree *, tree, tree, int, bool *);
static tree handle_format_attribute (tree *, tree, tree, int, bool *);
static tree handle_leaf_attribute (tree *, tree, tree, int, bool *);
static tree handle_malloc_attribute (tree *, tree, tree, int, bool *);
static tree handle_noinline_attribute (tree *, tree, tree, int, bool *);
static tree handle_nonnull_attribute (tree *, tree, tree, int, bool *);
static tree handle_noreturn_attribute (tree *, tree, tree, int, bool *);
static tree handle_nothrow_attribute (tree *, tree, tree, int, bool *);
static tree handle_novops_attribute (tree *, tree, tree, int, bool *);
static tree handle_patchable_function_entry_attribute (tree *, tree, tree,
						       int, bool *);
static tree handle_pure_attribute (tree *, tree, tree, int, bool *);
static tree handle_returns_twice_attribute (tree *, tree, tree, int, bool *);
static tree handle_sentinel_attribute (tree *, tree, tree, int, bool *);
static tree handle_target_attribute (tree *, tree, tree, int, bool *);
static tree handle_transaction_pure_attribute (tree *, tree, tree, int, bool *);
static tree handle_type_generic_attribute (tree *, tree, tree, int, bool *);
static tree handle_used_attribute (tree *, tree, tree, int, bool *);
static tree handle_visibility_attribute (tree *, tree, tree, int,
					 bool *);
static tree handle_weak_attribute (tree *, tree, tree, int, bool *) ;

static tree ignore_attribute (tree *, tree, tree, int, bool *);

/* Helper to define attribute exclusions.  */
#define ATTR_EXCL(name, function, type, variable)	\
  { name, function, type, variable }

/* Define attributes that are mutually exclusive with one another.  */
static const struct attribute_spec::exclusions attr_noreturn_exclusions[] =
{
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

/* Exclusions that apply to attribute alloc_align, alloc_size, and malloc.  */
static const struct attribute_spec::exclusions attr_alloc_exclusions[] =
{
  ATTR_EXCL ("const", true, true, true),
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL ("pure", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_const_pure_exclusions[] =
{
  ATTR_EXCL ("const", true, true, true),
  ATTR_EXCL ("alloc_align", true, true, true),
  ATTR_EXCL ("alloc_size", true, true, true),
  ATTR_EXCL ("malloc", true, true, true),
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL ("pure", true, true, true),
  ATTR_EXCL (NULL, false, false, false)
};

static const struct attribute_spec::exclusions attr_always_inline_exclusions[] =
{
  ATTR_EXCL ("noinline", true, true, true),
  ATTR_EXCL ("target_clones", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

extern const struct attribute_spec::exclusions attr_cold_hot_exclusions[] =
{
  ATTR_EXCL ("cold", true, true, true),
  ATTR_EXCL ("hot", true, true, true),
  ATTR_EXCL (NULL, false, false, false)
};

static const struct attribute_spec::exclusions attr_noinline_exclusions[] =
{
  ATTR_EXCL ("always_inline", true, true, true),
  ATTR_EXCL ("gnu_inline", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_target_exclusions[] =
{
  ATTR_EXCL ("target_clones", TARGET_HAS_FMV_TARGET_ATTRIBUTE,
             TARGET_HAS_FMV_TARGET_ATTRIBUTE, TARGET_HAS_FMV_TARGET_ATTRIBUTE),
  ATTR_EXCL (NULL, false, false, false),
};

/* These variables act as a cache for the target builtins. This is needed in
   order to be able to type-check the calls since we can only get those types
   in the playback phase while we need them in the recording phase.  */
hash_map<nofree_string_hash, tree> target_builtins{};
std::unordered_map<std::string, recording::function_type*> target_function_types
{};
recording::context target_builtins_ctxt{NULL};

/* Table of machine-independent attributes supported in libgccjit.  */
static const attribute_spec jit_gnu_attributes[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "alias",		      1, 1, true,  false, false, false,
			      handle_alias_attribute, NULL },
  { "always_inline",	      0, 0, true,  false, false, false,
			      handle_always_inline_attribute,
			      attr_always_inline_exclusions },
  { "cold",		      0, 0, true,  false, false, false,
			      handle_cold_attribute,
			      attr_cold_hot_exclusions },
  /* The same comments as for noreturn attributes apply to const ones.  */
  { "const",		      0, 0, true,  false, false, false,
			      handle_const_attribute,
			      attr_const_pure_exclusions },
  { "fn spec",		      1, 1, false, true, true, false,
			      handle_fnspec_attribute, NULL },

  { "leaf",		      0, 0, true,  false, false, false,
			      handle_leaf_attribute, NULL },
  { "malloc",		      0, 0, true,  false, false, false,
			      handle_malloc_attribute, attr_alloc_exclusions },
  { "noreturn",		      0, 0, true,  false, false, false,
			      handle_noreturn_attribute,
			      attr_noreturn_exclusions },
  { "no vops",		      0, 0, true,  false, false, false,
			      handle_novops_attribute, NULL },
  { "noinline",		      0, 0, true,  false, false, false,
			      handle_noinline_attribute,
			      attr_noinline_exclusions },
  { "nonnull",		      0, -1, false, true, true, false,
			      handle_nonnull_attribute, NULL },
  { "nothrow",		      0, 0, true,  false, false, false,
			      handle_nothrow_attribute, NULL },
  { "patchable_function_entry", 1, 2, true, false, false, false,
			      handle_patchable_function_entry_attribute,
			      NULL },
  { "pure",		      0, 0, true,  false, false, false,
			      handle_pure_attribute,
			      attr_const_pure_exclusions },
  { "returns_twice",	      0, 0, true,  false, false, false,
			      handle_returns_twice_attribute,
			      attr_returns_twice_exclusions },
  { "sentinel",		      0, 1, false, true, true, false,
			      handle_sentinel_attribute, NULL },
  { "target",		      1, -1, true, false, false, false,
			      handle_target_attribute, attr_target_exclusions },
  { "type generic",	      0, 0, false, true, true, false,
			      handle_type_generic_attribute, NULL },
  { "transaction_pure",	      0, 0, false, true, true, false,
			      handle_transaction_pure_attribute, NULL },
  { "used",         0, 0, true,  false, false, false,
            handle_used_attribute, NULL },
  { "visibility",       1, 1, false, false, false, false,
            handle_visibility_attribute, NULL },
  { "weak",         0, 0, true,  false, false, false,
            handle_weak_attribute, NULL },
  /* For internal use only.  The leading '*' both prevents its usage in
     source code and signals that it may be overridden by machine tables.  */
  { "*tm regparm",            0, 0, false, true, true, false,
			      ignore_attribute, NULL },
};

static const scoped_attribute_specs jit_gnu_attribute_table =
{
  "gnu", { jit_gnu_attributes }
};

/* Give the specifications for the format attributes, used by C and all
   descendants.  */

static const attribute_spec jit_format_attributes[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "format",		      3, 3, false, true,  true, false,
			      handle_format_attribute, NULL },
  { "format_arg",             1, 1, false, true,  true, false,
			      handle_format_arg_attribute, NULL }
};

static const scoped_attribute_specs jit_format_attribute_table =
{
  "gnu", { jit_format_attributes }
};

static const scoped_attribute_specs *const jit_attribute_table[] =
{
  &jit_gnu_attribute_table,
  &jit_format_attribute_table
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
handle_const_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree type = TREE_TYPE (*node);

  /* See FIXME comment on noreturn in c_common_attribute_table.  */
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_READONLY (*node) = 1;
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    TREE_TYPE (*node)
      = (build_qualified_type
	 (build_pointer_type
	  (build_type_variant (TREE_TYPE (type), 1,
			       TREE_THIS_VOLATILE (TREE_TYPE (type)))),
	  TYPE_QUALS (type)));
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

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

/* Handle an "visibility" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_visibility_attribute (tree *node, tree name, tree args,
			     int ARG_UNUSED (flags),
			     bool *ARG_UNUSED (no_add_attrs))
{
  tree decl = *node;
  tree id = TREE_VALUE (args);
  enum symbol_visibility vis;

  if (TYPE_P (*node))
    {
      if (TREE_CODE (*node) == ENUMERAL_TYPE)
	/* OK.  */;
      else if (!RECORD_OR_UNION_TYPE_P (*node))
	{
	  warning (OPT_Wattributes, "%qE attribute ignored on non-class types",
		   name);
	  return NULL_TREE;
	}
      else if (TYPE_FIELDS (*node))
	{
	  error ("%qE attribute ignored because %qT is already defined",
		 name, *node);
	  return NULL_TREE;
	}
    }
  else if (decl_function_context (decl) != 0 || !TREE_PUBLIC (decl))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      return NULL_TREE;
    }

  if (TREE_CODE (id) != STRING_CST)
    {
      error ("visibility argument not a string");
      return NULL_TREE;
    }

  /*  If this is a type, set the visibility on the type decl.  */
  if (TYPE_P (decl))
    {
      decl = TYPE_NAME (decl);
      if (!decl)
	return NULL_TREE;
      if (TREE_CODE (decl) == IDENTIFIER_NODE)
	{
	   warning (OPT_Wattributes, "%qE attribute ignored on types",
		    name);
	   return NULL_TREE;
	}
    }

  if (strcmp (TREE_STRING_POINTER (id), "default") == 0)
    vis = VISIBILITY_DEFAULT;
  else if (strcmp (TREE_STRING_POINTER (id), "internal") == 0)
    vis = VISIBILITY_INTERNAL;
  else if (strcmp (TREE_STRING_POINTER (id), "hidden") == 0)
    vis = VISIBILITY_HIDDEN;
  else if (strcmp (TREE_STRING_POINTER (id), "protected") == 0)
    vis = VISIBILITY_PROTECTED;
  else
    {
      error ("attribute %qE argument must be one of %qs, %qs, %qs, or %qs",
	     name, "default", "hidden", "protected", "internal");
      vis = VISIBILITY_DEFAULT;
    }

  if (DECL_VISIBILITY_SPECIFIED (decl)
      && vis != DECL_VISIBILITY (decl))
    {
      tree attributes = (TYPE_P (*node)
			 ? TYPE_ATTRIBUTES (*node)
			 : DECL_ATTRIBUTES (decl));
      if (lookup_attribute ("visibility", attributes))
	error ("%qD redeclared with different visibility", decl);
      else if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	       && lookup_attribute ("dllimport", attributes))
	error ("%qD was declared %qs which implies default visibility",
	       decl, "dllimport");
      else if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	       && lookup_attribute ("dllexport", attributes))
	error ("%qD was declared %qs which implies default visibility",
	       decl, "dllexport");
    }

  DECL_VISIBILITY (decl) = vis;
  DECL_VISIBILITY_SPECIFIED (decl) = 1;

  /* Go ahead and attach the attribute to the node as well.  This is needed
     so we can determine whether we have VISIBILITY_DEFAULT because the
     visibility was not specified, or because it was explicitly overridden
     from the containing scope.  */

  return NULL_TREE;
}

/* Handle a "always_inline" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_always_inline_attribute (tree *node, tree name,
				tree ARG_UNUSED (args),
				int ARG_UNUSED (flags),
				bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    {
      /* Set the attribute and mark it for disregarding inline
	 limits.  */
      DECL_DISREGARD_INLINE_LIMITS (*node) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "cold" and attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_cold_attribute (tree *node, tree name, tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      || TREE_CODE (*node) == LABEL_DECL)
    {
      /* Attribute cold processing is done later with lookup_attribute.  */
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "noinline" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noinline_attribute (tree *node, tree name,
			   tree ARG_UNUSED (args),
			   int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_UNINLINABLE (*node) = 1;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "weak" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_weak_attribute (tree *node, tree name,
		       tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags),
		       bool * ARG_UNUSED (no_add_attrs))
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      && DECL_DECLARED_INLINE_P (*node))
    {
      warning (OPT_Wattributes, "inline function %q+D declared weak", *node);
      *no_add_attrs = true;
    }
  else if (lookup_attribute ("ifunc", DECL_ATTRIBUTES (*node)))
    {
      error ("indirect function %q+D cannot be declared weak", *node);
      *no_add_attrs = true;
      return NULL_TREE;
    }
  else if (VAR_OR_FUNCTION_DECL_P (*node))
    declare_weak (*node);
  else
    warning (OPT_Wattributes, "%qE attribute ignored", name);

  return NULL_TREE;
}

/* Handle a "target" attribute.  */

static tree
handle_target_attribute (tree *node, tree name, tree args, int flags,
			 bool *no_add_attrs)
{
  /* Ensure we have a function declaration.  */
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else if (! targetm.target_option.valid_attribute_p (*node, name, args,
						      flags))
    *no_add_attrs = true;

  /* Check that there's no empty string in values of the attribute.  */
  for (tree t = args; t != NULL_TREE; t = TREE_CHAIN (t))
    {
      tree value = TREE_VALUE (t);
      if (TREE_CODE (value) == STRING_CST
	  && TREE_STRING_LENGTH (value) == 1
	  && TREE_STRING_POINTER (value)[0] == '\0')
	{
	  warning (OPT_Wattributes, "empty string in attribute %<target%>");
	  *no_add_attrs = true;
	}
    }

  return NULL_TREE;
}

/* Handle a "used" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_used_attribute (tree *pnode, tree name, tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree node = *pnode;

  if (TREE_CODE (node) == FUNCTION_DECL
      || (VAR_P (node) && TREE_STATIC (node))
      || (TREE_CODE (node) == TYPE_DECL))
    {
      TREE_USED (node) = 1;
      DECL_PRESERVE_P (node) = 1;
      if (VAR_P (node))
	DECL_READ_P (node) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "alias" or "ifunc" attribute; arguments as in
   struct attribute_spec.handler, except that IS_ALIAS tells us
   whether this is an alias as opposed to ifunc attribute.  */

static tree
handle_alias_ifunc_attribute (bool is_alias, tree *node, tree name, tree args,
			      bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL
      && (!is_alias || !VAR_P (decl)))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else if ((TREE_CODE (decl) == FUNCTION_DECL && DECL_INITIAL (decl))
      || (TREE_CODE (decl) != FUNCTION_DECL
	  && TREE_PUBLIC (decl) && !DECL_EXTERNAL (decl))
      /* A static variable declaration is always a tentative definition,
	 but the alias is a non-tentative definition which overrides.  */
      || (TREE_CODE (decl) != FUNCTION_DECL
	  && ! TREE_PUBLIC (decl) && DECL_INITIAL (decl)))
    {
      error ("%q+D defined both normally and as %qE attribute", decl, name);
      *no_add_attrs = true;
      return NULL_TREE;
    }
  else if (!is_alias
	   && (lookup_attribute ("weak", DECL_ATTRIBUTES (decl))
	       || lookup_attribute ("weakref", DECL_ATTRIBUTES (decl))))
    {
      error ("weak %q+D cannot be defined %qE", decl, name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Note that the very first time we process a nested declaration,
     decl_function_context will not be set.  Indeed, *would* never
     be set except for the DECL_INITIAL/DECL_EXTERNAL frobbery that
     we do below.  After such frobbery, pushdecl would set the context.
     In any case, this is never what we want.  */
  else if (decl_function_context (decl) == 0 && current_function_decl == NULL)
    {
      tree id;

      id = TREE_VALUE (args);
      if (TREE_CODE (id) != STRING_CST)
	{
	  error ("attribute %qE argument not a string", name);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
      id = get_identifier (TREE_STRING_POINTER (id));
      /* This counts as a use of the object pointed to.  */
      TREE_USED (id) = 1;

      if (TREE_CODE (decl) == FUNCTION_DECL)
	DECL_INITIAL (decl) = error_mark_node;
      else
	TREE_STATIC (decl) = 1;

      if (!is_alias)
	{
	  /* ifuncs are also aliases, so set that attribute too.  */
	  DECL_ATTRIBUTES (decl)
	    = tree_cons (get_identifier ("alias"), args,
			 DECL_ATTRIBUTES (decl));
	  DECL_ATTRIBUTES (decl) = tree_cons (get_identifier ("ifunc"),
					      NULL, DECL_ATTRIBUTES (decl));
	}
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  if (decl_in_symtab_p (*node))
    {
      struct symtab_node *n = symtab_node::get (decl);
      if (n && n->refuse_visibility_changes)
	error ("%+qD declared %qs after being used",
	       decl, is_alias ? "alias" : "ifunc");
    }


  return NULL_TREE;
}

/* Handle an "alias" or "ifunc" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_alias_attribute (tree *node, tree name, tree args,
			int ARG_UNUSED (flags), bool *no_add_attrs)
{
  return handle_alias_ifunc_attribute (true, node, name, args, no_add_attrs);
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

/* Subclass of diagnostic_output_format for libgccjit: like text
   output, but capture the message and call add_diagnostic with it
   on the active playback context.  */

class jit_diagnostic_listener : public diagnostic_text_output_format
{
public:
  jit_diagnostic_listener (diagnostic_context &dc,
			   gcc::jit::playback::context &playback_ctxt)
  : diagnostic_text_output_format (dc),
    m_playback_ctxt (playback_ctxt)
  {
  }

  void dump (FILE *out, int indent) const final override
  {
    fprintf (out, "%*sjit_diagnostic_listener\n", indent, "");
    fprintf (out, "%*sm_playback_context: %p\n",
	     indent + 2, "",
	     (void *)&m_playback_ctxt);
  }

  void on_report_diagnostic (const diagnostic_info &info,
			     diagnostic_t orig_diag_kind)
  {
    JIT_LOG_SCOPE (gcc::jit::active_playback_ctxt->get_logger ());

    /* Let the text output format do most of the work.  */
    diagnostic_text_output_format::on_report_diagnostic (info, orig_diag_kind);

    const char *text = pp_formatted_text (get_printer ());

    /* Delegate to the playback context (and thence to the
       recording context).  */
    gcc::jit::active_playback_ctxt->add_diagnostic (text, info);

    pp_clear_output_area (get_printer ());
  }

private:
  gcc::jit::playback::context &m_playback_ctxt;
};

/* JIT-specific implementation of diagnostic callbacks.  */

/* Implementation of "begin_diagnostic".  */

static void
jit_begin_diagnostic (diagnostic_text_output_format &,
		      const diagnostic_info */*diagnostic*/)
{
  gcc_assert (gcc::jit::active_playback_ctxt);
  JIT_LOG_SCOPE (gcc::jit::active_playback_ctxt->get_logger ());

  /* No-op (apart from logging); the real error-handling is done by the
     jit_diagnostic_listener.  */
}

/* Implementation of "end_diagnostic".  */

static void
jit_end_diagnostic (diagnostic_text_output_format &,
		    const diagnostic_info *,
		    diagnostic_t)
{
  gcc_assert (gcc::jit::active_playback_ctxt);
  JIT_LOG_SCOPE (gcc::jit::active_playback_ctxt->get_logger ());

  /* No-op (apart from logging); the real error-handling is done by the
     jit_diagnostic_listener.  */
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
  diagnostic_text_starter (global_dc) = jit_begin_diagnostic;
  diagnostic_text_finalizer (global_dc) = jit_end_diagnostic;
  auto sink
    = ::make_unique<jit_diagnostic_listener> (*global_dc,
					      *gcc::jit::active_playback_ctxt);
  global_dc->set_output_format (std::move (sink));

  build_common_tree_nodes (flag_signed_char);

  target_builtins.empty ();
  build_common_builtin_nodes ();

  /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may
     eventually be controllable by a command line option.  */
  mpfr_set_default_prec (256);

  targetm.init_builtins ();

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

recording::type* tree_type_to_jit_type (tree type)
{
  if (TREE_CODE (type) == VECTOR_TYPE)
  {
    tree inner_type = TREE_TYPE (type);
    recording::type* element_type = tree_type_to_jit_type (inner_type);
    poly_uint64 size = TYPE_VECTOR_SUBPARTS (type);
    long constant_size = size.to_constant ();
    if (element_type != NULL)
      return element_type->get_vector (constant_size);
    return NULL;
  }
  if (TREE_CODE (type) == REFERENCE_TYPE)
    // For __builtin_ms_va_start.
    // FIXME: wrong type.
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_VOID);
  if (TREE_CODE (type) == RECORD_TYPE)
    // For __builtin_sysv_va_copy.
    // FIXME: wrong type.
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_VOID);
  /* TODO: Remove when we add support for sized floating-point types.  */
  for (int i = 0; i < NUM_FLOATN_NX_TYPES; i++)
    if (type == FLOATN_NX_TYPE_NODE (i))
      // FIXME: wrong type.
      return new recording::memento_of_get_type (&target_builtins_ctxt,
						 GCC_JIT_TYPE_VOID);
  if (type == void_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_VOID);
  else if (type == ptr_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_VOID_PTR);
  else if (type == const_ptr_type_node)
  {
    // Void const ptr.
    recording::type* result =
      new recording::memento_of_get_type (&target_builtins_ctxt,
					  GCC_JIT_TYPE_VOID_PTR);
    return new recording::memento_of_get_const (result);
  }
  else if (type == unsigned_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_UNSIGNED_INT);
  else if (type == long_unsigned_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_UNSIGNED_LONG);
  else if (type == integer_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_INT);
  else if (type == long_integer_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_LONG);
  else if (type == long_long_integer_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_LONG_LONG);
  else if (type == signed_char_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_SIGNED_CHAR);
  else if (type == char_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_CHAR);
  else if (type == unsigned_intQI_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_UINT8_T);
  else if (type == short_integer_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_SHORT);
  else if (type == short_unsigned_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_UNSIGNED_SHORT);
  else if (type == complex_float_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_COMPLEX_FLOAT);
  else if (type == complex_double_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_COMPLEX_DOUBLE);
  else if (type == complex_long_double_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					    GCC_JIT_TYPE_COMPLEX_LONG_DOUBLE);
  else if (type == float_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_FLOAT);
  else if (type == double_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_DOUBLE);
  else if (type == long_double_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_LONG_DOUBLE);
  else if (type == bfloat16_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_BFLOAT16);
  else if (type == dfloat128_type_node)
    // FIXME: wrong type.
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_VOID);
  else if (type == long_long_unsigned_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_UNSIGNED_LONG_LONG);
  else if (type == boolean_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_BOOL);
  else if (type == size_type_node)
    return new recording::memento_of_get_type (&target_builtins_ctxt,
					       GCC_JIT_TYPE_SIZE_T);
  else if (TREE_CODE (type) == POINTER_TYPE)
  {
    tree inner_type = TREE_TYPE (type);
    recording::type* element_type = tree_type_to_jit_type (inner_type);
    return element_type->get_pointer ();
  }
  else
  {
    // Attempt to find an unqualified type when the current type has qualifiers.
    tree tp = TYPE_MAIN_VARIANT (type);
    for ( ; tp != NULL ; tp = TYPE_NEXT_VARIANT (tp))
    {
      if (TYPE_QUALS (tp) == 0 && type != tp)
      {
	recording::type* result = tree_type_to_jit_type (tp);
	if (result != NULL)
	{
	  if (TYPE_READONLY (tp))
	    result = new recording::memento_of_get_const (result);
	  if (TYPE_VOLATILE (tp))
	    result = new recording::memento_of_get_volatile (result);
	  return result;
	}
      }
    }

    fprintf (stderr, "Unknown type:\n");
    debug_tree (type);
    abort ();
  }

  return NULL;
}

/* Record a builtin function.  We save their types to be able to check types
   in recording and for reflection.  */

static tree
jit_langhook_builtin_function (tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
  {
    const char* name = IDENTIFIER_POINTER (DECL_NAME (decl));
    target_builtins.put (name, decl);

    std::string string_name (name);
    if (target_function_types.count (string_name) == 0)
    {
      tree function_type = TREE_TYPE (decl);
      tree arg = TYPE_ARG_TYPES (function_type);
      bool is_variadic = false;

      auto_vec <recording::type *> param_types;

      while (arg != void_list_node)
      {
	if (arg == NULL)
	{
	  is_variadic = true;
	  break;
	}
	if (arg != void_list_node)
	{
	  recording::type* arg_type = tree_type_to_jit_type (TREE_VALUE (arg));
	  if (arg_type == NULL)
	    return decl;
	  param_types.safe_push (arg_type);
	}
	arg = TREE_CHAIN (arg);
      }

      tree result_type = TREE_TYPE (function_type);
      recording::type* return_type = tree_type_to_jit_type (result_type);

      if (return_type == NULL)
	return decl;

      recording::function_type* func_type =
	new recording::function_type (&target_builtins_ctxt, return_type,
				      param_types.length (),
				      param_types.address (), is_variadic,
				      false);

      target_function_types[string_name] = func_type;
    }
  }
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
#undef LANG_HOOKS_ATTRIBUTE_TABLE
#define LANG_HOOKS_ATTRIBUTE_TABLE jit_attribute_table

#undef  LANG_HOOKS_DEEP_UNSHARING
#define LANG_HOOKS_DEEP_UNSHARING	true

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-jit-dummy-frontend.h"
#include "gtype-jit.h"
