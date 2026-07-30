/* Language-independent attribute handlers shared across front ends.
   Copyright (C) 2026 Free Software Foundation, Inc.

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
#include "function.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "attr-handlers.h"
#include "diagnostic-core.h"
#include "options.h"
#include "cgraph.h"
#include "varasm.h"
#include "common/common-target.h"
#include "tree-pretty-print.h"

/* Helper to define attribute exclusions.  */
#define ATTR_EXCL(name, function, type, variable)	\
  { name, function, type, variable }

/* Define attributes that are mutually exclusive with one another.  */
const struct attribute_spec::exclusions attr_noreturn_exclusions[] =
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

const struct attribute_spec::exclusions attr_returns_twice_exclusions[] =
{
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

/* Exclusions that apply to attribute alloc_align, alloc_size, and malloc.  */
const struct attribute_spec::exclusions attr_alloc_exclusions[] =
{
  ATTR_EXCL ("const", true, true, true),
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL ("pure", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

const struct attribute_spec::exclusions attr_const_pure_exclusions[] =
{
  ATTR_EXCL ("const", true, true, true),
  ATTR_EXCL ("alloc_align", true, true, true),
  ATTR_EXCL ("alloc_size", true, true, true),
  ATTR_EXCL ("malloc", true, true, true),
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL ("pure", true, true, true),
  ATTR_EXCL (NULL, false, false, false)
};

const struct attribute_spec::exclusions attr_always_inline_exclusions[] =
{
  ATTR_EXCL ("noinline", true, true, true),
  ATTR_EXCL ("target_clones", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

const struct attribute_spec::exclusions attr_cold_hot_exclusions[] =
{
  ATTR_EXCL ("cold", true, true, true),
  ATTR_EXCL ("hot", true, true, true),
  ATTR_EXCL (NULL, false, false, false)
};

const struct attribute_spec::exclusions attr_noinline_exclusions[] =
{
  ATTR_EXCL ("always_inline", true, true, true),
  ATTR_EXCL ("gnu_inline", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

const struct attribute_spec::exclusions attr_target_exclusions[] =
{
  ATTR_EXCL ("target_clones", TARGET_HAS_FMV_TARGET_ATTRIBUTE,
	     TARGET_HAS_FMV_TARGET_ATTRIBUTE, TARGET_HAS_FMV_TARGET_ATTRIBUTE),
  ATTR_EXCL (NULL, false, false, false),
};

/* Exclusions that apply to attributes that put declarations in specific
   sections.  */
const struct attribute_spec::exclusions attr_section_exclusions[] =
{
  ATTR_EXCL ("noinit", true, true, true),
  ATTR_EXCL ("persistent", true, true, true),
  ATTR_EXCL ("section", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

/* Return the first of DECL or TYPE attributes installed in NODE if it's
   a DECL, or TYPE attributes if it's a TYPE, or null otherwise.  */

static tree
decl_or_type_attrs (tree node)
{
  if (DECL_P (node))
    {
      if (tree attrs = DECL_ATTRIBUTES (node))
	return attrs;

      tree type = TREE_TYPE (node);
      if (type == error_mark_node)
	return NULL_TREE;
      return TYPE_ATTRIBUTES (type);
    }

  if (TYPE_P (node))
    return TYPE_ATTRIBUTES (node);

  return NULL_TREE;
}

/* Given a pair of NODEs for arbitrary DECLs or TYPEs, validate one or
   two integral or string attribute arguments NEWARGS to be applied to
   NODE[0] for the absence of conflicts with the same attribute arguments
   already applied to NODE[1]. Issue a warning for conflicts and return
   false.  Otherwise, when no conflicts are found, return true.  */

bool
validate_attr_args (tree node[2], tree name, tree newargs[2])
{
  /* First validate the arguments against those already applied to
     the same declaration (or type).  */
  tree self[2] = { node[0], node[0] };
  if (node[0] != node[1] && !validate_attr_args (self, name, newargs))
    return false;

  if (!node[1])
    return true;

  /* Extract the same attribute from the previous declaration or type.  */
  tree prevattr = decl_or_type_attrs (node[1]);
  const char* const namestr = IDENTIFIER_POINTER (name);
  prevattr = lookup_attribute (namestr, prevattr);
  if (!prevattr)
    return true;

  /* Extract one or both attribute arguments.  */
  tree prevargs[2];
  prevargs[0] = TREE_VALUE (TREE_VALUE (prevattr));
  prevargs[1] = TREE_CHAIN (TREE_VALUE (prevattr));
  if (prevargs[1])
    prevargs[1] = TREE_VALUE (prevargs[1]);

  /* Both arguments must be equal or, for the second pair, neither must
     be provided to succeed.  */
  bool arg1eq, arg2eq;
  if (TREE_CODE (newargs[0]) == INTEGER_CST)
    {
      arg1eq = tree_int_cst_equal (newargs[0], prevargs[0]);
      if (newargs[1] && prevargs[1])
	arg2eq = tree_int_cst_equal (newargs[1], prevargs[1]);
      else
	arg2eq = newargs[1] == prevargs[1];
    }
  else if (TREE_CODE (newargs[0]) == STRING_CST)
    {
      const char *s0 = TREE_STRING_POINTER (newargs[0]);
      const char *s1 = TREE_STRING_POINTER (prevargs[0]);
      arg1eq = strcmp (s0, s1) == 0;
      if (newargs[1] && prevargs[1])
	{
	  s0 = TREE_STRING_POINTER (newargs[1]);
	  s1 = TREE_STRING_POINTER (prevargs[1]);
	  arg2eq = strcmp (s0, s1) == 0;
	}
      else
	arg2eq = newargs[1] == prevargs[1];
    }
  else
    gcc_unreachable ();

  if (arg1eq && arg2eq)
    return true;

  /* If the two locations are different print a note pointing to
     the previous one.  */
  const location_t curloc = input_location;
  const location_t prevloc
    = DECL_P (node[1]) ? DECL_SOURCE_LOCATION (node[1]) : curloc;

  /* Format the attribute specification for convenience.  */
  char newspec[80], prevspec[80];
  if (newargs[1])
    snprintf (newspec, sizeof newspec, "%s (%s, %s)", namestr,
	      print_generic_expr_to_str (newargs[0]),
	      print_generic_expr_to_str (newargs[1]));
  else
    snprintf (newspec, sizeof newspec, "%s (%s)", namestr,
	      print_generic_expr_to_str (newargs[0]));

  if (prevargs[1])
    snprintf (prevspec, sizeof prevspec, "%s (%s, %s)", namestr,
	      print_generic_expr_to_str (prevargs[0]),
	      print_generic_expr_to_str (prevargs[1]));
  else
    snprintf (prevspec, sizeof prevspec, "%s (%s)", namestr,
	      print_generic_expr_to_str (prevargs[0]));

  if (warning_at (curloc, OPT_Wattributes,
		  "ignoring attribute %qs because it conflicts "
		  "with previous %qs",
		  newspec, prevspec)
      && curloc != prevloc)
    inform (prevloc, "previous declaration here");

  return false;
}

/* Convenience wrapper for validate_attr_args to validate a single
   attribute argument.  Used by handlers for attributes that take
   just a single argument.  */

bool
validate_attr_arg (tree node[2], tree name, tree newarg)
{
  tree argarray[2] = { newarg, NULL_TREE };
  return validate_attr_args (node, name, argarray);
}

/* Handle an "alias" or "ifunc" attribute; arguments as in
   struct attribute_spec.handler, except that IS_ALIAS tells us
   whether this is an alias as opposed to ifunc attribute.  */

tree
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

/* Ignore the given attribute.  Used when this attribute may be usefully
   overridden by the target, but is not used generically.  */

tree
ignore_attribute (tree * ARG_UNUSED (node), tree ARG_UNUSED (name),
		  tree ARG_UNUSED (args), int ARG_UNUSED (flags),
		  bool *no_add_attrs)
{
  *no_add_attrs = true;
  return NULL_TREE;
}

/* Handle a "leaf" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
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
      warning (OPT_Wattributes, "%qE attribute has no effect on unit local "
	       "functions", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "const" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_const_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			int flags, bool *no_add_attrs)
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

  /* void __builtin_unreachable (void) is const.  Accept other such
     built-ins but warn on user-defined functions that return void.  */
  if (!(flags & ATTR_FLAG_BUILT_IN)
      && TREE_CODE (*node) == FUNCTION_DECL
      && VOID_TYPE_P (TREE_TYPE (type)))
    warning (OPT_Wattributes, "%qE attribute on function "
	     "returning %<void%>", name);

  return NULL_TREE;
}

/* Handle a "pure" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_pure_attribute (tree *node, tree name, tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    {
      tree type = TREE_TYPE (*node);
      if (VOID_TYPE_P (TREE_TYPE (type)))
	warning (OPT_Wattributes, "%qE attribute on function "
		 "returning %<void%>", name);

      DECL_PURE_P (*node) = 1;
      /* ??? TODO: Support types.  */
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "no vops" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_novops_attribute (tree *node, tree ARG_UNUSED (name),
			 tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			 bool *ARG_UNUSED (no_add_attrs))
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);
  DECL_IS_NOVOPS (*node) = 1;
  return NULL_TREE;
}

/* Handle a "nothrow" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_nothrow_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_NOTHROW (*node) = 1;
  /* ??? TODO: Support types.  */
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "type_generic" attribute.  */

tree
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

/* Handle a "returns_twice" attribute.  */

tree
handle_returns_twice_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			 int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_IS_RETURNS_TWICE (*node) = 1;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "fn spec" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
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

tree
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

tree
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

/* Handle a "noinline" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
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

tree
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

tree
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

tree
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
   struct attribute_spec.handler.  */

tree
handle_alias_attribute (tree *node, tree name, tree args,
			int ARG_UNUSED (flags), bool *no_add_attrs)
{
  return handle_alias_ifunc_attribute (true, node, name, args, no_add_attrs);
}

/* Handle a "section" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_section_attribute (tree *node, tree name, tree args,
			  int flags, bool *no_add_attrs)
{
  tree decl = *node;
  tree res = NULL_TREE;
  tree argval = TREE_VALUE (args);
  const char* new_section_name;

  if (!targetm_common.have_named_sections)
    {
      error_at (DECL_SOURCE_LOCATION (*node),
		"section attributes are not supported for this target");
      goto fail;
    }

  if (!VAR_OR_FUNCTION_DECL_P (decl))
    {
      error ("section attribute not allowed for %q+D", *node);
      goto fail;
    }

  if (TREE_CODE (argval) != STRING_CST)
    {
      error ("section attribute argument not a string constant");
      goto fail;
    }

  if (VAR_P (decl)
      && current_function_decl != NULL_TREE
      && !TREE_STATIC (decl))
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"section attribute cannot be specified for local variables");
      goto fail;
    }

  new_section_name = TREE_STRING_POINTER (argval);

  /* The decl may have already been given a section attribute
     from a previous declaration.  Ensure they match.  */
  if (const char* const old_section_name = DECL_SECTION_NAME (decl))
    if (strcmp (old_section_name, new_section_name) != 0)
      {
	error ("section of %q+D conflicts with previous declaration",
	       *node);
	goto fail;
      }

  if (VAR_P (decl)
      && !targetm.have_tls && targetm.emutls.tmpl_section
      && DECL_THREAD_LOCAL_P (decl))
    {
      error ("section of %q+D cannot be overridden", *node);
      goto fail;
    }

  if (!validate_attr_arg (node, name, argval))
    goto fail;

  res = targetm.handle_generic_attribute (node, name, args, flags,
					  no_add_attrs);

  /* If the back end confirms the attribute can be added then continue onto
     final processing.  */
  if (!(*no_add_attrs))
    {
      set_decl_section_name (decl, new_section_name);
      return res;
    }

fail:
  *no_add_attrs = true;
  return res;
}

/* Handle a "retain" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_retain_attribute (tree *pnode, tree name, tree ARG_UNUSED (args),
			 int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree node = *pnode;

  if (SUPPORTS_SHF_GNU_RETAIN
      && (TREE_CODE (node) == FUNCTION_DECL
	  || (VAR_P (node) && TREE_STATIC (node))))
    ;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Part of the `malloc` attribute handling which is shared by libgccjit and
   the C-family front-end.  */

tree
handle_malloc_common (tree *node, tree name, tree ARG_UNUSED (args),
		      int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored; valid only "
	       "for functions", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  tree rettype = TREE_TYPE (TREE_TYPE (*node));
  if (!POINTER_TYPE_P (rettype))
    {
      warning (OPT_Wattributes, "%qE attribute ignored on functions "
	       "returning %qT; valid only for pointer return types",
	       name, rettype);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  DECL_IS_MALLOC (*node) = 1;
  return NULL_TREE;
}
