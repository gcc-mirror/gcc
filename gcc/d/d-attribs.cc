/* d-attribs.c -- D attributes handling.
   Copyright (C) 2015-2019 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Implementation of attribute handlers for user defined attributes and
   internal built-in functions.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/declaration.h"
#include "dmd/mtype.h"

#include "tree.h"
#include "diagnostic.h"
#include "tm.h"
#include "cgraph.h"
#include "toplev.h"
#include "target.h"
#include "common/common-target.h"
#include "stringpool.h"
#include "attribs.h"
#include "varasm.h"

#include "d-tree.h"


/* Internal attribute handlers for built-in functions.  */
static tree handle_noreturn_attribute (tree *, tree, tree, int, bool *);
static tree handle_leaf_attribute (tree *, tree, tree, int, bool *);
static tree handle_const_attribute (tree *, tree, tree, int, bool *);
static tree handle_malloc_attribute (tree *, tree, tree, int, bool *);
static tree handle_pure_attribute (tree *, tree, tree, int, bool *);
static tree handle_novops_attribute (tree *, tree, tree, int, bool *);
static tree handle_nonnull_attribute (tree *, tree, tree, int, bool *);
static tree handle_nothrow_attribute (tree *, tree, tree, int, bool *);
static tree handle_type_generic_attribute (tree *, tree, tree, int, bool *);
static tree handle_transaction_pure_attribute (tree *, tree, tree, int, bool *);
static tree handle_returns_twice_attribute (tree *, tree, tree, int, bool *);
static tree handle_fnspec_attribute (tree *, tree, tree, int, bool *);

/* D attribute handlers for user defined attributes.  */
static tree d_handle_noinline_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_forceinline_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_flatten_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_target_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_noclone_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_section_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_alias_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_weak_attribute (tree *, tree, tree, int, bool *) ;

/* Helper to define attribute exclusions.  */
#define ATTR_EXCL(name, function, type, variable)	\
  { name, function, type, variable }

/* Define attributes that are mutually exclusive with one another.  */
static const struct attribute_spec::exclusions attr_noreturn_exclusions[] =
{
  ATTR_EXCL ("const", true, true, true),
  ATTR_EXCL ("malloc", true, true, true),
  ATTR_EXCL ("pure", true, true, true),
  ATTR_EXCL ("returns_twice", true, true, true),
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

static const struct attribute_spec::exclusions attr_inline_exclusions[] =
{
  ATTR_EXCL ("noinline", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_noinline_exclusions[] =
{
  ATTR_EXCL ("forceinline", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

/* Helper to define an attribute.  */
#define ATTR_SPEC(name, min_len, max_len, decl_req, type_req, fn_type_req, \
		  affects_type_identity, handler, exclude)		   \
  { name, min_len, max_len, decl_req, type_req, fn_type_req,		   \
    affects_type_identity, handler, exclude }

/* Table of machine-independent attributes.
   For internal use (marking of built-ins) only.  */
const attribute_spec d_langhook_common_attribute_table[] =
{
  ATTR_SPEC ("noreturn", 0, 0, true, false, false, false,
	     handle_noreturn_attribute, attr_noreturn_exclusions),
  ATTR_SPEC ("leaf", 0, 0, true, false, false, false,
	     handle_leaf_attribute, NULL),
  ATTR_SPEC ("const", 0, 0, true, false, false, false,
	     handle_const_attribute, attr_const_pure_exclusions),
  ATTR_SPEC ("malloc", 0, 0, true, false, false, false,
	     handle_malloc_attribute, NULL),
  ATTR_SPEC ("returns_twice", 0, 0, true, false, false, false,
	     handle_returns_twice_attribute, attr_returns_twice_exclusions),
  ATTR_SPEC ("pure", 0, 0, true, false, false, false,
	     handle_pure_attribute, attr_const_pure_exclusions),
  ATTR_SPEC ("nonnull", 0, -1, false, true, true, false,
	     handle_nonnull_attribute, NULL),
  ATTR_SPEC ("nothrow", 0, 0, true, false, false, false,
	     handle_nothrow_attribute, NULL),
  ATTR_SPEC ("transaction_pure", 0, 0, false, true, true, false,
	     handle_transaction_pure_attribute, NULL),
  ATTR_SPEC ("no vops", 0, 0, true, false, false, false,
	     handle_novops_attribute, NULL),
  ATTR_SPEC ("type generic", 0, 0, false, true, true, false,
	     handle_type_generic_attribute, NULL),
  ATTR_SPEC ("fn spec", 1, 1, false, true, true, false,
	     handle_fnspec_attribute, NULL),
  ATTR_SPEC (NULL, 0, 0, false, false, false, false, NULL, NULL),
};

/* Table of D language attributes exposed by `gcc.attribute' UDAs.  */
const attribute_spec d_langhook_attribute_table[] =
{
  ATTR_SPEC ("noinline", 0, 0, true, false, false, false,
	     d_handle_noinline_attribute, attr_noinline_exclusions),
  ATTR_SPEC ("forceinline", 0, 0, true, false, false, false,
	     d_handle_forceinline_attribute, attr_inline_exclusions),
  ATTR_SPEC ("flatten", 0, 0, true, false, false, false,
	     d_handle_flatten_attribute, NULL),
  ATTR_SPEC ("target", 1, -1, true, false, false, false,
	     d_handle_target_attribute, NULL),
  ATTR_SPEC ("noclone", 0, 0, true, false, false, false,
	     d_handle_noclone_attribute, NULL),
  ATTR_SPEC ("section", 1, 1, true, false, false, false,
	     d_handle_section_attribute, NULL),
  ATTR_SPEC ("alias", 1, 1, true, false, false, false,
	     d_handle_alias_attribute, NULL),
  ATTR_SPEC ("weak", 0, 0, true, false, false, false,
	     d_handle_weak_attribute, NULL),
  ATTR_SPEC (NULL, 0, 0, false, false, false, false, NULL, NULL),
};


/* Insert the type attribute ATTRNAME with value VALUE into TYPE.
   Returns a new variant of the original type declaration.  */

tree
insert_type_attribute (tree type, const char *attrname, tree value)
{
  tree ident = get_identifier (attrname);

  if (value)
    value = tree_cons (NULL_TREE, value, NULL_TREE);

  tree attribs = merge_attributes (TYPE_ATTRIBUTES (type),
				   tree_cons (ident, value, NULL_TREE));

  return build_type_attribute_variant (type, attribs);
}

/* Insert the decl attribute ATTRNAME with value VALUE into DECL.  */

tree
insert_decl_attribute (tree decl, const char *attrname, tree value)
{
  tree ident = get_identifier (attrname);

  if (value)
    value = tree_cons (NULL_TREE, value, NULL_TREE);

  tree attribs = merge_attributes (DECL_ATTRIBUTES (decl),
				   tree_cons (ident, value, NULL_TREE));

  return build_decl_attribute_variant (decl, attribs);
}

/* Returns TRUE if NAME is an attribute recognized as being handled by
   the `gcc.attribute' module.  */

static bool
uda_attribute_p (const char *name)
{
  tree ident = get_identifier (name);

  /* Search both our language, and target attribute tables.
     Common and format attributes are kept internal.  */
  for (const attribute_spec *p = d_langhook_attribute_table; p->name; p++)
    {
      if (get_identifier (p->name) == ident)
	return true;
    }

  for (const attribute_spec *p = targetm.attribute_table; p->name; p++)
    {
      if (get_identifier (p->name) == ident)
	return true;
    }

  return false;
}

/* [attribute/uda]

   User Defined Attributes (UDA) are compile time expressions that can be
   attached to a declaration.  These attributes can then be queried, extracted,
   and manipulated at compile-time.  There is no run-time component to them.

   Expand and merge all UDAs found in the EATTRS list that are of type
   `gcc.attribute.Attribute'.  This symbol is internally recognized by the
   compiler and maps them to their equivalent GCC attribute.  */

tree
build_attributes (Expressions *eattrs)
{
  if (!eattrs)
    return NULL_TREE;

  expandTuples (eattrs);

  tree attribs = NULL_TREE;

  for (size_t i = 0; i < eattrs->dim; i++)
    {
      Expression *attr = (*eattrs)[i];
      Dsymbol *sym = attr->type->toDsymbol (0);

      if (!sym)
	continue;

      /* Attribute symbol must come from the `gcc.attribute' module.  */
      Dsymbol *mod = (Dsymbol*) sym->getModule ();
      if (!(strcmp (mod->toChars (), "attribute") == 0
	    && mod->parent != NULL
	    && strcmp (mod->parent->toChars (), "gcc") == 0
	    && !mod->parent->parent))
	continue;

      /* Get the result of the attribute if it hasn't already been folded.  */
      if (attr->op == TOKcall)
	attr = attr->ctfeInterpret ();

      /* Should now have a struct `Attribute("attrib", "value", ...)'
	 initializer list.  */
      gcc_assert (attr->op == TOKstructliteral);
      Expressions *elems = ((StructLiteralExp*) attr)->elements;
      Expression *e0 = (*elems)[0];

      if (e0->op != TOKstring)
	{
	  error ("expected string attribute, not %qs", e0->toChars ());
	  return error_mark_node;
	}

      StringExp *se = (StringExp*) e0;
      gcc_assert (se->sz == 1);

      /* Empty string attribute, just ignore it.  */
      if (se->len == 0)
	continue;

      /* Check if the attribute is recognized and handled.
	 Done here to report the diagnostic at the right location.  */
      const char *name = (const char *)(se->len ? se->string : "");
      if (!uda_attribute_p (name))
	{
	  warning_at (make_location_t (e0->loc), OPT_Wattributes,
		      "unknown attribute %qs", name);
	  return error_mark_node;
	}

      /* Chain all attribute arguments together.  */
      tree args = NULL_TREE;

      for (size_t j = 1; j < elems->dim; j++)
	{
	  Expression *e = (*elems)[j];
	  tree t;
	  if (e->op == TOKstring && ((StringExp *) e)->sz == 1)
	    {
	      StringExp *s = (StringExp *) e;
	      const char *string = (const char *)(s->len ? s->string : "");
	      t = build_string (s->len, string);
	    }
	  else
	    t = build_expr (e);

	  args = chainon (args, build_tree_list (0, t));
	}

      tree list = build_tree_list (get_identifier (name), args);
      attribs = chainon (attribs, list);
    }

  return attribs;
}

/* Built-in attribute handlers.  */

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
      warning (OPT_Wattributes, "%qE attribute has no effect", name);
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
  tree type = TREE_TYPE (*node);

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

tree
handle_malloc_attribute (tree *node, tree ARG_UNUSED (name),
			 tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			 bool * ARG_UNUSED (no_add_attrs))
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL
	      && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (*node))));
  DECL_IS_MALLOC (*node) = 1;
  return NULL_TREE;
}

/* Handle a "pure" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_pure_attribute (tree *node, tree ARG_UNUSED (name),
		       tree ARG_UNUSED (args), int ARG_UNUSED (flags),
		       bool * ARG_UNUSED (no_add_attrs))
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);
  DECL_PURE_P (*node) = 1;
  return NULL_TREE;
}

/* Handle a "no vops" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_novops_attribute (tree *node, tree ARG_UNUSED (name),
			 tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			 bool * ARG_UNUSED (no_add_attrs))
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
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);
  TREE_NOTHROW (*node) = 1;
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

/* Language specific attribute handlers.  */

/* Handle a "noinline" attribute.  */

static tree
d_handle_noinline_attribute (tree *node, tree name,
			     tree ARG_UNUSED (args),
			     int ARG_UNUSED (flags), bool *no_add_attrs)
{
  Type *t = TYPE_LANG_FRONTEND (TREE_TYPE (*node));

  if (t->ty == Tfunction)
    DECL_UNINLINABLE (*node) = 1;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "forceinline" attribute.  */

static tree
d_handle_forceinline_attribute (tree *node, tree name,
				tree ARG_UNUSED (args),
				int ARG_UNUSED (flags),
				bool *no_add_attrs)
{
  Type *t = TYPE_LANG_FRONTEND (TREE_TYPE (*node));

  if (t->ty == Tfunction)
    {
      tree attributes = DECL_ATTRIBUTES (*node);

      /* Push attribute always_inline.  */
      if (! lookup_attribute ("always_inline", attributes))
	DECL_ATTRIBUTES (*node) = tree_cons (get_identifier ("always_inline"),
					     NULL_TREE, attributes);

      DECL_DECLARED_INLINE_P (*node) = 1;
      DECL_NO_INLINE_WARNING_P (*node) = 1;
      DECL_DISREGARD_INLINE_LIMITS (*node) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "flatten" attribute.  */

static tree
d_handle_flatten_attribute (tree *node, tree name,
			    tree args ATTRIBUTE_UNUSED,
			    int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  Type *t = TYPE_LANG_FRONTEND (TREE_TYPE (*node));

  if (t->ty != Tfunction)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "target" attribute.  */

static tree
d_handle_target_attribute (tree *node, tree name, tree args, int flags,
			   bool *no_add_attrs)
{
  Type *t = TYPE_LANG_FRONTEND (TREE_TYPE (*node));

  /* Ensure we have a function type.  */
  if (t->ty != Tfunction)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else if (! targetm.target_option.valid_attribute_p (*node, name, args, flags))
    *no_add_attrs = true;

  return NULL_TREE;
}

/* Handle a "noclone" attribute.  */

static tree
d_handle_noclone_attribute (tree *node, tree name,
				tree ARG_UNUSED (args),
				int ARG_UNUSED (flags),
				bool *no_add_attrs)
{
  Type *t = TYPE_LANG_FRONTEND (TREE_TYPE (*node));

  if (t->ty == Tfunction)
    {
      tree attributes = DECL_ATTRIBUTES (*node);

      /* Push attribute noclone.  */
      if (! lookup_attribute ("noclone", attributes))
	DECL_ATTRIBUTES (*node) = tree_cons (get_identifier ("noclone"),
					     NULL_TREE, attributes);
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "section" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_section_attribute (tree *node, tree ARG_UNUSED (name), tree args,
			    int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;

  if (targetm_common.have_named_sections)
    {
      if (VAR_OR_FUNCTION_DECL_P (decl)
	  && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
	{
	  if (VAR_P (decl)
	      && current_function_decl != NULL_TREE
	      && !TREE_STATIC (decl))
	    {
	      error_at (DECL_SOURCE_LOCATION (decl),
			"section attribute cannot be specified for "
			"local variables");
	      *no_add_attrs = true;
	    }

	  /* The decl may have already been given a section attribute
	     from a previous declaration.  Ensure they match.  */
	  else if (DECL_SECTION_NAME (decl) != NULL
		   && strcmp (DECL_SECTION_NAME (decl),
			      TREE_STRING_POINTER (TREE_VALUE (args))) != 0)
	    {
	      error ("section of %q+D conflicts with previous declaration",
		     *node);
	      *no_add_attrs = true;
	    }
	  else if (VAR_P (decl)
		   && !targetm.have_tls && targetm.emutls.tmpl_section
		   && DECL_THREAD_LOCAL_P (decl))
	    {
	      error ("section of %q+D cannot be overridden", *node);
	      *no_add_attrs = true;
	    }
	  else
	    set_decl_section_name (decl,
				   TREE_STRING_POINTER (TREE_VALUE (args)));
	}
      else
	{
	  error ("section attribute not allowed for %q+D", *node);
	  *no_add_attrs = true;
	}
    }
  else
    {
      error_at (DECL_SOURCE_LOCATION (*node),
		"section attributes are not supported for this target");
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "alias" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_alias_attribute (tree *node, tree ARG_UNUSED (name),
			  tree args, int ARG_UNUSED (flags),
			  bool *no_add_attrs ATTRIBUTE_UNUSED)
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL
      && TREE_CODE (decl) != VAR_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
      return NULL_TREE;
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
  else if (decl_function_context (decl))
    {
      error ("%q+D alias functions must be global", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }
  else
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

      return NULL_TREE;
    }
}

/* Handle a "weak" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_weak_attribute (tree *node, tree name,
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
  else if (VAR_OR_FUNCTION_DECL_P (*node))
    {
      struct symtab_node *n = symtab_node::get (*node);
      if (n && n->refuse_visibility_changes)
	error ("%q+D declared weak after being used", *node);
      declare_weak (*node);
    }
  else
    warning (OPT_Wattributes, "%qE attribute ignored", name);

  return NULL_TREE;
}

