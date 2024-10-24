/* d-attribs.c -- D attributes handling.
   Copyright (C) 2015-2024 Free Software Foundation, Inc.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/attrib.h"
#include "dmd/declaration.h"
#include "dmd/expression.h"
#include "dmd/module.h"
#include "dmd/mtype.h"
#include "dmd/template.h"

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
#include "fold-const.h"
#include "opts.h"

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
static tree handle_omp_declare_simd_attribute (tree *, tree, tree, int, bool *);

/* D attribute handlers for user defined attributes.  */
static tree d_handle_noinline_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_always_inline_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_flatten_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_target_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_target_clones_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_optimize_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_noclone_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_noicf_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_noipa_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_section_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_symver_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_weak_attribute (tree *, tree, tree, int, bool *) ;
static tree d_handle_noplt_attribute (tree *, tree, tree, int, bool *) ;
static tree d_handle_alloc_size_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_cold_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_register_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_restrict_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_used_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_visibility_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_no_sanitize_attribute (tree *, tree, tree, int, bool *);
static tree d_handle_simd_attribute (tree *, tree, tree, int, bool *);

/* Helper to define attribute exclusions.  */
#define ATTR_EXCL(name, function, type, variable)	\
  { name, function, type, variable }

/* Define attributes that are mutually exclusive with one another.  */
static const struct attribute_spec::exclusions attr_noreturn_exclusions[] =
{
  ATTR_EXCL ("alloc_size", true, true, true),
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
  ATTR_EXCL ("alloc_size", true, true, true),
  ATTR_EXCL ("const", true, true, true),
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL ("pure", true, true, true),
  ATTR_EXCL (NULL, false, false, false)
};

static const struct attribute_spec::exclusions attr_inline_exclusions[] =
{
  ATTR_EXCL ("noinline", true, true, true),
  ATTR_EXCL ("target_clones", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_noinline_exclusions[] =
{
  ATTR_EXCL ("always_inline", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_target_exclusions[] =
{
  ATTR_EXCL ("target_clones", TARGET_HAS_FMV_TARGET_ATTRIBUTE,
	     TARGET_HAS_FMV_TARGET_ATTRIBUTE, TARGET_HAS_FMV_TARGET_ATTRIBUTE),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_target_clones_exclusions[] =
{
  ATTR_EXCL ("always_inline", true, true, true),
  ATTR_EXCL ("target", TARGET_HAS_FMV_TARGET_ATTRIBUTE,
	     TARGET_HAS_FMV_TARGET_ATTRIBUTE, TARGET_HAS_FMV_TARGET_ATTRIBUTE),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_alloc_exclusions[] =
{
  ATTR_EXCL ("const", true, true, true),
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL ("pure", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

extern const struct attribute_spec::exclusions attr_cold_hot_exclusions[] =
{
  ATTR_EXCL ("cold", true, true, true),
  ATTR_EXCL ("hot", true, true, true),
  ATTR_EXCL (NULL, false, false, false)
};

/* Helper to define an attribute.  */
#define ATTR_SPEC(name, min_len, max_len, decl_req, type_req, fn_type_req, \
		  affects_type_identity, handler, exclude)		   \
  { name, min_len, max_len, decl_req, type_req, fn_type_req,		   \
    affects_type_identity, handler, exclude }

/* Table of machine-independent attributes.
   For internal use (marking of built-ins) only.  */
static const attribute_spec d_langhook_common_attributes[] =
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
  ATTR_SPEC ("omp declare simd", 0, -1, true,  false, false, false,
	     handle_omp_declare_simd_attribute, NULL),
};

const scoped_attribute_specs d_langhook_common_attribute_table =
{
  "gnu", { d_langhook_common_attributes }
};

/* Table of D language attributes exposed by `gcc.attribute' UDAs.  */
static const attribute_spec d_langhook_gnu_attributes[] =
{
  ATTR_SPEC ("noinline", 0, 0, true, false, false, false,
	     d_handle_noinline_attribute, attr_noinline_exclusions),
  ATTR_SPEC ("always_inline", 0, 0, true,  false, false, false,
	     d_handle_always_inline_attribute, attr_inline_exclusions),
  ATTR_SPEC ("flatten", 0, 0, true, false, false, false,
	     d_handle_flatten_attribute, NULL),
  ATTR_SPEC ("target", 1, -1, true, false, false, false,
	     d_handle_target_attribute, attr_target_exclusions),
  ATTR_SPEC ("target_clones", 1, -1, true, false, false, false,
	     d_handle_target_clones_attribute, attr_target_clones_exclusions),
  ATTR_SPEC ("optimize", 1, -1, true, false, false, false,
	     d_handle_optimize_attribute, NULL),
  ATTR_SPEC ("noclone", 0, 0, true, false, false, false,
	     d_handle_noclone_attribute, NULL),
  ATTR_SPEC ("no_icf", 0, 0, true, false, false, false,
	     d_handle_noicf_attribute, NULL),
  ATTR_SPEC ("noipa", 0, 0, true, false, false, false,
	     d_handle_noipa_attribute, NULL),
  ATTR_SPEC ("section", 1, 1, true, false, false, false,
	     d_handle_section_attribute, NULL),
  ATTR_SPEC ("symver", 1, -1, true, false, false, false,
	     d_handle_symver_attribute, NULL),
  ATTR_SPEC ("weak", 0, 0, true, false, false, false,
	     d_handle_weak_attribute, NULL),
  ATTR_SPEC ("noplt", 0, 0, true, false, false, false,
	     d_handle_noplt_attribute, NULL),
  ATTR_SPEC ("alloc_size", 1, 3, false, true, true, false,
	     d_handle_alloc_size_attribute, attr_alloc_exclusions),
  ATTR_SPEC ("cold", 0, 0, true, false, false, false,
	     d_handle_cold_attribute, attr_cold_hot_exclusions),
  ATTR_SPEC ("no_sanitize", 1, -1, true, false, false, false,
	     d_handle_no_sanitize_attribute, NULL),
  ATTR_SPEC ("register", 1, 1, true, false, false, false,
	     d_handle_register_attribute, NULL),
  ATTR_SPEC ("restrict", 0, 0, true, false, false, false,
	     d_handle_restrict_attribute, NULL),
  ATTR_SPEC ("simd", 0, 1, true,  false, false, false,
	     d_handle_simd_attribute, NULL),
  ATTR_SPEC ("used", 0, 0, true, false, false, false,
	     d_handle_used_attribute, NULL),
  ATTR_SPEC ("visibility", 1, 1, false, false, false, false,
	     d_handle_visibility_attribute, NULL),
};

const scoped_attribute_specs d_langhook_gnu_attribute_table =
{
  "gnu", { d_langhook_gnu_attributes }
};

/* Insert the type attribute ATTRNAME with value VALUE into TYPE.
   Returns a new variant of the original type declaration.  */

tree
insert_type_attribute (tree type, const char *attrname, tree value)
{
  tree ident = get_identifier (attrname);

  if (value)
    value = tree_cons (NULL_TREE, value, NULL_TREE);

  decl_attributes (&type, build_tree_list (ident, value),
		   ATTR_FLAG_TYPE_IN_PLACE);
  return type;
}

/* Insert the decl attribute ATTRNAME with value VALUE into DECL.  */

tree
insert_decl_attribute (tree decl, const char *attrname, tree value)
{
  tree ident = get_identifier (attrname);

  if (value)
    value = tree_cons (NULL_TREE, value, NULL_TREE);

  decl_attributes (&decl, build_tree_list (ident, value), 0);

  return decl;
}

/* Returns TRUE if NAME is an attribute recognized as being handled by
   the `gcc.attribute' module.  */

static bool
uda_attribute_p (const char *name)
{
  tree ident = get_identifier (name);

  /* Search both our language, and target attribute tables.
     Common and format attributes are kept internal.  */
  for (const attribute_spec &p : d_langhook_gnu_attributes)
    if (get_identifier (p.name) == ident)
      return true;

  for (auto scoped_attributes : targetm.attribute_table)
    for (const attribute_spec &p : scoped_attributes->attributes)
      if (get_identifier (p.name) == ident)
	return true;

  return false;
}

/* [attribute/uda]

   User Defined Attributes (UDA) are compile time expressions that can be
   attached to a declaration.  These attributes can then be queried, extracted,
   and manipulated at compile-time.  There is no run-time component to them.

   Expand and merge all UDAs found in the EATTRS list that are of type
   `gcc.attribute.Attribute'.  This symbol is internally recognized by the
   compiler and maps them to their equivalent GCC attribute.  */

static tree
build_attributes (Expressions *eattrs)
{
  if (!eattrs)
    return NULL_TREE;

  dmd::expandTuples (eattrs);

  tree attribs = NULL_TREE;

  for (size_t i = 0; i < eattrs->length; i++)
    {
      Expression *attr = (*eattrs)[i];
      Dsymbol *sym = dmd::toDsymbol (attr->type, NULL);

      if (!sym)
	{
	  /* If attribute is a template symbol, perhaps arguments were not
	     supplied, so warn about attribute having no effect.  */
	  if (TemplateExp *te = attr->isTemplateExp ())
	    {
	      if (!te->td || !te->td->onemember)
		continue;

	      sym = te->td->onemember;
	    }
	  else
	    continue;
	}

      /* Attribute symbol must come from the `gcc.attribute' module.  */
      Dsymbol *mod = sym->getModule ();
      if (!(strcmp (mod->toChars (), "attributes") == 0
	    && mod->parent != NULL
	    && strcmp (mod->parent->toChars (), "gcc") == 0
	    && !mod->parent->parent))
	continue;

      /* Get the result of the attribute if it hasn't already been folded.  */
      if (attr->op == EXP::call)
	attr = dmd::ctfeInterpret (attr);

      if (attr->op != EXP::structLiteral)
	{
	  warning_at (make_location_t (attr->loc), OPT_Wattributes,
		      "%qE attribute has no effect",
		      get_identifier (sym->toChars ()));
	  continue;
	}

      /* Should now have a struct `Attribute("attrib", "value", ...)'
	 initializer list.  */
      Expressions *elems = attr->isStructLiteralExp ()->elements;
      Expression *e0 = (*elems)[0];

      if (e0->op != EXP::string_)
	{
	  warning_at (make_location_t (attr->loc), OPT_Wattributes,
		      "unknown attribute %qs", e0->toChars());
	  continue;
	}

      StringExp *se = e0->toStringExp ();
      gcc_assert (se->sz == 1);

      /* Empty string attribute, just ignore it.  */
      if (se->len == 0)
	continue;

      /* Check if the attribute is recognized and handled.
	 Done here to report the diagnostic at the right location.  */
      const char *name = (const char *)(se->len ? se->string : "");
      if (!uda_attribute_p (name))
	{
	  warning_at (make_location_t (attr->loc), OPT_Wattributes,
		      "unknown attribute %qs", name);
	  continue;
	}

      /* Chain all attribute arguments together.  */
      tree args = NULL_TREE;

      for (size_t j = 1; j < elems->length; j++)
	{
	  Expression *e = (*elems)[j];
	  /* Stop after the first `void' argument.  */
	  if (e == NULL)
	    break;

	  StringExp *s = e->isStringExp ();
	  tree t;
	  if (s != NULL && s->sz == 1)
	    {
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

/* If any GCC attributes are found in the declaration SYM, apply them to the
   type or decl NODE.  */

void
apply_user_attributes (Dsymbol *sym, tree node)
{
  UserAttributeDeclaration *uda = sym->userAttribDecl ();
  if (uda == NULL)
    return;

  location_t saved_location = input_location;
  input_location = make_location_t (sym->loc);

  int attr_flags = 0;
  if (TYPE_P (node) && !COMPLETE_TYPE_P (node))
    attr_flags |= ATTR_FLAG_TYPE_IN_PLACE;

  Expressions *attrs = uda->getAttributes ();
  decl_attributes (&node, build_attributes (attrs), attr_flags);

  input_location = saved_location;
}

/* Built-in attribute handlers.
   These functions take the arguments:
   (tree *node, tree name, tree args, int flags, bool *no_add_attrs)  */

/* Handle a "noreturn" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noreturn_attribute (tree *node, tree, tree, int, bool *)
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
handle_leaf_attribute (tree *node, tree name, tree, int, bool *no_add_attrs)
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
handle_const_attribute (tree *node, tree, tree, int, bool *)
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
handle_malloc_attribute (tree *node, tree, tree, int, bool *)
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL
	      && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (*node))));
  DECL_IS_MALLOC (*node) = 1;
  return NULL_TREE;
}

/* Handle a "pure" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_pure_attribute (tree *node, tree, tree, int, bool *)
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);
  DECL_PURE_P (*node) = 1;
  return NULL_TREE;
}

/* Handle a "no vops" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_novops_attribute (tree *node, tree, tree, int, bool *)
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
handle_nonnull_attribute (tree *node, tree, tree args, int, bool *)
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
handle_nothrow_attribute (tree *node, tree, tree, int, bool *)
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);
  TREE_NOTHROW (*node) = 1;
  return NULL_TREE;
}

/* Handle a "type generic" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_type_generic_attribute (tree *node, tree, tree, int, bool *)
{
  /* Ensure we have a function type.  */
  gcc_assert (TREE_CODE (*node) == FUNCTION_TYPE);

  /* Ensure we have a variadic function.  */
  gcc_assert (!prototype_p (*node) || stdarg_p (*node));

  return NULL_TREE;
}

/* Handle a "transaction_pure" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_transaction_pure_attribute (tree *node, tree, tree, int, bool *)
{
  /* Ensure we have a function type.  */
  gcc_assert (TREE_CODE (*node) == FUNCTION_TYPE);

  return NULL_TREE;
}

/* Handle a "returns_twice" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_returns_twice_attribute (tree *node, tree, tree, int, bool *)
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);

  DECL_IS_RETURNS_TWICE (*node) = 1;

  return NULL_TREE;
}

/* Handle a "fn spec" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_fnspec_attribute (tree *, tree, tree args, int, bool *)
{
  gcc_assert (args
	      && TREE_CODE (TREE_VALUE (args)) == STRING_CST
	      && !TREE_CHAIN (args));
  return NULL_TREE;
}

/* Handle an "omp declare simd" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_omp_declare_simd_attribute (tree *node, tree, tree, int, bool *)
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);
  return NULL_TREE;
}

/* Language specific attribute handlers.
   These functions take the arguments:
   (tree *node, tree name, tree args, int flags, bool *no_add_attrs)  */

/* Handle a "noinline" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_noinline_attribute (tree *node, tree name, tree, int,
			     bool *no_add_attrs)
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

/* Handle a "always_inline" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_always_inline_attribute (tree *node, tree name, tree, int,
				  bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    {
      DECL_DECLARED_INLINE_P (*node) = 1;
      DECL_DISREGARD_INLINE_LIMITS (*node) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "flatten" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_flatten_attribute (tree *node, tree name, tree, int,
			    bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "target" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_target_attribute (tree *node, tree name, tree args, int flags,
			   bool *no_add_attrs)
{
  /* Ensure we have a function type.  */
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else if (!targetm.target_option.valid_attribute_p (*node, name, args, flags))
    *no_add_attrs = true;

  /* Check that there's no empty string in values of the attribute.  */
  for (tree t = args; t != NULL_TREE; t = TREE_CHAIN (t))
    {
      tree value = TREE_VALUE (t);
      if (TREE_CODE (value) != STRING_CST
	  || (TREE_STRING_LENGTH (value) != 0
	      && TREE_STRING_POINTER (value)[0] != '\0'))
	continue;

      warning (OPT_Wattributes, "empty string in attribute %<target%>");
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "target_clones" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_target_clones_attribute (tree *node, tree name, tree, int,
				  bool *no_add_attrs)
{
  /* Ensure we have a function type.  */
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else
    {
      /* Do not inline functions with multiple clone targets.  */
      DECL_UNINLINABLE (*node) = 1;
    }

  return NULL_TREE;
}

/* Arguments being collected for optimization.  */
static GTY(()) vec <const char *, va_gc> *optimize_args;

/* Inner function to convert a TREE_LIST to argv string to parse the optimize
   options in ARGS.  */

static bool
parse_optimize_options (tree args)
{
  bool ret = true;

  /* Build up argv vector.  Just in case the string is stored away, use garbage
     collected strings.  */
  vec_safe_truncate (optimize_args, 0);
  vec_safe_push (optimize_args, (const char *) NULL);

  for (tree ap = args; ap != NULL_TREE; ap = TREE_CHAIN (ap))
    {
      tree value = TREE_VALUE (ap);

      if (TREE_CODE (value) == INTEGER_CST)
	{
	  char buffer[20];
	  sprintf (buffer, "-O%ld", (long) TREE_INT_CST_LOW (value));
	  vec_safe_push (optimize_args, ggc_strdup (buffer));
	}
      else if (TREE_CODE (value) == STRING_CST)
	{
	  size_t len = TREE_STRING_LENGTH (value);
	  const char *p = TREE_STRING_POINTER (value);

	  /* If the user supplied -Oxxx or -fxxx, only allow -Oxxx or -fxxx
	     options.  */
	  if (*p == '-' && p[1] != 'O' && p[1] != 'f')
	    {
	      ret = false;
	      warning (OPT_Wattributes,
		       "bad option %qs to attribute %<optimize%>", p);
	      continue;
	    }

	  /* Can't use GC memory here.  */
	  char *q = XOBNEWVEC (&opts_obstack, char, len + 3);
	  char *r = q;

	  if (*p != '-')
	    {
	      *r++ = '-';

	      /* Assume that Ox is -Ox, a numeric value is -Ox, a s by
		 itself is -Os, and any other switch begins with a -f.  */
	      if ((*p >= '0' && *p <= '9') || (p[0] == 's' && p[1] == '\0'))
		*r++ = 'O';
	      else if (*p != 'O')
		*r++ = 'f';
	    }

	  memcpy (r, p, len);
	  r[len] = '\0';
	  vec_safe_push (optimize_args, (const char *) q);
	}
    }

  unsigned opt_argc = optimize_args->length ();
  const char **opt_argv
    = (const char **) alloca (sizeof (char *) * (opt_argc + 1));

  for (unsigned i = 1; i < opt_argc; i++)
    opt_argv[i] = (*optimize_args)[i];

  /* Now parse the options.  */
  struct cl_decoded_option *decoded_options;
  unsigned int decoded_options_count;

  decode_cmdline_options_to_array_default_mask (opt_argc, opt_argv,
						&decoded_options,
						&decoded_options_count);
  /* Drop non-Optimization options.  */
  unsigned j = 1;
  for (unsigned i = 1; i < decoded_options_count; ++i)
    {
      unsigned opt_index = decoded_options[i].opt_index;
      if (opt_index >= cl_options_count
	  || ! (cl_options[opt_index].flags & CL_OPTIMIZATION))
	{
	  ret = false;
	  warning (OPT_Wattributes,
		   "bad option %qs to attribute %<optimize%>",
		   decoded_options[i].orig_option_with_args_text);
	  continue;
	}
      if (i != j)
	decoded_options[j] = decoded_options[i];
      j++;
    }
  decoded_options_count = j;
  /* And apply them.  */
  decode_options (&global_options, &global_options_set,
		  decoded_options, decoded_options_count,
		  input_location, global_dc, NULL);

  targetm.override_options_after_change();

  optimize_args->truncate (0);
  return ret;
}

/* Handle a "optimize" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_optimize_attribute (tree *node, tree name, tree args, int,
			     bool *no_add_attrs)
{
  /* Ensure we have a function type.  */
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else
    {
      struct cl_optimization cur_opts;
      tree old_opts = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (*node);

      /* Save current options.  */
      cl_optimization_save (&cur_opts, &global_options, &global_options_set);
      tree prev_target_node = build_target_option_node (&global_options,
							&global_options_set);

      /* If we previously had some optimization options, use them as the
	 default.  */
      gcc_options *saved_global_options = NULL;
      if (flag_checking)
	{
	  saved_global_options = XNEW (gcc_options);
	  *saved_global_options = global_options;
	}

      if (old_opts)
	cl_optimization_restore (&global_options, &global_options_set,
				 TREE_OPTIMIZATION (old_opts));

      /* Parse options, and update the vector.  */
      parse_optimize_options (args);
      DECL_FUNCTION_SPECIFIC_OPTIMIZATION (*node)
	= build_optimization_node (&global_options, &global_options_set);
      tree target_node = build_target_option_node (&global_options,
						   &global_options_set);
      if (prev_target_node != target_node)
	DECL_FUNCTION_SPECIFIC_TARGET (*node) = target_node;

      /* Restore current options.  */
      cl_optimization_restore (&global_options, &global_options_set,
			       &cur_opts);
      cl_target_option_restore (&global_options, &global_options_set,
				TREE_TARGET_OPTION (prev_target_node));
      if (saved_global_options != NULL)
	{
	  cl_optimization_compare (saved_global_options, &global_options);
	  free (saved_global_options);
	}
    }

  return NULL_TREE;
}

/* Handle a "noclone" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_noclone_attribute (tree *node, tree name, tree, int,
			    bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "no_icf" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_noicf_attribute (tree *node, tree name, tree, int,
			  bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "noipa" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_noipa_attribute (tree *node, tree name, tree, int,
			  bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "section" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_section_attribute (tree *node, tree name, tree args, int flags,
			    bool *no_add_attrs)
{
  if (!targetm_common.have_named_sections)
    {
      error ("section attributes are not supported for this target");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (!VAR_OR_FUNCTION_DECL_P (*node))
    {
      error ("section attribute not allowed for %q+D", *node);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (TREE_CODE (TREE_VALUE (args)) != STRING_CST)
    {
      error ("%qE attribute argument not a string constant", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (VAR_P (*node)
      && current_function_decl != NULL_TREE
      && !TREE_STATIC (*node))
    {
      error ("section attribute cannot be specified for local variables");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* The decl may have already been given a section attribute
     from a previous declaration.  Ensure they match.  */
  if (DECL_SECTION_NAME (*node) != NULL
      && strcmp (DECL_SECTION_NAME (*node),
		 TREE_STRING_POINTER (TREE_VALUE (args))) != 0)
    {
      error ("section of %q+D conflicts with previous declaration", *node);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (VAR_P (*node)
      && !targetm.have_tls && targetm.emutls.tmpl_section
      && DECL_THREAD_LOCAL_P (*node))
    {
      error ("section of %q+D cannot be overridden", *node);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  tree res = targetm.handle_generic_attribute (node, name, args, flags,
					       no_add_attrs);

  /* If the back end confirms the attribute can be added then continue onto
     final processing.  */
  if (*no_add_attrs)
    return NULL_TREE;

  set_decl_section_name (*node, TREE_STRING_POINTER (TREE_VALUE (args)));
  return res;
}

/* Handle a "symver" and attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_symver_attribute (tree *node, tree name, tree args, int,
			   bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL && TREE_CODE (*node) != VAR_DECL)
    {
      warning (OPT_Wattributes,
	       "%<symver%> attribute only applies to functions and variables");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (!decl_in_symtab_p (*node))
    {
      warning (OPT_Wattributes,
	       "%<symver%> attribute is only applicable to symbols");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  for (; args; args = TREE_CHAIN (args))
    {
      tree symver = TREE_VALUE (args);
      if (TREE_CODE (symver) != STRING_CST)
	{
	  error ("%qE attribute argument not a string constant", name);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}

      const char *symver_str = TREE_STRING_POINTER (symver);

      int ats = 0;
      for (int n = 0; (int)n < TREE_STRING_LENGTH (symver); n++)
	if (symver_str[n] == '@')
	  ats++;

      if (ats != 1 && ats != 2)
	{
	  error ("symver attribute argument must have format %<name@nodename%>");
	  error ("%<symver%> attribute argument %qs must contain one or two "
		 "%<@%>", symver_str);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
    }

  return NULL_TREE;
}

/* Handle a "weak" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_weak_attribute (tree *node, tree name, tree, int, bool *no_add_attrs)
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

/* Handle a "noplt" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_noplt_attribute (tree *node, tree name, tree, int, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Verify that argument value POS at position ARGNO to attribute ATNAME applied
   to function FNTYPE refers to a function parameter at position POS and is a
   valid integer type.  When ZERO_BASED is true, POS is adjusted to be 1-based.
   If successful, POS is returned.  Otherwise, issue appropriate warnings and
   return null.  A non-zero 1-based ARGNO should be passed in by callers only
   for attributes with more than one argument.  */

static tree
positional_argument (const_tree fntype, const_tree atname, tree pos,
		     int argno, bool zero_based)
{
  tree postype = TREE_TYPE (pos);

  if (pos == error_mark_node || !postype)
    {
      /* Only mention the positional argument number when it's non-zero.  */
      if (argno < 1)
	warning (OPT_Wattributes,
		 "%qE attribute argument is invalid", atname);
      else
	warning (OPT_Wattributes,
		 "%qE attribute argument %i is invalid", atname, argno);

      return NULL_TREE;
    }

  if (!INTEGRAL_TYPE_P (postype))
    {
      /* Handle this case specially to avoid mentioning the value
	 of pointer constants in diagnostics.  Only mention
	 the positional argument number when it's non-zero.  */
      if (argno < 1)
	warning (OPT_Wattributes,
		 "%qE attribute argument has type %qT",
		 atname, postype);
      else
	warning (OPT_Wattributes,
		 "%qE attribute argument %i has type %qT",
		 atname, argno, postype);

      return NULL_TREE;
    }

  if (TREE_CODE (pos) != INTEGER_CST)
    {
      /* Only mention the argument number when it's non-zero.  */
      if (argno < 1)
	warning (OPT_Wattributes,
		 "%qE attribute argument value %qE is not an integer "
		 "constant",
		 atname, pos);
      else
	warning (OPT_Wattributes,
		 "%qE attribute argument %i value %qE is not an integer "
		 "constant",
		 atname, argno, pos);

      return NULL_TREE;
    }

  /* Validate the value of the position argument.  If 0-based, then it should
     not be negative.  If 1-based, it should be greater than zero.  */
  if ((zero_based && tree_int_cst_sgn (pos) < 0)
      || (!zero_based && tree_int_cst_sgn (pos) < 1))
    {
      if (argno < 1)
	warning (OPT_Wattributes,
		 "%qE attribute argument value %qE does not refer to "
		 "a function parameter",
		 atname, pos);
      else
	warning (OPT_Wattributes,
		 "%qE attribute argument %i value %qE does not refer to "
		 "a function parameter",
		 atname, argno, pos);

      return NULL_TREE;
    }

  /* Adjust the value of pos to be 1-based.  */
  tree adjusted_pos = (zero_based)
    ? int_const_binop (PLUS_EXPR, pos, integer_one_node) : pos;

  if (!prototype_p (fntype))
    return adjusted_pos;

  /* Verify that the argument position does not exceed the number
     of formal arguments to the function.  */
  unsigned nargs = type_num_arguments (fntype);
  if (!nargs
      || !tree_fits_uhwi_p (adjusted_pos)
      || !IN_RANGE (tree_to_uhwi (adjusted_pos), 1, nargs))
    {
      if (argno < 1)
	warning (OPT_Wattributes,
		 "%qE attribute argument value %qE exceeds the number "
		 "of function parameters %u",
		 atname, pos, nargs);
      else
	warning (OPT_Wattributes,
		 "%qE attribute argument %i value %qE exceeds the number "
		 "of function parameters %u",
		 atname, argno, pos, nargs);

      return NULL_TREE;
    }

  /* Verify that the type of the referenced formal argument matches
     the expected type.  */
  unsigned HOST_WIDE_INT ipos = tree_to_uhwi (adjusted_pos);

  /* Zero was handled above.  */
  gcc_assert (ipos != 0);

  if (tree argtype = type_argument_type (fntype, ipos))
    {
      /* Accept types that match INTEGRAL_TYPE_P except for bool.  */
      if (!INTEGRAL_TYPE_P (argtype) || TREE_CODE (argtype) == BOOLEAN_TYPE)
	{
	  if (argno < 1)
	    warning (OPT_Wattributes,
		     "%qE attribute argument value %qE refers to "
		     "parameter type %qT",
		     atname, pos, argtype);
	  else
	    warning (OPT_Wattributes,
		     "%qE attribute argument %i value %qE refers to "
		     "parameter type %qT",
		     atname, argno, pos, argtype);

	  return NULL_TREE;
	}

      return adjusted_pos;
    }

  /* Argument position exceeding number of parameters was handled above.  */
  gcc_unreachable ();
}

/* Handle a "alloc_size" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_alloc_size_attribute (tree *node, tree name, tree args, int,
			       bool *no_add_attrs)
{
  tree fntype = *node;
  tree rettype = TREE_TYPE (fntype);
  if (!POINTER_TYPE_P (rettype))
    {
      warning (OPT_Wattributes,
	       "%qE attribute ignored on a function returning %qT",
	       name, rettype);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* The first argument SIZE_ARG is never null.  */
  tree size_arg = TREE_VALUE (args);
  tree next = TREE_CHAIN (args);

  /* NUM_ARG is null when the attribute includes just one argument, or is
     explictly set to null if it has been left uninitialized by the caller.  */
  tree num_arg = NULL_TREE;
  if (next != NULL_TREE)
    {
      if (TREE_VALUE (next) != TYPE_MIN_VALUE (d_int_type))
	num_arg = TREE_VALUE (next);

      next = TREE_CHAIN (next);
    }

  /* If ZERO_ARG is set and true, arguments positions are treated as 0-based.
     Otherwise the default is 1-based.  */
  bool zero_based = false;
  if (next != NULL_TREE)
    zero_based = integer_truep (TREE_VALUE (next));

  /* Update the argument values with the real argument position.  */
  if (tree val = positional_argument (fntype, name, size_arg, num_arg ? 1 : 0,
				      zero_based))
    TREE_VALUE (args) = val;
  else
    *no_add_attrs = true;

  if (num_arg != NULL_TREE)
    {
      args = TREE_CHAIN (args);
      if (tree val = positional_argument (fntype, name, num_arg, 2, zero_based))
	TREE_VALUE (args) = val;
      else
	*no_add_attrs = true;
    }

  /* Terminate the original TREE_CHAIN in `args' to remove any remaining
     D-specific `alloc_size` arguments.  */
  TREE_CHAIN (args) = NULL_TREE;

  return NULL_TREE;
}

/* Handle a "cold" and attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_cold_attribute (tree *node, tree name, tree, int, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "no_sanitize" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_no_sanitize_attribute (tree *node, tree name, tree args, int,
				bool *no_add_attrs)
{
  *no_add_attrs = true;

  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      return NULL_TREE;
    }

  unsigned int flags = 0;
  for (; args; args = TREE_CHAIN (args))
    {
      tree id = TREE_VALUE (args);
      if (TREE_CODE (id) != STRING_CST)
	{
	  error ("%qE attribute argument not a string constant", name);
	  return NULL_TREE;
	}

      char *string = ASTRDUP (TREE_STRING_POINTER (id));
      flags |= parse_no_sanitize_attribute (string);
    }

  /* Store the flags argument back into no_sanitize attribute as an integer,
     merge existing flags if no_sanitize was previously handled.  */
  if (tree attr = lookup_attribute ("no_sanitize", DECL_ATTRIBUTES (*node)))
    {
      unsigned int old_value = tree_to_uhwi (TREE_VALUE (attr));
      flags |= old_value;

      if (flags != old_value)
	TREE_VALUE (attr) = build_int_cst (d_uint_type, flags);
    }
  else
    {
      DECL_ATTRIBUTES (*node) = tree_cons (get_identifier ("no_sanitize"),
					   build_int_cst (d_uint_type, flags),
					   DECL_ATTRIBUTES (*node));
    }

  return NULL_TREE;
}

/* Handle a "register" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_register_attribute (tree *node, tree name, tree args, int,
			     bool *no_add_attrs)
{
  if (!VAR_P (*node))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else if (TREE_CODE (TREE_VALUE (args)) != STRING_CST)
    {
      error ("%qE attribute argument not a string constant", name);
      *no_add_attrs = true;
    }
  else if (TREE_STRING_LENGTH (TREE_VALUE (args)) == 0
	   || TREE_STRING_POINTER (TREE_VALUE (args))[0] == '\0')
    {
      error ("register name not specified for %q+D", *node);
      *no_add_attrs = true;
    }
  else
    {
      DECL_REGISTER (*node) = 1;
      set_user_assembler_name (*node, TREE_STRING_POINTER (TREE_VALUE (args)));
      DECL_HARD_REGISTER (*node) = 1;
    }

  return NULL_TREE;
}

/* Handle a "restrict" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_restrict_attribute (tree *node, tree name, tree, int,
			     bool *no_add_attrs)
{
  if (TREE_CODE (*node) == PARM_DECL && POINTER_TYPE_P (TREE_TYPE (*node)))
    {
      TREE_TYPE (*node) = build_qualified_type (TREE_TYPE (*node),
						TYPE_QUAL_RESTRICT);
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "simd" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_simd_attribute (tree *node, tree name, tree args, int,
			 bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  tree omp_attr = get_identifier ("omp declare simd");
  tree omp_flags = NULL_TREE;
  if (args)
    {
      tree id = TREE_VALUE (args);

      if (TREE_CODE (id) != STRING_CST)
	{
	  error ("%qE attribute argument not a string constant", name);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}

      if (strcmp (TREE_STRING_POINTER (id), "notinbranch") == 0)
	omp_flags = build_omp_clause (DECL_SOURCE_LOCATION (*node),
				      OMP_CLAUSE_NOTINBRANCH);
      else if (strcmp (TREE_STRING_POINTER (id), "inbranch") == 0)
	omp_flags = build_omp_clause (DECL_SOURCE_LOCATION (*node),
				      OMP_CLAUSE_INBRANCH);
      else
	{
	  error ("only %<inbranch%> and %<notinbranch%> flags are "
		 "allowed for %<simd%> attribute");
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
    }

  DECL_ATTRIBUTES (*node) =
    tree_cons (omp_attr, build_tree_list (NULL_TREE, omp_flags),
	       DECL_ATTRIBUTES (*node));

  return NULL_TREE;
}

/* Handle a "used" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_used_attribute (tree *node, tree name, tree, int, bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      || (VAR_P (*node) && TREE_STATIC (*node))
      || (TREE_CODE (*node) == TYPE_DECL))
    {
      TREE_USED (*node) = 1;
      DECL_PRESERVE_P (*node) = 1;
      if (VAR_P (*node))
	DECL_READ_P (*node) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "visibility" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
d_handle_visibility_attribute (tree *node, tree name, tree args,
			       int, bool *)
{
  /*  If this is a type, set the visibility on the type decl.  */
  tree decl = *node;
  if (TYPE_P (decl))
    {
      decl = TYPE_NAME (decl);
      if (decl == NULL_TREE || TREE_CODE (decl) != TYPE_DECL)
	{
	  warning (OPT_Wattributes, "%qE attribute ignored on types", name);
	  return NULL_TREE;
	}
    }

  if (decl_function_context (decl) != 0 || !TREE_PUBLIC (decl))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      return NULL_TREE;
    }

  tree id = TREE_VALUE (args);
  if (TREE_CODE (id) != STRING_CST)
    {
      error ("%qE attribute argument not a string constant", name);
      return NULL_TREE;
    }

  enum symbol_visibility vis;
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

  if (DECL_VISIBILITY_SPECIFIED (decl) && vis != DECL_VISIBILITY (decl))
    {
      tree attributes = (TYPE_P (*node)
			 ? TYPE_ATTRIBUTES (*node)
			 : DECL_ATTRIBUTES (decl));
      if (lookup_attribute ("visibility", attributes))
	error ("%qD redeclared with different visibility", decl);
      else if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	       && lookup_attribute ("dllimport", attributes))
	error ("%qD was declared %qs which implies default visibility",
	       decl, "export");
      else if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	       && lookup_attribute ("dllexport", attributes))
	error ("%qD was declared %qs which implies default visibility",
	       decl, "export");
    }

  DECL_VISIBILITY (decl) = vis;
  DECL_VISIBILITY_SPECIFIED (decl) = 1;

  /* Go ahead and attach the attribute to the node as well.  This is needed
     so we can determine whether we have VISIBILITY_DEFAULT because the
     visibility was not specified, or because it was explicitly overridden
     from the containing scope.  */

  return NULL_TREE;
}
