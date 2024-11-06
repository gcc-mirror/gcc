/* rust-attribs.c -- Rust attributes handling.
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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

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

/* Heavily based on the D frontend Only a subset of the attributes found in the
 * D frontend have been pulled, the goal being to have the builtin function
 * correctly setup. It's possible we may need to add extra attributes in the
 * future.
 */

extern const attribute_spec grs_langhook_common_attribute_table[];

/* Internal attribute handlers for built-in functions.  */
static tree
handle_noreturn_attribute (tree *, tree, tree, int, bool *);
static tree
handle_leaf_attribute (tree *, tree, tree, int, bool *);
static tree
handle_const_attribute (tree *, tree, tree, int, bool *);
static tree
handle_malloc_attribute (tree *, tree, tree, int, bool *);
static tree
handle_pure_attribute (tree *, tree, tree, int, bool *);
static tree
handle_novops_attribute (tree *, tree, tree, int, bool *);
static tree
handle_nonnull_attribute (tree *, tree, tree, int, bool *);
static tree
handle_nothrow_attribute (tree *, tree, tree, int, bool *);
static tree
handle_type_generic_attribute (tree *, tree, tree, int, bool *);
static tree
handle_transaction_pure_attribute (tree *, tree, tree, int, bool *);
static tree
handle_returns_twice_attribute (tree *, tree, tree, int, bool *);
static tree
handle_fnspec_attribute (tree *, tree, tree, int, bool *);
static tree
handle_omp_declare_simd_attribute (tree *, tree, tree, int, bool *);

/* Helper to define attribute exclusions.  */
#define ATTR_EXCL(name, function, type, variable)                              \
  {                                                                            \
    name, function, type, variable                                             \
  }

static const struct attribute_spec::exclusions attr_noreturn_exclusions[] = {
  //  ATTR_EXCL ("alloc_size", true, true, true),
  ATTR_EXCL ("const", true, true, true),
  //  ATTR_EXCL ("malloc", true, true, true),
  ATTR_EXCL ("pure", true, true, true),
  ATTR_EXCL ("returns_twice", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_returns_twice_exclusions[]
  = {
    ATTR_EXCL ("noreturn", true, true, true),
    ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_const_pure_exclusions[] = {
  // ATTR_EXCL ("alloc_size", true, true, true),
  ATTR_EXCL ("const", true, true, true),
  ATTR_EXCL ("noreturn", true, true, true),
  ATTR_EXCL ("pure", true, true, true), ATTR_EXCL (NULL, false, false, false)};

/* Helper to define an attribute.  */
#define ATTR_SPEC(name, min_len, max_len, decl_req, type_req, fn_type_req,     \
		  affects_type_identity, handler, exclude)                     \
  {                                                                            \
    name, min_len, max_len, decl_req, type_req, fn_type_req,                   \
      affects_type_identity, handler, exclude                                  \
  }

/* Table of machine-independent attributes.
   For internal use (marking of built-ins) only.  */
const attribute_spec grs_langhook_common_attribute_table[] = {
  ATTR_SPEC ("noreturn", 0, 0, true, false, false, false,
	     handle_noreturn_attribute, attr_noreturn_exclusions),
  ATTR_SPEC ("leaf", 0, 0, true, false, false, false, handle_leaf_attribute,
	     NULL),
  ATTR_SPEC ("const", 0, 0, true, false, false, false, handle_const_attribute,
	     attr_const_pure_exclusions),
  ATTR_SPEC ("malloc", 0, 0, true, false, false, false, handle_malloc_attribute,
	     NULL),
  ATTR_SPEC ("returns_twice", 0, 0, true, false, false, false,
	     handle_returns_twice_attribute, attr_returns_twice_exclusions),
  ATTR_SPEC ("pure", 0, 0, true, false, false, false, handle_pure_attribute,
	     attr_const_pure_exclusions),
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
  ATTR_SPEC ("fn spec", 1, 1, false, true, true, false, handle_fnspec_attribute,
	     NULL),
  ATTR_SPEC ("omp declare simd", 0, -1, true, false, false, false,
	     handle_omp_declare_simd_attribute, NULL),
  ATTR_SPEC (NULL, 0, 0, false, false, false, false, NULL, NULL),
};

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
    TREE_TYPE (*node) = build_pointer_type (
      build_type_variant (TREE_TYPE (type), TYPE_READONLY (TREE_TYPE (type)),
			  1));
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
    TREE_TYPE (*node) = build_pointer_type (
      build_type_variant (TREE_TYPE (type), 1,
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
      gcc_assert (prototype_p (type) || !TYPE_ATTRIBUTES (type)
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
	  for (ck_num = 1;; ck_num++)
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
  gcc_assert (args && TREE_CODE (TREE_VALUE (args)) == STRING_CST
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
