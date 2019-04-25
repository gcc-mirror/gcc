/* C-family attributes handling.
   Copyright (C) 1992-2019 Free Software Foundation, Inc.

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
#include "memmodel.h"
#include "c-common.h"
#include "gimple-expr.h"
#include "tm_p.h"
#include "stringpool.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "intl.h"
#include "stor-layout.h"
#include "calls.h"
#include "attribs.h"
#include "varasm.h"
#include "trans-mem.h"
#include "c-objc.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "tree-inline.h"
#include "toplev.h"
#include "tree-iterator.h"
#include "opts.h"
#include "gimplify.h"
#include "tree-pretty-print.h"

static tree handle_packed_attribute (tree *, tree, tree, int, bool *);
static tree handle_nocommon_attribute (tree *, tree, tree, int, bool *);
static tree handle_common_attribute (tree *, tree, tree, int, bool *);
static tree handle_noreturn_attribute (tree *, tree, tree, int, bool *);
static tree handle_hot_attribute (tree *, tree, tree, int, bool *);
static tree handle_cold_attribute (tree *, tree, tree, int, bool *);
static tree handle_no_sanitize_attribute (tree *, tree, tree, int, bool *);
static tree handle_no_sanitize_address_attribute (tree *, tree, tree,
						  int, bool *);
static tree handle_no_sanitize_thread_attribute (tree *, tree, tree,
						 int, bool *);
static tree handle_no_address_safety_analysis_attribute (tree *, tree, tree,
							 int, bool *);
static tree handle_no_sanitize_undefined_attribute (tree *, tree, tree, int,
						    bool *);
static tree handle_asan_odr_indicator_attribute (tree *, tree, tree, int,
						 bool *);
static tree handle_stack_protect_attribute (tree *, tree, tree, int, bool *);
static tree handle_noinline_attribute (tree *, tree, tree, int, bool *);
static tree handle_noclone_attribute (tree *, tree, tree, int, bool *);
static tree handle_nocf_check_attribute (tree *, tree, tree, int, bool *);
static tree handle_noicf_attribute (tree *, tree, tree, int, bool *);
static tree handle_noipa_attribute (tree *, tree, tree, int, bool *);
static tree handle_leaf_attribute (tree *, tree, tree, int, bool *);
static tree handle_always_inline_attribute (tree *, tree, tree, int,
					    bool *);
static tree handle_gnu_inline_attribute (tree *, tree, tree, int, bool *);
static tree handle_artificial_attribute (tree *, tree, tree, int, bool *);
static tree handle_flatten_attribute (tree *, tree, tree, int, bool *);
static tree handle_error_attribute (tree *, tree, tree, int, bool *);
static tree handle_used_attribute (tree *, tree, tree, int, bool *);
static tree handle_externally_visible_attribute (tree *, tree, tree, int,
						 bool *);
static tree handle_no_reorder_attribute (tree *, tree, tree, int,
						 bool *);
static tree handle_const_attribute (tree *, tree, tree, int, bool *);
static tree handle_transparent_union_attribute (tree *, tree, tree,
						int, bool *);
static tree handle_scalar_storage_order_attribute (tree *, tree, tree,
						   int, bool *);
static tree handle_constructor_attribute (tree *, tree, tree, int, bool *);
static tree handle_destructor_attribute (tree *, tree, tree, int, bool *);
static tree handle_mode_attribute (tree *, tree, tree, int, bool *);
static tree handle_section_attribute (tree *, tree, tree, int, bool *);
static tree handle_aligned_attribute (tree *, tree, tree, int, bool *);
static tree handle_warn_if_not_aligned_attribute (tree *, tree, tree,
						  int, bool *);
static tree handle_weak_attribute (tree *, tree, tree, int, bool *) ;
static tree handle_noplt_attribute (tree *, tree, tree, int, bool *) ;
static tree handle_alias_ifunc_attribute (bool, tree *, tree, tree, bool *);
static tree handle_ifunc_attribute (tree *, tree, tree, int, bool *);
static tree handle_alias_attribute (tree *, tree, tree, int, bool *);
static tree handle_weakref_attribute (tree *, tree, tree, int, bool *) ;
static tree handle_visibility_attribute (tree *, tree, tree, int,
					 bool *);
static tree handle_tls_model_attribute (tree *, tree, tree, int,
					bool *);
static tree handle_no_instrument_function_attribute (tree *, tree,
						     tree, int, bool *);
static tree handle_no_profile_instrument_function_attribute (tree *, tree,
							     tree, int, bool *);
static tree handle_malloc_attribute (tree *, tree, tree, int, bool *);
static tree handle_returns_twice_attribute (tree *, tree, tree, int, bool *);
static tree handle_no_limit_stack_attribute (tree *, tree, tree, int,
					     bool *);
static tree handle_pure_attribute (tree *, tree, tree, int, bool *);
static tree handle_tm_attribute (tree *, tree, tree, int, bool *);
static tree handle_tm_wrap_attribute (tree *, tree, tree, int, bool *);
static tree handle_novops_attribute (tree *, tree, tree, int, bool *);
static tree handle_deprecated_attribute (tree *, tree, tree, int,
					 bool *);
static tree handle_vector_size_attribute (tree *, tree, tree, int,
					  bool *);
static tree handle_nonnull_attribute (tree *, tree, tree, int, bool *);
static tree handle_nonstring_attribute (tree *, tree, tree, int, bool *);
static tree handle_nothrow_attribute (tree *, tree, tree, int, bool *);
static tree handle_cleanup_attribute (tree *, tree, tree, int, bool *);
static tree handle_warn_unused_result_attribute (tree *, tree, tree, int,
						 bool *);
static tree handle_sentinel_attribute (tree *, tree, tree, int, bool *);
static tree handle_type_generic_attribute (tree *, tree, tree, int, bool *);
static tree handle_alloc_size_attribute (tree *, tree, tree, int, bool *);
static tree handle_alloc_align_attribute (tree *, tree, tree, int, bool *);
static tree handle_assume_aligned_attribute (tree *, tree, tree, int, bool *);
static tree handle_target_attribute (tree *, tree, tree, int, bool *);
static tree handle_target_clones_attribute (tree *, tree, tree, int, bool *);
static tree handle_optimize_attribute (tree *, tree, tree, int, bool *);
static tree ignore_attribute (tree *, tree, tree, int, bool *);
static tree handle_no_split_stack_attribute (tree *, tree, tree, int, bool *);
static tree handle_fnspec_attribute (tree *, tree, tree, int, bool *);
static tree handle_warn_unused_attribute (tree *, tree, tree, int, bool *);
static tree handle_returns_nonnull_attribute (tree *, tree, tree, int, bool *);
static tree handle_omp_declare_simd_attribute (tree *, tree, tree, int,
					       bool *);
static tree handle_simd_attribute (tree *, tree, tree, int, bool *);
static tree handle_omp_declare_target_attribute (tree *, tree, tree, int,
						 bool *);
static tree handle_designated_init_attribute (tree *, tree, tree, int, bool *);
static tree handle_fallthrough_attribute (tree *, tree, tree, int, bool *);
static tree handle_patchable_function_entry_attribute (tree *, tree, tree,
						       int, bool *);
static tree handle_copy_attribute (tree *, tree, tree, int, bool *);

/* Helper to define attribute exclusions.  */
#define ATTR_EXCL(name, function, type, variable)	\
  { name, function, type, variable }

/* Define attributes that are mutually exclusive with one another.  */
static const struct attribute_spec::exclusions attr_aligned_exclusions[] =
{
  /* Attribute name     exclusion applies to:
	                function, type, variable */
  ATTR_EXCL ("aligned", true, false, false),
  ATTR_EXCL ("packed", true, false, false),
  ATTR_EXCL (NULL, false, false, false)
};

extern const struct attribute_spec::exclusions attr_cold_hot_exclusions[] =
{
  ATTR_EXCL ("cold", true, true, true),
  ATTR_EXCL ("hot", true, true, true),
  ATTR_EXCL (NULL, false, false, false)
};

static const struct attribute_spec::exclusions attr_common_exclusions[] =
{
  ATTR_EXCL ("common", true, true, true),
  ATTR_EXCL ("nocommon", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_inline_exclusions[] =
{
  ATTR_EXCL ("noinline", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

static const struct attribute_spec::exclusions attr_noinline_exclusions[] =
{
  ATTR_EXCL ("always_inline", true, true, true),
  ATTR_EXCL ("gnu_inline", true, true, true),
  ATTR_EXCL (NULL, false, false, false),
};

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

static const struct attribute_spec::exclusions
attr_warn_unused_result_exclusions[] =
{
  ATTR_EXCL ("noreturn", true, true, true),
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

/* Table of machine-independent attributes common to all C-like languages.

   Current list of processed common attributes: nonnull.  */
const struct attribute_spec c_common_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "packed",                 0, 0, false, false, false, false,
			      handle_packed_attribute,
	                      attr_aligned_exclusions },
  { "nocommon",               0, 0, true,  false, false, false,
			      handle_nocommon_attribute,
	                      attr_common_exclusions },
  { "common",                 0, 0, true,  false, false, false,
			      handle_common_attribute,
	                      attr_common_exclusions },
  /* FIXME: logically, noreturn attributes should be listed as
     "false, true, true" and apply to function types.  But implementing this
     would require all the places in the compiler that use TREE_THIS_VOLATILE
     on a decl to identify non-returning functions to be located and fixed
     to check the function type instead.  */
  { "noreturn",               0, 0, true,  false, false, false,
			      handle_noreturn_attribute,
	                      attr_noreturn_exclusions },
  { "volatile",               0, 0, true,  false, false, false,
			      handle_noreturn_attribute, NULL },
  { "stack_protect",          0, 0, true,  false, false, false,
			      handle_stack_protect_attribute, NULL },
  { "noinline",               0, 0, true,  false, false, false,
			      handle_noinline_attribute,
	                      attr_noinline_exclusions },
  { "noclone",                0, 0, true,  false, false, false,
			      handle_noclone_attribute, NULL },
  { "no_icf",                 0, 0, true,  false, false, false,
			      handle_noicf_attribute, NULL },
  { "noipa",		      0, 0, true,  false, false, false,
			      handle_noipa_attribute, NULL },
  { "leaf",                   0, 0, true,  false, false, false,
			      handle_leaf_attribute, NULL },
  { "always_inline",          0, 0, true,  false, false, false,
			      handle_always_inline_attribute,
	                      attr_inline_exclusions },
  { "gnu_inline",             0, 0, true,  false, false, false,
			      handle_gnu_inline_attribute,
	                      attr_inline_exclusions },
  { "artificial",             0, 0, true,  false, false, false,
			      handle_artificial_attribute, NULL },
  { "flatten",                0, 0, true,  false, false, false,
			      handle_flatten_attribute, NULL },
  { "used",                   0, 0, true,  false, false, false,
			      handle_used_attribute, NULL },
  { "unused",                 0, 0, false, false, false, false,
			      handle_unused_attribute, NULL },
  { "externally_visible",     0, 0, true,  false, false, false,
			      handle_externally_visible_attribute, NULL },
  { "no_reorder",	      0, 0, true, false, false, false,
	                      handle_no_reorder_attribute, NULL },
  /* The same comments as for noreturn attributes apply to const ones.  */
  { "const",                  0, 0, true,  false, false, false,
			      handle_const_attribute,
	                      attr_const_pure_exclusions },
  { "scalar_storage_order",   1, 1, false, false, false, false,
			      handle_scalar_storage_order_attribute, NULL },
  { "transparent_union",      0, 0, false, false, false, false,
			      handle_transparent_union_attribute, NULL },
  { "constructor",            0, 1, true,  false, false, false,
			      handle_constructor_attribute, NULL },
  { "destructor",             0, 1, true,  false, false, false,
			      handle_destructor_attribute, NULL },
  { "mode",                   1, 1, false,  true, false, false,
			      handle_mode_attribute, NULL },
  { "section",                1, 1, true,  false, false, false,
			      handle_section_attribute, NULL },
  { "aligned",                0, 1, false, false, false, false,
			      handle_aligned_attribute,
	                      attr_aligned_exclusions },
  { "warn_if_not_aligned",    0, 1, false, false, false, false,
			      handle_warn_if_not_aligned_attribute, NULL },
  { "weak",                   0, 0, true,  false, false, false,
			      handle_weak_attribute, NULL },
  { "noplt",                   0, 0, true,  false, false, false,
			      handle_noplt_attribute, NULL },
  { "ifunc",                  1, 1, true,  false, false, false,
			      handle_ifunc_attribute, NULL },
  { "alias",                  1, 1, true,  false, false, false,
			      handle_alias_attribute, NULL },
  { "weakref",                0, 1, true,  false, false, false,
			      handle_weakref_attribute, NULL },
  { "no_instrument_function", 0, 0, true,  false, false, false,
			      handle_no_instrument_function_attribute,
			      NULL },
  { "no_profile_instrument_function",  0, 0, true, false, false, false,
			      handle_no_profile_instrument_function_attribute,
			      NULL },
  { "malloc",                 0, 0, true,  false, false, false,
			      handle_malloc_attribute, attr_alloc_exclusions },
  { "returns_twice",          0, 0, true,  false, false, false,
			      handle_returns_twice_attribute,
	                      attr_returns_twice_exclusions },
  { "no_stack_limit",         0, 0, true,  false, false, false,
			      handle_no_limit_stack_attribute, NULL },
  { "pure",                   0, 0, true,  false, false, false,
			      handle_pure_attribute,
	                      attr_const_pure_exclusions },
  { "transaction_callable",   0, 0, false, true,  false, false,
			      handle_tm_attribute, NULL },
  { "transaction_unsafe",     0, 0, false, true,  false, true,
			      handle_tm_attribute, NULL },
  { "transaction_safe",       0, 0, false, true,  false, true,
			      handle_tm_attribute, NULL },
  { "transaction_safe_dynamic", 0, 0, true, false,  false, false,
			      handle_tm_attribute, NULL },
  { "transaction_may_cancel_outer", 0, 0, false, true, false, false,
			      handle_tm_attribute, NULL },
  /* ??? These two attributes didn't make the transition from the
     Intel language document to the multi-vendor language document.  */
  { "transaction_pure",       0, 0, false, true,  false, false,
			      handle_tm_attribute, NULL },
  { "transaction_wrap",       1, 1, true,  false,  false, false,
			     handle_tm_wrap_attribute, NULL },
  /* For internal use (marking of builtins) only.  The name contains space
     to prevent its usage in source code.  */
  { "no vops",                0, 0, true,  false, false, false,
			      handle_novops_attribute, NULL },
  { "deprecated",             0, 1, false, false, false, false,
			      handle_deprecated_attribute, NULL },
  { "vector_size",	      1, 1, false, true, false, true,
			      handle_vector_size_attribute, NULL },
  { "visibility",	      1, 1, false, false, false, false,
			      handle_visibility_attribute, NULL },
  { "tls_model",	      1, 1, true,  false, false, false,
			      handle_tls_model_attribute, NULL },
  { "nonnull",                0, -1, false, true, true, false,
			      handle_nonnull_attribute, NULL },
  { "nonstring",              0, 0, true, false, false, false,
			      handle_nonstring_attribute, NULL },
  { "nothrow",                0, 0, true,  false, false, false,
			      handle_nothrow_attribute, NULL },
  { "may_alias",	      0, 0, false, true, false, false, NULL, NULL },
  { "cleanup",		      1, 1, true, false, false, false,
			      handle_cleanup_attribute, NULL },
  { "warn_unused_result",     0, 0, false, true, true, false,
			      handle_warn_unused_result_attribute,
	                      attr_warn_unused_result_exclusions },
  { "sentinel",               0, 1, false, true, true, false,
			      handle_sentinel_attribute, NULL },
  /* For internal use (marking of builtins) only.  The name contains space
     to prevent its usage in source code.  */
  { "type generic",           0, 0, false, true, true, false,
			      handle_type_generic_attribute, NULL },
  { "alloc_size",	      1, 2, false, true, true, false,
			      handle_alloc_size_attribute,
	                      attr_alloc_exclusions },
  { "cold",                   0, 0, true,  false, false, false,
			      handle_cold_attribute,
	                      attr_cold_hot_exclusions },
  { "hot",                    0, 0, true,  false, false, false,
			      handle_hot_attribute,
	                      attr_cold_hot_exclusions },
  { "no_address_safety_analysis",
			      0, 0, true, false, false, false,
			      handle_no_address_safety_analysis_attribute,
			      NULL },
  { "no_sanitize",	      1, -1, true, false, false, false,
			      handle_no_sanitize_attribute, NULL },
  { "no_sanitize_address",    0, 0, true, false, false, false,
			      handle_no_sanitize_address_attribute, NULL },
  { "no_sanitize_thread",     0, 0, true, false, false, false,
			      handle_no_sanitize_thread_attribute, NULL },
  { "no_sanitize_undefined",  0, 0, true, false, false, false,
			      handle_no_sanitize_undefined_attribute, NULL },
  { "asan odr indicator",     0, 0, true, false, false, false,
			      handle_asan_odr_indicator_attribute, NULL },
  { "warning",		      1, 1, true,  false, false, false,
			      handle_error_attribute, NULL },
  { "error",		      1, 1, true,  false, false, false,
			      handle_error_attribute, NULL },
  { "target",                 1, -1, true, false, false, false,
			      handle_target_attribute, NULL },
  { "target_clones",          1, -1, true, false, false, false,
			      handle_target_clones_attribute, NULL },
  { "optimize",               1, -1, true, false, false, false,
			      handle_optimize_attribute, NULL },
  /* For internal use only.  The leading '*' both prevents its usage in
     source code and signals that it may be overridden by machine tables.  */
  { "*tm regparm",            0, 0, false, true, true, false,
			      ignore_attribute, NULL },
  { "no_split_stack",	      0, 0, true,  false, false, false,
			      handle_no_split_stack_attribute, NULL },
  /* For internal use (marking of builtins and runtime functions) only.
     The name contains space to prevent its usage in source code.  */
  { "fn spec",		      1, 1, false, true, true, false,
			      handle_fnspec_attribute, NULL },
  { "warn_unused",            0, 0, false, false, false, false,
			      handle_warn_unused_attribute, NULL },
  { "returns_nonnull",        0, 0, false, true, true, false,
			      handle_returns_nonnull_attribute, NULL },
  { "omp declare simd",       0, -1, true,  false, false, false,
			      handle_omp_declare_simd_attribute, NULL },
  { "simd",		      0, 1, true,  false, false, false,
			      handle_simd_attribute, NULL },
  { "omp declare target",     0, 0, true, false, false, false,
			      handle_omp_declare_target_attribute, NULL },
  { "omp declare target link", 0, 0, true, false, false, false,
			      handle_omp_declare_target_attribute, NULL },
  { "omp declare target implicit", 0, 0, true, false, false, false,
			      handle_omp_declare_target_attribute, NULL },
  { "alloc_align",	      1, 1, false, true, true, false,
			      handle_alloc_align_attribute,
	                      attr_alloc_exclusions },
  { "assume_aligned",	      1, 2, false, true, true, false,
			      handle_assume_aligned_attribute, NULL },
  { "designated_init",        0, 0, false, true, false, false,
			      handle_designated_init_attribute, NULL },
  { "fallthrough",	      0, 0, false, false, false, false,
			      handle_fallthrough_attribute, NULL },
  { "patchable_function_entry",	1, 2, true, false, false, false,
			      handle_patchable_function_entry_attribute,
			      NULL },
  { "nocf_check",	      0, 0, false, true, true, true,
			      handle_nocf_check_attribute, NULL },
  { "copy",                   1, 1, false, false, false, false,
			      handle_copy_attribute, NULL },
  { NULL,                     0, 0, false, false, false, false, NULL, NULL }
};

/* Give the specifications for the format attributes, used by C and all
   descendants.

   Current list of processed format attributes: format, format_arg.  */
const struct attribute_spec c_common_format_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "format",                 3, 3, false, true,  true, false,
			      handle_format_attribute, NULL },
  { "format_arg",             1, 1, false, true,  true, false,
			      handle_format_arg_attribute, NULL },
  { NULL,                     0, 0, false, false, false, false, NULL, NULL }
};

/* Returns TRUE iff the attribute indicated by ATTR_ID takes a plain
   identifier as an argument, so the front end shouldn't look it up.  */

bool
attribute_takes_identifier_p (const_tree attr_id)
{
  const struct attribute_spec *spec = lookup_attribute_spec (attr_id);
  if (spec == NULL)
    /* Unknown attribute that we'll end up ignoring, return true so we
       don't complain about an identifier argument.  */
    return true;
  else if (!strcmp ("mode", spec->name)
	   || !strcmp ("format", spec->name)
	   || !strcmp ("cleanup", spec->name))
    return true;
  else
    return targetm.attribute_takes_identifier_p (attr_id);
}

/* Verify that argument value POS at position ARGNO to attribute NAME
   applied to function TYPE refers to a function parameter at position
   POS and the expected type CODE.  Treat CODE == INTEGER_TYPE as
   matching all C integral types except bool.  If successful, return
   POS after default conversions, if any.  Otherwise, issue appropriate
   warnings and return null.  A non-zero 1-based ARGNO should be passed
   in by callers only for attributes with more than one argument.  */

tree
positional_argument (const_tree fntype, const_tree atname, tree pos,
		     tree_code code, int argno /* = 0 */,
		     int flags /* = posargflags () */)
{
  if (pos && TREE_CODE (pos) != IDENTIFIER_NODE
      && TREE_CODE (pos) != FUNCTION_DECL)
    pos = default_conversion (pos);

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

  /* Argument positions are 1-based.  */
  if (integer_zerop (pos))
    {
      if (flags & POSARG_ZERO)
	/* Zero is explicitly allowed.  */
	return pos;

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

  if (!prototype_p (fntype))
    return pos;

  /* Verify that the argument position does not exceed the number
     of formal arguments to the function.  When POSARG_ELLIPSIS
     is set, ARGNO may be beyond the last argument of a vararg
     function.  */
  unsigned nargs = type_num_arguments (fntype);
  if (!nargs
      || !tree_fits_uhwi_p (pos)
      || ((flags & POSARG_ELLIPSIS) == 0
	  && !IN_RANGE (tree_to_uhwi (pos), 1, nargs)))
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
  unsigned HOST_WIDE_INT ipos = tree_to_uhwi (pos);

  /* Zero was handled above.  */
  gcc_assert (ipos != 0);

  if (tree argtype = type_argument_type (fntype, ipos))
    {
      if (flags & POSARG_ELLIPSIS)
	{
	  if (argno < 1)
	    error ("%qE attribute argument value %qE does not refer to "
		   "a variable argument list",
		   atname, pos);
	  else
	    error ("%qE attribute argument %i value %qE does not refer to "
		   "a variable argument list",
		   atname, argno, pos);
	  return NULL_TREE;
	}

      /* Where the expected code is STRING_CST accept any pointer
	 expected by attribute format (this includes possibly qualified
	 char pointers and, for targets like Darwin, also pointers to
	 struct CFString).  */
      bool type_match;
      if (code == STRING_CST)
	type_match = valid_format_string_type_p (argtype);
      else if (code == INTEGER_TYPE)
	/* For integers, accept enums, wide characters and other types
	   that match INTEGRAL_TYPE_P except for bool.  */
	type_match = (INTEGRAL_TYPE_P (argtype)
		      && TREE_CODE (argtype) != BOOLEAN_TYPE);
      else
	type_match = TREE_CODE (argtype) == code;

      if (!type_match)
	{
	  if (code == STRING_CST)
	    {
	      /* Reject invalid format strings with an error.  */
	      if (argno < 1)
		error ("%qE attribute argument value %qE refers to "
		       "parameter type %qT",
		       atname, pos, argtype);
	      else
		error ("%qE attribute argument %i value %qE refers to "
		       "parameter type %qT",
		       atname, argno, pos, argtype);

	      return NULL_TREE;
	    }

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
    }
  else if (!(flags & POSARG_ELLIPSIS))
    {
      if (argno < 1)
	warning (OPT_Wattributes,
		 "%qE attribute argument value %qE refers to "
		 "a variadic function parameter of unknown type",
		 atname, pos);
      else
	warning (OPT_Wattributes,
		 "%qE attribute argument %i value %qE refers to "
		 "a variadic function parameter of unknown type",
		 atname, argno, pos);
      return NULL_TREE;
    }

  return pos;
}


/* Attribute handlers common to C front ends.  */

/* Handle a "packed" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_packed_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			 int flags, bool *no_add_attrs)
{
  if (TYPE_P (*node))
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	{
	  warning (OPT_Wattributes,
		   "%qE attribute ignored for type %qT", name, *node);
	  *no_add_attrs = true;
	}
      else
	TYPE_PACKED (*node) = 1;
    }
  else if (TREE_CODE (*node) == FIELD_DECL)
    {
      if (TYPE_ALIGN (TREE_TYPE (*node)) <= BITS_PER_UNIT
	  /* Still pack bitfields.  */
	  && ! DECL_C_BIT_FIELD (*node))
	warning (OPT_Wattributes,
		 "%qE attribute ignored for field of type %qT",
		 name, TREE_TYPE (*node));
      else
	DECL_PACKED (*node) = 1;
    }
  /* We can't set DECL_PACKED for a VAR_DECL, because the bit is
     used for DECL_REGISTER.  It wouldn't mean anything anyway.
     We can't set DECL_PACKED on the type of a TYPE_DECL, because
     that changes what the typedef is typing.  */
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "nocommon" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_nocommon_attribute (tree *node, tree name,
			   tree ARG_UNUSED (args),
			   int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (VAR_P (*node))
    DECL_COMMON (*node) = 0;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "common" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_common_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			 int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (VAR_P (*node))
    DECL_COMMON (*node) = 1;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "noreturn" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noreturn_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			   int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree type = TREE_TYPE (*node);

  /* See FIXME comment in c_common_attribute_table.  */
  if (TREE_CODE (*node) == FUNCTION_DECL
      || objc_method_decl (TREE_CODE (*node)))
    TREE_THIS_VOLATILE (*node) = 1;
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    TREE_TYPE (*node)
      = (build_qualified_type
	 (build_pointer_type
	  (build_type_variant (TREE_TYPE (type),
			       TYPE_READONLY (TREE_TYPE (type)), 1)),
	  TYPE_QUALS (type)));
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "hot" and attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_hot_attribute (tree *node, tree name, tree ARG_UNUSED (args),
		      int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      || TREE_CODE (*node) == LABEL_DECL)
    {
      /* Attribute hot processing is done later with lookup_attribute.  */
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

/* Add FLAGS for a function NODE to no_sanitize_flags in DECL_ATTRIBUTES.  */

void
add_no_sanitize_value (tree node, unsigned int flags)
{
  tree attr = lookup_attribute ("no_sanitize", DECL_ATTRIBUTES (node));
  if (attr)
    {
      unsigned int old_value = tree_to_uhwi (TREE_VALUE (attr));
      flags |= old_value;

      if (flags == old_value)
	return;

      TREE_VALUE (attr) = build_int_cst (unsigned_type_node, flags);
    }
  else
    DECL_ATTRIBUTES (node)
      = tree_cons (get_identifier ("no_sanitize"),
		   build_int_cst (unsigned_type_node, flags),
		   DECL_ATTRIBUTES (node));
}

/* Handle a "no_sanitize" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_sanitize_attribute (tree *node, tree name, tree args, int,
			      bool *no_add_attrs)
{
  unsigned int flags = 0;
  *no_add_attrs = true;
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      return NULL_TREE;
    }

  for (; args; args = TREE_CHAIN (args))
    {
      tree id = TREE_VALUE (args);
      if (TREE_CODE (id) != STRING_CST)
	{
	  error ("no_sanitize argument not a string");
	  return NULL_TREE;
	}

      char *string = ASTRDUP (TREE_STRING_POINTER (id));
      flags |= parse_no_sanitize_attribute (string);
    }

  add_no_sanitize_value (*node, flags);

  return NULL_TREE;
}

/* Handle a "no_sanitize_address" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_sanitize_address_attribute (tree *node, tree name, tree, int,
				      bool *no_add_attrs)
{
  *no_add_attrs = true;
  if (TREE_CODE (*node) != FUNCTION_DECL)
    warning (OPT_Wattributes, "%qE attribute ignored", name);
  else
    add_no_sanitize_value (*node, SANITIZE_ADDRESS);

  return NULL_TREE;
}

/* Handle a "no_sanitize_thread" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_sanitize_thread_attribute (tree *node, tree name, tree, int,
				      bool *no_add_attrs)
{
  *no_add_attrs = true;
  if (TREE_CODE (*node) != FUNCTION_DECL)
    warning (OPT_Wattributes, "%qE attribute ignored", name);
  else
    add_no_sanitize_value (*node, SANITIZE_THREAD);

  return NULL_TREE;
}


/* Handle a "no_address_safety_analysis" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_address_safety_analysis_attribute (tree *node, tree name, tree, int,
					     bool *no_add_attrs)
{
  *no_add_attrs = true;
  if (TREE_CODE (*node) != FUNCTION_DECL)
    warning (OPT_Wattributes, "%qE attribute ignored", name);
  else
    add_no_sanitize_value (*node, SANITIZE_ADDRESS);

  return NULL_TREE;
}

/* Handle a "no_sanitize_undefined" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_sanitize_undefined_attribute (tree *node, tree name, tree, int,
				      bool *no_add_attrs)
{
  *no_add_attrs = true;
  if (TREE_CODE (*node) != FUNCTION_DECL)
    warning (OPT_Wattributes, "%qE attribute ignored", name);
  else
    add_no_sanitize_value (*node,
			   SANITIZE_UNDEFINED | SANITIZE_UNDEFINED_NONDEFAULT);

  return NULL_TREE;
}

/* Handle an "asan odr indicator" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_asan_odr_indicator_attribute (tree *, tree, tree, int, bool *)
{
  return NULL_TREE;
}

/* Handle a "stack_protect" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_stack_protect_attribute (tree *node, tree name, tree, int,
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
handle_noipa_attribute (tree *node, tree name, tree, int, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
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
    {
      if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (*node)))
	{
	  warning (OPT_Wattributes, "%qE attribute ignored due to conflict "
		   "with attribute %qs", name, "always_inline");
	  *no_add_attrs = true;
	}
      else
	DECL_UNINLINABLE (*node) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "noclone" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noclone_attribute (tree *node, tree name,
			  tree ARG_UNUSED (args),
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "nocf_check" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_nocf_check_attribute (tree *node, tree name,
			  tree ARG_UNUSED (args),
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != METHOD_TYPE)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else if (!(flag_cf_protection & CF_BRANCH))
    {
      warning (OPT_Wattributes, "%qE attribute ignored. Use "
				"%<-fcf-protection%> option to enable it",
				name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "no_icf" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noicf_attribute (tree *node, tree name,
			tree ARG_UNUSED (args),
			int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

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
      if (lookup_attribute ("noinline", DECL_ATTRIBUTES (*node)))
	{
	  warning (OPT_Wattributes, "%qE attribute ignored due to conflict "
		   "with %qs attribute", name, "noinline");
	  *no_add_attrs = true;
	}
      else if (lookup_attribute ("target_clones", DECL_ATTRIBUTES (*node)))
	{
	  warning (OPT_Wattributes, "%qE attribute ignored due to conflict "
		   "with %qs attribute", name, "target_clones");
	  *no_add_attrs = true;
	}
      else
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

/* Handle a "gnu_inline" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_gnu_inline_attribute (tree *node, tree name,
			     tree ARG_UNUSED (args),
			     int ARG_UNUSED (flags),
			     bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (*node))
    {
      /* Do nothing else, just set the attribute.  We'll get at
	 it later with lookup_attribute.  */
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

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
      warning (OPT_Wattributes, "%qE attribute has no effect on unit local "
	       "functions", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "artificial" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_artificial_attribute (tree *node, tree name,
			     tree ARG_UNUSED (args),
			     int ARG_UNUSED (flags),
			     bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL && DECL_DECLARED_INLINE_P (*node))
    {
      /* Do nothing else, just set the attribute.  We'll get at
	 it later with lookup_attribute.  */
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
handle_flatten_attribute (tree *node, tree name,
			  tree args ATTRIBUTE_UNUSED,
			  int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    /* Do nothing else, just set the attribute.  We'll get at
       it later with lookup_attribute.  */
    ;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "warning" or "error" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_error_attribute (tree *node, tree name, tree args,
			int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
    /* Do nothing else, just set the attribute.  We'll get at
       it later with lookup_attribute.  */
    ;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
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

/* Handle a "unused" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_unused_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			 int flags, bool *no_add_attrs)
{
  if (DECL_P (*node))
    {
      tree decl = *node;

      if (TREE_CODE (decl) == PARM_DECL
	  || VAR_OR_FUNCTION_DECL_P (decl)
	  || TREE_CODE (decl) == LABEL_DECL
	  || TREE_CODE (decl) == CONST_DECL
	  || TREE_CODE (decl) == TYPE_DECL)
	{
	  TREE_USED (decl) = 1;
	  if (VAR_P (decl) || TREE_CODE (decl) == PARM_DECL)
	    DECL_READ_P (decl) = 1;
	}
      else
	{
	  warning (OPT_Wattributes, "%qE attribute ignored", name);
	  *no_add_attrs = true;
	}
    }
  else
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*node = build_variant_type_copy (*node);
      TREE_USED (*node) = 1;
    }

  return NULL_TREE;
}

/* Handle a "externally_visible" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_externally_visible_attribute (tree *pnode, tree name,
				     tree ARG_UNUSED (args),
				     int ARG_UNUSED (flags),
				     bool *no_add_attrs)
{
  tree node = *pnode;

  if (VAR_OR_FUNCTION_DECL_P (node))
    {
      if ((!TREE_STATIC (node) && TREE_CODE (node) != FUNCTION_DECL
	   && !DECL_EXTERNAL (node)) || !TREE_PUBLIC (node))
	{
	  warning (OPT_Wattributes,
		   "%qE attribute have effect only on public objects", name);
	  *no_add_attrs = true;
	}
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle the "no_reorder" attribute.  Arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_reorder_attribute (tree *pnode,
			     tree name,
			     tree,
			     int,
			     bool *no_add_attrs)
{
  tree node = *pnode;

  if (!VAR_OR_FUNCTION_DECL_P (node)
	&& !(TREE_STATIC (node) || DECL_EXTERNAL (node)))
    {
      warning (OPT_Wattributes,
		"%qE attribute only affects top level objects",
		name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "const" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
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

  /* void __builtin_unreachable(void) is const.  Accept other such
     built-ins but warn on user-defined functions that return void.  */
  if (!(flags & ATTR_FLAG_BUILT_IN)
      && TREE_CODE (*node) == FUNCTION_DECL
      && VOID_TYPE_P (TREE_TYPE (type)))
    warning (OPT_Wattributes, "%qE attribute on function "
	     "returning %<void%>", name);

  return NULL_TREE;
}

/* Handle a "scalar_storage_order" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_scalar_storage_order_attribute (tree *node, tree name, tree args,
				       int flags, bool *no_add_attrs)
{
  tree id = TREE_VALUE (args);
  tree type;

  if (TREE_CODE (*node) == TYPE_DECL
      && ! (flags & ATTR_FLAG_CXX11))
    node = &TREE_TYPE (*node);
  type = *node;

  if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
    {
      error ("scalar_storage_order is not supported because endianness "
	    "is not uniform");
      return NULL_TREE;
    }

  if (RECORD_OR_UNION_TYPE_P (type) && !c_dialect_cxx ())
    {
      bool reverse = false;

      if (TREE_CODE (id) == STRING_CST
	  && strcmp (TREE_STRING_POINTER (id), "big-endian") == 0)
	reverse = !BYTES_BIG_ENDIAN;
      else if (TREE_CODE (id) == STRING_CST
	       && strcmp (TREE_STRING_POINTER (id), "little-endian") == 0)
	reverse = BYTES_BIG_ENDIAN;
      else
	{
	  error ("scalar_storage_order argument must be one of \"big-endian\""
		 " or \"little-endian\"");
	  return NULL_TREE;
	}

      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	{
	  if (reverse)
	    /* A type variant isn't good enough, since we don't want a cast
	       to such a type to be removed as a no-op.  */
	    *node = type = build_duplicate_type (type);
	}

      TYPE_REVERSE_STORAGE_ORDER (type) = reverse;
      return NULL_TREE;
    }

  warning (OPT_Wattributes, "%qE attribute ignored", name);
  *no_add_attrs = true;
  return NULL_TREE;
}

/* Handle a "transparent_union" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_transparent_union_attribute (tree *node, tree name,
				    tree ARG_UNUSED (args), int flags,
				    bool *no_add_attrs)
{
  tree type;

  *no_add_attrs = true;

  if (TREE_CODE (*node) == TYPE_DECL
      && ! (flags & ATTR_FLAG_CXX11))
    node = &TREE_TYPE (*node);
  type = *node;

  if (TREE_CODE (type) == UNION_TYPE)
    {
      /* Make sure that the first field will work for a transparent union.
	 If the type isn't complete yet, leave the check to the code in
	 finish_struct.  */
      if (TYPE_SIZE (type))
	{
	  tree first = first_field (type);
	  if (first == NULL_TREE
	      || DECL_ARTIFICIAL (first)
	      || TYPE_MODE (type) != DECL_MODE (first))
	    goto ignored;
	}

      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	{
	  /* If the type isn't complete yet, setting the flag
	     on a variant wouldn't ever be checked.  */
	  if (!TYPE_SIZE (type))
	    goto ignored;

	  /* build_duplicate_type doesn't work for C++.  */
	  if (c_dialect_cxx ())
	    goto ignored;

	  /* A type variant isn't good enough, since we don't want a cast
	     to such a type to be removed as a no-op.  */
	  *node = type = build_duplicate_type (type);
	}

      for (tree t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	TYPE_TRANSPARENT_AGGR (t) = 1;
      return NULL_TREE;
    }

 ignored:
  warning (OPT_Wattributes, "%qE attribute ignored", name);
  return NULL_TREE;
}

/* Subroutine of handle_{con,de}structor_attribute.  Evaluate ARGS to
   get the requested priority for a constructor or destructor,
   possibly issuing diagnostics for invalid or reserved
   priorities.  */

static priority_type
get_priority (tree args, bool is_destructor)
{
  HOST_WIDE_INT pri;
  tree arg;

  if (!args)
    return DEFAULT_INIT_PRIORITY;

  if (!SUPPORTS_INIT_PRIORITY)
    {
      if (is_destructor)
	error ("destructor priorities are not supported");
      else
	error ("constructor priorities are not supported");
      return DEFAULT_INIT_PRIORITY;
    }

  arg = TREE_VALUE (args);
  if (TREE_CODE (arg) == IDENTIFIER_NODE)
    goto invalid;
  if (arg == error_mark_node)
    return DEFAULT_INIT_PRIORITY;
  arg = default_conversion (arg);
  if (!tree_fits_shwi_p (arg)
      || !INTEGRAL_TYPE_P (TREE_TYPE (arg)))
    goto invalid;

  pri = tree_to_shwi (arg);
  if (pri < 0 || pri > MAX_INIT_PRIORITY)
    goto invalid;

  if (pri <= MAX_RESERVED_INIT_PRIORITY)
    {
      if (is_destructor)
	warning (OPT_Wprio_ctor_dtor,
		 "destructor priorities from 0 to %d are reserved "
		 "for the implementation",
		 MAX_RESERVED_INIT_PRIORITY);
      else
	warning (OPT_Wprio_ctor_dtor,
		 "constructor priorities from 0 to %d are reserved "
		 "for the implementation",
		 MAX_RESERVED_INIT_PRIORITY);
    }
  return pri;

 invalid:
  if (is_destructor)
    error ("destructor priorities must be integers from 0 to %d inclusive",
	   MAX_INIT_PRIORITY);
  else
    error ("constructor priorities must be integers from 0 to %d inclusive",
	   MAX_INIT_PRIORITY);
  return DEFAULT_INIT_PRIORITY;
}

/* Handle a "constructor" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_constructor_attribute (tree *node, tree name, tree args,
			      int ARG_UNUSED (flags),
			      bool *no_add_attrs)
{
  tree decl = *node;
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && TREE_CODE (type) == FUNCTION_TYPE
      && decl_function_context (decl) == 0)
    {
      priority_type priority;
      DECL_STATIC_CONSTRUCTOR (decl) = 1;
      priority = get_priority (args, /*is_destructor=*/false);
      SET_DECL_INIT_PRIORITY (decl, priority);
      TREE_USED (decl) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "destructor" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_destructor_attribute (tree *node, tree name, tree args,
			     int ARG_UNUSED (flags),
			     bool *no_add_attrs)
{
  tree decl = *node;
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (decl) == FUNCTION_DECL
      && TREE_CODE (type) == FUNCTION_TYPE
      && decl_function_context (decl) == 0)
    {
      priority_type priority;
      DECL_STATIC_DESTRUCTOR (decl) = 1;
      priority = get_priority (args, /*is_destructor=*/true);
      SET_DECL_FINI_PRIORITY (decl, priority);
      TREE_USED (decl) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Nonzero if the mode is a valid vector mode for this architecture.
   This returns nonzero even if there is no hardware support for the
   vector mode, but we can emulate with narrower modes.  */

static bool
vector_mode_valid_p (machine_mode mode)
{
  enum mode_class mclass = GET_MODE_CLASS (mode);

  /* Doh!  What's going on?  */
  if (mclass != MODE_VECTOR_INT
      && mclass != MODE_VECTOR_FLOAT
      && mclass != MODE_VECTOR_FRACT
      && mclass != MODE_VECTOR_UFRACT
      && mclass != MODE_VECTOR_ACCUM
      && mclass != MODE_VECTOR_UACCUM)
    return false;

  /* Hardware support.  Woo hoo!  */
  if (targetm.vector_mode_supported_p (mode))
    return true;

  /* We should probably return 1 if requesting V4DI and we have no DI,
     but we have V2DI, but this is probably very unlikely.  */

  /* If we have support for the inner mode, we can safely emulate it.
     We may not have V2DI, but me can emulate with a pair of DIs.  */
  return targetm.scalar_mode_supported_p (GET_MODE_INNER (mode));
}


/* Handle a "mode" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_mode_attribute (tree *node, tree name, tree args,
		       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree type = *node;
  tree ident = TREE_VALUE (args);

  *no_add_attrs = true;

  if (TREE_CODE (ident) != IDENTIFIER_NODE)
    warning (OPT_Wattributes, "%qE attribute ignored", name);
  else
    {
      int j;
      const char *p = IDENTIFIER_POINTER (ident);
      int len = strlen (p);
      machine_mode mode = VOIDmode;
      tree typefm;
      bool valid_mode;

      if (len > 4 && p[0] == '_' && p[1] == '_'
	  && p[len - 1] == '_' && p[len - 2] == '_')
	{
	  char *newp = (char *) alloca (len - 1);

	  strcpy (newp, &p[2]);
	  newp[len - 4] = '\0';
	  p = newp;
	}

      /* Change this type to have a type with the specified mode.
	 First check for the special modes.  */
      if (!strcmp (p, "byte"))
	mode = byte_mode;
      else if (!strcmp (p, "word"))
	mode = word_mode;
      else if (!strcmp (p, "pointer"))
	mode = ptr_mode;
      else if (!strcmp (p, "libgcc_cmp_return"))
	mode = targetm.libgcc_cmp_return_mode ();
      else if (!strcmp (p, "libgcc_shift_count"))
	mode = targetm.libgcc_shift_count_mode ();
      else if (!strcmp (p, "unwind_word"))
	mode = targetm.unwind_word_mode ();
      else
	for (j = 0; j < NUM_MACHINE_MODES; j++)
	  if (!strcmp (p, GET_MODE_NAME (j)))
	    {
	      mode = (machine_mode) j;
	      break;
	    }

      if (mode == VOIDmode)
	{
	  error ("unknown machine mode %qE", ident);
	  return NULL_TREE;
	}

      /* Allow the target a chance to translate MODE into something supported.
	 See PR86324.  */
      mode = targetm.translate_mode_attribute (mode);

      valid_mode = false;
      switch (GET_MODE_CLASS (mode))
	{
	case MODE_INT:
	case MODE_PARTIAL_INT:
	case MODE_FLOAT:
	case MODE_DECIMAL_FLOAT:
	case MODE_FRACT:
	case MODE_UFRACT:
	case MODE_ACCUM:
	case MODE_UACCUM:
	  valid_mode
	    = targetm.scalar_mode_supported_p (as_a <scalar_mode> (mode));
	  break;

	case MODE_COMPLEX_INT:
	case MODE_COMPLEX_FLOAT:
	  valid_mode = targetm.scalar_mode_supported_p (GET_MODE_INNER (mode));
	  break;

	case MODE_VECTOR_INT:
	case MODE_VECTOR_FLOAT:
	case MODE_VECTOR_FRACT:
	case MODE_VECTOR_UFRACT:
	case MODE_VECTOR_ACCUM:
	case MODE_VECTOR_UACCUM:
	  warning (OPT_Wattributes, "specifying vector types with "
		   "__attribute__ ((mode)) is deprecated");
	  warning (OPT_Wattributes,
		   "use __attribute__ ((vector_size)) instead");
	  valid_mode = vector_mode_valid_p (mode);
	  break;

	default:
	  break;
	}
      if (!valid_mode)
	{
	  error ("unable to emulate %qs", p);
	  return NULL_TREE;
	}

      if (POINTER_TYPE_P (type))
	{
	  scalar_int_mode addr_mode;
	  addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (type));
	  tree (*fn)(tree, machine_mode, bool);

	  if (!is_a <scalar_int_mode> (mode, &addr_mode)
	      || !targetm.addr_space.valid_pointer_mode (addr_mode, as))
	    {
	      error ("invalid pointer mode %qs", p);
	      return NULL_TREE;
	    }

	  if (TREE_CODE (type) == POINTER_TYPE)
	    fn = build_pointer_type_for_mode;
	  else
	    fn = build_reference_type_for_mode;
	  typefm = fn (TREE_TYPE (type), addr_mode, false);
	}
      else
	{
	  /* For fixed-point modes, we need to test if the signness of type
	     and the machine mode are consistent.  */
	  if (ALL_FIXED_POINT_MODE_P (mode)
	      && TYPE_UNSIGNED (type) != UNSIGNED_FIXED_POINT_MODE_P (mode))
	    {
	      error ("signedness of type and machine mode %qs don%'t match", p);
	      return NULL_TREE;
	    }
	  /* For fixed-point modes, we need to pass saturating info.  */
	  typefm = lang_hooks.types.type_for_mode (mode,
			ALL_FIXED_POINT_MODE_P (mode) ? TYPE_SATURATING (type)
						      : TYPE_UNSIGNED (type));
	}

      if (typefm == NULL_TREE)
	{
	  error ("no data type for mode %qs", p);
	  return NULL_TREE;
	}
      else if (TREE_CODE (type) == ENUMERAL_TYPE)
	{
	  /* For enumeral types, copy the precision from the integer
	     type returned above.  If not an INTEGER_TYPE, we can't use
	     this mode for this type.  */
	  if (TREE_CODE (typefm) != INTEGER_TYPE)
	    {
	      error ("cannot use mode %qs for enumeral types", p);
	      return NULL_TREE;
	    }

	  if (flags & ATTR_FLAG_TYPE_IN_PLACE)
	    {
	      TYPE_PRECISION (type) = TYPE_PRECISION (typefm);
	      typefm = type;
	    }
	  else
	    {
	      /* We cannot build a type variant, as there's code that assumes
		 that TYPE_MAIN_VARIANT has the same mode.  This includes the
		 debug generators.  Instead, create a subrange type.  This
		 results in all of the enumeral values being emitted only once
		 in the original, and the subtype gets them by reference.  */
	      if (TYPE_UNSIGNED (type))
		typefm = make_unsigned_type (TYPE_PRECISION (typefm));
	      else
		typefm = make_signed_type (TYPE_PRECISION (typefm));
	      TREE_TYPE (typefm) = type;
	    }
	}
      else if (VECTOR_MODE_P (mode)
	       ? TREE_CODE (type) != TREE_CODE (TREE_TYPE (typefm))
	       : TREE_CODE (type) != TREE_CODE (typefm))
	{
	  error ("mode %qs applied to inappropriate type", p);
	  return NULL_TREE;
	}

      *node = build_qualified_type (typefm, TYPE_QUALS (type));
    }

  return NULL_TREE;
}

/* Handle a "section" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_section_attribute (tree *node, tree ARG_UNUSED (name), tree args,
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;

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

  if (TREE_CODE (TREE_VALUE (args)) != STRING_CST)
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

  /* The decl may have already been given a section attribute
     from a previous declaration.  Ensure they match.  */
  if (DECL_SECTION_NAME (decl) != NULL
      && strcmp (DECL_SECTION_NAME (decl),
		 TREE_STRING_POINTER (TREE_VALUE (args))) != 0)
    {
      error ("section of %q+D conflicts with previous declaration", *node);
      goto fail;
    }

  if (VAR_P (decl)
      && !targetm.have_tls && targetm.emutls.tmpl_section
      && DECL_THREAD_LOCAL_P (decl))
    {
      error ("section of %q+D cannot be overridden", *node);
      goto fail;
    }

  set_decl_section_name (decl, TREE_STRING_POINTER (TREE_VALUE (args)));
  return NULL_TREE;

fail:
  *no_add_attrs = true;
  return NULL_TREE;
}

/* If in c++-11, check if the c++-11 alignment constraint with respect
   to fundamental alignment (in [dcl.align]) are satisfied.  If not in
   c++-11 mode, does nothing.

   [dcl.align]2/ says:

   [* if the constant expression evaluates to a fundamental alignment,
   the alignment requirement of the declared entity shall be the
   specified fundamental alignment.

   * if the constant expression evaluates to an extended alignment
   and the implementation supports that alignment in the context
   of the declaration, the alignment of the declared entity shall
   be that alignment

   * if the constant expression evaluates to an extended alignment
   and the implementation does not support that alignment in the
   context of the declaration, the program is ill-formed].  */

static bool
check_cxx_fundamental_alignment_constraints (tree node,
					     unsigned align_log,
					     int flags)
{
  bool alignment_too_large_p = false;
  unsigned requested_alignment = (1U << align_log) * BITS_PER_UNIT;
  unsigned max_align = 0;

  if ((!(flags & ATTR_FLAG_CXX11) && !warn_cxx_compat)
      || (node == NULL_TREE || node == error_mark_node))
    return true;

  if (cxx_fundamental_alignment_p (requested_alignment))
    return true;

  if (VAR_P (node))
    {
      if (TREE_STATIC (node) || DECL_EXTERNAL (node))
	/* For file scope variables and static members, the target supports
	   alignments that are at most MAX_OFILE_ALIGNMENT.  */
	max_align = MAX_OFILE_ALIGNMENT;
      else
	/* For stack variables, the target supports at most
	   MAX_STACK_ALIGNMENT.  */
	max_align = MAX_STACK_ALIGNMENT;
      if (requested_alignment > max_align)
	alignment_too_large_p = true;
    }
  /* Let's be liberal for types and fields; don't limit their alignment any
     more than check_user_alignment already did.  */

  if (alignment_too_large_p)
    pedwarn (input_location, OPT_Wattributes,
	     "requested alignment %d is larger than %d",
	     requested_alignment / BITS_PER_UNIT, max_align / BITS_PER_UNIT);

  return !alignment_too_large_p;
}

/* Common codes shared by handle_warn_if_not_aligned_attribute and
   handle_aligned_attribute.  */

static tree
common_handle_aligned_attribute (tree *node, tree name, tree args, int flags,
				 bool *no_add_attrs,
				 bool warn_if_not_aligned_p)
{
  tree decl = NULL_TREE;
  tree *type = NULL;
  bool is_type = false;
  tree align_expr;

  /* The last (already pushed) declaration with all validated attributes
     merged in or the current about-to-be-pushed one if one hasn't been
     yet.  */
  tree last_decl = node[1] ? node[1] : *node;

  if (args)
    {
      align_expr = TREE_VALUE (args);
      if (align_expr && TREE_CODE (align_expr) != IDENTIFIER_NODE
	  && TREE_CODE (align_expr) != FUNCTION_DECL)
	align_expr = default_conversion (align_expr);
    }
  else
    align_expr = size_int (ATTRIBUTE_ALIGNED_VALUE / BITS_PER_UNIT);

  if (DECL_P (*node))
    {
      decl = *node;
      type = &TREE_TYPE (decl);
      is_type = TREE_CODE (*node) == TYPE_DECL;
    }
  else if (TYPE_P (*node))
    type = node, is_type = true;

  /* True to consider invalid alignments greater than MAX_OFILE_ALIGNMENT.  */
  bool objfile = (TREE_CODE (*node) == FUNCTION_DECL
		  || (VAR_P (*node) && TREE_STATIC (*node)));
  /* Log2 of specified alignment.  */
  int pow2align = check_user_alignment (align_expr, objfile,
					/* warn_zero = */ true);
  if (pow2align == -1
      || !check_cxx_fundamental_alignment_constraints (*node, pow2align, flags))
    {
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* The alignment in bits corresponding to the specified alignment.  */
  unsigned bitalign = (1U << pow2align) * BITS_PER_UNIT;

  /* The alignment of the current declaration and that of the last
     pushed declaration, determined on demand below.  */
  unsigned curalign = 0;
  unsigned lastalign = 0;

  /* True when SET_DECL_ALIGN() should be called for the decl when
     *NO_ADD_ATTRS is false.  */
  bool set_align = true;
  if (is_type)
    {
      if ((flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	/* OK, modify the type in place.  */;
      /* If we have a TYPE_DECL, then copy the type, so that we
	 don't accidentally modify a builtin type.  See pushdecl.  */
      else if (decl && TREE_TYPE (decl) != error_mark_node
	       && DECL_ORIGINAL_TYPE (decl) == NULL_TREE)
	{
	  tree tt = TREE_TYPE (decl);
	  *type = build_variant_type_copy (*type);
	  DECL_ORIGINAL_TYPE (decl) = tt;
	  TYPE_NAME (*type) = decl;
	  TREE_USED (*type) = TREE_USED (decl);
	  TREE_TYPE (decl) = *type;
	}
      else
	*type = build_variant_type_copy (*type);

      if (warn_if_not_aligned_p)
	{
	  SET_TYPE_WARN_IF_NOT_ALIGN (*type, bitalign);
	  warn_if_not_aligned_p = false;
	}
      else
	{
	  SET_TYPE_ALIGN (*type, bitalign);
	  TYPE_USER_ALIGN (*type) = 1;
	}
    }
  else if (! VAR_OR_FUNCTION_DECL_P (decl)
	   && TREE_CODE (decl) != FIELD_DECL)
    {
      error ("alignment may not be specified for %q+D", decl);
      *no_add_attrs = true;
    }
  else if (TREE_CODE (decl) == FUNCTION_DECL
	   && ((curalign = DECL_ALIGN (decl)) > bitalign
	       || ((lastalign = DECL_ALIGN (last_decl)) > bitalign)))
    {
      /* Either a prior attribute on the same declaration or one
	 on a prior declaration of the same function specifies
	 stricter alignment than this attribute.  */
      bool note = lastalign != 0;
      if (lastalign)
	curalign = lastalign;

      curalign /= BITS_PER_UNIT;
      unsigned newalign = bitalign / BITS_PER_UNIT;

      auto_diagnostic_group d;
      if ((DECL_USER_ALIGN (decl)
	   || DECL_USER_ALIGN (last_decl)))
	{
	  if (warning (OPT_Wattributes,
		       "ignoring attribute %<%E (%u)%> because it conflicts "
		       "with attribute %<%E (%u)%>",
		       name, newalign, name, curalign)
	      && note)
	    inform (DECL_SOURCE_LOCATION (last_decl),
		    "previous declaration here");
	  /* Only reject attempts to relax/override an alignment
	     explicitly specified previously and accept declarations
	     that appear to relax the implicit function alignment for
	     the target.  Both increasing and increasing the alignment
	     set by -falign-functions setting is permitted.  */
	  *no_add_attrs = true;
	}
      else if (!warn_if_not_aligned_p)
	{
	  /* Do not fail for attribute warn_if_not_aligned.  Otherwise,
	     silently avoid applying the alignment to the declaration
	     because it's implicitly satisfied by the target.  Apply
	     the attribute nevertheless so it can be retrieved by
	     __builtin_has_attribute.  */
	  set_align = false;
	}
    }
  else if (DECL_USER_ALIGN (decl)
	   && DECL_ALIGN (decl) > bitalign)
    /* C++-11 [dcl.align/4]:

	   When multiple alignment-specifiers are specified for an
	   entity, the alignment requirement shall be set to the
	   strictest specified alignment.

      This formally comes from the c++11 specification but we are
      doing it for the GNU attribute syntax as well.  */
    *no_add_attrs = true;
  else if (!warn_if_not_aligned_p
	   && TREE_CODE (decl) == FUNCTION_DECL
	   && DECL_ALIGN (decl) > bitalign)
    {
      /* Don't warn for function alignment here if warn_if_not_aligned_p
	 is true.  It will be warned about later.  */
      if (DECL_USER_ALIGN (decl))
	{
	  /* Only reject attempts to relax/override an alignment
	     explicitly specified previously and accept declarations
	     that appear to relax the implicit function alignment for
	     the target.  Both increasing and increasing the alignment
	     set by -falign-functions setting is permitted.  */
	  error ("alignment for %q+D was previously specified as %d "
		 "and may not be decreased", decl,
		 DECL_ALIGN (decl) / BITS_PER_UNIT);
	  *no_add_attrs = true;
	}
    }
  else if (warn_if_not_aligned_p
	   && TREE_CODE (decl) == FIELD_DECL
	   && !DECL_C_BIT_FIELD (decl))
    {
      SET_DECL_WARN_IF_NOT_ALIGN (decl, bitalign);
      warn_if_not_aligned_p = false;
      set_align = false;
    }

  if (warn_if_not_aligned_p)
    {
      error ("%<warn_if_not_aligned%> may not be specified for %q+D",
	     decl);
      *no_add_attrs = true;
    }
  else if (!is_type && !*no_add_attrs && set_align)
    {
      SET_DECL_ALIGN (decl, bitalign);
      DECL_USER_ALIGN (decl) = 1;
    }

  return NULL_TREE;
}

/* Handle a "aligned" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_aligned_attribute (tree *node, tree name, tree args,
			  int flags, bool *no_add_attrs)
{
  return common_handle_aligned_attribute (node, name, args, flags,
					 no_add_attrs, false);
}

/* Handle a "warn_if_not_aligned" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_warn_if_not_aligned_attribute (tree *node, tree name,
				      tree args, int flags,
				      bool *no_add_attrs)
{
  return common_handle_aligned_attribute (node, name, args, flags,
					  no_add_attrs, true);
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

/* Handle a "noplt" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_noplt_attribute (tree *node, tree name,
		       tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags),
		       bool * ARG_UNUSED (no_add_attrs))
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes,
	       "%qE attribute is only applicable on functions", name);
      *no_add_attrs = true;
      return NULL_TREE;
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
	{
	  if (is_alias)
	    error ("%+qD declared alias after being used", decl);
	  else
	    error ("%+qD declared ifunc after being used", decl);
	}
    }


  return NULL_TREE;
}

/* Handle an "alias" or "ifunc" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_ifunc_attribute (tree *node, tree name, tree args,
			int ARG_UNUSED (flags), bool *no_add_attrs)
{
  return handle_alias_ifunc_attribute (false, node, name, args, no_add_attrs);
}

/* Handle an "alias" or "ifunc" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_alias_attribute (tree *node, tree name, tree args,
			int ARG_UNUSED (flags), bool *no_add_attrs)
{
  return handle_alias_ifunc_attribute (true, node, name, args, no_add_attrs);
}

/* Handle the "copy" attribute NAME by copying the set of attributes
   from the symbol referenced by ARGS to the declaration of *NODE.  */

static tree
handle_copy_attribute (tree *node, tree name, tree args,
		       int flags, bool *no_add_attrs)
{
  /* Do not apply the copy attribute itself.  It serves no purpose
     other than to copy other attributes.  */
  *no_add_attrs = true;

  tree decl = *node;

  tree ref = TREE_VALUE (args);
  if (ref == error_mark_node)
    return NULL_TREE;

  if (TREE_CODE (ref) == STRING_CST)
    {
      /* Explicitly handle this case since using a string literal
	 as an argument is a likely mistake.  */
      error_at (DECL_SOURCE_LOCATION (decl),
		"%qE attribute argument cannot be a string",
		name);
      return NULL_TREE;
    }

  if (CONSTANT_CLASS_P (ref)
      && (INTEGRAL_TYPE_P (TREE_TYPE (ref))
	  || FLOAT_TYPE_P (TREE_TYPE (ref))))
    {
      /* Similar to the string case, since some function attributes
	 accept literal numbers as arguments (e.g., alloc_size or
	 nonnull) using one here is a likely mistake.  */
      error_at (DECL_SOURCE_LOCATION (decl),
		"%qE attribute argument cannot be a constant arithmetic "
		"expression",
		name);
      return NULL_TREE;
    }

  if (ref == node[1])
    {
      /* Another possible mistake (but indirect self-references aren't
	 and diagnosed and shouldn't be).  */
      if (warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wattributes,
		      "%qE attribute ignored on a redeclaration "
		      "of the referenced symbol",
		      name))
	inform (DECL_SOURCE_LOCATION (node[1]),
		"previous declaration here");
      return NULL_TREE;
    }

  /* Consider address-of expressions in the attribute argument
     as requests to copy from the referenced entity.  */
  if (TREE_CODE (ref) == ADDR_EXPR)
    ref = TREE_OPERAND (ref, 0);

  do
    {
      /* Drill down into references to find the referenced decl.  */
      tree_code refcode = TREE_CODE (ref);
      if (refcode == ARRAY_REF
	  || refcode == INDIRECT_REF)
	ref = TREE_OPERAND (ref, 0);
      else if (refcode == COMPONENT_REF)
	ref = TREE_OPERAND (ref, 1);
      else
	break;
    } while (!DECL_P (ref));

  /* For object pointer expressions, consider those to be requests
     to copy from their type, such as in:
       struct __attribute__ (copy ((struct T *)0)) U { ... };
     which copies type attributes from struct T to the declaration
     of struct U.  */
  if ((CONSTANT_CLASS_P (ref) || EXPR_P (ref))
      && POINTER_TYPE_P (TREE_TYPE (ref))
      && !FUNCTION_POINTER_TYPE_P (TREE_TYPE (ref)))
    ref = TREE_TYPE (ref);

  if (DECL_P (decl))
    {
      if ((VAR_P (decl)
	   && (TREE_CODE (ref) == FUNCTION_DECL
	       || (EXPR_P (ref)
		   && POINTER_TYPE_P (TREE_TYPE (ref))
		   && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (TREE_TYPE (ref))))))
	  || (TREE_CODE (decl) == FUNCTION_DECL
	      && (VAR_P (ref)
		  || (EXPR_P (ref)
		      && !FUNC_OR_METHOD_TYPE_P (TREE_TYPE (ref))))))
	{
	  /* It makes no sense to try to copy function attributes
	     to a variable, or variable attributes to a function.  */
	  if (warning (OPT_Wattributes,
		       "%qE attribute ignored on a declaration of "
		       "a different kind than referenced symbol",
		       name)
	      && DECL_P (ref))
	    inform (DECL_SOURCE_LOCATION (ref),
		    "symbol %qD referenced by %qD declared here", ref, decl);
	  return NULL_TREE;
	}

      tree attrs = NULL_TREE;
      if (DECL_P (ref))
	attrs = DECL_ATTRIBUTES (ref);
      else if (TYPE_P (ref))
	attrs = TYPE_ATTRIBUTES (ref);

      /* Copy decl attributes from REF to DECL.  */
      for (tree at = attrs; at; at = TREE_CHAIN (at))
	{
	  /* Avoid copying attributes that affect a symbol linkage,
	     inlining, or visibility since those in all likelihood
	     only apply to the target.
	     FIXME: make it possible to specify which attributes to
	     copy or not to copy in the copy attribute itself.  */
	  tree atname = get_attribute_name (at);
	  if (is_attribute_p ("alias", atname)
	      || is_attribute_p ("always_inline", atname)
	      || is_attribute_p ("gnu_inline", atname)
	      || is_attribute_p ("ifunc", atname)
	      || is_attribute_p ("noinline", atname)
	      || is_attribute_p ("visibility", atname)
	      || is_attribute_p ("weak", atname)
	      || is_attribute_p ("weakref", atname))
	    continue;

	  /* Attribute leaf only applies to extern functions.
	     Avoid copying it to static ones.  */
	  if (!TREE_PUBLIC (decl)
	      && is_attribute_p ("leaf", atname))
	    continue;

	  tree atargs = TREE_VALUE (at);
	  /* Create a copy of just the one attribute ar AT, including
	     its argumentsm and add it to DECL.  */
	  tree attr = tree_cons (atname, copy_list (atargs), NULL_TREE);
	  decl_attributes (node, attr, flags, ref);
	}

      /* Proceed to copy type attributes below.  */
    }
  else if (!TYPE_P (decl))
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"%qE attribute must apply to a declaration",
		name);
      return NULL_TREE;
    }

  /* A function declared with attribute nothrow has the attribute
     attached to it, but a C++ throw() function does not.  */
  if (TREE_NOTHROW (ref))
    TREE_NOTHROW (decl) = true;

  /* Similarly, a function declared with attribute noreturn has it
     attached on to it, but a C11 _Noreturn function does not.  */
  tree reftype = ref;
  if (DECL_P (ref)
      && TREE_THIS_VOLATILE (ref)
      && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (reftype)))
    TREE_THIS_VOLATILE (decl) = true;

  if (DECL_P (ref) || EXPR_P (ref))
    reftype = TREE_TYPE (ref);

  if (POINTER_TYPE_P (reftype))
    reftype = TREE_TYPE (reftype);

  if (!TYPE_P (reftype))
    return NULL_TREE;

  tree attrs = TYPE_ATTRIBUTES (reftype);

  /* Copy type attributes from REF to DECL.  */
  for (tree at = attrs; at; at = TREE_CHAIN (at))
    decl_attributes (node, at, flags, ref);

  return NULL_TREE;
}

/* Handle a "weakref" attribute; arguments as in struct
   attribute_spec.handler.  */

static tree
handle_weakref_attribute (tree *node, tree ARG_UNUSED (name), tree args,
			  int flags, bool *no_add_attrs)
{
  tree attr = NULL_TREE;

  /* We must ignore the attribute when it is associated with
     local-scoped decls, since attribute alias is ignored and many
     such symbols do not even have a DECL_WEAK field.  */
  if (decl_function_context (*node)
      || current_function_decl
      || !VAR_OR_FUNCTION_DECL_P (*node))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (lookup_attribute ("ifunc", DECL_ATTRIBUTES (*node)))
    {
      error ("indirect function %q+D cannot be declared weakref", *node);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* The idea here is that `weakref("name")' mutates into `weakref,
     alias("name")', and weakref without arguments, in turn,
     implicitly adds weak.  */

  if (args)
    {
      attr = tree_cons (get_identifier ("alias"), args, attr);
      attr = tree_cons (get_identifier ("weakref"), NULL_TREE, attr);

      *no_add_attrs = true;

      decl_attributes (node, attr, flags);
    }
  else
    {
      if (lookup_attribute ("alias", DECL_ATTRIBUTES (*node)))
	error_at (DECL_SOURCE_LOCATION (*node),
		  "weakref attribute must appear before alias attribute");

      /* Can't call declare_weak because it wants this to be TREE_PUBLIC,
	 and that isn't supported; and because it wants to add it to
	 the list of weak decls, which isn't helpful.  */
      DECL_WEAK (*node) = 1;
    }

  if (decl_in_symtab_p (*node))
    {
      struct symtab_node *n = symtab_node::get (*node);
      if (n && n->refuse_visibility_changes)
	error ("%+qD declared weakref after being used", *node);
    }

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
	/* OK */;
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
      error ("visibility argument must be one of \"default\", \"hidden\", \"protected\" or \"internal\"");
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

/* Handle an "tls_model" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_tls_model_attribute (tree *node, tree name, tree args,
			    int ARG_UNUSED (flags),
			    bool *ARG_UNUSED (no_add_attrs))
{
  tree id;
  tree decl = *node;
  enum tls_model kind;

  if (!VAR_P (decl))
    {
      warning (OPT_Wattributes, "%qE attribute ignored because %qD "
	       "is not a variable",
	       name, decl);
      return NULL_TREE;
    }

  if (!DECL_THREAD_LOCAL_P (decl))
    {
      warning (OPT_Wattributes, "%qE attribute ignored because %qD does "
	       "not have thread storage duration", name, decl);
      return NULL_TREE;
    }

  kind = DECL_TLS_MODEL (decl);
  id = TREE_VALUE (args);
  if (TREE_CODE (id) != STRING_CST)
    {
      error ("%qE argument not a string", name);
      return NULL_TREE;
    }

  if (!strcmp (TREE_STRING_POINTER (id), "local-exec"))
    kind = TLS_MODEL_LOCAL_EXEC;
  else if (!strcmp (TREE_STRING_POINTER (id), "initial-exec"))
    kind = TLS_MODEL_INITIAL_EXEC;
  else if (!strcmp (TREE_STRING_POINTER (id), "local-dynamic"))
    kind = optimize ? TLS_MODEL_LOCAL_DYNAMIC : TLS_MODEL_GLOBAL_DYNAMIC;
  else if (!strcmp (TREE_STRING_POINTER (id), "global-dynamic"))
    kind = TLS_MODEL_GLOBAL_DYNAMIC;
  else
    error ("%qE argument must be one of %qs, %qs, %qs, or %qs",
	   name,
	   "local-exec", "initial-exec", "local-dynamic", "global-dynamic");

  set_decl_tls_model (decl, kind);
  return NULL_TREE;
}

/* Handle a "no_instrument_function" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_instrument_function_attribute (tree *node, tree name,
					 tree ARG_UNUSED (args),
					 int ARG_UNUSED (flags),
					 bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"%qE attribute applies only to functions", name);
      *no_add_attrs = true;
    }
  else
    DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl) = 1;

  return NULL_TREE;
}

/* Handle a "no_profile_instrument_function" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_profile_instrument_function_attribute (tree *node, tree name, tree,
						 int, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "malloc" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_malloc_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			 int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL
      && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (*node))))
    DECL_IS_MALLOC (*node) = 1;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "alloc_size" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_alloc_size_attribute (tree *node, tree name, tree args,
			     int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;
  tree rettype = TREE_TYPE (decl);
  if (!POINTER_TYPE_P (rettype))
    {
      warning (OPT_Wattributes,
	       "%qE attribute ignored on a function returning %qT",
	       name, rettype);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  for (int i = 1; args; ++i)
    {
      tree pos = TREE_VALUE (args);
      /* NEXT is null when the attribute includes just one argument.
	 That's used to tell positional_argument to avoid mentioning
	 the argument number in diagnostics (since there's just one
	 mentioning it is unnecessary and coule be confusing).  */
      tree next = TREE_CHAIN (args);
      if (tree val = positional_argument (decl, name, pos, INTEGER_TYPE,
					  next || i > 1 ? i : 0))
	TREE_VALUE (args) = val;
      else
	{
	  *no_add_attrs = true;
	  break;
	}

      args = next;
    }

  return NULL_TREE;
}

/* Handle a "alloc_align" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_alloc_align_attribute (tree *node, tree name, tree args, int,
			      bool *no_add_attrs)
{
  tree decl = *node;
  tree rettype = TREE_TYPE (decl);
  if (!POINTER_TYPE_P (rettype))
    {
      warning (OPT_Wattributes,
	       "%qE attribute ignored on a function returning %qT",
	       name, rettype);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (!positional_argument (*node, name, TREE_VALUE (args), INTEGER_TYPE))
    *no_add_attrs = true;

  return NULL_TREE;
}

/* Handle a "assume_aligned" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_assume_aligned_attribute (tree *node, tree name, tree args, int,
				 bool *no_add_attrs)
{
  tree decl = *node;
  tree rettype = TREE_TYPE (decl);
  if (TREE_CODE (rettype) != POINTER_TYPE)
    {
      warning (OPT_Wattributes,
	       "%qE attribute ignored on a function returning %qT",
	       name, rettype);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* The alignment specified by the first argument.  */
  tree align = NULL_TREE;

  for (; args; args = TREE_CHAIN (args))
    {
      tree val = TREE_VALUE (args);
      if (val && TREE_CODE (val) != IDENTIFIER_NODE
	  && TREE_CODE (val) != FUNCTION_DECL)
	val = default_conversion (val);

      if (!tree_fits_shwi_p (val))
	{
	  warning (OPT_Wattributes,
		   "%qE attribute %E is not an integer constant",
		   name, val);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}

      if (!align)
	{
	  /* Validate and save the alignment.  */
	  if (!integer_pow2p (val))
	    {
	      warning (OPT_Wattributes,
		       "%qE attribute argument %E is not a power of 2",
		       name, val);
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }

	  align = val;
	}
      else if (tree_int_cst_sgn (val) < 0 || tree_int_cst_le (align, val))
	{
	  /* The misalignment specified by the second argument
	     must be non-negative and less than the alignment.  */
	  warning (OPT_Wattributes,
		   "%qE attribute argument %E is not in the range [0, %E)",
		   name, val, align);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
    }
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

/* Handle a "warn_unused" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_warn_unused_attribute (tree *node, tree name,
			      tree args ATTRIBUTE_UNUSED,
			      int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TYPE_P (*node))
    /* Do nothing else, just set the attribute.  We'll get at
       it later with lookup_attribute.  */
    ;
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "omp declare simd" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_omp_declare_simd_attribute (tree *, tree, tree, int, bool *)
{
  return NULL_TREE;
}

/* Handle a "simd" attribute.  */

static tree
handle_simd_attribute (tree *node, tree name, tree args, int, bool *no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    {
      tree t = get_identifier ("omp declare simd");
      tree attr = NULL_TREE;
      if (args)
	{
	  tree id = TREE_VALUE (args);

	  if (TREE_CODE (id) != STRING_CST)
	    {
	      error ("attribute %qE argument not a string", name);
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }

	  if (strcmp (TREE_STRING_POINTER (id), "notinbranch") == 0)
	    attr = build_omp_clause (DECL_SOURCE_LOCATION (*node),
				     OMP_CLAUSE_NOTINBRANCH);
	  else if (strcmp (TREE_STRING_POINTER (id), "inbranch") == 0)
	    attr = build_omp_clause (DECL_SOURCE_LOCATION (*node),
				     OMP_CLAUSE_INBRANCH);
	  else
	    {
	      error ("only %<inbranch%> and %<notinbranch%> flags are "
		     "allowed for %<__simd__%> attribute");
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }
	}

      DECL_ATTRIBUTES (*node)
	= tree_cons (t, build_tree_list (NULL_TREE, attr),
		     DECL_ATTRIBUTES (*node));
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "omp declare target" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_omp_declare_target_attribute (tree *, tree, tree, int, bool *)
{
  return NULL_TREE;
}

/* Handle a "returns_twice" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
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

/* Handle a "no_limit_stack" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_no_limit_stack_attribute (tree *node, tree name,
				 tree ARG_UNUSED (args),
				 int ARG_UNUSED (flags),
				 bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
	     "%qE attribute applies only to functions", name);
      *no_add_attrs = true;
    }
  else if (DECL_INITIAL (decl))
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"can%'t set %qE attribute after definition", name);
      *no_add_attrs = true;
    }
  else
    DECL_NO_LIMIT_STACK (decl) = 1;

  return NULL_TREE;
}

/* Handle a "pure" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
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

/* Digest an attribute list destined for a transactional memory statement.
   ALLOWED is the set of attributes that are allowed for this statement;
   return the attribute we parsed.  Multiple attributes are never allowed.  */

int
parse_tm_stmt_attr (tree attrs, int allowed)
{
  tree a_seen = NULL;
  int m_seen = 0;

  for ( ; attrs ; attrs = TREE_CHAIN (attrs))
    {
      tree a = TREE_PURPOSE (attrs);
      int m = 0;

      if (is_attribute_p ("outer", a))
	m = TM_STMT_ATTR_OUTER;

      if ((m & allowed) == 0)
	{
	  warning (OPT_Wattributes, "%qE attribute directive ignored", a);
	  continue;
	}

      if (m_seen == 0)
	{
	  a_seen = a;
	  m_seen = m;
	}
      else if (m_seen == m)
	warning (OPT_Wattributes, "%qE attribute duplicated", a);
      else
	warning (OPT_Wattributes, "%qE attribute follows %qE", a, a_seen);
    }

  return m_seen;
}

/* Transform a TM attribute name into a maskable integer and back.
   Note that NULL (i.e. no attribute) is mapped to UNKNOWN, corresponding
   to how the lack of an attribute is treated.  */

int
tm_attr_to_mask (tree attr)
{
  if (attr == NULL)
    return 0;
  if (is_attribute_p ("transaction_safe", attr))
    return TM_ATTR_SAFE;
  if (is_attribute_p ("transaction_callable", attr))
    return TM_ATTR_CALLABLE;
  if (is_attribute_p ("transaction_pure", attr))
    return TM_ATTR_PURE;
  if (is_attribute_p ("transaction_unsafe", attr))
    return TM_ATTR_IRREVOCABLE;
  if (is_attribute_p ("transaction_may_cancel_outer", attr))
    return TM_ATTR_MAY_CANCEL_OUTER;
  return 0;
}

tree
tm_mask_to_attr (int mask)
{
  const char *str;
  switch (mask)
    {
    case TM_ATTR_SAFE:
      str = "transaction_safe";
      break;
    case TM_ATTR_CALLABLE:
      str = "transaction_callable";
      break;
    case TM_ATTR_PURE:
      str = "transaction_pure";
      break;
    case TM_ATTR_IRREVOCABLE:
      str = "transaction_unsafe";
      break;
    case TM_ATTR_MAY_CANCEL_OUTER:
      str = "transaction_may_cancel_outer";
      break;
    default:
      gcc_unreachable ();
    }
  return get_identifier (str);
}

/* Return the first TM attribute seen in LIST.  */

tree
find_tm_attribute (tree list)
{
  for (; list ; list = TREE_CHAIN (list))
    {
      tree name = TREE_PURPOSE (list);
      if (tm_attr_to_mask (name) != 0)
	return name;
    }
  return NULL_TREE;
}

/* Handle the TM attributes; arguments as in struct attribute_spec.handler.
   Here we accept only function types, and verify that none of the other
   function TM attributes are also applied.  */
/* ??? We need to accept class types for C++, but not C.  This greatly
   complicates this function, since we can no longer rely on the extra
   processing given by function_type_required.  */

static tree
handle_tm_attribute (tree *node, tree name, tree args,
		     int flags, bool *no_add_attrs)
{
  /* Only one path adds the attribute; others don't.  */
  *no_add_attrs = true;

  switch (TREE_CODE (*node))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
      /* Only tm_callable and tm_safe apply to classes.  */
      if (tm_attr_to_mask (name) & ~(TM_ATTR_SAFE | TM_ATTR_CALLABLE))
	goto ignored;
      /* FALLTHRU */

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree old_name = find_tm_attribute (TYPE_ATTRIBUTES (*node));
	if (old_name == name)
	  ;
	else if (old_name != NULL_TREE)
	  error ("type was previously declared %qE", old_name);
	else
	  *no_add_attrs = false;
      }
      break;

    case FUNCTION_DECL:
      {
	/* transaction_safe_dynamic goes on the FUNCTION_DECL, but we also
	   want to set transaction_safe on the type.  */
	gcc_assert (is_attribute_p ("transaction_safe_dynamic", name));
	if (!TYPE_P (DECL_CONTEXT (*node)))
	  error_at (DECL_SOURCE_LOCATION (*node),
		    "%<transaction_safe_dynamic%> may only be specified for "
		    "a virtual function");
	*no_add_attrs = false;
	decl_attributes (&TREE_TYPE (*node),
			 build_tree_list (get_identifier ("transaction_safe"),
					  NULL_TREE),
			 0);
	break;
      }

    case POINTER_TYPE:
      {
	enum tree_code subcode = TREE_CODE (TREE_TYPE (*node));
	if (subcode == FUNCTION_TYPE || subcode == METHOD_TYPE)
	  {
	    tree fn_tmp = TREE_TYPE (*node);
	    decl_attributes (&fn_tmp, tree_cons (name, args, NULL), 0);
	    *node = build_pointer_type (fn_tmp);
	    break;
	  }
      }
      /* FALLTHRU */

    default:
      /* If a function is next, pass it on to be tried next.  */
      if (flags & (int) ATTR_FLAG_FUNCTION_NEXT)
	return tree_cons (name, args, NULL);

    ignored:
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      break;
    }

  return NULL_TREE;
}

/* Handle the TM_WRAP attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_tm_wrap_attribute (tree *node, tree name, tree args,
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;

  /* We don't need the attribute even on success, since we
     record the entry in an external table.  */
  *no_add_attrs = true;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    warning (OPT_Wattributes, "%qE attribute ignored", name);
  else
    {
      tree wrap_decl = TREE_VALUE (args);
      if (error_operand_p (wrap_decl))
	;
      else if (TREE_CODE (wrap_decl) != IDENTIFIER_NODE
	       && !VAR_OR_FUNCTION_DECL_P (wrap_decl))
	error ("%qE argument not an identifier", name);
      else
	{
	  if (TREE_CODE (wrap_decl) == IDENTIFIER_NODE)
	    wrap_decl = lookup_name (wrap_decl);
	  if (wrap_decl && TREE_CODE (wrap_decl) == FUNCTION_DECL)
	    {
	      if (lang_hooks.types_compatible_p (TREE_TYPE (decl),
						 TREE_TYPE (wrap_decl)))
		record_tm_replacement (wrap_decl, decl);
	      else
		error ("%qD is not compatible with %qD", wrap_decl, decl);
	    }
	  else
	    error ("%qE argument is not a function", name);
	}
    }

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

/* Handle a "deprecated" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_deprecated_attribute (tree *node, tree name,
			     tree args, int flags,
			     bool *no_add_attrs)
{
  tree type = NULL_TREE;
  int warn = 0;
  tree what = NULL_TREE;

  if (!args)
    *no_add_attrs = true;
  else if (TREE_CODE (TREE_VALUE (args)) != STRING_CST)
    {
      error ("deprecated message is not a string");
      *no_add_attrs = true;
    }

  if (DECL_P (*node))
    {
      tree decl = *node;
      type = TREE_TYPE (decl);

      if (TREE_CODE (decl) == TYPE_DECL
	  || TREE_CODE (decl) == PARM_DECL
	  || VAR_OR_FUNCTION_DECL_P (decl)
	  || TREE_CODE (decl) == FIELD_DECL
	  || TREE_CODE (decl) == CONST_DECL
	  || objc_method_decl (TREE_CODE (decl)))
	TREE_DEPRECATED (decl) = 1;
      else
	warn = 1;
    }
  else if (TYPE_P (*node))
    {
      if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
	*node = build_variant_type_copy (*node);
      TREE_DEPRECATED (*node) = 1;
      type = *node;
    }
  else
    warn = 1;

  if (warn)
    {
      *no_add_attrs = true;
      if (type && TYPE_NAME (type))
	{
	  if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	    what = TYPE_NAME (*node);
	  else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (type)))
	    what = DECL_NAME (TYPE_NAME (type));
	}
      if (what)
	warning (OPT_Wattributes, "%qE attribute ignored for %qE", name, what);
      else
	warning (OPT_Wattributes, "%qE attribute ignored", name);
    }

  return NULL_TREE;
}

/* Return the "base" type from TYPE that is suitable to apply attribute
   vector_size to by stripping arrays, function types, etc.  */
static tree
type_for_vector_size (tree type)
{
  /* We need to provide for vector pointers, vector arrays, and
     functions returning vectors.  For example:

       __attribute__((vector_size(16))) short *foo;

     In this case, the mode is SI, but the type being modified is
     HI, so we need to look further.  */

  while (POINTER_TYPE_P (type)
	 || TREE_CODE (type) == FUNCTION_TYPE
	 || TREE_CODE (type) == METHOD_TYPE
	 || TREE_CODE (type) == ARRAY_TYPE
	 || TREE_CODE (type) == OFFSET_TYPE)
    type = TREE_TYPE (type);

  return type;
}

/* Given TYPE, return the base type to which the vector_size attribute
   ATNAME with ARGS, when non-null, can be applied, if one exists.
   On success and when both ARGS and PTRNUNITS are non-null, set
   *PTRNUNINTS to the number of vector units.  When PTRNUNITS is not
   null, issue a warning when the attribute argument is not constant
   and an error if there is no such type.  Otherwise issue a warning
   in the latter case and return null.  */

static tree
type_valid_for_vector_size (tree type, tree atname, tree args,
			    unsigned HOST_WIDE_INT *ptrnunits)
{
  bool error_p = ptrnunits != NULL;

  /* Get the mode of the type being modified.  */
  machine_mode orig_mode = TYPE_MODE (type);

  if ((!INTEGRAL_TYPE_P (type)
       && !SCALAR_FLOAT_TYPE_P (type)
       && !FIXED_POINT_TYPE_P (type))
      || (!SCALAR_FLOAT_MODE_P (orig_mode)
	  && GET_MODE_CLASS (orig_mode) != MODE_INT
	  && !ALL_SCALAR_FIXED_POINT_MODE_P (orig_mode))
      || !tree_fits_uhwi_p (TYPE_SIZE_UNIT (type))
      || TREE_CODE (type) == BOOLEAN_TYPE)
    {
      if (error_p)
	error ("invalid vector type for attribute %qE", atname);
      else
	warning (OPT_Wattributes, "invalid vector type for attribute %qE",
		 atname);
      return NULL_TREE;
    }

  /* When no argument has been provided this is just a request to validate
     the type above.  Return TYPE to indicate success.  */
  if (!args)
    return type;

  tree size = TREE_VALUE (args);
  /* Erroneous arguments have already been diagnosed.  */
  if (size == error_mark_node)
    return NULL_TREE;

  if (size && TREE_CODE (size) != IDENTIFIER_NODE
      && TREE_CODE (size) != FUNCTION_DECL)
    size = default_conversion (size);

  if (TREE_CODE (size) != INTEGER_CST)
    {
      if (error_p)
	error ("%qE attribute argument value %qE is not an integer constant",
	       atname, size);
      else
	warning (OPT_Wattributes,
		 "%qE attribute argument value %qE is not an integer constant",
		 atname, size);
      return NULL_TREE;
    }

  if (!TYPE_UNSIGNED (TREE_TYPE (size))
      && tree_int_cst_sgn (size) < 0)
    {
      if (error_p)
	error ("%qE attribute argument value %qE is negative",
	       atname, size);
      else
	warning (OPT_Wattributes,
		 "%qE attribute argument value %qE is negative",
		 atname, size);
      return NULL_TREE;
    }

  /* The attribute argument value is constrained by the maximum bit
     alignment representable in unsigned int on the host.  */
  unsigned HOST_WIDE_INT vecsize;
  unsigned HOST_WIDE_INT maxsize = tree_to_uhwi (max_object_size ());
  if (!tree_fits_uhwi_p (size)
      || (vecsize = tree_to_uhwi (size)) > maxsize)
    {
      if (error_p)
	error ("%qE attribute argument value %qE exceeds %wu",
	       atname, size, maxsize);
      else
	warning (OPT_Wattributes,
		 "%qE attribute argument value %qE exceeds %wu",
		 atname, size, maxsize);
      return NULL_TREE;
    }

  if (vecsize % tree_to_uhwi (TYPE_SIZE_UNIT (type)))
    {
      if (error_p)
	error ("vector size not an integral multiple of component size");
      return NULL_TREE;
    }

  if (vecsize == 0)
    {
      error ("zero vector size");
      return NULL;
    }

  /* Calculate how many units fit in the vector.  */
  unsigned HOST_WIDE_INT nunits = vecsize / tree_to_uhwi (TYPE_SIZE_UNIT (type));
  if (nunits & (nunits - 1))
    {
      if (error_p)
	error ("number of components of the vector not a power of two");
      else
	warning (OPT_Wattributes,
		 "number of components of the vector not a power of two");
      return NULL_TREE;
    }

  if (ptrnunits)
    *ptrnunits = nunits;

  return type;
}

/* Handle a "vector_size" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_vector_size_attribute (tree *node, tree name, tree args,
			      int ARG_UNUSED (flags),
			      bool *no_add_attrs)
{
  *no_add_attrs = true;

  /* Determine the "base" type to apply the attribute to.  */
  tree type = type_for_vector_size (*node);

  /* Get the vector size (in bytes) and let the function compute
     the number of vector units.  */
  unsigned HOST_WIDE_INT nunits;
  type = type_valid_for_vector_size (type, name, args, &nunits);
  if (!type)
    return NULL_TREE;

  tree new_type = build_vector_type (type, nunits);

  /* Build back pointers if needed.  */
  *node = lang_hooks.types.reconstruct_complex_type (*node, new_type);

  return NULL_TREE;
}

/* Handle the "nonnull" attribute.  */

static tree
handle_nonnull_attribute (tree *node, tree name,
			  tree args, int ARG_UNUSED (flags),
			  bool *no_add_attrs)
{
  tree type = *node;

  /* If no arguments are specified, all pointer arguments should be
     non-null.  Verify a full prototype is given so that the arguments
     will have the correct types when we actually check them later.
     Avoid diagnosing type-generic built-ins since those have no
     prototype.  */
  if (!args)
    {
      if (!prototype_p (type)
	  && (!TYPE_ATTRIBUTES (type)
	      || !lookup_attribute ("type generic", TYPE_ATTRIBUTES (type))))
	{
	  error ("nonnull attribute without arguments on a non-prototype");
	  *no_add_attrs = true;
	}
      return NULL_TREE;
    }

  for (int i = 1; args; ++i)
    {
      tree pos = TREE_VALUE (args);
      /* NEXT is null when the attribute includes just one argument.
	 That's used to tell positional_argument to avoid mentioning
	 the argument number in diagnostics (since there's just one
	 mentioning it is unnecessary and coule be confusing).  */
      tree next = TREE_CHAIN (args);
      if (tree val = positional_argument (type, name, pos, POINTER_TYPE,
					  next || i > 1 ? i : 0))
	TREE_VALUE (args) = val;
      else
	{
	  *no_add_attrs = true;
	  break;
	}
      args = next;
    }

  return NULL_TREE;
}

/* Handle the "nonstring" variable attribute.  */

static tree
handle_nonstring_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			    int ARG_UNUSED (flags), bool *no_add_attrs)
{
  gcc_assert (!args);
  tree_code code = TREE_CODE (*node);

  if (VAR_P (*node)
      || code == FIELD_DECL
      || code == PARM_DECL)
    {
      tree type = TREE_TYPE (*node);

      if (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
	{
	  /* Accept the attribute on arrays and pointers to all three
	     narrow character types.  */
	  tree eltype = TREE_TYPE (type);
	  eltype = TYPE_MAIN_VARIANT (eltype);
	  if (eltype == char_type_node
	      || eltype == signed_char_type_node
	      || eltype == unsigned_char_type_node)
	    return NULL_TREE;
	}

      warning (OPT_Wattributes,
	       "%qE attribute ignored on objects of type %qT",
	       name, type);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (code == FUNCTION_DECL)
    warning (OPT_Wattributes,
	     "%qE attribute does not apply to functions", name);
  else if (code == TYPE_DECL)
    warning (OPT_Wattributes,
	     "%qE attribute does not apply to types", name);
  else
    warning (OPT_Wattributes, "%qE attribute ignored", name);

  *no_add_attrs = true;
  return NULL_TREE;
}

/* Handle a "nothrow" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
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

/* Handle a "cleanup" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_cleanup_attribute (tree *node, tree name, tree args,
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;
  tree cleanup_id, cleanup_decl;

  /* ??? Could perhaps support cleanups on TREE_STATIC, much like we do
     for global destructors in C++.  This requires infrastructure that
     we don't have generically at the moment.  It's also not a feature
     we'd be missing too much, since we do have attribute constructor.  */
  if (!VAR_P (decl) || TREE_STATIC (decl))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Verify that the argument is a function in scope.  */
  /* ??? We could support pointers to functions here as well, if
     that was considered desirable.  */
  cleanup_id = TREE_VALUE (args);
  if (TREE_CODE (cleanup_id) != IDENTIFIER_NODE)
    {
      error ("cleanup argument not an identifier");
      *no_add_attrs = true;
      return NULL_TREE;
    }
  cleanup_decl = lookup_name (cleanup_id);
  if (!cleanup_decl || TREE_CODE (cleanup_decl) != FUNCTION_DECL)
    {
      error ("cleanup argument not a function");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* That the function has proper type is checked with the
     eventual call to build_function_call.  */

  return NULL_TREE;
}

/* Handle a "warn_unused_result" attribute.  No special handling.  */

static tree
handle_warn_unused_result_attribute (tree *node, tree name,
			       tree ARG_UNUSED (args),
			       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  /* Ignore the attribute for functions not returning any value.  */
  if (VOID_TYPE_P (TREE_TYPE (*node)))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "sentinel" attribute.  */

static tree
handle_sentinel_attribute (tree *node, tree name, tree args,
			   int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (!prototype_p (*node))
    {
      warning (OPT_Wattributes,
	       "%qE attribute requires prototypes with named arguments", name);
      *no_add_attrs = true;
    }
  else
    {
      if (!stdarg_p (*node))
	{
	  warning (OPT_Wattributes,
		   "%qE attribute only applies to variadic functions", name);
	  *no_add_attrs = true;
	}
    }

  if (args)
    {
      tree position = TREE_VALUE (args);
      if (position && TREE_CODE (position) != IDENTIFIER_NODE
	  && TREE_CODE (position) != FUNCTION_DECL)
	position = default_conversion (position);

      if (TREE_CODE (position) != INTEGER_CST
	  || !INTEGRAL_TYPE_P (TREE_TYPE (position)))
	{
	  warning (OPT_Wattributes,
		   "requested position is not an integer constant");
	  *no_add_attrs = true;
	}
      else
	{
	  if (tree_int_cst_lt (position, integer_zero_node))
	    {
	      warning (OPT_Wattributes,
		       "requested position is less than zero");
	      *no_add_attrs = true;
	    }
	}
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

/* Handle a "target" attribute.  */

static tree
handle_target_attribute (tree *node, tree name, tree args, int flags,
			 bool *no_add_attrs)
{
  /* Ensure we have a function type.  */
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  else if (lookup_attribute ("target_clones", DECL_ATTRIBUTES (*node)))
    {
      warning (OPT_Wattributes, "%qE attribute ignored due to conflict "
		   "with %qs attribute", name, "target_clones");
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

/* Handle a "target_clones" attribute.  */

static tree
handle_target_clones_attribute (tree *node, tree name, tree ARG_UNUSED (args),
			  int ARG_UNUSED (flags), bool *no_add_attrs)
{
  /* Ensure we have a function type.  */
  if (TREE_CODE (*node) == FUNCTION_DECL)
    {
      if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (*node)))
	{
	  warning (OPT_Wattributes, "%qE attribute ignored due to conflict "
		   "with %qs attribute", name, "always_inline");
	  *no_add_attrs = true;
	}
      else if (lookup_attribute ("target", DECL_ATTRIBUTES (*node)))
	{
	  warning (OPT_Wattributes, "%qE attribute ignored due to conflict "
		   "with %qs attribute", name, "target");
	  *no_add_attrs = true;
	}
      else
      /* Do not inline functions with multiple clone targets.  */
	DECL_UNINLINABLE (*node) = 1;
    }
  else
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
}

/* For handling "optimize" attribute. arguments as in
   struct attribute_spec.handler.  */

static tree
handle_optimize_attribute (tree *node, tree name, tree args,
			   int ARG_UNUSED (flags), bool *no_add_attrs)
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
      cl_optimization_save (&cur_opts, &global_options);

      /* If we previously had some optimization options, use them as the
	 default.  */
      if (old_opts)
	cl_optimization_restore (&global_options,
				 TREE_OPTIMIZATION (old_opts));

      /* Parse options, and update the vector.  */
      parse_optimize_options (args, true);
      DECL_FUNCTION_SPECIFIC_OPTIMIZATION (*node)
	= build_optimization_node (&global_options);

      /* Restore current options.  */
      cl_optimization_restore (&global_options, &cur_opts);
    }

  return NULL_TREE;
}

/* Handle a "no_split_stack" attribute.  */

static tree
handle_no_split_stack_attribute (tree *node, tree name,
				 tree ARG_UNUSED (args),
				 int ARG_UNUSED (flags),
				 bool *no_add_attrs)
{
  tree decl = *node;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"%qE attribute applies only to functions", name);
      *no_add_attrs = true;
    }
  else if (DECL_INITIAL (decl))
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"can%'t set %qE attribute after definition", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "returns_nonnull" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_returns_nonnull_attribute (tree *node, tree, tree, int,
				  bool *no_add_attrs)
{
  // Even without a prototype we still have a return type we can check.
  if (TREE_CODE (TREE_TYPE (*node)) != POINTER_TYPE)
    {
      error ("returns_nonnull attribute on a function not returning a pointer");
      *no_add_attrs = true;
    }
  return NULL_TREE;
}

/* Handle a "designated_init" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_designated_init_attribute (tree *node, tree name, tree, int,
				  bool *no_add_attrs)
{
  if (TREE_CODE (*node) != RECORD_TYPE)
    {
      error ("%qE attribute is only valid on %<struct%> type", name);
      *no_add_attrs = true;
    }
  return NULL_TREE;
}


/* Handle a "fallthrough" attribute; arguments as in struct
   attribute_spec.handler.  */

static tree
handle_fallthrough_attribute (tree *, tree name, tree, int,
			      bool *no_add_attrs)
{
  warning (OPT_Wattributes, "%qE attribute ignored", name);
  *no_add_attrs = true;
  return NULL_TREE;
}

/* Handle a "patchable_function_entry" attributes; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_patchable_function_entry_attribute (tree *, tree name, tree args,
					   int, bool *no_add_attrs)
{
  for (; args; args = TREE_CHAIN (args))
    {
      tree val = TREE_VALUE (args);
      if (val && TREE_CODE (val) != IDENTIFIER_NODE
	  && TREE_CODE (val) != FUNCTION_DECL)
	val = default_conversion (val);

      if (!tree_fits_uhwi_p (val))
	{
	  warning (OPT_Wattributes,
		   "%qE attribute argument %qE is not an integer constant",
		   name, val);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
    }
  return NULL_TREE;
}

/* Attempt to partially validate a single attribute ATTR as if
   it were to be applied to an entity OPER.  */

static bool
validate_attribute (location_t atloc, tree oper, tree attr)
{
  /* Determine whether the name of the attribute is valid
     and fail with an error if not.  */
  tree atname = get_attribute_name (attr);
  if (!lookup_attribute_spec (atname))
    {
      if (atloc != UNKNOWN_LOCATION)
	error_at (atloc, "unknown attribute %qE", atname);
      return false;
    }

  tree args = TREE_VALUE (attr);
  if (!args)
    return true;

  /* FIXME: Do some validation.  */
  const char *atstr = IDENTIFIER_POINTER (atname);
  if (!strcmp (atstr, "format"))
    return true;

  /* Only when attribute arguments have been provided try to validate
     the whole thing.  decl_attributes doesn't return an indication of
     success or failure so proceed regardless.  */
  const char tmpname[] = "__builtin_has_attribute_tmp.";
  tree tmpid = get_identifier (tmpname);
  tree tmpdecl;
  if (!strcmp (atstr, "vector_size"))
    {
      tree type = TYPE_P (oper) ? oper : TREE_TYPE (oper);
      /* Check for function type here since type_for_vector_size
	 strips it while looking for a function's return type.  */
      if (FUNC_OR_METHOD_TYPE_P (type))
	{
	  warning_at (atloc, OPT_Wattributes,
		      "invalid operand type %qT for %qs", type, atstr);
	  return false;
	}

      type = type_for_vector_size (type);
      if (VECTOR_TYPE_P (type))
	type = TREE_TYPE (type);
      /* Avoid trying to apply attribute vector_size to OPER since
	 it's overly restrictive.  Simply make sure it has the right
	 type.  */
      return type_valid_for_vector_size (type, atname, args, NULL);
    }

  if (TYPE_P (oper))
    tmpdecl = build_decl (atloc, TYPE_DECL, tmpid, oper);
  else if (DECL_P (oper))
    tmpdecl = build_decl (atloc, TREE_CODE (oper), tmpid, TREE_TYPE (oper));
  else if (EXPR_P (oper))
    tmpdecl = build_decl (atloc, TYPE_DECL, tmpid, TREE_TYPE (oper));
  else
    return false;

  /* Temporarily clear CURRENT_FUNCTION_DECL to make decl_attributes
     believe the DECL declared above is at file scope.  (See bug 87526.)  */
  tree save_curfunc = current_function_decl;
  current_function_decl = NULL_TREE;
  if (DECL_P (tmpdecl))
    {
      if (DECL_P (oper))
	/* An alias cannot be a definition so declare the symbol extern.  */
	DECL_EXTERNAL (tmpdecl) = true;
      /* Attribute visibility only applies to symbols visible from other
	 translation units so make it "public."   */
      TREE_PUBLIC (tmpdecl) = TREE_PUBLIC (oper);
    }
  decl_attributes (&tmpdecl, attr, 0);
  current_function_decl = save_curfunc;

  /* FIXME: Change decl_attributes to indicate success or failure (and
     parameterize it to avoid failing with errors).  */
  return true;
}

/* Return true if the DECL, EXPR, or TYPE t has been declared with
   attribute ATTR.  For DECL, consider also its type.  For EXPR,
   consider just its type.  */

bool
has_attribute (location_t atloc, tree t, tree attr, tree (*convert)(tree))
{
  if (!attr || !t || t == error_mark_node)
    return false;

  if (!validate_attribute (atloc, t, attr))
    return false;

  tree type = NULL_TREE;
  tree expr = NULL_TREE;
  if (TYPE_P (t))
    type = t;
  else
    {
      do
	{
	  /* Determine the array element/member declaration from
	     a COMPONENT_REF and an INDIRECT_REF involving a refeence.  */
	  STRIP_NOPS (t);
	  tree_code code = TREE_CODE (t);
	  if (code == INDIRECT_REF)
	    {
	      tree op0 = TREE_OPERAND (t, 0);
	      if (TREE_CODE (TREE_TYPE (op0)) == REFERENCE_TYPE)
		t = op0;
	      else
		break;
	    }
	  else if (code == COMPONENT_REF)
	    t = TREE_OPERAND (t, 1);
	  else
	    break;
	} while (true);
      expr = t;
    }

  /* Set to true when an attribute is found in the referenced entity
     that matches the specified attribute.  */
  bool found_match = false;

  tree atname = get_attribute_name (attr);
  const char *namestr = IDENTIFIER_POINTER (atname);

   /* Iterate once for a type and twice for a function or variable
     declaration: once for the DECL and the second time for its
     TYPE.  */
  for (bool done = false; !found_match && !done; )
    {
      tree atlist;
      if (type)
	{
	  if (type == error_mark_node)
	    {
	      /* This could be a label.  FIXME: add support for labels.  */
	      warning_at (atloc, OPT_Wattributes,
			  (TYPE_P (t)
			   ? G_("%qs attribute not supported for %qT "
				"in %<__builtin_has_attribute%>")
			   : G_("%qs attribute not supported for %qE "
				"in %<__builtin_has_attribute%>")),
			  namestr, t);
	      return false;
	    }

	  /* Clear EXPR to prevent considering it again below.  */
	  atlist = TYPE_ATTRIBUTES (type);
	  expr = NULL_TREE;
	  done = true;
	}
      else if (DECL_P (expr))
	{
	  /* Set TYPE to the DECL's type to process it on the next
	     iteration.  */
	  atlist = DECL_ATTRIBUTES (expr);
	  type = TREE_TYPE (expr);
	}
      else
	{
	  type = TREE_TYPE (expr);
	  atlist = TYPE_ATTRIBUTES (type);
	  done = true;
	}

     /* True when an attribute with the sought name (though not necessarily
	 with the sought attributes) has been found on the attribute chain.  */
      bool found_attr = false;

      /* When clear, the first mismatched attribute argument results
	 in failure.  Otherwise, the first matched attribute argument
	 results in success.  */
      bool attr_nonnull = !strcmp ("nonnull", namestr);
      bool ignore_mismatches = attr_nonnull;

      /* Iterate over the instances of the sought attribute on the DECL or
	 TYPE (there may be multiple instances with different arguments).  */
      for (; (atlist = lookup_attribute (namestr, atlist));
	   found_attr = true, atlist = TREE_CHAIN (atlist))
	{
	  /* If there are no arguments to match the result is true except
	     for nonnull where the attribute with no arguments must match.  */
	  if (!TREE_VALUE (attr))
	    return attr_nonnull ? !TREE_VALUE (atlist) : true;

	  /* Attribute nonnull with no arguments subsumes all values of
	     the attribute.  FIXME: This is overly broad since it only
	     applies to pointer arguments, but querying non-pointer
	     arguments is diagnosed.  */
	  if (!TREE_VALUE (atlist) && attr_nonnull)
	    return true;

	  /* Iterate over the DECL or TYPE attribute argument's values.  */
	  for (tree val = TREE_VALUE (atlist); val; val = TREE_CHAIN (val))
	    {
	      /* Iterate over the arguments in the sought attribute comparing
		 their values to those specified for the DECL or TYPE.  */
	      for (tree arg = TREE_VALUE (attr); arg; arg = TREE_CHAIN (arg))
		{
		  tree v1 = TREE_VALUE (val);
		  tree v2 = TREE_VALUE (arg);
		  if (v1 == v2)
		    return true;

		  if (!v1 || !v2)
		    break;

		  if (TREE_CODE (v1) == IDENTIFIER_NODE
		      || TREE_CODE (v2) == IDENTIFIER_NODE)
		    /* Two identifiers are the same if their values are
		       equal (that's handled above).  Otherwise ther are
		       either not the same or oneis not an identifier.  */
		    return false;

		  /* Convert to make them equality-comparable.  */
		  v1 = convert (v1);
		  v2 = convert (v2);

		  /* A positive value indicates equality, negative means
		     "don't know."  */
		  if (simple_cst_equal (v1, v2) == 1)
		    return true;

		  if (!ignore_mismatches)
		    break;
		}
	    }
	}

      if (!found_attr)
	{
	  /* Some attributes are encoded directly in the tree node.  */
	  if (!strcmp ("aligned", namestr))
	    {
	      if (tree arg = TREE_VALUE (attr))
		{
		  arg = convert (TREE_VALUE (arg));
		  if (!tree_fits_uhwi_p (arg))
		    /* Invalid argument.  */;
		  else if (expr && DECL_P (expr)
			   && DECL_USER_ALIGN (expr))
		    found_match = DECL_ALIGN_UNIT (expr) == tree_to_uhwi (arg);
		  else if (type && TYPE_USER_ALIGN (type))
		    found_match = TYPE_ALIGN_UNIT (type) == tree_to_uhwi (arg);
		}
	      else if (expr && DECL_P (expr))
		found_match = DECL_USER_ALIGN (expr);
	      else if (type)
		found_match = TYPE_USER_ALIGN (type);
	    }
	  else if (!strcmp ("const", namestr))
	    {
	      if (expr && DECL_P (expr))
		found_match = TREE_READONLY (expr);
	    }
	  else if (!strcmp ("noreturn", namestr))
	    {
	      /* C11 _Noreturn sets the volatile bit without attaching
		 an attribute to the decl.  */
	      if (expr
		  && DECL_P (expr)
		  && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (expr)))
		found_match = TREE_THIS_VOLATILE (expr);
	    }
	  else if (!strcmp ("pure", namestr))
	    {
	      if (expr && DECL_P (expr))
		found_match = DECL_PURE_P (expr);
	    }
	  else if (!strcmp ("deprecated", namestr))
	    {
	      found_match = TREE_DEPRECATED (expr ? expr : type);
	      if (found_match)
		return true;
	    }
	  else if (!strcmp ("vector_size", namestr))
	    {
	      if (!type || !VECTOR_TYPE_P (type))
		return false;

	      if (tree arg = TREE_VALUE (attr))
		{
		  /* Compare the vector size argument for equality.  */
		  arg = convert (TREE_VALUE (arg));
		  return tree_int_cst_equal (arg, TYPE_SIZE_UNIT (type)) == 1;
		}
	      else
		return true;
	    }
	  else if (!strcmp ("warn_if_not_aligned", namestr))
	    {
	      if (tree arg = TREE_VALUE (attr))
		{
		  arg = convert (TREE_VALUE (arg));
		  if (expr && DECL_P (expr))
		    found_match = (DECL_WARN_IF_NOT_ALIGN (expr)
				   == tree_to_uhwi (arg) * BITS_PER_UNIT);
		  else if (type)
		    found_match = (TYPE_WARN_IF_NOT_ALIGN (type)
				   == tree_to_uhwi (arg) * BITS_PER_UNIT);
		}
	      else if (expr && DECL_P (expr))
		found_match = DECL_WARN_IF_NOT_ALIGN (expr);
	      else if (type)
		found_match = TYPE_WARN_IF_NOT_ALIGN (type);
	    }
	  else if (!strcmp ("transparent_union", namestr))
	    {
	      if (type)
		found_match = TYPE_TRANSPARENT_AGGR (type) != 0;
	    }
	  else if (!strcmp ("mode", namestr))
	    {
	      /* Finally issue a warning for attributes that cannot
		 be supported in this context.  Attribute mode is not
		 added to a symbol and cannot be determined from it.  */
	      warning_at (atloc, OPT_Wattributes,
			  "%qs attribute not supported in "
			  "%<__builtin_has_attribute%>", namestr);
	      break;
	    }
	}
    }
  return found_match;
}
