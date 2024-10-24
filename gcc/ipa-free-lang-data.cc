/* Pass to free or clear language-specific data structures from
   the IL before they reach the middle end.

   Copyright (C) 1987-2024 Free Software Foundation, Inc.

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

/* This file contains the low level primitives for operating on tree nodes,
   including allocation, list operations, interning of identifiers,
   construction of data type nodes and statement nodes,
   and construction of type conversion nodes.  It also contains
   tables index by tree code that describe how to take apart
   nodes of that code.

   It is intended to be language-independent but can occasionally
   calls language-dependent routines.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "alias.h"
#include "attribs.h"
#include "langhooks.h"
#include "gimple-iterator.h"
#include "langhooks-def.h"
#include "tree-diagnostic.h"
#include "except.h"
#include "ipa-utils.h"

namespace {

/* Data used when collecting DECLs and TYPEs for language data removal.  */

class free_lang_data_d
{
public:
  free_lang_data_d () : decls (100), types (100) {}

  /* Worklist to avoid excessive recursion.  */
  auto_vec<tree> worklist;

  /* Set of traversed objects.  Used to avoid duplicate visits.  */
  hash_set<tree> pset;

  /* Array of symbols to process with free_lang_data_in_decl.  */
  auto_vec<tree> decls;

  /* Array of types to process with free_lang_data_in_type.  */
  auto_vec<tree> types;
};


/* Add type or decl T to one of the list of tree nodes that need their
   language data removed.  The lists are held inside FLD.  */

static void
add_tree_to_fld_list (tree t, class free_lang_data_d *fld)
{
  if (DECL_P (t))
    fld->decls.safe_push (t);
  else if (TYPE_P (t))
    fld->types.safe_push (t);
  else
    gcc_unreachable ();
}

/* Push tree node T into FLD->WORKLIST.  */

static inline void
fld_worklist_push (tree t, class free_lang_data_d *fld)
{
  if (t && !is_lang_specific (t) && !fld->pset.contains (t))
    fld->worklist.safe_push ((t));
}



/* Return simplified TYPE_NAME of TYPE.  */

static tree
fld_simplified_type_name (tree type)
{
  if (!TYPE_NAME (type) || TREE_CODE (TYPE_NAME (type)) != TYPE_DECL)
    return TYPE_NAME (type);
  /* Drop TYPE_DECLs in TYPE_NAME in favor of the identifier in the
     TYPE_DECL if the type doesn't have linkage.
     this must match fld_  */
  if (type != TYPE_MAIN_VARIANT (type)
      || (!DECL_ASSEMBLER_NAME_SET_P (TYPE_NAME (type))
	  && (TREE_CODE (type) != RECORD_TYPE
	      || !TYPE_BINFO (type)
	      || !BINFO_VTABLE (TYPE_BINFO (type)))))
    return DECL_NAME (TYPE_NAME (type));
  return TYPE_NAME (type);
}

/* Do same comparsion as check_qualified_type skipping lang part of type
   and be more permissive about type names: we only care that names are
   same (for diagnostics) and that ODR names are the same.
   If INNER_TYPE is non-NULL, be sure that TREE_TYPE match it.  */

static bool
fld_type_variant_equal_p (tree t, tree v, tree inner_type)
{
  if (TYPE_QUALS (t) != TYPE_QUALS (v)
      /* We want to match incomplete variants with complete types.
	 In this case we need to ignore alignment.   */
      || ((!RECORD_OR_UNION_TYPE_P (t) || COMPLETE_TYPE_P (v))
	  && (TYPE_ALIGN (t) != TYPE_ALIGN (v)
	      || TYPE_USER_ALIGN (t) != TYPE_USER_ALIGN (v)))
      || fld_simplified_type_name (t) != fld_simplified_type_name (v)
      || !attribute_list_equal (TYPE_ATTRIBUTES (t),
			        TYPE_ATTRIBUTES (v))
      || (inner_type && TREE_TYPE (v) != inner_type))
    return false;

  return true;
}

/* Find variant of FIRST that match T and create new one if necessary.
   Set TREE_TYPE to INNER_TYPE if non-NULL.  */

static tree
fld_type_variant (tree first, tree t, class free_lang_data_d *fld,
		  tree inner_type = NULL)
{
  if (first == TYPE_MAIN_VARIANT (t))
    return t;
  for (tree v = first; v; v = TYPE_NEXT_VARIANT (v))
    if (fld_type_variant_equal_p (t, v, inner_type))
      return v;
  tree v = build_variant_type_copy (first);
  TYPE_READONLY (v) = TYPE_READONLY (t);
  TYPE_VOLATILE (v) = TYPE_VOLATILE (t);
  TYPE_ATOMIC (v) = TYPE_ATOMIC (t);
  TYPE_RESTRICT (v) = TYPE_RESTRICT (t);
  TYPE_ADDR_SPACE (v) = TYPE_ADDR_SPACE (t);
  TYPE_NAME (v) = TYPE_NAME (t);
  TYPE_ATTRIBUTES (v) = TYPE_ATTRIBUTES (t);
  TYPE_CANONICAL (v) = TYPE_CANONICAL (t);
  /* Variants of incomplete types should have alignment
     set to BITS_PER_UNIT.  Do not copy the actual alignment.  */
  if (!RECORD_OR_UNION_TYPE_P (v) || COMPLETE_TYPE_P (v))
    {
      SET_TYPE_ALIGN (v, TYPE_ALIGN (t));
      TYPE_USER_ALIGN (v) = TYPE_USER_ALIGN (t);
    }
  if (inner_type)
    TREE_TYPE (v) = inner_type;
  gcc_checking_assert (fld_type_variant_equal_p (t,v, inner_type));
  if (!fld->pset.add (v))
    add_tree_to_fld_list (v, fld);
  return v;
}

/* Map complete types to incomplete types.  */

static hash_map<tree, tree> *fld_incomplete_types;

/* Map types to simplified types.  */

static hash_map<tree, tree> *fld_simplified_types;

/* Produce variant of T whose TREE_TYPE is T2. If it is main variant,
   use MAP to prevent duplicates.  */

static tree
fld_process_array_type (tree t, tree t2, hash_map<tree, tree> *map,
			class free_lang_data_d *fld)
{
  if (TREE_TYPE (t) == t2)
    return t;

  if (TYPE_MAIN_VARIANT (t) != t)
    {
      return fld_type_variant
	(fld_process_array_type (TYPE_MAIN_VARIANT (t),
				 TYPE_MAIN_VARIANT (t2), map, fld),
	 t, fld, t2);
    }

  bool existed;
  tree &array
    = map->get_or_insert (t, &existed);
  if (!existed)
    {
      array
	= build_array_type_1 (t2, TYPE_DOMAIN (t), TYPE_TYPELESS_STORAGE (t),
			      false, false);
      TYPE_CANONICAL (array) = TYPE_CANONICAL (t);
      if (!fld->pset.add (array))
	add_tree_to_fld_list (array, fld);
    }
  return array;
}

/* Return CTX after removal of contexts that are not relevant  */

static tree
fld_decl_context (tree ctx)
{
  /* Variably modified types are needed for tree_is_indexable to decide
     whether the type needs to go to local or global section.
     This code is semi-broken but for now it is easiest to keep contexts
     as expected.  */
  if (ctx && TYPE_P (ctx)
      && !variably_modified_type_p (ctx, NULL_TREE))
    {
      while (ctx && TYPE_P (ctx))
	ctx = TYPE_CONTEXT (ctx);
    }
  return ctx;
}

/* For T being aggregate type try to turn it into an incomplete variant.
   Return T if no simplification is possible.  */

static tree
fld_incomplete_type_of (tree t, class free_lang_data_d *fld)
{
  if (!t)
    return NULL;
  if (POINTER_TYPE_P (t))
    {
      tree t2 = fld_incomplete_type_of (TREE_TYPE (t), fld);
      if (t2 != TREE_TYPE (t))
	{
	  tree first;
	  if (TREE_CODE (t) == POINTER_TYPE)
	    first = build_pointer_type_for_mode (t2, TYPE_MODE (t),
						 TYPE_REF_CAN_ALIAS_ALL (t));
	  else
	    first = build_reference_type_for_mode (t2, TYPE_MODE (t),
						   TYPE_REF_CAN_ALIAS_ALL (t));
	  gcc_assert (TYPE_CANONICAL (t2) != t2
		      && TYPE_CANONICAL (t2) == TYPE_CANONICAL (TREE_TYPE (t)));
	  if (!fld->pset.add (first))
	    add_tree_to_fld_list (first, fld);
	  return fld_type_variant (first, t, fld);
	}
      return t;
    }
  if (TREE_CODE (t) == ARRAY_TYPE)
    return fld_process_array_type (t,
				   fld_incomplete_type_of (TREE_TYPE (t), fld),
				   fld_incomplete_types, fld);
  if ((!RECORD_OR_UNION_TYPE_P (t) && TREE_CODE (t) != ENUMERAL_TYPE)
      || !COMPLETE_TYPE_P (t))
    return t;
  if (TYPE_MAIN_VARIANT (t) == t)
    {
      bool existed;
      tree &copy
	= fld_incomplete_types->get_or_insert (t, &existed);

      if (!existed)
	{
	  copy = build_distinct_type_copy (t);

	  /* It is possible that type was not seen by free_lang_data yet.  */
	  if (!fld->pset.add (copy))
	    add_tree_to_fld_list (copy, fld);
	  TYPE_SIZE (copy) = NULL;
	  TYPE_USER_ALIGN (copy) = 0;
	  TYPE_SIZE_UNIT (copy) = NULL;
	  TYPE_CANONICAL (copy) = TYPE_CANONICAL (t);
	  TREE_ADDRESSABLE (copy) = 0;
	  if (AGGREGATE_TYPE_P (t))
	    {
	      SET_TYPE_MODE (copy, VOIDmode);
	      SET_TYPE_ALIGN (copy, BITS_PER_UNIT);
	      TYPE_TYPELESS_STORAGE (copy) = 0;
	      TYPE_FIELDS (copy) = NULL;
	      TYPE_BINFO (copy) = NULL;
	      TYPE_FINAL_P (copy) = 0;
	      TYPE_EMPTY_P (copy) = 0;
	    }
	  else
	    {
	      TYPE_VALUES (copy) = NULL;
	      ENUM_IS_OPAQUE (copy) = 0;
	      ENUM_IS_SCOPED (copy) = 0;
	    }

	  /* Build copy of TYPE_DECL in TYPE_NAME if necessary.
	     This is needed for ODR violation warnings to come out right (we
	     want duplicate TYPE_DECLs whenever the type is duplicated because
	     of ODR violation.  Because lang data in the TYPE_DECL may not
	     have been freed yet, rebuild it from scratch and copy relevant
	     fields.  */
	  TYPE_NAME (copy) = fld_simplified_type_name (copy);
	  tree name = TYPE_NAME (copy);

	  if (name && TREE_CODE (name) == TYPE_DECL)
	    {
	      gcc_checking_assert (TREE_TYPE (name) == t);
	      tree name2 = build_decl (DECL_SOURCE_LOCATION (name), TYPE_DECL,
				       DECL_NAME (name), copy);
	      if (DECL_ASSEMBLER_NAME_SET_P (name))
		SET_DECL_ASSEMBLER_NAME (name2, DECL_ASSEMBLER_NAME (name));
	      SET_DECL_ALIGN (name2, 0);
	      DECL_CONTEXT (name2) = fld_decl_context
		(DECL_CONTEXT (name));
	      TYPE_NAME (copy) = name2;
	    }
	}
      return copy;
    }
  return (fld_type_variant
	  (fld_incomplete_type_of (TYPE_MAIN_VARIANT (t), fld), t, fld));
}

/* Simplify type T for scenarios where we do not need complete pointer
   types.  */

static tree
fld_simplified_type (tree t, class free_lang_data_d *fld)
{
  if (!t)
    return t;
  if (POINTER_TYPE_P (t))
    return fld_incomplete_type_of (t, fld);
  /* FIXME: This triggers verification error, see PR88140.  */
#if 0
  if (TREE_CODE (t) == ARRAY_TYPE)
    return fld_process_array_type (t, fld_simplified_type (TREE_TYPE (t), fld),
				   fld_simplified_types, fld);
#endif
  return t;
}

/* Reset the expression *EXPR_P, a size or position.

   ??? We could reset all non-constant sizes or positions.  But it's cheap
   enough to not do so and refrain from adding workarounds to dwarf2out.cc.

   We need to reset self-referential sizes or positions because they cannot
   be gimplified and thus can contain a CALL_EXPR after the gimplification
   is finished, which will run afoul of LTO streaming.  And they need to be
   reset to something essentially dummy but not constant, so as to preserve
   the properties of the object they are attached to.  */

static inline void
free_lang_data_in_one_sizepos (tree *expr_p)
{
  tree expr = *expr_p;
  if (CONTAINS_PLACEHOLDER_P (expr))
    *expr_p = build0 (PLACEHOLDER_EXPR, TREE_TYPE (expr));
}


/* Reset all the fields in a binfo node BINFO.  We only keep
   BINFO_VTABLE, which is used by gimple_fold_obj_type_ref.  */

static void
free_lang_data_in_binfo (tree binfo)
{
  unsigned i;
  tree t;

  gcc_assert (TREE_CODE (binfo) == TREE_BINFO);

  BINFO_VIRTUALS (binfo) = NULL_TREE;
  BINFO_BASE_ACCESSES (binfo) = NULL;
  BINFO_INHERITANCE_CHAIN (binfo) = NULL_TREE;
  BINFO_SUBVTT_INDEX (binfo) = NULL_TREE;
  BINFO_VPTR_FIELD (binfo) = NULL_TREE;
  TREE_PUBLIC (binfo) = 0;

  FOR_EACH_VEC_ELT (*BINFO_BASE_BINFOS (binfo), i, t)
    free_lang_data_in_binfo (t);
}


/* Reset all language specific information still present in TYPE.  */

static void
free_lang_data_in_type (tree type, class free_lang_data_d *fld)
{
  gcc_assert (TYPE_P (type));

  /* Give the FE a chance to remove its own data first.  */
  lang_hooks.free_lang_data (type);

  TREE_LANG_FLAG_0 (type) = 0;
  TREE_LANG_FLAG_1 (type) = 0;
  TREE_LANG_FLAG_2 (type) = 0;
  TREE_LANG_FLAG_3 (type) = 0;
  TREE_LANG_FLAG_4 (type) = 0;
  TREE_LANG_FLAG_5 (type) = 0;
  TREE_LANG_FLAG_6 (type) = 0;

  TYPE_NEEDS_CONSTRUCTING (type) = 0;

  /* Purge non-marked variants from the variants chain, so that they
     don't reappear in the IL after free_lang_data.  */
  while (TYPE_NEXT_VARIANT (type)
	 && !fld->pset.contains (TYPE_NEXT_VARIANT (type)))
    {
      tree t = TYPE_NEXT_VARIANT (type);
      TYPE_NEXT_VARIANT (type) = TYPE_NEXT_VARIANT (t);
      /* Turn the removed types into distinct types.  */
      TYPE_MAIN_VARIANT (t) = t;
      TYPE_NEXT_VARIANT (t) = NULL_TREE;
    }

  if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      TREE_TYPE (type) = fld_simplified_type (TREE_TYPE (type), fld);
      /* Remove the const and volatile qualifiers from arguments.  The
	 C++ front end removes them, but the C front end does not,
	 leading to false ODR violation errors when merging two
	 instances of the same function signature compiled by
	 different front ends.  */
      for (tree p = TYPE_ARG_TYPES (type); p; p = TREE_CHAIN (p))
	{
	  TREE_VALUE (p) = fld_simplified_type (TREE_VALUE (p), fld);
	  tree arg_type = TREE_VALUE (p);

	  if (TYPE_READONLY (arg_type) || TYPE_VOLATILE (arg_type))
	    {
	      int quals = TYPE_QUALS (arg_type)
		& ~TYPE_QUAL_CONST
		& ~TYPE_QUAL_VOLATILE;
	      TREE_VALUE (p) = build_qualified_type (arg_type, quals);
	      if (!fld->pset.add (TREE_VALUE (p)))
		free_lang_data_in_type (TREE_VALUE (p), fld);
	    }
	  /* C++ FE uses TREE_PURPOSE to store initial values.  */
	  TREE_PURPOSE (p) = NULL;
	}
    }
  else if (TREE_CODE (type) == METHOD_TYPE)
    {
      TREE_TYPE (type) = fld_simplified_type (TREE_TYPE (type), fld);
      for (tree p = TYPE_ARG_TYPES (type); p; p = TREE_CHAIN (p))
	{
	  /* C++ FE uses TREE_PURPOSE to store initial values.  */
	  TREE_VALUE (p) = fld_simplified_type (TREE_VALUE (p), fld);
	  TREE_PURPOSE (p) = NULL;
	}
    }
  else if (RECORD_OR_UNION_TYPE_P (type))
    {
      /* Remove members that are not FIELD_DECLs from the field list
	 of an aggregate.  These occur in C++.  */
      for (tree *prev = &TYPE_FIELDS (type), member; (member = *prev);)
	if (TREE_CODE (member) == FIELD_DECL)
	  prev = &DECL_CHAIN (member);
	else
	  *prev = DECL_CHAIN (member);

      TYPE_VFIELD (type) = NULL_TREE;

      if (TYPE_BINFO (type))
	{
	  free_lang_data_in_binfo (TYPE_BINFO (type));
	  /* We need to preserve link to bases and virtual table for all
	     polymorphic types to make devirtualization machinery working.  */
	  if (!BINFO_VTABLE (TYPE_BINFO (type)))
	    TYPE_BINFO (type) = NULL;
	}
    }
  else if (INTEGRAL_TYPE_P (type)
	   || SCALAR_FLOAT_TYPE_P (type)
	   || FIXED_POINT_TYPE_P (type))
    {
      if (TREE_CODE (type) == ENUMERAL_TYPE)
	{
	  ENUM_IS_OPAQUE (type) = 0;
	  ENUM_IS_SCOPED (type) = 0;
	  /* Type values are used only for C++ ODR checking.  Drop them
	     for all type variants and non-ODR types.
	     For ODR types the data is freed in free_odr_warning_data.  */
	  if (!TYPE_VALUES (type))
	    ;
	  else if (TYPE_MAIN_VARIANT (type) != type
		   || !type_with_linkage_p (type)
		   || type_in_anonymous_namespace_p (type))
	    TYPE_VALUES (type) = NULL;
	  else
	    register_odr_enum (type);
	}
      free_lang_data_in_one_sizepos (&TYPE_MIN_VALUE (type));
      free_lang_data_in_one_sizepos (&TYPE_MAX_VALUE (type));
    }

  TYPE_LANG_SLOT_1 (type) = NULL_TREE;

  free_lang_data_in_one_sizepos (&TYPE_SIZE (type));
  free_lang_data_in_one_sizepos (&TYPE_SIZE_UNIT (type));

  if (TYPE_CONTEXT (type)
      && TREE_CODE (TYPE_CONTEXT (type)) == BLOCK)
    {
      tree ctx = TYPE_CONTEXT (type);
      do
	{
	  ctx = BLOCK_SUPERCONTEXT (ctx);
	}
      while (ctx && TREE_CODE (ctx) == BLOCK);
      TYPE_CONTEXT (type) = ctx;
    }

  TYPE_STUB_DECL (type) = NULL;
  TYPE_NAME (type) = fld_simplified_type_name (type);
}

/* Reset all language specific information still present in symbol
   DECL.  */

static void
free_lang_data_in_decl (tree decl, class free_lang_data_d *fld)
{
  gcc_assert (DECL_P (decl));

  /* Give the FE a chance to remove its own data first.  */
  lang_hooks.free_lang_data (decl);

  TREE_LANG_FLAG_0 (decl) = 0;
  TREE_LANG_FLAG_1 (decl) = 0;
  TREE_LANG_FLAG_2 (decl) = 0;
  TREE_LANG_FLAG_3 (decl) = 0;
  TREE_LANG_FLAG_4 (decl) = 0;
  TREE_LANG_FLAG_5 (decl) = 0;
  TREE_LANG_FLAG_6 (decl) = 0;

  free_lang_data_in_one_sizepos (&DECL_SIZE (decl));
  free_lang_data_in_one_sizepos (&DECL_SIZE_UNIT (decl));
  if (TREE_CODE (decl) == FIELD_DECL)
    {
      DECL_FCONTEXT (decl) = NULL;
      free_lang_data_in_one_sizepos (&DECL_FIELD_OFFSET (decl));
      if (TREE_CODE (DECL_CONTEXT (decl)) == QUAL_UNION_TYPE)
	DECL_QUALIFIER (decl) = NULL_TREE;
    }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      struct cgraph_node *node;
      /* Frontends do not set TREE_ADDRESSABLE on public variables even though
	 the address may be taken in other unit, so this flag has no practical
	 use for middle-end.

	 It would make more sense if frontends set TREE_ADDRESSABLE to 0 only
	 for public objects that indeed cannot be adressed, but it is not
	 the case.  Set the flag to true so we do not get merge failures for
	 i.e. virtual tables between units that take address of it and
	 units that don't.  */
      if (TREE_PUBLIC (decl))
	TREE_ADDRESSABLE (decl) = true;
      TREE_TYPE (decl) = fld_simplified_type (TREE_TYPE (decl), fld);
      if (!(node = cgraph_node::get (decl))
	  || (!node->definition && !node->clones))
	{
	  if (node && !node->declare_variant_alt)
	    node->release_body ();
	  else
	    {
	      release_function_body (decl);
	      DECL_ARGUMENTS (decl) = NULL;
	      DECL_RESULT (decl) = NULL;
	      DECL_INITIAL (decl) = error_mark_node;
	    }
	}
      if (gimple_has_body_p (decl) || (node && node->thunk))
	{
	  tree t;

	  /* If DECL has a gimple body, then the context for its
	     arguments must be DECL.  Otherwise, it doesn't really
	     matter, as we will not be emitting any code for DECL.  In
	     general, there may be other instances of DECL created by
	     the front end and since PARM_DECLs are generally shared,
	     their DECL_CONTEXT changes as the replicas of DECL are
	     created.  The only time where DECL_CONTEXT is important
	     is for the FUNCTION_DECLs that have a gimple body (since
	     the PARM_DECL will be used in the function's body).  */
	  for (t = DECL_ARGUMENTS (decl); t; t = TREE_CHAIN (t))
	    DECL_CONTEXT (t) = decl;
	  if (!DECL_FUNCTION_SPECIFIC_TARGET (decl))
	    DECL_FUNCTION_SPECIFIC_TARGET (decl)
	      = target_option_default_node;
	  if (!DECL_FUNCTION_SPECIFIC_OPTIMIZATION (decl))
	    DECL_FUNCTION_SPECIFIC_OPTIMIZATION (decl)
	      = optimization_default_node;
	}

      /* DECL_SAVED_TREE holds the GENERIC representation for DECL.
	 At this point, it is not needed anymore.  */
      DECL_SAVED_TREE (decl) = NULL_TREE;

      /* Clear the abstract origin if it refers to a method.
         Otherwise dwarf2out.cc will ICE as we splice functions out of
         TYPE_FIELDS and thus the origin will not be output
         correctly.  */
      if (DECL_ABSTRACT_ORIGIN (decl)
	  && DECL_CONTEXT (DECL_ABSTRACT_ORIGIN (decl))
	  && RECORD_OR_UNION_TYPE_P
	  (DECL_CONTEXT (DECL_ABSTRACT_ORIGIN (decl))))
	DECL_ABSTRACT_ORIGIN (decl) = NULL_TREE;

      DECL_VINDEX (decl) = NULL_TREE;
    }
  else if (VAR_P (decl))
    {
      /* See comment above why we set the flag for functions.  */
      if (TREE_PUBLIC (decl))
	TREE_ADDRESSABLE (decl) = true;
      if ((DECL_EXTERNAL (decl)
	   && (!TREE_STATIC (decl) || !TREE_READONLY (decl)))
	  || (decl_function_context (decl) && !TREE_STATIC (decl)))
	DECL_INITIAL (decl) = NULL_TREE;
    }
  else if (TREE_CODE (decl) == TYPE_DECL)
    {
      DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
      DECL_VISIBILITY_SPECIFIED (decl) = 0;
      TREE_PUBLIC (decl) = 0;
      TREE_PRIVATE (decl) = 0;
      DECL_ARTIFICIAL (decl) = 0;
      TYPE_DECL_SUPPRESS_DEBUG (decl) = 0;
      DECL_INITIAL (decl) = NULL_TREE;
      DECL_ORIGINAL_TYPE (decl) = NULL_TREE;
      DECL_MODE (decl) = VOIDmode;
      SET_DECL_ALIGN (decl, 0);
      /* TREE_TYPE is cleared at WPA time in free_odr_warning_data.  */
    }
  else if (TREE_CODE (decl) == FIELD_DECL)
    {
      TREE_TYPE (decl) = fld_simplified_type (TREE_TYPE (decl), fld);
      DECL_INITIAL (decl) = NULL_TREE;
    }
  else if (TREE_CODE (decl) == TRANSLATION_UNIT_DECL
           && DECL_INITIAL (decl)
           && TREE_CODE (DECL_INITIAL (decl)) == BLOCK)
    {
      /* Strip builtins from the translation-unit BLOCK.  We still have targets
	 without builtin_decl_explicit support and also builtins are shared
	 nodes and thus we can't use TREE_CHAIN in multiple lists.  */
      tree *nextp = &BLOCK_VARS (DECL_INITIAL (decl));
      while (*nextp)
	{
	  tree var = *nextp;
	  if (TREE_CODE (var) == FUNCTION_DECL
	      && fndecl_built_in_p (var))
	    *nextp = TREE_CHAIN (var);
	  else
	    nextp = &TREE_CHAIN (var);
        }
    }
  /* We need to keep field decls associated with their trees. Otherwise tree
     merging may merge some fields and keep others disjoint which in turn will
     not do well with TREE_CHAIN pointers linking them.

     Also do not drop containing types for virtual methods and tables because
     these are needed by devirtualization.
     C++ destructors are special because C++ frontends sometimes produces
     virtual destructor as an alias of non-virtual destructor.  In
     devirutalization code we always walk through aliases and we need
     context to be preserved too.  See PR89335  */
  if (TREE_CODE (decl) != FIELD_DECL
      && ((TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != FUNCTION_DECL)
          || (!DECL_VIRTUAL_P (decl)
	      && (TREE_CODE (decl) != FUNCTION_DECL
		  || !DECL_CXX_DESTRUCTOR_P (decl)))))
    DECL_CONTEXT (decl) = fld_decl_context (DECL_CONTEXT (decl));
}


/* Operand callback helper for free_lang_data_in_node.  *TP is the
   subtree operand being considered.  */

static tree
find_decls_types_r (tree *tp, int *ws, void *data)
{
  tree t = *tp;
  class free_lang_data_d *fld = (class free_lang_data_d *) data;

  if (TREE_CODE (t) == TREE_LIST)
    return NULL_TREE;

  /* Language specific nodes will be removed, so there is no need
     to gather anything under them.  */
  if (is_lang_specific (t))
    {
      *ws = 0;
      return NULL_TREE;
    }

  if (DECL_P (t))
    {
      /* Note that walk_tree does not traverse every possible field in
	 decls, so we have to do our own traversals here.  */
      add_tree_to_fld_list (t, fld);

      fld_worklist_push (DECL_NAME (t), fld);
      fld_worklist_push (DECL_CONTEXT (t), fld);
      fld_worklist_push (DECL_SIZE (t), fld);
      fld_worklist_push (DECL_SIZE_UNIT (t), fld);

      /* We are going to remove everything under DECL_INITIAL for
	 TYPE_DECLs.  No point walking them.  */
      if (TREE_CODE (t) != TYPE_DECL)
	fld_worklist_push (DECL_INITIAL (t), fld);

      fld_worklist_push (DECL_ATTRIBUTES (t), fld);
      fld_worklist_push (DECL_ABSTRACT_ORIGIN (t), fld);

      if (TREE_CODE (t) == FUNCTION_DECL)
	{
	  fld_worklist_push (DECL_ARGUMENTS (t), fld);
	  fld_worklist_push (DECL_RESULT (t), fld);
	}
      else if (TREE_CODE (t) == FIELD_DECL)
	{
	  fld_worklist_push (DECL_FIELD_OFFSET (t), fld);
	  fld_worklist_push (DECL_BIT_FIELD_TYPE (t), fld);
	  fld_worklist_push (DECL_FIELD_BIT_OFFSET (t), fld);
	  fld_worklist_push (DECL_FCONTEXT (t), fld);
	}

      if ((VAR_P (t) || TREE_CODE (t) == PARM_DECL)
	  && DECL_HAS_VALUE_EXPR_P (t))
	fld_worklist_push (DECL_VALUE_EXPR (t), fld);

      if (TREE_CODE (t) != FIELD_DECL
	  && TREE_CODE (t) != TYPE_DECL)
	fld_worklist_push (TREE_CHAIN (t), fld);
      *ws = 0;
    }
  else if (TYPE_P (t))
    {
      /* Note that walk_tree does not traverse every possible field in
	 types, so we have to do our own traversals here.  */
      add_tree_to_fld_list (t, fld);

      if (!RECORD_OR_UNION_TYPE_P (t))
	fld_worklist_push (TYPE_CACHED_VALUES (t), fld);
      fld_worklist_push (TYPE_SIZE (t), fld);
      fld_worklist_push (TYPE_SIZE_UNIT (t), fld);
      fld_worklist_push (TYPE_ATTRIBUTES (t), fld);
      fld_worklist_push (TYPE_POINTER_TO (t), fld);
      fld_worklist_push (TYPE_REFERENCE_TO (t), fld);
      fld_worklist_push (TYPE_NAME (t), fld);
      /* While we do not stream TYPE_POINTER_TO and TYPE_REFERENCE_TO
	 lists, we may look types up in these lists and use them while
	 optimizing the function body.  Thus we need to free lang data
	 in them.  */
      if (TREE_CODE (t) == POINTER_TYPE)
	fld_worklist_push (TYPE_NEXT_PTR_TO (t), fld);
      if (TREE_CODE (t) == REFERENCE_TYPE)
	fld_worklist_push (TYPE_NEXT_REF_TO (t), fld);
      if (!POINTER_TYPE_P (t))
	fld_worklist_push (TYPE_MIN_VALUE_RAW (t), fld);
      /* TYPE_MAX_VALUE_RAW is TYPE_BINFO for record types.  */
      if (!RECORD_OR_UNION_TYPE_P (t))
	fld_worklist_push (TYPE_MAX_VALUE_RAW (t), fld);
      fld_worklist_push (TYPE_MAIN_VARIANT (t), fld);
      /* Do not walk TYPE_NEXT_VARIANT.  We do not stream it and thus
	 do not and want not to reach unused variants this way.  */
      if (TYPE_CONTEXT (t))
	{
	  tree ctx = TYPE_CONTEXT (t);
	  /* We adjust BLOCK TYPE_CONTEXTs to the innermost non-BLOCK one.
	     So push that instead.  */
	  while (ctx && TREE_CODE (ctx) == BLOCK)
	    ctx = BLOCK_SUPERCONTEXT (ctx);
	  fld_worklist_push (ctx, fld);
	}
      fld_worklist_push (TYPE_CANONICAL (t), fld);

      if (RECORD_OR_UNION_TYPE_P (t) && TYPE_BINFO (t))
	{
	  unsigned i;
	  tree tem;
	  FOR_EACH_VEC_ELT (*BINFO_BASE_BINFOS (TYPE_BINFO (t)), i, tem)
	    fld_worklist_push (TREE_TYPE (tem), fld);
	  fld_worklist_push (BINFO_TYPE (TYPE_BINFO (t)), fld);
	  fld_worklist_push (BINFO_VTABLE (TYPE_BINFO (t)), fld);
	}
      if (RECORD_OR_UNION_TYPE_P (t))
	{
	  tree tem;
	  /* Push all TYPE_FIELDS - there can be interleaving interesting
	     and non-interesting things.  */
	  tem = TYPE_FIELDS (t);
	  while (tem)
	    {
	      if (TREE_CODE (tem) == FIELD_DECL)
		fld_worklist_push (tem, fld);
	      tem = TREE_CHAIN (tem);
	    }
	}
      if (FUNC_OR_METHOD_TYPE_P (t))
	fld_worklist_push (TYPE_METHOD_BASETYPE (t), fld);

      fld_worklist_push (TYPE_STUB_DECL (t), fld);
      *ws = 0;
    }
  else if (TREE_CODE (t) == BLOCK)
    {
      for (tree *tem = &BLOCK_VARS (t); *tem; )
	{
	  if (TREE_CODE (*tem) != LABEL_DECL
	      && (TREE_CODE (*tem) != VAR_DECL
		  || !auto_var_in_fn_p (*tem, DECL_CONTEXT (*tem))))
	    {
	      gcc_assert (TREE_CODE (*tem) != RESULT_DECL
			  && TREE_CODE (*tem) != PARM_DECL);
	      *tem = TREE_CHAIN (*tem);
	    }
	  else
	    {
	      fld_worklist_push (*tem, fld);
	      tem = &TREE_CHAIN (*tem);
	    }
	}
      for (tree tem = BLOCK_SUBBLOCKS (t); tem; tem = BLOCK_CHAIN (tem))
	fld_worklist_push (tem, fld);
      fld_worklist_push (BLOCK_ABSTRACT_ORIGIN (t), fld);
    }

  if (TREE_CODE (t) != IDENTIFIER_NODE
      && CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPED))
    fld_worklist_push (TREE_TYPE (t), fld);

  return NULL_TREE;
}


/* Find decls and types in T.  */

static void
find_decls_types (tree t, class free_lang_data_d *fld)
{
  while (1)
    {
      if (!fld->pset.contains (t))
	walk_tree (&t, find_decls_types_r, fld, &fld->pset);
      if (fld->worklist.is_empty ())
	break;
      t = fld->worklist.pop ();
    }
}

/* Translate all the types in LIST with the corresponding runtime
   types.  */

static tree
get_eh_types_for_runtime (tree list)
{
  tree head, prev;

  if (list == NULL_TREE)
    return NULL_TREE;

  head = build_tree_list (0, lookup_type_for_runtime (TREE_VALUE (list)));
  prev = head;
  list = TREE_CHAIN (list);
  while (list)
    {
      tree n = build_tree_list (0, lookup_type_for_runtime (TREE_VALUE (list)));
      TREE_CHAIN (prev) = n;
      prev = TREE_CHAIN (prev);
      list = TREE_CHAIN (list);
    }

  return head;
}


/* Find decls and types referenced in EH region R and store them in
   FLD->DECLS and FLD->TYPES.  */

static void
find_decls_types_in_eh_region (eh_region r, class free_lang_data_d *fld)
{
  switch (r->type)
    {
    case ERT_CLEANUP:
      break;

    case ERT_TRY:
      {
	eh_catch c;

	/* The types referenced in each catch must first be changed to the
	   EH types used at runtime.  This removes references to FE types
	   in the region.  */
	for (c = r->u.eh_try.first_catch; c ; c = c->next_catch)
	  {
	    c->type_list = get_eh_types_for_runtime (c->type_list);
	    walk_tree (&c->type_list, find_decls_types_r, fld, &fld->pset);
	  }
      }
      break;

    case ERT_ALLOWED_EXCEPTIONS:
      r->u.allowed.type_list
	= get_eh_types_for_runtime (r->u.allowed.type_list);
      walk_tree (&r->u.allowed.type_list, find_decls_types_r, fld, &fld->pset);
      break;

    case ERT_MUST_NOT_THROW:
      walk_tree (&r->u.must_not_throw.failure_decl,
		 find_decls_types_r, fld, &fld->pset);
      break;
    }
}


/* Find decls and types referenced in cgraph node N and store them in
   FLD->DECLS and FLD->TYPES.  Unlike pass_referenced_vars, this will
   look for *every* kind of DECL and TYPE node reachable from N,
   including those embedded inside types and decls (i.e,, TYPE_DECLs,
   NAMESPACE_DECLs, etc).  */

static void
find_decls_types_in_node (struct cgraph_node *n, class free_lang_data_d *fld)
{
  basic_block bb;
  struct function *fn;
  unsigned ix;
  tree t;

  find_decls_types (n->decl, fld);

  if (!gimple_has_body_p (n->decl))
    return;

  gcc_assert (current_function_decl == NULL_TREE && cfun == NULL);

  fn = DECL_STRUCT_FUNCTION (n->decl);

  /* Traverse locals. */
  FOR_EACH_LOCAL_DECL (fn, ix, t)
    find_decls_types (t, fld);

  /* Traverse EH regions in FN.  */
  {
    eh_region r;
    FOR_ALL_EH_REGION_FN (r, fn)
      find_decls_types_in_eh_region (r, fld);
  }

  /* Traverse every statement in FN.  */
  FOR_EACH_BB_FN (bb, fn)
    {
      gphi_iterator psi;
      gimple_stmt_iterator si;
      unsigned i;

      for (psi = gsi_start_phis (bb); !gsi_end_p (psi); gsi_next (&psi))
	{
	  gphi *phi = psi.phi ();

	  for (i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      tree *arg_p = gimple_phi_arg_def_ptr (phi, i);
	      find_decls_types (*arg_p, fld);
	    }
	}

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);

	  if (is_gimple_call (stmt))
	    find_decls_types (gimple_call_fntype (stmt), fld);

	  for (i = 0; i < gimple_num_ops (stmt); i++)
	    {
	      tree arg = gimple_op (stmt, i);
	      find_decls_types (arg, fld);
	      /* find_decls_types doesn't walk TREE_PURPOSE of TREE_LISTs,
		 which we need for asm stmts.  */
	      if (arg
		  && TREE_CODE (arg) == TREE_LIST
		  && TREE_PURPOSE (arg)
		  && gimple_code (stmt) == GIMPLE_ASM)
		find_decls_types (TREE_PURPOSE (arg), fld);
	    }
	}
    }
}


/* Find decls and types referenced in varpool node N and store them in
   FLD->DECLS and FLD->TYPES.  Unlike pass_referenced_vars, this will
   look for *every* kind of DECL and TYPE node reachable from N,
   including those embedded inside types and decls (i.e,, TYPE_DECLs,
   NAMESPACE_DECLs, etc).  */

static void
find_decls_types_in_var (varpool_node *v, class free_lang_data_d *fld)
{
  find_decls_types (v->decl, fld);
}

/* Free language specific information for every operand and expression
   in every node of the call graph.  This process operates in three stages:

   1- Every callgraph node and varpool node is traversed looking for
   decls and types embedded in them.  This is a more exhaustive
   search than that done by find_referenced_vars, because it will
   also collect individual fields, decls embedded in types, etc.

   2- All the decls found are sent to free_lang_data_in_decl.

   3- All the types found are sent to free_lang_data_in_type.

   The ordering between decls and types is important because
   free_lang_data_in_decl sets assembler names, which includes
   mangling.  So types cannot be freed up until assembler names have
   been set up.  */

static void
free_lang_data_in_cgraph (class free_lang_data_d *fld)
{
  struct cgraph_node *n;
  varpool_node *v;
  tree t;
  unsigned i;
  alias_pair *p;

  /* Find decls and types in the body of every function in the callgraph.  */
  FOR_EACH_FUNCTION (n)
    find_decls_types_in_node (n, fld);

  FOR_EACH_VEC_SAFE_ELT (alias_pairs, i, p)
    find_decls_types (p->decl, fld);

  /* Find decls and types in every varpool symbol.  */
  FOR_EACH_VARIABLE (v)
    find_decls_types_in_var (v, fld);

  /* Set the assembler name on every decl found.  We need to do this
     now because free_lang_data_in_decl will invalidate data needed
     for mangling.  This breaks mangling on interdependent decls.  */
  FOR_EACH_VEC_ELT (fld->decls, i, t)
    assign_assembler_name_if_needed (t);

  /* Traverse every decl found freeing its language data.  */
  FOR_EACH_VEC_ELT (fld->decls, i, t)
    free_lang_data_in_decl (t, fld);

  /* Traverse every type found freeing its language data.  */
  FOR_EACH_VEC_ELT (fld->types, i, t)
    free_lang_data_in_type (t, fld);
}


/* Free resources that are used by FE but are not needed once they are done. */

static unsigned
free_lang_data (void)
{
  unsigned i;
  class free_lang_data_d fld;

  /* If we are the LTO frontend we have freed lang-specific data already.  */
  if (in_lto_p
      || (!flag_generate_lto && !flag_generate_offload))
    {
      /* Rebuild type inheritance graph even when not doing LTO to get
	 consistent profile data.  */
      rebuild_type_inheritance_graph ();
      return 0;
    }

  fld_incomplete_types = new hash_map<tree, tree>;
  fld_simplified_types = new hash_map<tree, tree>;

  /* Provide a dummy TRANSLATION_UNIT_DECL if the FE failed to provide one.  */
  if (vec_safe_is_empty (all_translation_units))
    build_translation_unit_decl (NULL_TREE);

  /* Allocate and assign alias sets to the standard integer types
     while the slots are still in the way the frontends generated them.  */
  for (i = 0; i < itk_none; ++i)
    if (integer_types[i])
      TYPE_ALIAS_SET (integer_types[i]) = get_alias_set (integer_types[i]);

  /* Traverse the IL resetting language specific information for
     operands, expressions, etc.  */
  free_lang_data_in_cgraph (&fld);

  /* Create gimple variants for common types.  */
  for (unsigned i = 0; i < ARRAY_SIZE (builtin_structptr_types); ++i)
    builtin_structptr_types[i].node = builtin_structptr_types[i].base;

  /* Reset some langhooks.  Do not reset types_compatible_p, it may
     still be used indirectly via the get_alias_set langhook.  */
  lang_hooks.dwarf_name = lhd_dwarf_name;
  lang_hooks.decl_printable_name = gimple_decl_printable_name;
  lang_hooks.gimplify_expr = lhd_gimplify_expr;
  lang_hooks.overwrite_decl_assembler_name = lhd_overwrite_decl_assembler_name;
  lang_hooks.print_xnode = lhd_print_tree_nothing;
  lang_hooks.print_decl = lhd_print_tree_nothing;
  lang_hooks.print_type = lhd_print_tree_nothing;
  lang_hooks.print_identifier = lhd_print_tree_nothing;

  lang_hooks.tree_inlining.var_mod_type_p = hook_bool_tree_tree_false;

  if (flag_checking)
    {
      int i;
      tree t;

      FOR_EACH_VEC_ELT (fld.types, i, t)
	verify_type (t);
    }

  /* We do not want the default decl_assembler_name implementation,
     rather if we have fixed everything we want a wrapper around it
     asserting that all non-local symbols already got their assembler
     name and only produce assembler names for local symbols.  Or rather
     make sure we never call decl_assembler_name on local symbols and
     devise a separate, middle-end private scheme for it.  */

  /* Reset diagnostic machinery.  */
  tree_diagnostics_defaults (global_dc);

  rebuild_type_inheritance_graph ();

  delete fld_incomplete_types;
  delete fld_simplified_types;

  return 0;
}

const pass_data pass_data_ipa_free_lang_data =
  {
   SIMPLE_IPA_PASS, /* type */
   "*free_lang_data", /* name */
   OPTGROUP_NONE, /* optinfo_flags */
   TV_IPA_FREE_LANG_DATA, /* tv_id */
   0, /* properties_required */
   0, /* properties_provided */
   0, /* properties_destroyed */
   0, /* todo_flags_start */
   0, /* todo_flags_finish */
  };

class pass_ipa_free_lang_data : public simple_ipa_opt_pass
{
public:
  pass_ipa_free_lang_data (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_free_lang_data, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute (function *) final override { return free_lang_data (); }

}; // class pass_ipa_free_lang_data

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_free_lang_data (gcc::context *ctxt)
{
  return new pass_ipa_free_lang_data (ctxt);
}
