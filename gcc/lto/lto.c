/* Top-level LTO routines.
   Copyright (C) 2009-2013 Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

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
#include "opts.h"
#include "toplev.h"
#include "tree.h"
#include "diagnostic-core.h"
#include "tm.h"
#include "cgraph.h"
#include "ggc.h"
#include "tree-ssa-operands.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "vec.h"
#include "bitmap.h"
#include "pointer-set.h"
#include "ipa-prop.h"
#include "common.h"
#include "debug.h"
#include "gimple.h"
#include "lto.h"
#include "lto-tree.h"
#include "lto-streamer.h"
#include "tree-streamer.h"
#include "splay-tree.h"
#include "lto-partition.h"
#include "data-streamer.h"
#include "context.h"
#include "pass_manager.h"

/* Vector to keep track of external variables we've seen so far.  */
vec<tree, va_gc> *lto_global_var_decls;

static GTY(()) tree first_personality_decl;

/* Returns a hash code for P.  */

static hashval_t
hash_name (const void *p)
{
  const struct lto_section_slot *ds = (const struct lto_section_slot *) p;
  return (hashval_t) htab_hash_string (ds->name);
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_name (const void *p1, const void *p2)
{
  const struct lto_section_slot *s1 =
    (const struct lto_section_slot *) p1;
  const struct lto_section_slot *s2 =
    (const struct lto_section_slot *) p2;

  return strcmp (s1->name, s2->name) == 0;
}

/* Free lto_section_slot */

static void
free_with_string (void *arg)
{
  struct lto_section_slot *s = (struct lto_section_slot *)arg;

  free (CONST_CAST (char *, s->name));
  free (arg);
}

/* Create section hash table */

htab_t 
lto_obj_create_section_hash_table (void)
{
  return htab_create (37, hash_name, eq_name, free_with_string);
}

/* Delete an allocated integer KEY in the splay tree.  */

static void
lto_splay_tree_delete_id (splay_tree_key key)
{
  free ((void *) key);
}

/* Compare splay tree node ids A and B.  */

static int
lto_splay_tree_compare_ids (splay_tree_key a, splay_tree_key b)
{
  unsigned HOST_WIDE_INT ai;
  unsigned HOST_WIDE_INT bi;

  ai = *(unsigned HOST_WIDE_INT *) a;
  bi = *(unsigned HOST_WIDE_INT *) b;

  if (ai < bi)
    return -1;
  else if (ai > bi)
    return 1;
  return 0;
}

/* Look up splay tree node by ID in splay tree T.  */

static splay_tree_node
lto_splay_tree_lookup (splay_tree t, unsigned HOST_WIDE_INT id)
{
  return splay_tree_lookup (t, (splay_tree_key) &id);
}

/* Check if KEY has ID.  */

static bool
lto_splay_tree_id_equal_p (splay_tree_key key, unsigned HOST_WIDE_INT id)
{
  return *(unsigned HOST_WIDE_INT *) key == id;
}

/* Insert a splay tree node into tree T with ID as key and FILE_DATA as value. 
   The ID is allocated separately because we need HOST_WIDE_INTs which may
   be wider than a splay_tree_key. */

static void
lto_splay_tree_insert (splay_tree t, unsigned HOST_WIDE_INT id,
		       struct lto_file_decl_data *file_data)
{
  unsigned HOST_WIDE_INT *idp = XCNEW (unsigned HOST_WIDE_INT);
  *idp = id;
  splay_tree_insert (t, (splay_tree_key) idp, (splay_tree_value) file_data);
}

/* Create a splay tree.  */

static splay_tree
lto_splay_tree_new (void)
{
  return splay_tree_new (lto_splay_tree_compare_ids,
	 	         lto_splay_tree_delete_id,
			 NULL);
}

/* Return true when NODE has a clone that is analyzed (i.e. we need
   to load its body even if the node itself is not needed).  */

static bool
has_analyzed_clone_p (struct cgraph_node *node)
{
  struct cgraph_node *orig = node;
  node = node->clones;
  if (node)
    while (node != orig)
      {
	if (node->analyzed)
	  return true;
	if (node->clones)
	  node = node->clones;
	else if (node->next_sibling_clone)
	  node = node->next_sibling_clone;
	else
	  {
	    while (node != orig && !node->next_sibling_clone)
	      node = node->clone_of;
	    if (node != orig)
	      node = node->next_sibling_clone;
	  }
      }
  return false;
}

/* Read the function body for the function associated with NODE.  */

static void
lto_materialize_function (struct cgraph_node *node)
{
  tree decl;

  decl = node->decl;
  /* Read in functions with body (analyzed nodes)
     and also functions that are needed to produce virtual clones.  */
  if ((cgraph_function_with_gimple_body_p (node) && node->analyzed)
      || node->used_as_abstract_origin
      || has_analyzed_clone_p (node))
    {
      /* Clones don't need to be read.  */
      if (node->clone_of)
	return;
      if (DECL_FUNCTION_PERSONALITY (decl) && !first_personality_decl)
	first_personality_decl = DECL_FUNCTION_PERSONALITY (decl);
    }

  /* Let the middle end know about the function.  */
  rest_of_decl_compilation (decl, 1, 0);
}


/* Decode the content of memory pointed to by DATA in the in decl
   state object STATE. DATA_IN points to a data_in structure for
   decoding. Return the address after the decoded object in the
   input.  */

static const uint32_t *
lto_read_in_decl_state (struct data_in *data_in, const uint32_t *data,
			struct lto_in_decl_state *state)
{
  uint32_t ix;
  tree decl;
  uint32_t i, j;

  ix = *data++;
  decl = streamer_tree_cache_get_tree (data_in->reader_cache, ix);
  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      gcc_assert (decl == void_type_node);
      decl = NULL_TREE;
    }
  state->fn_decl = decl;

  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    {
      uint32_t size = *data++;
      tree *decls = ggc_alloc_vec_tree (size);

      for (j = 0; j < size; j++)
	decls[j] = streamer_tree_cache_get_tree (data_in->reader_cache, data[j]);

      state->streams[i].size = size;
      state->streams[i].trees = decls;
      data += size;
    }

  return data;
}


/* Global canonical type table.  */
static htab_t gimple_canonical_types;
static pointer_map <hashval_t> *canonical_type_hash_cache;
static unsigned long num_canonical_type_hash_entries;
static unsigned long num_canonical_type_hash_queries;

static hashval_t iterative_hash_canonical_type (tree type, hashval_t val);
static hashval_t gimple_canonical_type_hash (const void *p);
static void gimple_register_canonical_type_1 (tree t, hashval_t hash);

/* Returning a hash value for gimple type TYPE.

   The hash value returned is equal for types considered compatible
   by gimple_canonical_types_compatible_p.  */

static hashval_t
hash_canonical_type (tree type)
{
  hashval_t v;

  /* Combine a few common features of types so that types are grouped into
     smaller sets; when searching for existing matching types to merge,
     only existing types having the same features as the new type will be
     checked.  */
  v = iterative_hash_hashval_t (TREE_CODE (type), 0);
  v = iterative_hash_hashval_t (TREE_ADDRESSABLE (type), v);
  v = iterative_hash_hashval_t (TYPE_ALIGN (type), v);
  v = iterative_hash_hashval_t (TYPE_MODE (type), v);

  /* Incorporate common features of numerical types.  */
  if (INTEGRAL_TYPE_P (type)
      || SCALAR_FLOAT_TYPE_P (type)
      || FIXED_POINT_TYPE_P (type)
      || TREE_CODE (type) == OFFSET_TYPE
      || POINTER_TYPE_P (type))
    {
      v = iterative_hash_hashval_t (TYPE_PRECISION (type), v);
      v = iterative_hash_hashval_t (TYPE_UNSIGNED (type), v);
    }

  if (VECTOR_TYPE_P (type))
    {
      v = iterative_hash_hashval_t (TYPE_VECTOR_SUBPARTS (type), v);
      v = iterative_hash_hashval_t (TYPE_UNSIGNED (type), v);
    }

  if (TREE_CODE (type) == COMPLEX_TYPE)
    v = iterative_hash_hashval_t (TYPE_UNSIGNED (type), v);

  /* For pointer and reference types, fold in information about the type
     pointed to but do not recurse to the pointed-to type.  */
  if (POINTER_TYPE_P (type))
    {
      v = iterative_hash_hashval_t (TYPE_REF_CAN_ALIAS_ALL (type), v);
      v = iterative_hash_hashval_t (TYPE_ADDR_SPACE (TREE_TYPE (type)), v);
      v = iterative_hash_hashval_t (TYPE_RESTRICT (type), v);
      v = iterative_hash_hashval_t (TREE_CODE (TREE_TYPE (type)), v);
    }

  /* For integer types hash only the string flag.  */
  if (TREE_CODE (type) == INTEGER_TYPE)
    v = iterative_hash_hashval_t (TYPE_STRING_FLAG (type), v);

  /* For array types hash the domain bounds and the string flag.  */
  if (TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type))
    {
      v = iterative_hash_hashval_t (TYPE_STRING_FLAG (type), v);
      /* OMP lowering can introduce error_mark_node in place of
	 random local decls in types.  */
      if (TYPE_MIN_VALUE (TYPE_DOMAIN (type)) != error_mark_node)
	v = iterative_hash_expr (TYPE_MIN_VALUE (TYPE_DOMAIN (type)), v);
      if (TYPE_MAX_VALUE (TYPE_DOMAIN (type)) != error_mark_node)
	v = iterative_hash_expr (TYPE_MAX_VALUE (TYPE_DOMAIN (type)), v);
    }

  /* Recurse for aggregates with a single element type.  */
  if (TREE_CODE (type) == ARRAY_TYPE
      || TREE_CODE (type) == COMPLEX_TYPE
      || TREE_CODE (type) == VECTOR_TYPE)
    v = iterative_hash_canonical_type (TREE_TYPE (type), v);

  /* Incorporate function return and argument types.  */
  if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
    {
      unsigned na;
      tree p;

      /* For method types also incorporate their parent class.  */
      if (TREE_CODE (type) == METHOD_TYPE)
	v = iterative_hash_canonical_type (TYPE_METHOD_BASETYPE (type), v);

      v = iterative_hash_canonical_type (TREE_TYPE (type), v);

      for (p = TYPE_ARG_TYPES (type), na = 0; p; p = TREE_CHAIN (p))
	{
	  v = iterative_hash_canonical_type (TREE_VALUE (p), v);
	  na++;
	}

      v = iterative_hash_hashval_t (na, v);
    }

  if (RECORD_OR_UNION_TYPE_P (type))
    {
      unsigned nf;
      tree f;

      for (f = TYPE_FIELDS (type), nf = 0; f; f = TREE_CHAIN (f))
	if (TREE_CODE (f) == FIELD_DECL)
	  {
	    v = iterative_hash_canonical_type (TREE_TYPE (f), v);
	    nf++;
	  }

      v = iterative_hash_hashval_t (nf, v);
    }

  return v;
}

/* Returning a hash value for gimple type TYPE combined with VAL.  */

static hashval_t
iterative_hash_canonical_type (tree type, hashval_t val)
{
  hashval_t v;
  /* An already processed type.  */
  if (TYPE_CANONICAL (type))
    {
      type = TYPE_CANONICAL (type);
      v = gimple_canonical_type_hash (type);
    }
  else
    {
      /* Canonical types should not be able to form SCCs by design, this
	 recursion is just because we do not register canonical types in
	 optimal order.  To avoid quadratic behavior also register the
	 type here.  */
      v = hash_canonical_type (type);
      gimple_register_canonical_type_1 (type, v);
    }
  return iterative_hash_hashval_t (v, val);
}

/* Returns the hash for a canonical type P.  */

static hashval_t
gimple_canonical_type_hash (const void *p)
{
  num_canonical_type_hash_queries++;
  hashval_t *slot
    = canonical_type_hash_cache->contains (CONST_CAST_TREE ((const_tree) p));
  gcc_assert (slot != NULL);
  return *slot;
}


/* The TYPE_CANONICAL merging machinery.  It should closely resemble
   the middle-end types_compatible_p function.  It needs to avoid
   claiming types are different for types that should be treated
   the same with respect to TBAA.  Canonical types are also used
   for IL consistency checks via the useless_type_conversion_p
   predicate which does not handle all type kinds itself but falls
   back to pointer-comparison of TYPE_CANONICAL for aggregates
   for example.  */

/* Return true iff T1 and T2 are structurally identical for what
   TBAA is concerned.  */

static bool
gimple_canonical_types_compatible_p (tree t1, tree t2)
{
  /* Before starting to set up the SCC machinery handle simple cases.  */

  /* Check first for the obvious case of pointer identity.  */
  if (t1 == t2)
    return true;

  /* Check that we have two types to compare.  */
  if (t1 == NULL_TREE || t2 == NULL_TREE)
    return false;

  /* If the types have been previously registered and found equal
     they still are.  */
  if (TYPE_CANONICAL (t1)
      && TYPE_CANONICAL (t1) == TYPE_CANONICAL (t2))
    return true;

  /* Can't be the same type if the types don't have the same code.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return false;

  if (TREE_ADDRESSABLE (t1) != TREE_ADDRESSABLE (t2))
    return false;

  /* Qualifiers do not matter for canonical type comparison purposes.  */

  /* Void types and nullptr types are always the same.  */
  if (TREE_CODE (t1) == VOID_TYPE
      || TREE_CODE (t1) == NULLPTR_TYPE)
    return true;

  /* Can't be the same type if they have different alignment, or mode.  */
  if (TYPE_ALIGN (t1) != TYPE_ALIGN (t2)
      || TYPE_MODE (t1) != TYPE_MODE (t2))
    return false;

  /* Non-aggregate types can be handled cheaply.  */
  if (INTEGRAL_TYPE_P (t1)
      || SCALAR_FLOAT_TYPE_P (t1)
      || FIXED_POINT_TYPE_P (t1)
      || TREE_CODE (t1) == VECTOR_TYPE
      || TREE_CODE (t1) == COMPLEX_TYPE
      || TREE_CODE (t1) == OFFSET_TYPE
      || POINTER_TYPE_P (t1))
    {
      /* Can't be the same type if they have different sign or precision.  */
      if (TYPE_PRECISION (t1) != TYPE_PRECISION (t2)
	  || TYPE_UNSIGNED (t1) != TYPE_UNSIGNED (t2))
	return false;

      if (TREE_CODE (t1) == INTEGER_TYPE
	  && TYPE_STRING_FLAG (t1) != TYPE_STRING_FLAG (t2))
	return false;

      /* For canonical type comparisons we do not want to build SCCs
	 so we cannot compare pointed-to types.  But we can, for now,
	 require the same pointed-to type kind and match what
	 useless_type_conversion_p would do.  */
      if (POINTER_TYPE_P (t1))
	{
	  /* If the two pointers have different ref-all attributes,
	     they can't be the same type.  */
	  if (TYPE_REF_CAN_ALIAS_ALL (t1) != TYPE_REF_CAN_ALIAS_ALL (t2))
	    return false;

	  if (TYPE_ADDR_SPACE (TREE_TYPE (t1))
	      != TYPE_ADDR_SPACE (TREE_TYPE (t2)))
	    return false;

	  if (TYPE_RESTRICT (t1) != TYPE_RESTRICT (t2))
	    return false;

	  if (TREE_CODE (TREE_TYPE (t1)) != TREE_CODE (TREE_TYPE (t2)))
	    return false;
	}

      /* Tail-recurse to components.  */
      if (TREE_CODE (t1) == VECTOR_TYPE
	  || TREE_CODE (t1) == COMPLEX_TYPE)
	return gimple_canonical_types_compatible_p (TREE_TYPE (t1),
						    TREE_TYPE (t2));

      return true;
    }

  /* Do type-specific comparisons.  */
  switch (TREE_CODE (t1))
    {
    case ARRAY_TYPE:
      /* Array types are the same if the element types are the same and
	 the number of elements are the same.  */
      if (!gimple_canonical_types_compatible_p (TREE_TYPE (t1), TREE_TYPE (t2))
	  || TYPE_STRING_FLAG (t1) != TYPE_STRING_FLAG (t2)
	  || TYPE_NONALIASED_COMPONENT (t1) != TYPE_NONALIASED_COMPONENT (t2))
	return false;
      else
	{
	  tree i1 = TYPE_DOMAIN (t1);
	  tree i2 = TYPE_DOMAIN (t2);

	  /* For an incomplete external array, the type domain can be
 	     NULL_TREE.  Check this condition also.  */
	  if (i1 == NULL_TREE && i2 == NULL_TREE)
	    return true;
	  else if (i1 == NULL_TREE || i2 == NULL_TREE)
	    return false;
	  else
	    {
	      tree min1 = TYPE_MIN_VALUE (i1);
	      tree min2 = TYPE_MIN_VALUE (i2);
	      tree max1 = TYPE_MAX_VALUE (i1);
	      tree max2 = TYPE_MAX_VALUE (i2);

	      /* The minimum/maximum values have to be the same.  */
	      if ((min1 == min2
		   || (min1 && min2
		       && ((TREE_CODE (min1) == PLACEHOLDER_EXPR
			    && TREE_CODE (min2) == PLACEHOLDER_EXPR)
		           || operand_equal_p (min1, min2, 0))))
		  && (max1 == max2
		      || (max1 && max2
			  && ((TREE_CODE (max1) == PLACEHOLDER_EXPR
			       && TREE_CODE (max2) == PLACEHOLDER_EXPR)
			      || operand_equal_p (max1, max2, 0)))))
		return true;
	      else
		return false;
	    }
	}

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      /* Function types are the same if the return type and arguments types
	 are the same.  */
      if (!gimple_canonical_types_compatible_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	return false;

      if (!comp_type_attributes (t1, t2))
	return false;

      if (TYPE_ARG_TYPES (t1) == TYPE_ARG_TYPES (t2))
	return true;
      else
	{
	  tree parms1, parms2;

	  for (parms1 = TYPE_ARG_TYPES (t1), parms2 = TYPE_ARG_TYPES (t2);
	       parms1 && parms2;
	       parms1 = TREE_CHAIN (parms1), parms2 = TREE_CHAIN (parms2))
	    {
	      if (!gimple_canonical_types_compatible_p
		     (TREE_VALUE (parms1), TREE_VALUE (parms2)))
		return false;
	    }

	  if (parms1 || parms2)
	    return false;

	  return true;
	}

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree f1, f2;

	/* For aggregate types, all the fields must be the same.  */
	for (f1 = TYPE_FIELDS (t1), f2 = TYPE_FIELDS (t2);
	     f1 || f2;
	     f1 = TREE_CHAIN (f1), f2 = TREE_CHAIN (f2))
	  {
	    /* Skip non-fields.  */
	    while (f1 && TREE_CODE (f1) != FIELD_DECL)
	      f1 = TREE_CHAIN (f1);
	    while (f2 && TREE_CODE (f2) != FIELD_DECL)
	      f2 = TREE_CHAIN (f2);
	    if (!f1 || !f2)
	      break;
	    /* The fields must have the same name, offset and type.  */
	    if (DECL_NONADDRESSABLE_P (f1) != DECL_NONADDRESSABLE_P (f2)
		|| !gimple_compare_field_offset (f1, f2)
		|| !gimple_canonical_types_compatible_p
		      (TREE_TYPE (f1), TREE_TYPE (f2)))
	      return false;
	  }

	/* If one aggregate has more fields than the other, they
	   are not the same.  */
	if (f1 || f2)
	  return false;

	return true;
      }

    default:
      gcc_unreachable ();
    }
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
gimple_canonical_type_eq (const void *p1, const void *p2)
{
  const_tree t1 = (const_tree) p1;
  const_tree t2 = (const_tree) p2;
  return gimple_canonical_types_compatible_p (CONST_CAST_TREE (t1),
					      CONST_CAST_TREE (t2));
}

/* Main worker for gimple_register_canonical_type.  */

static void
gimple_register_canonical_type_1 (tree t, hashval_t hash)
{
  void **slot;

  gcc_checking_assert (TYPE_P (t) && !TYPE_CANONICAL (t));

  slot = htab_find_slot_with_hash (gimple_canonical_types, t, hash, INSERT);
  if (*slot)
    {
      tree new_type = (tree)(*slot);
      gcc_checking_assert (new_type != t);
      TYPE_CANONICAL (t) = new_type;
    }
  else
    {
      TYPE_CANONICAL (t) = t;
      *slot = (void *) t;
      /* Cache the just computed hash value.  */
      num_canonical_type_hash_entries++;
      bool existed_p;
      hashval_t *hslot = canonical_type_hash_cache->insert (t, &existed_p);
      gcc_assert (!existed_p);
      *hslot = hash;
    }
}

/* Register type T in the global type table gimple_types and set
   TYPE_CANONICAL of T accordingly.
   This is used by LTO to merge structurally equivalent types for
   type-based aliasing purposes across different TUs and languages.

   ???  This merging does not exactly match how the tree.c middle-end
   functions will assign TYPE_CANONICAL when new types are created
   during optimization (which at least happens for pointer and array
   types).  */

static void
gimple_register_canonical_type (tree t)
{
  if (TYPE_CANONICAL (t))
    return;

  gimple_register_canonical_type_1 (t, hash_canonical_type (t));
}

/* Re-compute TYPE_CANONICAL for NODE and related types.  */

static void
lto_register_canonical_types (tree node, bool first_p)
{
  if (!node
      || !TYPE_P (node))
    return;

  if (first_p)
    TYPE_CANONICAL (node) = NULL_TREE;

  if (POINTER_TYPE_P (node)
      || TREE_CODE (node) == COMPLEX_TYPE
      || TREE_CODE (node) == ARRAY_TYPE)
    lto_register_canonical_types (TREE_TYPE (node), first_p);

 if (!first_p) 
    gimple_register_canonical_type (node);
}


/* Remember trees that contains references to declarations.  */
static GTY(()) vec <tree, va_gc> *tree_with_vars;

#define CHECK_VAR(tt) \
  do \
    { \
      if ((tt) && VAR_OR_FUNCTION_DECL_P (tt) \
	  && (TREE_PUBLIC (tt) || DECL_EXTERNAL (tt))) \
	return true; \
    } while (0)

#define CHECK_NO_VAR(tt) \
  gcc_checking_assert (!(tt) || !VAR_OR_FUNCTION_DECL_P (tt))

/* Check presence of pointers to decls in fields of a tree_typed T.  */

static inline bool
mentions_vars_p_typed (tree t)
{
  CHECK_NO_VAR (TREE_TYPE (t));
  return false;
}

/* Check presence of pointers to decls in fields of a tree_common T.  */

static inline bool
mentions_vars_p_common (tree t)
{
  if (mentions_vars_p_typed (t))
    return true;
  CHECK_NO_VAR (TREE_CHAIN (t));
  return false;
}

/* Check presence of pointers to decls in fields of a decl_minimal T.  */

static inline bool
mentions_vars_p_decl_minimal (tree t)
{
  if (mentions_vars_p_common (t))
    return true;
  CHECK_NO_VAR (DECL_NAME (t));
  CHECK_VAR (DECL_CONTEXT (t));
  return false;
}

/* Check presence of pointers to decls in fields of a decl_common T.  */

static inline bool
mentions_vars_p_decl_common (tree t)
{
  if (mentions_vars_p_decl_minimal (t))
    return true;
  CHECK_VAR (DECL_SIZE (t));
  CHECK_VAR (DECL_SIZE_UNIT (t));
  CHECK_VAR (DECL_INITIAL (t));
  CHECK_NO_VAR (DECL_ATTRIBUTES (t));
  CHECK_VAR (DECL_ABSTRACT_ORIGIN (t));
  return false;
}

/* Check presence of pointers to decls in fields of a decl_with_vis T.  */

static inline bool
mentions_vars_p_decl_with_vis (tree t)
{
  if (mentions_vars_p_decl_common (t))
    return true;

  /* Accessor macro has side-effects, use field-name here. */
  CHECK_NO_VAR (t->decl_with_vis.assembler_name);
  CHECK_NO_VAR (DECL_SECTION_NAME (t));
  return false;
}

/* Check presence of pointers to decls in fields of a decl_non_common T.  */

static inline bool
mentions_vars_p_decl_non_common (tree t)
{
  if (mentions_vars_p_decl_with_vis (t))
    return true;
  CHECK_NO_VAR (DECL_ARGUMENT_FLD (t));
  CHECK_NO_VAR (DECL_RESULT_FLD (t));
  CHECK_NO_VAR (DECL_VINDEX (t));
  return false;
}

/* Check presence of pointers to decls in fields of a decl_non_common T.  */

static bool
mentions_vars_p_function (tree t)
{
  if (mentions_vars_p_decl_non_common (t))
    return true;
  CHECK_VAR (DECL_FUNCTION_PERSONALITY (t));
  return false;
}

/* Check presence of pointers to decls in fields of a field_decl T.  */

static bool
mentions_vars_p_field_decl (tree t)
{
  if (mentions_vars_p_decl_common (t))
    return true;
  CHECK_VAR (DECL_FIELD_OFFSET (t));
  CHECK_NO_VAR (DECL_BIT_FIELD_TYPE (t));
  CHECK_NO_VAR (DECL_QUALIFIER (t));
  CHECK_NO_VAR (DECL_FIELD_BIT_OFFSET (t));
  CHECK_NO_VAR (DECL_FCONTEXT (t));
  return false;
}

/* Check presence of pointers to decls in fields of a type T.  */

static bool
mentions_vars_p_type (tree t)
{
  if (mentions_vars_p_common (t))
    return true;
  CHECK_NO_VAR (TYPE_CACHED_VALUES (t));
  CHECK_VAR (TYPE_SIZE (t));
  CHECK_VAR (TYPE_SIZE_UNIT (t));
  CHECK_NO_VAR (TYPE_ATTRIBUTES (t));
  CHECK_NO_VAR (TYPE_NAME (t));

  CHECK_VAR (TYPE_MINVAL (t));
  CHECK_VAR (TYPE_MAXVAL (t));

  /* Accessor is for derived node types only. */
  CHECK_NO_VAR (t->type_non_common.binfo);

  CHECK_VAR (TYPE_CONTEXT (t));
  CHECK_NO_VAR (TYPE_CANONICAL (t));
  CHECK_NO_VAR (TYPE_MAIN_VARIANT (t));
  CHECK_NO_VAR (TYPE_NEXT_VARIANT (t));
  return false;
}

/* Check presence of pointers to decls in fields of a BINFO T.  */

static bool
mentions_vars_p_binfo (tree t)
{
  unsigned HOST_WIDE_INT i, n;

  if (mentions_vars_p_common (t))
    return true;
  CHECK_VAR (BINFO_VTABLE (t));
  CHECK_NO_VAR (BINFO_OFFSET (t));
  CHECK_NO_VAR (BINFO_VIRTUALS (t));
  CHECK_NO_VAR (BINFO_VPTR_FIELD (t));
  n = vec_safe_length (BINFO_BASE_ACCESSES (t));
  for (i = 0; i < n; i++)
    CHECK_NO_VAR (BINFO_BASE_ACCESS (t, i));
  /* Do not walk BINFO_INHERITANCE_CHAIN, BINFO_SUBVTT_INDEX
     and BINFO_VPTR_INDEX; these are used by C++ FE only.  */
  n = BINFO_N_BASE_BINFOS (t);
  for (i = 0; i < n; i++)
    CHECK_NO_VAR (BINFO_BASE_BINFO (t, i));
  return false;
}

/* Check presence of pointers to decls in fields of a CONSTRUCTOR T.  */

static bool
mentions_vars_p_constructor (tree t)
{
  unsigned HOST_WIDE_INT idx;
  constructor_elt *ce;

  if (mentions_vars_p_typed (t))
    return true;

  for (idx = 0; vec_safe_iterate (CONSTRUCTOR_ELTS (t), idx, &ce); idx++)
    {
      CHECK_NO_VAR (ce->index);
      CHECK_VAR (ce->value);
    }
  return false;
}

/* Check presence of pointers to decls in fields of an expression tree T.  */

static bool
mentions_vars_p_expr (tree t)
{
  int i;
  if (mentions_vars_p_typed (t))
    return true;
  for (i = TREE_OPERAND_LENGTH (t) - 1; i >= 0; --i)
    CHECK_VAR (TREE_OPERAND (t, i));
  return false;
}

/* Check presence of pointers to decls that needs later fixup in T.  */

static bool
mentions_vars_p (tree t)
{
  switch (TREE_CODE (t))
    {
    case IDENTIFIER_NODE:
      break;

    case TREE_LIST:
      CHECK_VAR (TREE_VALUE (t));
      CHECK_VAR (TREE_PURPOSE (t));
      CHECK_NO_VAR (TREE_CHAIN (t));
      break;

    case FIELD_DECL:
      return mentions_vars_p_field_decl (t);
      break;

    case LABEL_DECL:
    case CONST_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case IMPORTED_DECL:
    case NAMESPACE_DECL:
      return mentions_vars_p_decl_common (t);
      break;

    case VAR_DECL:
      return mentions_vars_p_decl_with_vis (t);
      break;

    case TYPE_DECL:
      return mentions_vars_p_decl_non_common (t);
      break;

    case FUNCTION_DECL:
      return mentions_vars_p_function (t);
      break;

    case TREE_BINFO:
      return mentions_vars_p_binfo (t);
      break;

    case PLACEHOLDER_EXPR:
      return mentions_vars_p_common (t);
      break;

    case BLOCK:
    case TRANSLATION_UNIT_DECL:
    case OPTIMIZATION_NODE:
    case TARGET_OPTION_NODE:
      break;

    case CONSTRUCTOR:
      return mentions_vars_p_constructor (t);
      break;

    default:
      if (TYPE_P (t))
	{
	  if (mentions_vars_p_type (t))
	    return true;
	}
      else if (EXPR_P (t))
	{
	  if (mentions_vars_p_expr (t))
	    return true;
	}
      else if (CONSTANT_CLASS_P (t))
	CHECK_NO_VAR (TREE_TYPE (t));
      else
	gcc_unreachable ();
    }
  return false;
}


/* Return the resolution for the decl with index INDEX from DATA_IN. */

static enum ld_plugin_symbol_resolution
get_resolution (struct data_in *data_in, unsigned index)
{
  if (data_in->globals_resolution.exists ())
    {
      ld_plugin_symbol_resolution_t ret;
      /* We can have references to not emitted functions in
	 DECL_FUNCTION_PERSONALITY at least.  So we can and have
	 to indeed return LDPR_UNKNOWN in some cases.   */
      if (data_in->globals_resolution.length () <= index)
	return LDPR_UNKNOWN;
      ret = data_in->globals_resolution[index];
      return ret;
    }
  else
    /* Delay resolution finding until decl merging.  */
    return LDPR_UNKNOWN;
}

/* We need to record resolutions until symbol table is read.  */
static void
register_resolution (struct lto_file_decl_data *file_data, tree decl,
		     enum ld_plugin_symbol_resolution resolution)
{
  if (resolution == LDPR_UNKNOWN)
    return;
  if (!file_data->resolution_map)
    file_data->resolution_map = pointer_map_create ();
  *pointer_map_insert (file_data->resolution_map, decl) = (void *)(size_t)resolution;
}

/* Register DECL with the global symbol table and change its
   name if necessary to avoid name clashes for static globals across
   different files.  */

static void
lto_register_var_decl_in_symtab (struct data_in *data_in, tree decl,
				 unsigned ix)
{
  tree context;

  /* Variable has file scope, not local.  */
  if (!TREE_PUBLIC (decl)
      && !((context = decl_function_context (decl))
	   && auto_var_in_fn_p (decl, context)))
    rest_of_decl_compilation (decl, 1, 0);

  /* If this variable has already been declared, queue the
     declaration for merging.  */
  if (TREE_PUBLIC (decl))
    register_resolution (data_in->file_data,
			 decl, get_resolution (data_in, ix));
}


/* Register DECL with the global symbol table and change its
   name if necessary to avoid name clashes for static globals across
   different files.  DATA_IN contains descriptors and tables for the
   file being read.  */

static void
lto_register_function_decl_in_symtab (struct data_in *data_in, tree decl,
				      unsigned ix)
{
  /* If this variable has already been declared, queue the
     declaration for merging.  */
  if (TREE_PUBLIC (decl) && !DECL_ABSTRACT (decl))
    register_resolution (data_in->file_data,
			 decl, get_resolution (data_in, ix));
}


/* For the type T re-materialize it in the type variant list and
   the pointer/reference-to chains.  */

static void
lto_fixup_prevailing_type (tree t)
{
  /* The following re-creates proper variant lists while fixing up
     the variant leaders.  We do not stream TYPE_NEXT_VARIANT so the
     variant list state before fixup is broken.  */

  /* If we are not our own variant leader link us into our new leaders
     variant list.  */
  if (TYPE_MAIN_VARIANT (t) != t)
    {
      tree mv = TYPE_MAIN_VARIANT (t);
      TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (mv);
      TYPE_NEXT_VARIANT (mv) = t;
    }

  /* The following reconstructs the pointer chains
     of the new pointed-to type if we are a main variant.  We do
     not stream those so they are broken before fixup.  */
  if (TREE_CODE (t) == POINTER_TYPE
      && TYPE_MAIN_VARIANT (t) == t)
    {
      TYPE_NEXT_PTR_TO (t) = TYPE_POINTER_TO (TREE_TYPE (t));
      TYPE_POINTER_TO (TREE_TYPE (t)) = t;
    }
  else if (TREE_CODE (t) == REFERENCE_TYPE
	   && TYPE_MAIN_VARIANT (t) == t)
    {
      TYPE_NEXT_REF_TO (t) = TYPE_REFERENCE_TO (TREE_TYPE (t));
      TYPE_REFERENCE_TO (TREE_TYPE (t)) = t;
    }
}


/* We keep prevailing tree SCCs in a hashtable with manual collision
   handling (in case all hashes compare the same) and keep the colliding
   entries in the tree_scc->next chain.  */

struct tree_scc
{
  tree_scc *next;
  /* Hash of the whole SCC.  */
  hashval_t hash;
  /* Number of trees in the SCC.  */
  unsigned len;
  /* Number of possible entries into the SCC (tree nodes [0..entry_len-1]
     which share the same individual tree hash).  */
  unsigned entry_len;
  /* The members of the SCC.
     We only need to remember the first entry node candidate for prevailing
     SCCs (but of course have access to all entries for SCCs we are
     processing).
     ???  For prevailing SCCs we really only need hash and the first
     entry candidate, but that's too awkward to implement.  */
  tree entries[1];
};

struct tree_scc_hasher : typed_noop_remove <tree_scc>
{
  typedef tree_scc value_type;
  typedef tree_scc compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

hashval_t
tree_scc_hasher::hash (const value_type *scc)
{
  return scc->hash;
}

bool
tree_scc_hasher::equal (const value_type *scc1, const compare_type *scc2)
{
  if (scc1->hash != scc2->hash
      || scc1->len != scc2->len
      || scc1->entry_len != scc2->entry_len)
    return false;
  return true;
}

static hash_table <tree_scc_hasher> tree_scc_hash;
static struct obstack tree_scc_hash_obstack;

static unsigned long num_merged_types;
static unsigned long num_prevailing_types;
static unsigned long num_type_scc_trees;
static unsigned long total_scc_size;
static unsigned long num_sccs_read;
static unsigned long total_scc_size_merged;
static unsigned long num_sccs_merged;
static unsigned long num_scc_compares;
static unsigned long num_scc_compare_collisions;


/* Compare the two entries T1 and T2 of two SCCs that are possibly equal,
   recursing through in-SCC tree edges.  Returns true if the SCCs entered
   through T1 and T2 are equal and fills in *MAP with the pairs of
   SCC entries we visited, starting with (*MAP)[0] = T1 and (*MAP)[1] = T2.  */

static bool
compare_tree_sccs_1 (tree t1, tree t2, tree **map)
{
  enum tree_code code;

  /* Mark already visited nodes.  */
  TREE_ASM_WRITTEN (t2) = 1;

  /* Push the pair onto map.  */
  (*map)[0] = t1;
  (*map)[1] = t2;
  *map = *map + 2;

  /* Compare value-fields.  */
#define compare_values(X) \
  do { \
    if (X(t1) != X(t2)) \
      return false; \
  } while (0)

  compare_values (TREE_CODE);
  code = TREE_CODE (t1);

  if (!TYPE_P (t1))
    {
      compare_values (TREE_SIDE_EFFECTS);
      compare_values (TREE_CONSTANT);
      compare_values (TREE_READONLY);
      compare_values (TREE_PUBLIC);
    }
  compare_values (TREE_ADDRESSABLE);
  compare_values (TREE_THIS_VOLATILE);
  if (DECL_P (t1))
    compare_values (DECL_UNSIGNED);
  else if (TYPE_P (t1))
    compare_values (TYPE_UNSIGNED);
  if (TYPE_P (t1))
    compare_values (TYPE_ARTIFICIAL);
  else
    compare_values (TREE_NO_WARNING);
  compare_values (TREE_NOTHROW);
  compare_values (TREE_STATIC);
  if (code != TREE_BINFO)
    compare_values (TREE_PRIVATE);
  compare_values (TREE_PROTECTED);
  compare_values (TREE_DEPRECATED);
  if (TYPE_P (t1))
    {
      compare_values (TYPE_SATURATING);
      compare_values (TYPE_ADDR_SPACE);
    }
  else if (code == SSA_NAME)
    compare_values (SSA_NAME_IS_DEFAULT_DEF);

  if (CODE_CONTAINS_STRUCT (code, TS_INT_CST))
    {
      compare_values (TREE_INT_CST_LOW);
      compare_values (TREE_INT_CST_HIGH);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_REAL_CST))
    {
      /* ???  No suitable compare routine available.  */
      REAL_VALUE_TYPE r1 = TREE_REAL_CST (t1);
      REAL_VALUE_TYPE r2 = TREE_REAL_CST (t2);
      if (r1.cl != r2.cl
	  || r1.decimal != r2.decimal
	  || r1.sign != r2.sign
	  || r1.signalling != r2.signalling
	  || r1.canonical != r2.canonical
	  || r1.uexp != r2.uexp)
	return false;
      for (unsigned i = 0; i < SIGSZ; ++i)
	if (r1.sig[i] != r2.sig[i])
	  return false;
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    if (!fixed_compare (EQ_EXPR,
			TREE_FIXED_CST_PTR (t1), TREE_FIXED_CST_PTR (t2)))
      return false;


  /* We don't want to compare locations, so there is nothing do compare
     for TS_DECL_MINIMAL.  */

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      compare_values (DECL_MODE);
      compare_values (DECL_NONLOCAL);
      compare_values (DECL_VIRTUAL_P);
      compare_values (DECL_IGNORED_P);
      compare_values (DECL_ABSTRACT);
      compare_values (DECL_ARTIFICIAL);
      compare_values (DECL_USER_ALIGN);
      compare_values (DECL_PRESERVE_P);
      compare_values (DECL_EXTERNAL);
      compare_values (DECL_GIMPLE_REG_P);
      compare_values (DECL_ALIGN);
      if (code == LABEL_DECL)
	{
	  compare_values (EH_LANDING_PAD_NR);
	  compare_values (LABEL_DECL_UID);
	}
      else if (code == FIELD_DECL)
	{
	  compare_values (DECL_PACKED);
	  compare_values (DECL_NONADDRESSABLE_P);
	  compare_values (DECL_OFFSET_ALIGN);
	}
      else if (code == VAR_DECL)
	{
	  compare_values (DECL_HAS_DEBUG_EXPR_P);
	  compare_values (DECL_NONLOCAL_FRAME);
	}
      if (code == RESULT_DECL
	  || code == PARM_DECL
	  || code == VAR_DECL)
	{
	  compare_values (DECL_BY_REFERENCE);
	  if (code == VAR_DECL
	      || code == PARM_DECL)
	    compare_values (DECL_HAS_VALUE_EXPR_P);
	}
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    compare_values (DECL_REGISTER);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      compare_values (DECL_COMMON);
      compare_values (DECL_DLLIMPORT_P);
      compare_values (DECL_WEAK);
      compare_values (DECL_SEEN_IN_BIND_EXPR_P);
      compare_values (DECL_COMDAT);
      compare_values (DECL_VISIBILITY);
      compare_values (DECL_VISIBILITY_SPECIFIED);
      if (code == VAR_DECL)
	{
	  compare_values (DECL_HARD_REGISTER);
          /* DECL_IN_TEXT_SECTION is set during final asm output only.  */
	  compare_values (DECL_IN_CONSTANT_POOL);
	  compare_values (DECL_TLS_MODEL);
	}
      if (VAR_OR_FUNCTION_DECL_P (t1))
	compare_values (DECL_INIT_PRIORITY);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      compare_values (DECL_BUILT_IN_CLASS);
      compare_values (DECL_STATIC_CONSTRUCTOR);
      compare_values (DECL_STATIC_DESTRUCTOR);
      compare_values (DECL_UNINLINABLE);
      compare_values (DECL_POSSIBLY_INLINED);
      compare_values (DECL_IS_NOVOPS);
      compare_values (DECL_IS_RETURNS_TWICE);
      compare_values (DECL_IS_MALLOC);
      compare_values (DECL_IS_OPERATOR_NEW);
      compare_values (DECL_DECLARED_INLINE_P);
      compare_values (DECL_STATIC_CHAIN);
      compare_values (DECL_NO_INLINE_WARNING_P);
      compare_values (DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT);
      compare_values (DECL_NO_LIMIT_STACK);
      compare_values (DECL_DISREGARD_INLINE_LIMITS);
      compare_values (DECL_PURE_P);
      compare_values (DECL_LOOPING_CONST_OR_PURE_P);
      compare_values (DECL_FINAL_P);
      compare_values (DECL_CXX_CONSTRUCTOR_P);
      compare_values (DECL_CXX_DESTRUCTOR_P);
      if (DECL_BUILT_IN_CLASS (t1) != NOT_BUILT_IN)
	compare_values (DECL_FUNCTION_CODE);
      if (DECL_STATIC_DESTRUCTOR (t1))
	compare_values (DECL_FINI_PRIORITY);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      compare_values (TYPE_MODE);
      compare_values (TYPE_STRING_FLAG);
      compare_values (TYPE_NO_FORCE_BLK);
      compare_values (TYPE_NEEDS_CONSTRUCTING);
      if (RECORD_OR_UNION_TYPE_P (t1))
	{
	  compare_values (TYPE_TRANSPARENT_AGGR);
	  compare_values (TYPE_FINAL_P);
	}
      else if (code == ARRAY_TYPE)
	compare_values (TYPE_NONALIASED_COMPONENT);
      compare_values (TYPE_PACKED);
      compare_values (TYPE_RESTRICT);
      compare_values (TYPE_USER_ALIGN);
      compare_values (TYPE_READONLY);
      compare_values (TYPE_PRECISION);
      compare_values (TYPE_ALIGN);
      compare_values (TYPE_ALIAS_SET);
    }

  /* We don't want to compare locations, so there is nothing do compare
     for TS_EXP.  */

  /* BLOCKs are function local and we don't merge anything there, so
     simply refuse to merge.  */
  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    return false;

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    if (strcmp (TRANSLATION_UNIT_LANGUAGE (t1),
		TRANSLATION_UNIT_LANGUAGE (t2)) != 0)
      return false;

  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION))
    if (memcmp (TREE_TARGET_OPTION (t1), TREE_TARGET_OPTION (t2),
		sizeof (struct cl_target_option)) != 0)
      return false;

  if (CODE_CONTAINS_STRUCT (code, TS_OPTIMIZATION))
    if (memcmp (TREE_OPTIMIZATION (t1), TREE_OPTIMIZATION (t2),
		sizeof (struct cl_optimization)) != 0)
      return false;

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    if (vec_safe_length (BINFO_BASE_ACCESSES (t1))
	!= vec_safe_length (BINFO_BASE_ACCESSES (t2)))
      return false;

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    compare_values (CONSTRUCTOR_NELTS);

  if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    if (IDENTIFIER_LENGTH (t1) != IDENTIFIER_LENGTH (t2)
	|| memcmp (IDENTIFIER_POINTER (t1), IDENTIFIER_POINTER (t2),
		   IDENTIFIER_LENGTH (t1)) != 0)
      return false;

  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    if (TREE_STRING_LENGTH (t1) != TREE_STRING_LENGTH (t2)
	|| memcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
		   TREE_STRING_LENGTH (t1)) != 0)
      return false;

#undef compare_values


  /* Compare pointer fields.  */

  /* Recurse.  Search & Replaced from DFS_write_tree_body.
     Folding the early checks into the compare_tree_edges recursion
     macro makes debugging way quicker as you are able to break on
     compare_tree_sccs_1 and simply finish until a call returns false
     to spot the SCC members with the difference.  */
#define compare_tree_edges(E1, E2) \
  do { \
    tree t1_ = (E1), t2_ = (E2); \
    if (t1_ != t2_ \
	&& (!t1_ || !t2_ \
	    || !TREE_VISITED (t2_) \
	    || (!TREE_ASM_WRITTEN (t2_) \
		&& !compare_tree_sccs_1 (t1_, t2_, map)))) \
      return false; \
    /* Only non-NULL trees outside of the SCC may compare equal.  */ \
    gcc_checking_assert (t1_ != t2_ || (!t2_ || !TREE_VISITED (t2_))); \
  } while (0)

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      if (code != IDENTIFIER_NODE)
	compare_tree_edges (TREE_TYPE (t1), TREE_TYPE (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    {
      unsigned i;
      /* Note that the number of elements for EXPR has already been emitted
	 in EXPR's header (see streamer_write_tree_header).  */
      for (i = 0; i < VECTOR_CST_NELTS (t1); ++i)
	compare_tree_edges (VECTOR_CST_ELT (t1, i), VECTOR_CST_ELT (t2, i));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    {
      compare_tree_edges (TREE_REALPART (t1), TREE_REALPART (t2));
      compare_tree_edges (TREE_IMAGPART (t1), TREE_IMAGPART (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    {
      compare_tree_edges (DECL_NAME (t1), DECL_NAME (t2));
      /* ???  Global decls from different TUs have non-matching
	 TRANSLATION_UNIT_DECLs.  Only consider a small set of
	 decls equivalent, we should not end up merging others.  */
      if ((code == TYPE_DECL
	   || code == NAMESPACE_DECL
	   || code == IMPORTED_DECL
	   || code == CONST_DECL
	   || (VAR_OR_FUNCTION_DECL_P (t1)
	       && (TREE_PUBLIC (t1) || DECL_EXTERNAL (t1))))
	  && DECL_FILE_SCOPE_P (t1) && DECL_FILE_SCOPE_P (t2))
	;
      else
	compare_tree_edges (DECL_CONTEXT (t1), DECL_CONTEXT (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      compare_tree_edges (DECL_SIZE (t1), DECL_SIZE (t2));
      compare_tree_edges (DECL_SIZE_UNIT (t1), DECL_SIZE_UNIT (t2));
      compare_tree_edges (DECL_ATTRIBUTES (t1), DECL_ATTRIBUTES (t2));
      if ((code == VAR_DECL
	   || code == PARM_DECL)
	  && DECL_HAS_VALUE_EXPR_P (t1))
	compare_tree_edges (DECL_VALUE_EXPR (t1), DECL_VALUE_EXPR (t2));
      if (code == VAR_DECL
	  && DECL_HAS_DEBUG_EXPR_P (t1))
	compare_tree_edges (DECL_DEBUG_EXPR (t1), DECL_DEBUG_EXPR (t2));
      /* LTO specific edges.  */
      if (code != FUNCTION_DECL
	  && code != TRANSLATION_UNIT_DECL)
	compare_tree_edges (DECL_INITIAL (t1), DECL_INITIAL (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    {
      if (code == FUNCTION_DECL)
	{
	  tree a1, a2;
	  for (a1 = DECL_ARGUMENTS (t1), a2 = DECL_ARGUMENTS (t2);
	       a1 || a2;
	       a1 = TREE_CHAIN (a1), a2 = TREE_CHAIN (a2))
	    compare_tree_edges (a1, a2);
	  compare_tree_edges (DECL_RESULT (t1), DECL_RESULT (t2));
	}
      else if (code == TYPE_DECL)
	compare_tree_edges (DECL_ORIGINAL_TYPE (t1), DECL_ORIGINAL_TYPE (t2));
      compare_tree_edges (DECL_VINDEX (t1), DECL_VINDEX (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      /* Make sure we don't inadvertently set the assembler name.  */
      if (DECL_ASSEMBLER_NAME_SET_P (t1))
	compare_tree_edges (DECL_ASSEMBLER_NAME (t1),
			    DECL_ASSEMBLER_NAME (t2));
      compare_tree_edges (DECL_SECTION_NAME (t1), DECL_SECTION_NAME (t2));
      compare_tree_edges (DECL_COMDAT_GROUP (t1), DECL_COMDAT_GROUP (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
    {
      compare_tree_edges (DECL_FIELD_OFFSET (t1), DECL_FIELD_OFFSET (t2));
      compare_tree_edges (DECL_BIT_FIELD_TYPE (t1), DECL_BIT_FIELD_TYPE (t2));
      compare_tree_edges (DECL_BIT_FIELD_REPRESENTATIVE (t1),
			  DECL_BIT_FIELD_REPRESENTATIVE (t2));
      compare_tree_edges (DECL_FIELD_BIT_OFFSET (t1),
			  DECL_FIELD_BIT_OFFSET (t2));
      compare_tree_edges (DECL_FCONTEXT (t1), DECL_FCONTEXT (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      compare_tree_edges (DECL_FUNCTION_PERSONALITY (t1),
			  DECL_FUNCTION_PERSONALITY (t2));
      compare_tree_edges (DECL_FUNCTION_SPECIFIC_TARGET (t1),
			  DECL_FUNCTION_SPECIFIC_TARGET (t2));
      compare_tree_edges (DECL_FUNCTION_SPECIFIC_OPTIMIZATION (t1),
			  DECL_FUNCTION_SPECIFIC_OPTIMIZATION (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      compare_tree_edges (TYPE_SIZE (t1), TYPE_SIZE (t2));
      compare_tree_edges (TYPE_SIZE_UNIT (t1), TYPE_SIZE_UNIT (t2));
      compare_tree_edges (TYPE_ATTRIBUTES (t1), TYPE_ATTRIBUTES (t2));
      compare_tree_edges (TYPE_NAME (t1), TYPE_NAME (t2));
      /* Do not compare TYPE_POINTER_TO or TYPE_REFERENCE_TO.  They will be
	 reconstructed during fixup.  */
      /* Do not compare TYPE_NEXT_VARIANT, we reconstruct the variant lists
	 during fixup.  */
      compare_tree_edges (TYPE_MAIN_VARIANT (t1), TYPE_MAIN_VARIANT (t2));
      /* ???  Global types from different TUs have non-matching
	 TRANSLATION_UNIT_DECLs.  Still merge them if they are otherwise
	 equal.  */
      if (TYPE_FILE_SCOPE_P (t1) && TYPE_FILE_SCOPE_P (t2))
	;
      else
	compare_tree_edges (TYPE_CONTEXT (t1), TYPE_CONTEXT (t2));
      /* TYPE_CANONICAL is re-computed during type merging, so do not
	 compare it here.  */
      compare_tree_edges (TYPE_STUB_DECL (t1), TYPE_STUB_DECL (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      if (code == ENUMERAL_TYPE)
	compare_tree_edges (TYPE_VALUES (t1), TYPE_VALUES (t2));
      else if (code == ARRAY_TYPE)
	compare_tree_edges (TYPE_DOMAIN (t1), TYPE_DOMAIN (t2));
      else if (RECORD_OR_UNION_TYPE_P (t1))
	{
	  tree f1, f2;
	  for (f1 = TYPE_FIELDS (t1), f2 = TYPE_FIELDS (t2);
	       f1 || f2;
	       f1 = TREE_CHAIN (f1), f2 = TREE_CHAIN (f2))
	    compare_tree_edges (f1, f2);
	  compare_tree_edges (TYPE_BINFO (t1), TYPE_BINFO (t2));
	}
      else if (code == FUNCTION_TYPE
	       || code == METHOD_TYPE)
	compare_tree_edges (TYPE_ARG_TYPES (t1), TYPE_ARG_TYPES (t2));
      if (!POINTER_TYPE_P (t1))
	compare_tree_edges (TYPE_MINVAL (t1), TYPE_MINVAL (t2));
      compare_tree_edges (TYPE_MAXVAL (t1), TYPE_MAXVAL (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    {
      compare_tree_edges (TREE_PURPOSE (t1), TREE_PURPOSE (t2));
      compare_tree_edges (TREE_VALUE (t1), TREE_VALUE (t2));
      compare_tree_edges (TREE_CHAIN (t1), TREE_CHAIN (t2));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    for (int i = 0; i < TREE_VEC_LENGTH (t1); i++)
      compare_tree_edges (TREE_VEC_ELT (t1, i), TREE_VEC_ELT (t2, i));

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    {
      for (int i = 0; i < TREE_OPERAND_LENGTH (t1); i++)
	compare_tree_edges (TREE_OPERAND (t1, i),
			    TREE_OPERAND (t2, i));

      /* BLOCKs are function local and we don't merge anything there.  */
      if (TREE_BLOCK (t1) || TREE_BLOCK (t2))
	return false;
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    {
      unsigned i;
      tree t;
      /* Lengths have already been compared above.  */
      FOR_EACH_VEC_ELT (*BINFO_BASE_BINFOS (t1), i, t)
	compare_tree_edges (t, BINFO_BASE_BINFO (t2, i));
      FOR_EACH_VEC_SAFE_ELT (BINFO_BASE_ACCESSES (t1), i, t)
	compare_tree_edges (t, BINFO_BASE_ACCESS (t2, i));
      compare_tree_edges (BINFO_OFFSET (t1), BINFO_OFFSET (t2));
      compare_tree_edges (BINFO_VTABLE (t1), BINFO_VTABLE (t2));
      compare_tree_edges (BINFO_VPTR_FIELD (t1), BINFO_VPTR_FIELD (t2));
      /* Do not walk BINFO_INHERITANCE_CHAIN, BINFO_SUBVTT_INDEX
	 and BINFO_VPTR_INDEX; these are used by C++ FE only.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    {
      unsigned i;
      tree index, value;
      /* Lengths have already been compared above.  */
      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t1), i, index, value)
	{
	  compare_tree_edges (index, CONSTRUCTOR_ELT (t2, i)->index);
	  compare_tree_edges (value, CONSTRUCTOR_ELT (t2, i)->value);
	}
    }

#undef compare_tree_edges

  return true;
}

/* Compare the tree scc SCC to the prevailing candidate PSCC, filling
   out MAP if they are equal.  */

static bool
compare_tree_sccs (tree_scc *pscc, tree_scc *scc,
		   tree *map)
{
  /* Assume SCC entry hashes are sorted after their cardinality.  Which
     means we can simply take the first n-tuple of equal hashes
     (which is recorded as entry_len) and do n SCC entry candidate
     comparisons.  */
  for (unsigned i = 0; i < pscc->entry_len; ++i)
    {
      tree *mapp = map;
      num_scc_compare_collisions++;
      if (compare_tree_sccs_1 (pscc->entries[0], scc->entries[i], &mapp))
	{
	  /* Equal - no need to reset TREE_VISITED or TREE_ASM_WRITTEN
	     on the scc as all trees will be freed.  */
	  return true;
	}
      /* Reset TREE_ASM_WRITTEN on scc for the next compare or in case
         the SCC prevails.  */
      for (unsigned j = 0; j < scc->len; ++j)
	TREE_ASM_WRITTEN (scc->entries[j]) = 0;
    }

  return false;
}

/* QSort sort function to sort a map of two pointers after the 2nd
   pointer.  */

static int
cmp_tree (const void *p1_, const void *p2_)
{
  tree *p1 = (tree *)(const_cast<void *>(p1_));
  tree *p2 = (tree *)(const_cast<void *>(p2_));
  if (p1[1] == p2[1])
    return 0;
  return ((uintptr_t)p1[1] < (uintptr_t)p2[1]) ? -1 : 1;
}

/* Try to unify the SCC with nodes FROM to FROM + LEN in CACHE and
   hash value SCC_HASH with an already recorded SCC.  Return true if
   that was successful, otherwise return false.  */

static bool
unify_scc (struct streamer_tree_cache_d *cache, unsigned from,
	   unsigned len, unsigned scc_entry_len, hashval_t scc_hash)
{
  bool unified_p = false;
  tree_scc *scc
    = (tree_scc *) alloca (sizeof (tree_scc) + (len - 1) * sizeof (tree));
  scc->next = NULL;
  scc->hash = scc_hash;
  scc->len = len;
  scc->entry_len = scc_entry_len;
  for (unsigned i = 0; i < len; ++i)
    {
      tree t = streamer_tree_cache_get_tree (cache, from + i);
      scc->entries[i] = t;
      /* Do not merge SCCs with local entities inside them.  Also do
	 not merge TRANSLATION_UNIT_DECLs.  */
      if (TREE_CODE (t) == TRANSLATION_UNIT_DECL
	  || (VAR_OR_FUNCTION_DECL_P (t)
	      && !(TREE_PUBLIC (t) || DECL_EXTERNAL (t)))
	  || TREE_CODE (t) == LABEL_DECL)
	{
	  /* Avoid doing any work for these cases and do not worry to
	     record the SCCs for further merging.  */
	  return false;
	}
    }

  /* Look for the list of candidate SCCs to compare against.  */
  tree_scc **slot;
  slot = tree_scc_hash.find_slot_with_hash (scc, scc_hash, INSERT);
  if (*slot)
    {
      /* Try unifying against each candidate.  */
      num_scc_compares++;

      /* Set TREE_VISITED on the scc so we can easily identify tree nodes
	 outside of the scc when following tree edges.  Make sure
	 that TREE_ASM_WRITTEN is unset so we can use it as 2nd bit
	 to track whether we visited the SCC member during the compare.
	 We cannot use TREE_VISITED on the pscc members as the extended
	 scc and pscc can overlap.  */
      for (unsigned i = 0; i < scc->len; ++i)
	{
	  TREE_VISITED (scc->entries[i]) = 1;
	  gcc_checking_assert (!TREE_ASM_WRITTEN (scc->entries[i]));
	}

      tree *map = XALLOCAVEC (tree, 2 * len);
      for (tree_scc *pscc = *slot; pscc; pscc = pscc->next)
	{
	  if (!compare_tree_sccs (pscc, scc, map))
	    continue;

	  /* Found an equal SCC.  */
	  unified_p = true;
	  num_scc_compare_collisions--;
	  num_sccs_merged++;
	  total_scc_size_merged += len;

#ifdef ENABLE_CHECKING
	  for (unsigned i = 0; i < len; ++i)
	    {
	      tree t = map[2*i+1];
	      enum tree_code code = TREE_CODE (t);
	      /* IDENTIFIER_NODEs should be singletons and are merged by the
		 streamer.  The others should be singletons, too, and we
		 should not merge them in any way.  */
	      gcc_assert (code != TRANSLATION_UNIT_DECL
			  && code != IDENTIFIER_NODE
			  && !streamer_handle_as_builtin_p (t));
	    }
#endif

	  /* Fixup the streamer cache with the prevailing nodes according
	     to the tree node mapping computed by compare_tree_sccs.  */
	  if (len == 1)
	    streamer_tree_cache_replace_tree (cache, pscc->entries[0], from);
	  else
	    {
	      tree *map2 = XALLOCAVEC (tree, 2 * len);
	      for (unsigned i = 0; i < len; ++i)
		{
		  map2[i*2] = (tree)(uintptr_t)(from + i);
		  map2[i*2+1] = scc->entries[i];
		}
	      qsort (map2, len, 2 * sizeof (tree), cmp_tree);
	      qsort (map, len, 2 * sizeof (tree), cmp_tree);
	      for (unsigned i = 0; i < len; ++i)
		streamer_tree_cache_replace_tree (cache, map[2*i],
						  (uintptr_t)map2[2*i]);
	    }

	  /* Free the tree nodes from the read SCC.  */
	  for (unsigned i = 0; i < len; ++i)
	    {
	      if (TYPE_P (scc->entries[i]))
		num_merged_types++;
	      ggc_free (scc->entries[i]);
	    }

	  break;
	}

      /* Reset TREE_VISITED if we didn't unify the SCC with another.  */
      if (!unified_p)
	for (unsigned i = 0; i < scc->len; ++i)
	  TREE_VISITED (scc->entries[i]) = 0;
    }

  /* If we didn't unify it to any candidate duplicate the relevant
     pieces to permanent storage and link it into the chain.  */
  if (!unified_p)
    {
      tree_scc *pscc
	= XOBNEWVAR (&tree_scc_hash_obstack, tree_scc, sizeof (tree_scc));
      memcpy (pscc, scc, sizeof (tree_scc));
      pscc->next = (*slot);
      *slot = pscc;
    }
  return unified_p;
}


/* Read all the symbols from buffer DATA, using descriptors in DECL_DATA.
   RESOLUTIONS is the set of symbols picked by the linker (read from the
   resolution file when the linker plugin is being used).  */

static void
lto_read_decls (struct lto_file_decl_data *decl_data, const void *data,
		vec<ld_plugin_symbol_resolution_t> resolutions)
{
  const struct lto_decl_header *header = (const struct lto_decl_header *) data;
  const int decl_offset = sizeof (struct lto_decl_header);
  const int main_offset = decl_offset + header->decl_state_size;
  const int string_offset = main_offset + header->main_size;
  struct lto_input_block ib_main;
  struct data_in *data_in;
  unsigned int i;
  const uint32_t *data_ptr, *data_end;
  uint32_t num_decl_states;

  LTO_INIT_INPUT_BLOCK (ib_main, (const char *) data + main_offset, 0,
			header->main_size);

  data_in = lto_data_in_create (decl_data, (const char *) data + string_offset,
				header->string_size, resolutions);

  /* We do not uniquify the pre-loaded cache entries, those are middle-end
     internal types that should not be merged.  */

  /* Read the global declarations and types.  */
  while (ib_main.p < ib_main.len)
    {
      tree t;
      unsigned from = data_in->reader_cache->nodes.length ();
      /* Read and uniquify SCCs as in the input stream.  */
      enum LTO_tags tag = streamer_read_record_start (&ib_main);
      if (tag == LTO_tree_scc)
	{
	  unsigned len_;
	  unsigned scc_entry_len;
	  hashval_t scc_hash = lto_input_scc (&ib_main, data_in, &len_,
					      &scc_entry_len);
	  unsigned len = data_in->reader_cache->nodes.length () - from;
	  gcc_assert (len == len_);

	  total_scc_size += len;
	  num_sccs_read++;

	  /* We have the special case of size-1 SCCs that are pre-merged
	     by means of identifier and string sharing for example.
	     ???  Maybe we should avoid streaming those as SCCs.  */
	  tree first = streamer_tree_cache_get_tree (data_in->reader_cache,
						     from);
	  if (len == 1
	      && (TREE_CODE (first) == IDENTIFIER_NODE
		  || TREE_CODE (first) == INTEGER_CST
		  || TREE_CODE (first) == TRANSLATION_UNIT_DECL
		  || streamer_handle_as_builtin_p (first)))
	    continue;

	  /* Try to unify the SCC with already existing ones.  */
	  if (!flag_ltrans
	      && unify_scc (data_in->reader_cache, from,
			    len, scc_entry_len, scc_hash))
	    continue;

	  /* Do remaining fixup tasks for prevailing nodes.  */
	  bool seen_type = false;
	  for (unsigned i = 0; i < len; ++i)
	    {
	      tree t = streamer_tree_cache_get_tree (data_in->reader_cache,
						     from + i);
	      /* Reconstruct the type variant and pointer-to/reference-to
		 chains.  */
	      if (TYPE_P (t))
		{
		  seen_type = true;
		  num_prevailing_types++;
		  lto_fixup_prevailing_type (t);
		}
	      /* Compute the canonical type of all types.
		 ???  Should be able to assert that !TYPE_CANONICAL.  */
	      if (TYPE_P (t) && !TYPE_CANONICAL (t))
		gimple_register_canonical_type (t);
	      /* Link shared INTEGER_CSTs into TYPE_CACHED_VALUEs of its
		 type which is also member of this SCC.  */
	      if (TREE_CODE (t) == INTEGER_CST
		  && !TREE_OVERFLOW (t))
		cache_integer_cst (t);
	      /* Register TYPE_DECLs with the debuginfo machinery.  */
	      if (!flag_wpa
		  && TREE_CODE (t) == TYPE_DECL)
		debug_hooks->type_decl (t, !DECL_FILE_SCOPE_P (t));
	      if (!flag_ltrans)
		{
		  /* Register variables and functions with the
		     symbol table.  */
		  if (TREE_CODE (t) == VAR_DECL)
		    lto_register_var_decl_in_symtab (data_in, t, from + i);
		  else if (TREE_CODE (t) == FUNCTION_DECL
			   && !DECL_BUILT_IN (t))
		    lto_register_function_decl_in_symtab (data_in, t, from + i);
		  /* Scan the tree for references to global functions or
		     variables and record those for later fixup.  */
		  if (mentions_vars_p (t))
		    vec_safe_push (tree_with_vars, t);
		}
	    }
	  if (seen_type)
	    num_type_scc_trees += len;
	}
      else
	{
	  /* Pickle stray references.  */
	  t = lto_input_tree_1 (&ib_main, data_in, tag, 0);
	  gcc_assert (t && data_in->reader_cache->nodes.length () == from);
	}
    }

  /* Read in lto_in_decl_state objects.  */
  data_ptr = (const uint32_t *) ((const char*) data + decl_offset); 
  data_end =
     (const uint32_t *) ((const char*) data_ptr + header->decl_state_size);
  num_decl_states = *data_ptr++;
  
  gcc_assert (num_decl_states > 0);
  decl_data->global_decl_state = lto_new_in_decl_state ();
  data_ptr = lto_read_in_decl_state (data_in, data_ptr,
				     decl_data->global_decl_state);

  /* Read in per-function decl states and enter them in hash table.  */
  decl_data->function_decl_states =
    htab_create_ggc (37, lto_hash_in_decl_state, lto_eq_in_decl_state, NULL);

  for (i = 1; i < num_decl_states; i++)
    {
      struct lto_in_decl_state *state = lto_new_in_decl_state ();
      void **slot;

      data_ptr = lto_read_in_decl_state (data_in, data_ptr, state);
      slot = htab_find_slot (decl_data->function_decl_states, state, INSERT);
      gcc_assert (*slot == NULL);
      *slot = state;
    }

  if (data_ptr != data_end)
    internal_error ("bytecode stream: garbage at the end of symbols section");

  /* Set the current decl state to be the global state. */
  decl_data->current_decl_state = decl_data->global_decl_state;

  lto_data_in_delete (data_in);
}

/* Custom version of strtoll, which is not portable.  */

static HOST_WIDEST_INT
lto_parse_hex (const char *p)
{
  HOST_WIDEST_INT ret = 0;

  for (; *p != '\0'; ++p)
    {
      char c = *p;
      unsigned char part;
      ret <<= 4;
      if (c >= '0' && c <= '9')
        part = c - '0';
      else if (c >= 'a' && c <= 'f')
        part = c - 'a' + 10;
      else if (c >= 'A' && c <= 'F')
        part = c - 'A' + 10;
      else
        internal_error ("could not parse hex number");
      ret |= part;
    }

  return ret;
}

/* Read resolution for file named FILE_NAME. The resolution is read from
   RESOLUTION. */

static void
lto_resolution_read (splay_tree file_ids, FILE *resolution, lto_file *file)
{
  /* We require that objects in the resolution file are in the same
     order as the lto1 command line. */
  unsigned int name_len;
  char *obj_name;
  unsigned int num_symbols;
  unsigned int i;
  struct lto_file_decl_data *file_data;
  splay_tree_node nd = NULL; 

  if (!resolution)
    return;

  name_len = strlen (file->filename);
  obj_name = XNEWVEC (char, name_len + 1);
  fscanf (resolution, " ");   /* Read white space. */

  fread (obj_name, sizeof (char), name_len, resolution);
  obj_name[name_len] = '\0';
  if (filename_cmp (obj_name, file->filename) != 0)
    internal_error ("unexpected file name %s in linker resolution file. "
		    "Expected %s", obj_name, file->filename);
  if (file->offset != 0)
    {
      int t;
      char offset_p[17];
      HOST_WIDEST_INT offset;
      t = fscanf (resolution, "@0x%16s", offset_p);
      if (t != 1)
        internal_error ("could not parse file offset");
      offset = lto_parse_hex (offset_p);
      if (offset != file->offset)
        internal_error ("unexpected offset");
    }

  free (obj_name);

  fscanf (resolution, "%u", &num_symbols);

  for (i = 0; i < num_symbols; i++)
    {
      int t;
      unsigned index;
      unsigned HOST_WIDE_INT id;
      char r_str[27];
      enum ld_plugin_symbol_resolution r = (enum ld_plugin_symbol_resolution) 0;
      unsigned int j;
      unsigned int lto_resolution_str_len =
	sizeof (lto_resolution_str) / sizeof (char *);
      res_pair rp;

      t = fscanf (resolution, "%u " HOST_WIDE_INT_PRINT_HEX_PURE " %26s %*[^\n]\n", 
		  &index, &id, r_str);
      if (t != 3)
        internal_error ("invalid line in the resolution file");

      for (j = 0; j < lto_resolution_str_len; j++)
	{
	  if (strcmp (lto_resolution_str[j], r_str) == 0)
	    {
	      r = (enum ld_plugin_symbol_resolution) j;
	      break;
	    }
	}
      if (j == lto_resolution_str_len)
	internal_error ("invalid resolution in the resolution file");

      if (!(nd && lto_splay_tree_id_equal_p (nd->key, id)))
	{
	  nd = lto_splay_tree_lookup (file_ids, id);
	  if (nd == NULL)
	    internal_error ("resolution sub id %wx not in object file", id);
	}

      file_data = (struct lto_file_decl_data *)nd->value;
      /* The indexes are very sparse. To save memory save them in a compact
         format that is only unpacked later when the subfile is processed. */
      rp.res = r;
      rp.index = index;
      file_data->respairs.safe_push (rp);
      if (file_data->max_index < index)
        file_data->max_index = index;
    }
}

/* List of file_decl_datas */
struct file_data_list
  {
    struct lto_file_decl_data *first, *last;
  };

/* Is the name for a id'ed LTO section? */

static int 
lto_section_with_id (const char *name, unsigned HOST_WIDE_INT *id)
{
  const char *s;

  if (strncmp (name, LTO_SECTION_NAME_PREFIX, strlen (LTO_SECTION_NAME_PREFIX)))
    return 0;
  s = strrchr (name, '.');
  return s && sscanf (s, "." HOST_WIDE_INT_PRINT_HEX_PURE, id) == 1;
}

/* Create file_data of each sub file id */

static int 
create_subid_section_table (struct lto_section_slot *ls, splay_tree file_ids,
                            struct file_data_list *list)
{
  struct lto_section_slot s_slot, *new_slot;
  unsigned HOST_WIDE_INT id;
  splay_tree_node nd;
  void **hash_slot;
  char *new_name;
  struct lto_file_decl_data *file_data;

  if (!lto_section_with_id (ls->name, &id))
    return 1;
  
  /* Find hash table of sub module id */
  nd = lto_splay_tree_lookup (file_ids, id);
  if (nd != NULL)
    {
      file_data = (struct lto_file_decl_data *)nd->value;
    }
  else
    {
      file_data = ggc_alloc_lto_file_decl_data ();
      memset(file_data, 0, sizeof (struct lto_file_decl_data));
      file_data->id = id;
      file_data->section_hash_table = lto_obj_create_section_hash_table ();;
      lto_splay_tree_insert (file_ids, id, file_data);

      /* Maintain list in linker order */
      if (!list->first)
        list->first = file_data;
      if (list->last)
        list->last->next = file_data;
      list->last = file_data;
    }

  /* Copy section into sub module hash table */
  new_name = XDUPVEC (char, ls->name, strlen (ls->name) + 1);
  s_slot.name = new_name;
  hash_slot = htab_find_slot (file_data->section_hash_table, &s_slot, INSERT);
  gcc_assert (*hash_slot == NULL);

  new_slot = XDUP (struct lto_section_slot, ls);
  new_slot->name = new_name;
  *hash_slot = new_slot;
  return 1;
}

/* Read declarations and other initializations for a FILE_DATA. */

static void
lto_file_finalize (struct lto_file_decl_data *file_data, lto_file *file)
{
  const char *data;
  size_t len;
  vec<ld_plugin_symbol_resolution_t>
	resolutions = vNULL;
  int i;
  res_pair *rp;

  /* Create vector for fast access of resolution. We do this lazily
     to save memory. */ 
  resolutions.safe_grow_cleared (file_data->max_index + 1);
  for (i = 0; file_data->respairs.iterate (i, &rp); i++)
    resolutions[rp->index] = rp->res;
  file_data->respairs.release ();

  file_data->renaming_hash_table = lto_create_renaming_table ();
  file_data->file_name = file->filename;
  data = lto_get_section_data (file_data, LTO_section_decls, NULL, &len);
  if (data == NULL)
    {
      internal_error ("cannot read LTO decls from %s", file_data->file_name);
      return;
    }
  /* Frees resolutions */
  lto_read_decls (file_data, data, resolutions);
  lto_free_section_data (file_data, LTO_section_decls, NULL, data, len);
}

/* Finalize FILE_DATA in FILE and increase COUNT. */

static int 
lto_create_files_from_ids (lto_file *file, struct lto_file_decl_data *file_data,
			   int *count)
{
  lto_file_finalize (file_data, file);
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Creating file %s with sub id " HOST_WIDE_INT_PRINT_HEX "\n", 
	     file_data->file_name, file_data->id);
  (*count)++;
  return 0;
}

/* Generate a TREE representation for all types and external decls
   entities in FILE.  

   Read all of the globals out of the file.  Then read the cgraph
   and process the .o index into the cgraph nodes so that it can open
   the .o file to load the functions and ipa information.   */

static struct lto_file_decl_data *
lto_file_read (lto_file *file, FILE *resolution_file, int *count)
{
  struct lto_file_decl_data *file_data = NULL;
  splay_tree file_ids;
  htab_t section_hash_table;
  struct lto_section_slot *section;
  struct file_data_list file_list;
  struct lto_section_list section_list;
 
  memset (&section_list, 0, sizeof (struct lto_section_list)); 
  section_hash_table = lto_obj_build_section_table (file, &section_list);

  /* Find all sub modules in the object and put their sections into new hash
     tables in a splay tree. */
  file_ids = lto_splay_tree_new ();
  memset (&file_list, 0, sizeof (struct file_data_list));
  for (section = section_list.first; section != NULL; section = section->next)
    create_subid_section_table (section, file_ids, &file_list);

  /* Add resolutions to file ids */
  lto_resolution_read (file_ids, resolution_file, file);

  /* Finalize each lto file for each submodule in the merged object */
  for (file_data = file_list.first; file_data != NULL; file_data = file_data->next)
    lto_create_files_from_ids (file, file_data, count);
 
  splay_tree_delete (file_ids);
  htab_delete (section_hash_table);

  return file_list.first;
}

#if HAVE_MMAP_FILE && HAVE_SYSCONF && defined _SC_PAGE_SIZE
#define LTO_MMAP_IO 1
#endif

#if LTO_MMAP_IO
/* Page size of machine is used for mmap and munmap calls.  */
static size_t page_mask;
#endif

/* Get the section data of length LEN from FILENAME starting at
   OFFSET.  The data segment must be freed by the caller when the
   caller is finished.  Returns NULL if all was not well.  */

static char *
lto_read_section_data (struct lto_file_decl_data *file_data,
		       intptr_t offset, size_t len)
{
  char *result;
  static int fd = -1;
  static char *fd_name;
#if LTO_MMAP_IO
  intptr_t computed_len;
  intptr_t computed_offset;
  intptr_t diff;
#endif

  /* Keep a single-entry file-descriptor cache.  The last file we
     touched will get closed at exit.
     ???  Eventually we want to add a more sophisticated larger cache
     or rather fix function body streaming to not stream them in
     practically random order.  */
  if (fd != -1
      && filename_cmp (fd_name, file_data->file_name) != 0)
    {
      free (fd_name);
      close (fd);
      fd = -1;
    }
  if (fd == -1)
    {
      fd = open (file_data->file_name, O_RDONLY|O_BINARY);
      if (fd == -1)
        {
	  fatal_error ("Cannot open %s", file_data->file_name);
	  return NULL;
        }
      fd_name = xstrdup (file_data->file_name);
    }

#if LTO_MMAP_IO
  if (!page_mask)
    {
      size_t page_size = sysconf (_SC_PAGE_SIZE);
      page_mask = ~(page_size - 1);
    }

  computed_offset = offset & page_mask;
  diff = offset - computed_offset;
  computed_len = len + diff;

  result = (char *) mmap (NULL, computed_len, PROT_READ, MAP_PRIVATE,
			  fd, computed_offset);
  if (result == MAP_FAILED)
    {
      fatal_error ("Cannot map %s", file_data->file_name);
      return NULL;
    }

  return result + diff;
#else
  result = (char *) xmalloc (len);
  if (lseek (fd, offset, SEEK_SET) != offset
      || read (fd, result, len) != (ssize_t) len)
    {
      free (result);
      fatal_error ("Cannot read %s", file_data->file_name);
      result = NULL;
    }
#ifdef __MINGW32__
  /* Native windows doesn't supports delayed unlink on opened file. So
     we close file here again. This produces higher I/O load, but at least
     it prevents to have dangling file handles preventing unlink.  */
  free (fd_name);
  fd_name = NULL;
  close (fd);
  fd = -1;
#endif
  return result;
#endif
}    


/* Get the section data from FILE_DATA of SECTION_TYPE with NAME.
   NAME will be NULL unless the section type is for a function
   body.  */

static const char *
get_section_data (struct lto_file_decl_data *file_data,
		      enum lto_section_type section_type,
		      const char *name,
		      size_t *len)
{
  htab_t section_hash_table = file_data->section_hash_table;
  struct lto_section_slot *f_slot;
  struct lto_section_slot s_slot;
  const char *section_name = lto_get_section_name (section_type, name, file_data);
  char *data = NULL;

  *len = 0;
  s_slot.name = section_name;
  f_slot = (struct lto_section_slot *) htab_find (section_hash_table, &s_slot);
  if (f_slot)
    {
      data = lto_read_section_data (file_data, f_slot->start, f_slot->len);
      *len = f_slot->len;
    }

  free (CONST_CAST (char *, section_name));
  return data;
}


/* Free the section data from FILE_DATA of SECTION_TYPE with NAME that
   starts at OFFSET and has LEN bytes.  */

static void
free_section_data (struct lto_file_decl_data *file_data ATTRIBUTE_UNUSED,
		   enum lto_section_type section_type ATTRIBUTE_UNUSED,
		   const char *name ATTRIBUTE_UNUSED,
		   const char *offset, size_t len ATTRIBUTE_UNUSED)
{
#if LTO_MMAP_IO
  intptr_t computed_len;
  intptr_t computed_offset;
  intptr_t diff;
#endif

#if LTO_MMAP_IO
  computed_offset = ((intptr_t) offset) & page_mask;
  diff = (intptr_t) offset - computed_offset;
  computed_len = len + diff;

  munmap ((caddr_t) computed_offset, computed_len);
#else
  free (CONST_CAST(char *, offset));
#endif
}

static lto_file *current_lto_file;

/* Helper for qsort; compare partitions and return one with smaller size.
   We sort from greatest to smallest so parallel build doesn't stale on the
   longest compilation being executed too late.  */

static int
cmp_partitions_size (const void *a, const void *b)
{
  const struct ltrans_partition_def *pa
     = *(struct ltrans_partition_def *const *)a;
  const struct ltrans_partition_def *pb
     = *(struct ltrans_partition_def *const *)b;
  return pb->insns - pa->insns;
}

/* Helper for qsort; compare partitions and return one with smaller order.  */

static int
cmp_partitions_order (const void *a, const void *b)
{
  const struct ltrans_partition_def *pa
     = *(struct ltrans_partition_def *const *)a;
  const struct ltrans_partition_def *pb
     = *(struct ltrans_partition_def *const *)b;
  int ordera = -1, orderb = -1;

  if (lto_symtab_encoder_size (pa->encoder))
    ordera = lto_symtab_encoder_deref (pa->encoder, 0)->order;
  if (lto_symtab_encoder_size (pb->encoder))
    orderb = lto_symtab_encoder_deref (pb->encoder, 0)->order;
  return orderb - ordera;
}

/* Write all output files in WPA mode and the file with the list of
   LTRANS units.  */

static void
lto_wpa_write_files (void)
{
  unsigned i, n_sets;
  lto_file *file;
  ltrans_partition part;
  FILE *ltrans_output_list_stream;
  char *temp_filename;
  size_t blen;

  /* Open the LTRANS output list.  */
  if (!ltrans_output_list)
    fatal_error ("no LTRANS output list filename provided");
  ltrans_output_list_stream = fopen (ltrans_output_list, "w");
  if (ltrans_output_list_stream == NULL)
    fatal_error ("opening LTRANS output list %s: %m", ltrans_output_list);

  timevar_push (TV_WHOPR_WPA);

  FOR_EACH_VEC_ELT (ltrans_partitions, i, part)
    lto_stats.num_output_symtab_nodes += lto_symtab_encoder_size (part->encoder);

  /* Find out statics that need to be promoted
     to globals with hidden visibility because they are accessed from multiple
     partitions.  */
  lto_promote_cross_file_statics ();

  timevar_pop (TV_WHOPR_WPA);

  timevar_push (TV_WHOPR_WPA_IO);

  /* Generate a prefix for the LTRANS unit files.  */
  blen = strlen (ltrans_output_list);
  temp_filename = (char *) xmalloc (blen + sizeof ("2147483648.o"));
  strcpy (temp_filename, ltrans_output_list);
  if (blen > sizeof (".out")
      && strcmp (temp_filename + blen - sizeof (".out") + 1,
		 ".out") == 0)
    temp_filename[blen - sizeof (".out") + 1] = '\0';
  blen = strlen (temp_filename);

  n_sets = ltrans_partitions.length ();

  /* Sort partitions by size so small ones are compiled last.
     FIXME: Even when not reordering we may want to output one list for parallel make
     and other for final link command.  */
  ltrans_partitions.qsort (flag_toplevel_reorder
			   ? cmp_partitions_size
			   : cmp_partitions_order);
  for (i = 0; i < n_sets; i++)
    {
      size_t len;
      ltrans_partition part = ltrans_partitions[i];

      /* Write all the nodes in SET.  */
      sprintf (temp_filename + blen, "%u.o", i);
      file = lto_obj_file_open (temp_filename, true);
      if (!file)
	fatal_error ("lto_obj_file_open() failed");

      if (!quiet_flag)
	fprintf (stderr, " %s (%s %i insns)", temp_filename, part->name, part->insns);
      if (cgraph_dump_file)
	{
          lto_symtab_encoder_iterator lsei;
	  
	  fprintf (cgraph_dump_file, "Writing partition %s to file %s, %i insns\n",
		   part->name, temp_filename, part->insns);
	  fprintf (cgraph_dump_file, "  Symbols in partition: ");
	  for (lsei = lsei_start_in_partition (part->encoder); !lsei_end_p (lsei);
	       lsei_next_in_partition (&lsei))
	    {
	      symtab_node *node = lsei_node (lsei);
	      fprintf (cgraph_dump_file, "%s ", symtab_node_asm_name (node));
	    }
	  fprintf (cgraph_dump_file, "\n  Symbols in boundary: ");
	  for (lsei = lsei_start (part->encoder); !lsei_end_p (lsei);
	       lsei_next (&lsei))
	    {
	      symtab_node *node = lsei_node (lsei);
	      if (!lto_symtab_encoder_in_partition_p (part->encoder, node))
		{
	          fprintf (cgraph_dump_file, "%s ", symtab_node_asm_name (node));
		  cgraph_node *cnode = dyn_cast <cgraph_node> (node);
		  if (cnode
		      && lto_symtab_encoder_encode_body_p (part->encoder, cnode))
		    fprintf (cgraph_dump_file, "(body included)");
		  else
		    {
		      varpool_node *vnode = dyn_cast <varpool_node> (node);
		      if (vnode
			  && lto_symtab_encoder_encode_initializer_p (part->encoder, vnode))
			fprintf (cgraph_dump_file, "(initializer included)");
		    }
		}
	    }
	  fprintf (cgraph_dump_file, "\n");
	}
      gcc_checking_assert (lto_symtab_encoder_size (part->encoder) || !i);

      lto_set_current_out_file (file);

      ipa_write_optimization_summaries (part->encoder);

      lto_set_current_out_file (NULL);
      lto_obj_file_close (file);
      free (file);
      part->encoder = NULL;

      len = strlen (temp_filename);
      if (fwrite (temp_filename, 1, len, ltrans_output_list_stream) < len
	  || fwrite ("\n", 1, 1, ltrans_output_list_stream) < 1)
	fatal_error ("writing to LTRANS output list %s: %m",
		     ltrans_output_list);
    }

  lto_stats.num_output_files += n_sets;

  /* Close the LTRANS output list.  */
  if (fclose (ltrans_output_list_stream))
    fatal_error ("closing LTRANS output list %s: %m", ltrans_output_list);

  free_ltrans_partitions();
  free (temp_filename);

  timevar_pop (TV_WHOPR_WPA_IO);
}


/* If TT is a variable or function decl replace it with its
   prevailing variant.  */
#define LTO_SET_PREVAIL(tt) \
  do {\
    if ((tt) && VAR_OR_FUNCTION_DECL_P (tt) \
	&& (TREE_PUBLIC (tt) || DECL_EXTERNAL (tt))) \
      { \
        tt = lto_symtab_prevailing_decl (tt); \
	fixed = true; \
      } \
  } while (0)

/* Ensure that TT isn't a replacable var of function decl.  */
#define LTO_NO_PREVAIL(tt) \
  gcc_assert (!(tt) || !VAR_OR_FUNCTION_DECL_P (tt))

/* Given a tree T replace all fields referring to variables or functions
   with their prevailing variant.  */
static void
lto_fixup_prevailing_decls (tree t)
{
  enum tree_code code = TREE_CODE (t);
  bool fixed = false;

  gcc_checking_assert (code != CONSTRUCTOR && code != TREE_BINFO);
  LTO_NO_PREVAIL (TREE_TYPE (t));
  if (CODE_CONTAINS_STRUCT (code, TS_COMMON))
    LTO_NO_PREVAIL (TREE_CHAIN (t));
  if (DECL_P (t))
    {
      LTO_NO_PREVAIL (DECL_NAME (t));
      LTO_SET_PREVAIL (DECL_CONTEXT (t));
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
	{
	  LTO_SET_PREVAIL (DECL_SIZE (t));
	  LTO_SET_PREVAIL (DECL_SIZE_UNIT (t));
	  LTO_SET_PREVAIL (DECL_INITIAL (t));
	  LTO_NO_PREVAIL (DECL_ATTRIBUTES (t));
	  LTO_SET_PREVAIL (DECL_ABSTRACT_ORIGIN (t));
	}
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
	{
	  LTO_NO_PREVAIL (t->decl_with_vis.assembler_name);
	  LTO_NO_PREVAIL (DECL_SECTION_NAME (t));
	}
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
	{
	  LTO_NO_PREVAIL (DECL_ARGUMENT_FLD (t));
	  LTO_NO_PREVAIL (DECL_RESULT_FLD (t));
	  LTO_NO_PREVAIL (DECL_VINDEX (t));
	}
      if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
	LTO_SET_PREVAIL (DECL_FUNCTION_PERSONALITY (t));
      if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
	{
	  LTO_SET_PREVAIL (DECL_FIELD_OFFSET (t));
	  LTO_NO_PREVAIL (DECL_BIT_FIELD_TYPE (t));
	  LTO_NO_PREVAIL (DECL_QUALIFIER (t));
	  LTO_NO_PREVAIL (DECL_FIELD_BIT_OFFSET (t));
	  LTO_NO_PREVAIL (DECL_FCONTEXT (t));
	}
    }
  else if (TYPE_P (t))
    {
      LTO_NO_PREVAIL (TYPE_CACHED_VALUES (t));
      LTO_SET_PREVAIL (TYPE_SIZE (t));
      LTO_SET_PREVAIL (TYPE_SIZE_UNIT (t));
      LTO_NO_PREVAIL (TYPE_ATTRIBUTES (t));
      LTO_NO_PREVAIL (TYPE_NAME (t));

      LTO_SET_PREVAIL (TYPE_MINVAL (t));
      LTO_SET_PREVAIL (TYPE_MAXVAL (t));
      LTO_NO_PREVAIL (t->type_non_common.binfo);

      LTO_SET_PREVAIL (TYPE_CONTEXT (t));

      LTO_NO_PREVAIL (TYPE_CANONICAL (t));
      LTO_NO_PREVAIL (TYPE_MAIN_VARIANT (t));
      LTO_NO_PREVAIL (TYPE_NEXT_VARIANT (t));
    }
  else if (EXPR_P (t))
    {
      int i;
      for (i = TREE_OPERAND_LENGTH (t) - 1; i >= 0; --i)
	LTO_SET_PREVAIL (TREE_OPERAND (t, i));
    }
  else
    {
      switch (code)
	{
	case TREE_LIST:
	  LTO_SET_PREVAIL (TREE_VALUE (t));
	  LTO_SET_PREVAIL (TREE_PURPOSE (t));
	  LTO_NO_PREVAIL (TREE_PURPOSE (t));
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  /* If we fixed nothing, then we missed something seen by
     mentions_vars_p.  */
  gcc_checking_assert (fixed);
}
#undef LTO_SET_PREVAIL
#undef LTO_NO_PREVAIL

/* Helper function of lto_fixup_decls. Walks the var and fn streams in STATE,
   replaces var and function decls with the corresponding prevailing def.  */

static void
lto_fixup_state (struct lto_in_decl_state *state)
{
  unsigned i, si;
  struct lto_tree_ref_table *table;

  /* Although we only want to replace FUNCTION_DECLs and VAR_DECLs,
     we still need to walk from all DECLs to find the reachable
     FUNCTION_DECLs and VAR_DECLs.  */
  for (si = 0; si < LTO_N_DECL_STREAMS; si++)
    {
      table = &state->streams[si];
      for (i = 0; i < table->size; i++)
	{
	  tree *tp = table->trees + i;
	  if (VAR_OR_FUNCTION_DECL_P (*tp)
	      && (TREE_PUBLIC (*tp) || DECL_EXTERNAL (*tp)))
	    *tp = lto_symtab_prevailing_decl (*tp);
	}
    }
}

/* A callback of htab_traverse. Just extracts a state from SLOT
   and calls lto_fixup_state. */

static int
lto_fixup_state_aux (void **slot, void *aux ATTRIBUTE_UNUSED)
{
  struct lto_in_decl_state *state = (struct lto_in_decl_state *) *slot;
  lto_fixup_state (state);
  return 1;
}

/* Fix the decls from all FILES. Replaces each decl with the corresponding
   prevailing one.  */

static void
lto_fixup_decls (struct lto_file_decl_data **files)
{
  unsigned int i;
  tree t;

  if (tree_with_vars)
    FOR_EACH_VEC_ELT ((*tree_with_vars), i, t)
      lto_fixup_prevailing_decls (t);

  for (i = 0; files[i]; i++)
    {
      struct lto_file_decl_data *file = files[i];
      struct lto_in_decl_state *state = file->global_decl_state;
      lto_fixup_state (state);

      htab_traverse (file->function_decl_states, lto_fixup_state_aux, NULL);
    }
}

static GTY((length ("lto_stats.num_input_files + 1"))) struct lto_file_decl_data **all_file_decl_data;

/* Turn file datas for sub files into a single array, so that they look
   like separate files for further passes. */

static void
lto_flatten_files (struct lto_file_decl_data **orig, int count, int last_file_ix)
{
  struct lto_file_decl_data *n, *next;
  int i, k;

  lto_stats.num_input_files = count;
  all_file_decl_data
    = ggc_alloc_cleared_vec_lto_file_decl_data_ptr (count + 1);
  /* Set the hooks so that all of the ipa passes can read in their data.  */
  lto_set_in_hooks (all_file_decl_data, get_section_data, free_section_data);
  for (i = 0, k = 0; i < last_file_ix; i++) 
    {
      for (n = orig[i]; n != NULL; n = next)
	{
	  all_file_decl_data[k++] = n;
	  next = n->next;
	  n->next = NULL;
	}
    }
  all_file_decl_data[k] = NULL;
  gcc_assert (k == count);
}

/* Input file data before flattening (i.e. splitting them to subfiles to support
   incremental linking.  */
static int real_file_count;
static GTY((length ("real_file_count + 1"))) struct lto_file_decl_data **real_file_decl_data;

static void print_lto_report_1 (void);

/* Read all the symbols from the input files FNAMES.  NFILES is the
   number of files requested in the command line.  Instantiate a
   global call graph by aggregating all the sub-graphs found in each
   file.  */

static void
read_cgraph_and_symbols (unsigned nfiles, const char **fnames)
{
  unsigned int i, last_file_ix;
  FILE *resolution;
  int count = 0;
  struct lto_file_decl_data **decl_data;
  void **res;
  symtab_node *snode;

  init_cgraph ();

  timevar_push (TV_IPA_LTO_DECL_IN);

  real_file_decl_data
    = decl_data = ggc_alloc_cleared_vec_lto_file_decl_data_ptr (nfiles + 1);
  real_file_count = nfiles;

  /* Read the resolution file.  */
  resolution = NULL;
  if (resolution_file_name)
    {
      int t;
      unsigned num_objects;

      resolution = fopen (resolution_file_name, "r");
      if (resolution == NULL)
	fatal_error ("could not open symbol resolution file: %m");

      t = fscanf (resolution, "%u", &num_objects);
      gcc_assert (t == 1);

      /* True, since the plugin splits the archives.  */
      gcc_assert (num_objects == nfiles);
    }
  cgraph_state = CGRAPH_LTO_STREAMING;

  canonical_type_hash_cache = new pointer_map <hashval_t>;
  gimple_canonical_types = htab_create_ggc (16381, gimple_canonical_type_hash,
					    gimple_canonical_type_eq, 0);
  gcc_obstack_init (&tree_scc_hash_obstack);
  tree_scc_hash.create (4096);

  /* Register the common node types with the canonical type machinery so
     we properly share alias-sets across languages and TUs.  Do not
     expose the common nodes as type merge target - those that should be
     are already exposed so by pre-loading the LTO streamer caches.
     Do two passes - first clear TYPE_CANONICAL and then re-compute it.  */
  for (i = 0; i < itk_none; ++i)
    lto_register_canonical_types (integer_types[i], true);
  for (i = 0; i < stk_type_kind_last; ++i)
    lto_register_canonical_types (sizetype_tab[i], true);
  for (i = 0; i < TI_MAX; ++i)
    lto_register_canonical_types (global_trees[i], true);
  for (i = 0; i < itk_none; ++i)
    lto_register_canonical_types (integer_types[i], false);
  for (i = 0; i < stk_type_kind_last; ++i)
    lto_register_canonical_types (sizetype_tab[i], false);
  for (i = 0; i < TI_MAX; ++i)
    lto_register_canonical_types (global_trees[i], false);

  if (!quiet_flag)
    fprintf (stderr, "Reading object files:");

  /* Read all of the object files specified on the command line.  */
  for (i = 0, last_file_ix = 0; i < nfiles; ++i)
    {
      struct lto_file_decl_data *file_data = NULL;
      if (!quiet_flag)
	{
	  fprintf (stderr, " %s", fnames[i]);
	  fflush (stderr);
	}

      current_lto_file = lto_obj_file_open (fnames[i], false);
      if (!current_lto_file)
	break;

      file_data = lto_file_read (current_lto_file, resolution, &count);
      if (!file_data)
	{
	  lto_obj_file_close (current_lto_file);
	  free (current_lto_file);
	  current_lto_file = NULL;
	  break;
	}

      decl_data[last_file_ix++] = file_data;

      lto_obj_file_close (current_lto_file);
      free (current_lto_file);
      current_lto_file = NULL;
    }

  lto_flatten_files (decl_data, count, last_file_ix);
  lto_stats.num_input_files = count;
  ggc_free(decl_data);
  real_file_decl_data = NULL;

  if (resolution_file_name)
    fclose (resolution);

  /* Show the LTO report before launching LTRANS.  */
  if (flag_lto_report || (flag_wpa && flag_lto_report_wpa))
    print_lto_report_1 ();

  /* Free gimple type merging datastructures.  */
  tree_scc_hash.dispose ();
  obstack_free (&tree_scc_hash_obstack, NULL);
  htab_delete (gimple_canonical_types);
  gimple_canonical_types = NULL;
  delete canonical_type_hash_cache;
  canonical_type_hash_cache = NULL;
  ggc_collect ();

  /* Set the hooks so that all of the ipa passes can read in their data.  */
  lto_set_in_hooks (all_file_decl_data, get_section_data, free_section_data);

  timevar_pop (TV_IPA_LTO_DECL_IN);

  if (!quiet_flag)
    fprintf (stderr, "\nReading the callgraph\n");

  timevar_push (TV_IPA_LTO_CGRAPH_IO);
  /* Read the symtab.  */
  input_symtab ();

  /* Store resolutions into the symbol table.  */

  FOR_EACH_SYMBOL (snode)
    if (symtab_real_symbol_p (snode)
	&& snode->lto_file_data
	&& snode->lto_file_data->resolution_map
	&& (res = pointer_map_contains (snode->lto_file_data->resolution_map,
					snode->decl)))
      snode->resolution
	= (enum ld_plugin_symbol_resolution)(size_t)*res;
  for (i = 0; all_file_decl_data[i]; i++)
    if (all_file_decl_data[i]->resolution_map)
      {
        pointer_map_destroy (all_file_decl_data[i]->resolution_map);
        all_file_decl_data[i]->resolution_map = NULL;
      }
  
  timevar_pop (TV_IPA_LTO_CGRAPH_IO);

  if (!quiet_flag)
    fprintf (stderr, "Merging declarations\n");

  timevar_push (TV_IPA_LTO_DECL_MERGE);
  /* Merge global decls.  In ltrans mode we read merged cgraph, we do not
     need to care about resolving symbols again, we only need to replace
     duplicated declarations read from the callgraph and from function
     sections.  */
  if (!flag_ltrans)
    {
      lto_symtab_merge_decls ();

      /* If there were errors during symbol merging bail out, we have no
	 good way to recover here.  */
      if (seen_error ())
	fatal_error ("errors during merging of translation units");

      /* Fixup all decls.  */
      lto_fixup_decls (all_file_decl_data);
    }
  if (tree_with_vars)
    ggc_free (tree_with_vars);
  tree_with_vars = NULL;
  ggc_collect ();

  timevar_pop (TV_IPA_LTO_DECL_MERGE);
  /* Each pass will set the appropriate timer.  */

  if (!quiet_flag)
    fprintf (stderr, "Reading summaries\n");

  /* Read the IPA summary data.  */
  if (flag_ltrans)
    ipa_read_optimization_summaries ();
  else
    ipa_read_summaries ();

  for (i = 0; all_file_decl_data[i]; i++)
    {
      gcc_assert (all_file_decl_data[i]->symtab_node_encoder);
      lto_symtab_encoder_delete (all_file_decl_data[i]->symtab_node_encoder);
      all_file_decl_data[i]->symtab_node_encoder = NULL;
      lto_free_function_in_decl_state (all_file_decl_data[i]->global_decl_state);
      all_file_decl_data[i]->global_decl_state = NULL;
      all_file_decl_data[i]->current_decl_state = NULL; 
    }

  /* Finally merge the cgraph according to the decl merging decisions.  */
  timevar_push (TV_IPA_LTO_CGRAPH_MERGE);
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Before merging:\n");
      dump_symtab (cgraph_dump_file);
    }
  lto_symtab_merge_symbols ();
  ggc_collect ();
  cgraph_state = CGRAPH_STATE_IPA_SSA;

  timevar_pop (TV_IPA_LTO_CGRAPH_MERGE);

  timevar_push (TV_IPA_LTO_DECL_INIT_IO);

  /* Indicate that the cgraph is built and ready.  */
  cgraph_function_flags_ready = true;

  timevar_pop (TV_IPA_LTO_DECL_INIT_IO);
  ggc_free (all_file_decl_data);
  all_file_decl_data = NULL;
}


/* Materialize all the bodies for all the nodes in the callgraph.  */

static void
materialize_cgraph (void)
{
  tree decl;
  struct cgraph_node *node; 
  unsigned i;
  timevar_id_t lto_timer;

  if (!quiet_flag)
    fprintf (stderr,
	     flag_wpa ? "Materializing decls:" : "Reading function bodies:");

  /* Now that we have input the cgraph, we need to clear all of the aux
     nodes and read the functions if we are not running in WPA mode.  */
  timevar_push (TV_IPA_LTO_GIMPLE_IN);

  FOR_EACH_FUNCTION (node)
    {
      if (node->lto_file_data)
	{
	  lto_materialize_function (node);
	  lto_stats.num_input_cgraph_nodes++;
	}
    }

  timevar_pop (TV_IPA_LTO_GIMPLE_IN);

  /* Start the appropriate timer depending on the mode that we are
     operating in.  */
  lto_timer = (flag_wpa) ? TV_WHOPR_WPA
	      : (flag_ltrans) ? TV_WHOPR_LTRANS
	      : TV_LTO;
  timevar_push (lto_timer);

  current_function_decl = NULL;
  set_cfun (NULL);

  /* Inform the middle end about the global variables we have seen.  */
  FOR_EACH_VEC_ELT (*lto_global_var_decls, i, decl)
    rest_of_decl_compilation (decl, 1, 0);

  if (!quiet_flag)
    fprintf (stderr, "\n");

  timevar_pop (lto_timer);
}


/* Show various memory usage statistics related to LTO.  */
static void
print_lto_report_1 (void)
{
  const char *pfx = (flag_lto) ? "LTO" : (flag_wpa) ? "WPA" : "LTRANS";
  fprintf (stderr, "%s statistics\n", pfx);

  fprintf (stderr, "[%s] read %lu SCCs of average size %f\n",
	   pfx, num_sccs_read, total_scc_size / (double)num_sccs_read);
  fprintf (stderr, "[%s] %lu tree bodies read in total\n", pfx, total_scc_size);
  if (flag_wpa && tree_scc_hash.is_created ())
    {
      fprintf (stderr, "[%s] tree SCC table: size %ld, %ld elements, "
	       "collision ratio: %f\n", pfx,
	       (long) tree_scc_hash.size (),
	       (long) tree_scc_hash.elements (),
	       tree_scc_hash.collisions ());
      hash_table<tree_scc_hasher>::iterator hiter;
      tree_scc *scc, *max_scc = NULL;
      unsigned max_length = 0;
      FOR_EACH_HASH_TABLE_ELEMENT (tree_scc_hash, scc, x, hiter)
	{
	  unsigned length = 0;
	  tree_scc *s = scc;
	  for (; s; s = s->next)
	    length++;
	  if (length > max_length)
	    {
	      max_length = length;
	      max_scc = scc;
	    }
	}
      fprintf (stderr, "[%s] tree SCC max chain length %u (size %u)\n",
	       pfx, max_length, max_scc->len);
      fprintf (stderr, "[%s] Compared %lu SCCs, %lu collisions (%f)\n", pfx,
	       num_scc_compares, num_scc_compare_collisions,
	       num_scc_compare_collisions / (double) num_scc_compares);
      fprintf (stderr, "[%s] Merged %lu SCCs\n", pfx, num_sccs_merged);
      fprintf (stderr, "[%s] Merged %lu tree bodies\n", pfx,
	       total_scc_size_merged);
      fprintf (stderr, "[%s] Merged %lu types\n", pfx, num_merged_types);
      fprintf (stderr, "[%s] %lu types prevailed (%lu associated trees)\n",
	       pfx, num_prevailing_types, num_type_scc_trees);
      fprintf (stderr, "[%s] GIMPLE canonical type table: size %ld, "
	       "%ld elements, %ld searches, %ld collisions (ratio: %f)\n", pfx,
	       (long) htab_size (gimple_canonical_types),
	       (long) htab_elements (gimple_canonical_types),
	       (long) gimple_canonical_types->searches,
	       (long) gimple_canonical_types->collisions,
	       htab_collisions (gimple_canonical_types));
      fprintf (stderr, "[%s] GIMPLE canonical type pointer-map: "
	       "%lu elements, %ld searches\n", pfx,
	       num_canonical_type_hash_entries,
	       num_canonical_type_hash_queries);
    }

  print_lto_report (pfx);
}

/* Perform whole program analysis (WPA) on the callgraph and write out the
   optimization plan.  */

static void
do_whole_program_analysis (void)
{
  symtab_node *node;

  timevar_start (TV_PHASE_OPT_GEN);

  /* Note that since we are in WPA mode, materialize_cgraph will not
     actually read in all the function bodies.  It only materializes
     the decls and cgraph nodes so that analysis can be performed.  */
  materialize_cgraph ();

  /* Reading in the cgraph uses different timers, start timing WPA now.  */
  timevar_push (TV_WHOPR_WPA);

  if (pre_ipa_mem_report)
    {
      fprintf (stderr, "Memory consumption before IPA\n");
      dump_memory_report (false);
    }

  cgraph_function_flags_ready = true;

  if (cgraph_dump_file)
    dump_symtab (cgraph_dump_file);
  bitmap_obstack_initialize (NULL);
  cgraph_state = CGRAPH_STATE_IPA_SSA;

  execute_ipa_pass_list (g->get_passes ()->all_regular_ipa_passes);
  symtab_remove_unreachable_nodes (false, dump_file);

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Optimized ");
      dump_symtab (cgraph_dump_file);
    }
#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif
  bitmap_obstack_release (NULL);

  /* We are about to launch the final LTRANS phase, stop the WPA timer.  */
  timevar_pop (TV_WHOPR_WPA);

  timevar_push (TV_WHOPR_PARTITIONING);
  if (flag_lto_partition_1to1)
    lto_1_to_1_map ();
  else if (flag_lto_partition_max)
    lto_max_map ();
  else
    lto_balanced_map ();

  /* AUX pointers are used by partitioning code to bookkeep number of
     partitions symbol is in.  This is no longer needed.  */
  FOR_EACH_SYMBOL (node)
    node->aux = NULL;

  lto_stats.num_cgraph_partitions += ltrans_partitions.length ();
  timevar_pop (TV_WHOPR_PARTITIONING);

  timevar_stop (TV_PHASE_OPT_GEN);
  timevar_start (TV_PHASE_STREAM_OUT);

  if (!quiet_flag)
    {
      fprintf (stderr, "\nStreaming out");
      fflush (stderr);
    }
  lto_wpa_write_files ();
  if (!quiet_flag)
    fprintf (stderr, "\n");

  timevar_stop (TV_PHASE_STREAM_OUT);

  ggc_collect ();
  if (post_ipa_mem_report)
    {
      fprintf (stderr, "Memory consumption after IPA\n");
      dump_memory_report (false);
    }

  /* Show the LTO report before launching LTRANS.  */
  if (flag_lto_report || (flag_wpa && flag_lto_report_wpa))
    print_lto_report_1 ();
  if (mem_report_wpa)
    dump_memory_report (true);
}


static GTY(()) tree lto_eh_personality_decl;

/* Return the LTO personality function decl.  */

tree
lto_eh_personality (void)
{
  if (!lto_eh_personality_decl)
    {
      /* Use the first personality DECL for our personality if we don't
	 support multiple ones.  This ensures that we don't artificially
	 create the need for them in a single-language program.  */
      if (first_personality_decl && !dwarf2out_do_cfi_asm ())
	lto_eh_personality_decl = first_personality_decl;
      else
	lto_eh_personality_decl = lhd_gcc_personality ();
    }

  return lto_eh_personality_decl;
}

/* Set the process name based on the LTO mode. */

static void 
lto_process_name (void)
{
  if (flag_lto)
    setproctitle ("lto1-lto");
  if (flag_wpa)
    setproctitle ("lto1-wpa");
  if (flag_ltrans)
    setproctitle ("lto1-ltrans");
}


/* Initialize the LTO front end.  */

static void
lto_init (void)
{
  lto_process_name ();
  lto_streamer_hooks_init ();
  lto_reader_init ();
  lto_set_in_hooks (NULL, get_section_data, free_section_data);
  memset (&lto_stats, 0, sizeof (lto_stats));
  bitmap_obstack_initialize (NULL);
  gimple_register_cfg_hooks ();
}


/* Main entry point for the GIMPLE front end.  This front end has
   three main personalities:

   - LTO (-flto).  All the object files on the command line are
     loaded in memory and processed as a single translation unit.
     This is the traditional link-time optimization behavior.

   - WPA (-fwpa).  Only the callgraph and summary information for
     files in the command file are loaded.  A single callgraph
     (without function bodies) is instantiated for the whole set of
     files.  IPA passes are only allowed to analyze the call graph
     and make transformation decisions.  The callgraph is
     partitioned, each partition is written to a new object file
     together with the transformation decisions.

   - LTRANS (-fltrans).  Similar to -flto but it prevents the IPA
     summary files from running again.  Since WPA computed summary
     information and decided what transformations to apply, LTRANS
     simply applies them.  */

void
lto_main (void)
{
  /* LTO is called as a front end, even though it is not a front end.
     Because it is called as a front end, TV_PHASE_PARSING and
     TV_PARSE_GLOBAL are active, and we need to turn them off while
     doing LTO.  Later we turn them back on so they are active up in
     toplev.c.  */
  timevar_pop (TV_PARSE_GLOBAL);
  timevar_stop (TV_PHASE_PARSING);

  timevar_start (TV_PHASE_SETUP);

  /* Initialize the LTO front end.  */
  lto_init ();

  timevar_stop (TV_PHASE_SETUP);
  timevar_start (TV_PHASE_STREAM_IN);

  /* Read all the symbols and call graph from all the files in the
     command line.  */
  read_cgraph_and_symbols (num_in_fnames, in_fnames);

  timevar_stop (TV_PHASE_STREAM_IN);

  if (!seen_error ())
    {
      /* If WPA is enabled analyze the whole call graph and create an
	 optimization plan.  Otherwise, read in all the function
	 bodies and continue with optimization.  */
      if (flag_wpa)
	do_whole_program_analysis ();
      else
	{
	  struct varpool_node *vnode;

	  timevar_start (TV_PHASE_OPT_GEN);

	  materialize_cgraph ();
	  if (!flag_ltrans)
	    lto_promote_statics_nonwpa ();

	  /* Let the middle end know that we have read and merged all of
	     the input files.  */ 
	  compile ();

	  timevar_stop (TV_PHASE_OPT_GEN);

	  /* FIXME lto, if the processes spawned by WPA fail, we miss
	     the chance to print WPA's report, so WPA will call
	     print_lto_report before launching LTRANS.  If LTRANS was
	     launched directly by the driver we would not need to do
	     this.  */
	  if (flag_lto_report || (flag_wpa && flag_lto_report_wpa))
	    print_lto_report_1 ();

	  /* Record the global variables.  */
	  FOR_EACH_DEFINED_VARIABLE (vnode)
	    vec_safe_push (lto_global_var_decls, vnode->decl);
	}
    }

  /* Here we make LTO pretend to be a parser.  */
  timevar_start (TV_PHASE_PARSING);
  timevar_push (TV_PARSE_GLOBAL);
}

#include "gt-lto-lto.h"
