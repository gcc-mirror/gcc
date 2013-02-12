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
#include "tree-flow.h"
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
  struct lto_file_decl_data *file_data;
  const char *data, *name;
  size_t len;

  decl = node->symbol.decl;
  /* Read in functions with body (analyzed nodes)
     and also functions that are needed to produce virtual clones.  */
  if (cgraph_function_with_gimple_body_p (node) || has_analyzed_clone_p (node))
    {
      /* Clones don't need to be read.  */
      if (node->clone_of)
	return;

      /* Load the function body only if not operating in WPA mode.  In
	 WPA mode, the body of the function is not needed.  */
      if (!flag_wpa)
	{
	  file_data = node->symbol.lto_file_data;
	  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

	  /* We may have renamed the declaration, e.g., a static function.  */
	  name = lto_get_decl_name_mapping (file_data, name);

	  data = lto_get_section_data (file_data, LTO_section_function_body,
				       name, &len);
	  if (!data)
	    fatal_error ("%s: section %s is missing",
			 file_data->file_name,
			 name);

	  gcc_assert (DECL_STRUCT_FUNCTION (decl) == NULL);

	  push_struct_function (decl);
	  announce_function (decl);
	  lto_input_function_body (file_data, decl, data);
	  if (DECL_FUNCTION_PERSONALITY (decl) && !first_personality_decl)
	    first_personality_decl = DECL_FUNCTION_PERSONALITY (decl);
	  lto_stats.num_function_bodies++;
	  lto_free_section_data (file_data, LTO_section_function_body, name,
				 data, len);
	  pop_cfun ();
	  ggc_collect ();
	}
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
  decl = streamer_tree_cache_get (data_in->reader_cache, ix);
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
	decls[j] = streamer_tree_cache_get (data_in->reader_cache, data[j]);

      state->streams[i].size = size;
      state->streams[i].trees = decls;
      data += size;
    }

  return data;
}



/* Global type table.  FIXME, it should be possible to re-use some
   of the type hashing routines in tree.c (type_hash_canon, type_hash_lookup,
   etc), but those assume that types were built with the various
   build_*_type routines which is not the case with the streamer.  */
static GTY((if_marked ("ggc_marked_p"), param_is (union tree_node)))
  htab_t gimple_types;
static GTY((if_marked ("tree_int_map_marked_p"), param_is (struct tree_int_map)))
  htab_t type_hash_cache;

static hashval_t gimple_type_hash (const void *);

/* Structure used to maintain a cache of some type pairs compared by
   gimple_types_compatible_p when comparing aggregate types.  There are
   three possible values for SAME_P:

   	-2: The pair (T1, T2) has just been inserted in the table.
	 0: T1 and T2 are different types.
	 1: T1 and T2 are the same type.  */

struct type_pair_d
{
  unsigned int uid1;
  unsigned int uid2;
  signed char same_p;
};
typedef struct type_pair_d *type_pair_t;

#define GIMPLE_TYPE_PAIR_SIZE 16381
struct type_pair_d *type_pair_cache;


/* Lookup the pair of types T1 and T2 in *VISITED_P.  Insert a new
   entry if none existed.  */

static inline type_pair_t
lookup_type_pair (tree t1, tree t2)
{
  unsigned int index;
  unsigned int uid1, uid2;

  if (TYPE_UID (t1) < TYPE_UID (t2))
    {
      uid1 = TYPE_UID (t1);
      uid2 = TYPE_UID (t2);
    }
  else
    {
      uid1 = TYPE_UID (t2);
      uid2 = TYPE_UID (t1);
    }
  gcc_checking_assert (uid1 != uid2);

  /* iterative_hash_hashval_t imply an function calls.
     We know that UIDS are in limited range.  */
  index = ((((unsigned HOST_WIDE_INT)uid1 << HOST_BITS_PER_WIDE_INT / 2) + uid2)
	   % GIMPLE_TYPE_PAIR_SIZE);
  if (type_pair_cache [index].uid1 == uid1
      && type_pair_cache [index].uid2 == uid2)
    return &type_pair_cache[index];

  type_pair_cache [index].uid1 = uid1;
  type_pair_cache [index].uid2 = uid2;
  type_pair_cache [index].same_p = -2;

  return &type_pair_cache[index];
}

/* Per pointer state for the SCC finding.  The on_sccstack flag
   is not strictly required, it is true when there is no hash value
   recorded for the type and false otherwise.  But querying that
   is slower.  */

struct sccs
{
  unsigned int dfsnum;
  unsigned int low;
  bool on_sccstack;
  union {
    hashval_t hash;
    signed char same_p;
  } u;
};

static unsigned int next_dfs_num;
static unsigned int gtc_next_dfs_num;

/* GIMPLE type merging cache.  A direct-mapped cache based on TYPE_UID.  */

typedef struct GTY(()) gimple_type_leader_entry_s {
  tree type;
  tree leader;
} gimple_type_leader_entry;

#define GIMPLE_TYPE_LEADER_SIZE 16381
static GTY((length("GIMPLE_TYPE_LEADER_SIZE")))
  gimple_type_leader_entry *gimple_type_leader;

/* Lookup an existing leader for T and return it or NULL_TREE, if
   there is none in the cache.  */

static inline tree
gimple_lookup_type_leader (tree t)
{
  gimple_type_leader_entry *leader;

  leader = &gimple_type_leader[TYPE_UID (t) % GIMPLE_TYPE_LEADER_SIZE];
  if (leader->type != t)
    return NULL_TREE;

  return leader->leader;
}


/* Return true if T1 and T2 have the same name.  If FOR_COMPLETION_P is
   true then if any type has no name return false, otherwise return
   true if both types have no names.  */

static bool
compare_type_names_p (tree t1, tree t2)
{
  tree name1 = TYPE_NAME (t1);
  tree name2 = TYPE_NAME (t2);

  if ((name1 != NULL_TREE) != (name2 != NULL_TREE))
    return false;

  if (name1 == NULL_TREE)
    return true;

  /* Either both should be a TYPE_DECL or both an IDENTIFIER_NODE.  */
  if (TREE_CODE (name1) != TREE_CODE (name2))
    return false;

  if (TREE_CODE (name1) == TYPE_DECL)
    name1 = DECL_NAME (name1);
  gcc_checking_assert (!name1 || TREE_CODE (name1) == IDENTIFIER_NODE);

  if (TREE_CODE (name2) == TYPE_DECL)
    name2 = DECL_NAME (name2);
  gcc_checking_assert (!name2 || TREE_CODE (name2) == IDENTIFIER_NODE);

  /* Identifiers can be compared with pointer equality rather
     than a string comparison.  */
  if (name1 == name2)
    return true;

  return false;
}

static bool
gimple_types_compatible_p_1 (tree, tree, type_pair_t,
			     vec<type_pair_t> *,
			     struct pointer_map_t *, struct obstack *);

/* DFS visit the edge from the callers type pair with state *STATE to
   the pair T1, T2 while operating in FOR_MERGING_P mode.
   Update the merging status if it is not part of the SCC containing the
   callers pair and return it.
   SCCSTACK, SCCSTATE and SCCSTATE_OBSTACK are state for the DFS walk done.  */

static bool
gtc_visit (tree t1, tree t2,
	   struct sccs *state,
	   vec<type_pair_t> *sccstack,
	   struct pointer_map_t *sccstate,
	   struct obstack *sccstate_obstack)
{
  struct sccs *cstate = NULL;
  type_pair_t p;
  void **slot;
  tree leader1, leader2;

  /* Check first for the obvious case of pointer identity.  */
  if (t1 == t2)
    return true;

  /* Check that we have two types to compare.  */
  if (t1 == NULL_TREE || t2 == NULL_TREE)
    return false;

  /* Can't be the same type if the types don't have the same code.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return false;

  /* Can't be the same type if they have different CV qualifiers.  */
  if (TYPE_QUALS (t1) != TYPE_QUALS (t2))
    return false;

  if (TREE_ADDRESSABLE (t1) != TREE_ADDRESSABLE (t2))
    return false;

  /* Void types and nullptr types are always the same.  */
  if (TREE_CODE (t1) == VOID_TYPE
      || TREE_CODE (t1) == NULLPTR_TYPE)
    return true;

  /* Can't be the same type if they have different alignment or mode.  */
  if (TYPE_ALIGN (t1) != TYPE_ALIGN (t2)
      || TYPE_MODE (t1) != TYPE_MODE (t2))
    return false;

  /* Do some simple checks before doing three hashtable queries.  */
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

      /* That's all we need to check for float and fixed-point types.  */
      if (SCALAR_FLOAT_TYPE_P (t1)
	  || FIXED_POINT_TYPE_P (t1))
	return true;

      /* For other types fall through to more complex checks.  */
    }

  /* If the types have been previously registered and found equal
     they still are.  */
  leader1 = gimple_lookup_type_leader (t1);
  leader2 = gimple_lookup_type_leader (t2);
  if (leader1 == t2
      || t1 == leader2
      || (leader1 && leader1 == leader2))
    return true;

  /* If the hash values of t1 and t2 are different the types can't
     possibly be the same.  This helps keeping the type-pair hashtable
     small, only tracking comparisons for hash collisions.  */
  if (gimple_type_hash (t1) != gimple_type_hash (t2))
    return false;

  /* Allocate a new cache entry for this comparison.  */
  p = lookup_type_pair (t1, t2);
  if (p->same_p == 0 || p->same_p == 1)
    {
      /* We have already decided whether T1 and T2 are the
	 same, return the cached result.  */
      return p->same_p == 1;
    }

  if ((slot = pointer_map_contains (sccstate, p)) != NULL)
    cstate = (struct sccs *)*slot;
  /* Not yet visited.  DFS recurse.  */
  if (!cstate)
    {
      gimple_types_compatible_p_1 (t1, t2, p,
				   sccstack, sccstate, sccstate_obstack);
      cstate = (struct sccs *)* pointer_map_contains (sccstate, p);
      state->low = MIN (state->low, cstate->low);
    }
  /* If the type is still on the SCC stack adjust the parents low.  */
  if (cstate->dfsnum < state->dfsnum
      && cstate->on_sccstack)
    state->low = MIN (cstate->dfsnum, state->low);

  /* Return the current lattice value.  We start with an equality
     assumption so types part of a SCC will be optimistically
     treated equal unless proven otherwise.  */
  return cstate->u.same_p;
}

/* Worker for gimple_types_compatible.
   SCCSTACK, SCCSTATE and SCCSTATE_OBSTACK are state for the DFS walk done.  */

static bool
gimple_types_compatible_p_1 (tree t1, tree t2, type_pair_t p,
			     vec<type_pair_t> *sccstack,
			     struct pointer_map_t *sccstate,
			     struct obstack *sccstate_obstack)
{
  struct sccs *state;

  gcc_assert (p->same_p == -2);

  state = XOBNEW (sccstate_obstack, struct sccs);
  *pointer_map_insert (sccstate, p) = state;

  sccstack->safe_push (p);
  state->dfsnum = gtc_next_dfs_num++;
  state->low = state->dfsnum;
  state->on_sccstack = true;
  /* Start with an equality assumption.  As we DFS recurse into child
     SCCs this assumption may get revisited.  */
  state->u.same_p = 1;

  /* The struct tags shall compare equal.  */
  if (!compare_type_names_p (t1, t2))
    goto different_types;

  /* The main variant of both types should compare equal.  */
  if (TYPE_MAIN_VARIANT (t1) != t1
      || TYPE_MAIN_VARIANT (t2) != t2)
    {
      if (!gtc_visit (TYPE_MAIN_VARIANT (t1), TYPE_MAIN_VARIANT (t2),
		      state, sccstack, sccstate, sccstate_obstack))
	goto different_types;
    }

  /* We may not merge typedef types to the same type in different
     contexts.  */
  if (TYPE_NAME (t1)
      && TREE_CODE (TYPE_NAME (t1)) == TYPE_DECL
      && DECL_CONTEXT (TYPE_NAME (t1))
      && TYPE_P (DECL_CONTEXT (TYPE_NAME (t1))))
    {
      if (!gtc_visit (DECL_CONTEXT (TYPE_NAME (t1)),
		      DECL_CONTEXT (TYPE_NAME (t2)),
		      state, sccstack, sccstate, sccstate_obstack))
	goto different_types;
    }

  /* If their attributes are not the same they can't be the same type.  */
  if (!attribute_list_equal (TYPE_ATTRIBUTES (t1), TYPE_ATTRIBUTES (t2)))
    goto different_types;

  /* Do type-specific comparisons.  */
  switch (TREE_CODE (t1))
    {
    case VECTOR_TYPE:
    case COMPLEX_TYPE:
      if (!gtc_visit (TREE_TYPE (t1), TREE_TYPE (t2),
		      state, sccstack, sccstate, sccstate_obstack))
	goto different_types;
      goto same_types;

    case ARRAY_TYPE:
      /* Array types are the same if the element types are the same and
	 the number of elements are the same.  */
      if (!gtc_visit (TREE_TYPE (t1), TREE_TYPE (t2),
		      state, sccstack, sccstate, sccstate_obstack)
	  || TYPE_STRING_FLAG (t1) != TYPE_STRING_FLAG (t2)
	  || TYPE_NONALIASED_COMPONENT (t1) != TYPE_NONALIASED_COMPONENT (t2))
	goto different_types;
      else
	{
	  tree i1 = TYPE_DOMAIN (t1);
	  tree i2 = TYPE_DOMAIN (t2);

	  /* For an incomplete external array, the type domain can be
 	     NULL_TREE.  Check this condition also.  */
	  if (i1 == NULL_TREE && i2 == NULL_TREE)
	    goto same_types;
	  else if (i1 == NULL_TREE || i2 == NULL_TREE)
	    goto different_types;
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
		goto same_types;
	      else
		goto different_types;
	    }
	}

    case METHOD_TYPE:
      /* Method types should belong to the same class.  */
      if (!gtc_visit (TYPE_METHOD_BASETYPE (t1), TYPE_METHOD_BASETYPE (t2),
		      state, sccstack, sccstate, sccstate_obstack))
	goto different_types;

      /* Fallthru  */

    case FUNCTION_TYPE:
      /* Function types are the same if the return type and arguments types
	 are the same.  */
      if (!gtc_visit (TREE_TYPE (t1), TREE_TYPE (t2),
		      state, sccstack, sccstate, sccstate_obstack))
	goto different_types;

      if (!comp_type_attributes (t1, t2))
	goto different_types;

      if (TYPE_ARG_TYPES (t1) == TYPE_ARG_TYPES (t2))
	goto same_types;
      else
	{
	  tree parms1, parms2;

	  for (parms1 = TYPE_ARG_TYPES (t1), parms2 = TYPE_ARG_TYPES (t2);
	       parms1 && parms2;
	       parms1 = TREE_CHAIN (parms1), parms2 = TREE_CHAIN (parms2))
	    {
	      if (!gtc_visit (TREE_VALUE (parms1), TREE_VALUE (parms2),
			      state, sccstack, sccstate, sccstate_obstack))
		goto different_types;
	    }

	  if (parms1 || parms2)
	    goto different_types;

	  goto same_types;
	}

    case OFFSET_TYPE:
      {
	if (!gtc_visit (TREE_TYPE (t1), TREE_TYPE (t2),
			state, sccstack, sccstate, sccstate_obstack)
	    || !gtc_visit (TYPE_OFFSET_BASETYPE (t1),
			   TYPE_OFFSET_BASETYPE (t2),
			   state, sccstack, sccstate, sccstate_obstack))
	  goto different_types;

	goto same_types;
      }

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
	/* If the two pointers have different ref-all attributes,
	   they can't be the same type.  */
	if (TYPE_REF_CAN_ALIAS_ALL (t1) != TYPE_REF_CAN_ALIAS_ALL (t2))
	  goto different_types;

	/* Otherwise, pointer and reference types are the same if the
	   pointed-to types are the same.  */
	if (gtc_visit (TREE_TYPE (t1), TREE_TYPE (t2),
		       state, sccstack, sccstate, sccstate_obstack))
	  goto same_types;

	goto different_types;
      }

    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
      {
	tree min1 = TYPE_MIN_VALUE (t1);
	tree max1 = TYPE_MAX_VALUE (t1);
	tree min2 = TYPE_MIN_VALUE (t2);
	tree max2 = TYPE_MAX_VALUE (t2);
	bool min_equal_p = false;
	bool max_equal_p = false;

	/* If either type has a minimum value, the other type must
	   have the same.  */
	if (min1 == NULL_TREE && min2 == NULL_TREE)
	  min_equal_p = true;
	else if (min1 && min2 && operand_equal_p (min1, min2, 0))
	  min_equal_p = true;

	/* Likewise, if either type has a maximum value, the other
	   type must have the same.  */
	if (max1 == NULL_TREE && max2 == NULL_TREE)
	  max_equal_p = true;
	else if (max1 && max2 && operand_equal_p (max1, max2, 0))
	  max_equal_p = true;

	if (!min_equal_p || !max_equal_p)
	  goto different_types;

	goto same_types;
      }

    case ENUMERAL_TYPE:
      {
	/* FIXME lto, we cannot check bounds on enumeral types because
	   different front ends will produce different values.
	   In C, enumeral types are integers, while in C++ each element
	   will have its own symbolic value.  We should decide how enums
	   are to be represented in GIMPLE and have each front end lower
	   to that.  */
	tree v1, v2;

	/* For enumeral types, all the values must be the same.  */
	if (TYPE_VALUES (t1) == TYPE_VALUES (t2))
	  goto same_types;

	for (v1 = TYPE_VALUES (t1), v2 = TYPE_VALUES (t2);
	     v1 && v2;
	     v1 = TREE_CHAIN (v1), v2 = TREE_CHAIN (v2))
	  {
	    tree c1 = TREE_VALUE (v1);
	    tree c2 = TREE_VALUE (v2);

	    if (TREE_CODE (c1) == CONST_DECL)
	      c1 = DECL_INITIAL (c1);

	    if (TREE_CODE (c2) == CONST_DECL)
	      c2 = DECL_INITIAL (c2);

	    if (tree_int_cst_equal (c1, c2) != 1)
	      goto different_types;

	    if (TREE_PURPOSE (v1) != TREE_PURPOSE (v2))
	      goto different_types;
	  }

	/* If one enumeration has more values than the other, they
	   are not the same.  */
	if (v1 || v2)
	  goto different_types;

	goto same_types;
      }

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree f1, f2;

	/* For aggregate types, all the fields must be the same.  */
	for (f1 = TYPE_FIELDS (t1), f2 = TYPE_FIELDS (t2);
	     f1 && f2;
	     f1 = TREE_CHAIN (f1), f2 = TREE_CHAIN (f2))
	  {
	    /* Different field kinds are not compatible.  */
	    if (TREE_CODE (f1) != TREE_CODE (f2))
	      goto different_types;
	    /* Field decls must have the same name and offset.  */
	    if (TREE_CODE (f1) == FIELD_DECL
		&& (DECL_NONADDRESSABLE_P (f1) != DECL_NONADDRESSABLE_P (f2)
		    || !gimple_compare_field_offset (f1, f2)))
	      goto different_types;
	    /* All entities should have the same name and type.  */
	    if (DECL_NAME (f1) != DECL_NAME (f2)
		|| !gtc_visit (TREE_TYPE (f1), TREE_TYPE (f2),
			       state, sccstack, sccstate, sccstate_obstack))
	      goto different_types;
	  }

	/* If one aggregate has more fields than the other, they
	   are not the same.  */
	if (f1 || f2)
	  goto different_types;

	goto same_types;
      }

    default:
      gcc_unreachable ();
    }

  /* Common exit path for types that are not compatible.  */
different_types:
  state->u.same_p = 0;
  goto pop;

  /* Common exit path for types that are compatible.  */
same_types:
  gcc_assert (state->u.same_p == 1);

pop:
  if (state->low == state->dfsnum)
    {
      type_pair_t x;

      /* Pop off the SCC and set its cache values to the final
         comparison result.  */
      do
	{
	  struct sccs *cstate;
	  x = sccstack->pop ();
	  cstate = (struct sccs *)*pointer_map_contains (sccstate, x);
	  cstate->on_sccstack = false;
	  x->same_p = state->u.same_p;
	}
      while (x != p);
    }

  return state->u.same_p;
}

/* Return true iff T1 and T2 are structurally identical.  When
   FOR_MERGING_P is true the an incomplete type and a complete type
   are considered different, otherwise they are considered compatible.  */

static bool
gimple_types_compatible_p (tree t1, tree t2)
{
  vec<type_pair_t> sccstack = vNULL;
  struct pointer_map_t *sccstate;
  struct obstack sccstate_obstack;
  type_pair_t p = NULL;
  bool res;
  tree leader1, leader2;

  /* Before starting to set up the SCC machinery handle simple cases.  */

  /* Check first for the obvious case of pointer identity.  */
  if (t1 == t2)
    return true;

  /* Check that we have two types to compare.  */
  if (t1 == NULL_TREE || t2 == NULL_TREE)
    return false;

  /* Can't be the same type if the types don't have the same code.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return false;

  /* Can't be the same type if they have different CV qualifiers.  */
  if (TYPE_QUALS (t1) != TYPE_QUALS (t2))
    return false;

  if (TREE_ADDRESSABLE (t1) != TREE_ADDRESSABLE (t2))
    return false;

  /* Void types and nullptr types are always the same.  */
  if (TREE_CODE (t1) == VOID_TYPE
      || TREE_CODE (t1) == NULLPTR_TYPE)
    return true;

  /* Can't be the same type if they have different alignment or mode.  */
  if (TYPE_ALIGN (t1) != TYPE_ALIGN (t2)
      || TYPE_MODE (t1) != TYPE_MODE (t2))
    return false;

  /* Do some simple checks before doing three hashtable queries.  */
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

      /* That's all we need to check for float and fixed-point types.  */
      if (SCALAR_FLOAT_TYPE_P (t1)
	  || FIXED_POINT_TYPE_P (t1))
	return true;

      /* For other types fall through to more complex checks.  */
    }

  /* If the types have been previously registered and found equal
     they still are.  */
  leader1 = gimple_lookup_type_leader (t1);
  leader2 = gimple_lookup_type_leader (t2);
  if (leader1 == t2
      || t1 == leader2
      || (leader1 && leader1 == leader2))
    return true;

  /* If the hash values of t1 and t2 are different the types can't
     possibly be the same.  This helps keeping the type-pair hashtable
     small, only tracking comparisons for hash collisions.  */
  if (gimple_type_hash (t1) != gimple_type_hash (t2))
    return false;

  /* If we've visited this type pair before (in the case of aggregates
     with self-referential types), and we made a decision, return it.  */
  p = lookup_type_pair (t1, t2);
  if (p->same_p == 0 || p->same_p == 1)
    {
      /* We have already decided whether T1 and T2 are the
	 same, return the cached result.  */
      return p->same_p == 1;
    }

  /* Now set up the SCC machinery for the comparison.  */
  gtc_next_dfs_num = 1;
  sccstate = pointer_map_create ();
  gcc_obstack_init (&sccstate_obstack);
  res = gimple_types_compatible_p_1 (t1, t2, p,
				     &sccstack, sccstate, &sccstate_obstack);
  sccstack.release ();
  pointer_map_destroy (sccstate);
  obstack_free (&sccstate_obstack, NULL);

  return res;
}

static hashval_t
iterative_hash_gimple_type (tree, hashval_t, vec<tree> *,
			    struct pointer_map_t *, struct obstack *);

/* DFS visit the edge from the callers type with state *STATE to T.
   Update the callers type hash V with the hash for T if it is not part
   of the SCC containing the callers type and return it.
   SCCSTACK, SCCSTATE and SCCSTATE_OBSTACK are state for the DFS walk done.  */

static hashval_t
visit (tree t, struct sccs *state, hashval_t v,
       vec<tree> *sccstack,
       struct pointer_map_t *sccstate,
       struct obstack *sccstate_obstack)
{
  struct sccs *cstate = NULL;
  struct tree_int_map m;
  void **slot;

  /* If there is a hash value recorded for this type then it can't
     possibly be part of our parent SCC.  Simply mix in its hash.  */
  m.base.from = t;
  if ((slot = htab_find_slot (type_hash_cache, &m, NO_INSERT))
      && *slot)
    return iterative_hash_hashval_t (((struct tree_int_map *) *slot)->to, v);

  if ((slot = pointer_map_contains (sccstate, t)) != NULL)
    cstate = (struct sccs *)*slot;
  if (!cstate)
    {
      hashval_t tem;
      /* Not yet visited.  DFS recurse.  */
      tem = iterative_hash_gimple_type (t, v,
					sccstack, sccstate, sccstate_obstack);
      if (!cstate)
	cstate = (struct sccs *)* pointer_map_contains (sccstate, t);
      state->low = MIN (state->low, cstate->low);
      /* If the type is no longer on the SCC stack and thus is not part
         of the parents SCC mix in its hash value.  Otherwise we will
	 ignore the type for hashing purposes and return the unaltered
	 hash value.  */
      if (!cstate->on_sccstack)
	return tem;
    }
  if (cstate->dfsnum < state->dfsnum
      && cstate->on_sccstack)
    state->low = MIN (cstate->dfsnum, state->low);

  /* We are part of our parents SCC, skip this type during hashing
     and return the unaltered hash value.  */
  return v;
}

/* Hash NAME with the previous hash value V and return it.  */

static hashval_t
iterative_hash_name (tree name, hashval_t v)
{
  if (!name)
    return v;
  v = iterative_hash_hashval_t (TREE_CODE (name), v);
  if (TREE_CODE (name) == TYPE_DECL)
    name = DECL_NAME (name);
  if (!name)
    return v;
  gcc_assert (TREE_CODE (name) == IDENTIFIER_NODE);
  return iterative_hash_object (IDENTIFIER_HASH_VALUE (name), v);
}

/* A type, hashvalue pair for sorting SCC members.  */

struct type_hash_pair {
  tree type;
  hashval_t hash;
};

/* Compare two type, hashvalue pairs.  */

static int
type_hash_pair_compare (const void *p1_, const void *p2_)
{
  const struct type_hash_pair *p1 = (const struct type_hash_pair *) p1_;
  const struct type_hash_pair *p2 = (const struct type_hash_pair *) p2_;
  if (p1->hash < p2->hash)
    return -1;
  else if (p1->hash > p2->hash)
    return 1;
  return 0;
}

/* Returning a hash value for gimple type TYPE combined with VAL.
   SCCSTACK, SCCSTATE and SCCSTATE_OBSTACK are state for the DFS walk done.

   To hash a type we end up hashing in types that are reachable.
   Through pointers we can end up with cycles which messes up the
   required property that we need to compute the same hash value
   for structurally equivalent types.  To avoid this we have to
   hash all types in a cycle (the SCC) in a commutative way.  The
   easiest way is to not mix in the hashes of the SCC members at
   all.  To make this work we have to delay setting the hash
   values of the SCC until it is complete.  */

static hashval_t
iterative_hash_gimple_type (tree type, hashval_t val,
			    vec<tree> *sccstack,
			    struct pointer_map_t *sccstate,
			    struct obstack *sccstate_obstack)
{
  hashval_t v;
  void **slot;
  struct sccs *state;

  /* Not visited during this DFS walk.  */
  gcc_checking_assert (!pointer_map_contains (sccstate, type));
  state = XOBNEW (sccstate_obstack, struct sccs);
  *pointer_map_insert (sccstate, type) = state;

  sccstack->safe_push (type);
  state->dfsnum = next_dfs_num++;
  state->low = state->dfsnum;
  state->on_sccstack = true;

  /* Combine a few common features of types so that types are grouped into
     smaller sets; when searching for existing matching types to merge,
     only existing types having the same features as the new type will be
     checked.  */
  v = iterative_hash_name (TYPE_NAME (type), 0);
  if (TYPE_NAME (type)
      && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
      && DECL_CONTEXT (TYPE_NAME (type))
      && TYPE_P (DECL_CONTEXT (TYPE_NAME (type))))
    v = visit (DECL_CONTEXT (TYPE_NAME (type)), state, v,
	       sccstack, sccstate, sccstate_obstack);

  /* Factor in the variant structure.  */
  if (TYPE_MAIN_VARIANT (type) != type)
    v = visit (TYPE_MAIN_VARIANT (type), state, v,
	       sccstack, sccstate, sccstate_obstack);

  v = iterative_hash_hashval_t (TREE_CODE (type), v);
  v = iterative_hash_hashval_t (TYPE_QUALS (type), v);
  v = iterative_hash_hashval_t (TREE_ADDRESSABLE (type), v);

  /* Do not hash the types size as this will cause differences in
     hash values for the complete vs. the incomplete type variant.  */

  /* Incorporate common features of numerical types.  */
  if (INTEGRAL_TYPE_P (type)
      || SCALAR_FLOAT_TYPE_P (type)
      || FIXED_POINT_TYPE_P (type))
    {
      v = iterative_hash_hashval_t (TYPE_PRECISION (type), v);
      v = iterative_hash_hashval_t (TYPE_MODE (type), v);
      v = iterative_hash_hashval_t (TYPE_UNSIGNED (type), v);
    }

  /* For pointer and reference types, fold in information about the type
     pointed to.  */
  if (POINTER_TYPE_P (type))
    v = visit (TREE_TYPE (type), state, v,
	       sccstack, sccstate, sccstate_obstack);

  /* For integer types hash the types min/max values and the string flag.  */
  if (TREE_CODE (type) == INTEGER_TYPE)
    {
      /* OMP lowering can introduce error_mark_node in place of
	 random local decls in types.  */
      if (TYPE_MIN_VALUE (type) != error_mark_node)
	v = iterative_hash_expr (TYPE_MIN_VALUE (type), v);
      if (TYPE_MAX_VALUE (type) != error_mark_node)
	v = iterative_hash_expr (TYPE_MAX_VALUE (type), v);
      v = iterative_hash_hashval_t (TYPE_STRING_FLAG (type), v);
    }

  /* For array types hash the domain and the string flag.  */
  if (TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type))
    {
      v = iterative_hash_hashval_t (TYPE_STRING_FLAG (type), v);
      v = visit (TYPE_DOMAIN (type), state, v,
		 sccstack, sccstate, sccstate_obstack);
    }

  /* Recurse for aggregates with a single element type.  */
  if (TREE_CODE (type) == ARRAY_TYPE
      || TREE_CODE (type) == COMPLEX_TYPE
      || TREE_CODE (type) == VECTOR_TYPE)
    v = visit (TREE_TYPE (type), state, v,
	       sccstack, sccstate, sccstate_obstack);

  /* Incorporate function return and argument types.  */
  if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
    {
      unsigned na;
      tree p;

      /* For method types also incorporate their parent class.  */
      if (TREE_CODE (type) == METHOD_TYPE)
	v = visit (TYPE_METHOD_BASETYPE (type), state, v,
		   sccstack, sccstate, sccstate_obstack);

      /* Check result and argument types.  */
      v = visit (TREE_TYPE (type), state, v,
		 sccstack, sccstate, sccstate_obstack);
      for (p = TYPE_ARG_TYPES (type), na = 0; p; p = TREE_CHAIN (p))
	{
	  v = visit (TREE_VALUE (p), state, v,
		     sccstack, sccstate, sccstate_obstack);
	  na++;
	}

      v = iterative_hash_hashval_t (na, v);
    }

  if (RECORD_OR_UNION_TYPE_P (type))
    {
      unsigned nf;
      tree f;

      for (f = TYPE_FIELDS (type), nf = 0; f; f = TREE_CHAIN (f))
	{
	  v = iterative_hash_name (DECL_NAME (f), v);
	  v = visit (TREE_TYPE (f), state, v,
		     sccstack, sccstate, sccstate_obstack);
	  nf++;
	}

      v = iterative_hash_hashval_t (nf, v);
    }

  /* Record hash for us.  */
  state->u.hash = v;

  /* See if we found an SCC.  */
  if (state->low == state->dfsnum)
    {
      tree x;
      struct tree_int_map *m;

      /* Pop off the SCC and set its hash values.  */
      x = sccstack->pop ();
      /* Optimize SCC size one.  */
      if (x == type)
	{
	  state->on_sccstack = false;
	  m = ggc_alloc_cleared_tree_int_map ();
	  m->base.from = x;
	  m->to = v;
	  slot = htab_find_slot (type_hash_cache, m, INSERT);
	  gcc_assert (!*slot);
	  *slot = (void *) m;
	}
      else
	{
	  struct sccs *cstate;
	  unsigned first, i, size, j;
	  struct type_hash_pair *pairs;
	  /* Pop off the SCC and build an array of type, hash pairs.  */
	  first = sccstack->length () - 1;
	  while ((*sccstack)[first] != type)
	    --first;
	  size = sccstack->length () - first + 1;
	  pairs = XALLOCAVEC (struct type_hash_pair, size);
	  i = 0;
	  cstate = (struct sccs *)*pointer_map_contains (sccstate, x);
	  cstate->on_sccstack = false;
	  pairs[i].type = x;
	  pairs[i].hash = cstate->u.hash;
	  do
	    {
	      x = sccstack->pop ();
	      cstate = (struct sccs *)*pointer_map_contains (sccstate, x);
	      cstate->on_sccstack = false;
	      ++i;
	      pairs[i].type = x;
	      pairs[i].hash = cstate->u.hash;
	    }
	  while (x != type);
	  gcc_assert (i + 1 == size);
	  /* Sort the arrays of type, hash pairs so that when we mix in
	     all members of the SCC the hash value becomes independent on
	     the order we visited the SCC.  Disregard hashes equal to
	     the hash of the type we mix into because we cannot guarantee
	     a stable sort for those across different TUs.  */
	  qsort (pairs, size, sizeof (struct type_hash_pair),
		 type_hash_pair_compare);
	  for (i = 0; i < size; ++i)
	    {
	      hashval_t hash;
	      m = ggc_alloc_cleared_tree_int_map ();
	      m->base.from = pairs[i].type;
	      hash = pairs[i].hash;
	      /* Skip same hashes.  */
	      for (j = i + 1; j < size && pairs[j].hash == pairs[i].hash; ++j)
		;
	      for (; j < size; ++j)
		hash = iterative_hash_hashval_t (pairs[j].hash, hash);
	      for (j = 0; pairs[j].hash != pairs[i].hash; ++j)
		hash = iterative_hash_hashval_t (pairs[j].hash, hash);
	      m->to = hash;
	      if (pairs[i].type == type)
		v = hash;
	      slot = htab_find_slot (type_hash_cache, m, INSERT);
	      gcc_assert (!*slot);
	      *slot = (void *) m;
	    }
	}
    }

  return iterative_hash_hashval_t (v, val);
}

/* Returns a hash value for P (assumed to be a type).  The hash value
   is computed using some distinguishing features of the type.  Note
   that we cannot use pointer hashing here as we may be dealing with
   two distinct instances of the same type.

   This function should produce the same hash value for two compatible
   types according to gimple_types_compatible_p.  */

static hashval_t
gimple_type_hash (const void *p)
{
  const_tree t = (const_tree) p;
  vec<tree> sccstack = vNULL;
  struct pointer_map_t *sccstate;
  struct obstack sccstate_obstack;
  hashval_t val;
  void **slot;
  struct tree_int_map m;

  m.base.from = CONST_CAST_TREE (t);
  if ((slot = htab_find_slot (type_hash_cache, &m, NO_INSERT))
      && *slot)
    return iterative_hash_hashval_t (((struct tree_int_map *) *slot)->to, 0);

  /* Perform a DFS walk and pre-hash all reachable types.  */
  next_dfs_num = 1;
  sccstate = pointer_map_create ();
  gcc_obstack_init (&sccstate_obstack);
  val = iterative_hash_gimple_type (CONST_CAST_TREE (t), 0,
				    &sccstack, sccstate, &sccstate_obstack);
  sccstack.release ();
  pointer_map_destroy (sccstate);
  obstack_free (&sccstate_obstack, NULL);

  return val;
}

/* Returns nonzero if P1 and P2 are equal.  */

static int
gimple_type_eq (const void *p1, const void *p2)
{
  const_tree t1 = (const_tree) p1;
  const_tree t2 = (const_tree) p2;
  return gimple_types_compatible_p (CONST_CAST_TREE (t1),
				    CONST_CAST_TREE (t2));
}


/* Worker for gimple_register_type.
   Register type T in the global type table gimple_types.
   When REGISTERING_MV is false first recurse for the main variant of T.  */

static tree
gimple_register_type_1 (tree t, bool registering_mv)
{
  void **slot;
  gimple_type_leader_entry *leader;

  /* If we registered this type before return the cached result.  */
  leader = &gimple_type_leader[TYPE_UID (t) % GIMPLE_TYPE_LEADER_SIZE];
  if (leader->type == t)
    return leader->leader;

  /* Always register the main variant first.  This is important so we
     pick up the non-typedef variants as canonical, otherwise we'll end
     up taking typedef ids for structure tags during comparison.
     It also makes sure that main variants will be merged to main variants.
     As we are operating on a possibly partially fixed up type graph
     do not bother to recurse more than once, otherwise we may end up
     walking in circles.
     If we are registering a main variant it will either remain its
     own main variant or it will be merged to something else in which
     case we do not care for the main variant leader.  */
  if (!registering_mv
      && TYPE_MAIN_VARIANT (t) != t)
    gimple_register_type_1 (TYPE_MAIN_VARIANT (t), true);

  /* See if we already have an equivalent type registered.  */
  slot = htab_find_slot (gimple_types, t, INSERT);
  if (*slot
      && *(tree *)slot != t)
    {
      tree new_type = (tree) *((tree *) slot);
      leader->type = t;
      leader->leader = new_type;
      return new_type;
    }

  /* If not, insert it to the cache and the hash.  */
  leader->type = t;
  leader->leader = t;
  *slot = (void *) t;
  return t;
}

/* Register type T in the global type table gimple_types.
   If another type T', compatible with T, already existed in
   gimple_types then return T', otherwise return T.  This is used by
   LTO to merge identical types read from different TUs.  */

static tree
gimple_register_type (tree t)
{
  gcc_assert (TYPE_P (t));
  return gimple_register_type_1 (t, false);
}

#define GIMPLE_REGISTER_TYPE(tt) \
   (TREE_VISITED (tt) ? gimple_register_type (tt) : tt)



/* A hashtable of trees that potentially refer to variables or functions
   that must be replaced with their prevailing variant.  */
static GTY((if_marked ("ggc_marked_p"), param_is (union tree_node))) htab_t
  tree_with_vars;

/* Remember that T is a tree that (potentially) refers to a variable
   or function decl that may be replaced with its prevailing variant.  */
static void
remember_with_vars (tree t)
{
  *(tree *) htab_find_slot (tree_with_vars, t, INSERT) = t;
}

#define LTO_FIXUP_TREE(tt) \
  do \
    { \
      if (tt) \
	{ \
	  if (TYPE_P (tt)) \
	    (tt) = GIMPLE_REGISTER_TYPE (tt); \
	  if (VAR_OR_FUNCTION_DECL_P (tt) && TREE_PUBLIC (tt)) \
	    remember_with_vars (t); \
	  if (TREE_CODE (tt) == INTEGER_CST) \
	    (tt) = fixup_integer_cst (tt); \
	} \
    } while (0)

static void lto_fixup_types (tree);

/* Return integer_cst T with updated type.  */

static tree
fixup_integer_cst (tree t)
{
  tree type = GIMPLE_REGISTER_TYPE (TREE_TYPE (t));

  if (type == TREE_TYPE (t))
    return t;

  /* If overflow was set, streamer_read_integer_cst
     produced local copy of T. */
  if (TREE_OVERFLOW (t))
    {
      TREE_TYPE (t) = type;
      return t;
    }
  else
  /* Otherwise produce new shared node for the new type.  */
    return build_int_cst_wide (type, TREE_INT_CST_LOW (t),
			       TREE_INT_CST_HIGH (t));
}

/* Fix up fields of a tree_typed T.  */

static void
lto_ft_typed (tree t)
{
  LTO_FIXUP_TREE (TREE_TYPE (t));
}

/* Fix up fields of a tree_common T.  */

static void
lto_ft_common (tree t)
{
  lto_ft_typed (t);
  LTO_FIXUP_TREE (TREE_CHAIN (t));
}

/* Fix up fields of a decl_minimal T.  */

static void
lto_ft_decl_minimal (tree t)
{
  lto_ft_common (t);
  LTO_FIXUP_TREE (DECL_NAME (t));
  LTO_FIXUP_TREE (DECL_CONTEXT (t));
}

/* Fix up fields of a decl_common T.  */

static void
lto_ft_decl_common (tree t)
{
  lto_ft_decl_minimal (t);
  LTO_FIXUP_TREE (DECL_SIZE (t));
  LTO_FIXUP_TREE (DECL_SIZE_UNIT (t));
  LTO_FIXUP_TREE (DECL_INITIAL (t));
  LTO_FIXUP_TREE (DECL_ATTRIBUTES (t));
  LTO_FIXUP_TREE (DECL_ABSTRACT_ORIGIN (t));
}

/* Fix up fields of a decl_with_vis T.  */

static void
lto_ft_decl_with_vis (tree t)
{
  lto_ft_decl_common (t);

  /* Accessor macro has side-effects, use field-name here. */
  LTO_FIXUP_TREE (t->decl_with_vis.assembler_name);
  LTO_FIXUP_TREE (DECL_SECTION_NAME (t));
}

/* Fix up fields of a decl_non_common T.  */

static void
lto_ft_decl_non_common (tree t)
{
  lto_ft_decl_with_vis (t);
  LTO_FIXUP_TREE (DECL_ARGUMENT_FLD (t));
  LTO_FIXUP_TREE (DECL_RESULT_FLD (t));
  LTO_FIXUP_TREE (DECL_VINDEX (t));
  /* The C frontends may create exact duplicates for DECL_ORIGINAL_TYPE
     like for 'typedef enum foo foo'.  We have no way of avoiding to
     merge them and dwarf2out.c cannot deal with this,
     so fix this up by clearing DECL_ORIGINAL_TYPE in this case.  */
  if (TREE_CODE (t) == TYPE_DECL
      && DECL_ORIGINAL_TYPE (t) == TREE_TYPE (t))
    DECL_ORIGINAL_TYPE (t) = NULL_TREE;
}

/* Fix up fields of a decl_non_common T.  */

static void
lto_ft_function (tree t)
{
  lto_ft_decl_non_common (t);
  LTO_FIXUP_TREE (DECL_FUNCTION_PERSONALITY (t));
}

/* Fix up fields of a field_decl T.  */

static void
lto_ft_field_decl (tree t)
{
  lto_ft_decl_common (t);
  LTO_FIXUP_TREE (DECL_FIELD_OFFSET (t));
  LTO_FIXUP_TREE (DECL_BIT_FIELD_TYPE (t));
  LTO_FIXUP_TREE (DECL_QUALIFIER (t));
  LTO_FIXUP_TREE (DECL_FIELD_BIT_OFFSET (t));
  LTO_FIXUP_TREE (DECL_FCONTEXT (t));
}

/* Fix up fields of a type T.  */

static void
lto_ft_type (tree t)
{
  lto_ft_common (t);
  LTO_FIXUP_TREE (TYPE_CACHED_VALUES (t));
  LTO_FIXUP_TREE (TYPE_SIZE (t));
  LTO_FIXUP_TREE (TYPE_SIZE_UNIT (t));
  LTO_FIXUP_TREE (TYPE_ATTRIBUTES (t));
  LTO_FIXUP_TREE (TYPE_NAME (t));

  /* Accessors are for derived node types only. */
  if (!POINTER_TYPE_P (t))
    LTO_FIXUP_TREE (TYPE_MINVAL (t));
  LTO_FIXUP_TREE (TYPE_MAXVAL (t));

  /* Accessor is for derived node types only. */
  LTO_FIXUP_TREE (t->type_non_common.binfo);

  LTO_FIXUP_TREE (TYPE_CONTEXT (t));
}

/* Fix up fields of a BINFO T.  */

static void
lto_ft_binfo (tree t)
{
  unsigned HOST_WIDE_INT i, n;
  tree base, saved_base;

  lto_ft_common (t);
  LTO_FIXUP_TREE (BINFO_VTABLE (t));
  LTO_FIXUP_TREE (BINFO_OFFSET (t));
  LTO_FIXUP_TREE (BINFO_VIRTUALS (t));
  LTO_FIXUP_TREE (BINFO_VPTR_FIELD (t));
  n = vec_safe_length (BINFO_BASE_ACCESSES (t));
  for (i = 0; i < n; i++)
    {
      saved_base = base = BINFO_BASE_ACCESS (t, i);
      LTO_FIXUP_TREE (base);
      if (base != saved_base)
	(*BINFO_BASE_ACCESSES (t))[i] = base;
    }
  LTO_FIXUP_TREE (BINFO_INHERITANCE_CHAIN (t));
  LTO_FIXUP_TREE (BINFO_SUBVTT_INDEX (t));
  LTO_FIXUP_TREE (BINFO_VPTR_INDEX (t));
  n = BINFO_N_BASE_BINFOS (t);
  for (i = 0; i < n; i++)
    {
      saved_base = base = BINFO_BASE_BINFO (t, i);
      LTO_FIXUP_TREE (base);
      if (base != saved_base)
	(*BINFO_BASE_BINFOS (t))[i] = base;
    }
}

/* Fix up fields of a CONSTRUCTOR T.  */

static void
lto_ft_constructor (tree t)
{
  unsigned HOST_WIDE_INT idx;
  constructor_elt *ce;

  lto_ft_typed (t);

  for (idx = 0; vec_safe_iterate (CONSTRUCTOR_ELTS (t), idx, &ce); idx++)
    {
      LTO_FIXUP_TREE (ce->index);
      LTO_FIXUP_TREE (ce->value);
    }
}

/* Fix up fields of an expression tree T.  */

static void
lto_ft_expr (tree t)
{
  int i;
  lto_ft_typed (t);
  for (i = TREE_OPERAND_LENGTH (t) - 1; i >= 0; --i)
    LTO_FIXUP_TREE (TREE_OPERAND (t, i));
}

/* Given a tree T fixup fields of T by replacing types with their merged
   variant and other entities by an equal entity from an earlier compilation
   unit, or an entity being canonical in a different way.  This includes
   for instance integer or string constants.  */

static void
lto_fixup_types (tree t)
{
  switch (TREE_CODE (t))
    {
    case IDENTIFIER_NODE:
      break;

    case TREE_LIST:
      LTO_FIXUP_TREE (TREE_VALUE (t));
      LTO_FIXUP_TREE (TREE_PURPOSE (t));
      LTO_FIXUP_TREE (TREE_CHAIN (t));
      break;

    case FIELD_DECL:
      lto_ft_field_decl (t);
      break;

    case LABEL_DECL:
    case CONST_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case IMPORTED_DECL:
      lto_ft_decl_common (t);
      break;

    case VAR_DECL:
      lto_ft_decl_with_vis (t);
      break;

    case TYPE_DECL:
      lto_ft_decl_non_common (t);
      break;

    case FUNCTION_DECL:
      lto_ft_function (t);
      break;

    case TREE_BINFO:
      lto_ft_binfo (t);
      break;

    case PLACEHOLDER_EXPR:
      lto_ft_common (t);
      break;

    case BLOCK:
    case TRANSLATION_UNIT_DECL:
    case OPTIMIZATION_NODE:
    case TARGET_OPTION_NODE:
      break;

    default:
      if (TYPE_P (t))
	lto_ft_type (t);
      else if (TREE_CODE (t) == CONSTRUCTOR)
	lto_ft_constructor (t);
      else if (CONSTANT_CLASS_P (t))
	LTO_FIXUP_TREE (TREE_TYPE (t));
      else if (EXPR_P (t))
	{
	  lto_ft_expr (t);
	}
      else
	{
	  remember_with_vars (t);
	}
    }
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

/* Map assigning declarations their resolutions.  */
static pointer_map_t *resolution_map;

/* We need to record resolutions until symbol table is read.  */
static void
register_resolution (tree decl, enum ld_plugin_symbol_resolution resolution)
{
  if (resolution == LDPR_UNKNOWN)
    return;
  if (!resolution_map)
    resolution_map = pointer_map_create ();
  *pointer_map_insert (resolution_map, decl) = (void *)(size_t)resolution;
}

/* Register DECL with the global symbol table and change its
   name if necessary to avoid name clashes for static globals across
   different files.  */

static void
lto_register_var_decl_in_symtab (struct data_in *data_in, tree decl)
{
  tree context;

  /* Variable has file scope, not local. Need to ensure static variables
     between different files don't clash unexpectedly.  */
  if (!TREE_PUBLIC (decl)
      && !((context = decl_function_context (decl))
	   && auto_var_in_fn_p (decl, context)))
    {
      /* ??? We normally pre-mangle names before we serialize them
	 out.  Here, in lto1, we do not know the language, and
	 thus cannot do the mangling again. Instead, we just
	 append a suffix to the mangled name.  The resulting name,
	 however, is not a properly-formed mangled name, and will
	 confuse any attempt to unmangle it.  */
      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      char *label;

      ASM_FORMAT_PRIVATE_NAME (label, name, DECL_UID (decl));
      SET_DECL_ASSEMBLER_NAME (decl, get_identifier (label));
      rest_of_decl_compilation (decl, 1, 0);
    }

  /* If this variable has already been declared, queue the
     declaration for merging.  */
  if (TREE_PUBLIC (decl))
    {
      unsigned ix;
      if (!streamer_tree_cache_lookup (data_in->reader_cache, decl, &ix))
	gcc_unreachable ();
      register_resolution (decl, get_resolution (data_in, ix));
    }
}


/* Register DECL with the global symbol table and change its
   name if necessary to avoid name clashes for static globals across
   different files.  DATA_IN contains descriptors and tables for the
   file being read.  */

static void
lto_register_function_decl_in_symtab (struct data_in *data_in, tree decl)
{
  /* Need to ensure static entities between different files
     don't clash unexpectedly.  */
  if (!TREE_PUBLIC (decl))
    {
      /* We must not use the DECL_ASSEMBLER_NAME macro here, as it
	 may set the assembler name where it was previously empty.  */
      tree old_assembler_name = decl->decl_with_vis.assembler_name;

      /* FIXME lto: We normally pre-mangle names before we serialize
	 them out.  Here, in lto1, we do not know the language, and
	 thus cannot do the mangling again. Instead, we just append a
	 suffix to the mangled name.  The resulting name, however, is
	 not a properly-formed mangled name, and will confuse any
	 attempt to unmangle it.  */
      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      char *label;

      ASM_FORMAT_PRIVATE_NAME (label, name, DECL_UID (decl));
      SET_DECL_ASSEMBLER_NAME (decl, get_identifier (label));

      /* We may arrive here with the old assembler name not set
	 if the function body is not needed, e.g., it has been
	 inlined away and does not appear in the cgraph.  */
      if (old_assembler_name)
	{
	  tree new_assembler_name = DECL_ASSEMBLER_NAME (decl);

	  /* Make the original assembler name available for later use.
	     We may have used it to indicate the section within its
	     object file where the function body may be found.
	     FIXME lto: Find a better way to maintain the function decl
	     to body section mapping so we don't need this hack.  */
	  lto_record_renamed_decl (data_in->file_data,
				   IDENTIFIER_POINTER (old_assembler_name),
				   IDENTIFIER_POINTER (new_assembler_name));
	}
    }

  /* If this variable has already been declared, queue the
     declaration for merging.  */
  if (TREE_PUBLIC (decl) && !DECL_ABSTRACT (decl))
    {
      unsigned ix;
      if (!streamer_tree_cache_lookup (data_in->reader_cache, decl, &ix))
	gcc_unreachable ();
      register_resolution (decl, get_resolution (data_in, ix));
    }
}


/* Given a streamer cache structure DATA_IN (holding a sequence of trees
   for one compilation unit) go over all trees starting at index FROM until the
   end of the sequence and replace fields of those trees, and the trees
   themself with their canonical variants as per gimple_register_type.  */

static void
uniquify_nodes (struct data_in *data_in, unsigned from)
{
  struct streamer_tree_cache_d *cache = data_in->reader_cache;
  unsigned len = cache->nodes.length ();
  unsigned i;

  /* Go backwards because children streamed for the first time come
     as part of their parents, and hence are created after them.  */

  /* First register all the types in the cache.  This makes sure to
     have the original structure in the type cycles when registering
     them and computing hashes.  */
  for (i = len; i-- > from;)
    {
      tree t = cache->nodes[i];
      if (t && TYPE_P (t))
	{
	  tree newt = gimple_register_type (t);
	  /* Mark non-prevailing types so we fix them up.  No need
	     to reset that flag afterwards - nothing that refers
	     to those types is left and they are collected.  */
	  if (newt != t)
	    TREE_VISITED (t) = 1;
	}
    }

  /* Second fixup all trees in the new cache entries.  */
  for (i = len; i-- > from;)
    {
      tree t = cache->nodes[i];
      tree oldt = t;
      if (!t)
	continue;

      /* First fixup the fields of T.  */
      lto_fixup_types (t);

      if (!TYPE_P (t))
	continue;

      /* Now try to find a canonical variant of T itself.  */
      t = GIMPLE_REGISTER_TYPE (t);

      if (t == oldt)
	{
	  /* The following re-creates proper variant lists while fixing up
	     the variant leaders.  We do not stream TYPE_NEXT_VARIANT so the
	     variant list state before fixup is broken.  */
	  tree tem, mv;

#ifdef ENABLE_CHECKING
	  /* Remove us from our main variant list if we are not the
	     variant leader.  */
	  if (TYPE_MAIN_VARIANT (t) != t)
	    {
	      tem = TYPE_MAIN_VARIANT (t);
	      while (tem && TYPE_NEXT_VARIANT (tem) != t)
		tem = TYPE_NEXT_VARIANT (tem);
	      gcc_assert (!tem && !TYPE_NEXT_VARIANT (t));
	    }
#endif

	  /* Query our new main variant.  */
	  mv = GIMPLE_REGISTER_TYPE (TYPE_MAIN_VARIANT (t));

	  /* If we were the variant leader and we get replaced ourselves drop
	     all variants from our list.  */
	  if (TYPE_MAIN_VARIANT (t) == t
	      && mv != t)
	    {
	      tem = t;
	      while (tem)
		{
		  tree tem2 = TYPE_NEXT_VARIANT (tem);
		  TYPE_NEXT_VARIANT (tem) = NULL_TREE;
		  tem = tem2;
		}
	    }

	  /* If we are not our own variant leader link us into our new leaders
	     variant list.  */
	  if (mv != t)
	    {
	      TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (mv);
	      TYPE_NEXT_VARIANT (mv) = t;
	      if (RECORD_OR_UNION_TYPE_P (t))
		TYPE_BINFO (t) = TYPE_BINFO (mv);
	      /* Preserve the invariant that type variants share their
		 TYPE_FIELDS.  */
	      if (RECORD_OR_UNION_TYPE_P (t)
		  && TYPE_FIELDS (mv) != TYPE_FIELDS (t))
		{
		  tree f1, f2;
		  for (f1 = TYPE_FIELDS (mv), f2 = TYPE_FIELDS (t);
		       f1 && f2; f1 = TREE_CHAIN (f1), f2 = TREE_CHAIN (f2))
		    {
		      unsigned ix;
		      gcc_assert (f1 != f2
				  && DECL_NAME (f1) == DECL_NAME (f2));
		      if (!streamer_tree_cache_lookup (cache, f2, &ix))
			gcc_unreachable ();
		      /* If we're going to replace an element which we'd
			 still visit in the next iterations, we wouldn't
			 handle it, so do it here.  We do have to handle it
			 even though the field_decl itself will be removed,
			 as it could refer to e.g. integer_cst which we
			 wouldn't reach via any other way, hence they
			 (and their type) would stay uncollected.  */
		      /* ???  We should rather make sure to replace all
			 references to f2 with f1.  That means handling
			 COMPONENT_REFs and CONSTRUCTOR elements in
			 lto_fixup_types and special-case the field-decl
			 operand handling.  */
		      /* ???  Not sure the above is all relevant in this
		         path canonicalizing TYPE_FIELDS to that of the
			 main variant.  */
		      if (ix < i)
			lto_fixup_types (f2);
		      streamer_tree_cache_insert_at (cache, f1, ix);
		    }
		  TYPE_FIELDS (t) = TYPE_FIELDS (mv);
		}
	    }

	  /* Finally adjust our main variant and fix it up.  */
	  TYPE_MAIN_VARIANT (t) = mv;

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

      else
	{
	  if (RECORD_OR_UNION_TYPE_P (t))
	    {
	      tree f1, f2;
	      if (TYPE_FIELDS (t) != TYPE_FIELDS (oldt))
		for (f1 = TYPE_FIELDS (t), f2 = TYPE_FIELDS (oldt);
		     f1 && f2; f1 = TREE_CHAIN (f1), f2 = TREE_CHAIN (f2))
		  {
		    unsigned ix;
		    gcc_assert (f1 != f2 && DECL_NAME (f1) == DECL_NAME (f2));
		    if (!streamer_tree_cache_lookup (cache, f2, &ix))
		      gcc_unreachable ();
		    /* If we're going to replace an element which we'd
		       still visit in the next iterations, we wouldn't
		       handle it, so do it here.  We do have to handle it
		       even though the field_decl itself will be removed,
		       as it could refer to e.g. integer_cst which we
		       wouldn't reach via any other way, hence they
		       (and their type) would stay uncollected.  */
		    /* ???  We should rather make sure to replace all
		       references to f2 with f1.  That means handling
		       COMPONENT_REFs and CONSTRUCTOR elements in
		       lto_fixup_types and special-case the field-decl
		       operand handling.  */
		    if (ix < i)
		      lto_fixup_types (f2);
		    streamer_tree_cache_insert_at (cache, f1, ix);
		  }
	    }

	  /* If we found a tree that is equal to oldt replace it in the
	     cache, so that further users (in the various LTO sections)
	     make use of it.  */
	  streamer_tree_cache_insert_at (cache, t, i);
	}
    }

  /* Finally compute the canonical type of all TREE_TYPEs and register
     VAR_DECL and FUNCTION_DECL nodes in the symbol table.
     From this point there are no longer any types with
     TYPE_STRUCTURAL_EQUALITY_P and its type-based alias problems.
     This step requires the TYPE_POINTER_TO lists being present, so
     make sure it is done last.  */
  for (i = len; i-- > from;)
    {
      tree t = cache->nodes[i];
      if (t == NULL_TREE)
	continue;

      if (TREE_CODE (t) == VAR_DECL)
	lto_register_var_decl_in_symtab (data_in, t);
      else if (TREE_CODE (t) == FUNCTION_DECL && !DECL_BUILT_IN (t))
	lto_register_function_decl_in_symtab (data_in, t);
      else if (!flag_wpa
	       && TREE_CODE (t) == TYPE_DECL)
	debug_hooks->type_decl (t, !DECL_FILE_SCOPE_P (t));
      else if (TYPE_P (t) && !TYPE_CANONICAL (t))
	TYPE_CANONICAL (t) = gimple_register_canonical_type (t);
    }
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
      t = stream_read_tree (&ib_main, data_in);
      gcc_assert (t && ib_main.p <= ib_main.len);
      uniquify_nodes (data_in, from);
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
    ordera = lto_symtab_encoder_deref (pa->encoder, 0)->symbol.order;
  if (lto_symtab_encoder_size (pb->encoder))
    orderb = lto_symtab_encoder_deref (pb->encoder, 0)->symbol.order;
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
	      symtab_node node = lsei_node (lsei);
	      fprintf (cgraph_dump_file, "%s ", symtab_node_asm_name (node));
	    }
	  fprintf (cgraph_dump_file, "\n  Symbols in boundary: ");
	  for (lsei = lsei_start (part->encoder); !lsei_end_p (lsei);
	       lsei_next (&lsei))
	    {
	      symtab_node node = lsei_node (lsei);
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
    if ((tt) && VAR_OR_FUNCTION_DECL_P (tt)) \
      tt = lto_symtab_prevailing_decl (tt); \
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
	  LTO_NO_PREVAIL (DECL_FIELD_OFFSET (t));
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
      LTO_SET_PREVAIL (t->type_non_common.binfo);

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
	  break;
	default:
	  gcc_unreachable ();
	}
    }
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
	  if (VAR_OR_FUNCTION_DECL_P (*tp))
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
  htab_iterator hi;
  tree t;

  FOR_EACH_HTAB_ELEMENT (tree_with_vars, t, tree, hi)
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

/* Read all the symbols from the input files FNAMES.  NFILES is the
   number of files requested in the command line.  Instantiate a
   global call graph by aggregating all the sub-graphs found in each
   file.  */

static void
read_cgraph_and_symbols (unsigned nfiles, const char **fnames)
{
  unsigned int i, last_file_ix;
  FILE *resolution;
  struct cgraph_node *node;
  int count = 0;
  struct lto_file_decl_data **decl_data;

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

  tree_with_vars = htab_create_ggc (101, htab_hash_pointer, htab_eq_pointer,
				    NULL);
  type_hash_cache = htab_create_ggc (512, tree_int_map_hash,
				     tree_int_map_eq, NULL);
  type_pair_cache = XCNEWVEC (struct type_pair_d, GIMPLE_TYPE_PAIR_SIZE);
  gimple_type_leader = ggc_alloc_cleared_vec_gimple_type_leader_entry_s
		        (GIMPLE_TYPE_LEADER_SIZE);
  gimple_types = htab_create_ggc (16381, gimple_type_hash, gimple_type_eq, 0);

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
      ggc_collect ();
    }

  lto_flatten_files (decl_data, count, last_file_ix);
  lto_stats.num_input_files = count;
  ggc_free(decl_data);
  real_file_decl_data = NULL;

  if (resolution_file_name)
    fclose (resolution);

  /* Free gimple type merging datastructures.  */
  htab_delete (gimple_types);
  gimple_types = NULL;
  htab_delete (type_hash_cache);
  type_hash_cache = NULL;
  free (type_pair_cache);
  type_pair_cache = NULL;
  gimple_type_leader = NULL;
  free_gimple_type_tables ();
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
  if (resolution_map)
    {
      void **res;
      symtab_node snode;

      FOR_EACH_SYMBOL (snode)
	if (symtab_real_symbol_p (snode)
	    && (res = pointer_map_contains (resolution_map,
				            snode->symbol.decl)))
	  snode->symbol.resolution
	    = (enum ld_plugin_symbol_resolution)(size_t)*res;

      pointer_map_destroy (resolution_map);
      resolution_map = NULL;
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
  htab_delete (tree_with_vars);
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
    }

  /* Finally merge the cgraph according to the decl merging decisions.  */
  timevar_push (TV_IPA_LTO_CGRAPH_MERGE);
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Before merging:\n");
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }
  lto_symtab_merge_cgraph_nodes ();
  ggc_collect ();

  /* FIXME: ipa_transforms_to_apply holds list of passes that have optimization
     summaries computed and needs to apply changes.  At the moment WHOPR only
     supports inlining, so we can push it here by hand.  In future we need to stream
     this field into ltrans compilation.  */
  if (flag_ltrans)
    FOR_EACH_DEFINED_FUNCTION (node)
      node->ipa_transforms_to_apply.safe_push ((ipa_opt_pass)&pass_ipa_inline);

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
      if (node->symbol.lto_file_data)
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

  if (gimple_types)
    fprintf (stderr, "[%s] GIMPLE type table: size %ld, %ld elements, "
	     "%ld searches, %ld collisions (ratio: %f)\n", pfx,
	     (long) htab_size (gimple_types),
	     (long) htab_elements (gimple_types),
	     (long) gimple_types->searches,
	     (long) gimple_types->collisions,
	     htab_collisions (gimple_types));
  else
    fprintf (stderr, "[%s] GIMPLE type table is empty\n", pfx);
  if (type_hash_cache)
    fprintf (stderr, "[%s] GIMPLE type hash table: size %ld, %ld elements, "
	     "%ld searches, %ld collisions (ratio: %f)\n", pfx,
	     (long) htab_size (type_hash_cache),
	     (long) htab_elements (type_hash_cache),
	     (long) type_hash_cache->searches,
	     (long) type_hash_cache->collisions,
	     htab_collisions (type_hash_cache));
  else
    fprintf (stderr, "[%s] GIMPLE type hash table is empty\n", pfx);

  print_gimple_types_stats (pfx);
  print_lto_report (pfx);
}

/* Perform whole program analysis (WPA) on the callgraph and write out the
   optimization plan.  */

static void
do_whole_program_analysis (void)
{
  symtab_node node;

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
    {
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }
  bitmap_obstack_initialize (NULL);
  cgraph_state = CGRAPH_STATE_IPA_SSA;

  execute_ipa_pass_list (all_regular_ipa_passes);
  symtab_remove_unreachable_nodes (false, dump_file);

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Optimized ");
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
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
    node->symbol.aux = NULL;

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
  if (flag_lto_report)
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

	  /* Let the middle end know that we have read and merged all of
	     the input files.  */ 
	  compile ();

	  timevar_stop (TV_PHASE_OPT_GEN);

	  /* FIXME lto, if the processes spawned by WPA fail, we miss
	     the chance to print WPA's report, so WPA will call
	     print_lto_report before launching LTRANS.  If LTRANS was
	     launched directly by the driver we would not need to do
	     this.  */
	  if (flag_lto_report)
	    print_lto_report_1 ();

	  /* Record the global variables.  */
	  FOR_EACH_DEFINED_VARIABLE (vnode)
	    vec_safe_push (lto_global_var_decls, vnode->symbol.decl);
	}
    }

  /* Here we make LTO pretend to be a parser.  */
  timevar_start (TV_PHASE_PARSING);
  timevar_push (TV_PARSE_GLOBAL);
}

#include "gt-lto-lto.h"
