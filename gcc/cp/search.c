/* Breadth-first and depth-first routines for
   searching multiple-inheritance lattice for GNU C++.
   Copyright (C) 1987, 89, 92-96, 1997 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* High-level class interface.  */

#include "config.h"
#include "tree.h"
#include <stdio.h>
#include "cp-tree.h"
#include "obstack.h"
#include "flags.h"
#include "rtl.h"
#include "output.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern struct obstack *current_obstack;
extern tree abort_fndecl;

#include "stack.h"

/* Obstack used for remembering decision points of breadth-first.  */

static struct obstack search_obstack;

/* Methods for pushing and popping objects to and from obstacks.  */

struct stack_level *
push_stack_level (obstack, tp, size)
     struct obstack *obstack;
     char *tp;  /* Sony NewsOS 5.0 compiler doesn't like void * here.  */
     int size;
{
  struct stack_level *stack;
  obstack_grow (obstack, tp, size);
  stack = (struct stack_level *) ((char*)obstack_next_free (obstack) - size);
  obstack_finish (obstack);
  stack->obstack = obstack;
  stack->first = (tree *) obstack_base (obstack);
  stack->limit = obstack_room (obstack) / sizeof (tree *);
  return stack;
}

struct stack_level *
pop_stack_level (stack)
     struct stack_level *stack;
{
  struct stack_level *tem = stack;
  struct obstack *obstack = tem->obstack;
  stack = tem->prev;
  obstack_free (obstack, tem);
  return stack;
}

#define search_level stack_level
static struct search_level *search_stack;

static void clear_memoized_cache PROTO((void));
static tree make_memoized_table_entry PROTO((tree, tree, int));
static tree get_abstract_virtuals_1 PROTO((tree, int, tree));
static tree get_vbase_1 PROTO((tree, tree, unsigned int *));
static tree convert_pointer_to_vbase PROTO((tree, tree));
static tree lookup_field_1 PROTO((tree, tree));
static tree convert_pointer_to_single_level PROTO((tree, tree));
static int lookup_fnfields_1 PROTO((tree, tree));
static int lookup_fnfields_here PROTO((tree, tree));
static int is_subobject_of_p PROTO((tree, tree));
static int hides PROTO((tree, tree));
static tree virtual_context PROTO((tree, tree, tree));
static tree get_template_base_recursive
	PROTO((tree, tree, tree, int));
static void dfs_walk PROTO((tree, void (*) (tree), int (*) (tree)));
static void envelope_add_decl PROTO((tree, tree, tree *));
static int get_base_distance_recursive
	PROTO((tree, int, int, int, int *, tree *, tree, tree *,
	       int, int *, int, int));
static void expand_upcast_fixups 
	PROTO((tree, tree, tree, tree, tree, tree, tree *));
static void fixup_virtual_upcast_offsets
	PROTO((tree, tree, int, int, tree, tree, tree, tree,
	       tree *));
static int markedp PROTO((tree));
static int unmarkedp PROTO((tree));
static int numberedp PROTO((tree));
static int unnumberedp PROTO((tree));
static int marked_vtable_pathp PROTO((tree));
static int unmarked_vtable_pathp PROTO((tree));
static int marked_new_vtablep PROTO((tree));
static int unmarked_new_vtablep PROTO((tree));
static int dfs_debug_unmarkedp PROTO((tree));
static void dfs_number PROTO((tree));
static void dfs_unnumber PROTO((tree));
static void dfs_debug_mark PROTO((tree));
static void dfs_find_vbases PROTO((tree));
static void dfs_clear_vbase_slots PROTO((tree));
static void dfs_unmark PROTO((tree));
static void dfs_init_vbase_pointers PROTO((tree));
static void dfs_get_vbase_types PROTO((tree));
static void dfs_record_inheritance PROTO((tree));
static void dfs_pushdecls PROTO((tree));
static void dfs_compress_decls PROTO((tree));
static void dfs_unuse_fields PROTO((tree));
static void add_conversions PROTO((tree));
static tree get_virtuals_named_this PROTO((tree));
static tree get_virtual_destructor PROTO((tree, int));
static int tree_has_any_destructor_p PROTO((tree, int));
static struct search_level *push_search_level
	PROTO((struct stack_level *, struct obstack *));
static struct search_level *pop_search_level
	PROTO((struct stack_level *));
static struct type_level *push_type_level
	PROTO((struct stack_level *, struct obstack *));
static struct type_level *pop_type_level
	PROTO((struct type_level *));
static tree my_tree_cons PROTO((tree, tree, tree));
static tree my_build_string PROTO((char *));
static struct memoized_entry * my_new_memoized_entry
	PROTO((struct memoized_entry *));
static HOST_WIDE_INT breadth_first_search
	PROTO((tree, int (*) (tree, int), int (*) (tree, int)));

static tree vbase_types;
static tree vbase_decl_ptr_intermediate, vbase_decl_ptr;
static tree vbase_init_result;

/* Allocate a level of searching.  */

static struct search_level *
push_search_level (stack, obstack)
     struct stack_level *stack;
     struct obstack *obstack;
{
  struct search_level tem;

  tem.prev = stack;
  return push_stack_level (obstack, (char *)&tem, sizeof (tem));
}

/* Discard a level of search allocation.  */

static struct search_level *
pop_search_level (obstack)
     struct stack_level *obstack;
{
  register struct search_level *stack = pop_stack_level (obstack);

  return stack;
}

/* Search memoization.  */

struct type_level
{
  struct stack_level base;

  /* First object allocated in obstack of entries.  */
  char *entries;

  /* Number of types memoized in this context.  */
  int len;

  /* Type being memoized; save this if we are saving
     memoized contexts.  */
  tree type;
};

/* Obstack used for memoizing member and member function lookup.  */

static struct obstack type_obstack, type_obstack_entries;
static struct type_level *type_stack;
static tree _vptr_name;

/* Make things that look like tree nodes, but allocate them
   on type_obstack_entries.  */
static int my_tree_node_counter;

extern int flag_memoize_lookups, flag_save_memoized_contexts;

/* Variables for gathering statistics.  */
static int my_memoized_entry_counter;
static int memoized_fast_finds[2], memoized_adds[2], memoized_fast_rejects[2];
static int memoized_fields_searched[2];
#ifdef GATHER_STATISTICS
static int n_fields_searched;
static int n_calls_lookup_field, n_calls_lookup_field_1;
static int n_calls_lookup_fnfields, n_calls_lookup_fnfields_1;
static int n_calls_get_base_type;
static int n_outer_fields_searched;
static int n_contexts_saved;
#endif /* GATHER_STATISTICS */

/* Local variables to help save memoization contexts.  */
static tree prev_type_memoized;
static struct type_level *prev_type_stack;

/* This list is used by push_class_decls to know what decls need to
   be pushed into class scope.  */
static tree closed_envelopes = NULL_TREE;

/* Allocate a level of type memoization context.  */

static struct type_level *
push_type_level (stack, obstack)
     struct stack_level *stack;
     struct obstack *obstack;
{
  struct type_level tem;

  tem.base.prev = stack;

  obstack_finish (&type_obstack_entries);
  tem.entries = (char *) obstack_base (&type_obstack_entries);
  tem.len = 0;
  tem.type = NULL_TREE;

  return (struct type_level *)push_stack_level (obstack, (char *)&tem, sizeof (tem));
}

/* Discard a level of type memoization context.  */

static struct type_level *
pop_type_level (stack)
     struct type_level *stack;
{
  obstack_free (&type_obstack_entries, stack->entries);
  return (struct type_level *)pop_stack_level ((struct stack_level *)stack);
}

/* Make something that looks like a TREE_LIST, but
   do it on the type_obstack_entries obstack.  */

static tree
my_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  tree p = (tree)obstack_alloc (&type_obstack_entries, sizeof (struct tree_list));
  ++my_tree_node_counter;
  TREE_TYPE (p) = NULL_TREE;
  ((HOST_WIDE_INT *)p)[3] = 0;
  TREE_SET_CODE (p, TREE_LIST);
  TREE_PURPOSE (p) = purpose;
  TREE_VALUE (p) = value;
  TREE_CHAIN (p) = chain;
  return p;
}

static tree
my_build_string (str)
     char *str;
{
  tree p = (tree)obstack_alloc (&type_obstack_entries, sizeof (struct tree_string));
  ++my_tree_node_counter;
  TREE_TYPE (p) = 0;
  ((int *)p)[3] = 0;
  TREE_SET_CODE (p, STRING_CST);
  TREE_STRING_POINTER (p) = str;
  TREE_STRING_LENGTH (p) = strlen (str);
  return p;
}

/* Memoizing machinery to make searches for multiple inheritance
   reasonably efficient.  */

#define MEMOIZE_HASHSIZE 8
typedef struct memoized_entry
{
  struct memoized_entry *chain;
  int uid;
  tree data_members[MEMOIZE_HASHSIZE];
  tree function_members[MEMOIZE_HASHSIZE];
} *ME;

#define MEMOIZED_CHAIN(ENTRY) (((ME)ENTRY)->chain)
#define MEMOIZED_UID(ENTRY) (((ME)ENTRY)->uid)
#define MEMOIZED_FIELDS(ENTRY,INDEX) (((ME)ENTRY)->data_members[INDEX])
#define MEMOIZED_FNFIELDS(ENTRY,INDEX) (((ME)ENTRY)->function_members[INDEX])
/* The following is probably a lousy hash function.  */
#define MEMOIZED_HASH_FN(NODE) (((long)(NODE)>>4)&(MEMOIZE_HASHSIZE - 1))

static struct memoized_entry *
my_new_memoized_entry (chain)
     struct memoized_entry *chain;
{
  struct memoized_entry *p
    = (struct memoized_entry *)obstack_alloc (&type_obstack_entries,
					      sizeof (struct memoized_entry));
  bzero ((char *) p, sizeof (struct memoized_entry));
  MEMOIZED_CHAIN (p) = chain;
  MEMOIZED_UID (p) = ++my_memoized_entry_counter;
  return p;
}

/* Clears the deferred pop from pop_memoized_context, if any.  */

static void
clear_memoized_cache ()
{
  if (prev_type_stack)
    {
      type_stack = pop_type_level (prev_type_stack);
      prev_type_memoized = 0;
      prev_type_stack = 0;
    }
}

/* Make an entry in the memoized table for type TYPE
   that the entry for NAME is FIELD.  */

static tree
make_memoized_table_entry (type, name, function_p)
     tree type, name;
     int function_p;
{
  int idx = MEMOIZED_HASH_FN (name);
  tree entry, *prev_entry;

  /* Since we allocate from the type_obstack, we must pop any deferred
     levels.  */
   clear_memoized_cache ();

  memoized_adds[function_p] += 1;
  if (CLASSTYPE_MTABLE_ENTRY (type) == 0)
    {
      obstack_ptr_grow (&type_obstack, type);
      obstack_blank (&type_obstack, sizeof (struct memoized_entry *));
      CLASSTYPE_MTABLE_ENTRY (type) = (char *)my_new_memoized_entry ((struct memoized_entry *)0);
      type_stack->len++;
      if (type_stack->len * 2 >= type_stack->base.limit)
	my_friendly_abort (88);
    }
  if (function_p)
    prev_entry = &MEMOIZED_FNFIELDS (CLASSTYPE_MTABLE_ENTRY (type), idx);
  else
    prev_entry = &MEMOIZED_FIELDS (CLASSTYPE_MTABLE_ENTRY (type), idx);

  entry = my_tree_cons (name, NULL_TREE, *prev_entry);
  *prev_entry = entry;

  /* Don't know the error message to give yet.  */
  TREE_TYPE (entry) = error_mark_node;

  return entry;
}

/* When a new function or class context is entered, we build
   a table of types which have been searched for members.
   The table is an array (obstack) of types.  When a type is
   entered into the obstack, its CLASSTYPE_MTABLE_ENTRY
   field is set to point to a new record, of type struct memoized_entry.

   A non-NULL TREE_TYPE of the entry contains an access control error message.

   The slots for the data members are arrays of tree nodes.
   These tree nodes are lists, with the TREE_PURPOSE
   of this list the known member name, and the TREE_VALUE
   as the FIELD_DECL for the member.

   For member functions, the TREE_PURPOSE is again the
   name of the member functions for that class,
   and the TREE_VALUE of the list is a pairs
   whose TREE_PURPOSE is a member functions of this name,
   and whose TREE_VALUE is a list of known argument lists this
   member function has been called with.  The TREE_TYPE of the pair,
   if non-NULL, is an error message to print.  */

/* Tell search machinery that we are entering a new context, and
   to update tables appropriately.

   TYPE is the type of the context we are entering, which can
   be NULL_TREE if we are not in a class's scope.

   USE_OLD, if nonzero tries to use previous context.  */

void
push_memoized_context (type, use_old)
     tree type;
     int use_old;
{
  int len;
  tree *tem;

  if (prev_type_stack)
    {
      if (use_old && prev_type_memoized == type)
	{
#ifdef GATHER_STATISTICS
	  n_contexts_saved++;
#endif /* GATHER_STATISTICS */
	  type_stack = prev_type_stack;
	  prev_type_stack = 0;

	  tem = &type_stack->base.first[0];
	  len = type_stack->len;
	  while (len--)
	    CLASSTYPE_MTABLE_ENTRY (tem[len*2]) = (char *)tem[len*2+1];
	  return;
	}
      /* Otherwise, need to pop old stack here.  */
      clear_memoized_cache ();
    }

  type_stack = push_type_level ((struct stack_level *)type_stack,
				&type_obstack);
  type_stack->type = type;
}

/* Tell search machinery that we have left a context.
   We do not currently save these contexts for later use.
   If we wanted to, we could not use pop_search_level, since
   poping that level allows the data we have collected to
   be clobbered; a stack of obstacks would be needed.  */

void
pop_memoized_context (use_old)
     int use_old;
{
  int len;
  tree *tem = &type_stack->base.first[0];

  if (! flag_save_memoized_contexts)
    use_old = 0;
  else if (use_old)
    {
      len = type_stack->len;
      while (len--)
	tem[len*2+1] = (tree)CLASSTYPE_MTABLE_ENTRY (tem[len*2]);

      /* If there was a deferred pop, we need to pop it now.  */
      clear_memoized_cache ();

      prev_type_stack = type_stack;
      prev_type_memoized = type_stack->type;
    }

  if (flag_memoize_lookups)
    {
      len = type_stack->len;
      while (len--)
	CLASSTYPE_MTABLE_ENTRY (tem[len*2])
	  = (char *)MEMOIZED_CHAIN (CLASSTYPE_MTABLE_ENTRY (tem[len*2]));
    }
  if (! use_old)
    type_stack = pop_type_level (type_stack);
  else
    type_stack = (struct type_level *)type_stack->base.prev;
}

/* Get a virtual binfo that is found inside BINFO's hierarchy that is
   the same type as the type given in PARENT.  To be optimal, we want
   the first one that is found by going through the least number of
   virtual bases.  */

static tree
get_vbase_1 (parent, binfo, depth)
     tree parent, binfo;
     unsigned int *depth;
{
  tree binfos;
  int i, n_baselinks;
  tree rval = NULL_TREE;

  if (BINFO_TYPE (binfo) == parent && TREE_VIA_VIRTUAL (binfo))
    {
      *depth = 0;
      return binfo;
    }

  *depth = *depth - 1;

  binfos = BINFO_BASETYPES (binfo);
  n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  /* Process base types.  */
  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree nrval;

      if (*depth == 0)
	break;

      nrval = get_vbase_1 (parent, base_binfo, depth);
      if (nrval)
	rval = nrval;
    }
  *depth = *depth+1;
  return rval;
}

tree
get_vbase (parent, binfo)
     tree parent;
     tree binfo;
{
  unsigned int d = (unsigned int)-1;
  return get_vbase_1 (parent, binfo, &d);
}

/* Convert EXPR to a virtual base class of type TYPE.  We know that
   EXPR is a non-null POINTER_TYPE to RECORD_TYPE.  We also know that
   the type of what expr points to has a virtual base of type TYPE.  */

static tree
convert_pointer_to_vbase (type, expr)
     tree type;
     tree expr;
{
  tree vb = get_vbase (type, TYPE_BINFO (TREE_TYPE (TREE_TYPE (expr))));
  return convert_pointer_to_real (vb, expr);
}

/* Check whether the type given in BINFO is derived from PARENT.  If
   it isn't, return 0.  If it is, but the derivation is MI-ambiguous
   AND protect != 0, emit an error message and return error_mark_node.

   Otherwise, if TYPE is derived from PARENT, return the actual base
   information, unless a one of the protection violations below
   occurs, in which case emit an error message and return error_mark_node.

   If PROTECT is 1, then check if access to a public field of PARENT
   would be private.  Also check for ambiguity.  */

tree
get_binfo (parent, binfo, protect)
     register tree parent, binfo;
     int protect;
{
  tree type;
  int dist;
  tree rval = NULL_TREE;
  
  if (TREE_CODE (parent) == TREE_VEC)
    parent = BINFO_TYPE (parent);
  else if (! IS_AGGR_TYPE_CODE (TREE_CODE (parent)))
    my_friendly_abort (89);

  if (TREE_CODE (binfo) == TREE_VEC)
    type = BINFO_TYPE (binfo);
  else if (IS_AGGR_TYPE_CODE (TREE_CODE (binfo)))
    type = binfo;
  else
    my_friendly_abort (90);
  
  dist = get_base_distance (parent, binfo, protect, &rval);

  if (dist == -3)
    {
      cp_error ("fields of `%T' are inaccessible in `%T' due to private inheritance",
		parent, type);
      return error_mark_node;
    }
  else if (dist == -2 && protect)
    {
      cp_error ("type `%T' is ambiguous base class for type `%T'", parent,
		type);
      return error_mark_node;
    }

  return rval;
}

/* This is the newer depth first get_base_distance routine.  */

static int
get_base_distance_recursive (binfo, depth, is_private, rval,
			     rval_private_ptr, new_binfo_ptr, parent, path_ptr,
			     protect, via_virtual_ptr, via_virtual,
			     current_scope_in_chain)
     tree binfo;
     int depth, is_private, rval;
     int *rval_private_ptr;
     tree *new_binfo_ptr, parent, *path_ptr;
     int protect, *via_virtual_ptr, via_virtual;
     int current_scope_in_chain;
{
  tree binfos;
  int i, n_baselinks;

  if (protect
      && !current_scope_in_chain
      && is_friend (BINFO_TYPE (binfo), current_scope ()))
    current_scope_in_chain = 1;

  if (BINFO_TYPE (binfo) == parent || binfo == parent)
    {
      if (rval == -1)
	{
	  rval = depth;
	  *rval_private_ptr = is_private;
	  *new_binfo_ptr = binfo;
	  *via_virtual_ptr = via_virtual;
	}
      else
	{
	  int same_object = (tree_int_cst_equal (BINFO_OFFSET (*new_binfo_ptr),
						 BINFO_OFFSET (binfo))
			     && *via_virtual_ptr && via_virtual);
			     
	  if (*via_virtual_ptr && via_virtual==0)
	    {
	      *rval_private_ptr = is_private;
	      *new_binfo_ptr = binfo;
	      *via_virtual_ptr = via_virtual;
	    }
	  else if (same_object)
	    {
	      if (*rval_private_ptr && ! is_private)
		{
		  *rval_private_ptr = is_private;
		  *new_binfo_ptr = binfo;
		  *via_virtual_ptr = via_virtual;
		}
	      return rval;
	    }

	  rval = -2;
	}
      return rval;
    }

  binfos = BINFO_BASETYPES (binfo);
  n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;
  depth += 1;

  /* Process base types.  */
  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);

      /* Find any specific instance of a virtual base, when searching with
	 a binfo...  */
      if (BINFO_MARKED (base_binfo) == 0 || TREE_CODE (parent) == TREE_VEC)
	{
	  int via_private
	    = (protect
	       && (is_private
		   || (!TREE_VIA_PUBLIC (base_binfo)
		       && !(TREE_VIA_PROTECTED (base_binfo)
			    && current_scope_in_chain)
		       && !is_friend (BINFO_TYPE (binfo), current_scope ()))));
	  int this_virtual = via_virtual || TREE_VIA_VIRTUAL (base_binfo);
	  int was;

	  /* When searching for a non-virtual, we cannot mark
	     virtually found binfos.  */
	  if (! this_virtual)
	    SET_BINFO_MARKED (base_binfo);

#define WATCH_VALUES(rval, via_private) (rval == -1 ? 3 : via_private)

	  was = WATCH_VALUES (rval, *via_virtual_ptr);
	  rval = get_base_distance_recursive (base_binfo, depth, via_private,
					      rval, rval_private_ptr,
					      new_binfo_ptr, parent, path_ptr,
					      protect, via_virtual_ptr,
					      this_virtual,
					      current_scope_in_chain);
	  /* watch for updates; only update if path is good.  */
	  if (path_ptr && WATCH_VALUES (rval, *via_virtual_ptr) != was)
	    BINFO_INHERITANCE_CHAIN (base_binfo) = binfo;
	  if (rval == -2 && *via_virtual_ptr == 0)
	    return rval;

#undef WATCH_VALUES

	}
    }

  return rval;
}

/* Return the number of levels between type PARENT and the type given
   in BINFO, following the leftmost path to PARENT not found along a
   virtual path, if there are no real PARENTs (all come from virtual
   base classes), then follow the leftmost path to PARENT.

   Return -1 if TYPE is not derived from PARENT.
   Return -2 if PARENT is an ambiguous base class of TYPE, and PROTECT is
    non-negative.
   Return -3 if PARENT is private to TYPE, and PROTECT is non-zero.

   If PATH_PTR is non-NULL, then also build the list of types
   from PARENT to TYPE, with TREE_VIA_VIRTUAL and TREE_VIA_PUBLIC
   set.

   PARENT can also be a binfo, in which case that exact parent is found
   and no other.  convert_pointer_to_real uses this functionality.

   If BINFO is a binfo, its BINFO_INHERITANCE_CHAIN will be left alone.  */

int
get_base_distance (parent, binfo, protect, path_ptr)
     register tree parent, binfo;
     int protect;
     tree *path_ptr;
{
  int rval;
  int rval_private = 0;
  tree type;
  tree new_binfo = NULL_TREE;
  int via_virtual;
  int watch_access = protect;

  /* Should we be completing types here?  */
  if (TREE_CODE (parent) != TREE_VEC)
    parent = complete_type (TYPE_MAIN_VARIANT (parent));
  else
    complete_type (TREE_TYPE (parent));

  if (TREE_CODE (binfo) == TREE_VEC)
    type = BINFO_TYPE (binfo);
  else if (IS_AGGR_TYPE_CODE (TREE_CODE (binfo)))
    {
      type = complete_type (binfo);
      binfo = TYPE_BINFO (type);

      if (path_ptr)
	BINFO_INHERITANCE_CHAIN (binfo) = NULL_TREE;
    }
  else
    my_friendly_abort (92);

  if (parent == type || parent == binfo)
    {
      /* If the distance is 0, then we don't really need
	 a path pointer, but we shouldn't let garbage go back.  */
      if (path_ptr)
	*path_ptr = binfo;
      return 0;
    }

  if (path_ptr)
    watch_access = 1;

  rval = get_base_distance_recursive (binfo, 0, 0, -1,
				      &rval_private, &new_binfo, parent,
				      path_ptr, watch_access, &via_virtual, 0,
				      0);

  dfs_walk (binfo, dfs_unmark, markedp);

  /* Access restrictions don't count if we found an ambiguous basetype.  */
  if (rval == -2 && protect >= 0)
    rval_private = 0;

  if (rval && protect && rval_private)
    return -3;

  /* find real virtual base classes.  */
  if (rval == -1 && TREE_CODE (parent) == TREE_VEC
      && parent == binfo_member (BINFO_TYPE (parent),
				 CLASSTYPE_VBASECLASSES (type)))
    {
      BINFO_INHERITANCE_CHAIN (parent) = binfo;
      new_binfo = parent;
      rval = 1;
    }

  if (path_ptr)
    *path_ptr = new_binfo;
  return rval;
}

/* Search for a member with name NAME in a multiple inheritance lattice
   specified by TYPE.  If it does not exist, return NULL_TREE.
   If the member is ambiguously referenced, return `error_mark_node'.
   Otherwise, return the FIELD_DECL.  */

/* Do a 1-level search for NAME as a member of TYPE.  The caller must
   figure out whether it can access this field.  (Since it is only one
   level, this is reasonable.)  */

static tree
lookup_field_1 (type, name)
     tree type, name;
{
  register tree field = TYPE_FIELDS (type);

#ifdef GATHER_STATISTICS
  n_calls_lookup_field_1++;
#endif /* GATHER_STATISTICS */
  while (field)
    {
#ifdef GATHER_STATISTICS
      n_fields_searched++;
#endif /* GATHER_STATISTICS */
      if (DECL_NAME (field) == NULL_TREE
	  && TREE_CODE (TREE_TYPE (field)) == UNION_TYPE)
	{
	  tree temp = lookup_field_1 (TREE_TYPE (field), name);
	  if (temp)
	    return temp;
	}
      if (DECL_NAME (field) == name)
	{
	  if ((TREE_CODE(field) == VAR_DECL || TREE_CODE(field) == CONST_DECL)
	      && DECL_ASSEMBLER_NAME (field) != NULL)
	    GNU_xref_ref(current_function_decl,
			 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (field)));
	  return field;
	}
      field = TREE_CHAIN (field);
    }
  /* Not found.  */
  if (name == _vptr_name)
    {
      /* Give the user what s/he thinks s/he wants.  */
      if (TYPE_VIRTUAL_P (type))
	return CLASSTYPE_VFIELD (type);
    }
  return NULL_TREE;
}

/* There are a number of cases we need to be aware of here:
			 current_class_type	current_function_decl
     global			NULL			NULL
     fn-local			NULL			SET
     class-local		SET			NULL
     class->fn			SET			SET
     fn->class			SET			SET

   Those last two make life interesting.  If we're in a function which is
   itself inside a class, we need decls to go into the fn's decls (our
   second case below).  But if we're in a class and the class itself is
   inside a function, we need decls to go into the decls for the class.  To
   achieve this last goal, we must see if, when both current_class_ptr and
   current_function_decl are set, the class was declared inside that
   function.  If so, we know to put the decls into the class's scope.  */

tree
current_scope ()
{
  if (current_function_decl == NULL_TREE)
    return current_class_type;
  if (current_class_type == NULL_TREE)
    return current_function_decl;
  if (DECL_CLASS_CONTEXT (current_function_decl) == current_class_type)
    return current_function_decl;

  return current_class_type;
}

/* Compute the access of FIELD.  This is done by computing
   the access available to each type in BASETYPES (which comes
   as a list of [via_public/basetype] in reverse order, namely base
   class before derived class).  The first one which defines a
   access defines the access for the field.  Otherwise, the
   access of the field is that which occurs normally.

   Uses global variables CURRENT_CLASS_TYPE and
   CURRENT_FUNCTION_DECL to use friend relationships
   if necessary.

   This will be static when lookup_fnfield comes into this file.

   access_public_node means that the field can be accessed by the current lexical
   scope.

   access_protected_node means that the field cannot be accessed by the current
   lexical scope because it is protected.

   access_private_node means that the field cannot be accessed by the current
   lexical scope because it is private.  */

#if 0
#define PUBLIC_RETURN return (DECL_PUBLIC (field) = 1), access_public_node
#define PROTECTED_RETURN return (DECL_PROTECTED (field) = 1), access_protected_node
#define PRIVATE_RETURN return (DECL_PRIVATE (field) = 1), access_private_node
#else
#define PUBLIC_RETURN return access_public_node
#define PROTECTED_RETURN return access_protected_node
#define PRIVATE_RETURN return access_private_node
#endif

#if 0
/* Disabled with DECL_PUBLIC &c.  */
static tree previous_scope = NULL_TREE;
#endif

tree
compute_access (basetype_path, field)
     tree basetype_path, field;
{
  tree access;
  tree types;
  tree context;
  int protected_ok, via_protected;
  extern int flag_access_control;
#if 1
  /* Replaces static decl above.  */
  tree previous_scope;
#endif
  int static_mem
    = ((TREE_CODE (field) == FUNCTION_DECL && DECL_STATIC_FUNCTION_P (field))
       || (TREE_CODE (field) != FUNCTION_DECL && TREE_STATIC (field)));

  if (! flag_access_control)
    return access_public_node;

  /* The field lives in the current class.  */
  if (BINFO_TYPE (basetype_path) == current_class_type)
    return access_public_node;

#if 0
  /* Disabled until pushing function scope clears these out.  If ever.  */
  /* Make these special cases fast.  */
  if (current_scope () == previous_scope)
    {
      if (DECL_PUBLIC (field))
	return access_public_node;
      if (DECL_PROTECTED (field))
	return access_protected_node;
      if (DECL_PRIVATE (field))
	return access_private_node;
    }
#endif

  /* We don't currently support access control on nested types.  */
  if (TREE_CODE (field) == TYPE_DECL)
    return access_public_node;

  previous_scope = current_scope ();
  
  context = DECL_CLASS_CONTEXT (field);
  if (context == NULL_TREE)
    context = DECL_CONTEXT (field);

  /* Fields coming from nested anonymous unions have their DECL_CLASS_CONTEXT
     slot set to the union type rather than the record type containing
     the anonymous union.  */
  if (context && TREE_CODE (context) == UNION_TYPE
      && ANON_AGGRNAME_P (TYPE_IDENTIFIER (context)))
    context = TYPE_CONTEXT (context);

  /* Virtual function tables are never private.  But we should know that
     we are looking for this, and not even try to hide it.  */
  if (DECL_NAME (field) && VFIELD_NAME_P (DECL_NAME (field)) == 1)
    PUBLIC_RETURN;

  /* Member found immediately within object.  */
  if (BINFO_INHERITANCE_CHAIN (basetype_path) == NULL_TREE)
    {
      /* Are we (or an enclosing scope) friends with the class that has
         FIELD? */
      if (is_friend (context, previous_scope))
	PUBLIC_RETURN;

      /* If it's private, it's private, you letch.  */
      if (TREE_PRIVATE (field))
	PRIVATE_RETURN;

      /* ARM $11.5.  Member functions of a derived class can access the
	 non-static protected members of a base class only through a
	 pointer to the derived class, a reference to it, or an object
	 of it. Also any subsequently derived classes also have
	 access.  */
      else if (TREE_PROTECTED (field))
	{
	  if (current_class_type
	      && static_mem
	      && ACCESSIBLY_DERIVED_FROM_P (context, current_class_type))
	    PUBLIC_RETURN;
	  else
	    PROTECTED_RETURN;
	}
      else
	PUBLIC_RETURN;
    }

  /* must reverse more than one element */
  basetype_path = reverse_path (basetype_path);
  types = basetype_path;
  via_protected = 0;
  access = access_default_node;
  protected_ok = static_mem && current_class_type
    && ACCESSIBLY_DERIVED_FROM_P (BINFO_TYPE (types), current_class_type);

  while (1)
    {
      tree member;
      tree binfo = types;
      tree type = BINFO_TYPE (binfo);
      int private_ok = 0;

      /* Friends of a class can see protected members of its bases.
         Note that classes are their own friends.  */
      if (is_friend (type, previous_scope))
	{
	  protected_ok = 1;
	  private_ok = 1;
	}

      member = purpose_member (type, DECL_ACCESS (field));
      if (member)
	{
	  access = TREE_VALUE (member);
	  break;
	}

      types = BINFO_INHERITANCE_CHAIN (types);

      /* If the next type was VIA_PROTECTED, then fields of all remaining
	 classes past that one are *at least* protected.  */
      if (types)
	{
	  if (TREE_VIA_PROTECTED (types))
	    via_protected = 1;
	  else if (! TREE_VIA_PUBLIC (types) && ! private_ok)
	    {
	      access = access_private_node;
	      break;
	    }
	}
      else
	break;
    }
  reverse_path (basetype_path);

  /* No special visibilities apply.  Use normal rules.  */

  if (access == access_default_node)
    {
      if (is_friend (context, previous_scope))
	access = access_public_node;
      else if (TREE_PRIVATE (field))
	access = access_private_node;
      else if (TREE_PROTECTED (field))
	access = access_protected_node;
      else
	access = access_public_node;
    }

  if (access == access_public_node && via_protected)
    access = access_protected_node;

  if (access == access_protected_node && protected_ok)
    access = access_public_node;

#if 0
  if (access == access_public_node)
    DECL_PUBLIC (field) = 1;
  else if (access == access_protected_node)
    DECL_PROTECTED (field) = 1;
  else if (access == access_private_node)
    DECL_PRIVATE (field) = 1;
  else my_friendly_abort (96);
#endif
  return access;
}

/* Routine to see if the sub-object denoted by the binfo PARENT can be
   found as a base class and sub-object of the object denoted by
   BINFO.  This routine relies upon binfos not being shared, except
   for binfos for virtual bases.  */

static int
is_subobject_of_p (parent, binfo)
     tree parent, binfo;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  if (parent == binfo)
    return 1;

  /* Process and/or queue base types.  */
  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      if (TREE_VIA_VIRTUAL (base_binfo))
	base_binfo = TYPE_BINFO (BINFO_TYPE (base_binfo));
      if (is_subobject_of_p (parent, base_binfo))
	return 1;
    }
  return 0;
}

/* See if a one FIELD_DECL hides another.  This routine is meant to
   correspond to ANSI working paper Sept 17, 1992 10p4.  The two
   binfos given are the binfos corresponding to the particular places
   the FIELD_DECLs are found.  This routine relies upon binfos not
   being shared, except for virtual bases.  */

static int
hides (hider_binfo, hidee_binfo)
     tree hider_binfo, hidee_binfo;
{
  /* hider hides hidee, if hider has hidee as a base class and
     the instance of hidee is a sub-object of hider.  The first
     part is always true is the second part is true.

     When hider and hidee are the same (two ways to get to the exact
     same member) we consider either one as hiding the other.  */
  return is_subobject_of_p (hidee_binfo, hider_binfo);
}

/* Very similar to lookup_fnfields_1 but it ensures that at least one
   function was declared inside the class given by TYPE.  It really should
   only return functions that match the given TYPE.  */

static int
lookup_fnfields_here (type, name)
     tree type, name;
{
  int idx = lookup_fnfields_1 (type, name);
  tree fndecls;

  /* ctors and dtors are always only in the right class.  */
  if (idx <= 1)
    return idx;
  fndecls = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), idx);
  while (fndecls)
    {
      if (TYPE_MAIN_VARIANT (DECL_CLASS_CONTEXT (fndecls))
	  == TYPE_MAIN_VARIANT (type))
	return idx;
      fndecls = TREE_CHAIN (fndecls);
    }
  return -1;
}

/* Look for a field named NAME in an inheritance lattice dominated by
   XBASETYPE.  PROTECT is zero if we can avoid computing access
   information, otherwise it is 1.  WANT_TYPE is 1 when we should only
   return TYPE_DECLs, if no TYPE_DECL can be found return NULL_TREE.

   It was not clear what should happen if WANT_TYPE is set, and an
   ambiguity is found.  At least one use (lookup_name) to not see
   the error.  */

tree
lookup_field (xbasetype, name, protect, want_type)
     register tree xbasetype, name;
     int protect, want_type;
{
  int head = 0, tail = 0;
  tree rval, rval_binfo = NULL_TREE, rval_binfo_h;
  tree type, basetype_chain, basetype_path;
  tree this_v = access_default_node;
  tree entry, binfo, binfo_h;
  tree own_access = access_default_node;
  int vbase_name_p = VBASE_NAME_P (name);

  /* rval_binfo is the binfo associated with the found member, note,
     this can be set with useful information, even when rval is not
     set, because it must deal with ALL members, not just non-function
     members.  It is used for ambiguity checking and the hidden
     checks.  Whereas rval is only set if a proper (not hidden)
     non-function member is found.  */

  /* rval_binfo_h and binfo_h are binfo values used when we perform the
     hiding checks, as virtual base classes may not be shared.  The strategy
     is we always go into the the binfo hierarchy owned by TYPE_BINFO of
     virtual base classes, as we cross virtual base class lines.  This way
     we know that binfo of a virtual base class will always == itself when
     found along any line.  (mrs)  */

  char *errstr = 0;

  /* Set this to nonzero if we don't know how to compute
     accurate error messages for access control.  */
  int idx = MEMOIZED_HASH_FN (name);

#if 0
  /* We cannot search for constructor/destructor names like this.  */
  /* This can't go here, but where should it go?  */
  /* If we are looking for a constructor in a templated type, use the
     unspecialized name, as that is how we store it.  */
  if (IDENTIFIER_TEMPLATE (name))
    name = constructor_name (name);
#endif

  if (xbasetype == current_class_type && TYPE_BEING_DEFINED (xbasetype)
      && IDENTIFIER_CLASS_VALUE (name))
    {
      tree field = IDENTIFIER_CLASS_VALUE (name);
      if (TREE_CODE (field) != FUNCTION_DECL
	  && ! (want_type && TREE_CODE (field) != TYPE_DECL))
	return field;
    }

  if (TREE_CODE (xbasetype) == TREE_VEC)
    {
      type = BINFO_TYPE (xbasetype);
      basetype_path = xbasetype;
    }
  else if (IS_AGGR_TYPE_CODE (TREE_CODE (xbasetype)))
    {
      type = xbasetype;
      basetype_path = TYPE_BINFO (type);
      BINFO_VIA_PUBLIC (basetype_path) = 1;
      BINFO_INHERITANCE_CHAIN (basetype_path) = NULL_TREE;
    }
  else
    my_friendly_abort (97);

  complete_type (type);

  if (CLASSTYPE_MTABLE_ENTRY (type))
    {
      tree tem = MEMOIZED_FIELDS (CLASSTYPE_MTABLE_ENTRY (type), idx);

      while (tem && TREE_PURPOSE (tem) != name)
	{
	  memoized_fields_searched[0]++;
	  tem = TREE_CHAIN (tem);
	}
      if (tem)
	{
	  if (protect && TREE_TYPE (tem))
	    {
	      error (TREE_STRING_POINTER (TREE_TYPE (tem)),
		     IDENTIFIER_POINTER (name),
		     TYPE_NAME_STRING (DECL_FIELD_CONTEXT (TREE_VALUE (tem))));
	      return error_mark_node;
	    }
	  if (TREE_VALUE (tem) == NULL_TREE)
	    memoized_fast_rejects[0] += 1;
	  else
	    memoized_fast_finds[0] += 1;
	  return TREE_VALUE (tem);
	}
    }

#ifdef GATHER_STATISTICS
  n_calls_lookup_field++;
#endif /* GATHER_STATISTICS */
  if (protect && flag_memoize_lookups && ! global_bindings_p ())
    entry = make_memoized_table_entry (type, name, 0);
  else
    entry = 0;

  rval = lookup_field_1 (type, name);

  if (rval || lookup_fnfields_here (type, name) >= 0)
    {
      if (rval)
	{
	  if (want_type)
	    {
	      if (TREE_CODE (rval) != TYPE_DECL)
		{
		  rval = purpose_member (name, CLASSTYPE_TAGS (type));
		  if (rval)
		    rval = TYPE_MAIN_DECL (TREE_VALUE (rval));
		}
	    }
	  else
	    {
	      if (TREE_CODE (rval) == TYPE_DECL
		  && lookup_fnfields_here (type, name) >= 0)
		rval = NULL_TREE;
	    }
	}

      if (protect && rval)
	{
	  if (TREE_PRIVATE (rval) | TREE_PROTECTED (rval))
	    this_v = compute_access (basetype_path, rval);
	  if (TREE_CODE (rval) == CONST_DECL)
	    {
	      if (this_v == access_private_node)
		errstr = "enum `%D' is a private value of class `%T'";
	      else if (this_v == access_protected_node)
		errstr = "enum `%D' is a protected value of class `%T'";
	    }
	  else
	    {
	      if (this_v == access_private_node)
		errstr = "member `%D' is a private member of class `%T'";
	      else if (this_v == access_protected_node)
		errstr = "member `%D' is a protected member of class `%T'";
	    }
	}

      if (entry)
	{
	  if (errstr)
	    {
	      /* This depends on behavior of lookup_field_1!  */
	      tree error_string = my_build_string (errstr);
	      TREE_TYPE (entry) = error_string;
	    }
	  else
	    {
	      /* Let entry know there is no problem with this access.  */
	      TREE_TYPE (entry) = NULL_TREE;
	    }
	  TREE_VALUE (entry) = rval;
	}

      if (errstr && protect)
	{
	  cp_error (errstr, name, type);
	  return error_mark_node;
	}
      return rval;
    }

  basetype_chain = build_expr_list (NULL_TREE, basetype_path);
  TREE_VIA_PUBLIC (basetype_chain) = TREE_VIA_PUBLIC (basetype_path);
  TREE_VIA_PROTECTED (basetype_chain) = TREE_VIA_PROTECTED (basetype_path);
  TREE_VIA_VIRTUAL (basetype_chain) = TREE_VIA_VIRTUAL (basetype_path);

  /* The ambiguity check relies upon breadth first searching.  */

  search_stack = push_search_level (search_stack, &search_obstack);
  binfo = basetype_path;
  binfo_h = binfo;

  while (1)
    {
      tree binfos = BINFO_BASETYPES (binfo);
      int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;
      tree nval;

      /* Process and/or queue base types.  */
      for (i = 0; i < n_baselinks; i++)
	{
	  tree base_binfo = TREE_VEC_ELT (binfos, i);
	  if (BINFO_FIELDS_MARKED (base_binfo) == 0)
	    {
	      tree btypes;

	      SET_BINFO_FIELDS_MARKED (base_binfo);
	      btypes = my_tree_cons (NULL_TREE, base_binfo, basetype_chain);
	      TREE_VIA_PUBLIC (btypes) = TREE_VIA_PUBLIC (base_binfo);
	      TREE_VIA_PROTECTED (btypes) = TREE_VIA_PROTECTED (base_binfo);
	      TREE_VIA_VIRTUAL (btypes) = TREE_VIA_VIRTUAL (base_binfo);
	      if (TREE_VIA_VIRTUAL (base_binfo))
		btypes = my_tree_cons (NULL_TREE,
				    TYPE_BINFO (BINFO_TYPE (TREE_VEC_ELT (BINFO_BASETYPES (binfo_h), i))),
				    btypes);
	      else
		btypes = my_tree_cons (NULL_TREE,
				    TREE_VEC_ELT (BINFO_BASETYPES (binfo_h), i),
				    btypes);
	      obstack_ptr_grow (&search_obstack, btypes);
	      tail += 1;
	      if (tail >= search_stack->limit)
		my_friendly_abort (98);
	    }
	}

      /* Process head of queue, if one exists.  */
      if (head >= tail)
	break;

      basetype_chain = search_stack->first[head++];
      binfo_h = TREE_VALUE (basetype_chain);
      basetype_chain = TREE_CHAIN (basetype_chain);
      basetype_path = TREE_VALUE (basetype_chain);
      if (TREE_CHAIN (basetype_chain))
	BINFO_INHERITANCE_CHAIN (basetype_path) = TREE_VALUE (TREE_CHAIN (basetype_chain));
      else
	BINFO_INHERITANCE_CHAIN (basetype_path) = NULL_TREE;

      binfo = basetype_path;
      type = BINFO_TYPE (binfo);

      /* See if we can find NAME in TYPE.  If RVAL is nonzero,
	 and we do find NAME in TYPE, verify that such a second
	 sighting is in fact valid.  */

      nval = lookup_field_1 (type, name);

      if (nval || lookup_fnfields_here (type, name)>=0)
	{
	  if (nval && nval == rval && SHARED_MEMBER_P (nval))
	    {
	      /* This is ok, the member found is the same [class.ambig] */
	    }
	  else if (rval_binfo && hides (rval_binfo_h, binfo_h))
	    {
	      /* This is ok, the member found is in rval_binfo, not
		 here (binfo).  */
	    }
	  else if (rval_binfo==NULL_TREE || hides (binfo_h, rval_binfo_h))
	    {
	      /* This is ok, the member found is here (binfo), not in
		 rval_binfo.  */
	      if (nval)
		{
		  rval = nval;
		  if (entry || protect)
		    this_v = compute_access (basetype_path, rval);
		  /* These may look ambiguous, but they really are not.  */
		  if (vbase_name_p)
		    break;
		}
	      else
		{
		  /* Undo finding it before, as something else hides it.  */
		  rval = NULL_TREE;
		}
	      rval_binfo = binfo;
	      rval_binfo_h = binfo_h;
	    }
	  else
	    {
	      /* This is ambiguous.  */
	      errstr = "request for member `%D' is ambiguous";
	      protect += 2;
	      break;
	    }
	}
    }
  {
    tree *tp = search_stack->first;
    tree *search_tail = tp + tail;

    if (entry)
      TREE_VALUE (entry) = rval;

    if (rval_binfo)
      {
	type = BINFO_TYPE (rval_binfo);

	if (rval)
	  {
	    if (want_type)
	      {
		if (TREE_CODE (rval) != TYPE_DECL)
		  {
		    rval = purpose_member (name, CLASSTYPE_TAGS (type));
		    if (rval)
		      rval = TYPE_MAIN_DECL (TREE_VALUE (rval));
		  }
	      }
	    else
	      {
		if (TREE_CODE (rval) == TYPE_DECL
		    && lookup_fnfields_here (type, name) >= 0)
		  rval = NULL_TREE;
	      }
	  }
      }

    if (rval == NULL_TREE)
      errstr = 0;

    /* If this FIELD_DECL defines its own access level, deal with that.  */
    if (rval && errstr == 0
	&& ((protect&1) || entry)
	&& DECL_LANG_SPECIFIC (rval)
	&& DECL_ACCESS (rval))
      {
	while (tp < search_tail)
	  {
	    /* If is possible for one of the derived types on the path to
	       have defined special access for this field.  Look for such
	       declarations and report an error if a conflict is found.  */
	    tree new_v;

	    if (this_v != access_default_node)
	      new_v = compute_access (TREE_VALUE (TREE_CHAIN (*tp)), rval);
	    if (this_v != access_default_node && new_v != this_v)
	      {
		errstr = "conflicting access to member `%D'";
		this_v = access_default_node;
	      }
	    own_access = new_v;
	    CLEAR_BINFO_FIELDS_MARKED (TREE_VALUE (TREE_CHAIN (*tp)));
	    tp += 1;
	  }
      }
    else
      {
	while (tp < search_tail)
	  {
	    CLEAR_BINFO_FIELDS_MARKED (TREE_VALUE (TREE_CHAIN (*tp)));
	    tp += 1;
	  }
      }
  }
  search_stack = pop_search_level (search_stack);

  if (errstr == 0)
    {
      if (own_access == access_private_node)
	errstr = "member `%D' declared private";
      else if (own_access == access_protected_node)
	errstr = "member `%D' declared protected";
      else if (this_v == access_private_node)
	errstr = TREE_PRIVATE (rval)
	  ? "member `%D' is private"
	    : "member `%D' is from private base class";
      else if (this_v == access_protected_node)
	errstr = TREE_PROTECTED (rval)
	  ? "member `%D' is protected"
	    : "member `%D' is from protected base class";
    }

  if (entry)
    {
      if (errstr)
	{
	  tree error_string = my_build_string (errstr);
	  /* Save error message with entry.  */
	  TREE_TYPE (entry) = error_string;
	}
      else
	{
	  /* Mark entry as having no error string.  */
	  TREE_TYPE (entry) = NULL_TREE;
	}
    }

  if (protect == 2)
    {
      /* If we are not interested in ambiguities, don't report them,
	 just return NULL_TREE.  */
      rval = NULL_TREE;
      protect = 0;
    }

  if (errstr && protect)
    {
      cp_error (errstr, name, type);
      rval = error_mark_node;
    }
  return rval;
}

/* Try to find NAME inside a nested class.  */

tree
lookup_nested_field (name, complain)
     tree name;
     int complain;
{
  register tree t;

  tree id = NULL_TREE;
  if (TREE_CHAIN (current_class_type))
    {
      /* Climb our way up the nested ladder, seeing if we're trying to
	 modify a field in an enclosing class.  If so, we should only
	 be able to modify if it's static.  */
      for (t = TREE_CHAIN (current_class_type);
	   t && DECL_CONTEXT (t);
	   t = TREE_CHAIN (DECL_CONTEXT (t)))
	{
	  if (TREE_CODE (DECL_CONTEXT (t)) != RECORD_TYPE)
	    break;

	  /* N.B.: lookup_field will do the access checking for us */
	  id = lookup_field (DECL_CONTEXT (t), name, complain, 0);
	  if (id == error_mark_node)
	    {
	      id = NULL_TREE;
	      continue;
	    }

	  if (id != NULL_TREE)
	    {
	      if (TREE_CODE (id) == FIELD_DECL
		  && ! TREE_STATIC (id)
		  && TREE_TYPE (id) != error_mark_node)
		{
		  if (complain)
		    {
		      /* At parse time, we don't want to give this error, since
			 we won't have enough state to make this kind of
			 decision properly.  But there are times (e.g., with
			 enums in nested classes) when we do need to call
			 this fn at parse time.  So, in those cases, we pass
			 complain as a 0 and just return a NULL_TREE.  */
		      cp_error ("assignment to non-static member `%D' of enclosing class `%T'",
				id, DECL_CONTEXT (t));
		      /* Mark this for do_identifier().  It would otherwise
			 claim that the variable was undeclared.  */
		      TREE_TYPE (id) = error_mark_node;
		    }
		  else
		    {
		      id = NULL_TREE;
		      continue;
		    }
		}
	      break;
	    }
	}
    }

  return id;
}

/* TYPE is a class type. Return the index of the fields within
   the method vector with name NAME, or -1 is no such field exists.  */

static int
lookup_fnfields_1 (type, name)
     tree type, name;
{
  register tree method_vec = CLASSTYPE_METHOD_VEC (type);

  if (method_vec != 0)
    {
      register tree *methods = &TREE_VEC_ELT (method_vec, 0);
      register tree *end = TREE_VEC_END (method_vec);

#ifdef GATHER_STATISTICS
      n_calls_lookup_fnfields_1++;
#endif /* GATHER_STATISTICS */

      /* Constructors are first...  */
      if (*methods && name == ctor_identifier)
	return 0;

      /* and destructors are second.  */
      if (*++methods && name == dtor_identifier)
	return 1;

      while (++methods != end)
	{
#ifdef GATHER_STATISTICS
	  n_outer_fields_searched++;
#endif /* GATHER_STATISTICS */
	  if (DECL_NAME (*methods) == name)
	    break;
	}

      /* If we didn't find it, it might have been a template
	 conversion operator.  (Note that we don't look for this case
	 above so that we will always find specializations first.)  */
      if (methods == end 
	  && IDENTIFIER_TYPENAME_P (name)) 
	{
	  methods = &TREE_VEC_ELT (method_vec, 0) + 1;
	  
	  while (++methods != end)
	    {
	      if (TREE_CODE (*methods) == TEMPLATE_DECL 
		  && IDENTIFIER_TYPENAME_P (DECL_NAME (*methods)))
		break;
	    }
	}

      if (methods != end)
	return methods - &TREE_VEC_ELT (method_vec, 0);
    }

  return -1;
}

/* Starting from BASETYPE, return a TREE_BASELINK-like object
   which gives the following information (in a list):

   TREE_TYPE: list of basetypes needed to get to...
   TREE_VALUE: list of all functions in a given type
   which have name NAME.

   No access information is computed by this function,
   other then to adorn the list of basetypes with
   TREE_VIA_PUBLIC.

   If there are two ways to find a name (two members), if COMPLAIN is
   non-zero, then error_mark_node is returned, and an error message is
   printed, otherwise, just an error_mark_node is returned.

   As a special case, is COMPLAIN is -1, we don't complain, and we
   don't return error_mark_node, but rather the complete list of
   virtuals.  This is used by get_virtuals_named_this.  */

tree
lookup_fnfields (basetype_path, name, complain)
     tree basetype_path, name;
     int complain;
{
  int head = 0, tail = 0;
  tree type, rval, rval_binfo = NULL_TREE, rvals = NULL_TREE, rval_binfo_h;
  tree entry, binfo, basetype_chain, binfo_h;
  int find_all = 0;

  /* rval_binfo is the binfo associated with the found member, note,
     this can be set with useful information, even when rval is not
     set, because it must deal with ALL members, not just function
     members.  It is used for ambiguity checking and the hidden
     checks.  Whereas rval is only set if a proper (not hidden)
     function member is found.  */

  /* rval_binfo_h and binfo_h are binfo values used when we perform the
     hiding checks, as virtual base classes may not be shared.  The strategy
     is we always go into the the binfo hierarchy owned by TYPE_BINFO of
     virtual base classes, as we cross virtual base class lines.  This way
     we know that binfo of a virtual base class will always == itself when
     found along any line.  (mrs)  */

  /* For now, don't try this.  */
  int protect = complain;

  char *errstr = 0;

  /* Set this to nonzero if we don't know how to compute
     accurate error messages for access control.  */
  int idx = MEMOIZED_HASH_FN (name);

  if (complain == -1)
    {
      find_all = 1;
      protect = complain = 0;
    }

#if 0
  /* We cannot search for constructor/destructor names like this.  */
  /* This can't go here, but where should it go?  */
  /* If we are looking for a constructor in a templated type, use the
     unspecialized name, as that is how we store it.  */
  if (IDENTIFIER_TEMPLATE (name))
    name = constructor_name (name);
#endif

  binfo = basetype_path;
  binfo_h = binfo;
  type = complete_type (BINFO_TYPE (basetype_path));

  /* The memoization code is in need of maintenance.  */
  if (!find_all && CLASSTYPE_MTABLE_ENTRY (type))
    {
      tree tem = MEMOIZED_FNFIELDS (CLASSTYPE_MTABLE_ENTRY (type), idx);

      while (tem && TREE_PURPOSE (tem) != name)
	{
	  memoized_fields_searched[1]++;
	  tem = TREE_CHAIN (tem);
	}
      if (tem)
	{
	  if (protect && TREE_TYPE (tem))
	    {
	      error (TREE_STRING_POINTER (TREE_TYPE (tem)),
		     IDENTIFIER_POINTER (name),
		     TYPE_NAME_STRING (DECL_CLASS_CONTEXT (TREE_VALUE (TREE_VALUE (tem)))));
	      return error_mark_node;
	    }
	  if (TREE_VALUE (tem) == NULL_TREE)
	    {
	      memoized_fast_rejects[1] += 1;
	      return NULL_TREE;
	    }
	  else
	    {
	      /* Want to return this, but we must make sure
		 that access information is consistent.  */
	      tree baselink = TREE_VALUE (tem);
	      tree memoized_basetypes = TREE_PURPOSE (baselink);
	      tree these_basetypes = basetype_path;
	      while (memoized_basetypes && these_basetypes)
		{
		  memoized_fields_searched[1]++;
		  if (TREE_VALUE (memoized_basetypes) != these_basetypes)
		    break;
		  memoized_basetypes = TREE_CHAIN (memoized_basetypes);
		  these_basetypes = BINFO_INHERITANCE_CHAIN (these_basetypes);
		}
	      /* The following statement is true only when both are NULL.  */
	      if (memoized_basetypes == these_basetypes)
		{
		  memoized_fast_finds[1] += 1;
		  return TREE_VALUE (tem);
		}
	      /* else, we must re-find this field by hand.  */
	      baselink = tree_cons (basetype_path, TREE_VALUE (baselink), TREE_CHAIN (baselink));
	      return baselink;
	    }
	}
    }

#ifdef GATHER_STATISTICS
  n_calls_lookup_fnfields++;
#endif /* GATHER_STATISTICS */
  if (protect && flag_memoize_lookups && ! global_bindings_p ())
    entry = make_memoized_table_entry (type, name, 1);
  else
    entry = 0;

  idx = lookup_fnfields_here (type, name);
  if (idx >= 0 || lookup_field_1 (type, name))
    {
      rval_binfo = basetype_path;
      rval_binfo_h = rval_binfo;
    }

  if (idx >= 0)
    {
      rval = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), idx);
      rvals = my_tree_cons (basetype_path, rval, rvals);
      if (BINFO_BASETYPES (binfo) && CLASSTYPE_BASELINK_VEC (type))
	TREE_TYPE (rvals) = TREE_VEC_ELT (CLASSTYPE_BASELINK_VEC (type), idx);

      if (entry)
	{
	  TREE_VALUE (entry) = rvals;
	  TREE_TYPE (entry) = NULL_TREE;
	}

      return rvals;
    }
  rval = NULL_TREE;

  if (name == ctor_identifier || name == dtor_identifier)
    {
      /* Don't allow lookups of constructors and destructors to go
 	 deeper than the first place we look.  */
      if (entry)
 	TREE_TYPE (entry) = TREE_VALUE (entry) = NULL_TREE;

      return NULL_TREE;
    }

  if (basetype_path == TYPE_BINFO (type))
    {
      basetype_chain = CLASSTYPE_BINFO_AS_LIST (type);
      TREE_VIA_PUBLIC (basetype_chain) = 1;
      BINFO_VIA_PUBLIC (basetype_path) = 1;
      BINFO_INHERITANCE_CHAIN (basetype_path) = NULL_TREE;
    }
  else
    {
      basetype_chain = build_expr_list (NULL_TREE, basetype_path);
      TREE_VIA_PUBLIC (basetype_chain) = TREE_VIA_PUBLIC (basetype_path);
      TREE_VIA_PROTECTED (basetype_chain) = TREE_VIA_PROTECTED (basetype_path);
      TREE_VIA_VIRTUAL (basetype_chain) = TREE_VIA_VIRTUAL (basetype_path);
    }

  /* The ambiguity check relies upon breadth first searching.  */

  search_stack = push_search_level (search_stack, &search_obstack);
  binfo = basetype_path;
  binfo_h = binfo;

  while (1)
    {
      tree binfos = BINFO_BASETYPES (binfo);
      int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;
      int idx;

      /* Process and/or queue base types.  */
      for (i = 0; i < n_baselinks; i++)
	{
	  tree base_binfo = TREE_VEC_ELT (binfos, i);
	  if (BINFO_FIELDS_MARKED (base_binfo) == 0)
	    {
	      tree btypes;

	      SET_BINFO_FIELDS_MARKED (base_binfo);
	      btypes = my_tree_cons (NULL_TREE, base_binfo, basetype_chain);
	      TREE_VIA_PUBLIC (btypes) = TREE_VIA_PUBLIC (base_binfo);
	      TREE_VIA_PROTECTED (btypes) = TREE_VIA_PROTECTED (base_binfo);
	      TREE_VIA_VIRTUAL (btypes) = TREE_VIA_VIRTUAL (base_binfo);
	      if (TREE_VIA_VIRTUAL (base_binfo))
		btypes = my_tree_cons (NULL_TREE,
				    TYPE_BINFO (BINFO_TYPE (TREE_VEC_ELT (BINFO_BASETYPES (binfo_h), i))),
				    btypes);
	      else
		btypes = my_tree_cons (NULL_TREE,
				    TREE_VEC_ELT (BINFO_BASETYPES (binfo_h), i),
				    btypes);
	      obstack_ptr_grow (&search_obstack, btypes);
	      tail += 1;
	      if (tail >= search_stack->limit)
		my_friendly_abort (99);
	    }
	}

      /* Process head of queue, if one exists.  */
      if (head >= tail)
	break;

      basetype_chain = search_stack->first[head++];
      binfo_h = TREE_VALUE (basetype_chain);
      basetype_chain = TREE_CHAIN (basetype_chain);
      basetype_path = TREE_VALUE (basetype_chain);
      if (TREE_CHAIN (basetype_chain))
	BINFO_INHERITANCE_CHAIN (basetype_path) = TREE_VALUE (TREE_CHAIN (basetype_chain));
      else
	BINFO_INHERITANCE_CHAIN (basetype_path) = NULL_TREE;

      binfo = basetype_path;
      type = BINFO_TYPE (binfo);

      /* See if we can find NAME in TYPE.  If RVAL is nonzero,
	 and we do find NAME in TYPE, verify that such a second
	 sighting is in fact valid.  */

      idx = lookup_fnfields_here (type, name);

      if (idx >= 0 || (lookup_field_1 (type, name)!=NULL_TREE && !find_all))
	{
	  if (rval_binfo && !find_all && hides (rval_binfo_h, binfo_h))
	    {
	      /* This is ok, the member found is in rval_binfo, not
		 here (binfo).  */
	    }
	  else if (rval_binfo==NULL_TREE || find_all || hides (binfo_h, rval_binfo_h))
	    {
	      /* This is ok, the member found is here (binfo), not in
		 rval_binfo.  */
	      if (idx >= 0)
		{
		  rval = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), idx);
		  /* Note, rvals can only be previously set if find_all is
		     true.  */
		  rvals = my_tree_cons (basetype_path, rval, rvals);
		  if (TYPE_BINFO_BASETYPES (type)
		      && CLASSTYPE_BASELINK_VEC (type))
		    TREE_TYPE (rvals) = TREE_VEC_ELT (CLASSTYPE_BASELINK_VEC (type), idx);
		}
	      else
		{
		  /* Undo finding it before, as something else hides it.  */
		  rval = NULL_TREE;
		  rvals = NULL_TREE;
		}
	      rval_binfo = binfo;
	      rval_binfo_h = binfo_h;
	    }
	  else
	    {
	      /* This is ambiguous.  */
	      errstr = "request for method `%D' is ambiguous";
	      rvals = error_mark_node;
	      break;
	    }
	}
    }
  {
    tree *tp = search_stack->first;
    tree *search_tail = tp + tail;

    while (tp < search_tail)
      {
	CLEAR_BINFO_FIELDS_MARKED (TREE_VALUE (TREE_CHAIN (*tp)));
	tp += 1;
      }
  }
  search_stack = pop_search_level (search_stack);

  if (entry)
    {
      if (errstr)
	{
	  tree error_string = my_build_string (errstr);
	  /* Save error message with entry.  */
	  TREE_TYPE (entry) = error_string;
	}
      else
	{
	  /* Mark entry as having no error string.  */
	  TREE_TYPE (entry) = NULL_TREE;
	  TREE_VALUE (entry) = rvals;
	}
    }

  if (errstr && protect)
    {
      cp_error (errstr, name);
      rvals = error_mark_node;
    }

  return rvals;
}

/* BREADTH-FIRST SEARCH ROUTINES.  */

/* Search a multiple inheritance hierarchy by breadth-first search.

   BINFO is an aggregate type, possibly in a multiple-inheritance hierarchy.
   TESTFN is a function, which, if true, means that our condition has been met,
   and its return value should be returned.
   QFN, if non-NULL, is a predicate dictating whether the type should
   even be queued.  */

static HOST_WIDE_INT
breadth_first_search (binfo, testfn, qfn)
     tree binfo;
     int (*testfn) PROTO((tree, int));
     int (*qfn) PROTO((tree, int));
{
  int head = 0, tail = 0;
  int rval = 0;

  search_stack = push_search_level (search_stack, &search_obstack);

  while (1)
    {
      tree binfos = BINFO_BASETYPES (binfo);
      int n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;
      int i;

      /* Process and/or queue base types.  */
      for (i = 0; i < n_baselinks; i++)
	{
	  tree base_binfo = TREE_VEC_ELT (binfos, i);

	  if (BINFO_MARKED (base_binfo) == 0
	      && (qfn == 0 || (*qfn) (binfo, i)))
	    {
	      SET_BINFO_MARKED (base_binfo);
	      obstack_ptr_grow (&search_obstack, binfo);
	      obstack_ptr_grow (&search_obstack, (HOST_WIDE_INT) i);
	      tail += 2;
	      if (tail >= search_stack->limit)
		my_friendly_abort (100);
	    }
	}
      /* Process head of queue, if one exists.  */
      if (head >= tail)
	{
	  rval = 0;
	  break;
	}

      binfo = search_stack->first[head++];
      i = (HOST_WIDE_INT) search_stack->first[head++];
      if (rval = (*testfn) (binfo, i))
	break;
      binfo = BINFO_BASETYPE (binfo, i);
    }
  {
    tree *tp = search_stack->first;
    tree *search_tail = tp + tail;
    while (tp < search_tail)
      {
	tree binfo = *tp++;
	int i = (HOST_WIDE_INT)(*tp++);
	CLEAR_BINFO_MARKED (BINFO_BASETYPE (binfo, i));
      }
  }

  search_stack = pop_search_level (search_stack);
  return rval;
}

/* Functions to use in breadth first searches.  */
typedef int (*pfi) PROTO((tree, int));

static tree declarator;

static tree
get_virtuals_named_this (binfo)
     tree binfo;
{
  tree fields;

  fields = lookup_fnfields (binfo, declarator, -1);
  /* fields cannot be error_mark_node */

  if (fields == 0)
    return 0;

  /* Get to the function decls, and return the first virtual function
     with this name, if there is one.  */
  while (fields)
    {
      tree fndecl;

      for (fndecl = TREE_VALUE (fields); fndecl; fndecl = DECL_CHAIN (fndecl))
	if (DECL_VINDEX (fndecl))
	  return fields;
      fields = next_baselink (fields);
    }
  return NULL_TREE;
}

static tree
get_virtual_destructor (binfo, i)
     tree binfo;
     int i;
{
  tree type = BINFO_TYPE (binfo);
  if (i >= 0)
    type = BINFO_TYPE (TREE_VEC_ELT (BINFO_BASETYPES (binfo), i));
  if (TYPE_HAS_DESTRUCTOR (type)
      && DECL_VINDEX (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), 1)))
    return TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), 1);
  return 0;
}

static int
tree_has_any_destructor_p (binfo, i)
     tree binfo;
     int i;
{
  tree type = BINFO_TYPE (binfo);
  if (i >= 0)
    type = BINFO_TYPE (TREE_VEC_ELT (BINFO_BASETYPES (binfo), i));
  return TYPE_NEEDS_DESTRUCTOR (type);
}

/* Given a class type TYPE, and a function decl FNDECL, look for a
   virtual function in TYPE's hierarchy which FNDECL could match as a
   virtual function.  It doesn't matter which one we find.

   DTORP is nonzero if we are looking for a destructor.  Destructors
   need special treatment because they do not match by name.  */

tree
get_matching_virtual (binfo, fndecl, dtorp)
     tree binfo, fndecl;
     int dtorp;
{
  tree tmp = NULL_TREE;

  /* Breadth first search routines start searching basetypes
     of TYPE, so we must perform first ply of search here.  */
  if (dtorp)
    {
      if (tree_has_any_destructor_p (binfo, -1))
	tmp = get_virtual_destructor (binfo, -1);

      if (tmp)
	return tmp;

      tmp = (tree) breadth_first_search (binfo,
					 (pfi) get_virtual_destructor,
					 tree_has_any_destructor_p);
      return tmp;
    }
  else
    {
      tree drettype, dtypes, btypes, instptr_type;
      tree basetype = DECL_CLASS_CONTEXT (fndecl);
      tree baselink, best = NULL_TREE;
      tree name = DECL_ASSEMBLER_NAME (fndecl);

      declarator = DECL_NAME (fndecl);
      if (IDENTIFIER_VIRTUAL_P (declarator) == 0)
	return NULL_TREE;

      baselink = get_virtuals_named_this (binfo);
      if (baselink == NULL_TREE)
	return NULL_TREE;

      drettype = TREE_TYPE (TREE_TYPE (fndecl));
      dtypes = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
      if (DECL_STATIC_FUNCTION_P (fndecl))
	instptr_type = NULL_TREE;
      else
	instptr_type = TREE_TYPE (TREE_VALUE (dtypes));

      for (; baselink; baselink = next_baselink (baselink))
	{
	  for (tmp = TREE_VALUE (baselink); tmp; tmp = DECL_CHAIN (tmp))
	    {
	      if (! DECL_VINDEX (tmp))
		continue;

	      btypes = TYPE_ARG_TYPES (TREE_TYPE (tmp));
	      if (instptr_type == NULL_TREE)
		{
		  if (compparms (TREE_CHAIN (btypes), dtypes, 3))
		    /* Caller knows to give error in this case.  */
		    return tmp;
		  return NULL_TREE;
		}

	      if ((TYPE_READONLY (TREE_TYPE (TREE_VALUE (btypes)))
		   == TYPE_READONLY (instptr_type))
		  && compparms (TREE_CHAIN (btypes), TREE_CHAIN (dtypes), 3))
		{
		  tree brettype = TREE_TYPE (TREE_TYPE (tmp));
		  if (comptypes (brettype, drettype, 1))
		    /* OK */;
		  else if
		    (TREE_CODE (brettype) == TREE_CODE (drettype)
		     && (TREE_CODE (brettype) == POINTER_TYPE
			 || TREE_CODE (brettype) == REFERENCE_TYPE)
		     && comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (brettype)),
				   TYPE_MAIN_VARIANT (TREE_TYPE (drettype)),
				   0))
		      /* covariant return type */
		    {
		      tree b = TREE_TYPE (brettype), d = TREE_TYPE (drettype);
		      if (TYPE_MAIN_VARIANT (b) != TYPE_MAIN_VARIANT (d))
			{
			  tree binfo = get_binfo (b, d, 1);
			  if (binfo != error_mark_node
			      && (! BINFO_OFFSET_ZEROP (binfo)
				  || TREE_VIA_VIRTUAL (binfo)))
			    sorry ("adjusting pointers for covariant returns");
			}
		      if (TYPE_READONLY (d) > TYPE_READONLY (b))
			{
			  cp_error_at ("return type of `%#D' adds const", fndecl);
			  cp_error_at ("  overriding definition as `%#D'",
				       tmp);
			}
		      else if (TYPE_VOLATILE (d) > TYPE_VOLATILE (b))
			{
			  cp_error_at ("return type of `%#D' adds volatile",
				    fndecl);
			  cp_error_at ("  overriding definition as `%#D'",
				       tmp);
			}
		    }
		  else if (IS_AGGR_TYPE_2 (brettype, drettype)
			   && comptypes (brettype, drettype, 0))
		    {
		      error ("invalid covariant return type (must use pointer or reference)");
		      cp_error_at ("  overriding `%#D'", tmp);
		      cp_error_at ("  with `%#D'", fndecl);
		    }
		  else if (IDENTIFIER_ERROR_LOCUS (name) == NULL_TREE)
		    {
		      cp_error_at ("conflicting return type specified for virtual function `%#D'", fndecl);
		      cp_error_at ("  overriding definition as `%#D'", tmp);
		      SET_IDENTIFIER_ERROR_LOCUS (name, basetype);
		    }
		  break;
		}
	    }
	  if (tmp)
	    {
	      best = tmp;
	      break;
	    }
	}

      return best;
    }
}

/* Return the list of virtual functions which are abstract in type
   TYPE that come from non virtual base classes.  See
   expand_direct_vtbls_init for the style of search we do.  */

static tree
get_abstract_virtuals_1 (binfo, do_self, abstract_virtuals)
     tree binfo;
     int do_self;
     tree abstract_virtuals;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      int is_not_base_vtable
	= i != CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (binfo));
      if (! TREE_VIA_VIRTUAL (base_binfo))
	abstract_virtuals
	  = get_abstract_virtuals_1 (base_binfo, is_not_base_vtable,
				     abstract_virtuals);
    }
  /* Should we use something besides CLASSTYPE_VFIELDS? */
  if (do_self && CLASSTYPE_VFIELDS (BINFO_TYPE (binfo)))
    {
      tree virtuals = BINFO_VIRTUALS (binfo);

      skip_rtti_stuff (&virtuals);

      while (virtuals)
	{
	  tree base_pfn = FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (virtuals));
	  tree base_fndecl = TREE_OPERAND (base_pfn, 0);
	  if (DECL_ABSTRACT_VIRTUAL_P (base_fndecl))
	    abstract_virtuals = tree_cons (NULL_TREE, base_fndecl, abstract_virtuals);
	  virtuals = TREE_CHAIN (virtuals);
	}
    }
  return abstract_virtuals;
}

/* Return the list of virtual functions which are abstract in type TYPE.
   This information is cached, and so must be built on a
   non-temporary obstack.  */

tree
get_abstract_virtuals (type)
     tree type;
{
  tree vbases;
  tree abstract_virtuals = CLASSTYPE_ABSTRACT_VIRTUALS (type);

  /* First get all from non-virtual bases.  */
  abstract_virtuals
    = get_abstract_virtuals_1 (TYPE_BINFO (type), 1, abstract_virtuals);
					       
  for (vbases = CLASSTYPE_VBASECLASSES (type); vbases; vbases = TREE_CHAIN (vbases))
    {
      tree virtuals = BINFO_VIRTUALS (vbases);

      skip_rtti_stuff (&virtuals);

      while (virtuals)
	{
	  tree base_pfn = FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (virtuals));
	  tree base_fndecl = TREE_OPERAND (base_pfn, 0);
	  if (DECL_ABSTRACT_VIRTUAL_P (base_fndecl))
	    abstract_virtuals = tree_cons (NULL_TREE, base_fndecl, abstract_virtuals);
	  virtuals = TREE_CHAIN (virtuals);
	}
    }
  return nreverse (abstract_virtuals);
}

/* For the type TYPE, return a list of member functions available from
   base classes with name NAME.  The TREE_VALUE of the list is a chain of
   member functions with name NAME.  The TREE_PURPOSE of the list is a
   basetype, or a list of base types (in reverse order) which were
   traversed to reach the chain of member functions.  If we reach a base
   type which provides a member function of name NAME, and which has at
   most one base type itself, then we can terminate the search.  */

tree
get_baselinks (type_as_binfo_list, type, name)
     tree type_as_binfo_list;
     tree type, name;
{
  int head = 0, tail = 0, idx;
  tree rval = 0, nval = 0;
  tree basetypes = type_as_binfo_list;
  tree binfo = TYPE_BINFO (type);

  search_stack = push_search_level (search_stack, &search_obstack);

  while (1)
    {
      tree binfos = BINFO_BASETYPES (binfo);
      int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

      /* Process and/or queue base types.  */
      for (i = 0; i < n_baselinks; i++)
	{
	  tree base_binfo = TREE_VEC_ELT (binfos, i);
	  tree btypes;

	  btypes = hash_tree_cons (TREE_VIA_PUBLIC (base_binfo),
				   TREE_VIA_VIRTUAL (base_binfo),
				   TREE_VIA_PROTECTED (base_binfo),
				   NULL_TREE, base_binfo,
				   basetypes);
	  obstack_ptr_grow (&search_obstack, btypes);
	  search_stack->first = (tree *)obstack_base (&search_obstack);
	  tail += 1;
	}

    dont_queue:
      /* Process head of queue, if one exists.  */
      if (head >= tail)
	break;

      basetypes = search_stack->first[head++];
      binfo = TREE_VALUE (basetypes);
      type = BINFO_TYPE (binfo);
      idx = lookup_fnfields_1 (type, name);
      if (idx >= 0)
	{
	  nval = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), idx);
	  rval = hash_tree_cons (0, 0, 0, basetypes, nval, rval);
	  if (TYPE_BINFO_BASETYPES (type) == 0)
	    goto dont_queue;
	  else if (TREE_VEC_LENGTH (TYPE_BINFO_BASETYPES (type)) == 1)
	    {
	      if (CLASSTYPE_BASELINK_VEC (type))
		TREE_TYPE (rval) = TREE_VEC_ELT (CLASSTYPE_BASELINK_VEC (type), idx);
	      goto dont_queue;
	    }
	}
      nval = NULL_TREE;
    }

  search_stack = pop_search_level (search_stack);
  return rval;
}

tree
next_baselink (baselink)
     tree baselink;
{
  tree tmp = TREE_TYPE (baselink);
  baselink = TREE_CHAIN (baselink);
  while (tmp)
    {
      /* @@ does not yet add previous base types.  */
      baselink = tree_cons (TREE_PURPOSE (tmp), TREE_VALUE (tmp),
			    baselink);
      TREE_TYPE (baselink) = TREE_TYPE (tmp);
      tmp = TREE_CHAIN (tmp);
    }
  return baselink;
}

/* DEPTH-FIRST SEARCH ROUTINES.  */

/* Assign unique numbers to _CLASSTYPE members of the lattice
   specified by TYPE.  The root nodes are marked first; the nodes
   are marked depth-fisrt, left-right.  */

static int cid;

/* Matrix implementing a relation from CLASSTYPE X CLASSTYPE => INT.
   Relation yields 1 if C1 <= C2, 0 otherwise.  */
typedef char mi_boolean;
static mi_boolean *mi_matrix;

/* Type for which this matrix is defined.  */
static tree mi_type;

/* Size of the matrix for indexing purposes.  */
static int mi_size;

/* Return nonzero if class C2 derives from class C1.  */
#define BINFO_DERIVES_FROM(C1, C2)	\
  ((mi_matrix+mi_size*(BINFO_CID (C1)-1))[BINFO_CID (C2)-1])
#define TYPE_DERIVES_FROM(C1, C2)	\
  ((mi_matrix+mi_size*(CLASSTYPE_CID (C1)-1))[CLASSTYPE_CID (C2)-1])
#define BINFO_DERIVES_FROM_STAR(C)	\
  (mi_matrix+(BINFO_CID (C)-1))

/* This routine converts a pointer to be a pointer of an immediate
   base class.  The normal convert_pointer_to routine would diagnose
   the conversion as ambiguous, under MI code that has the base class
   as an ambiguous base class.  */

static tree
convert_pointer_to_single_level (to_type, expr)
     tree to_type, expr;
{
  tree binfo_of_derived;
  tree last;

  binfo_of_derived = TYPE_BINFO (TREE_TYPE (TREE_TYPE (expr)));
  last = get_binfo (to_type, TREE_TYPE (TREE_TYPE (expr)), 0);
  BINFO_INHERITANCE_CHAIN (last) = binfo_of_derived;
  BINFO_INHERITANCE_CHAIN (binfo_of_derived) = NULL_TREE;
  return build_vbase_path (PLUS_EXPR, build_pointer_type (to_type), expr, last, 1);
}

/* The main function which implements depth first search.

   This routine has to remember the path it walked up, when
   dfs_init_vbase_pointers is the work function, as otherwise there
   would be no record.  */

static void
dfs_walk (binfo, fn, qfn)
     tree binfo;
     void (*fn) PROTO((tree));
     int (*qfn) PROTO((tree));
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);

      if (qfn == 0 || (*qfn)(base_binfo))
	{
	  if (TREE_CODE (BINFO_TYPE (base_binfo)) == TEMPLATE_TYPE_PARM)
	    /* Pass */;
	  else if (fn == dfs_init_vbase_pointers)
	    {
	      /* When traversing an arbitrary MI hierarchy, we need to keep
		 a record of the path we took to get down to the final base
		 type, as otherwise there would be no record of it, and just
		 trying to blindly convert at the bottom would be ambiguous.

		 The easiest way is to do the conversions one step at a time,
		 as we know we want the immediate base class at each step.

		 The only special trick to converting one step at a time,
		 is that when we hit the last virtual base class, we must
		 use the SLOT value for it, and not use the normal convert
		 routine.  We use the last virtual base class, as in our
		 implementation, we have pointers to all virtual base
		 classes in the base object.  */

	      tree saved_vbase_decl_ptr_intermediate
		= vbase_decl_ptr_intermediate;

	      if (TREE_VIA_VIRTUAL (base_binfo))
		{
		  /* No need for the conversion here, as we know it is the
		     right type.  */
		  vbase_decl_ptr_intermediate
		    = CLASSTYPE_SEARCH_SLOT (BINFO_TYPE (base_binfo));
		}
	      else
		{
		  vbase_decl_ptr_intermediate
		    = convert_pointer_to_single_level (BINFO_TYPE (base_binfo),
						       vbase_decl_ptr_intermediate);
		}

	      dfs_walk (base_binfo, fn, qfn);

	      vbase_decl_ptr_intermediate = saved_vbase_decl_ptr_intermediate;
	    }
	  else
	    dfs_walk (base_binfo, fn, qfn);
	}
    }

  fn (binfo);
}

/* Predicate functions which serve for dfs_walk.  */
static int numberedp (binfo) tree binfo;
{ return BINFO_CID (binfo); }
static int unnumberedp (binfo) tree binfo;
{ return BINFO_CID (binfo) == 0; }

static int markedp (binfo) tree binfo;
{ return BINFO_MARKED (binfo); }
static int unmarkedp (binfo) tree binfo;
{ return BINFO_MARKED (binfo) == 0; }

#if 0
static int bfs_markedp (binfo, i) tree binfo; int i;
{ return BINFO_MARKED (BINFO_BASETYPE (binfo, i)); }
static int bfs_unmarkedp (binfo, i) tree binfo; int i;
{ return BINFO_MARKED (BINFO_BASETYPE (binfo, i)) == 0; }
static int bfs_marked_vtable_pathp (binfo, i) tree binfo; int i;
{ return BINFO_VTABLE_PATH_MARKED (BINFO_BASETYPE (binfo, i)); }
static int bfs_unmarked_vtable_pathp (binfo, i) tree binfo; int i;
{ return BINFO_VTABLE_PATH_MARKED (BINFO_BASETYPE (binfo, i)) == 0; }
static int bfs_marked_new_vtablep (binfo, i) tree binfo; int i;
{ return BINFO_NEW_VTABLE_MARKED (BINFO_BASETYPE (binfo, i)); }
static int bfs_unmarked_new_vtablep (binfo, i) tree binfo; int i;
{ return BINFO_NEW_VTABLE_MARKED (BINFO_BASETYPE (binfo, i)) == 0; }
#endif

static int marked_vtable_pathp (binfo) tree binfo;
{ return BINFO_VTABLE_PATH_MARKED (binfo); }
static int unmarked_vtable_pathp (binfo) tree binfo;
{ return BINFO_VTABLE_PATH_MARKED (binfo) == 0; }
static int marked_new_vtablep (binfo) tree binfo;
{ return BINFO_NEW_VTABLE_MARKED (binfo); }
static int unmarked_new_vtablep (binfo) tree binfo;
{ return BINFO_NEW_VTABLE_MARKED (binfo) == 0; }

#if 0
static int dfs_search_slot_nonempty_p (binfo) tree binfo;
{ return CLASSTYPE_SEARCH_SLOT (BINFO_TYPE (binfo)) != 0; }
#endif

static int dfs_debug_unmarkedp (binfo) tree binfo;
{ return CLASSTYPE_DEBUG_REQUESTED (BINFO_TYPE (binfo)) == 0; }

/* The worker functions for `dfs_walk'.  These do not need to
   test anything (vis a vis marking) if they are paired with
   a predicate function (above).  */

/* Assign each type within the lattice a number which is unique
   in the lattice.  The first number assigned is 1.  */

static void
dfs_number (binfo)
     tree binfo;
{
  BINFO_CID (binfo) = ++cid;
}

static void
dfs_unnumber (binfo)
     tree binfo;
{
  BINFO_CID (binfo) = 0;
}

#if 0
static void
dfs_mark (binfo) tree binfo;
{ SET_BINFO_MARKED (binfo); }
#endif

static void
dfs_unmark (binfo) tree binfo;
{ CLEAR_BINFO_MARKED (binfo); }

#if 0
static void
dfs_mark_vtable_path (binfo) tree binfo;
{ SET_BINFO_VTABLE_PATH_MARKED (binfo); }

static void
dfs_unmark_vtable_path (binfo) tree binfo;
{ CLEAR_BINFO_VTABLE_PATH_MARKED (binfo); }

static void
dfs_mark_new_vtable (binfo) tree binfo;
{ SET_BINFO_NEW_VTABLE_MARKED (binfo); }

static void
dfs_unmark_new_vtable (binfo) tree binfo;
{ CLEAR_BINFO_NEW_VTABLE_MARKED (binfo); }

static void
dfs_clear_search_slot (binfo) tree binfo;
{ CLASSTYPE_SEARCH_SLOT (BINFO_TYPE (binfo)) = 0; }
#endif

static void
dfs_debug_mark (binfo)
     tree binfo;
{
  tree t = BINFO_TYPE (binfo);

  /* Use heuristic that if there are virtual functions,
     ignore until we see a non-inline virtual function.  */
  tree methods = CLASSTYPE_METHOD_VEC (t);

  CLASSTYPE_DEBUG_REQUESTED (t) = 1;

  if (methods == 0)
    return;

  /* If interface info is known, either we've already emitted the debug
     info or we don't need to.  */
  if (CLASSTYPE_INTERFACE_KNOWN (t)
      || (write_virtuals == 2 && TYPE_VIRTUAL_P (t)))
    return;

  /* If debug info is requested from this context for this type, supply it.
     If debug info is requested from another context for this type,
     see if some third context can supply it.  */
  if (current_function_decl == NULL_TREE
      || DECL_CLASS_CONTEXT (current_function_decl) != t)
    {
      if (TREE_VEC_ELT (methods, 1))
	methods = TREE_VEC_ELT (methods, 1);
      else if (TREE_VEC_ELT (methods, 0))
	methods = TREE_VEC_ELT (methods, 0);
      else
	methods = TREE_VEC_ELT (methods, 2);
      while (methods)
	{
	  if (DECL_VINDEX (methods)
	      && DECL_THIS_INLINE (methods) == 0
	      && DECL_ABSTRACT_VIRTUAL_P (methods) == 0)
	    {
	      /* Somebody, somewhere is going to have to define this
		 virtual function.  When they do, they will provide
		 the debugging info.  */
	      return;
	    }
	  methods = TREE_CHAIN (methods);
	}
    }
  /* We cannot rely on some alien method to solve our problems,
     so we must write out the debug info ourselves.  */
  TYPE_DECL_SUPPRESS_DEBUG (TYPE_NAME (t)) = 0;
  rest_of_type_compilation (t, global_bindings_p ());
}

/*  Attach to the type of the virtual base class, the pointer to the
    virtual base class, given the global pointer vbase_decl_ptr.

    We use the global vbase_types.  ICK!  */

static void
dfs_find_vbases (binfo)
     tree binfo;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  for (i = n_baselinks-1; i >= 0; i--)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);

      if (TREE_VIA_VIRTUAL (base_binfo)
	  && CLASSTYPE_SEARCH_SLOT (BINFO_TYPE (base_binfo)) == 0)
	{
	  tree vbase = BINFO_TYPE (base_binfo);
	  tree binfo = binfo_member (vbase, vbase_types);

	  CLASSTYPE_SEARCH_SLOT (vbase)
	    = build (PLUS_EXPR, build_pointer_type (vbase),
		     vbase_decl_ptr, BINFO_OFFSET (binfo));
	}
    }
  SET_BINFO_VTABLE_PATH_MARKED (binfo);
  SET_BINFO_NEW_VTABLE_MARKED (binfo);
}

static void
dfs_init_vbase_pointers (binfo)
     tree binfo;
{
  tree type = BINFO_TYPE (binfo);
  tree fields = TYPE_FIELDS (type);
  tree this_vbase_ptr;

  CLEAR_BINFO_VTABLE_PATH_MARKED (binfo);

#if 0
  /* See finish_struct_1 for when we can enable this.  */
  /* If we have a vtable pointer first, skip it.  */
  if (VFIELD_NAME_P (DECL_NAME (fields)))
    fields = TREE_CHAIN (fields);
#endif

  if (fields == NULL_TREE
      || DECL_NAME (fields) == NULL_TREE
      || ! VBASE_NAME_P (DECL_NAME (fields)))
    return;

  this_vbase_ptr = vbase_decl_ptr_intermediate;

  if (build_pointer_type (type) != TYPE_MAIN_VARIANT (TREE_TYPE (this_vbase_ptr)))
    my_friendly_abort (125);

  while (fields && DECL_NAME (fields)
	 && VBASE_NAME_P (DECL_NAME (fields)))
    {
      tree ref = build (COMPONENT_REF, TREE_TYPE (fields),
			build_indirect_ref (this_vbase_ptr, NULL_PTR), fields);
      tree init = CLASSTYPE_SEARCH_SLOT (TREE_TYPE (TREE_TYPE (fields)));
      vbase_init_result = tree_cons (binfo_member (TREE_TYPE (TREE_TYPE (fields)),
						   vbase_types),
				     build_modify_expr (ref, NOP_EXPR, init),
				     vbase_init_result);
      fields = TREE_CHAIN (fields);
    }
}

/* Sometimes this needs to clear both VTABLE_PATH and NEW_VTABLE.  Other
   times, just NEW_VTABLE, but optimizer should make both with equal
   efficiency (though it does not currently).  */

static void
dfs_clear_vbase_slots (binfo)
     tree binfo;
{
  tree type = BINFO_TYPE (binfo);
  CLASSTYPE_SEARCH_SLOT (type) = 0;
  CLEAR_BINFO_VTABLE_PATH_MARKED (binfo);
  CLEAR_BINFO_NEW_VTABLE_MARKED (binfo);
}

tree
init_vbase_pointers (type, decl_ptr)
     tree type;
     tree decl_ptr;
{
  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
    {
      int old_flag = flag_this_is_variable;
      tree binfo = TYPE_BINFO (type);
      flag_this_is_variable = -2;
      vbase_types = CLASSTYPE_VBASECLASSES (type);
      vbase_decl_ptr = vbase_decl_ptr_intermediate = decl_ptr;
      vbase_init_result = NULL_TREE;
      dfs_walk (binfo, dfs_find_vbases, unmarked_vtable_pathp);
      dfs_walk (binfo, dfs_init_vbase_pointers, marked_vtable_pathp);
      dfs_walk (binfo, dfs_clear_vbase_slots, marked_new_vtablep);
      flag_this_is_variable = old_flag;
      return vbase_init_result;
    }
  return 0;
}

/* get the virtual context (the vbase that directly contains the
   DECL_CLASS_CONTEXT of the FNDECL) that the given FNDECL is declared in,
   or NULL_TREE if there is none.

   FNDECL must come from a virtual table from a virtual base to ensure that
   there is only one possible DECL_CLASS_CONTEXT.

   We know that if there is more than one place (binfo) the fndecl that the
   declared, they all refer to the same binfo.  See get_class_offset_1 for
   the check that ensures this.  */

static tree
virtual_context (fndecl, t, vbase)
     tree fndecl, t, vbase;
{
  tree path;
  if (get_base_distance (DECL_CLASS_CONTEXT (fndecl), t, 0, &path) < 0)
    {
      /* DECL_CLASS_CONTEXT can be ambiguous in t.  */
      if (get_base_distance (DECL_CLASS_CONTEXT (fndecl), vbase, 0, &path) >= 0)
	{
	  while (path)
	    {
	      /* Not sure if checking path == vbase is necessary here, but just in
		 case it is.  */
	      if (TREE_VIA_VIRTUAL (path) || path == vbase)
		return binfo_member (BINFO_TYPE (path), CLASSTYPE_VBASECLASSES (t));
	      path = BINFO_INHERITANCE_CHAIN (path);
	    }
	}
      /* This shouldn't happen, I don't want errors! */
      warning ("recoverable compiler error, fixups for virtual function");
      return vbase;
    }
  while (path)
    {
      if (TREE_VIA_VIRTUAL (path))
	return binfo_member (BINFO_TYPE (path), CLASSTYPE_VBASECLASSES (t));
      path = BINFO_INHERITANCE_CHAIN (path);
    }
  return 0;
}

/* Fixups upcast offsets for one vtable.
   Entries may stay within the VBASE given, or
   they may upcast into a direct base, or
   they may upcast into a different vbase.

   We only need to do fixups in case 2 and 3.  In case 2, we add in
   the virtual base offset to effect an upcast, in case 3, we add in
   the virtual base offset to effect an upcast, then subtract out the
   offset for the other virtual base, to effect a downcast into it.

   This routine mirrors fixup_vtable_deltas in functionality, though
   this one is runtime based, and the other is compile time based.
   Conceivably that routine could be removed entirely, and all fixups
   done at runtime.

   VBASE_OFFSETS is an association list of virtual bases that contains
   offset information for the virtual bases, so the offsets are only
   calculated once.  The offsets are computed by where we think the
   vbase should be (as noted by the CLASSTYPE_SEARCH_SLOT) minus where
   the vbase really is.  */

static void
expand_upcast_fixups (binfo, addr, orig_addr, vbase, vbase_addr, t,
		      vbase_offsets)
     tree binfo, addr, orig_addr, vbase, vbase_addr, t, *vbase_offsets;
{
  tree virtuals = BINFO_VIRTUALS (binfo);
  tree vc;
  tree delta;
  unsigned HOST_WIDE_INT n;
  
  delta = purpose_member (vbase, *vbase_offsets);
  if (! delta)
    {
      delta = CLASSTYPE_SEARCH_SLOT (BINFO_TYPE (vbase));
      delta = build (MINUS_EXPR, ptrdiff_type_node, delta, vbase_addr);
      delta = save_expr (delta);
      delta = tree_cons (vbase, delta, *vbase_offsets);
      *vbase_offsets = delta;
    }

  n = skip_rtti_stuff (&virtuals);

  while (virtuals)
    {
      tree current_fndecl = TREE_VALUE (virtuals);
      current_fndecl = FNADDR_FROM_VTABLE_ENTRY (current_fndecl);
      current_fndecl = TREE_OPERAND (current_fndecl, 0);
      if (current_fndecl
	  && current_fndecl != abort_fndecl
	  && (vc=virtual_context (current_fndecl, t, vbase)) != vbase)
	{
	  /* This may in fact need a runtime fixup.  */
	  tree idx = build_int_2 (n, 0);
	  tree vtbl = BINFO_VTABLE (binfo);
	  tree nvtbl = lookup_name (DECL_NAME (vtbl), 0);
	  tree aref, ref, naref;
	  tree old_delta, new_delta;
	  tree init;

	  if (nvtbl == NULL_TREE
	      || nvtbl == IDENTIFIER_GLOBAL_VALUE (DECL_NAME (vtbl)))
	    {
	      /* Dup it if it isn't in local scope yet.  */
	      nvtbl = build_decl (VAR_DECL,
				  DECL_NAME (vtbl),
				  TYPE_MAIN_VARIANT (TREE_TYPE (BINFO_VTABLE (binfo))));
	      DECL_ALIGN (nvtbl) = MAX (TYPE_ALIGN (double_type_node),
					DECL_ALIGN (nvtbl));
	      TREE_READONLY (nvtbl) = 0;
	      DECL_ARTIFICIAL (nvtbl) = 1;
	      nvtbl = pushdecl (nvtbl);
	      init = NULL_TREE;
	      cp_finish_decl (nvtbl, init, NULL_TREE, 0, LOOKUP_ONLYCONVERTING);
	      DECL_VIRTUAL_P (nvtbl) = 1;
	      DECL_CONTEXT (nvtbl) = t;
	      init = build (MODIFY_EXPR, TREE_TYPE (nvtbl),
			    nvtbl, vtbl);
	      TREE_SIDE_EFFECTS (init) = 1;
	      expand_expr_stmt (init);
	      /* Update the vtable pointers as necessary.  */
	      ref = build_vfield_ref (build_indirect_ref (addr, NULL_PTR), DECL_CONTEXT (CLASSTYPE_VFIELD (BINFO_TYPE (binfo))));
	      expand_expr_stmt (build_modify_expr (ref, NOP_EXPR,
						   build_unary_op (ADDR_EXPR, nvtbl, 0)));
	    }
	  assemble_external (vtbl);
	  aref = build_array_ref (vtbl, idx);
	  naref = build_array_ref (nvtbl, idx);
	  old_delta = build_component_ref (aref, delta_identifier, NULL_TREE, 0);
	  new_delta = build_component_ref (naref, delta_identifier, NULL_TREE, 0);

	  /* This is a upcast, so we have to add the offset for the
	     virtual base.  */
	  old_delta = build_binary_op (PLUS_EXPR, old_delta,
				       TREE_VALUE (delta), 0);
	  if (vc)
	    {
	      /* If this is set, we need to subtract out the delta
		 adjustments for the other virtual base that we
		 downcast into.  */
	      tree vc_delta = purpose_member (vc, *vbase_offsets);
	      if (! vc_delta)
		{
		  tree vc_addr = convert_pointer_to_real (vc, orig_addr);
		  vc_delta = CLASSTYPE_SEARCH_SLOT (BINFO_TYPE (vc));
		  vc_delta = build (MINUS_EXPR, ptrdiff_type_node,
				    vc_delta, vc_addr);
		  vc_delta = save_expr (vc_delta);
		  *vbase_offsets = tree_cons (vc, vc_delta, *vbase_offsets);
		}
	      else
		vc_delta = TREE_VALUE (vc_delta);
   
	      /* This is a downcast, so we have to subtract the offset
		 for the virtual base.  */
	      old_delta = build_binary_op (MINUS_EXPR, old_delta, vc_delta, 0);
	    }

	  TREE_READONLY (new_delta) = 0;
	  expand_expr_stmt (build_modify_expr (new_delta, NOP_EXPR,
					       old_delta));
	}
      ++n;
      virtuals = TREE_CHAIN (virtuals);
    }
}

/* Fixup upcast offsets for all direct vtables.  Patterned after
   expand_direct_vtbls_init.  */

static void
fixup_virtual_upcast_offsets (real_binfo, binfo, init_self, can_elide, addr, orig_addr, type, vbase, vbase_offsets)
     tree real_binfo, binfo;
     int init_self, can_elide;
     tree addr, orig_addr, type, vbase, *vbase_offsets;
{
  tree real_binfos = BINFO_BASETYPES (real_binfo);
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = real_binfos ? TREE_VEC_LENGTH (real_binfos) : 0;

  for (i = 0; i < n_baselinks; i++)
    {
      tree real_base_binfo = TREE_VEC_ELT (real_binfos, i);
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      int is_not_base_vtable
	= i != CLASSTYPE_VFIELD_PARENT (BINFO_TYPE (real_binfo));
      if (! TREE_VIA_VIRTUAL (real_base_binfo))
	fixup_virtual_upcast_offsets (real_base_binfo, base_binfo,
				      is_not_base_vtable, can_elide, addr,
				      orig_addr, type, vbase, vbase_offsets);
    }
#if 0
  /* Before turning this on, make sure it is correct.  */
  if (can_elide && ! BINFO_MODIFIED (binfo))
    return;
#endif
  /* Should we use something besides CLASSTYPE_VFIELDS? */
  if (init_self && CLASSTYPE_VFIELDS (BINFO_TYPE (real_binfo)))
    {
      tree new_addr = convert_pointer_to_real (binfo, addr);
      expand_upcast_fixups (real_binfo, new_addr, orig_addr, vbase, addr,
			    type, vbase_offsets);
    }
}

/* Build a COMPOUND_EXPR which when expanded will generate the code
   needed to initialize all the virtual function table slots of all
   the virtual baseclasses.  MAIN_BINFO is the binfo which determines
   the virtual baseclasses to use; TYPE is the type of the object to
   which the initialization applies.  TRUE_EXP is the true object we
   are initializing, and DECL_PTR is the pointer to the sub-object we
   are initializing.

   When USE_COMPUTED_OFFSETS is non-zero, we can assume that the
   object was laid out by a top-level constructor and the computed
   offsets are valid to store vtables.  When zero, we must store new
   vtables through virtual baseclass pointers.

   We setup and use the globals: vbase_decl_ptr, vbase_types
   ICK!  */

void
expand_indirect_vtbls_init (binfo, true_exp, decl_ptr)
     tree binfo;
     tree true_exp, decl_ptr;
{
  tree type = BINFO_TYPE (binfo);

  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
    {
      rtx fixup_insns = NULL_RTX;
      tree vbases = CLASSTYPE_VBASECLASSES (type);
      vbase_types = vbases;
      vbase_decl_ptr = true_exp ? build_unary_op (ADDR_EXPR, true_exp, 0) : decl_ptr;

      dfs_walk (binfo, dfs_find_vbases, unmarked_new_vtablep);

      /* Initialized with vtables of type TYPE.  */
      for (; vbases; vbases = TREE_CHAIN (vbases))
	{
	  tree addr;

	  addr = convert_pointer_to_vbase (TREE_TYPE (vbases), vbase_decl_ptr);

	  /* Do all vtables from this virtual base.  */
	  /* This assumes that virtual bases can never serve as parent
	     binfos.  (in the CLASSTYPE_VFIELD_PARENT sense)  */
	  expand_direct_vtbls_init (vbases, TYPE_BINFO (BINFO_TYPE (vbases)),
				    1, 0, addr);

	  /* Now we adjust the offsets for virtual functions that
	     cross virtual boundaries on an implicit upcast on vf call
	     so that the layout of the most complete type is used,
	     instead of assuming the layout of the virtual bases from
	     our current type.  */

	  if (flag_vtable_thunks)
	    {
	      /* We don't have dynamic thunks yet!
		 So for now, just fail silently.  */
	    }
	  else
	    {
	      tree vbase_offsets = NULL_TREE;
	      push_to_sequence (fixup_insns);
	      fixup_virtual_upcast_offsets (vbases,
					    TYPE_BINFO (BINFO_TYPE (vbases)),
					    1, 0, addr, vbase_decl_ptr,
					    type, vbases, &vbase_offsets);
	      fixup_insns = get_insns ();
	      end_sequence ();
	    }
	}

      if (fixup_insns)
	{
	  extern tree in_charge_identifier;
	  tree in_charge_node = lookup_name (in_charge_identifier, 0);
	  if (! in_charge_node)
	    {
	      warning ("recoverable internal compiler error, nobody's in charge!");
	      in_charge_node = integer_zero_node;
	    }
	  in_charge_node = build_binary_op (EQ_EXPR, in_charge_node, integer_zero_node, 1);
	  expand_start_cond (in_charge_node, 0);
	  emit_insns (fixup_insns);
	  expand_end_cond ();
	}

      dfs_walk (binfo, dfs_clear_vbase_slots, marked_new_vtablep);
    }
}

/* get virtual base class types.
   This adds type to the vbase_types list in reverse dfs order.
   Ordering is very important, so don't change it.  */

static void
dfs_get_vbase_types (binfo)
     tree binfo;
{
  if (TREE_VIA_VIRTUAL (binfo) && ! BINFO_VBASE_MARKED (binfo))
    {
      vbase_types = make_binfo (integer_zero_node, binfo,
				BINFO_VTABLE (binfo),
				BINFO_VIRTUALS (binfo), vbase_types);
      TREE_VIA_VIRTUAL (vbase_types) = 1;
      SET_BINFO_VBASE_MARKED (binfo);
    }
  SET_BINFO_MARKED (binfo);
}

/* get a list of virtual base classes in dfs order.  */

tree
get_vbase_types (type)
     tree type;
{
  tree vbases;
  tree binfo;

  if (TREE_CODE (type) == TREE_VEC)
    binfo = type;
  else
    binfo = TYPE_BINFO (type);

  vbase_types = NULL_TREE;
  dfs_walk (binfo, dfs_get_vbase_types, unmarkedp);
  dfs_walk (binfo, dfs_unmark, markedp);
  /* Rely upon the reverse dfs ordering from dfs_get_vbase_types, and now
     reverse it so that we get normal dfs ordering.  */
  vbase_types = nreverse (vbase_types);

  /* unmark marked vbases */
  for (vbases = vbase_types; vbases; vbases = TREE_CHAIN (vbases))
    CLEAR_BINFO_VBASE_MARKED (vbases);

  return vbase_types;
}

static void
dfs_record_inheritance (binfo)
     tree binfo;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;
  mi_boolean *derived_row = BINFO_DERIVES_FROM_STAR (binfo);

  for (i = n_baselinks-1; i >= 0; i--)
    {
      int j;
      tree base_binfo = TREE_VEC_ELT (binfos, i);
      tree baseclass = BINFO_TYPE (base_binfo);
      mi_boolean *base_row = BINFO_DERIVES_FROM_STAR (base_binfo);

      if (TREE_CODE (baseclass) == TEMPLATE_TYPE_PARM)
	continue;
      my_friendly_assert (CLASSTYPE_CID (baseclass) != 0, 2365);

      /* Don't search if there's nothing there!  MI_SIZE can be
	 zero as a result of parse errors.  */
      if (TYPE_BINFO_BASETYPES (baseclass) && mi_size > 0)
	for (j = mi_size*(CLASSTYPE_CID (baseclass)-1); j >= 0; j -= mi_size)
	  derived_row[j] |= base_row[j];
      TYPE_DERIVES_FROM (baseclass, BINFO_TYPE (binfo)) = 1;
    }

  SET_BINFO_MARKED (binfo);
}

/* Given a _CLASSTYPE node in a multiple inheritance lattice,
   convert the lattice into a simple relation such that,
   given to CIDs, C1 and C2, one can determine if C1 <= C2
   or C2 <= C1 or C1 <> C2.

   Once constructed, we walk the lattice depth fisrt,
   applying various functions to elements as they are encountered.

   We use xmalloc here, in case we want to randomly free these tables.  */

#define SAVE_MI_MATRIX

void
build_mi_matrix (type)
     tree type;
{
  tree binfo = TYPE_BINFO (type);
  cid = 0;

#ifdef SAVE_MI_MATRIX
  if (CLASSTYPE_MI_MATRIX (type))
    {
      mi_size = CLASSTYPE_N_SUPERCLASSES (type) + CLASSTYPE_N_VBASECLASSES (type);
      mi_matrix = CLASSTYPE_MI_MATRIX (type);
      mi_type = type;
      dfs_walk (binfo, dfs_number, unnumberedp);
      return;
    }
#endif

  dfs_walk (binfo, dfs_number, unnumberedp);

  mi_size = CLASSTYPE_N_SUPERCLASSES (type) + CLASSTYPE_N_VBASECLASSES (type);
  if (mi_size < (cid-1))
    mi_size = cid-1;
  mi_matrix = (char *)xmalloc ((mi_size + 1) * (mi_size + 1));
  mi_type = type;
  bzero (mi_matrix, (mi_size + 1) * (mi_size + 1));
  dfs_walk (binfo, dfs_record_inheritance, unmarkedp);
  dfs_walk (binfo, dfs_unmark, markedp);
}

void
free_mi_matrix ()
{
  dfs_walk (TYPE_BINFO (mi_type), dfs_unnumber, numberedp);

#ifdef SAVE_MI_MATRIX
  CLASSTYPE_MI_MATRIX (mi_type) = mi_matrix;
#else
  free (mi_matrix);
  mi_size = 0;
  cid = 0;
#endif
}

/* If we want debug info for a type TYPE, make sure all its base types
   are also marked as being potentially interesting.  This avoids
   the problem of not writing any debug info for intermediate basetypes
   that have abstract virtual functions.  Also mark member types.  */

void
note_debug_info_needed (type)
     tree type;
{
  tree field;

  if (current_template_parms)
    return;

  /* We can't do the TYPE_DECL_SUPPRESS_DEBUG thing with DWARF, which
     does not support name references between translation units.  Well, we
     could, but that would mean putting global labels in the debug output
     before each exported type and each of its functions and static data
     members.  */
  if (write_symbols == DWARF_DEBUG || write_symbols == DWARF2_DEBUG)
    return;

  dfs_walk (TYPE_BINFO (type), dfs_debug_mark, dfs_debug_unmarkedp);
  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      tree ttype;
      if (TREE_CODE (field) == FIELD_DECL
	  && IS_AGGR_TYPE (ttype = target_type (TREE_TYPE (field)))
	  && dfs_debug_unmarkedp (TYPE_BINFO (ttype)))
	note_debug_info_needed (ttype);
    }
}

/* Subroutines of push_class_decls ().  */

/* Add in a decl to the envelope.  */
static void
envelope_add_decl (type, decl, values)
     tree type, decl, *values;
{
  tree context, *tmp;
  tree name = DECL_NAME (decl);
  int dont_add = 0;

  /* virtual base names are always unique.  */
  if (VBASE_NAME_P (name))
    *values = NULL_TREE;

  /* Possible ambiguity.  If its defining type(s)
     is (are all) derived from us, no problem.  */
  else if (*values && TREE_CODE (*values) != TREE_LIST)
    {
      tree value = *values;
      /* Only complain if we shadow something we can access.  */
      if (warn_shadow && TREE_CODE (decl) == FUNCTION_DECL
	  && ((DECL_LANG_SPECIFIC (*values)
	       && DECL_CLASS_CONTEXT (value) == current_class_type)
	      || ! TREE_PRIVATE (value)))
	/* Should figure out access control more accurately.  */
	{
	  cp_warning_at ("member `%#D' is shadowed", value);
	  cp_warning_at ("by member function `%#D'", decl);
	  warning ("in this context");
	}

      context = (TREE_CODE (value) == FUNCTION_DECL
		 && DECL_VIRTUAL_P (value))
	? DECL_CLASS_CONTEXT (value)
	  : DECL_CONTEXT (value);

      if (context == type)
	{
	  if (TREE_CODE (value) == TYPE_DECL
	      && DECL_ARTIFICIAL (value))
	    *values = NULL_TREE;
	  else
	    dont_add = 1;
	}
      /* If we don't check CLASSTYPE_CID on CONTEXT right now, we'll end
	 up subtracting from the address of MI_MATRIX, putting us off
	 in la la land.  */
      else if (context
	       && CLASSTYPE_CID (context)
	       && TYPE_DERIVES_FROM (context, type))
	{
	  /* Don't add in *values to list */
	  *values = NULL_TREE;
	}
      else
	*values = build_tree_list (NULL_TREE, value);
    }
  else
    for (tmp = values; *tmp;)
      {
	tree value = TREE_VALUE (*tmp);
	my_friendly_assert (TREE_CODE (value) != TREE_LIST, 999);
	context = (TREE_CODE (value) == FUNCTION_DECL
		   && DECL_VIRTUAL_P (value))
	  ? DECL_CLASS_CONTEXT (value)
	    : DECL_CONTEXT (value);

	/* If we don't check CLASSTYPE_CID on CONTEXT right now, we'll end
	   up subtracting from the address of MI_MATRIX, putting us off
	   in la la land.  */
	if (context
	    && CLASSTYPE_CID (context)
	    && TYPE_DERIVES_FROM (context, type))
	  {
	    /* remove *tmp from list */
	    *tmp = TREE_CHAIN (*tmp);
	  }
	else
	  tmp = &TREE_CHAIN (*tmp);
      }

  if (! dont_add)
    {
      /* Put the new contents in our envelope.  */
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  *values = tree_cons (name, decl, *values);
	  TREE_NONLOCAL_FLAG (*values) = 1;
	  TREE_TYPE (*values) = unknown_type_node;
	}
      else
	{
	  if (*values)
	    {
	      *values = tree_cons (NULL_TREE, decl, *values);
	      /* Mark this as a potentially ambiguous member.  */
	      /* Leaving TREE_TYPE blank is intentional.
		 We cannot use `error_mark_node' (lookup_name)
		 or `unknown_type_node' (all member functions use this).  */
	      TREE_NONLOCAL_FLAG (*values) = 1;
	    }
	  else
	    *values = decl;
	}
    }
}

/* Add the instance variables which this class contributed to the
   current class binding contour.  When a redefinition occurs, if the
   redefinition is strictly within a single inheritance path, we just
   overwrite the old declaration with the new.  If the fields are not
   within a single inheritance path, we must cons them.

   In order to know what decls are new (stemming from the current
   invocation of push_class_decls) we enclose them in an "envelope",
   which is a TREE_LIST node where the TREE_PURPOSE slot contains the
   new decl (or possibly a list of competing ones), the TREE_VALUE slot
   points to the old value and the TREE_CHAIN slot chains together all
   envelopes which needs to be "opened" in push_class_decls.  Opening an
   envelope means: push the old value onto the class_shadowed list,
   install the new one and if it's a TYPE_DECL do the same to the
   IDENTIFIER_TYPE_VALUE.  Such an envelope is recognized by seeing that
   the TREE_PURPOSE slot is non-null, and that it is not an identifier.
   Because if it is, it could be a set of overloaded methods from an
   outer scope.  */

static void
dfs_pushdecls (binfo)
     tree binfo;
{
  tree type = BINFO_TYPE (binfo);
  tree fields, *methods, *end;
  tree method_vec;

  for (fields = TYPE_FIELDS (type); fields; fields = TREE_CHAIN (fields))
    {
      /* Unmark so that if we are in a constructor, and then find that
	 this field was initialized by a base initializer,
	 we can emit an error message.  */
      if (TREE_CODE (fields) == FIELD_DECL)
	TREE_USED (fields) = 0;

      /* Recurse into anonymous unions.  */
      if (DECL_NAME (fields) == NULL_TREE
	  && TREE_CODE (TREE_TYPE (fields)) == UNION_TYPE)
	{
	  dfs_pushdecls (TYPE_BINFO (TREE_TYPE (fields)));
	  continue;
	}

      if (DECL_NAME (fields))
	{
	  tree name = DECL_NAME (fields);
	  tree class_value = IDENTIFIER_CLASS_VALUE (name);

	  /* If the class value is not an envelope of the kind described in
	     the comment above, we create a new envelope.  */
	  if (class_value == NULL_TREE || TREE_CODE (class_value) != TREE_LIST
	      || TREE_PURPOSE (class_value) == NULL_TREE
	      || TREE_CODE (TREE_PURPOSE (class_value)) == IDENTIFIER_NODE)
	    {
	      /* See comment above for a description of envelopes.  */
	      closed_envelopes = tree_cons (NULL_TREE, class_value,
					    closed_envelopes);
	      IDENTIFIER_CLASS_VALUE (name) = closed_envelopes;
	      class_value = IDENTIFIER_CLASS_VALUE (name);
	    }

	  envelope_add_decl (type, fields, &TREE_PURPOSE (class_value));
	}
    }

  method_vec = CLASSTYPE_METHOD_VEC (type);
  if (method_vec)
    {
      /* Farm out constructors and destructors.  */
      methods = &TREE_VEC_ELT (method_vec, 2);
      end = TREE_VEC_END (method_vec);

      while (methods != end)
	{
	  /* This will cause lookup_name to return a pointer
	     to the tree_list of possible methods of this name.  */
	  tree name = DECL_NAME (*methods);
	  tree class_value = IDENTIFIER_CLASS_VALUE (name);

	  /* If the class value is not an envelope of the kind described in
	     the comment above, we create a new envelope.  */
	  if (class_value == NULL_TREE || TREE_CODE (class_value) != TREE_LIST
	      || TREE_PURPOSE (class_value) == NULL_TREE
	      || TREE_CODE (TREE_PURPOSE (class_value)) == IDENTIFIER_NODE)
	    {
	      /* See comment above for a description of envelopes.  */
	      closed_envelopes = tree_cons (NULL_TREE, class_value,
					    closed_envelopes);
	      IDENTIFIER_CLASS_VALUE (name) = closed_envelopes;
	      class_value = IDENTIFIER_CLASS_VALUE (name);
	    }

	  /* Here we try to rule out possible ambiguities.
	     If we can't do that, keep a TREE_LIST with possibly ambiguous
	     decls in there.  */
	  maybe_push_cache_obstack ();
	  envelope_add_decl (type, *methods, &TREE_PURPOSE (class_value));
	  pop_obstacks ();

	  methods++;
	}
    }
  SET_BINFO_MARKED (binfo);
}

/* Consolidate unique (by name) member functions.  */

static void
dfs_compress_decls (binfo)
     tree binfo;
{
  tree type = BINFO_TYPE (binfo);
  tree method_vec = CLASSTYPE_METHOD_VEC (type);

  if (method_vec != 0)
    {
      /* Farm out constructors and destructors.  */
      tree *methods = &TREE_VEC_ELT (method_vec, 2);
      tree *end = TREE_VEC_END (method_vec);

      for (; methods != end; methods++)
	{
	  /* This is known to be an envelope of the kind described before
	     dfs_pushdecls.  */
	  tree class_value = IDENTIFIER_CLASS_VALUE (DECL_NAME (*methods));
	  tree tmp = TREE_PURPOSE (class_value);

	  /* This was replaced in scope by somebody else.  Just leave it
	     alone.  */
	  if (TREE_CODE (tmp) != TREE_LIST)
	    continue;

	  if (TREE_CHAIN (tmp) == NULL_TREE
	      && TREE_VALUE (tmp)
	      && DECL_CHAIN (TREE_VALUE (tmp)) == NULL_TREE)
	    {
	      TREE_PURPOSE (class_value) = TREE_VALUE (tmp);
	    }
	}
    }
  CLEAR_BINFO_MARKED (binfo);
}

/* When entering the scope of a class, we cache all of the
   fields that that class provides within its inheritance
   lattice.  Where ambiguities result, we mark them
   with `error_mark_node' so that if they are encountered
   without explicit qualification, we can emit an error
   message.  */

void
push_class_decls (type)
     tree type;
{
  struct obstack *ambient_obstack = current_obstack;
  search_stack = push_search_level (search_stack, &search_obstack);

  /* Push class fields into CLASS_VALUE scope, and mark.  */
  dfs_walk (TYPE_BINFO (type), dfs_pushdecls, unmarkedp);

  /* Compress fields which have only a single entry
     by a given name, and unmark.  */
  dfs_walk (TYPE_BINFO (type), dfs_compress_decls, markedp);

  /* Open up all the closed envelopes and push the contained decls into
     class scope.  */
  while (closed_envelopes)
    {
      tree new = TREE_PURPOSE (closed_envelopes);
      tree id;

      /* This is messy because the class value may be a *_DECL, or a
	 TREE_LIST of overloaded *_DECLs or even a TREE_LIST of ambiguous
	 *_DECLs.  The name is stored at different places in these three
	 cases.  */
      if (TREE_CODE (new) == TREE_LIST)
	{
	  if (TREE_PURPOSE (new) != NULL_TREE)
	    id = TREE_PURPOSE (new);
	  else
	    {
	      tree node = TREE_VALUE (new);

	      if (TREE_CODE (node) == TYPE_DECL
		  && DECL_ARTIFICIAL (node)
		  && IS_AGGR_TYPE (TREE_TYPE (node))
		  && CLASSTYPE_TEMPLATE_INFO (TREE_TYPE (node)))
		{
		  tree t = CLASSTYPE_TI_TEMPLATE (TREE_TYPE (node));
		  tree n = new;

		  for (; n; n = TREE_CHAIN (n))
		    {
		      tree d = TREE_VALUE (n);
		      if (TREE_CODE (d) == TYPE_DECL
			  && DECL_ARTIFICIAL (node)
			  && IS_AGGR_TYPE (TREE_TYPE (d))
			  && CLASSTYPE_TEMPLATE_INFO (TREE_TYPE (d))
			  && CLASSTYPE_TI_TEMPLATE (TREE_TYPE (d)) == t)
			/* OK */;
		      else
			break;
		    }

		  if (n == NULL_TREE)
		    new = t;
		}
	      else while (TREE_CODE (node) == TREE_LIST)
		node = TREE_VALUE (node);
	      id = DECL_NAME (node);
	    }
	}
      else
	id = DECL_NAME (new);

      /* Install the original class value in order to make
	 pushdecl_class_level work correctly.  */
      IDENTIFIER_CLASS_VALUE (id) = TREE_VALUE (closed_envelopes);
      if (TREE_CODE (new) == TREE_LIST)
	push_class_level_binding (id, new);
      else
	pushdecl_class_level (new);
      closed_envelopes = TREE_CHAIN (closed_envelopes);
    }
  current_obstack = ambient_obstack;
}

/* Here's a subroutine we need because C lacks lambdas.  */

static void
dfs_unuse_fields (binfo)
     tree binfo;
{
  tree type = TREE_TYPE (binfo);
  tree fields;

  for (fields = TYPE_FIELDS (type); fields; fields = TREE_CHAIN (fields))
    {
      if (TREE_CODE (fields) != FIELD_DECL)
	continue;

      TREE_USED (fields) = 0;
      if (DECL_NAME (fields) == NULL_TREE
	  && TREE_CODE (TREE_TYPE (fields)) == UNION_TYPE)
	unuse_fields (TREE_TYPE (fields));
    }
}

void
unuse_fields (type)
     tree type;
{
  dfs_walk (TYPE_BINFO (type), dfs_unuse_fields, unmarkedp);
}

void
pop_class_decls ()
{
  /* We haven't pushed a search level when dealing with cached classes,
     so we'd better not try to pop it.  */
  if (search_stack)
    search_stack = pop_search_level (search_stack);
}

void
print_search_statistics ()
{
#ifdef GATHER_STATISTICS
  if (flag_memoize_lookups)
    {
      fprintf (stderr, "%d memoized contexts saved\n",
	       n_contexts_saved);
      fprintf (stderr, "%d local tree nodes made\n", my_tree_node_counter);
      fprintf (stderr, "%d local hash nodes made\n", my_memoized_entry_counter);
      fprintf (stderr, "fields statistics:\n");
      fprintf (stderr, "  memoized finds = %d; rejects = %d; (searches = %d)\n",
	       memoized_fast_finds[0], memoized_fast_rejects[0],
	       memoized_fields_searched[0]);
      fprintf (stderr, "  memoized_adds = %d\n", memoized_adds[0]);
      fprintf (stderr, "fnfields statistics:\n");
      fprintf (stderr, "  memoized finds = %d; rejects = %d; (searches = %d)\n",
	       memoized_fast_finds[1], memoized_fast_rejects[1],
	       memoized_fields_searched[1]);
      fprintf (stderr, "  memoized_adds = %d\n", memoized_adds[1]);
    }
  fprintf (stderr, "%d fields searched in %d[%d] calls to lookup_field[_1]\n",
	   n_fields_searched, n_calls_lookup_field, n_calls_lookup_field_1);
  fprintf (stderr, "%d fnfields searched in %d calls to lookup_fnfields\n",
	   n_outer_fields_searched, n_calls_lookup_fnfields);
  fprintf (stderr, "%d calls to get_base_type\n", n_calls_get_base_type);
#else /* GATHER_STATISTICS */
  fprintf (stderr, "no search statistics\n");
#endif /* GATHER_STATISTICS */
}

void
init_search_processing ()
{
  gcc_obstack_init (&search_obstack);
  gcc_obstack_init (&type_obstack);
  gcc_obstack_init (&type_obstack_entries);

  /* This gives us room to build our chains of basetypes,
     whether or not we decide to memoize them.  */
  type_stack = push_type_level ((struct stack_level *)0, &type_obstack);
  _vptr_name = get_identifier ("_vptr");
}

void
reinit_search_statistics ()
{
  my_memoized_entry_counter = 0;
  memoized_fast_finds[0] = 0;
  memoized_fast_finds[1] = 0;
  memoized_adds[0] = 0;
  memoized_adds[1] = 0;
  memoized_fast_rejects[0] = 0;
  memoized_fast_rejects[1] = 0;
  memoized_fields_searched[0] = 0;
  memoized_fields_searched[1] = 0;
#ifdef GATHER_STATISTICS
  n_fields_searched = 0;
  n_calls_lookup_field = 0, n_calls_lookup_field_1 = 0;
  n_calls_lookup_fnfields = 0, n_calls_lookup_fnfields_1 = 0;
  n_calls_get_base_type = 0;
  n_outer_fields_searched = 0;
  n_contexts_saved = 0;
#endif /* GATHER_STATISTICS */
}

#define scratch_tree_cons expr_tree_cons

static tree conversions;
static void
add_conversions (binfo)
     tree binfo;
{
  int i;
  tree method_vec = CLASSTYPE_METHOD_VEC (BINFO_TYPE (binfo));

  for (i = 2; i < TREE_VEC_LENGTH (method_vec); ++i)
    {
      tree tmp = TREE_VEC_ELT (method_vec, i);
      if (! IDENTIFIER_TYPENAME_P (DECL_NAME (tmp)))
	break;
      conversions = scratch_tree_cons (binfo, tmp, conversions);
    }
  SET_BINFO_MARKED (binfo);
}

tree
lookup_conversions (type)
     tree type;
{
  conversions = NULL_TREE;
  if (TYPE_SIZE (type))
    {
      dfs_walk (TYPE_BINFO (type), add_conversions, unmarkedp);
      dfs_walk (TYPE_BINFO (type), dfs_unmark, markedp);
    }
  return conversions;
}

/* Subroutine of get_template_base.  */

static tree
get_template_base_recursive (binfo, rval, template, via_virtual)
     tree binfo, template, rval;
     int via_virtual;
{
  tree binfos;
  int i, n_baselinks;
  tree type = BINFO_TYPE (binfo);

  if (CLASSTYPE_TEMPLATE_INFO (type)
      && CLASSTYPE_TI_TEMPLATE (type) == template)
    {
      if (rval == NULL_TREE || rval == type)
	return type;
      else
	return error_mark_node;
    }

  binfos = BINFO_BASETYPES (binfo);
  n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  /* Process base types.  */
  for (i = 0; i < n_baselinks; i++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);

      /* Find any specific instance of a virtual base, when searching with
	 a binfo...  */
      if (BINFO_MARKED (base_binfo) == 0)
	{
	  int this_virtual = via_virtual || TREE_VIA_VIRTUAL (base_binfo);

	  /* When searching for a non-virtual, we cannot mark
	     virtually found binfos.  */
	  if (! this_virtual)
	    SET_BINFO_MARKED (base_binfo);

	  rval = get_template_base_recursive
	    (base_binfo, rval, template, this_virtual);
	  if (rval == error_mark_node)
	    return rval;
	}
    }

  return rval;
}

/* Given a class template TEMPLATE and a class type or binfo node BINFO,
   find the unique base type in BINFO that is an instance of TEMPLATE.
   If there are more than one, return error_mark_node.  Used by unify.  */

tree
get_template_base (template, binfo)
     register tree template, binfo;
{
  tree type, rval;

  if (TREE_CODE (binfo) == TREE_VEC)
    type = BINFO_TYPE (binfo);
  else if (IS_AGGR_TYPE_CODE (TREE_CODE (binfo)))
    {
      type = complete_type (binfo);
      binfo = TYPE_BINFO (type);
    }
  else
    my_friendly_abort (92);

  if (CLASSTYPE_TEMPLATE_INFO (type)
      && CLASSTYPE_TI_TEMPLATE (type) == template)
    return type;

  rval = get_template_base_recursive (binfo, NULL_TREE, template, 0);
  dfs_walk (binfo, dfs_unmark, markedp);

  return rval;
}
