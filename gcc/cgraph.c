/* Callgraph handling code.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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

/*  This file contains basic routines manipulating call graph

The callgraph:

    The call-graph is data structure designed for intra-procedural optimization
    but it is also used in non-unit-at-a-time compilation to allow easier code
    sharing.

    The call-graph consist of nodes and edges represented via linked lists.
    Each function (external or not) corresponds to the unique node.

    The mapping from declarations to call-graph nodes is done using hash table
    based on DECL_UID.  The call-graph nodes are created lazily using
    cgraph_node function when called for unknown declaration.

    The callgraph at the moment does not represent all indirect calls or calls
    from other compilation units.  Flag NEEDED is set for each node that may be
    accessed in such an invisible way and it shall be considered an entry point
    to the callgraph.

    On the other hand, the callgraph currently does contain some edges for
    indirect calls with unknown callees which can be accessed through
    indirect_calls field of a node.  It should be noted however that at the
    moment only calls which are potential candidates for indirect inlining are
    added there.

    Interprocedural information:

      Callgraph is place to store data needed for interprocedural optimization.
      All data structures are divided into three components: local_info that
      is produced while analyzing the function, global_info that is result
      of global walking of the callgraph on the end of compilation and
      rtl_info used by RTL backend to propagate data from already compiled
      functions to their callers.

      Moreover, each node has a uid which can be used to keep information in
      on-the-side arrays.  UIDs are reused and therefore reasonably dense.

    Inlining plans:

      The function inlining information is decided in advance and maintained
      in the callgraph as so called inline plan.
      For each inlined call, the callee's node is cloned to represent the
      new function copy produced by inliner.
      Each inlined call gets a unique corresponding clone node of the callee
      and the data structure is updated while inlining is performed, so
      the clones are eliminated and their callee edges redirected to the
      caller.

      Each edge has "inline_failed" field.  When the field is set to NULL,
      the call will be inlined.  When it is non-NULL it contains a reason
      why inlining wasn't performed.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "hashtab.h"
#include "toplev.h"
#include "flags.h"
#include "ggc.h"
#include "debug.h"
#include "target.h"
#include "basic-block.h"
#include "cgraph.h"
#include "output.h"
#include "intl.h"
#include "gimple.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "value-prof.h"
#include "except.h"
#include "diagnostic-core.h"
#include "rtl.h"
#include "ipa-utils.h"
#include "lto-streamer.h"
#include "ipa-inline.h"

const char * const ld_plugin_symbol_resolution_names[]=
{
  "",
  "undef",
  "prevailing_def",
  "prevailing_def_ironly",
  "preempted_reg",
  "preempted_ir",
  "resolved_ir",
  "resolved_exec",
  "resolved_dyn",
  "prevailing_def_ironly_exp"
};

static void cgraph_node_remove_callers (struct cgraph_node *node);
static inline void cgraph_edge_remove_caller (struct cgraph_edge *e);
static inline void cgraph_edge_remove_callee (struct cgraph_edge *e);

/* Hash table used to convert declarations into nodes.  */
static GTY((param_is (struct cgraph_node))) htab_t cgraph_hash;
/* Hash table used to convert assembler names into nodes.  */
static GTY((param_is (struct cgraph_node))) htab_t assembler_name_hash;

/* The linked list of cgraph nodes.  */
struct cgraph_node *cgraph_nodes;

/* Queue of cgraph nodes scheduled to be lowered.  */
struct cgraph_node *cgraph_nodes_queue;

/* Queue of cgraph nodes scheduled to be added into cgraph.  This is a
   secondary queue used during optimization to accommodate passes that
   may generate new functions that need to be optimized and expanded.  */
struct cgraph_node *cgraph_new_nodes;

/* Number of nodes in existence.  */
int cgraph_n_nodes;

/* Maximal uid used in cgraph nodes.  */
int cgraph_max_uid;

/* Maximal uid used in cgraph edges.  */
int cgraph_edge_max_uid;

/* Set when whole unit has been analyzed so we can access global info.  */
bool cgraph_global_info_ready = false;

/* What state callgraph is in right now.  */
enum cgraph_state cgraph_state = CGRAPH_STATE_CONSTRUCTION;

/* Set when the cgraph is fully build and the basic flags are computed.  */
bool cgraph_function_flags_ready = false;

/* Linked list of cgraph asm nodes.  */
struct cgraph_asm_node *cgraph_asm_nodes;

/* Last node in cgraph_asm_nodes.  */
static GTY(()) struct cgraph_asm_node *cgraph_asm_last_node;

/* The order index of the next cgraph node to be created.  This is
   used so that we can sort the cgraph nodes in order by when we saw
   them, to support -fno-toplevel-reorder.  */
int cgraph_order;

/* List of hooks triggered on cgraph_edge events.  */
struct cgraph_edge_hook_list {
  cgraph_edge_hook hook;
  void *data;
  struct cgraph_edge_hook_list *next;
};

/* List of hooks triggered on cgraph_node events.  */
struct cgraph_node_hook_list {
  cgraph_node_hook hook;
  void *data;
  struct cgraph_node_hook_list *next;
};

/* List of hooks triggered on events involving two cgraph_edges.  */
struct cgraph_2edge_hook_list {
  cgraph_2edge_hook hook;
  void *data;
  struct cgraph_2edge_hook_list *next;
};

/* List of hooks triggered on events involving two cgraph_nodes.  */
struct cgraph_2node_hook_list {
  cgraph_2node_hook hook;
  void *data;
  struct cgraph_2node_hook_list *next;
};

/* List of hooks triggered when an edge is removed.  */
struct cgraph_edge_hook_list *first_cgraph_edge_removal_hook;
/* List of hooks triggered when a node is removed.  */
struct cgraph_node_hook_list *first_cgraph_node_removal_hook;
/* List of hooks triggered when an edge is duplicated.  */
struct cgraph_2edge_hook_list *first_cgraph_edge_duplicated_hook;
/* List of hooks triggered when a node is duplicated.  */
struct cgraph_2node_hook_list *first_cgraph_node_duplicated_hook;
/* List of hooks triggered when an function is inserted.  */
struct cgraph_node_hook_list *first_cgraph_function_insertion_hook;

/* Head of a linked list of unused (freed) call graph nodes.
   Do not GTY((delete)) this list so UIDs gets reliably recycled.  */
static GTY(()) struct cgraph_node *free_nodes;
/* Head of a linked list of unused (freed) call graph edges.
   Do not GTY((delete)) this list so UIDs gets reliably recycled.  */
static GTY(()) struct cgraph_edge *free_edges;

/* Did procss_same_body_aliases run?  */
bool same_body_aliases_done;

/* Macros to access the next item in the list of free cgraph nodes and
   edges. */
#define NEXT_FREE_NODE(NODE) (NODE)->next
#define NEXT_FREE_EDGE(EDGE) (EDGE)->prev_caller

/* Register HOOK to be called with DATA on each removed edge.  */
struct cgraph_edge_hook_list *
cgraph_add_edge_removal_hook (cgraph_edge_hook hook, void *data)
{
  struct cgraph_edge_hook_list *entry;
  struct cgraph_edge_hook_list **ptr = &first_cgraph_edge_removal_hook;

  entry = (struct cgraph_edge_hook_list *) xmalloc (sizeof (*entry));
  entry->hook = hook;
  entry->data = data;
  entry->next = NULL;
  while (*ptr)
    ptr = &(*ptr)->next;
  *ptr = entry;
  return entry;
}

/* Remove ENTRY from the list of hooks called on removing edges.  */
void
cgraph_remove_edge_removal_hook (struct cgraph_edge_hook_list *entry)
{
  struct cgraph_edge_hook_list **ptr = &first_cgraph_edge_removal_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all edge removal hooks.  */
static void
cgraph_call_edge_removal_hooks (struct cgraph_edge *e)
{
  struct cgraph_edge_hook_list *entry = first_cgraph_edge_removal_hook;
  while (entry)
  {
    entry->hook (e, entry->data);
    entry = entry->next;
  }
}

/* Register HOOK to be called with DATA on each removed node.  */
struct cgraph_node_hook_list *
cgraph_add_node_removal_hook (cgraph_node_hook hook, void *data)
{
  struct cgraph_node_hook_list *entry;
  struct cgraph_node_hook_list **ptr = &first_cgraph_node_removal_hook;

  entry = (struct cgraph_node_hook_list *) xmalloc (sizeof (*entry));
  entry->hook = hook;
  entry->data = data;
  entry->next = NULL;
  while (*ptr)
    ptr = &(*ptr)->next;
  *ptr = entry;
  return entry;
}

/* Remove ENTRY from the list of hooks called on removing nodes.  */
void
cgraph_remove_node_removal_hook (struct cgraph_node_hook_list *entry)
{
  struct cgraph_node_hook_list **ptr = &first_cgraph_node_removal_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all node removal hooks.  */
static void
cgraph_call_node_removal_hooks (struct cgraph_node *node)
{
  struct cgraph_node_hook_list *entry = first_cgraph_node_removal_hook;
  while (entry)
  {
    entry->hook (node, entry->data);
    entry = entry->next;
  }
}

/* Register HOOK to be called with DATA on each inserted node.  */
struct cgraph_node_hook_list *
cgraph_add_function_insertion_hook (cgraph_node_hook hook, void *data)
{
  struct cgraph_node_hook_list *entry;
  struct cgraph_node_hook_list **ptr = &first_cgraph_function_insertion_hook;

  entry = (struct cgraph_node_hook_list *) xmalloc (sizeof (*entry));
  entry->hook = hook;
  entry->data = data;
  entry->next = NULL;
  while (*ptr)
    ptr = &(*ptr)->next;
  *ptr = entry;
  return entry;
}

/* Remove ENTRY from the list of hooks called on inserted nodes.  */
void
cgraph_remove_function_insertion_hook (struct cgraph_node_hook_list *entry)
{
  struct cgraph_node_hook_list **ptr = &first_cgraph_function_insertion_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all node insertion hooks.  */
void
cgraph_call_function_insertion_hooks (struct cgraph_node *node)
{
  struct cgraph_node_hook_list *entry = first_cgraph_function_insertion_hook;
  while (entry)
  {
    entry->hook (node, entry->data);
    entry = entry->next;
  }
}

/* Register HOOK to be called with DATA on each duplicated edge.  */
struct cgraph_2edge_hook_list *
cgraph_add_edge_duplication_hook (cgraph_2edge_hook hook, void *data)
{
  struct cgraph_2edge_hook_list *entry;
  struct cgraph_2edge_hook_list **ptr = &first_cgraph_edge_duplicated_hook;

  entry = (struct cgraph_2edge_hook_list *) xmalloc (sizeof (*entry));
  entry->hook = hook;
  entry->data = data;
  entry->next = NULL;
  while (*ptr)
    ptr = &(*ptr)->next;
  *ptr = entry;
  return entry;
}

/* Remove ENTRY from the list of hooks called on duplicating edges.  */
void
cgraph_remove_edge_duplication_hook (struct cgraph_2edge_hook_list *entry)
{
  struct cgraph_2edge_hook_list **ptr = &first_cgraph_edge_duplicated_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all edge duplication hooks.  */
static void
cgraph_call_edge_duplication_hooks (struct cgraph_edge *cs1,
				    struct cgraph_edge *cs2)
{
  struct cgraph_2edge_hook_list *entry = first_cgraph_edge_duplicated_hook;
  while (entry)
  {
    entry->hook (cs1, cs2, entry->data);
    entry = entry->next;
  }
}

/* Register HOOK to be called with DATA on each duplicated node.  */
struct cgraph_2node_hook_list *
cgraph_add_node_duplication_hook (cgraph_2node_hook hook, void *data)
{
  struct cgraph_2node_hook_list *entry;
  struct cgraph_2node_hook_list **ptr = &first_cgraph_node_duplicated_hook;

  entry = (struct cgraph_2node_hook_list *) xmalloc (sizeof (*entry));
  entry->hook = hook;
  entry->data = data;
  entry->next = NULL;
  while (*ptr)
    ptr = &(*ptr)->next;
  *ptr = entry;
  return entry;
}

/* Remove ENTRY from the list of hooks called on duplicating nodes.  */
void
cgraph_remove_node_duplication_hook (struct cgraph_2node_hook_list *entry)
{
  struct cgraph_2node_hook_list **ptr = &first_cgraph_node_duplicated_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all node duplication hooks.  */
void
cgraph_call_node_duplication_hooks (struct cgraph_node *node1,
				    struct cgraph_node *node2)
{
  struct cgraph_2node_hook_list *entry = first_cgraph_node_duplicated_hook;
  while (entry)
  {
    entry->hook (node1, node2, entry->data);
    entry = entry->next;
  }
}

/* Returns a hash code for P.  */

static hashval_t
hash_node (const void *p)
{
  const struct cgraph_node *n = (const struct cgraph_node *) p;
  return (hashval_t) DECL_UID (n->decl);
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_node (const void *p1, const void *p2)
{
  const struct cgraph_node *n1 = (const struct cgraph_node *) p1;
  const struct cgraph_node *n2 = (const struct cgraph_node *) p2;
  return DECL_UID (n1->decl) == DECL_UID (n2->decl);
}

/* Allocate new callgraph node.  */

static inline struct cgraph_node *
cgraph_allocate_node (void)
{
  struct cgraph_node *node;

  if (free_nodes)
    {
      node = free_nodes;
      free_nodes = NEXT_FREE_NODE (node);
    }
  else
    {
      node = ggc_alloc_cleared_cgraph_node ();
      node->uid = cgraph_max_uid++;
    }

  return node;
}

/* Allocate new callgraph node and insert it into basic data structures.  */

static struct cgraph_node *
cgraph_create_node_1 (void)
{
  struct cgraph_node *node = cgraph_allocate_node ();

  node->next = cgraph_nodes;
  node->order = cgraph_order++;
  if (cgraph_nodes)
    cgraph_nodes->previous = node;
  node->previous = NULL;
  node->frequency = NODE_FREQUENCY_NORMAL;
  node->count_materialization_scale = REG_BR_PROB_BASE;
  ipa_empty_ref_list (&node->ref_list);
  cgraph_nodes = node;
  cgraph_n_nodes++;
  return node;
}

/* Return cgraph node assigned to DECL.  Create new one when needed.  */

struct cgraph_node *
cgraph_create_node (tree decl)
{
  struct cgraph_node key, *node, **slot;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);

  if (!cgraph_hash)
    cgraph_hash = htab_create_ggc (10, hash_node, eq_node, NULL);

  key.decl = decl;
  slot = (struct cgraph_node **) htab_find_slot (cgraph_hash, &key, INSERT);
  gcc_assert (!*slot);

  node = cgraph_create_node_1 ();
  node->decl = decl;
  *slot = node;
  if (DECL_CONTEXT (decl) && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL)
    {
      node->origin = cgraph_get_create_node (DECL_CONTEXT (decl));
      node->next_nested = node->origin->nested;
      node->origin->nested = node;
    }
  if (assembler_name_hash)
    {
      void **aslot;
      tree name = DECL_ASSEMBLER_NAME (decl);

      aslot = htab_find_slot_with_hash (assembler_name_hash, name,
					decl_assembler_name_hash (name),
					INSERT);
      /* We can have multiple declarations with same assembler name. For C++
	 it is __builtin_strlen and strlen, for instance.  Do we need to
	 record them all?  Original implementation marked just first one
	 so lets hope for the best.  */
      if (*aslot == NULL)
	*aslot = node;
    }
  return node;
}

/* Try to find a call graph node for declaration DECL and if it does not exist,
   create it.  */

struct cgraph_node *
cgraph_get_create_node (tree decl)
{
  struct cgraph_node *node;

  node = cgraph_get_node (decl);
  if (node)
    return node;

  return cgraph_create_node (decl);
}

/* Mark ALIAS as an alias to DECL.  DECL_NODE is cgraph node representing
   the function body is associated with (not neccesarily cgraph_node (DECL).  */

struct cgraph_node *
cgraph_create_function_alias (tree alias, tree decl)
{
  struct cgraph_node *alias_node;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);
  gcc_assert (TREE_CODE (alias) == FUNCTION_DECL);
  alias_node = cgraph_get_create_node (alias);
  gcc_assert (!alias_node->local.finalized);
  alias_node->thunk.alias = decl;
  alias_node->local.finalized = true;
  alias_node->alias = 1;

  if ((TREE_PUBLIC (alias) && !DECL_COMDAT (alias) && !DECL_EXTERNAL (alias))
      || (DECL_VIRTUAL_P (alias)
	  && (DECL_COMDAT (alias) || DECL_EXTERNAL (alias))))
    cgraph_mark_reachable_node (alias_node);
  return alias_node;
}

/* Attempt to mark ALIAS as an alias to DECL.  Return alias node if successful
   and NULL otherwise.
   Same body aliases are output whenever the body of DECL is output,
   and cgraph_get_node (ALIAS) transparently returns cgraph_get_node (DECL).  */

struct cgraph_node *
cgraph_same_body_alias (struct cgraph_node *decl_node ATTRIBUTE_UNUSED, tree alias, tree decl)
{
  struct cgraph_node *n;
#ifndef ASM_OUTPUT_DEF
  /* If aliases aren't supported by the assembler, fail.  */
  return NULL;
#endif
  /* Langhooks can create same body aliases of symbols not defined.
     Those are useless. Drop them on the floor.  */
  if (cgraph_global_info_ready)
    return NULL;

  n = cgraph_create_function_alias (alias, decl);
  n->same_body_alias = true;
  if (same_body_aliases_done)
    ipa_record_reference (n, NULL, cgraph_get_node (decl), NULL, IPA_REF_ALIAS,
			  NULL);
  return n;
}

/* Add thunk alias into callgraph.  The alias declaration is ALIAS and it
   aliases DECL with an adjustments made into the first parameter.
   See comments in thunk_adjust for detail on the parameters.  */

struct cgraph_node *
cgraph_add_thunk (struct cgraph_node *decl_node ATTRIBUTE_UNUSED,
		  tree alias, tree decl,
		  bool this_adjusting,
		  HOST_WIDE_INT fixed_offset, HOST_WIDE_INT virtual_value,
		  tree virtual_offset,
		  tree real_alias)
{
  struct cgraph_node *node;

  node = cgraph_get_node (alias);
  if (node)
    {
      gcc_assert (node->local.finalized);
      gcc_assert (!node->alias);
      gcc_assert (!node->thunk.thunk_p);
      cgraph_remove_node (node);
    }
  
  node = cgraph_create_node (alias);
  gcc_checking_assert (!virtual_offset
		       || double_int_equal_p
		            (tree_to_double_int (virtual_offset),
			     shwi_to_double_int (virtual_value)));
  node->thunk.fixed_offset = fixed_offset;
  node->thunk.this_adjusting = this_adjusting;
  node->thunk.virtual_value = virtual_value;
  node->thunk.virtual_offset_p = virtual_offset != NULL;
  node->thunk.alias = real_alias;
  node->thunk.thunk_p = true;
  node->local.finalized = true;

  if (cgraph_decide_is_function_needed (node, decl))
    cgraph_mark_needed_node (node);

  if ((TREE_PUBLIC (decl) && !DECL_COMDAT (decl) && !DECL_EXTERNAL (decl))
      || (DECL_VIRTUAL_P (decl)
	  && (DECL_COMDAT (decl) || DECL_EXTERNAL (decl))))
    cgraph_mark_reachable_node (node);

  return node;
}

/* Returns the cgraph node assigned to DECL or NULL if no cgraph node
   is assigned.  */

struct cgraph_node *
cgraph_get_node (const_tree decl)
{
  struct cgraph_node key, *node = NULL, **slot;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);

  if (!cgraph_hash)
    return NULL;

  key.decl = CONST_CAST2 (tree, const_tree, decl);

  slot = (struct cgraph_node **) htab_find_slot (cgraph_hash, &key,
						 NO_INSERT);

  if (slot && *slot)
    node = *slot;
  return node;
}

/* Insert already constructed node into hashtable.  */

void
cgraph_insert_node_to_hashtable (struct cgraph_node *node)
{
  struct cgraph_node **slot;

  slot = (struct cgraph_node **) htab_find_slot (cgraph_hash, node, INSERT);

  gcc_assert (!*slot);
  *slot = node;
}

/* Returns a hash code for P.  */

static hashval_t
hash_node_by_assembler_name (const void *p)
{
  const struct cgraph_node *n = (const struct cgraph_node *) p;
  return (hashval_t) decl_assembler_name_hash (DECL_ASSEMBLER_NAME (n->decl));
}

/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_assembler_name (const void *p1, const void *p2)
{
  const struct cgraph_node *n1 = (const struct cgraph_node *) p1;
  const_tree name = (const_tree)p2;
  return (decl_assembler_name_equal (n1->decl, name));
}

/* Return the cgraph node that has ASMNAME for its DECL_ASSEMBLER_NAME.
   Return NULL if there's no such node.  */

struct cgraph_node *
cgraph_node_for_asm (tree asmname)
{
  struct cgraph_node *node;
  void **slot;

  if (!assembler_name_hash)
    {
      assembler_name_hash =
	htab_create_ggc (10, hash_node_by_assembler_name, eq_assembler_name,
			 NULL);
      for (node = cgraph_nodes; node; node = node->next)
        if (!node->global.inlined_to)
	  {
	    tree name = DECL_ASSEMBLER_NAME (node->decl);
	    slot = htab_find_slot_with_hash (assembler_name_hash, name,
					     decl_assembler_name_hash (name),
					     INSERT);
	    /* We can have multiple declarations with same assembler name. For C++
	       it is __builtin_strlen and strlen, for instance.  Do we need to
	       record them all?  Original implementation marked just first one
	       so lets hope for the best.  */
	    if (!*slot)
	      *slot = node;
	  }
    }

  slot = htab_find_slot_with_hash (assembler_name_hash, asmname,
				   decl_assembler_name_hash (asmname),
				   NO_INSERT);

  if (slot)
    {
      node = (struct cgraph_node *) *slot;
      return node;
    }
  return NULL;
}

/* Returns a hash value for X (which really is a die_struct).  */

static hashval_t
edge_hash (const void *x)
{
  return htab_hash_pointer (((const struct cgraph_edge *) x)->call_stmt);
}

/* Return nonzero if decl_id of die_struct X is the same as UID of decl *Y.  */

static int
edge_eq (const void *x, const void *y)
{
  return ((const struct cgraph_edge *) x)->call_stmt == y;
}

/* Add call graph edge E to call site hash of its caller.  */

static inline void
cgraph_add_edge_to_call_site_hash (struct cgraph_edge *e)
{
  void **slot;
  slot = htab_find_slot_with_hash (e->caller->call_site_hash,
				   e->call_stmt,
				   htab_hash_pointer (e->call_stmt),
				   INSERT);
  gcc_assert (!*slot);
  *slot = e;
}

/* Return the callgraph edge representing the GIMPLE_CALL statement
   CALL_STMT.  */

struct cgraph_edge *
cgraph_edge (struct cgraph_node *node, gimple call_stmt)
{
  struct cgraph_edge *e, *e2;
  int n = 0;

  if (node->call_site_hash)
    return (struct cgraph_edge *)
      htab_find_with_hash (node->call_site_hash, call_stmt,
      	                   htab_hash_pointer (call_stmt));

  /* This loop may turn out to be performance problem.  In such case adding
     hashtables into call nodes with very many edges is probably best
     solution.  It is not good idea to add pointer into CALL_EXPR itself
     because we want to make possible having multiple cgraph nodes representing
     different clones of the same body before the body is actually cloned.  */
  for (e = node->callees; e; e = e->next_callee)
    {
      if (e->call_stmt == call_stmt)
	break;
      n++;
    }

  if (!e)
    for (e = node->indirect_calls; e; e = e->next_callee)
      {
	if (e->call_stmt == call_stmt)
	  break;
	n++;
      }

  if (n > 100)
    {
      node->call_site_hash = htab_create_ggc (120, edge_hash, edge_eq, NULL);
      for (e2 = node->callees; e2; e2 = e2->next_callee)
	cgraph_add_edge_to_call_site_hash (e2);
      for (e2 = node->indirect_calls; e2; e2 = e2->next_callee)
	cgraph_add_edge_to_call_site_hash (e2);
    }

  return e;
}


/* Change field call_stmt of edge E to NEW_STMT.  */

void
cgraph_set_call_stmt (struct cgraph_edge *e, gimple new_stmt)
{
  tree decl;

  if (e->caller->call_site_hash)
    {
      htab_remove_elt_with_hash (e->caller->call_site_hash,
				 e->call_stmt,
				 htab_hash_pointer (e->call_stmt));
    }

  e->call_stmt = new_stmt;
  if (e->indirect_unknown_callee
      && (decl = gimple_call_fndecl (new_stmt)))
    {
      /* Constant propagation (and possibly also inlining?) can turn an
	 indirect call into a direct one.  */
      struct cgraph_node *new_callee = cgraph_get_node (decl);

      gcc_checking_assert (new_callee);
      cgraph_make_edge_direct (e, new_callee);
    }

  push_cfun (DECL_STRUCT_FUNCTION (e->caller->decl));
  e->can_throw_external = stmt_can_throw_external (new_stmt);
  pop_cfun ();
  if (e->caller->call_site_hash)
    cgraph_add_edge_to_call_site_hash (e);
}

/* Like cgraph_set_call_stmt but walk the clone tree and update all
   clones sharing the same function body.  */

void
cgraph_set_call_stmt_including_clones (struct cgraph_node *orig,
				       gimple old_stmt, gimple new_stmt)
{
  struct cgraph_node *node;
  struct cgraph_edge *edge = cgraph_edge (orig, old_stmt);

  if (edge)
    cgraph_set_call_stmt (edge, new_stmt);

  node = orig->clones;
  if (node)
    while (node != orig)
      {
	struct cgraph_edge *edge = cgraph_edge (node, old_stmt);
	if (edge)
	  cgraph_set_call_stmt (edge, new_stmt);
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
}

/* Like cgraph_create_edge walk the clone tree and update all clones sharing
   same function body.  If clones already have edge for OLD_STMT; only
   update the edge same way as cgraph_set_call_stmt_including_clones does.

   TODO: COUNT and LOOP_DEPTH should be properly distributed based on relative
   frequencies of the clones.  */

void
cgraph_create_edge_including_clones (struct cgraph_node *orig,
				     struct cgraph_node *callee,
				     gimple old_stmt,
				     gimple stmt, gcov_type count,
				     int freq,
				     cgraph_inline_failed_t reason)
{
  struct cgraph_node *node;
  struct cgraph_edge *edge;

  if (!cgraph_edge (orig, stmt))
    {
      edge = cgraph_create_edge (orig, callee, stmt, count, freq);
      edge->inline_failed = reason;
    }

  node = orig->clones;
  if (node)
    while (node != orig)
      {
	struct cgraph_edge *edge = cgraph_edge (node, old_stmt);

        /* It is possible that clones already contain the edge while
	   master didn't.  Either we promoted indirect call into direct
	   call in the clone or we are processing clones of unreachable
	   master where edges has been removed.  */
	if (edge)
	  cgraph_set_call_stmt (edge, stmt);
	else if (!cgraph_edge (node, stmt))
	  {
	    edge = cgraph_create_edge (node, callee, stmt, count,
				       freq);
	    edge->inline_failed = reason;
	  }

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
}

/* Allocate a cgraph_edge structure and fill it with data according to the
   parameters of which only CALLEE can be NULL (when creating an indirect call
   edge).  */

static struct cgraph_edge *
cgraph_create_edge_1 (struct cgraph_node *caller, struct cgraph_node *callee,
		       gimple call_stmt, gcov_type count, int freq)
{
  struct cgraph_edge *edge;

  /* LTO does not actually have access to the call_stmt since these
     have not been loaded yet.  */
  if (call_stmt)
    {
      /* This is a rather expensive check possibly triggering
	 construction of call stmt hashtable.  */
      gcc_checking_assert (!cgraph_edge (caller, call_stmt));

      gcc_assert (is_gimple_call (call_stmt));
    }

  if (free_edges)
    {
      edge = free_edges;
      free_edges = NEXT_FREE_EDGE (edge);
    }
  else
    {
      edge = ggc_alloc_cgraph_edge ();
      edge->uid = cgraph_edge_max_uid++;
    }

  edge->aux = NULL;
  edge->caller = caller;
  edge->callee = callee;
  edge->prev_caller = NULL;
  edge->next_caller = NULL;
  edge->prev_callee = NULL;
  edge->next_callee = NULL;

  edge->count = count;
  gcc_assert (count >= 0);
  edge->frequency = freq;
  gcc_assert (freq >= 0);
  gcc_assert (freq <= CGRAPH_FREQ_MAX);

  edge->call_stmt = call_stmt;
  push_cfun (DECL_STRUCT_FUNCTION (caller->decl));
  edge->can_throw_external
    = call_stmt ? stmt_can_throw_external (call_stmt) : false;
  pop_cfun ();
  edge->call_stmt_cannot_inline_p =
    (call_stmt ? gimple_call_cannot_inline_p (call_stmt) : false);
  if (call_stmt && caller->call_site_hash)
    cgraph_add_edge_to_call_site_hash (edge);

  edge->indirect_info = NULL;
  edge->indirect_inlining_edge = 0;

  return edge;
}

/* Create edge from CALLER to CALLEE in the cgraph.  */

struct cgraph_edge *
cgraph_create_edge (struct cgraph_node *caller, struct cgraph_node *callee,
		    gimple call_stmt, gcov_type count, int freq)
{
  struct cgraph_edge *edge = cgraph_create_edge_1 (caller, callee, call_stmt,
						   count, freq);

  edge->indirect_unknown_callee = 0;
  initialize_inline_failed (edge);

  edge->next_caller = callee->callers;
  if (callee->callers)
    callee->callers->prev_caller = edge;
  edge->next_callee = caller->callees;
  if (caller->callees)
    caller->callees->prev_callee = edge;
  caller->callees = edge;
  callee->callers = edge;

  return edge;
}

/* Allocate cgraph_indirect_call_info and set its fields to default values. */

struct cgraph_indirect_call_info *
cgraph_allocate_init_indirect_info (void)
{
  struct cgraph_indirect_call_info *ii;

  ii = ggc_alloc_cleared_cgraph_indirect_call_info ();
  ii->param_index = -1;
  return ii;
}

/* Create an indirect edge with a yet-undetermined callee where the call
   statement destination is a formal parameter of the caller with index
   PARAM_INDEX. */

struct cgraph_edge *
cgraph_create_indirect_edge (struct cgraph_node *caller, gimple call_stmt,
			     int ecf_flags,
			     gcov_type count, int freq)
{
  struct cgraph_edge *edge = cgraph_create_edge_1 (caller, NULL, call_stmt,
						   count, freq);

  edge->indirect_unknown_callee = 1;
  initialize_inline_failed (edge);

  edge->indirect_info = cgraph_allocate_init_indirect_info ();
  edge->indirect_info->ecf_flags = ecf_flags;

  edge->next_callee = caller->indirect_calls;
  if (caller->indirect_calls)
    caller->indirect_calls->prev_callee = edge;
  caller->indirect_calls = edge;

  return edge;
}

/* Remove the edge E from the list of the callers of the callee.  */

static inline void
cgraph_edge_remove_callee (struct cgraph_edge *e)
{
  gcc_assert (!e->indirect_unknown_callee);
  if (e->prev_caller)
    e->prev_caller->next_caller = e->next_caller;
  if (e->next_caller)
    e->next_caller->prev_caller = e->prev_caller;
  if (!e->prev_caller)
    e->callee->callers = e->next_caller;
}

/* Remove the edge E from the list of the callees of the caller.  */

static inline void
cgraph_edge_remove_caller (struct cgraph_edge *e)
{
  if (e->prev_callee)
    e->prev_callee->next_callee = e->next_callee;
  if (e->next_callee)
    e->next_callee->prev_callee = e->prev_callee;
  if (!e->prev_callee)
    {
      if (e->indirect_unknown_callee)
	e->caller->indirect_calls = e->next_callee;
      else
	e->caller->callees = e->next_callee;
    }
  if (e->caller->call_site_hash)
    htab_remove_elt_with_hash (e->caller->call_site_hash,
			       e->call_stmt,
	  		       htab_hash_pointer (e->call_stmt));
}

/* Put the edge onto the free list.  */

static void
cgraph_free_edge (struct cgraph_edge *e)
{
  int uid = e->uid;

  /* Clear out the edge so we do not dangle pointers.  */
  memset (e, 0, sizeof (*e));
  e->uid = uid;
  NEXT_FREE_EDGE (e) = free_edges;
  free_edges = e;
}

/* Remove the edge E in the cgraph.  */

void
cgraph_remove_edge (struct cgraph_edge *e)
{
  /* Call all edge removal hooks.  */
  cgraph_call_edge_removal_hooks (e);

  if (!e->indirect_unknown_callee)
    /* Remove from callers list of the callee.  */
    cgraph_edge_remove_callee (e);

  /* Remove from callees list of the callers.  */
  cgraph_edge_remove_caller (e);

  /* Put the edge onto the free list.  */
  cgraph_free_edge (e);
}

/* Set callee of call graph edge E and add it to the corresponding set of
   callers. */

static void
cgraph_set_edge_callee (struct cgraph_edge *e, struct cgraph_node *n)
{
  e->prev_caller = NULL;
  if (n->callers)
    n->callers->prev_caller = e;
  e->next_caller = n->callers;
  n->callers = e;
  e->callee = n;
}

/* Redirect callee of E to N.  The function does not update underlying
   call expression.  */

void
cgraph_redirect_edge_callee (struct cgraph_edge *e, struct cgraph_node *n)
{
  /* Remove from callers list of the current callee.  */
  cgraph_edge_remove_callee (e);

  /* Insert to callers list of the new callee.  */
  cgraph_set_edge_callee (e, n);
}

/* Make an indirect EDGE with an unknown callee an ordinary edge leading to
   CALLEE.  DELTA is an integer constant that is to be added to the this
   pointer (first parameter) to compensate for skipping a thunk adjustment.  */

void
cgraph_make_edge_direct (struct cgraph_edge *edge, struct cgraph_node *callee)
{
  edge->indirect_unknown_callee = 0;

  /* Get the edge out of the indirect edge list. */
  if (edge->prev_callee)
    edge->prev_callee->next_callee = edge->next_callee;
  if (edge->next_callee)
    edge->next_callee->prev_callee = edge->prev_callee;
  if (!edge->prev_callee)
    edge->caller->indirect_calls = edge->next_callee;

  /* Put it into the normal callee list */
  edge->prev_callee = NULL;
  edge->next_callee = edge->caller->callees;
  if (edge->caller->callees)
    edge->caller->callees->prev_callee = edge;
  edge->caller->callees = edge;

  /* Insert to callers list of the new callee.  */
  cgraph_set_edge_callee (edge, callee);

  if (edge->call_stmt
      && !gimple_check_call_matching_types (edge->call_stmt, callee->decl))
    {
      gimple_call_set_cannot_inline (edge->call_stmt, true);
      edge->call_stmt_cannot_inline_p = true;
    }

  /* We need to re-determine the inlining status of the edge.  */
  initialize_inline_failed (edge);
}


/* Update or remove the corresponding cgraph edge if a GIMPLE_CALL
   OLD_STMT changed into NEW_STMT.  OLD_CALL is gimple_call_fndecl
   of OLD_STMT if it was previously call statement.
   If NEW_STMT is NULL, the call has been dropped without any
   replacement.  */

static void
cgraph_update_edges_for_call_stmt_node (struct cgraph_node *node,
					gimple old_stmt, tree old_call,
					gimple new_stmt)
{
  tree new_call = (new_stmt && is_gimple_call (new_stmt))
		  ? gimple_call_fndecl (new_stmt) : 0;

  /* We are seeing indirect calls, then there is nothing to update.  */
  if (!new_call && !old_call)
    return;
  /* See if we turned indirect call into direct call or folded call to one builtin
     into different builtin.  */
  if (old_call != new_call)
    {
      struct cgraph_edge *e = cgraph_edge (node, old_stmt);
      struct cgraph_edge *ne = NULL;
      gcov_type count;
      int frequency;

      if (e)
	{
	  /* See if the edge is already there and has the correct callee.  It
	     might be so because of indirect inlining has already updated
	     it.  We also might've cloned and redirected the edge.  */
	  if (new_call && e->callee)
	    {
	      struct cgraph_node *callee = e->callee;
	      while (callee)
		{
		  if (callee->decl == new_call
		      || callee->former_clone_of == new_call)
		    return;
		  callee = callee->clone_of;
		}
	    }

	  /* Otherwise remove edge and create new one; we can't simply redirect
	     since function has changed, so inline plan and other information
	     attached to edge is invalid.  */
	  count = e->count;
	  frequency = e->frequency;
	  cgraph_remove_edge (e);
	}
      else if (new_call)
	{
	  /* We are seeing new direct call; compute profile info based on BB.  */
	  basic_block bb = gimple_bb (new_stmt);
	  count = bb->count;
	  frequency = compute_call_stmt_bb_frequency (current_function_decl,
						      bb);
	}

      if (new_call)
	{
	  ne = cgraph_create_edge (node, cgraph_get_create_node (new_call),
				   new_stmt, count, frequency);
	  gcc_assert (ne->inline_failed);
	}
    }
  /* We only updated the call stmt; update pointer in cgraph edge..  */
  else if (old_stmt != new_stmt)
    cgraph_set_call_stmt (cgraph_edge (node, old_stmt), new_stmt);
}

/* Update or remove the corresponding cgraph edge if a GIMPLE_CALL
   OLD_STMT changed into NEW_STMT.  OLD_DECL is gimple_call_fndecl
   of OLD_STMT before it was updated (updating can happen inplace).  */

void
cgraph_update_edges_for_call_stmt (gimple old_stmt, tree old_decl, gimple new_stmt)
{
  struct cgraph_node *orig = cgraph_get_node (cfun->decl);
  struct cgraph_node *node;

  gcc_checking_assert (orig);
  cgraph_update_edges_for_call_stmt_node (orig, old_stmt, old_decl, new_stmt);
  if (orig->clones)
    for (node = orig->clones; node != orig;)
      {
        cgraph_update_edges_for_call_stmt_node (node, old_stmt, old_decl, new_stmt);
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
}


/* Remove all callees from the node.  */

void
cgraph_node_remove_callees (struct cgraph_node *node)
{
  struct cgraph_edge *e, *f;

  /* It is sufficient to remove the edges from the lists of callers of
     the callees.  The callee list of the node can be zapped with one
     assignment.  */
  for (e = node->callees; e; e = f)
    {
      f = e->next_callee;
      cgraph_call_edge_removal_hooks (e);
      if (!e->indirect_unknown_callee)
	cgraph_edge_remove_callee (e);
      cgraph_free_edge (e);
    }
  for (e = node->indirect_calls; e; e = f)
    {
      f = e->next_callee;
      cgraph_call_edge_removal_hooks (e);
      if (!e->indirect_unknown_callee)
	cgraph_edge_remove_callee (e);
      cgraph_free_edge (e);
    }
  node->indirect_calls = NULL;
  node->callees = NULL;
  if (node->call_site_hash)
    {
      htab_delete (node->call_site_hash);
      node->call_site_hash = NULL;
    }
}

/* Remove all callers from the node.  */

static void
cgraph_node_remove_callers (struct cgraph_node *node)
{
  struct cgraph_edge *e, *f;

  /* It is sufficient to remove the edges from the lists of callees of
     the callers.  The caller list of the node can be zapped with one
     assignment.  */
  for (e = node->callers; e; e = f)
    {
      f = e->next_caller;
      cgraph_call_edge_removal_hooks (e);
      cgraph_edge_remove_caller (e);
      cgraph_free_edge (e);
    }
  node->callers = NULL;
}

/* Release memory used to represent body of function NODE.  */

void
cgraph_release_function_body (struct cgraph_node *node)
{
  if (DECL_STRUCT_FUNCTION (node->decl))
    {
      tree old_decl = current_function_decl;
      push_cfun (DECL_STRUCT_FUNCTION (node->decl));
      if (cfun->gimple_df)
	{
	  current_function_decl = node->decl;
	  delete_tree_ssa ();
	  delete_tree_cfg_annotations ();
	  cfun->eh = NULL;
	  current_function_decl = old_decl;
	}
      if (cfun->cfg)
	{
	  gcc_assert (dom_computed[0] == DOM_NONE);
	  gcc_assert (dom_computed[1] == DOM_NONE);
	  clear_edges ();
	}
      if (cfun->value_histograms)
	free_histograms ();
      gcc_assert (!current_loops);
      pop_cfun();
      gimple_set_body (node->decl, NULL);
      VEC_free (ipa_opt_pass, heap,
      		node->ipa_transforms_to_apply);
      /* Struct function hangs a lot of data that would leak if we didn't
         removed all pointers to it.   */
      ggc_free (DECL_STRUCT_FUNCTION (node->decl));
      DECL_STRUCT_FUNCTION (node->decl) = NULL;
    }
  DECL_SAVED_TREE (node->decl) = NULL;
  /* If the node is abstract and needed, then do not clear DECL_INITIAL
     of its associated function function declaration because it's
     needed to emit debug info later.  */
  if (!node->abstract_and_needed)
    DECL_INITIAL (node->decl) = error_mark_node;
}

/* Remove the node from cgraph.  */

void
cgraph_remove_node (struct cgraph_node *node)
{
  void **slot;
  bool kill_body = false;
  struct cgraph_node *n;
  int uid = node->uid;

  cgraph_call_node_removal_hooks (node);
  cgraph_node_remove_callers (node);
  cgraph_node_remove_callees (node);
  ipa_remove_all_references (&node->ref_list);
  ipa_remove_all_refering (&node->ref_list);
  VEC_free (ipa_opt_pass, heap,
            node->ipa_transforms_to_apply);

  /* Incremental inlining access removed nodes stored in the postorder list.
     */
  node->needed = node->reachable = false;
  for (n = node->nested; n; n = n->next_nested)
    n->origin = NULL;
  node->nested = NULL;
  if (node->origin)
    {
      struct cgraph_node **node2 = &node->origin->nested;

      while (*node2 != node)
	node2 = &(*node2)->next_nested;
      *node2 = node->next_nested;
    }
  if (node->previous)
    node->previous->next = node->next;
  else
    cgraph_nodes = node->next;
  if (node->next)
    node->next->previous = node->previous;
  node->next = NULL;
  node->previous = NULL;
  slot = htab_find_slot (cgraph_hash, node, NO_INSERT);
  if (*slot == node)
    {
      struct cgraph_node *next_inline_clone;

      for (next_inline_clone = node->clones;
      	   next_inline_clone && next_inline_clone->decl != node->decl;
	   next_inline_clone = next_inline_clone->next_sibling_clone)
	;

      /* If there is inline clone of the node being removed, we need
         to put it into the position of removed node and reorganize all
	 other clones to be based on it.  */
      if (next_inline_clone)
	{
	  struct cgraph_node *n;
	  struct cgraph_node *new_clones;

	  *slot = next_inline_clone;

	  /* Unlink inline clone from the list of clones of removed node.  */
	  if (next_inline_clone->next_sibling_clone)
	    next_inline_clone->next_sibling_clone->prev_sibling_clone
	      = next_inline_clone->prev_sibling_clone;
	  if (next_inline_clone->prev_sibling_clone)
	    {
	      gcc_assert (node->clones != next_inline_clone);
	      next_inline_clone->prev_sibling_clone->next_sibling_clone
	        = next_inline_clone->next_sibling_clone;
	    }
	  else
	    {
	      gcc_assert (node->clones == next_inline_clone);
	      node->clones = next_inline_clone->next_sibling_clone;
	    }

	  new_clones = node->clones;
	  node->clones = NULL;

	  /* Copy clone info.  */
	  next_inline_clone->clone = node->clone;

	  /* Now place it into clone tree at same level at NODE.  */
	  next_inline_clone->clone_of = node->clone_of;
	  next_inline_clone->prev_sibling_clone = NULL;
	  next_inline_clone->next_sibling_clone = NULL;
	  if (node->clone_of)
	    {
	      if (node->clone_of->clones)
	        node->clone_of->clones->prev_sibling_clone = next_inline_clone;
	      next_inline_clone->next_sibling_clone = node->clone_of->clones;
	      node->clone_of->clones = next_inline_clone;
	    }

	  /* Merge the clone list.  */
	  if (new_clones)
	    {
	      if (!next_inline_clone->clones)
		next_inline_clone->clones = new_clones;
	      else
		{
		  n = next_inline_clone->clones;
		  while (n->next_sibling_clone)
		    n =  n->next_sibling_clone;
		  n->next_sibling_clone = new_clones;
		  new_clones->prev_sibling_clone = n;
		}
	    }

	  /* Update clone_of pointers.  */
	  n = new_clones;
	  while (n)
	    {
	      n->clone_of = next_inline_clone;
	      n = n->next_sibling_clone;
	    }
	}
      else
	{
	  htab_clear_slot (cgraph_hash, slot);
	  kill_body = true;
	}

    }
  if (node->prev_sibling_clone)
    node->prev_sibling_clone->next_sibling_clone = node->next_sibling_clone;
  else if (node->clone_of)
    node->clone_of->clones = node->next_sibling_clone;
  if (node->next_sibling_clone)
    node->next_sibling_clone->prev_sibling_clone = node->prev_sibling_clone;
  if (node->clones)
    {
      struct cgraph_node *n, *next;

      if (node->clone_of)
        {
	  for (n = node->clones; n->next_sibling_clone; n = n->next_sibling_clone)
	    n->clone_of = node->clone_of;
	  n->clone_of = node->clone_of;
	  n->next_sibling_clone = node->clone_of->clones;
	  if (node->clone_of->clones)
	    node->clone_of->clones->prev_sibling_clone = n;
	  node->clone_of->clones = node->clones;
	}
      else
        {
	  /* We are removing node with clones.  this makes clones inconsistent,
	     but assume they will be removed subsequently and just keep clone
	     tree intact.  This can happen in unreachable function removal since
	     we remove unreachable functions in random order, not by bottom-up
	     walk of clone trees.  */
	  for (n = node->clones; n; n = next)
	    {
	       next = n->next_sibling_clone;
	       n->next_sibling_clone = NULL;
	       n->prev_sibling_clone = NULL;
	       n->clone_of = NULL;
	    }
	}
    }

  if (node->same_comdat_group)
    {
      struct cgraph_node *prev;
      for (prev = node->same_comdat_group;
	   prev->same_comdat_group != node;
	   prev = prev->same_comdat_group)
	;
      if (node->same_comdat_group == prev)
	prev->same_comdat_group = NULL;
      else
	prev->same_comdat_group = node->same_comdat_group;
      node->same_comdat_group = NULL;
    }

  /* While all the clones are removed after being proceeded, the function
     itself is kept in the cgraph even after it is compiled.  Check whether
     we are done with this body and reclaim it proactively if this is the case.
     */
  if (!kill_body && *slot)
    {
      struct cgraph_node *n = (struct cgraph_node *) *slot;
      if (!n->clones && !n->clone_of && !n->global.inlined_to
	  && (cgraph_global_info_ready
	      && (TREE_ASM_WRITTEN (n->decl) || DECL_EXTERNAL (n->decl)
		  || n->in_other_partition)))
	kill_body = true;
    }
  if (assembler_name_hash)
    {
      tree name = DECL_ASSEMBLER_NAME (node->decl);
      slot = htab_find_slot_with_hash (assembler_name_hash, name,
				       decl_assembler_name_hash (name),
				       NO_INSERT);
      /* Inline clones are not hashed.  */
      if (slot && *slot == node)
        htab_clear_slot (assembler_name_hash, slot);
    }

  if (kill_body)
    cgraph_release_function_body (node);
  node->decl = NULL;
  if (node->call_site_hash)
    {
      htab_delete (node->call_site_hash);
      node->call_site_hash = NULL;
    }
  cgraph_n_nodes--;

  /* Clear out the node to NULL all pointers and add the node to the free
     list.  */
  memset (node, 0, sizeof(*node));
  node->uid = uid;
  NEXT_FREE_NODE (node) = free_nodes;
  free_nodes = node;
}

/* Add NEW_ to the same comdat group that OLD is in.  */

void
cgraph_add_to_same_comdat_group (struct cgraph_node *new_,
				 struct cgraph_node *old)
{
  gcc_assert (DECL_ONE_ONLY (old->decl));
  gcc_assert (!new_->same_comdat_group);
  gcc_assert (new_ != old);

  DECL_COMDAT_GROUP (new_->decl) = DECL_COMDAT_GROUP (old->decl);
  new_->same_comdat_group = old;
  if (!old->same_comdat_group)
    old->same_comdat_group = new_;
  else
    {
      struct cgraph_node *n;
      for (n = old->same_comdat_group;
	   n->same_comdat_group != old;
	   n = n->same_comdat_group)
	;
      n->same_comdat_group = new_;
    }
}

/* Remove the node from cgraph.  */

void
cgraph_remove_node_and_inline_clones (struct cgraph_node *node)
{
  struct cgraph_edge *e, *next;
  for (e = node->callees; e; e = next)
    {
      next = e->next_callee;
      if (!e->inline_failed)
        cgraph_remove_node_and_inline_clones (e->callee);
    }
  cgraph_remove_node (node);
}

/* Notify finalize_compilation_unit that given node is reachable.  */

void
cgraph_mark_reachable_node (struct cgraph_node *node)
{
  if (!node->reachable && node->local.finalized)
    {
      if (cgraph_global_info_ready)
        {
	  /* Verify that function does not appear to be needed out of blue
	     during the optimization process.  This can happen for extern
	     inlines when bodies was removed after inlining.  */
	  gcc_assert ((node->analyzed || node->in_other_partition
		       || DECL_EXTERNAL (node->decl)));
	}
      else
        notice_global_symbol (node->decl);
      node->reachable = 1;

      node->next_needed = cgraph_nodes_queue;
      cgraph_nodes_queue = node;
    }
}

/* Likewise indicate that a node is needed, i.e. reachable via some
   external means.  */

void
cgraph_mark_needed_node (struct cgraph_node *node)
{
  node->needed = 1;
  gcc_assert (!node->global.inlined_to);
  cgraph_mark_reachable_node (node);
}

/* Likewise indicate that a node is having address taken.  */

void
cgraph_mark_address_taken_node (struct cgraph_node *node)
{
  gcc_assert (!node->global.inlined_to);
  cgraph_mark_reachable_node (node);
  /* FIXME: address_taken flag is used both as a shortcut for testing whether
     IPA_REF_ADDR reference exists (and thus it should be set on node
     representing alias we take address of) and as a test whether address
     of the object was taken (and thus it should be set on node alias is
     referring to).  We should remove the first use and the remove the
     following set.  */
  node->address_taken = 1;
  node = cgraph_function_or_thunk_node (node, NULL);
  node->address_taken = 1;
}

/* Return local info for the compiled function.  */

struct cgraph_local_info *
cgraph_local_info (tree decl)
{
  struct cgraph_node *node;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);
  node = cgraph_get_node (decl);
  if (!node)
    return NULL;
  return &node->local;
}

/* Return local info for the compiled function.  */

struct cgraph_global_info *
cgraph_global_info (tree decl)
{
  struct cgraph_node *node;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL && cgraph_global_info_ready);
  node = cgraph_get_node (decl);
  if (!node)
    return NULL;
  return &node->global;
}

/* Return local info for the compiled function.  */

struct cgraph_rtl_info *
cgraph_rtl_info (tree decl)
{
  struct cgraph_node *node;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);
  node = cgraph_get_node (decl);
  if (!node
      || (decl != current_function_decl
	  && !TREE_ASM_WRITTEN (node->decl)))
    return NULL;
  return &node->rtl;
}

/* Return a string describing the failure REASON.  */

const char*
cgraph_inline_failed_string (cgraph_inline_failed_t reason)
{
#undef DEFCIFCODE
#define DEFCIFCODE(code, string)	string,

  static const char *cif_string_table[CIF_N_REASONS] = {
#include "cif-code.def"
  };

  /* Signedness of an enum type is implementation defined, so cast it
     to unsigned before testing. */
  gcc_assert ((unsigned) reason < CIF_N_REASONS);
  return cif_string_table[reason];
}

/* Return name of the node used in debug output.  */
const char *
cgraph_node_name (struct cgraph_node *node)
{
  return lang_hooks.decl_printable_name (node->decl, 2);
}

/* Names used to print out the availability enum.  */
const char * const cgraph_availability_names[] =
  {"unset", "not_available", "overwritable", "available", "local"};


/* Dump call graph node NODE to file F.  */

void
dump_cgraph_node (FILE *f, struct cgraph_node *node)
{
  struct cgraph_edge *edge;
  int indirect_calls_count = 0;

  fprintf (f, "%s/%i", cgraph_node_name (node), node->uid);
  dump_addr (f, " @", (void *)node);
  if (DECL_ASSEMBLER_NAME_SET_P (node->decl))
    fprintf (f, " (asm: %s)", IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->decl)));
  if (node->global.inlined_to)
    fprintf (f, " (inline copy in %s/%i)",
	     cgraph_node_name (node->global.inlined_to),
	     node->global.inlined_to->uid);
  if (node->same_comdat_group)
    fprintf (f, " (same comdat group as %s/%i)",
	     cgraph_node_name (node->same_comdat_group),
	     node->same_comdat_group->uid);
  if (node->clone_of)
    fprintf (f, " (clone of %s/%i)",
	     cgraph_node_name (node->clone_of),
	     node->clone_of->uid);
  if (cgraph_function_flags_ready)
    fprintf (f, " availability:%s",
	     cgraph_availability_names [cgraph_function_body_availability (node)]);
  if (node->analyzed)
    fprintf (f, " analyzed");
  if (node->in_other_partition)
    fprintf (f, " in_other_partition");
  if (node->count)
    fprintf (f, " executed "HOST_WIDEST_INT_PRINT_DEC"x",
	     (HOST_WIDEST_INT)node->count);
  if (node->origin)
    fprintf (f, " nested in: %s", cgraph_node_name (node->origin));
  if (node->needed)
    fprintf (f, " needed");
  if (node->address_taken)
    fprintf (f, " address_taken");
  else if (node->reachable)
    fprintf (f, " reachable");
  else if (node->reachable_from_other_partition)
    fprintf (f, " reachable_from_other_partition");
  if (gimple_has_body_p (node->decl))
    fprintf (f, " body");
  if (node->process)
    fprintf (f, " process");
  if (node->local.local)
    fprintf (f, " local");
  if (node->local.externally_visible)
    fprintf (f, " externally_visible");
  if (node->resolution != LDPR_UNKNOWN)
    fprintf (f, " %s",
 	     ld_plugin_symbol_resolution_names[(int)node->resolution]);
  if (node->local.finalized)
    fprintf (f, " finalized");
  if (node->local.redefined_extern_inline)
    fprintf (f, " redefined_extern_inline");
  if (TREE_ASM_WRITTEN (node->decl))
    fprintf (f, " asm_written");
  if (node->only_called_at_startup)
    fprintf (f, " only_called_at_startup");
  if (node->only_called_at_exit)
    fprintf (f, " only_called_at_exit");
  else if (node->alias)
    fprintf (f, " alias");
  if (node->tm_clone)
    fprintf (f, " tm_clone");

  fprintf (f, "\n");

  if (node->thunk.thunk_p)
    {
      fprintf (f, "  thunk of %s (asm: %s) fixed offset %i virtual value %i has "
	       "virtual offset %i)\n",
	       lang_hooks.decl_printable_name (node->thunk.alias, 2),
	       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->thunk.alias)),
	       (int)node->thunk.fixed_offset,
	       (int)node->thunk.virtual_value,
	       (int)node->thunk.virtual_offset_p);
    }
  if (node->alias && node->thunk.alias)
    {
      fprintf (f, "  alias of %s",
	       lang_hooks.decl_printable_name (node->thunk.alias, 2));
      if (DECL_ASSEMBLER_NAME_SET_P (node->thunk.alias))
        fprintf (f, " (asm: %s)",
		 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->thunk.alias)));
      fprintf (f, "\n");
    }
  
  fprintf (f, "  called by: ");

  for (edge = node->callers; edge; edge = edge->next_caller)
    {
      fprintf (f, "%s/%i ", cgraph_node_name (edge->caller),
	       edge->caller->uid);
      if (edge->count)
	fprintf (f, "("HOST_WIDEST_INT_PRINT_DEC"x) ",
		 (HOST_WIDEST_INT)edge->count);
      if (edge->frequency)
	fprintf (f, "(%.2f per call) ",
		 edge->frequency / (double)CGRAPH_FREQ_BASE);
      if (!edge->inline_failed)
	fprintf(f, "(inlined) ");
      if (edge->indirect_inlining_edge)
	fprintf(f, "(indirect_inlining) ");
      if (edge->can_throw_external)
	fprintf(f, "(can throw external) ");
    }

  fprintf (f, "\n  calls: ");
  for (edge = node->callees; edge; edge = edge->next_callee)
    {
      fprintf (f, "%s/%i ", cgraph_node_name (edge->callee),
	       edge->callee->uid);
      if (!edge->inline_failed)
	fprintf(f, "(inlined) ");
      if (edge->indirect_inlining_edge)
	fprintf(f, "(indirect_inlining) ");
      if (edge->count)
	fprintf (f, "("HOST_WIDEST_INT_PRINT_DEC"x) ",
		 (HOST_WIDEST_INT)edge->count);
      if (edge->frequency)
	fprintf (f, "(%.2f per call) ",
		 edge->frequency / (double)CGRAPH_FREQ_BASE);
      if (edge->can_throw_external)
	fprintf(f, "(can throw external) ");
    }
  fprintf (f, "\n");
  fprintf (f, "  References: ");
  ipa_dump_references (f, &node->ref_list);
  fprintf (f, "  Refering this function: ");
  ipa_dump_refering (f, &node->ref_list);

  for (edge = node->indirect_calls; edge; edge = edge->next_callee)
    indirect_calls_count++;
  if (indirect_calls_count)
    fprintf (f, "  has %i outgoing edges for indirect calls.\n",
	     indirect_calls_count);
}


/* Dump call graph node NODE to stderr.  */

DEBUG_FUNCTION void
debug_cgraph_node (struct cgraph_node *node)
{
  dump_cgraph_node (stderr, node);
}


/* Dump the callgraph to file F.  */

void
dump_cgraph (FILE *f)
{
  struct cgraph_node *node;

  fprintf (f, "callgraph:\n\n");
  for (node = cgraph_nodes; node; node = node->next)
    dump_cgraph_node (f, node);
}


/* Dump the call graph to stderr.  */

DEBUG_FUNCTION void
debug_cgraph (void)
{
  dump_cgraph (stderr);
}


/* Set the DECL_ASSEMBLER_NAME and update cgraph hashtables.  */

void
change_decl_assembler_name (tree decl, tree name)
{
  struct cgraph_node *node;
  void **slot;
  if (!DECL_ASSEMBLER_NAME_SET_P (decl))
    SET_DECL_ASSEMBLER_NAME (decl, name);
  else
    {
      if (name == DECL_ASSEMBLER_NAME (decl))
	return;

      if (assembler_name_hash
	  && TREE_CODE (decl) == FUNCTION_DECL
	  && (node = cgraph_get_node (decl)) != NULL)
	{
	  tree old_name = DECL_ASSEMBLER_NAME (decl);
	  slot = htab_find_slot_with_hash (assembler_name_hash, old_name,
					   decl_assembler_name_hash (old_name),
					   NO_INSERT);
	  /* Inline clones are not hashed.  */
	  if (slot && *slot == node)
	    htab_clear_slot (assembler_name_hash, slot);
	}
      if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))
	  && DECL_RTL_SET_P (decl))
	warning (0, "%D renamed after being referenced in assembly", decl);

      SET_DECL_ASSEMBLER_NAME (decl, name);
    }
  if (assembler_name_hash
      && TREE_CODE (decl) == FUNCTION_DECL
      && (node = cgraph_get_node (decl)) != NULL)
    {
      slot = htab_find_slot_with_hash (assembler_name_hash, name,
				       decl_assembler_name_hash (name),
				       INSERT);
      gcc_assert (!*slot);
      *slot = node;
    }
}

/* Add a top-level asm statement to the list.  */

struct cgraph_asm_node *
cgraph_add_asm_node (tree asm_str)
{
  struct cgraph_asm_node *node;

  node = ggc_alloc_cleared_cgraph_asm_node ();
  node->asm_str = asm_str;
  node->order = cgraph_order++;
  node->next = NULL;
  if (cgraph_asm_nodes == NULL)
    cgraph_asm_nodes = node;
  else
    cgraph_asm_last_node->next = node;
  cgraph_asm_last_node = node;
  return node;
}

/* Return true when the DECL can possibly be inlined.  */
bool
cgraph_function_possibly_inlined_p (tree decl)
{
  if (!cgraph_global_info_ready)
    return !DECL_UNINLINABLE (decl);
  return DECL_POSSIBLY_INLINED (decl);
}

/* Create clone of E in the node N represented by CALL_EXPR the callgraph.  */
struct cgraph_edge *
cgraph_clone_edge (struct cgraph_edge *e, struct cgraph_node *n,
		   gimple call_stmt, unsigned stmt_uid, gcov_type count_scale,
		   int freq_scale, bool update_original)
{
  struct cgraph_edge *new_edge;
  gcov_type count = e->count * count_scale / REG_BR_PROB_BASE;
  gcov_type freq;

  /* We do not want to ignore loop nest after frequency drops to 0.  */
  if (!freq_scale)
    freq_scale = 1;
  freq = e->frequency * (gcov_type) freq_scale / CGRAPH_FREQ_BASE;
  if (freq > CGRAPH_FREQ_MAX)
    freq = CGRAPH_FREQ_MAX;

  if (e->indirect_unknown_callee)
    {
      tree decl;

      if (call_stmt && (decl = gimple_call_fndecl (call_stmt)))
	{
	  struct cgraph_node *callee = cgraph_get_node (decl);
	  gcc_checking_assert (callee);
	  new_edge = cgraph_create_edge (n, callee, call_stmt, count, freq);
	}
      else
	{
	  new_edge = cgraph_create_indirect_edge (n, call_stmt,
						  e->indirect_info->ecf_flags,
						  count, freq);
	  *new_edge->indirect_info = *e->indirect_info;
	}
    }
  else
    {
      new_edge = cgraph_create_edge (n, e->callee, call_stmt, count, freq);
      if (e->indirect_info)
	{
	  new_edge->indirect_info
	    = ggc_alloc_cleared_cgraph_indirect_call_info ();
	  *new_edge->indirect_info = *e->indirect_info;
	}
    }

  new_edge->inline_failed = e->inline_failed;
  new_edge->indirect_inlining_edge = e->indirect_inlining_edge;
  new_edge->lto_stmt_uid = stmt_uid;
  /* Clone flags that depend on call_stmt availability manually.  */
  new_edge->can_throw_external = e->can_throw_external;
  new_edge->call_stmt_cannot_inline_p = e->call_stmt_cannot_inline_p;
  if (update_original)
    {
      e->count -= new_edge->count;
      if (e->count < 0)
	e->count = 0;
    }
  cgraph_call_edge_duplication_hooks (e, new_edge);
  return new_edge;
}


/* Create node representing clone of N executed COUNT times.  Decrease
   the execution counts from original node too.
   The new clone will have decl set to DECL that may or may not be the same
   as decl of N.

   When UPDATE_ORIGINAL is true, the counts are subtracted from the original
   function's profile to reflect the fact that part of execution is handled
   by node.  
   When CALL_DUPLICATOIN_HOOK is true, the ipa passes are acknowledged about
   the new clone. Otherwise the caller is responsible for doing so later.  */

struct cgraph_node *
cgraph_clone_node (struct cgraph_node *n, tree decl, gcov_type count, int freq,
		   bool update_original,
		   VEC(cgraph_edge_p,heap) *redirect_callers,
		   bool call_duplication_hook)
{
  struct cgraph_node *new_node = cgraph_create_node_1 ();
  struct cgraph_edge *e;
  gcov_type count_scale;
  unsigned i;

  new_node->decl = decl;
  new_node->origin = n->origin;
  if (new_node->origin)
    {
      new_node->next_nested = new_node->origin->nested;
      new_node->origin->nested = new_node;
    }
  new_node->analyzed = n->analyzed;
  new_node->local = n->local;
  new_node->local.externally_visible = false;
  new_node->local.local = true;
  new_node->global = n->global;
  new_node->rtl = n->rtl;
  new_node->count = count;
  new_node->frequency = n->frequency;
  new_node->clone = n->clone;
  new_node->clone.tree_map = 0;
  if (n->count)
    {
      if (new_node->count > n->count)
        count_scale = REG_BR_PROB_BASE;
      else
        count_scale = new_node->count * REG_BR_PROB_BASE / n->count;
    }
  else
    count_scale = 0;
  if (update_original)
    {
      n->count -= count;
      if (n->count < 0)
	n->count = 0;
    }

  FOR_EACH_VEC_ELT (cgraph_edge_p, redirect_callers, i, e)
    {
      /* Redirect calls to the old version node to point to its new
	 version.  */
      cgraph_redirect_edge_callee (e, new_node);
    }


  for (e = n->callees;e; e=e->next_callee)
    cgraph_clone_edge (e, new_node, e->call_stmt, e->lto_stmt_uid,
		       count_scale, freq, update_original);

  for (e = n->indirect_calls; e; e = e->next_callee)
    cgraph_clone_edge (e, new_node, e->call_stmt, e->lto_stmt_uid,
		       count_scale, freq, update_original);
  ipa_clone_references (new_node, NULL, &n->ref_list);

  new_node->next_sibling_clone = n->clones;
  if (n->clones)
    n->clones->prev_sibling_clone = new_node;
  n->clones = new_node;
  new_node->clone_of = n;

  if (n->decl != decl)
    {
      struct cgraph_node **slot;
      slot = (struct cgraph_node **) htab_find_slot (cgraph_hash, new_node, INSERT);
      gcc_assert (!*slot);
      *slot = new_node;
      if (assembler_name_hash)
	{
	  void **aslot;
	  tree name = DECL_ASSEMBLER_NAME (decl);

	  aslot = htab_find_slot_with_hash (assembler_name_hash, name,
					    decl_assembler_name_hash (name),
					    INSERT);
	  gcc_assert (!*aslot);
	  *aslot = new_node;
	}
    }

  if (call_duplication_hook)
    cgraph_call_node_duplication_hooks (n, new_node);
  return new_node;
}

/* Create a new name for clone of DECL, add SUFFIX.  Returns an identifier.  */

static GTY(()) unsigned int clone_fn_id_num;

tree
clone_function_name (tree decl, const char *suffix)
{
  tree name = DECL_ASSEMBLER_NAME (decl);
  size_t len = IDENTIFIER_LENGTH (name);
  char *tmp_name, *prefix;

  prefix = XALLOCAVEC (char, len + strlen (suffix) + 2);
  memcpy (prefix, IDENTIFIER_POINTER (name), len);
  strcpy (prefix + len + 1, suffix);
#ifndef NO_DOT_IN_LABEL
  prefix[len] = '.';
#elif !defined NO_DOLLAR_IN_LABEL
  prefix[len] = '$';
#else
  prefix[len] = '_';
#endif
  ASM_FORMAT_PRIVATE_NAME (tmp_name, prefix, clone_fn_id_num++);
  return get_identifier (tmp_name);
}

/* Create callgraph node clone with new declaration.  The actual body will
   be copied later at compilation stage.

   TODO: after merging in ipa-sra use function call notes instead of args_to_skip
   bitmap interface.
   */
struct cgraph_node *
cgraph_create_virtual_clone (struct cgraph_node *old_node,
			     VEC(cgraph_edge_p,heap) *redirect_callers,
			     VEC(ipa_replace_map_p,gc) *tree_map,
			     bitmap args_to_skip,
			     const char * suffix)
{
  tree old_decl = old_node->decl;
  struct cgraph_node *new_node = NULL;
  tree new_decl;
  size_t i;
  struct ipa_replace_map *map;

  if (!flag_wpa)
    gcc_checking_assert  (tree_versionable_function_p (old_decl));

  gcc_assert (old_node->local.can_change_signature || !args_to_skip);

  /* Make a new FUNCTION_DECL tree node */
  if (!args_to_skip)
    new_decl = copy_node (old_decl);
  else
    new_decl = build_function_decl_skip_args (old_decl, args_to_skip);
  DECL_STRUCT_FUNCTION (new_decl) = NULL;

  /* Generate a new name for the new version. */
  DECL_NAME (new_decl) = clone_function_name (old_decl, suffix);
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));
  SET_DECL_RTL (new_decl, NULL);

  new_node = cgraph_clone_node (old_node, new_decl, old_node->count,
				CGRAPH_FREQ_BASE, false,
				redirect_callers, false);
  /* Update the properties.
     Make clone visible only within this translation unit.  Make sure
     that is not weak also.
     ??? We cannot use COMDAT linkage because there is no
     ABI support for this.  */
  DECL_EXTERNAL (new_node->decl) = 0;
  if (DECL_ONE_ONLY (old_decl))
    DECL_SECTION_NAME (new_node->decl) = NULL;
  DECL_COMDAT_GROUP (new_node->decl) = 0;
  TREE_PUBLIC (new_node->decl) = 0;
  DECL_COMDAT (new_node->decl) = 0;
  DECL_WEAK (new_node->decl) = 0;
  DECL_STATIC_CONSTRUCTOR (new_node->decl) = 0;
  DECL_STATIC_DESTRUCTOR (new_node->decl) = 0;
  new_node->clone.tree_map = tree_map;
  new_node->clone.args_to_skip = args_to_skip;
  FOR_EACH_VEC_ELT (ipa_replace_map_p, tree_map, i, map)
    {
      tree var = map->new_tree;

      STRIP_NOPS (var);
      if (TREE_CODE (var) != ADDR_EXPR)
	continue;
      var = get_base_var (var);
      if (!var)
	continue;

      /* Record references of the future statement initializing the constant
	 argument.  */
      if (TREE_CODE (var) == FUNCTION_DECL)
	{
	  struct cgraph_node *ref_node = cgraph_get_node (var);
	  gcc_checking_assert (ref_node);
	  ipa_record_reference (new_node, NULL, ref_node, NULL, IPA_REF_ADDR,
				NULL);
	}
      else if (TREE_CODE (var) == VAR_DECL)
	ipa_record_reference (new_node, NULL, NULL, varpool_node (var),
			      IPA_REF_ADDR, NULL);
    }
  if (!args_to_skip)
    new_node->clone.combined_args_to_skip = old_node->clone.combined_args_to_skip;
  else if (old_node->clone.combined_args_to_skip)
    {
      int newi = 0, oldi = 0;
      tree arg;
      bitmap new_args_to_skip = BITMAP_GGC_ALLOC ();
      struct cgraph_node *orig_node;
      for (orig_node = old_node; orig_node->clone_of; orig_node = orig_node->clone_of)
        ;
      for (arg = DECL_ARGUMENTS (orig_node->decl); arg; arg = DECL_CHAIN (arg), oldi++)
	{
	  if (bitmap_bit_p (old_node->clone.combined_args_to_skip, oldi))
	    {
	      bitmap_set_bit (new_args_to_skip, oldi);
	      continue;
	    }
	  if (bitmap_bit_p (args_to_skip, newi))
	    bitmap_set_bit (new_args_to_skip, oldi);
	  newi++;
	}
      new_node->clone.combined_args_to_skip = new_args_to_skip;
    }
  else
    new_node->clone.combined_args_to_skip = args_to_skip;
  new_node->local.externally_visible = 0;
  new_node->local.local = 1;
  new_node->lowered = true;
  new_node->reachable = true;

  cgraph_call_node_duplication_hooks (old_node, new_node);


  return new_node;
}

/* NODE is no longer nested function; update cgraph accordingly.  */
void
cgraph_unnest_node (struct cgraph_node *node)
{
  struct cgraph_node **node2 = &node->origin->nested;
  gcc_assert (node->origin);

  while (*node2 != node)
    node2 = &(*node2)->next_nested;
  *node2 = node->next_nested;
  node->origin = NULL;
}

/* Return function availability.  See cgraph.h for description of individual
   return values.  */
enum availability
cgraph_function_body_availability (struct cgraph_node *node)
{
  enum availability avail;
  gcc_assert (cgraph_function_flags_ready);
  if (!node->analyzed)
    avail = AVAIL_NOT_AVAILABLE;
  else if (node->local.local)
    avail = AVAIL_LOCAL;
  else if (!node->local.externally_visible)
    avail = AVAIL_AVAILABLE;
  /* Inline functions are safe to be analyzed even if their symbol can
     be overwritten at runtime.  It is not meaningful to enforce any sane
     behaviour on replacing inline function by different body.  */
  else if (DECL_DECLARED_INLINE_P (node->decl))
    avail = AVAIL_AVAILABLE;

  /* If the function can be overwritten, return OVERWRITABLE.  Take
     care at least of two notable extensions - the COMDAT functions
     used to share template instantiations in C++ (this is symmetric
     to code cp_cannot_inline_tree_fn and probably shall be shared and
     the inlinability hooks completely eliminated).

     ??? Does the C++ one definition rule allow us to always return
     AVAIL_AVAILABLE here?  That would be good reason to preserve this
     bit.  */

  else if (decl_replaceable_p (node->decl) && !DECL_EXTERNAL (node->decl))
    avail = AVAIL_OVERWRITABLE;
  else avail = AVAIL_AVAILABLE;

  return avail;
}

/* Add the function FNDECL to the call graph.
   Unlike cgraph_finalize_function, this function is intended to be used
   by middle end and allows insertion of new function at arbitrary point
   of compilation.  The function can be either in high, low or SSA form
   GIMPLE.

   The function is assumed to be reachable and have address taken (so no
   API breaking optimizations are performed on it).

   Main work done by this function is to enqueue the function for later
   processing to avoid need the passes to be re-entrant.  */

void
cgraph_add_new_function (tree fndecl, bool lowered)
{
  struct cgraph_node *node;
  switch (cgraph_state)
    {
      case CGRAPH_STATE_CONSTRUCTION:
	/* Just enqueue function to be processed at nearest occurrence.  */
	node = cgraph_create_node (fndecl);
	node->next_needed = cgraph_new_nodes;
	if (lowered)
	  node->lowered = true;
	cgraph_new_nodes = node;
        break;

      case CGRAPH_STATE_IPA:
      case CGRAPH_STATE_IPA_SSA:
      case CGRAPH_STATE_EXPANSION:
	/* Bring the function into finalized state and enqueue for later
	   analyzing and compilation.  */
	node = cgraph_get_create_node (fndecl);
	node->local.local = false;
	node->local.finalized = true;
	node->reachable = node->needed = true;
	if (!lowered && cgraph_state == CGRAPH_STATE_EXPANSION)
	  {
	    push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	    current_function_decl = fndecl;
	    gimple_register_cfg_hooks ();
	    tree_lowering_passes (fndecl);
	    bitmap_obstack_initialize (NULL);
	    if (!gimple_in_ssa_p (DECL_STRUCT_FUNCTION (fndecl)))
	      execute_pass_list (pass_early_local_passes.pass.sub);
	    bitmap_obstack_release (NULL);
	    pop_cfun ();
	    current_function_decl = NULL;

	    lowered = true;
	  }
	if (lowered)
	  node->lowered = true;
	node->next_needed = cgraph_new_nodes;
	cgraph_new_nodes = node;
        break;

      case CGRAPH_STATE_FINISHED:
	/* At the very end of compilation we have to do all the work up
	   to expansion.  */
	node = cgraph_create_node (fndecl);
	if (lowered)
	  node->lowered = true;
	cgraph_analyze_function (node);
	push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	current_function_decl = fndecl;
	gimple_register_cfg_hooks ();
	bitmap_obstack_initialize (NULL);
	if (!gimple_in_ssa_p (DECL_STRUCT_FUNCTION (fndecl)))
	  execute_pass_list (pass_early_local_passes.pass.sub);
	bitmap_obstack_release (NULL);
	tree_rest_of_compilation (fndecl);
	pop_cfun ();
	current_function_decl = NULL;
	break;
    }

  /* Set a personality if required and we already passed EH lowering.  */
  if (lowered
      && (function_needs_eh_personality (DECL_STRUCT_FUNCTION (fndecl))
	  == eh_personality_lang))
    DECL_FUNCTION_PERSONALITY (fndecl) = lang_hooks.eh_personality ();
}

/* Worker for cgraph_node_can_be_local_p.  */
static bool
cgraph_node_cannot_be_local_p_1 (struct cgraph_node *node,
				 void *data ATTRIBUTE_UNUSED)
{
  return !(!node->needed
	   && ((DECL_COMDAT (node->decl) && !node->same_comdat_group)
	       || !node->local.externally_visible));
}

/* Return true if NODE can be made local for API change.
   Extern inline functions and C++ COMDAT functions can be made local
   at the expense of possible code size growth if function is used in multiple
   compilation units.  */
bool
cgraph_node_can_be_local_p (struct cgraph_node *node)
{
  return (!node->address_taken
	  && !cgraph_for_node_and_aliases (node,
					   cgraph_node_cannot_be_local_p_1,
					   NULL, true));
}

/* Make DECL local.  FIXME: We shouldn't need to mess with rtl this early,
   but other code such as notice_global_symbol generates rtl.  */
void
cgraph_make_decl_local (tree decl)
{
  rtx rtl, symbol;

  if (TREE_CODE (decl) == VAR_DECL)
    DECL_COMMON (decl) = 0;
  else gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);

  if (DECL_ONE_ONLY (decl) || DECL_COMDAT (decl))
    {
      /* It is possible that we are linking against library defining same COMDAT
	 function.  To avoid conflict we need to rename our local name of the
	 function just in the case WHOPR partitioning decide to make it hidden
	 to avoid cross partition references.  */
      if (flag_wpa)
	{
	  const char *old_name;

	  old_name  = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    {
	      struct cgraph_node *node = cgraph_get_node (decl);
	      change_decl_assembler_name (decl,
					  clone_function_name (decl, "local"));
	      if (node->local.lto_file_data)
		lto_record_renamed_decl (node->local.lto_file_data,
					 old_name,
					 IDENTIFIER_POINTER
					   (DECL_ASSEMBLER_NAME (decl)));
	    }
	  else if (TREE_CODE (decl) == VAR_DECL)
	    {
	      struct varpool_node *vnode = varpool_get_node (decl);
	      /* change_decl_assembler_name will warn here on vtables because
		 C++ frontend still sets TREE_SYMBOL_REFERENCED on them.  */
	      SET_DECL_ASSEMBLER_NAME (decl,
				       clone_function_name (decl, "local"));
	      if (vnode->lto_file_data)
		lto_record_renamed_decl (vnode->lto_file_data,
					 old_name,
					 IDENTIFIER_POINTER
					   (DECL_ASSEMBLER_NAME (decl)));
	    }
	}
      DECL_SECTION_NAME (decl) = 0;
      DECL_COMDAT (decl) = 0;
    }
  DECL_COMDAT_GROUP (decl) = 0;
  DECL_WEAK (decl) = 0;
  DECL_EXTERNAL (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  if (!DECL_RTL_SET_P (decl))
    return;

  /* Update rtl flags.  */
  make_decl_rtl (decl);

  rtl = DECL_RTL (decl);
  if (!MEM_P (rtl))
    return;

  symbol = XEXP (rtl, 0);
  if (GET_CODE (symbol) != SYMBOL_REF)
    return;

  SYMBOL_REF_WEAK (symbol) = DECL_WEAK (decl);
}

/* Call calback on NODE, thunks and aliases asociated to NODE. 
   When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
   skipped. */

bool
cgraph_for_node_thunks_and_aliases (struct cgraph_node *node,
			            bool (*callback) (struct cgraph_node *, void *),
			            void *data,
				    bool include_overwritable)
{
  struct cgraph_edge *e;
  int i;
  struct ipa_ref *ref;

  if (callback (node, data))
    return true;
  for (e = node->callers; e; e = e->next_caller)
    if (e->caller->thunk.thunk_p
	&& (include_overwritable
	    || cgraph_function_body_availability (e->caller) > AVAIL_OVERWRITABLE))
      if (cgraph_for_node_thunks_and_aliases (e->caller, callback, data,
					      include_overwritable))
	return true;
  for (i = 0; ipa_ref_list_refering_iterate (&node->ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      {
	struct cgraph_node *alias = ipa_ref_refering_node (ref);
	if (include_overwritable
	    || cgraph_function_body_availability (alias) > AVAIL_OVERWRITABLE)
	  if (cgraph_for_node_thunks_and_aliases (alias, callback, data,
						  include_overwritable))
	    return true;
      }
  return false;
}

/* Call calback on NODE and aliases asociated to NODE. 
   When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
   skipped. */

bool
cgraph_for_node_and_aliases (struct cgraph_node *node,
			     bool (*callback) (struct cgraph_node *, void *),
			     void *data,
			     bool include_overwritable)
{
  int i;
  struct ipa_ref *ref;

  if (callback (node, data))
    return true;
  for (i = 0; ipa_ref_list_refering_iterate (&node->ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      {
	struct cgraph_node *alias = ipa_ref_refering_node (ref);
	if (include_overwritable
	    || cgraph_function_body_availability (alias) > AVAIL_OVERWRITABLE)
          if (cgraph_for_node_and_aliases (alias, callback, data,
					   include_overwritable))
	    return true;
      }
  return false;
}

/* Worker to bring NODE local.  */

static bool
cgraph_make_node_local_1 (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  gcc_checking_assert (cgraph_node_can_be_local_p (node));
  if (DECL_COMDAT (node->decl) || DECL_EXTERNAL (node->decl))
    {
      cgraph_make_decl_local (node->decl);

      node->local.externally_visible = false;
      node->local.local = true;
      node->resolution = LDPR_PREVAILING_DEF_IRONLY;
      gcc_assert (cgraph_function_body_availability (node) == AVAIL_LOCAL);
    }
  return false;
}

/* Bring NODE local.  */

void
cgraph_make_node_local (struct cgraph_node *node)
{
  cgraph_for_node_thunks_and_aliases (node, cgraph_make_node_local_1,
				      NULL, true);
}

/* Worker to set nothrow flag.  */

static bool
cgraph_set_nothrow_flag_1 (struct cgraph_node *node, void *data)
{
  struct cgraph_edge *e;

  TREE_NOTHROW (node->decl) = data != NULL;

  if (data != NULL)
    for (e = node->callers; e; e = e->next_caller)
      e->can_throw_external = false;
  return false;
}

/* Set TREE_NOTHROW on NODE's decl and on aliases of NODE
   if any to NOTHROW.  */

void
cgraph_set_nothrow_flag (struct cgraph_node *node, bool nothrow)
{
  cgraph_for_node_thunks_and_aliases (node, cgraph_set_nothrow_flag_1,
			              (void *)(size_t)nothrow, false);
}

/* Worker to set const flag.  */

static bool
cgraph_set_const_flag_1 (struct cgraph_node *node, void *data)
{
  /* Static constructors and destructors without a side effect can be
     optimized out.  */
  if (data && !((size_t)data & 2))
    {
      if (DECL_STATIC_CONSTRUCTOR (node->decl))
	DECL_STATIC_CONSTRUCTOR (node->decl) = 0;
      if (DECL_STATIC_DESTRUCTOR (node->decl))
	DECL_STATIC_DESTRUCTOR (node->decl) = 0;
    }
  TREE_READONLY (node->decl) = data != NULL;
  DECL_LOOPING_CONST_OR_PURE_P (node->decl) = ((size_t)data & 2) != 0;
  return false;
}

/* Set TREE_READONLY on NODE's decl and on aliases of NODE
   if any to READONLY.  */

void
cgraph_set_const_flag (struct cgraph_node *node, bool readonly, bool looping)
{
  cgraph_for_node_thunks_and_aliases (node, cgraph_set_const_flag_1,
			              (void *)(size_t)(readonly + (int)looping * 2),
				      false);
}

/* Worker to set pure flag.  */

static bool
cgraph_set_pure_flag_1 (struct cgraph_node *node, void *data)
{
  /* Static pureructors and destructors without a side effect can be
     optimized out.  */
  if (data && !((size_t)data & 2))
    {
      if (DECL_STATIC_CONSTRUCTOR (node->decl))
	DECL_STATIC_CONSTRUCTOR (node->decl) = 0;
      if (DECL_STATIC_DESTRUCTOR (node->decl))
	DECL_STATIC_DESTRUCTOR (node->decl) = 0;
    }
  DECL_PURE_P (node->decl) = data != NULL;
  DECL_LOOPING_CONST_OR_PURE_P (node->decl) = ((size_t)data & 2) != 0;
  return false;
}

/* Set DECL_PURE_P on NODE's decl and on aliases of NODE
   if any to PURE.  */

void
cgraph_set_pure_flag (struct cgraph_node *node, bool pure, bool looping)
{
  cgraph_for_node_thunks_and_aliases (node, cgraph_set_pure_flag_1,
			              (void *)(size_t)(pure + (int)looping * 2),
				      false);
}

/* Data used by cgraph_propagate_frequency.  */

struct cgraph_propagate_frequency_data
{
  bool maybe_unlikely_executed;
  bool maybe_executed_once;
  bool only_called_at_startup;
  bool only_called_at_exit;
};

/* Worker for cgraph_propagate_frequency_1.  */

static bool
cgraph_propagate_frequency_1 (struct cgraph_node *node, void *data)
{
  struct cgraph_propagate_frequency_data *d;
  struct cgraph_edge *edge;

  d = (struct cgraph_propagate_frequency_data *)data;
  for (edge = node->callers;
       edge && (d->maybe_unlikely_executed || d->maybe_executed_once
	        || d->only_called_at_startup || d->only_called_at_exit);
       edge = edge->next_caller)
    {
      if (edge->caller != node)
	{
          d->only_called_at_startup &= edge->caller->only_called_at_startup;
	  /* It makes sense to put main() together with the static constructors.
	     It will be executed for sure, but rest of functions called from
	     main are definitely not at startup only.  */
	  if (MAIN_NAME_P (DECL_NAME (edge->caller->decl)))
	    d->only_called_at_startup = 0;
          d->only_called_at_exit &= edge->caller->only_called_at_exit;
	}
      if (!edge->frequency)
	continue;
      switch (edge->caller->frequency)
        {
	case NODE_FREQUENCY_UNLIKELY_EXECUTED:
	  break;
	case NODE_FREQUENCY_EXECUTED_ONCE:
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Called by %s that is executed once\n",
		     cgraph_node_name (edge->caller));
	  d->maybe_unlikely_executed = false;
	  if (inline_edge_summary (edge)->loop_depth)
	    {
	      d->maybe_executed_once = false;
	      if (dump_file && (dump_flags & TDF_DETAILS))
	        fprintf (dump_file, "  Called in loop\n");
	    }
	  break;
	case NODE_FREQUENCY_HOT:
	case NODE_FREQUENCY_NORMAL:
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Called by %s that is normal or hot\n",
		     cgraph_node_name (edge->caller));
	  d->maybe_unlikely_executed = false;
	  d->maybe_executed_once = false;
	  break;
	}
    }
  return edge != NULL;
}

/* See if the frequency of NODE can be updated based on frequencies of its
   callers.  */
bool
cgraph_propagate_frequency (struct cgraph_node *node)
{
  struct cgraph_propagate_frequency_data d = {true, true, true, true};
  bool changed = false;

  if (!node->local.local)
    return false;
  gcc_assert (node->analyzed);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Processing frequency %s\n", cgraph_node_name (node));

  cgraph_for_node_and_aliases (node, cgraph_propagate_frequency_1, &d, true);

  if ((d.only_called_at_startup && !d.only_called_at_exit)
      && !node->only_called_at_startup)
    {
       node->only_called_at_startup = true;
       if (dump_file)
         fprintf (dump_file, "Node %s promoted to only called at startup.\n",
		  cgraph_node_name (node));
       changed = true;
    }
  if ((d.only_called_at_exit && !d.only_called_at_startup)
      && !node->only_called_at_exit)
    {
       node->only_called_at_exit = true;
       if (dump_file)
         fprintf (dump_file, "Node %s promoted to only called at exit.\n",
		  cgraph_node_name (node));
       changed = true;
    }
  /* These come either from profile or user hints; never update them.  */
  if (node->frequency == NODE_FREQUENCY_HOT
      || node->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED)
    return changed;
  if (d.maybe_unlikely_executed)
    {
      node->frequency = NODE_FREQUENCY_UNLIKELY_EXECUTED;
      if (dump_file)
	fprintf (dump_file, "Node %s promoted to unlikely executed.\n",
		 cgraph_node_name (node));
      changed = true;
    }
  else if (d.maybe_executed_once && node->frequency != NODE_FREQUENCY_EXECUTED_ONCE)
    {
      node->frequency = NODE_FREQUENCY_EXECUTED_ONCE;
      if (dump_file)
	fprintf (dump_file, "Node %s promoted to executed once.\n",
		 cgraph_node_name (node));
      changed = true;
    }
  return changed;
}

/* Return true when NODE can not return or throw and thus
   it is safe to ignore its side effects for IPA analysis.  */

bool
cgraph_node_cannot_return (struct cgraph_node *node)
{
  int flags = flags_from_decl_or_type (node->decl);
  if (!flag_exceptions)
    return (flags & ECF_NORETURN) != 0;
  else
    return ((flags & (ECF_NORETURN | ECF_NOTHROW))
	     == (ECF_NORETURN | ECF_NOTHROW));
}

/* Return true when call of E can not lead to return from caller
   and thus it is safe to ignore its side effects for IPA analysis
   when computing side effects of the caller.
   FIXME: We could actually mark all edges that have no reaching
   patch to EXIT_BLOCK_PTR or throw to get better results.  */
bool
cgraph_edge_cannot_lead_to_return (struct cgraph_edge *e)
{
  if (cgraph_node_cannot_return (e->caller))
    return true;
  if (e->indirect_unknown_callee)
    {
      int flags = e->indirect_info->ecf_flags;
      if (!flag_exceptions)
	return (flags & ECF_NORETURN) != 0;
      else
	return ((flags & (ECF_NORETURN | ECF_NOTHROW))
		 == (ECF_NORETURN | ECF_NOTHROW));
    }
  else
    return cgraph_node_cannot_return (e->callee);
}

/* Return true when function NODE can be removed from callgraph
   if all direct calls are eliminated.  */

bool
cgraph_can_remove_if_no_direct_calls_and_refs_p (struct cgraph_node *node)
{
  gcc_assert (!node->global.inlined_to);
  /* Extern inlines can always go, we will use the external definition.  */
  if (DECL_EXTERNAL (node->decl))
    return true;
  /* When function is needed, we can not remove it.  */
  if (node->needed || node->reachable_from_other_partition)
    return false;
  if (DECL_STATIC_CONSTRUCTOR (node->decl)
      || DECL_STATIC_DESTRUCTOR (node->decl))
    return false;
  /* Only COMDAT functions can be removed if externally visible.  */
  if (node->local.externally_visible
      && (!DECL_COMDAT (node->decl)
	  || cgraph_used_from_object_file_p (node)))
    return false;
  return true;
}

/* Worker for cgraph_can_remove_if_no_direct_calls_p.  */

static bool
nonremovable_p (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  return !cgraph_can_remove_if_no_direct_calls_and_refs_p (node);
}

/* Return true when function NODE and its aliases can be removed from callgraph
   if all direct calls are eliminated.  */

bool
cgraph_can_remove_if_no_direct_calls_p (struct cgraph_node *node)
{
  /* Extern inlines can always go, we will use the external definition.  */
  if (DECL_EXTERNAL (node->decl))
    return true;
  if (node->address_taken)
    return false;
  return !cgraph_for_node_and_aliases (node, nonremovable_p, NULL, true);
}

/* Worker for cgraph_can_remove_if_no_direct_calls_p.  */

static bool
used_from_object_file_p (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  return cgraph_used_from_object_file_p (node);
}

/* Return true when function NODE can be expected to be removed
   from program when direct calls in this compilation unit are removed.

   As a special case COMDAT functions are
   cgraph_can_remove_if_no_direct_calls_p while the are not
   cgraph_only_called_directly_p (it is possible they are called from other
   unit)

   This function behaves as cgraph_only_called_directly_p because eliminating
   all uses of COMDAT function does not make it necessarily disappear from
   the program unless we are compiling whole program or we do LTO.  In this
   case we know we win since dynamic linking will not really discard the
   linkonce section.  */

bool
cgraph_will_be_removed_from_program_if_no_direct_calls (struct cgraph_node *node)
{
  gcc_assert (!node->global.inlined_to);
  if (cgraph_for_node_and_aliases (node, used_from_object_file_p, NULL, true))
    return false;
  if (!in_lto_p && !flag_whole_program)
    return cgraph_only_called_directly_p (node);
  else
    {
       if (DECL_EXTERNAL (node->decl))
         return true;
      return cgraph_can_remove_if_no_direct_calls_p (node);
    }
}

/* Return true when RESOLUTION indicate that linker will use
   the symbol from non-LTO object files.  */

bool
resolution_used_from_other_file_p (enum ld_plugin_symbol_resolution resolution)
{
  return (resolution == LDPR_PREVAILING_DEF
          || resolution == LDPR_PREEMPTED_REG
          || resolution == LDPR_RESOLVED_EXEC
          || resolution == LDPR_RESOLVED_DYN);
}


/* Return true when NODE is known to be used from other (non-LTO) object file.
   Known only when doing LTO via linker plugin.  */

bool
cgraph_used_from_object_file_p (struct cgraph_node *node)
{
  gcc_assert (!node->global.inlined_to);
  if (!TREE_PUBLIC (node->decl) || DECL_EXTERNAL (node->decl))
    return false;
  if (resolution_used_from_other_file_p (node->resolution))
    return true;
  return false;
}

/* Worker for cgraph_only_called_directly_p.  */

static bool
cgraph_not_only_called_directly_p_1 (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  return !cgraph_only_called_directly_or_aliased_p (node);
}

/* Return true when function NODE and all its aliases are only called
   directly.
   i.e. it is not externally visible, address was not taken and
   it is not used in any other non-standard way.  */

bool
cgraph_only_called_directly_p (struct cgraph_node *node)
{
  gcc_assert (cgraph_function_or_thunk_node (node, NULL) == node);
  return !cgraph_for_node_and_aliases (node, cgraph_not_only_called_directly_p_1,
				       NULL, true);
}


/* Collect all callers of NODE.  Worker for collect_callers_of_node.  */

static bool
collect_callers_of_node_1 (struct cgraph_node *node, void *data)
{
  VEC (cgraph_edge_p, heap) ** redirect_callers = (VEC (cgraph_edge_p, heap) **)data;
  struct cgraph_edge *cs;
  enum availability avail;
  cgraph_function_or_thunk_node (node, &avail);

  if (avail > AVAIL_OVERWRITABLE)
    for (cs = node->callers; cs != NULL; cs = cs->next_caller)
      if (!cs->indirect_inlining_edge)
        VEC_safe_push (cgraph_edge_p, heap, *redirect_callers, cs);
  return false;
}

/* Collect all callers of NODE and its aliases that are known to lead to NODE
   (i.e. are not overwritable).  */

VEC (cgraph_edge_p, heap) *
collect_callers_of_node (struct cgraph_node *node)
{
  VEC (cgraph_edge_p, heap) * redirect_callers = NULL;
  cgraph_for_node_and_aliases (node, collect_callers_of_node_1,
			       &redirect_callers, false);
  return redirect_callers;
}

#include "gt-cgraph.h"
