/* Callgraph handling code.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009
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

    The callgraph at the moment does not represent indirect calls or calls
    from other compilation unit.  Flag NEEDED is set for each node that may
    be accessed in such an invisible way and it shall be considered an
    entry point to the callgraph.

    Interprocedural information:

      Callgraph is place to store data needed for interprocedural optimization.
      All data structures are divided into three components: local_info that
      is produced while analyzing the function, global_info that is result
      of global walking of the callgraph on the end of compilation and
      rtl_info used by RTL backend to propagate data from already compiled
      functions to their callers.

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

/* Maximal pid used for profiling */
int cgraph_max_pid;

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

/* List of hooks trigerred on cgraph_edge events.  */
struct cgraph_edge_hook_list {
  cgraph_edge_hook hook;
  void *data;
  struct cgraph_edge_hook_list *next;
};

/* List of hooks trigerred on cgraph_node events.  */
struct cgraph_node_hook_list {
  cgraph_node_hook hook;
  void *data;
  struct cgraph_node_hook_list *next;
};

/* List of hooks trigerred on events involving two cgraph_edges.  */
struct cgraph_2edge_hook_list {
  cgraph_2edge_hook hook;
  void *data;
  struct cgraph_2edge_hook_list *next;
};

/* List of hooks trigerred on events involving two cgraph_nodes.  */
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

/* Register HOOK to be called with DATA on each removed node.  */
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

/* Remove ENTRY from the list of hooks called on removing nodes.  */
void
cgraph_remove_function_insertion_hook (struct cgraph_node_hook_list *entry)
{
  struct cgraph_node_hook_list **ptr = &first_cgraph_function_insertion_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all node removal hooks.  */
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
static void
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

/* Allocate new callgraph node and insert it into basic data structures.  */

static struct cgraph_node *
cgraph_create_node (void)
{
  struct cgraph_node *node;

  if (free_nodes)
    {
      node = free_nodes;
      free_nodes = NEXT_FREE_NODE (node);
    }
  else
    {
      node = GGC_CNEW (struct cgraph_node);
      node->uid = cgraph_max_uid++;
    }

  node->next = cgraph_nodes;
  node->pid = -1;
  node->order = cgraph_order++;
  if (cgraph_nodes)
    cgraph_nodes->previous = node;
  node->previous = NULL;
  node->global.estimated_growth = INT_MIN;
  cgraph_nodes = node;
  cgraph_n_nodes++;
  return node;
}

/* Return cgraph node assigned to DECL.  Create new one when needed.  */

struct cgraph_node *
cgraph_node (tree decl)
{
  struct cgraph_node key, *node, **slot;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);

  if (!cgraph_hash)
    cgraph_hash = htab_create_ggc (10, hash_node, eq_node, NULL);

  key.decl = decl;

  slot = (struct cgraph_node **) htab_find_slot (cgraph_hash, &key, INSERT);

  if (*slot)
    {
      node = *slot;
      return node;
    }

  node = cgraph_create_node ();
  node->decl = decl;
  *slot = node;
  if (DECL_CONTEXT (decl) && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL)
    {
      node->origin = cgraph_node (DECL_CONTEXT (decl));
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
	    if (*slot)
	      continue;
	    *slot = node;
	  }
    }

  slot = htab_find_slot_with_hash (assembler_name_hash, asmname,
				   decl_assembler_name_hash (asmname),
				   NO_INSERT);

  if (slot)
    return (struct cgraph_node *) *slot;
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
  for (e = node->callees; e; e= e->next_callee)
    {
      if (e->call_stmt == call_stmt)
	break;
      n++;
    }

  if (n > 100)
    {
      node->call_site_hash = htab_create_ggc (120, edge_hash, edge_eq, NULL);
      for (e2 = node->callees; e2; e2 = e2->next_callee)
	{
          void **slot;
	  slot = htab_find_slot_with_hash (node->call_site_hash,
					   e2->call_stmt,
					   htab_hash_pointer (e2->call_stmt),
					   INSERT);
	  gcc_assert (!*slot);
	  *slot = e2;
	}
    }

  return e;
}


/* Change field call_stmt of edge E to NEW_STMT.  */

void
cgraph_set_call_stmt (struct cgraph_edge *e, gimple new_stmt)
{
  if (e->caller->call_site_hash)
    {
      htab_remove_elt_with_hash (e->caller->call_site_hash,
				 e->call_stmt,
				 htab_hash_pointer (e->call_stmt));
    }
  e->call_stmt = new_stmt;
  push_cfun (DECL_STRUCT_FUNCTION (e->caller->decl));
  e->can_throw_external = stmt_can_throw_external (new_stmt);
  pop_cfun ();
  if (e->caller->call_site_hash)
    {
      void **slot;
      slot = htab_find_slot_with_hash (e->caller->call_site_hash,
				       e->call_stmt,
				       htab_hash_pointer
				       (e->call_stmt), INSERT);
      gcc_assert (!*slot);
      *slot = e;
    }
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
   same function body.  
   
   TODO: COUNT and LOOP_DEPTH should be properly distributed based on relative
   frequencies of the clones.  */

void
cgraph_create_edge_including_clones (struct cgraph_node *orig,
				     struct cgraph_node *callee,
				     gimple stmt, gcov_type count,
				     int freq, int loop_depth,
				     cgraph_inline_failed_t reason)
{
  struct cgraph_node *node;
  struct cgraph_edge *edge;

  if (!cgraph_edge (orig, stmt))
    {
      edge = cgraph_create_edge (orig, callee, stmt, count, freq, loop_depth);
      edge->inline_failed = reason;
    }

  node = orig->clones;
  if (node)
    while (node != orig)
      {
        /* It is possible that we already constant propagated into the clone
	   and turned indirect call into dirrect call.  */
        if (!cgraph_edge (node, stmt))
	  {
	    edge = cgraph_create_edge (node, callee, stmt, count,
				       freq, loop_depth);
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

/* Give initial reasons why inlining would fail on EDGE.  This gets either
   nullified or usually overwritten by more precise reasons later.  */

static void
initialize_inline_failed (struct cgraph_edge *e)
{
  struct cgraph_node *callee = e->callee;

  if (!callee->analyzed)
    e->inline_failed = CIF_BODY_NOT_AVAILABLE;
  else if (callee->local.redefined_extern_inline)
    e->inline_failed = CIF_REDEFINED_EXTERN_INLINE;
  else if (!callee->local.inlinable)
    e->inline_failed = CIF_FUNCTION_NOT_INLINABLE;
  else if (gimple_call_cannot_inline_p (e->call_stmt))
    e->inline_failed = CIF_MISMATCHED_ARGUMENTS;
  else
    e->inline_failed = CIF_FUNCTION_NOT_CONSIDERED;
}

/* Create edge from CALLER to CALLEE in the cgraph.  */

struct cgraph_edge *
cgraph_create_edge (struct cgraph_node *caller, struct cgraph_node *callee,
		    gimple call_stmt, gcov_type count, int freq, int nest)
{
  struct cgraph_edge *edge;

#ifdef ENABLE_CHECKING
  /* This is rather pricely check possibly trigerring construction of call stmt
     hashtable.  */
  gcc_assert (!cgraph_edge (caller, call_stmt));
#endif

  gcc_assert (is_gimple_call (call_stmt));

  if (free_edges)
    {
      edge = free_edges;
      free_edges = NEXT_FREE_EDGE (edge);
    }
  else
    {
      edge = GGC_NEW (struct cgraph_edge);
      edge->uid = cgraph_edge_max_uid++;
    }

  edge->aux = NULL;

  edge->caller = caller;
  edge->callee = callee;
  edge->call_stmt = call_stmt;
  push_cfun (DECL_STRUCT_FUNCTION (caller->decl));
  edge->can_throw_external = stmt_can_throw_external (call_stmt);
  pop_cfun ();
  edge->prev_caller = NULL;
  edge->next_caller = callee->callers;
  if (callee->callers)
    callee->callers->prev_caller = edge;
  edge->prev_callee = NULL;
  edge->next_callee = caller->callees;
  if (caller->callees)
    caller->callees->prev_callee = edge;
  caller->callees = edge;
  callee->callers = edge;
  edge->count = count;
  gcc_assert (count >= 0);
  edge->frequency = freq;
  gcc_assert (freq >= 0);
  gcc_assert (freq <= CGRAPH_FREQ_MAX);
  edge->loop_nest = nest;
  edge->indirect_call = 0;
  if (caller->call_site_hash)
    {
      void **slot;
      slot = htab_find_slot_with_hash (caller->call_site_hash,
				       edge->call_stmt,
				       htab_hash_pointer
					 (edge->call_stmt),
				       INSERT);
      gcc_assert (!*slot);
      *slot = edge;
    }

  initialize_inline_failed (edge);

  return edge;
}

/* Remove the edge E from the list of the callers of the callee.  */

static inline void
cgraph_edge_remove_callee (struct cgraph_edge *e)
{
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
    e->caller->callees = e->next_callee;
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

  /* Remove from callers list of the callee.  */
  cgraph_edge_remove_callee (e);

  /* Remove from callees list of the callers.  */
  cgraph_edge_remove_caller (e);

  /* Put the edge onto the free list.  */
  cgraph_free_edge (e);
}

/* Redirect callee of E to N.  The function does not update underlying
   call expression.  */

void
cgraph_redirect_edge_callee (struct cgraph_edge *e, struct cgraph_node *n)
{
  /* Remove from callers list of the current callee.  */
  cgraph_edge_remove_callee (e);

  /* Insert to callers list of the new callee.  */
  e->prev_caller = NULL;
  if (n->callers)
    n->callers->prev_caller = e;
  e->next_caller = n->callers;
  n->callers = e;
  e->callee = n;
}


/* Update or remove the corresponding cgraph edge if a GIMPLE_CALL
   OLD_STMT changed into NEW_STMT.  OLD_CALL is gimple_call_fndecl
   of OLD_STMT if it was previously call statement.  */

static void
cgraph_update_edges_for_call_stmt_node (struct cgraph_node *node,
					gimple old_stmt, tree old_call, gimple new_stmt)
{
  tree new_call = (is_gimple_call (new_stmt)) ? gimple_call_fndecl (new_stmt) : 0;

  /* We are seeing indirect calls, then there is nothing to update.  */
  if (!new_call && !old_call)
    return;
  /* See if we turned indirect call into direct call or folded call to one builtin
     into different bultin.  */
  if (old_call != new_call)
    {
      struct cgraph_edge *e = cgraph_edge (node, old_stmt);
      struct cgraph_edge *ne = NULL;
      gcov_type count;
      int frequency;
      int loop_nest;

      if (e)
	{
	  /* See if the call is already there.  It might be because of indirect
	     inlining already found it.  */
	  if (new_call && e->callee->decl == new_call)
	    return;

	  /* Otherwise remove edge and create new one; we can't simply redirect
	     since function has changed, so inline plan and other information
	     attached to edge is invalid.  */
	  cgraph_remove_edge (e);
	  count = e->count;
	  frequency = e->frequency;
	  loop_nest = e->loop_nest;
	}
      else
	{
	  /* We are seeing new direct call; compute profile info based on BB.  */
	  basic_block bb = gimple_bb (new_stmt);
	  count = bb->count;
	  frequency = compute_call_stmt_bb_frequency (current_function_decl,
						      bb);
	  loop_nest = bb->loop_depth;
	}

      if (new_call)
	{
	  ne = cgraph_create_edge (node, cgraph_node (new_call),
				   new_stmt, count, frequency,
				   loop_nest);
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
  struct cgraph_node *orig = cgraph_node (cfun->decl);
  struct cgraph_node *node;

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
      cgraph_edge_remove_callee (e);
      cgraph_free_edge (e);
    }
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
      		DECL_STRUCT_FUNCTION (node->decl)->ipa_transforms_to_apply);
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
	      next_inline_clone->prev_sibling_clone->next_sibling_clone
	        = next_inline_clone->next_sibling_clone;
	    }
	  else
	   node->clones = next_inline_clone->next_sibling_clone;

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
  else
    gcc_assert (node->clone_of);
  if (node->prev_sibling_clone)
    node->prev_sibling_clone->next_sibling_clone = node->next_sibling_clone;
  else if (node->clone_of)
    node->clone_of->clones = node->next_sibling_clone;
  if (node->next_sibling_clone)
    node->next_sibling_clone->prev_sibling_clone = node->prev_sibling_clone;
  if (node->clones)
    {
      struct cgraph_node *n;

      for (n = node->clones; n->next_sibling_clone; n = n->next_sibling_clone)
	n->clone_of = node->clone_of;
      n->clone_of = node->clone_of;
      n->next_sibling_clone = node->clone_of->clones;
      if (node->clone_of->clones)
	node->clone_of->clones->prev_sibling_clone = n;
      node->clone_of->clones = node->clones;
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
	      && (TREE_ASM_WRITTEN (n->decl) || DECL_EXTERNAL (n->decl))))
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
      notice_global_symbol (node->decl);
      node->reachable = 1;
      gcc_assert (!cgraph_global_info_ready);

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
  cgraph_mark_reachable_node (node);
}

/* Likewise indicate that a node is having address taken.  */

void
cgraph_mark_address_taken_node (struct cgraph_node *node)
{
  node->address_taken = 1;
  cgraph_mark_needed_node (node);
}

/* Return local info for the compiled function.  */

struct cgraph_local_info *
cgraph_local_info (tree decl)
{
  struct cgraph_node *node;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);
  node = cgraph_node (decl);
  return &node->local;
}

/* Return local info for the compiled function.  */

struct cgraph_global_info *
cgraph_global_info (tree decl)
{
  struct cgraph_node *node;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL && cgraph_global_info_ready);
  node = cgraph_node (decl);
  return &node->global;
}

/* Return local info for the compiled function.  */

struct cgraph_rtl_info *
cgraph_rtl_info (tree decl)
{
  struct cgraph_node *node;

  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);
  node = cgraph_node (decl);
  if (decl != current_function_decl
      && !TREE_ASM_WRITTEN (node->decl))
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
  fprintf (f, "%s/%i(%i)", cgraph_node_name (node), node->uid,
	   node->pid);
  dump_addr (f, " @", (void *)node);
  if (node->global.inlined_to)
    fprintf (f, " (inline copy in %s/%i)",
	     cgraph_node_name (node->global.inlined_to),
	     node->global.inlined_to->uid);
  if (node->clone_of)
    fprintf (f, " (clone of %s/%i)",
	     cgraph_node_name (node->clone_of),
	     node->clone_of->uid);
  if (cgraph_function_flags_ready)
    fprintf (f, " availability:%s",
	     cgraph_availability_names [cgraph_function_body_availability (node)]);
  if (node->count)
    fprintf (f, " executed "HOST_WIDEST_INT_PRINT_DEC"x",
	     (HOST_WIDEST_INT)node->count);
  if (node->local.inline_summary.self_time)
    fprintf (f, " %i time, %i benefit", node->local.inline_summary.self_time,
    					node->local.inline_summary.time_inlining_benefit);
  if (node->global.time && node->global.time
      != node->local.inline_summary.self_time)
    fprintf (f, " (%i after inlining)", node->global.time);
  if (node->local.inline_summary.self_size)
    fprintf (f, " %i size, %i benefit", node->local.inline_summary.self_size,
    					node->local.inline_summary.size_inlining_benefit);
  if (node->global.size && node->global.size
      != node->local.inline_summary.self_size)
    fprintf (f, " (%i after inlining)", node->global.size);
  if (node->local.inline_summary.estimated_self_stack_size)
    fprintf (f, " %i bytes stack usage", (int)node->local.inline_summary.estimated_self_stack_size);
  if (node->global.estimated_stack_size != node->local.inline_summary.estimated_self_stack_size)
    fprintf (f, " %i bytes after inlining", (int)node->global.estimated_stack_size);
  if (node->origin)
    fprintf (f, " nested in: %s", cgraph_node_name (node->origin));
  if (node->needed)
    fprintf (f, " needed");
  if (node->address_taken)
    fprintf (f, " address_taken");
  else if (node->reachable)
    fprintf (f, " reachable");
  if (gimple_has_body_p (node->decl))
    fprintf (f, " body");
  if (node->process)
    fprintf (f, " process");
  if (node->local.local)
    fprintf (f, " local");
  if (node->local.externally_visible)
    fprintf (f, " externally_visible");
  if (node->local.finalized)
    fprintf (f, " finalized");
  if (node->local.disregard_inline_limits)
    fprintf (f, " always_inline");
  else if (node->local.inlinable)
    fprintf (f, " inlinable");
  if (node->local.redefined_extern_inline)
    fprintf (f, " redefined_extern_inline");
  if (TREE_ASM_WRITTEN (node->decl))
    fprintf (f, " asm_written");

  fprintf (f, "\n  called by: ");
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
      if (edge->indirect_call)
	fprintf(f, "(indirect) ");
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
      if (edge->indirect_call)
	fprintf(f, "(indirect) ");
      if (edge->count)
	fprintf (f, "("HOST_WIDEST_INT_PRINT_DEC"x) ",
		 (HOST_WIDEST_INT)edge->count);
      if (edge->frequency)
	fprintf (f, "(%.2f per call) ",
		 edge->frequency / (double)CGRAPH_FREQ_BASE);
      if (edge->loop_nest)
	fprintf (f, "(nested in %i loops) ", edge->loop_nest);
      if (edge->can_throw_external)
	fprintf(f, "(can throw external) ");
    }
  fprintf (f, "\n");
}


/* Dump call graph node NODE to stderr.  */

void
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

void
debug_cgraph (void)
{
  dump_cgraph (stderr);
}


/* Set the DECL_ASSEMBLER_NAME and update cgraph hashtables.  */

void
change_decl_assembler_name (tree decl, tree name)
{
  gcc_assert (!assembler_name_hash);
  if (!DECL_ASSEMBLER_NAME_SET_P (decl))
    {
      SET_DECL_ASSEMBLER_NAME (decl, name);
      return;
    }
  if (name == DECL_ASSEMBLER_NAME (decl))
    return;

  if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))
      && DECL_RTL_SET_P (decl))
    warning (0, "%D renamed after being referenced in assembly", decl);

  SET_DECL_ASSEMBLER_NAME (decl, name);
}

/* Add a top-level asm statement to the list.  */

struct cgraph_asm_node *
cgraph_add_asm_node (tree asm_str)
{
  struct cgraph_asm_node *node;

  node = GGC_CNEW (struct cgraph_asm_node);
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
		   gimple call_stmt, gcov_type count_scale, int freq_scale,
		   int loop_nest, bool update_original)
{
  struct cgraph_edge *new_edge;
  gcov_type count = e->count * count_scale / REG_BR_PROB_BASE;
  gcov_type freq = e->frequency * (gcov_type) freq_scale / CGRAPH_FREQ_BASE;

  if (freq > CGRAPH_FREQ_MAX)
    freq = CGRAPH_FREQ_MAX;
  new_edge = cgraph_create_edge (n, e->callee, call_stmt, count, freq,
			    e->loop_nest + loop_nest);

  new_edge->inline_failed = e->inline_failed;
  new_edge->indirect_call = e->indirect_call;
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

   When UPDATE_ORIGINAL is true, the counts are subtracted from the original
   function's profile to reflect the fact that part of execution is handled
   by node.  */
struct cgraph_node *
cgraph_clone_node (struct cgraph_node *n, gcov_type count, int freq,
		   int loop_nest, bool update_original)
{
  struct cgraph_node *new_node = cgraph_create_node ();
  struct cgraph_edge *e;
  gcov_type count_scale;

  new_node->decl = n->decl;
  new_node->origin = n->origin;
  if (new_node->origin)
    {
      new_node->next_nested = new_node->origin->nested;
      new_node->origin->nested = new_node;
    }
  new_node->analyzed = n->analyzed;
  new_node->local = n->local;
  new_node->global = n->global;
  new_node->rtl = n->rtl;
  new_node->count = count;
  new_node->clone = n->clone;
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

  for (e = n->callees;e; e=e->next_callee)
    cgraph_clone_edge (e, new_node, e->call_stmt, count_scale, freq, loop_nest,
		       update_original);

  new_node->next_sibling_clone = n->clones;
  if (n->clones)
    n->clones->prev_sibling_clone = new_node;
  n->clones = new_node;
  new_node->clone_of = n;

  cgraph_call_node_duplication_hooks (n, new_node);
  return new_node;
}

/* Create a new name for omp child function.  Returns an identifier.  */

static GTY(()) unsigned int clone_fn_id_num;

static tree
clone_function_name (tree decl)
{
  tree name = DECL_ASSEMBLER_NAME (decl);
  size_t len = IDENTIFIER_LENGTH (name);
  char *tmp_name, *prefix;

  prefix = XALLOCAVEC (char, len + strlen ("_clone") + 1);
  memcpy (prefix, IDENTIFIER_POINTER (name), len);
  strcpy (prefix + len, "_clone");
#ifndef NO_DOT_IN_LABEL
  prefix[len] = '.';
#elif !defined NO_DOLLAR_IN_LABEL
  prefix[len] = '$';
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
			     bitmap args_to_skip)
{
  tree old_decl = old_node->decl;
  struct cgraph_node *new_node = NULL;
  tree new_decl;
  struct cgraph_node key, **slot;
  unsigned i;
  struct cgraph_edge *e;

  gcc_assert  (tree_versionable_function_p (old_decl));

  /* Make a new FUNCTION_DECL tree node */
  if (!args_to_skip)
    new_decl = copy_node (old_decl);
  else
    new_decl = build_function_decl_skip_args (old_decl, args_to_skip);
  DECL_STRUCT_FUNCTION (new_decl) = NULL;

  /* Generate a new name for the new version. */
  DECL_NAME (new_decl) = clone_function_name (old_decl);
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));
  SET_DECL_RTL (new_decl, NULL);

  new_node = cgraph_clone_node (old_node, old_node->count,
  				CGRAPH_FREQ_BASE, 0, false);
  new_node->decl = new_decl;
  /* Update the properties.
     Make clone visible only within this translation unit.  Make sure
     that is not weak also.
     ??? We cannot use COMDAT linkage because there is no
     ABI support for this.  */
  DECL_EXTERNAL (new_node->decl) = 0;
  DECL_COMDAT_GROUP (new_node->decl) = 0;
  TREE_PUBLIC (new_node->decl) = 0;
  DECL_COMDAT (new_node->decl) = 0;
  DECL_WEAK (new_node->decl) = 0;
  new_node->clone.tree_map = tree_map;
  new_node->clone.args_to_skip = args_to_skip;
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
      for (arg = DECL_ARGUMENTS (orig_node->decl); arg; arg = TREE_CHAIN (arg), oldi++)
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

  key.decl = new_decl;
  slot = (struct cgraph_node **) htab_find_slot (cgraph_hash, &key, INSERT);
  gcc_assert (!*slot);
  *slot = new_node;
  if (assembler_name_hash)
    {
      void **aslot;
      tree name = DECL_ASSEMBLER_NAME (new_decl);

      aslot = htab_find_slot_with_hash (assembler_name_hash, name,
					decl_assembler_name_hash (name),
					INSERT);
      gcc_assert (!*aslot);
      *aslot = new_node;
    }
   for (i = 0; VEC_iterate (cgraph_edge_p, redirect_callers, i, e); i++)
     {
       /* Redirect calls to the old version node to point to its new
	  version.  */
       cgraph_redirect_edge_callee (e, new_node);
     }
  
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
  /* Inline functions are safe to be analyzed even if their sybol can
     be overwritten at runtime.  It is not meaningful to enfore any sane
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

  else if (DECL_REPLACEABLE_P (node->decl) && !DECL_EXTERNAL (node->decl))
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
	node = cgraph_node (fndecl);
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
	node = cgraph_node (fndecl);
	node->local.local = false;
	node->local.finalized = true;
	node->reachable = node->needed = true;
	if (!lowered && cgraph_state == CGRAPH_STATE_EXPANSION)
	  {
	    push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	    current_function_decl = fndecl;
	    gimple_register_cfg_hooks ();
	    /* C++ Thunks are emitted late via this function, gimplify them.  */
	    if (!gimple_body (fndecl))
	      gimplify_function_tree (fndecl);
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
	push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	current_function_decl = fndecl;
	gimple_register_cfg_hooks ();
	if (!lowered)
          tree_lowering_passes (fndecl);
	bitmap_obstack_initialize (NULL);
	if (!gimple_in_ssa_p (DECL_STRUCT_FUNCTION (fndecl)))
	  execute_pass_list (pass_early_local_passes.pass.sub);
	bitmap_obstack_release (NULL);
	tree_rest_of_compilation (fndecl);
	pop_cfun ();
	current_function_decl = NULL;
	break;
    }
}

/* Return true if NODE can be made local for API change.
   Extern inline functions and C++ COMDAT functions can be made local
   at the expense of possible code size growth if function is used in multiple
   compilation units.  */
bool
cgraph_node_can_be_local_p (struct cgraph_node *node)
{
  return !node->needed;
}

/* Bring NODE local.  */
void
cgraph_make_node_local (struct cgraph_node *node)
{
  gcc_assert (cgraph_node_can_be_local_p (node));
  if (DECL_COMDAT (node->decl) || DECL_EXTERNAL (node->decl))
    {
      DECL_COMDAT (node->decl) = 0;
      DECL_COMDAT_GROUP (node->decl) = 0;
      TREE_PUBLIC (node->decl) = 0;
      DECL_WEAK (node->decl) = 0;
      DECL_EXTERNAL (node->decl) = 0;
      node->local.externally_visible = false;
      node->local.local = true;
      gcc_assert (cgraph_function_body_availability (node) == AVAIL_LOCAL);
    }
}

#include "gt-cgraph.h"
