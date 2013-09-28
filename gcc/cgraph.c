/* Callgraph handling code.
   Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

    The call-graph is a data structure designed for intra-procedural optimization.
    It represents a multi-graph where nodes are functions and edges are call sites. */

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
#include "intl.h"
#include "gimple.h"
#include "timevar.h"
#include "dumpfile.h"
#include "tree-ssa.h"
#include "value-prof.h"
#include "except.h"
#include "diagnostic-core.h"
#include "rtl.h"
#include "ipa-utils.h"
#include "lto-streamer.h"
#include "ipa-inline.h"
#include "cfgloop.h"
#include "gimple-pretty-print.h"

/* FIXME: Only for PROP_loops, but cgraph shouldn't have to know about this.  */
#include "tree-pass.h"

static void cgraph_node_remove_callers (struct cgraph_node *node);
static inline void cgraph_edge_remove_caller (struct cgraph_edge *e);
static inline void cgraph_edge_remove_callee (struct cgraph_edge *e);

/* Queue of cgraph nodes scheduled to be lowered.  */
symtab_node x_cgraph_nodes_queue;
#define cgraph_nodes_queue ((struct cgraph_node *)x_cgraph_nodes_queue)

/* Number of nodes in existence.  */
int cgraph_n_nodes;

/* Maximal uid used in cgraph nodes.  */
int cgraph_max_uid;

/* Maximal uid used in cgraph edges.  */
int cgraph_edge_max_uid;

/* Set when whole unit has been analyzed so we can access global info.  */
bool cgraph_global_info_ready = false;

/* What state callgraph is in right now.  */
enum cgraph_state cgraph_state = CGRAPH_STATE_PARSING;

/* Set when the cgraph is fully build and the basic flags are computed.  */
bool cgraph_function_flags_ready = false;

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
bool cpp_implicit_aliases_done;

/* Map a cgraph_node to cgraph_function_version_info using this htab.
   The cgraph_function_version_info has a THIS_NODE field that is the
   corresponding cgraph_node..  */

static htab_t GTY((param_is (struct cgraph_function_version_info *)))
  cgraph_fnver_htab = NULL;

/* Hash function for cgraph_fnver_htab.  */
static hashval_t
cgraph_fnver_htab_hash (const void *ptr)
{
  int uid = ((const struct cgraph_function_version_info *)ptr)->this_node->uid;
  return (hashval_t)(uid);
}

/* eq function for cgraph_fnver_htab.  */
static int
cgraph_fnver_htab_eq (const void *p1, const void *p2)
{
  const struct cgraph_function_version_info *n1
    = (const struct cgraph_function_version_info *)p1;
  const struct cgraph_function_version_info *n2
    = (const struct cgraph_function_version_info *)p2;

  return n1->this_node->uid == n2->this_node->uid;
}

/* Mark as GC root all allocated nodes.  */
static GTY(()) struct cgraph_function_version_info *
  version_info_node = NULL;

/* Get the cgraph_function_version_info node corresponding to node.  */
struct cgraph_function_version_info *
get_cgraph_node_version (struct cgraph_node *node)
{
  struct cgraph_function_version_info *ret;
  struct cgraph_function_version_info key;
  key.this_node = node;

  if (cgraph_fnver_htab == NULL)
    return NULL;

  ret = (struct cgraph_function_version_info *)
    htab_find (cgraph_fnver_htab, &key);

  return ret;
}

/* Insert a new cgraph_function_version_info node into cgraph_fnver_htab
   corresponding to cgraph_node NODE.  */
struct cgraph_function_version_info *
insert_new_cgraph_node_version (struct cgraph_node *node)
{
  void **slot;
  
  version_info_node = NULL;
  version_info_node = ggc_alloc_cleared_cgraph_function_version_info ();
  version_info_node->this_node = node;

  if (cgraph_fnver_htab == NULL)
    cgraph_fnver_htab = htab_create_ggc (2, cgraph_fnver_htab_hash,
				         cgraph_fnver_htab_eq, NULL);

  slot = htab_find_slot (cgraph_fnver_htab, version_info_node, INSERT);
  gcc_assert (slot != NULL);
  *slot = version_info_node;
  return version_info_node;
}

/* Remove the cgraph_function_version_info and cgraph_node for DECL.  This
   DECL is a duplicate declaration.  */
void
delete_function_version (tree decl)
{
  struct cgraph_node *decl_node = cgraph_get_node (decl);
  struct cgraph_function_version_info *decl_v = NULL;

  if (decl_node == NULL)
    return;

  decl_v = get_cgraph_node_version (decl_node);

  if (decl_v == NULL)
    return;

  if (decl_v->prev != NULL)
   decl_v->prev->next = decl_v->next;

  if (decl_v->next != NULL)
    decl_v->next->prev = decl_v->prev;

  if (cgraph_fnver_htab != NULL)
    htab_remove_elt (cgraph_fnver_htab, decl_v);

  cgraph_remove_node (decl_node);
}

/* Record that DECL1 and DECL2 are semantically identical function
   versions.  */
void
record_function_versions (tree decl1, tree decl2)
{
  struct cgraph_node *decl1_node = cgraph_get_create_node (decl1);
  struct cgraph_node *decl2_node = cgraph_get_create_node (decl2);
  struct cgraph_function_version_info *decl1_v = NULL;
  struct cgraph_function_version_info *decl2_v = NULL;
  struct cgraph_function_version_info *before;
  struct cgraph_function_version_info *after;

  gcc_assert (decl1_node != NULL && decl2_node != NULL);
  decl1_v = get_cgraph_node_version (decl1_node);
  decl2_v = get_cgraph_node_version (decl2_node);

  if (decl1_v != NULL && decl2_v != NULL)
    return;

  if (decl1_v == NULL)
    decl1_v = insert_new_cgraph_node_version (decl1_node);

  if (decl2_v == NULL)
    decl2_v = insert_new_cgraph_node_version (decl2_node);

  /* Chain decl2_v and decl1_v.  All semantically identical versions
     will be chained together.  */

  before = decl1_v;
  after = decl2_v;

  while (before->next != NULL)
    before = before->next;

  while (after->prev != NULL)
    after= after->prev;

  before->next = after;
  after->prev = before;
}

/* Macros to access the next item in the list of free cgraph nodes and
   edges. */
#define NEXT_FREE_NODE(NODE) cgraph ((NODE)->symbol.next)
#define SET_NEXT_FREE_NODE(NODE,NODE2) ((NODE))->symbol.next = (symtab_node)NODE2
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
void
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

struct cgraph_node *
cgraph_create_empty_node (void)
{
  struct cgraph_node *node = cgraph_allocate_node ();

  node->symbol.type = SYMTAB_FUNCTION;
  node->frequency = NODE_FREQUENCY_NORMAL;
  node->count_materialization_scale = REG_BR_PROB_BASE;
  cgraph_n_nodes++;
  return node;
}

/* Return cgraph node assigned to DECL.  Create new one when needed.  */

struct cgraph_node *
cgraph_create_node (tree decl)
{
  struct cgraph_node *node = cgraph_create_empty_node ();
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);

  node->symbol.decl = decl;
  symtab_register_node ((symtab_node) node);

  if (DECL_CONTEXT (decl) && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL)
    {
      node->origin = cgraph_get_create_node (DECL_CONTEXT (decl));
      node->next_nested = node->origin->nested;
      node->origin->nested = node;
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
   the function body is associated with (not necessarily cgraph_node (DECL).  */

struct cgraph_node *
cgraph_create_function_alias (tree alias, tree target)
{
  struct cgraph_node *alias_node;

  gcc_assert (TREE_CODE (target) == FUNCTION_DECL
	      || TREE_CODE (target) == IDENTIFIER_NODE);
  gcc_assert (TREE_CODE (alias) == FUNCTION_DECL);
  alias_node = cgraph_get_create_node (alias);
  gcc_assert (!alias_node->symbol.definition);
  alias_node->symbol.alias_target = target;
  alias_node->symbol.definition = true;
  alias_node->symbol.alias = true;
  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (alias)) != NULL)
    alias_node->symbol.weakref = true;
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
  n->symbol.cpp_implicit_alias = true;
  if (cpp_implicit_aliases_done)
    symtab_resolve_alias ((symtab_node)n,
			  (symtab_node)cgraph_get_node (decl));
  return n;
}

/* Add thunk alias into callgraph.  The alias declaration is ALIAS and it
   aliases DECL with an adjustments made into the first parameter.
   See comments in thunk_adjust for detail on the parameters.  */

struct cgraph_node *
cgraph_add_thunk (struct cgraph_node *decl_node ATTRIBUTE_UNUSED,
		  tree alias, tree decl ATTRIBUTE_UNUSED,
		  bool this_adjusting,
		  HOST_WIDE_INT fixed_offset, HOST_WIDE_INT virtual_value,
		  tree virtual_offset,
		  tree real_alias)
{
  struct cgraph_node *node;

  node = cgraph_get_node (alias);
  if (node)
    {
      gcc_assert (node->symbol.definition);
      gcc_assert (!node->symbol.alias);
      gcc_assert (!node->thunk.thunk_p);
      cgraph_remove_node (node);
    }
  
  node = cgraph_create_node (alias);
  gcc_checking_assert (!virtual_offset
		       || tree_to_double_int (virtual_offset) ==
			     double_int::from_shwi (virtual_value));
  node->thunk.fixed_offset = fixed_offset;
  node->thunk.this_adjusting = this_adjusting;
  node->thunk.virtual_value = virtual_value;
  node->thunk.virtual_offset_p = virtual_offset != NULL;
  node->thunk.alias = real_alias;
  node->thunk.thunk_p = true;
  node->symbol.definition = true;

  return node;
}

/* Return the cgraph node that has ASMNAME for its DECL_ASSEMBLER_NAME.
   Return NULL if there's no such node.  */

struct cgraph_node *
cgraph_node_for_asm (tree asmname)
{
  /* We do not want to look at inline clones.  */
  for (symtab_node node = symtab_node_for_asm (asmname);
       node;
       node = node->symbol.next_sharing_asm_name)
    {
      cgraph_node *cn = dyn_cast <cgraph_node> (node);
      if (cn && !cn->global.inlined_to)
	return cn;
    }
  return NULL;
}

/* Returns a hash value for X (which really is a cgraph_edge).  */

static hashval_t
edge_hash (const void *x)
{
  return htab_hash_pointer (((const struct cgraph_edge *) x)->call_stmt);
}

/* Return nonzero if the call_stmt of of cgraph_edge X is stmt *Y.  */

static int
edge_eq (const void *x, const void *y)
{
  return ((const struct cgraph_edge *) x)->call_stmt == y;
}

/* Add call graph edge E to call site hash of its caller.  */

static inline void
cgraph_update_edge_in_call_site_hash (struct cgraph_edge *e)
{
  void **slot;
  slot = htab_find_slot_with_hash (e->caller->call_site_hash,
				   e->call_stmt,
				   htab_hash_pointer (e->call_stmt),
				   INSERT);
  *slot = e;
}

/* Add call graph edge E to call site hash of its caller.  */

static inline void
cgraph_add_edge_to_call_site_hash (struct cgraph_edge *e)
{
  void **slot;
  /* There are two speculative edges for every statement (one direct,
     one indirect); always hash the direct one.  */
  if (e->speculative && e->indirect_unknown_callee)
    return;
  slot = htab_find_slot_with_hash (e->caller->call_site_hash,
				   e->call_stmt,
				   htab_hash_pointer (e->call_stmt),
				   INSERT);
  if (*slot)
    {
      gcc_assert (((struct cgraph_edge *)*slot)->speculative);
      if (e->callee)
	*slot = e;
      return;
    }
  gcc_assert (!*slot || e->speculative);
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


/* Change field call_stmt of edge E to NEW_STMT.
   If UPDATE_SPECULATIVE and E is any component of speculative
   edge, then update all components.  */

void
cgraph_set_call_stmt (struct cgraph_edge *e, gimple new_stmt,
		      bool update_speculative)
{
  tree decl;

  /* Speculative edges has three component, update all of them
     when asked to.  */
  if (update_speculative && e->speculative)
    {
      struct cgraph_edge *direct, *indirect;
      struct ipa_ref *ref;

      cgraph_speculative_call_info (e, direct, indirect, ref);
      cgraph_set_call_stmt (direct, new_stmt, false);
      cgraph_set_call_stmt (indirect, new_stmt, false);
      ref->stmt = new_stmt;
      return;
    }

  /* Only direct speculative edges go to call_site_hash.  */
  if (e->caller->call_site_hash
      && (!e->speculative || !e->indirect_unknown_callee))
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
      e = cgraph_make_edge_direct (e, new_callee);
    }

  push_cfun (DECL_STRUCT_FUNCTION (e->caller->symbol.decl));
  e->can_throw_external = stmt_can_throw_external (new_stmt);
  pop_cfun ();
  if (e->caller->call_site_hash)
    cgraph_add_edge_to_call_site_hash (e);
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
#ifdef ENABLE_CHECKING
      struct cgraph_edge *e;
      gcc_checking_assert (!(e=cgraph_edge (caller, call_stmt)) || e->speculative);
#endif

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
  edge->lto_stmt_uid = 0;

  edge->count = count;
  gcc_assert (count >= 0);
  edge->frequency = freq;
  gcc_assert (freq >= 0);
  gcc_assert (freq <= CGRAPH_FREQ_MAX);

  edge->call_stmt = call_stmt;
  push_cfun (DECL_STRUCT_FUNCTION (caller->symbol.decl));
  edge->can_throw_external
    = call_stmt ? stmt_can_throw_external (call_stmt) : false;
  pop_cfun ();
  if (call_stmt
      && callee && callee->symbol.decl
      && !gimple_check_call_matching_types (call_stmt, callee->symbol.decl,
					    false))
    edge->call_stmt_cannot_inline_p = true;
  else
    edge->call_stmt_cannot_inline_p = false;

  edge->indirect_info = NULL;
  edge->indirect_inlining_edge = 0;
  edge->speculative = false;
  if (call_stmt && caller->call_site_hash)
    cgraph_add_edge_to_call_site_hash (edge);

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
  tree target;

  edge->indirect_unknown_callee = 1;
  initialize_inline_failed (edge);

  edge->indirect_info = cgraph_allocate_init_indirect_info ();
  edge->indirect_info->ecf_flags = ecf_flags;

  /* Record polymorphic call info.  */
  if (call_stmt
      && (target = gimple_call_fn (call_stmt))
      && virtual_method_call_p (target))
    {
      tree type = obj_type_ref_class (target);


      /* Only record types can have virtual calls.  */
      gcc_assert (TREE_CODE (type) == RECORD_TYPE);
      edge->indirect_info->param_index = -1;
      edge->indirect_info->otr_token
	 = tree_low_cst (OBJ_TYPE_REF_TOKEN (target), 1);
      edge->indirect_info->otr_type = type;
      edge->indirect_info->polymorphic = 1;
    }

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

  if (e->indirect_info)
    ggc_free (e->indirect_info);

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

/* Turn edge E into speculative call calling N2. Update
   the profile so the direct call is taken COUNT times
   with FREQUENCY.  

   At clone materialization time, the indirect call E will
   be expanded as:

   if (call_dest == N2)
     n2 ();
   else
     call call_dest

   At this time the function just creates the direct call,
   the referencd representing the if conditional and attaches
   them all to the orginal indirect call statement.  

   Return direct edge created.  */

struct cgraph_edge *
cgraph_turn_edge_to_speculative (struct cgraph_edge *e,
				 struct cgraph_node *n2,
				 gcov_type direct_count,
				 int direct_frequency)
{
  struct cgraph_node *n = e->caller;
  struct ipa_ref *ref;
  struct cgraph_edge *e2;

  if (dump_file)
    {
      fprintf (dump_file, "Indirect call -> speculative call"
	       " %s/%i => %s/%i\n",
	       xstrdup (cgraph_node_name (n)), n->symbol.order,
	       xstrdup (cgraph_node_name (n2)), n2->symbol.order);
    }
  e->speculative = true;
  e2 = cgraph_create_edge (n, n2, e->call_stmt, direct_count, direct_frequency);
  initialize_inline_failed (e2);
  e2->speculative = true;
  if (TREE_NOTHROW (n2->symbol.decl))
    e2->can_throw_external = false;
  else
    e2->can_throw_external = e->can_throw_external;
  e2->lto_stmt_uid = e->lto_stmt_uid;
  e->count -= e2->count;
  e->frequency -= e2->frequency;
  cgraph_call_edge_duplication_hooks (e, e2);
  ref = ipa_record_reference ((symtab_node)n, (symtab_node)n2,
			      IPA_REF_ADDR, e->call_stmt);
  ref->lto_stmt_uid = e->lto_stmt_uid;
  ref->speculative = e->speculative;
  cgraph_mark_address_taken_node (n2);
  return e2;
}

/* Speculative call consist of three components:
   1) an indirect edge representing the original call
   2) an direct edge representing the new call
   3) ADDR_EXPR reference representing the speculative check.
   All three components are attached to single statement (the indirect
   call) and if one of them exists, all of them must exist.

   Given speculative call edge E, return all three components. 
 */

void
cgraph_speculative_call_info (struct cgraph_edge *e,
			      struct cgraph_edge *&direct,
			      struct cgraph_edge *&indirect,
			      struct ipa_ref *&reference)
{
  struct ipa_ref *ref;
  int i;
  struct cgraph_edge *e2;

  if (!e->indirect_unknown_callee)
    for (e2 = e->caller->indirect_calls;
	 e2->call_stmt != e->call_stmt || e2->lto_stmt_uid != e->lto_stmt_uid;
	 e2 = e2->next_callee)
      ;
  else
    {
      e2 = e;
      /* We can take advantage of the call stmt hash.  */
      if (e2->call_stmt)
	{
	  e = cgraph_edge (e->caller, e2->call_stmt);
	  gcc_assert (e->speculative && !e->indirect_unknown_callee);
	}
      else
	for (e = e->caller->callees; 
	     e2->call_stmt != e->call_stmt
	     || e2->lto_stmt_uid != e->lto_stmt_uid;
	     e = e->next_callee)
	  ;
    }
  gcc_assert (e->speculative && e2->speculative);
  direct = e;
  indirect = e2;

  reference = NULL;
  for (i = 0; ipa_ref_list_reference_iterate (&e->caller->symbol.ref_list,
					      i, ref); i++)
    if (ref->speculative
	&& ((ref->stmt && ref->stmt == e->call_stmt)
	    || (!ref->stmt && ref->lto_stmt_uid == e->lto_stmt_uid)))
      {
	reference = ref;
	break;
      }

  /* Speculative edge always consist of all three components - direct edge,
     indirect and reference.  */
  
  gcc_assert (e && e2 && ref);
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

/* Speculative call EDGE turned out to be direct call to CALLE_DECL.
   Remove the speculative call sequence and return edge representing the call.
   It is up to caller to redirect the call as appropriate. */

struct cgraph_edge *
cgraph_resolve_speculation (struct cgraph_edge *edge, tree callee_decl)
{
  struct cgraph_edge *e2;
  struct ipa_ref *ref;

  gcc_assert (edge->speculative);
  cgraph_speculative_call_info (edge, e2, edge, ref);
  if (!callee_decl
      || !symtab_semantically_equivalent_p ((symtab_node) ref->referred,
					    symtab_get_node (callee_decl)))
    {
      if (dump_file)
	{
	  if (callee_decl)
	    {
	      fprintf (dump_file, "Speculative indirect call %s/%i => %s/%i has "
		       "turned out to have contradicting known target ",
		       xstrdup (cgraph_node_name (edge->caller)), edge->caller->symbol.order,
		       xstrdup (cgraph_node_name (e2->callee)), e2->callee->symbol.order);
	      print_generic_expr (dump_file, callee_decl, 0);
	      fprintf (dump_file, "\n");
	    }
	  else
	    {
	      fprintf (dump_file, "Removing speculative call %s/%i => %s/%i\n",
		       xstrdup (cgraph_node_name (edge->caller)), edge->caller->symbol.order,
		       xstrdup (cgraph_node_name (e2->callee)), e2->callee->symbol.order);
	    }
	}
    }
  else
    {
      struct cgraph_edge *tmp = edge;
      if (dump_file)
        fprintf (dump_file, "Speculative call turned into direct call.\n");
      edge = e2;
      e2 = tmp;
      /* FIXME:  If EDGE is inlined, we should scale up the frequencies and counts
         in the functions inlined through it.  */
    }
  edge->count += e2->count;
  edge->frequency += e2->frequency;
  if (edge->frequency > CGRAPH_FREQ_MAX)
    edge->frequency = CGRAPH_FREQ_MAX;
  edge->speculative = false;
  e2->speculative = false;
  ipa_remove_reference (ref);
  if (e2->indirect_unknown_callee || e2->inline_failed)
    cgraph_remove_edge (e2);
  else
    cgraph_remove_node_and_inline_clones (e2->callee, NULL);
  if (edge->caller->call_site_hash)
    cgraph_update_edge_in_call_site_hash (edge);
  return edge;
}

/* Make an indirect EDGE with an unknown callee an ordinary edge leading to
   CALLEE.  DELTA is an integer constant that is to be added to the this
   pointer (first parameter) to compensate for skipping a thunk adjustment.  */

struct cgraph_edge *
cgraph_make_edge_direct (struct cgraph_edge *edge, struct cgraph_node *callee)
{
  gcc_assert (edge->indirect_unknown_callee);

  /* If we are redirecting speculative call, make it non-speculative.  */
  if (edge->indirect_unknown_callee && edge->speculative)
    {
      edge = cgraph_resolve_speculation (edge, callee->symbol.decl);

      /* On successful speculation just return the pre existing direct edge.  */
      if (!edge->indirect_unknown_callee)
        return edge;
    }

  edge->indirect_unknown_callee = 0;
  ggc_free (edge->indirect_info);
  edge->indirect_info = NULL;

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

  if (edge->call_stmt)
    edge->call_stmt_cannot_inline_p
      = !gimple_check_call_matching_types (edge->call_stmt, callee->symbol.decl,
					   false);

  /* We need to re-determine the inlining status of the edge.  */
  initialize_inline_failed (edge);
  return edge;
}

/* If necessary, change the function declaration in the call statement
   associated with E so that it corresponds to the edge callee.  */

gimple
cgraph_redirect_edge_call_stmt_to_callee (struct cgraph_edge *e)
{
  tree decl = gimple_call_fndecl (e->call_stmt);
  gimple new_stmt;
  gimple_stmt_iterator gsi;
#ifdef ENABLE_CHECKING
  struct cgraph_node *node;
#endif

  if (e->speculative)
    {
      struct cgraph_edge *e2;
      gimple new_stmt;
      struct ipa_ref *ref;

      cgraph_speculative_call_info (e, e, e2, ref);
      /* If there already is an direct call (i.e. as a result of inliner's
	 substitution), forget about speculating.  */
      if (decl)
	e = cgraph_resolve_speculation (e, decl);
      /* If types do not match, speculation was likely wrong. 
         The direct edge was posisbly redirected to the clone with a different
	 signature.  We did not update the call statement yet, so compare it 
	 with the reference that still points to the proper type.  */
      else if (!gimple_check_call_matching_types (e->call_stmt,
						  ref->referred->symbol.decl,
						  true))
	{
	  if (dump_file)
	    fprintf (dump_file, "Not expanding speculative call of %s/%i -> %s/%i\n"
		     "Type mismatch.\n",
		     xstrdup (cgraph_node_name (e->caller)),
		     e->caller->symbol.order,
		     xstrdup (cgraph_node_name (e->callee)),
		     e->callee->symbol.order);
	  e = cgraph_resolve_speculation (e, NULL);
	  /* We are producing the final function body and will throw away the
	     callgraph edges really soon.  Reset the counts/frequencies to
	     keep verifier happy in the case of roundoff errors.  */
	  e->count = gimple_bb (e->call_stmt)->count;
	  e->frequency = compute_call_stmt_bb_frequency
			  (e->caller->symbol.decl, gimple_bb (e->call_stmt));
	}
      /* Expand speculation into GIMPLE code.  */
      else
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Expanding speculative call of %s/%i -> %s/%i count:"
		     HOST_WIDEST_INT_PRINT_DEC"\n",
		     xstrdup (cgraph_node_name (e->caller)),
		     e->caller->symbol.order,
		     xstrdup (cgraph_node_name (e->callee)),
		     e->callee->symbol.order,
		     (HOST_WIDEST_INT)e->count);
	  gcc_assert (e2->speculative);
	  push_cfun (DECL_STRUCT_FUNCTION (e->caller->symbol.decl));
	  new_stmt = gimple_ic (e->call_stmt, cgraph (ref->referred),
				e->count || e2->count
				?  RDIV (e->count * REG_BR_PROB_BASE,
					 e->count + e2->count)
				: e->frequency || e2->frequency
				? RDIV (e->frequency * REG_BR_PROB_BASE,
					e->frequency + e2->frequency)
				: REG_BR_PROB_BASE / 2,
				e->count, e->count + e2->count);
	  e->speculative = false;
	  cgraph_set_call_stmt_including_clones (e->caller, e->call_stmt,
						 new_stmt, false);
	  e->frequency = compute_call_stmt_bb_frequency
			   (e->caller->symbol.decl, gimple_bb (e->call_stmt));
	  e2->frequency = compute_call_stmt_bb_frequency
			   (e2->caller->symbol.decl, gimple_bb (e2->call_stmt));
	  e2->speculative = false;
	  ref->speculative = false;
	  ref->stmt = NULL;
	  /* Indirect edges are not both in the call site hash.
	     get it updated.  */
	  if (e->caller->call_site_hash)
	    cgraph_update_edge_in_call_site_hash (e2);
	  pop_cfun ();
	  /* Continue redirecting E to proper target.  */
	}
    }

  if (e->indirect_unknown_callee
      || decl == e->callee->symbol.decl)
    return e->call_stmt;

#ifdef ENABLE_CHECKING
  if (decl)
    {
      node = cgraph_get_node (decl);
      gcc_assert (!node || !node->clone.combined_args_to_skip);
    }
#endif

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "updating call of %s/%i -> %s/%i: ",
	       xstrdup (cgraph_node_name (e->caller)), e->caller->symbol.order,
	       xstrdup (cgraph_node_name (e->callee)), e->callee->symbol.order);
      print_gimple_stmt (cgraph_dump_file, e->call_stmt, 0, dump_flags);
      if (e->callee->clone.combined_args_to_skip)
	{
	  fprintf (cgraph_dump_file, " combined args to skip: ");
	  dump_bitmap (cgraph_dump_file,
		       e->callee->clone.combined_args_to_skip);
	}
    }

  if (e->callee->clone.combined_args_to_skip)
    {
      int lp_nr;

      new_stmt
	= gimple_call_copy_skip_args (e->call_stmt,
				      e->callee->clone.combined_args_to_skip);
      gimple_call_set_fndecl (new_stmt, e->callee->symbol.decl);
      gimple_call_set_fntype (new_stmt, gimple_call_fntype (e->call_stmt));

      if (gimple_vdef (new_stmt)
	  && TREE_CODE (gimple_vdef (new_stmt)) == SSA_NAME)
	SSA_NAME_DEF_STMT (gimple_vdef (new_stmt)) = new_stmt;

      gsi = gsi_for_stmt (e->call_stmt);
      gsi_replace (&gsi, new_stmt, false);
      /* We need to defer cleaning EH info on the new statement to
         fixup-cfg.  We may not have dominator information at this point
	 and thus would end up with unreachable blocks and have no way
	 to communicate that we need to run CFG cleanup then.  */
      lp_nr = lookup_stmt_eh_lp (e->call_stmt);
      if (lp_nr != 0)
	{
	  remove_stmt_from_eh_lp (e->call_stmt);
	  add_stmt_to_eh_lp (new_stmt, lp_nr);
	}
    }
  else
    {
      new_stmt = e->call_stmt;
      gimple_call_set_fndecl (new_stmt, e->callee->symbol.decl);
      update_stmt (new_stmt);
    }

  cgraph_set_call_stmt_including_clones (e->caller, e->call_stmt, new_stmt, false);

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "  updated to:");
      print_gimple_stmt (cgraph_dump_file, e->call_stmt, 0, dump_flags);
    }
  return new_stmt;
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
		  if (callee->symbol.decl == new_call
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

/* Helper function for cgraph_release_function_body and free_lang_data.
   It releases body from function DECL without having to inspect its
   possibly non-existent symtab node.  */

void
release_function_body (tree decl)
{
  if (DECL_STRUCT_FUNCTION (decl))
    {
      push_cfun (DECL_STRUCT_FUNCTION (decl));
      if (cfun->cfg
	  && current_loops)
	{
	  cfun->curr_properties &= ~PROP_loops;
	  loop_optimizer_finalize ();
	}
      if (cfun->gimple_df)
	{
	  delete_tree_ssa ();
	  delete_tree_cfg_annotations ();
	  cfun->eh = NULL;
	}
      if (cfun->cfg)
	{
	  gcc_assert (dom_computed[0] == DOM_NONE);
	  gcc_assert (dom_computed[1] == DOM_NONE);
	  clear_edges ();
	  cfun->cfg = NULL;
	}
      if (cfun->value_histograms)
	free_histograms ();
      pop_cfun ();
      gimple_set_body (decl, NULL);
      /* Struct function hangs a lot of data that would leak if we didn't
         removed all pointers to it.   */
      ggc_free (DECL_STRUCT_FUNCTION (decl));
      DECL_STRUCT_FUNCTION (decl) = NULL;
    }
  DECL_SAVED_TREE (decl) = NULL;
}

/* Release memory used to represent body of function NODE.
   Use this only for functions that are released before being translated to
   target code (i.e. RTL).  Functions that are compiled to RTL and beyond
   are free'd in final.c via free_after_compilation().  */

void
cgraph_release_function_body (struct cgraph_node *node)
{
  node->ipa_transforms_to_apply.release ();
  if (!node->used_as_abstract_origin && cgraph_state != CGRAPH_STATE_PARSING)
    {
      DECL_RESULT (node->symbol.decl) = NULL;
      DECL_ARGUMENTS (node->symbol.decl) = NULL;
    }
  /* If the node is abstract and needed, then do not clear DECL_INITIAL
     of its associated function function declaration because it's
     needed to emit debug info later.  */
  if (!node->used_as_abstract_origin && DECL_INITIAL (node->symbol.decl))
    DECL_INITIAL (node->symbol.decl) = error_mark_node;
  release_function_body (node->symbol.decl);
  if (node->symbol.lto_file_data)
    lto_free_function_in_decl_state_for_node ((symtab_node) node);
}

/* Remove the node from cgraph.  */

void
cgraph_remove_node (struct cgraph_node *node)
{
  struct cgraph_node *n;
  int uid = node->uid;

  cgraph_call_node_removal_hooks (node);
  cgraph_node_remove_callers (node);
  cgraph_node_remove_callees (node);
  node->ipa_transforms_to_apply.release ();

  /* Incremental inlining access removed nodes stored in the postorder list.
     */
  node->symbol.force_output = false;
  node->symbol.forced_by_abi = false;
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
  symtab_unregister_node ((symtab_node)node);
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
	  /* We are removing node with clones.  This makes clones inconsistent,
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

  /* While all the clones are removed after being proceeded, the function
     itself is kept in the cgraph even after it is compiled.  Check whether
     we are done with this body and reclaim it proactively if this is the case.
     */
  if (cgraph_state != CGRAPH_LTO_STREAMING)
    {
      n = cgraph_get_node (node->symbol.decl);
      if (!n
	  || (!n->clones && !n->clone_of && !n->global.inlined_to
	      && (cgraph_global_info_ready
		  && (TREE_ASM_WRITTEN (n->symbol.decl)
		      || DECL_EXTERNAL (n->symbol.decl)
		      || !n->symbol.analyzed
		      || (!flag_wpa && n->symbol.in_other_partition)))))
	cgraph_release_function_body (node);
    }

  node->symbol.decl = NULL;
  if (node->call_site_hash)
    {
      htab_delete (node->call_site_hash);
      node->call_site_hash = NULL;
    }
  cgraph_n_nodes--;

  /* Clear out the node to NULL all pointers and add the node to the free
     list.  */
  memset (node, 0, sizeof (*node));
  node->symbol.type = SYMTAB_FUNCTION;
  node->uid = uid;
  SET_NEXT_FREE_NODE (node, free_nodes);
  free_nodes = node;
}

/* Likewise indicate that a node is having address taken.  */

void
cgraph_mark_address_taken_node (struct cgraph_node *node)
{
  /* Indirect inlining can figure out that all uses of the address are
     inlined.  */
  if (node->global.inlined_to)
    {
      gcc_assert (cfun->after_inlining);
      gcc_assert (node->callers->indirect_inlining_edge);
      return;
    }
  /* FIXME: address_taken flag is used both as a shortcut for testing whether
     IPA_REF_ADDR reference exists (and thus it should be set on node
     representing alias we take address of) and as a test whether address
     of the object was taken (and thus it should be set on node alias is
     referring to).  We should remove the first use and the remove the
     following set.  */
  node->symbol.address_taken = 1;
  node = cgraph_function_or_thunk_node (node, NULL);
  node->symbol.address_taken = 1;
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
	  && !TREE_ASM_WRITTEN (node->symbol.decl)))
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

/* Names used to print out the availability enum.  */
const char * const cgraph_availability_names[] =
  {"unset", "not_available", "overwritable", "available", "local"};


/* Dump call graph node NODE to file F.  */

void
dump_cgraph_node (FILE *f, struct cgraph_node *node)
{
  struct cgraph_edge *edge;
  int indirect_calls_count = 0;

  dump_symtab_base (f, (symtab_node) node);

  if (node->global.inlined_to)
    fprintf (f, "  Function %s/%i is inline copy in %s/%i\n",
	     xstrdup (cgraph_node_name (node)),
	     node->symbol.order,
	     xstrdup (cgraph_node_name (node->global.inlined_to)),
	     node->global.inlined_to->symbol.order);
  if (node->clone_of)
    fprintf (f, "  Clone of %s/%i\n",
	     cgraph_node_asm_name (node->clone_of),
	     node->clone_of->symbol.order);
  if (cgraph_function_flags_ready)
    fprintf (f, "  Availability: %s\n",
	     cgraph_availability_names [cgraph_function_body_availability (node)]);

  if (node->profile_id)
    fprintf (f, "  Profile id: %i\n",
	     node->profile_id);
  fprintf (f, "  Function flags:");
  if (node->count)
    fprintf (f, " executed "HOST_WIDEST_INT_PRINT_DEC"x",
	     (HOST_WIDEST_INT)node->count);
  if (node->origin)
    fprintf (f, " nested in: %s", cgraph_node_asm_name (node->origin));
  if (gimple_has_body_p (node->symbol.decl))
    fprintf (f, " body");
  if (node->process)
    fprintf (f, " process");
  if (node->local.local)
    fprintf (f, " local");
  if (node->local.redefined_extern_inline)
    fprintf (f, " redefined_extern_inline");
  if (node->only_called_at_startup)
    fprintf (f, " only_called_at_startup");
  if (node->only_called_at_exit)
    fprintf (f, " only_called_at_exit");
  if (node->tm_clone)
    fprintf (f, " tm_clone");

  fprintf (f, "\n");

  if (node->thunk.thunk_p)
    {
      fprintf (f, "  Thunk");
      if (node->thunk.alias)
        fprintf (f, "  of %s (asm: %s)",
	         lang_hooks.decl_printable_name (node->thunk.alias, 2),
	         IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->thunk.alias)));
      fprintf (f, " fixed offset %i virtual value %i has "
	       "virtual offset %i)\n",
	       (int)node->thunk.fixed_offset,
	       (int)node->thunk.virtual_value,
	       (int)node->thunk.virtual_offset_p);
    }
  if (node->symbol.alias && node->thunk.alias
      && DECL_P (node->thunk.alias))
    {
      fprintf (f, "  Alias of %s",
	       lang_hooks.decl_printable_name (node->thunk.alias, 2));
      if (DECL_ASSEMBLER_NAME_SET_P (node->thunk.alias))
        fprintf (f, " (asm: %s)",
		 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->thunk.alias)));
      fprintf (f, "\n");
    }
  
  fprintf (f, "  Called by: ");

  for (edge = node->callers; edge; edge = edge->next_caller)
    {
      fprintf (f, "%s/%i ", cgraph_node_asm_name (edge->caller),
	       edge->caller->symbol.order);
      if (edge->count)
	fprintf (f, "("HOST_WIDEST_INT_PRINT_DEC"x) ",
		 (HOST_WIDEST_INT)edge->count);
      if (edge->frequency)
	fprintf (f, "(%.2f per call) ",
		 edge->frequency / (double)CGRAPH_FREQ_BASE);
      if (edge->speculative)
	fprintf (f, "(speculative) ");
      if (!edge->inline_failed)
	fprintf (f, "(inlined) ");
      if (edge->indirect_inlining_edge)
	fprintf (f, "(indirect_inlining) ");
      if (edge->can_throw_external)
	fprintf (f, "(can throw external) ");
    }

  fprintf (f, "\n  Calls: ");
  for (edge = node->callees; edge; edge = edge->next_callee)
    {
      fprintf (f, "%s/%i ", cgraph_node_asm_name (edge->callee),
	       edge->callee->symbol.order);
      if (edge->speculative)
	fprintf (f, "(speculative) ");
      if (!edge->inline_failed)
	fprintf (f, "(inlined) ");
      if (edge->indirect_inlining_edge)
	fprintf (f, "(indirect_inlining) ");
      if (edge->count)
	fprintf (f, "("HOST_WIDEST_INT_PRINT_DEC"x) ",
		 (HOST_WIDEST_INT)edge->count);
      if (edge->frequency)
	fprintf (f, "(%.2f per call) ",
		 edge->frequency / (double)CGRAPH_FREQ_BASE);
      if (edge->can_throw_external)
	fprintf (f, "(can throw external) ");
    }
  fprintf (f, "\n");

  for (edge = node->indirect_calls; edge; edge = edge->next_callee)
    indirect_calls_count++;
  if (indirect_calls_count)
    fprintf (f, "  Has %i outgoing edges for indirect calls.\n",
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
  FOR_EACH_FUNCTION (node)
    dump_cgraph_node (f, node);
}


/* Dump the call graph to stderr.  */

DEBUG_FUNCTION void
debug_cgraph (void)
{
  dump_cgraph (stderr);
}

/* Return true when the DECL can possibly be inlined.  */
bool
cgraph_function_possibly_inlined_p (tree decl)
{
  if (!cgraph_global_info_ready)
    return !DECL_UNINLINABLE (decl);
  return DECL_POSSIBLY_INLINED (decl);
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
  if (!node->symbol.analyzed)
    avail = AVAIL_NOT_AVAILABLE;
  else if (node->local.local)
    avail = AVAIL_LOCAL;
  else if (node->symbol.alias && node->symbol.weakref)
    cgraph_function_or_thunk_node (node, &avail);
  else if (lookup_attribute ("ifunc", DECL_ATTRIBUTES (node->symbol.decl)))
    avail = AVAIL_OVERWRITABLE;
  else if (!node->symbol.externally_visible)
    avail = AVAIL_AVAILABLE;
  /* Inline functions are safe to be analyzed even if their symbol can
     be overwritten at runtime.  It is not meaningful to enforce any sane
     behaviour on replacing inline function by different body.  */
  else if (DECL_DECLARED_INLINE_P (node->symbol.decl))
    avail = AVAIL_AVAILABLE;

  /* If the function can be overwritten, return OVERWRITABLE.  Take
     care at least of two notable extensions - the COMDAT functions
     used to share template instantiations in C++ (this is symmetric
     to code cp_cannot_inline_tree_fn and probably shall be shared and
     the inlinability hooks completely eliminated).

     ??? Does the C++ one definition rule allow us to always return
     AVAIL_AVAILABLE here?  That would be good reason to preserve this
     bit.  */

  else if (decl_replaceable_p (node->symbol.decl)
	   && !DECL_EXTERNAL (node->symbol.decl))
    avail = AVAIL_OVERWRITABLE;
  else avail = AVAIL_AVAILABLE;

  return avail;
}

/* Worker for cgraph_node_can_be_local_p.  */
static bool
cgraph_node_cannot_be_local_p_1 (struct cgraph_node *node,
				 void *data ATTRIBUTE_UNUSED)
{
  return !(!node->symbol.force_output
	   && ((DECL_COMDAT (node->symbol.decl)
		&& !node->symbol.forced_by_abi
	        && !symtab_used_from_object_file_p ((symtab_node) node)
		&& !node->symbol.same_comdat_group)
	       || !node->symbol.externally_visible));
}

/* Return true if NODE can be made local for API change.
   Extern inline functions and C++ COMDAT functions can be made local
   at the expense of possible code size growth if function is used in multiple
   compilation units.  */
bool
cgraph_node_can_be_local_p (struct cgraph_node *node)
{
  return (!node->symbol.address_taken
	  && !cgraph_for_node_and_aliases (node,
					   cgraph_node_cannot_be_local_p_1,
					   NULL, true));
}

/* Call calback on NODE, thunks and aliases associated to NODE. 
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
  for (i = 0; ipa_ref_list_referring_iterate (&node->symbol.ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      {
	struct cgraph_node *alias = ipa_ref_referring_node (ref);
	if (include_overwritable
	    || cgraph_function_body_availability (alias) > AVAIL_OVERWRITABLE)
	  if (cgraph_for_node_thunks_and_aliases (alias, callback, data,
						  include_overwritable))
	    return true;
      }
  return false;
}

/* Call calback on NODE and aliases associated to NODE. 
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
  for (i = 0; ipa_ref_list_referring_iterate (&node->symbol.ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      {
	struct cgraph_node *alias = ipa_ref_referring_node (ref);
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
  if (DECL_COMDAT (node->symbol.decl) || DECL_EXTERNAL (node->symbol.decl))
    {
      symtab_make_decl_local (node->symbol.decl);

      node->symbol.externally_visible = false;
      node->symbol.forced_by_abi = false;
      node->local.local = true;
      node->symbol.unique_name = (node->symbol.resolution == LDPR_PREVAILING_DEF_IRONLY
				  || node->symbol.resolution == LDPR_PREVAILING_DEF_IRONLY_EXP);
      node->symbol.resolution = LDPR_PREVAILING_DEF_IRONLY;
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

  TREE_NOTHROW (node->symbol.decl) = data != NULL;

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
      if (DECL_STATIC_CONSTRUCTOR (node->symbol.decl))
	DECL_STATIC_CONSTRUCTOR (node->symbol.decl) = 0;
      if (DECL_STATIC_DESTRUCTOR (node->symbol.decl))
	DECL_STATIC_DESTRUCTOR (node->symbol.decl) = 0;
    }
  TREE_READONLY (node->symbol.decl) = data != NULL;
  DECL_LOOPING_CONST_OR_PURE_P (node->symbol.decl) = ((size_t)data & 2) != 0;
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
  /* Static constructors and destructors without a side effect can be
     optimized out.  */
  if (data && !((size_t)data & 2))
    {
      if (DECL_STATIC_CONSTRUCTOR (node->symbol.decl))
	DECL_STATIC_CONSTRUCTOR (node->symbol.decl) = 0;
      if (DECL_STATIC_DESTRUCTOR (node->symbol.decl))
	DECL_STATIC_DESTRUCTOR (node->symbol.decl) = 0;
    }
  DECL_PURE_P (node->symbol.decl) = data != NULL;
  DECL_LOOPING_CONST_OR_PURE_P (node->symbol.decl) = ((size_t)data & 2) != 0;
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

/* Return true when NODE can not return or throw and thus
   it is safe to ignore its side effects for IPA analysis.  */

bool
cgraph_node_cannot_return (struct cgraph_node *node)
{
  int flags = flags_from_decl_or_type (node->symbol.decl);
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
  if (DECL_EXTERNAL (node->symbol.decl))
    return true;
  /* When function is needed, we can not remove it.  */
  if (node->symbol.force_output || node->symbol.used_from_other_partition)
    return false;
  if (DECL_STATIC_CONSTRUCTOR (node->symbol.decl)
      || DECL_STATIC_DESTRUCTOR (node->symbol.decl))
    return false;
  /* Only COMDAT functions can be removed if externally visible.  */
  if (node->symbol.externally_visible
      && (!DECL_COMDAT (node->symbol.decl)
	  || node->symbol.forced_by_abi
	  || symtab_used_from_object_file_p ((symtab_node) node)))
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
  if (DECL_EXTERNAL (node->symbol.decl))
    return true;
  if (node->symbol.address_taken)
    return false;
  return !cgraph_for_node_and_aliases (node, nonremovable_p, NULL, true);
}

/* Worker for cgraph_can_remove_if_no_direct_calls_p.  */

static bool
used_from_object_file_p (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  return symtab_used_from_object_file_p ((symtab_node) node);
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
       if (DECL_EXTERNAL (node->symbol.decl))
         return true;
      return cgraph_can_remove_if_no_direct_calls_p (node);
    }
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
  vec<cgraph_edge_p> *redirect_callers = (vec<cgraph_edge_p> *)data;
  struct cgraph_edge *cs;
  enum availability avail;
  cgraph_function_or_thunk_node (node, &avail);

  if (avail > AVAIL_OVERWRITABLE)
    for (cs = node->callers; cs != NULL; cs = cs->next_caller)
      if (!cs->indirect_inlining_edge)
        redirect_callers->safe_push (cs);
  return false;
}

/* Collect all callers of NODE and its aliases that are known to lead to NODE
   (i.e. are not overwritable).  */

vec<cgraph_edge_p> 
collect_callers_of_node (struct cgraph_node *node)
{
  vec<cgraph_edge_p> redirect_callers = vNULL;
  cgraph_for_node_and_aliases (node, collect_callers_of_node_1,
			       &redirect_callers, false);
  return redirect_callers;
}

/* Return TRUE if NODE2 is equivalent to NODE or its clone.  */
static bool
clone_of_p (struct cgraph_node *node, struct cgraph_node *node2)
{
  node = cgraph_function_or_thunk_node (node, NULL);
  node2 = cgraph_function_or_thunk_node (node2, NULL);
  while (node != node2 && node2)
    node2 = node2->clone_of;
  return node2 != NULL;
}

/* Verify edge E count and frequency.  */

static bool
verify_edge_count_and_frequency (struct cgraph_edge *e)
{
  bool error_found = false;
  if (e->count < 0)
    {
      error ("caller edge count is negative");
      error_found = true;
    }
  if (e->frequency < 0)
    {
      error ("caller edge frequency is negative");
      error_found = true;
    }
  if (e->frequency > CGRAPH_FREQ_MAX)
    {
      error ("caller edge frequency is too large");
      error_found = true;
    }
  if (gimple_has_body_p (e->caller->symbol.decl)
      && !e->caller->global.inlined_to
      && !e->speculative
      /* FIXME: Inline-analysis sets frequency to 0 when edge is optimized out.
	 Remove this once edges are actually removed from the function at that time.  */
      && (e->frequency
	  || (inline_edge_summary_vec.exists ()
	      && ((inline_edge_summary_vec.length () <= (unsigned) e->uid)
	          || !inline_edge_summary (e)->predicate)))
      && (e->frequency
	  != compute_call_stmt_bb_frequency (e->caller->symbol.decl,
					     gimple_bb (e->call_stmt))))
    {
      error ("caller edge frequency %i does not match BB frequency %i",
	     e->frequency,
	     compute_call_stmt_bb_frequency (e->caller->symbol.decl,
					     gimple_bb (e->call_stmt)));
      error_found = true;
    }
  return error_found;
}

/* Switch to THIS_CFUN if needed and print STMT to stderr.  */
static void
cgraph_debug_gimple_stmt (struct function *this_cfun, gimple stmt)
{
  bool fndecl_was_null = false;
  /* debug_gimple_stmt needs correct cfun */
  if (cfun != this_cfun)
    set_cfun (this_cfun);
  /* ...and an actual current_function_decl */
  if (!current_function_decl)
    {
      current_function_decl = this_cfun->decl;
      fndecl_was_null = true;
    }
  debug_gimple_stmt (stmt);
  if (fndecl_was_null)
    current_function_decl = NULL;
}

/* Verify that call graph edge E corresponds to DECL from the associated
   statement.  Return true if the verification should fail.  */

static bool
verify_edge_corresponds_to_fndecl (struct cgraph_edge *e, tree decl)
{
  struct cgraph_node *node;

  if (!decl || e->callee->global.inlined_to)
    return false;
  if (cgraph_state == CGRAPH_LTO_STREAMING)
    return false;
  node = cgraph_get_node (decl);

  /* We do not know if a node from a different partition is an alias or what it
     aliases and therefore cannot do the former_clone_of check reliably.  */
  if (!node || node->symbol.in_other_partition || e->callee->symbol.in_other_partition)
    return false;
  node = cgraph_function_or_thunk_node (node, NULL);

  if (e->callee->former_clone_of != node->symbol.decl
      /* IPA-CP sometimes redirect edge to clone and then back to the former
	 function.  This ping-pong has to go, eventually.  */
      && (node != cgraph_function_or_thunk_node (e->callee, NULL))
      && !clone_of_p (cgraph_function_or_thunk_node (node, NULL), e->callee))
    return true;
  else
    return false;
}

/* Verify cgraph nodes of given cgraph node.  */
DEBUG_FUNCTION void
verify_cgraph_node (struct cgraph_node *node)
{
  struct cgraph_edge *e;
  struct function *this_cfun = DECL_STRUCT_FUNCTION (node->symbol.decl);
  basic_block this_block;
  gimple_stmt_iterator gsi;
  bool error_found = false;

  if (seen_error ())
    return;

  timevar_push (TV_CGRAPH_VERIFY);
  error_found |= verify_symtab_base ((symtab_node) node);
  for (e = node->callees; e; e = e->next_callee)
    if (e->aux)
      {
	error ("aux field set for edge %s->%s",
	       identifier_to_locale (cgraph_node_name (e->caller)),
	       identifier_to_locale (cgraph_node_name (e->callee)));
	error_found = true;
      }
  if (node->count < 0)
    {
      error ("execution count is negative");
      error_found = true;
    }
  if (node->global.inlined_to && node->symbol.same_comdat_group)
    {
      error ("inline clone in same comdat group list");
      error_found = true;
    }
  if (!node->symbol.definition && !node->symbol.in_other_partition && node->local.local)
    {
      error ("local symbols must be defined");
      error_found = true;
    }
  if (node->global.inlined_to && node->symbol.externally_visible)
    {
      error ("externally visible inline clone");
      error_found = true;
    }
  if (node->global.inlined_to && node->symbol.address_taken)
    {
      error ("inline clone with address taken");
      error_found = true;
    }
  if (node->global.inlined_to && node->symbol.force_output)
    {
      error ("inline clone is forced to output");
      error_found = true;
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      if (e->aux)
	{
	  error ("aux field set for indirect edge from %s",
		 identifier_to_locale (cgraph_node_name (e->caller)));
	  error_found = true;
	}
      if (!e->indirect_unknown_callee
	  || !e->indirect_info)
	{
	  error ("An indirect edge from %s is not marked as indirect or has "
		 "associated indirect_info, the corresponding statement is: ",
		 identifier_to_locale (cgraph_node_name (e->caller)));
	  cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
	  error_found = true;
	}
    }
  for (e = node->callers; e; e = e->next_caller)
    {
      if (verify_edge_count_and_frequency (e))
	error_found = true;
      if (!e->inline_failed)
	{
	  if (node->global.inlined_to
	      != (e->caller->global.inlined_to
		  ? e->caller->global.inlined_to : e->caller))
	    {
	      error ("inlined_to pointer is wrong");
	      error_found = true;
	    }
	  if (node->callers->next_caller)
	    {
	      error ("multiple inline callers");
	      error_found = true;
	    }
	}
      else
	if (node->global.inlined_to)
	  {
	    error ("inlined_to pointer set for noninline callers");
	    error_found = true;
	  }
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    if (verify_edge_count_and_frequency (e))
      error_found = true;
  if (!node->callers && node->global.inlined_to)
    {
      error ("inlined_to pointer is set but no predecessors found");
      error_found = true;
    }
  if (node->global.inlined_to == node)
    {
      error ("inlined_to pointer refers to itself");
      error_found = true;
    }

  if (node->clone_of)
    {
      struct cgraph_node *n;
      for (n = node->clone_of->clones; n; n = n->next_sibling_clone)
        if (n == node)
	  break;
      if (!n)
	{
	  error ("node has wrong clone_of");
	  error_found = true;
	}
    }
  if (node->clones)
    {
      struct cgraph_node *n;
      for (n = node->clones; n; n = n->next_sibling_clone)
        if (n->clone_of != node)
	  break;
      if (n)
	{
	  error ("node has wrong clone list");
	  error_found = true;
	}
    }
  if ((node->prev_sibling_clone || node->next_sibling_clone) && !node->clone_of)
    {
       error ("node is in clone list but it is not clone");
       error_found = true;
    }
  if (!node->prev_sibling_clone && node->clone_of && node->clone_of->clones != node)
    {
      error ("node has wrong prev_clone pointer");
      error_found = true;
    }
  if (node->prev_sibling_clone && node->prev_sibling_clone->next_sibling_clone != node)
    {
      error ("double linked list of clones corrupted");
      error_found = true;
    }

  if (node->symbol.analyzed && node->symbol.alias)
    {
      bool ref_found = false;
      int i;
      struct ipa_ref *ref;

      if (node->callees)
	{
	  error ("Alias has call edges");
          error_found = true;
	}
      for (i = 0; ipa_ref_list_reference_iterate (&node->symbol.ref_list,
						  i, ref); i++)
	if (ref->use != IPA_REF_ALIAS)
	  {
	    error ("Alias has non-alias reference");
	    error_found = true;
	  }
	else if (ref_found)
	  {
	    error ("Alias has more than one alias reference");
	    error_found = true;
	  }
	else
	  ref_found = true;
	if (!ref_found)
	  {
	    error ("Analyzed alias has no reference");
	    error_found = true;
	  }
    }
  if (node->symbol.analyzed && node->thunk.thunk_p)
    {
      if (!node->callees)
	{
	  error ("No edge out of thunk node");
          error_found = true;
	}
      else if (node->callees->next_callee)
	{
	  error ("More than one edge out of thunk node");
          error_found = true;
	}
      if (gimple_has_body_p (node->symbol.decl))
        {
	  error ("Thunk is not supposed to have body");
          error_found = true;
        }
    }
  else if (node->symbol.analyzed && gimple_has_body_p (node->symbol.decl)
           && !TREE_ASM_WRITTEN (node->symbol.decl)
           && (!DECL_EXTERNAL (node->symbol.decl) || node->global.inlined_to)
           && !flag_wpa)
    {
      if (this_cfun->cfg)
	{
	  pointer_set_t *stmts = pointer_set_create ();
	  int i;
	  struct ipa_ref *ref;

	  /* Reach the trees by walking over the CFG, and note the
	     enclosing basic-blocks in the call edges.  */
	  FOR_EACH_BB_FN (this_block, this_cfun)
	    {
	      for (gsi = gsi_start_phis (this_block);
		   !gsi_end_p (gsi); gsi_next (&gsi))
		pointer_set_insert (stmts, gsi_stmt (gsi));
	      for (gsi = gsi_start_bb (this_block);
		   !gsi_end_p (gsi);
		   gsi_next (&gsi))
		{
		  gimple stmt = gsi_stmt (gsi);
		  pointer_set_insert (stmts, stmt);
		  if (is_gimple_call (stmt))
		    {
		      struct cgraph_edge *e = cgraph_edge (node, stmt);
		      tree decl = gimple_call_fndecl (stmt);
		      if (e)
			{
			  if (e->aux)
			    {
			      error ("shared call_stmt:");
			      cgraph_debug_gimple_stmt (this_cfun, stmt);
			      error_found = true;
			    }
			  if (!e->indirect_unknown_callee)
			    {
			      if (verify_edge_corresponds_to_fndecl (e, decl))
				{
				  error ("edge points to wrong declaration:");
				  debug_tree (e->callee->symbol.decl);
				  fprintf (stderr," Instead of:");
				  debug_tree (decl);
				  error_found = true;
				}
			    }
			  else if (decl)
			    {
			      error ("an indirect edge with unknown callee "
				     "corresponding to a call_stmt with "
				     "a known declaration:");
			      error_found = true;
			      cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
			    }
			  e->aux = (void *)1;
			}
		      else if (decl)
			{
			  error ("missing callgraph edge for call stmt:");
			  cgraph_debug_gimple_stmt (this_cfun, stmt);
			  error_found = true;
			}
		    }
		}
	      }
	    for (i = 0;
		 ipa_ref_list_reference_iterate (&node->symbol.ref_list, i, ref);
		 i++)
	      if (ref->stmt && !pointer_set_contains (stmts, ref->stmt))
		{
		  error ("reference to dead statement");
		  cgraph_debug_gimple_stmt (this_cfun, ref->stmt);
		  error_found = true;
		}
	    pointer_set_destroy (stmts);
	}
      else
	/* No CFG available?!  */
	gcc_unreachable ();

      for (e = node->callees; e; e = e->next_callee)
	{
	  if (!e->aux)
	    {
	      error ("edge %s->%s has no corresponding call_stmt",
		     identifier_to_locale (cgraph_node_name (e->caller)),
		     identifier_to_locale (cgraph_node_name (e->callee)));
	      cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
	      error_found = true;
	    }
	  e->aux = 0;
	}
      for (e = node->indirect_calls; e; e = e->next_callee)
	{
	  if (!e->aux && !e->speculative)
	    {
	      error ("an indirect edge from %s has no corresponding call_stmt",
		     identifier_to_locale (cgraph_node_name (e->caller)));
	      cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
	      error_found = true;
	    }
	  e->aux = 0;
	}
    }
  if (error_found)
    {
      dump_cgraph_node (stderr, node);
      internal_error ("verify_cgraph_node failed");
    }
  timevar_pop (TV_CGRAPH_VERIFY);
}

/* Verify whole cgraph structure.  */
DEBUG_FUNCTION void
verify_cgraph (void)
{
  struct cgraph_node *node;

  if (seen_error ())
    return;

  FOR_EACH_FUNCTION (node)
    verify_cgraph_node (node);
}

/* Create external decl node for DECL.
   The difference i nbetween cgraph_get_create_node and
   cgraph_get_create_real_symbol_node is that cgraph_get_create_node
   may return inline clone, while cgraph_get_create_real_symbol_node
   will create a new node in this case.
   FIXME: This function should be removed once clones are put out of decl
   hash.  */

struct cgraph_node *
cgraph_get_create_real_symbol_node (tree decl)
{
  struct cgraph_node *first_clone = cgraph_get_node (decl);
  struct cgraph_node *node;
  /* create symbol table node.  even if inline clone exists, we can not take
     it as a target of non-inlined call.  */
  node = cgraph_get_node (decl);
  if (node && !node->global.inlined_to)
    return node;

  node = cgraph_create_node (decl);

  /* ok, we previously inlined the function, then removed the offline copy and
     now we want it back for external call.  this can happen when devirtualizing
     while inlining function called once that happens after extern inlined and
     virtuals are already removed.  in this case introduce the external node
     and make it available for call.  */
  if (first_clone)
    {
      first_clone->clone_of = node;
      node->clones = first_clone;
      symtab_prevail_in_asm_name_hash ((symtab_node) node);
      symtab_insert_node_to_hashtable ((symtab_node) node);
      if (dump_file)
	fprintf (dump_file, "Introduced new external node "
		 "(%s/%i) and turned into root of the clone tree.\n",
		 xstrdup (cgraph_node_name (node)), node->symbol.order);
    }
  else if (dump_file)
    fprintf (dump_file, "Introduced new external node "
	     "(%s/%i).\n", xstrdup (cgraph_node_name (node)),
	     node->symbol.order);
  return node;
}


/* Given NODE, walk the alias chain to return the function NODE is alias of.
   Walk through thunk, too.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.  */

struct cgraph_node *
cgraph_function_node (struct cgraph_node *node, enum availability *availability)
{
  do
    {
      node = cgraph_function_or_thunk_node (node, availability);
      if (node->thunk.thunk_p)
	{
	  node = node->callees->callee;
	  if (availability)
	    {
	      enum availability a;
	      a = cgraph_function_body_availability (node);
	      if (a < *availability)
		*availability = a;
	    }
	  node = cgraph_function_or_thunk_node (node, availability);
	}
    } while (node && node->thunk.thunk_p);
  return node;
}

/* When doing LTO, read NODE's body from disk if it is not already present.  */

bool
cgraph_get_body (struct cgraph_node *node)
{
  struct lto_file_decl_data *file_data;
  const char *data, *name;
  size_t len;
  tree decl = node->symbol.decl;

  if (DECL_RESULT (decl))
    return false;

  gcc_assert (in_lto_p);

  file_data = node->symbol.lto_file_data;
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  /* We may have renamed the declaration, e.g., a static function.  */
  name = lto_get_decl_name_mapping (file_data, name);

  data = lto_get_section_data (file_data, LTO_section_function_body,
			       name, &len);
  if (!data)
    {
	dump_cgraph_node (stderr, node);
    fatal_error ("%s: section %s is missing",
		 file_data->file_name,
		 name);
    }

  gcc_assert (DECL_STRUCT_FUNCTION (decl) == NULL);

  lto_input_function_body (file_data, node, data);
  lto_stats.num_function_bodies++;
  lto_free_section_data (file_data, LTO_section_function_body, name,
			 data, len);
  lto_free_function_in_decl_state_for_node ((symtab_node) node);
  return true;
}

#include "gt-cgraph.h"
