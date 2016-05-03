/* Callgraph handling code.
   Copyright (C) 2003-2016 Free Software Foundation, Inc.
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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "alloc-pool.h"
#include "gimple-ssa.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "fold-const.h"
#include "varasm.h"
#include "calls.h"
#include "print-tree.h"
#include "langhooks.h"
#include "intl.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa.h"
#include "value-prof.h"
#include "ipa-utils.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-inline.h"
#include "cfgloop.h"
#include "gimple-pretty-print.h"
#include "tree-dfa.h"
#include "profile.h"
#include "params.h"
#include "tree-chkp.h"
#include "context.h"
#include "gimplify.h"

/* FIXME: Only for PROP_loops, but cgraph shouldn't have to know about this.  */
#include "tree-pass.h"

/* Queue of cgraph nodes scheduled to be lowered.  */
symtab_node *x_cgraph_nodes_queue;
#define cgraph_nodes_queue ((cgraph_node *)x_cgraph_nodes_queue)

/* Symbol table global context.  */
symbol_table *symtab;

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

/* Hash descriptor for cgraph_function_version_info.  */

struct function_version_hasher : ggc_ptr_hash<cgraph_function_version_info>
{
  static hashval_t hash (cgraph_function_version_info *);
  static bool equal (cgraph_function_version_info *,
		     cgraph_function_version_info *);
};

/* Map a cgraph_node to cgraph_function_version_info using this htab.
   The cgraph_function_version_info has a THIS_NODE field that is the
   corresponding cgraph_node..  */

static GTY(()) hash_table<function_version_hasher> *cgraph_fnver_htab = NULL;

/* Hash function for cgraph_fnver_htab.  */
hashval_t
function_version_hasher::hash (cgraph_function_version_info *ptr)
{
  int uid = ptr->this_node->uid;
  return (hashval_t)(uid);
}

/* eq function for cgraph_fnver_htab.  */
bool
function_version_hasher::equal (cgraph_function_version_info *n1,
			       	cgraph_function_version_info *n2)
{
  return n1->this_node->uid == n2->this_node->uid;
}

/* Mark as GC root all allocated nodes.  */
static GTY(()) struct cgraph_function_version_info *
  version_info_node = NULL;

/* Return true if NODE's address can be compared.  */

bool
symtab_node::address_can_be_compared_p ()
{
  /* Address of virtual tables and functions is never compared.  */
  if (DECL_VIRTUAL_P (decl))
    return false;
  /* Address of C++ cdtors is never compared.  */
  if (is_a <cgraph_node *> (this)
      && (DECL_CXX_CONSTRUCTOR_P (decl)
	  || DECL_CXX_DESTRUCTOR_P (decl)))
    return false;
  /* Constant pool symbols addresses are never compared.
     flag_merge_constants permits us to assume the same on readonly vars.  */
  if (is_a <varpool_node *> (this)
      && (DECL_IN_CONSTANT_POOL (decl)
	  || (flag_merge_constants >= 2
	      && TREE_READONLY (decl) && !TREE_THIS_VOLATILE (decl))))
    return false;
  return true;
}

/* Get the cgraph_function_version_info node corresponding to node.  */
cgraph_function_version_info *
cgraph_node::function_version (void)
{
  cgraph_function_version_info key;
  key.this_node = this;

  if (cgraph_fnver_htab == NULL)
    return NULL;

  return cgraph_fnver_htab->find (&key);
}

/* Insert a new cgraph_function_version_info node into cgraph_fnver_htab
   corresponding to cgraph_node NODE.  */
cgraph_function_version_info *
cgraph_node::insert_new_function_version (void)
{
  version_info_node = NULL;
  version_info_node = ggc_cleared_alloc<cgraph_function_version_info> ();
  version_info_node->this_node = this;

  if (cgraph_fnver_htab == NULL)
    cgraph_fnver_htab = hash_table<function_version_hasher>::create_ggc (2);

  *cgraph_fnver_htab->find_slot (version_info_node, INSERT)
    = version_info_node;
  return version_info_node;
}

/* Remove the cgraph_function_version_info and cgraph_node for DECL.  This
   DECL is a duplicate declaration.  */
void
cgraph_node::delete_function_version (tree decl)
{
  cgraph_node *decl_node = cgraph_node::get (decl);
  cgraph_function_version_info *decl_v = NULL;

  if (decl_node == NULL)
    return;

  decl_v = decl_node->function_version ();

  if (decl_v == NULL)
    return;

  if (decl_v->prev != NULL)
   decl_v->prev->next = decl_v->next;

  if (decl_v->next != NULL)
    decl_v->next->prev = decl_v->prev;

  if (cgraph_fnver_htab != NULL)
    cgraph_fnver_htab->remove_elt (decl_v);

  decl_node->remove ();
}

/* Record that DECL1 and DECL2 are semantically identical function
   versions.  */
void
cgraph_node::record_function_versions (tree decl1, tree decl2)
{
  cgraph_node *decl1_node = cgraph_node::get_create (decl1);
  cgraph_node *decl2_node = cgraph_node::get_create (decl2);
  cgraph_function_version_info *decl1_v = NULL;
  cgraph_function_version_info *decl2_v = NULL;
  cgraph_function_version_info *before;
  cgraph_function_version_info *after;

  gcc_assert (decl1_node != NULL && decl2_node != NULL);
  decl1_v = decl1_node->function_version ();
  decl2_v = decl2_node->function_version ();

  if (decl1_v != NULL && decl2_v != NULL)
    return;

  if (decl1_v == NULL)
    decl1_v = decl1_node->insert_new_function_version ();

  if (decl2_v == NULL)
    decl2_v = decl2_node->insert_new_function_version ();

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

/* Initialize callgraph dump file.  */

void
symbol_table::initialize (void)
{
  if (!dump_file)
    dump_file = dump_begin (TDI_cgraph, NULL);
}

/* Allocate new callgraph node and insert it into basic data structures.  */

cgraph_node *
symbol_table::create_empty (void)
{
  cgraph_node *node = allocate_cgraph_symbol ();

  node->type = SYMTAB_FUNCTION;
  node->frequency = NODE_FREQUENCY_NORMAL;
  node->count_materialization_scale = REG_BR_PROB_BASE;
  cgraph_count++;

  return node;
}

/* Register HOOK to be called with DATA on each removed edge.  */
cgraph_edge_hook_list *
symbol_table::add_edge_removal_hook (cgraph_edge_hook hook, void *data)
{
  cgraph_edge_hook_list *entry;
  cgraph_edge_hook_list **ptr = &m_first_edge_removal_hook;

  entry = (cgraph_edge_hook_list *) xmalloc (sizeof (*entry));
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
symbol_table::remove_edge_removal_hook (cgraph_edge_hook_list *entry)
{
  cgraph_edge_hook_list **ptr = &m_first_edge_removal_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all edge removal hooks.  */
void
symbol_table::call_edge_removal_hooks (cgraph_edge *e)
{
  cgraph_edge_hook_list *entry = m_first_edge_removal_hook;
  while (entry)
  {
    entry->hook (e, entry->data);
    entry = entry->next;
  }
}

/* Register HOOK to be called with DATA on each removed node.  */
cgraph_node_hook_list *
symbol_table::add_cgraph_removal_hook (cgraph_node_hook hook, void *data)
{
  cgraph_node_hook_list *entry;
  cgraph_node_hook_list **ptr = &m_first_cgraph_removal_hook;

  entry = (cgraph_node_hook_list *) xmalloc (sizeof (*entry));
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
symbol_table::remove_cgraph_removal_hook (cgraph_node_hook_list *entry)
{
  cgraph_node_hook_list **ptr = &m_first_cgraph_removal_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all node removal hooks.  */
void
symbol_table::call_cgraph_removal_hooks (cgraph_node *node)
{
  cgraph_node_hook_list *entry = m_first_cgraph_removal_hook;
  while (entry)
  {
    entry->hook (node, entry->data);
    entry = entry->next;
  }
}

/* Call all node removal hooks.  */
void
symbol_table::call_cgraph_insertion_hooks (cgraph_node *node)
{
  cgraph_node_hook_list *entry = m_first_cgraph_insertion_hook;
  while (entry)
  {
    entry->hook (node, entry->data);
    entry = entry->next;
  }
}


/* Register HOOK to be called with DATA on each inserted node.  */
cgraph_node_hook_list *
symbol_table::add_cgraph_insertion_hook (cgraph_node_hook hook, void *data)
{
  cgraph_node_hook_list *entry;
  cgraph_node_hook_list **ptr = &m_first_cgraph_insertion_hook;

  entry = (cgraph_node_hook_list *) xmalloc (sizeof (*entry));
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
symbol_table::remove_cgraph_insertion_hook (cgraph_node_hook_list *entry)
{
  cgraph_node_hook_list **ptr = &m_first_cgraph_insertion_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Register HOOK to be called with DATA on each duplicated edge.  */
cgraph_2edge_hook_list *
symbol_table::add_edge_duplication_hook (cgraph_2edge_hook hook, void *data)
{
  cgraph_2edge_hook_list *entry;
  cgraph_2edge_hook_list **ptr = &m_first_edge_duplicated_hook;

  entry = (cgraph_2edge_hook_list *) xmalloc (sizeof (*entry));
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
symbol_table::remove_edge_duplication_hook (cgraph_2edge_hook_list *entry)
{
  cgraph_2edge_hook_list **ptr = &m_first_edge_duplicated_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all edge duplication hooks.  */
void
symbol_table::call_edge_duplication_hooks (cgraph_edge *cs1, cgraph_edge *cs2)
{
  cgraph_2edge_hook_list *entry = m_first_edge_duplicated_hook;
  while (entry)
  {
    entry->hook (cs1, cs2, entry->data);
    entry = entry->next;
  }
}

/* Register HOOK to be called with DATA on each duplicated node.  */
cgraph_2node_hook_list *
symbol_table::add_cgraph_duplication_hook (cgraph_2node_hook hook, void *data)
{
  cgraph_2node_hook_list *entry;
  cgraph_2node_hook_list **ptr = &m_first_cgraph_duplicated_hook;

  entry = (cgraph_2node_hook_list *) xmalloc (sizeof (*entry));
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
symbol_table::remove_cgraph_duplication_hook (cgraph_2node_hook_list *entry)
{
  cgraph_2node_hook_list **ptr = &m_first_cgraph_duplicated_hook;

  while (*ptr != entry)
    ptr = &(*ptr)->next;
  *ptr = entry->next;
  free (entry);
}

/* Call all node duplication hooks.  */
void
symbol_table::call_cgraph_duplication_hooks (cgraph_node *node,
					     cgraph_node *node2)
{
  cgraph_2node_hook_list *entry = m_first_cgraph_duplicated_hook;
  while (entry)
  {
    entry->hook (node, node2, entry->data);
    entry = entry->next;
  }
}

/* Return cgraph node assigned to DECL.  Create new one when needed.  */

cgraph_node *
cgraph_node::create (tree decl)
{
  cgraph_node *node = symtab->create_empty ();
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);

  node->decl = decl;

  if ((flag_openacc || flag_openmp)
      && lookup_attribute ("omp declare target", DECL_ATTRIBUTES (decl)))
    {
      node->offloadable = 1;
      if (ENABLE_OFFLOADING)
	g->have_offload = true;
    }

  node->register_symbol ();

  if (DECL_CONTEXT (decl) && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL)
    {
      node->origin = cgraph_node::get_create (DECL_CONTEXT (decl));
      node->next_nested = node->origin->nested;
      node->origin->nested = node;
    }
  return node;
}

/* Try to find a call graph node for declaration DECL and if it does not exist
   or if it corresponds to an inline clone, create a new one.  */

cgraph_node *
cgraph_node::get_create (tree decl)
{
  cgraph_node *first_clone = cgraph_node::get (decl);

  if (first_clone && !first_clone->global.inlined_to)
    return first_clone;

  cgraph_node *node = cgraph_node::create (decl);
  if (first_clone)
    {
      first_clone->clone_of = node;
      node->clones = first_clone;
      symtab->symtab_prevail_in_asm_name_hash (node);
      node->decl->decl_with_vis.symtab_node = node;
      if (dump_file)
	fprintf (dump_file, "Introduced new external node "
		 "(%s/%i) and turned into root of the clone tree.\n",
		 node->name (), node->order);
    }
  else if (dump_file)
    fprintf (dump_file, "Introduced new external node "
	     "(%s/%i).\n", node->name (), node->order);
  return node;
}

/* Mark ALIAS as an alias to DECL.  DECL_NODE is cgraph node representing
   the function body is associated with (not necessarily cgraph_node (DECL).  */

cgraph_node *
cgraph_node::create_alias (tree alias, tree target)
{
  cgraph_node *alias_node;

  gcc_assert (TREE_CODE (target) == FUNCTION_DECL
	      || TREE_CODE (target) == IDENTIFIER_NODE);
  gcc_assert (TREE_CODE (alias) == FUNCTION_DECL);
  alias_node = cgraph_node::get_create (alias);
  gcc_assert (!alias_node->definition);
  alias_node->alias_target = target;
  alias_node->definition = true;
  alias_node->alias = true;
  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (alias)) != NULL)
    alias_node->transparent_alias = alias_node->weakref = true;
  return alias_node;
}

/* Attempt to mark ALIAS as an alias to DECL.  Return alias node if successful
   and NULL otherwise.
   Same body aliases are output whenever the body of DECL is output,
   and cgraph_node::get (ALIAS) transparently returns
   cgraph_node::get (DECL).  */

cgraph_node *
cgraph_node::create_same_body_alias (tree alias, tree decl)
{
  cgraph_node *n;
#ifndef ASM_OUTPUT_DEF
  /* If aliases aren't supported by the assembler, fail.  */
  return NULL;
#endif
  /* Langhooks can create same body aliases of symbols not defined.
     Those are useless. Drop them on the floor.  */
  if (symtab->global_info_ready)
    return NULL;

  n = cgraph_node::create_alias (alias, decl);
  n->cpp_implicit_alias = true;
  if (symtab->cpp_implicit_aliases_done)
    n->resolve_alias (cgraph_node::get (decl));
  return n;
}

/* Add thunk alias into callgraph.  The alias declaration is ALIAS and it
   aliases DECL with an adjustments made into the first parameter.
   See comments in thunk_adjust for detail on the parameters.  */

cgraph_node *
cgraph_node::create_thunk (tree alias, tree, bool this_adjusting,
			   HOST_WIDE_INT fixed_offset,
			   HOST_WIDE_INT virtual_value,
			   tree virtual_offset,
			   tree real_alias)
{
  cgraph_node *node;

  node = cgraph_node::get (alias);
  if (node)
    node->reset ();
  else
    node = cgraph_node::create (alias);
  gcc_checking_assert (!virtual_offset
		       || wi::eq_p (virtual_offset, virtual_value));
  node->thunk.fixed_offset = fixed_offset;
  node->thunk.this_adjusting = this_adjusting;
  node->thunk.virtual_value = virtual_value;
  node->thunk.virtual_offset_p = virtual_offset != NULL;
  node->thunk.alias = real_alias;
  node->thunk.thunk_p = true;
  node->definition = true;

  return node;
}

/* Return the cgraph node that has ASMNAME for its DECL_ASSEMBLER_NAME.
   Return NULL if there's no such node.  */

cgraph_node *
cgraph_node::get_for_asmname (tree asmname)
{
  /* We do not want to look at inline clones.  */
  for (symtab_node *node = symtab_node::get_for_asmname (asmname);
       node;
       node = node->next_sharing_asm_name)
    {
      cgraph_node *cn = dyn_cast <cgraph_node *> (node);
      if (cn && !cn->global.inlined_to)
	return cn;
    }
  return NULL;
}

/* Returns a hash value for X (which really is a cgraph_edge).  */

hashval_t
cgraph_edge_hasher::hash (cgraph_edge *e)
{
  /* This is a really poor hash function, but it is what htab_hash_pointer
     uses.  */
  return (hashval_t) ((intptr_t)e->call_stmt >> 3);
}

/* Returns a hash value for X (which really is a cgraph_edge).  */

hashval_t
cgraph_edge_hasher::hash (gimple *call_stmt)
{
  /* This is a really poor hash function, but it is what htab_hash_pointer
     uses.  */
  return (hashval_t) ((intptr_t)call_stmt >> 3);
}

/* Return nonzero if the call_stmt of cgraph_edge X is stmt *Y.  */

inline bool
cgraph_edge_hasher::equal (cgraph_edge *x, gimple *y)
{
  return x->call_stmt == y;
}

/* Add call graph edge E to call site hash of its caller.  */

static inline void
cgraph_update_edge_in_call_site_hash (cgraph_edge *e)
{
  gimple *call = e->call_stmt;
  *e->caller->call_site_hash->find_slot_with_hash
      (call, cgraph_edge_hasher::hash (call), INSERT) = e;
}

/* Add call graph edge E to call site hash of its caller.  */

static inline void
cgraph_add_edge_to_call_site_hash (cgraph_edge *e)
{
  /* There are two speculative edges for every statement (one direct,
     one indirect); always hash the direct one.  */
  if (e->speculative && e->indirect_unknown_callee)
    return;
  cgraph_edge **slot = e->caller->call_site_hash->find_slot_with_hash
      (e->call_stmt, cgraph_edge_hasher::hash (e->call_stmt), INSERT);
  if (*slot)
    {
      gcc_assert (((cgraph_edge *)*slot)->speculative);
      if (e->callee)
	*slot = e;
      return;
    }
  gcc_assert (!*slot || e->speculative);
  *slot = e;
}

/* Return the callgraph edge representing the GIMPLE_CALL statement
   CALL_STMT.  */

cgraph_edge *
cgraph_node::get_edge (gimple *call_stmt)
{
  cgraph_edge *e, *e2;
  int n = 0;

  if (call_site_hash)
    return call_site_hash->find_with_hash
	(call_stmt, cgraph_edge_hasher::hash (call_stmt));

  /* This loop may turn out to be performance problem.  In such case adding
     hashtables into call nodes with very many edges is probably best
     solution.  It is not good idea to add pointer into CALL_EXPR itself
     because we want to make possible having multiple cgraph nodes representing
     different clones of the same body before the body is actually cloned.  */
  for (e = callees; e; e = e->next_callee)
    {
      if (e->call_stmt == call_stmt)
	break;
      n++;
    }

  if (!e)
    for (e = indirect_calls; e; e = e->next_callee)
      {
	if (e->call_stmt == call_stmt)
	  break;
	n++;
      }

  if (n > 100)
    {
      call_site_hash = hash_table<cgraph_edge_hasher>::create_ggc (120);
      for (e2 = callees; e2; e2 = e2->next_callee)
	cgraph_add_edge_to_call_site_hash (e2);
      for (e2 = indirect_calls; e2; e2 = e2->next_callee)
	cgraph_add_edge_to_call_site_hash (e2);
    }

  return e;
}


/* Change field call_stmt of edge to NEW_STMT.
   If UPDATE_SPECULATIVE and E is any component of speculative
   edge, then update all components.  */

void
cgraph_edge::set_call_stmt (gcall *new_stmt, bool update_speculative)
{
  tree decl;

  /* Speculative edges has three component, update all of them
     when asked to.  */
  if (update_speculative && speculative)
    {
      cgraph_edge *direct, *indirect;
      ipa_ref *ref;

      speculative_call_info (direct, indirect, ref);
      direct->set_call_stmt (new_stmt, false);
      indirect->set_call_stmt (new_stmt, false);
      ref->stmt = new_stmt;
      return;
    }

  /* Only direct speculative edges go to call_site_hash.  */
  if (caller->call_site_hash
      && (!speculative || !indirect_unknown_callee))
    {
      caller->call_site_hash->remove_elt_with_hash
	(call_stmt, cgraph_edge_hasher::hash (call_stmt));
    }

  cgraph_edge *e = this;

  call_stmt = new_stmt;
  if (indirect_unknown_callee
      && (decl = gimple_call_fndecl (new_stmt)))
    {
      /* Constant propagation (and possibly also inlining?) can turn an
	 indirect call into a direct one.  */
      cgraph_node *new_callee = cgraph_node::get (decl);

      gcc_checking_assert (new_callee);
      e = make_direct (new_callee);
    }

  push_cfun (DECL_STRUCT_FUNCTION (e->caller->decl));
  e->can_throw_external = stmt_can_throw_external (new_stmt);
  pop_cfun ();
  if (e->caller->call_site_hash)
    cgraph_add_edge_to_call_site_hash (e);
}

/* Allocate a cgraph_edge structure and fill it with data according to the
   parameters of which only CALLEE can be NULL (when creating an indirect call
   edge).  */

cgraph_edge *
symbol_table::create_edge (cgraph_node *caller, cgraph_node *callee,
			   gcall *call_stmt, gcov_type count, int freq,
			   bool indir_unknown_callee)
{
  cgraph_edge *edge;

  /* LTO does not actually have access to the call_stmt since these
     have not been loaded yet.  */
  if (call_stmt)
    {
      /* This is a rather expensive check possibly triggering
	 construction of call stmt hashtable.  */
      cgraph_edge *e;
      gcc_checking_assert (!(e = caller->get_edge (call_stmt))
			   || e->speculative);

      gcc_assert (is_gimple_call (call_stmt));
    }

  if (free_edges)
    {
      edge = free_edges;
      free_edges = NEXT_FREE_EDGE (edge);
    }
  else
    {
      edge = ggc_alloc<cgraph_edge> ();
      edge->uid = edges_max_uid++;
    }

  edges_count++;

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
  push_cfun (DECL_STRUCT_FUNCTION (caller->decl));
  edge->can_throw_external
    = call_stmt ? stmt_can_throw_external (call_stmt) : false;
  pop_cfun ();
  if (call_stmt
      && callee && callee->decl
      && !gimple_check_call_matching_types (call_stmt, callee->decl,
					    false))
    {
      edge->inline_failed = CIF_MISMATCHED_ARGUMENTS;
      edge->call_stmt_cannot_inline_p = true;
    }
  else
    {
      edge->inline_failed = CIF_FUNCTION_NOT_CONSIDERED;
      edge->call_stmt_cannot_inline_p = false;
    }

  edge->indirect_info = NULL;
  edge->indirect_inlining_edge = 0;
  edge->speculative = false;
  edge->indirect_unknown_callee = indir_unknown_callee;
  if (opt_for_fn (edge->caller->decl, flag_devirtualize)
      && call_stmt && DECL_STRUCT_FUNCTION (caller->decl))
    edge->in_polymorphic_cdtor
      = decl_maybe_in_construction_p (NULL, NULL, call_stmt,
				      caller->decl);
  else
    edge->in_polymorphic_cdtor = caller->thunk.thunk_p;
  if (call_stmt && caller->call_site_hash)
    cgraph_add_edge_to_call_site_hash (edge);

  return edge;
}

/* Create edge from a given function to CALLEE in the cgraph.  */

cgraph_edge *
cgraph_node::create_edge (cgraph_node *callee,
			  gcall *call_stmt, gcov_type count, int freq)
{
  cgraph_edge *edge = symtab->create_edge (this, callee, call_stmt, count,
					   freq, false);

  initialize_inline_failed (edge);

  edge->next_caller = callee->callers;
  if (callee->callers)
    callee->callers->prev_caller = edge;
  edge->next_callee = callees;
  if (callees)
    callees->prev_callee = edge;
  callees = edge;
  callee->callers = edge;

  return edge;
}

/* Allocate cgraph_indirect_call_info and set its fields to default values. */

cgraph_indirect_call_info *
cgraph_allocate_init_indirect_info (void)
{
  cgraph_indirect_call_info *ii;

  ii = ggc_cleared_alloc<cgraph_indirect_call_info> ();
  ii->param_index = -1;
  return ii;
}

/* Create an indirect edge with a yet-undetermined callee where the call
   statement destination is a formal parameter of the caller with index
   PARAM_INDEX. */

cgraph_edge *
cgraph_node::create_indirect_edge (gcall *call_stmt, int ecf_flags,
				   gcov_type count, int freq,
				   bool compute_indirect_info)
{
  cgraph_edge *edge = symtab->create_edge (this, NULL, call_stmt,
							    count, freq, true);
  tree target;

  initialize_inline_failed (edge);

  edge->indirect_info = cgraph_allocate_init_indirect_info ();
  edge->indirect_info->ecf_flags = ecf_flags;
  edge->indirect_info->vptr_changed = true;

  /* Record polymorphic call info.  */
  if (compute_indirect_info
      && call_stmt
      && (target = gimple_call_fn (call_stmt))
      && virtual_method_call_p (target))
    {
      ipa_polymorphic_call_context context (decl, target, call_stmt);

      /* Only record types can have virtual calls.  */
      edge->indirect_info->polymorphic = true;
      edge->indirect_info->param_index = -1;
      edge->indirect_info->otr_token
	 = tree_to_uhwi (OBJ_TYPE_REF_TOKEN (target));
      edge->indirect_info->otr_type = obj_type_ref_class (target);
      gcc_assert (TREE_CODE (edge->indirect_info->otr_type) == RECORD_TYPE);
      edge->indirect_info->context = context;
    }

  edge->next_callee = indirect_calls;
  if (indirect_calls)
    indirect_calls->prev_callee = edge;
  indirect_calls = edge;

  return edge;
}

/* Remove the edge from the list of the callees of the caller.  */

void
cgraph_edge::remove_caller (void)
{
  if (prev_callee)
    prev_callee->next_callee = next_callee;
  if (next_callee)
    next_callee->prev_callee = prev_callee;
  if (!prev_callee)
    {
      if (indirect_unknown_callee)
	caller->indirect_calls = next_callee;
      else
	caller->callees = next_callee;
    }
  if (caller->call_site_hash)
    caller->call_site_hash->remove_elt_with_hash
	(call_stmt, cgraph_edge_hasher::hash (call_stmt));
}

/* Put the edge onto the free list.  */

void
symbol_table::free_edge (cgraph_edge *e)
{
  int uid = e->uid;

  if (e->indirect_info)
    ggc_free (e->indirect_info);

  /* Clear out the edge so we do not dangle pointers.  */
  memset (e, 0, sizeof (*e));
  e->uid = uid;
  NEXT_FREE_EDGE (e) = free_edges;
  free_edges = e;
  edges_count--;
}

/* Remove the edge in the cgraph.  */

void
cgraph_edge::remove (void)
{
  /* Call all edge removal hooks.  */
  symtab->call_edge_removal_hooks (this);

  if (!indirect_unknown_callee)
    /* Remove from callers list of the callee.  */
    remove_callee ();

  /* Remove from callees list of the callers.  */
  remove_caller ();

  /* Put the edge onto the free list.  */
  symtab->free_edge (this);
}

/* Turn edge into speculative call calling N2. Update
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

cgraph_edge *
cgraph_edge::make_speculative (cgraph_node *n2, gcov_type direct_count,
			       int direct_frequency)
{
  cgraph_node *n = caller;
  ipa_ref *ref = NULL;
  cgraph_edge *e2;

  if (dump_file)
    {
      fprintf (dump_file, "Indirect call -> speculative call"
	       " %s/%i => %s/%i\n",
	       xstrdup_for_dump (n->name ()), n->order,
	       xstrdup_for_dump (n2->name ()), n2->order);
    }
  speculative = true;
  e2 = n->create_edge (n2, call_stmt, direct_count, direct_frequency);
  initialize_inline_failed (e2);
  e2->speculative = true;
  if (TREE_NOTHROW (n2->decl))
    e2->can_throw_external = false;
  else
    e2->can_throw_external = can_throw_external;
  e2->lto_stmt_uid = lto_stmt_uid;
  e2->in_polymorphic_cdtor = in_polymorphic_cdtor;
  count -= e2->count;
  frequency -= e2->frequency;
  symtab->call_edge_duplication_hooks (this, e2);
  ref = n->create_reference (n2, IPA_REF_ADDR, call_stmt);
  ref->lto_stmt_uid = lto_stmt_uid;
  ref->speculative = speculative;
  n2->mark_address_taken ();
  return e2;
}

/* Speculative call consist of three components:
   1) an indirect edge representing the original call
   2) an direct edge representing the new call
   3) ADDR_EXPR reference representing the speculative check.
   All three components are attached to single statement (the indirect
   call) and if one of them exists, all of them must exist.

   Given speculative call edge, return all three components.
 */

void
cgraph_edge::speculative_call_info (cgraph_edge *&direct,
				    cgraph_edge *&indirect,
				    ipa_ref *&reference)
{
  ipa_ref *ref;
  int i;
  cgraph_edge *e2;
  cgraph_edge *e = this;

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
	  e = e->caller->get_edge (e2->call_stmt);
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
  for (i = 0; e->caller->iterate_reference (i, ref); i++)
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

/* Speculative call edge turned out to be direct call to CALLE_DECL.
   Remove the speculative call sequence and return edge representing the call.
   It is up to caller to redirect the call as appropriate. */

cgraph_edge *
cgraph_edge::resolve_speculation (tree callee_decl)
{
  cgraph_edge *edge = this;
  cgraph_edge *e2;
  ipa_ref *ref;

  gcc_assert (edge->speculative);
  edge->speculative_call_info (e2, edge, ref);
  if (!callee_decl
      || !ref->referred->semantically_equivalent_p
	   (symtab_node::get (callee_decl)))
    {
      if (dump_file)
	{
	  if (callee_decl)
	    {
	      fprintf (dump_file, "Speculative indirect call %s/%i => %s/%i has "
		       "turned out to have contradicting known target ",
		       xstrdup_for_dump (edge->caller->name ()),
		       edge->caller->order,
		       xstrdup_for_dump (e2->callee->name ()),
		       e2->callee->order);
	      print_generic_expr (dump_file, callee_decl, 0);
	      fprintf (dump_file, "\n");
	    }
	  else
	    {
	      fprintf (dump_file, "Removing speculative call %s/%i => %s/%i\n",
		       xstrdup_for_dump (edge->caller->name ()),
		       edge->caller->order,
		       xstrdup_for_dump (e2->callee->name ()),
		       e2->callee->order);
	    }
	}
    }
  else
    {
      cgraph_edge *tmp = edge;
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
  ref->remove_reference ();
  if (e2->indirect_unknown_callee || e2->inline_failed)
    e2->remove ();
  else
    e2->callee->remove_symbol_and_inline_clones ();
  if (edge->caller->call_site_hash)
    cgraph_update_edge_in_call_site_hash (edge);
  return edge;
}

/* Make an indirect edge with an unknown callee an ordinary edge leading to
   CALLEE.  DELTA is an integer constant that is to be added to the this
   pointer (first parameter) to compensate for skipping a thunk adjustment.  */

cgraph_edge *
cgraph_edge::make_direct (cgraph_node *callee)
{
  cgraph_edge *edge = this;
  gcc_assert (indirect_unknown_callee);

  /* If we are redirecting speculative call, make it non-speculative.  */
  if (indirect_unknown_callee && speculative)
    {
      edge = edge->resolve_speculation (callee->decl);

      /* On successful speculation just return the pre existing direct edge.  */
      if (!indirect_unknown_callee)
        return edge;
    }

  indirect_unknown_callee = 0;
  ggc_free (indirect_info);
  indirect_info = NULL;

  /* Get the edge out of the indirect edge list. */
  if (prev_callee)
    prev_callee->next_callee = next_callee;
  if (next_callee)
    next_callee->prev_callee = prev_callee;
  if (!prev_callee)
    caller->indirect_calls = next_callee;

  /* Put it into the normal callee list */
  prev_callee = NULL;
  next_callee = caller->callees;
  if (caller->callees)
    caller->callees->prev_callee = edge;
  caller->callees = edge;

  /* Insert to callers list of the new callee.  */
  edge->set_callee (callee);

  if (call_stmt
      && !gimple_check_call_matching_types (call_stmt, callee->decl, false))
    {
      call_stmt_cannot_inline_p = true;
      inline_failed = CIF_MISMATCHED_ARGUMENTS;
    }

  /* We need to re-determine the inlining status of the edge.  */
  initialize_inline_failed (edge);
  return edge;
}

/* If necessary, change the function declaration in the call statement
   associated with E so that it corresponds to the edge callee.  */

gimple *
cgraph_edge::redirect_call_stmt_to_callee (void)
{
  cgraph_edge *e = this;

  tree decl = gimple_call_fndecl (e->call_stmt);
  tree lhs = gimple_call_lhs (e->call_stmt);
  gcall *new_stmt;
  gimple_stmt_iterator gsi;
  bool skip_bounds = false;

  if (e->speculative)
    {
      cgraph_edge *e2;
      gcall *new_stmt;
      ipa_ref *ref;

      e->speculative_call_info (e, e2, ref);
      /* If there already is an direct call (i.e. as a result of inliner's
	 substitution), forget about speculating.  */
      if (decl)
	e = e->resolve_speculation (decl);
      /* If types do not match, speculation was likely wrong. 
         The direct edge was possibly redirected to the clone with a different
	 signature.  We did not update the call statement yet, so compare it 
	 with the reference that still points to the proper type.  */
      else if (!gimple_check_call_matching_types (e->call_stmt,
						  ref->referred->decl,
						  true))
	{
	  if (dump_file)
	    fprintf (dump_file, "Not expanding speculative call of %s/%i -> %s/%i\n"
		     "Type mismatch.\n",
		     xstrdup_for_dump (e->caller->name ()),
		     e->caller->order,
		     xstrdup_for_dump (e->callee->name ()),
		     e->callee->order);
	  e = e->resolve_speculation ();
	  /* We are producing the final function body and will throw away the
	     callgraph edges really soon.  Reset the counts/frequencies to
	     keep verifier happy in the case of roundoff errors.  */
	  e->count = gimple_bb (e->call_stmt)->count;
	  e->frequency = compute_call_stmt_bb_frequency
			  (e->caller->decl, gimple_bb (e->call_stmt));
	}
      /* Expand speculation into GIMPLE code.  */
      else
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "Expanding speculative call of %s/%i -> %s/%i count:"
		     "%" PRId64"\n",
		     xstrdup_for_dump (e->caller->name ()),
		     e->caller->order,
		     xstrdup_for_dump (e->callee->name ()),
		     e->callee->order,
		     (int64_t)e->count);
	  gcc_assert (e2->speculative);
	  push_cfun (DECL_STRUCT_FUNCTION (e->caller->decl));
	  new_stmt = gimple_ic (e->call_stmt,
				dyn_cast<cgraph_node *> (ref->referred),
				e->count || e2->count
				?  RDIV (e->count * REG_BR_PROB_BASE,
					 e->count + e2->count)
				: e->frequency || e2->frequency
				? RDIV (e->frequency * REG_BR_PROB_BASE,
					e->frequency + e2->frequency)
				: REG_BR_PROB_BASE / 2,
				e->count, e->count + e2->count);
	  e->speculative = false;
	  e->caller->set_call_stmt_including_clones (e->call_stmt, new_stmt,
						     false);

	  /* Fix edges for BUILT_IN_CHKP_BNDRET calls attached to the
	     processed call stmt.  */
	  if (gimple_call_with_bounds_p (new_stmt)
	      && gimple_call_lhs (new_stmt)
	      && chkp_retbnd_call_by_val (gimple_call_lhs (e2->call_stmt)))
	    {
	      tree dresult = gimple_call_lhs (new_stmt);
	      tree iresult = gimple_call_lhs (e2->call_stmt);
	      gcall *dbndret = chkp_retbnd_call_by_val (dresult);
	      gcall *ibndret = chkp_retbnd_call_by_val (iresult);
	      struct cgraph_edge *iedge
		= e2->caller->cgraph_node::get_edge (ibndret);
	      struct cgraph_edge *dedge;

	      if (dbndret)
		{
		  dedge = iedge->caller->create_edge (iedge->callee,
						      dbndret, e->count,
						      e->frequency);
		  dedge->frequency = compute_call_stmt_bb_frequency
		    (dedge->caller->decl, gimple_bb (dedge->call_stmt));
		}
	      iedge->frequency = compute_call_stmt_bb_frequency
		(iedge->caller->decl, gimple_bb (iedge->call_stmt));
	    }

	  e->frequency = compute_call_stmt_bb_frequency
			   (e->caller->decl, gimple_bb (e->call_stmt));
	  e2->frequency = compute_call_stmt_bb_frequency
			   (e2->caller->decl, gimple_bb (e2->call_stmt));
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

  /* We might propagate instrumented function pointer into
     not instrumented function and vice versa.  In such a
     case we need to either fix function declaration or
     remove bounds from call statement.  */
  if (flag_check_pointer_bounds && e->callee)
    skip_bounds = chkp_redirect_edge (e);

  if (e->indirect_unknown_callee
      || (decl == e->callee->decl
	  && !skip_bounds))
    return e->call_stmt;

  if (flag_checking && decl)
    {
      cgraph_node *node = cgraph_node::get (decl);
      gcc_assert (!node || !node->clone.combined_args_to_skip);
    }

  if (symtab->dump_file)
    {
      fprintf (symtab->dump_file, "updating call of %s/%i -> %s/%i: ",
	       xstrdup_for_dump (e->caller->name ()), e->caller->order,
	       xstrdup_for_dump (e->callee->name ()), e->callee->order);
      print_gimple_stmt (symtab->dump_file, e->call_stmt, 0, dump_flags);
      if (e->callee->clone.combined_args_to_skip)
	{
	  fprintf (symtab->dump_file, " combined args to skip: ");
	  dump_bitmap (symtab->dump_file,
		       e->callee->clone.combined_args_to_skip);
	}
    }

  if (e->callee->clone.combined_args_to_skip
      || skip_bounds)
    {
      int lp_nr;

      new_stmt = e->call_stmt;
      if (e->callee->clone.combined_args_to_skip)
	new_stmt
	  = gimple_call_copy_skip_args (new_stmt,
					e->callee->clone.combined_args_to_skip);
      if (skip_bounds)
	new_stmt = chkp_copy_call_skip_bounds (new_stmt);

      gimple_call_set_fndecl (new_stmt, e->callee->decl);
      gimple_call_set_fntype (new_stmt, gimple_call_fntype (e->call_stmt));

      if (gimple_vdef (new_stmt)
	  && TREE_CODE (gimple_vdef (new_stmt)) == SSA_NAME)
	SSA_NAME_DEF_STMT (gimple_vdef (new_stmt)) = new_stmt;

      gsi = gsi_for_stmt (e->call_stmt);

      /* For optimized away parameters, add on the caller side
	 before the call
	 DEBUG D#X => parm_Y(D)
	 stmts and associate D#X with parm in decl_debug_args_lookup
	 vector to say for debug info that if parameter parm had been passed,
	 it would have value parm_Y(D).  */
      if (e->callee->clone.combined_args_to_skip && MAY_HAVE_DEBUG_STMTS)
	{
	  vec<tree, va_gc> **debug_args
	    = decl_debug_args_lookup (e->callee->decl);
	  tree old_decl = gimple_call_fndecl (e->call_stmt);
	  if (debug_args && old_decl)
	    {
	      tree parm;
	      unsigned i = 0, num;
	      unsigned len = vec_safe_length (*debug_args);
	      unsigned nargs = gimple_call_num_args (e->call_stmt);
	      for (parm = DECL_ARGUMENTS (old_decl), num = 0;
		   parm && num < nargs;
		   parm = DECL_CHAIN (parm), num++)
		if (bitmap_bit_p (e->callee->clone.combined_args_to_skip, num)
		    && is_gimple_reg (parm))
		  {
		    unsigned last = i;

		    while (i < len && (**debug_args)[i] != DECL_ORIGIN (parm))
		      i += 2;
		    if (i >= len)
		      {
			i = 0;
			while (i < last
			       && (**debug_args)[i] != DECL_ORIGIN (parm))
			  i += 2;
			if (i >= last)
			  continue;
		      }
		    tree ddecl = (**debug_args)[i + 1];
		    tree arg = gimple_call_arg (e->call_stmt, num);
		    if (!useless_type_conversion_p (TREE_TYPE (ddecl),
						    TREE_TYPE (arg)))
		      {
			tree rhs1;
			if (!fold_convertible_p (TREE_TYPE (ddecl), arg))
			  continue;
			if (TREE_CODE (arg) == SSA_NAME
			    && gimple_assign_cast_p (SSA_NAME_DEF_STMT (arg))
			    && (rhs1
				= gimple_assign_rhs1 (SSA_NAME_DEF_STMT (arg)))
			    && useless_type_conversion_p (TREE_TYPE (ddecl),
							  TREE_TYPE (rhs1)))
			  arg = rhs1;
			else
			  arg = fold_convert (TREE_TYPE (ddecl), arg);
		      }

		    gimple *def_temp
		      = gimple_build_debug_bind (ddecl, unshare_expr (arg),
						 e->call_stmt);
		    gsi_insert_before (&gsi, def_temp, GSI_SAME_STMT);
		  }
	    }
	}

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
      gimple_call_set_fndecl (new_stmt, e->callee->decl);
      update_stmt_fn (DECL_STRUCT_FUNCTION (e->caller->decl), new_stmt);
    }

  /* If the call becomes noreturn, remove the LHS if possible.  */
  if (lhs
      && (gimple_call_flags (new_stmt) & ECF_NORETURN)
      && TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (lhs))) == INTEGER_CST)
    {
      if (TREE_CODE (lhs) == SSA_NAME)
	{
	  tree var = create_tmp_reg_fn (DECL_STRUCT_FUNCTION (e->caller->decl),
					TREE_TYPE (lhs), NULL);
	  var = get_or_create_ssa_default_def
		  (DECL_STRUCT_FUNCTION (e->caller->decl), var);
	  gimple *set_stmt = gimple_build_assign (lhs, var);
          gsi = gsi_for_stmt (new_stmt);
	  gsi_insert_before_without_update (&gsi, set_stmt, GSI_SAME_STMT);
	  update_stmt_fn (DECL_STRUCT_FUNCTION (e->caller->decl), set_stmt);
	}
      gimple_call_set_lhs (new_stmt, NULL_TREE);
      update_stmt_fn (DECL_STRUCT_FUNCTION (e->caller->decl), new_stmt);
    }

  /* If new callee has no static chain, remove it.  */
  if (gimple_call_chain (new_stmt) && !DECL_STATIC_CHAIN (e->callee->decl))
    {
      gimple_call_set_chain (new_stmt, NULL);
      update_stmt_fn (DECL_STRUCT_FUNCTION (e->caller->decl), new_stmt);
    }

  maybe_remove_unused_call_args (DECL_STRUCT_FUNCTION (e->caller->decl),
				 new_stmt);

  e->caller->set_call_stmt_including_clones (e->call_stmt, new_stmt, false);

  if (symtab->dump_file)
    {
      fprintf (symtab->dump_file, "  updated to:");
      print_gimple_stmt (symtab->dump_file, e->call_stmt, 0, dump_flags);
    }
  return new_stmt;
}

/* Update or remove the corresponding cgraph edge if a GIMPLE_CALL
   OLD_STMT changed into NEW_STMT.  OLD_CALL is gimple_call_fndecl
   of OLD_STMT if it was previously call statement.
   If NEW_STMT is NULL, the call has been dropped without any
   replacement.  */

static void
cgraph_update_edges_for_call_stmt_node (cgraph_node *node,
					gimple *old_stmt, tree old_call,
					gimple *new_stmt)
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
      cgraph_edge *e = node->get_edge (old_stmt);
      cgraph_edge *ne = NULL;
      gcov_type count;
      int frequency;

      if (e)
	{
	  /* Keep calls marked as dead dead.  */
	  if (new_stmt && is_gimple_call (new_stmt) && e->callee
	      && DECL_BUILT_IN_CLASS (e->callee->decl) == BUILT_IN_NORMAL
	      && DECL_FUNCTION_CODE (e->callee->decl) == BUILT_IN_UNREACHABLE)
	    {
              node->get_edge (old_stmt)->set_call_stmt
		 (as_a <gcall *> (new_stmt));
	      return;
	    }
	  /* See if the edge is already there and has the correct callee.  It
	     might be so because of indirect inlining has already updated
	     it.  We also might've cloned and redirected the edge.  */
	  if (new_call && e->callee)
	    {
	      cgraph_node *callee = e->callee;
	      while (callee)
		{
		  if (callee->decl == new_call
		      || callee->former_clone_of == new_call)
		    {
		      e->set_call_stmt (as_a <gcall *> (new_stmt));
		      return;
		    }
		  callee = callee->clone_of;
		}
	    }

	  /* Otherwise remove edge and create new one; we can't simply redirect
	     since function has changed, so inline plan and other information
	     attached to edge is invalid.  */
	  count = e->count;
	  frequency = e->frequency;
 	  if (e->indirect_unknown_callee || e->inline_failed)
	    e->remove ();
	  else
	    e->callee->remove_symbol_and_inline_clones ();
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
	  ne = node->create_edge (cgraph_node::get_create (new_call),
				  as_a <gcall *> (new_stmt), count,
				  frequency);
	  gcc_assert (ne->inline_failed);
	}
    }
  /* We only updated the call stmt; update pointer in cgraph edge..  */
  else if (old_stmt != new_stmt)
    node->get_edge (old_stmt)->set_call_stmt (as_a <gcall *> (new_stmt));
}

/* Update or remove the corresponding cgraph edge if a GIMPLE_CALL
   OLD_STMT changed into NEW_STMT.  OLD_DECL is gimple_call_fndecl
   of OLD_STMT before it was updated (updating can happen inplace).  */

void
cgraph_update_edges_for_call_stmt (gimple *old_stmt, tree old_decl,
				   gimple *new_stmt)
{
  cgraph_node *orig = cgraph_node::get (cfun->decl);
  cgraph_node *node;

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
cgraph_node::remove_callees (void)
{
  cgraph_edge *e, *f;

  /* It is sufficient to remove the edges from the lists of callers of
     the callees.  The callee list of the node can be zapped with one
     assignment.  */
  for (e = callees; e; e = f)
    {
      f = e->next_callee;
      symtab->call_edge_removal_hooks (e);
      if (!e->indirect_unknown_callee)
	e->remove_callee ();
      symtab->free_edge (e);
    }
  for (e = indirect_calls; e; e = f)
    {
      f = e->next_callee;
      symtab->call_edge_removal_hooks (e);
      if (!e->indirect_unknown_callee)
	e->remove_callee ();
      symtab->free_edge (e);
    }
  indirect_calls = NULL;
  callees = NULL;
  if (call_site_hash)
    {
      call_site_hash->empty ();
      call_site_hash = NULL;
    }
}

/* Remove all callers from the node.  */

void
cgraph_node::remove_callers (void)
{
  cgraph_edge *e, *f;

  /* It is sufficient to remove the edges from the lists of callees of
     the callers.  The caller list of the node can be zapped with one
     assignment.  */
  for (e = callers; e; e = f)
    {
      f = e->next_caller;
      symtab->call_edge_removal_hooks (e);
      e->remove_caller ();
      symtab->free_edge (e);
    }
  callers = NULL;
}

/* Helper function for cgraph_release_function_body and free_lang_data.
   It releases body from function DECL without having to inspect its
   possibly non-existent symtab node.  */

void
release_function_body (tree decl)
{
  function *fn = DECL_STRUCT_FUNCTION (decl);
  if (fn)
    {
      if (fn->cfg
	  && loops_for_fn (fn))
	{
	  fn->curr_properties &= ~PROP_loops;
	  loop_optimizer_finalize (fn);
	}
      if (fn->gimple_df)
	{
	  delete_tree_ssa (fn);
	  fn->eh = NULL;
	}
      if (fn->cfg)
	{
	  gcc_assert (!dom_info_available_p (fn, CDI_DOMINATORS));
	  gcc_assert (!dom_info_available_p (fn, CDI_POST_DOMINATORS));
	  delete_tree_cfg_annotations (fn);
	  clear_edges (fn);
	  fn->cfg = NULL;
	}
      if (fn->value_histograms)
	free_histograms (fn);
      gimple_set_body (decl, NULL);
      /* Struct function hangs a lot of data that would leak if we didn't
         removed all pointers to it.   */
      ggc_free (fn);
      DECL_STRUCT_FUNCTION (decl) = NULL;
    }
  DECL_SAVED_TREE (decl) = NULL;
}

/* Release memory used to represent body of function.
   Use this only for functions that are released before being translated to
   target code (i.e. RTL).  Functions that are compiled to RTL and beyond
   are free'd in final.c via free_after_compilation().
   KEEP_ARGUMENTS are useful only if you want to rebuild body as thunk.  */

void
cgraph_node::release_body (bool keep_arguments)
{
  ipa_transforms_to_apply.release ();
  if (!used_as_abstract_origin && symtab->state != PARSING)
    {
      DECL_RESULT (decl) = NULL;

      if (!keep_arguments)
	DECL_ARGUMENTS (decl) = NULL;
    }
  /* If the node is abstract and needed, then do not clear
     DECL_INITIAL of its associated function declaration because it's
     needed to emit debug info later.  */
  if (!used_as_abstract_origin && DECL_INITIAL (decl))
    DECL_INITIAL (decl) = error_mark_node;
  release_function_body (decl);
  if (lto_file_data)
    {
      lto_free_function_in_decl_state_for_node (this);
      lto_file_data = NULL;
    }
}

/* Remove function from symbol table.  */

void
cgraph_node::remove (void)
{
  cgraph_node *n;
  int uid = this->uid;

  symtab->call_cgraph_removal_hooks (this);
  remove_callers ();
  remove_callees ();
  ipa_transforms_to_apply.release ();

  /* Incremental inlining access removed nodes stored in the postorder list.
     */
  force_output = false;
  forced_by_abi = false;
  for (n = nested; n; n = n->next_nested)
    n->origin = NULL;
  nested = NULL;
  if (origin)
    {
      cgraph_node **node2 = &origin->nested;

      while (*node2 != this)
	node2 = &(*node2)->next_nested;
      *node2 = next_nested;
    }
  unregister ();
  if (prev_sibling_clone)
    prev_sibling_clone->next_sibling_clone = next_sibling_clone;
  else if (clone_of)
    clone_of->clones = next_sibling_clone;
  if (next_sibling_clone)
    next_sibling_clone->prev_sibling_clone = prev_sibling_clone;
  if (clones)
    {
      cgraph_node *n, *next;

      if (clone_of)
        {
	  for (n = clones; n->next_sibling_clone; n = n->next_sibling_clone)
	    n->clone_of = clone_of;
	  n->clone_of = clone_of;
	  n->next_sibling_clone = clone_of->clones;
	  if (clone_of->clones)
	    clone_of->clones->prev_sibling_clone = n;
	  clone_of->clones = clones;
	}
      else
        {
	  /* We are removing node with clones.  This makes clones inconsistent,
	     but assume they will be removed subsequently and just keep clone
	     tree intact.  This can happen in unreachable function removal since
	     we remove unreachable functions in random order, not by bottom-up
	     walk of clone trees.  */
	  for (n = clones; n; n = next)
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
  if (symtab->state != LTO_STREAMING)
    {
      n = cgraph_node::get (decl);
      if (!n
	  || (!n->clones && !n->clone_of && !n->global.inlined_to
	      && ((symtab->global_info_ready || in_lto_p)
		  && (TREE_ASM_WRITTEN (n->decl)
		      || DECL_EXTERNAL (n->decl)
		      || !n->analyzed
		      || (!flag_wpa && n->in_other_partition)))))
	release_body ();
    }
  else
    {
      lto_free_function_in_decl_state_for_node (this);
      lto_file_data = NULL;
    }

  decl = NULL;
  if (call_site_hash)
    {
      call_site_hash->empty ();
      call_site_hash = NULL;
    }

  if (instrumented_version)
    {
      instrumented_version->instrumented_version = NULL;
      instrumented_version = NULL;
    }

  symtab->release_symbol (this, uid);
}

/* Likewise indicate that a node is having address taken.  */

void
cgraph_node::mark_address_taken (void)
{
  /* Indirect inlining can figure out that all uses of the address are
     inlined.  */
  if (global.inlined_to)
    {
      gcc_assert (cfun->after_inlining);
      gcc_assert (callers->indirect_inlining_edge);
      return;
    }
  /* FIXME: address_taken flag is used both as a shortcut for testing whether
     IPA_REF_ADDR reference exists (and thus it should be set on node
     representing alias we take address of) and as a test whether address
     of the object was taken (and thus it should be set on node alias is
     referring to).  We should remove the first use and the remove the
     following set.  */
  address_taken = 1;
  cgraph_node *node = ultimate_alias_target ();
  node->address_taken = 1;
}

/* Return local info for the compiled function.  */

cgraph_local_info *
cgraph_node::local_info (tree decl)
{
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);
  cgraph_node *node = get (decl);
  if (!node)
    return NULL;
  return &node->ultimate_alias_target ()->local;
}

/* Return local info for the compiled function.  */

cgraph_rtl_info *
cgraph_node::rtl_info (tree decl)
{
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);
  cgraph_node *node = get (decl);
  if (!node)
    return NULL;
  node = node->ultimate_alias_target ();
  if (node->decl != current_function_decl
      && !TREE_ASM_WRITTEN (node->decl))
    return NULL;
  /* Allocate if it doesnt exist.  */
  if (node->ultimate_alias_target ()->rtl == NULL)
    node->ultimate_alias_target ()->rtl = ggc_cleared_alloc<cgraph_rtl_info> ();
  return node->ultimate_alias_target ()->rtl;
}

/* Return a string describing the failure REASON.  */

const char*
cgraph_inline_failed_string (cgraph_inline_failed_t reason)
{
#undef DEFCIFCODE
#define DEFCIFCODE(code, type, string)	string,

  static const char *cif_string_table[CIF_N_REASONS] = {
#include "cif-code.def"
  };

  /* Signedness of an enum type is implementation defined, so cast it
     to unsigned before testing. */
  gcc_assert ((unsigned) reason < CIF_N_REASONS);
  return cif_string_table[reason];
}

/* Return a type describing the failure REASON.  */

cgraph_inline_failed_type_t
cgraph_inline_failed_type (cgraph_inline_failed_t reason)
{
#undef DEFCIFCODE
#define DEFCIFCODE(code, type, string)	type,

  static cgraph_inline_failed_type_t cif_type_table[CIF_N_REASONS] = {
#include "cif-code.def"
  };

  /* Signedness of an enum type is implementation defined, so cast it
     to unsigned before testing. */
  gcc_assert ((unsigned) reason < CIF_N_REASONS);
  return cif_type_table[reason];
}

/* Names used to print out the availability enum.  */
const char * const cgraph_availability_names[] =
  {"unset", "not_available", "overwritable", "available", "local"};

/* Output flags of edge to a file F.  */

void
cgraph_edge::dump_edge_flags (FILE *f)
{
  if (speculative)
    fprintf (f, "(speculative) ");
  if (!inline_failed)
    fprintf (f, "(inlined) ");
  if (call_stmt_cannot_inline_p)
    fprintf (f, "(call_stmt_cannot_inline_p) ");
  if (indirect_inlining_edge)
    fprintf (f, "(indirect_inlining) ");
  if (count)
    fprintf (f, "(%" PRId64"x) ", (int64_t)count);
  if (frequency)
    fprintf (f, "(%.2f per call) ", frequency / (double)CGRAPH_FREQ_BASE);
  if (can_throw_external)
    fprintf (f, "(can throw external) ");
}

/* Dump call graph node to file F.  */

void
cgraph_node::dump (FILE *f)
{
  cgraph_edge *edge;

  dump_base (f);

  if (global.inlined_to)
    fprintf (f, "  Function %s/%i is inline copy in %s/%i\n",
	     xstrdup_for_dump (name ()),
	     order,
	     xstrdup_for_dump (global.inlined_to->name ()),
	     global.inlined_to->order);
  if (clone_of)
    fprintf (f, "  Clone of %s/%i\n",
	     clone_of->asm_name (),
	     clone_of->order);
  if (symtab->function_flags_ready)
    fprintf (f, "  Availability: %s\n",
	     cgraph_availability_names [get_availability ()]);

  if (profile_id)
    fprintf (f, "  Profile id: %i\n",
	     profile_id);
  fprintf (f, "  First run: %i\n", tp_first_run);
  fprintf (f, "  Function flags:");
  if (count)
    fprintf (f, " executed %" PRId64"x",
	     (int64_t)count);
  if (origin)
    fprintf (f, " nested in: %s", origin->asm_name ());
  if (gimple_has_body_p (decl))
    fprintf (f, " body");
  if (process)
    fprintf (f, " process");
  if (local.local)
    fprintf (f, " local");
  if (local.redefined_extern_inline)
    fprintf (f, " redefined_extern_inline");
  if (only_called_at_startup)
    fprintf (f, " only_called_at_startup");
  if (only_called_at_exit)
    fprintf (f, " only_called_at_exit");
  if (tm_clone)
    fprintf (f, " tm_clone");
  if (icf_merged)
    fprintf (f, " icf_merged");
  if (merged_comdat)
    fprintf (f, " merged_comdat");
  if (split_part)
    fprintf (f, " split_part");
  if (indirect_call_target)
    fprintf (f, " indirect_call_target");
  if (nonfreeing_fn)
    fprintf (f, " nonfreeing_fn");
  if (DECL_STATIC_CONSTRUCTOR (decl))
    fprintf (f," static_constructor (priority:%i)", get_init_priority ());
  if (DECL_STATIC_DESTRUCTOR (decl))
    fprintf (f," static_destructor (priority:%i)", get_fini_priority ());
  if (frequency == NODE_FREQUENCY_HOT)
    fprintf (f, " hot");
  if (frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED)
    fprintf (f, " unlikely_executed");
  if (frequency == NODE_FREQUENCY_EXECUTED_ONCE)
    fprintf (f, " executed_once");
  if (only_called_at_startup)
    fprintf (f, " only_called_at_startup");
  if (only_called_at_exit)
    fprintf (f, " only_called_at_exit");
  if (opt_for_fn (decl, optimize_size))
    fprintf (f, " optimize_size");
  if (parallelized_function)
    fprintf (f, " parallelized_function");

  fprintf (f, "\n");

  if (thunk.thunk_p)
    {
      fprintf (f, "  Thunk");
      if (thunk.alias)
        fprintf (f, "  of %s (asm: %s)",
		 lang_hooks.decl_printable_name (thunk.alias, 2),
		 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk.alias)));
      fprintf (f, " fixed offset %i virtual value %i has "
	       "virtual offset %i)\n",
	       (int)thunk.fixed_offset,
	       (int)thunk.virtual_value,
	       (int)thunk.virtual_offset_p);
    }
  if (alias && thunk.alias
      && DECL_P (thunk.alias))
    {
      fprintf (f, "  Alias of %s",
	       lang_hooks.decl_printable_name (thunk.alias, 2));
      if (DECL_ASSEMBLER_NAME_SET_P (thunk.alias))
        fprintf (f, " (asm: %s)",
		 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk.alias)));
      fprintf (f, "\n");
    }
  
  fprintf (f, "  Called by: ");

  for (edge = callers; edge; edge = edge->next_caller)
    {
      fprintf (f, "%s/%i ", edge->caller->asm_name (),
	       edge->caller->order);
      edge->dump_edge_flags (f);
    }

  fprintf (f, "\n  Calls: ");
  for (edge = callees; edge; edge = edge->next_callee)
    {
      fprintf (f, "%s/%i ", edge->callee->asm_name (),
	       edge->callee->order);
      edge->dump_edge_flags (f);
    }
  fprintf (f, "\n");

  for (edge = indirect_calls; edge; edge = edge->next_callee)
    {
      if (edge->indirect_info->polymorphic)
	{
          fprintf (f, "   Polymorphic indirect call of type ");
	  print_generic_expr (f, edge->indirect_info->otr_type, TDF_SLIM);
	  fprintf (f, " token:%i", (int) edge->indirect_info->otr_token);
	}
      else
        fprintf (f, "   Indirect call");
      edge->dump_edge_flags (f);
      if (edge->indirect_info->param_index != -1)
	{
	  fprintf (f, " of param:%i", edge->indirect_info->param_index);
	  if (edge->indirect_info->agg_contents)
	   fprintf (f, " loaded from %s %s at offset %i",
		    edge->indirect_info->member_ptr ? "member ptr" : "aggregate",
		    edge->indirect_info->by_ref ? "passed by reference":"",
		    (int)edge->indirect_info->offset);
	  if (edge->indirect_info->vptr_changed)
	    fprintf (f, " (vptr maybe changed)");
	}
      fprintf (f, "\n");
      if (edge->indirect_info->polymorphic)
	edge->indirect_info->context.dump (f);
    }

  if (instrumentation_clone)
    fprintf (f, "  Is instrumented version.\n");
  else if (instrumented_version)
    fprintf (f, "  Has instrumented version.\n");
}

/* Dump call graph node NODE to stderr.  */

DEBUG_FUNCTION void
cgraph_node::debug (void)
{
  dump (stderr);
}

/* Dump the callgraph to file F.  */

void
cgraph_node::dump_cgraph (FILE *f)
{
  cgraph_node *node;

  fprintf (f, "callgraph:\n\n");
  FOR_EACH_FUNCTION (node)
    node->dump (f);
}

/* Return true when the DECL can possibly be inlined.  */

bool
cgraph_function_possibly_inlined_p (tree decl)
{
  if (!symtab->global_info_ready)
    return !DECL_UNINLINABLE (decl);
  return DECL_POSSIBLY_INLINED (decl);
}

/* cgraph_node is no longer nested function; update cgraph accordingly.  */
void
cgraph_node::unnest (void)
{
  cgraph_node **node2 = &origin->nested;
  gcc_assert (origin);

  while (*node2 != this)
    node2 = &(*node2)->next_nested;
  *node2 = next_nested;
  origin = NULL;
}

/* Return function availability.  See cgraph.h for description of individual
   return values.  */
enum availability
cgraph_node::get_availability (symtab_node *ref)
{
  if (ref)
    {
      cgraph_node *cref = dyn_cast <cgraph_node *> (ref);
      if (cref)
	ref = cref->global.inlined_to;
    }
  enum availability avail;
  if (!analyzed)
    avail = AVAIL_NOT_AVAILABLE;
  else if (local.local)
    avail = AVAIL_LOCAL;
  else if (global.inlined_to)
    avail = AVAIL_AVAILABLE;
  else if (transparent_alias)
    ultimate_alias_target (&avail, ref);
  else if (lookup_attribute ("ifunc", DECL_ATTRIBUTES (decl)))
    avail = AVAIL_INTERPOSABLE;
  else if (!externally_visible)
    avail = AVAIL_AVAILABLE;
  /* If this is a reference from symbol itself and there are no aliases, we
     may be sure that the symbol was not interposed by soemthing else because
     the symbol itself would be unreachable otherwise.

     Also comdat groups are always resolved in groups.  */
  else if ((this == ref && !has_aliases_p ())
           || (ref && get_comdat_group ()
               && get_comdat_group () == ref->get_comdat_group ()))
    avail = AVAIL_AVAILABLE;
  /* Inline functions are safe to be analyzed even if their symbol can
     be overwritten at runtime.  It is not meaningful to enforce any sane
     behavior on replacing inline function by different body.  */
  else if (DECL_DECLARED_INLINE_P (decl))
    avail = AVAIL_AVAILABLE;

  /* If the function can be overwritten, return OVERWRITABLE.  Take
     care at least of two notable extensions - the COMDAT functions
     used to share template instantiations in C++ (this is symmetric
     to code cp_cannot_inline_tree_fn and probably shall be shared and
     the inlinability hooks completely eliminated).  */

  else if (decl_replaceable_p (decl) && !DECL_EXTERNAL (decl))
    avail = AVAIL_INTERPOSABLE;
  else avail = AVAIL_AVAILABLE;

  return avail;
}

/* Worker for cgraph_node_can_be_local_p.  */
static bool
cgraph_node_cannot_be_local_p_1 (cgraph_node *node, void *)
{
  return !(!node->force_output
	   && ((DECL_COMDAT (node->decl)
		&& !node->forced_by_abi
		&& !node->used_from_object_file_p ()
		&& !node->same_comdat_group)
	       || !node->externally_visible));
}

/* Return true if cgraph_node can be made local for API change.
   Extern inline functions and C++ COMDAT functions can be made local
   at the expense of possible code size growth if function is used in multiple
   compilation units.  */
bool
cgraph_node::can_be_local_p (void)
{
  return (!address_taken
	  && !call_for_symbol_thunks_and_aliases (cgraph_node_cannot_be_local_p_1,
						NULL, true));
}

/* Call callback on cgraph_node, thunks and aliases associated to cgraph_node.
   When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
   skipped.  When EXCLUDE_VIRTUAL_THUNKS is true, virtual thunks are
   skipped.  */
bool
cgraph_node::call_for_symbol_thunks_and_aliases (bool (*callback)
						   (cgraph_node *, void *),
						 void *data,
						 bool include_overwritable,
						 bool exclude_virtual_thunks)
{
  cgraph_edge *e;
  ipa_ref *ref;

  if (callback (this, data))
    return true;
  FOR_EACH_ALIAS (this, ref)
    {
      cgraph_node *alias = dyn_cast <cgraph_node *> (ref->referring);
      if (include_overwritable
	  || alias->get_availability () > AVAIL_INTERPOSABLE)
	if (alias->call_for_symbol_thunks_and_aliases (callback, data,
						     include_overwritable,
						     exclude_virtual_thunks))
	  return true;
    }
  if (get_availability () <= AVAIL_INTERPOSABLE)
    return false;
  for (e = callers; e; e = e->next_caller)
    if (e->caller->thunk.thunk_p
	&& (include_overwritable
	    || e->caller->get_availability () > AVAIL_INTERPOSABLE)
	&& !(exclude_virtual_thunks
	     && e->caller->thunk.virtual_offset_p))
      if (e->caller->call_for_symbol_thunks_and_aliases (callback, data,
						       include_overwritable,
						       exclude_virtual_thunks))
	return true;

  return false;
}

/* Worker to bring NODE local.  */

bool
cgraph_node::make_local (cgraph_node *node, void *)
{
  gcc_checking_assert (node->can_be_local_p ());
  if (DECL_COMDAT (node->decl) || DECL_EXTERNAL (node->decl))
    {
      node->make_decl_local ();
      node->set_section (NULL);
      node->set_comdat_group (NULL);
      node->externally_visible = false;
      node->forced_by_abi = false;
      node->local.local = true;
      node->set_section (NULL);
      node->unique_name = ((node->resolution == LDPR_PREVAILING_DEF_IRONLY
			   || node->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP)
			   && !flag_incremental_link);
      node->resolution = LDPR_PREVAILING_DEF_IRONLY;
      gcc_assert (node->get_availability () == AVAIL_LOCAL);
    }
  return false;
}

/* Bring cgraph node local.  */

void
cgraph_node::make_local (void)
{
  call_for_symbol_thunks_and_aliases (cgraph_node::make_local, NULL, true);
}

/* Worker to set nothrow flag.  */

static void
set_nothrow_flag_1 (cgraph_node *node, bool nothrow, bool non_call,
		    bool *changed)
{
  cgraph_edge *e;

  if (nothrow && !TREE_NOTHROW (node->decl))
    {
      /* With non-call exceptions we can't say for sure if other function body
	 was not possibly optimized to stil throw.  */
      if (!non_call || node->binds_to_current_def_p ())
	{
	  TREE_NOTHROW (node->decl) = true;
	  *changed = true;
	  for (e = node->callers; e; e = e->next_caller)
	    e->can_throw_external = false;
	}
    }
  else if (!nothrow && TREE_NOTHROW (node->decl))
    {
      TREE_NOTHROW (node->decl) = false;
      *changed = true;
    }
  ipa_ref *ref;
  FOR_EACH_ALIAS (node, ref)
    {
      cgraph_node *alias = dyn_cast <cgraph_node *> (ref->referring);
      if (!nothrow || alias->get_availability () > AVAIL_INTERPOSABLE)
	set_nothrow_flag_1 (alias, nothrow, non_call, changed);
    }
  for (cgraph_edge *e = node->callers; e; e = e->next_caller)
    if (e->caller->thunk.thunk_p
	&& (!nothrow || e->caller->get_availability () > AVAIL_INTERPOSABLE))
      set_nothrow_flag_1 (e->caller, nothrow, non_call, changed);
}

/* Set TREE_NOTHROW on NODE's decl and on aliases of NODE
   if any to NOTHROW.  */

bool
cgraph_node::set_nothrow_flag (bool nothrow)
{
  bool changed = false;
  bool non_call = opt_for_fn (decl, flag_non_call_exceptions);

  if (!nothrow || get_availability () > AVAIL_INTERPOSABLE)
    set_nothrow_flag_1 (this, nothrow, non_call, &changed);
  else
    {
      ipa_ref *ref;

      FOR_EACH_ALIAS (this, ref)
	{
	  cgraph_node *alias = dyn_cast <cgraph_node *> (ref->referring);
	  if (!nothrow || alias->get_availability () > AVAIL_INTERPOSABLE)
	    set_nothrow_flag_1 (alias, nothrow, non_call, &changed);
	}
    }
  return changed;
}

/* Worker to set_const_flag.  */

static void
set_const_flag_1 (cgraph_node *node, bool set_const, bool looping,
		  bool *changed)
{
  /* Static constructors and destructors without a side effect can be
     optimized out.  */
  if (set_const && !looping)
    {
      if (DECL_STATIC_CONSTRUCTOR (node->decl))
	{
	  DECL_STATIC_CONSTRUCTOR (node->decl) = 0;
	  *changed = true;
	}
      if (DECL_STATIC_DESTRUCTOR (node->decl))
	{
	  DECL_STATIC_DESTRUCTOR (node->decl) = 0;
	  *changed = true;
	}
    }
  if (!set_const)
    {
      if (TREE_READONLY (node->decl))
	{
          TREE_READONLY (node->decl) = 0;
          DECL_LOOPING_CONST_OR_PURE_P (node->decl) = false;
	  *changed = true;
	}
    }
  else
    {
      /* Consider function:

	 bool a(int *p)
	 {
	   return *p==*p;
	 }

	 During early optimization we will turn this into:

	 bool a(int *p)
	 {
	   return true;
	 }

	 Now if this function will be detected as CONST however when interposed
	 it may end up being just pure.  We always must assume the worst
	 scenario here.  */
      if (TREE_READONLY (node->decl))
	{
	  if (!looping && DECL_LOOPING_CONST_OR_PURE_P (node->decl))
	    {
              DECL_LOOPING_CONST_OR_PURE_P (node->decl) = false;
	      *changed = true;
	    }
	}
      else if (node->binds_to_current_def_p ())
	{
	  TREE_READONLY (node->decl) = true;
          DECL_LOOPING_CONST_OR_PURE_P (node->decl) = looping;
	  DECL_PURE_P (node->decl) = false;
	  *changed = true;
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Dropping state to PURE because function does "
		     "not bind to current def.\n");
	  if (!DECL_PURE_P (node->decl))
	    {
	      DECL_PURE_P (node->decl) = true;
              DECL_LOOPING_CONST_OR_PURE_P (node->decl) = looping;
	      *changed = true;
	    }
	  else if (!looping && DECL_LOOPING_CONST_OR_PURE_P (node->decl))
	    {
              DECL_LOOPING_CONST_OR_PURE_P (node->decl) = false;
	      *changed = true;
	    }
	}
    }

  ipa_ref *ref;
  FOR_EACH_ALIAS (node, ref)
    {
      cgraph_node *alias = dyn_cast <cgraph_node *> (ref->referring);
      if (!set_const || alias->get_availability () > AVAIL_INTERPOSABLE)
	set_const_flag_1 (alias, set_const, looping, changed);
    }
  for (cgraph_edge *e = node->callers; e; e = e->next_caller)
    if (e->caller->thunk.thunk_p
	&& (!set_const || e->caller->get_availability () > AVAIL_INTERPOSABLE))
      {
	/* Virtual thunks access virtual offset in the vtable, so they can
	   only be pure, never const.  */
        if (set_const
	    && (e->caller->thunk.virtual_offset_p
	        || !node->binds_to_current_def_p (e->caller)))
	  *changed |= e->caller->set_pure_flag (true, looping);
	else
	  set_const_flag_1 (e->caller, set_const, looping, changed);
      }
}

/* If SET_CONST is true, mark function, aliases and thunks to be ECF_CONST.
   If SET_CONST if false, clear the flag.

   When setting the flag be careful about possible interposition and
   do not set the flag for functions that can be interposet and set pure
   flag for functions that can bind to other definition. 

   Return true if any change was done. */

bool
cgraph_node::set_const_flag (bool set_const, bool looping)
{
  bool changed = false;
  if (!set_const || get_availability () > AVAIL_INTERPOSABLE)
    set_const_flag_1 (this, set_const, looping, &changed);
  else
    {
      ipa_ref *ref;

      FOR_EACH_ALIAS (this, ref)
	{
	  cgraph_node *alias = dyn_cast <cgraph_node *> (ref->referring);
	  if (!set_const || alias->get_availability () > AVAIL_INTERPOSABLE)
	    set_const_flag_1 (alias, set_const, looping, &changed);
	}
    }
  return changed;
}

/* Info used by set_pure_flag_1.  */

struct set_pure_flag_info
{
  bool pure;
  bool looping;
  bool changed;
};

/* Worker to set_pure_flag.  */

static bool
set_pure_flag_1 (cgraph_node *node, void *data)
{
  struct set_pure_flag_info *info = (struct set_pure_flag_info *)data;
  /* Static constructors and destructors without a side effect can be
     optimized out.  */
  if (info->pure && !info->looping)
    {
      if (DECL_STATIC_CONSTRUCTOR (node->decl))
	{
	  DECL_STATIC_CONSTRUCTOR (node->decl) = 0;
	  info->changed = true;
	}
      if (DECL_STATIC_DESTRUCTOR (node->decl))
	{
	  DECL_STATIC_DESTRUCTOR (node->decl) = 0;
	  info->changed = true;
	}
    }
  if (info->pure)
    {
      if (!DECL_PURE_P (node->decl) && !TREE_READONLY (node->decl))
	{
          DECL_PURE_P (node->decl) = true;
          DECL_LOOPING_CONST_OR_PURE_P (node->decl) = info->looping;
	  info->changed = true;
	}
      else if (DECL_LOOPING_CONST_OR_PURE_P (node->decl)
	       && !info->looping)
	{
          DECL_LOOPING_CONST_OR_PURE_P (node->decl) = false;
	  info->changed = true;
	}
    }
  else
    {
      if (DECL_PURE_P (node->decl))
	{
          DECL_PURE_P (node->decl) = false;
          DECL_LOOPING_CONST_OR_PURE_P (node->decl) = false;
	  info->changed = true;
	}
    }
  return false;
}

/* Set DECL_PURE_P on cgraph_node's decl and on aliases of the node
   if any to PURE.

   When setting the flag, be careful about possible interposition.
   Return true if any change was done. */

bool
cgraph_node::set_pure_flag (bool pure, bool looping)
{
  struct set_pure_flag_info info = {pure, looping, false};
  if (!pure)
    looping = false;
  call_for_symbol_thunks_and_aliases (set_pure_flag_1, &info, !pure, true);
  return info.changed;
}

/* Return true when cgraph_node can not return or throw and thus
   it is safe to ignore its side effects for IPA analysis.  */

bool
cgraph_node::cannot_return_p (void)
{
  int flags = flags_from_decl_or_type (decl);
  if (!opt_for_fn (decl, flag_exceptions))
    return (flags & ECF_NORETURN) != 0;
  else
    return ((flags & (ECF_NORETURN | ECF_NOTHROW))
	     == (ECF_NORETURN | ECF_NOTHROW));
}

/* Return true when call of edge can not lead to return from caller
   and thus it is safe to ignore its side effects for IPA analysis
   when computing side effects of the caller.
   FIXME: We could actually mark all edges that have no reaching
   patch to the exit block or throw to get better results.  */
bool
cgraph_edge::cannot_lead_to_return_p (void)
{
  if (caller->cannot_return_p ())
    return true;
  if (indirect_unknown_callee)
    {
      int flags = indirect_info->ecf_flags;
      if (!opt_for_fn (caller->decl, flag_exceptions))
	return (flags & ECF_NORETURN) != 0;
      else
	return ((flags & (ECF_NORETURN | ECF_NOTHROW))
		 == (ECF_NORETURN | ECF_NOTHROW));
    }
  else
    return callee->cannot_return_p ();
}

/* Return true if the call can be hot.  */

bool
cgraph_edge::maybe_hot_p (void)
{
  /* TODO: Export profile_status from cfun->cfg to cgraph_node.  */
  if (profile_info
      && opt_for_fn (caller->decl, flag_branch_probabilities)
      && !maybe_hot_count_p (NULL, count))
    return false;
  if (caller->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED
      || (callee
	  && callee->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED))
    return false;
  if (caller->frequency > NODE_FREQUENCY_UNLIKELY_EXECUTED
      && (callee
	  && callee->frequency <= NODE_FREQUENCY_EXECUTED_ONCE))
    return false;
  if (opt_for_fn (caller->decl, optimize_size))
    return false;
  if (caller->frequency == NODE_FREQUENCY_HOT)
    return true;
  if (caller->frequency == NODE_FREQUENCY_EXECUTED_ONCE
      && frequency < CGRAPH_FREQ_BASE * 3 / 2)
    return false;
  if (opt_for_fn (caller->decl, flag_guess_branch_prob))
    {
      if (PARAM_VALUE (HOT_BB_FREQUENCY_FRACTION) == 0
	  || frequency <= (CGRAPH_FREQ_BASE
			   / PARAM_VALUE (HOT_BB_FREQUENCY_FRACTION)))
        return false;
    }
  return true;
}

/* Worker for cgraph_can_remove_if_no_direct_calls_p.  */

static bool
nonremovable_p (cgraph_node *node, void *)
{
  return !node->can_remove_if_no_direct_calls_and_refs_p ();
}

/* Return true if whole comdat group can be removed if there are no direct
   calls to THIS.  */

bool
cgraph_node::can_remove_if_no_direct_calls_p (bool will_inline)
{
  struct ipa_ref *ref;

  /* For local symbols or non-comdat group it is the same as 
     can_remove_if_no_direct_calls_p.  */
  if (!externally_visible || !same_comdat_group)
    {
      if (DECL_EXTERNAL (decl))
	return true;
      if (address_taken)
	return false;
      return !call_for_symbol_and_aliases (nonremovable_p, NULL, true);
    }

  if (will_inline && address_taken)
    return false;

  /* Otheriwse check if we can remove the symbol itself and then verify
     that only uses of the comdat groups are direct call to THIS
     or its aliases.   */
  if (!can_remove_if_no_direct_calls_and_refs_p ())
    return false;

  /* Check that all refs come from within the comdat group.  */
  for (int i = 0; iterate_referring (i, ref); i++)
    if (ref->referring->get_comdat_group () != get_comdat_group ())
      return false;

  struct cgraph_node *target = ultimate_alias_target ();
  for (cgraph_node *next = dyn_cast<cgraph_node *> (same_comdat_group);
       next != this; next = dyn_cast<cgraph_node *> (next->same_comdat_group))
    {
      if (!externally_visible)
	continue;
      if (!next->alias
	  && !next->can_remove_if_no_direct_calls_and_refs_p ())
	return false;

      /* If we see different symbol than THIS, be sure to check calls.  */
      if (next->ultimate_alias_target () != target)
	for (cgraph_edge *e = next->callers; e; e = e->next_caller)
	  if (e->caller->get_comdat_group () != get_comdat_group ()
	      || will_inline)
	    return false;

      /* If function is not being inlined, we care only about
	 references outside of the comdat group.  */
      if (!will_inline)
        for (int i = 0; next->iterate_referring (i, ref); i++)
	  if (ref->referring->get_comdat_group () != get_comdat_group ())
	    return false;
    }
  return true;
}

/* Return true when function cgraph_node can be expected to be removed
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
cgraph_node::will_be_removed_from_program_if_no_direct_calls_p
	 (bool will_inline)
{
  gcc_assert (!global.inlined_to);
  if (DECL_EXTERNAL (decl))
    return true;

  if (!in_lto_p && !flag_whole_program)
    {
      /* If the symbol is in comdat group, we need to verify that whole comdat
	 group becomes unreachable.  Technically we could skip references from
	 within the group, too.  */
      if (!only_called_directly_p ())
	return false;
      if (same_comdat_group && externally_visible)
	{
	  struct cgraph_node *target = ultimate_alias_target ();

	  if (will_inline && address_taken)
	    return true;
	  for (cgraph_node *next = dyn_cast<cgraph_node *> (same_comdat_group);
	       next != this;
	       next = dyn_cast<cgraph_node *> (next->same_comdat_group))
	    {
	      if (!externally_visible)
		continue;
	      if (!next->alias
		  && !next->only_called_directly_p ())
		return false;

	      /* If we see different symbol than THIS,
		 be sure to check calls.  */
	      if (next->ultimate_alias_target () != target)
		for (cgraph_edge *e = next->callers; e; e = e->next_caller)
		  if (e->caller->get_comdat_group () != get_comdat_group ()
		      || will_inline)
		    return false;
	    }
	}
      return true;
    }
  else
    return can_remove_if_no_direct_calls_p (will_inline);
}


/* Worker for cgraph_only_called_directly_p.  */

static bool
cgraph_not_only_called_directly_p_1 (cgraph_node *node, void *)
{
  return !node->only_called_directly_or_aliased_p ();
}

/* Return true when function cgraph_node and all its aliases are only called
   directly.
   i.e. it is not externally visible, address was not taken and
   it is not used in any other non-standard way.  */

bool
cgraph_node::only_called_directly_p (void)
{
  gcc_assert (ultimate_alias_target () == this);
  return !call_for_symbol_and_aliases (cgraph_not_only_called_directly_p_1,
				       NULL, true);
}


/* Collect all callers of NODE.  Worker for collect_callers_of_node.  */

static bool
collect_callers_of_node_1 (cgraph_node *node, void *data)
{
  vec<cgraph_edge *> *redirect_callers = (vec<cgraph_edge *> *)data;
  cgraph_edge *cs;
  enum availability avail;
  node->ultimate_alias_target (&avail);

  if (avail > AVAIL_INTERPOSABLE)
    for (cs = node->callers; cs != NULL; cs = cs->next_caller)
      if (!cs->indirect_inlining_edge
	  && !cs->caller->thunk.thunk_p)
        redirect_callers->safe_push (cs);
  return false;
}

/* Collect all callers of cgraph_node and its aliases that are known to lead to
   cgraph_node (i.e. are not overwritable).  */

vec<cgraph_edge *>
cgraph_node::collect_callers (void)
{
  vec<cgraph_edge *> redirect_callers = vNULL;
  call_for_symbol_thunks_and_aliases (collect_callers_of_node_1,
				    &redirect_callers, false);
  return redirect_callers;
}

/* Return TRUE if NODE2 a clone of NODE or is equivalent to it.  */

static bool
clone_of_p (cgraph_node *node, cgraph_node *node2)
{
  bool skipped_thunk = false;
  node = node->ultimate_alias_target ();
  node2 = node2->ultimate_alias_target ();

  /* There are no virtual clones of thunks so check former_clone_of or if we
     might have skipped thunks because this adjustments are no longer
     necessary.  */
  while (node->thunk.thunk_p)
    {
      if (node2->former_clone_of == node->decl)
	return true;
      if (!node->thunk.this_adjusting)
	return false;
      node = node->callees->callee->ultimate_alias_target ();
      skipped_thunk = true;
    }

  if (skipped_thunk)
    {
      if (!node2->clone.args_to_skip
	  || !bitmap_bit_p (node2->clone.args_to_skip, 0))
	return false;
      if (node2->former_clone_of == node->decl)
	return true;
      else if (!node2->clone_of)
	return false;
    }

  while (node != node2 && node2)
    node2 = node2->clone_of;
  return node2 != NULL;
}

/* Verify edge count and frequency.  */

bool
cgraph_edge::verify_count_and_frequency ()
{
  bool error_found = false;
  if (count < 0)
    {
      error ("caller edge count is negative");
      error_found = true;
    }
  if (frequency < 0)
    {
      error ("caller edge frequency is negative");
      error_found = true;
    }
  if (frequency > CGRAPH_FREQ_MAX)
    {
      error ("caller edge frequency is too large");
      error_found = true;
    }
  return error_found;
}

/* Switch to THIS_CFUN if needed and print STMT to stderr.  */
static void
cgraph_debug_gimple_stmt (function *this_cfun, gimple *stmt)
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

/* Verify that call graph edge corresponds to DECL from the associated
   statement.  Return true if the verification should fail.  */

bool
cgraph_edge::verify_corresponds_to_fndecl (tree decl)
{
  cgraph_node *node;

  if (!decl || callee->global.inlined_to)
    return false;
  if (symtab->state == LTO_STREAMING)
    return false;
  node = cgraph_node::get (decl);

  /* We do not know if a node from a different partition is an alias or what it
     aliases and therefore cannot do the former_clone_of check reliably.  When
     body_removed is set, we have lost all information about what was alias or
     thunk of and also cannot proceed.  */
  if (!node
      || node->body_removed
      || node->in_other_partition
      || callee->icf_merged
      || callee->in_other_partition)
    return false;

  node = node->ultimate_alias_target ();

  /* Optimizers can redirect unreachable calls or calls triggering undefined
     behavior to builtin_unreachable.  */
  if (DECL_BUILT_IN_CLASS (callee->decl) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (callee->decl) == BUILT_IN_UNREACHABLE)
    return false;

  if (callee->former_clone_of != node->decl
      && (node != callee->ultimate_alias_target ())
      && !clone_of_p (node, callee))
    return true;
  else
    return false;
}

/* Verify cgraph nodes of given cgraph node.  */
DEBUG_FUNCTION void
cgraph_node::verify_node (void)
{
  cgraph_edge *e;
  function *this_cfun = DECL_STRUCT_FUNCTION (decl);
  basic_block this_block;
  gimple_stmt_iterator gsi;
  bool error_found = false;

  if (seen_error ())
    return;

  timevar_push (TV_CGRAPH_VERIFY);
  error_found |= verify_base ();
  for (e = callees; e; e = e->next_callee)
    if (e->aux)
      {
	error ("aux field set for edge %s->%s",
	       identifier_to_locale (e->caller->name ()),
	       identifier_to_locale (e->callee->name ()));
	error_found = true;
      }
  if (count < 0)
    {
      error ("execution count is negative");
      error_found = true;
    }
  if (global.inlined_to && same_comdat_group)
    {
      error ("inline clone in same comdat group list");
      error_found = true;
    }
  if (!definition && !in_other_partition && local.local)
    {
      error ("local symbols must be defined");
      error_found = true;
    }
  if (global.inlined_to && externally_visible)
    {
      error ("externally visible inline clone");
      error_found = true;
    }
  if (global.inlined_to && address_taken)
    {
      error ("inline clone with address taken");
      error_found = true;
    }
  if (global.inlined_to && force_output)
    {
      error ("inline clone is forced to output");
      error_found = true;
    }
  for (e = indirect_calls; e; e = e->next_callee)
    {
      if (e->aux)
	{
	  error ("aux field set for indirect edge from %s",
		 identifier_to_locale (e->caller->name ()));
	  error_found = true;
	}
      if (!e->indirect_unknown_callee
	  || !e->indirect_info)
	{
	  error ("An indirect edge from %s is not marked as indirect or has "
		 "associated indirect_info, the corresponding statement is: ",
		 identifier_to_locale (e->caller->name ()));
	  cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
	  error_found = true;
	}
    }
  bool check_comdat = comdat_local_p ();
  for (e = callers; e; e = e->next_caller)
    {
      if (e->verify_count_and_frequency ())
	error_found = true;
      if (check_comdat
	  && !in_same_comdat_group_p (e->caller))
	{
	  error ("comdat-local function called by %s outside its comdat",
		 identifier_to_locale (e->caller->name ()));
	  error_found = true;
	}
      if (!e->inline_failed)
	{
	  if (global.inlined_to
	      != (e->caller->global.inlined_to
		  ? e->caller->global.inlined_to : e->caller))
	    {
	      error ("inlined_to pointer is wrong");
	      error_found = true;
	    }
	  if (callers->next_caller)
	    {
	      error ("multiple inline callers");
	      error_found = true;
	    }
	}
      else
	if (global.inlined_to)
	  {
	    error ("inlined_to pointer set for noninline callers");
	    error_found = true;
	  }
    }
  for (e = callees; e; e = e->next_callee)
    {
      if (e->verify_count_and_frequency ())
	error_found = true;
      if (gimple_has_body_p (e->caller->decl)
	  && !e->caller->global.inlined_to
	  && !e->speculative
	  /* Optimized out calls are redirected to __builtin_unreachable.  */
	  && (e->frequency
	      || e->callee->decl
		 != builtin_decl_implicit (BUILT_IN_UNREACHABLE))
	  && (e->frequency
	      != compute_call_stmt_bb_frequency (e->caller->decl,
						 gimple_bb (e->call_stmt))))
	{
	  error ("caller edge frequency %i does not match BB frequency %i",
		 e->frequency,
		 compute_call_stmt_bb_frequency (e->caller->decl,
						 gimple_bb (e->call_stmt)));
	  error_found = true;
	}
    }
  for (e = indirect_calls; e; e = e->next_callee)
    {
      if (e->verify_count_and_frequency ())
	error_found = true;
      if (gimple_has_body_p (e->caller->decl)
	  && !e->caller->global.inlined_to
	  && !e->speculative
	  && (e->frequency
	      != compute_call_stmt_bb_frequency (e->caller->decl,
						 gimple_bb (e->call_stmt))))
	{
	  error ("indirect call frequency %i does not match BB frequency %i",
		 e->frequency,
		 compute_call_stmt_bb_frequency (e->caller->decl,
						 gimple_bb (e->call_stmt)));
	  error_found = true;
	}
    }
  if (!callers && global.inlined_to)
    {
      error ("inlined_to pointer is set but no predecessors found");
      error_found = true;
    }
  if (global.inlined_to == this)
    {
      error ("inlined_to pointer refers to itself");
      error_found = true;
    }

  if (clone_of)
    {
      cgraph_node *n;
      for (n = clone_of->clones; n; n = n->next_sibling_clone)
	if (n == this)
	  break;
      if (!n)
	{
	  error ("cgraph_node has wrong clone_of");
	  error_found = true;
	}
    }
  if (clones)
    {
      cgraph_node *n;
      for (n = clones; n; n = n->next_sibling_clone)
	if (n->clone_of != this)
	  break;
      if (n)
	{
	  error ("cgraph_node has wrong clone list");
	  error_found = true;
	}
    }
  if ((prev_sibling_clone || next_sibling_clone) && !clone_of)
    {
       error ("cgraph_node is in clone list but it is not clone");
       error_found = true;
    }
  if (!prev_sibling_clone && clone_of && clone_of->clones != this)
    {
      error ("cgraph_node has wrong prev_clone pointer");
      error_found = true;
    }
  if (prev_sibling_clone && prev_sibling_clone->next_sibling_clone != this)
    {
      error ("double linked list of clones corrupted");
      error_found = true;
    }

  if (analyzed && alias)
    {
      bool ref_found = false;
      int i;
      ipa_ref *ref = NULL;

      if (callees)
	{
	  error ("Alias has call edges");
          error_found = true;
	}
      for (i = 0; iterate_reference (i, ref); i++)
	if (ref->use == IPA_REF_CHKP)
	  ;
	else if (ref->use != IPA_REF_ALIAS)
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

  /* Check instrumented version reference.  */
  if (instrumented_version
      && instrumented_version->instrumented_version != this)
    {
      error ("Instrumentation clone does not reference original node");
      error_found = true;
    }

  /* Cannot have orig_decl for not instrumented nodes.  */
  if (!instrumentation_clone && orig_decl)
    {
      error ("Not instrumented node has non-NULL original declaration");
      error_found = true;
    }

  /* If original not instrumented node still exists then we may check
     original declaration is set properly.  */
  if (instrumented_version
      && orig_decl
      && orig_decl != instrumented_version->decl)
    {
      error ("Instrumented node has wrong original declaration");
      error_found = true;
    }

  /* Check all nodes have chkp reference to their instrumented versions.  */
  if (analyzed
      && instrumented_version
      && !instrumentation_clone)
    {
      bool ref_found = false;
      int i;
      struct ipa_ref *ref;

      for (i = 0; iterate_reference (i, ref); i++)
	if (ref->use == IPA_REF_CHKP)
	  {
	    if (ref_found)
	      {
		error ("Node has more than one chkp reference");
		error_found = true;
	      }
	    if (ref->referred != instrumented_version)
	      {
		error ("Wrong node is referenced with chkp reference");
		error_found = true;
	      }
	    ref_found = true;
	  }

      if (!ref_found)
	{
	  error ("Analyzed node has no reference to instrumented version");
	  error_found = true;
	}
    }

  if (instrumentation_clone
      && DECL_BUILT_IN_CLASS (decl) == NOT_BUILT_IN)
    {
      tree name = DECL_ASSEMBLER_NAME (decl);
      tree orig_name = DECL_ASSEMBLER_NAME (orig_decl);

      if (!IDENTIFIER_TRANSPARENT_ALIAS (name)
	  || TREE_CHAIN (name) != orig_name)
	{
	  error ("Alias chain for instrumented node is broken");
	  error_found = true;
	}
    }

  if (analyzed && thunk.thunk_p)
    {
      if (!callees)
	{
	  error ("No edge out of thunk node");
          error_found = true;
	}
      else if (callees->next_callee)
	{
	  error ("More than one edge out of thunk node");
          error_found = true;
	}
      if (gimple_has_body_p (decl))
        {
	  error ("Thunk is not supposed to have body");
          error_found = true;
        }
      if (thunk.add_pointer_bounds_args
	  && !instrumented_version->semantically_equivalent_p (callees->callee))
	{
	  error ("Instrumentation thunk has wrong edge callee");
          error_found = true;
	}
    }
  else if (analyzed && gimple_has_body_p (decl)
	   && !TREE_ASM_WRITTEN (decl)
	   && (!DECL_EXTERNAL (decl) || global.inlined_to)
	   && !flag_wpa)
    {
      if (this_cfun->cfg)
	{
	  hash_set<gimple *> stmts;
	  int i;
	  ipa_ref *ref = NULL;

	  /* Reach the trees by walking over the CFG, and note the
	     enclosing basic-blocks in the call edges.  */
	  FOR_EACH_BB_FN (this_block, this_cfun)
	    {
	      for (gsi = gsi_start_phis (this_block);
		   !gsi_end_p (gsi); gsi_next (&gsi))
		stmts.add (gsi_stmt (gsi));
	      for (gsi = gsi_start_bb (this_block);
		   !gsi_end_p (gsi);
		   gsi_next (&gsi))
		{
		  gimple *stmt = gsi_stmt (gsi);
		  stmts.add (stmt);
		  if (is_gimple_call (stmt))
		    {
		      cgraph_edge *e = get_edge (stmt);
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
			      if (e->verify_corresponds_to_fndecl (decl))
				{
				  error ("edge points to wrong declaration:");
				  debug_tree (e->callee->decl);
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
	    for (i = 0; iterate_reference (i, ref); i++)
	      if (ref->stmt && !stmts.contains (ref->stmt))
		{
		  error ("reference to dead statement");
		  cgraph_debug_gimple_stmt (this_cfun, ref->stmt);
		  error_found = true;
		}
	}
      else
	/* No CFG available?!  */
	gcc_unreachable ();

      for (e = callees; e; e = e->next_callee)
	{
	  if (!e->aux)
	    {
	      error ("edge %s->%s has no corresponding call_stmt",
		     identifier_to_locale (e->caller->name ()),
		     identifier_to_locale (e->callee->name ()));
	      cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
	      error_found = true;
	    }
	  e->aux = 0;
	}
      for (e = indirect_calls; e; e = e->next_callee)
	{
	  if (!e->aux && !e->speculative)
	    {
	      error ("an indirect edge from %s has no corresponding call_stmt",
		     identifier_to_locale (e->caller->name ()));
	      cgraph_debug_gimple_stmt (this_cfun, e->call_stmt);
	      error_found = true;
	    }
	  e->aux = 0;
	}
    }
  if (error_found)
    {
      dump (stderr);
      internal_error ("verify_cgraph_node failed");
    }
  timevar_pop (TV_CGRAPH_VERIFY);
}

/* Verify whole cgraph structure.  */
DEBUG_FUNCTION void
cgraph_node::verify_cgraph_nodes (void)
{
  cgraph_node *node;

  if (seen_error ())
    return;

  FOR_EACH_FUNCTION (node)
    node->verify ();
}

/* Walk the alias chain to return the function cgraph_node is alias of.
   Walk through thunks, too.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.
   When REF is non-NULL, assume that reference happens in symbol REF
   when determining the availability.  */

cgraph_node *
cgraph_node::function_symbol (enum availability *availability,
			      struct symtab_node *ref)
{
  cgraph_node *node = ultimate_alias_target (availability, ref);

  while (node->thunk.thunk_p)
    {
      ref = node;
      node = node->callees->callee;
      if (availability)
	{
	  enum availability a;
	  a = node->get_availability (ref);
	  if (a < *availability)
	    *availability = a;
	}
      node = node->ultimate_alias_target (availability, ref);
    }
  return node;
}

/* Walk the alias chain to return the function cgraph_node is alias of.
   Walk through non virtual thunks, too.  Thus we return either a function
   or a virtual thunk node.
   When AVAILABILITY is non-NULL, get minimal availability in the chain. 
   When REF is non-NULL, assume that reference happens in symbol REF
   when determining the availability.  */

cgraph_node *
cgraph_node::function_or_virtual_thunk_symbol
				(enum availability *availability,
				 struct symtab_node *ref)
{
  cgraph_node *node = ultimate_alias_target (availability, ref);

  while (node->thunk.thunk_p && !node->thunk.virtual_offset_p)
    {
      ref = node;
      node = node->callees->callee;
      if (availability)
	{
	  enum availability a;
	  a = node->get_availability (ref);
	  if (a < *availability)
	    *availability = a;
	}
      node = node->ultimate_alias_target (availability, ref);
    }
  return node;
}

/* When doing LTO, read cgraph_node's body from disk if it is not already
   present.  */

bool
cgraph_node::get_untransformed_body (void)
{
  lto_file_decl_data *file_data;
  const char *data, *name;
  size_t len;
  tree decl = this->decl;

  /* Check if body is already there.  Either we have gimple body or
     the function is thunk and in that case we set DECL_ARGUMENTS.  */
  if (DECL_ARGUMENTS (decl) || gimple_has_body_p (decl))
    return false;

  gcc_assert (in_lto_p && !DECL_RESULT (decl));

  timevar_push (TV_IPA_LTO_GIMPLE_IN);

  file_data = lto_file_data;
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  /* We may have renamed the declaration, e.g., a static function.  */
  name = lto_get_decl_name_mapping (file_data, name);
  struct lto_in_decl_state *decl_state
	 = lto_get_function_in_decl_state (file_data, decl);

  data = lto_get_section_data (file_data, LTO_section_function_body,
			       name, &len, decl_state->compressed);
  if (!data)
    fatal_error (input_location, "%s: section %s is missing",
		 file_data->file_name,
		 name);

  gcc_assert (DECL_STRUCT_FUNCTION (decl) == NULL);

  lto_input_function_body (file_data, this, data);
  lto_stats.num_function_bodies++;
  lto_free_section_data (file_data, LTO_section_function_body, name,
			 data, len, decl_state->compressed);
  lto_free_function_in_decl_state_for_node (this);
  /* Keep lto file data so ipa-inline-analysis knows about cross module
     inlining.  */

  timevar_pop (TV_IPA_LTO_GIMPLE_IN);

  return true;
}

/* Prepare function body.  When doing LTO, read cgraph_node's body from disk 
   if it is not already present.  When some IPA transformations are scheduled,
   apply them.  */

bool
cgraph_node::get_body (void)
{
  bool updated;

  updated = get_untransformed_body ();

  /* Getting transformed body makes no sense for inline clones;
     we should never use this on real clones because they are materialized
     early.
     TODO: Materializing clones here will likely lead to smaller LTRANS
     footprint. */
  gcc_assert (!global.inlined_to && !clone_of);
  if (ipa_transforms_to_apply.exists ())
    {
      opt_pass *saved_current_pass = current_pass;
      FILE *saved_dump_file = dump_file;
      const char *saved_dump_file_name = dump_file_name;
      int saved_dump_flags = dump_flags;
      dump_file_name = NULL;
      dump_file = NULL;

      push_cfun (DECL_STRUCT_FUNCTION (decl));
      execute_all_ipa_transforms ();
      cgraph_edge::rebuild_edges ();
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      pop_cfun ();
      updated = true;

      current_pass = saved_current_pass;
      dump_file = saved_dump_file;
      dump_file_name = saved_dump_file_name;
      dump_flags = saved_dump_flags;
    }
  return updated;
}

/* Return the DECL_STRUCT_FUNCTION of the function.  */

struct function *
cgraph_node::get_fun (void)
{
  cgraph_node *node = this;
  struct function *fun = DECL_STRUCT_FUNCTION (node->decl);

  while (!fun && node->clone_of)
    {
      node = node->clone_of;
      fun = DECL_STRUCT_FUNCTION (node->decl);
    }

  return fun;
}

/* Verify if the type of the argument matches that of the function
   declaration.  If we cannot verify this or there is a mismatch,
   return false.  */

static bool
gimple_check_call_args (gimple *stmt, tree fndecl, bool args_count_match)
{
  tree parms, p;
  unsigned int i, nargs;

  /* Calls to internal functions always match their signature.  */
  if (gimple_call_internal_p (stmt))
    return true;

  nargs = gimple_call_num_args (stmt);

  /* Get argument types for verification.  */
  if (fndecl)
    parms = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  else
    parms = TYPE_ARG_TYPES (gimple_call_fntype (stmt));

  /* Verify if the type of the argument matches that of the function
     declaration.  If we cannot verify this or there is a mismatch,
     return false.  */
  if (fndecl && DECL_ARGUMENTS (fndecl))
    {
      for (i = 0, p = DECL_ARGUMENTS (fndecl);
	   i < nargs;
	   i++, p = DECL_CHAIN (p))
	{
	  tree arg;
	  /* We cannot distinguish a varargs function from the case
	     of excess parameters, still deferring the inlining decision
	     to the callee is possible.  */
	  if (!p)
	    break;
	  arg = gimple_call_arg (stmt, i);
	  if (p == error_mark_node
	      || DECL_ARG_TYPE (p) == error_mark_node
	      || arg == error_mark_node
	      || (!types_compatible_p (DECL_ARG_TYPE (p), TREE_TYPE (arg))
		  && !fold_convertible_p (DECL_ARG_TYPE (p), arg)))
            return false;
	}
      if (args_count_match && p)
	return false;
    }
  else if (parms)
    {
      for (i = 0, p = parms; i < nargs; i++, p = TREE_CHAIN (p))
	{
	  tree arg;
	  /* If this is a varargs function defer inlining decision
	     to callee.  */
	  if (!p)
	    break;
	  arg = gimple_call_arg (stmt, i);
	  if (TREE_VALUE (p) == error_mark_node
	      || arg == error_mark_node
	      || TREE_CODE (TREE_VALUE (p)) == VOID_TYPE
	      || (!types_compatible_p (TREE_VALUE (p), TREE_TYPE (arg))
		  && !fold_convertible_p (TREE_VALUE (p), arg)))
            return false;
	}
    }
  else
    {
      if (nargs != 0)
        return false;
    }
  return true;
}

/* Verify if the type of the argument and lhs of CALL_STMT matches
   that of the function declaration CALLEE. If ARGS_COUNT_MATCH is
   true, the arg count needs to be the same.
   If we cannot verify this or there is a mismatch, return false.  */

bool
gimple_check_call_matching_types (gimple *call_stmt, tree callee,
				  bool args_count_match)
{
  tree lhs;

  if ((DECL_RESULT (callee)
       && !DECL_BY_REFERENCE (DECL_RESULT (callee))
       && (lhs = gimple_call_lhs (call_stmt)) != NULL_TREE
       && !useless_type_conversion_p (TREE_TYPE (DECL_RESULT (callee)),
                                      TREE_TYPE (lhs))
       && !fold_convertible_p (TREE_TYPE (DECL_RESULT (callee)), lhs))
      || !gimple_check_call_args (call_stmt, callee, args_count_match))
    return false;
  return true;
}

/* Reset all state within cgraph.c so that we can rerun the compiler
   within the same process.  For use by toplev::finalize.  */

void
cgraph_c_finalize (void)
{
  symtab = NULL;

  x_cgraph_nodes_queue = NULL;

  cgraph_fnver_htab = NULL;
  version_info_node = NULL;
}

/* A wroker for call_for_symbol_and_aliases.  */

bool
cgraph_node::call_for_symbol_and_aliases_1 (bool (*callback) (cgraph_node *,
							      void *),
					    void *data,
					    bool include_overwritable)
{
  ipa_ref *ref;
  FOR_EACH_ALIAS (this, ref)
    {
      cgraph_node *alias = dyn_cast <cgraph_node *> (ref->referring);
      if (include_overwritable
	  || alias->get_availability () > AVAIL_INTERPOSABLE)
	if (alias->call_for_symbol_and_aliases (callback, data,
						include_overwritable))
	  return true;
    }
  return false;
}

/* Return true if NODE has thunk.  */

bool
cgraph_node::has_thunk_p (cgraph_node *node, void *)
{
  for (cgraph_edge *e = node->callers; e; e = e->next_caller)
    if (e->caller->thunk.thunk_p)
      return true;
  return false;
}

#include "gt-cgraph.h"
