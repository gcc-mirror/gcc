/* Callgraph handling code.
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "langhooks.h"
#include "hashtab.h"
#include "toplev.h"
#include "flags.h"
#include "ggc.h"
#include "debug.h"
#include "target.h"
#include "cgraph.h"
#include "varray.h"
#include "output.h"
#include "intl.h"


/* Hash table used to convert declarations into nodes.  */
static GTY((param_is (struct cgraph_node))) htab_t cgraph_hash;

/* The linked list of cgraph nodes.  */
struct cgraph_node *cgraph_nodes;

/* Queue of cgraph nodes scheduled to be lowered.  */
struct cgraph_node *cgraph_nodes_queue;

/* Number of nodes in existence.  */
int cgraph_n_nodes;

/* Maximal uid used in cgraph nodes.  */
int cgraph_max_uid;

/* Set when whole unit has been analyzed so we can access global info.  */
bool cgraph_global_info_ready = false;

/* Hash table used to convert declarations into nodes.  */
static GTY((param_is (struct cgraph_varpool_node))) htab_t cgraph_varpool_hash;

/* Queue of cgraph nodes scheduled to be lowered and output.  */
struct cgraph_varpool_node *cgraph_varpool_nodes_queue;

/* Number of nodes in existence.  */
int cgraph_varpool_n_nodes;

/* The linked list of cgraph varpool nodes.  */
static GTY(())  struct cgraph_varpool_node *cgraph_varpool_nodes;

static struct cgraph_edge *create_edge (struct cgraph_node *,
					struct cgraph_node *);
static hashval_t hash_node (const void *);
static int eq_node (const void *, const void *);

/* Returns a hash code for P.  */

static hashval_t
hash_node (const void *p)
{
  return ((hashval_t)
	  IDENTIFIER_HASH_VALUE (DECL_ASSEMBLER_NAME
				 (((struct cgraph_node *) p)->decl)));
}

/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_node (const void *p1, const void *p2)
{
  return ((DECL_ASSEMBLER_NAME (((struct cgraph_node *) p1)->decl)) ==
	  (tree) p2);
}

/* Return cgraph node assigned to DECL.  Create new one when needed.  */
struct cgraph_node *
cgraph_node (tree decl)
{
  struct cgraph_node *node;
  struct cgraph_node **slot;

  if (TREE_CODE (decl) != FUNCTION_DECL)
    abort ();

  if (!cgraph_hash)
    cgraph_hash = htab_create_ggc (10, hash_node, eq_node, NULL);

  slot = (struct cgraph_node **)
    htab_find_slot_with_hash (cgraph_hash, DECL_ASSEMBLER_NAME (decl),
			      IDENTIFIER_HASH_VALUE
			        (DECL_ASSEMBLER_NAME (decl)), INSERT);
  if (*slot)
    return *slot;
  node = ggc_alloc_cleared (sizeof (*node));
  node->decl = decl;
  node->next = cgraph_nodes;
  node->uid = cgraph_max_uid++;
  if (cgraph_nodes)
    cgraph_nodes->previous = node;
  node->previous = NULL;
  cgraph_nodes = node;
  cgraph_n_nodes++;
  *slot = node;
  if (DECL_CONTEXT (decl) && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL)
    {
      node->origin = cgraph_node (DECL_CONTEXT (decl));
      node->next_nested = node->origin->nested;
      node->origin->nested = node;
    }
  return node;
}

/* Try to find existing function for identifier ID.  */
struct cgraph_node *
cgraph_node_for_identifier (tree id)
{
  struct cgraph_node **slot;

  if (TREE_CODE (id) != IDENTIFIER_NODE)
    abort ();

  if (!cgraph_hash)
    return NULL;

  slot = (struct cgraph_node **)
    htab_find_slot_with_hash (cgraph_hash, id,
			      IDENTIFIER_HASH_VALUE (id), NO_INSERT);
  if (!slot)
    return NULL;
  return *slot;
}

/* Create edge from CALLER to CALLEE in the cgraph.  */

static struct cgraph_edge *
create_edge (struct cgraph_node *caller, struct cgraph_node *callee)
{
  struct cgraph_edge *edge = ggc_alloc (sizeof (struct cgraph_edge));
  struct cgraph_edge *edge2;

  if (!DECL_SAVED_TREE (callee->decl))
    edge->inline_failed = N_("function body not available");
  else if (callee->local.redefined_extern_inline)
    edge->inline_failed = N_("redefined extern inline functions are not "
			     "considered for inlining");
  else if (callee->local.inlinable)
    edge->inline_failed = N_("function not considered for inlining");
  else
    edge->inline_failed = N_("function not inlinable");

  /* At the moment we don't associate calls with specific CALL_EXPRs
     as we probably ought to, so we must preserve inline_call flags to
     be the same in all copies of the same edge.  */
  if (cgraph_global_info_ready)
    for (edge2 = caller->callees; edge2; edge2 = edge2->next_callee)
      if (edge2->callee == callee)
	{
	  edge->inline_failed = edge2->inline_failed;
	  break;
	}

  edge->caller = caller;
  edge->callee = callee;
  edge->next_caller = callee->callers;
  edge->next_callee = caller->callees;
  caller->callees = edge;
  callee->callers = edge;
  return edge;
}

/* Remove the edge from CALLER to CALLEE in the cgraph.  */

void
cgraph_remove_edge (struct cgraph_node *caller, struct cgraph_node *callee)
{
  struct cgraph_edge **edge, **edge2;

  for (edge = &callee->callers; *edge && (*edge)->caller != caller;
       edge = &((*edge)->next_caller))
    continue;
  if (!*edge)
    abort ();
  *edge = (*edge)->next_caller;
  for (edge2 = &caller->callees; *edge2 && (*edge2)->callee != callee;
       edge2 = &(*edge2)->next_callee)
    continue;
  if (!*edge2)
    abort ();
  *edge2 = (*edge2)->next_callee;
}

/* Remove the node from cgraph.  */

void
cgraph_remove_node (struct cgraph_node *node)
{
  void **slot;
  while (node->callers)
    cgraph_remove_edge (node->callers->caller, node);
  while (node->callees)
    cgraph_remove_edge (node, node->callees->callee);
  while (node->nested)
    cgraph_remove_node (node->nested);
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
  DECL_SAVED_TREE (node->decl) = NULL;
  DECL_SAVED_INSNS (node->decl) = NULL;
  DECL_ARGUMENTS (node->decl) = NULL;
  DECL_INITIAL (node->decl) = error_mark_node;
  slot = 
    htab_find_slot_with_hash (cgraph_hash, DECL_ASSEMBLER_NAME (node->decl),
			      IDENTIFIER_HASH_VALUE (DECL_ASSEMBLER_NAME
						     (node->decl)), NO_INSERT);
  if (slot == 0)
    {
      /* We use DECL_ASSEMBLER_NAME as key, which may not work in
	 all cases. See PR/15666. Gcc 3.5 uses DECL_UID as key,
	 which doesn't have this problem.  */
      if (!DECL_BUILT_IN (node->decl))
	abort ();
    }
  else
    htab_clear_slot (cgraph_hash, slot);
  /* Do not free the structure itself so the walk over chain can continue.  */
}

/* Notify finalize_compilation_unit that given node is reachable.  */

void
cgraph_mark_reachable_node (struct cgraph_node *node)
{
  if (!node->reachable && node->local.finalized)
    {
      notice_global_symbol (node->decl);
      node->reachable = 1;

      node->next_needed = cgraph_nodes_queue;
      cgraph_nodes_queue = node;

      /* At the moment frontend automatically emits all nested functions.  */
      if (node->nested)
	{
	  struct cgraph_node *node2;

	  for (node2 = node->nested; node2; node2 = node2->next_nested)
	    if (!node2->reachable)
	      cgraph_mark_reachable_node (node2);
	}
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

/* Record call from CALLER to CALLEE.  */

struct cgraph_edge *
cgraph_record_call (tree caller, tree callee)
{
  return create_edge (cgraph_node (caller), cgraph_node (callee));
}

void
cgraph_remove_call (tree caller, tree callee)
{
  cgraph_remove_edge (cgraph_node (caller), cgraph_node (callee));
}

/* Return true when CALLER_DECL calls CALLEE_DECL.  */

bool
cgraph_calls_p (tree caller_decl, tree callee_decl)
{
  struct cgraph_node *caller = cgraph_node (caller_decl);
  struct cgraph_node *callee = cgraph_node (callee_decl);
  struct cgraph_edge *edge;

  for (edge = callee->callers; edge && (edge)->caller != caller;
       edge = (edge->next_caller))
    continue;
  return edge != NULL;
}

/* Return local info for the compiled function.  */

struct cgraph_local_info *
cgraph_local_info (tree decl)
{
  struct cgraph_node *node;
  if (TREE_CODE (decl) != FUNCTION_DECL)
    abort ();
  node = cgraph_node (decl);
  return &node->local;
}

/* Return local info for the compiled function.  */

struct cgraph_global_info *
cgraph_global_info (tree decl)
{
  struct cgraph_node *node;
  if (TREE_CODE (decl) != FUNCTION_DECL || !cgraph_global_info_ready)
    abort ();
  node = cgraph_node (decl);
  return &node->global;
}

/* Return local info for the compiled function.  */

struct cgraph_rtl_info *
cgraph_rtl_info (tree decl)
{
  struct cgraph_node *node;
  if (TREE_CODE (decl) != FUNCTION_DECL)
    abort ();
  node = cgraph_node (decl);
  if (decl != current_function_decl
      && !TREE_ASM_WRITTEN (node->decl))
    return NULL;
  return &node->rtl;
}

/* Return name of the node used in debug output.  */
const char *
cgraph_node_name (struct cgraph_node *node)
{
  return (*lang_hooks.decl_printable_name) (node->decl, 2);
}

/* Dump the callgraph.  */

void
dump_cgraph (FILE *f)
{
  struct cgraph_node *node;

  fprintf (f, "callgraph:\n\n");
  for (node = cgraph_nodes; node; node = node->next)
    {
      struct cgraph_edge *edge;
      fprintf (f, "%s:", cgraph_node_name (node));
      if (node->local.self_insns)
        fprintf (f, " %i insns", node->local.self_insns);
      if (node->global.insns && node->global.insns != node->local.self_insns)
	fprintf (f, " (%i after inlining)", node->global.insns);
      if (node->origin)
	fprintf (f, " nested in: %s", cgraph_node_name (node->origin));
      if (node->needed)
	fprintf (f, " needed");
      else if (node->reachable)
	fprintf (f, " reachable");
      if (DECL_SAVED_TREE (node->decl))
	fprintf (f, " tree");

      if (node->local.local)
	fprintf (f, " local");
      if (node->local.disregard_inline_limits)
	fprintf (f, " always_inline");
      else if (node->local.inlinable)
	fprintf (f, " inlinable");
      if (node->global.cloned_times > 1)
	fprintf (f, " cloned %ix", node->global.cloned_times);

      fprintf (f, "\n  called by: ");
      for (edge = node->callers; edge; edge = edge->next_caller)
	{
	  fprintf (f, "%s ", cgraph_node_name (edge->caller));
	  if (!edge->inline_failed)
	    fprintf(f, "(inlined) ");
	}

      fprintf (f, "\n  calls: ");
      for (edge = node->callees; edge; edge = edge->next_callee)
	{
	  fprintf (f, "%s ", cgraph_node_name (edge->callee));
	  if (!edge->inline_failed)
	    fprintf(f, "(inlined) ");
	}
      fprintf (f, "\n");
    }
}

/* Returns a hash code for P.  */

static hashval_t
cgraph_varpool_hash_node (const void *p)
{
  return ((hashval_t)
	  IDENTIFIER_HASH_VALUE (DECL_ASSEMBLER_NAME
				 (((struct cgraph_varpool_node *) p)->decl)));
}

/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_cgraph_varpool_node (const void *p1, const void *p2)
{
  return ((DECL_ASSEMBLER_NAME (((struct cgraph_varpool_node *) p1)->decl)) ==
	  (tree) p2);
}

/* Return cgraph_varpool node assigned to DECL.  Create new one when needed.  */
struct cgraph_varpool_node *
cgraph_varpool_node (tree decl)
{
  struct cgraph_varpool_node *node;
  struct cgraph_varpool_node **slot;

  if (!DECL_P (decl) || TREE_CODE (decl) == FUNCTION_DECL)
    abort ();

  if (!cgraph_varpool_hash)
    cgraph_varpool_hash = htab_create_ggc (10, cgraph_varpool_hash_node,
				           eq_cgraph_varpool_node, NULL);
  slot = (struct cgraph_varpool_node **)
    htab_find_slot_with_hash (cgraph_varpool_hash, DECL_ASSEMBLER_NAME (decl),
			      IDENTIFIER_HASH_VALUE (DECL_ASSEMBLER_NAME (decl)),
			      INSERT);
  if (*slot)
    return *slot;
  node = ggc_alloc_cleared (sizeof (*node));
  node->decl = decl;
  cgraph_varpool_n_nodes++;
  cgraph_varpool_nodes = node;
  *slot = node;
  return node;
}

/* Set the DECL_ASSEMBLER_NAME and update cgraph hashtables.  */
void
change_decl_assembler_name (tree decl, tree name)
{
  struct cgraph_node *node = NULL;
  struct cgraph_varpool_node *vnode = NULL;
  void **slot;

  if (!DECL_ASSEMBLER_NAME_SET_P (decl))
    {
      SET_DECL_ASSEMBLER_NAME (decl, name);
      return;
    }
  if (name == DECL_ASSEMBLER_NAME (decl))
    return;

  if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))
      && DECL_RTL_SET_P (decl))
    warning ("%D renamed after being referenced in assembly", decl);

  if (TREE_CODE (decl) == FUNCTION_DECL && cgraph_hash)
    {
      /* Take a look whether declaration is in the cgraph structure.  */
      slot = 
	htab_find_slot_with_hash (cgraph_hash, DECL_ASSEMBLER_NAME (decl),
				   IDENTIFIER_HASH_VALUE (DECL_ASSEMBLER_NAME
							  (decl)), NO_INSERT);
      if (slot)
	node = *slot;

      /* It is, verify that we are the canonical node for this decl.  */
      if (node && node->decl == decl)
	{
	  node = *slot;
	  htab_clear_slot (cgraph_hash, slot);
      	 }
       else
	 node = NULL;
    }
  if (TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl) && cgraph_varpool_hash)
    {
      /* Take a look whether declaration is in the cgraph structure.  */
      slot = 
	htab_find_slot_with_hash (cgraph_varpool_hash, DECL_ASSEMBLER_NAME (decl),
				   IDENTIFIER_HASH_VALUE (DECL_ASSEMBLER_NAME
							  (decl)), NO_INSERT);
      if (slot)
	vnode = *slot;

      /* It is, verify that we are the canonical vnode for this decl.  */
      if (vnode && vnode->decl == decl)
	{
	  vnode = *slot;
	  htab_clear_slot (cgraph_varpool_hash, slot);
      	 }
       else
	 vnode = NULL;
    }
  SET_DECL_ASSEMBLER_NAME (decl, name);
  if (node)
    {
      slot = 
	htab_find_slot_with_hash (cgraph_hash, name,
				  IDENTIFIER_HASH_VALUE (name), INSERT);
      if (*slot)
	abort ();
      *slot = node;
    }
  if (vnode)
    {
      slot = 
	htab_find_slot_with_hash (cgraph_varpool_hash, name,
				  IDENTIFIER_HASH_VALUE (name), INSERT);
      if (*slot)
	abort ();
      *slot = vnode;
    }
}

/* Try to find existing function for identifier ID.  */
struct cgraph_varpool_node *
cgraph_varpool_node_for_identifier (tree id)
{
  struct cgraph_varpool_node **slot;

  if (TREE_CODE (id) != IDENTIFIER_NODE)
    abort ();

  if (!cgraph_varpool_hash)
    return NULL;

  slot = (struct cgraph_varpool_node **)
    htab_find_slot_with_hash (cgraph_varpool_hash, id,
			      IDENTIFIER_HASH_VALUE (id), NO_INSERT);
  if (!slot)
    return NULL;
  return *slot;
}

/* Notify finalize_compilation_unit that given node is reachable
   or needed.  */
void
cgraph_varpool_mark_needed_node (struct cgraph_varpool_node *node)
{
  if (!node->needed && node->finalized)
    {
      node->next_needed = cgraph_varpool_nodes_queue;
      cgraph_varpool_nodes_queue = node;
      notice_global_symbol (node->decl);
    }
  node->needed = 1;
}

void
cgraph_varpool_finalize_decl (tree decl)
{
  struct cgraph_varpool_node *node = cgraph_varpool_node (decl);
 
  /* The first declaration of a variable that comes through this function
     decides whether it is global (in C, has external linkage)
     or local (in C, has internal linkage).  So do nothing more
     if this function has already run.  */
  if (node->finalized)
    return;
  if (node->needed)
    {
      node->next_needed = cgraph_varpool_nodes_queue;
      cgraph_varpool_nodes_queue = node;
      notice_global_symbol (decl);
    }
  node->finalized = true;

  if (/* Externally visible variables must be output.  The exception are
	 COMDAT functions that must be output only when they are needed.  */
      (TREE_PUBLIC (decl) && !DECL_COMDAT (decl))
      /* Function whose name is output to the assembler file must be produced.
	 It is possible to assemble the name later after finalizing the function
	 and the fact is noticed in assemble_name then.  */
      || (DECL_ASSEMBLER_NAME_SET_P (decl)
	  && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))))
    {
      cgraph_varpool_mark_needed_node (node);
    }
}

bool
cgraph_varpool_assemble_pending_decls (void)
{
  bool changed = false;

  while (cgraph_varpool_nodes_queue)
    {
      tree decl = cgraph_varpool_nodes_queue->decl;
      struct cgraph_varpool_node *node = cgraph_varpool_nodes_queue;

      cgraph_varpool_nodes_queue = cgraph_varpool_nodes_queue->next_needed;
      if (!TREE_ASM_WRITTEN (decl))
	{
	  assemble_variable (decl, 0, 1, 0);
	  changed = true;
	}
      node->next_needed = NULL;
    }
  return changed;
}

/* Return true when the DECL can possibly be inlined.  */
bool
cgraph_function_possibly_inlined_p (tree decl)
{
  if (!cgraph_global_info_ready)
    return (DECL_INLINE (decl)
	    && (!flag_really_no_inline
		|| (*lang_hooks.tree_inlining.disregard_inline_limits) (decl)));
  return cgraph_node (decl)->global.inlined;
}

#include "gt-cgraph.h"
