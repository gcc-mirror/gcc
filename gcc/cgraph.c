/* Callgraph handling code.
   Copyright (C) 2003 Free Software Foundation, Inc.
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
#include "tree-inline.h"
#include "langhooks.h"
#include "hashtab.h"
#include "toplev.h"
#include "flags.h"
#include "ggc.h"
#include "debug.h"
#include "target.h"

/* The cgraph data strutcture.
   Each function decl has assigned cgraph_node listing calees and callers.  */

struct cgraph_node
{
  tree decl;
  struct cgraph_edge *callees;
  struct cgraph_edge *callers;
  struct cgraph_node *next;
  /* For nested functions points to function the node is nested in.  */
  struct cgraph_node *origin;
  /* Points to first nested function, if any.  */
  struct cgraph_node *nested;
  /* Pointer to the next function with same origin, if any.  */
  struct cgraph_node *next_nested;
  void *aux;

  /* Set when function must be output - it is externally visible
     or it's address is taken.  */
  bool needed;
  /* Set when function is reachable by call from other function
     that is eighter reachable or needed.  */
  bool reachable;
  /* Set when the frontend has been asked to lower representation of this
     function into trees.  Callees lists are not available when lowered
     is not set.  */
  bool lowered;
  /* Set when function is scheduled to be assembled.  */
  bool output;
};

struct cgraph_edge
{
  struct cgraph_node *caller, *callee;
  struct cgraph_edge *next_caller;
  struct cgraph_edge *next_callee;
};

/* Hash table used to convert declarations into nodes.  */
static htab_t cgraph_hash = 0;

/* The linked list of cgraph nodes.  */
static struct cgraph_node *cgraph_nodes;

/* Number of nodes in existence.  */
static int cgraph_n_nodes;

static struct cgraph_node *cgraph_node PARAMS ((tree decl));
static struct cgraph_edge *create_edge PARAMS ((struct cgraph_node *,
						struct cgraph_node *));
static void remove_edge PARAMS ((struct cgraph_node *, struct cgraph_node *));
static struct cgraph_edge *record_call PARAMS ((tree, tree));
static tree record_call_1 PARAMS ((tree *, int *, void *));
static hashval_t hash_node PARAMS ((const PTR));
static int eq_node PARAMS ((const PTR, const PTR));
static struct cgraph_node *cgraph_node PARAMS ((tree));
static void cgraph_expand_functions PARAMS ((void));
static void cgraph_mark_functions_to_output PARAMS ((void));
static void cgraph_expand_function PARAMS ((struct cgraph_node *));
static void cgraph_mark_needed_node PARAMS ((struct cgraph_node *, int));

/* Returns a hash code for P.  */

static hashval_t
hash_node (p)
     const PTR p;
{
  return (hashval_t)
    htab_hash_pointer (DECL_ASSEMBLER_NAME
		       (((struct cgraph_node *) p)->decl));
}

/* Returns non-zero if P1 and P2 are equal.  */

static int
eq_node (p1, p2)
     const PTR p1;
     const PTR p2;
{
  return ((DECL_ASSEMBLER_NAME (((struct cgraph_node *) p1)->decl)) ==
	  DECL_ASSEMBLER_NAME ((tree) p2));
}

/* Return cgraph node assigned to DECL.  Create new one when needed.  */
static struct cgraph_node *
cgraph_node (decl)
     tree decl;
{
  struct cgraph_node *node;
  struct cgraph_node **slot;

  if (!cgraph_hash)
    cgraph_hash = htab_create (10, hash_node, eq_node, NULL);

  slot =
    (struct cgraph_node **) htab_find_slot_with_hash (cgraph_hash, decl,
						      htab_hash_pointer
						      (DECL_ASSEMBLER_NAME
						       (decl)), 1);
  if (*slot)
    return *slot;
  node = xcalloc (sizeof (*node), 1);
  node->decl = decl;
  node->next = cgraph_nodes;
  cgraph_nodes = node;
  cgraph_n_nodes++;
  *slot = node;
  if (DECL_CONTEXT (decl))
    {
      node->origin = cgraph_node (DECL_CONTEXT (decl));
      node->next_nested = node->origin->nested;
      node->origin->nested = node;
    }
  return node;
}

/* Create edge from CALLER to CALLEE in the cgraph.  */

static struct cgraph_edge *
create_edge (caller, callee)
     struct cgraph_node *caller, *callee;
{
  struct cgraph_edge *edge = xmalloc (sizeof (struct cgraph_edge));

  edge->caller = caller;
  edge->callee = callee;
  edge->next_caller = callee->callers;
  edge->next_callee = caller->callees;
  caller->callees = edge;
  callee->callers = edge;
  return edge;
}

/* Remove the edge from CALLER to CALLEE in the cgraph.  */

static void
remove_edge (caller, callee)
     struct cgraph_node *caller, *callee;
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

/* Record call from CALLER to CALLEE  */

static struct cgraph_edge *
record_call (caller, callee)
     tree caller, callee;
{
  return create_edge (cgraph_node (caller), cgraph_node (callee));
}

void
cgraph_remove_call (caller, callee)
     tree caller, callee;
{
  remove_edge (cgraph_node (caller), cgraph_node (callee));
}

/* Return true when CALLER_DECL calls CALLEE_DECL.  */

bool
cgraph_calls_p (caller_decl, callee_decl)
     tree caller_decl, callee_decl;
{
  struct cgraph_node *caller = cgraph_node (caller_decl);
  struct cgraph_node *callee = cgraph_node (callee_decl);
  struct cgraph_edge *edge;

  for (edge = callee->callers; edge && (edge)->caller != caller;
       edge = (edge->next_caller))
    continue;
  return edge != NULL;
}

/* Walk tree and record all calls.  Called via walk_tree.  */
static tree
record_call_1 (tp, walk_subtrees, data)
     tree *tp;
     int *walk_subtrees;
     void *data;
{
  /* Record dereferences to the functions.  This makes the functions
     reachable unconditionally.  */
  if (TREE_CODE (*tp) == ADDR_EXPR)
    {
      tree decl = TREE_OPERAND (*tp, 0);
      if (TREE_CODE (decl) == FUNCTION_DECL)
        cgraph_mark_needed_node (cgraph_node (decl), 1);
    }
  else if (TREE_CODE (*tp) == CALL_EXPR)
    {
      tree decl = TREE_OPERAND (*tp, 0);
      if (TREE_CODE (decl) == ADDR_EXPR)
	decl = TREE_OPERAND (decl, 0);
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  if (DECL_BUILT_IN (decl))
	    return NULL;
	  record_call (data, decl);
	  walk_tree (&TREE_OPERAND (*tp, 1), record_call_1, data, NULL);
	  *walk_subtrees = 0;
	}
    }
  return NULL;
}

/* Create cgraph edges for function calles via BODY.  */

void
cgraph_create_edges (decl, body)
     tree decl;
     tree body;
{
  walk_tree (&body, record_call_1, decl, NULL);
}

/* Analyze function once it is parsed.  Set up the local information
   available - create cgraph edges for function calles via BODY.  */

void
cgraph_finalize_function (decl, body)
     tree decl;
     tree body ATTRIBUTE_UNUSED;
{
  struct cgraph_node *node = cgraph_node (decl);

  node->decl = decl;

  /* Set TREE_UNINLINABLE flag.  */
  tree_inlinable_function_p (decl);

  (*debug_hooks->deferred_inline_function) (decl);
}

/* Dump the callgraph.  */

void
dump_cgraph (f)
     FILE *f;
{
  struct cgraph_node *node;

  fprintf (f, "\nCallgraph:\n\n");
  for (node = cgraph_nodes; node; node = node->next)
    {
      struct cgraph_edge *edge;
      fprintf (f, "%s", IDENTIFIER_POINTER (DECL_NAME (node->decl)));
      if (node->origin)
	fprintf (f, " nested in: %s",
		 IDENTIFIER_POINTER (DECL_NAME (node->origin->decl)));
      if (node->needed)
	fprintf (f, " needed");
      else if (node->reachable)
	fprintf (f, " reachable");
      if (DECL_SAVED_TREE (node->decl))
	fprintf (f, " tree");

      fprintf (f, "\n  called by :");
      for (edge = node->callers; edge; edge = edge->next_caller)
	fprintf (f, "%s ",
		 IDENTIFIER_POINTER (DECL_NAME (edge->caller->decl)));

      fprintf (f, "\n  calls: ");
      for (edge = node->callees; edge; edge = edge->next_callee)
	fprintf (f, "%s ",
		 IDENTIFIER_POINTER (DECL_NAME (edge->callee->decl)));
      fprintf (f, "\n");
    }
}

static struct cgraph_node *queue = NULL;

/* Notify finalize_compilation_unit that given node is reachable
   or needed.  */
static void
cgraph_mark_needed_node (node, needed)
     struct cgraph_node *node;
     int needed;
{
  if (needed)
    {
      if (DECL_SAVED_TREE (node->decl))
        announce_function (node->decl);
      node->needed = 1;
    }
  if (!node->reachable)
    {
      node->reachable = 1;
      if (DECL_SAVED_TREE (node->decl))
	{
	  node->aux = queue;
	  queue = node;
        }
    }
}

/* Analyze the whole compilation unit once it is parsed completely.  */

void
cgraph_finalize_compilation_unit ()
{
  struct cgraph_node *node;
  struct cgraph_edge *edge;

  /* Collect entry points to the unit.  */

  if (!quiet_flag)
    fprintf (stderr, "\n\nUnit entry points:");

  for (node = cgraph_nodes; node; node = node->next)
    {
      tree decl = node->decl;

      if (!DECL_SAVED_TREE (decl))
	continue;
      if ((TREE_PUBLIC (decl) && !DECL_COMDAT (decl) && !DECL_EXTERNAL (decl))
	  || (DECL_ASSEMBLER_NAME_SET_P (decl)
	      && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))))
	{
          cgraph_mark_needed_node (node, 1);
	}
    }

  /*  Propagate reachability flag and lower representation of all reachable
      functions.  In the future, lowering will introduce new functions and
      new entry points on the way (by template instantiation and virtual
      method table generation for instance).  */
  while (queue)
    {
      tree decl = queue->decl;

      node = queue;
      queue = queue->aux;
      if (node->lowered || !node->reachable || !DECL_SAVED_TREE (decl))
	abort ();

      /* At the moment frontend automatically emits all nested functions.  */
      if (node->nested)
	{
	  struct cgraph_node *node2;

	  for (node2 = node->nested; node2; node2 = node2->next_nested)
	    if (!node2->reachable)
	      cgraph_mark_needed_node (node2, 0);
	}

      if (lang_hooks.callgraph.lower_function)
	(*lang_hooks.callgraph.lower_function) (decl);
      /* First kill forward declaration so reverse inling works properly.  */
      cgraph_create_edges (decl, DECL_SAVED_TREE (decl));

      for (edge = node->callees; edge; edge = edge->next_callee)
	{
	  if (!edge->callee->reachable)
            cgraph_mark_needed_node (edge->callee, 0);
	}
      node->lowered = true;
    }
  if (!quiet_flag)
    fprintf (stderr, "\n\nReclaiming functions:");

  for (node = cgraph_nodes; node; node = node->next)
    {
      tree decl = node->decl;

      if (!node->reachable && DECL_SAVED_TREE (decl))
	{
	  DECL_SAVED_TREE (decl) = NULL;
	  announce_function (decl);
	}
    }
  ggc_collect ();
}

/* Expand all functions that must be output.  */

#define NPREDECESORS(node) (size_t)((node)->aux)
#define SET_NPREDECESORS(node,n) (node)->aux = (void *) (n);

/* Figure out what functions we want to assemble.  */

static void
cgraph_mark_functions_to_output ()
{
  struct cgraph_node *node;

  /* Figure out functions we want to assemble.  */
  for (node = cgraph_nodes; node; node = node->next)
    {
      tree decl = node->decl;

      if (DECL_SAVED_TREE (decl)
	  && (node->needed
	      || (DECL_UNINLINABLE (decl) && node->reachable)
	      || TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
	  && !TREE_ASM_WRITTEN (decl) && !node->origin
	  && !DECL_EXTERNAL (decl))
	node->output = 1;
    }
}

/* Expand function specified by NODE.  */
static void
cgraph_expand_function (node)
     struct cgraph_node *node;
{
  tree decl = node->decl;

  announce_function (decl);
  if (flag_inline_trees)
    optimize_inline_calls (decl);
  (*lang_hooks.callgraph.expand_function) (decl);
  if (DECL_UNINLINABLE (decl))
    DECL_SAVED_TREE (decl) = NULL;
  current_function_decl = NULL;
}


/* Expand all functions that must be output. 
  
   Attempt to topologically sort the nodes so function is output when
   all called functions are already assembled to allow data to be propagated
   accross the callgraph.  Use stack to get smaller distance between function
   and it's callees (later we may use more sophisticated algorithm for
   function reordering, we will likely want to use subsections to make output
   functions to appear in top-down order, not bottom-up they are assembled).  */

static void
cgraph_expand_functions ()
{
  struct cgraph_node *node;
  struct cgraph_node **stack =
    xcalloc (sizeof (struct cgraph_node *), cgraph_n_nodes);
  int stack_size = 0;
  struct cgraph_edge *edge;

  cgraph_mark_functions_to_output ();

  for (node = cgraph_nodes; node; node = node->next)
    if (node->output)
      {
	int n = 0;
	for (edge = node->callees; edge; edge = edge->next_callee)
	  if (edge->callee->output)
	    n++;
	SET_NPREDECESORS (node, n);
	if (n == 0)
	  stack[stack_size++] = node;
      }
  while (1)
    {
      struct cgraph_node *minnode;
      while (stack_size)
	{
	  node = stack[--stack_size];
	  node->output = 0;

	  for (edge = node->callers; edge; edge = edge->next_caller)
	    if (edge->caller->output)
	      {
	        SET_NPREDECESORS (edge->caller,
		    		  NPREDECESORS (edge->caller) - 1);
		if (!NPREDECESORS (edge->caller))
		  stack[stack_size++] = edge->caller;
	      }
	  if (!node->reachable)
	    abort ();
	  cgraph_expand_function (node);
	}
      minnode = NULL;
      /* We found cycle.  Break it and try again.  */
      for (node = cgraph_nodes; node; node = node->next)
	if (node->output
	    && (!minnode
	        || NPREDECESORS (minnode) > NPREDECESORS (node)))
	  minnode = node;
      if (!minnode)
	return;
      stack[stack_size++] = minnode;
    }
}

/* Perform simple optimizations based on callgraph.  */

void
cgraph_optimize ()
{
  struct cgraph_node *node;
  bool changed = true;
  struct cgraph_edge *edge;

  if (!quiet_flag)
    fprintf (stderr, "\n\nAssembling functions:");

  /* Output everything.  
     ??? Our inline heuristic may decide to not inline functions previously
     marked as inlinable thus adding new function bodies that must be output.
     Later we should move all inlining decisions to callgraph code to make
     this impossible.  */
  cgraph_expand_functions ();
  while (changed)
    {
      changed = false;
      for (node = cgraph_nodes; node; node = node->next)
	{
	  if (!node->needed)
	    continue;

	  for (edge = node->callees; edge; edge = edge->next_callee)
	    if (!edge->callee->needed)
	      changed = edge->callee->needed = true;
	}
    }
  cgraph_expand_functions ();
}
