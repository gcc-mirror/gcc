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
#include "cgraph.h"
#include "diagnostic.h"

static void cgraph_expand_functions PARAMS ((void));
static void cgraph_mark_functions_to_output PARAMS ((void));
static void cgraph_expand_function PARAMS ((struct cgraph_node *));
static tree record_call_1 PARAMS ((tree *, int *, void *));
static void cgraph_mark_local_functions PARAMS ((void));
static void cgraph_mark_functions_to_inline_once PARAMS ((void));
static void cgraph_optimize_function PARAMS ((struct cgraph_node *));

/* Analyze function once it is parsed.  Set up the local information
   available - create cgraph edges for function calles via BODY.  */

void
cgraph_finalize_function (decl, body)
     tree decl;
     tree body ATTRIBUTE_UNUSED;
{
  struct cgraph_node *node = cgraph_node (decl);

  node->decl = decl;

  if (/* Externally visible functions must be output.  The exception are
	 COMDAT functions that must be output only when they are needed.
	 Similarly are handled defered functions and
	 external functions (GCC extension "extern inline") */
      (TREE_PUBLIC (decl) && !DECL_COMDAT (decl) && !DECL_EXTERNAL (decl))
      /* ??? Constructors and destructors not called otherwise can be inlined
	 into single construction/destruction function per section to save some
	 resources.  For now just mark it as reachable.  */
      || DECL_STATIC_CONSTRUCTOR (decl)
      || DECL_STATIC_DESTRUCTOR (decl)
      /* Function whose name is output to the assembler file must be produced.
	 It is possible to assemble the name later after finalizing the function
	 and the fact is noticed in assemble_name then.  */
      || (DECL_ASSEMBLER_NAME_SET_P (decl)
	  && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))))
    {
      cgraph_mark_needed_node (node, 1);
    }

  if (!node->needed && !DECL_COMDAT (node->decl))
    node->local.can_inline_once = tree_inlinable_function_p (decl, 1);
  else
    node->local.can_inline_once = 0;
  if (flag_inline_trees)
    node->local.inline_many = tree_inlinable_function_p (decl, 0);
  else
    node->local.inline_many = 0;

  (*debug_hooks->deferred_inline_function) (decl);
}

/* Walk tree and record all calls.  Called via walk_tree.  */
static tree
record_call_1 (tp, walk_subtrees, data)
     tree *tp;
     int *walk_subtrees;
     void *data;
{
  if (TREE_CODE (*tp) == VAR_DECL && TREE_STATIC (*tp))
    cgraph_varpool_mark_needed_node (cgraph_varpool_node (*tp));
  /* Record dereferences to the functions.  This makes the functions
     reachable unconditionally.  */
  else if (TREE_CODE (*tp) == ADDR_EXPR)
    {
      tree decl = TREE_OPERAND (*tp, 0);
      if (TREE_CODE (decl) == FUNCTION_DECL)
        cgraph_mark_needed_node (cgraph_node (decl), 1);
    }
  else if (TREE_CODE (*tp) == CALL_EXPR)
    {
      tree decl = get_callee_fndecl (*tp);
      if (decl && TREE_CODE (decl) == FUNCTION_DECL)
	{
	  if (DECL_BUILT_IN (decl))
	    return NULL;
	  cgraph_record_call (data, decl);
	     
	  /* When we see a function call, we don't want to look at the
	     function reference in the ADDR_EXPR that is hanging from
	     the CALL_EXPR we're examining here, because we would
	     conclude incorrectly that the function's address could be
	     taken by something that is not a function call.  So only
	     walk the function parameter list, skip the other subtrees.  */

	  walk_tree (&TREE_OPERAND (*tp, 1), record_call_1, data, NULL);
	  *walk_subtrees = 0;
	}
    }
  return NULL;
}

/* Create cgraph edges for function calls inside BODY from DECL.  */

void
cgraph_create_edges (decl, body)
     tree decl;
     tree body;
{
  /* The nodes we're interested in are never shared, so walk
     the tree ignoring duplicates.  */
  walk_tree_without_duplicates (&body, record_call_1, decl);
}

/* Analyze the whole compilation unit once it is parsed completely.  */

void
cgraph_finalize_compilation_unit ()
{
  struct cgraph_node *node;
  struct cgraph_edge *edge;

  cgraph_varpool_assemble_pending_decls ();

  if (!quiet_flag)
    {
      fprintf (stderr, "\n\nInitial entry points:");
      for (node = cgraph_nodes; node; node = node->next)
	if (node->needed && DECL_SAVED_TREE (node->decl))
	  announce_function (node->decl);
    }

  /* Propagate reachability flag and lower representation of all reachable
     functions.  In the future, lowering will introduce new functions and
     new entry points on the way (by template instantiation and virtual
     method table generation for instance).  */
  while (cgraph_nodes_queue)
    {
      tree decl = cgraph_nodes_queue->decl;

      node = cgraph_nodes_queue;
      cgraph_nodes_queue = cgraph_nodes_queue->aux;

      if (node->lowered || !node->reachable || !DECL_SAVED_TREE (decl))
	abort ();

      if (lang_hooks.callgraph.lower_function)
	(*lang_hooks.callgraph.lower_function) (decl);

      /* At the moment frontend automatically emits all nested functions.  */
      if (node->nested)
	{
	  struct cgraph_node *node2;

	  for (node2 = node->nested; node2; node2 = node2->next_nested)
	    if (!node2->reachable)
	      cgraph_mark_needed_node (node2, 0);
	}

      /* First kill forward declaration so reverse inling works properly.  */
      cgraph_create_edges (decl, DECL_SAVED_TREE (decl));

      for (edge = node->callees; edge; edge = edge->next_callee)
	{
	  if (!edge->callee->reachable)
            cgraph_mark_needed_node (edge->callee, 0);
	}
      node->lowered = true;
      cgraph_varpool_assemble_pending_decls ();
    }
  /* Collect entry points to the unit.  */

  if (!quiet_flag)
    {
      fprintf (stderr, "\n\nUnit entry points:");
      for (node = cgraph_nodes; node; node = node->next)
	if (node->needed && DECL_SAVED_TREE (node->decl))
	  announce_function (node->decl);
    }

  if (!quiet_flag)
    fprintf (stderr, "\n\nReclaiming functions:");

  for (node = cgraph_nodes; node; node = node->next)
    {
      tree decl = node->decl;

      if (!node->reachable && DECL_SAVED_TREE (decl))
	{
	  cgraph_remove_node (node);
	  announce_function (decl);
	}
    }
  ggc_collect ();
}

/* Figure out what functions we want to assemble.  */

static void
cgraph_mark_functions_to_output ()
{
  struct cgraph_node *node;

  for (node = cgraph_nodes; node; node = node->next)
    {
      tree decl = node->decl;

      /* We need to output all local functions that are used and not
	 always inlined, as well as those that are reachable from
	 outside the current compilation unit.  */
      if (DECL_SAVED_TREE (decl)
	  && (node->needed
	      || (!node->local.inline_many && !node->global.inline_once
		  && node->reachable)
	      || (DECL_ASSEMBLER_NAME_SET_P (decl)
	          && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))))
	  && !TREE_ASM_WRITTEN (decl) && !node->origin
	  && !DECL_EXTERNAL (decl))
	node->output = 1;
    }
}

/* Optimize the function before expansion.  */

static void
cgraph_optimize_function (node)
     struct cgraph_node *node;
{
  tree decl = node->decl;

  if (flag_inline_trees)
    optimize_inline_calls (decl);
  if (node->nested)
    {
      for (node = node->nested; node; node = node->next_nested)
	cgraph_optimize_function (node);
    }
}

/* Expand function specified by NODE.  */

static void
cgraph_expand_function (node)
     struct cgraph_node *node;
{
  tree decl = node->decl;

  announce_function (decl);

  cgraph_optimize_function (node);

  /* Generate RTL for the body of DECL.  Nested functions are expanded
     via lang_expand_decl_stmt.  */
  (*lang_hooks.callgraph.expand_function) (decl);

  /* When we decided to inline the function once, we never ever should
     need to output it separately.  */
  if (node->global.inline_once)
    abort ();
  if (!node->local.inline_many
      || !node->callers)
    DECL_SAVED_TREE (decl) = NULL;
  current_function_decl = NULL;
}


/* Expand all functions that must be output. 
  
   Attempt to topologically sort the nodes so function is output when
   all called functions are already assembled to allow data to be
   propagated accross the callgraph.  Use a stack to get smaller distance
   between a function and it's callees (later we may choose to use a more
   sophisticated algorithm for function reordering; we will likely want
   to use subsections to make the output functions appear in top-down
   order.  */

static void
cgraph_expand_functions ()
{
  struct cgraph_node *node, *node2;
  struct cgraph_node **stack =
    xcalloc (sizeof (struct cgraph_node *), cgraph_n_nodes);
  struct cgraph_node **order =
    xcalloc (sizeof (struct cgraph_node *), cgraph_n_nodes);
  int stack_size = 0;
  int order_pos = 0;
  struct cgraph_edge *edge, last;
  int i;

  cgraph_mark_functions_to_output ();

  /* We have to deal with cycles nicely, so use a depth first traversal
     output algorithm.  Ignore the fact that some functions won't need
     to be output and put them into order as well, so we get dependencies
     right through intline functions.  */
  for (node = cgraph_nodes; node; node = node->next)
    node->aux = NULL;
  for (node = cgraph_nodes; node; node = node->next)
    if (!node->aux)
      {
	node2 = node;
	if (!node->callers)
	  node->aux = &last;
	else
	  node->aux = node->callers;
	while (node2)
	  {
	    while (node2->aux != &last)
	      {
		edge = node2->aux;
		if (edge->next_caller)
		  node2->aux = edge->next_caller;
		else
		  node2->aux = &last;
		if (!edge->caller->aux)
		  {
		    if (!edge->caller->callers)
		      edge->caller->aux = &last;
		    else
		      edge->caller->aux = edge->caller->callers;
		    stack[stack_size++] = node2;
		    node2 = edge->caller;
		    break;
		  }
	      }
	    if (node2->aux == &last)
	      {
		order[order_pos++] = node2;
		if (stack_size)
		  node2 = stack[--stack_size];
		else
		  node2 = NULL;
	      }
	  }
      }
  for (i = order_pos - 1; i >= 0; i--)
    {
      node = order[i];
      if (node->output)
	{
	  if (!node->reachable)
	    abort ();
	  node->output = 0;
	  cgraph_expand_function (node);
	}
    }
  free (stack);
  free (order);
}

/* Mark all local functions.
   We can not use node->needed directly as it is modified during
   execution of cgraph_optimize.  */

static void
cgraph_mark_local_functions ()
{
  struct cgraph_node *node;

  if (!quiet_flag)
    fprintf (stderr, "\n\nMarking local functions:");

  /* Figure out functions we want to assemble.  */
  for (node = cgraph_nodes; node; node = node->next)
    {
      node->local.local = (!node->needed
		           && DECL_SAVED_TREE (node->decl)
			   && !DECL_COMDAT (node->decl)
		           && !TREE_PUBLIC (node->decl));
      if (node->local.local)
	announce_function (node->decl);
    }
}

/* Decide what function should be inlined because they are invoked once
   (so inlining won't result in duplication of the code).  */

static void
cgraph_mark_functions_to_inline_once ()
{
  struct cgraph_node *node, *node1;

  if (!quiet_flag)
    fprintf (stderr, "\n\nMarking functions to inline once:");

  /* Now look for function called only once and mark them to inline.
     From this point number of calls to given function won't grow.  */
  for (node = cgraph_nodes; node; node = node->next)
    {
      if (node->callers && !node->callers->next_caller && !node->needed
	  && node->local.can_inline_once)
	{
	  bool ok = true;

	  /* Verify that we won't duplicate the caller.  */
	  for (node1 = node->callers->caller;
	       node1->local.inline_many
	       && node1->callers
	       && ok;
	       node1 = node1->callers->caller)
	    if (node1->callers->next_caller || node1->needed)
	      ok = false;
	  if (ok)
	    {
	      node->global.inline_once = true;
	      announce_function (node->decl);
	    }
	}
    }
}


/* Perform simple optimizations based on callgraph.  */

void
cgraph_optimize ()
{
  struct cgraph_node *node;
  bool changed = true;

  cgraph_mark_local_functions ();

  cgraph_mark_functions_to_inline_once ();

  cgraph_global_info_ready = true;
  if (!quiet_flag)
    fprintf (stderr, "\n\nAssembling functions:");

  /* Output everything.  
     ??? Our inline heuristic may decide to not inline functions previously
     marked as inlinable thus adding new function bodies that must be output.
     Later we should move all inlining decisions to callgraph code to make
     this impossible.  */
  cgraph_expand_functions ();
  if (!quiet_flag)
    fprintf (stderr, "\n\nAssembling functions that failed to inline:");
  while (changed && !errorcount && !sorrycount)
    {
      changed = false;
      for (node = cgraph_nodes; node; node = node->next)
	{
	  tree decl = node->decl;
	  if (!node->origin
	      && !TREE_ASM_WRITTEN (decl)
	      && DECL_SAVED_TREE (decl)
	      && !DECL_EXTERNAL (decl))
	    {
	      struct cgraph_edge *edge;

	      for (edge = node->callers; edge; edge = edge->next_caller)
		if (TREE_ASM_WRITTEN (edge->caller->decl))
		  {
		    changed = true;
		    cgraph_expand_function (node);
		    break;
		  }
	    }
	}
    }
}
