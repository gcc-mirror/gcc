/* Callgraph based intraprocedural optimizations.
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
#include "timevar.h"
#include "params.h"
#include "fibheap.h"
#include "c-common.h"

#define INSNS_PER_CALL 10

static void cgraph_expand_functions (void);
static void cgraph_mark_functions_to_output (void);
static void cgraph_expand_function (struct cgraph_node *);
static tree record_call_1 (tree *, int *, void *);
static void cgraph_mark_local_functions (void);
static void cgraph_optimize_function (struct cgraph_node *);

/* Statistics we collect about inlining algorithm.  */
static int ncalls_inlined;
static int nfunctions_inlined;
static int initial_insns;
static int overall_insns;

/* Analyze function once it is parsed.  Set up the local information
   available - create cgraph edges for function calls via BODY.  */

void
cgraph_finalize_function (tree decl, tree body ATTRIBUTE_UNUSED)
{
  struct cgraph_node *node = cgraph_node (decl);

  node->decl = decl;
  node->local.finalized = true;

  /* Function now has DECL_SAVED_TREE set.  Enqueue it into cgraph_nodes_queue
     if needed.  */
  if (node->needed)
    cgraph_mark_needed_node (node, 0);
  if (/* Externally visible functions must be output.  The exception are
	 COMDAT functions that must be output only when they are needed.
	 Similarly are handled deferred functions and
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
	  && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
      || lookup_attribute ("used", DECL_ATTRIBUTES (decl)))
    {
      cgraph_mark_needed_node (node, 1);
    }

  (*debug_hooks->deferred_inline_function) (decl);
}

/* Walk tree and record all calls.  Called via walk_tree.  */
static tree
record_call_1 (tree *tp, int *walk_subtrees, void *data)
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

	  walk_tree_without_duplicates (&TREE_OPERAND (*tp, 1),
					record_call_1, data);
	  *walk_subtrees = 0;
	}
    }
  /* Save some cycles by not walking types and declaration as we won't find anything
     usefull there anyway.  */
  if (DECL_P (*tp) || TYPE_P (*tp))
    *walk_subtrees = 0;
  return NULL;
}

/* Create cgraph edges for function calls inside BODY from DECL.  */

void
cgraph_create_edges (tree decl, tree body)
{
  /* The nodes we're interested in are never shared, so walk
     the tree ignoring duplicates.  */
  walk_tree_without_duplicates (&body, record_call_1, decl);
}

/* Analyze the function scheduled to be output.  */
static void
cgraph_analyze_function (struct cgraph_node *node)
{
  tree decl = node->decl;

  if (lang_hooks.callgraph.lower_function)
    (*lang_hooks.callgraph.lower_function) (decl);

  current_function_decl = node->decl;

  /* First kill forward declaration so reverse inlining works properly.  */
  cgraph_create_edges (decl, DECL_SAVED_TREE (decl));

  node->local.inlinable = tree_inlinable_function_p (decl);
  if (!DECL_ESTIMATED_INSNS (decl))
    DECL_ESTIMATED_INSNS (decl)
      = (*lang_hooks.tree_inlining.estimate_num_insns) (decl);
  node->local.self_insns = DECL_ESTIMATED_INSNS (decl);
  if (node->local.inlinable)
    node->local.disregard_inline_limits
      = (*lang_hooks.tree_inlining.disregard_inline_limits) (decl);

  /* Inlining characteristics are maintained by the cgraph_mark_inline.  */
  node->global.insns = node->local.self_insns;
  if (!DECL_EXTERNAL (node->decl))
    {
      node->global.cloned_times = 1;
      node->global.will_be_output = true;
    }

  node->lowered = true;
}

/* Analyze the whole compilation unit once it is parsed completely.  */

void
cgraph_finalize_compilation_unit (void)
{
  struct cgraph_node *node;

  cgraph_varpool_assemble_pending_decls ();
  if (!quiet_flag)
    fprintf (stderr, "\nAnalyzing compilation unit\n");

  timevar_push (TV_CGRAPH);
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "\nInitial entry points:");
      for (node = cgraph_nodes; node; node = node->next)
	if (node->needed && DECL_SAVED_TREE (node->decl))
	  fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
      fprintf (cgraph_dump_file, "\n");
    }

  /* Propagate reachability flag and lower representation of all reachable
     functions.  In the future, lowering will introduce new functions and
     new entry points on the way (by template instantiation and virtual
     method table generation for instance).  */
  while (cgraph_nodes_queue)
    {
      struct cgraph_edge *edge;
      tree decl = cgraph_nodes_queue->decl;

      node = cgraph_nodes_queue;
      cgraph_nodes_queue = cgraph_nodes_queue->next_needed;

      if (node->lowered || !node->reachable || !DECL_SAVED_TREE (decl))
	abort ();

      cgraph_analyze_function (node);
      for (edge = node->callees; edge; edge = edge->next_callee)
      {
	if (!edge->callee->reachable)
	    cgraph_mark_needed_node (edge->callee, 0);
      }
      cgraph_varpool_assemble_pending_decls ();
    }
  /* Collect entry points to the unit.  */

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "\nUnit entry points:");
      for (node = cgraph_nodes; node; node = node->next)
	if (node->needed && DECL_SAVED_TREE (node->decl))
	  fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
      fprintf (cgraph_dump_file, "\n");
      dump_cgraph (cgraph_dump_file);
    }

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nReclaiming functions:");

  for (node = cgraph_nodes; node; node = node->next)
    {
      tree decl = node->decl;

      if (!node->reachable && DECL_SAVED_TREE (decl))
	{
	  cgraph_remove_node (node);
	  if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
	}
    }
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\n");
  ggc_collect ();
  timevar_pop (TV_CGRAPH);
}

/* Figure out what functions we want to assemble.  */

static void
cgraph_mark_functions_to_output (void)
{
  struct cgraph_node *node;

  for (node = cgraph_nodes; node; node = node->next)
    {
      tree decl = node->decl;
      struct cgraph_edge *e;
      if (node->output)
	abort ();

      for (e = node->callers; e; e = e->next_caller)
	if (!e->inline_call)
	  break;

      /* We need to output all local functions that are used and not
	 always inlined, as well as those that are reachable from
	 outside the current compilation unit.  */
      if (DECL_SAVED_TREE (decl)
	  && (node->needed
	      || (e && node->reachable))
	  && !TREE_ASM_WRITTEN (decl) && !node->origin
	  && !DECL_EXTERNAL (decl))
	node->output = 1;
    }
}

/* Optimize the function before expansion.  */

static void
cgraph_optimize_function (struct cgraph_node *node)
{
  tree decl = node->decl;

  timevar_push (TV_INTEGRATION);
  /* optimize_inline_calls avoids inlining of current_function_decl.  */
  current_function_decl = decl;
  if (flag_inline_trees)
    optimize_inline_calls (decl);
  if (node->nested)
    {
      for (node = node->nested; node; node = node->next_nested)
	cgraph_optimize_function (node);
    }
  timevar_pop (TV_INTEGRATION);
}

/* Expand function specified by NODE.  */

static void
cgraph_expand_function (struct cgraph_node *node)
{
  tree decl = node->decl;
  struct cgraph_edge *e;

  announce_function (decl);

  cgraph_optimize_function (node);

  /* Generate RTL for the body of DECL.  Nested functions are expanded
     via lang_expand_decl_stmt.  */
  (*lang_hooks.callgraph.expand_function) (decl);

  for (e = node->callers; e; e = e->next_caller)
    if (e->inline_call)
      break;
  if (!e)
    DECL_SAVED_TREE (decl) = NULL;
  current_function_decl = NULL;
}

/* Fill array order with all nodes with output flag set in the reverse
   topological order.  */
static int
cgraph_postorder (struct cgraph_node **order)
{
  struct cgraph_node *node, *node2;
  int stack_size = 0;
  int order_pos = 0;
  struct cgraph_edge *edge, last;

  struct cgraph_node **stack =
    xcalloc (cgraph_n_nodes, sizeof (struct cgraph_node *));

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
  free (stack);
  return order_pos;
}

#define INLINED_TIMES(node) ((size_t)(node)->aux)
#define SET_INLINED_TIMES(node,times) ((node)->aux = (void *)(times))

/* Return list of nodes we decided to inline NODE into, set their output
   flag and compute INLINED_TIMES.

   We do simple backtracing to get INLINED_TIMES right.  This should not be
   expensive as we limit the amount of inlining.  Alternatively we may first
   discover set of nodes, topologically sort these and propagate
   INLINED_TIMES  */

static int
cgraph_inlined_into (struct cgraph_node *node, struct cgraph_node **array)
{
  int nfound = 0;
  struct cgraph_edge **stack;
  struct cgraph_edge *e, *e1;
  int sp;
  int i;

  /* Fast path: since we traverse in mostly topological order, we will likely
     find no edges.  */
  for (e = node->callers; e; e = e->next_caller)
    if (e->inline_call)
      break;

  if (!e)
    return 0;

  /* Allocate stack for back-tracking up callgraph.  */
  stack = xmalloc ((cgraph_n_nodes + 1) * sizeof (struct cgraph_edge));
  sp = 0;

  /* Push the first edge on to the stack.  */
  stack[sp++] = e;

  while (sp)
    {
      struct cgraph_node *caller;

      /* Look at the edge on the top of the stack.  */
      e = stack[sp - 1];
      caller = e->caller;

      /* Check if the caller destination has been visited yet.  */
      if (!caller->output)
	{
	  array[nfound++] = e->caller;
	  /* Mark that we have visited the destination.  */
	  caller->output = true;
	  SET_INLINED_TIMES (caller, 0);
	}
      SET_INLINED_TIMES (caller, INLINED_TIMES (caller) + 1);

      for (e1 = caller->callers; e1; e1 = e1->next_caller)
	if (e1->inline_call)
	  break;
      if (e1)
	stack[sp++] = e1;
      else
	{
	  while (true)
	    {
	      for (e1 = e->next_caller; e1; e1 = e1->next_caller)
		if (e1->inline_call)
		  break;

	      if (e1)
		{
		  stack[sp - 1] = e1;
		  break;
		}
	      else
		{
		  sp--;
		  if (!sp)
		    break;
		  e = stack[sp - 1];
		}
	    }
	}
    }

  free (stack);


  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Found inline predecesors of %s:",
	       cgraph_node_name (node));
      for (i = 0; i < nfound; i++)
	{
	  fprintf (cgraph_dump_file, " %s", cgraph_node_name (array[i]));
	  if (INLINED_TIMES (array[i]) != 1)
	    fprintf (cgraph_dump_file, " (%i times)",
		     (int)INLINED_TIMES (array[i]));
	}
      fprintf (cgraph_dump_file, "\n");
    }

  return nfound;
}

/* Return list of nodes we decided to inline into NODE, set their output
   flag and compute INLINED_TIMES.

   This function is identical to cgraph_inlined_into with callers and callees
   nodes swapped.  */

static int
cgraph_inlined_callees (struct cgraph_node *node, struct cgraph_node **array)
{
  int nfound = 0;
  struct cgraph_edge **stack;
  struct cgraph_edge *e, *e1;
  int sp;
  int i;

  /* Fast path: since we traverse in mostly topological order, we will likely
     find no edges.  */
  for (e = node->callees; e; e = e->next_callee)
    if (e->inline_call)
      break;

  if (!e)
    return 0;

  /* Allocate stack for back-tracking up callgraph.  */
  stack = xmalloc ((cgraph_n_nodes + 1) * sizeof (struct cgraph_edge));
  sp = 0;

  /* Push the first edge on to the stack.  */
  stack[sp++] = e;

  while (sp)
    {
      struct cgraph_node *callee;

      /* Look at the edge on the top of the stack.  */
      e = stack[sp - 1];
      callee = e->callee;

      /* Check if the callee destination has been visited yet.  */
      if (!callee->output)
	{
	  array[nfound++] = e->callee;
	  /* Mark that we have visited the destination.  */
	  callee->output = true;
	  SET_INLINED_TIMES (callee, 0);
	}
      SET_INLINED_TIMES (callee, INLINED_TIMES (callee) + 1);

      for (e1 = callee->callees; e1; e1 = e1->next_callee)
	if (e1->inline_call)
	  break;
      if (e1)
	stack[sp++] = e1;
      else
	{
	  while (true)
	    {
	      for (e1 = e->next_callee; e1; e1 = e1->next_callee)
		if (e1->inline_call)
		  break;

	      if (e1)
		{
		  stack[sp - 1] = e1;
		  break;
		}
	      else
		{
		  sp--;
		  if (!sp)
		    break;
		  e = stack[sp - 1];
		}
	    }
	}
    }

  free (stack);

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Found inline successors of %s:",
	       cgraph_node_name (node));
      for (i = 0; i < nfound; i++)
	{
	  fprintf (cgraph_dump_file, " %s", cgraph_node_name (array[i]));
	  if (INLINED_TIMES (array[i]) != 1)
	    fprintf (cgraph_dump_file, " (%i times)",
		     (int)INLINED_TIMES (array[i]));
	}
      fprintf (cgraph_dump_file, "\n");
    }

  return nfound;
}

/* Estimate size of the function after inlining WHAT into TO.  */

static int
cgraph_estimate_size_after_inlining (int times, struct cgraph_node *to,
				     struct cgraph_node *what)
{
  return (what->global.insns - INSNS_PER_CALL) *times + to->global.insns;
}

/* Estimate the growth caused by inlining NODE into all callees.  */

static int
cgraph_estimate_growth (struct cgraph_node *node)
{
  int growth = 0;
  int calls_saved = 0;
  int clones_added = 0;
  struct cgraph_edge *e;

  for (e = node->callers; e; e = e->next_caller)
    if (!e->inline_call)
      {
	growth += ((cgraph_estimate_size_after_inlining (1, e->caller, node)
		    -
		    e->caller->global.insns) *e->caller->global.cloned_times);
	calls_saved += e->caller->global.cloned_times;
	clones_added += e->caller->global.cloned_times;
      }

  /* ??? Wrong for self recursive functions or cases where we decide to not
     inline for different reasons, but it is not big deal as in that case
     we will keep the body around, but we will also avoid some inlining.  */
  if (!node->needed && !node->origin && !DECL_EXTERNAL (node->decl))
    growth -= node->global.insns, clones_added--;

  if (!calls_saved)
    calls_saved = 1;

  return growth;
}

/* Update insn sizes after inlining WHAT into TO that is already inlined into
   all nodes in INLINED array.  */

static void
cgraph_mark_inline (struct cgraph_node *to, struct cgraph_node *what,
		    struct cgraph_node **inlined, int ninlined,
		    struct cgraph_node **inlined_callees,
		    int ninlined_callees)
{
  int i;
  int times = 0;
  int clones = 0;
  struct cgraph_edge *e;
  bool called = false;
  int new_insns;

  for (e = what->callers; e; e = e->next_caller)
    {
      if (e->caller == to)
	{
	  if (e->inline_call)
	    abort ();
	  e->inline_call = true;
	  times++;
	  clones += e->caller->global.cloned_times;
	}
      else if (!e->inline_call)
	called = true;
    }
  if (!times)
    abort ();
  ncalls_inlined += times;

  new_insns = cgraph_estimate_size_after_inlining (times, to, what);
  if (to->global.will_be_output)
    overall_insns += new_insns - to->global.insns;
  to->global.insns = new_insns;

  if (!called && !what->needed && !what->origin
      && !DECL_EXTERNAL (what->decl))
    {
      if (!what->global.will_be_output)
	abort ();
      clones--;
      nfunctions_inlined++;
      what->global.will_be_output = 0;
      overall_insns -= what->global.insns;
    }
  what->global.cloned_times += clones;
  for (i = 0; i < ninlined; i++)
    {
      new_insns =
	cgraph_estimate_size_after_inlining (INLINED_TIMES (inlined[i]) *
					     times, inlined[i], what);
      if (inlined[i]->global.will_be_output)
	overall_insns += new_insns - inlined[i]->global.insns;
      inlined[i]->global.insns = new_insns;
    }
  for (i = 0; i < ninlined_callees; i++)
    {
      inlined_callees[i]->global.cloned_times +=
	INLINED_TIMES (inlined_callees[i]) * clones;
    }
}

/* Return false when inlining WHAT into TO is not good idea as it would cause
   too large growth of function bodies.  */

static bool
cgraph_check_inline_limits (struct cgraph_node *to, struct cgraph_node *what,
			    struct cgraph_node **inlined, int ninlined)
{
  int i;
  int times = 0;
  struct cgraph_edge *e;
  int newsize;
  int limit;

  for (e = to->callees; e; e = e->next_callee)
    if (e->callee == what)
      times++;

  /* When inlining large function body called once into small function,
     take the inlined function as base for limiting the growth.  */
  if (to->local.self_insns > what->local.self_insns)
    limit = to->local.self_insns;
  else
    limit = what->local.self_insns;

  limit += limit * PARAM_VALUE (PARAM_LARGE_FUNCTION_GROWTH) / 100;

  newsize = cgraph_estimate_size_after_inlining (times, to, what);
  if (newsize > PARAM_VALUE (PARAM_LARGE_FUNCTION_INSNS)
      && newsize > limit)
    return false;
  for (i = 0; i < ninlined; i++)
    {
      newsize =
	cgraph_estimate_size_after_inlining (INLINED_TIMES (inlined[i]) *
					     times, inlined[i], what);
      if (newsize > PARAM_VALUE (PARAM_LARGE_FUNCTION_INSNS)
	  && newsize >
	  inlined[i]->local.self_insns *
	  (100 + PARAM_VALUE (PARAM_LARGE_FUNCTION_GROWTH)) / 100)
	return false;
    }
  return true;
}

/* Return true when function N is small enought to be inlined.  */

static bool
cgraph_default_inline_p (struct cgraph_node *n)
{
  if (!DECL_INLINE (n->decl) || !DECL_SAVED_TREE (n->decl))
    return false;
  if (DECL_DECLARED_INLINE_P (n->decl))
    return n->global.insns < MAX_INLINE_INSNS_SINGLE;
  else
    return n->global.insns < MAX_INLINE_INSNS_AUTO;
}

/* We use greedy algorithm for inlining of small functions:
   All inline candidates are put into prioritized heap based on estimated
   growth of the overall number of instructions and then update the estimates.

   INLINED and INLINED_CALEES are just pointers to arrays large enought
   to be passed to cgraph_inlined_into and cgraph_inlined_callees.  */

static void
cgraph_decide_inlining_of_small_functions (struct cgraph_node **inlined,
					   struct cgraph_node **inlined_callees)
{
  int i;
  struct cgraph_node *node;
  fibheap_t heap = fibheap_new ();
  struct fibnode **heap_node =
    xcalloc (cgraph_max_uid, sizeof (struct fibnode *));
  int ninlined, ninlined_callees;
  int max_insns = ((HOST_WIDEST_INT) initial_insns
		   * (100 + PARAM_VALUE (PARAM_INLINE_UNIT_GROWTH)) / 100);

  /* Put all inline candidates into the heap.  */

  for (node = cgraph_nodes; node; node = node->next)
    {
      struct cgraph_edge *e;

      if (!node->local.inlinable || !node->callers
	  || !cgraph_default_inline_p (node))
	continue;

      /* Rule out always_inline functions we dealt with earlier.  */
      for (e = node->callers; e; e = e->next_caller)
	if (e->inline_call)
	  break;
      if (e)
	continue;
      heap_node[node->uid] =
	fibheap_insert (heap, cgraph_estimate_growth (node), node);
    }

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\n\nDeciding on inlining: ");
  while ((node = fibheap_extract_min (heap)) && overall_insns <= max_insns)
    {
      struct cgraph_edge *e;
      int old_insns = overall_insns;

      heap_node[node->uid] = NULL;
      if (cgraph_dump_file)
	fprintf (cgraph_dump_file, "Considering %s %i insns, growth %i.\n",
		 cgraph_node_name (node), node->global.insns,
		 cgraph_estimate_growth (node));
      if (!cgraph_default_inline_p (node))
	{
	  if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, "Function too large.\n");
	  continue;
	}
      ninlined_callees = cgraph_inlined_callees (node, inlined_callees);
      for (e = node->callers; e; e = e->next_caller)
	if (!e->inline_call && e->caller != node)
	  {
	    ninlined = cgraph_inlined_into (e->caller, inlined);
	    if (e->callee->output
		|| !cgraph_check_inline_limits (e->caller, node, inlined,
						ninlined))
	      {
		for (i = 0; i < ninlined; i++)
		  inlined[i]->output = 0, node->aux = 0;
		if (cgraph_dump_file)
		  fprintf (cgraph_dump_file, "Not inlining into %s\n",
			   cgraph_node_name (e->caller));
		continue;
	      }
	    cgraph_mark_inline (e->caller, node, inlined, ninlined,
				inlined_callees, ninlined_callees);
	    if (heap_node[e->caller->uid])
	      fibheap_replace_key (heap, heap_node[e->caller->uid],
				   cgraph_estimate_growth (e->caller));

	    /* Size of the functions we updated into has changed, so update
	       the keys.  */
	    for (i = 0; i < ninlined; i++)
	      {
		inlined[i]->output = 0, node->aux = 0;
		if (heap_node[inlined[i]->uid])
		  fibheap_replace_key (heap, heap_node[inlined[i]->uid],
				       cgraph_estimate_growth (inlined[i]));
	      }
	  }

      /* Similarly all functions called by function we just inlined
         are now called more times; update keys.  */

      for (e = node->callees; e; e = e->next_callee)
	if (!e->inline_call && heap_node[e->callee->uid])
	  fibheap_replace_key (heap, heap_node[e->callee->uid],
			       cgraph_estimate_growth (e->callee));

      for (i = 0; i < ninlined_callees; i++)
	{
	  struct cgraph_edge *e;

	  for (e = inlined_callees[i]->callees; e; e = e->next_callee)
	    if (!e->inline_call && heap_node[e->callee->uid])
	      fibheap_replace_key (heap, heap_node[e->callee->uid],
				   cgraph_estimate_growth (e->callee));

	  inlined_callees[i]->output = 0, node->aux = 0;
	}
      if (cgraph_dump_file)
	fprintf (cgraph_dump_file,
		 "Created %i clones, Num insns:%i (%+i), %.2f%%.\n\n",
		 node->global.cloned_times - 1,
		 overall_insns, overall_insns - old_insns,
		 overall_insns * 100.0 / initial_insns);
    }
  if (cgraph_dump_file && !fibheap_empty (heap))
    fprintf (cgraph_dump_file, "inline-unit-growth limit reached.\n");
  fibheap_delete (heap);
  free (heap_node);
}

/* Decide on the inlining.  We do so in the topological order to avoid
   expenses on updating datastructures.  */

static void
cgraph_decide_inlining (void)
{
  struct cgraph_node *node;
  int nnodes;
  struct cgraph_node **order =
    xcalloc (cgraph_n_nodes, sizeof (struct cgraph_node *));
  struct cgraph_node **inlined =
    xcalloc (cgraph_n_nodes, sizeof (struct cgraph_node *));
  struct cgraph_node **inlined_callees =
    xcalloc (cgraph_n_nodes, sizeof (struct cgraph_node *));
  int ninlined;
  int ninlined_callees;
  int i, y;

  for (node = cgraph_nodes; node; node = node->next)
    initial_insns += node->local.self_insns;
  overall_insns = initial_insns;

  nnodes = cgraph_postorder (order);

  for (node = cgraph_nodes; node; node = node->next)
    node->aux = 0;

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\n\nDeciding on always_inline functions:\n");

  /* In the first pass mark all always_inline edges.  Do this with a priority
     so no our decisions makes this impossible.  */
  for (i = nnodes - 1; i >= 0; i--)
    {
      struct cgraph_edge *e;

      node = order[i];

      for (e = node->callees; e; e = e->next_callee)
	if (e->callee->local.disregard_inline_limits)
	  break;
      if (!e)
	continue;
      if (cgraph_dump_file)
	fprintf (cgraph_dump_file,
		 "Considering %s %i insns (always inline)\n",
		 cgraph_node_name (node), node->global.insns);
      ninlined = cgraph_inlined_into (order[i], inlined);
      for (; e; e = e->next_callee)
	{
	  if (e->inline_call || !e->callee->local.disregard_inline_limits)
	    continue;
	  if (e->callee->output || e->callee == node)
	    continue;
	  ninlined_callees =
	    cgraph_inlined_callees (e->callee, inlined_callees);
	  cgraph_mark_inline (node, e->callee, inlined, ninlined,
			      inlined_callees, ninlined_callees);
	  for (y = 0; y < ninlined_callees; y++)
	    inlined_callees[y]->output = 0, node->aux = 0;
	  if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, "Inlined %i times. Now %i insns\n\n",
		     node->global.cloned_times, overall_insns);
	}
      for (y = 0; y < ninlined; y++)
	inlined[y]->output = 0, node->aux = 0;
    }

  cgraph_decide_inlining_of_small_functions (inlined, inlined_callees);

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\n\nFunctions to inline once:\n");

  /* And finally decide what functions are called once.  */

  for (i = nnodes - 1; i >= 0; i--)
    {
      node = order[i];

      if (node->callers && !node->callers->next_caller && !node->needed
	  && node->local.inlinable && !node->callers->inline_call
	  && !DECL_EXTERNAL (node->decl) && !DECL_COMDAT (node->decl))
	{
	  bool ok = true;
	  struct cgraph_node *node1;

	  /* Verify that we won't duplicate the caller.  */
	  for (node1 = node->callers->caller;
	       node1->callers && node1->callers->inline_call
	       && ok; node1 = node1->callers->caller)
	    if (node1->callers->next_caller || node1->needed)
	      ok = false;
	  if (ok)
	    {
	      if (cgraph_dump_file)
		fprintf (cgraph_dump_file,
			 "Considering %s %i insns (called once)\n",
			 cgraph_node_name (node), node->global.insns);
	      ninlined = cgraph_inlined_into (node->callers->caller, inlined);
	      if (cgraph_check_inline_limits
		  (node->callers->caller, node, inlined, ninlined))
		{
		  ninlined_callees =
		    cgraph_inlined_callees (node, inlined_callees);
		  cgraph_mark_inline (node->callers->caller, node, inlined,
				      ninlined, inlined_callees,
				      ninlined_callees);
		  for (y = 0; y < ninlined_callees; y++)
		    inlined_callees[y]->output = 0, node->aux = 0;
		  if (cgraph_dump_file)
		    fprintf (cgraph_dump_file, "Inlined. Now %i insns\n\n", overall_insns);
		}
	      for (y = 0; y < ninlined; y++)
		inlined[y]->output = 0, node->aux = 0;
	    }
	}
    }

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file,
	     "\nInlined %i calls, elliminated %i functions, %i insns turned to %i insns.\n",
	     ncalls_inlined, nfunctions_inlined, initial_insns,
	     overall_insns);
  free (order);
  free (inlined);
  free (inlined_callees);
}

/* Return true when CALLER_DECL should be inlined into CALLEE_DECL.  */

bool
cgraph_inline_p (tree caller_decl, tree callee_decl)
{
  struct cgraph_node *caller = cgraph_node (caller_decl);
  struct cgraph_node *callee = cgraph_node (callee_decl);
  struct cgraph_edge *e;

  for (e = caller->callees; e; e = e->next_callee)
    if (e->callee == callee)
      return e->inline_call;
  /* We do not record builtins in the callgraph.  Perhaps it would make more
     sense to do so and then prune out those not overwritten by explicit
     function body.  */
  return false;
}
/* Expand all functions that must be output.

   Attempt to topologically sort the nodes so function is output when
   all called functions are already assembled to allow data to be
   propagated across the callgraph.  Use a stack to get smaller distance
   between a function and it's callees (later we may choose to use a more
   sophisticated algorithm for function reordering; we will likely want
   to use subsections to make the output functions appear in top-down
   order).  */

static void
cgraph_expand_functions (void)
{
  struct cgraph_node *node;
  struct cgraph_node **order =
    xcalloc (cgraph_n_nodes, sizeof (struct cgraph_node *));
  int order_pos = 0;
  int i;

  cgraph_mark_functions_to_output ();

  order_pos = cgraph_postorder (order);

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
  free (order);
}

/* Mark all local functions.

   A local function is one whose calls can occur only in the
   current compilation unit, so we change its calling convention.
   We simply mark all static functions whose address is not taken
   as local.  */

static void
cgraph_mark_local_functions (void)
{
  struct cgraph_node *node;

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Marking local functions:");

  /* Figure out functions we want to assemble.  */
  for (node = cgraph_nodes; node; node = node->next)
    {
      node->local.local = (!node->needed
		           && DECL_SAVED_TREE (node->decl)
		           && !TREE_PUBLIC (node->decl));
      if (cgraph_dump_file && node->local.local)
	fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
    }
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\n");
}

/* Perform simple optimizations based on callgraph.  */

void
cgraph_optimize (void)
{
  timevar_push (TV_CGRAPHOPT);
  if (!quiet_flag)
    fprintf (stderr, "Performing intraprocedural optimizations\n");
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Initial callgraph:");
      dump_cgraph (cgraph_dump_file);
    }
  cgraph_mark_local_functions ();

  cgraph_decide_inlining ();

  cgraph_global_info_ready = true;
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Optimized callgraph:");
      dump_cgraph (cgraph_dump_file);
    }
  timevar_pop (TV_CGRAPHOPT);
  if (!quiet_flag)
    fprintf (stderr, "Assembling functions:");

  /* Output everything.  */
  cgraph_expand_functions ();
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Final callgraph:");
      dump_cgraph (cgraph_dump_file);
    }
}
