/* Callgraph based intraprocedural optimizations.
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
#include "intl.h"
#include "function.h"

#define INSNS_PER_CALL 10

static void cgraph_expand_all_functions (void);
static void cgraph_mark_functions_to_output (void);
static void cgraph_expand_function (struct cgraph_node *);
static tree record_call_1 (tree *, int *, void *);
static void cgraph_mark_local_functions (void);
static void cgraph_optimize_function (struct cgraph_node *);
static bool cgraph_default_inline_p (struct cgraph_node *n);
static void cgraph_analyze_function (struct cgraph_node *node);
static void cgraph_decide_inlining_incrementally (struct cgraph_node *);

/* Statistics we collect about inlining algorithm.  */
static int ncalls_inlined;
static int nfunctions_inlined;
static int initial_insns;
static int overall_insns;

/* Records tree nodes seen in cgraph_create_edges.  Simply using
   walk_tree_without_duplicates doesn't guarantee each node is visited
   once because it gets a new htab upon each recursive call from
   record_calls_1.  */
static htab_t visited_nodes;

/* Determine if function DECL is needed.  That is, visible to something
   either outside this translation unit, something magic in the system
   configury, or (if not doing unit-at-a-time) to something we havn't
   seen yet.  */

static bool
decide_is_function_needed (struct cgraph_node *node, tree decl)
{
  /* If we decided it was needed before, but at the time we didn't have
     the body of the function available, then it's still needed.  We have
     to go back and re-check its dependencies now.  */
  if (node->needed)
    return true;

  /* Externally visible functions must be output.  The exception is
     COMDAT functions that must be output only when they are needed.  */
  if (TREE_PUBLIC (decl) && !DECL_COMDAT (decl) && !DECL_EXTERNAL (decl))
    return true;

  /* Constructors and destructors are reachable from the runtime by
     some mechanism.  */
  if (DECL_STATIC_CONSTRUCTOR (decl) || DECL_STATIC_DESTRUCTOR (decl))
    return true;

  /* If the user told us it is used, then it must be so.  */
  if (lookup_attribute ("used", DECL_ATTRIBUTES (decl)))
    return true;

  /* ??? If the assembler name is set by hand, it is possible to assemble
     the name later after finalizing the function and the fact is noticed
     in assemble_name then.  This is arguably a bug.  */
  if (DECL_ASSEMBLER_NAME_SET_P (decl)
      && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
    return true;

  if (flag_unit_at_a_time)
    return false;

  /* If not doing unit at a time, then we'll only defer this function
     if its marked for inlining.  Otherwise we want to emit it now.  */

  /* "extern inline" functions are never output locally.  */
  if (DECL_EXTERNAL (decl))
    return false;
  /* We want to emit COMDAT functions only when absolutely necessary.  */
  if (DECL_COMDAT (decl))
    return false;
  if (!DECL_INLINE (decl)
      || (!node->local.disregard_inline_limits
	  /* When declared inline, defer even the uninlinable functions.
	     This allows them to be eliminated when unused.  */
	  && !DECL_DECLARED_INLINE_P (decl) 
	  && (!node->local.inlinable || !cgraph_default_inline_p (node))))
    return true;

  return false;
}

/* When not doing unit-at-a-time, output all functions enqueued.
   Return true when such a functions were found.  */

bool
cgraph_assemble_pending_functions (void)
{
  bool output = false;

  if (flag_unit_at_a_time)
    return false;

  while (cgraph_nodes_queue)
    {
      struct cgraph_node *n = cgraph_nodes_queue;

      cgraph_nodes_queue = cgraph_nodes_queue->next_needed;
      if (!n->origin && !DECL_EXTERNAL (n->decl))
	{
	  cgraph_expand_function (n);
	  output = true;
	}
    }

  return output;
}

/* DECL has been parsed.  Take it, queue it, compile it at the whim of the
   logic in effect.  If NESTED is true, then our caller cannot stand to have
   the garbage collector run at the moment.  We would need to either create
   a new GC context, or just not compile right now.  */

void
cgraph_finalize_function (tree decl, bool nested)
{
  struct cgraph_node *node = cgraph_node (decl);

  if (node->local.finalized)
    {
      /* As an GCC extension we allow redefinition of the function.  The
	 semantics when both copies of bodies differ is not well defined.
	 We replace the old body with new body so in unit at a time mode
	 we always use new body, while in normal mode we may end up with
	 old body inlined into some functions and new body expanded and
	 inlined in others.
	 
	 ??? It may make more sense to use one body for inlining and other
	 body for expanding the function but this is difficult to do.  */

      /* If node->output is set, then this is a unit-at-a-time compilation
	 and we have already begun whole-unit analysis.  This is *not*
	 testing for whether we've already emitted the function.  That
	 case can be sort-of legitimately seen with real function 
	 redefinition errors.  I would argue that the front end should
	 never present us with such a case, but don't enforce that for now.  */
      if (node->output)
	abort ();

      /* Reset our datastructures so we can analyze the function again.  */
      memset (&node->local, 0, sizeof (node->local));
      memset (&node->global, 0, sizeof (node->global));
      memset (&node->rtl, 0, sizeof (node->rtl));
      node->analyzed = false;
      node->local.redefined_extern_inline = true;
      while (node->callees)
	cgraph_remove_edge (node, node->callees->callee);

      /* We may need to re-queue the node for assembling in case
         we already proceeded it and ignored as not needed.  */
      if (node->reachable && !flag_unit_at_a_time)
	{
	  struct cgraph_node *n;

	  for (n = cgraph_nodes_queue; n; n = n->next_needed)
	    if (n == node)
	      break;
	  if (!n)
	    node->reachable = 0;
	}
    }

  notice_global_symbol (decl);
  node->decl = decl;
  node->local.finalized = true;

  /* If not unit at a time, then we need to create the call graph
     now, so that called functions can be queued and emitted now.  */
  if (!flag_unit_at_a_time)
    {
      cgraph_analyze_function (node);
      cgraph_decide_inlining_incrementally (node);
    }

  if (decide_is_function_needed (node, decl))
    cgraph_mark_needed_node (node);

  /* If not unit at a time, go ahead and emit everything we've found
     to be reachable at this time.  */
  if (!nested)
    {
      if (!cgraph_assemble_pending_functions ())
	ggc_collect ();
    }

  /* If we've not yet emitted decl, tell the debug info about it.  */
  if (!TREE_ASM_WRITTEN (decl))
    (*debug_hooks->deferred_inline_function) (decl);

  /* We will never really output the function body, clear the SAVED_INSNS array
     early then.  */
  if (DECL_EXTERNAL (decl))
    DECL_SAVED_INSNS (decl) = NULL;
}

/* Walk tree and record all calls.  Called via walk_tree.  */
static tree
record_call_1 (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
      /* ??? Really, we should mark this decl as *potentially* referenced
	 by this function and re-examine whether the decl is actually used
	 after rtl has been generated.  */
      if (TREE_STATIC (t))
        cgraph_varpool_mark_needed_node (cgraph_varpool_node (t));
      break;

    case ADDR_EXPR:
      if (flag_unit_at_a_time)
	{
	  /* Record dereferences to the functions.  This makes the
	     functions reachable unconditionally.  */
	  tree decl = TREE_OPERAND (*tp, 0);
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    cgraph_mark_needed_node (cgraph_node (decl));
	}
      break;

    case CALL_EXPR:
      {
	tree decl = get_callee_fndecl (*tp);
	if (decl && TREE_CODE (decl) == FUNCTION_DECL)
	  {
	    cgraph_record_call (data, decl);

	    /* When we see a function call, we don't want to look at the
	       function reference in the ADDR_EXPR that is hanging from
	       the CALL_EXPR we're examining here, because we would
	       conclude incorrectly that the function's address could be
	       taken by something that is not a function call.  So only
	       walk the function parameter list, skip the other subtrees.  */

	    walk_tree (&TREE_OPERAND (*tp, 1), record_call_1, data,
		       visited_nodes);
	    *walk_subtrees = 0;
	  }
	break;
      }

    default:
      /* Save some cycles by not walking types and declaration as we
	 won't find anything useful there anyway.  */
      if (DECL_P (*tp) || TYPE_P (*tp))
	{
	  *walk_subtrees = 0;
	  break;
	}

      if ((unsigned int) TREE_CODE (t) >= LAST_AND_UNUSED_TREE_CODE)
	return (*lang_hooks.callgraph.analyze_expr) (tp, walk_subtrees, data);
      break;
    }

  return NULL;
}

/* Create cgraph edges for function calls inside BODY from DECL.  */

void
cgraph_create_edges (tree decl, tree body)
{
  /* The nodes we're interested in are never shared, so walk
     the tree ignoring duplicates.  */
  visited_nodes = htab_create (37, htab_hash_pointer,
				    htab_eq_pointer, NULL);
  walk_tree (&body, record_call_1, decl, visited_nodes);
  htab_delete (visited_nodes);
  visited_nodes = NULL;
}

/* Analyze the function scheduled to be output.  */
static void
cgraph_analyze_function (struct cgraph_node *node)
{
  tree decl = node->decl;
  struct cgraph_edge *e;

  current_function_decl = decl;

  /* First kill forward declaration so reverse inlining works properly.  */
  cgraph_create_edges (decl, DECL_SAVED_TREE (decl));

  node->local.inlinable = tree_inlinable_function_p (decl);
  if (!node->local.self_insns)
    node->local.self_insns
      = (*lang_hooks.tree_inlining.estimate_num_insns) (decl);
  if (node->local.inlinable)
    node->local.disregard_inline_limits
      = (*lang_hooks.tree_inlining.disregard_inline_limits) (decl);
  for (e = node->callers; e; e = e->next_caller)
    if (e->inline_failed)
      {
	if (node->local.redefined_extern_inline)
	  e->inline_failed = N_("redefined extern inline functions are not "
				"considered for inlining");
	else if (!node->local.inlinable)
	  e->inline_failed = N_("function not inlinable");
	else
	  e->inline_failed = N_("function not considered for inlining");
      }
  if (flag_really_no_inline && !node->local.disregard_inline_limits)
    node->local.inlinable = 0;
  /* Inlining characteristics are maintained by the cgraph_mark_inline.  */
  node->global.insns = node->local.self_insns;
  if (!DECL_EXTERNAL (decl))
    {
      node->global.cloned_times = 1;
      node->global.will_be_output = true;
    }

  node->analyzed = true;
  current_function_decl = NULL;

  /* Possibly warn about unused parameters.  */
  if (warn_unused_parameter)
    do_warn_unused_parameter (decl);
}

/* Analyze the whole compilation unit once it is parsed completely.  */

void
cgraph_finalize_compilation_unit (void)
{
  struct cgraph_node *node;

  if (!flag_unit_at_a_time)
    {
      cgraph_assemble_pending_functions ();
      return;
    }

  cgraph_varpool_assemble_pending_decls ();
  if (!quiet_flag)
    fprintf (stderr, "\nAnalyzing compilation unit\n");

  timevar_push (TV_CGRAPH);
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Initial entry points:");
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

      /* ??? It is possible to create extern inline function and later using
	 weak alas attribute to kill it's body. See
	 gcc.c-torture/compile/20011119-1.c  */
      if (!DECL_SAVED_TREE (decl))
	continue;

      if (node->analyzed || !node->reachable || !DECL_SAVED_TREE (decl))
	abort ();

      cgraph_analyze_function (node);

      for (edge = node->callees; edge; edge = edge->next_callee)
	if (!edge->callee->reachable)
	  cgraph_mark_reachable_node (edge->callee);

      cgraph_varpool_assemble_pending_decls ();
    }

  /* Collect entry points to the unit.  */

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Unit entry points:");
      for (node = cgraph_nodes; node; node = node->next)
	if (node->needed && DECL_SAVED_TREE (node->decl))
	  fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
      fprintf (cgraph_dump_file, "\n\nInitial ");
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
      else
	node->next_needed = NULL;
    }
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "\n\nReclaimed ");
      dump_cgraph (cgraph_dump_file);
    }
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
	if (e->inline_failed)
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
      else
        DECL_SAVED_INSNS (decl) = NULL;
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
    {
      struct cgraph_edge *e;

      for (e = node->callees; e; e = e->next_callee)
	if (!e->inline_failed || warn_inline
	    || (DECL_DECLARED_INLINE_P (e->callee->decl)
		&& lookup_attribute ("always_inline",
				     DECL_ATTRIBUTES (e->callee->decl))))
	  break;
      if (e)
        optimize_inline_calls (decl);
    }
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

  if (flag_unit_at_a_time)
    announce_function (decl);

  cgraph_optimize_function (node);

  /* Generate RTL for the body of DECL.  Nested functions are expanded
     via lang_expand_decl_stmt.  */
  (*lang_hooks.callgraph.expand_function) (decl);
  if (DECL_DEFER_OUTPUT (decl))
    abort ();

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
    if (!e->inline_failed)
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
	if (!e1->inline_failed)
	  break;

      if (e1)
	stack[sp++] = e1;
      else
	{
	  while (true)
	    {
	      for (e1 = e->next_caller; e1; e1 = e1->next_caller)
		if (!e1->inline_failed)
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
      fprintf (cgraph_dump_file, " Found inline predecesors of %s:",
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
    if (!e->inline_failed)
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
	if (!e1->inline_failed)
	  break;
      if (e1)
	stack[sp++] = e1;
      else
	{
	  while (true)
	    {
	      for (e1 = e->next_callee; e1; e1 = e1->next_callee)
		if (!e1->inline_failed)
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
      fprintf (cgraph_dump_file, " Found inline successors of %s:",
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

/* Perform reachability analysis and reclaim all unreachable nodes.
   This function also remove unneeded bodies of extern inline functions
   and thus needs to be done only after inlining decisions has been made.  */
static bool
cgraph_remove_unreachable_nodes (void)
{
  struct cgraph_node *first = (void *) 1;
  struct cgraph_node *node;
  bool changed = false;
  int insns = 0;

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nReclaiming functions:");
#ifdef ENABLE_CHECKING
  for (node = cgraph_nodes; node; node = node->next)
    if (node->aux)
      abort ();
#endif
  for (node = cgraph_nodes; node; node = node->next)
    if (node->needed && (!DECL_EXTERNAL (node->decl) || !node->analyzed))
      {
	node->aux = first;
	first = node;
      }
    else if (node->aux)
      abort ();

  /* Perform reachability analysis.  As a special case do not consider
     extern inline functions not inlined as live because we won't output
     them at all.  */
  while (first != (void *) 1)
    {
      struct cgraph_edge *e;
      node = first;
      first = first->aux;

      for (e = node->callees; e; e = e->next_callee)
	if (!e->callee->aux
	    && node->analyzed
	    && (!e->inline_failed || !e->callee->analyzed
		|| !DECL_EXTERNAL (e->callee->decl)))
	  {
	    e->callee->aux = first;
	    first = e->callee;
	  }
    }

  /* Remove unreachable nodes.  Extern inline functions need special care;
     Unreachable extern inline functions shall be removed.
     Reachable extern inline functions we never inlined shall get their bodies
     elliminated
     Reachable extern inline functions we sometimes inlined will be turned into
     unanalyzed nodes so they look like for true extern functions to the rest
     of code.  */
  for (node = cgraph_nodes; node; node = node->next)
    {
      if (!node->aux)
	{
	  int local_insns;
	  tree decl = node->decl;

	  if (DECL_SAVED_INSNS (decl))
	    local_insns = node->local.self_insns;
	  else
	    local_insns = 0;
	  if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
	  if (!node->analyzed || !DECL_EXTERNAL (node->decl))
	    cgraph_remove_node (node);
	  else
	    {
	      struct cgraph_edge *e;

	      for (e = node->callers; e; e = e->next_caller)
		if (e->caller->aux)
		  break;
	      if (e || node->needed)
		{
		  DECL_SAVED_TREE (node->decl) = NULL_TREE;
		  while (node->callees)
		    cgraph_remove_edge (node, node->callees->callee);
		  node->analyzed = false;
		}
	      else
		cgraph_remove_node (node);
	    }
	  if (!DECL_SAVED_TREE (decl))
	    insns += local_insns;
	  changed = true;
	}
    }
  for (node = cgraph_nodes; node; node = node->next)
    node->aux = NULL;
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nReclaimed %i insns", insns);
  return changed;
}


/* Estimate size of the function after inlining WHAT into TO.  */

static int
cgraph_estimate_size_after_inlining (int times, struct cgraph_node *to,
				     struct cgraph_node *what)
{
  return (what->global.insns - INSNS_PER_CALL) * times + to->global.insns;
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
    if (e->inline_failed)
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

  what->global.inlined = 1;
  for (e = what->callers; e; e = e->next_caller)
    {
      if (e->caller == to)
	{
	  if (!e->inline_failed)
	    continue;
	  e->inline_failed = NULL;
	  times++;
	  clones += e->caller->global.cloned_times;
	}
      else if (e->inline_failed)
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
      && flag_unit_at_a_time
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
			    struct cgraph_node **inlined, int ninlined,
			    const char **reason)
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
    {
      *reason = N_("--param large-function-growth limit reached");
      return false;
    }
  for (i = 0; i < ninlined; i++)
    {
      newsize =
	cgraph_estimate_size_after_inlining (INLINED_TIMES (inlined[i]) *
					     times, inlined[i], what);
      if (newsize > PARAM_VALUE (PARAM_LARGE_FUNCTION_INSNS)
	  && newsize >
	  inlined[i]->local.self_insns *
	  (100 + PARAM_VALUE (PARAM_LARGE_FUNCTION_GROWTH)) / 100)
	{
	  *reason = N_("--param large-function-growth limit reached while inlining the caller");
	  return false;
	}
    }
  return true;
}

/* Return true when function N is small enough to be inlined.  */

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

/* Set inline_failed for all callers of given function to REASON.  */

static void
cgraph_set_inline_failed (struct cgraph_node *node, const char *reason)
{
  struct cgraph_edge *e;

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Inlining failed: %s\n", reason);
  for (e = node->callers; e; e = e->next_caller)
    if (e->inline_failed)
      e->inline_failed = reason;
}

/* We use greedy algorithm for inlining of small functions:
   All inline candidates are put into prioritized heap based on estimated
   growth of the overall number of instructions and then update the estimates.

   INLINED and INLINED_CALEES are just pointers to arrays large enough
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
      if (!node->local.inlinable || !node->callers
	  || node->local.disregard_inline_limits)
	continue;

      if (!cgraph_default_inline_p (node))
	{
	  cgraph_set_inline_failed (node,
	    N_("--param max-inline-insns-single limit reached"));
	  continue;
	}
      heap_node[node->uid] =
	fibheap_insert (heap, cgraph_estimate_growth (node), node);
    }

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nDeciding on smaller functions:\n");
  while (overall_insns <= max_insns && (node = fibheap_extract_min (heap)))
    {
      struct cgraph_edge *e;
      int old_insns = overall_insns;

      heap_node[node->uid] = NULL;
      if (cgraph_dump_file)
	fprintf (cgraph_dump_file, 
		 "\nConsidering %s with %i insns\n"
		 " Estimated growth is %+i insns.\n",
		 cgraph_node_name (node), node->global.insns,
		 cgraph_estimate_growth (node));
      if (!cgraph_default_inline_p (node))
	{
	  cgraph_set_inline_failed (node,
	    N_("--param max-inline-insns-single limit reached after inlining into the callee"));
	  continue;
	}
      ninlined_callees = cgraph_inlined_callees (node, inlined_callees);
      for (e = node->callers; e; e = e->next_caller)
	if (e->inline_failed)
	  {
	    /* Marking recursive function inlinine has sane semantic and
	       thus we should not warn on it.  */
 	    if (e->caller == node)
 	      {
 	        e->inline_failed = "";
 		continue;
 	      }
	    ninlined = cgraph_inlined_into (e->caller, inlined);
	    if (e->callee->output)
	      e->inline_failed = "";
	    if (e->callee->output
		|| !cgraph_check_inline_limits (e->caller, node, inlined,
						ninlined, &e->inline_failed))
	      {
		for (i = 0; i < ninlined; i++)
		  inlined[i]->output = 0, inlined[i]->aux = 0;
		if (cgraph_dump_file)
		  fprintf (cgraph_dump_file, " Not inlining into %s.\n",
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
		inlined[i]->output = 0, inlined[i]->aux = 0;
		if (heap_node[inlined[i]->uid])
		  fibheap_replace_key (heap, heap_node[inlined[i]->uid],
				       cgraph_estimate_growth (inlined[i]));
	      }
	    if (cgraph_dump_file)
	      fprintf (cgraph_dump_file, 
		       " Inlined into %s which now has %i insns.\n",
		       cgraph_node_name (e->caller),
		       e->caller->global.insns);
	  }

      /* Similarly all functions called by the function we just inlined
         are now called more times; update keys.  */

      for (e = node->callees; e; e = e->next_callee)
	if (e->inline_failed && heap_node[e->callee->uid])
	  fibheap_replace_key (heap, heap_node[e->callee->uid],
			       cgraph_estimate_growth (e->callee));

      for (i = 0; i < ninlined_callees; i++)
	{
	  struct cgraph_edge *e;

	  for (e = inlined_callees[i]->callees; e; e = e->next_callee)
	    if (e->inline_failed && heap_node[e->callee->uid])
	      fibheap_replace_key (heap, heap_node[e->callee->uid],
				   cgraph_estimate_growth (e->callee));

	  inlined_callees[i]->output = 0;
	  inlined_callees[i]->aux = 0;
	}
      if (cgraph_dump_file)
	fprintf (cgraph_dump_file, 
		 " Inlined %i times for a net change of %+i insns.\n",
		 node->global.cloned_times, overall_insns - old_insns);
    }
  while ((node = fibheap_extract_min (heap)) != NULL)
    if (!node->local.disregard_inline_limits)
      cgraph_set_inline_failed (node, N_("--param inline-unit-growth limit reached"));
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
  int old_insns = 0;
  int i, y;

  for (node = cgraph_nodes; node; node = node->next)
    initial_insns += node->local.self_insns;
  overall_insns = initial_insns;

  nnodes = cgraph_postorder (order);

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file,
	     "\nDeciding on inlining.  Starting with %i insns.\n",
	     initial_insns);

  for (node = cgraph_nodes; node; node = node->next)
    node->aux = 0;

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nInlining always_inline functions:\n");
#ifdef ENABLE_CHECKING
  for (node = cgraph_nodes; node; node = node->next)
    if (node->aux || node->output)
      abort ();
#endif

  /* In the first pass mark all always_inline edges.  Do this with a priority
     so none of our later choices will make this impossible.  */
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
		 "\nConsidering %s %i insns (always inline)\n",
		 cgraph_node_name (e->callee), e->callee->global.insns);
      ninlined = cgraph_inlined_into (order[i], inlined);
      for (; e; e = e->next_callee)
	{
	  old_insns = overall_insns;
	  if (!e->inline_failed || !e->callee->local.inlinable
	      || !e->callee->local.disregard_inline_limits)
  	    continue;
  	  if (e->callee->output || e->callee == node)
	    {
	      e->inline_failed = N_("recursive inlining");
	      continue;
	    }
	  ninlined_callees =
	    cgraph_inlined_callees (e->callee, inlined_callees);
	  cgraph_mark_inline (node, e->callee, inlined, ninlined,
			      inlined_callees, ninlined_callees);
	  for (y = 0; y < ninlined_callees; y++)
	    inlined_callees[y]->output = 0, inlined_callees[y]->aux = 0;
	  if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, 
		     " Inlined into %s which now has %i insns.\n",
		     cgraph_node_name (node->callees->caller),
	             node->callees->caller->global.insns);
	}
      if (cgraph_dump_file && node->global.cloned_times > 0)
	fprintf (cgraph_dump_file, 
		 " Inlined %i times for a net change of %+i insns.\n",
		 node->global.cloned_times, overall_insns - old_insns);
      for (y = 0; y < ninlined; y++)
	inlined[y]->output = 0, inlined[y]->aux = 0;
    }
#ifdef ENABLE_CHECKING
  for (node = cgraph_nodes; node; node = node->next)
    if (node->aux || node->output)
      abort ();
#endif

  if (!flag_really_no_inline)
    {
      cgraph_decide_inlining_of_small_functions (inlined, inlined_callees);
#ifdef ENABLE_CHECKING
      for (node = cgraph_nodes; node; node = node->next)
	if (node->aux || node->output)
	  abort ();
#endif

      if (cgraph_dump_file)
	fprintf (cgraph_dump_file, "\nDeciding on functions called once:\n");

      /* And finally decide what functions are called once.  */

      for (i = nnodes - 1; i >= 0; i--)
	{
	  node = order[i];

	  if (node->callers && !node->callers->next_caller && !node->needed
	      && node->local.inlinable && node->callers->inline_failed
	      && !DECL_EXTERNAL (node->decl) && !DECL_COMDAT (node->decl))
	    {
	      bool ok = true;
	      struct cgraph_node *node1;

	      /* Verify that we won't duplicate the caller.  */
	      for (node1 = node->callers->caller;
		   node1->callers && !node1->callers->inline_failed
		   && ok; node1 = node1->callers->caller)
		if (node1->callers->next_caller || node1->needed)
		  ok = false;
	      if (ok)
		{
		  const char *dummy_reason;
		  if (cgraph_dump_file)
		    fprintf (cgraph_dump_file,
			     "\nConsidering %s %i insns.\n"
			     " Called once from %s %i insns.\n",
			     cgraph_node_name (node), node->global.insns,
			     cgraph_node_name (node->callers->caller),
			     node->callers->caller->global.insns);
		  ninlined = cgraph_inlined_into (node->callers->caller,
		      				  inlined);
		  old_insns = overall_insns;

		  /* Inlining functions once would never cause inlining warnings.  */
		  if (cgraph_check_inline_limits
		      (node->callers->caller, node, inlined, ninlined,
		       &dummy_reason))
		    {
		      ninlined_callees =
			cgraph_inlined_callees (node, inlined_callees);
		      cgraph_mark_inline (node->callers->caller, node, inlined,
					  ninlined, inlined_callees,
					  ninlined_callees);
		      for (y = 0; y < ninlined_callees; y++)
			inlined_callees[y]->output = 0, inlined_callees[y]->aux = 0;
		      if (cgraph_dump_file)
			fprintf (cgraph_dump_file,
				 " Inlined into %s which now has %i insns"
				 " for a net change of %+i insns.\n",
				 cgraph_node_name (node->callers->caller),
				 node->callers->caller->global.insns,
				 overall_insns - old_insns);
		    }
		  else
		    {
		      if (cgraph_dump_file)
			fprintf (cgraph_dump_file,
				 " Inline limit reached, not inlined.\n");
		    }
		  for (y = 0; y < ninlined; y++)
		    inlined[y]->output = 0, inlined[y]->aux = 0;
		}
	    }
	}
    }
  cgraph_remove_unreachable_nodes ();

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file,
	     "\nInlined %i calls, eliminated %i functions, "
	     "%i insns turned to %i insns.\n\n",
	     ncalls_inlined, nfunctions_inlined, initial_insns,
	     overall_insns);
  free (order);
  free (inlined);
  free (inlined_callees);
}

/* Decide on the inlining.  We do so in the topological order to avoid
   expenses on updating datastructures.  */

static void
cgraph_decide_inlining_incrementally (struct cgraph_node *node)
{
  struct cgraph_edge *e;
  struct cgraph_node **inlined =
    xmalloc (sizeof (struct cgraph_node *) * cgraph_n_nodes);
  struct cgraph_node **inlined_callees =
    xmalloc (sizeof (struct cgraph_node *) * cgraph_n_nodes);
  int ninlined;
  int ninlined_callees;
  int y;

  ninlined = cgraph_inlined_into (node, inlined);

  /* First of all look for always inline functions.  */
  for (e = node->callees; e; e = e->next_callee)
    if (e->callee->local.disregard_inline_limits && e->inline_failed
	/* ??? It is possible that renaming variable removed the function body
	   in duplicate_decls. See gcc.c-torture/compile/20011119-2.c  */
	&& DECL_SAVED_TREE (e->callee->decl))
      {
	if (e->callee->output || e->callee == node)
	  {
 	    e->inline_failed = N_("recursive inlining");
	    continue;
	  }
	ninlined_callees = cgraph_inlined_callees (e->callee, inlined_callees);
	cgraph_mark_inline (node, e->callee, inlined, ninlined,
			    inlined_callees, ninlined_callees);
	for (y = 0; y < ninlined_callees; y++)
	  inlined_callees[y]->output = 0, inlined_callees[y]->aux = 0;
      }

  if (!flag_really_no_inline)
    {
      /* Now do the automatic inlining.  */
      for (e = node->callees; e; e = e->next_callee)
	if (e->callee->local.inlinable && e->inline_failed
	    && cgraph_default_inline_p (e->callee)
	    && cgraph_check_inline_limits (node, e->callee, inlined,
					   ninlined, &e->inline_failed)
	    && DECL_SAVED_TREE (e->callee->decl))
	  {
	    /* Marking recursive function inlinine has sane semantic and thus
	       we should not warn on it.  */
	    if (e->callee->output || e->callee == node)
	      {
		e->inline_failed = "";
		continue;
	      }
	    ninlined_callees = cgraph_inlined_callees (e->callee,
						       inlined_callees);
	    cgraph_mark_inline (node, e->callee, inlined, ninlined,
				inlined_callees, ninlined_callees);
	    for (y = 0; y < ninlined_callees; y++)
	      inlined_callees[y]->output = 0, inlined_callees[y]->aux = 0;
	  }
    }

  /* Clear the flags set by cgraph_inlined_into.  */
  for (y = 0; y < ninlined; y++)
    inlined[y]->output = 0, inlined[y]->aux = 0;

  free (inlined);
  free (inlined_callees);
}


/* Return true when CALLER_DECL should be inlined into CALLEE_DECL.
   When returned false and reason is non-NULL, set it to the reason
   why the call was not inlined.  */

bool
cgraph_inline_p (tree caller_decl, tree callee_decl, const char **reason)
{
  struct cgraph_node *caller = cgraph_node (caller_decl);
  struct cgraph_node *callee = cgraph_node (callee_decl);
  struct cgraph_edge *e;

  for (e = caller->callees; e; e = e->next_callee)
    if (e->callee == callee)
      {
	if (e->inline_failed && reason)
	  *reason = e->inline_failed;
        return !e->inline_failed;
      }
  /* We do not record builtins in the callgraph.  Perhaps it would make more
     sense to do so and then prune out those not overwritten by explicit
     function body.  */
  if (reason)
    *reason = "originally indirect function calls never inlined";
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
cgraph_expand_all_functions (void)
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
   current compilation unit and all it's calls are explicit,
   so we can change its calling convention.
   We simply mark all static functions whose address is not taken
   as local.  */

static void
cgraph_mark_local_functions (void)
{
  struct cgraph_node *node;

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nMarking local functions:");

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
    fprintf (cgraph_dump_file, "\n\n");
}

/* Perform simple optimizations based on callgraph.  */

void
cgraph_optimize (void)
{
  if (!flag_unit_at_a_time)
    return;
  timevar_push (TV_CGRAPHOPT);
  if (!quiet_flag)
    fprintf (stderr, "Performing intraprocedural optimizations\n");

  cgraph_mark_local_functions ();
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Marked ");
      dump_cgraph (cgraph_dump_file);
    }

  cgraph_decide_inlining ();
  cgraph_global_info_ready = true;
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Optimized ");
      dump_cgraph (cgraph_dump_file);
    }
  timevar_pop (TV_CGRAPHOPT);

  /* Output everything.  */
  if (!quiet_flag)
    fprintf (stderr, "Assembling functions:\n");
  cgraph_expand_all_functions ();
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "\nFinal ");
      dump_cgraph (cgraph_dump_file);
    }
}
