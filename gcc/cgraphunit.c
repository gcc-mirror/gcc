/* Callgraph based interprocedural optimizations.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
   2011 Free Software Foundation, Inc.
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

/* This module implements main driver of compilation process as well as
   few basic interprocedural optimizers.

   The main scope of this file is to act as an interface in between
   tree based frontends and the backend (and middle end)

   The front-end is supposed to use following functionality:

    - cgraph_finalize_function

      This function is called once front-end has parsed whole body of function
      and it is certain that the function body nor the declaration will change.

      (There is one exception needed for implementing GCC extern inline
	function.)

    - varpool_finalize_variable

      This function has same behavior as the above but is used for static
      variables.

    - cgraph_finalize_compilation_unit

      This function is called once (source level) compilation unit is finalized
      and it will no longer change.

      In the call-graph construction and local function analysis takes
      place here.  Bodies of unreachable functions are released to
      conserve memory usage.

      The function can be called multiple times when multiple source level
      compilation units are combined (such as in C frontend)

    - cgraph_optimize

      In this unit-at-a-time compilation the intra procedural analysis takes
      place here.  In particular the static functions whose address is never
      taken are marked as local.  Backend can then use this information to
      modify calling conventions, do better inlining or similar optimizations.

    - cgraph_mark_needed_node
    - varpool_mark_needed_node

      When function or variable is referenced by some hidden way the call-graph
      data structure must be updated accordingly by this function.
      There should be little need to call this function and all the references
      should be made explicit to cgraph code.  At present these functions are
      used by C++ frontend to explicitly mark the keyed methods.

    - analyze_expr callback

      This function is responsible for lowering tree nodes not understood by
      generic code into understandable ones or alternatively marking
      callgraph and varpool nodes referenced by the as needed.

      ??? On the tree-ssa genericizing should take place here and we will avoid
      need for these hooks (replacing them by genericizing hook)

        Analyzing of all functions is deferred
	to cgraph_finalize_compilation_unit and expansion into cgraph_optimize.

	In cgraph_finalize_compilation_unit the reachable functions are
	analyzed.  During analysis the call-graph edges from reachable
	functions are constructed and their destinations are marked as
	reachable.  References to functions and variables are discovered too
	and variables found to be needed output to the assembly file.  Via
	mark_referenced call in assemble_variable functions referenced by
	static variables are noticed too.

	The intra-procedural information is produced and its existence
	indicated by global_info_ready.  Once this flag is set it is impossible
	to change function from !reachable to reachable and thus
	assemble_variable no longer call mark_referenced.

	Finally the call-graph is topologically sorted and all reachable functions
	that has not been completely inlined or are not external are output.

	??? It is possible that reference to function or variable is optimized
	out.  We can not deal with this nicely because topological order is not
	suitable for it.  For tree-ssa we may consider another pass doing
	optimization and re-discovering reachable functions.

	??? Reorganize code so variables are output very last and only if they
	really has been referenced by produced code, so we catch more cases
	where reference has been optimized out.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "toplev.h"
#include "flags.h"
#include "ggc.h"
#include "debug.h"
#include "target.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "timevar.h"
#include "params.h"
#include "fibheap.h"
#include "intl.h"
#include "function.h"
#include "ipa-prop.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "output.h"
#include "coverage.h"
#include "plugin.h"
#include "ipa-inline.h"
#include "ipa-utils.h"
#include "lto-streamer.h"

static void cgraph_expand_all_functions (void);
static void cgraph_mark_functions_to_output (void);
static void cgraph_expand_function (struct cgraph_node *);
static void cgraph_output_pending_asms (void);

FILE *cgraph_dump_file;

/* Used for vtable lookup in thunk adjusting.  */
static GTY (()) tree vtable_entry_type;

/* Determine if function DECL is needed.  That is, visible to something
   either outside this translation unit, something magic in the system
   configury.  */

bool
cgraph_decide_is_function_needed (struct cgraph_node *node, tree decl)
{
  /* If the user told us it is used, then it must be so.  */
  if (node->local.externally_visible)
    return true;

  /* ??? If the assembler name is set by hand, it is possible to assemble
     the name later after finalizing the function and the fact is noticed
     in assemble_name then.  This is arguably a bug.  */
  if (DECL_ASSEMBLER_NAME_SET_P (decl)
      && (!node->thunk.thunk_p && !node->same_body_alias)
      && TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
    return true;

  /* With -fkeep-inline-functions we are keeping all inline functions except
     for extern inline ones.  */
  if (flag_keep_inline_functions
      && DECL_DECLARED_INLINE_P (decl)
      && !DECL_EXTERNAL (decl)
      && !DECL_DISREGARD_INLINE_LIMITS (decl))
     return true;

  /* If we decided it was needed before, but at the time we didn't have
     the body of the function available, then it's still needed.  We have
     to go back and re-check its dependencies now.  */
  if (node->needed)
    return true;

  /* Externally visible functions must be output.  The exception is
     COMDAT functions that must be output only when they are needed.

     When not optimizing, also output the static functions. (see
     PR24561), but don't do so for always_inline functions, functions
     declared inline and nested functions.  These were optimized out
     in the original implementation and it is unclear whether we want
     to change the behavior here.  */
  if (((TREE_PUBLIC (decl)
	|| (!optimize
	    && !DECL_DISREGARD_INLINE_LIMITS (decl)
	    && !DECL_DECLARED_INLINE_P (decl)
	    && !(DECL_CONTEXT (decl)
		 && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL)))
       && !flag_whole_program
       && !flag_lto)
      && !DECL_COMDAT (decl) && !DECL_EXTERNAL (decl))
    return true;

  return false;
}

/* Process CGRAPH_NEW_FUNCTIONS and perform actions necessary to add these
   functions into callgraph in a way so they look like ordinary reachable
   functions inserted into callgraph already at construction time.  */

bool
cgraph_process_new_functions (void)
{
  bool output = false;
  tree fndecl;
  struct cgraph_node *node;

  varpool_analyze_pending_decls ();
  /*  Note that this queue may grow as its being processed, as the new
      functions may generate new ones.  */
  while (cgraph_new_nodes)
    {
      node = cgraph_new_nodes;
      fndecl = node->decl;
      cgraph_new_nodes = cgraph_new_nodes->next_needed;
      switch (cgraph_state)
	{
	case CGRAPH_STATE_CONSTRUCTION:
	  /* At construction time we just need to finalize function and move
	     it into reachable functions list.  */

	  node->next_needed = NULL;
	  cgraph_finalize_function (fndecl, false);
	  cgraph_mark_reachable_node (node);
	  output = true;
          cgraph_call_function_insertion_hooks (node);
	  break;

	case CGRAPH_STATE_IPA:
	case CGRAPH_STATE_IPA_SSA:
	  /* When IPA optimization already started, do all essential
	     transformations that has been already performed on the whole
	     cgraph but not on this function.  */

	  gimple_register_cfg_hooks ();
	  if (!node->analyzed)
	    cgraph_analyze_function (node);
	  push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	  current_function_decl = fndecl;
	  if ((cgraph_state == CGRAPH_STATE_IPA_SSA
	      && !gimple_in_ssa_p (DECL_STRUCT_FUNCTION (fndecl)))
	      /* When not optimizing, be sure we run early local passes anyway
		 to expand OMP.  */
	      || !optimize)
	    execute_pass_list (pass_early_local_passes.pass.sub);
	  else
	    compute_inline_parameters (node, true);
	  free_dominance_info (CDI_POST_DOMINATORS);
	  free_dominance_info (CDI_DOMINATORS);
	  pop_cfun ();
	  current_function_decl = NULL;
          cgraph_call_function_insertion_hooks (node);
	  break;

	case CGRAPH_STATE_EXPANSION:
	  /* Functions created during expansion shall be compiled
	     directly.  */
	  node->process = 0;
          cgraph_call_function_insertion_hooks (node);
	  cgraph_expand_function (node);
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
      varpool_analyze_pending_decls ();
    }
  return output;
}

/* As an GCC extension we allow redefinition of the function.  The
   semantics when both copies of bodies differ is not well defined.
   We replace the old body with new body so in unit at a time mode
   we always use new body, while in normal mode we may end up with
   old body inlined into some functions and new body expanded and
   inlined in others.

   ??? It may make more sense to use one body for inlining and other
   body for expanding the function but this is difficult to do.  */

static void
cgraph_reset_node (struct cgraph_node *node)
{
  /* If node->process is set, then we have already begun whole-unit analysis.
     This is *not* testing for whether we've already emitted the function.
     That case can be sort-of legitimately seen with real function redefinition
     errors.  I would argue that the front end should never present us with
     such a case, but don't enforce that for now.  */
  gcc_assert (!node->process);

  /* Reset our data structures so we can analyze the function again.  */
  memset (&node->local, 0, sizeof (node->local));
  memset (&node->global, 0, sizeof (node->global));
  memset (&node->rtl, 0, sizeof (node->rtl));
  node->analyzed = false;
  node->local.finalized = false;

  cgraph_node_remove_callees (node);
}

static void
cgraph_lower_function (struct cgraph_node *node)
{
  if (node->lowered)
    return;

  if (node->nested)
    lower_nested_functions (node->decl);
  gcc_assert (!node->nested);

  tree_lowering_passes (node->decl);
  node->lowered = true;
}

/* DECL has been parsed.  Take it, queue it, compile it at the whim of the
   logic in effect.  If NESTED is true, then our caller cannot stand to have
   the garbage collector run at the moment.  We would need to either create
   a new GC context, or just not compile right now.  */

void
cgraph_finalize_function (tree decl, bool nested)
{
  struct cgraph_node *node = cgraph_get_create_node (decl);

  if (node->local.finalized)
    {
      cgraph_reset_node (node);
      node->local.redefined_extern_inline = true;
    }

  notice_global_symbol (decl);
  node->local.finalized = true;
  node->lowered = DECL_STRUCT_FUNCTION (decl)->cfg != NULL;

  if (cgraph_decide_is_function_needed (node, decl))
    cgraph_mark_needed_node (node);

  /* Since we reclaim unreachable nodes at the end of every language
     level unit, we need to be conservative about possible entry points
     there.  */
  if ((TREE_PUBLIC (decl) && !DECL_COMDAT (decl) && !DECL_EXTERNAL (decl))
      || DECL_STATIC_CONSTRUCTOR (decl)
      || DECL_STATIC_DESTRUCTOR (decl)
      /* COMDAT virtual functions may be referenced by vtable from
	 other compilation unit.  Still we want to devirtualize calls
	 to those so we need to analyze them.
	 FIXME: We should introduce may edges for this purpose and update
	 their handling in unreachable function removal and inliner too.  */
      || (DECL_VIRTUAL_P (decl)
	  && optimize && (DECL_COMDAT (decl) || DECL_EXTERNAL (decl))))
    cgraph_mark_reachable_node (node);

  /* If we've not yet emitted decl, tell the debug info about it.  */
  if (!TREE_ASM_WRITTEN (decl))
    (*debug_hooks->deferred_inline_function) (decl);

  /* Possibly warn about unused parameters.  */
  if (warn_unused_parameter)
    do_warn_unused_parameter (decl);

  if (!nested)
    ggc_collect ();
}

/* C99 extern inline keywords allow changing of declaration after function
   has been finalized.  We need to re-decide if we want to mark the function as
   needed then.   */

void
cgraph_mark_if_needed (tree decl)
{
  struct cgraph_node *node = cgraph_get_node (decl);
  if (node->local.finalized && cgraph_decide_is_function_needed (node, decl))
    cgraph_mark_needed_node (node);
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
  if (gimple_has_body_p (e->caller->decl)
      && !e->caller->global.inlined_to
      /* FIXME: Inline-analysis sets frequency to 0 when edge is optimized out.
	 Remove this once edges are actualy removed from the function at that time.  */
      && (e->frequency
	  || (inline_edge_summary_vec
	      && ((VEC_length(inline_edge_summary_t, inline_edge_summary_vec)
		  <= (unsigned) e->uid)
	          || !inline_edge_summary (e)->predicate)))
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
  return error_found;
}

/* Switch to THIS_CFUN if needed and print STMT to stderr.  */
static void
cgraph_debug_gimple_stmt (struct function *this_cfun, gimple stmt)
{
  /* debug_gimple_stmt needs correct cfun */
  if (cfun != this_cfun)
    set_cfun (this_cfun);
  debug_gimple_stmt (stmt);
}

/* Verify that call graph edge E corresponds to DECL from the associated
   statement.  Return true if the verification should fail.  */

static bool
verify_edge_corresponds_to_fndecl (struct cgraph_edge *e, tree decl)
{
  struct cgraph_node *node;

  if (!decl || e->callee->global.inlined_to)
    return false;
  node = cgraph_get_node (decl);

  /* We do not know if a node from a different partition is an alias or what it
     aliases and therefore cannot do the former_clone_of check reliably.  */
  if (!node || node->in_other_partition)
    return false;
  node = cgraph_function_or_thunk_node (node, NULL);

  if ((e->callee->former_clone_of != node->decl)
      /* IPA-CP sometimes redirect edge to clone and then back to the former
	 function.  This ping-pong has to go, eventaully.  */
      && (node != cgraph_function_or_thunk_node (e->callee, NULL))
      && !clone_of_p (node, e->callee))
    return true;
  else
    return false;
}

/* Verify cgraph nodes of given cgraph node.  */
DEBUG_FUNCTION void
verify_cgraph_node (struct cgraph_node *node)
{
  struct cgraph_edge *e;
  struct function *this_cfun = DECL_STRUCT_FUNCTION (node->decl);
  basic_block this_block;
  gimple_stmt_iterator gsi;
  bool error_found = false;

  if (seen_error ())
    return;

  timevar_push (TV_CGRAPH_VERIFY);
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
  if (node->global.inlined_to && node->local.externally_visible)
    {
      error ("externally visible inline clone");
      error_found = true;
    }
  if (node->global.inlined_to && node->address_taken)
    {
      error ("inline clone with address taken");
      error_found = true;
    }
  if (node->global.inlined_to && node->needed)
    {
      error ("inline clone is needed");
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

  if (!cgraph_get_node (node->decl))
    {
      error ("node not found in cgraph_hash");
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
  if (node->same_comdat_group)
    {
      struct cgraph_node *n = node->same_comdat_group;

      if (!DECL_ONE_ONLY (node->decl))
	{
	  error ("non-DECL_ONE_ONLY node in a same_comdat_group list");
	  error_found = true;
	}
      if (n == node)
	{
	  error ("node is alone in a comdat group");
	  error_found = true;
	}
      do
	{
	  if (!n->same_comdat_group)
	    {
	      error ("same_comdat_group is not a circular list");
	      error_found = true;
	      break;
	    }
	  n = n->same_comdat_group;
	}
      while (n != node);
    }

  if (node->analyzed && node->alias)
    {
      bool ref_found = false;
      int i;
      struct ipa_ref *ref;

      if (node->callees)
	{
	  error ("Alias has call edges");
          error_found = true;
	}
      for (i = 0; ipa_ref_list_reference_iterate (&node->ref_list, i, ref); i++)
	if (ref->use != IPA_REF_ALIAS)
	  {
	    error ("Alias has non-alias refernece");
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
  if (node->analyzed && node->thunk.thunk_p)
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
      if (gimple_has_body_p (node->decl))
        {
	  error ("Thunk is not supposed to have body");
          error_found = true;
        }
    }
  else if (node->analyzed && gimple_has_body_p (node->decl)
           && !TREE_ASM_WRITTEN (node->decl)
           && (!DECL_EXTERNAL (node->decl) || node->global.inlined_to)
           && !flag_wpa)
    {
      if (this_cfun->cfg)
	{
	  /* The nodes we're interested in are never shared, so walk
	     the tree ignoring duplicates.  */
	  struct pointer_set_t *visited_nodes = pointer_set_create ();
	  /* Reach the trees by walking over the CFG, and note the
	     enclosing basic-blocks in the call edges.  */
	  FOR_EACH_BB_FN (this_block, this_cfun)
	    for (gsi = gsi_start_bb (this_block);
                 !gsi_end_p (gsi);
                 gsi_next (&gsi))
	      {
		gimple stmt = gsi_stmt (gsi);
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
	  pointer_set_destroy (visited_nodes);
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
	  if (!e->aux)
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

  for (node = cgraph_nodes; node; node = node->next)
    verify_cgraph_node (node);
}

/* Output all asm statements we have stored up to be output.  */

static void
cgraph_output_pending_asms (void)
{
  struct cgraph_asm_node *can;

  if (seen_error ())
    return;

  for (can = cgraph_asm_nodes; can; can = can->next)
    assemble_asm (can->asm_str);
  cgraph_asm_nodes = NULL;
}

/* Analyze the function scheduled to be output.  */
void
cgraph_analyze_function (struct cgraph_node *node)
{
  tree save = current_function_decl;
  tree decl = node->decl;

  if (node->alias && node->thunk.alias)
    {
      struct cgraph_node *tgt = cgraph_get_node (node->thunk.alias);
      if (!VEC_length (ipa_ref_t, node->ref_list.references))
        ipa_record_reference (node, NULL, tgt, NULL, IPA_REF_ALIAS, NULL);
      if (node->same_body_alias)
	{ 
	  DECL_VIRTUAL_P (node->decl) = DECL_VIRTUAL_P (node->thunk.alias);
	  DECL_DECLARED_INLINE_P (node->decl)
	     = DECL_DECLARED_INLINE_P (node->thunk.alias);
	  DECL_DISREGARD_INLINE_LIMITS (node->decl)
	     = DECL_DISREGARD_INLINE_LIMITS (node->thunk.alias);
	}

      /* Fixup visibility nonsences C++ frontend produce on same body aliases.  */
      if (TREE_PUBLIC (node->decl) && node->same_body_alias)
	{
          DECL_EXTERNAL (node->decl) = DECL_EXTERNAL (node->thunk.alias);
	  if (DECL_ONE_ONLY (node->thunk.alias))
	    {
	      DECL_COMDAT (node->decl) = DECL_COMDAT (node->thunk.alias);
	      DECL_COMDAT_GROUP (node->decl) = DECL_COMDAT_GROUP (node->thunk.alias);
	      if (DECL_ONE_ONLY (node->thunk.alias) && !node->same_comdat_group)
		{
		  struct cgraph_node *tgt = cgraph_get_node (node->thunk.alias);
		  node->same_comdat_group = tgt;
		  if (!tgt->same_comdat_group)
		    tgt->same_comdat_group = node;
		  else
		    {
		      struct cgraph_node *n;
		      for (n = tgt->same_comdat_group;
			   n->same_comdat_group != tgt;
			   n = n->same_comdat_group)
			;
		      n->same_comdat_group = node;
		    }
		}
	    }
	}
      cgraph_mark_reachable_node (cgraph_alias_aliased_node (node));
      if (node->address_taken)
	cgraph_mark_address_taken_node (cgraph_alias_aliased_node (node));
      if (cgraph_decide_is_function_needed (node, node->decl))
	cgraph_mark_needed_node (node);
    }
  else if (node->thunk.thunk_p)
    {
      cgraph_create_edge (node, cgraph_get_node (node->thunk.alias),
			  NULL, 0, CGRAPH_FREQ_BASE);
    }
  else
    {
      current_function_decl = decl;
      push_cfun (DECL_STRUCT_FUNCTION (decl));

      assign_assembler_name_if_neeeded (node->decl);

      /* Make sure to gimplify bodies only once.  During analyzing a
	 function we lower it, which will require gimplified nested
	 functions, so we can end up here with an already gimplified
	 body.  */
      if (!gimple_body (decl))
	gimplify_function_tree (decl);
      dump_function (TDI_generic, decl);

      cgraph_lower_function (node);
      pop_cfun ();
    }
  node->analyzed = true;

  current_function_decl = save;
}

/* C++ frontend produce same body aliases all over the place, even before PCH
   gets streamed out. It relies on us linking the aliases with their function
   in order to do the fixups, but ipa-ref is not PCH safe.  Consequentely we
   first produce aliases without links, but once C++ FE is sure he won't sream
   PCH we build the links via this function.  */

void
cgraph_process_same_body_aliases (void)
{
  struct cgraph_node *node;
  for (node = cgraph_nodes; node; node = node->next)
    if (node->same_body_alias
	&& !VEC_length (ipa_ref_t, node->ref_list.references))
      {
        struct cgraph_node *tgt = cgraph_get_node (node->thunk.alias);
        ipa_record_reference (node, NULL, tgt, NULL, IPA_REF_ALIAS, NULL);
      }
  same_body_aliases_done = true;
}

/* Process attributes common for vars and functions.  */

static void
process_common_attributes (tree decl)
{
  tree weakref = lookup_attribute ("weakref", DECL_ATTRIBUTES (decl));

  if (weakref && !lookup_attribute ("alias", DECL_ATTRIBUTES (decl)))
    {
      warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wattributes,
		  "%<weakref%> attribute should be accompanied with"
		  " an %<alias%> attribute");
      DECL_WEAK (decl) = 0;
      DECL_ATTRIBUTES (decl) = remove_attribute ("weakref",
						 DECL_ATTRIBUTES (decl));
    }
}

/* Look for externally_visible and used attributes and mark cgraph nodes
   accordingly.

   We cannot mark the nodes at the point the attributes are processed (in
   handle_*_attribute) because the copy of the declarations available at that
   point may not be canonical.  For example, in:

    void f();
    void f() __attribute__((used));

   the declaration we see in handle_used_attribute will be the second
   declaration -- but the front end will subsequently merge that declaration
   with the original declaration and discard the second declaration.

   Furthermore, we can't mark these nodes in cgraph_finalize_function because:

    void f() {}
    void f() __attribute__((externally_visible));

   is valid.

   So, we walk the nodes at the end of the translation unit, applying the
   attributes at that point.  */

static void
process_function_and_variable_attributes (struct cgraph_node *first,
                                          struct varpool_node *first_var)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;

  for (node = cgraph_nodes; node != first; node = node->next)
    {
      tree decl = node->decl;
      if (DECL_PRESERVE_P (decl))
	cgraph_mark_needed_node (node);
      if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	  && lookup_attribute ("dllexport", DECL_ATTRIBUTES (decl))
	  && TREE_PUBLIC (node->decl))
	{
	  if (node->local.finalized)
	    cgraph_mark_needed_node (node);
	}
      else if (lookup_attribute ("externally_visible", DECL_ATTRIBUTES (decl)))
	{
	  if (! TREE_PUBLIC (node->decl))
	    warning_at (DECL_SOURCE_LOCATION (node->decl), OPT_Wattributes,
			"%<externally_visible%>"
			" attribute have effect only on public objects");
	  else if (node->local.finalized)
	     cgraph_mark_needed_node (node);
	}
      if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl))
	  && (node->local.finalized && !node->alias))
	{
	  warning_at (DECL_SOURCE_LOCATION (node->decl), OPT_Wattributes,
		      "%<weakref%> attribute ignored"
		      " because function is defined");
	  DECL_WEAK (decl) = 0;
	  DECL_ATTRIBUTES (decl) = remove_attribute ("weakref",
						     DECL_ATTRIBUTES (decl));
	}

      if (lookup_attribute ("always_inline", DECL_ATTRIBUTES (decl))
	  && !DECL_DECLARED_INLINE_P (decl)
	  /* redefining extern inline function makes it DECL_UNINLINABLE.  */
	  && !DECL_UNINLINABLE (decl))
	warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wattributes,
		    "always_inline function might not be inlinable");
     
      process_common_attributes (decl);
    }
  for (vnode = varpool_nodes; vnode != first_var; vnode = vnode->next)
    {
      tree decl = vnode->decl;
      if (DECL_PRESERVE_P (decl))
	{
	  vnode->force_output = true;
	  if (vnode->finalized)
	    varpool_mark_needed_node (vnode);
	}
      if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	  && lookup_attribute ("dllexport", DECL_ATTRIBUTES (decl))
	  && TREE_PUBLIC (vnode->decl))
	{
	  if (vnode->finalized)
	    varpool_mark_needed_node (vnode);
	}
      else if (lookup_attribute ("externally_visible", DECL_ATTRIBUTES (decl)))
	{
	  if (! TREE_PUBLIC (vnode->decl))
	    warning_at (DECL_SOURCE_LOCATION (vnode->decl), OPT_Wattributes,
			"%<externally_visible%>"
			" attribute have effect only on public objects");
	  else if (vnode->finalized)
	    varpool_mark_needed_node (vnode);
	}
      if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl))
	  && vnode->finalized
	  && DECL_INITIAL (decl))
	{
	  warning_at (DECL_SOURCE_LOCATION (vnode->decl), OPT_Wattributes,
		      "%<weakref%> attribute ignored"
		      " because variable is initialized");
	  DECL_WEAK (decl) = 0;
	  DECL_ATTRIBUTES (decl) = remove_attribute ("weakref",
						      DECL_ATTRIBUTES (decl));
	}
      process_common_attributes (decl);
    }
}

/* Process CGRAPH_NODES_NEEDED queue, analyze each function (and transitively
   each reachable functions) and build cgraph.
   The function can be called multiple times after inserting new nodes
   into beginning of queue.  Just the new part of queue is re-scanned then.  */

static void
cgraph_analyze_functions (void)
{
  /* Keep track of already processed nodes when called multiple times for
     intermodule optimization.  */
  static struct cgraph_node *first_analyzed;
  struct cgraph_node *first_processed = first_analyzed;
  static struct varpool_node *first_analyzed_var;
  struct cgraph_node *node, *next;

  bitmap_obstack_initialize (NULL);
  process_function_and_variable_attributes (first_processed,
					    first_analyzed_var);
  first_processed = cgraph_nodes;
  first_analyzed_var = varpool_nodes;
  varpool_analyze_pending_decls ();
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Initial entry points:");
      for (node = cgraph_nodes; node != first_analyzed; node = node->next)
	if (node->needed)
	  fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
      fprintf (cgraph_dump_file, "\n");
    }
  cgraph_process_new_functions ();

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
      node->next_needed = NULL;

      /* ??? It is possible to create extern inline function and later using
	 weak alias attribute to kill its body. See
	 gcc.c-torture/compile/20011119-1.c  */
      if (!DECL_STRUCT_FUNCTION (decl)
	  && (!node->alias || !node->thunk.alias)
	  && !node->thunk.thunk_p)
	{
	  cgraph_reset_node (node);
          node->local.redefined_extern_inline = true;
	  continue;
	}

      if (!node->analyzed)
	cgraph_analyze_function (node);

      for (edge = node->callees; edge; edge = edge->next_callee)
	if (!edge->callee->reachable)
	  cgraph_mark_reachable_node (edge->callee);
      for (edge = node->callers; edge; edge = edge->next_caller)
	if (!edge->caller->reachable && edge->caller->thunk.thunk_p)
	  cgraph_mark_reachable_node (edge->caller);

      if (node->same_comdat_group)
	{
	  for (next = node->same_comdat_group;
	       next != node;
	       next = next->same_comdat_group)
	    cgraph_mark_reachable_node (next);
	}

      /* If decl is a clone of an abstract function, mark that abstract
	 function so that we don't release its body. The DECL_INITIAL() of that
	 abstract function declaration will be later needed to output debug
	 info.  */
      if (DECL_ABSTRACT_ORIGIN (decl))
	{
	  struct cgraph_node *origin_node;
	  origin_node = cgraph_get_node (DECL_ABSTRACT_ORIGIN (decl));
	  origin_node->abstract_and_needed = true;
	}

      /* We finalize local static variables during constructing callgraph
         edges.  Process their attributes too.  */
      process_function_and_variable_attributes (first_processed,
						first_analyzed_var);
      first_processed = cgraph_nodes;
      first_analyzed_var = varpool_nodes;
      varpool_analyze_pending_decls ();
      cgraph_process_new_functions ();
    }

  /* Collect entry points to the unit.  */
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Unit entry points:");
      for (node = cgraph_nodes; node != first_analyzed; node = node->next)
	if (node->needed)
	  fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
      fprintf (cgraph_dump_file, "\n\nInitial ");
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nReclaiming functions:");

  for (node = cgraph_nodes; node != first_analyzed; node = next)
    {
      tree decl = node->decl;
      next = node->next;

      if (node->local.finalized && !gimple_has_body_p (decl)
	  && (!node->alias || !node->thunk.alias)
	  && !node->thunk.thunk_p)
	cgraph_reset_node (node);

      if (!node->reachable
	  && (gimple_has_body_p (decl) || node->thunk.thunk_p
	      || (node->alias && node->thunk.alias)))
	{
	  if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, " %s", cgraph_node_name (node));
	  cgraph_remove_node (node);
	  continue;
	}
      else
	node->next_needed = NULL;
      gcc_assert (!node->local.finalized || node->thunk.thunk_p
		  || node->alias
		  || gimple_has_body_p (decl));
      gcc_assert (node->analyzed == node->local.finalized);
    }
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "\n\nReclaimed ");
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }
  bitmap_obstack_release (NULL);
  first_analyzed = cgraph_nodes;
  ggc_collect ();
}

/* Translate the ugly representation of aliases as alias pairs into nice
   representation in callgraph.  We don't handle all cases yet,
   unforutnately.  */

static void
handle_alias_pairs (void)
{
  alias_pair *p;
  unsigned i;
  struct cgraph_node *target_node;
  struct cgraph_node *src_node;
  struct varpool_node *target_vnode;
  
  for (i = 0; VEC_iterate (alias_pair, alias_pairs, i, p);)
    {
      if (TREE_CODE (p->decl) == FUNCTION_DECL
	  && (target_node = cgraph_node_for_asm (p->target)) != NULL)
	{
	  src_node = cgraph_get_node (p->decl);
	  if (src_node && src_node->local.finalized)
            cgraph_reset_node (src_node);
	  /* Normally EXTERNAL flag is used to mark external inlines,
	     however for aliases it seems to be allowed to use it w/o
	     any meaning. See gcc.dg/attr-alias-3.c  
	     However for weakref we insist on EXTERNAL flag being set.
	     See gcc.dg/attr-alias-5.c  */
	  if (DECL_EXTERNAL (p->decl))
	    DECL_EXTERNAL (p->decl) = lookup_attribute ("weakref",
							DECL_ATTRIBUTES (p->decl)) != NULL;
	  cgraph_create_function_alias (p->decl, target_node->decl);
	  VEC_unordered_remove (alias_pair, alias_pairs, i);
	}
      else if (TREE_CODE (p->decl) == VAR_DECL
	       && (target_vnode = varpool_node_for_asm (p->target)) != NULL)
	{
	  /* Normally EXTERNAL flag is used to mark external inlines,
	     however for aliases it seems to be allowed to use it w/o
	     any meaning. See gcc.dg/attr-alias-3.c  
	     However for weakref we insist on EXTERNAL flag being set.
	     See gcc.dg/attr-alias-5.c  */
	  if (DECL_EXTERNAL (p->decl))
	    DECL_EXTERNAL (p->decl) = lookup_attribute ("weakref",
							DECL_ATTRIBUTES (p->decl)) != NULL;
	  varpool_create_variable_alias (p->decl, target_vnode->decl);
	  VEC_unordered_remove (alias_pair, alias_pairs, i);
	}
      /* Weakrefs with target not defined in current unit are easy to handle; they
	 behave just as external variables except we need to note the alias flag
	 to later output the weakref pseudo op into asm file.  */
      else if (lookup_attribute ("weakref", DECL_ATTRIBUTES (p->decl)) != NULL
	       && (TREE_CODE (p->decl) == FUNCTION_DECL
		   ? (varpool_node_for_asm (p->target) == NULL)
		   : (cgraph_node_for_asm (p->target) == NULL)))
	{
	  if (TREE_CODE (p->decl) == FUNCTION_DECL)
	    cgraph_get_create_node (p->decl)->alias = true;
	  else
	    varpool_get_node (p->decl)->alias = true;
	  DECL_EXTERNAL (p->decl) = 1;
	  VEC_unordered_remove (alias_pair, alias_pairs, i);
	}
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "Unhandled alias %s->%s\n",
		     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (p->decl)),
		     IDENTIFIER_POINTER (p->target));

	  i++;
	}
    }
}


/* Analyze the whole compilation unit once it is parsed completely.  */

void
cgraph_finalize_compilation_unit (void)
{
  timevar_push (TV_CGRAPH);

  /* If LTO is enabled, initialize the streamer hooks needed by GIMPLE.  */
  if (flag_lto)
    lto_streamer_hooks_init ();

  /* If we're here there's no current function anymore.  Some frontends
     are lazy in clearing these.  */
  current_function_decl = NULL;
  set_cfun (NULL);

  /* Do not skip analyzing the functions if there were errors, we
     miss diagnostics for following functions otherwise.  */

  /* Emit size functions we didn't inline.  */
  finalize_size_functions ();

  /* Mark alias targets necessary and emit diagnostics.  */
  finish_aliases_1 ();
  handle_alias_pairs ();

  if (!quiet_flag)
    {
      fprintf (stderr, "\nAnalyzing compilation unit\n");
      fflush (stderr);
    }

  if (flag_dump_passes)
    dump_passes ();

  /* Gimplify and lower all functions, compute reachability and
     remove unreachable nodes.  */
  cgraph_analyze_functions ();

  /* Mark alias targets necessary and emit diagnostics.  */
  finish_aliases_1 ();
  handle_alias_pairs ();

  /* Gimplify and lower thunks.  */
  cgraph_analyze_functions ();

  /* Finally drive the pass manager.  */
  cgraph_optimize ();

  timevar_pop (TV_CGRAPH);
}


/* Figure out what functions we want to assemble.  */

static void
cgraph_mark_functions_to_output (void)
{
  struct cgraph_node *node;
#ifdef ENABLE_CHECKING
  bool check_same_comdat_groups = false;

  for (node = cgraph_nodes; node; node = node->next)
    gcc_assert (!node->process);
#endif

  for (node = cgraph_nodes; node; node = node->next)
    {
      tree decl = node->decl;
      struct cgraph_edge *e;

      gcc_assert (!node->process || node->same_comdat_group);
      if (node->process)
	continue;

      for (e = node->callers; e; e = e->next_caller)
	if (e->inline_failed)
	  break;

      /* We need to output all local functions that are used and not
	 always inlined, as well as those that are reachable from
	 outside the current compilation unit.  */
      if (node->analyzed
	  && !node->thunk.thunk_p
	  && !node->alias
	  && !node->global.inlined_to
	  && (!cgraph_only_called_directly_p (node)
	      || ((e || ipa_ref_has_aliases_p (&node->ref_list))
		  && node->reachable))
	  && !TREE_ASM_WRITTEN (decl)
	  && !DECL_EXTERNAL (decl))
	{
	  node->process = 1;
	  if (node->same_comdat_group)
	    {
	      struct cgraph_node *next;
	      for (next = node->same_comdat_group;
		   next != node;
		   next = next->same_comdat_group)
		if (!next->thunk.thunk_p && !next->alias)
		  next->process = 1;
	    }
	}
      else if (node->same_comdat_group)
	{
#ifdef ENABLE_CHECKING
	  check_same_comdat_groups = true;
#endif
	}
      else
	{
	  /* We should've reclaimed all functions that are not needed.  */
#ifdef ENABLE_CHECKING
	  if (!node->global.inlined_to
	      && gimple_has_body_p (decl)
	      /* FIXME: in ltrans unit when offline copy is outside partition but inline copies
		 are inside partition, we can end up not removing the body since we no longer
		 have analyzed node pointing to it.  */
	      && !node->in_other_partition
	      && !node->alias
	      && !DECL_EXTERNAL (decl))
	    {
	      dump_cgraph_node (stderr, node);
	      internal_error ("failed to reclaim unneeded function");
	    }
#endif
	  gcc_assert (node->global.inlined_to
		      || !gimple_has_body_p (decl)
		      || node->in_other_partition
		      || DECL_EXTERNAL (decl));

	}

    }
#ifdef ENABLE_CHECKING
  if (check_same_comdat_groups)
    for (node = cgraph_nodes; node; node = node->next)
      if (node->same_comdat_group && !node->process)
	{
	  tree decl = node->decl;
	  if (!node->global.inlined_to
	      && gimple_has_body_p (decl)
	      /* FIXME: in ltrans unit when offline copy is outside partition but inline copies
		 are inside partition, we can end up not removing the body since we no longer
		 have analyzed node pointing to it.  */
	      && !node->in_other_partition
	      && !DECL_EXTERNAL (decl))
	    {
	      dump_cgraph_node (stderr, node);
	      internal_error ("failed to reclaim unneeded functionin same comdat group");
	    }
	}
#endif
}

/* DECL is FUNCTION_DECL.  Initialize datastructures so DECL is a function
   in lowered gimple form.
   
   Set current_function_decl and cfun to newly constructed empty function body.
   return basic block in the function body.  */

static basic_block
init_lowered_empty_function (tree decl)
{
  basic_block bb;

  current_function_decl = decl;
  allocate_struct_function (decl, false);
  gimple_register_cfg_hooks ();
  init_empty_tree_cfg ();
  init_tree_ssa (cfun);
  init_ssa_operands ();
  cfun->gimple_df->in_ssa_p = true;
  DECL_INITIAL (decl) = make_node (BLOCK);

  DECL_SAVED_TREE (decl) = error_mark_node;
  cfun->curr_properties |=
    (PROP_gimple_lcf | PROP_gimple_leh | PROP_cfg | PROP_referenced_vars |
     PROP_ssa | PROP_gimple_any);

  /* Create BB for body of the function and connect it properly.  */
  bb = create_basic_block (NULL, (void *) 0, ENTRY_BLOCK_PTR);
  make_edge (ENTRY_BLOCK_PTR, bb, EDGE_FALLTHRU);
  make_edge (bb, EXIT_BLOCK_PTR, EDGE_FALLTHRU);

  return bb;
}

/* Adjust PTR by the constant FIXED_OFFSET, and by the vtable
   offset indicated by VIRTUAL_OFFSET, if that is
   non-null. THIS_ADJUSTING is nonzero for a this adjusting thunk and
   zero for a result adjusting thunk.  */

static tree
thunk_adjust (gimple_stmt_iterator * bsi,
	      tree ptr, bool this_adjusting,
	      HOST_WIDE_INT fixed_offset, tree virtual_offset)
{
  gimple stmt;
  tree ret;

  if (this_adjusting
      && fixed_offset != 0)
    {
      stmt = gimple_build_assign
		(ptr, fold_build_pointer_plus_hwi_loc (input_location,
						       ptr,
						       fixed_offset));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
    }

  /* If there's a virtual offset, look up that value in the vtable and
     adjust the pointer again.  */
  if (virtual_offset)
    {
      tree vtabletmp;
      tree vtabletmp2;
      tree vtabletmp3;

      if (!vtable_entry_type)
	{
	  tree vfunc_type = make_node (FUNCTION_TYPE);
	  TREE_TYPE (vfunc_type) = integer_type_node;
	  TYPE_ARG_TYPES (vfunc_type) = NULL_TREE;
	  layout_type (vfunc_type);

	  vtable_entry_type = build_pointer_type (vfunc_type);
	}

      vtabletmp =
	create_tmp_var (build_pointer_type
			(build_pointer_type (vtable_entry_type)), "vptr");

      /* The vptr is always at offset zero in the object.  */
      stmt = gimple_build_assign (vtabletmp,
				  build1 (NOP_EXPR, TREE_TYPE (vtabletmp),
					  ptr));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
      mark_symbols_for_renaming (stmt);
      find_referenced_vars_in (stmt);

      /* Form the vtable address.  */
      vtabletmp2 = create_tmp_var (TREE_TYPE (TREE_TYPE (vtabletmp)),
				   "vtableaddr");
      stmt = gimple_build_assign (vtabletmp2,
				  build_simple_mem_ref (vtabletmp));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
      mark_symbols_for_renaming (stmt);
      find_referenced_vars_in (stmt);

      /* Find the entry with the vcall offset.  */
      stmt = gimple_build_assign (vtabletmp2,
				  fold_build_pointer_plus_loc (input_location,
							       vtabletmp2,
							       virtual_offset));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Get the offset itself.  */
      vtabletmp3 = create_tmp_var (TREE_TYPE (TREE_TYPE (vtabletmp2)),
				   "vcalloffset");
      stmt = gimple_build_assign (vtabletmp3,
				  build_simple_mem_ref (vtabletmp2));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
      mark_symbols_for_renaming (stmt);
      find_referenced_vars_in (stmt);

      /* Adjust the `this' pointer.  */
      ptr = fold_build_pointer_plus_loc (input_location, ptr, vtabletmp3);
      ptr = force_gimple_operand_gsi (bsi, ptr, true, NULL_TREE, false,
				      GSI_CONTINUE_LINKING);
    }

  if (!this_adjusting
      && fixed_offset != 0)
    /* Adjust the pointer by the constant.  */
    {
      tree ptrtmp;

      if (TREE_CODE (ptr) == VAR_DECL)
        ptrtmp = ptr;
      else
        {
          ptrtmp = create_tmp_var (TREE_TYPE (ptr), "ptr");
          stmt = gimple_build_assign (ptrtmp, ptr);
	  gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
	  mark_symbols_for_renaming (stmt);
	  find_referenced_vars_in (stmt);
	}
      ptr = fold_build_pointer_plus_hwi_loc (input_location,
					     ptrtmp, fixed_offset);
    }

  /* Emit the statement and gimplify the adjustment expression.  */
  ret = create_tmp_var (TREE_TYPE (ptr), "adjusted_this");
  stmt = gimple_build_assign (ret, ptr);
  mark_symbols_for_renaming (stmt);
  find_referenced_vars_in (stmt);
  gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

  return ret;
}

/* Produce assembler for thunk NODE.  */

static void
assemble_thunk (struct cgraph_node *node)
{
  bool this_adjusting = node->thunk.this_adjusting;
  HOST_WIDE_INT fixed_offset = node->thunk.fixed_offset;
  HOST_WIDE_INT virtual_value = node->thunk.virtual_value;
  tree virtual_offset = NULL;
  tree alias = node->thunk.alias;
  tree thunk_fndecl = node->decl;
  tree a = DECL_ARGUMENTS (thunk_fndecl);

  current_function_decl = thunk_fndecl;

  /* Ensure thunks are emitted in their correct sections.  */
  resolve_unique_section (thunk_fndecl, 0, flag_function_sections);

  if (this_adjusting
      && targetm.asm_out.can_output_mi_thunk (thunk_fndecl, fixed_offset,
					      virtual_value, alias))
    {
      const char *fnname;
      tree fn_block;
      tree restype = TREE_TYPE (TREE_TYPE (thunk_fndecl));
      
      DECL_RESULT (thunk_fndecl)
	= build_decl (DECL_SOURCE_LOCATION (thunk_fndecl),
		      RESULT_DECL, 0, restype);
      fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk_fndecl));

      /* The back end expects DECL_INITIAL to contain a BLOCK, so we
	 create one.  */
      fn_block = make_node (BLOCK);
      BLOCK_VARS (fn_block) = a;
      DECL_INITIAL (thunk_fndecl) = fn_block;
      init_function_start (thunk_fndecl);
      cfun->is_thunk = 1;
      assemble_start_function (thunk_fndecl, fnname);

      targetm.asm_out.output_mi_thunk (asm_out_file, thunk_fndecl,
				       fixed_offset, virtual_value, alias);

      assemble_end_function (thunk_fndecl, fnname);
      init_insn_lengths ();
      free_after_compilation (cfun);
      set_cfun (NULL);
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
      node->thunk.thunk_p = false;
      node->analyzed = false;
    }
  else
    {
      tree restype;
      basic_block bb, then_bb, else_bb, return_bb;
      gimple_stmt_iterator bsi;
      int nargs = 0;
      tree arg;
      int i;
      tree resdecl;
      tree restmp = NULL;
      VEC(tree, heap) *vargs;

      gimple call;
      gimple ret;

      DECL_IGNORED_P (thunk_fndecl) = 1;
      bitmap_obstack_initialize (NULL);

      if (node->thunk.virtual_offset_p)
        virtual_offset = size_int (virtual_value);

      /* Build the return declaration for the function.  */
      restype = TREE_TYPE (TREE_TYPE (thunk_fndecl));
      if (DECL_RESULT (thunk_fndecl) == NULL_TREE)
	{
	  resdecl = build_decl (input_location, RESULT_DECL, 0, restype);
	  DECL_ARTIFICIAL (resdecl) = 1;
	  DECL_IGNORED_P (resdecl) = 1;
	  DECL_RESULT (thunk_fndecl) = resdecl;
	}
      else
	resdecl = DECL_RESULT (thunk_fndecl);

      bb = then_bb = else_bb = return_bb = init_lowered_empty_function (thunk_fndecl);

      bsi = gsi_start_bb (bb);

      /* Build call to the function being thunked.  */
      if (!VOID_TYPE_P (restype))
	{
	  if (!is_gimple_reg_type (restype))
	    {
	      restmp = resdecl;
	      add_local_decl (cfun, restmp);
	      BLOCK_VARS (DECL_INITIAL (current_function_decl)) = restmp;
	    }
	  else
            restmp = create_tmp_var_raw (restype, "retval");
	}

      for (arg = a; arg; arg = DECL_CHAIN (arg))
        nargs++;
      vargs = VEC_alloc (tree, heap, nargs);
      if (this_adjusting)
        VEC_quick_push (tree, vargs,
			thunk_adjust (&bsi,
				      a, 1, fixed_offset,
				      virtual_offset));
      else
        VEC_quick_push (tree, vargs, a);
      for (i = 1, arg = DECL_CHAIN (a); i < nargs; i++, arg = DECL_CHAIN (arg))
        VEC_quick_push (tree, vargs, arg);
      call = gimple_build_call_vec (build_fold_addr_expr_loc (0, alias), vargs);
      VEC_free (tree, heap, vargs);
      gimple_call_set_from_thunk (call, true);
      if (restmp)
        gimple_call_set_lhs (call, restmp);
      gsi_insert_after (&bsi, call, GSI_NEW_STMT);
      mark_symbols_for_renaming (call);
      find_referenced_vars_in (call);
      update_stmt (call);

      if (restmp && !this_adjusting)
        {
	  tree true_label = NULL_TREE;

	  if (TREE_CODE (TREE_TYPE (restmp)) == POINTER_TYPE)
	    {
	      gimple stmt;
	      /* If the return type is a pointer, we need to
		 protect against NULL.  We know there will be an
		 adjustment, because that's why we're emitting a
		 thunk.  */
	      then_bb = create_basic_block (NULL, (void *) 0, bb);
	      return_bb = create_basic_block (NULL, (void *) 0, then_bb);
	      else_bb = create_basic_block (NULL, (void *) 0, else_bb);
	      remove_edge (single_succ_edge (bb));
	      true_label = gimple_block_label (then_bb);
	      stmt = gimple_build_cond (NE_EXPR, restmp,
	      				build_zero_cst (TREE_TYPE (restmp)),
	      			        NULL_TREE, NULL_TREE);
	      gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
	      make_edge (bb, then_bb, EDGE_TRUE_VALUE);
	      make_edge (bb, else_bb, EDGE_FALSE_VALUE);
	      make_edge (return_bb, EXIT_BLOCK_PTR, 0);
	      make_edge (then_bb, return_bb, EDGE_FALLTHRU);
	      make_edge (else_bb, return_bb, EDGE_FALLTHRU);
	      bsi = gsi_last_bb (then_bb);
	    }

	  restmp = thunk_adjust (&bsi, restmp, /*this_adjusting=*/0,
			         fixed_offset, virtual_offset);
	  if (true_label)
	    {
	      gimple stmt;
	      bsi = gsi_last_bb (else_bb);
	      stmt = gimple_build_assign (restmp,
					  build_zero_cst (TREE_TYPE (restmp)));
	      gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
	      bsi = gsi_last_bb (return_bb);
	    }
	}
      else
        gimple_call_set_tail (call, true);

      /* Build return value.  */
      ret = gimple_build_return (restmp);
      gsi_insert_after (&bsi, ret, GSI_NEW_STMT);

      delete_unreachable_blocks ();
      update_ssa (TODO_update_ssa);

      /* Since we want to emit the thunk, we explicitly mark its name as
	 referenced.  */
      node->thunk.thunk_p = false;
      cgraph_node_remove_callees (node);
      cgraph_add_new_function (thunk_fndecl, true);
      bitmap_obstack_release (NULL);
    }
  current_function_decl = NULL;
}



/* Assemble thunks and aliases asociated to NODE.  */

static void
assemble_thunks_and_aliases (struct cgraph_node *node)
{
  struct cgraph_edge *e;
  int i;
  struct ipa_ref *ref;

  for (e = node->callers; e;)
    if (e->caller->thunk.thunk_p)
      {
	struct cgraph_node *thunk = e->caller;

	e = e->next_caller;
	assemble_thunks_and_aliases (thunk);
        assemble_thunk (thunk);
      }
    else
      e = e->next_caller;
  for (i = 0; ipa_ref_list_refering_iterate (&node->ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      {
	struct cgraph_node *alias = ipa_ref_refering_node (ref);
        bool saved_written = TREE_ASM_WRITTEN (alias->thunk.alias);

	/* Force assemble_alias to really output the alias this time instead
	   of buffering it in same alias pairs.  */
	TREE_ASM_WRITTEN (alias->thunk.alias) = 1;
	assemble_alias (alias->decl,
			DECL_ASSEMBLER_NAME (alias->thunk.alias));
	assemble_thunks_and_aliases (alias);
	TREE_ASM_WRITTEN (alias->thunk.alias) = saved_written;
      }
}

/* Expand function specified by NODE.  */

static void
cgraph_expand_function (struct cgraph_node *node)
{
  tree decl = node->decl;

  /* We ought to not compile any inline clones.  */
  gcc_assert (!node->global.inlined_to);

  announce_function (decl);
  node->process = 0;
  gcc_assert (node->lowered);

  /* Generate RTL for the body of DECL.  */
  tree_rest_of_compilation (decl);

  /* Make sure that BE didn't give up on compiling.  */
  gcc_assert (TREE_ASM_WRITTEN (decl));
  current_function_decl = NULL;
  gcc_assert (!cgraph_preserve_function_body_p (node));

  /* It would make a lot more sense to output thunks before function body to get more
     forward and lest backwarding jumps.  This is however would need solving problem
     with comdats. See PR48668.  Also aliases must come after function itself to
     make one pass assemblers, like one on AIX happy.  See PR 50689.
     FIXME: Perhaps thunks should be move before function IFF they are not in comdat
     groups.  */
  assemble_thunks_and_aliases (node);
  cgraph_release_function_body (node);
  /* Eliminate all call edges.  This is important so the GIMPLE_CALL no longer
     points to the dead function body.  */
  cgraph_node_remove_callees (node);

  cgraph_function_flags_ready = true;
}

/* Return true when CALLER_DECL should be inlined into CALLEE_DECL.  */

bool
cgraph_inline_p (struct cgraph_edge *e, cgraph_inline_failed_t *reason)
{
  *reason = e->inline_failed;
  return !e->inline_failed;
}



/* Expand all functions that must be output.

   Attempt to topologically sort the nodes so function is output when
   all called functions are already assembled to allow data to be
   propagated across the callgraph.  Use a stack to get smaller distance
   between a function and its callees (later we may choose to use a more
   sophisticated algorithm for function reordering; we will likely want
   to use subsections to make the output functions appear in top-down
   order).  */

static void
cgraph_expand_all_functions (void)
{
  struct cgraph_node *node;
  struct cgraph_node **order = XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  int order_pos, new_order_pos = 0;
  int i;

  order_pos = ipa_reverse_postorder (order);
  gcc_assert (order_pos == cgraph_n_nodes);

  /* Garbage collector may remove inline clones we eliminate during
     optimization.  So we must be sure to not reference them.  */
  for (i = 0; i < order_pos; i++)
    if (order[i]->process)
      order[new_order_pos++] = order[i];

  for (i = new_order_pos - 1; i >= 0; i--)
    {
      node = order[i];
      if (node->process)
	{
	  gcc_assert (node->reachable);
	  node->process = 0;
	  cgraph_expand_function (node);
	}
    }
  cgraph_process_new_functions ();

  free (order);

}

/* This is used to sort the node types by the cgraph order number.  */

enum cgraph_order_sort_kind
{
  ORDER_UNDEFINED = 0,
  ORDER_FUNCTION,
  ORDER_VAR,
  ORDER_ASM
};

struct cgraph_order_sort
{
  enum cgraph_order_sort_kind kind;
  union
  {
    struct cgraph_node *f;
    struct varpool_node *v;
    struct cgraph_asm_node *a;
  } u;
};

/* Output all functions, variables, and asm statements in the order
   according to their order fields, which is the order in which they
   appeared in the file.  This implements -fno-toplevel-reorder.  In
   this mode we may output functions and variables which don't really
   need to be output.  */

static void
cgraph_output_in_order (void)
{
  int max;
  struct cgraph_order_sort *nodes;
  int i;
  struct cgraph_node *pf;
  struct varpool_node *pv;
  struct cgraph_asm_node *pa;

  max = cgraph_order;
  nodes = XCNEWVEC (struct cgraph_order_sort, max);

  varpool_analyze_pending_decls ();

  for (pf = cgraph_nodes; pf; pf = pf->next)
    {
      if (pf->process && !pf->thunk.thunk_p && !pf->alias)
	{
	  i = pf->order;
	  gcc_assert (nodes[i].kind == ORDER_UNDEFINED);
	  nodes[i].kind = ORDER_FUNCTION;
	  nodes[i].u.f = pf;
	}
    }

  for (pv = varpool_nodes_queue; pv; pv = pv->next_needed)
    {
      i = pv->order;
      gcc_assert (nodes[i].kind == ORDER_UNDEFINED);
      nodes[i].kind = ORDER_VAR;
      nodes[i].u.v = pv;
    }

  for (pa = cgraph_asm_nodes; pa; pa = pa->next)
    {
      i = pa->order;
      gcc_assert (nodes[i].kind == ORDER_UNDEFINED);
      nodes[i].kind = ORDER_ASM;
      nodes[i].u.a = pa;
    }

  /* In toplevel reorder mode we output all statics; mark them as needed.  */
  for (i = 0; i < max; ++i)
    {
      if (nodes[i].kind == ORDER_VAR)
        {
	  varpool_mark_needed_node (nodes[i].u.v);
	}
    }
  varpool_empty_needed_queue ();

  for (i = 0; i < max; ++i)
    if (nodes[i].kind == ORDER_VAR)
      varpool_finalize_named_section_flags (nodes[i].u.v);

  for (i = 0; i < max; ++i)
    {
      switch (nodes[i].kind)
	{
	case ORDER_FUNCTION:
	  nodes[i].u.f->process = 0;
	  cgraph_expand_function (nodes[i].u.f);
	  break;

	case ORDER_VAR:
	  varpool_assemble_decl (nodes[i].u.v);
	  break;

	case ORDER_ASM:
	  assemble_asm (nodes[i].u.a->asm_str);
	  break;

	case ORDER_UNDEFINED:
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  cgraph_asm_nodes = NULL;
  free (nodes);
}

/* Return true when function body of DECL still needs to be kept around
   for later re-use.  */
bool
cgraph_preserve_function_body_p (struct cgraph_node *node)
{
  gcc_assert (cgraph_global_info_ready);
  gcc_assert (!node->alias && !node->thunk.thunk_p);

  /* Look if there is any clone around.  */
  if (node->clones)
    return true;
  return false;
}

static void
ipa_passes (void)
{
  set_cfun (NULL);
  current_function_decl = NULL;
  gimple_register_cfg_hooks ();
  bitmap_obstack_initialize (NULL);

  invoke_plugin_callbacks (PLUGIN_ALL_IPA_PASSES_START, NULL);

  if (!in_lto_p)
    {
      execute_ipa_pass_list (all_small_ipa_passes);
      if (seen_error ())
	return;
    }

  /* We never run removal of unreachable nodes after early passes.  This is
     because TODO is run before the subpasses.  It is important to remove
     the unreachable functions to save works at IPA level and to get LTO
     symbol tables right.  */
  cgraph_remove_unreachable_nodes (true, cgraph_dump_file);

  /* If pass_all_early_optimizations was not scheduled, the state of
     the cgraph will not be properly updated.  Update it now.  */
  if (cgraph_state < CGRAPH_STATE_IPA_SSA)
    cgraph_state = CGRAPH_STATE_IPA_SSA;

  if (!in_lto_p)
    {
      /* Generate coverage variables and constructors.  */
      coverage_finish ();

      /* Process new functions added.  */
      set_cfun (NULL);
      current_function_decl = NULL;
      cgraph_process_new_functions ();

      execute_ipa_summary_passes
	((struct ipa_opt_pass_d *) all_regular_ipa_passes);
    }

  /* Some targets need to handle LTO assembler output specially.  */
  if (flag_generate_lto)
    targetm.asm_out.lto_start ();

  execute_ipa_summary_passes ((struct ipa_opt_pass_d *) all_lto_gen_passes);

  if (!in_lto_p)
    ipa_write_summaries ();

  if (flag_generate_lto)
    targetm.asm_out.lto_end ();

  if (!flag_ltrans && (in_lto_p || !flag_lto || flag_fat_lto_objects))
    execute_ipa_pass_list (all_regular_ipa_passes);
  invoke_plugin_callbacks (PLUGIN_ALL_IPA_PASSES_END, NULL);

  bitmap_obstack_release (NULL);
}


/* Return string alias is alias of.  */

static tree
get_alias_symbol (tree decl)
{
  tree alias = lookup_attribute ("alias", DECL_ATTRIBUTES (decl));
  return get_identifier (TREE_STRING_POINTER
			  (TREE_VALUE (TREE_VALUE (alias))));
}


/* Weakrefs may be associated to external decls and thus not output
   at expansion time.  Emit all neccesary aliases.  */

static void
output_weakrefs (void)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;
  for (node = cgraph_nodes; node; node = node->next)
    if (node->alias && DECL_EXTERNAL (node->decl)
        && !TREE_ASM_WRITTEN (node->decl)
	&& lookup_attribute ("weakref", DECL_ATTRIBUTES (node->decl)))
      assemble_alias (node->decl,
		      node->thunk.alias ? DECL_ASSEMBLER_NAME (node->thunk.alias)
		      : get_alias_symbol (node->decl));
  for (vnode = varpool_nodes; vnode; vnode = vnode->next)
    if (vnode->alias && DECL_EXTERNAL (vnode->decl)
        && !TREE_ASM_WRITTEN (vnode->decl)
	&& lookup_attribute ("weakref", DECL_ATTRIBUTES (vnode->decl)))
      assemble_alias (vnode->decl,
		      vnode->alias_of ? DECL_ASSEMBLER_NAME (vnode->alias_of)
		      : get_alias_symbol (vnode->decl));
}


/* Perform simple optimizations based on callgraph.  */

void
cgraph_optimize (void)
{
  if (seen_error ())
    return;

#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif

  /* Frontend may output common variables after the unit has been finalized.
     It is safe to deal with them here as they are always zero initialized.  */
  varpool_analyze_pending_decls ();

  timevar_push (TV_CGRAPHOPT);
  if (pre_ipa_mem_report)
    {
      fprintf (stderr, "Memory consumption before IPA\n");
      dump_memory_report (false);
    }
  if (!quiet_flag)
    fprintf (stderr, "Performing interprocedural optimizations\n");
  cgraph_state = CGRAPH_STATE_IPA;

  /* Don't run the IPA passes if there was any error or sorry messages.  */
  if (!seen_error ())
    ipa_passes ();

  /* Do nothing else if any IPA pass found errors or if we are just streaming LTO.  */
  if (seen_error ()
      || (!in_lto_p && flag_lto && !flag_fat_lto_objects))
    {
      timevar_pop (TV_CGRAPHOPT);
      return;
    }

  /* This pass remove bodies of extern inline functions we never inlined.
     Do this later so other IPA passes see what is really going on.  */
  cgraph_remove_unreachable_nodes (false, dump_file);
  cgraph_global_info_ready = true;
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Optimized ");
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }
  if (post_ipa_mem_report)
    {
      fprintf (stderr, "Memory consumption after IPA\n");
      dump_memory_report (false);
    }
  timevar_pop (TV_CGRAPHOPT);

  /* Output everything.  */
  (*debug_hooks->assembly_start) ();
  if (!quiet_flag)
    fprintf (stderr, "Assembling functions:\n");
#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif

  cgraph_materialize_all_clones ();
  bitmap_obstack_initialize (NULL);
  execute_ipa_pass_list (all_late_ipa_passes);
  cgraph_remove_unreachable_nodes (true, dump_file);
#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif
  bitmap_obstack_release (NULL);
  cgraph_mark_functions_to_output ();

  cgraph_state = CGRAPH_STATE_EXPANSION;
  if (!flag_toplevel_reorder)
    cgraph_output_in_order ();
  else
    {
      cgraph_output_pending_asms ();

      cgraph_expand_all_functions ();
      varpool_remove_unreferenced_decls ();

      varpool_assemble_pending_decls ();
    }

  output_weakrefs ();
  cgraph_process_new_functions ();
  cgraph_state = CGRAPH_STATE_FINISHED;

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "\nFinal ");
      dump_cgraph (cgraph_dump_file);
      dump_varpool (cgraph_dump_file);
    }
#ifdef ENABLE_CHECKING
  verify_cgraph ();
  /* Double check that all inline clones are gone and that all
     function bodies have been released from memory.  */
  if (!seen_error ())
    {
      struct cgraph_node *node;
      bool error_found = false;

      for (node = cgraph_nodes; node; node = node->next)
	if (node->analyzed
	    && (node->global.inlined_to
		|| gimple_has_body_p (node->decl)))
	  {
	    error_found = true;
	    dump_cgraph_node (stderr, node);
	  }
      if (error_found)
	internal_error ("nodes with unreleased memory found");
    }
#endif
}

void
init_cgraph (void)
{
  if (!cgraph_dump_file)
    cgraph_dump_file = dump_begin (TDI_cgraph, NULL);
}

/* The edges representing the callers of the NEW_VERSION node were
   fixed by cgraph_function_versioning (), now the call_expr in their
   respective tree code should be updated to call the NEW_VERSION.  */

static void
update_call_expr (struct cgraph_node *new_version)
{
  struct cgraph_edge *e;

  gcc_assert (new_version);

  /* Update the call expr on the edges to call the new version.  */
  for (e = new_version->callers; e; e = e->next_caller)
    {
      struct function *inner_function = DECL_STRUCT_FUNCTION (e->caller->decl);
      gimple_call_set_fndecl (e->call_stmt, new_version->decl);
      maybe_clean_eh_stmt_fn (inner_function, e->call_stmt);
    }
}


/* Create a new cgraph node which is the new version of
   OLD_VERSION node.  REDIRECT_CALLERS holds the callers
   edges which should be redirected to point to
   NEW_VERSION.  ALL the callees edges of OLD_VERSION
   are cloned to the new version node.  Return the new
   version node. 

   If non-NULL BLOCK_TO_COPY determine what basic blocks 
   was copied to prevent duplications of calls that are dead
   in the clone.  */

struct cgraph_node *
cgraph_copy_node_for_versioning (struct cgraph_node *old_version,
				 tree new_decl,
				 VEC(cgraph_edge_p,heap) *redirect_callers,
				 bitmap bbs_to_copy)
 {
   struct cgraph_node *new_version;
   struct cgraph_edge *e;
   unsigned i;

   gcc_assert (old_version);

   new_version = cgraph_create_node (new_decl);

   new_version->analyzed = old_version->analyzed;
   new_version->local = old_version->local;
   new_version->local.externally_visible = false;
   new_version->local.local = true;
   new_version->global = old_version->global;
   new_version->rtl = old_version->rtl;
   new_version->reachable = true;
   new_version->count = old_version->count;

   for (e = old_version->callees; e; e=e->next_callee)
     if (!bbs_to_copy
	 || bitmap_bit_p (bbs_to_copy, gimple_bb (e->call_stmt)->index))
       cgraph_clone_edge (e, new_version, e->call_stmt,
			  e->lto_stmt_uid, REG_BR_PROB_BASE,
			  CGRAPH_FREQ_BASE,
			  true);
   for (e = old_version->indirect_calls; e; e=e->next_callee)
     if (!bbs_to_copy
	 || bitmap_bit_p (bbs_to_copy, gimple_bb (e->call_stmt)->index))
       cgraph_clone_edge (e, new_version, e->call_stmt,
			  e->lto_stmt_uid, REG_BR_PROB_BASE,
			  CGRAPH_FREQ_BASE,
			  true);
   FOR_EACH_VEC_ELT (cgraph_edge_p, redirect_callers, i, e)
     {
       /* Redirect calls to the old version node to point to its new
	  version.  */
       cgraph_redirect_edge_callee (e, new_version);
     }

   cgraph_call_node_duplication_hooks (old_version, new_version);

   return new_version;
 }

 /* Perform function versioning.
    Function versioning includes copying of the tree and
    a callgraph update (creating a new cgraph node and updating
    its callees and callers).

    REDIRECT_CALLERS varray includes the edges to be redirected
    to the new version.

    TREE_MAP is a mapping of tree nodes we want to replace with
    new ones (according to results of prior analysis).
    OLD_VERSION_NODE is the node that is versioned.
    It returns the new version's cgraph node.
    If non-NULL ARGS_TO_SKIP determine function parameters to remove
    from new version.
    If non-NULL BLOCK_TO_COPY determine what basic blocks to copy.
    If non_NULL NEW_ENTRY determine new entry BB of the clone.  */

struct cgraph_node *
cgraph_function_versioning (struct cgraph_node *old_version_node,
			    VEC(cgraph_edge_p,heap) *redirect_callers,
			    VEC (ipa_replace_map_p,gc)* tree_map,
			    bitmap args_to_skip,
			    bitmap bbs_to_copy,
			    basic_block new_entry_block,
			    const char *clone_name)
{
  tree old_decl = old_version_node->decl;
  struct cgraph_node *new_version_node = NULL;
  tree new_decl;

  if (!tree_versionable_function_p (old_decl))
    return NULL;

  gcc_assert (old_version_node->local.can_change_signature || !args_to_skip);

  /* Make a new FUNCTION_DECL tree node for the
     new version. */
  if (!args_to_skip)
    new_decl = copy_node (old_decl);
  else
    new_decl = build_function_decl_skip_args (old_decl, args_to_skip);

  /* Generate a new name for the new version. */
  DECL_NAME (new_decl) = clone_function_name (old_decl, clone_name);
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));
  SET_DECL_RTL (new_decl, NULL);

  /* When the old decl was a con-/destructor make sure the clone isn't.  */
  DECL_STATIC_CONSTRUCTOR(new_decl) = 0;
  DECL_STATIC_DESTRUCTOR(new_decl) = 0;

  /* Create the new version's call-graph node.
     and update the edges of the new node. */
  new_version_node =
    cgraph_copy_node_for_versioning (old_version_node, new_decl,
				     redirect_callers, bbs_to_copy);

  /* Copy the OLD_VERSION_NODE function tree to the new version.  */
  tree_function_versioning (old_decl, new_decl, tree_map, false, args_to_skip,
			    bbs_to_copy, new_entry_block);

  /* Update the new version's properties.
     Make The new version visible only within this translation unit.  Make sure
     that is not weak also.
     ??? We cannot use COMDAT linkage because there is no
     ABI support for this.  */
  cgraph_make_decl_local (new_version_node->decl);
  DECL_VIRTUAL_P (new_version_node->decl) = 0;
  new_version_node->local.externally_visible = 0;
  new_version_node->local.local = 1;
  new_version_node->lowered = true;

  /* Update the call_expr on the edges to call the new version node. */
  update_call_expr (new_version_node);

  cgraph_call_function_insertion_hooks (new_version_node);
  return new_version_node;
}

/* Given virtual clone, turn it into actual clone.  */
static void
cgraph_materialize_clone (struct cgraph_node *node)
{
  bitmap_obstack_initialize (NULL);
  node->former_clone_of = node->clone_of->decl;
  if (node->clone_of->former_clone_of)
    node->former_clone_of = node->clone_of->former_clone_of;
  /* Copy the OLD_VERSION_NODE function tree to the new version.  */
  tree_function_versioning (node->clone_of->decl, node->decl,
  			    node->clone.tree_map, true,
			    node->clone.args_to_skip, NULL, NULL);
  if (cgraph_dump_file)
    {
      dump_function_to_file (node->clone_of->decl, cgraph_dump_file, dump_flags);
      dump_function_to_file (node->decl, cgraph_dump_file, dump_flags);
    }

  /* Function is no longer clone.  */
  if (node->next_sibling_clone)
    node->next_sibling_clone->prev_sibling_clone = node->prev_sibling_clone;
  if (node->prev_sibling_clone)
    node->prev_sibling_clone->next_sibling_clone = node->next_sibling_clone;
  else
    node->clone_of->clones = node->next_sibling_clone;
  node->next_sibling_clone = NULL;
  node->prev_sibling_clone = NULL;
  if (!node->clone_of->analyzed && !node->clone_of->clones)
    {
      cgraph_release_function_body (node->clone_of);
      cgraph_node_remove_callees (node->clone_of);
      ipa_remove_all_references (&node->clone_of->ref_list);
    }
  node->clone_of = NULL;
  bitmap_obstack_release (NULL);
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

  if (e->indirect_unknown_callee
      || decl == e->callee->decl)
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
	       cgraph_node_name (e->caller), e->caller->uid,
	       cgraph_node_name (e->callee), e->callee->uid);
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
      gimple_call_set_fndecl (new_stmt, e->callee->decl);

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
      gimple_call_set_fndecl (new_stmt, e->callee->decl);
      update_stmt (new_stmt);
    }

  cgraph_set_call_stmt_including_clones (e->caller, e->call_stmt, new_stmt);

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "  updated to:");
      print_gimple_stmt (cgraph_dump_file, e->call_stmt, 0, dump_flags);
    }
  return new_stmt;
}

/* Once all functions from compilation unit are in memory, produce all clones
   and update all calls.  We might also do this on demand if we don't want to
   bring all functions to memory prior compilation, but current WHOPR
   implementation does that and it is is bit easier to keep everything right in
   this order.  */
void
cgraph_materialize_all_clones (void)
{
  struct cgraph_node *node;
  bool stabilized = false;

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Materializing clones\n");
#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif

  /* We can also do topological order, but number of iterations should be
     bounded by number of IPA passes since single IPA pass is probably not
     going to create clones of clones it created itself.  */
  while (!stabilized)
    {
      stabilized = true;
      for (node = cgraph_nodes; node; node = node->next)
        {
	  if (node->clone_of && node->decl != node->clone_of->decl
	      && !gimple_has_body_p (node->decl))
	    {
	      if (gimple_has_body_p (node->clone_of->decl))
	        {
		  if (cgraph_dump_file)
		    {
		      fprintf (cgraph_dump_file, "cloning %s to %s\n",
			       cgraph_node_name (node->clone_of),
			       cgraph_node_name (node));
		      if (node->clone.tree_map)
		        {
			  unsigned int i;
		          fprintf (cgraph_dump_file, "   replace map: ");
			  for (i = 0; i < VEC_length (ipa_replace_map_p,
			  			      node->clone.tree_map);
						      i++)
			    {
			      struct ipa_replace_map *replace_info;
			      replace_info = VEC_index (ipa_replace_map_p,
			      				node->clone.tree_map,
							i);
			      print_generic_expr (cgraph_dump_file, replace_info->old_tree, 0);
			      fprintf (cgraph_dump_file, " -> ");
			      print_generic_expr (cgraph_dump_file, replace_info->new_tree, 0);
			      fprintf (cgraph_dump_file, "%s%s;",
			      	       replace_info->replace_p ? "(replace)":"",
				       replace_info->ref_p ? "(ref)":"");
			    }
			  fprintf (cgraph_dump_file, "\n");
			}
		      if (node->clone.args_to_skip)
			{
		          fprintf (cgraph_dump_file, "   args_to_skip: ");
		          dump_bitmap (cgraph_dump_file, node->clone.args_to_skip);
			}
		      if (node->clone.args_to_skip)
			{
		          fprintf (cgraph_dump_file, "   combined_args_to_skip:");
		          dump_bitmap (cgraph_dump_file, node->clone.combined_args_to_skip);
			}
		    }
		  cgraph_materialize_clone (node);
		  stabilized = false;
	        }
	    }
	}
    }
  for (node = cgraph_nodes; node; node = node->next)
    if (!node->analyzed && node->callees)
      cgraph_node_remove_callees (node);
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Materialization Call site updates done.\n");
#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif
  cgraph_remove_unreachable_nodes (false, cgraph_dump_file);
}

#include "gt-cgraphunit.h"
