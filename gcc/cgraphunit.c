/* Driver of optimization process
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

/* This module implements main driver of compilation process.

   The main scope of this file is to act as an interface in between
   tree based frontends and the backend.

   The front-end is supposed to use following functionality:

    - cgraph_finalize_function

      This function is called once front-end has parsed whole body of function
      and it is certain that the function body nor the declaration will change.

      (There is one exception needed for implementing GCC extern inline
	function.)

    - varpool_finalize_decl

      This function has same behavior as the above but is used for static
      variables.

    - add_asm_node

      Insert new toplevel ASM statement

    - finalize_compilation_unit

      This function is called once (source level) compilation unit is finalized
      and it will no longer change.

      The symbol table is constructed starting from the trivially needed
      symbols finalized by the frontend.  Functions are lowered into
      GIMPLE representation and callgraph/reference lists are constructed.
      Those are used to discover other necessary functions and variables.

      At the end the bodies of unreachable functions are removed.

      The function can be called multiple times when multiple source level
      compilation units are combined.

    - compile

      This passes control to the back-end.  Optimizations are performed and
      final assembler is generated.  This is done in the following way. Note
      that with link time optimization the process is split into three
      stages (compile time, linktime analysis and parallel linktime as
      indicated bellow).

      Compile time:

	1) Inter-procedural optimization.
	   (ipa_passes)

	   This part is further split into:

	   a) early optimizations. These are local passes executed in
	      the topological order on the callgraph.

	      The purpose of early optimiations is to optimize away simple
	      things that may otherwise confuse IP analysis. Very simple
	      propagation across the callgraph is done i.e. to discover
	      functions without side effects and simple inlining is performed.

	   b) early small interprocedural passes.

	      Those are interprocedural passes executed only at compilation
	      time.  These include, for example, transational memory lowering,
	      unreachable code removal and other simple transformations.

	   c) IP analysis stage.  All interprocedural passes do their
	      analysis.

	      Interprocedural passes differ from small interprocedural
	      passes by their ability to operate across whole program
	      at linktime.  Their analysis stage is performed early to
	      both reduce linking times and linktime memory usage by	
	      not having to represent whole program in memory.

	   d) LTO sreaming.  When doing LTO, everything important gets
	      streamed into the object file.

       Compile time and or linktime analysis stage (WPA):

	      At linktime units gets streamed back and symbol table is
	      merged.  Function bodies are not streamed in and not
	      available.
	   e) IP propagation stage.  All IP passes execute their
	      IP propagation. This is done based on the earlier analysis
	      without having function bodies at hand.
	   f) Ltrans streaming.  When doing WHOPR LTO, the program
	      is partitioned and streamed into multple object files.

       Compile time and/or parallel linktime stage (ltrans)

	      Each of the object files is streamed back and compiled
	      separately.  Now the function bodies becomes available
	      again.

	 2) Virtual clone materialization
	    (cgraph_materialize_clone)

	    IP passes can produce copies of existing functoins (such
	    as versioned clones or inline clones) without actually
	    manipulating their bodies by creating virtual clones in
	    the callgraph. At this time the virtual clones are
	    turned into real functions
	 3) IP transformation

	    All IP passes transform function bodies based on earlier
	    decision of the IP propagation.

	 4) late small IP passes

	    Simple IP passes working within single program partition.

	 5) Expansion
	    (expand_all_functions)

	    At this stage functions that needs to be output into
	    assembler are identified and compiled in topological order
	 6) Output of variables and aliases
	    Now it is known what variable references was not optimized
	    out and thus all variables are output to the file.

	    Note that with -fno-toplevel-reorder passes 5 and 6
	    are combined together in cgraph_output_in_order.  

   Finally there are functions to manipulate the callgraph from
   backend.
    - cgraph_add_new_function is used to add backend produced
      functions introduced after the unit is finalized.
      The functions are enqueue for later processing and inserted
      into callgraph with cgraph_process_new_functions.

    - cgraph_function_versioning

      produces a copy of function into new one (a version)
      and apply simple transformations
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "output.h"
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
#include "params.h"
#include "fibheap.h"
#include "intl.h"
#include "function.h"
#include "ipa-prop.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "gimple-pretty-print.h"
#include "output.h"
#include "coverage.h"
#include "plugin.h"
#include "ipa-inline.h"
#include "ipa-utils.h"
#include "lto-streamer.h"
#include "except.h"
#include "cfgloop.h"
#include "regset.h"     /* FIXME: For reg_obstack.  */
#include "context.h"
#include "pass_manager.h"

/* Queue of cgraph nodes scheduled to be added into cgraph.  This is a
   secondary queue used during optimization to accommodate passes that
   may generate new functions that need to be optimized and expanded.  */
cgraph_node_set cgraph_new_nodes;

static void expand_all_functions (void);
static void mark_functions_to_output (void);
static void expand_function (struct cgraph_node *);
static void analyze_function (struct cgraph_node *);
static void handle_alias_pairs (void);

FILE *cgraph_dump_file;

/* Linked list of cgraph asm nodes.  */
struct asm_node *asm_nodes;

/* Last node in cgraph_asm_nodes.  */
static GTY(()) struct asm_node *asm_last_node;

/* Used for vtable lookup in thunk adjusting.  */
static GTY (()) tree vtable_entry_type;

/* Determine if symbol DECL is needed.  That is, visible to something
   either outside this translation unit, something magic in the system
   configury */
bool
decide_is_symbol_needed (symtab_node node)
{
  tree decl = node->symbol.decl;

  /* Double check that no one output the function into assembly file
     early.  */
  gcc_checking_assert (!DECL_ASSEMBLER_NAME_SET_P (decl)
	               || !TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)));

  if (!node->symbol.definition)
    return false;

  if (DECL_EXTERNAL (decl))
    return false;

  /* If the user told us it is used, then it must be so.  */
  if (node->symbol.force_output)
    return true;

  /* ABI forced symbols are needed when they are external.  */
  if (node->symbol.forced_by_abi && TREE_PUBLIC (decl))
    return true;

 /* Keep constructors, destructors and virtual functions.  */
   if (TREE_CODE (decl) == FUNCTION_DECL
       && (DECL_STATIC_CONSTRUCTOR (decl) || DECL_STATIC_DESTRUCTOR (decl)))
    return true;

  /* Externally visible variables must be output.  The exception is
     COMDAT variables that must be output only when they are needed.  */
  if (TREE_PUBLIC (decl) && !DECL_COMDAT (decl))
    return true;

  return false;
}

/* Head of the queue of nodes to be processed while building callgraph */

static symtab_node first = (symtab_node)(void *)1;

/* Add NODE to queue starting at FIRST. 
   The queue is linked via AUX pointers and terminated by pointer to 1.  */

static void
enqueue_node (symtab_node node)
{
  if (node->symbol.aux)
    return;
  gcc_checking_assert (first);
  node->symbol.aux = first;
  first = node;
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
  cgraph_node_set_iterator csi;

  if (!cgraph_new_nodes)
    return false;
  handle_alias_pairs ();
  /*  Note that this queue may grow as its being processed, as the new
      functions may generate new ones.  */
  for (csi = csi_start (cgraph_new_nodes); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      fndecl = node->symbol.decl;
      switch (cgraph_state)
	{
	case CGRAPH_STATE_CONSTRUCTION:
	  /* At construction time we just need to finalize function and move
	     it into reachable functions list.  */

	  cgraph_finalize_function (fndecl, false);
	  output = true;
          cgraph_call_function_insertion_hooks (node);
	  enqueue_node ((symtab_node) node);
	  break;

	case CGRAPH_STATE_IPA:
	case CGRAPH_STATE_IPA_SSA:
	  /* When IPA optimization already started, do all essential
	     transformations that has been already performed on the whole
	     cgraph but not on this function.  */

	  gimple_register_cfg_hooks ();
	  if (!node->symbol.analyzed)
	    analyze_function (node);
	  push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	  if (cgraph_state == CGRAPH_STATE_IPA_SSA
	      && !gimple_in_ssa_p (DECL_STRUCT_FUNCTION (fndecl)))
	    g->get_passes ()->execute_early_local_passes ();
	  else if (inline_summary_vec != NULL)
	    compute_inline_parameters (node, true);
	  free_dominance_info (CDI_POST_DOMINATORS);
	  free_dominance_info (CDI_DOMINATORS);
	  pop_cfun ();
          cgraph_call_function_insertion_hooks (node);
	  break;

	case CGRAPH_STATE_EXPANSION:
	  /* Functions created during expansion shall be compiled
	     directly.  */
	  node->process = 0;
          cgraph_call_function_insertion_hooks (node);
	  expand_function (node);
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
    }
  free_cgraph_node_set (cgraph_new_nodes);
  cgraph_new_nodes = NULL;
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

void
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
  node->symbol.analyzed = false;
  node->symbol.definition = false;
  node->symbol.alias = false;
  node->symbol.weakref = false;
  node->symbol.cpp_implicit_alias = false;

  cgraph_node_remove_callees (node);
  ipa_remove_all_references (&node->symbol.ref_list);
}

/* Return true when there are references to NODE.  */

static bool
referred_to_p (symtab_node node)
{
  struct ipa_ref *ref;

  /* See if there are any references at all.  */
  if (ipa_ref_list_referring_iterate (&node->symbol.ref_list, 0, ref))
    return true;
  /* For functions check also calls.  */
  cgraph_node *cn = dyn_cast <cgraph_node> (node);
  if (cn && cn->callers)
    return true;
  return false;
}

/* DECL has been parsed.  Take it, queue it, compile it at the whim of the
   logic in effect.  If NO_COLLECT is true, then our caller cannot stand to have
   the garbage collector run at the moment.  We would need to either create
   a new GC context, or just not compile right now.  */

void
cgraph_finalize_function (tree decl, bool no_collect)
{
  struct cgraph_node *node = cgraph_get_create_node (decl);

  if (node->symbol.definition)
    {
      /* Nested functions should only be defined once.  */
      gcc_assert (!DECL_CONTEXT (decl)
		  || TREE_CODE (DECL_CONTEXT (decl)) !=	FUNCTION_DECL);
      cgraph_reset_node (node);
      node->local.redefined_extern_inline = true;
    }

  notice_global_symbol (decl);
  node->symbol.definition = true;
  node->lowered = DECL_STRUCT_FUNCTION (decl)->cfg != NULL;

  /* With -fkeep-inline-functions we are keeping all inline functions except
     for extern inline ones.  */
  if (flag_keep_inline_functions
      && DECL_DECLARED_INLINE_P (decl)
      && !DECL_EXTERNAL (decl)
      && !DECL_DISREGARD_INLINE_LIMITS (decl))
    node->symbol.force_output = 1;

  /* When not optimizing, also output the static functions. (see
     PR24561), but don't do so for always_inline functions, functions
     declared inline and nested functions.  These were optimized out
     in the original implementation and it is unclear whether we want
     to change the behavior here.  */
  if ((!optimize
       && !node->symbol.cpp_implicit_alias
       && !DECL_DISREGARD_INLINE_LIMITS (decl)
       && !DECL_DECLARED_INLINE_P (decl)
       && !(DECL_CONTEXT (decl)
	    && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL))
      && !DECL_COMDAT (decl) && !DECL_EXTERNAL (decl))
    node->symbol.force_output = 1;

  /* If we've not yet emitted decl, tell the debug info about it.  */
  if (!TREE_ASM_WRITTEN (decl))
    (*debug_hooks->deferred_inline_function) (decl);

  /* Possibly warn about unused parameters.  */
  if (warn_unused_parameter)
    do_warn_unused_parameter (decl);

  if (!no_collect)
    ggc_collect ();

  if (cgraph_state == CGRAPH_STATE_CONSTRUCTION
      && (decide_is_symbol_needed ((symtab_node) node)
	  || referred_to_p ((symtab_node)node)))
    enqueue_node ((symtab_node)node);
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
  gcc::pass_manager *passes = g->get_passes ();
  struct cgraph_node *node;
  switch (cgraph_state)
    {
      case CGRAPH_STATE_PARSING:
	cgraph_finalize_function (fndecl, false);
	break;
      case CGRAPH_STATE_CONSTRUCTION:
	/* Just enqueue function to be processed at nearest occurrence.  */
	node = cgraph_create_node (fndecl);
	if (lowered)
	  node->lowered = true;
	if (!cgraph_new_nodes)
	  cgraph_new_nodes = cgraph_node_set_new ();
	cgraph_node_set_add (cgraph_new_nodes, node);
        break;

      case CGRAPH_STATE_IPA:
      case CGRAPH_STATE_IPA_SSA:
      case CGRAPH_STATE_EXPANSION:
	/* Bring the function into finalized state and enqueue for later
	   analyzing and compilation.  */
	node = cgraph_get_create_node (fndecl);
	node->local.local = false;
	node->symbol.definition = true;
	node->symbol.force_output = true;
	if (!lowered && cgraph_state == CGRAPH_STATE_EXPANSION)
	  {
	    push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	    gimple_register_cfg_hooks ();
	    bitmap_obstack_initialize (NULL);
	    execute_pass_list (passes->all_lowering_passes);
	    passes->execute_early_local_passes ();
	    bitmap_obstack_release (NULL);
	    pop_cfun ();

	    lowered = true;
	  }
	if (lowered)
	  node->lowered = true;
	if (!cgraph_new_nodes)
	  cgraph_new_nodes = cgraph_node_set_new ();
	cgraph_node_set_add (cgraph_new_nodes, node);
        break;

      case CGRAPH_STATE_FINISHED:
	/* At the very end of compilation we have to do all the work up
	   to expansion.  */
	node = cgraph_create_node (fndecl);
	if (lowered)
	  node->lowered = true;
	node->symbol.definition = true;
	analyze_function (node);
	push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	gimple_register_cfg_hooks ();
	bitmap_obstack_initialize (NULL);
	if (!gimple_in_ssa_p (DECL_STRUCT_FUNCTION (fndecl)))
	  g->get_passes ()->execute_early_local_passes ();
	bitmap_obstack_release (NULL);
	pop_cfun ();
	expand_function (node);
	break;

      default:
	gcc_unreachable ();
    }

  /* Set a personality if required and we already passed EH lowering.  */
  if (lowered
      && (function_needs_eh_personality (DECL_STRUCT_FUNCTION (fndecl))
	  == eh_personality_lang))
    DECL_FUNCTION_PERSONALITY (fndecl) = lang_hooks.eh_personality ();
}

/* Add a top-level asm statement to the list.  */

struct asm_node *
add_asm_node (tree asm_str)
{
  struct asm_node *node;

  node = ggc_alloc_cleared_asm_node ();
  node->asm_str = asm_str;
  node->order = symtab_order++;
  node->next = NULL;
  if (asm_nodes == NULL)
    asm_nodes = node;
  else
    asm_last_node->next = node;
  asm_last_node = node;
  return node;
}

/* Output all asm statements we have stored up to be output.  */

static void
output_asm_statements (void)
{
  struct asm_node *can;

  if (seen_error ())
    return;

  for (can = asm_nodes; can; can = can->next)
    assemble_asm (can->asm_str);
  asm_nodes = NULL;
}

/* Analyze the function scheduled to be output.  */
static void
analyze_function (struct cgraph_node *node)
{
  tree decl = node->symbol.decl;
  location_t saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (decl);

  if (node->symbol.alias)
    symtab_resolve_alias
       ((symtab_node) node, (symtab_node) cgraph_get_node (node->symbol.alias_target));
  else if (node->thunk.thunk_p)
    {
      cgraph_create_edge (node, cgraph_get_node (node->thunk.alias),
			  NULL, 0, CGRAPH_FREQ_BASE);
      node->thunk.alias = NULL;
    }
  else if (node->dispatcher_function)
    {
      /* Generate the dispatcher body of multi-versioned functions.  */
      struct cgraph_function_version_info *dispatcher_version_info
	= get_cgraph_node_version (node);
      if (dispatcher_version_info != NULL
          && (dispatcher_version_info->dispatcher_resolver
	      == NULL_TREE))
	{
	  tree resolver = NULL_TREE;
	  gcc_assert (targetm.generate_version_dispatcher_body);
	  resolver = targetm.generate_version_dispatcher_body (node);
	  gcc_assert (resolver != NULL_TREE);
	}
    }
  else
    {
      push_cfun (DECL_STRUCT_FUNCTION (decl));

      assign_assembler_name_if_neeeded (node->symbol.decl);

      /* Make sure to gimplify bodies only once.  During analyzing a
	 function we lower it, which will require gimplified nested
	 functions, so we can end up here with an already gimplified
	 body.  */
      if (!gimple_has_body_p (decl))
	gimplify_function_tree (decl);
      dump_function (TDI_generic, decl);

      /* Lower the function.  */
      if (!node->lowered)
	{
	  if (node->nested)
	    lower_nested_functions (node->symbol.decl);
	  gcc_assert (!node->nested);

	  gimple_register_cfg_hooks ();
	  bitmap_obstack_initialize (NULL);
	  execute_pass_list (g->get_passes ()->all_lowering_passes);
	  free_dominance_info (CDI_POST_DOMINATORS);
	  free_dominance_info (CDI_DOMINATORS);
	  compact_blocks ();
	  bitmap_obstack_release (NULL);
	  node->lowered = true;
	}

      pop_cfun ();
    }
  node->symbol.analyzed = true;

  input_location = saved_loc;
}

/* C++ frontend produce same body aliases all over the place, even before PCH
   gets streamed out. It relies on us linking the aliases with their function
   in order to do the fixups, but ipa-ref is not PCH safe.  Consequentely we
   first produce aliases without links, but once C++ FE is sure he won't sream
   PCH we build the links via this function.  */

void
cgraph_process_same_body_aliases (void)
{
  symtab_node node;
  FOR_EACH_SYMBOL (node)
    if (node->symbol.cpp_implicit_alias && !node->symbol.analyzed)
      symtab_resolve_alias
        (node,
	 TREE_CODE (node->symbol.alias_target) == VAR_DECL
	 ? (symtab_node)varpool_node_for_decl (node->symbol.alias_target)
	 : (symtab_node)cgraph_get_create_node (node->symbol.alias_target));
  cpp_implicit_aliases_done = true;
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

  for (node = cgraph_first_function (); node != first;
       node = cgraph_next_function (node))
    {
      tree decl = node->symbol.decl;
      if (DECL_PRESERVE_P (decl))
	cgraph_mark_force_output_node (node);
      else if (lookup_attribute ("externally_visible", DECL_ATTRIBUTES (decl)))
	{
	  if (! TREE_PUBLIC (node->symbol.decl))
	    warning_at (DECL_SOURCE_LOCATION (node->symbol.decl), OPT_Wattributes,
			"%<externally_visible%>"
			" attribute have effect only on public objects");
	}
      if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl))
	  && (node->symbol.definition && !node->symbol.alias))
	{
	  warning_at (DECL_SOURCE_LOCATION (node->symbol.decl), OPT_Wattributes,
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
  for (vnode = varpool_first_variable (); vnode != first_var;
       vnode = varpool_next_variable (vnode))
    {
      tree decl = vnode->symbol.decl;
      if (DECL_EXTERNAL (decl)
	  && DECL_INITIAL (decl))
	varpool_finalize_decl (decl);
      if (DECL_PRESERVE_P (decl))
	vnode->symbol.force_output = true;
      else if (lookup_attribute ("externally_visible", DECL_ATTRIBUTES (decl)))
	{
	  if (! TREE_PUBLIC (vnode->symbol.decl))
	    warning_at (DECL_SOURCE_LOCATION (vnode->symbol.decl), OPT_Wattributes,
			"%<externally_visible%>"
			" attribute have effect only on public objects");
	}
      if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl))
	  && vnode->symbol.definition
	  && DECL_INITIAL (decl))
	{
	  warning_at (DECL_SOURCE_LOCATION (vnode->symbol.decl), OPT_Wattributes,
		      "%<weakref%> attribute ignored"
		      " because variable is initialized");
	  DECL_WEAK (decl) = 0;
	  DECL_ATTRIBUTES (decl) = remove_attribute ("weakref",
						      DECL_ATTRIBUTES (decl));
	}
      process_common_attributes (decl);
    }
}

/* Mark DECL as finalized.  By finalizing the declaration, frontend instruct the
   middle end to output the variable to asm file, if needed or externally
   visible.  */

void
varpool_finalize_decl (tree decl)
{
  struct varpool_node *node = varpool_node_for_decl (decl);

  gcc_assert (TREE_STATIC (decl) || DECL_EXTERNAL (decl));

  if (node->symbol.definition)
    return;
  notice_global_symbol (decl);
  node->symbol.definition = true;
  if (TREE_THIS_VOLATILE (decl) || DECL_PRESERVE_P (decl)
      /* Traditionally we do not eliminate static variables when not
	 optimizing and when not doing toplevel reoder.  */
      || (!flag_toplevel_reorder && !DECL_COMDAT (node->symbol.decl)
	  && !DECL_ARTIFICIAL (node->symbol.decl)))
    node->symbol.force_output = true;

  if (cgraph_state == CGRAPH_STATE_CONSTRUCTION
      && (decide_is_symbol_needed ((symtab_node) node)
	  || referred_to_p ((symtab_node)node)))
    enqueue_node ((symtab_node)node);
  if (cgraph_state >= CGRAPH_STATE_IPA_SSA)
    varpool_analyze_node (node);
  /* Some frontends produce various interface variables after compilation
     finished.  */
  if (cgraph_state == CGRAPH_STATE_FINISHED)
    varpool_assemble_decl (node);
}

/* EDGE is an polymorphic call.  Mark all possible targets as reachable
   and if there is only one target, perform trivial devirtualization. 
   REACHABLE_CALL_TARGETS collects target lists we already walked to
   avoid udplicate work.  */

static void
walk_polymorphic_call_targets (pointer_set_t *reachable_call_targets,
			       struct cgraph_edge *edge)
{
  unsigned int i;
  void *cache_token;
  bool final;
  vec <cgraph_node *>targets
    = possible_polymorphic_call_targets
	(edge, &final, &cache_token);

  if (!pointer_set_insert (reachable_call_targets,
			   cache_token))
    {
      if (cgraph_dump_file)
	dump_possible_polymorphic_call_targets 
	  (cgraph_dump_file, edge);

      for (i = 0; i < targets.length(); i++)
	{
	  /* Do not bother to mark virtual methods in anonymous namespace;
	     either we will find use of virtual table defining it, or it is
	     unused.  */
	  if (targets[i]->symbol.definition
	      && TREE_CODE
		  (TREE_TYPE (targets[i]->symbol.decl))
		   == METHOD_TYPE
	      && !type_in_anonymous_namespace_p
		   (method_class_type
		     (TREE_TYPE (targets[i]->symbol.decl))))
	  enqueue_node ((symtab_node) targets[i]);
	}
    }

  /* Very trivial devirtualization; when the type is
     final or anonymous (so we know all its derivation)
     and there is only one possible virtual call target,
     make the edge direct.  */
  if (final)
    {
      if (targets.length() <= 1)
	{
	  cgraph_node *target;
	  if (targets.length () == 1)
	    target = targets[0];
	  else
	    target = cgraph_get_create_node
		       (builtin_decl_implicit (BUILT_IN_UNREACHABLE));

	  if (cgraph_dump_file)
	    {
	      fprintf (cgraph_dump_file,
		       "Devirtualizing call: ");
	      print_gimple_stmt (cgraph_dump_file,
				 edge->call_stmt, 0,
				 TDF_SLIM);
	    }
	  cgraph_make_edge_direct (edge, target);
	  cgraph_redirect_edge_call_stmt_to_callee (edge);
	  if (cgraph_dump_file)
	    {
	      fprintf (cgraph_dump_file,
		       "Devirtualized as: ");
	      print_gimple_stmt (cgraph_dump_file,
				 edge->call_stmt, 0,
				 TDF_SLIM);
	    }
	}
    }
}


/* Discover all functions and variables that are trivially needed, analyze
   them as well as all functions and variables referred by them  */

static void
analyze_functions (void)
{
  /* Keep track of already processed nodes when called multiple times for
     intermodule optimization.  */
  static struct cgraph_node *first_analyzed;
  struct cgraph_node *first_handled = first_analyzed;
  static struct varpool_node *first_analyzed_var;
  struct varpool_node *first_handled_var = first_analyzed_var;
  struct pointer_set_t *reachable_call_targets = pointer_set_create ();

  symtab_node node, next;
  int i;
  struct ipa_ref *ref;
  bool changed = true;

  bitmap_obstack_initialize (NULL);
  cgraph_state = CGRAPH_STATE_CONSTRUCTION;

  /* Ugly, but the fixup can not happen at a time same body alias is created;
     C++ FE is confused about the COMDAT groups being right.  */
  if (cpp_implicit_aliases_done)
    FOR_EACH_SYMBOL (node)
      if (node->symbol.cpp_implicit_alias)
	  fixup_same_cpp_alias_visibility (node, symtab_alias_target (node));
  if (optimize && flag_devirtualize)
    build_type_inheritance_graph ();

  /* Analysis adds static variables that in turn adds references to new functions.
     So we need to iterate the process until it stabilize.  */
  while (changed)
    {
      changed = false;
      process_function_and_variable_attributes (first_analyzed,
						first_analyzed_var);

      /* First identify the trivially needed symbols.  */
      for (node = symtab_nodes;
	   node != (symtab_node)first_analyzed
	   && node != (symtab_node)first_analyzed_var; node = node->symbol.next)
	{
	  if (decide_is_symbol_needed (node))
	    {
	      enqueue_node (node);
	      if (!changed && cgraph_dump_file)
		fprintf (cgraph_dump_file, "Trivially needed symbols:");
	      changed = true;
	      if (cgraph_dump_file)
		fprintf (cgraph_dump_file, " %s", symtab_node_asm_name (node));
	      if (!changed && cgraph_dump_file)
		fprintf (cgraph_dump_file, "\n");
	    }
	  if (node == (symtab_node)first_analyzed
	      || node == (symtab_node)first_analyzed_var)
	    break;
	}
      cgraph_process_new_functions ();
      first_analyzed_var = varpool_first_variable ();
      first_analyzed = cgraph_first_function ();

      if (changed && dump_file)
	fprintf (cgraph_dump_file, "\n");

      /* Lower representation, build callgraph edges and references for all trivially
         needed symbols and all symbols referred by them.  */
      while (first != (symtab_node)(void *)1)
	{
	  changed = true;
	  node = first;
	  first = (symtab_node)first->symbol.aux;
	  cgraph_node *cnode = dyn_cast <cgraph_node> (node);
	  if (cnode && cnode->symbol.definition)
	    {
	      struct cgraph_edge *edge;
	      tree decl = cnode->symbol.decl;

	      /* ??? It is possible to create extern inline function
	      and later using weak alias attribute to kill its body.
	      See gcc.c-torture/compile/20011119-1.c  */
	      if (!DECL_STRUCT_FUNCTION (decl)
		  && !cnode->symbol.alias
		  && !cnode->thunk.thunk_p
		  && !cnode->dispatcher_function)
		{
		  cgraph_reset_node (cnode);
		  cnode->local.redefined_extern_inline = true;
		  continue;
		}

	      if (!cnode->symbol.analyzed)
		analyze_function (cnode);

	      for (edge = cnode->callees; edge; edge = edge->next_callee)
		if (edge->callee->symbol.definition)
		   enqueue_node ((symtab_node)edge->callee);
	      if (optimize && flag_devirtualize)
		{
		  struct cgraph_edge *next;

		  for (edge = cnode->indirect_calls; edge; edge = next)
		    {
		      next = edge->next_callee;
		      if (edge->indirect_info->polymorphic)
			walk_polymorphic_call_targets (reachable_call_targets,
						       edge);
		    }
		}

	      /* If decl is a clone of an abstract function,
	      mark that abstract function so that we don't release its body.
	      The DECL_INITIAL() of that abstract function declaration
	      will be later needed to output debug info.  */
	      if (DECL_ABSTRACT_ORIGIN (decl))
		{
		  struct cgraph_node *origin_node
	    	  = cgraph_get_node (DECL_ABSTRACT_ORIGIN (decl));
		  origin_node->used_as_abstract_origin = true;
		}
	    }
	  else
	    {
	      varpool_node *vnode = dyn_cast <varpool_node> (node);
	      if (vnode && vnode->symbol.definition && !vnode->symbol.analyzed)
		varpool_analyze_node (vnode);
	    }

	  if (node->symbol.same_comdat_group)
	    {
	      symtab_node next;
	      for (next = node->symbol.same_comdat_group;
		   next != node;
		   next = next->symbol.same_comdat_group)
		enqueue_node (next);
	    }
	  for (i = 0; ipa_ref_list_reference_iterate (&node->symbol.ref_list, i, ref); i++)
	    if (ref->referred->symbol.definition)
	      enqueue_node (ref->referred);
          cgraph_process_new_functions ();
	}
    }
  if (optimize && flag_devirtualize)
    update_type_inheritance_graph ();

  /* Collect entry points to the unit.  */
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "\n\nInitial ");
      dump_symtab (cgraph_dump_file);
    }

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "\nRemoving unused symbols:");

  for (node = symtab_nodes;
       node != (symtab_node)first_handled
       && node != (symtab_node)first_handled_var; node = next)
    {
      next = node->symbol.next;
      if (!node->symbol.aux && !referred_to_p (node))
	{
	  if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, " %s", symtab_node_name (node));
	  symtab_remove_node (node);
	  continue;
	}
      if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
	{
	  tree decl = node->symbol.decl;

	  if (cnode->symbol.definition && !gimple_has_body_p (decl)
	      && !cnode->symbol.alias
	      && !cnode->thunk.thunk_p)
	    cgraph_reset_node (cnode);

	  gcc_assert (!cnode->symbol.definition || cnode->thunk.thunk_p
		      || cnode->symbol.alias
		      || gimple_has_body_p (decl));
	  gcc_assert (cnode->symbol.analyzed == cnode->symbol.definition);
	}
      node->symbol.aux = NULL;
    }
  for (;node; node = node->symbol.next)
    node->symbol.aux = NULL;
  first_analyzed = cgraph_first_function ();
  first_analyzed_var = varpool_first_variable ();
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "\n\nReclaimed ");
      dump_symtab (cgraph_dump_file);
    }
  bitmap_obstack_release (NULL);
  pointer_set_destroy (reachable_call_targets);
  ggc_collect ();
  /* Initialize assembler name hash, in particular we want to trigger C++
     mangling and same body alias creation before we free DECL_ARGUMENTS
     used by it.  */
  if (!seen_error ())
    symtab_initialize_asm_name_hash ();
}

/* Translate the ugly representation of aliases as alias pairs into nice
   representation in callgraph.  We don't handle all cases yet,
   unfortunately.  */

static void
handle_alias_pairs (void)
{
  alias_pair *p;
  unsigned i;
  
  for (i = 0; alias_pairs && alias_pairs->iterate (i, &p);)
    {
      symtab_node target_node = symtab_node_for_asm (p->target);

      /* Weakrefs with target not defined in current unit are easy to handle:
	 they behave just as external variables except we need to note the
	 alias flag to later output the weakref pseudo op into asm file.  */
      if (!target_node
	  && lookup_attribute ("weakref", DECL_ATTRIBUTES (p->decl)) != NULL)
	{
	  symtab_node node = symtab_get_node (p->decl);
	  if (node)
	    {
	      node->symbol.alias_target = p->target;
	      node->symbol.weakref = true;
	      node->symbol.alias = true;
	    }
	  alias_pairs->unordered_remove (i);
	  continue;
	}
      else if (!target_node)
	{
	  error ("%q+D aliased to undefined symbol %qE", p->decl, p->target);
	  symtab_node node = symtab_get_node (p->decl);
	  if (node)
	    node->symbol.alias = false;
	  alias_pairs->unordered_remove (i);
	  continue;
	}

      if (DECL_EXTERNAL (target_node->symbol.decl)
	  /* We use local aliases for C++ thunks to force the tailcall
	     to bind locally.  This is a hack - to keep it working do
	     the following (which is not strictly correct).  */
	  && (! TREE_CODE (target_node->symbol.decl) == FUNCTION_DECL
	      || ! DECL_VIRTUAL_P (target_node->symbol.decl))
	  && ! lookup_attribute ("weakref", DECL_ATTRIBUTES (p->decl)))
	{
	  error ("%q+D aliased to external symbol %qE",
		 p->decl, p->target);
	}

      if (TREE_CODE (p->decl) == FUNCTION_DECL
          && target_node && is_a <cgraph_node> (target_node))
	{
	  struct cgraph_node *src_node = cgraph_get_node (p->decl);
	  if (src_node && src_node->symbol.definition)
            cgraph_reset_node (src_node);
	  cgraph_create_function_alias (p->decl, target_node->symbol.decl);
	  alias_pairs->unordered_remove (i);
	}
      else if (TREE_CODE (p->decl) == VAR_DECL
	       && target_node && is_a <varpool_node> (target_node))
	{
	  varpool_create_variable_alias (p->decl, target_node->symbol.decl);
	  alias_pairs->unordered_remove (i);
	}
      else
	{
	  error ("%q+D alias in between function and variable is not supported",
		 p->decl);
	  warning (0, "%q+D aliased declaration",
		   target_node->symbol.decl);
	  alias_pairs->unordered_remove (i);
	}
    }
  vec_free (alias_pairs);
}


/* Figure out what functions we want to assemble.  */

static void
mark_functions_to_output (void)
{
  struct cgraph_node *node;
#ifdef ENABLE_CHECKING
  bool check_same_comdat_groups = false;

  FOR_EACH_FUNCTION (node)
    gcc_assert (!node->process);
#endif

  FOR_EACH_FUNCTION (node)
    {
      tree decl = node->symbol.decl;

      gcc_assert (!node->process || node->symbol.same_comdat_group);
      if (node->process)
	continue;

      /* We need to output all local functions that are used and not
	 always inlined, as well as those that are reachable from
	 outside the current compilation unit.  */
      if (node->symbol.analyzed
	  && !node->thunk.thunk_p
	  && !node->symbol.alias
	  && !node->global.inlined_to
	  && !TREE_ASM_WRITTEN (decl)
	  && !DECL_EXTERNAL (decl))
	{
	  node->process = 1;
	  if (node->symbol.same_comdat_group)
	    {
	      struct cgraph_node *next;
	      for (next = cgraph (node->symbol.same_comdat_group);
		   next != node;
		   next = cgraph (next->symbol.same_comdat_group))
		if (!next->thunk.thunk_p && !next->symbol.alias)
		  next->process = 1;
	    }
	}
      else if (node->symbol.same_comdat_group)
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
	      && !node->symbol.in_other_partition
	      && !node->symbol.alias
	      && !node->clones
	      && !DECL_EXTERNAL (decl))
	    {
	      dump_cgraph_node (stderr, node);
	      internal_error ("failed to reclaim unneeded function");
	    }
#endif
	  gcc_assert (node->global.inlined_to
		      || !gimple_has_body_p (decl)
		      || node->symbol.in_other_partition
		      || node->clones
		      || DECL_ARTIFICIAL (decl)
		      || DECL_EXTERNAL (decl));

	}

    }
#ifdef ENABLE_CHECKING
  if (check_same_comdat_groups)
    FOR_EACH_FUNCTION (node)
      if (node->symbol.same_comdat_group && !node->process)
	{
	  tree decl = node->symbol.decl;
	  if (!node->global.inlined_to
	      && gimple_has_body_p (decl)
	      /* FIXME: in an ltrans unit when the offline copy is outside a
		 partition but inline copies are inside a partition, we can
		 end up not removing the body since we no longer have an
		 analyzed node pointing to it.  */
	      && !node->symbol.in_other_partition
	      && !node->clones
	      && !DECL_EXTERNAL (decl))
	    {
	      dump_cgraph_node (stderr, node);
	      internal_error ("failed to reclaim unneeded function in same "
			      "comdat group");
	    }
	}
#endif
}

/* DECL is FUNCTION_DECL.  Initialize datastructures so DECL is a function
   in lowered gimple form.  IN_SSA is true if the gimple is in SSA.
   
   Set current_function_decl and cfun to newly constructed empty function body.
   return basic block in the function body.  */

basic_block
init_lowered_empty_function (tree decl, bool in_ssa)
{
  basic_block bb;

  current_function_decl = decl;
  allocate_struct_function (decl, false);
  gimple_register_cfg_hooks ();
  init_empty_tree_cfg ();

  if (in_ssa)
    {
      init_tree_ssa (cfun);
      init_ssa_operands (cfun);
      cfun->gimple_df->in_ssa_p = true;
      cfun->curr_properties |= PROP_ssa;
    }

  DECL_INITIAL (decl) = make_node (BLOCK);

  DECL_SAVED_TREE (decl) = error_mark_node;
  cfun->curr_properties |= (PROP_gimple_lcf | PROP_gimple_leh | PROP_gimple_any
			    | PROP_cfg | PROP_loops);

  set_loops_for_fn (cfun, ggc_alloc_cleared_loops ());
  init_loops_structure (cfun, loops_for_fn (cfun), 1);
  loops_for_fn (cfun)->state |= LOOPS_MAY_HAVE_MULTIPLE_LATCHES;

  /* Create BB for body of the function and connect it properly.  */
  bb = create_basic_block (NULL, (void *) 0, ENTRY_BLOCK_PTR);
  make_edge (ENTRY_BLOCK_PTR, bb, EDGE_FALLTHRU);
  make_edge (bb, EXIT_BLOCK_PTR, 0);
  add_bb_to_loop (bb, ENTRY_BLOCK_PTR->loop_father);

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
	create_tmp_reg (build_pointer_type
			  (build_pointer_type (vtable_entry_type)), "vptr");

      /* The vptr is always at offset zero in the object.  */
      stmt = gimple_build_assign (vtabletmp,
				  build1 (NOP_EXPR, TREE_TYPE (vtabletmp),
					  ptr));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Form the vtable address.  */
      vtabletmp2 = create_tmp_reg (TREE_TYPE (TREE_TYPE (vtabletmp)),
				     "vtableaddr");
      stmt = gimple_build_assign (vtabletmp2,
				  build_simple_mem_ref (vtabletmp));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Find the entry with the vcall offset.  */
      stmt = gimple_build_assign (vtabletmp2,
				  fold_build_pointer_plus_loc (input_location,
							       vtabletmp2,
							       virtual_offset));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

      /* Get the offset itself.  */
      vtabletmp3 = create_tmp_reg (TREE_TYPE (TREE_TYPE (vtabletmp2)),
				     "vcalloffset");
      stmt = gimple_build_assign (vtabletmp3,
				  build_simple_mem_ref (vtabletmp2));
      gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

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
          ptrtmp = create_tmp_reg (TREE_TYPE (ptr), "ptr");
          stmt = gimple_build_assign (ptrtmp, ptr);
	  gsi_insert_after (bsi, stmt, GSI_NEW_STMT);
	}
      ptr = fold_build_pointer_plus_hwi_loc (input_location,
					     ptrtmp, fixed_offset);
    }

  /* Emit the statement and gimplify the adjustment expression.  */
  ret = create_tmp_reg (TREE_TYPE (ptr), "adjusted_this");
  stmt = gimple_build_assign (ret, ptr);
  gsi_insert_after (bsi, stmt, GSI_NEW_STMT);

  return ret;
}

/* Produce assembler for thunk NODE.  */

void
expand_thunk (struct cgraph_node *node)
{
  bool this_adjusting = node->thunk.this_adjusting;
  HOST_WIDE_INT fixed_offset = node->thunk.fixed_offset;
  HOST_WIDE_INT virtual_value = node->thunk.virtual_value;
  tree virtual_offset = NULL;
  tree alias = node->callees->callee->symbol.decl;
  tree thunk_fndecl = node->symbol.decl;
  tree a;

  if (in_lto_p)
    cgraph_get_body (node);
  a = DECL_ARGUMENTS (thunk_fndecl);

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
      insn_locations_init ();
      set_curr_insn_location (DECL_SOURCE_LOCATION (thunk_fndecl));
      prologue_location = curr_insn_location ();
      assemble_start_function (thunk_fndecl, fnname);

      targetm.asm_out.output_mi_thunk (asm_out_file, thunk_fndecl,
				       fixed_offset, virtual_value, alias);

      assemble_end_function (thunk_fndecl, fnname);
      insn_locations_finalize ();
      init_insn_lengths ();
      free_after_compilation (cfun);
      set_cfun (NULL);
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
      node->thunk.thunk_p = false;
      node->symbol.analyzed = false;
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
      vec<tree> vargs;

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

      bb = then_bb = else_bb = return_bb = init_lowered_empty_function (thunk_fndecl, true);

      bsi = gsi_start_bb (bb);

      /* Build call to the function being thunked.  */
      if (!VOID_TYPE_P (restype))
	{
	  if (DECL_BY_REFERENCE (resdecl))
	    restmp = gimple_fold_indirect_ref (resdecl);
	  else if (!is_gimple_reg_type (restype))
	    {
	      restmp = resdecl;
	      add_local_decl (cfun, restmp);
	      BLOCK_VARS (DECL_INITIAL (current_function_decl)) = restmp;
	    }
	  else
	    restmp = create_tmp_reg (restype, "retval");
	}

      for (arg = a; arg; arg = DECL_CHAIN (arg))
        nargs++;
      vargs.create (nargs);
      if (this_adjusting)
        vargs.quick_push (thunk_adjust (&bsi, a, 1, fixed_offset,
					virtual_offset));
      else if (nargs)
        vargs.quick_push (a);

      if (nargs)
        for (i = 1, arg = DECL_CHAIN (a); i < nargs; i++, arg = DECL_CHAIN (arg))
	  vargs.quick_push (arg);
      call = gimple_build_call_vec (build_fold_addr_expr_loc (0, alias), vargs);
      vargs.release ();
      gimple_call_set_from_thunk (call, true);
      if (restmp)
	{
          gimple_call_set_lhs (call, restmp);
	  gcc_assert (useless_type_conversion_p (TREE_TYPE (restmp),
						 TREE_TYPE (TREE_TYPE (alias))));
	}
      gsi_insert_after (&bsi, call, GSI_NEW_STMT);
      if (!(gimple_call_flags (call) & ECF_NORETURN))
	{
	  if (restmp && !this_adjusting
	      && (fixed_offset || virtual_offset))
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
		  add_bb_to_loop (then_bb, bb->loop_father);
		  add_bb_to_loop (return_bb, bb->loop_father);
		  add_bb_to_loop (else_bb, bb->loop_father);
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
	}
      else
	{
	  gimple_call_set_tail (call, true);
	  remove_edge (single_succ_edge (bb));
	}

      delete_unreachable_blocks ();
      update_ssa (TODO_update_ssa);
#ifdef ENABLE_CHECKING
      verify_flow_info ();
#endif

      /* Since we want to emit the thunk, we explicitly mark its name as
	 referenced.  */
      node->thunk.thunk_p = false;
      rebuild_cgraph_edges ();
      cgraph_add_new_function (thunk_fndecl, true);
      bitmap_obstack_release (NULL);
    }
  current_function_decl = NULL;
  set_cfun (NULL);
}

/* Assemble thunks and aliases associated to NODE.  */

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
        expand_thunk (thunk);
      }
    else
      e = e->next_caller;
  for (i = 0; ipa_ref_list_referring_iterate (&node->symbol.ref_list,
					     i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      {
	struct cgraph_node *alias = ipa_ref_referring_node (ref);
        bool saved_written = TREE_ASM_WRITTEN (node->symbol.decl);

	/* Force assemble_alias to really output the alias this time instead
	   of buffering it in same alias pairs.  */
	TREE_ASM_WRITTEN (node->symbol.decl) = 1;
	do_assemble_alias (alias->symbol.decl,
			   DECL_ASSEMBLER_NAME (node->symbol.decl));
	assemble_thunks_and_aliases (alias);
	TREE_ASM_WRITTEN (node->symbol.decl) = saved_written;
      }
}

/* Expand function specified by NODE.  */

static void
expand_function (struct cgraph_node *node)
{
  tree decl = node->symbol.decl;
  location_t saved_loc;

  /* We ought to not compile any inline clones.  */
  gcc_assert (!node->global.inlined_to);

  announce_function (decl);
  node->process = 0;
  gcc_assert (node->lowered);
  cgraph_get_body (node);

  /* Generate RTL for the body of DECL.  */

  timevar_push (TV_REST_OF_COMPILATION);

  gcc_assert (cgraph_global_info_ready);

  /* Initialize the default bitmap obstack.  */
  bitmap_obstack_initialize (NULL);

  /* Initialize the RTL code for the function.  */
  current_function_decl = decl;
  saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (decl);
  init_function_start (decl);

  gimple_register_cfg_hooks ();

  bitmap_obstack_initialize (&reg_obstack); /* FIXME, only at RTL generation*/

  execute_all_ipa_transforms ();

  /* Perform all tree transforms and optimizations.  */

  /* Signal the start of passes.  */
  invoke_plugin_callbacks (PLUGIN_ALL_PASSES_START, NULL);

  execute_pass_list (g->get_passes ()->all_passes);

  /* Signal the end of passes.  */
  invoke_plugin_callbacks (PLUGIN_ALL_PASSES_END, NULL);

  bitmap_obstack_release (&reg_obstack);

  /* Release the default bitmap obstack.  */
  bitmap_obstack_release (NULL);

  /* If requested, warn about function definitions where the function will
     return a value (usually of some struct or union type) which itself will
     take up a lot of stack space.  */
  if (warn_larger_than && !DECL_EXTERNAL (decl) && TREE_TYPE (decl))
    {
      tree ret_type = TREE_TYPE (TREE_TYPE (decl));

      if (ret_type && TYPE_SIZE_UNIT (ret_type)
	  && TREE_CODE (TYPE_SIZE_UNIT (ret_type)) == INTEGER_CST
	  && 0 < compare_tree_int (TYPE_SIZE_UNIT (ret_type),
				   larger_than_size))
	{
	  unsigned int size_as_int
	    = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (ret_type));

	  if (compare_tree_int (TYPE_SIZE_UNIT (ret_type), size_as_int) == 0)
	    warning (OPT_Wlarger_than_, "size of return value of %q+D is %u bytes",
                     decl, size_as_int);
	  else
	    warning (OPT_Wlarger_than_, "size of return value of %q+D is larger than %wd bytes",
                     decl, larger_than_size);
	}
    }

  gimple_set_body (decl, NULL);
  if (DECL_STRUCT_FUNCTION (decl) == 0
      && !cgraph_get_node (decl)->origin)
    {
      /* Stop pointing to the local nodes about to be freed.
	 But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.
	 For a nested function, this is done in c_pop_function_context.
	 If rest_of_compilation set this to 0, leave it 0.  */
      if (DECL_INITIAL (decl) != 0)
	DECL_INITIAL (decl) = error_mark_node;
    }

  input_location = saved_loc;

  ggc_collect ();
  timevar_pop (TV_REST_OF_COMPILATION);

  /* Make sure that BE didn't give up on compiling.  */
  gcc_assert (TREE_ASM_WRITTEN (decl));
  set_cfun (NULL);
  current_function_decl = NULL;

  /* It would make a lot more sense to output thunks before function body to get more
     forward and lest backwarding jumps.  This however would need solving problem
     with comdats. See PR48668.  Also aliases must come after function itself to
     make one pass assemblers, like one on AIX, happy.  See PR 50689.
     FIXME: Perhaps thunks should be move before function IFF they are not in comdat
     groups.  */
  assemble_thunks_and_aliases (node);
  cgraph_release_function_body (node);
  /* Eliminate all call edges.  This is important so the GIMPLE_CALL no longer
     points to the dead function body.  */
  cgraph_node_remove_callees (node);
  ipa_remove_all_references (&node->symbol.ref_list);
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
expand_all_functions (void)
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
	  node->process = 0;
	  expand_function (node);
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
    struct asm_node *a;
  } u;
};

/* Output all functions, variables, and asm statements in the order
   according to their order fields, which is the order in which they
   appeared in the file.  This implements -fno-toplevel-reorder.  In
   this mode we may output functions and variables which don't really
   need to be output.  */

static void
output_in_order (void)
{
  int max;
  struct cgraph_order_sort *nodes;
  int i;
  struct cgraph_node *pf;
  struct varpool_node *pv;
  struct asm_node *pa;

  max = symtab_order;
  nodes = XCNEWVEC (struct cgraph_order_sort, max);

  FOR_EACH_DEFINED_FUNCTION (pf)
    {
      if (pf->process && !pf->thunk.thunk_p && !pf->symbol.alias)
	{
	  i = pf->symbol.order;
	  gcc_assert (nodes[i].kind == ORDER_UNDEFINED);
	  nodes[i].kind = ORDER_FUNCTION;
	  nodes[i].u.f = pf;
	}
    }

  FOR_EACH_DEFINED_VARIABLE (pv)
    if (!DECL_EXTERNAL (pv->symbol.decl))
      {
	i = pv->symbol.order;
	gcc_assert (nodes[i].kind == ORDER_UNDEFINED);
	nodes[i].kind = ORDER_VAR;
	nodes[i].u.v = pv;
      }

  for (pa = asm_nodes; pa; pa = pa->next)
    {
      i = pa->order;
      gcc_assert (nodes[i].kind == ORDER_UNDEFINED);
      nodes[i].kind = ORDER_ASM;
      nodes[i].u.a = pa;
    }

  /* In toplevel reorder mode we output all statics; mark them as needed.  */

  for (i = 0; i < max; ++i)
    if (nodes[i].kind == ORDER_VAR)
      varpool_finalize_named_section_flags (nodes[i].u.v);

  for (i = 0; i < max; ++i)
    {
      switch (nodes[i].kind)
	{
	case ORDER_FUNCTION:
	  nodes[i].u.f->process = 0;
	  expand_function (nodes[i].u.f);
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

  asm_nodes = NULL;
  free (nodes);
}

static void
ipa_passes (void)
{
  gcc::pass_manager *passes = g->get_passes ();

  set_cfun (NULL);
  current_function_decl = NULL;
  gimple_register_cfg_hooks ();
  bitmap_obstack_initialize (NULL);

  invoke_plugin_callbacks (PLUGIN_ALL_IPA_PASSES_START, NULL);

  if (!in_lto_p)
    {
      execute_ipa_pass_list (passes->all_small_ipa_passes);
      if (seen_error ())
	return;
    }

  /* We never run removal of unreachable nodes after early passes.  This is
     because TODO is run before the subpasses.  It is important to remove
     the unreachable functions to save works at IPA level and to get LTO
     symbol tables right.  */
  symtab_remove_unreachable_nodes (true, cgraph_dump_file);

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
	((struct ipa_opt_pass_d *) passes->all_regular_ipa_passes);
    }

  /* Some targets need to handle LTO assembler output specially.  */
  if (flag_generate_lto)
    targetm.asm_out.lto_start ();

  execute_ipa_summary_passes ((struct ipa_opt_pass_d *)
			      passes->all_lto_gen_passes);

  if (!in_lto_p)
    ipa_write_summaries ();

  if (flag_generate_lto)
    targetm.asm_out.lto_end ();

  if (!flag_ltrans && (in_lto_p || !flag_lto || flag_fat_lto_objects))
    execute_ipa_pass_list (passes->all_regular_ipa_passes);
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
   at expansion time.  Emit all necessary aliases.  */

static void
output_weakrefs (void)
{
  symtab_node node;
  FOR_EACH_SYMBOL (node)
    if (node->symbol.alias
        && !TREE_ASM_WRITTEN (node->symbol.decl)
	&& node->symbol.weakref)
      {
	tree target;

	/* Weakrefs are special by not requiring target definition in current
	   compilation unit.  It is thus bit hard to work out what we want to
	   alias.
	   When alias target is defined, we need to fetch it from symtab reference,
	   otherwise it is pointed to by alias_target.  */
	if (node->symbol.alias_target)
	  target = (DECL_P (node->symbol.alias_target)
		    ? DECL_ASSEMBLER_NAME (node->symbol.alias_target)
		    : node->symbol.alias_target);
	else if (node->symbol.analyzed)
	  target = DECL_ASSEMBLER_NAME (symtab_alias_target (node)->symbol.decl);
	else
	  {
	    gcc_unreachable ();
	    target = get_alias_symbol (node->symbol.decl);
	  }
        do_assemble_alias (node->symbol.decl, target);
      }
}

/* Initialize callgraph dump file.  */

void
init_cgraph (void)
{
  if (!cgraph_dump_file)
    cgraph_dump_file = dump_begin (TDI_cgraph, NULL);
}


/* Perform simple optimizations based on callgraph.  */

void
compile (void)
{
  if (seen_error ())
    return;

#ifdef ENABLE_CHECKING
  verify_symtab ();
#endif

  timevar_push (TV_CGRAPHOPT);
  if (pre_ipa_mem_report)
    {
      fprintf (stderr, "Memory consumption before IPA\n");
      dump_memory_report (false);
    }
  if (!quiet_flag)
    fprintf (stderr, "Performing interprocedural optimizations\n");
  cgraph_state = CGRAPH_STATE_IPA;

  /* If LTO is enabled, initialize the streamer hooks needed by GIMPLE.  */
  if (flag_lto)
    lto_streamer_hooks_init ();

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
  symtab_remove_unreachable_nodes (false, dump_file);
  cgraph_global_info_ready = true;
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Optimized ");
      dump_symtab (cgraph_dump_file);
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
  verify_symtab ();
#endif

  cgraph_materialize_all_clones ();
  bitmap_obstack_initialize (NULL);
  execute_ipa_pass_list (g->get_passes ()->all_late_ipa_passes);
  symtab_remove_unreachable_nodes (true, dump_file);
#ifdef ENABLE_CHECKING
  verify_symtab ();
#endif
  bitmap_obstack_release (NULL);
  mark_functions_to_output ();

  /* When weakref support is missing, we autmatically translate all
     references to NODE to references to its ultimate alias target.
     The renaming mechanizm uses flag IDENTIFIER_TRANSPARENT_ALIAS and
     TREE_CHAIN.

     Set up this mapping before we output any assembler but once we are sure
     that all symbol renaming is done.

     FIXME: All this uglyness can go away if we just do renaming at gimple
     level by physically rewritting the IL.  At the moment we can only redirect
     calls, so we need infrastructure for renaming references as well.  */
#ifndef ASM_OUTPUT_WEAKREF
  symtab_node node;

  FOR_EACH_SYMBOL (node)
    if (node->symbol.alias
	&& lookup_attribute ("weakref", DECL_ATTRIBUTES (node->symbol.decl)))
      {
	IDENTIFIER_TRANSPARENT_ALIAS
	   (DECL_ASSEMBLER_NAME (node->symbol.decl)) = 1;
	TREE_CHAIN (DECL_ASSEMBLER_NAME (node->symbol.decl))
	   = (node->symbol.alias_target ? node->symbol.alias_target
	      : DECL_ASSEMBLER_NAME (symtab_alias_target (node)->symbol.decl));
      }
#endif

  cgraph_state = CGRAPH_STATE_EXPANSION;
  if (!flag_toplevel_reorder)
    output_in_order ();
  else
    {
      output_asm_statements ();

      expand_all_functions ();
      varpool_output_variables ();
    }

  cgraph_process_new_functions ();
  cgraph_state = CGRAPH_STATE_FINISHED;
  output_weakrefs ();

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "\nFinal ");
      dump_symtab (cgraph_dump_file);
    }
#ifdef ENABLE_CHECKING
  verify_symtab ();
  /* Double check that all inline clones are gone and that all
     function bodies have been released from memory.  */
  if (!seen_error ())
    {
      struct cgraph_node *node;
      bool error_found = false;

      FOR_EACH_DEFINED_FUNCTION (node)
	if (node->global.inlined_to
	    || gimple_has_body_p (node->symbol.decl))
	  {
	    error_found = true;
	    dump_cgraph_node (stderr, node);
	  }
      if (error_found)
	internal_error ("nodes with unreleased memory found");
    }
#endif
}


/* Analyze the whole compilation unit once it is parsed completely.  */

void
finalize_compilation_unit (void)
{
  timevar_push (TV_CGRAPH);

  /* If we're here there's no current function anymore.  Some frontends
     are lazy in clearing these.  */
  current_function_decl = NULL;
  set_cfun (NULL);

  /* Do not skip analyzing the functions if there were errors, we
     miss diagnostics for following functions otherwise.  */

  /* Emit size functions we didn't inline.  */
  finalize_size_functions ();

  /* Mark alias targets necessary and emit diagnostics.  */
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
  analyze_functions ();

  /* Mark alias targets necessary and emit diagnostics.  */
  handle_alias_pairs ();

  /* Gimplify and lower thunks.  */
  analyze_functions ();

  /* Finally drive the pass manager.  */
  compile ();

  timevar_pop (TV_CGRAPH);
}


#include "gt-cgraphunit.h"
