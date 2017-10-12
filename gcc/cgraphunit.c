/* Driver of optimization process
   Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

    - finalize_function

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "regset.h"     /* FIXME: For reg_obstack.  */
#include "alloc-pool.h"
#include "tree-pass.h"
#include "stringpool.h"
#include "gimple-ssa.h"
#include "cgraph.h"
#include "coverage.h"
#include "lto-streamer.h"
#include "fold-const.h"
#include "varasm.h"
#include "stor-layout.h"
#include "output.h"
#include "cfgcleanup.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "langhooks.h"
#include "toplev.h"
#include "debug.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-prop.h"
#include "gimple-pretty-print.h"
#include "plugin.h"
#include "ipa-fnsummary.h"
#include "ipa-utils.h"
#include "except.h"
#include "cfgloop.h"
#include "context.h"
#include "pass_manager.h"
#include "tree-nested.h"
#include "dbgcnt.h"
#include "tree-chkp.h"
#include "lto-section-names.h"
#include "stringpool.h"
#include "attribs.h"

/* Queue of cgraph nodes scheduled to be added into cgraph.  This is a
   secondary queue used during optimization to accommodate passes that
   may generate new functions that need to be optimized and expanded.  */
vec<cgraph_node *> cgraph_new_nodes;

static void expand_all_functions (void);
static void mark_functions_to_output (void);
static void handle_alias_pairs (void);

/* Used for vtable lookup in thunk adjusting.  */
static GTY (()) tree vtable_entry_type;

/* Return true if this symbol is a function from the C frontend specified
   directly in RTL form (with "__RTL").  */

bool
symtab_node::native_rtl_p () const
{
  if (TREE_CODE (decl) != FUNCTION_DECL)
    return false;
  if (!DECL_STRUCT_FUNCTION (decl))
    return false;
  return DECL_STRUCT_FUNCTION (decl)->curr_properties & PROP_rtl;
}

/* Determine if symbol declaration is needed.  That is, visible to something
   either outside this translation unit, something magic in the system
   configury */
bool
symtab_node::needed_p (void)
{
  /* Double check that no one output the function into assembly file
     early.  */
  if (!native_rtl_p ())
      gcc_checking_assert
	(!DECL_ASSEMBLER_NAME_SET_P (decl)
	 || !TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)));

  if (!definition)
    return false;

  if (DECL_EXTERNAL (decl))
    return false;

  /* If the user told us it is used, then it must be so.  */
  if (force_output)
    return true;

  /* ABI forced symbols are needed when they are external.  */
  if (forced_by_abi && TREE_PUBLIC (decl))
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

/* Head and terminator of the queue of nodes to be processed while building
   callgraph.  */

static symtab_node symtab_terminator;
static symtab_node *queued_nodes = &symtab_terminator;

/* Add NODE to queue starting at QUEUED_NODES. 
   The queue is linked via AUX pointers and terminated by pointer to 1.  */

static void
enqueue_node (symtab_node *node)
{
  if (node->aux)
    return;
  gcc_checking_assert (queued_nodes);
  node->aux = queued_nodes;
  queued_nodes = node;
}

/* Process CGRAPH_NEW_FUNCTIONS and perform actions necessary to add these
   functions into callgraph in a way so they look like ordinary reachable
   functions inserted into callgraph already at construction time.  */

void
symbol_table::process_new_functions (void)
{
  tree fndecl;

  if (!cgraph_new_nodes.exists ())
    return;

  handle_alias_pairs ();
  /*  Note that this queue may grow as its being processed, as the new
      functions may generate new ones.  */
  for (unsigned i = 0; i < cgraph_new_nodes.length (); i++)
    {
      cgraph_node *node = cgraph_new_nodes[i];
      fndecl = node->decl;
      switch (state)
	{
	case CONSTRUCTION:
	  /* At construction time we just need to finalize function and move
	     it into reachable functions list.  */

	  cgraph_node::finalize_function (fndecl, false);
	  call_cgraph_insertion_hooks (node);
	  enqueue_node (node);
	  break;

	case IPA:
	case IPA_SSA:
	case IPA_SSA_AFTER_INLINING:
	  /* When IPA optimization already started, do all essential
	     transformations that has been already performed on the whole
	     cgraph but not on this function.  */

	  gimple_register_cfg_hooks ();
	  if (!node->analyzed)
	    node->analyze ();
	  push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	  if ((state == IPA_SSA || state == IPA_SSA_AFTER_INLINING)
	      && !gimple_in_ssa_p (DECL_STRUCT_FUNCTION (fndecl)))
	    {
	      bool summaried_computed = ipa_fn_summaries != NULL;
	      g->get_passes ()->execute_early_local_passes ();
	      /* Early passes compure inline parameters to do inlining
		 and splitting.  This is redundant for functions added late.
		 Just throw away whatever it did.  */
	      if (!summaried_computed)
		ipa_free_fn_summary ();
	    }
	  else if (ipa_fn_summaries != NULL)
	    compute_fn_summary (node, true);
	  free_dominance_info (CDI_POST_DOMINATORS);
	  free_dominance_info (CDI_DOMINATORS);
	  pop_cfun ();
	  call_cgraph_insertion_hooks (node);
	  break;

	case EXPANSION:
	  /* Functions created during expansion shall be compiled
	     directly.  */
	  node->process = 0;
	  call_cgraph_insertion_hooks (node);
	  node->expand ();
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
    }

  cgraph_new_nodes.release ();
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
cgraph_node::reset (void)
{
  /* If process is set, then we have already begun whole-unit analysis.
     This is *not* testing for whether we've already emitted the function.
     That case can be sort-of legitimately seen with real function redefinition
     errors.  I would argue that the front end should never present us with
     such a case, but don't enforce that for now.  */
  gcc_assert (!process);

  /* Reset our data structures so we can analyze the function again.  */
  memset (&local, 0, sizeof (local));
  memset (&global, 0, sizeof (global));
  memset (&rtl, 0, sizeof (rtl));
  analyzed = false;
  definition = false;
  alias = false;
  transparent_alias = false;
  weakref = false;
  cpp_implicit_alias = false;

  remove_callees ();
  remove_all_references ();
}

/* Return true when there are references to the node.  INCLUDE_SELF is
   true if a self reference counts as a reference.  */

bool
symtab_node::referred_to_p (bool include_self)
{
  ipa_ref *ref = NULL;

  /* See if there are any references at all.  */
  if (iterate_referring (0, ref))
    return true;
  /* For functions check also calls.  */
  cgraph_node *cn = dyn_cast <cgraph_node *> (this);
  if (cn && cn->callers)
    {
      if (include_self)
	return true;
      for (cgraph_edge *e = cn->callers; e; e = e->next_caller)
	if (e->caller != this)
	  return true;
    }
  return false;
}

/* DECL has been parsed.  Take it, queue it, compile it at the whim of the
   logic in effect.  If NO_COLLECT is true, then our caller cannot stand to have
   the garbage collector run at the moment.  We would need to either create
   a new GC context, or just not compile right now.  */

void
cgraph_node::finalize_function (tree decl, bool no_collect)
{
  cgraph_node *node = cgraph_node::get_create (decl);

  if (node->definition)
    {
      /* Nested functions should only be defined once.  */
      gcc_assert (!DECL_CONTEXT (decl)
		  || TREE_CODE (DECL_CONTEXT (decl)) !=	FUNCTION_DECL);
      node->reset ();
      node->local.redefined_extern_inline = true;
    }

  /* Set definition first before calling notice_global_symbol so that
     it is available to notice_global_symbol.  */
  node->definition = true;
  notice_global_symbol (decl);
  node->lowered = DECL_STRUCT_FUNCTION (decl)->cfg != NULL;
  if (!flag_toplevel_reorder)
    node->no_reorder = true;

  /* With -fkeep-inline-functions we are keeping all inline functions except
     for extern inline ones.  */
  if (flag_keep_inline_functions
      && DECL_DECLARED_INLINE_P (decl)
      && !DECL_EXTERNAL (decl)
      && !DECL_DISREGARD_INLINE_LIMITS (decl))
    node->force_output = 1;

  /* __RTL functions were already output as soon as they were parsed (due
     to the large amount of global state in the backend).
     Mark such functions as "force_output" to reflect the fact that they
     will be in the asm file when considering the symbols they reference.
     The attempt to output them later on will bail out immediately.  */
  if (node->native_rtl_p ())
    node->force_output = 1;

  /* When not optimizing, also output the static functions. (see
     PR24561), but don't do so for always_inline functions, functions
     declared inline and nested functions.  These were optimized out
     in the original implementation and it is unclear whether we want
     to change the behavior here.  */
  if (((!opt_for_fn (decl, optimize) || flag_keep_static_functions
	|| node->no_reorder)
       && !node->cpp_implicit_alias
       && !DECL_DISREGARD_INLINE_LIMITS (decl)
       && !DECL_DECLARED_INLINE_P (decl)
       && !(DECL_CONTEXT (decl)
	    && TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL))
      && !DECL_COMDAT (decl) && !DECL_EXTERNAL (decl))
    node->force_output = 1;

  /* If we've not yet emitted decl, tell the debug info about it.  */
  if (!TREE_ASM_WRITTEN (decl))
    (*debug_hooks->deferred_inline_function) (decl);

  if (!no_collect)
    ggc_collect ();

  if (symtab->state == CONSTRUCTION
      && (node->needed_p () || node->referred_to_p ()))
    enqueue_node (node);
}

/* Add the function FNDECL to the call graph.
   Unlike finalize_function, this function is intended to be used
   by middle end and allows insertion of new function at arbitrary point
   of compilation.  The function can be either in high, low or SSA form
   GIMPLE.

   The function is assumed to be reachable and have address taken (so no
   API breaking optimizations are performed on it).

   Main work done by this function is to enqueue the function for later
   processing to avoid need the passes to be re-entrant.  */

void
cgraph_node::add_new_function (tree fndecl, bool lowered)
{
  gcc::pass_manager *passes = g->get_passes ();
  cgraph_node *node;

  if (dump_file)
    {
      struct function *fn = DECL_STRUCT_FUNCTION (fndecl);
      const char *function_type = ((gimple_has_body_p (fndecl))
				   ? (lowered
				      ? (gimple_in_ssa_p (fn)
					 ? "ssa gimple"
					 : "low gimple")
				      : "high gimple")
				   : "to-be-gimplified");
      fprintf (dump_file,
	       "Added new %s function %s to callgraph\n",
	       function_type,
	       fndecl_name (fndecl));
    }

  switch (symtab->state)
    {
      case PARSING:
	cgraph_node::finalize_function (fndecl, false);
	break;
      case CONSTRUCTION:
	/* Just enqueue function to be processed at nearest occurrence.  */
	node = cgraph_node::get_create (fndecl);
	if (lowered)
	  node->lowered = true;
	cgraph_new_nodes.safe_push (node);
        break;

      case IPA:
      case IPA_SSA:
      case IPA_SSA_AFTER_INLINING:
      case EXPANSION:
	/* Bring the function into finalized state and enqueue for later
	   analyzing and compilation.  */
	node = cgraph_node::get_create (fndecl);
	node->local.local = false;
	node->definition = true;
	node->force_output = true;
	if (TREE_PUBLIC (fndecl))
	  node->externally_visible = true;
	if (!lowered && symtab->state == EXPANSION)
	  {
	    push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	    gimple_register_cfg_hooks ();
	    bitmap_obstack_initialize (NULL);
	    execute_pass_list (cfun, passes->all_lowering_passes);
	    passes->execute_early_local_passes ();
	    bitmap_obstack_release (NULL);
	    pop_cfun ();

	    lowered = true;
	  }
	if (lowered)
	  node->lowered = true;
	cgraph_new_nodes.safe_push (node);
        break;

      case FINISHED:
	/* At the very end of compilation we have to do all the work up
	   to expansion.  */
	node = cgraph_node::create (fndecl);
	if (lowered)
	  node->lowered = true;
	node->definition = true;
	node->analyze ();
	push_cfun (DECL_STRUCT_FUNCTION (fndecl));
	gimple_register_cfg_hooks ();
	bitmap_obstack_initialize (NULL);
	if (!gimple_in_ssa_p (DECL_STRUCT_FUNCTION (fndecl)))
	  g->get_passes ()->execute_early_local_passes ();
	bitmap_obstack_release (NULL);
	pop_cfun ();
	node->expand ();
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

/* Analyze the function scheduled to be output.  */
void
cgraph_node::analyze (void)
{
  if (native_rtl_p ())
    {
      analyzed = true;
      return;
    }

  tree decl = this->decl;
  location_t saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (decl);

  if (thunk.thunk_p)
    {
      cgraph_node *t = cgraph_node::get (thunk.alias);

      create_edge (t, NULL, t->count, CGRAPH_FREQ_BASE);
      callees->can_throw_external = !TREE_NOTHROW (t->decl);
      /* Target code in expand_thunk may need the thunk's target
	 to be analyzed, so recurse here.  */
      if (!t->analyzed)
	t->analyze ();
      if (t->alias)
	{
	  t = t->get_alias_target ();
	  if (!t->analyzed)
	    t->analyze ();
	}
      if (!expand_thunk (false, false))
	{
	  thunk.alias = NULL;
	  return;
	}
      thunk.alias = NULL;
    }
  if (alias)
    resolve_alias (cgraph_node::get (alias_target), transparent_alias);
  else if (dispatcher_function)
    {
      /* Generate the dispatcher body of multi-versioned functions.  */
      cgraph_function_version_info *dispatcher_version_info
	= function_version ();
      if (dispatcher_version_info != NULL
          && (dispatcher_version_info->dispatcher_resolver
	      == NULL_TREE))
	{
	  tree resolver = NULL_TREE;
	  gcc_assert (targetm.generate_version_dispatcher_body);
	  resolver = targetm.generate_version_dispatcher_body (this);
	  gcc_assert (resolver != NULL_TREE);
	}
    }
  else
    {
      push_cfun (DECL_STRUCT_FUNCTION (decl));

      assign_assembler_name_if_needed (decl);

      /* Make sure to gimplify bodies only once.  During analyzing a
	 function we lower it, which will require gimplified nested
	 functions, so we can end up here with an already gimplified
	 body.  */
      if (!gimple_has_body_p (decl))
	gimplify_function_tree (decl);

      /* Lower the function.  */
      if (!lowered)
	{
	  if (nested)
	    lower_nested_functions (decl);
	  gcc_assert (!nested);

	  gimple_register_cfg_hooks ();
	  bitmap_obstack_initialize (NULL);
	  execute_pass_list (cfun, g->get_passes ()->all_lowering_passes);
	  free_dominance_info (CDI_POST_DOMINATORS);
	  free_dominance_info (CDI_DOMINATORS);
	  compact_blocks ();
	  bitmap_obstack_release (NULL);
	  lowered = true;
	}

      pop_cfun ();
    }
  analyzed = true;

  input_location = saved_loc;
}

/* C++ frontend produce same body aliases all over the place, even before PCH
   gets streamed out. It relies on us linking the aliases with their function
   in order to do the fixups, but ipa-ref is not PCH safe.  Consequentely we
   first produce aliases without links, but once C++ FE is sure he won't sream
   PCH we build the links via this function.  */

void
symbol_table::process_same_body_aliases (void)
{
  symtab_node *node;
  FOR_EACH_SYMBOL (node)
    if (node->cpp_implicit_alias && !node->analyzed)
      node->resolve_alias
	(VAR_P (node->alias_target)
	 ? (symtab_node *)varpool_node::get_create (node->alias_target)
	 : (symtab_node *)cgraph_node::get_create (node->alias_target));
  cpp_implicit_aliases_done = true;
}

/* Process attributes common for vars and functions.  */

static void
process_common_attributes (symtab_node *node, tree decl)
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

  if (lookup_attribute ("no_reorder", DECL_ATTRIBUTES (decl)))
    node->no_reorder = 1;
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

   Furthermore, we can't mark these nodes in finalize_function because:

    void f() {}
    void f() __attribute__((externally_visible));

   is valid.

   So, we walk the nodes at the end of the translation unit, applying the
   attributes at that point.  */

static void
process_function_and_variable_attributes (cgraph_node *first,
                                          varpool_node *first_var)
{
  cgraph_node *node;
  varpool_node *vnode;

  for (node = symtab->first_function (); node != first;
       node = symtab->next_function (node))
    {
      tree decl = node->decl;
      if (DECL_PRESERVE_P (decl))
	node->mark_force_output ();
      else if (lookup_attribute ("externally_visible", DECL_ATTRIBUTES (decl)))
	{
	  if (! TREE_PUBLIC (node->decl))
	    warning_at (DECL_SOURCE_LOCATION (node->decl), OPT_Wattributes,
			"%<externally_visible%>"
			" attribute have effect only on public objects");
	}
      if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl))
	  && (node->definition && !node->alias))
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
     
      process_common_attributes (node, decl);
    }
  for (vnode = symtab->first_variable (); vnode != first_var;
       vnode = symtab->next_variable (vnode))
    {
      tree decl = vnode->decl;
      if (DECL_EXTERNAL (decl)
	  && DECL_INITIAL (decl))
	varpool_node::finalize_decl (decl);
      if (DECL_PRESERVE_P (decl))
	vnode->force_output = true;
      else if (lookup_attribute ("externally_visible", DECL_ATTRIBUTES (decl)))
	{
	  if (! TREE_PUBLIC (vnode->decl))
	    warning_at (DECL_SOURCE_LOCATION (vnode->decl), OPT_Wattributes,
			"%<externally_visible%>"
			" attribute have effect only on public objects");
	}
      if (lookup_attribute ("weakref", DECL_ATTRIBUTES (decl))
	  && vnode->definition
	  && DECL_INITIAL (decl))
	{
	  warning_at (DECL_SOURCE_LOCATION (vnode->decl), OPT_Wattributes,
		      "%<weakref%> attribute ignored"
		      " because variable is initialized");
	  DECL_WEAK (decl) = 0;
	  DECL_ATTRIBUTES (decl) = remove_attribute ("weakref",
						      DECL_ATTRIBUTES (decl));
	}
      process_common_attributes (vnode, decl);
    }
}

/* Mark DECL as finalized.  By finalizing the declaration, frontend instruct the
   middle end to output the variable to asm file, if needed or externally
   visible.  */

void
varpool_node::finalize_decl (tree decl)
{
  varpool_node *node = varpool_node::get_create (decl);

  gcc_assert (TREE_STATIC (decl) || DECL_EXTERNAL (decl));

  if (node->definition)
    return;
  /* Set definition first before calling notice_global_symbol so that
     it is available to notice_global_symbol.  */
  node->definition = true;
  notice_global_symbol (decl);
  if (!flag_toplevel_reorder)
    node->no_reorder = true;
  if (TREE_THIS_VOLATILE (decl) || DECL_PRESERVE_P (decl)
      /* Traditionally we do not eliminate static variables when not
	 optimizing and when not doing toplevel reoder.  */
      || (node->no_reorder && !DECL_COMDAT (node->decl)
	  && !DECL_ARTIFICIAL (node->decl)))
    node->force_output = true;

  if (symtab->state == CONSTRUCTION
      && (node->needed_p () || node->referred_to_p ()))
    enqueue_node (node);
  if (symtab->state >= IPA_SSA)
    node->analyze ();
  /* Some frontends produce various interface variables after compilation
     finished.  */
  if (symtab->state == FINISHED
      || (node->no_reorder
	  && symtab->state == EXPANSION))
    node->assemble_decl ();

  if (DECL_INITIAL (decl))
    chkp_register_var_initializer (decl);
}

/* EDGE is an polymorphic call.  Mark all possible targets as reachable
   and if there is only one target, perform trivial devirtualization. 
   REACHABLE_CALL_TARGETS collects target lists we already walked to
   avoid udplicate work.  */

static void
walk_polymorphic_call_targets (hash_set<void *> *reachable_call_targets,
			       cgraph_edge *edge)
{
  unsigned int i;
  void *cache_token;
  bool final;
  vec <cgraph_node *>targets
    = possible_polymorphic_call_targets
	(edge, &final, &cache_token);

  if (!reachable_call_targets->add (cache_token))
    {
      if (symtab->dump_file)
	dump_possible_polymorphic_call_targets 
	  (symtab->dump_file, edge);

      for (i = 0; i < targets.length (); i++)
	{
	  /* Do not bother to mark virtual methods in anonymous namespace;
	     either we will find use of virtual table defining it, or it is
	     unused.  */
	  if (targets[i]->definition
	      && TREE_CODE
		  (TREE_TYPE (targets[i]->decl))
		   == METHOD_TYPE
	      && !type_in_anonymous_namespace_p
		   (TYPE_METHOD_BASETYPE (TREE_TYPE (targets[i]->decl))))
	    enqueue_node (targets[i]);
	}
    }

  /* Very trivial devirtualization; when the type is
     final or anonymous (so we know all its derivation)
     and there is only one possible virtual call target,
     make the edge direct.  */
  if (final)
    {
      if (targets.length () <= 1 && dbg_cnt (devirt))
	{
	  cgraph_node *target;
	  if (targets.length () == 1)
	    target = targets[0];
	  else
	    target = cgraph_node::create
			(builtin_decl_implicit (BUILT_IN_UNREACHABLE));

	  if (symtab->dump_file)
	    {
	      fprintf (symtab->dump_file,
		       "Devirtualizing call: ");
	      print_gimple_stmt (symtab->dump_file,
				 edge->call_stmt, 0,
				 TDF_SLIM);
	    }
          if (dump_enabled_p ())
            {
	      location_t locus = gimple_location_safe (edge->call_stmt);
	      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, locus,
			       "devirtualizing call in %s to %s\n",
			       edge->caller->name (), target->name ());
	    }

	  edge->make_direct (target);
	  edge->redirect_call_stmt_to_callee ();

	  /* Call to __builtin_unreachable shouldn't be instrumented.  */
	  if (!targets.length ())
	    gimple_call_set_with_bounds (edge->call_stmt, false);

	  if (symtab->dump_file)
	    {
	      fprintf (symtab->dump_file,
		       "Devirtualized as: ");
	      print_gimple_stmt (symtab->dump_file,
				 edge->call_stmt, 0,
				 TDF_SLIM);
	    }
	}
    }
}

/* Issue appropriate warnings for the global declaration DECL.  */

static void
check_global_declaration (symtab_node *snode)
{
  const char *decl_file;
  tree decl = snode->decl;

  /* Warn about any function declared static but not defined.  We don't
     warn about variables, because many programs have static variables
     that exist only to get some text into the object file.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_INITIAL (decl) == 0
      && DECL_EXTERNAL (decl)
      && ! DECL_ARTIFICIAL (decl)
      && ! TREE_NO_WARNING (decl)
      && ! TREE_PUBLIC (decl)
      && (warn_unused_function
	  || snode->referred_to_p (/*include_self=*/false)))
    {
      if (snode->referred_to_p (/*include_self=*/false))
	pedwarn (input_location, 0, "%q+F used but never defined", decl);
      else
	warning (OPT_Wunused_function, "%q+F declared %<static%> but never defined", decl);
      /* This symbol is effectively an "extern" declaration now.  */
      TREE_PUBLIC (decl) = 1;
    }

  /* Warn about static fns or vars defined but not used.  */
  if (((warn_unused_function && TREE_CODE (decl) == FUNCTION_DECL)
       || (((warn_unused_variable && ! TREE_READONLY (decl))
	    || (warn_unused_const_variable > 0 && TREE_READONLY (decl)
		&& (warn_unused_const_variable == 2
		    || (main_input_filename != NULL
			&& (decl_file = DECL_SOURCE_FILE (decl)) != NULL
			&& filename_cmp (main_input_filename,
					 decl_file) == 0))))
	   && VAR_P (decl)))
      && ! DECL_IN_SYSTEM_HEADER (decl)
      && ! snode->referred_to_p (/*include_self=*/false)
      /* This TREE_USED check is needed in addition to referred_to_p
	 above, because the `__unused__' attribute is not being
	 considered for referred_to_p.  */
      && ! TREE_USED (decl)
      /* The TREE_USED bit for file-scope decls is kept in the identifier,
	 to handle multiple external decls in different scopes.  */
      && ! (DECL_NAME (decl) && TREE_USED (DECL_NAME (decl)))
      && ! DECL_EXTERNAL (decl)
      && ! DECL_ARTIFICIAL (decl)
      && ! DECL_ABSTRACT_ORIGIN (decl)
      && ! TREE_PUBLIC (decl)
      /* A volatile variable might be used in some non-obvious way.  */
      && (! VAR_P (decl) || ! TREE_THIS_VOLATILE (decl))
      /* Global register variables must be declared to reserve them.  */
      && ! (VAR_P (decl) && DECL_REGISTER (decl))
      /* Global ctors and dtors are called by the runtime.  */
      && (TREE_CODE (decl) != FUNCTION_DECL
	  || (!DECL_STATIC_CONSTRUCTOR (decl)
	      && !DECL_STATIC_DESTRUCTOR (decl)))
      /* Otherwise, ask the language.  */
      && lang_hooks.decls.warn_unused_global (decl))
    warning_at (DECL_SOURCE_LOCATION (decl),
		(TREE_CODE (decl) == FUNCTION_DECL)
		? OPT_Wunused_function
		: (TREE_READONLY (decl)
		   ? OPT_Wunused_const_variable_
		   : OPT_Wunused_variable),
		"%qD defined but not used", decl);
}

/* Discover all functions and variables that are trivially needed, analyze
   them as well as all functions and variables referred by them  */
static cgraph_node *first_analyzed;
static varpool_node *first_analyzed_var;

/* FIRST_TIME is set to TRUE for the first time we are called for a
   translation unit from finalize_compilation_unit() or false
   otherwise.  */

static void
analyze_functions (bool first_time)
{
  /* Keep track of already processed nodes when called multiple times for
     intermodule optimization.  */
  cgraph_node *first_handled = first_analyzed;
  varpool_node *first_handled_var = first_analyzed_var;
  hash_set<void *> reachable_call_targets;

  symtab_node *node;
  symtab_node *next;
  int i;
  ipa_ref *ref;
  bool changed = true;
  location_t saved_loc = input_location;

  bitmap_obstack_initialize (NULL);
  symtab->state = CONSTRUCTION;
  input_location = UNKNOWN_LOCATION;

  /* Ugly, but the fixup can not happen at a time same body alias is created;
     C++ FE is confused about the COMDAT groups being right.  */
  if (symtab->cpp_implicit_aliases_done)
    FOR_EACH_SYMBOL (node)
      if (node->cpp_implicit_alias)
	  node->fixup_same_cpp_alias_visibility (node->get_alias_target ());
  build_type_inheritance_graph ();

  /* Analysis adds static variables that in turn adds references to new functions.
     So we need to iterate the process until it stabilize.  */
  while (changed)
    {
      changed = false;
      process_function_and_variable_attributes (first_analyzed,
						first_analyzed_var);

      /* First identify the trivially needed symbols.  */
      for (node = symtab->first_symbol ();
	   node != first_analyzed
	   && node != first_analyzed_var; node = node->next)
	{
	  /* Convert COMDAT group designators to IDENTIFIER_NODEs.  */
	  node->get_comdat_group_id ();
	  if (node->needed_p ())
	    {
	      enqueue_node (node);
	      if (!changed && symtab->dump_file)
		fprintf (symtab->dump_file, "Trivially needed symbols:");
	      changed = true;
	      if (symtab->dump_file)
		fprintf (symtab->dump_file, " %s", node->asm_name ());
	      if (!changed && symtab->dump_file)
		fprintf (symtab->dump_file, "\n");
	    }
	  if (node == first_analyzed
	      || node == first_analyzed_var)
	    break;
	}
      symtab->process_new_functions ();
      first_analyzed_var = symtab->first_variable ();
      first_analyzed = symtab->first_function ();

      if (changed && symtab->dump_file)
	fprintf (symtab->dump_file, "\n");

      /* Lower representation, build callgraph edges and references for all trivially
         needed symbols and all symbols referred by them.  */
      while (queued_nodes != &symtab_terminator)
	{
	  changed = true;
	  node = queued_nodes;
	  queued_nodes = (symtab_node *)queued_nodes->aux;
	  cgraph_node *cnode = dyn_cast <cgraph_node *> (node);
	  if (cnode && cnode->definition)
	    {
	      cgraph_edge *edge;
	      tree decl = cnode->decl;

	      /* ??? It is possible to create extern inline function
	      and later using weak alias attribute to kill its body.
	      See gcc.c-torture/compile/20011119-1.c  */
	      if (!DECL_STRUCT_FUNCTION (decl)
		  && !cnode->alias
		  && !cnode->thunk.thunk_p
		  && !cnode->dispatcher_function)
		{
		  cnode->reset ();
		  cnode->local.redefined_extern_inline = true;
		  continue;
		}

	      if (!cnode->analyzed)
		cnode->analyze ();

	      for (edge = cnode->callees; edge; edge = edge->next_callee)
		if (edge->callee->definition
		    && (!DECL_EXTERNAL (edge->callee->decl)
			/* When not optimizing, do not try to analyze extern
			   inline functions.  Doing so is pointless.  */
			|| opt_for_fn (edge->callee->decl, optimize)
			/* Weakrefs needs to be preserved.  */
			|| edge->callee->alias
			/* always_inline functions are inlined aven at -O0.  */
		        || lookup_attribute
				 ("always_inline",
			          DECL_ATTRIBUTES (edge->callee->decl))
			/* Multiversioned functions needs the dispatcher to
			   be produced locally even for extern functions.  */
			|| edge->callee->function_version ()))
		   enqueue_node (edge->callee);
	      if (opt_for_fn (cnode->decl, optimize)
		  && opt_for_fn (cnode->decl, flag_devirtualize))
		{
		  cgraph_edge *next;

		  for (edge = cnode->indirect_calls; edge; edge = next)
		    {
		      next = edge->next_callee;
		      if (edge->indirect_info->polymorphic)
			walk_polymorphic_call_targets (&reachable_call_targets,
						       edge);
		    }
		}

	      /* If decl is a clone of an abstract function,
		 mark that abstract function so that we don't release its body.
		 The DECL_INITIAL() of that abstract function declaration
		 will be later needed to output debug info.  */
	      if (DECL_ABSTRACT_ORIGIN (decl))
		{
		  cgraph_node *origin_node
		    = cgraph_node::get_create (DECL_ABSTRACT_ORIGIN (decl));
		  origin_node->used_as_abstract_origin = true;
		}
	      /* Preserve a functions function context node.  It will
		 later be needed to output debug info.  */
	      if (tree fn = decl_function_context (decl))
		{
		  cgraph_node *origin_node = cgraph_node::get_create (fn);
		  enqueue_node (origin_node);
		}
	    }
	  else
	    {
	      varpool_node *vnode = dyn_cast <varpool_node *> (node);
	      if (vnode && vnode->definition && !vnode->analyzed)
		vnode->analyze ();
	    }

	  if (node->same_comdat_group)
	    {
	      symtab_node *next;
	      for (next = node->same_comdat_group;
		   next != node;
		   next = next->same_comdat_group)
		if (!next->comdat_local_p ())
		  enqueue_node (next);
	    }
	  for (i = 0; node->iterate_reference (i, ref); i++)
	    if (ref->referred->definition
		&& (!DECL_EXTERNAL (ref->referred->decl)
		    || ((TREE_CODE (ref->referred->decl) != FUNCTION_DECL
			 && optimize)
			|| (TREE_CODE (ref->referred->decl) == FUNCTION_DECL
			    && opt_for_fn (ref->referred->decl, optimize))
		    || node->alias
		    || ref->referred->alias)))
	      enqueue_node (ref->referred);
	  symtab->process_new_functions ();
	}
    }
  update_type_inheritance_graph ();

  /* Collect entry points to the unit.  */
  if (symtab->dump_file)
    {
      fprintf (symtab->dump_file, "\n\nInitial ");
      symtab->dump (symtab->dump_file);
    }

  if (first_time)
    {
      symtab_node *snode;
      FOR_EACH_SYMBOL (snode)
	check_global_declaration (snode);
    }

  if (symtab->dump_file)
    fprintf (symtab->dump_file, "\nRemoving unused symbols:");

  for (node = symtab->first_symbol ();
       node != first_handled
       && node != first_handled_var; node = next)
    {
      next = node->next;
      if (!node->aux && !node->referred_to_p ())
	{
	  if (symtab->dump_file)
	    fprintf (symtab->dump_file, " %s", node->name ());

	  /* See if the debugger can use anything before the DECL
	     passes away.  Perhaps it can notice a DECL that is now a
	     constant and can tag the early DIE with an appropriate
	     attribute.

	     Otherwise, this is the last chance the debug_hooks have
	     at looking at optimized away DECLs, since
	     late_global_decl will subsequently be called from the
	     contents of the now pruned symbol table.  */
	  if (VAR_P (node->decl)
	      && !decl_function_context (node->decl))
	    {
	      /* We are reclaiming totally unreachable code and variables
	         so they effectively appear as readonly.  Show that to
		 the debug machinery.  */
	      TREE_READONLY (node->decl) = 1;
	      node->definition = false;
	      (*debug_hooks->late_global_decl) (node->decl);
	    }

	  node->remove ();
	  continue;
	}
      if (cgraph_node *cnode = dyn_cast <cgraph_node *> (node))
	{
	  tree decl = node->decl;

	  if (cnode->definition && !gimple_has_body_p (decl)
	      && !cnode->alias
	      && !cnode->thunk.thunk_p)
	    cnode->reset ();

	  gcc_assert (!cnode->definition || cnode->thunk.thunk_p
		      || cnode->alias
		      || gimple_has_body_p (decl)
		      || cnode->native_rtl_p ());
	  gcc_assert (cnode->analyzed == cnode->definition);
	}
      node->aux = NULL;
    }
  for (;node; node = node->next)
    node->aux = NULL;
  first_analyzed = symtab->first_function ();
  first_analyzed_var = symtab->first_variable ();
  if (symtab->dump_file)
    {
      fprintf (symtab->dump_file, "\n\nReclaimed ");
      symtab->dump (symtab->dump_file);
    }
  bitmap_obstack_release (NULL);
  ggc_collect ();
  /* Initialize assembler name hash, in particular we want to trigger C++
     mangling and same body alias creation before we free DECL_ARGUMENTS
     used by it.  */
  if (!seen_error ())
    symtab->symtab_initialize_asm_name_hash ();

  input_location = saved_loc;
}

/* Check declaration of the type of ALIAS for compatibility with its TARGET
   (which may be an ifunc resolver) and issue a diagnostic when they are
   not compatible according to language rules (plus a C++ extension for
   non-static member functions).  */

static void
maybe_diag_incompatible_alias (tree alias, tree target)
{
  tree altype = TREE_TYPE (alias);
  tree targtype = TREE_TYPE (target);

  bool ifunc = lookup_attribute ("ifunc", DECL_ATTRIBUTES (alias));
  tree funcptr = altype;

  if (ifunc)
    {
      /* Handle attribute ifunc first.  */
      if (TREE_CODE (altype) == METHOD_TYPE)
	{
	  /* Set FUNCPTR to the type of the alias target.  If the type
	     is a non-static member function of class C, construct a type
	     of an ordinary function taking C* as the first argument,
	     followed by the member function argument list, and use it
	     instead to check for incompatibility.  This conversion is
	     not defined by the language but an extension provided by
	     G++.  */

	  tree rettype = TREE_TYPE (altype);
	  tree args = TYPE_ARG_TYPES (altype);
	  altype = build_function_type (rettype, args);
	  funcptr = altype;
	}

      targtype = TREE_TYPE (targtype);

      if (POINTER_TYPE_P (targtype))
	{
	  targtype = TREE_TYPE (targtype);

	  /* Only issue Wattribute-alias for conversions to void* with
	     -Wextra.  */
	  if (VOID_TYPE_P (targtype) && !extra_warnings)
	    return;

	  /* Proceed to handle incompatible ifunc resolvers below.  */
	}
      else
	{
	  funcptr = build_pointer_type (funcptr);

	  error_at (DECL_SOURCE_LOCATION (target),
		    "%<ifunc%> resolver for %qD must return %qT",
		 alias, funcptr);
	  inform (DECL_SOURCE_LOCATION (alias),
		  "resolver indirect function declared here");
	  return;
	}
    }

  if ((!FUNC_OR_METHOD_TYPE_P (targtype)
       || (prototype_p (altype)
	   && prototype_p (targtype)
	   && !types_compatible_p (altype, targtype))))
    {
      /* Warn for incompatibilities.  Avoid warning for functions
	 without a prototype to make it possible to declare aliases
	 without knowing the exact type, as libstdc++ does.  */
      if (ifunc)
	{
	  funcptr = build_pointer_type (funcptr);

	  if (warning_at (DECL_SOURCE_LOCATION (target),
			  OPT_Wattribute_alias,
			  "%<ifunc%> resolver for %qD should return %qT",
			  alias, funcptr))
	    inform (DECL_SOURCE_LOCATION (alias),
		    "resolver indirect function declared here");
	}
      else if (warning_at (DECL_SOURCE_LOCATION (alias),
			   OPT_Wattribute_alias,
			   "%qD alias between functions of incompatible "
			   "types %qT and %qT", alias, altype, targtype))
	inform (DECL_SOURCE_LOCATION (target),
		"aliased declaration here");
    }
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
      symtab_node *target_node = symtab_node::get_for_asmname (p->target);

      /* Weakrefs with target not defined in current unit are easy to handle:
	 they behave just as external variables except we need to note the
	 alias flag to later output the weakref pseudo op into asm file.  */
      if (!target_node
	  && lookup_attribute ("weakref", DECL_ATTRIBUTES (p->decl)) != NULL)
	{
	  symtab_node *node = symtab_node::get (p->decl);
	  if (node)
	    {
	      node->alias_target = p->target;
	      node->weakref = true;
	      node->alias = true;
	      node->transparent_alias = true;
	    }
	  alias_pairs->unordered_remove (i);
	  continue;
	}
      else if (!target_node)
	{
	  error ("%q+D aliased to undefined symbol %qE", p->decl, p->target);
	  symtab_node *node = symtab_node::get (p->decl);
	  if (node)
	    node->alias = false;
	  alias_pairs->unordered_remove (i);
	  continue;
	}

      if (DECL_EXTERNAL (target_node->decl)
	  /* We use local aliases for C++ thunks to force the tailcall
	     to bind locally.  This is a hack - to keep it working do
	     the following (which is not strictly correct).  */
	  && (TREE_CODE (target_node->decl) != FUNCTION_DECL
	      || ! DECL_VIRTUAL_P (target_node->decl))
	  && ! lookup_attribute ("weakref", DECL_ATTRIBUTES (p->decl)))
	{
	  error ("%q+D aliased to external symbol %qE",
		 p->decl, p->target);
	}

      if (TREE_CODE (p->decl) == FUNCTION_DECL
          && target_node && is_a <cgraph_node *> (target_node))
	{
	  maybe_diag_incompatible_alias (p->decl, target_node->decl);

	  cgraph_node *src_node = cgraph_node::get (p->decl);
	  if (src_node && src_node->definition)
	    src_node->reset ();
	  cgraph_node::create_alias (p->decl, target_node->decl);
	  alias_pairs->unordered_remove (i);
	}
      else if (VAR_P (p->decl)
	       && target_node && is_a <varpool_node *> (target_node))
	{
	  varpool_node::create_alias (p->decl, target_node->decl);
	  alias_pairs->unordered_remove (i);
	}
      else
	{
	  error ("%q+D alias between function and variable is not supported",
		 p->decl);
	  inform (DECL_SOURCE_LOCATION (target_node->decl),
		  "aliased declaration here");

	  alias_pairs->unordered_remove (i);
	}
    }
  vec_free (alias_pairs);
}


/* Figure out what functions we want to assemble.  */

static void
mark_functions_to_output (void)
{
  bool check_same_comdat_groups = false;
  cgraph_node *node;

  if (flag_checking)
    FOR_EACH_FUNCTION (node)
      gcc_assert (!node->process);

  FOR_EACH_FUNCTION (node)
    {
      tree decl = node->decl;

      gcc_assert (!node->process || node->same_comdat_group);
      if (node->process)
	continue;

      /* We need to output all local functions that are used and not
	 always inlined, as well as those that are reachable from
	 outside the current compilation unit.  */
      if (node->analyzed
	  && !node->thunk.thunk_p
	  && !node->alias
	  && !node->global.inlined_to
	  && !TREE_ASM_WRITTEN (decl)
	  && !DECL_EXTERNAL (decl))
	{
	  node->process = 1;
	  if (node->same_comdat_group)
	    {
	      cgraph_node *next;
	      for (next = dyn_cast<cgraph_node *> (node->same_comdat_group);
		   next != node;
		   next = dyn_cast<cgraph_node *> (next->same_comdat_group))
		if (!next->thunk.thunk_p && !next->alias
		    && !next->comdat_local_p ())
		  next->process = 1;
	    }
	}
      else if (node->same_comdat_group)
	{
	  if (flag_checking)
	    check_same_comdat_groups = true;
	}
      else
	{
	  /* We should've reclaimed all functions that are not needed.  */
	  if (flag_checking
	      && !node->global.inlined_to
	      && gimple_has_body_p (decl)
	      /* FIXME: in ltrans unit when offline copy is outside partition but inline copies
		 are inside partition, we can end up not removing the body since we no longer
		 have analyzed node pointing to it.  */
	      && !node->in_other_partition
	      && !node->alias
	      && !node->clones
	      && !DECL_EXTERNAL (decl))
	    {
	      node->debug ();
	      internal_error ("failed to reclaim unneeded function");
	    }
	  gcc_assert (node->global.inlined_to
		      || !gimple_has_body_p (decl)
		      || node->in_other_partition
		      || node->clones
		      || DECL_ARTIFICIAL (decl)
		      || DECL_EXTERNAL (decl));

	}

    }
  if (flag_checking && check_same_comdat_groups)
    FOR_EACH_FUNCTION (node)
      if (node->same_comdat_group && !node->process)
	{
	  tree decl = node->decl;
	  if (!node->global.inlined_to
	      && gimple_has_body_p (decl)
	      /* FIXME: in an ltrans unit when the offline copy is outside a
		 partition but inline copies are inside a partition, we can
		 end up not removing the body since we no longer have an
		 analyzed node pointing to it.  */
	      && !node->in_other_partition
	      && !node->clones
	      && !DECL_EXTERNAL (decl))
	    {
	      node->debug ();
	      internal_error ("failed to reclaim unneeded function in same "
			      "comdat group");
	    }
	}
}

/* DECL is FUNCTION_DECL.  Initialize datastructures so DECL is a function
   in lowered gimple form.  IN_SSA is true if the gimple is in SSA.
   
   Set current_function_decl and cfun to newly constructed empty function body.
   return basic block in the function body.  */

basic_block
init_lowered_empty_function (tree decl, bool in_ssa, profile_count count)
{
  basic_block bb;
  edge e;

  current_function_decl = decl;
  allocate_struct_function (decl, false);
  gimple_register_cfg_hooks ();
  init_empty_tree_cfg ();
  init_tree_ssa (cfun);

  if (in_ssa)
    {
      init_ssa_operands (cfun);
      cfun->gimple_df->in_ssa_p = true;
      cfun->curr_properties |= PROP_ssa;
    }

  DECL_INITIAL (decl) = make_node (BLOCK);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (decl)) = decl;

  DECL_SAVED_TREE (decl) = error_mark_node;
  cfun->curr_properties |= (PROP_gimple_lcf | PROP_gimple_leh | PROP_gimple_any
			    | PROP_cfg | PROP_loops);

  set_loops_for_fn (cfun, ggc_cleared_alloc<loops> ());
  init_loops_structure (cfun, loops_for_fn (cfun), 1);
  loops_for_fn (cfun)->state |= LOOPS_MAY_HAVE_MULTIPLE_LATCHES;

  /* Create BB for body of the function and connect it properly.  */
  ENTRY_BLOCK_PTR_FOR_FN (cfun)->count = count;
  ENTRY_BLOCK_PTR_FOR_FN (cfun)->frequency = BB_FREQ_MAX;
  EXIT_BLOCK_PTR_FOR_FN (cfun)->count = count;
  EXIT_BLOCK_PTR_FOR_FN (cfun)->frequency = BB_FREQ_MAX;
  bb = create_basic_block (NULL, ENTRY_BLOCK_PTR_FOR_FN (cfun));
  bb->count = count;
  bb->frequency = BB_FREQ_MAX;
  e = make_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun), bb, EDGE_FALLTHRU);
  e->count = count;
  e->probability = profile_probability::always ();
  e = make_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun), 0);
  e->count = count;
  e->probability = profile_probability::always ();
  add_bb_to_loop (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun)->loop_father);

  return bb;
}

/* Adjust PTR by the constant FIXED_OFFSET, and by the vtable
   offset indicated by VIRTUAL_OFFSET, if that is
   non-null. THIS_ADJUSTING is nonzero for a this adjusting thunk and
   zero for a result adjusting thunk.  */

tree
thunk_adjust (gimple_stmt_iterator * bsi,
	      tree ptr, bool this_adjusting,
	      HOST_WIDE_INT fixed_offset, tree virtual_offset)
{
  gassign *stmt;
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

      if (VAR_P (ptr))
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

/* Expand thunk NODE to gimple if possible.
   When FORCE_GIMPLE_THUNK is true, gimple thunk is created and
   no assembler is produced.
   When OUTPUT_ASM_THUNK is true, also produce assembler for
   thunks that are not lowered.  */

bool
cgraph_node::expand_thunk (bool output_asm_thunks, bool force_gimple_thunk)
{
  bool this_adjusting = thunk.this_adjusting;
  HOST_WIDE_INT fixed_offset = thunk.fixed_offset;
  HOST_WIDE_INT virtual_value = thunk.virtual_value;
  tree virtual_offset = NULL;
  tree alias = callees->callee->decl;
  tree thunk_fndecl = decl;
  tree a;

  /* Instrumentation thunk is the same function with
     a different signature.  Never need to expand it.  */
  if (thunk.add_pointer_bounds_args)
    return false;

  if (!force_gimple_thunk && this_adjusting
      && targetm.asm_out.can_output_mi_thunk (thunk_fndecl, fixed_offset,
					      virtual_value, alias))
    {
      const char *fnname;
      tree fn_block;
      tree restype = TREE_TYPE (TREE_TYPE (thunk_fndecl));

      if (!output_asm_thunks)
	{
	  analyzed = true;
	  return false;
	}

      if (in_lto_p)
	get_untransformed_body ();
      a = DECL_ARGUMENTS (thunk_fndecl);
      
      current_function_decl = thunk_fndecl;

      /* Ensure thunks are emitted in their correct sections.  */
      resolve_unique_section (thunk_fndecl, 0,
			      flag_function_sections);

      DECL_RESULT (thunk_fndecl)
	= build_decl (DECL_SOURCE_LOCATION (thunk_fndecl),
		      RESULT_DECL, 0, restype);
      DECL_CONTEXT (DECL_RESULT (thunk_fndecl)) = thunk_fndecl;
      fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk_fndecl));

      /* The back end expects DECL_INITIAL to contain a BLOCK, so we
	 create one.  */
      fn_block = make_node (BLOCK);
      BLOCK_VARS (fn_block) = a;
      DECL_INITIAL (thunk_fndecl) = fn_block;
      BLOCK_SUPERCONTEXT (fn_block) = thunk_fndecl;
      allocate_struct_function (thunk_fndecl, false);
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
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
      thunk.thunk_p = false;
      analyzed = false;
    }
  else if (stdarg_p (TREE_TYPE (thunk_fndecl)))
    {
      error ("generic thunk code fails for method %qD which uses %<...%>",
	     thunk_fndecl);
      TREE_ASM_WRITTEN (thunk_fndecl) = 1;
      analyzed = true;
      return false;
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
      tree resbnd = NULL;

      gcall *call;
      greturn *ret;
      bool alias_is_noreturn = TREE_THIS_VOLATILE (alias);

      /* We may be called from expand_thunk that releses body except for
	 DECL_ARGUMENTS.  In this case force_gimple_thunk is true.  */
      if (in_lto_p && !force_gimple_thunk)
	get_untransformed_body ();
      a = DECL_ARGUMENTS (thunk_fndecl);

      current_function_decl = thunk_fndecl;

      /* Ensure thunks are emitted in their correct sections.  */
      resolve_unique_section (thunk_fndecl, 0,
			      flag_function_sections);

      DECL_IGNORED_P (thunk_fndecl) = 1;
      bitmap_obstack_initialize (NULL);

      if (thunk.virtual_offset_p)
        virtual_offset = size_int (virtual_value);

      /* Build the return declaration for the function.  */
      restype = TREE_TYPE (TREE_TYPE (thunk_fndecl));
      if (DECL_RESULT (thunk_fndecl) == NULL_TREE)
	{
	  resdecl = build_decl (input_location, RESULT_DECL, 0, restype);
	  DECL_ARTIFICIAL (resdecl) = 1;
	  DECL_IGNORED_P (resdecl) = 1;
	  DECL_RESULT (thunk_fndecl) = resdecl;
          DECL_CONTEXT (DECL_RESULT (thunk_fndecl)) = thunk_fndecl;
	}
      else
	resdecl = DECL_RESULT (thunk_fndecl);

      bb = then_bb = else_bb = return_bb
	= init_lowered_empty_function (thunk_fndecl, true, count);

      bsi = gsi_start_bb (bb);

      /* Build call to the function being thunked.  */
      if (!VOID_TYPE_P (restype)
	  && (!alias_is_noreturn
	      || TREE_ADDRESSABLE (restype)
	      || TREE_CODE (TYPE_SIZE_UNIT (restype)) != INTEGER_CST))
	{
	  if (DECL_BY_REFERENCE (resdecl))
	    {
	      restmp = gimple_fold_indirect_ref (resdecl);
	      if (!restmp)
		restmp = build2 (MEM_REF,
				 TREE_TYPE (TREE_TYPE (DECL_RESULT (alias))),
				 resdecl,
				 build_int_cst (TREE_TYPE
				   (DECL_RESULT (alias)), 0));
	    }
	  else if (!is_gimple_reg_type (restype))
	    {
	      if (aggregate_value_p (resdecl, TREE_TYPE (thunk_fndecl)))
		{
		  restmp = resdecl;

		  if (VAR_P (restmp))
		    add_local_decl (cfun, restmp);
		  BLOCK_VARS (DECL_INITIAL (current_function_decl)) = restmp;
		}
	      else
		restmp = create_tmp_var (restype, "retval");
	    }
	  else
	    restmp = create_tmp_reg (restype, "retval");
	}

      for (arg = a; arg; arg = DECL_CHAIN (arg))
        nargs++;
      auto_vec<tree> vargs (nargs);
      i = 0;
      arg = a;
      if (this_adjusting)
	{
	  vargs.quick_push (thunk_adjust (&bsi, a, 1, fixed_offset,
					  virtual_offset));
	  arg = DECL_CHAIN (a);
	  i = 1;
	}

      if (nargs)
	for (; i < nargs; i++, arg = DECL_CHAIN (arg))
	  {
	    tree tmp = arg;
	    if (VECTOR_TYPE_P (TREE_TYPE (arg))
		|| TREE_CODE (TREE_TYPE (arg)) == COMPLEX_TYPE)
	      DECL_GIMPLE_REG_P (arg) = 1;

	    if (!is_gimple_val (arg))
	      {
		tmp = create_tmp_reg (TYPE_MAIN_VARIANT
				      (TREE_TYPE (arg)), "arg");
		gimple *stmt = gimple_build_assign (tmp, arg);
		gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
	      }
	    vargs.quick_push (tmp);
	  }
      call = gimple_build_call_vec (build_fold_addr_expr_loc (0, alias), vargs);
      callees->call_stmt = call;
      gimple_call_set_from_thunk (call, true);
      gimple_call_set_with_bounds (call, instrumentation_clone);

      /* Return slot optimization is always possible and in fact requred to
         return values with DECL_BY_REFERENCE.  */
      if (aggregate_value_p (resdecl, TREE_TYPE (thunk_fndecl))
	  && (!is_gimple_reg_type (TREE_TYPE (resdecl))
	      || DECL_BY_REFERENCE (resdecl)))
        gimple_call_set_return_slot_opt (call, true);

      if (restmp)
	{
          gimple_call_set_lhs (call, restmp);
	  gcc_assert (useless_type_conversion_p (TREE_TYPE (restmp),
						 TREE_TYPE (TREE_TYPE (alias))));
	}
      gsi_insert_after (&bsi, call, GSI_NEW_STMT);
      if (!alias_is_noreturn)
	{
	  if (instrumentation_clone
	      && !DECL_BY_REFERENCE (resdecl)
	      && restmp
	      && BOUNDED_P (restmp))
	    {
	      resbnd = chkp_insert_retbnd_call (NULL, restmp, &bsi);
	      create_edge (get_create (gimple_call_fndecl (gsi_stmt (bsi))),
			   as_a <gcall *> (gsi_stmt (bsi)),
			   callees->count, callees->frequency);
	    }

	  if (restmp && !this_adjusting
	      && (fixed_offset || virtual_offset))
	    {
	      tree true_label = NULL_TREE;

	      if (TREE_CODE (TREE_TYPE (restmp)) == POINTER_TYPE)
		{
		  gimple *stmt;
		  edge e;
		  /* If the return type is a pointer, we need to
		     protect against NULL.  We know there will be an
		     adjustment, because that's why we're emitting a
		     thunk.  */
		  then_bb = create_basic_block (NULL, bb);
		  then_bb->count = count - count.apply_scale (1, 16);
		  then_bb->frequency = BB_FREQ_MAX - BB_FREQ_MAX / 16;
		  return_bb = create_basic_block (NULL, then_bb);
		  return_bb->count = count;
		  return_bb->frequency = BB_FREQ_MAX;
		  else_bb = create_basic_block (NULL, else_bb);
		  then_bb->count = count.apply_scale (1, 16);
		  then_bb->frequency = BB_FREQ_MAX / 16;
		  add_bb_to_loop (then_bb, bb->loop_father);
		  add_bb_to_loop (return_bb, bb->loop_father);
		  add_bb_to_loop (else_bb, bb->loop_father);
		  remove_edge (single_succ_edge (bb));
		  true_label = gimple_block_label (then_bb);
		  stmt = gimple_build_cond (NE_EXPR, restmp,
					    build_zero_cst (TREE_TYPE (restmp)),
					    NULL_TREE, NULL_TREE);
		  gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
		  e = make_edge (bb, then_bb, EDGE_TRUE_VALUE);
		  e->probability = profile_probability::guessed_always ()
					.apply_scale (1, 16);
		  e->count = count - count.apply_scale (1, 16);
		  e = make_edge (bb, else_bb, EDGE_FALSE_VALUE);
		  e->probability = profile_probability::guessed_always ()
					.apply_scale (1, 16);
		  e->count = count.apply_scale (1, 16);
		  make_single_succ_edge (return_bb,
					 EXIT_BLOCK_PTR_FOR_FN (cfun), 0);
		  make_single_succ_edge (then_bb, return_bb, EDGE_FALLTHRU);
		  e = make_edge (else_bb, return_bb, EDGE_FALLTHRU);
		  e->probability = profile_probability::always ();
		  e->count = count.apply_scale (1, 16);
		  bsi = gsi_last_bb (then_bb);
		}

	      restmp = thunk_adjust (&bsi, restmp, /*this_adjusting=*/0,
				     fixed_offset, virtual_offset);
	      if (true_label)
		{
		  gimple *stmt;
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
	  if (!DECL_BY_REFERENCE (resdecl))
	    ret = gimple_build_return (restmp);
	  else
	    ret = gimple_build_return (resdecl);
	  gimple_return_set_retbnd (ret, resbnd);

	  gsi_insert_after (&bsi, ret, GSI_NEW_STMT);
	}
      else
	{
	  gimple_call_set_tail (call, true);
	  remove_edge (single_succ_edge (bb));
	}

      cfun->gimple_df->in_ssa_p = true;
      profile_status_for_fn (cfun)
        = count.initialized_p () ? PROFILE_READ : PROFILE_GUESSED;
      /* FIXME: C++ FE should stop setting TREE_ASM_WRITTEN on thunks.  */
      TREE_ASM_WRITTEN (thunk_fndecl) = false;
      delete_unreachable_blocks ();
      update_ssa (TODO_update_ssa);
      checking_verify_flow_info ();
      free_dominance_info (CDI_DOMINATORS);

      /* Since we want to emit the thunk, we explicitly mark its name as
	 referenced.  */
      thunk.thunk_p = false;
      lowered = true;
      bitmap_obstack_release (NULL);
    }
  current_function_decl = NULL;
  set_cfun (NULL);
  return true;
}

/* Assemble thunks and aliases associated to node.  */

void
cgraph_node::assemble_thunks_and_aliases (void)
{
  cgraph_edge *e;
  ipa_ref *ref;

  for (e = callers; e;)
    if (e->caller->thunk.thunk_p
	&& !e->caller->global.inlined_to
	&& !e->caller->thunk.add_pointer_bounds_args)
      {
	cgraph_node *thunk = e->caller;

	e = e->next_caller;
	thunk->expand_thunk (true, false);
	thunk->assemble_thunks_and_aliases ();
      }
    else
      e = e->next_caller;

  FOR_EACH_ALIAS (this, ref)
    {
      cgraph_node *alias = dyn_cast <cgraph_node *> (ref->referring);
      if (!alias->transparent_alias)
	{
	  bool saved_written = TREE_ASM_WRITTEN (decl);

	  /* Force assemble_alias to really output the alias this time instead
	     of buffering it in same alias pairs.  */
	  TREE_ASM_WRITTEN (decl) = 1;
	  do_assemble_alias (alias->decl,
			     DECL_ASSEMBLER_NAME (decl));
	  alias->assemble_thunks_and_aliases ();
	  TREE_ASM_WRITTEN (decl) = saved_written;
	}
    }
}

/* Expand function specified by node.  */

void
cgraph_node::expand (void)
{
  location_t saved_loc;

  /* We ought to not compile any inline clones.  */
  gcc_assert (!global.inlined_to);

  /* __RTL functions are compiled as soon as they are parsed, so don't
     do it again.  */
  if (native_rtl_p ())
    return;

  announce_function (decl);
  process = 0;
  gcc_assert (lowered);
  get_untransformed_body ();

  /* Generate RTL for the body of DECL.  */

  timevar_push (TV_REST_OF_COMPILATION);

  gcc_assert (symtab->global_info_ready);

  /* Initialize the default bitmap obstack.  */
  bitmap_obstack_initialize (NULL);

  /* Initialize the RTL code for the function.  */
  saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (decl);

  gcc_assert (DECL_STRUCT_FUNCTION (decl));
  push_cfun (DECL_STRUCT_FUNCTION (decl));
  init_function_start (decl);

  gimple_register_cfg_hooks ();

  bitmap_obstack_initialize (&reg_obstack); /* FIXME, only at RTL generation*/

  execute_all_ipa_transforms ();

  /* Perform all tree transforms and optimizations.  */

  /* Signal the start of passes.  */
  invoke_plugin_callbacks (PLUGIN_ALL_PASSES_START, NULL);

  execute_pass_list (cfun, g->get_passes ()->all_passes);

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
      && !cgraph_node::get (decl)->origin)
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
  if (cfun)
    pop_cfun ();

  /* It would make a lot more sense to output thunks before function body to get more
     forward and lest backwarding jumps.  This however would need solving problem
     with comdats. See PR48668.  Also aliases must come after function itself to
     make one pass assemblers, like one on AIX, happy.  See PR 50689.
     FIXME: Perhaps thunks should be move before function IFF they are not in comdat
     groups.  */
  assemble_thunks_and_aliases ();
  release_body ();
  /* Eliminate all call edges.  This is important so the GIMPLE_CALL no longer
     points to the dead function body.  */
  remove_callees ();
  remove_all_references ();
}

/* Node comparer that is responsible for the order that corresponds
   to time when a function was launched for the first time.  */

static int
node_cmp (const void *pa, const void *pb)
{
  const cgraph_node *a = *(const cgraph_node * const *) pa;
  const cgraph_node *b = *(const cgraph_node * const *) pb;

  /* Functions with time profile must be before these without profile.  */
  if (!a->tp_first_run || !b->tp_first_run)
    return a->tp_first_run - b->tp_first_run;

  return a->tp_first_run != b->tp_first_run
	 ? b->tp_first_run - a->tp_first_run
	 : b->order - a->order;
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
  cgraph_node *node;
  cgraph_node **order = XCNEWVEC (cgraph_node *,
					 symtab->cgraph_count);
  unsigned int expanded_func_count = 0, profiled_func_count = 0;
  int order_pos, new_order_pos = 0;
  int i;

  order_pos = ipa_reverse_postorder (order);
  gcc_assert (order_pos == symtab->cgraph_count);

  /* Garbage collector may remove inline clones we eliminate during
     optimization.  So we must be sure to not reference them.  */
  for (i = 0; i < order_pos; i++)
    if (order[i]->process)
      order[new_order_pos++] = order[i];

  if (flag_profile_reorder_functions)
    qsort (order, new_order_pos, sizeof (cgraph_node *), node_cmp);

  for (i = new_order_pos - 1; i >= 0; i--)
    {
      node = order[i];

      if (node->process)
	{
	  expanded_func_count++;
	  if(node->tp_first_run)
	    profiled_func_count++;

	  if (symtab->dump_file)
	    fprintf (symtab->dump_file,
		     "Time profile order in expand_all_functions:%s:%d\n",
		     node->asm_name (), node->tp_first_run);
	  node->process = 0;
	  node->expand ();
	}
    }

    if (dump_file)
      fprintf (dump_file, "Expanded functions with time profile (%s):%u/%u\n",
               main_input_filename, profiled_func_count, expanded_func_count);

  if (symtab->dump_file && flag_profile_reorder_functions)
    fprintf (symtab->dump_file, "Expanded functions with time profile:%u/%u\n",
             profiled_func_count, expanded_func_count);

  symtab->process_new_functions ();
  free_gimplify_stack ();

  free (order);
}

/* This is used to sort the node types by the cgraph order number.  */

enum cgraph_order_sort_kind
{
  ORDER_UNDEFINED = 0,
  ORDER_FUNCTION,
  ORDER_VAR,
  ORDER_VAR_UNDEF,
  ORDER_ASM
};

struct cgraph_order_sort
{
  enum cgraph_order_sort_kind kind;
  union
  {
    cgraph_node *f;
    varpool_node *v;
    asm_node *a;
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
  cgraph_order_sort *nodes;
  int i;
  cgraph_node *pf;
  varpool_node *pv;
  asm_node *pa;
  max = symtab->order;
  nodes = XCNEWVEC (cgraph_order_sort, max);

  FOR_EACH_DEFINED_FUNCTION (pf)
    {
      if (pf->process && !pf->thunk.thunk_p && !pf->alias)
	{
	  if (!pf->no_reorder)
	    continue;
	  i = pf->order;
	  gcc_assert (nodes[i].kind == ORDER_UNDEFINED);
	  nodes[i].kind = ORDER_FUNCTION;
	  nodes[i].u.f = pf;
	}
    }

  /* There is a similar loop in symbol_table::output_variables.
     Please keep them in sync.  */
  FOR_EACH_VARIABLE (pv)
    {
      if (!pv->no_reorder)
	continue;
      if (DECL_HARD_REGISTER (pv->decl)
	  || DECL_HAS_VALUE_EXPR_P (pv->decl))
	continue;
      i = pv->order;
      gcc_assert (nodes[i].kind == ORDER_UNDEFINED);
      nodes[i].kind = pv->definition ? ORDER_VAR : ORDER_VAR_UNDEF;
      nodes[i].u.v = pv;
    }

  for (pa = symtab->first_asm_symbol (); pa; pa = pa->next)
    {
      i = pa->order;
      gcc_assert (nodes[i].kind == ORDER_UNDEFINED);
      nodes[i].kind = ORDER_ASM;
      nodes[i].u.a = pa;
    }

  /* In toplevel reorder mode we output all statics; mark them as needed.  */

  for (i = 0; i < max; ++i)
    if (nodes[i].kind == ORDER_VAR)
      nodes[i].u.v->finalize_named_section_flags ();

  for (i = 0; i < max; ++i)
    {
      switch (nodes[i].kind)
	{
	case ORDER_FUNCTION:
	  nodes[i].u.f->process = 0;
	  nodes[i].u.f->expand ();
	  break;

	case ORDER_VAR:
	  nodes[i].u.v->assemble_decl ();
	  break;

	case ORDER_VAR_UNDEF:
	  assemble_undefined_decl (nodes[i].u.v->decl);
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

  symtab->clear_asm_symbols ();

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

  /* This extra symtab_remove_unreachable_nodes pass tends to catch some
     devirtualization and other changes where removal iterate.  */
  symtab->remove_unreachable_nodes (symtab->dump_file);

  /* If pass_all_early_optimizations was not scheduled, the state of
     the cgraph will not be properly updated.  Update it now.  */
  if (symtab->state < IPA_SSA)
    symtab->state = IPA_SSA;

  if (!in_lto_p)
    {
      /* Generate coverage variables and constructors.  */
      coverage_finish ();

      /* Process new functions added.  */
      set_cfun (NULL);
      current_function_decl = NULL;
      symtab->process_new_functions ();

      execute_ipa_summary_passes
	((ipa_opt_pass_d *) passes->all_regular_ipa_passes);
    }

  /* Some targets need to handle LTO assembler output specially.  */
  if (flag_generate_lto || flag_generate_offload)
    targetm.asm_out.lto_start ();

  if (!in_lto_p)
    {
      if (g->have_offload)
	{
	  section_name_prefix = OFFLOAD_SECTION_NAME_PREFIX;
	  lto_stream_offload_p = true;
	  ipa_write_summaries ();
	  lto_stream_offload_p = false;
	}
      if (flag_lto)
	{
	  section_name_prefix = LTO_SECTION_NAME_PREFIX;
	  lto_stream_offload_p = false;
	  ipa_write_summaries ();
	}
    }

  if (flag_generate_lto || flag_generate_offload)
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

void
symbol_table::output_weakrefs (void)
{
  symtab_node *node;
  cgraph_node *cnode;
  FOR_EACH_SYMBOL (node)
    if (node->alias
        && !TREE_ASM_WRITTEN (node->decl)
	&& (!(cnode = dyn_cast <cgraph_node *> (node))
	    || !cnode->instrumented_version
	    || !TREE_ASM_WRITTEN (cnode->instrumented_version->decl))
	&& node->weakref)
      {
	tree target;

	/* Weakrefs are special by not requiring target definition in current
	   compilation unit.  It is thus bit hard to work out what we want to
	   alias.
	   When alias target is defined, we need to fetch it from symtab reference,
	   otherwise it is pointed to by alias_target.  */
	if (node->alias_target)
	  target = (DECL_P (node->alias_target)
		    ? DECL_ASSEMBLER_NAME (node->alias_target)
		    : node->alias_target);
	else if (node->analyzed)
	  target = DECL_ASSEMBLER_NAME (node->get_alias_target ()->decl);
	else
	  {
	    gcc_unreachable ();
	    target = get_alias_symbol (node->decl);
	  }
        do_assemble_alias (node->decl, target);
      }
}

/* Perform simple optimizations based on callgraph.  */

void
symbol_table::compile (void)
{
  if (seen_error ())
    return;

  symtab_node::checking_verify_symtab_nodes ();

  timevar_push (TV_CGRAPHOPT);
  if (pre_ipa_mem_report)
    {
      fprintf (stderr, "Memory consumption before IPA\n");
      dump_memory_report (false);
    }
  if (!quiet_flag)
    fprintf (stderr, "Performing interprocedural optimizations\n");
  state = IPA;

  /* If LTO is enabled, initialize the streamer hooks needed by GIMPLE.  */
  if (flag_generate_lto || flag_generate_offload)
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

  global_info_ready = true;
  if (dump_file)
    {
      fprintf (dump_file, "Optimized ");
      symtab->dump (dump_file);
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
  symtab_node::checking_verify_symtab_nodes ();

  bitmap_obstack_initialize (NULL);
  execute_ipa_pass_list (g->get_passes ()->all_late_ipa_passes);
  bitmap_obstack_release (NULL);
  mark_functions_to_output ();

  /* When weakref support is missing, we automatically translate all
     references to NODE to references to its ultimate alias target.
     The renaming mechanizm uses flag IDENTIFIER_TRANSPARENT_ALIAS and
     TREE_CHAIN.

     Set up this mapping before we output any assembler but once we are sure
     that all symbol renaming is done.

     FIXME: All this uglyness can go away if we just do renaming at gimple
     level by physically rewritting the IL.  At the moment we can only redirect
     calls, so we need infrastructure for renaming references as well.  */
#ifndef ASM_OUTPUT_WEAKREF
  symtab_node *node;

  FOR_EACH_SYMBOL (node)
    if (node->alias
	&& lookup_attribute ("weakref", DECL_ATTRIBUTES (node->decl)))
      {
	IDENTIFIER_TRANSPARENT_ALIAS
	   (DECL_ASSEMBLER_NAME (node->decl)) = 1;
	TREE_CHAIN (DECL_ASSEMBLER_NAME (node->decl))
	   = (node->alias_target ? node->alias_target
	      : DECL_ASSEMBLER_NAME (node->get_alias_target ()->decl));
      }
#endif

  state = EXPANSION;

  /* Output first asm statements and anything ordered. The process
     flag is cleared for these nodes, so we skip them later.  */
  output_in_order ();
  expand_all_functions ();
  output_variables ();

  process_new_functions ();
  state = FINISHED;
  output_weakrefs ();

  if (dump_file)
    {
      fprintf (dump_file, "\nFinal ");
      symtab->dump (dump_file);
    }
  if (!flag_checking)
    return;
  symtab_node::verify_symtab_nodes ();
  /* Double check that all inline clones are gone and that all
     function bodies have been released from memory.  */
  if (!seen_error ())
    {
      cgraph_node *node;
      bool error_found = false;

      FOR_EACH_DEFINED_FUNCTION (node)
	if (node->global.inlined_to
	    || gimple_has_body_p (node->decl))
	  {
	    error_found = true;
	    node->debug ();
	  }
      if (error_found)
	internal_error ("nodes with unreleased memory found");
    }
}


/* Analyze the whole compilation unit once it is parsed completely.  */

void
symbol_table::finalize_compilation_unit (void)
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
  analyze_functions (/*first_time=*/true);

  /* Mark alias targets necessary and emit diagnostics.  */
  handle_alias_pairs ();

  /* Gimplify and lower thunks.  */
  analyze_functions (/*first_time=*/false);

  /* Offloading requires LTO infrastructure.  */
  if (!in_lto_p && g->have_offload)
    flag_generate_offload = 1;

  if (!seen_error ())
    {
      /* Emit early debug for reachable functions, and by consequence,
	 locally scoped symbols.  */
      struct cgraph_node *cnode;
      FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (cnode)
	(*debug_hooks->early_global_decl) (cnode->decl);

      /* Clean up anything that needs cleaning up after initial debug
	 generation.  */
      (*debug_hooks->early_finish) (main_input_filename);
    }

  /* Finally drive the pass manager.  */
  compile ();

  timevar_pop (TV_CGRAPH);
}

/* Reset all state within cgraphunit.c so that we can rerun the compiler
   within the same process.  For use by toplev::finalize.  */

void
cgraphunit_c_finalize (void)
{
  gcc_assert (cgraph_new_nodes.length () == 0);
  cgraph_new_nodes.truncate (0);

  vtable_entry_type = NULL;
  queued_nodes = &symtab_terminator;

  first_analyzed = NULL;
  first_analyzed_var = NULL;
}

/* Creates a wrapper from cgraph_node to TARGET node. Thunk is used for this
   kind of wrapper method.  */

void
cgraph_node::create_wrapper (cgraph_node *target)
{
  /* Preserve DECL_RESULT so we get right by reference flag.  */
  tree decl_result = DECL_RESULT (decl);

  /* Remove the function's body but keep arguments to be reused
     for thunk.  */
  release_body (true);
  reset ();

  DECL_UNINLINABLE (decl) = false;
  DECL_RESULT (decl) = decl_result;
  DECL_INITIAL (decl) = NULL;
  allocate_struct_function (decl, false);
  set_cfun (NULL);

  /* Turn alias into thunk and expand it into GIMPLE representation.  */
  definition = true;

  memset (&thunk, 0, sizeof (cgraph_thunk_info));
  thunk.thunk_p = true;
  create_edge (target, NULL, count, CGRAPH_FREQ_BASE);
  callees->can_throw_external = !TREE_NOTHROW (target->decl);

  tree arguments = DECL_ARGUMENTS (decl);

  while (arguments)
    {
      TREE_ADDRESSABLE (arguments) = false;
      arguments = TREE_CHAIN (arguments);
    }

  expand_thunk (false, true);

  /* Inline summary set-up.  */
  analyze ();
  inline_analyze_function (this);
}

#include "gt-cgraphunit.h"
