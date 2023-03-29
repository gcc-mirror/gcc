/* Basic IPA optimizations and utilities.
   Copyright (C) 2003-2023 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "stringpool.h"
#include "cgraph.h"
#include "gimplify.h"
#include "tree-iterator.h"
#include "ipa-utils.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "dbgcnt.h"
#include "debug.h"
#include "stringpool.h"
#include "attribs.h"

/* Return true when NODE has ADDR reference.  */

static bool
has_addr_references_p (struct cgraph_node *node,
		       void *)
{
  int i;
  struct ipa_ref *ref = NULL;

  for (i = 0; node->iterate_referring (i, ref); i++)
    if (ref->use == IPA_REF_ADDR)
      return true;
  return false;
}

/* Return true when NODE can be target of an indirect call.  */

static bool
is_indirect_call_target_p (struct cgraph_node *node, void *)
{
  return node->indirect_call_target;
}

/* Look for all functions inlined to NODE and update their inlined_to pointers
   to INLINED_TO.  */

static void
update_inlined_to_pointer (struct cgraph_node *node, struct cgraph_node *inlined_to)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    if (e->callee->inlined_to)
      {
	e->callee->inlined_to = inlined_to;
	update_inlined_to_pointer (e->callee, inlined_to);
      }
}

/* Add symtab NODE to queue starting at FIRST.

   The queue is linked via AUX pointers and terminated by pointer to 1.
   We enqueue nodes at two occasions: when we find them reachable or when we find
   their bodies needed for further clonning.  In the second case we mark them
   by pointer to 2 after processing so they are re-queue when they become
   reachable.  */

static void
enqueue_node (symtab_node *node, symtab_node **first,
	      hash_set<symtab_node *> *reachable)
{
  /* Node is still in queue; do nothing.  */
  if (node->aux && node->aux != (void *) 2)
    return;
  /* Node was already processed as unreachable, re-enqueue
     only if it became reachable now.  */
  if (node->aux == (void *)2 && !reachable->contains (node))
    return;
  node->aux = *first;
  *first = node;
}

/* Return true if NODE may get inlined later.
   This is used to keep DECL_EXTERNAL function bodies around long enough
   so inliner can proces them.  */

static bool
possible_inline_candidate_p (symtab_node *node)
{
  if (symtab->state >= IPA_SSA_AFTER_INLINING)
    return false;
  cgraph_node *cnode = dyn_cast <cgraph_node *> (node);
  if (!cnode)
    return false;
  if (DECL_UNINLINABLE (cnode->decl))
    return false;
  if (opt_for_fn (cnode->decl, optimize))
    return true;
  if (symtab->state >= IPA_SSA)
    return false;
  return lookup_attribute ("always_inline", DECL_ATTRIBUTES (node->decl));
}

/* Process references.  */

static void
process_references (symtab_node *snode,
		    symtab_node **first,
		    hash_set<symtab_node *> *reachable)
{
  int i;
  struct ipa_ref *ref = NULL;
  for (i = 0; snode->iterate_reference (i, ref); i++)
    {
      symtab_node *node = ref->referred;
      symtab_node *body = node->ultimate_alias_target ();

      if (node->definition && !node->in_other_partition
	  && ((!DECL_EXTERNAL (node->decl) || node->alias)
	      || (possible_inline_candidate_p (node)
		  /* We use variable constructors during late compilation for
		     constant folding.  Keep references alive so partitioning
		     knows about potential references.  */
		  || (VAR_P (node->decl)
		      && (flag_wpa
			  || flag_incremental_link
			 	 == INCREMENTAL_LINK_LTO)
		      && dyn_cast <varpool_node *> (node)
		      	   ->ctor_useable_for_folding_p ()))))
	{
	  /* Be sure that we will not optimize out alias target
	     body.  */
	  if (DECL_EXTERNAL (node->decl)
	      && node->alias
	      && symtab->state < IPA_SSA_AFTER_INLINING)
	    reachable->add (body);
	  reachable->add (node);
	}
      enqueue_node (node, first, reachable);
    }
}

/* EDGE is an polymorphic call.  If BEFORE_INLINING_P is set, mark
   all its potential targets as reachable to permit later inlining if
   devirtualization happens.  After inlining still keep their declarations
   around, so we can devirtualize to a direct call.

   Also try to make trivial devirutalization when no or only one target is
   possible.  */

static void
walk_polymorphic_call_targets (hash_set<void *> *reachable_call_targets,
			       struct cgraph_edge *edge,
			       symtab_node **first,
			       hash_set<symtab_node *> *reachable)
{
  unsigned int i;
  void *cache_token;
  bool final;
  vec <cgraph_node *>targets
    = possible_polymorphic_call_targets
	(edge, &final, &cache_token);

  if (cache_token != NULL && !reachable_call_targets->add (cache_token))
    {
      for (i = 0; i < targets.length (); i++)
	{
	  struct cgraph_node *n = targets[i];

	  /* Do not bother to mark virtual methods in anonymous namespace;
	     either we will find use of virtual table defining it, or it is
	     unused.  */
	  if (TREE_CODE (TREE_TYPE (n->decl)) == METHOD_TYPE
	      && type_in_anonymous_namespace_p
		    (TYPE_METHOD_BASETYPE (TREE_TYPE (n->decl))))
	    continue;

	  n->indirect_call_target = true;
	  symtab_node *body = n->function_symbol ();

	  /* Prior inlining, keep alive bodies of possible targets for
	     devirtualization.  */
	  if (n->definition
	      && (possible_inline_candidate_p (body)
		  && opt_for_fn (body->decl, flag_devirtualize)))
	     {
		/* Be sure that we will not optimize out alias target
		   body.  */
		if (DECL_EXTERNAL (n->decl)
		    && n->alias
		    && symtab->state < IPA_SSA_AFTER_INLINING)
		  reachable->add (body);
	       reachable->add (n);
	     }
	  /* Even after inlining we want to keep the possible targets in the
	     boundary, so late passes can still produce direct call even if
	     the chance for inlining is lost.  */
	  enqueue_node (n, first, reachable);
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
	  cgraph_node *target, *node = edge->caller;
	  if (targets.length () == 1)
	    target = targets[0];
	  else
	    target = cgraph_node::get_create (builtin_decl_unreachable ());

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, edge->call_stmt,
			       "devirtualizing call in %s to %s\n",
			       edge->caller->dump_name (),
			       target->dump_name ());
	    }
	  edge = cgraph_edge::make_direct (edge, target);
	  if (ipa_fn_summaries)
	    ipa_update_overall_fn_summary (node->inlined_to
					   ? node->inlined_to : node);
	  else if (edge->call_stmt)
	    cgraph_edge::redirect_call_stmt_to_callee (edge);
	}
    }
}

/* Perform reachability analysis and reclaim all unreachable nodes.

   The algorithm is basically mark&sweep but with some extra refinements:

   - reachable extern inline functions needs special handling; the bodies needs
     to stay in memory until inlining in hope that they will be inlined.
     After inlining we release their bodies and turn them into unanalyzed
     nodes even when they are reachable.

   - virtual functions are kept in callgraph even if they seem unreachable in
     hope calls to them will be devirtualized. 

     Again we remove them after inlining.  In late optimization some
     devirtualization may happen, but it is not important since we won't inline
     the call. In theory early opts and IPA should work out all important cases.

   - virtual clones needs bodies of their origins for later materialization;
     this means that we want to keep the body even if the origin is unreachable
     otherwise.  To avoid origin from sitting in the callgraph and being
     walked by IPA passes, we turn them into unanalyzed nodes with body
     defined.

     We maintain set of function declaration where body needs to stay in
     body_needed_for_clonning

     Inline clones represent special case: their declaration match the
     declaration of origin and cgraph_remove_node already knows how to
     reshape callgraph and preserve body when offline copy of function or
     inline clone is being removed.

   - C++ virtual tables keyed to other unit are represented as DECL_EXTERNAL
     variables with DECL_INITIAL set.  We finalize these and keep reachable
     ones around for constant folding purposes.  After inlining we however
     stop walking their references to let everything static referenced by them
     to be removed when it is otherwise unreachable.

   We maintain queue of both reachable symbols (i.e. defined symbols that needs
   to stay) and symbols that are in boundary (i.e. external symbols referenced
   by reachable symbols or origins of clones).  The queue is represented
   as linked list by AUX pointer terminated by 1.

   At the end we keep all reachable symbols. For symbols in boundary we always
   turn definition into a declaration, but we may keep function body around
   based on body_needed_for_clonning

   All symbols that enter the queue have AUX pointer non-zero and are in the
   boundary.  Pointer set REACHABLE is used to track reachable symbols.

   Every symbol can be visited twice - once as part of boundary and once
   as real reachable symbol. enqueue_node needs to decide whether the
   node needs to be re-queued for second processing.  For this purpose
   we set AUX pointer of processed symbols in the boundary to constant 2.  */

bool
symbol_table::remove_unreachable_nodes (FILE *file)
{
  symtab_node *first = (symtab_node *) (void *) 1;
  struct cgraph_node *node, *next;
  varpool_node *vnode, *vnext;
  bool changed = false;
  hash_set<symtab_node *> reachable;
  hash_set<tree> body_needed_for_clonning;
  hash_set<void *> reachable_call_targets;

  timevar_push (TV_IPA_UNREACHABLE);
  build_type_inheritance_graph ();
  if (file)
    fprintf (file, "\nReclaiming functions:");
  if (flag_checking)
    {
      FOR_EACH_FUNCTION (node)
	gcc_assert (!node->aux);
      FOR_EACH_VARIABLE (vnode)
	gcc_assert (!vnode->aux);
    }
  /* Mark functions whose bodies are obviously needed.
     This is mostly when they can be referenced externally.  Inline clones
     are special since their declarations are shared with master clone and thus
     cgraph_can_remove_if_no_direct_calls_and_refs_p should not be called on them.  */
  FOR_EACH_FUNCTION (node)
    {
      node->used_as_abstract_origin = false;
      node->indirect_call_target = false;
      if (node->definition
	  && !node->inlined_to
	  && !node->in_other_partition
	  && !node->can_remove_if_no_direct_calls_and_refs_p ())
	{
	  gcc_assert (!node->inlined_to);
	  reachable.add (node);
	  enqueue_node (node, &first, &reachable);
	}
      else
	gcc_assert (!node->aux);
     }

  /* Mark variables that are obviously needed.  */
  FOR_EACH_DEFINED_VARIABLE (vnode)
    if (!vnode->can_remove_if_no_refs_p()
	&& !vnode->in_other_partition)
      {
	reachable.add (vnode);
	enqueue_node (vnode, &first, &reachable);
      }

  /* Perform reachability analysis.  */
  while (first != (symtab_node *) (void *) 1)
    {
      bool in_boundary_p = !reachable.contains (first);
      symtab_node *node = first;

      first = (symtab_node *)first->aux;

      /* If we are processing symbol in boundary, mark its AUX pointer for
	 possible later re-processing in enqueue_node.  */
      if (in_boundary_p)
	{
	  node->aux = (void *)2;
	  if (node->alias && node->analyzed)
	    enqueue_node (node->get_alias_target (), &first, &reachable);
	}
      else
	{
	  if (TREE_CODE (node->decl) == FUNCTION_DECL
	      && DECL_ABSTRACT_ORIGIN (node->decl))
	    {
	      struct cgraph_node *origin_node
	      = cgraph_node::get (DECL_ABSTRACT_ORIGIN (node->decl));
	      if (origin_node && !origin_node->used_as_abstract_origin)
		{
	          origin_node->used_as_abstract_origin = true;
		  gcc_assert (!origin_node->prev_sibling_clone);
		  gcc_assert (!origin_node->next_sibling_clone);
		  for (cgraph_node *n = origin_node->clones; n;
		       n = n->next_sibling_clone)
		    if (n->decl == DECL_ABSTRACT_ORIGIN (node->decl))
		      n->used_as_abstract_origin = true;
		}
	    }
	  /* If any non-external and non-local symbol in a comdat group is
 	     reachable, force all externally visible symbols in the same comdat
	     group to be reachable as well.  Comdat-local symbols
	     can be discarded if all uses were inlined.  */
	  if (node->same_comdat_group
	      && node->externally_visible
	      && !DECL_EXTERNAL (node->decl))
	    {
	      symtab_node *next;
	      for (next = node->same_comdat_group;
		   next != node;
		   next = next->same_comdat_group)
		if (!next->comdat_local_p ()
		    && !DECL_EXTERNAL (next->decl)
		    && !reachable.add (next))
		  enqueue_node (next, &first, &reachable);
	    }
	  /* Mark references as reachable.  */
	  process_references (node, &first, &reachable);
	}

      if (cgraph_node *cnode = dyn_cast <cgraph_node *> (node))
	{
	  /* Mark the callees reachable unless they are direct calls to extern
 	     inline functions we decided to not inline.  */
	  if (!in_boundary_p)
	    {
	      struct cgraph_edge *e;
	      /* Keep alive possible targets for devirtualization.  */
	      if (opt_for_fn (cnode->decl, optimize)
		  && opt_for_fn (cnode->decl, flag_devirtualize))
		{
		  struct cgraph_edge *next;
		  for (e = cnode->indirect_calls; e; e = next)
		    {
		      next = e->next_callee;
		      if (e->indirect_info->polymorphic)
			walk_polymorphic_call_targets (&reachable_call_targets,
						       e, &first, &reachable);
		    }
		}
	      for (e = cnode->callees; e; e = e->next_callee)
		{
	          symtab_node *body = e->callee->function_symbol ();
		  if (e->callee->definition
		      && !e->callee->in_other_partition
		      && (!e->inline_failed
			  || !DECL_EXTERNAL (e->callee->decl)
			  || e->callee->alias
			  || possible_inline_candidate_p (e->callee)))
		    {
		      /* Be sure that we will not optimize out alias target
			 body.  */
		      if (DECL_EXTERNAL (e->callee->decl)
			  && e->callee->alias
			  && symtab->state < IPA_SSA_AFTER_INLINING)
			reachable.add (body);
		      reachable.add (e->callee);
		    }
		  else if (e->callee->declare_variant_alt
			   && !e->callee->in_other_partition)
		    reachable.add (e->callee);
		  enqueue_node (e->callee, &first, &reachable);
		}

	      /* When inline clone exists, mark body to be preserved so when removing
		 offline copy of the function we don't kill it.  */
	      if (cnode->inlined_to)
	        body_needed_for_clonning.add (cnode->decl);

	      /* For non-inline clones, force their origins to the boundary and ensure
		 that body is not removed.  */
	      while (cnode->clone_of)
		{
		  bool noninline = cnode->clone_of->decl != cnode->decl;
		  cnode = cnode->clone_of;
		  if (noninline)
		    {
		      body_needed_for_clonning.add (cnode->decl);
		      enqueue_node (cnode, &first, &reachable);
		    }
		}

	    }
	  else if (cnode->thunk)
	    enqueue_node (cnode->callees->callee, &first, &reachable);

	  /* If any reachable function has simd clones, mark them as
	     reachable as well.  */
	  if (cnode->simd_clones)
	    {
	      cgraph_node *next;
	      for (next = cnode->simd_clones;
		   next;
		   next = next->simdclone->next_clone)
		if (in_boundary_p
		    || !reachable.add (next))
		  enqueue_node (next, &first, &reachable);
	    }
	}
      /* When we see constructor of external variable, keep referred nodes in the
	boundary.  This will also hold initializers of the external vars NODE
	refers to.  */
      varpool_node *vnode = dyn_cast <varpool_node *> (node);
      if (vnode
	  && DECL_EXTERNAL (node->decl)
	  && !vnode->alias
	  && in_boundary_p)
	{
	  struct ipa_ref *ref = NULL;
	  for (int i = 0; node->iterate_reference (i, ref); i++)
	    enqueue_node (ref->referred, &first, &reachable);
	}
    }

  /* Remove unreachable functions.   */
  for (node = first_function (); node; node = next)
    {
      next = next_function (node);

      /* If node is not needed at all, remove it.  */
      if (!node->aux)
	{
	  if (file)
	    fprintf (file, " %s", node->dump_name ());
	  node->remove ();
	  changed = true;
	}
      /* If node is unreachable, remove its body.  */
      else if (!reachable.contains (node))
        {
	  /* We keep definitions of thunks and aliases in the boundary so
	     we can walk to the ultimate alias targets and function symbols
	     reliably.  */
	  if (node->alias || node->thunk)
	    ;
	  else if (!body_needed_for_clonning.contains (node->decl))
	    {
	      /* Make the node a non-clone so that we do not attempt to
		 materialize it later.  */
	      if (node->clone_of)
		node->remove_from_clone_tree ();
	      node->release_body ();
	    }
	  else if (!node->clone_of)
	    gcc_assert (in_lto_p || DECL_RESULT (node->decl));
	  if (node->definition && !node->alias && !node->thunk)
	    {
	      if (file)
		fprintf (file, " %s", node->dump_name ());
	      node->body_removed = true;
	      node->analyzed = false;
	      node->definition = false;
	      node->cpp_implicit_alias = false;
	      node->alias = false;
	      node->transparent_alias = false;
	      node->thunk = false;
	      node->weakref = false;
	      /* After early inlining we drop always_inline attributes on
		 bodies of functions that are still referenced (have their
		 address taken).  */
	      DECL_ATTRIBUTES (node->decl)
		= remove_attribute ("always_inline",
				    DECL_ATTRIBUTES (node->decl));
	      if (!node->in_other_partition)
		node->local = false;
	      node->remove_callees ();
	      node->remove_all_references ();
	      changed = true;
	    }
	}
      else
	gcc_assert (node->clone_of || !node->has_gimple_body_p ()
		    || in_lto_p || DECL_RESULT (node->decl));
    }

  /* Inline clones might be kept around so their materializing allows further
     cloning.  If the function the clone is inlined into is removed, we need
     to turn it into normal cone.  */
  FOR_EACH_FUNCTION (node)
    {
      if (node->inlined_to
	  && !node->callers)
	{
	  gcc_assert (node->clones);
	  node->inlined_to = NULL;
	  update_inlined_to_pointer (node, node);
	}
      node->aux = NULL;
    }

  /* Remove unreachable variables.  */
  if (file)
    fprintf (file, "\nReclaiming variables:");
  for (vnode = first_variable (); vnode; vnode = vnext)
    {
      vnext = next_variable (vnode);
      if (!vnode->aux
	  /* For can_refer_decl_in_current_unit_p we want to track for
	     all external variables if they are defined in other partition
	     or not.  */
	  && (!flag_ltrans || !DECL_EXTERNAL (vnode->decl)))
	{
	  struct ipa_ref *ref = NULL;

	  /* First remove the aliases, so varpool::remove can possibly lookup
	     the constructor and save it for future use.  */
	  while (vnode->iterate_direct_aliases (0, ref))
	    {
	      if (file)
		fprintf (file, " %s", ref->referred->dump_name ());
	      ref->referring->remove ();
	    }
	  if (file)
	    fprintf (file, " %s", vnode->dump_name ());
          vnext = next_variable (vnode);
	  /* Signal removal to the debug machinery.  */
	  if (! flag_wpa || flag_incremental_link == INCREMENTAL_LINK_LTO)
	    {
	      vnode->definition = false;
	      (*debug_hooks->late_global_decl) (vnode->decl);
	    }
	  vnode->remove ();
	  changed = true;
	}
      else if (!reachable.contains (vnode) && !vnode->alias)
        {
	  tree init;
	  if (vnode->definition)
	    {
	      if (file)
		fprintf (file, " %s", vnode->dump_name ());
	      changed = true;
	    }
	  /* Keep body if it may be useful for constant folding.  */
	  if ((flag_wpa || flag_incremental_link == INCREMENTAL_LINK_LTO)
	      || ((init = ctor_for_folding (vnode->decl)) == error_mark_node))
	    vnode->remove_initializer ();
	  else
	    DECL_INITIAL (vnode->decl) = init;
	  vnode->body_removed = true;
	  vnode->definition = false;
	  vnode->analyzed = false;
	  vnode->aux = NULL;

	  vnode->remove_from_same_comdat_group ();

	  vnode->remove_all_references ();
	}
      else
	vnode->aux = NULL;
    }

  /* Now update address_taken flags and try to promote functions to be local.  */
  if (file)
    fprintf (file, "\nClearing address taken flags:");
  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->address_taken
	&& !node->used_from_other_partition)
      {
	if (!node->call_for_symbol_and_aliases
	    (has_addr_references_p, NULL, true))
	  {
	    if (file)
	      fprintf (file, " %s", node->dump_name ());
	    node->address_taken = false;
	    changed = true;
	    if (node->local_p ()
		/* Virtual functions may be kept in cgraph just because
		   of possible later devirtualization.  Do not mark them as
		   local too early so we won't optimize them out before
		   we are done with polymorphic call analysis.  */
		&& (symtab->state >= IPA_SSA_AFTER_INLINING
		    || !node->call_for_symbol_and_aliases
		       (is_indirect_call_target_p, NULL, true)))
	      {
		node->local = true;
		if (file)
		  fprintf (file, " (local)");
	      }
	  }
      }
  if (file)
    fprintf (file, "\n");

  symtab_node::checking_verify_symtab_nodes ();

  /* If we removed something, perhaps profile could be improved.  */
  if (changed && (optimize || in_lto_p) && ipa_call_summaries)
    FOR_EACH_DEFINED_FUNCTION (node)
      ipa_propagate_frequency (node);

  timevar_pop (TV_IPA_UNREACHABLE);
  return changed;
}

/* Process references to VNODE and set flags WRITTEN, ADDRESS_TAKEN, READ
   as needed, also clear EXPLICIT_REFS if the references to given variable
   do not need to be explicit.  */

void
process_references (varpool_node *vnode,
		    bool *written, bool *address_taken,
		    bool *read, bool *explicit_refs)
{
  int i;
  struct ipa_ref *ref;

  if (!vnode->all_refs_explicit_p ()
      || TREE_THIS_VOLATILE (vnode->decl))
    *explicit_refs = false;

  for (i = 0; vnode->iterate_referring (i, ref)
	      && *explicit_refs && (!*written || !*address_taken || !*read); i++)
    switch (ref->use)
      {
      case IPA_REF_ADDR:
	*address_taken = true;
	break;
      case IPA_REF_LOAD:
	*read = true;
	break;
      case IPA_REF_STORE:
	*written = true;
	break;
      case IPA_REF_ALIAS:
	process_references (dyn_cast<varpool_node *> (ref->referring), written,
			    address_taken, read, explicit_refs);
	break;
      }
}

/* Set TREE_READONLY bit.  */

bool
set_readonly_bit (varpool_node *vnode, void *data ATTRIBUTE_UNUSED)
{
  TREE_READONLY (vnode->decl) = true;
  return false;
}

/* Set writeonly bit and clear the initalizer, since it will not be needed.  */

bool
set_writeonly_bit (varpool_node *vnode, void *data)
{
  vnode->writeonly = true;
  if (optimize || in_lto_p)
    {
      DECL_INITIAL (vnode->decl) = NULL;
      if (!vnode->alias)
	{
	  if (vnode->num_references ())
	    *(bool *)data = true;
	  vnode->remove_all_references ();
	}
    }
  return false;
}

/* Clear addressale bit of VNODE.  */

bool
clear_addressable_bit (varpool_node *vnode, void *data ATTRIBUTE_UNUSED)
{
  vnode->address_taken = false;
  TREE_ADDRESSABLE (vnode->decl) = 0;
  return false;
}

/* Discover variables that have no longer address taken, are read-only or
   write-only and update their flags.

   Return true when unreachable symbol removal should be done.

   FIXME: This cannot be done in between gimplify and omp_expand since
   readonly flag plays role on what is shared and what is not.  Currently we do
   this transformation as part of whole program visibility and re-do at
   ipa-reference pass (to take into account clonning), but it would
   make sense to do it before early optimizations.  */

bool
ipa_discover_variable_flags (void)
{
  if (!flag_ipa_reference_addressable)
    return false;

  bool remove_p = false;
  varpool_node *vnode;
  if (dump_file)
    fprintf (dump_file, "Clearing variable flags:");
  FOR_EACH_VARIABLE (vnode)
    if (!vnode->alias
	&& (TREE_ADDRESSABLE (vnode->decl)
	    || !vnode->writeonly
	    || !TREE_READONLY (vnode->decl)))
      {
	bool written = false;
	bool address_taken = false;
	bool read = false;
	bool explicit_refs = true;

	process_references (vnode, &written, &address_taken, &read,
			    &explicit_refs);
	if (!explicit_refs)
	  continue;
	if (!address_taken)
	  {
	    if (TREE_ADDRESSABLE (vnode->decl) && dump_file)
	      fprintf (dump_file, " %s (non-addressable)",
		       vnode->dump_name ());
	    vnode->call_for_symbol_and_aliases (clear_addressable_bit, NULL,
					        true);
	  }
	if (!address_taken && !written
	    /* Making variable in explicit section readonly can cause section
	       type conflict. 
	       See e.g. gcc.c-torture/compile/pr23237.c */
	    && vnode->get_section () == NULL)
	  {
	    if (!TREE_READONLY (vnode->decl) && dump_file)
	      fprintf (dump_file, " %s (read-only)", vnode->dump_name ());
	    vnode->call_for_symbol_and_aliases (set_readonly_bit, NULL, true);
	  }
	if (!vnode->writeonly && !read && !address_taken && written)
	  {
	    if (dump_file)
	      fprintf (dump_file, " %s (write-only)", vnode->dump_name ());
	    vnode->call_for_symbol_and_aliases (set_writeonly_bit, &remove_p, 
					        true);
	  }
      }
  if (dump_file)
    fprintf (dump_file, "\n");
  return remove_p;
}

/* Generate and emit a static constructor or destructor.  WHICH must
   be one of 'I' (for a constructor), 'D' (for a destructor).
   BODY is a STATEMENT_LIST containing GENERIC
   statements.  PRIORITY is the initialization priority for this
   constructor or destructor.

   FINAL specify whether the externally visible name for collect2 should
   be produced. */

static tree
cgraph_build_static_cdtor_1 (char which, tree body, int priority, bool final,
			     tree optimization,
			     tree target)
{
  static int counter = 0;
  char which_buf[16];
  tree decl, name, resdecl;

  /* The priority is encoded in the constructor or destructor name.
     collect2 will sort the names and arrange that they are called at
     program startup.  */
  if (!targetm.have_ctors_dtors && final)
    {
      sprintf (which_buf, "%c_%.5d_%d", which, priority, counter++);
      name = get_file_function_name (which_buf);
    }
  else
    {
      /* Proudce sane name but one not recognizable by collect2, just for the
	 case we fail to inline the function.  */
      sprintf (which_buf, "_sub_%c_%.5d_%d", which, priority, counter++);
      name = get_identifier (which_buf);
    }

  decl = build_decl (input_location, FUNCTION_DECL, name,
		     build_function_type_list (void_type_node, NULL_TREE));
  current_function_decl = decl;

  resdecl = build_decl (input_location,
			RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_RESULT (decl) = resdecl;
  DECL_CONTEXT (resdecl) = decl;

  allocate_struct_function (decl, false);

  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;
  DECL_FUNCTION_SPECIFIC_OPTIMIZATION (decl) = optimization;
  DECL_FUNCTION_SPECIFIC_TARGET (decl) = target;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl) = 1;
  DECL_SAVED_TREE (decl) = body;
  if (!targetm.have_ctors_dtors && final)
    {
      TREE_PUBLIC (decl) = 1;
      DECL_PRESERVE_P (decl) = 1;
    }
  DECL_UNINLINABLE (decl) = 1;

  DECL_INITIAL (decl) = make_node (BLOCK);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (decl)) = decl;
  TREE_USED (DECL_INITIAL (decl)) = 1;

  DECL_SOURCE_LOCATION (decl) = input_location;
  cfun->function_end_locus = input_location;

  switch (which)
    {
    case 'I':
      DECL_STATIC_CONSTRUCTOR (decl) = 1;
      decl_init_priority_insert (decl, priority);
      break;
    case 'D':
      DECL_STATIC_DESTRUCTOR (decl) = 1;
      decl_fini_priority_insert (decl, priority);
      break;
    default:
      gcc_unreachable ();
    }

  gimplify_function_tree (decl);

  cgraph_node::add_new_function (decl, false);

  set_cfun (NULL);
  current_function_decl = NULL;
  return decl;
}

/* Generate and emit a static constructor or destructor.  WHICH must
   be one of 'I' (for a constructor) or 'D' (for a destructor).
   BODY is a STATEMENT_LIST containing GENERIC
   statements.  PRIORITY is the initialization priority for this
   constructor or destructor.  */

void
cgraph_build_static_cdtor (char which, tree body, int priority)
{
  /* FIXME: We should be able to
     gcc_assert (!in_lto_p);
     because at LTO time the global options are not safe to use.
     Unfortunately ASAN finish_file will produce constructors late and they
     may lead to surprises.  */
  cgraph_build_static_cdtor_1 (which, body, priority, false,
			       optimization_default_node,
			       target_option_default_node);
}

/* When target does not have ctors and dtors, we call all constructor
   and destructor by special initialization/destruction function
   recognized by collect2.

   When we are going to build this function, collect all constructors and
   destructors and turn them into normal functions.  */

static void
record_cdtor_fn (struct cgraph_node *node, vec<tree> *ctors, vec<tree> *dtors)
{
  if (DECL_STATIC_CONSTRUCTOR (node->decl))
    ctors->safe_push (node->decl);
  if (DECL_STATIC_DESTRUCTOR (node->decl))
    dtors->safe_push (node->decl);
  node = cgraph_node::get (node->decl);
  DECL_DISREGARD_INLINE_LIMITS (node->decl) = 1;
}

/* Define global constructors/destructor functions for the CDTORS, of
   which they are LEN.  The CDTORS are sorted by initialization
   priority.  If CTOR_P is true, these are constructors; otherwise,
   they are destructors.  */

static void
build_cdtor (bool ctor_p, const vec<tree> &cdtors)
{
  size_t i,j;
  size_t len = cdtors.length ();

  i = 0;
  while (i < len)
    {
      tree body;
      tree fn;
      priority_type priority;

      priority = 0;
      body = NULL_TREE;
      j = i;
      do
	{
	  priority_type p;
	  fn = cdtors[j];
	  p = ctor_p ? DECL_INIT_PRIORITY (fn) : DECL_FINI_PRIORITY (fn);
	  if (j == i)
	    priority = p;
	  else if (p != priority)
	    break;
	  j++;
	}
      while (j < len);

      /* When there is only one cdtor and target supports them, do nothing.  */
      if (j == i + 1
	  && targetm.have_ctors_dtors)
	{
	  i++;
	  continue;
	}
      /* Find the next batch of constructors/destructors with the same
	 initialization priority.  */
      for (;i < j; i++)
	{
	  tree call;
	  fn = cdtors[i];
	  call = build_call_expr (fn, 0);
	  if (ctor_p)
	    DECL_STATIC_CONSTRUCTOR (fn) = 0;
	  else
	    DECL_STATIC_DESTRUCTOR (fn) = 0;
	  /* We do not want to optimize away pure/const calls here.
	     When optimizing, these should be already removed, when not
	     optimizing, we want user to be able to breakpoint in them.  */
	  TREE_SIDE_EFFECTS (call) = 1;
	  append_to_statement_list (call, &body);
	}
      gcc_assert (body != NULL_TREE);
      /* Generate a function to call all the function of like
	 priority.  */
      cgraph_build_static_cdtor_1 (ctor_p ? 'I' : 'D', body, priority, true,
				   DECL_FUNCTION_SPECIFIC_OPTIMIZATION (cdtors[0]),
				   DECL_FUNCTION_SPECIFIC_TARGET (cdtors[0]));
    }
}

/* Helper functions for build_cxa_dtor_registrations ().
   Build a decl for __cxa_atexit ().  */

static tree
build_cxa_atexit_decl ()
{
  /* The parameter to "__cxa_atexit" is "void (*)(void *)".  */
  tree fn_type = build_function_type_list (void_type_node,
					   ptr_type_node, NULL_TREE);
  tree fn_ptr_type = build_pointer_type (fn_type);
  /* The declaration for `__cxa_atexit' is:
     int __cxa_atexit (void (*)(void *), void *, void *).  */
  const char *name = "__cxa_atexit";
  tree cxa_name = get_identifier (name);
  fn_type = build_function_type_list (integer_type_node, fn_ptr_type,
				      ptr_type_node, ptr_type_node, NULL_TREE);
  tree atexit_fndecl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
				   cxa_name, fn_type);
  SET_DECL_ASSEMBLER_NAME (atexit_fndecl, cxa_name);
  DECL_VISIBILITY (atexit_fndecl) = VISIBILITY_DEFAULT;
  DECL_VISIBILITY_SPECIFIED (atexit_fndecl) = true;
  set_call_expr_flags (atexit_fndecl, ECF_LEAF | ECF_NOTHROW);
  TREE_PUBLIC (atexit_fndecl) = true;
  DECL_EXTERNAL (atexit_fndecl) = true;
  DECL_ARTIFICIAL (atexit_fndecl) = true;
  return atexit_fndecl;
}

/* Build a decl for __dso_handle.  */

static tree
build_dso_handle_decl ()
{
  /* Declare the __dso_handle variable.  */
  tree dso_handle_decl = build_decl (UNKNOWN_LOCATION, VAR_DECL,
				     get_identifier ("__dso_handle"),
				     ptr_type_node);
  TREE_PUBLIC (dso_handle_decl) = true;
  DECL_EXTERNAL (dso_handle_decl) = true;
  DECL_ARTIFICIAL (dso_handle_decl) = true;
#ifdef HAVE_GAS_HIDDEN
  if (dso_handle_decl != error_mark_node)
    {
      DECL_VISIBILITY (dso_handle_decl) = VISIBILITY_HIDDEN;
      DECL_VISIBILITY_SPECIFIED (dso_handle_decl) = true;
    }
#endif
  return dso_handle_decl;
}

/*  This builds one or more constructor functions that register DTORs with
    __cxa_atexit ().  Within a priority level, DTORs are registered in TU
    order - which means that they will run in reverse TU order from cxa_atexit.
    This is the same behavior as using a .fini / .mod_term_funcs section.
    As the functions are built, they are appended to the CTORs vector.  */

static void
build_cxa_dtor_registrations (const vec<tree> &dtors, vec<tree> *ctors)
{
  size_t i,j;
  size_t len = dtors.length ();

  location_t sav_loc = input_location;
  input_location = UNKNOWN_LOCATION;

  tree atexit_fndecl = build_cxa_atexit_decl ();
  tree dso_handle_decl = build_dso_handle_decl ();

  /* We want &__dso_handle.  */
  tree dso_ptr = build1_loc (UNKNOWN_LOCATION, ADDR_EXPR,
			     ptr_type_node, dso_handle_decl);

  i = 0;
  while (i < len)
    {
      priority_type priority = 0;
      tree body = NULL_TREE;
      j = i;
      do
	{
	  priority_type p;
	  tree fn = dtors[j];
	  p = DECL_FINI_PRIORITY (fn);
	  if (j == i)
	    priority = p;
	  else if (p != priority)
	    break;
	  j++;
	}
      while (j < len);

      /* Find the next batch of destructors with the same initialization
	 priority.  */
      for (;i < j; i++)
	{
	  tree fn = dtors[i];
	  DECL_STATIC_DESTRUCTOR (fn) = 0;
	  tree dtor_ptr = build1_loc (UNKNOWN_LOCATION, ADDR_EXPR,
				      ptr_type_node, fn);
	  tree call_cxa_atexit
	    = build_call_expr_loc (UNKNOWN_LOCATION, atexit_fndecl, 3,
				   dtor_ptr, null_pointer_node, dso_ptr);
	  TREE_SIDE_EFFECTS (call_cxa_atexit) = 1;
	  append_to_statement_list (call_cxa_atexit, &body);
	}

      gcc_assert (body != NULL_TREE);
      /* Generate a function to register the DTORs at this priority.  */
      tree new_ctor
	= cgraph_build_static_cdtor_1 ('I', body, priority, true,
				       DECL_FUNCTION_SPECIFIC_OPTIMIZATION (dtors[0]),
				       DECL_FUNCTION_SPECIFIC_TARGET (dtors[0]));
      /* Add this to the list of ctors.  */
      ctors->safe_push (new_ctor);
    }
  input_location = sav_loc;
}

/* Comparison function for qsort.  P1 and P2 are actually of type
   "tree *" and point to static constructors.  DECL_INIT_PRIORITY is
   used to determine the sort order.  */

static int
compare_ctor (const void *p1, const void *p2)
{
  tree f1;
  tree f2;
  int priority1;
  int priority2;

  f1 = *(const tree *)p1;
  f2 = *(const tree *)p2;
  priority1 = DECL_INIT_PRIORITY (f1);
  priority2 = DECL_INIT_PRIORITY (f2);

  if (priority1 < priority2)
    return -1;
  else if (priority1 > priority2)
    return 1;
  else
    /* Ensure a stable sort.  Constructors are executed in backwarding
       order to make LTO initialize braries first.  */
    return DECL_UID (f2) - DECL_UID (f1);
}

/* Comparison function for qsort.  P1 and P2 are actually of type
   "tree *" and point to static destructors.  DECL_FINI_PRIORITY is
   used to determine the sort order.  */

static int
compare_dtor (const void *p1, const void *p2)
{
  tree f1;
  tree f2;
  int priority1;
  int priority2;

  f1 = *(const tree *)p1;
  f2 = *(const tree *)p2;
  priority1 = DECL_FINI_PRIORITY (f1);
  priority2 = DECL_FINI_PRIORITY (f2);

  if (priority1 < priority2)
    return -1;
  else if (priority1 > priority2)
    return 1;
  else
    /* Ensure a stable sort - into TU order.  */
    return DECL_UID (f1) - DECL_UID (f2);
}

/* Comparison function for qsort.  P1 and P2 are of type "tree *" and point to
   a pair of static constructors or destructors.  We first sort on the basis of
   priority and then into TU order (on the strict assumption that DECL_UIDs are
   ordered in the same way as the original functions).  ???: this seems quite
   fragile. */

static int
compare_cdtor_tu_order (const void *p1, const void *p2)
{
  tree f1;
  tree f2;
  int priority1;
  int priority2;

  f1 = *(const tree *)p1;
  f2 = *(const tree *)p2;
  /* We process the DTORs first, and then remove their flag, so this order
     allows for functions that are declared as both CTOR and DTOR.  */
  if (DECL_STATIC_DESTRUCTOR (f1))
    {
      gcc_checking_assert (DECL_STATIC_DESTRUCTOR (f2));
      priority1 = DECL_FINI_PRIORITY (f1);
      priority2 = DECL_FINI_PRIORITY (f2);
    }
  else
    {
      priority1 = DECL_INIT_PRIORITY (f1);
      priority2 = DECL_INIT_PRIORITY (f2);
    }

  if (priority1 < priority2)
    return -1;
  else if (priority1 > priority2)
    return 1;
  else
    /* For equal priority, sort into the order of definition in the TU.  */
    return DECL_UID (f1) - DECL_UID (f2);
}

/* Generate functions to call static constructors and destructors
   for targets that do not support .ctors/.dtors sections.  These
   functions have magic names which are detected by collect2.  */

static void
build_cdtor_fns (vec<tree> *ctors, vec<tree> *dtors)
{
  if (!ctors->is_empty ())
    {
      gcc_assert (!targetm.have_ctors_dtors || in_lto_p);
      ctors->qsort (compare_ctor);
      build_cdtor (/*ctor_p=*/true, *ctors);
    }

  if (!dtors->is_empty ())
    {
      gcc_assert (!targetm.have_ctors_dtors || in_lto_p);
      dtors->qsort (compare_dtor);
      build_cdtor (/*ctor_p=*/false, *dtors);
    }
}

/* Generate new CTORs to register static destructors with __cxa_atexit and add
   them to the existing list of CTORs; we then process the revised CTORs list.

   We sort the DTORs into priority and then TU order, this means that they are
   registered in that order with __cxa_atexit () and therefore will be run in
   the reverse order.

   Likewise, CTORs are sorted into priority and then TU order, which means that
   they will run in that order.

   This matches the behavior of using init/fini or mod_init_func/mod_term_func
   sections.  */

static void
build_cxa_atexit_fns (vec<tree> *ctors, vec<tree> *dtors)
{
  if (!dtors->is_empty ())
    {
      gcc_assert (targetm.dtors_from_cxa_atexit);
      dtors->qsort (compare_cdtor_tu_order);
      build_cxa_dtor_registrations (*dtors, ctors);
    }

  if (!ctors->is_empty ())
    {
      gcc_assert (targetm.dtors_from_cxa_atexit);
      ctors->qsort (compare_cdtor_tu_order);
      build_cdtor (/*ctor_p=*/true, *ctors);
    }
}

/* Look for constructors and destructors and produce function calling them.
   This is needed for targets not supporting ctors or dtors, but we perform the
   transformation also at linktime to merge possibly numerous
   constructors/destructors into single function to improve code locality and
   reduce size.  */

static unsigned int
ipa_cdtor_merge (void)
{
  /* A vector of FUNCTION_DECLs declared as static constructors.  */
  auto_vec<tree, 20> ctors;
  /* A vector of FUNCTION_DECLs declared as static destructors.  */
  auto_vec<tree, 20> dtors;
  struct cgraph_node *node;
  FOR_EACH_DEFINED_FUNCTION (node)
    if (DECL_STATIC_CONSTRUCTOR (node->decl)
	|| DECL_STATIC_DESTRUCTOR (node->decl))
       record_cdtor_fn (node, &ctors, &dtors);
  if (targetm.dtors_from_cxa_atexit)
    build_cxa_atexit_fns (&ctors, &dtors);
  else
    build_cdtor_fns (&ctors, &dtors);
  return 0;
}

namespace {

const pass_data pass_data_ipa_cdtor_merge =
{
  IPA_PASS, /* type */
  "cdtor", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_CGRAPHOPT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_cdtor_merge : public ipa_opt_pass_d
{
public:
  pass_ipa_cdtor_merge (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_cdtor_merge, ctxt,
		      NULL, /* generate_summary */
		      NULL, /* write_summary */
		      NULL, /* read_summary */
		      NULL, /* write_optimization_summary */
		      NULL, /* read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  bool gate (function *) final override;
  unsigned int execute (function *) final override
  {
    return ipa_cdtor_merge ();
  }

}; // class pass_ipa_cdtor_merge

bool
pass_ipa_cdtor_merge::gate (function *)
{
  /* Perform the pass when we have no ctors/dtors support
     or at LTO time to merge multiple constructors into single
     function.  */
  return !targetm.have_ctors_dtors || in_lto_p || targetm.dtors_from_cxa_atexit;
}

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_cdtor_merge (gcc::context *ctxt)
{
  return new pass_ipa_cdtor_merge (ctxt);
}

/* Invalid pointer representing BOTTOM for single user dataflow.  */
#define BOTTOM ((cgraph_node *)(size_t) 2)

/* Meet operation for single user dataflow.
   Here we want to associate variables with sigle function that may access it.

   FUNCTION is current single user of a variable, VAR is variable that uses it.
   Latttice is stored in SINGLE_USER_MAP.

   We represent: 
    - TOP by no entry in SIGNLE_USER_MAP
    - BOTTOM by BOTTOM in AUX pointer (to save lookups)
    - known single user by cgraph pointer in SINGLE_USER_MAP.  */

cgraph_node *
meet (cgraph_node *function, varpool_node *var,
       hash_map<varpool_node *, cgraph_node *> &single_user_map)
{
  struct cgraph_node *user, **f;

  if (var->aux == BOTTOM)
    return BOTTOM;

  f = single_user_map.get (var);
  if (!f)
    return function;
  user = *f;
  if (!function)
    return user;
  else if (function != user)
    return BOTTOM;
  else
    return function;
}

/* Propagation step of single-use dataflow.

   Check all uses of VNODE and see if they are used by single function FUNCTION.
   SINGLE_USER_MAP represents the dataflow lattice.  */

cgraph_node *
propagate_single_user (varpool_node *vnode, cgraph_node *function,
		       hash_map<varpool_node *, cgraph_node *> &single_user_map)
{
  int i;
  struct ipa_ref *ref;

  gcc_assert (!vnode->externally_visible);

  /* If node is an alias, first meet with its target.  */
  if (vnode->alias)
    function = meet (function, vnode->get_alias_target (), single_user_map);

  /* Check all users and see if they correspond to a single function.  */
  for (i = 0; vnode->iterate_referring (i, ref) && function != BOTTOM; i++)
    {
      struct cgraph_node *cnode = dyn_cast <cgraph_node *> (ref->referring);
      if (cnode)
	{
	  if (cnode->inlined_to)
	    cnode = cnode->inlined_to;
	  if (!function)
	    function = cnode;
	  else if (function != cnode)
	    function = BOTTOM;
	}
      else
	function = meet (function, dyn_cast <varpool_node *> (ref->referring),
			 single_user_map);
    }
  return function;
}

/* Pass setting used_by_single_function flag.
   This flag is set on variable when there is only one function that may
   possibly referr to it.  */

static unsigned int
ipa_single_use (void)
{
  varpool_node *first = (varpool_node *) (void *) 1;
  varpool_node *var;
  hash_map<varpool_node *, cgraph_node *> single_user_map;

  FOR_EACH_DEFINED_VARIABLE (var)
    if (!var->all_refs_explicit_p ())
      var->aux = BOTTOM;
    else
      {
	/* Enqueue symbol for dataflow.  */
        var->aux = first;
	first = var;
      }

  /* The actual dataflow.  */

  while (first != (void *) 1)
    {
      cgraph_node *user, *orig_user, **f;

      var = first;
      first = (varpool_node *)first->aux;

      f = single_user_map.get (var);
      if (f)
	orig_user = *f;
      else
	orig_user = NULL;
      user = propagate_single_user (var, orig_user, single_user_map);

      gcc_checking_assert (var->aux != BOTTOM);

      /* If user differs, enqueue all references.  */
      if (user != orig_user)
	{
	  unsigned int i;
	  ipa_ref *ref;

	  single_user_map.put (var, user);

	  /* Enqueue all aliases for re-processing.  */
	  for (i = 0; var->iterate_direct_aliases (i, ref); i++)
	    if (!ref->referring->aux)
	      {
		ref->referring->aux = first;
		first = dyn_cast <varpool_node *> (ref->referring);
	      }
	  /* Enqueue all users for re-processing.  */
	  for (i = 0; var->iterate_reference (i, ref); i++)
	    if (!ref->referred->aux
	        && ref->referred->definition
		&& is_a <varpool_node *> (ref->referred))
	      {
		ref->referred->aux = first;
		first = dyn_cast <varpool_node *> (ref->referred);
	      }

	  /* If user is BOTTOM, just punt on this var.  */
	  if (user == BOTTOM)
	    var->aux = BOTTOM;
	  else
	    var->aux = NULL;
	}
      else
	var->aux = NULL;
    }

  FOR_EACH_DEFINED_VARIABLE (var)
    {
      if (var->aux != BOTTOM)
	{
	  /* Not having the single user known means that the VAR is
	     unreachable.  Either someone forgot to remove unreachable
	     variables or the reachability here is wrong.  */

	  gcc_checking_assert (single_user_map.get (var));

	  if (dump_file)
	    {
	      fprintf (dump_file, "Variable %s is used by single function\n",
		       var->dump_name ());
	    }
	  var->used_by_single_function = true;
	}
      var->aux = NULL;
    }
  return 0;
}

namespace {

const pass_data pass_data_ipa_single_use =
{
  IPA_PASS, /* type */
  "single-use", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_CGRAPHOPT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_single_use : public ipa_opt_pass_d
{
public:
  pass_ipa_single_use (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_single_use, ctxt,
		      NULL, /* generate_summary */
		      NULL, /* write_summary */
		      NULL, /* read_summary */
		      NULL, /* write_optimization_summary */
		      NULL, /* read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  unsigned int execute (function *) final override { return ipa_single_use (); }

}; // class pass_ipa_single_use

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_single_use (gcc::context *ctxt)
{
  return new pass_ipa_single_use (ctxt);
}

