/* Basic IPA optimizations and utilities.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "calls.h"
#include "stringpool.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "pointer-set.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "flags.h"
#include "target.h"
#include "tree-iterator.h"
#include "ipa-utils.h"
#include "ipa-inline.h"
#include "tree-inline.h"
#include "profile.h"
#include "params.h"

/* Return true when NODE can not be local. Worker for cgraph_local_node_p.  */

static bool
cgraph_non_local_node_p_1 (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
   /* FIXME: Aliases can be local, but i386 gets thunks wrong then.  */
   return !(cgraph_only_called_directly_or_aliased_p (node)
	    && !ipa_ref_has_aliases_p (&node->ref_list)
	    && node->definition
	    && !DECL_EXTERNAL (node->decl)
	    && !node->externally_visible
	    && !node->used_from_other_partition
	    && !node->in_other_partition);
}

/* Return true when function can be marked local.  */

static bool
cgraph_local_node_p (struct cgraph_node *node)
{
   struct cgraph_node *n = cgraph_function_or_thunk_node (node, NULL);

   /* FIXME: thunks can be considered local, but we need prevent i386
      from attempting to change calling convention of them.  */
   if (n->thunk.thunk_p)
     return false;
   return !cgraph_for_node_and_aliases (n,
					cgraph_non_local_node_p_1, NULL, true);
					
}

/* Return true when NODE has ADDR reference.  */

static bool
has_addr_references_p (struct cgraph_node *node,
		       void *data ATTRIBUTE_UNUSED)
{
  int i;
  struct ipa_ref *ref;

  for (i = 0; ipa_ref_list_referring_iterate (&node->ref_list,
					      i, ref); i++)
    if (ref->use == IPA_REF_ADDR)
      return true;
  return false;
}

/* Look for all functions inlined to NODE and update their inlined_to pointers
   to INLINED_TO.  */

static void
update_inlined_to_pointer (struct cgraph_node *node, struct cgraph_node *inlined_to)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    if (e->callee->global.inlined_to)
      {
        e->callee->global.inlined_to = inlined_to;
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
	      struct pointer_set_t *reachable)
{
  /* Node is still in queue; do nothing.  */
  if (node->aux && node->aux != (void *) 2)
    return;
  /* Node was already processed as unreachable, re-enqueue
     only if it became reachable now.  */
  if (node->aux == (void *)2 && !pointer_set_contains (reachable, node))
    return;
  node->aux = *first;
  *first = node;
}

/* Process references.  */

static void
process_references (struct ipa_ref_list *list,
		    symtab_node **first,
		    bool before_inlining_p,
		    struct pointer_set_t *reachable)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_reference_iterate (list, i, ref); i++)
    {
      symtab_node *node = ref->referred;

      if (node->definition && !node->in_other_partition
	  && ((!DECL_EXTERNAL (node->decl) || node->alias)
	      || (before_inlining_p
		  /* We use variable constructors during late complation for
		     constant folding.  Keep references alive so partitioning
		     knows about potential references.  */
		  || (TREE_CODE (node->decl) == VAR_DECL
		      && flag_wpa
		      && ctor_for_folding (node->decl)
		         != error_mark_node))))
	pointer_set_insert (reachable, node);
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
walk_polymorphic_call_targets (pointer_set_t *reachable_call_targets,
			       struct cgraph_edge *edge,
			       symtab_node **first,
			       pointer_set_t *reachable, bool before_inlining_p)
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
      for (i = 0; i < targets.length (); i++)
	{
	  struct cgraph_node *n = targets[i];

	  /* Do not bother to mark virtual methods in anonymous namespace;
	     either we will find use of virtual table defining it, or it is
	     unused.  */
	  if (TREE_CODE (TREE_TYPE (n->decl)) == METHOD_TYPE
	      && type_in_anonymous_namespace_p
		    (method_class_type (TREE_TYPE (n->decl))))
	    continue;

	  /* Prior inlining, keep alive bodies of possible targets for
	     devirtualization.  */
	   if (n->definition
	       && before_inlining_p)
	     pointer_set_insert (reachable, n);

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
      if (targets.length () <= 1)
	{
	  cgraph_node *target, *node = edge->caller;
	  if (targets.length () == 1)
	    target = targets[0];
	  else
	    target = cgraph_get_create_node
		       (builtin_decl_implicit (BUILT_IN_UNREACHABLE));

	  if (dump_file)
	    fprintf (dump_file,
		     "Devirtualizing call in %s/%i to %s/%i\n",
		     edge->caller->name (),
		     edge->caller->order,
		     target->name (), target->order);
	  edge = cgraph_make_edge_direct (edge, target);
	  if (inline_summary_vec)
	    inline_update_overall_summary (node);
	  else if (edge->call_stmt)
	    cgraph_redirect_edge_call_stmt_to_callee (edge);
	}
    }
}

/* Perform reachability analysis and reclaim all unreachable nodes.

   The algorithm is basically mark&sweep but with some extra refinements:

   - reachable extern inline functions needs special handling; the bodies needs
     to stay in memory until inlining in hope that they will be inlined.
     After inlining we release their bodies and turn them into unanalyzed
     nodes even when they are reachable.

     BEFORE_INLINING_P specify whether we are before or after inlining.

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
     stop walking their references to let everything static referneced by them
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
symtab_remove_unreachable_nodes (bool before_inlining_p, FILE *file)
{
  symtab_node *first = (symtab_node *) (void *) 1;
  struct cgraph_node *node, *next;
  varpool_node *vnode, *vnext;
  bool changed = false;
  struct pointer_set_t *reachable = pointer_set_create ();
  struct pointer_set_t *body_needed_for_clonning = pointer_set_create ();
  struct pointer_set_t *reachable_call_targets = pointer_set_create ();

  timevar_push (TV_IPA_UNREACHABLE);
#ifdef ENABLE_CHECKING
  verify_symtab ();
#endif
  if (optimize && flag_devirtualize)
    build_type_inheritance_graph ();
  if (file)
    fprintf (file, "\nReclaiming functions:");
#ifdef ENABLE_CHECKING
  FOR_EACH_FUNCTION (node)
    gcc_assert (!node->aux);
  FOR_EACH_VARIABLE (vnode)
    gcc_assert (!vnode->aux);
#endif
  /* Mark functions whose bodies are obviously needed.
     This is mostly when they can be referenced externally.  Inline clones
     are special since their declarations are shared with master clone and thus
     cgraph_can_remove_if_no_direct_calls_and_refs_p should not be called on them.  */
  FOR_EACH_FUNCTION (node)
    {
      node->used_as_abstract_origin = false;
      if (node->definition
	  && !node->global.inlined_to
	  && !node->in_other_partition
	  && !cgraph_can_remove_if_no_direct_calls_and_refs_p (node))
	{
	  gcc_assert (!node->global.inlined_to);
	  pointer_set_insert (reachable, node);
	  enqueue_node (node, &first, reachable);
	}
      else
	gcc_assert (!node->aux);
     }

  /* Mark variables that are obviously needed.  */
  FOR_EACH_DEFINED_VARIABLE (vnode)
    if (!varpool_can_remove_if_no_refs (vnode)
	&& !vnode->in_other_partition)
      {
	pointer_set_insert (reachable, vnode);
	enqueue_node (vnode, &first, reachable);
      }

  /* Perform reachability analysis.  */
  while (first != (symtab_node *) (void *) 1)
    {
      bool in_boundary_p = !pointer_set_contains (reachable, first);
      symtab_node *node = first;

      first = (symtab_node *)first->aux;

      /* If we are processing symbol in boundary, mark its AUX pointer for
	 possible later re-processing in enqueue_node.  */
      if (in_boundary_p)
	node->aux = (void *)2;
      else
	{
	  if (DECL_ABSTRACT_ORIGIN (node->decl))
	    {
	      struct cgraph_node *origin_node
	      = cgraph_get_create_node (DECL_ABSTRACT_ORIGIN (node->decl));
	      origin_node->used_as_abstract_origin = true;
	      enqueue_node (origin_node, &first, reachable);
	    }
	  /* If any symbol in a comdat group is reachable, force
	     all externally visible symbols in the same comdat
	     group to be reachable as well.  Comdat-local symbols
	     can be discarded if all uses were inlined.  */
	  if (node->same_comdat_group)
	    {
	      symtab_node *next;
	      for (next = node->same_comdat_group;
		   next != node;
		   next = next->same_comdat_group)
		if (!symtab_comdat_local_p (next)
		    && !pointer_set_insert (reachable, next))
		  enqueue_node (next, &first, reachable);
	    }
	  /* Mark references as reachable.  */
	  process_references (&node->ref_list, &first,
			      before_inlining_p, reachable);
	}

      if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
	{
	  /* Mark the callees reachable unless they are direct calls to extern
 	     inline functions we decided to not inline.  */
	  if (!in_boundary_p)
	    {
	      struct cgraph_edge *e;
	      /* Keep alive possible targets for devirtualization.  */
	      if (optimize && flag_devirtualize)
		{
		  struct cgraph_edge *next;
		  for (e = cnode->indirect_calls; e; e = next)
		    {
		      next = e->next_callee;
		      if (e->indirect_info->polymorphic)
			walk_polymorphic_call_targets (reachable_call_targets,
						       e, &first, reachable,
						       before_inlining_p);
		    }
		}
	      for (e = cnode->callees; e; e = e->next_callee)
		{
		  if (e->callee->definition
		      && !e->callee->in_other_partition
		      && (!e->inline_failed
			  || !DECL_EXTERNAL (e->callee->decl)
			  || e->callee->alias
			  || before_inlining_p))
		    pointer_set_insert (reachable, e->callee);
		  enqueue_node (e->callee, &first, reachable);
		}

	      /* When inline clone exists, mark body to be preserved so when removing
		 offline copy of the function we don't kill it.  */
	      if (cnode->global.inlined_to)
	        pointer_set_insert (body_needed_for_clonning, cnode->decl);

	      /* For non-inline clones, force their origins to the boundary and ensure
		 that body is not removed.  */
	      while (cnode->clone_of)
		{
		  bool noninline = cnode->clone_of->decl != cnode->decl;
		  cnode = cnode->clone_of;
		  if (noninline)
		    {
		      pointer_set_insert (body_needed_for_clonning, cnode->decl);
		      enqueue_node (cnode, &first, reachable);
		    }
		}

	    }
	  /* If any reachable function has simd clones, mark them as
	     reachable as well.  */
	  if (cnode->simd_clones)
	    {
	      cgraph_node *next;
	      for (next = cnode->simd_clones;
		   next;
		   next = next->simdclone->next_clone)
		if (in_boundary_p
		    || !pointer_set_insert (reachable, next))
		  enqueue_node (next, &first, reachable);
	    }
	}
      /* When we see constructor of external variable, keep referred nodes in the
	boundary.  This will also hold initializers of the external vars NODE
	refers to.  */
      varpool_node *vnode = dyn_cast <varpool_node> (node);
      if (vnode
	  && DECL_EXTERNAL (node->decl)
	  && !vnode->alias
	  && in_boundary_p)
	{
	  struct ipa_ref *ref;
	  for (int i = 0; ipa_ref_list_reference_iterate (&node->ref_list, i, ref); i++)
	    enqueue_node (ref->referred, &first, reachable);
	}
    }

  /* Remove unreachable functions.   */
  for (node = cgraph_first_function (); node; node = next)
    {
      next = cgraph_next_function (node);

      /* If node is not needed at all, remove it.  */
      if (!node->aux)
	{
	  if (file)
	    fprintf (file, " %s", node->name ());
	  cgraph_remove_node (node);
	  changed = true;
	}
      /* If node is unreachable, remove its body.  */
      else if (!pointer_set_contains (reachable, node))
        {
	  if (!pointer_set_contains (body_needed_for_clonning, node->decl))
	    cgraph_release_function_body (node);
	  else if (!node->clone_of)
	    gcc_assert (in_lto_p || DECL_RESULT (node->decl));
	  if (node->definition)
	    {
	      if (file)
		fprintf (file, " %s", node->name ());
	      node->analyzed = false;
	      node->definition = false;
	      node->cpp_implicit_alias = false;
	      node->alias = false;
	      node->weakref = false;
	      if (!node->in_other_partition)
		node->local.local = false;
	      cgraph_node_remove_callees (node);
	      ipa_remove_all_references (&node->ref_list);
	      changed = true;
	    }
	}
      else
	gcc_assert (node->clone_of || !cgraph_function_with_gimple_body_p (node)
		    || in_lto_p || DECL_RESULT (node->decl));
    }

  /* Inline clones might be kept around so their materializing allows further
     cloning.  If the function the clone is inlined into is removed, we need
     to turn it into normal cone.  */
  FOR_EACH_FUNCTION (node)
    {
      if (node->global.inlined_to
	  && !node->callers)
	{
	  gcc_assert (node->clones);
	  node->global.inlined_to = NULL;
	  update_inlined_to_pointer (node, node);
	}
      node->aux = NULL;
    }

  /* Remove unreachable variables.  */
  if (file)
    fprintf (file, "\nReclaiming variables:");
  for (vnode = varpool_first_variable (); vnode; vnode = vnext)
    {
      vnext = varpool_next_variable (vnode);
      if (!vnode->aux
	  /* For can_refer_decl_in_current_unit_p we want to track for
	     all external variables if they are defined in other partition
	     or not.  */
	  && (!flag_ltrans || !DECL_EXTERNAL (vnode->decl)))
	{
	  if (file)
	    fprintf (file, " %s", vnode->name ());
	  varpool_remove_node (vnode);
	  changed = true;
	}
      else if (!pointer_set_contains (reachable, vnode))
        {
	  tree init;
	  if (vnode->definition)
	    {
	      if (file)
		fprintf (file, " %s", vnode->name ());
	      changed = true;
	    }
	  vnode->definition = false;
	  vnode->analyzed = false;
	  vnode->aux = NULL;

	  /* Keep body if it may be useful for constant folding.  */
	  if ((init = ctor_for_folding (vnode->decl)) == error_mark_node)
	    varpool_remove_initializer (vnode);
	  else
	    DECL_INITIAL (vnode->decl) = init;
	  ipa_remove_all_references (&vnode->ref_list);
	}
      else
	vnode->aux = NULL;
    }

  pointer_set_destroy (reachable);
  pointer_set_destroy (body_needed_for_clonning);
  pointer_set_destroy (reachable_call_targets);

  /* Now update address_taken flags and try to promote functions to be local.  */
  if (file)
    fprintf (file, "\nClearing address taken flags:");
  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->address_taken
	&& !node->used_from_other_partition)
      {
	if (!cgraph_for_node_and_aliases (node, has_addr_references_p, NULL, true))
	  {
	    if (file)
	      fprintf (file, " %s", node->name ());
	    node->address_taken = false;
	    changed = true;
	    if (cgraph_local_node_p (node))
	      {
		node->local.local = true;
		if (file)
		  fprintf (file, " (local)");
	      }
	  }
      }
  if (file)
    fprintf (file, "\n");

#ifdef ENABLE_CHECKING
  verify_symtab ();
#endif

  /* If we removed something, perhaps profile could be improved.  */
  if (changed && optimize && inline_edge_summary_vec.exists ())
    FOR_EACH_DEFINED_FUNCTION (node)
      ipa_propagate_frequency (node);

  timevar_pop (TV_IPA_UNREACHABLE);
  return changed;
}

/* Discover variables that have no longer address taken or that are read only
   and update their flags.

   FIXME: This can not be done in between gimplify and omp_expand since
   readonly flag plays role on what is shared and what is not.  Currently we do
   this transformation as part of whole program visibility and re-do at
   ipa-reference pass (to take into account clonning), but it would
   make sense to do it before early optimizations.  */

void
ipa_discover_readonly_nonaddressable_vars (void)
{
  varpool_node *vnode;
  if (dump_file)
    fprintf (dump_file, "Clearing variable flags:");
  FOR_EACH_VARIABLE (vnode)
    if (vnode->definition && varpool_all_refs_explicit_p (vnode)
	&& (TREE_ADDRESSABLE (vnode->decl)
	    || !TREE_READONLY (vnode->decl)))
      {
	bool written = false;
	bool address_taken = false;
	int i;
        struct ipa_ref *ref;
        for (i = 0; ipa_ref_list_referring_iterate (&vnode->ref_list,
						   i, ref)
		    && (!written || !address_taken); i++)
	  switch (ref->use)
	    {
	    case IPA_REF_ADDR:
	      address_taken = true;
	      break;
	    case IPA_REF_LOAD:
	      break;
	    case IPA_REF_STORE:
	      written = true;
	      break;
	    }
	if (TREE_ADDRESSABLE (vnode->decl) && !address_taken)
	  {
	    if (dump_file)
	      fprintf (dump_file, " %s (addressable)", vnode->name ());
	    TREE_ADDRESSABLE (vnode->decl) = 0;
	  }
	if (!TREE_READONLY (vnode->decl) && !address_taken && !written
	    /* Making variable in explicit section readonly can cause section
	       type conflict. 
	       See e.g. gcc.c-torture/compile/pr23237.c */
	    && DECL_SECTION_NAME (vnode->decl) == NULL)
	  {
	    if (dump_file)
	      fprintf (dump_file, " %s (read-only)", vnode->name ());
	    TREE_READONLY (vnode->decl) = 1;
	  }
      }
  if (dump_file)
    fprintf (dump_file, "\n");
}

/* Return true when there is a reference to node and it is not vtable.  */
static bool
address_taken_from_non_vtable_p (symtab_node *node)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_referring_iterate (&node->ref_list,
					     i, ref); i++)
    if (ref->use == IPA_REF_ADDR)
      {
	varpool_node *node;
	if (is_a <cgraph_node> (ref->referring))
	  return true;
	node = ipa_ref_referring_varpool_node (ref);
	if (!DECL_VIRTUAL_P (node->decl))
	  return true;
      }
  return false;
}

/* A helper for comdat_can_be_unshared_p.  */

static bool
comdat_can_be_unshared_p_1 (symtab_node *node)
{
  /* When address is taken, we don't know if equality comparison won't
     break eventually. Exception are virutal functions, C++
     constructors/destructors and vtables, where this is not possible by
     language standard.  */
  if (!DECL_VIRTUAL_P (node->decl)
      && (TREE_CODE (node->decl) != FUNCTION_DECL
	  || (!DECL_CXX_CONSTRUCTOR_P (node->decl)
	      && !DECL_CXX_DESTRUCTOR_P (node->decl)))
      && address_taken_from_non_vtable_p (node))
    return false;

  /* If the symbol is used in some weird way, better to not touch it.  */
  if (node->force_output)
    return false;

  /* Explicit instantiations needs to be output when possibly
     used externally.  */
  if (node->forced_by_abi
      && TREE_PUBLIC (node->decl)
      && (node->resolution != LDPR_PREVAILING_DEF_IRONLY
          && !flag_whole_program))
    return false;

  /* Non-readonly and volatile variables can not be duplicated.  */
  if (is_a <varpool_node> (node)
      && (!TREE_READONLY (node->decl)
	  || TREE_THIS_VOLATILE (node->decl)))
    return false;
  return true;
}

/* COMDAT functions must be shared only if they have address taken,
   otherwise we can produce our own private implementation with
   -fwhole-program.  
   Return true when turning COMDAT functoin static can not lead to wrong
   code when the resulting object links with a library defining same COMDAT.

   Virtual functions do have their addresses taken from the vtables,
   but in C++ there is no way to compare their addresses for equality.  */

static bool
comdat_can_be_unshared_p (symtab_node *node)
{
  if (!comdat_can_be_unshared_p_1 (node))
    return false;
  if (node->same_comdat_group)
    {
      symtab_node *next;

      /* If more than one function is in the same COMDAT group, it must
         be shared even if just one function in the comdat group has
         address taken.  */
      for (next = node->same_comdat_group;
	   next != node; next = next->same_comdat_group)
        if (!comdat_can_be_unshared_p_1 (next))
          return false;
    }
  return true;
}

/* Return true when function NODE should be considered externally visible.  */

static bool
cgraph_externally_visible_p (struct cgraph_node *node,
			     bool whole_program)
{
  if (!node->definition)
    return false;
  if (!TREE_PUBLIC (node->decl)
      || DECL_EXTERNAL (node->decl))
    return false;

  /* Do not try to localize built-in functions yet.  One of problems is that we
     end up mangling their asm for WHOPR that makes it impossible to call them
     using the implicit built-in declarations anymore.  Similarly this enables
     us to remove them as unreachable before actual calls may appear during
     expansion or folding.  */
  if (DECL_BUILT_IN (node->decl))
    return true;

  /* If linker counts on us, we must preserve the function.  */
  if (symtab_used_from_object_file_p (node))
    return true;
  if (DECL_PRESERVE_P (node->decl))
    return true;
  if (lookup_attribute ("externally_visible",
			DECL_ATTRIBUTES (node->decl)))
    return true;
  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
      && lookup_attribute ("dllexport",
			   DECL_ATTRIBUTES (node->decl)))
    return true;
  if (node->resolution == LDPR_PREVAILING_DEF_IRONLY)
    return false;
  /* When doing LTO or whole program, we can bring COMDAT functoins static.
     This improves code quality and we know we will duplicate them at most twice
     (in the case that we are not using plugin and link with object file
      implementing same COMDAT)  */
  if ((in_lto_p || whole_program)
      && DECL_COMDAT (node->decl)
      && comdat_can_be_unshared_p (node))
    return false;

  /* When doing link time optimizations, hidden symbols become local.  */
  if (in_lto_p
      && (DECL_VISIBILITY (node->decl) == VISIBILITY_HIDDEN
	  || DECL_VISIBILITY (node->decl) == VISIBILITY_INTERNAL)
      /* Be sure that node is defined in IR file, not in other object
	 file.  In that case we don't set used_from_other_object_file.  */
      && node->definition)
    ;
  else if (!whole_program)
    return true;

  if (MAIN_NAME_P (DECL_NAME (node->decl)))
    return true;

  return false;
}

/* Return true when variable VNODE should be considered externally visible.  */

bool
varpool_externally_visible_p (varpool_node *vnode)
{
  if (DECL_EXTERNAL (vnode->decl))
    return true;

  if (!TREE_PUBLIC (vnode->decl))
    return false;

  /* If linker counts on us, we must preserve the function.  */
  if (symtab_used_from_object_file_p (vnode))
    return true;

  if (DECL_HARD_REGISTER (vnode->decl))
    return true;
  if (DECL_PRESERVE_P (vnode->decl))
    return true;
  if (lookup_attribute ("externally_visible",
			DECL_ATTRIBUTES (vnode->decl)))
    return true;
  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
      && lookup_attribute ("dllexport",
			   DECL_ATTRIBUTES (vnode->decl)))
    return true;

  /* See if we have linker information about symbol not being used or
     if we need to make guess based on the declaration.

     Even if the linker clams the symbol is unused, never bring internal
     symbols that are declared by user as used or externally visible.
     This is needed for i.e. references from asm statements.   */
  if (symtab_used_from_object_file_p (vnode))
    return true;
  if (vnode->resolution == LDPR_PREVAILING_DEF_IRONLY)
    return false;

  /* As a special case, the COMDAT virtual tables can be unshared.
     In LTO mode turn vtables into static variables.  The variable is readonly,
     so this does not enable more optimization, but referring static var
     is faster for dynamic linking.  Also this match logic hidding vtables
     from LTO symbol tables.  */
  if ((in_lto_p || flag_whole_program)
      && DECL_COMDAT (vnode->decl)
      && comdat_can_be_unshared_p (vnode))
    return false;

  /* When doing link time optimizations, hidden symbols become local.  */
  if (in_lto_p
      && (DECL_VISIBILITY (vnode->decl) == VISIBILITY_HIDDEN
	  || DECL_VISIBILITY (vnode->decl) == VISIBILITY_INTERNAL)
      /* Be sure that node is defined in IR file, not in other object
	 file.  In that case we don't set used_from_other_object_file.  */
      && vnode->definition)
    ;
  else if (!flag_whole_program)
    return true;

  /* Do not attempt to privatize COMDATS by default.
     This would break linking with C++ libraries sharing
     inline definitions.

     FIXME: We can do so for readonly vars with no address taken and
     possibly also for vtables since no direct pointer comparsion is done.
     It might be interesting to do so to reduce linking overhead.  */
  if (DECL_COMDAT (vnode->decl) || DECL_WEAK (vnode->decl))
    return true;
  return false;
}

/* Return true if reference to NODE can be replaced by a local alias.
   Local aliases save dynamic linking overhead and enable more optimizations.
 */

bool
can_replace_by_local_alias (symtab_node *node)
{
  return (symtab_node_availability (node) > AVAIL_OVERWRITABLE
	  && !symtab_can_be_discarded (node));
}

/* Mark visibility of all functions.

   A local function is one whose calls can occur only in the current
   compilation unit and all its calls are explicit, so we can change
   its calling convention.  We simply mark all static functions whose
   address is not taken as local.

   We also change the TREE_PUBLIC flag of all declarations that are public
   in language point of view but we want to overwrite this default
   via visibilities for the backend point of view.  */

static unsigned int
function_and_variable_visibility (bool whole_program)
{
  struct cgraph_node *node;
  varpool_node *vnode;

  /* All aliases should be procssed at this point.  */
  gcc_checking_assert (!alias_pairs || !alias_pairs->length ());

  FOR_EACH_FUNCTION (node)
    {
      int flags = flags_from_decl_or_type (node->decl);

      /* Optimize away PURE and CONST constructors and destructors.  */
      if (optimize
	  && (flags & (ECF_CONST | ECF_PURE))
	  && !(flags & ECF_LOOPING_CONST_OR_PURE))
	{
	  DECL_STATIC_CONSTRUCTOR (node->decl) = 0;
	  DECL_STATIC_DESTRUCTOR (node->decl) = 0;
	}

      /* Frontends and alias code marks nodes as needed before parsing is finished.
	 We may end up marking as node external nodes where this flag is meaningless
	 strip it.  */
      if (DECL_EXTERNAL (node->decl) || !node->definition)
	{
	  node->force_output = 0;
	  node->forced_by_abi = 0;
	}

      /* C++ FE on lack of COMDAT support create local COMDAT functions
	 (that ought to be shared but can not due to object format
	 limitations).  It is necessary to keep the flag to make rest of C++ FE
	 happy.  Clear the flag here to avoid confusion in middle-end.  */
      if (DECL_COMDAT (node->decl) && !TREE_PUBLIC (node->decl))
        DECL_COMDAT (node->decl) = 0;

      /* For external decls stop tracking same_comdat_group. It doesn't matter
	 what comdat group they are in when they won't be emitted in this TU.  */
      if (node->same_comdat_group && DECL_EXTERNAL (node->decl))
	{
#ifdef ENABLE_CHECKING
	  symtab_node *n;

	  for (n = node->same_comdat_group;
	       n != node;
	       n = n->same_comdat_group)
	      /* If at least one of same comdat group functions is external,
		 all of them have to be, otherwise it is a front-end bug.  */
	      gcc_assert (DECL_EXTERNAL (n->decl));
#endif
	  symtab_dissolve_same_comdat_group_list (node);
	}
      gcc_assert ((!DECL_WEAK (node->decl)
		  && !DECL_COMDAT (node->decl))
      	          || TREE_PUBLIC (node->decl)
		  || node->weakref
		  || DECL_EXTERNAL (node->decl));
      if (cgraph_externally_visible_p (node, whole_program))
        {
	  gcc_assert (!node->global.inlined_to);
	  node->externally_visible = true;
	}
      else
	{
	  node->externally_visible = false;
	  node->forced_by_abi = false;
	}
      if (!node->externally_visible
	  && node->definition && !node->weakref
	  && !DECL_EXTERNAL (node->decl))
	{
	  gcc_assert (whole_program || in_lto_p
		      || !TREE_PUBLIC (node->decl));
	  node->unique_name = ((node->resolution == LDPR_PREVAILING_DEF_IRONLY
				|| node->unique_name
				|| node->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP)
				&& TREE_PUBLIC (node->decl));
	  node->resolution = LDPR_PREVAILING_DEF_IRONLY;
	  if (node->same_comdat_group && TREE_PUBLIC (node->decl))
	    {
	      symtab_node *next = node;

	      /* Set all members of comdat group local.  */
	      if (node->same_comdat_group)
		for (next = node->same_comdat_group;
		     next != node;
		     next = next->same_comdat_group)
		{
		  symtab_make_decl_local (next->decl);
		  next->unique_name = ((next->resolution == LDPR_PREVAILING_DEF_IRONLY
					|| next->unique_name
					|| next->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP)
					&& TREE_PUBLIC (next->decl));
		}
	      /* cgraph_externally_visible_p has already checked all other nodes
	         in the group and they will all be made local.  We need to
	         dissolve the group at once so that the predicate does not
	         segfault though. */
	      symtab_dissolve_same_comdat_group_list (node);
	    }
	  symtab_make_decl_local (node->decl);
	}

      if (node->thunk.thunk_p
	  && TREE_PUBLIC (node->decl))
	{
	  struct cgraph_node *decl_node = node;

	  decl_node = cgraph_function_node (decl_node->callees->callee, NULL);

	  /* Thunks have the same visibility as function they are attached to.
	     Make sure the C++ front end set this up properly.  */
	  if (DECL_ONE_ONLY (decl_node->decl))
	    {
	      gcc_checking_assert (DECL_COMDAT (node->decl)
				   == DECL_COMDAT (decl_node->decl));
	      gcc_checking_assert (DECL_COMDAT_GROUP (node->decl)
				   == DECL_COMDAT_GROUP (decl_node->decl));
	      gcc_checking_assert (node->same_comdat_group);
	    }
	  if (DECL_EXTERNAL (decl_node->decl))
	    DECL_EXTERNAL (node->decl) = 1;
	}

      /* If whole comdat group is used only within LTO code, we can dissolve it,
	 we handle the unification ourselves.
	 We keep COMDAT and weak so visibility out of DSO does not change.
	 Later we may bring the symbols static if they are not exported.  */
      if (DECL_ONE_ONLY (node->decl)
	  && (node->resolution == LDPR_PREVAILING_DEF_IRONLY
	      || node->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP))
	{
	  symtab_node *next = node;

	  if (node->same_comdat_group)
	    for (next = node->same_comdat_group;
		 next != node;
		 next = next->same_comdat_group)
	      if (next->externally_visible
		  && (next->resolution != LDPR_PREVAILING_DEF_IRONLY
		      && next->resolution != LDPR_PREVAILING_DEF_IRONLY_EXP))
		break;
	  if (node == next)
	    {
	      if (node->same_comdat_group)
	        for (next = node->same_comdat_group;
		     next != node;
		     next = next->same_comdat_group)
		{
		  DECL_COMDAT_GROUP (next->decl) = NULL;
		  DECL_WEAK (next->decl) = false;
		}
	      DECL_COMDAT_GROUP (node->decl) = NULL;
	      symtab_dissolve_same_comdat_group_list (node);
	    }
	}
    }
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      node->local.local |= cgraph_local_node_p (node);

      /* If we know that function can not be overwritten by a different semantics
	 and moreover its section can not be discarded, replace all direct calls
	 by calls to an nonoverwritable alias.  This make dynamic linking
	 cheaper and enable more optimization.

	 TODO: We can also update virtual tables.  */
      if (node->callers && can_replace_by_local_alias (node))
	{
	  struct cgraph_node *alias = cgraph (symtab_nonoverwritable_alias (node));

	  if (alias && alias != node)
	    {
	      while (node->callers)
		{
		  struct cgraph_edge *e = node->callers;

		  cgraph_redirect_edge_callee (e, alias);
		  if (gimple_has_body_p (e->caller->decl))
		    {
		      push_cfun (DECL_STRUCT_FUNCTION (e->caller->decl));
		      cgraph_redirect_edge_call_stmt_to_callee (e);
		      pop_cfun ();
		    }
		}
	    }
	}
    }
  FOR_EACH_VARIABLE (vnode)
    {
      /* weak flag makes no sense on local variables.  */
      gcc_assert (!DECL_WEAK (vnode->decl)
		  || vnode->weakref
      		  || TREE_PUBLIC (vnode->decl)
		  || DECL_EXTERNAL (vnode->decl));
      /* In several cases declarations can not be common:

	 - when declaration has initializer
	 - when it is in weak
	 - when it has specific section
	 - when it resides in non-generic address space.
	 - if declaration is local, it will get into .local common section
	   so common flag is not needed.  Frontends still produce these in
	   certain cases, such as for:

	     static int a __attribute__ ((common))

	 Canonicalize things here and clear the redundant flag.  */
      if (DECL_COMMON (vnode->decl)
	  && (!(TREE_PUBLIC (vnode->decl)
	      || DECL_EXTERNAL (vnode->decl))
	      || (DECL_INITIAL (vnode->decl)
		  && DECL_INITIAL (vnode->decl) != error_mark_node)
	      || DECL_WEAK (vnode->decl)
	      || DECL_SECTION_NAME (vnode->decl) != NULL
	      || ! (ADDR_SPACE_GENERIC_P
		    (TYPE_ADDR_SPACE (TREE_TYPE (vnode->decl))))))
	DECL_COMMON (vnode->decl) = 0;
    }
  FOR_EACH_DEFINED_VARIABLE (vnode)
    {
      if (!vnode->definition)
        continue;
      if (varpool_externally_visible_p (vnode))
	vnode->externally_visible = true;
      else
	{
          vnode->externally_visible = false;
	  vnode->forced_by_abi = false;
	}
      if (!vnode->externally_visible
	  && !vnode->weakref)
	{
	  gcc_assert (in_lto_p || whole_program || !TREE_PUBLIC (vnode->decl));
	  vnode->unique_name = ((vnode->resolution == LDPR_PREVAILING_DEF_IRONLY
				       || vnode->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP)
				       && TREE_PUBLIC (vnode->decl));
	  symtab_make_decl_local (vnode->decl);
	  if (vnode->same_comdat_group)
	    symtab_dissolve_same_comdat_group_list (vnode);
	  vnode->resolution = LDPR_PREVAILING_DEF_IRONLY;
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "\nMarking local functions:");
      FOR_EACH_DEFINED_FUNCTION (node)
	if (node->local.local)
	  fprintf (dump_file, " %s", node->name ());
      fprintf (dump_file, "\n\n");
      fprintf (dump_file, "\nMarking externally visible functions:");
      FOR_EACH_DEFINED_FUNCTION (node)
	if (node->externally_visible)
	  fprintf (dump_file, " %s", node->name ());
      fprintf (dump_file, "\n\n");
      fprintf (dump_file, "\nMarking externally visible variables:");
      FOR_EACH_DEFINED_VARIABLE (vnode)
	if (vnode->externally_visible)
	  fprintf (dump_file, " %s", vnode->name ());
      fprintf (dump_file, "\n\n");
    }
  cgraph_function_flags_ready = true;
  return 0;
}

/* Local function pass handling visibilities.  This happens before LTO streaming
   so in particular -fwhole-program should be ignored at this level.  */

static unsigned int
local_function_and_variable_visibility (void)
{
  return function_and_variable_visibility (flag_whole_program && !flag_lto);
}

namespace {

const pass_data pass_data_ipa_function_and_variable_visibility =
{
  SIMPLE_IPA_PASS, /* type */
  "visibility", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_CGRAPHOPT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_remove_functions | TODO_dump_symtab ), /* todo_flags_finish */
};

class pass_ipa_function_and_variable_visibility : public simple_ipa_opt_pass
{
public:
  pass_ipa_function_and_variable_visibility (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_function_and_variable_visibility,
			   ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute () {
    return local_function_and_variable_visibility ();
  }

}; // class pass_ipa_function_and_variable_visibility

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_function_and_variable_visibility (gcc::context *ctxt)
{
  return new pass_ipa_function_and_variable_visibility (ctxt);
}

/* Free inline summary.  */

static unsigned
free_inline_summary (void)
{
  inline_free_summary ();
  return 0;
}

namespace {

const pass_data pass_data_ipa_free_inline_summary =
{
  SIMPLE_IPA_PASS, /* type */
  "*free_inline_summary", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  false, /* has_gate */
  true, /* has_execute */
  TV_IPA_FREE_INLINE_SUMMARY, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_free_inline_summary : public simple_ipa_opt_pass
{
public:
  pass_ipa_free_inline_summary (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_free_inline_summary, ctxt)
  {}

  /* opt_pass methods: */
  unsigned int execute () { return free_inline_summary (); }

}; // class pass_ipa_free_inline_summary

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_free_inline_summary (gcc::context *ctxt)
{
  return new pass_ipa_free_inline_summary (ctxt);
}

/* Do not re-run on ltrans stage.  */

static bool
gate_whole_program_function_and_variable_visibility (void)
{
  return !flag_ltrans;
}

/* Bring functionss local at LTO time with -fwhole-program.  */

static unsigned int
whole_program_function_and_variable_visibility (void)
{
  function_and_variable_visibility (flag_whole_program);
  if (optimize)
    ipa_discover_readonly_nonaddressable_vars ();
  return 0;
}

namespace {

const pass_data pass_data_ipa_whole_program_visibility =
{
  IPA_PASS, /* type */
  "whole-program", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_CGRAPHOPT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_remove_functions | TODO_dump_symtab ), /* todo_flags_finish */
};

class pass_ipa_whole_program_visibility : public ipa_opt_pass_d
{
public:
  pass_ipa_whole_program_visibility (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_whole_program_visibility, ctxt,
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
  bool gate () {
    return gate_whole_program_function_and_variable_visibility ();
  }
  unsigned int execute () {
    return whole_program_function_and_variable_visibility ();
  }

}; // class pass_ipa_whole_program_visibility

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_whole_program_visibility (gcc::context *ctxt)
{
  return new pass_ipa_whole_program_visibility (ctxt);
}

/* Generate and emit a static constructor or destructor.  WHICH must
   be one of 'I' (for a constructor) or 'D' (for a destructor).  BODY
   is a STATEMENT_LIST containing GENERIC statements.  PRIORITY is the
   initialization priority for this constructor or destructor. 

   FINAL specify whether the externally visible name for collect2 should
   be produced. */

static void
cgraph_build_static_cdtor_1 (char which, tree body, int priority, bool final)
{
  static int counter = 0;
  char which_buf[16];
  tree decl, name, resdecl;

  /* The priority is encoded in the constructor or destructor name.
     collect2 will sort the names and arrange that they are called at
     program startup.  */
  if (final)
    sprintf (which_buf, "%c_%.5d_%d", which, priority, counter++);
  else
  /* Proudce sane name but one not recognizable by collect2, just for the
     case we fail to inline the function.  */
    sprintf (which_buf, "sub_%c_%.5d_%d", which, priority, counter++);
  name = get_file_function_name (which_buf);

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
  DECL_ARTIFICIAL (decl) = 1;
  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl) = 1;
  DECL_SAVED_TREE (decl) = body;
  if (!targetm.have_ctors_dtors && final)
    {
      TREE_PUBLIC (decl) = 1;
      DECL_PRESERVE_P (decl) = 1;
    }
  DECL_UNINLINABLE (decl) = 1;

  DECL_INITIAL (decl) = make_node (BLOCK);
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

  cgraph_add_new_function (decl, false);

  set_cfun (NULL);
  current_function_decl = NULL;
}

/* Generate and emit a static constructor or destructor.  WHICH must
   be one of 'I' (for a constructor) or 'D' (for a destructor).  BODY
   is a STATEMENT_LIST containing GENERIC statements.  PRIORITY is the
   initialization priority for this constructor or destructor.  */

void
cgraph_build_static_cdtor (char which, tree body, int priority)
{
  cgraph_build_static_cdtor_1 (which, body, priority, false);
}

/* A vector of FUNCTION_DECLs declared as static constructors.  */
static vec<tree> static_ctors;
/* A vector of FUNCTION_DECLs declared as static destructors.  */
static vec<tree> static_dtors;

/* When target does not have ctors and dtors, we call all constructor
   and destructor by special initialization/destruction function
   recognized by collect2.

   When we are going to build this function, collect all constructors and
   destructors and turn them into normal functions.  */

static void
record_cdtor_fn (struct cgraph_node *node)
{
  if (DECL_STATIC_CONSTRUCTOR (node->decl))
    static_ctors.safe_push (node->decl);
  if (DECL_STATIC_DESTRUCTOR (node->decl))
    static_dtors.safe_push (node->decl);
  node = cgraph_get_node (node->decl);
  DECL_DISREGARD_INLINE_LIMITS (node->decl) = 1;
}

/* Define global constructors/destructor functions for the CDTORS, of
   which they are LEN.  The CDTORS are sorted by initialization
   priority.  If CTOR_P is true, these are constructors; otherwise,
   they are destructors.  */

static void
build_cdtor (bool ctor_p, vec<tree> cdtors)
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
      cgraph_build_static_cdtor_1 (ctor_p ? 'I' : 'D', body, priority, true);
    }
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
    /* Ensure a stable sort.  */
    return DECL_UID (f1) - DECL_UID (f2);
}

/* Generate functions to call static constructors and destructors
   for targets that do not support .ctors/.dtors sections.  These
   functions have magic names which are detected by collect2.  */

static void
build_cdtor_fns (void)
{
  if (!static_ctors.is_empty ())
    {
      gcc_assert (!targetm.have_ctors_dtors || in_lto_p);
      static_ctors.qsort (compare_ctor);
      build_cdtor (/*ctor_p=*/true, static_ctors);
    }

  if (!static_dtors.is_empty ())
    {
      gcc_assert (!targetm.have_ctors_dtors || in_lto_p);
      static_dtors.qsort (compare_dtor);
      build_cdtor (/*ctor_p=*/false, static_dtors);
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
  struct cgraph_node *node;
  FOR_EACH_DEFINED_FUNCTION (node)
    if (DECL_STATIC_CONSTRUCTOR (node->decl)
	|| DECL_STATIC_DESTRUCTOR (node->decl))
       record_cdtor_fn (node);
  build_cdtor_fns ();
  static_ctors.release ();
  static_dtors.release ();
  return 0;
}

/* Perform the pass when we have no ctors/dtors support
   or at LTO time to merge multiple constructors into single
   function.  */

static bool
gate_ipa_cdtor_merge (void)
{
  return !targetm.have_ctors_dtors || (optimize && in_lto_p);
}

namespace {

const pass_data pass_data_ipa_cdtor_merge =
{
  IPA_PASS, /* type */
  "cdtor", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
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
  bool gate () { return gate_ipa_cdtor_merge (); }
  unsigned int execute () { return ipa_cdtor_merge (); }

}; // class pass_ipa_cdtor_merge

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_cdtor_merge (gcc::context *ctxt)
{
  return new pass_ipa_cdtor_merge (ctxt);
}
