/* Basic IPA optimizations and utilities.
   Copyright (C) 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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
#include "cgraph.h"
#include "tree-pass.h"
#include "timevar.h"
#include "gimple.h"
#include "ggc.h"
#include "flags.h"
#include "pointer-set.h"
#include "target.h"
#include "tree-iterator.h"
#include "ipa-utils.h"

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

/* Add cgraph NODE to queue starting at FIRST.

   The queue is linked via AUX pointers and terminated by pointer to 1.
   We enqueue nodes at two occasions: when we find them reachable or when we find
   their bodies needed for further clonning.  In the second case we mark them
   by pointer to 2 after processing so they are re-queue when they become
   reachable.  */

static void
enqueue_cgraph_node (struct cgraph_node *node, struct cgraph_node **first)
{
  /* Node is still in queue; do nothing.  */
  if (node->aux && node->aux != (void *) 2)
    return;
  /* Node was already processed as unreachable, re-enqueue
     only if it became reachable now.  */
  if (node->aux == (void *)2 && !node->reachable)
    return;
  node->aux = *first;
  *first = node;
}

/* Add varpool NODE to queue starting at FIRST.  */

static void
enqueue_varpool_node (struct varpool_node *node, struct varpool_node **first)
{
  node->aux = *first;
  *first = node;
}

/* Process references.  */

static void
process_references (struct ipa_ref_list *list,
		    struct cgraph_node **first,
		    struct varpool_node **first_varpool,
		    bool before_inlining_p)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_reference_iterate (list, i, ref); i++)
    {
      if (ref->refered_type == IPA_REF_CGRAPH)
	{
	  struct cgraph_node *node = ipa_ref_node (ref);
	  if (!node->reachable
	      && node->analyzed
	      && (!DECL_EXTERNAL (node->decl)
	          || before_inlining_p))
	    node->reachable = true;
	  enqueue_cgraph_node (node, first);
	}
      else
	{
	  struct varpool_node *node = ipa_ref_varpool_node (ref);
	  if (!node->needed)
	    {
	      varpool_mark_needed_node (node);
	      enqueue_varpool_node (node, first_varpool);
	    }
	}
    }
}


/* Return true when NODE can not be local. Worker for cgraph_local_node_p.  */

static bool
cgraph_non_local_node_p_1 (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
   /* FIXME: Aliases can be local, but i386 gets thunks wrong then.  */
   return !(cgraph_only_called_directly_or_aliased_p (node)
	    && !ipa_ref_has_aliases_p (&node->ref_list)
	    && node->analyzed
	    && !DECL_EXTERNAL (node->decl)
	    && !node->local.externally_visible
	    && !node->reachable_from_other_partition
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

  for (i = 0; ipa_ref_list_refering_iterate (&node->ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ADDR)
      return true;
  return false;
}

/* Perform reachability analysis and reclaim all unreachable nodes.
   If BEFORE_INLINING_P is true this function is called before inlining
   decisions has been made.  If BEFORE_INLINING_P is false this function also
   removes unneeded bodies of extern inline functions.  */

bool
cgraph_remove_unreachable_nodes (bool before_inlining_p, FILE *file)
{
  struct cgraph_node *first = (struct cgraph_node *) (void *) 1;
  struct varpool_node *first_varpool = (struct varpool_node *) (void *) 1;
  struct cgraph_node *node, *next;
  struct varpool_node *vnode, *vnext;
  bool changed = false;

#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif
  if (file)
    fprintf (file, "\nReclaiming functions:");
#ifdef ENABLE_CHECKING
  for (node = cgraph_nodes; node; node = node->next)
    gcc_assert (!node->aux);
  for (vnode = varpool_nodes; vnode; vnode = vnode->next)
    gcc_assert (!vnode->aux);
#endif
  varpool_reset_queue ();
  /* Mark functions whose bodies are obviously needed.
     This is mostly when they can be referenced externally.  Inline clones
     are special since their declarations are shared with master clone and thus
     cgraph_can_remove_if_no_direct_calls_and_refs_p should not be called on them.  */
  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed && !node->global.inlined_to
	&& (!cgraph_can_remove_if_no_direct_calls_and_refs_p (node)
	    /* Keep around virtual functions for possible devirtualization.  */
	    || (before_inlining_p
		&& DECL_VIRTUAL_P (node->decl)
		&& (DECL_COMDAT (node->decl) || DECL_EXTERNAL (node->decl)))))
      {
        gcc_assert (!node->global.inlined_to);
	enqueue_cgraph_node (node, &first);
	node->reachable = true;
      }
    else
      {
        gcc_assert (!node->aux);
	node->reachable = false;
      }

  /* Mark variables that are obviously needed.  */
  for (vnode = varpool_nodes; vnode; vnode = vnode->next)
    {
      vnode->next_needed = NULL;
      vnode->prev_needed = NULL;
      if ((vnode->analyzed || vnode->force_output)
	  && !varpool_can_remove_if_no_refs (vnode))
	{
	  vnode->needed = false;
	  varpool_mark_needed_node (vnode);
	  enqueue_varpool_node (vnode, &first_varpool);
	}
      else
	vnode->needed = false;
    }

  /* Perform reachability analysis.  As a special case do not consider
     extern inline functions not inlined as live because we won't output
     them at all. 

     We maintain two worklist, one for cgraph nodes other for varpools and
     are finished once both are empty.  */

  while (first != (struct cgraph_node *) (void *) 1
  	 || first_varpool != (struct varpool_node *) (void *) 1)
    {
      if (first != (struct cgraph_node *) (void *) 1)
	{
	  struct cgraph_edge *e;
	  node = first;
	  first = (struct cgraph_node *) first->aux;
	  if (!node->reachable)
	    node->aux = (void *)2;

	  /* If we found this node reachable, first mark on the callees
	     reachable too, unless they are direct calls to extern inline functions
	     we decided to not inline.  */
	  if (node->reachable)
	    {
	      for (e = node->callees; e; e = e->next_callee)
		{
		  if (!e->callee->reachable
		      && node->analyzed
		      && (!e->inline_failed
			  || !DECL_EXTERNAL (e->callee->decl)
			  || before_inlining_p))
		    e->callee->reachable = true;
		  enqueue_cgraph_node (e->callee, &first);
		}
	      process_references (&node->ref_list, &first, &first_varpool, before_inlining_p);
	    }

	  /* If any function in a comdat group is reachable, force
	     all other functions in the same comdat group to be
	     also reachable.  */
	  if (node->same_comdat_group
	      && node->reachable
	      && !node->global.inlined_to)
	    {
	      for (next = node->same_comdat_group;
		   next != node;
		   next = next->same_comdat_group)
		if (!next->reachable)
		  {
		    next->reachable = true;
		    enqueue_cgraph_node (next, &first);
		  }
	    }

	  /* We can freely remove inline clones even if they are cloned, however if
	     function is clone of real clone, we must keep it around in order to
	     make materialize_clones produce function body with the changes
	     applied.  */
	  while (node->clone_of && !node->clone_of->aux
	         && !gimple_has_body_p (node->decl))
	    {
	      bool noninline = node->clone_of->decl != node->decl;
	      node = node->clone_of;
	      if (noninline && !node->reachable && !node->aux)
	      	{
		  enqueue_cgraph_node (node, &first);
		  break;
		}
	    }
	}
      if (first_varpool != (struct varpool_node *) (void *) 1)
	{
	  vnode = first_varpool;
	  first_varpool = (struct varpool_node *)first_varpool->aux;
	  vnode->aux = NULL;
	  process_references (&vnode->ref_list, &first, &first_varpool, before_inlining_p);
	  /* If any function in a comdat group is reachable, force
	     all other functions in the same comdat group to be
	     also reachable.  */
	  if (vnode->same_comdat_group)
	    {
	      struct varpool_node *next;
	      for (next = vnode->same_comdat_group;
		   next != vnode;
		   next = next->same_comdat_group)
		if (!next->needed)
		  {
		    varpool_mark_needed_node (next);
		    enqueue_varpool_node (next, &first_varpool);
		  }
	    }
	}
    }

  /* Remove unreachable nodes. 

     Completely unreachable functions can be fully removed from the callgraph.
     Extern inline functions that we decided to not inline need to become unanalyzed nodes of
     callgraph (so we still have edges to them).  We remove function body then.

     Also we need to care functions that are unreachable but we need to keep them around
     for later clonning.  In this case we also turn them to unanalyzed nodes, but
     keep the body around.  */
  for (node = cgraph_nodes; node; node = next)
    {
      next = node->next;
      if (node->aux && !node->reachable)
        {
	  cgraph_node_remove_callees (node);
	  ipa_remove_all_references (&node->ref_list);
	  node->analyzed = false;
	}
      if (!node->aux)
	{
	  struct cgraph_edge *e;
	  bool found = false;
	  int i;
	  struct ipa_ref *ref;

          node->global.inlined_to = NULL;
	  if (file)
	    fprintf (file, " %s", cgraph_node_name (node));
	  /* See if there is reachable caller.  */
	  for (e = node->callers; e && !found; e = e->next_caller)
	    if (e->caller->reachable)
	      found = true;
	  for (i = 0; (ipa_ref_list_refering_iterate (&node->ref_list, i, ref)
		       && !found); i++)
	    if (ref->refering_type == IPA_REF_CGRAPH
		&& ipa_ref_refering_node (ref)->reachable)
	      found = true;
	    else if (ref->refering_type == IPA_REF_VARPOOL
		     && ipa_ref_refering_varpool_node (ref)->needed)
	      found = true;

	  /* If so, we need to keep node in the callgraph.  */
	  if (found)
	    {
	      if (node->analyzed)
		{
		  struct cgraph_node *clone;

		  /* If there are still clones, we must keep body around.
		     Otherwise we can just remove the body but keep the clone.  */
		  for (clone = node->clones; clone;
		       clone = clone->next_sibling_clone)
		    if (clone->aux)
		      break;
		  if (!clone)
		    {
		      cgraph_release_function_body (node);
		      if (node->prev_sibling_clone)
			node->prev_sibling_clone->next_sibling_clone = node->next_sibling_clone;
		      else if (node->clone_of)
			node->clone_of->clones = node->next_sibling_clone;
		      if (node->next_sibling_clone)
			node->next_sibling_clone->prev_sibling_clone = node->prev_sibling_clone;
		      if (node->clone_of)
			node->former_clone_of = node->clone_of->decl;
		      node->clone_of = NULL;
		      node->next_sibling_clone = NULL;
		      node->prev_sibling_clone = NULL;
		    }
		  else
		    gcc_assert (!clone->in_other_partition);
		  node->analyzed = false;
		  changed = true;
		  cgraph_node_remove_callees (node);
		  ipa_remove_all_references (&node->ref_list);
		}
	    }
	  else
	    {
	      cgraph_remove_node (node);
	      changed = true;
	    }
	}
    }
  for (node = cgraph_nodes; node; node = node->next)
    {
      /* Inline clones might be kept around so their materializing allows further
         cloning.  If the function the clone is inlined into is removed, we need
         to turn it into normal cone.  */
      if (node->global.inlined_to
	  && !node->callers)
	{
	  gcc_assert (node->clones);
	  node->global.inlined_to = NULL;
	  update_inlined_to_pointer (node, node);
	}
      node->aux = NULL;
    }

  if (file)
    fprintf (file, "\n");

  /* We must release unused extern inlines or sanity checking will fail.  Rest of transformations
     are undesirable at -O0 since we do not want to remove anything.  */
  if (!optimize)
    return changed;

  if (file)
    fprintf (file, "Reclaiming variables:");
  for (vnode = varpool_nodes; vnode; vnode = vnext)
    {
      vnext = vnode->next;
      if (!vnode->needed)
        {
	  if (file)
	    fprintf (file, " %s", varpool_node_name (vnode));
	  varpool_remove_node (vnode);
	  changed = true;
	}
    }

  /* Now update address_taken flags and try to promote functions to be local.  */

  if (file)
    fprintf (file, "\nClearing address taken flags:");
  for (node = cgraph_nodes; node; node = node->next)
    if (node->address_taken
	&& !node->reachable_from_other_partition)
      {
	if (!cgraph_for_node_and_aliases (node, has_addr_references_p, NULL, true))
	  {
	    if (file)
	      fprintf (file, " %s", cgraph_node_name (node));
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
  verify_cgraph ();
#endif

  /* Reclaim alias pairs for functions that have disappeared from the
     call graph.  */
  remove_unreachable_alias_pairs ();

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
  struct varpool_node *vnode;
  if (dump_file)
    fprintf (dump_file, "Clearing variable flags:");
  for (vnode = varpool_nodes; vnode; vnode = vnode->next)
    if (vnode->finalized && varpool_all_refs_explicit_p (vnode)
	&& (TREE_ADDRESSABLE (vnode->decl) || !TREE_READONLY (vnode->decl)))
      {
	bool written = false;
	bool address_taken = false;
	int i;
        struct ipa_ref *ref;
        for (i = 0; ipa_ref_list_refering_iterate (&vnode->ref_list, i, ref)
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
	      fprintf (dump_file, " %s (addressable)", varpool_node_name (vnode));
	    TREE_ADDRESSABLE (vnode->decl) = 0;
	  }
	if (!TREE_READONLY (vnode->decl) && !address_taken && !written
	    /* Making variable in explicit section readonly can cause section
	       type conflict. 
	       See e.g. gcc.c-torture/compile/pr23237.c */
	    && DECL_SECTION_NAME (vnode->decl) == NULL)
	  {
	    if (dump_file)
	      fprintf (dump_file, " %s (read-only)", varpool_node_name (vnode));
	    TREE_READONLY (vnode->decl) = 1;
	  }
      }
  if (dump_file)
    fprintf (dump_file, "\n");
}

/* Return true when there is a reference to node and it is not vtable.  */
static bool
cgraph_address_taken_from_non_vtable_p (struct cgraph_node *node)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_refering_iterate (&node->ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ADDR)
      {
	struct varpool_node *node;
	if (ref->refering_type == IPA_REF_CGRAPH)
	  return true;
	node = ipa_ref_refering_varpool_node (ref);
	if (!DECL_VIRTUAL_P (node->decl))
	  return true;
      }
  return false;
}

/* COMDAT functions must be shared only if they have address taken,
   otherwise we can produce our own private implementation with
   -fwhole-program.  
   Return true when turning COMDAT functoin static can not lead to wrong
   code when the resulting object links with a library defining same COMDAT.

   Virtual functions do have their addresses taken from the vtables,
   but in C++ there is no way to compare their addresses for equality.  */

bool
cgraph_comdat_can_be_unshared_p (struct cgraph_node *node)
{
  if ((cgraph_address_taken_from_non_vtable_p (node)
       && !DECL_VIRTUAL_P (node->decl))
      || !node->analyzed)
    return false;
  if (node->same_comdat_group)
    {
      struct cgraph_node *next;

      /* If more than one function is in the same COMDAT group, it must
         be shared even if just one function in the comdat group has
         address taken.  */
      for (next = node->same_comdat_group;
	   next != node; next = next->same_comdat_group)
	if (cgraph_address_taken_from_non_vtable_p (next)
	    && !DECL_VIRTUAL_P (next->decl))
	  return false;
    }
  return true;
}

/* Return true when function NODE should be considered externally visible.  */

static bool
cgraph_externally_visible_p (struct cgraph_node *node,
			     bool whole_program, bool aliased)
{
  if (!node->local.finalized)
    return false;
  if (!DECL_COMDAT (node->decl)
      && (!TREE_PUBLIC (node->decl) || DECL_EXTERNAL (node->decl)))
    return false;

  /* Do not even try to be smart about aliased nodes.  Until we properly
     represent everything by same body alias, these are just evil.  */
  if (aliased)
    return true;

  /* Do not try to localize built-in functions yet.  One of problems is that we
     end up mangling their asm for WHOPR that makes it impossible to call them
     using the implicit built-in declarations anymore.  Similarly this enables
     us to remove them as unreachable before actual calls may appear during
     expansion or folding.  */
  if (DECL_BUILT_IN (node->decl))
    return true;

  /* If linker counts on us, we must preserve the function.  */
  if (cgraph_used_from_object_file_p (node))
    return true;
  if (DECL_PRESERVE_P (node->decl))
    return true;
  if (lookup_attribute ("externally_visible", DECL_ATTRIBUTES (node->decl)))
    return true;
  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
      && lookup_attribute ("dllexport", DECL_ATTRIBUTES (node->decl)))
    return true;
  if (node->resolution == LDPR_PREVAILING_DEF_IRONLY)
    return false;
  /* When doing LTO or whole program, we can bring COMDAT functoins static.
     This improves code quality and we know we will duplicate them at most twice
     (in the case that we are not using plugin and link with object file
      implementing same COMDAT)  */
  if ((in_lto_p || whole_program)
      && DECL_COMDAT (node->decl)
      && cgraph_comdat_can_be_unshared_p (node))
    return false;

  /* When doing link time optimizations, hidden symbols become local.  */
  if (in_lto_p
      && (DECL_VISIBILITY (node->decl) == VISIBILITY_HIDDEN
	  || DECL_VISIBILITY (node->decl) == VISIBILITY_INTERNAL)
      /* Be sure that node is defined in IR file, not in other object
	 file.  In that case we don't set used_from_other_object_file.  */
      && node->analyzed)
    ;
  else if (!whole_program)
    return true;

  if (MAIN_NAME_P (DECL_NAME (node->decl)))
    return true;

  return false;
}

/* Return true when variable VNODE should be considered externally visible.  */

bool
varpool_externally_visible_p (struct varpool_node *vnode, bool aliased)
{
  if (!DECL_COMDAT (vnode->decl) && !TREE_PUBLIC (vnode->decl))
    return false;

  /* Do not even try to be smart about aliased nodes.  Until we properly
     represent everything by same body alias, these are just evil.  */
  if (aliased)
    return true;

  /* If linker counts on us, we must preserve the function.  */
  if (varpool_used_from_object_file_p (vnode))
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
  if (varpool_used_from_object_file_p (vnode))
    return true;
  if (vnode->resolution == LDPR_PREVAILING_DEF_IRONLY)
    return false;

  /* As a special case, the COMDAT virutal tables can be unshared.
     In LTO mode turn vtables into static variables.  The variable is readonly,
     so this does not enable more optimization, but referring static var
     is faster for dynamic linking.  Also this match logic hidding vtables
     from LTO symbol tables.  */
  if ((in_lto_p || flag_whole_program)
      && !vnode->force_output
      && DECL_COMDAT (vnode->decl) && DECL_VIRTUAL_P (vnode->decl))
    return false;

  /* When doing link time optimizations, hidden symbols become local.  */
  if (in_lto_p
      && (DECL_VISIBILITY (vnode->decl) == VISIBILITY_HIDDEN
	  || DECL_VISIBILITY (vnode->decl) == VISIBILITY_INTERNAL)
      /* Be sure that node is defined in IR file, not in other object
	 file.  In that case we don't set used_from_other_object_file.  */
      && vnode->finalized)
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

/* Dissolve the same_comdat_group list in which NODE resides.  */

static void
dissolve_same_comdat_group_list (struct cgraph_node *node)
{
  struct cgraph_node *n = node, *next;
  do
    {
      next = n->same_comdat_group;
      n->same_comdat_group = NULL;
      n = next;
    }
  while (n != node);
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
  struct varpool_node *vnode;
  struct pointer_set_t *aliased_nodes = pointer_set_create ();
  struct pointer_set_t *aliased_vnodes = pointer_set_create ();
  unsigned i;
  alias_pair *p;

  /* Discover aliased nodes.  */
  FOR_EACH_VEC_ELT (alias_pair, alias_pairs, i, p)
    {
      if (dump_file)
       fprintf (dump_file, "Alias %s->%s",
		IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (p->decl)),
		IDENTIFIER_POINTER (p->target));
		
      if ((node = cgraph_node_for_asm (p->target)) != NULL
	  && !DECL_EXTERNAL (node->decl))
        {
	  if (!node->analyzed)
	    continue;
	  cgraph_mark_needed_node (node);
	  gcc_assert (node->needed);
	  pointer_set_insert (aliased_nodes, node);
	  if (dump_file)
	    fprintf (dump_file, "  node %s/%i",
		     cgraph_node_name (node), node->uid);
        }
      else if ((vnode = varpool_node_for_asm (p->target)) != NULL
	       && !DECL_EXTERNAL (vnode->decl))
        {
	  varpool_mark_needed_node (vnode);
	  gcc_assert (vnode->needed);
	  pointer_set_insert (aliased_vnodes, vnode);
	  if (dump_file)
	    fprintf (dump_file, "  varpool node %s",
		     varpool_node_name (vnode));
        }
      if (dump_file)
       fprintf (dump_file, "\n");
    }

  for (node = cgraph_nodes; node; node = node->next)
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
      if (node->needed
	  && (DECL_EXTERNAL (node->decl) || !node->analyzed))
	node->needed = 0;

      /* C++ FE on lack of COMDAT support create local COMDAT functions
	 (that ought to be shared but can not due to object format
	 limitations).  It is neccesary to keep the flag to make rest of C++ FE
	 happy.  Clear the flag here to avoid confusion in middle-end.  */
      if (DECL_COMDAT (node->decl) && !TREE_PUBLIC (node->decl))
        DECL_COMDAT (node->decl) = 0;
      /* For external decls stop tracking same_comdat_group, it doesn't matter
	 what comdat group they are in when they won't be emitted in this TU,
	 and simplifies later passes.  */
      if (node->same_comdat_group && DECL_EXTERNAL (node->decl))
	{
#ifdef ENABLE_CHECKING
	  struct cgraph_node *n;

	  for (n = node->same_comdat_group;
	       n != node;
	       n = n->same_comdat_group)
	      /* If at least one of same comdat group functions is external,
		 all of them have to be, otherwise it is a front-end bug.  */
	      gcc_assert (DECL_EXTERNAL (n->decl));
#endif
	  dissolve_same_comdat_group_list (node);
	}
      gcc_assert ((!DECL_WEAK (node->decl) && !DECL_COMDAT (node->decl))
      	          || TREE_PUBLIC (node->decl) || DECL_EXTERNAL (node->decl));
      if (cgraph_externally_visible_p (node, whole_program,
				       pointer_set_contains (aliased_nodes,
							     node)))
        {
	  gcc_assert (!node->global.inlined_to);
	  node->local.externally_visible = true;
	}
      else
	node->local.externally_visible = false;
      if (!node->local.externally_visible && node->analyzed
	  && !DECL_EXTERNAL (node->decl))
	{
	  gcc_assert (whole_program || in_lto_p || !TREE_PUBLIC (node->decl));
	  cgraph_make_decl_local (node->decl);
	  node->resolution = LDPR_PREVAILING_DEF_IRONLY;
	  if (node->same_comdat_group)
	    /* cgraph_externally_visible_p has already checked all other nodes
	       in the group and they will all be made local.  We need to
	       dissolve the group at once so that the predicate does not
	       segfault though. */
	    dissolve_same_comdat_group_list (node);
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
    }
  for (node = cgraph_nodes; node; node = node->next)
    node->local.local = cgraph_local_node_p (node);
  for (vnode = varpool_nodes; vnode; vnode = vnode->next)
    {
      /* weak flag makes no sense on local variables.  */
      gcc_assert (!DECL_WEAK (vnode->decl)
      		  || TREE_PUBLIC (vnode->decl) || DECL_EXTERNAL (vnode->decl));
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
	  && (!(TREE_PUBLIC (vnode->decl) || DECL_EXTERNAL (vnode->decl))
	      || (DECL_INITIAL (vnode->decl)
		  && DECL_INITIAL (vnode->decl) != error_mark_node)
	      || DECL_WEAK (vnode->decl)
	      || DECL_SECTION_NAME (vnode->decl) != NULL
	      || ! (ADDR_SPACE_GENERIC_P
		    (TYPE_ADDR_SPACE (TREE_TYPE (vnode->decl))))))
	DECL_COMMON (vnode->decl) = 0;
    }
  for (vnode = varpool_nodes_queue; vnode; vnode = vnode->next_needed)
    {
      if (!vnode->finalized)
        continue;
      if (vnode->needed
	  && varpool_externally_visible_p
	      (vnode, 
	       pointer_set_contains (aliased_vnodes, vnode)))
	vnode->externally_visible = true;
      else
        vnode->externally_visible = false;
      if (!vnode->externally_visible)
	{
	  gcc_assert (in_lto_p || whole_program || !TREE_PUBLIC (vnode->decl));
	  cgraph_make_decl_local (vnode->decl);
	  vnode->resolution = LDPR_PREVAILING_DEF_IRONLY;
	}
     gcc_assert (TREE_STATIC (vnode->decl));
    }
  pointer_set_destroy (aliased_nodes);
  pointer_set_destroy (aliased_vnodes);

  if (dump_file)
    {
      fprintf (dump_file, "\nMarking local functions:");
      for (node = cgraph_nodes; node; node = node->next)
	if (node->local.local)
	  fprintf (dump_file, " %s", cgraph_node_name (node));
      fprintf (dump_file, "\n\n");
      fprintf (dump_file, "\nMarking externally visible functions:");
      for (node = cgraph_nodes; node; node = node->next)
	if (node->local.externally_visible)
	  fprintf (dump_file, " %s", cgraph_node_name (node));
      fprintf (dump_file, "\n\n");
      fprintf (dump_file, "\nMarking externally visible variables:");
      for (vnode = varpool_nodes_queue; vnode; vnode = vnode->next_needed)
	if (vnode->externally_visible)
	  fprintf (dump_file, " %s", varpool_node_name (vnode));
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

struct simple_ipa_opt_pass pass_ipa_function_and_variable_visibility =
{
 {
  SIMPLE_IPA_PASS,
  "visibility",				/* name */
  NULL,					/* gate */
  local_function_and_variable_visibility,/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_CGRAPHOPT,				/* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_remove_functions | TODO_dump_cgraph
  | TODO_ggc_collect			/* todo_flags_finish */
 }
};

/* Do not re-run on ltrans stage.  */

static bool
gate_whole_program_function_and_variable_visibility (void)
{
  return !flag_ltrans;
}

/* Bring functionss local at LTO time whith -fwhole-program.  */

static unsigned int
whole_program_function_and_variable_visibility (void)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;

  function_and_variable_visibility (flag_whole_program);

  for (node = cgraph_nodes; node; node = node->next)
    if ((node->local.externally_visible && !DECL_COMDAT (node->decl))
        && node->local.finalized)
      cgraph_mark_needed_node (node);
  for (vnode = varpool_nodes_queue; vnode; vnode = vnode->next_needed)
    if (vnode->externally_visible && !DECL_COMDAT (vnode->decl))
      varpool_mark_needed_node (vnode);
  if (dump_file)
    {
      fprintf (dump_file, "\nNeeded variables:");
      for (vnode = varpool_nodes_queue; vnode; vnode = vnode->next_needed)
	if (vnode->needed)
	  fprintf (dump_file, " %s", varpool_node_name (vnode));
      fprintf (dump_file, "\n\n");
    }
  if (optimize)
    ipa_discover_readonly_nonaddressable_vars ();
  return 0;
}

struct ipa_opt_pass_d pass_ipa_whole_program_visibility =
{
 {
  IPA_PASS,
  "whole-program",			/* name */
  gate_whole_program_function_and_variable_visibility,/* gate */
  whole_program_function_and_variable_visibility,/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_CGRAPHOPT,				/* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_remove_functions | TODO_dump_cgraph
  | TODO_ggc_collect			/* todo_flags_finish */
 },
 NULL,					/* generate_summary */
 NULL,					/* write_summary */
 NULL,					/* read_summary */
 NULL,					/* write_optimization_summary */
 NULL,					/* read_optimization_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,					/* function_transform */
 NULL,					/* variable_transform */
};


/* Simple ipa profile pass propagating frequencies across the callgraph.  */

static unsigned int
ipa_profile (void)
{
  struct cgraph_node **order = XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  struct cgraph_edge *e;
  int order_pos;
  bool something_changed = false;
  int i;

  order_pos = ipa_reverse_postorder (order);
  for (i = order_pos - 1; i >= 0; i--)
    {
      if (order[i]->local.local && cgraph_propagate_frequency (order[i]))
	{
	  for (e = order[i]->callees; e; e = e->next_callee)
	    if (e->callee->local.local && !e->callee->aux)
	      {
	        something_changed = true;
	        e->callee->aux = (void *)1;
	      }
	}
      order[i]->aux = NULL;
    }

  while (something_changed)
    {
      something_changed = false;
      for (i = order_pos - 1; i >= 0; i--)
	{
	  if (order[i]->aux && cgraph_propagate_frequency (order[i]))
	    {
	      for (e = order[i]->callees; e; e = e->next_callee)
		if (e->callee->local.local && !e->callee->aux)
		  {
		    something_changed = true;
		    e->callee->aux = (void *)1;
		  }
	    }
	  order[i]->aux = NULL;
	}
    }
  free (order);
  return 0;
}

static bool
gate_ipa_profile (void)
{
  return flag_ipa_profile;
}

struct ipa_opt_pass_d pass_ipa_profile =
{
 {
  IPA_PASS,
  "profile_estimate",			/* name */
  gate_ipa_profile,			/* gate */
  ipa_profile,			        /* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_PROFILE,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                                     /* todo_flags_finish */
 },
 NULL,				        /* generate_summary */
 NULL,					/* write_summary */
 NULL,					/* read_summary */
 NULL,					/* write_optimization_summary */
 NULL,					/* read_optimization_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};

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
static VEC(tree, heap) *static_ctors;
/* A vector of FUNCTION_DECLs declared as static destructors.  */
static VEC(tree, heap) *static_dtors;

/* When target does not have ctors and dtors, we call all constructor
   and destructor by special initialization/destruction function
   recognized by collect2.

   When we are going to build this function, collect all constructors and
   destructors and turn them into normal functions.  */

static void
record_cdtor_fn (struct cgraph_node *node)
{
  if (DECL_STATIC_CONSTRUCTOR (node->decl))
    VEC_safe_push (tree, heap, static_ctors, node->decl);
  if (DECL_STATIC_DESTRUCTOR (node->decl))
    VEC_safe_push (tree, heap, static_dtors, node->decl);
  node = cgraph_get_node (node->decl);
  DECL_DISREGARD_INLINE_LIMITS (node->decl) = 1;
}

/* Define global constructors/destructor functions for the CDTORS, of
   which they are LEN.  The CDTORS are sorted by initialization
   priority.  If CTOR_P is true, these are constructors; otherwise,
   they are destructors.  */

static void
build_cdtor (bool ctor_p, VEC (tree, heap) *cdtors)
{
  size_t i,j;
  size_t len = VEC_length (tree, cdtors);

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
	  fn = VEC_index (tree, cdtors, j);
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
	  fn = VEC_index (tree, cdtors, i);
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
  if (!VEC_empty (tree, static_ctors))
    {
      gcc_assert (!targetm.have_ctors_dtors || in_lto_p);
      VEC_qsort (tree, static_ctors, compare_ctor);
      build_cdtor (/*ctor_p=*/true, static_ctors);
    }

  if (!VEC_empty (tree, static_dtors))
    {
      gcc_assert (!targetm.have_ctors_dtors || in_lto_p);
      VEC_qsort (tree, static_dtors, compare_dtor);
      build_cdtor (/*ctor_p=*/false, static_dtors);
    }
}

/* Look for constructors and destructors and produce function calling them.
   This is needed for targets not supporting ctors or dtors, but we perform the
   transformation also at linktime to merge possibly numberous
   constructors/destructors into single function to improve code locality and
   reduce size.  */

static unsigned int
ipa_cdtor_merge (void)
{
  struct cgraph_node *node;
  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed
	&& (DECL_STATIC_CONSTRUCTOR (node->decl)
	    || DECL_STATIC_DESTRUCTOR (node->decl)))
       record_cdtor_fn (node);
  build_cdtor_fns ();
  VEC_free (tree, heap, static_ctors);
  VEC_free (tree, heap, static_dtors);
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

struct ipa_opt_pass_d pass_ipa_cdtor_merge =
{
 {
  IPA_PASS,
  "cdtor",				/* name */
  gate_ipa_cdtor_merge,			/* gate */
  ipa_cdtor_merge,		        /* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_CGRAPHOPT,			        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0                                     /* todo_flags_finish */
 },
 NULL,				        /* generate_summary */
 NULL,					/* write_summary */
 NULL,					/* read_summary */
 NULL,					/* write_optimization_summary */
 NULL,					/* read_optimization_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};
