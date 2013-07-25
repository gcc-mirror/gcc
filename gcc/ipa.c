/* Basic IPA optimizations and utilities.
   Copyright (C) 2003-2013 Free Software Foundation, Inc.

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
#include "gimple.h"
#include "ggc.h"
#include "flags.h"
#include "pointer-set.h"
#include "target.h"
#include "tree-iterator.h"
#include "ipa-utils.h"
#include "pointer-set.h"
#include "ipa-inline.h"
#include "hash-table.h"
#include "tree-inline.h"
#include "profile.h"
#include "params.h"
#include "lto-streamer.h"
#include "data-streamer.h"

/* Return true when NODE can not be local. Worker for cgraph_local_node_p.  */

static bool
cgraph_non_local_node_p_1 (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
   /* FIXME: Aliases can be local, but i386 gets thunks wrong then.  */
   return !(cgraph_only_called_directly_or_aliased_p (node)
	    && !ipa_ref_has_aliases_p (&node->symbol.ref_list)
	    && node->symbol.definition
	    && !DECL_EXTERNAL (node->symbol.decl)
	    && !node->symbol.externally_visible
	    && !node->symbol.used_from_other_partition
	    && !node->symbol.in_other_partition);
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

  for (i = 0; ipa_ref_list_referring_iterate (&node->symbol.ref_list,
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
enqueue_node (symtab_node node, symtab_node *first,
	      struct pointer_set_t *reachable)
{
  /* Node is still in queue; do nothing.  */
  if (node->symbol.aux && node->symbol.aux != (void *) 2)
    return;
  /* Node was already processed as unreachable, re-enqueue
     only if it became reachable now.  */
  if (node->symbol.aux == (void *)2 && !pointer_set_contains (reachable, node))
    return;
  node->symbol.aux = *first;
  *first = node;
}

/* Process references.  */

static void
process_references (struct ipa_ref_list *list,
		    symtab_node *first,
		    bool before_inlining_p,
		    struct pointer_set_t *reachable)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_reference_iterate (list, i, ref); i++)
    {
      symtab_node node = ref->referred;

      if (node->symbol.definition && !node->symbol.in_other_partition
	  && ((!DECL_EXTERNAL (node->symbol.decl) || node->symbol.alias)
	      || (before_inlining_p
		  /* We use variable constructors during late complation for
		     constant folding.  Keep references alive so partitioning
		     knows about potential references.  */
		  || (TREE_CODE (node->symbol.decl) == VAR_DECL
		      && flag_wpa
		      && ctor_for_folding (node->symbol.decl)
		         != error_mark_node))))
	pointer_set_insert (reachable, node);
      enqueue_node ((symtab_node) node, first, reachable);
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
     devirtualization may happen, but it is not importnat since we won't inline
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

   A the end we keep all reachable symbols. For symbols in boundary we always
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
  symtab_node first = (symtab_node) (void *) 1;
  struct cgraph_node *node, *next;
  struct varpool_node *vnode, *vnext;
  bool changed = false;
  struct pointer_set_t *reachable = pointer_set_create ();
  struct pointer_set_t *body_needed_for_clonning = pointer_set_create ();

#ifdef ENABLE_CHECKING
  verify_symtab ();
#endif
  if (file)
    fprintf (file, "\nReclaiming functions:");
#ifdef ENABLE_CHECKING
  FOR_EACH_FUNCTION (node)
    gcc_assert (!node->symbol.aux);
  FOR_EACH_VARIABLE (vnode)
    gcc_assert (!vnode->symbol.aux);
#endif
  /* Mark functions whose bodies are obviously needed.
     This is mostly when they can be referenced externally.  Inline clones
     are special since their declarations are shared with master clone and thus
     cgraph_can_remove_if_no_direct_calls_and_refs_p should not be called on them.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    if (!node->global.inlined_to
	&& !node->symbol.in_other_partition
	&& (!cgraph_can_remove_if_no_direct_calls_and_refs_p (node)
	    /* Keep around virtual functions for possible devirtualization.  */
	    || (before_inlining_p
		&& DECL_VIRTUAL_P (node->symbol.decl))))
      {
        gcc_assert (!node->global.inlined_to);
	pointer_set_insert (reachable, node);
	enqueue_node ((symtab_node)node, &first, reachable);
      }
    else
      gcc_assert (!node->symbol.aux);

  /* Mark variables that are obviously needed.  */
  FOR_EACH_DEFINED_VARIABLE (vnode)
    if (!varpool_can_remove_if_no_refs (vnode)
	&& !vnode->symbol.in_other_partition)
      {
	pointer_set_insert (reachable, vnode);
	enqueue_node ((symtab_node)vnode, &first, reachable);
      }

  /* Perform reachability analysis.  */
  while (first != (symtab_node) (void *) 1)
    {
      bool in_boundary_p = !pointer_set_contains (reachable, first);
      symtab_node node = first;

      first = (symtab_node)first->symbol.aux;

      /* If we are processing symbol in boundary, mark its AUX pointer for
	 possible later re-processing in enqueue_node.  */
      if (in_boundary_p)
	node->symbol.aux = (void *)2;
      else
	{
	  /* If any symbol in a comdat group is reachable, force
	     all other in the same comdat group to be also reachable.  */
	  if (node->symbol.same_comdat_group)
	    {
	      symtab_node next;
	      for (next = node->symbol.same_comdat_group;
		   next != node;
		   next = next->symbol.same_comdat_group)
		if (!pointer_set_insert (reachable, next))
		  enqueue_node ((symtab_node) next, &first, reachable);
	    }
	  /* Mark references as reachable.  */
	  process_references (&node->symbol.ref_list, &first,
			      before_inlining_p, reachable);
	}

      if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
	{
	  /* Mark the callees reachable unless they are direct calls to extern
 	     inline functions we decided to not inline.  */
	  if (!in_boundary_p)
	    {
	      struct cgraph_edge *e;
	      for (e = cnode->callees; e; e = e->next_callee)
		{
		  if (e->callee->symbol.definition
		      && !e->callee->symbol.in_other_partition
		      && (!e->inline_failed
			  || !DECL_EXTERNAL (e->callee->symbol.decl)
			  || e->callee->symbol.alias
			  || before_inlining_p))
		    pointer_set_insert (reachable, e->callee);
		  enqueue_node ((symtab_node) e->callee, &first, reachable);
		}

	      /* When inline clone exists, mark body to be preserved so when removing
		 offline copy of the function we don't kill it.  */
	      if (cnode->global.inlined_to)
	        pointer_set_insert (body_needed_for_clonning, cnode->symbol.decl);

	      /* For non-inline clones, force their origins to the boundary and ensure
		 that body is not removed.  */
	      while (cnode->clone_of)
		{
		  bool noninline = cnode->clone_of->symbol.decl != cnode->symbol.decl;
		  cnode = cnode->clone_of;
		  if (noninline)
		    {
		      pointer_set_insert (body_needed_for_clonning, cnode->symbol.decl);
		      enqueue_node ((symtab_node)cnode, &first, reachable);
		    }
		}
	    }
	}
      /* When we see constructor of external variable, keep referred nodes in the
	boundary.  This will also hold initializers of the external vars NODE
	refers to.  */
      varpool_node *vnode = dyn_cast <varpool_node> (node);
      if (vnode
	  && DECL_EXTERNAL (node->symbol.decl)
	  && !vnode->symbol.alias
	  && in_boundary_p)
	{
	  struct ipa_ref *ref;
	  for (int i = 0; ipa_ref_list_reference_iterate (&node->symbol.ref_list, i, ref); i++)
	    enqueue_node (ref->referred, &first, reachable);
	}
    }

  /* Remove unreachable functions.   */
  for (node = cgraph_first_function (); node; node = next)
    {
      next = cgraph_next_function (node);

      /* If node is not needed at all, remove it.  */
      if (!node->symbol.aux)
	{
	  if (file)
	    fprintf (file, " %s", cgraph_node_name (node));
	  cgraph_remove_node (node);
	  changed = true;
	}
      /* If node is unreachable, remove its body.  */
      else if (!pointer_set_contains (reachable, node))
        {
	  if (!pointer_set_contains (body_needed_for_clonning, node->symbol.decl))
	    cgraph_release_function_body (node);
	  else if (!node->clone_of)
	    gcc_assert (DECL_RESULT (node->symbol.decl));
	  if (node->symbol.definition)
	    {
	      if (file)
		fprintf (file, " %s", cgraph_node_name (node));
	      cgraph_reset_node (node);
	      changed = true;
	    }
	}
      else
	gcc_assert (node->clone_of || !cgraph_function_with_gimple_body_p (node)
		    || DECL_RESULT (node->symbol.decl));
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
      node->symbol.aux = NULL;
    }

  /* Remove unreachable variables.  */
  if (file)
    fprintf (file, "\nReclaiming variables:");
  for (vnode = varpool_first_variable (); vnode; vnode = vnext)
    {
      vnext = varpool_next_variable (vnode);
      if (!vnode->symbol.aux
	  /* For can_refer_decl_in_current_unit_p we want to track for
	     all external variables if they are defined in other partition
	     or not.  */
	  && (!flag_ltrans || !DECL_EXTERNAL (vnode->symbol.decl)))
	{
	  if (file)
	    fprintf (file, " %s", varpool_node_name (vnode));
	  varpool_remove_node (vnode);
	  changed = true;
	}
      else if (!pointer_set_contains (reachable, vnode))
        {
	  tree init;
	  if (vnode->symbol.definition)
	    {
	      if (file)
		fprintf (file, " %s", varpool_node_name (vnode));
	      changed = true;
	    }
	  vnode->symbol.definition = false;
	  vnode->symbol.analyzed = false;
	  vnode->symbol.aux = NULL;

	  /* Keep body if it may be useful for constant folding.  */
	  if ((init = ctor_for_folding (vnode->symbol.decl)) == error_mark_node)
	    varpool_remove_initializer (vnode);
	  else
	    DECL_INITIAL (vnode->symbol.decl) = init;
	  ipa_remove_all_references (&vnode->symbol.ref_list);
	}
      else
	vnode->symbol.aux = NULL;
    }

  pointer_set_destroy (reachable);
  pointer_set_destroy (body_needed_for_clonning);

  /* Now update address_taken flags and try to promote functions to be local.  */
  if (file)
    fprintf (file, "\nClearing address taken flags:");
  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->symbol.address_taken
	&& !node->symbol.used_from_other_partition)
      {
	if (!cgraph_for_node_and_aliases (node, has_addr_references_p, NULL, true))
	  {
	    if (file)
	      fprintf (file, " %s", cgraph_node_name (node));
	    node->symbol.address_taken = false;
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
      cgraph_propagate_frequency (node);

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
  FOR_EACH_VARIABLE (vnode)
    if (vnode->symbol.definition && varpool_all_refs_explicit_p (vnode)
	&& (TREE_ADDRESSABLE (vnode->symbol.decl)
	    || !TREE_READONLY (vnode->symbol.decl)))
      {
	bool written = false;
	bool address_taken = false;
	int i;
        struct ipa_ref *ref;
        for (i = 0; ipa_ref_list_referring_iterate (&vnode->symbol.ref_list,
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
	if (TREE_ADDRESSABLE (vnode->symbol.decl) && !address_taken)
	  {
	    if (dump_file)
	      fprintf (dump_file, " %s (addressable)", varpool_node_name (vnode));
	    TREE_ADDRESSABLE (vnode->symbol.decl) = 0;
	  }
	if (!TREE_READONLY (vnode->symbol.decl) && !address_taken && !written
	    /* Making variable in explicit section readonly can cause section
	       type conflict. 
	       See e.g. gcc.c-torture/compile/pr23237.c */
	    && DECL_SECTION_NAME (vnode->symbol.decl) == NULL)
	  {
	    if (dump_file)
	      fprintf (dump_file, " %s (read-only)", varpool_node_name (vnode));
	    TREE_READONLY (vnode->symbol.decl) = 1;
	  }
      }
  if (dump_file)
    fprintf (dump_file, "\n");
}

/* Return true when there is a reference to node and it is not vtable.  */
static bool
address_taken_from_non_vtable_p (symtab_node node)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_referring_iterate (&node->symbol.ref_list,
					     i, ref); i++)
    if (ref->use == IPA_REF_ADDR)
      {
	struct varpool_node *node;
	if (is_a <cgraph_node> (ref->referring))
	  return true;
	node = ipa_ref_referring_varpool_node (ref);
	if (!DECL_VIRTUAL_P (node->symbol.decl))
	  return true;
      }
  return false;
}

/* A helper for comdat_can_be_unshared_p.  */

static bool
comdat_can_be_unshared_p_1 (symtab_node node)
{
  /* When address is taken, we don't know if equality comparison won't
     break eventually. Exception are virutal functions and vtables,
     where this is not possible by language standard.  */
  if (!DECL_VIRTUAL_P (node->symbol.decl)
      && address_taken_from_non_vtable_p (node))
    return false;

  /* If the symbol is used in some weird way, better to not touch it.  */
  if (node->symbol.force_output)
    return false;

  /* Explicit instantiations needs to be output when possibly
     used externally.  */
  if (node->symbol.forced_by_abi
      && TREE_PUBLIC (node->symbol.decl)
      && (node->symbol.resolution != LDPR_PREVAILING_DEF_IRONLY
          && !flag_whole_program))
    return false;

  /* Non-readonly and volatile variables can not be duplicated.  */
  if (is_a <varpool_node> (node)
      && (!TREE_READONLY (node->symbol.decl)
	  || TREE_THIS_VOLATILE (node->symbol.decl)))
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
comdat_can_be_unshared_p (symtab_node node)
{
  if (!comdat_can_be_unshared_p_1 (node))
    return false;
  if (node->symbol.same_comdat_group)
    {
      symtab_node next;

      /* If more than one function is in the same COMDAT group, it must
         be shared even if just one function in the comdat group has
         address taken.  */
      for (next = node->symbol.same_comdat_group;
	   next != node; next = next->symbol.same_comdat_group)
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
  if (!node->symbol.definition)
    return false;
  if (!TREE_PUBLIC (node->symbol.decl)
      || DECL_EXTERNAL (node->symbol.decl))
    return false;

  /* Do not try to localize built-in functions yet.  One of problems is that we
     end up mangling their asm for WHOPR that makes it impossible to call them
     using the implicit built-in declarations anymore.  Similarly this enables
     us to remove them as unreachable before actual calls may appear during
     expansion or folding.  */
  if (DECL_BUILT_IN (node->symbol.decl))
    return true;

  /* If linker counts on us, we must preserve the function.  */
  if (symtab_used_from_object_file_p ((symtab_node) node))
    return true;
  if (DECL_PRESERVE_P (node->symbol.decl))
    return true;
  if (lookup_attribute ("externally_visible",
			DECL_ATTRIBUTES (node->symbol.decl)))
    return true;
  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
      && lookup_attribute ("dllexport",
			   DECL_ATTRIBUTES (node->symbol.decl)))
    return true;
  if (node->symbol.resolution == LDPR_PREVAILING_DEF_IRONLY)
    return false;
  /* When doing LTO or whole program, we can bring COMDAT functoins static.
     This improves code quality and we know we will duplicate them at most twice
     (in the case that we are not using plugin and link with object file
      implementing same COMDAT)  */
  if ((in_lto_p || whole_program)
      && DECL_COMDAT (node->symbol.decl)
      && comdat_can_be_unshared_p ((symtab_node) node))
    return false;

  /* When doing link time optimizations, hidden symbols become local.  */
  if (in_lto_p
      && (DECL_VISIBILITY (node->symbol.decl) == VISIBILITY_HIDDEN
	  || DECL_VISIBILITY (node->symbol.decl) == VISIBILITY_INTERNAL)
      /* Be sure that node is defined in IR file, not in other object
	 file.  In that case we don't set used_from_other_object_file.  */
      && node->symbol.definition)
    ;
  else if (!whole_program)
    return true;

  if (MAIN_NAME_P (DECL_NAME (node->symbol.decl)))
    return true;

  return false;
}

/* Return true when variable VNODE should be considered externally visible.  */

bool
varpool_externally_visible_p (struct varpool_node *vnode)
{
  if (DECL_EXTERNAL (vnode->symbol.decl))
    return true;

  if (!TREE_PUBLIC (vnode->symbol.decl))
    return false;

  /* If linker counts on us, we must preserve the function.  */
  if (symtab_used_from_object_file_p ((symtab_node) vnode))
    return true;

  if (DECL_HARD_REGISTER (vnode->symbol.decl))
    return true;
  if (DECL_PRESERVE_P (vnode->symbol.decl))
    return true;
  if (lookup_attribute ("externally_visible",
			DECL_ATTRIBUTES (vnode->symbol.decl)))
    return true;
  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
      && lookup_attribute ("dllexport",
			   DECL_ATTRIBUTES (vnode->symbol.decl)))
    return true;

  /* See if we have linker information about symbol not being used or
     if we need to make guess based on the declaration.

     Even if the linker clams the symbol is unused, never bring internal
     symbols that are declared by user as used or externally visible.
     This is needed for i.e. references from asm statements.   */
  if (symtab_used_from_object_file_p ((symtab_node) vnode))
    return true;
  if (vnode->symbol.resolution == LDPR_PREVAILING_DEF_IRONLY)
    return false;

  /* As a special case, the COMDAT virtual tables can be unshared.
     In LTO mode turn vtables into static variables.  The variable is readonly,
     so this does not enable more optimization, but referring static var
     is faster for dynamic linking.  Also this match logic hidding vtables
     from LTO symbol tables.  */
  if ((in_lto_p || flag_whole_program)
      && DECL_COMDAT (vnode->symbol.decl)
      && comdat_can_be_unshared_p ((symtab_node) vnode))
    return false;

  /* When doing link time optimizations, hidden symbols become local.  */
  if (in_lto_p
      && (DECL_VISIBILITY (vnode->symbol.decl) == VISIBILITY_HIDDEN
	  || DECL_VISIBILITY (vnode->symbol.decl) == VISIBILITY_INTERNAL)
      /* Be sure that node is defined in IR file, not in other object
	 file.  In that case we don't set used_from_other_object_file.  */
      && vnode->symbol.definition)
    ;
  else if (!flag_whole_program)
    return true;

  /* Do not attempt to privatize COMDATS by default.
     This would break linking with C++ libraries sharing
     inline definitions.

     FIXME: We can do so for readonly vars with no address taken and
     possibly also for vtables since no direct pointer comparsion is done.
     It might be interesting to do so to reduce linking overhead.  */
  if (DECL_COMDAT (vnode->symbol.decl) || DECL_WEAK (vnode->symbol.decl))
    return true;
  return false;
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

  /* All aliases should be procssed at this point.  */
  gcc_checking_assert (!alias_pairs || !alias_pairs->length());

  FOR_EACH_FUNCTION (node)
    {
      int flags = flags_from_decl_or_type (node->symbol.decl);

      /* Optimize away PURE and CONST constructors and destructors.  */
      if (optimize
	  && (flags & (ECF_CONST | ECF_PURE))
	  && !(flags & ECF_LOOPING_CONST_OR_PURE))
	{
	  DECL_STATIC_CONSTRUCTOR (node->symbol.decl) = 0;
	  DECL_STATIC_DESTRUCTOR (node->symbol.decl) = 0;
	}

      /* Frontends and alias code marks nodes as needed before parsing is finished.
	 We may end up marking as node external nodes where this flag is meaningless
	 strip it.  */
      if (DECL_EXTERNAL (node->symbol.decl) || !node->symbol.definition)
	{
	  node->symbol.force_output = 0;
	  node->symbol.forced_by_abi = 0;
	}

      /* C++ FE on lack of COMDAT support create local COMDAT functions
	 (that ought to be shared but can not due to object format
	 limitations).  It is necessary to keep the flag to make rest of C++ FE
	 happy.  Clear the flag here to avoid confusion in middle-end.  */
      if (DECL_COMDAT (node->symbol.decl) && !TREE_PUBLIC (node->symbol.decl))
        DECL_COMDAT (node->symbol.decl) = 0;

      /* For external decls stop tracking same_comdat_group. It doesn't matter
	 what comdat group they are in when they won't be emitted in this TU.  */
      if (node->symbol.same_comdat_group && DECL_EXTERNAL (node->symbol.decl))
	{
#ifdef ENABLE_CHECKING
	  symtab_node n;

	  for (n = node->symbol.same_comdat_group;
	       n != (symtab_node)node;
	       n = n->symbol.same_comdat_group)
	      /* If at least one of same comdat group functions is external,
		 all of them have to be, otherwise it is a front-end bug.  */
	      gcc_assert (DECL_EXTERNAL (n->symbol.decl));
#endif
	  symtab_dissolve_same_comdat_group_list ((symtab_node) node);
	}
      gcc_assert ((!DECL_WEAK (node->symbol.decl)
		  && !DECL_COMDAT (node->symbol.decl))
      	          || TREE_PUBLIC (node->symbol.decl)
		  || node->symbol.weakref
		  || DECL_EXTERNAL (node->symbol.decl));
      if (cgraph_externally_visible_p (node, whole_program))
        {
	  gcc_assert (!node->global.inlined_to);
	  node->symbol.externally_visible = true;
	}
      else
	{
	  node->symbol.externally_visible = false;
	  node->symbol.forced_by_abi = false;
	}
      if (!node->symbol.externally_visible
	  && node->symbol.definition && !node->symbol.weakref
	  && !DECL_EXTERNAL (node->symbol.decl))
	{
	  gcc_assert (whole_program || in_lto_p
		      || !TREE_PUBLIC (node->symbol.decl));
	  node->symbol.unique_name = ((node->symbol.resolution == LDPR_PREVAILING_DEF_IRONLY
				      || node->symbol.resolution == LDPR_PREVAILING_DEF_IRONLY_EXP)
				      && TREE_PUBLIC (node->symbol.decl));
	  symtab_make_decl_local (node->symbol.decl);
	  node->symbol.resolution = LDPR_PREVAILING_DEF_IRONLY;
	  if (node->symbol.same_comdat_group)
	    /* cgraph_externally_visible_p has already checked all other nodes
	       in the group and they will all be made local.  We need to
	       dissolve the group at once so that the predicate does not
	       segfault though. */
	    symtab_dissolve_same_comdat_group_list ((symtab_node) node);
	}

      if (node->thunk.thunk_p
	  && TREE_PUBLIC (node->symbol.decl))
	{
	  struct cgraph_node *decl_node = node;

	  decl_node = cgraph_function_node (decl_node->callees->callee, NULL);

	  /* Thunks have the same visibility as function they are attached to.
	     Make sure the C++ front end set this up properly.  */
	  if (DECL_ONE_ONLY (decl_node->symbol.decl))
	    {
	      gcc_checking_assert (DECL_COMDAT (node->symbol.decl)
				   == DECL_COMDAT (decl_node->symbol.decl));
	      gcc_checking_assert (DECL_COMDAT_GROUP (node->symbol.decl)
				   == DECL_COMDAT_GROUP (decl_node->symbol.decl));
	      gcc_checking_assert (node->symbol.same_comdat_group);
	    }
	  if (DECL_EXTERNAL (decl_node->symbol.decl))
	    DECL_EXTERNAL (node->symbol.decl) = 1;
	}
    }
  FOR_EACH_DEFINED_FUNCTION (node)
    node->local.local = cgraph_local_node_p (node);
  FOR_EACH_VARIABLE (vnode)
    {
      /* weak flag makes no sense on local variables.  */
      gcc_assert (!DECL_WEAK (vnode->symbol.decl)
		  || vnode->symbol.weakref
      		  || TREE_PUBLIC (vnode->symbol.decl)
		  || DECL_EXTERNAL (vnode->symbol.decl));
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
      if (DECL_COMMON (vnode->symbol.decl)
	  && (!(TREE_PUBLIC (vnode->symbol.decl)
	      || DECL_EXTERNAL (vnode->symbol.decl))
	      || (DECL_INITIAL (vnode->symbol.decl)
		  && DECL_INITIAL (vnode->symbol.decl) != error_mark_node)
	      || DECL_WEAK (vnode->symbol.decl)
	      || DECL_SECTION_NAME (vnode->symbol.decl) != NULL
	      || ! (ADDR_SPACE_GENERIC_P
		    (TYPE_ADDR_SPACE (TREE_TYPE (vnode->symbol.decl))))))
	DECL_COMMON (vnode->symbol.decl) = 0;
    }
  FOR_EACH_DEFINED_VARIABLE (vnode)
    {
      if (!vnode->symbol.definition)
        continue;
      if (varpool_externally_visible_p (vnode))
	vnode->symbol.externally_visible = true;
      else
	{
          vnode->symbol.externally_visible = false;
	  vnode->symbol.forced_by_abi = false;
	}
      if (!vnode->symbol.externally_visible
	  && !vnode->symbol.weakref)
	{
	  gcc_assert (in_lto_p || whole_program || !TREE_PUBLIC (vnode->symbol.decl));
	  symtab_make_decl_local (vnode->symbol.decl);
	  vnode->symbol.unique_name = ((vnode->symbol.resolution == LDPR_PREVAILING_DEF_IRONLY
				       || vnode->symbol.resolution == LDPR_PREVAILING_DEF_IRONLY_EXP)
				       && TREE_PUBLIC (vnode->symbol.decl));
	  if (vnode->symbol.same_comdat_group)
	    symtab_dissolve_same_comdat_group_list ((symtab_node) vnode);
	  vnode->symbol.resolution = LDPR_PREVAILING_DEF_IRONLY;
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "\nMarking local functions:");
      FOR_EACH_DEFINED_FUNCTION (node)
	if (node->local.local)
	  fprintf (dump_file, " %s", cgraph_node_name (node));
      fprintf (dump_file, "\n\n");
      fprintf (dump_file, "\nMarking externally visible functions:");
      FOR_EACH_DEFINED_FUNCTION (node)
	if (node->symbol.externally_visible)
	  fprintf (dump_file, " %s", cgraph_node_name (node));
      fprintf (dump_file, "\n\n");
      fprintf (dump_file, "\nMarking externally visible variables:");
      FOR_EACH_DEFINED_VARIABLE (vnode)
	if (vnode->symbol.externally_visible)
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
  OPTGROUP_NONE,                        /* optinfo_flags */
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
  TODO_remove_functions | TODO_dump_symtab /* todo_flags_finish */
 }
};

/* Free inline summary.  */

static unsigned
free_inline_summary (void)
{
  inline_free_summary ();
  return 0;
}

struct simple_ipa_opt_pass pass_ipa_free_inline_summary =
{
 {
  SIMPLE_IPA_PASS,
  "*free_inline_summary",		/* name */
  OPTGROUP_NONE,                        /* optinfo_flags */
  NULL,					/* gate */
  free_inline_summary,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_FREE_INLINE_SUMMARY,		/* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  0					/* todo_flags_finish */
 }
};

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

struct ipa_opt_pass_d pass_ipa_whole_program_visibility =
{
 {
  IPA_PASS,
  "whole-program",			/* name */
  OPTGROUP_NONE,                        /* optinfo_flags */
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
  TODO_remove_functions | TODO_dump_symtab /* todo_flags_finish */
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

/* Entry in the histogram.  */

struct histogram_entry
{
  gcov_type count;
  int time;
  int size;
};

/* Histogram of profile values.
   The histogram is represented as an ordered vector of entries allocated via
   histogram_pool. During construction a separate hashtable is kept to lookup
   duplicate entries.  */

vec<histogram_entry *> histogram;
static alloc_pool histogram_pool;

/* Hashtable support for storing SSA names hashed by their SSA_NAME_VAR.  */

struct histogram_hash : typed_noop_remove <histogram_entry>
{
  typedef histogram_entry value_type;
  typedef histogram_entry compare_type;
  static inline hashval_t hash (const value_type *);
  static inline int equal (const value_type *, const compare_type *);
};

inline hashval_t
histogram_hash::hash (const histogram_entry *val)
{
  return val->count;
}

inline int
histogram_hash::equal (const histogram_entry *val, const histogram_entry *val2)
{
  return val->count == val2->count;
}

/* Account TIME and SIZE executed COUNT times into HISTOGRAM.
   HASHTABLE is the on-side hash kept to avoid duplicates.  */

static void
account_time_size (hash_table <histogram_hash> hashtable,
		   vec<histogram_entry *> &histogram,
		   gcov_type count, int time, int size)
{
  histogram_entry key = {count, 0, 0};
  histogram_entry **val = hashtable.find_slot (&key, INSERT);

  if (!*val)
    {
      *val = (histogram_entry *) pool_alloc (histogram_pool);
      **val = key;
      histogram.safe_push (*val);
    }
  (*val)->time += time;
  (*val)->size += size;
}

int
cmp_counts (const void *v1, const void *v2)
{
  const histogram_entry *h1 = *(const histogram_entry * const *)v1;
  const histogram_entry *h2 = *(const histogram_entry * const *)v2;
  if (h1->count < h2->count)
    return 1;
  if (h1->count > h2->count)
    return -1;
  return 0;
}

/* Dump HISTOGRAM to FILE.  */

static void
dump_histogram (FILE *file, vec<histogram_entry *> histogram)
{
  unsigned int i;
  gcov_type overall_time = 0, cumulated_time = 0, cumulated_size = 0, overall_size = 0;
  
  fprintf (dump_file, "Histogram:\n");
  for (i = 0; i < histogram.length (); i++)
    {
      overall_time += histogram[i]->count * histogram[i]->time;
      overall_size += histogram[i]->size;
    }
  if (!overall_time)
    overall_time = 1;
  if (!overall_size)
    overall_size = 1;
  for (i = 0; i < histogram.length (); i++)
    {
      cumulated_time += histogram[i]->count * histogram[i]->time;
      cumulated_size += histogram[i]->size;
      fprintf (file, "  "HOST_WIDEST_INT_PRINT_DEC": time:%i (%2.2f) size:%i (%2.2f)\n",
	       (HOST_WIDEST_INT) histogram[i]->count,
	       histogram[i]->time,
	       cumulated_time * 100.0 / overall_time,
	       histogram[i]->size,
	       cumulated_size * 100.0 / overall_size);
   }
}

/* Collect histogram from CFG profiles.  */

static void
ipa_profile_generate_summary (void)
{
  struct cgraph_node *node;
  gimple_stmt_iterator gsi;
  hash_table <histogram_hash> hashtable;
  basic_block bb;

  hashtable.create (10);
  histogram_pool = create_alloc_pool ("IPA histogram", sizeof (struct histogram_entry),
				      10);
  
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (node->symbol.decl))
      {
	int time = 0;
	int size = 0;
        for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	  {
	    time += estimate_num_insns (gsi_stmt (gsi), &eni_time_weights);
	    size += estimate_num_insns (gsi_stmt (gsi), &eni_size_weights);
	  }
	account_time_size (hashtable, histogram, bb->count, time, size);
      }
  hashtable.dispose ();
  histogram.qsort (cmp_counts);
}

/* Serialize the ipa info for lto.  */

static void
ipa_profile_write_summary (void)
{
  struct lto_simple_output_block *ob
    = lto_create_simple_output_block (LTO_section_ipa_profile);
  unsigned int i;

  streamer_write_uhwi_stream (ob->main_stream, histogram.length());
  for (i = 0; i < histogram.length (); i++)
    {
      streamer_write_gcov_count_stream (ob->main_stream, histogram[i]->count);
      streamer_write_uhwi_stream (ob->main_stream, histogram[i]->time);
      streamer_write_uhwi_stream (ob->main_stream, histogram[i]->size);
    }
  lto_destroy_simple_output_block (ob);
}

/* Deserialize the ipa info for lto.  */

static void
ipa_profile_read_summary (void)
{
  struct lto_file_decl_data ** file_data_vec
    = lto_get_file_decl_data ();
  struct lto_file_decl_data * file_data;
  hash_table <histogram_hash> hashtable;
  int j = 0;

  hashtable.create (10);
  histogram_pool = create_alloc_pool ("IPA histogram", sizeof (struct histogram_entry),
				      10);

  while ((file_data = file_data_vec[j++]))
    {
      const char *data;
      size_t len;
      struct lto_input_block *ib
	= lto_create_simple_input_block (file_data,
					 LTO_section_ipa_profile,
					 &data, &len);
      if (ib)
	{
          unsigned int num = streamer_read_uhwi (ib);
	  unsigned int n;
	  for (n = 0; n < num; n++)
	    {
	      gcov_type count = streamer_read_gcov_count (ib);
	      int time = streamer_read_uhwi (ib);
	      int size = streamer_read_uhwi (ib);
	      account_time_size (hashtable, histogram,
				 count, time, size);
	    }
	  lto_destroy_simple_input_block (file_data,
					  LTO_section_ipa_profile,
					  ib, data, len);
	}
    }
  hashtable.dispose ();
  histogram.qsort (cmp_counts);
}

/* Simple ipa profile pass propagating frequencies across the callgraph.  */

static unsigned int
ipa_profile (void)
{
  struct cgraph_node **order = XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  struct cgraph_edge *e;
  int order_pos;
  bool something_changed = false;
  int i;
  gcov_type overall_time = 0, cutoff = 0, cumulated = 0, overall_size = 0;

  if (dump_file)
    dump_histogram (dump_file, histogram);
  for (i = 0; i < (int)histogram.length (); i++)
    {
      overall_time += histogram[i]->count * histogram[i]->time;
      overall_size += histogram[i]->size;
    }
  if (overall_time)
    {
      gcov_type threshold;

      gcc_assert (overall_size);
      if (dump_file)
	{
	  gcov_type min, cumulated_time = 0, cumulated_size = 0;

	  fprintf (dump_file, "Overall time: "HOST_WIDEST_INT_PRINT_DEC"\n", 
		   (HOST_WIDEST_INT)overall_time);
	  min = get_hot_bb_threshold ();
          for (i = 0; i < (int)histogram.length () && histogram[i]->count >= min;
	       i++)
	    {
	      cumulated_time += histogram[i]->count * histogram[i]->time;
	      cumulated_size += histogram[i]->size;
	    }
	  fprintf (dump_file, "GCOV min count: "HOST_WIDEST_INT_PRINT_DEC
		   " Time:%3.2f%% Size:%3.2f%%\n", 
		   (HOST_WIDEST_INT)min,
		   cumulated_time * 100.0 / overall_time,
		   cumulated_size * 100.0 / overall_size);
	}
      cutoff = (overall_time * PARAM_VALUE (HOT_BB_COUNT_WS_PERMILLE) + 500) / 1000;
      threshold = 0;
      for (i = 0; cumulated < cutoff; i++)
	{
	  cumulated += histogram[i]->count * histogram[i]->time;
          threshold = histogram[i]->count;
	}
      if (!threshold)
	threshold = 1;
      if (dump_file)
	{
	  gcov_type cumulated_time = 0, cumulated_size = 0;

          for (i = 0;
	       i < (int)histogram.length () && histogram[i]->count >= threshold;
	       i++)
	    {
	      cumulated_time += histogram[i]->count * histogram[i]->time;
	      cumulated_size += histogram[i]->size;
	    }
	  fprintf (dump_file, "Determined min count: "HOST_WIDEST_INT_PRINT_DEC
		   " Time:%3.2f%% Size:%3.2f%%\n", 
		   (HOST_WIDEST_INT)threshold,
		   cumulated_time * 100.0 / overall_time,
		   cumulated_size * 100.0 / overall_size);
	}
      if (threshold > get_hot_bb_threshold ()
	  || in_lto_p)
	{
	  if (dump_file)
	    fprintf (dump_file, "Threshold updated.\n");
          set_hot_bb_threshold (threshold);
	}
    }
  histogram.release();
  free_alloc_pool (histogram_pool);

  order_pos = ipa_reverse_postorder (order);
  for (i = order_pos - 1; i >= 0; i--)
    {
      if (order[i]->local.local && cgraph_propagate_frequency (order[i]))
	{
	  for (e = order[i]->callees; e; e = e->next_callee)
	    if (e->callee->local.local && !e->callee->symbol.aux)
	      {
	        something_changed = true;
	        e->callee->symbol.aux = (void *)1;
	      }
	}
      order[i]->symbol.aux = NULL;
    }

  while (something_changed)
    {
      something_changed = false;
      for (i = order_pos - 1; i >= 0; i--)
	{
	  if (order[i]->symbol.aux && cgraph_propagate_frequency (order[i]))
	    {
	      for (e = order[i]->callees; e; e = e->next_callee)
		if (e->callee->local.local && !e->callee->symbol.aux)
		  {
		    something_changed = true;
		    e->callee->symbol.aux = (void *)1;
		  }
	    }
	  order[i]->symbol.aux = NULL;
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
  OPTGROUP_NONE,                        /* optinfo_flags */
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
 ipa_profile_generate_summary,	        /* generate_summary */
 ipa_profile_write_summary,		/* write_summary */
 ipa_profile_read_summary,		/* read_summary */
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
  if (DECL_STATIC_CONSTRUCTOR (node->symbol.decl))
    static_ctors.safe_push (node->symbol.decl);
  if (DECL_STATIC_DESTRUCTOR (node->symbol.decl))
    static_dtors.safe_push (node->symbol.decl);
  node = cgraph_get_node (node->symbol.decl);
  DECL_DISREGARD_INLINE_LIMITS (node->symbol.decl) = 1;
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
    if (DECL_STATIC_CONSTRUCTOR (node->symbol.decl)
	|| DECL_STATIC_DESTRUCTOR (node->symbol.decl))
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

struct ipa_opt_pass_d pass_ipa_cdtor_merge =
{
 {
  IPA_PASS,
  "cdtor",				/* name */
  OPTGROUP_NONE,                        /* optinfo_flags */
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
