/* IPA visibility pass
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

/* This file implements two related passes: 

     - pass_data_ipa_function_and_variable_visibility run just after
       symbol table, references and callgraph are built

     - pass_data_ipa_function_and_variable_visibility run as first
       proper IPA pass (that is after early optimization, or, (with LTO)
       as a first pass done at link-time.

   Purpose of both passes is to set correctly visibility properties
   of all symbols.  This includes:

    - Symbol privatization:

      Some symbols that are declared public by frontend may be
      turned local (either by -fwhole-program flag, by linker plugin feedback
      or by other reasons)

    - Discovery of local functions:

      A local function is one whose calls can occur only in the current
      compilation unit and all its calls are explicit, so we can change
      its calling convention.  We simply mark all static functions whose
      address is not taken as local.

      externally_visible flag is set for symbols that can not be privatized.
      For privatized symbols we clear TREE_PUBLIC flag and dismantle comdat
      group.

    - Dismantling of comdat groups:

      Comdat group represent a section that may be replaced by linker by
      a different copy of the same section from other unit.
      If we have resolution information (from linker plugin) and we know that
      a given comdat gorup is prevailing, we can dismantle it and turn symbols
      into normal symbols.  If the resolution information says that the
      section was previaled by copy from non-LTO code, we can also dismantle
      it and turn all symbols into external.

    - Local aliases:

      Some symbols can be interposed by dynamic linker. Refering to these
      symbols is expensive, since it needs to be overwritable by the dynamic
      linker.  In some cases we know that the interposition does not change
      semantic and we can always refer to a local copy (as in the case of
      inline function).  In this case we produce a local alias and redirect
      calls to it.

      TODO: This should be done for references, too.

    - Removal of static ocnstructors and destructors that have no side effects.

    - Regularization of several oddities introduced by frontends that may
      be impractical later in the optimization queue.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "pointer-set.h"
#include "calls.h"
#include "gimple-expr.h"
#include "varasm.h"

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

bool
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

/* Return true when there is a reference to node and it is not vtable.  */
bool
address_taken_from_non_vtable_p (symtab_node *node)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_referring_iterate (&node->ref_list,
					     i, ref); i++)
    if (ref->use == IPA_REF_ADDR)
      {
	varpool_node *node;
	if (is_a <cgraph_node *> (ref->referring))
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
  if (!node->externally_visible)
    return true;
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
  if (is_a <varpool_node *> (node)
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
  /* When doing LTO or whole program, we can bring COMDAT functions static.
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
	  && !decl_binds_to_current_def_p (node->decl)
	  && !symtab_can_be_discarded (node));
}

/* Return true if we can replace refernece to NODE by local alias
   within a virtual table.  Generally we can replace function pointers
   and virtual table pointers.  */

bool
can_replace_by_local_alias_in_vtable (symtab_node *node)
{
  if (is_a <varpool_node *> (node)
      && !DECL_VIRTUAL_P (node->decl))
    return false;
  return can_replace_by_local_alias (node);
}

/* walk_tree callback that rewrites initializer references.   */

static tree
update_vtable_references (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (*tp) == VAR_DECL
      || TREE_CODE (*tp) == FUNCTION_DECL)
    {
      if (can_replace_by_local_alias_in_vtable (symtab_get_node (*tp)))
	*tp = symtab_nonoverwritable_alias (symtab_get_node (*tp))->decl;
      *walk_subtrees = 0;
    }
  else if (IS_TYPE_OR_DECL_P (*tp))
    *walk_subtrees = 0;
  return NULL;
}

/* In LTO we can remove COMDAT groups and weak symbols.
   Either turn them into normal symbols or external symbol depending on 
   resolution info.  */

static void
update_visibility_by_resolution_info (symtab_node * node)
{
  bool define;

  if (!node->externally_visible
      || (!DECL_WEAK (node->decl) && !DECL_ONE_ONLY (node->decl))
      || node->resolution == LDPR_UNKNOWN
      || node->resolution == LDPR_UNDEF)
    return;

  define = (node->resolution == LDPR_PREVAILING_DEF_IRONLY
	    || node->resolution == LDPR_PREVAILING_DEF
	    || node->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP);

  /* The linker decisions ought to agree in the whole group.  */
  if (node->same_comdat_group)
    for (symtab_node *next = node->same_comdat_group;
	 next != node; next = next->same_comdat_group)
      gcc_assert (!next->externally_visible
		  || define == (next->resolution == LDPR_PREVAILING_DEF_IRONLY
			        || next->resolution == LDPR_PREVAILING_DEF
			        || next->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP));

  if (node->same_comdat_group)
    for (symtab_node *next = node->same_comdat_group;
	 next != node; next = next->same_comdat_group)
      {
	next->set_comdat_group (NULL);
	DECL_WEAK (next->decl) = false;
	if (next->externally_visible
	    && !define)
	  DECL_EXTERNAL (next->decl) = true;
	if (!next->alias)
	  next->reset_section ();
      }
  node->set_comdat_group (NULL);
  DECL_WEAK (node->decl) = false;
  if (!define)
    DECL_EXTERNAL (node->decl) = true;
  if (!node->alias)
    node->reset_section ();
  symtab_dissolve_same_comdat_group_list (node);
}

/* Decide on visibility of all symbols.  */

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
	  bool reset = TREE_PUBLIC (node->decl);
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
		  next->set_comdat_group (NULL);
		  symtab_make_decl_local (next->decl);
		  if (!node->alias)
		    node->reset_section ();
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
	  if (TREE_PUBLIC (node->decl))
	    node->set_comdat_group (NULL);
	  symtab_make_decl_local (node->decl);
	  if (reset && !node->alias)
	    node->reset_section ();
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
	      gcc_checking_assert (symtab_in_same_comdat_p (node, decl_node));
	      gcc_checking_assert (node->same_comdat_group);
	    }
	  node->forced_by_abi = decl_node->forced_by_abi;
	  if (DECL_EXTERNAL (decl_node->decl))
	    DECL_EXTERNAL (node->decl) = 1;
	}

      update_visibility_by_resolution_info (node);
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
	  bool reset = TREE_PUBLIC (vnode->decl);
	  gcc_assert (in_lto_p || whole_program || !TREE_PUBLIC (vnode->decl));
	  vnode->unique_name = ((vnode->resolution == LDPR_PREVAILING_DEF_IRONLY
				       || vnode->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP)
				       && TREE_PUBLIC (vnode->decl));
	  if (vnode->same_comdat_group && TREE_PUBLIC (vnode->decl))
	    {
	      symtab_node *next = vnode;

	      /* Set all members of comdat group local.  */
	      if (vnode->same_comdat_group)
		for (next = vnode->same_comdat_group;
		     next != vnode;
		     next = next->same_comdat_group)
		{
		  next->set_comdat_group (NULL);
		  symtab_make_decl_local (next->decl);
		  if (!next->alias)
		    next->reset_section ();
		  next->unique_name = ((next->resolution == LDPR_PREVAILING_DEF_IRONLY
					|| next->unique_name
					|| next->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP)
				       && TREE_PUBLIC (next->decl));
		}
	      symtab_dissolve_same_comdat_group_list (vnode);
	    }
	  if (TREE_PUBLIC (vnode->decl))
	    vnode->set_comdat_group (NULL);
	  symtab_make_decl_local (vnode->decl);
	  if (reset && !vnode->alias)
	    vnode->reset_section ();
	  vnode->resolution = LDPR_PREVAILING_DEF_IRONLY;
	}
      update_visibility_by_resolution_info (vnode);

      /* Update virtual tables to point to local aliases where possible.  */
      if (DECL_VIRTUAL_P (vnode->decl)
	  && !DECL_EXTERNAL (vnode->decl)
	  /* FIXME: currently this optimization breaks on AIX.  Disable it for targets
	     without comdat support for now.  */
	  && SUPPORTS_ONE_ONLY)
	{
	  int i;
	  struct ipa_ref *ref;
	  bool found = false;

	  /* See if there is something to update.  */
	  for (i = 0; ipa_ref_list_referring_iterate (&vnode->ref_list,
						      i, ref); i++)
	    if (ref->use == IPA_REF_ADDR
		&& can_replace_by_local_alias_in_vtable (ref->referred))
	      {
	        found = true;
		break;
	      }
	  if (found)
	    {
	      struct pointer_set_t *visited_nodes = pointer_set_create ();
	      walk_tree (&DECL_INITIAL (vnode->decl),
			 update_vtable_references, NULL, visited_nodes);
	      pointer_set_destroy (visited_nodes);
	      ipa_remove_all_references (&vnode->ref_list);
	      record_references_in_initializer (vnode->decl, false);
	    }
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

namespace {

const pass_data pass_data_ipa_function_and_variable_visibility =
{
  SIMPLE_IPA_PASS, /* type */
  "visibility", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_CGRAPHOPT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_remove_functions | TODO_dump_symtab ), /* todo_flags_finish */
};

/* Bring functions local at LTO time with -fwhole-program.  */

static unsigned int
whole_program_function_and_variable_visibility (void)
{
  function_and_variable_visibility (flag_whole_program);
  if (optimize)
    ipa_discover_readonly_nonaddressable_vars ();
  return 0;
}

} // anon namespace

namespace {

const pass_data pass_data_ipa_whole_program_visibility =
{
  IPA_PASS, /* type */
  "whole-program", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
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

  virtual bool gate (function *)
    {
      /* Do not re-run on ltrans stage.  */
      return !flag_ltrans;
    }
  virtual unsigned int execute (function *)
    {
      return whole_program_function_and_variable_visibility ();
    }

}; // class pass_ipa_whole_program_visibility

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_whole_program_visibility (gcc::context *ctxt)
{
  return new pass_ipa_whole_program_visibility (ctxt);
}

class pass_ipa_function_and_variable_visibility : public simple_ipa_opt_pass
{
public:
  pass_ipa_function_and_variable_visibility (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_function_and_variable_visibility,
			   ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *)
    {
      return function_and_variable_visibility (flag_whole_program && !flag_lto);
    }

}; // class pass_ipa_function_and_variable_visibility

simple_ipa_opt_pass *
make_pass_ipa_function_and_variable_visibility (gcc::context *ctxt)
{
  return new pass_ipa_function_and_variable_visibility (ctxt);
}
