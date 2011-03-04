/* Perform optimizations on tree structure.
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2007, 2008, 2009,
   2010 Free Software Foundation, Inc.
   Written by Mark Michell (mark@codesourcery.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "cp-tree.h"
#include "input.h"
#include "params.h"
#include "hashtab.h"
#include "target.h"
#include "debug.h"
#include "tree-inline.h"
#include "flags.h"
#include "langhooks.h"
#include "diagnostic-core.h"
#include "tree-dump.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "cgraph.h"

/* Prototypes.  */

static void update_cloned_parm (tree, tree, bool);

/* CLONED_PARM is a copy of CLONE, generated for a cloned constructor
   or destructor.  Update it to ensure that the source-position for
   the cloned parameter matches that for the original, and that the
   debugging generation code will be able to find the original PARM.  */

static void
update_cloned_parm (tree parm, tree cloned_parm, bool first)
{
  DECL_ABSTRACT_ORIGIN (cloned_parm) = parm;

  /* We may have taken its address.  */
  TREE_ADDRESSABLE (cloned_parm) = TREE_ADDRESSABLE (parm);

  /* The definition might have different constness.  */
  TREE_READONLY (cloned_parm) = TREE_READONLY (parm);

  TREE_USED (cloned_parm) = !first || TREE_USED (parm);

  /* The name may have changed from the declaration.  */
  DECL_NAME (cloned_parm) = DECL_NAME (parm);
  DECL_SOURCE_LOCATION (cloned_parm) = DECL_SOURCE_LOCATION (parm);
  TREE_TYPE (cloned_parm) = TREE_TYPE (parm);

  DECL_GIMPLE_REG_P (cloned_parm) = DECL_GIMPLE_REG_P (parm);
}


/* FN is a function in High GIMPLE form that has a complete body and no
   CFG.  CLONE is a function whose body is to be set to a copy of FN,
   mapping argument declarations according to the ARG_MAP splay_tree.  */

static void
clone_body (tree clone, tree fn, void *arg_map)
{
  copy_body_data id;
  tree stmts;

  /* Clone the body, as if we were making an inline call.  But, remap
     the parameters in the callee to the parameters of caller.  */
  memset (&id, 0, sizeof (id));
  id.src_fn = fn;
  id.dst_fn = clone;
  id.src_cfun = DECL_STRUCT_FUNCTION (fn);
  id.decl_map = (struct pointer_map_t *) arg_map;

  id.copy_decl = copy_decl_no_change;
  id.transform_call_graph_edges = CB_CGE_DUPLICATE;
  id.transform_new_cfg = true;
  id.transform_return_to_modify = false;
  id.transform_lang_insert_block = NULL;

  /* We're not inside any EH region.  */
  id.eh_lp_nr = 0;

  stmts = DECL_SAVED_TREE (fn);
  walk_tree (&stmts, copy_tree_body_r, &id, NULL);

  /* Also remap the initializer of any static variables so that they (in
     particular, any label addresses) correspond to the base variant rather
     than the abstract one.  */
  if (DECL_NAME (clone) == base_dtor_identifier
      || DECL_NAME (clone) == base_ctor_identifier)
    {
      unsigned ix;
      tree decl;

      FOR_EACH_LOCAL_DECL (DECL_STRUCT_FUNCTION (fn), ix, decl)
        walk_tree (&DECL_INITIAL (decl), copy_tree_body_r, &id, NULL);
    }

  append_to_statement_list_force (stmts, &DECL_SAVED_TREE (clone));
}

/* DELETE_DTOR is a delete destructor whose body will be built.
   COMPLETE_DTOR is the corresponding complete destructor.  */

static void
build_delete_destructor_body (tree delete_dtor, tree complete_dtor)
{
  tree call_dtor, call_delete;
  tree parm = DECL_ARGUMENTS (delete_dtor);
  tree virtual_size = cxx_sizeof (current_class_type);

  /* Call the corresponding complete destructor.  */
  gcc_assert (complete_dtor);
  call_dtor = build_cxx_call (complete_dtor, 1, &parm);
  add_stmt (call_dtor);

  add_stmt (build_stmt (0, LABEL_EXPR, cdtor_label));

  /* Call the delete function.  */
  call_delete = build_op_delete_call (DELETE_EXPR, current_class_ptr,
                                      virtual_size,
                                      /*global_p=*/false,
                                      /*placement=*/NULL_TREE,
                                      /*alloc_fn=*/NULL_TREE);
  add_stmt (call_delete);

  /* Return the address of the object.  */
  if (targetm.cxx.cdtor_returns_this ())
    {
      tree val = DECL_ARGUMENTS (delete_dtor);
      val = build2 (MODIFY_EXPR, TREE_TYPE (val),
                    DECL_RESULT (delete_dtor), val);
      add_stmt (build_stmt (0, RETURN_EXPR, val));
    }
}

/* Return name of comdat group for complete and base ctor (or dtor)
   that have the same body.  If dtor is virtual, deleting dtor goes
   into this comdat group as well.  */

static tree
cdtor_comdat_group (tree complete, tree base)
{
  tree complete_name = DECL_COMDAT_GROUP (complete);
  tree base_name = DECL_COMDAT_GROUP (base);
  char *grp_name;
  const char *p, *q;
  bool diff_seen = false;
  size_t idx;
  if (complete_name == NULL)
    complete_name = cxx_comdat_group (complete);
  if (base_name == NULL)
    base_name = cxx_comdat_group (base);
  gcc_assert (IDENTIFIER_LENGTH (complete_name)
	      == IDENTIFIER_LENGTH (base_name));
  grp_name = XALLOCAVEC (char, IDENTIFIER_LENGTH (complete_name) + 1);
  p = IDENTIFIER_POINTER (complete_name);
  q = IDENTIFIER_POINTER (base_name);
  for (idx = 0; idx < IDENTIFIER_LENGTH (complete_name); idx++)
    if (p[idx] == q[idx])
      grp_name[idx] = p[idx];
    else
      {
	gcc_assert (!diff_seen
		    && idx > 0
		    && (p[idx - 1] == 'C' || p[idx - 1] == 'D')
		    && p[idx] == '1'
		    && q[idx] == '2');
	grp_name[idx] = '5';
	diff_seen = true;
      }
  grp_name[idx] = '\0';
  gcc_assert (diff_seen);
  return get_identifier (grp_name);
}

/* FN is a function that has a complete body.  Clone the body as
   necessary.  Returns nonzero if there's no longer any need to
   process the main body.  */

bool
maybe_clone_body (tree fn)
{
  tree comdat_group = NULL_TREE;
  tree clone;
  tree fns[3];
  bool first = true;
  bool in_charge_parm_used;
  int idx;
  bool need_alias = false;

  /* We only clone constructors and destructors.  */
  if (!DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (fn)
      && !DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (fn))
    return 0;

  /* Emit the DWARF1 abstract instance.  */
  (*debug_hooks->deferred_inline_function) (fn);

  in_charge_parm_used = CLASSTYPE_VBASECLASSES (DECL_CONTEXT (fn)) != NULL;
  fns[0] = NULL_TREE;
  fns[1] = NULL_TREE;
  fns[2] = NULL_TREE;

  /* Look for the complete destructor which may be used to build the
     delete destructor.  */
  FOR_EACH_CLONE (clone, fn)
    if (DECL_NAME (clone) == complete_dtor_identifier
	|| DECL_NAME (clone) == complete_ctor_identifier)
      fns[1] = clone;
    else if (DECL_NAME (clone) == base_dtor_identifier
	     || DECL_NAME (clone) == base_ctor_identifier)
      fns[0] = clone;
    else if (DECL_NAME (clone) == deleting_dtor_identifier)
      fns[2] = clone;
    else
      gcc_unreachable ();

  /* Remember if we can't have multiple clones for some reason.  We need to
     check this before we remap local static initializers in clone_body.  */
  if (!tree_versionable_function_p (fn))
    need_alias = true;

  /* We know that any clones immediately follow FN in the TYPE_METHODS
     list.  */
  push_to_top_level ();
  for (idx = 0; idx < 3; idx++)
    {
      tree parm;
      tree clone_parm;
      int parmno;
      bool alias = false;
      struct pointer_map_t *decl_map;

      clone = fns[idx];
      if (!clone)
	continue;      

      /* Update CLONE's source position information to match FN's.  */
      DECL_SOURCE_LOCATION (clone) = DECL_SOURCE_LOCATION (fn);
      DECL_DECLARED_INLINE_P (clone) = DECL_DECLARED_INLINE_P (fn);
      DECL_DECLARED_CONSTEXPR_P (clone) = DECL_DECLARED_CONSTEXPR_P (fn);
      DECL_COMDAT (clone) = DECL_COMDAT (fn);
      DECL_WEAK (clone) = DECL_WEAK (fn);

      /* We don't copy the comdat group from fn to clone because the assembler
	 name of fn was corrupted by write_mangled_name by adding *INTERNAL*
	 to it. By doing so, it also corrupted the comdat group. */
      if (DECL_ONE_ONLY (fn))
	DECL_COMDAT_GROUP (clone) = cxx_comdat_group (clone);
      DECL_SECTION_NAME (clone) = DECL_SECTION_NAME (fn);
      DECL_USE_TEMPLATE (clone) = DECL_USE_TEMPLATE (fn);
      DECL_EXTERNAL (clone) = DECL_EXTERNAL (fn);
      DECL_INTERFACE_KNOWN (clone) = DECL_INTERFACE_KNOWN (fn);
      DECL_NOT_REALLY_EXTERN (clone) = DECL_NOT_REALLY_EXTERN (fn);
      TREE_PUBLIC (clone) = TREE_PUBLIC (fn);
      DECL_VISIBILITY (clone) = DECL_VISIBILITY (fn);
      DECL_VISIBILITY_SPECIFIED (clone) = DECL_VISIBILITY_SPECIFIED (fn);
      DECL_DLLIMPORT_P (clone) = DECL_DLLIMPORT_P (fn);
      DECL_ATTRIBUTES (clone) = copy_list (DECL_ATTRIBUTES (fn));
      DECL_DISREGARD_INLINE_LIMITS (clone) = DECL_DISREGARD_INLINE_LIMITS (fn);

      /* Adjust the parameter names and locations.  */
      parm = DECL_ARGUMENTS (fn);
      clone_parm = DECL_ARGUMENTS (clone);
      /* Update the `this' parameter, which is always first.  */
      update_cloned_parm (parm, clone_parm, first);
      parm = DECL_CHAIN (parm);
      clone_parm = DECL_CHAIN (clone_parm);
      if (DECL_HAS_IN_CHARGE_PARM_P (fn))
	parm = DECL_CHAIN (parm);
      if (DECL_HAS_VTT_PARM_P (fn))
	parm = DECL_CHAIN (parm);
      if (DECL_HAS_VTT_PARM_P (clone))
	clone_parm = DECL_CHAIN (clone_parm);
      for (; parm;
	   parm = DECL_CHAIN (parm), clone_parm = DECL_CHAIN (clone_parm))
	/* Update this parameter.  */
	update_cloned_parm (parm, clone_parm, first);

      /* Start processing the function.  */
      start_preparsed_function (clone, NULL_TREE, SF_PRE_PARSED);

      /* Tell cgraph if both ctors or both dtors are known to have
	 the same body.  */
      if (!in_charge_parm_used
	  && fns[0]
	  && idx == 1
	  && !flag_use_repository
	  && DECL_INTERFACE_KNOWN (fns[0])
	  && (SUPPORTS_ONE_ONLY || !DECL_WEAK (fns[0]))
	  && (!DECL_ONE_ONLY (fns[0])
	      || (HAVE_COMDAT_GROUP
		  && DECL_WEAK (fns[0])))
	  && cgraph_same_body_alias (cgraph_node (fns[0]), clone, fns[0]))
	{
	  alias = true;
	  if (DECL_ONE_ONLY (fns[0]))
	    {
	      /* For comdat base and complete cdtors put them
		 into the same, *[CD]5* comdat group instead of
		 *[CD][12]*.  */
	      comdat_group = cdtor_comdat_group (fns[1], fns[0]);
	      DECL_COMDAT_GROUP (fns[0]) = comdat_group;
	    }
	}

      /* Build the delete destructor by calling complete destructor
         and delete function.  */
      if (idx == 2)
	build_delete_destructor_body (clone, fns[1]);
      else if (alias)
	/* No need to populate body.  */ ;
      else
	{
	  /* If we can't have multiple copies of FN (say, because there's a
	     static local initialized with the address of a label), we need
	     to use an alias for the complete variant.  */
	  if (idx == 1 && need_alias)
	    {
	      if (DECL_STRUCT_FUNCTION (fn)->cannot_be_copied_set)
		sorry (DECL_STRUCT_FUNCTION (fn)->cannot_be_copied_reason, fn);
	      else
		sorry ("making multiple clones of %qD", fn);
	    }

          /* Remap the parameters.  */
          decl_map = pointer_map_create ();
          for (parmno = 0,
                parm = DECL_ARGUMENTS (fn),
                clone_parm = DECL_ARGUMENTS (clone);
              parm;
              ++parmno,
                parm = DECL_CHAIN (parm))
            {
              /* Map the in-charge parameter to an appropriate constant.  */
              if (DECL_HAS_IN_CHARGE_PARM_P (fn) && parmno == 1)
                {
                  tree in_charge;
                  in_charge = in_charge_arg_for_name (DECL_NAME (clone));
                  *pointer_map_insert (decl_map, parm) = in_charge;
                }
              else if (DECL_ARTIFICIAL (parm)
                       && DECL_NAME (parm) == vtt_parm_identifier)
                {
                  /* For a subobject constructor or destructor, the next
                     argument is the VTT parameter.  Remap the VTT_PARM
                     from the CLONE to this parameter.  */
                  if (DECL_HAS_VTT_PARM_P (clone))
                    {
                      DECL_ABSTRACT_ORIGIN (clone_parm) = parm;
                      *pointer_map_insert (decl_map, parm) = clone_parm;
                      clone_parm = DECL_CHAIN (clone_parm);
                    }
                  /* Otherwise, map the VTT parameter to `NULL'.  */
                  else
                    *pointer_map_insert (decl_map, parm)
                       = fold_convert (TREE_TYPE (parm), null_pointer_node);
                }
              /* Map other parameters to their equivalents in the cloned
                 function.  */
              else
                {
                  *pointer_map_insert (decl_map, parm) = clone_parm;
                  clone_parm = DECL_CHAIN (clone_parm);
                }
            }

          if (targetm.cxx.cdtor_returns_this ())
            {
              parm = DECL_RESULT (fn);
              clone_parm = DECL_RESULT (clone);
              *pointer_map_insert (decl_map, parm) = clone_parm;
            }

          /* Clone the body.  */
          clone_body (clone, fn, decl_map);

          /* Clean up.  */
          pointer_map_destroy (decl_map);
        }

      /* The clone can throw iff the original function can throw.  */
      cp_function_chain->can_throw = !TREE_NOTHROW (fn);

      /* Now, expand this function into RTL, if appropriate.  */
      finish_function (0);
      BLOCK_ABSTRACT_ORIGIN (DECL_INITIAL (clone)) = DECL_INITIAL (fn);
      if (alias)
	{
	  if (expand_or_defer_fn_1 (clone))
	    emit_associated_thunks (clone);
	}
      else
	expand_or_defer_fn (clone);
      first = false;
    }
  pop_from_top_level ();

  if (comdat_group)
    {
      DECL_COMDAT_GROUP (fns[1]) = comdat_group;
      if (fns[2])
	{
	  struct cgraph_node *base_dtor_node, *deleting_dtor_node;
	  /* If *[CD][12]* dtors go into the *[CD]5* comdat group and dtor is
	     virtual, it goes into the same comdat group as well.  */
	  DECL_COMDAT_GROUP (fns[2]) = comdat_group;
	  base_dtor_node = cgraph_node (fns[0]);
	  deleting_dtor_node = cgraph_node (fns[2]);
	  gcc_assert (base_dtor_node->same_comdat_group == NULL);
	  gcc_assert (deleting_dtor_node->same_comdat_group == NULL);
	  base_dtor_node->same_comdat_group = deleting_dtor_node;
	  deleting_dtor_node->same_comdat_group = base_dtor_node;
	}
    }

  /* We don't need to process the original function any further.  */
  return 1;
}
