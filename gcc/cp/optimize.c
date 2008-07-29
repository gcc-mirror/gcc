/* Perform optimizations on tree structure.
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2007, 2008
   Free Software Foundation, Inc.
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
#include "rtl.h"
#include "insn-config.h"
#include "input.h"
#include "integrate.h"
#include "toplev.h"
#include "varray.h"
#include "params.h"
#include "hashtab.h"
#include "target.h"
#include "debug.h"
#include "tree-inline.h"
#include "flags.h"
#include "langhooks.h"
#include "diagnostic.h"
#include "tree-dump.h"
#include "gimple.h"

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
  gimple_seq new_body;

  /* FN must already be in GIMPLE form.  */
  gcc_assert (gimple_body (fn));

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
  id.eh_region = -1;

  /* Actually copy the body.  */
  new_body = remap_gimple_seq (gimple_body (fn), &id);
  gimple_set_body (clone, new_body);
}

/* FN is a function that has a complete body.  Clone the body as
   necessary.  Returns nonzero if there's no longer any need to
   process the main body.  */

bool
maybe_clone_body (tree fn)
{
  tree clone;
  bool first = true;

  /* We only clone constructors and destructors.  */
  if (!DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (fn)
      && !DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (fn))
    return 0;

  /* Emit the DWARF1 abstract instance.  */
  (*debug_hooks->deferred_inline_function) (fn);

  /* We know that any clones immediately follow FN in the TYPE_METHODS
     list.  */
  push_to_top_level ();
  FOR_EACH_CLONE (clone, fn)
    {
      tree parm;
      tree clone_parm;
      int parmno;
      struct pointer_map_t *decl_map;

      /* Update CLONE's source position information to match FN's.  */
      DECL_SOURCE_LOCATION (clone) = DECL_SOURCE_LOCATION (fn);
      DECL_DECLARED_INLINE_P (clone) = DECL_DECLARED_INLINE_P (fn);
      DECL_COMDAT (clone) = DECL_COMDAT (fn);
      DECL_WEAK (clone) = DECL_WEAK (fn);
      DECL_ONE_ONLY (clone) = DECL_ONE_ONLY (fn);
      DECL_SECTION_NAME (clone) = DECL_SECTION_NAME (fn);
      DECL_USE_TEMPLATE (clone) = DECL_USE_TEMPLATE (fn);
      DECL_EXTERNAL (clone) = DECL_EXTERNAL (fn);
      DECL_INTERFACE_KNOWN (clone) = DECL_INTERFACE_KNOWN (fn);
      DECL_NOT_REALLY_EXTERN (clone) = DECL_NOT_REALLY_EXTERN (fn);
      TREE_PUBLIC (clone) = TREE_PUBLIC (fn);
      DECL_VISIBILITY (clone) = DECL_VISIBILITY (fn);
      DECL_VISIBILITY_SPECIFIED (clone) = DECL_VISIBILITY_SPECIFIED (fn);
      DECL_DLLIMPORT_P (clone) = DECL_DLLIMPORT_P (fn);

      /* Adjust the parameter names and locations.  */
      parm = DECL_ARGUMENTS (fn);
      clone_parm = DECL_ARGUMENTS (clone);
      /* Update the `this' parameter, which is always first.  */
      update_cloned_parm (parm, clone_parm, first);
      parm = TREE_CHAIN (parm);
      clone_parm = TREE_CHAIN (clone_parm);
      if (DECL_HAS_IN_CHARGE_PARM_P (fn))
	parm = TREE_CHAIN (parm);
      if (DECL_HAS_VTT_PARM_P (fn))
	parm = TREE_CHAIN (parm);
      if (DECL_HAS_VTT_PARM_P (clone))
	clone_parm = TREE_CHAIN (clone_parm);
      for (; parm;
	   parm = TREE_CHAIN (parm), clone_parm = TREE_CHAIN (clone_parm))
	/* Update this parameter.  */
	update_cloned_parm (parm, clone_parm, first);

      /* Start processing the function.  */
      start_preparsed_function (clone, NULL_TREE, SF_PRE_PARSED);

      /* Remap the parameters.  */
      decl_map = pointer_map_create ();
      for (parmno = 0,
	     parm = DECL_ARGUMENTS (fn),
	     clone_parm = DECL_ARGUMENTS (clone);
	   parm;
	   ++parmno,
	     parm = TREE_CHAIN (parm))
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
		  clone_parm = TREE_CHAIN (clone_parm);
		}
	      /* Otherwise, map the VTT parameter to `NULL'.  */
	      else
		*pointer_map_insert (decl_map, parm) = null_pointer_node;
	    }
	  /* Map other parameters to their equivalents in the cloned
	     function.  */
	  else
	    {
	      *pointer_map_insert (decl_map, parm) = clone_parm;
	      clone_parm = TREE_CHAIN (clone_parm);
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

      /* The clone can throw iff the original function can throw.  */
      cp_function_chain->can_throw = !TREE_NOTHROW (fn);

      /* Now, expand this function into RTL, if appropriate.  */
      finish_function (0);
      BLOCK_ABSTRACT_ORIGIN (DECL_INITIAL (clone)) = DECL_INITIAL (fn);
      DECL_SAVED_TREE (clone) = NULL;
      expand_or_defer_fn (clone);
      first = false;
    }
  pop_from_top_level ();

  /* We don't need to process the original function any further.  */
  return 1;
}
