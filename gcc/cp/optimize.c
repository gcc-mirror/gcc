/* Perform optimizations on tree structure.
   Copyright (C) 1998-2020 Free Software Foundation, Inc.
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
#include "target.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "cgraph.h"
#include "debug.h"
#include "tree-inline.h"
#include "tree-iterator.h"

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

  DECL_BY_REFERENCE (cloned_parm) = DECL_BY_REFERENCE (parm);

  /* The definition might have different constness.  */
  TREE_READONLY (cloned_parm) = TREE_READONLY (parm);

  TREE_USED (cloned_parm) = !first || TREE_USED (parm);

  /* The name may have changed from the declaration.  */
  DECL_NAME (cloned_parm) = DECL_NAME (parm);
  DECL_SOURCE_LOCATION (cloned_parm) = DECL_SOURCE_LOCATION (parm);
  TREE_TYPE (cloned_parm) = TREE_TYPE (parm);

  DECL_GIMPLE_REG_P (cloned_parm) = DECL_GIMPLE_REG_P (parm);
}

/* Like copy_decl_no_change, but handle DECL_OMP_PRIVATIZED_MEMBER
   properly.  */

static tree
cxx_copy_decl (tree decl, copy_body_data *id)
{
  tree copy = copy_decl_no_change (decl, id);
  if (VAR_P (decl)
      && DECL_HAS_VALUE_EXPR_P (decl)
      && DECL_ARTIFICIAL (decl)
      && DECL_LANG_SPECIFIC (decl)
      && DECL_OMP_PRIVATIZED_MEMBER (decl))
    {
      tree expr = DECL_VALUE_EXPR (copy);
      walk_tree (&expr, copy_tree_body_r, id, NULL);
      SET_DECL_VALUE_EXPR (copy, expr);
    }
  return copy;
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
  id.decl_map = static_cast<hash_map<tree, tree> *> (arg_map);

  id.copy_decl = cxx_copy_decl;
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
  tree parm = DECL_ARGUMENTS (delete_dtor);
  tree virtual_size = cxx_sizeof (current_class_type);

  /* Call the delete function.  */
  tree call_delete = build_op_delete_call (DELETE_EXPR, current_class_ptr,
					   virtual_size,
					   /*global_p=*/false,
					   /*placement=*/NULL_TREE,
					   /*alloc_fn=*/NULL_TREE,
					   tf_warning_or_error);

  tree op = get_callee_fndecl (call_delete);
  if (op && DECL_P (op) && destroying_delete_p (op))
    {
      /* The destroying delete will handle calling complete_dtor.  */
      add_stmt (call_delete);
    }
  else
    {
      /* Call the corresponding complete destructor.  */
      gcc_assert (complete_dtor);
      tree call_dtor = build_cxx_call (complete_dtor, 1, &parm,
				       tf_warning_or_error);

      /* Operator delete must be called, whether or not the dtor throws.  */
      add_stmt (build2 (TRY_FINALLY_EXPR, void_type_node,
			call_dtor, call_delete));
    }

  /* Return the address of the object.
     ??? How is it useful to return an invalid address?  */
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
  tree complete_name = DECL_ASSEMBLER_NAME (complete);
  tree base_name = DECL_ASSEMBLER_NAME (base);
  char *grp_name;
  const char *p, *q;
  bool diff_seen = false;
  size_t idx;
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
		    && (p[idx - 1] == 'C' || p[idx - 1] == 'D'
			|| p[idx - 1] == 'I')
		    && p[idx] == '1'
		    && q[idx] == '2');
	grp_name[idx] = '5';
	diff_seen = true;
      }
  grp_name[idx] = '\0';
  gcc_assert (diff_seen);
  return get_identifier (grp_name);
}

/* Returns true iff we can make the base and complete [cd]tor aliases of
   the same symbol rather than separate functions.  */

static bool
can_alias_cdtor (tree fn)
{
  /* If aliases aren't supported by the assembler, fail.  */
  if (!TARGET_SUPPORTS_ALIASES)
    return false;

  /* We can't use an alias if there are virtual bases.  */
  if (CLASSTYPE_VBASECLASSES (DECL_CONTEXT (fn)))
    return false;
  gcc_assert (DECL_MAYBE_IN_CHARGE_CDTOR_P (fn));
  /* Don't use aliases for weak/linkonce definitions unless we can put both
     symbols in the same COMDAT group.  */
  return (DECL_INTERFACE_KNOWN (fn)
	  && (SUPPORTS_ONE_ONLY || !DECL_WEAK (fn))
	  && (!DECL_ONE_ONLY (fn)
	      || (HAVE_COMDAT_GROUP && DECL_WEAK (fn))));
}

/* FN is a [cd]tor, fns is a pointer to an array of length 3.  Fill fns
   with pointers to the base, complete, and deleting variants.  */

static void
populate_clone_array (tree fn, tree *fns)
{
  tree clone;

  fns[0] = NULL_TREE;
  fns[1] = NULL_TREE;
  fns[2] = NULL_TREE;

  tree ctx = DECL_CONTEXT (fn);

  FOR_EACH_CLONE (clone, fn)
    if (DECL_NAME (clone) == complete_dtor_identifier
	|| DECL_NAME (clone) == complete_ctor_identifier)
      fns[1] = clone;
    else if (DECL_NAME (clone) == base_dtor_identifier
	     || DECL_NAME (clone) == base_ctor_identifier)
      {
	/* We don't need to define the base variants for a final class.  */
	if (!CLASSTYPE_FINAL (ctx))
	  fns[0] = clone;
      }
    else if (DECL_NAME (clone) == deleting_dtor_identifier)
      fns[2] = clone;
    else
      gcc_unreachable ();
}

/* FN is a constructor or destructor, and there are FUNCTION_DECLs
   cloned from it nearby.  Instead of cloning this body, leave it
   alone and create tiny one-call bodies for the cloned
   FUNCTION_DECLs.  These clones are sibcall candidates, and their
   resulting code will be very thunk-esque.  */

static bool
maybe_thunk_body (tree fn, bool force)
{
  tree bind, block, call, clone, clone_result, fn_parm, fn_parm_typelist;
  tree last_arg, modify, *args;
  int parmno, vtt_parmno, max_parms;
  tree fns[3];

  if (!force && !flag_declone_ctor_dtor)
    return 0;

  /* If function accepts variable arguments, give up.  */
  last_arg = tree_last (TYPE_ARG_TYPES (TREE_TYPE (fn)));
  if (last_arg != void_list_node)
    return 0;

  /* If we got this far, we've decided to turn the clones into thunks.  */

  /* We're going to generate code for fn, so it is no longer "abstract."
     Also make the unified ctor/dtor private to either the translation unit
     (for non-vague linkage ctors) or the COMDAT group (otherwise).  */

  populate_clone_array (fn, fns);

  /* Can happen during error recovery (c++/71464).  */
  if (!fns[0] || !fns[1])
    return 0;

  /* Don't use thunks if the base clone omits inherited parameters.  */
  if (ctor_omit_inherited_parms (fns[0]))
    return 0;

  DECL_ABSTRACT_P (fn) = false;
  if (!DECL_WEAK (fn))
    {
      TREE_PUBLIC (fn) = false;
      DECL_EXTERNAL (fn) = false;
      DECL_INTERFACE_KNOWN (fn) = true;
    }
  else if (HAVE_COMDAT_GROUP)
    {
      /* At eof, defer creation of mangling aliases temporarily.  */
      bool save_defer_mangling_aliases = defer_mangling_aliases;
      defer_mangling_aliases = true;
      tree comdat_group = cdtor_comdat_group (fns[1], fns[0]);
      defer_mangling_aliases = save_defer_mangling_aliases;
      cgraph_node::get_create (fns[0])->set_comdat_group (comdat_group);
      cgraph_node::get_create (fns[1])->add_to_same_comdat_group
	(cgraph_node::get_create (fns[0]));
      symtab_node::get (fn)->add_to_same_comdat_group
	(symtab_node::get (fns[0]));
      if (fns[2])
	/* If *[CD][12]* dtors go into the *[CD]5* comdat group and dtor is
	   virtual, it goes into the same comdat group as well.  */
	cgraph_node::get_create (fns[2])->add_to_same_comdat_group
	  (symtab_node::get (fns[0]));
      /* Emit them now that the thunks are same comdat group aliases.  */
      if (!save_defer_mangling_aliases)
	generate_mangling_aliases ();
      TREE_PUBLIC (fn) = false;
      DECL_EXTERNAL (fn) = false;
      DECL_INTERFACE_KNOWN (fn) = true;
      /* function_and_variable_visibility doesn't want !PUBLIC decls to
	 have these flags set.  */
      DECL_WEAK (fn) = false;
      DECL_COMDAT (fn) = false;
    }

  /* Find the vtt_parm, if present.  */
  for (vtt_parmno = -1, parmno = 0, fn_parm = DECL_ARGUMENTS (fn);
       fn_parm;
       ++parmno, fn_parm = TREE_CHAIN (fn_parm))
    {
      if (DECL_ARTIFICIAL (fn_parm)
	  && DECL_NAME (fn_parm) == vtt_parm_identifier)
	{
	  /* Compensate for removed in_charge parameter.  */
	  vtt_parmno = parmno;
	  break;
	}
    }

  /* Allocate an argument buffer for build_cxx_call().
     Make sure it is large enough for any of the clones.  */
  max_parms = 0;
  FOR_EACH_CLONE (clone, fn)
    {
      int length = list_length (DECL_ARGUMENTS (fn));
      if (length > max_parms)
        max_parms = length;
    }
  args = XALLOCAVEC (tree, max_parms);

  /* We know that any clones immediately follow FN in TYPE_FIELDS.  */
  FOR_EACH_CLONE (clone, fn)
    {
      tree clone_parm;

      /* If we've already generated a body for this clone, avoid
	 duplicating it.  (Is it possible for a clone-list to grow after we
	 first see it?)  */
      if (DECL_SAVED_TREE (clone) || TREE_ASM_WRITTEN (clone))
	continue;

      /* Start processing the function.  */
      start_preparsed_function (clone, NULL_TREE, SF_PRE_PARSED);

      if (clone == fns[2])
	{
	  for (clone_parm = DECL_ARGUMENTS (clone); clone_parm;
	       clone_parm = TREE_CHAIN (clone_parm))
	    DECL_ABSTRACT_ORIGIN (clone_parm) = NULL_TREE;
	  /* Build the delete destructor by calling complete destructor and
	     delete function.  */
	  build_delete_destructor_body (clone, fns[1]);
	}
      else
	{
	  /* Walk parameter lists together, creating parameter list for
	     call to original function.  */
	  for (parmno = 0,
		 fn_parm = DECL_ARGUMENTS (fn),
		 fn_parm_typelist = TYPE_ARG_TYPES (TREE_TYPE (fn)),
		 clone_parm = DECL_ARGUMENTS (clone);
	       fn_parm;
	       ++parmno,
		 fn_parm = TREE_CHAIN (fn_parm))
	    {
	      if (parmno == vtt_parmno && ! DECL_HAS_VTT_PARM_P (clone))
		{
		  gcc_assert (fn_parm_typelist);
		  /* Clobber argument with formal parameter type.  */
		  args[parmno]
		    = convert (TREE_VALUE (fn_parm_typelist),
			       null_pointer_node);
		}
	      else if (parmno == 1 && DECL_HAS_IN_CHARGE_PARM_P (fn))
		{
		  tree in_charge
		    = copy_node (in_charge_arg_for_name (DECL_NAME (clone)));
		  args[parmno] = in_charge;
		}
	      /* Map other parameters to their equivalents in the cloned
		 function.  */
	      else
		{
		  gcc_assert (clone_parm);
		  DECL_ABSTRACT_ORIGIN (clone_parm) = NULL;
		  args[parmno] = clone_parm;
		  /* Clear TREE_ADDRESSABLE on thunk arguments.  */
		  TREE_ADDRESSABLE (clone_parm) = 0;
		  clone_parm = TREE_CHAIN (clone_parm);
		}
	      if (fn_parm_typelist)
		fn_parm_typelist = TREE_CHAIN (fn_parm_typelist);
	    }

	  /* We built this list backwards; fix now.  */
	  mark_used (fn);
	  call = build_cxx_call (fn, parmno, args, tf_warning_or_error);
	  /* Arguments passed to the thunk by invisible reference should
	     be transmitted to the callee unchanged.  Do not create a
	     temporary and invoke the copy constructor.  The thunking
	     transformation must not introduce any constructor calls.  */
	  CALL_FROM_THUNK_P (call) = 1;
	  block = make_node (BLOCK);
	  if (targetm.cxx.cdtor_returns_this ())
	    {
	      clone_result = DECL_RESULT (clone);
	      modify = build2 (MODIFY_EXPR, TREE_TYPE (clone_result),
			       clone_result, call);
	      modify = build1 (RETURN_EXPR, void_type_node, modify);
	      add_stmt (modify);
	    }
	  else
	    {
	      add_stmt (call);
	    }
	  bind = c_build_bind_expr (DECL_SOURCE_LOCATION (clone),
				    block, cur_stmt_list);
	  DECL_SAVED_TREE (clone) = push_stmt_list ();
	  add_stmt (bind);
	}

      DECL_ABSTRACT_ORIGIN (clone) = NULL;
      expand_or_defer_fn (finish_function (/*inline_p=*/false));
    }
  return 1;
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
  int idx;
  bool need_alias = false;

  /* We only clone constructors and destructors.  */
  if (!DECL_MAYBE_IN_CHARGE_CDTOR_P (fn))
    return 0;

  populate_clone_array (fn, fns);

  /* Remember if we can't have multiple clones for some reason.  We need to
     check this before we remap local static initializers in clone_body.  */
  if (!tree_versionable_function_p (fn) && fns[0] && fns[1])
    need_alias = true;

  /* We know that any clones immediately follow FN in the TYPE_FIELDS
     list.  */
  push_to_top_level ();
  for (idx = 0; idx < 3; idx++)
    {
      tree parm;
      tree clone_parm;

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
	cgraph_node::get_create (clone)->set_comdat_group (cxx_comdat_group (clone));
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
      set_decl_section_name (clone, DECL_SECTION_NAME (fn));

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
      for (; parm && clone_parm;
	   parm = DECL_CHAIN (parm), clone_parm = DECL_CHAIN (clone_parm))
	/* Update this parameter.  */
	update_cloned_parm (parm, clone_parm, first);
    }

  bool can_alias = can_alias_cdtor (fn);

  /* If we decide to turn clones into thunks, they will branch to fn.
     Must have original function available to call.  */
  if (!can_alias && maybe_thunk_body (fn, need_alias))
    {
      pop_from_top_level ();
      /* We still need to emit the original function.  */
      return 0;
    }

  /* Emit the DWARF1 abstract instance.  */
  (*debug_hooks->deferred_inline_function) (fn);

  /* We know that any clones immediately follow FN in the TYPE_FIELDS. */
  for (idx = 0; idx < 3; idx++)
    {
      tree parm;
      tree clone_parm;
      int parmno;
      hash_map<tree, tree> *decl_map;
      bool alias = false;

      clone = fns[idx];
      if (!clone)
	continue;

      /* Start processing the function.  */
      start_preparsed_function (clone, NULL_TREE, SF_PRE_PARSED);

      /* Tell cgraph if both ctors or both dtors are known to have
	 the same body.  */
      if (can_alias
	  && fns[0]
	  && idx == 1
	  && cgraph_node::get_create (fns[0])->create_same_body_alias
	       (clone, fns[0]))
	{
	  alias = true;
	  if (DECL_ONE_ONLY (fns[0]))
	    {
	      /* For comdat base and complete cdtors put them
		 into the same, *[CD]5* comdat group instead of
		 *[CD][12]*.  */
	      comdat_group = cdtor_comdat_group (fns[1], fns[0]);
	      cgraph_node::get_create (fns[0])->set_comdat_group (comdat_group);
	      if (symtab_node::get (clone)->same_comdat_group)
		symtab_node::get (clone)->remove_from_same_comdat_group ();
	      symtab_node::get (clone)->add_to_same_comdat_group
		(symtab_node::get (fns[0]));
	    }
	}

      /* Build the delete destructor by calling complete destructor
         and delete function.  */
      if (idx == 2)
	{
	  build_delete_destructor_body (clone, fns[1]);
	  /* If *[CD][12]* dtors go into the *[CD]5* comdat group and dtor is
	     virtual, it goes into the same comdat group as well.  */
	  if (comdat_group)
	    cgraph_node::get_create (clone)->add_to_same_comdat_group
	      (symtab_node::get (fns[0]));
	}
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
          decl_map = new hash_map<tree, tree>;
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
                  decl_map->put (parm, in_charge);
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
                      decl_map->put (parm, clone_parm);
                      clone_parm = DECL_CHAIN (clone_parm);
                    }
                  /* Otherwise, map the VTT parameter to `NULL'.  */
                  else
		    {
		      tree t
			= fold_convert (TREE_TYPE (parm), null_pointer_node);
		      decl_map->put (parm, t);
		    }
                }
              /* Map other parameters to their equivalents in the cloned
                 function.  */
              else
                {
		  tree replacement;
		  if (clone_parm)
		    {
		      replacement = clone_parm;
		      clone_parm = DECL_CHAIN (clone_parm);
		    }
		  else
		    {
		      /* Inheriting ctors can omit parameters from the base
			 clone.  Replace them with null lvalues.  */
		      tree reftype = build_reference_type (TREE_TYPE (parm));
		      replacement = fold_convert (reftype, null_pointer_node);
		      replacement = convert_from_reference (replacement);
		    }
                  decl_map->put (parm, replacement);
                }
            }

          if (targetm.cxx.cdtor_returns_this ())
            {
              parm = DECL_RESULT (fn);
              clone_parm = DECL_RESULT (clone);
              decl_map->put (parm, clone_parm);
            }

          /* Clone the body.  */
          clone_body (clone, fn, decl_map);

          /* Clean up.  */
          delete decl_map;
        }

      /* The clone can throw iff the original function can throw.  */
      cp_function_chain->can_throw = !TREE_NOTHROW (fn);

      /* Now, expand this function into RTL, if appropriate.  */
      finish_function (/*inline_p=*/false);
      BLOCK_ABSTRACT_ORIGIN (DECL_INITIAL (clone)) = DECL_INITIAL (fn);
      if (alias)
	{
	  if (expand_or_defer_fn_1 (clone))
	    emit_associated_thunks (clone);
	  /* We didn't generate a body, so remove the empty one.  */
	  DECL_SAVED_TREE (clone) = NULL_TREE;
	}
      else
	expand_or_defer_fn (clone);
      first = false;
    }
  pop_from_top_level ();

  /* We don't need to process the original function any further.  */
  return 1;
}
