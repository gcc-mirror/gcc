/* LTO symbol table.
   Copyright (C) 2009-2017 Free Software Foundation, Inc.
   Contributed by CodeSourcery, Inc.

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
#include "target.h"
#include "function.h"
#include "basic-block.h"
#include "tree.h"
#include "gimple.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "ipa-utils.h"
#include "builtins.h"
#include "alias.h"
#include "lto-symtab.h"
#include "stringpool.h"
#include "attribs.h"

/* Replace the cgraph node NODE with PREVAILING_NODE in the cgraph, merging
   all edges and removing the old node.  */

static void
lto_cgraph_replace_node (struct cgraph_node *node,
			 struct cgraph_node *prevailing_node)
{
  struct cgraph_edge *e, *next;
  bool compatible_p;

  if (symtab->dump_file)
    {
      fprintf (symtab->dump_file, "Replacing cgraph node %s by %s"
 	       " for symbol %s\n",
	       node->dump_name (),
	       prevailing_node->dump_name (),
	       IDENTIFIER_POINTER ((*targetm.asm_out.mangle_assembler_name)
		 (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->decl)))));
    }

  /* Merge node flags.  */
  if (node->force_output)
    prevailing_node->mark_force_output ();
  if (node->forced_by_abi)
    prevailing_node->forced_by_abi = true;
  if (node->address_taken)
    {
      gcc_assert (!prevailing_node->global.inlined_to);
      prevailing_node->mark_address_taken ();
    }
  if (node->definition && prevailing_node->definition
      && DECL_COMDAT (node->decl) && DECL_COMDAT (prevailing_node->decl))
    prevailing_node->merged_comdat = true;

  /* Redirect all incoming edges.  */
  compatible_p
    = types_compatible_p (TREE_TYPE (TREE_TYPE (prevailing_node->decl)),
			  TREE_TYPE (TREE_TYPE (node->decl)));
  for (e = node->callers; e; e = next)
    {
      next = e->next_caller;
      e->redirect_callee (prevailing_node);
      /* If there is a mismatch between the supposed callee return type and
	 the real one do not attempt to inline this function.
	 ???  We really need a way to match function signatures for ABI
	 compatibility and perform related promotions at inlining time.  */
      if (!compatible_p)
	{
	  e->inline_failed = CIF_LTO_MISMATCHED_DECLARATIONS;
	  e->call_stmt_cannot_inline_p = 1;
	}
    }
  /* Redirect incomming references.  */
  prevailing_node->clone_referring (node);

  /* Fix instrumentation references.  */
  if (node->instrumented_version)
    {
      gcc_assert (node->instrumentation_clone
		  == prevailing_node->instrumentation_clone);
      node->instrumented_version->instrumented_version = prevailing_node;
      if (!prevailing_node->instrumented_version)
	prevailing_node->instrumented_version = node->instrumented_version;
      /* Need to reset node->instrumented_version to NULL,
	 otherwise node removal code would reset
	 node->instrumented_version->instrumented_version.  */
      node->instrumented_version = NULL;
    }

  lto_free_function_in_decl_state_for_node (node);

  if (node->decl != prevailing_node->decl)
    node->release_body ();

  /* Finally remove the replaced node.  */
  node->remove ();
}

/* Replace the cgraph node NODE with PREVAILING_NODE in the cgraph, merging
   all edges and removing the old node.  */

static void
lto_varpool_replace_node (varpool_node *vnode,
			  varpool_node *prevailing_node)
{
  gcc_assert (!vnode->definition || prevailing_node->definition);
  gcc_assert (!vnode->analyzed || prevailing_node->analyzed);

  prevailing_node->clone_referring (vnode);
  if (vnode->force_output)
    prevailing_node->force_output = true;
  if (vnode->forced_by_abi)
    prevailing_node->forced_by_abi = true;

  /* Be sure we can garbage collect the initializer.  */
  if (DECL_INITIAL (vnode->decl)
      && vnode->decl != prevailing_node->decl)
    DECL_INITIAL (vnode->decl) = error_mark_node;

  /* Check and report ODR violations on virtual tables.  */
  if (DECL_VIRTUAL_P (vnode->decl) || DECL_VIRTUAL_P (prevailing_node->decl))
    compare_virtual_tables (prevailing_node, vnode);

  if (vnode->tls_model != prevailing_node->tls_model)
    {
      bool error = false;

      /* Non-TLS and TLS never mix together.  Also emulated model is not
	 compatible with anything else.  */
      if (prevailing_node->tls_model == TLS_MODEL_NONE
	  || prevailing_node->tls_model == TLS_MODEL_EMULATED
	  || vnode->tls_model == TLS_MODEL_NONE
	  || vnode->tls_model == TLS_MODEL_EMULATED)
	error = true;
      /* Linked is silently supporting transitions
	 GD -> IE, GD -> LE, LD -> LE, IE -> LE, LD -> IE.
	 Do the same transitions and error out on others.  */
      else if ((prevailing_node->tls_model == TLS_MODEL_REAL
		|| prevailing_node->tls_model == TLS_MODEL_LOCAL_DYNAMIC)
	       && (vnode->tls_model == TLS_MODEL_INITIAL_EXEC
		   || vnode->tls_model == TLS_MODEL_LOCAL_EXEC))
	prevailing_node->tls_model = vnode->tls_model;
      else if ((vnode->tls_model == TLS_MODEL_REAL
		|| vnode->tls_model == TLS_MODEL_LOCAL_DYNAMIC)
	       && (prevailing_node->tls_model == TLS_MODEL_INITIAL_EXEC
		   || prevailing_node->tls_model == TLS_MODEL_LOCAL_EXEC))
	;
      else if (prevailing_node->tls_model == TLS_MODEL_INITIAL_EXEC
	       && vnode->tls_model == TLS_MODEL_LOCAL_EXEC)
	prevailing_node->tls_model = vnode->tls_model;
      else if (vnode->tls_model == TLS_MODEL_INITIAL_EXEC
	       && prevailing_node->tls_model == TLS_MODEL_LOCAL_EXEC)
	;
      else
	error = true;
      if (error)
	{
	  error_at (DECL_SOURCE_LOCATION (vnode->decl),
		    "%qD is defined with tls model %s", vnode->decl, tls_model_names [vnode->tls_model]);
	  inform (DECL_SOURCE_LOCATION (prevailing_node->decl),
		  "previously defined here as %s",
		  tls_model_names [prevailing_node->tls_model]);
	}
    }
  /* Finally remove the replaced node.  */
  vnode->remove ();
}

/* Return non-zero if we want to output waring about T1 and T2.
   Return value is a bitmask of reasons of violation:
   Bit 0 indicates that types are not compatible.
   Bit 1 indicates that types are not compatible because of C++ ODR rule.
   If COMMON_OR_EXTERN is true, do not warn on size mismatches of arrays.
   Bit 2 indicates that types are not ODR compatible

   The interoperability rules are language specific.  At present we do only
   full checking for C++ ODR rule and for other languages we do basic check
   that data structures are of same size and TBAA compatible.  Our TBAA
   implementation should be coarse enough so all valid type transitions
   across different languages are allowed.

   In partiucular we thus allow almost arbitrary type changes with
   -fno-strict-aliasing which may be tough of as a feature rather than bug
   as it allows to implement dodgy tricks in the language runtimes.

   Naturally this code can be strenghtened significantly if we could track
   down the language of origin.  */

static int
warn_type_compatibility_p (tree prevailing_type, tree type,
			   bool common_or_extern)
{
  int lev = 0;
  bool odr_p = odr_or_derived_type_p (prevailing_type)
	       && odr_or_derived_type_p (type);

  if (prevailing_type == type)
    return 0;

  /* C++ provide a robust way to check for type compatibility via the ODR
     rule.  */
  if (odr_p && !odr_types_equivalent_p (prevailing_type, type))
    lev |= 2;

  /* Function types needs special care, because types_compatible_p never
     thinks prototype is compatible to non-prototype.  */
  if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
    {
      if (TREE_CODE (type) != TREE_CODE (prevailing_type))
	lev |= 1;
      lev |= warn_type_compatibility_p (TREE_TYPE (prevailing_type),
				        TREE_TYPE (type), false);
      if (TREE_CODE (type) == METHOD_TYPE
	  && TREE_CODE (prevailing_type) == METHOD_TYPE)
	lev |= warn_type_compatibility_p (TYPE_METHOD_BASETYPE (prevailing_type),
					  TYPE_METHOD_BASETYPE (type), false);
      if (prototype_p (prevailing_type) && prototype_p (type)
	  && TYPE_ARG_TYPES (prevailing_type) != TYPE_ARG_TYPES (type))
	{
	  tree parm1, parm2;
	  for (parm1 = TYPE_ARG_TYPES (prevailing_type),
	       parm2 = TYPE_ARG_TYPES (type);
	       parm1 && parm2;
	       parm1 = TREE_CHAIN (parm1),
	       parm2 = TREE_CHAIN (parm2))
	    lev |= warn_type_compatibility_p (TREE_VALUE (parm1),
					      TREE_VALUE (parm2), false);
	  if (parm1 || parm2)
	    lev |= odr_p ? 3 : 1;
	}
      if (comp_type_attributes (prevailing_type, type) == 0)
	lev |= 1;
      return lev;
    }

  /* Get complete type.  */
  prevailing_type = TYPE_MAIN_VARIANT (prevailing_type);
  type = TYPE_MAIN_VARIANT (type);

  /* We can not use types_compatible_p because we permit some changes
     across types.  For example unsigned size_t and "signed size_t" may be
     compatible when merging C and Fortran types.  */
  if (COMPLETE_TYPE_P (prevailing_type)
      && COMPLETE_TYPE_P (type)
      /* While global declarations are never variadic, we can recurse here
	 for function parameter types.  */
      && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
      && TREE_CODE (TYPE_SIZE (prevailing_type)) == INTEGER_CST
      && !tree_int_cst_equal (TYPE_SIZE (type), TYPE_SIZE (prevailing_type)))
    {
       /* As a special case do not warn about merging
	  int a[];
	  and
	  int a[]={1,2,3};
	  here the first declaration is COMMON or EXTERN
	  and sizeof(a) == sizeof (int).  */
       if (!common_or_extern
	   || TREE_CODE (type) != ARRAY_TYPE
	   || TYPE_SIZE (type) != TYPE_SIZE (TREE_TYPE (type)))
       lev |= 1;
    }

  /* Verify TBAA compatibility.  Take care of alias set 0 and the fact that
     we make ptr_type_node to TBAA compatible with every other type.  */
  if (type_with_alias_set_p (type) && type_with_alias_set_p (prevailing_type))
    {
      alias_set_type set1 = get_alias_set (type);
      alias_set_type set2 = get_alias_set (prevailing_type);

      if (set1 && set2 && set1 != set2 
          && (!POINTER_TYPE_P (type) || !POINTER_TYPE_P (prevailing_type)
	      || (set1 != TYPE_ALIAS_SET (ptr_type_node)
		  && set2 != TYPE_ALIAS_SET (ptr_type_node))))
        lev |= 5;
    }

  return lev;
}

/* Merge two variable or function symbol table entries PREVAILING and ENTRY.
   Return false if the symbols are not fully compatible and a diagnostic
   should be emitted.  */

static bool
lto_symtab_merge (symtab_node *prevailing, symtab_node *entry)
{
  tree prevailing_decl = prevailing->decl;
  tree decl = entry->decl;

  if (prevailing_decl == decl)
    return true;

  if (TREE_CODE (decl) != TREE_CODE (prevailing_decl))
    return false;

  /* Merge decl state in both directions, we may still end up using
     the new decl.  */
  TREE_ADDRESSABLE (prevailing_decl) |= TREE_ADDRESSABLE (decl);
  TREE_ADDRESSABLE (decl) |= TREE_ADDRESSABLE (prevailing_decl);

  /* The linker may ask us to combine two incompatible symbols.
     Detect this case and notify the caller of required diagnostics.  */

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* Merge decl state in both directions, we may still end up using
	 the new decl.  */
      DECL_POSSIBLY_INLINED (prevailing_decl) |= DECL_POSSIBLY_INLINED (decl);
      DECL_POSSIBLY_INLINED (decl) |= DECL_POSSIBLY_INLINED (prevailing_decl);

      if (warn_type_compatibility_p (TREE_TYPE (prevailing_decl),
			             TREE_TYPE (decl),
				     DECL_COMMON (decl)
				     || DECL_EXTERNAL (decl)))
	return false;

      return true;
    }

  if (warn_type_compatibility_p (TREE_TYPE (prevailing_decl),
				 TREE_TYPE (decl),
				 DECL_COMMON (decl) || DECL_EXTERNAL (decl)))
    return false;

  /* There is no point in comparing too many details of the decls here.
     The type compatibility checks or the completing of types has properly
     dealt with most issues.  */

  /* The following should all not invoke fatal errors as in non-LTO
     mode the linker wouldn't complain either.  Just emit warnings.  */

  /* Report a warning if user-specified alignments do not match.  */
  if ((DECL_USER_ALIGN (prevailing_decl) && DECL_USER_ALIGN (decl))
      && DECL_ALIGN (prevailing_decl) < DECL_ALIGN (decl))
    return false;

  if (DECL_SIZE (decl) && DECL_SIZE (prevailing_decl)
      && !tree_int_cst_equal (DECL_SIZE (decl), DECL_SIZE (prevailing_decl))
      /* As a special case do not warn about merging
	 int a[];
	 and
	 int a[]={1,2,3};
	 here the first declaration is COMMON
	 and sizeof(a) == sizeof (int).  */
      && ((!DECL_COMMON (decl) && !DECL_EXTERNAL (decl))
	  || TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE
	  || TYPE_SIZE (TREE_TYPE (decl))
	     != TYPE_SIZE (TREE_TYPE (TREE_TYPE (decl)))))
    return false;

  return true;
}

/* Return true if the symtab entry E can be replaced by another symtab
   entry.  */

static bool
lto_symtab_resolve_replaceable_p (symtab_node *e)
{
  if (DECL_EXTERNAL (e->decl)
      || DECL_COMDAT (e->decl)
      || DECL_ONE_ONLY (e->decl)
      || DECL_WEAK (e->decl))
    return true;

  if (TREE_CODE (e->decl) == VAR_DECL)
    return (DECL_COMMON (e->decl)
	    || (!flag_no_common && !DECL_INITIAL (e->decl)));

  return false;
}

/* Return true, if the symbol E should be resolved by lto-symtab.
   Those are all external symbols and all real symbols that are not static (we
   handle renaming of static later in partitioning).  */

static bool
lto_symtab_symbol_p (symtab_node *e)
{
  if (!TREE_PUBLIC (e->decl) && !DECL_EXTERNAL (e->decl))
    return false;
  return e->real_symbol_p ();
}

/* Return true if the symtab entry E can be the prevailing one.  */

static bool
lto_symtab_resolve_can_prevail_p (symtab_node *e)
{
  if (!lto_symtab_symbol_p (e))
    return false;

  /* The C++ frontend ends up neither setting TREE_STATIC nor
     DECL_EXTERNAL on virtual methods but only TREE_PUBLIC.
     So do not reject !TREE_STATIC here but only DECL_EXTERNAL.  */
  if (DECL_EXTERNAL (e->decl))
    return false;

  return e->definition;
}

/* Resolve the symbol with the candidates in the chain *SLOT and store
   their resolutions.  */

static symtab_node *
lto_symtab_resolve_symbols (symtab_node *first)
{
  symtab_node *e;
  symtab_node *prevailing = NULL;

  /* Always set e->node so that edges are updated to reflect decl merging. */
  for (e = first; e; e = e->next_sharing_asm_name)
    if (lto_symtab_symbol_p (e)
	&& (e->resolution == LDPR_PREVAILING_DEF_IRONLY
	    || e->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP
	    || e->resolution == LDPR_PREVAILING_DEF))
      {
	prevailing = e;
	break;
      }

  /* If the chain is already resolved there is nothing else to do.  */
  if (prevailing)
    {
      /* Assert it's the only one.  */
      for (e = prevailing->next_sharing_asm_name; e; e = e->next_sharing_asm_name)
	if (lto_symtab_symbol_p (e)
	    && (e->resolution == LDPR_PREVAILING_DEF_IRONLY
		|| e->resolution == LDPR_PREVAILING_DEF_IRONLY_EXP
		|| e->resolution == LDPR_PREVAILING_DEF))
	  fatal_error (input_location, "multiple prevailing defs for %qE",
		       DECL_NAME (prevailing->decl));
      return prevailing;
    }

  /* Find the single non-replaceable prevailing symbol and
     diagnose ODR violations.  */
  for (e = first; e; e = e->next_sharing_asm_name)
    {
      if (!lto_symtab_resolve_can_prevail_p (e))
	continue;

      /* If we have a non-replaceable definition it prevails.  */
      if (!lto_symtab_resolve_replaceable_p (e))
	{
	  if (prevailing)
	    {
	      error_at (DECL_SOURCE_LOCATION (e->decl),
			"%qD has already been defined", e->decl);
	      inform (DECL_SOURCE_LOCATION (prevailing->decl),
		      "previously defined here");
	    }
	  prevailing = e;
	}
    }
  if (prevailing)
    return prevailing;

  /* Do a second round choosing one from the replaceable prevailing decls.  */
  for (e = first; e; e = e->next_sharing_asm_name)
    {
      if (!lto_symtab_resolve_can_prevail_p (e))
	continue;

      /* Choose the first function that can prevail as prevailing.  */
      if (TREE_CODE (e->decl) == FUNCTION_DECL)
	{
	  prevailing = e;
	  break;
	}

      /* From variables that can prevail choose the largest one.  */
      if (!prevailing
	  || tree_int_cst_lt (DECL_SIZE (prevailing->decl),
			      DECL_SIZE (e->decl))
	  /* When variables are equivalent try to chose one that has useful
	     DECL_INITIAL.  This makes sense for keyed vtables that are
	     DECL_EXTERNAL but initialized.  In units that do not need them
	     we replace the initializer by error_mark_node to conserve
	     memory.

	     We know that the vtable is keyed outside the LTO unit - otherwise
	     the keyed instance would prevail.  We still can preserve useful
	     info in the initializer.  */
	  || (DECL_SIZE (prevailing->decl) == DECL_SIZE (e->decl)
	      && (DECL_INITIAL (e->decl)
		  && DECL_INITIAL (e->decl) != error_mark_node)
	      && (!DECL_INITIAL (prevailing->decl)
		  || DECL_INITIAL (prevailing->decl) == error_mark_node)))
	prevailing = e;
    }

  return prevailing;
}

/* Decide if it is OK to merge DECL into PREVAILING.
   Because we wrap most of uses of declarations in MEM_REF, we can tolerate
   some differences but other code may inspect directly the DECL.  */

static bool
lto_symtab_merge_p (tree prevailing, tree decl)
{
  if (TREE_CODE (prevailing) != TREE_CODE (decl))
    {
      if (symtab->dump_file)
	fprintf (symtab->dump_file, "Not merging decls; "
		 "TREE_CODE mismatch\n");
      return false;
    }
  gcc_checking_assert (TREE_CHAIN (prevailing) == TREE_CHAIN (decl));
  
  if (TREE_CODE (prevailing) == FUNCTION_DECL)
    {
      if (DECL_BUILT_IN (prevailing) != DECL_BUILT_IN (decl))
	{
          if (symtab->dump_file)
	    fprintf (symtab->dump_file, "Not merging decls; "
		     "DECL_BUILT_IN mismatch\n");
	  return false;
	}
      if (DECL_BUILT_IN (prevailing)
	  && (DECL_BUILT_IN_CLASS (prevailing) != DECL_BUILT_IN_CLASS (decl)
	      || DECL_FUNCTION_CODE (prevailing) != DECL_FUNCTION_CODE (decl)))
	{
          if (symtab->dump_file)
	    fprintf (symtab->dump_file, "Not merging decls; "
		     "DECL_BUILT_IN_CLASS or CODE mismatch\n");
	  return false;
	}
    }
  if (DECL_ATTRIBUTES (prevailing) != DECL_ATTRIBUTES (decl))
    {
      tree prev_attr = lookup_attribute ("error", DECL_ATTRIBUTES (prevailing));
      tree attr = lookup_attribute ("error", DECL_ATTRIBUTES (decl));
      if ((prev_attr == NULL) != (attr == NULL)
	  || (prev_attr
	      && TREE_VALUE (TREE_VALUE (prev_attr))
		 != TREE_VALUE (TREE_VALUE (attr))))
	{
          if (symtab->dump_file)
	    fprintf (symtab->dump_file, "Not merging decls; "
		     "error attribute mismatch\n");
	  return false;
	}

      prev_attr = lookup_attribute ("warning", DECL_ATTRIBUTES (prevailing));
      attr = lookup_attribute ("warning", DECL_ATTRIBUTES (decl));
      if ((prev_attr == NULL) != (attr == NULL)
	  || (prev_attr
	      && TREE_VALUE (TREE_VALUE (prev_attr))
		 != TREE_VALUE (TREE_VALUE (attr))))
	{
          if (symtab->dump_file)
	    fprintf (symtab->dump_file, "Not merging decls; "
		     "warning attribute mismatch\n");
	  return false;
	}
    }
  return true;
}

/* Merge all decls in the symbol table chain to the prevailing decl and
   issue diagnostics about type mismatches.  If DIAGNOSED_P is true
   do not issue further diagnostics.*/

static void
lto_symtab_merge_decls_2 (symtab_node *first, bool diagnosed_p)
{
  symtab_node *prevailing;
  symtab_node *e;
  vec<tree> mismatches = vNULL;
  unsigned i;
  tree decl;
  bool tbaa_p = false;

  /* Nothing to do for a single entry.  */
  prevailing = first;
  if (!prevailing->next_sharing_asm_name)
    return;

  /* Try to merge each entry with the prevailing one.  */
  symtab_node *last_prevailing = prevailing, *next;
  for (e = prevailing->next_sharing_asm_name; e; e = next)
    {
      next = e->next_sharing_asm_name;

      /* Skip non-LTO symbols and symbols whose declaration we already
	 visited.  */
      if (lto_symtab_prevailing_decl (e->decl) != e->decl
	  || !lto_symtab_symbol_p (e)
          || e->decl == prevailing->decl)
	continue;

      if (!lto_symtab_merge (prevailing, e)
	  && !diagnosed_p
	  && !DECL_ARTIFICIAL (e->decl))
	mismatches.safe_push (e->decl);

      symtab_node *this_prevailing;
      for (this_prevailing = prevailing; ;
	   this_prevailing = this_prevailing->next_sharing_asm_name)
	{
	  if (this_prevailing->decl != e->decl
	      && lto_symtab_merge_p (this_prevailing->decl, e->decl))
	    break;
	  if (this_prevailing == last_prevailing)
	    {
	      this_prevailing = NULL;
	      break;
	    }
	}

      if (this_prevailing)
	lto_symtab_prevail_decl (this_prevailing->decl, e->decl);
      /* Maintain LRU list: relink the new prevaililng symbol
	 just after previaling node in the chain and update last_prevailing.
	 Since the number of possible declarations of a given symbol is
	 small, this should be faster than building a hash.  */
      else if (e == prevailing->next_sharing_asm_name)
	last_prevailing = e;
      else
	{
	  if (e->next_sharing_asm_name)
	    e->next_sharing_asm_name->previous_sharing_asm_name
	      = e->previous_sharing_asm_name;
	  e->previous_sharing_asm_name->next_sharing_asm_name
	    = e->next_sharing_asm_name;
	  e->previous_sharing_asm_name = prevailing;
	  e->next_sharing_asm_name = prevailing->next_sharing_asm_name;
	  prevailing->next_sharing_asm_name->previous_sharing_asm_name = e;
	  prevailing->next_sharing_asm_name = e;
	  if (last_prevailing == prevailing)
	    last_prevailing = e;
	}
    }
  if (mismatches.is_empty ())
    return;

  /* Diagnose all mismatched re-declarations.  */
  FOR_EACH_VEC_ELT (mismatches, i, decl)
    {
      /* Do not diagnose two built-in declarations, there is no useful
         location in that case.  It also happens for AVR if two built-ins
         use the same asm name because their libgcc assembler code is the
         same, see PR78562.  */
      if (DECL_IS_BUILTIN (prevailing->decl)
	  && DECL_IS_BUILTIN (decl))
	continue;

      int level = warn_type_compatibility_p (TREE_TYPE (prevailing->decl),
					     TREE_TYPE (decl),
					     DECL_COMDAT (decl));
      if (level)
	{
	  bool diag = false;
	  if (level & 2)
	    diag = warning_at (DECL_SOURCE_LOCATION (decl),
			       OPT_Wodr,
			       "%qD violates the C++ One Definition Rule ",
			       decl);
	  if (!diag && (level & 1))
	    diag = warning_at (DECL_SOURCE_LOCATION (decl),
			       OPT_Wlto_type_mismatch,
			       "type of %qD does not match original "
			       "declaration", decl);
	  if (diag)
	    {
	      warn_types_mismatch (TREE_TYPE (prevailing->decl),
				   TREE_TYPE (decl),
				   DECL_SOURCE_LOCATION (prevailing->decl),
				   DECL_SOURCE_LOCATION (decl));
	      if ((level & 4)
		  && !TREE_READONLY (prevailing->decl))
		tbaa_p = true;
	    }
	  diagnosed_p |= diag;
	}
      else if ((DECL_USER_ALIGN (prevailing->decl)
	        && DECL_USER_ALIGN (decl))
	       && DECL_ALIGN (prevailing->decl) < DECL_ALIGN (decl))
	{
	  diagnosed_p |= warning_at (DECL_SOURCE_LOCATION (decl),
				     OPT_Wlto_type_mismatch,
				     "alignment of %qD is bigger than "
				     "original declaration", decl);
	}
      else
	diagnosed_p |= warning_at (DECL_SOURCE_LOCATION (decl),
				   OPT_Wlto_type_mismatch,
				   "size of %qD differ from the size of "
				   "original declaration", decl);
    }
  if (diagnosed_p)
    inform (DECL_SOURCE_LOCATION (prevailing->decl),
	    "%qD was previously declared here", prevailing->decl);
  if (tbaa_p)
    inform (DECL_SOURCE_LOCATION (prevailing->decl),
	    "code may be misoptimized unless "
	    "-fno-strict-aliasing is used");

  mismatches.release ();
}

/* Helper to process the decl chain for the symbol table entry *SLOT.  */

static void
lto_symtab_merge_decls_1 (symtab_node *first)
{
  symtab_node *e;
  symtab_node *prevailing;
  bool diagnosed_p = false;

  if (symtab->dump_file)
    {
      fprintf (symtab->dump_file, "Merging nodes for %s. Candidates:\n",
	       first->asm_name ());
      for (e = first; e; e = e->next_sharing_asm_name)
	if (TREE_PUBLIC (e->decl))
	  e->dump (symtab->dump_file);
    }

  /* Compute the symbol resolutions.  This is a no-op when using the
     linker plugin and resolution was decided by the linker.  */
  prevailing = lto_symtab_resolve_symbols (first);

  /* If there's not a prevailing symbol yet it's an external reference.
     Happens a lot during ltrans.  Choose the first symbol with a
     cgraph or a varpool node.  */
  if (!prevailing)
    {
      for (prevailing = first;
	   prevailing; prevailing = prevailing->next_sharing_asm_name)
	if (lto_symtab_symbol_p (prevailing))
	  break;
      if (!prevailing)
	return;
      /* For variables chose with a priority variant with vnode
	 attached (i.e. from unit where external declaration of
	 variable is actually used).
	 When there are multiple variants, chose one with size.
	 This is needed for C++ typeinfos, for example in
	 lto/20081204-1 there are typeifos in both units, just
	 one of them do have size.  */
      if (TREE_CODE (prevailing->decl) == VAR_DECL)
	{
	  for (e = prevailing->next_sharing_asm_name;
	       e; e = e->next_sharing_asm_name)
	    if (!COMPLETE_TYPE_P (TREE_TYPE (prevailing->decl))
		&& COMPLETE_TYPE_P (TREE_TYPE (e->decl))
		&& lto_symtab_symbol_p (e))
	      prevailing = e;
	}
      /* For functions prefer the non-builtin if one is available.  */
      else if (TREE_CODE (prevailing->decl) == FUNCTION_DECL)
	{
	  for (e = first; e; e = e->next_sharing_asm_name)
	    if (TREE_CODE (e->decl) == FUNCTION_DECL
		&& !DECL_BUILT_IN (e->decl)
		&& lto_symtab_symbol_p (e))
	      {
		prevailing = e;
		break;
	      }
	}
    }

  symtab->symtab_prevail_in_asm_name_hash (prevailing);

  /* Diagnose mismatched objects.  */
  for (e = prevailing->next_sharing_asm_name;
       e; e = e->next_sharing_asm_name)
    {
      if (TREE_CODE (prevailing->decl)
	  == TREE_CODE (e->decl))
	continue;
      if (!lto_symtab_symbol_p (e))
	continue;

      switch (TREE_CODE (prevailing->decl))
	{
	case VAR_DECL:
	  gcc_assert (TREE_CODE (e->decl) == FUNCTION_DECL);
	  error_at (DECL_SOURCE_LOCATION (e->decl),
		    "variable %qD redeclared as function",
		    prevailing->decl);
	  break;

	case FUNCTION_DECL:
	  gcc_assert (TREE_CODE (e->decl) == VAR_DECL);
	  error_at (DECL_SOURCE_LOCATION (e->decl),
		    "function %qD redeclared as variable",
		    prevailing->decl);
	  break;

	default:
	  gcc_unreachable ();
	}

      diagnosed_p = true;
    }
  if (diagnosed_p)
      inform (DECL_SOURCE_LOCATION (prevailing->decl),
	      "previously declared here");

  /* Merge the chain to the single prevailing decl and diagnose
     mismatches.  */
  lto_symtab_merge_decls_2 (prevailing, diagnosed_p);

  if (symtab->dump_file)
    {
      fprintf (symtab->dump_file, "After resolution:\n");
      for (e = prevailing; e; e = e->next_sharing_asm_name)
	e->dump (symtab->dump_file);
    }
}

/* Resolve and merge all symbol table chains to a prevailing decl.  */

void
lto_symtab_merge_decls (void)
{
  symtab_node *node;

  /* Populate assembler name hash.   */
  symtab->symtab_initialize_asm_name_hash ();

  FOR_EACH_SYMBOL (node)
    if (!node->previous_sharing_asm_name
	&& node->next_sharing_asm_name)
      lto_symtab_merge_decls_1 (node);
}

/* Helper to process the decl chain for the symbol table entry *SLOT.  */

static void
lto_symtab_merge_symbols_1 (symtab_node *prevailing)
{
  symtab_node *e;
  symtab_node *next;

  prevailing->decl->decl_with_vis.symtab_node = prevailing;

  /* Replace the cgraph node of each entry with the prevailing one.  */
  for (e = prevailing->next_sharing_asm_name; e;
       e = next)
    {
      next = e->next_sharing_asm_name;

      if (!lto_symtab_symbol_p (e))
	continue;
      cgraph_node *ce = dyn_cast <cgraph_node *> (e);
      symtab_node *to = symtab_node::get (lto_symtab_prevailing_decl (e->decl));

      /* No matter how we are going to deal with resolution, we will ultimately
	 use prevailing definition.  */
      if (ce)
          ipa_merge_profiles (dyn_cast<cgraph_node *> (prevailing),
			      dyn_cast<cgraph_node *> (e));

      /* If we decided to replace the node by TO, do it.  */
      if (e != to)
	{
	  if (ce)
	    lto_cgraph_replace_node (ce, dyn_cast<cgraph_node *> (to));
	  else if (varpool_node *ve = dyn_cast <varpool_node *> (e))
	    lto_varpool_replace_node (ve, dyn_cast<varpool_node *> (to));
	}
      /* Watch out for duplicated symbols for a given declaration.  */
      else if (!e->transparent_alias
	       || !e->definition || e->get_alias_target () != to)
	{
	  /* We got a new declaration we do not want to merge.  In this case
	     get rid of the existing definition and create a transparent
	     alias.  */
	  if (ce)
	    {
	      lto_free_function_in_decl_state_for_node (ce);
	      if (!ce->weakref)
	        ce->release_body ();
	      ce->reset ();
	      symtab->call_cgraph_removal_hooks (ce);
	    }
	  else
	    {
	      DECL_INITIAL (e->decl) = error_mark_node;
	      if (e->lto_file_data)
		{
		  lto_free_function_in_decl_state_for_node (e);
		  e->lto_file_data = NULL;
		}
	      symtab->call_varpool_removal_hooks (dyn_cast<varpool_node *> (e));
	    }
	  e->remove_all_references ();
	  e->analyzed = e->body_removed = false;
	  e->resolve_alias (prevailing, true);
	  gcc_assert (e != prevailing);
	}
    }

  return;
}

/* Merge cgraph nodes according to the symbol merging done by
   lto_symtab_merge_decls.  */

void
lto_symtab_merge_symbols (void)
{
  symtab_node *node;

  if (!flag_ltrans)
    {
      symtab->symtab_initialize_asm_name_hash ();

      /* Do the actual merging.  
         At this point we invalidate hash translating decls into symtab nodes
	 because after removing one of duplicate decls the hash is not correcly
	 updated to the ohter dupliate.  */
      FOR_EACH_SYMBOL (node)
	if (lto_symtab_symbol_p (node)
	    && node->next_sharing_asm_name
	    && !node->previous_sharing_asm_name)
	  lto_symtab_merge_symbols_1 (node);

      /* Resolve weakref aliases whose target are now in the compilation unit.  
	 also re-populate the hash translating decls into symtab nodes*/
      FOR_EACH_SYMBOL (node)
	{
	  cgraph_node *cnode, *cnode2;
	  varpool_node *vnode;
	  symtab_node *node2;

	  if (!node->analyzed && node->alias_target)
	    {
	      symtab_node *tgt = symtab_node::get_for_asmname (node->alias_target);
	      gcc_assert (node->weakref);
	      if (tgt)
		node->resolve_alias (tgt, true);
	    }
	  /* If the symbol was preempted outside IR, see if we want to get rid
	     of the definition.  */
	  if (node->analyzed
	      && !DECL_EXTERNAL (node->decl)
	      && (node->resolution == LDPR_PREEMPTED_REG
		  || node->resolution == LDPR_RESOLVED_IR
		  || node->resolution == LDPR_RESOLVED_EXEC
		  || node->resolution == LDPR_RESOLVED_DYN))
	    {
	      DECL_EXTERNAL (node->decl) = 1;
	      /* If alias to local symbol was preempted by external definition,
		 we know it is not pointing to the local symbol.  Remove it.  */
	      if (node->alias
		  && !node->weakref
		  && !node->transparent_alias
		  && node->get_alias_target ()->binds_to_current_def_p ())
		{
		  node->alias = false;
		  node->remove_all_references ();
		  node->definition = false;
		  node->analyzed = false;
		  node->cpp_implicit_alias = false;
		}
	      else if (!node->alias
		       && node->definition
		       && node->get_availability () <= AVAIL_INTERPOSABLE)
		{
		  if ((cnode = dyn_cast <cgraph_node *> (node)) != NULL)
		    cnode->reset ();
		  else
		    {
		      node->analyzed = node->definition = false;
		      node->remove_all_references ();
		    }
		}
	    }

	  if (!(cnode = dyn_cast <cgraph_node *> (node))
	      || !cnode->clone_of
	      || cnode->clone_of->decl != cnode->decl)
	    {
	      /* Builtins are not merged via decl merging.  It is however
		 possible that tree merging unified the declaration.  We
		 do not want duplicate entries in symbol table.  */
	      if (cnode && DECL_BUILT_IN (node->decl)
		  && (cnode2 = cgraph_node::get (node->decl))
		  && cnode2 != cnode)
		lto_cgraph_replace_node (cnode2, cnode);

	      /* The user defined assembler variables are also not unified by their
		 symbol name (since it is irrelevant), but we need to unify symbol
		 nodes if tree merging occurred.  */
	      if ((vnode = dyn_cast <varpool_node *> (node))
		  && DECL_HARD_REGISTER (vnode->decl)
		  && (node2 = symtab_node::get (vnode->decl))
		  && node2 != node)
		lto_varpool_replace_node (dyn_cast <varpool_node *> (node2),
					  vnode);
	  

	      /* Abstract functions may have duplicated cgraph nodes attached;
		 remove them.  */
	      else if (cnode && DECL_ABSTRACT_P (cnode->decl)
		       && (cnode2 = cgraph_node::get (node->decl))
		       && cnode2 != cnode)
		cnode2->remove ();

	      node->decl->decl_with_vis.symtab_node = node;
	    }
	}
    }
}

/* Virtual tables may matter for code generation even if they are not
   directly refernced by the code because they may be used for devirtualizaiton.
   For this reason it is important to merge even virtual tables that have no
   associated symbol table entries.  Without doing so we lose optimization
   oppurtunities by losing track of the vtable constructor.
   FIXME: we probably ought to introduce explicit symbol table entries for
   those before streaming.  */

tree
lto_symtab_prevailing_virtual_decl (tree decl)
{
  if (DECL_ABSTRACT_P (decl))
    return decl;
  gcc_checking_assert (!type_in_anonymous_namespace_p (DECL_CONTEXT (decl))
		       && DECL_ASSEMBLER_NAME_SET_P (decl));

  symtab_node *n = symtab_node::get_for_asmname
		     (DECL_ASSEMBLER_NAME (decl));
  while (n && ((!DECL_EXTERNAL (n->decl) && !TREE_PUBLIC (n->decl))
	       || !DECL_VIRTUAL_P (n->decl)))
    n = n->next_sharing_asm_name;
  if (n)
    {
      /* Merge decl state in both directions, we may still end up using
	 the other decl.  */
      TREE_ADDRESSABLE (n->decl) |= TREE_ADDRESSABLE (decl);
      TREE_ADDRESSABLE (decl) |= TREE_ADDRESSABLE (n->decl);

      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  /* Merge decl state in both directions, we may still end up using
	     the other decl.  */
	  DECL_POSSIBLY_INLINED (n->decl) |= DECL_POSSIBLY_INLINED (decl);
	  DECL_POSSIBLY_INLINED (decl) |= DECL_POSSIBLY_INLINED (n->decl);
	}
      lto_symtab_prevail_decl (n->decl, decl);
      decl = n->decl;
    }
  else
    symtab_node::get_create (decl);

  return decl;
}
