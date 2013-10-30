/* LTO symbol table.
   Copyright (C) 2009-2013 Free Software Foundation, Inc.
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
#include "diagnostic-core.h"
#include "tree.h"
#include "gimple.h"
#include "ggc.h"
#include "hashtab.h"
#include "plugin-api.h"
#include "lto-streamer.h"
#include "ipa-utils.h"

/* Replace the cgraph node NODE with PREVAILING_NODE in the cgraph, merging
   all edges and removing the old node.  */

static void
lto_cgraph_replace_node (struct cgraph_node *node,
			 struct cgraph_node *prevailing_node)
{
  struct cgraph_edge *e, *next;
  bool compatible_p;

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Replacing cgraph node %s/%i by %s/%i"
 	       " for symbol %s\n",
	       cgraph_node_name (node), node->order,
	       cgraph_node_name (prevailing_node),
	       prevailing_node->order,
	       IDENTIFIER_POINTER ((*targetm.asm_out.mangle_assembler_name)
		 (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->decl)))));
    }

  /* Merge node flags.  */
  if (node->force_output)
    cgraph_mark_force_output_node (prevailing_node);
  if (node->address_taken)
    {
      gcc_assert (!prevailing_node->global.inlined_to);
      cgraph_mark_address_taken_node (prevailing_node);
    }

  /* Redirect all incoming edges.  */
  compatible_p
    = types_compatible_p (TREE_TYPE (TREE_TYPE (prevailing_node->decl)),
			  TREE_TYPE (TREE_TYPE (node->decl)));
  for (e = node->callers; e; e = next)
    {
      next = e->next_caller;
      cgraph_redirect_edge_callee (e, prevailing_node);
      /* If there is a mismatch between the supposed callee return type and
	 the real one do not attempt to inline this function.
	 ???  We really need a way to match function signatures for ABI
	 compatibility and perform related promotions at inlining time.  */
      if (!compatible_p)
	e->call_stmt_cannot_inline_p = 1;
    }
  /* Redirect incomming references.  */
  ipa_clone_referring (prevailing_node, &node->ref_list);

  ipa_merge_profiles (prevailing_node, node);
  lto_free_function_in_decl_state_for_node (node);

  if (node->decl != prevailing_node->decl)
    cgraph_release_function_body (node);

  /* Finally remove the replaced node.  */
  cgraph_remove_node (node);
}

/* Replace the cgraph node NODE with PREVAILING_NODE in the cgraph, merging
   all edges and removing the old node.  */

static void
lto_varpool_replace_node (struct varpool_node *vnode,
			  struct varpool_node *prevailing_node)
{
  gcc_assert (!vnode->definition || prevailing_node->definition);
  gcc_assert (!vnode->analyzed || prevailing_node->analyzed);

  ipa_clone_referring (prevailing_node, &vnode->ref_list);

  /* Be sure we can garbage collect the initializer.  */
  if (DECL_INITIAL (vnode->decl)
      && vnode->decl != prevailing_node->decl)
    DECL_INITIAL (vnode->decl) = error_mark_node;
  /* Finally remove the replaced node.  */
  varpool_remove_node (vnode);
}

/* Merge two variable or function symbol table entries PREVAILING and ENTRY.
   Return false if the symbols are not fully compatible and a diagnostic
   should be emitted.  */

static bool
lto_symtab_merge (symtab_node prevailing, symtab_node entry)
{
  tree prevailing_decl = prevailing->decl;
  tree decl = entry->decl;
  tree prevailing_type, type;

  if (prevailing_decl == decl)
    return true;

  /* Merge decl state in both directions, we may still end up using
     the new decl.  */
  TREE_ADDRESSABLE (prevailing_decl) |= TREE_ADDRESSABLE (decl);
  TREE_ADDRESSABLE (decl) |= TREE_ADDRESSABLE (prevailing_decl);

  /* The linker may ask us to combine two incompatible symbols.
     Detect this case and notify the caller of required diagnostics.  */

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (!types_compatible_p (TREE_TYPE (prevailing_decl),
			       TREE_TYPE (decl)))
	/* If we don't have a merged type yet...sigh.  The linker
	   wouldn't complain if the types were mismatched, so we
	   probably shouldn't either.  Just use the type from
	   whichever decl appears to be associated with the
	   definition.  If for some odd reason neither decl is, the
	   older one wins.  */
	(void) 0;

      return true;
    }

  /* Now we exclusively deal with VAR_DECLs.  */

  /* Sharing a global symbol is a strong hint that two types are
     compatible.  We could use this information to complete
     incomplete pointed-to types more aggressively here, ignoring
     mismatches in both field and tag names.  It's difficult though
     to guarantee that this does not have side-effects on merging
     more compatible types from other translation units though.  */

  /* We can tolerate differences in type qualification, the
     qualification of the prevailing definition will prevail.
     ???  In principle we might want to only warn for structurally
     incompatible types here, but unless we have protective measures
     for TBAA in place that would hide useful information.  */
  prevailing_type = TYPE_MAIN_VARIANT (TREE_TYPE (prevailing_decl));
  type = TYPE_MAIN_VARIANT (TREE_TYPE (decl));

  if (!types_compatible_p (prevailing_type, type))
    {
      if (COMPLETE_TYPE_P (type))
	return false;

      /* If type is incomplete then avoid warnings in the cases
	 that TBAA handles just fine.  */

      if (TREE_CODE (prevailing_type) != TREE_CODE (type))
	return false;

      if (TREE_CODE (prevailing_type) == ARRAY_TYPE)
	{
	  tree tem1 = TREE_TYPE (prevailing_type);
	  tree tem2 = TREE_TYPE (type);
	  while (TREE_CODE (tem1) == ARRAY_TYPE
		 && TREE_CODE (tem2) == ARRAY_TYPE)
	    {
	      tem1 = TREE_TYPE (tem1);
	      tem2 = TREE_TYPE (tem2);
	    }

	  if (TREE_CODE (tem1) != TREE_CODE (tem2))
	    return false;

	  if (!types_compatible_p (tem1, tem2))
	    return false;
	}

      /* Fallthru.  Compatible enough.  */
    }

  /* ???  We might want to emit a warning here if type qualification
     differences were spotted.  Do not do this unconditionally though.  */

  /* There is no point in comparing too many details of the decls here.
     The type compatibility checks or the completing of types has properly
     dealt with most issues.  */

  /* The following should all not invoke fatal errors as in non-LTO
     mode the linker wouldn't complain either.  Just emit warnings.  */

  /* Report a warning if user-specified alignments do not match.  */
  if ((DECL_USER_ALIGN (prevailing_decl) && DECL_USER_ALIGN (decl))
      && DECL_ALIGN (prevailing_decl) < DECL_ALIGN (decl))
    return false;

  return true;
}

/* Return true if the symtab entry E can be replaced by another symtab
   entry.  */

static bool
lto_symtab_resolve_replaceable_p (symtab_node e)
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
lto_symtab_symbol_p (symtab_node e)
{
  if (!TREE_PUBLIC (e->decl) && !DECL_EXTERNAL (e->decl))
    return false;
  return symtab_real_symbol_p (e);
}

/* Return true if the symtab entry E can be the prevailing one.  */

static bool
lto_symtab_resolve_can_prevail_p (symtab_node e)
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

static symtab_node
lto_symtab_resolve_symbols (symtab_node first)
{
  symtab_node e;
  symtab_node prevailing = NULL;

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
	  fatal_error ("multiple prevailing defs for %qE",
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

/* Merge all decls in the symbol table chain to the prevailing decl and
   issue diagnostics about type mismatches.  If DIAGNOSED_P is true
   do not issue further diagnostics.*/

static void
lto_symtab_merge_decls_2 (symtab_node first, bool diagnosed_p)
{
  symtab_node prevailing;
  symtab_node e;
  vec<tree> mismatches = vNULL;
  unsigned i;
  tree decl;

  /* Nothing to do for a single entry.  */
  prevailing = first;
  if (!prevailing->next_sharing_asm_name)
    return;

  /* Try to merge each entry with the prevailing one.  */
  for (e = prevailing->next_sharing_asm_name;
       e; e = e->next_sharing_asm_name)
    if (TREE_PUBLIC (e->decl))
      {
	if (!lto_symtab_merge (prevailing, e)
	    && !diagnosed_p)
	  mismatches.safe_push (e->decl);
      }
  if (mismatches.is_empty ())
    return;

  /* Diagnose all mismatched re-declarations.  */
  FOR_EACH_VEC_ELT (mismatches, i, decl)
    {
      if (!types_compatible_p (TREE_TYPE (prevailing->decl),
			       TREE_TYPE (decl)))
	diagnosed_p |= warning_at (DECL_SOURCE_LOCATION (decl), 0,
				   "type of %qD does not match original "
				   "declaration", decl);

      else if ((DECL_USER_ALIGN (prevailing->decl)
	        && DECL_USER_ALIGN (decl))
	       && DECL_ALIGN (prevailing->decl) < DECL_ALIGN (decl))
	{
	  diagnosed_p |= warning_at (DECL_SOURCE_LOCATION (decl), 0,
				     "alignment of %qD is bigger than "
				     "original declaration", decl);
	}
    }
  if (diagnosed_p)
    inform (DECL_SOURCE_LOCATION (prevailing->decl),
	    "previously declared here");

  mismatches.release ();
}

/* Helper to process the decl chain for the symbol table entry *SLOT.  */

static void
lto_symtab_merge_decls_1 (symtab_node first)
{
  symtab_node e;
  symtab_node prevailing;
  bool diagnosed_p = false;

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Merging nodes for %s. Candidates:\n",
	       symtab_node_asm_name (first));
      for (e = first; e; e = e->next_sharing_asm_name)
	if (TREE_PUBLIC (e->decl))
	  dump_symtab_node (cgraph_dump_file, e);
    }

  /* Compute the symbol resolutions.  This is a no-op when using the
     linker plugin and resolution was decided by the linker.  */
  prevailing = lto_symtab_resolve_symbols (first);

  /* If there's not a prevailing symbol yet it's an external reference.
     Happens a lot during ltrans.  Choose the first symbol with a
     cgraph or a varpool node.  */
  if (!prevailing)
    {
      prevailing = first;
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
      /* For variables prefer the non-builtin if one is available.  */
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

  symtab_prevail_in_asm_name_hash (prevailing);

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

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "After resolution:\n");
      for (e = prevailing; e; e = e->next_sharing_asm_name)
	dump_symtab_node (cgraph_dump_file, e);
    }
}

/* Resolve and merge all symbol table chains to a prevailing decl.  */

void
lto_symtab_merge_decls (void)
{
  symtab_node node;

  /* Populate assembler name hash.   */
  symtab_initialize_asm_name_hash ();

  FOR_EACH_SYMBOL (node)
    if (!node->previous_sharing_asm_name
	&& node->next_sharing_asm_name)
      lto_symtab_merge_decls_1 (node);
}

/* Helper to process the decl chain for the symbol table entry *SLOT.  */

static void
lto_symtab_merge_symbols_1 (symtab_node prevailing)
{
  symtab_node e;
  symtab_node next;

  /* Replace the cgraph node of each entry with the prevailing one.  */
  for (e = prevailing->next_sharing_asm_name; e;
       e = next)
    {
      next = e->next_sharing_asm_name;

      if (!lto_symtab_symbol_p (e))
	continue;
      cgraph_node *ce = dyn_cast <cgraph_node> (e);
      if (ce && !DECL_BUILT_IN (e->decl))
	lto_cgraph_replace_node (ce, cgraph (prevailing));
      if (varpool_node *ve = dyn_cast <varpool_node> (e))
	lto_varpool_replace_node (ve, varpool (prevailing));
    }

  return;
}

/* Merge cgraph nodes according to the symbol merging done by
   lto_symtab_merge_decls.  */

void
lto_symtab_merge_symbols (void)
{
  symtab_node node;

  if (!flag_ltrans)
    {
      symtab_initialize_asm_name_hash ();

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
	  symtab_node node2;

	  if (!node->analyzed && node->alias_target)
	    {
	      symtab_node tgt = symtab_node_for_asm (node->alias_target);
	      gcc_assert (node->weakref);
	      if (tgt)
		symtab_resolve_alias (node, tgt);
	    }
	  node->aux = NULL;

	  if (!(cnode = dyn_cast <cgraph_node> (node))
	      || !cnode->clone_of
	      || cnode->clone_of->decl != cnode->decl)
	    {
	      /* Builtins are not merged via decl merging.  It is however
		 possible that tree merging unified the declaration.  We
		 do not want duplicate entries in symbol table.  */
	      if (cnode && DECL_BUILT_IN (node->decl)
		  && (cnode2 = cgraph_get_node (node->decl))
		  && cnode2 != cnode)
		lto_cgraph_replace_node (cnode2, cnode);

	      /* The user defined assembler variables are also not unified by their
		 symbol name (since it is irrelevant), but we need to unify symbol
		 nodes if tree merging occured.  */
	      if ((vnode = dyn_cast <varpool_node> (node))
		  && DECL_HARD_REGISTER (vnode->decl)
		  && (node2 = symtab_get_node (vnode->decl))
		  && node2 != node)
		lto_varpool_replace_node (dyn_cast <varpool_node> (node2),
					  vnode);
	  

	      /* Abstract functions may have duplicated cgraph nodes attached;
		 remove them.  */
	      else if (cnode && DECL_ABSTRACT (cnode->decl)
		       && (cnode2 = cgraph_get_node (node->decl))
		       && cnode2 != cnode)
		cgraph_remove_node (cnode2);

	      symtab_insert_node_to_hashtable (node);
	    }
	}
    }
}

/* Given the decl DECL, return the prevailing decl with the same name. */

tree
lto_symtab_prevailing_decl (tree decl)
{
  symtab_node ret;

  /* Builtins and local symbols are their own prevailing decl.  */
  if ((!TREE_PUBLIC (decl) && !DECL_EXTERNAL (decl)) || is_builtin_fn (decl))
    return decl;

  /* DECL_ABSTRACTs are their own prevailng decl.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_ABSTRACT (decl))
    return decl;

  /* Likewise builtins are their own prevailing decl.  This preserves
     non-builtin vs. builtin uses from compile-time.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_BUILT_IN (decl))
    return decl;

  /* Ensure DECL_ASSEMBLER_NAME will not set assembler name.  */
  gcc_assert (DECL_ASSEMBLER_NAME_SET_P (decl));

  /* Walk through the list of candidates and return the one we merged to.  */
  ret = symtab_node_for_asm (DECL_ASSEMBLER_NAME (decl));
  if (!ret)
    return decl;

  return ret->decl;
}
