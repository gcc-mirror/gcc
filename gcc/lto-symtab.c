/* LTO symbol table.
   Copyright 2009, 2010 Free Software Foundation, Inc.
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

/* Vector to keep track of external variables we've seen so far.  */
VEC(tree,gc) *lto_global_var_decls;

/* Symbol table entry.  */

struct GTY(()) lto_symtab_entry_def
{
  /* The symbol table entry key, an IDENTIFIER.  */
  tree id;
  /* The symbol table entry, a DECL.  */
  tree decl;
  /* The cgraph node if decl is a function decl.  Filled in during the
     merging process.  */
  struct cgraph_node *node;
  /* The varpool node if decl is a variable decl.  Filled in during the
     merging process.  */
  struct varpool_node *vnode;
  /* LTO file-data and symbol resolution for this decl.  */
  struct lto_file_decl_data * GTY((skip (""))) file_data;
  enum ld_plugin_symbol_resolution resolution;
  /* True when resolution was guessed and not read from the file.  */
  bool guessed;
  /* Pointer to the next entry with the same key.  Before decl merging
     this links all symbols from the different TUs.  After decl merging
     this links merged but incompatible decls, thus all prevailing ones
     remaining.  */
  struct lto_symtab_entry_def *next;
};
typedef struct lto_symtab_entry_def *lto_symtab_entry_t;

/* A poor man's symbol table. This hashes identifier to prevailing DECL
   if there is one. */

static GTY ((if_marked ("lto_symtab_entry_marked_p"),
	     param_is (struct lto_symtab_entry_def)))
  htab_t lto_symtab_identifiers;

/* Free symtab hashtable.  */

void
lto_symtab_free (void)
{
  htab_delete (lto_symtab_identifiers);
  lto_symtab_identifiers = NULL;
}

/* Return the hash value of an lto_symtab_entry_t object pointed to by P.  */

static hashval_t
lto_symtab_entry_hash (const void *p)
{
  const struct lto_symtab_entry_def *base =
    (const struct lto_symtab_entry_def *) p;
  return IDENTIFIER_HASH_VALUE (base->id);
}

/* Return non-zero if P1 and P2 points to lto_symtab_entry_def structs
   corresponding to the same symbol.  */

static int
lto_symtab_entry_eq (const void *p1, const void *p2)
{
  const struct lto_symtab_entry_def *base1 =
     (const struct lto_symtab_entry_def *) p1;
  const struct lto_symtab_entry_def *base2 =
     (const struct lto_symtab_entry_def *) p2;
  return (base1->id == base2->id);
}

/* Returns non-zero if P points to an lto_symtab_entry_def struct that needs
   to be marked for GC.  */

static int
lto_symtab_entry_marked_p (const void *p)
{
  const struct lto_symtab_entry_def *base =
     (const struct lto_symtab_entry_def *) p;

  /* Keep this only if the common IDENTIFIER_NODE of the symtab chain
     is marked which it will be if at least one of the DECLs in the
     chain is marked.  */
  return ggc_marked_p (base->id);
}

/* Lazily initialize resolution hash tables.  */

static void
lto_symtab_maybe_init_hash_table (void)
{
  if (lto_symtab_identifiers)
    return;

  lto_symtab_identifiers =
    htab_create_ggc (1021, lto_symtab_entry_hash,
		     lto_symtab_entry_eq, NULL);
}

/* Registers DECL with the LTO symbol table as having resolution RESOLUTION
   and read from FILE_DATA. */

void
lto_symtab_register_decl (tree decl,
			  ld_plugin_symbol_resolution_t resolution,
			  struct lto_file_decl_data *file_data)
{
  lto_symtab_entry_t new_entry;
  void **slot;

  /* Check that declarations reaching this function do not have
     properties inconsistent with having external linkage.  If any of
     these asertions fail, then the object file reader has failed to
     detect these cases and issue appropriate error messages.  */
  gcc_assert (decl
	      && TREE_PUBLIC (decl)
	      && (TREE_CODE (decl) == VAR_DECL
		  || TREE_CODE (decl) == FUNCTION_DECL)
	      && DECL_ASSEMBLER_NAME_SET_P (decl));
  if (TREE_CODE (decl) == VAR_DECL
      && DECL_INITIAL (decl))
    gcc_assert (!DECL_EXTERNAL (decl)
		|| (TREE_STATIC (decl) && TREE_READONLY (decl)));
  if (TREE_CODE (decl) == FUNCTION_DECL)
    gcc_assert (!DECL_ABSTRACT (decl));

  new_entry = ggc_alloc_cleared_lto_symtab_entry_def ();
  new_entry->id = (*targetm.asm_out.mangle_assembler_name)
		  (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
  new_entry->decl = decl;
  new_entry->resolution = resolution;
  new_entry->file_data = file_data;

  lto_symtab_maybe_init_hash_table ();
  slot = htab_find_slot (lto_symtab_identifiers, new_entry, INSERT);
  new_entry->next = (lto_symtab_entry_t) *slot;
  *slot = new_entry;
}

/* Get the lto_symtab_entry_def struct associated with ID
   if there is one.  */

static lto_symtab_entry_t
lto_symtab_get (tree id)
{
  struct lto_symtab_entry_def temp;
  void **slot;

  lto_symtab_maybe_init_hash_table ();
  temp.id = id;
  slot = htab_find_slot (lto_symtab_identifiers, &temp, NO_INSERT);
  return slot ? (lto_symtab_entry_t) *slot : NULL;
}

/* Get the linker resolution for DECL.  */

enum ld_plugin_symbol_resolution
lto_symtab_get_resolution (tree decl)
{
  lto_symtab_entry_t e;

  gcc_assert (DECL_ASSEMBLER_NAME_SET_P (decl));

  e = lto_symtab_get ((*targetm.asm_out.mangle_assembler_name)
		      (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl))));
  while (e && e->decl != decl)
    e = e->next;
  if (!e)
    return LDPR_UNKNOWN;

  return e->resolution;
}


/* Replace the cgraph node NODE with PREVAILING_NODE in the cgraph, merging
   all edges and removing the old node.  */

static void
lto_cgraph_replace_node (struct cgraph_node *node,
			 struct cgraph_node *prevailing_node)
{
  struct cgraph_edge *e, *next;
  bool no_aliases_please = false;
  bool compatible_p;

  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Replacing cgraph node %s/%i by %s/%i"
 	       " for symbol %s\n",
	       cgraph_node_name (node), node->uid,
	       cgraph_node_name (prevailing_node),
	       prevailing_node->uid,
	       IDENTIFIER_POINTER ((*targetm.asm_out.mangle_assembler_name)
		 (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->decl)))));
    }

  if (prevailing_node->same_body_alias)
    {
      if (prevailing_node->thunk.thunk_p)
	no_aliases_please = true;
      prevailing_node = prevailing_node->same_body;
    }

  /* Merge node flags.  */
  if (node->needed)
    cgraph_mark_needed_node (prevailing_node);
  if (node->reachable)
    cgraph_mark_reachable_node (prevailing_node);
  if (node->address_taken)
    {
      gcc_assert (!prevailing_node->global.inlined_to);
      cgraph_mark_address_taken_node (prevailing_node);
    }

  /* Redirect all incoming edges.  */
  compatible_p
    = gimple_types_compatible_p (TREE_TYPE (TREE_TYPE (prevailing_node->decl)),
				 TREE_TYPE (TREE_TYPE (node->decl)), GTC_DIAG);
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
  ipa_clone_refering (prevailing_node, NULL, &node->ref_list);

  /* If we have aliases, redirect them to the prevailing node.  */
  if (!node->same_body_alias && node->same_body)
    {
      struct cgraph_node *alias, *last;
      /* We prevail aliases/tunks by a thunk.  This is doable but
         would need thunk combination.  Hopefully no ABI changes will
         every be crazy enough.  */
      gcc_assert (!no_aliases_please);

      for (alias = node->same_body; alias; alias = alias->next)
	{
	  last = alias;
	  gcc_assert (alias->same_body_alias);
	  alias->same_body = prevailing_node;
	}
      last->next = prevailing_node->same_body;
      /* Node with aliases is prevailed by alias.
	 We could handle this, but combining thunks together will be tricky.
	 Hopefully this does not happen.  */
      if (prevailing_node->same_body)
	prevailing_node->same_body->previous = last;
      prevailing_node->same_body = node->same_body;
      node->same_body = NULL;
    }

  /* Finally remove the replaced node.  */
  if (node->same_body_alias)
    cgraph_remove_same_body_alias (node);
  else
    cgraph_remove_node (node);
}

/* Replace the cgraph node NODE with PREVAILING_NODE in the cgraph, merging
   all edges and removing the old node.  */

static void
lto_varpool_replace_node (struct varpool_node *vnode,
			  struct varpool_node *prevailing_node)
{
  /* Merge node flags.  */
  if (vnode->needed)
    {
      gcc_assert (!vnode->analyzed || prevailing_node->analyzed);
      varpool_mark_needed_node (prevailing_node);
    }
  /* Relink aliases.  */
  if (vnode->extra_name && !vnode->alias)
    {
      struct varpool_node *alias, *last;
      for (alias = vnode->extra_name;
	   alias; alias = alias->next)
	{
	  last = alias;
	  alias->extra_name = prevailing_node;
	}

      if (prevailing_node->extra_name)
	{
	  last->next = prevailing_node->extra_name;
	  prevailing_node->extra_name->prev = last;
	}
      prevailing_node->extra_name = vnode->extra_name;
      vnode->extra_name = NULL;
    }
  gcc_assert (!vnode->finalized || prevailing_node->finalized);
  gcc_assert (!vnode->analyzed || prevailing_node->analyzed);

  /* When replacing by an alias, the references goes to the original
     variable.  */
  if (prevailing_node->alias && prevailing_node->extra_name)
    prevailing_node = prevailing_node->extra_name;
  ipa_clone_refering (NULL, prevailing_node, &vnode->ref_list);

  /* Be sure we can garbage collect the initializer.  */
  if (DECL_INITIAL (vnode->decl))
    DECL_INITIAL (vnode->decl) = error_mark_node;
  /* Finally remove the replaced node.  */
  varpool_remove_node (vnode);
}

/* Merge two variable or function symbol table entries PREVAILING and ENTRY.
   Return false if the symbols are not fully compatible and a diagnostic
   should be emitted.  */

static bool
lto_symtab_merge (lto_symtab_entry_t prevailing, lto_symtab_entry_t entry)
{
  tree prevailing_decl = prevailing->decl;
  tree decl = entry->decl;
  tree prevailing_type, type;

  /* Merge decl state in both directions, we may still end up using
     the new decl.  */
  TREE_ADDRESSABLE (prevailing_decl) |= TREE_ADDRESSABLE (decl);
  TREE_ADDRESSABLE (decl) |= TREE_ADDRESSABLE (prevailing_decl);

  /* The linker may ask us to combine two incompatible symbols.
     Detect this case and notify the caller of required diagnostics.  */

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (!gimple_types_compatible_p (TREE_TYPE (prevailing_decl),
				      TREE_TYPE (decl), GTC_DIAG))
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

  /* We have to register and fetch canonical types here as the global
     fixup process didn't yet run.  */
  prevailing_type = gimple_register_type (prevailing_type);
  type = gimple_register_type (type);
  if (!gimple_types_compatible_p (prevailing_type, type, GTC_DIAG))
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

	  if (!gimple_types_compatible_p (gimple_register_type (tem1),
					  gimple_register_type (tem2),
					  GTC_DIAG))
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
lto_symtab_resolve_replaceable_p (lto_symtab_entry_t e)
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

/* Return true if the symtab entry E can be the prevailing one.  */

static bool
lto_symtab_resolve_can_prevail_p (lto_symtab_entry_t e)
{
  /* The C++ frontend ends up neither setting TREE_STATIC nor
     DECL_EXTERNAL on virtual methods but only TREE_PUBLIC.
     So do not reject !TREE_STATIC here but only DECL_EXTERNAL.  */
  if (DECL_EXTERNAL (e->decl))
    return false;

  /* For functions we need a non-discarded body.  */
  if (TREE_CODE (e->decl) == FUNCTION_DECL)
    return (e->node
	    && (e->node->analyzed
	        || (e->node->same_body_alias && e->node->same_body->analyzed)));

  /* A variable should have a size.  */
  else if (TREE_CODE (e->decl) == VAR_DECL)
    {
      if (!e->vnode)
	return false;
      if (e->vnode->finalized)
	return true;
      return e->vnode->alias && e->vnode->extra_name->finalized;
    }

  gcc_unreachable ();
}

/* Resolve the symbol with the candidates in the chain *SLOT and store
   their resolutions.  */

static void
lto_symtab_resolve_symbols (void **slot)
{
  lto_symtab_entry_t e;
  lto_symtab_entry_t prevailing = NULL;

  /* Always set e->node so that edges are updated to reflect decl merging. */
  for (e = (lto_symtab_entry_t) *slot; e; e = e->next)
    {
      if (TREE_CODE (e->decl) == FUNCTION_DECL)
	e->node = cgraph_get_node_or_alias (e->decl);
      else if (TREE_CODE (e->decl) == VAR_DECL)
	e->vnode = varpool_get_node (e->decl);
    }

  e = (lto_symtab_entry_t) *slot;

  /* If the chain is already resolved there is nothing else to do.  */
  if (e->resolution != LDPR_UNKNOWN)
    return;

  /* Find the single non-replaceable prevailing symbol and
     diagnose ODR violations.  */
  for (e = (lto_symtab_entry_t) *slot; e; e = e->next)
    {
      if (!lto_symtab_resolve_can_prevail_p (e))
	{
	  e->resolution = LDPR_RESOLVED_IR;
          e->guessed = true;
	  continue;
	}

      /* Set a default resolution - the final prevailing one will get
         adjusted later.  */
      e->resolution = LDPR_PREEMPTED_IR;
      e->guessed = true;
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
    goto found;

  /* Do a second round choosing one from the replaceable prevailing decls.  */
  for (e = (lto_symtab_entry_t) *slot; e; e = e->next)
    {
      if (e->resolution != LDPR_PREEMPTED_IR)
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
			      DECL_SIZE (e->decl)))
	prevailing = e;
    }

  if (!prevailing)
    return;

found:
  /* If current lto files represent the whole program,
    it is correct to use LDPR_PREVALING_DEF_IRONLY.
    If current lto files are part of whole program, internal
    resolver doesn't know if it is LDPR_PREVAILING_DEF
    or LDPR_PREVAILING_DEF_IRONLY.  Use IRONLY conforms to
    using -fwhole-program.  Otherwise, it doesn't
    matter using either LDPR_PREVAILING_DEF or
    LDPR_PREVAILING_DEF_IRONLY
    
    FIXME: above workaround due to gold plugin makes some
    variables IRONLY, which are indeed PREVAILING_DEF in
    resolution file.  These variables still need manual
    externally_visible attribute.  */
    prevailing->resolution = LDPR_PREVAILING_DEF_IRONLY;
    prevailing->guessed = true;
}

/* Merge all decls in the symbol table chain to the prevailing decl and
   issue diagnostics about type mismatches.  If DIAGNOSED_P is true
   do not issue further diagnostics.*/

static void
lto_symtab_merge_decls_2 (void **slot, bool diagnosed_p)
{
  lto_symtab_entry_t prevailing, e;
  VEC(tree, heap) *mismatches = NULL;
  unsigned i;
  tree decl;

  /* Nothing to do for a single entry.  */
  prevailing = (lto_symtab_entry_t) *slot;
  if (!prevailing->next)
    return;

  /* Try to merge each entry with the prevailing one.  */
  for (e = prevailing->next; e; e = e->next)
    {
      if (!lto_symtab_merge (prevailing, e)
	  && !diagnosed_p)
	VEC_safe_push (tree, heap, mismatches, e->decl);
    }
  if (VEC_empty (tree, mismatches))
    return;

  /* Diagnose all mismatched re-declarations.  */
  FOR_EACH_VEC_ELT (tree, mismatches, i, decl)
    {
      if (!gimple_types_compatible_p (TREE_TYPE (prevailing->decl),
				      TREE_TYPE (decl), GTC_DIAG))
	diagnosed_p |= warning_at (DECL_SOURCE_LOCATION (decl), 0,
				   "type of %qD does not match original "
				   "declaration", decl);

      else if ((DECL_USER_ALIGN (prevailing->decl) && DECL_USER_ALIGN (decl))
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

  VEC_free (tree, heap, mismatches);
}

/* Helper to process the decl chain for the symbol table entry *SLOT.  */

static int
lto_symtab_merge_decls_1 (void **slot, void *data ATTRIBUTE_UNUSED)
{
  lto_symtab_entry_t e, prevailing;
  bool diagnosed_p = false;

  /* Compute the symbol resolutions.  This is a no-op when using the
     linker plugin.  */
  lto_symtab_resolve_symbols (slot);

  /* Find the prevailing decl.  */
  for (prevailing = (lto_symtab_entry_t) *slot;
       prevailing
       && prevailing->resolution != LDPR_PREVAILING_DEF_IRONLY
       && prevailing->resolution != LDPR_PREVAILING_DEF;
       prevailing = prevailing->next)
    ;

  /* Assert it's the only one.  */
  if (prevailing)
    for (e = prevailing->next; e; e = e->next)
      {
	if (e->resolution == LDPR_PREVAILING_DEF_IRONLY
	    || e->resolution == LDPR_PREVAILING_DEF)
	  fatal_error ("multiple prevailing defs for %qE",
		       DECL_NAME (prevailing->decl));
      }

  /* If there's not a prevailing symbol yet it's an external reference.
     Happens a lot during ltrans.  Choose the first symbol with a
     cgraph or a varpool node.  */
  if (!prevailing)
    {
      prevailing = (lto_symtab_entry_t) *slot;
      /* For functions choose one with a cgraph node.  */
      if (TREE_CODE (prevailing->decl) == FUNCTION_DECL)
	while (!prevailing->node
	       && prevailing->next)
	  prevailing = prevailing->next;
      /* For variables chose with a priority variant with vnode
	 attached (i.e. from unit where external declaration of
	 variable is actually used).
	 When there are multiple variants, chose one with size.
	 This is needed for C++ typeinfos, for example in
	 lto/20081204-1 there are typeifos in both units, just
	 one of them do have size.  */
      if (TREE_CODE (prevailing->decl) == VAR_DECL)
	{
	  for (e = prevailing->next; e; e = e->next)
	    if ((!prevailing->vnode && e->vnode)
		|| ((prevailing->vnode != NULL) == (e->vnode != NULL)
		    && !COMPLETE_TYPE_P (TREE_TYPE (prevailing->decl))
		    && COMPLETE_TYPE_P (TREE_TYPE (e->decl))))
	      prevailing = e;
	}
    }

  /* Move it first in the list.  */
  if ((lto_symtab_entry_t) *slot != prevailing)
    {
      for (e = (lto_symtab_entry_t) *slot; e->next != prevailing; e = e->next)
	;
      e->next = prevailing->next;
      prevailing->next = (lto_symtab_entry_t) *slot;
      *slot = (void *) prevailing;
    }

  /* Record the prevailing variable.  */
  if (TREE_CODE (prevailing->decl) == VAR_DECL)
    VEC_safe_push (tree, gc, lto_global_var_decls, prevailing->decl);

  /* Diagnose mismatched objects.  */
  for (e = prevailing->next; e; e = e->next)
    {
      if (TREE_CODE (prevailing->decl) == TREE_CODE (e->decl))
	continue;

      switch (TREE_CODE (prevailing->decl))
	{
	case VAR_DECL:
	  gcc_assert (TREE_CODE (e->decl) == FUNCTION_DECL);
	  error_at (DECL_SOURCE_LOCATION (e->decl),
		    "variable %qD redeclared as function", prevailing->decl);
	  break;

	case FUNCTION_DECL:
	  gcc_assert (TREE_CODE (e->decl) == VAR_DECL);
	  error_at (DECL_SOURCE_LOCATION (e->decl),
		    "function %qD redeclared as variable", prevailing->decl);
	  break;

	default:
	  gcc_unreachable ();
	}

      diagnosed_p = true;
    }
  if (diagnosed_p)
      inform (DECL_SOURCE_LOCATION (prevailing->decl),
	      "previously declared here");

  /* Register and adjust types of the entries.  */
  for (e = (lto_symtab_entry_t) *slot; e; e = e->next)
    TREE_TYPE (e->decl) = gimple_register_type (TREE_TYPE (e->decl));

  /* Merge the chain to the single prevailing decl and diagnose
     mismatches.  */
  lto_symtab_merge_decls_2 (slot, diagnosed_p);

  /* Store resolution decision into the callgraph.  
     In LTRANS don't overwrite information we stored into callgraph at
     WPA stage.

     Do not bother to store guessed decisions.  Generic code knows how
     to handle UNKNOWN relocation well.

     The problem with storing guessed decision is whether to use
     PREVAILING_DEF or PREVAILING_DEF_IRONLY.  First one would disable
     some whole program optimizations, while ther second would imply
     to many whole program assumptions.  */
  if (prevailing->node && !flag_ltrans && !prevailing->guessed)
    prevailing->node->resolution = prevailing->resolution;
  else if (prevailing->vnode && !flag_ltrans && !prevailing->guessed)
    prevailing->vnode->resolution = prevailing->resolution;
  return 1;
}

/* Resolve and merge all symbol table chains to a prevailing decl.  */

void
lto_symtab_merge_decls (void)
{
  lto_symtab_maybe_init_hash_table ();
  htab_traverse (lto_symtab_identifiers, lto_symtab_merge_decls_1, NULL);
}

/* Helper to process the decl chain for the symbol table entry *SLOT.  */

static int
lto_symtab_merge_cgraph_nodes_1 (void **slot, void *data ATTRIBUTE_UNUSED)
{
  lto_symtab_entry_t e, prevailing = (lto_symtab_entry_t) *slot;

  if (!prevailing->next)
    return 1;

  /* Replace the cgraph node of each entry with the prevailing one.  */
  for (e = prevailing->next; e; e = e->next)
    {
      if (e->node != NULL)
	{
	  /* In case we prevail funcion by an alias, we can run into case
	     that the alias has no cgraph node attached, since it was
	     previously unused.  Create the node.  */
	  if (!prevailing->node)
	    {
	      prevailing->node = cgraph_node (prevailing->decl);
	      prevailing->node->alias = true;
	    }
	  lto_cgraph_replace_node (e->node, prevailing->node);
	}
      if (e->vnode != NULL)
	{
	  if (!prevailing->vnode)
	    {
	      prevailing->vnode = varpool_node (prevailing->decl);
	      prevailing->vnode->alias = true;
	    }
	  lto_varpool_replace_node (e->vnode, prevailing->vnode);
	}
    }

  /* Drop all but the prevailing decl from the symtab.  */
  prevailing->next = NULL;

  return 1;
}

/* Merge cgraph nodes according to the symbol merging done by
   lto_symtab_merge_decls.  */

void
lto_symtab_merge_cgraph_nodes (void)
{
  struct cgraph_node *node, *alias, *next;
  lto_symtab_maybe_init_hash_table ();
  htab_traverse (lto_symtab_identifiers, lto_symtab_merge_cgraph_nodes_1, NULL);

  for (node = cgraph_nodes; node; node = node->next)
    for (alias = node->same_body; alias; alias = next)
      {
	next = alias->next;
	alias->thunk.alias = lto_symtab_prevailing_decl (alias->thunk.alias);
      }
}

/* Given the decl DECL, return the prevailing decl with the same name. */

tree
lto_symtab_prevailing_decl (tree decl)
{
  lto_symtab_entry_t ret;

  /* Builtins and local symbols are their own prevailing decl.  */
  if (!TREE_PUBLIC (decl) || is_builtin_fn (decl))
    return decl;

  /* DECL_ABSTRACTs are their own prevailng decl.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_ABSTRACT (decl))
    return decl;

  /* Ensure DECL_ASSEMBLER_NAME will not set assembler name.  */
  gcc_assert (DECL_ASSEMBLER_NAME_SET_P (decl));

  /* Walk through the list of candidates and return the one we merged to.  */
  ret = lto_symtab_get ((*targetm.asm_out.mangle_assembler_name)
			(IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl))));
  if (!ret)
    return NULL_TREE;

  return ret->decl;
}

#include "gt-lto-symtab.h"
