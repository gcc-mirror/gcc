/* LTO symbol table.
   Copyright 2009 Free Software Foundation, Inc.
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
#include "toplev.h"
#include "tree.h"
#include "gimple.h"
#include "ggc.h"	/* lambda.h needs this */
#include "lambda.h"	/* gcd */
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
  /* LTO file-data and symbol resolution for this decl.  */
  struct lto_file_decl_data * GTY((skip (""))) file_data;
  enum ld_plugin_symbol_resolution resolution;
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

/* Return the hash value of an lto_symtab_entry_t object pointed to by P.  */

static hashval_t
lto_symtab_entry_hash (const void *p)
{
  const struct lto_symtab_entry_def *base =
    (const struct lto_symtab_entry_def *) p;
  return htab_hash_pointer (base->id);
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

  /* Keep this only if the decl or the chain is marked.  */
  return (ggc_marked_p (base->decl)
	  || (base->next && ggc_marked_p (base->next)));
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

static bool maybe_merge_incomplete_and_complete_type (tree, tree);

/* Try to merge an incomplete type INCOMPLETE with a complete type
   COMPLETE of same kinds.
   Return true if they were merged, false otherwise.  */

static bool
merge_incomplete_and_complete_type (tree incomplete, tree complete)
{
  /* For merging array types do some extra sanity checking.  */
  if (TREE_CODE (incomplete) == ARRAY_TYPE
      && !maybe_merge_incomplete_and_complete_type (TREE_TYPE (incomplete),
						    TREE_TYPE (complete))
      && !gimple_types_compatible_p (TREE_TYPE (incomplete),
				     TREE_TYPE (complete)))
    return false;

  /* ??? Ideally we would do this by means of a common canonical type, but
     that's difficult as we do not have links from the canonical type
     back to all its children.  */
  gimple_force_type_merge (incomplete, complete);

  return true;
}

/* Try to merge a maybe complete / incomplete type pair TYPE1 and TYPE2.
   Return true if they were merged, false otherwise.  */

static bool
maybe_merge_incomplete_and_complete_type (tree type1, tree type2)
{
  bool res = false;

  if (TREE_CODE (type1) != TREE_CODE (type2))
    return false;

  if (!COMPLETE_TYPE_P (type1) && COMPLETE_TYPE_P (type2))
    res = merge_incomplete_and_complete_type (type1, type2);
  else if (COMPLETE_TYPE_P (type1) && !COMPLETE_TYPE_P (type2))
    res = merge_incomplete_and_complete_type (type2, type1);

  /* Recurse on pointer targets.  */
  if (!res
      && POINTER_TYPE_P (type1)
      && POINTER_TYPE_P (type2))
    res = maybe_merge_incomplete_and_complete_type (TREE_TYPE (type1),
						    TREE_TYPE (type2));

  return res;
}

/* Check if OLD_DECL and NEW_DECL are compatible. */

static bool
lto_symtab_compatible (tree old_decl, tree new_decl)
{
  tree old_type, new_type;

  if (TREE_CODE (old_decl) != TREE_CODE (new_decl))
    {
      switch (TREE_CODE (new_decl))
	{
	case VAR_DECL:
	  gcc_assert (TREE_CODE (old_decl) == FUNCTION_DECL);
	  error_at (DECL_SOURCE_LOCATION (new_decl),
		    "function %qD redeclared as variable", new_decl);
	  inform (DECL_SOURCE_LOCATION (old_decl),
		  "previously declared here");
	  return false;

	case FUNCTION_DECL:
	  gcc_assert (TREE_CODE (old_decl) == VAR_DECL);
	  error_at (DECL_SOURCE_LOCATION (new_decl),
		    "variable %qD redeclared as function", new_decl);
	  inform (DECL_SOURCE_LOCATION (old_decl),
		  "previously declared here");
	  return false;

	default:
	  gcc_unreachable ();
	}
    }

  if (TREE_CODE (new_decl) == FUNCTION_DECL)
    {
      if (!gimple_types_compatible_p (TREE_TYPE (old_decl),
				      TREE_TYPE (new_decl)))
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

  /* Handle external declarations with incomplete type or pointed-to
     incomplete types by forcefully merging the types.
     ???  In principle all types involved in the two decls should
     be merged forcefully, for example without considering type or
     field names.  */
  old_type = TREE_TYPE (old_decl);
  new_type = TREE_TYPE (new_decl);

  if (DECL_EXTERNAL (old_decl) || DECL_EXTERNAL (new_decl))
    maybe_merge_incomplete_and_complete_type (old_type, new_type);
  else if (POINTER_TYPE_P (old_type)
	   && POINTER_TYPE_P (new_type))
    maybe_merge_incomplete_and_complete_type (TREE_TYPE (old_type),
					      TREE_TYPE (new_type));

  /* For array types we have to accept external declarations with
     different sizes than the actual definition (164.gzip).
     ???  We could emit a warning here.  */
  if (TREE_CODE (old_type) == TREE_CODE (new_type)
      && TREE_CODE (old_type) == ARRAY_TYPE
      && COMPLETE_TYPE_P (old_type)
      && COMPLETE_TYPE_P (new_type)
      && tree_int_cst_compare (TYPE_SIZE (old_type),
			       TYPE_SIZE (new_type)) != 0
      && gimple_types_compatible_p (TREE_TYPE (old_type),
				    TREE_TYPE (new_type)))
    {
      /* If only one is external use the type of the non-external decl.
	 Else use the larger one and also adjust the decl size.
	 ???  Directional merging would allow us to simply pick the
	 larger one instead of rewriting it.  */
      if (DECL_EXTERNAL (old_decl) ^ DECL_EXTERNAL (new_decl))
	{
	  if (DECL_EXTERNAL (old_decl))
	    TREE_TYPE (old_decl) = new_type;
	  else if (DECL_EXTERNAL (new_decl))
	    TREE_TYPE (new_decl) = old_type;
	}
      else
	{
	  if (tree_int_cst_compare (TYPE_SIZE (old_type),
				    TYPE_SIZE (new_type)) < 0)
	    {
	      TREE_TYPE (old_decl) = new_type;
	      DECL_SIZE (old_decl) = DECL_SIZE (new_decl);
	      DECL_SIZE_UNIT (old_decl) = DECL_SIZE_UNIT (new_decl);
	    }
	  else
	    {
	      TREE_TYPE (new_decl) = old_type;
	      DECL_SIZE (new_decl) = DECL_SIZE (old_decl);
	      DECL_SIZE_UNIT (new_decl) = DECL_SIZE_UNIT (old_decl);
	    }
	}
    }

  /* We can tolerate differences in type qualification, the
     qualification of the prevailing definition will prevail.  */
  old_type = TYPE_MAIN_VARIANT (TREE_TYPE (old_decl));
  new_type = TYPE_MAIN_VARIANT (TREE_TYPE (new_decl));
  if (!gimple_types_compatible_p (old_type, new_type))
    {
      if (warning_at (DECL_SOURCE_LOCATION (new_decl), 0,
		      "type of %qD does not match original declaration",
		      new_decl))
	inform (DECL_SOURCE_LOCATION (old_decl),
		"previously declared here");
      return false;
    }

  /* ???  We might want to emit a warning here if type qualification
     differences were spotted.  Do not do this unconditionally though.  */

  /* There is no point in comparing too many details of the decls here.
     The type compatibility checks or the completing of types has properly
     dealt with most issues.  */

  /* The following should all not invoke fatal errors as in non-LTO
     mode the linker wouldn't complain either.  Just emit warnings.  */

  /* Report a warning if user-specified alignments do not match.  */
  if ((DECL_USER_ALIGN (old_decl) && DECL_USER_ALIGN (new_decl))
      && DECL_ALIGN (old_decl) != DECL_ALIGN (new_decl))
    {
      warning_at (DECL_SOURCE_LOCATION (new_decl), 0,
		  "alignment of %qD does not match original declaration",
		  new_decl);
      inform (DECL_SOURCE_LOCATION (old_decl), "previously declared here");
      return false;
    }

  return true;
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
  if (TREE_CODE (decl) == VAR_DECL)
    gcc_assert (!(DECL_EXTERNAL (decl) && DECL_INITIAL (decl)));
  if (TREE_CODE (decl) == FUNCTION_DECL)
    gcc_assert (!DECL_ABSTRACT (decl));

  new_entry = GGC_CNEW (struct lto_symtab_entry_def);
  new_entry->id = DECL_ASSEMBLER_NAME (decl);
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

  e = lto_symtab_get (DECL_ASSEMBLER_NAME (decl));
  while (e && e->decl != decl)
    e = e->next;
  if (!e)
    return LDPR_UNKNOWN;

  return e->resolution;
}

/* Replace the cgraph node OLD_NODE with NEW_NODE in the cgraph, merging
   all edges and removing the old node.  */

static void
lto_cgraph_replace_node (struct cgraph_node *old_node,
			 struct cgraph_node *new_node)
{
  struct cgraph_edge *e, *next;

  /* Merge node flags.  */
  if (old_node->needed)
    cgraph_mark_needed_node (new_node);
  if (old_node->reachable)
    cgraph_mark_reachable_node (new_node);
  if (old_node->address_taken)
    cgraph_mark_address_taken_node (new_node);

  /* Redirect all incoming edges.  */
  for (e = old_node->callers; e; e = next)
    {
      next = e->next_caller;
      cgraph_redirect_edge_callee (e, new_node);
    }

  /* There are not supposed to be any outgoing edges from a node we
     replace.  Still this can happen for multiple instances of weak
     functions.
     ???  For now do what the old code did.  Do not create edges for them.  */
  for (e = old_node->callees; e; e = next)
    {
      next = e->next_callee;
      cgraph_remove_edge (e);
    }

  /* Finally remove the replaced node.  */
  cgraph_remove_node (old_node);
}

/* Merge two variable or function symbol table entries ENTRY1 and ENTRY2.
   Return the prevailing one or NULL if a merge is not possible.  */

static lto_symtab_entry_t
lto_symtab_merge (lto_symtab_entry_t entry1, lto_symtab_entry_t entry2)
{
  tree old_decl = entry1->decl;
  tree new_decl = entry2->decl;
  ld_plugin_symbol_resolution_t old_resolution = entry1->resolution;
  ld_plugin_symbol_resolution_t new_resolution = entry2->resolution;
  struct cgraph_node *old_node = NULL;
  struct cgraph_node *new_node = NULL;

  /* Give ODR violation errors.  */
  if (new_resolution == LDPR_PREVAILING_DEF
      || new_resolution == LDPR_PREVAILING_DEF_IRONLY)
    {
      if ((old_resolution == LDPR_PREVAILING_DEF
	   || old_resolution == LDPR_PREVAILING_DEF_IRONLY)
	  && (old_resolution != new_resolution || flag_no_common))
	{
	  error_at (DECL_SOURCE_LOCATION (new_decl),
		    "%qD has already been defined", new_decl);
	  inform (DECL_SOURCE_LOCATION (old_decl),
		  "previously defined here");
	  return NULL;
	}
    }

  /* The linker may ask us to combine two incompatible symbols.  */
  if (!lto_symtab_compatible (old_decl, new_decl))
    return NULL;

  if (TREE_CODE (old_decl) == FUNCTION_DECL)
    old_node = cgraph_get_node (old_decl);
  if (TREE_CODE (new_decl) == FUNCTION_DECL)
    new_node = cgraph_get_node (new_decl);

  /* Merge decl state in both directions, we may still end up using
     the new decl.  */
  TREE_ADDRESSABLE (old_decl) |= TREE_ADDRESSABLE (new_decl);
  TREE_ADDRESSABLE (new_decl) |= TREE_ADDRESSABLE (old_decl);

  gcc_assert (new_resolution != LDPR_UNKNOWN
	      && new_resolution != LDPR_UNDEF
	      && old_resolution != LDPR_UNKNOWN
	      && old_resolution != LDPR_UNDEF);

  if (new_resolution == LDPR_PREVAILING_DEF
      || new_resolution == LDPR_PREVAILING_DEF_IRONLY
      || (!old_node && new_node))
    {
      gcc_assert ((!old_node && new_node)
		  || old_resolution == LDPR_PREEMPTED_IR
		  || old_resolution ==  LDPR_RESOLVED_IR
		  || (old_resolution == new_resolution && !flag_no_common));
      if (old_node)
	lto_cgraph_replace_node (old_node, new_node);
      /* Choose new_decl, entry2.  */
      return entry2;
    }

  if (new_resolution == LDPR_PREEMPTED_REG
      || new_resolution == LDPR_RESOLVED_EXEC
      || new_resolution == LDPR_RESOLVED_DYN)
    gcc_assert (old_resolution == LDPR_PREEMPTED_REG
		|| old_resolution == LDPR_RESOLVED_EXEC
		|| old_resolution == LDPR_RESOLVED_DYN);

  if (new_resolution == LDPR_PREEMPTED_IR
      || new_resolution == LDPR_RESOLVED_IR)
    gcc_assert (old_resolution == LDPR_PREVAILING_DEF
		|| old_resolution == LDPR_PREVAILING_DEF_IRONLY
		|| old_resolution == LDPR_PREEMPTED_IR
		|| old_resolution == LDPR_RESOLVED_IR);

  if (new_node)
    lto_cgraph_replace_node (new_node, old_node);

  /* Choose old_decl, entry1.  */
  return entry1;
}

/* Resolve the symbol with the candidates in the chain *SLOT and store
   their resolutions.  */

static void
lto_symtab_resolve_symbols (void **slot)
{
  lto_symtab_entry_t e = (lto_symtab_entry_t) *slot;

  /* If the chain is already resolved there is nothing to do.  */
  if (e->resolution != LDPR_UNKNOWN)
    return;

  /* This is a poor mans resolver.  */
  for (; e; e = e->next)
    {
      gcc_assert (e->resolution == LDPR_UNKNOWN);
      if (DECL_EXTERNAL (e->decl)
	  || (TREE_CODE (e->decl) == FUNCTION_DECL
	      && !cgraph_get_node (e->decl)))
	e->resolution = LDPR_RESOLVED_IR;
      else
	{
	  if (TREE_READONLY (e->decl))
	    e->resolution = LDPR_PREVAILING_DEF_IRONLY;
	  else
	    e->resolution = LDPR_PREVAILING_DEF;
	}
    }
}

/* Merge one symbol table chain to a (set of) prevailing decls.  */

static void
lto_symtab_merge_decls_2 (void **slot)
{
  lto_symtab_entry_t e2, e1;

  /* Nothing to do for a single entry.  */
  e1 = (lto_symtab_entry_t) *slot;
  if (!e1->next)
    return;

  /* Try to merge each entry with each other entry.  In case of a
     single prevailing decl this is linear.  */
restart:
  for (; e1; e1 = e1->next)
    for (e2 = e1->next; e2; e2 = e2->next)
      {
	lto_symtab_entry_t prevailing = lto_symtab_merge (e1, e2);
	if (prevailing == e1)
	  {
	    lto_symtab_entry_t tmp = prevailing;
	    while (tmp->next != e2)
	      tmp = tmp->next;
	    tmp->next = e2->next;
	    e2->next = NULL;
	    e2 = tmp;
	  }
	else if (prevailing == e2)
	  {
	    lto_symtab_entry_t tmp = (lto_symtab_entry_t) *slot;
	    if (tmp == e1)
	      {
		*slot = e1->next;
		tmp = e1->next;
	      }
	    else
	      {
		while (tmp->next != e1)
		  tmp = tmp->next;
		tmp->next = e1->next;
	      }
	    e1->next = NULL;
	    e1 = tmp;
	    goto restart;
	  }
      }
}

/* Fixup the chain of prevailing variable decls *SLOT that are commonized
   during link-time.  */

static void
lto_symtab_fixup_var_decls (void **slot)
{
  lto_symtab_entry_t e = (lto_symtab_entry_t) *slot;
  tree size = bitsize_zero_node;

  /* Find the largest prevailing decl and move it to the front of the chain.
     This is the decl we will output as representative for the common
     section.  */
  size = bitsize_zero_node;
  if (e->resolution == LDPR_PREVAILING_DEF_IRONLY
      || e->resolution == LDPR_PREVAILING_DEF)
    size = DECL_SIZE (e->decl);
  for (; e->next;)
    {
      lto_symtab_entry_t next = e->next;
      if ((next->resolution == LDPR_PREVAILING_DEF_IRONLY
	   || next->resolution == LDPR_PREVAILING_DEF)
	  && tree_int_cst_lt (size, DECL_SIZE (next->decl)))
	{
	  size = DECL_SIZE (next->decl);
	  e->next = next->next;
	  next->next = (lto_symtab_entry_t) *slot;
	  *slot = next;
	}
      else
	e = next;
    }

  /* Mark everything apart from the first var as written out.  */
  e = (lto_symtab_entry_t) *slot;
  for (e = e->next; e; e = e->next)
    TREE_ASM_WRITTEN (e->decl) = true;
}

/* Helper to process the decl chain for the symbol table entry *SLOT.  */

static int
lto_symtab_merge_decls_1 (void **slot, void *data ATTRIBUTE_UNUSED)
{
  lto_symtab_entry_t e;

  /* Compute the symbol resolutions.  */
  lto_symtab_resolve_symbols (slot);

  /* Register and adjust types of the entries.  */
  for (e = (lto_symtab_entry_t) *slot; e; e = e->next)
    TREE_TYPE (e->decl) = gimple_register_type (TREE_TYPE (e->decl));

  /* Merge the chain to a (hopefully) single prevailing decl.  */
  lto_symtab_merge_decls_2 (slot);

  /* ???  Ideally we should delay all diagnostics until this point to
     avoid duplicates.  */

  /* All done for FUNCTION_DECLs.  */
  e = (lto_symtab_entry_t) *slot;
  if (TREE_CODE (e->decl) == FUNCTION_DECL)
    return 1;

  /* Fixup variables in case there are multiple prevailing ones.  */
  if (e->next)
    lto_symtab_fixup_var_decls (slot);

  /* Insert all variable decls into the global variable decl vector.  */
  for (e = (lto_symtab_entry_t) *slot; e; e = e->next)
    VEC_safe_push (tree, gc, lto_global_var_decls, e->decl);

  return 1;
}

/* Resolve and merge all symbol table chains to a prevailing decl.  */

void
lto_symtab_merge_decls (void)
{
  lto_symtab_maybe_init_hash_table ();
  htab_traverse (lto_symtab_identifiers, lto_symtab_merge_decls_1, NULL);
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
  ret = lto_symtab_get (DECL_ASSEMBLER_NAME (decl));
  if (!ret)
    return NULL_TREE;

  /* If there is only one candidate return it.  */
  if (ret->next == NULL)
    return ret->decl;

  /* If there are multiple decls to choose from find the one we merged
     with and return that.  */
  while (ret)
    {
      if (gimple_types_compatible_p (TREE_TYPE (decl), TREE_TYPE (ret->decl)))
	return ret->decl;

      ret = ret->next;
    }

  gcc_unreachable ();
}

/* Remove any storage used to store resolution of DECL.  */

void
lto_symtab_clear_resolution (tree decl)
{
  struct lto_symtab_entry_def temp;
  lto_symtab_entry_t head;
  void **slot;

  if (!TREE_PUBLIC (decl))
    return;

  /* LTO FIXME: There should be no DECL_ABSTRACT in the middle end. */
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_ABSTRACT (decl))
    return;

  gcc_assert (DECL_ASSEMBLER_NAME_SET_P (decl));

  lto_symtab_maybe_init_hash_table ();
  temp.id = DECL_ASSEMBLER_NAME (decl);
  slot = htab_find_slot (lto_symtab_identifiers, &temp, NO_INSERT);
  if (!*slot)
    return;

  head = (lto_symtab_entry_t) *slot;
  if (head->decl == decl)
    {
      if (head->next)
	{
	  *slot = head->next;
	  head->next = NULL;
	}
      else
	htab_remove_elt (lto_symtab_identifiers, &temp);
    }
  else
    {
      lto_symtab_entry_t e;
      while (head->next && head->next->decl != decl)
	head = head->next;
      if (head->next)
	{
	  e = head->next;
	  head->next = e->next;
	  e->next = NULL;
	}
    }
}

#include "gt-lto-symtab.h"
