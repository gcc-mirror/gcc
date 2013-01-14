/* Symbol table.
   Copyright (C) 2012-2013 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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
#include "tree.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "hashtab.h"
#include "ggc.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "timevar.h"
#include "lto-streamer.h"
#include "rtl.h"

const char * const ld_plugin_symbol_resolution_names[]=
{
  "",
  "undef",
  "prevailing_def",
  "prevailing_def_ironly",
  "preempted_reg",
  "preempted_ir",
  "resolved_ir",
  "resolved_exec",
  "resolved_dyn",
  "prevailing_def_ironly_exp"
};

/* Hash table used to convert declarations into nodes.  */
static GTY((param_is (union symtab_node_def))) htab_t symtab_hash;
/* Hash table used to convert assembler names into nodes.  */
static GTY((param_is (union symtab_node_def))) htab_t assembler_name_hash;

/* Linked list of symbol table nodes.  */
symtab_node symtab_nodes;

/* The order index of the next symtab node to be created.  This is
   used so that we can sort the cgraph nodes in order by when we saw
   them, to support -fno-toplevel-reorder.  */
int symtab_order;

/* Returns a hash code for P.  */

static hashval_t
hash_node (const void *p)
{
  const_symtab_node n = (const_symtab_node ) p;
  return (hashval_t) DECL_UID (n->symbol.decl);
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_node (const void *p1, const void *p2)
{
  const_symtab_node n1 = (const_symtab_node) p1;
  const_symtab_node n2 = (const_symtab_node) p2;
  return DECL_UID (n1->symbol.decl) == DECL_UID (n2->symbol.decl);
}

/* Returns a hash code for P.  */

static hashval_t
hash_node_by_assembler_name (const void *p)
{
  const_symtab_node n = (const_symtab_node) p;
  return (hashval_t) decl_assembler_name_hash (DECL_ASSEMBLER_NAME (n->symbol.decl));
}

/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_assembler_name (const void *p1, const void *p2)
{
  const_symtab_node n1 = (const_symtab_node) p1;
  const_tree name = (const_tree)p2;
  return (decl_assembler_name_equal (n1->symbol.decl, name));
}

/* Insert NODE to assembler name hash.  */

static void
insert_to_assembler_name_hash (symtab_node node)
{
  if (is_a <varpool_node> (node) && DECL_HARD_REGISTER (node->symbol.decl))
    return;
  gcc_checking_assert (!node->symbol.previous_sharing_asm_name
		       && !node->symbol.next_sharing_asm_name);
  if (assembler_name_hash)
    {
      void **aslot;
      tree name = DECL_ASSEMBLER_NAME (node->symbol.decl);

      aslot = htab_find_slot_with_hash (assembler_name_hash, name,
					decl_assembler_name_hash (name),
					INSERT);
      gcc_assert (*aslot != node);
      node->symbol.next_sharing_asm_name = (symtab_node)*aslot;
      if (*aslot != NULL)
	((symtab_node)*aslot)->symbol.previous_sharing_asm_name = node;
      *aslot = node;
    }

}

/* Remove NODE from assembler name hash.  */

static void
unlink_from_assembler_name_hash (symtab_node node)
{
  if (assembler_name_hash)
    {
      if (node->symbol.next_sharing_asm_name)
	node->symbol.next_sharing_asm_name->symbol.previous_sharing_asm_name
	  = node->symbol.previous_sharing_asm_name;
      if (node->symbol.previous_sharing_asm_name)
	{
	  node->symbol.previous_sharing_asm_name->symbol.next_sharing_asm_name
	    = node->symbol.next_sharing_asm_name;
	}
      else
	{
	  tree name = DECL_ASSEMBLER_NAME (node->symbol.decl);
          void **slot;
	  slot = htab_find_slot_with_hash (assembler_name_hash, name,
					   decl_assembler_name_hash (name),
					   NO_INSERT);
	  gcc_assert (*slot == node);
	  if (!node->symbol.next_sharing_asm_name)
	    htab_clear_slot (assembler_name_hash, slot);
	  else
	    *slot = node->symbol.next_sharing_asm_name;
	}
      node->symbol.next_sharing_asm_name = NULL;
      node->symbol.previous_sharing_asm_name = NULL;
    }
}

/* Arrange node to be first in its entry of assembler_name_hash.  */

void
symtab_prevail_in_asm_name_hash (symtab_node node)
{
  unlink_from_assembler_name_hash (node);
  insert_to_assembler_name_hash (node);
}


/* Add node into symbol table.  This function is not used directly, but via
   cgraph/varpool node creation routines.  */

void
symtab_register_node (symtab_node node)
{
  struct symtab_node_base key;
  symtab_node *slot;

  node->symbol.next = symtab_nodes;
  node->symbol.previous = NULL;
  if (symtab_nodes)
    symtab_nodes->symbol.previous = node;
  symtab_nodes = node;

  if (!symtab_hash)
    symtab_hash = htab_create_ggc (10, hash_node, eq_node, NULL);
  key.decl = node->symbol.decl;
  slot = (symtab_node *) htab_find_slot (symtab_hash, &key, INSERT);
  if (*slot == NULL)
    *slot = node;

  ipa_empty_ref_list (&node->symbol.ref_list);

  node->symbol.order = symtab_order++;

  /* Be sure to do this last; C++ FE might create new nodes via
     DECL_ASSEMBLER_NAME langhook!  */
  insert_to_assembler_name_hash (node);
}

/* Make NODE to be the one symtab hash is pointing to.  Used when reshaping tree
   of inline clones.  */

void
symtab_insert_node_to_hashtable (symtab_node node)
{
  struct symtab_node_base key;
  symtab_node *slot;

  if (!symtab_hash)
    symtab_hash = htab_create_ggc (10, hash_node, eq_node, NULL);
  key.decl = node->symbol.decl;
  slot = (symtab_node *) htab_find_slot (symtab_hash, &key, INSERT);
  *slot = node;
}

/* Remove node from symbol table.  This function is not used directly, but via
   cgraph/varpool node removal routines.  */

void
symtab_unregister_node (symtab_node node)
{
  void **slot;
  ipa_remove_all_references (&node->symbol.ref_list);
  ipa_remove_all_referring (&node->symbol.ref_list);

  if (node->symbol.same_comdat_group)
    {
      symtab_node prev;
      for (prev = node->symbol.same_comdat_group;
	   prev->symbol.same_comdat_group != node;
	   prev = prev->symbol.same_comdat_group)
	;
      if (node->symbol.same_comdat_group == prev)
	prev->symbol.same_comdat_group = NULL;
      else
	prev->symbol.same_comdat_group = node->symbol.same_comdat_group;
      node->symbol.same_comdat_group = NULL;
    }

  if (node->symbol.previous)
    node->symbol.previous->symbol.next = node->symbol.next;
  else
    symtab_nodes = node->symbol.next;
  if (node->symbol.next)
    node->symbol.next->symbol.previous = node->symbol.previous;
  node->symbol.next = NULL;
  node->symbol.previous = NULL;

  slot = htab_find_slot (symtab_hash, node, NO_INSERT);
  if (*slot == node)
    {
      symtab_node replacement_node = NULL;
      if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
	replacement_node = (symtab_node)cgraph_find_replacement_node (cnode);
      if (!replacement_node)
	htab_clear_slot (symtab_hash, slot);
      else
	*slot = replacement_node;
    }
  unlink_from_assembler_name_hash (node);
}

/* Return symbol table node associated with DECL, if any,
   and NULL otherwise.  */

symtab_node
symtab_get_node (const_tree decl)
{
  symtab_node *slot;
  struct symtab_node_base key;

  gcc_checking_assert (TREE_CODE (decl) == FUNCTION_DECL
		       || (TREE_CODE (decl) == VAR_DECL
			   && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
			       || in_lto_p)));

  if (!symtab_hash)
    return NULL;

  key.decl = CONST_CAST2 (tree, const_tree, decl);

  slot = (symtab_node *) htab_find_slot (symtab_hash, &key,
					 NO_INSERT);

  if (slot)
    return *slot;
  return NULL;
}

/* Remove symtab NODE from the symbol table.  */

void
symtab_remove_node (symtab_node node)
{
  if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
    cgraph_remove_node (cnode);
  else if (varpool_node *vnode = dyn_cast <varpool_node> (node))
    varpool_remove_node (vnode);
}

/* Initalize asm name hash unless.  */

void
symtab_initialize_asm_name_hash (void)
{
  symtab_node node;
  if (!assembler_name_hash)
    {
      assembler_name_hash =
	htab_create_ggc (10, hash_node_by_assembler_name, eq_assembler_name,
			 NULL);
      FOR_EACH_SYMBOL (node)
	insert_to_assembler_name_hash (node);
    }
}

/* Return the cgraph node that has ASMNAME for its DECL_ASSEMBLER_NAME.
   Return NULL if there's no such node.  */

symtab_node
symtab_node_for_asm (const_tree asmname)
{
  symtab_node node;
  void **slot;

  symtab_initialize_asm_name_hash ();
  slot = htab_find_slot_with_hash (assembler_name_hash, asmname,
				   decl_assembler_name_hash (asmname),
				   NO_INSERT);

  if (slot)
    {
      node = (symtab_node) *slot;
      return node;
    }
  return NULL;
}

/* Set the DECL_ASSEMBLER_NAME and update symtab hashtables.  */

void
change_decl_assembler_name (tree decl, tree name)
{
  symtab_node node = NULL;

  /* We can have user ASM names on things, like global register variables, that
     are not in the symbol table.  */
  if ((TREE_CODE (decl) == VAR_DECL
       && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
      || TREE_CODE (decl) == FUNCTION_DECL)
    node = symtab_get_node (decl);
  if (!DECL_ASSEMBLER_NAME_SET_P (decl))
    {
      SET_DECL_ASSEMBLER_NAME (decl, name);
      if (node)
	insert_to_assembler_name_hash (node);
    }
  else
    {
      if (name == DECL_ASSEMBLER_NAME (decl))
	return;

      if (node)
	unlink_from_assembler_name_hash (node);
      if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))
	  && DECL_RTL_SET_P (decl))
	warning (0, "%D renamed after being referenced in assembly", decl);

      SET_DECL_ASSEMBLER_NAME (decl, name);
      if (node)
	insert_to_assembler_name_hash (node);
    }
}

/* Add NEW_ to the same comdat group that OLD is in.  */

void
symtab_add_to_same_comdat_group (symtab_node new_node,
				 symtab_node old_node)
{
  gcc_assert (DECL_ONE_ONLY (old_node->symbol.decl));
  gcc_assert (!new_node->symbol.same_comdat_group);
  gcc_assert (new_node != old_node);

  DECL_COMDAT_GROUP (new_node->symbol.decl) = DECL_COMDAT_GROUP (old_node->symbol.decl);
  new_node->symbol.same_comdat_group = old_node;
  if (!old_node->symbol.same_comdat_group)
    old_node->symbol.same_comdat_group = new_node;
  else
    {
      symtab_node n;
      for (n = old_node->symbol.same_comdat_group;
	   n->symbol.same_comdat_group != old_node;
	   n = n->symbol.same_comdat_group)
	;
      n->symbol.same_comdat_group = new_node;
    }
}

/* Dissolve the same_comdat_group list in which NODE resides.  */

void
symtab_dissolve_same_comdat_group_list (symtab_node node)
{
  symtab_node n = node, next;

  if (!node->symbol.same_comdat_group)
    return;
  do
    {
      next = n->symbol.same_comdat_group;
      n->symbol.same_comdat_group = NULL;
      n = next;
    }
  while (n != node);
}

/* Return printable assembler name of NODE.
   This function is used only for debugging.  When assembler name
   is unknown go with identifier name.  */

const char *
symtab_node_asm_name (symtab_node node)
{
  if (!DECL_ASSEMBLER_NAME_SET_P (node->symbol.decl))
    return lang_hooks.decl_printable_name (node->symbol.decl, 2);
  return IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (node->symbol.decl));
}

/* Return printable identifier name.  */

const char *
symtab_node_name (symtab_node node)
{
  return lang_hooks.decl_printable_name (node->symbol.decl, 2);
}

static const char * const symtab_type_names[] = {"symbol", "function", "variable"};

/* Dump base fields of symtab nodes.  Not to be used directly.  */

void
dump_symtab_base (FILE *f, symtab_node node)
{
  static const char * const visibility_types[] = {
    "default", "protected", "hidden", "internal"
  };

  fprintf (f, "%s/%i (%s)",
	   symtab_node_asm_name (node),
	   node->symbol.order,
	   symtab_node_name (node));
  dump_addr (f, " @", (void *)node);
  fprintf (f, "\n  Type: %s\n", symtab_type_names[node->symbol.type]);
  fprintf (f, "  Visibility:");

  if (node->symbol.in_other_partition)
    fprintf (f, " in_other_partition");
  if (node->symbol.used_from_other_partition)
    fprintf (f, " used_from_other_partition");
  if (node->symbol.force_output)
    fprintf (f, " force_output");
  if (node->symbol.resolution != LDPR_UNKNOWN)
    fprintf (f, " %s",
 	     ld_plugin_symbol_resolution_names[(int)node->symbol.resolution]);
  if (TREE_ASM_WRITTEN (node->symbol.decl))
    fprintf (f, " asm_written");
  if (DECL_EXTERNAL (node->symbol.decl))
    fprintf (f, " external");
  if (TREE_PUBLIC (node->symbol.decl))
    fprintf (f, " public");
  if (DECL_COMMON (node->symbol.decl))
    fprintf (f, " common");
  if (DECL_WEAK (node->symbol.decl))
    fprintf (f, " weak");
  if (DECL_DLLIMPORT_P (node->symbol.decl))
    fprintf (f, " dll_import");
  if (DECL_COMDAT (node->symbol.decl))
    fprintf (f, " comdat");
  if (DECL_COMDAT_GROUP (node->symbol.decl))
    fprintf (f, " comdat_group:%s",
	     IDENTIFIER_POINTER (DECL_COMDAT_GROUP (node->symbol.decl)));
  if (DECL_ONE_ONLY (node->symbol.decl))
    fprintf (f, " one_only");
  if (DECL_SECTION_NAME (node->symbol.decl))
    fprintf (f, " section_name:%s",
	     TREE_STRING_POINTER (DECL_SECTION_NAME (node->symbol.decl)));
  if (DECL_VISIBILITY_SPECIFIED (node->symbol.decl))
    fprintf (f, " visibility_specified");
  if (DECL_VISIBILITY (node->symbol.decl))
    fprintf (f, " visibility:%s",
	     visibility_types [DECL_VISIBILITY (node->symbol.decl)]);
  if (DECL_VIRTUAL_P (node->symbol.decl))
    fprintf (f, " virtual");
  if (DECL_ARTIFICIAL (node->symbol.decl))
    fprintf (f, " artificial");
  if (TREE_CODE (node->symbol.decl) == FUNCTION_DECL)
    {
      if (DECL_STATIC_CONSTRUCTOR (node->symbol.decl))
	fprintf (f, " constructor");
      if (DECL_STATIC_DESTRUCTOR (node->symbol.decl))
	fprintf (f, " destructor");
    }
  fprintf (f, "\n");
  
  if (node->symbol.same_comdat_group)
    fprintf (f, "  Same comdat group as: %s/%i\n",
	     symtab_node_asm_name (node->symbol.same_comdat_group),
	     node->symbol.same_comdat_group->symbol.order);
  if (node->symbol.next_sharing_asm_name)
    fprintf (f, "  next sharing asm name: %i\n",
	     node->symbol.next_sharing_asm_name->symbol.order);
  if (node->symbol.previous_sharing_asm_name)
    fprintf (f, "  previous sharing asm name: %i\n",
	     node->symbol.previous_sharing_asm_name->symbol.order);

  if (node->symbol.address_taken)
    fprintf (f, "  Address is taken.\n");
  if (node->symbol.aux)
    {
      fprintf (f, "  Aux:");
      dump_addr (f, " @", (void *)node->symbol.aux);
    }

  fprintf (f, "  References: ");
  ipa_dump_references (f, &node->symbol.ref_list);
  fprintf (f, "  Referring: ");
  ipa_dump_referring (f, &node->symbol.ref_list);
  if (node->symbol.lto_file_data)
    fprintf (f, "  Read from file: %s\n",
	     node->symbol.lto_file_data->file_name);
}

/* Dump symtab node.  */

void
dump_symtab_node (FILE *f, symtab_node node)
{
  if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
    dump_cgraph_node (f, cnode);
  else if (varpool_node *vnode = dyn_cast <varpool_node> (node))
    dump_varpool_node (f, vnode);
}

/* Dump symbol table.  */

void
dump_symtab (FILE *f)
{
  symtab_node node;
  fprintf (f, "Symbol table:\n\n");
  FOR_EACH_SYMBOL (node)
    dump_symtab_node (f, node);
}

/* Dump symtab node NODE to stderr.  */

DEBUG_FUNCTION void
debug_symtab_node (symtab_node node)
{
  dump_symtab_node (stderr, node);
}

/* Dump symbol table to stderr.  */

DEBUG_FUNCTION void
debug_symtab (void)
{
  dump_symtab (stderr);
}

/* Verify common part of symtab nodes.  */

DEBUG_FUNCTION bool
verify_symtab_base (symtab_node node)
{
  bool error_found = false;
  symtab_node hashed_node;

  if (is_a <cgraph_node> (node))
    {
      if (TREE_CODE (node->symbol.decl) != FUNCTION_DECL)
	{
          error ("function symbol is not function");
          error_found = true;
	}
    }
  else if (is_a <varpool_node> (node))
    {
      if (TREE_CODE (node->symbol.decl) != VAR_DECL)
	{
          error ("variable symbol is not variable");
          error_found = true;
	}
    }
  else
    {
      error ("node has unknown type");
      error_found = true;
    }
   
  hashed_node = symtab_get_node (node->symbol.decl);
  if (!hashed_node)
    {
      error ("node not found in symtab decl hashtable");
      error_found = true;
    }
  if (assembler_name_hash)
    {
      hashed_node = symtab_node_for_asm (DECL_ASSEMBLER_NAME (node->symbol.decl));
      if (hashed_node && hashed_node->symbol.previous_sharing_asm_name)
	{
          error ("assembler name hash list corrupted");
          error_found = true;
	}
      while (hashed_node)
	{
	  if (hashed_node == node)
	    break;
	  hashed_node = hashed_node->symbol.next_sharing_asm_name;
	}
      if (!hashed_node
          && !(is_a <varpool_node> (node)
	       || DECL_HARD_REGISTER (node->symbol.decl)))
	{
          error ("node not found in symtab assembler name hash");
          error_found = true;
	}
    }
  if (node->symbol.previous_sharing_asm_name
      && node->symbol.previous_sharing_asm_name->symbol.next_sharing_asm_name != node)
    {
      error ("double linked list of assembler names corrupted");
    }
  if (node->symbol.same_comdat_group)
    {
      symtab_node n = node->symbol.same_comdat_group;

      if (!DECL_ONE_ONLY (n->symbol.decl))
	{
	  error ("non-DECL_ONE_ONLY node in a same_comdat_group list");
	  error_found = true;
	}
      if (n->symbol.type != node->symbol.type)
	{
	  error ("mixing different types of symbol in same comdat groups is not supported");
	  error_found = true;
	}
      if (n == node)
	{
	  error ("node is alone in a comdat group");
	  error_found = true;
	}
      do
	{
	  if (!n->symbol.same_comdat_group)
	    {
	      error ("same_comdat_group is not a circular list");
	      error_found = true;
	      break;
	    }
	  n = n->symbol.same_comdat_group;
	}
      while (n != node);
    }
  return error_found;
}

/* Verify consistency of NODE.  */

DEBUG_FUNCTION void
verify_symtab_node (symtab_node node)
{
  if (seen_error ())
    return;

  timevar_push (TV_CGRAPH_VERIFY);
  if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
    verify_cgraph_node (cnode);
  else
    if (verify_symtab_base (node))
      {
        dump_symtab_node (stderr, node);
        internal_error ("verify_symtab_node failed");
      }
  timevar_pop (TV_CGRAPH_VERIFY);
}

/* Verify symbol table for internal consistency.  */

DEBUG_FUNCTION void
verify_symtab (void)
{
  symtab_node node;
  FOR_EACH_SYMBOL (node)
   verify_symtab_node (node);
}

/* Return true when RESOLUTION indicate that linker will use
   the symbol from non-LTO object files.  */

bool
resolution_used_from_other_file_p (enum ld_plugin_symbol_resolution resolution)
{
  return (resolution == LDPR_PREVAILING_DEF
          || resolution == LDPR_PREEMPTED_REG
          || resolution == LDPR_RESOLVED_EXEC
          || resolution == LDPR_RESOLVED_DYN);
}

/* Return true when NODE is known to be used from other (non-LTO) object file.
   Known only when doing LTO via linker plugin.  */

bool
symtab_used_from_object_file_p (symtab_node node)
{
  if (!TREE_PUBLIC (node->symbol.decl) || DECL_EXTERNAL (node->symbol.decl))
    return false;
  if (resolution_used_from_other_file_p (node->symbol.resolution))
    return true;
  return false;
}

/* Make DECL local.  FIXME: We shouldn't need to mess with rtl this early,
   but other code such as notice_global_symbol generates rtl.  */
void
symtab_make_decl_local (tree decl)
{
  rtx rtl, symbol;

  if (TREE_CODE (decl) == VAR_DECL)
    DECL_COMMON (decl) = 0;
  else gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);

  if (DECL_ONE_ONLY (decl) || DECL_COMDAT (decl))
    {
      /* It is possible that we are linking against library defining same COMDAT
	 function.  To avoid conflict we need to rename our local name of the
	 function just in the case WHOPR partitioning decide to make it hidden
	 to avoid cross partition references.  */
      if (flag_wpa)
	{
	  const char *old_name;
          symtab_node node = symtab_get_node (decl);
	  old_name  = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
	  change_decl_assembler_name (decl,
				      clone_function_name (decl, "local"));
	  if (node->symbol.lto_file_data)
	    lto_record_renamed_decl (node->symbol.lto_file_data,
				     old_name,
				     IDENTIFIER_POINTER
				       (DECL_ASSEMBLER_NAME (decl)));
	}
      DECL_SECTION_NAME (decl) = 0;
      DECL_COMDAT (decl) = 0;
    }
  DECL_COMDAT_GROUP (decl) = 0;
  DECL_WEAK (decl) = 0;
  DECL_EXTERNAL (decl) = 0;
  DECL_VISIBILITY_SPECIFIED (decl) = 0;
  DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
  TREE_PUBLIC (decl) = 0;
  DECL_VISIBILITY_SPECIFIED (decl) = 0;
  DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
  if (!DECL_RTL_SET_P (decl))
    return;

  /* Update rtl flags.  */
  make_decl_rtl (decl);

  rtl = DECL_RTL (decl);
  if (!MEM_P (rtl))
    return;

  symbol = XEXP (rtl, 0);
  if (GET_CODE (symbol) != SYMBOL_REF)
    return;

  SYMBOL_REF_WEAK (symbol) = DECL_WEAK (decl);
}
#include "gt-symtab.h"
