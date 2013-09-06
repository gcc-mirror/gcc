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
insert_to_assembler_name_hash (symtab_node node, bool with_clones)
{
  if (is_a <varpool_node> (node) && DECL_HARD_REGISTER (node->symbol.decl))
    return;
  gcc_checking_assert (!node->symbol.previous_sharing_asm_name
		       && !node->symbol.next_sharing_asm_name);
  if (assembler_name_hash)
    {
      void **aslot;
      struct cgraph_node *cnode;
      tree decl = node->symbol.decl;

      tree name = DECL_ASSEMBLER_NAME (node->symbol.decl);

      aslot = htab_find_slot_with_hash (assembler_name_hash, name,
					decl_assembler_name_hash (name),
					INSERT);
      gcc_assert (*aslot != node);
      node->symbol.next_sharing_asm_name = (symtab_node)*aslot;
      if (*aslot != NULL)
	((symtab_node)*aslot)->symbol.previous_sharing_asm_name = node;
      *aslot = node;

      /* Update also possible inline clones sharing a decl.  */
      cnode = dyn_cast <cgraph_node> (node);
      if (cnode && cnode->clones && with_clones)
	for (cnode = cnode->clones; cnode; cnode = cnode->next_sibling_clone)
	  if (cnode->symbol.decl == decl)
	    insert_to_assembler_name_hash ((symtab_node) cnode, true);
    }

}

/* Remove NODE from assembler name hash.  */

static void
unlink_from_assembler_name_hash (symtab_node node, bool with_clones)
{
  if (assembler_name_hash)
    {
      struct cgraph_node *cnode;
      tree decl = node->symbol.decl;

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

      /* Update also possible inline clones sharing a decl.  */
      cnode = dyn_cast <cgraph_node> (node);
      if (cnode && cnode->clones && with_clones)
	for (cnode = cnode->clones; cnode; cnode = cnode->next_sibling_clone)
	  if (cnode->symbol.decl == decl)
	    unlink_from_assembler_name_hash ((symtab_node) cnode, true);
    }
}

/* Arrange node to be first in its entry of assembler_name_hash.  */

void
symtab_prevail_in_asm_name_hash (symtab_node node)
{
  unlink_from_assembler_name_hash (node, false);
  insert_to_assembler_name_hash (node, false);
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
  insert_to_assembler_name_hash (node, false);
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

  /* During LTO symtab merging we temporarily corrupt decl to symtab node
     hash.  */
  gcc_assert ((slot && *slot) || in_lto_p);
  if (slot && *slot && *slot == node)
    {
      symtab_node replacement_node = NULL;
      if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
	replacement_node = (symtab_node)cgraph_find_replacement_node (cnode);
      if (!replacement_node)
	htab_clear_slot (symtab_hash, slot);
      else
	*slot = replacement_node;
    }
  if (!is_a <varpool_node> (node) || !DECL_HARD_REGISTER (node->symbol.decl))
    unlink_from_assembler_name_hash (node, false);
}

/* Return symbol table node associated with DECL, if any,
   and NULL otherwise.  */

symtab_node
symtab_get_node (const_tree decl)
{
  symtab_node *slot;
  struct symtab_node_base key;

#ifdef ENABLE_CHECKING
  /* Check that we are called for sane type of object - functions
     and static or external variables.  */
  gcc_checking_assert (TREE_CODE (decl) == FUNCTION_DECL
		       || (TREE_CODE (decl) == VAR_DECL
			   && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
			       || in_lto_p)));
#endif

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
	insert_to_assembler_name_hash (node, false);
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
	insert_to_assembler_name_hash (node, true);
    }
  else
    {
      if (name == DECL_ASSEMBLER_NAME (decl))
	return;

      tree alias = (IDENTIFIER_TRANSPARENT_ALIAS (DECL_ASSEMBLER_NAME (decl))
		    ? TREE_CHAIN (DECL_ASSEMBLER_NAME (decl))
		    : NULL);
      if (node)
	unlink_from_assembler_name_hash (node, true);
      if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))
	  && DECL_RTL_SET_P (decl))
	warning (0, "%D renamed after being referenced in assembly", decl);

      SET_DECL_ASSEMBLER_NAME (decl, name);
      if (alias)
	{
	  IDENTIFIER_TRANSPARENT_ALIAS (name) = 1;
	  TREE_CHAIN (DECL_ASSEMBLER_NAME (name)) = alias;
	}
      if (node)
	insert_to_assembler_name_hash (node, true);
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
  fprintf (f, "\n  Type: %s", symtab_type_names[node->symbol.type]);

  if (node->symbol.definition)
    fprintf (f, " definition");
  if (node->symbol.analyzed)
    fprintf (f, " analyzed");
  if (node->symbol.alias)
    fprintf (f, " alias");
  if (node->symbol.weakref)
    fprintf (f, " weakref");
  if (node->symbol.cpp_implicit_alias)
    fprintf (f, " cpp_implicit_alias");
  if (node->symbol.alias_target)
    fprintf (f, " target:%s",
	     DECL_P (node->symbol.alias_target) 
	     ? IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME
				     (node->symbol.alias_target))
	     : IDENTIFIER_POINTER (node->symbol.alias_target));
  fprintf (f, "\n  Visibility:");
  if (node->symbol.in_other_partition)
    fprintf (f, " in_other_partition");
  if (node->symbol.used_from_other_partition)
    fprintf (f, " used_from_other_partition");
  if (node->symbol.force_output)
    fprintf (f, " force_output");
  if (node->symbol.forced_by_abi)
    fprintf (f, " forced_by_abi");
  if (node->symbol.externally_visible)
    fprintf (f, " externally_visible");
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
   
  if (cgraph_state != CGRAPH_LTO_STREAMING)
    {
      hashed_node = symtab_get_node (node->symbol.decl);
      if (!hashed_node)
	{
	  error ("node not found in symtab decl hashtable");
	  error_found = true;
	}
      if (hashed_node != node
	  && (!is_a <cgraph_node> (node)
	      || !dyn_cast <cgraph_node> (node)->clone_of
	      || dyn_cast <cgraph_node> (node)->clone_of->symbol.decl
		 != node->symbol.decl))
	{
	  error ("node differs from symtab decl hashtable");
	  error_found = true;
	}
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
      error_found = true;
    }
  if (node->symbol.analyzed && !node->symbol.definition)
    {
      error ("node is analyzed byt it is not a definition");
      error_found = true;
    }
  if (node->symbol.cpp_implicit_alias && !node->symbol.alias)
    {
      error ("node is alias but not implicit alias");
      error_found = true;
    }
  if (node->symbol.alias && !node->symbol.definition
      && !node->symbol.weakref)
    {
      error ("node is alias but not definition");
      error_found = true;
    }
  if (node->symbol.weakref && !node->symbol.alias)
    {
      error ("node is weakref but not an alias");
      error_found = true;
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
      DECL_SECTION_NAME (decl) = 0;
      DECL_COMDAT (decl) = 0;
    }
  DECL_COMDAT_GROUP (decl) = 0;
  DECL_WEAK (decl) = 0;
  DECL_EXTERNAL (decl) = 0;
  DECL_VISIBILITY_SPECIFIED (decl) = 0;
  DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
  TREE_PUBLIC (decl) = 0;
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

/* Return availability of NODE.  */

enum availability
symtab_node_availability (symtab_node node)
{
  if (is_a <cgraph_node> (node))
    return cgraph_function_body_availability (cgraph (node));
  else
    return cgraph_variable_initializer_availability (varpool (node));
}

/* Given NODE, walk the alias chain to return the symbol NODE is alias of.
   If NODE is not an alias, return NODE.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.  */

symtab_node
symtab_alias_ultimate_target (symtab_node node, enum availability *availability)
{
  bool weakref_p = false;

  if (!node->symbol.alias)
    {
      if (availability)
        *availability = symtab_node_availability (node);
      return node;
    }

  /* To determine visibility of the target, we follow ELF semantic of aliases.
     Here alias is an alternative assembler name of a given definition. Its
     availability prevails the availability of its target (i.e. static alias of
     weak definition is available.

     Weakref is a different animal (and not part of ELF per se). It is just
     alternative name of a given symbol used within one complation unit
     and is translated prior hitting the object file.  It inherits the
     visibility of its target (i.e. weakref of non-overwritable definition
     is non-overwritable, while weakref of weak definition is weak).

     If we ever get into supporting targets with different semantics, a target
     hook will be needed here.  */

  if (availability)
    {
      weakref_p = node->symbol.weakref;
      if (!weakref_p)
        *availability = symtab_node_availability (node);
      else
	*availability = AVAIL_LOCAL;
    }
  while (node)
    {
      if (node->symbol.alias && node->symbol.analyzed)
	node = symtab_alias_target (node);
      else
	{
	  if (!availability)
	    ;
	  else if (node->symbol.analyzed)
	    {
	      if (weakref_p)
		{
		  enum availability a = symtab_node_availability (node);
		  if (a < *availability)
		    *availability = a;
		}
	    }
	  else
	    *availability = AVAIL_NOT_AVAILABLE;
	  return node;
	}
      if (node && availability && weakref_p)
	{
	  enum availability a = symtab_node_availability (node);
	  if (a < *availability)
	    *availability = a;
          weakref_p = node->symbol.weakref;
	}
    }
  if (availability)
    *availability = AVAIL_NOT_AVAILABLE;
  return NULL;
}

/* C++ FE sometimes change linkage flags after producing same body aliases.

   FIXME: C++ produce implicit aliases for virtual functions and vtables that
   are obviously equivalent.  The way it is doing so is however somewhat
   kludgy and interferes with the visibility code. As a result we need to
   copy the visibility from the target to get things right.  */

void
fixup_same_cpp_alias_visibility (symtab_node node, symtab_node target)
{
  if (is_a <cgraph_node> (node))
    {
      DECL_DECLARED_INLINE_P (node->symbol.decl)
	 = DECL_DECLARED_INLINE_P (target->symbol.decl);
      DECL_DISREGARD_INLINE_LIMITS (node->symbol.decl)
	 = DECL_DISREGARD_INLINE_LIMITS (target->symbol.decl);
    }
  /* FIXME: It is not really clear why those flags should not be copied for
     functions, too.  */
  else
    {
      DECL_WEAK (node->symbol.decl) = DECL_WEAK (target->symbol.decl);
      DECL_EXTERNAL (node->symbol.decl) = DECL_EXTERNAL (target->symbol.decl);
      DECL_VISIBILITY (node->symbol.decl) = DECL_VISIBILITY (target->symbol.decl);
    }
  DECL_VIRTUAL_P (node->symbol.decl) = DECL_VIRTUAL_P (target->symbol.decl);
  if (TREE_PUBLIC (node->symbol.decl))
    {
      DECL_EXTERNAL (node->symbol.decl) = DECL_EXTERNAL (target->symbol.decl);
      DECL_COMDAT (node->symbol.decl) = DECL_COMDAT (target->symbol.decl);
      DECL_COMDAT_GROUP (node->symbol.decl)
	 = DECL_COMDAT_GROUP (target->symbol.decl);
      if (DECL_ONE_ONLY (target->symbol.decl)
	  && !node->symbol.same_comdat_group)
	symtab_add_to_same_comdat_group ((symtab_node)node, (symtab_node)target);
    }
  node->symbol.externally_visible = target->symbol.externally_visible;
}

/* Add reference recording that NODE is alias of TARGET.
   The function can fail in the case of aliasing cycles; in this case
   it returns false.  */

bool
symtab_resolve_alias (symtab_node node, symtab_node target)
{
  symtab_node n;

  gcc_assert (!node->symbol.analyzed
	      && !vec_safe_length (node->symbol.ref_list.references));

  /* Never let cycles to creep into the symbol table alias references;
     those will make alias walkers to be infinite.  */
  for (n = target; n && n->symbol.alias;
       n = n->symbol.analyzed ? symtab_alias_target (n) : NULL)
    if (n == node)
       {
	 if (is_a <cgraph_node> (node))
           error ("function %q+D part of alias cycle", node->symbol.decl);
         else if (is_a <varpool_node> (node))
           error ("variable %q+D part of alias cycle", node->symbol.decl);
	 else
	   gcc_unreachable ();
	 node->symbol.alias = false;
	 return false;
       }

  /* "analyze" the node - i.e. mark the reference.  */
  node->symbol.definition = true;
  node->symbol.alias = true;
  node->symbol.analyzed = true;
  ipa_record_reference (node, target, IPA_REF_ALIAS, NULL);

  /* Alias targets become reudndant after alias is resolved into an reference.
     We do not want to keep it around or we would have to mind updating them
     when renaming symbols.  */
  node->symbol.alias_target = NULL;

  if (node->symbol.cpp_implicit_alias && cgraph_state >= CGRAPH_STATE_CONSTRUCTION)
    fixup_same_cpp_alias_visibility (node, target);

  /* If alias has address taken, so does the target.  */
  if (node->symbol.address_taken)
    symtab_alias_ultimate_target (target, NULL)->symbol.address_taken = true;
  return true;
}

/* Call calback on NODE and aliases associated to NODE. 
   When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
   skipped. */

bool
symtab_for_node_and_aliases (symtab_node node,
			     bool (*callback) (symtab_node, void *),
			     void *data,
			     bool include_overwritable)
{
  int i;
  struct ipa_ref *ref;

  if (callback (node, data))
    return true;
  for (i = 0; ipa_ref_list_referring_iterate (&node->symbol.ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      {
	symtab_node alias = ref->referring;
	if (include_overwritable
	    || symtab_node_availability (alias) > AVAIL_OVERWRITABLE)
          if (symtab_for_node_and_aliases (alias, callback, data,
					   include_overwritable))
	    return true;
      }
  return false;
}

/* Worker searching nonoverwritable alias.  */

static bool
symtab_nonoverwritable_alias_1 (symtab_node node, void *data)
{
  if (decl_binds_to_current_def_p (node->symbol.decl))
    {
      *(symtab_node *)data = node;
      return true;
    }
  return false;
}

/* If NODE can not be overwriten by static or dynamic linker to point to different
   definition, return NODE. Otherwise look for alias with such property and if
   none exists, introduce new one.  */

symtab_node
symtab_nonoverwritable_alias (symtab_node node)
{
  tree new_decl;
  symtab_node new_node = NULL;

  /* First try to look up existing alias or base object
     (if that is already non-overwritable).  */
  node = symtab_alias_ultimate_target (node, NULL);
  gcc_assert (!node->symbol.alias && !node->symbol.weakref);
  symtab_for_node_and_aliases (node, symtab_nonoverwritable_alias_1,
		               (void *)&new_node, true);
  if (new_node)
    return new_node;

  /* Otherwise create a new one.  */
  new_decl = copy_node (node->symbol.decl);
  DECL_NAME (new_decl) = clone_function_name (node->symbol.decl, "localalias");
  if (TREE_CODE (new_decl) == FUNCTION_DECL)
    DECL_STRUCT_FUNCTION (new_decl) = NULL;
  DECL_INITIAL (new_decl) = NULL;
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));
  SET_DECL_RTL (new_decl, NULL);

  /* Update the properties.  */
  DECL_EXTERNAL (new_decl) = 0;
  if (DECL_ONE_ONLY (node->symbol.decl))
    DECL_SECTION_NAME (new_decl) = NULL;
  DECL_COMDAT_GROUP (new_decl) = 0;
  TREE_PUBLIC (new_decl) = 0;
  DECL_COMDAT (new_decl) = 0;
  DECL_WEAK (new_decl) = 0;
  DECL_VIRTUAL_P (new_decl) = 0;
  if (TREE_CODE (new_decl) == FUNCTION_DECL)
    {
      DECL_STATIC_CONSTRUCTOR (new_decl) = 0;
      DECL_STATIC_DESTRUCTOR (new_decl) = 0;
      new_node = (symtab_node) cgraph_create_function_alias
				 (new_decl, node->symbol.decl);
    }
  else
    new_node = (symtab_node) varpool_create_variable_alias (new_decl,
							    node->symbol.decl);
  symtab_resolve_alias (new_node, node);  
  gcc_assert (decl_binds_to_current_def_p (new_decl));
  return new_node;
}

/* Return true if A and B represents semantically equivalent symbols.  */

bool
symtab_semantically_equivalent_p (symtab_node a,
				  symtab_node b)
{
  enum availability avail;
  symtab_node ba, bb;

  /* Equivalent functions are equivalent.  */
  if (a->symbol.decl == b->symbol.decl)
    return true;

  /* If symbol is not overwritable by different implementation,
     walk to the base object it defines.  */
  ba = symtab_alias_ultimate_target (a, &avail);
  if (avail >= AVAIL_AVAILABLE)
    {
      if (ba == b)
	return true;
    }
  else
    ba = a;
  bb = symtab_alias_ultimate_target (b, &avail);
  if (avail >= AVAIL_AVAILABLE)
    {
      if (a == bb)
	return true;
    }
  else
    bb = b;
  return bb == ba;
}
#include "gt-symtab.h"
