/* Symbol table.
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
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
#include "rtl.h"
#include "tree.h"
#include "print-tree.h"
#include "varasm.h"
#include "function.h"
#include "emit-rtl.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "hashtab.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "timevar.h"
#include "lto-streamer.h"
#include "output.h"

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
static GTY((param_is (symtab_node))) htab_t symtab_hash;
/* Hash table used to convert assembler names into nodes.  */
static GTY((param_is (symtab_node))) htab_t assembler_name_hash;

/* Linked list of symbol table nodes.  */
symtab_node *symtab_nodes;

/* The order index of the next symtab node to be created.  This is
   used so that we can sort the cgraph nodes in order by when we saw
   them, to support -fno-toplevel-reorder.  */
int symtab_order;

/* Returns a hash code for P.  */

static hashval_t
hash_node (const void *p)
{
  const symtab_node *n = (const symtab_node *) p;
  return (hashval_t) DECL_UID (n->decl);
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_node (const void *p1, const void *p2)
{
  const symtab_node *n1 = (const symtab_node *) p1;
  const symtab_node *n2 = (const symtab_node *) p2;
  return DECL_UID (n1->decl) == DECL_UID (n2->decl);
}

/* Hash asmnames ignoring the user specified marks.  */

static hashval_t
decl_assembler_name_hash (const_tree asmname)
{
  if (IDENTIFIER_POINTER (asmname)[0] == '*')
    {
      const char *decl_str = IDENTIFIER_POINTER (asmname) + 1;
      size_t ulp_len = strlen (user_label_prefix);

      if (ulp_len == 0)
	;
      else if (strncmp (decl_str, user_label_prefix, ulp_len) == 0)
	decl_str += ulp_len;

      return htab_hash_string (decl_str);
    }

  return htab_hash_string (IDENTIFIER_POINTER (asmname));
}


/* Returns a hash code for P.  */

static hashval_t
hash_node_by_assembler_name (const void *p)
{
  const symtab_node *n = (const symtab_node *) p;
  return (hashval_t) decl_assembler_name_hash (DECL_ASSEMBLER_NAME (n->decl));
}

/* Compare ASMNAME with the DECL_ASSEMBLER_NAME of DECL.  */

static bool
decl_assembler_name_equal (tree decl, const_tree asmname)
{
  tree decl_asmname = DECL_ASSEMBLER_NAME (decl);
  const char *decl_str;
  const char *asmname_str;
  bool test = false;

  if (decl_asmname == asmname)
    return true;

  decl_str = IDENTIFIER_POINTER (decl_asmname);
  asmname_str = IDENTIFIER_POINTER (asmname);


  /* If the target assembler name was set by the user, things are trickier.
     We have a leading '*' to begin with.  After that, it's arguable what
     is the correct thing to do with -fleading-underscore.  Arguably, we've
     historically been doing the wrong thing in assemble_alias by always
     printing the leading underscore.  Since we're not changing that, make
     sure user_label_prefix follows the '*' before matching.  */
  if (decl_str[0] == '*')
    {
      size_t ulp_len = strlen (user_label_prefix);

      decl_str ++;

      if (ulp_len == 0)
	test = true;
      else if (strncmp (decl_str, user_label_prefix, ulp_len) == 0)
	decl_str += ulp_len, test=true;
      else
	decl_str --;
    }
  if (asmname_str[0] == '*')
    {
      size_t ulp_len = strlen (user_label_prefix);

      asmname_str ++;

      if (ulp_len == 0)
	test = true;
      else if (strncmp (asmname_str, user_label_prefix, ulp_len) == 0)
	asmname_str += ulp_len, test=true;
      else
	asmname_str --;
    }

  if (!test)
    return false;
  return strcmp (decl_str, asmname_str) == 0;
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_assembler_name (const void *p1, const void *p2)
{
  const symtab_node *n1 = (const symtab_node *) p1;
  const_tree name = (const_tree)p2;
  return (decl_assembler_name_equal (n1->decl, name));
}

/* Insert NODE to assembler name hash.  */

static void
insert_to_assembler_name_hash (symtab_node *node, bool with_clones)
{
  if (is_a <varpool_node> (node) && DECL_HARD_REGISTER (node->decl))
    return;
  gcc_checking_assert (!node->previous_sharing_asm_name
		       && !node->next_sharing_asm_name);
  if (assembler_name_hash)
    {
      void **aslot;
      struct cgraph_node *cnode;
      tree decl = node->decl;

      tree name = DECL_ASSEMBLER_NAME (node->decl);

      aslot = htab_find_slot_with_hash (assembler_name_hash, name,
					decl_assembler_name_hash (name),
					INSERT);
      gcc_assert (*aslot != node);
      node->next_sharing_asm_name = (symtab_node *)*aslot;
      if (*aslot != NULL)
	((symtab_node *)*aslot)->previous_sharing_asm_name = node;
      *aslot = node;

      /* Update also possible inline clones sharing a decl.  */
      cnode = dyn_cast <cgraph_node> (node);
      if (cnode && cnode->clones && with_clones)
	for (cnode = cnode->clones; cnode; cnode = cnode->next_sibling_clone)
	  if (cnode->decl == decl)
	    insert_to_assembler_name_hash (cnode, true);
    }

}

/* Remove NODE from assembler name hash.  */

static void
unlink_from_assembler_name_hash (symtab_node *node, bool with_clones)
{
  if (assembler_name_hash)
    {
      struct cgraph_node *cnode;
      tree decl = node->decl;

      if (node->next_sharing_asm_name)
	node->next_sharing_asm_name->previous_sharing_asm_name
	  = node->previous_sharing_asm_name;
      if (node->previous_sharing_asm_name)
	{
	  node->previous_sharing_asm_name->next_sharing_asm_name
	    = node->next_sharing_asm_name;
	}
      else
	{
	  tree name = DECL_ASSEMBLER_NAME (node->decl);
          void **slot;
	  slot = htab_find_slot_with_hash (assembler_name_hash, name,
					   decl_assembler_name_hash (name),
					   NO_INSERT);
	  gcc_assert (*slot == node);
	  if (!node->next_sharing_asm_name)
	    htab_clear_slot (assembler_name_hash, slot);
	  else
	    *slot = node->next_sharing_asm_name;
	}
      node->next_sharing_asm_name = NULL;
      node->previous_sharing_asm_name = NULL;

      /* Update also possible inline clones sharing a decl.  */
      cnode = dyn_cast <cgraph_node> (node);
      if (cnode && cnode->clones && with_clones)
	for (cnode = cnode->clones; cnode; cnode = cnode->next_sibling_clone)
	  if (cnode->decl == decl)
	    unlink_from_assembler_name_hash (cnode, true);
    }
}

/* Arrange node to be first in its entry of assembler_name_hash.  */

void
symtab_prevail_in_asm_name_hash (symtab_node *node)
{
  unlink_from_assembler_name_hash (node, false);
  insert_to_assembler_name_hash (node, false);
}


/* Add node into symbol table.  This function is not used directly, but via
   cgraph/varpool node creation routines.  */

void
symtab_register_node (symtab_node *node)
{
  struct symtab_node key;
  symtab_node **slot;

  node->next = symtab_nodes;
  node->previous = NULL;
  if (symtab_nodes)
    symtab_nodes->previous = node;
  symtab_nodes = node;

  if (!symtab_hash)
    symtab_hash = htab_create_ggc (10, hash_node, eq_node, NULL);
  key.decl = node->decl;
  slot = (symtab_node **) htab_find_slot (symtab_hash, &key, INSERT);
  if (*slot == NULL)
    *slot = node;

  ipa_empty_ref_list (&node->ref_list);

  node->order = symtab_order++;

  /* Be sure to do this last; C++ FE might create new nodes via
     DECL_ASSEMBLER_NAME langhook!  */
  insert_to_assembler_name_hash (node, false);
}

/* Make NODE to be the one symtab hash is pointing to.  Used when reshaping tree
   of inline clones.  */

void
symtab_insert_node_to_hashtable (symtab_node *node)
{
  struct symtab_node key;
  symtab_node **slot;

  if (!symtab_hash)
    symtab_hash = htab_create_ggc (10, hash_node, eq_node, NULL);
  key.decl = node->decl;
  slot = (symtab_node **) htab_find_slot (symtab_hash, &key, INSERT);
  *slot = node;
}

/* Remove node from symbol table.  This function is not used directly, but via
   cgraph/varpool node removal routines.  */

void
symtab_unregister_node (symtab_node *node)
{
  void **slot;
  ipa_remove_all_references (&node->ref_list);
  ipa_remove_all_referring (&node->ref_list);

  if (node->same_comdat_group)
    {
      symtab_node *prev;
      for (prev = node->same_comdat_group;
	   prev->same_comdat_group != node;
	   prev = prev->same_comdat_group)
	;
      if (node->same_comdat_group == prev)
	prev->same_comdat_group = NULL;
      else
	prev->same_comdat_group = node->same_comdat_group;
      node->same_comdat_group = NULL;
    }

  if (node->previous)
    node->previous->next = node->next;
  else
    symtab_nodes = node->next;
  if (node->next)
    node->next->previous = node->previous;
  node->next = NULL;
  node->previous = NULL;

  slot = htab_find_slot (symtab_hash, node, NO_INSERT);

  /* During LTO symtab merging we temporarily corrupt decl to symtab node
     hash.  */
  gcc_assert ((slot && *slot) || in_lto_p);
  if (slot && *slot && *slot == node)
    {
      symtab_node *replacement_node = NULL;
      if (cgraph_node *cnode = dyn_cast <cgraph_node> (node))
	replacement_node = cgraph_find_replacement_node (cnode);
      if (!replacement_node)
	htab_clear_slot (symtab_hash, slot);
      else
	*slot = replacement_node;
    }
  if (!is_a <varpool_node> (node) || !DECL_HARD_REGISTER (node->decl))
    unlink_from_assembler_name_hash (node, false);
}

/* Return symbol table node associated with DECL, if any,
   and NULL otherwise.  */

symtab_node *
symtab_get_node (const_tree decl)
{
  symtab_node **slot;
  struct symtab_node key;

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

  slot = (symtab_node **) htab_find_slot (symtab_hash, &key,
					 NO_INSERT);

  if (slot)
    return *slot;
  return NULL;
}

/* Remove symtab NODE from the symbol table.  */

void
symtab_remove_node (symtab_node *node)
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
  symtab_node *node;
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

symtab_node *
symtab_node_for_asm (const_tree asmname)
{
  symtab_node *node;
  void **slot;

  symtab_initialize_asm_name_hash ();
  slot = htab_find_slot_with_hash (assembler_name_hash, asmname,
				   decl_assembler_name_hash (asmname),
				   NO_INSERT);

  if (slot)
    {
      node = (symtab_node *) *slot;
      return node;
    }
  return NULL;
}

/* Set the DECL_ASSEMBLER_NAME and update symtab hashtables.  */

void
change_decl_assembler_name (tree decl, tree name)
{
  symtab_node *node = NULL;

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
	  TREE_CHAIN (name) = alias;
	}
      if (node)
	insert_to_assembler_name_hash (node, true);
    }
}

/* Add NEW_ to the same comdat group that OLD is in.  */

void
symtab_add_to_same_comdat_group (symtab_node *new_node,
				 symtab_node *old_node)
{
  gcc_assert (DECL_ONE_ONLY (old_node->decl));
  gcc_assert (!new_node->same_comdat_group);
  gcc_assert (new_node != old_node);

  DECL_COMDAT_GROUP (new_node->decl) = DECL_COMDAT_GROUP (old_node->decl);
  new_node->same_comdat_group = old_node;
  if (!old_node->same_comdat_group)
    old_node->same_comdat_group = new_node;
  else
    {
      symtab_node *n;
      for (n = old_node->same_comdat_group;
	   n->same_comdat_group != old_node;
	   n = n->same_comdat_group)
	;
      n->same_comdat_group = new_node;
    }
}

/* Dissolve the same_comdat_group list in which NODE resides.  */

void
symtab_dissolve_same_comdat_group_list (symtab_node *node)
{
  symtab_node *n = node;
  symtab_node *next;

  if (!node->same_comdat_group)
    return;
  do
    {
      next = n->same_comdat_group;
      n->same_comdat_group = NULL;
      /* Clear DECL_COMDAT_GROUP for comdat locals, since
         make_decl_local doesn't.  */
      if (!TREE_PUBLIC (n->decl))
	DECL_COMDAT_GROUP (n->decl) = NULL_TREE;
      n = next;
    }
  while (n != node);
}

/* Return printable assembler name of NODE.
   This function is used only for debugging.  When assembler name
   is unknown go with identifier name.  */

const char *
symtab_node::asm_name () const
{
  if (!DECL_ASSEMBLER_NAME_SET_P (decl))
    return lang_hooks.decl_printable_name (decl, 2);
  return IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
}

/* Return printable identifier name.  */

const char *
symtab_node::name () const
{
  return lang_hooks.decl_printable_name (decl, 2);
}

static const char * const symtab_type_names[] = {"symbol", "function", "variable"};

/* Dump base fields of symtab nodes.  Not to be used directly.  */

void
dump_symtab_base (FILE *f, symtab_node *node)
{
  static const char * const visibility_types[] = {
    "default", "protected", "hidden", "internal"
  };

  fprintf (f, "%s/%i (%s)",
	   node->asm_name (),
	   node->order,
	   node->name ());
  dump_addr (f, " @", (void *)node);
  fprintf (f, "\n  Type: %s", symtab_type_names[node->type]);

  if (node->definition)
    fprintf (f, " definition");
  if (node->analyzed)
    fprintf (f, " analyzed");
  if (node->alias)
    fprintf (f, " alias");
  if (node->weakref)
    fprintf (f, " weakref");
  if (node->cpp_implicit_alias)
    fprintf (f, " cpp_implicit_alias");
  if (node->alias_target)
    fprintf (f, " target:%s",
	     DECL_P (node->alias_target) 
	     ? IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME
				     (node->alias_target))
	     : IDENTIFIER_POINTER (node->alias_target));
  if (node->body_removed)
    fprintf (f, "\n  Body removed by symtab_remove_unreachable_nodes");
  fprintf (f, "\n  Visibility:");
  if (node->in_other_partition)
    fprintf (f, " in_other_partition");
  if (node->used_from_other_partition)
    fprintf (f, " used_from_other_partition");
  if (node->force_output)
    fprintf (f, " force_output");
  if (node->forced_by_abi)
    fprintf (f, " forced_by_abi");
  if (node->externally_visible)
    fprintf (f, " externally_visible");
  if (node->resolution != LDPR_UNKNOWN)
    fprintf (f, " %s",
 	     ld_plugin_symbol_resolution_names[(int)node->resolution]);
  if (TREE_ASM_WRITTEN (node->decl))
    fprintf (f, " asm_written");
  if (DECL_EXTERNAL (node->decl))
    fprintf (f, " external");
  if (TREE_PUBLIC (node->decl))
    fprintf (f, " public");
  if (DECL_COMMON (node->decl))
    fprintf (f, " common");
  if (DECL_WEAK (node->decl))
    fprintf (f, " weak");
  if (DECL_DLLIMPORT_P (node->decl))
    fprintf (f, " dll_import");
  if (DECL_COMDAT (node->decl))
    fprintf (f, " comdat");
  if (DECL_COMDAT_GROUP (node->decl))
    fprintf (f, " comdat_group:%s",
	     IDENTIFIER_POINTER (DECL_COMDAT_GROUP (node->decl)));
  if (DECL_ONE_ONLY (node->decl))
    fprintf (f, " one_only");
  if (DECL_SECTION_NAME (node->decl))
    fprintf (f, " section_name:%s",
	     TREE_STRING_POINTER (DECL_SECTION_NAME (node->decl)));
  if (DECL_VISIBILITY_SPECIFIED (node->decl))
    fprintf (f, " visibility_specified");
  if (DECL_VISIBILITY (node->decl))
    fprintf (f, " visibility:%s",
	     visibility_types [DECL_VISIBILITY (node->decl)]);
  if (DECL_VIRTUAL_P (node->decl))
    fprintf (f, " virtual");
  if (DECL_ARTIFICIAL (node->decl))
    fprintf (f, " artificial");
  if (TREE_CODE (node->decl) == FUNCTION_DECL)
    {
      if (DECL_STATIC_CONSTRUCTOR (node->decl))
	fprintf (f, " constructor");
      if (DECL_STATIC_DESTRUCTOR (node->decl))
	fprintf (f, " destructor");
    }
  fprintf (f, "\n");
  
  if (node->same_comdat_group)
    fprintf (f, "  Same comdat group as: %s/%i\n",
	     node->same_comdat_group->asm_name (),
	     node->same_comdat_group->order);
  if (node->next_sharing_asm_name)
    fprintf (f, "  next sharing asm name: %i\n",
	     node->next_sharing_asm_name->order);
  if (node->previous_sharing_asm_name)
    fprintf (f, "  previous sharing asm name: %i\n",
	     node->previous_sharing_asm_name->order);

  if (node->address_taken)
    fprintf (f, "  Address is taken.\n");
  if (node->aux)
    {
      fprintf (f, "  Aux:");
      dump_addr (f, " @", (void *)node->aux);
    }

  fprintf (f, "  References: ");
  ipa_dump_references (f, &node->ref_list);
  fprintf (f, "  Referring: ");
  ipa_dump_referring (f, &node->ref_list);
  if (node->lto_file_data)
    fprintf (f, "  Read from file: %s\n",
	     node->lto_file_data->file_name);
}

/* Dump symtab node.  */

void
dump_symtab_node (FILE *f, symtab_node *node)
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
  symtab_node *node;
  fprintf (f, "Symbol table:\n\n");
  FOR_EACH_SYMBOL (node)
    dump_symtab_node (f, node);
}

/* Dump symtab node NODE to stderr.  */

DEBUG_FUNCTION void
debug_symtab_node (symtab_node *node)
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
verify_symtab_base (symtab_node *node)
{
  bool error_found = false;
  symtab_node *hashed_node;

  if (is_a <cgraph_node> (node))
    {
      if (TREE_CODE (node->decl) != FUNCTION_DECL)
	{
          error ("function symbol is not function");
          error_found = true;
	}
    }
  else if (is_a <varpool_node> (node))
    {
      if (TREE_CODE (node->decl) != VAR_DECL)
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
      hashed_node = symtab_get_node (node->decl);
      if (!hashed_node)
	{
	  error ("node not found in symtab decl hashtable");
	  error_found = true;
	}
      if (hashed_node != node
	  && (!is_a <cgraph_node> (node)
	      || !dyn_cast <cgraph_node> (node)->clone_of
	      || dyn_cast <cgraph_node> (node)->clone_of->decl
		 != node->decl))
	{
	  error ("node differs from symtab decl hashtable");
	  error_found = true;
	}
    }
  if (assembler_name_hash)
    {
      hashed_node = symtab_node_for_asm (DECL_ASSEMBLER_NAME (node->decl));
      if (hashed_node && hashed_node->previous_sharing_asm_name)
	{
          error ("assembler name hash list corrupted");
          error_found = true;
	}
      while (hashed_node)
	{
	  if (hashed_node == node)
	    break;
	  hashed_node = hashed_node->next_sharing_asm_name;
	}
      if (!hashed_node
          && !(is_a <varpool_node> (node)
	       || DECL_HARD_REGISTER (node->decl)))
	{
          error ("node not found in symtab assembler name hash");
          error_found = true;
	}
    }
  if (node->previous_sharing_asm_name
      && node->previous_sharing_asm_name->next_sharing_asm_name != node)
    {
      error ("double linked list of assembler names corrupted");
      error_found = true;
    }
  if (node->analyzed && !node->definition)
    {
      error ("node is analyzed byt it is not a definition");
      error_found = true;
    }
  if (node->cpp_implicit_alias && !node->alias)
    {
      error ("node is alias but not implicit alias");
      error_found = true;
    }
  if (node->alias && !node->definition
      && !node->weakref)
    {
      error ("node is alias but not definition");
      error_found = true;
    }
  if (node->weakref && !node->alias)
    {
      error ("node is weakref but not an alias");
      error_found = true;
    }
  if (node->same_comdat_group)
    {
      symtab_node *n = node->same_comdat_group;

      if (!DECL_ONE_ONLY (n->decl))
	{
	  error ("non-DECL_ONE_ONLY node in a same_comdat_group list");
	  error_found = true;
	}
      if (n->type != node->type)
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
	  if (!n->same_comdat_group)
	    {
	      error ("same_comdat_group is not a circular list");
	      error_found = true;
	      break;
	    }
	  n = n->same_comdat_group;
	}
      while (n != node);
      if (symtab_comdat_local_p (node))
	{
	  struct ipa_ref_list *refs = &node->ref_list;
	  struct ipa_ref *ref;
	  for (int i = 0; ipa_ref_list_referring_iterate (refs, i, ref); ++i)
	    {
	      if (!symtab_in_same_comdat_p (ref->referring, node))
		{
		  error ("comdat-local symbol referred to by %s outside its "
			 "comdat",
			 identifier_to_locale (ref->referring->name()));
		  error_found = true;
		}
	    }
	}
    }
  return error_found;
}

/* Verify consistency of NODE.  */

DEBUG_FUNCTION void
verify_symtab_node (symtab_node *node)
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
  symtab_node *node;
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
symtab_used_from_object_file_p (symtab_node *node)
{
  if (!TREE_PUBLIC (node->decl) || DECL_EXTERNAL (node->decl))
    return false;
  if (resolution_used_from_other_file_p (node->resolution))
    return true;
  return false;
}

/* Make DECL local.  FIXME: We shouldn't need to mess with rtl this early,
   but other code such as notice_global_symbol generates rtl.  */

void
symtab_make_decl_local (tree decl)
{
  rtx rtl, symbol;

  /* Avoid clearing DECL_COMDAT_GROUP on comdat-local decls.  */
  if (TREE_PUBLIC (decl) == 0)
    return;

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
symtab_node_availability (symtab_node *node)
{
  if (is_a <cgraph_node> (node))
    return cgraph_function_body_availability (cgraph (node));
  else
    return cgraph_variable_initializer_availability (varpool (node));
}

/* Given NODE, walk the alias chain to return the symbol NODE is alias of.
   If NODE is not an alias, return NODE.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.  */

symtab_node *
symtab_alias_ultimate_target (symtab_node *node, enum availability *availability)
{
  bool weakref_p = false;

  if (!node->alias)
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
      weakref_p = node->weakref;
      if (!weakref_p)
        *availability = symtab_node_availability (node);
      else
	*availability = AVAIL_LOCAL;
    }
  while (node)
    {
      if (node->alias && node->analyzed)
	node = symtab_alias_target (node);
      else
	{
	  if (!availability)
	    ;
	  else if (node->analyzed)
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
          weakref_p = node->weakref;
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
fixup_same_cpp_alias_visibility (symtab_node *node, symtab_node *target)
{
  if (is_a <cgraph_node> (node))
    {
      DECL_DECLARED_INLINE_P (node->decl)
	 = DECL_DECLARED_INLINE_P (target->decl);
      DECL_DISREGARD_INLINE_LIMITS (node->decl)
	 = DECL_DISREGARD_INLINE_LIMITS (target->decl);
    }
  /* FIXME: It is not really clear why those flags should not be copied for
     functions, too.  */
  else
    {
      DECL_WEAK (node->decl) = DECL_WEAK (target->decl);
      DECL_EXTERNAL (node->decl) = DECL_EXTERNAL (target->decl);
      DECL_VISIBILITY (node->decl) = DECL_VISIBILITY (target->decl);
    }
  DECL_VIRTUAL_P (node->decl) = DECL_VIRTUAL_P (target->decl);
  if (TREE_PUBLIC (node->decl))
    {
      DECL_EXTERNAL (node->decl) = DECL_EXTERNAL (target->decl);
      DECL_COMDAT (node->decl) = DECL_COMDAT (target->decl);
      DECL_COMDAT_GROUP (node->decl)
	 = DECL_COMDAT_GROUP (target->decl);
      if (DECL_ONE_ONLY (target->decl)
	  && !node->same_comdat_group)
	symtab_add_to_same_comdat_group (node, target);
    }
  node->externally_visible = target->externally_visible;
}

/* Add reference recording that NODE is alias of TARGET.
   The function can fail in the case of aliasing cycles; in this case
   it returns false.  */

bool
symtab_resolve_alias (symtab_node *node, symtab_node *target)
{
  symtab_node *n;

  gcc_assert (!node->analyzed
	      && !vec_safe_length (node->ref_list.references));

  /* Never let cycles to creep into the symbol table alias references;
     those will make alias walkers to be infinite.  */
  for (n = target; n && n->alias;
       n = n->analyzed ? symtab_alias_target (n) : NULL)
    if (n == node)
       {
	 if (is_a <cgraph_node> (node))
           error ("function %q+D part of alias cycle", node->decl);
         else if (is_a <varpool_node> (node))
           error ("variable %q+D part of alias cycle", node->decl);
	 else
	   gcc_unreachable ();
	 node->alias = false;
	 return false;
       }

  /* "analyze" the node - i.e. mark the reference.  */
  node->definition = true;
  node->alias = true;
  node->analyzed = true;
  ipa_record_reference (node, target, IPA_REF_ALIAS, NULL);

  /* Alias targets become reudndant after alias is resolved into an reference.
     We do not want to keep it around or we would have to mind updating them
     when renaming symbols.  */
  node->alias_target = NULL;

  if (node->cpp_implicit_alias && cgraph_state >= CGRAPH_STATE_CONSTRUCTION)
    fixup_same_cpp_alias_visibility (node, target);

  /* If alias has address taken, so does the target.  */
  if (node->address_taken)
    symtab_alias_ultimate_target (target, NULL)->address_taken = true;
  return true;
}

/* Call calback on NODE and aliases associated to NODE. 
   When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
   skipped. */

bool
symtab_for_node_and_aliases (symtab_node *node,
			     bool (*callback) (symtab_node *, void *),
			     void *data,
			     bool include_overwritable)
{
  int i;
  struct ipa_ref *ref;

  if (callback (node, data))
    return true;
  for (i = 0; ipa_ref_list_referring_iterate (&node->ref_list, i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      {
	symtab_node *alias = ref->referring;
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
symtab_nonoverwritable_alias_1 (symtab_node *node, void *data)
{
  if (decl_binds_to_current_def_p (node->decl))
    {
      *(symtab_node **)data = node;
      return true;
    }
  return false;
}

/* If NODE can not be overwriten by static or dynamic linker to point to different
   definition, return NODE. Otherwise look for alias with such property and if
   none exists, introduce new one.  */

symtab_node *
symtab_nonoverwritable_alias (symtab_node *node)
{
  tree new_decl;
  symtab_node *new_node = NULL;

  /* First try to look up existing alias or base object
     (if that is already non-overwritable).  */
  node = symtab_alias_ultimate_target (node, NULL);
  gcc_assert (!node->alias && !node->weakref);
  symtab_for_node_and_aliases (node, symtab_nonoverwritable_alias_1,
		               (void *)&new_node, true);
  if (new_node)
    return new_node;
#ifndef ASM_OUTPUT_DEF
  /* If aliases aren't supported by the assembler, fail.  */
  return NULL;
#endif

  /* Otherwise create a new one.  */
  new_decl = copy_node (node->decl);
  DECL_NAME (new_decl) = clone_function_name (node->decl, "localalias");
  if (TREE_CODE (new_decl) == FUNCTION_DECL)
    DECL_STRUCT_FUNCTION (new_decl) = NULL;
  DECL_INITIAL (new_decl) = NULL;
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));
  SET_DECL_RTL (new_decl, NULL);

  /* Update the properties.  */
  DECL_EXTERNAL (new_decl) = 0;
  if (DECL_ONE_ONLY (node->decl))
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
      new_node = cgraph_create_function_alias
				 (new_decl, node->decl);
    }
  else
    new_node = varpool_create_variable_alias (new_decl,
							    node->decl);
  symtab_resolve_alias (new_node, node);  
  gcc_assert (decl_binds_to_current_def_p (new_decl));
  return new_node;
}

/* Return true if A and B represents semantically equivalent symbols.  */

bool
symtab_semantically_equivalent_p (symtab_node *a,
				  symtab_node *b)
{
  enum availability avail;
  symtab_node *ba;
  symtab_node *bb;

  /* Equivalent functions are equivalent.  */
  if (a->decl == b->decl)
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

/* Classify symbol NODE for partitioning.  */

enum symbol_partitioning_class
symtab_get_symbol_partitioning_class (symtab_node *node)
{
  /* Inline clones are always duplicated.
     This include external delcarations.   */
  cgraph_node *cnode = dyn_cast <cgraph_node> (node);

  if (DECL_ABSTRACT (node->decl))
    return SYMBOL_EXTERNAL;

  if (cnode && cnode->global.inlined_to)
    return SYMBOL_DUPLICATE;

  /* Weakref aliases are always duplicated.  */
  if (node->weakref)
    return SYMBOL_DUPLICATE;

  /* External declarations are external.  */
  if (DECL_EXTERNAL (node->decl))
    return SYMBOL_EXTERNAL;

  if (varpool_node *vnode = dyn_cast <varpool_node> (node))
    {
      /* Constant pool references use local symbol names that can not
         be promoted global.  We should never put into a constant pool
         objects that can not be duplicated across partitions.  */
      if (DECL_IN_CONSTANT_POOL (node->decl))
	return SYMBOL_DUPLICATE;
      gcc_checking_assert (vnode->definition);
    }
  /* Functions that are cloned may stay in callgraph even if they are unused.
     Handle them as external; compute_ltrans_boundary take care to make
     proper things to happen (i.e. to make them appear in the boundary but
     with body streamed, so clone can me materialized).  */
  else if (!cgraph (node)->definition)
    return SYMBOL_EXTERNAL;

  /* Linker discardable symbols are duplicated to every use unless they are
     keyed.
     Keyed symbols or those.  */
  if (DECL_ONE_ONLY (node->decl)
      && !node->force_output
      && !node->forced_by_abi
      && !symtab_used_from_object_file_p (node))
    return SYMBOL_DUPLICATE;

  return SYMBOL_PARTITION;
}
#include "gt-symtab.h"
