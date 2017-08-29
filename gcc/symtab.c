/* Symbol table.
   Copyright (C) 2012-2017 Free Software Foundation, Inc.
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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "timevar.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "print-tree.h"
#include "varasm.h"
#include "langhooks.h"
#include "output.h"
#include "ipa-utils.h"
#include "calls.h"
#include "stringpool.h"
#include "attribs.h"

static const char *ipa_ref_use_name[] = {"read","write","addr","alias","chkp"};

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

/* Follow the IDENTIFIER_TRANSPARENT_ALIAS chain starting at ALIAS
   until we find an identifier that is not itself a transparent alias.  */

static inline tree
ultimate_transparent_alias_target (tree alias)
{
  tree target = alias;

  while (IDENTIFIER_TRANSPARENT_ALIAS (target))
    {
      gcc_checking_assert (TREE_CHAIN (target));
      target = TREE_CHAIN (target);
    }
  gcc_checking_assert (! IDENTIFIER_TRANSPARENT_ALIAS (target)
		       && ! TREE_CHAIN (target));

  return target;
}


/* Hash asmnames ignoring the user specified marks.  */

hashval_t
symbol_table::decl_assembler_name_hash (const_tree asmname)
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

/* Return true if assembler names NAME1 and NAME2 leads to the same symbol
   name.  */

bool
symbol_table::assembler_names_equal_p (const char *name1, const char *name2)
{
  if (name1 != name2)
    {
      if (name1[0] == '*')
	{
	  size_t ulp_len = strlen (user_label_prefix);

	  name1 ++;

	  if (ulp_len == 0)
	    ;
	  else if (strncmp (name1, user_label_prefix, ulp_len) == 0)
	    name1 += ulp_len;
	  else
	    return false;
	}
      if (name2[0] == '*')
	{
	  size_t ulp_len = strlen (user_label_prefix);

	  name2 ++;

	  if (ulp_len == 0)
	    ;
	  else if (strncmp (name2, user_label_prefix, ulp_len) == 0)
	    name2 += ulp_len;
	  else
	    return false;
	}
      return !strcmp (name1, name2);
    }
  return true;
}

/* Compare ASMNAME with the DECL_ASSEMBLER_NAME of DECL.  */

bool
symbol_table::decl_assembler_name_equal (tree decl, const_tree asmname)
{
  tree decl_asmname = DECL_ASSEMBLER_NAME (decl);
  const char *decl_str;
  const char *asmname_str;

  if (decl_asmname == asmname)
    return true;

  decl_str = IDENTIFIER_POINTER (decl_asmname);
  asmname_str = IDENTIFIER_POINTER (asmname);
  return assembler_names_equal_p (decl_str, asmname_str);
}


/* Returns nonzero if P1 and P2 are equal.  */

/* Insert NODE to assembler name hash.  */

void
symbol_table::insert_to_assembler_name_hash (symtab_node *node,
					     bool with_clones)
{
  if (is_a <varpool_node *> (node) && DECL_HARD_REGISTER (node->decl))
    return;
  gcc_checking_assert (!node->previous_sharing_asm_name
		       && !node->next_sharing_asm_name);
  if (assembler_name_hash)
    {
      symtab_node **aslot;
      cgraph_node *cnode;
      tree decl = node->decl;

      tree name = DECL_ASSEMBLER_NAME (node->decl);

      /* C++ FE can produce decls without associated assembler name and insert
	 them to symtab to hold section or TLS information.  */
      if (!name)
	return;

      hashval_t hash = decl_assembler_name_hash (name);
      aslot = assembler_name_hash->find_slot_with_hash (name, hash, INSERT);
      gcc_assert (*aslot != node);
      node->next_sharing_asm_name = (symtab_node *)*aslot;
      if (*aslot != NULL)
	(*aslot)->previous_sharing_asm_name = node;
      *aslot = node;

      /* Update also possible inline clones sharing a decl.  */
      cnode = dyn_cast <cgraph_node *> (node);
      if (cnode && cnode->clones && with_clones)
	for (cnode = cnode->clones; cnode; cnode = cnode->next_sibling_clone)
	  if (cnode->decl == decl)
	    insert_to_assembler_name_hash (cnode, true);
    }

}

/* Remove NODE from assembler name hash.  */

void
symbol_table::unlink_from_assembler_name_hash (symtab_node *node,
					       bool with_clones)
{
  if (assembler_name_hash)
    {
      cgraph_node *cnode;
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
          symtab_node **slot;

	  if (!name)
	    return;

	  hashval_t hash = decl_assembler_name_hash (name);
	  slot = assembler_name_hash->find_slot_with_hash (name, hash,
							   NO_INSERT);
	  gcc_assert (*slot == node);
	  if (!node->next_sharing_asm_name)
	    assembler_name_hash->clear_slot (slot);
	  else
	    *slot = node->next_sharing_asm_name;
	}
      node->next_sharing_asm_name = NULL;
      node->previous_sharing_asm_name = NULL;

      /* Update also possible inline clones sharing a decl.  */
      cnode = dyn_cast <cgraph_node *> (node);
      if (cnode && cnode->clones && with_clones)
	for (cnode = cnode->clones; cnode; cnode = cnode->next_sibling_clone)
	  if (cnode->decl == decl)
	    unlink_from_assembler_name_hash (cnode, true);
    }
}

/* Arrange node to be first in its entry of assembler_name_hash.  */

void
symbol_table::symtab_prevail_in_asm_name_hash (symtab_node *node)
{
  unlink_from_assembler_name_hash (node, false);
  insert_to_assembler_name_hash (node, false);
}

/* Initalize asm name hash unless.  */

void
symbol_table::symtab_initialize_asm_name_hash (void)
{
  symtab_node *node;
  if (!assembler_name_hash)
    {
      assembler_name_hash = hash_table<asmname_hasher>::create_ggc (10);
      FOR_EACH_SYMBOL (node)
	insert_to_assembler_name_hash (node, false);
    }
}

/* Set the DECL_ASSEMBLER_NAME and update symtab hashtables.  */

void
symbol_table::change_decl_assembler_name (tree decl, tree name)
{
  symtab_node *node = NULL;

  /* We can have user ASM names on things, like global register variables, that
     are not in the symbol table.  */
  if ((VAR_P (decl) && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
      || TREE_CODE (decl) == FUNCTION_DECL)
    node = symtab_node::get (decl);
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

      const char *old_name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))
	  && DECL_RTL_SET_P (decl))
	warning (0, "%qD renamed after being referenced in assembly", decl);

      SET_DECL_ASSEMBLER_NAME (decl, name);
      if (alias)
	{
	  IDENTIFIER_TRANSPARENT_ALIAS (name) = 1;
	  TREE_CHAIN (name) = alias;
	}
      /* If we change assembler name, also all transparent aliases must
	 be updated.  There are three kinds - those having same assembler name,
	 those being renamed in varasm.c and weakref being renamed by the
	 assembler.  */
      if (node)
	{
	  insert_to_assembler_name_hash (node, true);
	  ipa_ref *ref;
	  for (unsigned i = 0; node->iterate_direct_aliases (i, ref); i++)
	    {
	      struct symtab_node *alias = ref->referring;
	      if (alias->transparent_alias && !alias->weakref
		  && symbol_table::assembler_names_equal_p
			 (old_name, IDENTIFIER_POINTER (
				      DECL_ASSEMBLER_NAME (alias->decl))))
		change_decl_assembler_name (alias->decl, name);
	      else if (alias->transparent_alias
		       && IDENTIFIER_TRANSPARENT_ALIAS (alias->decl))
		{
		  gcc_assert (TREE_CHAIN (DECL_ASSEMBLER_NAME (alias->decl))
			      && IDENTIFIER_TRANSPARENT_ALIAS
				     (DECL_ASSEMBLER_NAME (alias->decl)));

		  TREE_CHAIN (DECL_ASSEMBLER_NAME (alias->decl)) = 
		    ultimate_transparent_alias_target
			 (DECL_ASSEMBLER_NAME (node->decl));
		}
#ifdef ASM_OUTPUT_WEAKREF
	     else gcc_assert (!alias->transparent_alias || alias->weakref);
#else
	     else gcc_assert (!alias->transparent_alias);
#endif
	    }
	  gcc_assert (!node->transparent_alias || !node->definition
		      || node->weakref
		      || TREE_CHAIN (DECL_ASSEMBLER_NAME (decl))
		      || symbol_table::assembler_names_equal_p
			  (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)),
			   IDENTIFIER_POINTER
			     (DECL_ASSEMBLER_NAME
				 (node->get_alias_target ()->decl))));
	}
    }
}

/* Hash sections by their names.  */

hashval_t
section_name_hasher::hash (section_hash_entry *n)
{
  return htab_hash_string (n->name);
}

/* Return true if section P1 name equals to P2.  */

bool
section_name_hasher::equal (section_hash_entry *n1, const char *name)
{
  return n1->name == name || !strcmp (n1->name, name);
}

/* Add node into symbol table.  This function is not used directly, but via
   cgraph/varpool node creation routines.  */

void
symtab_node::register_symbol (void)
{
  symtab->register_symbol (this);

  if (!decl->decl_with_vis.symtab_node)
    decl->decl_with_vis.symtab_node = this;

  ref_list.clear ();

  /* Be sure to do this last; C++ FE might create new nodes via
     DECL_ASSEMBLER_NAME langhook!  */
  symtab->insert_to_assembler_name_hash (this, false);
}

/* Remove NODE from same comdat group.   */

void
symtab_node::remove_from_same_comdat_group (void)
{
  if (same_comdat_group)
    {
      symtab_node *prev;
      for (prev = same_comdat_group;
	   prev->same_comdat_group != this;
	   prev = prev->same_comdat_group)
	;
      if (same_comdat_group == prev)
	prev->same_comdat_group = NULL;
      else
	prev->same_comdat_group = same_comdat_group;
      same_comdat_group = NULL;
      set_comdat_group (NULL);
    }
}

/* Remove node from symbol table.  This function is not used directly, but via
   cgraph/varpool node removal routines.  */

void
symtab_node::unregister (void)
{
  remove_all_references ();
  remove_all_referring ();

  /* Remove reference to section.  */
  set_section_for_node (NULL);

  remove_from_same_comdat_group ();

  symtab->unregister (this);

  /* During LTO symtab merging we temporarily corrupt decl to symtab node
     hash.  */
  gcc_assert (decl->decl_with_vis.symtab_node || in_lto_p);
  if (decl->decl_with_vis.symtab_node == this)
    {
      symtab_node *replacement_node = NULL;
      if (cgraph_node *cnode = dyn_cast <cgraph_node *> (this))
	replacement_node = cnode->find_replacement ();
      decl->decl_with_vis.symtab_node = replacement_node;
    }
  if (!is_a <varpool_node *> (this) || !DECL_HARD_REGISTER (decl))
    symtab->unlink_from_assembler_name_hash (this, false);
  if (in_init_priority_hash)
    symtab->init_priority_hash->remove (this);
}


/* Remove symbol from symbol table.  */

void
symtab_node::remove (void)
{
  if (cgraph_node *cnode = dyn_cast <cgraph_node *> (this))
    cnode->remove ();
  else if (varpool_node *vnode = dyn_cast <varpool_node *> (this))
    vnode->remove ();
}

/* Add NEW_ to the same comdat group that OLD is in.  */

void
symtab_node::add_to_same_comdat_group (symtab_node *old_node)
{
  gcc_assert (old_node->get_comdat_group ());
  gcc_assert (!same_comdat_group);
  gcc_assert (this != old_node);

  set_comdat_group (old_node->get_comdat_group ());
  same_comdat_group = old_node;
  if (!old_node->same_comdat_group)
    old_node->same_comdat_group = this;
  else
    {
      symtab_node *n;
      for (n = old_node->same_comdat_group;
	   n->same_comdat_group != old_node;
	   n = n->same_comdat_group)
	;
      n->same_comdat_group = this;
    }
}

/* Dissolve the same_comdat_group list in which NODE resides.  */

void
symtab_node::dissolve_same_comdat_group_list (void)
{
  symtab_node *n = this;
  symtab_node *next;

  if (!same_comdat_group)
    return;
  do
    {
      next = n->same_comdat_group;
      n->same_comdat_group = NULL;
      /* Clear comdat_group for comdat locals, since
         make_decl_local doesn't.  */
      if (!TREE_PUBLIC (n->decl))
	n->set_comdat_group (NULL);
      n = next;
    }
  while (n != this);
}

/* Return printable assembler name of NODE.
   This function is used only for debugging.  When assembler name
   is unknown go with identifier name.  */

const char *
symtab_node::asm_name () const
{
  if (!DECL_ASSEMBLER_NAME_SET_P (decl))
    return name ();
  return IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
}

/* Return printable identifier name.  */

const char *
symtab_node::name () const
{
  if (!DECL_NAME (decl))
    {
      if (DECL_ASSEMBLER_NAME_SET_P (decl))
	return asm_name ();
      else
        return "<unnamed>";
    }
  return lang_hooks.decl_printable_name (decl, 2);
}

const char *
symtab_node::get_dump_name (bool asm_name_p) const
{
#define EXTRA 16
  const char *fname = asm_name_p ? asm_name () : name ();
  unsigned l = strlen (fname);

  char *s = (char *)ggc_internal_cleared_alloc (l + EXTRA);
  snprintf (s, l + EXTRA, "%s/%d", fname, order);

  return s;
}

const char *
symtab_node::dump_name () const
{
  return get_dump_name (false);
}

const char *
symtab_node::dump_asm_name () const
{
  return get_dump_name (true);
}

/* Return ipa reference from this symtab_node to
   REFERED_NODE or REFERED_VARPOOL_NODE. USE_TYPE specify type
   of the use.  */

ipa_ref *
symtab_node::create_reference (symtab_node *referred_node,
			       enum ipa_ref_use use_type)
{
  return create_reference (referred_node, use_type, NULL);
}


/* Return ipa reference from this symtab_node to
   REFERED_NODE or REFERED_VARPOOL_NODE. USE_TYPE specify type
   of the use and STMT the statement (if it exists).  */

ipa_ref *
symtab_node::create_reference (symtab_node *referred_node,
			       enum ipa_ref_use use_type, gimple *stmt)
{
  ipa_ref *ref = NULL, *ref2 = NULL;
  ipa_ref_list *list, *list2;
  ipa_ref_t *old_references;

  gcc_checking_assert (!stmt || is_a <cgraph_node *> (this));
  gcc_checking_assert (use_type != IPA_REF_ALIAS || !stmt);

  list = &ref_list;
  old_references = vec_safe_address (list->references);
  vec_safe_grow (list->references, vec_safe_length (list->references) + 1);
  ref = &list->references->last ();

  list2 = &referred_node->ref_list;

  /* IPA_REF_ALIAS is always inserted at the beginning of the list.   */
  if(use_type == IPA_REF_ALIAS)
    {
      list2->referring.safe_insert (0, ref);
      ref->referred_index = 0;

      for (unsigned int i = 1; i < list2->referring.length (); i++)
	list2->referring[i]->referred_index = i;
    }
  else
    {
      list2->referring.safe_push (ref);
      ref->referred_index = list2->referring.length () - 1;
    }

  ref->referring = this;
  ref->referred = referred_node;
  ref->stmt = stmt;
  ref->lto_stmt_uid = 0;
  ref->use = use_type;
  ref->speculative = 0;

  /* If vector was moved in memory, update pointers.  */
  if (old_references != list->references->address ())
    {
      int i;
      for (i = 0; iterate_reference(i, ref2); i++)
	ref2->referred_ref_list ()->referring[ref2->referred_index] = ref2;
    }
  return ref;
}

ipa_ref *
symtab_node::maybe_create_reference (tree val, gimple *stmt)
{
  STRIP_NOPS (val);
  ipa_ref_use use_type;

  switch (TREE_CODE (val))
    {
    case VAR_DECL:
      use_type = IPA_REF_LOAD;
      break;
    case ADDR_EXPR:
      use_type = IPA_REF_ADDR;
      break;
    default:
      gcc_assert (!handled_component_p (val));
      return NULL;
    }

  val = get_base_var (val);
  if (val && VAR_OR_FUNCTION_DECL_P (val))
    {
      symtab_node *referred = symtab_node::get (val);
      gcc_checking_assert (referred);
      return create_reference (referred, use_type, stmt);
    }
  return NULL;
}

/* Clone all references from symtab NODE to this symtab_node.  */

void
symtab_node::clone_references (symtab_node *node)
{
  ipa_ref *ref = NULL, *ref2 = NULL;
  int i;
  for (i = 0; node->iterate_reference (i, ref); i++)
    {
      bool speculative = ref->speculative;
      unsigned int stmt_uid = ref->lto_stmt_uid;

      ref2 = create_reference (ref->referred, ref->use, ref->stmt);
      ref2->speculative = speculative;
      ref2->lto_stmt_uid = stmt_uid;
    }
}

/* Clone all referring from symtab NODE to this symtab_node.  */

void
symtab_node::clone_referring (symtab_node *node)
{
  ipa_ref *ref = NULL, *ref2 = NULL;
  int i;
  for (i = 0; node->iterate_referring(i, ref); i++)
    {
      bool speculative = ref->speculative;
      unsigned int stmt_uid = ref->lto_stmt_uid;

      ref2 = ref->referring->create_reference (this, ref->use, ref->stmt);
      ref2->speculative = speculative;
      ref2->lto_stmt_uid = stmt_uid;
    }
}

/* Clone reference REF to this symtab_node and set its stmt to STMT.  */

ipa_ref *
symtab_node::clone_reference (ipa_ref *ref, gimple *stmt)
{
  bool speculative = ref->speculative;
  unsigned int stmt_uid = ref->lto_stmt_uid;
  ipa_ref *ref2;

  ref2 = create_reference (ref->referred, ref->use, stmt);
  ref2->speculative = speculative;
  ref2->lto_stmt_uid = stmt_uid;
  return ref2;
}

/* Find the structure describing a reference to REFERRED_NODE
   and associated with statement STMT.  */

ipa_ref *
symtab_node::find_reference (symtab_node *referred_node,
			     gimple *stmt, unsigned int lto_stmt_uid)
{
  ipa_ref *r = NULL;
  int i;

  for (i = 0; iterate_reference (i, r); i++)
    if (r->referred == referred_node
	&& !r->speculative
	&& ((stmt && r->stmt == stmt)
	    || (lto_stmt_uid && r->lto_stmt_uid == lto_stmt_uid)
	    || (!stmt && !lto_stmt_uid && !r->stmt && !r->lto_stmt_uid)))
      return r;
  return NULL;
}

/* Remove all references that are associated with statement STMT.  */

void
symtab_node::remove_stmt_references (gimple *stmt)
{
  ipa_ref *r = NULL;
  int i = 0;

  while (iterate_reference (i, r))
    if (r->stmt == stmt)
      r->remove_reference ();
    else
      i++;
}

/* Remove all stmt references in non-speculative references.
   Those are not maintained during inlining & clonning.
   The exception are speculative references that are updated along
   with callgraph edges associated with them.  */

void
symtab_node::clear_stmts_in_references (void)
{
  ipa_ref *r = NULL;
  int i;

  for (i = 0; iterate_reference (i, r); i++)
    if (!r->speculative)
      {
	r->stmt = NULL;
	r->lto_stmt_uid = 0;
      }
}

/* Remove all references in ref list.  */

void
symtab_node::remove_all_references (void)
{
  while (vec_safe_length (ref_list.references))
    ref_list.references->last ().remove_reference ();
  vec_free (ref_list.references);
}

/* Remove all referring items in ref list.  */

void
symtab_node::remove_all_referring (void)
{
  while (ref_list.referring.length ())
    ref_list.referring.last ()->remove_reference ();
  ref_list.referring.release ();
}

/* Dump references in ref list to FILE.  */

void
symtab_node::dump_references (FILE *file)
{
  ipa_ref *ref = NULL;
  int i;
  for (i = 0; iterate_reference (i, ref); i++)
    {
      fprintf (file, "%s (%s)",
	       ref->referred->dump_asm_name (),
	       ipa_ref_use_name [ref->use]);
      if (ref->speculative)
	fprintf (file, " (speculative)");
    }
  fprintf (file, "\n");
}

/* Dump referring in list to FILE.  */

void
symtab_node::dump_referring (FILE *file)
{
  ipa_ref *ref = NULL;
  int i;
  for (i = 0; iterate_referring(i, ref); i++)
    {
      fprintf (file, "%s (%s)",
	       ref->referring->dump_asm_name (),
	       ipa_ref_use_name [ref->use]);
      if (ref->speculative)
	fprintf (file, " (speculative)");
    }
  fprintf (file, "\n");
}

static const char * const symtab_type_names[] = {"symbol", "function", "variable"};

/* Dump base fields of symtab nodes to F.  Not to be used directly.  */

void
symtab_node::dump_base (FILE *f)
{
  static const char * const visibility_types[] = {
    "default", "protected", "hidden", "internal"
  };

  fprintf (f, "%s (%s)", dump_asm_name (), name ());
  dump_addr (f, " @", (void *)this);
  fprintf (f, "\n  Type: %s", symtab_type_names[type]);

  if (definition)
    fprintf (f, " definition");
  if (analyzed)
    fprintf (f, " analyzed");
  if (alias)
    fprintf (f, " alias");
  if (transparent_alias)
    fprintf (f, " transparent_alias");
  if (weakref)
    fprintf (f, " weakref");
  if (cpp_implicit_alias)
    fprintf (f, " cpp_implicit_alias");
  if (alias_target)
    fprintf (f, " target:%s",
	     DECL_P (alias_target)
	     ? IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME
				     (alias_target))
	     : IDENTIFIER_POINTER (alias_target));
  if (body_removed)
    fprintf (f, "\n  Body removed by symtab_remove_unreachable_nodes");
  fprintf (f, "\n  Visibility:");
  if (in_other_partition)
    fprintf (f, " in_other_partition");
  if (used_from_other_partition)
    fprintf (f, " used_from_other_partition");
  if (force_output)
    fprintf (f, " force_output");
  if (forced_by_abi)
    fprintf (f, " forced_by_abi");
  if (externally_visible)
    fprintf (f, " externally_visible");
  if (no_reorder)
    fprintf (f, " no_reorder");
  if (resolution != LDPR_UNKNOWN)
    fprintf (f, " %s",
 	     ld_plugin_symbol_resolution_names[(int)resolution]);
  if (TREE_ASM_WRITTEN (decl))
    fprintf (f, " asm_written");
  if (DECL_EXTERNAL (decl))
    fprintf (f, " external");
  if (TREE_PUBLIC (decl))
    fprintf (f, " public");
  if (DECL_COMMON (decl))
    fprintf (f, " common");
  if (DECL_WEAK (decl))
    fprintf (f, " weak");
  if (DECL_DLLIMPORT_P (decl))
    fprintf (f, " dll_import");
  if (DECL_COMDAT (decl))
    fprintf (f, " comdat");
  if (get_comdat_group ())
    fprintf (f, " comdat_group:%s",
	     IDENTIFIER_POINTER (get_comdat_group_id ()));
  if (DECL_ONE_ONLY (decl))
    fprintf (f, " one_only");
  if (get_section ())
    fprintf (f, " section:%s",
	     get_section ());
  if (implicit_section)
    fprintf (f," (implicit_section)");
  if (DECL_VISIBILITY_SPECIFIED (decl))
    fprintf (f, " visibility_specified");
  if (DECL_VISIBILITY (decl))
    fprintf (f, " visibility:%s",
	     visibility_types [DECL_VISIBILITY (decl)]);
  if (DECL_VIRTUAL_P (decl))
    fprintf (f, " virtual");
  if (DECL_ARTIFICIAL (decl))
    fprintf (f, " artificial");
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (DECL_STATIC_CONSTRUCTOR (decl))
	fprintf (f, " constructor");
      if (DECL_STATIC_DESTRUCTOR (decl))
	fprintf (f, " destructor");
    }
  fprintf (f, "\n");
  
  if (same_comdat_group)
    fprintf (f, "  Same comdat group as: %s\n",
	     same_comdat_group->dump_asm_name ());
  if (next_sharing_asm_name)
    fprintf (f, "  next sharing asm name: %i\n",
	     next_sharing_asm_name->order);
  if (previous_sharing_asm_name)
    fprintf (f, "  previous sharing asm name: %i\n",
	     previous_sharing_asm_name->order);

  if (address_taken)
    fprintf (f, "  Address is taken.\n");
  if (aux)
    {
      fprintf (f, "  Aux:");
      dump_addr (f, " @", (void *)aux);
      fprintf (f, "\n");
    }

  fprintf (f, "  References: ");
  dump_references (f);
  fprintf (f, "  Referring: ");
  dump_referring (f);
  if (lto_file_data)
    fprintf (f, "  Read from file: %s\n",
	     lto_file_data->file_name);
}

/* Dump symtab node to F.  */

void
symtab_node::dump (FILE *f)
{
  if (cgraph_node *cnode = dyn_cast <cgraph_node *> (this))
    cnode->dump (f);
  else if (varpool_node *vnode = dyn_cast <varpool_node *> (this))
    vnode->dump (f);
}

void
symbol_table::dump (FILE *f)
{
  symtab_node *node;
  fprintf (f, "Symbol table:\n\n");
  FOR_EACH_SYMBOL (node)
    node->dump (f);
}

/* Return the cgraph node that has ASMNAME for its DECL_ASSEMBLER_NAME.
   Return NULL if there's no such node.  */

symtab_node *
symtab_node::get_for_asmname (const_tree asmname)
{
  symtab_node *node;

  symtab->symtab_initialize_asm_name_hash ();
  hashval_t hash = symtab->decl_assembler_name_hash (asmname);
  symtab_node **slot
    = symtab->assembler_name_hash->find_slot_with_hash (asmname, hash,
							NO_INSERT);

  if (slot)
    {
      node = *slot;
      return node;
    }
  return NULL;
}

/* Dump symtab node NODE to stderr.  */

DEBUG_FUNCTION void
symtab_node::debug (void)
{
  dump (stderr);
}

/* Verify common part of symtab nodes.  */

DEBUG_FUNCTION bool
symtab_node::verify_base (void)
{
  bool error_found = false;
  symtab_node *hashed_node;

  if (is_a <cgraph_node *> (this))
    {
      if (TREE_CODE (decl) != FUNCTION_DECL)
	{
          error ("function symbol is not function");
          error_found = true;
	}
    }
  else if (is_a <varpool_node *> (this))
    {
      if (!VAR_P (decl))
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
   
  if (symtab->state != LTO_STREAMING)
    {
      hashed_node = symtab_node::get (decl);
      if (!hashed_node)
	{
	  error ("node not found node->decl->decl_with_vis.symtab_node");
	  error_found = true;
	}
      if (hashed_node != this
	  && (!is_a <cgraph_node *> (this)
	      || !dyn_cast <cgraph_node *> (this)->clone_of
	      || dyn_cast <cgraph_node *> (this)->clone_of->decl != decl))
	{
	  error ("node differs from node->decl->decl_with_vis.symtab_node");
	  error_found = true;
	}
    }
  if (symtab->assembler_name_hash)
    {
      hashed_node = symtab_node::get_for_asmname (DECL_ASSEMBLER_NAME (decl));
      if (hashed_node && hashed_node->previous_sharing_asm_name)
	{
          error ("assembler name hash list corrupted");
          error_found = true;
	}
      while (hashed_node)
	{
	  if (hashed_node == this)
	    break;
	  hashed_node = hashed_node->next_sharing_asm_name;
	}
      if (!hashed_node
	  && !(is_a <varpool_node *> (this)
	       && DECL_HARD_REGISTER (decl)))
	{
          error ("node not found in symtab assembler name hash");
          error_found = true;
	}
    }
  if (previous_sharing_asm_name
      && previous_sharing_asm_name->next_sharing_asm_name != this)
    {
      error ("double linked list of assembler names corrupted");
      error_found = true;
    }
  if (body_removed && definition)
    {
      error ("node has body_removed but is definition");
      error_found = true;
    }
  if (analyzed && !definition)
    {
      error ("node is analyzed but it is not a definition");
      error_found = true;
    }
  if (cpp_implicit_alias && !alias)
    {
      error ("node is alias but not implicit alias");
      error_found = true;
    }
  if (alias && !definition && !weakref)
    {
      error ("node is alias but not definition");
      error_found = true;
    }
  if (weakref && !transparent_alias)
    {
      error ("node is weakref but not an transparent_alias");
      error_found = true;
    }
  if (transparent_alias && !alias)
    {
      error ("node is transparent_alias but not an alias");
      error_found = true;
    }
  if (same_comdat_group)
    {
      symtab_node *n = same_comdat_group;

      if (!n->get_comdat_group ())
	{
	  error ("node is in same_comdat_group list but has no comdat_group");
	  error_found = true;
	}
      if (n->get_comdat_group () != get_comdat_group ())
	{
	  error ("same_comdat_group list across different groups");
	  error_found = true;
	}
      if (n->type != type)
	{
	  error ("mixing different types of symbol in same comdat groups is not supported");
	  error_found = true;
	}
      if (n == this)
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
      while (n != this);
      if (comdat_local_p ())
	{
	  ipa_ref *ref = NULL;

	  for (int i = 0; iterate_referring (i, ref); ++i)
	    {
	      if (!in_same_comdat_group_p (ref->referring))
		{
		  error ("comdat-local symbol referred to by %s outside its "
			 "comdat",
			 identifier_to_locale (ref->referring->name()));
		  error_found = true;
		}
	    }
	}
    }
  if (implicit_section && !get_section ())
    {
      error ("implicit_section flag is set but section isn't");
      error_found = true;
    }
  if (get_section () && get_comdat_group ()
      && !implicit_section
      && !lookup_attribute ("section", DECL_ATTRIBUTES (decl)))
    {
      error ("Both section and comdat group is set");
      error_found = true;
    }
  /* TODO: Add string table for sections, so we do not keep holding duplicated
     strings.  */
  if (alias && definition
      && get_section () != get_alias_target ()->get_section ()
      && (!get_section()
	  || !get_alias_target ()->get_section ()
	  || strcmp (get_section(),
		     get_alias_target ()->get_section ())))
    {
      error ("Alias and target's section differs");
      get_alias_target ()->dump (stderr);
      error_found = true;
    }
  if (alias && definition
      && get_comdat_group () != get_alias_target ()->get_comdat_group ())
    {
      error ("Alias and target's comdat groups differs");
      get_alias_target ()->dump (stderr);
      error_found = true;
    }
  if (transparent_alias && definition && !weakref)
    {
      symtab_node *to = get_alias_target ();
      const char *name1
	= IDENTIFIER_POINTER (
	    ultimate_transparent_alias_target (DECL_ASSEMBLER_NAME (decl)));
      const char *name2
	= IDENTIFIER_POINTER (
	    ultimate_transparent_alias_target (DECL_ASSEMBLER_NAME (to->decl)));
      if (!symbol_table::assembler_names_equal_p (name1, name2))
	{
	  error ("Transparent alias and target's assembler names differs");
	  get_alias_target ()->dump (stderr);
	  error_found = true;
	}
    }
  if (transparent_alias && definition
      && get_alias_target()->transparent_alias && get_alias_target()->analyzed)
    {
      error ("Chained transparent aliases");
      get_alias_target ()->dump (stderr);
      error_found = true;
    }

  return error_found;
}

/* Verify consistency of NODE.  */

DEBUG_FUNCTION void
symtab_node::verify (void)
{
  if (seen_error ())
    return;

  timevar_push (TV_CGRAPH_VERIFY);
  if (cgraph_node *node = dyn_cast <cgraph_node *> (this))
    node->verify_node ();
  else
    if (verify_base ())
      {
	debug ();
	internal_error ("symtab_node::verify failed");
      }
  timevar_pop (TV_CGRAPH_VERIFY);
}

/* Verify symbol table for internal consistency.  */

DEBUG_FUNCTION void
symtab_node::verify_symtab_nodes (void)
{
  symtab_node *node;
  hash_map<tree, symtab_node *> comdat_head_map (251);

  FOR_EACH_SYMBOL (node)
    {
      node->verify ();
      if (node->get_comdat_group ())
	{
	  symtab_node **entry, *s;
	  bool existed;

	  entry = &comdat_head_map.get_or_insert (node->get_comdat_group (),
						  &existed);
	  if (!existed)
	    *entry = node;
	  else if (!DECL_EXTERNAL (node->decl))
	    {
	      for (s = (*entry)->same_comdat_group;
		   s != NULL && s != node && s != *entry;
		   s = s->same_comdat_group)
		;
	      if (!s || s == *entry)
		{
		  error ("Two symbols with same comdat_group are not linked by "
			 "the same_comdat_group list.");
		  (*entry)->debug ();
		  node->debug ();
		  internal_error ("symtab_node::verify failed");
		}
	    }
	}
    }
}

/* Make DECL local.  FIXME: We shouldn't need to mess with rtl this early,
   but other code such as notice_global_symbol generates rtl.  */

void
symtab_node::make_decl_local (void)
{
  rtx rtl, symbol;

  if (weakref)
    {
      weakref = false;
      IDENTIFIER_TRANSPARENT_ALIAS (DECL_ASSEMBLER_NAME (decl)) = 0;
      TREE_CHAIN (DECL_ASSEMBLER_NAME (decl)) = NULL_TREE;
      symtab->change_decl_assembler_name
	 (decl, DECL_ASSEMBLER_NAME (get_alias_target ()->decl));
      DECL_ATTRIBUTES (decl) = remove_attribute ("weakref",
						 DECL_ATTRIBUTES (decl));
    }
  /* Avoid clearing comdat_groups on comdat-local decls.  */
  else if (TREE_PUBLIC (decl) == 0)
    return;

  /* Localizing a symbol also make all its transparent aliases local.  */
  ipa_ref *ref;
  for (unsigned i = 0; iterate_direct_aliases (i, ref); i++)
    {
      struct symtab_node *alias = ref->referring;
      if (alias->transparent_alias)
	alias->make_decl_local ();
    }

  if (VAR_P (decl))
    {
      DECL_COMMON (decl) = 0;
      /* ADDRESSABLE flag is not defined for public symbols.  */
      TREE_ADDRESSABLE (decl) = 1;
      TREE_STATIC (decl) = 1;
    }
  else
    gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);

  DECL_COMDAT (decl) = 0;
  DECL_WEAK (decl) = 0;
  DECL_EXTERNAL (decl) = 0;
  DECL_VISIBILITY_SPECIFIED (decl) = 0;
  DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
  TREE_PUBLIC (decl) = 0;
  DECL_DLLIMPORT_P (decl) = 0;
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

/* Copy visibility from N.
   This is useful when THIS becomes a transparent alias of N.  */

void
symtab_node::copy_visibility_from (symtab_node *n)
{
  gcc_checking_assert (n->weakref == weakref);

  ipa_ref *ref;
  for (unsigned i = 0; iterate_direct_aliases (i, ref); i++)
    {
      struct symtab_node *alias = ref->referring;
      if (alias->transparent_alias)
	alias->copy_visibility_from (n);
    }

  if (VAR_P (decl))
    {
      DECL_COMMON (decl) = DECL_COMMON (n->decl);
      /* ADDRESSABLE flag is not defined for public symbols.  */
      if (TREE_PUBLIC (decl) && !TREE_PUBLIC (n->decl))
        TREE_ADDRESSABLE (decl) = 1;
      TREE_STATIC (decl) = TREE_STATIC (n->decl);
    }
  else gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);

  DECL_COMDAT (decl) = DECL_COMDAT (n->decl);
  DECL_WEAK (decl) = DECL_WEAK (n->decl);
  DECL_EXTERNAL (decl) = DECL_EXTERNAL (n->decl);
  DECL_VISIBILITY_SPECIFIED (decl) = DECL_VISIBILITY_SPECIFIED (n->decl);
  DECL_VISIBILITY (decl) = DECL_VISIBILITY (n->decl);
  TREE_PUBLIC (decl) = TREE_PUBLIC (n->decl);
  DECL_DLLIMPORT_P (decl) = DECL_DLLIMPORT_P (n->decl);
  resolution = n->resolution;
  set_comdat_group (n->get_comdat_group ());
  call_for_symbol_and_aliases (symtab_node::set_section,
			     const_cast<char *>(n->get_section ()), true);
  externally_visible = n->externally_visible;
  if (!DECL_RTL_SET_P (decl))
    return;

  /* Update rtl flags.  */
  make_decl_rtl (decl);

  rtx rtl = DECL_RTL (decl);
  if (!MEM_P (rtl))
    return;

  rtx symbol = XEXP (rtl, 0);
  if (GET_CODE (symbol) != SYMBOL_REF)
    return;

  SYMBOL_REF_WEAK (symbol) = DECL_WEAK (decl);
}

/* Walk the alias chain to return the symbol NODE is alias of.
   If NODE is not an alias, return NODE.
   Assumes NODE is known to be alias.  */

symtab_node *
symtab_node::ultimate_alias_target_1 (enum availability *availability,
				      symtab_node *ref)
{
  bool transparent_p = false;

  /* To determine visibility of the target, we follow ELF semantic of aliases.
     Here alias is an alternative assembler name of a given definition. Its
     availability prevails the availability of its target (i.e. static alias of
     weak definition is available.

     Transaparent alias is just alternative anme of a given symbol used within
     one compilation unit and is translated prior hitting the object file.  It
     inherits the visibility of its target.
     Weakref is a different animal (and noweak definition is weak).

     If we ever get into supporting targets with different semantics, a target
     hook will be needed here.  */

  if (availability)
    {
      transparent_p = transparent_alias;
      if (!transparent_p)
	*availability = get_availability (ref);
      else
	*availability = AVAIL_NOT_AVAILABLE;
    }

  symtab_node *node = this;
  while (node)
    {
      if (node->alias && node->analyzed)
	node = node->get_alias_target ();
      else
	{
	  if (!availability || (!transparent_p && node->analyzed))
	    ;
	  else if (node->analyzed && !node->transparent_alias)
	    *availability = node->get_availability (ref);
	  else
	    *availability = AVAIL_NOT_AVAILABLE;
	  return node;
	}
      if (node && availability && transparent_p
	  && node->transparent_alias)
	{
	  *availability = node->get_availability (ref);
	  transparent_p = false;
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
symtab_node::fixup_same_cpp_alias_visibility (symtab_node *target)
{
  if (is_a <cgraph_node *> (this))
    {
      DECL_DECLARED_INLINE_P (decl)
	 = DECL_DECLARED_INLINE_P (target->decl);
      DECL_DISREGARD_INLINE_LIMITS (decl)
	 = DECL_DISREGARD_INLINE_LIMITS (target->decl);
    }
  /* FIXME: It is not really clear why those flags should not be copied for
     functions, too.  */
  else
    {
      DECL_WEAK (decl) = DECL_WEAK (target->decl);
      DECL_EXTERNAL (decl) = DECL_EXTERNAL (target->decl);
      DECL_VISIBILITY (decl) = DECL_VISIBILITY (target->decl);
    }
  if (TREE_PUBLIC (decl))
    {
      tree group;

      DECL_EXTERNAL (decl) = DECL_EXTERNAL (target->decl);
      DECL_COMDAT (decl) = DECL_COMDAT (target->decl);
      group = target->get_comdat_group ();
      set_comdat_group (group);
      if (group && !same_comdat_group)
	add_to_same_comdat_group (target);
    }
  externally_visible = target->externally_visible;
}

/* Set section, do not recurse into aliases.
   When one wants to change section of a symbol and its aliases,
   use set_section.  */

void
symtab_node::set_section_for_node (const char *section)
{
  const char *current = get_section ();
  section_hash_entry **slot;

  if (current == section
      || (current && section
	  && !strcmp (current, section)))
    return;

  if (current)
    {
      x_section->ref_count--;
      if (!x_section->ref_count)
	{
	  hashval_t hash = htab_hash_string (x_section->name);
	  slot = symtab->section_hash->find_slot_with_hash (x_section->name,
							    hash, INSERT);
	  ggc_free (x_section);
	  symtab->section_hash->clear_slot (slot);
	}
      x_section = NULL;
    }
  if (!section)
    {
      implicit_section = false;
      return;
    }
  if (!symtab->section_hash)
    symtab->section_hash = hash_table<section_name_hasher>::create_ggc (10);
  slot = symtab->section_hash->find_slot_with_hash (section,
						    htab_hash_string (section),
						    INSERT);
  if (*slot)
    x_section = (section_hash_entry *)*slot;
  else
    {
      int len = strlen (section);
      *slot = x_section = ggc_cleared_alloc<section_hash_entry> ();
      x_section->name = ggc_vec_alloc<char> (len + 1);
      memcpy (x_section->name, section, len + 1);
    }
  x_section->ref_count++;
}

/* Worker for set_section.  */

bool
symtab_node::set_section (symtab_node *n, void *s)
{
  n->set_section_for_node ((char *)s);
  return false;
}

/* Set section of symbol and its aliases.  */

void
symtab_node::set_section (const char *section)
{
  gcc_assert (!this->alias);
  call_for_symbol_and_aliases
    (symtab_node::set_section, const_cast<char *>(section), true);
}

/* Return the initialization priority.  */

priority_type
symtab_node::get_init_priority ()
{
  if (!this->in_init_priority_hash)
    return DEFAULT_INIT_PRIORITY;

  symbol_priority_map *h = symtab->init_priority_hash->get (this);
  return h ? h->init : DEFAULT_INIT_PRIORITY;
}

/* Return the finalization priority.  */

priority_type
cgraph_node::get_fini_priority ()
{
  if (!this->in_init_priority_hash)
    return DEFAULT_INIT_PRIORITY;
  symbol_priority_map *h = symtab->init_priority_hash->get (this);
  return h ? h->fini : DEFAULT_INIT_PRIORITY;
}

/* Return the initialization and finalization priority information for
   DECL.  If there is no previous priority information, a freshly
   allocated structure is returned.  */

symbol_priority_map *
symtab_node::priority_info (void)
{
  if (!symtab->init_priority_hash)
    symtab->init_priority_hash = hash_map<symtab_node *, symbol_priority_map>::create_ggc (13);

  bool existed;
  symbol_priority_map *h
    = &symtab->init_priority_hash->get_or_insert (this, &existed);
  if (!existed)
    {
      h->init = DEFAULT_INIT_PRIORITY;
      h->fini = DEFAULT_INIT_PRIORITY;
      in_init_priority_hash = true;
    }

  return h;
}

/* Set initialization priority to PRIORITY.  */

void
symtab_node::set_init_priority (priority_type priority)
{
  symbol_priority_map *h;

  if (is_a <cgraph_node *> (this))
    gcc_assert (DECL_STATIC_CONSTRUCTOR (this->decl));

  if (priority == DEFAULT_INIT_PRIORITY)
    {
      gcc_assert (get_init_priority() == priority);
      return;
    }
  h = priority_info ();
  h->init = priority;
}

/* Set fialization priority to PRIORITY.  */

void
cgraph_node::set_fini_priority (priority_type priority)
{
  symbol_priority_map *h;

  gcc_assert (DECL_STATIC_DESTRUCTOR (this->decl));

  if (priority == DEFAULT_INIT_PRIORITY)
    {
      gcc_assert (get_fini_priority() == priority);
      return;
    }
  h = priority_info ();
  h->fini = priority;
}

/* Worker for symtab_resolve_alias.  */

bool
symtab_node::set_implicit_section (symtab_node *n,
				   void *data ATTRIBUTE_UNUSED)
{
  n->implicit_section = true;
  return false;
}

/* Add reference recording that symtab node is alias of TARGET.
   The function can fail in the case of aliasing cycles; in this case
   it returns false.  */

bool
symtab_node::resolve_alias (symtab_node *target, bool transparent)
{
  symtab_node *n;

  gcc_assert (!analyzed && !vec_safe_length (ref_list.references));

  /* Never let cycles to creep into the symbol table alias references;
     those will make alias walkers to be infinite.  */
  for (n = target; n && n->alias;
       n = n->analyzed ? n->get_alias_target () : NULL)
    if (n == this)
       {
	 if (is_a <cgraph_node *> (this))
	   error ("function %q+D part of alias cycle", decl);
	 else if (is_a <varpool_node *> (this))
	   error ("variable %q+D part of alias cycle", decl);
	 else
	   gcc_unreachable ();
	 alias = false;
	 return false;
       }

  /* "analyze" the node - i.e. mark the reference.  */
  definition = true;
  alias = true;
  analyzed = true;
  transparent |= transparent_alias;
  transparent_alias = transparent;
  if (transparent)
    while (target->transparent_alias && target->analyzed)
      target = target->get_alias_target ();
  create_reference (target, IPA_REF_ALIAS, NULL);

  /* Add alias into the comdat group of its target unless it is already there.  */
  if (same_comdat_group)
    remove_from_same_comdat_group ();
  set_comdat_group (NULL);
  if (target->get_comdat_group ())
    add_to_same_comdat_group (target);

  if ((get_section () != target->get_section ()
       || target->get_comdat_group ()) && get_section () && !implicit_section)
    {
      error ("section of alias %q+D must match section of its target", decl);
    }
  call_for_symbol_and_aliases (symtab_node::set_section,
			     const_cast<char *>(target->get_section ()), true);
  if (target->implicit_section)
    call_for_symbol_and_aliases (set_implicit_section, NULL, true);

  /* Alias targets become redundant after alias is resolved into an reference.
     We do not want to keep it around or we would have to mind updating them
     when renaming symbols.  */
  alias_target = NULL;

  if (!transparent && cpp_implicit_alias && symtab->state >= CONSTRUCTION)
    fixup_same_cpp_alias_visibility (target);

  /* If alias has address taken, so does the target.  */
  if (address_taken)
    target->ultimate_alias_target ()->address_taken = true;

  /* All non-transparent aliases of THIS are now in fact aliases of TARGET.
     If alias is transparent, also all transparent aliases of THIS are now
     aliases of TARGET.
     Also merge same comdat group lists.  */
  ipa_ref *ref;
  for (unsigned i = 0; iterate_direct_aliases (i, ref);)
    {
      struct symtab_node *alias_alias = ref->referring;
      if (alias_alias->get_comdat_group ())
	{
	  alias_alias->remove_from_same_comdat_group ();
	  alias_alias->set_comdat_group (NULL);
	  if (target->get_comdat_group ())
	    alias_alias->add_to_same_comdat_group (target);
	}
      if (!alias_alias->transparent_alias || transparent)
	{
	  alias_alias->remove_all_references ();
	  alias_alias->create_reference (target, IPA_REF_ALIAS, NULL);
	}
      else i++;
    }
  return true;
}

/* Worker searching noninterposable alias.  */

bool
symtab_node::noninterposable_alias (symtab_node *node, void *data)
{
  if (!node->transparent_alias && decl_binds_to_current_def_p (node->decl))
    {
      symtab_node *fn = node->ultimate_alias_target ();

      /* Ensure that the alias is well formed this may not be the case
	 of user defined aliases and currently it is not always the case
	 of C++ same body aliases (that is a bug).  */
      if (TREE_TYPE (node->decl) != TREE_TYPE (fn->decl)
	  || DECL_CONTEXT (node->decl) != DECL_CONTEXT (fn->decl)
	  || (TREE_CODE (node->decl) == FUNCTION_DECL
	      && flags_from_decl_or_type (node->decl)
		 != flags_from_decl_or_type (fn->decl))
	  || DECL_ATTRIBUTES (node->decl) != DECL_ATTRIBUTES (fn->decl))
	return false;
      *(symtab_node **)data = node;
      return true;
    }
  return false;
}

/* If node can not be overwriten by static or dynamic linker to point to
   different definition, return NODE. Otherwise look for alias with such
   property and if none exists, introduce new one.  */

symtab_node *
symtab_node::noninterposable_alias (void)
{
  tree new_decl;
  symtab_node *new_node = NULL;

  /* First try to look up existing alias or base object
     (if that is already non-overwritable).  */
  symtab_node *node = ultimate_alias_target ();
  gcc_assert (!node->alias && !node->weakref);
  node->call_for_symbol_and_aliases (symtab_node::noninterposable_alias,
				   (void *)&new_node, true);
  if (new_node)
    return new_node;

  /* If aliases aren't supported by the assembler, fail.  */
  if (!TARGET_SUPPORTS_ALIASES)
    return NULL;

  /* Otherwise create a new one.  */
  new_decl = copy_node (node->decl);
  DECL_DLLIMPORT_P (new_decl) = 0;
  DECL_NAME (new_decl) = clone_function_name (node->decl, "localalias");
  if (TREE_CODE (new_decl) == FUNCTION_DECL)
    DECL_STRUCT_FUNCTION (new_decl) = NULL;
  DECL_INITIAL (new_decl) = NULL;
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));
  SET_DECL_RTL (new_decl, NULL);

  /* Update the properties.  */
  DECL_EXTERNAL (new_decl) = 0;
  TREE_PUBLIC (new_decl) = 0;
  DECL_COMDAT (new_decl) = 0;
  DECL_WEAK (new_decl) = 0;

  /* Since the aliases can be added to vtables, keep DECL_VIRTUAL flag.  */
  DECL_VIRTUAL_P (new_decl) = DECL_VIRTUAL_P (node->decl);
  if (TREE_CODE (new_decl) == FUNCTION_DECL)
    {
      DECL_STATIC_CONSTRUCTOR (new_decl) = 0;
      DECL_STATIC_DESTRUCTOR (new_decl) = 0;
      new_node = cgraph_node::create_alias (new_decl, node->decl);
    }
  else
    {
      TREE_READONLY (new_decl) = TREE_READONLY (node->decl);
      DECL_INITIAL (new_decl) = error_mark_node;
      new_node = varpool_node::create_alias (new_decl, node->decl);
    }
  new_node->resolve_alias (node);
  gcc_assert (decl_binds_to_current_def_p (new_decl)
	      && targetm.binds_local_p (new_decl));
  return new_node;
}

/* Return true if symtab node and TARGET represents
   semantically equivalent symbols.  */

bool
symtab_node::semantically_equivalent_p (symtab_node *target)
{
  enum availability avail;
  symtab_node *ba;
  symtab_node *bb;

  /* Equivalent functions are equivalent.  */
  if (decl == target->decl)
    return true;

  /* If symbol is not overwritable by different implementation,
     walk to the base object it defines.  */
  ba = ultimate_alias_target (&avail);
  if (avail >= AVAIL_AVAILABLE)
    {
      if (target == ba)
	return true;
    }
  else
    ba = this;
  bb = target->ultimate_alias_target (&avail);
  if (avail >= AVAIL_AVAILABLE)
    {
      if (this == bb)
	return true;
    }
  else
    bb = target;
  return bb == ba;
}

/* Classify symbol symtab node for partitioning.  */

enum symbol_partitioning_class
symtab_node::get_partitioning_class (void)
{
  /* Inline clones are always duplicated.
     This include external delcarations.   */
  cgraph_node *cnode = dyn_cast <cgraph_node *> (this);

  if (DECL_ABSTRACT_P (decl))
    return SYMBOL_EXTERNAL;

  if (cnode && cnode->global.inlined_to)
    return SYMBOL_DUPLICATE;

  /* Transparent aliases are always duplicated.  */
  if (transparent_alias)
    return definition ? SYMBOL_DUPLICATE : SYMBOL_EXTERNAL;

  /* External declarations are external.  */
  if (DECL_EXTERNAL (decl))
    return SYMBOL_EXTERNAL;

  if (varpool_node *vnode = dyn_cast <varpool_node *> (this))
    {
      if (alias && definition && !ultimate_alias_target ()->definition)
	return SYMBOL_EXTERNAL;
      /* Constant pool references use local symbol names that can not
         be promoted global.  We should never put into a constant pool
         objects that can not be duplicated across partitions.  */
      if (DECL_IN_CONSTANT_POOL (decl))
	return SYMBOL_DUPLICATE;
      if (DECL_HARD_REGISTER (decl))
	return SYMBOL_DUPLICATE;
      gcc_checking_assert (vnode->definition);
    }
  /* Functions that are cloned may stay in callgraph even if they are unused.
     Handle them as external; compute_ltrans_boundary take care to make
     proper things to happen (i.e. to make them appear in the boundary but
     with body streamed, so clone can me materialized).  */
  else if (!dyn_cast <cgraph_node *> (this)->function_symbol ()->definition)
    return SYMBOL_EXTERNAL;

  /* Linker discardable symbols are duplicated to every use unless they are
     keyed.  */
  if (DECL_ONE_ONLY (decl)
      && !force_output
      && !forced_by_abi
      && !used_from_object_file_p ())
    return SYMBOL_DUPLICATE;

  return SYMBOL_PARTITION;
}

/* Return true when symbol is known to be non-zero.  */

bool
symtab_node::nonzero_address ()
{
  /* Weakrefs may be NULL when their target is not defined.  */
  if (alias && weakref)
    {
      if (analyzed)
	{
	  symtab_node *target = ultimate_alias_target ();

	  if (target->alias && target->weakref)
	    return false;
	  /* We can not recurse to target::nonzero.  It is possible that the
	     target is used only via the alias.
	     We may walk references and look for strong use, but we do not know
	     if this strong use will survive to final binary, so be
	     conservative here.  
	     ??? Maybe we could do the lookup during late optimization that
	     could be useful to eliminate the NULL pointer checks in LTO
	     programs.  */
	  if (target->definition && !DECL_EXTERNAL (target->decl))
	      return true;
	  if (target->resolution != LDPR_UNKNOWN
	      && target->resolution != LDPR_UNDEF
	      && !target->can_be_discarded_p ()
	      && flag_delete_null_pointer_checks)
	    return true;
	  return false;
	}
      else
        return false;
    }

  /* With !flag_delete_null_pointer_checks we assume that symbols may
     bind to NULL. This is on by default on embedded targets only.

     Otherwise all non-WEAK symbols must be defined and thus non-NULL or
     linking fails.  Important case of WEAK we want to do well are comdats.
     Those are handled by later check for definition.

     When parsing, beware the cases when WEAK attribute is added later.  */
  if (!DECL_WEAK (decl)
      && flag_delete_null_pointer_checks)
    {
      refuse_visibility_changes = true;
      return true;
    }

  /* If target is defined and not extern, we know it will be output and thus
     it will bind to non-NULL.
     Play safe for flag_delete_null_pointer_checks where weak definition maye
     be re-defined by NULL.  */
  if (definition && !DECL_EXTERNAL (decl)
      && (flag_delete_null_pointer_checks || !DECL_WEAK (decl)))
    {
      if (!DECL_WEAK (decl))
        refuse_visibility_changes = true;
      return true;
    }

  /* As the last resort, check the resolution info.  */
  if (resolution != LDPR_UNKNOWN
      && resolution != LDPR_UNDEF
      && !can_be_discarded_p ()
      && flag_delete_null_pointer_checks)
    return true;
  return false;
}

/* Return 0 if symbol is known to have different address than S2,
   Return 1 if symbol is known to have same address as S2,
   return -1 otherwise.  

   If MEMORY_ACCESSED is true, assume that both memory pointer to THIS
   and S2 is going to be accessed.  This eliminates the situations when
   either THIS or S2 is NULL and is seful for comparing bases when deciding
   about memory aliasing.  */
int
symtab_node::equal_address_to (symtab_node *s2, bool memory_accessed)
{
  enum availability avail1, avail2;

  /* A Shortcut: equivalent symbols are always equivalent.  */
  if (this == s2)
    return 1;

  /* Unwind transparent aliases first; those are always equal to their
     target.  */
  if (this->transparent_alias && this->analyzed)
    return this->get_alias_target ()->equal_address_to (s2);
  while (s2->transparent_alias && s2->analyzed)
    s2 = s2->get_alias_target();

  if (this == s2)
    return 1;

  /* For non-interposable aliases, lookup and compare their actual definitions.
     Also check if the symbol needs to bind to given definition.  */
  symtab_node *rs1 = ultimate_alias_target (&avail1);
  symtab_node *rs2 = s2->ultimate_alias_target (&avail2);
  bool binds_local1 = rs1->analyzed && decl_binds_to_current_def_p (this->decl);
  bool binds_local2 = rs2->analyzed && decl_binds_to_current_def_p (s2->decl);
  bool really_binds_local1 = binds_local1;
  bool really_binds_local2 = binds_local2;

  /* Addresses of vtables and virtual functions can not be used by user
     code and are used only within speculation.  In this case we may make
     symbol equivalent to its alias even if interposition may break this
     rule.  Doing so will allow us to turn speculative inlining into
     non-speculative more agressively.  */
  if (DECL_VIRTUAL_P (this->decl) && avail1 >= AVAIL_AVAILABLE)
    binds_local1 = true;
  if (DECL_VIRTUAL_P (s2->decl) && avail2 >= AVAIL_AVAILABLE)
    binds_local2 = true;

  /* If both definitions are available we know that even if they are bound
     to other unit they must be defined same way and therefore we can use
     equivalence test.  */
  if (rs1 != rs2 && avail1 >= AVAIL_AVAILABLE && avail2 >= AVAIL_AVAILABLE)
    binds_local1 = binds_local2 = true;

  if (binds_local1 && binds_local2 && rs1 == rs2)
    {
      /* We made use of the fact that alias is not weak.  */
      if (rs1 != this)
        refuse_visibility_changes = true;
      if (rs2 != s2)
        s2->refuse_visibility_changes = true;
      return 1;
    }

  /* If both symbols may resolve to NULL, we can not really prove them
     different.  */
  if (!memory_accessed && !nonzero_address () && !s2->nonzero_address ())
    return -1;

  /* Except for NULL, functions and variables never overlap.  */
  if (TREE_CODE (decl) != TREE_CODE (s2->decl))
    return 0;

  /* If one of the symbols is unresolved alias, punt.  */
  if (rs1->alias || rs2->alias)
    return -1;

  /* If we have a non-interposale definition of at least one of the symbols
     and the other symbol is different, we know other unit can not interpose
     it to the first symbol; all aliases of the definition needs to be 
     present in the current unit.  */
  if (((really_binds_local1 || really_binds_local2)
      /* If we have both definitions and they are different, we know they
	 will be different even in units they binds to.  */
       || (binds_local1 && binds_local2))
      && rs1 != rs2)
    {
      /* We make use of the fact that one symbol is not alias of the other
	 and that the definition is non-interposable.  */
      refuse_visibility_changes = true;
      s2->refuse_visibility_changes = true;
      rs1->refuse_visibility_changes = true;
      rs2->refuse_visibility_changes = true;
      return 0;
    }

  /* TODO: Alias oracle basically assume that addresses of global variables
     are different unless they are declared as alias of one to another while
     the code folding comparsions doesn't.
     We probably should be consistent and use this fact here, too, but for
     the moment return false only when we are called from the alias oracle.  */

  return memory_accessed && rs1 != rs2 ? 0 : -1;
}

/* Worker for call_for_symbol_and_aliases.  */

bool
symtab_node::call_for_symbol_and_aliases_1 (bool (*callback) (symtab_node *,
							      void *),
					    void *data,
					    bool include_overwritable)
{
  ipa_ref *ref;
  FOR_EACH_ALIAS (this, ref)
    {
      symtab_node *alias = ref->referring;
      if (include_overwritable
	  || alias->get_availability () > AVAIL_INTERPOSABLE)
	if (alias->call_for_symbol_and_aliases (callback, data,
					      include_overwritable))
	  return true;
    }
  return false;
}

/* Return true if address of N is possibly compared.  */

static bool
address_matters_1 (symtab_node *n, void *)
{
  struct ipa_ref *ref;

  if (!n->address_can_be_compared_p ())
    return false;
  if (n->externally_visible || n->force_output)
    return true;

  for (unsigned int i = 0; n->iterate_referring (i, ref); i++)
    if (ref->address_matters_p ())
      return true;
  return false;
}

/* Return true if symbol's address may possibly be compared to other
   symbol's address.  */

bool
symtab_node::address_matters_p ()
{
  gcc_assert (!alias);
  return call_for_symbol_and_aliases (address_matters_1, NULL, true);
}

/* Return true if symbol's alignment may be increased.  */

bool
symtab_node::can_increase_alignment_p (void)
{
  symtab_node *target = ultimate_alias_target ();

  /* For now support only variables.  */
  if (!VAR_P (decl))
    return false;

  /* With -fno-toplevel-reorder we may have already output the constant.  */
  if (TREE_ASM_WRITTEN (target->decl))
    return false;

  /* If target is already placed in an anchor, we can not touch its
     alignment.  */
  if (DECL_RTL_SET_P (target->decl)
      && MEM_P (DECL_RTL (target->decl))
      && SYMBOL_REF_HAS_BLOCK_INFO_P (XEXP (DECL_RTL (target->decl), 0)))
    return false;

  /* Constant pool entries may be shared.  */
  if (DECL_IN_CONSTANT_POOL (target->decl))
    return false;

  /* We cannot change alignment of symbols that may bind to symbols
     in other translation unit that may contain a definition with lower
     alignment.  */
  if (!decl_binds_to_current_def_p (decl))
    return false;

  /* When compiling partition, be sure the symbol is not output by other
     partition.  */
  if (flag_ltrans
      && (target->in_other_partition
	  || target->get_partitioning_class () == SYMBOL_DUPLICATE))
    return false;

  /* Do not override the alignment as specified by the ABI when the used
     attribute is set.  */
  if (DECL_PRESERVE_P (decl) || DECL_PRESERVE_P (target->decl))
    return false;

  /* Do not override explicit alignment set by the user when an explicit
     section name is also used.  This is a common idiom used by many
     software projects.  */
  if (DECL_SECTION_NAME (target->decl) != NULL && !target->implicit_section)
    return false;

  return true;
}

/* Worker for symtab_node::increase_alignment.  */

static bool
increase_alignment_1 (symtab_node *n, void *v)
{
  unsigned int align = (size_t)v;
  if (DECL_ALIGN (n->decl) < align
      && n->can_increase_alignment_p ())
    {
      SET_DECL_ALIGN (n->decl, align);
      DECL_USER_ALIGN (n->decl) = 1;
    }
  return false;
}

/* Increase alignment of THIS to ALIGN.  */

void
symtab_node::increase_alignment (unsigned int align)
{
  gcc_assert (can_increase_alignment_p () && align < MAX_OFILE_ALIGNMENT);
  ultimate_alias_target()->call_for_symbol_and_aliases (increase_alignment_1,
						        (void *)(size_t) align,
						        true);
  gcc_assert (DECL_ALIGN (decl) >= align);
}

/* Helper for symtab_node::definition_alignment.  */

static bool
get_alignment_1 (symtab_node *n, void *v)
{
  *((unsigned int *)v) = MAX (*((unsigned int *)v), DECL_ALIGN (n->decl));
  return false;
}

/* Return desired alignment of the definition.  This is NOT alignment useful
   to access THIS, because THIS may be interposable and DECL_ALIGN should
   be used instead.  It however must be guaranteed when output definition
   of THIS.  */

unsigned int
symtab_node::definition_alignment ()
{
  unsigned int align = 0;
  gcc_assert (!alias);
  call_for_symbol_and_aliases (get_alignment_1, &align, true);
  return align;
}

/* Return symbol used to separate symbol name from suffix.  */

char 
symbol_table::symbol_suffix_separator ()
{
#ifndef NO_DOT_IN_LABEL
  return '.';
#elif !defined NO_DOLLAR_IN_LABEL
  return '$';
#else
  return '_';
#endif
}

/* Return true when references to this symbol from REF must bind to current
   definition in final executable.  */

bool
symtab_node::binds_to_current_def_p (symtab_node *ref)
{
  if (!definition)
    return false;
  if (transparent_alias)
    return definition
	   && get_alias_target()->binds_to_current_def_p (ref);
  if (lookup_attribute ("ifunc", DECL_ATTRIBUTES (decl)))
    return false;
  if (decl_binds_to_current_def_p (decl))
    return true;

  /* Inline clones always binds locally.  */
  cgraph_node *cnode = dyn_cast <cgraph_node *> (this);
  if (cnode && cnode->global.inlined_to)
    return true;

  if (DECL_EXTERNAL (decl))
    return false;

  gcc_assert (externally_visible);

  if (ref)
    {
      cgraph_node *cref = dyn_cast <cgraph_node *> (ref);
      if (cref)
	ref = cref->global.inlined_to;
    }

  /* If this is a reference from symbol itself and there are no aliases, we
     may be sure that the symbol was not interposed by something else because
     the symbol itself would be unreachable otherwise.  This is important
     to optimize recursive functions well.

     This assumption may be broken by inlining: if symbol is interposable
     but the body is available (i.e. declared inline), inliner may make
     the body reachable even with interposition.  */
  if (this == ref && !has_aliases_p ()
      && (!cnode
	  || symtab->state >= IPA_SSA_AFTER_INLINING
	  || get_availability () >= AVAIL_INTERPOSABLE))
    return true;


  /* References within one comdat group are always bound in a group.  */
  if (ref
      && symtab->state >= IPA_SSA_AFTER_INLINING
      && get_comdat_group ()
      && get_comdat_group () == ref->get_comdat_group ())
    return true;

  return false;
}
