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
#include "ipa-utils.h"
#include "calls.h"

static const char *ipa_ref_use_name[] = {"read","write","addr","alias"};

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


/* Returns a hash code for P.  */

hashval_t
symbol_table::hash_node_by_assembler_name (const void *p)
{
  const symtab_node *n = (const symtab_node *) p;
  return (hashval_t) decl_assembler_name_hash (DECL_ASSEMBLER_NAME (n->decl));
}

/* Compare ASMNAME with the DECL_ASSEMBLER_NAME of DECL.  */

bool
symbol_table::decl_assembler_name_equal (tree decl, const_tree asmname)
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

int
symbol_table::eq_assembler_name (const void *p1, const void *p2)
{
  const symtab_node *n1 = (const symtab_node *) p1;
  const_tree name = (const_tree)p2;
  return (decl_assembler_name_equal (n1->decl, name));
}

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
      void **aslot;
      cgraph_node *cnode;
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
      assembler_name_hash =
	htab_create_ggc (10, hash_node_by_assembler_name, eq_assembler_name,
			 NULL);
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
  if ((TREE_CODE (decl) == VAR_DECL
       && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
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

/* Hash sections by their names.  */

static hashval_t
hash_section_hash_entry (const void *p)
{
  const section_hash_entry *n = (const section_hash_entry *) p;
  return htab_hash_string (n->name);
}

/* Return true if section P1 name equals to P2.  */

static int
eq_sections (const void *p1, const void *p2)
{
  const section_hash_entry *n1 = (const section_hash_entry *) p1;
  const char *name = (const char *)p2;
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
    return lang_hooks.decl_printable_name (decl, 2);
  return IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
}

/* Return printable identifier name.  */

const char *
symtab_node::name () const
{
  return lang_hooks.decl_printable_name (decl, 2);
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
			       enum ipa_ref_use use_type, gimple stmt)
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

/* If VAL is a reference to a function or a variable, add a reference from
   this symtab_node to the corresponding symbol table node.  USE_TYPE specify
   type of the use and STMT the statement (if it exists).  Return the new
   reference or NULL if none was created.  */

ipa_ref *
symtab_node::maybe_create_reference (tree val, enum ipa_ref_use use_type,
				     gimple stmt)
{
  STRIP_NOPS (val);
  if (TREE_CODE (val) != ADDR_EXPR)
    return NULL;
  val = get_base_var (val);
  if (val && (TREE_CODE (val) == FUNCTION_DECL
	       || TREE_CODE (val) == VAR_DECL))
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
symtab_node::clone_reference (ipa_ref *ref, gimple stmt)
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
			     gimple stmt, unsigned int lto_stmt_uid)
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
symtab_node::remove_stmt_references (gimple stmt)
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
      fprintf (file, "%s/%i (%s)",
               ref->referred->asm_name (),
               ref->referred->order,
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
      fprintf (file, "%s/%i (%s)",
               ref->referring->asm_name (),
               ref->referring->order,
	       ipa_ref_use_name [ref->use]);
      if (ref->speculative)
	fprintf (file, " (speculative)");
    }
  fprintf (file, "\n");
}

/* Return true if list contains an alias.  */
bool
symtab_node::has_aliases_p (void)
{
  ipa_ref *ref = NULL;
  int i;

  for (i = 0; iterate_referring (i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
      return true;
  return false;
}

/* Iterates I-th reference in the list, REF is also set.  */

ipa_ref *
symtab_node::iterate_reference (unsigned i, ipa_ref *&ref)
{
  vec_safe_iterate (ref_list.references, i, &ref);

  return ref;
}

/* Iterates I-th referring item in the list, REF is also set.  */

ipa_ref *
symtab_node::iterate_referring (unsigned i, ipa_ref *&ref)
{
  ref_list.referring.iterate (i, &ref);

  return ref;
}

/* Iterates I-th referring alias item in the list, REF is also set.  */

ipa_ref *
symtab_node::iterate_direct_aliases (unsigned i, ipa_ref *&ref)
{
  ref_list.referring.iterate (i, &ref);

  if (ref && ref->use != IPA_REF_ALIAS)
    return NULL;

  return ref;
}

static const char * const symtab_type_names[] = {"symbol", "function", "variable"};

/* Dump base fields of symtab nodes to F.  Not to be used directly.  */

void
symtab_node::dump_base (FILE *f)
{
  static const char * const visibility_types[] = {
    "default", "protected", "hidden", "internal"
  };

  fprintf (f, "%s/%i (%s)", asm_name (), order, name ());
  dump_addr (f, " @", (void *)this);
  fprintf (f, "\n  Type: %s", symtab_type_names[type]);

  if (definition)
    fprintf (f, " definition");
  if (analyzed)
    fprintf (f, " analyzed");
  if (alias)
    fprintf (f, " alias");
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
    fprintf (f, "  Same comdat group as: %s/%i\n",
	     same_comdat_group->asm_name (),
	     same_comdat_group->order);
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

/* Dump symbol table to F.  */

void
symtab_node::dump_table (FILE *f)
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
  void **slot;

  symtab->symtab_initialize_asm_name_hash ();
  slot = htab_find_slot_with_hash (symtab->assembler_name_hash, asmname,
				   symtab->decl_assembler_name_hash (asmname),
				   NO_INSERT);

  if (slot)
    {
      node = (symtab_node *) *slot;
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
      if (TREE_CODE (decl) != VAR_DECL)
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
	       || DECL_HARD_REGISTER (decl)))
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
  if (analyzed && !definition)
    {
      error ("node is analyzed byt it is not a definition");
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
  if (weakref && !alias)
    {
      error ("node is weakref but not an alias");
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
      if (!n->definition)
	{
	  error ("Node has same_comdat_group but it is not a definition");
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
      && !implicit_section)
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
	  else
	    for (s = (*entry)->same_comdat_group; s != NULL && s != node; s = s->same_comdat_group)
	      if (!s || s == *entry)
		{
		  error ("Two symbols with same comdat_group are not linked by the same_comdat_group list.");
		  (*entry)->debug ();
		  node->debug ();
		  internal_error ("symtab_node::verify failed");
		}
	}
    }
}

/* Return true when NODE is known to be used from other (non-LTO)
   object file. Known only when doing LTO via linker plugin.  */

bool
symtab_node::used_from_object_file_p_worker (symtab_node *node)
{
  if (!TREE_PUBLIC (node->decl) || DECL_EXTERNAL (node->decl))
    return false;
  if (resolution_used_from_other_file_p (node->resolution))
    return true;
  return false;
}


/* Return true when symtab_node is known to be used from other (non-LTO)
   object file. Known only when doing LTO via linker plugin.  */

bool
symtab_node::used_from_object_file_p (void)
{
  return symtab_node::used_from_object_file_p_worker (this);
}

/* Make DECL local.  FIXME: We shouldn't need to mess with rtl this early,
   but other code such as notice_global_symbol generates rtl.  */

void
symtab_node::make_decl_local (void)
{
  rtx rtl, symbol;

  /* Avoid clearing comdat_groups on comdat-local decls.  */
  if (TREE_PUBLIC (decl) == 0)
    return;

  if (TREE_CODE (decl) == VAR_DECL)
    DECL_COMMON (decl) = 0;
  else gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);

  DECL_COMDAT (decl) = 0;
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

/* Walk the alias chain to return the symbol NODE is alias of.
   If NODE is not an alias, return NODE.
   When AVAILABILITY is non-NULL, get minimal availability in the chain.  */

symtab_node *
symtab_node::ultimate_alias_target (enum availability *availability)
{
  bool weakref_p = false;

  if (!alias)
    {
      if (availability)
	*availability = get_availability ();
      return this;
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
      weakref_p = weakref;
      if (!weakref_p)
	*availability = get_availability ();
      else
	*availability = AVAIL_LOCAL;
    }

  symtab_node *node = this;
  while (node)
    {
      if (node->alias && node->analyzed)
	node = node->get_alias_target ();
      else
	{
	  if (!availability)
	    ;
	  else if (node->analyzed)
	    {
	      if (weakref_p)
		{
		  enum availability a = node->get_availability ();
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
	  enum availability a = node->get_availability ();
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
  DECL_VIRTUAL_P (decl) = DECL_VIRTUAL_P (target->decl);
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
   When one wants to change section of symbol and its aliases,
   use set_section.  */

void
symtab_node::set_section_for_node (const char *section)
{
  const char *current = get_section ();
  void **slot;

  if (current == section
      || (current && section
	  && !strcmp (current, section)))
    return;

  if (current)
    {
      x_section->ref_count--;
      if (!x_section->ref_count)
	{
	  slot = htab_find_slot_with_hash (symtab->section_hash, x_section->name,
					   htab_hash_string (x_section->name),
					   INSERT);
	  ggc_free (x_section);
	  htab_clear_slot (symtab->section_hash, slot);
	}
      x_section = NULL;
    }
  if (!section)
    {
      implicit_section = false;
      return;
    }
  if (!symtab->section_hash)
    symtab->section_hash = htab_create_ggc (10, hash_section_hash_entry,
				    eq_sections, NULL);
  slot = htab_find_slot_with_hash (symtab->section_hash, section,
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

/* Return availability of NODE.  */
enum availability symtab_node::get_availability (void)
{
  if (is_a <cgraph_node *> (this))
    return dyn_cast <cgraph_node *> (this)->get_availability ();
  else
    return dyn_cast <varpool_node *> (this)->get_availability ();;
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
symtab_node::resolve_alias (symtab_node *target)
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

  if (cpp_implicit_alias && symtab->state >= CONSTRUCTION)
    fixup_same_cpp_alias_visibility (target);

  /* If alias has address taken, so does the target.  */
  if (address_taken)
    target->ultimate_alias_target ()->address_taken = true;
  return true;
}

/* Call calback on symtab node and aliases associated to this node.
   When INCLUDE_OVERWRITABLE is false, overwritable aliases and thunks are
   skipped. */

bool
symtab_node::call_for_symbol_and_aliases (bool (*callback) (symtab_node *,
							  void *),
					void *data, bool include_overwritable)
{
  int i;
  ipa_ref *ref;

  if (callback (this, data))
    return true;
  for (i = 0; iterate_referring (i, ref); i++)
    if (ref->use == IPA_REF_ALIAS)
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

/* Worker searching noninterposable alias.  */

bool
symtab_node::noninterposable_alias (symtab_node *node, void *data)
{
  if (decl_binds_to_current_def_p (node->decl))
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

  if (DECL_ABSTRACT (decl))
    return SYMBOL_EXTERNAL;

  if (cnode && cnode->global.inlined_to)
    return SYMBOL_DUPLICATE;

  /* Weakref aliases are always duplicated.  */
  if (weakref)
    return SYMBOL_DUPLICATE;

  /* External declarations are external.  */
  if (DECL_EXTERNAL (decl))
    return SYMBOL_EXTERNAL;

  if (varpool_node *vnode = dyn_cast <varpool_node *> (this))
    {
      /* Constant pool references use local symbol names that can not
         be promoted global.  We should never put into a constant pool
         objects that can not be duplicated across partitions.  */
      if (DECL_IN_CONSTANT_POOL (decl))
	return SYMBOL_DUPLICATE;
      gcc_checking_assert (vnode->definition);
    }
  /* Functions that are cloned may stay in callgraph even if they are unused.
     Handle them as external; compute_ltrans_boundary take care to make
     proper things to happen (i.e. to make them appear in the boundary but
     with body streamed, so clone can me materialized).  */
  else if (!dyn_cast <cgraph_node *> (this)->definition)
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
      && flag_delete_null_pointer_checks)
    return true;
  return false;
}
