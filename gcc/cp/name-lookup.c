/* Definitions for C++ name lookup routines.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "cp-tree.h"
#include "name-lookup.h"
#include "timevar.h"
#include "toplev.h"

/* Compute the chain index of a binding_entry given the HASH value of its
   name and the total COUNT of chains.  COUNT is assumed to be a power
   of 2.  */
#define ENTRY_INDEX(HASH, COUNT) (((HASH) >> 3) & ((COUNT) - 1))

/* A free list of "binding_entry"s awaiting for re-use.  */
static GTY((deletable(""))) binding_entry free_binding_entry = NULL;

/* Create a binding_entry object for (NAME, TYPE).  */
static inline binding_entry
binding_entry_make (tree name, tree type)
{
  binding_entry entry;

  if (free_binding_entry)
    {
      entry = free_binding_entry;
      free_binding_entry = entry->chain;
    }
  else
    entry = ggc_alloc (sizeof (struct binding_entry_s));

  entry->name = name;
  entry->type = type;
  entry->chain = NULL;

  return entry;
}

/* Put ENTRY back on the free list.  */
static inline void
binding_entry_free (binding_entry entry)
{
  entry->chain = free_binding_entry;
  free_binding_entry = entry;
}

/* The datatype used to implement the mapping from names to types at
   a given scope.  */
struct binding_table_s GTY(())
{
  /* Array of chains of "binding_entry"s  */
  binding_entry * GTY((length ("%h.chain_count"))) chain;

  /* The number of chains in this table.  This is the length of the
     the member "chain" considered as an array.  */
  size_t chain_count;

  /* Number of "binding_entry"s in this table.  */
  size_t entry_count;
};

/* Construct TABLE with an initial CHAIN_COUNT.  */
static inline void
binding_table_construct (binding_table table, size_t chain_count)
{
  table->chain_count = chain_count;
  table->entry_count = 0;
  table->chain = ggc_alloc_cleared
    (table->chain_count * sizeof (binding_entry));
}

/* Free TABLE by making its entries ready for reuse.  */
void
binding_table_free (binding_table table)
{
  size_t i;
  if (table == NULL)
    return;

  for (i = 0; i < table->chain_count; ++i)
    {
      binding_entry temp = table->chain[i];
      while (temp != NULL)
        {
          binding_entry entry = temp;
          temp = entry->chain;
          entry->chain = NULL; 
          binding_entry_free (entry);
        }
      table->chain[i] = temp;
    }
  table->entry_count = 0;
}

/* Allocate a table with CHAIN_COUNT, assumed to be a power of two.  */
binding_table
binding_table_new (size_t chain_count)
{
  binding_table table = ggc_alloc (sizeof (struct binding_table_s));
  table->chain = NULL;
  binding_table_construct (table, chain_count);
  return table;
}

/* Expand TABLE to twice its current chain_count.  */
static void
binding_table_expand (binding_table table)
{
  const size_t old_chain_count = table->chain_count;
  const size_t old_entry_count = table->entry_count;
  const size_t new_chain_count = 2 * old_chain_count;
  binding_entry *old_chains = table->chain;
  size_t i;

  binding_table_construct (table, new_chain_count);
  for (i = 0; i < old_chain_count; ++i)
    {
      binding_entry entry = old_chains[i];
      for (; entry != NULL; entry = old_chains[i])
        {
          const unsigned int hash = IDENTIFIER_HASH_VALUE (entry->name);
          const size_t j = ENTRY_INDEX (hash, new_chain_count);

          old_chains[i] = entry->chain;
          entry->chain = table->chain[j];
          table->chain[j] = entry;
        }
    }
  table->entry_count = old_entry_count;
}

/* Insert a binding for NAME to TYPe into TABLE.  */
void
binding_table_insert (binding_table table, tree name, tree type)
{
  const unsigned int hash = IDENTIFIER_HASH_VALUE (name);
  const size_t i = ENTRY_INDEX (hash, table->chain_count);
  binding_entry entry = binding_entry_make (name, type);

  entry->chain = table->chain[i];
  table->chain[i] = entry;
  ++table->entry_count;

  if (3 * table->chain_count < 5 * table->entry_count)
    binding_table_expand (table);
}

/* Return the binding_entry, if any, that maps NAME.  */
binding_entry
binding_table_find (binding_table table, tree name)
{
  const unsigned int hash = IDENTIFIER_HASH_VALUE (name);
  binding_entry entry = table->chain[ENTRY_INDEX (hash, table->chain_count)];

  while (entry != NULL && entry->name != name)
    entry = entry->chain;

  return entry;
}

/* Return the binding_entry, if any, that maps name to an anonymous type.  */
tree
binding_table_find_anon_type (binding_table table, tree name)
{
  const unsigned int hash = IDENTIFIER_HASH_VALUE (name);
  binding_entry entry = table->chain[ENTRY_INDEX (hash, table->chain_count)];

  while (entry != NULL && TYPE_IDENTIFIER (entry->type) != name)
    entry = entry->chain;

  return entry ? entry->type : NULL;
}

/* Return the binding_entry, if any, that has TYPE as target.  If NAME
   is non-null, then set the domain and rehash that entry.  */
binding_entry
binding_table_reverse_maybe_remap (binding_table table, tree type, tree name)
{
  const size_t chain_count = table->chain_count;
  binding_entry entry = NULL;
  binding_entry *p = NULL;
  size_t i;

  for (i = 0; i < chain_count && entry == NULL; ++i)
    {
      p = &table->chain[i];
      while (*p != NULL && entry == NULL)
        if ((*p)->type == type)
          entry = *p;
        else
          p = &(*p)->chain;
    }

  if (entry != NULL && name != NULL && entry->name != name)
    {
      /* Remove the bucket from the previous chain.  */
      *p = (*p)->chain;

      /* Remap the name type to type.  */
      i = ENTRY_INDEX (IDENTIFIER_HASH_VALUE (name), chain_count);
      entry->chain = table->chain[i];
      entry->name = name;
      table->chain[i] = entry;
    }

  return entry;
}

/* Remove from TABLE all entries that map to anonymous enums or
   class-types.  */
void
binding_table_remove_anonymous_types (binding_table table)
{
  const size_t chain_count = table->chain_count;
  size_t i;

  for (i = 0; i < chain_count; ++i)
    {
      binding_entry *p = &table->chain[i];

      while (*p != NULL)
        if (ANON_AGGRNAME_P ((*p)->name))
          {
            binding_entry e = *p;
            *p = (*p)->chain;
            --table->entry_count;
            binding_entry_free (e);
          }
        else
          p = &(*p)->chain;
    }
}

/* Apply PROC -- with DATA -- to all entries in TABLE.  */
void
binding_table_foreach (binding_table table, bt_foreach_proc proc, void *data)
{
  const size_t chain_count = table->chain_count;
  size_t i;

  for (i = 0; i < chain_count; ++i)
    {
      binding_entry entry = table->chain[i];
      for (; entry != NULL; entry = entry->chain)
        proc (entry, data);
    }
}


/* A free list of "cxx_binding"s, connected by their PREVIOUS.  */
static GTY((deletable (""))) cxx_binding *free_bindings;

/* (GC)-allocate a binding object with VALUE and TYPE member initialized.  */
cxx_binding *
cxx_binding_make (tree value, tree type)
{
  cxx_binding *binding;
  if (free_bindings)
    {
      binding = free_bindings;
      free_bindings = binding->previous;
    }
  else
    binding = ggc_alloc (sizeof (cxx_binding));

  binding->value = value;
  binding->type = type;
  binding->previous = NULL;

  return binding;
}

/* Put BINDING back on the free list.  */
void
cxx_binding_free (cxx_binding *binding)
{
  binding->previous = free_bindings;
  free_bindings = binding;
}

/* BINDING records an existing declaration for a namein the current scope.
   But, DECL is another declaration for that same identifier in the
   same scope.  This is the `struct stat' hack whereby a non-typedef
   class name or enum-name can be bound at the same level as some other
   kind of entity.
   3.3.7/1

     A class name (9.1) or enumeration name (7.2) can be hidden by the
     name of an object, function, or enumerator declared in the same scope.
     If a class or enumeration name and an object, function, or enumerator
     are declared in the same scope (in any order) with the same name, the
     class or enumeration name is hidden wherever the object, function, or
     enumerator name is visible.

   It's the responsibility of the caller to check that
   inserting this name is valid here.  Returns nonzero if the new binding
   was successful.  */

bool
supplement_binding (cxx_binding *binding, tree decl)
{
  tree bval = binding->value;
  bool ok = true;

  timevar_push (TV_NAME_LOOKUP);
  if (TREE_CODE (decl) == TYPE_DECL && DECL_ARTIFICIAL (decl))
    /* The new name is the type name.  */
    binding->type = decl;
  else if (!bval)
    /* This situation arises when push_class_level_binding moves an
       inherited type-binding out of the way to make room for a new
       value binding.  */
    binding->value = decl;
  else if (TREE_CODE (bval) == TYPE_DECL && DECL_ARTIFICIAL (bval))
    {
      /* The old binding was a type name.  It was placed in
	 BINDING_VALUE because it was thought, at the point it was
	 declared, to be the only entity with such a name.  Move the
	 type name into the type slot; it is now hidden by the new
	 binding.  */
      binding->type = bval;
      binding->value = decl;
      binding->value_is_inherited = false;
    }
  else if (TREE_CODE (bval) == TYPE_DECL
	   && TREE_CODE (decl) == TYPE_DECL
	   && DECL_NAME (decl) == DECL_NAME (bval)
	   && (same_type_p (TREE_TYPE (decl), TREE_TYPE (bval))
	       /* If either type involves template parameters, we must
		  wait until instantiation.  */
	       || uses_template_parms (TREE_TYPE (decl))
	       || uses_template_parms (TREE_TYPE (bval))))
    /* We have two typedef-names, both naming the same type to have
       the same name.  This is OK because of:

         [dcl.typedef]

	 In a given scope, a typedef specifier can be used to redefine
	 the name of any type declared in that scope to refer to the
	 type to which it already refers.  */
    ok = false;
  /* There can be two block-scope declarations of the same variable,
     so long as they are `extern' declarations.  However, there cannot
     be two declarations of the same static data member:

       [class.mem]

       A member shall not be declared twice in the
       member-specification.  */
  else if (TREE_CODE (decl) == VAR_DECL && TREE_CODE (bval) == VAR_DECL
	   && DECL_EXTERNAL (decl) && DECL_EXTERNAL (bval)
	   && !DECL_CLASS_SCOPE_P (decl))
    {
      duplicate_decls (decl, binding->value);
      ok = false;
    }
  else
    {
      error ("declaration of `%#D'", decl);
      cp_error_at ("conflicts with previous declaration `%#D'",
		   binding->value);
      ok = false;
    }

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, ok);
}

/* Return (from the stack of) the BINDING, if any, establihsed at SCOPE.  */ 

static inline cxx_binding *
find_binding (cxx_scope *scope, cxx_binding *binding)
{
  timevar_push (TV_NAME_LOOKUP);

  for (; binding != NULL; binding = binding->previous)
    if (BINDING_SCOPE (binding) == scope)
      POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, binding);

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, (cxx_binding *)0);
}

/* Return the binding for NAME in SCOPE, if any.  Otherwise, return NULL.  */
cxx_binding *
cxx_scope_find_binding_for_name (cxx_scope *scope, tree name)
{
  cxx_binding *b = IDENTIFIER_NAMESPACE_BINDINGS (name);
  if (b)
    {
      /* Fold-in case where NAME is used only once.  */
      if (scope == BINDING_SCOPE (b) && b->previous == NULL)
        return b;
      return find_binding (scope, b);
    }
  return NULL;
}

/* Always returns a binding for name in scope.  If no binding is
   found, make a new one.  */

cxx_binding *
binding_for_name (cxx_scope *scope, tree name)
{
  cxx_binding *result;

  result = cxx_scope_find_binding_for_name (scope, name);
  if (result)
    return result;
  /* Not found, make a new one.  */
  result = cxx_binding_make (NULL, NULL);
  result->previous = IDENTIFIER_NAMESPACE_BINDINGS (name);
  BINDING_SCOPE (result) = scope;
  result->is_local = false;
  result->value_is_inherited = false;
  IDENTIFIER_NAMESPACE_BINDINGS (name) = result;
  return result;
}

/* Namespace-scope manipulation routines.  */

/* Return the binding value for name in scope.  */

tree
namespace_binding (tree name, tree scope)
{
  cxx_binding *binding;

  if (scope == NULL)
    scope = global_namespace;
  scope = ORIGINAL_NAMESPACE (scope);
  binding = cxx_scope_find_binding_for_name (NAMESPACE_LEVEL (scope), name);

  return binding ? binding->value : NULL_TREE;
}

/* Set the binding value for name in scope.  */

void
set_namespace_binding (tree name, tree scope, tree val)
{
  cxx_binding *b;

  timevar_push (TV_NAME_LOOKUP);
  if (scope == NULL_TREE)
    scope = global_namespace;
  b = binding_for_name (NAMESPACE_LEVEL (scope), name);
  if (!BINDING_VALUE (b)
      || TREE_CODE (val) == OVERLOAD 
      || val == error_mark_node)
    BINDING_VALUE (b) = val;
  else
    supplement_binding (b, val);
  timevar_pop (TV_NAME_LOOKUP);
}

#include "gt-cp-name-lookup.h"
