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
#include "diagnostic.h"

static cxx_scope *innermost_nonclass_level (void);
static tree select_decl (cxx_binding *, int);


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
  entry->name = NULL;
  entry->type = NULL;
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

/* Make TABLE's entries ready for reuse.  */

static void
binding_table_free (binding_table table)
{
  size_t i;
  size_t count;

  if (table == NULL)
    return;

  for (i = 0, count = table->chain_count; i < count; ++i)
    {
      binding_entry temp = table->chain[i];
      while (temp != NULL)
        {
          binding_entry entry = temp;
          temp = entry->chain;
          binding_entry_free (entry);
        }
      table->chain[i] = NULL;
    }
  table->entry_count = 0;
}

/* Allocate a table with CHAIN_COUNT, assumed to be a power of two.  */

static inline binding_table
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

/* Insert a binding for NAME to TYPE into TABLE.  */

static void
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

/* Return the binding_entry, if any, that maps NAME to an anonymous type.  */

static tree
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

static binding_entry
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

#ifndef ENABLE_SCOPE_CHECKING
#  define ENABLE_SCOPE_CHECKING 0
#else
#  define ENABLE_SCOPE_CHECKING 1
#endif

/* A free list of "cxx_binding"s, connected by their PREVIOUS.  */

static GTY((deletable (""))) cxx_binding *free_bindings;

/* (GC)-allocate a binding object with VALUE and TYPE member initialized.  */

static cxx_binding *
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

static inline void
cxx_binding_free (cxx_binding *binding)
{
  binding->scope = NULL;
  binding->previous = free_bindings;
  free_bindings = binding;
}

/* Make DECL the innermost binding for ID.  The LEVEL is the binding
   level at which this declaration is being bound.  */

static void
push_binding (tree id, tree decl, cxx_scope* level)
{
   cxx_binding *binding = cxx_binding_make (decl, NULL);

  /* Now, fill in the binding information.  */
  binding->previous = IDENTIFIER_BINDING (id);
  binding->scope = level;
  INHERITED_VALUE_BINDING_P (binding) = 0;
  LOCAL_BINDING_P (binding) = (level != class_binding_level);

  /* And put it on the front of the list of bindings for ID.  */
  IDENTIFIER_BINDING (id) = binding;
}

/* Remove the binding for DECL which should be the innermost binding
   for ID.  */

void
pop_binding (tree id, tree decl)
{
  cxx_binding *binding;

  if (id == NULL_TREE)
    /* It's easiest to write the loops that call this function without
       checking whether or not the entities involved have names.  We
       get here for such an entity.  */
    return;

  /* Get the innermost binding for ID.  */
  binding = IDENTIFIER_BINDING (id);

  /* The name should be bound.  */
  my_friendly_assert (binding != NULL, 0);

  /* The DECL will be either the ordinary binding or the type
     binding for this identifier.  Remove that binding.  */
  if (binding->value == decl)
    binding->value = NULL_TREE;
  else if (binding->type == decl)
    binding->type = NULL_TREE;
  else
    abort ();

  if (!binding->value && !binding->type)
    {
      /* We're completely done with the innermost binding for this
	 identifier.  Unhook it from the list of bindings.  */
      IDENTIFIER_BINDING (id) = binding->previous;

      /* Add it to the free list.  */
      cxx_binding_free (binding);
    }
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

static bool
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
	 VALUE field because it was thought, at the point it was
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

/* Add DECL to the list of things declared in B.  */

void
add_decl_to_level (tree decl, cxx_scope *b)
{
  if (TREE_CODE (decl) == NAMESPACE_DECL 
      && !DECL_NAMESPACE_ALIAS (decl))
    {
      TREE_CHAIN (decl) = b->namespaces;
      b->namespaces = decl;
    }
  else if (TREE_CODE (decl) == VAR_DECL && DECL_VIRTUAL_P (decl))
    {
      TREE_CHAIN (decl) = b->vtables;
      b->vtables = decl;
    }
  else       
    {
      /* We build up the list in reverse order, and reverse it later if
         necessary.  */
      TREE_CHAIN (decl) = b->names;
      b->names = decl;
      b->names_size++;

      /* If appropriate, add decl to separate list of statics */
      if (b->kind == sk_namespace)
	if ((TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl))
	    || (TREE_CODE (decl) == FUNCTION_DECL
		&& (!TREE_PUBLIC (decl) || DECL_DECLARED_INLINE_P (decl))))
	  VARRAY_PUSH_TREE (b->static_decls, decl);
    }
}

/* Bind DECL to ID in the current_binding_level, assumed to be a local
   binding level.  If PUSH_USING is set in FLAGS, we know that DECL
   doesn't really belong to this binding level, that it got here
   through a using-declaration.  */

void
push_local_binding (tree id, tree decl, int flags)
{
  struct cp_binding_level *b;

  /* Skip over any local classes.  This makes sense if we call
     push_local_binding with a friend decl of a local class.  */
  b = innermost_nonclass_level ();

  if (lookup_name_current_level (id))
    {
      /* Supplement the existing binding.  */
      if (!supplement_binding (IDENTIFIER_BINDING (id), decl))
	/* It didn't work.  Something else must be bound at this
	   level.  Do not add DECL to the list of things to pop
	   later.  */
	return;
    }
  else
    /* Create a new binding.  */
    push_binding (id, decl, b);

  if (TREE_CODE (decl) == OVERLOAD || (flags & PUSH_USING))
    /* We must put the OVERLOAD into a TREE_LIST since the
       TREE_CHAIN of an OVERLOAD is already used.  Similarly for
       decls that got here through a using-declaration.  */
    decl = build_tree_list (NULL_TREE, decl);

  /* And put DECL on the list of things declared by the current
     binding level.  */
  add_decl_to_level (decl, b);
}

/* true means unconditionally make a BLOCK for the next level pushed.  */

static bool keep_next_level_flag;

static int binding_depth = 0;
static int is_class_level = 0;

static void
indent (int depth)
{
  int i;

  for (i = 0; i < depth * 2; i++)
    putc (' ', stderr);
}

/* Return a string describing the kind of SCOPE we have.  */
static const char *
cxx_scope_descriptor (cxx_scope *scope)
{
  /* The order of this table must match the "scope_kind"
     enumerators.  */
  static const char* scope_kind_names[] = {
    "block-scope",
    "cleanup-scope",
    "try-scope",
    "catch-scope",
    "for-scope",
    "function-parameter-scope",
    "class-scope",
    "namespace-scope",
    "template-parameter-scope",
    "template-explicit-spec-scope"
  };
  const scope_kind kind = scope->explicit_spec_p
    ? sk_template_spec : scope->kind;

  return scope_kind_names[kind];
}

/* Output a debugging information about SCOPE when performning
   ACTION at LINE.  */
static void
cxx_scope_debug (cxx_scope *scope, int line, const char *action)
{
  const char *desc = cxx_scope_descriptor (scope);
  if (scope->this_entity)
    verbatim ("%s %s(%E) %p %d\n", action, desc,
              scope->this_entity, (void *) scope, line);
  else
    verbatim ("%s %s %p %d\n", action, desc, (void *) scope, line);
}

/* Return the estimated initial size of the hashtable of a NAMESPACE
   scope.  */

static inline size_t
namespace_scope_ht_size (tree ns)
{
  tree name = DECL_NAME (ns);

  return name == std_identifier
    ? NAMESPACE_STD_HT_SIZE
    : (name == global_scope_name
       ? GLOBAL_SCOPE_HT_SIZE
       : NAMESPACE_ORDINARY_HT_SIZE);
}

/* A chain of binding_level structures awaiting reuse.  */

static GTY((deletable (""))) struct cp_binding_level *free_binding_level;

/* Create a new KIND scope and make it the top of the active scopes stack.
   ENTITY is the scope of the associated C++ entity (namespace, class,
   function); it is NULL otherwise.  */

cxx_scope *
begin_scope (scope_kind kind, tree entity)
{
  cxx_scope *scope;
  
  /* Reuse or create a struct for this binding level.  */
  if (!ENABLE_SCOPE_CHECKING && free_binding_level)
    {
      scope = free_binding_level;
      free_binding_level = scope->level_chain;
    }
  else
    scope = ggc_alloc (sizeof (cxx_scope));
  memset (scope, 0, sizeof (cxx_scope));

  scope->this_entity = entity;
  scope->more_cleanups_ok = true;
  switch (kind)
    {
    case sk_cleanup:
      scope->keep = true;
      break;
      
    case sk_template_spec:
      scope->explicit_spec_p = true;
      kind = sk_template_parms;
      /* fall through */
    case sk_template_parms:
    case sk_block:
    case sk_try:
    case sk_catch:
    case sk_for:
    case sk_class:
    case sk_function_parms:
      scope->keep = keep_next_level_flag;
      break;

    case sk_namespace:
      scope->type_decls = binding_table_new (namespace_scope_ht_size (entity));
      NAMESPACE_LEVEL (entity) = scope;
      VARRAY_TREE_INIT (scope->static_decls,
                        DECL_NAME (entity) == std_identifier
                        || DECL_NAME (entity) == global_scope_name
                        ? 200 : 10,
                        "Static declarations");
      break;

    default:
      /* Should not happen.  */
      my_friendly_assert (false, 20030922);
      break;
    }
  scope->kind = kind;

  /* Add it to the front of currently active scopes stack.  */
  scope->level_chain = current_binding_level;
  current_binding_level = scope;
  keep_next_level_flag = false;

  if (ENABLE_SCOPE_CHECKING)
    {
      scope->binding_depth = binding_depth;
      indent (binding_depth);
      cxx_scope_debug (scope, input_location.line, "push");
      is_class_level = 0;
      binding_depth++;
    }

  return scope;
}

/* We're about to leave current scope.  Pop the top of the stack of
   currently active scopes.  Return the enclosing scope, now active.  */

cxx_scope *
leave_scope (void)
{
  cxx_scope *scope = current_binding_level;

  if (scope->kind == sk_namespace && class_binding_level)
    current_binding_level = class_binding_level;

  /* We cannot leave a scope, if there are none left.  */
  if (NAMESPACE_LEVEL (global_namespace))
    my_friendly_assert (!global_scope_p (scope), 20030527);
  
  if (ENABLE_SCOPE_CHECKING)
    {
      indent (--binding_depth);
      cxx_scope_debug (scope, input_location.line, "leave");
      if (is_class_level != (scope == class_binding_level))
        {
          indent (binding_depth);
          verbatim ("XXX is_class_level != (current_scope == class_scope)\n");
        }
      is_class_level = 0;
    }

  /* Move one nesting level up.  */
  current_binding_level = scope->level_chain;

  /* Namespace-scopes are left most probably temporarily, not completely;
     they can be reopen later, e.g. in namespace-extension or any name
     binding acttivity that requires us to resume a namespace.  For other
     scopes, we just make the structure available for reuse.  */
  if (scope->kind != sk_namespace)
    {
      scope->level_chain = free_binding_level;
      if (scope->kind == sk_class)
        scope->type_decls = NULL;
      else
        binding_table_free (scope->type_decls);
      my_friendly_assert (!ENABLE_SCOPE_CHECKING
                          || scope->binding_depth == binding_depth,
                          20030529);
      free_binding_level = scope;
    }

  /* Find the innermost enclosing class scope, and reset
     CLASS_BINDING_LEVEL appropriately.  */
  for (scope = current_binding_level;
       scope && scope->kind != sk_class;
       scope = scope->level_chain)
    ;
  class_binding_level = scope && scope->kind == sk_class ? scope : NULL;

  return current_binding_level;
}

static void
resume_scope (struct cp_binding_level* b)
{
  /* Resuming binding levels is meant only for namespaces,
     and those cannot nest into classes.  */
  my_friendly_assert(!class_binding_level, 386);
  /* Also, resuming a non-directly nested namespace is a no-no.  */
  my_friendly_assert(b->level_chain == current_binding_level, 386);
  current_binding_level = b;
  if (ENABLE_SCOPE_CHECKING)
    {
      b->binding_depth = binding_depth;
      indent (binding_depth);
      cxx_scope_debug (b, input_location.line, "resume");
      is_class_level = 0;
      binding_depth++;
    }
}

/* Return the innermost binding level that is not for a class scope.  */

static cxx_scope *
innermost_nonclass_level (void)
{
  cxx_scope *b;

  b = current_binding_level;
  while (b->kind == sk_class)
    b = b->level_chain;

  return b;
}

/* We're defining an object of type TYPE.  If it needs a cleanup, but
   we're not allowed to add any more objects with cleanups to the current
   scope, create a new binding level.  */

void
maybe_push_cleanup_level (tree type)
{
  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
      && current_binding_level->more_cleanups_ok == 0)
    {
      begin_scope (sk_cleanup, NULL);
      clear_last_expr ();
      add_scope_stmt (/*begin_p=*/1, /*partial_p=*/1);
    }
}

/* Nonzero if we are currently in the global binding level.  */

int
global_bindings_p (void)
{
  return global_scope_p (current_binding_level);
}

/* True if we are currently in a toplevel binding level.  This
   means either the global binding level or a namespace in a toplevel
   binding level.  Since there are no non-toplevel namespace levels,
   this really means any namespace or template parameter level.  We
   also include a class whose context is toplevel.  */

bool
toplevel_bindings_p (void)
{
  struct cp_binding_level *b = innermost_nonclass_level ();

  return b->kind == sk_namespace || b->kind == sk_template_parms;
}

/* True if this is a namespace scope, or if we are defining a class
   which is itself at namespace scope, or whose enclosing class is
   such a class, etc.  */

bool
namespace_bindings_p (void)
{
  struct cp_binding_level *b = innermost_nonclass_level ();

  return b->kind == sk_namespace;
}

/* True if the current level needs to have a BLOCK made.  */

bool
kept_level_p (void)
{
  return (current_binding_level->blocks != NULL_TREE
	  || current_binding_level->keep
          || current_binding_level->kind == sk_cleanup
	  || current_binding_level->names != NULL_TREE
	  || current_binding_level->type_decls != NULL);
}

/* Returns the kind of the innermost scope.  */

scope_kind
innermost_scope_kind (void)
{
  return current_binding_level->kind;
}

/* Returns true if this scope was created to store template parameters.  */

bool
template_parm_scope_p (void)
{
  return innermost_scope_kind () == sk_template_parms;
}

/* If KEEP is true, make a BLOCK node for the next binding level,
   unconditionally.  Otherwise, use the normal logic to decide whether
   or not to create a BLOCK.  */

void
keep_next_level (bool keep)
{
  keep_next_level_flag = keep;
}

/* Return the list of declarations of the current level.
   Note that this list is in reverse order unless/until
   you nreverse it; and when you do nreverse it, you must
   store the result back using `storedecls' or you will lose.  */

tree
getdecls (void)
{
  return current_binding_level->names;
}

/* Set the current binding TABLE for type declarations..  This is a
   temporary workaround of the fact that the data structure classtypes
   does not currently carry its allocated cxx_scope structure.  */
void
cxx_remember_type_decls (binding_table table)
{
  current_binding_level->type_decls = table;
}

/* For debugging.  */
static int no_print_functions = 0;
static int no_print_builtins = 0;

/* Called from print_binding_level through binding_table_foreach to
   print the content of binding ENTRY.  DATA is a pointer to line offset
   marker.  */
static void
bt_print_entry (binding_entry entry, void *data)
{
  int *p = (int *) data;
  int len;

  if (entry->name == NULL)
    len = 3;
  else if (entry->name == TYPE_IDENTIFIER (entry->type))
    len = 2;
  else
    len = 4;
    len = 4;

  *p += len;

  if (*p > 5)
    {
      fprintf (stderr, "\n\t");
      *p = len;
    }
  if (entry->name == NULL)
    {
      print_node_brief (stderr, "<unnamed-typedef", entry->type, 0);
      fprintf (stderr, ">");
    }
  else if (entry->name == TYPE_IDENTIFIER (entry->type))
    print_node_brief (stderr, "", entry->type, 0);
  else
    {
      print_node_brief (stderr, "<typedef", entry->name, 0);
      print_node_brief (stderr, "", entry->type, 0);
      fprintf (stderr, ">");
    }
}

void
print_binding_level (struct cp_binding_level* lvl)
{
  tree t;
  int i = 0, len;
  fprintf (stderr, " blocks=" HOST_PTR_PRINTF, (void *) lvl->blocks);
  if (lvl->more_cleanups_ok)
    fprintf (stderr, " more-cleanups-ok");
  if (lvl->have_cleanups)
    fprintf (stderr, " have-cleanups");
  fprintf (stderr, "\n");
  if (lvl->names)
    {
      fprintf (stderr, " names:\t");
      /* We can probably fit 3 names to a line?  */
      for (t = lvl->names; t; t = TREE_CHAIN (t))
	{
	  if (no_print_functions && (TREE_CODE (t) == FUNCTION_DECL))
	    continue;
	  if (no_print_builtins
	      && (TREE_CODE (t) == TYPE_DECL)
	      && (!strcmp (DECL_SOURCE_FILE (t),"<built-in>")))
	    continue;

	  /* Function decls tend to have longer names.  */
	  if (TREE_CODE (t) == FUNCTION_DECL)
	    len = 3;
	  else
	    len = 2;
	  i += len;
	  if (i > 6)
	    {
	      fprintf (stderr, "\n\t");
	      i = len;
	    }
	  print_node_brief (stderr, "", t, 0);
	  if (t == error_mark_node)
	    break;
	}
      if (i)
        fprintf (stderr, "\n");
    }
  if (lvl->type_decls)
    {
      fprintf (stderr, " tags:\t");
      i = 0;
      binding_table_foreach (lvl->type_decls, bt_print_entry, &i);
      if (i)
	fprintf (stderr, "\n");
    }
  if (lvl->class_shadowed)
    {
      fprintf (stderr, " class-shadowed:");
      for (t = lvl->class_shadowed; t; t = TREE_CHAIN (t))
	{
	  fprintf (stderr, " %s ", IDENTIFIER_POINTER (TREE_PURPOSE (t)));
	}
      fprintf (stderr, "\n");
    }
  if (lvl->type_shadowed)
    {
      fprintf (stderr, " type-shadowed:");
      for (t = lvl->type_shadowed; t; t = TREE_CHAIN (t))
        {
	  fprintf (stderr, " %s ", IDENTIFIER_POINTER (TREE_PURPOSE (t)));
        }
      fprintf (stderr, "\n");
    }
}

void
print_other_binding_stack (struct cp_binding_level *stack)
{
  struct cp_binding_level *level;
  for (level = stack; !global_scope_p (level); level = level->level_chain)
    {
      fprintf (stderr, "binding level " HOST_PTR_PRINTF "\n", (void *) level);
      print_binding_level (level);
    }
}

void
print_binding_stack (void)
{
  struct cp_binding_level *b;
  fprintf (stderr, "current_binding_level=" HOST_PTR_PRINTF
	   "\nclass_binding_level=" HOST_PTR_PRINTF
	   "\nNAMESPACE_LEVEL (global_namespace)=" HOST_PTR_PRINTF "\n",
	   (void *) current_binding_level, (void *) class_binding_level,
           (void *) NAMESPACE_LEVEL (global_namespace));
  if (class_binding_level)
    {
      for (b = class_binding_level; b; b = b->level_chain)
	if (b == current_binding_level)
	  break;
      if (b)
	b = class_binding_level;
      else
	b = current_binding_level;
    }
  else
    b = current_binding_level;
  print_other_binding_stack (b);
  fprintf (stderr, "global:\n");
  print_binding_level (NAMESPACE_LEVEL (global_namespace));
}

/* Return the type associated with id.  */

tree
identifier_type_value (tree id)
{
  timevar_push (TV_NAME_LOOKUP);
  /* There is no type with that name, anywhere.  */
  if (REAL_IDENTIFIER_TYPE_VALUE (id) == NULL_TREE)
    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
  /* This is not the type marker, but the real thing.  */
  if (REAL_IDENTIFIER_TYPE_VALUE (id) != global_type_node)
    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, REAL_IDENTIFIER_TYPE_VALUE (id));
  /* Have to search for it. It must be on the global level, now.
     Ask lookup_name not to return non-types.  */
  id = lookup_name_real (id, 2, 1, 0, LOOKUP_COMPLAIN);
  if (id)
    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, TREE_TYPE (id));
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
}

/* Return the IDENTIFIER_GLOBAL_VALUE of T, for use in common code, since
   the definition of IDENTIFIER_GLOBAL_VALUE is different for C and C++.  */

tree
identifier_global_value	(tree t)
{
  return IDENTIFIER_GLOBAL_VALUE (t);
}

/* Push a definition of struct, union or enum tag named ID.  into
   binding_level B.  DECL is a TYPE_DECL for the type.  We assume that
   the tag ID is not already defined.  */

static void
set_identifier_type_value_with_scope (tree id, tree decl, cxx_scope *b)
{
  tree type;

  if (b->kind != sk_namespace)
    {
      /* Shadow the marker, not the real thing, so that the marker
	 gets restored later.  */
      tree old_type_value = REAL_IDENTIFIER_TYPE_VALUE (id);
      b->type_shadowed
	= tree_cons (id, old_type_value, b->type_shadowed);
      type = decl ? TREE_TYPE (decl) : NULL_TREE;
    }
  else
    {
      cxx_binding *binding =
	binding_for_name (NAMESPACE_LEVEL (current_namespace), id);
      if (decl)
	{
	  if (binding->value)
	    supplement_binding (binding, decl);
	  else
	    binding->value = decl;
	}
      else
	abort ();
      /* Store marker instead of real type.  */
      type = global_type_node;
    }
  SET_IDENTIFIER_TYPE_VALUE (id, type);
}

/* As set_identifier_type_value_with_scope, but using
   current_binding_level.  */

void
set_identifier_type_value (tree id, tree decl)
{
  set_identifier_type_value_with_scope (id, decl, current_binding_level);
}

/* Return (from the stack of) the BINDING, if any, establihsed at SCOPE.  */ 

static inline cxx_binding *
find_binding (cxx_scope *scope, cxx_binding *binding)
{
  timevar_push (TV_NAME_LOOKUP);

  for (; binding != NULL; binding = binding->previous)
    if (binding->scope == scope)
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
      if (scope == b->scope && b->previous == NULL)
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
  result->scope = scope;
  result->is_local = false;
  result->value_is_inherited = false;
  IDENTIFIER_NAMESPACE_BINDINGS (name) = result;
  return result;
}

/* Same as pushdecl, but define X in binding-level LEVEL.  We rely on the
   caller to set DECL_CONTEXT properly.  */

tree
pushdecl_with_scope (tree x, cxx_scope *level)
{
  register struct cp_binding_level *b;
  tree function_decl = current_function_decl;

  timevar_push (TV_NAME_LOOKUP);
  current_function_decl = NULL_TREE;
  if (level->kind == sk_class)
    {
      b = class_binding_level;
      class_binding_level = level;
      pushdecl_class_level (x);
      class_binding_level = b;
    }
  else
    {
      b = current_binding_level;
      current_binding_level = level;
      x = pushdecl (x);
      current_binding_level = b;
    }
  current_function_decl = function_decl;
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, x);
}

/* Return the type that should be used when TYPE's name is preceded
   by a tag such as 'struct' or 'union', or null if the name cannot
   be used in this way.

   For example, when processing the third line of:

	struct A;
	typedef struct A A;
	struct A;

   lookup of A will find the typedef.  Given A's typedef, this function
   will return the type associated with "struct A".  For the tag to be
   anything other than TYPE, TYPE must be a typedef whose original type
   has the same name and context as TYPE itself.

   It is not valid for a typedef of an anonymous type to be used with
   an explicit tag:

       typedef struct { ... } B;
       struct B;

   Return null for this case.  */

static tree
follow_tag_typedef (tree type)
{
  tree original;

  original = original_type (type);
  if (! TYPE_NAME (original))
    return NULL_TREE;
  if (TYPE_IDENTIFIER (original) == TYPE_IDENTIFIER (type)
      && (CP_DECL_CONTEXT (TYPE_NAME (original))
	  == CP_DECL_CONTEXT (TYPE_NAME (type)))
      && !(CLASS_TYPE_P (original) && TYPE_WAS_ANONYMOUS (original)))
    return original;
  else
    return NULL_TREE;
}

/* Given NAME, an IDENTIFIER_NODE,
   return the structure (or union or enum) definition for that name.
   Searches binding levels from its SCOPE up to the global level.
   If THISLEVEL_ONLY is nonzero, searches only the specified context
   (but skips any sk_cleanup contexts to find one that is
   meaningful for tags).
   FORM says which kind of type the caller wants;
   it is RECORD_TYPE or UNION_TYPE or ENUMERAL_TYPE.
   If the wrong kind of type is found, and it's not a template, an error is
   reported.  */

tree
lookup_tag (enum tree_code form, tree name,
            cxx_scope *binding_level, int thislevel_only)
{
  register struct cp_binding_level *level;
  /* Nonzero if, we should look past a template parameter level, even
     if THISLEVEL_ONLY.  */
  int allow_template_parms_p = 1;
  bool type_is_anonymous = ANON_AGGRNAME_P (name);

  timevar_push (TV_NAME_LOOKUP);
  for (level = binding_level; level; level = level->level_chain)
    {
      register tree tail;
      if (type_is_anonymous && level->type_decls != NULL)
        {
          tree type = binding_table_find_anon_type (level->type_decls, name);
          /* There is no need for error checking here, because
           anon names are unique throughout the compilation.  */
          if (type != NULL)
            POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, type);
        }
      else if (level->kind == sk_namespace)
	/* Do namespace lookup.  */
	for (tail = current_namespace; 1; tail = CP_DECL_CONTEXT (tail))
	  {
            cxx_binding *binding =
              cxx_scope_find_binding_for_name (NAMESPACE_LEVEL (tail), name);
	    tree old;

	    /* If we just skipped past a template parameter level,
	       even though THISLEVEL_ONLY, and we find a template
	       class declaration, then we use the _TYPE node for the
	       template.  See the example below.  */
	    if (thislevel_only && !allow_template_parms_p
		&& binding && binding->value
		&& DECL_CLASS_TEMPLATE_P (binding->value))
	      old = binding->value;
	    else if (binding)
	      old = select_decl (binding, LOOKUP_PREFER_TYPES);
            else
              old = NULL_TREE;

	    if (old)
	      {
		/* We've found something at this binding level.  If it is
		   a typedef, extract the tag it refers to.  Lookup fails
		   if the typedef doesn't refer to a taggable type.  */
		old = TREE_TYPE (old);
		old = follow_tag_typedef (old);
		if (!old)
		  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
		if (TREE_CODE (old) != form
		    && (form == ENUMERAL_TYPE
			|| TREE_CODE (old) == ENUMERAL_TYPE))
		  {
		    error ("`%#D' redeclared as %C", old, form);
		    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
		  }
		POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, old);
	      }
	    if (thislevel_only || tail == global_namespace)
	      POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
	  }
      else if (level->type_decls != NULL)
        {
          binding_entry entry = binding_table_find (level->type_decls, name);
          if (entry != NULL)
            {
              enum tree_code code = TREE_CODE (entry->type);
		
              if (code != form
                  && (form == ENUMERAL_TYPE || code == ENUMERAL_TYPE))
                {
                  /* Definition isn't the kind we were looking for.  */
                  error ("`%#D' redeclared as %C", entry->type, form);
                  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
                }
              POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, entry->type);
            }
	  }
      if (thislevel_only && level->kind != sk_cleanup)
	{
	  if (level->kind == sk_template_parms && allow_template_parms_p)
	    {
	      /* We must deal with cases like this:

	           template <class T> struct S;
		   template <class T> struct S {};

		 When looking up `S', for the second declaration, we
		 would like to find the first declaration.  But, we
		 are in the pseudo-global level created for the
		 template parameters, rather than the (surrounding)
		 namespace level.  Thus, we keep going one more level,
		 even though THISLEVEL_ONLY is nonzero.  */
	      allow_template_parms_p = 0;
	      continue;
	    }
	  else
	    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
	}
    }
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
}

/* Given a type, find the tag that was defined for it and return the tag name.
   Otherwise return 0.  However, the value can never be 0
   in the cases in which this is used.

   C++: If NAME is nonzero, this is the new name to install.  This is
   done when replacing anonymous tags with real tag names.  */

tree
lookup_tag_reverse (tree type, tree name)
{
  register struct cp_binding_level *level;

  timevar_push (TV_NAME_LOOKUP);
  for (level = current_binding_level; level; level = level->level_chain)
    {
      binding_entry entry = level->type_decls == NULL
        ? NULL
        : binding_table_reverse_maybe_remap (level->type_decls, type, name);
      if (entry)
        POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, entry->name);
    }
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
}

/* Do a pushlevel for class declarations.  */

void
pushlevel_class (void)
{
  if (ENABLE_SCOPE_CHECKING)
    is_class_level = 1;

  class_binding_level = begin_scope (sk_class, current_class_type);
}

/* ...and a poplevel for class declarations.  */

void
poplevel_class (void)
{
  register struct cp_binding_level *level = class_binding_level;
  tree shadowed;

  timevar_push (TV_NAME_LOOKUP);
  my_friendly_assert (level != 0, 354);

  /* If we're leaving a toplevel class, don't bother to do the setting
     of IDENTIFIER_CLASS_VALUE to NULL_TREE, since first of all this slot
     shouldn't even be used when current_class_type isn't set, and second,
     if we don't touch it here, we're able to use the cache effect if the
     next time we're entering a class scope, it is the same class.  */
  if (current_class_depth != 1)
    {
      struct cp_binding_level* b;

      /* Clear out our IDENTIFIER_CLASS_VALUEs.  */
      for (shadowed = level->class_shadowed;
	   shadowed;
	   shadowed = TREE_CHAIN (shadowed))
	IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (shadowed)) = NULL_TREE;

      /* Find the next enclosing class, and recreate
	 IDENTIFIER_CLASS_VALUEs appropriate for that class.  */
      b = level->level_chain;
      while (b && b->kind != sk_class)
	b = b->level_chain;

      if (b)
	for (shadowed = b->class_shadowed;
	     shadowed;
	     shadowed = TREE_CHAIN (shadowed))
	  {
	    cxx_binding *binding;
            
	    binding = IDENTIFIER_BINDING (TREE_PURPOSE (shadowed));
	    while (binding && binding->scope != b)
	      binding = binding->previous;

	    if (binding)
	      IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (shadowed))
		= binding->value;
	  }
    }
  else
    /* Remember to save what IDENTIFIER's were bound in this scope so we
       can recover from cache misses.  */
    {
      previous_class_type = current_class_type;
      previous_class_values = class_binding_level->class_shadowed;
    }
  for (shadowed = level->type_shadowed;
       shadowed;
       shadowed = TREE_CHAIN (shadowed))
    SET_IDENTIFIER_TYPE_VALUE (TREE_PURPOSE (shadowed), TREE_VALUE (shadowed));

  /* Remove the bindings for all of the class-level declarations.  */
  for (shadowed = level->class_shadowed;
       shadowed;
       shadowed = TREE_CHAIN (shadowed))
    pop_binding (TREE_PURPOSE (shadowed), TREE_TYPE (shadowed));

  /* Now, pop out of the binding level which we created up in the
     `pushlevel_class' routine.  */
  if (ENABLE_SCOPE_CHECKING)
    is_class_level = 1;

  leave_scope ();
  timevar_pop (TV_NAME_LOOKUP);
}

/* Bind DECL to ID in the class_binding_level.  Returns nonzero if the
   binding was successful.  */

int
push_class_binding (tree id, tree decl)
{
  int result = 1;
  cxx_binding *binding = IDENTIFIER_BINDING (id);
  tree context;

  timevar_push (TV_NAME_LOOKUP);
  /* Note that we declared this value so that we can issue an error if
     this is an invalid redeclaration of a name already used for some
     other purpose.  */
  note_name_declared_in_class (id, decl);

  if (binding && binding->scope == class_binding_level)
    /* Supplement the existing binding.  */
    result = supplement_binding (IDENTIFIER_BINDING (id), decl);
  else
    /* Create a new binding.  */
    push_binding (id, decl, class_binding_level);

  /* Update the IDENTIFIER_CLASS_VALUE for this ID to be the
     class-level declaration.  Note that we do not use DECL here
     because of the possibility of the `struct stat' hack; if DECL is
     a class-name or enum-name we might prefer a field-name, or some
     such.  */
  IDENTIFIER_CLASS_VALUE (id) = IDENTIFIER_BINDING (id)->value;

  /* If this is a binding from a base class, mark it as such.  */
  binding = IDENTIFIER_BINDING (id);
  if (binding->value == decl && TREE_CODE (decl) != TREE_LIST)
    {
      if (TREE_CODE (decl) == OVERLOAD)
	context = CP_DECL_CONTEXT (OVL_CURRENT (decl));
      else
	{
	  my_friendly_assert (DECL_P (decl), 0);
	  context = context_for_name_lookup (decl);
	}

      if (is_properly_derived_from (current_class_type, context))
	INHERITED_VALUE_BINDING_P (binding) = 1;
      else
	INHERITED_VALUE_BINDING_P (binding) = 0;
    }
  else if (binding->value == decl)
    /* We only encounter a TREE_LIST when push_class_decls detects an
       ambiguity.  Such an ambiguity can be overridden by a definition
       in this class.  */
    INHERITED_VALUE_BINDING_P (binding) = 1;

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, result);
}

/* We are entering the scope of a class.  Clear IDENTIFIER_CLASS_VALUE
   for any names in enclosing classes.  */

void
clear_identifier_class_values (void)
{
  tree t;

  if (!class_binding_level)
    return;

  for (t = class_binding_level->class_shadowed;
       t;
       t = TREE_CHAIN (t))
    IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (t)) = NULL_TREE;
}

/* Make the declaration of X appear in CLASS scope.  */

bool
pushdecl_class_level (tree x)
{
  tree name;
  bool is_valid = true;

  timevar_push (TV_NAME_LOOKUP);
  /* Get the name of X.  */
  if (TREE_CODE (x) == OVERLOAD)
    name = DECL_NAME (get_first_fn (x));
  else
    name = DECL_NAME (x);

  if (name)
    {
      is_valid = push_class_level_binding (name, x);
      if (TREE_CODE (x) == TYPE_DECL)
	set_identifier_type_value (name, x);
    }
  else if (ANON_AGGR_TYPE_P (TREE_TYPE (x)))
    {
      /* If X is an anonymous aggregate, all of its members are
	 treated as if they were members of the class containing the
	 aggregate, for naming purposes.  */
      tree f;

      for (f = TYPE_FIELDS (TREE_TYPE (x)); f; f = TREE_CHAIN (f))
	{
	  location_t save_location = input_location;
	  input_location = DECL_SOURCE_LOCATION (f);
	  if (!pushdecl_class_level (f))
	    is_valid = false;
	  input_location = save_location;
	}
    }
  timevar_pop (TV_NAME_LOOKUP);

  return is_valid;
}

/* Make the declaration(s) of X appear in CLASS scope under the name
   NAME.  Returns true if the binding is valid.  */

bool
push_class_level_binding (tree name, tree x)
{
  cxx_binding *binding;

  timevar_push (TV_NAME_LOOKUP);
  /* The class_binding_level will be NULL if x is a template
     parameter name in a member template.  */
  if (!class_binding_level)
    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, true);

  /* Make sure that this new member does not have the same name
     as a template parameter.  */
  if (TYPE_BEING_DEFINED (current_class_type))
    check_template_shadow (x);

  /* If this declaration shadows a declaration from an enclosing
     class, then we will need to restore IDENTIFIER_CLASS_VALUE when
     we leave this class.  Record the shadowed declaration here.  */
  binding = IDENTIFIER_BINDING (name);
  if (binding && binding->value)
    {
      tree bval = binding->value;
      tree old_decl = NULL_TREE;

      if (INHERITED_VALUE_BINDING_P (binding))
	{
	  /* If the old binding was from a base class, and was for a
  	     tag name, slide it over to make room for the new binding.
  	     The old binding is still visible if explicitly qualified
  	     with a class-key.  */
	  if (TREE_CODE (bval) == TYPE_DECL && DECL_ARTIFICIAL (bval)
	      && !(TREE_CODE (x) == TYPE_DECL && DECL_ARTIFICIAL (x)))
	    {
	      old_decl = binding->type;
	      binding->type = bval;
	      binding->value = NULL_TREE;
	      INHERITED_VALUE_BINDING_P (binding) = 0;
	    }
	  else
	    old_decl = bval;
	}
      else if (TREE_CODE (x) == OVERLOAD && is_overloaded_fn (bval))
	old_decl = bval;
      else if (TREE_CODE (x) == USING_DECL && TREE_CODE (bval) == USING_DECL)
	POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, true);
      else if (TREE_CODE (x) == USING_DECL && is_overloaded_fn (bval))
	old_decl = bval;
      else if (TREE_CODE (bval) == USING_DECL && is_overloaded_fn (x))
	POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, true);
      
      if (old_decl)
	{
	  tree shadow;
	  
	  /* Find the previous binding of name on the class-shadowed
             list, and update it.  */
	  for (shadow = class_binding_level->class_shadowed;
	       shadow;
	       shadow = TREE_CHAIN (shadow))
	    if (TREE_PURPOSE (shadow) == name
		&& TREE_TYPE (shadow) == old_decl)
	      {
		binding->value = x;
		INHERITED_VALUE_BINDING_P (binding) = 0;
		TREE_TYPE (shadow) = x;
		IDENTIFIER_CLASS_VALUE (name) = x;
		POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, true);
	      }
	}
    }

  /* If we didn't replace an existing binding, put the binding on the
     stack of bindings for the identifier, and update the shadowed list.  */
  if (push_class_binding (name, x))
    {
      class_binding_level->class_shadowed
	= tree_cons (name, NULL,
		     class_binding_level->class_shadowed);
      /* Record the value we are binding NAME to so that we can know
	 what to pop later.  */
      TREE_TYPE (class_binding_level->class_shadowed) = x;
      POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, true);
    }

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, false);
}

void
set_class_shadows (tree shadows)
{
  class_binding_level->class_shadowed = shadows;
}

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
  if (!b->value || TREE_CODE (val) == OVERLOAD || val == error_mark_node)
    b->value = val;
  else
    supplement_binding (b, val);
  timevar_pop (TV_NAME_LOOKUP);
}

/* Push into the scope of the NAME namespace.  If NAME is NULL_TREE, then we
   select a name that is unique to this compilation unit.  */

void
push_namespace (tree name)
{
  tree d = NULL_TREE;
  int need_new = 1;
  int implicit_use = 0;

  timevar_push (TV_NAME_LOOKUP);
  
  /* We should not get here if the global_namespace is not yet constructed
     nor if NAME designates the global namespace:  The global scope is
     constructed elsewhere.  */
  my_friendly_assert (global_namespace != NULL && name != global_scope_name,
                      20030531);

  if (!name)
    {
      /* The name of anonymous namespace is unique for the translation
         unit.  */
      if (!anonymous_namespace_name)
        anonymous_namespace_name = get_file_function_name ('N');
      name = anonymous_namespace_name;
      d = IDENTIFIER_NAMESPACE_VALUE (name);
      if (d)
        /* Reopening anonymous namespace.  */
        need_new = 0;
      implicit_use = 1;
    }
  else
    {
      /* Check whether this is an extended namespace definition.  */
      d = IDENTIFIER_NAMESPACE_VALUE (name);
      if (d != NULL_TREE && TREE_CODE (d) == NAMESPACE_DECL)
        {
          need_new = 0;
          if (DECL_NAMESPACE_ALIAS (d))
            {
              error ("namespace alias `%D' not allowed here, assuming `%D'",
                        d, DECL_NAMESPACE_ALIAS (d));
              d = DECL_NAMESPACE_ALIAS (d);
            }
        }
    }

  if (need_new)
    {
      /* Make a new namespace, binding the name to it.  */
      d = build_lang_decl (NAMESPACE_DECL, name, void_type_node);
      DECL_CONTEXT (d) = FROB_CONTEXT (current_namespace);
      d = pushdecl (d);
      begin_scope (sk_namespace, d);
    }
  else
    resume_scope (NAMESPACE_LEVEL (d));

  if (implicit_use)
    do_using_directive (d);
  /* Enter the name space.  */
  current_namespace = d;

  timevar_pop (TV_NAME_LOOKUP);
}

/* Pop from the scope of the current namespace.  */

void
pop_namespace (void)
{
  my_friendly_assert (current_namespace != global_namespace, 20010801);
  current_namespace = CP_DECL_CONTEXT (current_namespace);
  /* The binding level is not popped, as it might be re-opened later.  */
  leave_scope ();
}

/* Push into the scope of the namespace NS, even if it is deeply
   nested within another namespace.  */

void
push_nested_namespace (tree ns)
{
  if (ns == global_namespace)
    push_to_top_level ();
  else
    {
      push_nested_namespace (CP_DECL_CONTEXT (ns));
      push_namespace (DECL_NAME (ns));
    }
}

/* Pop back from the scope of the namespace NS, which was previously
   entered with push_nested_namespace.  */

void
pop_nested_namespace (tree ns)
{
  timevar_push (TV_NAME_LOOKUP);
  while (ns != global_namespace)
    {
      pop_namespace ();
      ns = CP_DECL_CONTEXT (ns);
    }

  pop_from_top_level ();
  timevar_pop (TV_NAME_LOOKUP);
}

/* Like pushdecl, only it places X in the current namespace,
   if appropriate.  */

tree
pushdecl_namespace_level (tree x)
{
  register struct cp_binding_level *b = current_binding_level;
  register tree t;

  timevar_push (TV_NAME_LOOKUP);
  t = pushdecl_with_scope (x, NAMESPACE_LEVEL (current_namespace));

  /* Now, the type_shadowed stack may screw us.  Munge it so it does
     what we want.  */
  if (TREE_CODE (x) == TYPE_DECL)
    {
      tree name = DECL_NAME (x);
      tree newval;
      tree *ptr = (tree *)0;
      for (; !global_scope_p (b); b = b->level_chain)
        {
          tree shadowed = b->type_shadowed;
          for (; shadowed; shadowed = TREE_CHAIN (shadowed))
            if (TREE_PURPOSE (shadowed) == name)
              {
		ptr = &TREE_VALUE (shadowed);
		/* Can't break out of the loop here because sometimes
		   a binding level will have duplicate bindings for
		   PT names.  It's gross, but I haven't time to fix it.  */
              }
        }
      newval = TREE_TYPE (x);
      if (ptr == (tree *)0)
        {
          /* @@ This shouldn't be needed.  My test case "zstring.cc" trips
             up here if this is changed to an assertion.  --KR  */
	  SET_IDENTIFIER_TYPE_VALUE (name, x);
	}
      else
        {
	  *ptr = newval;
        }
    }
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, t);
}

/* Return the declarations that are members of the namespace NS.  */

tree
cp_namespace_decls (tree ns)
{
  return NAMESPACE_LEVEL (ns)->names;
}

/* Combine prefer_type and namespaces_only into flags.  */

static int
lookup_flags (int prefer_type, int namespaces_only)
{
  if (namespaces_only)
    return LOOKUP_PREFER_NAMESPACES;
  if (prefer_type > 1)
    return LOOKUP_PREFER_TYPES;
  if (prefer_type > 0)
    return LOOKUP_PREFER_BOTH;
  return 0;
}

/* Given a lookup that returned VAL, use FLAGS to decide if we want to
   ignore it or not.  Subroutine of lookup_name_real.  */

static tree
qualify_lookup (tree val, int flags)
{
  if (val == NULL_TREE)
    return val;
  if ((flags & LOOKUP_PREFER_NAMESPACES) && TREE_CODE (val) == NAMESPACE_DECL)
    return val;
  if ((flags & LOOKUP_PREFER_TYPES)
      && (TREE_CODE (val) == TYPE_DECL || TREE_CODE (val) == TEMPLATE_DECL))
    return val;
  if (flags & (LOOKUP_PREFER_NAMESPACES | LOOKUP_PREFER_TYPES))
    return NULL_TREE;
  return val;
}

/* Look up NAME in the NAMESPACE.  */

tree
lookup_namespace_name (tree namespace, tree name)
{
  tree val;
  tree template_id = NULL_TREE;
  cxx_binding binding;

  timevar_push (TV_NAME_LOOKUP);
  my_friendly_assert (TREE_CODE (namespace) == NAMESPACE_DECL, 370);

  if (TREE_CODE (name) == NAMESPACE_DECL)
    /* This happens for A::B<int> when B is a namespace.  */
    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, name);
  else if (TREE_CODE (name) == TEMPLATE_DECL)
    {
      /* This happens for A::B where B is a template, and there are no
	 template arguments.  */
      error ("invalid use of `%D'", name);
      POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, error_mark_node);
    }

  namespace = ORIGINAL_NAMESPACE (namespace);

  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    {
      template_id = name;
      name = TREE_OPERAND (name, 0);
      if (TREE_CODE (name) == OVERLOAD)
	name = DECL_NAME (OVL_CURRENT (name));
      else if (DECL_P (name))
	name = DECL_NAME (name);
    }

  my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 373);

  cxx_binding_clear (&binding);
  if (!qualified_lookup_using_namespace (name, namespace, &binding, 0))
    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, error_mark_node);

  if (binding.value)
    {
      val = binding.value;

      if (template_id)
	{
	  if (DECL_CLASS_TEMPLATE_P (val))
	    val = lookup_template_class (val,
					 TREE_OPERAND (template_id, 1),
					 /*in_decl=*/NULL_TREE,
					 /*context=*/NULL_TREE,
					 /*entering_scope=*/0,
	                                 tf_error | tf_warning);
	  else if (DECL_FUNCTION_TEMPLATE_P (val)
		   || TREE_CODE (val) == OVERLOAD)
	    val = lookup_template_function (val,
					    TREE_OPERAND (template_id, 1));
	  else
	    {
	      error ("`%D::%D' is not a template",
			namespace, name);
	      POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, error_mark_node);
	    }
	}

      /* If we have a single function from a using decl, pull it out.  */
      if (TREE_CODE (val) == OVERLOAD && ! really_overloaded_fn (val))
	val = OVL_FUNCTION (val);

      /* Ignore built-in functions that haven't been prototyped yet.  */
      if (!val || !DECL_P(val)
          || !DECL_LANG_SPECIFIC(val)
          || !DECL_ANTICIPATED (val))
        POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, val);
    }

  error ("`%D' undeclared in namespace `%D'", name, namespace);
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, error_mark_node);
}

/* Select the right _DECL from multiple choices.  */

static tree
select_decl (cxx_binding *binding, int flags)
{
  tree val;
  val = binding->value;

  timevar_push (TV_NAME_LOOKUP);
  if (LOOKUP_NAMESPACES_ONLY (flags))
    {
      /* We are not interested in types.  */
      if (val && TREE_CODE (val) == NAMESPACE_DECL)
        POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, val);
      POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
    }

  /* If looking for a type, or if there is no non-type binding, select
     the value binding.  */
  if (binding->type && (!val || (flags & LOOKUP_PREFER_TYPES)))
    val = binding->type;
  /* Don't return non-types if we really prefer types.  */
  else if (val && LOOKUP_TYPES_ONLY (flags)  && TREE_CODE (val) != TYPE_DECL
	   && (TREE_CODE (val) != TEMPLATE_DECL
	       || !DECL_CLASS_TEMPLATE_P (val)))
    val = NULL_TREE;

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, val);
}

/* Unscoped lookup of a global: iterate over current namespaces,
   considering using-directives.  If SPACESP is non-NULL, store a list
   of the namespaces we've considered in it.  */

tree
unqualified_namespace_lookup (tree name, int flags, tree* spacesp)
{
  tree initial = current_decl_namespace ();
  tree scope = initial;
  tree siter;
  struct cp_binding_level *level;
  tree val = NULL_TREE;
  cxx_binding binding;

  timevar_push (TV_NAME_LOOKUP);
  cxx_binding_clear (&binding);
  if (spacesp)
    *spacesp = NULL_TREE;

  for (; !val; scope = CP_DECL_CONTEXT (scope))
    {
      cxx_binding *b =
         cxx_scope_find_binding_for_name (NAMESPACE_LEVEL (scope), name);
      if (spacesp)
	*spacesp = tree_cons (scope, NULL_TREE, *spacesp);

      /* Ignore anticipated built-in functions.  */
      if (b && b->value && DECL_P (b->value)
          && DECL_LANG_SPECIFIC (b->value) && DECL_ANTICIPATED (b->value))
        /* Keep binding cleared.  */;
      else if (b)
        {
          /* Initialize binding for this context.  */
          binding.value = b->value;
          binding.type = b->type;
        }

      /* Add all _DECLs seen through local using-directives.  */
      for (level = current_binding_level;
	   level->kind != sk_namespace;
	   level = level->level_chain)
	if (!lookup_using_namespace (name, &binding, level->using_directives,
                                     scope, flags, spacesp))
	  /* Give up because of error.  */
	  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, error_mark_node);

      /* Add all _DECLs seen through global using-directives.  */
      /* XXX local and global using lists should work equally.  */
      siter = initial;
      while (1)
	{
	  if (!lookup_using_namespace (name, &binding,
                                       DECL_NAMESPACE_USING (siter),
				       scope, flags, spacesp))
	    /* Give up because of error.  */
	    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, error_mark_node);
	  if (siter == scope) break;
	  siter = CP_DECL_CONTEXT (siter);
	}

      val = select_decl (&binding, flags);
      if (scope == global_namespace)
	break;
    }
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, val);
}

/* Look up NAME (an IDENTIFIER_NODE) in SCOPE (either a NAMESPACE_DECL
   or a class TYPE).  If IS_TYPE_P is TRUE, then ignore non-type
   bindings.  

   Returns a DECL (or OVERLOAD, or BASELINK) representing the
   declaration found.  If no suitable declaration can be found,
   ERROR_MARK_NODE is returned.  Iif COMPLAIN is true and SCOPE is
   neither a class-type nor a namespace a diagnostic is issued.  */

tree
lookup_qualified_name (tree scope, tree name, bool is_type_p, bool complain)
{
  int flags = 0;

  if (TREE_CODE (scope) == NAMESPACE_DECL)
    {
      cxx_binding binding;

      cxx_binding_clear (&binding);
      flags |= LOOKUP_COMPLAIN;
      if (is_type_p)
	flags |= LOOKUP_PREFER_TYPES;
      if (qualified_lookup_using_namespace (name, scope, &binding, 
					    flags))
	return select_decl (&binding, flags);
    }
  else if (is_aggr_type (scope, complain))
    {
      tree t;
      t = lookup_member (scope, name, 0, is_type_p);
      if (t)
	return t;
    }

  return error_mark_node;
}

/* Look up NAME in the current binding level and its superiors in the
   namespace of variables, functions and typedefs.  Return a ..._DECL
   node of some kind representing its definition if there is only one
   such declaration, or return a TREE_LIST with all the overloaded
   definitions if there are many, or return 0 if it is undefined.

   If PREFER_TYPE is > 0, we prefer TYPE_DECLs or namespaces.
   If PREFER_TYPE is > 1, we reject non-type decls (e.g. namespaces).
   Otherwise we prefer non-TYPE_DECLs.

   If NONCLASS is nonzero, we don't look for the NAME in class scope,
   using IDENTIFIER_CLASS_VALUE.  */

tree
lookup_name_real (tree name, int prefer_type, int nonclass, 
		  int namespaces_only, int flags)
{
  cxx_binding *iter;
  tree val = NULL_TREE;

  timevar_push (TV_NAME_LOOKUP);
  /* Conversion operators are handled specially because ordinary
     unqualified name lookup will not find template conversion
     operators.  */
  if (IDENTIFIER_TYPENAME_P (name)) 
    {
      struct cp_binding_level *level;

      for (level = current_binding_level; 
	   level && level->kind != sk_namespace;
	   level = level->level_chain)
	{
	  tree class_type;
	  tree operators;
	  
	  /* A conversion operator can only be declared in a class 
	     scope.  */
	  if (level->kind != sk_class)
	    continue;
	  
	  /* Lookup the conversion operator in the class.  */
	  class_type = level->this_entity;
	  operators = lookup_fnfields (class_type, name, /*protect=*/0);
	  if (operators)
	    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, operators);
	}

      POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);
    }

  flags |= lookup_flags (prefer_type, namespaces_only);

  /* First, look in non-namespace scopes.  */

  if (current_class_type == NULL_TREE)
    nonclass = 1;

  for (iter = IDENTIFIER_BINDING (name); iter; iter = iter->previous)
    {
      tree binding;

      if (!LOCAL_BINDING_P (iter) && nonclass)
	/* We're not looking for class-scoped bindings, so keep going.  */
	continue;

      /* If this is the kind of thing we're looking for, we're done.  */
      if (qualify_lookup (iter->value, flags))
	binding = iter->value;
      else if ((flags & LOOKUP_PREFER_TYPES)
	       && qualify_lookup (iter->type, flags))
	binding = iter->type;
      else
	binding = NULL_TREE;

      if (binding)
	{
	  val = binding;
	  break;
	}
    }

  /* Now lookup in namespace scopes.  */
  if (!val)
    {
      tree t = unqualified_namespace_lookup (name, flags, 0);
      if (t)
	val = t;
    }

  if (val)
    {
      /* If we have a single function from a using decl, pull it out.  */
      if (TREE_CODE (val) == OVERLOAD && ! really_overloaded_fn (val))
	val = OVL_FUNCTION (val);
    }

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, val);
}

tree
lookup_name_nonclass (tree name)
{
  return lookup_name_real (name, 0, 1, 0, LOOKUP_COMPLAIN);
}

tree
lookup_function_nonclass (tree name, tree args)
{
  return lookup_arg_dependent (name, lookup_name_nonclass (name), args);
}

tree
lookup_name (tree name, int prefer_type)
{
  return lookup_name_real (name, prefer_type, 0, 0, LOOKUP_COMPLAIN);
}

/* Similar to `lookup_name' but look only in the innermost non-class
   binding level.  */

tree
lookup_name_current_level (tree name)
{
  struct cp_binding_level *b;
  tree t = NULL_TREE;

  timevar_push (TV_NAME_LOOKUP);
  b = innermost_nonclass_level ();

  if (b->kind == sk_namespace)
    {
      t = IDENTIFIER_NAMESPACE_VALUE (name);

      /* extern "C" function() */
      if (t != NULL_TREE && TREE_CODE (t) == TREE_LIST)
	t = TREE_VALUE (t);
    }
  else if (IDENTIFIER_BINDING (name)
	   && LOCAL_BINDING_P (IDENTIFIER_BINDING (name)))
    {
      while (1)
	{
	  if (IDENTIFIER_BINDING (name)->scope == b)
	    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, IDENTIFIER_VALUE (name));

	  if (b->kind == sk_cleanup)
	    b = b->level_chain;
	  else
	    break;
	}
    }

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, t);
}

/* Like lookup_name_current_level, but for types.  */

tree
lookup_type_current_level (tree name)
{
  register tree t = NULL_TREE;

  timevar_push (TV_NAME_LOOKUP);
  my_friendly_assert (current_binding_level->kind != sk_namespace, 
		      980716);

  if (REAL_IDENTIFIER_TYPE_VALUE (name) != NULL_TREE
      && REAL_IDENTIFIER_TYPE_VALUE (name) != global_type_node)
    {
      struct cp_binding_level *b = current_binding_level;
      while (1)
	{
	  if (purpose_member (name, b->type_shadowed))
	    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP,
                                    REAL_IDENTIFIER_TYPE_VALUE (name));
	  if (b->kind == sk_cleanup)
	    b = b->level_chain;
	  else
	    break;
	}
    }

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, t);
}

/* Add namespace to using_directives. Return NULL_TREE if nothing was
   changed (i.e. there was already a directive), or the fresh
   TREE_LIST otherwise.  */

tree
push_using_directive (tree used)
{
  tree ud = current_binding_level->using_directives;
  tree iter, ancestor;

  timevar_push (TV_NAME_LOOKUP);
  /* Check if we already have this.  */
  if (purpose_member (used, ud) != NULL_TREE)
    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, NULL_TREE);

  ancestor = namespace_ancestor (current_decl_namespace (), used);
  ud = current_binding_level->using_directives;
  ud = tree_cons (used, ancestor, ud);
  current_binding_level->using_directives = ud;

  /* Recursively add all namespaces used.  */
  for (iter = DECL_NAMESPACE_USING (used); iter; iter = TREE_CHAIN (iter))
    push_using_directive (TREE_PURPOSE (iter));

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, ud);
}

/* The type TYPE is being declared.  If it is a class template, or a
   specialization of a class template, do any processing required and
   perform error-checking.  If IS_FRIEND is nonzero, this TYPE is
   being declared a friend.  B is the binding level at which this TYPE
   should be bound.

   Returns the TYPE_DECL for TYPE, which may have been altered by this
   processing.  */

static tree
maybe_process_template_type_declaration (tree type, int globalize,
                                         cxx_scope *b)
{
  tree decl = TYPE_NAME (type);

  if (processing_template_parmlist)
    /* You can't declare a new template type in a template parameter
       list.  But, you can declare a non-template type:

         template <class A*> struct S;

       is a forward-declaration of `A'.  */
    ;
  else
    {
      maybe_check_template_type (type);

      my_friendly_assert (IS_AGGR_TYPE (type)
			  || TREE_CODE (type) == ENUMERAL_TYPE, 0);


      if (processing_template_decl)
	{
	  /* This may change after the call to
	     push_template_decl_real, but we want the original value.  */
	  tree name = DECL_NAME (decl);

	  decl = push_template_decl_real (decl, globalize);
	  /* If the current binding level is the binding level for the
	     template parameters (see the comment in
	     begin_template_parm_list) and the enclosing level is a class
	     scope, and we're not looking at a friend, push the
	     declaration of the member class into the class scope.  In the
	     friend case, push_template_decl will already have put the
	     friend into global scope, if appropriate.  */
	  if (TREE_CODE (type) != ENUMERAL_TYPE
	      && !globalize && b->kind == sk_template_parms
	      && b->level_chain->kind == sk_class)
	    {
	      finish_member_declaration (CLASSTYPE_TI_TEMPLATE (type));
	      /* Put this UDT in the table of UDTs for the class, since
		 that won't happen below because B is not the class
		 binding level, but is instead the pseudo-global level.  */
              if (b->level_chain->type_decls == NULL)
                b->level_chain->type_decls =
                  binding_table_new (SCOPE_DEFAULT_HT_SIZE);
              binding_table_insert (b->level_chain->type_decls, name, type);
	      if (!COMPLETE_TYPE_P (current_class_type))
		{
		  maybe_add_class_template_decl_list (current_class_type,
						      type, /*friend_p=*/0);
		  CLASSTYPE_NESTED_UTDS (current_class_type) =
                    b->level_chain->type_decls;
		}
	    }
	}
    }

  return decl;
}

/* Push a tag name NAME for struct/class/union/enum type TYPE.
   Normally put it into the inner-most non-sk_cleanup scope,
   but if GLOBALIZE is true, put it in the inner-most non-class scope.
   The latter is needed for implicit declarations.  */

void
pushtag (tree name, tree type, int globalize)
{
  register struct cp_binding_level *b;

  timevar_push (TV_NAME_LOOKUP);
  b = current_binding_level;
  while (b->kind == sk_cleanup
	 || (b->kind == sk_class
	     && (globalize
		 /* We may be defining a new type in the initializer
		    of a static member variable. We allow this when
		    not pedantic, and it is particularly useful for
		    type punning via an anonymous union.  */
		 || COMPLETE_TYPE_P (b->this_entity))))
    b = b->level_chain;

  if (b->type_decls == NULL)
    b->type_decls = binding_table_new (SCOPE_DEFAULT_HT_SIZE);
  binding_table_insert (b->type_decls, name, type);

  if (name)
    {
      /* Do C++ gratuitous typedefing.  */
      if (IDENTIFIER_TYPE_VALUE (name) != type)
        {
          register tree d = NULL_TREE;
	  int in_class = 0;
	  tree context = TYPE_CONTEXT (type);

	  if (! context)
	    {
	      tree cs = current_scope ();

	      if (! globalize)
		context = cs;
	      else if (cs != NULL_TREE && TYPE_P (cs))
		/* When declaring a friend class of a local class, we want
		   to inject the newly named class into the scope
		   containing the local class, not the namespace scope.  */
		context = decl_function_context (get_type_decl (cs));
	    }
	  if (!context)
	    context = current_namespace;

	  if (b->kind == sk_class
	      || (b->kind == sk_template_parms 
		  && b->level_chain->kind == sk_class))
	    in_class = 1;

	  if (current_lang_name == lang_name_java)
	    TYPE_FOR_JAVA (type) = 1;

	  d = create_implicit_typedef (name, type);
	  DECL_CONTEXT (d) = FROB_CONTEXT (context);
	  if (! in_class)
	    set_identifier_type_value_with_scope (name, d, b);

	  d = maybe_process_template_type_declaration (type,
						       globalize, b);

	  if (b->kind == sk_class)
	    {
	      if (!PROCESSING_REAL_TEMPLATE_DECL_P ())
		/* Put this TYPE_DECL on the TYPE_FIELDS list for the
		   class.  But if it's a member template class, we
		   want the TEMPLATE_DECL, not the TYPE_DECL, so this
		   is done later.  */
		finish_member_declaration (d);
	      else
		pushdecl_class_level (d);
	    }
	  else
	    d = pushdecl_with_scope (d, b);

	  /* FIXME what if it gets a name from typedef?  */
	  if (ANON_AGGRNAME_P (name))
	    DECL_IGNORED_P (d) = 1;

	  TYPE_CONTEXT (type) = DECL_CONTEXT (d);

	  /* If this is a local class, keep track of it.  We need this
	     information for name-mangling, and so that it is possible to find
	     all function definitions in a translation unit in a convenient
	     way.  (It's otherwise tricky to find a member function definition
	     it's only pointed to from within a local class.)  */
	  if (TYPE_CONTEXT (type)
	      && TREE_CODE (TYPE_CONTEXT (type)) == FUNCTION_DECL
	      && !processing_template_decl)
	    VARRAY_PUSH_TREE (local_classes, type);
        }
      if (b->kind == sk_class
	  && !COMPLETE_TYPE_P (current_class_type))
	{
	  maybe_add_class_template_decl_list (current_class_type,
					      type, /*friend_p=*/0);
	  CLASSTYPE_NESTED_UTDS (current_class_type) = b->type_decls;
	}
    }

  if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL)
    /* Use the canonical TYPE_DECL for this node.  */
    TYPE_STUB_DECL (type) = TYPE_NAME (type);
  else
    {
      /* Create a fake NULL-named TYPE_DECL node whose TREE_TYPE
	 will be the tagged type we just added to the current
	 binding level.  This fake NULL-named TYPE_DECL node helps
	 dwarfout.c to know when it needs to output a
	 representation of a tagged type, and it also gives us a
	 convenient place to record the "scope start" address for
	 the tagged type.  */

      tree d = build_decl (TYPE_DECL, NULL_TREE, type);
      TYPE_STUB_DECL (type) = pushdecl_with_scope (d, b);
    }
  timevar_pop (TV_NAME_LOOKUP);
}

/* Allocate storage for saving a C++ binding.  */
#define cxx_saved_binding_make() \
  (ggc_alloc (sizeof (cxx_saved_binding)))

struct cxx_saved_binding GTY(())
{
  /* Link that chains saved C++ bindings for a given name into a stack.  */
  cxx_saved_binding *previous;
  /* The name of the current binding.  */
  tree identifier;
  /* The binding we're saving.  */
  cxx_binding *binding;
  tree class_value;
  tree real_type_value;
};

/* Subroutines for reverting temporarily to top-level for instantiation
   of templates and such.  We actually need to clear out the class- and
   local-value slots of all identifiers, so that only the global values
   are at all visible.  Simply setting current_binding_level to the global
   scope isn't enough, because more binding levels may be pushed.  */
struct saved_scope *scope_chain;

static cxx_saved_binding *
store_bindings (tree names, cxx_saved_binding *old_bindings)
{
  tree t;
  cxx_saved_binding *search_bindings = old_bindings;

  timevar_push (TV_NAME_LOOKUP);
  for (t = names; t; t = TREE_CHAIN (t))
    {
      tree id;
      cxx_saved_binding *saved;
      cxx_saved_binding *t1;

      if (TREE_CODE (t) == TREE_LIST)
	id = TREE_PURPOSE (t);
      else
	id = DECL_NAME (t);

      if (!id
	  /* Note that we may have an IDENTIFIER_CLASS_VALUE even when
	     we have no IDENTIFIER_BINDING if we have left the class
	     scope, but cached the class-level declarations.  */
	  || !(IDENTIFIER_BINDING (id) || IDENTIFIER_CLASS_VALUE (id)))
	continue;

      for (t1 = search_bindings; t1; t1 = t1->previous)
	if (t1->identifier == id)
	  goto skip_it;

      my_friendly_assert (TREE_CODE (id) == IDENTIFIER_NODE, 135);
      saved = cxx_saved_binding_make ();
      saved->previous = old_bindings;
      saved->identifier = id;
      saved->binding = IDENTIFIER_BINDING (id);
      saved->class_value = IDENTIFIER_CLASS_VALUE (id);;
      saved->real_type_value = REAL_IDENTIFIER_TYPE_VALUE (id);
      IDENTIFIER_BINDING (id) = NULL;
      IDENTIFIER_CLASS_VALUE (id) = NULL_TREE;
      old_bindings = saved;
    skip_it:
      ;
    }
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, old_bindings);
}

void
maybe_push_to_top_level (int pseudo)
{
  struct saved_scope *s;
  struct cp_binding_level *b;
  cxx_saved_binding *old_bindings;
  int need_pop;

  timevar_push (TV_NAME_LOOKUP);
  s = ggc_alloc_cleared (sizeof (struct saved_scope));

  b = scope_chain ? current_binding_level : 0;

  /* If we're in the middle of some function, save our state.  */
  if (cfun)
    {
      need_pop = 1;
      push_function_context_to (NULL_TREE);
    }
  else
    need_pop = 0;

  old_bindings = NULL;
  if (scope_chain && previous_class_type)
    old_bindings = store_bindings (previous_class_values, old_bindings);

  /* Have to include the global scope, because class-scope decls
     aren't listed anywhere useful.  */
  for (; b; b = b->level_chain)
    {
      tree t;

      /* Template IDs are inserted into the global level. If they were
	 inserted into namespace level, finish_file wouldn't find them
	 when doing pending instantiations. Therefore, don't stop at
	 namespace level, but continue until :: .  */
      if (global_scope_p (b) || (pseudo && b->kind == sk_template_parms))
	break;

      old_bindings = store_bindings (b->names, old_bindings);
      /* We also need to check class_shadowed to save class-level type
	 bindings, since pushclass doesn't fill in b->names.  */
      if (b->kind == sk_class)
	old_bindings = store_bindings (b->class_shadowed, old_bindings);

      /* Unwind type-value slots back to top level.  */
      for (t = b->type_shadowed; t; t = TREE_CHAIN (t))
	SET_IDENTIFIER_TYPE_VALUE (TREE_PURPOSE (t), TREE_VALUE (t));
    }
  s->prev = scope_chain;
  s->old_bindings = old_bindings;
  s->bindings = b;
  s->need_pop_function_context = need_pop;
  s->function_decl = current_function_decl;
  s->last_parms = last_function_parms;

  scope_chain = s;
  current_function_decl = NULL_TREE;
  VARRAY_TREE_INIT (current_lang_base, 10, "current_lang_base");
  current_lang_name = lang_name_cplusplus;
  current_namespace = global_namespace;
  timevar_pop (TV_NAME_LOOKUP);
}

void
push_to_top_level (void)
{
  maybe_push_to_top_level (0);
}

void
pop_from_top_level (void)
{
  struct saved_scope *s = scope_chain;
  cxx_saved_binding *saved;

  timevar_push (TV_NAME_LOOKUP); 
  /* Clear out class-level bindings cache.  */
  if (previous_class_type)
    invalidate_class_lookup_cache ();

  current_lang_base = 0;

  scope_chain = s->prev;
  for (saved = s->old_bindings; saved; saved = saved->previous)
    {
      tree id = saved->identifier;

      IDENTIFIER_BINDING (id) = saved->binding;
      IDENTIFIER_CLASS_VALUE (id) = saved->class_value;
      SET_IDENTIFIER_TYPE_VALUE (id, saved->real_type_value);
    }

  /* If we were in the middle of compiling a function, restore our
     state.  */
  if (s->need_pop_function_context)
    pop_function_context_from (NULL_TREE);
  current_function_decl = s->function_decl;
  last_function_parms = s->last_parms;
  timevar_pop (TV_NAME_LOOKUP);
}

/* Pop off extraneous binding levels left over due to syntax errors.

   We don't pop past namespaces, as they might be valid.  */

void
pop_everything (void)
{
  if (ENABLE_SCOPE_CHECKING)
    verbatim ("XXX entering pop_everything ()\n");
  while (!toplevel_bindings_p ())
    {
      if (current_binding_level->kind == sk_class)
	pop_nested_class ();
      else
	poplevel (0, 0, 0);
    }
  if (ENABLE_SCOPE_CHECKING)
    verbatim ("XXX leaving pop_everything ()\n");
}

#include "gt-cp-name-lookup.h"
