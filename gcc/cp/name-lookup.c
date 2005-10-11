/* Definitions for C++ name lookup routines.
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
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
#include "flags.h"
#include "tree.h"
#include "cp-tree.h"
#include "name-lookup.h"
#include "timevar.h"
#include "toplev.h"
#include "diagnostic.h"

static cxx_scope *innermost_nonclass_level (void);
static tree select_decl (cxx_binding *, int);
static cxx_binding *binding_for_name (cxx_scope *, tree);
static tree lookup_name_current_level (tree);
static tree push_overloaded_decl (tree, int);
static bool lookup_using_namespace (tree, cxx_binding *, tree,
                                    tree, int);
static bool qualified_lookup_using_namespace (tree, tree, cxx_binding *, int);
static tree lookup_type_current_level (tree);
static tree push_using_directive (tree);


/* The :: namespace.  */

tree global_namespace;

/* The name of the anonymous namespace, throughout this translation
   unit.  */
GTY(()) tree anonymous_namespace_name;


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

/* Zero out a cxx_binding pointed to by B.  */
#define cxx_binding_clear(B) memset ((B), 0, sizeof (cxx_binding))

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
  else if (/* BVAL is null when push_class_level_binding moves an
	      inherited type-binding out of the way to make room for a
	      new value binding.  */
	   !bval 
	   /* BVAL is error_mark_node when DECL's name has been used
	      in a non-class scope prior declaration.  In that case,
	      we should have already issued a diagnostic; for graceful
	      error recovery purpose, pretend this was the intended
	      declaration for that name.  */
	   || bval == error_mark_node
	   /* If BVAL is a built-in that has not yet been declared,
	      pretend it is not there at all.  */
	   || (TREE_CODE (bval) == FUNCTION_DECL
	       && DECL_ANTICIPATED (bval)))
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
  else if (TREE_CODE (decl) == NAMESPACE_DECL
	   && TREE_CODE (bval) == NAMESPACE_DECL
	   && DECL_NAMESPACE_ALIAS (decl)
	   && DECL_NAMESPACE_ALIAS (bval)
	   && ORIGINAL_NAMESPACE (bval) == ORIGINAL_NAMESPACE (decl))
    /* [namespace.alias]
       
      In a declarative region, a namespace-alias-definition can be
      used to redefine a namespace-alias declared in that declarative
      region to refer only to the namespace to which it already
      refers.  */
    ok = false;
  else
    {
      error ("declaration of `%#D'", decl);
      cp_error_at ("conflicts with previous declaration `%#D'", bval);
      ok = false;
    }

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, ok);
}

/* Add DECL to the list of things declared in B.  */

static void
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

      /* If appropriate, add decl to separate list of statics.  We
	 include extern variables because they might turn out to be 
	 static later.  It's OK for this list to contain a few false
	 positives. */
      if (b->kind == sk_namespace)
	if ((TREE_CODE (decl) == VAR_DECL
	     && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
	    || (TREE_CODE (decl) == FUNCTION_DECL
		&& (!TREE_PUBLIC (decl) || DECL_DECLARED_INLINE_P (decl))))
	  VARRAY_PUSH_TREE (b->static_decls, decl);
    }
}

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says.  */

tree
pushdecl (tree x)
{
  tree t;
  tree name;
  int need_new_binding;

  timevar_push (TV_NAME_LOOKUP);

  need_new_binding = 1;

  if (DECL_TEMPLATE_PARM_P (x))
    /* Template parameters have no context; they are not X::T even
       when declared within a class or namespace.  */
    ;
  else
    {
      if (current_function_decl && x != current_function_decl
	  /* A local declaration for a function doesn't constitute
             nesting.  */
	  && TREE_CODE (x) != FUNCTION_DECL
	  /* A local declaration for an `extern' variable is in the
	     scope of the current namespace, not the current
	     function.  */
	  && !(TREE_CODE (x) == VAR_DECL && DECL_EXTERNAL (x))
	  && !DECL_CONTEXT (x))
	DECL_CONTEXT (x) = current_function_decl;

      /* If this is the declaration for a namespace-scope function,
	 but the declaration itself is in a local scope, mark the
	 declaration.  */
      if (TREE_CODE (x) == FUNCTION_DECL
	  && DECL_NAMESPACE_SCOPE_P (x)
	  && current_function_decl
	  && x != current_function_decl)
	DECL_LOCAL_FUNCTION_P (x) = 1;
    }

  name = DECL_NAME (x);
  if (name)
    {
      int different_binding_level = 0;

      if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
	name = TREE_OPERAND (name, 0);

      /* In case this decl was explicitly namespace-qualified, look it
	 up in its namespace context.  */
      if (DECL_NAMESPACE_SCOPE_P (x) && namespace_bindings_p ())
	t = namespace_binding (name, DECL_CONTEXT (x));
      else
	t = lookup_name_current_level (name);

      /* [basic.link] If there is a visible declaration of an entity
	 with linkage having the same name and type, ignoring entities
	 declared outside the innermost enclosing namespace scope, the
	 block scope declaration declares that same entity and
	 receives the linkage of the previous declaration.  */
      if (! t && current_function_decl && x != current_function_decl
	  && (TREE_CODE (x) == FUNCTION_DECL || TREE_CODE (x) == VAR_DECL)
	  && DECL_EXTERNAL (x))
	{
	  /* Look in block scope.  */
	  t = IDENTIFIER_VALUE (name);
	  /* Or in the innermost namespace.  */
	  if (! t)
	    t = namespace_binding (name, DECL_CONTEXT (x));
	  /* Does it have linkage?  Note that if this isn't a DECL, it's an
	     OVERLOAD, which is OK.  */
	  if (t && DECL_P (t) && ! (TREE_STATIC (t) || DECL_EXTERNAL (t)))
	    t = NULL_TREE;
	  if (t)
	    different_binding_level = 1;
	}

      /* If we are declaring a function, and the result of name-lookup
	 was an OVERLOAD, look for an overloaded instance that is
	 actually the same as the function we are declaring.  (If
	 there is one, we have to merge our declaration with the
	 previous declaration.)  */
      if (t && TREE_CODE (t) == OVERLOAD)
	{
	  tree match;

	  if (TREE_CODE (x) == FUNCTION_DECL)
	    for (match = t; match; match = OVL_NEXT (match))
	      {
		if (decls_match (OVL_CURRENT (match), x))
		  break;
	      }
	  else
	    /* Just choose one.  */
	    match = t;

	  if (match)
	    t = OVL_CURRENT (match);
	  else
	    t = NULL_TREE;
	}

      if (t == error_mark_node)
	{
	  /* error_mark_node is 0 for a while during initialization!  */
	  t = NULL_TREE;
	  cp_error_at ("`%#D' used prior to declaration", x);
	}
      else if (t != NULL_TREE)
	{
	  if (different_binding_level)
	    {
	      if (decls_match (x, t))
		/* The standard only says that the local extern
		   inherits linkage from the previous decl; in
		   particular, default args are not shared.  It would
		   be nice to propagate inlining info, though.  FIXME.  */
		TREE_PUBLIC (x) = TREE_PUBLIC (t);
	    }
	  else if (TREE_CODE (t) == PARM_DECL)
	    {
	      if (DECL_CONTEXT (t) == NULL_TREE)
		/* This is probably caused by too many errors, but calling
		   abort will say that if errors have occurred.  */
		abort ();

	      /* Check for duplicate params.  */
	      if (duplicate_decls (x, t))
		POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, t);
	    }
	  else if ((DECL_EXTERN_C_FUNCTION_P (x)
		    || DECL_FUNCTION_TEMPLATE_P (x))
		   && is_overloaded_fn (t))
	    /* Don't do anything just yet.  */;
	  else if (t == wchar_decl_node)
	    {
	      if (pedantic && ! DECL_IN_SYSTEM_HEADER (x))
		pedwarn ("redeclaration of `wchar_t' as `%T'",
			    TREE_TYPE (x));

	      /* Throw away the redeclaration.  */
	      POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, t);
	    }
	  else
	    {
	      tree olddecl = duplicate_decls (x, t);
	      
	      /* If the redeclaration failed, we can stop at this
		 point.  */
	      if (olddecl == error_mark_node)
		POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, error_mark_node);

	      if (olddecl)
		{
		  if (TREE_CODE (t) == TYPE_DECL)
		    SET_IDENTIFIER_TYPE_VALUE (name, TREE_TYPE (t));
		  else if (TREE_CODE (t) == FUNCTION_DECL)
		    check_default_args (t);

		  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, t);
		}
	      else if (DECL_MAIN_P (x) && TREE_CODE (t) == FUNCTION_DECL)
		{
		  /* A redeclaration of main, but not a duplicate of the
		     previous one.
		     
		     [basic.start.main]
		     
		     This function shall not be overloaded.  */
		  cp_error_at ("invalid redeclaration of `%D'", t);
		  error ("as `%D'", x);
		  /* We don't try to push this declaration since that
		     causes a crash.  */
		  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, x);
		}
	    }
	}

      check_template_shadow (x);

      /* If this is a function conjured up by the backend, massage it
	 so it looks friendly.  */
      if (DECL_NON_THUNK_FUNCTION_P (x) && ! DECL_LANG_SPECIFIC (x))
	{
	  retrofit_lang_decl (x);
	  SET_DECL_LANGUAGE (x, lang_c);
	}

      if (DECL_NON_THUNK_FUNCTION_P (x) && ! DECL_FUNCTION_MEMBER_P (x))
	{
	  t = push_overloaded_decl (x, PUSH_LOCAL);
	  if (t != x)
	    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, t);
	  if (!namespace_bindings_p ())
	    /* We do not need to create a binding for this name;
	       push_overloaded_decl will have already done so if
	       necessary.  */
	    need_new_binding = 0;
	}
      else if (DECL_FUNCTION_TEMPLATE_P (x) && DECL_NAMESPACE_SCOPE_P (x))
	{
	  t = push_overloaded_decl (x, PUSH_GLOBAL);
	  if (t == x)
	    add_decl_to_level (x, NAMESPACE_LEVEL (CP_DECL_CONTEXT (t)));
	  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, t);
	}

      /* If declaring a type as a typedef, copy the type (unless we're
	 at line 0), and install this TYPE_DECL as the new type's typedef
	 name.  See the extensive comment in ../c-decl.c (pushdecl).  */
      if (TREE_CODE (x) == TYPE_DECL)
	{
	  tree type = TREE_TYPE (x);
	  if (DECL_SOURCE_LINE (x) == 0)
            {
	      if (TYPE_NAME (type) == 0)
	        TYPE_NAME (type) = x;
            }
          else if (type != error_mark_node && TYPE_NAME (type) != x
		   /* We don't want to copy the type when all we're
		      doing is making a TYPE_DECL for the purposes of
		      inlining.  */
		   && (!TYPE_NAME (type)
		       || TYPE_NAME (type) != DECL_ABSTRACT_ORIGIN (x)))
            {
	      DECL_ORIGINAL_TYPE (x) = type;
              type = build_type_copy (type);
	      TYPE_STUB_DECL (type) = TYPE_STUB_DECL (DECL_ORIGINAL_TYPE (x));
              TYPE_NAME (type) = x;
              TREE_TYPE (x) = type;
            }

	  if (type != error_mark_node
	      && TYPE_NAME (type)
	      && TYPE_IDENTIFIER (type))
            set_identifier_type_value (DECL_NAME (x), x);
	}

      /* Multiple external decls of the same identifier ought to match.

	 We get warnings about inline functions where they are defined.
	 We get warnings about other functions from push_overloaded_decl.

	 Avoid duplicate warnings where they are used.  */
      if (TREE_PUBLIC (x) && TREE_CODE (x) != FUNCTION_DECL)
	{
	  tree decl;

	  decl = IDENTIFIER_NAMESPACE_VALUE (name);
	  if (decl && TREE_CODE (decl) == OVERLOAD)
	    decl = OVL_FUNCTION (decl);

	  if (decl && decl != error_mark_node
	      && (DECL_EXTERNAL (decl) || TREE_PUBLIC (decl))
	      /* If different sort of thing, we already gave an error.  */
	      && TREE_CODE (decl) == TREE_CODE (x)
	      && !same_type_p (TREE_TYPE (x), TREE_TYPE (decl)))
	    {
	      pedwarn ("type mismatch with previous external decl of `%#D'", x);
	      cp_pedwarn_at ("previous external decl of `%#D'", decl);
	    }
	}

      /* This name is new in its binding level.
	 Install the new declaration and return it.  */
      if (namespace_bindings_p ())
	{
	  /* Install a global value.  */

	  /* If the first global decl has external linkage,
	     warn if we later see static one.  */
	  if (IDENTIFIER_GLOBAL_VALUE (name) == NULL_TREE && TREE_PUBLIC (x))
	    TREE_PUBLIC (name) = 1;

 	  /* Bind the name for the entity.  */
 	  if (!(TREE_CODE (x) == TYPE_DECL && DECL_ARTIFICIAL (x)
  		&& t != NULL_TREE)
 	      && (TREE_CODE (x) == TYPE_DECL
 		  || TREE_CODE (x) == VAR_DECL
 		  || TREE_CODE (x) == ALIAS_DECL
 		  || TREE_CODE (x) == NAMESPACE_DECL
 		  || TREE_CODE (x) == CONST_DECL
 		  || TREE_CODE (x) == TEMPLATE_DECL))
 	    SET_IDENTIFIER_NAMESPACE_VALUE (name, x);

	  /* Don't forget if the function was used via an implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_USED (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_USED (x) = 1;

	  /* Don't forget if its address was taken in that way.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_ADDRESSABLE (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_ADDRESSABLE (x) = 1;

	  /* Warn about mismatches against previous implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name) != NULL_TREE
	      /* If this real decl matches the implicit, don't complain.  */
	      && ! (TREE_CODE (x) == FUNCTION_DECL
		    && TREE_TYPE (TREE_TYPE (x)) == integer_type_node))
	    warning
	      ("`%D' was previously implicitly declared to return `int'", x);

	  /* If new decl is `static' and an `extern' was seen previously,
	     warn about it.  */
	  if (x != NULL_TREE && t != NULL_TREE && decls_match (x, t))
	    warn_extern_redeclared_static (x, t);
	}
      else
	{
	  /* Here to install a non-global value.  */
	  tree oldlocal = IDENTIFIER_VALUE (name);
	  tree oldglobal = IDENTIFIER_NAMESPACE_VALUE (name);

	  if (need_new_binding)
	    {
	      push_local_binding (name, x, 0);
	      /* Because push_local_binding will hook X on to the
		 current_binding_level's name list, we don't want to
		 do that again below.  */
	      need_new_binding = 0;
	    }

	  /* If this is a TYPE_DECL, push it into the type value slot.  */
	  if (TREE_CODE (x) == TYPE_DECL)
	    set_identifier_type_value (name, x);

	  /* Clear out any TYPE_DECL shadowed by a namespace so that
	     we won't think this is a type.  The C struct hack doesn't
	     go through namespaces.  */
	  if (TREE_CODE (x) == NAMESPACE_DECL)
	    set_identifier_type_value (name, NULL_TREE);

	  if (oldlocal)
	    {
	      tree d = oldlocal;

	      while (oldlocal
		     && TREE_CODE (oldlocal) == VAR_DECL
		     && DECL_DEAD_FOR_LOCAL (oldlocal))
		oldlocal = DECL_SHADOWED_FOR_VAR (oldlocal);

	      if (oldlocal == NULL_TREE)
		oldlocal = IDENTIFIER_NAMESPACE_VALUE (DECL_NAME (d));
	    }

	  /* If this is an extern function declaration, see if we
	     have a global definition or declaration for the function.  */
	  if (oldlocal == NULL_TREE
	      && DECL_EXTERNAL (x)
	      && oldglobal != NULL_TREE
	      && TREE_CODE (x) == FUNCTION_DECL
	      && TREE_CODE (oldglobal) == FUNCTION_DECL)
	    {
	      /* We have one.  Their types must agree.  */
	      if (decls_match (x, oldglobal))
		/* OK */;
	      else
		{
		  warning ("extern declaration of `%#D' doesn't match", x);
		  cp_warning_at ("global declaration `%#D'", oldglobal);
		}
	    }
	  /* If we have a local external declaration,
	     and no file-scope declaration has yet been seen,
	     then if we later have a file-scope decl it must not be static.  */
	  if (oldlocal == NULL_TREE
	      && oldglobal == NULL_TREE
	      && DECL_EXTERNAL (x)
	      && TREE_PUBLIC (x))
	    TREE_PUBLIC (name) = 1;

	  /* Warn if shadowing an argument at the top level of the body.  */
	  if (oldlocal != NULL_TREE && !DECL_EXTERNAL (x)
	      /* Inline decls shadow nothing.  */
	      && !DECL_FROM_INLINE (x)
	      && TREE_CODE (oldlocal) == PARM_DECL
	      /* Don't check the `this' parameter.  */
	      && !DECL_ARTIFICIAL (oldlocal))
	    {
	      bool err = false;

	      /* Don't complain if it's from an enclosing function.  */
	      if (DECL_CONTEXT (oldlocal) == current_function_decl
		  && TREE_CODE (x) != PARM_DECL)
		{
		  /* Go to where the parms should be and see if we find
		     them there.  */
		  struct cp_binding_level *b = current_binding_level->level_chain;

		  /* Skip the ctor/dtor cleanup level.  */
		  b = b->level_chain;

		  /* ARM $8.3 */
		  if (b->kind == sk_function_parms)
		    {
		      error ("declaration of '%#D' shadows a parameter", x);
		      err = true;
		    }
		}

	      if (warn_shadow && !err)
		{
		  warning ("declaration of '%#D' shadows a parameter", x);
		  warning ("%Jshadowed declaration is here", oldlocal);
		}
	    }

	  /* Maybe warn if shadowing something else.  */
	  else if (warn_shadow && !DECL_EXTERNAL (x)
	      /* No shadow warnings for internally generated vars.  */
	      && ! DECL_ARTIFICIAL (x)
	      /* No shadow warnings for vars made for inlining.  */
	      && ! DECL_FROM_INLINE (x))
	    {
	      if (IDENTIFIER_CLASS_VALUE (name) != NULL_TREE
		       && current_class_ptr
		       && !TREE_STATIC (name))
		{
		  /* Location of previous decl is not useful in this case.  */
		  warning ("declaration of '%D' shadows a member of 'this'",
			   x);
		}
	      else if (oldlocal != NULL_TREE
		       && TREE_CODE (oldlocal) == VAR_DECL)
		{
		  warning ("declaration of '%D' shadows a previous local", x);
		  warning ("%Jshadowed declaration is here", oldlocal);
		}
	      else if (oldglobal != NULL_TREE
		       && TREE_CODE (oldglobal) == VAR_DECL)
		/* XXX shadow warnings in outer-more namespaces */
		{
		  warning ("declaration of '%D' shadows a global declaration",
			   x);
		  warning ("%Jshadowed declaration is here", oldglobal);
		}
	    }
	}

      if (TREE_CODE (x) == FUNCTION_DECL)
	check_default_args (x);

      if (TREE_CODE (x) == VAR_DECL)
	maybe_register_incomplete_var (x);
    }

  if (need_new_binding)
    add_decl_to_level (x,
		       DECL_NAMESPACE_SCOPE_P (x)
		       ? NAMESPACE_LEVEL (CP_DECL_CONTEXT (x))
		       : current_binding_level);

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, x);
}

/* Enter DECL into the symbol table, if that's appropriate.  Returns
   DECL, or a modified version thereof.  */

tree
maybe_push_decl (tree decl)
{
  tree type = TREE_TYPE (decl);

  /* Add this decl to the current binding level, but not if it comes
     from another scope, e.g. a static member variable.  TEM may equal
     DECL or it may be a previous decl of the same name.  */
  if (decl == error_mark_node
      || (TREE_CODE (decl) != PARM_DECL
	  && DECL_CONTEXT (decl) != NULL_TREE
	  /* Definitions of namespace members outside their namespace are
	     possible.  */
	  && TREE_CODE (DECL_CONTEXT (decl)) != NAMESPACE_DECL)
      || (TREE_CODE (decl) == TEMPLATE_DECL && !namespace_bindings_p ())
      || TREE_CODE (type) == UNKNOWN_TYPE
      /* The declaration of a template specialization does not affect
	 the functions available for overload resolution, so we do not
	 call pushdecl.  */
      || (TREE_CODE (decl) == FUNCTION_DECL
	  && DECL_TEMPLATE_SPECIALIZATION (decl)))
    return decl;
  else
    return pushdecl (decl);
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

/* The old ARM scoping rules injected variables declared in the
   initialization statement of a for-statement into the surrounding
   scope.  We support this usage, in order to be backward-compatible.
   DECL is a just-declared VAR_DECL; if necessary inject its
   declaration into the surrounding scope.  */

void
maybe_inject_for_scope_var (tree decl)
{
  timevar_push (TV_NAME_LOOKUP);
  if (!DECL_NAME (decl))
    {
      timevar_pop (TV_NAME_LOOKUP);
      return;
    }
  
  /* Declarations of __FUNCTION__ and its ilk appear magically when
     the variable is first used.  If that happens to be inside a
     for-loop, we don't want to do anything special.  */
  if (DECL_PRETTY_FUNCTION_P (decl))
    {
      timevar_pop (TV_NAME_LOOKUP);
      return;
    }

  if (current_binding_level->kind == sk_for)
    {
      struct cp_binding_level *outer
	= current_binding_level->level_chain;

      /* Check to see if the same name is already bound at the outer
	 level, either because it was directly declared, or because a
	 dead for-decl got preserved.  In either case, the code would
	 not have been valid under the ARM scope rules, so clear
	 is_for_scope for the current_binding_level.

	 Otherwise, we need to preserve the temp slot for decl to last
	 into the outer binding level.  */

      cxx_binding *outer_binding
	= IDENTIFIER_BINDING (DECL_NAME (decl))->previous;

      if (outer_binding && outer_binding->scope == outer
	  && (TREE_CODE (outer_binding->value) == VAR_DECL)
	  && DECL_DEAD_FOR_LOCAL (outer_binding->value))
	{
	  outer_binding->value = DECL_SHADOWED_FOR_VAR (outer_binding->value);
	  current_binding_level->kind = sk_block;
	}
    }
  timevar_pop (TV_NAME_LOOKUP);
}

/* Check to see whether or not DECL is a variable that would have been
   in scope under the ARM, but is not in scope under the ANSI/ISO
   standard.  If so, issue an error message.  If name lookup would
   work in both cases, but return a different result, this function
   returns the result of ANSI/ISO lookup.  Otherwise, it returns
   DECL.  */

tree
check_for_out_of_scope_variable (tree decl)
{
  tree shadowed;

  /* We only care about out of scope variables.  */
  if (!(TREE_CODE (decl) == VAR_DECL && DECL_DEAD_FOR_LOCAL (decl)))
    return decl;

  shadowed = DECL_SHADOWED_FOR_VAR (decl);
  while (shadowed != NULL_TREE && TREE_CODE (shadowed) == VAR_DECL
	 && DECL_DEAD_FOR_LOCAL (shadowed))
    shadowed = DECL_SHADOWED_FOR_VAR (shadowed);
  if (!shadowed)
    shadowed = IDENTIFIER_NAMESPACE_VALUE (DECL_NAME (decl));
  if (shadowed)
    {
      if (!DECL_ERROR_REPORTED (decl))
	{
	  warning ("name lookup of `%D' changed",
		      DECL_NAME (decl));
	  cp_warning_at ("  matches this `%D' under ISO standard rules",
			 shadowed);
	  cp_warning_at ("  matches this `%D' under old rules", decl);
	  DECL_ERROR_REPORTED (decl) = 1;
	}
      return shadowed;
    }

  /* If we have already complained about this declaration, there's no
     need to do it again.  */
  if (DECL_ERROR_REPORTED (decl))
    return decl;

  DECL_ERROR_REPORTED (decl) = 1;

  if (TREE_TYPE (decl) == error_mark_node)
    return decl;

  if (TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TREE_TYPE (decl)))
    {
      error ("name lookup of `%D' changed for new ISO `for' scoping",
	     DECL_NAME (decl));
      cp_error_at ("  cannot use obsolete binding at `%D' because it has a destructor", decl);
      return error_mark_node;
    }
  else
    {
      pedwarn ("name lookup of `%D' changed for new ISO `for' scoping",
	       DECL_NAME (decl));
      cp_pedwarn_at ("  using obsolete binding at `%D'", decl);
    }

  return decl;
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

/* Output a debugging information about SCOPE when performing
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
      /* Fall through.  */
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
     binding activity that requires us to resume a namespace.  For other
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
  if (type != error_mark_node
      && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type)
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

/* Return the name for the constructor (or destructor) for the
   specified class TYPE.  When given a template, this routine doesn't
   lose the specialization.  */

tree
constructor_name_full (tree type)
{
  type = TYPE_MAIN_VARIANT (type);
  if (CLASS_TYPE_P (type) && TYPE_WAS_ANONYMOUS (type) 
      && TYPE_HAS_CONSTRUCTOR (type))
    return DECL_NAME (OVL_CURRENT (CLASSTYPE_CONSTRUCTORS (type)));
  else
    return TYPE_IDENTIFIER (type);
}

/* Return the name for the constructor (or destructor) for the
   specified class.  When given a template, return the plain
   unspecialized name.  */

tree
constructor_name (tree type)
{
  tree name;
  name = constructor_name_full (type);
  if (IDENTIFIER_TEMPLATE (name))
    name = IDENTIFIER_TEMPLATE (name);
  return name;
}

/* Returns TRUE if NAME is the name for the constructor for TYPE.  */

bool
constructor_name_p (tree name, tree type)
{
  tree ctor_name;

  if (!name)
    return false;
  
  if (TREE_CODE (name) != IDENTIFIER_NODE)
    return false;
  
  ctor_name = constructor_name_full (type);
  if (name == ctor_name)
    return true;
  if (IDENTIFIER_TEMPLATE (ctor_name)
      && name == IDENTIFIER_TEMPLATE (ctor_name))
    return true;
  return false;
}

/* Counter used to create anonymous type names.  */

static GTY(()) int anon_cnt;

/* Return an IDENTIFIER which can be used as a name for
   anonymous structs and unions.  */

tree
make_anon_name (void)
{
  char buf[32];

  sprintf (buf, ANON_AGGRNAME_FORMAT, anon_cnt++);
  return get_identifier (buf);
}

/* Clear the TREE_PURPOSE slot of UTDs which have anonymous typenames.
   This keeps dbxout from getting confused.  */

void
clear_anon_tags (void)
{
  struct cp_binding_level *b;
  static int last_cnt = 0;

  /* Fast out if no new anon names were declared.  */
  if (last_cnt == anon_cnt)
    return;

  b = current_binding_level;
  while (b->kind == sk_cleanup)
    b = b->level_chain;
  if (b->type_decls != NULL)
    binding_table_remove_anonymous_types (b->type_decls);
  last_cnt = anon_cnt;
}

/* Return (from the stack of) the BINDING, if any, established at SCOPE.  */ 

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

static inline cxx_binding *
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

static cxx_binding *
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

/* Insert another USING_DECL into the current binding level, returning
   this declaration. If this is a redeclaration, do nothing, and
   return NULL_TREE if this not in namespace scope (in namespace
   scope, a using decl might extend any previous bindings).  */

tree
push_using_decl (tree scope, tree name)
{
  tree decl;

  timevar_push (TV_NAME_LOOKUP);
  my_friendly_assert (TREE_CODE (scope) == NAMESPACE_DECL, 383);
  my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 384);
  for (decl = current_binding_level->usings; decl; decl = TREE_CHAIN (decl))
    if (DECL_INITIAL (decl) == scope && DECL_NAME (decl) == name)
      break;
  if (decl)
    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP,
                            namespace_bindings_p () ? decl : NULL_TREE);
  decl = build_lang_decl (USING_DECL, name, void_type_node);
  DECL_INITIAL (decl) = scope;
  TREE_CHAIN (decl) = current_binding_level->usings;
  current_binding_level->usings = decl;
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, decl);
}

/* Same as pushdecl, but define X in binding-level LEVEL.  We rely on the
   caller to set DECL_CONTEXT properly.  */

tree
pushdecl_with_scope (tree x, cxx_scope *level)
{
  struct cp_binding_level *b;
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

/* DECL is a FUNCTION_DECL for a non-member function, which may have
   other definitions already in place.  We get around this by making
   the value of the identifier point to a list of all the things that
   want to be referenced by that name.  It is then up to the users of
   that name to decide what to do with that list.

   DECL may also be a TEMPLATE_DECL, with a FUNCTION_DECL in its
   DECL_TEMPLATE_RESULT.  It is dealt with the same way.

   FLAGS is a bitwise-or of the following values:
     PUSH_LOCAL: Bind DECL in the current scope, rather than at
                 namespace scope.
     PUSH_USING: DECL is being pushed as the result of a using
                 declaration.

   The value returned may be a previous declaration if we guessed wrong
   about what language DECL should belong to (C or C++).  Otherwise,
   it's always DECL (and never something that's not a _DECL).  */

static tree
push_overloaded_decl (tree decl, int flags)
{
  tree name = DECL_NAME (decl);
  tree old;
  tree new_binding;
  int doing_global = (namespace_bindings_p () || !(flags & PUSH_LOCAL));

  timevar_push (TV_NAME_LOOKUP);
  if (doing_global)
    old = namespace_binding (name, DECL_CONTEXT (decl));
  else
    old = lookup_name_current_level (name);

  if (old)
    {
      if (TREE_CODE (old) == TYPE_DECL && DECL_ARTIFICIAL (old))
	{
	  tree t = TREE_TYPE (old);
	  if (IS_AGGR_TYPE (t) && warn_shadow
	      && (! DECL_IN_SYSTEM_HEADER (decl)
		  || ! DECL_IN_SYSTEM_HEADER (old)))
	    warning ("`%#D' hides constructor for `%#T'", decl, t);
	  old = NULL_TREE;
	}
      else if (is_overloaded_fn (old))
        {
          tree tmp;

	  for (tmp = old; tmp; tmp = OVL_NEXT (tmp))
	    {
	      tree fn = OVL_CURRENT (tmp);

	      if (TREE_CODE (tmp) == OVERLOAD && OVL_USED (tmp)
		  && !(flags & PUSH_USING)
		  && compparms (TYPE_ARG_TYPES (TREE_TYPE (fn)),
				TYPE_ARG_TYPES (TREE_TYPE (decl)))
		  && ! decls_match (fn, decl))
		error ("`%#D' conflicts with previous using declaration `%#D'",
			  decl, fn);

	      if (duplicate_decls (decl, fn) == fn)
		POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, fn);
	    }
	}
      else if (old == error_mark_node)
	/* Ignore the undefined symbol marker.  */
	old = NULL_TREE;
      else
	{
	  cp_error_at ("previous non-function declaration `%#D'", old);
	  error ("conflicts with function declaration `%#D'", decl);
	  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, decl);
	}
    }

  if (old || TREE_CODE (decl) == TEMPLATE_DECL
      /* If it's a using declaration, we always need to build an OVERLOAD,
	 because it's the only way to remember that the declaration comes
	 from 'using', and have the lookup behave correctly.  */
      || (flags & PUSH_USING))
    {
      if (old && TREE_CODE (old) != OVERLOAD)
	new_binding = ovl_cons (decl, ovl_cons (old, NULL_TREE));
      else
	new_binding = ovl_cons (decl, old);
      if (flags & PUSH_USING)
	OVL_USED (new_binding) = 1;
    }
  else
    /* NAME is not ambiguous.  */
    new_binding = decl;

  if (doing_global)
    set_namespace_binding (name, current_namespace, new_binding);
  else
    {
      /* We only create an OVERLOAD if there was a previous binding at
	 this level, or if decl is a template. In the former case, we
	 need to remove the old binding and replace it with the new
	 binding.  We must also run through the NAMES on the binding
	 level where the name was bound to update the chain.  */

      if (TREE_CODE (new_binding) == OVERLOAD && old)
	{
	  tree *d;

	  for (d = &IDENTIFIER_BINDING (name)->scope->names;
	       *d;
	       d = &TREE_CHAIN (*d))
	    if (*d == old
		|| (TREE_CODE (*d) == TREE_LIST
		    && TREE_VALUE (*d) == old))
	      {
		if (TREE_CODE (*d) == TREE_LIST)
		  /* Just replace the old binding with the new.  */
		  TREE_VALUE (*d) = new_binding;
		else
		  /* Build a TREE_LIST to wrap the OVERLOAD.  */
		  *d = tree_cons (NULL_TREE, new_binding,
				  TREE_CHAIN (*d));

		/* And update the cxx_binding node.  */
		IDENTIFIER_BINDING (name)->value = new_binding;
		POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, decl);
	      }

	  /* We should always find a previous binding in this case.  */
	  abort ();
	}

      /* Install the new binding.  */
      push_local_binding (name, new_binding, flags);
    }

  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, decl);
}

/* Check a non-member using-declaration. Return the name and scope
   being used, and the USING_DECL, or NULL_TREE on failure.  */

static tree
validate_nonmember_using_decl (tree decl, tree scope, tree name)
{
  if (TREE_CODE (decl) == TEMPLATE_ID_EXPR)
    {
      /* 7.3.3/5
	   A using-declaration shall not name a template-id.  */
      error ("a using-declaration cannot specify a template-id.  Try `using %D'", name);
      return NULL_TREE;
    }

  if (TREE_CODE (decl) == NAMESPACE_DECL)
    {
      error ("namespace `%D' not allowed in using-declaration", decl);
      return NULL_TREE;
    }

  if (TREE_CODE (decl) == SCOPE_REF)
    {
      /* It's a nested name with template parameter dependent scope.
	 This can only be using-declaration for class member.  */
      error ("`%T' is not a namespace", TREE_OPERAND (decl, 0));
      return NULL_TREE;
    }

  if (is_overloaded_fn (decl))
    decl = get_first_fn (decl);

  my_friendly_assert (DECL_P (decl), 20020908);

  /* [namespace.udecl]
       A using-declaration for a class member shall be a
       member-declaration.  */
  if (TYPE_P (scope))
    {
      error ("`%T' is not a namespace", scope);
      return NULL_TREE;
    }

  /* Make a USING_DECL.  */
  return push_using_decl (scope, name);
}

/* Process local and global using-declarations.  */

static void
do_nonmember_using_decl (tree scope, tree name, tree oldval, tree oldtype,
                         tree *newval, tree *newtype)
{
  cxx_binding decls;

  *newval = *newtype = NULL_TREE;
  cxx_binding_clear (&decls);
  if (!qualified_lookup_using_namespace (name, scope, &decls, 0))
    /* Lookup error */
    return;

  if (!decls.value && !decls.type)
    {
      error ("`%D' not declared", name);
      return;
    }

  /* Check for using functions.  */
  if (decls.value && is_overloaded_fn (decls.value))
    {
      tree tmp, tmp1;

      if (oldval && !is_overloaded_fn (oldval))
	{
	  if (!DECL_IMPLICIT_TYPEDEF_P (oldval))
	    error ("`%D' is already declared in this scope", name);
	  oldval = NULL_TREE;
	}

      *newval = oldval;
      for (tmp = decls.value; tmp; tmp = OVL_NEXT (tmp))
	{
	  tree new_fn = OVL_CURRENT (tmp);

	  /* [namespace.udecl]

	     If a function declaration in namespace scope or block
	     scope has the same name and the same parameter types as a
	     function introduced by a using declaration the program is
	     ill-formed.  */
	  for (tmp1 = oldval; tmp1; tmp1 = OVL_NEXT (tmp1))
	    {
	      tree old_fn = OVL_CURRENT (tmp1);

              if (new_fn == old_fn)
                /* The function already exists in the current namespace.  */
                break;
	      else if (OVL_USED (tmp1))
	        continue; /* this is a using decl */
	      else if (compparms (TYPE_ARG_TYPES (TREE_TYPE (new_fn)),
		  		  TYPE_ARG_TYPES (TREE_TYPE (old_fn))))
		{
	          /* There was already a non-using declaration in
		     this scope with the same parameter types. If both
	             are the same extern "C" functions, that's ok.  */
                  if (decls_match (new_fn, old_fn))
		    {
		      /* If the OLD_FN was a builtin, there is now a
			 real declaration.  */
		      if (DECL_ANTICIPATED (old_fn))
			DECL_ANTICIPATED (old_fn) = 0;
		      break;
		    }
		  else if (!DECL_ANTICIPATED (old_fn))
		    {
		      /* If the OLD_FN was really declared, the
			 declarations don't match.  */
		      error ("`%D' is already declared in this scope", name);
		      break;
		    }

		  /* If the OLD_FN was not really there, just ignore
		     it and keep going.  */
		}
	    }

	  /* If we broke out of the loop, there's no reason to add
	     this function to the using declarations for this
	     scope.  */
	  if (tmp1)
	    continue;
	    
	  /* If we are adding to an existing OVERLOAD, then we no
	     longer know the type of the set of functions.  */
	  if (*newval && TREE_CODE (*newval) == OVERLOAD)
	    TREE_TYPE (*newval) = unknown_type_node;
	  /* Add this new function to the set.  */
	  *newval = build_overload (OVL_CURRENT (tmp), *newval);
	  /* If there is only one function, then we use its type.  (A
	     using-declaration naming a single function can be used in
	     contexts where overload resolution cannot be
	     performed.)  */
	  if (TREE_CODE (*newval) != OVERLOAD)
	    {
	      *newval = ovl_cons (*newval, NULL_TREE);
	      TREE_TYPE (*newval) = TREE_TYPE (OVL_CURRENT (tmp));
	    }
	  OVL_USED (*newval) = 1;
	}
    }
  else 
    {
      *newval = decls.value;
      if (oldval && !decls_match (*newval, oldval))
	error ("`%D' is already declared in this scope", name);
    }

  *newtype = decls.type;
  if (oldtype && *newtype && !same_type_p (oldtype, *newtype))
    {
      error ("using declaration `%D' introduced ambiguous type `%T'",
		name, oldtype);
      return;
    }
}

/* Process a using-declaration at function scope.  */

void
do_local_using_decl (tree decl, tree scope, tree name)
{
  tree oldval, oldtype, newval, newtype;

  decl = validate_nonmember_using_decl (decl, scope, name);
  if (decl == NULL_TREE)
    return;

  if (building_stmt_tree ()
      && at_function_scope_p ())
    add_decl_stmt (decl);

  oldval = lookup_name_current_level (name);
  oldtype = lookup_type_current_level (name);

  do_nonmember_using_decl (scope, name, oldval, oldtype, &newval, &newtype);

  if (newval)
    {
      if (is_overloaded_fn (newval))
	{
	  tree fn, term;

	  /* We only need to push declarations for those functions
	     that were not already bound in the current level.
	     The old value might be NULL_TREE, it might be a single
	     function, or an OVERLOAD.  */
	  if (oldval && TREE_CODE (oldval) == OVERLOAD)
	    term = OVL_FUNCTION (oldval);
	  else
	    term = oldval;
	  for (fn = newval; fn && OVL_CURRENT (fn) != term; 
	       fn = OVL_NEXT (fn))
	    push_overloaded_decl (OVL_CURRENT (fn), 
				  PUSH_LOCAL | PUSH_USING);
	}
      else
	push_local_binding (name, newval, PUSH_USING);
    }
  if (newtype)
    {
      push_local_binding (name, newtype, PUSH_USING);
      set_identifier_type_value (name, newtype);
    }
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
  struct cp_binding_level *level;
  /* Nonzero if, we should look past a template parameter level, even
     if THISLEVEL_ONLY.  */
  int allow_template_parms_p = 1;
  bool type_is_anonymous = ANON_AGGRNAME_P (name);

  timevar_push (TV_NAME_LOOKUP);
  for (level = binding_level; level; level = level->level_chain)
    {
      tree tail;
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
  struct cp_binding_level *level;

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

/* Returns true if ROOT (a namespace, class, or function) encloses
   CHILD.  CHILD may be either a class type or a namespace.  */

bool
is_ancestor (tree root, tree child)
{
  my_friendly_assert ((TREE_CODE (root) == NAMESPACE_DECL
		       || TREE_CODE (root) == FUNCTION_DECL
		       || CLASS_TYPE_P (root)), 20030307);
  my_friendly_assert ((TREE_CODE (child) == NAMESPACE_DECL
		       || CLASS_TYPE_P (child)),
		      20030307);
  
  /* The global namespace encloses everything.  */
  if (root == global_namespace)
    return true;

  while (true)
    {
      /* If we've run out of scopes, stop.  */
      if (!child)
	return false;
      /* If we've reached the ROOT, it encloses CHILD.  */
      if (root == child)
	return true;
      /* Go out one level.  */
      if (TYPE_P (child))
	child = TYPE_NAME (child);
      child = DECL_CONTEXT (child);
    }
}

/* Enter the class or namespace scope indicated by T.  Returns TRUE iff
   pop_scope should be called later to exit this scope.  */

bool
push_scope (tree t)
{
  bool pop = true;

  if (TREE_CODE (t) == NAMESPACE_DECL)
    push_decl_namespace (t);
  else if (CLASS_TYPE_P (t))
    {
      if (!at_class_scope_p ()
	  || !same_type_p (current_class_type, t))
	push_nested_class (t);
      else
	/* T is the same as the current scope.  There is therefore no
	   need to re-enter the scope.  Since we are not actually
	   pushing a new scope, our caller should not call
	   pop_scope.  */
	pop = false;
    }

  return pop;
}

/* Leave scope pushed by push_scope.  */

void
pop_scope (tree t)
{
  if (TREE_CODE (t) == NAMESPACE_DECL)
    pop_decl_namespace ();
  else if CLASS_TYPE_P (t)
    pop_nested_class ();
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
  struct cp_binding_level *level = class_binding_level;
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

  /* [class.mem]

     If T is the name of a class, then each of the following shall
     have a name different from T:

     -- every static data member of class T;

     -- every member of class T that is itself a type;

     -- every enumerator of every member of class T that is an
	enumerated type;

     -- every member of every anonymous union that is a member of
	class T.

     (Non-static data members were also forbidden to have the same
     name as T until TC1.)  */
  if ((TREE_CODE (x) == VAR_DECL
       || TREE_CODE (x) == CONST_DECL
       || (TREE_CODE (x) == TYPE_DECL
	   && !DECL_SELF_REFERENCE_P (x))
       || DECL_CLASS_TEMPLATE_P (x)
       /* A data member of an anonymous union.  */
       || (TREE_CODE (x) == FIELD_DECL
	   && DECL_CONTEXT (x) != current_class_type))
      && DECL_NAME (x) == constructor_name (current_class_type))
    {
      tree scope = context_for_name_lookup (x);
      if (TYPE_P (scope) && same_type_p (scope, current_class_type))
	{
	  error ("`%D' has the same name as the class in which it is declared",
		 x);
	  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, false);
	}
    }

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

tree
do_class_using_decl (tree decl)
{
  tree name, value, scope, type;
  
  if (TREE_CODE (decl) != SCOPE_REF
      || !TREE_OPERAND (decl, 0)
      || !TYPE_P (TREE_OPERAND (decl, 0)))
    {
      error ("using-declaration for non-member at class scope");
      return NULL_TREE;
    }
  scope = TREE_OPERAND (decl, 0);
  name = TREE_OPERAND (decl, 1);
  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      error ("using-declaration cannot name destructor");
      return NULL_TREE;
    }
  if (TREE_CODE (name) == TYPE_DECL)
    name = DECL_NAME (name);
  else if (TREE_CODE (name) == TEMPLATE_DECL)
     name = DECL_NAME (name);
  else if (BASELINK_P (name))
    {
      tree fns = BASELINK_FUNCTIONS (name);
      name = DECL_NAME (get_first_fn (fns));
    }

  my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 980716);

  /* Dependent using decls have a NULL type, non-dependent ones have a
     void type.  */
  type = dependent_type_p (scope) ? NULL_TREE : void_type_node;
  value = build_lang_decl (USING_DECL, name, type);
  DECL_INITIAL (value) = scope;
  return value;
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

/* Set the context of a declaration to scope. Complain if we are not
   outside scope.  */

void
set_decl_namespace (tree decl, tree scope, bool friendp)
{
  tree old;
  
  /* Get rid of namespace aliases.  */
  scope = ORIGINAL_NAMESPACE (scope);
  
  /* It is ok for friends to be qualified in parallel space.  */
  if (!friendp && !is_ancestor (current_namespace, scope))
    error ("declaration of `%D' not in a namespace surrounding `%D'",
	      decl, scope);
  DECL_CONTEXT (decl) = FROB_CONTEXT (scope);
  if (scope != current_namespace)
    {
      /* See whether this has been declared in the namespace.  */
      old = namespace_binding (DECL_NAME (decl), scope);
      if (!old)
	/* No old declaration at all.  */
	goto complain;
      /* A template can be explicitly specialized in any namespace.  */
      if (processing_explicit_instantiation)
	return;
      if (!is_overloaded_fn (decl))
	/* Don't compare non-function decls with decls_match here,
	   since it can't check for the correct constness at this
	   point. pushdecl will find those errors later.  */
	return;
      /* Since decl is a function, old should contain a function decl.  */
      if (!is_overloaded_fn (old))
	goto complain;
      if (processing_template_decl || processing_specialization)
	/* We have not yet called push_template_decl to turn a
	   FUNCTION_DECL into a TEMPLATE_DECL, so the declarations
	   won't match.  But, we'll check later, when we construct the
	   template.  */
	return;
      if (is_overloaded_fn (old))
	{
	  for (; old; old = OVL_NEXT (old))
	    if (decls_match (decl, OVL_CURRENT (old)))
	      return;
	}
      else
	if (decls_match (decl, old))
	  return;
    }
  else
    {
      /* Writing "int N::i" to declare a variable within "N" is invalid.  */
      if (at_namespace_scope_p ())
       error ("explicit qualification in declaration of `%D'", decl);
      return;
    }

 complain:
  error ("`%D' should have been declared inside `%D'",
	    decl, scope);
} 

/* Return the namespace where the current declaration is declared.  */

tree
current_decl_namespace (void)
{
  tree result;
  /* If we have been pushed into a different namespace, use it.  */
  if (decl_namespace_list)
    return TREE_PURPOSE (decl_namespace_list);

  if (current_class_type)
    result = decl_namespace_context (current_class_type);
  else if (current_function_decl)
    result = decl_namespace_context (current_function_decl);
  else 
    result = current_namespace;
  return result;
}

/* Push into the scope of the NAME namespace.  If NAME is NULL_TREE, then we
   select a name that is unique to this compilation unit.  */

void
push_namespace (tree name)
{
  tree d = NULL_TREE;
  int need_new = 1;
  int implicit_use = 0;
  bool anon = !name;

  timevar_push (TV_NAME_LOOKUP);
  
  /* We should not get here if the global_namespace is not yet constructed
     nor if NAME designates the global namespace:  The global scope is
     constructed elsewhere.  */
  my_friendly_assert (global_namespace != NULL && name != global_scope_name,
                      20030531);

  if (anon)
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
      if (anon)
	{
	  /* Clear DECL_NAME for the benefit of debugging back ends.  */
	  SET_DECL_ASSEMBLER_NAME (d, name);
	  DECL_NAME (d) = NULL_TREE;
	}
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

/* Temporarily set the namespace for the current declaration.  */

void
push_decl_namespace (tree decl)
{
  if (TREE_CODE (decl) != NAMESPACE_DECL)
    decl = decl_namespace_context (decl);
  decl_namespace_list = tree_cons (ORIGINAL_NAMESPACE (decl),
                                   NULL_TREE, decl_namespace_list);
}

/* [namespace.memdef]/2 */

void
pop_decl_namespace (void)
{
  decl_namespace_list = TREE_CHAIN (decl_namespace_list);
}

/* Return the namespace that is the common ancestor 
   of two given namespaces.  */

static tree
namespace_ancestor (tree ns1, tree ns2)
{
  timevar_push (TV_NAME_LOOKUP);
  if (is_ancestor (ns1, ns2))
    POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, ns1);
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP,
                          namespace_ancestor (CP_DECL_CONTEXT (ns1), ns2));
}

/* Process a namespace-alias declaration.  */

void
do_namespace_alias (tree alias, tree namespace)
{
  if (namespace == error_mark_node)
    return;

  my_friendly_assert (TREE_CODE (namespace) == NAMESPACE_DECL, 20050830);

  namespace = ORIGINAL_NAMESPACE (namespace);

  /* Build the alias.  */
  alias = build_lang_decl (NAMESPACE_DECL, alias, void_type_node);     
  DECL_NAMESPACE_ALIAS (alias) = namespace;
  DECL_EXTERNAL (alias) = 1;
  DECL_CONTEXT (alias) = current_scope ();
  if (!DECL_CONTEXT (alias))
    DECL_CONTEXT (alias) = FROB_CONTEXT (current_namespace);
  pushdecl (alias);
}

/* Like pushdecl, only it places X in the current namespace,
   if appropriate.  */

tree
pushdecl_namespace_level (tree x)
{
  struct cp_binding_level *b = current_binding_level;
  tree t;

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

/* Insert USED into the using list of USER. Set INDIRECT_flag if this
   directive is not directly from the source. Also find the common
   ancestor and let our users know about the new namespace */
static void 
add_using_namespace (tree user, tree used, bool indirect)
{
  tree t;
  timevar_push (TV_NAME_LOOKUP);
  /* Using oneself is a no-op.  */
  if (user == used)
    {
      timevar_pop (TV_NAME_LOOKUP);
      return;
    }
  my_friendly_assert (TREE_CODE (user) == NAMESPACE_DECL, 380);
  my_friendly_assert (TREE_CODE (used) == NAMESPACE_DECL, 380);
  /* Check if we already have this.  */
  t = purpose_member (used, DECL_NAMESPACE_USING (user));
  if (t != NULL_TREE)
    {
      if (!indirect)
	/* Promote to direct usage.  */
	TREE_INDIRECT_USING (t) = 0;
      timevar_pop (TV_NAME_LOOKUP);
      return;
    }

  /* Add used to the user's using list.  */
  DECL_NAMESPACE_USING (user) 
    = tree_cons (used, namespace_ancestor (user, used), 
		 DECL_NAMESPACE_USING (user));

  TREE_INDIRECT_USING (DECL_NAMESPACE_USING (user)) = indirect;

  /* Add user to the used's users list.  */
  DECL_NAMESPACE_USERS (used)
    = tree_cons (user, 0, DECL_NAMESPACE_USERS (used));

  /* Recursively add all namespaces used.  */
  for (t = DECL_NAMESPACE_USING (used); t; t = TREE_CHAIN (t))
    /* indirect usage */
    add_using_namespace (user, TREE_PURPOSE (t), 1);

  /* Tell everyone using us about the new used namespaces.  */
  for (t = DECL_NAMESPACE_USERS (user); t; t = TREE_CHAIN (t))
    add_using_namespace (TREE_PURPOSE (t), used, 1);
  timevar_pop (TV_NAME_LOOKUP);
}

/* Process a using-declaration not appearing in class or local scope.  */

void
do_toplevel_using_decl (tree decl, tree scope, tree name)
{
  tree oldval, oldtype, newval, newtype;
  cxx_binding *binding;

  decl = validate_nonmember_using_decl (decl, scope, name);
  if (decl == NULL_TREE)
    return;
  
  binding = binding_for_name (NAMESPACE_LEVEL (current_namespace), name);

  oldval = binding->value;
  oldtype = binding->type;

  do_nonmember_using_decl (scope, name, oldval, oldtype, &newval, &newtype);

  /* Copy declarations found.  */
  if (newval)
    binding->value = newval;
  if (newtype)
    binding->type = newtype;
  return;
}

/* Process a using-directive.  */

void
do_using_directive (tree namespace)
{
  if (namespace == error_mark_node)
    return;

  my_friendly_assert (TREE_CODE (namespace) == NAMESPACE_DECL, 20050830);

  if (building_stmt_tree ())
    add_stmt (build_stmt (USING_STMT, namespace));
  namespace = ORIGINAL_NAMESPACE (namespace);

  if (!toplevel_bindings_p ())
    push_using_directive (namespace);
  else
    /* direct usage */
    add_using_namespace (current_namespace, namespace, 0);
}

/* Deal with a using-directive seen by the parser.  Currently we only
   handle attributes here, since they cannot appear inside a template.  */

void
parse_using_directive (tree namespace, tree attribs)
{
  tree a;

  do_using_directive (namespace);

  for (a = attribs; a; a = TREE_CHAIN (a))
    {
      tree name = TREE_PURPOSE (a);
      if (is_attribute_p ("strong", name))
	{
	  if (!toplevel_bindings_p ())
	    error ("strong using only meaningful at namespace scope");
	  else if (namespace != error_mark_node)
	    DECL_NAMESPACE_ASSOCIATIONS (namespace)
	      = tree_cons (current_namespace, 0,
			   DECL_NAMESPACE_ASSOCIATIONS (namespace));
	}
      else
	warning ("`%D' attribute directive ignored", name);
    }
}

/* Like pushdecl, only it places X in the global scope if appropriate.
   Calls cp_finish_decl to register the variable, initializing it with
   *INIT, if INIT is non-NULL.  */

static tree
pushdecl_top_level_1 (tree x, tree *init)
{
  timevar_push (TV_NAME_LOOKUP);
  push_to_top_level ();
  x = pushdecl_namespace_level (x);
  if (init)
    cp_finish_decl (x, *init, NULL_TREE, 0);
  pop_from_top_level ();
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, x);
}

/* Like pushdecl, only it places X in the global scope if appropriate.  */

tree
pushdecl_top_level (tree x)
{
  return pushdecl_top_level_1 (x, NULL);
}

/* Like pushdecl, only it places X in the global scope if
   appropriate.  Calls cp_finish_decl to register the variable,
   initializing it with INIT.  */

tree
pushdecl_top_level_and_finish (tree x, tree init)
{
  return pushdecl_top_level_1 (x, &init);
}

/* Combines two sets of overloaded functions into an OVERLOAD chain, removing
   duplicates.  The first list becomes the tail of the result.

   The algorithm is O(n^2).  We could get this down to O(n log n) by
   doing a sort on the addresses of the functions, if that becomes
   necessary.  */

static tree
merge_functions (tree s1, tree s2)
{
  for (; s2; s2 = OVL_NEXT (s2))
    {
      tree fn2 = OVL_CURRENT (s2);
      tree fns1;

      for (fns1 = s1; fns1; fns1 = OVL_NEXT (fns1))
	{
	  tree fn1 = OVL_CURRENT (fns1);

	  /* If the function from S2 is already in S1, there is no
	     need to add it again.  For `extern "C"' functions, we
	     might have two FUNCTION_DECLs for the same function, in
	     different namespaces; again, we only need one of them.  */
	  if (fn1 == fn2 
	      || (DECL_EXTERN_C_P (fn1) && DECL_EXTERN_C_P (fn2)
		  && DECL_NAME (fn1) == DECL_NAME (fn2)))
	    break;
	}
      
      /* If we exhausted all of the functions in S1, FN2 is new.  */
      if (!fns1)
	s1 = build_overload (fn2, s1);
    }
  return s1;
}

/* This should return an error not all definitions define functions.
   It is not an error if we find two functions with exactly the
   same signature, only if these are selected in overload resolution.
   old is the current set of bindings, new the freshly-found binding.
   XXX Do we want to give *all* candidates in case of ambiguity?
   XXX In what way should I treat extern declarations?
   XXX I don't want to repeat the entire duplicate_decls here */

static cxx_binding *
ambiguous_decl (tree name, cxx_binding *old, cxx_binding *new, int flags)
{
  tree val, type;
  my_friendly_assert (old != NULL, 393);
  /* Copy the value.  */
  val = new->value;
  if (val)
    switch (TREE_CODE (val))
      {
      case TEMPLATE_DECL:
        /* If we expect types or namespaces, and not templates,
           or this is not a template class.  */
        if (LOOKUP_QUALIFIERS_ONLY (flags)
            && !DECL_CLASS_TEMPLATE_P (val))
          val = NULL_TREE;
        break;
      case TYPE_DECL:
        if (LOOKUP_NAMESPACES_ONLY (flags))
          val = NULL_TREE;
        break;
      case NAMESPACE_DECL:
        if (LOOKUP_TYPES_ONLY (flags))
          val = NULL_TREE;
        break;
      case FUNCTION_DECL:
        /* Ignore built-in functions that are still anticipated.  */
        if (LOOKUP_QUALIFIERS_ONLY (flags) || DECL_ANTICIPATED (val))
          val = NULL_TREE;
        break;
      default:
        if (LOOKUP_QUALIFIERS_ONLY (flags))
          val = NULL_TREE;
      }
        
  if (!old->value)
    old->value = val;
  else if (val && val != old->value)
    {
      if (is_overloaded_fn (old->value) && is_overloaded_fn (val))
        old->value = merge_functions (old->value, val);
      else
	{
	  /* Some declarations are functions, some are not.  */
          if (flags & LOOKUP_COMPLAIN)
            {
	      /* If we've already given this error for this lookup,
		 old->value is error_mark_node, so let's not
		 repeat ourselves.  */
	      if (old->value != error_mark_node)
		{
		  error ("use of `%D' is ambiguous", name);
		  cp_error_at ("  first declared as `%#D' here",
			       old->value);
		}
              cp_error_at ("  also declared as `%#D' here", val);
            }
	  old->value = error_mark_node;
	}
    }
  /* ... and copy the type.  */
  type = new->type;
  if (LOOKUP_NAMESPACES_ONLY (flags))
    type = NULL_TREE;
  if (!old->type)
    old->type = type;
  else if (type && old->type != type)
    {
      if (flags & LOOKUP_COMPLAIN)
        {
          error ("`%D' denotes an ambiguous type",name);
          error ("%J  first type here", TYPE_MAIN_DECL (old->type));
          error ("%J  other type here", TYPE_MAIN_DECL (type));
        }
    }
  return old;
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
   considering using-directives.  */

static tree
unqualified_namespace_lookup (tree name, int flags)
{
  tree initial = current_decl_namespace ();
  tree scope = initial;
  tree siter;
  struct cp_binding_level *level;
  tree val = NULL_TREE;
  cxx_binding binding;

  timevar_push (TV_NAME_LOOKUP);
  cxx_binding_clear (&binding);

  for (; !val; scope = CP_DECL_CONTEXT (scope))
    {
      cxx_binding *b =
         cxx_scope_find_binding_for_name (NAMESPACE_LEVEL (scope), name);

      if (b)
	{
	  if (b->value && DECL_P (b->value)
	      && DECL_LANG_SPECIFIC (b->value) 
	      && DECL_ANTICIPATED (b->value))
	    /* Ignore anticipated built-in functions.  */
	    ;
	  else
	    binding.value = b->value;
	  binding.type = b->type;
	}

      /* Add all _DECLs seen through local using-directives.  */
      for (level = current_binding_level;
	   level->kind != sk_namespace;
	   level = level->level_chain)
	if (!lookup_using_namespace (name, &binding, level->using_directives,
                                     scope, flags))
	  /* Give up because of error.  */
	  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, error_mark_node);

      /* Add all _DECLs seen through global using-directives.  */
      /* XXX local and global using lists should work equally.  */
      siter = initial;
      while (1)
	{
	  if (!lookup_using_namespace (name, &binding,
                                       DECL_NAMESPACE_USING (siter),
				       scope, flags))
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
      if (qualified_lookup_using_namespace (name, scope, &binding, flags))
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

/* Subroutine of unqualified_namespace_lookup:
   Add the bindings of NAME in used namespaces to VAL.
   We are currently looking for names in namespace SCOPE, so we
   look through USINGS for using-directives of namespaces
   which have SCOPE as a common ancestor with the current scope.
   Returns false on errors.  */

static bool
lookup_using_namespace (tree name, cxx_binding *val, tree usings, tree scope,
                        int flags)
{
  tree iter;
  timevar_push (TV_NAME_LOOKUP);
  /* Iterate over all used namespaces in current, searching for using
     directives of scope.  */
  for (iter = usings; iter; iter = TREE_CHAIN (iter))
    if (TREE_VALUE (iter) == scope)
      {
        tree used = ORIGINAL_NAMESPACE (TREE_PURPOSE (iter));
        cxx_binding *val1 =
          cxx_scope_find_binding_for_name (NAMESPACE_LEVEL (used), name);
        /* Resolve ambiguities.  */
        if (val1)
          val = ambiguous_decl (name, val, val1, flags);
      }
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, val->value != error_mark_node);
}

/* [namespace.qual]
   Accepts the NAME to lookup and its qualifying SCOPE.
   Returns the name/type pair found into the cxx_binding *RESULT,
   or false on error.  */

static bool
qualified_lookup_using_namespace (tree name, tree scope, cxx_binding *result,
                                  int flags)
{
  /* Maintain a list of namespaces visited...  */
  tree seen = NULL_TREE;
  /* ... and a list of namespace yet to see.  */
  tree todo = NULL_TREE;
  tree todo_maybe = NULL_TREE;
  tree usings;
  timevar_push (TV_NAME_LOOKUP);
  /* Look through namespace aliases.  */
  scope = ORIGINAL_NAMESPACE (scope);
  while (scope && result->value != error_mark_node)
    {
      cxx_binding *binding =
	cxx_scope_find_binding_for_name (NAMESPACE_LEVEL (scope), name);
      seen = tree_cons (scope, NULL_TREE, seen);
      if (binding)
        result = ambiguous_decl (name, result, binding, flags);

      /* Consider strong using directives always, and non-strong ones
	 if we haven't found a binding yet.  ??? Shouldn't we consider
	 non-strong ones if the initial RESULT is non-NULL, but the
	 binding in the given namespace is?  */
      for (usings = DECL_NAMESPACE_USING (scope); usings;
	   usings = TREE_CHAIN (usings))
	/* If this was a real directive, and we have not seen it.  */
	if (!TREE_INDIRECT_USING (usings))
	  {
	    /* Try to avoid queuing the same namespace more than once,
	       the exception being when a namespace was already
	       enqueued for todo_maybe and then a strong using is
	       found for it.  We could try to remove it from
	       todo_maybe, but it's probably not worth the effort.  */
	    if (is_associated_namespace (scope, TREE_PURPOSE (usings))
		&& !purpose_member (TREE_PURPOSE (usings), seen)
		&& !purpose_member (TREE_PURPOSE (usings), todo))
	      todo = tree_cons (TREE_PURPOSE (usings), NULL_TREE, todo);
	    else if ((!result->value && !result->type)
		     && !purpose_member (TREE_PURPOSE (usings), seen)
		     && !purpose_member (TREE_PURPOSE (usings), todo)
		     && !purpose_member (TREE_PURPOSE (usings), todo_maybe))
	      todo_maybe = tree_cons (TREE_PURPOSE (usings), NULL_TREE,
				      todo_maybe);
	  }
      if (todo)
	{
	  scope = TREE_PURPOSE (todo);
	  todo = TREE_CHAIN (todo);
	}
      else if (todo_maybe
	       && (!result->value && !result->type))
	{
	  scope = TREE_PURPOSE (todo_maybe);
	  todo = TREE_CHAIN (todo_maybe);
	  todo_maybe = NULL_TREE;
	}
      else
	scope = NULL_TREE; /* If there never was a todo list.  */
    }
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, result->value != error_mark_node);
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
      tree t = unqualified_namespace_lookup (name, flags);
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

static tree
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

static tree
lookup_type_current_level (tree name)
{
  tree t = NULL_TREE;

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

/* [basic.lookup.koenig] */
/* A nonzero return value in the functions below indicates an error.  */

struct arg_lookup
{
  tree name;
  tree namespaces;
  tree classes;
  tree functions;
};

static bool arg_assoc (struct arg_lookup*, tree);
static bool arg_assoc_args (struct arg_lookup*, tree);
static bool arg_assoc_type (struct arg_lookup*, tree);
static bool add_function (struct arg_lookup *, tree);
static bool arg_assoc_namespace (struct arg_lookup *, tree);
static bool arg_assoc_class (struct arg_lookup *, tree);
static bool arg_assoc_template_arg (struct arg_lookup*, tree);

/* Add a function to the lookup structure.
   Returns true on error.  */

static bool
add_function (struct arg_lookup *k, tree fn)
{
  /* We used to check here to see if the function was already in the list,
     but that's O(n^2), which is just too expensive for function lookup.
     Now we deal with the occasional duplicate in joust.  In doing this, we
     assume that the number of duplicates will be small compared to the
     total number of functions being compared, which should usually be the
     case.  */

  /* We must find only functions, or exactly one non-function.  */
  if (!k->functions) 
    k->functions = fn;
  else if (fn == k->functions)
    ;
  else if (is_overloaded_fn (k->functions) && is_overloaded_fn (fn))
    k->functions = build_overload (fn, k->functions);
  else
    {
      tree f1 = OVL_CURRENT (k->functions);
      tree f2 = fn;
      if (is_overloaded_fn (f1))
	{
	  fn = f1; f1 = f2; f2 = fn;
	}
      cp_error_at ("`%D' is not a function,", f1);
      cp_error_at ("  conflict with `%D'", f2);
      error ("  in call to `%D'", k->name);
      return true;
    }

  return false;
}

/* Returns true iff CURRENT has declared itself to be an associated
   namespace of SCOPE via a strong using-directive (or transitive chain
   thereof).  Both are namespaces.  */

bool
is_associated_namespace (tree current, tree scope)
{
  tree seen = NULL_TREE;
  tree todo = NULL_TREE;
  tree t;
  while (1)
    {
      if (scope == current)
	return true;
      seen = tree_cons (scope, NULL_TREE, seen);
      for (t = DECL_NAMESPACE_ASSOCIATIONS (scope); t; t = TREE_CHAIN (t))
	if (!purpose_member (TREE_PURPOSE (t), seen))
	  todo = tree_cons (TREE_PURPOSE (t), NULL_TREE, todo);
      if (todo)
	{
	  scope = TREE_PURPOSE (todo);
	  todo = TREE_CHAIN (todo);
	}
      else
	return false;
    }
}

/* Add functions of a namespace to the lookup structure.
   Returns true on error.  */

static bool
arg_assoc_namespace (struct arg_lookup *k, tree scope)
{
  tree value;

  if (purpose_member (scope, k->namespaces))
    return 0;
  k->namespaces = tree_cons (scope, NULL_TREE, k->namespaces);

  /* Check out our super-users.  */
  for (value = DECL_NAMESPACE_ASSOCIATIONS (scope); value;
       value = TREE_CHAIN (value))
    if (arg_assoc_namespace (k, TREE_PURPOSE (value)))
      return true;
  
  value = namespace_binding (k->name, scope);
  if (!value)
    return false;

  for (; value; value = OVL_NEXT (value))
    if (add_function (k, OVL_CURRENT (value)))
      return true;
  
  return false;
}

/* Adds everything associated with a template argument to the lookup
   structure.  Returns true on error.  */

static bool
arg_assoc_template_arg (struct arg_lookup *k, tree arg)
{
  /* [basic.lookup.koenig]

     If T is a template-id, its associated namespaces and classes are
     ... the namespaces and classes associated with the types of the
     template arguments provided for template type parameters
     (excluding template template parameters); the namespaces in which
     any template template arguments are defined; and the classes in
     which any member templates used as template template arguments
     are defined.  [Note: non-type template arguments do not
     contribute to the set of associated namespaces.  ]  */

  /* Consider first template template arguments.  */
  if (TREE_CODE (arg) == TEMPLATE_TEMPLATE_PARM
      || TREE_CODE (arg) == UNBOUND_CLASS_TEMPLATE)
    return false;
  else if (TREE_CODE (arg) == TEMPLATE_DECL)
    {
      tree ctx = CP_DECL_CONTEXT (arg);

      /* It's not a member template.  */
      if (TREE_CODE (ctx) == NAMESPACE_DECL)
        return arg_assoc_namespace (k, ctx);
      /* Otherwise, it must be member template.  */
      else 
        return arg_assoc_class (k, ctx);
    }
  /* It's not a template template argument, but it is a type template
     argument.  */
  else if (TYPE_P (arg))
    return arg_assoc_type (k, arg);
  /* It's a non-type template argument.  */
  else
    return false;
}

/* Adds everything associated with class to the lookup structure.
   Returns true on error.  */

static bool
arg_assoc_class (struct arg_lookup *k, tree type)
{
  tree list, friends, context;
  int i;
  
  /* Backend build structures, such as __builtin_va_list, aren't
     affected by all this.  */
  if (!CLASS_TYPE_P (type))
    return false;

  if (purpose_member (type, k->classes))
    return false;
  k->classes = tree_cons (type, NULL_TREE, k->classes);
  
  context = decl_namespace_context (type);
  if (arg_assoc_namespace (k, context))
    return true;
  
  /* Process baseclasses.  */
  for (i = 0; i < CLASSTYPE_N_BASECLASSES (type); i++)
    if (arg_assoc_class (k, TYPE_BINFO_BASETYPE (type, i)))
      return true;
  
  /* Process friends.  */
  for (list = DECL_FRIENDLIST (TYPE_MAIN_DECL (type)); list; 
       list = TREE_CHAIN (list))
    if (k->name == FRIEND_NAME (list))
      for (friends = FRIEND_DECLS (list); friends; 
	   friends = TREE_CHAIN (friends))
	{
	  tree fn = TREE_VALUE (friends);

	  /* Only interested in global functions with potentially hidden
	     (i.e. unqualified) declarations.  */
	  if (CP_DECL_CONTEXT (fn) != context)
	    continue;
	  /* Template specializations are never found by name lookup.
	     (Templates themselves can be found, but not template
	     specializations.)  */
	  if (TREE_CODE (fn) == FUNCTION_DECL && DECL_USE_TEMPLATE (fn))
	    continue;
	  if (add_function (k, fn))
	    return true;
	}

  /* Process template arguments.  */
  if (CLASSTYPE_TEMPLATE_INFO (type))
    {
      list = INNERMOST_TEMPLATE_ARGS (CLASSTYPE_TI_ARGS (type));
      for (i = 0; i < TREE_VEC_LENGTH (list); ++i) 
        arg_assoc_template_arg (k, TREE_VEC_ELT (list, i));
    }

  return false;
}

/* Adds everything associated with a given type.
   Returns 1 on error.  */

static bool
arg_assoc_type (struct arg_lookup *k, tree type)
{
  /* As we do not get the type of non-type dependent expressions
     right, we can end up with such things without a type.  */
  if (!type)
    return false;

  if (TYPE_PTRMEM_P (type))
    {
      /* Pointer to member: associate class type and value type.  */
      if (arg_assoc_type (k, TYPE_PTRMEM_CLASS_TYPE (type)))
	return true;
      return arg_assoc_type (k, TYPE_PTRMEM_POINTED_TO_TYPE (type));
    }
  else switch (TREE_CODE (type))
    {
    case ERROR_MARK:
      return false;
    case VOID_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case CHAR_TYPE:
    case BOOLEAN_TYPE:
      return false;
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (type))
	return arg_assoc_type (k, TYPE_PTRMEMFUNC_FN_TYPE (type));
      return arg_assoc_class (k, type);
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case ARRAY_TYPE:
      return arg_assoc_type (k, TREE_TYPE (type));
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      return arg_assoc_namespace (k, decl_namespace_context (type));
    case METHOD_TYPE:
      /* The basetype is referenced in the first arg type, so just
	 fall through.  */
    case FUNCTION_TYPE:
      /* Associate the parameter types.  */
      if (arg_assoc_args (k, TYPE_ARG_TYPES (type)))
	return true;
      /* Associate the return type.  */
      return arg_assoc_type (k, TREE_TYPE (type));
    case TEMPLATE_TYPE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      return false;
    case TYPENAME_TYPE:
      return false;
    case LANG_TYPE:
      if (type == unknown_type_node)
	return false;
      /* else fall through */
    default:
      abort ();
    }
  return false;
}

/* Adds everything associated with arguments.  Returns true on error.  */

static bool
arg_assoc_args (struct arg_lookup *k, tree args)
{
  for (; args; args = TREE_CHAIN (args))
    if (arg_assoc (k, TREE_VALUE (args)))
      return true;
  return false;
}

/* Adds everything associated with a given tree_node.  Returns 1 on error.  */

static bool
arg_assoc (struct arg_lookup *k, tree n)
{
  if (n == error_mark_node)
    return false;

  if (TYPE_P (n))
    return arg_assoc_type (k, n);

  if (! type_unknown_p (n))
    return arg_assoc_type (k, TREE_TYPE (n));

  if (TREE_CODE (n) == ADDR_EXPR)
    n = TREE_OPERAND (n, 0);
  if (TREE_CODE (n) == COMPONENT_REF)
    n = TREE_OPERAND (n, 1);
  if (TREE_CODE (n) == OFFSET_REF)
    n = TREE_OPERAND (n, 1);
  while (TREE_CODE (n) == TREE_LIST)
    n = TREE_VALUE (n);
  if (TREE_CODE (n) == BASELINK)
    n = BASELINK_FUNCTIONS (n);

  if (TREE_CODE (n) == FUNCTION_DECL)
    return arg_assoc_type (k, TREE_TYPE (n));
  if (TREE_CODE (n) == TEMPLATE_ID_EXPR)
    {
      /* [basic.lookup.koenig]

	 If T is a template-id, its associated namespaces and classes
	 are the namespace in which the template is defined; for
	 member templates, the member template's class...  */
      tree template = TREE_OPERAND (n, 0);
      tree args = TREE_OPERAND (n, 1);
      tree ctx;
      int ix;

      if (TREE_CODE (template) == COMPONENT_REF)
        template = TREE_OPERAND (template, 1);
      
      /* First, the template.  There may actually be more than one if
	 this is an overloaded function template.  But, in that case,
	 we only need the first; all the functions will be in the same
	 namespace.  */
      template = OVL_CURRENT (template);

      ctx = CP_DECL_CONTEXT (template);
       
      if (TREE_CODE (ctx) == NAMESPACE_DECL)
	{
	  if (arg_assoc_namespace (k, ctx) == 1)
	    return true;
	}
      /* It must be a member template.  */
      else if (arg_assoc_class (k, ctx) == 1)
	return true;

      /* Now the arguments.  */
      for (ix = TREE_VEC_LENGTH (args); ix--;)
	if (arg_assoc_template_arg (k, TREE_VEC_ELT (args, ix)) == 1)
	  return true;
    }
  else if (TREE_CODE (n) == OVERLOAD)
    {
      for (; n; n = OVL_CHAIN (n))
	if (arg_assoc_type (k, TREE_TYPE (OVL_FUNCTION (n))))
	  return true;
    }

  return false;
}

/* Performs Koenig lookup depending on arguments, where fns
   are the functions found in normal lookup.  */

tree
lookup_arg_dependent (tree name, tree fns, tree args)
{
  struct arg_lookup k;
  tree fn = NULL_TREE;

  timevar_push (TV_NAME_LOOKUP);
  k.name = name;
  k.functions = fns;
  k.classes = NULL_TREE;

  /* We've already looked at some namespaces during normal unqualified
     lookup -- but we don't know exactly which ones.  If the functions
     we found were brought into the current namespace via a using
     declaration, we have not really checked the namespace from which
     they came.  Therefore, we check all namespaces here -- unless the
     function we have is from the current namespace.  Even then, we
     must check all namespaces if the function is a local
     declaration; any other declarations present at namespace scope
     should be visible during argument-dependent lookup.  */
  if (fns)
    fn = OVL_CURRENT (fns);
  if (fn && TREE_CODE (fn) == FUNCTION_DECL 
      && (CP_DECL_CONTEXT (fn) != current_decl_namespace ()
	  || DECL_LOCAL_FUNCTION_P (fn)))
    k.namespaces = NULL_TREE;
  else
    /* Setting NAMESPACES is purely an optimization; it prevents
       adding functions which are already in FNS.  Adding them would
       be safe -- "joust" will eliminate the duplicates -- but
       wasteful.  */
    k.namespaces = build_tree_list (current_decl_namespace (), NULL_TREE);

  arg_assoc_args (&k, args);
  POP_TIMEVAR_AND_RETURN (TV_NAME_LOOKUP, k.functions);
}

/* Add namespace to using_directives. Return NULL_TREE if nothing was
   changed (i.e. there was already a directive), or the fresh
   TREE_LIST otherwise.  */

static tree
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
  struct cp_binding_level *b;

  timevar_push (TV_NAME_LOOKUP);
  b = current_binding_level;
  while (/* Cleanup scopes are not scopes from the point of view of
	    the language.  */
	 b->kind == sk_cleanup
	 /* Neither are the scopes used to hold template parameters
	    for an explicit specialization.  For an ordinary template
	    declaration, these scopes are not scopes from the point of
	    view of the language -- but we need a place to stash
	    things that will go in the containing namespace when the
	    template is instantiated.  */
	 || (b->kind == sk_template_parms && b->explicit_spec_p)
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
          tree d = NULL_TREE;
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
push_to_top_level (void)
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
      if (global_scope_p (b))
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

  scope_chain = s;
  current_function_decl = NULL_TREE;
  VARRAY_TREE_INIT (current_lang_base, 10, "current_lang_base");
  current_lang_name = lang_name_cplusplus;
  current_namespace = global_namespace;
  timevar_pop (TV_NAME_LOOKUP);
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
