/* Definitions for C++ name lookup routines.
   Copyright (C) 2003-2016 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "timevar.h"
#include "stringpool.h"
#include "print-tree.h"
#include "attribs.h"
#include "debug.h"
#include "c-family/c-pragma.h"
#include "params.h"

/* The bindings for a particular name in a particular scope.  */

struct scope_binding {
  tree value;
  tree type;
};
#define EMPTY_SCOPE_BINDING { NULL_TREE, NULL_TREE }

static cp_binding_level *innermost_nonclass_level (void);
static cxx_binding *binding_for_name (cp_binding_level *, tree);
static tree push_overloaded_decl (tree, int, bool);
static bool lookup_using_namespace (tree, struct scope_binding *, tree,
				    tree, int);
static bool qualified_lookup_using_namespace (tree, tree,
					      struct scope_binding *, int);
static tree lookup_type_current_level (tree);
static tree push_using_directive (tree);
static tree lookup_extern_c_fun_in_all_ns (tree);
static void diagnose_name_conflict (tree, tree);

/* The :: namespace.  */

tree global_namespace;

/* The name of the anonymous namespace, throughout this translation
   unit.  */
static GTY(()) tree anonymous_namespace_name;

/* Initialize anonymous_namespace_name if necessary, and return it.  */

static tree
get_anonymous_namespace_name (void)
{
  if (!anonymous_namespace_name)
    {
      /* We used to use get_file_function_name here, but that isn't
	 necessary now that anonymous namespace typeinfos
	 are !TREE_PUBLIC, and thus compared by address.  */
      /* The demangler expects anonymous namespaces to be called
	 something starting with '_GLOBAL__N_'.  */
      anonymous_namespace_name = get_identifier ("_GLOBAL__N_1");
    }
  return anonymous_namespace_name;
}

/* Compute the chain index of a binding_entry given the HASH value of its
   name and the total COUNT of chains.  COUNT is assumed to be a power
   of 2.  */

#define ENTRY_INDEX(HASH, COUNT) (((HASH) >> 3) & ((COUNT) - 1))

/* A free list of "binding_entry"s awaiting for re-use.  */

static GTY((deletable)) binding_entry free_binding_entry = NULL;

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
    entry = ggc_alloc<binding_entry_s> ();

  entry->name = name;
  entry->type = type;
  entry->chain = NULL;

  return entry;
}

/* Put ENTRY back on the free list.  */
#if 0
static inline void
binding_entry_free (binding_entry entry)
{
  entry->name = NULL;
  entry->type = NULL;
  entry->chain = free_binding_entry;
  free_binding_entry = entry;
}
#endif

/* The datatype used to implement the mapping from names to types at
   a given scope.  */
struct GTY(()) binding_table_s {
  /* Array of chains of "binding_entry"s  */
  binding_entry * GTY((length ("%h.chain_count"))) chain;

  /* The number of chains in this table.  This is the length of the
     member "chain" considered as an array.  */
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
  table->chain = ggc_cleared_vec_alloc<binding_entry> (table->chain_count);
}

/* Make TABLE's entries ready for reuse.  */
#if 0
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
#endif

/* Allocate a table with CHAIN_COUNT, assumed to be a power of two.  */

static inline binding_table
binding_table_new (size_t chain_count)
{
  binding_table table = ggc_alloc<binding_table_s> ();
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

/* Apply PROC -- with DATA -- to all entries in TABLE.  */

void
binding_table_foreach (binding_table table, bt_foreach_proc proc, void *data)
{
  size_t chain_count;
  size_t i;

  if (!table)
    return;

  chain_count = table->chain_count;
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

static GTY((deletable)) cxx_binding *free_bindings;

/* Initialize VALUE and TYPE field for BINDING, and set the PREVIOUS
   field to NULL.  */

static inline void
cxx_binding_init (cxx_binding *binding, tree value, tree type)
{
  binding->value = value;
  binding->type = type;
  binding->previous = NULL;
}

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
    binding = ggc_alloc<cxx_binding> ();

  cxx_binding_init (binding, value, type);

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

/* Create a new binding for NAME (with the indicated VALUE and TYPE
   bindings) in the class scope indicated by SCOPE.  */

static cxx_binding *
new_class_binding (tree name, tree value, tree type, cp_binding_level *scope)
{
  cp_class_binding cb = {cxx_binding_make (value, type), name};
  cxx_binding *binding = cb.base;
  vec_safe_push (scope->class_shadowed, cb);
  binding->scope = scope;
  return binding;
}

/* Make DECL the innermost binding for ID.  The LEVEL is the binding
   level at which this declaration is being bound.  */

void
push_binding (tree id, tree decl, cp_binding_level* level)
{
  cxx_binding *binding;

  if (level != class_binding_level)
    {
      binding = cxx_binding_make (decl, NULL_TREE);
      binding->scope = level;
    }
  else
    binding = new_class_binding (id, decl, /*type=*/NULL_TREE, level);

  /* Now, fill in the binding information.  */
  binding->previous = IDENTIFIER_BINDING (id);
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
  gcc_assert (binding != NULL);

  /* The DECL will be either the ordinary binding or the type
     binding for this identifier.  Remove that binding.  */
  if (binding->value == decl)
    binding->value = NULL_TREE;
  else
    {
      gcc_assert (binding->type == decl);
      binding->type = NULL_TREE;
    }

  if (!binding->value && !binding->type)
    {
      /* We're completely done with the innermost binding for this
	 identifier.  Unhook it from the list of bindings.  */
      IDENTIFIER_BINDING (id) = binding->previous;

      /* Add it to the free list.  */
      cxx_binding_free (binding);
    }
}

/* Remove the bindings for the decls of the current level and leave
   the current scope.  */

void
pop_bindings_and_leave_scope (void)
{
  for (tree t = getdecls (); t; t = DECL_CHAIN (t))
    pop_binding (DECL_NAME (t), t);
  leave_scope ();
}

/* Strip non dependent using declarations. If DECL is dependent,
   surreptitiously create a typename_type and return it.  */

tree
strip_using_decl (tree decl)
{
  if (decl == NULL_TREE)
    return NULL_TREE;

  while (TREE_CODE (decl) == USING_DECL && !DECL_DEPENDENT_P (decl))
    decl = USING_DECL_DECLS (decl);

  if (TREE_CODE (decl) == USING_DECL && DECL_DEPENDENT_P (decl)
      && USING_DECL_TYPENAME_P (decl))
    {
      /* We have found a type introduced by a using
	 declaration at class scope that refers to a dependent
	 type.
	     
	 using typename :: [opt] nested-name-specifier unqualified-id ;
      */
      decl = make_typename_type (TREE_TYPE (decl),
				 DECL_NAME (decl),
				 typename_type, tf_error);
      if (decl != error_mark_node)
	decl = TYPE_NAME (decl);
    }

  return decl;
}

/* BINDING records an existing declaration for a name in the current scope.
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
supplement_binding_1 (cxx_binding *binding, tree decl)
{
  tree bval = binding->value;
  bool ok = true;
  tree target_bval = strip_using_decl (bval);
  tree target_decl = strip_using_decl (decl);

  if (TREE_CODE (target_decl) == TYPE_DECL && DECL_ARTIFICIAL (target_decl)
      && target_decl != target_bval
      && (TREE_CODE (target_bval) != TYPE_DECL
	  /* We allow pushing an enum multiple times in a class
	     template in order to handle late matching of underlying
	     type on an opaque-enum-declaration followed by an
	     enum-specifier.  */
	  || (processing_template_decl
	      && TREE_CODE (TREE_TYPE (target_decl)) == ENUMERAL_TYPE
	      && TREE_CODE (TREE_TYPE (target_bval)) == ENUMERAL_TYPE
	      && (dependent_type_p (ENUM_UNDERLYING_TYPE
				    (TREE_TYPE (target_decl)))
		  || dependent_type_p (ENUM_UNDERLYING_TYPE
				       (TREE_TYPE (target_bval)))))))
    /* The new name is the type name.  */
    binding->type = decl;
  else if (/* TARGET_BVAL is null when push_class_level_binding moves
	      an inherited type-binding out of the way to make room
	      for a new value binding.  */
	   !target_bval
	   /* TARGET_BVAL is error_mark_node when TARGET_DECL's name
	      has been used in a non-class scope prior declaration.
	      In that case, we should have already issued a
	      diagnostic; for graceful error recovery purpose, pretend
	      this was the intended declaration for that name.  */
	   || target_bval == error_mark_node
	   /* If TARGET_BVAL is anticipated but has not yet been
	      declared, pretend it is not there at all.  */
	   || (TREE_CODE (target_bval) == FUNCTION_DECL
	       && DECL_ANTICIPATED (target_bval)
	       && !DECL_HIDDEN_FRIEND_P (target_bval)))
    binding->value = decl;
  else if (TREE_CODE (target_bval) == TYPE_DECL
	   && DECL_ARTIFICIAL (target_bval)
	   && target_decl != target_bval
	   && (TREE_CODE (target_decl) != TYPE_DECL
	       || same_type_p (TREE_TYPE (target_decl),
			       TREE_TYPE (target_bval))))
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
  else if (TREE_CODE (target_bval) == TYPE_DECL
	   && TREE_CODE (target_decl) == TYPE_DECL
	   && DECL_NAME (target_decl) == DECL_NAME (target_bval)
	   && binding->scope->kind != sk_class
	   && (same_type_p (TREE_TYPE (target_decl), TREE_TYPE (target_bval))
	       /* If either type involves template parameters, we must
		  wait until instantiation.  */
	       || uses_template_parms (TREE_TYPE (target_decl))
	       || uses_template_parms (TREE_TYPE (target_bval))))
    /* We have two typedef-names, both naming the same type to have
       the same name.  In general, this is OK because of:

	 [dcl.typedef]

	 In a given scope, a typedef specifier can be used to redefine
	 the name of any type declared in that scope to refer to the
	 type to which it already refers.

       However, in class scopes, this rule does not apply due to the
       stricter language in [class.mem] prohibiting redeclarations of
       members.  */
    ok = false;
  /* There can be two block-scope declarations of the same variable,
     so long as they are `extern' declarations.  However, there cannot
     be two declarations of the same static data member:

       [class.mem]

       A member shall not be declared twice in the
       member-specification.  */
  else if (VAR_P (target_decl)
	   && VAR_P (target_bval)
	   && DECL_EXTERNAL (target_decl) && DECL_EXTERNAL (target_bval)
	   && !DECL_CLASS_SCOPE_P (target_decl))
    {
      duplicate_decls (decl, binding->value, /*newdecl_is_friend=*/false);
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
  else if (maybe_remove_implicit_alias (bval))
    {
      /* There was a mangling compatibility alias using this mangled name,
	 but now we have a real decl that wants to use it instead.  */
      binding->value = decl;
    }
  else
    {
      diagnose_name_conflict (decl, bval);
      ok = false;
    }

  return ok;
}

/* Diagnose a name conflict between DECL and BVAL.  */

static void
diagnose_name_conflict (tree decl, tree bval)
{
  if (TREE_CODE (decl) == TREE_CODE (bval)
      && (TREE_CODE (decl) != TYPE_DECL
	  || (DECL_ARTIFICIAL (decl) && DECL_ARTIFICIAL (bval))
	  || (!DECL_ARTIFICIAL (decl) && !DECL_ARTIFICIAL (bval)))
      && !is_overloaded_fn (decl))
    error ("redeclaration of %q#D", decl);
  else
    error ("%q#D conflicts with a previous declaration", decl);

  inform (location_of (bval), "previous declaration %q#D", bval);
}

/* Wrapper for supplement_binding_1.  */

static bool
supplement_binding (cxx_binding *binding, tree decl)
{
  bool ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = supplement_binding_1 (binding, decl);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

/* Add DECL to the list of things declared in B.  */

static void
add_decl_to_level (tree decl, cp_binding_level *b)
{
  /* We used to record virtual tables as if they were ordinary
     variables, but no longer do so.  */
  gcc_assert (!(VAR_P (decl) && DECL_VIRTUAL_P (decl)));

  if (TREE_CODE (decl) == NAMESPACE_DECL
      && !DECL_NAMESPACE_ALIAS (decl))
    {
      DECL_CHAIN (decl) = b->namespaces;
      b->namespaces = decl;
    }
  else
    {
      /* We build up the list in reverse order, and reverse it later if
	 necessary.  */
      TREE_CHAIN (decl) = b->names;
      b->names = decl;

      /* If appropriate, add decl to separate list of statics.  We
	 include extern variables because they might turn out to be
	 static later.  It's OK for this list to contain a few false
	 positives.  */
      if (b->kind == sk_namespace)
	if ((VAR_P (decl)
	     && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
	    || (TREE_CODE (decl) == FUNCTION_DECL
		&& (!TREE_PUBLIC (decl)
		    || decl_anon_ns_mem_p (decl)
		    || DECL_DECLARED_INLINE_P (decl))))
	  vec_safe_push (b->static_decls, decl);
    }
}

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).  IS_FRIEND is true if X is
   declared as a friend.

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says.  */

static tree
pushdecl_maybe_friend_1 (tree x, bool is_friend)
{
  tree t;
  tree name;
  int need_new_binding;

  if (x == error_mark_node)
    return error_mark_node;

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
	  && !(VAR_P (x) && DECL_EXTERNAL (x))
	  /* When parsing the parameter list of a function declarator,
	     don't set DECL_CONTEXT to an enclosing function.  When we
	     push the PARM_DECLs in order to process the function body,
	     current_binding_level->this_entity will be set.  */
	  && !(TREE_CODE (x) == PARM_DECL
	       && current_binding_level->kind == sk_function_parms
	       && current_binding_level->this_entity == NULL)
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
	t = lookup_name_innermost_nonclass_level (name);

      /* [basic.link] If there is a visible declaration of an entity
	 with linkage having the same name and type, ignoring entities
	 declared outside the innermost enclosing namespace scope, the
	 block scope declaration declares that same entity and
	 receives the linkage of the previous declaration.  */
      if (! t && current_function_decl && x != current_function_decl
	  && VAR_OR_FUNCTION_DECL_P (x)
	  && DECL_EXTERNAL (x))
	{
	  /* Look in block scope.  */
	  t = innermost_non_namespace_value (name);
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

      if (t && t != error_mark_node)
	{
	  if (different_binding_level)
	    {
	      if (decls_match (x, t))
		/* The standard only says that the local extern
		   inherits linkage from the previous decl; in
		   particular, default args are not shared.  Add
		   the decl into a hash table to make sure only
		   the previous decl in this case is seen by the
		   middle end.  */
		{
		  struct cxx_int_tree_map *h;

		  TREE_PUBLIC (x) = TREE_PUBLIC (t);

		  if (cp_function_chain->extern_decl_map == NULL)
		    cp_function_chain->extern_decl_map
		      = hash_table<cxx_int_tree_map_hasher>::create_ggc (20);

		  h = ggc_alloc<cxx_int_tree_map> ();
		  h->uid = DECL_UID (x);
		  h->to = t;
		  cxx_int_tree_map **loc = cp_function_chain->extern_decl_map
		    ->find_slot (h, INSERT);
		  *loc = h;
		}
	    }
	  else if (TREE_CODE (t) == PARM_DECL)
	    {
	      /* Check for duplicate params.  */
	      tree d = duplicate_decls (x, t, is_friend);
	      if (d)
		return d;
	    }
	  else if ((DECL_EXTERN_C_FUNCTION_P (x)
		    || DECL_FUNCTION_TEMPLATE_P (x))
		   && is_overloaded_fn (t))
	    /* Don't do anything just yet.  */;
	  else if (t == wchar_decl_node)
	    {
	      if (! DECL_IN_SYSTEM_HEADER (x))
		pedwarn (input_location, OPT_Wpedantic, "redeclaration of %<wchar_t%> as %qT",
			 TREE_TYPE (x));
	      
	      /* Throw away the redeclaration.  */
	      return t;
	    }
	  else
	    {
	      tree olddecl = duplicate_decls (x, t, is_friend);

	      /* If the redeclaration failed, we can stop at this
		 point.  */
	      if (olddecl == error_mark_node)
		return error_mark_node;

	      if (olddecl)
		{
		  if (TREE_CODE (t) == TYPE_DECL)
		    SET_IDENTIFIER_TYPE_VALUE (name, TREE_TYPE (t));

		  return t;
		}
	      else if (DECL_MAIN_P (x) && TREE_CODE (t) == FUNCTION_DECL)
		{
		  /* A redeclaration of main, but not a duplicate of the
		     previous one.

		     [basic.start.main]

		     This function shall not be overloaded.  */
		  error ("invalid redeclaration of %q+D", t);
		  error ("as %qD", x);
		  /* We don't try to push this declaration since that
		     causes a crash.  */
		  return x;
		}
	    }
	}

      /* If x has C linkage-specification, (extern "C"),
	 lookup its binding, in case it's already bound to an object.
	 The lookup is done in all namespaces.
	 If we find an existing binding, make sure it has the same
	 exception specification as x, otherwise, bail in error [7.5, 7.6].  */
      if ((TREE_CODE (x) == FUNCTION_DECL)
	  && DECL_EXTERN_C_P (x)
          /* We should ignore declarations happening in system headers.  */
	  && !DECL_ARTIFICIAL (x)
	  && !DECL_IN_SYSTEM_HEADER (x))
	{
	  tree previous = lookup_extern_c_fun_in_all_ns (x);
	  if (previous
	      && !DECL_ARTIFICIAL (previous)
              && !DECL_IN_SYSTEM_HEADER (previous)
	      && DECL_CONTEXT (previous) != DECL_CONTEXT (x))
	    {
	      /* In case either x or previous is declared to throw an exception,
	         make sure both exception specifications are equal.  */
	      if (decls_match (x, previous))
		{
		  tree x_exception_spec = NULL_TREE;
		  tree previous_exception_spec = NULL_TREE;

		  x_exception_spec =
				TYPE_RAISES_EXCEPTIONS (TREE_TYPE (x));
		  previous_exception_spec =
				TYPE_RAISES_EXCEPTIONS (TREE_TYPE (previous));
		  if (!comp_except_specs (previous_exception_spec,
					  x_exception_spec,
					  ce_normal))
		    {
		      pedwarn (input_location, 0,
                               "declaration of %q#D with C language linkage",
			       x);
		      pedwarn (DECL_SOURCE_LOCATION (previous), 0,
                               "conflicts with previous declaration %q#D",
			       previous);
		      pedwarn (input_location, 0,
                               "due to different exception specifications");
		      return error_mark_node;
		    }
		  if (DECL_ASSEMBLER_NAME_SET_P (previous))
		    SET_DECL_ASSEMBLER_NAME (x,
					     DECL_ASSEMBLER_NAME (previous));
		}
	      else
		{
		  pedwarn (input_location, 0,
			   "declaration of %q#D with C language linkage", x);
		  pedwarn (DECL_SOURCE_LOCATION (previous), 0,
			   "conflicts with previous declaration %q#D",
			   previous);
		}
	    }
	}

      check_template_shadow (x);

      /* If this is a function conjured up by the back end, massage it
	 so it looks friendly.  */
      if (DECL_NON_THUNK_FUNCTION_P (x) && ! DECL_LANG_SPECIFIC (x))
	{
	  retrofit_lang_decl (x);
	  SET_DECL_LANGUAGE (x, lang_c);
	}

      t = x;
      if (DECL_NON_THUNK_FUNCTION_P (x) && ! DECL_FUNCTION_MEMBER_P (x))
	{
	  t = push_overloaded_decl (x, PUSH_LOCAL, is_friend);
	  if (!namespace_bindings_p ())
	    /* We do not need to create a binding for this name;
	       push_overloaded_decl will have already done so if
	       necessary.  */
	    need_new_binding = 0;
	}
      else if (DECL_FUNCTION_TEMPLATE_P (x) && DECL_NAMESPACE_SCOPE_P (x))
	{
	  t = push_overloaded_decl (x, PUSH_GLOBAL, is_friend);
	  if (t == x)
	    add_decl_to_level (x, NAMESPACE_LEVEL (CP_DECL_CONTEXT (t)));
	}

      if (DECL_DECLARES_FUNCTION_P (t))
	{
	  check_default_args (t);

	  if (is_friend && t == x && !flag_friend_injection)
	    {
	      /* This is a new friend declaration of a function or a
		 function template, so hide it from ordinary function
		 lookup.  */
	      DECL_ANTICIPATED (t) = 1;
	      DECL_HIDDEN_FRIEND_P (t) = 1;
	    }
	}

      if (t != x || DECL_FUNCTION_TEMPLATE_P (t))
	return t;

      /* If declaring a type as a typedef, copy the type (unless we're
	 at line 0), and install this TYPE_DECL as the new type's typedef
	 name.  See the extensive comment of set_underlying_type ().  */
      if (TREE_CODE (x) == TYPE_DECL)
	{
	  tree type = TREE_TYPE (x);

	  if (DECL_IS_BUILTIN (x)
	      || (TREE_TYPE (x) != error_mark_node
		  && TYPE_NAME (type) != x
		  /* We don't want to copy the type when all we're
		     doing is making a TYPE_DECL for the purposes of
		     inlining.  */
		  && (!TYPE_NAME (type)
		      || TYPE_NAME (type) != DECL_ABSTRACT_ORIGIN (x))))
	    set_underlying_type (x);

	  if (type != error_mark_node
	      && TYPE_IDENTIFIER (type))
	    set_identifier_type_value (DECL_NAME (x), x);

	  /* If this is a locally defined typedef in a function that
	     is not a template instantation, record it to implement
	     -Wunused-local-typedefs.  */
	  if (!instantiating_current_function_p ())
	    record_locally_defined_typedef (x);
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
	      && !comptypes (TREE_TYPE (x), TREE_TYPE (decl),
			     COMPARE_REDECLARATION))
	    {
	      if (permerror (input_location, "type mismatch with previous "
			     "external decl of %q#D", x))
		inform (DECL_SOURCE_LOCATION (decl),
			"previous external decl of %q#D", decl);
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
		  || VAR_P (x)
		  || TREE_CODE (x) == NAMESPACE_DECL
		  || TREE_CODE (x) == CONST_DECL
		  || TREE_CODE (x) == TEMPLATE_DECL))
	    SET_IDENTIFIER_NAMESPACE_VALUE (name, x);

	  /* If new decl is `static' and an `extern' was seen previously,
	     warn about it.  */
	  if (x != NULL_TREE && t != NULL_TREE && decls_match (x, t))
	    warn_extern_redeclared_static (x, t);
	}
      else
	{
	  /* Here to install a non-global value.  */
	  tree oldglobal = IDENTIFIER_NAMESPACE_VALUE (name);
	  tree oldlocal = NULL_TREE;
	  cp_binding_level *oldscope = NULL;
	  cxx_binding *oldbinding = outer_binding (name, NULL, true);
	  if (oldbinding)
	    {
	      oldlocal = oldbinding->value;
	      oldscope = oldbinding->scope;
	    }

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
		     && VAR_P (oldlocal)
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
		  warning (0, "extern declaration of %q#D doesn%'t match", x);
		  warning_at (DECL_SOURCE_LOCATION (oldglobal), 0,
			      "global declaration %q#D", oldglobal);
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

	  /* Don't complain about the parms we push and then pop
	     while tentatively parsing a function declarator.  */
	  if (TREE_CODE (x) == PARM_DECL && DECL_CONTEXT (x) == NULL_TREE)
	    /* Ignore.  */;

	  /* Warn if shadowing an argument at the top level of the body.  */
	  else if (oldlocal != NULL_TREE && !DECL_EXTERNAL (x)
		   /* Inline decls shadow nothing.  */
		   && !DECL_FROM_INLINE (x)
		   && (TREE_CODE (oldlocal) == PARM_DECL
		       || VAR_P (oldlocal)
                       /* If the old decl is a type decl, only warn if the
                          old decl is an explicit typedef or if both the old
                          and new decls are type decls.  */
                       || (TREE_CODE (oldlocal) == TYPE_DECL
                           && (!DECL_ARTIFICIAL (oldlocal)
                               || TREE_CODE (x) == TYPE_DECL)))
                   /* Don't check for internally generated vars unless
                      it's an implicit typedef (see create_implicit_typedef
                      in decl.c).  */
		   && (!DECL_ARTIFICIAL (x) || DECL_IMPLICIT_TYPEDEF_P (x)))
	    {
	      bool nowarn = false;

	      /* Don't complain if it's from an enclosing function.  */
	      if (DECL_CONTEXT (oldlocal) == current_function_decl
		  && TREE_CODE (x) != PARM_DECL
		  && TREE_CODE (oldlocal) == PARM_DECL)
		{
		  /* Go to where the parms should be and see if we find
		     them there.  */
		  cp_binding_level *b = current_binding_level->level_chain;

		  if (FUNCTION_NEEDS_BODY_BLOCK (current_function_decl))
		    /* Skip the ctor/dtor cleanup level.  */
		    b = b->level_chain;

		  /* ARM $8.3 */
		  if (b->kind == sk_function_parms)
		    {
		      error ("declaration of %q#D shadows a parameter", x);
		      nowarn = true;
		    }
		}

	      /* The local structure or class can't use parameters of
		 the containing function anyway.  */
	      if (DECL_CONTEXT (oldlocal) != current_function_decl)
		{
		  cp_binding_level *scope = current_binding_level;
		  tree context = DECL_CONTEXT (oldlocal);
		  for (; scope; scope = scope->level_chain)
		   {
		     if (scope->kind == sk_function_parms
			 && scope->this_entity == context)
		      break;
		     if (scope->kind == sk_class
			 && !LAMBDA_TYPE_P (scope->this_entity))
		       {
			 nowarn = true;
			 break;
		       }
		   }
		}
	      /* Error if redeclaring a local declared in a
		 for-init-statement or in the condition of an if or
		 switch statement when the new declaration is in the
		 outermost block of the controlled statement.
		 Redeclaring a variable from a for or while condition is
		 detected elsewhere.  */
	      else if (VAR_P (oldlocal)
		       && oldscope == current_binding_level->level_chain
		       && (oldscope->kind == sk_cond
			   || oldscope->kind == sk_for))
		{
		  error ("redeclaration of %q#D", x);
		  inform (DECL_SOURCE_LOCATION (oldlocal),
			  "%q#D previously declared here", oldlocal);
		  nowarn = true;
		}
	      /* C++11:
		 3.3.3/3:  The name declared in an exception-declaration (...)
		 shall not be redeclared in the outermost block of the handler.
		 3.3.3/2:  A parameter name shall not be redeclared (...) in
		 the outermost block of any handler associated with a
		 function-try-block.
		 3.4.1/15: The function parameter names shall not be redeclared
		 in the exception-declaration nor in the outermost block of a
		 handler for the function-try-block.  */
	      else if ((VAR_P (oldlocal)
			&& oldscope == current_binding_level->level_chain
			&& oldscope->kind == sk_catch)
		       || (TREE_CODE (oldlocal) == PARM_DECL
			   && (current_binding_level->kind == sk_catch
			       || (current_binding_level->level_chain->kind
				   == sk_catch))
			   && in_function_try_handler))
		{
		  if (permerror (input_location, "redeclaration of %q#D", x))
		    inform (DECL_SOURCE_LOCATION (oldlocal),
			    "%q#D previously declared here", oldlocal);
		  nowarn = true;
		}

	      if (warn_shadow && !nowarn)
		{
		  bool warned;

		  if (TREE_CODE (oldlocal) == PARM_DECL)
		    warned = warning_at (input_location, OPT_Wshadow,
				"declaration of %q#D shadows a parameter", x);
		  else if (is_capture_proxy (oldlocal))
		    warned = warning_at (input_location, OPT_Wshadow,
				"declaration of %qD shadows a lambda capture",
				x);
		  else
		    warned = warning_at (input_location, OPT_Wshadow,
				"declaration of %qD shadows a previous local",
				x);

		  if (warned)
		    inform (DECL_SOURCE_LOCATION (oldlocal),
			    "shadowed declaration is here");
		}
	    }

	  /* Maybe warn if shadowing something else.  */
	  else if (warn_shadow && !DECL_EXTERNAL (x)
                   /* No shadow warnings for internally generated vars unless
                      it's an implicit typedef (see create_implicit_typedef
                      in decl.c).  */
                   && (! DECL_ARTIFICIAL (x) || DECL_IMPLICIT_TYPEDEF_P (x))
                   /* No shadow warnings for vars made for inlining.  */
                   && ! DECL_FROM_INLINE (x))
	    {
	      tree member;

	      if (nonlambda_method_basetype ())
		member = lookup_member (current_nonlambda_class_type (),
					name,
					/*protect=*/0,
					/*want_type=*/false,
					tf_warning_or_error);
	      else
		member = NULL_TREE;

	      if (member && !TREE_STATIC (member))
		{
		  if (BASELINK_P (member))
		    member = BASELINK_FUNCTIONS (member);
		  member = OVL_CURRENT (member);
	
		  /* Do not warn if a variable shadows a function, unless
		     the variable is a function or a pointer-to-function.  */
		  if (TREE_CODE (member) != FUNCTION_DECL
		      || TREE_CODE (x) == FUNCTION_DECL
		      || TYPE_PTRFN_P (TREE_TYPE (x))
		      || TYPE_PTRMEMFUNC_P (TREE_TYPE (x)))
		    {
		      if (warning_at (input_location, OPT_Wshadow,
				      "declaration of %qD shadows a member of %qT",
				      x, current_nonlambda_class_type ())
			  && DECL_P (member))
			inform (DECL_SOURCE_LOCATION (member),
				"shadowed declaration is here");
		    }
		}
	      else if (oldglobal != NULL_TREE
		       && (VAR_P (oldglobal)
                           /* If the old decl is a type decl, only warn if the
                              old decl is an explicit typedef or if both the
                              old and new decls are type decls.  */
                           || (TREE_CODE (oldglobal) == TYPE_DECL
                               && (!DECL_ARTIFICIAL (oldglobal)
                                   || TREE_CODE (x) == TYPE_DECL)))
		       && !instantiating_current_function_p ())
		/* XXX shadow warnings in outer-more namespaces */
		{
		  if (warning_at (input_location, OPT_Wshadow,
				  "declaration of %qD shadows a "
				  "global declaration", x))
		    inform (DECL_SOURCE_LOCATION (oldglobal),
			    "shadowed declaration is here");
		}
	    }
	}

      if (VAR_P (x))
	maybe_register_incomplete_var (x);
    }

  if (need_new_binding)
    add_decl_to_level (x,
		       DECL_NAMESPACE_SCOPE_P (x)
		       ? NAMESPACE_LEVEL (CP_DECL_CONTEXT (x))
		       : current_binding_level);

  return x;
}

/* Wrapper for pushdecl_maybe_friend_1.  */

tree
pushdecl_maybe_friend (tree x, bool is_friend)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = pushdecl_maybe_friend_1 (x, is_friend);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

/* Record a decl-node X as belonging to the current lexical scope.  */

tree
pushdecl (tree x)
{
  return pushdecl_maybe_friend (x, false);
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
	  && !DECL_NAMESPACE_SCOPE_P (decl))
      || (TREE_CODE (decl) == TEMPLATE_DECL && !namespace_bindings_p ())
      || type == unknown_type_node
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
  cp_binding_level *b;

  /* Skip over any local classes.  This makes sense if we call
     push_local_binding with a friend decl of a local class.  */
  b = innermost_nonclass_level ();

  if (lookup_name_innermost_nonclass_level (id))
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
  if (!(VAR_P (decl) && DECL_DEAD_FOR_LOCAL (decl)))
    return decl;

  shadowed = DECL_HAS_SHADOWED_FOR_VAR_P (decl)
    ? DECL_SHADOWED_FOR_VAR (decl) : NULL_TREE ;
  while (shadowed != NULL_TREE && VAR_P (shadowed)
	 && DECL_DEAD_FOR_LOCAL (shadowed))
    shadowed = DECL_HAS_SHADOWED_FOR_VAR_P (shadowed)
      ? DECL_SHADOWED_FOR_VAR (shadowed) : NULL_TREE;
  if (!shadowed)
    shadowed = IDENTIFIER_NAMESPACE_VALUE (DECL_NAME (decl));
  if (shadowed)
    {
      if (!DECL_ERROR_REPORTED (decl))
	{
	  warning (0, "name lookup of %qD changed", DECL_NAME (decl));
	  warning_at (DECL_SOURCE_LOCATION (shadowed), 0,
		      "  matches this %qD under ISO standard rules",
		      shadowed);
	  warning_at (DECL_SOURCE_LOCATION (decl), 0,
		      "  matches this %qD under old rules", decl);
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
      error ("name lookup of %qD changed for ISO %<for%> scoping",
	     DECL_NAME (decl));
      error ("  cannot use obsolete binding at %q+D because "
	     "it has a destructor", decl);
      return error_mark_node;
    }
  else
    {
      permerror (input_location, "name lookup of %qD changed for ISO %<for%> scoping",
	         DECL_NAME (decl));
      if (flag_permissive)
        permerror (DECL_SOURCE_LOCATION (decl),
		   "  using obsolete binding at %qD", decl);
      else
	{
	  static bool hint;
	  if (!hint)
	    {
	      inform (input_location, "(if you use %<-fpermissive%> G++ will accept your code)");
	      hint = true;
	    }
	}
    }

  return decl;
}

/* true means unconditionally make a BLOCK for the next level pushed.  */

static bool keep_next_level_flag;

static int binding_depth = 0;

static void
indent (int depth)
{
  int i;

  for (i = 0; i < depth * 2; i++)
    putc (' ', stderr);
}

/* Return a string describing the kind of SCOPE we have.  */
static const char *
cp_binding_level_descriptor (cp_binding_level *scope)
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
cp_binding_level_debug (cp_binding_level *scope, int line, const char *action)
{
  const char *desc = cp_binding_level_descriptor (scope);
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

static GTY((deletable)) cp_binding_level *free_binding_level;

/* Insert SCOPE as the innermost binding level.  */

void
push_binding_level (cp_binding_level *scope)
{
  /* Add it to the front of currently active scopes stack.  */
  scope->level_chain = current_binding_level;
  current_binding_level = scope;
  keep_next_level_flag = false;

  if (ENABLE_SCOPE_CHECKING)
    {
      scope->binding_depth = binding_depth;
      indent (binding_depth);
      cp_binding_level_debug (scope, LOCATION_LINE (input_location),
			      "push");
      binding_depth++;
    }
}

/* Create a new KIND scope and make it the top of the active scopes stack.
   ENTITY is the scope of the associated C++ entity (namespace, class,
   function, C++0x enumeration); it is NULL otherwise.  */

cp_binding_level *
begin_scope (scope_kind kind, tree entity)
{
  cp_binding_level *scope;

  /* Reuse or create a struct for this binding level.  */
  if (!ENABLE_SCOPE_CHECKING && free_binding_level)
    {
      scope = free_binding_level;
      free_binding_level = scope->level_chain;
      memset (scope, 0, sizeof (cp_binding_level));
    }
  else
    scope = ggc_cleared_alloc<cp_binding_level> ();

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
    case sk_cond:
    case sk_class:
    case sk_scoped_enum:
    case sk_function_parms:
    case sk_transaction:
    case sk_omp:
      scope->keep = keep_next_level_flag;
      break;

    case sk_namespace:
      NAMESPACE_LEVEL (entity) = scope;
      vec_alloc (scope->static_decls,
		 (DECL_NAME (entity) == std_identifier
		  || DECL_NAME (entity) == global_scope_name) ? 200 : 10);
      break;

    default:
      /* Should not happen.  */
      gcc_unreachable ();
      break;
    }
  scope->kind = kind;

  push_binding_level (scope);

  return scope;
}

/* We're about to leave current scope.  Pop the top of the stack of
   currently active scopes.  Return the enclosing scope, now active.  */

cp_binding_level *
leave_scope (void)
{
  cp_binding_level *scope = current_binding_level;

  if (scope->kind == sk_namespace && class_binding_level)
    current_binding_level = class_binding_level;

  /* We cannot leave a scope, if there are none left.  */
  if (NAMESPACE_LEVEL (global_namespace))
    gcc_assert (!global_scope_p (scope));

  if (ENABLE_SCOPE_CHECKING)
    {
      indent (--binding_depth);
      cp_binding_level_debug (scope, LOCATION_LINE (input_location),
			      "leave");
    }

  /* Move one nesting level up.  */
  current_binding_level = scope->level_chain;

  /* Namespace-scopes are left most probably temporarily, not
     completely; they can be reopened later, e.g. in namespace-extension
     or any name binding activity that requires us to resume a
     namespace.  For classes, we cache some binding levels.  For other
     scopes, we just make the structure available for reuse.  */
  if (scope->kind != sk_namespace
      && scope->kind != sk_class)
    {
      scope->level_chain = free_binding_level;
      gcc_assert (!ENABLE_SCOPE_CHECKING
		  || scope->binding_depth == binding_depth);
      free_binding_level = scope;
    }

  if (scope->kind == sk_class)
    {
      /* Reset DEFINING_CLASS_P to allow for reuse of a
	 class-defining scope in a non-defining context.  */
      scope->defining_class_p = 0;

      /* Find the innermost enclosing class scope, and reset
	 CLASS_BINDING_LEVEL appropriately.  */
      class_binding_level = NULL;
      for (scope = current_binding_level; scope; scope = scope->level_chain)
	if (scope->kind == sk_class)
	  {
	    class_binding_level = scope;
	    break;
	  }
    }

  return current_binding_level;
}

static void
resume_scope (cp_binding_level* b)
{
  /* Resuming binding levels is meant only for namespaces,
     and those cannot nest into classes.  */
  gcc_assert (!class_binding_level);
  /* Also, resuming a non-directly nested namespace is a no-no.  */
  gcc_assert (b->level_chain == current_binding_level);
  current_binding_level = b;
  if (ENABLE_SCOPE_CHECKING)
    {
      b->binding_depth = binding_depth;
      indent (binding_depth);
      cp_binding_level_debug (b, LOCATION_LINE (input_location), "resume");
      binding_depth++;
    }
}

/* Return the innermost binding level that is not for a class scope.  */

static cp_binding_level *
innermost_nonclass_level (void)
{
  cp_binding_level *b;

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
      current_binding_level->statement_list = push_stmt_list ();
    }
}

/* Return true if we are in the global binding level.  */

bool
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
  cp_binding_level *b = innermost_nonclass_level ();

  return b->kind == sk_namespace || b->kind == sk_template_parms;
}

/* True if this is a namespace scope, or if we are defining a class
   which is itself at namespace scope, or whose enclosing class is
   such a class, etc.  */

bool
namespace_bindings_p (void)
{
  cp_binding_level *b = innermost_nonclass_level ();

  return b->kind == sk_namespace;
}

/* True if the innermost non-class scope is a block scope.  */

bool
local_bindings_p (void)
{
  cp_binding_level *b = innermost_nonclass_level ();
  return b->kind < sk_function_parms || b->kind == sk_omp;
}

/* True if the current level needs to have a BLOCK made.  */

bool
kept_level_p (void)
{
  return (current_binding_level->blocks != NULL_TREE
	  || current_binding_level->keep
	  || current_binding_level->kind == sk_cleanup
	  || current_binding_level->names != NULL_TREE
	  || current_binding_level->using_directives);
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

/* Return how many function prototypes we are currently nested inside.  */

int
function_parm_depth (void)
{
  int level = 0;
  cp_binding_level *b;

  for (b = current_binding_level;
       b->kind == sk_function_parms;
       b = b->level_chain)
    ++level;

  return level;
}

/* For debugging.  */
static int no_print_functions = 0;
static int no_print_builtins = 0;

static void
print_binding_level (cp_binding_level* lvl)
{
  tree t;
  int i = 0, len;
  fprintf (stderr, " blocks=%p", (void *) lvl->blocks);
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
	      && DECL_IS_BUILTIN (t))
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
  if (vec_safe_length (lvl->class_shadowed))
    {
      size_t i;
      cp_class_binding *b;
      fprintf (stderr, " class-shadowed:");
      FOR_EACH_VEC_ELT (*lvl->class_shadowed, i, b)
	fprintf (stderr, " %s ", IDENTIFIER_POINTER (b->identifier));
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

DEBUG_FUNCTION void
debug (cp_binding_level &ref)
{
  print_binding_level (&ref);
}

DEBUG_FUNCTION void
debug (cp_binding_level *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


void
print_other_binding_stack (cp_binding_level *stack)
{
  cp_binding_level *level;
  for (level = stack; !global_scope_p (level); level = level->level_chain)
    {
      fprintf (stderr, "binding level %p\n", (void *) level);
      print_binding_level (level);
    }
}

void
print_binding_stack (void)
{
  cp_binding_level *b;
  fprintf (stderr, "current_binding_level=%p\n"
	   "class_binding_level=%p\n"
	   "NAMESPACE_LEVEL (global_namespace)=%p\n",
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

/* Return the type associated with ID.  */

static tree
identifier_type_value_1 (tree id)
{
  /* There is no type with that name, anywhere.  */
  if (REAL_IDENTIFIER_TYPE_VALUE (id) == NULL_TREE)
    return NULL_TREE;
  /* This is not the type marker, but the real thing.  */
  if (REAL_IDENTIFIER_TYPE_VALUE (id) != global_type_node)
    return REAL_IDENTIFIER_TYPE_VALUE (id);
  /* Have to search for it. It must be on the global level, now.
     Ask lookup_name not to return non-types.  */
  id = lookup_name_real (id, 2, 1, /*block_p=*/true, 0, 0);
  if (id)
    return TREE_TYPE (id);
  return NULL_TREE;
}

/* Wrapper for identifier_type_value_1.  */

tree
identifier_type_value (tree id)
{
  tree ret;
  timevar_start (TV_NAME_LOOKUP);
  ret = identifier_type_value_1 (id);
  timevar_stop (TV_NAME_LOOKUP);
  return ret;
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
set_identifier_type_value_with_scope (tree id, tree decl, cp_binding_level *b)
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
      TREE_TYPE (b->type_shadowed) = type;
    }
  else
    {
      cxx_binding *binding =
	binding_for_name (NAMESPACE_LEVEL (current_namespace), id);
      gcc_assert (decl);
      if (binding->value)
	supplement_binding (binding, decl);
      else
	binding->value = decl;

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

static inline tree
constructor_name_full (tree type)
{
  return TYPE_IDENTIFIER (TYPE_MAIN_VARIANT (type));
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

/* Returns TRUE if NAME is the name for the constructor for TYPE,
   which must be a class type.  */

bool
constructor_name_p (tree name, tree type)
{
  tree ctor_name;

  gcc_assert (MAYBE_CLASS_TYPE_P (type));

  if (!name)
    return false;

  if (!identifier_p (name))
    return false;

  /* These don't have names.  */
  if (TREE_CODE (type) == DECLTYPE_TYPE
      || TREE_CODE (type) == TYPEOF_TYPE)
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

  sprintf (buf, anon_aggrname_format (), anon_cnt++);
  return get_identifier (buf);
}

/* This code is practically identical to that for creating
   anonymous names, but is just used for lambdas instead.  This isn't really
   necessary, but it's convenient to avoid treating lambdas like other
   anonymous types.  */

static GTY(()) int lambda_cnt = 0;

tree
make_lambda_name (void)
{
  char buf[32];

  sprintf (buf, LAMBDANAME_FORMAT, lambda_cnt++);
  return get_identifier (buf);
}

/* Return (from the stack of) the BINDING, if any, established at SCOPE.  */

static inline cxx_binding *
find_binding (cp_binding_level *scope, cxx_binding *binding)
{
  for (; binding != NULL; binding = binding->previous)
    if (binding->scope == scope)
      return binding;

  return (cxx_binding *)0;
}

/* Return the binding for NAME in SCOPE, if any.  Otherwise, return NULL.  */

static inline cxx_binding *
cp_binding_level_find_binding_for_name (cp_binding_level *scope, tree name)
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
binding_for_name (cp_binding_level *scope, tree name)
{
  cxx_binding *result;

  result = cp_binding_level_find_binding_for_name (scope, name);
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

/* Walk through the bindings associated to the name of FUNCTION,
   and return the first declaration of a function with a
   "C" linkage specification, a.k.a 'extern "C"'.
   This function looks for the binding, regardless of which scope it
   has been defined in. It basically looks in all the known scopes.
   Note that this function does not lookup for bindings of builtin functions
   or for functions declared in system headers.  */
static tree
lookup_extern_c_fun_in_all_ns (tree function)
{
  tree name;
  cxx_binding *iter;

  gcc_assert (function && TREE_CODE (function) == FUNCTION_DECL);

  name = DECL_NAME (function);
  gcc_assert (name && identifier_p (name));

  for (iter = IDENTIFIER_NAMESPACE_BINDINGS (name);
       iter;
       iter = iter->previous)
    {
      tree ovl;
      for (ovl = iter->value; ovl; ovl = OVL_NEXT (ovl))
	{
	  tree decl = OVL_CURRENT (ovl);
	  if (decl
	      && TREE_CODE (decl) == FUNCTION_DECL
	      && DECL_EXTERN_C_P (decl)
	      && !DECL_ARTIFICIAL (decl))
	    {
	      return decl;
	    }
	}
    }
  return NULL;
}

/* Returns a list of C-linkage decls with the name NAME.  */

tree
c_linkage_bindings (tree name)
{
  tree decls = NULL_TREE;
  cxx_binding *iter;

  for (iter = IDENTIFIER_NAMESPACE_BINDINGS (name);
       iter;
       iter = iter->previous)
    {
      tree ovl;
      for (ovl = iter->value; ovl; ovl = OVL_NEXT (ovl))
	{
	  tree decl = OVL_CURRENT (ovl);
	  if (decl
	      && DECL_EXTERN_C_P (decl)
	      && !DECL_ARTIFICIAL (decl))
	    {
	      if (decls == NULL_TREE)
		decls = decl;
	      else
		decls = tree_cons (NULL_TREE, decl, decls);
	    }
	}
    }
  return decls;
}

/* Insert another USING_DECL into the current binding level, returning
   this declaration. If this is a redeclaration, do nothing, and
   return NULL_TREE if this not in namespace scope (in namespace
   scope, a using decl might extend any previous bindings).  */

static tree
push_using_decl_1 (tree scope, tree name)
{
  tree decl;

  gcc_assert (TREE_CODE (scope) == NAMESPACE_DECL);
  gcc_assert (identifier_p (name));
  for (decl = current_binding_level->usings; decl; decl = DECL_CHAIN (decl))
    if (USING_DECL_SCOPE (decl) == scope && DECL_NAME (decl) == name)
      break;
  if (decl)
    return namespace_bindings_p () ? decl : NULL_TREE;
  decl = build_lang_decl (USING_DECL, name, NULL_TREE);
  USING_DECL_SCOPE (decl) = scope;
  DECL_CHAIN (decl) = current_binding_level->usings;
  current_binding_level->usings = decl;
  return decl;
}

/* Wrapper for push_using_decl_1.  */

static tree
push_using_decl (tree scope, tree name)
{
  tree ret;
  timevar_start (TV_NAME_LOOKUP);
  ret = push_using_decl_1 (scope, name);
  timevar_stop (TV_NAME_LOOKUP);
  return ret;
}

/* Same as pushdecl, but define X in binding-level LEVEL.  We rely on the
   caller to set DECL_CONTEXT properly.

   Note that this must only be used when X will be the new innermost
   binding for its name, as we tack it onto the front of IDENTIFIER_BINDING
   without checking to see if the current IDENTIFIER_BINDING comes from a
   closer binding level than LEVEL.  */

static tree
pushdecl_with_scope_1 (tree x, cp_binding_level *level, bool is_friend)
{
  cp_binding_level *b;
  tree function_decl = current_function_decl;

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
      x = pushdecl_maybe_friend (x, is_friend);
      current_binding_level = b;
    }
  current_function_decl = function_decl;
  return x;
}
 
/* Wrapper for pushdecl_with_scope_1.  */

tree
pushdecl_with_scope (tree x, cp_binding_level *level, bool is_friend)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = pushdecl_with_scope_1 (x, level, is_friend);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

/* Helper function for push_overloaded_decl_1 and do_nonmember_using_decl.
   Compares the parameter-type-lists of DECL1 and DECL2 and returns false
   if they are different.  If the DECLs are template functions, the return
   types and the template parameter lists are compared too (DR 565).  */

static bool
compparms_for_decl_and_using_decl (tree decl1, tree decl2)
{
  if (!compparms (TYPE_ARG_TYPES (TREE_TYPE (decl1)),
		  TYPE_ARG_TYPES (TREE_TYPE (decl2))))
    return false;

  if (! DECL_FUNCTION_TEMPLATE_P (decl1)
      || ! DECL_FUNCTION_TEMPLATE_P (decl2))
    return true;

  return (comp_template_parms (DECL_TEMPLATE_PARMS (decl1),
			       DECL_TEMPLATE_PARMS (decl2))
	  && same_type_p (TREE_TYPE (TREE_TYPE (decl1)),
			  TREE_TYPE (TREE_TYPE (decl2))));
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

   IS_FRIEND is true if this is a friend declaration.

   The value returned may be a previous declaration if we guessed wrong
   about what language DECL should belong to (C or C++).  Otherwise,
   it's always DECL (and never something that's not a _DECL).  */

static tree
push_overloaded_decl_1 (tree decl, int flags, bool is_friend)
{
  tree name = DECL_NAME (decl);
  tree old;
  tree new_binding;
  int doing_global = (namespace_bindings_p () || !(flags & PUSH_LOCAL));

  if (doing_global)
    old = namespace_binding (name, DECL_CONTEXT (decl));
  else
    old = lookup_name_innermost_nonclass_level (name);

  if (old)
    {
      if (TREE_CODE (old) == TYPE_DECL && DECL_ARTIFICIAL (old))
	{
	  tree t = TREE_TYPE (old);
	  if (MAYBE_CLASS_TYPE_P (t) && warn_shadow
	      && (! DECL_IN_SYSTEM_HEADER (decl)
		  || ! DECL_IN_SYSTEM_HEADER (old)))
	    warning (OPT_Wshadow, "%q#D hides constructor for %q#T", decl, t);
	  old = NULL_TREE;
	}
      else if (is_overloaded_fn (old))
	{
	  tree tmp;

	  for (tmp = old; tmp; tmp = OVL_NEXT (tmp))
	    {
	      tree fn = OVL_CURRENT (tmp);
	      tree dup;

	      if (TREE_CODE (tmp) == OVERLOAD && OVL_USED (tmp)
		  && !(flags & PUSH_USING)
		  && compparms_for_decl_and_using_decl (fn, decl)
		  && ! decls_match (fn, decl))
		diagnose_name_conflict (decl, fn);

	      dup = duplicate_decls (decl, fn, is_friend);
	      /* If DECL was a redeclaration of FN -- even an invalid
		 one -- pass that information along to our caller.  */
	      if (dup == fn || dup == error_mark_node)
		return dup;
	    }

	  /* We don't overload implicit built-ins.  duplicate_decls()
	     may fail to merge the decls if the new decl is e.g. a
	     template function.  */
	  if (TREE_CODE (old) == FUNCTION_DECL
	      && DECL_ANTICIPATED (old)
	      && !DECL_HIDDEN_FRIEND_P (old))
	    old = NULL;
	}
      else if (old == error_mark_node)
	/* Ignore the undefined symbol marker.  */
	old = NULL_TREE;
      else
	{
	  error ("previous non-function declaration %q+#D", old);
	  error ("conflicts with function declaration %q#D", decl);
	  return decl;
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
		return decl;
	      }

	  /* We should always find a previous binding in this case.  */
	  gcc_unreachable ();
	}

      /* Install the new binding.  */
      push_local_binding (name, new_binding, flags);
    }

  return decl;
}

/* Wrapper for push_overloaded_decl_1.  */

static tree
push_overloaded_decl (tree decl, int flags, bool is_friend)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = push_overloaded_decl_1 (decl, flags, is_friend);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

/* Check a non-member using-declaration. Return the name and scope
   being used, and the USING_DECL, or NULL_TREE on failure.  */

static tree
validate_nonmember_using_decl (tree decl, tree scope, tree name)
{
  /* [namespace.udecl]
       A using-declaration for a class member shall be a
       member-declaration.  */
  if (TYPE_P (scope))
    {
      error ("%qT is not a namespace or unscoped enum", scope);
      return NULL_TREE;
    }
  else if (scope == error_mark_node)
    return NULL_TREE;

  if (TREE_CODE (decl) == TEMPLATE_ID_EXPR)
    {
      /* 7.3.3/5
	   A using-declaration shall not name a template-id.  */
      error ("a using-declaration cannot specify a template-id.  "
	     "Try %<using %D%>", name);
      return NULL_TREE;
    }

  if (TREE_CODE (decl) == NAMESPACE_DECL)
    {
      error ("namespace %qD not allowed in using-declaration", decl);
      return NULL_TREE;
    }

  if (TREE_CODE (decl) == SCOPE_REF)
    {
      /* It's a nested name with template parameter dependent scope.
	 This can only be using-declaration for class member.  */
      error ("%qT is not a namespace", TREE_OPERAND (decl, 0));
      return NULL_TREE;
    }

  if (is_overloaded_fn (decl))
    decl = get_first_fn (decl);

  gcc_assert (DECL_P (decl));

  /* Make a USING_DECL.  */
  tree using_decl = push_using_decl (scope, name);

  if (using_decl == NULL_TREE
      && at_function_scope_p ()
      && VAR_P (decl))
    /* C++11 7.3.3/10.  */
    error ("%qD is already declared in this scope", name);
  
  return using_decl;
}

/* Process local and global using-declarations.  */

static void
do_nonmember_using_decl (tree scope, tree name, tree oldval, tree oldtype,
			 tree *newval, tree *newtype)
{
  struct scope_binding decls = EMPTY_SCOPE_BINDING;

  *newval = *newtype = NULL_TREE;
  if (!qualified_lookup_using_namespace (name, scope, &decls, 0))
    /* Lookup error */
    return;

  if (!decls.value && !decls.type)
    {
      error ("%qD not declared", name);
      return;
    }

  /* Shift the old and new bindings around so we're comparing class and
     enumeration names to each other.  */
  if (oldval && DECL_IMPLICIT_TYPEDEF_P (oldval))
    {
      oldtype = oldval;
      oldval = NULL_TREE;
    }

  if (decls.value && DECL_IMPLICIT_TYPEDEF_P (decls.value))
    {
      decls.type = decls.value;
      decls.value = NULL_TREE;
    }

  if (decls.value)
    {
      /* Check for using functions.  */
      if (is_overloaded_fn (decls.value))
	{
	  tree tmp, tmp1;

	  if (oldval && !is_overloaded_fn (oldval))
	    {
	      error ("%qD is already declared in this scope", name);
	      oldval = NULL_TREE;
	    }

	  *newval = oldval;
	  for (tmp = decls.value; tmp; tmp = OVL_NEXT (tmp))
	    {
	      tree new_fn = OVL_CURRENT (tmp);

	      /* Don't import functions that haven't been declared.  */
	      if (DECL_ANTICIPATED (new_fn))
		continue;

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
		  else if (TREE_CODE (tmp1) == OVERLOAD && OVL_USED (tmp1))
		    continue; /* this is a using decl */
		  else if (compparms_for_decl_and_using_decl (new_fn, old_fn))
		    {
		      /* There was already a non-using declaration in
			 this scope with the same parameter types. If both
			 are the same extern "C" functions, that's ok.  */
		      if (DECL_ANTICIPATED (old_fn)
			  && !DECL_HIDDEN_FRIEND_P (old_fn))
			/* Ignore anticipated built-ins.  */;
		      else if (decls_match (new_fn, old_fn))
			break;
		      else
			{
			  diagnose_name_conflict (new_fn, old_fn);
			  break;
			}
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
	  /* If we're declaring a non-function and OLDVAL is an anticipated
	     built-in, just pretend it isn't there.  */
	  if (oldval
	      && TREE_CODE (oldval) == FUNCTION_DECL
	      && DECL_ANTICIPATED (oldval)
	      && !DECL_HIDDEN_FRIEND_P (oldval))
	    oldval = NULL_TREE;

	  *newval = decls.value;
	  if (oldval && !decls_match (*newval, oldval))
	    error ("%qD is already declared in this scope", name);
	}
    }
  else
    *newval = oldval;

  if (decls.type && TREE_CODE (decls.type) == TREE_LIST)
    {
      error ("reference to %qD is ambiguous", name);
      print_candidates (decls.type);
    }
  else
    {
      *newtype = decls.type;
      if (oldtype && *newtype && !decls_match (oldtype, *newtype))
	error ("%qD is already declared in this scope", name);
    }

    /* If *newval is empty, shift any class or enumeration name down.  */
    if (!*newval)
      {
	*newval = *newtype;
	*newtype = NULL_TREE;
      }
}

/* Process a using-declaration at function scope.  */

void
do_local_using_decl (tree decl, tree scope, tree name)
{
  tree oldval, oldtype, newval, newtype;
  tree orig_decl = decl;

  decl = validate_nonmember_using_decl (decl, scope, name);
  if (decl == NULL_TREE)
    return;

  if (building_stmt_list_p ()
      && at_function_scope_p ())
    add_decl_expr (decl);

  oldval = lookup_name_innermost_nonclass_level (name);
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
				  PUSH_LOCAL | PUSH_USING,
				  false);
	}
      else
	push_local_binding (name, newval, PUSH_USING);
    }
  if (newtype)
    {
      push_local_binding (name, newtype, PUSH_USING);
      set_identifier_type_value (name, newtype);
    }

  /* Emit debug info.  */
  if (!processing_template_decl)
    cp_emit_debug_info_for_using (orig_decl, current_scope());
}

/* Returns true if ROOT (a namespace, class, or function) encloses
   CHILD.  CHILD may be either a class type or a namespace.  */

bool
is_ancestor (tree root, tree child)
{
  gcc_assert ((TREE_CODE (root) == NAMESPACE_DECL
	       || TREE_CODE (root) == FUNCTION_DECL
	       || CLASS_TYPE_P (root)));
  gcc_assert ((TREE_CODE (child) == NAMESPACE_DECL
	       || CLASS_TYPE_P (child)));

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

/* Enter the class or namespace scope indicated by T suitable for name
   lookup.  T can be arbitrary scope, not necessary nested inside the
   current scope.  Returns a non-null scope to pop iff pop_scope
   should be called later to exit this scope.  */

tree
push_scope (tree t)
{
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
	t = NULL_TREE;
    }

  return t;
}

/* Leave scope pushed by push_scope.  */

void
pop_scope (tree t)
{
  if (t == NULL_TREE)
    return;
  if (TREE_CODE (t) == NAMESPACE_DECL)
    pop_decl_namespace ();
  else if CLASS_TYPE_P (t)
    pop_nested_class ();
}

/* Subroutine of push_inner_scope.  */

static void
push_inner_scope_r (tree outer, tree inner)
{
  tree prev;

  if (outer == inner
      || (TREE_CODE (inner) != NAMESPACE_DECL && !CLASS_TYPE_P (inner)))
    return;

  prev = CP_DECL_CONTEXT (TREE_CODE (inner) == NAMESPACE_DECL ? inner : TYPE_NAME (inner));
  if (outer != prev)
    push_inner_scope_r (outer, prev);
  if (TREE_CODE (inner) == NAMESPACE_DECL)
    {
      cp_binding_level *save_template_parm = 0;
      /* Temporary take out template parameter scopes.  They are saved
	 in reversed order in save_template_parm.  */
      while (current_binding_level->kind == sk_template_parms)
	{
	  cp_binding_level *b = current_binding_level;
	  current_binding_level = b->level_chain;
	  b->level_chain = save_template_parm;
	  save_template_parm = b;
	}

      resume_scope (NAMESPACE_LEVEL (inner));
      current_namespace = inner;

      /* Restore template parameter scopes.  */
      while (save_template_parm)
	{
	  cp_binding_level *b = save_template_parm;
	  save_template_parm = b->level_chain;
	  b->level_chain = current_binding_level;
	  current_binding_level = b;
	}
    }
  else
    pushclass (inner);
}

/* Enter the scope INNER from current scope.  INNER must be a scope
   nested inside current scope.  This works with both name lookup and
   pushing name into scope.  In case a template parameter scope is present,
   namespace is pushed under the template parameter scope according to
   name lookup rule in 14.6.1/6.

   Return the former current scope suitable for pop_inner_scope.  */

tree
push_inner_scope (tree inner)
{
  tree outer = current_scope ();
  if (!outer)
    outer = current_namespace;

  push_inner_scope_r (outer, inner);
  return outer;
}

/* Exit the current scope INNER back to scope OUTER.  */

void
pop_inner_scope (tree outer, tree inner)
{
  if (outer == inner
      || (TREE_CODE (inner) != NAMESPACE_DECL && !CLASS_TYPE_P (inner)))
    return;

  while (outer != inner)
    {
      if (TREE_CODE (inner) == NAMESPACE_DECL)
	{
	  cp_binding_level *save_template_parm = 0;
	  /* Temporary take out template parameter scopes.  They are saved
	     in reversed order in save_template_parm.  */
	  while (current_binding_level->kind == sk_template_parms)
	    {
	      cp_binding_level *b = current_binding_level;
	      current_binding_level = b->level_chain;
	      b->level_chain = save_template_parm;
	      save_template_parm = b;
	    }

	  pop_namespace ();

	  /* Restore template parameter scopes.  */
	  while (save_template_parm)
	    {
	      cp_binding_level *b = save_template_parm;
	      save_template_parm = b->level_chain;
	      b->level_chain = current_binding_level;
	      current_binding_level = b;
	    }
	}
      else
	popclass ();

      inner = CP_DECL_CONTEXT (TREE_CODE (inner) == NAMESPACE_DECL ? inner : TYPE_NAME (inner));
    }
}

/* Do a pushlevel for class declarations.  */

void
pushlevel_class (void)
{
  class_binding_level = begin_scope (sk_class, current_class_type);
}

/* ...and a poplevel for class declarations.  */

void
poplevel_class (void)
{
  cp_binding_level *level = class_binding_level;
  cp_class_binding *cb;
  size_t i;
  tree shadowed;

  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  gcc_assert (level != 0);

  /* If we're leaving a toplevel class, cache its binding level.  */
  if (current_class_depth == 1)
    previous_class_level = level;
  for (shadowed = level->type_shadowed;
       shadowed;
       shadowed = TREE_CHAIN (shadowed))
    SET_IDENTIFIER_TYPE_VALUE (TREE_PURPOSE (shadowed), TREE_VALUE (shadowed));

  /* Remove the bindings for all of the class-level declarations.  */
  if (level->class_shadowed)
    {
      FOR_EACH_VEC_ELT (*level->class_shadowed, i, cb)
	{
	  IDENTIFIER_BINDING (cb->identifier) = cb->base->previous;
	  cxx_binding_free (cb->base);
	}
      ggc_free (level->class_shadowed);
      level->class_shadowed = NULL;
    }

  /* Now, pop out of the binding level which we created up in the
     `pushlevel_class' routine.  */
  gcc_assert (current_binding_level == level);
  leave_scope ();
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

/* Set INHERITED_VALUE_BINDING_P on BINDING to true or false, as
   appropriate.  DECL is the value to which a name has just been
   bound.  CLASS_TYPE is the class in which the lookup occurred.  */

static void
set_inherited_value_binding_p (cxx_binding *binding, tree decl,
			       tree class_type)
{
  if (binding->value == decl && TREE_CODE (decl) != TREE_LIST)
    {
      tree context;

      if (TREE_CODE (decl) == OVERLOAD)
	context = ovl_scope (decl);
      else
	{
	  gcc_assert (DECL_P (decl));
	  context = context_for_name_lookup (decl);
	}

      if (is_properly_derived_from (class_type, context))
	INHERITED_VALUE_BINDING_P (binding) = 1;
      else
	INHERITED_VALUE_BINDING_P (binding) = 0;
    }
  else if (binding->value == decl)
    /* We only encounter a TREE_LIST when there is an ambiguity in the
       base classes.  Such an ambiguity can be overridden by a
       definition in this class.  */
    INHERITED_VALUE_BINDING_P (binding) = 1;
  else
    INHERITED_VALUE_BINDING_P (binding) = 0;
}

/* Make the declaration of X appear in CLASS scope.  */

bool
pushdecl_class_level (tree x)
{
  tree name;
  bool is_valid = true;
  bool subtime;

  /* Do nothing if we're adding to an outer lambda closure type,
     outer_binding will add it later if it's needed.  */
  if (current_class_type != class_binding_level->this_entity)
    return true;

  subtime = timevar_cond_start (TV_NAME_LOOKUP);
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

      for (f = TYPE_FIELDS (TREE_TYPE (x)); f; f = DECL_CHAIN (f))
	{
	  location_t save_location = input_location;
	  input_location = DECL_SOURCE_LOCATION (f);
	  if (!pushdecl_class_level (f))
	    is_valid = false;
	  input_location = save_location;
	}
    }
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return is_valid;
}

/* Return the BINDING (if any) for NAME in SCOPE, which is a class
   scope.  If the value returned is non-NULL, and the PREVIOUS field
   is not set, callers must set the PREVIOUS field explicitly.  */

static cxx_binding *
get_class_binding (tree name, cp_binding_level *scope)
{
  tree class_type;
  tree type_binding;
  tree value_binding;
  cxx_binding *binding;

  class_type = scope->this_entity;

  /* Get the type binding.  */
  type_binding = lookup_member (class_type, name,
				/*protect=*/2, /*want_type=*/true,
				tf_warning_or_error);
  /* Get the value binding.  */
  value_binding = lookup_member (class_type, name,
				 /*protect=*/2, /*want_type=*/false,
				 tf_warning_or_error);

  if (value_binding
      && (TREE_CODE (value_binding) == TYPE_DECL
	  || DECL_CLASS_TEMPLATE_P (value_binding)
	  || (TREE_CODE (value_binding) == TREE_LIST
	      && TREE_TYPE (value_binding) == error_mark_node
	      && (TREE_CODE (TREE_VALUE (value_binding))
		  == TYPE_DECL))))
    /* We found a type binding, even when looking for a non-type
       binding.  This means that we already processed this binding
       above.  */
    ;
  else if (value_binding)
    {
      if (TREE_CODE (value_binding) == TREE_LIST
	  && TREE_TYPE (value_binding) == error_mark_node)
	/* NAME is ambiguous.  */
	;
      else if (BASELINK_P (value_binding))
	/* NAME is some overloaded functions.  */
	value_binding = BASELINK_FUNCTIONS (value_binding);
    }

  /* If we found either a type binding or a value binding, create a
     new binding object.  */
  if (type_binding || value_binding)
    {
      binding = new_class_binding (name,
				   value_binding,
				   type_binding,
				   scope);
      /* This is a class-scope binding, not a block-scope binding.  */
      LOCAL_BINDING_P (binding) = 0;
      set_inherited_value_binding_p (binding, value_binding, class_type);
    }
  else
    binding = NULL;

  return binding;
}

/* Make the declaration(s) of X appear in CLASS scope under the name
   NAME.  Returns true if the binding is valid.  */

static bool
push_class_level_binding_1 (tree name, tree x)
{
  cxx_binding *binding;
  tree decl = x;
  bool ok;

  /* The class_binding_level will be NULL if x is a template
     parameter name in a member template.  */
  if (!class_binding_level)
    return true;

  if (name == error_mark_node)
    return false;

  /* Can happen for an erroneous declaration (c++/60384).  */
  if (!identifier_p (name))
    {
      gcc_assert (errorcount || sorrycount);
      return false;
    }

  /* Check for invalid member names.  But don't worry about a default
     argument-scope lambda being pushed after the class is complete.  */
  gcc_assert (TYPE_BEING_DEFINED (current_class_type)
	      || LAMBDA_TYPE_P (TREE_TYPE (decl)));
  /* Check that we're pushing into the right binding level.  */
  gcc_assert (current_class_type == class_binding_level->this_entity);

  /* We could have been passed a tree list if this is an ambiguous
     declaration. If so, pull the declaration out because
     check_template_shadow will not handle a TREE_LIST.  */
  if (TREE_CODE (decl) == TREE_LIST
      && TREE_TYPE (decl) == error_mark_node)
    decl = TREE_VALUE (decl);

  if (!check_template_shadow (decl))
    return false;

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
  if ((VAR_P (x)
       || TREE_CODE (x) == CONST_DECL
       || (TREE_CODE (x) == TYPE_DECL
	   && !DECL_SELF_REFERENCE_P (x))
       /* A data member of an anonymous union.  */
       || (TREE_CODE (x) == FIELD_DECL
	   && DECL_CONTEXT (x) != current_class_type))
      && DECL_NAME (x) == constructor_name (current_class_type))
    {
      tree scope = context_for_name_lookup (x);
      if (TYPE_P (scope) && same_type_p (scope, current_class_type))
	{
	  error ("%qD has the same name as the class in which it is "
		 "declared",
		 x);
	  return false;
	}
    }

  /* Get the current binding for NAME in this class, if any.  */
  binding = IDENTIFIER_BINDING (name);
  if (!binding || binding->scope != class_binding_level)
    {
      binding = get_class_binding (name, class_binding_level);
      /* If a new binding was created, put it at the front of the
	 IDENTIFIER_BINDING list.  */
      if (binding)
	{
	  binding->previous = IDENTIFIER_BINDING (name);
	  IDENTIFIER_BINDING (name) = binding;
	}
    }

  /* If there is already a binding, then we may need to update the
     current value.  */
  if (binding && binding->value)
    {
      tree bval = binding->value;
      tree old_decl = NULL_TREE;
      tree target_decl = strip_using_decl (decl);
      tree target_bval = strip_using_decl (bval);

      if (INHERITED_VALUE_BINDING_P (binding))
	{
	  /* If the old binding was from a base class, and was for a
	     tag name, slide it over to make room for the new binding.
	     The old binding is still visible if explicitly qualified
	     with a class-key.  */
	  if (TREE_CODE (target_bval) == TYPE_DECL
	      && DECL_ARTIFICIAL (target_bval)
	      && !(TREE_CODE (target_decl) == TYPE_DECL
		   && DECL_ARTIFICIAL (target_decl)))
	    {
	      old_decl = binding->type;
	      binding->type = bval;
	      binding->value = NULL_TREE;
	      INHERITED_VALUE_BINDING_P (binding) = 0;
	    }
	  else
	    {
	      old_decl = bval;
	      /* Any inherited type declaration is hidden by the type
		 declaration in the derived class.  */
	      if (TREE_CODE (target_decl) == TYPE_DECL
		  && DECL_ARTIFICIAL (target_decl))
		binding->type = NULL_TREE;
	    }
	}
      else if (TREE_CODE (target_decl) == OVERLOAD
	       && is_overloaded_fn (target_bval))
	old_decl = bval;
      else if (TREE_CODE (decl) == USING_DECL
	       && TREE_CODE (bval) == USING_DECL
	       && same_type_p (USING_DECL_SCOPE (decl),
			       USING_DECL_SCOPE (bval)))
	/* This is a using redeclaration that will be diagnosed later
	   in supplement_binding */
	;
      else if (TREE_CODE (decl) == USING_DECL
	       && TREE_CODE (bval) == USING_DECL
	       && DECL_DEPENDENT_P (decl)
	       && DECL_DEPENDENT_P (bval))
	return true;
      else if (TREE_CODE (decl) == USING_DECL
	       && is_overloaded_fn (target_bval))
	old_decl = bval;
      else if (TREE_CODE (bval) == USING_DECL
	       && is_overloaded_fn (target_decl))
	return true;

      if (old_decl && binding->scope == class_binding_level)
	{
	  binding->value = x;
	  /* It is always safe to clear INHERITED_VALUE_BINDING_P
	     here.  This function is only used to register bindings
	     from with the class definition itself.  */
	  INHERITED_VALUE_BINDING_P (binding) = 0;
	  return true;
	}
    }

  /* Note that we declared this value so that we can issue an error if
     this is an invalid redeclaration of a name already used for some
     other purpose.  */
  note_name_declared_in_class (name, decl);

  /* If we didn't replace an existing binding, put the binding on the
     stack of bindings for the identifier, and update the shadowed
     list.  */
  if (binding && binding->scope == class_binding_level)
    /* Supplement the existing binding.  */
    ok = supplement_binding (binding, decl);
  else
    {
      /* Create a new binding.  */
      push_binding (name, decl, class_binding_level);
      ok = true;
    }

  return ok;
}

/* Wrapper for push_class_level_binding_1.  */

bool
push_class_level_binding (tree name, tree x)
{
  bool ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = push_class_level_binding_1 (name, x);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

/* Process "using SCOPE::NAME" in a class scope.  Return the
   USING_DECL created.  */

tree
do_class_using_decl (tree scope, tree name)
{
  /* The USING_DECL returned by this function.  */
  tree value;
  /* The declaration (or declarations) name by this using
     declaration.  NULL if we are in a template and cannot figure out
     what has been named.  */
  tree decl;
  /* True if SCOPE is a dependent type.  */
  bool scope_dependent_p;
  /* True if SCOPE::NAME is dependent.  */
  bool name_dependent_p;
  /* True if any of the bases of CURRENT_CLASS_TYPE are dependent.  */
  bool bases_dependent_p;
  tree binfo;
  tree base_binfo;
  int i;

  if (name == error_mark_node)
    return NULL_TREE;

  if (!scope || !TYPE_P (scope))
    {
      error ("using-declaration for non-member at class scope");
      return NULL_TREE;
    }

  /* Make sure the name is not invalid */
  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      error ("%<%T::%D%> names destructor", scope, name);
      return NULL_TREE;
    }
  /* Using T::T declares inheriting ctors, even if T is a typedef.  */
  if (MAYBE_CLASS_TYPE_P (scope)
      && (name == TYPE_IDENTIFIER (scope)
	  || constructor_name_p (name, scope)))
    {
      maybe_warn_cpp0x (CPP0X_INHERITING_CTORS);
      name = ctor_identifier;
    }
  if (constructor_name_p (name, current_class_type))
    {
      error ("%<%T::%D%> names constructor in %qT",
	     scope, name, current_class_type);
      return NULL_TREE;
    }

  scope_dependent_p = dependent_scope_p (scope);
  name_dependent_p = (scope_dependent_p
		      || (IDENTIFIER_TYPENAME_P (name)
			  && dependent_type_p (TREE_TYPE (name))));

  bases_dependent_p = false;
  if (processing_template_decl)
    for (binfo = TYPE_BINFO (current_class_type), i = 0;
	 BINFO_BASE_ITERATE (binfo, i, base_binfo);
	 i++)
      if (dependent_type_p (TREE_TYPE (base_binfo)))
	{
	  bases_dependent_p = true;
	  break;
	}

  decl = NULL_TREE;

  /* From [namespace.udecl]:

       A using-declaration used as a member-declaration shall refer to a
       member of a base class of the class being defined.

     In general, we cannot check this constraint in a template because
     we do not know the entire set of base classes of the current
     class type. Morover, if SCOPE is dependent, it might match a
     non-dependent base.  */

  if (!scope_dependent_p)
    {
      base_kind b_kind;
      binfo = lookup_base (current_class_type, scope, ba_any, &b_kind,
			   tf_warning_or_error);
      if (b_kind < bk_proper_base)
	{
	  if (!bases_dependent_p || b_kind == bk_same_type)
	    {
	      error_not_base_type (scope, current_class_type);
	      return NULL_TREE;
	    }
	}
      else if (!name_dependent_p)
	{
	  decl = lookup_member (binfo, name, 0, false, tf_warning_or_error);
	  if (!decl)
	    {
	      error ("no members matching %<%T::%D%> in %q#T", scope, name,
		     scope);
	      return NULL_TREE;
	    }
	  /* The binfo from which the functions came does not matter.  */
	  if (BASELINK_P (decl))
	    decl = BASELINK_FUNCTIONS (decl);
	}
    }

  value = build_lang_decl (USING_DECL, name, NULL_TREE);
  USING_DECL_DECLS (value) = decl;
  USING_DECL_SCOPE (value) = scope;
  DECL_DEPENDENT_P (value) = !decl;

  return value;
}


/* Return the binding value for name in scope.  */


static tree
namespace_binding_1 (tree name, tree scope)
{
  cxx_binding *binding;

  if (SCOPE_FILE_SCOPE_P (scope))
    scope = global_namespace;
  else
    /* Unnecessary for the global namespace because it can't be an alias. */
    scope = ORIGINAL_NAMESPACE (scope);

  binding = cp_binding_level_find_binding_for_name (NAMESPACE_LEVEL (scope), name);

  return binding ? binding->value : NULL_TREE;
}

tree
namespace_binding (tree name, tree scope)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = namespace_binding_1 (name, scope);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

/* Set the binding value for name in scope.  */

static void
set_namespace_binding_1 (tree name, tree scope, tree val)
{
  cxx_binding *b;

  if (scope == NULL_TREE)
    scope = global_namespace;
  b = binding_for_name (NAMESPACE_LEVEL (scope), name);
  if (!b->value
      /* For templates and using we create a single element OVERLOAD.
	 Look for the chain to know whether this is really augmenting
	 an existing overload.  */
      || (TREE_CODE (val) == OVERLOAD && OVL_CHAIN (val))
      || val == error_mark_node)
    b->value = val;
  else
    supplement_binding (b, val);
}

/* Wrapper for set_namespace_binding_1.  */

void
set_namespace_binding (tree name, tree scope, tree val)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  set_namespace_binding_1 (name, scope, val);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
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
    error ("declaration of %qD not in a namespace surrounding %qD",
	   decl, scope);
  DECL_CONTEXT (decl) = FROB_CONTEXT (scope);

  /* Writing "int N::i" to declare a variable within "N" is invalid.  */
  if (scope == current_namespace)
    {
      if (at_namespace_scope_p ())
	error ("explicit qualification in declaration of %qD",
	       decl);
      return;
    }

  /* See whether this has been declared in the namespace.  */
  old = lookup_qualified_name (scope, DECL_NAME (decl), /*type*/false,
			       /*complain*/true, /*hidden*/true);
  if (old == error_mark_node)
    /* No old declaration at all.  */
    goto complain;
  /* If it's a TREE_LIST, the result of the lookup was ambiguous.  */
  if (TREE_CODE (old) == TREE_LIST)
    {
      error ("reference to %qD is ambiguous", decl);
      print_candidates (old);
      return;
    }
  if (!is_overloaded_fn (decl))
    {
      /* We might have found OLD in an inline namespace inside SCOPE.  */
      if (TREE_CODE (decl) == TREE_CODE (old))
	DECL_CONTEXT (decl) = DECL_CONTEXT (old);
      /* Don't compare non-function decls with decls_match here, since
	 it can't check for the correct constness at this
	 point. pushdecl will find those errors later.  */
      return;
    }
  /* Since decl is a function, old should contain a function decl.  */
  if (!is_overloaded_fn (old))
    goto complain;
  /* A template can be explicitly specialized in any namespace.  */
  if (processing_explicit_instantiation)
    return;
  if (processing_template_decl || processing_specialization)
    /* We have not yet called push_template_decl to turn a
       FUNCTION_DECL into a TEMPLATE_DECL, so the declarations won't
       match.  But, we'll check later, when we construct the
       template.  */
    return;
  /* Instantiations or specializations of templates may be declared as
     friends in any namespace.  */
  if (friendp && DECL_USE_TEMPLATE (decl))
    return;
  if (is_overloaded_fn (old))
    {
      tree found = NULL_TREE;
      tree elt = old;
      for (; elt; elt = OVL_NEXT (elt))
	{
	  tree ofn = OVL_CURRENT (elt);
	  /* Adjust DECL_CONTEXT first so decls_match will return true
	     if DECL will match a declaration in an inline namespace.  */
	  DECL_CONTEXT (decl) = DECL_CONTEXT (ofn);
	  if (decls_match (decl, ofn))
	    {
	      if (found && !decls_match (found, ofn))
		{
		  DECL_CONTEXT (decl) = FROB_CONTEXT (scope);
		  error ("reference to %qD is ambiguous", decl);
		  print_candidates (old);
		  return;
		}
	      found = ofn;
	    }
	}
      if (found)
	{
	  if (!is_associated_namespace (scope, CP_DECL_CONTEXT (found)))
	    goto complain;
	  if (DECL_HIDDEN_FRIEND_P (found))
	    {
	      pedwarn (DECL_SOURCE_LOCATION (decl), 0,
		       "%qD has not been declared within %D", decl, scope);
	      inform (DECL_SOURCE_LOCATION (found), "only here as a friend");
	    }
	  DECL_CONTEXT (decl) = DECL_CONTEXT (found);
	  return;
	}
    }
  else
    {
      DECL_CONTEXT (decl) = DECL_CONTEXT (old);
      if (decls_match (decl, old))
	return;
    }

  /* It didn't work, go back to the explicit scope.  */
  DECL_CONTEXT (decl) = FROB_CONTEXT (scope);
 complain:
  error ("%qD should have been declared inside %qD", decl, scope);
}

/* Return the namespace where the current declaration is declared.  */

tree
current_decl_namespace (void)
{
  tree result;
  /* If we have been pushed into a different namespace, use it.  */
  if (!vec_safe_is_empty (decl_namespace_list))
    return decl_namespace_list->last ();

  if (current_class_type)
    result = decl_namespace_context (current_class_type);
  else if (current_function_decl)
    result = decl_namespace_context (current_function_decl);
  else
    result = current_namespace;
  return result;
}

/* Process any ATTRIBUTES on a namespace definition.  Returns true if
   attribute visibility is seen.  */

bool
handle_namespace_attrs (tree ns, tree attributes)
{
  tree d;
  bool saw_vis = false;

  for (d = attributes; d; d = TREE_CHAIN (d))
    {
      tree name = get_attribute_name (d);
      tree args = TREE_VALUE (d);

      if (is_attribute_p ("visibility", name))
	{
	  /* attribute visibility is a property of the syntactic block
	     rather than the namespace as a whole, so we don't touch the
	     NAMESPACE_DECL at all.  */
	  tree x = args ? TREE_VALUE (args) : NULL_TREE;
	  if (x == NULL_TREE || TREE_CODE (x) != STRING_CST || TREE_CHAIN (args))
	    {
	      warning (OPT_Wattributes,
		       "%qD attribute requires a single NTBS argument",
		       name);
	      continue;
	    }

	  if (!TREE_PUBLIC (ns))
	    warning (OPT_Wattributes,
		     "%qD attribute is meaningless since members of the "
		     "anonymous namespace get local symbols", name);

	  push_visibility (TREE_STRING_POINTER (x), 1);
	  saw_vis = true;
	}
      else if (is_attribute_p ("abi_tag", name))
	{
	  if (!DECL_NAMESPACE_ASSOCIATIONS (ns))
	    {
	      warning (OPT_Wattributes, "ignoring %qD attribute on non-inline "
		       "namespace", name);
	      continue;
	    }
	  if (!DECL_NAME (ns))
	    {
	      warning (OPT_Wattributes, "ignoring %qD attribute on anonymous "
		       "namespace", name);
	      continue;
	    }
	  if (!args)
	    {
	      tree dn = DECL_NAME (ns);
	      args = build_string (IDENTIFIER_LENGTH (dn) + 1,
				   IDENTIFIER_POINTER (dn));
	      TREE_TYPE (args) = char_array_type_node;
	      args = fix_string_type (args);
	      args = build_tree_list (NULL_TREE, args);
	    }
	  if (check_abi_tag_args (args, name))
	    DECL_ATTRIBUTES (ns) = tree_cons (name, args,
					      DECL_ATTRIBUTES (ns));
	}
      else
	{
	  warning (OPT_Wattributes, "%qD attribute directive ignored",
		   name);
	  continue;
	}
    }

  return saw_vis;
}
  
/* Push into the scope of the NAME namespace.  If NAME is NULL_TREE, then we
   select a name that is unique to this compilation unit.  */

void
push_namespace (tree name)
{
  tree d = NULL_TREE;
  bool need_new = true;
  bool implicit_use = false;
  bool anon = !name;

  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);

  /* We should not get here if the global_namespace is not yet constructed
     nor if NAME designates the global namespace:  The global scope is
     constructed elsewhere.  */
  gcc_assert (global_namespace != NULL && name != global_scope_name);

  if (anon)
    {
      name = get_anonymous_namespace_name();
      d = IDENTIFIER_NAMESPACE_VALUE (name);
      if (d)
	/* Reopening anonymous namespace.  */
	need_new = false;
      implicit_use = true;
    }
  else
    {
      /* Check whether this is an extended namespace definition.  */
      d = IDENTIFIER_NAMESPACE_VALUE (name);
      if (d != NULL_TREE && TREE_CODE (d) == NAMESPACE_DECL)
	{
	  tree dna = DECL_NAMESPACE_ALIAS (d);
	  if (dna)
 	    {
	      /* We do some error recovery for, eg, the redeclaration
		 of M here:

		 namespace N {}
		 namespace M = N;
		 namespace M {}

		 However, in nasty cases like:

		 namespace N
		 {
		   namespace M = N;
		   namespace M {}
		 }

		 we just error out below, in duplicate_decls.  */
	      if (NAMESPACE_LEVEL (dna)->level_chain
		  == current_binding_level)
		{
		  error ("namespace alias %qD not allowed here, "
			 "assuming %qD", d, dna);
		  d = dna;
		  need_new = false;
		}
	    }
	  else
	    need_new = false;
	}
    }

  if (need_new)
    {
      /* Make a new namespace, binding the name to it.  */
      d = build_lang_decl (NAMESPACE_DECL, name, void_type_node);
      DECL_CONTEXT (d) = FROB_CONTEXT (current_namespace);
      /* The name of this namespace is not visible to other translation
	 units if it is an anonymous namespace or member thereof.  */
      if (anon || decl_anon_ns_mem_p (current_namespace))
	TREE_PUBLIC (d) = 0;
      else
	TREE_PUBLIC (d) = 1;
      pushdecl (d);
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

  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

/* Pop from the scope of the current namespace.  */

void
pop_namespace (void)
{
  gcc_assert (current_namespace != global_namespace);
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
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  gcc_assert (current_namespace == ns);
  while (ns != global_namespace)
    {
      pop_namespace ();
      ns = CP_DECL_CONTEXT (ns);
    }

  pop_from_top_level ();
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

/* Temporarily set the namespace for the current declaration.  */

void
push_decl_namespace (tree decl)
{
  if (TREE_CODE (decl) != NAMESPACE_DECL)
    decl = decl_namespace_context (decl);
  vec_safe_push (decl_namespace_list, ORIGINAL_NAMESPACE (decl));
}

/* [namespace.memdef]/2 */

void
pop_decl_namespace (void)
{
  decl_namespace_list->pop ();
}

/* Return the namespace that is the common ancestor
   of two given namespaces.  */

static tree
namespace_ancestor_1 (tree ns1, tree ns2)
{
  tree nsr;
  if (is_ancestor (ns1, ns2))
    nsr = ns1;
  else
    nsr = namespace_ancestor_1 (CP_DECL_CONTEXT (ns1), ns2);
  return nsr;
}

/* Wrapper for namespace_ancestor_1.  */

static tree
namespace_ancestor (tree ns1, tree ns2)
{
  tree nsr;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  nsr = namespace_ancestor_1 (ns1, ns2);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return nsr;
}

/* Process a namespace-alias declaration.  */

void
do_namespace_alias (tree alias, tree name_space)
{
  if (name_space == error_mark_node)
    return;

  gcc_assert (TREE_CODE (name_space) == NAMESPACE_DECL);

  name_space = ORIGINAL_NAMESPACE (name_space);

  /* Build the alias.  */
  alias = build_lang_decl (NAMESPACE_DECL, alias, void_type_node);
  DECL_NAMESPACE_ALIAS (alias) = name_space;
  DECL_EXTERNAL (alias) = 1;
  DECL_CONTEXT (alias) = FROB_CONTEXT (current_scope ());
  pushdecl (alias);

  /* Emit debug info for namespace alias.  */
  if (!building_stmt_list_p ())
    (*debug_hooks->early_global_decl) (alias);
}

/* Like pushdecl, only it places X in the current namespace,
   if appropriate.  */

tree
pushdecl_namespace_level (tree x, bool is_friend)
{
  cp_binding_level *b = current_binding_level;
  tree t;

  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  t = pushdecl_with_scope (x, NAMESPACE_LEVEL (current_namespace), is_friend);

  /* Now, the type_shadowed stack may screw us.  Munge it so it does
     what we want.  */
  if (TREE_CODE (t) == TYPE_DECL)
    {
      tree name = DECL_NAME (t);
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
      newval = TREE_TYPE (t);
      if (ptr == (tree *)0)
	{
	  /* @@ This shouldn't be needed.  My test case "zstring.cc" trips
	     up here if this is changed to an assertion.  --KR  */
	  SET_IDENTIFIER_TYPE_VALUE (name, t);
	}
      else
	{
	  *ptr = newval;
	}
    }
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return t;
}

/* Insert USED into the using list of USER. Set INDIRECT_flag if this
   directive is not directly from the source. Also find the common
   ancestor and let our users know about the new namespace */

static void
add_using_namespace_1 (tree user, tree used, bool indirect)
{
  tree t;
  /* Using oneself is a no-op.  */
  if (user == used)
    return;
  gcc_assert (TREE_CODE (user) == NAMESPACE_DECL);
  gcc_assert (TREE_CODE (used) == NAMESPACE_DECL);
  /* Check if we already have this.  */
  t = purpose_member (used, DECL_NAMESPACE_USING (user));
  if (t != NULL_TREE)
    {
      if (!indirect)
	/* Promote to direct usage.  */
	TREE_INDIRECT_USING (t) = 0;
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
    add_using_namespace_1 (user, TREE_PURPOSE (t), 1);

  /* Tell everyone using us about the new used namespaces.  */
  for (t = DECL_NAMESPACE_USERS (user); t; t = TREE_CHAIN (t))
    add_using_namespace_1 (TREE_PURPOSE (t), used, 1);
}

/* Wrapper for add_using_namespace_1.  */

static void
add_using_namespace (tree user, tree used, bool indirect)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  add_using_namespace_1 (user, used, indirect);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

/* Process a using-declaration not appearing in class or local scope.  */

void
do_toplevel_using_decl (tree decl, tree scope, tree name)
{
  tree oldval, oldtype, newval, newtype;
  tree orig_decl = decl;
  cxx_binding *binding;

  decl = validate_nonmember_using_decl (decl, scope, name);
  if (decl == NULL_TREE)
    return;

  binding = binding_for_name (NAMESPACE_LEVEL (current_namespace), name);

  oldval = binding->value;
  oldtype = binding->type;

  do_nonmember_using_decl (scope, name, oldval, oldtype, &newval, &newtype);

  /* Emit debug info.  */
  if (!processing_template_decl)
    cp_emit_debug_info_for_using (orig_decl, current_namespace);

  /* Copy declarations found.  */
  if (newval)
    binding->value = newval;
  if (newtype)
    binding->type = newtype;
}

/* Process a using-directive.  */

void
do_using_directive (tree name_space)
{
  tree context = NULL_TREE;

  if (name_space == error_mark_node)
    return;

  gcc_assert (TREE_CODE (name_space) == NAMESPACE_DECL);

  if (building_stmt_list_p ())
    add_stmt (build_stmt (input_location, USING_STMT, name_space));
  name_space = ORIGINAL_NAMESPACE (name_space);

  if (!toplevel_bindings_p ())
    {
      push_using_directive (name_space);
    }
  else
    {
      /* direct usage */
      add_using_namespace (current_namespace, name_space, 0);
      if (current_namespace != global_namespace)
	context = current_namespace;

      /* Emit debugging info.  */
      if (!processing_template_decl)
	(*debug_hooks->imported_module_or_decl) (name_space, NULL_TREE,
						 context, false);
    }
}

/* Deal with a using-directive seen by the parser.  Currently we only
   handle attributes here, since they cannot appear inside a template.  */

void
parse_using_directive (tree name_space, tree attribs)
{
  do_using_directive (name_space);

  if (attribs == error_mark_node)
    return;

  for (tree a = attribs; a; a = TREE_CHAIN (a))
    {
      tree name = get_attribute_name (a);
      if (is_attribute_p ("strong", name))
	{
	  if (!toplevel_bindings_p ())
	    error ("strong using only meaningful at namespace scope");
	  else if (name_space != error_mark_node)
	    {
	      if (!is_ancestor (current_namespace, name_space))
		error ("current namespace %qD does not enclose strongly used namespace %qD",
		       current_namespace, name_space);
	      DECL_NAMESPACE_ASSOCIATIONS (name_space)
		= tree_cons (current_namespace, 0,
			     DECL_NAMESPACE_ASSOCIATIONS (name_space));
	    }
	}
      else
	warning (OPT_Wattributes, "%qD attribute directive ignored", name);
    }
}

/* Like pushdecl, only it places X in the global scope if appropriate.
   Calls cp_finish_decl to register the variable, initializing it with
   *INIT, if INIT is non-NULL.  */

static tree
pushdecl_top_level_1 (tree x, tree *init, bool is_friend)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  push_to_top_level ();
  x = pushdecl_namespace_level (x, is_friend);
  if (init)
    cp_finish_decl (x, *init, false, NULL_TREE, 0);
  pop_from_top_level ();
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return x;
}

/* Like pushdecl, only it places X in the global scope if appropriate.  */

tree
pushdecl_top_level (tree x)
{
  return pushdecl_top_level_1 (x, NULL, false);
}

/* Like pushdecl_top_level, but adding the IS_FRIEND parameter.  */

tree
pushdecl_top_level_maybe_friend (tree x, bool is_friend)
{
  return pushdecl_top_level_1 (x, NULL, is_friend);
}

/* Like pushdecl, only it places X in the global scope if
   appropriate.  Calls cp_finish_decl to register the variable,
   initializing it with INIT.  */

tree
pushdecl_top_level_and_finish (tree x, tree init)
{
  return pushdecl_top_level_1 (x, &init, false);
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
	     different namespaces, but let's leave them in case
	     they have different default arguments.  */
	  if (fn1 == fn2)
	    break;
	}

      /* If we exhausted all of the functions in S1, FN2 is new.  */
      if (!fns1)
	s1 = build_overload (fn2, s1);
    }
  return s1;
}

/* Returns TRUE iff OLD and NEW are the same entity.

   3 [basic]/3: An entity is a value, object, reference, function,
   enumerator, type, class member, template, template specialization,
   namespace, parameter pack, or this.

   7.3.4 [namespace.udir]/4: If name lookup finds a declaration for a name
   in two different namespaces, and the declarations do not declare the
   same entity and do not declare functions, the use of the name is
   ill-formed.  */

static bool
same_entity_p (tree one, tree two)
{
  if (one == two)
    return true;
  if (!one || !two)
    return false;
  if (TREE_CODE (one) == TYPE_DECL
      && TREE_CODE (two) == TYPE_DECL
      && same_type_p (TREE_TYPE (one), TREE_TYPE (two)))
    return true;
  return false;
}

/* This should return an error not all definitions define functions.
   It is not an error if we find two functions with exactly the
   same signature, only if these are selected in overload resolution.
   old is the current set of bindings, new_binding the freshly-found binding.
   XXX Do we want to give *all* candidates in case of ambiguity?
   XXX In what way should I treat extern declarations?
   XXX I don't want to repeat the entire duplicate_decls here */

static void
ambiguous_decl (struct scope_binding *old, cxx_binding *new_binding, int flags)
{
  tree val, type;
  gcc_assert (old != NULL);

  /* Copy the type.  */
  type = new_binding->type;
  if (LOOKUP_NAMESPACES_ONLY (flags)
      || (type && hidden_name_p (type) && !(flags & LOOKUP_HIDDEN)))
    type = NULL_TREE;

  /* Copy the value.  */
  val = new_binding->value;
  if (val)
    {
      if (!(flags & LOOKUP_HIDDEN))
	val = remove_hidden_names (val);
      if (val)
	switch (TREE_CODE (val))
	  {
	  case TEMPLATE_DECL:
	    /* If we expect types or namespaces, and not templates,
	       or this is not a template class.  */
	    if ((LOOKUP_QUALIFIERS_ONLY (flags)
		 && !DECL_TYPE_TEMPLATE_P (val)))
	      val = NULL_TREE;
	    break;
	  case TYPE_DECL:
	    if (LOOKUP_NAMESPACES_ONLY (flags)
		|| (type && (flags & LOOKUP_PREFER_TYPES)))
	      val = NULL_TREE;
	    break;
	  case NAMESPACE_DECL:
	    if (LOOKUP_TYPES_ONLY (flags))
	      val = NULL_TREE;
	    break;
	  case FUNCTION_DECL:
	    /* Ignore built-in functions that are still anticipated.  */
	    if (LOOKUP_QUALIFIERS_ONLY (flags))
	      val = NULL_TREE;
	    break;
	  default:
	    if (LOOKUP_QUALIFIERS_ONLY (flags))
	      val = NULL_TREE;
	  }
    }

  /* If val is hidden, shift down any class or enumeration name.  */
  if (!val)
    {
      val = type;
      type = NULL_TREE;
    }

  if (!old->value)
    old->value = val;
  else if (val && !same_entity_p (val, old->value))
    {
      if (is_overloaded_fn (old->value) && is_overloaded_fn (val))
	old->value = merge_functions (old->value, val);
      else
	{
	  old->value = tree_cons (NULL_TREE, old->value,
				  build_tree_list (NULL_TREE, val));
	  TREE_TYPE (old->value) = error_mark_node;
	}
    }

  if (!old->type)
    old->type = type;
  else if (type && old->type != type)
    {
      old->type = tree_cons (NULL_TREE, old->type,
			     build_tree_list (NULL_TREE, type));
      TREE_TYPE (old->type) = error_mark_node;
    }
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
   ignore it or not.  Subroutine of lookup_name_real and
   lookup_type_scope.  */

static bool
qualify_lookup (tree val, int flags)
{
  if (val == NULL_TREE)
    return false;
  if ((flags & LOOKUP_PREFER_NAMESPACES) && TREE_CODE (val) == NAMESPACE_DECL)
    return true;
  if (flags & LOOKUP_PREFER_TYPES)
    {
      tree target_val = strip_using_decl (val);
      if (TREE_CODE (target_val) == TYPE_DECL
	  || TREE_CODE (target_val) == TEMPLATE_DECL)
	return true;
    }
  if (flags & (LOOKUP_PREFER_NAMESPACES | LOOKUP_PREFER_TYPES))
    return false;
  /* Look through lambda things that we shouldn't be able to see.  */
  if (is_lambda_ignored_entity (val))
    return false;
  return true;
}

/* Given a lookup that returned VAL, decide if we want to ignore it or
   not based on DECL_ANTICIPATED.  */

bool
hidden_name_p (tree val)
{
  if (DECL_P (val)
      && DECL_LANG_SPECIFIC (val)
      && TYPE_FUNCTION_OR_TEMPLATE_DECL_P (val)
      && DECL_ANTICIPATED (val))
    return true;
  if (TREE_CODE (val) == OVERLOAD)
    {
      for (tree o = val; o; o = OVL_CHAIN (o))
	if (!hidden_name_p (OVL_FUNCTION (o)))
	  return false;
      return true;
    }
  return false;
}

/* Remove any hidden declarations from a possibly overloaded set
   of functions.  */

tree
remove_hidden_names (tree fns)
{
  if (!fns)
    return fns;

  if (DECL_P (fns) && hidden_name_p (fns))
    fns = NULL_TREE;
  else if (TREE_CODE (fns) == OVERLOAD)
    {
      tree o;

      for (o = fns; o; o = OVL_NEXT (o))
	if (hidden_name_p (OVL_CURRENT (o)))
	  break;
      if (o)
	{
	  tree n = NULL_TREE;

	  for (o = fns; o; o = OVL_NEXT (o))
	    if (!hidden_name_p (OVL_CURRENT (o)))
	      n = build_overload (OVL_CURRENT (o), n);
	  fns = n;
	}
    }

  return fns;
}

/* Suggest alternatives for NAME, an IDENTIFIER_NODE for which name
   lookup failed.  Search through all available namespaces and print out
   possible candidates.  */

void
suggest_alternatives_for (location_t location, tree name)
{
  vec<tree> candidates = vNULL;
  vec<tree> namespaces_to_search = vNULL;
  int max_to_search = PARAM_VALUE (CXX_MAX_NAMESPACES_FOR_DIAGNOSTIC_HELP);
  int n_searched = 0;
  tree t;
  unsigned ix;

  namespaces_to_search.safe_push (global_namespace);

  while (!namespaces_to_search.is_empty ()
	 && n_searched < max_to_search)
    {
      tree scope = namespaces_to_search.pop ();
      struct scope_binding binding = EMPTY_SCOPE_BINDING;
      cp_binding_level *level = NAMESPACE_LEVEL (scope);

      /* Look in this namespace.  */
      qualified_lookup_using_namespace (name, scope, &binding, 0);

      n_searched++;

      if (binding.value)
	candidates.safe_push (binding.value);

      /* Add child namespaces.  */
      for (t = level->namespaces; t; t = DECL_CHAIN (t))
	namespaces_to_search.safe_push (t);
    }

  /* If we stopped before we could examine all namespaces, inform the
     user.  Do this even if we don't have any candidates, since there
     might be more candidates further down that we weren't able to
     find.  */
  if (n_searched >= max_to_search
      && !namespaces_to_search.is_empty ())
    inform (location,
	    "maximum limit of %d namespaces searched for %qE",
	    max_to_search, name);

  namespaces_to_search.release ();

  /* Nothing useful to report.  */
  if (candidates.is_empty ())
    return;

  inform_n (location, candidates.length (),
	    "suggested alternative:",
	    "suggested alternatives:");

  FOR_EACH_VEC_ELT (candidates, ix, t)
    inform (location_of (t), "  %qE", t);

  candidates.release ();
}

/* Unscoped lookup of a global: iterate over current namespaces,
   considering using-directives.  */

static tree
unqualified_namespace_lookup_1 (tree name, int flags)
{
  tree initial = current_decl_namespace ();
  tree scope = initial;
  tree siter;
  cp_binding_level *level;
  tree val = NULL_TREE;

  for (; !val; scope = CP_DECL_CONTEXT (scope))
    {
      struct scope_binding binding = EMPTY_SCOPE_BINDING;
      cxx_binding *b =
	 cp_binding_level_find_binding_for_name (NAMESPACE_LEVEL (scope), name);

      if (b)
	ambiguous_decl (&binding, b, flags);

      /* Add all _DECLs seen through local using-directives.  */
      for (level = current_binding_level;
	   level->kind != sk_namespace;
	   level = level->level_chain)
	if (!lookup_using_namespace (name, &binding, level->using_directives,
				     scope, flags))
	  /* Give up because of error.  */
	  return error_mark_node;

      /* Add all _DECLs seen through global using-directives.  */
      /* XXX local and global using lists should work equally.  */
      siter = initial;
      while (1)
	{
	  if (!lookup_using_namespace (name, &binding,
				       DECL_NAMESPACE_USING (siter),
				       scope, flags))
	    /* Give up because of error.  */
	    return error_mark_node;
	  if (siter == scope) break;
	  siter = CP_DECL_CONTEXT (siter);
	}

      val = binding.value;
      if (scope == global_namespace)
	break;
    }
  return val;
}

/* Wrapper for unqualified_namespace_lookup_1.  */

static tree
unqualified_namespace_lookup (tree name, int flags)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = unqualified_namespace_lookup_1 (name, flags);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

/* Look up NAME (an IDENTIFIER_NODE) in SCOPE (either a NAMESPACE_DECL
   or a class TYPE).

   If PREFER_TYPE is > 0, we only return TYPE_DECLs or namespaces.
   If PREFER_TYPE is > 1, we only return TYPE_DECLs.

   Returns a DECL (or OVERLOAD, or BASELINK) representing the
   declaration found.  If no suitable declaration can be found,
   ERROR_MARK_NODE is returned.  If COMPLAIN is true and SCOPE is
   neither a class-type nor a namespace a diagnostic is issued.  */

tree
lookup_qualified_name (tree scope, tree name, int prefer_type, bool complain,
		       bool find_hidden)
{
  tree t = NULL_TREE;

  if (TREE_CODE (scope) == NAMESPACE_DECL)
    {
      struct scope_binding binding = EMPTY_SCOPE_BINDING;

      int flags = lookup_flags (prefer_type, /*namespaces_only*/false);
      if (find_hidden)
	flags |= LOOKUP_HIDDEN;
      if (qualified_lookup_using_namespace (name, scope, &binding, flags))
	t = binding.value;
    }
  else if (cxx_dialect != cxx98 && TREE_CODE (scope) == ENUMERAL_TYPE)
    t = lookup_enumerator (scope, name);
  else if (is_class_type (scope, complain))
    t = lookup_member (scope, name, 2, prefer_type, tf_warning_or_error);

  if (!t)
    return error_mark_node;
  return t;
}

/* Subroutine of unqualified_namespace_lookup:
   Add the bindings of NAME in used namespaces to VAL.
   We are currently looking for names in namespace SCOPE, so we
   look through USINGS for using-directives of namespaces
   which have SCOPE as a common ancestor with the current scope.
   Returns false on errors.  */

static bool
lookup_using_namespace (tree name, struct scope_binding *val,
			tree usings, tree scope, int flags)
{
  tree iter;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  /* Iterate over all used namespaces in current, searching for using
     directives of scope.  */
  for (iter = usings; iter; iter = TREE_CHAIN (iter))
    if (TREE_VALUE (iter) == scope)
      {
	tree used = ORIGINAL_NAMESPACE (TREE_PURPOSE (iter));
	cxx_binding *val1 =
	  cp_binding_level_find_binding_for_name (NAMESPACE_LEVEL (used), name);
	/* Resolve ambiguities.  */
	if (val1)
	  ambiguous_decl (val, val1, flags);
      }
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return val->value != error_mark_node;
}

/* Returns true iff VEC contains TARGET.  */

static bool
tree_vec_contains (vec<tree, va_gc> *vec, tree target)
{
  unsigned int i;
  tree elt;
  FOR_EACH_VEC_SAFE_ELT (vec,i,elt)
    if (elt == target)
      return true;
  return false;
}

/* [namespace.qual]
   Accepts the NAME to lookup and its qualifying SCOPE.
   Returns the name/type pair found into the cxx_binding *RESULT,
   or false on error.  */

static bool
qualified_lookup_using_namespace (tree name, tree scope,
				  struct scope_binding *result, int flags)
{
  /* Maintain a list of namespaces visited...  */
  vec<tree, va_gc> *seen = NULL;
  vec<tree, va_gc> *seen_inline = NULL;
  /* ... and a list of namespace yet to see.  */
  vec<tree, va_gc> *todo = NULL;
  vec<tree, va_gc> *todo_maybe = NULL;
  vec<tree, va_gc> *todo_inline = NULL;
  tree usings;
  timevar_start (TV_NAME_LOOKUP);
  /* Look through namespace aliases.  */
  scope = ORIGINAL_NAMESPACE (scope);

  /* Algorithm: Starting with SCOPE, walk through the set of used
     namespaces.  For each used namespace, look through its inline
     namespace set for any bindings and usings.  If no bindings are
     found, add any usings seen to the set of used namespaces.  */
  vec_safe_push (todo, scope);

  while (todo->length ())
    {
      bool found_here;
      scope = todo->pop ();
      if (tree_vec_contains (seen, scope))
	continue;
      vec_safe_push (seen, scope);
      vec_safe_push (todo_inline, scope);

      found_here = false;
      while (todo_inline->length ())
	{
	  cxx_binding *binding;

	  scope = todo_inline->pop ();
	  if (tree_vec_contains (seen_inline, scope))
	    continue;
	  vec_safe_push (seen_inline, scope);

	  binding =
	    cp_binding_level_find_binding_for_name (NAMESPACE_LEVEL (scope), name);
	  if (binding)
	    {
	      ambiguous_decl (result, binding, flags);
	      if (result->type || result->value)
		found_here = true;
	    }

	  for (usings = DECL_NAMESPACE_USING (scope); usings;
	       usings = TREE_CHAIN (usings))
	    if (!TREE_INDIRECT_USING (usings))
	      {
		if (is_associated_namespace (scope, TREE_PURPOSE (usings)))
		  vec_safe_push (todo_inline, TREE_PURPOSE (usings));
		else
		  vec_safe_push (todo_maybe, TREE_PURPOSE (usings));
	      }
	}

      if (found_here)
	vec_safe_truncate (todo_maybe, 0);
      else
	while (vec_safe_length (todo_maybe))
	  vec_safe_push (todo, todo_maybe->pop ());
    }
  vec_free (todo);
  vec_free (todo_maybe);
  vec_free (todo_inline);
  vec_free (seen);
  vec_free (seen_inline);
  timevar_stop (TV_NAME_LOOKUP);
  return result->value != error_mark_node;
}

/* Subroutine of outer_binding.

   Returns TRUE if BINDING is a binding to a template parameter of
   SCOPE.  In that case SCOPE is the scope of a primary template
   parameter -- in the sense of G++, i.e, a template that has its own
   template header.

   Returns FALSE otherwise.  */

static bool
binding_to_template_parms_of_scope_p (cxx_binding *binding,
				      cp_binding_level *scope)
{
  tree binding_value, tmpl, tinfo;
  int level;

  if (!binding || !scope || !scope->this_entity)
    return false;

  binding_value = binding->value ?  binding->value : binding->type;
  tinfo = get_template_info (scope->this_entity);

  /* BINDING_VALUE must be a template parm.  */
  if (binding_value == NULL_TREE
      || (!DECL_P (binding_value)
          || !DECL_TEMPLATE_PARM_P (binding_value)))
    return false;

  /*  The level of BINDING_VALUE.  */
  level =
    template_type_parameter_p (binding_value)
    ? TEMPLATE_PARM_LEVEL (TEMPLATE_TYPE_PARM_INDEX
			 (TREE_TYPE (binding_value)))
    : TEMPLATE_PARM_LEVEL (DECL_INITIAL (binding_value));

  /* The template of the current scope, iff said scope is a primary
     template.  */
  tmpl = (tinfo
	  && PRIMARY_TEMPLATE_P (TI_TEMPLATE (tinfo))
	  ? TI_TEMPLATE (tinfo)
	  : NULL_TREE);

  /* If the level of the parm BINDING_VALUE equals the depth of TMPL,
     then BINDING_VALUE is a parameter of TMPL.  */
  return (tmpl && level == TMPL_PARMS_DEPTH (DECL_TEMPLATE_PARMS (tmpl)));
}

/* Return the innermost non-namespace binding for NAME from a scope
   containing BINDING, or, if BINDING is NULL, the current scope.
   Please note that for a given template, the template parameters are
   considered to be in the scope containing the current scope.
   If CLASS_P is false, then class bindings are ignored.  */

cxx_binding *
outer_binding (tree name,
	       cxx_binding *binding,
	       bool class_p)
{
  cxx_binding *outer;
  cp_binding_level *scope;
  cp_binding_level *outer_scope;

  if (binding)
    {
      scope = binding->scope->level_chain;
      outer = binding->previous;
    }
  else
    {
      scope = current_binding_level;
      outer = IDENTIFIER_BINDING (name);
    }
  outer_scope = outer ? outer->scope : NULL;

  /* Because we create class bindings lazily, we might be missing a
     class binding for NAME.  If there are any class binding levels
     between the LAST_BINDING_LEVEL and the scope in which OUTER was
     declared, we must lookup NAME in those class scopes.  */
  if (class_p)
    while (scope && scope != outer_scope && scope->kind != sk_namespace)
      {
	if (scope->kind == sk_class)
	  {
	    cxx_binding *class_binding;

	    class_binding = get_class_binding (name, scope);
	    if (class_binding)
	      {
		/* Thread this new class-scope binding onto the
		   IDENTIFIER_BINDING list so that future lookups
		   find it quickly.  */
		class_binding->previous = outer;
		if (binding)
		  binding->previous = class_binding;
		else
		  IDENTIFIER_BINDING (name) = class_binding;
		return class_binding;
	      }
	  }
	/* If we are in a member template, the template parms of the member
	   template are considered to be inside the scope of the containing
	   class, but within G++ the class bindings are all pushed between the
	   template parms and the function body.  So if the outer binding is
	   a template parm for the current scope, return it now rather than
	   look for a class binding.  */
	if (outer_scope && outer_scope->kind == sk_template_parms
	    && binding_to_template_parms_of_scope_p (outer, scope))
	  return outer;

	scope = scope->level_chain;
      }

  return outer;
}

/* Return the innermost block-scope or class-scope value binding for
   NAME, or NULL_TREE if there is no such binding.  */

tree
innermost_non_namespace_value (tree name)
{
  cxx_binding *binding;
  binding = outer_binding (name, /*binding=*/NULL, /*class_p=*/true);
  return binding ? binding->value : NULL_TREE;
}

/* Look up NAME in the current binding level and its superiors in the
   namespace of variables, functions and typedefs.  Return a ..._DECL
   node of some kind representing its definition if there is only one
   such declaration, or return a TREE_LIST with all the overloaded
   definitions if there are many, or return 0 if it is undefined.
   Hidden name, either friend declaration or built-in function, are
   not ignored.

   If PREFER_TYPE is > 0, we prefer TYPE_DECLs or namespaces.
   If PREFER_TYPE is > 1, we reject non-type decls (e.g. namespaces).
   Otherwise we prefer non-TYPE_DECLs.

   If NONCLASS is nonzero, bindings in class scopes are ignored.  If
   BLOCK_P is false, bindings in block scopes are ignored.  */

static tree
lookup_name_real_1 (tree name, int prefer_type, int nonclass, bool block_p,
		    int namespaces_only, int flags)
{
  cxx_binding *iter;
  tree val = NULL_TREE;

  /* Conversion operators are handled specially because ordinary
     unqualified name lookup will not find template conversion
     operators.  */
  if (IDENTIFIER_TYPENAME_P (name))
    {
      cp_binding_level *level;

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
	    return operators;
	}

      return NULL_TREE;
    }

  flags |= lookup_flags (prefer_type, namespaces_only);

  /* First, look in non-namespace scopes.  */

  if (current_class_type == NULL_TREE)
    nonclass = 1;

  if (block_p || !nonclass)
    for (iter = outer_binding (name, NULL, !nonclass);
	 iter;
	 iter = outer_binding (name, iter, !nonclass))
      {
	tree binding;

	/* Skip entities we don't want.  */
	if (LOCAL_BINDING_P (iter) ? !block_p : nonclass)
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
	    if (hidden_name_p (binding))
	      {
		/* A non namespace-scope binding can only be hidden in the
		   presence of a local class, due to friend declarations.

		   In particular, consider:

		   struct C;
		   void f() {
		     struct A {
		       friend struct B;
		       friend struct C;
		       void g() {
		         B* b; // error: B is hidden
			 C* c; // OK, finds ::C
		       } 
		     };
		     B *b;  // error: B is hidden
		     C *c;  // OK, finds ::C
		     struct B {};
		     B *bb; // OK
		   }

		   The standard says that "B" is a local class in "f"
		   (but not nested within "A") -- but that name lookup
		   for "B" does not find this declaration until it is
		   declared directly with "f".

		   In particular:

		   [class.friend]

		   If a friend declaration appears in a local class and
		   the name specified is an unqualified name, a prior
		   declaration is looked up without considering scopes
		   that are outside the innermost enclosing non-class
		   scope. For a friend function declaration, if there is
		   no prior declaration, the program is ill-formed. For a
		   friend class declaration, if there is no prior
		   declaration, the class that is specified belongs to the
		   innermost enclosing non-class scope, but if it is
		   subsequently referenced, its name is not found by name
		   lookup until a matching declaration is provided in the
		   innermost enclosing nonclass scope.

		   So just keep looking for a non-hidden binding.
		*/
		gcc_assert (TREE_CODE (binding) == TYPE_DECL);
		continue;
	      }
	    val = binding;
	    break;
	  }
      }

  /* Now lookup in namespace scopes.  */
  if (!val)
    val = unqualified_namespace_lookup (name, flags);

  /* Anticipated built-ins and friends aren't found by normal lookup.  */
  if (val && !(flags & LOOKUP_HIDDEN))
    val = remove_hidden_names (val);

  /* If we have a single function from a using decl, pull it out.  */
  if (val && TREE_CODE (val) == OVERLOAD && !really_overloaded_fn (val))
    val = OVL_FUNCTION (val);

  return val;
}

/* Wrapper for lookup_name_real_1.  */

tree
lookup_name_real (tree name, int prefer_type, int nonclass, bool block_p,
		  int namespaces_only, int flags)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = lookup_name_real_1 (name, prefer_type, nonclass, block_p,
			    namespaces_only, flags);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

tree
lookup_name_nonclass (tree name)
{
  return lookup_name_real (name, 0, 1, /*block_p=*/true, 0, 0);
}

tree
lookup_function_nonclass (tree name, vec<tree, va_gc> *args, bool block_p)
{
  return
    lookup_arg_dependent (name,
			  lookup_name_real (name, 0, 1, block_p, 0, 0),
			  args);
}

tree
lookup_name (tree name)
{
  return lookup_name_real (name, 0, 0, /*block_p=*/true, 0, 0);
}

tree
lookup_name_prefer_type (tree name, int prefer_type)
{
  return lookup_name_real (name, prefer_type, 0, /*block_p=*/true, 0, 0);
}

/* Look up NAME for type used in elaborated name specifier in
   the scopes given by SCOPE.  SCOPE can be either TS_CURRENT or
   TS_WITHIN_ENCLOSING_NON_CLASS.  Although not implied by the
   name, more scopes are checked if cleanup or template parameter
   scope is encountered.

   Unlike lookup_name_real, we make sure that NAME is actually
   declared in the desired scope, not from inheritance, nor using
   directive.  For using declaration, there is DR138 still waiting
   to be resolved.  Hidden name coming from an earlier friend
   declaration is also returned.

   A TYPE_DECL best matching the NAME is returned.  Catching error
   and issuing diagnostics are caller's responsibility.  */

static tree
lookup_type_scope_1 (tree name, tag_scope scope)
{
  cxx_binding *iter = NULL;
  tree val = NULL_TREE;

  /* Look in non-namespace scope first.  */
  if (current_binding_level->kind != sk_namespace)
    iter = outer_binding (name, NULL, /*class_p=*/ true);
  for (; iter; iter = outer_binding (name, iter, /*class_p=*/ true))
    {
      /* Check if this is the kind of thing we're looking for.
	 If SCOPE is TS_CURRENT, also make sure it doesn't come from
	 base class.  For ITER->VALUE, we can simply use
	 INHERITED_VALUE_BINDING_P.  For ITER->TYPE, we have to use
	 our own check.

	 We check ITER->TYPE before ITER->VALUE in order to handle
	   typedef struct C {} C;
	 correctly.  */

      if (qualify_lookup (iter->type, LOOKUP_PREFER_TYPES)
	  && (scope != ts_current
	      || LOCAL_BINDING_P (iter)
	      || DECL_CONTEXT (iter->type) == iter->scope->this_entity))
	val = iter->type;
      else if ((scope != ts_current
		|| !INHERITED_VALUE_BINDING_P (iter))
	       && qualify_lookup (iter->value, LOOKUP_PREFER_TYPES))
	val = iter->value;

      if (val)
	break;
    }

  /* Look in namespace scope.  */
  if (!val)
    {
      iter = cp_binding_level_find_binding_for_name
	       (NAMESPACE_LEVEL (current_decl_namespace ()), name);

      if (iter)
	{
	  /* If this is the kind of thing we're looking for, we're done.  */
	  if (qualify_lookup (iter->type, LOOKUP_PREFER_TYPES))
	    val = iter->type;
	  else if (qualify_lookup (iter->value, LOOKUP_PREFER_TYPES))
	    val = iter->value;
	}

    }

  /* Type found, check if it is in the allowed scopes, ignoring cleanup
     and template parameter scopes.  */
  if (val)
    {
      cp_binding_level *b = current_binding_level;
      while (b)
	{
	  if (iter->scope == b)
	    return val;

	  if (b->kind == sk_cleanup || b->kind == sk_template_parms
	      || b->kind == sk_function_parms)
	    b = b->level_chain;
	  else if (b->kind == sk_class
		   && scope == ts_within_enclosing_non_class)
	    b = b->level_chain;
	  else
	    break;
	}
    }

  return NULL_TREE;
}
 
/* Wrapper for lookup_type_scope_1.  */

tree
lookup_type_scope (tree name, tag_scope scope)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = lookup_type_scope_1 (name, scope);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}


/* Similar to `lookup_name' but look only in the innermost non-class
   binding level.  */

static tree
lookup_name_innermost_nonclass_level_1 (tree name)
{
  cp_binding_level *b;
  tree t = NULL_TREE;

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
      cxx_binding *binding;
      binding = IDENTIFIER_BINDING (name);
      while (1)
	{
	  if (binding->scope == b
	      && !(VAR_P (binding->value)
		   && DECL_DEAD_FOR_LOCAL (binding->value)))
	    return binding->value;

	  if (b->kind == sk_cleanup)
	    b = b->level_chain;
	  else
	    break;
	}
    }

  return t;
}

/* Wrapper for lookup_name_innermost_nonclass_level_1.  */

tree
lookup_name_innermost_nonclass_level (tree name)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = lookup_name_innermost_nonclass_level_1 (name);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}


/* Returns true iff DECL is a block-scope extern declaration of a function
   or variable.  */

bool
is_local_extern (tree decl)
{
  cxx_binding *binding;

  /* For functions, this is easy.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    return DECL_LOCAL_FUNCTION_P (decl);

  if (!VAR_P (decl))
    return false;
  if (!current_function_decl)
    return false;

  /* For variables, this is not easy.  We need to look at the binding stack
     for the identifier to see whether the decl we have is a local.  */
  for (binding = IDENTIFIER_BINDING (DECL_NAME (decl));
       binding && binding->scope->kind != sk_namespace;
       binding = binding->previous)
    if (binding->value == decl)
      return LOCAL_BINDING_P (binding);

  return false;
}

/* Like lookup_name_innermost_nonclass_level, but for types.  */

static tree
lookup_type_current_level (tree name)
{
  tree t = NULL_TREE;

  timevar_start (TV_NAME_LOOKUP);
  gcc_assert (current_binding_level->kind != sk_namespace);

  if (REAL_IDENTIFIER_TYPE_VALUE (name) != NULL_TREE
      && REAL_IDENTIFIER_TYPE_VALUE (name) != global_type_node)
    {
      cp_binding_level *b = current_binding_level;
      while (1)
	{
	  if (purpose_member (name, b->type_shadowed))
	    {
	      t = REAL_IDENTIFIER_TYPE_VALUE (name);
	      break;
	    }
	  if (b->kind == sk_cleanup)
	    b = b->level_chain;
	  else
	    break;
	}
    }

  timevar_stop (TV_NAME_LOOKUP);
  return t;
}

/* [basic.lookup.koenig] */
/* A nonzero return value in the functions below indicates an error.  */

struct arg_lookup
{
  tree name;
  vec<tree, va_gc> *args;
  vec<tree, va_gc> *namespaces;
  vec<tree, va_gc> *classes;
  tree functions;
  hash_set<tree> *fn_set;
};

static bool arg_assoc (struct arg_lookup*, tree);
static bool arg_assoc_args (struct arg_lookup*, tree);
static bool arg_assoc_args_vec (struct arg_lookup*, vec<tree, va_gc> *);
static bool arg_assoc_type (struct arg_lookup*, tree);
static bool add_function (struct arg_lookup *, tree);
static bool arg_assoc_namespace (struct arg_lookup *, tree);
static bool arg_assoc_class_only (struct arg_lookup *, tree);
static bool arg_assoc_bases (struct arg_lookup *, tree);
static bool arg_assoc_class (struct arg_lookup *, tree);
static bool arg_assoc_template_arg (struct arg_lookup*, tree);

/* Add a function to the lookup structure.
   Returns true on error.  */

static bool
add_function (struct arg_lookup *k, tree fn)
{
  if (!is_overloaded_fn (fn))
    /* All names except those of (possibly overloaded) functions and
       function templates are ignored.  */;
  else if (k->fn_set && k->fn_set->add (fn))
    /* It's already in the list.  */;
  else if (!k->functions)
    k->functions = fn;
  else if (fn == k->functions)
    ;
  else
    {
      k->functions = build_overload (fn, k->functions);
      if (TREE_CODE (k->functions) == OVERLOAD)
	OVL_ARG_DEPENDENT (k->functions) = true;
    }

  return false;
}

/* Returns true iff CURRENT has declared itself to be an associated
   namespace of SCOPE via a strong using-directive (or transitive chain
   thereof).  Both are namespaces.  */

bool
is_associated_namespace (tree current, tree scope)
{
  vec<tree, va_gc> *seen = make_tree_vector ();
  vec<tree, va_gc> *todo = make_tree_vector ();
  tree t;
  bool ret;

  while (1)
    {
      if (scope == current)
	{
	  ret = true;
	  break;
	}
      vec_safe_push (seen, scope);
      for (t = DECL_NAMESPACE_ASSOCIATIONS (scope); t; t = TREE_CHAIN (t))
	if (!vec_member (TREE_PURPOSE (t), seen))
	  vec_safe_push (todo, TREE_PURPOSE (t));
      if (!todo->is_empty ())
	{
	  scope = todo->last ();
	  todo->pop ();
	}
      else
	{
	  ret = false;
	  break;
	}
    }

  release_tree_vector (seen);
  release_tree_vector (todo);

  return ret;
}

/* Add functions of a namespace to the lookup structure.
   Returns true on error.  */

static bool
arg_assoc_namespace (struct arg_lookup *k, tree scope)
{
  tree value;

  if (vec_member (scope, k->namespaces))
    return false;
  vec_safe_push (k->namespaces, scope);

  /* Check out our super-users.  */
  for (value = DECL_NAMESPACE_ASSOCIATIONS (scope); value;
       value = TREE_CHAIN (value))
    if (arg_assoc_namespace (k, TREE_PURPOSE (value)))
      return true;

  /* Also look down into inline namespaces.  */
  for (value = DECL_NAMESPACE_USING (scope); value;
       value = TREE_CHAIN (value))
    if (is_associated_namespace (scope, TREE_PURPOSE (value)))
      if (arg_assoc_namespace (k, TREE_PURPOSE (value)))
	return true;

  value = namespace_binding (k->name, scope);
  if (!value)
    return false;

  for (; value; value = OVL_NEXT (value))
    {
      /* We don't want to find arbitrary hidden functions via argument
	 dependent lookup.  We only want to find friends of associated
	 classes, which we'll do via arg_assoc_class.  */
      if (hidden_name_p (OVL_CURRENT (value)))
	continue;

      if (add_function (k, OVL_CURRENT (value)))
	return true;
    }

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
	return arg_assoc_class_only (k, ctx);
    }
  /* It's an argument pack; handle it recursively.  */
  else if (ARGUMENT_PACK_P (arg))
    {
      tree args = ARGUMENT_PACK_ARGS (arg);
      int i, len = TREE_VEC_LENGTH (args);
      for (i = 0; i < len; ++i) 
	if (arg_assoc_template_arg (k, TREE_VEC_ELT (args, i)))
	  return true;

      return false;
    }
  /* It's not a template template argument, but it is a type template
     argument.  */
  else if (TYPE_P (arg))
    return arg_assoc_type (k, arg);
  /* It's a non-type template argument.  */
  else
    return false;
}

/* Adds the class and its friends to the lookup structure.
   Returns true on error.  */

static bool
arg_assoc_class_only (struct arg_lookup *k, tree type)
{
  tree list, friends, context;

  /* Backend-built structures, such as __builtin_va_list, aren't
     affected by all this.  */
  if (!CLASS_TYPE_P (type))
    return false;

  context = decl_namespace_context (type);
  if (arg_assoc_namespace (k, context))
    return true;

  complete_type (type);

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

  return false;
}

/* Adds the class and its bases to the lookup structure.
   Returns true on error.  */

static bool
arg_assoc_bases (struct arg_lookup *k, tree type)
{
  if (arg_assoc_class_only (k, type))
    return true;

  if (TYPE_BINFO (type))
    {
      /* Process baseclasses.  */
      tree binfo, base_binfo;
      int i;

      for (binfo = TYPE_BINFO (type), i = 0;
	   BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	if (arg_assoc_bases (k, BINFO_TYPE (base_binfo)))
	  return true;
    }

  return false;
}

/* Adds everything associated with a class argument type to the lookup
   structure.  Returns true on error.

   If T is a class type (including unions), its associated classes are: the
   class itself; the class of which it is a member, if any; and its direct
   and indirect base classes. Its associated namespaces are the namespaces
   of which its associated classes are members. Furthermore, if T is a
   class template specialization, its associated namespaces and classes
   also include: the namespaces and classes associated with the types of
   the template arguments provided for template type parameters (excluding
   template template parameters); the namespaces of which any template
   template arguments are members; and the classes of which any member
   templates used as template template arguments are members. [ Note:
   non-type template arguments do not contribute to the set of associated
   namespaces.  --end note] */

static bool
arg_assoc_class (struct arg_lookup *k, tree type)
{
  tree list;
  int i;

  /* Backend build structures, such as __builtin_va_list, aren't
     affected by all this.  */
  if (!CLASS_TYPE_P (type))
    return false;

  if (vec_member (type, k->classes))
    return false;
  vec_safe_push (k->classes, type);

  if (TYPE_CLASS_SCOPE_P (type)
      && arg_assoc_class_only (k, TYPE_CONTEXT (type)))
    return true;

  if (arg_assoc_bases (k, type))
    return true;

  /* Process template arguments.  */
  if (CLASSTYPE_TEMPLATE_INFO (type)
      && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (type)))
    {
      list = INNERMOST_TEMPLATE_ARGS (CLASSTYPE_TI_ARGS (type));
      for (i = 0; i < TREE_VEC_LENGTH (list); ++i)
	if (arg_assoc_template_arg (k, TREE_VEC_ELT (list, i)))
	  return true;
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

  if (TYPE_PTRDATAMEM_P (type))
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
    case BOOLEAN_TYPE:
    case FIXED_POINT_TYPE:
    case DECLTYPE_TYPE:
    case NULLPTR_TYPE:
      return false;
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (type))
	return arg_assoc_type (k, TYPE_PTRMEMFUNC_FN_TYPE (type));
    case UNION_TYPE:
      return arg_assoc_class (k, type);
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case ARRAY_TYPE:
      return arg_assoc_type (k, TREE_TYPE (type));
    case ENUMERAL_TYPE:
      if (TYPE_CLASS_SCOPE_P (type)
	  && arg_assoc_class_only (k, TYPE_CONTEXT (type)))
	return true;
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
      gcc_assert (type == unknown_type_node
		  || type == init_list_type_node);
      return false;
    case TYPE_PACK_EXPANSION:
      return arg_assoc_type (k, PACK_EXPANSION_PATTERN (type));

    default:
      gcc_unreachable ();
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

/* Adds everything associated with an argument vector.  Returns true
   on error.  */

static bool
arg_assoc_args_vec (struct arg_lookup *k, vec<tree, va_gc> *args)
{
  unsigned int ix;
  tree arg;

  FOR_EACH_VEC_SAFE_ELT (args, ix, arg)
    if (arg_assoc (k, arg))
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
  if (BASELINK_P (n))
    n = BASELINK_FUNCTIONS (n);

  if (TREE_CODE (n) == FUNCTION_DECL)
    return arg_assoc_type (k, TREE_TYPE (n));
  if (TREE_CODE (n) == TEMPLATE_ID_EXPR)
    {
      /* The working paper doesn't currently say how to handle template-id
	 arguments.  The sensible thing would seem to be to handle the list
	 of template candidates like a normal overload set, and handle the
	 template arguments like we do for class template
	 specializations.  */
      tree templ = TREE_OPERAND (n, 0);
      tree args = TREE_OPERAND (n, 1);
      int ix;

      /* First the templates.  */
      if (arg_assoc (k, templ))
	return true;

      /* Now the arguments.  */
      if (args)
	for (ix = TREE_VEC_LENGTH (args); ix--;)
	  if (arg_assoc_template_arg (k, TREE_VEC_ELT (args, ix)) == 1)
	    return true;
    }
  else if (TREE_CODE (n) == OVERLOAD)
    {
      for (; n; n = OVL_NEXT (n))
	if (arg_assoc_type (k, TREE_TYPE (OVL_CURRENT (n))))
	  return true;
    }

  return false;
}

/* Performs Koenig lookup depending on arguments, where fns
   are the functions found in normal lookup.  */

static cp_expr
lookup_arg_dependent_1 (tree name, tree fns, vec<tree, va_gc> *args)
{
  struct arg_lookup k;

  /* Remove any hidden friend functions from the list of functions
     found so far.  They will be added back by arg_assoc_class as
     appropriate.  */
  fns = remove_hidden_names (fns);

  k.name = name;
  k.args = args;
  k.functions = fns;
  k.classes = make_tree_vector ();

  /* We previously performed an optimization here by setting
     NAMESPACES to the current namespace when it was safe. However, DR
     164 says that namespaces that were already searched in the first
     stage of template processing are searched again (potentially
     picking up later definitions) in the second stage. */
  k.namespaces = make_tree_vector ();

  /* We used to allow duplicates and let joust discard them, but
     since the above change for DR 164 we end up with duplicates of
     all the functions found by unqualified lookup.  So keep track
     of which ones we've seen.  */
  if (fns)
    {
      tree ovl;
      /* We shouldn't be here if lookup found something other than
	 namespace-scope functions.  */
      gcc_assert (DECL_NAMESPACE_SCOPE_P (OVL_CURRENT (fns)));
      k.fn_set = new hash_set<tree>;
      for (ovl = fns; ovl; ovl = OVL_NEXT (ovl))
	k.fn_set->add (OVL_CURRENT (ovl));
    }
  else
    k.fn_set = NULL;

  arg_assoc_args_vec (&k, args);

  fns = k.functions;
  
  if (fns
      && !VAR_P (fns)
      && !is_overloaded_fn (fns))
    {
      error ("argument dependent lookup finds %q+D", fns);
      error ("  in call to %qD", name);
      fns = error_mark_node;
    }

  release_tree_vector (k.classes);
  release_tree_vector (k.namespaces);
  delete k.fn_set;
    
  return fns;
}

/* Wrapper for lookup_arg_dependent_1.  */

cp_expr
lookup_arg_dependent (tree name, tree fns, vec<tree, va_gc> *args)
{
  cp_expr ret;
  bool subtime;
  subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = lookup_arg_dependent_1 (name, fns, args);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}


/* Add namespace to using_directives. Return NULL_TREE if nothing was
   changed (i.e. there was already a directive), or the fresh
   TREE_LIST otherwise.  */

static tree
push_using_directive_1 (tree used)
{
  tree ud = current_binding_level->using_directives;
  tree iter, ancestor;

  /* Check if we already have this.  */
  if (purpose_member (used, ud) != NULL_TREE)
    return NULL_TREE;

  ancestor = namespace_ancestor (current_decl_namespace (), used);
  ud = current_binding_level->using_directives;
  ud = tree_cons (used, ancestor, ud);
  current_binding_level->using_directives = ud;

  /* Recursively add all namespaces used.  */
  for (iter = DECL_NAMESPACE_USING (used); iter; iter = TREE_CHAIN (iter))
    push_using_directive (TREE_PURPOSE (iter));

  return ud;
}

/* Wrapper for push_using_directive_1.  */

static tree
push_using_directive (tree used)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = push_using_directive_1 (used);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

/* The type TYPE is being declared.  If it is a class template, or a
   specialization of a class template, do any processing required and
   perform error-checking.  If IS_FRIEND is nonzero, this TYPE is
   being declared a friend.  B is the binding level at which this TYPE
   should be bound.

   Returns the TYPE_DECL for TYPE, which may have been altered by this
   processing.  */

static tree
maybe_process_template_type_declaration (tree type, int is_friend,
					 cp_binding_level *b)
{
  tree decl = TYPE_NAME (type);

  if (processing_template_parmlist)
    /* You can't declare a new template type in a template parameter
       list.  But, you can declare a non-template type:

	 template <class A*> struct S;

       is a forward-declaration of `A'.  */
    ;
  else if (b->kind == sk_namespace
	   && current_binding_level->kind != sk_namespace)
    /* If this new type is being injected into a containing scope,
       then it's not a template type.  */
    ;
  else
    {
      gcc_assert (MAYBE_CLASS_TYPE_P (type)
		  || TREE_CODE (type) == ENUMERAL_TYPE);

      if (processing_template_decl)
	{
	  /* This may change after the call to
	     push_template_decl_real, but we want the original value.  */
	  tree name = DECL_NAME (decl);

	  decl = push_template_decl_real (decl, is_friend);
	  if (decl == error_mark_node)
	    return error_mark_node;

	  /* If the current binding level is the binding level for the
	     template parameters (see the comment in
	     begin_template_parm_list) and the enclosing level is a class
	     scope, and we're not looking at a friend, push the
	     declaration of the member class into the class scope.  In the
	     friend case, push_template_decl will already have put the
	     friend into global scope, if appropriate.  */
	  if (TREE_CODE (type) != ENUMERAL_TYPE
	      && !is_friend && b->kind == sk_template_parms
	      && b->level_chain->kind == sk_class)
	    {
	      finish_member_declaration (CLASSTYPE_TI_TEMPLATE (type));

	      if (!COMPLETE_TYPE_P (current_class_type))
		{
		  maybe_add_class_template_decl_list (current_class_type,
						      type, /*friend_p=*/0);
		  /* Put this UTD in the table of UTDs for the class.  */
		  if (CLASSTYPE_NESTED_UTDS (current_class_type) == NULL)
		    CLASSTYPE_NESTED_UTDS (current_class_type) =
		      binding_table_new (SCOPE_DEFAULT_HT_SIZE);

		  binding_table_insert
		    (CLASSTYPE_NESTED_UTDS (current_class_type), name, type);
		}
	    }
	}
    }

  return decl;
}

/* Push a tag name NAME for struct/class/union/enum type TYPE.  In case
   that the NAME is a class template, the tag is processed but not pushed.

   The pushed scope depend on the SCOPE parameter:
   - When SCOPE is TS_CURRENT, put it into the inner-most non-sk_cleanup
     scope.
   - When SCOPE is TS_GLOBAL, put it in the inner-most non-class and
     non-template-parameter scope.  This case is needed for forward
     declarations.
   - When SCOPE is TS_WITHIN_ENCLOSING_NON_CLASS, this is similar to
     TS_GLOBAL case except that names within template-parameter scopes
     are not pushed at all.

   Returns TYPE upon success and ERROR_MARK_NODE otherwise.  */

static tree
pushtag_1 (tree name, tree type, tag_scope scope)
{
  cp_binding_level *b;
  tree decl;

  b = current_binding_level;
  while (/* Cleanup scopes are not scopes from the point of view of
	    the language.  */
	 b->kind == sk_cleanup
	 /* Neither are function parameter scopes.  */
	 || b->kind == sk_function_parms
	 /* Neither are the scopes used to hold template parameters
	    for an explicit specialization.  For an ordinary template
	    declaration, these scopes are not scopes from the point of
	    view of the language.  */
	 || (b->kind == sk_template_parms
	     && (b->explicit_spec_p || scope == ts_global))
	 || (b->kind == sk_class
	     && (scope != ts_current
		 /* We may be defining a new type in the initializer
		    of a static member variable. We allow this when
		    not pedantic, and it is particularly useful for
		    type punning via an anonymous union.  */
		 || COMPLETE_TYPE_P (b->this_entity))))
    b = b->level_chain;

  gcc_assert (identifier_p (name));

  /* Do C++ gratuitous typedefing.  */
  if (identifier_type_value_1 (name) != type)
    {
      tree tdef;
      int in_class = 0;
      tree context = TYPE_CONTEXT (type);

      if (! context)
	{
	  tree cs = current_scope ();

	  if (scope == ts_current
	      || (cs && TREE_CODE (cs) == FUNCTION_DECL))
	    context = cs;
	  else if (cs != NULL_TREE && TYPE_P (cs))
	    /* When declaring a friend class of a local class, we want
	       to inject the newly named class into the scope
	       containing the local class, not the namespace
	       scope.  */
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

      tdef = create_implicit_typedef (name, type);
      DECL_CONTEXT (tdef) = FROB_CONTEXT (context);
      if (scope == ts_within_enclosing_non_class)
	{
	  /* This is a friend.  Make this TYPE_DECL node hidden from
	     ordinary name lookup.  Its corresponding TEMPLATE_DECL
	     will be marked in push_template_decl_real.  */
	  retrofit_lang_decl (tdef);
	  DECL_ANTICIPATED (tdef) = 1;
	  DECL_FRIEND_P (tdef) = 1;
	}

      decl = maybe_process_template_type_declaration
	(type, scope == ts_within_enclosing_non_class, b);
      if (decl == error_mark_node)
	return decl;

      if (b->kind == sk_class)
	{
	  if (!TYPE_BEING_DEFINED (current_class_type))
	    return error_mark_node;

	  if (!PROCESSING_REAL_TEMPLATE_DECL_P ())
	    /* Put this TYPE_DECL on the TYPE_FIELDS list for the
	       class.  But if it's a member template class, we want
	       the TEMPLATE_DECL, not the TYPE_DECL, so this is done
	       later.  */
	    finish_member_declaration (decl);
	  else
	    pushdecl_class_level (decl);
	}
      else if (b->kind != sk_template_parms)
	{
	  decl = pushdecl_with_scope_1 (decl, b, /*is_friend=*/false);
	  if (decl == error_mark_node)
	    return decl;
	}

      if (! in_class)
	set_identifier_type_value_with_scope (name, tdef, b);

      TYPE_CONTEXT (type) = DECL_CONTEXT (decl);

      /* If this is a local class, keep track of it.  We need this
	 information for name-mangling, and so that it is possible to
	 find all function definitions in a translation unit in a
	 convenient way.  (It's otherwise tricky to find a member
	 function definition it's only pointed to from within a local
	 class.)  */
      if (TYPE_FUNCTION_SCOPE_P (type))
	{
	  if (processing_template_decl)
	    {
	      /* Push a DECL_EXPR so we call pushtag at the right time in
		 template instantiation rather than in some nested context.  */
	      add_decl_expr (decl);
	    }
	  else
	    vec_safe_push (local_classes, type);
	}
    }
  if (b->kind == sk_class
      && !COMPLETE_TYPE_P (current_class_type))
    {
      maybe_add_class_template_decl_list (current_class_type,
					  type, /*friend_p=*/0);

      if (CLASSTYPE_NESTED_UTDS (current_class_type) == NULL)
	CLASSTYPE_NESTED_UTDS (current_class_type)
	  = binding_table_new (SCOPE_DEFAULT_HT_SIZE);

      binding_table_insert
	(CLASSTYPE_NESTED_UTDS (current_class_type), name, type);
    }

  decl = TYPE_NAME (type);
  gcc_assert (TREE_CODE (decl) == TYPE_DECL);

  /* Set type visibility now if this is a forward declaration.  */
  TREE_PUBLIC (decl) = 1;
  determine_visibility (decl);

  return type;
}

/* Wrapper for pushtag_1.  */

tree
pushtag (tree name, tree type, tag_scope scope)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = pushtag_1 (name, type, scope);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

/* Subroutines for reverting temporarily to top-level for instantiation
   of templates and such.  We actually need to clear out the class- and
   local-value slots of all identifiers, so that only the global values
   are at all visible.  Simply setting current_binding_level to the global
   scope isn't enough, because more binding levels may be pushed.  */
struct saved_scope *scope_chain;

/* Return true if ID has not already been marked.  */

static inline bool
store_binding_p (tree id)
{
  if (!id || !IDENTIFIER_BINDING (id))
    return false;

  if (IDENTIFIER_MARKED (id))
    return false;

  return true;
}

/* Add an appropriate binding to *OLD_BINDINGS which needs to already
   have enough space reserved.  */

static void
store_binding (tree id, vec<cxx_saved_binding, va_gc> **old_bindings)
{
  cxx_saved_binding saved;

  gcc_checking_assert (store_binding_p (id));

  IDENTIFIER_MARKED (id) = 1;

  saved.identifier = id;
  saved.binding = IDENTIFIER_BINDING (id);
  saved.real_type_value = REAL_IDENTIFIER_TYPE_VALUE (id);
  (*old_bindings)->quick_push (saved);
  IDENTIFIER_BINDING (id) = NULL;
}

static void
store_bindings (tree names, vec<cxx_saved_binding, va_gc> **old_bindings)
{
  static vec<tree> bindings_need_stored = vNULL;
  tree t, id;
  size_t i;

  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  for (t = names; t; t = TREE_CHAIN (t))
    {
      if (TREE_CODE (t) == TREE_LIST)
	id = TREE_PURPOSE (t);
      else
	id = DECL_NAME (t);

      if (store_binding_p (id))
	bindings_need_stored.safe_push (id);
    }
  if (!bindings_need_stored.is_empty ())
    {
      vec_safe_reserve_exact (*old_bindings, bindings_need_stored.length ());
      for (i = 0; bindings_need_stored.iterate (i, &id); ++i)
	{
	  /* We can appearantly have duplicates in NAMES.  */
	  if (store_binding_p (id))
	    store_binding (id, old_bindings);
	}
      bindings_need_stored.truncate (0);
    }
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

/* Like store_bindings, but NAMES is a vector of cp_class_binding
   objects, rather than a TREE_LIST.  */

static void
store_class_bindings (vec<cp_class_binding, va_gc> *names,
		      vec<cxx_saved_binding, va_gc> **old_bindings)
{
  static vec<tree> bindings_need_stored = vNULL;
  size_t i;
  cp_class_binding *cb;

  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  for (i = 0; vec_safe_iterate (names, i, &cb); ++i)
    if (store_binding_p (cb->identifier))
      bindings_need_stored.safe_push (cb->identifier);
  if (!bindings_need_stored.is_empty ())
    {
      tree id;
      vec_safe_reserve_exact (*old_bindings, bindings_need_stored.length ());
      for (i = 0; bindings_need_stored.iterate (i, &id); ++i)
	store_binding (id, old_bindings);
      bindings_need_stored.truncate (0);
    }
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

void
push_to_top_level (void)
{
  struct saved_scope *s;
  cp_binding_level *b;
  cxx_saved_binding *sb;
  size_t i;
  bool need_pop;

  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  s = ggc_cleared_alloc<saved_scope> ();

  b = scope_chain ? current_binding_level : 0;

  /* If we're in the middle of some function, save our state.  */
  if (cfun)
    {
      need_pop = true;
      push_function_context ();
    }
  else
    need_pop = false;

  if (scope_chain && previous_class_level)
    store_class_bindings (previous_class_level->class_shadowed,
			  &s->old_bindings);

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

      store_bindings (b->names, &s->old_bindings);
      /* We also need to check class_shadowed to save class-level type
	 bindings, since pushclass doesn't fill in b->names.  */
      if (b->kind == sk_class)
	store_class_bindings (b->class_shadowed, &s->old_bindings);

      /* Unwind type-value slots back to top level.  */
      for (t = b->type_shadowed; t; t = TREE_CHAIN (t))
	SET_IDENTIFIER_TYPE_VALUE (TREE_PURPOSE (t), TREE_VALUE (t));
    }

  FOR_EACH_VEC_SAFE_ELT (s->old_bindings, i, sb)
    IDENTIFIER_MARKED (sb->identifier) = 0;

  s->prev = scope_chain;
  s->bindings = b;
  s->need_pop_function_context = need_pop;
  s->function_decl = current_function_decl;
  s->unevaluated_operand = cp_unevaluated_operand;
  s->inhibit_evaluation_warnings = c_inhibit_evaluation_warnings;
  s->x_stmt_tree.stmts_are_full_exprs_p = true;

  scope_chain = s;
  current_function_decl = NULL_TREE;
  vec_alloc (current_lang_base, 10);
  current_lang_name = lang_name_cplusplus;
  current_namespace = global_namespace;
  push_class_stack ();
  cp_unevaluated_operand = 0;
  c_inhibit_evaluation_warnings = 0;
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

static void
pop_from_top_level_1 (void)
{
  struct saved_scope *s = scope_chain;
  cxx_saved_binding *saved;
  size_t i;

  /* Clear out class-level bindings cache.  */
  if (previous_class_level)
    invalidate_class_lookup_cache ();
  pop_class_stack ();

  current_lang_base = 0;

  scope_chain = s->prev;
  FOR_EACH_VEC_SAFE_ELT (s->old_bindings, i, saved)
    {
      tree id = saved->identifier;

      IDENTIFIER_BINDING (id) = saved->binding;
      SET_IDENTIFIER_TYPE_VALUE (id, saved->real_type_value);
    }

  /* If we were in the middle of compiling a function, restore our
     state.  */
  if (s->need_pop_function_context)
    pop_function_context ();
  current_function_decl = s->function_decl;
  cp_unevaluated_operand = s->unevaluated_operand;
  c_inhibit_evaluation_warnings = s->inhibit_evaluation_warnings;
}

/* Wrapper for pop_from_top_level_1.  */

void
pop_from_top_level (void)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  pop_from_top_level_1 ();
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
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

/* Emit debugging information for using declarations and directives.
   If input tree is overloaded fn then emit debug info for all
   candidates.  */

void
cp_emit_debug_info_for_using (tree t, tree context)
{
  /* Don't try to emit any debug information if we have errors.  */
  if (seen_error ())
    return;

  /* Ignore this FUNCTION_DECL if it refers to a builtin declaration
     of a builtin function.  */
  if (TREE_CODE (t) == FUNCTION_DECL
      && DECL_EXTERNAL (t)
      && DECL_BUILT_IN (t))
    return;

  /* Do not supply context to imported_module_or_decl, if
     it is a global namespace.  */
  if (context == global_namespace)
    context = NULL_TREE;

  if (BASELINK_P (t))
    t = BASELINK_FUNCTIONS (t);

  /* FIXME: Handle TEMPLATE_DECLs.  */
  for (t = OVL_CURRENT (t); t; t = OVL_NEXT (t))
    if (TREE_CODE (t) != TEMPLATE_DECL)
      {
	if (building_stmt_list_p ())
	  add_stmt (build_stmt (input_location, USING_STMT, t));
	else
	  (*debug_hooks->imported_module_or_decl) (t, NULL_TREE, context, false);
      }
}

#include "gt-cp-name-lookup.h"
