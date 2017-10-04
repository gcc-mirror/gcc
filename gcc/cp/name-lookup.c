/* Definitions for C++ name lookup routines.
   Copyright (C) 2003-2017 Free Software Foundation, Inc.
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
#include "gcc-rich-location.h"
#include "spellcheck-tree.h"
#include "parser.h"

static cxx_binding *cxx_binding_make (tree value, tree type);
static cp_binding_level *innermost_nonclass_level (void);
static void set_identifier_type_value_with_scope (tree id, tree decl,
						  cp_binding_level *b);

/* Create an overload suitable for recording an artificial TYPE_DECL
   and another decl.  We use this machanism to implement the struct
   stat hack within a namespace.  It'd be nice to use it everywhere.  */

#define STAT_HACK_P(N) ((N) && TREE_CODE (N) == OVERLOAD && OVL_LOOKUP_P (N))
#define STAT_TYPE(N) TREE_TYPE (N)
#define STAT_DECL(N) OVL_FUNCTION (N)
#define MAYBE_STAT_DECL(N) (STAT_HACK_P (N) ? STAT_DECL (N) : N)
#define MAYBE_STAT_TYPE(N) (STAT_HACK_P (N) ? STAT_TYPE (N) : NULL_TREE)

/* Create a STAT_HACK node with DECL as the value binding and TYPE as
   the type binding.  */

static tree
stat_hack (tree decl = NULL_TREE, tree type = NULL_TREE)
{
  tree result = make_node (OVERLOAD);

  /* Mark this as a lookup, so we can tell this is a stat hack.  */
  OVL_LOOKUP_P (result) = true;
  STAT_DECL (result) = decl;
  STAT_TYPE (result) = type;
  return result;
}

/* Create a local binding level for NAME.  */

static cxx_binding *
create_local_binding (cp_binding_level *level, tree name)
{
  cxx_binding *binding = cxx_binding_make (NULL, NULL);

  INHERITED_VALUE_BINDING_P (binding) = false;
  LOCAL_BINDING_P (binding) = true;
  binding->scope = level;
  binding->previous = IDENTIFIER_BINDING (name);

  IDENTIFIER_BINDING (name) = binding;
  
  return binding;
}

/* Find the binding for NAME in namespace NS.  If CREATE_P is true,
   make an empty binding if there wasn't one.  */

static tree *
find_namespace_slot (tree ns, tree name, bool create_p = false)
{
  tree *slot;

  if (create_p)
    {
      bool existed;
      slot = &DECL_NAMESPACE_BINDINGS (ns)->get_or_insert (name, &existed);
      if (!existed)
	*slot = NULL_TREE;
    }
  else
    slot = DECL_NAMESPACE_BINDINGS (ns)->get (name);
  return slot;
}

static tree
find_namespace_value (tree ns, tree name)
{
  tree *b = find_namespace_slot (ns, name);

  return b ? MAYBE_STAT_DECL (*b) : NULL_TREE;
}

/* Add DECL to the list of things declared in B.  */

static void
add_decl_to_level (cp_binding_level *b, tree decl)
{
  gcc_assert (b->kind != sk_class);

  /* Make sure we don't create a circular list.  xref_tag can end
     up pushing the same artificial decl more than once.  We
     should have already detected that in update_binding.  */
  gcc_assert (b->names != decl);

  /* We build up the list in reverse order, and reverse it later if
     necessary.  */
  TREE_CHAIN (decl) = b->names;
  b->names = decl;

  /* If appropriate, add decl to separate list of statics.  We
     include extern variables because they might turn out to be
     static later.  It's OK for this list to contain a few false
     positives.  */
  if (b->kind == sk_namespace
      && ((VAR_P (decl)
	   && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
	  || (TREE_CODE (decl) == FUNCTION_DECL
	      && (!TREE_PUBLIC (decl)
		  || decl_anon_ns_mem_p (decl)
		  || DECL_DECLARED_INLINE_P (decl)))))
    vec_safe_push (static_decls, decl);
}

/* Find the binding for NAME in the local binding level B.  */

static cxx_binding *
find_local_binding (cp_binding_level *b, tree name)
{
  if (cxx_binding *binding = IDENTIFIER_BINDING (name))
    for (;; b = b->level_chain)
      {
	if (binding->scope == b
	    && !(VAR_P (binding->value)
		 && DECL_DEAD_FOR_LOCAL (binding->value)))
	  return binding;

	/* Cleanup contours are transparent to the language.  */
	if (b->kind != sk_cleanup)
	  break;
      }
  return NULL;
}

struct name_lookup
{
public:
  typedef std::pair<tree, tree> using_pair;
  typedef vec<using_pair, va_heap, vl_embed> using_queue;

public:
  tree name;	/* The identifier being looked for.  */
  tree value;	/* A (possibly ambiguous) set of things found.  */
  tree type;	/* A type that has been found.  */
  int flags;	/* Lookup flags.  */
  bool deduping; /* Full deduping is needed because using declarations
		    are in play.  */
  vec<tree, va_heap, vl_embed> *scopes;
  name_lookup *previous; /* Previously active lookup.  */

protected:
  /* Marked scope stack for outermost name lookup.  */
  static vec<tree, va_heap, vl_embed> *shared_scopes;
  /* Currently active lookup.  */
  static name_lookup *active;

public:
  name_lookup (tree n, int f = 0)
  : name (n), value (NULL_TREE), type (NULL_TREE), flags (f),
    deduping (false), scopes (NULL), previous (NULL)
  {
    preserve_state ();
  }
  ~name_lookup ()
  {
    restore_state ();
  }

private: /* Uncopyable, unmovable, unassignable. I am a rock. */
  name_lookup (const name_lookup &);
  name_lookup &operator= (const name_lookup &);

protected:
  static bool seen_p (tree scope)
  {
    return LOOKUP_SEEN_P (scope);
  }
  static bool found_p (tree scope)
  {
    return LOOKUP_FOUND_P (scope);
  }
  
  void mark_seen (tree scope); /* Mark and add to scope vector. */
  static void mark_found (tree scope)
  {
    gcc_checking_assert (seen_p (scope));
    LOOKUP_FOUND_P (scope) = true;
  }
  bool see_and_mark (tree scope)
  {
    bool ret = seen_p (scope);
    if (!ret)
      mark_seen (scope);
    return ret;
  }
  bool find_and_mark (tree scope);

private:
  void preserve_state ();
  void restore_state ();

private:
  static tree ambiguous (tree thing, tree current);
  void add_overload (tree fns);
  void add_value (tree new_val);
  void add_type (tree new_type);
  bool process_binding (tree val_bind, tree type_bind);

  /* Look in only namespace.  */
  bool search_namespace_only (tree scope);
  /* Look in namespace and its (recursive) inlines. Ignore using
     directives.  Return true if something found (inc dups). */
  bool search_namespace (tree scope);
  /* Look in the using directives of namespace + inlines using
     qualified lookup rules.  */
  bool search_usings (tree scope);

private:
  using_queue *queue_namespace (using_queue *queue, int depth, tree scope);
  using_queue *do_queue_usings (using_queue *queue, int depth,
				vec<tree, va_gc> *usings);
  using_queue *queue_usings (using_queue *queue, int depth,
			     vec<tree, va_gc> *usings)
  {
    if (usings)
      queue = do_queue_usings (queue, depth, usings);
    return queue;
  }

private:
  void add_fns (tree);

  void adl_expr (tree);
  void adl_type (tree);
  void adl_template_arg (tree);
  void adl_class (tree);
  void adl_bases (tree);
  void adl_class_only (tree);
  void adl_namespace (tree);
  void adl_namespace_only (tree);

public:
  /* Search namespace + inlines + maybe usings as qualified lookup.  */
  bool search_qualified (tree scope, bool usings = true);

  /* Search namespace + inlines + usings as unqualified lookup.  */
  bool search_unqualified (tree scope, cp_binding_level *);

  /* ADL lookup of ARGS.  */
  tree search_adl (tree fns, vec<tree, va_gc> *args);
};

/* Scope stack shared by all outermost lookups.  This avoids us
   allocating and freeing on every single lookup.  */
vec<tree, va_heap, vl_embed> *name_lookup::shared_scopes;

/* Currently active lookup.  */
name_lookup *name_lookup::active;

/* Name lookup is recursive, becase ADL can cause template
   instatiation.  This is of course a rare event, so we optimize for
   it not happening.  When we discover an active name-lookup, which
   must be an ADL lookup,  we need to unmark the marked scopes and also
   unmark the lookup we might have been accumulating.  */

void
name_lookup::preserve_state ()
{
  previous = active;
  if (previous)
    {
      unsigned length = vec_safe_length (previous->scopes);
      vec_safe_reserve (previous->scopes, length * 2);
      for (unsigned ix = length; ix--;)
	{
	  tree decl = (*previous->scopes)[ix];

	  gcc_checking_assert (LOOKUP_SEEN_P (decl));
	  LOOKUP_SEEN_P (decl) = false;

	  /* Preserve the FOUND_P state on the interrupted lookup's
	     stack.  */
	  if (LOOKUP_FOUND_P (decl))
	    {
	      LOOKUP_FOUND_P (decl) = false;
	      previous->scopes->quick_push (decl);
	    }
	}

      /* Unmark the outer partial lookup.  */
      if (previous->deduping)
	lookup_mark (previous->value, false);
    }
  else
    scopes = shared_scopes;
  active = this;
}

/* Restore the marking state of a lookup we interrupted.  */

void
name_lookup::restore_state ()
{
  if (deduping)
    lookup_mark (value, false);

  /* Unmark and empty this lookup's scope stack.  */
  for (unsigned ix = vec_safe_length (scopes); ix--;)
    {
      tree decl = scopes->pop ();
      gcc_checking_assert (LOOKUP_SEEN_P (decl));
      LOOKUP_SEEN_P (decl) = false;
      LOOKUP_FOUND_P (decl) = false;
    }

  active = previous;
  if (previous)
    {
      free (scopes);

      unsigned length = vec_safe_length (previous->scopes);
      for (unsigned ix = 0; ix != length; ix++)
	{
	  tree decl = (*previous->scopes)[ix];
	  if (LOOKUP_SEEN_P (decl))
	    {
	      /* The remainder of the scope stack must be recording
		 FOUND_P decls, which we want to pop off.  */
	      do
		{
		  tree decl = previous->scopes->pop ();
		  gcc_checking_assert (LOOKUP_SEEN_P (decl)
				       && !LOOKUP_FOUND_P (decl));
		  LOOKUP_FOUND_P (decl) = true;
		}
	      while (++ix != length);
	      break;
	    }

	  gcc_checking_assert (!LOOKUP_FOUND_P (decl));
	  LOOKUP_SEEN_P (decl) = true;
	}

      /* Remark the outer partial lookup.  */
      if (previous->deduping)
	lookup_mark (previous->value, true);
    }
  else
    shared_scopes = scopes;
}

void
name_lookup::mark_seen (tree scope)
{
  gcc_checking_assert (!seen_p (scope));
  LOOKUP_SEEN_P (scope) = true;
  vec_safe_push (scopes, scope);
}

bool
name_lookup::find_and_mark (tree scope)
{
  bool result = LOOKUP_FOUND_P (scope);
  if (!result)
    {
      LOOKUP_FOUND_P (scope) = true;
      if (!LOOKUP_SEEN_P (scope))
	vec_safe_push (scopes, scope);
    }

  return result;
}

/* THING and CURRENT are ambiguous, concatenate them.  */

tree
name_lookup::ambiguous (tree thing, tree current)
{
  if (TREE_CODE (current) != TREE_LIST)
    {
      current = build_tree_list (NULL_TREE, current);
      TREE_TYPE (current) = error_mark_node;
    }
  current = tree_cons (NULL_TREE, thing, current);
  TREE_TYPE (current) = error_mark_node;

  return current;
}

/* FNS is a new overload set to add to the exising set.  */

void
name_lookup::add_overload (tree fns)
{
  if (!deduping && TREE_CODE (fns) == OVERLOAD)
    {
      tree probe = fns;
      if (flags & LOOKUP_HIDDEN)
	probe = ovl_skip_hidden (probe);
      if (probe && TREE_CODE (probe) == OVERLOAD && OVL_USING_P (probe))
	{
	  /* We're about to add something found by a using
	     declaration, so need to engage deduping mode.  */
	  lookup_mark (value, true);
	  deduping = true;
	}
    }

  value = lookup_maybe_add (fns, value, deduping);
}

/* Add a NEW_VAL, a found value binding into the current value binding.  */

void
name_lookup::add_value (tree new_val)
{
  if (OVL_P (new_val) && (!value || OVL_P (value)))
    add_overload (new_val);
  else if (!value)
    value = new_val;
  else if (value == new_val)
    ;
  else if ((TREE_CODE (value) == TYPE_DECL
	    && TREE_CODE (new_val) == TYPE_DECL
	    && same_type_p (TREE_TYPE (value), TREE_TYPE (new_val))))
    /* Typedefs to the same type. */;
  else if (TREE_CODE (value) == NAMESPACE_DECL
	   && TREE_CODE (new_val) == NAMESPACE_DECL
	   && ORIGINAL_NAMESPACE (value) == ORIGINAL_NAMESPACE (new_val))
    /* Namespace (possibly aliased) to the same namespace.  Locate
       the namespace*/
    value = ORIGINAL_NAMESPACE (value);
  else
    {
      if (deduping)
	{
	  /* Disengage deduping mode.  */
	  lookup_mark (value, false);
	  deduping = false;
	}
      value = ambiguous (new_val, value);
    }
}

/* Add a NEW_TYPE, a found type binding into the current type binding.  */

void
name_lookup::add_type (tree new_type)
{
  if (!type)
    type = new_type;
  else if (TREE_CODE (type) == TREE_LIST
	   || !same_type_p (TREE_TYPE (type), TREE_TYPE (new_type)))
    type = ambiguous (new_type, type);
}

/* Process a found binding containing NEW_VAL and NEW_TYPE.  Returns
   true if we actually found something noteworthy.  */

bool
name_lookup::process_binding (tree new_val, tree new_type)
{
  /* Did we really see a type? */
  if (new_type
      && (LOOKUP_NAMESPACES_ONLY (flags)
	  || (!(flags & LOOKUP_HIDDEN)
	      && DECL_LANG_SPECIFIC (new_type)
	      && DECL_ANTICIPATED (new_type))))
    new_type = NULL_TREE;

  if (new_val && !(flags & LOOKUP_HIDDEN))
    new_val = ovl_skip_hidden (new_val);

  /* Do we really see a value? */
  if (new_val)
    switch (TREE_CODE (new_val))
      {
      case TEMPLATE_DECL:
	/* If we expect types or namespaces, and not templates,
	   or this is not a template class.  */
	if ((LOOKUP_QUALIFIERS_ONLY (flags)
	     && !DECL_TYPE_TEMPLATE_P (new_val)))
	  new_val = NULL_TREE;
	break;
      case TYPE_DECL:
	if (LOOKUP_NAMESPACES_ONLY (flags)
	    || (new_type && (flags & LOOKUP_PREFER_TYPES)))
	  new_val = NULL_TREE;
	break;
      case NAMESPACE_DECL:
	if (LOOKUP_TYPES_ONLY (flags))
	  new_val = NULL_TREE;
	break;
      default:
	if (LOOKUP_QUALIFIERS_ONLY (flags))
	  new_val = NULL_TREE;
      }

  if (!new_val)
    {
      new_val = new_type;
      new_type = NULL_TREE;
    }

  /* Merge into the lookup  */
  if (new_val)
    add_value (new_val);
  if (new_type)
    add_type (new_type);

  return new_val != NULL_TREE;
}

/* Look in exactly namespace SCOPE.  */

bool
name_lookup::search_namespace_only (tree scope)
{
  bool found = false;

  if (tree *binding = find_namespace_slot (scope, name))
    found |= process_binding (MAYBE_STAT_DECL (*binding),
			      MAYBE_STAT_TYPE (*binding));

  return found;
}

/* Conditionally look in namespace SCOPE and inline children.  */

bool
name_lookup::search_namespace (tree scope)
{
  if (see_and_mark (scope))
    /* We've visited this scope before.  Return what we found then.  */
    return found_p (scope);

  /* Look in exactly namespace. */
  bool found = search_namespace_only (scope);
  
  /* Recursively look in its inline children.  */
  if (vec<tree, va_gc> *inlinees = DECL_NAMESPACE_INLINEES (scope))
    for (unsigned ix = inlinees->length (); ix--;)
      found |= search_namespace ((*inlinees)[ix]);

  if (found)
    mark_found (scope);

  return found;
}

/* Recursively follow using directives of SCOPE & its inline children.
   Such following is essentially a flood-fill algorithm.  */

bool
name_lookup::search_usings (tree scope)
{
  /* We do not check seen_p here, as that was already set during the
     namespace_only walk.  */
  if (found_p (scope))
    return true;

  bool found = false;
  if (vec<tree, va_gc> *usings = DECL_NAMESPACE_USING (scope))
    for (unsigned ix = usings->length (); ix--;)
      found |= search_qualified ((*usings)[ix], true);

  /* Look in its inline children.  */
  if (vec<tree, va_gc> *inlinees = DECL_NAMESPACE_INLINEES (scope))
    for (unsigned ix = inlinees->length (); ix--;)
      found |= search_usings ((*inlinees)[ix]);

  if (found)
    mark_found (scope);

  return found;
}

/* Qualified namespace lookup in SCOPE.
   1) Look in SCOPE (+inlines).  If found, we're done.
   2) Otherwise, if USINGS is true,
      recurse for every using directive of SCOPE (+inlines).

   Trickiness is (a) loops and (b) multiple paths to same namespace.
   In both cases we want to not repeat any lookups, and know whether
   to stop the caller's step #2.  Do this via the FOUND_P marker.  */

bool
name_lookup::search_qualified (tree scope, bool usings)
{
  bool found = false;

  if (seen_p (scope))
    found = found_p (scope);
  else 
    {
      found = search_namespace (scope);
      if (!found && usings)
	found = search_usings (scope);
    }

  return found;
}

/* Add SCOPE to the unqualified search queue, recursively add its
   inlines and those via using directives.  */

name_lookup::using_queue *
name_lookup::queue_namespace (using_queue *queue, int depth, tree scope)
{
  if (see_and_mark (scope))
    return queue;

  /* Record it.  */
  tree common = scope;
  while (SCOPE_DEPTH (common) > depth)
    common = CP_DECL_CONTEXT (common);
  vec_safe_push (queue, using_pair (common, scope));

  /* Queue its inline children.  */
  if (vec<tree, va_gc> *inlinees = DECL_NAMESPACE_INLINEES (scope))
    for (unsigned ix = inlinees->length (); ix--;)
      queue = queue_namespace (queue, depth, (*inlinees)[ix]);

  /* Queue its using targets.  */
  queue = queue_usings (queue, depth, DECL_NAMESPACE_USING (scope));

  return queue;
}

/* Add the namespaces in USINGS to the unqualified search queue.  */

name_lookup::using_queue *
name_lookup::do_queue_usings (using_queue *queue, int depth,
			      vec<tree, va_gc> *usings)
{
  for (unsigned ix = usings->length (); ix--;)
    queue = queue_namespace (queue, depth, (*usings)[ix]);

  return queue;
}

/* Unqualified namespace lookup in SCOPE.
   1) add scope+inlins to worklist.
   2) recursively add target of every using directive
   3) for each worklist item where SCOPE is common ancestor, search it
   4) if nothing find, scope=parent, goto 1.  */

bool
name_lookup::search_unqualified (tree scope, cp_binding_level *level)
{
  /* Make static to avoid continual reallocation.  We're not
     recursive.  */
  static using_queue *queue = NULL;
  bool found = false;
  int length = vec_safe_length (queue);

  /* Queue local using-directives.  */
  for (; level->kind != sk_namespace; level = level->level_chain)
    queue = queue_usings (queue, SCOPE_DEPTH (scope), level->using_directives);

  for (; !found; scope = CP_DECL_CONTEXT (scope))
    {
      gcc_assert (!DECL_NAMESPACE_ALIAS (scope));
      int depth = SCOPE_DEPTH (scope);

      /* Queue namespaces reachable from SCOPE. */
      queue = queue_namespace (queue, depth, scope);

      /* Search every queued namespace where SCOPE is the common
	 ancestor.  Adjust the others.  */
      unsigned ix = length;
      do
	{
	  using_pair &pair = (*queue)[ix];
	  while (pair.first == scope)
	    {
	      found |= search_namespace_only (pair.second);
	      pair = queue->pop ();
	      if (ix == queue->length ())
		goto done;
	    }
	  /* The depth is the same as SCOPE, find the parent scope.  */
	  if (SCOPE_DEPTH (pair.first) == depth)
	    pair.first = CP_DECL_CONTEXT (pair.first);
	  ix++;
	}
      while (ix < queue->length ());
    done:;
      if (scope == global_namespace)
	break;
    }

  vec_safe_truncate (queue, length);

  return found;
}

/* FNS is a value binding.  If it is a (set of overloaded) functions,
   add them into the current value.  */

void
name_lookup::add_fns (tree fns)
{
  if (!fns)
    return;
  else if (TREE_CODE (fns) == OVERLOAD)
    {
      if (TREE_TYPE (fns) != unknown_type_node)
	fns = OVL_FUNCTION (fns);
    }
  else if (!DECL_DECLARES_FUNCTION_P (fns))
    return;

  add_overload (fns);
}

/* Add functions of a namespace to the lookup structure.  */

void
name_lookup::adl_namespace_only (tree scope)
{
  mark_seen (scope);

  /* Look down into inline namespaces.  */
  if (vec<tree, va_gc> *inlinees = DECL_NAMESPACE_INLINEES (scope))
    for (unsigned ix = inlinees->length (); ix--;)
      adl_namespace_only ((*inlinees)[ix]);

  if (tree fns = find_namespace_value (scope, name))
    add_fns (ovl_skip_hidden (fns));
}

/* Find the containing non-inlined namespace, add it and all its
   inlinees.  */

void
name_lookup::adl_namespace (tree scope)
{
  if (seen_p (scope))
    return;

  /* Find the containing non-inline namespace.  */
  while (DECL_NAMESPACE_INLINE_P (scope))
    scope = CP_DECL_CONTEXT (scope);

  adl_namespace_only (scope);
}

/* Adds the class and its friends to the lookup structure.  */

void
name_lookup::adl_class_only (tree type)
{
  /* Backend-built structures, such as __builtin_va_list, aren't
     affected by all this.  */
  if (!CLASS_TYPE_P (type))
    return;

  type = TYPE_MAIN_VARIANT (type);

  if (see_and_mark (type))
    return;

  tree context = decl_namespace_context (type);
  adl_namespace (context);

  complete_type (type);

  /* Add friends.  */
  for (tree list = DECL_FRIENDLIST (TYPE_MAIN_DECL (type)); list;
       list = TREE_CHAIN (list))
    if (name == FRIEND_NAME (list))
      for (tree friends = FRIEND_DECLS (list); friends;
	   friends = TREE_CHAIN (friends))
	{
	  tree fn = TREE_VALUE (friends);

	  /* Only interested in global functions with potentially hidden
	     (i.e. unqualified) declarations.  */
	  if (CP_DECL_CONTEXT (fn) != context)
	    continue;

	  /* Only interested in anticipated friends.  (Non-anticipated
	     ones will have been inserted during the namespace
	     adl.)  */
	  if (!DECL_ANTICIPATED (fn))
	    continue;

	  /* Template specializations are never found by name lookup.
	     (Templates themselves can be found, but not template
	     specializations.)  */
	  if (TREE_CODE (fn) == FUNCTION_DECL && DECL_USE_TEMPLATE (fn))
	    continue;

	  add_fns (fn);
	}
}

/* Adds the class and its bases to the lookup structure.
   Returns true on error.  */

void
name_lookup::adl_bases (tree type)
{
  adl_class_only (type);

  /* Process baseclasses.  */
  if (tree binfo = TYPE_BINFO (type))
    {
      tree base_binfo;
      int i;

      for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	adl_bases (BINFO_TYPE (base_binfo));
    }
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

void
name_lookup::adl_class (tree type)
{
  /* Backend build structures, such as __builtin_va_list, aren't
     affected by all this.  */
  if (!CLASS_TYPE_P (type))
    return;

  type = TYPE_MAIN_VARIANT (type);
  /* We don't set found here because we have to have set seen first,
     which is done in the adl_bases walk.  */
  if (found_p (type))
    return;

  adl_bases (type);
  mark_found (type);

  if (TYPE_CLASS_SCOPE_P (type))
    adl_class_only (TYPE_CONTEXT (type));

  /* Process template arguments.  */
  if (CLASSTYPE_TEMPLATE_INFO (type)
      && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (type)))
    {
      tree list = INNERMOST_TEMPLATE_ARGS (CLASSTYPE_TI_ARGS (type));
      for (int i = 0; i < TREE_VEC_LENGTH (list); ++i)
	adl_template_arg (TREE_VEC_ELT (list, i));
    }
}

void
name_lookup::adl_expr (tree expr)
{
  if (!expr)
    return;

  gcc_assert (!TYPE_P (expr));

  if (TREE_TYPE (expr) != unknown_type_node)
    {
      adl_type (TREE_TYPE (expr));
      return;
    }

  if (TREE_CODE (expr) == ADDR_EXPR)
    expr = TREE_OPERAND (expr, 0);
  if (TREE_CODE (expr) == COMPONENT_REF
      || TREE_CODE (expr) == OFFSET_REF)
    expr = TREE_OPERAND (expr, 1);
  expr = MAYBE_BASELINK_FUNCTIONS (expr);

  if (OVL_P (expr))
    for (lkp_iterator iter (expr); iter; ++iter)
      adl_type (TREE_TYPE (*iter));
  else if (TREE_CODE (expr) == TEMPLATE_ID_EXPR)
    {
      /* The working paper doesn't currently say how to handle
	 template-id arguments.  The sensible thing would seem to be
	 to handle the list of template candidates like a normal
	 overload set, and handle the template arguments like we do
	 for class template specializations.  */

      /* First the templates.  */
      adl_expr (TREE_OPERAND (expr, 0));

      /* Now the arguments.  */
      if (tree args = TREE_OPERAND (expr, 1))
	for (int ix = TREE_VEC_LENGTH (args); ix--;)
	  adl_template_arg (TREE_VEC_ELT (args, ix));
    }
}

void
name_lookup::adl_type (tree type)
{
  if (!type)
    return;

  if (TYPE_PTRDATAMEM_P (type))
    {
      /* Pointer to member: associate class type and value type.  */
      adl_type (TYPE_PTRMEM_CLASS_TYPE (type));
      adl_type (TYPE_PTRMEM_POINTED_TO_TYPE (type));
      return;
    }

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (type))
	{
	  adl_type (TYPE_PTRMEMFUNC_FN_TYPE (type));
	  return;
	}
      /* FALLTHRU */
    case UNION_TYPE:
      adl_class (type);
      return;

    case METHOD_TYPE:
      /* The basetype is referenced in the first arg type, so just
	 fall through.  */
    case FUNCTION_TYPE:
      /* Associate the parameter types.  */
      for (tree args = TYPE_ARG_TYPES (type); args; args = TREE_CHAIN (args))
	adl_type (TREE_VALUE (args));
      /* FALLTHROUGH */

    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case ARRAY_TYPE:
      adl_type (TREE_TYPE (type));
      return;

    case ENUMERAL_TYPE:
      if (TYPE_CLASS_SCOPE_P (type))
	adl_class_only (TYPE_CONTEXT (type));
      adl_namespace (decl_namespace_context (type));
      return;

    case LANG_TYPE:
      gcc_assert (type == unknown_type_node
		  || type == init_list_type_node);
      return;

    case TYPE_PACK_EXPANSION:
      adl_type (PACK_EXPANSION_PATTERN (type));
      return;

    default:
      break;
    }
}

/* Adds everything associated with a template argument to the lookup
   structure.  */

void
name_lookup::adl_template_arg (tree arg)
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
    ;
  else if (TREE_CODE (arg) == TEMPLATE_DECL)
    {
      tree ctx = CP_DECL_CONTEXT (arg);

      /* It's not a member template.  */
      if (TREE_CODE (ctx) == NAMESPACE_DECL)
	adl_namespace (ctx);
      /* Otherwise, it must be member template.  */
      else
	adl_class_only (ctx);
    }
  /* It's an argument pack; handle it recursively.  */
  else if (ARGUMENT_PACK_P (arg))
    {
      tree args = ARGUMENT_PACK_ARGS (arg);
      int i, len = TREE_VEC_LENGTH (args);
      for (i = 0; i < len; ++i) 
	adl_template_arg (TREE_VEC_ELT (args, i));
    }
  /* It's not a template template argument, but it is a type template
     argument.  */
  else if (TYPE_P (arg))
    adl_type (arg);
}

/* Perform ADL lookup.  FNS is the existing lookup result and ARGS are
   the call arguments.  */

tree
name_lookup::search_adl (tree fns, vec<tree, va_gc> *args)
{
  if (fns)
    {
      deduping = true;
      lookup_mark (fns, true);
    }
  value = fns;

  unsigned ix;
  tree arg;

  FOR_EACH_VEC_ELT_REVERSE (*args, ix, arg)
    /* OMP reduction operators put an ADL-significant type as the
       first arg. */
    if (TYPE_P (arg))
      adl_type (arg);
    else
      adl_expr (arg);

  fns = value;

  return fns;
}

static bool qualified_namespace_lookup (tree, name_lookup *);
static void consider_binding_level (tree name,
				    best_match <tree, const char *> &bm,
				    cp_binding_level *lvl,
				    bool look_within_fields,
				    enum lookup_name_fuzzy_kind kind);
static void diagnose_name_conflict (tree, tree);

/* ADL lookup of NAME.  FNS is the result of regular lookup, and we
   don't add duplicates to it.  ARGS is the vector of call
   arguments (which will not be empty).  */

tree
lookup_arg_dependent (tree name, tree fns, vec<tree, va_gc> *args)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  name_lookup lookup (name);
  fns = lookup.search_adl (fns, args);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return fns;
}

/* FNS is an overload set of conversion functions.  Return the
   overloads converting to TYPE.  */

static tree
extract_conversion_operator (tree fns, tree type)
{
  tree convs = NULL_TREE;
  tree tpls = NULL_TREE;

  for (ovl_iterator iter (fns); iter; ++iter)
    {
      if (same_type_p (DECL_CONV_FN_TYPE (*iter), type))
	convs = lookup_add (*iter, convs);

      if (TREE_CODE (*iter) == TEMPLATE_DECL)
	tpls = lookup_add (*iter, tpls);
    }

  if (!convs)
    convs = tpls;

  return convs;
}

/* Binary search of (ordered) MEMBER_VEC for NAME.  */

static tree
member_vec_binary_search (vec<tree, va_gc> *member_vec, tree name)
{
  for (unsigned lo = 0, hi = member_vec->length (); lo < hi;)
    {
      unsigned mid = (lo + hi) / 2;
      tree binding = (*member_vec)[mid];
      tree binding_name = OVL_NAME (binding);

      if (binding_name > name)
	hi = mid;
      else if (binding_name < name)
	lo = mid + 1;
      else
	return binding;
    }

  return NULL_TREE;
}

/* Linear search of (unordered) MEMBER_VEC for NAME.  */

static tree
member_vec_linear_search (vec<tree, va_gc> *member_vec, tree name)
{
  for (int ix = member_vec->length (); ix--;)
    /* We can get a NULL binding during insertion of a new method
       name, because the identifier_binding machinery performs a
       lookup.  If we find such a NULL slot, that's the thing we were
       looking for, so we might as well bail out immediately.  */
    if (tree binding = (*member_vec)[ix])
      {
	if (OVL_NAME (binding) == name)
	  return binding;
      }
    else
      break;

  return NULL_TREE;
}

/* Linear search of (partially ordered) fields of KLASS for NAME.  */

static tree
fields_linear_search (tree klass, tree name, bool want_type)
{
  for (tree fields = TYPE_FIELDS (klass); fields; fields = DECL_CHAIN (fields))
    {
      tree decl = fields;

      if (!want_type
	  && TREE_CODE (decl) == FIELD_DECL
	  && ANON_AGGR_TYPE_P (TREE_TYPE (decl)))
	{
	  tree anon = TREE_TYPE (decl);
	  gcc_assert (COMPLETE_TYPE_P (anon));
	  tree temp;
	  
	  if (vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (anon))
	    temp = member_vec_linear_search (member_vec, name);
	  else
	    temp = fields_linear_search (anon, name, want_type);

	  if (temp)
	    {
	      /* Anon members can only contain fields.  */
	      gcc_assert (!STAT_HACK_P (temp) && !DECL_DECLARES_TYPE_P (temp));
	      return temp;
	    }
	}

      if (DECL_NAME (decl) != name)
	continue;
      
      if (TREE_CODE (decl) == USING_DECL)
	{
	  decl = strip_using_decl (decl);
	  if (is_overloaded_fn (decl))
	    continue;
	}

      if (DECL_DECLARES_FUNCTION_P (decl))
	/* Functions are found separately.  */
	continue;

      if (!want_type || DECL_DECLARES_TYPE_P (decl))
	return decl;
    }

  return NULL_TREE;
}

/* Look for NAME as an immediate member of KLASS (including
   anon-members or unscoped enum member).  TYPE_OR_FNS is zero for
   regular search.  >0 to get a type binding (if there is one) and <0
   if you want (just) the member function binding.

   Use this if you do not want lazy member creation.  */

tree
get_class_binding_direct (tree klass, tree name, int type_or_fns)
{
  gcc_checking_assert (RECORD_OR_UNION_TYPE_P (klass));

  /* Conversion operators can only be found by the marker conversion
     operator name.  */
  bool conv_op = IDENTIFIER_CONV_OP_P (name);
  tree lookup = conv_op ? conv_op_identifier : name;
  tree val = NULL_TREE;
  vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (klass);

  if (COMPLETE_TYPE_P (klass) && member_vec)
    {
      val = member_vec_binary_search (member_vec, lookup);
      if (!val)
	;
      else if (type_or_fns > 0)
	{
	  if (STAT_HACK_P (val))
	    val = STAT_TYPE (val);
	  else if (!DECL_DECLARES_TYPE_P (val))
	    val = NULL_TREE;
	}
      else if (STAT_HACK_P (val))
	val = STAT_DECL (val);

      if (val && TREE_CODE (val) == OVERLOAD
	  && TREE_CODE (OVL_FUNCTION (val)) == USING_DECL)
	{
	  /* An overload with a dependent USING_DECL.  Does the caller
	     want the USING_DECL or the functions?  */
	  if (type_or_fns < 0)
	    val = OVL_CHAIN (val);
	  else
	    val = OVL_FUNCTION (val);  
	}
    }
  else
    {
      if (member_vec && type_or_fns <= 0)
	val = member_vec_linear_search (member_vec, lookup);

      if (type_or_fns < 0)
	/* Don't bother looking for field.  We don't want it.  */;
      else if (!val || (TREE_CODE (val) == OVERLOAD && OVL_USING_P (val)))
	/* Dependent using declarations are a 'field', make sure we
	   return that even if we saw an overload already.  */
	if (tree field_val = fields_linear_search (klass, lookup,
						   type_or_fns > 0))
	  if (!val || TREE_CODE (field_val) == USING_DECL)
	    val = field_val;
    }

  /* Extract the conversion operators asked for, unless the general
     conversion operator was requested.   */
  if (val && conv_op)
    {
      gcc_checking_assert (OVL_FUNCTION (val) == conv_op_marker);
      val = OVL_CHAIN (val);
      if (tree type = TREE_TYPE (name))
	val = extract_conversion_operator (val, type);
    }

  return val;
}

/* Look for NAME's binding in exactly KLASS.  See
   get_class_binding_direct for argument description.  Does lazy
   special function creation as necessary.  */

tree
get_class_binding (tree klass, tree name, int type_or_fns)
{
  klass = complete_type (klass);

  if (COMPLETE_TYPE_P (klass))
    {
      /* Lazily declare functions, if we're going to search these.  */
      if (IDENTIFIER_CTOR_P (name))
	{
	  if (CLASSTYPE_LAZY_DEFAULT_CTOR (klass))
	    lazily_declare_fn (sfk_constructor, klass);
	  if (CLASSTYPE_LAZY_COPY_CTOR (klass))
	    lazily_declare_fn (sfk_copy_constructor, klass);
	  if (CLASSTYPE_LAZY_MOVE_CTOR (klass))
	    lazily_declare_fn (sfk_move_constructor, klass);
	}
      else if (IDENTIFIER_DTOR_P (name))
	{
	  if (CLASSTYPE_LAZY_DESTRUCTOR (klass))
	    lazily_declare_fn (sfk_destructor, klass);
	}
      else if (name == cp_assignment_operator_id (NOP_EXPR))
	{
	  if (CLASSTYPE_LAZY_COPY_ASSIGN (klass))
	    lazily_declare_fn (sfk_copy_assignment, klass);
	  if (CLASSTYPE_LAZY_MOVE_ASSIGN (klass))
	    lazily_declare_fn (sfk_move_assignment, klass);
	}
    }

  return get_class_binding_direct (klass, name, type_or_fns);
}

/* Find the slot containing overloads called 'NAME'.  If there is no
   such slot, create an empty one.  KLASS might be complete at this
   point, in which case we need to preserve ordering.  Deals with
   conv_op marker handling.  */

tree *
get_member_slot (tree klass, tree name)
{
  bool complete_p = COMPLETE_TYPE_P (klass);
  
  vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (klass);
  if (!member_vec)
    {
      vec_alloc (member_vec, 8);
      CLASSTYPE_MEMBER_VEC (klass) = member_vec;
      if (complete_p)
	{
	  /* If the class is complete but had no member_vec, we need
	     to add the TYPE_FIELDS into it.  We're also most likely
	     to be adding ctors & dtors, so ask for 6 spare slots (the
	     abstract cdtors and their clones).  */
	  set_class_bindings (klass, 6);
	  member_vec = CLASSTYPE_MEMBER_VEC (klass);
	}
    }

  if (IDENTIFIER_CONV_OP_P (name))
    name = conv_op_identifier;

  unsigned ix, length = member_vec->length ();
  for (ix = 0; ix < length; ix++)
    {
      tree *slot = &(*member_vec)[ix];
      tree fn_name = OVL_NAME (*slot);

      if (fn_name == name)
	{
	  /* If we found an existing slot, it must be a function set.
	     Even with insertion after completion, because those only
	     happen with artificial fns that have unspellable names.
	     This means we do not have to deal with the stat hack
	     either.  */
	  gcc_checking_assert (OVL_P (*slot));
	  if (name == conv_op_identifier)
	    {
	      gcc_checking_assert (OVL_FUNCTION (*slot) == conv_op_marker);
	      /* Skip the conv-op marker. */
	      slot = &OVL_CHAIN (*slot);
	    }
	  return slot;
	}

      if (complete_p && fn_name > name)
	break;
    }

  /* No slot found.  Create one at IX.  We know in this case that our
     caller will succeed in adding the function.  */
  if (complete_p)
    {
      /* Do exact allocation when complete, as we don't expect to add
	 many.  */
      vec_safe_reserve_exact (member_vec, 1);
      member_vec->quick_insert (ix, NULL_TREE);
    }
  else
    {
      gcc_checking_assert (ix == length);
      vec_safe_push (member_vec, NULL_TREE);
    }
  CLASSTYPE_MEMBER_VEC (klass) = member_vec;

  tree *slot = &(*member_vec)[ix];
  if (name == conv_op_identifier)
    {
      /* Install the marker prefix.  */
      *slot = ovl_make (conv_op_marker, NULL_TREE);
      slot = &OVL_CHAIN (*slot);
    }

  return slot;
}

/* Comparison function to compare two MEMBER_VEC entries by name.
   Because we can have duplicates during insertion of TYPE_FIELDS, we
   do extra checking so deduping doesn't have to deal with so many
   cases.  */

static int
member_name_cmp (const void *a_p, const void *b_p)
{
  tree a = *(const tree *)a_p;
  tree b = *(const tree *)b_p;
  tree name_a = DECL_NAME (TREE_CODE (a) == OVERLOAD ? OVL_FUNCTION (a) : a);
  tree name_b = DECL_NAME (TREE_CODE (b) == OVERLOAD ? OVL_FUNCTION (b) : b);

  gcc_checking_assert (name_a && name_b);
  if (name_a != name_b)
    return name_a < name_b ? -1 : +1;

  if (name_a == conv_op_identifier)
    {
      /* Strip the conv-op markers. */
      gcc_checking_assert (OVL_FUNCTION (a) == conv_op_marker
			   && OVL_FUNCTION (b) == conv_op_marker);
      a = OVL_CHAIN (a);
      b = OVL_CHAIN (b);
    }

  if (TREE_CODE (a) == OVERLOAD)
    a = OVL_FUNCTION (a);
  if (TREE_CODE (b) == OVERLOAD)
    b = OVL_FUNCTION (b);

  /* We're in STAT_HACK or USING_DECL territory (or possibly error-land). */
  if (TREE_CODE (a) != TREE_CODE (b))
    {
      /* If one of them is a TYPE_DECL, it loses.  */
      if (TREE_CODE (a) == TYPE_DECL)
	return +1;
      else if (TREE_CODE (b) == TYPE_DECL)
	return -1;

      /* If one of them is a USING_DECL, it loses.  */
      if (TREE_CODE (a) == USING_DECL)
	return +1;
      else if (TREE_CODE (b) == USING_DECL)
	return -1;

      /* There are no other cases with different kinds of decls, as
	 duplicate detection should have kicked in earlier.  However,
	 some erroneous cases get though. */
      gcc_assert (errorcount);
    }
  
  /* Using source location would be the best thing here, but we can
     get identically-located decls in the following circumstances:

     1) duplicate artificial type-decls for the same type.

     2) pack expansions of using-decls.

     We should not be doing #1, but in either case it doesn't matter
     how we order these.  Use UID as a proxy for source ordering, so
     that identically-located decls still have a well-defined stable
     ordering.  */
  return DECL_UID (a) < DECL_UID (b) ? -1 : +1;
}

static struct {
  gt_pointer_operator new_value;
  void *cookie;
} resort_data;

/* This routine compares two fields like member_name_cmp but using the
   pointer operator in resort_field_decl_data.  We don't have to deal
   with duplicates here.  */

static int
resort_member_name_cmp (const void *a_p, const void *b_p)
{
  tree a = *(const tree *)a_p;
  tree b = *(const tree *)b_p;
  tree name_a = OVL_NAME (a);
  tree name_b = OVL_NAME (b);

  resort_data.new_value (&name_a, resort_data.cookie);
  resort_data.new_value (&name_b, resort_data.cookie);

  gcc_checking_assert (name_a != name_b);

  return name_a < name_b ? -1 : +1;
}

/* Resort CLASSTYPE_MEMBER_VEC because pointers have been reordered.  */

void
resort_type_member_vec (void *obj, void */*orig_obj*/,
			gt_pointer_operator new_value, void* cookie)
{
  if (vec<tree, va_gc> *member_vec = (vec<tree, va_gc> *) obj)
    {
      resort_data.new_value = new_value;
      resort_data.cookie = cookie;
      qsort (member_vec->address (), member_vec->length (),
	     sizeof (tree), resort_member_name_cmp);
    }
}

/* Recursively count the number of fields in KLASS, including anonymous
   union members.  */

static unsigned
count_class_fields (tree klass)
{
  unsigned n_fields = 0;

  for (tree fields = TYPE_FIELDS (klass); fields; fields = DECL_CHAIN (fields))
    if (DECL_DECLARES_FUNCTION_P (fields))
      /* Functions are dealt with separately.  */;
    else if (TREE_CODE (fields) == FIELD_DECL
	     && ANON_AGGR_TYPE_P (TREE_TYPE (fields)))
      n_fields += count_class_fields (TREE_TYPE (fields));
    else if (DECL_NAME (fields))
      n_fields += 1;

  return n_fields;
}

/* Append all the nonfunction members fields of KLASS to MEMBER_VEC.
   Recurse for anonymous members.  MEMBER_VEC must have space.  */

static void
member_vec_append_class_fields (vec<tree, va_gc> *member_vec, tree klass)
{
  for (tree fields = TYPE_FIELDS (klass); fields; fields = DECL_CHAIN (fields))
    if (DECL_DECLARES_FUNCTION_P (fields))
      /* Functions are handled separately.  */;
    else if (TREE_CODE (fields) == FIELD_DECL
	     && ANON_AGGR_TYPE_P (TREE_TYPE (fields)))
      member_vec_append_class_fields (member_vec, TREE_TYPE (fields));
    else if (DECL_NAME (fields))
      {
	tree field = fields;
	/* Mark a conv-op USING_DECL with the conv-op-marker.  */
	if (TREE_CODE (field) == USING_DECL
	    && IDENTIFIER_CONV_OP_P (DECL_NAME (field)))
	  field = ovl_make (conv_op_marker, field);
	member_vec->quick_push (field);
      }
}

/* Append all of the enum values of ENUMTYPE to MEMBER_VEC.
   MEMBER_VEC must have space.  */

static void
member_vec_append_enum_values (vec<tree, va_gc> *member_vec, tree enumtype)
{
  for (tree values = TYPE_VALUES (enumtype);
       values; values = TREE_CHAIN (values))
    member_vec->quick_push (TREE_VALUE (values));
}

/* MEMBER_VEC has just had new DECLs added to it, but is sorted.
   DeDup adjacent DECLS of the same name.  We already dealt with
   conflict resolution when adding the fields or methods themselves.
   There are three cases (which could all be combined):
   1) a TYPE_DECL and non TYPE_DECL.  Deploy STAT_HACK as appropriate.
   2) a USING_DECL and an overload.  If the USING_DECL is dependent,
   it wins.  Otherwise the OVERLOAD does.
   3) two USING_DECLS. ...

   member_name_cmp will have ordered duplicates as
   <fns><using><type>  */

static void
member_vec_dedup (vec<tree, va_gc> *member_vec)
{
  unsigned len = member_vec->length ();
  unsigned store = 0;

  tree current = (*member_vec)[0], name = OVL_NAME (current);
  tree next = NULL_TREE, next_name = NULL_TREE;
  for (unsigned jx, ix = 0; ix < len;
       ix = jx, current = next, name = next_name)
    {
      tree to_type = NULL_TREE;
      tree to_using = NULL_TREE;
      tree marker = NULL_TREE;
      if (IDENTIFIER_CONV_OP_P (name))
	{
	  marker = current;
	  current = OVL_CHAIN (current);
	  name = DECL_NAME (OVL_FUNCTION (marker));
	  gcc_checking_assert (name == conv_op_identifier);
	}

      if (TREE_CODE (current) == USING_DECL)
	{
	  current = strip_using_decl (current);
	  if (is_overloaded_fn (current))
	    current = NULL_TREE;
	  else if (TREE_CODE (current) == USING_DECL)
	    {
	      to_using = current;
	      current = NULL_TREE;
	    }
	}

      if (current && DECL_DECLARES_TYPE_P (current))
	{
	  to_type = current;
	  current = NULL_TREE;
	}

      for (jx = ix + 1; jx < len; jx++)
	{
	  next = (*member_vec)[jx];
	  next_name = OVL_NAME (next);
	  if (next_name != name)
	    break;

	  if (marker)
	    {
	      gcc_checking_assert (OVL_FUNCTION (marker)
				   == OVL_FUNCTION (next));
	      next = OVL_CHAIN (next);
	    }

	  if (TREE_CODE (next) == USING_DECL)
	    {
	      next = strip_using_decl (next);
	      if (is_overloaded_fn (next))
		next = NULL_TREE;
	      else if (TREE_CODE (next) == USING_DECL)
		{
		  to_using = next;
		  next = NULL_TREE;
		}
	    }

	  if (next && DECL_DECLARES_TYPE_P (next))
	    to_type = next;
	}

      if (to_using)
	{
	  if (!current)
	    current = to_using;
	  else
	    current = ovl_make (to_using, current);
	}

      if (to_type)
	{
	  if (!current)
	    current = to_type;
	  else
	    current = stat_hack (current, to_type);
	}

      gcc_assert (current);
      if (marker)
	{
	  OVL_CHAIN (marker) = current;
	  current = marker;
	}
      (*member_vec)[store++] = current;
    }

  while (store++ < len)
    member_vec->pop ();
}

/* Add the non-function members to CLASSTYPE_MEMBER_VEC.  If there is
   no existing MEMBER_VEC and fewer than 8 fields, do nothing.  We
   know there must be at least 1 field -- the self-reference
   TYPE_DECL, except for anon aggregates, which will have at least
   one field.  */

void 
set_class_bindings (tree klass, unsigned extra)
{
  unsigned n_fields = count_class_fields (klass);
  vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (klass);

  if (member_vec || n_fields >= 8)
    {
      /* Append the new fields.  */
      vec_safe_reserve_exact (member_vec, extra + n_fields);
      member_vec_append_class_fields (member_vec, klass);
    }

  if (member_vec)
    {
      CLASSTYPE_MEMBER_VEC (klass) = member_vec;
      qsort (member_vec->address (), member_vec->length (),
	     sizeof (tree), member_name_cmp);
      member_vec_dedup (member_vec);
    }
}

/* Insert lately defined enum ENUMTYPE into KLASS for the sorted case.  */

void
insert_late_enum_def_bindings (tree klass, tree enumtype)
{
  int n_fields;
  vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (klass);

  /* The enum bindings will already be on the TYPE_FIELDS, so don't
     count them twice.  */
  if (!member_vec)
    n_fields = count_class_fields (klass);
  else
    n_fields = list_length (TYPE_VALUES (enumtype));

  if (member_vec || n_fields >= 8)
    {
      vec_safe_reserve_exact (member_vec, n_fields);
      if (CLASSTYPE_MEMBER_VEC (klass))
	member_vec_append_enum_values (member_vec, enumtype);
      else
	member_vec_append_class_fields (member_vec, klass);
      CLASSTYPE_MEMBER_VEC (klass) = member_vec;
      qsort (member_vec->address (), member_vec->length (),
	     sizeof (tree), member_name_cmp);
      member_vec_dedup (member_vec);
    }
}

/* Compute the chain index of a binding_entry given the HASH value of its
   name and the total COUNT of chains.  COUNT is assumed to be a power
   of 2.  */

#define ENTRY_INDEX(HASH, COUNT) (((HASH) >> 3) & ((COUNT) - 1))

/* A free list of "binding_entry"s awaiting for re-use.  */

static GTY((deletable)) binding_entry free_binding_entry = NULL;

/* The binding oracle; see cp-tree.h.  */

cp_binding_oracle_function *cp_binding_oracle;

/* If we have a binding oracle, ask it for all namespace-scoped
   definitions of NAME.  */

static inline void
query_oracle (tree name)
{
  if (!cp_binding_oracle)
    return;

  /* LOOKED_UP holds the set of identifiers that we have already
     looked up with the oracle.  */
  static hash_set<tree> looked_up;
  if (looked_up.add (name))
    return;

  cp_binding_oracle (CP_ORACLE_IDENTIFIER, name);
}

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
pop_local_binding (tree id, tree decl)
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
  for (tree t = get_local_decls (); t; t = DECL_CHAIN (t))
    {
      tree decl = TREE_CODE (t) == TREE_LIST ? TREE_VALUE (t) : t;
      tree name = OVL_NAME (decl);

      pop_local_binding (name, decl);
    }

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

/* Return true if OVL is an overload for an anticipated builtin.  */

static bool
anticipated_builtin_p (tree ovl)
{
  if (TREE_CODE (ovl) != OVERLOAD)
    return false;

  if (!OVL_HIDDEN_P (ovl))
    return false;

  tree fn = OVL_FUNCTION (ovl);
  gcc_checking_assert (DECL_ANTICIPATED (fn));

  if (DECL_HIDDEN_FRIEND_P (fn))
    return false;

  return true;
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
	   || anticipated_builtin_p (target_bval))
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
  else
    {
      if (!error_operand_p (bval))
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
      && TREE_CODE (decl) != NAMESPACE_DECL
      && !DECL_DECLARES_FUNCTION_P (decl)
      && (TREE_CODE (decl) != TYPE_DECL
	  || DECL_ARTIFICIAL (decl) == DECL_ARTIFICIAL (bval))
      && CP_DECL_CONTEXT (decl) == CP_DECL_CONTEXT (bval))
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

/* Replace BINDING's current value on its scope's name list with
   NEWVAL.  */

static void
update_local_overload (cxx_binding *binding, tree newval)
{
  tree *d;

  for (d = &binding->scope->names; ; d = &TREE_CHAIN (*d))
    if (*d == binding->value)
      {
	/* Stitch new list node in.  */
	*d = tree_cons (NULL_TREE, NULL_TREE, TREE_CHAIN (*d));
	break;
      }
    else if (TREE_CODE (*d) == TREE_LIST && TREE_VALUE (*d) == binding->value)
      break;

  TREE_VALUE (*d) = newval;
}

/* Compares the parameter-type-lists of ONE and TWO and
   returns false if they are different.  If the DECLs are template
   functions, the return types and the template parameter lists are
   compared too (DR 565).  */

static bool
matching_fn_p (tree one, tree two)
{
  if (!compparms (TYPE_ARG_TYPES (TREE_TYPE (one)),
		  TYPE_ARG_TYPES (TREE_TYPE (two))))
    return false;

  if (TREE_CODE (one) == TEMPLATE_DECL
      && TREE_CODE (two) == TEMPLATE_DECL)
    {
      /* Compare template parms.  */
      if (!comp_template_parms (DECL_TEMPLATE_PARMS (one),
				DECL_TEMPLATE_PARMS (two)))
	return false;

      /* And return type.  */
      if (!same_type_p (TREE_TYPE (TREE_TYPE (one)),
			TREE_TYPE (TREE_TYPE (two))))
	return false;
    }

  return true;
}

/* Push DECL into nonclass LEVEL BINDING or SLOT.  OLD is the current
   binding value (possibly with anticipated builtins stripped).
   Diagnose conflicts and return updated decl.  */

static tree
update_binding (cp_binding_level *level, cxx_binding *binding, tree *slot,
		tree old, tree decl, bool is_friend)
{
  tree to_val = decl;
  tree old_type = slot ? MAYBE_STAT_TYPE (*slot) : binding->type;
  tree to_type = old_type;

  gcc_assert (level->kind == sk_namespace ? !binding
	      : level->kind != sk_class && !slot);
  if (old == error_mark_node)
    old = NULL_TREE;

  if (TREE_CODE (decl) == TYPE_DECL && DECL_ARTIFICIAL (decl))
    {
      tree other = to_type;

      if (old && TREE_CODE (old) == TYPE_DECL && DECL_ARTIFICIAL (old))
	other = old;

      /* Pushing an artificial typedef.  See if this matches either
	 the type slot or the old value slot.  */
      if (!other)
	;
      else if (same_type_p (TREE_TYPE (other), TREE_TYPE (decl)))
	/* Two artificial decls to same type.  Do nothing.  */
	return other;
      else
	goto conflict;

      if (old)
	{
	  /* Slide decl into the type slot, keep old unaltered  */
	  to_type = decl;
	  to_val = old;
	  goto done;
	}
    }

  if (old && TREE_CODE (old) == TYPE_DECL && DECL_ARTIFICIAL (old))
    {
      /* Slide old into the type slot.  */
      to_type = old;
      old = NULL_TREE;
    }

  if (DECL_DECLARES_FUNCTION_P (decl))
    {
      if (!old)
	;
      else if (OVL_P (old))
	{
	  for (ovl_iterator iter (old); iter; ++iter)
	    {
	      tree fn = *iter;

	      if (iter.using_p () && matching_fn_p (fn, decl))
		{
		  /* If a function declaration in namespace scope or
		     block scope has the same name and the same
		     parameter-type- list (8.3.5) as a function
		     introduced by a using-declaration, and the
		     declarations do not declare the same function,
		     the program is ill-formed.  [namespace.udecl]/14 */
		  if (tree match = duplicate_decls (decl, fn, is_friend))
		    return match;
		  else
		    /* FIXME: To preserve existing error behavior, we
		       still push the decl.  This might change.  */
		    diagnose_name_conflict (decl, fn);
		}
	    }
	}
      else
	goto conflict;

      if (to_type != old_type
	  && warn_shadow
	  && MAYBE_CLASS_TYPE_P (TREE_TYPE (to_type))
	  && !(DECL_IN_SYSTEM_HEADER (decl)
	       && DECL_IN_SYSTEM_HEADER (to_type)))
	warning (OPT_Wshadow, "%q#D hides constructor for %q#D",
		 decl, to_type);

      to_val = ovl_insert (decl, old);
    }
  else if (!old)
    ;
  else if (TREE_CODE (old) != TREE_CODE (decl))
    /* Different kinds of decls conflict.  */
    goto conflict;
  else if (TREE_CODE (old) == TYPE_DECL)
    {
      if (same_type_p (TREE_TYPE (old), TREE_TYPE (decl)))
	/* Two type decls to the same type.  Do nothing.  */
	return old;
      else
	goto conflict;
    }
  else if (TREE_CODE (old) == NAMESPACE_DECL)
    {
      /* Two maybe-aliased namespaces.  If they're to the same target
	 namespace, that's ok.  */
      if (ORIGINAL_NAMESPACE (old) != ORIGINAL_NAMESPACE (decl))
	goto conflict;

      /* The new one must be an alias at this point.  */
      gcc_assert (DECL_NAMESPACE_ALIAS (decl));
      return old;
    }
  else if (TREE_CODE (old) == VAR_DECL)
    {
      /* There can be two block-scope declarations of the same
	 variable, so long as they are `extern' declarations.  */
      if (!DECL_EXTERNAL (old) || !DECL_EXTERNAL (decl))
	goto conflict;
      else if (tree match = duplicate_decls (decl, old, false))
	return match;
      else
	goto conflict;
    }
  else
    {
    conflict:
      diagnose_name_conflict (decl, old);
      to_val = NULL_TREE;
    }

 done:
  if (to_val)
    {
      if (level->kind != sk_namespace
	  && !to_type && binding->value && OVL_P (to_val))
	update_local_overload (binding, to_val);
      else
	{
	  tree to_add = to_val;
      
	  if (level->kind == sk_namespace)
	    to_add = decl;
	  else if (to_type == decl)
	    to_add = decl;
	  else if (TREE_CODE (to_add) == OVERLOAD)
	    to_add = build_tree_list (NULL_TREE, to_add);

	  add_decl_to_level (level, to_add);
	}

      if (slot)
	{
	  if (STAT_HACK_P (*slot))
	    {
	      STAT_TYPE (*slot) = to_type;
	      STAT_DECL (*slot) = to_val;
	    }
	  else if (to_type)
	    *slot = stat_hack (to_val, to_type);
	  else
	    *slot = to_val;
	}
      else
	{
	  binding->type = to_type;
	  binding->value = to_val;
	}
    }

  return decl;
}

/* Map of identifiers to extern C functions (or LISTS thereof).  */

static GTY(()) hash_map<lang_identifier *, tree> *extern_c_fns;

/* DECL has C linkage. If we have an existing instance, make sure it
   has the same exception specification [7.5, 7.6].  If there's no
   instance, add DECL to the map.  */

static void
check_extern_c_conflict (tree decl)
{
  /* Ignore artificial or system header decls.  */
  if (DECL_ARTIFICIAL (decl) || DECL_IN_SYSTEM_HEADER (decl))
    return;

  if (!extern_c_fns)
    extern_c_fns = hash_map<lang_identifier *,tree>::create_ggc (127);

  bool existed;
  tree *slot = &extern_c_fns->get_or_insert (DECL_NAME (decl), &existed);
  if (!existed)
    *slot = decl;
  else
    {
      tree old = *slot;
      if (TREE_CODE (old) == TREE_LIST)
	old = TREE_VALUE (old);

      int mismatch = 0;
      if (DECL_CONTEXT (old) == DECL_CONTEXT (decl))
	; /* If they're in the same context, we'll have already complained
	     about a (possible) mismatch, when inserting the decl.  */
      else if (!decls_match (decl, old))
	mismatch = 1;
      else if (!comp_except_specs (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (old)),
				   TYPE_RAISES_EXCEPTIONS (TREE_TYPE (decl)),
				   ce_normal))
	mismatch = -1;
      else if (DECL_ASSEMBLER_NAME_SET_P (old))
	SET_DECL_ASSEMBLER_NAME (decl, DECL_ASSEMBLER_NAME (old));

      if (mismatch)
	{
	  pedwarn (input_location, 0,
		   "declaration of %q#D with C language linkage", decl);
	  pedwarn (DECL_SOURCE_LOCATION (old), 0,
		   "conflicts with previous declaration %q#D", old);
	  if (mismatch < 0)
	    pedwarn (input_location, 0,
		     "due to different exception specifications");
	}
      else
	/* Chain it on for c_linkage_binding's use.  */
	*slot = tree_cons (NULL_TREE, decl, *slot);
    }
}

/* Returns a list of C-linkage decls with the name NAME.  Used in
   c-family/c-pragma.c to implement redefine_extname pragma.  */

tree
c_linkage_bindings (tree name)
{
  if (extern_c_fns)
    if (tree *slot = extern_c_fns->get (name))
      return *slot;
  return NULL_TREE;
}

/* DECL is being declared at a local scope.  Emit suitable shadow
   warnings.  */

static void
check_local_shadow (tree decl)
{
  /* Don't complain about the parms we push and then pop
     while tentatively parsing a function declarator.  */
  if (TREE_CODE (decl) == PARM_DECL && !DECL_CONTEXT (decl))
    return;

  /* Inline decls shadow nothing.  */
  if (DECL_FROM_INLINE (decl))
    return;

  /* External decls are something else.  */
  if (DECL_EXTERNAL (decl))
    return;

  tree old = NULL_TREE;
  cp_binding_level *old_scope = NULL;
  if (cxx_binding *binding = outer_binding (DECL_NAME (decl), NULL, true))
    {
      old = binding->value;
      old_scope = binding->scope;
    }
  while (old && VAR_P (old) && DECL_DEAD_FOR_LOCAL (old))
    old = DECL_SHADOWED_FOR_VAR (old);

  tree shadowed = NULL_TREE;
  if (old
      && (TREE_CODE (old) == PARM_DECL
	  || VAR_P (old)
	  || (TREE_CODE (old) == TYPE_DECL
	      && (!DECL_ARTIFICIAL (old)
		  || TREE_CODE (decl) == TYPE_DECL)))
      && (!DECL_ARTIFICIAL (decl)
	  || DECL_IMPLICIT_TYPEDEF_P (decl)
	  || (VAR_P (decl) && DECL_ANON_UNION_VAR_P (decl))))
    {
      /* DECL shadows a local thing possibly of interest.  */

      /* Don't complain if it's from an enclosing function.  */
      if (DECL_CONTEXT (old) == current_function_decl
	  && TREE_CODE (decl) != PARM_DECL
	  && TREE_CODE (old) == PARM_DECL)
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
	      error ("declaration of %q#D shadows a parameter", decl);
	      return;
	    }
	}

      /* The local structure or class can't use parameters of
	 the containing function anyway.  */
      if (DECL_CONTEXT (old) != current_function_decl)
	{
	  for (cp_binding_level *scope = current_binding_level;
	       scope != old_scope; scope = scope->level_chain)
	    if (scope->kind == sk_class
		&& !LAMBDA_TYPE_P (scope->this_entity))
	      return;
	}
      /* Error if redeclaring a local declared in a
	 init-statement or in the condition of an if or
	 switch statement when the new declaration is in the
	 outermost block of the controlled statement.
	 Redeclaring a variable from a for or while condition is
	 detected elsewhere.  */
      else if (VAR_P (old)
	       && old_scope == current_binding_level->level_chain
	       && (old_scope->kind == sk_cond || old_scope->kind == sk_for))
	{
	  error ("redeclaration of %q#D", decl);
	  inform (DECL_SOURCE_LOCATION (old),
		  "%q#D previously declared here", old);
	  return;
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
      else if ((TREE_CODE (old) == VAR_DECL
		&& old_scope == current_binding_level->level_chain
		&& old_scope->kind == sk_catch)
	       || (TREE_CODE (old) == PARM_DECL
		   && (current_binding_level->kind == sk_catch
		       || current_binding_level->level_chain->kind == sk_catch)
		   && in_function_try_handler))
	{
	  if (permerror (input_location, "redeclaration of %q#D", decl))
	    inform (DECL_SOURCE_LOCATION (old),
		    "%q#D previously declared here", old);
	  return;
	}

      /* If '-Wshadow=compatible-local' is specified without other
	 -Wshadow= flags, we will warn only when the type of the
	 shadowing variable (DECL) can be converted to that of the
	 shadowed parameter (OLD_LOCAL). The reason why we only check
	 if DECL's type can be converted to OLD_LOCAL's type (but not the
	 other way around) is because when users accidentally shadow a
	 parameter, more than often they would use the variable
	 thinking (mistakenly) it's still the parameter. It would be
	 rare that users would use the variable in the place that
	 expects the parameter but thinking it's a new decl.  */

      enum opt_code warning_code;
      if (warn_shadow)
	warning_code = OPT_Wshadow;
      else if (warn_shadow_local)
	warning_code = OPT_Wshadow_local;
      else if (warn_shadow_compatible_local
	       && can_convert (TREE_TYPE (old), TREE_TYPE (decl), tf_none))
	warning_code = OPT_Wshadow_compatible_local;
      else
	return;

      const char *msg;
      if (TREE_CODE (old) == PARM_DECL)
	msg = "declaration of %q#D shadows a parameter";
      else if (is_capture_proxy (old))
	msg = "declaration of %qD shadows a lambda capture";
      else
	msg = "declaration of %qD shadows a previous local";

      if (warning_at (input_location, warning_code, msg, decl))
	{
	  shadowed = old;
	  goto inform_shadowed;
	}
      return;
    }

  if (!warn_shadow)
    return;

  /* Don't warn for artificial things that are not implicit typedefs.  */
  if (DECL_ARTIFICIAL (decl) && !DECL_IMPLICIT_TYPEDEF_P (decl))
    return;
  
  if (nonlambda_method_basetype ())
    if (tree member = lookup_member (current_nonlambda_class_type (),
				     DECL_NAME (decl), /*protect=*/0,
				     /*want_type=*/false, tf_warning_or_error))
      {
	member = MAYBE_BASELINK_FUNCTIONS (member);

	/* Warn if a variable shadows a non-function, or the variable
	   is a function or a pointer-to-function.  */
	if (!OVL_P (member)
	    || TREE_CODE (decl) == FUNCTION_DECL
	    || TYPE_PTRFN_P (TREE_TYPE (decl))
	    || TYPE_PTRMEMFUNC_P (TREE_TYPE (decl)))
	  {
	    if (warning_at (input_location, OPT_Wshadow,
			    "declaration of %qD shadows a member of %qT",
			    decl, current_nonlambda_class_type ())
		&& DECL_P (member))
	      {
		shadowed = member;
		goto inform_shadowed;
	      }
	  }
	return;
      }

  /* Now look for a namespace shadow.  */
  old = find_namespace_value (current_namespace, DECL_NAME (decl));
  if (old
      && (VAR_P (old)
	  || (TREE_CODE (old) == TYPE_DECL
	      && (!DECL_ARTIFICIAL (old)
		  || TREE_CODE (decl) == TYPE_DECL)))
      && !instantiating_current_function_p ())
    /* XXX shadow warnings in outer-more namespaces */
    {
      if (warning_at (input_location, OPT_Wshadow,
		      "declaration of %qD shadows a global declaration",
		      decl))
	{
	  shadowed = old;
	  goto inform_shadowed;
	}
      return;
    }

  return;

 inform_shadowed:
  inform (DECL_SOURCE_LOCATION (shadowed), "shadowed declaration is here");
}

/* DECL is being pushed inside function CTX.  Set its context, if
   needed.  */

static void
set_decl_context_in_fn (tree ctx, tree decl)
{
  if (!DECL_CONTEXT (decl)
      /* A local declaration for a function doesn't constitute
	 nesting.  */
      && TREE_CODE (decl) != FUNCTION_DECL
      /* A local declaration for an `extern' variable is in the
	 scope of the current namespace, not the current
	 function.  */
      && !(VAR_P (decl) && DECL_EXTERNAL (decl))
      /* When parsing the parameter list of a function declarator,
	 don't set DECL_CONTEXT to an enclosing function.  When we
	 push the PARM_DECLs in order to process the function body,
	 current_binding_level->this_entity will be set.  */
      && !(TREE_CODE (decl) == PARM_DECL
	   && current_binding_level->kind == sk_function_parms
	   && current_binding_level->this_entity == NULL))
    DECL_CONTEXT (decl) = ctx;

  /* If this is the declaration for a namespace-scope function,
     but the declaration itself is in a local scope, mark the
     declaration.  */
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_NAMESPACE_SCOPE_P (decl))
    DECL_LOCAL_FUNCTION_P (decl) = 1;
}

/* DECL is a local-scope decl with linkage.  SHADOWED is true if the
   name is already bound at the current level.

   [basic.link] If there is a visible declaration of an entity with
   linkage having the same name and type, ignoring entities declared
   outside the innermost enclosing namespace scope, the block scope
   declaration declares that same entity and receives the linkage of
   the previous declaration.

   Also, make sure that this decl matches any existing external decl
   in the enclosing namespace.  */

static void
set_local_extern_decl_linkage (tree decl, bool shadowed)
{
  tree ns_value = decl; /* Unique marker.  */

  if (!shadowed)
    {
      tree loc_value = innermost_non_namespace_value (DECL_NAME (decl));
      if (!loc_value)
	{
	  ns_value
	    = find_namespace_value (current_namespace, DECL_NAME (decl));
	  loc_value = ns_value;
	}
      if (loc_value == error_mark_node)
	loc_value = NULL_TREE;

      for (ovl_iterator iter (loc_value); iter; ++iter)
	if (!iter.hidden_p ()
	    && (TREE_STATIC (*iter) || DECL_EXTERNAL (*iter))
	    && decls_match (*iter, decl))
	  {
	    /* The standard only says that the local extern inherits
	       linkage from the previous decl; in particular, default
	       args are not shared.  Add the decl into a hash table to
	       make sure only the previous decl in this case is seen
	       by the middle end.  */
	    struct cxx_int_tree_map *h;

	    /* We inherit the outer decl's linkage.  But we're a
	       different decl.  */
	    TREE_PUBLIC (decl) = TREE_PUBLIC (*iter);

	    if (cp_function_chain->extern_decl_map == NULL)
	      cp_function_chain->extern_decl_map
		= hash_table<cxx_int_tree_map_hasher>::create_ggc (20);

	    h = ggc_alloc<cxx_int_tree_map> ();
	    h->uid = DECL_UID (decl);
	    h->to = *iter;
	    cxx_int_tree_map **loc = cp_function_chain->extern_decl_map
	      ->find_slot (h, INSERT);
	    *loc = h;
	    break;
	  }
    }

  if (TREE_PUBLIC (decl))
    {
      /* DECL is externally visible.  Make sure it matches a matching
	 decl in the namespace scope.  We only really need to check
	 this when inserting the decl, not when we find an existing
	 match in the current scope.  However, in practice we're
	 going to be inserting a new decl in the majority of cases --
	 who writes multiple extern decls for the same thing in the
	 same local scope?  Doing it here often avoids a duplicate
	 namespace lookup.  */

      /* Avoid repeating a lookup.  */
      if (ns_value == decl)
	ns_value = find_namespace_value (current_namespace, DECL_NAME (decl));

      if (ns_value == error_mark_node)
	ns_value = NULL_TREE;

      for (ovl_iterator iter (ns_value); iter; ++iter)
	{
	  tree other = *iter;

	  if (!(TREE_PUBLIC (other) || DECL_EXTERNAL (other)))
	    ; /* Not externally visible.   */
	  else if (DECL_EXTERN_C_P (decl) && DECL_EXTERN_C_P (other))
	    ; /* Both are extern "C", we'll check via that mechanism.  */
	  else if (TREE_CODE (other) != TREE_CODE (decl)
		   || ((VAR_P (decl) || matching_fn_p (other, decl))
		       && !comptypes (TREE_TYPE (decl), TREE_TYPE (other),
				      COMPARE_REDECLARATION)))
	    {
	      if (permerror (DECL_SOURCE_LOCATION (decl),
			     "local external declaration %q#D", decl))
		inform (DECL_SOURCE_LOCATION (other),
			"does not match previous declaration %q#D", other);
	      break;
	    }
	}
    }
}

/* Record DECL as belonging to the current lexical scope.  Check for
   errors (such as an incompatible declaration for the same name
   already seen in the same scope).  IS_FRIEND is true if DECL is
   declared as a friend.

   Returns either DECL or an old decl for the same name.  If an old
   decl is returned, it may have been smashed to agree with what DECL
   says.  */

static tree
do_pushdecl (tree decl, bool is_friend)
{
  if (decl == error_mark_node)
    return error_mark_node;

  if (!DECL_TEMPLATE_PARM_P (decl) && current_function_decl)
    set_decl_context_in_fn (current_function_decl, decl);

  /* The binding level we will be pushing into.  During local class
     pushing, we want to push to the containing scope.  */
  cp_binding_level *level = current_binding_level;
  while (level->kind == sk_class)
    level = level->level_chain;

  if (tree name = DECL_NAME (decl))
    {
      cxx_binding *binding = NULL; /* Local scope binding.  */
      tree ns = NULL_TREE; /* Searched namespace.  */
      tree *slot = NULL; /* Binding slot in namespace.  */
      tree old = NULL_TREE;

      if (level->kind == sk_namespace)
	{
	  /* We look in the decl's namespace for an existing
	     declaration, even though we push into the current
	     namespace.  */
	  ns = (DECL_NAMESPACE_SCOPE_P (decl)
		? CP_DECL_CONTEXT (decl) : current_namespace);
	  /* Create the binding, if this is current namespace, because
	     that's where we'll be pushing anyway.  */
	  slot = find_namespace_slot (ns, name, ns == current_namespace);
	  if (slot)
	    old = MAYBE_STAT_DECL (*slot);
	}
      else
	{
	  binding = find_local_binding (level, name);
	  if (binding)
	    old = binding->value;
	}

      if (current_function_decl && VAR_OR_FUNCTION_DECL_P (decl)
	  && DECL_EXTERNAL (decl))
	set_local_extern_decl_linkage (decl, old != NULL_TREE);

      if (old == error_mark_node)
	old = NULL_TREE;

      for (ovl_iterator iter (old); iter; ++iter)
	if (iter.using_p ())
	  ; /* Ignore using decls here.  */
	else if (tree match = duplicate_decls (decl, *iter, is_friend))
	  {
	    if (match == error_mark_node)
	      ;
	    else if (TREE_CODE (match) == TYPE_DECL)
	      /* The IDENTIFIER will have the type referring to the
		 now-smashed TYPE_DECL, because ...?  Reset it.  */
	      SET_IDENTIFIER_TYPE_VALUE (name, TREE_TYPE (match));
	    else if (iter.hidden_p () && !DECL_HIDDEN_P (match))
	      {
		/* Unhiding a previously hidden decl.  */
		tree head = iter.reveal_node (old);
		if (head != old)
		  {
		    if (!ns)
		      {
			update_local_overload (binding, head);
			binding->value = head;
		      }
		    else if (STAT_HACK_P (*slot))
		      STAT_DECL (*slot) = head;
		    else
		      *slot = head;
		  }
		if (TREE_CODE (match) == FUNCTION_DECL
		    && DECL_EXTERN_C_P (match))
		  /* We need to check and register the fn now.  */
		  check_extern_c_conflict (match);
	      }
	    return match;
	  }

      /* We are pushing a new decl.  */

      /* Skip a hidden builtin we failed to match already.  There can
	 only be one.  */
      if (old && anticipated_builtin_p (old))
	old = OVL_CHAIN (old);

      check_template_shadow (decl);

      if (DECL_DECLARES_FUNCTION_P (decl))
	{
	  check_default_args (decl);

	  if (is_friend)
	    {
	      if (level->kind != sk_namespace)
		/* In a local class, a friend function declaration must
		   find a matching decl in the innermost non-class scope.
		   [class.friend/11] */
		error ("friend declaration %qD in local class without "
		       "prior local declaration", decl);
	      else if (!flag_friend_injection)
		/* Hide it from ordinary lookup.  */
		DECL_ANTICIPATED (decl) = DECL_HIDDEN_FRIEND_P (decl) = true;
	    }
	}

      if (level->kind != sk_namespace)
	{
	  check_local_shadow (decl);

	  if (TREE_CODE (decl) == NAMESPACE_DECL)
	    /* A local namespace alias.  */
	    set_identifier_type_value (name, NULL_TREE);

	  if (!binding)
	    binding = create_local_binding (level, name);
	}
      else if (!slot)
	{
	  ns = current_namespace;
	  slot = find_namespace_slot (ns, name, true);
	  /* Update OLD to reflect the namespace we're going to be
	     pushing into.  */
	  old = MAYBE_STAT_DECL (*slot);
	}

      old = update_binding (level, binding, slot, old, decl, is_friend);

      if (old != decl)
	/* An existing decl matched, use it.  */
	decl = old;
      else if (TREE_CODE (decl) == TYPE_DECL)
	{
	  tree type = TREE_TYPE (decl);

	  if (type != error_mark_node)
	    {
	      if (TYPE_NAME (type) != decl)
		set_underlying_type (decl);

	      if (!ns)
		set_identifier_type_value_with_scope (name, decl, level);
	      else
		SET_IDENTIFIER_TYPE_VALUE (name, global_type_node);
	    }

	  /* If this is a locally defined typedef in a function that
	     is not a template instantation, record it to implement
	     -Wunused-local-typedefs.  */
	  if (!instantiating_current_function_p ())
	    record_locally_defined_typedef (decl);
	}
      else if (VAR_P (decl))
	maybe_register_incomplete_var (decl);
      else if (TREE_CODE (decl) == FUNCTION_DECL && DECL_EXTERN_C_P (decl))
	check_extern_c_conflict (decl);
    }
  else
    add_decl_to_level (level, decl);

  return decl;
}

/* Record a decl-node X as belonging to the current lexical scope.
   It's a friend if IS_FRIEND is true -- which affects exactly where
   we push it.  */

tree
pushdecl (tree x, bool is_friend)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  tree ret = do_pushdecl (x, is_friend);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
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
   binding level.  If IS_USING is true, DECL got here through a
   using-declaration.  */

static void
push_local_binding (tree id, tree decl, bool is_using)
{
  /* Skip over any local classes.  This makes sense if we call
     push_local_binding with a friend decl of a local class.  */
  cp_binding_level *b = innermost_nonclass_level ();

  gcc_assert (b->kind != sk_namespace);
  if (find_local_binding (b, id))
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

  if (TREE_CODE (decl) == OVERLOAD || is_using)
    /* We must put the OVERLOAD or using into a TREE_LIST since we
       cannot use the decl's chain itself.  */
    decl = build_tree_list (NULL_TREE, decl);

  /* And put DECL on the list of things declared by the current
     binding level.  */
  add_decl_to_level (b, decl);
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
    shadowed = find_namespace_value (current_namespace, DECL_NAME (decl));
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
    verbatim ("%s %<%s(%E)%> %p %d\n", action, desc,
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
    : (name == global_identifier
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

/* Return the list of declarations of the current local scope.  */

tree
get_local_decls (void)
{
  gcc_assert (current_binding_level->kind != sk_namespace
	      && current_binding_level->kind != sk_class);
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
      tree *slot = find_namespace_slot (current_namespace, id, true);
      gcc_assert (decl);
      update_binding (b, NULL, slot, MAYBE_STAT_DECL (*slot), decl, false);

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
   specified class.  */

tree
constructor_name (tree type)
{
  tree decl = TYPE_NAME (TYPE_MAIN_VARIANT (type));

  return decl ? DECL_NAME (decl) : NULL_TREE;
}

/* Returns TRUE if NAME is the name for the constructor for TYPE,
   which must be a class type.  */

bool
constructor_name_p (tree name, tree type)
{
  gcc_assert (MAYBE_CLASS_TYPE_P (type));

  /* These don't have names.  */
  if (TREE_CODE (type) == DECLTYPE_TYPE
      || TREE_CODE (type) == TYPEOF_TYPE)
    return false;

  if (name && name == constructor_name (type))
    return true;

  return false;
}

/* Counter used to create anonymous type names.  */

static GTY(()) int anon_cnt;

/* Return an IDENTIFIER which can be used as a name for
   unnamed structs and unions.  */

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
   unnamed types.  */

static GTY(()) int lambda_cnt = 0;

tree
make_lambda_name (void)
{
  char buf[32];

  sprintf (buf, LAMBDANAME_FORMAT, lambda_cnt++);
  return get_identifier (buf);
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
do_pushdecl_with_scope (tree x, cp_binding_level *level, bool is_friend)
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
      x = pushdecl (x, is_friend);
      current_binding_level = b;
    }
  current_function_decl = function_decl;
  return x;
}

/* Inject X into the local scope just before the function parms.  */

tree
pushdecl_outermost_localscope (tree x)
{
  cp_binding_level *b = NULL;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);

  /* Find the scope just inside the function parms.  */
  for (cp_binding_level *n = current_binding_level;
       n->kind != sk_function_parms; n = b->level_chain)
    b = n;

  tree ret = b ? do_pushdecl_with_scope (x, b, false) : error_mark_node;
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

  decl = OVL_FIRST (decl);

  /* Make a USING_DECL.  */
  tree using_decl = push_using_decl (scope, name);

  if (using_decl == NULL_TREE
      && at_function_scope_p ()
      && VAR_P (decl))
    /* C++11 7.3.3/10.  */
    error ("%qD is already declared in this scope", name);
  
  return using_decl;
}

/* Process a local-scope or namespace-scope using declaration.  SCOPE
   is the nominated scope to search for NAME.  VALUE_P and TYPE_P
   point to the binding for NAME in the current scope and are
   updated.  */

static void
do_nonmember_using_decl (tree scope, tree name, tree *value_p, tree *type_p)
{
  name_lookup lookup (name, 0);

  if (!qualified_namespace_lookup (scope, &lookup))
    {
      error ("%qD not declared", name);
      return;
    }
  else if (TREE_CODE (lookup.value) == TREE_LIST)
    {
      error ("reference to %qD is ambiguous", name);
      print_candidates (lookup.value);
      lookup.value = NULL_TREE;
    }

  if (lookup.type && TREE_CODE (lookup.type) == TREE_LIST)
    {
      error ("reference to %qD is ambiguous", name);
      print_candidates (lookup.type);
      lookup.type = NULL_TREE;
    }

  tree value = *value_p;
  tree type = *type_p;

  /* Shift the old and new bindings around so we're comparing class and
     enumeration names to each other.  */
  if (value && DECL_IMPLICIT_TYPEDEF_P (value))
    {
      type = value;
      value = NULL_TREE;
    }

  if (lookup.value && DECL_IMPLICIT_TYPEDEF_P (lookup.value))
    {
      lookup.type = lookup.value;
      lookup.value = NULL_TREE;
    }

  if (lookup.value && lookup.value != value)
    {
      /* Check for using functions.  */
      if (OVL_P (lookup.value) && (!value || OVL_P (value)))
	{
	  for (lkp_iterator usings (lookup.value); usings; ++usings)
	    {
	      tree new_fn = *usings;

	      /* [namespace.udecl]

		 If a function declaration in namespace scope or block
		 scope has the same name and the same parameter types as a
		 function introduced by a using declaration the program is
		 ill-formed.  */
	      bool found = false;
	      for (ovl_iterator old (value); !found && old; ++old)
		{
		  tree old_fn = *old;

		  if (new_fn == old_fn)
		    /* The function already exists in the current
		       namespace.  */
		    found = true;
		  else if (old.using_p ())
		    continue; /* This is a using decl. */
		  else if (old.hidden_p () && !DECL_HIDDEN_FRIEND_P (old_fn))
		    continue; /* This is an anticipated builtin.  */
		  else if (!matching_fn_p (new_fn, old_fn))
		    continue; /* Parameters do not match.  */
		  else if (decls_match (new_fn, old_fn))
		    found = true;
		  else
		    {
		      diagnose_name_conflict (new_fn, old_fn);
		      found = true;
		    }
		}

	      if (!found)
		/* Unlike the overload case we don't drop anticipated
		   builtins here.  They don't cause a problem, and
		   we'd like to match them with a future
		   declaration.  */
		value = ovl_insert (new_fn, value, true);
	    }
	}
      else if (value
	       /* Ignore anticipated builtins.  */
	       && !anticipated_builtin_p (value)
	       && !decls_match (lookup.value, value))
	diagnose_name_conflict (lookup.value, value);
      else
	value = lookup.value;
    }

  if (lookup.type && lookup.type != type)
    {
      if (type && !decls_match (lookup.type, type))
	diagnose_name_conflict (lookup.type, type);
      else
	type = lookup.type;
    }

  /* If bind->value is empty, shift any class or enumeration name back.  */
  if (!value)
    {
      value = type;
      type = NULL_TREE;
    }

  *value_p = value;
  *type_p = type;
}

/* Returns true if ANCESTOR encloses DESCENDANT, including matching.
   Both are namespaces.  */

bool
is_nested_namespace (tree ancestor, tree descendant, bool inline_only)
{
  int depth = SCOPE_DEPTH (ancestor);

  if (!depth && !inline_only)
    /* The global namespace encloses everything.  */
    return true;

  while (SCOPE_DEPTH (descendant) > depth
	 && (!inline_only || DECL_NAMESPACE_INLINE_P (descendant)))
    descendant = CP_DECL_CONTEXT (descendant);

  return ancestor == descendant;
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

  /* Search until we reach namespace scope.  */
  while (TREE_CODE (child) != NAMESPACE_DECL)
    {
      /* If we've reached the ROOT, it encloses CHILD.  */
      if (root == child)
	return true;
      /* Go out one level.  */
      if (TYPE_P (child))
	child = TYPE_NAME (child);
      child = CP_DECL_CONTEXT (child);
    }

  if (TREE_CODE (root) == NAMESPACE_DECL)
    return is_nested_namespace (root, child);

  return false;
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
  bool is_valid = true;
  bool subtime;

  /* Do nothing if we're adding to an outer lambda closure type,
     outer_binding will add it later if it's needed.  */
  if (current_class_type != class_binding_level->this_entity)
    return true;

  subtime = timevar_cond_start (TV_NAME_LOOKUP);
  /* Get the name of X.  */
  tree name = OVL_NAME (x);

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
      && DECL_NAME (x) == DECL_NAME (TYPE_NAME (current_class_type)))
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
	       && OVL_P (target_bval))
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
	       && OVL_P (target_bval))
	old_decl = bval;
      else if (TREE_CODE (bval) == USING_DECL
	       && OVL_P (target_decl))
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
      CLASSTYPE_NON_AGGREGATE (current_class_type) = true;
    }

  /* Cannot introduce a constructor name.  */
  if (constructor_name_p (name, current_class_type))
    {
      error ("%<%T::%D%> names constructor in %qT",
	     scope, name, current_class_type);
      return NULL_TREE;
    }

  /* From [namespace.udecl]:

       A using-declaration used as a member-declaration shall refer to a
       member of a base class of the class being defined.

     In general, we cannot check this constraint in a template because
     we do not know the entire set of base classes of the current
     class type. Morover, if SCOPE is dependent, it might match a
     non-dependent base.  */

  tree decl = NULL_TREE;
  if (!dependent_scope_p (scope))
    {
      base_kind b_kind;
      tree binfo = lookup_base (current_class_type, scope, ba_any, &b_kind,
				tf_warning_or_error);
      if (b_kind < bk_proper_base)
	{
	  /* If there are dependent bases, scope might resolve at
	     instantiation time, even if it isn't exactly one of the
	     dependent bases.  */
	  if (b_kind == bk_same_type || !any_dependent_bases_p ())
	    {
	      error_not_base_type (scope, current_class_type);
	      return NULL_TREE;
	    }
	}
      else if (name == ctor_identifier && !binfo_direct_p (binfo))
	{
	  error ("cannot inherit constructors from indirect base %qT", scope);
	  return NULL_TREE;
	}
      else if (!IDENTIFIER_CONV_OP_P (name)
	       || !dependent_type_p (TREE_TYPE (name)))
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

  tree value = build_lang_decl (USING_DECL, name, NULL_TREE);
  USING_DECL_DECLS (value) = decl;
  USING_DECL_SCOPE (value) = scope;
  DECL_DEPENDENT_P (value) = !decl;

  return value;
}


/* Return the binding for NAME in NS.  If NS is NULL, look in
   global_namespace.  */

tree
get_namespace_binding (tree ns, tree name)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  if (!ns)
    ns = global_namespace;
  gcc_checking_assert (!DECL_NAMESPACE_ALIAS (ns));
  tree ret = find_namespace_value (ns, name);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return ret;
}

/* Set value binding of NAME in the global namespace to VAL.  Does not
   add it to the list of things in the namespace.  */

void
set_global_binding (tree name, tree val)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);

  gcc_checking_assert (name == DECL_NAME (val));
  tree *slot = find_namespace_slot (global_namespace, name, true);
  tree old = MAYBE_STAT_DECL (*slot);

  if (!old)
    *slot = val;
  else if (old == val)
    ;
  else if (!STAT_HACK_P (*slot)
	   && TREE_CODE (val) == TYPE_DECL && DECL_ARTIFICIAL (val))
    *slot = stat_hack (old, val);
  else if (!STAT_HACK_P (*slot)
	   && TREE_CODE (old) == TYPE_DECL && DECL_ARTIFICIAL (old))
    *slot = stat_hack (val, old);
  else
    /* The user's placed something in the implementor's
       namespace.  */
    diagnose_name_conflict (val, old);

  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

/* Set the context of a declaration to scope. Complain if we are not
   outside scope.  */

void
set_decl_namespace (tree decl, tree scope, bool friendp)
{
  /* Get rid of namespace aliases.  */
  scope = ORIGINAL_NAMESPACE (scope);

  /* It is ok for friends to be qualified in parallel space.  */
  if (!friendp && !is_nested_namespace (current_namespace, scope))
    error ("declaration of %qD not in a namespace surrounding %qD",
	   decl, scope);
  DECL_CONTEXT (decl) = FROB_CONTEXT (scope);

  /* See whether this has been declared in the namespace or inline
     children.  */
  tree old = NULL_TREE;
  {
    name_lookup lookup (DECL_NAME (decl), LOOKUP_HIDDEN);
    if (!lookup.search_qualified (scope, /*usings=*/false))
      /* No old declaration at all.  */
      goto not_found;
    old = lookup.value;
  }

  /* If it's a TREE_LIST, the result of the lookup was ambiguous.  */
  if (TREE_CODE (old) == TREE_LIST)
    {
    ambiguous:
      DECL_CONTEXT (decl) = FROB_CONTEXT (scope);
      error ("reference to %qD is ambiguous", decl);
      print_candidates (old);
      return;
    }

  if (!DECL_DECLARES_FUNCTION_P (decl))
    {
      /* Don't compare non-function decls with decls_match here, since
	 it can't check for the correct constness at this
	 point.  pushdecl will find those errors later.  */

      /* We might have found it in an inline namespace child of SCOPE.  */
      if (TREE_CODE (decl) == TREE_CODE (old))
	DECL_CONTEXT (decl) = DECL_CONTEXT (old);

    found:
      /* Writing "N::i" to declare something directly in "N" is invalid.  */
      if (CP_DECL_CONTEXT (decl) == current_namespace
	  && at_namespace_scope_p ())
	error ("explicit qualification in declaration of %qD", decl);
      return;
    }

  /* Since decl is a function, old should contain a function decl.  */
  if (!OVL_P (old))
    goto not_found;

  /* We handle these in check_explicit_instantiation_namespace.  */
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

  tree found;
  found = NULL_TREE;

  for (lkp_iterator iter (old); iter; ++iter)
    {
      if (iter.using_p ())
	continue;

      tree ofn = *iter;

      /* Adjust DECL_CONTEXT first so decls_match will return true
	 if DECL will match a declaration in an inline namespace.  */
      DECL_CONTEXT (decl) = DECL_CONTEXT (ofn);
      if (decls_match (decl, ofn))
	{
	  if (found)
	    {
	      /* We found more than one matching declaration.  */
	      DECL_CONTEXT (decl) = FROB_CONTEXT (scope);
	      goto ambiguous;
	    }
	  found = ofn;
	}
    }

  if (found)
    {
      if (DECL_HIDDEN_FRIEND_P (found))
	{
	  pedwarn (DECL_SOURCE_LOCATION (decl), 0,
		   "%qD has not been declared within %qD", decl, scope);
	  inform (DECL_SOURCE_LOCATION (found),
		  "only here as a %<friend%>");
	}
      DECL_CONTEXT (decl) = DECL_CONTEXT (found);
      goto found;
    }

 not_found:
  /* It didn't work, go back to the explicit scope.  */
  DECL_CONTEXT (decl) = FROB_CONTEXT (scope);
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
	  if (!DECL_NAME (ns))
	    {
	      warning (OPT_Wattributes, "ignoring %qD attribute on anonymous "
		       "namespace", name);
	      continue;
	    }
	  if (!DECL_NAMESPACE_INLINE_P (ns))
	    {
	      warning (OPT_Wattributes, "ignoring %qD attribute on non-inline "
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
  t = do_pushdecl_with_scope
    (x, NAMESPACE_LEVEL (current_namespace), is_friend);

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

/* Process a using-declaration appearing in namespace scope.  */

void
finish_namespace_using_decl (tree decl, tree scope, tree name)
{
  tree orig_decl = decl;

  gcc_checking_assert (current_binding_level->kind == sk_namespace
		       && !processing_template_decl);
  decl = validate_nonmember_using_decl (decl, scope, name);
  if (decl == NULL_TREE)
    return;

  tree *slot = find_namespace_slot (current_namespace, name, true);
  tree val = slot ? MAYBE_STAT_DECL (*slot) : NULL_TREE;
  tree type = slot ? MAYBE_STAT_TYPE (*slot) : NULL_TREE;
  do_nonmember_using_decl (scope, name, &val, &type);
  if (STAT_HACK_P (*slot))
    {
      STAT_DECL (*slot) = val;
      STAT_TYPE (*slot) = type;
    }
  else if (type)
    *slot = stat_hack (val, type);
  else
    *slot = val;

  /* Emit debug info.  */
  cp_emit_debug_info_for_using (orig_decl, current_namespace);
}

/* Process a using-declaration at function scope.  */

void
finish_local_using_decl (tree decl, tree scope, tree name)
{
  tree orig_decl = decl;

  gcc_checking_assert (current_binding_level->kind != sk_class
		       && current_binding_level->kind != sk_namespace);
  decl = validate_nonmember_using_decl (decl, scope, name);
  if (decl == NULL_TREE)
    return;

  add_decl_expr (decl);

  cxx_binding *binding = find_local_binding (current_binding_level, name);
  tree value = binding ? binding->value : NULL_TREE;
  tree type = binding ? binding->type : NULL_TREE;

  do_nonmember_using_decl (scope, name, &value, &type);

  if (!value)
    ;
  else if (binding && value == binding->value)
    ;
  else if (binding && binding->value && TREE_CODE (value) == OVERLOAD)
    {
      update_local_overload (IDENTIFIER_BINDING (name), value);
      IDENTIFIER_BINDING (name)->value = value;
    }
  else
    /* Install the new binding.  */
    push_local_binding (name, value, true);

  if (!type)
    ;
  else if (binding && type == binding->type)
    ;
  else
    {
      push_local_binding (name, type, true);
      set_identifier_type_value (name, type);
    }

  /* Emit debug info.  */
  if (!processing_template_decl)
    cp_emit_debug_info_for_using (orig_decl, current_scope ());
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
  if (!(flags & LOOKUP_HIDDEN) && is_lambda_ignored_entity (val))
    return false;
  return true;
}

/* Suggest alternatives for NAME, an IDENTIFIER_NODE for which name
   lookup failed.  Search through all available namespaces and print out
   possible candidates.  If no exact matches are found, and
   SUGGEST_MISSPELLINGS is true, then also look for near-matches and
   suggest the best near-match, if there is one.  */

void
suggest_alternatives_for (location_t location, tree name,
			  bool suggest_misspellings)
{
  vec<tree> candidates = vNULL;
  vec<tree> worklist = vNULL;
  unsigned limit = PARAM_VALUE (CXX_MAX_NAMESPACES_FOR_DIAGNOSTIC_HELP);
  bool limited = false;

  /* Breadth-first search of namespaces.  Up to limit namespaces
     searched (limit zero == unlimited).  */
  worklist.safe_push (global_namespace);
  for (unsigned ix = 0; ix != worklist.length (); ix++)
    {
      tree ns = worklist[ix];
      name_lookup lookup (name);

      if (lookup.search_qualified (ns, false))
	candidates.safe_push (lookup.value);

      if (!limited)
	{
	  /* Look for child namespaces.  We have to do this
	     indirectly because they are chained in reverse order,
	     which is confusing to the user.  */
	  vec<tree> children = vNULL;

	  for (tree decl = NAMESPACE_LEVEL (ns)->names;
	       decl; decl = TREE_CHAIN (decl))
	    if (TREE_CODE (decl) == NAMESPACE_DECL
		&& !DECL_NAMESPACE_ALIAS (decl)
		&& !DECL_NAMESPACE_INLINE_P (decl))
	      children.safe_push (decl);

	  while (!limited && !children.is_empty ())
	    {
	      if (worklist.length () == limit)
		{
		  /* Unconditionally warn that the search was truncated.  */
		  inform (location,
			  "maximum limit of %d namespaces searched for %qE",
			  limit, name);
		  limited = true;
		}
	      else
		worklist.safe_push (children.pop ());
	    }
	  children.release ();
	}
    }
  worklist.release ();

  if (candidates.length ())
    {
      inform_n (location, candidates.length (),
		"suggested alternative:",
		"suggested alternatives:");
      for (unsigned ix = 0; ix != candidates.length (); ix++)
	{
	  tree val = candidates[ix];

	  inform (location_of (val), "  %qE", val);
	}
      candidates.release ();
    }
  else if (!suggest_misspellings)
    ;
  else if (const char *fuzzy = lookup_name_fuzzy (name, FUZZY_LOOKUP_NAME))
    {
      /* Show a spelling correction.  */
      gcc_rich_location richloc (location);

      richloc.add_fixit_replace (fuzzy);
      inform_at_rich_loc (&richloc, "suggested alternative: %qs", fuzzy);
    }
}

/* Subroutine of maybe_suggest_missing_header for handling unrecognized names
   for some of the most common names within "std::".
   Given non-NULL NAME, a name for lookup within "std::", return the header
   name defining it within the C++ Standard Library (with '<' and '>'),
   or NULL.  */

static const char *
get_std_name_hint (const char *name)
{
  struct std_name_hint
  {
    const char *name;
    const char *header;
  };
  static const std_name_hint hints[] = {
    /* <array>.  */
    {"array", "<array>"}, // C++11
    /* <deque>.  */
    {"deque", "<deque>"},
    /* <forward_list>.  */
    {"forward_list", "<forward_list>"},  // C++11
    /* <fstream>.  */
    {"basic_filebuf", "<fstream>"},
    {"basic_ifstream", "<fstream>"},
    {"basic_ofstream", "<fstream>"},
    {"basic_fstream", "<fstream>"},
    /* <iostream>.  */
    {"cin", "<iostream>"},
    {"cout", "<iostream>"},
    {"cerr", "<iostream>"},
    {"clog", "<iostream>"},
    {"wcin", "<iostream>"},
    {"wcout", "<iostream>"},
    {"wclog", "<iostream>"},
    /* <list>.  */
    {"list", "<list>"},
    /* <map>.  */
    {"map", "<map>"},
    {"multimap", "<map>"},
    /* <queue>.  */
    {"queue", "<queue>"},
    {"priority_queue", "<queue>"},
    /* <ostream>.  */
    {"ostream", "<ostream>"},
    {"wostream", "<ostream>"},
    {"ends", "<ostream>"},
    {"flush", "<ostream>"},
    {"endl", "<ostream>"},
    /* <set>.  */
    {"set", "<set>"},
    {"multiset", "<set>"},
    /* <sstream>.  */
    {"basic_stringbuf", "<sstream>"},
    {"basic_istringstream", "<sstream>"},
    {"basic_ostringstream", "<sstream>"},
    {"basic_stringstream", "<sstream>"},
    /* <stack>.  */
    {"stack", "<stack>"},
    /* <string>.  */
    {"string", "<string>"},
    {"wstring", "<string>"},
    {"u16string", "<string>"},
    {"u32string", "<string>"},
    /* <unordered_map>.  */
    {"unordered_map", "<unordered_map>"}, // C++11
    {"unordered_multimap", "<unordered_map>"}, // C++11
    /* <unordered_set>.  */
    {"unordered_set", "<unordered_set>"}, // C++11
    {"unordered_multiset", "<unordered_set>"}, // C++11
    /* <vector>.  */
    {"vector", "<vector>"},
  };
  const size_t num_hints = sizeof (hints) / sizeof (hints[0]);
  for (size_t i = 0; i < num_hints; i++)
    {
      if (0 == strcmp (name, hints[i].name))
	return hints[i].header;
    }
  return NULL;
}

/* If SCOPE is the "std" namespace, then suggest pertinent header
   files for NAME at LOCATION.
   Return true iff a suggestion was offered.  */

static bool
maybe_suggest_missing_header (location_t location, tree name, tree scope)
{
  if (scope == NULL_TREE)
    return false;
  if (TREE_CODE (scope) != NAMESPACE_DECL)
    return false;
  /* We only offer suggestions for the "std" namespace.  */
  if (scope != std_node)
    return false;
  gcc_assert (TREE_CODE (name) == IDENTIFIER_NODE);

  const char *name_str = IDENTIFIER_POINTER (name);
  const char *header_hint = get_std_name_hint (name_str);
  if (!header_hint)
    return false;

  gcc_rich_location richloc (location);
  maybe_add_include_fixit (&richloc, header_hint);
  inform_at_rich_loc (&richloc,
		      "%<std::%s%> is defined in header %qs;"
		      " did you forget to %<#include %s%>?",
		      name_str, header_hint, header_hint);
  return true;
}

/* Look for alternatives for NAME, an IDENTIFIER_NODE for which name
   lookup failed within the explicitly provided SCOPE.  Suggest the
   the best meaningful candidates (if any) as a fix-it hint.
   Return true iff a suggestion was provided.  */

bool
suggest_alternative_in_explicit_scope (location_t location, tree name,
				       tree scope)
{
  /* Resolve any namespace aliases.  */
  scope = ORIGINAL_NAMESPACE (scope);

  if (maybe_suggest_missing_header (location, name, scope))
    return true;

  cp_binding_level *level = NAMESPACE_LEVEL (scope);

  best_match <tree, const char *> bm (name);
  consider_binding_level (name, bm, level, false, FUZZY_LOOKUP_NAME);

  /* See if we have a good suggesion for the user.  */
  const char *fuzzy_name = bm.get_best_meaningful_candidate ();
  if (fuzzy_name)
    {
      gcc_rich_location richloc (location);
      richloc.add_fixit_replace (fuzzy_name);
      inform_at_rich_loc (&richloc, "suggested alternative: %qs",
			  fuzzy_name);
      return true;
    }

  return false;
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
      int flags = lookup_flags (prefer_type, /*namespaces_only*/false);
      if (find_hidden)
	flags |= LOOKUP_HIDDEN;
      name_lookup lookup (name, flags);

      if (qualified_namespace_lookup (scope, &lookup))
	t = lookup.value;
    }
  else if (cxx_dialect != cxx98 && TREE_CODE (scope) == ENUMERAL_TYPE)
    t = lookup_enumerator (scope, name);
  else if (is_class_type (scope, complain))
    t = lookup_member (scope, name, 2, prefer_type, tf_warning_or_error);

  if (!t)
    return error_mark_node;
  return t;
}

/* [namespace.qual]
   Accepts the NAME to lookup and its qualifying SCOPE.
   Returns the name/type pair found into the cxx_binding *RESULT,
   or false on error.  */

static bool
qualified_namespace_lookup (tree scope, name_lookup *lookup)
{
  timevar_start (TV_NAME_LOOKUP);
  query_oracle (lookup->name);
  bool found = lookup->search_qualified (ORIGINAL_NAMESPACE (scope));
  timevar_stop (TV_NAME_LOOKUP);
  return found;
}

/* Helper function for lookup_name_fuzzy.
   Traverse binding level LVL, looking for good name matches for NAME
   (and BM).  */
static void
consider_binding_level (tree name, best_match <tree, const char *> &bm,
			cp_binding_level *lvl, bool look_within_fields,
			enum lookup_name_fuzzy_kind kind)
{
  if (look_within_fields)
    if (lvl->this_entity && TREE_CODE (lvl->this_entity) == RECORD_TYPE)
      {
	tree type = lvl->this_entity;
	bool want_type_p = (kind == FUZZY_LOOKUP_TYPENAME);
	tree best_matching_field
	  = lookup_member_fuzzy (type, name, want_type_p);
	if (best_matching_field)
	  bm.consider (IDENTIFIER_POINTER (best_matching_field));
      }

  for (tree t = lvl->names; t; t = TREE_CHAIN (t))
    {
      tree d = t;

      /* OVERLOADs or decls from using declaration are wrapped into
	 TREE_LIST.  */
      if (TREE_CODE (d) == TREE_LIST)
	d = OVL_FIRST (TREE_VALUE (d));

      /* Don't use bindings from implicitly declared functions,
	 as they were likely misspellings themselves.  */
      if (TREE_TYPE (d) == error_mark_node)
	continue;

      /* Skip anticipated decls of builtin functions.  */
      if (TREE_CODE (d) == FUNCTION_DECL
	  && DECL_BUILT_IN (d)
	  && DECL_ANTICIPATED (d))
	continue;

      if (tree name = DECL_NAME (d))
	/* Ignore internal names with spaces in them.  */
	if (!strchr (IDENTIFIER_POINTER (name), ' '))
	  bm.consider (IDENTIFIER_POINTER (name));
    }
}

/* Search for near-matches for NAME within the current bindings, and within
   macro names, returning the best match as a const char *, or NULL if
   no reasonable match is found.  */

const char *
lookup_name_fuzzy (tree name, enum lookup_name_fuzzy_kind kind)
{
  gcc_assert (TREE_CODE (name) == IDENTIFIER_NODE);

  best_match <tree, const char *> bm (name);

  cp_binding_level *lvl;
  for (lvl = scope_chain->class_bindings; lvl; lvl = lvl->level_chain)
    consider_binding_level (name, bm, lvl, true, kind);

  for (lvl = current_binding_level; lvl; lvl = lvl->level_chain)
    consider_binding_level (name, bm, lvl, false, kind);

  /* Consider macros: if the user misspelled a macro name e.g. "SOME_MACRO"
     as:
       x = SOME_OTHER_MACRO (y);
     then "SOME_OTHER_MACRO" will survive to the frontend and show up
     as a misspelled identifier.

     Use the best distance so far so that a candidate is only set if
     a macro is better than anything so far.  This allows early rejection
     (without calculating the edit distance) of macro names that must have
     distance >= bm.get_best_distance (), and means that we only get a
     non-NULL result for best_macro_match if it's better than any of
     the identifiers already checked.  */
  best_macro_match bmm (name, bm.get_best_distance (), parse_in);
  cpp_hashnode *best_macro = bmm.get_best_meaningful_candidate ();
  /* If a macro is the closest so far to NAME, consider it.  */
  if (best_macro)
    bm.consider ((const char *)best_macro->ident.str);

  /* Try the "starts_decl_specifier_p" keywords to detect
     "singed" vs "signed" typos.  */
  for (unsigned i = 0; i < num_c_common_reswords; i++)
    {
      const c_common_resword *resword = &c_common_reswords[i];

      if (kind == FUZZY_LOOKUP_TYPENAME)
	if (!cp_keyword_starts_decl_specifier_p (resword->rid))
	  continue;

      tree resword_identifier = ridpointers [resword->rid];
      if (!resword_identifier)
	continue;
      gcc_assert (TREE_CODE (resword_identifier) == IDENTIFIER_NODE);

      /* Only consider reserved words that survived the
	 filtering in init_reswords (e.g. for -std).  */
      if (!IDENTIFIER_KEYWORD_P (resword_identifier))
	continue;

      bm.consider (IDENTIFIER_POINTER (resword_identifier));
    }

  return bm.get_best_meaningful_candidate ();
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

  query_oracle (name);

  /* Conversion operators are handled specially because ordinary
     unqualified name lookup will not find template conversion
     operators.  */
  if (IDENTIFIER_CONV_OP_P (name))
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
	    if (TREE_CODE (binding) == TYPE_DECL && DECL_HIDDEN_P (binding))
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
    {
      name_lookup lookup (name, flags);
      if (lookup.search_unqualified
	  (current_decl_namespace (), current_binding_level))
	val = lookup.value;
    }

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
  cp_binding_level *level = NULL;

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
  if (val)
    level = iter->scope;
  else
    {
      tree ns = current_decl_namespace ();

      if (tree *slot = find_namespace_slot (ns, name))
	{
	  /* If this is the kind of thing we're looking for, we're done.  */
	  if (tree type = MAYBE_STAT_TYPE (*slot))
	    if (qualify_lookup (type, LOOKUP_PREFER_TYPES))
	      val = type;
	  if (!val)
	    {
	      if (tree decl = MAYBE_STAT_DECL (*slot))
		if (qualify_lookup (decl, LOOKUP_PREFER_TYPES))
		  val = decl;
	    }
	  level = NAMESPACE_LEVEL (ns);
	}
    }

  /* Type found, check if it is in the allowed scopes, ignoring cleanup
     and template parameter scopes.  */
  if (val)
    {
      cp_binding_level *b = current_binding_level;
      while (b)
	{
	  if (level == b)
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
do_pushtag (tree name, tree type, tag_scope scope)
{
  tree decl;

  cp_binding_level *b = current_binding_level;
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
	 /* Pushing into a class is ok for lambdas or when we want current  */
	 || (b->kind == sk_class
	     && scope != ts_lambda
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
	      || scope == ts_lambda
	      || (cs && TREE_CODE (cs) == FUNCTION_DECL))
	    context = cs;
	  else if (cs && TYPE_P (cs))
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
	  if (!TYPE_BEING_DEFINED (current_class_type)
	      && scope != ts_lambda)
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
	  decl = do_pushdecl_with_scope (decl, b, /*is_friend=*/false);
	  if (decl == error_mark_node)
	    return decl;

	  if (DECL_CONTEXT (decl) == std_node
	      && init_list_identifier == DECL_NAME (TYPE_NAME (type))
	      && !CLASSTYPE_TEMPLATE_INFO (type))
	    {
	      error ("declaration of std::initializer_list does not match "
		     "#include <initializer_list>, isn't a template");
	      return error_mark_node;
	    }
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

/* Wrapper for do_pushtag.  */

tree
pushtag (tree name, tree type, tag_scope scope)
{
  tree ret;
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  ret = do_pushtag (name, type, scope);
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
  static vec<tree> bindings_need_stored;
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
	  /* We can apparently have duplicates in NAMES.  */
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
  static vec<tree> bindings_need_stored;
  size_t i;
  cp_class_binding *cb;

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
}

/* A chain of saved_scope structures awaiting reuse.  */

static GTY((deletable)) struct saved_scope *free_saved_scope;

static void
do_push_to_top_level (void)
{
  struct saved_scope *s;
  cp_binding_level *b;
  cxx_saved_binding *sb;
  size_t i;
  bool need_pop;

  /* Reuse or create a new structure for this saved scope.  */
  if (free_saved_scope != NULL)
    {
      s = free_saved_scope;
      free_saved_scope = s->prev;

      vec<cxx_saved_binding, va_gc> *old_bindings = s->old_bindings;
      memset (s, 0, sizeof (*s));
      /* Also reuse the structure's old_bindings vector.  */
      vec_safe_truncate (old_bindings, 0);
      s->old_bindings = old_bindings;
    }
  else
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
}

static void
do_pop_from_top_level (void)
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

  /* Make this saved_scope structure available for reuse by
     push_to_top_level.  */
  s->prev = free_saved_scope;
  free_saved_scope = s;
}

/* Push into the scope of the namespace NS, even if it is deeply
   nested within another namespace.  */

static void
do_push_nested_namespace (tree ns)
{
  if (ns == global_namespace)
    do_push_to_top_level ();
  else
    {
      do_push_nested_namespace (CP_DECL_CONTEXT (ns));
      gcc_checking_assert
	(find_namespace_value (current_namespace,
			       DECL_NAME (ns) ? DECL_NAME (ns)
			       : anon_identifier) == ns);
      resume_scope (NAMESPACE_LEVEL (ns));
      current_namespace = ns;
    }
}

/* Pop back from the scope of the namespace NS, which was previously
   entered with push_nested_namespace.  */

static void
do_pop_nested_namespace (tree ns)
{
  while (ns != global_namespace)
    {
      ns = CP_DECL_CONTEXT (ns);
      current_namespace = ns;
      leave_scope ();
    }

  do_pop_from_top_level ();
}

/* Add TARGET to USINGS, if it does not already exist there.
   We used to build the complete graph of usings at this point, from
   the POV of the source namespaces.  Now we build that as we perform
   the unqualified search.  */

static void
add_using_namespace (vec<tree, va_gc> *&usings, tree target)
{
  if (usings)
    for (unsigned ix = usings->length (); ix--;)
      if ((*usings)[ix] == target)
	return;

  vec_safe_push (usings, target);
}

/* Tell the debug system of a using directive.  */

static void
emit_debug_info_using_namespace (tree from, tree target, bool implicit)
{
  /* Emit debugging info.  */
  tree context = from != global_namespace ? from : NULL_TREE;
  debug_hooks->imported_module_or_decl (target, NULL_TREE, context, false,
					implicit);
}

/* Process a namespace-scope using directive.  */

void
finish_namespace_using_directive (tree target, tree attribs)
{
  gcc_checking_assert (namespace_bindings_p ());
  if (target == error_mark_node)
    return;

  add_using_namespace (DECL_NAMESPACE_USING (current_namespace),
		       ORIGINAL_NAMESPACE (target));
  emit_debug_info_using_namespace (current_namespace,
				   ORIGINAL_NAMESPACE (target), false);

  if (attribs == error_mark_node)
    return;

  for (tree a = attribs; a; a = TREE_CHAIN (a))
    {
      tree name = get_attribute_name (a);
      if (is_attribute_p ("strong", name))
	{
	  warning (0, "strong using directive no longer supported");
	  if (CP_DECL_CONTEXT (target) == current_namespace)
	    inform (DECL_SOURCE_LOCATION (target),
		    "you may use an inline namespace instead");
	}
      else
	warning (OPT_Wattributes, "%qD attribute directive ignored", name);
    }
}

/* Process a function-scope using-directive.  */

void
finish_local_using_directive (tree target, tree attribs)
{
  gcc_checking_assert (local_bindings_p ());
  if (target == error_mark_node)
    return;

  if (attribs)
    warning (OPT_Wattributes, "attributes ignored on local using directive");

  add_stmt (build_stmt (input_location, USING_STMT, target));

  add_using_namespace (current_binding_level->using_directives,
		       ORIGINAL_NAMESPACE (target));
}

/* Pushes X into the global namespace.  */

tree
pushdecl_top_level (tree x, bool is_friend)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  do_push_to_top_level ();
  x = pushdecl_namespace_level (x, is_friend);
  do_pop_from_top_level ();
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return x;
}

/* Pushes X into the global namespace and calls cp_finish_decl to
   register the variable, initializing it with INIT.  */

tree
pushdecl_top_level_and_finish (tree x, tree init)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  do_push_to_top_level ();
  x = pushdecl_namespace_level (x, false);
  cp_finish_decl (x, init, false, NULL_TREE, 0);
  do_pop_from_top_level ();
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return x;
}

/* Enter the namespaces from current_namerspace to NS.  */

static int
push_inline_namespaces (tree ns)
{
  int count = 0;
  if (ns != current_namespace)
    {
      gcc_assert (ns != global_namespace);
      count += push_inline_namespaces (CP_DECL_CONTEXT (ns));
      resume_scope (NAMESPACE_LEVEL (ns));
      current_namespace = ns;
      count++;
    }
  return count;
}

/* Push into the scope of the NAME namespace.  If NAME is NULL_TREE,
   then we enter an anonymous namespace.  If MAKE_INLINE is true, then
   we create an inline namespace (it is up to the caller to check upon
   redefinition). Return the number of namespaces entered.  */

int
push_namespace (tree name, bool make_inline)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  int count = 0;

  /* We should not get here if the global_namespace is not yet constructed
     nor if NAME designates the global namespace:  The global scope is
     constructed elsewhere.  */
  gcc_assert (global_namespace != NULL && name != global_identifier);

  if (!name)
    name = anon_identifier;

  tree ns = NULL_TREE;
  {
    name_lookup lookup (name, 0);
    if (!lookup.search_qualified (current_namespace, /*usings=*/false))
      ;
    else if (TREE_CODE (lookup.value) != NAMESPACE_DECL)
      ;
    else if (tree dna = DECL_NAMESPACE_ALIAS (lookup.value))
      {
	/* A namespace alias is not allowed here, but if the alias
	   is for a namespace also inside the current scope,
	   accept it with a diagnostic.  That's better than dying
	   horribly.  */
	if (is_nested_namespace (current_namespace, CP_DECL_CONTEXT (dna)))
	  {
	    error ("namespace alias %qD not allowed here, "
		   "assuming %qD", lookup.value, dna);
	    ns = dna;
	  }
      }
    else
      ns = lookup.value;
  }

  bool new_ns = false;
  if (ns)
    /* DR2061.  NS might be a member of an inline namespace.  We
       need to push into those namespaces.  */
    count += push_inline_namespaces (CP_DECL_CONTEXT (ns));
  else
    {
      ns = build_lang_decl (NAMESPACE_DECL, name, void_type_node);
      SCOPE_DEPTH (ns) = SCOPE_DEPTH (current_namespace) + 1;
      if (!SCOPE_DEPTH (ns))
	/* We only allow depth 255. */
	sorry ("cannot nest more than %d namespaces",
	       SCOPE_DEPTH (current_namespace));
      DECL_CONTEXT (ns) = FROB_CONTEXT (current_namespace);
      new_ns = true;

      if (pushdecl (ns) == error_mark_node)
	ns = NULL_TREE;
      else
	{
	  if (name == anon_identifier)
	    {
	      /* Clear DECL_NAME for the benefit of debugging back ends.  */
	      SET_DECL_ASSEMBLER_NAME (ns, name);
	      DECL_NAME (ns) = NULL_TREE;

	      if (!make_inline)
		add_using_namespace (DECL_NAMESPACE_USING (current_namespace),
				     ns);
	    }
	  else if (TREE_PUBLIC (current_namespace))
	    TREE_PUBLIC (ns) = 1;

	  if (make_inline)
	    {
	      DECL_NAMESPACE_INLINE_P (ns) = true;
	      vec_safe_push (DECL_NAMESPACE_INLINEES (current_namespace), ns);
	    }

	  if (name == anon_identifier || make_inline)
	    emit_debug_info_using_namespace (current_namespace, ns, true);
	}
    }

  if (ns)
    {
      if (make_inline && !DECL_NAMESPACE_INLINE_P (ns))
	{
	  error ("inline namespace must be specified at initial definition");
	  inform (DECL_SOURCE_LOCATION (ns), "%qD defined here", ns);
	}
      if (new_ns)
	begin_scope (sk_namespace, ns);
      else
	resume_scope (NAMESPACE_LEVEL (ns));
      current_namespace = ns;
      count++;
    }

  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
  return count;
}

/* Pop from the scope of the current namespace.  */

void
pop_namespace (void)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);

  gcc_assert (current_namespace != global_namespace);
  current_namespace = CP_DECL_CONTEXT (current_namespace);
  /* The binding level is not popped, as it might be re-opened later.  */
  leave_scope ();

  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

/* External entry points for do_{push_to/pop_from}_top_level.  */

void
push_to_top_level (void)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  do_push_to_top_level ();
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

void
pop_from_top_level (void)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  do_pop_from_top_level ();
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

/* External entry points for do_{push,pop}_nested_namespace.  */

void
push_nested_namespace (tree ns)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  do_push_nested_namespace (ns);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

void
pop_nested_namespace (tree ns)
{
  bool subtime = timevar_cond_start (TV_NAME_LOOKUP);
  gcc_assert (current_namespace == ns);
  do_pop_nested_namespace (ns);
  timevar_cond_stop (TV_NAME_LOOKUP, subtime);
}

/* Pop off extraneous binding levels left over due to syntax errors.
   We don't pop past namespaces, as they might be valid.  */

void
pop_everything (void)
{
  if (ENABLE_SCOPE_CHECKING)
    verbatim ("XXX entering pop_everything ()\n");
  while (!namespace_bindings_p ())
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

  t = MAYBE_BASELINK_FUNCTIONS (t);

  /* FIXME: Handle TEMPLATE_DECLs.  */
  for (lkp_iterator iter (t); iter; ++iter)
    {
      tree fn = *iter;
      if (TREE_CODE (fn) != TEMPLATE_DECL)
	{
	  if (building_stmt_list_p ())
	    add_stmt (build_stmt (input_location, USING_STMT, fn));
	  else
	    debug_hooks->imported_module_or_decl (fn, NULL_TREE, context,
						  false, false);
	}
    }
}

#include "gt-cp-name-lookup.h"
