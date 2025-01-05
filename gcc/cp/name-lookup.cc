/* Definitions for C++ name lookup routines.
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
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
#include "gcc-rich-location.h"
#include "spellcheck-tree.h"
#include "parser.h"
#include "c-family/name-hint.h"
#include "c-family/known-headers.h"
#include "c-family/c-spellcheck.h"
#include "bitmap.h"

static cxx_binding *cxx_binding_make (tree value, tree type);
static cp_binding_level *innermost_nonclass_level (void);
static void set_identifier_type_value_with_scope (tree id, tree decl,
						  cp_binding_level *b);
static name_hint maybe_suggest_missing_std_header (location_t location,
						   tree name);
static name_hint suggest_alternatives_for_1 (location_t location, tree name,
					     bool suggest_misspellings);

/* Slots in BINDING_VECTOR.  */
enum binding_slots
{
 BINDING_SLOT_CURRENT,	/* Slot for current TU.  */
 BINDING_SLOT_GLOBAL,	/* Slot for merged global module. */
 BINDING_SLOT_PARTITION, /* Slot for merged partition entities or
			    imported friends.  */

 /* Number of always-allocated slots.  */
 BINDING_SLOTS_FIXED = BINDING_SLOT_GLOBAL + 1
};

/* Create an overload suitable for recording an artificial TYPE_DECL
   and another decl.  We use this machanism to implement the struct
   stat hack.  */

#define STAT_HACK_P(N) ((N) && TREE_CODE (N) == OVERLOAD && OVL_LOOKUP_P (N))
#define STAT_TYPE_VISIBLE_P(N) TREE_USED (OVERLOAD_CHECK (N))
#define STAT_TYPE(N) TREE_TYPE (N)
#define STAT_DECL(N) OVL_FUNCTION (N)
#define STAT_VISIBLE(N) OVL_CHAIN (N)
#define MAYBE_STAT_DECL(N) (STAT_HACK_P (N) ? STAT_DECL (N) : N)
#define MAYBE_STAT_TYPE(N) (STAT_HACK_P (N) ? STAT_TYPE (N) : NULL_TREE)

/* When a STAT_HACK_P is true, OVL_USING_P and OVL_EXPORT_P are valid
   and apply to the hacked type.  */

/* For regular (maybe) overloaded functions, we have OVL_HIDDEN_P.
   But we also need to indicate hiddenness on implicit type decls
   (injected friend classes), and (coming soon) decls injected from
   block-scope externs.  It is too awkward to press the existing
   overload marking for that.  If we have a hidden non-function, we
   always create a STAT_HACK, and use these two markers as needed.  */
#define STAT_TYPE_HIDDEN_P(N) OVL_HIDDEN_P (N)
#define STAT_DECL_HIDDEN_P(N) OVL_DEDUP_P (N)

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
  tree *slot = DECL_NAMESPACE_BINDINGS (ns)
    ->find_slot_with_hash (name, name ? IDENTIFIER_HASH_VALUE (name) : 0,
			   create_p ? INSERT : NO_INSERT);
  return slot;
}

static tree
find_namespace_value (tree ns, tree name)
{
  tree *b = find_namespace_slot (ns, name);

  return b ? MAYBE_STAT_DECL (*b) : NULL_TREE;
}

/* Look in *SLOT for a the binding of NAME in imported module IX.
   Returns pointer to binding's slot, or NULL if not found.  Does a
   binary search, as this is mainly used for random access during
   importing.  Do not use for the fixed slots.  */

static binding_slot *
search_imported_binding_slot (tree *slot, unsigned ix)
{
  gcc_assert (ix);

  if (!*slot)
    return NULL;

  if (TREE_CODE (*slot) != BINDING_VECTOR)
    return NULL;

  unsigned clusters = BINDING_VECTOR_NUM_CLUSTERS (*slot);
  binding_cluster *cluster = BINDING_VECTOR_CLUSTER_BASE (*slot);

  if (BINDING_VECTOR_SLOTS_PER_CLUSTER == BINDING_SLOTS_FIXED)
    {
      clusters--;
      cluster++;
    }

  while (clusters > 1)
    {
      unsigned half = clusters / 2;
      gcc_checking_assert (cluster[half].indices[0].span);
      if (cluster[half].indices[0].base > ix)
	clusters = half;
      else
	{
	  clusters -= half;
	  cluster += half;
	}
    }

  if (clusters)
    /* Is it in this cluster?  */
    for (unsigned off = 0; off != BINDING_VECTOR_SLOTS_PER_CLUSTER; off++)
      {
	if (!cluster->indices[off].span)
	  break;
	if (cluster->indices[off].base > ix)
	  break;

	if (cluster->indices[off].base + cluster->indices[off].span > ix)
	  return &cluster->slots[off];
      }

  return NULL;
}

static void
init_global_partition (binding_cluster *cluster, tree decl)
{
  bool named = true;

  if (header_module_p ())
    named = false;
  else if (TREE_PUBLIC (decl)
	   && TREE_CODE (decl) == NAMESPACE_DECL
	   && !DECL_NAMESPACE_ALIAS (decl))
    named = false;
  else if (!get_originating_module (decl))
    named = false;

  binding_slot *mslot;
  if (named)
    mslot = &cluster[BINDING_SLOT_PARTITION
		     / BINDING_VECTOR_SLOTS_PER_CLUSTER]
      .slots[BINDING_SLOT_PARTITION
	     % BINDING_VECTOR_SLOTS_PER_CLUSTER];
  else
    mslot = &cluster[0].slots[BINDING_SLOT_GLOBAL];

  if (*mslot)
    decl = ovl_make (decl, *mslot);
  *mslot = decl;

  if (TREE_CODE (decl) == CONST_DECL)
    {
      tree type = TREE_TYPE (decl);
      if (TREE_CODE (type) == ENUMERAL_TYPE
	  && IDENTIFIER_ANON_P (DECL_NAME (TYPE_NAME (type)))
	  && decl == TREE_VALUE (TYPE_VALUES (type)))
	/* Anonymous enums are keyed by their first enumerator, put
	   the TYPE_DECL here too.  */
	*mslot = ovl_make (TYPE_NAME (type), *mslot);
    }
}

/* Get the fixed binding slot IX.  Creating the vector if CREATE is
   non-zero.  If CREATE is < 0, make sure there is at least 1 spare
   slot for an import.  (It is an error for CREATE < 0 and the slot to
   already exist.)  */

static tree *
get_fixed_binding_slot (tree *slot, tree name, unsigned ix, int create)
{
  gcc_checking_assert (ix <= BINDING_SLOT_PARTITION);

  /* An assumption is that the fixed slots all reside in one cluster.  */
  gcc_checking_assert (BINDING_VECTOR_SLOTS_PER_CLUSTER >= BINDING_SLOTS_FIXED);

  if (!*slot || TREE_CODE (*slot) != BINDING_VECTOR)
    {
      if (ix == BINDING_SLOT_CURRENT)
	/* The current TU can just use slot directly.  */
	return slot;

      if (!create)
	return NULL;

      /* The partition slot is always needed, in case we have imported
	 temploid friends with attachment different from the module we
	 imported them from.  */
      bool partition_slot = true;
      unsigned want = ((BINDING_SLOTS_FIXED + partition_slot + (create < 0)
			+ BINDING_VECTOR_SLOTS_PER_CLUSTER - 1)
		       / BINDING_VECTOR_SLOTS_PER_CLUSTER);
      tree new_vec = make_binding_vec (name, want);
      BINDING_VECTOR_NUM_CLUSTERS (new_vec) = want;
      binding_cluster *cluster = BINDING_VECTOR_CLUSTER_BASE (new_vec);

      /* Initialize the fixed slots.  */
      for (unsigned jx = BINDING_SLOTS_FIXED; jx--;)
	{
	  cluster[0].indices[jx].base = 0;
	  cluster[0].indices[jx].span = 1;
	  cluster[0].slots[jx] = NULL_TREE;
	}

      if (partition_slot)
	{
	  unsigned off = BINDING_SLOT_PARTITION % BINDING_VECTOR_SLOTS_PER_CLUSTER;
	  unsigned ind = BINDING_SLOT_PARTITION / BINDING_VECTOR_SLOTS_PER_CLUSTER;
	  cluster[ind].indices[off].base = 0;
	  cluster[ind].indices[off].span = 1;
	  cluster[ind].slots[off] = NULL_TREE;
	}

      if (tree orig = *slot)
	{
	  /* Propagate existing value to current slot.  */

	  /* Propagate global & module entities to the global and
	     partition slots.  */
	  if (tree type = strip_using_decl (MAYBE_STAT_TYPE (orig)))
	    init_global_partition (cluster, type);

	  for (ovl_iterator iter (strip_using_decl (MAYBE_STAT_DECL (orig)));
	       iter; ++iter)
	    {
	      tree decl = *iter;

	      /* Internal linkage entities are in deduplicateable.  */
	      init_global_partition (cluster, decl);
	    }

	  if (cluster[0].slots[BINDING_SLOT_GLOBAL]
	      && !(TREE_CODE (orig) == NAMESPACE_DECL
		   && !DECL_NAMESPACE_ALIAS (orig)))
	    {
	      /* Note that we had some GMF entries.  */
	      if (!STAT_HACK_P (orig))
		orig = stat_hack (orig);

	      MODULE_BINDING_GLOBAL_P (orig) = true;
	    }

	  cluster[0].slots[BINDING_SLOT_CURRENT] = orig;
	}

      *slot = new_vec;
    }
  else
    gcc_checking_assert (create >= 0);

  unsigned off = ix % BINDING_VECTOR_SLOTS_PER_CLUSTER;
  binding_cluster &cluster
    = BINDING_VECTOR_CLUSTER (*slot, ix / BINDING_VECTOR_SLOTS_PER_CLUSTER);

  /* There must always be slots for these indices  */
  gcc_checking_assert (cluster.indices[off].span == 1
		       && !cluster.indices[off].base
		       && !cluster.slots[off].is_lazy ());

  return reinterpret_cast<tree *> (&cluster.slots[off]);
}

/* *SLOT is a namespace binding slot.  Append a slot for imported
   module IX.  */

static binding_slot *
append_imported_binding_slot (tree *slot, tree name, unsigned ix)
{
  gcc_checking_assert (ix);

  if (!*slot ||  TREE_CODE (*slot) != BINDING_VECTOR)
    /* Make an initial module vector.  */
    get_fixed_binding_slot (slot, name, BINDING_SLOT_GLOBAL, -1);
  else if (!BINDING_VECTOR_CLUSTER_LAST (*slot)
	   ->indices[BINDING_VECTOR_SLOTS_PER_CLUSTER - 1].span)
    /* There is space in the last cluster.  */;
  else if (BINDING_VECTOR_NUM_CLUSTERS (*slot)
	   != BINDING_VECTOR_ALLOC_CLUSTERS (*slot))
    /* There is space in the vector.  */
    BINDING_VECTOR_NUM_CLUSTERS (*slot)++;
  else
    {
      /* Extend the vector.  */
      unsigned have = BINDING_VECTOR_NUM_CLUSTERS (*slot);
      unsigned want = (have * 3 + 1) / 2;

      if (want > (unsigned short)~0)
	want = (unsigned short)~0;

      tree new_vec = make_binding_vec (name, want);
      BINDING_VECTOR_NUM_CLUSTERS (new_vec) = have + 1;
      BINDING_VECTOR_GLOBAL_DUPS_P (new_vec)
	= BINDING_VECTOR_GLOBAL_DUPS_P (*slot);
      BINDING_VECTOR_PARTITION_DUPS_P (new_vec)
	= BINDING_VECTOR_PARTITION_DUPS_P (*slot);
      memcpy (BINDING_VECTOR_CLUSTER_BASE (new_vec),
	      BINDING_VECTOR_CLUSTER_BASE (*slot),
	      have * sizeof (binding_cluster));
      *slot = new_vec;
    }

  binding_cluster *last = BINDING_VECTOR_CLUSTER_LAST (*slot);
  for (unsigned off = 0; off != BINDING_VECTOR_SLOTS_PER_CLUSTER; off++)
    if (!last->indices[off].span)
      {
	/* Fill the free slot of the cluster.  */
	last->indices[off].base = ix;
	last->indices[off].span = 1;
	last->slots[off] = NULL_TREE;
	/* Check monotonicity.  */
	gcc_checking_assert (last[off ? 0 : -1]
			     .indices[off ? off - 1
				      : BINDING_VECTOR_SLOTS_PER_CLUSTER - 1]
			     .base < ix);
	return &last->slots[off];
      }

  gcc_unreachable ();
}

/* Add DECL to the list of things declared in binding level B.  */

static void
add_decl_to_level (cp_binding_level *b, tree decl)
{
  gcc_assert (b->kind != sk_class);

  /* Make sure we don't create a circular list.  xref_tag can end
     up pushing the same artificial decl more than once.  We
     should have already detected that in update_binding.  (This isn't a
     complete verification of non-circularity.)  */
  gcc_assert (b->names != decl);

  /* We build up the list in reverse order, and reverse it later if
     necessary.  */
  TREE_CHAIN (decl) = b->names;
  b->names = decl;

  /* If appropriate, add decl to separate list of statics.  We include
     extern variables because they might turn out to be static later.
     It's OK for this list to contain a few false positives.  */
  if (b->kind == sk_namespace
      && ((VAR_P (decl) && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
	  || (TREE_CODE (decl) == FUNCTION_DECL
	      && (!TREE_PUBLIC (decl)
		  || decl_internal_context_p (decl)
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
	if (binding->scope == b)
	  return binding;

	/* Cleanup contours are transparent to the language.  */
	if (b->kind != sk_cleanup)
	  break;
      }
  return NULL;
}

class name_lookup
{
public:
  typedef std::pair<tree, tree> using_pair;
  typedef auto_vec<using_pair, 16> using_queue;

public:
  tree name;	/* The identifier being looked for.  */

  /* Usually we just add things to the VALUE binding, but we record
     (hidden) IMPLICIT_TYPEDEFs on the type binding, which is used for
     using-decl resolution.  */
  tree value;	/* A (possibly ambiguous) set of things found.  */
  tree type;	/* A type that has been found.  */

  LOOK_want want;  /* What kind of entity we want.  */

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
  name_lookup (tree n, LOOK_want w = LOOK_want::NORMAL)
  : name (n), value (NULL_TREE), type (NULL_TREE),
    want (w),
    deduping (false), scopes (NULL), previous (NULL)
  {
    preserve_state ();
  }
  ~name_lookup ()
  {
    gcc_checking_assert (!deduping);
    restore_state ();
  }

private: /* Uncopyable, unmovable, unassignable. I am a rock. */
  name_lookup (const name_lookup &);
  name_lookup &operator= (const name_lookup &);

 public:
  /* Turn on or off deduping mode.  */
  void dedup (bool state)
  {
    if (deduping != state)
      {
	deduping = state;
	lookup_mark (value, state);
      }
  }

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

public:
  static tree ambiguous (tree thing, tree current);
  void add_value (tree new_val);
private:
  void add_overload (tree fns);
  void add_type (tree new_type);
  bool process_binding (tree val_bind, tree type_bind);
  unsigned process_module_binding (tree val_bind, tree type_bind, unsigned);
  /* Look in only namespace.  */
  bool search_namespace_only (tree scope);
  /* Look in namespace and its (recursive) inlines. Ignore using
     directives.  Return true if something found (inc dups). */
  bool search_namespace (tree scope);
  /* Look in the using directives of namespace + inlines using
     qualified lookup rules.  */
  bool search_usings (tree scope);

private:
  void queue_namespace (using_queue& queue, int depth, tree scope);
  void queue_usings (using_queue& queue, int depth, vec<tree, va_gc> *usings);

private:
  void add_fns (tree);

 private:
  void adl_expr (tree);
  void adl_type (tree);
  void adl_template_arg (tree);
  void adl_class (tree);
  void adl_enum (tree);
  void adl_bases (tree);
  void adl_class_only (tree);
  void adl_namespace (tree);
  void adl_class_fns (tree);
  void adl_namespace_fns (tree, bitmap);

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
  gcc_checking_assert (!deduping);

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
      if (!bool (want & LOOK_want::HIDDEN_FRIEND))
	probe = ovl_skip_hidden (probe);
      if (probe && TREE_CODE (probe) == OVERLOAD
	  && OVL_DEDUP_P (probe))
	/* We're about to add something found by multiple paths, so need to
	   engage deduping mode.  */
	dedup (true);
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
      /* Disengage deduping mode.  */
      dedup (false);
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
   true if we actually found something noteworthy.  Hiddenness has
   already been handled in the caller.  */

bool
name_lookup::process_binding (tree new_val, tree new_type)
{
  /* Did we really see a type? */
  if (new_type
      && (want & LOOK_want::TYPE_NAMESPACE) == LOOK_want::NAMESPACE)
    new_type = NULL_TREE;

  new_val = strip_using_decl (new_val);
  new_type = strip_using_decl (new_type);

  /* Do we really see a value? */
  if (new_val)
    switch (TREE_CODE (new_val))
      {
      case TEMPLATE_DECL:
	/* If we expect types or namespaces, and not templates,
	   or this is not a template class.  */
	if (bool (want & LOOK_want::TYPE_NAMESPACE)
	    && !DECL_TYPE_TEMPLATE_P (new_val))
	  new_val = NULL_TREE;
	break;
      case TYPE_DECL:
	if ((want & LOOK_want::TYPE_NAMESPACE) == LOOK_want::NAMESPACE
	    || (new_type && bool (want & LOOK_want::TYPE)))
	  new_val = NULL_TREE;
	break;
      case NAMESPACE_DECL:
	if ((want & LOOK_want::TYPE_NAMESPACE) == LOOK_want::TYPE)
	  new_val = NULL_TREE;
	break;
      default:
	if (bool (want & LOOK_want::TYPE_NAMESPACE))
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

/* If we're importing a module containing this binding, add it to the
   lookup set.  The trickiness is with namespaces, we only want to
   find it once.  */

unsigned
name_lookup::process_module_binding (tree new_val, tree new_type,
				     unsigned marker)
{
  /* Optimize for (re-)finding a public namespace.  We only need to
     look once.  */
  if (new_val && !new_type
      && TREE_CODE (new_val) == NAMESPACE_DECL
      && TREE_PUBLIC (new_val)
      && !DECL_NAMESPACE_ALIAS (new_val))
    {
      if (marker & 2)
	return marker;
      marker |= 2;
    }

  if (new_type || new_val)
    marker |= process_binding (new_val, new_type);

  return marker;
}

/* Look in exactly namespace SCOPE.  */

bool
name_lookup::search_namespace_only (tree scope)
{
  bool found = false;
  if (tree *binding = find_namespace_slot (scope, name))
    {
      tree val = *binding;
      if (TREE_CODE (val) == BINDING_VECTOR)
	{
	  /* I presume the binding list is going to be sparser than
	     the import bitmap.  Hence iterate over the former
	     checking for bits set in the bitmap.  */
	  bitmap imports = get_import_bitmap ();
	  binding_cluster *cluster = BINDING_VECTOR_CLUSTER_BASE (val);
	  int marker = 0;
	  int dup_detect = 0;

	  if (tree bind = cluster->slots[BINDING_SLOT_CURRENT])
	    {
	      if (!deduping)
		{
		  if (named_module_purview_p ())
		    {
		      dup_detect |= 2;

		      if (STAT_HACK_P (bind) && MODULE_BINDING_GLOBAL_P (bind))
			dup_detect |= 1;
		    }
		  else
		    dup_detect |= 1;
		}
	      tree type = NULL_TREE;
	      tree value = bind;

	      if (STAT_HACK_P (bind))
		{
		  type = STAT_TYPE (bind);
		  value = STAT_DECL (bind);

		  if (!bool (want & LOOK_want::HIDDEN_FRIEND))
		    {
		      if (STAT_TYPE_HIDDEN_P (bind))
			type = NULL_TREE;
		      if (STAT_DECL_HIDDEN_P (bind))
			value = NULL_TREE;
		      else
			value = ovl_skip_hidden (value);
		    }
		}
	      else if (!bool (want & LOOK_want::HIDDEN_FRIEND))
		value = ovl_skip_hidden (value);

	      marker = process_module_binding (value, type, marker);
	    }

	  /* Scan the imported bindings.  */
	  unsigned ix = BINDING_VECTOR_NUM_CLUSTERS (val);
	  if (BINDING_VECTOR_SLOTS_PER_CLUSTER == BINDING_SLOTS_FIXED)
	    {
	      ix--;
	      cluster++;
	    }

	  /* Do this in forward order, so we load modules in an order
	     the user expects.  */
	  for (; ix--; cluster++)
	    for (unsigned jx = 0; jx != BINDING_VECTOR_SLOTS_PER_CLUSTER; jx++)
	      {
		/* Are we importing this module?  */
		if (unsigned base = cluster->indices[jx].base)
		  if (unsigned span = cluster->indices[jx].span)
		    do
		      if (bool (want & LOOK_want::ANY_REACHABLE)
			  || bitmap_bit_p (imports, base))
			goto found;
		    while (++base, --span);
		continue;

	      found:;
		/* Is it loaded?  */
		if (cluster->slots[jx].is_lazy ())
		  {
		    gcc_assert (cluster->indices[jx].span == 1);
		    lazy_load_binding (cluster->indices[jx].base,
				       scope, name, &cluster->slots[jx]);
		  }
		tree bind = cluster->slots[jx];
		if (!bind)
		  /* Load errors could mean there's nothing here.  */
		  continue;

		/* Extract what we can see from here.  If there's no
		   stat_hack, then everything was exported.  */
		tree type = NULL_TREE;

		/* If STAT_HACK_P is false, everything is visible, and
		   there's no duplication possibilities.  */
		if (STAT_HACK_P (bind))
		  {
		    if (!deduping)
		      {
			/* Do we need to engage deduplication?  */
			int dup = 0;
			if (MODULE_BINDING_GLOBAL_P (bind))
			  dup |= 1;
			if (MODULE_BINDING_PARTITION_P (bind))
			  dup |= 2;
			if (unsigned hit = dup_detect & dup)
			  {
			    if ((hit & 1 && BINDING_VECTOR_GLOBAL_DUPS_P (val))
				|| (hit & 2
				    && BINDING_VECTOR_PARTITION_DUPS_P (val)))
			      dedup (true);
			  }
			dup_detect |= dup;
		      }

		    if (bool (want & LOOK_want::ANY_REACHABLE))
		      {
			type = STAT_TYPE (bind);
			bind = STAT_DECL (bind);
		      }
		    else
		      {
			if (STAT_TYPE_VISIBLE_P (bind))
			  type = STAT_TYPE (bind);
			bind = STAT_VISIBLE (bind);
		      }
		  }

		/* And process it.  */
		marker = process_module_binding (bind, type, marker);
	      }
	  found |= marker & 1;
	}
      else
	{
	  /* Only a current module binding, visible from the current module.  */
	  tree bind = *binding;
	  tree value = bind, type = NULL_TREE;

	  if (STAT_HACK_P (bind))
	    {
	      type = STAT_TYPE (bind);
	      value = STAT_DECL (bind);

	      if (!bool (want & LOOK_want::HIDDEN_FRIEND))
		{
		  if (STAT_TYPE_HIDDEN_P (bind))
		    type = NULL_TREE;
		  if (STAT_DECL_HIDDEN_P (bind))
		    value = NULL_TREE;
		  else
		    value = ovl_skip_hidden (value);
		}
	    }
	  else if (!bool (want & LOOK_want::HIDDEN_FRIEND))
	    value = ovl_skip_hidden (value);

	  found |= process_binding (value, type);
	}
    }

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

  /* Don't look into inline children, if we're looking for an
     anonymous name -- it must be in the current scope, if anywhere.  */
  if (name)
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
  if (vec<tree, va_gc> *usings = NAMESPACE_LEVEL (scope)->using_directives)
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

  dedup (false);

  return found;
}

/* Add SCOPE to the unqualified search queue, recursively add its
   inlines and those via using directives.  */

void
name_lookup::queue_namespace (using_queue& queue, int depth, tree scope)
{
  if (see_and_mark (scope))
    return;

  /* Record it.  */
  tree common = scope;
  while (SCOPE_DEPTH (common) > depth)
    common = CP_DECL_CONTEXT (common);
  queue.safe_push (using_pair (common, scope));

  /* Queue its inline children.  */
  if (vec<tree, va_gc> *inlinees = DECL_NAMESPACE_INLINEES (scope))
    for (unsigned ix = inlinees->length (); ix--;)
      queue_namespace (queue, depth, (*inlinees)[ix]);

  /* Queue its using targets.  */
  queue_usings (queue, depth, NAMESPACE_LEVEL (scope)->using_directives);
}

/* Add the namespaces in USINGS to the unqualified search queue.  */

void
name_lookup::queue_usings (using_queue& queue, int depth, vec<tree, va_gc> *usings)
{
  if (usings)
    for (unsigned ix = usings->length (); ix--;)
      queue_namespace (queue, depth, (*usings)[ix]);
}

/* Unqualified namespace lookup in SCOPE.
   1) add scope+inlins to worklist.
   2) recursively add target of every using directive
   3) for each worklist item where SCOPE is common ancestor, search it
   4) if nothing find, scope=parent, goto 1.  */

bool
name_lookup::search_unqualified (tree scope, cp_binding_level *level)
{
  using_queue queue;
  bool found = false;

  /* Queue local using-directives.  */
  for (; level->kind != sk_namespace; level = level->level_chain)
    queue_usings (queue, SCOPE_DEPTH (scope), level->using_directives);

  for (; !found; scope = CP_DECL_CONTEXT (scope))
    {
      gcc_assert (!DECL_NAMESPACE_ALIAS (scope));
      int depth = SCOPE_DEPTH (scope);

      /* Queue namespaces reachable from SCOPE. */
      queue_namespace (queue, depth, scope);

      /* Search every queued namespace where SCOPE is the common
	 ancestor.  Adjust the others.  */
      unsigned ix = 0;
      do
	{
	  using_pair &pair = queue[ix];
	  while (pair.first == scope)
	    {
	      found |= search_namespace_only (pair.second);
	      pair = queue.pop ();
	      if (ix == queue.length ())
		goto done;
	    }
	  /* The depth is the same as SCOPE, find the parent scope.  */
	  if (SCOPE_DEPTH (pair.first) == depth)
	    pair.first = CP_DECL_CONTEXT (pair.first);
	  ix++;
	}
      while (ix < queue.length ());
    done:;
      if (scope == global_namespace)
	break;

      /* If looking for hidden friends, we only look in the innermost
	 namespace scope.  [namespace.memdef]/3 If a friend
	 declaration in a non-local class first declares a class,
	 function, class template or function template the friend is a
	 member of the innermost enclosing namespace.  See also
	 [basic.lookup.unqual]/7 */
      if (bool (want & LOOK_want::HIDDEN_FRIEND))
	break;
    }

  dedup (false);

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

/* Add the overloaded fns of SCOPE.  */

void
name_lookup::adl_namespace_fns (tree scope, bitmap imports)
{
  if (tree *binding = find_namespace_slot (scope, name))
    {
      tree val = *binding;
      if (TREE_CODE (val) != BINDING_VECTOR)
	add_fns (ovl_skip_hidden (MAYBE_STAT_DECL (val)));
      else
	{
	  /* I presume the binding list is going to be sparser than
	     the import bitmap.  Hence iterate over the former
	     checking for bits set in the bitmap.  */
	  binding_cluster *cluster = BINDING_VECTOR_CLUSTER_BASE (val);
	  int dup_detect = 0;

	  if (tree bind = cluster->slots[BINDING_SLOT_CURRENT])
	    {
	      /* The current TU's bindings must be visible, we don't
		 need to check the bitmaps.  */

	      if (!deduping)
		{
		  if (named_module_purview_p ())
		    {
		      dup_detect |= 2;

		      if (STAT_HACK_P (bind) && MODULE_BINDING_GLOBAL_P (bind))
			dup_detect |= 1;
		    }
		  else
		    dup_detect |= 1;
		}

	      add_fns (ovl_skip_hidden (MAYBE_STAT_DECL (bind)));
	    }

	  /* Scan the imported bindings.  */
	  unsigned ix = BINDING_VECTOR_NUM_CLUSTERS (val);
	  if (BINDING_VECTOR_SLOTS_PER_CLUSTER == BINDING_SLOTS_FIXED)
	    {
	      ix--;
	      cluster++;
	    }

	  /* Do this in forward order, so we load modules in an order
	     the user expects.  */
	  for (; ix--; cluster++)
	    for (unsigned jx = 0; jx != BINDING_VECTOR_SLOTS_PER_CLUSTER; jx++)
	      {
		/* Functions are never on merged slots.  */
		if (!cluster->indices[jx].base
		    || cluster->indices[jx].span != 1)
		  continue;

		/* Is this slot visible?  */
		if (!bitmap_bit_p (imports, cluster->indices[jx].base))
		  continue;

		/* Is it loaded.  */
		if (cluster->slots[jx].is_lazy ())
		  lazy_load_binding (cluster->indices[jx].base,
				     scope, name, &cluster->slots[jx]);

		tree bind = cluster->slots[jx];
		if (!bind)
		  /* Load errors could mean there's nothing here.  */
		  continue;

		if (STAT_HACK_P (bind))
		  {
		    if (!deduping)
		      {
			/* Do we need to engage deduplication?  */
			int dup = 0;
			if (MODULE_BINDING_GLOBAL_P (bind))
			  dup |= 1;
			if (MODULE_BINDING_PARTITION_P (bind))
			  dup |= 2;
			if (unsigned hit = dup_detect & dup)
			  if ((hit & 1 && BINDING_VECTOR_GLOBAL_DUPS_P (val))
			      || (hit & 2
				  && BINDING_VECTOR_PARTITION_DUPS_P (val)))
			    dedup (true);
			dup_detect |= dup;
		      }

		    bind = STAT_VISIBLE (bind);
		  }

		add_fns (bind);
	      }
	}
    }
}

/* Add the hidden friends of SCOPE.  */

void
name_lookup::adl_class_fns (tree type)
{
  /* Add friends.  */
  for (tree list = DECL_FRIENDLIST (TYPE_MAIN_DECL (type));
       list; list = TREE_CHAIN (list))
    if (name == FRIEND_NAME (list))
      {
	tree context = NULL_TREE; /* Lazily computed.  */
	for (tree friends = FRIEND_DECLS (list); friends;
	     friends = TREE_CHAIN (friends))
	  {
	    tree fn = TREE_VALUE (friends);

	    /* Only interested in global functions with potentially hidden
	       (i.e. unqualified) declarations.  */
	    if (!context)
	      context = decl_namespace_context (type);
	    if (CP_DECL_CONTEXT (fn) != context)
	      continue;

	    dedup (true);

	    /* Template specializations are never found by name lookup.
	       (Templates themselves can be found, but not template
	       specializations.)  */
	    if (TREE_CODE (fn) == FUNCTION_DECL && DECL_USE_TEMPLATE (fn))
	      continue;

	    add_fns (fn);
	  }
      }
}

/* Find the containing non-inlined namespace, add it and all its
   inlinees.  */

void
name_lookup::adl_namespace (tree scope)
{
  if (see_and_mark (scope))
    return;

  /* Look down into inline namespaces.  */
  if (vec<tree, va_gc> *inlinees = DECL_NAMESPACE_INLINEES (scope))
    for (unsigned ix = inlinees->length (); ix--;)
      adl_namespace ((*inlinees)[ix]);

  if (DECL_NAMESPACE_INLINE_P (scope))
    /* Mark parent.  */
    adl_namespace (CP_DECL_CONTEXT (scope));
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
   structure.

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

  complete_type (type);
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
name_lookup::adl_enum (tree type)
{
  type = TYPE_MAIN_VARIANT (type);
  if (see_and_mark (type))
    return;

  if (TYPE_CLASS_SCOPE_P (type))
    adl_class_only (TYPE_CONTEXT (type));
  else
    adl_namespace (decl_namespace_context (type));
}

void
name_lookup::adl_expr (tree expr)
{
  if (!expr)
    return;

  gcc_assert (!TYPE_P (expr));

  if (TREE_TYPE (expr) != unknown_type_node)
    {
      adl_type (unlowered_expr_type (expr));
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
      adl_enum (type);
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
  gcc_checking_assert (!vec_safe_length (scopes));

  /* Gather each associated entity onto the lookup's scope list.  */
  unsigned ix;
  tree arg;

  FOR_EACH_VEC_ELT_REVERSE (*args, ix, arg)
    /* OMP reduction operators put an ADL-significant type as the
       first arg. */
    if (TYPE_P (arg))
      adl_type (arg);
    else
      adl_expr (arg);

  if (vec_safe_length (scopes))
    {
      /* Now do the lookups.  */
      value = fns;
      if (fns)
	dedup (true);

      /* INST_PATH will be NULL, if this is /not/ 2nd-phase ADL.  */
      bitmap inst_path = NULL;
      /* VISIBLE is the regular import bitmap.  */
      bitmap visible = visible_instantiation_path (&inst_path);

      for (unsigned ix = scopes->length (); ix--;)
	{
	  tree scope = (*scopes)[ix];
	  if (TREE_CODE (scope) == NAMESPACE_DECL)
	    adl_namespace_fns (scope, visible);
	  else
	    {
	      if (RECORD_OR_UNION_TYPE_P (scope))
		adl_class_fns (scope);

	      /* During 2nd phase ADL: Any exported declaration D in N
		 declared within the purview of a named module M
		 (10.2) is visible if there is an associated entity
		 attached to M with the same innermost enclosing
		 non-inline namespace as D.
		 [basic.lookup.argdep]/4.4 */

	      if (!inst_path)
		/* Not 2nd phase.  */
		continue;

	      tree ctx = CP_DECL_CONTEXT (TYPE_NAME (scope));
	      if (TREE_CODE (ctx) != NAMESPACE_DECL)
		/* Not namespace-scope class.  */
		continue;

	      tree origin = get_originating_module_decl (TYPE_NAME (scope));
	      tree not_tmpl = STRIP_TEMPLATE (origin);
	      if (!DECL_LANG_SPECIFIC (not_tmpl)
		  || !DECL_MODULE_IMPORT_P (not_tmpl))
		/* Not imported.  */
		continue;

	      unsigned module = get_importing_module (origin);

	      if (!bitmap_bit_p (inst_path, module))
		/* Not on path of instantiation.  */
		continue;

	      if (bitmap_bit_p (visible, module))
		/* If the module was in the visible set, we'll look at
		   its namespace partition anyway.  */
		continue;

	      if (tree *slot = find_namespace_slot (ctx, name, false))
		if (binding_slot *mslot = search_imported_binding_slot (slot, module))
		  {
		    if (mslot->is_lazy ())
		      lazy_load_binding (module, ctx, name, mslot);

		    if (tree bind = *mslot)
		      {
			/* We must turn on deduping, because some other class
			   from this module might also be in this namespace.  */
			dedup (true);

			/* Add the exported fns  */
			if (STAT_HACK_P (bind))
			  add_fns (STAT_VISIBLE (bind));
		      }
		  }
	    }
	}

      fns = value;
      dedup (false);
    }

  return fns;
}

static bool qualified_namespace_lookup (tree, name_lookup *);
static void consider_binding_level (tree name,
				    best_match <tree, const char *> &bm,
				    cp_binding_level *lvl,
				    bool look_within_fields,
				    enum lookup_name_fuzzy_kind kind);

/* ADL lookup of NAME.  FNS is the result of regular lookup, and we
   don't add duplicates to it.  ARGS is the vector of call
   arguments (which will not be empty).  */

tree
lookup_arg_dependent (tree name, tree fns, vec<tree, va_gc> *args)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);
  name_lookup lookup (name);
  return lookup.search_adl (fns, args);
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
    if (tree binding = (*member_vec)[ix])
      if (OVL_NAME (binding) == name)
	return binding;

  return NULL_TREE;
}

/* Linear search of (partially ordered) fields of KLASS for NAME.  */

static tree
fields_linear_search (tree klass, tree name, bool want_type)
{
  for (tree fields = TYPE_FIELDS (klass); fields; fields = DECL_CHAIN (fields))
    {
      tree decl = fields;

      if (TREE_CODE (decl) == FIELD_DECL
	  && ANON_AGGR_TYPE_P (TREE_TYPE (decl)))
	{
	  if (tree temp = search_anon_aggr (TREE_TYPE (decl), name, want_type))
	    return temp;
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

/* Like fields_linear_search, but specific for "_" name.  There can be multiple
   name-independent non-static data members and in that case a TREE_LIST with the
   ambiguous decls should be returned.  */

static tree
name_independent_linear_search (tree val, tree klass, tree name)
{
  for (tree fields = TYPE_FIELDS (klass); fields; fields = DECL_CHAIN (fields))
    {
      tree decl = fields;

      if (TREE_CODE (decl) == FIELD_DECL
	  && ANON_AGGR_TYPE_P (TREE_TYPE (decl)))
	{
	  if (tree temp = search_anon_aggr (TREE_TYPE (decl), name, false))
	    {
	      decl = temp;
	      goto add;
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

    add:
      if (val == NULL_TREE)
	val = decl;
      else
	{
	  if (TREE_CODE (val) != TREE_LIST)
	    {
	      if (TREE_CODE (val) == OVERLOAD
		  && OVL_DEDUP_P (val)
		  && TREE_CODE (decl) == USING_DECL)
		{
		  val = ovl_make (decl, val);
		  continue;
		}
	      val = tree_cons (NULL_TREE, val, NULL_TREE);
	      TREE_TYPE (val) = error_mark_node;
	    }
	  if (TREE_CODE (decl) == TREE_LIST)
	    val = chainon (decl, val);
	  else
	    {
	      val = tree_cons (NULL_TREE, decl, val);
	      TREE_TYPE (val) = error_mark_node;
	    }
	}
    }

  return val;
}

/* Look for NAME member inside of anonymous aggregate ANON.  Although
   such things should only contain FIELD_DECLs, we check that too
   late, and would give very confusing errors if we weren't
   permissive here.  */

tree
search_anon_aggr (tree anon, tree name, bool want_type)
{
  gcc_assert (COMPLETE_TYPE_P (anon));
  tree ret = get_class_binding_direct (anon, name, want_type);
  return ret;
}

/* Look for NAME as an immediate member of KLASS (including
   anon-members or unscoped enum member).  TYPE_OR_FNS is zero for
   regular search.  >0 to get a type binding (if there is one) and <0
   if you want (just) the member function binding.

   Use this if you do not want lazy member creation.  */

tree
get_class_binding_direct (tree klass, tree name, bool want_type)
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
      else if (TREE_CODE (val) == OVERLOAD
	       && OVL_NAME_INDEPENDENT_DECL_P (val))
	{
	  if (want_type)
	    {
	      while (TREE_CODE (val) == OVERLOAD
		     && OVL_NAME_INDEPENDENT_DECL_P (val))
		val = OVL_CHAIN (val);
	      if (STAT_HACK_P (val))
		val = STAT_TYPE (val);
	      else if (!DECL_DECLARES_TYPE_P (val))
		val = NULL_TREE;
	    }
	  else
	    {
	      /* OVERLOAD with a special OVL_NAME_INDEPENDENT_DECL_P
		 flag is used under the hood to represent lookup
		 results which include name-independent declarations,
		 and get_class_binding_direct is turning that into
		 TREE_LIST representation (which the callers expect for
		 ambiguous lookups) instead.
		 There are 2 reasons for that:
		 1) in order to keep the member_vec binary search fast, I
		 think it is better to keep OVL_NAME usable on all elements
		 because having to special case TREE_LIST would slow
		 everything down;
		 2) the callers need to be able to chain the results anyway
		 and so need an unshared TREE_LIST they can tweak/destroy.  */
	      tree ovl = val;
	      val = NULL_TREE;
	      while (TREE_CODE (ovl) == OVERLOAD
		     && OVL_NAME_INDEPENDENT_DECL_P (ovl))
		{
		  val = tree_cons (NULL_TREE, OVL_FUNCTION (ovl), val);
		  TREE_TYPE (val) = error_mark_node;
		  ovl = OVL_CHAIN (ovl);
		}
	      if (STAT_HACK_P (ovl))
		val = tree_cons (NULL_TREE, STAT_DECL (ovl), val);
	      else
		val = tree_cons (NULL_TREE, ovl, val);
	      TREE_TYPE (val) = error_mark_node;
	    }
	}
      else if (STAT_HACK_P (val))
	val = want_type ? STAT_TYPE (val) : STAT_DECL (val);
      else if (want_type && !DECL_DECLARES_TYPE_P (val))
	val = NULL_TREE;
    }
  else
    {
      if (member_vec && !want_type)
	val = member_vec_linear_search (member_vec, lookup);

      if (id_equal (lookup, "_") && !want_type)
	val = name_independent_linear_search (val, klass, lookup);
      else if (!val || (TREE_CODE (val) == OVERLOAD && OVL_DEDUP_P (val)))
	/* Dependent using declarations are a 'field', make sure we
	   return that even if we saw an overload already.  */
	if (tree field_val = fields_linear_search (klass, lookup, want_type))
	  {
	    if (!val)
	      val = field_val;
	    else if (TREE_CODE (field_val) == USING_DECL)
	      val = ovl_make (field_val, val);
	  }
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

/* We're about to lookup NAME in KLASS.  Make sure any lazily declared
   members are now declared.  */

static void
maybe_lazily_declare (tree klass, tree name)
{
  /* See big comment anout module_state::write_pendings regarding adding a check
     bit.  */
  if (modules_p ())
    lazy_load_pendings (TYPE_NAME (klass));

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
  else if (name == assign_op_identifier)
    {
      if (CLASSTYPE_LAZY_COPY_ASSIGN (klass))
	lazily_declare_fn (sfk_copy_assignment, klass);
      if (CLASSTYPE_LAZY_MOVE_ASSIGN (klass))
	lazily_declare_fn (sfk_move_assignment, klass);
    }
}

/* Look for NAME's binding in exactly KLASS.  See
   get_class_binding_direct for argument description.  Does lazy
   special function creation as necessary.  */

tree
get_class_binding (tree klass, tree name, bool want_type /*=false*/)
{
  klass = complete_type (klass);

  if (COMPLETE_TYPE_P (klass))
    maybe_lazily_declare (klass, name);

  return get_class_binding_direct (klass, name, want_type);
}

/* Find the slot containing overloads called 'NAME'.  If there is no
   such slot and the class is complete, create an empty one, at the
   correct point in the sorted member vector.  Otherwise return NULL.
   Deals with conv_op marker handling.  */

tree *
find_member_slot (tree klass, tree name)
{
  bool complete_p = COMPLETE_TYPE_P (klass);

  vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (klass);
  if (!member_vec)
    {
      vec_alloc (member_vec, 8);
      CLASSTYPE_MEMBER_VEC (klass) = member_vec;
      if (complete_p)
	/* If the class is complete but had no member_vec, we need to
	   add the TYPE_FIELDS into it.  We're also most likely to be
	   adding ctors & dtors, so ask for 6 spare slots (the
	   abstract cdtors and their clones).  */
	member_vec = set_class_bindings (klass, 6);
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

  /* No slot found, add one if the class is complete.  */
  if (complete_p)
    {
      /* Do exact allocation, as we don't expect to add many.  */
      gcc_assert (name != conv_op_identifier);
      vec_safe_reserve_exact (member_vec, 1);
      CLASSTYPE_MEMBER_VEC (klass) = member_vec;
      member_vec->quick_insert (ix, NULL_TREE);
      return &(*member_vec)[ix];
    }

  return NULL;
}

/* KLASS is an incomplete class to which we're adding a method NAME.
   Add a slot and deal with conv_op marker handling.  */

tree *
add_member_slot (tree klass, tree name)
{
  gcc_assert (!COMPLETE_TYPE_P (klass));

  vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (klass);
  vec_safe_push (member_vec, NULL_TREE);
  CLASSTYPE_MEMBER_VEC (klass) = member_vec;

  tree *slot = &member_vec->last ();
  if (IDENTIFIER_CONV_OP_P (name))
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

  if (id_equal (name_a, "_"))
    {
      /* Sort name-independent members first.  */
      if (name_independent_decl_p (a))
	{
	  if (name_independent_decl_p (b))
	    {
	      if (DECL_UID (a) != DECL_UID (b))
		return DECL_UID (a) < DECL_UID (b) ? -1 : +1;
	      gcc_assert (a == b);
	      return 0;
	    }
	  else
	    return -1;
	}
      else if (name_independent_decl_p (b))
	return +1;
    }

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
  if (DECL_UID (a) != DECL_UID (b))
    return DECL_UID (a) < DECL_UID (b) ? -1 : +1;
  gcc_assert (a == b);
  return 0;
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

  resort_data.new_value (&name_a, &name_a, resort_data.cookie);
  resort_data.new_value (&name_b, &name_b, resort_data.cookie);

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
      member_vec->qsort (resort_member_name_cmp);
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
   There are four cases (which could all be combined):
   1) a TYPE_DECL and non TYPE_DECL.  Deploy STAT_HACK as appropriate.
   2) a USING_DECL and an overload.  If the USING_DECL is dependent,
   it wins.  Otherwise the OVERLOAD does.
   3) two USING_DECLS.
   4) name-independent members plus others. ...

   member_name_cmp will have ordered duplicates as
   <name_independent><fns><using><type>  */

static void
member_vec_dedup (vec<tree, va_gc> *member_vec)
{
  unsigned len = member_vec->length ();
  unsigned store = 0;

  if (!len)
    return;

  tree name = OVL_NAME ((*member_vec)[0]);
  for (unsigned jx, ix = 0; ix < len; ix = jx)
    {
      tree current = NULL_TREE;
      tree to_type = NULL_TREE;
      tree to_using = NULL_TREE;
      tree marker = NULL_TREE;
      unsigned name_independent = ix;

      for (jx = ix; jx < len; jx++)
	{
	  tree next = (*member_vec)[jx];
	  if (jx != ix)
	    {
	      tree next_name = OVL_NAME (next);
	      if (next_name != name)
		{
		  name = next_name;
		  break;
		}
	    }

	  if (IDENTIFIER_CONV_OP_P (name))
	    {
	      marker = next;
	      next = OVL_CHAIN (next);
	    }

	  if (TREE_CODE (next) == USING_DECL)
	    {
	      if (IDENTIFIER_CTOR_P (name))
		/* Dependent inherited ctor. */
		continue;

	      next = strip_using_decl (next);
	      if (TREE_CODE (next) == USING_DECL)
		{
		  to_using = next;
		  continue;
		}

	      if (is_overloaded_fn (next))
		continue;
	    }

	  if (DECL_DECLARES_TYPE_P (next))
	    {
	      to_type = next;
	      continue;
	    }

	  if (name_independent_decl_p (next))
	    name_independent = jx + 1;
	  else if (!current)
	    current = next;
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

      for (unsigned kx = name_independent; kx > ix; --kx)
	if (!current)
	  current = (*member_vec)[kx - 1];
	else if (current == to_type)
	  current = stat_hack ((*member_vec)[kx - 1], to_type);
	else
	  {
	    current = ovl_make ((*member_vec)[kx - 1], current);
	    OVL_NAME_INDEPENDENT_DECL_P (current) = 1;
	  }

      if (current)
	{
	  if (marker)
	    {
	      OVL_CHAIN (marker) = current;
	      current = marker;
	    }
	  (*member_vec)[store++] = current;
	}
    }

  while (store++ < len)
    member_vec->pop ();
}

/* Add the non-function members to CLASSTYPE_MEMBER_VEC.  If there is
   no existing MEMBER_VEC and fewer than 8 fields, do nothing.  We
   know there must be at least 1 field -- the self-reference
   TYPE_DECL, except for anon aggregates, which will have at least
   one field anyway.  If EXTRA < 0, always create the vector.  */

vec<tree, va_gc> *
set_class_bindings (tree klass, int extra)
{
  unsigned n_fields = count_class_fields (klass);
  vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (klass);

  if (member_vec || n_fields >= 8 || extra < 0)
    {
      /* Append the new fields.  */
      vec_safe_reserve_exact (member_vec, n_fields + (extra >= 0 ? extra : 0));
      member_vec_append_class_fields (member_vec, klass);
    }

  if (member_vec)
    {
      CLASSTYPE_MEMBER_VEC (klass) = member_vec;
      member_vec->qsort (member_name_cmp);
      member_vec_dedup (member_vec);
    }

  return member_vec;
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
      member_vec->qsort (member_name_cmp);
      member_vec_dedup (member_vec);
    }
}

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
  cxx_binding *binding = free_bindings;

  if (binding)
    free_bindings = binding->previous;
  else
    binding = ggc_alloc<cxx_binding> ();

  /* Clear flags by default.  */
  LOCAL_BINDING_P (binding) = false;
  INHERITED_VALUE_BINDING_P (binding) = false;
  HIDDEN_TYPE_BINDING_P (binding) = false;

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
  LOCAL_BINDING_P (binding) = (level != class_binding_level);

  /* And put it on the front of the list of bindings for ID.  */
  IDENTIFIER_BINDING (id) = binding;
}

/* Remove the binding for DECL which should be the innermost binding
   for ID.  */

void
pop_local_binding (tree id, tree decl)
{
  if (!id || IDENTIFIER_ANON_P (id))
    /* It's easiest to write the loops that call this function without
       checking whether or not the entities involved have names.  We
       get here for such an entity.  */
    return;

  /* Get the innermost binding for ID.  */
  cxx_binding *binding = IDENTIFIER_BINDING (id);

  /* The name should be bound.  */
  gcc_assert (binding != NULL);

  /* The DECL will be either the ordinary binding or the type binding
     for this identifier.  Remove that binding.  We don't have to
     clear HIDDEN_TYPE_BINDING_P, as the whole binding will be going
     away.  */
  if (binding->value == decl)
    binding->value = NULL_TREE;
  else if (binding->type == decl)
    binding->type = NULL_TREE;
  else
    {
      /* Name-independent variable was found after at least one declaration
	 with the same name.  */
      gcc_assert (TREE_CODE (binding->value) == TREE_LIST);
      if (TREE_VALUE (binding->value) != decl)
	{
	  binding->value = nreverse (binding->value);
	  /* Skip over TREE_LISTs added in pushdecl for check_local_shadow
	     detected declarations, formerly at the tail, now at the start
	     of the list.  */
	  while (TREE_PURPOSE (binding->value) == error_mark_node)
	    binding->value = TREE_CHAIN (binding->value);
	}
      gcc_assert (TREE_VALUE (binding->value) == decl);
      binding->value = TREE_CHAIN (binding->value);
      while (binding->value
	     && TREE_PURPOSE (binding->value) == error_mark_node)
	binding->value = TREE_CHAIN (binding->value);
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
      decl = make_typename_type (USING_DECL_SCOPE (decl),
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
  return (TREE_CODE (ovl) == OVERLOAD
	  && OVL_HIDDEN_P (ovl)
	  && DECL_IS_UNDECLARED_BUILTIN (OVL_FUNCTION (ovl)));
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
supplement_binding (cxx_binding *binding, tree decl)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);

  tree bval = binding->value;
  bool ok = true;
  if (bval
      && TREE_CODE (bval) == TREE_LIST
      && name_independent_decl_p (TREE_VALUE (bval)))
    bval = TREE_VALUE (bval);
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
  else if (TREE_CODE (bval) == USING_DECL
	   && CONST_DECL_USING_P (decl))
    /* Let the clone hide the using-decl that introduced it.  */
    binding->value = decl;
  else if (name_independent_decl_p (decl))
    {
      if (cxx_dialect < cxx26)
	pedwarn (DECL_SOURCE_LOCATION (decl), OPT_Wc__26_extensions,
		 "name-independent declarations only available with "
		 "%<-std=c++2c%> or %<-std=gnu++2c%>");
      binding->value = name_lookup::ambiguous (decl, binding->value);
    }
  else if (binding->scope->kind != sk_class
	   && TREE_CODE (decl) == USING_DECL
	   && decls_match (target_bval, target_decl))
    /* Since P1787 (DR 36) it is OK to redeclare entities via using-decl,
       except in class scopes.  */
    ok = false;
  else
    {
      if (!error_operand_p (bval))
	diagnose_name_conflict (decl, bval);
      ok = false;
    }

  return ok;
}

/* Diagnose a name conflict between DECL and BVAL.

   This is non-static so maybe_push_used_methods can use it and avoid changing
   the diagnostic for inherit/using4.C; otherwise it should not be used from
   outside this file.  */

void
diagnose_name_conflict (tree decl, tree bval)
{
  auto_diagnostic_group d;
  if (TREE_CODE (decl) == TREE_CODE (bval)
      && TREE_CODE (decl) != NAMESPACE_DECL
      && !DECL_DECLARES_FUNCTION_P (decl)
      && (TREE_CODE (decl) != TYPE_DECL
	  || DECL_ARTIFICIAL (decl) == DECL_ARTIFICIAL (bval))
      && CP_DECL_CONTEXT (decl) == CP_DECL_CONTEXT (bval))
    {
      if (concept_definition_p (decl))
        error ("redeclaration of %q#D with different template parameters",
               decl);
      else
        error ("redeclaration of %q#D", decl);
    }
  else
    error ("%q#D conflicts with a previous declaration", decl);

  inform (location_of (bval), "previous declaration %q#D", bval);
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
	*d = tree_cons (DECL_NAME (*d), NULL_TREE, TREE_CHAIN (*d));
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
  if (TREE_CODE (one) != TREE_CODE (two))
    return false;

  if (!compparms (TYPE_ARG_TYPES (TREE_TYPE (one)),
		  TYPE_ARG_TYPES (TREE_TYPE (two))))
    return false;

  if (TREE_CODE (one) == TEMPLATE_DECL)
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

  if (!equivalently_constrained (one, two))
    return false;

  return true;
}

/* Push DECL into nonclass LEVEL BINDING or SLOT.  OLD is the current
   binding value (possibly with anticipated builtins stripped).
   Diagnose conflicts and return updated decl.  */

static tree
update_binding (cp_binding_level *level, cxx_binding *binding, tree *slot,
		tree old, tree decl, bool hiding = false)
{
  tree old_type = NULL_TREE;
  bool hide_type = false;
  bool hide_value = false;
  bool name_independent_p = false;

  if (!slot)
    {
      old_type = binding->type;
      hide_type = HIDDEN_TYPE_BINDING_P (binding);
      if (!old_type)
	hide_value = hide_type, hide_type = false;
      name_independent_p = name_independent_decl_p (decl);
    }
  else if (STAT_HACK_P (*slot))
    {
      old_type = STAT_TYPE (*slot);
      hide_type = STAT_TYPE_HIDDEN_P (*slot);
      hide_value = STAT_DECL_HIDDEN_P (*slot);
    }

  tree to_val = decl;
  tree to_type = old_type;
  bool local_overload = false;

  gcc_assert (!level || level->kind == sk_namespace ? !binding
	      : level->kind != sk_class && !slot);

  if (old == error_mark_node)
    old = NULL_TREE;

  tree old_bval = old;
  old = strip_using_decl (old);

  if (DECL_IMPLICIT_TYPEDEF_P (decl))
    {
      /* Pushing an artificial decl.  We should not find another
         artificial decl here already -- lookup_elaborated_type will
         have already found it.  */
      gcc_checking_assert (!to_type
			   && !(old && DECL_IMPLICIT_TYPEDEF_P (old)));

      if (old)
	{
	  /* Put DECL into the type slot.  */
	  gcc_checking_assert (!to_type);
	  hide_type = hiding;
	  to_type = decl;
	  to_val = old_bval;
	}
      else
	hide_value = hiding;

      goto done;
    }

  if (old && DECL_IMPLICIT_TYPEDEF_P (old))
    {
      /* OLD is an implicit typedef.  Move it to to_type.  */
      gcc_checking_assert (!to_type);

      to_type = old_bval;
      hide_type = hide_value;
      old = NULL_TREE;
      hide_value = false;
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
		  gcc_checking_assert (!iter.hidden_p ());
		  /* If a function declaration in namespace scope or
		     block scope has the same name and the same
		     parameter-type- list (8.3.5) as a function
		     introduced by a using-declaration, and the
		     declarations do not declare the same function,
		     the program is ill-formed.  [namespace.udecl]/14 */
		  if (tree match = duplicate_decls (decl, fn, hiding))
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

      local_overload = old && level && level->kind != sk_namespace;
      to_val = ovl_insert (decl, old, -int (hiding));
    }
  else if (old)
    {
      if (name_independent_p)
	to_val = name_lookup::ambiguous (decl, old);
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
	  if (tree match = duplicate_decls (decl, old))
	    {
	      gcc_checking_assert (!hide_value && !hiding);
	      return match;
	    }
	  else
	    goto conflict;
	}
      else
	{
	conflict:
	  diagnose_name_conflict (decl, old_bval);
	  to_val = NULL_TREE;
	}
    }
  else if (hiding)
    hide_value = true;

 done:
  if (to_val)
    {
      if (local_overload)
	{
	  gcc_checking_assert (binding->value && OVL_P (binding->value));
	  update_local_overload (binding, to_val);
	}
      else if (level
	       && !(TREE_CODE (decl) == NAMESPACE_DECL
		    && !DECL_NAMESPACE_ALIAS (decl)))
	/* Don't add namespaces here.  They're done in
	   push_namespace.  */
	add_decl_to_level (level, decl);

      if (slot)
	{
	  if (STAT_HACK_P (*slot))
	    {
	      STAT_TYPE (*slot) = to_type;
	      STAT_DECL (*slot) = to_val;
	      STAT_TYPE_HIDDEN_P (*slot) = hide_type;
	      STAT_DECL_HIDDEN_P (*slot) = hide_value;
	    }
	  else if (to_type || hide_value)
	    {
	      *slot = stat_hack (to_val, to_type);
	      STAT_TYPE_HIDDEN_P (*slot) = hide_type;
	      STAT_DECL_HIDDEN_P (*slot) = hide_value;
	    }
	  else
	    {
	      gcc_checking_assert (!hide_type);
	      *slot = to_val;
	    }
	}
      else
	{
	  binding->type = to_type;
	  binding->value = to_val;
	  HIDDEN_TYPE_BINDING_P (binding) = hide_type || hide_value;
	}
    }

  return decl;
}

/* Table of identifiers to extern C declarations (or LISTS thereof).  */

static GTY(()) hash_table<named_decl_hash> *extern_c_decls;

/* DECL has C linkage. If we have an existing instance, make sure the
   new one is compatible.  Make sure it has the same exception
   specification [7.5, 7.6].  Add DECL to the map.  */

static void
check_extern_c_conflict (tree decl)
{
  /* Ignore artificial or system header decls.  */
  if (DECL_ARTIFICIAL (decl) || DECL_IN_SYSTEM_HEADER (decl))
    return;

  /* This only applies to decls at namespace scope.  */
  if (!DECL_NAMESPACE_SCOPE_P (decl))
    return;

  if (!extern_c_decls)
    extern_c_decls = hash_table<named_decl_hash>::create_ggc (127);

  tree *slot = extern_c_decls
    ->find_slot_with_hash (DECL_NAME (decl),
			   IDENTIFIER_HASH_VALUE (DECL_NAME (decl)), INSERT);
  if (tree old = *slot)
    {
      if (TREE_CODE (old) == OVERLOAD)
	old = OVL_FUNCTION (old);

      int mismatch = 0;
      if (DECL_CONTEXT (old) == DECL_CONTEXT (decl))
	; /* If they're in the same context, we'll have already complained
	     about a (possible) mismatch, when inserting the decl.  */
      else if (!decls_match (decl, old))
	mismatch = 1;
      else if (TREE_CODE (decl) == FUNCTION_DECL
	       && !comp_except_specs (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (old)),
				      TYPE_RAISES_EXCEPTIONS (TREE_TYPE (decl)),
				      ce_normal))
	mismatch = -1;
      else if (DECL_ASSEMBLER_NAME_SET_P (old))
	SET_DECL_ASSEMBLER_NAME (decl, DECL_ASSEMBLER_NAME (old));

      if (mismatch)
	{
	  auto_diagnostic_group d;
	  pedwarn (DECL_SOURCE_LOCATION (decl), 0,
		   "conflicting C language linkage declaration %q#D", decl);
	  inform (DECL_SOURCE_LOCATION (old),
		  "previous declaration %q#D", old);
	  if (mismatch < 0)
	    inform (DECL_SOURCE_LOCATION (decl),
		    "due to different exception specifications");
	}
      else
	{
	  if (old == *slot)
	    /* The hash table expects OVERLOADS, so construct one with
	       OLD as both the function and the chain.  This allocate
	       an excess OVERLOAD node, but it's rare to have multiple
	       extern "C" decls of the same name.  And we save
	       complicating the hash table logic (which is used
	       elsewhere).  */
	    *slot = ovl_make (old, old);

	  slot = &OVL_CHAIN (*slot);

	  /* Chain it on for c_linkage_binding's use.  */
	  *slot = tree_cons (NULL_TREE, decl, *slot);
	}
    }
  else
    *slot = decl;
}

/* Returns a list of C-linkage decls with the name NAME.  Used in
   c-family/c-pragma.cc to implement redefine_extname pragma.  */

tree
c_linkage_bindings (tree name)
{
  if (extern_c_decls)
    if (tree *slot = extern_c_decls
	->find_slot_with_hash (name, IDENTIFIER_HASH_VALUE (name), NO_INSERT))
      {
	tree result = *slot;
	if (TREE_CODE (result) == OVERLOAD)
	  result = OVL_CHAIN (result);
	return result;
      }

  return NULL_TREE;
}

/* Subroutine of check_local_shadow.  */

static void
inform_shadowed (tree shadowed)
{
  inform (DECL_SOURCE_LOCATION (shadowed),
	  "shadowed declaration is here");
}

/* DECL is being declared at a local scope.  Emit suitable shadow
   warnings.  */

static tree
check_local_shadow (tree decl)
{
  /* Don't complain about the parms we push and then pop
     while tentatively parsing a function declarator.  */
  if (TREE_CODE (decl) == PARM_DECL && !DECL_CONTEXT (decl))
    return NULL_TREE;

  if (DECL_FUNCTION_SCOPE_P (decl))
    {
      tree ctx = DECL_CONTEXT (decl);
      if (DECL_CLONED_FUNCTION_P (ctx)
	  || DECL_TEMPLATE_INSTANTIATED (ctx)
	  || (DECL_LANG_SPECIFIC (ctx)
	      && DECL_DEFAULTED_FN (ctx))
	  || (LAMBDA_FUNCTION_P (ctx)
	      && LAMBDA_EXPR_REGEN_INFO (CLASSTYPE_LAMBDA_EXPR
					 (DECL_CONTEXT (ctx)))))
	/* It suffices to check shadowing only when actually parsing.
	   So punt for clones, instantiations, defaulted functions and
	   regenerated lambdas.  This optimization helps reduce lazy
	   loading cascades with modules.  */
	return NULL_TREE;
    }

  tree old = NULL_TREE;
  cp_binding_level *old_scope = NULL;
  if (cxx_binding *binding = outer_binding (DECL_NAME (decl), NULL, true))
    {
      old = binding->value;
      old_scope = binding->scope;
    }

  if (old
      && (TREE_CODE (old) == PARM_DECL
	  || VAR_P (old)
	  || (TREE_CODE (old) == TYPE_DECL
	      && (!DECL_ARTIFICIAL (old)
		  || TREE_CODE (decl) == TYPE_DECL)))
      && DECL_FUNCTION_SCOPE_P (old)
      && (!DECL_ARTIFICIAL (decl)
	  || is_capture_proxy (decl)
	  || DECL_IMPLICIT_TYPEDEF_P (decl)
	  || (VAR_P (decl) && DECL_ANON_UNION_VAR_P (decl))))
    {
      /* DECL shadows a local thing possibly of interest.  */

      /* DR 2211: check that captures and parameters
	 do not have the same name. */
      if (is_capture_proxy (decl))
	{
	  if (current_lambda_expr ()
	      && DECL_CONTEXT (old) == lambda_function (current_lambda_expr ())
	      && TREE_CODE (old) == PARM_DECL
	      && DECL_NAME (decl) != this_identifier)
	    error_at (DECL_SOURCE_LOCATION (old),
		      "lambda parameter %qD "
		      "previously declared as a capture", old);
	  return NULL_TREE;
	}
      /* Don't complain if it's from an enclosing function.  */
      else if (DECL_CONTEXT (old) == current_function_decl
	       && TREE_CODE (decl) != PARM_DECL
	       && TREE_CODE (old) == PARM_DECL)
	{
	  /* Go to where the parms should be and see if we find
	     them there.  */
	  cp_binding_level *b = current_binding_level->level_chain;

	  if (in_function_try_handler && b->kind == sk_catch)
	    b = b->level_chain;

	  /* Skip artificially added scopes which aren't present
	     in the C++ standard, e.g. for function-try-block or
	     ctor/dtor cleanups.  */
	  while (b->artificial)
	    b = b->level_chain;

	  /* [basic.scope.param] A parameter name shall not be redeclared
	     in the outermost block of the function definition.  */
	  if (b->kind == sk_function_parms)
	    {
	      if (name_independent_decl_p (decl))
		return old;

	      auto_diagnostic_group d;
	      bool emit = true;
	      if (DECL_EXTERNAL (decl))
		emit = pedwarn (DECL_SOURCE_LOCATION (decl), OPT_Wpedantic,
				"declaration of %q#D shadows a parameter",
				decl);
	      else
		error_at (DECL_SOURCE_LOCATION (decl),
			  "declaration of %q#D shadows a parameter", decl);
	      if (emit)
		inform (DECL_SOURCE_LOCATION (old),
			"%q#D previously declared here", old);
	      return NULL_TREE;
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
	      return NULL_TREE;
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
	  if (name_independent_decl_p (decl))
	    return old;

	  auto_diagnostic_group d;
	  bool emit = true;
	  if (DECL_EXTERNAL (decl))
	    emit = pedwarn (DECL_SOURCE_LOCATION (decl), OPT_Wpedantic,
			    "redeclaration of %q#D", decl);
	  else
	    error_at (DECL_SOURCE_LOCATION (decl),
		      "redeclaration of %q#D", decl);
	  if (emit)
	    inform (DECL_SOURCE_LOCATION (old),
		    "%q#D previously declared here", old);
	  return NULL_TREE;
	}
      /* C++11:
	 3.3.3/3:  The name declared in an exception-declaration (...)
	 shall not be redeclared in the outermost block of the handler.
	 3.3.3/2:  A parameter name shall not be redeclared (...) in
	 the outermost block of any handler associated with a
	 function-try-block.  */
      else if (TREE_CODE (old) == VAR_DECL
	       && old_scope == current_binding_level->level_chain
	       && old_scope->kind == sk_catch)
	{
	  if (name_independent_decl_p (decl))
	    return old;

	  auto_diagnostic_group d;
	  bool emit;
	  if (DECL_EXTERNAL (decl))
	    emit = pedwarn (DECL_SOURCE_LOCATION (decl), OPT_Wpedantic,
			    "redeclaration of %q#D", decl);
	  else
	    emit = permerror (DECL_SOURCE_LOCATION (decl),
			      "redeclaration of %q#D", decl);
	  if (emit)
	    inform (DECL_SOURCE_LOCATION (old),
		    "%q#D previously declared here", old);
	  return NULL_TREE;
	}

      /* Don't emit -Wshadow* warnings for name-independent decls.  */
      if (name_independent_decl_p (decl) || name_independent_decl_p (old))
	return NULL_TREE;

      /* If '-Wshadow=compatible-local' is specified without other
	 -Wshadow= flags, we will warn only when the type of the
	 shadowing variable (DECL) can be converted to that of the
	 shadowed parameter (OLD_LOCAL). The reason why we only check
	 if DECL's type can be converted to OLD_LOCAL's type (but not the
	 other way around) is because when users accidentally shadow a
	 parameter, more than often they would use the variable
	 thinking (mistakenly) it's still the parameter. It would be
	 rare that users would use the variable in the place that
	 expects the parameter but thinking it's a new decl.
	 If either object is a TYPE_DECL, '-Wshadow=compatible-local'
	 warns regardless of whether one of the types involved
	 is a subclass of the other, since that is never okay.  */

      enum opt_code warning_code;
      if (warn_shadow)
	warning_code = OPT_Wshadow;
      else if ((TREE_CODE (decl) == TYPE_DECL)
	       ^ (TREE_CODE (old) == TYPE_DECL))
	/* If exactly one is a type, they aren't compatible.  */
	warning_code = OPT_Wshadow_local;
      else if ((TREE_TYPE (old)
		&& TREE_TYPE (decl)
		&& same_type_p (TREE_TYPE (old), TREE_TYPE (decl)))
	       || TREE_CODE (decl) == TYPE_DECL
	       || TREE_CODE (old) == TYPE_DECL
	       || (!dependent_type_p (TREE_TYPE (decl))
		   && !dependent_type_p (TREE_TYPE (old))
		   /* If the new decl uses auto, we don't yet know
		      its type (the old type cannot be using auto
		      at this point, without also being
		      dependent).  This is an indication we're
		      (now) doing the shadow checking too
		      early.  */
		   && !type_uses_auto (TREE_TYPE (decl))
		   && can_convert_arg (TREE_TYPE (old), TREE_TYPE (decl),
				       decl, LOOKUP_IMPLICIT, tf_none)))
	warning_code = OPT_Wshadow_compatible_local;
      else
	warning_code = OPT_Wshadow_local;

      const char *msg;
      if (TREE_CODE (old) == PARM_DECL)
	msg = "declaration of %q#D shadows a parameter";
      else if (is_capture_proxy (old))
	msg = "declaration of %qD shadows a lambda capture";
      else
	msg = "declaration of %qD shadows a previous local";

      auto_diagnostic_group d;
      if (warning_at (DECL_SOURCE_LOCATION (decl), warning_code, msg, decl))
	inform_shadowed (old);
      return NULL_TREE;
    }

  if (!warn_shadow)
    return NULL_TREE;

  /* Don't emit -Wshadow for name-independent decls.  */
  if (name_independent_decl_p (decl))
    return NULL_TREE;

  /* Don't warn for artificial things that are not implicit typedefs.  */
  if (DECL_ARTIFICIAL (decl) && !DECL_IMPLICIT_TYPEDEF_P (decl))
    return NULL_TREE;

  if (nonlambda_method_basetype ())
    if (tree member = lookup_member (current_nonlambda_class_type (),
				     DECL_NAME (decl), /*protect=*/0,
				     /*want_type=*/false, tf_warning_or_error))
      {
	member = MAYBE_BASELINK_FUNCTIONS (member);

	/* Warn if a variable shadows a non-function, or the variable
	   is a function or a pointer-to-function.  */
	if ((!OVL_P (member)
	     || TREE_CODE (decl) == FUNCTION_DECL
	     || (TREE_TYPE (decl)
		 && (TYPE_PTRFN_P (TREE_TYPE (decl))
		     || TYPE_PTRMEMFUNC_P (TREE_TYPE (decl)))))
	    && !warning_suppressed_p (decl, OPT_Wshadow))
	  {
	    auto_diagnostic_group d;
	    if (warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wshadow,
			    "declaration of %qD shadows a member of %qT",
			    decl, current_nonlambda_class_type ())
		&& DECL_P (member))
	      {
		inform_shadowed (member);
		suppress_warning (decl, OPT_Wshadow);
	      }
	  }
	return NULL_TREE;
      }

  /* Now look for a namespace shadow.  */
  old = find_namespace_value (current_namespace, DECL_NAME (decl));
  if (old
      && (VAR_P (old)
	  || (TREE_CODE (old) == TYPE_DECL
	      && (!DECL_ARTIFICIAL (old)
		  || TREE_CODE (decl) == TYPE_DECL)))
      && !DECL_EXTERNAL (decl)
      && !instantiating_current_function_p ()
      && !warning_suppressed_p (decl, OPT_Wshadow))
    /* XXX shadow warnings in outer-more namespaces */
    {
      auto_diagnostic_group d;
      if (warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wshadow,
		      "declaration of %qD shadows a global declaration",
		      decl))
	{
	  inform_shadowed (old);
	  suppress_warning (decl, OPT_Wshadow);
	}
      return NULL_TREE;
    }

  return NULL_TREE;
}

/* DECL is being pushed inside function CTX.  Set its context, if
   needed.  */

static void
set_decl_context_in_fn (tree ctx, tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL
      || (VAR_P (decl) && DECL_EXTERNAL (decl)))
    /* Make sure local externs are marked as such.  OMP UDRs really
       are nested functions.  */
    gcc_checking_assert (DECL_LOCAL_DECL_P (decl)
			 && (DECL_NAMESPACE_SCOPE_P (decl)
			     || (TREE_CODE (decl) == FUNCTION_DECL
				 && DECL_OMP_DECLARE_REDUCTION_P (decl))));

  if (!DECL_CONTEXT (decl)
      /* When parsing the parameter list of a function declarator,
	 don't set DECL_CONTEXT to an enclosing function.  */
      && !(TREE_CODE (decl) == PARM_DECL
	   && parsing_function_declarator ()))
    DECL_CONTEXT (decl) = ctx;
}

/* DECL is a local extern decl.  Find or create the namespace-scope
   decl that it aliases.  Also, determines the linkage of DECL.  */

void
push_local_extern_decl_alias (tree decl)
{
  if (dependent_type_p (TREE_TYPE (decl))
      || (processing_template_decl
	  && VAR_P (decl)
	  && CP_DECL_THREAD_LOCAL_P (decl)))
    return;
  /* EH specs were not part of the function type prior to c++17, but
     we still can't go pushing dependent eh specs into the namespace.  */
  if (cxx_dialect < cxx17
      && TREE_CODE (decl) == FUNCTION_DECL
      && (value_dependent_expression_p
	  (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (decl)))))
    return;

  gcc_checking_assert (!DECL_LANG_SPECIFIC (decl)
		       || !DECL_TEMPLATE_INFO (decl));
  if (DECL_LANG_SPECIFIC (decl) && DECL_LOCAL_DECL_ALIAS (decl))
    /* We're instantiating a non-dependent local decl, it already
       knows the alias.  */
    return;

  tree alias = NULL_TREE;

  if (DECL_SIZE (decl) && !TREE_CONSTANT (DECL_SIZE (decl)))
    /* Do not let a VLA creep into a namespace.  Diagnostic will be
       emitted in layout_var_decl later.  */
    alias = error_mark_node;
  else
    {
      /* First look for a decl that matches.  */
      tree ns = CP_DECL_CONTEXT (decl);
      tree binding = find_namespace_value (ns, DECL_NAME (decl));

      if (binding && TREE_CODE (binding) != TREE_LIST)
	for (ovl_iterator iter (binding); iter; ++iter)
	  if (decls_match (decl, *iter, /*record_versions*/false))
	    {
	      alias = *iter;
	      if (!validate_constexpr_redeclaration (alias, decl))
		return;
	      break;
	    }

      if (!alias)
	{
	  /* No existing namespace-scope decl.  Make one.  */
	  alias = copy_decl (decl);
	  if (TREE_CODE (alias) == FUNCTION_DECL)
	    {
	      /* Recontextualize the parms.  */
	      for (tree *chain = &DECL_ARGUMENTS (alias);
		   *chain; chain = &DECL_CHAIN (*chain))
		{
		  *chain = copy_decl (*chain);
		  DECL_CONTEXT (*chain) = alias;
		}

	      tree type = TREE_TYPE (alias);
	      for (tree args = TYPE_ARG_TYPES (type);
		   args; args = TREE_CHAIN (args))
		if (TREE_PURPOSE (args))
		  {
		    /* There are default args.  Lose them.  */
		    tree nargs = NULL_TREE;
		    tree *chain = &nargs;
		    for (args = TYPE_ARG_TYPES (type);
			 args; args = TREE_CHAIN (args))
		      if (args == void_list_node)
			{
			  *chain = args;
			  break;
			}
		      else
			{
			  *chain
			    = build_tree_list (NULL_TREE, TREE_VALUE (args));
			  chain = &TREE_CHAIN (*chain);
			}

		    tree fn_type = build_function_type (TREE_TYPE (type), nargs);

		    fn_type = apply_memfn_quals
		      (fn_type, type_memfn_quals (type));

		    fn_type = build_cp_fntype_variant
		      (fn_type, type_memfn_rqual (type),
		       TYPE_RAISES_EXCEPTIONS (type),
		       TYPE_HAS_LATE_RETURN_TYPE (type));

		    TREE_TYPE (alias) = fn_type;
		    break;
		  }
	    }

	  /* This is the real thing.  */
	  DECL_LOCAL_DECL_P (alias) = false;

	  /* Expected default linkage is from the namespace.  */
	  TREE_PUBLIC (alias) = TREE_PUBLIC (ns);
	  push_nested_namespace (ns);
	  alias = pushdecl (alias, /* hiding= */true);
	  pop_nested_namespace (ns);
	  if (VAR_P (decl)
	      && CP_DECL_THREAD_LOCAL_P (decl)
	      && alias != error_mark_node)
	    set_decl_tls_model (alias, DECL_TLS_MODEL (decl));

	  /* Adjust visibility.  */
	  determine_visibility (alias);
	}
    }

  retrofit_lang_decl (decl);
  DECL_LOCAL_DECL_ALIAS (decl) = alias;
}

/* If DECL has non-internal linkage, and we have a module vector,
   record it in the appropriate slot.  We have already checked for
   duplicates.  */

static void
maybe_record_mergeable_decl (tree *slot, tree name, tree decl)
{
  if (TREE_CODE (*slot) != BINDING_VECTOR)
    return;

  if (decl_linkage (decl) == lk_internal)
    return;

  tree not_tmpl = STRIP_TEMPLATE (decl);
  bool is_attached = (DECL_LANG_SPECIFIC (not_tmpl)
		      && DECL_MODULE_ATTACH_P (not_tmpl));
  tree *gslot = get_fixed_binding_slot
    (slot, name, is_attached ? BINDING_SLOT_PARTITION : BINDING_SLOT_GLOBAL,
     true);

  if (!is_attached)
    {
      binding_slot &orig
	= BINDING_VECTOR_CLUSTER (*slot, 0).slots[BINDING_SLOT_CURRENT];

      if (!STAT_HACK_P (tree (orig)))
	orig = stat_hack (tree (orig));

      MODULE_BINDING_GLOBAL_P (tree (orig)) = true;
    }

  add_mergeable_namespace_entity (gslot, decl);
}

/* DECL is being pushed.  Check whether it hides or ambiguates
   something seen as an import.  This include decls seen in our own
   interface, which is OK.  Also, check for merging a
   global/partition decl.  */

static tree
check_module_override (tree decl, tree mvec, bool hiding,
		       tree scope, tree name)
{
  tree match = NULL_TREE;
  bitmap imports = get_import_bitmap ();
  binding_cluster *cluster = BINDING_VECTOR_CLUSTER_BASE (mvec);
  unsigned ix = BINDING_VECTOR_NUM_CLUSTERS (mvec);

  tree nontmpl = STRIP_TEMPLATE (decl);
  bool attached = DECL_LANG_SPECIFIC (nontmpl) && DECL_MODULE_ATTACH_P (nontmpl);

  /* For deduction guides we don't do normal name lookup, but rather consider
     any reachable declaration, so we should check for overriding here too.  */
  bool any_reachable = deduction_guide_p (decl);

  if (BINDING_VECTOR_SLOTS_PER_CLUSTER == BINDING_SLOTS_FIXED)
    {
      cluster++;
      ix--;
    }

  for (; ix--; cluster++)
    for (unsigned jx = 0; jx != BINDING_VECTOR_SLOTS_PER_CLUSTER; jx++)
      {
	/* Are we importing this module?  */
	if (cluster->indices[jx].span != 1)
	  continue;
	if (!cluster->indices[jx].base)
	  continue;
	if (!any_reachable
	    && !bitmap_bit_p (imports, cluster->indices[jx].base))
	  continue;
	/* Is it loaded? */
	if (cluster->slots[jx].is_lazy ())
	  {
	    gcc_assert (cluster->indices[jx].span == 1);
	    lazy_load_binding (cluster->indices[jx].base,
			       scope, name, &cluster->slots[jx]);
	  }
	tree bind = cluster->slots[jx];
	if (!bind)
	  /* Errors could cause there to be nothing.  */
	  continue;

	tree type = NULL_TREE;
	if (STAT_HACK_P (bind))
	  {
	    /* If there was a matching STAT_TYPE here then xref_tag
	       should have found it, but we need to check anyway because
	       a conflicting using-declaration may exist.  */
	    if (any_reachable)
	      {
		type = STAT_TYPE (bind);
		bind = STAT_DECL (bind);
	      }
	    else
	      {
		if (STAT_TYPE_VISIBLE_P (bind))
		  type = STAT_TYPE (bind);
		bind = STAT_VISIBLE (bind);
	      }
	  }

	if (type)
	  {
	    match = duplicate_decls (decl, strip_using_decl (type), hiding);
	    if (match)
	      goto matched;
	  }

	for (ovl_iterator iter (strip_using_decl (bind)); iter; ++iter)
	  {
	    match = duplicate_decls (decl, *iter, hiding);
	    if (match)
	      goto matched;
	  }
      }

  if (TREE_PUBLIC (scope) && TREE_PUBLIC (STRIP_TEMPLATE (decl))
      /* Namespaces are dealt with specially in
	 make_namespace_finish.  */
      && !(TREE_CODE (decl) == NAMESPACE_DECL && !DECL_NAMESPACE_ALIAS (decl)))
    {
      /* Look in the appropriate mergeable decl slot.  */
      tree mergeable = NULL_TREE;
      if (attached)
	mergeable = BINDING_VECTOR_CLUSTER (mvec, BINDING_SLOT_PARTITION
					   / BINDING_VECTOR_SLOTS_PER_CLUSTER)
	  .slots[BINDING_SLOT_PARTITION % BINDING_VECTOR_SLOTS_PER_CLUSTER];
      else
	mergeable = BINDING_VECTOR_CLUSTER (mvec, 0).slots[BINDING_SLOT_GLOBAL];

      for (ovl_iterator iter (mergeable); iter; ++iter)
	{
	  match = duplicate_decls (decl, *iter, hiding);
	  if (match)
	    goto matched;
	}
    }

  return NULL_TREE;

 matched:
  if (match != error_mark_node)
    {
      if (attached)
	BINDING_VECTOR_PARTITION_DUPS_P (mvec) = true;
      else
	BINDING_VECTOR_GLOBAL_DUPS_P (mvec) = true;
    }

  return match;
}

/* Record DECL as belonging to the current lexical scope.  Check for
   errors (such as an incompatible declaration for the same name
   already seen in the same scope).

   The new binding is hidden if HIDING is true (an anticipated builtin
   or hidden friend).

   Returns either DECL or an old decl for the same name.  If an old
   decl is returned, it may have been smashed to agree with what DECL
   says.  */

tree
pushdecl (tree decl, bool hiding)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);

  if (decl == error_mark_node)
    return error_mark_node;

  if (!DECL_TEMPLATE_PARM_P (decl) && current_function_decl && !hiding)
    set_decl_context_in_fn (current_function_decl, decl);

  /* The binding level we will be pushing into.  During local class
     pushing, we want to push to the containing scope.  */
  cp_binding_level *level = current_binding_level;
  while (level->kind == sk_class
	 || level->kind == sk_cleanup)
    level = level->level_chain;

  /* An anonymous namespace has a NULL DECL_NAME, but we still want to
     insert it.  Other NULL-named decls, not so much.  */
  tree name = DECL_NAME (decl);
  if (name ? !IDENTIFIER_ANON_P (name) : TREE_CODE (decl) == NAMESPACE_DECL)
    {
      cxx_binding *binding = NULL; /* Local scope binding.  */
      tree ns = NULL_TREE; /* Searched namespace.  */
      tree *slot = NULL; /* Binding slot in namespace.  */
      tree *mslot = NULL; /* Current module slot in namespace.  */
      tree old = NULL_TREE;
      bool name_independent_p = false;
      bool name_independent_diagnosed_p = false;

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
	    {
	      mslot = get_fixed_binding_slot (slot, name, BINDING_SLOT_CURRENT,
					      ns == current_namespace);
	      old = MAYBE_STAT_DECL (*mslot);
	    }
	}
      else
	{
	  binding = find_local_binding (level, name);
	  if (binding)
	    old = binding->value;
	  name_independent_p = name_independent_decl_p (decl);
	}

      if (old == error_mark_node)
	old = NULL_TREE;

      tree oldi, oldn;
      for (oldi = old; oldi; oldi = oldn)
	{
	  if (TREE_CODE (oldi) == TREE_LIST)
	    {
	      gcc_checking_assert (level->kind != sk_namespace
				   && name_independent_decl_p
							(TREE_VALUE (old)));
	      oldn = TREE_CHAIN (oldi);
	      oldi = TREE_VALUE (oldi);
	    }
	  else
	    oldn = NULL_TREE;
	  for (ovl_iterator iter (oldi); iter; ++iter)
	    if (iter.using_p ())
	      ; /* Ignore using decls here.  */
	    else if (iter.hidden_p ()
		     && TREE_CODE (*iter) == FUNCTION_DECL
		     && DECL_LANG_SPECIFIC (*iter)
		     && DECL_MODULE_IMPORT_P (*iter))
	      ; /* An undeclared builtin imported from elsewhere.  */
	    else if (name_independent_p)
	      {
		/* Ignore name-independent declarations.  */
		if (cxx_dialect < cxx26 && !name_independent_diagnosed_p)
		  pedwarn (DECL_SOURCE_LOCATION (decl), OPT_Wc__26_extensions,
			   "name-independent declarations only available with "
			   "%<-std=c++2c%> or %<-std=gnu++2c%>");
		name_independent_diagnosed_p = true;
	      }
	    else if (tree match
		     = duplicate_decls (decl, *iter, hiding, iter.hidden_p ()))
	      {
		if (match == error_mark_node)
		  ;
		else if (TREE_CODE (match) == TYPE_DECL)
		  gcc_checking_assert (REAL_IDENTIFIER_TYPE_VALUE (name)
				       == (level->kind == sk_namespace
					   ? NULL_TREE : TREE_TYPE (match)));
		else if (iter.hidden_p () && !hiding)
		  {
		    /* Unhiding a previously hidden decl.  */
		    tree head = iter.reveal_node (oldi);
		    if (head != oldi)
		      {
			gcc_checking_assert (ns);
			if (STAT_HACK_P (*slot))
			  STAT_DECL (*slot) = head;
			else
			  *slot = head;
		      }
		    if (DECL_EXTERN_C_P (match))
		      /* We need to check and register the decl now.  */
		      check_extern_c_conflict (match);
		  }
		else if (slot
			 && !hiding
			 && STAT_HACK_P (*slot)
			 && STAT_DECL_HIDDEN_P (*slot))
		  {
		    /* Unhide the non-function.  */
		    gcc_checking_assert (oldi == match);
		    if (!STAT_TYPE (*slot))
		      *slot = match;
		    else
		      STAT_DECL (*slot) = match;
		  }
		return match;
	      }
	}

      /* Check for redeclaring an import.  */
      if (slot && *slot && TREE_CODE (*slot) == BINDING_VECTOR)
	if (tree match
	    = check_module_override (decl, *slot, hiding, ns, name))
	  {
	    if (match == error_mark_node)
	      return match;

	    /* We found a decl in an interface, push it into this
	       binding.  */
	    decl = update_binding (NULL, binding, mslot, old,
				   match, hiding);

	    return decl;
	  }

      /* We are pushing a new decl.  */

      /* Skip a hidden builtin we failed to match already.  There can
	 only be one.  */
      if (old && anticipated_builtin_p (old))
	old = OVL_CHAIN (old);

      if (hiding)
	; /* Hidden bindings don't shadow anything.  */
      else
	check_template_shadow (decl);

      if (DECL_DECLARES_FUNCTION_P (decl))
	{
	  check_default_args (decl);

	  if (hiding)
	    {
	      if (level->kind != sk_namespace)
		{
		  /* In a local class, a friend function declaration must
		     find a matching decl in the innermost non-class scope.
		     [class.friend/11] */
		  error_at (DECL_SOURCE_LOCATION (decl),
			    "friend declaration %qD in local class without "
			    "prior local declaration", decl);
		  /* Don't attempt to push it.  */
		  return error_mark_node;
		}
	    }
	}

      if (level->kind != sk_namespace)
	{
	  tree local_shadow = check_local_shadow (decl);
	  if (name_independent_p && local_shadow)
	    {
	      if (cxx_dialect < cxx26 && !name_independent_diagnosed_p)
		pedwarn (DECL_SOURCE_LOCATION (decl), OPT_Wc__26_extensions,
			 "name-independent declarations only available with "
			 "%<-std=c++2c%> or %<-std=gnu++2c%>");
	      name_independent_diagnosed_p = true;
	      /* When a name-independent declaration is pushed into a scope
		 which itself does not contain a _ named declaration yet (so
		 _ name lookups wouldn't be normally ambiguous), but it
		 shadows a _ declaration in some outer scope in cases
		 described in [basic.scope.block]/2 where if the names of
		 the shadowed and shadowing declarations were different it
		 would be ill-formed program, arrange for _ name lookups
		 in this scope to be ambiguous.  */
	      if (old == NULL_TREE)
		{
		  old = build_tree_list (error_mark_node, local_shadow);
		  TREE_TYPE (old) = error_mark_node;
		}
	    }

	  if (TREE_CODE (decl) == NAMESPACE_DECL)
	    /* A local namespace alias.  */
	    set_identifier_type_value_with_scope (name, NULL_TREE, level);

	  if (!binding)
	    binding = create_local_binding (level, name);
	}
      else if (!slot)
	{
	  ns = current_namespace;
	  slot = find_namespace_slot (ns, name, true);
	  mslot = get_fixed_binding_slot (slot, name, BINDING_SLOT_CURRENT, true);
	  /* Update OLD to reflect the namespace we're going to be
	     pushing into.  */
	  old = MAYBE_STAT_DECL (*mslot);
	}

      old = update_binding (level, binding, mslot, old, decl, hiding);

      if (old != decl)
	/* An existing decl matched, use it.  */
	decl = old;
      else
	{
	  if (TREE_CODE (decl) == TYPE_DECL)
	    {
	      tree type = TREE_TYPE (decl);

	      if (type != error_mark_node)
		{
		  if (TYPE_NAME (type) != decl)
		    set_underlying_type (decl);

		  set_identifier_type_value_with_scope (name, decl, level);

		  if (level->kind != sk_namespace
		      && !instantiating_current_function_p ())
		    /* This is a locally defined typedef in a function that
		       is not a template instantation, record it to implement
		       -Wunused-local-typedefs.  */
		    record_locally_defined_typedef (decl);
		}
	    }
	  else if (VAR_OR_FUNCTION_DECL_P (decl))
	    {
	      if (DECL_EXTERN_C_P (decl))
		check_extern_c_conflict (decl);

	      if (!DECL_LOCAL_DECL_P (decl)
		  && VAR_P (decl))
		maybe_register_incomplete_var (decl);

	      if (DECL_LOCAL_DECL_P (decl)
		  && NAMESPACE_SCOPE_P (decl))
		push_local_extern_decl_alias (decl);
	    }

	  if (level->kind == sk_namespace
	      && TREE_PUBLIC (level->this_entity)
	      && module_maybe_has_cmi_p ())
	    maybe_record_mergeable_decl (slot, name, decl);
	}
    }
  else
    add_decl_to_level (level, decl);

  return decl;
}

/* A mergeable entity is being loaded into namespace NS slot NAME.
   Create and return the appropriate vector slot for that.  Either a
   GMF slot or a module-specific one.  */

tree *
mergeable_namespace_slots (tree ns, tree name, bool is_attached, tree *vec)
{
  tree *mslot = find_namespace_slot (ns, name, true);
  tree *vslot = get_fixed_binding_slot
    (mslot, name, is_attached ? BINDING_SLOT_PARTITION : BINDING_SLOT_GLOBAL,
     true);

  gcc_checking_assert (TREE_CODE (*mslot) == BINDING_VECTOR);
  *vec = *mslot;

  return vslot;
}

/* Retrieve the bindings for an existing mergeable entity in namespace
   NS slot NAME.  Returns NULL if no such bindings exists.  */

static tree
get_mergeable_namespace_binding (tree ns, tree name, bool is_attached)
{
  tree *mslot = find_namespace_slot (ns, name, false);
  if (!mslot || !*mslot || TREE_CODE (*mslot) != BINDING_VECTOR)
    return NULL_TREE;

  tree *vslot = get_fixed_binding_slot
    (mslot, name, is_attached ? BINDING_SLOT_PARTITION : BINDING_SLOT_GLOBAL,
     false);
  return vslot ? *vslot : NULL_TREE;
}

/* DECL is a new mergeable namespace-scope decl.  Add it to the
   mergeable entities on GSLOT.  */

void
add_mergeable_namespace_entity (tree *gslot, tree decl)
{
  *gslot = ovl_make (decl, *gslot);
}

/* A mergeable entity of KLASS called NAME is being loaded.  Return
   the set of things it could be.  All such non-as_base classes have
   been given a member vec.  */

tree
lookup_class_binding (tree klass, tree name)
{
  tree found = NULL_TREE;

  if (!COMPLETE_TYPE_P (klass))
    ;
  else if (TYPE_LANG_SPECIFIC (klass))
    {
      vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (klass);

      found = member_vec_binary_search (member_vec, name);
      if (!found)
	;
      else if (STAT_HACK_P (found))
	/* Rearrange the stat hack so that we don't need to expose that
	   internal detail.  */
	found = ovl_make (STAT_TYPE (found), STAT_DECL (found));
      else if (IDENTIFIER_CONV_OP_P (name))
	{
	  gcc_checking_assert (name == conv_op_identifier);
	  found = OVL_CHAIN (found);
	}
    }
  else
    {
      gcc_checking_assert (IS_FAKE_BASE_TYPE (klass)
			   || TYPE_PTRMEMFUNC_P (klass));
      found = fields_linear_search (klass, name, false);
    }

  return found;
}

/* Whether this using is declared in the module purview.  */

bool
ovl_iterator::purview_p () const
{
  gcc_checking_assert (using_p ());
  if (TREE_CODE (ovl) == USING_DECL)
    return DECL_MODULE_PURVIEW_P (ovl);
  return OVL_PURVIEW_P (ovl);
}

/* Whether this using is exported from this module.  */

bool
ovl_iterator::exporting_p () const
{
  gcc_checking_assert (using_p ());
  if (TREE_CODE (ovl) == USING_DECL)
    return DECL_MODULE_EXPORT_P (ovl);
  return OVL_EXPORT_P (ovl);
}

/* Given a namespace-level binding BINDING, walk it, calling CALLBACK
   for all decls of the current module.  When partitions are involved,
   decls might be mentioned more than once.   Return the accumulation of
   CALLBACK results.  */

unsigned
walk_module_binding (tree binding, bitmap partitions,
		     bool (*callback) (tree decl, WMB_Flags, void *data),
		     void *data)
{
  tree current = binding;
  unsigned count = 0;

  if (TREE_CODE (binding) == BINDING_VECTOR)
    current = BINDING_VECTOR_CLUSTER (binding, 0).slots[BINDING_SLOT_CURRENT];

  bool decl_hidden = false;
  if (tree type = MAYBE_STAT_TYPE (current))
    {
      WMB_Flags flags = WMB_None;
      if (STAT_TYPE_HIDDEN_P (current))
	flags = WMB_Flags (flags | WMB_Hidden);
      if (TREE_CODE (type) == USING_DECL)
	{
	  flags = WMB_Flags (flags | WMB_Using);
	  if (DECL_MODULE_PURVIEW_P (type))
	    flags = WMB_Flags (flags | WMB_Purview);
	  if (DECL_MODULE_EXPORT_P (type))
	    flags = WMB_Flags (flags | WMB_Export);
	}
      count += callback (type, flags, data);
      decl_hidden = STAT_DECL_HIDDEN_P (current);
    }

  for (ovl_iterator iter (MAYBE_STAT_DECL (current)); iter; ++iter)
    {
      if (iter.hidden_p ())
	decl_hidden = true;
      if (!(decl_hidden && DECL_IS_UNDECLARED_BUILTIN (*iter)))
	{
	  WMB_Flags flags = WMB_None;
	  if (decl_hidden)
	    flags = WMB_Flags (flags | WMB_Hidden);
	  if (iter.using_p ())
	    {
	      flags = WMB_Flags (flags | WMB_Using);
	      if (iter.purview_p ())
		flags = WMB_Flags (flags | WMB_Purview);
	      if (iter.exporting_p ())
		flags = WMB_Flags (flags | WMB_Export);
	    }
	  count += callback (*iter, flags, data);
	}
      decl_hidden = false;
    }

  if (partitions && TREE_CODE (binding) == BINDING_VECTOR)
    {
      /* Process partition slots.  */
      binding_cluster *cluster = BINDING_VECTOR_CLUSTER_BASE (binding);
      unsigned ix = BINDING_VECTOR_NUM_CLUSTERS (binding);
      if (BINDING_VECTOR_SLOTS_PER_CLUSTER == BINDING_SLOTS_FIXED)
	{
	  ix--;
	  cluster++;
	}

      /* There could be duplicate module or GMF entries.  */
      bool maybe_dups = (BINDING_VECTOR_PARTITION_DUPS_P (binding)
			 || BINDING_VECTOR_GLOBAL_DUPS_P (binding));

      for (; ix--; cluster++)
	for (unsigned jx = 0; jx != BINDING_VECTOR_SLOTS_PER_CLUSTER; jx++)
	  if (!cluster->slots[jx].is_lazy ())
	    if (tree bind = cluster->slots[jx])
	      {
		if (TREE_CODE (bind) == NAMESPACE_DECL
		    && !DECL_NAMESPACE_ALIAS (bind))
		  {
		    if (unsigned base = cluster->indices[jx].base)
		      if (unsigned span = cluster->indices[jx].span)
			do
			  if (bitmap_bit_p (partitions, base))
			    goto found;
			while (++base, --span);
		    /* Not a partition's namespace.  */
		    continue;
		  found:

		    WMB_Flags flags = WMB_None;
		    if (maybe_dups)
		      flags = WMB_Flags (flags | WMB_Dups);
		    count += callback (bind, flags, data);
		  }
		else if (STAT_HACK_P (bind) && MODULE_BINDING_PARTITION_P (bind))
		  {
		    if (tree btype = STAT_TYPE (bind))
		      {
			WMB_Flags flags = WMB_None;
			if (maybe_dups)
			  flags = WMB_Flags (flags | WMB_Dups);
			if (STAT_TYPE_HIDDEN_P (bind))
			  flags = WMB_Flags (flags | WMB_Hidden);
			if (TREE_CODE (btype) == USING_DECL)
			  {
			    flags = WMB_Flags (flags | WMB_Using);
			    if (DECL_MODULE_PURVIEW_P (btype))
			      flags = WMB_Flags (flags | WMB_Purview);
			    if (DECL_MODULE_EXPORT_P (btype))
			      flags = WMB_Flags (flags | WMB_Export);
			  }
			count += callback (btype, flags, data);
		      }
		    bool part_hidden = STAT_DECL_HIDDEN_P (bind);
		    for (ovl_iterator iter (MAYBE_STAT_DECL (STAT_DECL (bind)));
			 iter; ++iter)
		      {
			if (iter.hidden_p ())
			  part_hidden = true;
			gcc_checking_assert
			  (!(part_hidden && DECL_IS_UNDECLARED_BUILTIN (*iter)));

			WMB_Flags flags = WMB_None;
			if (maybe_dups)
			  flags = WMB_Flags (flags | WMB_Dups);
			if (part_hidden)
			  flags = WMB_Flags (flags | WMB_Hidden);
			if (iter.using_p ())
			  {
			    flags = WMB_Flags (flags | WMB_Using);
			    if (iter.purview_p ())
			      flags = WMB_Flags (flags | WMB_Purview);
			    if (iter.exporting_p ())
			      flags = WMB_Flags (flags | WMB_Export);
			  }
			count += callback (*iter, flags, data);
			part_hidden = false;
		      }
		  }
	      }
    }

  return count;
}

/* Imported module MOD has a binding to NS::NAME, stored in section
   SNUM.  */

bool
import_module_binding  (tree ns, tree name, unsigned mod, unsigned snum)
{
  tree *slot = find_namespace_slot (ns, name, true);
  binding_slot *mslot = append_imported_binding_slot (slot, name, mod);

  if (mslot->is_lazy () || *mslot)
    /* Oops, something was already there.  */
    return false;

  mslot->set_lazy (snum);
  return true;
}

/* An import of MODULE is binding NS::NAME.  There should be no
   existing binding for >= MODULE.  GLOBAL_P indicates whether the
   bindings include global module entities.  PARTITION_P is true if
   it is part of the current module. VALUE and TYPE are the value
   and type bindings. VISIBLE are the value bindings being exported.  */

bool
set_module_binding (tree ns, tree name, unsigned mod, bool global_p,
		    bool partition_p, tree value, tree type, tree visible)
{
  if (!value)
    /* Bogus BMIs could give rise to nothing to bind.  */
    return false;

  gcc_assert (TREE_CODE (value) != NAMESPACE_DECL
	      || DECL_NAMESPACE_ALIAS (value));
  gcc_checking_assert (mod);

  tree *slot = find_namespace_slot (ns, name, true);
  binding_slot *mslot = search_imported_binding_slot (slot, mod);

  if (!mslot || !mslot->is_lazy ())
    /* Again, bogus BMI could give find to missing or already loaded slot.  */
    return false;

  tree bind = value;
  if (type || visible != bind || partition_p || global_p)
    {
      bind = stat_hack (bind, type);
      STAT_VISIBLE (bind) = visible;
      if ((partition_p && TREE_PUBLIC (ns))
	  || (type && DECL_MODULE_EXPORT_P (type)))
	STAT_TYPE_VISIBLE_P (bind) = true;
    }

  /* Note if this is this-module and/or global binding.  */
  if (partition_p)
    MODULE_BINDING_PARTITION_P (bind) = true;
  if (global_p)
    MODULE_BINDING_GLOBAL_P (bind) = true;

  *mslot = bind;

  return true;
}

void
add_module_namespace_decl (tree ns, tree decl)
{
  gcc_assert (!DECL_CHAIN (decl));
  gcc_checking_assert (!(VAR_OR_FUNCTION_DECL_P (decl)
			 && DECL_LOCAL_DECL_P (decl)));
  if (CHECKING_P)
    /* Expensive already-there? check.  */
    for (auto probe = NAMESPACE_LEVEL (ns)->names; probe;
	 probe = DECL_CHAIN (probe))
      gcc_assert (decl != probe);

  add_decl_to_level (NAMESPACE_LEVEL (ns), decl);

  if (VAR_P (decl))
    maybe_register_incomplete_var (decl);

  if (VAR_OR_FUNCTION_DECL_P (decl)
      && DECL_EXTERN_C_P (decl))
    check_extern_c_conflict (decl);
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
    decl = build_tree_list (id, decl);

  /* And put DECL on the list of things declared by the current
     binding level.  */
  add_decl_to_level (b, decl);
}

/* Lookup the FRIEND_TMPL within all merged module imports.  Used to dedup
   instantiations of temploid hidden friends from imported modules.  */

tree
lookup_imported_hidden_friend (tree friend_tmpl)
{
  /* For a class-scope friend class it should have been found by regular
     name lookup.  Otherwise we're looking in the current namespace.  */
  gcc_checking_assert (CP_DECL_CONTEXT (friend_tmpl) == current_namespace);

  tree inner = DECL_TEMPLATE_RESULT (friend_tmpl);
  if (!DECL_LANG_SPECIFIC (inner)
      || !DECL_MODULE_IMPORT_P (inner))
    return NULL_TREE;

  lazy_load_pendings (friend_tmpl);

  tree bind = get_mergeable_namespace_binding
    (current_namespace, DECL_NAME (inner), DECL_MODULE_ATTACH_P (inner));
  if (!bind)
    return NULL_TREE;

  /* We're only interested in declarations coming from the same module
     of the friend class we're attempting to instantiate.  */
  int m = get_originating_module (friend_tmpl);
  gcc_assert (m != 0);

  /* There should be at most one class template from the module we're
     looking for, return it.  */
  for (ovl_iterator iter (bind); iter; ++iter)
    if (DECL_CLASS_TEMPLATE_P (*iter)
	&& get_originating_module (*iter) == m)
      return *iter;

  return NULL_TREE;
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
    "cond-init-scope",
    "stmt-expr-scope",
    "function-parameter-scope",
    "class-scope",
    "enum-scope",
    "namespace-scope",
    "template-parameter-scope",
    "template-explicit-spec-scope",
    "transaction-scope",
    "openmp-scope"
  };
  static_assert (ARRAY_SIZE (scope_kind_names) == sk_count,
		 "must keep names aligned with scope_kind enum");

  scope_kind kind = scope->kind;
  if (kind == sk_template_parms && scope->explicit_spec_p)
    kind = sk_template_spec;

  return scope_kind_names[kind];
}

/* Output a debugging information about SCOPE when performing
   ACTION at LINE.  */
static void
cp_binding_level_debug (cp_binding_level *scope, int line, const char *action)
{
  const char *desc = cp_binding_level_descriptor (scope);
  if (scope->this_entity)
    verbatim ("%s %<%s(%E)%> %p %d", action, desc,
	      scope->this_entity, (void *) scope, line);
  else
    verbatim ("%s %s %p %d", action, desc, (void *) scope, line);
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
    case sk_transaction:
    case sk_omp:
    case sk_stmt_expr:
      scope->keep = keep_next_level_flag;
      break;

    case sk_function_parms:
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
      && scope != previous_class_level)
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

/* When we exit a toplevel class scope, we save its binding level so
   that we can restore it quickly.  Here, we've entered some other
   class, so we must invalidate our cache.  */

void
invalidate_class_lookup_cache (void)
{
  previous_class_level->level_chain = free_binding_level;
  free_binding_level = previous_class_level;
  previous_class_level = NULL;
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
  if (lvl->this_entity)
    print_node_brief (stderr, "entity=", lvl->this_entity, 1);
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
	      && DECL_IS_UNDECLARED_BUILTIN (t))
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

static void
print_other_binding_stack (cp_binding_level *stack)
{
  cp_binding_level *level;
  for (level = stack; !global_scope_p (level); level = level->level_chain)
    {
      fprintf (stderr, "binding level %p\n", (void *) level);
      print_binding_level (level);
    }
}

DEBUG_FUNCTION void
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

/* Push a definition of struct, union or enum tag named ID.  into
   binding_level B.  DECL is a TYPE_DECL for the type.  DECL has
   already been pushed into its binding level.  This is bookkeeping to
   find it easily.  */

static void
set_identifier_type_value_with_scope (tree id, tree decl, cp_binding_level *b)
{
  if (b->kind == sk_namespace)
    /* At namespace scope we should not see an identifier type value.  */
    gcc_checking_assert (!REAL_IDENTIFIER_TYPE_VALUE (id)
			 /* We could be pushing a friend underneath a template
			    parm (ill-formed).  */
			 || (TEMPLATE_PARM_P
			     (TYPE_NAME (REAL_IDENTIFIER_TYPE_VALUE (id)))));
  else
    {
      /* Push the current type value, so we can restore it later  */
      tree old = REAL_IDENTIFIER_TYPE_VALUE (id);
      b->type_shadowed = tree_cons (id, old, b->type_shadowed);
      tree type = decl ? TREE_TYPE (decl) : NULL_TREE;
      TREE_TYPE (b->type_shadowed) = type;
      SET_IDENTIFIER_TYPE_VALUE (id, type);
    }
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

/* Same as pushdecl, but define X in binding-level LEVEL.  We rely on the
   caller to set DECL_CONTEXT properly.

   Warning: For class and block-scope this must only be used when X
   will be the new innermost binding for its name, as we tack it onto
   the front of IDENTIFIER_BINDING without checking to see if the
   current IDENTIFIER_BINDING comes from a closer binding level than
   LEVEL.

   Warning: For namespace scope, this will look in LEVEL for an
   existing binding to match, but if not found will push the decl into
   CURRENT_NAMESPACE.  Use push_nested_namespace/pushdecl/
   pop_nested_namespace if you really need to push it into a foreign
   namespace.  */

static tree
do_pushdecl_with_scope (tree x, cp_binding_level *level, bool hiding = false)
{
  cp_binding_level *b;

  if (level->kind == sk_class)
    {
      gcc_checking_assert (!hiding);
      b = class_binding_level;
      class_binding_level = level;
      pushdecl_class_level (x);
      class_binding_level = b;
    }
  else
    {
      tree function_decl = current_function_decl;
      if (level->kind == sk_namespace)
	current_function_decl = NULL_TREE;
      b = current_binding_level;
      current_binding_level = level;
      x = pushdecl (x, hiding);
      current_binding_level = b;
      current_function_decl = function_decl;
    }
  return x;
}

/* Inject X into the local scope just before the function parms.  */

tree
pushdecl_outermost_localscope (tree x)
{
  cp_binding_level *b = NULL;
  auto_cond_timevar tv (TV_NAME_LOOKUP);

  /* Find the scope just inside the function parms.  */
  for (cp_binding_level *n = current_binding_level;
       n->kind != sk_function_parms; n = b->level_chain)
    b = n;

  return b ? do_pushdecl_with_scope (x, b) : error_mark_node;
}

/* Checks if BINDING is a binding that we can export.  */

static bool
check_can_export_using_decl (tree binding)
{
  /* Declarations in header units are always OK.  */
  if (header_module_p ())
    return true;

  /* We want the linkage of the underlying entity, so strip typedefs.
     If the underlying entity is a builtin type then we're OK.  */
  tree entity = binding;
  if (TREE_CODE (entity) == TYPE_DECL)
    {
      entity = TYPE_MAIN_DECL (TREE_TYPE (entity));
      if (!entity)
	return true;
    }

  linkage_kind linkage = decl_linkage (entity);
  tree not_tmpl = STRIP_TEMPLATE (entity);

  /* Attachment is determined by the owner of an enumerator.  */
  if (TREE_CODE (not_tmpl) == CONST_DECL)
    not_tmpl = TYPE_NAME (DECL_CONTEXT (not_tmpl));

  /* If the using decl is exported, the things it refers to must
     have external linkage.  decl_linkage returns lk_external for
     module linkage so also check for attachment.  */
  if (linkage != lk_external
      || (DECL_LANG_SPECIFIC (not_tmpl)
	  && DECL_MODULE_ATTACH_P (not_tmpl)
	  && !DECL_MODULE_EXPORT_P (not_tmpl)))
    {
      auto_diagnostic_group d;
      error ("exporting %q#D that does not have external linkage",
	     binding);
      if (linkage == lk_none)
	inform (DECL_SOURCE_LOCATION (entity),
		"%q#D declared here with no linkage", entity);
      else if (linkage == lk_internal)
	inform (DECL_SOURCE_LOCATION (entity),
		"%q#D declared here with internal linkage", entity);
      else
	inform (DECL_SOURCE_LOCATION (entity),
		"%q#D declared here with module linkage", entity);
      return false;
    }

  return true;
}

/* Process a local-scope or namespace-scope using declaration.  LOOKUP
   is the result of qualified lookup (both value & type are
   significant).  FN_SCOPE_P indicates if we're at function-scope (as
   opposed to namespace-scope).  *VALUE_P and *TYPE_P are the current
   bindings, which are altered to reflect the newly brought in
   declarations.  */

static bool
do_nonmember_using_decl (name_lookup &lookup, bool fn_scope_p,
			 bool insert_p, tree *value_p, tree *type_p)
{
  tree value = *value_p;
  tree type = *type_p;
  bool failed = false;

  /* Shift the old and new bindings around so we're comparing class and
     enumeration names to each other.  */
  if (value && DECL_IMPLICIT_TYPEDEF_P (strip_using_decl (value)))
    {
      type = value;
      value = NULL_TREE;
    }

  if (lookup.value && DECL_IMPLICIT_TYPEDEF_P (lookup.value))
    {
      lookup.type = lookup.value;
      lookup.value = NULL_TREE;
    }

  /* Only process exporting if we're going to be inserting.  */
  bool revealing_p = insert_p && !fn_scope_p && module_has_cmi_p ();

  /* First do the value binding.  */
  if (!lookup.value)
    /* Nothing (only implicit typedef found).  */
    gcc_checking_assert (lookup.type);
  else if (OVL_P (lookup.value) && (!value || OVL_P (value)))
    {
      for (lkp_iterator usings (lookup.value); usings; ++usings)
	{
	  tree new_fn = *usings;
	  tree inner = STRIP_TEMPLATE (new_fn);
	  bool exporting_p = revealing_p && module_exporting_p ();
	  if (exporting_p)
	    exporting_p = check_can_export_using_decl (new_fn);

	  /* [namespace.udecl]

	     If a function declaration in namespace scope or block
	     scope has the same name and the same parameter types as a
	     function introduced by a using declaration the program is
	     ill-formed.  */
	  /* This seems overreaching, asking core -- why do we care
	     about decls in the namespace that we cannot name (because
	     they are not transitively imported.  We just check the
	     decls that are in this TU.  */
	  bool found = false;
	  for (ovl_iterator old (value); !found && old; ++old)
	    {
	      tree old_fn = *old;

	      if (new_fn == old_fn)
		{
		  /* The function already exists in the current
		     namespace.  We will still want to insert it if
		     it is revealing a not-revealed thing.  */
		  found = true;
		  if (!revealing_p)
		    ;
		  else if (old.using_p ())
		    {
		      /* Update in place.  'tis ok.  */
		      OVL_PURVIEW_P (old.get_using ()) = true;
		      if (exporting_p)
			OVL_EXPORT_P (old.get_using ()) = true;
		    }
		  else if (!DECL_LANG_SPECIFIC (inner)
			   || !DECL_MODULE_PURVIEW_P (inner))
		    /* We need to re-insert this function as a revealed
		       (possibly exported) declaration.  We can't remove
		       the existing decl because that will change any
		       overloads cached in template functions.  */
		    found = false;
		  break;
		}
	      else if (old.using_p ())
		continue; /* This is a using decl. */
	      else if (old.hidden_p () && DECL_IS_UNDECLARED_BUILTIN (old_fn))
		continue; /* This is an anticipated builtin.  */
	      else if (!matching_fn_p (new_fn, old_fn))
		continue; /* Parameters do not match.  */
	      else if (decls_match (new_fn, old_fn))
		{
		  /* Extern "C" in different namespaces.  But similarly
		     to above, if revealing a not-revealed thing we may
		     need to reinsert.  */
		  found = true;
		  if (revealing_p
		      && (!DECL_LANG_SPECIFIC (inner)
			  || !DECL_MODULE_PURVIEW_P (inner)))
		    found = false;
		  break;
		}
	      else
		{
		  diagnose_name_conflict (new_fn, old_fn);
		  failed = true;
		  found = true;
		  break;
		}
	    }

	  if (!found && insert_p)
	    /* Unlike the decl-pushing case we don't drop anticipated
	       builtins here.  They don't cause a problem, and we'd
	       like to match them with a future declaration.  */
	    value = ovl_insert (new_fn, value, 1 + revealing_p + exporting_p);
	}
    }
  else if (value
	   /* Ignore anticipated builtins.  */
	   && !anticipated_builtin_p (value)
	   && !decls_match (lookup.value, strip_using_decl (value)))
    {
      diagnose_name_conflict (lookup.value, value);
      failed = true;
    }
  else if (insert_p)
    {
      /* A using-decl does not necessarily have the same purview-ness or
	 exporting as the declaration it reveals, so build a USING_DECL
	 that we can attach this information to.  This also gives us a
	 location for the using-decl that we can use in diagnostics.

	 But this is unnecessary if we're just redeclaring the same decl;
	 in that case we can just mark it purview or exported directly.  */
      if (value != lookup.value)
	{
	  value = build_lang_decl (USING_DECL, lookup.name, NULL_TREE);
	  USING_DECL_DECLS (value) = lookup.value;
	  USING_DECL_SCOPE (value) = CP_DECL_CONTEXT (lookup.value);
	  DECL_CONTEXT (value) = current_scope ();
	  DECL_MODULE_PURVIEW_P (value) = module_purview_p ();
	}
      else
	set_instantiating_module (value);

      if (revealing_p
	  && module_exporting_p ()
	  && check_can_export_using_decl (lookup.value))
	{
	  if (TREE_CODE (value) == TEMPLATE_DECL)
	    DECL_MODULE_EXPORT_P (DECL_TEMPLATE_RESULT (value)) = true;
	  DECL_MODULE_EXPORT_P (value) = true;
	}
    }

  /* Now the type binding.  */
  if (lookup.type)
    {
      if (type && !decls_match (lookup.type, strip_using_decl (type)))
	{
	  diagnose_name_conflict (lookup.type, type);
	  failed = true;
	}
      else if (insert_p)
	{
	  /* As with revealing value bindings.  */
	  if (type != lookup.type)
	    {
	      type = build_lang_decl (USING_DECL, lookup.name, NULL_TREE);
	      USING_DECL_DECLS (type) = lookup.type;
	      USING_DECL_SCOPE (type) = CP_DECL_CONTEXT (lookup.type);
	      DECL_CONTEXT (type) = current_scope ();
	      DECL_MODULE_PURVIEW_P (type) = module_purview_p ();
	    }
	  else
	    set_instantiating_module (type);

	  if (revealing_p
	      && module_exporting_p ()
	      && check_can_export_using_decl (lookup.type))
	    DECL_MODULE_EXPORT_P (type) = true;
	}
    }

  if (insert_p)
    {
      /* If value is empty, shift any class or enumeration name back.  */
      if (!value)
	{
	  value = type;
	  type = NULL_TREE;
	}
      *value_p = value;
      *type_p = type;
    }

  return failed;
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

/* Returns true if ROOT (a non-alias namespace, class, or function)
   encloses CHILD.  CHILD may be either a class type or a namespace
   (maybe alias).  */

bool
is_ancestor (tree root, tree child)
{
  gcc_checking_assert ((TREE_CODE (root) == NAMESPACE_DECL
			&& !DECL_NAMESPACE_ALIAS (root))
		       || TREE_CODE (root) == FUNCTION_DECL
		       || CLASS_TYPE_P (root));
  gcc_checking_assert (TREE_CODE (child) == NAMESPACE_DECL
		       || CLASS_TYPE_P (child));

  /* The global namespace encloses everything.  Early-out for the
     common case.  */
  if (root == global_namespace)
    return true;

  /* Search CHILD until we reach namespace scope.  */
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

  if (TREE_CODE (root) != NAMESPACE_DECL)
    /* Failed to meet the non-namespace we were looking for.  */
    return false;

  if (tree alias = DECL_NAMESPACE_ALIAS (child))
    child = alias;

  return is_nested_namespace (root, child);
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

  auto_cond_timevar tv (TV_NAME_LOOKUP);
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

      if (is_overloaded_fn (decl))
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

  /* Do nothing if we're adding to an outer lambda closure type,
     outer_binding will add it later if it's needed.  */
  if (current_class_type != class_binding_level->this_entity)
    return true;

  auto_cond_timevar tv (TV_NAME_LOOKUP);
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
      location_t save_location = input_location;
      tree anon = TREE_TYPE (x);
      if (vec<tree, va_gc> *member_vec = CLASSTYPE_MEMBER_VEC (anon))
	for (unsigned ix = member_vec->length (); ix--;)
	  {
	    tree binding = (*member_vec)[ix];
	    if (STAT_HACK_P (binding))
	      {
		if (!pushdecl_class_level (STAT_TYPE (binding)))
		  is_valid = false;
		binding = STAT_DECL (binding);
	      }
	    if (!pushdecl_class_level (binding))
	      is_valid = false;
	}
      else
	for (tree f = TYPE_FIELDS (anon); f; f = DECL_CHAIN (f))
	  if (TREE_CODE (f) == FIELD_DECL)
	    {
	      input_location = DECL_SOURCE_LOCATION (f);
	      if (!pushdecl_class_level (f))
		is_valid = false;
	    }
      input_location = save_location;
    }
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

  /* If we found either a type binding or a value binding, create a
     new binding object.  */
  if (type_binding || value_binding)
    {
      binding = new_class_binding (name,
				   value_binding,
				   type_binding,
				   scope);
      set_inherited_value_binding_p (binding, value_binding, class_type);
    }
  else
    binding = NULL;

  return binding;
}

/* Make the declaration(s) of X appear in CLASS scope under the name
   NAME.  Returns true if the binding is valid.  */

bool
push_class_level_binding (tree name, tree x)
{
  cxx_binding *binding;
  tree decl = x;
  bool ok;

  auto_cond_timevar tv (TV_NAME_LOOKUP);

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
	  error_at (DECL_SOURCE_LOCATION (x),
		    "%qD has the same name as the class in which it is "
		    "declared", x);
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
	       && DECL_DEPENDENT_P (decl)
	       && OVL_P (target_bval))
	/* The new dependent using beats an old overload.  */
	old_decl = bval;
      else if (TREE_CODE (bval) == USING_DECL
	       && DECL_DEPENDENT_P (bval)
	       && OVL_P (target_decl))
	/* The old dependent using beats a new overload.  */
	return true;
      else if (OVL_P (target_decl)
	       && OVL_P (target_bval))
	/* The new overload set contains the old one.  */
	old_decl = bval;

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

/* Process and lookup a using decl SCOPE::lookup.name, filling in
   lookup.values & lookup.type.  Return a USING_DECL, or NULL_TREE on
   failure.  */

static tree
lookup_using_decl (tree scope, name_lookup &lookup)
{
  tree current = current_scope ();
  bool dependent_p = false;
  tree binfo = NULL_TREE;
  base_kind b_kind = bk_not_base;

  /* Because C++20 breaks the invariant that only member using-decls
     refer to members and only non-member using-decls refer to
     non-members, we first do the lookups, and then do validation that
     what we found is ok.  */

  if (TREE_CODE (scope) == ENUMERAL_TYPE
      && cxx_dialect < cxx20
      && UNSCOPED_ENUM_P (scope)
      && !TYPE_FUNCTION_SCOPE_P (scope))
    {
      /* PR c++/60265 argued that since C++11 added explicit enum scope, we
	 should allow it as meaning the enclosing scope.  I don't see any
	 justification for this in C++11, but let's keep allowing it.  */
      tree ctx = CP_TYPE_CONTEXT (scope);
      if (CLASS_TYPE_P (ctx) == CLASS_TYPE_P (current))
	scope = ctx;
    }

  /* You cannot using-decl a destructor.  */
  if (TREE_CODE (lookup.name) == BIT_NOT_EXPR)
    {
      error ("%<%T%s%D%> names destructor", scope,
	     &"::"[scope == global_namespace ? 2 : 0], lookup.name);
      return NULL_TREE;
    }

  if (TREE_CODE (scope) == NAMESPACE_DECL)
    {
      /* Naming a namespace member.  */
      qualified_namespace_lookup (scope, &lookup);

      if (TYPE_P (current)
	  && (!lookup.value
	      || lookup.type
	      || cxx_dialect < cxx20
	      || TREE_CODE (lookup.value) != CONST_DECL))
	{
	  error ("using-declaration for non-member at class scope");
	  return NULL_TREE;
	}
    }
  else if (TREE_CODE (scope) == ENUMERAL_TYPE)
    {
      /* Naming an enumeration member.  */
      if (cxx_dialect < cxx20)
	error ("%<using%> with enumeration scope %q#T "
	       "only available with %<-std=c++20%> or %<-std=gnu++20%>",
	       scope);
      lookup.value = lookup_enumerator (scope, lookup.name);
    }
  else
    {
      /* Naming a class member.  This is awkward in C++20, because we
	 might be naming an enumerator of an unrelated class.  */

      tree npscope = scope;
      if (PACK_EXPANSION_P (scope))
	npscope = PACK_EXPANSION_PATTERN (scope);

      if (!MAYBE_CLASS_TYPE_P (npscope))
	{
	  error ("%qT is not a class, namespace, or enumeration", npscope);
	  return NULL_TREE;
	}

      /* Using T::T declares inheriting ctors, even if T is a typedef.  */
      if (lookup.name == TYPE_IDENTIFIER (npscope)
	  || constructor_name_p (lookup.name, npscope))
	{
	  if (!TYPE_P (current))
	    {
	      error ("non-member using-declaration names constructor of %qT",
		     npscope);
	      return NULL_TREE;
	    }
	  maybe_warn_cpp0x (CPP0X_INHERITING_CTORS);
	  lookup.name = ctor_identifier;
	  CLASSTYPE_NON_AGGREGATE (current) = true;
    	}

      if (!TYPE_P (current) && cxx_dialect < cxx20)
	{
	  error ("using-declaration for member at non-class scope");
	  return NULL_TREE;
	}

      bool depscope = dependent_scope_p (scope);

      if (depscope)
	/* Leave binfo null.  */;
      else if (TYPE_P (current))
	{
	  binfo = lookup_base (current, scope, ba_any, &b_kind, tf_none);
	  gcc_checking_assert (b_kind >= bk_not_base);

	  if (b_kind == bk_not_base && any_dependent_bases_p ())
	    /* Treat as-if dependent.  */
	    depscope = true;
	  else if (lookup.name == ctor_identifier
		   && (b_kind < bk_proper_base || !binfo_direct_p (binfo)))
	    {
	      if (any_dependent_bases_p ())
		depscope = true;
	      else
		{
		  error ("%qT is not a direct base of %qT", scope, current);
		  return NULL_TREE;
		}
	    }

	  if (b_kind < bk_proper_base)
	    binfo = TYPE_BINFO (scope);
	}
      else
	binfo = TYPE_BINFO (scope);

      dependent_p = (depscope
		     || (IDENTIFIER_CONV_OP_P (lookup.name)
			 && dependent_type_p (TREE_TYPE (lookup.name))));

      if (!dependent_p)
	lookup.value = lookup_member (binfo, lookup.name, /*protect=*/2,
				      /*want_type=*/false, tf_none);

      /* If the lookup in the base contains a dependent using, this
	 using is also dependent.  */
      if (!dependent_p && lookup.value && dependent_type_p (scope))
	{
	  tree val = lookup.value;
	  if (tree fns = maybe_get_fns (val))
	    val = fns;
	  for (tree f: lkp_range (val))
	    if (TREE_CODE (f) == USING_DECL && DECL_DEPENDENT_P (f))
	      {
		dependent_p = true;
		break;
	      }
	}

      if (!depscope && b_kind < bk_proper_base)
	{
	  if (cxx_dialect >= cxx20 && lookup.value
	      && TREE_CODE (lookup.value) == CONST_DECL)
	    {
	      /* Using an unrelated enum; check access here rather
		 than separately for class and non-class using.  */
	      perform_or_defer_access_check
		(binfo, lookup.value, lookup.value, tf_warning_or_error);
	      /* And then if this is a copy from handle_using_decl, look
		 through to the original enumerator.  */
	      if (CONST_DECL_USING_P (lookup.value))
		lookup.value = DECL_ABSTRACT_ORIGIN (lookup.value);
	    }
	  else if (!TYPE_P (current))
	    {
	      error ("using-declaration for member at non-class scope");
	      return NULL_TREE;
	    }
	  else
	    {
	      auto_diagnostic_group g;
	      error_not_base_type (scope, current);
	      if (lookup.value && DECL_IMPLICIT_TYPEDEF_P (lookup.value)
		  && TREE_CODE (TREE_TYPE (lookup.value)) == ENUMERAL_TYPE)
		inform (input_location,
			"did you mean %<using enum %T::%D%>?",
			scope, lookup.name);
	      return NULL_TREE;
	    }
	}
    }

  /* Did we find anything sane?  */
  if (dependent_p)
    ;
  else if (!lookup.value)
    {
      error ("%qD has not been declared in %qD", lookup.name, scope);
      return NULL_TREE;
    }
  else if (TREE_CODE (lookup.value) == TREE_LIST
	   /* We can (independently) have ambiguous implicit typedefs.  */
	   || (lookup.type && TREE_CODE (lookup.type) == TREE_LIST))
    {
      auto_diagnostic_group d;
      error ("reference to %qD is ambiguous", lookup.name);
      print_candidates (TREE_CODE (lookup.value) == TREE_LIST
			? lookup.value : lookup.type);
      return NULL_TREE;
    }
  else if (TREE_CODE (lookup.value) == NAMESPACE_DECL)
    {
      error ("using-declaration may not name namespace %qD", lookup.value);
      return NULL_TREE;
    }

  if (TYPE_P (current))
    {
      /* In class scope.  */

      /* Cannot introduce a constructor name.  */
      if (constructor_name_p (lookup.name, current))
	{
	  error ("%<%T::%D%> names constructor in %qT",
		 scope, lookup.name, current);
	  return NULL_TREE;
	}

      if (lookup.value && BASELINK_P (lookup.value))
	/* The binfo from which the functions came does not matter.  */
	lookup.value = BASELINK_FUNCTIONS (lookup.value);
    }

  tree using_decl = build_lang_decl (USING_DECL, lookup.name, NULL_TREE);
  USING_DECL_SCOPE (using_decl) = scope;
  USING_DECL_DECLS (using_decl) = lookup.value;
  DECL_DEPENDENT_P (using_decl) = dependent_p;
  DECL_CONTEXT (using_decl) = current;
  if (TYPE_P (current) && b_kind == bk_not_base)
    USING_DECL_UNRELATED_P (using_decl) = true;

  return using_decl;
}

/* Process "using SCOPE::NAME" in a class scope.  Return the
   USING_DECL created.  */

tree
do_class_using_decl (tree scope, tree name)
{
  if (name == error_mark_node
      || scope == error_mark_node)
    return NULL_TREE;

  name_lookup lookup (name);
  return lookup_using_decl (scope, lookup);
}


/* Return the binding for NAME in NS in the current TU.  If NS is
   NULL, look in global_namespace.  We will not find declarations
   from imports.  Users of this who, having found nothing, push a new
   decl must be prepared for that pushing to match an existing decl.  */

tree
get_namespace_binding (tree ns, tree name)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);
  if (!ns)
    ns = global_namespace;
  gcc_checking_assert (!DECL_NAMESPACE_ALIAS (ns));
  tree ret = NULL_TREE;

  if (tree *b = find_namespace_slot (ns, name))
    {
      ret = *b;

      if (TREE_CODE (ret) == BINDING_VECTOR)
	ret = BINDING_VECTOR_CLUSTER (ret, 0).slots[0];
      if (ret)
	ret = strip_using_decl (MAYBE_STAT_DECL (ret));
    }

  return ret;
}

/* Push internal DECL into the global namespace.  Does not do the
   full overload fn handling and does not add it to the list of things
   in the namespace.  */

void
set_global_binding (tree decl)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);

  tree *slot = find_namespace_slot (global_namespace, DECL_NAME (decl), true);

  if (*slot)
    /* The user's placed something in the implementor's namespace.  */
    diagnose_name_conflict (decl, MAYBE_STAT_DECL (*slot));

  /* Force the binding, so compiler internals continue to work.  */
  *slot = decl;
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
    name_lookup lookup (DECL_NAME (decl),
			LOOK_want::NORMAL | LOOK_want::HIDDEN_FRIEND);
    if (!lookup.search_qualified (scope, /*usings=*/false))
      /* No old declaration at all.  */
      goto not_found;
    old = lookup.value;
  }

  /* If it's a TREE_LIST, the result of the lookup was ambiguous.  */
  if (TREE_CODE (old) == TREE_LIST)
    {
    ambiguous:
      auto_diagnostic_group d;
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
	error_at (DECL_SOURCE_LOCATION (decl),
		  "explicit qualification in declaration of %qD", decl);
      return;
    }

  /* Since decl is a function, old should contain a function decl.  */
  if (!OVL_P (old))
    {
    not_found:
      /* It didn't work, go back to the explicit scope.  */
      DECL_CONTEXT (decl) = FROB_CONTEXT (scope);
      error ("%qD should have been declared inside %qD", decl, scope);

      return;
    }

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

  tree found = NULL_TREE;
  bool hidden_p = false;
  bool saw_template = false;

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
	      /* We found more than one matching declaration.  This
		 can happen if we have two inline namespace children,
		 each containing a suitable declaration.  */
	      DECL_CONTEXT (decl) = FROB_CONTEXT (scope);
	      goto ambiguous;
	    }
	  found = ofn;
	  hidden_p = iter.hidden_p ();
	}
      else if (TREE_CODE (decl) == FUNCTION_DECL
	       && TREE_CODE (ofn) == TEMPLATE_DECL)
	saw_template = true;
    }

  if (!found && friendp && saw_template)
    {
      /* "[if no non-template match is found,] each remaining function template
	 is replaced with the specialization chosen by deduction from the
	 friend declaration or discarded if deduction fails."

	 So tell check_explicit_specialization to look for a match.  */
      SET_DECL_IMPLICIT_INSTANTIATION (decl);
      DECL_TEMPLATE_INFO (decl) = build_template_info (old, NULL_TREE);
      return;
    }

  if (found)
    {
      if (hidden_p)
	{
	  auto_diagnostic_group d;
	  pedwarn (DECL_SOURCE_LOCATION (decl), 0,
		   "%qD has not been declared within %qD", decl, scope);
	  inform (DECL_SOURCE_LOCATION (found),
		  "only here as a %<friend%>");
	}
      DECL_CONTEXT (decl) = DECL_CONTEXT (found);
      goto found;
    }

  goto not_found;
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

  if (attributes == error_mark_node)
    return false;

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
      else if (is_attribute_p ("deprecated", name))
	{
	  if (!DECL_NAME (ns))
	    {
	      warning (OPT_Wattributes, "ignoring %qD attribute on anonymous "
		       "namespace", name);
	      continue;
	    }
	  if (args && TREE_CODE (TREE_VALUE (args)) != STRING_CST)
	    {
	      error ("deprecated message is not a string");
	      continue;
	    }
	  TREE_DEPRECATED (ns) = 1;
	  if (args)
	    DECL_ATTRIBUTES (ns) = tree_cons (name, args,
					      DECL_ATTRIBUTES (ns));
	}
      else if (!attribute_ignored_p (d))
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
do_namespace_alias (location_t loc, tree alias, tree name_space)
{
  if (name_space == error_mark_node)
    return;

  gcc_assert (TREE_CODE (name_space) == NAMESPACE_DECL);

  name_space = ORIGINAL_NAMESPACE (name_space);

  /* Build the alias.  */
  alias = build_lang_decl_loc (loc, NAMESPACE_DECL, alias, void_type_node);
  DECL_NAMESPACE_ALIAS (alias) = name_space;
  DECL_EXTERNAL (alias) = 1;
  DECL_CONTEXT (alias) = FROB_CONTEXT (current_scope ());
  TREE_PUBLIC (alias) = TREE_PUBLIC (CP_DECL_CONTEXT (alias));

  alias = pushdecl (alias);

  if (!DECL_P (alias) || !DECL_NAMESPACE_ALIAS (alias))
    return;

  set_originating_module (alias);
  check_module_decl_linkage (alias);

  /* Emit debug info for namespace alias.  */
  if (!building_stmt_list_p ())
    (*debug_hooks->early_global_decl) (alias);
}

/* Like pushdecl, only it places DECL in the current namespace,
   if appropriate.  */

tree
pushdecl_namespace_level (tree decl, bool hiding)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);
  return do_pushdecl_with_scope (decl, NAMESPACE_LEVEL (current_namespace),
				 hiding);
}

/* Wrapper around push_local_binding to push the bindings for
   a non-member USING_DECL with NAME and VALUE.  LOOKUP, if non-null,
   is the result of name lookup during template parsing.  */

static void
push_using_decl_bindings (name_lookup *lookup, tree name, tree value)
{
  tree type = NULL_TREE;

  cxx_binding *binding = find_local_binding (current_binding_level, name);
  if (binding)
    {
      value = binding->value;
      type = binding->type;
    }

  if (lookup)
    do_nonmember_using_decl (*lookup, true, true, &value, &type);

  if (!value)
    ;
  else if (binding && value == binding->value)
    /* Redeclaration of this USING_DECL.  */;
  else if (binding && binding->value && TREE_CODE (value) == OVERLOAD)
    {
      /* We already have this binding, so replace it.  */
      update_local_overload (IDENTIFIER_BINDING (name), value);
      IDENTIFIER_BINDING (name)->value = value;
    }
  else
    /* Install the new binding.  */
    push_local_binding (name, value, /*using=*/true);

  if (!type)
    ;
  else if (binding && type == binding->type)
    ;
  else
    {
      push_local_binding (name, type, /*using=*/true);
      set_identifier_type_value (name, type);
    }
}

/* Overload for push_using_decl_bindings that doesn't take a name_lookup.  */

void
push_using_decl_bindings (tree name, tree value)
{
  push_using_decl_bindings (nullptr, name, value);
}

/* Process a using declaration in non-class scope.  */

void
finish_nonmember_using_decl (tree scope, tree name)
{
  gcc_checking_assert (current_binding_level->kind != sk_class);

  if (scope == error_mark_node || name == error_mark_node)
    return;

  name_lookup lookup (name);

  tree using_decl = lookup_using_decl (scope, lookup);
  if (!using_decl)
    return;

  /* Emit debug info.  */
  if (!processing_template_decl)
    cp_emit_debug_info_for_using (lookup.value,
				  current_binding_level->this_entity);

  if (current_binding_level->kind == sk_namespace)
    {
      tree *slot = find_namespace_slot (current_namespace, name, true);
      tree *mslot = get_fixed_binding_slot (slot, name,
					    BINDING_SLOT_CURRENT, true);
      bool failed = false;

      if (mslot != slot)
	{
	  /* A module vector.  I presume the binding list is going to
	     be sparser than the import bitmap.  Hence iterate over
	     the former checking for bits set in the bitmap.  */
	  bitmap imports = get_import_bitmap ();
	  binding_cluster *cluster = BINDING_VECTOR_CLUSTER_BASE (*slot);

	  /* Scan the imported bindings.  */
	  unsigned ix = BINDING_VECTOR_NUM_CLUSTERS (*slot);
	  if (BINDING_VECTOR_SLOTS_PER_CLUSTER == BINDING_SLOTS_FIXED)
	    {
	      ix--;
	      cluster++;
	    }

	  /* Do this in forward order, so we load modules in an order
	     the user expects.  */
	  for (; ix--; cluster++)
	    for (unsigned jx = 0; jx != BINDING_VECTOR_SLOTS_PER_CLUSTER; jx++)
	      {
		/* Are we importing this module?  */
		if (unsigned base = cluster->indices[jx].base)
		  if (unsigned span = cluster->indices[jx].span)
		    do
		      if (bitmap_bit_p (imports, base))
			goto found;
		    while (++base, --span);
		continue;

	      found:;
		/* Is it loaded?  */
		if (cluster->slots[jx].is_lazy ())
		  {
		    gcc_assert (cluster->indices[jx].span == 1);
		    lazy_load_binding (cluster->indices[jx].base,
				       scope, name, &cluster->slots[jx]);
		  }

		tree value = cluster->slots[jx];
		if (!value)
		  /* Load errors could mean there's nothing here.  */
		  continue;

		/* Extract what we can see from here.  If there's no
		   stat_hack, then everything was exported.  */
		tree type = NULL_TREE;

		/* If no stat hack, everything is visible.  */
		if (STAT_HACK_P (value))
		  {
		    if (STAT_TYPE_VISIBLE_P (value))
		      type = STAT_TYPE (value);
		    value = STAT_VISIBLE (value);
		  }

		if (do_nonmember_using_decl (lookup, false, false,
					     &value, &type))
		  {
		    failed = true;
		    break;
		  }
	      }
	}

      if (!failed)
	{
	  /* Now do the current slot.  */
	  tree value = MAYBE_STAT_DECL (*mslot);
	  tree type = MAYBE_STAT_TYPE (*mslot);

	  do_nonmember_using_decl (lookup, false, true, &value, &type);

	  // FIXME: Partition mergeableness?
	  if (STAT_HACK_P (*mslot))
	    {
	      STAT_DECL (*mslot) = value;
	      STAT_TYPE (*mslot) = type;
	    }
	  else if (type)
	    *mslot = stat_hack (value, type);
	  else
	    *mslot = value;
	}
    }
  else
    {
      add_decl_expr (using_decl);
      if (DECL_DEPENDENT_P (using_decl))
	lookup.value = using_decl;
      push_using_decl_bindings (&lookup, name, NULL_TREE);
    }
}

/* Return the declarations that are members of the namespace NS.  */

tree
cp_namespace_decls (tree ns)
{
  return NAMESPACE_LEVEL (ns)->names;
}

/* Given a lookup that returned VAL, use FLAGS to decide if we want to
   ignore it or not.  Subroutine of lookup_name_1 and lookup_type_scope.  */

static bool
qualify_lookup (tree val, LOOK_want want)
{
  if (val == NULL_TREE)
    return false;

  if (bool (want & LOOK_want::TYPE))
    {
      tree target_val = strip_using_decl (val);

      if (TREE_CODE (STRIP_TEMPLATE (target_val)) == TYPE_DECL)
	return true;
    }

  if (bool (want & LOOK_want::TYPE_NAMESPACE))
    return TREE_CODE (val) == NAMESPACE_DECL;

  return true;
}

/* Is there a "using namespace std;" directive within USINGS?  */

static bool
using_directives_contain_std_p (vec<tree, va_gc> *usings)
{
  if (!usings)
    return false;

  for (unsigned ix = usings->length (); ix--;)
    if ((*usings)[ix] == std_node)
      return true;

  return false;
}

/* Is there a "using namespace std;" directive within the current
   namespace (or its ancestors)?
   Compare with name_lookup::search_unqualified.  */

static bool
has_using_namespace_std_directive_p ()
{
  for (cp_binding_level *level = current_binding_level;
       level;
       level = level->level_chain)
    if (using_directives_contain_std_p (level->using_directives))
      return true;

  return false;
}

/* Subclass of deferred_diagnostic, for issuing a note when
   --param cxx-max-namespaces-for-diagnostic-help is reached.

   The note should be issued after the error, but before any other
   deferred diagnostics.  This is handled by decorating a wrapped
   deferred_diagnostic, and emitting a note before that wrapped note is
   deleted.  */

class namespace_limit_reached : public deferred_diagnostic
{
 public:
  namespace_limit_reached (location_t loc, unsigned limit, tree name,
			   std::unique_ptr<deferred_diagnostic> wrapped)
  : deferred_diagnostic (loc),
    m_limit (limit), m_name (name),
    m_wrapped (std::move (wrapped))
  {
  }

  ~namespace_limit_reached ()
  {
    /* Unconditionally warn that the search was truncated.  */
    inform (get_location (),
	    "maximum limit of %d namespaces searched for %qE",
	    m_limit, m_name);
    /* m_wrapped will be implicitly deleted after this, emitting any followup
       diagnostic after the above note.  */
  }

 private:
  unsigned m_limit;
  tree m_name;
  std::unique_ptr<deferred_diagnostic> m_wrapped;
};

/* Subclass of deferred_diagnostic, for use when issuing a single suggestion.
   Emit a note showing the location of the declaration of the suggestion.  */

class show_candidate_location : public deferred_diagnostic
{
 public:
  show_candidate_location (location_t loc, tree candidate)
  : deferred_diagnostic (loc),
    m_candidate (candidate)
  {
  }

  ~show_candidate_location ()
  {
    inform (location_of (m_candidate), "%qE declared here", m_candidate);
  }

 private:
  tree m_candidate;
};

/* Subclass of deferred_diagnostic, for use when there are multiple candidates
   to be suggested by suggest_alternatives_for.

   Emit a series of notes showing the various suggestions.  */

class suggest_alternatives : public deferred_diagnostic
{
 public:
  suggest_alternatives (location_t loc, vec<tree> candidates)
  : deferred_diagnostic (loc),
    m_candidates (candidates)
  {
  }

  ~suggest_alternatives ()
  {
    if (m_candidates.length ())
      {
	inform_n (get_location (), m_candidates.length (),
		  "suggested alternative:",
		  "suggested alternatives:");
	for (unsigned ix = 0; ix != m_candidates.length (); ix++)
	  {
	    tree val = m_candidates[ix];

	    inform (location_of (val), "  %qE", val);
	  }
      }
    m_candidates.release ();
  }

 private:
  vec<tree> m_candidates;
};

/* A class for encapsulating the result of a search across
   multiple namespaces (and scoped enums within them) for an
   unrecognized name seen at a given source location.  */

class namespace_hints
{
 public:
  namespace_hints (location_t loc, tree name);

  name_hint convert_candidates_to_name_hint ();
  name_hint maybe_decorate_with_limit (name_hint);

 private:
  void maybe_add_candidate_for_scoped_enum (tree scoped_enum, tree name);

  location_t m_loc;
  tree m_name;
  vec<tree> m_candidates;

  /* Value of "--param cxx-max-namespaces-for-diagnostic-help".  */
  unsigned m_limit;

  /* Was the limit reached?  */
  bool m_limited;
};

/* Constructor for namespace_hints.  Search namespaces and scoped enums,
   looking for an exact match for unrecognized NAME seen at LOC.  */

namespace_hints::namespace_hints (location_t loc, tree name)
: m_loc(loc), m_name (name)
{
  auto_vec<tree> worklist;

  m_candidates = vNULL;
  m_limited = false;
  m_limit = param_cxx_max_namespaces_for_diagnostic_help;

  /* Breadth-first search of namespaces.  Up to limit namespaces
     searched (limit zero == unlimited).  */
  worklist.safe_push (global_namespace);
  for (unsigned ix = 0; ix != worklist.length (); ix++)
    {
      tree ns = worklist[ix];
      name_lookup lookup (name);

      if (lookup.search_qualified (ns, false))
	m_candidates.safe_push (lookup.value);

      if (!m_limited)
	{
	  /* Look for child namespaces.  We have to do this
	     indirectly because they are chained in reverse order,
	     which is confusing to the user.  */
	  auto_vec<tree> children;

	  for (tree decl = NAMESPACE_LEVEL (ns)->names;
	       decl; decl = TREE_CHAIN (decl))
	    {
	      if (TREE_CODE (decl) == NAMESPACE_DECL
		  && !DECL_NAMESPACE_ALIAS (decl)
		  && !DECL_NAMESPACE_INLINE_P (decl))
		children.safe_push (decl);

	      /* Look for exact matches for NAME within scoped enums.
		 These aren't added to the worklist, and so don't count
		 against the search limit.  */
	      if (TREE_CODE (decl) == TYPE_DECL)
		{
		  tree type = TREE_TYPE (decl);
		  if (SCOPED_ENUM_P (type))
		    maybe_add_candidate_for_scoped_enum (type, name);
		}
	    }

	  while (!m_limited && !children.is_empty ())
	    {
	      if (worklist.length () == m_limit)
		m_limited = true;
	      else
		worklist.safe_push (children.pop ());
	    }
	}
    }
}

/* Drop ownership of m_candidates, using it to generate a name_hint at m_loc
   for m_name, an IDENTIFIER_NODE for which name lookup failed.

   If m_candidates is non-empty, use it to generate a suggestion and/or
   a deferred diagnostic that lists the possible candidate(s).
*/

name_hint
namespace_hints::convert_candidates_to_name_hint ()
{
  /* How many candidates do we have?  */

  /* If we have just one candidate, issue a name_hint with it as a suggestion
     (so that consumers are able to suggest it within the error message and emit
     it as a fix-it hint), and with a note showing the candidate's location.  */
  if (m_candidates.length () == 1)
    {
      tree candidate = m_candidates[0];
      /* Clean up CANDIDATES.  */
      m_candidates.release ();
      return name_hint (expr_to_string (candidate),
			new show_candidate_location (m_loc, candidate));
    }
  else if (m_candidates.length () > 1)
    /* If we have more than one candidate, issue a name_hint without a single
       "suggestion", but with a deferred diagnostic that lists the
       various candidates.  This takes ownership of m_candidates.  */
    return name_hint (NULL, new suggest_alternatives (m_loc, m_candidates));

  /* Otherwise, m_candidates ought to be empty, so no cleanup is necessary.  */
  gcc_assert (m_candidates.length () == 0);
  gcc_assert (m_candidates == vNULL);

  return name_hint ();
}

/* If --param cxx-max-namespaces-for-diagnostic-help was reached,
   then we want to emit a note about after the error, but before
   any other deferred diagnostics.

   Handle this by figuring out what hint is needed, then optionally
   decorating HINT with a namespace_limit_reached wrapper.  */

name_hint
namespace_hints::maybe_decorate_with_limit (name_hint hint)
{
  if (m_limited)
    return name_hint (hint.suggestion (),
		      new namespace_limit_reached (m_loc, m_limit,
						   m_name,
						   hint.take_deferred ()));
  else
    return hint;
}

/* Look inside SCOPED_ENUM for exact matches for NAME.
   If one is found, add its CONST_DECL to m_candidates.  */

void
namespace_hints::maybe_add_candidate_for_scoped_enum (tree scoped_enum,
						      tree name)
{
  gcc_assert (SCOPED_ENUM_P (scoped_enum));

  for (tree iter = TYPE_VALUES (scoped_enum); iter; iter = TREE_CHAIN (iter))
    {
      tree id = TREE_PURPOSE (iter);
      if (id == name)
	{
	  m_candidates.safe_push (TREE_VALUE (iter));
	  return;
	}
    }
}

/* Generate a name_hint at LOCATION for NAME, an IDENTIFIER_NODE for which
   name lookup failed.

   Search through all available namespaces and any scoped enums within them
   and generate a suggestion and/or a deferred diagnostic that lists possible
   candidate(s).

   If no exact matches are found, and SUGGEST_MISSPELLINGS is true, then also
   look for near-matches and suggest the best near-match, if there is one.

   If nothing is found, then an empty name_hint is returned.  */

name_hint
suggest_alternatives_for (location_t location, tree name,
			  bool suggest_misspellings)
{
  /* First, search for exact matches in other namespaces.  */
  namespace_hints ns_hints (location, name);
  name_hint result = ns_hints.convert_candidates_to_name_hint ();

  /* Otherwise, try other approaches.  */
  if (!result)
    result = suggest_alternatives_for_1 (location, name, suggest_misspellings);

  return ns_hints.maybe_decorate_with_limit (std::move (result));
}

/* The second half of suggest_alternatives_for, for when no exact matches
   were found in other namespaces.  */

static name_hint
suggest_alternatives_for_1 (location_t location, tree name,
			    bool suggest_misspellings)
{
  /* No candidates were found in the available namespaces.  */

  /* If there's a "using namespace std;" active, and this
     is one of the most common "std::" names, then it's probably a
     missing #include.  */
  if (has_using_namespace_std_directive_p ())
    {
      name_hint hint = maybe_suggest_missing_std_header (location, name);
      if (hint)
	return hint;
    }

  /* Look for exact matches for builtin defines that would have been
     defined if the user had passed a command-line option (e.g. -fopenmp
     for "_OPENMP").  */
  diagnostic_option_id option_id
    = get_option_for_builtin_define (IDENTIFIER_POINTER (name));
  if (option_id.m_idx > 0)
    return name_hint (nullptr,
		      new suggest_missing_option (location,
						  IDENTIFIER_POINTER (name),
						  option_id));

  /* Otherwise, consider misspellings.  */
  if (!suggest_misspellings)
    return name_hint ();

  return lookup_name_fuzzy (name, FUZZY_LOOKUP_NAME, location);
}

/* Generate a name_hint at LOCATION for NAME, an IDENTIFIER_NODE for which
   name lookup failed.

   Search through all available namespaces and generate a suggestion and/or
   a deferred diagnostic that lists possible candidate(s).

   This is similiar to suggest_alternatives_for, but doesn't fallback to
   the other approaches used by that function.  */

name_hint
suggest_alternatives_in_other_namespaces (location_t location, tree name)
{
  namespace_hints ns_hints (location, name);

  name_hint result = ns_hints.convert_candidates_to_name_hint ();

  return ns_hints.maybe_decorate_with_limit (std::move (result));
}

/* A well-known name within the C++ standard library, returned by
   get_std_name_hint.

   The gperf-generated file contains the definition of the class
   "std_name_hint_lookup" with a static member function which
   returns the pointer to a structure "std_name_hint" which
   is also defined in that file.  */

#include "std-name-hint.h"

/* Subroutine of maybe_suggest_missing_header for handling unrecognized names
   for some of the most common names within "std::".
   Given non-NULL NAME, return the std_name_hint for it, or NULL.  */

static const std_name_hint *
get_std_name_hint (const char *name)
{
  return std_name_hint_lookup::find(name, strlen(name));
}

/* Describe DIALECT.  */

const char *
get_cxx_dialect_name (enum cxx_dialect dialect)
{
  switch (dialect)
    {
    default:
      gcc_unreachable ();
    case cxx98:
      return "C++98";
    case cxx11:
      return "C++11";
    case cxx14:
      return "C++14";
    case cxx17:
      return "C++17";
    case cxx20:
      return "C++20";
    case cxx23:
      return "C++23";
    case cxx26:
      return "C++26";
    }
}

/* Subclass of deferred_diagnostic for use for names in the "std" namespace
   that weren't recognized, but for which we know which header it ought to be
   in.

   Emit a note either suggesting the header to be included, or noting that
   the current dialect is too early for the given name.  */

class missing_std_header : public deferred_diagnostic
{
 public:
  missing_std_header (location_t loc,
		      const char *name_str,
		      const std_name_hint *header_hint)
  : deferred_diagnostic (loc),
    m_name_str (name_str),
    m_header_hint (header_hint)
  {}
  ~missing_std_header ()
  {
    gcc_rich_location richloc (get_location ());
    if (cxx_dialect >= m_header_hint->min_dialect)
      {
	const char *header = m_header_hint->header;
	maybe_add_include_fixit (&richloc, header, true);
	inform (&richloc,
		"%<std::%s%> is defined in header %qs;"
		" this is probably fixable by adding %<#include %s%>",
		m_name_str, header, header);
      }
    else
      inform (&richloc,
	      "%<std::%s%> is only available from %s onwards",
	      m_name_str, get_cxx_dialect_name (m_header_hint->min_dialect));
  }

private:
  const char *m_name_str;
  const std_name_hint *m_header_hint;
};

/* Attempt to generate a name_hint that suggests pertinent header files
   for NAME at LOCATION, for common names within the "std" namespace,
   or an empty name_hint if this isn't applicable.  */

static name_hint
maybe_suggest_missing_std_header (location_t location, tree name)
{
  gcc_assert (TREE_CODE (name) == IDENTIFIER_NODE);

  const char *name_str = IDENTIFIER_POINTER (name);
  const std_name_hint *header_hint = get_std_name_hint (name_str);
  if (!header_hint)
    return name_hint ();

  return name_hint (NULL, new missing_std_header (location, name_str,
						  header_hint));
}

/* Attempt to generate a name_hint that suggests a missing header file
   for NAME within SCOPE at LOCATION, or an empty name_hint if this isn't
   applicable.  */

name_hint
maybe_suggest_missing_header (location_t location, tree name, tree scope)
{
  if (scope == NULL_TREE)
    return name_hint ();
  if (TREE_CODE (scope) != NAMESPACE_DECL)
    return name_hint ();
  /* We only offer suggestions for the "std" namespace.  */
  if (scope != std_node)
    return name_hint ();
  return maybe_suggest_missing_std_header (location, name);
}

/* Generate a name_hint at LOCATION for NAME, an IDENTIFIER_NODE for which name
   lookup failed within the explicitly provided SCOPE.

   Suggest the best meaningful candidates (if any), otherwise
   an empty name_hint is returned.  */

name_hint
suggest_alternative_in_explicit_scope (location_t location, tree name,
				       tree scope)
{
  /* Something went very wrong; don't suggest anything.  */
  if (name == error_mark_node)
    return name_hint ();

  if (TREE_CODE (scope) != NAMESPACE_DECL)
    return name_hint ();

  /* Resolve any namespace aliases.  */
  scope = ORIGINAL_NAMESPACE (scope);

  name_hint hint = maybe_suggest_missing_header (location, name, scope);
  if (hint)
    return hint;

  cp_binding_level *level = NAMESPACE_LEVEL (scope);

  best_match <tree, const char *> bm (name);
  consider_binding_level (name, bm, level, false, FUZZY_LOOKUP_NAME);

  /* See if we have a good suggesion for the user.  */
  const char *fuzzy_name = bm.get_best_meaningful_candidate ();
  if (fuzzy_name)
    return name_hint (fuzzy_name, NULL);

  return name_hint ();
}

/* Given NAME, look within SCOPED_ENUM for possible spell-correction
   candidates.  */

name_hint
suggest_alternative_in_scoped_enum (tree name, tree scoped_enum)
{
  gcc_assert (SCOPED_ENUM_P (scoped_enum));

  best_match <tree, const char *> bm (name);
  for (tree iter = TYPE_VALUES (scoped_enum); iter; iter = TREE_CHAIN (iter))
    {
      tree id = TREE_PURPOSE (iter);
      bm.consider (IDENTIFIER_POINTER (id));
    }
  return name_hint (bm.get_best_meaningful_candidate (), NULL);
}

/* Look up NAME (an IDENTIFIER_NODE) in SCOPE (either a NAMESPACE_DECL
   or a class TYPE).

   WANT as for lookup_name_1.

   Returns a DECL (or OVERLOAD, or BASELINK) representing the
   declaration found.  If no suitable declaration can be found,
   ERROR_MARK_NODE is returned.  If COMPLAIN is true and SCOPE is
   neither a class-type nor a namespace a diagnostic is issued.  */

tree
lookup_qualified_name (tree scope, tree name, LOOK_want want, bool complain)
{
  tree t = NULL_TREE;

  if (TREE_CODE (scope) == NAMESPACE_DECL)
    {
      name_lookup lookup (name, want);

      if (qualified_namespace_lookup (scope, &lookup))
	{
	  t = lookup.value;

	  /* If we have a known type overload, pull it out.  This can happen
	     for using decls.  */
	  if (TREE_CODE (t) == OVERLOAD && TREE_TYPE (t) != unknown_type_node)
	    t = OVL_FUNCTION (t);
	}
    }
  else if (cxx_dialect != cxx98 && TREE_CODE (scope) == ENUMERAL_TYPE)
    t = lookup_enumerator (scope, name);
  else if (is_class_type (scope, complain))
    t = lookup_member (scope, name, 2, bool (want & LOOK_want::TYPE),
		       tf_warning_or_error);

  if (!t)
    return error_mark_node;
  return t;
}

/* Wrapper for the above that takes a string argument.  The function name is
   not at the beginning of the line to keep this wrapper out of etags.  */

tree lookup_qualified_name (tree t, const char *p, LOOK_want w, bool c)
{
  return lookup_qualified_name (t, get_identifier (p), w, c);
}

/* [namespace.qual]
   Accepts the NAME to lookup and its qualifying SCOPE.
   Returns the name/type pair found into the cxx_binding *RESULT,
   or false on error.  */

static bool
qualified_namespace_lookup (tree scope, name_lookup *lookup)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);
  query_oracle (lookup->name);
  bool found = lookup->search_qualified (ORIGINAL_NAMESPACE (scope));
  return found;
}

/* If DECL is suitably visible to the user, consider its name for
   spelling correction.  */

static void
consider_decl (tree decl,  best_match <tree, const char *> &bm,
	       bool consider_impl_names)
{
  /* Skip compiler-generated variables (e.g. __for_begin/__for_end
     within range for).  */
  if (VAR_P (decl) && DECL_ARTIFICIAL (decl))
    return;

  tree suggestion = DECL_NAME (decl);
  if (!suggestion)
    return;

  /* Don't suggest names that are for anonymous aggregate types, as
     they are an implementation detail generated by the compiler.  */
  if (IDENTIFIER_ANON_P (suggestion))
    return;

  const char *suggestion_str = IDENTIFIER_POINTER (suggestion);

  /* Ignore internal names with spaces in them.  */
  if (strchr (suggestion_str, ' '))
    return;

  /* Don't suggest names that are reserved for use by the
     implementation, unless NAME began with an underscore.  */
  if (!consider_impl_names
      && name_reserved_for_implementation_p (suggestion_str))
    return;

  bm.consider (suggestion_str);
}

/* If DECL is suitably visible to the user, add its name to VEC and
   return true.  Otherwise return false.  */

static bool
maybe_add_fuzzy_decl (auto_vec<tree> &vec, tree decl)
{
  /* Skip compiler-generated variables (e.g. __for_begin/__for_end
     within range for).  */
  if (VAR_P (decl) && DECL_ARTIFICIAL (decl))
    return false;

  tree suggestion = DECL_NAME (decl);
  if (!suggestion)
    return false;

  /* Don't suggest names that are for anonymous aggregate types, as
     they are an implementation detail generated by the compiler.  */
  if (IDENTIFIER_ANON_P (suggestion))
    return false;

  vec.safe_push (suggestion);

  return true;
}

/* Examing the namespace binding BINDING, and add at most one instance
   of the name, if it contains a visible entity of interest.  Return
   true if we added something.  */

bool
maybe_add_fuzzy_binding (auto_vec<tree> &vec, tree binding,
			      lookup_name_fuzzy_kind kind)
{
  tree value = NULL_TREE;

  if (STAT_HACK_P (binding))
    {
      if (!STAT_TYPE_HIDDEN_P (binding)
	  && STAT_TYPE (binding))
	{
	  if (maybe_add_fuzzy_decl (vec, STAT_TYPE (binding)))
	    return true;
	}
      else if (!STAT_DECL_HIDDEN_P (binding))
	value = STAT_DECL (binding);
    }
  else
    value = binding;

  value = ovl_skip_hidden (value);
  if (value)
    {
      value = OVL_FIRST (value);
      if (kind != FUZZY_LOOKUP_TYPENAME
	  || TREE_CODE (STRIP_TEMPLATE (value)) == TYPE_DECL)
	if (maybe_add_fuzzy_decl (vec, value))
	  return true;
    }

  /* Nothing found.  */
  return false;
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

  /* Only suggest names reserved for the implementation if NAME begins
     with an underscore.  */
  bool consider_implementation_names = (IDENTIFIER_POINTER (name)[0] == '_');

  if (lvl->kind != sk_namespace)
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

	/* If we want a typename, ignore non-types.  */
	if (kind == FUZZY_LOOKUP_TYPENAME
	    && TREE_CODE (STRIP_TEMPLATE (d)) != TYPE_DECL)
	  continue;

	consider_decl (d, bm, consider_implementation_names);
      }
  else
    {
      /* We need to iterate over the namespace hash table, in order to
         not mention hidden entities.  But hash table iteration is
         (essentially) unpredictable, our correction-distance measure
         is very granular, and we pick the first of equal distances.
         Hence, we need to call the distance-measurer in a predictable
         order.  So, iterate over the namespace hash, inserting
         visible names into a vector.  Then sort the vector.  Then
         determine spelling distance.  */

      tree ns = lvl->this_entity;
      auto_vec<tree> vec;

      hash_table<named_decl_hash>::iterator end
	(DECL_NAMESPACE_BINDINGS (ns)->end ());
      for (hash_table<named_decl_hash>::iterator iter
	     (DECL_NAMESPACE_BINDINGS (ns)->begin ()); iter != end; ++iter)
	{
	  tree binding = *iter;

	  if (TREE_CODE (binding) == BINDING_VECTOR)
	    {
	      bitmap imports = get_import_bitmap ();
	      binding_cluster *cluster = BINDING_VECTOR_CLUSTER_BASE (binding);

	      if (tree bind = cluster->slots[BINDING_SLOT_CURRENT])
		if (maybe_add_fuzzy_binding (vec, bind, kind))
		  continue;

	      /* Scan the imported bindings.  */
	      unsigned ix = BINDING_VECTOR_NUM_CLUSTERS (binding);
	      if (BINDING_VECTOR_SLOTS_PER_CLUSTER == BINDING_SLOTS_FIXED)
		{
		  ix--;
		  cluster++;
		}

	      for (; ix--; cluster++)
		for (unsigned jx = 0; jx != BINDING_VECTOR_SLOTS_PER_CLUSTER;
		     jx++)
		  {
		    /* Are we importing this module?  */
		    if (unsigned base = cluster->indices[jx].base)
		      if (unsigned span = cluster->indices[jx].span)
			do
			  if (bitmap_bit_p (imports, base))
			    goto found;
			while (++base, --span);
		    continue;

		  found:;
		    /* Is it loaded?  */
		    if (cluster->slots[jx].is_lazy ())
		      /* Let's not read in everything on the first
			 spello! **/
		      continue;
		    if (tree bind = cluster->slots[jx])
		      if (maybe_add_fuzzy_binding (vec, bind, kind))
			break;
		  }
	    }
	  else
	    maybe_add_fuzzy_binding (vec, binding, kind);
	}

      vec.qsort ([] (const void *a_, const void *b_)
		 {
		   return strcmp (IDENTIFIER_POINTER (*(const tree *)a_),
				  IDENTIFIER_POINTER (*(const tree *)b_));
		 });

      /* Examine longest to shortest.  */
      for (unsigned ix = vec.length (); ix--;)
	{
	  const char *str = IDENTIFIER_POINTER (vec[ix]);

	  /* Ignore internal names with spaces in them.  */
	  if (strchr (str, ' '))
	    continue;

	  /* Don't suggest names that are reserved for use by the
	     implementation, unless NAME began with an underscore.  */
	  if (!consider_implementation_names
	      && name_reserved_for_implementation_p (str))
	    continue;

	  bm.consider (str);
	}
    }
}

/* Subclass of deferred_diagnostic.  Notify the user that the
   given macro was used before it was defined.
   This can be done in the C++ frontend since tokenization happens
   upfront.  */

class macro_use_before_def : public deferred_diagnostic
{
 public:
  /* Factory function.  Return a new macro_use_before_def instance if
     appropriate, or return NULL. */
  static macro_use_before_def *
  maybe_make (location_t use_loc, cpp_hashnode *macro)
  {
    location_t def_loc = cpp_macro_definition_location (macro);
    if (def_loc == UNKNOWN_LOCATION)
      return NULL;

    /* We only want to issue a note if the macro was used *before* it was
       defined.
       We don't want to issue a note for cases where a macro was incorrectly
       used, leaving it unexpanded (e.g. by using the wrong argument
       count).  */
    if (!linemap_location_before_p (line_table, use_loc, def_loc))
      return NULL;

    return new macro_use_before_def (use_loc, macro);
  }

 private:
  /* Ctor.  LOC is the location of the usage.  MACRO is the
     macro that was used.  */
  macro_use_before_def (location_t loc, cpp_hashnode *macro)
  : deferred_diagnostic (loc), m_macro (macro)
  {
    gcc_assert (macro);
  }

  ~macro_use_before_def ()
  {
    if (is_suppressed_p ())
      return;

    inform (get_location (), "the macro %qs had not yet been defined",
	    (const char *)m_macro->ident.str);
    inform (cpp_macro_definition_location (m_macro),
	    "it was later defined here");
  }

 private:
  cpp_hashnode *m_macro;
};

/* Determine if it can ever make sense to offer RID as a suggestion for
   a misspelling.

   Subroutine of lookup_name_fuzzy.  */

static bool
suggest_rid_p  (enum rid rid)
{
  switch (rid)
    {
    /* Support suggesting function-like keywords.  */
    case RID_STATIC_ASSERT:
      return true;

    default:
      /* Support suggesting the various decl-specifier words, to handle
	 e.g. "singed" vs "signed" typos.  */
      if (cp_keyword_starts_decl_specifier_p (rid))
	return true;

      /* Otherwise, don't offer it.  This avoids suggesting e.g. "if"
	 and "do" for short misspellings, which are likely to lead to
	 nonsensical results.  */
      return false;
    }
}

/* Search for near-matches for NAME within the current bindings, and within
   macro names, returning the best match as a const char *, or NULL if
   no reasonable match is found.

   Use LOC for any deferred diagnostics.  */

name_hint
lookup_name_fuzzy (tree name, enum lookup_name_fuzzy_kind kind, location_t loc)
{
  gcc_assert (TREE_CODE (name) == IDENTIFIER_NODE);

  /* First, try some well-known names in the C++ standard library, in case
     the user forgot a #include.  */
  const char *header_hint
    = get_cp_stdlib_header_for_name (IDENTIFIER_POINTER (name));
  if (header_hint)
    return name_hint (NULL,
		      new suggest_missing_header (loc,
						  IDENTIFIER_POINTER (name),
						  header_hint));

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
  else if (bmm.get_best_distance () == 0)
    {
      /* If we have an exact match for a macro name, then either the
	 macro was used with the wrong argument count, or the macro
	 has been used before it was defined.  */
      if (cpp_hashnode *macro = bmm.blithely_get_best_candidate ())
	if (cpp_user_macro_p (macro))
	  return name_hint (NULL,
			    macro_use_before_def::maybe_make (loc, macro));
    }

  /* Try the "starts_decl_specifier_p" keywords to detect
     "singed" vs "signed" typos.  */
  for (unsigned i = 0; i < num_c_common_reswords; i++)
    {
      const c_common_resword *resword = &c_common_reswords[i];

      if (!suggest_rid_p (resword->rid))
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

  return name_hint (bm.get_best_meaningful_candidate (), NULL);
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
		if (BASELINK_P (class_binding->value))
		  /* Don't put a BASELINK in IDENTIFIER_BINDING.  */
		  class_binding->value
		    = BASELINK_FUNCTIONS (class_binding->value);
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

/* True iff current_binding_level is within the potential scope of local
   variable DECL. */

bool
decl_in_scope_p (tree decl)
{
  gcc_checking_assert (DECL_FUNCTION_SCOPE_P (decl));

  tree name = DECL_NAME (decl);

  for (cxx_binding *iter = NULL;
       (iter = outer_binding (name, iter, /*class_p=*/false)); )
    {
      if (!LOCAL_BINDING_P (iter))
	return false;
      if (iter->value == decl)
	return true;
    }

  return false;
}

/* Look up NAME in the current binding level and its superiors in the
   namespace of variables, functions and typedefs.  Return a ..._DECL
   node of some kind representing its definition if there is only one
   such declaration, or return a TREE_LIST with all the overloaded
   definitions if there are many, or return NULL_TREE if it is undefined.
   Hidden name, either friend declaration or built-in function, are
   not ignored.

   WHERE controls which scopes are considered.  It is a bit mask of
   LOOK_where::BLOCK (look in block scope), LOOK_where::CLASS
   (look in class scopes) & LOOK_where::NAMESPACE (look in namespace
   scopes).  It is an error for no bits to be set.  These scopes are
   searched from innermost to outermost.

   WANT controls what kind of entity we'd happy with.
   LOOK_want::NORMAL for normal lookup (implicit typedefs can be
   hidden).  LOOK_want::TYPE for only TYPE_DECLS, LOOK_want::NAMESPACE
   for only NAMESPACE_DECLS.  These two can be bit-ored to find
   namespace or type.

   WANT can also have LOOK_want::HIDDEN_FRIEND or
   LOOK_want::HIDDEN_LAMBDa added to it.  */

tree
lookup_name (tree name, LOOK_where where, LOOK_want want)
{
  tree val = NULL_TREE;

  auto_cond_timevar tv (TV_NAME_LOOKUP);

  gcc_checking_assert (unsigned (where) != 0);
  /* If we're looking for hidden lambda things, we shouldn't be
     looking in namespace scope.  */
  gcc_checking_assert (!bool (want & LOOK_want::HIDDEN_LAMBDA)
		       || !bool (where & LOOK_where::NAMESPACE));
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
	  operators = lookup_fnfields (class_type, name, /*protect=*/0,
				       tf_warning_or_error);
	  if (operators)
	    return operators;
	}

      return NULL_TREE;
    }

  /* First, look in non-namespace scopes.  */

  if (current_class_type == NULL_TREE)
    /* Maybe avoid searching the binding stack at all.  */
    where = LOOK_where (unsigned (where) & ~unsigned (LOOK_where::CLASS));

  if (bool (where & (LOOK_where::BLOCK | LOOK_where::CLASS)))
    for (cxx_binding *iter = nullptr;
	 (iter = outer_binding (name, iter, bool (where & LOOK_where::CLASS)));)
      {
	/* Skip entities we don't want.  */
	if (!bool (where & (LOCAL_BINDING_P (iter)
			    ? LOOK_where::BLOCK : LOOK_where::CLASS)))
	  continue;

	/* If this is the kind of thing we're looking for, we're done.  */
	if (iter->value)
	  {
	    tree binding = NULL_TREE;

	    if (!(!iter->type && HIDDEN_TYPE_BINDING_P (iter))
		&& (bool (want & LOOK_want::HIDDEN_LAMBDA)
		    || !is_lambda_ignored_entity (iter->value))
		&& qualify_lookup (iter->value, want))
	      binding = iter->value;
	    else if (bool (want & LOOK_want::TYPE)
		     && !HIDDEN_TYPE_BINDING_P (iter)
		     && iter->type)
	      binding = iter->type;

	    if (binding)
	      {
		val = strip_using_decl (binding);
		break;
	      }
	  }
      }

  /* Now lookup in namespace scopes.  */
  if (!val && bool (where & LOOK_where::NAMESPACE))
    {
      name_lookup lookup (name, want);
      if (lookup.search_unqualified
	  (current_decl_namespace (), current_binding_level))
	val = lookup.value;
    }

  /* If we have a known type overload, pull it out.  This can happen
     for both using decls and unhidden functions.  */
  if (val && TREE_CODE (val) == OVERLOAD && TREE_TYPE (val) != unknown_type_node)
    val = OVL_FUNCTION (val);

  return val;
}

tree
lookup_name (tree name)
{
  return lookup_name (name, LOOK_where::ALL, LOOK_want::NORMAL);
}

/* Look up NAME for type used in elaborated name specifier in
   the scopes given by HOW.

   Unlike lookup_name_1, we make sure that NAME is actually
   declared in the desired scope, not from inheritance, nor using
   directive.  For using declaration, there is DR138 still waiting
   to be resolved.  Hidden name coming from an earlier friend
   declaration is also returned, and will be made visible unless HOW
   is TAG_how::HIDDEN_FRIEND.

   A TYPE_DECL best matching the NAME is returned.  Catching error
   and issuing diagnostics are caller's responsibility.  */

tree
lookup_elaborated_type (tree name, TAG_how how)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);

  cp_binding_level *b = current_binding_level;

  if (b->kind != sk_namespace)
    /* Look in non-namespace scopes.  */
    for (cxx_binding *iter = NULL;
	 (iter = outer_binding (name, iter, /*class_p=*/ true)); )
      {
	/* First check we're supposed to be looking in this scope --
	   if we're not, we're done.  */
	for (; b != iter->scope; b = b->level_chain)
	  if (!(b->kind == sk_cleanup
		|| b->kind == sk_template_parms
		|| b->kind == sk_function_parms
		|| (b->kind == sk_class && how != TAG_how::CURRENT_ONLY)))
	    return NULL_TREE;

	/* Check if this is the kind of thing we're looking for.  If
	   HOW is TAG_how::CURRENT_ONLY, also make sure it doesn't
	   come from base class.  For ITER->VALUE, we can simply use
	   INHERITED_VALUE_BINDING_P.  For ITER->TYPE, we have to use
	   our own check.

	   We check ITER->TYPE before ITER->VALUE in order to handle
	     typedef struct C {} C;
	   correctly.  */

	if (tree type = strip_using_decl (iter->type))
	  {
	    if (qualify_lookup (type, LOOK_want::TYPE)
		&& (how != TAG_how::CURRENT_ONLY
		    || LOCAL_BINDING_P (iter)
		    || DECL_CONTEXT (type) == iter->scope->this_entity))
	      {
		if (how != TAG_how::HIDDEN_FRIEND)
		  /* It is no longer a hidden binding.  */
		  HIDDEN_TYPE_BINDING_P (iter) = false;

		return type;
	      }
	  }
	else
	  {
	    tree value = strip_using_decl (iter->value);
	    if (qualify_lookup (value, LOOK_want::TYPE)
		&& (how != TAG_how::CURRENT_ONLY
		    || !INHERITED_VALUE_BINDING_P (iter)))
	      {
		if (how != TAG_how::HIDDEN_FRIEND && !iter->type)
		  /* It is no longer a hidden binding.  */
		  HIDDEN_TYPE_BINDING_P (iter) = false;

		return value;
	      }
	  }
      }

  /* Now check if we can look in namespace scope.  */
  for (; b->kind != sk_namespace; b = b->level_chain)
    if (!(b->kind == sk_cleanup
	  || b->kind == sk_template_parms
	  || b->kind == sk_function_parms
	  || (b->kind == sk_class && how != TAG_how::CURRENT_ONLY)))
      return NULL_TREE;

  /* Look in the innermost namespace.  */
  tree ns = b->this_entity;
  if (tree *slot = find_namespace_slot (ns, name))
    {
      tree bind = *slot;
      if (TREE_CODE (bind) == BINDING_VECTOR)
	bind = BINDING_VECTOR_CLUSTER (bind, 0).slots[BINDING_SLOT_CURRENT];

      if (bind)
	{
	  /* If this is the kind of thing we're looking for, we're done.  */
	  if (tree type = strip_using_decl (MAYBE_STAT_TYPE (bind)))
	    {
	      if (how != TAG_how::HIDDEN_FRIEND)
		/* No longer hidden.  */
		STAT_TYPE_HIDDEN_P (*slot) = false;

	      return type;
	    }
	  else if (tree decl = strip_using_decl (MAYBE_STAT_DECL (bind)))
	    {
	      if (qualify_lookup (decl, LOOK_want::TYPE))
		{
		  if (how != TAG_how::HIDDEN_FRIEND && STAT_HACK_P (bind)
		      && STAT_DECL_HIDDEN_P (bind))
		    {
		      if (STAT_TYPE (bind))
			STAT_DECL_HIDDEN_P (bind) = false;
		      else
			{
			  /* There is no type, just remove the stat
			     hack.  */
			  if (*slot == bind)
			    *slot = decl;
			  else
			    BINDING_VECTOR_CLUSTER (*slot, 0)
			      .slots[BINDING_SLOT_CURRENT] = decl;
			}
		    }
		  return decl;
		}
	    }
	}

      if (TREE_CODE (*slot) == BINDING_VECTOR)
	{
	  /* We could be redeclaring a global module entity, (from GMF
   	     or header unit), or from another partition, or
   	     specializing an imported template.  */
	  bitmap imports = get_import_bitmap ();
	  binding_cluster *cluster = BINDING_VECTOR_CLUSTER_BASE (*slot);

	  /* Scan the imported bindings.  */
	  unsigned ix = BINDING_VECTOR_NUM_CLUSTERS (*slot);
	  if (BINDING_VECTOR_SLOTS_PER_CLUSTER == BINDING_SLOTS_FIXED)
	    {
	      ix--;
	      cluster++;
	    }

	  /* Do this in forward order, so we load modules in an order
	     the user expects.  */
	  for (; ix--; cluster++)
	    for (unsigned jx = 0; jx != BINDING_VECTOR_SLOTS_PER_CLUSTER; jx++)
	      {
		/* Are we importing this module?  */
		if (unsigned base = cluster->indices[jx].base)
		  if (unsigned span = cluster->indices[jx].span)
		    do
		      if (bitmap_bit_p (imports, base))
			goto found;
		    while (++base, --span);
		continue;

	      found:;
		/* Is it loaded?  */
		if (cluster->slots[jx].is_lazy ())
		  {
		    gcc_assert (cluster->indices[jx].span == 1);
		    lazy_load_binding (cluster->indices[jx].base,
				       ns, name, &cluster->slots[jx]);
		  }
		tree bind = cluster->slots[jx];
		if (!bind)
		  /* Load errors could mean there's nothing here.  */
		  continue;

		/* Extract what we can see from here.  If there's no
		   stat_hack, then everything was exported.  */
		tree type = NULL_TREE;

		/* If no stat hack, everything is visible.  */
		if (STAT_HACK_P (bind))
		  {
		    if (STAT_TYPE_VISIBLE_P (bind))
		      type = STAT_TYPE (bind);
		    bind = STAT_VISIBLE (bind);
		  }

		if (type && qualify_lookup (type, LOOK_want::TYPE))
		  return strip_using_decl (type);

		if (bind && qualify_lookup (bind, LOOK_want::TYPE))
		  return strip_using_decl (bind);
	      }

	  if (!module_purview_p ())
	    {
	      /* We're in the global module, perhaps there's a tag
		 there?  */

	      /* FIXME: In general we should probably merge global module
		 classes in check_module_override rather than here, but for
		 GCC14 let's just fix lazy declarations of __class_type_info in
		 build_dynamic_cast_1.  */
	      if (current_namespace == abi_node)
		{
		  tree g = (BINDING_VECTOR_CLUSTER (*slot, 0)
			    .slots[BINDING_SLOT_GLOBAL]);
		  for (ovl_iterator iter (g); iter; ++iter)
		    if (qualify_lookup (*iter, LOOK_want::TYPE))
		      return *iter;
		}
	    }
	}
    }

  return NULL_TREE;
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
	  decl = push_template_decl (decl, is_friend);
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
		maybe_add_class_template_decl_list (current_class_type,
						    type, /*friend_p=*/0);
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

tree
pushtag (tree name, tree type, TAG_how how)
{
  tree decl;

  gcc_assert (identifier_p (name));

  auto_cond_timevar tv (TV_NAME_LOOKUP);

  cp_binding_level *b = current_binding_level;
  while (true)
    {
      if (/* Cleanup scopes are not scopes from the point of view of
	     the language.  */
	  b->kind == sk_cleanup
	  /* Neither are function parameter scopes.  */
	  || b->kind == sk_function_parms
	  /* Neither are the scopes used to hold template parameters
	     for an explicit specialization.  For an ordinary template
	     declaration, these scopes are not scopes from the point of
	     view of the language.  */
	  || (b->kind == sk_template_parms
	      && (b->explicit_spec_p || how == TAG_how::GLOBAL)))
	b = b->level_chain;
      else if (b->kind == sk_class && how != TAG_how::CURRENT_ONLY)
	{
	  b = b->level_chain;
	  if (b->kind == sk_template_parms)
	    b = b->level_chain;
	}
      else
	break;
    }

  /* Do C++ gratuitous typedefing.  */
  if (REAL_IDENTIFIER_TYPE_VALUE (name) != type)
    {
      tree tdef;
      tree context = TYPE_CONTEXT (type);

      if (! context)
	{
	  cp_binding_level *cb = b;
	  while (cb->kind != sk_namespace
		 && cb->kind != sk_class
		 && (cb->kind != sk_function_parms
		     || !cb->this_entity))
	    cb = cb->level_chain;
	  tree cs = cb->this_entity;

	  gcc_checking_assert (TREE_CODE (cs) == FUNCTION_DECL
			       ? cs == current_function_decl
			       : TYPE_P (cs) ? cs == current_class_type
			       : cs == current_namespace);

	  if (how == TAG_how::CURRENT_ONLY
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

      tdef = create_implicit_typedef (name, type);
      DECL_CONTEXT (tdef) = FROB_CONTEXT (context);
      set_originating_module (tdef);

      decl = maybe_process_template_type_declaration
	(type, how == TAG_how::HIDDEN_FRIEND, b);
      if (decl == error_mark_node)
	return decl;

      if (b->kind == sk_class)
	{
	  if (!TYPE_BEING_DEFINED (current_class_type))
	    /* Don't push anywhere if the class is complete; a lambda in an
	       NSDMI is not a member of the class.  */
	    ;
	  else if (!PROCESSING_REAL_TEMPLATE_DECL_P ())
	    /* Put this TYPE_DECL on the TYPE_FIELDS list for the
	       class.  But if it's a member template class, we want
	       the TEMPLATE_DECL, not the TYPE_DECL, so this is done
	       later.  */
	    finish_member_declaration (decl);
	  else
	    pushdecl_class_level (decl);
	}
      else if (b->kind == sk_template_parms)
	{
	  /* Do not push the tag here -- we'll want to push the
	     TEMPLATE_DECL.  */
	  if (b->level_chain->kind != sk_class)
	    set_identifier_type_value_with_scope (name, tdef, b->level_chain);
	}
      else
	{
	  decl = do_pushdecl_with_scope
	    (decl, b, /*hiding=*/(how == TAG_how::HIDDEN_FRIEND));
	  if (decl == error_mark_node)
	    return decl;

	  if (DECL_CONTEXT (decl) == std_node
	      && init_list_identifier == DECL_NAME (TYPE_NAME (type))
	      && !CLASSTYPE_TEMPLATE_INFO (type))
	    {
	      error ("declaration of %<std::initializer_list%> does not match "
		     "%<#include <initializer_list>%>, isn%'t a template");
	      return error_mark_node;
	    }
	}

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
	  /* Lambdas use LAMBDA_EXPR_DISCRIMINATOR instead.  */
	  else if (!LAMBDA_TYPE_P (type))
	    determine_local_discriminator (TYPE_NAME (type));
	}
    }

  if (b->kind == sk_class
      && !COMPLETE_TYPE_P (current_class_type))
    maybe_add_class_template_decl_list (current_class_type,
					type, /*friend_p=*/0);

  decl = TYPE_NAME (type);
  gcc_assert (TREE_CODE (decl) == TYPE_DECL);

  /* Set type visibility now if this is a forward declaration.  */
  TREE_PUBLIC (decl) = 1;
  determine_visibility (decl);
  check_module_decl_linkage (decl);

  return type;
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

  auto_cond_timevar tv (TV_NAME_LOOKUP);
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

void
push_to_top_level (void)
{
  struct saved_scope *s;
  cp_binding_level *b;
  cxx_saved_binding *sb;
  size_t i;
  bool need_pop;

  auto_cond_timevar tv (TV_NAME_LOOKUP);

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
  s->suppress_location_wrappers = suppress_location_wrappers;
  s->x_stmt_tree.stmts_are_full_exprs_p = true;

  scope_chain = s;
  current_function_decl = NULL_TREE;
  current_lang_base = NULL;
  current_lang_name = lang_name_cplusplus;
  current_namespace = global_namespace;
  push_class_stack ();
  cp_unevaluated_operand = 0;
  c_inhibit_evaluation_warnings = 0;
  suppress_location_wrappers = 0;
}

void
pop_from_top_level (void)
{
  struct saved_scope *s = scope_chain;
  cxx_saved_binding *saved;
  size_t i;

  auto_cond_timevar tv (TV_NAME_LOOKUP);

  pop_class_stack ();

  release_tree_vector (current_lang_base);

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
  suppress_location_wrappers = s->suppress_location_wrappers;

  /* Make this saved_scope structure available for reuse by
     push_to_top_level.  */
  s->prev = free_saved_scope;
  free_saved_scope = s;
}

namespace {

/* Helper class for saving/restoring relevant global flags for the
   function-local case of maybe_push_to_top_level.  */

struct local_state_t
{
  int cp_unevaluated_operand;
  int c_inhibit_evaluation_warnings;
  int cp_noexcept_operand_;

  static local_state_t
  save_and_clear ()
  {
    local_state_t s;
    s.cp_unevaluated_operand = ::cp_unevaluated_operand;
    ::cp_unevaluated_operand = 0;
    s.c_inhibit_evaluation_warnings = ::c_inhibit_evaluation_warnings;
    ::c_inhibit_evaluation_warnings = 0;
    s.cp_noexcept_operand_ = ::cp_noexcept_operand;
    ::cp_noexcept_operand = 0;
    return s;
  }

  void
  restore () const
  {
    ::cp_unevaluated_operand = this->cp_unevaluated_operand;
    ::c_inhibit_evaluation_warnings = this->c_inhibit_evaluation_warnings;
    ::cp_noexcept_operand = this->cp_noexcept_operand_;
  }
};

vec<local_state_t> local_state_stack;

} // anon namespace

/* Like push_to_top_level, but not if D is function-local.  Returns whether we
   did push to top.  */

bool
maybe_push_to_top_level (tree d)
{
  /* Push if D isn't function-local, or is a lambda function, for which name
     resolution is already done.  */
  const bool push_to_top
    = (LAMBDA_FUNCTION_P (d)
       || (TREE_CODE (d) == TYPE_DECL
	   && TREE_TYPE (d)
	   && LAMBDA_TYPE_P (TREE_TYPE (d)))
       || !current_function_decl
       || !decl_function_context (d));

  if (push_to_top)
    push_to_top_level ();
  else
    {
      gcc_assert (!processing_template_decl);
      push_function_context ();
      local_state_stack.safe_push (local_state_t::save_and_clear ());
    }

  return push_to_top;
}

/* Return from whatever maybe_push_to_top_level did.  */

void
maybe_pop_from_top_level (bool push_to_top)
{
  if (push_to_top)
    pop_from_top_level ();
  else
    {
      local_state_stack.pop ().restore ();
      pop_function_context ();
    }
}

/* Push into the scope of the namespace NS, even if it is deeply
   nested within another namespace.  */

void
push_nested_namespace (tree ns)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);
  if (ns == global_namespace)
    push_to_top_level ();
  else
    {
      push_nested_namespace (CP_DECL_CONTEXT (ns));
      resume_scope (NAMESPACE_LEVEL (ns));
      current_namespace = ns;
    }
}

/* Pop back from the scope of the namespace NS, which was previously
   entered with push_nested_namespace.  */

void
pop_nested_namespace (tree ns)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);
  while (ns != global_namespace)
    {
      ns = CP_DECL_CONTEXT (ns);
      current_namespace = ns;
      leave_scope ();
    }

  pop_from_top_level ();
}

/* Add TARGET to USINGS, if it does not already exist there.  We used
   to build the complete graph of usings at this point, from the POV
   of the source namespaces.  Now we build that as we perform the
   unqualified search.  */

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

/* Process a using directive.  */

void
finish_using_directive (tree target, tree attribs)
{
  if (target == error_mark_node)
    return;

  if (current_binding_level->kind != sk_namespace)
    add_stmt (build_stmt (input_location, USING_STMT, target));
  else
    emit_debug_info_using_namespace (current_binding_level->this_entity,
				     ORIGINAL_NAMESPACE (target), false);

  add_using_namespace (current_binding_level->using_directives,
		       ORIGINAL_NAMESPACE (target));

  bool diagnosed = false;
  if (attribs != error_mark_node)
    for (tree a = attribs; a; a = TREE_CHAIN (a))
      {
	tree name = get_attribute_name (a);
	if (current_binding_level->kind == sk_namespace
	    && is_attribute_p ("strong", name))
	  {
	    auto_diagnostic_group d;
	    if (warning (0, "%<strong%> using directive no longer supported")
		&& CP_DECL_CONTEXT (target) == current_namespace)
	      inform (DECL_SOURCE_LOCATION (target),
		      "you can use an inline namespace instead");
	  }
	else if ((flag_openmp || flag_openmp_simd)
		 && get_attribute_namespace (a) == omp_identifier
		 && (is_attribute_p ("directive", name)
		     || is_attribute_p ("sequence", name)
		     || is_attribute_p ("decl", name)))
	  {
	    if (!diagnosed)
	      {
		if (tree ar = TREE_VALUE (a))
		  {
		    tree d = TREE_VALUE (ar);
		    gcc_assert (TREE_CODE (d) == DEFERRED_PARSE);
		    error ("%<omp::%s%> not allowed to be specified in "
			   "this context",
			   TREE_PUBLIC (d) ? "decl" : "directive");
		  }
		else
		  error ("%<omp::%E%> not allowed to be specified in this "
			 "context", name);
		diagnosed = true;
	      }
	  }
	else if (!attribute_ignored_p (a))
	  warning (OPT_Wattributes, "%qD attribute directive ignored", name);
      }
}

/* Pushes X into the global namespace.  */

tree
pushdecl_top_level (tree x)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);
  push_to_top_level ();
  gcc_checking_assert (!DECL_CONTEXT (x));
  DECL_CONTEXT (x) = FROB_CONTEXT (global_namespace);
  x = pushdecl_namespace_level (x);
  pop_from_top_level ();
  return x;
}

/* Pushes X into the global namespace and calls cp_finish_decl to
   register the variable, initializing it with INIT.  */

tree
pushdecl_top_level_and_finish (tree x, tree init)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);
  push_to_top_level ();
  gcc_checking_assert (!DECL_CONTEXT (x));
  DECL_CONTEXT (x) = FROB_CONTEXT (global_namespace);
  x = pushdecl_namespace_level (x);
  cp_finish_decl (x, init, false, NULL_TREE, 0);
  pop_from_top_level ();
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

/* SLOT is the (possibly empty) binding slot for NAME in CTX.
   Reuse or create a namespace NAME.  NAME is null for the anonymous
   namespace.  */

static tree
reuse_namespace (tree *slot, tree ctx, tree name)
{
  if (modules_p () && *slot && TREE_PUBLIC (ctx) && name)
    {
      /* Public namespace.  Shared.  */
      tree *global_slot = slot;
      if (TREE_CODE (*slot) == BINDING_VECTOR)
	global_slot = get_fixed_binding_slot (slot, name,
					      BINDING_SLOT_GLOBAL, false);

      for (ovl_iterator iter (*global_slot); iter; ++iter)
	{
	  tree decl = *iter;

	  if (TREE_CODE (decl) == NAMESPACE_DECL && !DECL_NAMESPACE_ALIAS (decl))
	    return decl;
	}
    }
  return NULL_TREE;
}

static tree
make_namespace (tree ctx, tree name, location_t loc, bool inline_p)
{
  /* Create the namespace.  */
  tree ns = build_lang_decl (NAMESPACE_DECL, name, void_type_node);
  DECL_SOURCE_LOCATION (ns) = loc;
  SCOPE_DEPTH (ns) = SCOPE_DEPTH (ctx) + 1;
  if (!SCOPE_DEPTH (ns))
    /* We only allow depth 255. */
    sorry ("cannot nest more than %d namespaces", SCOPE_DEPTH (ctx));
  DECL_CONTEXT (ns) = FROB_CONTEXT (ctx);

  if (!name)
    /* Anon-namespaces in different header-unit imports are distinct.
       But that's ok as their contents all have internal linkage.
       (This is different to how they'd behave as textual includes,
       but doing this at all is really odd source.)  */
    SET_DECL_ASSEMBLER_NAME (ns, anon_identifier);
  else if (TREE_PUBLIC (ctx))
    TREE_PUBLIC (ns) = true;

  if (inline_p)
    DECL_NAMESPACE_INLINE_P (ns) = true;

  return ns;
}

/* NS was newly created, finish off making it.  */

static void
make_namespace_finish (tree ns, tree *slot, bool from_import = false)
{
  if (modules_p () && TREE_PUBLIC (ns) && (from_import || *slot != ns))
    {
      /* Merge into global slot.  */
      tree *gslot = get_fixed_binding_slot (slot, DECL_NAME (ns),
					    BINDING_SLOT_GLOBAL, true);
      *gslot = ns;
    }

  tree ctx = CP_DECL_CONTEXT (ns);
  cp_binding_level *scope = ggc_cleared_alloc<cp_binding_level> ();
  scope->this_entity = ns;
  scope->more_cleanups_ok = true;
  scope->kind = sk_namespace;
  scope->level_chain = NAMESPACE_LEVEL (ctx);
  NAMESPACE_LEVEL (ns) = scope;

  if (DECL_NAMESPACE_INLINE_P (ns))
    vec_safe_push (DECL_NAMESPACE_INLINEES (ctx), ns);

  if (DECL_NAMESPACE_INLINE_P (ns) || !DECL_NAME (ns))
    emit_debug_info_using_namespace (ctx, ns, true);

  /* An unnamed namespace implicitly has a using-directive inserted so
     that its contents are usable in the surrounding context.  */
  if (!DECL_NAMESPACE_INLINE_P (ns) && !DECL_NAME (ns))
    add_using_namespace (NAMESPACE_LEVEL (ctx)->using_directives, ns);
}

/* Push into the scope of the NAME namespace.  If NAME is NULL_TREE,
   then we enter an anonymous namespace.  If MAKE_INLINE is true, then
   we create an inline namespace (it is up to the caller to check upon
   redefinition). Return the number of namespaces entered.  */

int
push_namespace (tree name, bool make_inline)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);
  int count = 0;

  /* We should not get here if the global_namespace is not yet constructed
     nor if NAME designates the global namespace:  The global scope is
     constructed elsewhere.  */
  gcc_checking_assert (global_namespace != NULL && name != global_identifier);

  tree ns = NULL_TREE;
  {
    name_lookup lookup (name);
    if (!lookup.search_qualified (current_namespace, /*usings=*/false))
      ;
    else if (TREE_CODE (lookup.value) == TREE_LIST)
      {
	/* An ambiguous lookup.  If exactly one is a namespace, we
	   want that.  If more than one is a namespace, error, but
	   pick one of them.  */
	/* DR2061 can cause us to find multiple namespaces of the same
	   name.  We must treat that carefully and avoid thinking we
	   need to push a new (possibly) duplicate namespace.  Hey,
	   if you want to use the same identifier within an inline
	   nest, knock yourself out.  */
	for (tree *chain = &lookup.value, next; (next = *chain);)
	  {
	    tree decl = TREE_VALUE (next);
	    if (TREE_CODE (decl) == NAMESPACE_DECL)
	      {
		if (!ns)
		  ns = decl;
		else if (SCOPE_DEPTH (ns) >= SCOPE_DEPTH (decl))
		  ns = decl;

		/* Advance.  */
		chain = &TREE_CHAIN (next);
	      }
	    else
	      /* Stitch out.  */
	      *chain = TREE_CHAIN (next);
	  }

	if (TREE_CHAIN (lookup.value))
	  {
	    error ("%<namespace %E%> is ambiguous", name);
	    print_candidates (lookup.value);
	  }
      }
    else if (TREE_CODE (lookup.value) == NAMESPACE_DECL)
      ns = lookup.value;

    if (ns)
      if (tree dna = DECL_NAMESPACE_ALIAS (ns))
	{
	  /* A namespace alias is not allowed here, but if the alias
	     is for a namespace also inside the current scope,
	     accept it with a diagnostic.  That's better than dying
	     horribly.  */
	  if (is_nested_namespace (current_namespace, CP_DECL_CONTEXT (dna)))
	    {
	      error ("namespace alias %qD not allowed here, "
		     "assuming %qD", ns, dna);
	      ns = dna;
	    }
	  else
	    ns = NULL_TREE;
	}
  }

  if (ns)
    {
      /* DR2061.  NS might be a member of an inline namespace.  We
	 need to push into those namespaces.  */
      if (modules_p ())
	{
	  for (tree parent, ctx = ns; ctx != current_namespace;
	       ctx = parent)
	    {
	      parent = CP_DECL_CONTEXT (ctx);

	      tree bind = *find_namespace_slot (parent, DECL_NAME (ctx), false);
	      if (bind != ctx)
		{
		  auto &cluster = BINDING_VECTOR_CLUSTER (bind, 0);
		  binding_slot &slot = cluster.slots[BINDING_SLOT_CURRENT];
		  gcc_checking_assert (!(tree)slot || (tree)slot == ctx);
		  slot = ctx;
		}
	    }
	}

      count += push_inline_namespaces (CP_DECL_CONTEXT (ns));
      if (DECL_SOURCE_LOCATION (ns) == BUILTINS_LOCATION)
	/* It's not builtin now.  */
	DECL_SOURCE_LOCATION (ns) = input_location;
    }
  else
    {
      /* Before making a new namespace, see if we already have one in
	 the existing partitions of the current namespace.  */
      tree *slot = find_namespace_slot (current_namespace, name, false);
      if (slot)
	ns = reuse_namespace (slot, current_namespace, name);
      if (!ns)
	ns = make_namespace (current_namespace, name,
			     input_location, make_inline);

      if (pushdecl (ns) == error_mark_node)
	ns = NULL_TREE;
      else
	{
	  /* Finish up making the namespace.  */
	  add_decl_to_level (NAMESPACE_LEVEL (current_namespace), ns);
	  if (!slot)
	    {
	      slot = find_namespace_slot (current_namespace, name);
	      /* This should find the slot created by pushdecl.  */
	      gcc_checking_assert (slot && *slot == ns);
	    }
	  else
	    {
	      /* pushdecl could have expanded the hash table, so
		 slot might be invalid.  */
	      slot = find_namespace_slot (current_namespace, name);
	      gcc_checking_assert (slot);
	    }
	  make_namespace_finish (ns, slot);
	}
    }

  if (ns)
    {
      /* A public namespace is exported only if explicitly marked, or
	 it contains exported entities.  */
      if (module_exporting_p ())
	{
	  if (TREE_PUBLIC (ns))
	    DECL_MODULE_EXPORT_P (ns) = true;
	  else if (!header_module_p ())
	    {
	      if (name)
		{
		  auto_diagnostic_group d;
		  error_at (input_location, "exporting namespace %qD with "
			    "internal linkage", ns);
		  inform (input_location, "%qD has internal linkage because "
			  "it was declared in an unnamed namespace", ns);
		}
	      else
		error_at (input_location, "exporting unnamed namespace");
	    }
	}
      if (module_purview_p ())
	DECL_MODULE_PURVIEW_P (ns) = true;

      if (make_inline && !DECL_NAMESPACE_INLINE_P (ns))
	{
	  auto_diagnostic_group d;
	  error_at (input_location,
		    "inline namespace must be specified at initial definition");
	  inform (DECL_SOURCE_LOCATION (ns), "%qD defined here", ns);
	}
      resume_scope (NAMESPACE_LEVEL (ns));
      current_namespace = ns;
      count++;
    }

  return count;
}

/* Pop from the scope of the current namespace.  */

void
pop_namespace (void)
{
  auto_cond_timevar tv (TV_NAME_LOOKUP);

  gcc_assert (current_namespace != global_namespace);
  current_namespace = CP_DECL_CONTEXT (current_namespace);
  /* The binding level is not popped, as it might be re-opened later.  */
  leave_scope ();
}

/* An IMPORT is an import that is defining namespace NAME inside CTX.  Find or
   create that namespace and add it to the container's binding-vector.   */

tree
add_imported_namespace (tree ctx, tree name, location_t loc, unsigned import,
			bool inline_p, bool visible_p)
{
  // FIXME: Something is not correct about the VISIBLE_P handling.  We
  // need to insert this namespace into
  // (a) the GLOBAL or PARTITION slot, if it is TREE_PUBLIC
  // (b) The importing module's slot (always)
  // (c) Do we need to put it in the CURRENT slot?  This is the
  // confused piece.

  tree *slot = find_namespace_slot (ctx, name, true);
  tree decl = reuse_namespace (slot, ctx, name);

  /* Creating and binding.  */
  if (!decl)
    {
      decl = make_namespace (ctx, name, loc, inline_p);
      make_namespace_finish (decl, slot, true);
    }
  else if (DECL_NAMESPACE_INLINE_P (decl) != inline_p)
    {
      auto_diagnostic_group d;
      error_at (loc, "%s namespace %qD conflicts with reachable definition",
		inline_p ? "inline" : "non-inline", decl);
      inform (DECL_SOURCE_LOCATION (decl), "reachable %s definition here",
	      inline_p ? "non-inline" : "inline");
    }

  if (TREE_PUBLIC (decl) && TREE_CODE (*slot) == BINDING_VECTOR)
    {
      /* See if we can extend the final slot.  */
      binding_cluster *last = BINDING_VECTOR_CLUSTER_LAST (*slot);
      gcc_checking_assert (last->indices[0].span);
      unsigned jx = BINDING_VECTOR_SLOTS_PER_CLUSTER;

      while (--jx)
	if (last->indices[jx].span)
	  break;
      tree final = last->slots[jx];
      if (visible_p == !STAT_HACK_P (final)
	  && MAYBE_STAT_DECL (final) == decl
	  && last->indices[jx].base + last->indices[jx].span == import
	  && (BINDING_VECTOR_NUM_CLUSTERS (*slot) > 1
	      || (BINDING_VECTOR_SLOTS_PER_CLUSTER > BINDING_SLOTS_FIXED
		  && jx >= BINDING_SLOTS_FIXED)))
	{
	  last->indices[jx].span++;
	  return decl;
	}
    }

  /* Append a new slot.  */
  tree *mslot = &(tree &)*append_imported_binding_slot (slot, name, import);

  gcc_assert (!*mslot);
  *mslot = visible_p ? decl : stat_hack (decl, NULL_TREE);

  return decl;
}

/* Pop off extraneous binding levels left over due to syntax errors.
   We don't pop past namespaces, as they might be valid.  */

void
pop_everything (void)
{
  if (ENABLE_SCOPE_CHECKING)
    verbatim ("XXX entering %<pop_everything ()%>");
  while (!namespace_bindings_p ())
    {
      if (current_binding_level->kind == sk_class)
	pop_nested_class ();
      else
	poplevel (0, 0, 0);
    }
  if (ENABLE_SCOPE_CHECKING)
    verbatim ("XXX leaving %<pop_everything ()%>");
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

  /* Do not supply context to imported_module_or_decl, if
     it is a global namespace.  */
  if (context == global_namespace)
    context = NULL_TREE;

  t = MAYBE_BASELINK_FUNCTIONS (t);

  for (lkp_iterator iter (t); iter; ++iter)
    {
      tree fn = *iter;

      if (TREE_CODE (fn) == TEMPLATE_DECL)
	/* FIXME: Handle TEMPLATE_DECLs.  */
	continue;

      /* Ignore this FUNCTION_DECL if it refers to a builtin declaration
	 of a builtin function.  */
      if (TREE_CODE (fn) == FUNCTION_DECL
	  && DECL_EXTERNAL (fn)
	  && fndecl_built_in_p (fn))
	continue;

      if (building_stmt_list_p ())
	add_stmt (build_stmt (input_location, USING_STMT, fn));
      else
	debug_hooks->imported_module_or_decl (fn, NULL_TREE, context,
					      false, false);
    }
}

/* True if D is a local declaration in dependent scope.  Assumes that it is
   (part of) the current lookup result for its name.  */

bool
dependent_local_decl_p (tree d)
{
  if (!DECL_LOCAL_DECL_P (d))
    return false;

  cxx_binding *b = IDENTIFIER_BINDING (DECL_NAME (d));
  cp_binding_level *l = b->scope;
  while (!l->this_entity)
    l = l->level_chain;
  return uses_template_parms (l->this_entity);
}



#include "gt-cp-name-lookup.h"
