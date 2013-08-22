/* Basic IPA utilities for type inheritance graph construction and
   devirtualization.
   Copyright (C) 2013 Free Software Foundation, Inc.
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

/* Brief vocalburary:
     ODR = One Definition Rule
        In short, the ODR states that:
	1 In any translation unit, a template, type, function, or object can
	  have no more than one definition. Some of these can have any number
	  of declarations. A definition provides an instance.
        2 In the entire program, an object or non-inline function cannot have
	  more than one definition; if an object or function is used, it must
	  have exactly one definition. You can declare an object or function
	  that is never used, in which case you don't have to provide
	  a definition. In no event can there be more than one definition.
        3 Some things, like types, templates, and extern inline functions, can
	  be defined in more than one translation unit. For a given entity,
	  each definition must be the same. Non-extern objects and functions
	  in different translation units are different entities, even if their
	  names and types are the same.

     OTR = OBJ_TYPE_REF
       This is the Gimple representation of type information of a polymorphic call.
       It contains two parameters:
	 otr_type is a type of class whose method is called.
	 otr_token is the index into virtual table where address is taken.

     BINFO
       This is the type inheritance information attached to each tree
       RECORD_TYPE by the C++ frotend.  It provides information about base
       types and virtual tables.

       BINFO is linked to the RECORD_TYPE by TYPE_BINFO.
       BINFO also links to its type by BINFO_TYPE and to the virtual table by
       BINFO_VTABLE.

       Base types of a given type are enumerated by BINFO_BASE_BINFO
       vector.  Members of this vectors are not BINFOs associated
       with a base type.  Rather they are new copies of BINFOs
       (base BINFOs). Their virtual tables may differ from
       virtual table of the base type.  Also BINFO_OFFSET specifies
       offset of the base within the type.

       In the case of single inheritance, the virtual table is shared
       and BINFO_VTABLE of base BINFO is NULL.  In the case of multiple
       inheritance the individual virtual tables are pointer to by
       BINFO_VTABLE of base binfos (that differs of BINFO_VTABLE of 
       binfo associated to the base type).

       BINFO lookup for a given base type and offset can be done by
       get_binfo_at_offset.  It returns proper BINFO whose virtual table
       can be used for lookup of virtual methods associated with the
       base type.

     token
       This is an index of virtual method in virtual table associated
       to the type defining it. Token can be looked up from OBJ_TYPE_REF
       or from DECL_VINDEX of a given virtual table.

     polymorphic (indirect) call
       This is callgraph represention of virtual method call.  Every
       polymorphic call contains otr_type and otr_token taken from
       original OBJ_TYPE_REF at callgraph construction time.

   What we do here:

   build_type_inheritance_graph triggers a construction of the type inheritance
   graph.

     We reconstruct it based on types of methods we see in the unit.
     This means that the graph is not complete. Types with no methods are not
     inserted into the graph.  Also types without virtual methods are not
     represented at all, though it may be easy to add this.
  
     The inheritance graph is represented as follows:

       Vertices are structures odr_type.  Every odr_type may correspond
       to one or more tree type nodes that are equivalent by ODR rule.
       (the multiple type nodes appear only with linktime optimization)

       Edges are represented by odr_type->base and odr_type->derived_types.
       At the moment we do not track offsets of types for multiple inheritance.
       Adding this is easy.

  possible_polymorphic_call_targets returns, given an parameters found in
  indirect polymorphic edge all possible polymorphic call targets of the call.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "ggc.h"
#include "pointer-set.h"
#include "target.h"
#include "hash-table.h"
#include "tree-pretty-print.h"
#include "ipa-utils.h"
#include "gimple.h"

/* Pointer set of all call targets appearing in the cache.  */
static pointer_set_t *cached_polymorphic_call_targets;

/* The node of type inheritance graph.  For each type unique in
   One Defintion Rule (ODR) sense, we produce one node linking all 
   main variants of types equivalent to it, bases and derived types.  */

struct GTY(()) odr_type_d
{
  /* leader type.  */
  tree type;
  /* All bases.  */
  vec<odr_type> GTY((skip)) bases;
  /* All derrived types with virtual methods seen in unit.  */
  vec<odr_type> GTY((skip)) derived_types;

  /* Unique ID indexing the type in odr_types array.  */
  int id;
  /* Is it in anonymous namespace? */
  bool anonymous_namespace;
};


/* Return true if BINFO corresponds to a type with virtual methods. 

   Every type has several BINFOs.  One is the BINFO associated by the type
   while other represents bases of derived types.  The BINFOs representing
   bases do not have BINFO_VTABLE pointer set when this is the single
   inheritance (because vtables are shared).  Look up the BINFO of type
   and check presence of its vtable.  */

static inline bool
polymorphic_type_binfo_p (tree binfo)
{
  /* See if BINFO's type has an virtual table associtated with it.  */
  return BINFO_VTABLE (TYPE_BINFO (BINFO_TYPE (binfo)));
}

/* One Definition Rule hashtable helpers.  */

struct odr_hasher 
{
  typedef odr_type_d value_type;
  typedef union tree_node compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
  static inline void remove (value_type *);
};

/* Produce hash based on type name.  */

hashval_t
hash_type_name (tree t)
{
  gcc_checking_assert (TYPE_MAIN_VARIANT (t) == t);

  /* If not in LTO, all main variants are unique, so we can do
     pointer hash.  */
  if (!in_lto_p)
    return htab_hash_pointer (t);

  /* Anonymous types are unique.  */
  if (type_in_anonymous_namespace_p (t))
    return htab_hash_pointer (t);

  /* Rest is not implemented yet.  */
  gcc_unreachable ();
}

/* Return the computed hashcode for ODR_TYPE.  */

inline hashval_t
odr_hasher::hash (const value_type *odr_type)
{
  return hash_type_name (odr_type->type);
}

/* Compare types T1 and T2 and return true if they are
   equivalent.  */

inline bool
odr_hasher::equal (const value_type *t1, const compare_type *ct2)
{
  tree t2 = const_cast <tree> (ct2);

  gcc_checking_assert (TYPE_MAIN_VARIANT (ct2) == ct2);
  if (t1->type == t2)
    return true;
  if (!in_lto_p)
    return false;
  return types_same_for_odr (t1->type, t2);
}

/* Free ODR type V.  */

inline void
odr_hasher::remove (value_type *v)
{
  v->bases.release ();
  v->derived_types.release ();
  ggc_free (v);
}

/* ODR type hash used to lookup ODR type based on tree type node.  */

typedef hash_table <odr_hasher> odr_hash_type;
static odr_hash_type odr_hash;

/* ODR types are also stored into ODR_TYPE vector to allow consistent
   walking.  Bases appear before derived types.  Vector is garbage collected
   so we won't end up visiting empty types.  */

static GTY(()) vec <odr_type, va_gc> *odr_types_ptr;
#define odr_types (*odr_types_ptr)

/* Get ODR type hash entry for TYPE.  If INSERT is true, create
   possibly new entry.  */

odr_type
get_odr_type (tree type, bool insert)
{
  odr_type_d **slot;
  odr_type val;
  hashval_t hash;

  type = TYPE_MAIN_VARIANT (type);
  gcc_checking_assert (TYPE_MAIN_VARIANT (type) == type);
  hash = hash_type_name (type);
  slot = odr_hash.find_slot_with_hash (type, hash, insert ? INSERT : NO_INSERT);
  if (!slot)
    return NULL;

  /* See if we already have entry for type.  */
  if (*slot)
    {
      val = *slot;

      /* With LTO we will need to support multiple tree representation of
	 the same ODR type.  For now we ignore this.  */
      if (val->type == type)
	return val;
      gcc_unreachable ();
    }
  else
    {
      tree binfo = TYPE_BINFO (type);
      unsigned int i;

      val = ggc_alloc_cleared_odr_type_d ();
      val->type = type;
      val->bases = vNULL;
      val->derived_types = vNULL;
      val->anonymous_namespace = type_in_anonymous_namespace_p (type);
      *slot = val;
      for (i = 0; i < BINFO_N_BASE_BINFOS (binfo); i++)
	/* For now record only polymorphic types. other are
	   pointless for devirtualization and we can not precisely
	   determine ODR equivalency of these during LTO.  */
	if (polymorphic_type_binfo_p (BINFO_BASE_BINFO (binfo, i)))
	  {
	    odr_type base = get_odr_type (BINFO_TYPE (BINFO_BASE_BINFO (binfo,
									i)),
					  true);
	    base->derived_types.safe_push (val);
	    val->bases.safe_push (base);
	  }
      /* First record bases, then add into array so ids are increasing.  */
      if (odr_types_ptr)
        val->id = odr_types.length();
      vec_safe_push (odr_types_ptr, val);
    }
  return val;
}

/* Dump ODR type T and all its derrived type.  INDENT specify indentation for
   recusive printing.  */

static void
dump_odr_type (FILE *f, odr_type t, int indent=0)
{
  unsigned int i;
  fprintf (f, "%*s type %i: ", indent * 2, "", t->id);
  print_generic_expr (f, t->type, TDF_SLIM);
  fprintf (f, "%s\n", t->anonymous_namespace ? " (anonymous namespace)":"");
  if (TYPE_NAME (t->type))
    {
      fprintf (f, "%*s defined at: %s:%i\n", indent * 2, "",
	       DECL_SOURCE_FILE (TYPE_NAME (t->type)),
	       DECL_SOURCE_LINE (TYPE_NAME (t->type)));
    }
  if (t->bases.length())
    {
      fprintf (f, "%*s base odr type ids: ", indent * 2, "");
      for (i = 0; i < t->bases.length(); i++)
	fprintf (f, " %i", t->bases[i]->id);
      fprintf (f, "\n");
    }
  if (t->derived_types.length())
    {
      fprintf (f, "%*s derived types:\n", indent * 2, "");
      for (i = 0; i < t->derived_types.length(); i++)
        dump_odr_type (f, t->derived_types[i], indent + 1);
    }
  fprintf (f, "\n");
}

/* Dump the type inheritance graph.  */

static void
dump_type_inheritance_graph (FILE *f)
{
  unsigned int i;
  if (!odr_types_ptr)
    return;
  fprintf (f, "\n\nType inheritance graph:\n");
  for (i = 0; i < odr_types.length(); i++)
    {
      if (odr_types[i]->bases.length() == 0)
	dump_odr_type (f, odr_types[i]);
    }
}

/* Given method type T, return type of class it belongs to.
   Lookup this pointer and get its type.    */

static tree
method_class_type (tree t)
{
  tree first_parm_type = TREE_VALUE (TYPE_ARG_TYPES (t));

  return TREE_TYPE (first_parm_type);
}

/* Initialize IPA devirt and build inheritance tree graph.  */

void
build_type_inheritance_graph (void)
{
  struct cgraph_node *n;
  FILE *inheritance_dump_file;
  int flags;

  if (odr_hash.is_created ())
    return;
  timevar_push (TV_IPA_INHERITANCE);
  inheritance_dump_file = dump_begin (TDI_inheritance, &flags);
  odr_hash.create (23);

  /* We reconstruct the graph starting of types of all methods seen in the
     the unit.  */
  FOR_EACH_FUNCTION (n)
    if (DECL_VIRTUAL_P (n->symbol.decl)
	&& symtab_real_symbol_p ((symtab_node)n))
      get_odr_type (method_class_type (TREE_TYPE (n->symbol.decl)), true);
  if (inheritance_dump_file)
    {
      dump_type_inheritance_graph (inheritance_dump_file);
      dump_end (TDI_inheritance, inheritance_dump_file);
    }
  timevar_pop (TV_IPA_INHERITANCE);
}

/* If TARGET has associated node, record it in the NODES array.  */

static void
maybe_record_node (vec <cgraph_node *> &nodes,
		   tree target, pointer_set_t *inserted)
{
  struct cgraph_node *target_node;
  enum built_in_function fcode;

  if (target
      /* Those are used to mark impossible scenarios.  */
      && (fcode = DECL_FUNCTION_CODE (target))
	  != BUILT_IN_UNREACHABLE
      && fcode != BUILT_IN_TRAP
      && !pointer_set_insert (inserted, target)
      && (target_node = cgraph_get_node (target)) != NULL
      && symtab_real_symbol_p ((symtab_node)target_node))
    {
      pointer_set_insert (cached_polymorphic_call_targets,
			  target_node);
      nodes.safe_push (target_node);
    }
}

/* See if BINFO's type match OTR_TYPE.  If so, lookup method
   in vtable of TYPE_BINFO and insert method to NODES array.
   Otherwise recurse to base BINFOs.
   This match what get_binfo_at_offset does, but with offset
   being unknown.

   TYPE_BINFO is binfo holding an virtual table matching
   BINFO's type.  In the case of single inheritance, this
   is binfo of BINFO's type ancestor (vtable is shared),
   otherwise it is binfo of BINFO's type.

   MATCHED_VTABLES tracks virtual tables we already did lookup
   for virtual function in.
  */

static void
record_binfo (vec <cgraph_node *> &nodes,
	      tree binfo,
	      tree otr_type,
	      tree type_binfo,
	      HOST_WIDE_INT otr_token,
	      pointer_set_t *inserted,
	      pointer_set_t *matched_vtables)
{
  tree type = BINFO_TYPE (binfo);
  int i;
  tree base_binfo;

  gcc_checking_assert (BINFO_VTABLE (type_binfo));

  if (types_same_for_odr (type, otr_type)
      && !pointer_set_insert (matched_vtables, BINFO_VTABLE (type_binfo)))
    {
      tree target = gimple_get_virt_method_for_binfo (otr_token, type_binfo);
      if (target)
	maybe_record_node (nodes, target, inserted);
      return;
    }

  /* Walk bases.  */
  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    /* Walking bases that have no virtual method is pointless excercise.  */
    if (polymorphic_type_binfo_p (base_binfo))
      record_binfo (nodes, base_binfo, otr_type,
		    /* In the case of single inheritance, the virtual table
		       is shared with the outer type.  */
		    BINFO_VTABLE (base_binfo) ? base_binfo : type_binfo,
		    otr_token, inserted,
		    matched_vtables);
}
     
/* Lookup virtual methods matching OTR_TYPE (with OFFSET and OTR_TOKEN)
   of TYPE, insert them to NODES, recurse into derived nodes. 
   INSERTED is used to avoid duplicate insertions of methods into NODES.
   MATCHED_VTABLES are used to avoid duplicate walking vtables.  */

static void
possible_polymorphic_call_targets_1 (vec <cgraph_node *> &nodes,
				     pointer_set_t *inserted,
				     pointer_set_t *matched_vtables,
				     tree otr_type,
				     odr_type type,
				     HOST_WIDE_INT otr_token)
{
  tree binfo = TYPE_BINFO (type->type);
  unsigned int i;

  record_binfo (nodes, binfo, otr_type, binfo, otr_token, inserted,
	        matched_vtables);
  for (i = 0; i < type->derived_types.length(); i++)
    possible_polymorphic_call_targets_1 (nodes, inserted, 
					 matched_vtables,
					 otr_type,
					 type->derived_types[i],
					 otr_token);
}

/* Cache of queries for polymorphic call targets.

   Enumerating all call targets may get expensive when there are many
   polymorphic calls in the program, so we memoize all the previous
   queries and avoid duplicated work.  */

struct polymorphic_call_target_d
{
  odr_type type;
  HOST_WIDE_INT otr_token;
  vec <cgraph_node *> targets;
};

/* Polymorphic call target cache helpers.  */

struct polymorphic_call_target_hasher 
{
  typedef polymorphic_call_target_d value_type;
  typedef polymorphic_call_target_d compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
  static inline void remove (value_type *);
};

/* Return the computed hashcode for ODR_QUERY.  */

inline hashval_t
polymorphic_call_target_hasher::hash (const value_type *odr_query)
{
  return iterative_hash_hashval_t (odr_query->type->id,
				   odr_query->otr_token);
}

/* Compare cache entries T1 and T2.  */

inline bool
polymorphic_call_target_hasher::equal (const value_type *t1,
				       const compare_type *t2)
{
  return t1->type == t2->type && t1->otr_token == t2->otr_token;
}

/* Remove entry in polymorphic call target cache hash.  */

inline void
polymorphic_call_target_hasher::remove (value_type *v)
{
  v->targets.release ();
  free (v);
}

/* Polymorphic call target query cache.  */

typedef hash_table <polymorphic_call_target_hasher>
   polymorphic_call_target_hash_type;
static polymorphic_call_target_hash_type polymorphic_call_target_hash;

/* Destroy polymorphic call target query cache.  */

static void
free_polymorphic_call_targets_hash ()
{
  if (cached_polymorphic_call_targets)
    {
      polymorphic_call_target_hash.dispose ();
      pointer_set_destroy (cached_polymorphic_call_targets);
      cached_polymorphic_call_targets = NULL;
    }
}

/* When virtual function is removed, we may need to flush the cache.  */

static void
devirt_node_removal_hook (struct cgraph_node *n, void *d ATTRIBUTE_UNUSED)
{
  if (cached_polymorphic_call_targets
      && pointer_set_contains (cached_polymorphic_call_targets, n))
    free_polymorphic_call_targets_hash ();
}

/* Return vector containing possible targets of polymorphic call of type
   OTR_TYPE caling method OTR_TOKEN with OFFSET.  If FINALp is non-NULL,
   store true if the list is complette. 
   CACHE_TOKEN (if non-NULL) will get stored to an unique ID of entry
   in the target cache.  If user needs to visit every target list
   just once, it can memoize them.

   Returned vector is placed into cache.  It is NOT caller's responsibility
   to free it.  The vector can be freed on cgraph_remove_node call if
   the particular node is a virtual function present in the cache.  */

vec <cgraph_node *>
possible_polymorphic_call_targets (tree otr_type,
			           HOST_WIDE_INT otr_token,
			           bool *finalp,
			           void **cache_token)
{
  static struct cgraph_node_hook_list *node_removal_hook_holder;
  pointer_set_t *inserted;
  pointer_set_t *matched_vtables;
  vec <cgraph_node *> nodes=vNULL;
  odr_type type;
  polymorphic_call_target_d key;
  polymorphic_call_target_d **slot;
  unsigned int i;
  tree binfo, target;

  if (finalp)
    *finalp = false;

  type = get_odr_type (otr_type, false);
  /* If we do not have type in our hash it means we never seen any method
     in it.  */
  if (!type)
    return nodes;

  /* For anonymous namespace types we can attempt to build full type.
     All derivations must be in this unit.  */
  if (type->anonymous_namespace && finalp && !flag_ltrans)
    *finalp = true;

  /* Initialize query cache.  */
  if (!cached_polymorphic_call_targets)
    {
      cached_polymorphic_call_targets = pointer_set_create ();
      polymorphic_call_target_hash.create (23);
      if (!node_removal_hook_holder)
	node_removal_hook_holder =
	  cgraph_add_node_removal_hook (&devirt_node_removal_hook, NULL);
    }

  /* Lookup cached answer.  */
  key.type = type;
  key.otr_token = otr_token;
  slot = polymorphic_call_target_hash.find_slot (&key, INSERT);
  if (cache_token)
   *cache_token = (void *)*slot;
  if (*slot)
    return (*slot)->targets;

  /* Do actual search.  */
  timevar_push (TV_IPA_VIRTUAL_CALL);
  *slot = XCNEW (polymorphic_call_target_d);
  if (cache_token)
   *cache_token = (void *)*slot;
  (*slot)->type = type;
  (*slot)->otr_token = otr_token;

  inserted = pointer_set_create ();
  matched_vtables = pointer_set_create ();

  /* First see virtual method of type itself.  */

  binfo = TYPE_BINFO (type->type);
  target = gimple_get_virt_method_for_binfo (otr_token, binfo);
  if (target)
    maybe_record_node (nodes, target, inserted);
  pointer_set_insert (matched_vtables, BINFO_VTABLE (binfo));

  /* TODO: If method is final, we can stop here and signaize that
     list is final.  We need C++ FE to pass our info about final
     methods and classes.  */

  /* Walk recursively all derived types.  Here we need to lookup proper basetype
     via their BINFO walk that is done by record_binfo  */
  for (i = 0; i < type->derived_types.length(); i++)
    possible_polymorphic_call_targets_1 (nodes, inserted,
					 matched_vtables,
					 otr_type, type->derived_types[i],
					 otr_token);
  (*slot)->targets = nodes;

  pointer_set_destroy (inserted);
  pointer_set_destroy (matched_vtables);
  timevar_pop (TV_IPA_VIRTUAL_CALL);
  return nodes;
}

/* Dump all possible targets of a polymorphic call.  */

void
dump_possible_polymorphic_call_targets (FILE *f,
				    tree otr_type,
				    HOST_WIDE_INT otr_token)
{
  vec <cgraph_node *> targets;
  bool final;
  odr_type type = get_odr_type (otr_type, false);
  unsigned int i;

  if (!type)
    return;
  targets = possible_polymorphic_call_targets (otr_type, otr_token,
					       &final);
  fprintf (f, "Targets of polymorphic call of type %i ", type->id);
  print_generic_expr (f, type->type, TDF_SLIM);
  fprintf (f, " token %i%s:",
	   (int)otr_token,
	   final ? " (full list)" : " (partial list, may call to other unit)");
  for (i = 0; i < targets.length (); i++)
    fprintf (f, " %s/%i", cgraph_node_name (targets[i]),
	     targets[i]->symbol.order);
  fprintf (f, "\n");
}


/* Return true if N can be possibly target of a polymorphic call of
   OTR_TYPE/OTR_TOKEN.  */

bool
possible_polymorphic_call_target_p (tree otr_type,
				    HOST_WIDE_INT otr_token,
				    struct cgraph_node *n)
{
  vec <cgraph_node *> targets;
  unsigned int i;

  if (!odr_hash.is_created ())
    return true;
  targets = possible_polymorphic_call_targets (otr_type, otr_token);
  for (i = 0; i < targets.length (); i++)
    if (n == targets[i])
      return true;
  return false;
}


/* After callgraph construction new external nodes may appear.
   Add them into the graph.  */

void
update_type_inheritance_graph (void)
{
  struct cgraph_node *n;

  if (!odr_hash.is_created ())
    return;
  free_polymorphic_call_targets_hash ();
  timevar_push (TV_IPA_INHERITANCE);
  /* We reconstruct the graph starting of types of all methods seen in the
     the unit.  */
  FOR_EACH_FUNCTION (n)
    if (DECL_VIRTUAL_P (n->symbol.decl)
	&& !n->symbol.definition
	&& symtab_real_symbol_p ((symtab_node)n))
      get_odr_type (method_class_type (TREE_TYPE (n->symbol.decl)), true);
  timevar_pop (TV_IPA_INHERITANCE);
}
#include "gt-ipa-devirt.h"
