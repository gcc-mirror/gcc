/* Basic IPA utilities for type inheritance graph construction and
   devirtualization.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.
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

  pass_ipa_devirt performs simple speculative devirtualization.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "print-tree.h"
#include "calls.h"
#include "cgraph.h"
#include "expr.h"
#include "tree-pass.h"
#include "hash-set.h"
#include "target.h"
#include "hash-table.h"
#include "inchash.h"
#include "tree-pretty-print.h"
#include "ipa-utils.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "ipa-inline.h"
#include "diagnostic.h"
#include "tree-dfa.h"
#include "demangle.h"
#include "dbgcnt.h"
#include "gimple-pretty-print.h"
#include "stor-layout.h"
#include "intl.h"
#include "hash-map.h"

/* Hash based set of pairs of types.  */
typedef struct
{
  tree first;
  tree second;
} type_pair;

struct pair_traits : default_hashset_traits
{
  static hashval_t
  hash (type_pair p)
  {
    return TYPE_UID (p.first) ^ TYPE_UID (p.second);
  }
  static bool
  is_empty (type_pair p)
  {
    return p.first == NULL;
  }
  static bool
  is_deleted (type_pair p ATTRIBUTE_UNUSED)
    {
      return false;
    }
  static bool
  equal (const type_pair &a, const type_pair &b)
    {
      return a.first==b.first && a.second == b.second;
    }
  static void
  mark_empty (type_pair &e)
    {
      e.first = NULL;
    }
};

static bool odr_types_equivalent_p (tree, tree, bool, bool *,
				    hash_set<type_pair,pair_traits> *);

static bool odr_violation_reported = false;


/* Pointer set of all call targets appearing in the cache.  */
static hash_set<cgraph_node *> *cached_polymorphic_call_targets;

/* The node of type inheritance graph.  For each type unique in
   One Defintion Rule (ODR) sense, we produce one node linking all 
   main variants of types equivalent to it, bases and derived types.  */

struct GTY(()) odr_type_d
{
  /* leader type.  */
  tree type;
  /* All bases; built only for main variants of types  */
  vec<odr_type> GTY((skip)) bases;
  /* All derrived types with virtual methods seen in unit;
     built only for main variants oftypes  */
  vec<odr_type> GTY((skip)) derived_types;

  /* All equivalent types, if more than one.  */
  vec<tree, va_gc> *types;
  /* Set of all equivalent types, if NON-NULL.  */
  hash_set<tree> * GTY((skip)) types_set;

  /* Unique ID indexing the type in odr_types array.  */
  int id;
  /* Is it in anonymous namespace? */
  bool anonymous_namespace;
  /* Do we know about all derivations of given type?  */
  bool all_derivations_known;
  /* Did we report ODR violation here?  */
  bool odr_violated;
};

static bool contains_type_p (tree, HOST_WIDE_INT, tree);


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

/* Return TRUE if all derived types of T are known and thus
   we may consider the walk of derived type complete.

   This is typically true only for final anonymous namespace types and types
   defined within functions (that may be COMDAT and thus shared across units,
   but with the same set of derived types).  */

static bool
type_all_derivations_known_p (tree t)
{
  if (TYPE_FINAL_P (t))
    return true;
  if (flag_ltrans)
    return false;
  if (type_in_anonymous_namespace_p (t))
    return true;
  return (decl_function_context (TYPE_NAME (t)) != NULL);
}

/* Return TURE if type's constructors are all visible.  */

static bool
type_all_ctors_visible_p (tree t)
{
  return !flag_ltrans
	 && symtab->state >= CONSTRUCTION
	 /* We can not always use type_all_derivations_known_p.
	    For function local types we must assume case where
	    the function is COMDAT and shared in between units. 

	    TODO: These cases are quite easy to get, but we need
	    to keep track of C++ privatizing via -Wno-weak
	    as well as the  IPA privatizing.  */
	 && type_in_anonymous_namespace_p (t);
}

/* Return TRUE if type may have instance.  */

static bool
type_possibly_instantiated_p (tree t)
{
  tree vtable;
  varpool_node *vnode;

  /* TODO: Add abstract types here.  */
  if (!type_all_ctors_visible_p (t))
    return true;

  vtable = BINFO_VTABLE (TYPE_BINFO (t));
  if (TREE_CODE (vtable) == POINTER_PLUS_EXPR)
    vtable = TREE_OPERAND (TREE_OPERAND (vtable, 0), 0);
  vnode = varpool_node::get (vtable);
  return vnode && vnode->definition;
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

/* Return type that was declared with T's name so that T is an
   qualified variant of it.  */

static inline tree
main_odr_variant (const_tree t)
{
  if (TYPE_NAME (t) && TREE_CODE (TYPE_NAME (t)) == TYPE_DECL)
    return TREE_TYPE (TYPE_NAME (t));
  /* Unnamed types and non-C++ produced types can be compared by variants.  */
  else
    return TYPE_MAIN_VARIANT (t);
}

/* Produce hash based on type name.  */

static hashval_t
hash_type_name (tree t)
{
  gcc_checking_assert (main_odr_variant (t) == t);

  /* If not in LTO, all main variants are unique, so we can do
     pointer hash.  */
  if (!in_lto_p)
    return htab_hash_pointer (t);

  /* Anonymous types are unique.  */
  if (type_in_anonymous_namespace_p (t))
    return htab_hash_pointer (t);

  /* ODR types have name specified.  */
  if (TYPE_NAME (t)
      && DECL_ASSEMBLER_NAME_SET_P (TYPE_NAME (t)))
    return IDENTIFIER_HASH_VALUE (DECL_ASSEMBLER_NAME (TYPE_NAME (t)));

  /* For polymorphic types that was compiled with -fno-lto-odr-type-merging
     we can simply hash the virtual table.  */
  if (TREE_CODE (t) == RECORD_TYPE
      && TYPE_BINFO (t) && BINFO_VTABLE (TYPE_BINFO (t)))
    {
      tree v = BINFO_VTABLE (TYPE_BINFO (t));
      hashval_t hash = 0;

      if (TREE_CODE (v) == POINTER_PLUS_EXPR)
	{
	  hash = TREE_INT_CST_LOW (TREE_OPERAND (v, 1));
	  v = TREE_OPERAND (TREE_OPERAND (v, 0), 0);
	}

      v = DECL_ASSEMBLER_NAME (v);
      hash = iterative_hash_hashval_t (hash, htab_hash_pointer (v));
      return hash;
    }

  /* Builtin types may appear as main variants of ODR types and are unique.
     Sanity check we do not get anything that looks non-builtin.  */
  gcc_checking_assert (TREE_CODE (t) == INTEGER_TYPE
		       || TREE_CODE (t) == VOID_TYPE
		       || TREE_CODE (t) == COMPLEX_TYPE
		       || TREE_CODE (t) == REAL_TYPE
		       || TREE_CODE (t) == POINTER_TYPE);
  return htab_hash_pointer (t);
}

/* Return the computed hashcode for ODR_TYPE.  */

inline hashval_t
odr_hasher::hash (const value_type *odr_type)
{
  return hash_type_name (odr_type->type);
}

/* For languages with One Definition Rule, work out if
   types are the same based on their name.
 
   This is non-trivial for LTO where minnor differences in
   the type representation may have prevented type merging
   to merge two copies of otherwise equivalent type.

   Until we start streaming mangled type names, this function works
   only for polymorphic types.  */

bool
types_same_for_odr (const_tree type1, const_tree type2)
{
  gcc_checking_assert (TYPE_P (type1) && TYPE_P (type2));

  type1 = main_odr_variant (type1);
  type2 = main_odr_variant (type2);

  if (type1 == type2)
    return true;

  if (!in_lto_p)
    return false;

  /* Check for anonymous namespaces. Those have !TREE_PUBLIC
     on the corresponding TYPE_STUB_DECL.  */
  if (type_in_anonymous_namespace_p (type1)
      || type_in_anonymous_namespace_p (type2))
    return false;


  /* ODR name of the type is set in DECL_ASSEMBLER_NAME of its TYPE_NAME.

     Ideally we should never meed types without ODR names here.  It can however
     happen in two cases:

       1) for builtin types that are not streamed but rebuilt in lto/lto-lang.c
          Here testing for equivalence is safe, since their MAIN_VARIANTs are
          unique.
       2) for units streamed with -fno-lto-odr-type-merging.  Here we can't
	  establish precise ODR equivalency, but for correctness we care only
	  about equivalency on complete polymorphic types.  For these we can
	  compare assembler names of their virtual tables.  */
  if ((!TYPE_NAME (type1) || !DECL_ASSEMBLER_NAME_SET_P (TYPE_NAME (type1)))
      || (!TYPE_NAME (type2) || !DECL_ASSEMBLER_NAME_SET_P (TYPE_NAME (type2))))
    {
      /* See if types are obvoiusly different (i.e. different codes
	 or polymorphis wrt non-polymorphic).  This is not strictly correct
	 for ODR violating programs, but we can't do better without streaming
	 ODR names.  */
      if (TREE_CODE (type1) != TREE_CODE (type2))
	return false;
      if (TREE_CODE (type1) == RECORD_TYPE
	  && (TYPE_BINFO (type1) == NULL_TREE) != (TYPE_BINFO (type1) == NULL_TREE))
	return false;
      if (TREE_CODE (type1) == RECORD_TYPE && TYPE_BINFO (type1)
	  && (BINFO_VTABLE (TYPE_BINFO (type1)) == NULL_TREE)
	     != (BINFO_VTABLE (TYPE_BINFO (type2)) == NULL_TREE))
	return false;

      /* At the moment we have no way to establish ODR equivlaence at LTO
	 other than comparing virtual table pointrs of polymorphic types.
	 Eventually we should start saving mangled names in TYPE_NAME.
	 Then this condition will become non-trivial.  */

      if (TREE_CODE (type1) == RECORD_TYPE
	  && TYPE_BINFO (type1) && TYPE_BINFO (type2)
	  && BINFO_VTABLE (TYPE_BINFO (type1))
	  && BINFO_VTABLE (TYPE_BINFO (type2)))
	{
	  tree v1 = BINFO_VTABLE (TYPE_BINFO (type1));
	  tree v2 = BINFO_VTABLE (TYPE_BINFO (type2));
	  gcc_assert (TREE_CODE (v1) == POINTER_PLUS_EXPR
		      && TREE_CODE (v2) == POINTER_PLUS_EXPR);
	  return (operand_equal_p (TREE_OPERAND (v1, 1),
				   TREE_OPERAND (v2, 1), 0)
		  && DECL_ASSEMBLER_NAME
			 (TREE_OPERAND (TREE_OPERAND (v1, 0), 0))
		     == DECL_ASSEMBLER_NAME
			 (TREE_OPERAND (TREE_OPERAND (v2, 0), 0)));
	}
      gcc_unreachable ();
    }
  return (DECL_ASSEMBLER_NAME (TYPE_NAME (type1))
	  == DECL_ASSEMBLER_NAME (TYPE_NAME (type2)));
}

/* Return true if we can decide on ODR equivalency.

   In non-LTO it is always decide, in LTO however it depends in the type has
   ODR info attached.  */

static bool
types_odr_comparable (tree t1, tree t2)
{
  return (!in_lto_p
	  || main_odr_variant (t1) == main_odr_variant (t2)
	  || (odr_type_p (t1) && odr_type_p (t2))
	  || (TREE_CODE (t1) == RECORD_TYPE && TREE_CODE (t2) == RECORD_TYPE
	      && TYPE_BINFO (t1) && TYPE_BINFO (t2)
	      && polymorphic_type_binfo_p (TYPE_BINFO (t1))
	      && polymorphic_type_binfo_p (TYPE_BINFO (t2))));
}

/* Return true if T1 and T2 are ODR equivalent.  If ODR equivalency is not
   known, be conservative and return false.  */

static bool
types_must_be_same_for_odr (tree t1, tree t2)
{
  if (types_odr_comparable (t1, t2))
    return types_same_for_odr (t1, t2);
  else
    return main_odr_variant (t1) == main_odr_variant (t2);
}

/* Compare types T1 and T2 and return true if they are
   equivalent.  */

inline bool
odr_hasher::equal (const value_type *t1, const compare_type *ct2)
{
  tree t2 = const_cast <tree> (ct2);

  gcc_checking_assert (main_odr_variant (t2) == t2);
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
  if (v->types_set)
    delete v->types_set;
  ggc_free (v);
}

/* ODR type hash used to lookup ODR type based on tree type node.  */

typedef hash_table<odr_hasher> odr_hash_type;
static odr_hash_type *odr_hash;

/* ODR types are also stored into ODR_TYPE vector to allow consistent
   walking.  Bases appear before derived types.  Vector is garbage collected
   so we won't end up visiting empty types.  */

static GTY(()) vec <odr_type, va_gc> *odr_types_ptr;
#define odr_types (*odr_types_ptr)

/* Set TYPE_BINFO of TYPE and its variants to BINFO.  */
void
set_type_binfo (tree type, tree binfo)
{
  for (; type; type = TYPE_NEXT_VARIANT (type))
    if (COMPLETE_TYPE_P (type))
      TYPE_BINFO (type) = binfo;
    else
      gcc_assert (!TYPE_BINFO (type));
}

/* Compare T2 and T2 based on name or structure.  */

static bool
odr_subtypes_equivalent_p (tree t1, tree t2, hash_set<type_pair,pair_traits> *visited)
{
  bool an1, an2;

  /* This can happen in incomplete types that should be handled earlier.  */
  gcc_assert (t1 && t2);

  t1 = main_odr_variant (t1);
  t2 = main_odr_variant (t2);
  if (t1 == t2)
    return true;

  /* Anonymous namespace types must match exactly.  */
  an1 = type_in_anonymous_namespace_p (t1);
  an2 = type_in_anonymous_namespace_p (t2);
  if (an1 != an2 || an1)
    return false;

  /* For ODR types be sure to compare their names.
     To support -wno-odr-type-merging we allow one type to be non-ODR
     and other ODR even though it is a violation.  */
  if (types_odr_comparable (t1, t2))
    {
      if (!types_same_for_odr (t1, t2))
        return false;
      /* Limit recursion: If subtypes are ODR types and we know
         that they are same, be happy.  */
      if (!get_odr_type (t1, true)->odr_violated)
        return true;
    }

  /* Component types, builtins and possibly vioalting ODR types
     have to be compared structurally.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return false;
  if ((TYPE_NAME (t1) == NULL_TREE) != (TYPE_NAME (t2) == NULL_TREE))
    return false;
  if (TYPE_NAME (t1) && DECL_NAME (TYPE_NAME (t1)) != DECL_NAME (TYPE_NAME (t2)))
    return false;

  type_pair pair={t1,t2};
  if (TYPE_UID (t1) > TYPE_UID (t2))
    {
      pair.first = t2;
      pair.second = t1;
    }
  if (visited->add (pair))
    return true;
  return odr_types_equivalent_p (t1, t2, false, NULL, visited);
}

/* Compare two virtual tables, PREVAILING and VTABLE and output ODR
   violation warings.  */

void
compare_virtual_tables (varpool_node *prevailing, varpool_node *vtable)
{
  int n1, n2;
  if (DECL_VIRTUAL_P (prevailing->decl) != DECL_VIRTUAL_P (vtable->decl))
    {
      odr_violation_reported = true;
      if (DECL_VIRTUAL_P (prevailing->decl))
	{
	  varpool_node *tmp = prevailing;
	  prevailing = vtable;
	  vtable = tmp;
	}
      if (warning_at (DECL_SOURCE_LOCATION (TYPE_NAME (DECL_CONTEXT (vtable->decl))),
		      OPT_Wodr,
		      "virtual table of type %qD violates one definition rule",
		      DECL_CONTEXT (vtable->decl)))
	inform (DECL_SOURCE_LOCATION (prevailing->decl),
		"variable of same assembler name as the virtual table is "
		"defined in another translation unit");
      return;
    }
  if (!prevailing->definition || !vtable->definition)
    return;
  for (n1 = 0, n2 = 0; true; n1++, n2++)
    {
      struct ipa_ref *ref1, *ref2;
      bool end1, end2;
      end1 = !prevailing->iterate_reference (n1, ref1);
      end2 = !vtable->iterate_reference (n2, ref2);
      if (end1 && end2)
	return;
      if (!end1 && !end2
	  && DECL_ASSEMBLER_NAME (ref1->referred->decl)
	     != DECL_ASSEMBLER_NAME (ref2->referred->decl)
	  && !n2
	  && !DECL_VIRTUAL_P (ref2->referred->decl)
	  && DECL_VIRTUAL_P (ref1->referred->decl))
	{
	  if (warning_at (DECL_SOURCE_LOCATION (TYPE_NAME (DECL_CONTEXT (vtable->decl))), 0,
			  "virtual table of type %qD contains RTTI information",
			  DECL_CONTEXT (vtable->decl)))
	    {
	      inform (DECL_SOURCE_LOCATION (TYPE_NAME (DECL_CONTEXT (prevailing->decl))),
		      "but is prevailed by one without from other translation unit");
	      inform (DECL_SOURCE_LOCATION (TYPE_NAME (DECL_CONTEXT (prevailing->decl))),
		      "RTTI will not work on this type");
	    }
	  n2++;
          end2 = !vtable->iterate_reference (n2, ref2);
	}
      if (!end1 && !end2
	  && DECL_ASSEMBLER_NAME (ref1->referred->decl)
	     != DECL_ASSEMBLER_NAME (ref2->referred->decl)
	  && !n1
	  && !DECL_VIRTUAL_P (ref1->referred->decl)
	  && DECL_VIRTUAL_P (ref2->referred->decl))
	{
	  n1++;
          end1 = !vtable->iterate_reference (n1, ref1);
	}
      if (end1 || end2)
	{
	  if (end1)
	    {
	      varpool_node *tmp = prevailing;
	      prevailing = vtable;
	      vtable = tmp;
	      ref1 = ref2;
	    }
	  if (warning_at (DECL_SOURCE_LOCATION
			    (TYPE_NAME (DECL_CONTEXT (vtable->decl))), 0,
			  "virtual table of type %qD violates "
			  "one definition rule",
			  DECL_CONTEXT (vtable->decl)))
	    {
	      inform (DECL_SOURCE_LOCATION
		       (TYPE_NAME (DECL_CONTEXT (prevailing->decl))),
		      "the conflicting type defined in another translation "
		      "unit");
	      inform (DECL_SOURCE_LOCATION
		        (TYPE_NAME (DECL_CONTEXT (ref1->referring->decl))),
		      "contains additional virtual method %qD",
		      ref1->referred->decl);
	    }
	  return;
	}
      if (DECL_ASSEMBLER_NAME (ref1->referred->decl)
	  != DECL_ASSEMBLER_NAME (ref2->referred->decl))
	{
	  if (warning_at (DECL_SOURCE_LOCATION
			    (TYPE_NAME (DECL_CONTEXT (vtable->decl))), 0,
			  "virtual table of type %qD violates "
			  "one definition rule  ",
			  DECL_CONTEXT (vtable->decl)))
	    {
	      inform (DECL_SOURCE_LOCATION 
			(TYPE_NAME (DECL_CONTEXT (prevailing->decl))),
		      "the conflicting type defined in another translation "
		      "unit");
	      inform (DECL_SOURCE_LOCATION (ref1->referred->decl),
		      "virtual method %qD", ref1->referred->decl);
	      inform (DECL_SOURCE_LOCATION (ref2->referred->decl),
		      "ought to match virtual method %qD but does not",
		      ref2->referred->decl);
	      return;
	    }
	}
    }
}

/* Output ODR violation warning about T1 and T2 with REASON.
   Display location of ST1 and ST2 if REASON speaks about field or
   method of the type.
   If WARN is false, do nothing. Set WARNED if warning was indeed
   output.  */

void
warn_odr (tree t1, tree t2, tree st1, tree st2,
	  bool warn, bool *warned, const char *reason)
{
  tree decl2 = TYPE_NAME (t2);

  if (!warn)
    return;
  if (!warning_at (DECL_SOURCE_LOCATION (TYPE_NAME (t1)), OPT_Wodr,
		   "type %qT violates one definition rule",
		   t1))
    return;
  if (!st1 && !st2)
    ;
  /* For FIELD_DECL support also case where one of fields is
     NULL - this is used when the structures have mismatching number of
     elements.  */
  else if (!st1 || TREE_CODE (st1) == FIELD_DECL)
    {
      inform (DECL_SOURCE_LOCATION (decl2),
	      "a different type is defined in another translation unit");
      if (!st1)
	{
	  st1 = st2;
	  st2 = NULL;
	}
      inform (DECL_SOURCE_LOCATION (st1),
	      "the first difference of corresponding definitions is field %qD",
	      st1);
      if (st2)
        decl2 = st2;
    }
  else if (TREE_CODE (st1) == FUNCTION_DECL)
    {
      inform (DECL_SOURCE_LOCATION (decl2),
	      "a different type is defined in another translation unit");
      inform (DECL_SOURCE_LOCATION (st1),
	      "the first difference of corresponding definitions is method %qD",
	      st1);
      decl2 = st2;
    }
  else
    return;
  inform (DECL_SOURCE_LOCATION (decl2), reason);

  if (warned)
    *warned = true;
}

/* We already warned about ODR mismatch.  T1 and T2 ought to be equivalent
   because they are used on same place in ODR matching types.
   They are not; inform the user.  */

void
warn_types_mismatch (tree t1, tree t2)
{
  if (!TYPE_NAME (t1) || !TYPE_NAME (t2))
    return;
  /* In Firefox it is a common bug to have same types but in
     different namespaces.  Be a bit more informative on
     this.  */
  if (TYPE_CONTEXT (t1) && TYPE_CONTEXT (t2)
      && (((TREE_CODE (TYPE_CONTEXT (t1)) == NAMESPACE_DECL)
	    != (TREE_CODE (TYPE_CONTEXT (t2)) == NAMESPACE_DECL))
	   || (TREE_CODE (TYPE_CONTEXT (t1)) == NAMESPACE_DECL
	       && (DECL_NAME (TYPE_CONTEXT (t1)) !=
		   DECL_NAME (TYPE_CONTEXT (t2))))))
    inform (DECL_SOURCE_LOCATION (TYPE_NAME (t1)),
	    "type %qT should match type %qT but is defined "
	    "in different namespace  ",
	    t1, t2);
  else
    inform (DECL_SOURCE_LOCATION (TYPE_NAME (t1)),
	    "type %qT should match type %qT",
	    t1, t2);
  inform (DECL_SOURCE_LOCATION (TYPE_NAME (t2)),
	  "the incompatible type is defined here");
}

/* Compare T1 and T2, report ODR violations if WARN is true and set
   WARNED to true if anything is reported.  Return true if types match.
   If true is returned, the types are also compatible in the sense of
   gimple_canonical_types_compatible_p.  */

static bool
odr_types_equivalent_p (tree t1, tree t2, bool warn, bool *warned, hash_set<type_pair,pair_traits> *visited)
{
  /* Check first for the obvious case of pointer identity.  */
  if (t1 == t2)
    return true;
  gcc_assert (!type_in_anonymous_namespace_p (t1));
  gcc_assert (!type_in_anonymous_namespace_p (t2));

  /* Can't be the same type if the types don't have the same code.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    {
      warn_odr (t1, t2, NULL, NULL, warn, warned,
	        G_("a different type is defined in another translation unit"));
      return false;
    }

  if (TYPE_QUALS (t1) != TYPE_QUALS (t2))
    {
      warn_odr (t1, t2, NULL, NULL, warn, warned,
	        G_("a type with different qualifiers is defined in another "
		   "translation unit"));
      return false;
    }

  if (comp_type_attributes (t1, t2) != 1)
    {
      warn_odr (t1, t2, NULL, NULL, warn, warned,
	        G_("a type with attributes "
		   "is defined in another translation unit"));
      return false;
    }

  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    {
      tree v1, v2;
      for (v1 = TYPE_VALUES (t1), v2 = TYPE_VALUES (t2);
	   v1 && v2 ; v1 = TREE_CHAIN (v1), v2 = TREE_CHAIN (v2))
	{
	  if (TREE_PURPOSE (v1) != TREE_PURPOSE (v2))
	    {
	      warn_odr (t1, t2, NULL, NULL, warn, warned,
			G_("an enum with different value name"
			   " is defined in another translation unit"));
	      return false;
	    }
	  if (TREE_VALUE (v1) != TREE_VALUE (v2)
	      && !operand_equal_p (DECL_INITIAL (TREE_VALUE (v1)),
				   DECL_INITIAL (TREE_VALUE (v2)), 0))
	    {
	      warn_odr (t1, t2, NULL, NULL, warn, warned,
			G_("an enum with different values is defined"
			   " in another translation unit"));
	      return false;
	    }
	}
      if (v1 || v2)
	{
	  warn_odr (t1, t2, NULL, NULL, warn, warned,
		    G_("an enum with mismatching number of values "
		       "is defined in another translation unit"));
	  return false;
	}
    }

  /* Non-aggregate types can be handled cheaply.  */
  if (INTEGRAL_TYPE_P (t1)
      || SCALAR_FLOAT_TYPE_P (t1)
      || FIXED_POINT_TYPE_P (t1)
      || TREE_CODE (t1) == VECTOR_TYPE
      || TREE_CODE (t1) == COMPLEX_TYPE
      || TREE_CODE (t1) == OFFSET_TYPE
      || POINTER_TYPE_P (t1))
    {
      if (TYPE_PRECISION (t1) != TYPE_PRECISION (t2))
	{
	  warn_odr (t1, t2, NULL, NULL, warn, warned,
		    G_("a type with different precision is defined "
		       "in another translation unit"));
	  return false;
	}
      if (TYPE_UNSIGNED (t1) != TYPE_UNSIGNED (t2))
	{
	  warn_odr (t1, t2, NULL, NULL, warn, warned,
		    G_("a type with different signedness is defined "
		       "in another translation unit"));
	  return false;
	}

      if (TREE_CODE (t1) == INTEGER_TYPE
	  && TYPE_STRING_FLAG (t1) != TYPE_STRING_FLAG (t2))
	{
	  /* char WRT uint_8?  */
	  warn_odr (t1, t2, NULL, NULL, warn, warned,
		    G_("a different type is defined in another "
		       "translation unit"));
	  return false;
	}

      /* For canonical type comparisons we do not want to build SCCs
	 so we cannot compare pointed-to types.  But we can, for now,
	 require the same pointed-to type kind and match what
	 useless_type_conversion_p would do.  */
      if (POINTER_TYPE_P (t1))
	{
	  if (TYPE_ADDR_SPACE (TREE_TYPE (t1))
	      != TYPE_ADDR_SPACE (TREE_TYPE (t2)))
	    {
	      warn_odr (t1, t2, NULL, NULL, warn, warned,
			G_("it is defined as a pointer in different address "
			   "space in another translation unit"));
	      return false;
	    }

	  if (!odr_subtypes_equivalent_p (TREE_TYPE (t1), TREE_TYPE (t2), visited))
	    {
	      warn_odr (t1, t2, NULL, NULL, warn, warned,
			G_("it is defined as a pointer to different type "
			   "in another translation unit"));
	      if (warn && warned)
	        warn_types_mismatch (TREE_TYPE (t1), TREE_TYPE (t2));
	      return false;
	    }
	}

      if ((TREE_CODE (t1) == VECTOR_TYPE || TREE_CODE (t1) == COMPLEX_TYPE)
	  && !odr_subtypes_equivalent_p (TREE_TYPE (t1), TREE_TYPE (t2), visited))
	{
	  /* Probably specific enough.  */
	  warn_odr (t1, t2, NULL, NULL, warn, warned,
		    G_("a different type is defined "
		       "in another translation unit"));
	  if (warn && warned)
	    warn_types_mismatch (TREE_TYPE (t1), TREE_TYPE (t2));
	  return false;
	}
    }
  /* Do type-specific comparisons.  */
  else switch (TREE_CODE (t1))
    {
    case ARRAY_TYPE:
      {
	/* Array types are the same if the element types are the same and
	   the number of elements are the same.  */
	if (!odr_subtypes_equivalent_p (TREE_TYPE (t1), TREE_TYPE (t2), visited))
	  {
	    warn_odr (t1, t2, NULL, NULL, warn, warned,
		      G_("a different type is defined in another "
			 "translation unit"));
	    if (warn && warned)
	      warn_types_mismatch (TREE_TYPE (t1), TREE_TYPE (t2));
	  }
	gcc_assert (TYPE_STRING_FLAG (t1) == TYPE_STRING_FLAG (t2));
	gcc_assert (TYPE_NONALIASED_COMPONENT (t1)
		    == TYPE_NONALIASED_COMPONENT (t2));

	tree i1 = TYPE_DOMAIN (t1);
	tree i2 = TYPE_DOMAIN (t2);

	/* For an incomplete external array, the type domain can be
	   NULL_TREE.  Check this condition also.  */
	if (i1 == NULL_TREE || i2 == NULL_TREE)
	  return true;

	tree min1 = TYPE_MIN_VALUE (i1);
	tree min2 = TYPE_MIN_VALUE (i2);
	tree max1 = TYPE_MAX_VALUE (i1);
	tree max2 = TYPE_MAX_VALUE (i2);

	/* In C++, minimums should be always 0.  */
	gcc_assert (min1 == min2);
	if (!operand_equal_p (max1, max2, 0))
	  {
	    warn_odr (t1, t2, NULL, NULL, warn, warned,
		      G_("an array of different size is defined "
			 "in another translation unit"));
	    return false;
	  }
      }
    break;

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      /* Function types are the same if the return type and arguments types
	 are the same.  */
      if (!odr_subtypes_equivalent_p (TREE_TYPE (t1), TREE_TYPE (t2), visited))
	{
	  warn_odr (t1, t2, NULL, NULL, warn, warned,
		    G_("has different return value "
		       "in another translation unit"));
	  if (warn && warned)
	    warn_types_mismatch (TREE_TYPE (t1), TREE_TYPE (t2));
	  return false;
	}

      if (TYPE_ARG_TYPES (t1) == TYPE_ARG_TYPES (t2))
	return true;
      else
	{
	  tree parms1, parms2;

	  for (parms1 = TYPE_ARG_TYPES (t1), parms2 = TYPE_ARG_TYPES (t2);
	       parms1 && parms2;
	       parms1 = TREE_CHAIN (parms1), parms2 = TREE_CHAIN (parms2))
	    {
	      if (!odr_subtypes_equivalent_p
		     (TREE_VALUE (parms1), TREE_VALUE (parms2), visited))
		{
		  warn_odr (t1, t2, NULL, NULL, warn, warned,
			    G_("has different parameters in another "
			       "translation unit"));
		  if (warn && warned)
		    warn_types_mismatch (TREE_VALUE (parms1),
					 TREE_VALUE (parms2));
		  return false;
		}
	    }

	  if (parms1 || parms2)
	    {
	      warn_odr (t1, t2, NULL, NULL, warn, warned,
			G_("has different parameters "
			   "in another translation unit"));
	      return false;
	    }

	  return true;
	}

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree f1, f2;

	/* For aggregate types, all the fields must be the same.  */
	if (COMPLETE_TYPE_P (t1) && COMPLETE_TYPE_P (t2))
	  {
	    for (f1 = TYPE_FIELDS (t1), f2 = TYPE_FIELDS (t2);
		 f1 || f2;
		 f1 = TREE_CHAIN (f1), f2 = TREE_CHAIN (f2))
	      {
		/* Skip non-fields.  */
		while (f1 && TREE_CODE (f1) != FIELD_DECL)
		  f1 = TREE_CHAIN (f1);
		while (f2 && TREE_CODE (f2) != FIELD_DECL)
		  f2 = TREE_CHAIN (f2);
		if (!f1 || !f2)
		  break;
		if (DECL_ARTIFICIAL (f1) != DECL_ARTIFICIAL (f2))
		  break;
		if (DECL_NAME (f1) != DECL_NAME (f2)
		    && !DECL_ARTIFICIAL (f1))
		  {
		    warn_odr (t1, t2, f1, f2, warn, warned,
			      G_("a field with different name is defined "
				 "in another translation unit"));
		    return false;
		  }
		if (!odr_subtypes_equivalent_p (TREE_TYPE (f1), TREE_TYPE (f2), visited))
		  {
		    /* Do not warn about artificial fields and just go into generic
		       field mismatch warning.  */
		    if (DECL_ARTIFICIAL (f1))
		      break;

		    warn_odr (t1, t2, f1, f2, warn, warned,
			      G_("a field of same name but different type "
				 "is defined in another translation unit"));
		    if (warn && warned)
		      warn_types_mismatch (TREE_TYPE (f1), TREE_TYPE (f2));
		    return false;
		  }
		if (!gimple_compare_field_offset (f1, f2))
		  {
		    /* Do not warn about artificial fields and just go into generic
		       field mismatch warning.  */
		    if (DECL_ARTIFICIAL (f1))
		      break;
		    warn_odr (t1, t2, t1, t2, warn, warned,
			      G_("fields has different layout "
				 "in another translation unit"));
		    return false;
		  }
		gcc_assert (DECL_NONADDRESSABLE_P (f1)
			    == DECL_NONADDRESSABLE_P (f2));
	      }

	    /* If one aggregate has more fields than the other, they
	       are not the same.  */
	    if (f1 || f2)
	      {
		if (f1 && DECL_ARTIFICIAL (f1))
		  f1 = NULL;
		if (f2 && DECL_ARTIFICIAL (f2))
		  f2 = NULL;
		if (f1 || f2)
		  warn_odr (t1, t2, f1, f2, warn, warned,
			    G_("a type with different number of fields "
			       "is defined in another translation unit"));
		/* Ideally we should never get this generic message.  */
		else
		  warn_odr (t1, t2, f1, f2, warn, warned,
			    G_("a type with different memory representation "
			       "is defined in another translation unit"));
		
		return false;
	      }
	    if ((TYPE_MAIN_VARIANT (t1) == t1 || TYPE_MAIN_VARIANT (t2) == t2)
		&& (TYPE_METHODS (TYPE_MAIN_VARIANT (t1))
		    != TYPE_METHODS (TYPE_MAIN_VARIANT (t2))))
	      {
		for (f1 = TYPE_METHODS (TYPE_MAIN_VARIANT (t1)),
		     f2 = TYPE_METHODS (TYPE_MAIN_VARIANT (t2));
		     f1 && f2 ; f1 = DECL_CHAIN (f1), f2 = DECL_CHAIN (f2))
		  {
		    if (DECL_ASSEMBLER_NAME (f1) != DECL_ASSEMBLER_NAME (f2))
		      {
			warn_odr (t1, t2, f1, f2, warn, warned,
				  G_("a different method of same type "
				     "is defined in another translation unit"));
			return false;
		      }
		    if (DECL_VIRTUAL_P (f1) != DECL_VIRTUAL_P (f2))
		      {
			warn_odr (t1, t2, f1, f2, warn, warned,
				  G_("s definition that differs by virtual "
				     "keyword in another translation unit"));
			return false;
		      }
		    if (DECL_VINDEX (f1) != DECL_VINDEX (f2))
		      {
			warn_odr (t1, t2, f1, f2, warn, warned,
				  G_("virtual table layout differs in another "
				     "translation unit"));
			return false;
		      }
		    if (odr_subtypes_equivalent_p (TREE_TYPE (f1), TREE_TYPE (f2), visited))
		      {
			warn_odr (t1, t2, f1, f2, warn, warned,
				  G_("method with incompatible type is defined "
				     "in another translation unit"));
			return false;
		      }
		  }
		if (f1 || f2)
		  {
		    warn_odr (t1, t2, NULL, NULL, warn, warned,
			      G_("a type with different number of methods "
				 "is defined in another translation unit"));
		    return false;
		  }
	      }
	  }
	break;
      }
    case VOID_TYPE:
      break;

    default:
      debug_tree (t1);
      gcc_unreachable ();
    }

  /* Those are better to come last as they are utterly uninformative.  */
  if (TYPE_SIZE (t1) && TYPE_SIZE (t2)
      && !operand_equal_p (TYPE_SIZE (t1), TYPE_SIZE (t2), 0))
    {
      warn_odr (t1, t2, NULL, NULL, warn, warned,
		G_("a type with different size "
		   "is defined in another translation unit"));
      return false;
    }
  if (COMPLETE_TYPE_P (t1) && COMPLETE_TYPE_P (t2)
      && TYPE_ALIGN (t1) != TYPE_ALIGN (t2))
    {
      warn_odr (t1, t2, NULL, NULL, warn, warned,
		G_("a type with different alignment "
		   "is defined in another translation unit"));
      return false;
    }
  gcc_assert (!TYPE_SIZE_UNIT (t1) || !TYPE_SIZE_UNIT (t2)
	      || operand_equal_p (TYPE_SIZE_UNIT (t1),
				  TYPE_SIZE_UNIT (t2), 0));
  return true;
}

/* TYPE is equivalent to VAL by ODR, but its tree representation differs
   from VAL->type.  This may happen in LTO where tree merging did not merge
   all variants of the same type.  It may or may not mean the ODR violation.
   Add it to the list of duplicates and warn on some violations.  */

static bool
add_type_duplicate (odr_type val, tree type)
{
  bool build_bases = false;
  if (!val->types_set)
    val->types_set = new hash_set<tree>;

  /* Always prefer complete type to be the leader.  */
  if (!COMPLETE_TYPE_P (val->type)
      && COMPLETE_TYPE_P (type))
    {
      tree tmp = type;

      build_bases = true;
      type = val->type;
      val->type = tmp;
    }

  /* See if this duplicate is new.  */
  if (!val->types_set->add (type))
    {
      bool merge = true;
      bool base_mismatch = false;
      unsigned int i,j;
      bool warned = false;
      hash_set<type_pair,pair_traits> visited;

      gcc_assert (in_lto_p);
      vec_safe_push (val->types, type);

      /* First we compare memory layout.  */
      if (!odr_types_equivalent_p (val->type, type, !flag_ltrans && !val->odr_violated,
				   &warned, &visited))
	{
	  merge = false;
	  odr_violation_reported = true;
	  val->odr_violated = true;
	  if (symtab->dump_file)
	    {
	      fprintf (symtab->dump_file, "ODR violation\n");
	    
	      print_node (symtab->dump_file, "", val->type, 0);
	      putc ('\n',symtab->dump_file);
	      print_node (symtab->dump_file, "", type, 0);
	      putc ('\n',symtab->dump_file);
	    }
	}

      /* Next sanity check that bases are the same.  If not, we will end
	 up producing wrong answers.  */
      if (COMPLETE_TYPE_P (type) && COMPLETE_TYPE_P (val->type)
	  && TREE_CODE (val->type) == RECORD_TYPE
	  && TREE_CODE (type) == RECORD_TYPE
	  && TYPE_BINFO (val->type) && TYPE_BINFO (type))
	{
	  for (j = 0, i = 0; i < BINFO_N_BASE_BINFOS (TYPE_BINFO (type)); i++)
	    if (polymorphic_type_binfo_p (BINFO_BASE_BINFO (TYPE_BINFO (type), i)))
	      {
		odr_type base = get_odr_type
				   (BINFO_TYPE
				      (BINFO_BASE_BINFO (TYPE_BINFO (type),
							 i)),
				    true);
		if (val->bases.length () <= j || val->bases[j] != base)
		  base_mismatch = true;
		j++;
	      }
	  if (base_mismatch)
	    {
	      merge = false;
	      odr_violation_reported = true;

	      if (!warned && !val->odr_violated)
		warn_odr (type, val->type, NULL, NULL, !warned, &warned,
			  "a type with the same name but different bases is "
			  "defined in another translation unit");
	      val->odr_violated = true;
	      if (symtab->dump_file)
		{
		  fprintf (symtab->dump_file, "ODR bse violation or merging bug?\n");
		
		  print_node (symtab->dump_file, "", val->type, 0);
		  putc ('\n',symtab->dump_file);
		  print_node (symtab->dump_file, "", type, 0);
		  putc ('\n',symtab->dump_file);
		}
	    }
	}

      /* Regularize things a little.  During LTO same types may come with
	 different BINFOs.  Either because their virtual table was
	 not merged by tree merging and only later at decl merging or
	 because one type comes with external vtable, while other
	 with internal.  We want to merge equivalent binfos to conserve
	 memory and streaming overhead.

	 The external vtables are more harmful: they contain references
	 to external declarations of methods that may be defined in the
	 merged LTO unit.  For this reason we absolutely need to remove
	 them and replace by internal variants. Not doing so will lead
         to incomplete answers from possible_polymorphic_call_targets.

	 FIXME: disable for now; because ODR types are now build during
	 streaming in, the variants do not need to be linked to the type,
	 yet.  We need to do the merging in cleanup pass to be implemented
	 soon.  */
      if (!flag_ltrans && merge
	  && 0
	  && TREE_CODE (val->type) == RECORD_TYPE
	  && TREE_CODE (type) == RECORD_TYPE
	  && TYPE_BINFO (val->type) && TYPE_BINFO (type)
	  && TYPE_MAIN_VARIANT (type) == type
	  && TYPE_MAIN_VARIANT (val->type) == val->type
	  && BINFO_VTABLE (TYPE_BINFO (val->type))
	  && BINFO_VTABLE (TYPE_BINFO (type)))
	{
	  tree master_binfo = TYPE_BINFO (val->type);
	  tree v1 = BINFO_VTABLE (master_binfo);
	  tree v2 = BINFO_VTABLE (TYPE_BINFO (type));

	  if (TREE_CODE (v1) == POINTER_PLUS_EXPR)
	    {
	      gcc_assert (TREE_CODE (v2) == POINTER_PLUS_EXPR
			  && operand_equal_p (TREE_OPERAND (v1, 1),
					      TREE_OPERAND (v2, 1), 0));
	      v1 = TREE_OPERAND (TREE_OPERAND (v1, 0), 0);
	      v2 = TREE_OPERAND (TREE_OPERAND (v2, 0), 0);
	    }
	  gcc_assert (DECL_ASSEMBLER_NAME (v1)
		      == DECL_ASSEMBLER_NAME (v2));

	  if (DECL_EXTERNAL (v1) && !DECL_EXTERNAL (v2))
	    {
	      unsigned int i;

	      set_type_binfo (val->type, TYPE_BINFO (type));
	      for (i = 0; i < val->types->length (); i++)
		{
		  if (TYPE_BINFO ((*val->types)[i])
		      == master_binfo)
		    set_type_binfo ((*val->types)[i], TYPE_BINFO (type));
		}
	      BINFO_TYPE (TYPE_BINFO (type)) = val->type;
	    }
	  else
	    set_type_binfo (type, master_binfo);
	}
    }
  return build_bases;
}

/* Get ODR type hash entry for TYPE.  If INSERT is true, create
   possibly new entry.  */

odr_type
get_odr_type (tree type, bool insert)
{
  odr_type_d **slot;
  odr_type val;
  hashval_t hash;
  bool build_bases = false;
  bool insert_to_odr_array = false;
  int base_id = -1;

  type = main_odr_variant (type);

  hash = hash_type_name (type);
  slot = odr_hash->find_slot_with_hash (type, hash,
					insert ? INSERT : NO_INSERT);
  if (!slot)
    return NULL;

  /* See if we already have entry for type.  */
  if (*slot)
    {
      val = *slot;

      /* With LTO we need to support multiple tree representation of
	 the same ODR type.  */
      if (val->type != type)
        build_bases = add_type_duplicate (val, type);
    }
  else
    {
      val = ggc_cleared_alloc<odr_type_d> ();
      val->type = type;
      val->bases = vNULL;
      val->derived_types = vNULL;
      val->anonymous_namespace = type_in_anonymous_namespace_p (type);
      build_bases = COMPLETE_TYPE_P (val->type);
      insert_to_odr_array = true;
    }

  if (build_bases && TREE_CODE (type) == RECORD_TYPE && TYPE_BINFO (type)
      && type == TYPE_MAIN_VARIANT (type))
    {
      tree binfo = TYPE_BINFO (type);
      unsigned int i;

      gcc_assert (BINFO_TYPE (TYPE_BINFO (val->type)) = type);
  
      val->all_derivations_known = type_all_derivations_known_p (type);
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
	    gcc_assert (TYPE_MAIN_VARIANT (base->type) == base->type);
	    base->derived_types.safe_push (val);
	    val->bases.safe_push (base);
	    if (base->id > base_id)
	      base_id = base->id;
	  }
      }
  /* Ensure that type always appears after bases.  */
  if (insert_to_odr_array)
    {
      if (odr_types_ptr)
        val->id = odr_types.length ();
      vec_safe_push (odr_types_ptr, val);
    }
  else if (base_id > val->id)
    {
      odr_types[val->id] = 0;
      /* Be sure we did not recorded any derived types; these may need
	 renumbering too.  */
      gcc_assert (val->derived_types.length() == 0);
      if (odr_types_ptr)
	val->id = odr_types.length ();
      vec_safe_push (odr_types_ptr, val);
    }
  return val;
}

/* Add TYPE od ODR type hash.  */

void
register_odr_type (tree type)
{
  if (!odr_hash)
    odr_hash = new odr_hash_type (23);
  /* Arrange things to be nicer and insert main variants first.  */
  if (odr_type_p (TYPE_MAIN_VARIANT (type)))
    get_odr_type (TYPE_MAIN_VARIANT (type), true);
  if (TYPE_MAIN_VARIANT (type) != type)
    get_odr_type (type, true);
}

/* Dump ODR type T and all its derrived type.  INDENT specify indentation for
   recusive printing.  */

static void
dump_odr_type (FILE *f, odr_type t, int indent=0)
{
  unsigned int i;
  fprintf (f, "%*s type %i: ", indent * 2, "", t->id);
  print_generic_expr (f, t->type, TDF_SLIM);
  fprintf (f, "%s", t->anonymous_namespace ? " (anonymous namespace)":"");
  fprintf (f, "%s\n", t->all_derivations_known ? " (derivations known)":"");
  if (TYPE_NAME (t->type))
    {
      fprintf (f, "%*s defined at: %s:%i\n", indent * 2, "",
	       DECL_SOURCE_FILE (TYPE_NAME (t->type)),
	       DECL_SOURCE_LINE (TYPE_NAME (t->type)));
    }
  if (t->bases.length ())
    {
      fprintf (f, "%*s base odr type ids: ", indent * 2, "");
      for (i = 0; i < t->bases.length (); i++)
	fprintf (f, " %i", t->bases[i]->id);
      fprintf (f, "\n");
    }
  if (t->derived_types.length ())
    {
      fprintf (f, "%*s derived types:\n", indent * 2, "");
      for (i = 0; i < t->derived_types.length (); i++)
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
  for (i = 0; i < odr_types.length (); i++)
    {
      if (odr_types[i] && odr_types[i]->bases.length () == 0)
	dump_odr_type (f, odr_types[i]);
    }
  for (i = 0; i < odr_types.length (); i++)
    {
      if (odr_types[i] && odr_types[i]->types && odr_types[i]->types->length ())
	{
	  unsigned int j;
	  fprintf (f, "Duplicate tree types for odr type %i\n", i);
	  print_node (f, "", odr_types[i]->type, 0);
	  for (j = 0; j < odr_types[i]->types->length (); j++)
	    {
	      tree t;
	      fprintf (f, "duplicate #%i\n", j);
	      print_node (f, "", (*odr_types[i]->types)[j], 0);
	      t = (*odr_types[i]->types)[j];
	      while (TYPE_P (t) && TYPE_CONTEXT (t))
		{
		  t = TYPE_CONTEXT (t);
	          print_node (f, "", t, 0);
		}
	      putc ('\n',f);
	    }
	}
    }
}

/* Given method type T, return type of class it belongs to.
   Lookup this pointer and get its type.    */

tree
method_class_type (const_tree t)
{
  tree first_parm_type = TREE_VALUE (TYPE_ARG_TYPES (t));
  gcc_assert (TREE_CODE (t) == METHOD_TYPE);

  return TREE_TYPE (first_parm_type);
}

/* Initialize IPA devirt and build inheritance tree graph.  */

void
build_type_inheritance_graph (void)
{
  struct symtab_node *n;
  FILE *inheritance_dump_file;
  int flags;

  if (odr_hash)
    return;
  timevar_push (TV_IPA_INHERITANCE);
  inheritance_dump_file = dump_begin (TDI_inheritance, &flags);
  odr_hash = new odr_hash_type (23);

  /* We reconstruct the graph starting of types of all methods seen in the
     the unit.  */
  FOR_EACH_SYMBOL (n)
    if (is_a <cgraph_node *> (n)
	&& DECL_VIRTUAL_P (n->decl)
	&& n->real_symbol_p ())
      get_odr_type (TYPE_MAIN_VARIANT (method_class_type (TREE_TYPE (n->decl))),
		    true);

    /* Look also for virtual tables of types that do not define any methods.
 
       We need it in a case where class B has virtual base of class A
       re-defining its virtual method and there is class C with no virtual
       methods with B as virtual base.

       Here we output B's virtual method in two variant - for non-virtual
       and virtual inheritance.  B's virtual table has non-virtual version,
       while C's has virtual.

       For this reason we need to know about C in order to include both
       variants of B.  More correctly, record_target_from_binfo should
       add both variants of the method when walking B, but we have no
       link in between them.

       We rely on fact that either the method is exported and thus we
       assume it is called externally or C is in anonymous namespace and
       thus we will see the vtable.  */

    else if (is_a <varpool_node *> (n)
	     && DECL_VIRTUAL_P (n->decl)
	     && TREE_CODE (DECL_CONTEXT (n->decl)) == RECORD_TYPE
	     && TYPE_BINFO (DECL_CONTEXT (n->decl))
	     && polymorphic_type_binfo_p (TYPE_BINFO (DECL_CONTEXT (n->decl))))
      get_odr_type (TYPE_MAIN_VARIANT (DECL_CONTEXT (n->decl)), true);
  if (inheritance_dump_file)
    {
      dump_type_inheritance_graph (inheritance_dump_file);
      dump_end (TDI_inheritance, inheritance_dump_file);
    }
  timevar_pop (TV_IPA_INHERITANCE);
}

/* Return true if N has reference from live virtual table
   (and thus can be a destination of polymorphic call). 
   Be conservatively correct when callgraph is not built or
   if the method may be referred externally.  */

static bool
referenced_from_vtable_p (struct cgraph_node *node)
{
  int i;
  struct ipa_ref *ref;
  bool found = false;

  if (node->externally_visible
      || DECL_EXTERNAL (node->decl)
      || node->used_from_other_partition)
    return true;

  /* Keep this test constant time.
     It is unlikely this can happen except for the case where speculative
     devirtualization introduced many speculative edges to this node. 
     In this case the target is very likely alive anyway.  */
  if (node->ref_list.referring.length () > 100)
    return true;

  /* We need references built.  */
  if (symtab->state <= CONSTRUCTION)
    return true;

  for (i = 0; node->iterate_referring (i, ref); i++)
	
    if ((ref->use == IPA_REF_ALIAS
	 && referenced_from_vtable_p (dyn_cast<cgraph_node *> (ref->referring)))
	|| (ref->use == IPA_REF_ADDR
	    && TREE_CODE (ref->referring->decl) == VAR_DECL
	    && DECL_VIRTUAL_P (ref->referring->decl)))
      {
	found = true;
	break;
      }
  return found;
}

/* If TARGET has associated node, record it in the NODES array.
   CAN_REFER specify if program can refer to the target directly.
   if TARGET is unknown (NULL) or it can not be inserted (for example because
   its body was already removed and there is no way to refer to it), clear
   COMPLETEP.  */

static void
maybe_record_node (vec <cgraph_node *> &nodes,
		   tree target, hash_set<tree> *inserted,
		   bool can_refer,
		   bool *completep)
{
  struct cgraph_node *target_node, *alias_target;
  enum availability avail;

  /* cxa_pure_virtual and __builtin_unreachable do not need to be added into
     list of targets; the runtime effect of calling them is undefined.
     Only "real" virtual methods should be accounted.  */
  if (target && TREE_CODE (TREE_TYPE (target)) != METHOD_TYPE)
    return;

  if (!can_refer)
    {
      /* The only case when method of anonymous namespace becomes unreferable
	 is when we completely optimized it out.  */
      if (flag_ltrans
	  || !target 
	  || !type_in_anonymous_namespace_p (DECL_CONTEXT (target)))
	*completep = false;
      return;
    }

  if (!target)
    return;

  target_node = cgraph_node::get (target);

  /* Preffer alias target over aliases, so we do not get confused by
     fake duplicates.  */
  if (target_node)
    {
      alias_target = target_node->ultimate_alias_target (&avail);
      if (target_node != alias_target
	  && avail >= AVAIL_AVAILABLE
	  && target_node->get_availability ())
	target_node = alias_target;
    }

  /* Method can only be called by polymorphic call if any
     of vtables refering to it are alive. 

     While this holds for non-anonymous functions, too, there are
     cases where we want to keep them in the list; for example
     inline functions with -fno-weak are static, but we still
     may devirtualize them when instance comes from other unit.
     The same holds for LTO.

     Currently we ignore these functions in speculative devirtualization.
     ??? Maybe it would make sense to be more aggressive for LTO even
     eslewhere.  */
  if (!flag_ltrans
      && type_in_anonymous_namespace_p (DECL_CONTEXT (target))
      && (!target_node
          || !referenced_from_vtable_p (target_node)))
    ;
  /* See if TARGET is useful function we can deal with.  */
  else if (target_node != NULL
	   && (TREE_PUBLIC (target)
	       || DECL_EXTERNAL (target)
	       || target_node->definition)
	   && target_node->real_symbol_p ())
    {
      gcc_assert (!target_node->global.inlined_to);
      gcc_assert (target_node->real_symbol_p ());
      if (!inserted->add (target))
	{
	  cached_polymorphic_call_targets->add (target_node);
	  nodes.safe_push (target_node);
	}
    }
  else if (completep
	   && (!type_in_anonymous_namespace_p
		 (DECL_CONTEXT (target))
	       || flag_ltrans))
    *completep = false;
}

/* See if BINFO's type match OUTER_TYPE.  If so, lookup 
   BINFO of subtype of OTR_TYPE at OFFSET and in that BINFO find
   method in vtable and insert method to NODES array
   or BASES_TO_CONSIDER if this array is non-NULL.
   Otherwise recurse to base BINFOs.
   This match what get_binfo_at_offset does, but with offset
   being unknown.

   TYPE_BINFOS is a stack of BINFOS of types with defined
   virtual table seen on way from class type to BINFO.

   MATCHED_VTABLES tracks virtual tables we already did lookup
   for virtual function in. INSERTED tracks nodes we already
   inserted.

   ANONYMOUS is true if BINFO is part of anonymous namespace.

   Clear COMPLETEP when we hit unreferable target.
  */

static void
record_target_from_binfo (vec <cgraph_node *> &nodes,
			  vec <tree> *bases_to_consider,
			  tree binfo,
			  tree otr_type,
			  vec <tree> &type_binfos,
			  HOST_WIDE_INT otr_token,
			  tree outer_type,
			  HOST_WIDE_INT offset,
			  hash_set<tree> *inserted,
			  hash_set<tree> *matched_vtables,
			  bool anonymous,
			  bool *completep)
{
  tree type = BINFO_TYPE (binfo);
  int i;
  tree base_binfo;


  if (BINFO_VTABLE (binfo))
    type_binfos.safe_push (binfo);
  if (types_same_for_odr (type, outer_type))
    {
      int i;
      tree type_binfo = NULL;

      /* Lookup BINFO with virtual table.  For normal types it is always last
	 binfo on stack.  */
      for (i = type_binfos.length () - 1; i >= 0; i--)
	if (BINFO_OFFSET (type_binfos[i]) == BINFO_OFFSET (binfo))
	  {
	    type_binfo = type_binfos[i];
	    break;
	  }
      if (BINFO_VTABLE (binfo))
	type_binfos.pop ();
      /* If this is duplicated BINFO for base shared by virtual inheritance,
	 we may not have its associated vtable.  This is not a problem, since
	 we will walk it on the other path.  */
      if (!type_binfo)
	return;
      tree inner_binfo = get_binfo_at_offset (type_binfo,
					      offset, otr_type);
      if (!inner_binfo)
	{
	  gcc_assert (odr_violation_reported);
	  return;
	}
      /* For types in anonymous namespace first check if the respective vtable
	 is alive. If not, we know the type can't be called.  */
      if (!flag_ltrans && anonymous)
	{
	  tree vtable = BINFO_VTABLE (inner_binfo);
	  varpool_node *vnode;

	  if (TREE_CODE (vtable) == POINTER_PLUS_EXPR)
	    vtable = TREE_OPERAND (TREE_OPERAND (vtable, 0), 0);
	  vnode = varpool_node::get (vtable);
	  if (!vnode || !vnode->definition)
	    return;
	}
      gcc_assert (inner_binfo);
      if (bases_to_consider
	  ? !matched_vtables->contains (BINFO_VTABLE (inner_binfo))
	  : !matched_vtables->add (BINFO_VTABLE (inner_binfo)))
	{
	  bool can_refer;
	  tree target = gimple_get_virt_method_for_binfo (otr_token,
							  inner_binfo,
							  &can_refer);
	  if (!bases_to_consider)
	    maybe_record_node (nodes, target, inserted, can_refer, completep);
	  /* Destructors are never called via construction vtables.  */
	  else if (!target || !DECL_CXX_DESTRUCTOR_P (target))
	    bases_to_consider->safe_push (target);
	}
      return;
    }

  /* Walk bases.  */
  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    /* Walking bases that have no virtual method is pointless excercise.  */
    if (polymorphic_type_binfo_p (base_binfo))
      record_target_from_binfo (nodes, bases_to_consider, base_binfo, otr_type,
				type_binfos, 
				otr_token, outer_type, offset, inserted,
				matched_vtables, anonymous, completep);
  if (BINFO_VTABLE (binfo))
    type_binfos.pop ();
}
     
/* Lookup virtual methods matching OTR_TYPE (with OFFSET and OTR_TOKEN)
   of TYPE, insert them to NODES, recurse into derived nodes. 
   INSERTED is used to avoid duplicate insertions of methods into NODES.
   MATCHED_VTABLES are used to avoid duplicate walking vtables.
   Clear COMPLETEP if unreferable target is found.
 
   If CONSIDER_CONSTURCTION is true, record to BASES_TO_CONSDIER
   all cases where BASE_SKIPPED is true (because the base is abstract
   class).  */

static void
possible_polymorphic_call_targets_1 (vec <cgraph_node *> &nodes,
				     hash_set<tree> *inserted,
				     hash_set<tree> *matched_vtables,
				     tree otr_type,
				     odr_type type,
				     HOST_WIDE_INT otr_token,
				     tree outer_type,
				     HOST_WIDE_INT offset,
				     bool *completep,
				     vec <tree> &bases_to_consider,
				     bool consider_construction)
{
  tree binfo = TYPE_BINFO (type->type);
  unsigned int i;
  vec <tree> type_binfos = vNULL;
  bool possibly_instantiated = type_possibly_instantiated_p (type->type);

  /* We may need to consider types w/o instances because of possible derived
     types using their methods either directly or via construction vtables.
     We are safe to skip them when all derivations are known, since we will
     handle them later.
     This is done by recording them to BASES_TO_CONSIDER array.  */
  if (possibly_instantiated || consider_construction)
    {
      record_target_from_binfo (nodes,
				(!possibly_instantiated
				 && type_all_derivations_known_p (type->type))
				? &bases_to_consider : NULL,
				binfo, otr_type, type_binfos, otr_token,
				outer_type, offset,
				inserted, matched_vtables,
				type->anonymous_namespace, completep);
    }
  type_binfos.release ();
  for (i = 0; i < type->derived_types.length (); i++)
    possible_polymorphic_call_targets_1 (nodes, inserted, 
					 matched_vtables,
					 otr_type,
					 type->derived_types[i],
					 otr_token, outer_type, offset, completep,
					 bases_to_consider, consider_construction);
}

/* Cache of queries for polymorphic call targets.

   Enumerating all call targets may get expensive when there are many
   polymorphic calls in the program, so we memoize all the previous
   queries and avoid duplicated work.  */

struct polymorphic_call_target_d
{
  HOST_WIDE_INT otr_token;
  ipa_polymorphic_call_context context;
  odr_type type;
  vec <cgraph_node *> targets;
  int speculative_targets;
  bool complete;
  int type_warning;
  tree decl_warning;
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
  inchash::hash hstate (odr_query->otr_token);

  hstate.add_wide_int (odr_query->type->id);
  hstate.merge_hash (TYPE_UID (odr_query->context.outer_type));
  hstate.add_wide_int (odr_query->context.offset);

  if (odr_query->context.speculative_outer_type)
    {
      hstate.merge_hash (TYPE_UID (odr_query->context.speculative_outer_type));
      hstate.add_wide_int (odr_query->context.speculative_offset);
    }
  hstate.add_flag (odr_query->context.maybe_in_construction);
  hstate.add_flag (odr_query->context.maybe_derived_type);
  hstate.add_flag (odr_query->context.speculative_maybe_derived_type);
  hstate.commit_flag ();
  return hstate.end ();
}

/* Compare cache entries T1 and T2.  */

inline bool
polymorphic_call_target_hasher::equal (const value_type *t1,
				       const compare_type *t2)
{
  return (t1->type == t2->type && t1->otr_token == t2->otr_token
	  && t1->context.offset == t2->context.offset
	  && t1->context.speculative_offset == t2->context.speculative_offset
	  && t1->context.outer_type == t2->context.outer_type
	  && t1->context.speculative_outer_type == t2->context.speculative_outer_type
	  && t1->context.maybe_in_construction
	      == t2->context.maybe_in_construction
	  && t1->context.maybe_derived_type == t2->context.maybe_derived_type
	  && (t1->context.speculative_maybe_derived_type
	      == t2->context.speculative_maybe_derived_type));
}

/* Remove entry in polymorphic call target cache hash.  */

inline void
polymorphic_call_target_hasher::remove (value_type *v)
{
  v->targets.release ();
  free (v);
}

/* Polymorphic call target query cache.  */

typedef hash_table<polymorphic_call_target_hasher>
   polymorphic_call_target_hash_type;
static polymorphic_call_target_hash_type *polymorphic_call_target_hash;

/* Destroy polymorphic call target query cache.  */

static void
free_polymorphic_call_targets_hash ()
{
  if (cached_polymorphic_call_targets)
    {
      delete polymorphic_call_target_hash;
      polymorphic_call_target_hash = NULL;
      delete cached_polymorphic_call_targets;
      cached_polymorphic_call_targets = NULL;
    }
}

/* When virtual function is removed, we may need to flush the cache.  */

static void
devirt_node_removal_hook (struct cgraph_node *n, void *d ATTRIBUTE_UNUSED)
{
  if (cached_polymorphic_call_targets
      && cached_polymorphic_call_targets->contains (n))
    free_polymorphic_call_targets_hash ();
}

/* Return true when TYPE contains an polymorphic type and thus is interesting
   for devirtualization machinery.  */

bool
contains_polymorphic_type_p (const_tree type)
{
  type = TYPE_MAIN_VARIANT (type);

  if (RECORD_OR_UNION_TYPE_P (type))
    {
      if (TYPE_BINFO (type)
          && polymorphic_type_binfo_p (TYPE_BINFO (type)))
	return true;
      for (tree fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	if (TREE_CODE (fld) == FIELD_DECL
	    && !DECL_ARTIFICIAL (fld)
	    && contains_polymorphic_type_p (TREE_TYPE (fld)))
	  return true;
      return false;
    }
  if (TREE_CODE (type) == ARRAY_TYPE)
    return contains_polymorphic_type_p (TREE_TYPE (type));
  return false;
}

/* Return true if it seems valid to use placement new to build EXPECTED_TYPE
   at possition CUR_OFFSET within TYPE.  

   POD can be changed to an instance of a polymorphic type by
   placement new.  Here we play safe and assume that any
   non-polymorphic type is POD.  */
bool
possible_placement_new (tree type, tree expected_type,
			HOST_WIDE_INT cur_offset)
{
  return ((TREE_CODE (type) != RECORD_TYPE
	   || !TYPE_BINFO (type)
	   || cur_offset >= BITS_PER_WORD
	   || !polymorphic_type_binfo_p (TYPE_BINFO (type)))
	  && (!TYPE_SIZE (type)
	      || !tree_fits_shwi_p (TYPE_SIZE (type))
	      || (cur_offset
		  + (expected_type ? tree_to_uhwi (TYPE_SIZE (expected_type))
		     : 1)
		  <= tree_to_uhwi (TYPE_SIZE (type)))));
}

/* THIS->OUTER_TYPE is a type of memory object where object of EXPECTED_TYPE
   is contained at THIS->OFFSET.  Walk the memory representation of
   THIS->OUTER_TYPE and find the outermost class type that match
   EXPECTED_TYPE or contain EXPECTED_TYPE as a base.  Update THIS
   to represent it.

   If EXPECTED_TYPE is NULL, just find outermost polymorphic type with
   virtual table present at possition OFFSET.

   For example when THIS represents type
   class A
     {
       int a;
       class B b;
     }
   and we look for type at offset sizeof(int), we end up with B and offset 0.
   If the same is produced by multiple inheritance, we end up with A and offset
   sizeof(int). 

   If we can not find corresponding class, give up by setting
   THIS->OUTER_TYPE to EXPECTED_TYPE and THIS->OFFSET to NULL. 
   Return true when lookup was sucesful.  */

bool
ipa_polymorphic_call_context::restrict_to_inner_class (tree expected_type)
{
  tree type = outer_type;
  HOST_WIDE_INT cur_offset = offset;
  bool speculative = false;

 /* Update OUTER_TYPE to match EXPECTED_TYPE if it is not set.  */
 if (!outer_type)
   {
     clear_outer_type (expected_type);
     type = expected_type;
     cur_offset = 0;
   }
 /* See if OFFSET points inside OUTER_TYPE.  If it does not, we know
    that the context is either invalid, or the instance type must be
    derived from OUTER_TYPE.

    Because the instance type may contain field whose type is of OUTER_TYPE,
    we can not derive any effective information about it.

    TODO: In the case we know all derrived types, we can definitely do better
    here.  */
  else if (TYPE_SIZE (outer_type)
	   && tree_fits_shwi_p (TYPE_SIZE (outer_type))
	   && tree_to_shwi (TYPE_SIZE (outer_type)) >= 0
	   && tree_to_shwi (TYPE_SIZE (outer_type)) <= offset)
   {
     clear_outer_type (expected_type);
     type = expected_type;
     cur_offset = 0;

     /* If derived type is not allowed, we know that the context is invalid.  */
     if (!maybe_derived_type)
       {
	 clear_speculation ();
	 invalid = true;
	 return false;
       }
   }

  if (speculative_outer_type)
    {
      /* Short cirucit the busy work bellow and give up on case when speculation
	 is obviously the same as outer_type.  */
      if ((!maybe_derived_type
	   || speculative_maybe_derived_type)
	  && types_must_be_same_for_odr (speculative_outer_type, outer_type))
	clear_speculation ();

      /* See if SPECULATIVE_OUTER_TYPE is contained in or derived from OUTER_TYPE.
	 In this case speculation is valid only if derived types are allowed. 

	 The test does not really look for derivate, but also accepts the case where
	 outer_type is a field of speculative_outer_type.  In this case eiter
	 MAYBE_DERIVED_TYPE is false and we have full non-speculative information or
	 the loop bellow will correctly update SPECULATIVE_OUTER_TYPE
	 and SPECULATIVE_MAYBE_DERIVED_TYPE.  */
      else if (speculative_offset < offset
	       || !contains_type_p (speculative_outer_type,
				    speculative_offset - offset,
				    outer_type)
	       || !maybe_derived_type)
	clear_speculation ();
    }
  else
    /* Regularize things little bit and clear all the fields when no useful
       speculatin is known.  */
    clear_speculation ();

  if (!type)
    goto no_useful_type_info;

  /* Find the sub-object the constant actually refers to and mark whether it is
     an artificial one (as opposed to a user-defined one).

     This loop is performed twice; first time for outer_type and second time
     for speculative_outer_type.  The second run has SPECULATIVE set.  */
  while (true)
    {
      HOST_WIDE_INT pos, size;
      tree fld;
      bool size_unknown;

      /* If we do not know size of TYPE, we need to be more conservative
         about accepting cases where we can not find EXPECTED_TYPE.
	 Generally the types that do matter here are of constant size.
	 Size_unknown case should be very rare.  */
      if (TYPE_SIZE (type)
	  && tree_fits_shwi_p (TYPE_SIZE (type))
	  && tree_to_shwi (TYPE_SIZE (type)) >= 0)
	size_unknown = false;
      else
	size_unknown = true;

      /* On a match, just return what we found.  */
      if ((types_odr_comparable (type, expected_type)
	   && types_same_for_odr (type, expected_type))
	  || (!expected_type
	      && TREE_CODE (type) == RECORD_TYPE
	      && TYPE_BINFO (type)
	      && polymorphic_type_binfo_p (TYPE_BINFO (type))))
	{
	  if (speculative)
	    {
	      /* If we did not match the offset, just give up on speculation.  */
	      if (cur_offset != 0
		  /* Also check if speculation did not end up being same as
		     non-speculation.  */
		  || (types_must_be_same_for_odr (speculative_outer_type,
						  outer_type)
		      && (maybe_derived_type
			  == speculative_maybe_derived_type)))
		clear_speculation ();
	      return true;
	    }
	  else
	    {
	      /* If type is known to be final, do not worry about derived
		 types.  Testing it here may help us to avoid speculation.  */
	      if (type_all_derivations_known_p (outer_type)
		  && (TYPE_FINAL_P (outer_type)
		      || (odr_hash
			  && !get_odr_type (outer_type, true)->derived_types.length())))
		maybe_derived_type = false;

	      /* Type can not contain itself on an non-zero offset.  In that case
		 just give up.  Still accept the case where size is now known.
		 Either the second copy may appear past the end of type or within
		 the non-POD buffer located inside the variably sized type
		 itself.  */
	      if (cur_offset != 0)
		goto no_useful_type_info;
	      /* If we determined type precisely or we have no clue on
 		 speuclation, we are done.  */
	      if (!maybe_derived_type || !speculative_outer_type)
		{
		  clear_speculation ();
	          return true;
		}
	      /* Otherwise look into speculation now.  */
	      else
		{
		  speculative = true;
		  type = speculative_outer_type;
		  cur_offset = speculative_offset;
		  continue;
		}
	    }
	}

      /* Walk fields and find corresponding on at OFFSET.  */
      if (TREE_CODE (type) == RECORD_TYPE)
	{
	  for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	    {
	      if (TREE_CODE (fld) != FIELD_DECL)
		continue;

	      pos = int_bit_position (fld);
	      size = tree_to_uhwi (DECL_SIZE (fld));
	      if (pos <= cur_offset && (pos + size) > cur_offset)
		break;
	    }

	  if (!fld)
	    goto no_useful_type_info;

	  type = TYPE_MAIN_VARIANT (TREE_TYPE (fld));
	  cur_offset -= pos;
	  /* DECL_ARTIFICIAL represents a basetype.  */
	  if (!DECL_ARTIFICIAL (fld))
	    {
	      if (!speculative)
		{
		  outer_type = type;
		  offset = cur_offset;
		  /* As soon as we se an field containing the type,
		     we know we are not looking for derivations.  */
		  maybe_derived_type = false;
		}
	      else
		{
		  speculative_outer_type = type;
		  speculative_offset = cur_offset;
		  speculative_maybe_derived_type = false;
		}
	    }
	}
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree subtype = TYPE_MAIN_VARIANT (TREE_TYPE (type));

	  /* Give up if we don't know array size.  */
	  if (!TYPE_SIZE (subtype)
	      || !tree_fits_shwi_p (TYPE_SIZE (subtype))
	      || tree_to_shwi (TYPE_SIZE (subtype)) <= 0
	      || !contains_polymorphic_type_p (subtype))
	    goto no_useful_type_info;

	  HOST_WIDE_INT new_offset = cur_offset % tree_to_shwi (TYPE_SIZE (subtype));

	  /* We may see buffer for placement new.  In this case the expected type
	     can be bigger than the subtype.  */
	  if (TYPE_SIZE (subtype)
	      && (cur_offset
		  + (expected_type ? tree_to_uhwi (TYPE_SIZE (expected_type))
		     : 0)
		  > tree_to_uhwi (TYPE_SIZE (type))))
	    goto no_useful_type_info;

	  cur_offset = new_offset;
	  type = subtype;
	  if (!speculative)
	    {
	      outer_type = type;
	      offset = cur_offset;
	      maybe_derived_type = false;
	    }
	  else
	    {
	      speculative_outer_type = type;
	      speculative_offset = cur_offset;
	      speculative_maybe_derived_type = false;
	    }
	}
      /* Give up on anything else.  */
      else
	{
no_useful_type_info:
	  /* We found no way to embedd EXPECTED_TYPE in TYPE.
	     We still permit two special cases - placement new and
	     the case of variadic types containing themselves.  */
	  if (!speculative
	      && (size_unknown || !type
		  || possible_placement_new (type, expected_type, cur_offset)))
	    {
	      /* In these weird cases we want to accept the context.
		 In non-speculative run we have no useful outer_type info
		 (TODO: we may eventually want to record upper bound on the
		  type size that can be used to prune the walk),
		 but we still want to consider speculation that may
		 give useful info.  */
	      if (!speculative)
		{
		  clear_outer_type (expected_type);
		  if (speculative_outer_type)
		    {
		      speculative = true;
		      type = speculative_outer_type;
		      cur_offset = speculative_offset;
		    }
		  else
		    return true;
		}
	      else
		clear_speculation ();
	      return true;
	    }
	  else
	    {
	      clear_speculation ();
	      if (speculative)
		return true;
	      clear_outer_type (expected_type);
	      invalid = true; 
	      return false;
	    }
	}
    }
}

/* Return true if OUTER_TYPE contains OTR_TYPE at OFFSET.  */

static bool
contains_type_p (tree outer_type, HOST_WIDE_INT offset,
		 tree otr_type)
{
  ipa_polymorphic_call_context context;
  context.offset = offset;
  context.outer_type = TYPE_MAIN_VARIANT (outer_type);
  context.maybe_derived_type = false;
  return context.restrict_to_inner_class (otr_type);
}

/* Lookup base of BINFO that has virtual table VTABLE with OFFSET.  */

static tree
subbinfo_with_vtable_at_offset (tree binfo, unsigned HOST_WIDE_INT offset,
				tree vtable)
{
  tree v = BINFO_VTABLE (binfo);
  int i;
  tree base_binfo;
  unsigned HOST_WIDE_INT this_offset;

  if (v)
    {
      if (!vtable_pointer_value_to_vtable (v, &v, &this_offset))
	gcc_unreachable ();

      if (offset == this_offset
	  && DECL_ASSEMBLER_NAME (v) == DECL_ASSEMBLER_NAME (vtable))
	return binfo;
    }
  
  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    if (polymorphic_type_binfo_p (base_binfo))
      {
	base_binfo = subbinfo_with_vtable_at_offset (base_binfo, offset, vtable);
	if (base_binfo)
	  return base_binfo;
      }
  return NULL;
}

/* T is known constant value of virtual table pointer.
   Store virtual table to V and its offset to OFFSET. 
   Return false if T does not look like virtual table reference.  */

bool
vtable_pointer_value_to_vtable (const_tree t, tree *v,
				unsigned HOST_WIDE_INT *offset)
{
  /* We expect &MEM[(void *)&virtual_table + 16B].
     We obtain object's BINFO from the context of the virtual table. 
     This one contains pointer to virtual table represented via
     POINTER_PLUS_EXPR.  Verify that this pointer match to what
     we propagated through.

     In the case of virtual inheritance, the virtual tables may
     be nested, i.e. the offset may be different from 16 and we may
     need to dive into the type representation.  */
  if (TREE_CODE (t) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (t, 0)) == MEM_REF
      && TREE_CODE (TREE_OPERAND (TREE_OPERAND (t, 0), 0)) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (TREE_OPERAND (t, 0), 1)) == INTEGER_CST
      && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (TREE_OPERAND (t, 0), 0), 0))
	  == VAR_DECL)
      && DECL_VIRTUAL_P (TREE_OPERAND (TREE_OPERAND
					 (TREE_OPERAND (t, 0), 0), 0)))
    {
      *v = TREE_OPERAND (TREE_OPERAND (TREE_OPERAND (t, 0), 0), 0);
      *offset = tree_to_uhwi (TREE_OPERAND (TREE_OPERAND (t, 0), 1));
      return true;
    }

  /* Alternative representation, used by C++ frontend is POINTER_PLUS_EXPR.
     We need to handle it when T comes from static variable initializer or
     BINFO. */
  if (TREE_CODE (t) == POINTER_PLUS_EXPR)
    {
      *offset = tree_to_uhwi (TREE_OPERAND (t, 1));
      t = TREE_OPERAND (t, 0);
    }
  else
    *offset = 0;

  if (TREE_CODE (t) != ADDR_EXPR)
    return false;
  *v = TREE_OPERAND (t, 0);
  return true;
}

/* T is known constant value of virtual table pointer.  Return BINFO of the
   instance type.  */

tree
vtable_pointer_value_to_binfo (const_tree t)
{
  tree vtable;
  unsigned HOST_WIDE_INT offset;

  if (!vtable_pointer_value_to_vtable (t, &vtable, &offset))
    return NULL_TREE;

  /* FIXME: for stores of construction vtables we return NULL,
     because we do not have BINFO for those. Eventually we should fix
     our representation to allow this case to be handled, too.
     In the case we see store of BINFO we however may assume
     that standard folding will be ale to cope with it.  */
  return subbinfo_with_vtable_at_offset (TYPE_BINFO (DECL_CONTEXT (vtable)),
					 offset, vtable);
}

/* We know that the instance is stored in variable or parameter
   (not dynamically allocated) and we want to disprove the fact
   that it may be in construction at invocation of CALL.

   For the variable to be in construction we actually need to
   be in constructor of corresponding global variable or
   the inline stack of CALL must contain the constructor.
   Check this condition.  This check works safely only before
   IPA passes, because inline stacks may become out of date
   later.  */

bool
decl_maybe_in_construction_p (tree base, tree outer_type,
			      gimple call, tree function)
{
  outer_type = TYPE_MAIN_VARIANT (outer_type);
  gcc_assert (DECL_P (base));

  /* After inlining the code unification optimizations may invalidate
     inline stacks.  Also we need to give up on global variables after
     IPA, because addresses of these may have been propagated to their
     constructors.  */
  if (DECL_STRUCT_FUNCTION (function)->after_inlining)
    return true;

  /* Pure functions can not do any changes on the dynamic type;
     that require writting to memory.  */
  if (!auto_var_in_fn_p (base, function)
      && flags_from_decl_or_type (function) & (ECF_PURE | ECF_CONST))
    return false;

  for (tree block = gimple_block (call); block && TREE_CODE (block) == BLOCK;
       block = BLOCK_SUPERCONTEXT (block))
    if (BLOCK_ABSTRACT_ORIGIN (block)
	&& TREE_CODE (BLOCK_ABSTRACT_ORIGIN (block)) == FUNCTION_DECL)
      {
	tree fn = BLOCK_ABSTRACT_ORIGIN (block);

	if (TREE_CODE (TREE_TYPE (fn)) != METHOD_TYPE
	    || (!DECL_CXX_CONSTRUCTOR_P (fn)
		&& !DECL_CXX_DESTRUCTOR_P (fn)))
	  {
	    /* Watch for clones where we constant propagated the first
	       argument (pointer to the instance).  */
	    fn = DECL_ABSTRACT_ORIGIN (fn);
	    if (!fn
		|| !is_global_var (base)
	        || TREE_CODE (TREE_TYPE (fn)) != METHOD_TYPE
		|| (!DECL_CXX_CONSTRUCTOR_P (fn)
		    && !DECL_CXX_DESTRUCTOR_P (fn)))
	      continue;
	  }
	if (flags_from_decl_or_type (fn) & (ECF_PURE | ECF_CONST))
	  continue;

	/* FIXME: this can go away once we have ODR types equivalency on
	   LTO level.  */
	if (in_lto_p && !polymorphic_type_binfo_p (TYPE_BINFO (outer_type)))
	  return true;
	tree type = TYPE_MAIN_VARIANT (method_class_type (TREE_TYPE (fn)));
	if (types_same_for_odr (type, outer_type))
	  return true;
      }

  if (TREE_CODE (base) == VAR_DECL
      && is_global_var (base))
    {
      if (TREE_CODE (TREE_TYPE (function)) != METHOD_TYPE
	  || (!DECL_CXX_CONSTRUCTOR_P (function)
	      && !DECL_CXX_DESTRUCTOR_P (function)))
	{
	  if (!DECL_ABSTRACT_ORIGIN (function))
	    return false;
	  /* Watch for clones where we constant propagated the first
	     argument (pointer to the instance).  */
	  function = DECL_ABSTRACT_ORIGIN (function);
	  if (!function
	      || TREE_CODE (TREE_TYPE (function)) != METHOD_TYPE
	      || (!DECL_CXX_CONSTRUCTOR_P (function)
		  && !DECL_CXX_DESTRUCTOR_P (function)))
	    return false;
	}
      /* FIXME: this can go away once we have ODR types equivalency on
	 LTO level.  */
      if (in_lto_p && !polymorphic_type_binfo_p (TYPE_BINFO (outer_type)))
	return true;
      tree type = TYPE_MAIN_VARIANT (method_class_type (TREE_TYPE (function)));
      if (types_same_for_odr (type, outer_type))
	return true;
    }
  return false;
}

/* Dump human readable context to F.  */

void
ipa_polymorphic_call_context::dump (FILE *f) const
{
  fprintf (f, "    ");
  if (invalid)
    fprintf (f, "Call is known to be undefined\n");
  else
    {
      if (!outer_type && !offset && !speculative_outer_type)
	fprintf (f, "Empty context\n");
      if (outer_type || offset)
	{
	  fprintf (f, "Outer type:");
	  print_generic_expr (f, outer_type, TDF_SLIM);
	  if (maybe_derived_type)
	    fprintf (f, " (or a derived type)");
	  if (maybe_in_construction)
	    fprintf (f, " (maybe in construction)");
	  fprintf (f, " offset "HOST_WIDE_INT_PRINT_DEC,
		   offset);
	}
      if (speculative_outer_type)
	{
	  fprintf (f, " speculative outer type:");
	  print_generic_expr (f, speculative_outer_type, TDF_SLIM);
	  if (speculative_maybe_derived_type)
	    fprintf (f, " (or a derived type)");
	  fprintf (f, " at offset "HOST_WIDE_INT_PRINT_DEC,
		   speculative_offset);
	}
    }
  fprintf(f, "\n");
}

/* Print context to stderr.  */

void
ipa_polymorphic_call_context::debug () const
{
  dump (stderr);
}

/* Proudce polymorphic call context for call method of instance
   that is located within BASE (that is assumed to be a decl) at offset OFF. */

void
ipa_polymorphic_call_context::set_by_decl (tree base, HOST_WIDE_INT off)
{
  gcc_assert (DECL_P (base));

  outer_type = TYPE_MAIN_VARIANT (TREE_TYPE (base));
  offset = off;
  clear_speculation ();
  /* Make very conservative assumption that all objects
     may be in construction. 
 
     It is up to caller to revisit this via
     get_dynamic_type or decl_maybe_in_construction_p.  */
  maybe_in_construction = true;
  maybe_derived_type = false;
}

/* CST is an invariant (address of decl), try to get meaningful
   polymorphic call context for polymorphic call of method 
   if instance of OTR_TYPE that is located at offset OFF of this invariant.
   Return FALSE if nothing meaningful can be found.  */

bool
ipa_polymorphic_call_context::set_by_invariant (tree cst,
						tree otr_type,
						HOST_WIDE_INT off)
{
  HOST_WIDE_INT offset2, size, max_size;
  tree base;

  invalid = false;
  off = 0;
  clear_outer_type (otr_type);

  if (TREE_CODE (cst) != ADDR_EXPR)
    return false;

  cst = TREE_OPERAND (cst, 0);
  base = get_ref_base_and_extent (cst, &offset2, &size, &max_size);
  if (!DECL_P (base) || max_size == -1 || max_size != size)
    return false;

  /* Only type inconsistent programs can have otr_type that is
     not part of outer type.  */
  if (otr_type && !contains_type_p (TREE_TYPE (base), off, otr_type))
    return false;

  set_by_decl (base, off);
  return true;
}

/* See if OP is SSA name initialized as a copy or by single assignment.
   If so, walk the SSA graph up.  */

static tree
walk_ssa_copies (tree op)
{
  STRIP_NOPS (op);
  while (TREE_CODE (op) == SSA_NAME
	 && !SSA_NAME_IS_DEFAULT_DEF (op)
	 && SSA_NAME_DEF_STMT (op)
	 && gimple_assign_single_p (SSA_NAME_DEF_STMT (op)))
    {
      if (gimple_assign_load_p (SSA_NAME_DEF_STMT (op)))
	return op;
      op = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (op));
      STRIP_NOPS (op);
    }
  return op;
}

/* Create polymorphic call context from IP invariant CST.
   This is typically &global_var.
   OTR_TYPE specify type of polymorphic call or NULL if unknown, OFF
   is offset of call.  */

ipa_polymorphic_call_context::ipa_polymorphic_call_context (tree cst,
							    tree otr_type,
							    HOST_WIDE_INT off)
{
  clear_speculation ();
  set_by_invariant (cst, otr_type, off);
}

/* Build context for pointer REF contained in FNDECL at statement STMT.
   if INSTANCE is non-NULL, return pointer to the object described by
   the context or DECL where context is contained in.  */

ipa_polymorphic_call_context::ipa_polymorphic_call_context (tree fndecl,
							    tree ref,
							    gimple stmt,
							    tree *instance)
{
  tree otr_type = NULL;
  tree base_pointer;

  if (TREE_CODE (ref) == OBJ_TYPE_REF)
    {
      otr_type = obj_type_ref_class (ref);
      base_pointer = OBJ_TYPE_REF_OBJECT (ref);
    }
  else
    base_pointer = ref;

  /* Set up basic info in case we find nothing interesting in the analysis.  */
  clear_speculation ();
  clear_outer_type (otr_type);
  invalid = false;

  /* Walk SSA for outer object.  */
  do 
    {
      base_pointer = walk_ssa_copies (base_pointer);
      if (TREE_CODE (base_pointer) == ADDR_EXPR)
	{
	  HOST_WIDE_INT size, max_size;
	  HOST_WIDE_INT offset2;
	  tree base = get_ref_base_and_extent (TREE_OPERAND (base_pointer, 0),
					       &offset2, &size, &max_size);

	  /* If this is a varying address, punt.  */
	  if ((TREE_CODE (base) == MEM_REF || DECL_P (base))
	      && max_size != -1
	      && max_size == size)
	    {
	      /* We found dereference of a pointer.  Type of the pointer
		 and MEM_REF is meaningless, but we can look futher.  */
	      if (TREE_CODE (base) == MEM_REF)
		{
		  base_pointer = TREE_OPERAND (base, 0);
		  offset
		    += offset2 + mem_ref_offset (base).to_short_addr () * BITS_PER_UNIT;
		  outer_type = NULL;
		}
	      /* We found base object.  In this case the outer_type
		 is known.  */
	      else if (DECL_P (base))
		{
		  gcc_assert (!POINTER_TYPE_P (TREE_TYPE (base)));

		  /* Only type inconsistent programs can have otr_type that is
		     not part of outer type.  */
		  if (otr_type
		      && !contains_type_p (TREE_TYPE (base),
					   offset + offset2, otr_type))
		    {
		      invalid = true;
		      if (instance)
			*instance = base_pointer;
		      return;
		    }
		  set_by_decl (base, offset + offset2);
		  if (maybe_in_construction && stmt)
		    maybe_in_construction
		     = decl_maybe_in_construction_p (base,
						     outer_type,
						     stmt,
						     fndecl);
		  if (instance)
		    *instance = base;
		  return;
		}
	      else
		break;
	    }
	  else
	    break;
	}
      else if (TREE_CODE (base_pointer) == POINTER_PLUS_EXPR
	       && tree_fits_uhwi_p (TREE_OPERAND (base_pointer, 1)))
	{
	  offset += tree_to_shwi (TREE_OPERAND (base_pointer, 1))
		    * BITS_PER_UNIT;
	  base_pointer = TREE_OPERAND (base_pointer, 0);
	}
      else
	break;
    }
  while (true);

  /* Try to determine type of the outer object.  */
  if (TREE_CODE (base_pointer) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (base_pointer)
      && TREE_CODE (SSA_NAME_VAR (base_pointer)) == PARM_DECL)
    {
      /* See if parameter is THIS pointer of a method.  */
      if (TREE_CODE (TREE_TYPE (fndecl)) == METHOD_TYPE
	  && SSA_NAME_VAR (base_pointer) == DECL_ARGUMENTS (fndecl))
	{
	  outer_type
	     = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (base_pointer)));
	  gcc_assert (TREE_CODE (outer_type) == RECORD_TYPE);

	  /* Dynamic casting has possibly upcasted the type
	     in the hiearchy.  In this case outer type is less
	     informative than inner type and we should forget
	     about it.  */
	  if (otr_type
	      && !contains_type_p (outer_type, offset,
				   otr_type))
	    {
	      outer_type = NULL;
	      if (instance)
		*instance = base_pointer;
	      return;
	    }

	  /* If the function is constructor or destructor, then
	     the type is possibly in construction, but we know
	     it is not derived type.  */
	  if (DECL_CXX_CONSTRUCTOR_P (fndecl)
	      || DECL_CXX_DESTRUCTOR_P (fndecl))
	    {
	      maybe_in_construction = true;
	      maybe_derived_type = false;
	    }
	  else
	    {
	      maybe_derived_type = true;
	      maybe_in_construction = false;
	    }
	  if (instance)
	    *instance = base_pointer;
	  return;
	}
      /* Non-PODs passed by value are really passed by invisible
	 reference.  In this case we also know the type of the
	 object.  */
      if (DECL_BY_REFERENCE (SSA_NAME_VAR (base_pointer)))
	{
	  outer_type
	     = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (base_pointer)));
	  gcc_assert (!POINTER_TYPE_P (outer_type));
	  /* Only type inconsistent programs can have otr_type that is
	     not part of outer type.  */
	  if (!contains_type_p (outer_type, offset,
			        otr_type))
	    { 
	      invalid = true;
	      if (instance)
		*instance = base_pointer;
	      return;
	    }
	  maybe_derived_type = false;
	  maybe_in_construction = false;
	  if (instance)
	    *instance = base_pointer;
	  return;
	}
    }

  tree base_type = TREE_TYPE (base_pointer);

  if (TREE_CODE (base_pointer) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (base_pointer)
      && TREE_CODE (SSA_NAME_VAR (base_pointer)) != PARM_DECL)
    {
      invalid = true;
      if (instance)
	*instance = base_pointer;
      return;
    }
  if (TREE_CODE (base_pointer) == SSA_NAME
      && SSA_NAME_DEF_STMT (base_pointer)
      && gimple_assign_single_p (SSA_NAME_DEF_STMT (base_pointer)))
    base_type = TREE_TYPE (gimple_assign_rhs1
			    (SSA_NAME_DEF_STMT (base_pointer)));
 
  if (POINTER_TYPE_P (base_type)
      && (otr_type
	  || !contains_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (base_type)),
			       offset,
			       otr_type)))
    {
      speculative_outer_type = TYPE_MAIN_VARIANT
					  (TREE_TYPE (base_type));
      speculative_offset = offset;
      speculative_maybe_derived_type = true;
    }
  /* TODO: There are multiple ways to derive a type.  For instance
     if BASE_POINTER is passed to an constructor call prior our refernece.
     We do not make this type of flow sensitive analysis yet.  */
  if (instance)
    *instance = base_pointer;
  return;
}

/* Structure to be passed in between detect_type_change and
   check_stmt_for_type_change.  */

struct type_change_info
{
  /* Offset into the object where there is the virtual method pointer we are
     looking for.  */
  HOST_WIDE_INT offset;
  /* The declaration or SSA_NAME pointer of the base that we are checking for
     type change.  */
  tree instance;
  /* The reference to virtual table pointer used.  */
  tree vtbl_ptr_ref;
  tree otr_type;
  /* If we actually can tell the type that the object has changed to, it is
     stored in this field.  Otherwise it remains NULL_TREE.  */
  tree known_current_type;
  HOST_WIDE_INT known_current_offset;

  /* Set to true if dynamic type change has been detected.  */
  bool type_maybe_changed;
  /* Set to true if multiple types have been encountered.  known_current_type
     must be disregarded in that case.  */
  bool multiple_types_encountered;
  /* Set to true if we possibly missed some dynamic type changes and we should
     consider the set to be speculative.  */
  bool speculative;
  bool seen_unanalyzed_store;
};

/* Return true if STMT is not call and can modify a virtual method table pointer.
   We take advantage of fact that vtable stores must appear within constructor
   and destructor functions.  */

bool
noncall_stmt_may_be_vtbl_ptr_store (gimple stmt)
{
  if (is_gimple_assign (stmt))
    {
      tree lhs = gimple_assign_lhs (stmt);

      if (gimple_clobber_p (stmt))
	return false;
      if (!AGGREGATE_TYPE_P (TREE_TYPE (lhs)))
	{
	  if (flag_strict_aliasing
	      && !POINTER_TYPE_P (TREE_TYPE (lhs)))
	    return false;

	  if (TREE_CODE (lhs) == COMPONENT_REF
	      && !DECL_VIRTUAL_P (TREE_OPERAND (lhs, 1)))
	    return false;
	  /* In the future we might want to use get_base_ref_and_offset to find
	     if there is a field corresponding to the offset and if so, proceed
	     almost like if it was a component ref.  */
	}
    }

  /* Code unification may mess with inline stacks.  */
  if (cfun->after_inlining)
    return true;

  /* Walk the inline stack and watch out for ctors/dtors.
     TODO: Maybe we can require the store to appear in toplevel
     block of CTOR/DTOR.  */
  for (tree block = gimple_block (stmt); block && TREE_CODE (block) == BLOCK;
       block = BLOCK_SUPERCONTEXT (block))
    if (BLOCK_ABSTRACT_ORIGIN (block)
	&& TREE_CODE (BLOCK_ABSTRACT_ORIGIN (block)) == FUNCTION_DECL)
      {
	tree fn = BLOCK_ABSTRACT_ORIGIN (block);

	if (flags_from_decl_or_type (fn) & (ECF_PURE | ECF_CONST))
	  return false;
	return (TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE
		&& (DECL_CXX_CONSTRUCTOR_P (fn)
		    || DECL_CXX_DESTRUCTOR_P (fn)));
      }
  return (TREE_CODE (TREE_TYPE (current_function_decl)) == METHOD_TYPE
	  && (DECL_CXX_CONSTRUCTOR_P (current_function_decl)
	      || DECL_CXX_DESTRUCTOR_P (current_function_decl)));
}

/* If STMT can be proved to be an assignment to the virtual method table
   pointer of ANALYZED_OBJ and the type associated with the new table
   identified, return the type.  Otherwise return NULL_TREE.  */

static tree
extr_type_from_vtbl_ptr_store (gimple stmt, struct type_change_info *tci,
			       HOST_WIDE_INT *type_offset)
{
  HOST_WIDE_INT offset, size, max_size;
  tree lhs, rhs, base;

  if (!gimple_assign_single_p (stmt))
    return NULL_TREE;

  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (lhs) != COMPONENT_REF
      || !DECL_VIRTUAL_P (TREE_OPERAND (lhs, 1)))
     {
	if (dump_file)
	  fprintf (dump_file, "  LHS is not virtual table.\n");
	return NULL_TREE;
     }

  if (tci->vtbl_ptr_ref && operand_equal_p (lhs, tci->vtbl_ptr_ref, 0))
    ;
  else
    {
      base = get_ref_base_and_extent (lhs, &offset, &size, &max_size);
      if (offset != tci->offset
	  || size != POINTER_SIZE
	  || max_size != POINTER_SIZE)
	{
	  if (dump_file)
	    fprintf (dump_file, "    wrong offset %i!=%i or size %i\n",
		     (int)offset, (int)tci->offset, (int)size);
	  return NULL_TREE;
	}
      if (DECL_P (tci->instance))
	{
	  if (base != tci->instance)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "    base:");
		  print_generic_expr (dump_file, base, TDF_SLIM);
		  fprintf (dump_file, " does not match instance:");
		  print_generic_expr (dump_file, tci->instance, TDF_SLIM);
		  fprintf (dump_file, "\n");
		}
	      return NULL_TREE;
	    }
	}
      else if (TREE_CODE (base) == MEM_REF)
	{
	  if (!operand_equal_p (tci->instance, TREE_OPERAND (base, 0), 0)
	      || !integer_zerop (TREE_OPERAND (base, 1)))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "    base mem ref:");
		  print_generic_expr (dump_file, base, TDF_SLIM);
		  fprintf (dump_file, " has nonzero offset or does not match instance:");
		  print_generic_expr (dump_file, tci->instance, TDF_SLIM);
		  fprintf (dump_file, "\n");
		}
	      return NULL_TREE;
	    }
	}
      else if (!operand_equal_p (tci->instance, base, 0)
	       || tci->offset)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "    base:");
	      print_generic_expr (dump_file, base, TDF_SLIM);
	      fprintf (dump_file, " does not match instance:");
	      print_generic_expr (dump_file, tci->instance, TDF_SLIM);
	      fprintf (dump_file, " with offset %i\n", (int)tci->offset);
	    }
	  return NULL_TREE;
	}
    }

  tree vtable;
  unsigned HOST_WIDE_INT offset2;

  if (!vtable_pointer_value_to_vtable (rhs, &vtable, &offset2))
    {
      if (dump_file)
	fprintf (dump_file, "    Failed to lookup binfo\n");
      return NULL;
    }

  tree binfo = subbinfo_with_vtable_at_offset (TYPE_BINFO (DECL_CONTEXT (vtable)),
					       offset2, vtable);
  if (!binfo)
    {
      if (dump_file)
	fprintf (dump_file, "    Construction vtable used\n");
      /* FIXME: We should suport construction contextes.  */
      return NULL;
    }
 
  *type_offset = tree_to_shwi (BINFO_OFFSET (binfo)) * BITS_PER_UNIT;
  return DECL_CONTEXT (vtable);
}

/* Record dynamic type change of TCI to TYPE.  */

void
record_known_type (struct type_change_info *tci, tree type, HOST_WIDE_INT offset)
{
  if (dump_file)
    {
      if (type)
	{
          fprintf (dump_file, "  Recording type: ");
	  print_generic_expr (dump_file, type, TDF_SLIM);
          fprintf (dump_file, " at offset %i\n", (int)offset);
	}
     else
       fprintf (dump_file, "  Recording unknown type\n");
    }

  /* If we found a constructor of type that is not polymorphic or
     that may contain the type in question as a field (not as base),
     restrict to the inner class first to make type matching bellow
     happier.  */
  if (type
      && (offset
          || (TREE_CODE (type) != RECORD_TYPE
	      || !polymorphic_type_binfo_p (TYPE_BINFO (type)))))
    {
      ipa_polymorphic_call_context context;

      context.offset = offset;
      context.outer_type = type;
      context.maybe_in_construction = false;
      context.maybe_derived_type = false;
      /* If we failed to find the inner type, we know that the call
	 would be undefined for type produced here.  */
      if (!context.restrict_to_inner_class (tci->otr_type))
	{
	  if (dump_file)
	    fprintf (dump_file, "  Ignoring; does not contain otr_type\n");
	  return;
	}
      /* Watch for case we reached an POD type and anticipate placement
	 new.  */
      if (!context.maybe_derived_type)
	{
          type = context.outer_type;
          offset = context.offset;
	}
    }
  if (tci->type_maybe_changed
      && (!types_same_for_odr (type, tci->known_current_type)
	  || offset != tci->known_current_offset))
    tci->multiple_types_encountered = true;
  tci->known_current_type = TYPE_MAIN_VARIANT (type);
  tci->known_current_offset = offset;
  tci->type_maybe_changed = true;
}

/* Callback of walk_aliased_vdefs and a helper function for
   detect_type_change to check whether a particular statement may modify
   the virtual table pointer, and if possible also determine the new type of
   the (sub-)object.  It stores its result into DATA, which points to a
   type_change_info structure.  */

static bool
check_stmt_for_type_change (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef, void *data)
{
  gimple stmt = SSA_NAME_DEF_STMT (vdef);
  struct type_change_info *tci = (struct type_change_info *) data;
  tree fn;

  /* If we already gave up, just terminate the rest of walk.  */
  if (tci->multiple_types_encountered)
    return true;

  if (is_gimple_call (stmt))
    {
      if (gimple_call_flags (stmt) & (ECF_CONST | ECF_PURE))
	return false;

      /* Check for a constructor call.  */
      if ((fn = gimple_call_fndecl (stmt)) != NULL_TREE
	  && DECL_CXX_CONSTRUCTOR_P (fn)
	  && TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE
	  && gimple_call_num_args (stmt))
      {
	tree op = walk_ssa_copies (gimple_call_arg (stmt, 0));
	tree type = method_class_type (TREE_TYPE (fn));
	HOST_WIDE_INT offset = 0, size, max_size;

	if (dump_file)
	  {
	    fprintf (dump_file, "  Checking constructor call: ");
	    print_gimple_stmt (dump_file, stmt, 0, 0);
	  }

	/* See if THIS parameter seems like instance pointer.  */
	if (TREE_CODE (op) == ADDR_EXPR)
	  {
	    op = get_ref_base_and_extent (TREE_OPERAND (op, 0),
					  &offset, &size, &max_size);
	    if (size != max_size || max_size == -1)
	      {
                tci->speculative = true;
	        return false;
	      }
	    if (op && TREE_CODE (op) == MEM_REF)
	      {
		if (!tree_fits_shwi_p (TREE_OPERAND (op, 1)))
		  {
                    tci->speculative = true;
		    return false;
		  }
		offset += tree_to_shwi (TREE_OPERAND (op, 1))
			  * BITS_PER_UNIT;
		op = TREE_OPERAND (op, 0);
	      }
	    else if (DECL_P (op))
	      ;
	    else
	      {
                tci->speculative = true;
	        return false;
	      }
	    op = walk_ssa_copies (op);
	  }
	if (operand_equal_p (op, tci->instance, 0)
	    && TYPE_SIZE (type)
	    && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	    && tree_fits_shwi_p (TYPE_SIZE (type))
	    && tree_to_shwi (TYPE_SIZE (type)) + offset > tci->offset)
	  {
	    record_known_type (tci, type, tci->offset - offset);
	    return true;
	  }
      }
     /* Calls may possibly change dynamic type by placement new. Assume
        it will not happen, but make result speculative only.  */
     if (dump_file)
	{
          fprintf (dump_file, "  Function call may change dynamic type:");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	}
     tci->speculative = true;
     return false;
   }
  /* Check for inlined virtual table store.  */
  else if (noncall_stmt_may_be_vtbl_ptr_store (stmt))
    {
      tree type;
      HOST_WIDE_INT offset = 0;
      if (dump_file)
	{
	  fprintf (dump_file, "  Checking vtbl store: ");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	}

      type = extr_type_from_vtbl_ptr_store (stmt, tci, &offset);
      gcc_assert (!type || TYPE_MAIN_VARIANT (type) == type);
      if (!type)
	{
	  if (dump_file)
	    fprintf (dump_file, "  Unanalyzed store may change type.\n");
	  tci->seen_unanalyzed_store = true;
	  tci->speculative = true;
	}
      else
        record_known_type (tci, type, offset);
      return true;
    }
  else
    return false;
}

/* THIS is polymorphic call context obtained from get_polymorphic_context.
   OTR_OBJECT is pointer to the instance returned by OBJ_TYPE_REF_OBJECT.
   INSTANCE is pointer to the outer instance as returned by
   get_polymorphic_context.  To avoid creation of temporary expressions,
   INSTANCE may also be an declaration of get_polymorphic_context found the
   value to be in static storage.

   If the type of instance is not fully determined
   (either OUTER_TYPE is unknown or MAYBE_IN_CONSTRUCTION/INCLUDE_DERIVED_TYPES
   is set), try to walk memory writes and find the actual construction of the
   instance.

   We do not include this analysis in the context analysis itself, because
   it needs memory SSA to be fully built and the walk may be expensive.
   So it is not suitable for use withing fold_stmt and similar uses.  */

bool
ipa_polymorphic_call_context::get_dynamic_type (tree instance,
						tree otr_object,
						tree otr_type,
						gimple call)
{
  struct type_change_info tci;
  ao_ref ao;
  bool function_entry_reached = false;
  tree instance_ref = NULL;
  gimple stmt = call;
  /* Remember OFFSET before it is modified by restrict_to_inner_class.
     This is because we do not update INSTANCE when walking inwards.  */
  HOST_WIDE_INT instance_offset = offset;

  otr_type = TYPE_MAIN_VARIANT (otr_type);

  /* Walk into inner type. This may clear maybe_derived_type and save us
     from useless work.  It also makes later comparsions with static type
     easier.  */
  if (outer_type)
    {
      if (!restrict_to_inner_class (otr_type))
        return false;
    }

  if (!maybe_in_construction && !maybe_derived_type)
    return false;

  /* We need to obtain refernce to virtual table pointer.  It is better
     to look it up in the code rather than build our own.  This require bit
     of pattern matching, but we end up verifying that what we found is
     correct. 

     What we pattern match is:

       tmp = instance->_vptr.A;   // vtbl ptr load
       tmp2 = tmp[otr_token];	  // vtable lookup
       OBJ_TYPE_REF(tmp2;instance->0) (instance);
 
     We want to start alias oracle walk from vtbl pointer load,
     but we may not be able to identify it, for example, when PRE moved the
     load around.  */

  if (gimple_code (call) == GIMPLE_CALL)
    {
      tree ref = gimple_call_fn (call);
      HOST_WIDE_INT offset2, size, max_size;

      if (TREE_CODE (ref) == OBJ_TYPE_REF)
	{
	  ref = OBJ_TYPE_REF_EXPR (ref);
	  ref = walk_ssa_copies (ref);

	  /* Check if definition looks like vtable lookup.  */
	  if (TREE_CODE (ref) == SSA_NAME
	      && !SSA_NAME_IS_DEFAULT_DEF (ref)
	      && gimple_assign_load_p (SSA_NAME_DEF_STMT (ref))
	      && TREE_CODE (gimple_assign_rhs1
			     (SSA_NAME_DEF_STMT (ref))) == MEM_REF)
	    {
	      ref = get_base_address
		     (TREE_OPERAND (gimple_assign_rhs1
				     (SSA_NAME_DEF_STMT (ref)), 0));
	      ref = walk_ssa_copies (ref);
	      /* Find base address of the lookup and see if it looks like
		 vptr load.  */
	      if (TREE_CODE (ref) == SSA_NAME
		  && !SSA_NAME_IS_DEFAULT_DEF (ref)
		  && gimple_assign_load_p (SSA_NAME_DEF_STMT (ref)))
		{
		  tree ref_exp = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (ref));
		  tree base_ref = get_ref_base_and_extent
				   (ref_exp, &offset2, &size, &max_size);

		  /* Finally verify that what we found looks like read from OTR_OBJECT
		     or from INSTANCE with offset OFFSET.  */
		  if (base_ref
		      && ((TREE_CODE (base_ref) == MEM_REF
		           && ((offset2 == instance_offset
		                && TREE_OPERAND (base_ref, 0) == instance)
			       || (!offset2 && TREE_OPERAND (base_ref, 0) == otr_object)))
			  || (DECL_P (instance) && base_ref == instance
			      && offset2 == instance_offset)))
		    {
		      stmt = SSA_NAME_DEF_STMT (ref);
		      instance_ref = ref_exp;
		    }
		}
	    }
	}
    }
 
  /* If we failed to look up the refernece in code, build our own.  */
  if (!instance_ref)
    {
      /* If the statement in question does not use memory, we can't tell
	 anything.  */
      if (!gimple_vuse (stmt))
	return false;
      ao_ref_init_from_ptr_and_size (&ao, otr_object, NULL);
    }
  else
  /* Otherwise use the real reference.  */
    ao_ref_init (&ao, instance_ref);

  /* We look for vtbl pointer read.  */
  ao.size = POINTER_SIZE;
  ao.max_size = ao.size;
  ao.ref_alias_set
    = get_deref_alias_set (TREE_TYPE (BINFO_VTABLE (TYPE_BINFO (otr_type))));

  if (dump_file)
    {
      fprintf (dump_file, "Determining dynamic type for call: ");
      print_gimple_stmt (dump_file, call, 0, 0);
      fprintf (dump_file, "  Starting walk at: ");
      print_gimple_stmt (dump_file, stmt, 0, 0);
      fprintf (dump_file, "  instance pointer: ");
      print_generic_expr (dump_file, otr_object, TDF_SLIM);
      fprintf (dump_file, "  Outer instance pointer: ");
      print_generic_expr (dump_file, instance, TDF_SLIM);
      fprintf (dump_file, " offset: %i (bits)", (int)offset);
      fprintf (dump_file, " vtbl reference: ");
      print_generic_expr (dump_file, instance_ref, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  tci.offset = offset;
  tci.instance = instance;
  tci.vtbl_ptr_ref = instance_ref;
  gcc_assert (TREE_CODE (instance) != MEM_REF);
  tci.known_current_type = NULL_TREE;
  tci.known_current_offset = 0;
  tci.otr_type = otr_type;
  tci.type_maybe_changed = false;
  tci.multiple_types_encountered = false;
  tci.speculative = false;
  tci.seen_unanalyzed_store = false;

  walk_aliased_vdefs (&ao, gimple_vuse (stmt), check_stmt_for_type_change,
		      &tci, NULL, &function_entry_reached);

  /* If we did not find any type changing statements, we may still drop
     maybe_in_construction flag if the context already have outer type. 

     Here we make special assumptions about both constructors and
     destructors which are all the functions that are allowed to alter the
     VMT pointers.  It assumes that destructors begin with assignment into
     all VMT pointers and that constructors essentially look in the
     following way:

     1) The very first thing they do is that they call constructors of
     ancestor sub-objects that have them.

     2) Then VMT pointers of this and all its ancestors is set to new
     values corresponding to the type corresponding to the constructor.

     3) Only afterwards, other stuff such as constructor of member
     sub-objects and the code written by the user is run.  Only this may
     include calling virtual functions, directly or indirectly.

     4) placement new can not be used to change type of non-POD statically
     allocated variables.

     There is no way to call a constructor of an ancestor sub-object in any
     other way.

     This means that we do not have to care whether constructors get the
     correct type information because they will always change it (in fact,
     if we define the type to be given by the VMT pointer, it is undefined).

     The most important fact to derive from the above is that if, for some
     statement in the section 3, we try to detect whether the dynamic type
     has changed, we can safely ignore all calls as we examine the function
     body backwards until we reach statements in section 2 because these
     calls cannot be ancestor constructors or destructors (if the input is
     not bogus) and so do not change the dynamic type (this holds true only
     for automatically allocated objects but at the moment we devirtualize
     only these).  We then must detect that statements in section 2 change
     the dynamic type and can try to derive the new type.  That is enough
     and we can stop, we will never see the calls into constructors of
     sub-objects in this code. 

     Therefore if the static outer type was found (outer_type)
     we can safely ignore tci.speculative that is set on calls and give up
     only if there was dyanmic type store that may affect given variable
     (seen_unanalyzed_store)  */

  if (!tci.type_maybe_changed
      || (outer_type
	  && !tci.seen_unanalyzed_store
	  && !tci.multiple_types_encountered
	  && offset == tci.offset
	  && types_same_for_odr (tci.known_current_type,
				 outer_type)))
    {
      if (!outer_type || tci.seen_unanalyzed_store)
	return false;
      if (maybe_in_construction)
        maybe_in_construction = false;
      if (dump_file)
	fprintf (dump_file, "  No dynamic type change found.\n");
      return true;
    }

  if (tci.known_current_type
      && !function_entry_reached
      && !tci.multiple_types_encountered)
    {
      if (!tci.speculative)
	{
	  outer_type = TYPE_MAIN_VARIANT (tci.known_current_type);
	  offset = tci.known_current_offset;
	  maybe_in_construction = false;
	  maybe_derived_type = false;
	  if (dump_file)
	    fprintf (dump_file, "  Determined dynamic type.\n");
	}
      else if (!speculative_outer_type
	       || speculative_maybe_derived_type)
	{
	  speculative_outer_type = TYPE_MAIN_VARIANT (tci.known_current_type);
	  speculative_offset = tci.known_current_offset;
	  speculative_maybe_derived_type = false;
	  if (dump_file)
	    fprintf (dump_file, "  Determined speculative dynamic type.\n");
	}
    }
  else if (dump_file)
    {
      fprintf (dump_file, "  Found multiple types%s%s\n",
	       function_entry_reached ? " (function entry reached)" : "",
	       function_entry_reached ? " (multiple types encountered)" : "");
    }

  return true;
}

/* Walk bases of OUTER_TYPE that contain OTR_TYPE at OFFSET.
   Lookup their respecitve virtual methods for OTR_TOKEN and OTR_TYPE
   and insert them to NODES.

   MATCHED_VTABLES and INSERTED is used to avoid duplicated work.  */

static void
record_targets_from_bases (tree otr_type,
			   HOST_WIDE_INT otr_token,
			   tree outer_type,
			   HOST_WIDE_INT offset,
			   vec <cgraph_node *> &nodes,
			   hash_set<tree> *inserted,
			   hash_set<tree> *matched_vtables,
			   bool *completep)
{
  while (true)
    {
      HOST_WIDE_INT pos, size;
      tree base_binfo;
      tree fld;

      if (types_same_for_odr (outer_type, otr_type))
	return;

      for (fld = TYPE_FIELDS (outer_type); fld; fld = DECL_CHAIN (fld))
	{
	  if (TREE_CODE (fld) != FIELD_DECL)
	    continue;

	  pos = int_bit_position (fld);
	  size = tree_to_shwi (DECL_SIZE (fld));
	  if (pos <= offset && (pos + size) > offset
	      /* Do not get confused by zero sized bases.  */
	      && polymorphic_type_binfo_p (TYPE_BINFO (TREE_TYPE (fld))))
	    break;
	}
      /* Within a class type we should always find correcponding fields.  */
      gcc_assert (fld && TREE_CODE (TREE_TYPE (fld)) == RECORD_TYPE);

      /* Nonbasetypes should have been stripped by outer_class_type.  */
      gcc_assert (DECL_ARTIFICIAL (fld));

      outer_type = TREE_TYPE (fld);
      offset -= pos;

      base_binfo = get_binfo_at_offset (TYPE_BINFO (outer_type),
					offset, otr_type);
      if (!base_binfo)
	{
	  gcc_assert (odr_violation_reported);
	  return;
	}
      gcc_assert (base_binfo);
      if (!matched_vtables->add (BINFO_VTABLE (base_binfo)))
	{
	  bool can_refer;
	  tree target = gimple_get_virt_method_for_binfo (otr_token,
							  base_binfo,
							  &can_refer);
	  if (!target || ! DECL_CXX_DESTRUCTOR_P (target))
	    maybe_record_node (nodes, target, inserted, can_refer, completep);
	  matched_vtables->add (BINFO_VTABLE (base_binfo));
	}
    }
}

/* When virtual table is removed, we may need to flush the cache.  */

static void
devirt_variable_node_removal_hook (varpool_node *n,
				   void *d ATTRIBUTE_UNUSED)
{
  if (cached_polymorphic_call_targets
      && DECL_VIRTUAL_P (n->decl)
      && type_in_anonymous_namespace_p (DECL_CONTEXT (n->decl)))
    free_polymorphic_call_targets_hash ();
}

/* Record about how many calls would benefit from given type to be final.  */

struct odr_type_warn_count
{
  tree type;
  int count;
  gcov_type dyn_count;
};

/* Record about how many calls would benefit from given method to be final.  */

struct decl_warn_count
{
  tree decl;
  int count;
  gcov_type dyn_count;
};

/* Information about type and decl warnings.  */

struct final_warning_record
{
  gcov_type dyn_count;
  vec<odr_type_warn_count> type_warnings;
  hash_map<tree, decl_warn_count> decl_warnings;
};
struct final_warning_record *final_warning_records;

/* Return vector containing possible targets of polymorphic call of type
   OTR_TYPE caling method OTR_TOKEN within type of OTR_OUTER_TYPE and OFFSET.
   If INCLUDE_BASES is true, walk also base types of OUTER_TYPES containig
   OTR_TYPE and include their virtual method.  This is useful for types
   possibly in construction or destruction where the virtual table may
   temporarily change to one of base types.  INCLUDE_DERIVER_TYPES make
   us to walk the inheritance graph for all derivations.

   If COMPLETEP is non-NULL, store true if the list is complete. 
   CACHE_TOKEN (if non-NULL) will get stored to an unique ID of entry
   in the target cache.  If user needs to visit every target list
   just once, it can memoize them.

   SPECULATION_TARGETS specify number of targets that are speculatively
   likely.  These include targets specified by the speculative part
   of polymoprhic call context and also exclude all targets for classes
   in construction.

   Returned vector is placed into cache.  It is NOT caller's responsibility
   to free it.  The vector can be freed on cgraph_remove_node call if
   the particular node is a virtual function present in the cache.  */

vec <cgraph_node *>
possible_polymorphic_call_targets (tree otr_type,
			           HOST_WIDE_INT otr_token,
				   ipa_polymorphic_call_context context,
			           bool *completep,
			           void **cache_token,
				   int *speculative_targetsp)
{
  static struct cgraph_node_hook_list *node_removal_hook_holder;
  vec <cgraph_node *> nodes = vNULL;
  vec <tree> bases_to_consider = vNULL;
  odr_type type, outer_type;
  polymorphic_call_target_d key;
  polymorphic_call_target_d **slot;
  unsigned int i;
  tree binfo, target;
  bool complete;
  bool can_refer;
  bool skipped = false;

  otr_type = TYPE_MAIN_VARIANT (otr_type);

  /* If ODR is not initialized or the constext is invalid, return empty
     incomplete list.  */
  if (!odr_hash || context.invalid)
    {
      if (completep)
	*completep = context.invalid;
      if (cache_token)
	*cache_token = NULL;
      if (speculative_targetsp)
	*speculative_targetsp = 0;
      return nodes;
    }

  /* Do not bother to compute speculative info when user do not asks for it.  */
  if (!speculative_targetsp || !context.speculative_outer_type)
    context.clear_speculation ();

  type = get_odr_type (otr_type, true);

  /* Recording type variants would wast results cache.  */
  gcc_assert (!context.outer_type
	      || TYPE_MAIN_VARIANT (context.outer_type) == context.outer_type);

  /* Lookup the outer class type we want to walk.
     If we fail to do so, the context is invalid.  */
  if ((context.outer_type || context.speculative_outer_type)
      && !context.restrict_to_inner_class (otr_type))
    {
      fprintf (stderr, "Invalid\n");
      if (completep)
	*completep = true;
      if (cache_token)
	*cache_token = NULL;
      if (speculative_targetsp)
	*speculative_targetsp = 0;
      return nodes;
    }
  gcc_assert (!context.invalid);

  /* Check that restrict_to_inner_class kept the main variant.  */
  gcc_assert (!context.outer_type
	      || TYPE_MAIN_VARIANT (context.outer_type) == context.outer_type);

  /* We canonicalize our query, so we do not need extra hashtable entries.  */

  /* Without outer type, we have no use for offset.  Just do the
     basic search from innter type  */
  if (!context.outer_type)
    {
      context.outer_type = otr_type;
      context.offset = 0;
    }
  /* We need to update our hiearchy if the type does not exist.  */
  outer_type = get_odr_type (context.outer_type, true);
  /* If the type is complete, there are no derivations.  */
  if (TYPE_FINAL_P (outer_type->type))
    context.maybe_derived_type = false;

  /* Initialize query cache.  */
  if (!cached_polymorphic_call_targets)
    {
      cached_polymorphic_call_targets = new hash_set<cgraph_node *>;
      polymorphic_call_target_hash
       	= new polymorphic_call_target_hash_type (23);
      if (!node_removal_hook_holder)
	{
	  node_removal_hook_holder =
	    symtab->add_cgraph_removal_hook (&devirt_node_removal_hook, NULL);
	  symtab->add_varpool_removal_hook (&devirt_variable_node_removal_hook,
					 NULL);
	}
    }

  /* Lookup cached answer.  */
  key.type = type;
  key.otr_token = otr_token;
  key.context = context;
  slot = polymorphic_call_target_hash->find_slot (&key, INSERT);
  if (cache_token)
   *cache_token = (void *)*slot;
  if (*slot)
    {
      if (completep)
	*completep = (*slot)->complete;
      if (speculative_targetsp)
	*speculative_targetsp = (*slot)->speculative_targets;
      if ((*slot)->type_warning && final_warning_records)
	{
	  final_warning_records->type_warnings[(*slot)->type_warning - 1].count++;
	  final_warning_records->type_warnings[(*slot)->type_warning - 1].dyn_count
	    += final_warning_records->dyn_count;
	}
      if ((*slot)->decl_warning && final_warning_records)
	{
	  struct decl_warn_count *c =
	     final_warning_records->decl_warnings.get ((*slot)->decl_warning);
	  c->count++;
	  c->dyn_count += final_warning_records->dyn_count;
	}
      return (*slot)->targets;
    }

  complete = true;

  /* Do actual search.  */
  timevar_push (TV_IPA_VIRTUAL_CALL);
  *slot = XCNEW (polymorphic_call_target_d);
  if (cache_token)
    *cache_token = (void *)*slot;
  (*slot)->type = type;
  (*slot)->otr_token = otr_token;
  (*slot)->context = context;
  (*slot)->speculative_targets = 0;

  hash_set<tree> inserted;
  hash_set<tree> matched_vtables;

  /* First insert targets we speculatively identified as likely.  */
  if (context.speculative_outer_type)
    {
      odr_type speculative_outer_type;
      bool speculation_complete = true;

      /* First insert target from type itself and check if it may have derived types.  */
      speculative_outer_type = get_odr_type (context.speculative_outer_type, true);
      if (TYPE_FINAL_P (speculative_outer_type->type))
	context.speculative_maybe_derived_type = false;
      binfo = get_binfo_at_offset (TYPE_BINFO (speculative_outer_type->type),
				   context.speculative_offset, otr_type);
      if (binfo)
	target = gimple_get_virt_method_for_binfo (otr_token, binfo,
						   &can_refer);
      else
	target = NULL;

      /* In the case we get complete method, we don't need 
	 to walk derivations.  */
      if (target && DECL_FINAL_P (target))
	context.speculative_maybe_derived_type = false;
      if (type_possibly_instantiated_p (speculative_outer_type->type))
	maybe_record_node (nodes, target, &inserted, can_refer, &speculation_complete);
      if (binfo)
	matched_vtables.add (BINFO_VTABLE (binfo));


      /* Next walk recursively all derived types.  */
      if (context.speculative_maybe_derived_type)
	for (i = 0; i < speculative_outer_type->derived_types.length(); i++)
	  possible_polymorphic_call_targets_1 (nodes, &inserted,
					       &matched_vtables,
					       otr_type,
					       speculative_outer_type->derived_types[i],
					       otr_token, speculative_outer_type->type,
					       context.speculative_offset,
					       &speculation_complete,
					       bases_to_consider,
					       false);
      (*slot)->speculative_targets = nodes.length();
    }

  /* First see virtual method of type itself.  */
  binfo = get_binfo_at_offset (TYPE_BINFO (outer_type->type),
			       context.offset, otr_type);
  if (binfo)
    target = gimple_get_virt_method_for_binfo (otr_token, binfo,
					       &can_refer);
  else
    {
      gcc_assert (odr_violation_reported);
      target = NULL;
    }

  /* Destructors are never called through construction virtual tables,
     because the type is always known.  */
  if (target && DECL_CXX_DESTRUCTOR_P (target))
    context.maybe_in_construction = false;

  if (target)
    {
      /* In the case we get complete method, we don't need 
	 to walk derivations.  */
      if (DECL_FINAL_P (target))
	context.maybe_derived_type = false;
    }

  /* If OUTER_TYPE is abstract, we know we are not seeing its instance.  */
  if (type_possibly_instantiated_p (outer_type->type))
    maybe_record_node (nodes, target, &inserted, can_refer, &complete);
  else
    skipped = true;

  if (binfo)
    matched_vtables.add (BINFO_VTABLE (binfo));

  /* Next walk recursively all derived types.  */
  if (context.maybe_derived_type)
    {
      for (i = 0; i < outer_type->derived_types.length(); i++)
	possible_polymorphic_call_targets_1 (nodes, &inserted,
					     &matched_vtables,
					     otr_type,
					     outer_type->derived_types[i],
					     otr_token, outer_type->type,
					     context.offset, &complete,
					     bases_to_consider,
					     context.maybe_in_construction);

      if (!outer_type->all_derivations_known)
	{
	  if (final_warning_records)
	    {
	      if (complete
		  && nodes.length () == 1
		  && warn_suggest_final_types
		  && !outer_type->derived_types.length ())
		{
		  if (outer_type->id >= (int)final_warning_records->type_warnings.length ())
	            final_warning_records->type_warnings.safe_grow_cleared
		      (odr_types.length ());
		  final_warning_records->type_warnings[outer_type->id].count++;
		  final_warning_records->type_warnings[outer_type->id].dyn_count
		    += final_warning_records->dyn_count;
		  final_warning_records->type_warnings[outer_type->id].type
		    = outer_type->type;
		  (*slot)->type_warning = outer_type->id + 1;
		}
	      if (complete
		  && warn_suggest_final_methods
		  && nodes.length () == 1
		  && types_same_for_odr (DECL_CONTEXT (nodes[0]->decl),
					 outer_type->type))
		{
		  bool existed;
		  struct decl_warn_count &c =
		     final_warning_records->decl_warnings.get_or_insert
			(nodes[0]->decl, &existed);

		  if (existed)
		    {
		      c.count++;
		      c.dyn_count += final_warning_records->dyn_count;
		    }
		  else
		    {
		      c.count = 1;
		      c.dyn_count = final_warning_records->dyn_count;
		      c.decl = nodes[0]->decl;
		    }
		  (*slot)->decl_warning = nodes[0]->decl;
		}
	    }
	  complete = false;
	}
    }

  /* Finally walk bases, if asked to.  */
  if (!(*slot)->speculative_targets)
    (*slot)->speculative_targets = nodes.length();

  /* Destructors are never called through construction virtual tables,
     because the type is always known.  One of entries may be cxa_pure_virtual
     so look to at least two of them.  */
  if (context.maybe_in_construction)
    for (i =0 ; i < MIN (nodes.length (), 2); i++)
      if (DECL_CXX_DESTRUCTOR_P (nodes[i]->decl))
	context.maybe_in_construction = false;
  if (context.maybe_in_construction)
    {
      if (type != outer_type
	  && (!skipped
	      || (context.maybe_derived_type
	          && !type_all_derivations_known_p (outer_type->type))))
	record_targets_from_bases (otr_type, otr_token, outer_type->type,
				   context.offset, nodes, &inserted,
				   &matched_vtables, &complete);
      if (skipped)
        maybe_record_node (nodes, target, &inserted, can_refer, &complete);
      for (i = 0; i < bases_to_consider.length(); i++)
        maybe_record_node (nodes, bases_to_consider[i], &inserted, can_refer, &complete);
    }
  bases_to_consider.release();

  (*slot)->targets = nodes;
  (*slot)->complete = complete;
  if (completep)
    *completep = complete;
  if (speculative_targetsp)
    *speculative_targetsp = (*slot)->speculative_targets;

  timevar_pop (TV_IPA_VIRTUAL_CALL);
  return nodes;
}

bool
add_decl_warning (const tree &key ATTRIBUTE_UNUSED, const decl_warn_count &value,
		  vec<const decl_warn_count*> *vec)
{
  vec->safe_push (&value);
  return true;
}

/* Dump all possible targets of a polymorphic call.  */

void
dump_possible_polymorphic_call_targets (FILE *f,
					tree otr_type,
					HOST_WIDE_INT otr_token,
					const ipa_polymorphic_call_context &ctx)
{
  vec <cgraph_node *> targets;
  bool final;
  odr_type type = get_odr_type (TYPE_MAIN_VARIANT (otr_type), false);
  unsigned int i;
  int speculative;

  if (!type)
    return;
  targets = possible_polymorphic_call_targets (otr_type, otr_token,
					       ctx,
					       &final, NULL, &speculative);
  fprintf (f, "  Targets of polymorphic call of type %i:", type->id);
  print_generic_expr (f, type->type, TDF_SLIM);
  fprintf (f, " token %i\n", (int)otr_token);

  ctx.dump (f);

  fprintf (f, "    %s%s%s%s\n      ",
	   final ? "This is a complete list." :
	   "This is partial list; extra targets may be defined in other units.",
	   ctx.maybe_in_construction ? " (base types included)" : "",
	   ctx.maybe_derived_type ? " (derived types included)" : "",
	   ctx.speculative_maybe_derived_type ? " (speculative derived types included)" : "");
  for (i = 0; i < targets.length (); i++)
    {
      char *name = NULL;
      if (i == (unsigned)speculative)
	fprintf (f, "\n     Targets that are not likely:\n"
		 "      ");
      if (in_lto_p)
	name = cplus_demangle_v3 (targets[i]->asm_name (), 0);
      fprintf (f, " %s/%i", name ? name : targets[i]->name (), targets[i]->order);
      if (in_lto_p)
	free (name);
      if (!targets[i]->definition)
	fprintf (f, " (no definition%s)",
		 DECL_DECLARED_INLINE_P (targets[i]->decl)
		 ? " inline" : "");
    }
  fprintf (f, "\n\n");
}


/* Return true if N can be possibly target of a polymorphic call of
   OTR_TYPE/OTR_TOKEN.  */

bool
possible_polymorphic_call_target_p (tree otr_type,
				    HOST_WIDE_INT otr_token,
				    const ipa_polymorphic_call_context &ctx,
				    struct cgraph_node *n)
{
  vec <cgraph_node *> targets;
  unsigned int i;
  enum built_in_function fcode;
  bool final;

  if (TREE_CODE (TREE_TYPE (n->decl)) == FUNCTION_TYPE
      && ((fcode = DECL_FUNCTION_CODE (n->decl))
	  == BUILT_IN_UNREACHABLE
          || fcode == BUILT_IN_TRAP))
    return true;

  if (!odr_hash)
    return true;
  targets = possible_polymorphic_call_targets (otr_type, otr_token, ctx, &final);
  for (i = 0; i < targets.length (); i++)
    if (n->semantically_equivalent_p (targets[i]))
      return true;

  /* At a moment we allow middle end to dig out new external declarations
     as a targets of polymorphic calls.  */
  if (!final && !n->definition)
    return true;
  return false;
}



/* Return true if N can be possibly target of a polymorphic call of
   OBJ_TYPE_REF expression REF in STMT.  */

bool
possible_polymorphic_call_target_p (tree ref,
				    gimple stmt,
				    struct cgraph_node *n)
{
  ipa_polymorphic_call_context context (current_function_decl, ref, stmt);
  tree call_fn = gimple_call_fn (stmt);

  return possible_polymorphic_call_target_p (obj_type_ref_class (call_fn),
					     tree_to_uhwi
					       (OBJ_TYPE_REF_TOKEN (call_fn)),
					     context,
					     n);
}


/* After callgraph construction new external nodes may appear.
   Add them into the graph.  */

void
update_type_inheritance_graph (void)
{
  struct cgraph_node *n;

  if (!odr_hash)
    return;
  free_polymorphic_call_targets_hash ();
  timevar_push (TV_IPA_INHERITANCE);
  /* We reconstruct the graph starting from types of all methods seen in the
     the unit.  */
  FOR_EACH_FUNCTION (n)
    if (DECL_VIRTUAL_P (n->decl)
	&& !n->definition
	&& n->real_symbol_p ())
      get_odr_type (method_class_type (TYPE_MAIN_VARIANT (TREE_TYPE (n->decl))),
				       true);
  timevar_pop (TV_IPA_INHERITANCE);
}


/* Return true if N looks like likely target of a polymorphic call.
   Rule out cxa_pure_virtual, noreturns, function declared cold and
   other obvious cases.  */

bool
likely_target_p (struct cgraph_node *n)
{
  int flags;
  /* cxa_pure_virtual and similar things are not likely.  */
  if (TREE_CODE (TREE_TYPE (n->decl)) != METHOD_TYPE)
    return false;
  flags = flags_from_decl_or_type (n->decl);
  if (flags & ECF_NORETURN)
    return false;
  if (lookup_attribute ("cold",
			DECL_ATTRIBUTES (n->decl)))
    return false;
  if (n->frequency < NODE_FREQUENCY_NORMAL)
    return false;
  /* If there are no virtual tables refering the target alive,
     the only way the target can be called is an instance comming from other
     compilation unit; speculative devirtualization is build around an
     assumption that won't happen.  */
  if (!referenced_from_vtable_p (n))
    return false;
  return true;
}

/* Compare type warning records P1 and P2 and chose one with larger count;
   helper for qsort.  */

int
type_warning_cmp (const void *p1, const void *p2)
{
  const odr_type_warn_count *t1 = (const odr_type_warn_count *)p1;
  const odr_type_warn_count *t2 = (const odr_type_warn_count *)p2;

  if (t1->dyn_count < t2->dyn_count)
   return 1;
  if (t1->dyn_count > t2->dyn_count)
   return -1;
  return t2->count - t1->count;
}

/* Compare decl warning records P1 and P2 and chose one with larger count;
   helper for qsort.  */

int
decl_warning_cmp (const void *p1, const void *p2)
{
  const decl_warn_count *t1 = *(const decl_warn_count * const *)p1;
  const decl_warn_count *t2 = *(const decl_warn_count * const *)p2;

  if (t1->dyn_count < t2->dyn_count)
   return 1;
  if (t1->dyn_count > t2->dyn_count)
   return -1;
  return t2->count - t1->count;
}

/* The ipa-devirt pass.
   When polymorphic call has only one likely target in the unit,
   turn it into speculative call.  */

static unsigned int
ipa_devirt (void)
{
  struct cgraph_node *n;
  hash_set<void *> bad_call_targets;
  struct cgraph_edge *e;

  int npolymorphic = 0, nspeculated = 0, nconverted = 0, ncold = 0;
  int nmultiple = 0, noverwritable = 0, ndevirtualized = 0, nnotdefined = 0;
  int nwrong = 0, nok = 0, nexternal = 0, nartificial = 0;

  if (!odr_types_ptr)
    return 0;

  /* We can output -Wsuggest-final-methods and -Wsuggest-final-types warnings.
     This is implemented by setting up final_warning_records that are updated
     by get_polymorphic_call_targets.
     We need to clear cache in this case to trigger recomputation of all
     entries.  */
  if (warn_suggest_final_methods || warn_suggest_final_types)
    {
      final_warning_records = new (final_warning_record);
      final_warning_records->type_warnings = vNULL;
      final_warning_records->type_warnings.safe_grow_cleared (odr_types.length ());
      free_polymorphic_call_targets_hash ();
    }

  FOR_EACH_DEFINED_FUNCTION (n)
    {	
      bool update = false;
      if (dump_file && n->indirect_calls)
	fprintf (dump_file, "\n\nProcesing function %s/%i\n",
		 n->name (), n->order);
      for (e = n->indirect_calls; e; e = e->next_callee)
	if (e->indirect_info->polymorphic)
	  {
	    struct cgraph_node *likely_target = NULL;
	    void *cache_token;
	    bool final;
	    int speculative_targets;

	    if (final_warning_records)
	      final_warning_records->dyn_count = e->count;

	    vec <cgraph_node *>targets
	       = possible_polymorphic_call_targets
		    (e, &final, &cache_token, &speculative_targets);
	    unsigned int i;

	    if (dump_file)
	      dump_possible_polymorphic_call_targets 
		(dump_file, e);

	    npolymorphic++;

	    if (!flag_devirtualize_speculatively)
	      continue;

	    if (!e->maybe_hot_p ())
	      {
		if (dump_file)
		  fprintf (dump_file, "Call is cold\n\n");
		ncold++;
		continue;
	      }
	    if (e->speculative)
	      {
		if (dump_file)
		  fprintf (dump_file, "Call is aready speculated\n\n");
		nspeculated++;

		/* When dumping see if we agree with speculation.  */
		if (!dump_file)
		  continue;
	      }
	    if (bad_call_targets.contains (cache_token))
	      {
		if (dump_file)
		  fprintf (dump_file, "Target list is known to be useless\n\n");
		nmultiple++;
		continue;
	      }
	    for (i = 0; i < targets.length (); i++)
	      if (likely_target_p (targets[i]))
		{
		  if (likely_target)
		    {
		      if (i < (unsigned) speculative_targets)
			{
			  likely_target = NULL;
			  if (dump_file)
			    fprintf (dump_file, "More than one likely target\n\n");
			  nmultiple++;
			}
		      break;
		    }
		  likely_target = targets[i];
		}
	    if (!likely_target)
	      {
		bad_call_targets.add (cache_token);
	        continue;
	      }
	    /* This is reached only when dumping; check if we agree or disagree
 	       with the speculation.  */
	    if (e->speculative)
	      {
		struct cgraph_edge *e2;
		struct ipa_ref *ref;
		e->speculative_call_info (e2, e, ref);
		if (e2->callee->ultimate_alias_target ()
		    == likely_target->ultimate_alias_target ())
		  {
		    fprintf (dump_file, "We agree with speculation\n\n");
		    nok++;
		  }
		else
		  {
		    fprintf (dump_file, "We disagree with speculation\n\n");
		    nwrong++;
		  }
		continue;
	      }
	    if (!likely_target->definition)
	      {
		if (dump_file)
		  fprintf (dump_file, "Target is not an definition\n\n");
		nnotdefined++;
		continue;
	      }
	    /* Do not introduce new references to external symbols.  While we
	       can handle these just well, it is common for programs to
	       incorrectly with headers defining methods they are linked
	       with.  */
	    if (DECL_EXTERNAL (likely_target->decl))
	      {
		if (dump_file)
		  fprintf (dump_file, "Target is external\n\n");
		nexternal++;
		continue;
	      }
	    /* Don't use an implicitly-declared destructor (c++/58678).  */
	    struct cgraph_node *non_thunk_target
	      = likely_target->function_symbol ();
	    if (DECL_ARTIFICIAL (non_thunk_target->decl))
	      {
		if (dump_file)
		  fprintf (dump_file, "Target is artificial\n\n");
		nartificial++;
		continue;
	      }
	    if (likely_target->get_availability () <= AVAIL_INTERPOSABLE
		&& likely_target->can_be_discarded_p ())
	      {
		if (dump_file)
		  fprintf (dump_file, "Target is overwritable\n\n");
		noverwritable++;
		continue;
	      }
	    else if (dbg_cnt (devirt))
	      {
		if (dump_enabled_p ())
                  {
                    location_t locus = gimple_location_safe (e->call_stmt);
                    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, locus,
                                     "speculatively devirtualizing call in %s/%i to %s/%i\n",
                                     n->name (), n->order,
                                     likely_target->name (),
                                     likely_target->order);
                  }
		if (!likely_target->can_be_discarded_p ())
		  {
		    cgraph_node *alias;
		    alias = dyn_cast<cgraph_node *> (likely_target->noninterposable_alias ());
		    if (alias)
		      likely_target = alias;
		  }
		nconverted++;
		update = true;
		e->make_speculative
		  (likely_target, e->count * 8 / 10, e->frequency * 8 / 10);
	      }
	  }
      if (update)
	inline_update_overall_summary (n);
    }
  if (warn_suggest_final_methods || warn_suggest_final_types)
    {
      if (warn_suggest_final_types)
	{
	  final_warning_records->type_warnings.qsort (type_warning_cmp);
	  for (unsigned int i = 0;
	       i < final_warning_records->type_warnings.length (); i++)
	    if (final_warning_records->type_warnings[i].count)
	      {
	        tree type = final_warning_records->type_warnings[i].type;
	        int count = final_warning_records->type_warnings[i].count;
	        long long dyn_count
		  = final_warning_records->type_warnings[i].dyn_count;

		if (!dyn_count)
		  warning_n (DECL_SOURCE_LOCATION (TYPE_NAME (type)),
			     OPT_Wsuggest_final_types, count,
			     "Declaring type %qD final "
			     "would enable devirtualization of %i call",
			     "Declaring type %qD final "
			     "would enable devirtualization of %i calls",
			     type,
			     count);
		else
		  warning_n (DECL_SOURCE_LOCATION (TYPE_NAME (type)),
			     OPT_Wsuggest_final_types, count,
			     "Declaring type %qD final "
			     "would enable devirtualization of %i call "
			     "executed %lli times",
			     "Declaring type %qD final "
			     "would enable devirtualization of %i calls "
			     "executed %lli times",
			     type,
			     count,
			     dyn_count);
	      }
	}

      if (warn_suggest_final_methods)
	{
	  vec<const decl_warn_count*> decl_warnings_vec = vNULL;

	  final_warning_records->decl_warnings.traverse
	    <vec<const decl_warn_count *> *, add_decl_warning> (&decl_warnings_vec);
	  decl_warnings_vec.qsort (decl_warning_cmp);
	  for (unsigned int i = 0; i < decl_warnings_vec.length (); i++)
	    {
	      tree decl = decl_warnings_vec[i]->decl;
	      int count = decl_warnings_vec[i]->count;
	      long long dyn_count = decl_warnings_vec[i]->dyn_count;

	      if (!dyn_count)
		if (DECL_CXX_DESTRUCTOR_P (decl))
		  warning_n (DECL_SOURCE_LOCATION (decl),
			      OPT_Wsuggest_final_methods, count,
			      "Declaring virtual destructor of %qD final "
			      "would enable devirtualization of %i call",
			      "Declaring virtual destructor of %qD final "
			      "would enable devirtualization of %i calls",
			      DECL_CONTEXT (decl), count);
		else
		  warning_n (DECL_SOURCE_LOCATION (decl),
			      OPT_Wsuggest_final_methods, count,
			      "Declaring method %qD final "
			      "would enable devirtualization of %i call",
			      "Declaring method %qD final "
			      "would enable devirtualization of %i calls",
			      decl, count);
	       else if (DECL_CXX_DESTRUCTOR_P (decl))
		  warning_n (DECL_SOURCE_LOCATION (decl),
			      OPT_Wsuggest_final_methods, count,
			      "Declaring virtual destructor of %qD final "
			      "would enable devirtualization of %i call "
			      "executed %lli times",
			      "Declaring virtual destructor of %qD final "
			      "would enable devirtualization of %i calls "
			      "executed %lli times",
			      DECL_CONTEXT (decl), count, dyn_count);
		else
		  warning_n (DECL_SOURCE_LOCATION (decl),
			      OPT_Wsuggest_final_methods, count,
			      "Declaring method %qD final "
			      "would enable devirtualization of %i call "
			      "executed %lli times",
			      "Declaring method %qD final "
			      "would enable devirtualization of %i calls "
			      "executed %lli times",
			      decl, count, dyn_count);
	    }
	}
	
      delete (final_warning_records);
      final_warning_records = 0;
    }

  if (dump_file)
    fprintf (dump_file,
	     "%i polymorphic calls, %i devirtualized,"
	     " %i speculatively devirtualized, %i cold\n"
	     "%i have multiple targets, %i overwritable,"
	     " %i already speculated (%i agree, %i disagree),"
	     " %i external, %i not defined, %i artificial\n",
	     npolymorphic, ndevirtualized, nconverted, ncold,
	     nmultiple, noverwritable, nspeculated, nok, nwrong,
	     nexternal, nnotdefined, nartificial);
  return ndevirtualized ? TODO_remove_functions : 0;
}

namespace {

const pass_data pass_data_ipa_devirt =
{
  IPA_PASS, /* type */
  "devirt", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_DEVIRT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_dump_symtab ), /* todo_flags_finish */
};

class pass_ipa_devirt : public ipa_opt_pass_d
{
public:
  pass_ipa_devirt (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_devirt, ctxt,
		      NULL, /* generate_summary */
		      NULL, /* write_summary */
		      NULL, /* read_summary */
		      NULL, /* write_optimization_summary */
		      NULL, /* read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0, /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (flag_devirtualize
	      && (flag_devirtualize_speculatively
		  || (warn_suggest_final_methods
		      || warn_suggest_final_types))
	      && optimize);
    }

  virtual unsigned int execute (function *) { return ipa_devirt (); }

}; // class pass_ipa_devirt

} // anon namespace

ipa_opt_pass_d *
make_pass_ipa_devirt (gcc::context *ctxt)
{
  return new pass_ipa_devirt (ctxt);
}

#include "gt-ipa-devirt.h"
