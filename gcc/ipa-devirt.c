/* Basic IPA utilities for type inheritance graph construction and
   devirtualization.
   Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

/* Brief vocabulary:
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
       RECORD_TYPE by the C++ frontend.  It provides information about base
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
       This is callgraph representation of virtual method call.  Every
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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "fold-const.h"
#include "print-tree.h"
#include "calls.h"
#include "ipa-utils.h"
#include "gimple-fold.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "demangle.h"
#include "dbgcnt.h"
#include "gimple-pretty-print.h"
#include "intl.h"
#include "stringpool.h"
#include "attribs.h"

/* Hash based set of pairs of types.  */
struct type_pair
{
  tree first;
  tree second;
};

template <>
struct default_hash_traits <type_pair>
  : typed_noop_remove <type_pair>
{
  GTY((skip)) typedef type_pair value_type;
  GTY((skip)) typedef type_pair compare_type;
  static hashval_t
  hash (type_pair p)
  {
    return TYPE_UID (p.first) ^ TYPE_UID (p.second);
  }
  static const bool empty_zero_p = true;
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

/* HACK alert: this is used to communicate with ipa-inline-transform that
   thunk is being expanded and there is no need to clear the polymorphic
   call target cache.  */
bool thunk_expansion;

static bool odr_types_equivalent_p (tree, tree, bool, bool *,
				    hash_set<type_pair> *,
				    location_t, location_t);
static void warn_odr (tree t1, tree t2, tree st1, tree st2,
		      bool warn, bool *warned, const char *reason);

static bool odr_violation_reported = false;


/* Pointer set of all call targets appearing in the cache.  */
static hash_set<cgraph_node *> *cached_polymorphic_call_targets;

/* The node of type inheritance graph.  For each type unique in
   One Definition Rule (ODR) sense, we produce one node linking all
   main variants of types equivalent to it, bases and derived types.  */

struct GTY(()) odr_type_d
{
  /* leader type.  */
  tree type;
  /* All bases; built only for main variants of types.  */
  vec<odr_type> GTY((skip)) bases;
  /* All derived types with virtual methods seen in unit;
     built only for main variants of types.  */
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
  /* Set when virtual table without RTTI prevailed table with.  */
  bool rtti_broken;
  /* Set when the canonical type is determined using the type name.  */
  bool tbaa_enabled;
};

/* Return TRUE if all derived types of T are known and thus
   we may consider the walk of derived type complete.

   This is typically true only for final anonymous namespace types and types
   defined within functions (that may be COMDAT and thus shared across units,
   but with the same set of derived types).  */

bool
type_all_derivations_known_p (const_tree t)
{
  if (TYPE_FINAL_P (t))
    return true;
  if (flag_ltrans)
    return false;
  /* Non-C++ types may have IDENTIFIER_NODE here, do not crash.  */
  if (!TYPE_NAME (t) || TREE_CODE (TYPE_NAME (t)) != TYPE_DECL)
    return true;
  if (type_in_anonymous_namespace_p (t))
    return true;
  return (decl_function_context (TYPE_NAME (t)) != NULL);
}

/* Return TRUE if type's constructors are all visible.  */

static bool
type_all_ctors_visible_p (tree t)
{
  return !flag_ltrans
	 && symtab->state >= CONSTRUCTION
	 /* We cannot always use type_all_derivations_known_p.
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

/* Hash used to unify ODR types based on their mangled name and for anonymous
   namespace types.  */

struct odr_name_hasher : pointer_hash <odr_type_d>
{
  typedef union tree_node *compare_type;
  static inline hashval_t hash (const odr_type_d *);
  static inline bool equal (const odr_type_d *, const tree_node *);
  static inline void remove (odr_type_d *);
};

static bool
can_be_name_hashed_p (tree t)
{
  return (!in_lto_p || odr_type_p (t));
}

/* Hash type by its ODR name.  */

static hashval_t
hash_odr_name (const_tree t)
{
  gcc_checking_assert (TYPE_MAIN_VARIANT (t) == t);

  /* If not in LTO, all main variants are unique, so we can do
     pointer hash.  */
  if (!in_lto_p)
    return htab_hash_pointer (t);

  /* Anonymous types are unique.  */
  if (type_with_linkage_p (t) && type_in_anonymous_namespace_p (t))
    return htab_hash_pointer (t);

  gcc_checking_assert (TYPE_NAME (t)
		       && DECL_ASSEMBLER_NAME_SET_P (TYPE_NAME (t)));
  return IDENTIFIER_HASH_VALUE (DECL_ASSEMBLER_NAME (TYPE_NAME (t)));
}

/* Return the computed hashcode for ODR_TYPE.  */

inline hashval_t
odr_name_hasher::hash (const odr_type_d *odr_type)
{
  return hash_odr_name (odr_type->type);
}

/* For languages with One Definition Rule, work out if
   types are the same based on their name.

   This is non-trivial for LTO where minor differences in
   the type representation may have prevented type merging
   to merge two copies of otherwise equivalent type.

   Until we start streaming mangled type names, this function works
   only for polymorphic types.
*/

bool
types_same_for_odr (const_tree type1, const_tree type2)
{
  gcc_checking_assert (TYPE_P (type1) && TYPE_P (type2));

  type1 = TYPE_MAIN_VARIANT (type1);
  type2 = TYPE_MAIN_VARIANT (type2);

  if (type1 == type2)
    return true;

  if (!in_lto_p)
    return false;

  /* Anonymous namespace types are never duplicated.  */
  if ((type_with_linkage_p (type1) && type_in_anonymous_namespace_p (type1))
      || (type_with_linkage_p (type2) && type_in_anonymous_namespace_p (type2)))
    return false;

  /* If both type has mangled defined check if they are same.
     Watch for anonymous types which are all mangled as "<anon">.  */
  if (!type_with_linkage_p (type1) || !type_with_linkage_p (type2))
    return false;
  if (type_in_anonymous_namespace_p (type1)
      || type_in_anonymous_namespace_p (type2))
    return false;
  return (DECL_ASSEMBLER_NAME (TYPE_NAME (type1))
	  == DECL_ASSEMBLER_NAME (TYPE_NAME (type2)));
}

/* Return true if we can decide on ODR equivalency.

   In non-LTO it is always decide, in LTO however it depends in the type has
   ODR info attached. */

bool
types_odr_comparable (tree t1, tree t2)
{
  return (!in_lto_p
	  || TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2)
	  || (odr_type_p (TYPE_MAIN_VARIANT (t1))
	      && odr_type_p (TYPE_MAIN_VARIANT (t2))));
}

/* Return true if T1 and T2 are ODR equivalent.  If ODR equivalency is not
   known, be conservative and return false.  */

bool
types_must_be_same_for_odr (tree t1, tree t2)
{
  if (types_odr_comparable (t1, t2))
    return types_same_for_odr (t1, t2);
  else
    return TYPE_MAIN_VARIANT (t1) == TYPE_MAIN_VARIANT (t2);
}

/* If T is compound type, return type it is based on.  */

static tree
compound_type_base (const_tree t)
{
  if (TREE_CODE (t) == ARRAY_TYPE
      || POINTER_TYPE_P (t)
      || TREE_CODE (t) == COMPLEX_TYPE
      || VECTOR_TYPE_P (t))
    return TREE_TYPE (t);
  if (TREE_CODE (t) == METHOD_TYPE)
    return TYPE_METHOD_BASETYPE (t);
  if (TREE_CODE (t) == OFFSET_TYPE)
    return TYPE_OFFSET_BASETYPE (t);
  return NULL_TREE;
}

/* Return true if T is either ODR type or compound type based from it.
   If the function return true, we know that T is a type originating from C++
   source even at link-time.  */

bool
odr_or_derived_type_p (const_tree t)
{
  do
    {
      if (odr_type_p (TYPE_MAIN_VARIANT (t)))
	return true;
      /* Function type is a tricky one. Basically we can consider it
	 ODR derived if return type or any of the parameters is.
	 We need to check all parameters because LTO streaming merges
	 common types (such as void) and they are not considered ODR then.  */
      if (TREE_CODE (t) == FUNCTION_TYPE)
	{
	  if (TYPE_METHOD_BASETYPE (t))
	    t = TYPE_METHOD_BASETYPE (t);
	  else
	   {
	     if (TREE_TYPE (t) && odr_or_derived_type_p (TREE_TYPE (t)))
	       return true;
	     for (t = TYPE_ARG_TYPES (t); t; t = TREE_CHAIN (t))
	       if (odr_or_derived_type_p (TYPE_MAIN_VARIANT (TREE_VALUE (t))))
		 return true;
	     return false;
	   }
	}
      else
	t = compound_type_base (t);
    }
  while (t);
  return t;
}

/* Compare types T1 and T2 and return true if they are
   equivalent.  */

inline bool
odr_name_hasher::equal (const odr_type_d *o1, const tree_node *t2)
{
  tree t1 = o1->type;

  gcc_checking_assert (TYPE_MAIN_VARIANT (t2) == t2);
  gcc_checking_assert (TYPE_MAIN_VARIANT (t1) == t1);
  if (t1 == t2)
    return true;
  if (!in_lto_p)
    return false;
  /* Check for anonymous namespaces.  */
  if ((type_with_linkage_p (t1) && type_in_anonymous_namespace_p (t1))
      || (type_with_linkage_p (t2) && type_in_anonymous_namespace_p (t2)))
    return false;
  gcc_checking_assert (DECL_ASSEMBLER_NAME (TYPE_NAME (t1)));
  gcc_checking_assert (DECL_ASSEMBLER_NAME (TYPE_NAME (t2)));
  return (DECL_ASSEMBLER_NAME (TYPE_NAME (t1))
	  == DECL_ASSEMBLER_NAME (TYPE_NAME (t2)));
}

/* Free ODR type V.  */

inline void
odr_name_hasher::remove (odr_type_d *v)
{
  v->bases.release ();
  v->derived_types.release ();
  if (v->types_set)
    delete v->types_set;
  ggc_free (v);
}

/* ODR type hash used to look up ODR type based on tree type node.  */

typedef hash_table<odr_name_hasher> odr_hash_type;
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

/* Return true if type variants match.
   This assumes that we already verified that T1 and T2 are variants of the
   same type.  */

static bool
type_variants_equivalent_p (tree t1, tree t2)
{
  if (TYPE_QUALS (t1) != TYPE_QUALS (t2))
    return false;

  if (comp_type_attributes (t1, t2) != 1)
    return false;

  if (COMPLETE_TYPE_P (t1) && COMPLETE_TYPE_P (t2)
      && TYPE_ALIGN (t1) != TYPE_ALIGN (t2))
    return false;

  return true;
}

/* Compare T1 and T2 based on name or structure.  */

static bool
odr_subtypes_equivalent_p (tree t1, tree t2,
			   hash_set<type_pair> *visited,
			   location_t loc1, location_t loc2)
{

  /* This can happen in incomplete types that should be handled earlier.  */
  gcc_assert (t1 && t2);

  if (t1 == t2)
    return true;

  /* Anonymous namespace types must match exactly.  */
  if ((type_with_linkage_p (TYPE_MAIN_VARIANT (t1))
       && type_in_anonymous_namespace_p (TYPE_MAIN_VARIANT (t1)))
      || (type_with_linkage_p (TYPE_MAIN_VARIANT (t2))
	  && type_in_anonymous_namespace_p (TYPE_MAIN_VARIANT (t2))))
    return false;

  /* For ODR types be sure to compare their names.
     To support -Wno-odr-type-merging we allow one type to be non-ODR
     and other ODR even though it is a violation.  */
  if (types_odr_comparable (t1, t2))
    {
      if (t1 != t2
	  && odr_type_p (TYPE_MAIN_VARIANT (t1))
	  && get_odr_type (TYPE_MAIN_VARIANT (t1), true)->odr_violated)
	return false;
      if (!types_same_for_odr (t1, t2))
        return false;
      if (!type_variants_equivalent_p (t1, t2))
	return false;
      /* Limit recursion: If subtypes are ODR types and we know
	 that they are same, be happy.  */
      if (odr_type_p (TYPE_MAIN_VARIANT (t1)))
        return true;
    }

  /* Component types, builtins and possibly violating ODR types
     have to be compared structurally.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return false;
  if (AGGREGATE_TYPE_P (t1)
      && (TYPE_NAME (t1) == NULL_TREE) != (TYPE_NAME (t2) == NULL_TREE))
    return false;

  type_pair pair={TYPE_MAIN_VARIANT (t1), TYPE_MAIN_VARIANT (t2)};
  if (TYPE_UID (TYPE_MAIN_VARIANT (t1)) > TYPE_UID (TYPE_MAIN_VARIANT (t2)))
    {
      pair.first = TYPE_MAIN_VARIANT (t2);
      pair.second = TYPE_MAIN_VARIANT (t1);
    }
  if (visited->add (pair))
    return true;
  if (!odr_types_equivalent_p (TYPE_MAIN_VARIANT (t1), TYPE_MAIN_VARIANT (t2),
			      false, NULL, visited, loc1, loc2))
    return false;
  if (!type_variants_equivalent_p (t1, t2))
    return false;
  return true;
}

/* Return true if DECL1 and DECL2 are identical methods.  Consider
   name equivalent to name.localalias.xyz.  */

static bool
methods_equal_p (tree decl1, tree decl2)
{
  if (DECL_ASSEMBLER_NAME (decl1) == DECL_ASSEMBLER_NAME (decl2))
    return true;
  const char sep = symbol_table::symbol_suffix_separator ();

  const char *name1 = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl1));
  const char *ptr1 = strchr (name1, sep);
  int len1 = ptr1 ? ptr1 - name1 : strlen (name1);

  const char *name2 = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl2));
  const char *ptr2 = strchr (name2, sep);
  int len2 = ptr2 ? ptr2 - name2 : strlen (name2);

  if (len1 != len2)
    return false;
  return !strncmp (name1, name2, len1);
}

/* Compare two virtual tables, PREVAILING and VTABLE and output ODR
   violation warnings.  */

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
      auto_diagnostic_group d;
      if (warning_at (DECL_SOURCE_LOCATION
			(TYPE_NAME (DECL_CONTEXT (vtable->decl))),
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

  /* If we do not stream ODR type info, do not bother to do useful compare.  */
  if (!TYPE_BINFO (DECL_CONTEXT (vtable->decl))
      || !polymorphic_type_binfo_p (TYPE_BINFO (DECL_CONTEXT (vtable->decl))))
    return;

  odr_type class_type = get_odr_type (DECL_CONTEXT (vtable->decl), true);

  if (class_type->odr_violated)
    return;

  for (n1 = 0, n2 = 0; true; n1++, n2++)
    {
      struct ipa_ref *ref1, *ref2;
      bool end1, end2;

      end1 = !prevailing->iterate_reference (n1, ref1);
      end2 = !vtable->iterate_reference (n2, ref2);

      /* !DECL_VIRTUAL_P means RTTI entry;
	 We warn when RTTI is lost because non-RTTI prevails; we silently
	 accept the other case.  */
      while (!end2
	     && (end1
	         || (methods_equal_p (ref1->referred->decl,
				      ref2->referred->decl)
	             && TREE_CODE (ref1->referred->decl) == FUNCTION_DECL))
	     && TREE_CODE (ref2->referred->decl) != FUNCTION_DECL)
	{
	  if (!class_type->rtti_broken)
	    {
	      auto_diagnostic_group d;
	      if (warning_at (DECL_SOURCE_LOCATION
				  (TYPE_NAME (DECL_CONTEXT (vtable->decl))),
				OPT_Wodr,
				"virtual table of type %qD contains RTTI "
				"information",
				DECL_CONTEXT (vtable->decl)))
		{
		  inform (DECL_SOURCE_LOCATION
			      (TYPE_NAME (DECL_CONTEXT (prevailing->decl))),
			    "but is prevailed by one without from other"
			    " translation unit");
		  inform (DECL_SOURCE_LOCATION
			      (TYPE_NAME (DECL_CONTEXT (prevailing->decl))),
			    "RTTI will not work on this type");
		  class_type->rtti_broken = true;
		}
	    }
	  n2++;
          end2 = !vtable->iterate_reference (n2, ref2);
	}
      while (!end1
	     && (end2
	         || (methods_equal_p (ref2->referred->decl, ref1->referred->decl)
	             && TREE_CODE (ref2->referred->decl) == FUNCTION_DECL))
	     && TREE_CODE (ref1->referred->decl) != FUNCTION_DECL)
	{
	  n1++;
          end1 = !prevailing->iterate_reference (n1, ref1);
	}

      /* Finished?  */
      if (end1 && end2)
	{
	  /* Extra paranoia; compare the sizes.  We do not have information
	     about virtual inheritance offsets, so just be sure that these
	     match. 
	     Do this as very last check so the not very informative error
	     is not output too often.  */
	  if (DECL_SIZE (prevailing->decl) != DECL_SIZE (vtable->decl))
	    {
	      class_type->odr_violated = true;
	      auto_diagnostic_group d;
	      tree ctx = TYPE_NAME (DECL_CONTEXT (vtable->decl));
	      if (warning_at (DECL_SOURCE_LOCATION (ctx), OPT_Wodr,
			      "virtual table of type %qD violates "
			      "one definition rule",
			      DECL_CONTEXT (vtable->decl)))
		{
		  ctx = TYPE_NAME (DECL_CONTEXT (prevailing->decl));
		  inform (DECL_SOURCE_LOCATION (ctx),
			  "the conflicting type defined in another translation"
			  " unit has virtual table of different size");
		}
	    }
	  return;
	}

      if (!end1 && !end2)
	{
	  if (methods_equal_p (ref1->referred->decl, ref2->referred->decl))
	    continue;

	  class_type->odr_violated = true;

	  /* If the loops above stopped on non-virtual pointer, we have
	     mismatch in RTTI information mangling.  */
	  if (TREE_CODE (ref1->referred->decl) != FUNCTION_DECL
	      && TREE_CODE (ref2->referred->decl) != FUNCTION_DECL)
	    {
	      auto_diagnostic_group d;
	      if (warning_at (DECL_SOURCE_LOCATION
				(TYPE_NAME (DECL_CONTEXT (vtable->decl))),
			      OPT_Wodr,
			      "virtual table of type %qD violates "
			      "one definition rule",
			      DECL_CONTEXT (vtable->decl)))
		{
		  inform (DECL_SOURCE_LOCATION
			    (TYPE_NAME (DECL_CONTEXT (prevailing->decl))),
			  "the conflicting type defined in another translation "
			  "unit with different RTTI information");
		}
	      return;
	    }
	  /* At this point both REF1 and REF2 points either to virtual table
	     or virtual method.  If one points to virtual table and other to
	     method we can complain the same way as if one table was shorter
	     than other pointing out the extra method.  */
	  if (TREE_CODE (ref1->referred->decl)
	      != TREE_CODE (ref2->referred->decl))
	    {
	      if (VAR_P (ref1->referred->decl))
		end1 = true;
	      else if (VAR_P (ref2->referred->decl))
		end2 = true;
	    }
	}

      class_type->odr_violated = true;

      /* Complain about size mismatch.  Either we have too many virtual
 	 functions or too many virtual table pointers.  */
      if (end1 || end2)
	{
	  if (end1)
	    {
	      varpool_node *tmp = prevailing;
	      prevailing = vtable;
	      vtable = tmp;
	      ref1 = ref2;
	    }
	  auto_diagnostic_group d;
	  if (warning_at (DECL_SOURCE_LOCATION
			    (TYPE_NAME (DECL_CONTEXT (vtable->decl))),
			  OPT_Wodr,
			  "virtual table of type %qD violates "
			  "one definition rule",
			  DECL_CONTEXT (vtable->decl)))
	    {
	      if (TREE_CODE (ref1->referring->decl) == FUNCTION_DECL)
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
	      else
		{
		  inform (DECL_SOURCE_LOCATION
			   (TYPE_NAME (DECL_CONTEXT (prevailing->decl))),
			  "the conflicting type defined in another translation "
			  "unit has virtual table with more entries");
		}
	    }
	  return;
	}

      /* And in the last case we have either mismatch in between two virtual
	 methods or two virtual table pointers.  */
      auto_diagnostic_group d;
      if (warning_at (DECL_SOURCE_LOCATION
			(TYPE_NAME (DECL_CONTEXT (vtable->decl))), OPT_Wodr,
		      "virtual table of type %qD violates "
		      "one definition rule",
		      DECL_CONTEXT (vtable->decl)))
	{
	  if (TREE_CODE (ref1->referred->decl) == FUNCTION_DECL)
	    {
	      inform (DECL_SOURCE_LOCATION
			(TYPE_NAME (DECL_CONTEXT (prevailing->decl))),
		      "the conflicting type defined in another translation "
		      "unit");
	      gcc_assert (TREE_CODE (ref2->referred->decl)
			  == FUNCTION_DECL);
	      inform (DECL_SOURCE_LOCATION
			 (ref1->referred->ultimate_alias_target ()->decl),
		      "virtual method %qD",
		      ref1->referred->ultimate_alias_target ()->decl);
	      inform (DECL_SOURCE_LOCATION
			 (ref2->referred->ultimate_alias_target ()->decl),
		      "ought to match virtual method %qD but does not",
		      ref2->referred->ultimate_alias_target ()->decl);
	    }
	  else
	    inform (DECL_SOURCE_LOCATION
		      (TYPE_NAME (DECL_CONTEXT (prevailing->decl))),
		    "the conflicting type defined in another translation "
		    "unit has virtual table with different contents");
	  return;
	}
    }
}

/* Output ODR violation warning about T1 and T2 with REASON.
   Display location of ST1 and ST2 if REASON speaks about field or
   method of the type.
   If WARN is false, do nothing. Set WARNED if warning was indeed
   output.  */

static void
warn_odr (tree t1, tree t2, tree st1, tree st2,
	  bool warn, bool *warned, const char *reason)
{
  tree decl2 = TYPE_NAME (TYPE_MAIN_VARIANT (t2));
  if (warned)
    *warned = false;

  if (!warn || !TYPE_NAME(TYPE_MAIN_VARIANT (t1)))
    return;

  /* ODR warnings are output during LTO streaming; we must apply location
     cache for potential warnings to be output correctly.  */
  if (lto_location_cache::current_cache)
    lto_location_cache::current_cache->apply_location_cache ();

  auto_diagnostic_group d;
  if (t1 != TYPE_MAIN_VARIANT (t1)
      && TYPE_NAME (t1) != TYPE_NAME (TYPE_MAIN_VARIANT (t1)))
    {
      if (!warning_at (DECL_SOURCE_LOCATION (TYPE_NAME (TYPE_MAIN_VARIANT (t1))),
		       OPT_Wodr, "type %qT (typedef of %qT) violates the "
		       "C++ One Definition Rule",
		       t1, TYPE_MAIN_VARIANT (t1)))
	return;
    }
  else
    {
      if (!warning_at (DECL_SOURCE_LOCATION (TYPE_NAME (TYPE_MAIN_VARIANT (t1))),
		       OPT_Wodr, "type %qT violates the C++ One Definition Rule",
		       t1))
	return;
    }
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

/* Return true if T1 and T2 are incompatible and we want to recursively
   dive into them from warn_type_mismatch to give sensible answer.  */

static bool
type_mismatch_p (tree t1, tree t2)
{
  if (odr_or_derived_type_p (t1) && odr_or_derived_type_p (t2)
      && !odr_types_equivalent_p (t1, t2))
    return true;
  return !types_compatible_p (t1, t2);
}


/* Types T1 and T2 was found to be incompatible in a context they can't
   (either used to declare a symbol of same assembler name or unified by
   ODR rule).  We already output warning about this, but if possible, output
   extra information on how the types mismatch.

   This is hard to do in general.  We basically handle the common cases.

   If LOC1 and LOC2 are meaningful locations, use it in the case the types
   themselves do not have one.  */

void
warn_types_mismatch (tree t1, tree t2, location_t loc1, location_t loc2)
{
  /* Location of type is known only if it has TYPE_NAME and the name is
     TYPE_DECL.  */
  location_t loc_t1 = TYPE_NAME (t1) && TREE_CODE (TYPE_NAME (t1)) == TYPE_DECL
		      ? DECL_SOURCE_LOCATION (TYPE_NAME (t1))
		      : UNKNOWN_LOCATION;
  location_t loc_t2 = TYPE_NAME (t2) && TREE_CODE (TYPE_NAME (t2)) == TYPE_DECL
		      ? DECL_SOURCE_LOCATION (TYPE_NAME (t2))
		      : UNKNOWN_LOCATION;
  bool loc_t2_useful = false;

  /* With LTO it is a common case that the location of both types match.
     See if T2 has a location that is different from T1. If so, we will
     inform user about the location.
     Do not consider the location passed to us in LOC1/LOC2 as those are
     already output.  */
  if (loc_t2 > BUILTINS_LOCATION && loc_t2 != loc_t1)
    {
      if (loc_t1 <= BUILTINS_LOCATION)
	loc_t2_useful = true;
      else
	{
	  expanded_location xloc1 = expand_location (loc_t1);
	  expanded_location xloc2 = expand_location (loc_t2);

	  if (strcmp (xloc1.file, xloc2.file)
	      || xloc1.line != xloc2.line
	      || xloc1.column != xloc2.column)
	    loc_t2_useful = true;
	}
    }

  if (loc_t1 <= BUILTINS_LOCATION)
    loc_t1 = loc1;
  if (loc_t2 <= BUILTINS_LOCATION)
    loc_t2 = loc2;

  location_t loc = loc_t1 <= BUILTINS_LOCATION ? loc_t2 : loc_t1;

  /* It is a quite common bug to reference anonymous namespace type in
     non-anonymous namespace class.  */
  tree mt1 = TYPE_MAIN_VARIANT (t1);
  tree mt2 = TYPE_MAIN_VARIANT (t2);
  if ((type_with_linkage_p (mt1)
       && type_in_anonymous_namespace_p (mt1))
      || (type_with_linkage_p (mt2)
	  && type_in_anonymous_namespace_p (mt2)))
    {
      if (!type_with_linkage_p (mt1)
	  || !type_in_anonymous_namespace_p (mt1))
	{
	  std::swap (t1, t2);
	  std::swap (mt1, mt2);
	  std::swap (loc_t1, loc_t2);
	}
      gcc_assert (TYPE_NAME (mt1)
		  && TREE_CODE (TYPE_NAME (mt1)) == TYPE_DECL);
      tree n1 = TYPE_NAME (mt1);
      tree n2 = TYPE_NAME (mt2) ? TYPE_NAME (mt2) : NULL;

      if (TREE_CODE (n1) == TYPE_DECL)
	n1 = DECL_NAME (n1);
      if (n2 && TREE_CODE (n2) == TYPE_DECL)
	n2 = DECL_NAME (n2);
      /* Most of the time, the type names will match, do not be unnecessarily
         verbose.  */
      if (n1 != n2)
        inform (loc_t1,
	        "type %qT defined in anonymous namespace cannot match "
	        "type %qT across the translation unit boundary",
	        t1, t2);
      else
        inform (loc_t1,
	        "type %qT defined in anonymous namespace cannot match "
	        "across the translation unit boundary",
	        t1);
      if (loc_t2_useful)
        inform (loc_t2,
	        "the incompatible type defined in another translation unit");
      return;
    }
  /* If types have mangled ODR names and they are different, it is most
     informative to output those.
     This also covers types defined in different namespaces.  */
  const char *odr1 = get_odr_name_for_type (mt1);
  const char *odr2 = get_odr_name_for_type (mt2);
  if (odr1 != NULL && odr2 != NULL && odr1 != odr2)
    {
      const int opts = DMGL_PARAMS | DMGL_ANSI | DMGL_TYPES;
      char *name1 = xstrdup (cplus_demangle (odr1, opts));
      char *name2 = cplus_demangle (odr2, opts);
      if (name1 && name2 && strcmp (name1, name2))
	{
	  inform (loc_t1,
		  "type name %qs should match type name %qs",
		  name1, name2);
	  if (loc_t2_useful)
	    inform (loc_t2,
		    "the incompatible type is defined here");
	  free (name1);
	  return;
	}
      free (name1);
    }
  /* A tricky case are compound types.  Often they appear the same in source
     code and the mismatch is dragged in by type they are build from.
     Look for those differences in subtypes and try to be informative.  In other
     cases just output nothing because the source code is probably different
     and in this case we already output a all necessary info.  */
  if (!TYPE_NAME (t1) || !TYPE_NAME (t2))
    {
      if (TREE_CODE (t1) == TREE_CODE (t2))
	{
	  if (TREE_CODE (t1) == ARRAY_TYPE
	      && COMPLETE_TYPE_P (t1) && COMPLETE_TYPE_P (t2))
	    {
	      tree i1 = TYPE_DOMAIN (t1);
	      tree i2 = TYPE_DOMAIN (t2);
	
	      if (i1 && i2
		  && TYPE_MAX_VALUE (i1)
		  && TYPE_MAX_VALUE (i2)
		  && !operand_equal_p (TYPE_MAX_VALUE (i1),
				       TYPE_MAX_VALUE (i2), 0))
		{
		  inform (loc,
			  "array types have different bounds");
		  return;
		}
	    }
	  if ((POINTER_TYPE_P (t1) || TREE_CODE (t1) == ARRAY_TYPE)
	      && type_mismatch_p (TREE_TYPE (t1), TREE_TYPE (t2)))
	    warn_types_mismatch (TREE_TYPE (t1), TREE_TYPE (t2), loc_t1, loc_t2);
	  else if (TREE_CODE (t1) == METHOD_TYPE
		   || TREE_CODE (t1) == FUNCTION_TYPE)
	    {
	      tree parms1 = NULL, parms2 = NULL;
	      int count = 1;

	      if (type_mismatch_p (TREE_TYPE (t1), TREE_TYPE (t2)))
		{
		  inform (loc, "return value type mismatch");
		  warn_types_mismatch (TREE_TYPE (t1), TREE_TYPE (t2), loc_t1,
				       loc_t2);
		  return;
		}
	      if (prototype_p (t1) && prototype_p (t2))
		for (parms1 = TYPE_ARG_TYPES (t1), parms2 = TYPE_ARG_TYPES (t2);
		     parms1 && parms2;
		     parms1 = TREE_CHAIN (parms1), parms2 = TREE_CHAIN (parms2),
		     count++)
		  {
		    if (type_mismatch_p (TREE_VALUE (parms1), TREE_VALUE (parms2)))
		      {
			if (count == 1 && TREE_CODE (t1) == METHOD_TYPE)
			  inform (loc,
				  "implicit this pointer type mismatch");
			else
			  inform (loc,
				  "type mismatch in parameter %i",
				  count - (TREE_CODE (t1) == METHOD_TYPE));
			warn_types_mismatch (TREE_VALUE (parms1),
					     TREE_VALUE (parms2),
					     loc_t1, loc_t2);
			return;
		      }
		  }
	      if (parms1 || parms2)
		{
		  inform (loc,
			  "types have different parameter counts");
		  return;
		}
	    }
	}
      return;
    }

  if (types_odr_comparable (t1, t2)
      /* We make assign integers mangled names to be able to handle
	 signed/unsigned chars.  Accepting them here would however lead to
	 confusing message like
	 "type ‘const int’ itself violates the C++ One Definition Rule"  */
      && TREE_CODE (t1) != INTEGER_TYPE
      && types_same_for_odr (t1, t2))
    inform (loc_t1,
	    "type %qT itself violates the C++ One Definition Rule", t1);
  /* Prevent pointless warnings like "struct aa" should match "struct aa".  */
  else if (TYPE_NAME (t1) == TYPE_NAME (t2)
	   && TREE_CODE (t1) == TREE_CODE (t2) && !loc_t2_useful)
    return;
  else
    inform (loc_t1, "type %qT should match type %qT",
	    t1, t2);
  if (loc_t2_useful)
    inform (loc_t2, "the incompatible type is defined here");
}

/* Return true if T should be ignored in TYPE_FIELDS for ODR comparison.  */

static bool
skip_in_fields_list_p (tree t)
{
  if (TREE_CODE (t) != FIELD_DECL)
    return true;
  /* C++ FE introduces zero sized fields depending on -std setting, see
     PR89358.  */
  if (DECL_SIZE (t)
      && integer_zerop (DECL_SIZE (t))
      && DECL_ARTIFICIAL (t)
      && DECL_IGNORED_P (t)
      && !DECL_NAME (t))
    return true;
  return false;
}

/* Compare T1 and T2, report ODR violations if WARN is true and set
   WARNED to true if anything is reported.  Return true if types match.
   If true is returned, the types are also compatible in the sense of
   gimple_canonical_types_compatible_p.
   If LOC1 and LOC2 is not UNKNOWN_LOCATION it may be used to output a warning
   about the type if the type itself do not have location.  */

static bool
odr_types_equivalent_p (tree t1, tree t2, bool warn, bool *warned,
			hash_set<type_pair> *visited,
			location_t loc1, location_t loc2)
{
  /* Check first for the obvious case of pointer identity.  */
  if (t1 == t2)
    return true;

  /* Can't be the same type if the types don't have the same code.  */
  if (TREE_CODE (t1) != TREE_CODE (t2))
    {
      warn_odr (t1, t2, NULL, NULL, warn, warned,
	        G_("a different type is defined in another translation unit"));
      return false;
    }

  if ((type_with_linkage_p (TYPE_MAIN_VARIANT (t1))
       && type_in_anonymous_namespace_p (TYPE_MAIN_VARIANT (t1)))
      || (type_with_linkage_p (TYPE_MAIN_VARIANT (t2))
	  && type_in_anonymous_namespace_p (TYPE_MAIN_VARIANT (t2))))
    {
      /* We cannot trip this when comparing ODR types, only when trying to
	 match different ODR derivations from different declarations.
	 So WARN should be always false.  */
      gcc_assert (!warn);
      return false;
    }

  if (TREE_CODE (t1) == ENUMERAL_TYPE
      && TYPE_VALUES (t1) && TYPE_VALUES (t2))
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
	  if (!operand_equal_p (TREE_VALUE (v1), TREE_VALUE (v2), 0))
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

	  if (!odr_subtypes_equivalent_p (TREE_TYPE (t1), TREE_TYPE (t2),
					  visited, loc1, loc2))
	    {
	      warn_odr (t1, t2, NULL, NULL, warn, warned,
			G_("it is defined as a pointer to different type "
			   "in another translation unit"));
	      if (warn && warned)
	        warn_types_mismatch (TREE_TYPE (t1), TREE_TYPE (t2),
				     loc1, loc2);
	      return false;
	    }
	}

      if ((TREE_CODE (t1) == VECTOR_TYPE || TREE_CODE (t1) == COMPLEX_TYPE)
	  && !odr_subtypes_equivalent_p (TREE_TYPE (t1), TREE_TYPE (t2),
					 visited, loc1, loc2))
	{
	  /* Probably specific enough.  */
	  warn_odr (t1, t2, NULL, NULL, warn, warned,
		    G_("a different type is defined "
		       "in another translation unit"));
	  if (warn && warned)
	    warn_types_mismatch (TREE_TYPE (t1), TREE_TYPE (t2), loc1, loc2);
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
	if (!odr_subtypes_equivalent_p (TREE_TYPE (t1), TREE_TYPE (t2),
					visited, loc1, loc2))
	  {
	    warn_odr (t1, t2, NULL, NULL, warn, warned,
		      G_("a different type is defined in another "
			 "translation unit"));
	    if (warn && warned)
	      warn_types_mismatch (TREE_TYPE (t1), TREE_TYPE (t2), loc1, loc2);
	  }
	gcc_assert (TYPE_STRING_FLAG (t1) == TYPE_STRING_FLAG (t2));
	gcc_assert (TYPE_NONALIASED_COMPONENT (t1)
		    == TYPE_NONALIASED_COMPONENT (t2));

	tree i1 = TYPE_DOMAIN (t1);
	tree i2 = TYPE_DOMAIN (t2);

	/* For an incomplete external array, the type domain can be
	   NULL_TREE.  Check this condition also.  */
	if (i1 == NULL_TREE || i2 == NULL_TREE)
          return type_variants_equivalent_p (t1, t2);

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
      if (!odr_subtypes_equivalent_p (TREE_TYPE (t1), TREE_TYPE (t2),
				      visited, loc1, loc2))
	{
	  warn_odr (t1, t2, NULL, NULL, warn, warned,
		    G_("has different return value "
		       "in another translation unit"));
	  if (warn && warned)
	    warn_types_mismatch (TREE_TYPE (t1), TREE_TYPE (t2), loc1, loc2);
	  return false;
	}

      if (TYPE_ARG_TYPES (t1) == TYPE_ARG_TYPES (t2)
	  || !prototype_p (t1) || !prototype_p (t2))
        return type_variants_equivalent_p (t1, t2);
      else
	{
	  tree parms1, parms2;

	  for (parms1 = TYPE_ARG_TYPES (t1), parms2 = TYPE_ARG_TYPES (t2);
	       parms1 && parms2;
	       parms1 = TREE_CHAIN (parms1), parms2 = TREE_CHAIN (parms2))
	    {
	      if (!odr_subtypes_equivalent_p
		     (TREE_VALUE (parms1), TREE_VALUE (parms2),
		      visited, loc1, loc2))
		{
		  warn_odr (t1, t2, NULL, NULL, warn, warned,
			    G_("has different parameters in another "
			       "translation unit"));
		  if (warn && warned)
		    warn_types_mismatch (TREE_VALUE (parms1),
					 TREE_VALUE (parms2), loc1, loc2);
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

          return type_variants_equivalent_p (t1, t2);
	}

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree f1, f2;

	/* For aggregate types, all the fields must be the same.  */
	if (COMPLETE_TYPE_P (t1) && COMPLETE_TYPE_P (t2))
	  {
	    if (TYPE_BINFO (t1) && TYPE_BINFO (t2)
	        && polymorphic_type_binfo_p (TYPE_BINFO (t1))
		   != polymorphic_type_binfo_p (TYPE_BINFO (t2)))
	      {
		if (polymorphic_type_binfo_p (TYPE_BINFO (t1)))
		  warn_odr (t1, t2, NULL, NULL, warn, warned,
			    G_("a type defined in another translation unit "
			       "is not polymorphic"));
		else
		  warn_odr (t1, t2, NULL, NULL, warn, warned,
			    G_("a type defined in another translation unit "
			       "is polymorphic"));
		return false;
	      }
	    for (f1 = TYPE_FIELDS (t1), f2 = TYPE_FIELDS (t2);
		 f1 || f2;
		 f1 = TREE_CHAIN (f1), f2 = TREE_CHAIN (f2))
	      {
		/* Skip non-fields.  */
		while (f1 && skip_in_fields_list_p (f1))
		  f1 = TREE_CHAIN (f1);
		while (f2 && skip_in_fields_list_p (f2))
		  f2 = TREE_CHAIN (f2);
		if (!f1 || !f2)
		  break;
		if (DECL_VIRTUAL_P (f1) != DECL_VIRTUAL_P (f2))
		  {
		    warn_odr (t1, t2, NULL, NULL, warn, warned,
			      G_("a type with different virtual table pointers"
			         " is defined in another translation unit"));
		    return false;
		  }
		if (DECL_ARTIFICIAL (f1) != DECL_ARTIFICIAL (f2))
		  {
		    warn_odr (t1, t2, NULL, NULL, warn, warned,
			      G_("a type with different bases is defined "
				 "in another translation unit"));
		    return false;
		  }
		if (DECL_NAME (f1) != DECL_NAME (f2)
		    && !DECL_ARTIFICIAL (f1))
		  {
		    warn_odr (t1, t2, f1, f2, warn, warned,
			      G_("a field with different name is defined "
				 "in another translation unit"));
		    return false;
		  }
		if (!odr_subtypes_equivalent_p (TREE_TYPE (f1),
						TREE_TYPE (f2),
						visited, loc1, loc2))
		  {
		    /* Do not warn about artificial fields and just go into
 		       generic field mismatch warning.  */
		    if (DECL_ARTIFICIAL (f1))
		      break;

		    warn_odr (t1, t2, f1, f2, warn, warned,
			      G_("a field of same name but different type "
				 "is defined in another translation unit"));
		    if (warn && warned)
		      warn_types_mismatch (TREE_TYPE (f1), TREE_TYPE (f2), loc1, loc2);
		    return false;
		  }
		if (!gimple_compare_field_offset (f1, f2))
		  {
		    /* Do not warn about artificial fields and just go into
		       generic field mismatch warning.  */
		    if (DECL_ARTIFICIAL (f1))
		      break;
		    warn_odr (t1, t2, f1, f2, warn, warned,
			      G_("fields have different layout "
				 "in another translation unit"));
		    return false;
		  }
		if (DECL_BIT_FIELD (f1) != DECL_BIT_FIELD (f2))
		  {
		    warn_odr (t1, t2, f1, f2, warn, warned,
			      G_("one field is a bitfield while the other "
				 "is not"));
		    return false;
		  }
		else
		  gcc_assert (DECL_NONADDRESSABLE_P (f1)
			      == DECL_NONADDRESSABLE_P (f2));
	      }

	    /* If one aggregate has more fields than the other, they
	       are not the same.  */
	    if (f1 || f2)
	      {
		if ((f1 && DECL_VIRTUAL_P (f1)) || (f2 && DECL_VIRTUAL_P (f2)))
		  warn_odr (t1, t2, NULL, NULL, warn, warned,
			    G_("a type with different virtual table pointers"
			       " is defined in another translation unit"));
		else if ((f1 && DECL_ARTIFICIAL (f1))
		         || (f2 && DECL_ARTIFICIAL (f2)))
		  warn_odr (t1, t2, NULL, NULL, warn, warned,
			    G_("a type with different bases is defined "
			       "in another translation unit"));
		else
		  warn_odr (t1, t2, f1, f2, warn, warned,
			    G_("a type with different number of fields "
			       "is defined in another translation unit"));
		
		return false;
	      }
	  }
	break;
      }
    case VOID_TYPE:
    case NULLPTR_TYPE:
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

  if (TREE_ADDRESSABLE (t1) != TREE_ADDRESSABLE (t2)
      && COMPLETE_TYPE_P (t1) && COMPLETE_TYPE_P (t2))
    {
      warn_odr (t1, t2, NULL, NULL, warn, warned,
		G_("one type needs to be constructed while the other does not"));
      gcc_checking_assert (RECORD_OR_UNION_TYPE_P (t1));
      return false;
    }
  /* There is no really good user facing warning for this.
     Either the original reason for modes being different is lost during
     streaming or we should catch earlier warnings.  We however must detect
     the mismatch to avoid type verifier from cmplaining on mismatched
     types between type and canonical type. See PR91576.  */
  if (TYPE_MODE (t1) != TYPE_MODE (t2)
      && COMPLETE_TYPE_P (t1) && COMPLETE_TYPE_P (t2))
    {
      warn_odr (t1, t2, NULL, NULL, warn, warned,
		G_("memory layout mismatch"));
      return false;
    }

  gcc_assert (!TYPE_SIZE_UNIT (t1) || !TYPE_SIZE_UNIT (t2)
	      || operand_equal_p (TYPE_SIZE_UNIT (t1),
				  TYPE_SIZE_UNIT (t2), 0));
  return type_variants_equivalent_p (t1, t2);
}

/* Return true if TYPE1 and TYPE2 are equivalent for One Definition Rule.  */

bool
odr_types_equivalent_p (tree type1, tree type2)
{
  gcc_checking_assert (odr_or_derived_type_p (type1)
		       && odr_or_derived_type_p (type2));

  hash_set<type_pair> visited;
  return odr_types_equivalent_p (type1, type2, false, NULL,
			         &visited, UNKNOWN_LOCATION, UNKNOWN_LOCATION);
}

/* TYPE is equivalent to VAL by ODR, but its tree representation differs
   from VAL->type.  This may happen in LTO where tree merging did not merge
   all variants of the same type or due to ODR violation.

   Analyze and report ODR violations and add type to duplicate list.
   If TYPE is more specified than VAL->type, prevail VAL->type.  Also if
   this is first time we see definition of a class return true so the
   base types are analyzed.  */

static bool
add_type_duplicate (odr_type val, tree type)
{
  bool build_bases = false;
  bool prevail = false;
  bool odr_must_violate = false;

  if (!val->types_set)
    val->types_set = new hash_set<tree>;

  /* Chose polymorphic type as leader (this happens only in case of ODR
     violations.  */
  if ((TREE_CODE (type) == RECORD_TYPE && TYPE_BINFO (type)
       && polymorphic_type_binfo_p (TYPE_BINFO (type)))
      && (TREE_CODE (val->type) != RECORD_TYPE || !TYPE_BINFO (val->type)
          || !polymorphic_type_binfo_p (TYPE_BINFO (val->type))))
    {
      prevail = true;
      build_bases = true;
    }
  /* Always prefer complete type to be the leader.  */
  else if (!COMPLETE_TYPE_P (val->type) && COMPLETE_TYPE_P (type))
    {
      prevail = true;
      if (TREE_CODE (type) == RECORD_TYPE)
        build_bases = TYPE_BINFO (type);
    }
  else if (COMPLETE_TYPE_P (val->type) && !COMPLETE_TYPE_P (type))
    ;
  else if (TREE_CODE (val->type) == ENUMERAL_TYPE
	   && TREE_CODE (type) == ENUMERAL_TYPE
	   && !TYPE_VALUES (val->type) && TYPE_VALUES (type))
    prevail = true;
  else if (TREE_CODE (val->type) == RECORD_TYPE
	   && TREE_CODE (type) == RECORD_TYPE
	   && TYPE_BINFO (type) && !TYPE_BINFO (val->type))
    {
      gcc_assert (!val->bases.length ());
      build_bases = true;
      prevail = true;
    }

  if (prevail)
    std::swap (val->type, type);

  val->types_set->add (type);

  if (!odr_hash)
    return false;

  gcc_checking_assert (can_be_name_hashed_p (type)
		       && can_be_name_hashed_p (val->type));

  bool merge = true;
  bool base_mismatch = false;
  unsigned int i;
  bool warned = false;
  hash_set<type_pair> visited;

  gcc_assert (in_lto_p);
  vec_safe_push (val->types, type);

  /* If both are class types, compare the bases.  */
  if (COMPLETE_TYPE_P (type) && COMPLETE_TYPE_P (val->type)
      && TREE_CODE (val->type) == RECORD_TYPE
      && TREE_CODE (type) == RECORD_TYPE
      && TYPE_BINFO (val->type) && TYPE_BINFO (type))
    {
      if (BINFO_N_BASE_BINFOS (TYPE_BINFO (type))
	  != BINFO_N_BASE_BINFOS (TYPE_BINFO (val->type)))
	{
	  if (!flag_ltrans && !warned && !val->odr_violated)
	    {
	      tree extra_base;
	      warn_odr (type, val->type, NULL, NULL, !warned, &warned,
			"a type with the same name but different "
			"number of polymorphic bases is "
			"defined in another translation unit");
	      if (warned)
		{
		  if (BINFO_N_BASE_BINFOS (TYPE_BINFO (type))
		      > BINFO_N_BASE_BINFOS (TYPE_BINFO (val->type)))
		    extra_base = BINFO_BASE_BINFO
				 (TYPE_BINFO (type),
				  BINFO_N_BASE_BINFOS (TYPE_BINFO (val->type)));
		  else
		    extra_base = BINFO_BASE_BINFO
				 (TYPE_BINFO (val->type),
				  BINFO_N_BASE_BINFOS (TYPE_BINFO (type)));
		  tree extra_base_type = BINFO_TYPE (extra_base);
		  inform (DECL_SOURCE_LOCATION (TYPE_NAME (extra_base_type)),
			  "the extra base is defined here");
		}
	    }
	  base_mismatch = true;
	}
      else
	for (i = 0; i < BINFO_N_BASE_BINFOS (TYPE_BINFO (type)); i++)
	  {
	    tree base1 = BINFO_BASE_BINFO (TYPE_BINFO (type), i);
	    tree base2 = BINFO_BASE_BINFO (TYPE_BINFO (val->type), i);
	    tree type1 = BINFO_TYPE (base1);
	    tree type2 = BINFO_TYPE (base2);

	    if (types_odr_comparable (type1, type2))
	      {
		if (!types_same_for_odr (type1, type2))
		  base_mismatch = true;
	      }
	    else
	      if (!odr_types_equivalent_p (type1, type2))
		base_mismatch = true;
	    if (base_mismatch)
	      {
		if (!warned && !val->odr_violated)
		  {
		    warn_odr (type, val->type, NULL, NULL,
			      !warned, &warned,
			      "a type with the same name but different base "
			      "type is defined in another translation unit");
		    if (warned)
		      warn_types_mismatch (type1, type2,
					    UNKNOWN_LOCATION, UNKNOWN_LOCATION);
		  }
		break;
	      }
	    if (BINFO_OFFSET (base1) != BINFO_OFFSET (base2))
	      {
		base_mismatch = true;
		if (!warned && !val->odr_violated)
		  warn_odr (type, val->type, NULL, NULL,
			    !warned, &warned,
			    "a type with the same name but different base "
			    "layout is defined in another translation unit");
		break;
	      }
	    /* One of bases is not of complete type.  */
	    if (!TYPE_BINFO (type1) != !TYPE_BINFO (type2))
	      {
		/* If we have a polymorphic type info specified for TYPE1
		   but not for TYPE2 we possibly missed a base when recording
		   VAL->type earlier.
		   Be sure this does not happen.  */
		if (TYPE_BINFO (type1)
		    && polymorphic_type_binfo_p (TYPE_BINFO (type1))
		    && !build_bases)
		  odr_must_violate = true;
	        break;
	      }
	    /* One base is polymorphic and the other not.
	       This ought to be diagnosed earlier, but do not ICE in the
	       checking bellow.  */
	    else if (TYPE_BINFO (type1)
		     && polymorphic_type_binfo_p (TYPE_BINFO (type1))
		        != polymorphic_type_binfo_p (TYPE_BINFO (type2)))
	      {
		if (!warned && !val->odr_violated)
		  warn_odr (type, val->type, NULL, NULL,
			    !warned, &warned,
			    "a base of the type is polymorphic only in one "
			    "translation unit");
		base_mismatch = true;
		break;
	      }
	  }
      if (base_mismatch)
	{
	  merge = false;
	  odr_violation_reported = true;
	  val->odr_violated = true;

	  if (symtab->dump_file)
	    {
	      fprintf (symtab->dump_file, "ODR base violation\n");
	    
	      print_node (symtab->dump_file, "", val->type, 0);
	      putc ('\n',symtab->dump_file);
	      print_node (symtab->dump_file, "", type, 0);
	      putc ('\n',symtab->dump_file);
	    }
	}
    }

  /* Next compare memory layout.
     The DECL_SOURCE_LOCATIONs in this invocation came from LTO streaming.
     We must apply the location cache to ensure that they are valid
     before we can pass them to odr_types_equivalent_p (PR lto/83121).  */
  if (lto_location_cache::current_cache)
    lto_location_cache::current_cache->apply_location_cache ();
  /* As a special case we stream mangles names of integer types so we can see
     if they are believed to be same even though they have different
     representation.  Avoid bogus warning on mismatches in these.  */
  if (TREE_CODE (type) != INTEGER_TYPE
      && TREE_CODE (val->type) != INTEGER_TYPE
      && !odr_types_equivalent_p (val->type, type,
			       !flag_ltrans && !val->odr_violated && !warned,
			       &warned, &visited,
			       DECL_SOURCE_LOCATION (TYPE_NAME (val->type)),
			       DECL_SOURCE_LOCATION (TYPE_NAME (type))))
    {
      merge = false;
      odr_violation_reported = true;
      val->odr_violated = true;
    }
  gcc_assert (val->odr_violated || !odr_must_violate);
  /* Sanity check that all bases will be build same way again.  */
  if (flag_checking
      && COMPLETE_TYPE_P (type) && COMPLETE_TYPE_P (val->type)
      && TREE_CODE (val->type) == RECORD_TYPE
      && TREE_CODE (type) == RECORD_TYPE
      && TYPE_BINFO (val->type) && TYPE_BINFO (type)
      && !val->odr_violated
      && !base_mismatch && val->bases.length ())
    {
      unsigned int num_poly_bases = 0;
      unsigned int j;

      for (i = 0; i < BINFO_N_BASE_BINFOS (TYPE_BINFO (type)); i++)
	if (polymorphic_type_binfo_p (BINFO_BASE_BINFO
					 (TYPE_BINFO (type), i)))
	  num_poly_bases++;
      gcc_assert (num_poly_bases == val->bases.length ());
      for (j = 0, i = 0; i < BINFO_N_BASE_BINFOS (TYPE_BINFO (type));
	   i++)
	if (polymorphic_type_binfo_p (BINFO_BASE_BINFO
				       (TYPE_BINFO (type), i)))
	  {
	    odr_type base = get_odr_type
			       (BINFO_TYPE
				  (BINFO_BASE_BINFO (TYPE_BINFO (type),
						     i)),
				true);
	    gcc_assert (val->bases[j] == base);
	    j++;
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
  return build_bases;
}

/* REF is OBJ_TYPE_REF, return the class the ref corresponds to.  */

tree
obj_type_ref_class (const_tree ref)
{
  gcc_checking_assert (TREE_CODE (ref) == OBJ_TYPE_REF);
  ref = TREE_TYPE (ref);
  gcc_checking_assert (TREE_CODE (ref) == POINTER_TYPE);
  ref = TREE_TYPE (ref);
  /* We look for type THIS points to.  ObjC also builds
     OBJ_TYPE_REF with non-method calls, Their first parameter
     ID however also corresponds to class type. */
  gcc_checking_assert (TREE_CODE (ref) == METHOD_TYPE
		       || TREE_CODE (ref) == FUNCTION_TYPE);
  ref = TREE_VALUE (TYPE_ARG_TYPES (ref));
  gcc_checking_assert (TREE_CODE (ref) == POINTER_TYPE);
  tree ret = TREE_TYPE (ref);
  if (!in_lto_p && !TYPE_STRUCTURAL_EQUALITY_P (ret))
    ret = TYPE_CANONICAL (ret);
  else
    ret = get_odr_type (ret)->type;
  return ret;
}

/* Get ODR type hash entry for TYPE.  If INSERT is true, create
   possibly new entry.  */

odr_type
get_odr_type (tree type, bool insert)
{
  odr_type_d **slot = NULL;
  odr_type val = NULL;
  hashval_t hash;
  bool build_bases = false;
  bool insert_to_odr_array = false;
  int base_id = -1;

  type = TYPE_MAIN_VARIANT (type);
  if (!in_lto_p && !TYPE_STRUCTURAL_EQUALITY_P (type))
    type = TYPE_CANONICAL (type);

  gcc_checking_assert (can_be_name_hashed_p (type));

  hash = hash_odr_name (type);
  slot = odr_hash->find_slot_with_hash (type, hash,
					insert ? INSERT : NO_INSERT);

  if (!slot)
    return NULL;

  /* See if we already have entry for type.  */
  if (*slot)
    {
      val = *slot;

      if (val->type != type && insert
	  && (!val->types_set || !val->types_set->add (type)))
	build_bases = add_type_duplicate (val, type);
    }
  else
    {
      val = ggc_cleared_alloc<odr_type_d> ();
      val->type = type;
      val->bases = vNULL;
      val->derived_types = vNULL;
      if (type_with_linkage_p (type))
        val->anonymous_namespace = type_in_anonymous_namespace_p (type);
      else
	val->anonymous_namespace = 0;
      build_bases = COMPLETE_TYPE_P (val->type);
      insert_to_odr_array = true;
      *slot = val;
    }

  if (build_bases && TREE_CODE (type) == RECORD_TYPE && TYPE_BINFO (type)
      && type_with_linkage_p (type)
      && type == TYPE_MAIN_VARIANT (type))
    {
      tree binfo = TYPE_BINFO (type);
      unsigned int i;

      gcc_assert (BINFO_TYPE (TYPE_BINFO (val->type)) == type);
  
      val->all_derivations_known = type_all_derivations_known_p (type);
      for (i = 0; i < BINFO_N_BASE_BINFOS (binfo); i++)
	/* For now record only polymorphic types. other are
	   pointless for devirtualization and we cannot precisely
	   determine ODR equivalency of these during LTO.  */
	if (polymorphic_type_binfo_p (BINFO_BASE_BINFO (binfo, i)))
	  {
	    tree base_type= BINFO_TYPE (BINFO_BASE_BINFO (binfo, i));
	    odr_type base = get_odr_type (base_type, true);
	    gcc_assert (TYPE_MAIN_VARIANT (base_type) == base_type);
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
      val->id = odr_types.length ();
      vec_safe_push (odr_types_ptr, val);
    }
  return val;
}

/* Return type that in ODR type hash prevailed TYPE.  Be careful and punt
   on ODR violations.  */

tree
prevailing_odr_type (tree type)
{
  odr_type t = get_odr_type (type, false);
  if (!t || t->odr_violated)
    return type;
  return t->type;
}

/* Set tbaa_enabled flag for TYPE.  */

void
enable_odr_based_tbaa (tree type)
{
  odr_type t = get_odr_type (type, true);
  t->tbaa_enabled = true;
}

/* True if canonical type of TYPE is determined using ODR name.  */

bool
odr_based_tbaa_p (const_tree type)
{
  if (!RECORD_OR_UNION_TYPE_P (type))
    return false;
  odr_type t = get_odr_type (const_cast <tree> (type), false);
  if (!t || !t->tbaa_enabled)
    return false;
  return true;
}

/* Set TYPE_CANONICAL of type and all its variants and duplicates
   to CANONICAL.  */

void
set_type_canonical_for_odr_type (tree type, tree canonical)
{
  odr_type t = get_odr_type (type, false);
  unsigned int i;
  tree tt;

  for (tree t2 = t->type; t2; t2 = TYPE_NEXT_VARIANT (t2))
    TYPE_CANONICAL (t2) = canonical;
  if (t->types)
    FOR_EACH_VEC_ELT (*t->types, i, tt)
      for (tree t2 = tt; t2; t2 = TYPE_NEXT_VARIANT (t2))
        TYPE_CANONICAL (t2) = canonical;
}

/* Return true if we reported some ODR violation on TYPE.  */

bool
odr_type_violation_reported_p (tree type)
{
  return get_odr_type (type, false)->odr_violated;
}

/* Add TYPE of ODR type hash.  */

void
register_odr_type (tree type)
{
  if (!odr_hash)
    odr_hash = new odr_hash_type (23);
  if (type == TYPE_MAIN_VARIANT (type))
    {
      /* To get ODR warnings right, first register all sub-types.  */
      if (RECORD_OR_UNION_TYPE_P (type)
	  && COMPLETE_TYPE_P (type))
	{
	  /* Limit recursion on types which are already registered.  */
	  odr_type ot = get_odr_type (type, false);
	  if (ot
	      && (ot->type == type
		  || (ot->types_set
		      && ot->types_set->contains (type))))
	    return;
	  for (tree f = TYPE_FIELDS (type); f; f = TREE_CHAIN (f))
	    if (TREE_CODE (f) == FIELD_DECL)
	      {
		tree subtype = TREE_TYPE (f);

		while (TREE_CODE (subtype) == ARRAY_TYPE)
		  subtype = TREE_TYPE (subtype);
		if (type_with_linkage_p (TYPE_MAIN_VARIANT (subtype)))
		  register_odr_type (TYPE_MAIN_VARIANT (subtype));
	      }
	   if (TYPE_BINFO (type))
	     for (unsigned int i = 0;
	          i < BINFO_N_BASE_BINFOS (TYPE_BINFO (type)); i++)
	       register_odr_type (BINFO_TYPE (BINFO_BASE_BINFO
						 (TYPE_BINFO (type), i)));
	}
      get_odr_type (type, true);
    }
}

/* Return true if type is known to have no derivations.  */

bool
type_known_to_have_no_derivations_p (tree t)
{
  return (type_all_derivations_known_p (t)
	  && (TYPE_FINAL_P (t)
	      || (odr_hash
		  && !get_odr_type (t, true)->derived_types.length())));
}

/* Dump ODR type T and all its derived types.  INDENT specifies indentation for
   recursive printing.  */

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
      if (DECL_ASSEMBLER_NAME_SET_P (TYPE_NAME (t->type)))
        fprintf (f, "%*s mangled name: %s\n", indent * 2, "",
		 IDENTIFIER_POINTER
		   (DECL_ASSEMBLER_NAME (TYPE_NAME (t->type))));
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
  unsigned int num_all_types = 0, num_types = 0, num_duplicates = 0;
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
      if (!odr_types[i])
	continue;

      num_all_types++;
      if (!odr_types[i]->types || !odr_types[i]->types->length ())
	continue;

      /* To aid ODR warnings we also mangle integer constants but do
	 not consider duplicates there.  */
      if (TREE_CODE (odr_types[i]->type) == INTEGER_TYPE)
	continue;

      /* It is normal to have one duplicate and one normal variant.  */
      if (odr_types[i]->types->length () == 1
	  && COMPLETE_TYPE_P (odr_types[i]->type)
	  && !COMPLETE_TYPE_P ((*odr_types[i]->types)[0]))
	continue;

      num_types ++;

      unsigned int j;
      fprintf (f, "Duplicate tree types for odr type %i\n", i);
      print_node (f, "", odr_types[i]->type, 0);
      print_node (f, "", TYPE_NAME (odr_types[i]->type), 0);
      putc ('\n',f);
      for (j = 0; j < odr_types[i]->types->length (); j++)
	{
	  tree t;
	  num_duplicates ++;
	  fprintf (f, "duplicate #%i\n", j);
	  print_node (f, "", (*odr_types[i]->types)[j], 0);
	  t = (*odr_types[i]->types)[j];
	  while (TYPE_P (t) && TYPE_CONTEXT (t))
	    {
	      t = TYPE_CONTEXT (t);
	      print_node (f, "", t, 0);
	    }
	  print_node (f, "", TYPE_NAME ((*odr_types[i]->types)[j]), 0);
	  putc ('\n',f);
	}
    }
  fprintf (f, "Out of %i types there are %i types with duplicates; "
	   "%i duplicates overall\n", num_all_types, num_types, num_duplicates);
}

/* Save some WPA->ltrans streaming by freeing stuff needed only for good
   ODR warnings.
   We free TYPE_VALUES of enums and also make TYPE_DECLs to not point back
   to the type (which is needed to keep them in the same SCC and preserve
   location information to output warnings) and subsequently we make all
   TYPE_DECLS of same assembler name equivalent.  */

static void
free_odr_warning_data ()
{
  static bool odr_data_freed = false;

  if (odr_data_freed || !flag_wpa || !odr_types_ptr)
    return;

  odr_data_freed = true;

  for (unsigned int i = 0; i < odr_types.length (); i++)
    if (odr_types[i])
      {
	tree t = odr_types[i]->type;

	if (TREE_CODE (t) == ENUMERAL_TYPE)
	  TYPE_VALUES (t) = NULL;
	TREE_TYPE (TYPE_NAME (t)) = void_type_node;

	if (odr_types[i]->types)
          for (unsigned int j = 0; j < odr_types[i]->types->length (); j++)
	    {
	      tree td = (*odr_types[i]->types)[j];

	      if (TREE_CODE (td) == ENUMERAL_TYPE)
	        TYPE_VALUES (td) = NULL;
	      TYPE_NAME (td) = TYPE_NAME (t);
	    }
      }
  odr_data_freed = true;
}

/* Initialize IPA devirt and build inheritance tree graph.  */

void
build_type_inheritance_graph (void)
{
  struct symtab_node *n;
  FILE *inheritance_dump_file;
  dump_flags_t flags;

  if (odr_hash)
    {
      free_odr_warning_data ();
      return;
    }
  timevar_push (TV_IPA_INHERITANCE);
  inheritance_dump_file = dump_begin (TDI_inheritance, &flags);
  odr_hash = new odr_hash_type (23);

  /* We reconstruct the graph starting of types of all methods seen in the
     unit.  */
  FOR_EACH_SYMBOL (n)
    if (is_a <cgraph_node *> (n)
	&& DECL_VIRTUAL_P (n->decl)
	&& n->real_symbol_p ())
      get_odr_type (TYPE_METHOD_BASETYPE (TREE_TYPE (n->decl)), true);

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
  free_odr_warning_data ();
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
	    && VAR_P (ref->referring->decl)
	    && DECL_VIRTUAL_P (ref->referring->decl)))
      {
	found = true;
	break;
      }
  return found;
}

/* Return if TARGET is cxa_pure_virtual.  */

static bool
is_cxa_pure_virtual_p (tree target)
{
  return target && TREE_CODE (TREE_TYPE (target)) != METHOD_TYPE
	 && DECL_NAME (target)
	 && id_equal (DECL_NAME (target),
		     "__cxa_pure_virtual");
}

/* If TARGET has associated node, record it in the NODES array.
   CAN_REFER specify if program can refer to the target directly.
   if TARGET is unknown (NULL) or it cannot be inserted (for example because
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
  bool pure_virtual = is_cxa_pure_virtual_p (target);

  /* __builtin_unreachable do not need to be added into
     list of targets; the runtime effect of calling them is undefined.
     Only "real" virtual methods should be accounted.  */
  if (target && TREE_CODE (TREE_TYPE (target)) != METHOD_TYPE && !pure_virtual)
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

  /* Prefer alias target over aliases, so we do not get confused by
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
     of vtables referring to it are alive. 

     While this holds for non-anonymous functions, too, there are
     cases where we want to keep them in the list; for example
     inline functions with -fno-weak are static, but we still
     may devirtualize them when instance comes from other unit.
     The same holds for LTO.

     Currently we ignore these functions in speculative devirtualization.
     ??? Maybe it would make sense to be more aggressive for LTO even
     elsewhere.  */
  if (!flag_ltrans
      && !pure_virtual
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
      gcc_assert (!target_node->inlined_to);
      gcc_assert (target_node->real_symbol_p ());
      /* When sanitizing, do not assume that __cxa_pure_virtual is not called
	 by valid program.  */
      if (flag_sanitize & SANITIZE_UNREACHABLE)
	;
      /* Only add pure virtual if it is the only possible target.  This way
	 we will preserve the diagnostics about pure virtual called in many
	 cases without disabling optimization in other.  */
      else if (pure_virtual)
	{
	  if (nodes.length ())
	    return;
	}
      /* If we found a real target, take away cxa_pure_virtual.  */
      else if (!pure_virtual && nodes.length () == 1
	       && is_cxa_pure_virtual_p (nodes[0]->decl))
	nodes.pop ();
      if (pure_virtual && nodes.length ())
	return;
      if (!inserted->add (target))
	{
	  cached_polymorphic_call_targets->add (target_node);
	  nodes.safe_push (target_node);
	}
    }
  else if (!completep)
    ;
  /* We have definition of __cxa_pure_virtual that is not accessible (it is
     optimized out or partitioned to other unit) so we cannot add it.  When
     not sanitizing, there is nothing to do.
     Otherwise declare the list incomplete.  */
  else if (pure_virtual)
    {
      if (flag_sanitize & SANITIZE_UNREACHABLE)
	*completep = false;
    }
  else if (flag_ltrans
	   || !type_in_anonymous_namespace_p (DECL_CONTEXT (target)))
    *completep = false;
}

/* See if BINFO's type matches OUTER_TYPE.  If so, look up 
   BINFO of subtype of OTR_TYPE at OFFSET and in that BINFO find
   method in vtable and insert method to NODES array
   or BASES_TO_CONSIDER if this array is non-NULL.
   Otherwise recurse to base BINFOs.
   This matches what get_binfo_at_offset does, but with offset
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

      /* Look up BINFO with virtual table.  For normal types it is always last
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
    /* Walking bases that have no virtual method is pointless exercise.  */
    if (polymorphic_type_binfo_p (base_binfo))
      record_target_from_binfo (nodes, bases_to_consider, base_binfo, otr_type,
				type_binfos, 
				otr_token, outer_type, offset, inserted,
				matched_vtables, anonymous, completep);
  if (BINFO_VTABLE (binfo))
    type_binfos.pop ();
}
     
/* Look up virtual methods matching OTR_TYPE (with OFFSET and OTR_TOKEN)
   of TYPE, insert them to NODES, recurse into derived nodes. 
   INSERTED is used to avoid duplicate insertions of methods into NODES.
   MATCHED_VTABLES are used to avoid duplicate walking vtables.
   Clear COMPLETEP if unreferable target is found.
 
   If CONSIDER_CONSTRUCTION is true, record to BASES_TO_CONSIDER
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
  auto_vec <tree, 8> type_binfos;
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

class polymorphic_call_target_d
{
public:
  HOST_WIDE_INT otr_token;
  ipa_polymorphic_call_context context;
  odr_type type;
  vec <cgraph_node *> targets;
  tree decl_warning;
  int type_warning;
  unsigned int n_odr_types;
  bool complete;
  bool speculative;
};

/* Polymorphic call target cache helpers.  */

struct polymorphic_call_target_hasher
  : pointer_hash <polymorphic_call_target_d>
{
  static inline hashval_t hash (const polymorphic_call_target_d *);
  static inline bool equal (const polymorphic_call_target_d *,
			    const polymorphic_call_target_d *);
  static inline void remove (polymorphic_call_target_d *);
};

/* Return the computed hashcode for ODR_QUERY.  */

inline hashval_t
polymorphic_call_target_hasher::hash (const polymorphic_call_target_d *odr_query)
{
  inchash::hash hstate (odr_query->otr_token);

  hstate.add_hwi (odr_query->type->id);
  hstate.merge_hash (TYPE_UID (odr_query->context.outer_type));
  hstate.add_hwi (odr_query->context.offset);
  hstate.add_hwi (odr_query->n_odr_types);

  if (odr_query->context.speculative_outer_type)
    {
      hstate.merge_hash (TYPE_UID (odr_query->context.speculative_outer_type));
      hstate.add_hwi (odr_query->context.speculative_offset);
    }
  hstate.add_flag (odr_query->speculative);
  hstate.add_flag (odr_query->context.maybe_in_construction);
  hstate.add_flag (odr_query->context.maybe_derived_type);
  hstate.add_flag (odr_query->context.speculative_maybe_derived_type);
  hstate.commit_flag ();
  return hstate.end ();
}

/* Compare cache entries T1 and T2.  */

inline bool
polymorphic_call_target_hasher::equal (const polymorphic_call_target_d *t1,
				       const polymorphic_call_target_d *t2)
{
  return (t1->type == t2->type && t1->otr_token == t2->otr_token
	  && t1->speculative == t2->speculative
	  && t1->context.offset == t2->context.offset
	  && t1->context.speculative_offset == t2->context.speculative_offset
	  && t1->context.outer_type == t2->context.outer_type
	  && t1->context.speculative_outer_type == t2->context.speculative_outer_type
	  && t1->context.maybe_in_construction
	      == t2->context.maybe_in_construction
	  && t1->context.maybe_derived_type == t2->context.maybe_derived_type
	  && (t1->context.speculative_maybe_derived_type
	      == t2->context.speculative_maybe_derived_type)
	  /* Adding new type may affect outcome of target search.  */
	  && t1->n_odr_types == t2->n_odr_types);
}

/* Remove entry in polymorphic call target cache hash.  */

inline void
polymorphic_call_target_hasher::remove (polymorphic_call_target_d *v)
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

/* Force rebuilding type inheritance graph from scratch.
   This is use to make sure that we do not keep references to types
   which was not visible to free_lang_data.  */

void
rebuild_type_inheritance_graph ()
{
  if (!odr_hash)
    return;
  delete odr_hash;
  odr_hash = NULL;
  odr_types_ptr = NULL;
  free_polymorphic_call_targets_hash ();
}

/* When virtual function is removed, we may need to flush the cache.  */

static void
devirt_node_removal_hook (struct cgraph_node *n, void *d ATTRIBUTE_UNUSED)
{
  if (cached_polymorphic_call_targets
      && !thunk_expansion
      && cached_polymorphic_call_targets->contains (n))
    free_polymorphic_call_targets_hash ();
}

/* Look up base of BINFO that has virtual table VTABLE with OFFSET.  */

tree
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
     POINTER_PLUS_EXPR.  Verify that this pointer matches what
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
     that standard folding will be able to cope with it.  */
  return subbinfo_with_vtable_at_offset (TYPE_BINFO (DECL_CONTEXT (vtable)),
					 offset, vtable);
}

/* Walk bases of OUTER_TYPE that contain OTR_TYPE at OFFSET.
   Look up their respective virtual methods for OTR_TOKEN and OTR_TYPE
   and insert them in NODES.

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
      /* Within a class type we should always find corresponding fields.  */
      gcc_assert (fld && TREE_CODE (TREE_TYPE (fld)) == RECORD_TYPE);

      /* Nonbase types should have been stripped by outer_class_type.  */
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
  profile_count dyn_count;
};

/* Record about how many calls would benefit from given method to be final.  */

struct decl_warn_count
{
  tree decl;
  int count;
  profile_count dyn_count;
};

/* Information about type and decl warnings.  */

class final_warning_record
{
public:
  /* If needed grow type_warnings vector and initialize new decl_warn_count
     to have dyn_count set to profile_count::zero ().  */
  void grow_type_warnings (unsigned newlen);

  profile_count dyn_count;
  auto_vec<odr_type_warn_count> type_warnings;
  hash_map<tree, decl_warn_count> decl_warnings;
};

void
final_warning_record::grow_type_warnings (unsigned newlen)
{
  unsigned len = type_warnings.length ();
  if (newlen > len)
    {
      type_warnings.safe_grow_cleared (newlen);
      for (unsigned i = len; i < newlen; i++)
	type_warnings[i].dyn_count = profile_count::zero ();
    }
}

class final_warning_record *final_warning_records;

/* Return vector containing possible targets of polymorphic call of type
   OTR_TYPE calling method OTR_TOKEN within type of OTR_OUTER_TYPE and OFFSET.
   If INCLUDE_BASES is true, walk also base types of OUTER_TYPES containing
   OTR_TYPE and include their virtual method.  This is useful for types
   possibly in construction or destruction where the virtual table may
   temporarily change to one of base types.  INCLUDE_DERIVED_TYPES make
   us to walk the inheritance graph for all derivations.

   If COMPLETEP is non-NULL, store true if the list is complete. 
   CACHE_TOKEN (if non-NULL) will get stored to an unique ID of entry
   in the target cache.  If user needs to visit every target list
   just once, it can memoize them.

   If SPECULATIVE is set, the list will not contain targets that
   are not speculatively taken.

   Returned vector is placed into cache.  It is NOT caller's responsibility
   to free it.  The vector can be freed on cgraph_remove_node call if
   the particular node is a virtual function present in the cache.  */

vec <cgraph_node *>
possible_polymorphic_call_targets (tree otr_type,
			           HOST_WIDE_INT otr_token,
				   ipa_polymorphic_call_context context,
			           bool *completep,
			           void **cache_token,
				   bool speculative)
{
  static struct cgraph_node_hook_list *node_removal_hook_holder;
  vec <cgraph_node *> nodes = vNULL;
  auto_vec <tree, 8> bases_to_consider;
  odr_type type, outer_type;
  polymorphic_call_target_d key;
  polymorphic_call_target_d **slot;
  unsigned int i;
  tree binfo, target;
  bool complete;
  bool can_refer = false;
  bool skipped = false;

  otr_type = TYPE_MAIN_VARIANT (otr_type);

  /* If ODR is not initialized or the context is invalid, return empty
     incomplete list.  */
  if (!odr_hash || context.invalid || !TYPE_BINFO (otr_type))
    {
      if (completep)
	*completep = context.invalid;
      if (cache_token)
	*cache_token = NULL;
      return nodes;
    }

  /* Do not bother to compute speculative info when user do not asks for it.  */
  if (!speculative || !context.speculative_outer_type)
    context.clear_speculation ();

  type = get_odr_type (otr_type, true);

  /* Recording type variants would waste results cache.  */
  gcc_assert (!context.outer_type
	      || TYPE_MAIN_VARIANT (context.outer_type) == context.outer_type);

  /* Look up the outer class type we want to walk.
     If we fail to do so, the context is invalid.  */
  if ((context.outer_type || context.speculative_outer_type)
      && !context.restrict_to_inner_class (otr_type))
    {
      if (completep)
	*completep = true;
      if (cache_token)
	*cache_token = NULL;
      return nodes;
    }
  gcc_assert (!context.invalid);

  /* Check that restrict_to_inner_class kept the main variant.  */
  gcc_assert (!context.outer_type
	      || TYPE_MAIN_VARIANT (context.outer_type) == context.outer_type);

  /* We canonicalize our query, so we do not need extra hashtable entries.  */

  /* Without outer type, we have no use for offset.  Just do the
     basic search from inner type.  */
  if (!context.outer_type)
    context.clear_outer_type (otr_type);
  /* We need to update our hierarchy if the type does not exist.  */
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

  if (in_lto_p)
    {
      if (context.outer_type != otr_type)
        context.outer_type
	  = get_odr_type (context.outer_type, true)->type;
      if (context.speculative_outer_type)
        context.speculative_outer_type
	  = get_odr_type (context.speculative_outer_type, true)->type;
    }

  /* Look up cached answer.  */
  key.type = type;
  key.otr_token = otr_token;
  key.speculative = speculative;
  key.context = context;
  key.n_odr_types = odr_types.length ();
  slot = polymorphic_call_target_hash->find_slot (&key, INSERT);
  if (cache_token)
   *cache_token = (void *)*slot;
  if (*slot)
    {
      if (completep)
	*completep = (*slot)->complete;
      if ((*slot)->type_warning && final_warning_records)
	{
	  final_warning_records->type_warnings[(*slot)->type_warning - 1].count++;
	  if (!final_warning_records->type_warnings
		[(*slot)->type_warning - 1].dyn_count.initialized_p ())
	    final_warning_records->type_warnings
		[(*slot)->type_warning - 1].dyn_count = profile_count::zero ();
	  if (final_warning_records->dyn_count > 0)
	    final_warning_records->type_warnings[(*slot)->type_warning - 1].dyn_count
	      = final_warning_records->type_warnings[(*slot)->type_warning - 1].dyn_count
	        + final_warning_records->dyn_count;
	}
      if (!speculative && (*slot)->decl_warning && final_warning_records)
	{
	  struct decl_warn_count *c =
	     final_warning_records->decl_warnings.get ((*slot)->decl_warning);
	  c->count++;
	  if (final_warning_records->dyn_count > 0)
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
  (*slot)->speculative = speculative;

  hash_set<tree> inserted;
  hash_set<tree> matched_vtables;

  /* First insert targets we speculatively identified as likely.  */
  if (context.speculative_outer_type)
    {
      odr_type speculative_outer_type;
      bool speculation_complete = true;

      /* First insert target from type itself and check if it may have
	 derived types.  */
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
    }

  if (!speculative || !nodes.length ())
    {
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
	      if (!speculative && final_warning_records
		  && nodes.length () == 1
		  && TREE_CODE (TREE_TYPE (nodes[0]->decl)) == METHOD_TYPE)
		{
		  if (complete
		      && warn_suggest_final_types
		      && !outer_type->derived_types.length ())
		    {
		      final_warning_records->grow_type_warnings
			(outer_type->id);
		      final_warning_records->type_warnings[outer_type->id].count++;
		      if (!final_warning_records->type_warnings
				[outer_type->id].dyn_count.initialized_p ())
			final_warning_records->type_warnings
			   [outer_type->id].dyn_count = profile_count::zero ();
		      final_warning_records->type_warnings[outer_type->id].dyn_count
			+= final_warning_records->dyn_count;
		      final_warning_records->type_warnings[outer_type->id].type
			= outer_type->type;
		      (*slot)->type_warning = outer_type->id + 1;
		    }
		  if (complete
		      && warn_suggest_final_methods
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

      if (!speculative)
	{
	  /* Destructors are never called through construction virtual tables,
	     because the type is always known.  One of entries may be
	     cxa_pure_virtual so look to at least two of them.  */
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
	}
    }

  (*slot)->targets = nodes;
  (*slot)->complete = complete;
  (*slot)->n_odr_types = odr_types.length ();
  if (completep)
    *completep = complete;

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

/* Dump target list TARGETS into FILE.  */

static void
dump_targets (FILE *f, vec <cgraph_node *> targets, bool verbose)
{
  unsigned int i;

  for (i = 0; i < targets.length (); i++)
    {
      char *name = NULL;
      if (in_lto_p)
	name = cplus_demangle_v3 (targets[i]->asm_name (), 0);
      fprintf (f, " %s", name ? name : targets[i]->dump_name ());
      if (in_lto_p)
	free (name);
      if (!targets[i]->definition)
	fprintf (f, " (no definition%s)",
		 DECL_DECLARED_INLINE_P (targets[i]->decl)
		 ? " inline" : "");
      /* With many targets for every call polymorphic dumps are going to
	 be quadratic in size.  */
      if (i > 10 && !verbose)
	{
	  fprintf (f, " ... and %i more targets\n", targets.length () - i);
	  return;
	}
    }
  fprintf (f, "\n");
}

/* Dump all possible targets of a polymorphic call.  */

void
dump_possible_polymorphic_call_targets (FILE *f,
					tree otr_type,
					HOST_WIDE_INT otr_token,
					const ipa_polymorphic_call_context &ctx,
					bool verbose)
{
  vec <cgraph_node *> targets;
  bool final;
  odr_type type = get_odr_type (TYPE_MAIN_VARIANT (otr_type), false);
  unsigned int len;

  if (!type)
    return;
  targets = possible_polymorphic_call_targets (otr_type, otr_token,
					       ctx,
					       &final, NULL, false);
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
  len = targets.length ();
  dump_targets (f, targets, verbose);

  targets = possible_polymorphic_call_targets (otr_type, otr_token,
					       ctx,
					       &final, NULL, true);
  if (targets.length () != len)
    {
      fprintf (f, "  Speculative targets:");
      dump_targets (f, targets, verbose);
    }
  /* Ugly: during callgraph construction the target cache may get populated
     before all targets are found.  While this is harmless (because all local
     types are discovered and only in those case we devirtualize fully and we
     don't do speculative devirtualization before IPA stage) it triggers
     assert here when dumping at that stage also populates the case with
     speculative targets.  Quietly ignore this.  */
  gcc_assert (symtab->state < IPA_SSA || targets.length () <= len);
  fprintf (f, "\n");
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
  bool final;

  if (fndecl_built_in_p (n->decl, BUILT_IN_UNREACHABLE)
      || fndecl_built_in_p (n->decl, BUILT_IN_TRAP))
    return true;

  if (is_cxa_pure_virtual_p (n->decl))
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
				    gimple *stmt,
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
     unit.  */
  FOR_EACH_FUNCTION (n)
    if (DECL_VIRTUAL_P (n->decl)
	&& !n->definition
	&& n->real_symbol_p ())
      get_odr_type (TYPE_METHOD_BASETYPE (TREE_TYPE (n->decl)), true);
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
  /* If there are no live virtual tables referring the target,
     the only way the target can be called is an instance coming from other
     compilation unit; speculative devirtualization is built around an
     assumption that won't happen.  */
  if (!referenced_from_vtable_p (n))
    return false;
  return true;
}

/* Compare type warning records P1 and P2 and choose one with larger count;
   helper for qsort.  */

static int
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

/* Compare decl warning records P1 and P2 and choose one with larger count;
   helper for qsort.  */

static int
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


/* Try to speculatively devirtualize call to OTR_TYPE with OTR_TOKEN with
   context CTX.  */

struct cgraph_node *
try_speculative_devirtualization (tree otr_type, HOST_WIDE_INT otr_token,
				  ipa_polymorphic_call_context ctx)
{
  vec <cgraph_node *>targets
     = possible_polymorphic_call_targets
	  (otr_type, otr_token, ctx, NULL, NULL, true);
  unsigned int i;
  struct cgraph_node *likely_target = NULL;

  for (i = 0; i < targets.length (); i++)
    if (likely_target_p (targets[i]))
      {
	if (likely_target)
	  return NULL;
	likely_target = targets[i];
      }
  if (!likely_target
      ||!likely_target->definition
      || DECL_EXTERNAL (likely_target->decl))
    return NULL;

  /* Don't use an implicitly-declared destructor (c++/58678).  */
  struct cgraph_node *non_thunk_target
    = likely_target->function_symbol ();
  if (DECL_ARTIFICIAL (non_thunk_target->decl))
    return NULL;
  if (likely_target->get_availability () <= AVAIL_INTERPOSABLE
      && likely_target->can_be_discarded_p ())
    return NULL;
  return likely_target;
}

/* The ipa-devirt pass.
   When polymorphic call has only one likely target in the unit,
   turn it into a speculative call.  */

static unsigned int
ipa_devirt (void)
{
  struct cgraph_node *n;
  hash_set<void *> bad_call_targets;
  struct cgraph_edge *e;

  int npolymorphic = 0, nspeculated = 0, nconverted = 0, ncold = 0;
  int nmultiple = 0, noverwritable = 0, ndevirtualized = 0, nnotdefined = 0;
  int nwrong = 0, nok = 0, nexternal = 0, nartificial = 0;
  int ndropped = 0;

  if (!odr_types_ptr)
    return 0;

  if (dump_file)
    dump_type_inheritance_graph (dump_file);

  /* We can output -Wsuggest-final-methods and -Wsuggest-final-types warnings.
     This is implemented by setting up final_warning_records that are updated
     by get_polymorphic_call_targets.
     We need to clear cache in this case to trigger recomputation of all
     entries.  */
  if (warn_suggest_final_methods || warn_suggest_final_types)
    {
      final_warning_records = new (final_warning_record);
      final_warning_records->dyn_count = profile_count::zero ();
      final_warning_records->grow_type_warnings (odr_types.length ());
      free_polymorphic_call_targets_hash ();
    }

  FOR_EACH_DEFINED_FUNCTION (n)
    {	
      bool update = false;
      if (!opt_for_fn (n->decl, flag_devirtualize))
	continue;
      if (dump_file && n->indirect_calls)
	fprintf (dump_file, "\n\nProcesing function %s\n",
		 n->dump_name ());
      for (e = n->indirect_calls; e; e = e->next_callee)
	if (e->indirect_info->polymorphic)
	  {
	    struct cgraph_node *likely_target = NULL;
	    void *cache_token;
	    bool final;

	    if (final_warning_records)
	      final_warning_records->dyn_count = e->count.ipa ();

	    vec <cgraph_node *>targets
	       = possible_polymorphic_call_targets
		    (e, &final, &cache_token, true);
	    unsigned int i;

	    /* Trigger warnings by calculating non-speculative targets.  */
	    if (warn_suggest_final_methods || warn_suggest_final_types)
	      possible_polymorphic_call_targets (e);

	    if (dump_file)
	      dump_possible_polymorphic_call_targets 
		(dump_file, e, (dump_flags & TDF_DETAILS));

	    npolymorphic++;

	    /* See if the call can be devirtualized by means of ipa-prop's
	       polymorphic call context propagation.  If not, we can just
	       forget about this call being polymorphic and avoid some heavy
	       lifting in remove_unreachable_nodes that will otherwise try to
	       keep all possible targets alive until inlining and in the inliner
	       itself.

	       This may need to be revisited once we add further ways to use
	       the may edges, but it is a reasonable thing to do right now.  */

	    if ((e->indirect_info->param_index == -1
		|| (!opt_for_fn (n->decl, flag_devirtualize_speculatively)
		    && e->indirect_info->vptr_changed))
		&& !flag_ltrans_devirtualize)
	      {
		e->indirect_info->polymorphic = false;
		ndropped++;
	        if (dump_file)
		  fprintf (dump_file, "Dropping polymorphic call info;"
			   " it cannot be used by ipa-prop\n");
	      }

	    if (!opt_for_fn (n->decl, flag_devirtualize_speculatively))
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
		  fprintf (dump_file, "Call is already speculated\n\n");
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
		      likely_target = NULL;
		      if (dump_file)
			fprintf (dump_file, "More than one likely target\n\n");
		      nmultiple++;
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
		bool found = e->speculative_call_for_target (likely_target);
		if (found)
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
		  fprintf (dump_file, "Target is not a definition\n\n");
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
                    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, e->call_stmt,
				     "speculatively devirtualizing call "
				     "in %s to %s\n",
				     n->dump_name (),
				     likely_target->dump_name ());
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
		  (likely_target, e->count.apply_scale (8, 10));
	      }
	  }
      if (update)
	ipa_update_overall_fn_summary (n);
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
	        profile_count dyn_count
		  = final_warning_records->type_warnings[i].dyn_count;

		if (!(dyn_count > 0))
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
			     (long long) dyn_count.to_gcov_type ());
	      }
	}

      if (warn_suggest_final_methods)
	{
	  auto_vec<const decl_warn_count*> decl_warnings_vec;

	  final_warning_records->decl_warnings.traverse
	    <vec<const decl_warn_count *> *, add_decl_warning> (&decl_warnings_vec);
	  decl_warnings_vec.qsort (decl_warning_cmp);
	  for (unsigned int i = 0; i < decl_warnings_vec.length (); i++)
	    {
	      tree decl = decl_warnings_vec[i]->decl;
	      int count = decl_warnings_vec[i]->count;
	      profile_count dyn_count
		  = decl_warnings_vec[i]->dyn_count;

	      if (!(dyn_count > 0))
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
			      DECL_CONTEXT (decl), count,
			      (long long)dyn_count.to_gcov_type ());
		else
		  warning_n (DECL_SOURCE_LOCATION (decl),
			      OPT_Wsuggest_final_methods, count,
			      "Declaring method %qD final "
			      "would enable devirtualization of %i call "
			      "executed %lli times",
			      "Declaring method %qD final "
			      "would enable devirtualization of %i calls "
			      "executed %lli times",
			      decl, count,
			      (long long)dyn_count.to_gcov_type ());
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
	     " %i external, %i not defined, %i artificial, %i infos dropped\n",
	     npolymorphic, ndevirtualized, nconverted, ncold,
	     nmultiple, noverwritable, nspeculated, nok, nwrong,
	     nexternal, nnotdefined, nartificial, ndropped);
  return ndevirtualized || ndropped ? TODO_remove_functions : 0;
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
      /* In LTO, always run the IPA passes and decide on function basis if the
	 pass is enabled.  */
      if (in_lto_p)
	return true;
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

/* Print ODR name of a TYPE if available.
   Use demangler when option DEMANGLE is used.  */

DEBUG_FUNCTION void
debug_tree_odr_name (tree type, bool demangle)
{
  const char *odr = get_odr_name_for_type (type);
  if (demangle)
    {
      const int opts = DMGL_PARAMS | DMGL_ANSI | DMGL_TYPES;
      odr = cplus_demangle (odr, opts);
    }

  fprintf (stderr, "%s\n", odr);
}

#include "gt-ipa-devirt.h"
