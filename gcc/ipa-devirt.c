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
#include "pointer-set.h"
#include "target.h"
#include "hash-table.h"
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

static bool odr_violation_reported = false;

/* Dummy polymorphic call context.  */

const ipa_polymorphic_call_context ipa_dummy_polymorphic_call_context
   = {0, NULL, false, true};

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

  /* All equivalent types, if more than one.  */
  vec<tree, va_gc> *types;
  /* Set of all equivalent types, if NON-NULL.  */
  pointer_set_t * GTY((skip)) types_set;

  /* Unique ID indexing the type in odr_types array.  */
  int id;
  /* Is it in anonymous namespace? */
  bool anonymous_namespace;
  /* Do we know about all derivations of given type?  */
  bool all_derivations_known;
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
	 && cgraph_state >= CGRAPH_STATE_CONSTRUCTION
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
  vnode = varpool_get_node (vtable);
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

  /* For polymorphic types, we can simply hash the virtual table.  */
  if (TYPE_BINFO (t) && BINFO_VTABLE (TYPE_BINFO (t)))
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
  if (v->types_set)
    pointer_set_destroy (v->types_set);
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

/* TYPE is equivalent to VAL by ODR, but its tree representation differs
   from VAL->type.  This may happen in LTO where tree merging did not merge
   all variants of the same type.  It may or may not mean the ODR violation.
   Add it to the list of duplicates and warn on some violations.  */

static void
add_type_duplicate (odr_type val, tree type)
{
  if (!val->types_set)
    val->types_set = pointer_set_create ();

  /* See if this duplicate is new.  */
  if (!pointer_set_insert (val->types_set, type))
    {
      bool merge = true;
      bool base_mismatch = false;
      gcc_assert (in_lto_p);
      vec_safe_push (val->types, type);
      unsigned int i,j;

      /* First we compare memory layout.  */
      if (!types_compatible_p (val->type, type))
	{
	  merge = false;
	  odr_violation_reported = true;
	  if (BINFO_VTABLE (TYPE_BINFO (val->type))
	      && warning_at (DECL_SOURCE_LOCATION (TYPE_NAME (type)), 0,
			     "type %qD violates one definition rule  ",
			     type))
	    inform (DECL_SOURCE_LOCATION (TYPE_NAME (val->type)),
		    "a type with the same name but different layout is "
		    "defined in another translation unit");
	  if (cgraph_dump_file)
	    {
	      fprintf (cgraph_dump_file, "ODR violation or merging or ODR type bug?\n");
	    
	      print_node (cgraph_dump_file, "", val->type, 0);
	      putc ('\n',cgraph_dump_file);
	      print_node (cgraph_dump_file, "", type, 0);
	      putc ('\n',cgraph_dump_file);
	    }
	}

      /* Next sanity check that bases are the same.  If not, we will end
	 up producing wrong answers.  */
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

	  if (warning_at (DECL_SOURCE_LOCATION (TYPE_NAME (type)), 0,
			  "type %qD violates one definition rule  ",
			  type))
	    inform (DECL_SOURCE_LOCATION (TYPE_NAME (val->type)),
		    "a type with the same name but different bases is "
		    "defined in another translation unit");
	  if (cgraph_dump_file)
	    {
	      fprintf (cgraph_dump_file, "ODR bse violation or merging bug?\n");
	    
	      print_node (cgraph_dump_file, "", val->type, 0);
	      putc ('\n',cgraph_dump_file);
	      print_node (cgraph_dump_file, "", type, 0);
	      putc ('\n',cgraph_dump_file);
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
         to incomplete answers from possible_polymorphic_call_targets.  */
      if (!flag_ltrans && merge)
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

	      TYPE_BINFO (val->type) = TYPE_BINFO (type);
	      for (i = 0; i < val->types->length (); i++)
		{
		  if (TYPE_BINFO ((*val->types)[i])
		      == master_binfo)
		    TYPE_BINFO ((*val->types)[i]) = TYPE_BINFO (type);
		}
	    }
	  else
	    TYPE_BINFO (type) = master_binfo;
	}
    }
}

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

      /* With LTO we need to support multiple tree representation of
	 the same ODR type.  */
      if (val->type != type)
        add_type_duplicate (val, type);
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
	    base->derived_types.safe_push (val);
	    val->bases.safe_push (base);
	  }
      /* First record bases, then add into array so ids are increasing.  */
      if (odr_types_ptr)
        val->id = odr_types.length ();
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
      if (odr_types[i]->bases.length () == 0)
	dump_odr_type (f, odr_types[i]);
    }
  for (i = 0; i < odr_types.length (); i++)
    {
      if (odr_types[i]->types && odr_types[i]->types->length ())
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
method_class_type (tree t)
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

  if (odr_hash.is_created ())
    return;
  timevar_push (TV_IPA_INHERITANCE);
  inheritance_dump_file = dump_begin (TDI_inheritance, &flags);
  odr_hash.create (23);

  /* We reconstruct the graph starting of types of all methods seen in the
     the unit.  */
  FOR_EACH_SYMBOL (n)
    if (is_a <cgraph_node *> (n)
	&& DECL_VIRTUAL_P (n->decl)
	&& symtab_real_symbol_p (n))
      get_odr_type (method_class_type (TREE_TYPE (n->decl)), true);

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
      get_odr_type (DECL_CONTEXT (n->decl), true);
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
      || node->used_from_other_partition)
    return true;

  /* Keep this test constant time.
     It is unlikely this can happen except for the case where speculative
     devirtualization introduced many speculative edges to this node. 
     In this case the target is very likely alive anyway.  */
  if (node->ref_list.referring.length () > 100)
    return true;

  /* We need references built.  */
  if (cgraph_state <= CGRAPH_STATE_CONSTRUCTION)
    return true;

  for (i = 0; ipa_ref_list_referring_iterate (&node->ref_list,
					      i, ref); i++)
	
    if ((ref->use == IPA_REF_ALIAS
	 && referenced_from_vtable_p (cgraph (ref->referring)))
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
		   tree target, pointer_set_t *inserted,
		   bool can_refer,
		   bool *completep)
{
  struct cgraph_node *target_node;

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

  target_node = cgraph_get_node (target);

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
	   && symtab_real_symbol_p (target_node))
    {
      gcc_assert (!target_node->global.inlined_to);
      gcc_assert (symtab_real_symbol_p (target_node));
      if (!pointer_set_insert (inserted, target))
	{
	  pointer_set_insert (cached_polymorphic_call_targets,
			      target_node);
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
			  pointer_set_t *inserted,
			  pointer_set_t *matched_vtables,
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
	  vnode = varpool_get_node (vtable);
	  if (!vnode || !vnode->definition)
	    return;
	}
      gcc_assert (inner_binfo);
      if (bases_to_consider
	  ? !pointer_set_contains (matched_vtables, BINFO_VTABLE (inner_binfo))
	  : !pointer_set_insert (matched_vtables, BINFO_VTABLE (inner_binfo)))
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
				     pointer_set_t *inserted,
				     pointer_set_t *matched_vtables,
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
  int nonconstruction_targets;
  bool complete;
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
  hashval_t hash;

  hash = iterative_hash_host_wide_int
	  (odr_query->otr_token,
	   odr_query->type->id);
  hash = iterative_hash_hashval_t (TYPE_UID (odr_query->context.outer_type),
				   hash);
  hash = iterative_hash_host_wide_int (odr_query->context.offset, hash);
  return iterative_hash_hashval_t
	    (((int)odr_query->context.maybe_in_construction << 1)
	     | (int)odr_query->context.maybe_derived_type, hash);
}

/* Compare cache entries T1 and T2.  */

inline bool
polymorphic_call_target_hasher::equal (const value_type *t1,
				       const compare_type *t2)
{
  return (t1->type == t2->type && t1->otr_token == t2->otr_token
	  && t1->context.offset == t2->context.offset
	  && t1->context.outer_type == t2->context.outer_type
	  && t1->context.maybe_in_construction
	      == t2->context.maybe_in_construction
	  && t1->context.maybe_derived_type == t2->context.maybe_derived_type);
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

/* CONTEXT->OUTER_TYPE is a type of memory object where object of EXPECTED_TYPE
   is contained at CONTEXT->OFFSET.  Walk the memory representation of
   CONTEXT->OUTER_TYPE and find the outermost class type that match
   EXPECTED_TYPE or contain EXPECTED_TYPE as a base.  Update CONTEXT
   to represent it.

   For example when CONTEXT represents type
   class A
     {
       int a;
       class B b;
     }
   and we look for type at offset sizeof(int), we end up with B and offset 0.
   If the same is produced by multiple inheritance, we end up with A and offset
   sizeof(int). 

   If we can not find corresponding class, give up by setting
   CONTEXT->OUTER_TYPE to EXPECTED_TYPE and CONTEXT->OFFSET to NULL. 
   Return true when lookup was sucesful.  */

static bool
get_class_context (ipa_polymorphic_call_context *context,
		   tree expected_type)
{
  tree type = context->outer_type;
  HOST_WIDE_INT offset = context->offset;

  /* Find the sub-object the constant actually refers to and mark whether it is
     an artificial one (as opposed to a user-defined one).  */
  while (true)
    {
      HOST_WIDE_INT pos, size;
      tree fld;

      /* On a match, just return what we found.  */
      if (TREE_CODE (type) == TREE_CODE (expected_type)
	  && types_same_for_odr (type, expected_type))
	{
	  /* Type can not contain itself on an non-zero offset.  In that case
	     just give up.  */
	  if (offset != 0)
	    goto give_up;
	  gcc_assert (offset == 0);
	  return true;
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
	      if (pos <= offset && (pos + size) > offset)
		break;
	    }

	  if (!fld)
	    goto give_up;

	  type = TREE_TYPE (fld);
	  offset -= pos;
	  /* DECL_ARTIFICIAL represents a basetype.  */
	  if (!DECL_ARTIFICIAL (fld))
	    {
	      context->outer_type = type;
	      context->offset = offset;
	      /* As soon as we se an field containing the type,
		 we know we are not looking for derivations.  */
	      context->maybe_derived_type = false;
	    }
	}
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree subtype = TREE_TYPE (type);

	  /* Give up if we don't know array size.  */
	  if (!tree_fits_shwi_p (TYPE_SIZE (subtype))
	      || !tree_to_shwi (TYPE_SIZE (subtype)) <= 0)
	    goto give_up;
	  offset = offset % tree_to_shwi (TYPE_SIZE (subtype));
	  type = subtype;
	  context->outer_type = type;
	  context->offset = offset;
	  context->maybe_derived_type = false;
	}
      /* Give up on anything else.  */
      else
	goto give_up;
    }

  /* If we failed to find subtype we look for, give up and fall back to the
     most generic query.  */
give_up:
  context->outer_type = expected_type;
  context->offset = 0;
  context->maybe_derived_type = true;
  return false;
}

/* Return true if OUTER_TYPE contains OTR_TYPE at OFFSET.  */

static bool
contains_type_p (tree outer_type, HOST_WIDE_INT offset,
		 tree otr_type)
{
  ipa_polymorphic_call_context context = {offset, outer_type,
					  false, true};
  return get_class_context (&context, otr_type);
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
vtable_pointer_value_to_vtable (tree t, tree *v, unsigned HOST_WIDE_INT *offset)
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
vtable_pointer_value_to_binfo (tree t)
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

/* Proudce polymorphic call context for call method of instance
   that is located within BASE (that is assumed to be a decl) at OFFSET. */

static void
get_polymorphic_call_info_for_decl (ipa_polymorphic_call_context *context,
				    tree base, HOST_WIDE_INT offset)
{
  gcc_assert (DECL_P (base));

  context->outer_type = TREE_TYPE (base);
  context->offset = offset;
  /* Make very conservative assumption that all objects
     may be in construction. 
     TODO: ipa-prop already contains code to tell better. 
     merge it later.  */
  context->maybe_in_construction = true;
  context->maybe_derived_type = false;
}

/* CST is an invariant (address of decl), try to get meaningful
   polymorphic call context for polymorphic call of method 
   if instance of OTR_TYPE that is located at OFFSET of this invariant.
   Return FALSE if nothing meaningful can be found.  */

bool
get_polymorphic_call_info_from_invariant (ipa_polymorphic_call_context *context,
				          tree cst,
				          tree otr_type,
				          HOST_WIDE_INT offset)
{
  HOST_WIDE_INT offset2, size, max_size;
  tree base;

  if (TREE_CODE (cst) != ADDR_EXPR)
    return false;

  cst = TREE_OPERAND (cst, 0);
  base = get_ref_base_and_extent (cst, &offset2, &size, &max_size);
  if (!DECL_P (base) || max_size == -1 || max_size != size)
    return false;

  /* Only type inconsistent programs can have otr_type that is
     not part of outer type.  */
  if (!contains_type_p (TREE_TYPE (base), offset, otr_type))
    return false;

  get_polymorphic_call_info_for_decl (context, base, offset);
  return true;
}

/* Given REF call in FNDECL, determine class of the polymorphic
   call (OTR_TYPE), its token (OTR_TOKEN) and CONTEXT.
   Return pointer to object described by the context  */

tree
get_polymorphic_call_info (tree fndecl,
			   tree ref,
			   tree *otr_type,
			   HOST_WIDE_INT *otr_token,
			   ipa_polymorphic_call_context *context)
{
  tree base_pointer;
  *otr_type = obj_type_ref_class (ref);
  *otr_token = tree_to_uhwi (OBJ_TYPE_REF_TOKEN (ref));

  /* Set up basic info in case we find nothing interesting in the analysis.  */
  context->outer_type = *otr_type;
  context->offset = 0;
  base_pointer = OBJ_TYPE_REF_OBJECT (ref);
  context->maybe_derived_type = true;
  context->maybe_in_construction = true;

  /* Walk SSA for outer object.  */
  do 
    {
      if (TREE_CODE (base_pointer) == SSA_NAME
	  && !SSA_NAME_IS_DEFAULT_DEF (base_pointer)
	  && SSA_NAME_DEF_STMT (base_pointer)
	  && gimple_assign_single_p (SSA_NAME_DEF_STMT (base_pointer)))
	{
	  base_pointer = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (base_pointer));
	  STRIP_NOPS (base_pointer);
	}
      else if (TREE_CODE (base_pointer) == ADDR_EXPR)
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
		  context->offset
		     += offset2 + mem_ref_offset (base).low * BITS_PER_UNIT;
		  context->outer_type = NULL;
		}
	      /* We found base object.  In this case the outer_type
		 is known.  */
	      else if (DECL_P (base))
		{
		  gcc_assert (!POINTER_TYPE_P (TREE_TYPE (base)));

		  /* Only type inconsistent programs can have otr_type that is
		     not part of outer type.  */
		  if (!contains_type_p (TREE_TYPE (base),
					context->offset + offset2, *otr_type))
		    {
		      /* Use OTR_TOKEN = INT_MAX as a marker of probably type inconsistent
			 code sequences; we arrange the calls to be builtin_unreachable
			 later.  */
		      *otr_token = INT_MAX;
		      return base_pointer;
		    }
		  get_polymorphic_call_info_for_decl (context, base,
						      context->offset + offset2);
		  return NULL;
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
	  context->offset += tree_to_shwi (TREE_OPERAND (base_pointer, 1))
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
	  context->outer_type = TREE_TYPE (TREE_TYPE (base_pointer));
	  gcc_assert (TREE_CODE (context->outer_type) == RECORD_TYPE);

	  /* Dynamic casting has possibly upcasted the type
	     in the hiearchy.  In this case outer type is less
	     informative than inner type and we should forget
	     about it.  */
	  if (!contains_type_p (context->outer_type, context->offset,
				*otr_type))
	    {
	      context->outer_type = NULL;
	      return base_pointer;
	    }

	  /* If the function is constructor or destructor, then
	     the type is possibly in construction, but we know
	     it is not derived type.  */
	  if (DECL_CXX_CONSTRUCTOR_P (fndecl)
	      || DECL_CXX_DESTRUCTOR_P (fndecl))
	    {
	      context->maybe_in_construction = true;
	      context->maybe_derived_type = false;
	    }
	  else
	    {
	      context->maybe_derived_type = true;
	      context->maybe_in_construction = false;
	    }
	  return base_pointer;
	}
      /* Non-PODs passed by value are really passed by invisible
	 reference.  In this case we also know the type of the
	 object.  */
      if (DECL_BY_REFERENCE (SSA_NAME_VAR (base_pointer)))
	{
	  context->outer_type = TREE_TYPE (TREE_TYPE (base_pointer));
	  gcc_assert (!POINTER_TYPE_P (context->outer_type));
	  /* Only type inconsistent programs can have otr_type that is
	     not part of outer type.  */
	  if (!contains_type_p (context->outer_type, context->offset,
			        *otr_type))
	    { 
	      /* Use OTR_TOKEN = INT_MAX as a marker of probably type inconsistent
		 code sequences; we arrange the calls to be builtin_unreachable
		 later.  */
	      *otr_token = INT_MAX;
	      return base_pointer;
	    }
	  context->maybe_derived_type = false;
	  context->maybe_in_construction = false;
          return base_pointer;
	}
    }
  /* TODO: There are multiple ways to derive a type.  For instance
     if BASE_POINTER is passed to an constructor call prior our refernece.
     We do not make this type of flow sensitive analysis yet.  */
  return base_pointer;
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
			   pointer_set_t *inserted,
			   pointer_set_t *matched_vtables,
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
      if (!pointer_set_insert (matched_vtables, BINFO_VTABLE (base_binfo)))
	{
	  bool can_refer;
	  tree target = gimple_get_virt_method_for_binfo (otr_token,
							  base_binfo,
							  &can_refer);
	  if (!target || ! DECL_CXX_DESTRUCTOR_P (target))
	    maybe_record_node (nodes, target, inserted, can_refer, completep);
	  pointer_set_insert (matched_vtables, BINFO_VTABLE (base_binfo));
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

/* Return vector containing possible targets of polymorphic call of type
   OTR_TYPE caling method OTR_TOKEN within type of OTR_OUTER_TYPE and OFFSET.
   If INCLUDE_BASES is true, walk also base types of OUTER_TYPES containig
   OTR_TYPE and include their virtual method.  This is useful for types
   possibly in construction or destruction where the virtual table may
   temporarily change to one of base types.  INCLUDE_DERIVER_TYPES make
   us to walk the inheritance graph for all derivations.

   OTR_TOKEN == INT_MAX is used to mark calls that are provably
   undefined and should be redirected to unreachable.

   If COMPLETEP is non-NULL, store true if the list is complete. 
   CACHE_TOKEN (if non-NULL) will get stored to an unique ID of entry
   in the target cache.  If user needs to visit every target list
   just once, it can memoize them.

   NONCONSTRUCTION_TARGETS specify number of targets with asumption that
   the type is not in the construction.  Those targets appear first in the
   vector returned.

   Returned vector is placed into cache.  It is NOT caller's responsibility
   to free it.  The vector can be freed on cgraph_remove_node call if
   the particular node is a virtual function present in the cache.  */

vec <cgraph_node *>
possible_polymorphic_call_targets (tree otr_type,
			           HOST_WIDE_INT otr_token,
				   ipa_polymorphic_call_context context,
			           bool *completep,
			           void **cache_token,
				   int *nonconstruction_targetsp)
{
  static struct cgraph_node_hook_list *node_removal_hook_holder;
  pointer_set_t *inserted;
  pointer_set_t *matched_vtables;
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

  /* If ODR is not initialized, return empty incomplete list.  */
  if (!odr_hash.is_created ())
    {
      if (completep)
	*completep = false;
      if (nonconstruction_targetsp)
	*nonconstruction_targetsp = 0;
      return nodes;
    }

  /* If we hit type inconsistency, just return empty list of targets.  */
  if (otr_token == INT_MAX)
    {
      if (completep)
	*completep = true;
      if (nonconstruction_targetsp)
	*nonconstruction_targetsp = 0;
      return nodes;
    }

  type = get_odr_type (otr_type, true);

  /* Lookup the outer class type we want to walk.  */
  if (context.outer_type
      && !get_class_context (&context, otr_type))
    {
      if (completep)
	*completep = false;
      if (nonconstruction_targetsp)
	*nonconstruction_targetsp = 0;
      return nodes;
    }

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
      cached_polymorphic_call_targets = pointer_set_create ();
      polymorphic_call_target_hash.create (23);
      if (!node_removal_hook_holder)
	{
	  node_removal_hook_holder =
	    cgraph_add_node_removal_hook (&devirt_node_removal_hook, NULL);
	  varpool_add_node_removal_hook (&devirt_variable_node_removal_hook,
					 NULL);
	}
    }

  /* Lookup cached answer.  */
  key.type = type;
  key.otr_token = otr_token;
  key.context = context;
  slot = polymorphic_call_target_hash.find_slot (&key, INSERT);
  if (cache_token)
   *cache_token = (void *)*slot;
  if (*slot)
    {
      if (completep)
	*completep = (*slot)->complete;
      if (nonconstruction_targetsp)
	*nonconstruction_targetsp = (*slot)->nonconstruction_targets;
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

  inserted = pointer_set_create ();
  matched_vtables = pointer_set_create ();

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
    maybe_record_node (nodes, target, inserted, can_refer, &complete);
  else
    {
      skipped = true;
      gcc_assert (in_lto_p || context.maybe_derived_type);
    }

  pointer_set_insert (matched_vtables, BINFO_VTABLE (binfo));

  /* Next walk recursively all derived types.  */
  if (context.maybe_derived_type)
    {
      /* For anonymous namespace types we can attempt to build full type.
	 All derivations must be in this unit (unless we see partial unit).  */
      if (!type->all_derivations_known)
	complete = false;
      for (i = 0; i < outer_type->derived_types.length(); i++)
	possible_polymorphic_call_targets_1 (nodes, inserted,
					     matched_vtables,
					     otr_type,
					     outer_type->derived_types[i],
					     otr_token, outer_type->type,
					     context.offset, &complete,
					     bases_to_consider,
					     context.maybe_in_construction);
    }

  /* Finally walk bases, if asked to.  */
  (*slot)->nonconstruction_targets = nodes.length();

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
				   context.offset, nodes, inserted,
				   matched_vtables, &complete);
      if (skipped)
        maybe_record_node (nodes, target, inserted, can_refer, &complete);
      for (i = 0; i < bases_to_consider.length(); i++)
        maybe_record_node (nodes, bases_to_consider[i], inserted, can_refer, &complete);
    }
  bases_to_consider.release();

  (*slot)->targets = nodes;
  (*slot)->complete = complete;
  if (completep)
    *completep = complete;
  if (nonconstruction_targetsp)
    *nonconstruction_targetsp = (*slot)->nonconstruction_targets;

  pointer_set_destroy (inserted);
  pointer_set_destroy (matched_vtables);
  timevar_pop (TV_IPA_VIRTUAL_CALL);
  return nodes;
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
  odr_type type = get_odr_type (otr_type, false);
  unsigned int i;
  int nonconstruction;

  if (!type)
    return;
  targets = possible_polymorphic_call_targets (otr_type, otr_token,
					       ctx,
					       &final, NULL, &nonconstruction);
  fprintf (f, "  Targets of polymorphic call of type %i:", type->id);
  print_generic_expr (f, type->type, TDF_SLIM);
  fprintf (f, " token %i\n", (int)otr_token);
  if (ctx.outer_type || ctx.offset)
    {
      fprintf (f, "    Contained in type:");
      print_generic_expr (f, ctx.outer_type, TDF_SLIM);
      fprintf (f, " at offset "HOST_WIDE_INT_PRINT_DEC"\n",
	       ctx.offset);
    }

  fprintf (f, "    %s%s%s\n      ",
	   final ? "This is a complete list." :
	   "This is partial list; extra targets may be defined in other units.",
	   ctx.maybe_in_construction ? " (base types included)" : "",
	   ctx.maybe_derived_type ? " (derived types included)" : "");
  for (i = 0; i < targets.length (); i++)
    {
      char *name = NULL;
      if (i == (unsigned)nonconstruction)
	fprintf (f, "\n     If the type is in construction,"
		 " then additional tarets are:\n"
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

  if (!odr_hash.is_created ())
    return true;
  targets = possible_polymorphic_call_targets (otr_type, otr_token, ctx, &final);
  for (i = 0; i < targets.length (); i++)
    if (symtab_semantically_equivalent_p (n, targets[i]))
      return true;

  /* At a moment we allow middle end to dig out new external declarations
     as a targets of polymorphic calls.  */
  if (!final && !n->definition)
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
  /* We reconstruct the graph starting from types of all methods seen in the
     the unit.  */
  FOR_EACH_FUNCTION (n)
    if (DECL_VIRTUAL_P (n->decl)
	&& !n->definition
	&& symtab_real_symbol_p (n))
      get_odr_type (method_class_type (TREE_TYPE (n->decl)), true);
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

/* The ipa-devirt pass.
   When polymorphic call has only one likely target in the unit,
   turn it into speculative call.  */

static unsigned int
ipa_devirt (void)
{
  struct cgraph_node *n;
  struct pointer_set_t *bad_call_targets = pointer_set_create ();
  struct cgraph_edge *e;

  int npolymorphic = 0, nspeculated = 0, nconverted = 0, ncold = 0;
  int nmultiple = 0, noverwritable = 0, ndevirtualized = 0, nnotdefined = 0;
  int nwrong = 0, nok = 0, nexternal = 0, nartificial = 0;

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
	    int nonconstruction_targets;
	    vec <cgraph_node *>targets
	       = possible_polymorphic_call_targets
		    (e, &final, &cache_token, &nonconstruction_targets);
	    unsigned int i;

	    if (dump_file)
	      dump_possible_polymorphic_call_targets 
		(dump_file, e);

	    npolymorphic++;

	    if (!cgraph_maybe_hot_edge_p (e))
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
	    if (pointer_set_contains (bad_call_targets,
				      cache_token))
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
		      if (i < (unsigned) nonconstruction_targets)
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
		pointer_set_insert (bad_call_targets, cache_token);
	        continue;
	      }
	    /* This is reached only when dumping; check if we agree or disagree
 	       with the speculation.  */
	    if (e->speculative)
	      {
		struct cgraph_edge *e2;
		struct ipa_ref *ref;
		cgraph_speculative_call_info (e, e2, e, ref);
		if (cgraph_function_or_thunk_node (e2->callee, NULL)
		    == cgraph_function_or_thunk_node (likely_target, NULL))
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
	      = cgraph_function_node (likely_target);
	    if (DECL_ARTIFICIAL (non_thunk_target->decl)
		&& DECL_COMDAT (non_thunk_target->decl))
	      {
		if (dump_file)
		  fprintf (dump_file, "Target is artificial\n\n");
		nartificial++;
		continue;
	      }
	    if (cgraph_function_body_availability (likely_target)
		<= AVAIL_OVERWRITABLE
		&& symtab_can_be_discarded (likely_target))
	      {
		if (dump_file)
		  fprintf (dump_file, "Target is overwritable\n\n");
		noverwritable++;
		continue;
	      }
	    else
	      {
		if (dump_file)
		  fprintf (dump_file,
			   "Speculatively devirtualizing call in %s/%i to %s/%i\n\n",
			   n->name (), n->order,
			   likely_target->name (),
			   likely_target->order);
		if (!symtab_can_be_discarded (likely_target))
		  {
		    cgraph_node *alias;
		    alias = cgraph (symtab_nonoverwritable_alias
				     (likely_target));
		    if (alias)
		      likely_target = alias;
		  }
		nconverted++;
		update = true;
		cgraph_turn_edge_to_speculative
		  (e, likely_target, e->count * 8 / 10, e->frequency * 8 / 10);
	      }
	  }
      if (update)
	inline_update_overall_summary (n);
    }
  pointer_set_destroy (bad_call_targets);

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
  true, /* has_execute */
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
	      && flag_devirtualize_speculatively
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
