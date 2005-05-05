/* Language-dependent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

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
#include "flags.h"
#include "real.h"
#include "rtl.h"
#include "toplev.h"
#include "insn-config.h"
#include "integrate.h"
#include "tree-inline.h"
#include "target.h"

static tree bot_manip (tree *, int *, void *);
static tree bot_replace (tree *, int *, void *);
static tree build_cplus_array_type_1 (tree, tree);
static int list_hash_eq (const void *, const void *);
static hashval_t list_hash_pieces (tree, tree, tree);
static hashval_t list_hash (const void *);
static cp_lvalue_kind lvalue_p_1 (tree, int);
static tree no_linkage_helper (tree *, int *, void *);
static tree mark_local_for_remap_r (tree *, int *, void *);
static tree cp_unsave_r (tree *, int *, void *);
static tree build_target_expr (tree, tree);
static tree count_trees_r (tree *, int *, void *);
static tree verify_stmt_tree_r (tree *, int *, void *);
static tree find_tree_r (tree *, int *, void *);
static tree build_local_temp (tree);

static tree handle_java_interface_attribute (tree *, tree, tree, int, bool *);
static tree handle_com_interface_attribute (tree *, tree, tree, int, bool *);
static tree handle_init_priority_attribute (tree *, tree, tree, int, bool *);

/* If REF is an lvalue, returns the kind of lvalue that REF is.
   Otherwise, returns clk_none.  If TREAT_CLASS_RVALUES_AS_LVALUES is
   nonzero, rvalues of class type are considered lvalues.  */

static cp_lvalue_kind
lvalue_p_1 (tree ref, 
            int treat_class_rvalues_as_lvalues)
{
  cp_lvalue_kind op1_lvalue_kind = clk_none;
  cp_lvalue_kind op2_lvalue_kind = clk_none;

  if (TREE_CODE (TREE_TYPE (ref)) == REFERENCE_TYPE)
    return clk_ordinary;

  if (ref == current_class_ptr)
    return clk_none;

  switch (TREE_CODE (ref))
    {
      /* preincrements and predecrements are valid lvals, provided
	 what they refer to are valid lvals.  */
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case SAVE_EXPR:
    case UNSAVE_EXPR:
    case TRY_CATCH_EXPR:
    case WITH_CLEANUP_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      return lvalue_p_1 (TREE_OPERAND (ref, 0),
			 treat_class_rvalues_as_lvalues);

    case COMPONENT_REF:
      op1_lvalue_kind = lvalue_p_1 (TREE_OPERAND (ref, 0),
				    treat_class_rvalues_as_lvalues);
      /* In an expression of the form "X.Y", the packed-ness of the
	 expression does not depend on "X".  */
      op1_lvalue_kind &= ~clk_packed;
      /* Look at the member designator.  */
      if (!op1_lvalue_kind 
	  /* The "field" can be a FUNCTION_DECL or an OVERLOAD in some	
  	     situations.  */
 	  || TREE_CODE (TREE_OPERAND (ref, 1)) != FIELD_DECL)
 	;
      else if (DECL_C_BIT_FIELD (TREE_OPERAND (ref, 1)))
	{
	  /* Clear the ordinary bit.  If this object was a class
	     rvalue we want to preserve that information.  */
	  op1_lvalue_kind &= ~clk_ordinary;
	  /* The lvalue is for a bitfield.  */
	  op1_lvalue_kind |= clk_bitfield;
	}
      else if (DECL_PACKED (TREE_OPERAND (ref, 1)))
	op1_lvalue_kind |= clk_packed;
      
      return op1_lvalue_kind;

    case STRING_CST:
      return clk_ordinary;

    case VAR_DECL:
      if (TREE_READONLY (ref) && ! TREE_STATIC (ref)
	  && DECL_LANG_SPECIFIC (ref)
	  && DECL_IN_AGGR_P (ref))
	return clk_none;
    case INDIRECT_REF:
    case ARRAY_REF:
    case PARM_DECL:
    case RESULT_DECL:
      if (TREE_CODE (TREE_TYPE (ref)) != METHOD_TYPE)
	return clk_ordinary;
      break;

      /* A currently unresolved scope ref.  */
    case SCOPE_REF:
      abort ();
    case MAX_EXPR:
    case MIN_EXPR:
      op1_lvalue_kind = lvalue_p_1 (TREE_OPERAND (ref, 0),
				    treat_class_rvalues_as_lvalues);
      op2_lvalue_kind = lvalue_p_1 (TREE_OPERAND (ref, 1),
				    treat_class_rvalues_as_lvalues);
      break;

    case COND_EXPR:
      op1_lvalue_kind = lvalue_p_1 (TREE_OPERAND (ref, 1),
				    treat_class_rvalues_as_lvalues);
      op2_lvalue_kind = lvalue_p_1 (TREE_OPERAND (ref, 2),
				    treat_class_rvalues_as_lvalues);
      break;

    case MODIFY_EXPR:
      return clk_ordinary;

    case COMPOUND_EXPR:
      return lvalue_p_1 (TREE_OPERAND (ref, 1),
			 treat_class_rvalues_as_lvalues);

    case TARGET_EXPR:
      return treat_class_rvalues_as_lvalues ? clk_class : clk_none;

    case CALL_EXPR:
    case VA_ARG_EXPR:
      /* Any class-valued call would be wrapped in a TARGET_EXPR.  */
      return clk_none;

    case FUNCTION_DECL:
      /* All functions (except non-static-member functions) are
	 lvalues.  */
      return (DECL_NONSTATIC_MEMBER_FUNCTION_P (ref) 
	      ? clk_none : clk_ordinary);

    case NON_DEPENDENT_EXPR:
      /* We must consider NON_DEPENDENT_EXPRs to be lvalues so that
	 things like "&E" where "E" is an expression with a
	 non-dependent type work. It is safe to be lenient because an
	 error will be issued when the template is instantiated if "E"
	 is not an lvalue.  */
      return clk_ordinary;

    default:
      break;
    }

  /* If one operand is not an lvalue at all, then this expression is
     not an lvalue.  */
  if (!op1_lvalue_kind || !op2_lvalue_kind)
    return clk_none;

  /* Otherwise, it's an lvalue, and it has all the odd properties
     contributed by either operand.  */
  op1_lvalue_kind = op1_lvalue_kind | op2_lvalue_kind;
  /* It's not an ordinary lvalue if it involves either a bit-field or
     a class rvalue.  */
  if ((op1_lvalue_kind & ~clk_ordinary) != clk_none)
    op1_lvalue_kind &= ~clk_ordinary;
  return op1_lvalue_kind;
}

/* Returns the kind of lvalue that REF is, in the sense of
   [basic.lval].  This function should really be named lvalue_p; it
   computes the C++ definition of lvalue.  */

cp_lvalue_kind
real_lvalue_p (tree ref)
{
  return lvalue_p_1 (ref, 
		     /*treat_class_rvalues_as_lvalues=*/0);
}

/* This differs from real_lvalue_p in that class rvalues are
   considered lvalues.  */

int
lvalue_p (tree ref)
{
  return 
    (lvalue_p_1 (ref, /*class rvalue ok*/ 1) != clk_none);
}

/* Return nonzero if REF is an lvalue valid for this language;
   otherwise, print an error message and return zero.  */

int
lvalue_or_else (tree ref, const char* string)
{
  if (!lvalue_p (ref))
    {
      error ("non-lvalue in %s", string);
      return 0;
    }
  return 1;
}

/* Build a TARGET_EXPR, initializing the DECL with the VALUE.  */

static tree
build_target_expr (tree decl, tree value)
{
  tree t;

  t = build (TARGET_EXPR, TREE_TYPE (decl), decl, value, 
	     cxx_maybe_build_cleanup (decl), NULL_TREE);
  /* We always set TREE_SIDE_EFFECTS so that expand_expr does not
     ignore the TARGET_EXPR.  If there really turn out to be no
     side-effects, then the optimizer should be able to get rid of
     whatever code is generated anyhow.  */
  TREE_SIDE_EFFECTS (t) = 1;

  return t;
}

/* Return an undeclared local temporary of type TYPE for use in building a
   TARGET_EXPR.  */

static tree
build_local_temp (tree type)
{
  tree slot = build_decl (VAR_DECL, NULL_TREE, type);
  DECL_ARTIFICIAL (slot) = 1;
  DECL_CONTEXT (slot) = current_function_decl;
  layout_decl (slot, 0);
  return slot;
}

/* INIT is a CALL_EXPR which needs info about its target.
   TYPE is the type that this initialization should appear to have.

   Build an encapsulation of the initialization to perform
   and return it so that it can be processed by language-independent
   and language-specific expression expanders.  */

tree
build_cplus_new (tree type, tree init)
{
  tree fn;
  tree slot;
  tree rval;
  int is_ctor;

  /* Make sure that we're not trying to create an instance of an
     abstract class.  */
  abstract_virtuals_error (NULL_TREE, type);

  if (TREE_CODE (init) != CALL_EXPR && TREE_CODE (init) != AGGR_INIT_EXPR)
    return convert (type, init);

  fn = TREE_OPERAND (init, 0);
  is_ctor = (TREE_CODE (fn) == ADDR_EXPR
	     && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
	     && DECL_CONSTRUCTOR_P (TREE_OPERAND (fn, 0)));

  slot = build_local_temp (type);

  /* We split the CALL_EXPR into its function and its arguments here.
     Then, in expand_expr, we put them back together.  The reason for
     this is that this expression might be a default argument
     expression.  In that case, we need a new temporary every time the
     expression is used.  That's what break_out_target_exprs does; it
     replaces every AGGR_INIT_EXPR with a copy that uses a fresh
     temporary slot.  Then, expand_expr builds up a call-expression
     using the new slot.  */

  /* If we don't need to use a constructor to create an object of this
     type, don't mess with AGGR_INIT_EXPR.  */
  if (is_ctor || TREE_ADDRESSABLE (type))
    {
      rval = build (AGGR_INIT_EXPR, type, fn, TREE_OPERAND (init, 1), slot);
      TREE_SIDE_EFFECTS (rval) = 1;
      AGGR_INIT_VIA_CTOR_P (rval) = is_ctor;
    }
  else
    rval = init;

  rval = build_target_expr (slot, rval);

  return rval;
}

/* Build a TARGET_EXPR using INIT to initialize a new temporary of the
   indicated TYPE.  */

tree
build_target_expr_with_type (tree init, tree type)
{
  tree slot;

  if (TREE_CODE (init) == TARGET_EXPR)
    return init;
  else if (CLASS_TYPE_P (type) && !TYPE_HAS_TRIVIAL_INIT_REF (type)
	   && TREE_CODE (init) != COND_EXPR
	   && TREE_CODE (init) != CONSTRUCTOR
	   && TREE_CODE (init) != VA_ARG_EXPR)
    /* We need to build up a copy constructor call.  COND_EXPR is a special
       case because we already have copies on the arms and we don't want
       another one here.  A CONSTRUCTOR is aggregate initialization, which
       is handled separately.  A VA_ARG_EXPR is magic creation of an
       aggregate; there's no additional work to be done.  */
    return force_rvalue (init);

  slot = build_local_temp (type);
  return build_target_expr (slot, init);
}

/* Like the above function, but without the checking.  This function should
   only be used by code which is deliberately trying to subvert the type
   system, such as call_builtin_trap.  */

tree
force_target_expr (tree type, tree init)
{
  tree slot = build_local_temp (type);
  return build_target_expr (slot, init);
}

/* Like build_target_expr_with_type, but use the type of INIT.  */

tree
get_target_expr (tree init)
{
  return build_target_expr_with_type (init, TREE_TYPE (init));
}


static tree
build_cplus_array_type_1 (tree elt_type, tree index_type)
{
  tree t;

  if (elt_type == error_mark_node || index_type == error_mark_node)
    return error_mark_node;

  if (dependent_type_p (elt_type)
      || (index_type
	  && value_dependent_expression_p (TYPE_MAX_VALUE (index_type))))
    {
      t = make_node (ARRAY_TYPE);
      TREE_TYPE (t) = elt_type;
      TYPE_DOMAIN (t) = index_type;
    }
  else
    t = build_array_type (elt_type, index_type);

  /* Push these needs up so that initialization takes place
     more easily.  */
  TYPE_NEEDS_CONSTRUCTING (t) 
    = TYPE_NEEDS_CONSTRUCTING (TYPE_MAIN_VARIANT (elt_type));
  TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) 
    = TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TYPE_MAIN_VARIANT (elt_type));
  return t;
}

tree
build_cplus_array_type (tree elt_type, tree index_type)
{
  tree t;
  int type_quals = cp_type_quals (elt_type);

  if (type_quals != TYPE_UNQUALIFIED)
    elt_type = cp_build_qualified_type (elt_type, TYPE_UNQUALIFIED);

  t = build_cplus_array_type_1 (elt_type, index_type);

  if (type_quals != TYPE_UNQUALIFIED)
    t = cp_build_qualified_type (t, type_quals);

  return t;
}

/* Make a variant of TYPE, qualified with the TYPE_QUALS.  Handles
   arrays correctly.  In particular, if TYPE is an array of T's, and
   TYPE_QUALS is non-empty, returns an array of qualified T's.
  
   FLAGS determines how to deal with illformed qualifications. If
   tf_ignore_bad_quals is set, then bad qualifications are dropped
   (this is permitted if TYPE was introduced via a typedef or template
   type parameter). If bad qualifications are dropped and tf_warning
   is set, then a warning is issued for non-const qualifications.  If
   tf_ignore_bad_quals is not set and tf_error is not set, we
   return error_mark_node. Otherwise, we issue an error, and ignore
   the qualifications.

   Qualification of a reference type is valid when the reference came
   via a typedef or template type argument. [dcl.ref] No such
   dispensation is provided for qualifying a function type.  [dcl.fct]
   DR 295 queries this and the proposed resolution brings it into line
   with qualifying a reference.  We implement the DR.  We also behave
   in a similar manner for restricting non-pointer types.  */
 
tree
cp_build_qualified_type_real (tree type, 
                              int type_quals, 
                              tsubst_flags_t complain)
{
  tree result;
  int bad_quals = TYPE_UNQUALIFIED;

  if (type == error_mark_node)
    return type;

  if (type_quals == cp_type_quals (type))
    return type;

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* In C++, the qualification really applies to the array element
	 type.  Obtain the appropriately qualified element type.  */
      tree t;
      tree element_type 
	= cp_build_qualified_type_real (TREE_TYPE (type), 
					type_quals,
					complain);

      if (element_type == error_mark_node)
	return error_mark_node;

      /* See if we already have an identically qualified type.  */
      for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
	if (cp_type_quals (t) == type_quals 
	    && TYPE_NAME (t) == TYPE_NAME (type)
	    && TYPE_CONTEXT (t) == TYPE_CONTEXT (type))
	  break;
	  
      if (!t)
	{
	  /* Make a new array type, just like the old one, but with the
	     appropriately qualified element type.  */
	  t = build_type_copy (type);
	  TREE_TYPE (t) = element_type;
	}

      /* Even if we already had this variant, we update
	 TYPE_NEEDS_CONSTRUCTING and TYPE_HAS_NONTRIVIAL_DESTRUCTOR in case
	 they changed since the variant was originally created.  
	 
	 This seems hokey; if there is some way to use a previous
	 variant *without* coming through here,
	 TYPE_NEEDS_CONSTRUCTING will never be updated.  */
      TYPE_NEEDS_CONSTRUCTING (t) 
	= TYPE_NEEDS_CONSTRUCTING (TYPE_MAIN_VARIANT (element_type));
      TYPE_HAS_NONTRIVIAL_DESTRUCTOR (t) 
	= TYPE_HAS_NONTRIVIAL_DESTRUCTOR (TYPE_MAIN_VARIANT (element_type));
      return t;
    }
  else if (TYPE_PTRMEMFUNC_P (type))
    {
      /* For a pointer-to-member type, we can't just return a
	 cv-qualified version of the RECORD_TYPE.  If we do, we
	 haven't changed the field that contains the actual pointer to
	 a method, and so TYPE_PTRMEMFUNC_FN_TYPE will be wrong.  */
      tree t;

      t = TYPE_PTRMEMFUNC_FN_TYPE (type);
      t = cp_build_qualified_type_real (t, type_quals, complain);
      return build_ptrmemfunc_type (t);
    }
  
  /* A reference, function or method type shall not be cv qualified.
     [dcl.ref], [dct.fct]  */
  if (type_quals & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE)
      && (TREE_CODE (type) == REFERENCE_TYPE
	  || TREE_CODE (type) == FUNCTION_TYPE
	  || TREE_CODE (type) == METHOD_TYPE))
    {
      bad_quals |= type_quals & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE);
      type_quals &= ~(TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE);
    }
  
  /* A restrict-qualified type must be a pointer (or reference)
     to object or incomplete type.  */
  if ((type_quals & TYPE_QUAL_RESTRICT)
      && TREE_CODE (type) != TEMPLATE_TYPE_PARM
      && TREE_CODE (type) != TYPENAME_TYPE
      && !POINTER_TYPE_P (type))
    {
      bad_quals |= TYPE_QUAL_RESTRICT;
      type_quals &= ~TYPE_QUAL_RESTRICT;
    }

  if (bad_quals == TYPE_UNQUALIFIED)
    /*OK*/;
  else if (!(complain & (tf_error | tf_ignore_bad_quals)))
    return error_mark_node;
  else
    {
      if (complain & tf_ignore_bad_quals)
 	/* We're not going to warn about constifying things that can't
 	   be constified.  */
 	bad_quals &= ~TYPE_QUAL_CONST;
      if (bad_quals)
 	{
 	  tree bad_type = build_qualified_type (ptr_type_node, bad_quals);
 
 	  if (!(complain & tf_ignore_bad_quals))
 	    error ("`%V' qualifiers cannot be applied to `%T'",
		   bad_type, type);
 	}
    }
  
  /* Retrieve (or create) the appropriately qualified variant.  */
  result = build_qualified_type (type, type_quals);

  /* If this was a pointer-to-method type, and we just made a copy,
     then we need to unshare the record that holds the cached
     pointer-to-member-function type, because these will be distinct
     between the unqualified and qualified types.  */
  if (result != type 
      && TREE_CODE (type) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE)
    TYPE_LANG_SPECIFIC (result) = NULL;

  return result;
}

/* Returns the canonical version of TYPE.  In other words, if TYPE is
   a typedef, returns the underlying type.  The cv-qualification of
   the type returned matches the type input; they will always be
   compatible types.  */

tree
canonical_type_variant (tree t)
{
  return cp_build_qualified_type (TYPE_MAIN_VARIANT (t), cp_type_quals (t));
}

/* Makes new binfos for the indirect bases under BINFO. T is the most
   derived TYPE. PREV is the previous binfo, whose TREE_CHAIN we make
   point to this binfo. We return the last BINFO created.

   The CLASSTYPE_VBASECLASSES list of T is constructed in reverse
   order (pre-order, depth-first, right-to-left). You must nreverse it.

   The BINFO_INHERITANCE of a virtual base class points to the binfo
   og the most derived type.

   The binfo's TREE_CHAIN is set to inheritance graph order, but bases
   for non-class types are not included (i.e. those which are
   dependent bases in non-instantiated templates).  */

tree
copy_base_binfos (tree binfo, tree t, tree prev)
{
  tree binfos = BINFO_BASETYPES (binfo);
  int n, ix;

  if (prev)
    TREE_CHAIN (prev) = binfo;
  prev = binfo;
  
  if (binfos == NULL_TREE)
    return prev;

  n = TREE_VEC_LENGTH (binfos);
  
  /* Now copy the structure beneath BINFO.  */
  for (ix = 0; ix != n; ix++)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, ix);
      tree new_binfo = NULL_TREE;

      if (!CLASS_TYPE_P (BINFO_TYPE (base_binfo)))
	{
	  my_friendly_assert (binfo == TYPE_BINFO (t), 20030204);
	  
	  new_binfo = base_binfo;
	  TREE_CHAIN (prev) = new_binfo;
	  prev = new_binfo;
	  BINFO_INHERITANCE_CHAIN (new_binfo) = binfo;
	  BINFO_DEPENDENT_BASE_P (new_binfo) = 1;
	}
      else if (TREE_VIA_VIRTUAL (base_binfo))
	{
	  new_binfo = purpose_member (BINFO_TYPE (base_binfo),
				      CLASSTYPE_VBASECLASSES (t));
	  if (new_binfo)
	    new_binfo = TREE_VALUE (new_binfo);
	}
      
      if (!new_binfo)
	{
	  new_binfo = make_binfo (BINFO_OFFSET (base_binfo),
				  base_binfo, NULL_TREE,
				  BINFO_VIRTUALS (base_binfo));
	  prev = copy_base_binfos (new_binfo, t, prev);
	  if (TREE_VIA_VIRTUAL (base_binfo))
	    {
	      CLASSTYPE_VBASECLASSES (t)
		= tree_cons (BINFO_TYPE (new_binfo), new_binfo,
			     CLASSTYPE_VBASECLASSES (t));
	      TREE_VIA_VIRTUAL (new_binfo) = 1;
	      BINFO_INHERITANCE_CHAIN (new_binfo) = TYPE_BINFO (t);
	    }
	  else
	    BINFO_INHERITANCE_CHAIN (new_binfo) = binfo;
	}
      TREE_VEC_ELT (binfos, ix) = new_binfo;
    }

  return prev;
}


/* Hashing of lists so that we don't make duplicates.
   The entry point is `list_hash_canon'.  */

/* Now here is the hash table.  When recording a list, it is added
   to the slot whose index is the hash code mod the table size.
   Note that the hash table is used for several kinds of lists.
   While all these live in the same table, they are completely independent,
   and the hash code is computed differently for each of these.  */

static GTY ((param_is (union tree_node))) htab_t list_hash_table;

struct list_proxy 
{
  tree purpose;
  tree value;
  tree chain;
};

/* Compare ENTRY (an entry in the hash table) with DATA (a list_proxy
   for a node we are thinking about adding).  */

static int
list_hash_eq (const void* entry, const void* data)
{
  tree t = (tree) entry;
  struct list_proxy *proxy = (struct list_proxy *) data;

  return (TREE_VALUE (t) == proxy->value
	  && TREE_PURPOSE (t) == proxy->purpose
	  && TREE_CHAIN (t) == proxy->chain);
}

/* Compute a hash code for a list (chain of TREE_LIST nodes
   with goodies in the TREE_PURPOSE, TREE_VALUE, and bits of the
   TREE_COMMON slots), by adding the hash codes of the individual entries.  */

static hashval_t
list_hash_pieces (tree purpose, tree value, tree chain)
{
  hashval_t hashcode = 0;
  
  if (chain)
    hashcode += TYPE_HASH (chain);
  
  if (value)
    hashcode += TYPE_HASH (value);
  else
    hashcode += 1007;
  if (purpose)
    hashcode += TYPE_HASH (purpose);
  else
    hashcode += 1009;
  return hashcode;
}

/* Hash an already existing TREE_LIST.  */

static hashval_t
list_hash (const void* p)
{
  tree t = (tree) p;
  return list_hash_pieces (TREE_PURPOSE (t), 
			   TREE_VALUE (t), 
			   TREE_CHAIN (t));
}

/* Given list components PURPOSE, VALUE, AND CHAIN, return the canonical
   object for an identical list if one already exists.  Otherwise, build a
   new one, and record it as the canonical object.  */

tree
hash_tree_cons (tree purpose, tree value, tree chain)
{
  int hashcode = 0;
  void **slot;
  struct list_proxy proxy;

  /* Hash the list node.  */
  hashcode = list_hash_pieces (purpose, value, chain);
  /* Create a proxy for the TREE_LIST we would like to create.  We
     don't actually create it so as to avoid creating garbage.  */
  proxy.purpose = purpose;
  proxy.value = value;
  proxy.chain = chain;
  /* See if it is already in the table.  */
  slot = htab_find_slot_with_hash (list_hash_table, &proxy, hashcode,
				   INSERT);
  /* If not, create a new node.  */
  if (!*slot)
    *slot = tree_cons (purpose, value, chain);
  return *slot;
}

/* Constructor for hashed lists.  */

tree
hash_tree_chain (tree value, tree chain)
{
  return hash_tree_cons (NULL_TREE, value, chain);
}

/* Similar, but used for concatenating two lists.  */

tree
hash_chainon (tree list1, tree list2)
{
  if (list2 == 0)
    return list1;
  if (list1 == 0)
    return list2;
  if (TREE_CHAIN (list1) == NULL_TREE)
    return hash_tree_chain (TREE_VALUE (list1), list2);
  return hash_tree_chain (TREE_VALUE (list1),
			  hash_chainon (TREE_CHAIN (list1), list2));
}

/* Build an association between TYPE and some parameters:

   OFFSET is the offset added to `this' to convert it to a pointer
   of type `TYPE *'

   BINFO is the base binfo to use, if we are deriving from one.  This
   is necessary, as we want specialized parent binfos from base
   classes, so that the VTABLE_NAMEs of bases are for the most derived
   type, instead of the simple type.

   VTABLE is the virtual function table with which to initialize
   sub-objects of type TYPE.

   VIRTUALS are the virtual functions sitting in VTABLE.  */

tree
make_binfo (tree offset, tree binfo, tree vtable, tree virtuals)
{
  tree new_binfo = make_tree_vec (BINFO_LANG_ELTS);
  tree type;

  if (TREE_CODE (binfo) == TREE_VEC)
    {
      type = BINFO_TYPE (binfo);
      BINFO_DEPENDENT_BASE_P (new_binfo) = BINFO_DEPENDENT_BASE_P (binfo);
    }
  else
    {
      type = binfo;
      binfo = NULL_TREE;
      BINFO_DEPENDENT_BASE_P (new_binfo) = 1;
    }

  TREE_TYPE (new_binfo) = TYPE_MAIN_VARIANT (type);
  BINFO_OFFSET (new_binfo) = offset;
  BINFO_VTABLE (new_binfo) = vtable;
  BINFO_VIRTUALS (new_binfo) = virtuals;

  if (binfo && !BINFO_DEPENDENT_BASE_P (binfo)
      && BINFO_BASETYPES (binfo) != NULL_TREE)
    {
      BINFO_BASETYPES (new_binfo) = copy_node (BINFO_BASETYPES (binfo));
      /* We do not need to copy the accesses, as they are read only.  */
      BINFO_BASEACCESSES (new_binfo) = BINFO_BASEACCESSES (binfo);
    }
  return new_binfo;
}

void
debug_binfo (tree elem)
{
  HOST_WIDE_INT n;
  tree virtuals;

  fprintf (stderr, "type \"%s\", offset = " HOST_WIDE_INT_PRINT_DEC
	   "\nvtable type:\n",
	   TYPE_NAME_STRING (BINFO_TYPE (elem)),
	   TREE_INT_CST_LOW (BINFO_OFFSET (elem)));
  debug_tree (BINFO_TYPE (elem));
  if (BINFO_VTABLE (elem))
    fprintf (stderr, "vtable decl \"%s\"\n",
	     IDENTIFIER_POINTER (DECL_NAME (get_vtbl_decl_for_binfo (elem))));
  else
    fprintf (stderr, "no vtable decl yet\n");
  fprintf (stderr, "virtuals:\n");
  virtuals = BINFO_VIRTUALS (elem);
  n = 0;

  while (virtuals)
    {
      tree fndecl = TREE_VALUE (virtuals);
      fprintf (stderr, "%s [%ld =? %ld]\n",
	       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl)),
	       (long) n, (long) TREE_INT_CST_LOW (DECL_VINDEX (fndecl)));
      ++n;
      virtuals = TREE_CHAIN (virtuals);
    }
}

int
count_functions (tree t)
{
  int i;
  if (TREE_CODE (t) == FUNCTION_DECL)
    return 1;
  else if (TREE_CODE (t) == OVERLOAD)
    {
      for (i = 0; t; t = OVL_CHAIN (t))
	i++;
      return i;
    }

  abort ();
  return 0;
}

int
is_overloaded_fn (tree x)
{
  /* A baselink is also considered an overloaded function.  */
  if (TREE_CODE (x) == OFFSET_REF)
    x = TREE_OPERAND (x, 1);
  if (BASELINK_P (x))
    x = BASELINK_FUNCTIONS (x);
  return (TREE_CODE (x) == FUNCTION_DECL
	  || TREE_CODE (x) == TEMPLATE_ID_EXPR
	  || DECL_FUNCTION_TEMPLATE_P (x)
	  || TREE_CODE (x) == OVERLOAD);
}

int
really_overloaded_fn (tree x)
{     
  /* A baselink is also considered an overloaded function.  */
  if (TREE_CODE (x) == OFFSET_REF)
    x = TREE_OPERAND (x, 1);
  if (BASELINK_P (x))
    x = BASELINK_FUNCTIONS (x);
  
  return ((TREE_CODE (x) == OVERLOAD && OVL_CHAIN (x))
	  || DECL_FUNCTION_TEMPLATE_P (OVL_CURRENT (x))
	  || TREE_CODE (x) == TEMPLATE_ID_EXPR);
}

tree
get_first_fn (tree from)
{
  my_friendly_assert (is_overloaded_fn (from), 9);
  /* A baselink is also considered an overloaded function.  */
  if (BASELINK_P (from))
    from = BASELINK_FUNCTIONS (from);
  return OVL_CURRENT (from);
}

/* Returns nonzero if T is a ->* or .* expression that refers to a
   member function.  */

int
bound_pmf_p (tree t)
{
  return (TREE_CODE (t) == OFFSET_REF
	  && TYPE_PTRMEMFUNC_P (TREE_TYPE (TREE_OPERAND (t, 1))));
}

/* Return a new OVL node, concatenating it with the old one.  */

tree
ovl_cons (tree decl, tree chain)
{
  tree result = make_node (OVERLOAD);
  TREE_TYPE (result) = unknown_type_node;
  OVL_FUNCTION (result) = decl;
  TREE_CHAIN (result) = chain;
  
  return result;
}

/* Build a new overloaded function. If this is the first one,
   just return it; otherwise, ovl_cons the _DECLs */

tree
build_overload (tree decl, tree chain)
{
  if (! chain && TREE_CODE (decl) != TEMPLATE_DECL)
    return decl;
  if (chain && TREE_CODE (chain) != OVERLOAD)
    chain = ovl_cons (chain, NULL_TREE);
  return ovl_cons (decl, chain);
}


#define PRINT_RING_SIZE 4

const char *
cxx_printable_name (tree decl, int v)
{
  static tree decl_ring[PRINT_RING_SIZE];
  static char *print_ring[PRINT_RING_SIZE];
  static int ring_counter;
  int i;

  /* Only cache functions.  */
  if (v < 2
      || TREE_CODE (decl) != FUNCTION_DECL
      || DECL_LANG_SPECIFIC (decl) == 0)
    return lang_decl_name (decl, v);

  /* See if this print name is lying around.  */
  for (i = 0; i < PRINT_RING_SIZE; i++)
    if (decl_ring[i] == decl)
      /* yes, so return it.  */
      return print_ring[i];

  if (++ring_counter == PRINT_RING_SIZE)
    ring_counter = 0;

  if (current_function_decl != NULL_TREE)
    {
      if (decl_ring[ring_counter] == current_function_decl)
	ring_counter += 1;
      if (ring_counter == PRINT_RING_SIZE)
	ring_counter = 0;
      if (decl_ring[ring_counter] == current_function_decl)
	abort ();
    }

  if (print_ring[ring_counter])
    free (print_ring[ring_counter]);

  print_ring[ring_counter] = xstrdup (lang_decl_name (decl, v));
  decl_ring[ring_counter] = decl;
  return print_ring[ring_counter];
}

/* Build the FUNCTION_TYPE or METHOD_TYPE which may throw exceptions
   listed in RAISES.  */

tree
build_exception_variant (tree type, tree raises)
{
  tree v = TYPE_MAIN_VARIANT (type);
  int type_quals = TYPE_QUALS (type);

  for (; v; v = TYPE_NEXT_VARIANT (v))
    if (TYPE_QUALS (v) == type_quals
        && comp_except_specs (raises, TYPE_RAISES_EXCEPTIONS (v), 1)
	&& (*targetm.comp_type_attributes) (type, v))
      return v;

  /* Need to build a new variant.  */
  v = build_type_copy (type);
  TYPE_RAISES_EXCEPTIONS (v) = raises;
  return v;
}

/* Given a TEMPLATE_TEMPLATE_PARM node T, create a new
   BOUND_TEMPLATE_TEMPLATE_PARM bound with NEWARGS as its template
   arguments.  */

tree
bind_template_template_parm (tree t, tree newargs)
{
  tree decl = TYPE_NAME (t);
  tree t2;

  t2 = make_aggr_type (BOUND_TEMPLATE_TEMPLATE_PARM);
  decl = build_decl (TYPE_DECL, DECL_NAME (decl), NULL_TREE);

  /* These nodes have to be created to reflect new TYPE_DECL and template
     arguments.  */
  TEMPLATE_TYPE_PARM_INDEX (t2) = copy_node (TEMPLATE_TYPE_PARM_INDEX (t));
  TEMPLATE_PARM_DECL (TEMPLATE_TYPE_PARM_INDEX (t2)) = decl;
  TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (t2)
    = tree_cons (TEMPLATE_TEMPLATE_PARM_TEMPLATE_DECL (t), 
		 newargs, NULL_TREE);

  TREE_TYPE (decl) = t2;
  TYPE_NAME (t2) = decl;
  TYPE_STUB_DECL (t2) = decl;
  TYPE_SIZE (t2) = 0;

  return t2;
}

/* Called from count_trees via walk_tree.  */

static tree
count_trees_r (tree* tp ATTRIBUTE_UNUSED , 
               int* walk_subtrees ATTRIBUTE_UNUSED , 
               void* data)
{
  ++ *((int*) data);
  return NULL_TREE;
}

/* Debugging function for measuring the rough complexity of a tree
   representation.  */

int
count_trees (tree t)
{
  int n_trees = 0;
  walk_tree_without_duplicates (&t, count_trees_r, &n_trees);
  return n_trees;
}  

/* Called from verify_stmt_tree via walk_tree.  */

static tree
verify_stmt_tree_r (tree* tp, 
                    int* walk_subtrees ATTRIBUTE_UNUSED , 
                    void* data)
{
  tree t = *tp;
  htab_t *statements = (htab_t *) data;
  void **slot;

  if (!STATEMENT_CODE_P (TREE_CODE (t)))
    return NULL_TREE;

  /* If this statement is already present in the hash table, then
     there is a circularity in the statement tree.  */
  if (htab_find (*statements, t))
    abort ();
  
  slot = htab_find_slot (*statements, t, INSERT);
  *slot = t;

  return NULL_TREE;
}

/* Debugging function to check that the statement T has not been
   corrupted.  For now, this function simply checks that T contains no
   circularities.  */

void
verify_stmt_tree (tree t)
{
  htab_t statements;
  statements = htab_create (37, htab_hash_pointer, htab_eq_pointer, NULL);
  walk_tree (&t, verify_stmt_tree_r, &statements, NULL);
  htab_delete (statements);
}

/* Called from find_tree via walk_tree.  */

static tree
find_tree_r (tree* tp, 
             int* walk_subtrees ATTRIBUTE_UNUSED , 
             void* data)
{
  if (*tp == (tree) data)
    return (tree) data;

  return NULL_TREE;
}

/* Returns X if X appears in the tree structure rooted at T.  */

tree
find_tree (tree t, tree x)
{
  return walk_tree_without_duplicates (&t, find_tree_r, x);
}

/* Passed to walk_tree.  Checks for the use of types with no linkage.  */

static tree
no_linkage_helper (tree* tp, 
                   int* walk_subtrees ATTRIBUTE_UNUSED , 
                   void* data ATTRIBUTE_UNUSED )
{
  tree t = *tp;

  if (TYPE_P (t)
      && (CLASS_TYPE_P (t) || TREE_CODE (t) == ENUMERAL_TYPE)
      && (decl_function_context (TYPE_MAIN_DECL (t))
	  || TYPE_ANONYMOUS_P (t)))
    return t;
  return NULL_TREE;
}

/* Check if the type T depends on a type with no linkage and if so, return
   it.  */

tree
no_linkage_check (tree t)
{
  /* There's no point in checking linkage on template functions; we
     can't know their complete types.  */
  if (processing_template_decl)
    return NULL_TREE;

  t = walk_tree_without_duplicates (&t, no_linkage_helper, NULL);
  if (t != error_mark_node)
    return t;
  return NULL_TREE;
}

#ifdef GATHER_STATISTICS
extern int depth_reached;
#endif

void
cxx_print_statistics (void)
{
  print_search_statistics ();
  print_class_statistics ();
#ifdef GATHER_STATISTICS
  fprintf (stderr, "maximum template instantiation depth reached: %d\n",
	   depth_reached);
#endif
}

/* Return, as an INTEGER_CST node, the number of elements for TYPE
   (which is an ARRAY_TYPE).  This counts only elements of the top
   array.  */

tree
array_type_nelts_top (tree type)
{
  return fold (build (PLUS_EXPR, sizetype,
		      array_type_nelts (type),
		      integer_one_node));
}

/* Return, as an INTEGER_CST node, the number of elements for TYPE
   (which is an ARRAY_TYPE).  This one is a recursive count of all
   ARRAY_TYPEs that are clumped together.  */

tree
array_type_nelts_total (tree type)
{
  tree sz = array_type_nelts_top (type);
  type = TREE_TYPE (type);
  while (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree n = array_type_nelts_top (type);
      sz = fold (build (MULT_EXPR, sizetype, sz, n));
      type = TREE_TYPE (type);
    }
  return sz;
}

/* Called from break_out_target_exprs via mapcar.  */

static tree
bot_manip (tree* tp, int* walk_subtrees, void* data)
{
  splay_tree target_remap = ((splay_tree) data);
  tree t = *tp;

  if (TREE_CONSTANT (t))
    {
      /* There can't be any TARGET_EXPRs or their slot variables below
         this point.  We used to check !TREE_SIDE_EFFECTS, but then we
         failed to copy an ADDR_EXPR of the slot VAR_DECL.  */
      *walk_subtrees = 0;
      return NULL_TREE;
    }
  if (TREE_CODE (t) == TARGET_EXPR)
    {
      tree u;

      if (TREE_CODE (TREE_OPERAND (t, 1)) == AGGR_INIT_EXPR)
	{
	  mark_used (TREE_OPERAND (TREE_OPERAND (TREE_OPERAND (t, 1), 0), 0));
	  u = build_cplus_new
	    (TREE_TYPE (t), break_out_target_exprs (TREE_OPERAND (t, 1)));
	}
      else 
	{
	  u = build_target_expr_with_type
	    (break_out_target_exprs (TREE_OPERAND (t, 1)), TREE_TYPE (t));
	}

      /* Map the old variable to the new one.  */
      splay_tree_insert (target_remap, 
			 (splay_tree_key) TREE_OPERAND (t, 0), 
			 (splay_tree_value) TREE_OPERAND (u, 0));

      /* Replace the old expression with the new version.  */
      *tp = u;
      /* We don't have to go below this point; the recursive call to
	 break_out_target_exprs will have handled anything below this
	 point.  */
      *walk_subtrees = 0;
      return NULL_TREE;
    }
  else if (TREE_CODE (t) == CALL_EXPR)
    mark_used (TREE_OPERAND (TREE_OPERAND (t, 0), 0));

  /* Make a copy of this node.  */
  return copy_tree_r (tp, walk_subtrees, NULL);
}
  
/* Replace all remapped VAR_DECLs in T with their new equivalents.
   DATA is really a splay-tree mapping old variables to new
   variables.  */

static tree
bot_replace (tree* t, 
             int* walk_subtrees ATTRIBUTE_UNUSED , 
             void* data)
{
  splay_tree target_remap = ((splay_tree) data);

  if (TREE_CODE (*t) == VAR_DECL)
    {
      splay_tree_node n = splay_tree_lookup (target_remap,
					     (splay_tree_key) *t);
      if (n)
	*t = (tree) n->value;
    }

  return NULL_TREE;
}
	
/* When we parse a default argument expression, we may create
   temporary variables via TARGET_EXPRs.  When we actually use the
   default-argument expression, we make a copy of the expression, but
   we must replace the temporaries with appropriate local versions.  */

tree
break_out_target_exprs (tree t)
{
  static int target_remap_count;
  static splay_tree target_remap;

  if (!target_remap_count++)
    target_remap = splay_tree_new (splay_tree_compare_pointers, 
				   /*splay_tree_delete_key_fn=*/NULL, 
				   /*splay_tree_delete_value_fn=*/NULL);
  walk_tree (&t, bot_manip, target_remap, NULL);
  walk_tree (&t, bot_replace, target_remap, NULL);

  if (!--target_remap_count)
    {
      splay_tree_delete (target_remap);
      target_remap = NULL;
    }

  return t;
}

/* Similar to `build_nt', but for template definitions of dependent
   expressions  */

tree
build_min_nt (enum tree_code code, ...)
{
  tree t;
  int length;
  int i;
  va_list p;

  va_start (p, code);

  t = make_node (code);
  length = TREE_CODE_LENGTH (code);
  TREE_COMPLEXITY (t) = input_line;

  for (i = 0; i < length; i++)
    {
      tree x = va_arg (p, tree);
      TREE_OPERAND (t, i) = x;
    }

  va_end (p);
  return t;
}

/* Similar to `build', but for template definitions.  */

tree
build_min (enum tree_code code, tree tt, ...)
{
  tree t;
  int length;
  int i;
  va_list p;

  va_start (p, tt);

  t = make_node (code);
  length = TREE_CODE_LENGTH (code);
  TREE_TYPE (t) = tt;
  TREE_COMPLEXITY (t) = input_line;

  for (i = 0; i < length; i++)
    {
      tree x = va_arg (p, tree);
      TREE_OPERAND (t, i) = x;
      if (x && TREE_SIDE_EFFECTS (x))
	TREE_SIDE_EFFECTS (t) = 1;
    }

  va_end (p);
  return t;
}

/* Similar to `build', but for template definitions of non-dependent
   expressions. NON_DEP is the non-dependent expression that has been
   built.  */

tree
build_min_non_dep (enum tree_code code, tree non_dep, ...)
{
  tree t;
  int length;
  int i;
  va_list p;

  va_start (p, non_dep);

  t = make_node (code);
  length = TREE_CODE_LENGTH (code);
  TREE_TYPE (t) = TREE_TYPE (non_dep);
  TREE_COMPLEXITY (t) = input_line;
  TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (non_dep);

  for (i = 0; i < length; i++)
    {
      tree x = va_arg (p, tree);
      TREE_OPERAND (t, i) = x;
    }

  if (code == COMPOUND_EXPR && TREE_CODE (non_dep) != COMPOUND_EXPR)
    /* This should not be considered a COMPOUND_EXPR, because it
       resolves to an overload.  */
    COMPOUND_EXPR_OVERLOADED (t) = 1;
  
  va_end (p);
  return t;
}

/* Returns an INTEGER_CST (of type `int') corresponding to I.
   Multiple calls with the same value of I may or may not yield the
   same node; therefore, callers should never modify the node
   returned.  */

static GTY(()) tree shared_int_cache[256];

tree
build_shared_int_cst (int i)
{
  if (i >= 256)
    return build_int_2 (i, 0);
  
  if (!shared_int_cache[i])
    shared_int_cache[i] = build_int_2 (i, 0);
  
  return shared_int_cache[i];
}

tree
get_type_decl (tree t)
{
  if (TREE_CODE (t) == TYPE_DECL)
    return t;
  if (TYPE_P (t))
    return TYPE_STUB_DECL (t);
  if (t == error_mark_node)
    return t;
  
  abort ();

  /* Stop compiler from complaining control reaches end of non-void function.  */
  return 0;
}

/* Return first vector element whose BINFO_TYPE is ELEM.
   Return 0 if ELEM is not in VEC.  VEC may be NULL_TREE.  */

tree
vec_binfo_member (tree elem, tree vec)
{
  int i;

  if (vec)
    for (i = 0; i < TREE_VEC_LENGTH (vec); ++i)
      if (same_type_p (elem, BINFO_TYPE (TREE_VEC_ELT (vec, i))))
	return TREE_VEC_ELT (vec, i);

  return NULL_TREE;
}

/* Returns the namespace that contains DECL, whether directly or
   indirectly.  */

tree
decl_namespace_context (tree decl)
{
  while (1)
    {
      if (TREE_CODE (decl) == NAMESPACE_DECL)
	return decl;
      else if (TYPE_P (decl))
	decl = CP_DECL_CONTEXT (TYPE_MAIN_DECL (decl));
      else
	decl = CP_DECL_CONTEXT (decl);
    }
}

/* Return truthvalue of whether T1 is the same tree structure as T2.
   Return 1 if they are the same. Return 0 if they are different.  */

bool
cp_tree_equal (tree t1, tree t2)
{
  enum tree_code code1, code2;

  if (t1 == t2)
    return true;
  if (!t1 || !t2)
    return false;

  for (code1 = TREE_CODE (t1);
       code1 == NOP_EXPR || code1 == CONVERT_EXPR
	 || code1 == NON_LVALUE_EXPR;
       code1 = TREE_CODE (t1))
    t1 = TREE_OPERAND (t1, 0);
  for (code2 = TREE_CODE (t2);
       code2 == NOP_EXPR || code2 == CONVERT_EXPR
	 || code1 == NON_LVALUE_EXPR;
       code2 = TREE_CODE (t2))
    t2 = TREE_OPERAND (t2, 0);

  /* They might have become equal now.  */
  if (t1 == t2)
    return true;
  
  if (code1 != code2)
    return false;

  switch (code1)
    {
    case INTEGER_CST:
      return TREE_INT_CST_LOW (t1) == TREE_INT_CST_LOW (t2)
	&& TREE_INT_CST_HIGH (t1) == TREE_INT_CST_HIGH (t2);

    case REAL_CST:
      return REAL_VALUES_EQUAL (TREE_REAL_CST (t1), TREE_REAL_CST (t2));

    case STRING_CST:
      return TREE_STRING_LENGTH (t1) == TREE_STRING_LENGTH (t2)
	&& !memcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
		    TREE_STRING_LENGTH (t1));

    case CONSTRUCTOR:
      /* We need to do this when determining whether or not two
	 non-type pointer to member function template arguments
	 are the same.  */
      if (!(same_type_p (TREE_TYPE (t1), TREE_TYPE (t2))
	    /* The first operand is RTL.  */
	    && TREE_OPERAND (t1, 0) == TREE_OPERAND (t2, 0)))
	return false;
      return cp_tree_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case TREE_LIST:
      if (!cp_tree_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2)))
	return false;
      if (!cp_tree_equal (TREE_VALUE (t1), TREE_VALUE (t2)))
	return false;
      return cp_tree_equal (TREE_CHAIN (t1), TREE_CHAIN (t2));

    case SAVE_EXPR:
      return cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    case CALL_EXPR:
      if (!cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0)))
	return false;
      return cp_tree_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case TARGET_EXPR:
      {
	tree o1 = TREE_OPERAND (t1, 0);
	tree o2 = TREE_OPERAND (t2, 0);
	
	/* Special case: if either target is an unallocated VAR_DECL,
	   it means that it's going to be unified with whatever the
	   TARGET_EXPR is really supposed to initialize, so treat it
	   as being equivalent to anything.  */
	if (TREE_CODE (o1) == VAR_DECL && DECL_NAME (o1) == NULL_TREE
	    && !DECL_RTL_SET_P (o1))
	  /*Nop*/;
	else if (TREE_CODE (o2) == VAR_DECL && DECL_NAME (o2) == NULL_TREE
		 && !DECL_RTL_SET_P (o2))
	  /*Nop*/;
	else if (!cp_tree_equal (o1, o2))
	  return false;
      
	return cp_tree_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));
      }
      
    case WITH_CLEANUP_EXPR:
      if (!cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0)))
	return false;
      return cp_tree_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t1, 1));

    case COMPONENT_REF:
      if (TREE_OPERAND (t1, 1) != TREE_OPERAND (t2, 1))
	return false;
      return cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
    case TEMPLATE_DECL:
    case IDENTIFIER_NODE:
      return false;

    case BASELINK:
      return (BASELINK_BINFO (t1) == BASELINK_BINFO (t2)
	      && BASELINK_ACCESS_BINFO (t1) == BASELINK_ACCESS_BINFO (t2)
	      && cp_tree_equal (BASELINK_FUNCTIONS (t1),
				BASELINK_FUNCTIONS (t2)));

    case TEMPLATE_PARM_INDEX:
      return (TEMPLATE_PARM_IDX (t1) == TEMPLATE_PARM_IDX (t2)
	      && TEMPLATE_PARM_LEVEL (t1) == TEMPLATE_PARM_LEVEL (t2)
	      && same_type_p (TREE_TYPE (TEMPLATE_PARM_DECL (t1)),
			      TREE_TYPE (TEMPLATE_PARM_DECL (t2))));

    case TEMPLATE_ID_EXPR:
      {
	unsigned ix;
	tree vec1, vec2;
	
	if (!cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0)))
	  return false;
	vec1 = TREE_OPERAND (t1, 1);
	vec2 = TREE_OPERAND (t2, 1);

	if (!vec1 || !vec2)
	  return !vec1 && !vec2;
	
	if (TREE_VEC_LENGTH (vec1) != TREE_VEC_LENGTH (vec2))
	  return false;

	for (ix = TREE_VEC_LENGTH (vec1); ix--;)
	  if (!cp_tree_equal (TREE_VEC_ELT (vec1, ix),
			      TREE_VEC_ELT (vec2, ix)))
	    return false;
	
	return true;
      }
      
    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
      {
	tree o1 = TREE_OPERAND (t1, 0);
	tree o2 = TREE_OPERAND (t2, 0);
	
	if (TREE_CODE (o1) != TREE_CODE (o2))
	  return false;
	if (TYPE_P (o1))
	  return same_type_p (o1, o2);
	else
	  return cp_tree_equal (o1, o2);
      }
      
    case PTRMEM_CST:
      /* Two pointer-to-members are the same if they point to the same
	 field or function in the same class.  */
      if (PTRMEM_CST_MEMBER (t1) != PTRMEM_CST_MEMBER (t2))
	return false;

      return same_type_p (PTRMEM_CST_CLASS (t1), PTRMEM_CST_CLASS (t2));

    case OVERLOAD:
      if (OVL_FUNCTION (t1) != OVL_FUNCTION (t2))
	return false;
      return cp_tree_equal (OVL_CHAIN (t1), OVL_CHAIN (t2));

    default:
      break;
    }

  switch (TREE_CODE_CLASS (code1))
    {
    case '1':
    case '2':
    case '<':
    case 'e':
    case 'r':
    case 's':
      {
	int i;
	
	for (i = 0; i < TREE_CODE_LENGTH (code1); ++i)
	  if (!cp_tree_equal (TREE_OPERAND (t1, i), TREE_OPERAND (t2, i)))
	    return false;
	
	return true;
      }
    
    case 't':
      return same_type_p (t1, t2);
    }

  my_friendly_assert (0, 20030617);
  return false;
}

/* Build a wrapper around a 'struct z_candidate' so we can use it as a
   tree.  */

tree
build_zc_wrapper (struct z_candidate* ptr)
{
  tree t = make_node (WRAPPER);
  WRAPPER_ZC (t) = ptr;
  return t;
}

/* The type of ARG when used as an lvalue.  */

tree
lvalue_type (tree arg)
{
  tree type = TREE_TYPE (arg);
  return type;
}

/* The type of ARG for printing error messages; denote lvalues with
   reference types.  */

tree
error_type (tree arg)
{
  tree type = TREE_TYPE (arg);
  
  if (TREE_CODE (type) == ARRAY_TYPE)
    ;
  else if (TREE_CODE (type) == ERROR_MARK)
    ;
  else if (real_lvalue_p (arg))
    type = build_reference_type (lvalue_type (arg));
  else if (IS_AGGR_TYPE (type))
    type = lvalue_type (arg);

  return type;
}

/* Does FUNCTION use a variable-length argument list?  */

int
varargs_function_p (tree function)
{
  tree parm = TYPE_ARG_TYPES (TREE_TYPE (function));
  for (; parm; parm = TREE_CHAIN (parm))
    if (TREE_VALUE (parm) == void_type_node)
      return 0;
  return 1;
}

/* Returns 1 if decl is a member of a class.  */

int
member_p (tree decl)
{
  const tree ctx = DECL_CONTEXT (decl);
  return (ctx && TYPE_P (ctx));
}

/* Create a placeholder for member access where we don't actually have an
   object that the access is against.  */

tree
build_dummy_object (tree type)
{
  tree decl = build1 (NOP_EXPR, build_pointer_type (type), void_zero_node);
  return build_indirect_ref (decl, NULL);
}

/* We've gotten a reference to a member of TYPE.  Return *this if appropriate,
   or a dummy object otherwise.  If BINFOP is non-0, it is filled with the
   binfo path from current_class_type to TYPE, or 0.  */

tree
maybe_dummy_object (tree type, tree* binfop)
{
  tree decl, context;
  tree binfo;
  
  if (current_class_type
      && (binfo = lookup_base (current_class_type, type,
			       ba_ignore | ba_quiet, NULL)))
    context = current_class_type;
  else
    {
      /* Reference from a nested class member function.  */
      context = type;
      binfo = TYPE_BINFO (type);
    }

  if (binfop)
    *binfop = binfo;
  
  if (current_class_ref && context == current_class_type
      /* Kludge: Make sure that current_class_type is actually
         correct.  It might not be if we're in the middle of
         tsubst_default_argument.  */
      && same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (current_class_ref)),
		      current_class_type))
    decl = current_class_ref;
  else
    decl = build_dummy_object (context);

  return decl;
}

/* Returns 1 if OB is a placeholder object, or a pointer to one.  */

int
is_dummy_object (tree ob)
{
  if (TREE_CODE (ob) == INDIRECT_REF)
    ob = TREE_OPERAND (ob, 0);
  return (TREE_CODE (ob) == NOP_EXPR
	  && TREE_OPERAND (ob, 0) == void_zero_node);
}

/* Returns 1 iff type T is a POD type, as defined in [basic.types].  */

int
pod_type_p (tree t)
{
  t = strip_array_types (t);

  if (t == error_mark_node)
    return 1;
  if (INTEGRAL_TYPE_P (t))
    return 1;  /* integral, character or enumeral type */
  if (FLOAT_TYPE_P (t))
    return 1;
  if (TYPE_PTR_P (t))
    return 1; /* pointer to non-member */
  if (TYPE_PTR_TO_MEMBER_P (t))
    return 1; /* pointer to member */
  if (TREE_CODE (t) == VECTOR_TYPE)
    return 1; /* vectors are (small) arrays of scalars */
  
  if (! CLASS_TYPE_P (t))
    return 0; /* other non-class type (reference or function) */
  if (CLASSTYPE_NON_POD_P (t))
    return 0;
  return 1;
}

/* Returns 1 iff zero initialization of type T means actually storing
   zeros in it.  */

int
zero_init_p (tree t)
{
  t = strip_array_types (t);

  if (t == error_mark_node)
    return 1;

  /* NULL pointers to data members are initialized with -1.  */
  if (TYPE_PTRMEM_P (t))
    return 0;

  /* Classes that contain types that can't be zero-initialized, cannot
     be zero-initialized themselves.  */
  if (CLASS_TYPE_P (t) && CLASSTYPE_NON_ZERO_INIT_P (t))
    return 0;

  return 1;
}

/* Table of valid C++ attributes.  */
const struct attribute_spec cxx_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  { "java_interface", 0, 0, false, false, false, handle_java_interface_attribute },
  { "com_interface",  0, 0, false, false, false, handle_com_interface_attribute },
  { "init_priority",  1, 1, true,  false, false, handle_init_priority_attribute },
  { NULL,             0, 0, false, false, false, NULL }
};

/* Handle a "java_interface" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
handle_java_interface_attribute (tree* node, 
                                 tree name, 
                                 tree args ATTRIBUTE_UNUSED , 
                                 int flags, 
                                 bool* no_add_attrs)
{
  if (DECL_P (*node)
      || !CLASS_TYPE_P (*node)
      || !TYPE_FOR_JAVA (*node))
    {
      error ("`%s' attribute can only be applied to Java class definitions",
	     IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
      return NULL_TREE;
    }
  if (!(flags & (int) ATTR_FLAG_TYPE_IN_PLACE))
    *node = build_type_copy (*node);
  TYPE_JAVA_INTERFACE (*node) = 1;

  return NULL_TREE;
}

/* Handle a "com_interface" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
handle_com_interface_attribute (tree* node, 
                                tree name, 
                                tree args ATTRIBUTE_UNUSED , 
                                int flags ATTRIBUTE_UNUSED , 
                                bool* no_add_attrs)
{
  static int warned;

  *no_add_attrs = true;

  if (DECL_P (*node)
      || !CLASS_TYPE_P (*node)
      || *node != TYPE_MAIN_VARIANT (*node))
    {
      warning ("`%s' attribute can only be applied to class definitions",
	       IDENTIFIER_POINTER (name));
      return NULL_TREE;
    }

  if (!warned++)
    warning ("`%s' is obsolete; g++ vtables are now COM-compatible by default",
	     IDENTIFIER_POINTER (name));

  return NULL_TREE;
}

/* Handle an "init_priority" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
handle_init_priority_attribute (tree* node, 
                                tree name, 
                                tree args, 
                                int flags ATTRIBUTE_UNUSED , 
                                bool* no_add_attrs)
{
  tree initp_expr = TREE_VALUE (args);
  tree decl = *node;
  tree type = TREE_TYPE (decl);
  int pri;

  STRIP_NOPS (initp_expr);
	  
  if (!initp_expr || TREE_CODE (initp_expr) != INTEGER_CST)
    {
      error ("requested init_priority is not an integer constant");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  pri = TREE_INT_CST_LOW (initp_expr);
	
  type = strip_array_types (type);

  if (decl == NULL_TREE
      || TREE_CODE (decl) != VAR_DECL
      || !TREE_STATIC (decl)
      || DECL_EXTERNAL (decl)
      || (TREE_CODE (type) != RECORD_TYPE
	  && TREE_CODE (type) != UNION_TYPE)
      /* Static objects in functions are initialized the
	 first time control passes through that
	 function. This is not precise enough to pin down an
	 init_priority value, so don't allow it.  */
      || current_function_decl) 
    {
      error ("can only use `%s' attribute on file-scope definitions of objects of class type",
	     IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
      return NULL_TREE;
    }

  if (pri > MAX_INIT_PRIORITY || pri <= 0)
    {
      error ("requested init_priority is out of range");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Check for init_priorities that are reserved for
     language and runtime support implementations.*/
  if (pri <= MAX_RESERVED_INIT_PRIORITY)
    {
      warning 
	("requested init_priority is reserved for internal use");
    }

  if (SUPPORTS_INIT_PRIORITY)
    {
      DECL_INIT_PRIORITY (decl) = pri;
      return NULL_TREE;
    }
  else
    {
      error ("`%s' attribute is not supported on this platform",
	     IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
      return NULL_TREE;
    }
}

/* Return a new PTRMEM_CST of the indicated TYPE.  The MEMBER is the
   thing pointed to by the constant.  */

tree
make_ptrmem_cst (tree type, tree member)
{
  tree ptrmem_cst = make_node (PTRMEM_CST);
  /* If would seem a great convenience if make_node would set
     TREE_CONSTANT for things of class `c', but it does not.  */
  TREE_CONSTANT (ptrmem_cst) = 1;
  TREE_TYPE (ptrmem_cst) = type;
  PTRMEM_CST_MEMBER (ptrmem_cst) = member;
  return ptrmem_cst;
}

/* Build a variant of TYPE that has the indicated ATTRIBUTES.  May
   return an existing type of an appropriate type already exists.  */

tree
cp_build_type_attribute_variant (tree type, tree attributes)
{
  tree new_type;

  new_type = build_type_attribute_variant (type, attributes);
  if (TREE_CODE (new_type) == FUNCTION_TYPE
      && (TYPE_RAISES_EXCEPTIONS (new_type) 
	  != TYPE_RAISES_EXCEPTIONS (type)))
    new_type = build_exception_variant (new_type,
					TYPE_RAISES_EXCEPTIONS (type));
  return new_type;
}

/* Apply FUNC to all language-specific sub-trees of TP in a pre-order
   traversal.  Called from walk_tree().  */

tree 
cp_walk_subtrees (tree* tp, 
                  int* walk_subtrees_p, 
                  walk_tree_fn func, 
                  void* data, 
                  void* htab)
{
  enum tree_code code = TREE_CODE (*tp);
  tree result;
  
#define WALK_SUBTREE(NODE)				\
  do							\
    {							\
      result = walk_tree (&(NODE), func, data, htab);	\
      if (result)					\
	return result;					\
    }							\
  while (0)

  /* Not one of the easy cases.  We must explicitly go through the
     children.  */
  switch (code)
    {
    case DEFAULT_ARG:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
    case UNBOUND_CLASS_TEMPLATE:
    case TEMPLATE_PARM_INDEX:
    case TEMPLATE_TYPE_PARM:
    case TYPENAME_TYPE:
    case TYPEOF_TYPE:
    case BASELINK:
      /* None of these have subtrees other than those already walked
         above.  */
      *walk_subtrees_p = 0;
      break;

    case PTRMEM_CST:
      WALK_SUBTREE (TREE_TYPE (*tp));
      *walk_subtrees_p = 0;
      break;

    case TREE_LIST:
      WALK_SUBTREE (TREE_PURPOSE (*tp));
      break;

    case OVERLOAD:
      WALK_SUBTREE (OVL_FUNCTION (*tp));
      WALK_SUBTREE (OVL_CHAIN (*tp));
      *walk_subtrees_p = 0;
      break;

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (*tp))
	WALK_SUBTREE (TYPE_PTRMEMFUNC_FN_TYPE (*tp));
      break;

    default:
      break;
    }

  /* We didn't find what we were looking for.  */
  return NULL_TREE;

#undef WALK_SUBTREE
}

/* Decide whether there are language-specific reasons to not inline a
   function as a tree.  */

int
cp_cannot_inline_tree_fn (tree* fnp)
{
  tree fn = *fnp;

  /* We can inline a template instantiation only if it's fully
     instantiated.  */
  if (DECL_TEMPLATE_INFO (fn)
      && TI_PENDING_TEMPLATE_FLAG (DECL_TEMPLATE_INFO (fn)))
    {
      /* Don't instantiate functions that are not going to be
	 inlined.  */
      if (!DECL_INLINE (DECL_TEMPLATE_RESULT 
			(template_for_substitution (fn))))
	return 1;

      fn = *fnp = instantiate_decl (fn, /*defer_ok=*/0);

      if (TI_PENDING_TEMPLATE_FLAG (DECL_TEMPLATE_INFO (fn)))
	return 1;
    }

  if (flag_really_no_inline
      && lookup_attribute ("always_inline", DECL_ATTRIBUTES (fn)) == NULL)
    return 1;

  /* Don't auto-inline anything that might not be bound within
     this unit of translation.  */
  if (!DECL_DECLARED_INLINE_P (fn) && !(*targetm.binds_local_p) (fn))
    {
      DECL_UNINLINABLE (fn) = 1;
      return 1;
    }

  if (varargs_function_p (fn))
    {
      DECL_UNINLINABLE (fn) = 1;
      return 1;
    }

  if (! function_attribute_inlinable_p (fn))
    {
      DECL_UNINLINABLE (fn) = 1;
      return 1;
    }

  return 0;
}

/* Add any pending functions other than the current function (already
   handled by the caller), that thus cannot be inlined, to FNS_P, then
   return the latest function added to the array, PREV_FN.  */

tree
cp_add_pending_fn_decls (void* fns_p, tree prev_fn)
{
  varray_type *fnsp = (varray_type *)fns_p;
  struct saved_scope *s;

  for (s = scope_chain; s; s = s->prev)
    if (s->function_decl && s->function_decl != prev_fn)
      {
	VARRAY_PUSH_TREE (*fnsp, s->function_decl);
	prev_fn = s->function_decl;
      }

  return prev_fn;
}

/* Determine whether a tree node is an OVERLOAD node.  Used to decide
   whether to copy a node or to preserve its chain when inlining a
   function.  */

int
cp_is_overload_p (tree t)
{
  return TREE_CODE (t) == OVERLOAD;
}

/* Determine whether VAR is a declaration of an automatic variable in
   function FN.  */

int
cp_auto_var_in_fn_p (tree var, tree fn)
{
  return (DECL_P (var) && DECL_CONTEXT (var) == fn
	  && nonstatic_local_decl_p (var));
}

/* Tell whether a declaration is needed for the RESULT of a function
   FN being inlined into CALLER or if the top node of target_exprs is
   to be used.  */

tree
cp_copy_res_decl_for_inlining (tree result, 
                               tree fn, 
                               tree caller, 
                               void* decl_map_,
                               int* need_decl, 
                               tree return_slot_addr)
{
  splay_tree decl_map = (splay_tree)decl_map_;
  tree var;

  /* If FN returns an aggregate then the caller will always pass the
     address of the return slot explicitly.  If we were just to
     create a new VAR_DECL here, then the result of this function
     would be copied (bitwise) into the variable initialized by the
     TARGET_EXPR.  That's incorrect, so we must transform any
     references to the RESULT into references to the target.  */

  /* We should have an explicit return slot iff the return type is
     TREE_ADDRESSABLE.  See simplify_aggr_init_expr.  */
  if (TREE_ADDRESSABLE (TREE_TYPE (result))
      != (return_slot_addr != NULL_TREE))
    abort ();

  *need_decl = !return_slot_addr;
  if (return_slot_addr)
    {
      var = build_indirect_ref (return_slot_addr, "");
      if (! same_type_ignoring_top_level_qualifiers_p (TREE_TYPE (var),
						       TREE_TYPE (result)))
	abort ();
    }
  /* Otherwise, make an appropriate copy.  */
  else
    var = copy_decl_for_inlining (result, fn, caller);

  if (DECL_SAVED_FUNCTION_DATA (fn))
    {
      tree nrv = DECL_SAVED_FUNCTION_DATA (fn)->x_return_value;
      if (nrv)
	{
	  /* We have a named return value; copy the name and source
	     position so we can get reasonable debugging information, and
	     register the return variable as its equivalent.  */
	  if (TREE_CODE (var) == VAR_DECL
	      /* But not if we're initializing a variable from the
		 enclosing function which already has its own name.  */
	      && DECL_NAME (var) == NULL_TREE)
	    {
	      DECL_NAME (var) = DECL_NAME (nrv);
	      DECL_SOURCE_LOCATION (var) = DECL_SOURCE_LOCATION (nrv);
	      DECL_ABSTRACT_ORIGIN (var) = DECL_ORIGIN (nrv);
	      /* Don't lose initialization info.  */
	      DECL_INITIAL (var) = DECL_INITIAL (nrv);
	      /* Don't forget that it needs to go in the stack.  */
	      TREE_ADDRESSABLE (var) = TREE_ADDRESSABLE (nrv);
	    }

	  splay_tree_insert (decl_map,
			     (splay_tree_key) nrv,
			     (splay_tree_value) var);
	}
    }

  return var;
}

/* Initialize tree.c.  */

void
init_tree (void)
{
  list_hash_table = htab_create_ggc (31, list_hash, list_hash_eq, NULL);
}

/* Called via walk_tree.  If *TP points to a DECL_STMT for a local
   declaration, copies the declaration and enters it in the splay_tree
   pointed to by DATA (which is really a `splay_tree *').  */

static tree
mark_local_for_remap_r (tree* tp, 
                        int* walk_subtrees ATTRIBUTE_UNUSED , 
                        void* data)
{
  tree t = *tp;
  splay_tree st = (splay_tree) data;
  tree decl;

  
  if (TREE_CODE (t) == DECL_STMT
      && nonstatic_local_decl_p (DECL_STMT_DECL (t)))
    decl = DECL_STMT_DECL (t);
  else if (TREE_CODE (t) == LABEL_STMT)
    decl = LABEL_STMT_LABEL (t);
  else if (TREE_CODE (t) == TARGET_EXPR
	   && nonstatic_local_decl_p (TREE_OPERAND (t, 0)))
    decl = TREE_OPERAND (t, 0);
  else if (TREE_CODE (t) == CASE_LABEL)
    decl = CASE_LABEL_DECL (t);
  else
    decl = NULL_TREE;

  if (decl)
    {
      tree copy;

      /* Make a copy.  */
      copy = copy_decl_for_inlining (decl, 
				     DECL_CONTEXT (decl), 
				     DECL_CONTEXT (decl));

      /* Remember the copy.  */
      splay_tree_insert (st,
			 (splay_tree_key) decl, 
			 (splay_tree_value) copy);
    }

  return NULL_TREE;
}

/* Called via walk_tree when an expression is unsaved.  Using the
   splay_tree pointed to by ST (which is really a `splay_tree'),
   remaps all local declarations to appropriate replacements.  */

static tree
cp_unsave_r (tree* tp, 
             int* walk_subtrees, 
             void* data)
{
  splay_tree st = (splay_tree) data;
  splay_tree_node n;

  /* Only a local declaration (variable or label).  */
  if (nonstatic_local_decl_p (*tp))
    {
      /* Lookup the declaration.  */
      n = splay_tree_lookup (st, (splay_tree_key) *tp);
      
      /* If it's there, remap it.  */
      if (n)
	*tp = (tree) n->value;
    }
  else if (TREE_CODE (*tp) == SAVE_EXPR)
    remap_save_expr (tp, st, current_function_decl, walk_subtrees);
  else
    {
      copy_tree_r (tp, walk_subtrees, NULL);

      /* Do whatever unsaving is required.  */
      unsave_expr_1 (*tp);
    }

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Called whenever an expression needs to be unsaved.  */

tree
cxx_unsave_expr_now (tree tp)
{
  splay_tree st;

  /* Create a splay-tree to map old local variable declarations to new
     ones.  */
  st = splay_tree_new (splay_tree_compare_pointers, NULL, NULL);

  /* Walk the tree once figuring out what needs to be remapped.  */
  walk_tree (&tp, mark_local_for_remap_r, st, NULL);

  /* Walk the tree again, copying, remapping, and unsaving.  */
  walk_tree (&tp, cp_unsave_r, st, NULL);

  /* Clean up.  */
  splay_tree_delete (st);

  return tp;
}

/* Returns the kind of special function that DECL (a FUNCTION_DECL)
   is.  Note that sfk_none is zero, so this function can be used as a
   predicate to test whether or not DECL is a special function.  */

special_function_kind
special_function_p (tree decl)
{
  /* Rather than doing all this stuff with magic names, we should
     probably have a field of type `special_function_kind' in
     DECL_LANG_SPECIFIC.  */
  if (DECL_COPY_CONSTRUCTOR_P (decl))
    return sfk_copy_constructor;
  if (DECL_CONSTRUCTOR_P (decl))
    return sfk_constructor;
  if (DECL_OVERLOADED_OPERATOR_P (decl) == NOP_EXPR)
    return sfk_assignment_operator;
  if (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (decl))
    return sfk_destructor;
  if (DECL_COMPLETE_DESTRUCTOR_P (decl))
    return sfk_complete_destructor;
  if (DECL_BASE_DESTRUCTOR_P (decl))
    return sfk_base_destructor;
  if (DECL_DELETING_DESTRUCTOR_P (decl))
    return sfk_deleting_destructor;
  if (DECL_CONV_FN_P (decl))
    return sfk_conversion;

  return sfk_none;
}

/* Returns true if and only if NODE is a name, i.e., a node created
   by the parser when processing an id-expression.  */

bool
name_p (tree node)
{
  if (TREE_CODE (node) == TEMPLATE_ID_EXPR)
    node = TREE_OPERAND (node, 0);
  return (/* An ordinary unqualified name.  */
	  TREE_CODE (node) == IDENTIFIER_NODE
	  /* A destructor name.  */
	  || TREE_CODE (node) == BIT_NOT_EXPR
	  /* A qualified name.  */
	  || TREE_CODE (node) == SCOPE_REF);
}

/* Returns nonzero if TYPE is a character type, including wchar_t.  */

int
char_type_p (tree type)
{
  return (same_type_p (type, char_type_node)
	  || same_type_p (type, unsigned_char_type_node)
	  || same_type_p (type, signed_char_type_node)
	  || same_type_p (type, wchar_type_node));
}

/* Returns the kind of linkage associated with the indicated DECL.  Th
   value returned is as specified by the language standard; it is
   independent of implementation details regarding template
   instantiation, etc.  For example, it is possible that a declaration
   to which this function assigns external linkage would not show up
   as a global symbol when you run `nm' on the resulting object file.  */

linkage_kind
decl_linkage (tree decl)
{
  /* This function doesn't attempt to calculate the linkage from first
     principles as given in [basic.link].  Instead, it makes use of
     the fact that we have already set TREE_PUBLIC appropriately, and
     then handles a few special cases.  Ideally, we would calculate
     linkage first, and then transform that into a concrete
     implementation.  */

  /* Things that don't have names have no linkage.  */
  if (!DECL_NAME (decl))
    return lk_none;

  /* Things that are TREE_PUBLIC have external linkage.  */
  if (TREE_PUBLIC (decl))
    return lk_external;

  /* Some things that are not TREE_PUBLIC have external linkage, too.
     For example, on targets that don't have weak symbols, we make all
     template instantiations have internal linkage (in the object
     file), but the symbols should still be treated as having external
     linkage from the point of view of the language.  */
  if (DECL_LANG_SPECIFIC (decl) && DECL_COMDAT (decl))
    return lk_external;

  /* Things in local scope do not have linkage, if they don't have
     TREE_PUBLIC set.  */
  if (decl_function_context (decl))
    return lk_none;

  /* Everything else has internal linkage.  */
  return lk_internal;
}

/* EXP is an expression that we want to pre-evaluate.  Returns via INITP an
   expression to perform the pre-evaluation, and returns directly an
   expression to use the precalculated result.  */

tree
stabilize_expr (tree exp, tree* initp)
{
  tree init_expr;

  if (!TREE_SIDE_EFFECTS (exp))
    {
      init_expr = NULL_TREE;
    }
  else if (!real_lvalue_p (exp)
	   || !TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (exp)))
    {
      init_expr = get_target_expr (exp);
      exp = TARGET_EXPR_SLOT (init_expr);
    }
  else
    {
      exp = build_unary_op (ADDR_EXPR, exp, 1);
      init_expr = get_target_expr (exp);
      exp = TARGET_EXPR_SLOT (init_expr);
      exp = build_indirect_ref (exp, 0);
    }

  *initp = init_expr;
  return exp;
}

/* Like stabilize_expr, but for a call whose args we want to
   pre-evaluate.  */

void
stabilize_call (tree call, tree *initp)
{
  tree inits = NULL_TREE;
  tree t;

  if (call == error_mark_node)
    return;

  if (TREE_CODE (call) != CALL_EXPR
      && TREE_CODE (call) != AGGR_INIT_EXPR)
    abort ();

  for (t = TREE_OPERAND (call, 1); t; t = TREE_CHAIN (t))
    if (TREE_SIDE_EFFECTS (TREE_VALUE (t)))
      {
	tree init;
	TREE_VALUE (t) = stabilize_expr (TREE_VALUE (t), &init);
	if (!init)
	  /* Nothing.  */;
	else if (inits)
	  inits = build (COMPOUND_EXPR, void_type_node, inits, init);
	else
	  inits = init;
      }

  *initp = inits;
}

/* Like stabilize_expr, but for an initialization.  If we are initializing
   an object of class type, we don't want to introduce an extra temporary,
   so we look past the TARGET_EXPR and stabilize the arguments of the call
   instead.  */

bool
stabilize_init (tree init, tree *initp)
{
  tree t = init;

  if (t == error_mark_node)
    return true;

  if (TREE_CODE (t) == INIT_EXPR
      && TREE_CODE (TREE_OPERAND (t, 1)) != TARGET_EXPR)
    TREE_OPERAND (t, 1) = stabilize_expr (TREE_OPERAND (t, 1), initp);
  else
    {
      if (TREE_CODE (t) == INIT_EXPR)
	t = TREE_OPERAND (t, 1);
      if (TREE_CODE (t) == TARGET_EXPR)
	t = TARGET_EXPR_INITIAL (t);
      if (TREE_CODE (t) == COMPOUND_EXPR)
	t = expr_last (t);
      if (TREE_CODE (t) == CONSTRUCTOR
	  && CONSTRUCTOR_ELTS (t) == NULL_TREE)
	{
	  /* Default-initialization.  */
	  *initp = NULL_TREE;
	  return true;
	}

      /* If the initializer is a COND_EXPR, we can't preevaluate
	 anything.  */
      if (TREE_CODE (t) == COND_EXPR)
	return false;

      /* The TARGET_EXPR might be initializing via bitwise copy from
	 another variable; leave that alone.  */
      if (TREE_SIDE_EFFECTS (t))
	stabilize_call (t, initp);
    }

  return true;
}

/* Like "fold", but should be used whenever we might be processing the
   body of a template.  */

tree
fold_if_not_in_template (tree expr)
{
  /* In the body of a template, there is never any need to call
     "fold".  We will call fold later when actually instantiating the
     template.  Integral constant expressions in templates will be
     evaluated via fold_non_dependent_expr, as necessary.  */
  return (processing_template_decl ? expr : fold (expr));
}


#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)
/* Complain that some language-specific thing hanging off a tree
   node has been accessed improperly.  */

void
lang_check_failed (const char* file, int line, const char* function)
{
  internal_error ("lang_* check: failed in %s, at %s:%d",
		  function, trim_filename (file), line);
}
#endif /* ENABLE_TREE_CHECKING */

#include "gt-cp-tree.h"
