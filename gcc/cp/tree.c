/* Language-dependent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987, 88, 92-98, 1999 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "obstack.h"
#include "tree.h"
#include "cp-tree.h"
#include "flags.h"
#include "rtl.h"
#include "toplev.h"
#include "ggc.h"
#include "insn-config.h"
#include "integrate.h"

static tree bot_manip PROTO((tree *, int *, void *));
static tree bot_replace PROTO((tree *, int *, void *));
static tree build_cplus_array_type_1 PROTO((tree, tree));
static void list_hash_add PROTO((int, tree));
static int list_hash PROTO((tree, tree, tree));
static tree list_hash_lookup PROTO((int, tree, tree, tree));
static void propagate_binfo_offsets PROTO((tree, tree));
static cp_lvalue_kind lvalue_p_1 PROTO((tree, int));
static tree no_linkage_helper PROTO((tree *, int *, void *));
static tree build_srcloc PROTO((char *, int));
static void mark_list_hash PROTO ((void *));
static int statement_code_p PROTO((enum tree_code));
static tree mark_local_for_remap_r PROTO((tree *, int *, void *));
static tree cp_unsave_r PROTO ((tree *, int *, void *));
static void cp_unsave PROTO((tree *));
static tree build_target_expr PROTO((tree, tree));

#define CEIL(x,y) (((x) + (y) - 1) / (y))

/* If REF is an lvalue, returns the kind of lvalue that REF is.
   Otherwise, returns clk_none.  If TREAT_CLASS_RVALUES_AS_LVALUES is
   non-zero, rvalues of class type are considered lvalues.  */

static cp_lvalue_kind
lvalue_p_1 (ref, treat_class_rvalues_as_lvalues)
     tree ref;
     int treat_class_rvalues_as_lvalues;
{
  cp_lvalue_kind op1_lvalue_kind = clk_none;
  cp_lvalue_kind op2_lvalue_kind = clk_none;

  if (TREE_CODE (TREE_TYPE (ref)) == REFERENCE_TYPE)
    return clk_ordinary;

  if (ref == current_class_ptr && flag_this_is_variable <= 0)
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
    case NOP_EXPR:
      return lvalue_p_1 (TREE_OPERAND (ref, 0),
			 treat_class_rvalues_as_lvalues);

    case COMPONENT_REF:
      op1_lvalue_kind = lvalue_p_1 (TREE_OPERAND (ref, 0),
				    treat_class_rvalues_as_lvalues);
      if (op1_lvalue_kind 
	  /* The "field" can be a FUNCTION_DECL or an OVERLOAD in some
	     situations.  */
	  && TREE_CODE (TREE_OPERAND (ref, 1)) == FIELD_DECL
	  && DECL_C_BIT_FIELD (TREE_OPERAND (ref, 1)))
	{
	  /* Clear the ordinary bit.  If this object was a class
	     rvalue we want to preserve that information.  */
	  op1_lvalue_kind &= ~clk_ordinary;
	  /* The lvalue is for a btifield.  */
	  op1_lvalue_kind |= clk_bitfield;
	}
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
      my_friendly_abort (103);
    case OFFSET_REF:
      if (TREE_CODE (TREE_OPERAND (ref, 1)) == FUNCTION_DECL)
	return clk_ordinary;
      /* Fall through.  */
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
      return ((treat_class_rvalues_as_lvalues
	       && IS_AGGR_TYPE (TREE_TYPE (ref)))
	      ? clk_class : clk_none);

    case FUNCTION_DECL:
      /* All functions (except non-static-member functions) are
	 lvalues.  */
      return (DECL_NONSTATIC_MEMBER_FUNCTION_P (ref) 
	      ? clk_none : clk_ordinary);

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

/* If REF is an lvalue, returns the kind of lvalue that REF is.
   Otherwise, returns clk_none.  Lvalues can be assigned, unless they
   have TREE_READONLY, or unless they are FUNCTION_DECLs.  Lvalues can
   have their address taken, unless they have DECL_REGISTER.  */

cp_lvalue_kind
real_lvalue_p (ref)
     tree ref;
{
  return lvalue_p_1 (ref, /*treat_class_rvalues_as_lvalues=*/0);
}

/* This differs from real_lvalue_p in that class rvalues are
   considered lvalues.  */

int
lvalue_p (ref)
     tree ref;
{
  return 
    (lvalue_p_1 (ref, /*treat_class_rvalues_as_lvalues=*/1) != clk_none);
}

/* Return nonzero if REF is an lvalue valid for this language;
   otherwise, print an error message and return zero.  */

int
lvalue_or_else (ref, string)
     tree ref;
     const char *string;
{
  int win = lvalue_p (ref);
  if (! win)
    error ("non-lvalue in %s", string);
  return win;
}

/* Build a TARGET_EXPR, initializing the DECL with the VALUE.  */

static tree
build_target_expr (decl, value)
     tree decl;
     tree value;
{
  tree t;

  t = build (TARGET_EXPR, TREE_TYPE (decl), decl, value, 
	     maybe_build_cleanup (decl), NULL_TREE);
  /* We always set TREE_SIDE_EFFECTS so that expand_expr does not
     ignore the TARGET_EXPR.  If there really turn out to be no
     side-effects, then the optimizer should be able to get rid of
     whatever code is generated anyhow.  */
  TREE_SIDE_EFFECTS (t) = 1;

  return t;
}

/* INIT is a CALL_EXPR which needs info about its target.
   TYPE is the type that this initialization should appear to have.

   Build an encapsulation of the initialization to perform
   and return it so that it can be processed by language-independent
   and language-specific expression expanders.  */

tree
build_cplus_new (type, init)
     tree type;
     tree init;
{
  tree fn;
  tree slot;
  tree rval;

  /* Make sure that we're not trying to create an instance of an
     abstract class.  */
  abstract_virtuals_error (NULL_TREE, type);

  if (TREE_CODE (init) != CALL_EXPR && TREE_CODE (init) != AGGR_INIT_EXPR)
    return convert (type, init);

  slot = build (VAR_DECL, type);
  DECL_ARTIFICIAL (slot) = 1;
  DECL_CONTEXT (slot) = current_function_decl;
  layout_decl (slot, 0);

  /* We split the CALL_EXPR into its function and its arguments here.
     Then, in expand_expr, we put them back together.  The reason for
     this is that this expression might be a default argument
     expression.  In that case, we need a new temporary every time the
     expression is used.  That's what break_out_target_exprs does; it
     replaces every AGGR_INIT_EXPR with a copy that uses a fresh
     temporary slot.  Then, expand_expr builds up a call-expression
     using the new slot.  */
  fn = TREE_OPERAND (init, 0);
  rval = build (AGGR_INIT_EXPR, type, fn, TREE_OPERAND (init, 1), slot);
  TREE_SIDE_EFFECTS (rval) = 1;
  AGGR_INIT_VIA_CTOR_P (rval) 
    = (TREE_CODE (fn) == ADDR_EXPR
       && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
       && DECL_CONSTRUCTOR_P (TREE_OPERAND (fn, 0)));
  rval = build_target_expr (slot, rval);

  return rval;
}

/* Buidl a TARGET_EXPR using INIT to initialize a new temporary of the
   indicated TYPE.  */

tree
build_target_expr_with_type (init, type)
     tree init;
     tree type;
{
  tree slot;
  tree rval;

  slot = build (VAR_DECL, type);
  DECL_ARTIFICIAL (slot) = 1;
  DECL_CONTEXT (slot) = current_function_decl;
  layout_decl (slot, 0);
  rval = build_target_expr (slot, init);

  return rval;
}

/* Like build_target_expr_with_type, but use the type of INIT.  */

tree
get_target_expr (init)
     tree init;
{
  return build_target_expr_with_type (init, TREE_TYPE (init));
}

/* Recursively search EXP for CALL_EXPRs that need cleanups and replace
   these CALL_EXPRs with tree nodes that will perform the cleanups.  */

tree
break_out_cleanups (exp)
     tree exp;
{
  tree tmp = exp;

  if (TREE_CODE (tmp) == CALL_EXPR
      && TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (tmp)))
    return build_cplus_new (TREE_TYPE (tmp), tmp);

  while (TREE_CODE (tmp) == NOP_EXPR
	 || TREE_CODE (tmp) == CONVERT_EXPR
	 || TREE_CODE (tmp) == NON_LVALUE_EXPR)
    {
      if (TREE_CODE (TREE_OPERAND (tmp, 0)) == CALL_EXPR
	  && TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (TREE_OPERAND (tmp, 0))))
	{
	  TREE_OPERAND (tmp, 0)
	    = build_cplus_new (TREE_TYPE (TREE_OPERAND (tmp, 0)),
			       TREE_OPERAND (tmp, 0));
	  break;
	}
      else
	tmp = TREE_OPERAND (tmp, 0);
    }
  return exp;
}

/* Recursively perform a preorder search EXP for CALL_EXPRs, making
   copies where they are found.  Returns a deep copy all nodes transitively
   containing CALL_EXPRs.  */

tree
break_out_calls (exp)
     tree exp;
{
  register tree t1, t2 = NULL_TREE;
  register enum tree_code code;
  register int changed = 0;
  register int i;

  if (exp == NULL_TREE)
    return exp;

  code = TREE_CODE (exp);

  if (code == CALL_EXPR)
    return copy_node (exp);

  /* Don't try and defeat a save_expr, as it should only be done once.  */
    if (code == SAVE_EXPR)
       return exp;

  switch (TREE_CODE_CLASS (code))
    {
    default:
      abort ();

    case 'c':  /* a constant */
    case 't':  /* a type node */
    case 'x':  /* something random, like an identifier or an ERROR_MARK.  */
      return exp;

    case 'd':  /* A decl node */
#if 0                               /* This is bogus.  jason 9/21/94 */

      t1 = break_out_calls (DECL_INITIAL (exp));
      if (t1 != DECL_INITIAL (exp))
	{
	  exp = copy_node (exp);
	  DECL_INITIAL (exp) = t1;
	}
#endif
      return exp;

    case 'b':  /* A block node */
      {
	/* Don't know how to handle these correctly yet.   Must do a
	   break_out_calls on all DECL_INITIAL values for local variables,
	   and also break_out_calls on all sub-blocks and sub-statements.  */
	abort ();
      }
      return exp;

    case 'e':  /* an expression */
    case 'r':  /* a reference */
    case 's':  /* an expression with side effects */
      for (i = tree_code_length[(int) code] - 1; i >= 0; i--)
	{
	  t1 = break_out_calls (TREE_OPERAND (exp, i));
	  if (t1 != TREE_OPERAND (exp, i))
	    {
	      exp = copy_node (exp);
	      TREE_OPERAND (exp, i) = t1;
	    }
	}
      return exp;

    case '<':  /* a comparison expression */
    case '2':  /* a binary arithmetic expression */
      t2 = break_out_calls (TREE_OPERAND (exp, 1));
      if (t2 != TREE_OPERAND (exp, 1))
	changed = 1;
    case '1':  /* a unary arithmetic expression */
      t1 = break_out_calls (TREE_OPERAND (exp, 0));
      if (t1 != TREE_OPERAND (exp, 0))
	changed = 1;
      if (changed)
	{
	  if (tree_code_length[(int) code] == 1)
	    return build1 (code, TREE_TYPE (exp), t1);
	  else
	    return build (code, TREE_TYPE (exp), t1, t2);
	}
      return exp;
    }

}

extern struct obstack permanent_obstack;

/* Here is how primitive or already-canonicalized types' hash
   codes are made.  MUST BE CONSISTENT WITH tree.c !!! */
#define TYPE_HASH(TYPE) ((HOST_WIDE_INT) (TYPE) & 0777777)

/* Construct, lay out and return the type of methods belonging to class
   BASETYPE and whose arguments are described by ARGTYPES and whose values
   are described by RETTYPE.  If each type exists already, reuse it.  */

tree
build_cplus_method_type (basetype, rettype, argtypes)
     tree basetype, rettype, argtypes;
{
  register tree t;
  tree ptype;
  int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (METHOD_TYPE);

  TYPE_METHOD_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = rettype;
  ptype = build_pointer_type (basetype);

  /* The actual arglist for this function includes a "hidden" argument
     which is "this".  Put it into the list of argument types.  Make
     sure that the new argument list is allocated on the same obstack
     as the type.  */
  argtypes = tree_cons (NULL_TREE, ptype, argtypes);
  TYPE_ARG_TYPES (t) = argtypes;
  TREE_SIDE_EFFECTS (argtypes) = 1;  /* Mark first argtype as "artificial".  */

  /* If we already have such a type, use the old one and free this one.
     Note that it also frees up the above cons cell if found.  */
  hashcode = TYPE_HASH (basetype) + TYPE_HASH (rettype) +
    type_hash_list (argtypes);

  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);

  return t;
}

static tree
build_cplus_array_type_1 (elt_type, index_type)
     tree elt_type;
     tree index_type;
{
  tree t;

  if (elt_type == error_mark_node || index_type == error_mark_node)
    return error_mark_node;

  if (processing_template_decl 
      || uses_template_parms (elt_type) 
      || uses_template_parms (index_type))
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
  TYPE_NEEDS_DESTRUCTOR (t) 
    = TYPE_NEEDS_DESTRUCTOR (TYPE_MAIN_VARIANT (elt_type));
  return t;
}

tree
build_cplus_array_type (elt_type, index_type)
     tree elt_type;
     tree index_type;
{
  tree t;
  int type_quals = CP_TYPE_QUALS (elt_type);

  elt_type = TYPE_MAIN_VARIANT (elt_type);

  t = build_cplus_array_type_1 (elt_type, index_type);

  if (type_quals != TYPE_UNQUALIFIED)
    t = cp_build_qualified_type (t, type_quals);

  return t;
}

/* Make a variant of TYPE, qualified with the TYPE_QUALS.  Handles
   arrays correctly.  In particular, if TYPE is an array of T's, and
   TYPE_QUALS is non-empty, returns an array of qualified T's.  If
   at attempt is made to qualify a type illegally, and COMPLAIN is
   non-zero, an error is issued.  If COMPLAIN is zero, error_mark_node
   is returned.  */

tree
cp_build_qualified_type_real (type, type_quals, complain)
     tree type;
     int type_quals;
     int complain;
{
  tree result;

  if (type == error_mark_node)
    return type;

  if (type_quals == TYPE_QUALS (type))
    return type;

  /* A restrict-qualified pointer type must be a pointer (or reference)
     to object or incomplete type.  */
  if ((type_quals & TYPE_QUAL_RESTRICT)
      && TREE_CODE (type) != TEMPLATE_TYPE_PARM
      && (!POINTER_TYPE_P (type)
	  || TYPE_PTRMEM_P (type)
	  || TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE))
    {
      if (complain)
	cp_error ("`%T' cannot be `restrict'-qualified", type);
      else
	return error_mark_node;

      type_quals &= ~TYPE_QUAL_RESTRICT;
    }

  if (type_quals != TYPE_UNQUALIFIED
      && TREE_CODE (type) == FUNCTION_TYPE)
    {
      if (complain)
	cp_error ("`%T' cannot be `const'-, `volatile'-, or `restrict'-qualified", type);
      else
	return error_mark_node;
      type_quals = TYPE_UNQUALIFIED;
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
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
	if (CP_TYPE_QUALS (t) == type_quals)
	  break;

      /* If we didn't already have it, create it now.  */
      if (!t)
	{
	  /* Make a new array type, just like the old one, but with the
	     appropriately qualified element type.  */
	  t = build_type_copy (type);
	  TREE_TYPE (t) = element_type;
	}

      /* Even if we already had this variant, we update
	 TYPE_NEEDS_CONSTRUCTING and TYPE_NEEDS_DESTRUCTOR in case
	 they changed since the variant was originally created.  
	 
	 This seems hokey; if there is some way to use a previous
	 variant *without* coming through here,
	 TYPE_NEEDS_CONSTRUCTING will never be updated.  */
      TYPE_NEEDS_CONSTRUCTING (t) 
	= TYPE_NEEDS_CONSTRUCTING (TYPE_MAIN_VARIANT (element_type));
      TYPE_NEEDS_DESTRUCTOR (t) 
	= TYPE_NEEDS_DESTRUCTOR (TYPE_MAIN_VARIANT (element_type));
      return t;
    }
  else if (TYPE_PTRMEMFUNC_P (type))
    {
      /* For a pointer-to-member type, we can't just return a
	 cv-qualified version of the RECORD_TYPE.  If we do, we
	 haven't change the field that contains the actual pointer to
	 a method, and so TYPE_PTRMEMFUNC_FN_TYPE will be wrong.  */
      tree t;

      t = TYPE_PTRMEMFUNC_FN_TYPE (type);
      t = cp_build_qualified_type_real (t, type_quals, complain);
      return build_ptrmemfunc_type (t);
    }

  /* Retrieve (or create) the appropriately qualified variant.  */
  result = build_qualified_type (type, type_quals);

  /* If this was a pointer-to-method type, and we just made a copy,
     then we need to clear the cached associated
     pointer-to-member-function type; it is not valid for the new
     type.  */
  if (result != type 
      && TREE_CODE (type) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE)
    TYPE_SET_PTRMEMFUNC_TYPE (result, NULL_TREE);

  return result;
}

/* Returns the canonical version of TYPE.  In other words, if TYPE is
   a typedef, returns the underlying type.  The cv-qualification of
   the type returned matches the type input; they will always be
   compatible types.  */

tree
canonical_type_variant (t)
     tree t;
{
  return cp_build_qualified_type (TYPE_MAIN_VARIANT (t), CP_TYPE_QUALS (t));
}

/* Add OFFSET to all base types of T.

   OFFSET, which is a type offset, is number of bytes.

   Note that we don't have to worry about having two paths to the
   same base type, since this type owns its association list.  */

static void
propagate_binfo_offsets (binfo, offset)
     tree binfo;
     tree offset;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  if (flag_new_abi)
    {
      for (i = 0; i < n_baselinks; ++i)
	{
	  tree base_binfo;

	  /* Figure out which base we're looking at.  */
	  base_binfo = TREE_VEC_ELT (binfos, i);

	  /* Skip virtual bases.  Their BINFO_OFFSET doesn't matter
	     since they are always reached by using offsets looked up
	     at run-time.  */
	  if (TREE_VIA_VIRTUAL (base_binfo))
	    continue;

	  /* Whatever offset this class used to have in its immediate
	     derived class, it is now at OFFSET more bytes in its
	     final derived class, since the immediate derived class is
	     already at the indicated OFFSET.  */
	  BINFO_OFFSET (base_binfo)
	    = size_binop (PLUS_EXPR, BINFO_OFFSET (base_binfo), offset);

	  propagate_binfo_offsets (base_binfo, offset);
	}
    }
  else
    {
      /* This algorithm, used for the old ABI, is neither simple, nor
	 general.  For example, it mishandles the case of:
       
           struct A;
	   struct B : public A;
	   struct C : public B;
	   
	 if B is at offset zero in C, but A is not in offset zero in
	 B.  In that case, it sets the BINFO_OFFSET for A to zero.
	 (This sitution arises in the new ABI if B has virtual
	 functions, but A does not.)  Rather than change this
	 algorithm, and risking breaking the old ABI, it is preserved
	 here.  */
      for (i = 0; i < n_baselinks; /* note increment is done in the
				      loop.  */)
	{
	  tree base_binfo = TREE_VEC_ELT (binfos, i);

	  if (TREE_VIA_VIRTUAL (base_binfo))
	    i += 1;
	  else
	    {
	      int j;
	      tree delta = NULL_TREE;

	      for (j = i+1; j < n_baselinks; j++)
		if (! TREE_VIA_VIRTUAL (TREE_VEC_ELT (binfos, j)))
		  {
		    /* The next basetype offset must take into account
		       the space between the classes, not just the
		       size of each class.  */
		    delta = size_binop (MINUS_EXPR,
					BINFO_OFFSET (TREE_VEC_ELT (binfos, 
								    j)),
					BINFO_OFFSET (base_binfo));
		    break;
		  }

	      BINFO_OFFSET (base_binfo) = offset;

	      propagate_binfo_offsets (base_binfo, offset);

	      /* Go to our next class that counts for offset
                 propagation.  */
	      i = j;
	      if (i < n_baselinks)
		offset = size_binop (PLUS_EXPR, offset, delta);
	    }
	}
    }
}

/* Makes new binfos for the indirect bases under BINFO, and updates
   BINFO_OFFSET for them and their bases.  */

void
unshare_base_binfos (binfo)
     tree binfo;
{
  tree binfos = BINFO_BASETYPES (binfo);
  tree new_binfo;
  int j;

  if (binfos == NULL_TREE)
    return;

  /* Now unshare the structure beneath BINFO.  */
  for (j = TREE_VEC_LENGTH (binfos)-1;
       j >= 0; j--)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, j);
      new_binfo = TREE_VEC_ELT (binfos, j)
	= make_binfo (BINFO_OFFSET (base_binfo),
		      base_binfo,
		      BINFO_VTABLE (base_binfo),
		      BINFO_VIRTUALS (base_binfo));
      TREE_VIA_PUBLIC (new_binfo) = TREE_VIA_PUBLIC (base_binfo);
      TREE_VIA_PROTECTED (new_binfo) = TREE_VIA_PROTECTED (base_binfo);
      TREE_VIA_VIRTUAL (new_binfo) = TREE_VIA_VIRTUAL (base_binfo);
      BINFO_INHERITANCE_CHAIN (new_binfo) = binfo;
      unshare_base_binfos (new_binfo);
    }
}

/* Finish the work of layout_record, now taking virtual bases into account.
   Also compute the actual offsets that our base classes will have.
   This must be performed after the fields are laid out, since virtual
   baseclasses must lay down at the end of the record.

   Returns the maximum number of virtual functions any of the
   baseclasses provide.  */

int
layout_basetypes (rec, max)
     tree rec;
     int max;
{
  tree binfos = TYPE_BINFO_BASETYPES (rec);
  int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (rec);
  tree vbase_types;
  tree *field;

  unsigned int record_align = MAX (BITS_PER_UNIT, TYPE_ALIGN (rec));
  unsigned int desired_align;

  /* Record size so far is CONST_SIZE bits, where CONST_SIZE is an integer.  */
  register unsigned int const_size = 0;
  unsigned int nonvirtual_const_size;

#ifdef STRUCTURE_SIZE_BOUNDARY
  /* Packed structures don't need to have minimum size.  */
  if (! TYPE_PACKED (rec))
    record_align = MAX (record_align, STRUCTURE_SIZE_BOUNDARY);
#endif

  /* Get all the virtual base types that this type uses.  The
     TREE_VALUE slot holds the virtual baseclass type.  Note that
     get_vbase_types makes copies of the virtual base BINFOs, so that
     the vbase_types are unshared.  */
  vbase_types = CLASSTYPE_VBASECLASSES (rec);

  my_friendly_assert (TREE_CODE (TYPE_SIZE (rec)) == INTEGER_CST, 19970302);
  const_size = TREE_INT_CST_LOW (TYPE_SIZE (rec));

  nonvirtual_const_size = const_size;

  while (vbase_types)
    {
      tree basetype = BINFO_TYPE (vbase_types);
      tree offset;

      desired_align = TYPE_ALIGN (basetype);
      record_align = MAX (record_align, desired_align);

      if (const_size == 0)
	offset = integer_zero_node;
      else
	{
	  /* Give each virtual base type the alignment it wants.  */
	  const_size = CEIL (const_size, desired_align) * desired_align;
	  offset = size_int (CEIL (const_size, BITS_PER_UNIT));
	}

      if (CLASSTYPE_VSIZE (basetype) > max)
	max = CLASSTYPE_VSIZE (basetype);
      BINFO_OFFSET (vbase_types) = offset;

      /* Every virtual baseclass takes a least a UNIT, so that we can
	 take it's address and get something different for each base.  */
      const_size += MAX (BITS_PER_UNIT,
			 TREE_INT_CST_LOW (CLASSTYPE_SIZE (basetype)));

      vbase_types = TREE_CHAIN (vbase_types);
    }

  if (const_size)
    {
      /* Because a virtual base might take a single byte above,
	 we have to re-adjust the total size to make sure it is
	 a multiple of the alignment.  */
      /* Give the whole object the alignment it wants.  */
      const_size = CEIL (const_size, record_align) * record_align;
    }

  /* Set the alignment in the complete type.  We don't set CLASSTYPE_ALIGN
   here, as that is for this class, without any virtual base classes.  */
  TYPE_ALIGN (rec) = record_align;
  if (const_size != nonvirtual_const_size)
    {
      TYPE_SIZE (rec) = size_int (const_size);
      TYPE_SIZE_UNIT (rec) = size_binop (FLOOR_DIV_EXPR, TYPE_SIZE (rec),
                                         size_int (BITS_PER_UNIT));
    }

  /* Now propagate offset information throughout the lattice.
     Simultaneously, remove the temporary FIELD_DECLS we created in
     build_base_fields to refer to base types.  */
  field = &TYPE_FIELDS (rec);
  if (TYPE_VFIELD (rec) == *field)
    {
      /* If this class did not have a primary base, we create a
	 virtual function table pointer.  It will be the first thing
	 in the class, under the new ABI.  Skip it; the base fields
	 will follow it.  */
      my_friendly_assert (flag_new_abi 
			  && !CLASSTYPE_HAS_PRIMARY_BASE_P (rec),
			  19991218);
      field = &TREE_CHAIN (*field);
    }
    
  for (i = 0; i < n_baseclasses; i++)
    {
      register tree base_binfo = TREE_VEC_ELT (binfos, i);
      register tree basetype = BINFO_TYPE (base_binfo);

      if (TREE_VIA_VIRTUAL (base_binfo))
	continue;

      my_friendly_assert (TREE_TYPE (*field) == basetype, 23897);

      if (get_base_distance (basetype, rec, 0, (tree*)0) == -2)
	cp_warning ("direct base `%T' inaccessible in `%T' due to ambiguity",
		    basetype, rec);

      BINFO_OFFSET (base_binfo)
	= size_int (CEIL (TREE_INT_CST_LOW (DECL_FIELD_BITPOS (*field)),
			  BITS_PER_UNIT));
      propagate_binfo_offsets (base_binfo, BINFO_OFFSET (base_binfo));

      /* Remove this field.  */
      *field = TREE_CHAIN (*field);
    }

  for (vbase_types = CLASSTYPE_VBASECLASSES (rec); vbase_types;
       vbase_types = TREE_CHAIN (vbase_types))
    {
      BINFO_INHERITANCE_CHAIN (vbase_types) = TYPE_BINFO (rec);
      unshare_base_binfos (vbase_types);
      propagate_binfo_offsets (vbase_types, BINFO_OFFSET (vbase_types));

      if (extra_warnings)
	{
	  tree basetype = BINFO_TYPE (vbase_types);
	  if (get_base_distance (basetype, rec, 0, (tree*)0) == -2)
	    cp_warning ("virtual base `%T' inaccessible in `%T' due to ambiguity",
			basetype, rec);
	}
    }

  return max;
}


/* Hashing of lists so that we don't make duplicates.
   The entry point is `list_hash_canon'.  */

/* Each hash table slot is a bucket containing a chain
   of these structures.  */

struct list_hash
{
  struct list_hash *next;	/* Next structure in the bucket.  */
  int hashcode;			/* Hash code of this list.  */
  tree list;			/* The list recorded here.  */
};

/* Now here is the hash table.  When recording a list, it is added
   to the slot whose index is the hash code mod the table size.
   Note that the hash table is used for several kinds of lists.
   While all these live in the same table, they are completely independent,
   and the hash code is computed differently for each of these.  */

#define TYPE_HASH_SIZE 59
static struct list_hash *list_hash_table[TYPE_HASH_SIZE];

/* Compute a hash code for a list (chain of TREE_LIST nodes
   with goodies in the TREE_PURPOSE, TREE_VALUE, and bits of the
   TREE_COMMON slots), by adding the hash codes of the individual entries.  */

static int
list_hash (purpose, value, chain)
     tree purpose, value, chain;
{
  register int hashcode = 0;

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

/* Look in the type hash table for a type isomorphic to TYPE.
   If one is found, return it.  Otherwise return 0.  */

static tree
list_hash_lookup (hashcode, purpose, value, chain)
     int hashcode;
     tree purpose, value, chain;
{
  register struct list_hash *h;

  for (h = list_hash_table[hashcode % TYPE_HASH_SIZE]; h; h = h->next)
    if (h->hashcode == hashcode
	&& TREE_PURPOSE (h->list) == purpose
	&& TREE_VALUE (h->list) == value
	&& TREE_CHAIN (h->list) == chain)
      return h->list;
  return 0;
}

/* Add an entry to the list-hash-table
   for a list TYPE whose hash code is HASHCODE.  */

static void
list_hash_add (hashcode, list)
     int hashcode;
     tree list;
{
  register struct list_hash *h;

  h = (struct list_hash *) obstack_alloc (&permanent_obstack, sizeof (struct list_hash));
  h->hashcode = hashcode;
  h->list = list;
  h->next = list_hash_table[hashcode % TYPE_HASH_SIZE];
  list_hash_table[hashcode % TYPE_HASH_SIZE] = h;
}

/* Given list components PURPOSE, VALUE, AND CHAIN, return the canonical
   object for an identical list if one already exists.  Otherwise, build a
   new one, and record it as the canonical object.  */

/* Set to 1 to debug without canonicalization.  Never set by program.  */

static int debug_no_list_hash = 0;

tree
hash_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  tree t;
  int hashcode = 0;

  if (! debug_no_list_hash)
    {
      hashcode = list_hash (purpose, value, chain);
      t = list_hash_lookup (hashcode, purpose, value, chain);
      if (t)
	return t;
    }

  t = tree_cons (purpose, value, chain);

  /* If this is a new list, record it for later reuse.  */
  if (! debug_no_list_hash)
    list_hash_add (hashcode, t);

  return t;
}

/* Constructor for hashed lists.  */

tree
hash_tree_chain (value, chain)
     tree value, chain;
{
  return hash_tree_cons (NULL_TREE, value, chain);
}

/* Similar, but used for concatenating two lists.  */

tree
hash_chainon (list1, list2)
     tree list1, list2;
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
make_binfo (offset, binfo, vtable, virtuals)
     tree offset, binfo;
     tree vtable, virtuals;
{
  tree new_binfo = make_tree_vec (7);
  tree type;

  if (TREE_CODE (binfo) == TREE_VEC)
    type = BINFO_TYPE (binfo);
  else
    {
      type = binfo;
      binfo = CLASS_TYPE_P (type) ? TYPE_BINFO (binfo) : NULL_TREE;
    }

  TREE_TYPE (new_binfo) = TYPE_MAIN_VARIANT (type);
  BINFO_OFFSET (new_binfo) = offset;
  BINFO_VTABLE (new_binfo) = vtable;
  BINFO_VIRTUALS (new_binfo) = virtuals;
  BINFO_VPTR_FIELD (new_binfo) = NULL_TREE;

  if (binfo && BINFO_BASETYPES (binfo) != NULL_TREE)
    BINFO_BASETYPES (new_binfo) = copy_node (BINFO_BASETYPES (binfo));      
  return new_binfo;
}

/* Return the binfo value for ELEM in TYPE.  */

tree
binfo_value (elem, type)
     tree elem;
     tree type;
{
  if (get_base_distance (elem, type, 0, (tree *)0) == -2)
    compiler_error ("base class `%s' ambiguous in binfo_value",
		    TYPE_NAME_STRING (elem));
  if (elem == type)
    return TYPE_BINFO (type);
  if (TREE_CODE (elem) == RECORD_TYPE && TYPE_BINFO (elem) == type)
    return type;
  return get_binfo (elem, type, 0);
}

/* Return a reversed copy of the BINFO-chain given by PATH.  (If the 
   BINFO_INHERITANCE_CHAIN points from base classes to derived
   classes, it will instead point from derived classes to base
   classes.)  Returns the first node in the reversed chain.  */

tree
reverse_path (path)
     tree path;
{
  register tree prev = NULL_TREE, cur;
  for (cur = path; cur; cur = BINFO_INHERITANCE_CHAIN (cur))
    {
      tree r = copy_node (cur);
      BINFO_INHERITANCE_CHAIN (r) = prev;
      prev = r;
    }
  return prev;
}

void
debug_binfo (elem)
     tree elem;
{
  unsigned HOST_WIDE_INT n;
  tree virtuals;

  fprintf (stderr, "type \"%s\"; offset = %ld\n",
	   TYPE_NAME_STRING (BINFO_TYPE (elem)),
	   (long) TREE_INT_CST_LOW (BINFO_OFFSET (elem)));
  fprintf (stderr, "vtable type:\n");
  debug_tree (BINFO_TYPE (elem));
  if (BINFO_VTABLE (elem))
    fprintf (stderr, "vtable decl \"%s\"\n", IDENTIFIER_POINTER (DECL_NAME (BINFO_VTABLE (elem))));
  else
    fprintf (stderr, "no vtable decl yet\n");
  fprintf (stderr, "virtuals:\n");
  virtuals = BINFO_VIRTUALS (elem);

  n = skip_rtti_stuff (&virtuals, BINFO_TYPE (elem));

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
count_functions (t)
     tree t;
{
  int i;
  if (TREE_CODE (t) == FUNCTION_DECL)
    return 1;
  else if (TREE_CODE (t) == OVERLOAD)
    {
      for (i=0; t; t = OVL_CHAIN (t))
	i++;
      return i;
    }

  my_friendly_abort (359);
  return 0;
}

int
is_overloaded_fn (x)
     tree x;
{
  /* A baselink is also considered an overloaded function.  */
  if (TREE_CODE (x) == OFFSET_REF)
    x = TREE_OPERAND (x, 1);
  if (BASELINK_P (x))
    x = TREE_VALUE (x);
  return (TREE_CODE (x) == FUNCTION_DECL
	  || TREE_CODE (x) == TEMPLATE_ID_EXPR
	  || DECL_FUNCTION_TEMPLATE_P (x)
	  || TREE_CODE (x) == OVERLOAD);
}

int
really_overloaded_fn (x)
     tree x;
{     
  /* A baselink is also considered an overloaded function.  */
  if (TREE_CODE (x) == OFFSET_REF)
    x = TREE_OPERAND (x, 1);
  if (BASELINK_P (x))
    x = TREE_VALUE (x);
  return (TREE_CODE (x) == OVERLOAD 
	  && (TREE_CHAIN (x) != NULL_TREE
	      || DECL_FUNCTION_TEMPLATE_P (OVL_FUNCTION (x))));
}

tree
get_first_fn (from)
     tree from;
{
  my_friendly_assert (is_overloaded_fn (from), 9);
  /* A baselink is also considered an overloaded function. */
  if (BASELINK_P (from))
    from = TREE_VALUE (from);
  return OVL_CURRENT (from);
}

/* Returns nonzero if T is a ->* or .* expression that refers to a
   member function.  */

int
bound_pmf_p (t)
     tree t;
{
  return (TREE_CODE (t) == OFFSET_REF
	  && TYPE_PTRMEMFUNC_P (TREE_TYPE (TREE_OPERAND (t, 1))));
}

/* Return a new OVL node, concatenating it with the old one. */

tree
ovl_cons (decl, chain)
     tree decl;
     tree chain;
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
build_overload (decl, chain)
     tree decl;
     tree chain;
{
  if (! chain && TREE_CODE (decl) != TEMPLATE_DECL)
    return decl;
  if (chain && TREE_CODE (chain) != OVERLOAD)
    chain = ovl_cons (chain, NULL_TREE);
  return ovl_cons (decl, chain);
}

/* True if fn is in ovl. */

int
ovl_member (fn, ovl)
     tree fn;
     tree ovl;
{
  if (ovl == NULL_TREE)
    return 0;
  if (TREE_CODE (ovl) != OVERLOAD)
    return ovl == fn;
  for (; ovl; ovl = OVL_CHAIN (ovl))
    if (OVL_FUNCTION (ovl) == fn)
      return 1;
  return 0;
}

int
is_aggr_type_2 (t1, t2)
     tree t1, t2;
{
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return 0;
  return IS_AGGR_TYPE (t1) && IS_AGGR_TYPE (t2);
}

/* Returns non-zero if CODE is the code for a statement.  */

static int
statement_code_p (code)
     enum tree_code code;
{
  switch (code)
    {
    case EXPR_STMT:
    case COMPOUND_STMT:
    case DECL_STMT:
    case IF_STMT:
    case FOR_STMT:
    case WHILE_STMT:
    case DO_STMT:
    case RETURN_STMT:
    case BREAK_STMT:
    case CONTINUE_STMT:
    case SWITCH_STMT:
    case GOTO_STMT:
    case LABEL_STMT:
    case ASM_STMT:
    case SUBOBJECT:
    case CLEANUP_STMT:
    case START_CATCH_STMT:
    case CTOR_STMT:
    case SCOPE_STMT:
    case CTOR_INITIALIZER:
    case CASE_LABEL:
    case RETURN_INIT:
    case TRY_BLOCK:
    case HANDLER:
      return 1;

    default:
      return 0;
    }
}

#define PRINT_RING_SIZE 4

const char *
lang_printable_name (decl, v)
     tree decl;
     int v;
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
	my_friendly_abort (106);
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
build_exception_variant (type, raises)
     tree type;
     tree raises;
{
  tree v = TYPE_MAIN_VARIANT (type);
  int type_quals = TYPE_QUALS (type);

  for (; v; v = TYPE_NEXT_VARIANT (v))
    if (TYPE_QUALS (v) == type_quals
        && comp_except_specs (raises, TYPE_RAISES_EXCEPTIONS (v), 1))
      return v;

  /* Need to build a new variant.  */
  v = build_type_copy (type);
  TYPE_RAISES_EXCEPTIONS (v) = raises;
  return v;
}

/* Given a TEMPLATE_TEMPLATE_PARM node T, create a new one together with its 
   lang_specific field and its corresponding TEMPLATE_DECL node */

tree
copy_template_template_parm (t)
     tree t;
{
  tree template = TYPE_NAME (t);
  tree t2;

  t2 = make_aggr_type (TEMPLATE_TEMPLATE_PARM);
  template = copy_node (template);
  copy_lang_decl (template);

  TREE_TYPE (template) = t2;
  TYPE_NAME (t2) = template;
  TYPE_STUB_DECL (t2) = template;

  /* No need to copy these */
  TYPE_FIELDS (t2) = TYPE_FIELDS (t);
  TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (t2) 
    = TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (t);
  return t2;
}

/* Apply FUNC to all the sub-trees of TP in a pre-order traversal.
   FUNC is called with the DATA and the address of each sub-tree.  If
   FUNC returns a non-NULL value, the traversal is aborted, and the
   value returned by FUNC is returned.  */

tree 
walk_tree (tp, func, data)
     tree *tp;
     walk_tree_fn func;
     void *data;
{
  enum tree_code code;
  int walk_subtrees;
  tree result;
  
#define WALK_SUBTREE(NODE)			\
  do						\
    {						\
      result = walk_tree (&(NODE), func, data);	\
      if (result)				\
	return result;				\
    }						\
  while (0)

  /* Skip empty subtrees.  */
  if (!*tp)
    return NULL_TREE;

  /* Call the function.  */
  walk_subtrees = 1;
  result = (*func) (tp, &walk_subtrees, data);

  /* If we found something, return it.  */
  if (result)
    return result;

  /* Even if we didn't, FUNC may have decided that there was nothing
     interesting below this point in the tree.  */
  if (!walk_subtrees)
    return NULL_TREE;

  code = TREE_CODE (*tp);

  /* Handle common cases up front.  */
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code))
      || TREE_CODE_CLASS (code) == 'r'
      || TREE_CODE_CLASS (code) == 's')
    {
      int i, len;

      /* Walk over all the sub-trees of this operand.  */
      len = first_rtl_op (code);
      /* TARGET_EXPRs are peculiar: operands 1 and 3 can be the same.
	 But, we only want to walk once.  */
      if (code == TARGET_EXPR
	  && TREE_OPERAND (*tp, 3) == TREE_OPERAND (*tp, 1))
	--len;
      /* Go through the subtrees.  We need to do this in forward order so
         that the scope of a FOR_EXPR is handled properly.  */
      for (i = 0; i < len; ++i)
	WALK_SUBTREE (TREE_OPERAND (*tp, i));

      /* For statements, we also walk the chain so that we cover the
	 entire statement tree.  */
      if (statement_code_p (code))
	{
	  if (code == DECL_STMT 
	      && DECL_STMT_DECL (*tp) 
	      && TREE_CODE_CLASS (TREE_CODE (DECL_STMT_DECL (*tp))) == 'd')
	    {
	      /* Walk the DECL_INITIAL and DECL_SIZE.  We don't want to walk
		 into declarations that are just mentioned, rather than
		 declared; they don't really belong to this part of the tree.
		 And, we can see cycles: the initializer for a declaration can
		 refer to the declaration itself.  */
	      WALK_SUBTREE (DECL_INITIAL (DECL_STMT_DECL (*tp)));
	      WALK_SUBTREE (DECL_SIZE (DECL_STMT_DECL (*tp)));
	    }

	  WALK_SUBTREE (TREE_CHAIN (*tp));
	}

      /* We didn't find what we were looking for.  */
      return NULL_TREE;
    }
  else if (TREE_CODE_CLASS (code) == 'd')
    {
      WALK_SUBTREE (TREE_TYPE (*tp));

      /* We didn't find what we were looking for.  */
      return NULL_TREE;
    }

  /* Not one of the easy cases.  We must explicitly go through the
     children.  */
  switch (code)
    {
    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case DEFAULT_ARG:
    case TEMPLATE_TEMPLATE_PARM:
    case TEMPLATE_PARM_INDEX:
    case TEMPLATE_TYPE_PARM:
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case VOID_TYPE:
    case BOOLEAN_TYPE:
    case TYPENAME_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
    case TYPEOF_TYPE:
    case BLOCK:
      /* None of thse have subtrees other than those already walked
         above.  */
      break;

    case PTRMEM_CST:
      WALK_SUBTREE (TREE_TYPE (*tp));
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      WALK_SUBTREE (TREE_TYPE (*tp));
      break;

    case TREE_LIST:
      WALK_SUBTREE (TREE_PURPOSE (*tp));
      WALK_SUBTREE (TREE_VALUE (*tp));
      WALK_SUBTREE (TREE_CHAIN (*tp));
      break;

    case OVERLOAD:
      WALK_SUBTREE (OVL_FUNCTION (*tp));
      WALK_SUBTREE (OVL_CHAIN (*tp));
      break;

    case TREE_VEC:
      {
	int len = TREE_VEC_LENGTH (*tp);
	while (len--)
	  WALK_SUBTREE (TREE_VEC_ELT (*tp, len));
      }
      break;

    case COMPLEX_CST:
      WALK_SUBTREE (TREE_REALPART (*tp));
      WALK_SUBTREE (TREE_IMAGPART (*tp));
      break;

    case CONSTRUCTOR:
      WALK_SUBTREE (CONSTRUCTOR_ELTS (*tp));
      break;

    case METHOD_TYPE:
      WALK_SUBTREE (TYPE_METHOD_BASETYPE (*tp));
      /* Fall through.  */

    case FUNCTION_TYPE:
      WALK_SUBTREE (TREE_TYPE (*tp));
      WALK_SUBTREE (TYPE_ARG_TYPES (*tp));
      break;

    case ARRAY_TYPE:
      WALK_SUBTREE (TREE_TYPE (*tp));
      WALK_SUBTREE (TYPE_DOMAIN (*tp));
      break;

    case INTEGER_TYPE:
      WALK_SUBTREE (TYPE_MIN_VALUE (*tp));
      WALK_SUBTREE (TYPE_MAX_VALUE (*tp));
      break;

    case OFFSET_TYPE:
      WALK_SUBTREE (TREE_TYPE (*tp));
      WALK_SUBTREE (TYPE_OFFSET_BASETYPE (*tp));
      break;

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (*tp))
	WALK_SUBTREE (TYPE_PTRMEMFUNC_FN_TYPE (*tp));
      break;

    default:
      my_friendly_abort (19990803);
    }

  /* We didn't find what we were looking for.  */
  return NULL_TREE;

#undef WALK_SUBTREE
}

/* Passed to walk_tree.  Checks for the use of types with no linkage.  */

static tree
no_linkage_helper (tp, walk_subtrees, data)
     tree *tp;
     int *walk_subtrees ATTRIBUTE_UNUSED;
     void *data ATTRIBUTE_UNUSED;
{
  tree t = *tp;

  if (TYPE_P (t)
      && (IS_AGGR_TYPE (t) || TREE_CODE (t) == ENUMERAL_TYPE)
      && (decl_function_context (TYPE_MAIN_DECL (t))
	  || ANON_AGGRNAME_P (TYPE_IDENTIFIER (t))))
    return t;
  return NULL_TREE;
}

/* Check if the type T depends on a type with no linkage and if so, return
   it.  */

tree
no_linkage_check (t)
     tree t;
{
  /* There's no point in checking linkage on template functions; we
     can't know their complete types.  */
  if (processing_template_decl)
    return NULL_TREE;

  t = walk_tree (&t, no_linkage_helper, NULL);
  if (t != error_mark_node)
    return t;
  return NULL_TREE;
}

/* Passed to walk_tree.  Copies the node pointed to, if appropriate.  */

tree
copy_tree_r (tp, walk_subtrees, data)
     tree *tp;
     int *walk_subtrees;
     void *data ATTRIBUTE_UNUSED;
{
  enum tree_code code = TREE_CODE (*tp);

  /* We make copies of most nodes.  */
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code))
      || TREE_CODE_CLASS (code) == 'r'
      || TREE_CODE_CLASS (code) == 'c'
      || TREE_CODE_CLASS (code) == 's'
      || code == PARM_DECL
      || code == TREE_LIST
      || code == TREE_VEC
      || code == OVERLOAD)
    {
      /* Because the chain gets clobbered when we make a copy, we save it
	 here.  */
      tree chain = TREE_CHAIN (*tp);

      /* Copy the node.  */
      *tp = copy_node (*tp);

      /* Now, restore the chain, if appropriate.  That will cause
	 walk_tree to walk into the chain as well.  */
      if (code == PARM_DECL || code == TREE_LIST || code == OVERLOAD
	  || statement_code_p (code))
	TREE_CHAIN (*tp) = chain;

      /* For now, we don't update BLOCKs when we make copies.  So, we
	 have to nullify all scope-statements.  */
      if (TREE_CODE (*tp) == SCOPE_STMT)
	SCOPE_STMT_BLOCK (*tp) = NULL_TREE;
    }
  else if (code == TEMPLATE_TEMPLATE_PARM)
    /* These must be copied specially.  */
    *tp = copy_template_template_parm (*tp);
  else if (TREE_CODE_CLASS (code) == 't')
    /* There's no need to copy types, or anything beneath them.  */
    *walk_subtrees = 0;

  return NULL_TREE;
}

#ifdef GATHER_STATISTICS
extern int depth_reached;
#endif

void
print_lang_statistics ()
{
  print_search_statistics ();
  print_class_statistics ();
#ifdef GATHER_STATISTICS
  fprintf (stderr, "maximum template instantiation depth reached: %d\n",
	   depth_reached);
#endif
}

/* This is used by the `assert' macro.  It is provided in libgcc.a,
   which `cc' doesn't know how to link.  Note that the C++ front-end
   no longer actually uses the `assert' macro (instead, it calls
   my_friendly_assert).  But all of the back-end files still need this.  */

void
__eprintf (string, expression, line, filename)
     const char *string;
     const char *expression;
     unsigned line;
     const char *filename;
{
  fprintf (stderr, string, expression, line, filename);
  fflush (stderr);
  abort ();
}

/* Return, as an INTEGER_CST node, the number of elements for TYPE
   (which is an ARRAY_TYPE).  This counts only elements of the top
   array.  */

tree
array_type_nelts_top (type)
     tree type;
{
  return fold (build (PLUS_EXPR, sizetype,
		      array_type_nelts (type),
		      integer_one_node));
}

/* Return, as an INTEGER_CST node, the number of elements for TYPE
   (which is an ARRAY_TYPE).  This one is a recursive count of all
   ARRAY_TYPEs that are clumped together.  */

tree
array_type_nelts_total (type)
     tree type;
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
bot_manip (tp, walk_subtrees, data)
     tree *tp;
     int *walk_subtrees;
     void *data;
{
  splay_tree target_remap = ((splay_tree) data);
  tree t = *tp;

  if (TREE_CODE (t) != TREE_LIST && ! TREE_SIDE_EFFECTS (t))
    {
      /* There can't be any TARGET_EXPRs below this point.  */
      *walk_subtrees = 0;
      return NULL_TREE;
    }
  else if (TREE_CODE (t) == TARGET_EXPR)
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
	  u = copy_node (t);
	  TREE_OPERAND (u, 0) = build (VAR_DECL, TREE_TYPE (t));
	  layout_decl (TREE_OPERAND (u, 0), 0);
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
bot_replace (t, walk_subtrees, data)
     tree *t;
     int *walk_subtrees ATTRIBUTE_UNUSED;
     void *data;
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
break_out_target_exprs (t)
     tree t;
{
  static int target_remap_count;
  static splay_tree target_remap;

  if (!target_remap_count++)
    target_remap = splay_tree_new (splay_tree_compare_pointers, 
				   /*splay_tree_delete_key_fn=*/NULL, 
				   /*splay_tree_delete_value_fn=*/NULL);
  walk_tree (&t, bot_manip, target_remap);
  walk_tree (&t, bot_replace, target_remap);

  if (!--target_remap_count)
    {
      splay_tree_delete (target_remap);
      target_remap = NULL;
    }

  return t;
}

/* Obstack used for allocating nodes in template function and variable
   definitions.  */

/* Similar to `build_nt', except that we set TREE_COMPLEXITY to be the
   current line number.  */

tree
build_min_nt VPROTO((enum tree_code code, ...))
{
#ifndef ANSI_PROTOTYPES
  enum tree_code code;
#endif
  va_list p;
  register tree t;
  register int length;
  register int i;

  VA_START (p, code);

#ifndef ANSI_PROTOTYPES
  code = va_arg (p, enum tree_code);
#endif

  t = make_node (code);
  length = tree_code_length[(int) code];
  TREE_COMPLEXITY (t) = lineno;

  for (i = 0; i < length; i++)
    {
      tree x = va_arg (p, tree);
      TREE_OPERAND (t, i) = x;
    }

  va_end (p);
  return t;
}

/* Similar to `build', except we set TREE_COMPLEXITY to the current
   line-number.  */

tree
build_min VPROTO((enum tree_code code, tree tt, ...))
{
#ifndef ANSI_PROTOTYPES
  enum tree_code code;
  tree tt;
#endif
  va_list p;
  register tree t;
  register int length;
  register int i;

  VA_START (p, tt);

#ifndef ANSI_PROTOTYPES
  code = va_arg (p, enum tree_code);
  tt = va_arg (p, tree);
#endif

  t = make_node (code);
  length = tree_code_length[(int) code];
  TREE_TYPE (t) = tt;
  TREE_COMPLEXITY (t) = lineno;

  for (i = 0; i < length; i++)
    {
      tree x = va_arg (p, tree);
      TREE_OPERAND (t, i) = x;
    }

  va_end (p);
  return t;
}

tree
get_type_decl (t)
     tree t;
{
  if (TREE_CODE (t) == TYPE_DECL)
    return t;
  if (TREE_CODE_CLASS (TREE_CODE (t)) == 't')
    return TYPE_STUB_DECL (t);
  
  my_friendly_abort (42);

  /* Stop compiler from complaining control reaches end of non-void function.  */
  return 0;
}

int
can_free (obstack, t)
     struct obstack *obstack;
     tree t;
{
  int size = 0;

  if (TREE_CODE (t) == TREE_VEC)
    size = (TREE_VEC_LENGTH (t)-1) * sizeof (tree) + sizeof (struct tree_vec);
  else
    my_friendly_abort (42);

#define ROUND(x) ((x + obstack_alignment_mask (obstack)) \
		  & ~ obstack_alignment_mask (obstack))
  if ((char *)t + ROUND (size) == obstack_next_free (obstack))
    return 1;
#undef ROUND

  return 0;
}

/* Return first vector element whose BINFO_TYPE is ELEM.
   Return 0 if ELEM is not in VEC.  VEC may be NULL_TREE.  */

tree
vec_binfo_member (elem, vec)
     tree elem, vec;
{
  int i;

  if (vec)
    for (i = 0; i < TREE_VEC_LENGTH (vec); ++i)
      if (same_type_p (elem, BINFO_TYPE (TREE_VEC_ELT (vec, i))))
	return TREE_VEC_ELT (vec, i);

  return NULL_TREE;
}

/* Kludge around the fact that DECL_CONTEXT for virtual functions returns
   the wrong thing for decl_function_context.  Hopefully the uses in the
   backend won't matter, since we don't need a static chain for local class
   methods.  FIXME!  */

tree
hack_decl_function_context (decl)
     tree decl;
{
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_FUNCTION_MEMBER_P (decl))
    return decl_function_context (TYPE_MAIN_DECL (DECL_CLASS_CONTEXT (decl)));
  return decl_function_context (decl);
}

/* Returns the namespace that contains DECL, whether directly or
   indirectly.  */

tree
decl_namespace_context (decl)
     tree decl;
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
   Return 1 if they are the same.
   Return 0 if they are understandably different.
   Return -1 if either contains tree structure not understood by
   this function.  */

int
cp_tree_equal (t1, t2)
     tree t1, t2;
{
  register enum tree_code code1, code2;
  int cmp;

  if (t1 == t2)
    return 1;
  if (t1 == 0 || t2 == 0)
    return 0;

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  if (code1 == NOP_EXPR || code1 == CONVERT_EXPR || code1 == NON_LVALUE_EXPR)
    {
      if (code2 == NOP_EXPR || code2 == CONVERT_EXPR || code2 == NON_LVALUE_EXPR)
	return cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      else
	return cp_tree_equal (TREE_OPERAND (t1, 0), t2);
    }
  else if (code2 == NOP_EXPR || code2 == CONVERT_EXPR
	   || code2 == NON_LVALUE_EXPR)
    return cp_tree_equal (t1, TREE_OPERAND (t2, 0));

  if (code1 != code2)
    return 0;

  switch (code1)
    {
    case INTEGER_CST:
      return TREE_INT_CST_LOW (t1) == TREE_INT_CST_LOW (t2)
	&& TREE_INT_CST_HIGH (t1) == TREE_INT_CST_HIGH (t2);

    case REAL_CST:
      return REAL_VALUES_EQUAL (TREE_REAL_CST (t1), TREE_REAL_CST (t2));

    case STRING_CST:
      return TREE_STRING_LENGTH (t1) == TREE_STRING_LENGTH (t2)
	&& !bcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
		  TREE_STRING_LENGTH (t1));

    case CONSTRUCTOR:
      /* We need to do this when determining whether or not two
	 non-type pointer to member function template arguments
	 are the same.  */
      if (!(same_type_p (TREE_TYPE (t1), TREE_TYPE (t2))
	    /* The first operand is RTL.  */
	    && TREE_OPERAND (t1, 0) == TREE_OPERAND (t2, 0)))
	return 0;
      return cp_tree_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case TREE_LIST:
      cmp = cp_tree_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2));
      if (cmp <= 0)
	return cmp;
      cmp = cp_tree_equal (TREE_VALUE (t1), TREE_VALUE (t2));
      if (cmp <= 0)
	return cmp;
      return cp_tree_equal (TREE_CHAIN (t1), TREE_CHAIN (t2));

    case SAVE_EXPR:
      return cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    case CALL_EXPR:
      cmp = cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;
      return simple_cst_list_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case TARGET_EXPR:
      /* Special case: if either target is an unallocated VAR_DECL,
	 it means that it's going to be unified with whatever the
	 TARGET_EXPR is really supposed to initialize, so treat it
	 as being equivalent to anything.  */
      if ((TREE_CODE (TREE_OPERAND (t1, 0)) == VAR_DECL
	   && DECL_NAME (TREE_OPERAND (t1, 0)) == NULL_TREE
	   && DECL_RTL (TREE_OPERAND (t1, 0)) == 0)
	  || (TREE_CODE (TREE_OPERAND (t2, 0)) == VAR_DECL
	      && DECL_NAME (TREE_OPERAND (t2, 0)) == NULL_TREE
	      && DECL_RTL (TREE_OPERAND (t2, 0)) == 0))
	cmp = 1;
      else
	cmp = cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;
      return cp_tree_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case WITH_CLEANUP_EXPR:
      cmp = cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;
      return cp_tree_equal (TREE_OPERAND (t1, 2), TREE_OPERAND (t1, 2));

    case COMPONENT_REF:
      if (TREE_OPERAND (t1, 1) == TREE_OPERAND (t2, 1))
	return cp_tree_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      return 0;

    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
      return 0;

    case TEMPLATE_PARM_INDEX:
      return TEMPLATE_PARM_IDX (t1) == TEMPLATE_PARM_IDX (t2)
	&& TEMPLATE_PARM_LEVEL (t1) == TEMPLATE_PARM_LEVEL (t2);

    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
      if (TREE_CODE (TREE_OPERAND (t1, 0)) != TREE_CODE (TREE_OPERAND (t2, 0)))
	return 0;
      if (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (t1, 0))) == 't')
	return same_type_p (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      break;

    case PTRMEM_CST:
      /* Two pointer-to-members are the same if they point to the same
	 field or function in the same class.  */
      return (PTRMEM_CST_MEMBER (t1) == PTRMEM_CST_MEMBER (t2)
	      && same_type_p (PTRMEM_CST_CLASS (t1), PTRMEM_CST_CLASS (t2)));

    default:
      break;
    }

  switch (TREE_CODE_CLASS (code1))
    {
      int i;
    case '1':
    case '2':
    case '<':
    case 'e':
    case 'r':
    case 's':
      cmp = 1;
      for (i=0; i<tree_code_length[(int) code1]; ++i)
	{
	  cmp = cp_tree_equal (TREE_OPERAND (t1, i), TREE_OPERAND (t2, i));
	  if (cmp <= 0)
	    return cmp;
	}
      return cmp;
    }

  return -1;
}

/* Build a wrapper around some pointer PTR so we can use it as a tree.  */

tree
build_ptr_wrapper (ptr)
     void *ptr;
{
  tree t = make_node (WRAPPER);
  WRAPPER_PTR (t) = ptr;
  return t;
}

/* Same, but on the expression_obstack.  */

tree
build_expr_ptr_wrapper (ptr)
     void *ptr;
{
  return build_ptr_wrapper (ptr);
}

/* Build a wrapper around some integer I so we can use it as a tree.  */

tree
build_int_wrapper (i)
     int i;
{
  tree t = make_node (WRAPPER);
  WRAPPER_INT (t) = i;
  return t;
}

static tree
build_srcloc (file, line)
     char *file;
     int line;
{
  tree t;

  t = make_node (SRCLOC);
  SRCLOC_FILE (t) = file;
  SRCLOC_LINE (t) = line;

  return t;
}

tree
build_srcloc_here ()
{
  return build_srcloc (input_filename, lineno);
}

/* The type of ARG when used as an lvalue.  */

tree
lvalue_type (arg)
     tree arg;
{
  tree type = TREE_TYPE (arg);
  if (TREE_CODE (arg) == OVERLOAD)
    type = unknown_type_node;
  return type;
}

/* The type of ARG for printing error messages; denote lvalues with
   reference types.  */

tree
error_type (arg)
     tree arg;
{
  tree type = TREE_TYPE (arg);
  if (TREE_CODE (type) == ARRAY_TYPE)
    ;
  else if (real_lvalue_p (arg))
    type = build_reference_type (lvalue_type (arg));
  else if (IS_AGGR_TYPE (type))
    type = lvalue_type (arg);

  return type;
}

/* Does FUNCTION use a variable-length argument list?  */

int
varargs_function_p (function)
     tree function;
{
  tree parm = TYPE_ARG_TYPES (TREE_TYPE (function));
  for (; parm; parm = TREE_CHAIN (parm))
    if (TREE_VALUE (parm) == void_type_node)
      return 0;
  return 1;
}

/* Returns 1 if decl is a member of a class.  */

int
member_p (decl)
     tree decl;
{
  tree ctx = DECL_CONTEXT (decl);
  return (ctx && TREE_CODE_CLASS (TREE_CODE (ctx)) == 't');
}

/* Create a placeholder for member access where we don't actually have an
   object that the access is against.  */

tree
build_dummy_object (type)
     tree type;
{
  tree decl = build1 (NOP_EXPR, build_pointer_type (type), void_zero_node);
  return build_indirect_ref (decl, NULL_PTR);
}

/* We've gotten a reference to a member of TYPE.  Return *this if appropriate,
   or a dummy object otherwise.  If BINFOP is non-0, it is filled with the
   binfo path from current_class_type to TYPE, or 0.  */

tree
maybe_dummy_object (type, binfop)
     tree type;
     tree *binfop;
{
  tree decl, context;

  if (current_class_type
      && get_base_distance (type, current_class_type, 0, binfop) != -1)
    context = current_class_type;
  else
    {
      /* Reference from a nested class member function.  */
      context = type;
      if (binfop)
	*binfop = TYPE_BINFO (type);
    }

  if (current_class_ref && context == current_class_type)
    decl = current_class_ref;
  else
    decl = build_dummy_object (context);

  return decl;
}

/* Returns 1 if OB is a placeholder object, or a pointer to one.  */

int
is_dummy_object (ob)
     tree ob;
{
  if (TREE_CODE (ob) == INDIRECT_REF)
    ob = TREE_OPERAND (ob, 0);
  return (TREE_CODE (ob) == NOP_EXPR
	  && TREE_OPERAND (ob, 0) == void_zero_node);
}

/* Returns 1 iff type T is a POD type, as defined in [basic.types].  */

int
pod_type_p (t)
     tree t;
{
  while (TREE_CODE (t) == ARRAY_TYPE)
    t = TREE_TYPE (t);

  if (INTEGRAL_TYPE_P (t))
    return 1;  /* integral, character or enumeral type */
  if (FLOAT_TYPE_P (t))
    return 1;
  if (TYPE_PTR_P (t))
    return 1; /* pointer to non-member */
  if (TYPE_PTRMEM_P (t))
    return 1; /* pointer to member object */
  if (TYPE_PTRMEMFUNC_P (t))
    return 1; /* pointer to member function */
  
  if (! CLASS_TYPE_P (t))
    return 0; /* other non-class type (reference or function) */
  if (CLASSTYPE_NON_POD_P (t))
    return 0;
  return 1;
}

/* Return a 1 if ATTR_NAME and ATTR_ARGS denote a valid C++-specific
   attribute for either declaration DECL or type TYPE and 0 otherwise.
   Plugged into valid_lang_attribute.  */

int
cp_valid_lang_attribute (attr_name, attr_args, decl, type)
  tree attr_name;
  tree attr_args ATTRIBUTE_UNUSED;
  tree decl ATTRIBUTE_UNUSED;
  tree type ATTRIBUTE_UNUSED;
{
  if (is_attribute_p ("com_interface", attr_name))
    {
      if (! flag_vtable_thunks)
	{
	  error ("`com_interface' only supported with -fvtable-thunks");
	  return 0;
	}

      if (attr_args != NULL_TREE
	  || decl != NULL_TREE
	  || ! CLASS_TYPE_P (type)
	  || type != TYPE_MAIN_VARIANT (type))
	{
	  warning ("`com_interface' attribute can only be applied to class definitions");
	  return 0;
	}

      CLASSTYPE_COM_INTERFACE (type) = 1;
      return 1;
    }
  else if (is_attribute_p ("init_priority", attr_name))
    {
      tree initp_expr = (attr_args ? TREE_VALUE (attr_args): NULL_TREE);
      int pri;

      if (initp_expr)
	STRIP_NOPS (initp_expr);
	  
      if (!initp_expr || TREE_CODE (initp_expr) != INTEGER_CST)
	{
	  error ("requested init_priority is not an integer constant");
	  return 0;
	}

      pri = TREE_INT_CST_LOW (initp_expr);
	
      while (TREE_CODE (type) == ARRAY_TYPE)
	type = TREE_TYPE (type);

      if (decl == NULL_TREE
	  || TREE_CODE (decl) != VAR_DECL
	  || ! TREE_STATIC (decl)
	  || DECL_EXTERNAL (decl)
	  || (TREE_CODE (type) != RECORD_TYPE
	      && TREE_CODE (type) != UNION_TYPE)
	  /* Static objects in functions are initialized the
	     first time control passes through that
	     function. This is not precise enough to pin down an
	     init_priority value, so don't allow it. */
	  || current_function_decl) 
	{
	  error ("can only use init_priority attribute on file-scope definitions of objects of class type");
	  return 0;
	}

      if (pri > MAX_INIT_PRIORITY || pri <= 0)
	{
	  error ("requested init_priority is out of range");
	  return 0;
	}

      /* Check for init_priorities that are reserved for
	 language and runtime support implementations.*/
      if (pri <= MAX_RESERVED_INIT_PRIORITY)
	{
	  warning 
	    ("requested init_priority is reserved for internal use");
	}

      DECL_INIT_PRIORITY (decl) = pri;
      return 1;
    }

  return 0;
}

/* Return a new PTRMEM_CST of the indicated TYPE.  The MEMBER is the
   thing pointed to by the constant.  */

tree
make_ptrmem_cst (type, member)
     tree type;
     tree member;
{
  tree ptrmem_cst = make_node (PTRMEM_CST);
  /* If would seem a great convenience if make_node would set
     TREE_CONSTANT for things of class `c', but it does not.  */
  TREE_CONSTANT (ptrmem_cst) = 1;
  TREE_TYPE (ptrmem_cst) = type;
  PTRMEM_CST_MEMBER (ptrmem_cst) = member;
  return ptrmem_cst;
}

/* Mark ARG (which is really a list_hash_table **) for GC.  */

static void
mark_list_hash (arg)
     void *arg;
{
  struct list_hash *lh;

  for (lh = * ((struct list_hash **) arg); lh; lh = lh->next)
    ggc_mark_tree (lh->list);
}

/* Initialize tree.c.  */

void
init_tree ()
{
  make_lang_type_fn = cp_make_lang_type;
  lang_unsave = cp_unsave;
  ggc_add_root (list_hash_table, 
		sizeof (list_hash_table) / sizeof (struct list_hash *),
		sizeof (struct list_hash *),
		mark_list_hash);
}

/* The SAVE_EXPR pointed to by TP is being copied.  If ST contains
   information indicating to what new SAVE_EXPR this one should be
   mapped, use that one.  Otherwise, create a new node and enter it in
   ST.  FN is the function into which the copy will be placed.  */

void
remap_save_expr (tp, st, fn, walk_subtrees)
     tree *tp;
     splay_tree st;
     tree fn;
     int *walk_subtrees;
{
  splay_tree_node n;

  /* See if we already encountered this SAVE_EXPR.  */
  n = splay_tree_lookup (st, (splay_tree_key) *tp);
      
  /* If we didn't already remap this SAVE_EXPR, do so now.  */
  if (!n)
    {
      tree t = copy_node (*tp);

      /* The SAVE_EXPR is now part of the function into which we
	 are inlining this body.  */
      SAVE_EXPR_CONTEXT (t) = fn;
      /* And we haven't evaluated it yet.  */
      SAVE_EXPR_RTL (t) = NULL_RTX;
      /* Remember this SAVE_EXPR.  */
      n = splay_tree_insert (st,
			     (splay_tree_key) *tp,
			     (splay_tree_value) t);
    }
  else
    /* We've already walked into this SAVE_EXPR, so we needn't do it
       again.  */
    *walk_subtrees = 0;

  /* Replace this SAVE_EXPR with the copy.  */
  *tp = (tree) n->value;
}

/* Called via walk_tree.  If *TP points to a DECL_STMT for a local
   declaration, copies the declaration and enters it in the splay_tree
   pointed to by DATA (which is really a `splay_tree *').  */

static tree
mark_local_for_remap_r (tp, walk_subtrees, data)
     tree *tp;
     int *walk_subtrees ATTRIBUTE_UNUSED;
     void *data;
{
  tree t = *tp;
  splay_tree st = (splay_tree) data;

  if ((TREE_CODE (t) == DECL_STMT
       && nonstatic_local_decl_p (DECL_STMT_DECL (t)))
      || TREE_CODE (t) == LABEL_STMT)
    {
      tree decl;
      tree copy;

      /* Figure out what's being declared.  */
      decl = (TREE_CODE (t) == DECL_STMT
	      ? DECL_STMT_DECL (t) : LABEL_STMT_LABEL (t));
      
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
   splay_tree pointed to by ST (which is really a `splay_tree *'),
   remaps all local declarations to appropriate replacements.  */

static tree
cp_unsave_r (tp, walk_subtrees, data)
     tree *tp;
     int *walk_subtrees;
     void *data;
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

/* Called by unsave_expr_now whenever an expression (*TP) needs to be
   unsaved.  */

static void
cp_unsave (tp)
     tree *tp;
{
  splay_tree st;

  /* Create a splay-tree to map old local variable declarations to new
     ones.  */
  st = splay_tree_new (splay_tree_compare_pointers, NULL, NULL);

  /* Walk the tree once figuring out what needs to be remapped.  */
  walk_tree (tp, mark_local_for_remap_r, st);

  /* Walk the tree again, copying, remapping, and unsaving.  */
  walk_tree (tp, cp_unsave_r, st);

  /* Clean up.  */
  splay_tree_delete (st);
}
