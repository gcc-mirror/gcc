/* Language-dependent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987, 88, 92, 93, 94, 1995 Free Software Foundation, Inc.
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
#include <stdio.h>
#include "obstack.h"
#include "tree.h"
#include "cp-tree.h"
#include "flags.h"
#include "rtl.h"

#define CEIL(x,y) (((x) + (y) - 1) / (y))

/* Return nonzero if REF is an lvalue valid for this language.
   Lvalues can be assigned, unless they have TREE_READONLY.
   Lvalues can have their address taken, unless they have DECL_REGISTER.  */

int
real_lvalue_p (ref)
     tree ref;
{
  if (! language_lvalue_valid (ref))
    return 0;
  
  if (TREE_CODE (TREE_TYPE (ref)) == REFERENCE_TYPE)
    return 1;

  if (ref == current_class_decl && flag_this_is_variable <= 0)
    return 0;

  switch (TREE_CODE (ref))
    {
      /* preincrements and predecrements are valid lvals, provided
	 what they refer to are valid lvals. */
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case COMPONENT_REF:
    case SAVE_EXPR:
      return real_lvalue_p (TREE_OPERAND (ref, 0));

    case STRING_CST:
      return 1;

    case VAR_DECL:
      if (TREE_READONLY (ref) && ! TREE_STATIC (ref)
	  && DECL_LANG_SPECIFIC (ref)
	  && DECL_IN_AGGR_P (ref))
	return 0;
    case INDIRECT_REF:
    case ARRAY_REF:
    case PARM_DECL:
    case RESULT_DECL:
    case ERROR_MARK:
      if (TREE_CODE (TREE_TYPE (ref)) != FUNCTION_TYPE
	  && TREE_CODE (TREE_TYPE (ref)) != METHOD_TYPE)
	return 1;
      break;

    case WITH_CLEANUP_EXPR:
      return real_lvalue_p (TREE_OPERAND (ref, 0));

      /* A currently unresolved scope ref.  */
    case SCOPE_REF:
      my_friendly_abort (103);
    case OFFSET_REF:
      if (TREE_CODE (TREE_OPERAND (ref, 1)) == FUNCTION_DECL)
	return 1;
      return real_lvalue_p (TREE_OPERAND (ref, 0))
	&& real_lvalue_p (TREE_OPERAND (ref, 1));
      break;

    case COND_EXPR:
      return (real_lvalue_p (TREE_OPERAND (ref, 1))
	      && real_lvalue_p (TREE_OPERAND (ref, 2)));

    case MODIFY_EXPR:
      return 1;

    case COMPOUND_EXPR:
      return real_lvalue_p (TREE_OPERAND (ref, 1));

    case MAX_EXPR:
    case MIN_EXPR:
      return (real_lvalue_p (TREE_OPERAND (ref, 0))
	      && real_lvalue_p (TREE_OPERAND (ref, 1)));
    }

  return 0;
}

int
lvalue_p (ref)
     tree ref;
{
  if (! language_lvalue_valid (ref))
    return 0;
  
  if (TREE_CODE (TREE_TYPE (ref)) == REFERENCE_TYPE)
    return 1;

  if (ref == current_class_decl && flag_this_is_variable <= 0)
    return 0;

  switch (TREE_CODE (ref))
    {
      /* preincrements and predecrements are valid lvals, provided
	 what they refer to are valid lvals. */
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case COMPONENT_REF:
    case SAVE_EXPR:
      return lvalue_p (TREE_OPERAND (ref, 0));

    case STRING_CST:
      return 1;

    case VAR_DECL:
      if (TREE_READONLY (ref) && ! TREE_STATIC (ref)
	  && DECL_LANG_SPECIFIC (ref)
	  && DECL_IN_AGGR_P (ref))
	return 0;
    case INDIRECT_REF:
    case ARRAY_REF:
    case PARM_DECL:
    case RESULT_DECL:
    case ERROR_MARK:
      if (TREE_CODE (TREE_TYPE (ref)) != FUNCTION_TYPE
	  && TREE_CODE (TREE_TYPE (ref)) != METHOD_TYPE)
	return 1;
      break;

    case WITH_CLEANUP_EXPR:
      return lvalue_p (TREE_OPERAND (ref, 0));

    case TARGET_EXPR:
      return 1;

    case CALL_EXPR:
      if (IS_AGGR_TYPE (TREE_TYPE (ref)))
	return 1;
      break;

      /* A currently unresolved scope ref.  */
    case SCOPE_REF:
      my_friendly_abort (103);
    case OFFSET_REF:
      if (TREE_CODE (TREE_OPERAND (ref, 1)) == FUNCTION_DECL)
	return 1;
      return lvalue_p (TREE_OPERAND (ref, 0))
	&& lvalue_p (TREE_OPERAND (ref, 1));
      break;

    case COND_EXPR:
      return (lvalue_p (TREE_OPERAND (ref, 1))
	      && lvalue_p (TREE_OPERAND (ref, 2)));

    case MODIFY_EXPR:
      return 1;

    case COMPOUND_EXPR:
      return lvalue_p (TREE_OPERAND (ref, 1));

    case MAX_EXPR:
    case MIN_EXPR:
      return (lvalue_p (TREE_OPERAND (ref, 0))
	      && lvalue_p (TREE_OPERAND (ref, 1)));
    }

  return 0;
}

/* Return nonzero if REF is an lvalue valid for this language;
   otherwise, print an error message and return zero.  */

int
lvalue_or_else (ref, string)
     tree ref;
     char *string;
{
  int win = lvalue_p (ref);
  if (! win)
    error ("non-lvalue in %s", string);
  return win;
}

/* INIT is a CALL_EXPR which needs info about its target.
   TYPE is the type that this initialization should appear to have.

   Build an encapsulation of the initialization to perform
   and return it so that it can be processed by language-independent
   and language-specific expression expanders.

   If WITH_CLEANUP_P is nonzero, we build a cleanup for this expression.
   Otherwise, cleanups are not built here.  For example, when building
   an initialization for a stack slot, since the called function handles
   the cleanup, we would not want to do it here.  */
tree
build_cplus_new (type, init, with_cleanup_p)
     tree type;
     tree init;
     int with_cleanup_p;
{
  tree slot;
  tree rval;

  slot = build (VAR_DECL, type);
  layout_decl (slot, 0);
  rval = build (NEW_EXPR, type,
		TREE_OPERAND (init, 0), TREE_OPERAND (init, 1), slot);
  TREE_SIDE_EFFECTS (rval) = 1;
  TREE_ADDRESSABLE (rval) = 1;
  rval = build (TARGET_EXPR, type, slot, rval, 0);
  TREE_SIDE_EFFECTS (rval) = 1;
  TREE_ADDRESSABLE (rval) = 1;

#if 0
  if (with_cleanup_p && TYPE_NEEDS_DESTRUCTOR (type))
    {
      TREE_OPERAND (rval, 2) = error_mark_node;
      rval = build (WITH_CLEANUP_EXPR, type, rval, 0,
		    build_delete (build_pointer_type (type),
				  build_unary_op (ADDR_EXPR, slot, 0),
				  integer_two_node,
				  LOOKUP_NORMAL|LOOKUP_DESTRUCTOR, 0));
      TREE_SIDE_EFFECTS (rval) = 1;
      TREE_ADDRESSABLE (rval) = 1;
    }
#endif
  return rval;
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
    return build_cplus_new (TREE_TYPE (tmp), tmp, 1);

  while (TREE_CODE (tmp) == NOP_EXPR
	 || TREE_CODE (tmp) == CONVERT_EXPR
	 || TREE_CODE (tmp) == NON_LVALUE_EXPR)
    {
      if (TREE_CODE (TREE_OPERAND (tmp, 0)) == CALL_EXPR
	  && TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (TREE_OPERAND (tmp, 0))))
	{
	  TREE_OPERAND (tmp, 0)
	    = build_cplus_new (TREE_TYPE (TREE_OPERAND (tmp, 0)),
			       TREE_OPERAND (tmp, 0), 1);
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
  register tree t1, t2;
  register enum tree_code code;
  register int changed = 0;
  register int i;

  if (exp == NULL_TREE)
    return exp;

  code = TREE_CODE (exp);

  if (code == CALL_EXPR)
    return copy_node (exp);

  /* Don't try and defeat a save_expr, as it should only be done once. */
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

extern struct obstack *current_obstack;
extern struct obstack permanent_obstack, class_obstack;
extern struct obstack *saveable_obstack;

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
  if (IS_SIGNATURE (basetype))
    ptype = build_signature_pointer_type (TYPE_MAIN_VARIANT (basetype),
					  TYPE_READONLY (basetype),
					  TYPE_VOLATILE (basetype));
  else
    ptype = build_pointer_type (basetype);

  /* The actual arglist for this function includes a "hidden" argument
     which is "this".  Put it into the list of argument types.  */

  argtypes = tree_cons (NULL_TREE, ptype, argtypes);
  TYPE_ARG_TYPES (t) = argtypes;
  TREE_SIDE_EFFECTS (argtypes) = 1;  /* Mark first argtype as "artificial".  */

  /* If we already have such a type, use the old one and free this one.
     Note that it also frees up the above cons cell if found.  */
  hashcode = TYPE_HASH (basetype) + TYPE_HASH (rettype) + type_hash_list (argtypes);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);

  return t;
}

tree
build_cplus_staticfn_type (basetype, rettype, argtypes)
     tree basetype, rettype, argtypes;
{
  register tree t;
  int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (FUNCTION_TYPE);

  TYPE_METHOD_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = rettype;

  TYPE_ARG_TYPES (t) = argtypes;

  /* If we already have such a type, use the old one and free this one.
     Note that it also frees up the above cons cell if found.  */
  hashcode = TYPE_HASH (basetype) + TYPE_HASH (rettype) + type_hash_list (argtypes);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);

  return t;
}

tree
build_cplus_array_type (elt_type, index_type)
     tree elt_type;
     tree index_type;
{
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;
  tree t;

  /* We need a new one.  If both ELT_TYPE and INDEX_TYPE are permanent,
     make this permanent too.  */
  if (TREE_PERMANENT (elt_type)
      && (index_type == 0 || TREE_PERMANENT (index_type)))
    {
      current_obstack = &permanent_obstack;
      saveable_obstack = &permanent_obstack;
    }

  t = build_array_type (elt_type, index_type);

  /* Push these needs up so that initialization takes place
     more easily.  */
  TYPE_NEEDS_CONSTRUCTING (t) = TYPE_NEEDS_CONSTRUCTING (TYPE_MAIN_VARIANT (elt_type));
  TYPE_NEEDS_DESTRUCTOR (t) = TYPE_NEEDS_DESTRUCTOR (TYPE_MAIN_VARIANT (elt_type));
  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;
  return t;
}

/* Make a variant type in the proper way for C/C++, propagating qualifiers
   down to the element type of an array.  */

tree
cp_build_type_variant (type, constp, volatilep)
     tree type;
     int constp, volatilep;
{
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree real_main_variant = TYPE_MAIN_VARIANT (type);

      push_obstacks (TYPE_OBSTACK (real_main_variant),
		     TYPE_OBSTACK (real_main_variant));
      type = build_cplus_array_type (cp_build_type_variant (TREE_TYPE (type),
							    constp, volatilep),
				     TYPE_DOMAIN (type));

      /* TYPE must be on same obstack as REAL_MAIN_VARIANT.  If not,
	 make a copy.  (TYPE might have come from the hash table and
	 REAL_MAIN_VARIANT might be in some function's obstack.)  */

      if (TYPE_OBSTACK (type) != TYPE_OBSTACK (real_main_variant))
	{
	  type = copy_node (type);
	  TYPE_POINTER_TO (type) = TYPE_REFERENCE_TO (type) = 0;
	}

      TYPE_MAIN_VARIANT (type) = real_main_variant;
      pop_obstacks ();
    }
  return build_type_variant (type, constp, volatilep);
}

/* Add OFFSET to all base types of T.

   OFFSET, which is a type offset, is number of bytes.

   Note that we don't have to worry about having two paths to the
   same base type, since this type owns its association list.  */
void
propagate_binfo_offsets (binfo, offset)
     tree binfo;
     tree offset;
{
  tree binfos = BINFO_BASETYPES (binfo);
  int i, n_baselinks = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  for (i = 0; i < n_baselinks; /* note increment is done in the loop.  */)
    {
      tree base_binfo = TREE_VEC_ELT (binfos, i);

      if (TREE_VIA_VIRTUAL (base_binfo))
	i += 1;
      else
	{
	  int j;
	  tree base_binfos = BINFO_BASETYPES (base_binfo);
	  tree delta;

	  for (j = i+1; j < n_baselinks; j++)
	    if (! TREE_VIA_VIRTUAL (TREE_VEC_ELT (binfos, j)))
	      {
		/* The next basetype offset must take into account the space
		   between the classes, not just the size of each class.  */
		delta = size_binop (MINUS_EXPR,
				    BINFO_OFFSET (TREE_VEC_ELT (binfos, j)),
				    BINFO_OFFSET (base_binfo));
		break;
	      }

#if 0
	  if (BINFO_OFFSET_ZEROP (base_binfo))
	    BINFO_OFFSET (base_binfo) = offset;
	  else
	    BINFO_OFFSET (base_binfo)
	      = size_binop (PLUS_EXPR, BINFO_OFFSET (base_binfo), offset);
#else
	  BINFO_OFFSET (base_binfo) = offset;
#endif
	  if (base_binfos)
	    {
	      int k;
	      tree chain = NULL_TREE;

	      /* Now unshare the structure beneath BASE_BINFO.  */
	      for (k = TREE_VEC_LENGTH (base_binfos)-1;
		   k >= 0; k--)
		{
		  tree base_base_binfo = TREE_VEC_ELT (base_binfos, k);
		  if (! TREE_VIA_VIRTUAL (base_base_binfo))
		    TREE_VEC_ELT (base_binfos, k)
		      = make_binfo (BINFO_OFFSET (base_base_binfo),
				    base_base_binfo,
				    BINFO_VTABLE (base_base_binfo),
				    BINFO_VIRTUALS (base_base_binfo),
				    chain);
		  chain = TREE_VEC_ELT (base_binfos, k);
		  TREE_VIA_PUBLIC (chain) = TREE_VIA_PUBLIC (base_base_binfo);
		  TREE_VIA_PROTECTED (chain) = TREE_VIA_PROTECTED (base_base_binfo);
		  BINFO_INHERITANCE_CHAIN (chain) = base_binfo;
		}
	      /* Now propagate the offset to the base types.  */
	      propagate_binfo_offsets (base_binfo, offset);
	    }

	  /* Go to our next class that counts for offset propagation.  */
	  i = j;
	  if (i < n_baselinks)
	    offset = size_binop (PLUS_EXPR, offset, delta);
	}
    }
}

/* Compute the actual offsets that our virtual base classes
   will have *for this type*.  This must be performed after
   the fields are laid out, since virtual baseclasses must
   lay down at the end of the record.

   Returns the maximum number of virtual functions any of the virtual
   baseclasses provide.  */
int
layout_vbasetypes (rec, max)
     tree rec;
     int max;
{
  /* Get all the virtual base types that this type uses.
     The TREE_VALUE slot holds the virtual baseclass type.  */
  tree vbase_types = get_vbase_types (rec);

#ifdef STRUCTURE_SIZE_BOUNDARY
  unsigned record_align = MAX (STRUCTURE_SIZE_BOUNDARY, TYPE_ALIGN (rec));
#else
  unsigned record_align = MAX (BITS_PER_UNIT, TYPE_ALIGN (rec));
#endif
  int desired_align;

  /* Record size so far is CONST_SIZE + VAR_SIZE bits,
     where CONST_SIZE is an integer
     and VAR_SIZE is a tree expression.
     If VAR_SIZE is null, the size is just CONST_SIZE.
     Naturally we try to avoid using VAR_SIZE.  */
  register unsigned const_size = 0;
  register tree var_size = 0;
  int nonvirtual_const_size;
  tree nonvirtual_var_size;

  CLASSTYPE_VBASECLASSES (rec) = vbase_types;

  if (TREE_CODE (TYPE_SIZE (rec)) == INTEGER_CST)
    const_size = TREE_INT_CST_LOW (TYPE_SIZE (rec));
  else
    var_size = TYPE_SIZE (rec);

  nonvirtual_const_size = const_size;
  nonvirtual_var_size = var_size;

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
	  const_size = CEIL (const_size, TYPE_ALIGN (basetype))
	    * TYPE_ALIGN (basetype);
	  offset = size_int (CEIL (const_size, BITS_PER_UNIT));
	}

      if (CLASSTYPE_VSIZE (basetype) > max)
	max = CLASSTYPE_VSIZE (basetype);
      BINFO_OFFSET (vbase_types) = offset;

      if (TREE_CODE (TYPE_SIZE (basetype)) == INTEGER_CST)
	{
	  /* Every virtual baseclass takes a least a UNIT, so that we can
	     take it's address and get something different for each base.  */
	  const_size += MAX (BITS_PER_UNIT,
			     TREE_INT_CST_LOW (TYPE_SIZE (basetype))
			     - TREE_INT_CST_LOW (CLASSTYPE_VBASE_SIZE (basetype)));
	}
      else if (var_size == 0)
	var_size = TYPE_SIZE (basetype);
      else
	var_size = size_binop (PLUS_EXPR, var_size, TYPE_SIZE (basetype));

      vbase_types = TREE_CHAIN (vbase_types);
    }

  if (const_size)
    {
      /* Because a virtual base might take a single byte above,
	 we have to re-adjust the total size to make sure it it
	 a multiple of the alignment.  */
      /* Give the whole object the alignment it wants.  */
      const_size = CEIL (const_size, record_align) * record_align;
    }

  /* Set the alignment in the complete type.  We don't set CLASSTYPE_ALIGN
   here, as that is for this class, without any virtual base classes.  */
  TYPE_ALIGN (rec) = record_align;
  if (const_size != nonvirtual_const_size)
    {
      CLASSTYPE_VBASE_SIZE (rec)
	= size_int (const_size - nonvirtual_const_size);
      TYPE_SIZE (rec) = size_int (const_size);
    }

  /* Now propagate offset information throughout the lattice
     under the vbase type.  */
  for (vbase_types = CLASSTYPE_VBASECLASSES (rec); vbase_types;
       vbase_types = TREE_CHAIN (vbase_types))
    {
      tree base_binfos = BINFO_BASETYPES (vbase_types);

      BINFO_INHERITANCE_CHAIN (vbase_types) = TYPE_BINFO (rec);

      if (base_binfos)
	{
	  tree chain = NULL_TREE;
	  int j;
	  /* Now unshare the structure beneath BASE_BINFO.  */

	  for (j = TREE_VEC_LENGTH (base_binfos)-1;
	       j >= 0; j--)
	    {
	      tree base_base_binfo = TREE_VEC_ELT (base_binfos, j);
	      if (! TREE_VIA_VIRTUAL (base_base_binfo))
		TREE_VEC_ELT (base_binfos, j)
		  = make_binfo (BINFO_OFFSET (base_base_binfo),
				base_base_binfo,
				BINFO_VTABLE (base_base_binfo),
				BINFO_VIRTUALS (base_base_binfo),
				chain);
	      chain = TREE_VEC_ELT (base_binfos, j);
	      TREE_VIA_PUBLIC (chain) = TREE_VIA_PUBLIC (base_base_binfo);
	      TREE_VIA_PROTECTED (chain) = TREE_VIA_PROTECTED (base_base_binfo);
	      BINFO_INHERITANCE_CHAIN (chain) = vbase_types;
	    }

	  propagate_binfo_offsets (vbase_types, BINFO_OFFSET (vbase_types));
	}
    }

  return max;
}

/* Lay out the base types of a record type, REC.
   Tentatively set the size and alignment of REC
   according to the base types alone.

   Offsets for immediate nonvirtual baseclasses are also computed here.

   TYPE_BINFO (REC) should be NULL_TREE on entry, and this routine
   creates a list of base_binfos in TYPE_BINFO (REC) from BINFOS.

   Returns list of virtual base classes in a FIELD_DECL chain.  */
tree
layout_basetypes (rec, binfos)
     tree rec, binfos;
{
  /* Chain to hold all the new FIELD_DECLs which point at virtual
     base classes.  */
  tree vbase_decls = NULL_TREE;

#ifdef STRUCTURE_SIZE_BOUNDARY
  unsigned record_align = MAX (STRUCTURE_SIZE_BOUNDARY, TYPE_ALIGN (rec));
#else
  unsigned record_align = MAX (BITS_PER_UNIT, TYPE_ALIGN (rec));
#endif

  /* Record size so far is CONST_SIZE + VAR_SIZE bits, where CONST_SIZE is
     an integer and VAR_SIZE is a tree expression.  If VAR_SIZE is null,
     the size is just CONST_SIZE.  Naturally we try to avoid using
     VAR_SIZE.  And so far, we've been successful. */
#if 0
  register tree var_size = 0;
#endif

  register unsigned const_size = 0;
  int i, n_baseclasses = binfos ? TREE_VEC_LENGTH (binfos) : 0;

  /* Handle basetypes almost like fields, but record their
     offsets differently.  */

  for (i = 0; i < n_baseclasses; i++)
    {
      int inc, desired_align, int_vbase_size;
      register tree base_binfo = TREE_VEC_ELT (binfos, i);
      register tree basetype = BINFO_TYPE (base_binfo);
      tree decl, offset;

      if (TYPE_SIZE (basetype) == 0)
	{
#if 0
	  /* This error is now reported in xref_tag, thus giving better
	     location information.  */
	  error_with_aggr_type (base_binfo,
				"base class `%s' has incomplete type");

	  TREE_VIA_PUBLIC (base_binfo) = 1;
	  TREE_VIA_PROTECTED (base_binfo) = 0;
	  TREE_VIA_VIRTUAL (base_binfo) = 0;

	  /* Should handle this better so that

	     class A;
	     class B: private A { virtual void F(); };

	     does not dump core when compiled. */
	  my_friendly_abort (121);
#endif
	  continue;
	}

      /* All basetypes are recorded in the association list of the
	 derived type.  */

      if (TREE_VIA_VIRTUAL (base_binfo))
	{
	  int j;
	  char *name = (char *)alloca (TYPE_NAME_LENGTH (basetype)
				       + sizeof (VBASE_NAME) + 1);

	  /* The offset for a virtual base class is only used in computing
	     virtual function tables and for initializing virtual base
	     pointers.  It is built once `get_vbase_types' is called.  */

	  /* If this basetype can come from another vbase pointer
	     without an additional indirection, we will share
	     that pointer.  If an indirection is involved, we
	     make our own pointer.  */
	  for (j = 0; j < n_baseclasses; j++)
	    {
	      tree other_base_binfo = TREE_VEC_ELT (binfos, j);
	      if (! TREE_VIA_VIRTUAL (other_base_binfo)
		  && binfo_member (basetype,
				   CLASSTYPE_VBASECLASSES (BINFO_TYPE (other_base_binfo))))
		goto got_it;
	    }
	  sprintf (name, VBASE_NAME_FORMAT, TYPE_NAME_STRING (basetype));
	  decl = build_lang_decl (FIELD_DECL, get_identifier (name),
				  build_pointer_type (basetype));
	  /* If you change any of the below, take a look at all the
	     other VFIELD_BASEs and VTABLE_BASEs in the code, and change
	     them too. */
	  DECL_ASSEMBLER_NAME (decl) = get_identifier (VTABLE_BASE);
	  DECL_VIRTUAL_P (decl) = 1;
	  DECL_FIELD_CONTEXT (decl) = rec;
	  DECL_CLASS_CONTEXT (decl) = rec;
	  DECL_FCONTEXT (decl) = basetype;
	  DECL_SAVED_INSNS (decl) = NULL_RTX;
	  DECL_FIELD_SIZE (decl) = 0;
	  DECL_ALIGN (decl) = TYPE_ALIGN (ptr_type_node);
	  TREE_CHAIN (decl) = vbase_decls;
	  BINFO_VPTR_FIELD (base_binfo) = decl;
	  vbase_decls = decl;

	  if (warn_nonvdtor && TYPE_HAS_DESTRUCTOR (basetype)
	      && DECL_VINDEX (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), 0)) == NULL_TREE)
	    {
	      warning_with_decl (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), 0),
				 "destructor `%s' non-virtual");
	      warning ("in inheritance relationship `%s: virtual %s'",
		       TYPE_NAME_STRING (rec),
		       TYPE_NAME_STRING (basetype));
	    }
	got_it:
	  /* The space this decl occupies has already been accounted for.  */
	  continue;
	}

      if (const_size == 0)
	offset = integer_zero_node;
      else
	{
	  /* Give each base type the alignment it wants.  */
	  const_size = CEIL (const_size, TYPE_ALIGN (basetype))
	    * TYPE_ALIGN (basetype);
	  offset = size_int ((const_size + BITS_PER_UNIT - 1) / BITS_PER_UNIT);

#if 0
	  /* bpk: Disabled this check until someone is willing to
	     claim it as theirs and explain exactly what circumstances
	     warrant the warning.  */ 
	  if (warn_nonvdtor && TYPE_HAS_DESTRUCTOR (basetype)
	      && DECL_VINDEX (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), 0)) == NULL_TREE)
	    {
	      warning_with_decl (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), 0),
				 "destructor `%s' non-virtual");
	      warning ("in inheritance relationship `%s:%s %s'",
		       TYPE_NAME_STRING (rec),
		       TREE_VIA_VIRTUAL (base_binfo) ? " virtual" : "",
		       TYPE_NAME_STRING (basetype));
	    }
#endif
	}
      BINFO_OFFSET (base_binfo) = offset;
      if (CLASSTYPE_VSIZE (basetype))
	{
	  BINFO_VTABLE (base_binfo) = TYPE_BINFO_VTABLE (basetype);
	  BINFO_VIRTUALS (base_binfo) = TYPE_BINFO_VIRTUALS (basetype);
	}
      TREE_CHAIN (base_binfo) = TYPE_BINFO (rec);
      TYPE_BINFO (rec) = base_binfo;

      /* Add only the amount of storage not present in
	 the virtual baseclasses.  */

      int_vbase_size = TREE_INT_CST_LOW (CLASSTYPE_VBASE_SIZE (basetype));
      if (TREE_INT_CST_LOW (TYPE_SIZE (basetype)) > int_vbase_size)
	{
	  inc = MAX (record_align,
		     (TREE_INT_CST_LOW (TYPE_SIZE (basetype))
		      - int_vbase_size));

	  /* Record must have at least as much alignment as any field.  */
	  desired_align = TYPE_ALIGN (basetype);
	  record_align = MAX (record_align, desired_align);

	  const_size += inc;
	}
    }

  if (const_size)
    CLASSTYPE_SIZE (rec) = size_int (const_size);
  else
    CLASSTYPE_SIZE (rec) = integer_zero_node;
  CLASSTYPE_ALIGN (rec) = record_align;

  return vbase_decls;
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
struct list_hash *list_hash_table[TYPE_HASH_SIZE];

/* Compute a hash code for a list (chain of TREE_LIST nodes
   with goodies in the TREE_PURPOSE, TREE_VALUE, and bits of the
   TREE_COMMON slots), by adding the hash codes of the individual entries.  */

int
list_hash (list)
     tree list;
{
  register int hashcode = 0;

  if (TREE_CHAIN (list))
    hashcode += TYPE_HASH (TREE_CHAIN (list));

  if (TREE_VALUE (list))
    hashcode += TYPE_HASH (TREE_VALUE (list));
  else
    hashcode += 1007;
  if (TREE_PURPOSE (list))
    hashcode += TYPE_HASH (TREE_PURPOSE (list));
  else
    hashcode += 1009;
  return hashcode;
}

/* Look in the type hash table for a type isomorphic to TYPE.
   If one is found, return it.  Otherwise return 0.  */

tree
list_hash_lookup (hashcode, list)
     int hashcode;
     tree list;
{
  register struct list_hash *h;
  for (h = list_hash_table[hashcode % TYPE_HASH_SIZE]; h; h = h->next)
    if (h->hashcode == hashcode
	&& TREE_VIA_VIRTUAL (h->list) == TREE_VIA_VIRTUAL (list)
	&& TREE_VIA_PUBLIC (h->list) == TREE_VIA_PUBLIC (list)
	&& TREE_VIA_PROTECTED (h->list) == TREE_VIA_PROTECTED (list)
	&& TREE_PURPOSE (h->list) == TREE_PURPOSE (list)
	&& TREE_VALUE (h->list) == TREE_VALUE (list)
	&& TREE_CHAIN (h->list) == TREE_CHAIN (list))
      {
	my_friendly_assert (TREE_TYPE (h->list) == TREE_TYPE (list), 299);
	return h->list;
      }
  return 0;
}

/* Add an entry to the list-hash-table
   for a list TYPE whose hash code is HASHCODE.  */

void
list_hash_add (hashcode, list)
     int hashcode;
     tree list;
{
  register struct list_hash *h;

  h = (struct list_hash *) obstack_alloc (&class_obstack, sizeof (struct list_hash));
  h->hashcode = hashcode;
  h->list = list;
  h->next = list_hash_table[hashcode % TYPE_HASH_SIZE];
  list_hash_table[hashcode % TYPE_HASH_SIZE] = h;
}

/* Given TYPE, and HASHCODE its hash code, return the canonical
   object for an identical list if one already exists.
   Otherwise, return TYPE, and record it as the canonical object
   if it is a permanent object.

   To use this function, first create a list of the sort you want.
   Then compute its hash code from the fields of the list that
   make it different from other similar lists.
   Then call this function and use the value.
   This function frees the list you pass in if it is a duplicate.  */

/* Set to 1 to debug without canonicalization.  Never set by program.  */
static int debug_no_list_hash = 0;

tree
list_hash_canon (hashcode, list)
     int hashcode;
     tree list;
{
  tree t1;

  if (debug_no_list_hash)
    return list;

  t1 = list_hash_lookup (hashcode, list);
  if (t1 != 0)
    {
      obstack_free (&class_obstack, list);
      return t1;
    }

  /* If this is a new list, record it for later reuse.  */
  list_hash_add (hashcode, list);

  return list;
}

tree
hash_tree_cons (via_public, via_virtual, via_protected, purpose, value, chain)
     int via_public, via_virtual, via_protected;
     tree purpose, value, chain;
{
  struct obstack *ambient_obstack = current_obstack;
  tree t;
  int hashcode;

  current_obstack = &class_obstack;
  t = tree_cons (purpose, value, chain);
  TREE_VIA_PUBLIC (t) = via_public;
  TREE_VIA_PROTECTED (t) = via_protected;
  TREE_VIA_VIRTUAL (t) = via_virtual;
  hashcode = list_hash (t);
  t = list_hash_canon (hashcode, t);
  current_obstack = ambient_obstack;
  return t;
}

/* Constructor for hashed lists.  */
tree
hash_tree_chain (value, chain)
     tree value, chain;
{
  struct obstack *ambient_obstack = current_obstack;
  tree t;
  int hashcode;

  current_obstack = &class_obstack;
  t = tree_cons (NULL_TREE, value, chain);
  hashcode = list_hash (t);
  t = list_hash_canon (hashcode, t);
  current_obstack = ambient_obstack;
  return t;
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

static tree
get_identifier_list (value)
     tree value;
{
  tree list = IDENTIFIER_AS_LIST (value);
  if (list != NULL_TREE
      && (TREE_CODE (list) != TREE_LIST
	  || TREE_VALUE (list) != value))
    list = NULL_TREE;
  else if (IDENTIFIER_HAS_TYPE_VALUE (value)
	   && TREE_CODE (IDENTIFIER_TYPE_VALUE (value)) == RECORD_TYPE
	   && IDENTIFIER_TYPE_VALUE (value)
	      == TYPE_MAIN_VARIANT (IDENTIFIER_TYPE_VALUE (value)))
    {
      tree type = IDENTIFIER_TYPE_VALUE (value);

      if (TYPE_PTRMEMFUNC_P (type))
	list = NULL_TREE;
      else if (type == current_class_type)
	/* Don't mess up the constructor name.  */
	list = tree_cons (NULL_TREE, value, NULL_TREE);
      else
	{
	  register tree id;
	  /* This will return the correct thing for regular types,
	     nested types, and templates.  Yay! */
	  if (TYPE_NESTED_NAME (type))
	    id = TYPE_NESTED_NAME (type);
	  else
	    id = TYPE_IDENTIFIER (type);

	  if (CLASSTYPE_ID_AS_LIST (type) == NULL_TREE)
	    CLASSTYPE_ID_AS_LIST (type)
	      = perm_tree_cons (NULL_TREE, id, NULL_TREE);
	  list = CLASSTYPE_ID_AS_LIST (type);
	}
    }
  return list;
}

tree
get_decl_list (value)
     tree value;
{
  tree list = NULL_TREE;

  if (TREE_CODE (value) == IDENTIFIER_NODE)
    list = get_identifier_list (value);
  else if (TREE_CODE (value) == RECORD_TYPE
	   && TYPE_LANG_SPECIFIC (value))
    list = CLASSTYPE_AS_LIST (value);

  if (list != NULL_TREE)
    {
      my_friendly_assert (TREE_CHAIN (list) == NULL_TREE, 301);
      return list;
    }

  return build_decl_list (NULL_TREE, value);
}

/* Look in the type hash table for a type isomorphic to
   `build_tree_list (NULL_TREE, VALUE)'.
   If one is found, return it.  Otherwise return 0.  */

tree
list_hash_lookup_or_cons (value)
     tree value;
{
  register int hashcode = TYPE_HASH (value);
  register struct list_hash *h;
  struct obstack *ambient_obstack;
  tree list = NULL_TREE;

  if (TREE_CODE (value) == IDENTIFIER_NODE)
    list = get_identifier_list (value);
  else if (TREE_CODE (value) == TYPE_DECL
	   && TREE_CODE (TREE_TYPE (value)) == RECORD_TYPE
	   && TYPE_LANG_SPECIFIC (TREE_TYPE (value)))
    list = CLASSTYPE_ID_AS_LIST (TREE_TYPE (value));
  else if (TREE_CODE (value) == RECORD_TYPE
	   && TYPE_LANG_SPECIFIC (value))
    list = CLASSTYPE_AS_LIST (value);

  if (list != NULL_TREE)
    {
      my_friendly_assert (TREE_CHAIN (list) == NULL_TREE, 302);
      return list;
    }

  if (debug_no_list_hash)
    return hash_tree_chain (value, NULL_TREE);

  for (h = list_hash_table[hashcode % TYPE_HASH_SIZE]; h; h = h->next)
    if (h->hashcode == hashcode
	&& TREE_VIA_VIRTUAL (h->list) == 0
	&& TREE_VIA_PUBLIC (h->list) == 0
	&& TREE_VIA_PROTECTED (h->list) == 0
	&& TREE_PURPOSE (h->list) == 0
	&& TREE_VALUE (h->list) == value)
      {
	my_friendly_assert (TREE_TYPE (h->list) == 0, 303);
	my_friendly_assert (TREE_CHAIN (h->list) == 0, 304);
	return h->list;
      }

  ambient_obstack = current_obstack;
  current_obstack = &class_obstack;
  list = build_tree_list (NULL_TREE, value);
  list_hash_add (hashcode, list);
  current_obstack = ambient_obstack;
  return list;
}

/* Build an association between TYPE and some parameters:

   OFFSET is the offset added to `this' to convert it to a pointer
   of type `TYPE *'

   BINFO is the base binfo to use, if we are deriving from one.  This
   is necessary, as we want specialized parent binfos from base
   classes, so that the VTABLE_NAMEs of bases are for the most derived
   type, instead of of the simple type.

   VTABLE is the virtual function table with which to initialize
   sub-objects of type TYPE.

   VIRTUALS are the virtual functions sitting in VTABLE.

   CHAIN are more associations we must retain.  */

tree
make_binfo (offset, binfo, vtable, virtuals, chain)
     tree offset, binfo;
     tree vtable, virtuals;
     tree chain;
{
  tree new_binfo = make_tree_vec (6);
  tree type;

  if (TREE_CODE (binfo) == TREE_VEC)
    type = BINFO_TYPE (binfo);
  else
    {
      type = binfo;
      binfo = TYPE_BINFO (binfo);
    }

  TREE_CHAIN (new_binfo) = chain;
  if (chain)
    TREE_USED (new_binfo) = TREE_USED (chain);

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

tree
reverse_path (path)
     tree path;
{
  register tree prev = 0, tmp, next;
  for (tmp = path; tmp; tmp = next)
    {
      next = BINFO_INHERITANCE_CHAIN (tmp);
      BINFO_INHERITANCE_CHAIN (tmp) = prev;
      prev = tmp;
    }
  return prev;
}

tree
virtual_member (elem, list)
     tree elem;
     tree list;
{
  tree t;
  tree rval, nval;

  for (t = list; t; t = TREE_CHAIN (t))
    if (elem == BINFO_TYPE (t))
      return t;
  rval = 0;
  for (t = list; t; t = TREE_CHAIN (t))
    {
      tree binfos = BINFO_BASETYPES (t);
      int i;

      if (binfos != NULL_TREE)
	for (i = TREE_VEC_LENGTH (binfos)-1; i >= 0; i--)
	  {
	    nval = binfo_value (elem, BINFO_TYPE (TREE_VEC_ELT (binfos, i)));
	    if (nval)
	      {
		if (rval && BINFO_OFFSET (nval) != BINFO_OFFSET (rval))
		  my_friendly_abort (104);
		rval = nval;
	      }
	  }
    }
  return rval;
}

void
debug_binfo (elem)
     tree elem;
{
  unsigned HOST_WIDE_INT n;
  tree virtuals;

  fprintf (stderr, "type \"%s\"; offset = %d\n",
	   TYPE_NAME_STRING (BINFO_TYPE (elem)),
	   TREE_INT_CST_LOW (BINFO_OFFSET (elem)));
  fprintf (stderr, "vtable type:\n");
  debug_tree (BINFO_TYPE (elem));
  if (BINFO_VTABLE (elem))
    fprintf (stderr, "vtable decl \"%s\"\n", IDENTIFIER_POINTER (DECL_NAME (BINFO_VTABLE (elem))));
  else
    fprintf (stderr, "no vtable decl yet\n");
  fprintf (stderr, "virtuals:\n");
  virtuals = BINFO_VIRTUALS (elem);

  n = skip_rtti_stuff (&virtuals);

  while (virtuals)
    {
      tree fndecl = TREE_OPERAND (FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (virtuals)), 0);
      fprintf (stderr, "%s [%d =? %d]\n",
	       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl)),
	       n, TREE_INT_CST_LOW (DECL_VINDEX (fndecl)));
      ++n;
      virtuals = TREE_CHAIN (virtuals);
    }
}

/* Return the length of a chain of nodes chained through DECL_CHAIN.
   We expect a null pointer to mark the end of the chain.
   This is the Lisp primitive `length'.  */

int
decl_list_length (t)
     tree t;
{
  register tree tail;
  register int len = 0;

  my_friendly_assert (TREE_CODE (t) == FUNCTION_DECL
		      || TREE_CODE (t) == TEMPLATE_DECL, 300);
  for (tail = t; tail; tail = DECL_CHAIN (tail))
    len++;

  return len;
}

int
count_functions (t)
     tree t;
{
  if (TREE_CODE (t) == FUNCTION_DECL)
    return 1;
  else if (TREE_CODE (t) == TREE_LIST)
    return decl_list_length (TREE_VALUE (t));

  my_friendly_abort (359);
  return 0;
}

/* Like value_member, but for DECL_CHAINs.  */
tree
decl_value_member (elem, list)
     tree elem, list;
{
  while (list)
    {
      if (elem == list)
	return list;
      list = DECL_CHAIN (list);
    }
  return NULL_TREE;
}

int
is_overloaded_fn (x)
     tree x;
{
  if (TREE_CODE (x) == FUNCTION_DECL)
    return 1;

  if (TREE_CODE (x) == TREE_LIST
      && (TREE_CODE (TREE_VALUE (x)) == FUNCTION_DECL
	  || TREE_CODE (TREE_VALUE (x)) == TEMPLATE_DECL))
    return 1;

  return 0;
}

int
really_overloaded_fn (x)
     tree x;
{     
  if (TREE_CODE (x) == TREE_LIST
      && (TREE_CODE (TREE_VALUE (x)) == FUNCTION_DECL
	  || TREE_CODE (TREE_VALUE (x)) == TEMPLATE_DECL))
    return 1;

  return 0;
}

tree
get_first_fn (from)
     tree from;
{
  if (TREE_CODE (from) == FUNCTION_DECL)
    return from;

  my_friendly_assert (TREE_CODE (from) == TREE_LIST, 9);
  
  return TREE_VALUE (from);
}

tree
fnaddr_from_vtable_entry (entry)
     tree entry;
{
  if (flag_vtable_thunks)
    {
      tree func = entry;
      if (TREE_CODE (func) == ADDR_EXPR)
	func = TREE_OPERAND (func, 0);
      if (TREE_CODE (func) == THUNK_DECL)
	return DECL_INITIAL (func);
      else
	return entry;
    }
  else
    return TREE_VALUE (TREE_CHAIN (TREE_CHAIN (CONSTRUCTOR_ELTS (entry))));
}

void
set_fnaddr_from_vtable_entry (entry, value)
     tree entry, value;
{
  if (flag_vtable_thunks)
    abort ();
  else
  TREE_VALUE (TREE_CHAIN (TREE_CHAIN (CONSTRUCTOR_ELTS (entry)))) = value;
}

tree
function_arg_chain (t)
     tree t;
{
  return TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (t)));
}

int
promotes_to_aggr_type (t, code)
     tree t;
     enum tree_code code;
{
  if (TREE_CODE (t) == code)
    t = TREE_TYPE (t);
  return IS_AGGR_TYPE (t);
}

int
is_aggr_type_2 (t1, t2)
     tree t1, t2;
{
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return 0;
  return IS_AGGR_TYPE (t1) && IS_AGGR_TYPE (t2);
}

/* Give message using types TYPE1 and TYPE2 as arguments.
   PFN is the function which will print the message;
   S is the format string for PFN to use.  */
void
message_2_types (pfn, s, type1, type2)
     void (*pfn) ();
     char *s;
     tree type1, type2;
{
  tree name1 = TYPE_NAME (type1);
  tree name2 = TYPE_NAME (type2);
  if (TREE_CODE (name1) == TYPE_DECL)
    name1 = DECL_NAME (name1);
  if (TREE_CODE (name2) == TYPE_DECL)
    name2 = DECL_NAME (name2);
  (*pfn) (s, IDENTIFIER_POINTER (name1), IDENTIFIER_POINTER (name2));
}

#define PRINT_RING_SIZE 4

char *
lang_printable_name (decl)
     tree decl;
{
  static tree decl_ring[PRINT_RING_SIZE];
  static char *print_ring[PRINT_RING_SIZE];
  static int ring_counter;
  int i;

  /* Only cache functions.  */
  if (TREE_CODE (decl) != FUNCTION_DECL
      || DECL_LANG_SPECIFIC (decl) == 0)
    return decl_as_string (decl, 1);

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

  {
    int print_ret_type_p
      = (!DECL_CONSTRUCTOR_P (decl)
	 && !DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (decl)));

    char *name = (char *)decl_as_string (decl, print_ret_type_p);
    print_ring[ring_counter] = (char *)malloc (strlen (name) + 1);
    strcpy (print_ring[ring_counter], name);
    decl_ring[ring_counter] = decl;
  }
  return print_ring[ring_counter];
}

/* Comparison function for sorting identifiers in RAISES lists.
   Note that because IDENTIFIER_NODEs are unique, we can sort
   them by address, saving an indirection.  */
static int
id_cmp (p1, p2)
     tree *p1, *p2;
{
  return (HOST_WIDE_INT)TREE_VALUE (*p1) - (HOST_WIDE_INT)TREE_VALUE (*p2);
}

/* Build the FUNCTION_TYPE or METHOD_TYPE which may throw exceptions
   listed in RAISES.  */
tree
build_exception_variant (type, raises)
     tree type;
     tree raises;
{
  int i;
  tree v = TYPE_MAIN_VARIANT (type);
  tree t, t2, cname;
  tree *a = (tree *)alloca ((list_length (raises)+1) * sizeof (tree));
  int constp = TYPE_READONLY (type);
  int volatilep = TYPE_VOLATILE (type);

  for (v = TYPE_NEXT_VARIANT (v); v; v = TYPE_NEXT_VARIANT (v))
    {
      if (TYPE_READONLY (v) != constp
	  || TYPE_VOLATILE (v) != volatilep)
	continue;

      /* @@ This should do set equality, not exact match. */
      if (simple_cst_list_equal (TYPE_RAISES_EXCEPTIONS (v), raises))
	/* List of exceptions raised matches previously found list.

	   @@ Nice to free up storage used in consing up the
	   @@ list of exceptions raised.  */
	return v;
    }

  /* Need to build a new variant.  */
  v = copy_node (type);
  TYPE_NEXT_VARIANT (v) = TYPE_NEXT_VARIANT (type);
  TYPE_NEXT_VARIANT (type) = v;
  if (raises && ! TREE_PERMANENT (raises))
    {
      push_obstacks_nochange ();
      end_temporary_allocation ();
      raises = copy_list (raises);
      pop_obstacks ();
    }
  TYPE_RAISES_EXCEPTIONS (v) = raises;
  return v;
}

/* Subroutine of copy_to_permanent

   Assuming T is a node build bottom-up, make it all exist on
   permanent obstack, if it is not permanent already.  */

tree
mapcar (t, func)
     tree t;
     tree (*func)();
{
  enum tree_code code;
  tree tmp;

  if (t == NULL_TREE)
    return t;

  if (tmp = func (t), tmp != NULL_TREE)
    return tmp;

  switch (code = TREE_CODE (t))
    {
    case ERROR_MARK:
      return error_mark_node;

    case VAR_DECL:
    case FUNCTION_DECL:
    case CONST_DECL:
      break;

    case PARM_DECL:
      {
	tree chain = TREE_CHAIN (t);
	t = copy_node (t);
	TREE_CHAIN (t) = mapcar (chain, func);
	TREE_TYPE (t) = mapcar (TREE_TYPE (t), func);
	DECL_INITIAL (t) = mapcar (DECL_INITIAL (t), func);
	DECL_SIZE (t) = mapcar (DECL_SIZE (t), func);
	return t;
      }

    case TREE_LIST:
      {
	tree chain = TREE_CHAIN (t);
	t = copy_node (t);
	TREE_PURPOSE (t) = mapcar (TREE_PURPOSE (t), func);
	TREE_VALUE (t) = mapcar (TREE_VALUE (t), func);
	TREE_CHAIN (t) = mapcar (chain, func);
	return t;
      }

    case TREE_VEC:
      {
	int len = TREE_VEC_LENGTH (t);

	t = copy_node (t);
	while (len--)
	  TREE_VEC_ELT (t, len) = mapcar (TREE_VEC_ELT (t, len), func);
	return t;
      }

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
      return copy_node (t);

    case COND_EXPR:
    case TARGET_EXPR:
    case NEW_EXPR:
      t = copy_node (t);
      TREE_OPERAND (t, 0) = mapcar (TREE_OPERAND (t, 0), func);
      TREE_OPERAND (t, 1) = mapcar (TREE_OPERAND (t, 1), func);
      TREE_OPERAND (t, 2) = mapcar (TREE_OPERAND (t, 2), func);
      return t;

    case SAVE_EXPR:
      t = copy_node (t);
      TREE_OPERAND (t, 0) = mapcar (TREE_OPERAND (t, 0), func);
      return t;

    case MODIFY_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case COMPOUND_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case CALL_EXPR:
      t = copy_node (t);
      TREE_OPERAND (t, 0) = mapcar (TREE_OPERAND (t, 0), func);
      TREE_OPERAND (t, 1) = mapcar (TREE_OPERAND (t, 1), func);
      return t;

    case CONVERT_EXPR:
    case ADDR_EXPR:
    case INDIRECT_REF:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case NOP_EXPR:
    case COMPONENT_REF:
      t = copy_node (t);
      TREE_OPERAND (t, 0) = mapcar (TREE_OPERAND (t, 0), func);
      return t;

    case POINTER_TYPE:
      return build_pointer_type (mapcar (TREE_TYPE (t), func));
    case REFERENCE_TYPE:
      return build_reference_type (mapcar (TREE_TYPE (t), func));
    case FUNCTION_TYPE:
      return build_function_type (mapcar (TREE_TYPE (t), func),
				  mapcar (TYPE_ARG_TYPES (t), func));
    case ARRAY_TYPE:
      return build_array_type (mapcar (TREE_TYPE (t), func),
			       mapcar (TYPE_DOMAIN (t), func));
    case INTEGER_TYPE:
      return build_index_type (mapcar (TYPE_MAX_VALUE (t), func));

    case OFFSET_TYPE:
      return build_offset_type (mapcar (TYPE_OFFSET_BASETYPE (t), func),
				mapcar (TREE_TYPE (t), func));
    case METHOD_TYPE:
      return build_method_type
	(mapcar (TYPE_METHOD_BASETYPE (t), func),
	 build_function_type
	 (mapcar (TREE_TYPE (t), func),
	  mapcar (TREE_CHAIN (TYPE_ARG_TYPES (t)), func)));

    case RECORD_TYPE:
      if (TYPE_PTRMEMFUNC_P (t))
	return build_ptrmemfunc_type
	  (mapcar (TYPE_PTRMEMFUNC_FN_TYPE (t), func));
      /* else fall through */
      
      /*  This list is incomplete, but should suffice for now.
	  It is very important that `sorry' does not call
	  `report_error_function'.  That could cause an infinite loop.  */
    default:
      sorry ("initializer contains unrecognized tree code");
      return error_mark_node;

    }
  my_friendly_abort (107);
  /* NOTREACHED */
  return NULL_TREE;
}

static tree
perm_manip (t)
     tree t;
{
  if (TREE_PERMANENT (t))
    return t;
  return NULL_TREE;
}

/* Assuming T is a node built bottom-up, make it all exist on
   permanent obstack, if it is not permanent already.  */
tree
copy_to_permanent (t)
     tree t;
{
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;
  int resume;

  if (t == NULL_TREE || TREE_PERMANENT (t))
    return t;

  saveable_obstack = &permanent_obstack;
  current_obstack = saveable_obstack;
  resume = suspend_momentary ();

  t = mapcar (t, perm_manip);

  resume_momentary (resume);
  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;

  return t;
}

void
print_lang_statistics ()
{
  extern struct obstack maybepermanent_obstack;
  print_obstack_statistics ("class_obstack", &class_obstack);
  print_obstack_statistics ("permanent_obstack", &permanent_obstack);
  print_obstack_statistics ("maybepermanent_obstack", &maybepermanent_obstack);
  print_search_statistics ();
  print_class_statistics ();
}

/* This is used by the `assert' macro.  It is provided in libgcc.a,
   which `cc' doesn't know how to link.  Note that the C++ front-end
   no longer actually uses the `assert' macro (instead, it calls
   my_friendly_assert).  But all of the back-end files still need this.  */
void
__eprintf (string, expression, line, filename)
#ifdef __STDC__
     const char *string;
     const char *expression;
     unsigned line;
     const char *filename;
#else
     char *string;
     char *expression;
     unsigned line;
     char *filename;
#endif
{
  fprintf (stderr, string, expression, line, filename);
  fflush (stderr);
  abort ();
}

/* Return, as an INTEGER_CST node, the number of elements for
   TYPE (which is an ARRAY_TYPE).  This counts only elements of the top array. */

tree
array_type_nelts_top (type)
     tree type;
{
  return fold (build (PLUS_EXPR, sizetype,
		      array_type_nelts (type),
		      integer_one_node));
}

/* Return, as an INTEGER_CST node, the number of elements for
   TYPE (which is an ARRAY_TYPE).  This one is a recursive count of all
   ARRAY_TYPEs that are clumped together. */

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

static
tree
bot_manip (t)
     tree t;
{
  if (TREE_CODE (t) != TREE_LIST && ! TREE_SIDE_EFFECTS (t))
    return t;
  else if (TREE_CODE (t) == TARGET_EXPR)
    return build_cplus_new (TREE_TYPE (t),
			    break_out_target_exprs (TREE_OPERAND (t, 1)), 0);
  return NULL_TREE;
}
  
/* Actually, we'll just clean out the target exprs for the moment.  */
tree
break_out_target_exprs (t)
     tree t;
{
  return mapcar (t, bot_manip);
}

tree
unsave_expr (expr)
     tree expr;
{
  tree t;

  t = build1 (UNSAVE_EXPR, TREE_TYPE (expr), expr);
  TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (expr);
  return t;
}

/* Modify a tree in place so that all the evaluate only once things
   are cleared out.  Return the EXPR given.  */
tree
unsave_expr_now (expr)
     tree expr;
{
  enum tree_code code;
  register int i;

  if (expr == NULL_TREE)
    return expr;

  code = TREE_CODE (expr);
  switch (code)
    {
    case SAVE_EXPR:
      SAVE_EXPR_RTL (expr) = NULL_RTX;
      break;

    case TARGET_EXPR:
      sorry ("TARGET_EXPR reused inside UNSAVE_EXPR");
      break;
      
    case RTL_EXPR:
      warning ("RTL_EXPR reused inside UNSAVE_EXPR");
      RTL_EXPR_SEQUENCE (expr) = NULL_RTX;
      break;

    case CALL_EXPR:
      CALL_EXPR_RTL (expr) = NULL_RTX;
      if (TREE_OPERAND (expr, 1)
	  && TREE_CODE (TREE_OPERAND (expr, 1)) == TREE_LIST)
	{
	  tree exp = TREE_OPERAND (expr, 1);
	  while (exp)
	    {
	      unsave_expr_now (TREE_VALUE (exp));
	      exp = TREE_CHAIN (exp);
	    }
	}
      break;
      
    case WITH_CLEANUP_EXPR:
      warning ("WITH_CLEANUP_EXPR reused inside UNSAVE_EXPR");
      RTL_EXPR_RTL (expr) = NULL_RTX;
      break;
    }

  switch (TREE_CODE_CLASS (code))
    {
    case 'c':  /* a constant */
    case 't':  /* a type node */
    case 'x':  /* something random, like an identifier or an ERROR_MARK.  */
    case 'd':  /* A decl node */
    case 'b':  /* A block node */
      return expr;

    case 'e':  /* an expression */
    case 'r':  /* a reference */
    case 's':  /* an expression with side effects */
    case '<':  /* a comparison expression */
    case '2':  /* a binary arithmetic expression */
    case '1':  /* a unary arithmetic expression */
      for (i = tree_code_length[(int) code] - 1; i >= 0; i--)
	unsave_expr_now (TREE_OPERAND (expr, i));
      return expr;

    default:
      my_friendly_abort (999);
    }
}

/* Since cleanup may have SAVE_EXPRs in it, we protect it with an
   UNSAVE_EXPR as the backend cannot yet handle SAVE_EXPRs in cleanups
   by itself.  */
int
cp_expand_decl_cleanup (decl, cleanup)
     tree decl, cleanup;
{
  return expand_decl_cleanup (decl, unsave_expr (cleanup));
}
