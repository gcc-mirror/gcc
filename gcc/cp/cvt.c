/* Language-level data type conversion for GNU C++.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.
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


/* This file contains the functions for converting C expressions
   to different data types.  The only entry point is `convert'.
   Every language front end must have a `convert' function
   but what kind of conversions it does will depend on the language.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "flags.h"
#include "cp-tree.h"
#include "convert.h"
#include "toplev.h"
#include "decl.h"

static tree cp_convert_to_pointer PARAMS ((tree, tree));
static tree convert_to_pointer_force PARAMS ((tree, tree));
static tree build_up_reference PARAMS ((tree, tree, int));

/* Change of width--truncation and extension of integers or reals--
   is represented with NOP_EXPR.  Proper functioning of many things
   assumes that no other conversions can be NOP_EXPRs.

   Conversion between integer and pointer is represented with CONVERT_EXPR.
   Converting integer to real uses FLOAT_EXPR
   and real to integer uses FIX_TRUNC_EXPR.

   Here is a list of all the functions that assume that widening and
   narrowing is always done with a NOP_EXPR:
     In convert.c, convert_to_integer.
     In c-typeck.c, build_binary_op_nodefault (boolean ops),
        and truthvalue_conversion.
     In expr.c: expand_expr, for operands of a MULT_EXPR.
     In fold-const.c: fold.
     In tree.c: get_narrower and get_unwidened.

   C++: in multiple-inheritance, converting between pointers may involve
   adjusting them by a delta stored within the class definition.  */

/* Subroutines of `convert'.  */

/* if converting pointer to pointer
     if dealing with classes, check for derived->base or vice versa
     else if dealing with method pointers, delegate
     else convert blindly
   else if converting class, pass off to build_type_conversion
   else try C-style pointer conversion  */

static tree
cp_convert_to_pointer (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form;
  tree rval;

  if (IS_AGGR_TYPE (intype))
    {
      intype = complete_type (intype);
      if (TYPE_SIZE (intype) == NULL_TREE)
	{
	  cp_error ("can't convert from incomplete type `%T' to `%T'",
		    intype, type);
	  return error_mark_node;
	}

      rval = build_type_conversion (type, expr, 1);
      if (rval)
	{
	  if (rval == error_mark_node)
	    cp_error ("conversion of `%E' from `%T' to `%T' is ambiguous",
		      expr, intype, type);
	  return rval;
	}
    }

  /* Handle anachronistic conversions from (::*)() to cv void* or (*)().  */
  if (TREE_CODE (type) == POINTER_TYPE
      && (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE
	  || TYPE_MAIN_VARIANT (TREE_TYPE (type)) == void_type_node))
    {
      /* Allow an implicit this pointer for pointer to member
	 functions.  */
      if (TYPE_PTRMEMFUNC_P (intype))
	{
	  tree fntype = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (intype));
	  tree decl = maybe_dummy_object (TYPE_METHOD_BASETYPE (fntype), 0);
	  expr = build (OFFSET_REF, fntype, decl, expr);
	}

      if (TREE_CODE (expr) == OFFSET_REF
	  && TREE_CODE (TREE_TYPE (expr)) == METHOD_TYPE)
	expr = resolve_offset_ref (expr);
      if (TREE_CODE (TREE_TYPE (expr)) == METHOD_TYPE)
	expr = build_addr_func (expr);
      if (TREE_CODE (TREE_TYPE (expr)) == POINTER_TYPE)
	{
	  if (TREE_CODE (TREE_TYPE (TREE_TYPE (expr))) == METHOD_TYPE)
	    if (pedantic || warn_pmf2ptr)
	      cp_pedwarn ("converting from `%T' to `%T'", TREE_TYPE (expr),
			  type);
	  return build1 (NOP_EXPR, type, expr);
	}
      intype = TREE_TYPE (expr);
    }

  form = TREE_CODE (intype);

  if (POINTER_TYPE_P (intype))
    {
      intype = TYPE_MAIN_VARIANT (intype);

      if (TYPE_MAIN_VARIANT (type) != intype
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE
	  && IS_AGGR_TYPE (TREE_TYPE (type))
	  && IS_AGGR_TYPE (TREE_TYPE (intype))
	  && TREE_CODE (TREE_TYPE (intype)) == RECORD_TYPE
	  /* If EXPR is NULL, then we don't need to do any arithmetic
	     to convert it:

	       [conv.ptr]

	       The null pointer value is converted to the null pointer
	       value of the destination type.  */
	  && !integer_zerop (expr))
	{
	  enum tree_code code = PLUS_EXPR;
	  tree binfo = get_binfo (TREE_TYPE (type), TREE_TYPE (intype), 1);
	  if (binfo == error_mark_node)
	    return error_mark_node;
	  if (binfo == NULL_TREE)
	    {
	      binfo = get_binfo (TREE_TYPE (intype), TREE_TYPE (type), 1);
	      if (binfo == error_mark_node)
		return error_mark_node;
	      code = MINUS_EXPR;
	    }
	  if (binfo)
	    {
	      if (TYPE_USES_VIRTUAL_BASECLASSES (TREE_TYPE (type))
		  || TYPE_USES_VIRTUAL_BASECLASSES (TREE_TYPE (intype))
		  || ! BINFO_OFFSET_ZEROP (binfo))
		{
		  /* Need to get the path we took.  */
		  tree path;

		  if (code == PLUS_EXPR)
		    get_base_distance (TREE_TYPE (type), TREE_TYPE (intype),
				       0, &path);
		  else
		    get_base_distance (TREE_TYPE (intype), TREE_TYPE (type),
				       0, &path);
		  return build_vbase_path (code, type, expr, path, 0);
		}
	    }
	}

      if (TYPE_PTRMEM_P (type) && TYPE_PTRMEM_P (intype))
	{
	  tree b1; 
	  tree b2;
	  tree binfo;
	  enum tree_code code;

	  b1 = TYPE_OFFSET_BASETYPE (TREE_TYPE (type));
	  b2 = TYPE_OFFSET_BASETYPE (TREE_TYPE (intype));
	  binfo = get_binfo (b2, b1, 1);

	  if (binfo == NULL_TREE)
	    {
	      binfo = get_binfo (b1, b2, 1);
	      code = MINUS_EXPR;
	    }
	  else
	    code = PLUS_EXPR;

	  if (binfo == error_mark_node)
	    return error_mark_node;

	  if (binfo_from_vbase (binfo))
	    {
	      cp_error ("conversion to `%T' from pointer to member of virtual base `%T'",
			type, intype);
	      return error_mark_node;
	    }
	      
	  if (TREE_CODE (expr) == PTRMEM_CST)
	    expr = cplus_expand_constant (expr);

	  if (binfo && ! TREE_VIA_VIRTUAL (binfo))
	    expr = size_binop (code, expr, BINFO_OFFSET (binfo));
	}
      else if (TYPE_PTRMEMFUNC_P (type))
	{
	  cp_error ("cannot convert `%E' from type `%T' to type `%T'",
		    expr, intype, type);
	  return error_mark_node;
	}

      rval = build1 (NOP_EXPR, type, expr);
      TREE_CONSTANT (rval) = TREE_CONSTANT (expr);
      return rval;
    }
  else if (TYPE_PTRMEMFUNC_P (type) && TYPE_PTRMEMFUNC_P (intype))
    return build_ptrmemfunc (TYPE_PTRMEMFUNC_FN_TYPE (type), expr, 0);
  else if (TYPE_PTRMEMFUNC_P (intype))
    {
      cp_error ("cannot convert `%E' from type `%T' to type `%T'",
		expr, intype, type);
      return error_mark_node;
    }

  my_friendly_assert (form != OFFSET_TYPE, 186);

  if (integer_zerop (expr))
    {
      if (TYPE_PTRMEMFUNC_P (type))
	return build_ptrmemfunc (TYPE_PTRMEMFUNC_FN_TYPE (type), expr, 0);
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  if (INTEGRAL_CODE_P (form))
    {
      if (TYPE_PRECISION (intype) == POINTER_SIZE)
	return build1 (CONVERT_EXPR, type, expr);
      expr = cp_convert (type_for_size (POINTER_SIZE, 0), expr);
      /* Modes may be different but sizes should be the same.  */
      if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (expr)))
	  != GET_MODE_SIZE (TYPE_MODE (type)))
	/* There is supposed to be some integral type
	   that is the same width as a pointer.  */
	abort ();
      return convert_to_pointer (type, expr);
    }

  if (type_unknown_p (expr))
    return instantiate_type (type, expr, 1);

  cp_error ("cannot convert `%E' from type `%T' to type `%T'",
	    expr, intype, type);
  return error_mark_node;
}

/* Like convert, except permit conversions to take place which
   are not normally allowed due to access restrictions
   (such as conversion from sub-type to private super-type).  */

static tree
convert_to_pointer_force (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  
  if (integer_zerop (expr))
    {
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  if (form == POINTER_TYPE)
    {
      intype = TYPE_MAIN_VARIANT (intype);

      if (TYPE_MAIN_VARIANT (type) != intype
	  && TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE
	  && IS_AGGR_TYPE (TREE_TYPE (type))
	  && IS_AGGR_TYPE (TREE_TYPE (intype))
	  && TREE_CODE (TREE_TYPE (intype)) == RECORD_TYPE)
	{
	  enum tree_code code = PLUS_EXPR;
	  tree path;
	  int distance = get_base_distance (TREE_TYPE (type),
					    TREE_TYPE (intype), 0, &path);
	  if (distance == -2)
	    {
	    ambig:
	      cp_error ("type `%T' is ambiguous baseclass of `%s'",
			TREE_TYPE (type),
			TYPE_NAME_STRING (TREE_TYPE (intype)));
	      return error_mark_node;
	    }
	  if (distance == -1)
	    {
	      distance = get_base_distance (TREE_TYPE (intype),
					    TREE_TYPE (type), 0, &path);
	      if (distance == -2)
		goto ambig;
	      if (distance < 0)
		/* Doesn't need any special help from us.  */
		return build1 (NOP_EXPR, type, expr);

	      code = MINUS_EXPR;
	    }
	  return build_vbase_path (code, type, expr, path, 0);
	}
    }

  return cp_convert_to_pointer (type, expr);
}

/* We are passing something to a function which requires a reference.
   The type we are interested in is in TYPE. The initial
   value we have to begin with is in ARG.

   FLAGS controls how we manage access checking.
   DIRECT_BIND in FLAGS controls how any temporaries are generated.  */

static tree
build_up_reference (type, arg, flags)
     tree type, arg;
     int flags;
{
  tree rval;
  tree argtype = TREE_TYPE (arg);
  tree target_type = TREE_TYPE (type);
  tree stmt_expr = NULL_TREE;

  my_friendly_assert (TREE_CODE (type) == REFERENCE_TYPE, 187);

  if ((flags & DIRECT_BIND) && ! real_lvalue_p (arg))
    {
      /* Create a new temporary variable.  */
      tree targ = arg;
      if (toplevel_bindings_p ())
	arg = get_temp_name (argtype, 1);
      else
	{
	  arg = pushdecl (build_decl (VAR_DECL, NULL_TREE, argtype));
	  DECL_ARTIFICIAL (arg) = 1;
	}

      /* Process the initializer for the declaration.  */
      DECL_INITIAL (arg) = targ;
      cp_finish_decl (arg, targ, NULL_TREE, 
		      LOOKUP_ONLYCONVERTING|DIRECT_BIND);
    }
  else if (!(flags & DIRECT_BIND) && ! lvalue_p (arg))
    return get_target_expr (arg);

  /* If we had a way to wrap this up, and say, if we ever needed it's
     address, transform all occurrences of the register, into a memory
     reference we could win better.  */
  rval = build_unary_op (ADDR_EXPR, arg, 1);
  if (rval == error_mark_node)
    return error_mark_node;

  if ((flags & LOOKUP_PROTECT)
      && TYPE_MAIN_VARIANT (argtype) != TYPE_MAIN_VARIANT (target_type)
      && IS_AGGR_TYPE (argtype)
      && IS_AGGR_TYPE (target_type))
    {
      /* We go through get_binfo for the access control.  */
      tree binfo = get_binfo (target_type, argtype, 1);
      if (binfo == error_mark_node)
	return error_mark_node;
      if (binfo == NULL_TREE)
	return error_not_base_type (target_type, argtype);
      rval = convert_pointer_to_real (binfo, rval);
    }
  else
    rval
      = convert_to_pointer_force (build_pointer_type (target_type), rval);
  rval = build1 (NOP_EXPR, type, rval);
  TREE_CONSTANT (rval) = TREE_CONSTANT (TREE_OPERAND (rval, 0));

  /* If we created and initialized a new temporary variable, add the
     representation of that initialization to the RVAL.  */
  if (stmt_expr)
    rval = build (COMPOUND_EXPR, TREE_TYPE (rval), stmt_expr, rval);

  /* And return the result.  */
  return rval;
}

/* For C++: Only need to do one-level references, but cannot
   get tripped up on signed/unsigned differences.

   DECL is either NULL_TREE or the _DECL node for a reference that is being
   initialized.  It can be error_mark_node if we don't know the _DECL but
   we know it's an initialization.  */

tree
convert_to_reference (reftype, expr, convtype, flags, decl)
     tree reftype, expr;
     int convtype, flags;
     tree decl;
{
  register tree type = TYPE_MAIN_VARIANT (TREE_TYPE (reftype));
  register tree intype = TREE_TYPE (expr);
  tree rval = NULL_TREE;
  tree rval_as_conversion = NULL_TREE;
  int i;

  if (TREE_CODE (type) == FUNCTION_TYPE && intype == unknown_type_node)
    {
      expr = instantiate_type (type, expr, 
			       (flags & LOOKUP_COMPLAIN) != 0);
      if (expr == error_mark_node)
	return error_mark_node;

      intype = TREE_TYPE (expr);
    }

  if (TREE_CODE (intype) == REFERENCE_TYPE)
    my_friendly_abort (364);

  intype = TYPE_MAIN_VARIANT (intype);

  i = comp_target_types (type, intype, 0);

  if (i <= 0 && (convtype & CONV_IMPLICIT) && IS_AGGR_TYPE (intype)
      && ! (flags & LOOKUP_NO_CONVERSION))
    {
      /* Look for a user-defined conversion to lvalue that we can use.  */

      rval_as_conversion
	= build_type_conversion (reftype, expr, 1);

      if (rval_as_conversion && rval_as_conversion != error_mark_node
	  && real_lvalue_p (rval_as_conversion))
	{
	  expr = rval_as_conversion;
	  rval_as_conversion = NULL_TREE;
	  intype = type;
	  i = 1;
	}
    }

  if (((convtype & CONV_STATIC) && i == -1)
      || ((convtype & CONV_IMPLICIT) && i == 1))
    {
      if (flags & LOOKUP_COMPLAIN)
	{
	  tree ttl = TREE_TYPE (reftype);
	  tree ttr = lvalue_type (expr);

	  /* [dcl.init.ref] says that if an rvalue is used to
	     initialize a reference, then the reference must be to a
	     non-volatile const type.  */
	  if (! real_lvalue_p (expr)
	      && !CP_TYPE_CONST_NON_VOLATILE_P (ttl))
	    {
	      const char *msg;

	      if (CP_TYPE_VOLATILE_P (ttl) && decl)
		msg = "initialization of volatile reference type `%#T'";
	      else if (CP_TYPE_VOLATILE_P (ttl))
		msg = "conversion to volatile reference type `%#T'";
	      else if (decl)
		msg = "initialization of non-const reference type `%#T'";
	      else
		msg = "conversion to non-const reference type `%#T'";

	      cp_pedwarn (msg, reftype);
	      cp_pedwarn ("from rvalue of type `%T'", intype);
	    }
	  else if (! (convtype & CONV_CONST)
		   && !at_least_as_qualified_p (ttl, ttr))
	    cp_pedwarn ("conversion from `%T' to `%T' discards qualifiers",
			ttr, reftype);
	}

      return build_up_reference (reftype, expr, flags);
    }
  else if ((convtype & CONV_REINTERPRET) && lvalue_p (expr))
    {
      /* When casting an lvalue to a reference type, just convert into
	 a pointer to the new type and deference it.  This is allowed
	 by San Diego WP section 5.2.9 paragraph 12, though perhaps it
	 should be done directly (jason).  (int &)ri ---> *(int*)&ri */

      /* B* bp; A& ar = (A&)bp; is valid, but it's probably not what they
         meant.  */
      if (TREE_CODE (intype) == POINTER_TYPE
	  && (comptypes (TREE_TYPE (intype), type, 
			 COMPARE_BASE | COMPARE_RELAXED )))
	cp_warning ("casting `%T' to `%T' does not dereference pointer",
		    intype, reftype);
	  
      rval = build_unary_op (ADDR_EXPR, expr, 0);
      if (rval != error_mark_node)
	rval = convert_force (build_pointer_type (TREE_TYPE (reftype)),
			      rval, 0);
      if (rval != error_mark_node)
	rval = build1 (NOP_EXPR, reftype, rval);
    }
  else
    {
      rval = convert_for_initialization (NULL_TREE, type, expr, flags,
					 "converting", 0, 0);
      if (rval == NULL_TREE || rval == error_mark_node)
	return rval;
      rval = build_up_reference (reftype, rval, flags);

      if (rval && ! CP_TYPE_CONST_P (TREE_TYPE (reftype)))
	cp_pedwarn ("initializing non-const `%T' with `%T' will use a temporary",
		    reftype, intype);
    }

  if (rval)
    {
      /* If we found a way to convert earlier, then use it.  */
      return rval;
    }

  my_friendly_assert (TREE_CODE (intype) != OFFSET_TYPE, 189);

  if (flags & LOOKUP_COMPLAIN)
    cp_error ("cannot convert type `%T' to type `%T'", intype, reftype);

  if (flags & LOOKUP_SPECULATIVELY)
    return NULL_TREE;

  return error_mark_node;
}

/* We are using a reference VAL for its value. Bash that reference all the
   way down to its lowest form.  */

tree
convert_from_reference (val)
     tree val;
{
  tree type = TREE_TYPE (val);

  if (TREE_CODE (type) == OFFSET_TYPE)
    type = TREE_TYPE (type);
  if (TREE_CODE (type) == REFERENCE_TYPE)
    return build_indirect_ref (val, NULL_PTR);
  return val;
}

/* Call this when we know (for any reason) that expr is not, in fact,
   zero.  This routine is like convert_pointer_to, but it pays
   attention to which specific instance of what type we want to
   convert to.  This routine should eventually become
   convert_to_pointer after all references to convert_to_pointer
   are removed.  */

tree
convert_pointer_to_real (binfo, expr)
     tree binfo, expr;
{
  register tree intype = TREE_TYPE (expr);
  tree ptr_type;
  tree type, rval;

  if (intype == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (binfo) == TREE_VEC)
    type = BINFO_TYPE (binfo);
  else if (IS_AGGR_TYPE (binfo))
    {
      type = binfo;
    }
  else
    {
      type = binfo;
      binfo = NULL_TREE;
    }

  ptr_type = cp_build_qualified_type (type,
				      CP_TYPE_QUALS (TREE_TYPE (intype)));
  ptr_type = build_pointer_type (ptr_type);
  if (same_type_p (ptr_type, TYPE_MAIN_VARIANT (intype)))
    return expr;

  my_friendly_assert (!integer_zerop (expr), 191);

  intype = TYPE_MAIN_VARIANT (TREE_TYPE (intype));
  if (TREE_CODE (type) == RECORD_TYPE
      && TREE_CODE (intype) == RECORD_TYPE
      && type != intype)
    {
      tree path;
      int distance
	= get_base_distance (binfo, intype, 0, &path);

      /* This function shouldn't be called with unqualified arguments
	 but if it is, give them an error message that they can read.  */
      if (distance < 0)
	{
	  cp_error ("cannot convert a pointer of type `%T' to a pointer of type `%T'",
		    intype, type);

	  if (distance == -2)
	    cp_error ("because `%T' is an ambiguous base class", type);
	  return error_mark_node;
	}

      return build_vbase_path (PLUS_EXPR, ptr_type, expr, path, 1);
    }
  rval = build1 (NOP_EXPR, ptr_type,
		 TREE_CODE (expr) == NOP_EXPR ? TREE_OPERAND (expr, 0) : expr);
  TREE_CONSTANT (rval) = TREE_CONSTANT (expr);
  return rval;
}

/* Call this when we know (for any reason) that expr is
   not, in fact, zero.  This routine gets a type out of the first
   argument and uses it to search for the type to convert to.  If there
   is more than one instance of that type in the expr, the conversion is
   ambiguous.  This routine should eventually go away, and all
   callers should use convert_to_pointer_real.  */

tree
convert_pointer_to (binfo, expr)
     tree binfo, expr;
{
  tree type;

  if (TREE_CODE (binfo) == TREE_VEC)
    type = BINFO_TYPE (binfo);
  else if (IS_AGGR_TYPE (binfo))
    type = binfo;
  else
    type = binfo;
  return convert_pointer_to_real (type, expr);
}

/* C++ conversions, preference to static cast conversions.  */

tree
cp_convert (type, expr)
     tree type, expr;
{
  return ocp_convert (type, expr, CONV_OLD_CONVERT, LOOKUP_NORMAL);
}

/* Conversion...

   FLAGS indicates how we should behave.  */

tree
ocp_convert (type, expr, convtype, flags)
     tree type, expr;
     int convtype, flags;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (e == error_mark_node
      || TREE_TYPE (e) == error_mark_node)
    return error_mark_node;

  complete_type (type);
  complete_type (TREE_TYPE (expr));

  if (TREE_READONLY_DECL_P (e))
    e = decl_constant_value (e);

  if (IS_AGGR_TYPE (type) && (convtype & CONV_FORCE_TEMP)
      /* Some internal structures (vtable_entry_type, sigtbl_ptr_type)
	 don't go through finish_struct, so they don't have the synthesized
	 constructors.  So don't force a temporary.  */
      && TYPE_HAS_CONSTRUCTOR (type))
    /* We need a new temporary; don't take this shortcut.  */;
  else if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (e)))
    {
      if (same_type_p (type, TREE_TYPE (e)))
	/* The call to fold will not always remove the NOP_EXPR as
	   might be expected, since if one of the types is a typedef;
	   the comparsion in fold is just equality of pointers, not a
	   call to comptypes.  We don't call fold in this case because
	   that can result in infinite recursion; fold will call
	   convert, which will call ocp_convert, etc.  */
	return e;
      else
	return fold (build1 (NOP_EXPR, type, e));
    }

  if (code == VOID_TYPE && (convtype & CONV_STATIC))
    {
      e = convert_to_void (e, /*implicit=*/NULL);
      return e;
    }

  /* Just convert to the type of the member.  */
  if (code == OFFSET_TYPE)
    {
      type = TREE_TYPE (type);
      code = TREE_CODE (type);
    }

  if (TREE_CODE (e) == OFFSET_REF)
    e = resolve_offset_ref (e);

  if (INTEGRAL_CODE_P (code))
    {
      tree intype = TREE_TYPE (e);
      /* enum = enum, enum = int, enum = float, (enum)pointer are all
         errors.  */
      if (TREE_CODE (type) == ENUMERAL_TYPE
	  && ((ARITHMETIC_TYPE_P (intype) && ! (convtype & CONV_STATIC))
	      || (TREE_CODE (intype) == POINTER_TYPE)))
	{
	  cp_pedwarn ("conversion from `%#T' to `%#T'", intype, type);

	  if (flag_pedantic_errors)
	    return error_mark_node;
	}
      if (IS_AGGR_TYPE (intype))
	{
	  tree rval;
	  rval = build_type_conversion (type, e, 1);
	  if (rval)
	    return rval;
	  if (flags & LOOKUP_COMPLAIN)
	    cp_error ("`%#T' used where a `%T' was expected", intype, type);
	  if (flags & LOOKUP_SPECULATIVELY)
	    return NULL_TREE;
	  return error_mark_node;
	}
      if (code == BOOLEAN_TYPE)
	{
	  tree fn = NULL_TREE;

	  /* Common Ada/Pascal programmer's mistake.  We always warn
             about this since it is so bad.  */
	  if (TREE_CODE (expr) == FUNCTION_DECL)
	    fn = expr;
	  else if (TREE_CODE (expr) == ADDR_EXPR 
		   && TREE_CODE (TREE_OPERAND (expr, 0)) == FUNCTION_DECL)
	    fn = TREE_OPERAND (expr, 0);
	  if (fn)
	    cp_warning ("the address of `%D', will always be `true'", fn);
	  return truthvalue_conversion (e);
	}
      return fold (convert_to_integer (type, e));
    }
  if (code == POINTER_TYPE || code == REFERENCE_TYPE
      || TYPE_PTRMEMFUNC_P (type))
    return fold (cp_convert_to_pointer (type, e));
  if (code == REAL_TYPE || code == COMPLEX_TYPE)
    {
      if (IS_AGGR_TYPE (TREE_TYPE (e)))
	{
	  tree rval;
	  rval = build_type_conversion (type, e, 1);
	  if (rval)
	    return rval;
	  else
	    if (flags & LOOKUP_COMPLAIN)
	      cp_error ("`%#T' used where a floating point value was expected",
			TREE_TYPE (e));
	}
      if (code == REAL_TYPE)
	return fold (convert_to_real (type, e));
      else if (code == COMPLEX_TYPE)
	return fold (convert_to_complex (type, e));
    }

  /* New C++ semantics:  since assignment is now based on
     memberwise copying,  if the rhs type is derived from the
     lhs type, then we may still do a conversion.  */
  if (IS_AGGR_TYPE_CODE (code))
    {
      tree dtype = TREE_TYPE (e);
      tree ctor = NULL_TREE;

      dtype = TYPE_MAIN_VARIANT (dtype);

      /* Conversion between aggregate types.  New C++ semantics allow
	 objects of derived type to be cast to objects of base type.
	 Old semantics only allowed this between pointers.

	 There may be some ambiguity between using a constructor
	 vs. using a type conversion operator when both apply.  */

      ctor = e;

      if (abstract_virtuals_error (NULL_TREE, type))
	return error_mark_node;

      if ((flags & LOOKUP_ONLYCONVERTING)
	  && ! (IS_AGGR_TYPE (dtype) && DERIVED_FROM_P (type, dtype)))
	/* For copy-initialization, first we create a temp of the proper type
	   with a user-defined conversion sequence, then we direct-initialize
	   the target with the temp (see [dcl.init]).  */
	ctor = build_user_type_conversion (type, ctor, flags);
      if (ctor)
	ctor = build_method_call (NULL_TREE, ctor_identifier,
				  build_tree_list (NULL_TREE, ctor),
				  TYPE_BINFO (type), flags);
      if (ctor)
	return build_cplus_new (type, ctor);
    }

  /* If TYPE or TREE_TYPE (E) is not on the permanent_obstack,
     then it won't be hashed and hence compare as not equal,
     even when it is.  */
  if (code == ARRAY_TYPE
      && TREE_TYPE (TREE_TYPE (e)) == TREE_TYPE (type)
      && index_type_equal (TYPE_DOMAIN (TREE_TYPE (e)), TYPE_DOMAIN (type)))
    return e;

  if (flags & LOOKUP_COMPLAIN)
    cp_error ("conversion from `%T' to non-scalar type `%T' requested",
	      TREE_TYPE (expr), type);
  if (flags & LOOKUP_SPECULATIVELY)
    return NULL_TREE;
  return error_mark_node;
}

/* When an expression is used in a void context, its value is discarded and
   no lvalue-rvalue and similar conversions happen [expr.static.cast/4,
   stmt.expr/1, expr.comma/1].  This permits dereferencing an incomplete type
   in a void context. The C++ standard does not define what an `access' to an
   object is, but there is reason to beleive that it is the lvalue to rvalue
   conversion -- if it were not, `*&*p = 1' would violate [expr]/4 in that it
   accesses `*p' not to calculate the value to be stored. But, dcl.type.cv/8
   indicates that volatile semantics should be the same between C and C++
   where ever possible. C leaves it implementation defined as to what
   constitutes an access to a volatile. So, we interpret `*vp' as a read of
   the volatile object `vp' points to, unless that is an incomplete type. For
   volatile references we do not do this interpretation, because that would
   make it impossible to ignore the reference return value from functions. We
   issue warnings in the confusing cases.
   
   IMPLICIT is tells us the context of an implicit void conversion.  */

tree
convert_to_void (expr, implicit)
     tree expr;
     const char *implicit;
{
  if (expr == error_mark_node 
      || TREE_TYPE (expr) == error_mark_node)
    return error_mark_node;
  if (!TREE_TYPE (expr))
    return expr;
  if (same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (expr)), void_type_node))
    return expr;
  switch (TREE_CODE (expr))
    {
    case COND_EXPR:
      {
        /* The two parts of a cond expr might be separate lvalues.  */
        tree op1 = TREE_OPERAND (expr,1);
        tree op2 = TREE_OPERAND (expr,2);
        tree new_op1 = convert_to_void (op1, implicit);
        tree new_op2 = convert_to_void (op2, implicit);
        
        if (new_op1 != op1 || new_op2 != op2)
          expr = build (COND_EXPR,
                        implicit ? TREE_TYPE (expr) : void_type_node,
                        TREE_OPERAND (expr, 0), new_op1, new_op2);
        break;
      }
    
    case COMPOUND_EXPR:
      {
        /* The second part of a compound expr contains the value.  */
        tree op1 = TREE_OPERAND (expr,1);
        tree new_op1 = convert_to_void (op1, implicit);
        
        if (new_op1 != op1)
          expr = build (COMPOUND_EXPR, TREE_TYPE (new_op1),
                        TREE_OPERAND (expr, 0), new_op1);
        break;
      }
    
    case NON_LVALUE_EXPR:
    case NOP_EXPR:
      /* These have already decayed to rvalue. */
      break;
    
    case CALL_EXPR:   /* we have a special meaning for volatile void fn() */
      break;
    
    case INDIRECT_REF:
      {
        tree type = TREE_TYPE (expr);
        int is_reference = TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0)))
                           == REFERENCE_TYPE;
        int is_volatile = TYPE_VOLATILE (type);
        int is_complete = TYPE_SIZE (complete_type (type)) != NULL_TREE;
        
        if (is_volatile && !is_complete)
          cp_warning ("object of incomplete type `%T' will not be accessed in %s",
                      type, implicit ? implicit : "void context");
        else if (is_reference && is_volatile)
          cp_warning ("object of type `%T' will not be accessed in %s",
                      TREE_TYPE (TREE_OPERAND (expr, 0)),
                      implicit ? implicit : "void context");
        if (is_reference || !is_volatile || !is_complete)
          expr = TREE_OPERAND (expr, 0);
      
        break;
      }
    
    case VAR_DECL:
      {
        /* External variables might be incomplete.  */
        tree type = TREE_TYPE (expr);
        int is_complete = TYPE_SIZE (complete_type (type)) != NULL_TREE;
        
        if (TYPE_VOLATILE (type) && !is_complete)
          cp_warning ("object `%E' of incomplete type `%T' will not be accessed in %s",
                      expr, type, implicit ? implicit : "void context");
        break;
      }
    
    default:;
    }
  {
    tree probe = expr;
  
    if (TREE_CODE (probe) == ADDR_EXPR)
      probe = TREE_OPERAND (expr, 0);
    if (!is_overloaded_fn (probe))
      ;/* OK */
    else if (really_overloaded_fn (probe))
        {
          /* [over.over] enumerates the places where we can take the address
             of an overloaded function, and this is not one of them.  */
          cp_pedwarn ("%s has no context for overloaded function name `%E'",
                      implicit ? implicit : "void cast", expr);
        }
    else if (implicit && probe == expr)
      /* Only warn when there is no &.  */
      cp_warning ("%s is a reference, not call, to function `%E'",
                    implicit, expr);
  }
  
  if (expr != error_mark_node
      && !same_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (expr)), void_type_node))
    {
      /* FIXME: This is where we should check for expressions with no
         effects.  At the moment we do that in both build_x_component_expr
         and expand_expr_stmt -- inconsistently too.  For the moment
         leave implicit void conversions unadorned so that expand_expr_stmt
         has a chance of detecting some of the cases.  */
      if (!implicit)
        expr = build1 (CONVERT_EXPR, void_type_node, expr);
    }
  return expr;
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.

   Most of this routine is from build_reinterpret_cast.

   The backend cannot call cp_convert (what was convert) because
   conversions to/from basetypes may involve memory references
   (vbases) and adding or subtracting small values (multiple
   inheritance), but it calls convert from the constant folding code
   on subtrees of already build trees after it has ripped them apart.

   Also, if we ever support range variables, we'll probably also have to
   do a little bit more work.  */

tree
convert (type, expr)
     tree type, expr;
{
  tree intype;

  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;

  intype = TREE_TYPE (expr);

  if (POINTER_TYPE_P (type) && POINTER_TYPE_P (intype))
    {
      if (TREE_READONLY_DECL_P (expr))
	expr = decl_constant_value (expr);
      return fold (build1 (NOP_EXPR, type, expr));
    }

  return ocp_convert (type, expr, CONV_OLD_CONVERT,
		      LOOKUP_NORMAL|LOOKUP_NO_CONVERSION);
}

/* Like cp_convert, except permit conversions to take place which
   are not normally allowed due to access restrictions
   (such as conversion from sub-type to private super-type).  */

tree
convert_force (type, expr, convtype)
     tree type;
     tree expr;
     int convtype;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (code == REFERENCE_TYPE)
    return fold (convert_to_reference (type, e, CONV_C_CAST, LOOKUP_COMPLAIN,
				       NULL_TREE));
  else if (TREE_CODE (TREE_TYPE (e)) == REFERENCE_TYPE)
    e = convert_from_reference (e);

  if (code == POINTER_TYPE)
    return fold (convert_to_pointer_force (type, e));

  /* From typeck.c convert_for_assignment */
  if (((TREE_CODE (TREE_TYPE (e)) == POINTER_TYPE && TREE_CODE (e) == ADDR_EXPR
	&& TREE_CODE (TREE_TYPE (e)) == POINTER_TYPE
	&& TREE_CODE (TREE_TYPE (TREE_TYPE (e))) == METHOD_TYPE)
       || integer_zerop (e)
       || TYPE_PTRMEMFUNC_P (TREE_TYPE (e)))
      && TYPE_PTRMEMFUNC_P (type))
    {
      /* compatible pointer to member functions.  */
      return build_ptrmemfunc (TYPE_PTRMEMFUNC_FN_TYPE (type), e, 1);
    }

  return ocp_convert (type, e, CONV_C_CAST|convtype, LOOKUP_NORMAL);
}

/* Convert an aggregate EXPR to type XTYPE.  If a conversion
   exists, return the attempted conversion.  This may
   return ERROR_MARK_NODE if the conversion is not
   allowed (references private members, etc).
   If no conversion exists, NULL_TREE is returned.

   If (FOR_SURE & 1) is non-zero, then we allow this type conversion
   to take place immediately.  Otherwise, we build a SAVE_EXPR
   which can be evaluated if the results are ever needed.

   Changes to this functions should be mirrored in user_harshness.

   FIXME: Ambiguity checking is wrong.  Should choose one by the implicit
   object parameter, or by the second standard conversion sequence if
   that doesn't do it.  This will probably wait for an overloading rewrite.
   (jason 8/9/95)  */

tree
build_type_conversion (xtype, expr, for_sure)
     tree xtype, expr;
     int for_sure;
{
  /* C++: check to see if we can convert this aggregate type
     into the required type.  */
  return build_user_type_conversion
    (xtype, expr, for_sure ? LOOKUP_NORMAL : 0);
}

/* Convert the given EXPR to one of a group of types suitable for use in an
   expression.  DESIRES is a combination of various WANT_* flags (q.v.)
   which indicates which types are suitable.  If COMPLAIN is 1, complain
   about ambiguity; otherwise, the caller will deal with it.  */

tree
build_expr_type_conversion (desires, expr, complain)
     int desires;
     tree expr;
     int complain;
{
  tree basetype = TREE_TYPE (expr);
  tree conv = NULL_TREE;
  tree winner = NULL_TREE;

  if (expr == null_node 
      && (desires & WANT_INT) 
      && !(desires & WANT_NULL))
    cp_warning ("converting NULL to non-pointer type");
    
  if (TREE_CODE (expr) == OFFSET_REF)
    expr = resolve_offset_ref (expr);
  expr = convert_from_reference (expr);
  basetype = TREE_TYPE (expr);

  if (basetype == error_mark_node)
    return error_mark_node;

  if (! IS_AGGR_TYPE (basetype))
    switch (TREE_CODE (basetype))
      {
      case INTEGER_TYPE:
	if ((desires & WANT_NULL) && null_ptr_cst_p (expr))
	  return expr;
	/* else fall through...  */

      case BOOLEAN_TYPE:
	return (desires & WANT_INT) ? expr : NULL_TREE;
      case ENUMERAL_TYPE:
	return (desires & WANT_ENUM) ? expr : NULL_TREE;
      case REAL_TYPE:
	return (desires & WANT_FLOAT) ? expr : NULL_TREE;
      case POINTER_TYPE:
	return (desires & WANT_POINTER) ? expr : NULL_TREE;
	
      case FUNCTION_TYPE:
      case ARRAY_TYPE:
	return (desires & WANT_POINTER) ? default_conversion (expr)
     	                                : NULL_TREE;
      default:
	return NULL_TREE;
      }

  /* The code for conversions from class type is currently only used for
     delete expressions.  Other expressions are handled by build_new_op.  */

  if (! TYPE_HAS_CONVERSION (basetype))
    return NULL_TREE;

  for (conv = lookup_conversions (basetype); conv; conv = TREE_CHAIN (conv))
    {
      int win = 0;
      tree candidate;
      tree cand = TREE_VALUE (conv);

      if (winner && winner == cand)
	continue;

      candidate = TREE_TYPE (TREE_TYPE (cand));
      if (TREE_CODE (candidate) == REFERENCE_TYPE)
	candidate = TREE_TYPE (candidate);

      switch (TREE_CODE (candidate))
	{
	case BOOLEAN_TYPE:
	case INTEGER_TYPE:
	  win = (desires & WANT_INT); break;
	case ENUMERAL_TYPE:
	  win = (desires & WANT_ENUM); break;
	case REAL_TYPE:
	  win = (desires & WANT_FLOAT); break;
	case POINTER_TYPE:
	  win = (desires & WANT_POINTER); break;

	default:
	  break;
	}

      if (win)
	{
	  if (winner)
	    {
	      if (complain)
		{
		  cp_error ("ambiguous default type conversion from `%T'",
			    basetype);
		  cp_error ("  candidate conversions include `%D' and `%D'",
			    winner, cand);
		}
	      return error_mark_node;
	    }
	  else
	    winner = cand;
	}
    }

  if (winner)
    {
      tree type = TREE_TYPE (TREE_TYPE (winner));
      if (TREE_CODE (type) == REFERENCE_TYPE)
	type = TREE_TYPE (type);
      return build_user_type_conversion (type, expr, LOOKUP_NORMAL);
    }

  return NULL_TREE;
}

/* Implements integral promotion (4.1) and float->double promotion.  */

tree
type_promotes_to (type)
     tree type;
{
  int type_quals;

  if (type == error_mark_node)
    return error_mark_node;

  type_quals = CP_TYPE_QUALS (type);
  type = TYPE_MAIN_VARIANT (type);

  /* bool always promotes to int (not unsigned), even if it's the same
     size.  */
  if (type == boolean_type_node)
    type = integer_type_node;

  /* Normally convert enums to int, but convert wide enums to something
     wider.  */
  else if (TREE_CODE (type) == ENUMERAL_TYPE
	   || type == wchar_type_node)
    {
      int precision = MAX (TYPE_PRECISION (type),
			   TYPE_PRECISION (integer_type_node));
      tree totype = type_for_size (precision, 0);
      if (TREE_UNSIGNED (type)
	  && ! int_fits_type_p (TYPE_MAX_VALUE (type), totype))
	type = type_for_size (precision, 1);
      else
	type = totype;
    }
  else if (C_PROMOTING_INTEGER_TYPE_P (type))
    {
      /* Retain unsignedness if really not getting bigger.  */
      if (TREE_UNSIGNED (type)
	  && TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node))
	type = unsigned_type_node;
      else
	type = integer_type_node;
    }
  else if (type == float_type_node)
    type = double_type_node;

  return cp_build_qualified_type (type, type_quals);
}

/* The routines below this point are carefully written to conform to
   the standard.  They use the same terminology, and follow the rules
   closely.  Although they are used only in pt.c at the moment, they
   should presumably be used everywhere in the future.  */

/* Attempt to perform qualification conversions on EXPR to convert it
   to TYPE.  Return the resulting expression, or error_mark_node if
   the conversion was impossible.  */

tree 
perform_qualification_conversions (type, expr)
     tree type;
     tree expr;
{
  if (TREE_CODE (type) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (expr)) == POINTER_TYPE
      && comp_ptr_ttypes (TREE_TYPE (type), TREE_TYPE (TREE_TYPE (expr))))
    return build1 (NOP_EXPR, type, expr);
  else
    return error_mark_node;
}
