/* Language-level data type conversion for GNU C++.
   Copyright (C) 1987, 88, 92, 93, 94, 95, 1996 Free Software Foundation, Inc.
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
#include "tree.h"
#include "flags.h"
#include "cp-tree.h"
#include "class.h"
#include "convert.h"

#undef NULL
#define NULL (char *)0

tree build_user_type_conversion ();

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

/* Build a thunk.  What it is, is an entry point that when called will
   adjust the this pointer (the first argument) by offset, and then
   goto the real address of the function given by REAL_ADDR that we
   would like called.  What we return is the address of the thunk.  */

static tree
build_thunk (offset, real_addr)
     tree offset, real_addr;
{
  if (TREE_CODE (real_addr) != ADDR_EXPR
      || TREE_CODE (TREE_OPERAND (real_addr, 0)) != FUNCTION_DECL)
    {
      sorry ("MI pointer to member conversion too complex");
      return error_mark_node;
    }
  sorry ("MI pointer to member conversion too complex");
  return error_mark_node;
}

/* Convert a `pointer to member' (POINTER_TYPE to METHOD_TYPE) into
   another `pointer to method'.  This may involved the creation of
   a thunk to handle the this offset calculation.  */

static tree
convert_fn_ptr (type, expr)
     tree type, expr;
{
#if 0				/* We don't use thunks for pmfs.  */
  if (flag_vtable_thunks)
    {
      tree intype = TREE_TYPE (expr);
      tree binfo = get_binfo (TYPE_METHOD_BASETYPE (TREE_TYPE (intype)),
			      TYPE_METHOD_BASETYPE (TREE_TYPE (type)), 1);
      if (binfo == error_mark_node)
	{
	  error ("  in pointer to member conversion");
	  return error_mark_node;
	}
      if (binfo == NULL_TREE)
	{
	  /* ARM 4.8 restriction.  */
	  error ("invalid pointer to member conversion");
	  return error_mark_node;
	}

      if (BINFO_OFFSET_ZEROP (binfo))
	return build1 (NOP_EXPR, type, expr);
      return build1 (NOP_EXPR, type, build_thunk (BINFO_OFFSET (binfo), expr));
    }
  else
#endif
    return build_ptrmemfunc (type, expr, 1);
}

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

  if (IS_AGGR_TYPE (intype))
    {
      tree rval;

      intype = complete_type (intype);
      if (TYPE_SIZE (intype) == NULL_TREE)
	{
	  cp_error ("can't convert from incomplete type `%T' to `%T'",
		    intype, type);
	  return error_mark_node;
	}

      rval = build_type_conversion (CONVERT_EXPR, type, expr, 1);
      if (rval)
	{
	  if (rval == error_mark_node)
	    cp_error ("conversion of `%E' from `%T' to `%T' is ambiguous",
		      expr, intype, type);
	  return rval;
	}
    }

  if (TYPE_PTRMEMFUNC_P (type))
    type = TYPE_PTRMEMFUNC_FN_TYPE (type);
  if (TYPE_PTRMEMFUNC_P (intype))
    intype = TYPE_PTRMEMFUNC_FN_TYPE (intype);

  form = TREE_CODE (intype);

  if (form == POINTER_TYPE || form == REFERENCE_TYPE)
    {
      intype = TYPE_MAIN_VARIANT (intype);

      if (TYPE_MAIN_VARIANT (type) != intype
	  && TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE
	  && IS_AGGR_TYPE (TREE_TYPE (type))
	  && IS_AGGR_TYPE (TREE_TYPE (intype))
	  && TREE_CODE (TREE_TYPE (intype)) == RECORD_TYPE)
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
		    get_base_distance (TREE_TYPE (type), TREE_TYPE (intype), 0, &path);
		  else
		    get_base_distance (TREE_TYPE (intype), TREE_TYPE (type), 0, &path);
		  return build_vbase_path (code, type, expr, path, 0);
		}
	    }
	}
      if (TREE_CODE (TREE_TYPE (intype)) == METHOD_TYPE
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE)
	return convert_fn_ptr (type, expr);

      if (TREE_CODE (TREE_TYPE (type)) == OFFSET_TYPE
	  && TREE_CODE (TREE_TYPE (intype)) == OFFSET_TYPE)
	{
	  tree b1 = TYPE_OFFSET_BASETYPE (TREE_TYPE (type));
	  tree b2 = TYPE_OFFSET_BASETYPE (TREE_TYPE (intype));
	  tree binfo = get_binfo (b1, b2, 1);
	  if (binfo == NULL_TREE)
	    binfo = get_binfo (b2, b1, 1);
	  if (binfo == error_mark_node)
	    return error_mark_node;
	}

      if (TREE_CODE (TREE_TYPE (intype)) == METHOD_TYPE
	  || (TREE_CODE (type) == POINTER_TYPE
	      && TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE))
	{
	  cp_error ("cannot convert `%E' from type `%T' to type `%T'",
		    expr, intype, type);
	  return error_mark_node;
	}

      return build1 (NOP_EXPR, type, expr);
    }

  my_friendly_assert (form != OFFSET_TYPE, 186);

  if (TYPE_LANG_SPECIFIC (intype)
      && (IS_SIGNATURE_POINTER (intype) || IS_SIGNATURE_REFERENCE (intype)))
    return convert_to_pointer (type, build_optr_ref (expr));

  if (integer_zerop (expr))
    {
      if (type == TREE_TYPE (null_pointer_node))
	return null_pointer_node;
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  if (INTEGRAL_CODE_P (form))
    {
      if (type_precision (intype) == POINTER_SIZE)
	return build1 (CONVERT_EXPR, type, expr);
      expr = convert (type_for_size (POINTER_SIZE, 0), expr);
      /* Modes may be different but sizes should be the same.  */
      if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (expr)))
	  != GET_MODE_SIZE (TYPE_MODE (type)))
	/* There is supposed to be some integral type
	   that is the same width as a pointer.  */
	abort ();
      return convert_to_pointer (type, expr);
    }

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
      if (type == TREE_TYPE (null_pointer_node))
	return null_pointer_node;
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  /* Convert signature pointer/reference to `void *' first.  */
  if (form == RECORD_TYPE
      && (IS_SIGNATURE_POINTER (intype) || IS_SIGNATURE_REFERENCE (intype)))
    {
      expr = build_optr_ref (expr);
      intype = TREE_TYPE (expr);
      form = TREE_CODE (intype);
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
	      cp_error ("type `%T' is ambiguous baseclass of `%s'", TREE_TYPE (type),
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
      return build1 (NOP_EXPR, type, expr);
    }

  return cp_convert_to_pointer (type, expr);
}

/* We are passing something to a function which requires a reference.
   The type we are interested in is in TYPE. The initial
   value we have to begin with is in ARG.

   FLAGS controls how we manage access checking.
   INDIRECT_BIND in FLAGS controls how any temporarys are generated.
   CHECKCONST controls if we report error messages on const subversion.  */

static tree
build_up_reference (type, arg, flags, checkconst)
     tree type, arg;
     int flags, checkconst;
{
  tree rval, targ;
  int literal_flag = 0;
  tree argtype = TREE_TYPE (arg);
  tree target_type = TREE_TYPE (type);
  tree binfo = NULL_TREE;

  my_friendly_assert (TREE_CODE (type) == REFERENCE_TYPE, 187);
  if ((flags & LOOKUP_PROTECT)
      && TYPE_MAIN_VARIANT (argtype) != TYPE_MAIN_VARIANT (target_type)
      && IS_AGGR_TYPE (argtype)
      && IS_AGGR_TYPE (target_type))
    {
      binfo = get_binfo (target_type, argtype, 1);
      if (binfo == error_mark_node)
	return error_mark_node;
      if (binfo == NULL_TREE)
	return error_not_base_type (target_type, argtype);
    }

  /* Pass along const and volatile down into the type.  */
  if (TYPE_READONLY (type) || TYPE_VOLATILE (type))
    target_type = cp_build_type_variant (target_type, TYPE_READONLY (type),
					TYPE_VOLATILE (type));
  targ = arg;
  if (TREE_CODE (targ) == SAVE_EXPR)
    targ = TREE_OPERAND (targ, 0);
  while (TREE_CODE (targ) == NOP_EXPR
	 && (TYPE_MAIN_VARIANT (argtype)
	     == TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (targ, 0)))))
    targ = TREE_OPERAND (targ, 0);

  switch (TREE_CODE (targ))
    {
    case INDIRECT_REF:
      /* This is a call to a constructor which did not know what it was
	 initializing until now: it needs to initialize a temporary.  */
      if (TREE_HAS_CONSTRUCTOR (targ))
	{
	  tree temp = build_cplus_new (argtype, TREE_OPERAND (targ, 0));
	  TREE_HAS_CONSTRUCTOR (targ) = 0;
	  return build_up_reference (type, temp, flags, 1);
	}
      /* Let &* cancel out to simplify resulting code.
         Also, throw away intervening NOP_EXPRs.  */
      arg = TREE_OPERAND (targ, 0);
      if (TREE_CODE (arg) == NOP_EXPR || TREE_CODE (arg) == NON_LVALUE_EXPR
	  || (TREE_CODE (arg) == CONVERT_EXPR && TREE_REFERENCE_EXPR (arg)))
	arg = TREE_OPERAND (arg, 0);

      /* in doing a &*, we have to get rid of the const'ness on the pointer
	 value.  Haven't thought about volatile here.  Pointers come to mind
	 here.  */
      if (TREE_READONLY (arg))
	{
	  arg = copy_node (arg);
	  TREE_READONLY (arg) = 0;
	}

      rval = build1 (CONVERT_EXPR, type, arg);
      TREE_REFERENCE_EXPR (rval) = 1;

      /* propagate the const flag on something like:

	 class Base {
	 public:
	   int foo;
	 };

      class Derived : public Base {
      public:
	int bar;
      };

      void func(Base&);

      void func2(const Derived& d) {
	func(d);
      }

        on the d parameter.  The below could have been avoided, if the flags
        were down in the tree, not sure why they are not.  (mrs) */
      /* The below code may have to be propagated to other parts of this
	 switch.  */
      if (TREE_READONLY (targ) && !TREE_READONLY (arg)
	  && (TREE_CODE (arg) == PARM_DECL || TREE_CODE (arg) == VAR_DECL)
	  && TREE_CODE (TREE_TYPE (arg)) == REFERENCE_TYPE
	  && (TYPE_READONLY (target_type) && checkconst))
	{
	  arg = copy_node (arg);
	  TREE_READONLY (arg) = TREE_READONLY (targ);
	}
      literal_flag = TREE_CONSTANT (arg);

      goto done;

      /* Get this out of a register if we happened to be in one by accident.
	 Also, build up references to non-lvalues it we must.  */
      /* For &x[y], return (&) x+y */
    case ARRAY_REF:
      if (mark_addressable (TREE_OPERAND (targ, 0)) == 0)
	return error_mark_node;
      rval = build_binary_op (PLUS_EXPR, TREE_OPERAND (targ, 0),
			      TREE_OPERAND (targ, 1), 1);
      TREE_TYPE (rval) = type;
      if (TREE_CONSTANT (TREE_OPERAND (targ, 1))
	  && staticp (TREE_OPERAND (targ, 0)))
	TREE_CONSTANT (rval) = 1;
      goto done;

    case SCOPE_REF:
      /* Could be a reference to a static member.  */
      {
	tree field = TREE_OPERAND (targ, 1);
	if (TREE_STATIC (field))
	  {
	    rval = build1 (ADDR_EXPR, type, field);
	    literal_flag = 1;
	    goto done;
	  }
      }

      /* We should have farmed out member pointers above.  */
      my_friendly_abort (188);

    case COMPONENT_REF:
      rval = build_component_addr (targ, build_pointer_type (argtype),
				   "attempt to make a reference to bit-field structure member `%s'");
      TREE_TYPE (rval) = type;
      literal_flag = staticp (TREE_OPERAND (targ, 0));

      goto done;

      /* Anything not already handled and not a true memory reference
	 needs to have a reference built up.  Do so silently for
	 things like integers and return values from function,
	 but complain if we need a reference to something declared
	 as `register'.  */

    case RESULT_DECL:
      if (staticp (targ))
	literal_flag = 1;
      TREE_ADDRESSABLE (targ) = 1;
      put_var_into_stack (targ);
      break;

    case PARM_DECL:
#if 0
      if (targ == current_class_ptr)
	{
	  error ("address of `this' not available");
/* #if 0 */	  
	  /* This code makes the following core dump the compiler on a sun4,
	     if the code below is used.

	     class e_decl;
	     class a_decl;
	     typedef a_decl* a_ref;

	     class a_s {
	     public:
	       a_s();
	       void* append(a_ref& item);
	     };
	     class a_decl {
	     public:
	       a_decl (e_decl *parent);
	       a_s  generic_s;
	       a_s  decls;
	       e_decl* parent;
	     };

	     class e_decl {
	     public:
	       e_decl();
	       a_s implementations;
	     };

	     void foobar(void *);

	     a_decl::a_decl(e_decl *parent) {
	       parent->implementations.append(this);
	     }
	   */

	  TREE_ADDRESSABLE (targ) = 1; /* so compiler doesn't die later */
	  put_var_into_stack (targ);
	  break;
/* #else */
	  return error_mark_node;
/* #endif */	  
	}
#endif
      /* Fall through.  */
    case VAR_DECL:
    case CONST_DECL:
      if (DECL_REGISTER (targ) && !TREE_ADDRESSABLE (targ)
	  && !DECL_ARTIFICIAL (targ))
	cp_warning ("address needed to build reference for `%D', which is declared `register'",
		    targ);
      else if (staticp (targ))
	literal_flag = 1;

      TREE_ADDRESSABLE (targ) = 1;
      put_var_into_stack (targ);
      break;

    case COMPOUND_EXPR:
      {
	tree real_reference = build_up_reference (type, TREE_OPERAND (targ, 1),
						  LOOKUP_PROTECT, checkconst);
	rval = build (COMPOUND_EXPR, type, TREE_OPERAND (targ, 0), real_reference);
	TREE_CONSTANT (rval) = staticp (TREE_OPERAND (targ, 1));
	return rval;
      }

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case MODIFY_EXPR:
    case INIT_EXPR:
      {
	tree real_reference = build_up_reference (type, TREE_OPERAND (targ, 0),
						  LOOKUP_PROTECT, checkconst);
	rval = build (COMPOUND_EXPR, type, arg, real_reference);
	TREE_CONSTANT (rval) = staticp (TREE_OPERAND (targ, 0));
	return rval;
      }

    case COND_EXPR:
      return build (COND_EXPR, type,
		    TREE_OPERAND (targ, 0),
		    build_up_reference (type, TREE_OPERAND (targ, 1),
					LOOKUP_PROTECT, checkconst),
		    build_up_reference (type, TREE_OPERAND (targ, 2),
					LOOKUP_PROTECT, checkconst));

      /* Undo the folding...  */
    case MIN_EXPR:
    case MAX_EXPR:
      return build (COND_EXPR, type,
		    build (TREE_CODE (targ) == MIN_EXPR ? LT_EXPR : GT_EXPR,
			   boolean_type_node, TREE_OPERAND (targ, 0),
			   TREE_OPERAND (targ, 1)),
		    build_up_reference (type, TREE_OPERAND (targ, 0),
					LOOKUP_PROTECT, checkconst),
		    build_up_reference (type, TREE_OPERAND (targ, 1),
					LOOKUP_PROTECT, checkconst));

    case BIND_EXPR:
      arg = TREE_OPERAND (targ, 1);
      if (arg == NULL_TREE)
	{
	  compiler_error ("({ ... }) expression not expanded when needed for reference");
	  return error_mark_node;
	}
      rval = build1 (ADDR_EXPR, type, arg);
      TREE_REFERENCE_EXPR (rval) = 1;
      return rval;

    default:
      break;
    }

  if (TREE_ADDRESSABLE (targ) == 0)
    {
      if (! (flags&INDIRECT_BIND)
	  && toplevel_bindings_p ())
	{
	  tree temp = get_temp_name (argtype, 0);
	  /* Give this new temp some rtl and initialize it.  */
	  DECL_INITIAL (temp) = targ;
	  TREE_STATIC (temp) = 1;
	  cp_finish_decl (temp, targ, NULL_TREE, 0, LOOKUP_ONLYCONVERTING);
	  /* Do this after declaring it static.  */
	  rval = build_unary_op (ADDR_EXPR, temp, 0);
	  TREE_TYPE (rval) = type;
	  literal_flag = TREE_CONSTANT (rval);
	  goto done;
	}

      if (TREE_CODE (targ) == CALL_EXPR && IS_AGGR_TYPE (argtype))
	{
	  arg = build_cplus_new (argtype, targ);
	}
      else if (flags&INDIRECT_BIND)
	{
	  /* This should be the default, not the below code.  */
	  /* All callers except grok_reference_init should probably
             use INDIRECT_BIND.  */
	  tree slot = build (VAR_DECL, argtype);
	  layout_decl (slot, 0);
	  arg = build (TARGET_EXPR, argtype, slot, arg, NULL_TREE, NULL_TREE);
	}
      else
	{
	  tree temp = get_temp_name (argtype, 0);
	  rval = build_unary_op (ADDR_EXPR, temp, 0);
	  if (binfo && !BINFO_OFFSET_ZEROP (binfo))
	    rval = convert_pointer_to (target_type, rval);
	  else
	    TREE_TYPE (rval) = type;

	  temp = build (MODIFY_EXPR, argtype, temp, arg);
	  TREE_SIDE_EFFECTS (temp) = 1;
	  return build (COMPOUND_EXPR, type, temp, rval);
	}
    }

  if (! (flags&INDIRECT_BIND))
    {
      if (TREE_CODE (arg) == TARGET_EXPR)
	{
	  tree decl = TREE_OPERAND (arg, 0);
	  tree cleanup;

	  if (! toplevel_bindings_p () && ! DECL_RTL (decl))
	    {
	      expand_decl (decl);
	      cleanup = maybe_build_cleanup (decl);
	      if (cleanup)
		expand_decl_cleanup (decl, cleanup);
	    }
	}
    }

  rval = build1 (ADDR_EXPR, type, arg);

 done:
  if (TYPE_USES_COMPLEX_INHERITANCE (argtype)
      || TYPE_USES_COMPLEX_INHERITANCE (target_type))
    {
      TREE_TYPE (rval) = build_pointer_type (argtype);
      if (flags & LOOKUP_PROTECT)
	rval = convert_pointer_to (target_type, rval);
      else
	rval
	  = convert_to_pointer_force (build_pointer_type (target_type), rval);
      TREE_TYPE (rval) = type;
      if (TREE_CODE (rval) == PLUS_EXPR || TREE_CODE (rval) == MINUS_EXPR)
	TREE_TYPE (TREE_OPERAND (rval, 0))
	  = TREE_TYPE (TREE_OPERAND (rval, 1)) = type;
    }
  TREE_CONSTANT (rval) = literal_flag;
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

  if (TREE_CODE (intype) == REFERENCE_TYPE)
    my_friendly_abort (364);

  intype = TYPE_MAIN_VARIANT (intype);

  i = comp_target_types (type, intype, 0);

  if (i <= 0 && (convtype & CONV_IMPLICIT) && IS_AGGR_TYPE (intype)
      && ! (flags & LOOKUP_NO_CONVERSION))
    {
      /* Look for a user-defined conversion to lvalue that we can use.  */

#ifdef NEW_OVER
      rval_as_conversion
	= build_type_conversion (CONVERT_EXPR, reftype, expr, 1);
      if (rval_as_conversion)
	rval_as_conversion = convert_from_reference (rval_as_conversion);
#else
      rval_as_conversion = build_type_conversion (CONVERT_EXPR, type, expr, 1);
#endif

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
	  tree ttr;
	  
	  {
	    int r = TREE_READONLY (expr);
	    int v = TREE_THIS_VOLATILE (expr);
	    ttr = cp_build_type_variant (TREE_TYPE (expr), r, v);
	  }

	  if (! real_lvalue_p (expr) &&
	      (decl == NULL_TREE || ! TYPE_READONLY (ttl)))
	    {
	      if (decl)
		/* Ensure semantics of [dcl.init.ref] */
		cp_pedwarn ("initialization of non-const `%T' from rvalue `%T'",
			    reftype, intype);
	      else
		cp_pedwarn ("conversion to `%T' from rvalue `%T'",
			    reftype, intype);
	    }
	  else if (! (convtype & CONV_CONST))
	    {
	      if (! TYPE_READONLY (ttl) && TYPE_READONLY (ttr))
		cp_pedwarn ("conversion from `%T' to `%T' discards const",
			    ttr, reftype);
	      else if (! TYPE_VOLATILE (ttl) && TYPE_VOLATILE (ttr))
		cp_pedwarn ("conversion from `%T' to `%T' discards volatile",
			    ttr, reftype);
	    }
	}

      return build_up_reference (reftype, expr, flags,
				 ! (convtype & CONV_CONST));
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
	  && (comptypes (TREE_TYPE (intype), type, -1)))
	cp_warning ("casting `%T' to `%T' does not dereference pointer",
		    intype, reftype);
	  
      rval = build_unary_op (ADDR_EXPR, expr, 0);
      if (rval != error_mark_node)
	rval = convert_force (build_pointer_type (TREE_TYPE (reftype)), rval, 0);
      if (rval != error_mark_node)
	rval = build1 (NOP_EXPR, reftype, rval);
    }
  else if (decl)
    {
      tree rval_as_ctor = NULL_TREE;
      
      if (rval_as_conversion)
	{
	  if (rval_as_conversion == error_mark_node)
	    {
	      cp_error ("conversion from `%T' to `%T' is ambiguous",
			intype, reftype);
	      return error_mark_node;
	    }
	  rval_as_conversion = build_up_reference (reftype, rval_as_conversion,
						   flags, 1);
	}
      
      /* Definitely need to go through a constructor here.  */
      if (TYPE_HAS_CONSTRUCTOR (type)
	  && ! CLASSTYPE_ABSTRACT_VIRTUALS (type)
	  && (rval = build_method_call
	      (NULL_TREE, ctor_identifier,
	       build_tree_list (NULL_TREE, expr), TYPE_BINFO (type),
	       LOOKUP_NO_CONVERSION|LOOKUP_SPECULATIVELY
	       | LOOKUP_ONLYCONVERTING)))
	{
	  tree init;

	  if (toplevel_bindings_p ())
	    {
	      extern tree static_aggregates;
	      tree t = get_temp_name (type, toplevel_bindings_p ());
	      init = build_method_call (t, ctor_identifier,
					build_tree_list (NULL_TREE, expr),
					TYPE_BINFO (type),
					LOOKUP_NORMAL|LOOKUP_NO_CONVERSION
					| LOOKUP_ONLYCONVERTING);

	      if (init == error_mark_node)
		return error_mark_node;

	      make_decl_rtl (t, NULL_PTR, 1);
	      static_aggregates = perm_tree_cons (expr, t, static_aggregates);
	      rval = build_unary_op (ADDR_EXPR, t, 0);
	    }
	  else
	    {
	      init = build_method_call (NULL_TREE, ctor_identifier,
					build_tree_list (NULL_TREE, expr),
					TYPE_BINFO (type),
					LOOKUP_NORMAL|LOOKUP_NO_CONVERSION
					|LOOKUP_ONLYCONVERTING);

	      if (init == error_mark_node)
		return error_mark_node;

	      rval = build_cplus_new (type, init);
	      rval = build_up_reference (reftype, rval, flags, 1);
	    }
	  rval_as_ctor = rval;
	}

      if (rval_as_ctor && rval_as_conversion)
	{
	  cp_error ("ambiguous conversion from `%T' to `%T'; both user-defined conversion and constructor apply",
		    intype, reftype);
	  return error_mark_node;
	}
      else if (rval_as_ctor)
	rval = rval_as_ctor;
      else if (rval_as_conversion)
	rval = rval_as_conversion;
      else if (! IS_AGGR_TYPE (type) && ! IS_AGGR_TYPE (intype))
	{
	  rval = convert (type, expr);
	  if (rval == error_mark_node)
	    return error_mark_node;
	  
	  rval = build_up_reference (reftype, rval, flags, 1);
	}

      if (rval && ! TYPE_READONLY (TREE_TYPE (reftype)))
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

/* See if there is a constructor of type TYPE which will convert
   EXPR.  The reference manual seems to suggest (8.5.6) that we need
   not worry about finding constructors for base classes, then converting
   to the derived class.

   MSGP is a pointer to a message that would be an appropriate error
   string.  If MSGP is NULL, then we are not interested in reporting
   errors.  */

tree
convert_to_aggr (type, expr, msgp, protect)
     tree type, expr;
     char **msgp;
     int protect;
{
  tree basetype = type;
  tree name = TYPE_IDENTIFIER (basetype);
  tree function, fndecl, fntype, parmtypes, parmlist, result;
#if 0
  /* See code below that used this.  */
  tree method_name;
#endif
  tree access;
  int can_be_private, can_be_protected;

  if (! TYPE_HAS_CONSTRUCTOR (basetype))
    {
      if (msgp)
	*msgp = "type `%s' does not have a constructor";
      return error_mark_node;
    }

  access = access_public_node;
  can_be_private = 0;
  can_be_protected = IDENTIFIER_CLASS_VALUE (name) || name == current_class_name;

  parmlist = build_tree_list (NULL_TREE, expr);
  parmtypes = tree_cons (NULL_TREE, TREE_TYPE (expr), void_list_node);

  if (TYPE_USES_VIRTUAL_BASECLASSES (basetype))
    {
      parmtypes = tree_cons (NULL_TREE, integer_type_node, parmtypes);
      parmlist = tree_cons (NULL_TREE, integer_one_node, parmlist);
    }

  /* The type of the first argument will be filled in inside the loop.  */
  parmlist = tree_cons (NULL_TREE, integer_zero_node, parmlist);
  parmtypes = tree_cons (NULL_TREE, build_pointer_type (basetype), parmtypes);

  /* No exact conversion was found.  See if an approximate
     one will do.  */
  fndecl = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), 0);

  {
    int saw_private = 0;
    int saw_protected = 0;
    struct candidate *candidates =
      (struct candidate *) alloca ((decl_list_length (fndecl)+1) * sizeof (struct candidate));
    struct candidate *cp = candidates;

    while (fndecl)
      {
	function = fndecl;
	cp->h_len = 2;
	cp->harshness = (struct harshness_code *)
	  alloca (3 * sizeof (struct harshness_code));

	compute_conversion_costs (fndecl, parmlist, cp, 2);
	if ((cp->h.code & EVIL_CODE) == 0)
	  {
	    cp->u.field = fndecl;
	    if (protect)
	      {
		if (TREE_PRIVATE (fndecl))
		  access = access_private_node;
		else if (TREE_PROTECTED (fndecl))
		  access = access_protected_node;
		else
		  access = access_public_node;
	      }
	    else
	      access = access_public_node;

	    if (access == access_private_node
		? (basetype == current_class_type
		   || is_friend (basetype, cp->function)
		   || purpose_member (basetype, DECL_ACCESS (fndecl)))
		: access == access_protected_node
		? (can_be_protected
		   || purpose_member (basetype, DECL_ACCESS (fndecl)))
		: 1)
	      {
		if (cp->h.code <= TRIVIAL_CODE)
		  goto found_and_ok;
		cp++;
	      }
	    else
	      {
		if (access == access_private_node)
		  saw_private = 1;
		else
		  saw_protected = 1;
	      }
	  }
	fndecl = DECL_CHAIN (fndecl);
      }
    if (cp - candidates)
      {
	/* Rank from worst to best.  Then cp will point to best one.
	   Private fields have their bits flipped.  For unsigned
	   numbers, this should make them look very large.
	   If the best alternate has a (signed) negative value,
	   then all we ever saw were private members.  */
	if (cp - candidates > 1)
	  qsort (candidates,	/* char *base */
		 cp - candidates, /* int nel */
		 sizeof (struct candidate), /* int width */
		 rank_for_overload); /* int (*compar)() */

	--cp;
	if (cp->h.code & EVIL_CODE)
	  {
	    if (msgp)
	      *msgp = "ambiguous type conversion possible for `%s'";
	    return error_mark_node;
	  }

	function = cp->function;
	fndecl = cp->u.field;
	goto found_and_ok;
      }
    else if (msgp)
      {
	if (saw_private)
	  if (saw_protected)
	    *msgp = "only private and protected conversions apply";
	  else
	    *msgp = "only private conversions apply";
	else if (saw_protected)
	  *msgp = "only protected conversions apply";
	else
	  *msgp = "no appropriate conversion to type `%s'";
      }
    return error_mark_node;
  }
  /* NOTREACHED */

 found:
  if (access == access_private_node)
    if (! can_be_private)
      {
	if (msgp)
	  *msgp = TREE_PRIVATE (fndecl)
	    ? "conversion to type `%s' is private"
	    : "conversion to type `%s' is from private base class";
	return error_mark_node;
      }
  if (access == access_protected_node)
    if (! can_be_protected)
      {
	if (msgp)
	  *msgp = TREE_PRIVATE (fndecl)
	    ? "conversion to type `%s' is protected"
	    : "conversion to type `%s' is from protected base class";
	return error_mark_node;
      }
  function = fndecl;
 found_and_ok:

  /* It will convert, but we don't do anything about it yet.  */
  if (msgp == 0)
    return NULL_TREE;

  fntype = TREE_TYPE (function);

  parmlist = convert_arguments (NULL_TREE, TYPE_ARG_TYPES (fntype),
				parmlist, NULL_TREE, LOOKUP_NORMAL);

  result = build_call (function, TREE_TYPE (fntype), parmlist);
  return result;
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

  ptr_type = cp_build_type_variant (type, TYPE_READONLY (TREE_TYPE (intype)),
				    TYPE_VOLATILE (TREE_TYPE (intype)));
  ptr_type = build_pointer_type (ptr_type);
  if (ptr_type == TYPE_MAIN_VARIANT (intype))
    return expr;

  if (intype == error_mark_node)
    return error_mark_node;

  my_friendly_assert (!integer_zerop (expr), 191);

  if (TREE_CODE (type) == RECORD_TYPE
      && TREE_CODE (TREE_TYPE (intype)) == RECORD_TYPE
      && type != TYPE_MAIN_VARIANT (TREE_TYPE (intype)))
    {
      tree path;
      int distance
	= get_base_distance (binfo, TYPE_MAIN_VARIANT (TREE_TYPE (intype)),
			     0, &path);

      /* This function shouldn't be called with unqualified arguments
	 but if it is, give them an error message that they can read.  */
      if (distance < 0)
	{
	  cp_error ("cannot convert a pointer of type `%T' to a pointer of type `%T'",
		    TREE_TYPE (intype), type);

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

/* Conversion...

   FLAGS indicates how we should behave.  */

tree
cp_convert (type, expr, convtype, flags)
     tree type, expr;
     int convtype, flags;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (TREE_CODE (e) == ERROR_MARK
      || TREE_CODE (TREE_TYPE (e)) == ERROR_MARK)
    return error_mark_node;

  if (IS_AGGR_TYPE (type) && (convtype & CONV_FORCE_TEMP))
    /* We need a new temporary; don't take this shortcut.  */;
  else if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (e)))
    /* Trivial conversion: cv-qualifiers do not matter on rvalues.  */
    return fold (build1 (NOP_EXPR, type, e));
  
  if (code == VOID_TYPE && (convtype & CONV_STATIC))
    return build1 (CONVERT_EXPR, type, e);

#if 0
  /* This is incorrect.  A truncation can't be stripped this way.
     Extensions will be stripped by the use of get_unwidened.  */
  if (TREE_CODE (e) == NOP_EXPR)
    return convert (type, TREE_OPERAND (e, 0));
#endif

  /* Just convert to the type of the member.  */
  if (code == OFFSET_TYPE)
    {
      type = TREE_TYPE (type);
      code = TREE_CODE (type);
    }

#if 0
  if (code == REFERENCE_TYPE)
    return fold (convert_to_reference (type, e, convtype, flags, NULL_TREE));
  else if (TREE_CODE (TREE_TYPE (e)) == REFERENCE_TYPE)
    e = convert_from_reference (e);
#endif

  if (TREE_CODE (e) == OFFSET_REF)
    e = resolve_offset_ref (e);

  if (TREE_READONLY_DECL_P (e))
    e = decl_constant_value (e);

  if (INTEGRAL_CODE_P (code))
    {
      tree intype = TREE_TYPE (e);
      /* enum = enum, enum = int, enum = float are all errors.  */
      if (flag_int_enum_equivalence == 0
	  && TREE_CODE (type) == ENUMERAL_TYPE
	  && ARITHMETIC_TYPE_P (intype)
	  && ! (convtype & CONV_STATIC))
	{
	  cp_pedwarn ("conversion from `%#T' to `%#T'", intype, type);

	  if (flag_pedantic_errors)
	    return error_mark_node;
	}
      if (IS_AGGR_TYPE (intype))
	{
	  tree rval;
	  rval = build_type_conversion (CONVERT_EXPR, type, e, 1);
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
	  /* Common Ada/Pascal programmer's mistake.  We always warn
             about this since it is so bad.  */
	  if (TREE_CODE (expr) == FUNCTION_DECL)
	    cp_warning ("the address of `%D', will always be `true'", expr);
	  return truthvalue_conversion (e);
	}
      return fold (convert_to_integer (type, e));
    }
  if (code == POINTER_TYPE || code == REFERENCE_TYPE
      || TYPE_PTRMEMFUNC_P (type))
    return fold (cp_convert_to_pointer (type, e));
  if (code == REAL_TYPE)
    {
      if (IS_AGGR_TYPE (TREE_TYPE (e)))
	{
	  tree rval;
	  rval = build_type_conversion (CONVERT_EXPR, type, e, 1);
	  if (rval)
	    return rval;
	  else
	    if (flags & LOOKUP_COMPLAIN)
	      cp_error ("`%#T' used where a floating point value was expected",
			TREE_TYPE (e));
	}
      return fold (convert_to_real (type, e));
    }

  /* New C++ semantics:  since assignment is now based on
     memberwise copying,  if the rhs type is derived from the
     lhs type, then we may still do a conversion.  */
  if (IS_AGGR_TYPE_CODE (code))
    {
      tree dtype = TREE_TYPE (e);
      tree ctor = NULL_TREE;
      tree conversion = NULL_TREE;

      dtype = TYPE_MAIN_VARIANT (dtype);

      /* Conversion of object pointers or signature pointers/references
	 to signature pointers/references.  */

      if (TYPE_LANG_SPECIFIC (type)
	  && (IS_SIGNATURE_POINTER (type) || IS_SIGNATURE_REFERENCE (type)))
	{
	  tree constructor = build_signature_pointer_constructor (type, expr);
	  tree sig_ty = SIGNATURE_TYPE (type);
	  tree sig_ptr;

	  if (constructor == error_mark_node)
	    return error_mark_node;

	  sig_ptr = get_temp_name (type, 1);
	  DECL_INITIAL (sig_ptr) = constructor;
	  CLEAR_SIGNATURE (sig_ty);
	  cp_finish_decl (sig_ptr, constructor, NULL_TREE, 0, 0);
	  SET_SIGNATURE (sig_ty);
	  TREE_READONLY (sig_ptr) = 1;

	  return sig_ptr;
	}

      /* Conversion between aggregate types.  New C++ semantics allow
	 objects of derived type to be cast to objects of base type.
	 Old semantics only allowed this between pointers.

	 There may be some ambiguity between using a constructor
	 vs. using a type conversion operator when both apply.  */

      if (IS_AGGR_TYPE (dtype) && ! DERIVED_FROM_P (type, dtype)
	  && TYPE_HAS_CONVERSION (dtype))
	conversion = build_type_conversion (CONVERT_EXPR, type, e, 1);

      if (conversion == error_mark_node)
	{
	  if (flags & LOOKUP_COMPLAIN)
	    error ("ambiguous pointer conversion");
	  return conversion;
	}

#ifndef NEW_OVER
      if (TYPE_HAS_CONSTRUCTOR (complete_type (type)))
	ctor = build_method_call (NULL_TREE, ctor_identifier,
				  build_tree_list (NULL_TREE, e),
				  TYPE_BINFO (type),
				  (flags & LOOKUP_NORMAL) | LOOKUP_SPECULATIVELY
				  | (convtype & CONV_NONCONVERTING ? 0 : LOOKUP_ONLYCONVERTING)
				  | (flags & LOOKUP_NO_CONVERSION)
				  | (conversion ? LOOKUP_NO_CONVERSION : 0));

      if (ctor == error_mark_node)
	{
	  if (flags & LOOKUP_COMPLAIN)
	    cp_error ("in conversion to type `%T'", type);
	  if (flags & LOOKUP_SPECULATIVELY)
	    return NULL_TREE;
	  return error_mark_node;
	}
      
      if (conversion && ctor)
	{
	  if (flags & LOOKUP_COMPLAIN)
	    error ("both constructor and type conversion operator apply");
	  if (flags & LOOKUP_SPECULATIVELY)
	    return NULL_TREE;
	  return error_mark_node;
	}
      else if (ctor)
	{
	  ctor = build_cplus_new (type, ctor);
	  return ctor;
	}
#endif
      else if (conversion)
	return conversion;
    }

  /* If TYPE or TREE_TYPE (E) is not on the permanent_obstack,
     then the it won't be hashed and hence compare as not equal,
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

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (type, expr)
     tree type, expr;
{
  return cp_convert (type, expr, CONV_OLD_CONVERT, LOOKUP_NORMAL);
}

/* Like convert, except permit conversions to take place which
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

  return cp_convert (type, e, CONV_C_CAST|convtype, LOOKUP_NORMAL);
}

/* Subroutine of build_type_conversion.  */

static tree
build_type_conversion_1 (xtype, basetype, expr, typename, for_sure)
     tree xtype, basetype;
     tree expr;
     tree typename;
     int for_sure;
{
  tree rval;
  int flags;

  if (for_sure == 0)
    flags = LOOKUP_PROTECT|LOOKUP_ONLYCONVERTING;
  else
    flags = LOOKUP_NORMAL|LOOKUP_ONLYCONVERTING;

  rval = build_method_call (expr, typename, NULL_TREE, NULL_TREE, flags);
  if (rval == error_mark_node)
    {
      if (for_sure == 0)
	return NULL_TREE;
      return error_mark_node;
    }

  if (IS_AGGR_TYPE (TREE_TYPE (rval)))
    return rval;

  if (warn_cast_qual
      && TREE_TYPE (xtype)
      && (TREE_READONLY (TREE_TYPE (TREE_TYPE (rval)))
	  > TREE_READONLY (TREE_TYPE (xtype))))
    warning ("user-defined conversion casting away `const'");
  return convert (xtype, rval);
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
build_type_conversion (code, xtype, expr, for_sure)
     enum tree_code code;
     tree xtype, expr;
     int for_sure;
{
#ifdef NEW_OVER
  return build_user_type_conversion
    (xtype, expr, for_sure ? LOOKUP_NORMAL : 0);
#else
  /* C++: check to see if we can convert this aggregate type
     into the required type.  */
  tree basetype;
  tree conv;
  tree winner = NULL_TREE;

  if (expr == error_mark_node)
    return error_mark_node;

  basetype = TREE_TYPE (expr);
  if (TREE_CODE (basetype) == REFERENCE_TYPE)
    basetype = TREE_TYPE (basetype);

  basetype = TYPE_MAIN_VARIANT (basetype);
  if (! TYPE_LANG_SPECIFIC (basetype) || ! TYPE_HAS_CONVERSION (basetype))
    return NULL_TREE;

  /* Do we have an exact match?  */
  {
    tree typename = build_typename_overload (xtype);
    if (lookup_fnfields (TYPE_BINFO (basetype), typename, 0))
      return build_type_conversion_1 (xtype, basetype, expr, typename,
				      for_sure);
  }

  /* Nope; try looking for others.  */
  for (conv = lookup_conversions (basetype); conv; conv = TREE_CHAIN (conv))
    {
      tree cand = TREE_VALUE (conv);

      if (winner && winner == cand)
	continue;

      if (can_convert (xtype, TREE_TYPE (TREE_TYPE (cand))))
	{
	  if (winner)
	    {
	      if (for_sure)
		{
		  cp_error ("ambiguous conversion from `%T' to `%T'", basetype,
			    xtype);
		  cp_error ("  candidate conversions include `%D' and `%D'",
			    winner, cand);
		}
	      return NULL_TREE;
	    }
	  else
	    winner = cand;
	}
    }

  if (winner)
    return build_type_conversion_1 (xtype, basetype, expr,
				    DECL_NAME (winner), for_sure);

  return NULL_TREE;
#endif
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
  tree conv;
  tree winner = NULL_TREE;

  if (TREE_CODE (basetype) == OFFSET_TYPE)
    expr = resolve_offset_ref (expr);
  expr = convert_from_reference (expr);
  basetype = TREE_TYPE (expr);

  if (! IS_AGGR_TYPE (basetype))
    switch (TREE_CODE (basetype))
      {
      case INTEGER_TYPE:
	if ((desires & WANT_NULL) && TREE_CODE (expr) == INTEGER_CST
	    && integer_zerop (expr))
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
      return build_type_conversion_1 (type, basetype, expr,
				      DECL_NAME (winner), 1);
    }

  return NULL_TREE;
}

/* Must convert two aggregate types to non-aggregate type.
   Attempts to find a non-ambiguous, "best" type conversion.

   Return 1 on success, 0 on failure.

   @@ What are the real semantics of this supposed to be??? */

int
build_default_binary_type_conversion (code, arg1, arg2)
     enum tree_code code;
     tree *arg1, *arg2;
{
  switch (code)
    {
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      *arg1 = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, *arg1, 0);
      *arg2 = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, *arg2, 0);
      break;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case BIT_AND_EXPR:
    case BIT_XOR_EXPR:
    case BIT_IOR_EXPR:
      *arg1 = build_expr_type_conversion (WANT_INT | WANT_ENUM, *arg1, 0);
      *arg2 = build_expr_type_conversion (WANT_INT | WANT_ENUM, *arg2, 0);
      break;

    case PLUS_EXPR:
      {
	tree a1, a2, p1, p2;
	int wins;

	a1 = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, *arg1, 0);
	a2 = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, *arg2, 0);
	p1 = build_expr_type_conversion (WANT_POINTER, *arg1, 0);
	p2 = build_expr_type_conversion (WANT_POINTER, *arg2, 0);

	wins = (a1 && a2) + (a1 && p2) + (p1 && a2);

	if (wins > 1)
	  error ("ambiguous default type conversion for `operator +'");

	if (a1 && a2)
	  *arg1 = a1, *arg2 = a2;
	else if (a1 && p2)
	  *arg1 = a1, *arg2 = p2;
	else
	  *arg1 = p1, *arg2 = a2;
	break;
      }

    case MINUS_EXPR:
      {
	tree a1, a2, p1, p2;
	int wins;

	a1 = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, *arg1, 0);
	a2 = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, *arg2, 0);
	p1 = build_expr_type_conversion (WANT_POINTER, *arg1, 0);
	p2 = build_expr_type_conversion (WANT_POINTER, *arg2, 0);

	wins = (a1 && a2) + (p1 && p2) + (p1 && a2);

	if (wins > 1)
	  error ("ambiguous default type conversion for `operator -'");

	if (a1 && a2)
	  *arg1 = a1, *arg2 = a2;
	else if (p1 && p2)
	  *arg1 = p1, *arg2 = p2;
	else
	  *arg1 = p1, *arg2 = a2;
	break;
      }

    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      {
	tree a1, a2, p1, p2;
	int wins;

	a1 = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, *arg1, 0);
	a2 = build_expr_type_conversion (WANT_ARITH | WANT_ENUM, *arg2, 0);
	p1 = build_expr_type_conversion (WANT_POINTER | WANT_NULL, *arg1, 0);
	p2 = build_expr_type_conversion (WANT_POINTER | WANT_NULL, *arg2, 0);

	wins = (a1 && a2) + (p1 && p2);

	if (wins > 1)
	  cp_error ("ambiguous default type conversion for `%O'", code);

	if (a1 && a2)
	  *arg1 = a1, *arg2 = a2;
	else
	  *arg1 = p1, *arg2 = p2;
	break;
      }

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      *arg1 = convert (boolean_type_node, *arg1);
      *arg2 = convert (boolean_type_node, *arg2);
      break;

    default:
      *arg1 = NULL_TREE;
      *arg2 = NULL_TREE;
    }

  if (*arg1 == error_mark_node || *arg2 == error_mark_node)
    cp_error ("ambiguous default type conversion for `%O'", code);

  if (*arg1 && *arg2)
    return 1;

  return 0;
}

/* Implements integral promotion (4.1) and float->double promotion.  */

tree
type_promotes_to (type)
     tree type;
{
  int constp, volatilep;

  if (type == error_mark_node)
    return error_mark_node;

  constp = TYPE_READONLY (type);
  volatilep = TYPE_VOLATILE (type);
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
      /* Traditionally, unsignedness is preserved in default promotions.
         Otherwise, retain unsignedness if really not getting bigger.  */
      if (TREE_UNSIGNED (type)
	  && (flag_traditional
	      || TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)))
	type = unsigned_type_node;
      else
	type = integer_type_node;
    }
  else if (type == float_type_node)
    type = double_type_node;

  return cp_build_type_variant (type, constp, volatilep);
}

#ifdef NEW_OVER
/* Work in progress.  Ask jason before removing.  */

struct z_candidate {
  tree fn;
  tree convs;
  tree second_conv;
  int viable;
  tree basetype_path;
  tree template;
  struct z_candidate *next;
};

#define EXACT_RANK 0
#define PROMO_RANK 1
#define STD_RANK 2
#define PBOOL_RANK 3
#define USER_RANK 4
#define ELLIPSIS_RANK 5

#define ICS_RANK(NODE)				\
  (ICS_ELLIPSIS_FLAG (NODE) ? ELLIPSIS_RANK	\
   : ICS_USER_FLAG (NODE) ? USER_RANK		\
   : ICS_STD_RANK (NODE))

#define ICS_STD_RANK(NODE) TREE_COMPLEXITY (NODE)

#define ICS_USER_FLAG(NODE) TREE_LANG_FLAG_0 (NODE)
#define ICS_ELLIPSIS_FLAG(NODE) TREE_LANG_FLAG_1 (NODE)

#define USER_CONV_FN(NODE) TREE_OPERAND (NODE, 1)

struct z_candidate * build_user_type_conversion_1 ();
tree convert_like ();
tree build_over_call ();
struct z_candidate * tourney ();

int
null_ptr_cst (t)
     tree t;
{
  return (INTEGRAL_TYPE_P (TREE_TYPE (t)) && integer_zerop (t));
}

tree
build_conv (code, type, from)
     enum tree_code code;
     tree type, from;
{
  tree t = build1 (code, type, from);
  int rank = ICS_STD_RANK (from);
  switch (code)
    {
    case PTR_CONV:
    case PMEM_CONV:
    case BASE_CONV:
    case STD_CONV:
      if (rank < STD_RANK)
	rank = STD_RANK;
      break;

    default:
      break;
    }
  ICS_STD_RANK (t) = rank;
  ICS_USER_FLAG (t) = ICS_USER_FLAG (from);
  return t;
}

tree
non_reference (t)
     tree t;
{
  if (TREE_CODE (t) == REFERENCE_TYPE)
    t = TREE_TYPE (t);
  return t;
}

tree
standard_conversion (to, from, expr)
     tree to, from, expr;
{
  enum tree_code fcode, tcode;
  tree conv;

  fcode = TREE_CODE (from);
  tcode = TREE_CODE (to);

  conv = build1 (EXACT_CONV, from, expr);

  if (from == to)
    return conv;

  if (fcode == FUNCTION_TYPE)
    {
      from = build_pointer_type (from);
      fcode = TREE_CODE (from);
      conv = build_conv (LVALUE_CONV, from, conv);
    }
  else if (fcode == ARRAY_TYPE)
    {
      from = build_pointer_type (TREE_TYPE (from));
      fcode = TREE_CODE (from);
      conv = build_conv (LVALUE_CONV, from, conv);
    }

  if ((tcode == POINTER_TYPE || TYPE_PTRMEMFUNC_P (to))
      && expr && null_ptr_cst (expr))
    {
      conv = build_conv (STD_CONV, to, conv);
    }
  else if (tcode == POINTER_TYPE && fcode == POINTER_TYPE)
    {
      enum tree_code ufcode = TREE_CODE (TREE_TYPE (from));
      enum tree_code utcode = TREE_CODE (TREE_TYPE (to));

      if (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (from)),
		     TYPE_MAIN_VARIANT (TREE_TYPE (to)), 1))
	/* OK for now */;
      else if (utcode == VOID_TYPE && ufcode != OFFSET_TYPE
	       && ufcode != FUNCTION_TYPE)
	{
	  from = build_pointer_type
	    (cp_build_type_variant (void_type_node,
				    TYPE_READONLY (TREE_TYPE (from)),
				    TYPE_VOLATILE (TREE_TYPE (from))));
	  conv = build_conv (PTR_CONV, from, conv);
	}
      else if (ufcode == OFFSET_TYPE && utcode == OFFSET_TYPE)
	{
	  tree fbase = TYPE_OFFSET_BASETYPE (TREE_TYPE (from));
	  tree tbase = TYPE_OFFSET_BASETYPE (TREE_TYPE (to));

	  if (DERIVED_FROM_P (tbase, fbase)
	      && (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (from))),
			     TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (to))),
			     1)))
	    {
	      from = build_offset_type (tbase, TREE_TYPE (TREE_TYPE (from)));
	      from = build_pointer_type (from);
	      conv = build_conv (PMEM_CONV, from, conv);
	    }
	  else
	    return 0;
	}
      else if (IS_AGGR_TYPE (TREE_TYPE (from))
	       && IS_AGGR_TYPE (TREE_TYPE (to)))
	{
	  if (DERIVED_FROM_P (TREE_TYPE (to), TREE_TYPE (from)))
	    {
	      from = cp_build_type_variant (TREE_TYPE (to),
					    TYPE_READONLY (TREE_TYPE (from)),
					    TYPE_VOLATILE (TREE_TYPE (from)));
	      from = build_pointer_type (from);
	      conv = build_conv (PTR_CONV, from, conv);
	    }
	  else
	    return 0;
	}
      else
	return 0;

      if (! comptypes (from, to, 1))
	{
	  if (! comp_ptr_ttypes (TREE_TYPE (to), TREE_TYPE (from)))
	    return 0;

	  from = to;
	  conv = build_conv (QUAL_CONV, from, conv);
	}
    }
  else if (TYPE_PTRMEMFUNC_P (to) && TYPE_PTRMEMFUNC_P (from))
    {
      tree fromfn = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (from));
      tree tofn = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (to));
      tree fbase = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (fromfn)));
      tree tbase = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (tofn)));

      if (! DERIVED_FROM_P (tbase, fbase)
	  || ! comptypes (TREE_TYPE (fromfn), TREE_TYPE (tofn), 1)
	  || ! compparms (TREE_CHAIN (TYPE_ARG_TYPES (fromfn)),
			  TREE_CHAIN (TYPE_ARG_TYPES (tofn)), 1)
	  || TYPE_READONLY (fbase) != TYPE_READONLY (tbase)
	  || TYPE_VOLATILE (fbase) != TYPE_VOLATILE (tbase))
	return 0;

      from = cp_build_type_variant (tbase, TYPE_READONLY (fbase),
				    TYPE_VOLATILE (fbase));
      from = build_cplus_method_type (from, TREE_TYPE (fromfn),
				      TREE_CHAIN (TYPE_ARG_TYPES (fromfn)));
      conv = build_conv (PMEM_CONV, from, conv);
    }
  else if (tcode == BOOLEAN_TYPE)
    {
      if (! (INTEGRAL_CODE_P (fcode) || fcode == REAL_TYPE
	     || fcode == POINTER_TYPE || TYPE_PTRMEMFUNC_P (from)))
	return 0;

      conv = build_conv (STD_CONV, to, conv);
      if (fcode == POINTER_TYPE || TYPE_PTRMEMFUNC_P (from)
	  && ICS_STD_RANK (conv) < PBOOL_RANK)
	ICS_STD_RANK (conv) = PBOOL_RANK;
    }
  else if (INTEGRAL_CODE_P (tcode) || tcode == REAL_TYPE)
    {
      if (! (INTEGRAL_CODE_P (fcode) || fcode == REAL_TYPE))
	return 0;
      conv = build_conv (STD_CONV, to, conv);

      /* Give this a better rank if it's a promotion.  */
      if (to == type_promotes_to (from)
	  && ICS_STD_RANK (TREE_OPERAND (conv, 0)) <= PROMO_RANK)
	ICS_STD_RANK (conv) = PROMO_RANK;
    }
  else if (IS_AGGR_TYPE (to) && IS_AGGR_TYPE (from)
	   && DERIVED_FROM_P (to, from))
    conv = build_conv (BASE_CONV, to, conv);
  else
    return 0;

  return conv;
}

tree
reference_binding (to, from, expr)
     tree to, from, expr;
{
  tree conv;
  int lvalue = 1;
  
  to = TREE_TYPE (to);

  if (TREE_CODE (from) == REFERENCE_TYPE)
    from = TREE_TYPE (from);
  else if (! expr || ! lvalue_p (expr))
    lvalue = 0;

  if (lvalue
      && TYPE_READONLY (to) >= TYPE_READONLY (from)
      && TYPE_VOLATILE (to) >= TYPE_VOLATILE (from))
    {
      conv = build1 (EXACT_CONV, from, expr);

      if (TYPE_MAIN_VARIANT (to) == TYPE_MAIN_VARIANT (from))
	conv = build_conv (REF_BIND, to, conv);
      else if (IS_AGGR_TYPE (to) && IS_AGGR_TYPE (from)
	       && DERIVED_FROM_P (to, from))
	{
	  conv = build_conv (REF_BIND, to, conv);
	  ICS_STD_RANK (conv) = STD_RANK;
	}
      else
	conv = NULL_TREE;
    }
  else
    conv = NULL_TREE;

  if (! conv && TYPE_READONLY (to) && ! TYPE_VOLATILE (to))
    {
      conv = standard_conversion
	(TYPE_MAIN_VARIANT (to), TYPE_MAIN_VARIANT (from), expr);
      if (conv)
	conv = build_conv (REF_BIND, to, conv);
    }

  return conv;
}

tree
implicit_conversion (to, from, expr, flags)
     tree to, from, expr;
     int flags;
{
  tree conv;
  struct z_candidate *cand;

  if (expr && type_unknown_p (expr))
    {
      expr = instantiate_type (to, expr, 0);
      if (expr == error_mark_node)
	return 0;
      from = TREE_TYPE (expr);
    }

  if (TREE_CODE (to) == REFERENCE_TYPE)
    conv = reference_binding (to, from, expr);
  else
    conv = standard_conversion
      (TYPE_MAIN_VARIANT (non_reference (to)),
       TYPE_MAIN_VARIANT (non_reference (from)), expr);

  if (conv)
    ;
  else if ((IS_AGGR_TYPE (non_reference (from))
	    || IS_AGGR_TYPE (non_reference (to)))
	   && (flags & LOOKUP_NO_CONVERSION) == 0)
    {
      cand = build_user_type_conversion_1 (to, expr, LOOKUP_ONLYCONVERTING);
      if (cand)
	conv = cand->second_conv;
      else if (TREE_CODE (to) == REFERENCE_TYPE
	       && TYPE_READONLY (TREE_TYPE (to))
	       && ! TYPE_VOLATILE (TREE_TYPE (to)))
	{
	  cand = build_user_type_conversion_1
	    (TYPE_MAIN_VARIANT (TREE_TYPE (to)), expr, LOOKUP_ONLYCONVERTING);
	  if (cand)
	    conv = build_conv (REF_BIND, TREE_TYPE (to), cand->second_conv);
	}
    }

  return conv;
}

struct z_candidate *
add_function_candidate (candidates, fn, arglist, flags)
     struct z_candidate *candidates;
     tree fn, arglist;
     int flags;
{
  tree parmlist = TYPE_ARG_TYPES (TREE_TYPE (fn));
  int i, len = list_length (arglist);
  tree convs = make_tree_vec (len);
  tree parmnode = parmlist;
  tree argnode = arglist;
  int viable = 1;
  struct z_candidate *cand;

  if (DECL_CONSTRUCTOR_P (fn))
    {
      parmnode = TREE_CHAIN (parmnode);
      if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
	parmnode = TREE_CHAIN (parmnode);
    }

  for (i = 0; i < len; ++i)
    {
      tree arg = TREE_VALUE (argnode);
      tree argtype = TREE_TYPE (arg);
      tree t;

      argtype = cp_build_type_variant
	(argtype, TREE_READONLY (arg), TREE_THIS_VOLATILE (arg));

      if (parmnode == void_list_node)
	break;
      else if (parmnode)
	t = implicit_conversion (TREE_VALUE (parmnode), argtype, arg, flags);
      else
	{
	  t = build1 (EXACT_CONV, argtype, arg);
	  ICS_ELLIPSIS_FLAG (t) = 1;
	}

      TREE_VEC_ELT (convs, i) = t;
      if (! t)
	break;

      if (parmnode)
	parmnode = TREE_CHAIN (parmnode);
      argnode = TREE_CHAIN (argnode);
    }

  if (i < len)
    viable = 0;

  for (; parmnode && parmnode != void_list_node;
       parmnode = TREE_CHAIN (parmnode))
    if (! TREE_PURPOSE (parmnode))
      {
	viable = 0;
	break;
      }

  cand = (struct z_candidate *) oballoc (sizeof (struct z_candidate));

  cand->fn = fn;
  cand->convs = convs;
  cand->second_conv = NULL_TREE;
  cand->viable = viable;
  cand->basetype_path = NULL_TREE;
  cand->template = NULL_TREE;
  cand->next = candidates;

  return cand;
}

struct z_candidate *
add_template_candidate (candidates, tmpl, arglist, flags)
     struct z_candidate *candidates;
     tree tmpl, arglist;
     int flags;
{
  int ntparms = TREE_VEC_LENGTH (DECL_TEMPLATE_PARMS (tmpl));
  tree *targs = (tree *) alloca (sizeof (tree) * ntparms);
  struct z_candidate *cand;
  int i, dummy; 
  tree fn;

  i = type_unification (DECL_TEMPLATE_PARMS (tmpl), targs,
			TYPE_ARG_TYPES (TREE_TYPE (tmpl)),
			arglist, &dummy, 0);
  if (i != 0)
    return candidates;

  fn = instantiate_template (tmpl, targs);
  if (fn == error_mark_node)
    return candidates;

  cand = add_function_candidate (candidates, fn, arglist, flags);
  cand->template = DECL_TEMPLATE_INFO (fn);
  return cand;
}

int
any_viable (cands)
     struct z_candidate *cands;
{
  for (; cands; cands = cands->next)
    if (cands->viable)
      return 1;
  return 0;
}

struct z_candidate *
splice_viable (cands)
     struct z_candidate *cands;
{
  struct z_candidate **p = &cands;

  for (; *p; )
    {
      if ((*p)->viable)
	p = &((*p)->next);
      else
	*p = (*p)->next;
    }

  return cands;
}

tree
build_this (obj)
     tree obj;
{
  /* Fix this to work on non-lvalues.  */
  return build_unary_op (ADDR_EXPR, obj, 0);
}

void
print_z_candidates (candidates)
     struct z_candidate *candidates;
{
  cp_error_at ("candidates are: %D", candidates->fn);
  candidates = candidates->next;

  for (; candidates; candidates = candidates->next)
    cp_error_at ("                %D", candidates->fn);
}

/* Returns the best overload candidate to perform the requested
   conversion.  */

struct z_candidate *
build_user_type_conversion_1 (totype, expr, flags)
     tree totype, expr;
     int flags;
{
  struct z_candidate *candidates, *cand;
  tree fromtype = TREE_TYPE (expr);
  tree ctors = NULL_TREE, convs = NULL_TREE, *p;
  tree args;

  if (IS_AGGR_TYPE (totype))
    ctors = lookup_fnfields (TYPE_BINFO (totype), ctor_identifier, 0);
  if (IS_AGGR_TYPE (fromtype))
    convs = lookup_conversions (fromtype);

  candidates = 0;
  flags |= LOOKUP_NO_CONVERSION;

  if (ctors)
    {
      ctors = TREE_VALUE (ctors);
      args = build_tree_list (NULL_TREE, expr);
    }
  for (; ctors; ctors = DECL_CHAIN (ctors))
    {
      if ((flags & LOOKUP_ONLYCONVERTING) && DECL_NONCONVERTING_P (ctors))
	continue;

      candidates = add_function_candidate (candidates, ctors, args, flags);
      candidates->second_conv = build1 (EXACT_CONV, totype, NULL_TREE);
      candidates->basetype_path = TYPE_BINFO (totype);
    }

  if (convs)
    args = build_tree_list (NULL_TREE, build_this (expr));

  for (; convs; convs = TREE_CHAIN (convs))
    {
      tree fn = TREE_VALUE (convs);
      tree ics = implicit_conversion
	(totype, TREE_TYPE (TREE_TYPE (fn)), 0, LOOKUP_NO_CONVERSION);
      if (ics)
	{
	  candidates = add_function_candidate (candidates, fn, args, flags);
	  candidates->second_conv = ics;
	  candidates->basetype_path = TREE_PURPOSE (convs);
	}
    }

  if (! any_viable (candidates))
    {
#if 0
      if (flags & LOOKUP_COMPLAIN)
	{
	  if (candidates && ! candidates->next)
	    /* say why this one won't work or try to be loose */;
	  else
	    cp_error ("no viable candidates");
	}
#endif

      return 0;
    }

  candidates = splice_viable (candidates);
  cand = tourney (candidates, totype);

  if (cand == 0)
    {
      if (flags & LOOKUP_COMPLAIN)
	{
	  cp_error ("ambiguous user-defined type conversion");
	  print_z_candidates (candidates);
	}

      cand = candidates;	/* any one will do */
      cand->second_conv = build1 (AMBIG_CONV, totype, expr);

      return cand;
    }

  for (p = &(cand->second_conv); TREE_CODE (*p) != EXACT_CONV; )
    p = &(TREE_OPERAND (*p, 0));

  *p = build
    (USER_CONV,
     (DECL_CONSTRUCTOR_P (cand->fn)
      ? totype : non_reference (TREE_TYPE (TREE_TYPE (cand->fn)))),
     NULL_TREE, cand->fn, cand->convs, cand->basetype_path);
  ICS_USER_FLAG (cand->second_conv) = 1;

  return cand;
}

tree
build_user_type_conversion (totype, expr, flags)
     tree totype, expr, flags;
{
  struct z_candidate *cand
    = build_user_type_conversion_1 (totype, expr, flags);

  if (cand)
    {
      if (TREE_CODE (cand->second_conv) == AMBIG_CONV)
	return error_mark_node;
      return convert_like (cand->second_conv, expr);
    }
  return NULL_TREE;
}

tree
build_new_function_call (fn, args, obj)
     tree fn, args, obj;
{
  struct z_candidate *candidates = 0, *cand;
 
  if (obj == NULL_TREE && TREE_CODE (fn) == TREE_LIST)
    {
      tree t = TREE_VALUE (fn);

      for (; t; t = DECL_CHAIN (t))
	{
	  if (TREE_CODE (t) == TEMPLATE_DECL)
	    candidates = add_template_candidate
	      (candidates, t, args, LOOKUP_NORMAL);
	  else
	    candidates = add_function_candidate
	      (candidates, t, args, LOOKUP_NORMAL);
	}

      if (! any_viable (candidates))
	{
	  if (candidates && ! candidates->next)
	    return build_function_call (candidates->fn, args);
	  else
	    cp_error ("no viable candidates");
	  return error_mark_node;
	}
      candidates = splice_viable (candidates);
      cand = tourney (candidates, NULL_TREE);

      if (cand == 0)
	{
	  cp_error ("ambiguous function call");
	  print_z_candidates (candidates);
	  return error_mark_node;
	}

      return build_over_call (cand->fn, cand->convs, args, LOOKUP_NORMAL);
    }

  return build_function_call (fn, args);
}

void
enforce_access (basetype_path, function)
     tree basetype_path, function;
{
  tree access = compute_access (basetype_path, function);

  if (access == access_private_node)
    {
      cp_error_at ("`%+#D' is %s", function, 
		   TREE_PRIVATE (function) ? "private"
		   : "from private base class");
      error ("within this context");
    }
  else if (access == access_protected_node)
    {
      cp_error_at ("`%+#D' %s", function,
		   TREE_PROTECTED (function) ? "is protected"
		   : "has protected accessibility");
      error ("within this context");
    }
}

tree
convert_like (convs, expr)
     tree convs, expr;
{
  switch (TREE_CODE (convs))
    {
    case USER_CONV:
      {
	tree fn = TREE_OPERAND (convs, 1);
	enforce_access (TREE_OPERAND (convs, 3), fn);
	expr = build_over_call
	  (TREE_OPERAND (convs, 1), TREE_OPERAND (convs, 2),
	   DECL_CONSTRUCTOR_P (fn) ? expr : build_this (expr), LOOKUP_NORMAL);

	/* If this is a constructor or a function returning an aggr type,
	   we need to build up a TARGET_EXPR.  */
	if (DECL_CONSTRUCTOR_P (fn)
	    || IS_AGGR_TYPE (TREE_TYPE (TREE_TYPE (fn))))
	  expr = build_cplus_new (TREE_TYPE (convs), expr);

	return expr;
      }
    case EXACT_CONV:
      if (type_unknown_p (expr))
	expr = instantiate_type (TREE_TYPE (convs), expr, 1);
      return expr;
    case AMBIG_CONV:
      /* Call build_user_type_conversion again for the error.  */
      return build_user_type_conversion
	(TREE_TYPE (convs), TREE_OPERAND (convs, 0), LOOKUP_NORMAL);
    };

  expr = convert_like (TREE_OPERAND (convs, 0), expr);
  switch (TREE_CODE (convs))
    {
    case REF_BIND:
      return convert_to_reference
	(build_reference_type (TREE_TYPE (convs)), expr,
	 CONV_IMPLICIT, LOOKUP_NORMAL|LOOKUP_NO_CONVERSION, error_mark_node);
    case LVALUE_CONV:
      return decay_conversion (expr);
    }
  return cp_convert (TREE_TYPE (convs), expr, CONV_IMPLICIT,
		     LOOKUP_NORMAL|LOOKUP_NO_CONVERSION);
}

tree
convert_default_arg (type, arg)
     tree type, arg;
{
  arg = break_out_target_exprs (arg);

  if (TREE_CODE (arg) == CONSTRUCTOR)
    {
      arg = digest_init (type, arg, 0);
      arg = convert_for_initialization (0, type, arg, LOOKUP_NORMAL,
					"default argument", 0, 0);
    }
  else
    {
      /* This could get clobbered by the following call.  */
      if (TREE_HAS_CONSTRUCTOR (arg))
	arg = copy_node (arg);

      arg = convert_for_initialization (0, type, arg, LOOKUP_NORMAL,
					"default argument", 0, 0);
#ifdef PROMOTE_PROTOTYPES
      if ((TREE_CODE (type) == INTEGER_TYPE
	   || TREE_CODE (type) == ENUMERAL_TYPE)
	  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
	arg = default_conversion (arg);
#endif
    }

  return arg;
}

tree
build_over_call (fn, convs, args, flags)
     tree fn, convs, args;
     int flags;
{
  tree converted_args = NULL_TREE;
  tree parm = TYPE_ARG_TYPES (TREE_TYPE (fn));
  tree conv, arg;
  int i;

  if (args && TREE_CODE (args) != TREE_LIST)
    args = build_tree_list (NULL_TREE, args);
  arg = args;

  if (DECL_CONSTRUCTOR_P (fn))
    {
      tree t = build_int_2 (0, 0);
      TREE_TYPE (t) = build_pointer_type (DECL_CONTEXT (fn));
      converted_args = tree_cons (NULL_TREE, t, converted_args);
      parm = TREE_CHAIN (parm);

      if (TYPE_USES_VIRTUAL_BASECLASSES (DECL_CONTEXT (fn)))
	{
	  converted_args = tree_cons
	    (NULL_TREE, integer_one_node, converted_args);
	  parm = TREE_CHAIN (parm);
	}
    }
  /* Bypass access control for 'this' parameter.  */
  else if (TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
    {
      converted_args = tree_cons
	(NULL_TREE, convert_force (TREE_VALUE (parm), TREE_VALUE (arg), CONV_C_CAST),
	 converted_args);
      parm = TREE_CHAIN (parm);
      arg = TREE_CHAIN (arg);
      conv = TREE_CHAIN (conv);
    }

  for (i = 0; conv = TREE_VEC_ELT (convs, i), arg && parm;
       parm = TREE_CHAIN (parm), arg = TREE_CHAIN (arg), ++i)
    converted_args = tree_cons
      (NULL_TREE, convert_like (conv, TREE_VALUE (arg)),
       converted_args);

  /* Default arguments */
  for (; parm && parm != void_list_node; parm = TREE_CHAIN (parm))
    converted_args = tree_cons
      (NULL_TREE,
       convert_default_arg (TREE_VALUE (parm), TREE_PURPOSE (parm)),
       converted_args);

  /* Ellipsis */
  for (; arg; arg = TREE_CHAIN (arg))
    converted_args = tree_cons
      (NULL_TREE, default_conversion (TREE_VALUE (arg)), converted_args);

  converted_args = nreverse (converted_args);

  mark_used (fn);
  /* Is it a synthesized method that needs to be synthesized?  */
  if (DECL_ARTIFICIAL (fn) && ! DECL_INITIAL (fn)
      /* Kludge: don't synthesize for default args.  */
      && current_function_decl)
    synthesize_method (fn);

  if (DECL_VINDEX (fn) && (flags & LOOKUP_NONVIRTUAL) == 0)
    {
      tree t = build_pointer_type (TREE_TYPE (fn));
      fn = build_vfn_ref (&TREE_VALUE (converted_args),
			  build_indirect_ref (TREE_VALUE (args), 0),
			  DECL_VINDEX (fn));
      TREE_TYPE (fn) = t;
    }
  else if (DECL_INLINE (fn))
    fn = inline_conversion (fn);
  else
    fn = build_addr_func (fn);

  return convert_from_reference
    (build_call (fn, TREE_TYPE (TREE_TYPE (TREE_TYPE (fn))), converted_args));
}

/* Compare two implicit conversion sequences that differ only in their
   qualification conversion.  Subroutine of compare_ics.  */

static int
compare_qual (ics1, ics2)
     tree ics1, ics2;
{
  tree to1 = TREE_TYPE (ics1);
  tree to2 = TREE_TYPE (ics2);

  if (TREE_CODE (ics1) != REF_BIND)
    {
      to1 = TREE_TYPE (to1);
      to2 = TREE_TYPE (to2);

      if (TREE_CODE (to1) == OFFSET_TYPE)
	{
	  to1 = TREE_TYPE (to1);
	  to2 = TREE_TYPE (to2);
	}
    }

  if (TYPE_READONLY (to1) >= TYPE_READONLY (to2)
      && TYPE_VOLATILE (to1) > TYPE_VOLATILE (to2))
    return -1;
  else if (TYPE_READONLY (to1) > TYPE_READONLY (to2)
	   && TYPE_VOLATILE (to1) == TYPE_VOLATILE (to2))
    return -1;
  else if (TYPE_READONLY (to1) <= TYPE_READONLY (to2)
	   && TYPE_VOLATILE (to1) < TYPE_VOLATILE (to2))
    return 1;
  else if (TYPE_READONLY (to1) < TYPE_READONLY (to2)
	   && TYPE_VOLATILE (to1) == TYPE_VOLATILE (to2))
    return 1;
  return 0;
}

/* Compare two implicit conversion sequences according to the rules set out in
   [over.ics.rank].  Return values:

      1: ics1 is better than ics2
     -1: ics2 is better than ics1
      0: ics1 and ics2 are indistinguishable */

int
compare_ics (ics1, ics2)
     tree ics1, ics2;
{
  tree main1, main2;

  if (ICS_RANK (ics1) > ICS_RANK (ics2))
    return -1;
  else if (ICS_RANK (ics1) < ICS_RANK (ics2))
    return 1;

  /* User-defined  conversion sequence U1 is a better conversion sequence
     than another user-defined conversion sequence U2 if they contain the
     same user-defined conversion operator or constructor and if the sec-
     ond standard conversion sequence of U1 is  better  than  the  second
     standard conversion sequence of U2.  */

  if (ICS_RANK (ics1) == USER_RANK)
    {
      tree t1, t2;

      for (t1 = ics1; TREE_CODE (t1) != USER_CONV; t1 = TREE_OPERAND (t1, 0))
	;
      for (t2 = ics2; TREE_CODE (t2) != USER_CONV; t2 = TREE_OPERAND (t2, 0))
	;

      if (USER_CONV_FN (t1) != USER_CONV_FN (t2))
	return 0;
      else if (ICS_STD_RANK (ics1) > ICS_STD_RANK (ics2))
	return -1;
      else if (ICS_STD_RANK (ics1) < ICS_STD_RANK (ics2))
	return 1;

      /* else fall through */
    }

#if 0 /* Handled by ranking */
  /* A conversion that is not a conversion of a pointer,  or  pointer  to
     member,  to  bool  is  better than another conversion that is such a
     conversion.  */
#endif

  if (TREE_CODE (ics1) == QUAL_CONV)
    main1 = TREE_OPERAND (ics1, 0);
  else
    main1 = ics1;

  if (TREE_CODE (ics2) == QUAL_CONV)
    main2 = TREE_OPERAND (ics2, 0);
  else
    main2 = ics2;

  if (TREE_CODE (main1) != TREE_CODE (main2))
    return 0;

  if (TREE_CODE (main1) == EXACT_CONV
      && (TREE_CODE (TREE_TYPE (main1)) == POINTER_TYPE
	  || TYPE_PTRMEMFUNC_P (TREE_TYPE (main1))))
    {
      if (TREE_TYPE (main1) == TREE_TYPE (main2))
	return compare_qual (ics1, ics2);

      /* existing practice, not WP-endorsed: const char * -> const char *
	 is better than char * -> const char *.  (jason 6/29/96) */
      if (TREE_TYPE (ics1) == TREE_TYPE (ics2))
	return -compare_qual (main1, main2);
    }

  if (TREE_CODE (main1) == PTR_CONV || TREE_CODE (main1) == PMEM_CONV
      || TREE_CODE (main1) == REF_BIND)
    {
      tree to1 = TREE_TYPE (main1);
      tree from1 = TREE_TYPE (TREE_OPERAND (main1, 0));
      tree to2 = TREE_TYPE (main2);
      tree from2 = TREE_TYPE (TREE_OPERAND (main2, 0));
      int distf, distt;

      /* Standard conversion sequence S1 is a better conversion sequence than
	 standard conversion sequence S2 if...

	 S1 and S2 differ only in their qualification conversion  and  they
	 yield types identical except for cv-qualifiers and S2 adds all the
	 qualifiers that S1 adds (and in the same places) and S2  adds  yet
	 more  cv-qualifiers  than  S1,  or the similar case with reference
	 binding15).  */
      if (TREE_CODE (main1) == REF_BIND)
	{
	  if (TYPE_MAIN_VARIANT (to1) == TYPE_MAIN_VARIANT (to2))
	    return compare_qual (ics1, ics2);
	}
      else if (from1 == from2 && to1 == to2)
	return compare_qual (ics1, ics2);
	
      if (TYPE_PTRMEMFUNC_P (to1))
	{
	  to1 = TYPE_METHOD_BASETYPE (TYPE_PTRMEMFUNC_FN_TYPE (to1));
	  from1 = TYPE_METHOD_BASETYPE (TYPE_PTRMEMFUNC_FN_TYPE (from1));
	}
      else if (TREE_CODE (main1) != REF_BIND)
	{
	  to1 = TREE_TYPE (to1);
	  from1 = TREE_TYPE (from1);

	  if (TREE_CODE (to1) == OFFSET_TYPE)
	    {
	      to1 = TYPE_OFFSET_BASETYPE (to1);
	      from1 = TYPE_OFFSET_BASETYPE (from1);
	    }
	}

      if (TYPE_PTRMEMFUNC_P (to2))
	{
	  to2 = TYPE_METHOD_BASETYPE (TYPE_PTRMEMFUNC_FN_TYPE (to2));
	  from2 = TYPE_METHOD_BASETYPE (TYPE_PTRMEMFUNC_FN_TYPE (from2));
	}
      else if (TREE_CODE (main2) != REF_BIND)
	{
	  to2 = TREE_TYPE (to2);
	  from2 = TREE_TYPE (from2);

	  if (TREE_CODE (to2) == OFFSET_TYPE)
	    {
	      to2 = TYPE_OFFSET_BASETYPE (to2);
	      from2 = TYPE_OFFSET_BASETYPE (from2);
	    }
	}

      if (! (IS_AGGR_TYPE (from1) && IS_AGGR_TYPE (from2)))
	return 0;

      distf = get_base_distance (from1, from2, 0, 0);
      if (distf == -1)
	{
	  distf = -get_base_distance (from2, from1, 0, 0);
	  if (distf == 1)
	    return 0;
	}

      /* If class B is derived directly or indirectly from class A,
	 conver- sion of B* to A* is better than conversion of B* to
	 void*, and conversion of A* to void* is better than
	 conversion of B* to void*.  */

      if (TREE_CODE (to1) == VOID_TYPE && TREE_CODE (to2) == VOID_TYPE)
	{
	  if (distf > 0)
	    return 1;
	  else if (distf < 0)
	    return -1;
	}
      else if (TREE_CODE (to2) == VOID_TYPE && IS_AGGR_TYPE (to1)
	       && get_base_distance (to1, from1, 0, 0) != -1)
	return 1;
      else if (TREE_CODE (to1) == VOID_TYPE && IS_AGGR_TYPE (to2)
	       && get_base_distance (to2, from2, 0, 0) != -1)
	return -1;

      if (! (IS_AGGR_TYPE (to1) && IS_AGGR_TYPE (to2)))
	return 0;

      /* If  class B is derived directly or indirectly from class A and class
	 C is derived directly or indirectly from B */

      distt = get_base_distance (to1, to2, 0, 0);
      if (distt == -1)
	{
	  distt = -get_base_distance (to2, to1, 0, 0);
	  if (distt == 1)
	    return 0;
	}

      /* --conversion of C* to B* is better than conversion of C* to A*, */
      if (distf == 0)
	{
	  if (distt > 0)
	    return -1;
	  else if (distt < 0)
	    return 1;
	}
      /* --conversion of B* to A* is better than conversion of C* to A*, */
      else if (distt == 0)
	{
	  if (distf > 0)
	    return 1;
	  else if (distf < 0)
	    return -1;
	}
    }
  return 0;
}

/* Compare two candidates for overloading as described in
   [over.match.best].  Return values:

      1: cand1 is better than cand2
     -1: cand2 is better than cand1
      0: cand1 and cand2 are indistinguishable */

int joust (cand1, cand2)
     struct z_candidate *cand1, *cand2;
{
  int winner = 0;
  int i;

  /* a viable function F1
     is defined to be a better function than another viable function F2  if
     for  all arguments i, ICSi(F1) is not a worse conversion sequence than
     ICSi(F2), and then */

  /* for some argument j, ICSj(F1) is a better conversion  sequence  than
     ICSj(F2) */

  for (i = 0; i < TREE_VEC_LENGTH (cand1->convs); ++i)
    {
      int comp = compare_ics (TREE_VEC_ELT (cand1->convs, i),
			      TREE_VEC_ELT (cand2->convs, i));

      if (comp != 0)
	{
	  if (winner && comp != winner)
	    return 0;
	  winner = comp;
	}
    }

  if (winner)
    return winner;

  /* or, if not that,
     F1 is a non-template function and F2 is a template function */

  if (! cand1->template && cand2->template)
    return 1;
  else if (cand1->template && ! cand2->template)
    return -1;

  /* or, if not that,
     the  context  is  an  initialization by user-defined conversion (see
     _dcl.init_  and  _over.match.user_)  and  the  standard   conversion
     sequence  from  the return type of F1 to the destination type (i.e.,
     the type of the entity being initialized)  is  a  better  conversion
     sequence  than the standard conversion sequence from the return type
     of F2 to the destination type.  */

  if (cand1->second_conv)
    winner = compare_ics (cand1->second_conv, cand2->second_conv);

  return winner;
}

/* Given a list of candidates for overloading, find the best one, if any.
   This algorithm has a worst case of O(2n) (winner is last), and a best
   case of O(n/2) (totally ambiguous); much better than a sorting
   algorithm.  */

struct z_candidate *
tourney (candidates)
     struct z_candidate *candidates;
{
  struct z_candidate *champ = candidates, *challenger;
  int fate;

  /* Walk through the list once, comparing each current champ to the next
     candidate, knocking out a candidate or two with each comparison.  */

  for (challenger = champ->next; challenger; )
    {
      fate = joust (champ, challenger);
      if (fate == 1)
	challenger = challenger->next;
      else
	{
	  if (fate == 0)
	    {
	      champ = challenger->next;
	      if (champ == 0)
		return 0;
	    }
	  else
	    champ = challenger;

	  challenger = champ->next;
	}
    }

  /* Make sure the champ is better than all the candidates it hasn't yet
     been compared to.  This may do one more comparison than necessary.  Oh
     well.  */

  for (challenger = candidates; challenger != champ;
       challenger = challenger->next)
    {
      fate = joust (champ, challenger);
      if (fate != 1)
	return 0;
    }

  return champ;
}
#endif
