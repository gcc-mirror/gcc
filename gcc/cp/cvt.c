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
#else
      if (TYPE_HAS_CONSTRUCTOR (complete_type (type)) && ! conversion)
#endif
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
      else if (conversion)
	return conversion;
      else if (ctor)
	{
	  ctor = build_cplus_new (type, ctor);
	  return ctor;
	}
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

#define IDENTITY_RANK 0
#define EXACT_RANK 1
#define PROMO_RANK 2
#define STD_RANK 3
#define PBOOL_RANK 4
#define USER_RANK 5
#define ELLIPSIS_RANK 6

#define ICS_RANK(NODE)				\
  (ICS_ELLIPSIS_FLAG (NODE) ? ELLIPSIS_RANK	\
   : ICS_USER_FLAG (NODE) ? USER_RANK		\
   : ICS_STD_RANK (NODE))

#define ICS_STD_RANK(NODE) TREE_COMPLEXITY (NODE)

#define ICS_USER_FLAG(NODE) TREE_LANG_FLAG_0 (NODE)
#define ICS_ELLIPSIS_FLAG(NODE) TREE_LANG_FLAG_1 (NODE)

#define USER_CONV_FN(NODE) TREE_OPERAND (NODE, 1)

static struct z_candidate * build_user_type_conversion_1 ();
static tree convert_like ();
static tree build_over_call ();
static struct z_candidate * tourney ();
static void enforce_access ();

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

    case LVALUE_CONV:
    case QUAL_CONV:
    case RVALUE_CONV:
      if (rank < EXACT_RANK)
	rank = EXACT_RANK;

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

/* Returns the standard conversion path (see [conv]) from type FROM to type
   TO, if any.  For proper handling of null pointer constants, you must
   also pass the expression EXPR to convert from.  */

tree
standard_conversion (to, from, expr)
     tree to, from, expr;
{
  enum tree_code fcode, tcode;
  tree conv;

  fcode = TREE_CODE (from);
  tcode = TREE_CODE (to);

  conv = build1 (IDENTITY_CONV, from, expr);

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

	  if (DERIVED_FROM_P (fbase, tbase)
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

      if (! DERIVED_FROM_P (fbase, tbase)
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
  /* We don't check for ENUMERAL_TYPE here because there are no standard
     conversions to enum type.  */
  else if (tcode == INTEGER_TYPE || tcode == BOOLEAN_TYPE
	   || tcode == REAL_TYPE)
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

/* Returns the conversion path from type FROM to reference type TO for
   purposes of reference binding.  For lvalue binding, either pass a
   reference type to FROM or an lvalue expression to EXPR.

   Currently does not distinguish in the generated trees between binding to
   an lvalue and a temporary.  Should it?  */

tree
reference_binding (rto, from, expr)
     tree rto, from, expr;
{
  tree conv;
  int lvalue = 1;
  tree to = TREE_TYPE (rto);

  if (TREE_CODE (from) == REFERENCE_TYPE)
    from = TREE_TYPE (from);
  else if (! expr || ! real_lvalue_p (expr))
    lvalue = 0;

  if (lvalue
      && TYPE_READONLY (to) >= TYPE_READONLY (from)
      && TYPE_VOLATILE (to) >= TYPE_VOLATILE (from))
    {
      conv = build1 (IDENTITY_CONV, from, expr);

      if (TYPE_MAIN_VARIANT (to) == TYPE_MAIN_VARIANT (from))
	conv = build_conv (REF_BIND, rto, conv);
      else if (IS_AGGR_TYPE (to) && IS_AGGR_TYPE (from)
	       && DERIVED_FROM_P (to, from))
	{
	  conv = build_conv (REF_BIND, rto, conv);
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
	{
	  conv = build_conv (REF_BIND, rto, conv);

	  /* Bind directly to a base subobject of a class rvalue.  */
	  if (TREE_CODE (TREE_OPERAND (conv, 0)) == BASE_CONV)
	    TREE_OPERAND (conv, 0) = TREE_OPERAND (TREE_OPERAND (conv, 0), 0);
	}
    }

  return conv;
}

/* Returns the implicit conversion sequence (see [over.ics]) from type FROM
   to type TO.  The optional expression EXPR may affect the conversion.
   FLAGS are the usual overloading flags.  Only LOOKUP_NO_CONVERSION is
   significant.  */

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
    {
      if (TREE_CODE (conv) == IDENTITY_CONV && IS_AGGR_TYPE (to)
	  && (TREE_CODE (from) == REFERENCE_TYPE || (expr && real_lvalue_p (expr))))
	conv = build_conv (RVALUE_CONV, to, conv);
    }
  else if ((IS_AGGR_TYPE (non_reference (from))
	    || IS_AGGR_TYPE (non_reference (to)))
	   && (flags & LOOKUP_NO_CONVERSION) == 0)
    {
      if (TREE_CODE (to) == REFERENCE_TYPE
	  && TYPE_READONLY (TREE_TYPE (to))
	  && ! TYPE_VOLATILE (TREE_TYPE (to)))
	{
	  cand = build_user_type_conversion_1
	    (TYPE_MAIN_VARIANT (TREE_TYPE (to)), expr, LOOKUP_ONLYCONVERTING);
	  if (cand)
	    conv = build_conv (REF_BIND, to, cand->second_conv);
	}
      else
	{
	  cand = build_user_type_conversion_1
	    (to, expr, LOOKUP_ONLYCONVERTING);
	  if (cand)
	    conv = cand->second_conv;
	}
    }

  return conv;
}

/* Create an overload candidate for the function or method FN called with
   the argument list ARGLIST and add it to CANDIDATES.  FLAGS is passed on
   to implicit_conversion.  */

static struct z_candidate *
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

  /* The `this' and `in_chrg' arguments to constructors are not considered
     in overload resolution.  */
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
	  t = build1 (IDENTITY_CONV, argtype, arg);
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

  /* Make sure there are default args for the rest of the parms.  */
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

/* Create an overload candidate for the conversion function FN which will
   be invoked for expression OBJ, producing a pointer-to-function which
   will in turn be called with the argument list ARGLIST, and add it to
   CANDIDATES.  FLAGS is passed on to implicit_conversion.  */

static struct z_candidate *
add_conv_candidate (candidates, fn, obj, arglist)
     struct z_candidate *candidates;
     tree fn, obj, arglist;
{
  tree totype = TREE_TYPE (TREE_TYPE (fn));
  tree parmlist = TYPE_ARG_TYPES (TREE_TYPE (totype));
  int i, len = list_length (arglist) + 1;
  tree convs = make_tree_vec (len);
  tree parmnode = parmlist;
  tree argnode = arglist;
  int viable = 1;
  struct z_candidate *cand;
  int flags = LOOKUP_NORMAL;

  for (i = 0; i < len; ++i)
    {
      tree arg = i == 0 ? obj : TREE_VALUE (argnode);
      tree argtype = TREE_TYPE (arg);
      tree t;

      argtype = cp_build_type_variant
	(argtype, TREE_READONLY (arg), TREE_THIS_VOLATILE (arg));

      if (i == 0)
	t = implicit_conversion (totype, argtype, arg, flags);
      else if (parmnode == void_list_node)
	break;
      else if (parmnode)
	t = implicit_conversion (TREE_VALUE (parmnode), argtype, arg, flags);
      else
	{
	  t = build1 (IDENTITY_CONV, argtype, arg);
	  ICS_ELLIPSIS_FLAG (t) = 1;
	}

      TREE_VEC_ELT (convs, i) = t;
      if (! t)
	break;

      if (i == 0)
	continue;

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

int
ptr_complete_ob (t)
     tree t;
{
  return (TREE_CODE (t) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (t)) != OFFSET_TYPE
	  && TREE_CODE (TREE_TYPE (t)) != FUNCTION_TYPE
	  && TREE_CODE (TREE_TYPE (t)) != VOID_TYPE
	  && TYPE_SIZE (complete_type (TREE_TYPE (t))) != NULL_TREE);
}

#define TYPE_PTRMEM_P(NODE)					\
  (TREE_CODE (NODE) == POINTER_TYPE				\
   && TREE_CODE (TREE_TYPE (NODE)) == OFFSET_TYPE)
#define TYPE_PTR_P(NODE)				\
  (TREE_CODE (NODE) == POINTER_TYPE			\
   && TREE_CODE (TREE_TYPE (NODE)) != OFFSET_TYPE)
#define TYPE_PTROB_P(NODE)						\
  (TYPE_PTR_P (NODE) && TREE_CODE (TREE_TYPE (NODE)) != FUNCTION_TYPE	\
   && TREE_CODE (TREE_TYPE (NODE)) != VOID_TYPE)

static struct z_candidate *
build_builtin_candidate (candidates, fnname, type1, type2,
			 args, argtypes, flags)
     struct z_candidate *candidates;
     tree fnname, type1, type2, *args, *argtypes;
     int flags;

{
  tree t, convs;
  int viable = 1, i;
  struct z_candidate *cand;
  tree types[2];

  types[0] = type1;
  types[1] = type2;

  convs = make_tree_vec (args[2] ? 3 : (args[1] ? 2 : 1));

  for (i = 0; i < 2; ++i)
    {
      if (! args[i])
	break;

      t = implicit_conversion (types[i], argtypes[i], args[i], flags);
      if (! t)
	{
	  viable = 0;
	  /* We need something for printing the candidate.  */
	  t = build1 (IDENTITY_CONV, types[i], NULL_TREE);
	}
      TREE_VEC_ELT (convs, i) = t;
    }

  /* For COND_EXPR we rearranged the arguments; undo that now.  */
  if (args[2])
    {
      TREE_VEC_ELT (convs, 2) = TREE_VEC_ELT (convs, 1);
      TREE_VEC_ELT (convs, 1) = TREE_VEC_ELT (convs, 0);
      t = implicit_conversion (boolean_type_node, argtypes[2], args[2], flags);
      if (t)
	TREE_VEC_ELT (convs, 0) = t;
      else
	viable = 0;
    }      

  cand = (struct z_candidate *) oballoc (sizeof (struct z_candidate));

  cand->fn = fnname;
  cand->convs = convs;
  cand->second_conv = NULL_TREE;
  cand->viable = viable;
  cand->basetype_path = NULL_TREE;
  cand->template = NULL_TREE;
  cand->next = candidates;

  return cand;
}

int
is_complete (t)
     tree t;
{
  return TYPE_SIZE (complete_type (t)) != NULL_TREE;
}

/* Create any builtin operator overload candidates for the operator in
   question given the converted operand types TYPE1 and TYPE2.  The other
   args are passed through from add_builtin_candidates to
   build_builtin_candidate.  */

static struct z_candidate *
add_builtin_candidate (candidates, code, code2, fnname, type1, type2,
		       args, argtypes, flags)
     struct z_candidate *candidates;
     enum tree_code code, code2;
     tree fnname, type1, type2, *args, *argtypes;
     int flags;
{
  switch (code)
    {
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      args[1] = integer_zero_node;
      type2 = integer_type_node;
    }

  switch (code)
    {

/* 4 For every pair T, VQ), where T is an arithmetic or  enumeration  type,
     and  VQ  is  either  volatile or empty, there exist candidate operator
     functions of the form
	     VQ T&   operator++(VQ T&);
	     T       operator++(VQ T&, int);
   5 For every pair T, VQ), where T is an enumeration type or an arithmetic
     type  other than bool, and VQ is either volatile or empty, there exist
     candidate operator functions of the form
	     VQ T&   operator--(VQ T&);
	     T       operator--(VQ T&, int);
   6 For every pair T, VQ), where T is  a  cv-qualified  or  cv-unqualified
     complete  object type, and VQ is either volatile or empty, there exist
     candidate operator functions of the form
	     T*VQ&   operator++(T*VQ&);
	     T*VQ&   operator--(T*VQ&);
	     T*      operator++(T*VQ&, int);
	     T*      operator--(T*VQ&, int);  */

    case POSTDECREMENT_EXPR:
    case PREDECREMENT_EXPR:
      if (TREE_CODE (type1) == BOOLEAN_TYPE)
	return candidates;
    case POSTINCREMENT_EXPR:
    case PREINCREMENT_EXPR:
      if (ARITHMETIC_TYPE_P (type1) || ptr_complete_ob (type1))
	{
	  type1 = build_reference_type (type1);
	  break;
	}
      return candidates;

/* 7 For every cv-qualified or cv-unqualified complete object type T, there
     exist candidate operator functions of the form

	     T&      operator*(T*);

   8 For every function type T, there exist candidate operator functions of
     the form
	     T&      operator*(T*);  */

    case INDIRECT_REF:
      if (TREE_CODE (type1) == POINTER_TYPE
	  && (ptr_complete_ob (type1)
	      || TREE_CODE (TREE_TYPE (type1)) == FUNCTION_TYPE))
	break;
      return candidates;

/* 9 For every type T, there exist candidate operator functions of the form
	     T*      operator+(T*);

   10For  every  promoted arithmetic type T, there exist candidate operator
     functions of the form
	     T       operator+(T);
	     T       operator-(T);  */

    case CONVERT_EXPR: /* unary + */
      if (TREE_CODE (type1) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (type1)) != OFFSET_TYPE)
	break;
    case NEGATE_EXPR:
      if (ARITHMETIC_TYPE_P (type1))
	break;
      return candidates;

/* 11For every promoted integral type T,  there  exist  candidate  operator
     functions of the form
	     T       operator~(T);  */

    case BIT_NOT_EXPR:
      if (INTEGRAL_TYPE_P (type1))
	break;
      return candidates;

/* 12For every quintuple C1, C2, T, CV1, CV2), where C2 is a class type, C1
     is the same type as C2 or is a derived class of C2, T  is  a  complete
     object type or a function type, and CV1 and CV2 are cv-qualifier-seqs,
     there exist candidate operator functions of the form
	     CV12 T& operator->*(CV1 C1*, CV2 T C2::*);
     where CV12 is the union of CV1 and CV2.  */

    case MEMBER_REF:
      if (TREE_CODE (type1) == POINTER_TYPE
	  && (TYPE_PTRMEMFUNC_P (type2) || TYPE_PTRMEM_P (type2)))
	{
	  tree c1 = TREE_TYPE (type1);
	  tree c2 = (TYPE_PTRMEMFUNC_P (type2)
		     ? TYPE_METHOD_BASETYPE (TYPE_PTRMEMFUNC_FN_TYPE (type2))
		     : TYPE_OFFSET_BASETYPE (TREE_TYPE (type2)));

	  if (IS_AGGR_TYPE (c1) && DERIVED_FROM_P (c2, c1)
	      && (TYPE_PTRMEMFUNC_P (type2)
		  || is_complete (TREE_TYPE (TREE_TYPE (type2)))))
	    break;
	}
      return candidates;

/* 13For every pair of promoted arithmetic types L and R, there exist  can-
     didate operator functions of the form
	     LR      operator*(L, R);
	     LR      operator/(L, R);
	     LR      operator+(L, R);
	     LR      operator-(L, R);
	     bool    operator<(L, R);
	     bool    operator>(L, R);
	     bool    operator<=(L, R);
	     bool    operator>=(L, R);
	     bool    operator==(L, R);
	     bool    operator!=(L, R);
     where  LR  is  the  result of the usual arithmetic conversions between
     types L and R.

   14For every pair of types T and I, where T  is  a  cv-qualified  or  cv-
     unqualified  complete  object  type and I is a promoted integral type,
     there exist candidate operator functions of the form
	     T*      operator+(T*, I);
	     T&      operator[](T*, I);
	     T*      operator-(T*, I);
	     T*      operator+(I, T*);
	     T&      operator[](I, T*);

   15For every T, where T is a pointer to complete object type, there exist
     candidate operator functions of the form112)
	     ptrdiff_t operator-(T, T);

   16For  every pointer type T, there exist candidate operator functions of
     the form
	     bool    operator<(T, T);
	     bool    operator>(T, T);
	     bool    operator<=(T, T);
	     bool    operator>=(T, T);
	     bool    operator==(T, T);
	     bool    operator!=(T, T);

   17For every pointer to member type T,  there  exist  candidate  operator
     functions of the form
	     bool    operator==(T, T);
	     bool    operator!=(T, T);  */

    case MINUS_EXPR:
      if (ptr_complete_ob (type1) && ptr_complete_ob (type2))
	break;
      if (ptr_complete_ob (type1) && INTEGRAL_TYPE_P (type2))
	{
	  type2 = ptrdiff_type_node;
	  break;
	}
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
      return candidates;

    case EQ_EXPR:
    case NE_EXPR:
      if (TYPE_PTRMEMFUNC_P (type1) && TYPE_PTRMEMFUNC_P (type2)
	  || TYPE_PTRMEM_P (type1) && TYPE_PTRMEM_P (type2))
	break;
      if ((TYPE_PTRMEMFUNC_P (type1) || TYPE_PTRMEM_P (type1))
	  && null_ptr_cst (args[1]))
	{
	  type2 = type1;
	  break;
	}
      if ((TYPE_PTRMEMFUNC_P (type2) || TYPE_PTRMEM_P (type2))
	  && null_ptr_cst (args[0]))
	{
	  type1 = type2;
	  break;
	}
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2)
	  || TYPE_PTR_P (type1) && TYPE_PTR_P (type2))
	break;
      if (TYPE_PTR_P (type1) && null_ptr_cst (args[1]))
	{
	  type2 = type1;
	  break;
	}
      if (null_ptr_cst (args[0]) && TYPE_PTR_P (type2))
	{
	  type1 = type2;
	  break;
	}
      return candidates;

    case PLUS_EXPR:
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
    case ARRAY_REF:
      if (INTEGRAL_TYPE_P (type1) && ptr_complete_ob (type2))
	{
	  type1 = ptrdiff_type_node;
	  break;
	}
      if (ptr_complete_ob (type1) && INTEGRAL_TYPE_P (type2))
	{
	  type2 = ptrdiff_type_node;
	  break;
	}
      return candidates;

/* 18For  every pair of promoted integral types L and R, there exist candi-
     date operator functions of the form
	     LR      operator%(L, R);
	     LR      operator&(L, R);
	     LR      operator^(L, R);
	     LR      operator|(L, R);
	     L       operator<<(L, R);
	     L       operator>>(L, R);
     where LR is the result of the  usual  arithmetic  conversions  between
     types L and R.  */

    case TRUNC_MOD_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      if (INTEGRAL_TYPE_P (type1) && INTEGRAL_TYPE_P (type2))
	break;
      return candidates;

/* 19For  every  triple  L, VQ, R), where L is an arithmetic or enumeration
     type, VQ is either volatile or empty, and R is a  promoted  arithmetic
     type, there exist candidate operator functions of the form
	     VQ L&   operator=(VQ L&, R);
	     VQ L&   operator*=(VQ L&, R);
	     VQ L&   operator/=(VQ L&, R);
	     VQ L&   operator+=(VQ L&, R);
	     VQ L&   operator-=(VQ L&, R);

   20For  every  pair T, VQ), where T is any type and VQ is either volatile
     or empty, there exist candidate operator functions of the form
	     T*VQ&   operator=(T*VQ&, T*);

   21For every pair T, VQ), where T is a pointer to member type and  VQ  is
     either  volatile or empty, there exist candidate operator functions of
     the form
	     VQ T&   operator=(VQ T&, T);

   22For every triple  T,  VQ,  I),  where  T  is  a  cv-qualified  or  cv-
     unqualified  complete object type, VQ is either volatile or empty, and
     I is a promoted integral type, there exist  candidate  operator  func-
     tions of the form
	     T*VQ&   operator+=(T*VQ&, I);
	     T*VQ&   operator-=(T*VQ&, I);

   23For  every  triple  L,  VQ,  R), where L is an integral or enumeration
     type, VQ is either volatile or empty, and R  is  a  promoted  integral
     type, there exist candidate operator functions of the form

	     VQ L&   operator%=(VQ L&, R);
	     VQ L&   operator<<=(VQ L&, R);
	     VQ L&   operator>>=(VQ L&, R);
	     VQ L&   operator&=(VQ L&, R);
	     VQ L&   operator^=(VQ L&, R);
	     VQ L&   operator|=(VQ L&, R);  */

    case MODIFY_EXPR:
      switch (code2)
	{
	case PLUS_EXPR:
	case MINUS_EXPR:
	  if (ptr_complete_ob (type1) && INTEGRAL_TYPE_P (type2))
	    {
	      type2 = ptrdiff_type_node;
	      break;
	    }
	case MULT_EXPR:
	case TRUNC_DIV_EXPR:
	  if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	    break;
	  return candidates;

	case TRUNC_MOD_EXPR:
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	  if (INTEGRAL_TYPE_P (type1) && INTEGRAL_TYPE_P (type2))
	    break;
	  return candidates;

	case NOP_EXPR:
	  if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	    break;
	  if ((TYPE_PTRMEMFUNC_P (type1) && TYPE_PTRMEMFUNC_P (type2))
	      || (TYPE_PTR_P (type1) && TYPE_PTR_P (type2))
	      || (TYPE_PTRMEM_P (type1) && TYPE_PTRMEM_P (type2))
	      || ((TYPE_PTRMEMFUNC_P (type1)
		   || TREE_CODE (type1) == POINTER_TYPE)
		  && null_ptr_cst (args[1])))
	    {
	      type2 = type1;
	      break;
	    }
	  return candidates;

	default:
	  my_friendly_abort (367);
	}
      type1 = build_reference_type (type1);
      break;

    case COND_EXPR:
      if (TREE_CODE (type1) == ENUMERAL_TYPE && type1 == type2)
	break;
      else if (TREE_CODE (type1) == ENUMERAL_TYPE
	       || TREE_CODE (type2) == ENUMERAL_TYPE)
	return candidates;
      if (ARITHMETIC_TYPE_P (type1) && ARITHMETIC_TYPE_P (type2))
	break;
      if (TREE_CODE (type1) == TREE_CODE (type2)
	  && (TREE_CODE (type1) == REFERENCE_TYPE
	      || TREE_CODE (type1) == POINTER_TYPE
	      || TYPE_PTRMEMFUNC_P (type1)
	      || IS_AGGR_TYPE (type1)))
	break;
      if (TREE_CODE (type1) == REFERENCE_TYPE
	  || TREE_CODE (type2) == REFERENCE_TYPE)
	return candidates;
      if (((TYPE_PTRMEMFUNC_P (type1) || TREE_CODE (type1) == POINTER_TYPE)
	   && null_ptr_cst (args[1]))
	  || IS_AGGR_TYPE (type1))
	{
	  type2 = type1;
	  break;
	}
      if (((TYPE_PTRMEMFUNC_P (type2) || TREE_CODE (type2) == POINTER_TYPE)
	   && null_ptr_cst (args[0]))
	  || IS_AGGR_TYPE (type2))
	{
	  type1 = type2;
	  break;
	}
      return candidates;

    default:
      my_friendly_abort (367);
    }

  /* If we're dealing with two pointer types, we need candidates
     for both of them.  */
  if (type2 && type1 != type2
      && TREE_CODE (type1) == TREE_CODE (type2)
      && (TREE_CODE (type1) == REFERENCE_TYPE
	  || TREE_CODE (type1) == POINTER_TYPE
	  || TYPE_PTRMEMFUNC_P (type1)
	  || IS_AGGR_TYPE (type1)))
    {
      candidates = build_builtin_candidate
	(candidates, fnname, type1, type1, args, argtypes, flags);
      return build_builtin_candidate
	(candidates, fnname, type2, type2, args, argtypes, flags);
    }

  return build_builtin_candidate
    (candidates, fnname, type1, type2, args, argtypes, flags);
}

tree
type_decays_to (type)
     tree type;
{
  if (TREE_CODE (type) == ARRAY_TYPE)
    return build_pointer_type (TREE_TYPE (type));
  if (TREE_CODE (type) == FUNCTION_TYPE)
    return build_pointer_type (type);
  return type;
}

/* There are three conditions of builtin candidates:

   1) bool-taking candidates.  These are the same regardless of the input.
   2) pointer-pair taking candidates.  These are generated for each type
      one of the input types converts to.
   3) arithmetic candidates.  According to the WP, we should generate
      all of these, but I'm trying not to... */

static struct z_candidate *
add_builtin_candidates (candidates, code, code2, fnname, args, flags)
     struct z_candidate *candidates;
     enum tree_code code, code2;
     tree fnname, *args;
     int flags;
{
  int ref1, i;
  tree type, argtypes[3], types[2];

  for (i = 0; i < 3; ++i)
    {
      if (args[i])
	argtypes[i]  = cp_build_type_variant
	  (TREE_TYPE (args[i]), TREE_READONLY (args[i]),
	   TREE_THIS_VOLATILE (args[i]));
      else
	argtypes[i] = NULL_TREE;
    }

  switch (code)
    {
/* 4 For every pair T, VQ), where T is an arithmetic or  enumeration  type,
     and  VQ  is  either  volatile or empty, there exist candidate operator
     functions of the form
		 VQ T&   operator++(VQ T&);  */

    case POSTINCREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case MODIFY_EXPR:
      ref1 = 1;
      break;

/* 24There also exist candidate operator functions of the form
	     bool    operator!(bool);
	     bool    operator&&(bool, bool);
	     bool    operator||(bool, bool);  */

    case TRUTH_NOT_EXPR:
      return build_builtin_candidate
	(candidates, fnname, boolean_type_node,
	 NULL_TREE, args, argtypes, flags);

    case TRUTH_ORIF_EXPR:
    case TRUTH_ANDIF_EXPR:
      return build_builtin_candidate
	(candidates, fnname, boolean_type_node,
	 boolean_type_node, args, argtypes, flags);

    case ADDR_EXPR:
    case COMPOUND_EXPR:
    case COMPONENT_REF:
      return candidates;

    default:
      ref1 = 0;
    }

  types[0] = types[1] = NULL_TREE;

  for (i = 0; i < 2; ++i)
    {
      if (! args[i])
	;
      else if (IS_AGGR_TYPE (argtypes[i]))
	{
	  tree convs = lookup_conversions (argtypes[i]);

	  if (code == COND_EXPR)
	    {
	      if (real_lvalue_p (args[i]))
		types[i] = tree_cons
		  (NULL_TREE, build_reference_type (argtypes[i]), types[i]);

	      types[i] = tree_cons
		(NULL_TREE, TYPE_MAIN_VARIANT (argtypes[i]), types[i]);
	    }
		
	  else if (! convs || (i == 0 && code == MODIFY_EXPR))
	    return candidates;

	  for (; convs; convs = TREE_CHAIN (convs))
	    {
	      type = TREE_TYPE (TREE_TYPE (TREE_VALUE (convs)));

	      if (i == 0 && ref1
		  && (TREE_CODE (type) != REFERENCE_TYPE
		      || TYPE_READONLY (TREE_TYPE (type))))
		continue;

	      if (code == COND_EXPR && TREE_CODE (type) == REFERENCE_TYPE)
		types[i] = tree_cons (NULL_TREE, type, types[i]);

	      type = non_reference (type);
	      if (i != 0 || ! ref1)
		{
		  type = type_decays_to (TYPE_MAIN_VARIANT (type));
		  if (code == COND_EXPR && TREE_CODE (type) == ENUMERAL_TYPE)
		    types[i] = tree_cons (NULL_TREE, type, types[i]);
		  type = type_promotes_to (type);
		}

	      if (! value_member (type, types[i]))
		types[i] = tree_cons (NULL_TREE, type, types[i]);
	    }
	}
      else
	{
	  if (code == COND_EXPR && real_lvalue_p (args[i]))
	    types[i] = tree_cons
	      (NULL_TREE, build_reference_type (argtypes[i]), types[i]);
	  type = non_reference (argtypes[i]);
	  if (i != 0 || ! ref1)
	    {
	      type = type_decays_to (TYPE_MAIN_VARIANT (type));
	      if (code == COND_EXPR && TREE_CODE (type) == ENUMERAL_TYPE)
		types[i] = tree_cons (NULL_TREE, type, types[i]);
	      type = type_promotes_to (type);
	    }
	  types[i] = tree_cons (NULL_TREE, type, types[i]);
	}
    }

  for (; types[0]; types[0] = TREE_CHAIN (types[0]))
    {
      if (types[1])
	for (type = types[1]; type; type = TREE_CHAIN (type))
	  candidates = add_builtin_candidate
	    (candidates, code, code2, fnname, TREE_VALUE (types[0]),
	     TREE_VALUE (type), args, argtypes, flags);
      else
	candidates = add_builtin_candidate
	  (candidates, code, code2, fnname, TREE_VALUE (types[0]),
	   NULL_TREE, args, argtypes, flags);
    }

  return candidates;
}

static struct z_candidate *
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
			arglist, &dummy, 0, 0);
  if (i != 0)
    return candidates;

  fn = instantiate_template (tmpl, targs);
  if (fn == error_mark_node)
    return candidates;

  cand = add_function_candidate (candidates, fn, arglist, flags);
  cand->template = DECL_TEMPLATE_INFO (fn);
  return cand;
}

static int
any_viable (cands)
     struct z_candidate *cands;
{
  for (; cands; cands = cands->next)
    if (cands->viable)
      return 1;
  return 0;
}

static struct z_candidate *
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

static void
print_z_candidates (candidates)
     struct z_candidate *candidates;
{
  if (! candidates)
    return;

  if (TREE_CODE (candidates->fn) == IDENTIFIER_NODE)
    {
      if (candidates->fn == ansi_opname [COND_EXPR])
	cp_error ("candidates are: %D(%T, %T, %T) <builtin>", candidates->fn,
		  TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)),
		  TREE_TYPE (TREE_VEC_ELT (candidates->convs, 1)),
		  TREE_TYPE (TREE_VEC_ELT (candidates->convs, 2)));
      else if (TREE_VEC_LENGTH (candidates->convs) == 2)
	cp_error ("candidates are: %D(%T, %T) <builtin>", candidates->fn,
		  TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)),
		  TREE_TYPE (TREE_VEC_ELT (candidates->convs, 1)));
      else
	cp_error ("candidates are: %D(%T) <builtin>", candidates->fn,
		  TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)));
    }
  else
    cp_error_at ("candidates are: %D", candidates->fn);
  candidates = candidates->next;

  for (; candidates; candidates = candidates->next)
    {
      if (TREE_CODE (candidates->fn) == IDENTIFIER_NODE)
	{
	  if (candidates->fn == ansi_opname [COND_EXPR])
	    cp_error ("                %D(%T, %T, %T) <builtin>",
		      candidates->fn,
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)),
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 1)),
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 2)));
	  else if (TREE_VEC_LENGTH (candidates->convs) == 2)
	    cp_error ("                %D(%T, %T) <builtin>", candidates->fn,
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)),
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 1)));
	  else
	    cp_error ("                %D(%T) <builtin>", candidates->fn,
		      TREE_TYPE (TREE_VEC_ELT (candidates->convs, 0)));
	}
      else
	cp_error_at ("                %D", candidates->fn);
    }
}

/* Returns the best overload candidate to perform the requested
   conversion.  */

static struct z_candidate *
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
  if (IS_AGGR_TYPE (fromtype)
      && (! IS_AGGR_TYPE (totype) || ! DERIVED_FROM_P (totype, fromtype)))
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
      candidates->second_conv = build1 (IDENTITY_CONV, totype, NULL_TREE);
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
	for (; fn; fn = DECL_CHAIN (fn))
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
      ICS_USER_FLAG (cand->second_conv) = 1;

      return cand;
    }

  for (p = &(cand->second_conv); TREE_CODE (*p) != IDENTITY_CONV; )
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
      return convert_from_reference (convert_like (cand->second_conv, expr));
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

tree
build_object_call (obj, args)
     tree obj, args;
{
  struct z_candidate *candidates = 0, *cand;
  tree fns, convs, mem_args, *p;
  enum tree_code code2 = NOP_EXPR;
  tree type = TREE_TYPE (obj);

  fns = lookup_fnfields (TYPE_BINFO (type), ansi_opname [CALL_EXPR], 0);

  if (fns)
    {
      tree fn = TREE_VALUE (fns);
      mem_args = tree_cons (NULL_TREE, build_this (obj), args);

      for (; fn; fn = DECL_CHAIN (fn))
	{
	  candidates = add_function_candidate
	    (candidates, fn, mem_args, LOOKUP_NORMAL);
	  candidates->basetype_path = TREE_PURPOSE (fns);
	}
    }

  convs = lookup_conversions (type);

  for (; convs; convs = TREE_CHAIN (convs))
    {
      tree fn = TREE_VALUE (convs);
      tree totype = TREE_TYPE (TREE_TYPE (fn));

      if (TREE_CODE (totype) == POINTER_TYPE
	  && TREE_CODE (TREE_TYPE (totype)) == FUNCTION_TYPE)
	{
	  candidates = add_conv_candidate (candidates, fn, obj, args);
	  candidates->basetype_path = TREE_PURPOSE (convs);
	}
    }

  if (! any_viable (candidates))
    {
      cp_error ("no viable candidates");
      print_z_candidates (candidates);
      return error_mark_node;
    }

  candidates = splice_viable (candidates);
  cand = tourney (candidates, NULL_TREE);

  if (cand == 0)
    {
      cp_error ("ambiguous object call");
      print_z_candidates (candidates);
      return error_mark_node;
    }

  if (DECL_NAME (cand->fn) == ansi_opname [CALL_EXPR])
    return build_over_call (cand->fn, cand->convs, mem_args, LOOKUP_NORMAL);

  obj = convert_like (TREE_VEC_ELT (cand->convs, 0), obj);

  /* FIXME */
  return build_function_call (obj, args);
}

static void
op_error (code, code2, arg1, arg2, arg3, problem)
     enum tree_code code, code2;
     tree arg1, arg2, arg3;
     char *problem;
{
  char * opname
    = (code == MODIFY_EXPR ? assignop_tab [code2] : opname_tab [code]);

  switch (code)
    {
    case COND_EXPR:
      cp_error ("%s for `%T ? %T : %T'", problem,
		TREE_TYPE (arg1), TREE_TYPE (arg2), TREE_TYPE (arg3));
      break;
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      cp_error ("%s for `%T%s'", problem, TREE_TYPE (arg1), opname);
      break;
    case ARRAY_REF:
      cp_error ("%s for `%T[%T]'", problem,
		TREE_TYPE (arg1), TREE_TYPE (arg2));
      break;
    default:
      if (arg2)
	cp_error ("%s for `%T %s %T'", problem,
		  TREE_TYPE (arg1), opname, TREE_TYPE (arg2));
      else
	cp_error ("%s for `%s%T'", problem, opname, TREE_TYPE (arg1));
    }
}

tree
build_new_op (code, flags, arg1, arg2, arg3)
     enum tree_code code;
     int flags;
     tree arg1, arg2, arg3;
{
  struct z_candidate *candidates = 0, *cand;
  tree fns, mem_arglist, arglist, fnname, *p;
  enum tree_code code2 = NOP_EXPR;

  if (arg1 == error_mark_node)
    return error_mark_node;

  if (code == MODIFY_EXPR)
    {
      code2 = TREE_CODE (arg3);
      arg3 = NULL_TREE;
      fnname = ansi_assopname[code2];
    }
  else
    fnname = ansi_opname[code];

  switch (code)
    {
    case NEW_EXPR:
    case VEC_NEW_EXPR:
      {
	tree rval;

	arglist = tree_cons (NULL_TREE, arg2, arg3);
	if (flags & LOOKUP_GLOBAL)
	  return build_new_function_call
	    (lookup_name_nonclass (fnname), arglist, NULL_TREE);

	/* FIXME */
	rval = build_method_call
	  (build_indirect_ref (build1 (NOP_EXPR, arg1, error_mark_node),
			       "new"),
	   fnname, arglist, NULL_TREE, flags);
	if (rval == error_mark_node)
	  /* User might declare fancy operator new, but invoke it
	     like standard one.  */
	  return rval;

	TREE_TYPE (rval) = arg1;
	TREE_CALLS_NEW (rval) = 1;
	return rval;
      }

    case VEC_DELETE_EXPR:
    case DELETE_EXPR:
      {
	tree rval;

	if (flags & LOOKUP_GLOBAL)
	  return build_new_function_call
	    (lookup_name_nonclass (fnname),
	     build_tree_list (NULL_TREE, arg1), NULL_TREE);

	arglist = tree_cons (NULL_TREE, arg1, build_tree_list (NULL_TREE, arg2));

	arg1 = TREE_TYPE (arg1);

	/* This handles the case where we're trying to delete
	   X (*a)[10];
	   a=new X[5][10];
	   delete[] a; */
	   
	if (TREE_CODE (TREE_TYPE (arg1)) == ARRAY_TYPE)
	  {
	    /* Strip off the pointer and the array.  */
	    arg1 = TREE_TYPE (TREE_TYPE (arg1));

	    while (TREE_CODE (arg1) == ARRAY_TYPE)
		arg1 = (TREE_TYPE (arg1));

	    arg1 = build_pointer_type (arg1);
	  }

	/* FIXME */
	rval = build_method_call
	  (build_indirect_ref (build1 (NOP_EXPR, arg1,
				       error_mark_node),
			       NULL_PTR),
	   fnname, arglist, NULL_TREE, flags);
#if 0
	/* This can happen when operator delete is protected.  */
	my_friendly_assert (rval != error_mark_node, 250);
	TREE_TYPE (rval) = void_type_node;
#endif
	return rval;
      }

    case CALL_EXPR:
      return build_object_call (arg1, arg2);
    }

  /* The comma operator can have void args.  */
  if (TREE_CODE (arg1) == OFFSET_REF)
    arg1 = resolve_offset_ref (arg1);
  if (arg2 && TREE_CODE (arg2) == OFFSET_REF)
    arg2 = resolve_offset_ref (arg2);
  if (arg3 && TREE_CODE (arg3) == OFFSET_REF)
    arg3 = resolve_offset_ref (arg3);

  if (! IS_OVERLOAD_TYPE (TREE_TYPE (arg1))
      && (! arg2 || ! IS_OVERLOAD_TYPE (TREE_TYPE (arg2)))
      && (! arg3 || ! IS_OVERLOAD_TYPE (TREE_TYPE (arg3))))
    return NULL_TREE;

  if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
    arg2 = integer_zero_node;

  fns = lookup_name_nonclass (fnname);
  /* + Koenig lookup */

  if (arg2 && arg3)
    arglist = tree_cons (NULL_TREE, arg1, tree_cons
		      (NULL_TREE, arg2, build_tree_list (NULL_TREE, arg3)));
  else if (arg2)
    arglist = tree_cons (NULL_TREE, arg1, build_tree_list (NULL_TREE, arg2));
  else
    arglist = build_tree_list (NULL_TREE, arg1);

  if (fns && TREE_CODE (fns) == TREE_LIST)
    fns = TREE_VALUE (fns);
  for (; fns; fns = DECL_CHAIN (fns))
    {
      if (TREE_CODE (fns) == TEMPLATE_DECL)
	candidates = add_template_candidate (candidates, fns, arglist, flags);
      else
	candidates = add_function_candidate (candidates, fns, arglist, flags);
    }

  if (IS_AGGR_TYPE (TREE_TYPE (arg1)))
    fns = lookup_fnfields (TYPE_BINFO (TREE_TYPE (arg1)), fnname, 0);
  else
    fns = NULL_TREE;

  if (fns)
    {
      tree fn = TREE_VALUE (fns);
      mem_arglist = tree_cons (NULL_TREE, build_this (arg1), TREE_CHAIN (arglist));
      for (; fn; fn = DECL_CHAIN (fn))
	{
	  if (TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE)
	    candidates = add_function_candidate
	      (candidates, fn, mem_arglist, flags);
	  else
	    candidates = add_function_candidate (candidates, fn, arglist, flags);
	  
	  candidates->basetype_path = TREE_PURPOSE (fns);
	}
    }

#if 0 /* Don't handle builtin COND_EXPR for now */
  if (code != COND_EXPR)
#endif
    {
      tree args[3];

      /* Rearrange the arguments for ?: so that add_builtin_candidate only has
	 to know about two args; a builtin candidate will always have a first
	 parameter of type bool.  We'll handle that in
	 build_builtin_candidate.  */
      if (code == COND_EXPR)
	{
	  args[0] = arg2;
	  args[1] = arg3;
	  args[2] = arg1;
	}
      else
	{
	  args[0] = arg1;
	  args[1] = arg2;
	  args[2] = NULL_TREE;
	}

      candidates = add_builtin_candidates
	(candidates, code, code2, fnname, args, flags);
    }

  if (! any_viable (candidates))
    {
      switch (code)
	{
	case POSTINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	  /* Look for an `operator++ (int)'.  If they didn't have
	     one, then we fall back to the old way of doing things.  */
	  if (flags & LOOKUP_COMPLAIN)
	    cp_pedwarn ("no `%D (int)' declared for postfix `%s', trying prefix operator instead",
			fnname, opname_tab [code]);
	  if (code == POSTINCREMENT_EXPR)
	    code = PREINCREMENT_EXPR;
	  else
	    code = PREDECREMENT_EXPR;	
	  return build_new_op (code, flags, arg1, NULL_TREE, NULL_TREE);
	  
	  /* FIXME */
	case ADDR_EXPR:
	  /*return build_unary_op (code, arg1, 1);*/
	case COMPOUND_EXPR:
	  /*return build (COMPOUND_EXPR, TREE_TYPE (arg2),
			break_out_cleanups (arg1), arg2);*/
	case COMPONENT_REF:
	  /*return build_x_arrow (arg1);*/
#if 0 /* Don't handle builtin COND_EXPR for now */
	case COND_EXPR:
#endif
	  return NULL_TREE;
	}
      if (flags & LOOKUP_COMPLAIN)
	{
	  op_error (code, code2, arg1, arg2, arg3, "no match");
	  print_z_candidates (candidates);
	}
      return error_mark_node;
    }
  candidates = splice_viable (candidates);
  cand = tourney (candidates, NULL_TREE);

  if (cand == 0)
    {
      if (flags & LOOKUP_COMPLAIN)
	{
	  op_error (code, code2, arg1, arg2, arg3, "ambiguous overload");
	  print_z_candidates (candidates);
	}
      return error_mark_node;
    }

  if (TREE_CODE (cand->fn) == FUNCTION_DECL)
    {
      extern int warn_synth;
      if (warn_synth
	  && fnname == ansi_opname[MODIFY_EXPR]
	  && DECL_ARTIFICIAL (cand->fn)
	  && candidates->next
	  && ! candidates->next->next)
	{
	  cp_warning ("using synthesized `%#D' for copy assignment",
		      cand->fn);
	  cp_warning_at ("  where cfront would use `%#D'",
			 cand == candidates
			 ? candidates->next->fn
			 : candidates->fn);
	}

      if (DECL_FUNCTION_MEMBER_P (cand->fn))
	enforce_access (cand->basetype_path, cand->fn);

      return build_over_call
	(cand->fn, cand->convs,
	 TREE_CODE (TREE_TYPE (cand->fn)) == METHOD_TYPE
	 ? mem_arglist : arglist,
	 LOOKUP_NORMAL);
    }

  arg1 = convert_from_reference
    (convert_like (TREE_VEC_ELT (cand->convs, 0), arg1));
  if (arg2)
    arg2 = convert_like (TREE_VEC_ELT (cand->convs, 1), arg2);
  if (arg3)
    arg3 = convert_like (TREE_VEC_ELT (cand->convs, 2), arg3);

  switch (code)
    {
    case MODIFY_EXPR:
      return build_modify_expr (arg1, code2, arg2);

    case INDIRECT_REF:
      return build_indirect_ref (arg1, "unary *");

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case GT_EXPR:
    case LT_EXPR:
    case GE_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case TRUNC_MOD_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      return build_binary_op_nodefault (code, arg1, arg2, code);

    case CONVERT_EXPR:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      return build_unary_op (code, arg1, 1);

    case ARRAY_REF:
      return build_array_ref (arg1, arg2);

    case COND_EXPR:
      return build_conditional_expr (arg1, arg2, arg3);

    default:
      my_friendly_abort (367);
    }
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

/* Perform the conversions in CONVS on the expression EXPR.  */

static tree
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
    case IDENTITY_CONV:
      if (type_unknown_p (expr))
	expr = instantiate_type (TREE_TYPE (convs), expr, 1);
      return expr;
    case AMBIG_CONV:
      /* Call build_user_type_conversion again for the error.  */
      return build_user_type_conversion
	(TREE_TYPE (convs), TREE_OPERAND (convs, 0), LOOKUP_NORMAL);
    };

  expr = convert_like (TREE_OPERAND (convs, 0), expr);
  if (expr == error_mark_node)
    return error_mark_node;

  switch (TREE_CODE (convs))
    {
    case BASE_CONV:
    case RVALUE_CONV:
      return build_user_type_conversion
	(TREE_TYPE (convs), expr, LOOKUP_NORMAL);
    case REF_BIND:
      return convert_to_reference
	(TREE_TYPE (convs), expr,
	 CONV_IMPLICIT, LOOKUP_NORMAL|LOOKUP_NO_CONVERSION|INDIRECT_BIND,
	 error_mark_node);
    case LVALUE_CONV:
      return decay_conversion (expr);
    }
  return cp_convert (TREE_TYPE (convs), expr, CONV_IMPLICIT,
		     LOOKUP_NORMAL|LOOKUP_NO_CONVERSION);
}

static tree
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

static tree
build_over_call (fn, convs, args, flags)
     tree fn, convs, args;
     int flags;
{
  tree converted_args = NULL_TREE;
  tree parm = TYPE_ARG_TYPES (TREE_TYPE (fn));
  tree conv, arg, val;
  int i = 0;

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
      ++i;
    }

  for (; conv = TREE_VEC_ELT (convs, i), arg && parm;
       parm = TREE_CHAIN (parm), arg = TREE_CHAIN (arg), ++i)
    {
      tree type = TREE_VALUE (parm);
      val = convert_like (conv, TREE_VALUE (arg));

#ifdef PROMOTE_PROTOTYPES
      if ((TREE_CODE (type) == INTEGER_TYPE
	   || TREE_CODE (type) == ENUMERAL_TYPE)
	  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
	val = default_conversion (val);
#endif
      converted_args = tree_cons (NULL_TREE, val, converted_args);
    }

  /* Default arguments */
  for (; parm && parm != void_list_node; parm = TREE_CHAIN (parm))
    converted_args = tree_cons
      (NULL_TREE,
       convert_default_arg (TREE_VALUE (parm), TREE_PURPOSE (parm)),
       converted_args);

  /* Ellipsis */
  for (; arg; arg = TREE_CHAIN (arg))
    {
      val = TREE_VALUE (arg);

      if (TREE_CODE (TREE_TYPE (val)) == REAL_TYPE
	  && (TYPE_PRECISION (TREE_TYPE (val))
	      < TYPE_PRECISION (double_type_node)))
	/* Convert `float' to `double'.  */
	val = convert (double_type_node, val);
      else if (TYPE_LANG_SPECIFIC (TREE_TYPE (val))
	       && ! TYPE_HAS_TRIVIAL_INIT_REF (TREE_TYPE (val)))
	cp_warning ("cannot pass objects of type `%T' through `...'",
		    TREE_TYPE (val));
      else
	/* Convert `short' and `char' to full-size `int'.  */
	val = default_conversion (val);

      converted_args = tree_cons (NULL_TREE, val, converted_args);
    }

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

  fn = build_call (fn, TREE_TYPE (TREE_TYPE (TREE_TYPE (fn))), converted_args);
  if (TREE_TYPE (fn) == void_type_node)
    return fn;
  return convert_from_reference (require_complete_type (fn));
}

/* Compare two implicit conversion sequences that differ only in their
   qualification conversion.  Subroutine of compare_ics.  */

static int
compare_qual (ics1, ics2)
     tree ics1, ics2;
{
  tree to1 = TREE_TYPE (ics1);
  tree to2 = TREE_TYPE (ics2);

  to1 = TREE_TYPE (to1);
  to2 = TREE_TYPE (to2);

  if (TREE_CODE (to1) == OFFSET_TYPE)
    {
      to1 = TREE_TYPE (to1);
      to2 = TREE_TYPE (to2);
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

static int
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
	if (TREE_CODE (t1) == AMBIG_CONV)
	  return 0;
      for (t2 = ics2; TREE_CODE (t2) != USER_CONV; t2 = TREE_OPERAND (t2, 0))
	if (TREE_CODE (t2) == AMBIG_CONV)
	  return 0;

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

  if (TREE_CODE (main1) == IDENTITY_CONV
      && (TREE_CODE (TREE_TYPE (main1)) == POINTER_TYPE
	  || TYPE_PTRMEMFUNC_P (TREE_TYPE (main1))))
    {
      if (TREE_TYPE (main1) == TREE_TYPE (main2))
	return compare_qual (ics1, ics2);

#if 0 /* This is now handled by making identity better than anything else.  */
      /* existing practice, not WP-endorsed: const char * -> const char *
	 is better than char * -> const char *.  (jason 6/29/96) */
      if (TREE_TYPE (ics1) == TREE_TYPE (ics2))
	return -compare_qual (main1, main2);
#endif
    }

  if (TREE_CODE (main1) == PTR_CONV || TREE_CODE (main1) == PMEM_CONV
      || TREE_CODE (main1) == REF_BIND || TREE_CODE (main1) == BASE_CONV)
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
	  if (TYPE_MAIN_VARIANT (TREE_TYPE (to1))
	      == TYPE_MAIN_VARIANT (TREE_TYPE (to2)))
	    return compare_qual (ics1, ics2);
	}
      else if (TREE_CODE (main1) != BASE_CONV && from1 == from2 && to1 == to2)
	return compare_qual (ics1, ics2);
	
      if (TYPE_PTRMEMFUNC_P (to1))
	{
	  to1 = TYPE_METHOD_BASETYPE (TYPE_PTRMEMFUNC_FN_TYPE (to1));
	  from1 = TYPE_METHOD_BASETYPE (TYPE_PTRMEMFUNC_FN_TYPE (from1));
	}
      else if (TREE_CODE (main1) != BASE_CONV)
	{
	  to1 = TREE_TYPE (to1);
	  if (TREE_CODE (main1) != REF_BIND)
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
      else if (TREE_CODE (main1) != BASE_CONV)
	{
	  to2 = TREE_TYPE (to2);
	  if (TREE_CODE (main1) != REF_BIND)
	    from2 = TREE_TYPE (from2);

	  if (TREE_CODE (to2) == OFFSET_TYPE)
	    {
	      to2 = TYPE_OFFSET_BASETYPE (to2);
	      from2 = TYPE_OFFSET_BASETYPE (from2);
	    }
	}

      if (! (IS_AGGR_TYPE (from1) && IS_AGGR_TYPE (from2)))
	return 0;

      /* The sense of pmem conversions is reversed from that of the other
	 conversions.  */
      if (TREE_CODE (main1) == PMEM_CONV)
	{
	  tree t = from1; from1 = from2; from2 = t;
	  t = to1; to1 = to2; to2 = t;
	}

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

static int
joust (cand1, cand2)
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
  else if (cand1->template && cand2->template)
    winner = more_specialized
      (TI_TEMPLATE (cand1->template), TI_TEMPLATE (cand2->template));

  /* or, if not that,
     the  context  is  an  initialization by user-defined conversion (see
     _dcl.init_  and  _over.match.user_)  and  the  standard   conversion
     sequence  from  the return type of F1 to the destination type (i.e.,
     the type of the entity being initialized)  is  a  better  conversion
     sequence  than the standard conversion sequence from the return type
     of F2 to the destination type.  */

  if (! winner && cand1->second_conv)
    winner = compare_ics (cand1->second_conv, cand2->second_conv);

  /* If the built-in candidates are the same, arbitrarily pick one.  */
  if (! winner && cand1->fn == cand2->fn
      && TREE_CODE (cand1->fn) == IDENTIFIER_NODE)
    {
      for (i = 0; i < TREE_VEC_LENGTH (cand1->convs); ++i)
	if (! comptypes (TREE_TYPE (TREE_VEC_ELT (cand1->convs, i)),
			 TREE_TYPE (TREE_VEC_ELT (cand2->convs, i)), 1))
	  break;
      if (i == TREE_VEC_LENGTH (cand1->convs))
	return 1;
    }

  return winner;
}

/* Given a list of candidates for overloading, find the best one, if any.
   This algorithm has a worst case of O(2n) (winner is last), and a best
   case of O(n/2) (totally ambiguous); much better than a sorting
   algorithm.  */

static struct z_candidate *
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
