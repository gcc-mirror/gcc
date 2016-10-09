/* Gimple decl, type, and expression support functions.

   Copyright (C) 2007-2016 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "stringpool.h"
#include "gimple-ssa.h"
#include "fold-const.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "stor-layout.h"
#include "demangle.h"
#include "hash-set.h"
#include "rtl.h"

/* ----- Type related -----  */

/* Return true if the conversion from INNER_TYPE to OUTER_TYPE is a
   useless type conversion, otherwise return false.

   This function implicitly defines the middle-end type system.  With
   the notion of 'a < b' meaning that useless_type_conversion_p (a, b)
   holds and 'a > b' meaning that useless_type_conversion_p (b, a) holds,
   the following invariants shall be fulfilled:

     1) useless_type_conversion_p is transitive.
	If a < b and b < c then a < c.

     2) useless_type_conversion_p is not symmetric.
	From a < b does not follow a > b.

     3) Types define the available set of operations applicable to values.
	A type conversion is useless if the operations for the target type
	is a subset of the operations for the source type.  For example
	casts to void* are useless, casts from void* are not (void* can't
	be dereferenced or offsetted, but copied, hence its set of operations
	is a strict subset of that of all other data pointer types).  Casts
	to const T* are useless (can't be written to), casts from const T*
	to T* are not.  */

bool
useless_type_conversion_p (tree outer_type, tree inner_type)
{
  /* Do the following before stripping toplevel qualifiers.  */
  if (POINTER_TYPE_P (inner_type)
      && POINTER_TYPE_P (outer_type))
    {
      /* Do not lose casts between pointers to different address spaces.  */
      if (TYPE_ADDR_SPACE (TREE_TYPE (outer_type))
	  != TYPE_ADDR_SPACE (TREE_TYPE (inner_type)))
	return false;
      /* Do not lose casts to function pointer types.  */
      if ((TREE_CODE (TREE_TYPE (outer_type)) == FUNCTION_TYPE
	   || TREE_CODE (TREE_TYPE (outer_type)) == METHOD_TYPE)
	  && !(TREE_CODE (TREE_TYPE (inner_type)) == FUNCTION_TYPE
	       || TREE_CODE (TREE_TYPE (inner_type)) == METHOD_TYPE))
	return false;
    }

  /* From now on qualifiers on value types do not matter.  */
  inner_type = TYPE_MAIN_VARIANT (inner_type);
  outer_type = TYPE_MAIN_VARIANT (outer_type);

  if (inner_type == outer_type)
    return true;

  /* Changes in machine mode are never useless conversions because the RTL
     middle-end expects explicit conversions between modes.  */
  if (TYPE_MODE (inner_type) != TYPE_MODE (outer_type))
    return false;

  /* If both the inner and outer types are integral types, then the
     conversion is not necessary if they have the same mode and
     signedness and precision, and both or neither are boolean.  */
  if (INTEGRAL_TYPE_P (inner_type)
      && INTEGRAL_TYPE_P (outer_type))
    {
      /* Preserve changes in signedness or precision.  */
      if (TYPE_UNSIGNED (inner_type) != TYPE_UNSIGNED (outer_type)
	  || TYPE_PRECISION (inner_type) != TYPE_PRECISION (outer_type))
	return false;

      /* Preserve conversions to/from BOOLEAN_TYPE if types are not
	 of precision one.  */
      if (((TREE_CODE (inner_type) == BOOLEAN_TYPE)
	   != (TREE_CODE (outer_type) == BOOLEAN_TYPE))
	  && TYPE_PRECISION (outer_type) != 1)
	return false;

      /* We don't need to preserve changes in the types minimum or
	 maximum value in general as these do not generate code
	 unless the types precisions are different.  */
      return true;
    }

  /* Scalar floating point types with the same mode are compatible.  */
  else if (SCALAR_FLOAT_TYPE_P (inner_type)
	   && SCALAR_FLOAT_TYPE_P (outer_type))
    return true;

  /* Fixed point types with the same mode are compatible.  */
  else if (FIXED_POINT_TYPE_P (inner_type)
	   && FIXED_POINT_TYPE_P (outer_type))
    return true;

  /* We need to take special care recursing to pointed-to types.  */
  else if (POINTER_TYPE_P (inner_type)
	   && POINTER_TYPE_P (outer_type))
    {
      /* We do not care for const qualification of the pointed-to types
	 as const qualification has no semantic value to the middle-end.  */

      /* Otherwise pointers/references are equivalent.  */
      return true;
    }

  /* Recurse for complex types.  */
  else if (TREE_CODE (inner_type) == COMPLEX_TYPE
	   && TREE_CODE (outer_type) == COMPLEX_TYPE)
    return useless_type_conversion_p (TREE_TYPE (outer_type),
				      TREE_TYPE (inner_type));

  /* Recurse for vector types with the same number of subparts.  */
  else if (TREE_CODE (inner_type) == VECTOR_TYPE
	   && TREE_CODE (outer_type) == VECTOR_TYPE
	   && TYPE_PRECISION (inner_type) == TYPE_PRECISION (outer_type))
    return useless_type_conversion_p (TREE_TYPE (outer_type),
				      TREE_TYPE (inner_type));

  else if (TREE_CODE (inner_type) == ARRAY_TYPE
	   && TREE_CODE (outer_type) == ARRAY_TYPE)
    {
      /* Preserve various attributes.  */
      if (TYPE_REVERSE_STORAGE_ORDER (inner_type)
	  != TYPE_REVERSE_STORAGE_ORDER (outer_type))
	return false;
      if (TYPE_STRING_FLAG (inner_type) != TYPE_STRING_FLAG (outer_type))
	return false;

      /* Conversions from array types with unknown extent to
	 array types with known extent are not useless.  */
      if (!TYPE_DOMAIN (inner_type) && TYPE_DOMAIN (outer_type))
	return false;

      /* Nor are conversions from array types with non-constant size to
         array types with constant size or to different size.  */
      if (TYPE_SIZE (outer_type)
	  && TREE_CODE (TYPE_SIZE (outer_type)) == INTEGER_CST
	  && (!TYPE_SIZE (inner_type)
	      || TREE_CODE (TYPE_SIZE (inner_type)) != INTEGER_CST
	      || !tree_int_cst_equal (TYPE_SIZE (outer_type),
				      TYPE_SIZE (inner_type))))
	return false;

      /* Check conversions between arrays with partially known extents.
	 If the array min/max values are constant they have to match.
	 Otherwise allow conversions to unknown and variable extents.
	 In particular this declares conversions that may change the
	 mode to BLKmode as useless.  */
      if (TYPE_DOMAIN (inner_type)
	  && TYPE_DOMAIN (outer_type)
	  && TYPE_DOMAIN (inner_type) != TYPE_DOMAIN (outer_type))
	{
	  tree inner_min = TYPE_MIN_VALUE (TYPE_DOMAIN (inner_type));
	  tree outer_min = TYPE_MIN_VALUE (TYPE_DOMAIN (outer_type));
	  tree inner_max = TYPE_MAX_VALUE (TYPE_DOMAIN (inner_type));
	  tree outer_max = TYPE_MAX_VALUE (TYPE_DOMAIN (outer_type));

	  /* After gimplification a variable min/max value carries no
	     additional information compared to a NULL value.  All that
	     matters has been lowered to be part of the IL.  */
	  if (inner_min && TREE_CODE (inner_min) != INTEGER_CST)
	    inner_min = NULL_TREE;
	  if (outer_min && TREE_CODE (outer_min) != INTEGER_CST)
	    outer_min = NULL_TREE;
	  if (inner_max && TREE_CODE (inner_max) != INTEGER_CST)
	    inner_max = NULL_TREE;
	  if (outer_max && TREE_CODE (outer_max) != INTEGER_CST)
	    outer_max = NULL_TREE;

	  /* Conversions NULL / variable <- cst are useless, but not
	     the other way around.  */
	  if (outer_min
	      && (!inner_min
		  || !tree_int_cst_equal (inner_min, outer_min)))
	    return false;
	  if (outer_max
	      && (!inner_max
		  || !tree_int_cst_equal (inner_max, outer_max)))
	    return false;
	}

      /* Recurse on the element check.  */
      return useless_type_conversion_p (TREE_TYPE (outer_type),
					TREE_TYPE (inner_type));
    }

  else if ((TREE_CODE (inner_type) == FUNCTION_TYPE
	    || TREE_CODE (inner_type) == METHOD_TYPE)
	   && TREE_CODE (inner_type) == TREE_CODE (outer_type))
    {
      tree outer_parm, inner_parm;

      /* If the return types are not compatible bail out.  */
      if (!useless_type_conversion_p (TREE_TYPE (outer_type),
				      TREE_TYPE (inner_type)))
	return false;

      /* Method types should belong to a compatible base class.  */
      if (TREE_CODE (inner_type) == METHOD_TYPE
	  && !useless_type_conversion_p (TYPE_METHOD_BASETYPE (outer_type),
					 TYPE_METHOD_BASETYPE (inner_type)))
	return false;

      /* A conversion to an unprototyped argument list is ok.  */
      if (!prototype_p (outer_type))
	return true;

      /* If the unqualified argument types are compatible the conversion
	 is useless.  */
      if (TYPE_ARG_TYPES (outer_type) == TYPE_ARG_TYPES (inner_type))
	return true;

      for (outer_parm = TYPE_ARG_TYPES (outer_type),
	   inner_parm = TYPE_ARG_TYPES (inner_type);
	   outer_parm && inner_parm;
	   outer_parm = TREE_CHAIN (outer_parm),
	   inner_parm = TREE_CHAIN (inner_parm))
	if (!useless_type_conversion_p
	       (TYPE_MAIN_VARIANT (TREE_VALUE (outer_parm)),
		TYPE_MAIN_VARIANT (TREE_VALUE (inner_parm))))
	  return false;

      /* If there is a mismatch in the number of arguments the functions
	 are not compatible.  */
      if (outer_parm || inner_parm)
	return false;

      /* Defer to the target if necessary.  */
      if (TYPE_ATTRIBUTES (inner_type) || TYPE_ATTRIBUTES (outer_type))
	return comp_type_attributes (outer_type, inner_type) != 0;

      return true;
    }

  /* For aggregates we rely on TYPE_CANONICAL exclusively and require
     explicit conversions for types involving to be structurally
     compared types.  */
  else if (AGGREGATE_TYPE_P (inner_type)
	   && TREE_CODE (inner_type) == TREE_CODE (outer_type))
    return TYPE_CANONICAL (inner_type)
	   && TYPE_CANONICAL (inner_type) == TYPE_CANONICAL (outer_type);

  else if (TREE_CODE (inner_type) == OFFSET_TYPE
	   && TREE_CODE (outer_type) == OFFSET_TYPE)
    return useless_type_conversion_p (TREE_TYPE (outer_type),
				      TREE_TYPE (inner_type))
	   && useless_type_conversion_p
	        (TYPE_OFFSET_BASETYPE (outer_type),
		 TYPE_OFFSET_BASETYPE (inner_type));

  return false;
}


/* ----- Decl related -----  */

/* Set sequence SEQ to be the GIMPLE body for function FN.  */

void
gimple_set_body (tree fndecl, gimple_seq seq)
{
  struct function *fn = DECL_STRUCT_FUNCTION (fndecl);
  if (fn == NULL)
    {
      /* If FNDECL still does not have a function structure associated
	 with it, then it does not make sense for it to receive a
	 GIMPLE body.  */
      gcc_assert (seq == NULL);
    }
  else
    fn->gimple_body = seq;
}


/* Return the body of GIMPLE statements for function FN.  After the
   CFG pass, the function body doesn't exist anymore because it has
   been split up into basic blocks.  In this case, it returns
   NULL.  */

gimple_seq
gimple_body (tree fndecl)
{
  struct function *fn = DECL_STRUCT_FUNCTION (fndecl);
  return fn ? fn->gimple_body : NULL;
}

/* Return true when FNDECL has Gimple body either in unlowered
   or CFG form.  */
bool
gimple_has_body_p (tree fndecl)
{
  struct function *fn = DECL_STRUCT_FUNCTION (fndecl);
  return (gimple_body (fndecl) || (fn && fn->cfg));
}

/* Return a printable name for symbol DECL.  */

const char *
gimple_decl_printable_name (tree decl, int verbosity)
{
  if (!DECL_NAME (decl))
    return NULL;

  if (DECL_ASSEMBLER_NAME_SET_P (decl))
    {
      const char *str, *mangled_str;
      int dmgl_opts = DMGL_NO_OPTS;

      if (verbosity >= 2)
	{
	  dmgl_opts = DMGL_VERBOSE
		      | DMGL_ANSI
		      | DMGL_GNU_V3
		      | DMGL_RET_POSTFIX;
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    dmgl_opts |= DMGL_PARAMS;
	}

      mangled_str = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      str = cplus_demangle_v3 (mangled_str, dmgl_opts);
      return (str) ? str : mangled_str;
    }

  return IDENTIFIER_POINTER (DECL_NAME (decl));
}


/* Create a new VAR_DECL and copy information from VAR to it.  */

tree
copy_var_decl (tree var, tree name, tree type)
{
  tree copy = build_decl (DECL_SOURCE_LOCATION (var), VAR_DECL, name, type);

  TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (var);
  TREE_THIS_VOLATILE (copy) = TREE_THIS_VOLATILE (var);
  DECL_GIMPLE_REG_P (copy) = DECL_GIMPLE_REG_P (var);
  DECL_ARTIFICIAL (copy) = DECL_ARTIFICIAL (var);
  DECL_IGNORED_P (copy) = DECL_IGNORED_P (var);
  DECL_CONTEXT (copy) = DECL_CONTEXT (var);
  TREE_NO_WARNING (copy) = TREE_NO_WARNING (var);
  TREE_USED (copy) = 1;
  DECL_SEEN_IN_BIND_EXPR_P (copy) = 1;
  DECL_ATTRIBUTES (copy) = DECL_ATTRIBUTES (var);
  if (DECL_USER_ALIGN (var))
    {
      SET_DECL_ALIGN (copy, DECL_ALIGN (var));
      DECL_USER_ALIGN (copy) = 1;
    }

  return copy;
}

/* Strip off a legitimate source ending from the input string NAME of
   length LEN.  Rather than having to know the names used by all of
   our front ends, we strip off an ending of a period followed by
   up to five characters.  (Java uses ".class".)  */

static inline void
remove_suffix (char *name, int len)
{
  int i;

  for (i = 2;  i < 8 && len > i;  i++)
    {
      if (name[len - i] == '.')
	{
	  name[len - i] = '\0';
	  break;
	}
    }
}

/* Create a new temporary name with PREFIX.  Return an identifier.  */

static GTY(()) unsigned int tmp_var_id_num;

tree
create_tmp_var_name (const char *prefix)
{
  char *tmp_name;

  if (prefix)
    {
      char *preftmp = ASTRDUP (prefix);

      remove_suffix (preftmp, strlen (preftmp));
      clean_symbol_name (preftmp);

      prefix = preftmp;
    }

  ASM_FORMAT_PRIVATE_NAME (tmp_name, prefix ? prefix : "T", tmp_var_id_num++);
  return get_identifier (tmp_name);
}

/* Create a new temporary variable declaration of type TYPE.
   Do NOT push it into the current binding.  */

tree
create_tmp_var_raw (tree type, const char *prefix)
{
  tree tmp_var;

  tmp_var = build_decl (input_location,
			VAR_DECL, prefix ? create_tmp_var_name (prefix) : NULL,
			type);

  /* The variable was declared by the compiler.  */
  DECL_ARTIFICIAL (tmp_var) = 1;
  /* And we don't want debug info for it.  */
  DECL_IGNORED_P (tmp_var) = 1;

  /* Make the variable writable.  */
  TREE_READONLY (tmp_var) = 0;

  DECL_EXTERNAL (tmp_var) = 0;
  TREE_STATIC (tmp_var) = 0;
  TREE_USED (tmp_var) = 1;

  return tmp_var;
}

/* Create a new temporary variable declaration of type TYPE.  DO push the
   variable into the current binding.  Further, assume that this is called
   only from gimplification or optimization, at which point the creation of
   certain types are bugs.  */

tree
create_tmp_var (tree type, const char *prefix)
{
  tree tmp_var;

  /* We don't allow types that are addressable (meaning we can't make copies),
     or incomplete.  We also used to reject every variable size objects here,
     but now support those for which a constant upper bound can be obtained.
     The processing for variable sizes is performed in gimple_add_tmp_var,
     point at which it really matters and possibly reached via paths not going
     through this function, e.g. after direct calls to create_tmp_var_raw.  */
  gcc_assert (!TREE_ADDRESSABLE (type) && COMPLETE_TYPE_P (type));

  tmp_var = create_tmp_var_raw (type, prefix);
  gimple_add_tmp_var (tmp_var);
  return tmp_var;
}

/* Create a new temporary variable declaration of type TYPE by calling
   create_tmp_var and if TYPE is a vector or a complex number, mark the new
   temporary as gimple register.  */

tree
create_tmp_reg (tree type, const char *prefix)
{
  tree tmp;

  tmp = create_tmp_var (type, prefix);
  if (TREE_CODE (type) == COMPLEX_TYPE
      || TREE_CODE (type) == VECTOR_TYPE)
    DECL_GIMPLE_REG_P (tmp) = 1;

  return tmp;
}

/* Create a new temporary variable declaration of type TYPE by calling
   create_tmp_var and if TYPE is a vector or a complex number, mark the new
   temporary as gimple register.  */

tree
create_tmp_reg_fn (struct function *fn, tree type, const char *prefix)
{
  tree tmp;

  tmp = create_tmp_var_raw (type, prefix);
  gimple_add_tmp_var_fn (fn, tmp);
  if (TREE_CODE (type) == COMPLEX_TYPE
      || TREE_CODE (type) == VECTOR_TYPE)
    DECL_GIMPLE_REG_P (tmp) = 1;

  return tmp;
}


/* ----- Expression related -----  */

/* Extract the operands and code for expression EXPR into *SUBCODE_P,
   *OP1_P, *OP2_P and *OP3_P respectively.  */

void
extract_ops_from_tree (tree expr, enum tree_code *subcode_p, tree *op1_p,
		       tree *op2_p, tree *op3_p)
{
  enum gimple_rhs_class grhs_class;

  *subcode_p = TREE_CODE (expr);
  grhs_class = get_gimple_rhs_class (*subcode_p);

  if (grhs_class == GIMPLE_TERNARY_RHS)
    {
      *op1_p = TREE_OPERAND (expr, 0);
      *op2_p = TREE_OPERAND (expr, 1);
      *op3_p = TREE_OPERAND (expr, 2);
    }
  else if (grhs_class == GIMPLE_BINARY_RHS)
    {
      *op1_p = TREE_OPERAND (expr, 0);
      *op2_p = TREE_OPERAND (expr, 1);
      *op3_p = NULL_TREE;
    }
  else if (grhs_class == GIMPLE_UNARY_RHS)
    {
      *op1_p = TREE_OPERAND (expr, 0);
      *op2_p = NULL_TREE;
      *op3_p = NULL_TREE;
    }
  else if (grhs_class == GIMPLE_SINGLE_RHS)
    {
      *op1_p = expr;
      *op2_p = NULL_TREE;
      *op3_p = NULL_TREE;
    }
  else
    gcc_unreachable ();
}

/* Extract operands for a GIMPLE_COND statement out of COND_EXPR tree COND.  */

void
gimple_cond_get_ops_from_tree (tree cond, enum tree_code *code_p,
                               tree *lhs_p, tree *rhs_p)
{
  gcc_assert (COMPARISON_CLASS_P (cond)
	      || TREE_CODE (cond) == TRUTH_NOT_EXPR
	      || is_gimple_min_invariant (cond)
	      || SSA_VAR_P (cond));

  extract_ops_from_tree (cond, code_p, lhs_p, rhs_p);

  /* Canonicalize conditionals of the form 'if (!VAL)'.  */
  if (*code_p == TRUTH_NOT_EXPR)
    {
      *code_p = EQ_EXPR;
      gcc_assert (*lhs_p && *rhs_p == NULL_TREE);
      *rhs_p = build_zero_cst (TREE_TYPE (*lhs_p));
    }
  /* Canonicalize conditionals of the form 'if (VAL)'  */
  else if (TREE_CODE_CLASS (*code_p) != tcc_comparison)
    {
      *code_p = NE_EXPR;
      gcc_assert (*lhs_p && *rhs_p == NULL_TREE);
      *rhs_p = build_zero_cst (TREE_TYPE (*lhs_p));
    }
}

/*  Return true if T is a valid LHS for a GIMPLE assignment expression.  */

bool
is_gimple_lvalue (tree t)
{
  return (is_gimple_addressable (t)
	  || TREE_CODE (t) == WITH_SIZE_EXPR
	  /* These are complex lvalues, but don't have addresses, so they
	     go here.  */
	  || TREE_CODE (t) == BIT_FIELD_REF);
}

/*  Return true if T is a GIMPLE condition.  */

bool
is_gimple_condexpr (tree t)
{
  return (is_gimple_val (t) || (COMPARISON_CLASS_P (t)
				&& !tree_could_throw_p (t)
				&& is_gimple_val (TREE_OPERAND (t, 0))
				&& is_gimple_val (TREE_OPERAND (t, 1))));
}

/* Return true if T is a gimple address.  */

bool
is_gimple_address (const_tree t)
{
  tree op;

  if (TREE_CODE (t) != ADDR_EXPR)
    return false;

  op = TREE_OPERAND (t, 0);
  while (handled_component_p (op))
    {
      if ((TREE_CODE (op) == ARRAY_REF
	   || TREE_CODE (op) == ARRAY_RANGE_REF)
	  && !is_gimple_val (TREE_OPERAND (op, 1)))
	    return false;

      op = TREE_OPERAND (op, 0);
    }

  if (CONSTANT_CLASS_P (op) || TREE_CODE (op) == MEM_REF)
    return true;

  switch (TREE_CODE (op))
    {
    case PARM_DECL:
    case RESULT_DECL:
    case LABEL_DECL:
    case FUNCTION_DECL:
    case VAR_DECL:
    case CONST_DECL:
      return true;

    default:
      return false;
    }
}

/* Return true if T is a gimple invariant address.  */

bool
is_gimple_invariant_address (const_tree t)
{
  const_tree op;

  if (TREE_CODE (t) != ADDR_EXPR)
    return false;

  op = strip_invariant_refs (TREE_OPERAND (t, 0));
  if (!op)
    return false;

  if (TREE_CODE (op) == MEM_REF)
    {
      const_tree op0 = TREE_OPERAND (op, 0);
      return (TREE_CODE (op0) == ADDR_EXPR
	      && (CONSTANT_CLASS_P (TREE_OPERAND (op0, 0))
		  || decl_address_invariant_p (TREE_OPERAND (op0, 0))));
    }

  return CONSTANT_CLASS_P (op) || decl_address_invariant_p (op);
}

/* Return true if T is a gimple invariant address at IPA level
   (so addresses of variables on stack are not allowed).  */

bool
is_gimple_ip_invariant_address (const_tree t)
{
  const_tree op;

  if (TREE_CODE (t) != ADDR_EXPR)
    return false;

  op = strip_invariant_refs (TREE_OPERAND (t, 0));
  if (!op)
    return false;

  if (TREE_CODE (op) == MEM_REF)
    {
      const_tree op0 = TREE_OPERAND (op, 0);
      return (TREE_CODE (op0) == ADDR_EXPR
	      && (CONSTANT_CLASS_P (TREE_OPERAND (op0, 0))
		  || decl_address_ip_invariant_p (TREE_OPERAND (op0, 0))));
    }

  return CONSTANT_CLASS_P (op) || decl_address_ip_invariant_p (op);
}

/* Return true if T is a GIMPLE minimal invariant.  It's a restricted
   form of function invariant.  */

bool
is_gimple_min_invariant (const_tree t)
{
  if (TREE_CODE (t) == ADDR_EXPR)
    return is_gimple_invariant_address (t);

  return is_gimple_constant (t);
}

/* Return true if T is a GIMPLE interprocedural invariant.  It's a restricted
   form of gimple minimal invariant.  */

bool
is_gimple_ip_invariant (const_tree t)
{
  if (TREE_CODE (t) == ADDR_EXPR)
    return is_gimple_ip_invariant_address (t);

  return is_gimple_constant (t);
}

/* Return true if T is a non-aggregate register variable.  */

bool
is_gimple_reg (tree t)
{
  if (virtual_operand_p (t))
    return false;

  if (TREE_CODE (t) == SSA_NAME)
    return true;

  if (!is_gimple_variable (t))
    return false;

  if (!is_gimple_reg_type (TREE_TYPE (t)))
    return false;

  /* A volatile decl is not acceptable because we can't reuse it as
     needed.  We need to copy it into a temp first.  */
  if (TREE_THIS_VOLATILE (t))
    return false;

  /* We define "registers" as things that can be renamed as needed,
     which with our infrastructure does not apply to memory.  */
  if (needs_to_live_in_memory (t))
    return false;

  /* Hard register variables are an interesting case.  For those that
     are call-clobbered, we don't know where all the calls are, since
     we don't (want to) take into account which operations will turn
     into libcalls at the rtl level.  For those that are call-saved,
     we don't currently model the fact that calls may in fact change
     global hard registers, nor do we examine ASM_CLOBBERS at the tree
     level, and so miss variable changes that might imply.  All around,
     it seems safest to not do too much optimization with these at the
     tree level at all.  We'll have to rely on the rtl optimizers to
     clean this up, as there we've got all the appropriate bits exposed.  */
  if (TREE_CODE (t) == VAR_DECL && DECL_HARD_REGISTER (t))
    return false;

  /* Complex and vector values must have been put into SSA-like form.
     That is, no assignments to the individual components.  */
  if (TREE_CODE (TREE_TYPE (t)) == COMPLEX_TYPE
      || TREE_CODE (TREE_TYPE (t)) == VECTOR_TYPE)
    return DECL_GIMPLE_REG_P (t);

  return true;
}


/* Return true if T is a GIMPLE rvalue, i.e. an identifier or a constant.  */

bool
is_gimple_val (tree t)
{
  /* Make loads from volatiles and memory vars explicit.  */
  if (is_gimple_variable (t)
      && is_gimple_reg_type (TREE_TYPE (t))
      && !is_gimple_reg (t))
    return false;

  return (is_gimple_variable (t) || is_gimple_min_invariant (t));
}

/* Similarly, but accept hard registers as inputs to asm statements.  */

bool
is_gimple_asm_val (tree t)
{
  if (TREE_CODE (t) == VAR_DECL && DECL_HARD_REGISTER (t))
    return true;

  return is_gimple_val (t);
}

/* Return true if T is a GIMPLE minimal lvalue.  */

bool
is_gimple_min_lval (tree t)
{
  if (!(t = CONST_CAST_TREE (strip_invariant_refs (t))))
    return false;
  return (is_gimple_id (t) || TREE_CODE (t) == MEM_REF);
}

/* Return true if T is a valid function operand of a CALL_EXPR.  */

bool
is_gimple_call_addr (tree t)
{
  return (TREE_CODE (t) == OBJ_TYPE_REF || is_gimple_val (t));
}

/* Return true if T is a valid address operand of a MEM_REF.  */

bool
is_gimple_mem_ref_addr (tree t)
{
  return (is_gimple_reg (t)
	  || TREE_CODE (t) == INTEGER_CST
	  || (TREE_CODE (t) == ADDR_EXPR
	      && (CONSTANT_CLASS_P (TREE_OPERAND (t, 0))
		  || decl_address_invariant_p (TREE_OPERAND (t, 0)))));
}

/* Hold trees marked addressable during expand.  */

static hash_set<tree> *mark_addressable_queue;

/* Mark X as addressable or queue it up if called during expand.  We
   don't want to apply it immediately during expand because decls are
   made addressable at that point due to RTL-only concerns, such as
   uses of memcpy for block moves, and TREE_ADDRESSABLE changes
   is_gimple_reg, which might make it seem like a variable that used
   to be a gimple_reg shouldn't have been an SSA name.  So we queue up
   this flag setting and only apply it when we're done with GIMPLE and
   only RTL issues matter.  */

static void
mark_addressable_1 (tree x)
{
  if (!currently_expanding_to_rtl)
    {
      TREE_ADDRESSABLE (x) = 1;
      return;
    }

  if (!mark_addressable_queue)
    mark_addressable_queue = new hash_set<tree>();
  mark_addressable_queue->add (x);
}

/* Adaptor for mark_addressable_1 for use in hash_set traversal.  */

bool
mark_addressable_2 (tree const &x, void * ATTRIBUTE_UNUSED = NULL)
{
  mark_addressable_1 (x);
  return false;
}

/* Mark all queued trees as addressable, and empty the queue.  To be
   called right after clearing CURRENTLY_EXPANDING_TO_RTL.  */

void
flush_mark_addressable_queue ()
{
  gcc_assert (!currently_expanding_to_rtl);
  if (mark_addressable_queue)
    {
      mark_addressable_queue->traverse<void*, mark_addressable_2> (NULL);
      delete mark_addressable_queue;
      mark_addressable_queue = NULL;
    }
}

/* Mark X addressable.  Unlike the langhook we expect X to be in gimple
   form and we don't do any syntax checking.  */

void
mark_addressable (tree x)
{
  while (handled_component_p (x))
    x = TREE_OPERAND (x, 0);
  if (TREE_CODE (x) == MEM_REF
      && TREE_CODE (TREE_OPERAND (x, 0)) == ADDR_EXPR)
    x = TREE_OPERAND (TREE_OPERAND (x, 0), 0);
  if (!VAR_P (x)
      && TREE_CODE (x) != PARM_DECL
      && TREE_CODE (x) != RESULT_DECL)
    return;
  mark_addressable_1 (x);

  /* Also mark the artificial SSA_NAME that points to the partition of X.  */
  if (TREE_CODE (x) == VAR_DECL
      && !DECL_EXTERNAL (x)
      && !TREE_STATIC (x)
      && cfun->gimple_df != NULL
      && cfun->gimple_df->decls_to_pointers != NULL)
    {
      tree *namep = cfun->gimple_df->decls_to_pointers->get (x);
      if (namep)
	mark_addressable_1 (*namep);
    }
}

/* Returns true iff T is a valid RHS for an assignment to a renamed
   user -- or front-end generated artificial -- variable.  */

bool
is_gimple_reg_rhs (tree t)
{
  return get_gimple_rhs_class (TREE_CODE (t)) != GIMPLE_INVALID_RHS;
}

#include "gt-gimple-expr.h"
