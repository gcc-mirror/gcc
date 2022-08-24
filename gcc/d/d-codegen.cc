/* d-codegen.cc --  Code generation and routines for manipulation of GCC trees.
   Copyright (C) 2006-2022 Free Software Foundation, Inc.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/aggregate.h"
#include "dmd/ctfe.h"
#include "dmd/declaration.h"
#include "dmd/identifier.h"
#include "dmd/module.h"
#include "dmd/target.h"
#include "dmd/template.h"

#include "tree.h"
#include "tree-iterator.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "target.h"
#include "stringpool.h"
#include "varasm.h"
#include "stor-layout.h"
#include "attribs.h"
#include "function.h"
#include "gimple-expr.h"

#include "d-tree.h"


/* Return the GCC location for the D frontend location LOC.  */

location_t
make_location_t (const Loc &loc)
{
  location_t gcc_location = input_location;

  if (loc.filename)
    {
      linemap_add (line_table, LC_ENTER, 0, loc.filename, loc.linnum);
      linemap_line_start (line_table, loc.linnum, 0);
      gcc_location = linemap_position_for_column (line_table, loc.charnum);
      linemap_add (line_table, LC_LEAVE, 0, NULL, 0);
    }

  return gcc_location;
}

/* Return the DECL_CONTEXT for symbol DSYM.  */

tree
d_decl_context (Dsymbol *dsym)
{
  Dsymbol *parent = dsym;
  Declaration *decl = dsym->isDeclaration ();
  AggregateDeclaration *ad = dsym->isAggregateDeclaration ();

  while ((parent = parent->toParent2 ()))
    {
      /* We've reached the top-level module namespace.
	 Set DECL_CONTEXT as the NAMESPACE_DECL of the enclosing module,
	 but only for extern(D) symbols.  */
      if (parent->isModule ())
	{
	  if ((decl != NULL && decl->resolvedLinkage () != LINK::d)
	      || (ad != NULL && ad->classKind != ClassKind::d))
	    return NULL_TREE;

	  return build_import_decl (parent);
	}

      /* Declarations marked as `static' or `__gshared' are never
	 part of any context except at module level.  */
      if (decl != NULL && decl->isDataseg ())
	continue;

      /* Nested functions.  */
      FuncDeclaration *fd = parent->isFuncDeclaration ();
      if (fd != NULL)
	return get_symbol_decl (fd);

      /* Methods of classes or structs.  */
      AggregateDeclaration *ad = parent->isAggregateDeclaration ();
      if (ad != NULL)
	{
	  tree context = build_ctype (ad->type);
	  /* Want the underlying RECORD_TYPE.  */
	  if (ad->isClassDeclaration ())
	    context = TREE_TYPE (context);

	  return context;
	}
    }

  return NULL_TREE;
}

/* Return a copy of record TYPE but safe to modify in any way.  */

tree
copy_aggregate_type (tree type)
{
  tree newtype = build_distinct_type_copy (type);
  TYPE_STUB_DECL (newtype) = TYPE_NAME (newtype);
  TYPE_FIELDS (newtype) = copy_list (TYPE_FIELDS (type));

  for (tree f = TYPE_FIELDS (newtype); f; f = DECL_CHAIN (f))
    DECL_FIELD_CONTEXT (f) = newtype;

  return newtype;
}

/* Return TRUE if declaration DECL is a reference type.  */

bool
declaration_reference_p (Declaration *decl)
{
  Type *tb = decl->type->toBasetype ();

  /* Declaration is a reference type.  */
  if (tb->ty == TY::Treference || decl->storage_class & (STCout | STCref))
    return true;

  return false;
}

/* Returns the real type for declaration DECL.  */

tree
declaration_type (Declaration *decl)
{
  /* Lazy declarations are converted to delegates.  */
  if (decl->storage_class & STClazy)
    {
      TypeFunction *tf = TypeFunction::create (NULL, decl->type,
					       VARARGnone, LINK::d);
      TypeDelegate *t = TypeDelegate::create (tf);
      return build_ctype (t->merge2 ());
    }

  /* Static array va_list have array->pointer conversions applied.  */
  if (decl->isParameter () && valist_array_p (decl->type))
    {
      Type *valist = decl->type->nextOf ()->pointerTo ();
      valist = valist->castMod (decl->type->mod);
      return build_ctype (valist);
    }

  tree type = build_ctype (decl->type);

  /* Parameter is passed by reference.  */
  if (declaration_reference_p (decl))
    return build_reference_type (type);

  /* The `this' parameter is always const.  */
  if (decl->isThisDeclaration ())
    return insert_type_modifiers (type, MODconst);

  return type;
}

/* These should match the Declaration versions above
   Return TRUE if parameter ARG is a reference type.  */

bool
parameter_reference_p (Parameter *arg)
{
  Type *tb = arg->type->toBasetype ();

  /* Parameter is a reference type.  */
  if (tb->ty == TY::Treference || arg->storageClass & (STCout | STCref))
    return true;

  return false;
}

/* Returns the real type for parameter ARG.  */

tree
parameter_type (Parameter *arg)
{
  /* Lazy parameters are converted to delegates.  */
  if (arg->storageClass & STClazy)
    {
      TypeFunction *tf = TypeFunction::create (NULL, arg->type,
					       VARARGnone, LINK::d);
      TypeDelegate *t = TypeDelegate::create (tf);
      return build_ctype (t->merge2 ());
    }

  /* Static array va_list have array->pointer conversions applied.  */
  if (valist_array_p (arg->type))
    {
      Type *valist = arg->type->nextOf ()->pointerTo ();
      valist = valist->castMod (arg->type->mod);
      return build_ctype (valist);
    }

  tree type = build_ctype (arg->type);

  /* Parameter is passed by reference.  */
  if (parameter_reference_p (arg))
    return build_reference_type (type);

  /* Pass non-POD structs by invisible reference.  */
  if (TREE_ADDRESSABLE (type))
    {
      type = build_reference_type (type);
      /* There are no other pointer to this temporary.  */
      type = build_qualified_type (type, TYPE_QUAL_RESTRICT);
    }

  /* Front-end has already taken care of type promotions.  */
  return type;
}

/* Build INTEGER_CST of type TYPE with the value VALUE.  */

tree
build_integer_cst (dinteger_t value, tree type)
{
  /* The type is error_mark_node, we can't do anything.  */
  if (error_operand_p (type))
    return type;

  return build_int_cst_type (type, value);
}

/* Build REAL_CST of type TOTYPE with the value VALUE.  */

tree
build_float_cst (const real_t &value, Type *totype)
{
  real_t new_value;
  TypeBasic *tb = totype->isTypeBasic ();

  gcc_assert (tb != NULL);

  tree type_node = build_ctype (tb);
  real_convert (&new_value.rv (), TYPE_MODE (type_node), &value.rv ());

  return build_real (type_node, new_value.rv ());
}

/* Returns the .length component from the D dynamic array EXP.  */

tree
d_array_length (tree exp)
{
  if (error_operand_p (exp))
    return exp;

  gcc_assert (TYPE_DYNAMIC_ARRAY (TREE_TYPE (exp)));

  /* Get the back-end type for the array and pick out the array
     length field (assumed to be the first field).  */
  tree len_field = TYPE_FIELDS (TREE_TYPE (exp));
  return component_ref (exp, len_field);
}

/* Returns the .ptr component from the D dynamic array EXP.  */

tree
d_array_ptr (tree exp)
{
  if (error_operand_p (exp))
    return exp;

  gcc_assert (TYPE_DYNAMIC_ARRAY (TREE_TYPE (exp)));

  /* Get the back-end type for the array and pick out the array
     data pointer field (assumed to be the second field).  */
  tree ptr_field = TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (exp)));
  return component_ref (exp, ptr_field);
}

/* Returns a constructor for D dynamic array type TYPE of .length LEN
   and .ptr pointing to DATA.  */

tree
d_array_value (tree type, tree len, tree data)
{
  tree len_field, ptr_field;
  vec <constructor_elt, va_gc> *ce = NULL;

  gcc_assert (TYPE_DYNAMIC_ARRAY (type));
  len_field = TYPE_FIELDS (type);
  ptr_field = TREE_CHAIN (len_field);

  len = convert (TREE_TYPE (len_field), len);
  data = convert (TREE_TYPE (ptr_field), data);

  CONSTRUCTOR_APPEND_ELT (ce, len_field, len);
  CONSTRUCTOR_APPEND_ELT (ce, ptr_field, data);

  return build_constructor (type, ce);
}

/* Returns value representing the array length of expression EXP.
   TYPE could be a dynamic or static array.  */

tree
get_array_length (tree exp, Type *type)
{
  Type *tb = type->toBasetype ();

  switch (tb->ty)
    {
    case TY::Tsarray:
      return size_int (tb->isTypeSArray ()->dim->toUInteger ());

    case TY::Tarray:
      return d_array_length (exp);

    default:
      error ("cannot determine the length of a %qs", type->toChars ());
      return error_mark_node;
    }
}

/* Create BINFO for a ClassDeclaration's inheritance tree.
   InterfaceDeclaration's are not included.  */

tree
build_class_binfo (tree super, ClassDeclaration *cd)
{
  tree binfo = make_tree_binfo (1);
  tree ctype = build_ctype (cd->type);

  /* Want RECORD_TYPE, not POINTER_TYPE.  */
  BINFO_TYPE (binfo) = TREE_TYPE (ctype);
  BINFO_INHERITANCE_CHAIN (binfo) = super;
  BINFO_OFFSET (binfo) = integer_zero_node;

  if (cd->baseClass)
    BINFO_BASE_APPEND (binfo, build_class_binfo (binfo, cd->baseClass));

  return binfo;
}

/* Create BINFO for an InterfaceDeclaration's inheritance tree.
   In order to access all inherited methods in the debugger,
   the entire tree must be described.
   This function makes assumptions about interface layout.  */

tree
build_interface_binfo (tree super, ClassDeclaration *cd, unsigned &offset)
{
  tree binfo = make_tree_binfo (cd->baseclasses->length);
  tree ctype = build_ctype (cd->type);

  /* Want RECORD_TYPE, not POINTER_TYPE.  */
  BINFO_TYPE (binfo) = TREE_TYPE (ctype);
  BINFO_INHERITANCE_CHAIN (binfo) = super;
  BINFO_OFFSET (binfo) = size_int (offset * target.ptrsize);
  BINFO_VIRTUAL_P (binfo) = 1;

  for (size_t i = 0; i < cd->baseclasses->length; i++, offset++)
    {
      BaseClass *bc = (*cd->baseclasses)[i];
      BINFO_BASE_APPEND (binfo, build_interface_binfo (binfo, bc->sym, offset));
    }

  return binfo;
}

/* Returns the .funcptr component from the D delegate EXP.  */

tree
delegate_method (tree exp)
{
  /* Get the back-end type for the delegate and pick out the funcptr field
     (assumed to be the second field).  */
  gcc_assert (TYPE_DELEGATE (TREE_TYPE (exp)));
  tree method_field = TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (exp)));
  return component_ref (exp, method_field);
}

/* Returns the .object component from the delegate EXP.  */

tree
delegate_object (tree exp)
{
  /* Get the back-end type for the delegate and pick out the object field
     (assumed to be the first field).  */
  gcc_assert (TYPE_DELEGATE (TREE_TYPE (exp)));
  tree obj_field = TYPE_FIELDS (TREE_TYPE (exp));
  return component_ref (exp, obj_field);
}

/* Build a delegate literal of type TYPE whose pointer function is
   METHOD, and hidden object is OBJECT.  */

tree
build_delegate_cst (tree method, tree object, Type *type)
{
  tree ctor = make_node (CONSTRUCTOR);
  tree ctype;

  Type *tb = type->toBasetype ();
  if (tb->ty == TY::Tdelegate)
    ctype = build_ctype (type);
  else
    {
      /* Convert a function method into an anonymous delegate.  */
      ctype = make_struct_type ("delegate()", 2,
				get_identifier ("ptr"), TREE_TYPE (object),
				get_identifier ("funcptr"), TREE_TYPE (method));
      TYPE_DELEGATE (ctype) = 1;
    }

  vec <constructor_elt, va_gc> *ce = NULL;
  CONSTRUCTOR_APPEND_ELT (ce, TYPE_FIELDS (ctype), object);
  CONSTRUCTOR_APPEND_ELT (ce, TREE_CHAIN (TYPE_FIELDS (ctype)), method);

  CONSTRUCTOR_ELTS (ctor) = ce;
  TREE_TYPE (ctor) = ctype;

  return ctor;
}

/* Builds a temporary tree to store the CALLEE and OBJECT
   of a method call expression of type TYPE.  */

tree
build_method_call (tree callee, tree object, Type *type)
{
  tree t = build_delegate_cst (callee, object, type);
  METHOD_CALL_EXPR (t) = 1;
  return t;
}

/* Extract callee and object from T and return in to CALLEE and OBJECT.  */

void
extract_from_method_call (tree t, tree &callee, tree &object)
{
  gcc_assert (METHOD_CALL_EXPR (t));
  object = CONSTRUCTOR_ELT (t, 0)->value;
  callee = CONSTRUCTOR_ELT (t, 1)->value;
}

/* Build a typeof(null) constant of type TYPE.  Handles certain special case
   conversions, where the underlying type is an aggregate with a nullable
   interior pointer.  */

tree
build_typeof_null_value (Type *type)
{
  Type *tb = type->toBasetype ();
  tree value;

  /* For dynamic arrays, set length and pointer fields to zero.  */
  if (tb->ty == TY::Tarray)
    value = d_array_value (build_ctype (type), size_int (0), null_pointer_node);

  /* For associative arrays, set the pointer field to null.  */
  else if (tb->ty == TY::Taarray)
    {
      tree ctype = build_ctype (type);
      gcc_assert (TYPE_ASSOCIATIVE_ARRAY (ctype));

      value = build_constructor_single (ctype, TYPE_FIELDS (ctype),
					null_pointer_node);
    }

  /* For delegates, set the frame and function pointer fields to null.  */
  else if (tb->ty == TY::Tdelegate)
    value = build_delegate_cst (null_pointer_node, null_pointer_node, type);

  /* Simple zero constant for all other types.  */
  else
    value = build_zero_cst (build_ctype (type));

  TREE_CONSTANT (value) = 1;
  return value;
}

/* Build a dereference into the virtual table for OBJECT to retrieve
   a function pointer of type FNTYPE at position INDEX.  */

tree
build_vindex_ref (tree object, tree fntype, size_t index)
{
  /* The vtable is the first field.  Interface methods are also in the class's
     vtable, so we don't need to convert from a class to an interface.  */
  tree result = build_deref (object);
  result = component_ref (result, TYPE_FIELDS (TREE_TYPE (result)));

  gcc_assert (POINTER_TYPE_P (fntype));

  return build_memref (fntype, result, size_int (target.ptrsize * index));
}

/* Return TRUE if EXP is a valid lvalue.  Lvalue references cannot be
   made into temporaries, otherwise any assignments will be lost.  */

static bool
lvalue_p (tree exp)
{
  const enum tree_code code = TREE_CODE (exp);

  switch (code)
    {
    case SAVE_EXPR:
      return false;

    case ARRAY_REF:
    case INDIRECT_REF:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      return !FUNC_OR_METHOD_TYPE_P (TREE_TYPE (exp));

    case IMAGPART_EXPR:
    case REALPART_EXPR:
    case COMPONENT_REF:
    CASE_CONVERT:
      return lvalue_p (TREE_OPERAND (exp, 0));

    case COND_EXPR:
      return (lvalue_p (TREE_OPERAND (exp, 1)
			? TREE_OPERAND (exp, 1)
			: TREE_OPERAND (exp, 0))
	      && lvalue_p (TREE_OPERAND (exp, 2)));

    case TARGET_EXPR:
      return true;

    case COMPOUND_EXPR:
      return lvalue_p (TREE_OPERAND (exp, 1));

    default:
      return false;
    }
}

/* Create a SAVE_EXPR if EXP might have unwanted side effects if referenced
   more than once in an expression.  */

tree
d_save_expr (tree exp)
{
  if (TREE_SIDE_EFFECTS (exp))
    {
      if (lvalue_p (exp))
	return stabilize_reference (exp);

      return save_expr (exp);
    }

  return exp;
}

/* VALUEP is an expression we want to pre-evaluate or perform a computation on.
   The expression returned by this function is the part whose value we don't
   care about, storing the value in VALUEP.  Callers must ensure that the
   returned expression is evaluated before VALUEP.  */

tree
stabilize_expr (tree *valuep)
{
  tree expr = *valuep;
  const enum tree_code code = TREE_CODE (expr);
  tree lhs;
  tree rhs;

  switch (code)
    {
    case COMPOUND_EXPR:
      /* Given ((e1, ...), eN):
	 Store the last RHS 'eN' expression in VALUEP.  */
      lhs = TREE_OPERAND (expr, 0);
      rhs = TREE_OPERAND (expr, 1);
      lhs = compound_expr (lhs, stabilize_expr (&rhs));
      *valuep = rhs;
      return lhs;

    default:
      return NULL_TREE;
    }
}

/* Return a TARGET_EXPR, initializing the DECL with EXP.  */

tree
build_target_expr (tree decl, tree exp)
{
  tree type = TREE_TYPE (decl);
  tree result = build4 (TARGET_EXPR, type, decl, exp, NULL_TREE, NULL_TREE);

  if (EXPR_HAS_LOCATION (exp))
    SET_EXPR_LOCATION (result, EXPR_LOCATION (exp));

  /* If decl must always reside in memory.  */
  if (TREE_ADDRESSABLE (type))
    d_mark_addressable (decl);

  /* Always set TREE_SIDE_EFFECTS so that expand_expr does not ignore the
     TARGET_EXPR.  If there really turn out to be no side effects, then the
     optimizer should be able to remove it.  */
  TREE_SIDE_EFFECTS (result) = 1;

  return result;
}

/* Like the above function, but initializes a new temporary.  */

tree
force_target_expr (tree exp)
{
  tree decl = create_tmp_var_raw (TREE_TYPE (exp));
  DECL_CONTEXT (decl) = current_function_decl;
  layout_decl (decl, 0);

  return build_target_expr (decl, exp);
}

/* Returns the address of the expression EXP.  */

tree
build_address (tree exp)
{
  if (error_operand_p (exp))
    return exp;

  tree ptrtype;
  tree type = TREE_TYPE (exp);

  if (TREE_CODE (exp) == STRING_CST)
    {
      /* Just convert string literals (char[]) to C-style strings (char *),
	 otherwise the latter method (char[]*) causes conversion problems
	 during gimplification.  */
      ptrtype = build_pointer_type (TREE_TYPE (type));
    }
  else if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (va_list_type_node)
	   && TREE_CODE (TYPE_MAIN_VARIANT (type)) == ARRAY_TYPE)
    {
      /* Special case for va_list, allow arrays to decay to a pointer.  */
      ptrtype = build_pointer_type (TREE_TYPE (type));
    }
  else
    ptrtype = build_pointer_type (type);

  /* Maybe rewrite: &(e1, e2) => (e1, &e2).  */
  tree init = stabilize_expr (&exp);

  /* Can't take the address of a manifest constant, instead use its value.  */
  if (TREE_CODE (exp) == CONST_DECL)
    exp = DECL_INITIAL (exp);

  /* Some expression lowering may request an address of a compile-time constant,
     or other non-lvalue expression.  Make sure it is assigned to a location we
     can reference.  */
  if (CONSTANT_CLASS_P (exp) && TREE_CODE (exp) != STRING_CST)
    exp = force_target_expr (exp);
  else if (TREE_CODE (exp) == CALL_EXPR)
    {
      /* When a struct or array is returned in registers, we need to again fill
	 in all alignment holes.  */
      if (AGGREGATE_TYPE_P (TREE_TYPE (exp))
	  && !aggregate_value_p (TREE_TYPE (exp), exp))
	{
	  tree tmp = build_local_temp (TREE_TYPE (exp));
	  init = compound_expr (init, build_memset_call (tmp));
	  init = compound_expr (init, modify_expr (tmp, exp));
	  exp = tmp;
	}
      else
	exp = force_target_expr (exp);
    }

  d_mark_addressable (exp);
  exp = build_fold_addr_expr_with_type_loc (input_location, exp, ptrtype);

  if (TREE_CODE (exp) == ADDR_EXPR)
    TREE_NO_TRAMPOLINE (exp) = 1;

  return compound_expr (init, exp);
}

/* Mark EXP saying that we need to be able to take the address of it; it should
   not be allocated in a register.  When COMPLAIN is true, issue an error if we
   are marking a register variable.  */

tree
d_mark_addressable (tree exp, bool complain)
{
  switch (TREE_CODE (exp))
    {
    case ADDR_EXPR:
    case COMPONENT_REF:
    case ARRAY_REF:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      d_mark_addressable (TREE_OPERAND (exp, 0));
      break;

    case VAR_DECL:
      if (complain && DECL_REGISTER (exp))
	{
	  if (DECL_HARD_REGISTER (exp) || DECL_EXTERNAL (exp))
	    error ("address of explicit register variable %qD requested", exp);
	  else
	    error ("address of register variable %qD requested", exp);
	}

      /* Fall through.  */
    case PARM_DECL:
    case RESULT_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
      if (!VAR_P (exp) || !DECL_HARD_REGISTER (exp))
	TREE_ADDRESSABLE (exp) = 1;
      break;

    case CONSTRUCTOR:
      TREE_ADDRESSABLE (exp) = 1;
      break;

    case TARGET_EXPR:
      TREE_ADDRESSABLE (exp) = 1;
      d_mark_addressable (TREE_OPERAND (exp, 0));
      break;

    default:
      break;
    }

  return exp;
}

/* Mark EXP as "used" in the program for the benefit of
   -Wunused warning purposes.  */

tree
d_mark_used (tree exp)
{
  switch (TREE_CODE (exp))
    {
    case VAR_DECL:
    case CONST_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case FUNCTION_DECL:
      TREE_USED (exp) = 1;
      break;

    case ARRAY_REF:
    case COMPONENT_REF:
    case MODIFY_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case NOP_EXPR:
    case CONVERT_EXPR:
    case ADDR_EXPR:
      d_mark_used (TREE_OPERAND (exp, 0));
      break;

    case COMPOUND_EXPR:
      d_mark_used (TREE_OPERAND (exp, 0));
      d_mark_used (TREE_OPERAND (exp, 1));
      break;

    default:
      break;
    }
  return exp;
}

/* Mark EXP as read, not just set, for set but not used -Wunused
   warning purposes.  */

tree
d_mark_read (tree exp)
{
  switch (TREE_CODE (exp))
    {
    case VAR_DECL:
    case PARM_DECL:
      TREE_USED (exp) = 1;
      DECL_READ_P (exp) = 1;
      break;

    case ARRAY_REF:
    case COMPONENT_REF:
    case MODIFY_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case NOP_EXPR:
    case CONVERT_EXPR:
    case ADDR_EXPR:
      d_mark_read (TREE_OPERAND (exp, 0));
      break;

    case COMPOUND_EXPR:
      d_mark_read (TREE_OPERAND (exp, 1));
      break;

    default:
      break;
    }
  return exp;
}

/* Build a call to memcmp(), compares the first NUM bytes of PTR1 with PTR2.  */

tree
build_memcmp_call (tree ptr1, tree ptr2, tree num)
{
  return build_call_expr (builtin_decl_explicit (BUILT_IN_MEMCMP), 3,
			  ptr1, ptr2, num);
}

/* Build a call to memcpy(), copies the first NUM bytes of SRC into DST.  */

tree
build_memcpy_call (tree dst, tree src, tree num)
{
  return build_call_expr (builtin_decl_explicit (BUILT_IN_MEMCPY), 3,
			  dst, src, num);
}

/* Build a call to memset(), fills the first NUM bytes of PTR with zeros.
   If NUM is NULL, then we expect PTR to be object that requires filling.  */

tree
build_memset_call (tree ptr, tree num)
{
  if (num == NULL_TREE)
    {
      gcc_assert (TREE_CODE (ptr) != ADDR_EXPR);
      num = TYPE_SIZE_UNIT (TREE_TYPE (ptr));
      ptr = build_address (ptr);
    }

  /* Use a zero constant to fill the destination if setting the entire object.
     For CONSTRUCTORs, the memcpy() is lowered to a ref-all pointer assignment,
     which can then be merged with other stores to the object.  */
  tree valtype = TREE_TYPE (TREE_TYPE (ptr));
  if (tree_int_cst_equal (TYPE_SIZE_UNIT (valtype), num))
    {
      tree cst = build_zero_cst (valtype);
      if (TREE_CODE (cst) == CONSTRUCTOR)
	return build_memcpy_call (ptr, build_address (cst), num);

      return modify_expr (build_deref (ptr), cst);
    }

  return build_call_expr (builtin_decl_explicit (BUILT_IN_MEMSET), 3,
			  ptr, integer_zero_node, num);
}

/* Return TRUE if the struct SD is suitable for comparison using memcmp.
   This is because we don't guarantee that padding is zero-initialized for
   a stack variable, so we can't use memcmp to compare struct values.  */

bool
identity_compare_p (StructDeclaration *sd)
{
  if (sd->isUnionDeclaration ())
    return true;

  unsigned offset = 0;

  for (size_t i = 0; i < sd->fields.length; i++)
    {
      VarDeclaration *vd = sd->fields[i];
      Type *tb = vd->type->toBasetype ();

      /* Check inner data structures.  */
      if (TypeStruct *ts = tb->isTypeStruct ())
	{
	  if (!identity_compare_p (ts->sym))
	    return false;
	}

      /* Check for types that may have padding.  */
      if ((tb->ty == TY::Tcomplex80
	   || tb->ty == TY::Tfloat80
	   || tb->ty == TY::Timaginary80)
	  && target.realpad != 0)
	return false;

      if (offset <= vd->offset)
	{
	  /* There's a hole in the struct.  */
	  if (offset != vd->offset)
	    return false;

	  offset += vd->type->size ();
	}
    }

  /* Any trailing padding may not be zero.  */
  if (offset < sd->structsize)
    return false;

  return true;
}

/* Build a floating-point identity comparison between T1 and T2, ignoring any
   excessive padding in the type.  CODE is EQ_EXPR or NE_EXPR comparison.  */

tree
build_float_identity (tree_code code, tree t1, tree t2)
{
  tree size = size_int (TYPE_PRECISION (TREE_TYPE (t1)) / BITS_PER_UNIT);
  tree result = build_memcmp_call (build_address (t1),
				   build_address (t2), size);
  return build_boolop (code, result, integer_zero_node);
}

/* Lower a field-by-field equality expression between T1 and T2 of type SD.
   CODE is the EQ_EXPR or NE_EXPR comparison.  */

static tree
lower_struct_comparison (tree_code code, StructDeclaration *sd,
			 tree t1, tree t2)
{
  tree_code tcode = (code == EQ_EXPR) ? TRUTH_ANDIF_EXPR : TRUTH_ORIF_EXPR;
  tree tmemcmp = NULL_TREE;

  /* We can skip the compare if the structs are empty.  */
  if (sd->fields.length == 0)
    {
      tmemcmp = build_boolop (code, integer_zero_node, integer_zero_node);
      if (TREE_SIDE_EFFECTS (t2))
	tmemcmp = compound_expr (t2, tmemcmp);
      if (TREE_SIDE_EFFECTS (t1))
	tmemcmp = compound_expr (t1, tmemcmp);

      return tmemcmp;
    }

  /* Let back-end take care of union comparisons.  */
  if (sd->isUnionDeclaration ())
    {
      tmemcmp = build_memcmp_call (build_address (t1), build_address (t2),
				   size_int (sd->structsize));
      return build_boolop (code, tmemcmp, integer_zero_node);
    }

  for (size_t i = 0; i < sd->fields.length; i++)
    {
      VarDeclaration *vd = sd->fields[i];
      Type *type = vd->type->toBasetype ();
      tree sfield = get_symbol_decl (vd);

      tree t1ref = component_ref (t1, sfield);
      tree t2ref = component_ref (t2, sfield);
      tree tcmp;

      if (TypeStruct *ts = type->isTypeStruct ())
	{
	  /* Compare inner data structures.  */
	  tcmp = lower_struct_comparison (code, ts->sym, t1ref, t2ref);
	}
      else if (type->ty != TY::Tvector && type->isintegral ())
	{
	  /* Integer comparison, no special handling required.  */
	  tcmp = build_boolop (code, t1ref, t2ref);
	}
      else if (type->ty != TY::Tvector && type->isfloating ())
	{
	  /* Floating-point comparison, don't compare padding in type.  */
	  if (!type->iscomplex ())
	    tcmp = build_float_identity (code, t1ref, t2ref);
	  else
	    {
	      tree req = build_float_identity (code, real_part (t1ref),
					       real_part (t2ref));
	      tree ieq = build_float_identity (code, imaginary_part (t1ref),
					       imaginary_part (t2ref));

	      tcmp = build_boolop (tcode, req, ieq);
	    }
	}
      else
	{
	  tree stype = build_ctype (type);
	  opt_scalar_int_mode mode = int_mode_for_mode (TYPE_MODE (stype));

	  if (mode.exists ())
	    {
	      /* Compare field bits as their corresponding integer type.
		    *((T*) &t1) == *((T*) &t2)  */
	      tree tmode = lang_hooks.types.type_for_mode (mode.require (), 1);

	      if (tmode == NULL_TREE)
		tmode = make_unsigned_type (GET_MODE_BITSIZE (mode.require ()));

	      t1ref = build_vconvert (tmode, t1ref);
	      t2ref = build_vconvert (tmode, t2ref);

	      tcmp = build_boolop (code, t1ref, t2ref);
	    }
	  else
	    {
	      /* Simple memcmp between types.  */
	      tcmp = build_memcmp_call (build_address (t1ref),
					build_address (t2ref),
					TYPE_SIZE_UNIT (stype));
	      tcmp = build_boolop (code, tcmp, integer_zero_node);
	    }
	}

      tmemcmp = (tmemcmp) ? build_boolop (tcode, tmemcmp, tcmp) : tcmp;
    }

  return tmemcmp;
}


/* Build an equality expression between two RECORD_TYPES T1 and T2 of type SD.
   If possible, use memcmp, otherwise field-by-field comparison is done.
   CODE is the EQ_EXPR or NE_EXPR comparison.  */

tree
build_struct_comparison (tree_code code, StructDeclaration *sd,
			 tree t1, tree t2)
{
  /* We can skip the compare if the structs are empty.  */
  if (sd->fields.length == 0)
    {
      tree exp = build_boolop (code, integer_zero_node, integer_zero_node);
      if (TREE_SIDE_EFFECTS (t2))
	exp = compound_expr (t2, exp);
      if (TREE_SIDE_EFFECTS (t1))
	exp = compound_expr (t1, exp);

      return exp;
    }

  /* Make temporaries to prevent multiple evaluations.  */
  tree t1init = stabilize_expr (&t1);
  tree t2init = stabilize_expr (&t2);
  tree result;

  t1 = d_save_expr (t1);
  t2 = d_save_expr (t2);

  /* Bitwise comparison of structs not returned in memory may not work
     due to data holes loosing its zero padding upon return.
     As a heuristic, small structs are not compared using memcmp either.  */
  if (TYPE_MODE (TREE_TYPE (t1)) != BLKmode || !identity_compare_p (sd))
    result = lower_struct_comparison (code, sd, t1, t2);
  else
    {
      /* Do bit compare of structs.  */
      tree tmemcmp = build_memcmp_call (build_address (t1), build_address (t2),
					size_int (sd->structsize));
      result = build_boolop (code, tmemcmp, integer_zero_node);
    }

  return compound_expr (compound_expr (t1init, t2init), result);
}

/* Build an equality expression between two ARRAY_TYPES of size LENGTH.
   The pointer references are T1 and T2, and the element type is SD.
   CODE is the EQ_EXPR or NE_EXPR comparison.  */

tree
build_array_struct_comparison (tree_code code, StructDeclaration *sd,
			       tree length, tree t1, tree t2)
{
  tree_code tcode = (code == EQ_EXPR) ? TRUTH_ANDIF_EXPR : TRUTH_ORIF_EXPR;

  /* Build temporary for the result of the comparison.
     Initialize as either 0 or 1 depending on operation.  */
  tree result = build_local_temp (d_bool_type);
  tree init = build_boolop (code, integer_zero_node, integer_zero_node);
  add_stmt (build_assign (INIT_EXPR, result, init));

  /* Cast pointer-to-array to pointer-to-struct.  */
  tree ptrtype = build_ctype (sd->type->pointerTo ());
  tree lentype = TREE_TYPE (length);

  push_binding_level (level_block);
  push_stmt_list ();

  /* Build temporary locals for length and pointers.  */
  tree t = build_local_temp (size_type_node);
  add_stmt (build_assign (INIT_EXPR, t, length));
  length = t;

  t = build_local_temp (ptrtype);
  add_stmt (build_assign (INIT_EXPR, t, d_convert (ptrtype, t1)));
  t1 = t;

  t = build_local_temp (ptrtype);
  add_stmt (build_assign (INIT_EXPR, t, d_convert (ptrtype, t2)));
  t2 = t;

  /* Build loop for comparing each element.  */
  push_stmt_list ();

  /* Exit logic for the loop.
	if (length == 0 || result OP 0) break;  */
  t = build_boolop (EQ_EXPR, length, d_convert (lentype, integer_zero_node));
  t = build_boolop (TRUTH_ORIF_EXPR, t, build_boolop (code, result,
						      boolean_false_node));
  t = build1 (EXIT_EXPR, void_type_node, t);
  add_stmt (t);

  /* Do comparison, caching the value.
	result = result OP (*t1 == *t2);  */
  t = build_struct_comparison (code, sd, build_deref (t1), build_deref (t2));
  t = build_boolop (tcode, result, t);
  t = modify_expr (result, t);
  add_stmt (t);

  /* Move both pointers to next element position.
	t1++, t2++;  */
  tree size = d_convert (ptrtype, TYPE_SIZE_UNIT (TREE_TYPE (ptrtype)));
  t = build2 (POSTINCREMENT_EXPR, ptrtype, t1, size);
  add_stmt (t);
  t = build2 (POSTINCREMENT_EXPR, ptrtype, t2, size);
  add_stmt (t);

  /* Decrease loop counter.
	length -= 1;  */
  t = build2 (POSTDECREMENT_EXPR, lentype, length,
	     d_convert (lentype, integer_one_node));
  add_stmt (t);

  /* Pop statements and finish loop.  */
  tree body = pop_stmt_list ();
  add_stmt (build1 (LOOP_EXPR, void_type_node, body));

  /* Wrap it up into a bind expression.  */
  tree stmt_list = pop_stmt_list ();
  tree block = pop_binding_level ();

  body = build3 (BIND_EXPR, void_type_node,
		 BLOCK_VARS (block), stmt_list, block);

  return compound_expr (body, result);
}

/* Build a constructor for a variable of aggregate type TYPE using the
   initializer INIT, an ordered flat list of fields and values provided
   by the frontend.  The returned constructor should be a value that
   matches the layout of TYPE.  */

tree
build_struct_literal (tree type, vec <constructor_elt, va_gc> *init)
{
  /* If the initializer was empty, use default zero initialization.  */
  if (vec_safe_is_empty (init))
    return build_constructor (type, NULL);

  /* Struct literals can be seen for special enums representing `_Complex',
     make sure to reinterpret the literal as the correct type.  */
  if (COMPLEX_FLOAT_TYPE_P (type))
    {
      gcc_assert (vec_safe_length (init) == 2);
      return complex_expr (type, (*init)[0].value, (*init)[1].value);
    }

  vec <constructor_elt, va_gc> *ve = NULL;
  HOST_WIDE_INT bitoffset = 0;
  bool constant_p = true;
  bool finished = false;

  /* Walk through each field, matching our initializer list.  */
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      bool is_initialized = false;
      tree value;

      if (DECL_NAME (field) == NULL_TREE
	  && RECORD_OR_UNION_TYPE_P (TREE_TYPE (field))
	  && ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	{
	  /* Search all nesting aggregates, if nothing is found, then
	     this will return an empty initializer to fill the hole.  */
	  value = build_struct_literal (TREE_TYPE (field), init);

	  if (!initializer_zerop (value))
	    is_initialized = true;
	}
      else
	{
	  /* Search for the value to initialize the next field.  Once found,
	     pop it from the init list so we don't look at it again.  */
	  unsigned HOST_WIDE_INT idx;
	  tree index;

	  FOR_EACH_CONSTRUCTOR_ELT (init, idx, index, value)
	    {
	      /* If the index is NULL, then just assign it to the next field.
		 This comes from layout_typeinfo(), which generates a flat
		 list of values that we must shape into the record type.  */
	      if (index == field || index == NULL_TREE)
		{
		  init->ordered_remove (idx);
		  if (!finished)
		    is_initialized = true;
		  break;
		}
	    }
	}

      if (is_initialized)
	{
	  HOST_WIDE_INT fieldpos = int_bit_position (field);
	  gcc_assert (value != NULL_TREE);

	  /* Must not initialize fields that overlap.  */
	  if (fieldpos < bitoffset)
	    {
	      /* Find the nearest user defined type and field.  */
	      tree vtype = type;
	      while (ANON_AGGR_TYPE_P (vtype))
		vtype = TYPE_CONTEXT (vtype);

	      tree vfield = field;
	      if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (vfield))
		  && ANON_AGGR_TYPE_P (TREE_TYPE (vfield)))
		vfield = TYPE_FIELDS (TREE_TYPE (vfield));

	      /* Must not generate errors for compiler generated fields.  */
	      gcc_assert (TYPE_NAME (vtype) && DECL_NAME (vfield));
	      error ("overlapping initializer for field %qT.%qD",
		     TYPE_NAME (vtype), DECL_NAME (vfield));
	    }

	  if (!TREE_CONSTANT (value))
	    constant_p = false;

	  CONSTRUCTOR_APPEND_ELT (ve, field, value);

	  /* For unions, only the first field is initialized, any other field
	     initializers found for this union are drained and ignored.  */
	  if (TREE_CODE (type) == UNION_TYPE)
	    finished = true;
	}

      /* Move bit offset to the next position in the struct.  */
      if (TREE_CODE (type) == RECORD_TYPE && DECL_SIZE (field))
	bitoffset = int_bit_position (field) + tree_to_shwi (DECL_SIZE (field));

      /* If all initializers have been assigned, there's nothing else to do.  */
      if (vec_safe_is_empty (init))
	break;
    }

  /* Ensure that we have consumed all values.  */
  gcc_assert (vec_safe_is_empty (init) || ANON_AGGR_TYPE_P (type));

  tree ctor = build_constructor (type, ve);

  if (constant_p)
    TREE_CONSTANT (ctor) = 1;

  return ctor;
}

/* Given the TYPE of an anonymous field inside T, return the
   FIELD_DECL for the field.  If not found return NULL_TREE.
   Because anonymous types can nest, we must also search all
   anonymous fields that are directly reachable.  */

static tree
lookup_anon_field (tree t, tree type)
{
  t = TYPE_MAIN_VARIANT (t);

  for (tree field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
    {
      if (DECL_NAME (field) == NULL_TREE)
	{
	  /* If we find it directly, return the field.  */
	  if (type == TYPE_MAIN_VARIANT (TREE_TYPE (field)))
	    return field;

	  /* Otherwise, it could be nested, search harder.  */
	  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (field))
	      && ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	    {
	      tree subfield = lookup_anon_field (TREE_TYPE (field), type);
	      if (subfield)
		return subfield;
	    }
	}
    }

  return NULL_TREE;
}

/* Builds OBJECT.FIELD component reference.  */

tree
component_ref (tree object, tree field)
{
  if (error_operand_p (object) || error_operand_p (field))
    return error_mark_node;

  gcc_assert (TREE_CODE (field) == FIELD_DECL);

  /* Maybe rewrite: (e1, e2).field => (e1, e2.field)  */
  tree init = stabilize_expr (&object);

  /* If the FIELD is from an anonymous aggregate, generate a reference
     to the anonymous data member, and recur to find FIELD.  */
  if (ANON_AGGR_TYPE_P (DECL_CONTEXT (field)))
    {
      tree anonymous_field = lookup_anon_field (TREE_TYPE (object),
						DECL_CONTEXT (field));
      object = component_ref (object, anonymous_field);
    }

  tree result = fold_build3_loc (input_location, COMPONENT_REF,
				 TREE_TYPE (field), object, field, NULL_TREE);

  return compound_expr (init, result);
}

/* Build an assignment expression of lvalue LHS from value RHS.
   CODE is the code for a binary operator that we use to combine
   the old value of LHS with RHS to get the new value.  */

tree
build_assign (tree_code code, tree lhs, tree rhs)
{
  tree result;
  tree init = stabilize_expr (&lhs);
  init = compound_expr (init, stabilize_expr (&rhs));

  /* If initializing the LHS using a function that returns via NRVO.  */
  if (code == INIT_EXPR && TREE_CODE (rhs) == CALL_EXPR
      && AGGREGATE_TYPE_P (TREE_TYPE (rhs))
      && aggregate_value_p (TREE_TYPE (rhs), rhs))
    {
      /* Mark as addressable here, which should ensure the return slot is the
	 address of the LHS expression, taken care of by back-end.  */
      d_mark_addressable (lhs);
      CALL_EXPR_RETURN_SLOT_OPT (rhs) = true;
    }
  /* If modifying an LHS whose type is marked TREE_ADDRESSABLE.  */
  else if (code == MODIFY_EXPR && TREE_ADDRESSABLE (TREE_TYPE (lhs))
	   && TREE_SIDE_EFFECTS (rhs) && TREE_CODE (rhs) != TARGET_EXPR)
    {
      /* LHS may be referenced by the RHS expression, so force a temporary.  */
      rhs = force_target_expr (rhs);
    }

  /* The LHS assignment replaces the temporary in TARGET_EXPR_SLOT.  */
  if (TREE_CODE (rhs) == TARGET_EXPR)
    {
      /* If CODE is not INIT_EXPR, can't initialize LHS directly,
	 since that would cause the LHS to be constructed twice.  */
      if (code != INIT_EXPR)
	{
	  init = compound_expr (init, rhs);
	  result = build_assign (code, lhs, TARGET_EXPR_SLOT (rhs));
	}
      else
	{
	  d_mark_addressable (lhs);
	  TARGET_EXPR_INITIAL (rhs) = build_assign (code, lhs,
						    TARGET_EXPR_INITIAL (rhs));
	  result = rhs;
	}
    }
  else
    {
      /* Simple assignment.  */
      result = fold_build2_loc (input_location, code,
				TREE_TYPE (lhs), lhs, rhs);
    }

  return compound_expr (init, result);
}

/* Build an assignment expression of lvalue LHS from value RHS.  */

tree
modify_expr (tree lhs, tree rhs)
{
  return build_assign (MODIFY_EXPR, lhs, rhs);
}

/* Return EXP represented as TYPE.  */

tree
build_nop (tree type, tree exp)
{
  if (error_operand_p (exp))
    return exp;

  /* Maybe rewrite: cast(TYPE)(e1, e2) => (e1, cast(TYPE) e2)  */
  tree init = stabilize_expr (&exp);
  exp = fold_build1_loc (input_location, NOP_EXPR, type, exp);

  return compound_expr (init, exp);
}

/* Return EXP to be viewed as being another type TYPE.  Same as build_nop,
   except that EXP is type-punned, rather than a straight-forward cast.  */

tree
build_vconvert (tree type, tree exp)
{
  /* Building *(cast(TYPE *)&e1) directly rather then using VIEW_CONVERT_EXPR
     makes sure this works for vector-to-array viewing, or if EXP ends up being
     used as the LHS of a MODIFY_EXPR.  */
  return indirect_ref (type, build_address (exp));
}

/* Maybe warn about ARG being an address that can never be null.  */

static void
warn_for_null_address (tree arg)
{
  if (TREE_CODE (arg) == ADDR_EXPR
      && decl_with_nonnull_addr_p (TREE_OPERAND (arg, 0)))
    warning (OPT_Waddress,
	     "the address of %qD will never be %<null%>",
	     TREE_OPERAND (arg, 0));
}

/* Build a boolean ARG0 op ARG1 expression.  */

tree
build_boolop (tree_code code, tree arg0, tree arg1)
{
  /* Aggregate comparisons may get lowered to a call to builtin memcmp,
     so need to remove all side effects incase its address is taken.  */
  if (AGGREGATE_TYPE_P (TREE_TYPE (arg0)))
    arg0 = d_save_expr (arg0);
  if (AGGREGATE_TYPE_P (TREE_TYPE (arg1)))
    arg1 = d_save_expr (arg1);

  if (VECTOR_TYPE_P (TREE_TYPE (arg0)) && VECTOR_TYPE_P (TREE_TYPE (arg1)))
    {
      /* Build a vector comparison.
	 VEC_COND_EXPR <e1 op e2, { -1, -1, -1, -1 }, { 0, 0, 0, 0 }>; */
      tree type = TREE_TYPE (arg0);
      tree cmptype = truth_type_for (type);
      tree cmp = fold_build2_loc (input_location, code, cmptype, arg0, arg1);

      return fold_build3_loc (input_location, VEC_COND_EXPR, type, cmp,
			      build_minus_one_cst (type),
			      build_zero_cst (type));
    }

  if (code == EQ_EXPR || code == NE_EXPR)
    {
      /* Check if comparing the address of a variable to null.  */
      if (POINTER_TYPE_P (TREE_TYPE (arg0)) && integer_zerop (arg1))
	warn_for_null_address (arg0);
      if (POINTER_TYPE_P (TREE_TYPE (arg1)) && integer_zerop (arg0))
	warn_for_null_address (arg1);
    }

  return fold_build2_loc (input_location, code, d_bool_type,
			  arg0, d_convert (TREE_TYPE (arg0), arg1));
}

/* Return a COND_EXPR.  ARG0, ARG1, and ARG2 are the three
   arguments to the conditional expression.  */

tree
build_condition (tree type, tree arg0, tree arg1, tree arg2)
{
  if (arg1 == void_node)
    arg1 = build_empty_stmt (input_location);

  if (arg2 == void_node)
    arg2 = build_empty_stmt (input_location);

  return fold_build3_loc (input_location, COND_EXPR,
			  type, arg0, arg1, arg2);
}

tree
build_vcondition (tree arg0, tree arg1, tree arg2)
{
  return build_condition (void_type_node, arg0, arg1, arg2);
}

/* Build a compound expr to join ARG0 and ARG1 together.  */

tree
compound_expr (tree arg0, tree arg1)
{
  if (arg1 == NULL_TREE)
    return arg0;

  if (arg0 == NULL_TREE || !TREE_SIDE_EFFECTS (arg0))
    return arg1;

  /* Remove intermediate expressions that have no side-effects.  */
  while (TREE_CODE (arg0) == COMPOUND_EXPR
	 && !TREE_SIDE_EFFECTS (TREE_OPERAND (arg0, 1)))
    arg0 = TREE_OPERAND (arg0, 0);

  if (TREE_CODE (arg1) == TARGET_EXPR)
    {
      /* If the rhs is a TARGET_EXPR, then build the compound expression
	 inside the target_expr's initializer.  This helps the compiler
	 to eliminate unnecessary temporaries.  */
      tree init = compound_expr (arg0, TARGET_EXPR_INITIAL (arg1));
      TARGET_EXPR_INITIAL (arg1) = init;

      return arg1;
    }

  return fold_build2_loc (input_location, COMPOUND_EXPR,
			  TREE_TYPE (arg1), arg0, arg1);
}

/* Build a return expression.  */

tree
return_expr (tree ret)
{
  /* Same as build_assign, the DECL_RESULT assignment replaces the temporary
     in TARGET_EXPR_SLOT.  */
  if (ret != NULL_TREE && TREE_CODE (ret) == TARGET_EXPR)
    {
      tree exp = TARGET_EXPR_INITIAL (ret);
      tree init = stabilize_expr (&exp);

      exp = fold_build1_loc (input_location, RETURN_EXPR, void_type_node, exp);
      TARGET_EXPR_INITIAL (ret) = compound_expr (init, exp);

      return ret;
    }

  return fold_build1_loc (input_location, RETURN_EXPR,
			  void_type_node, ret);
}

/* Return the product of ARG0 and ARG1 as a size_type_node.  */

tree
size_mult_expr (tree arg0, tree arg1)
{
  return fold_build2_loc (input_location, MULT_EXPR, size_type_node,
			  d_convert (size_type_node, arg0),
			  d_convert (size_type_node, arg1));

}

/* Return the real part of CE, which should be a complex expression.  */

tree
real_part (tree ce)
{
  return fold_build1_loc (input_location, REALPART_EXPR,
			  TREE_TYPE (TREE_TYPE (ce)), ce);
}

/* Return the imaginary part of CE, which should be a complex expression.  */

tree
imaginary_part (tree ce)
{
  return fold_build1_loc (input_location, IMAGPART_EXPR,
			  TREE_TYPE (TREE_TYPE (ce)), ce);
}

/* Build a complex expression of type TYPE using RE and IM.  */

tree
complex_expr (tree type, tree re, tree im)
{
  return fold_build2_loc (input_location, COMPLEX_EXPR,
			  type, re, im);
}

/* Build a two-field record TYPE representing the complex expression EXPR.  */

tree
underlying_complex_expr (tree type, tree expr)
{
  gcc_assert (list_length (TYPE_FIELDS (type)) == 2);

  expr = d_save_expr (expr);

  /* Build a constructor from the real and imaginary parts.  */
  if (COMPLEX_FLOAT_TYPE_P (TREE_TYPE (expr)) &&
      (!INDIRECT_REF_P (expr)
       || !CONVERT_EXPR_CODE_P (TREE_CODE (TREE_OPERAND (expr, 0)))))
    {
      vec <constructor_elt, va_gc> *ve = NULL;
      CONSTRUCTOR_APPEND_ELT (ve, TYPE_FIELDS (type),
                    real_part (expr));
      CONSTRUCTOR_APPEND_ELT (ve, TREE_CHAIN (TYPE_FIELDS (type)),
                    imaginary_part (expr));
      return build_constructor (type, ve);
    }

  /* Replace type in the reinterpret cast with a cast to the record type.  */
  return build_vconvert (type, expr);
}

/* Cast EXP (which should be a pointer) to TYPE* and then indirect.
   The back-end requires this cast in many cases.  */

tree
indirect_ref (tree type, tree exp)
{
  if (error_operand_p (exp))
    return exp;

  /* Maybe rewrite: *(e1, e2) => (e1, *e2)  */
  tree init = stabilize_expr (&exp);

  if (TREE_CODE (TREE_TYPE (exp)) == REFERENCE_TYPE)
    exp = fold_build1 (INDIRECT_REF, type, exp);
  else
    {
      exp = build_nop (build_pointer_type (type), exp);
      exp = build_deref (exp);
    }

  return compound_expr (init, exp);
}

/* Returns indirect reference of EXP, which must be a pointer type.  */

tree
build_deref (tree exp)
{
  if (error_operand_p (exp))
    return exp;

  /* Maybe rewrite: *(e1, e2) => (e1, *e2)  */
  tree init = stabilize_expr (&exp);

  gcc_assert (POINTER_TYPE_P (TREE_TYPE (exp)));

  if (TREE_CODE (exp) == ADDR_EXPR)
    exp = TREE_OPERAND (exp, 0);
  else
    exp = build_fold_indirect_ref (exp);

  return compound_expr (init, exp);
}

/* Builds pointer offset expression PTR[INDEX].  */

tree
build_pointer_index (tree ptr, tree index)
{
  if (error_operand_p (ptr) || error_operand_p (index))
    return error_mark_node;

  tree ptr_type = TREE_TYPE (ptr);
  tree target_type = TREE_TYPE (ptr_type);

  tree type = lang_hooks.types.type_for_size (TYPE_PRECISION (sizetype),
					      TYPE_UNSIGNED (sizetype));

  /* Array element size.  */
  tree size_exp = size_in_bytes (target_type);

  if (integer_zerop (size_exp) || integer_onep (size_exp))
    {
      /* Array of void or bytes -- No need to multiply.  */
      index = fold_convert (type, index);
    }
  else
    {
      index = d_convert (type, index);
      index = fold_build2 (MULT_EXPR, TREE_TYPE (index),
			   index, d_convert (TREE_TYPE (index), size_exp));
      index = fold_convert (type, index);
    }

  if (integer_zerop (index))
    return ptr;

  return fold_build2 (POINTER_PLUS_EXPR, ptr_type, ptr, index);
}

/* Builds pointer offset expression *(PTR OP OFFSET)
   OP could be a plus or minus expression.  */

tree
build_offset_op (tree_code op, tree ptr, tree offset)
{
  gcc_assert (op == MINUS_EXPR || op == PLUS_EXPR);

  tree type = lang_hooks.types.type_for_size (TYPE_PRECISION (sizetype),
					      TYPE_UNSIGNED (sizetype));
  offset = fold_convert (type, offset);

  if (op == MINUS_EXPR)
    offset = fold_build1 (NEGATE_EXPR, type, offset);

  return fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (ptr), ptr, offset);
}

/* Builds pointer offset expression *(PTR + OFFSET).  */

tree
build_offset (tree ptr, tree offset)
{
  return build_offset_op (PLUS_EXPR, ptr, offset);
}

tree
build_memref (tree type, tree ptr, tree offset)
{
  return fold_build2 (MEM_REF, type, ptr, fold_convert (type, offset));
}

/* Create a tree node to set multiple elements to a single value.  */

tree
build_array_set (tree ptr, tree length, tree value)
{
  tree ptrtype = TREE_TYPE (ptr);
  tree lentype = TREE_TYPE (length);

  push_binding_level (level_block);
  push_stmt_list ();

  /* Build temporary locals for length and ptr, and maybe value.  */
  tree t = build_local_temp (size_type_node);
  add_stmt (build_assign (INIT_EXPR, t, length));
  length = t;

  t = build_local_temp (ptrtype);
  add_stmt (build_assign (INIT_EXPR, t, ptr));
  ptr = t;

  if (TREE_SIDE_EFFECTS (value))
    {
      t = build_local_temp (TREE_TYPE (value));
      add_stmt (build_assign (INIT_EXPR, t, value));
      value = t;
    }

  /* Build loop to initialize { .length=length, .ptr=ptr } with value.  */
  push_stmt_list ();

  /* Exit logic for the loop.
	if (length == 0) break;  */
  t = build_boolop (EQ_EXPR, length, d_convert (lentype, integer_zero_node));
  t = build1 (EXIT_EXPR, void_type_node, t);
  add_stmt (t);

  /* Assign value to the current pointer position.
	*ptr = value;  */
  t = modify_expr (build_deref (ptr), value);
  add_stmt (t);

  /* Move pointer to next element position.
	ptr++;  */
  tree size = TYPE_SIZE_UNIT (TREE_TYPE (ptrtype));
  t = build2 (POSTINCREMENT_EXPR, ptrtype, ptr, d_convert (ptrtype, size));
  add_stmt (t);

  /* Decrease loop counter.
	length -= 1;  */
  t = build2 (POSTDECREMENT_EXPR, lentype, length,
	      d_convert (lentype, integer_one_node));
  add_stmt (t);

  /* Pop statements and finish loop.  */
  tree loop_body = pop_stmt_list ();
  add_stmt (build1 (LOOP_EXPR, void_type_node, loop_body));

  /* Wrap it up into a bind expression.  */
  tree stmt_list = pop_stmt_list ();
  tree block = pop_binding_level ();

  return build3 (BIND_EXPR, void_type_node,
		 BLOCK_VARS (block), stmt_list, block);
}


/* Build an array of type TYPE where all the elements are VAL.  */

tree
build_array_from_val (Type *type, tree val)
{
  tree etype = build_ctype (type->nextOf ());

  /* Initializing a multidimensional array.  */
  if (TREE_CODE (etype) == ARRAY_TYPE && TREE_TYPE (val) != etype)
    val = build_array_from_val (type->nextOf (), val);

  size_t dims = type->isTypeSArray ()->dim->toInteger ();
  vec <constructor_elt, va_gc> *elms = NULL;
  vec_safe_reserve (elms, dims);

  val = d_convert (etype, val);

  for (size_t i = 0; i < dims; i++)
    CONSTRUCTOR_APPEND_ELT (elms, size_int (i), val);

  return build_constructor (build_ctype (type), elms);
}

/* Build a static array of type TYPE from an array of EXPS.
   If CONST_P is true, then all elements in EXPS are constants.  */

tree
build_array_from_exprs (Type *type, Expressions *exps, bool const_p)
{
  /* Build a CONSTRUCTOR from all expressions.  */
  vec <constructor_elt, va_gc> *elms = NULL;
  vec_safe_reserve (elms, exps->length);

  Type *etype = type->nextOf ();
  tree satype = make_array_type (etype, exps->length);

  for (size_t i = 0; i < exps->length; i++)
    {
      Expression *expr = (*exps)[i];
      tree t = build_expr (expr, const_p);
      CONSTRUCTOR_APPEND_ELT (elms, size_int (i),
			      convert_expr (t, expr->type, etype));
    }

  /* Create a new temporary to store the array.  */
  tree var = build_local_temp (satype);

  /* Fill any alignment holes with zeroes.  */
  TypeStruct *ts = etype->baseElemOf ()->isTypeStruct ();
  tree init = NULL;
  if (ts && (!identity_compare_p (ts->sym) || ts->sym->isUnionDeclaration ()))
    init = build_memset_call (var);

  /* Initialize the temporary.  */
  tree assign = modify_expr (var, build_constructor (satype, elms));
  return compound_expr (compound_expr (init, assign), var);
}


/* Implicitly converts void* T to byte* as D allows { void[] a; &a[3]; }  */

tree
void_okay_p (tree t)
{
  tree type = TREE_TYPE (t);

  if (VOID_TYPE_P (TREE_TYPE (type)))
    {
      tree totype = build_ctype (Type::tuns8->pointerTo ());
      return fold_convert (totype, t);
    }

  return t;
}

/* Builds a STRING_CST representing the filename of location LOC.  When the
   location is not valid, the name of the source module is used instead.  */

static tree
build_filename_from_loc (const Loc &loc)
{
  const char *filename = loc.filename
    ? loc.filename : d_function_chain->module->srcfile.toChars ();

  unsigned length = strlen (filename);
  tree str = build_string (length, filename);
  TREE_TYPE (str) = make_array_type (Type::tchar, length + 1);

  return build_address (str);
}

/* Builds a CALL_EXPR at location LOC in the source file to call LIBCALL when
   an assert check fails.  When calling the msg variant functions, MSG is the
   error message supplied by the user.  */

tree
build_assert_call (const Loc &loc, libcall_fn libcall, tree msg)
{
  tree file;
  tree line = size_int (loc.linnum);

  switch (libcall)
    {
    case LIBCALL_ASSERT_MSG:
    case LIBCALL_UNITTEST_MSG:
      /* File location is passed as a D string.  */
      if (loc.filename)
	{
	  unsigned len = strlen (loc.filename);
	  tree str = build_string (len, loc.filename);
	  TREE_TYPE (str) = make_array_type (Type::tchar, len);

	  file = d_array_value (build_ctype (Type::tchar->arrayOf ()),
				size_int (len), build_address (str));
	}
      else
	file = null_array_node;
      break;

    case LIBCALL_ASSERTP:
    case LIBCALL_UNITTESTP:
      file = build_filename_from_loc (loc);
      break;

    default:
      gcc_unreachable ();
    }


  if (msg != NULL_TREE)
    return build_libcall (libcall, Type::tvoid, 3, msg, file, line);
  else
    return build_libcall (libcall, Type::tvoid, 2, file, line);
}

/* Builds a CALL_EXPR at location LOC in the source file to execute when an
   array bounds check fails.  */

tree
build_array_bounds_call (const Loc &loc)
{
  /* Terminate the program with a trap if no D runtime present.  */
  if (checkaction_trap_p ())
    return build_call_expr (builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      return build_libcall (LIBCALL_ARRAYBOUNDSP, Type::tvoid, 2,
			    build_filename_from_loc (loc),
			    size_int (loc.linnum));
    }
}

/* Builds a bounds condition checking that INDEX is between 0 and LENGTH
   in the index expression IE.  The condition returns the INDEX if true, or
   throws a `ArrayIndexError`.  */

tree
build_bounds_index_condition (IndexExp *ie, tree index, tree length)
{
  if (ie->indexIsInBounds || !array_bounds_check ())
    return index;

  /* Prevent multiple evaluations of the index.  */
  index = d_save_expr (index);

  /* Generate INDEX >= LENGTH && throw RangeError.
     No need to check whether INDEX >= 0 as the front-end should
     have already taken care of implicit casts to unsigned.  */
  tree condition = fold_build2 (GE_EXPR, d_bool_type, index, length);
  tree boundserr;

  if (checkaction_trap_p ())
    boundserr = build_call_expr (builtin_decl_explicit (BUILT_IN_TRAP), 0);
  else
    {
      boundserr = build_libcall (LIBCALL_ARRAYBOUNDS_INDEXP, Type::tvoid, 4,
				 build_filename_from_loc (ie->e2->loc),
				 size_int (ie->e2->loc.linnum), index, length);
    }

  return build_condition (TREE_TYPE (index), condition, boundserr, index);
}

/* Builds a bounds condition checking that the range LOWER..UPPER do not overlap
   the slice expression SE of the source array length LENGTH.  The condition
   returns the new array length if true, or throws an `ArraySliceError`.  */

tree
build_bounds_slice_condition (SliceExp *se, tree lower, tree upper, tree length)
{
  if (array_bounds_check ())
    {
      tree condition = NULL_TREE;

      /* Enforces that `upper <= length`.  */
      if (!se->upperIsInBounds && length != NULL_TREE)
	condition = fold_build2 (GT_EXPR, d_bool_type, upper, length);
      else
	length = integer_zero_node;

      /* Enforces that `lower <= upper`.  No need to check `lower <= length` as
	 we've already ensured that `upper <= length`.  */
      if (!se->lowerIsLessThanUpper)
	{
	  tree lwr_cond = fold_build2 (GT_EXPR, d_bool_type, lower, upper);

	  if (condition != NULL_TREE)
	    condition = build_boolop (TRUTH_ORIF_EXPR, condition, lwr_cond);
	  else
	    condition = lwr_cond;
	}

      if (condition != NULL_TREE)
	{
	  tree boundserr;

	  if (checkaction_trap_p ())
	    {
	      boundserr =
		build_call_expr (builtin_decl_explicit (BUILT_IN_TRAP), 0);
	    }
	  else
	    {
	      boundserr = build_libcall (LIBCALL_ARRAYBOUNDS_SLICEP,
					 Type::tvoid, 5,
					 build_filename_from_loc (se->loc),
					 size_int (se->loc.linnum),
					 lower, upper, length);
	    }

	  upper = build_condition (TREE_TYPE (upper), condition,
				   boundserr, upper);
	}
    }

  /* Need to ensure lower always gets evaluated first, as it may be a function
     call.  Generates (lower, upper) - lower.  */
  return fold_build2 (MINUS_EXPR, TREE_TYPE (upper),
		      compound_expr (lower, upper), lower);
}

/* Returns TRUE if array bounds checking code generation is turned on.  */

bool
array_bounds_check (void)
{
  FuncDeclaration *fd;

  switch (global.params.useArrayBounds)
    {
    case CHECKENABLEoff:
      return false;

    case CHECKENABLEon:
      return true;

    case CHECKENABLEsafeonly:
      /* For D2 safe functions only.  */
      fd = d_function_chain->function;
      if (fd && fd->type->ty == TY::Tfunction)
	{
	  if (fd->type->isTypeFunction ()->trust == TRUST::safe)
	    return true;
	}
      return false;

    default:
      gcc_unreachable ();
    }
}

/* Returns TRUE if we terminate the program with a trap if an array bounds or
   contract check fails.  */

bool
checkaction_trap_p (void)
{
  switch (global.params.checkAction)
    {
    case CHECKACTION_D:
    case CHECKACTION_context:
      return false;

    case CHECKACTION_C:
    case CHECKACTION_halt:
      return true;

    default:
      gcc_unreachable ();
    }
}

/* Returns the TypeFunction class for Type T.
   Assumes T is already ->toBasetype().  */

TypeFunction *
get_function_type (Type *t)
{
  TypeFunction *tf = NULL;
  if (t->ty == TY::Tpointer)
    t = t->nextOf ()->toBasetype ();
  if (t->ty == TY::Tfunction)
    tf = t->isTypeFunction ();
  else if (t->ty == TY::Tdelegate)
    tf = t->isTypeDelegate ()->next->isTypeFunction ();
  return tf;
}

/* Returns TRUE if CALLEE is a plain nested function outside the scope of
   CALLER.  In which case, CALLEE is being called through an alias that was
   passed to CALLER.  */

bool
call_by_alias_p (FuncDeclaration *caller, FuncDeclaration *callee)
{
  if (!callee->isNested ())
    return false;

  if (caller->toParent () == callee->toParent ())
    return false;

  Dsymbol *dsym = callee;

  while (dsym)
    {
      if (dsym->isTemplateInstance ())
	return false;
      else if (dsym->isFuncDeclaration () == caller)
	return false;
      dsym = dsym->toParent ();
    }

  return true;
}

/* Entry point for call routines.  Builds a function call to FD.
   OBJECT is the `this' reference passed and ARGS are the arguments to FD.  */

tree
d_build_call_expr (FuncDeclaration *fd, tree object, Expressions *arguments)
{
  return d_build_call (get_function_type (fd->type),
		       build_address (get_symbol_decl (fd)), object, arguments);
}

/* Builds a CALL_EXPR of type TF to CALLABLE.  OBJECT holds the `this' pointer,
   ARGUMENTS are evaluated in left to right order, saved and promoted
   before passing.  */

tree
d_build_call (TypeFunction *tf, tree callable, tree object,
	      Expressions *arguments)
{
  tree ctype = TREE_TYPE (callable);
  tree callee = callable;

  if (POINTER_TYPE_P (ctype))
    ctype = TREE_TYPE (ctype);
  else
    callee = build_address (callable);

  gcc_assert (FUNC_OR_METHOD_TYPE_P (ctype));
  gcc_assert (tf != NULL);
  gcc_assert (tf->ty == TY::Tfunction);

  if (TREE_CODE (ctype) != FUNCTION_TYPE && object == NULL_TREE)
    {
      /* Front-end apparently doesn't check this.  */
      if (TREE_CODE (callable) == FUNCTION_DECL)
	{
	  error ("need %<this%> to access member %qE", DECL_NAME (callable));
	  return error_mark_node;
	}

      /* Probably an internal error.  */
      gcc_unreachable ();
    }

  /* Build the argument list for the call.  */
  vec <tree, va_gc> *args = NULL;
  tree saved_args = NULL_TREE;
  bool noreturn_call = false;

  /* If this is a delegate call or a nested function being called as
     a delegate, the object should not be NULL.  */
  if (object != NULL_TREE)
    vec_safe_push (args, object);

  if (arguments)
    {
      /* First pass, evaluated expanded tuples in function arguments.  */
      for (size_t i = 0; i < arguments->length; ++i)
	{
	Lagain:
	  Expression *arg = (*arguments)[i];
	  gcc_assert (arg->op != EXP::tuple);

	  if (arg->op == EXP::comma)
	    {
	      CommaExp *ce = arg->isCommaExp ();
	      tree tce = build_expr (ce->e1);
	      saved_args = compound_expr (saved_args, tce);
	      (*arguments)[i] = ce->e2;
	      goto Lagain;
	    }
	}

      const size_t nparams = tf->parameterList.length ();
      /* if _arguments[] is the first argument.  */
      const size_t varargs = tf->isDstyleVariadic ();

      /* Assumes arguments->length <= formal_args->length if (!tf->varargs).  */
      for (size_t i = 0; i < arguments->length; ++i)
	{
	  Expression *arg = (*arguments)[i];
	  tree targ = build_expr (arg);

	  if (i - varargs < nparams && i >= varargs)
	    {
	      /* Actual arguments for declared formal arguments.  */
	      Parameter *parg = tf->parameterList[i - varargs];
	      targ = convert_for_argument (targ, parg);
	    }

	  /* Don't pass empty aggregates by value.  */
	  if (empty_aggregate_p (TREE_TYPE (targ)) && !TREE_ADDRESSABLE (targ)
	      && TREE_CODE (targ) != CONSTRUCTOR)
	    {
	      tree t = build_constructor (TREE_TYPE (targ), NULL);
	      targ = build2 (COMPOUND_EXPR, TREE_TYPE (t), targ, t);
	    }

	  /* Parameter is a struct or array passed by invisible reference.  */
	  if (TREE_ADDRESSABLE (TREE_TYPE (targ)))
	    {
	      Type *t = arg->type->toBasetype ();
	      StructDeclaration *sd = t->baseElemOf ()->isTypeStruct ()->sym;

	      /* Nested structs also have ADDRESSABLE set, but if the type has
		 neither a copy constructor nor a destructor available, then we
		 need to take care of copying its value before passing it.  */
	      if (arg->op == EXP::structLiteral || (!sd->postblit && !sd->dtor))
		targ = force_target_expr (targ);

	      targ = convert (build_reference_type (TREE_TYPE (targ)),
			      build_address (targ));
	    }

	  /* Complex types are exposed as special types with an underlying
	     struct representation, if we are passing the native type to a
	     function that accepts the library-defined version, then ensure
	     it is properly reinterpreted as the underlying struct type.  */
	  if (COMPLEX_FLOAT_TYPE_P (TREE_TYPE (targ))
	      && arg->type->isTypeStruct ())
	    targ = underlying_complex_expr (build_ctype (arg->type), targ);

	  /* Type `noreturn` is a terminator, as no other arguments can possibly
	     be evaluated after it.  */
	  if (TREE_TYPE (targ) == noreturn_type_node)
	    noreturn_call = true;

	  vec_safe_push (args, targ);
	}
    }

  /* Evaluate the callee before calling it.  */
  if (TREE_SIDE_EFFECTS (callee))
    {
      callee = d_save_expr (callee);
      saved_args = compound_expr (callee, saved_args);
    }

  /* If we saw a `noreturn` parameter, any unreachable argument evaluations
     after it are discarded, as well as the function call itself.  */
  if (noreturn_call)
    {
      if (TREE_SIDE_EFFECTS (callee))
	saved_args = compound_expr (callee, saved_args);

      tree arg;
      unsigned int ix;

      FOR_EACH_VEC_SAFE_ELT (args, ix, arg)
	saved_args = compound_expr (saved_args, arg);

      /* Add a stub result type for the expression.  */
      tree result = build_zero_cst (TREE_TYPE (ctype));
      return compound_expr (saved_args, result);
    }

  tree result = build_call_vec (TREE_TYPE (ctype), callee, args);
  SET_EXPR_LOCATION (result, input_location);

  result = maybe_expand_intrinsic (result);

  /* Return the value in a temporary slot so that it can be evaluated
     multiple times by the caller.  */
  if (TREE_CODE (result) == CALL_EXPR
      && AGGREGATE_TYPE_P (TREE_TYPE (result))
      && TREE_ADDRESSABLE (TREE_TYPE (result)))
    {
      CALL_EXPR_RETURN_SLOT_OPT (result) = true;
      result = force_target_expr (result);
    }

  return compound_expr (saved_args, result);
}

/* Build and return the correct call to fmod depending on TYPE.
   ARG0 and ARG1 are the arguments pass to the function.  */

tree
build_float_modulus (tree type, tree arg0, tree arg1)
{
  tree fmodfn = NULL_TREE;
  tree basetype = type;

  if (COMPLEX_FLOAT_TYPE_P (basetype))
    basetype = TREE_TYPE (basetype);

  if (TYPE_MAIN_VARIANT (basetype) == double_type_node
      || TYPE_MAIN_VARIANT (basetype) == idouble_type_node)
    fmodfn = builtin_decl_explicit (BUILT_IN_FMOD);
  else if (TYPE_MAIN_VARIANT (basetype) == float_type_node
	   || TYPE_MAIN_VARIANT (basetype) == ifloat_type_node)
    fmodfn = builtin_decl_explicit (BUILT_IN_FMODF);
  else if (TYPE_MAIN_VARIANT (basetype) == long_double_type_node
	   || TYPE_MAIN_VARIANT (basetype) == ireal_type_node)
    fmodfn = builtin_decl_explicit (BUILT_IN_FMODL);

  if (!fmodfn)
    {
      error ("tried to perform floating-point modulo division on %qT", type);
      return error_mark_node;
    }

  if (COMPLEX_FLOAT_TYPE_P (type))
    {
      tree re = build_call_expr (fmodfn, 2, real_part (arg0), arg1);
      tree im = build_call_expr (fmodfn, 2, imaginary_part (arg0), arg1);

      return complex_expr (type, re, im);
    }

  if (SCALAR_FLOAT_TYPE_P (type))
    return build_call_expr (fmodfn, 2, arg0, arg1);

  /* Should have caught this above.  */
  gcc_unreachable ();
}

/* Build a function type whose first argument is a pointer to BASETYPE,
   which is to be used for the `vthis' context parameter for TYPE.
   The base type may be a record for member functions, or a void for
   nested functions and delegates.  */

tree
build_vthis_function (tree basetype, tree type)
{
  gcc_assert (TREE_CODE (type) == FUNCTION_TYPE);

  tree argtypes = tree_cons (NULL_TREE, build_pointer_type (basetype),
			     TYPE_ARG_TYPES (type));
  tree fntype = build_function_type (TREE_TYPE (type), argtypes);

  /* Copy volatile qualifiers from the original function type.  */
  if (TYPE_QUALS (type) & TYPE_QUAL_VOLATILE)
    fntype = build_qualified_type (fntype, TYPE_QUAL_VOLATILE);

  if (RECORD_OR_UNION_TYPE_P (basetype))
    TYPE_METHOD_BASETYPE (fntype) = TYPE_MAIN_VARIANT (basetype);
  else
    gcc_assert (VOID_TYPE_P (basetype));

  return fntype;
}

/* Raise an error at that the context pointer of the function or object SYM is
   not accessible from the current scope.  */

tree
error_no_frame_access (Dsymbol *sym)
{
  error_at (input_location, "cannot get frame pointer to %qs",
	    sym->toPrettyChars ());
  return null_pointer_node;
}

/* If SYM is a nested function, return the static chain to be
   used when calling that function from the current function.

   If SYM is a nested class or struct, return the static chain
   to be used when creating an instance of the class from CFUN.  */

tree
get_frame_for_symbol (Dsymbol *sym)
{
  FuncDeclaration *thisfd
    = d_function_chain ? d_function_chain->function : NULL;
  FuncDeclaration *fd = sym->isFuncDeclaration ();
  FuncDeclaration *fdparent = NULL;
  FuncDeclaration *fdoverride = NULL;

  if (fd != NULL)
    {
      /* Check that the nested function is properly defined.  */
      if (!fd->fbody)
	{
	  /* Should instead error on line that references `fd'.  */
	  error_at (make_location_t (fd->loc), "nested function missing body");
	  return null_pointer_node;
	}

      fdparent = fd->toParent2 ()->isFuncDeclaration ();

      /* Special case for __ensure and __require.  */
      if ((fd->ident == Identifier::idPool ("__ensure")
	   || fd->ident == Identifier::idPool ("__require"))
	  && fdparent != thisfd)
	{
	  fdoverride = fdparent;
	  fdparent = thisfd;
	}
    }
  else
    {
      /* It's a class (or struct).  NewExp codegen has already determined its
	 outer scope is not another class, so it must be a function.  */
      while (sym && !sym->isFuncDeclaration ())
	sym = sym->toParent2 ();

      fdparent = (FuncDeclaration *) sym;
    }

  /* Not a nested function, there is no frame pointer to pass.  */
  if (fdparent == NULL)
    {
      /* Only delegate literals report as being nested, even if they are in
	 global scope.  */
      gcc_assert (fd && fd->isFuncLiteralDeclaration ());
      return null_pointer_node;
    }

  gcc_assert (thisfd != NULL);

  if (thisfd != fdparent)
    {
      /* If no frame pointer for this function.  */
      if (!thisfd->vthis)
	{
	  error_at (make_location_t (sym->loc),
		    "%qs is a nested function and cannot be accessed from %qs",
		    fdparent->toPrettyChars (), thisfd->toPrettyChars ());
	  return null_pointer_node;
	}

      /* Make sure we can get the frame pointer to the outer function.
	 Go up each nesting level until we find the enclosing function.  */
      Dsymbol *dsym = thisfd;

      while (fd != dsym)
	{
	  /* Check if enclosing function is a function.  */
	  FuncDeclaration *fdp = dsym->isFuncDeclaration ();
	  Dsymbol *parent = dsym->toParent2 ();

	  if (fdp != NULL)
	    {
	      if (fdparent == parent)
		break;

	      gcc_assert (fdp->isNested () || fdp->vthis);
	      dsym = parent;
	      continue;
	    }

	  /* Check if enclosed by an aggregate.  That means the current
	     function must be a member function of that aggregate.  */
	  AggregateDeclaration *adp = dsym->isAggregateDeclaration ();

	  if (adp != NULL)
	    {
	      if ((adp->isClassDeclaration () || adp->isStructDeclaration ())
		  && fdparent == parent)
		break;
	    }

	  /* No frame to outer function found.  */
	  if (!adp || !adp->isNested () || !adp->vthis)
	    return error_no_frame_access (sym);

	  dsym = parent;
	}
    }

  tree ffo = get_frameinfo (fdparent);
  if (FRAMEINFO_CREATES_FRAME (ffo) || FRAMEINFO_STATIC_CHAIN (ffo))
    {
      tree frame_ref = get_framedecl (thisfd, fdparent);

      /* If `thisfd' is a derived member function, then `fdparent' is the
	 overridden member function in the base class.  Even if there's a
	 closure environment, we should give the original stack data as the
	 nested function frame.  */
      if (fdoverride)
	{
	  ClassDeclaration *cdo = fdoverride->isThis ()->isClassDeclaration ();
	  ClassDeclaration *cd = thisfd->isThis ()->isClassDeclaration ();
	  gcc_assert (cdo && cd);

	  int offset;
	  if (cdo->isBaseOf (cd, &offset) && offset != 0)
	    {
	      /* Generate a new frame to pass to the overriden function that
		 has the `this' pointer adjusted.  */
	      gcc_assert (offset != OFFSET_RUNTIME);

	      tree type = FRAMEINFO_TYPE (get_frameinfo (fdoverride));
	      tree fields = TYPE_FIELDS (type);
	      /* The `this' field comes immediately after the `__chain'.  */
	      tree thisfield = chain_index (1, fields);
	      vec <constructor_elt, va_gc> *ve = NULL;

	      tree framefields = TYPE_FIELDS (FRAMEINFO_TYPE (ffo));
	      frame_ref = build_deref (frame_ref);

	      for (tree field = fields; field; field = DECL_CHAIN (field))
		{
		  tree value = component_ref (frame_ref, framefields);
		  if (field == thisfield)
		    value = build_offset (value, size_int (offset));

		  CONSTRUCTOR_APPEND_ELT (ve, field, value);
		  framefields = DECL_CHAIN (framefields);
		}

	      frame_ref = build_address (build_constructor (type, ve));
	    }
	}

      return frame_ref;
    }

  return null_pointer_node;
}

/* Return the parent function of a nested class or struct AD.  */

static FuncDeclaration *
get_outer_function (AggregateDeclaration *ad)
{
  FuncDeclaration *fd = NULL;
  while (ad && ad->isNested ())
    {
      Dsymbol *dsym = ad->toParent2 ();
      if ((fd = dsym->isFuncDeclaration ()))
	return fd;
      else
	ad = dsym->isAggregateDeclaration ();
    }

  return NULL;
}

/* Starting from the current function FD, try to find a suitable value of
   `this' in nested function instances.  A suitable `this' value is an
   instance of OCD or a class that has OCD as a base.  */

static tree
find_this_tree (ClassDeclaration *ocd)
{
  FuncDeclaration *fd = d_function_chain ? d_function_chain->function : NULL;

  while (fd)
    {
      AggregateDeclaration *ad = fd->isThis ();
      ClassDeclaration *cd = ad ? ad->isClassDeclaration () : NULL;

      if (cd != NULL)
	{
	  if (ocd == cd)
	    return get_decl_tree (fd->vthis);
	  else if (ocd->isBaseOf (cd, NULL))
	    return convert_expr (get_decl_tree (fd->vthis),
				 cd->type, ocd->type);

	  fd = get_outer_function (cd);
	  continue;
	}

      if (fd->isNested ())
	{
	  fd = fd->toParent2 ()->isFuncDeclaration ();
	  continue;
	}

      fd = NULL;
    }

  return NULL_TREE;
}

/* Retrieve the outer class/struct `this' value of DECL from
   the current function.  */

tree
build_vthis (AggregateDeclaration *decl)
{
  ClassDeclaration *cd = decl->isClassDeclaration ();
  StructDeclaration *sd = decl->isStructDeclaration ();

  /* If an aggregate nested in a function has no methods and there are no
     other nested functions, any static chain created here will never be
     translated.  Use a null pointer for the link in this case.  */
  tree vthis_value = null_pointer_node;

  if (cd != NULL || sd != NULL)
    {
      Dsymbol *outer = decl->toParent2 ();

      /* If the parent is a templated struct, the outer context is instead
	 the enclosing symbol of where the instantiation happened.  */
      if (outer->isStructDeclaration ())
	{
	  gcc_assert (outer->parent && outer->parent->isTemplateInstance ());
	  outer = ((TemplateInstance *) outer->parent)->enclosing;
	}

      /* For outer classes, get a suitable `this' value.
	 For outer functions, get a suitable frame/closure pointer.  */
      ClassDeclaration *cdo = outer->isClassDeclaration ();
      FuncDeclaration *fdo = outer->isFuncDeclaration ();

      if (cdo)
	{
	  vthis_value = find_this_tree (cdo);
	  gcc_assert (vthis_value != NULL_TREE);
	}
      else if (fdo)
	{
	  tree ffo = get_frameinfo (fdo);
	  if (FRAMEINFO_CREATES_FRAME (ffo) || FRAMEINFO_STATIC_CHAIN (ffo)
	      || fdo->hasNestedFrameRefs ())
	    vthis_value = get_frame_for_symbol (decl);
	  else if (cd != NULL)
	    {
	      /* Classes nested in methods are allowed to access any outer
		 class fields, use the function chain in this case.  */
	      if (fdo->vthis && fdo->vthis->type != Type::tvoidptr)
		vthis_value = get_decl_tree (fdo->vthis);
	    }
	}
      else
	gcc_unreachable ();
    }

  return vthis_value;
}

/* Build the RECORD_TYPE that describes the function frame or closure type for
   the function FD.  FFI is the tree holding all frame information.  */

static tree
build_frame_type (tree ffi, FuncDeclaration *fd)
{
  if (FRAMEINFO_TYPE (ffi))
    return FRAMEINFO_TYPE (ffi);

  tree frame_rec_type = make_node (RECORD_TYPE);
  char *name = concat (FRAMEINFO_IS_CLOSURE (ffi) ? "CLOSURE." : "FRAME.",
		       fd->toPrettyChars (), NULL);
  TYPE_NAME (frame_rec_type) = get_identifier (name);
  free (name);

  tree fields = NULL_TREE;

  /* Function is a member or nested, so must have field for outer context.  */
  if (fd->vthis)
    {
      tree ptr_field = build_decl (BUILTINS_LOCATION, FIELD_DECL,
				   get_identifier ("__chain"), ptr_type_node);
      DECL_FIELD_CONTEXT (ptr_field) = frame_rec_type;
      fields = chainon (NULL_TREE, ptr_field);
      DECL_NONADDRESSABLE_P (ptr_field) = 1;
    }

  /* The __ensure and __require are called directly, so never make the outer
     functions closure, but nevertheless could still be referencing parameters
     of the calling function non-locally.  So we add all parameters with nested
     refs to the function frame, this should also mean overriding methods will
     have the same frame layout when inheriting a contract.  */
  if ((global.params.useIn == CHECKENABLEon && fd->frequire)
      || (global.params.useOut == CHECKENABLEon && fd->fensure))
    {
      if (fd->parameters)
	{
	  for (size_t i = 0; fd->parameters && i < fd->parameters->length; i++)
	    {
	      VarDeclaration *v = (*fd->parameters)[i];
	      /* Remove if already in closureVars so can push to front.  */
	      size_t j = fd->closureVars.find (v);

	      if (j < fd->closureVars.length)
		fd->closureVars.remove (j);

	      fd->closureVars.insert (i, v);
	    }
	}

      /* Also add hidden `this' to outer context.  */
      if (fd->vthis)
	{
	  size_t i = fd->closureVars.find (fd->vthis);

	  if (i < fd->closureVars.length)
	    fd->closureVars.remove (i);

	  fd->closureVars.insert (0, fd->vthis);
	}
    }

  for (size_t i = 0; i < fd->closureVars.length; i++)
    {
      VarDeclaration *v = fd->closureVars[i];
      tree vsym = get_symbol_decl (v);
      tree ident = v->ident
	? get_identifier (v->ident->toChars ()) : NULL_TREE;

      tree field = build_decl (make_location_t (v->loc), FIELD_DECL, ident,
			       TREE_TYPE (vsym));
      SET_DECL_LANG_FRAME_FIELD (vsym, field);
      DECL_FIELD_CONTEXT (field) = frame_rec_type;
      fields = chainon (fields, field);
      TREE_USED (vsym) = 1;

      TREE_ADDRESSABLE (field) = TREE_ADDRESSABLE (vsym);
      DECL_NONADDRESSABLE_P (field) = !TREE_ADDRESSABLE (vsym);
      TREE_THIS_VOLATILE (field) = TREE_THIS_VOLATILE (vsym);

      if (DECL_LANG_NRVO (vsym))
	{
	  /* Store the nrvo variable in the frame by reference.  */
	  TREE_TYPE (field) = build_reference_type (TREE_TYPE (field));

	  /* Can't do nrvo if the variable is put in a closure, since what the
	     return slot points to may no longer exist.  */
	  gcc_assert (!FRAMEINFO_IS_CLOSURE (ffi));
	}

      if (FRAMEINFO_IS_CLOSURE (ffi))
	{
	  /* Because the value needs to survive the end of the scope.  */
	  if ((v->edtor && (v->storage_class & STCparameter))
	      || v->needsScopeDtor ())
	    error_at (make_location_t (v->loc),
		      "variable %qs has scoped destruction, "
		      "cannot build closure", v->toChars ());
	}

      if (DECL_REGISTER (vsym))
	{
	  /* Because the value will be in memory, not a register.  */
	  error_at (make_location_t (v->loc),
		    "explicit register variable %qs cannot be used in nested "
		    "function", v->toChars ());
	}
    }

  TYPE_FIELDS (frame_rec_type) = fields;
  TYPE_READONLY (frame_rec_type) = 1;
  TYPE_CXX_ODR_P (frame_rec_type) = 1;
  layout_type (frame_rec_type);
  d_keep (frame_rec_type);

  return frame_rec_type;
}

/* Closures are implemented by taking the local variables that
   need to survive the scope of the function, and copying them
   into a GC allocated chuck of memory.  That chunk, called the
   closure here, is inserted into the linked list of stack
   frames instead of the usual stack frame.

   If a closure is not required, but FD still needs a frame to lower
   nested refs, then instead build custom static chain decl on stack.  */

void
build_closure (FuncDeclaration *fd)
{
  tree ffi = get_frameinfo (fd);

  if (!FRAMEINFO_CREATES_FRAME (ffi))
    return;

  tree type = FRAMEINFO_TYPE (ffi);
  gcc_assert (COMPLETE_TYPE_P (type));

  tree decl, decl_ref;

  if (FRAMEINFO_IS_CLOSURE (ffi))
    {
      decl = build_local_temp (build_pointer_type (type));
      DECL_NAME (decl) = get_identifier ("__closptr");
      decl_ref = build_deref (decl);

      /* Allocate memory for closure.  */
      tree arg = convert (build_ctype (Type::tsize_t), TYPE_SIZE_UNIT (type));
      tree init = build_libcall (LIBCALL_ALLOCMEMORY, Type::tvoidptr, 1, arg);

      tree init_exp = build_assign (INIT_EXPR, decl,
				    build_nop (TREE_TYPE (decl), init));
      add_stmt (init_exp);
    }
  else
    {
      decl = build_local_temp (type);
      DECL_NAME (decl) = get_identifier ("__frame");
      decl_ref = decl;
    }

  /* Set the first entry to the parent closure/frame, if any.  */
  if (fd->vthis)
    {
      tree chain_field = component_ref (decl_ref, TYPE_FIELDS (type));
      tree chain_expr = modify_expr (chain_field,
				     d_function_chain->static_chain);
      add_stmt (chain_expr);
    }

  /* Copy parameters that are referenced nonlocally.  */
  for (size_t i = 0; i < fd->closureVars.length; i++)
    {
      VarDeclaration *v = fd->closureVars[i];
      tree vsym = get_symbol_decl (v);

      if (TREE_CODE (vsym) != PARM_DECL && !DECL_LANG_NRVO (vsym))
	continue;

      tree field = component_ref (decl_ref, DECL_LANG_FRAME_FIELD (vsym));

      /* Variable is an alias for the NRVO slot, store the reference.  */
      if (DECL_LANG_NRVO (vsym))
	vsym = build_address (DECL_LANG_NRVO (vsym));

      tree expr = modify_expr (field, vsym);
      add_stmt (expr);
    }

  if (!FRAMEINFO_IS_CLOSURE (ffi))
    decl = build_address (decl);

  d_function_chain->static_chain = decl;
}

/* Return the frame of FD.  This could be a static chain or a closure
   passed via the hidden `this' pointer.  */

tree
get_frameinfo (FuncDeclaration *fd)
{
  tree fds = get_symbol_decl (fd);
  if (DECL_LANG_FRAMEINFO (fds))
    return DECL_LANG_FRAMEINFO (fds);

  tree ffi = make_node (FUNCFRAME_INFO);

  DECL_LANG_FRAMEINFO (fds) = ffi;

  const bool requiresClosure = fd->requiresClosure;
  if (fd->needsClosure ())
    {
      /* This can shift due to templates being expanded that access alias
         symbols, give it a decent error for now.  */
      if (requiresClosure != fd->requiresClosure
	  && (fd->nrvo_var || global.params.betterC))
	fd->checkClosure ();

      /* Set-up a closure frame, this will be allocated on the heap.  */
      FRAMEINFO_CREATES_FRAME (ffi) = 1;
      FRAMEINFO_IS_CLOSURE (ffi) = 1;
    }
  else if (fd->hasNestedFrameRefs ())
    {
      /* Functions with nested refs must create a static frame for local
	 variables to be referenced from.  */
      FRAMEINFO_CREATES_FRAME (ffi) = 1;
    }
  else
    {
      /* For nested functions, default to creating a frame.  Even if there are
	 no fields to populate the frame, create it anyway, as this will be
	 used as the record type instead of `void*` for the this parameter.  */
      if (fd->vthis && fd->vthis->type == Type::tvoidptr)
	FRAMEINFO_CREATES_FRAME (ffi) = 1;

      /* In checkNestedReference, references from contracts are not added to the
	 closureVars array, so assume all parameters referenced.  */
      if ((global.params.useIn == CHECKENABLEon && fd->frequire)
	  || (global.params.useOut == CHECKENABLEon && fd->fensure))
	FRAMEINFO_CREATES_FRAME (ffi) = 1;

      /* If however `fd` is nested (deeply) in a function that creates a
	 closure, then `fd` instead inherits that closure via hidden vthis
	 pointer, and doesn't create a stack frame at all.  */
      FuncDeclaration *ff = fd;

      while (ff)
	{
	  tree ffo = get_frameinfo (ff);

	  if (ff != fd && FRAMEINFO_CREATES_FRAME (ffo))
	    {
	      gcc_assert (FRAMEINFO_TYPE (ffo));
	      FRAMEINFO_CREATES_FRAME (ffi) = 0;
	      FRAMEINFO_STATIC_CHAIN (ffi) = 1;
	      FRAMEINFO_IS_CLOSURE (ffi) = FRAMEINFO_IS_CLOSURE (ffo);
	      gcc_assert (COMPLETE_TYPE_P (FRAMEINFO_TYPE (ffo)));
	      FRAMEINFO_TYPE (ffi) = FRAMEINFO_TYPE (ffo);
	      break;
	    }

	  /* Stop looking if no frame pointer for this function.  */
	  if (ff->vthis == NULL)
	    break;

	  AggregateDeclaration *ad = ff->isThis ();
	  if (ad && ad->isNested ())
	    {
	      while (ad->isNested ())
		{
		  Dsymbol *d = ad->toParent2 ();
		  ad = d->isAggregateDeclaration ();
		  ff = d->isFuncDeclaration ();

		  if (ad == NULL)
		    break;
		}
	    }
	  else
	    ff = ff->toParent2 ()->isFuncDeclaration ();
	}
    }

  /* Build type now as may be referenced from another module.  */
  if (FRAMEINFO_CREATES_FRAME (ffi))
    FRAMEINFO_TYPE (ffi) = build_frame_type (ffi, fd);

  return ffi;
}

/* Return a pointer to the frame/closure block of OUTER
   so can be accessed from the function INNER.  */

tree
get_framedecl (FuncDeclaration *inner, FuncDeclaration *outer)
{
  tree result = d_function_chain->static_chain;
  FuncDeclaration *fd = inner;

  while (fd && fd != outer)
    {
      /* Parent frame link is the first field.  */
      if (FRAMEINFO_CREATES_FRAME (get_frameinfo (fd)))
	result = indirect_ref (ptr_type_node, result);

      if (fd->isNested ())
	fd = fd->toParent2 ()->isFuncDeclaration ();
      /* The frame/closure record always points to the outer function's
	 frame, even if there are intervening nested classes or structs.
	 So, we can just skip over these.  */
      else
	fd = get_outer_function (fd->isThis ());
    }

  if (fd != outer)
    return error_no_frame_access (outer);

  /* Go get our frame record.  */
  tree frame_type = FRAMEINFO_TYPE (get_frameinfo (outer));

  if (frame_type != NULL_TREE)
    {
      result = build_nop (build_pointer_type (frame_type), result);
      return result;
    }
  else
    {
      error_at (make_location_t (inner->loc),
		"forward reference to frame of %qs", outer->toChars ());
      return null_pointer_node;
    }
}
