/* m2treelib.cc provides call trees, modify_expr and miscelaneous.

Copyright (C) 2012-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "gcc-consolidation.h"

#include "../gm2-lang.h"
#include "../m2-tree.h"

#define m2treelib_c
#include "m2assert.h"
#include "m2block.h"
#include "m2convert.h"
#include "m2decl.h"
#include "m2expr.h"
#include "m2statement.h"
#include "m2tree.h"
#include "m2treelib.h"
#include "m2treelib.h"
#include "m2type.h"

/* do_jump_if_bit - tests bit in word against integer zero using
   operator, code.  If the result is true then jump to label.  */

void
m2treelib_do_jump_if_bit (location_t location, enum tree_code code, tree word,
                          tree bit, char *label)
{
  word = m2convert_ToWord (location, word);
  bit = m2convert_ToWord (location, bit);
  m2statement_DoJump (
      location,
      m2expr_build_binary_op (
          location, code,
          m2expr_build_binary_op (
              location, BIT_AND_EXPR, word,
              m2expr_BuildLSL (location, m2expr_GetWordOne (location), bit,
                               FALSE),
              FALSE),
          m2expr_GetWordZero (location), FALSE),
      NULL, label);
}

/* build_modify_expr - taken from c-typeck.cc and heavily pruned.

   Build an assignment expression of lvalue LHS from value RHS.  If
   LHS_ORIGTYPE is not NULL, it is the original type of LHS, which
   may differ from TREE_TYPE (LHS) for an enum bitfield.  MODIFYCODE
   is the code for a binary operator that we use to combine the old
   value of LHS with RHS to get the new value.  Or else MODIFYCODE is
   NOP_EXPR meaning do a simple assignment.  If RHS_ORIGTYPE is not
   NULL_TREE, it is the original type of RHS, which may differ from
   TREE_TYPE (RHS) for an enum value.

   LOCATION is the location of the MODIFYCODE operator.  RHS_LOC is the
   location of the RHS.  */

static tree
build_modify_expr (location_t location, tree lhs, enum tree_code modifycode,
                   tree rhs)
{
  tree result;
  tree newrhs;
  tree rhs_semantic_type = NULL_TREE;
  tree lhstype = TREE_TYPE (lhs);
  tree olhstype = lhstype;

  ASSERT_CONDITION (modifycode == NOP_EXPR);

  if (TREE_CODE (rhs) == EXCESS_PRECISION_EXPR)
    {
      rhs_semantic_type = TREE_TYPE (rhs);
      rhs = TREE_OPERAND (rhs, 0);
    }

  newrhs = rhs;

  /* If storing into a structure or union member, it has probably been
     given type `int'.  Compute the type that would go with the actual
     amount of storage the member occupies.  */

  if (TREE_CODE (lhs) == COMPONENT_REF
      && (TREE_CODE (lhstype) == INTEGER_TYPE
          || TREE_CODE (lhstype) == BOOLEAN_TYPE
	  || SCALAR_FLOAT_TYPE_P (lhstype)
          || TREE_CODE (lhstype) == ENUMERAL_TYPE))
    lhstype = TREE_TYPE (get_unwidened (lhs, 0));

  /* If storing in a field that is in actuality a short or narrower
     than one, we must store in the field in its actual type.  */

  if (lhstype != TREE_TYPE (lhs))
    {
      lhs = copy_node (lhs);
      TREE_TYPE (lhs) = lhstype;
    }

  newrhs = fold (newrhs);

  if (rhs_semantic_type)
    newrhs = build1 (EXCESS_PRECISION_EXPR, rhs_semantic_type, newrhs);

  /* Scan operands.  */

  result = build2 (MODIFY_EXPR, lhstype, lhs, newrhs);
  TREE_SIDE_EFFECTS (result) = 1;
  protected_set_expr_location (result, location);

  /* If we got the LHS in a different type for storing in, convert the
     result back to the nominal type of LHS so that the value we return
     always has the same type as the LHS argument.  */

  ASSERT_CONDITION (olhstype == TREE_TYPE (result));
  /* In Modula-2 I'm assuming this will be true this maybe wrong, but
     at least I'll know about it soon.  If true then we do not need to
     implement convert_for_assignment - which is a huge win.  */

  return result;
}

/* m2treelib_build_modify_expr - wrapper function for
   build_modify_expr.  */

tree
m2treelib_build_modify_expr (location_t location, tree des,
                             enum tree_code modifycode, tree copy)
{
  return build_modify_expr (location, des, modifycode, copy);
}

/* nCount - return the number of trees chained on, t.  */

static int
nCount (tree t)
{
  int i = 0;

  while (t != NULL)
    {
      i++;
      t = TREE_CHAIN (t);
    }
  return i;
}

/* DoCall - build a call tree arranging the parameter list as a
   vector.  */

tree
m2treelib_DoCall (location_t location, tree rettype, tree funcptr,
                  tree param_list)
{
  int n = nCount (param_list);
  tree *argarray = XALLOCAVEC (tree, n);
  tree l = param_list;
  int i;

  for (i = 0; i < n; i++)
    {
      argarray[i] = TREE_VALUE (l);
      l = TREE_CHAIN (l);
    }
  return build_call_array_loc (location, rettype, funcptr, n, argarray);
}

/* DoCall0 - build a call tree with no parameters.  */

tree
m2treelib_DoCall0 (location_t location, tree rettype, tree funcptr)
{
  tree *argarray = XALLOCAVEC (tree, 1);

  argarray[0] = NULL_TREE;
  return build_call_array_loc (location, rettype, funcptr, 0, argarray);
}

/* DoCall1 - build a call tree with 1 parameter.  */

tree
m2treelib_DoCall1 (location_t location, tree rettype, tree funcptr, tree arg0)
{
  tree *argarray = XALLOCAVEC (tree, 1);

  argarray[0] = arg0;
  return build_call_array_loc (location, rettype, funcptr, 1, argarray);
}

/* DoCall2 - build a call tree with 2 parameters.  */

tree
m2treelib_DoCall2 (location_t location, tree rettype, tree funcptr, tree arg0,
                   tree arg1)
{
  tree *argarray = XALLOCAVEC (tree, 2);

  argarray[0] = arg0;
  argarray[1] = arg1;
  return build_call_array_loc (location, rettype, funcptr, 2, argarray);
}

/* DoCall3 - build a call tree with 3 parameters.  */

tree
m2treelib_DoCall3 (location_t location, tree rettype, tree funcptr, tree arg0,
                   tree arg1, tree arg2)
{
  tree *argarray = XALLOCAVEC (tree, 3);

  argarray[0] = arg0;
  argarray[1] = arg1;
  argarray[2] = arg2;
  return build_call_array_loc (location, rettype, funcptr, 3, argarray);
}

/* get_rvalue - returns the rvalue of t.  The, type, is the object
   type to be copied upon indirection.  */

tree
m2treelib_get_rvalue (location_t location, tree t, tree type, bool is_lvalue)
{
  if (is_lvalue)
    return m2expr_BuildIndirect (location, t, type);
  else
    return t;
}

/* get_field_no - returns the field no for, op.  The, op, is either a
   constructor or a variable of type record.  If, op, is a
   constructor (a set constant in GNU Modula-2) then this function is
   essentially a no-op and it returns op.  Else we iterate over the
   field list and return the appropriate field number.  */

tree
m2treelib_get_field_no (tree type, tree op, bool is_const, unsigned int fieldNo)
{
  ASSERT_BOOL (is_const);
  if (is_const)
    return op;
  else
    {
      tree list = TYPE_FIELDS (type);
      while (fieldNo > 0 && list != NULL_TREE)
        {
          list = TREE_CHAIN (list);
          fieldNo--;
        }
      return list;
    }
}

/* get_set_value - returns the value indicated by, field, in the set.
   Either p->field or the constant(op.fieldNo) is returned.  */

tree
m2treelib_get_set_value (location_t location, tree p, tree field, bool is_const,
                         bool is_lvalue, tree op, unsigned int fieldNo)
{
  tree value;
  constructor_elt *ce;

  ASSERT_BOOL (is_const);
  ASSERT_BOOL (is_lvalue);
  if (is_const)
    {
      ASSERT_CONDITION (is_lvalue == FALSE);
      gcc_assert (!vec_safe_is_empty (CONSTRUCTOR_ELTS (op)));
      unsigned int size = vec_safe_length (CONSTRUCTOR_ELTS (op));
      if (size < fieldNo)
        internal_error ("field number exceeds definition of set");
      if (vec_safe_iterate (CONSTRUCTOR_ELTS (op), fieldNo, &ce))
        value = ce->value;
      else
        internal_error (
            "field number out of range trying to access set element");
    }
  else if (is_lvalue)
    {
      if (TREE_CODE (TREE_TYPE (p)) == POINTER_TYPE)
        value = m2expr_BuildComponentRef (
            location, m2expr_BuildIndirect (location, p, TREE_TYPE (p)),
            field);
      else
        {
          ASSERT_CONDITION (TREE_CODE (TREE_TYPE (p)) == REFERENCE_TYPE);
          value = m2expr_BuildComponentRef (location, p, field);
        }
    }
  else
    {
      tree type = TREE_TYPE (op);
      enum tree_code code = TREE_CODE (type);

      ASSERT_CONDITION (code == RECORD_TYPE
                        || (code == POINTER_TYPE
                            && (TREE_CODE (TREE_TYPE (type)) == RECORD_TYPE)));
      value = m2expr_BuildComponentRef (location, op, field);
    }
  value = m2convert_ToBitset (location, value);
  return value;
}

/* get_set_address - returns the address of op1.  */

tree
m2treelib_get_set_address (location_t location, tree op1, bool is_lvalue)
{
  if (is_lvalue)
    return op1;
  else
    return m2expr_BuildAddr (location, op1, FALSE);
}

/* get_set_field_lhs - returns the address of p->field.  */

tree
m2treelib_get_set_field_lhs (location_t location, tree p, tree field)
{
  return m2expr_BuildAddr (
      location, m2convert_ToBitset (
                    location, m2expr_BuildComponentRef (location, p, field)),
      FALSE);
}

/* get_set_field_rhs - returns the value of p->field.  */

tree
m2treelib_get_set_field_rhs (location_t location, tree p, tree field)
{
  return m2convert_ToBitset (location,
                             m2expr_BuildComponentRef (location, p, field));
}

/* get_set_field_des - returns the p->field ready to be a (rhs)
   designator.  */

tree
m2treelib_get_set_field_des (location_t location, tree p, tree field)
{
  return m2expr_BuildIndirect (
      location,
      m2expr_BuildAddr (location,
                        m2expr_BuildComponentRef (location, p, field), FALSE),
      m2type_GetBitsetType ());
}

/* get_set_address_if_var - returns the address of, op, providing it
   is not a constant.  NULL is returned if, op, is a constant.  */

tree
m2treelib_get_set_address_if_var (location_t location, tree op, bool is_lvalue,
                                  bool is_const)
{
  if (is_const)
    return NULL;
  else
    return m2treelib_get_set_address (location, op, is_lvalue);
}

/* add_stmt add stmt to the statement-tree.  */

tree
add_stmt (location_t location, tree stmt)
{
  return m2block_add_stmt (location, stmt);
}

/* taken from gcc/c-semantics.cc.  */

/* Build a generic statement based on the given type of node and
   arguments.  Similar to `build_nt', except that we set EXPR_LOCATION
   to LOC.  */

tree
build_stmt (location_t loc, enum tree_code code, ...)
{
  tree ret;
  int length, i;
  va_list p;
  bool side_effects;

  m2assert_AssertLocation (loc);
  /* This function cannot be used to construct variably-sized nodes.  */
  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  va_start (p, code);

  ret = make_node (code);
  TREE_TYPE (ret) = void_type_node;
  length = TREE_CODE_LENGTH (code);
  SET_EXPR_LOCATION (ret, loc);

  /* TREE_SIDE_EFFECTS will already be set for statements with implicit
     side effects.  Here we make sure it is set for other expressions by
     checking whether the parameters have side effects.  */

  side_effects = false;
  for (i = 0; i < length; i++)
    {
      tree t = va_arg (p, tree);
      if (t && !TYPE_P (t))
        side_effects |= TREE_SIDE_EFFECTS (t);
      TREE_OPERAND (ret, i) = t;
    }

  TREE_SIDE_EFFECTS (ret) |= side_effects;

  va_end (p);
  return ret;
}
