/* m2expr.cc provides an interface to GCC expression trees.

Copyright (C) 2012-2023 Free Software Foundation, Inc.
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
#include "m2convert.h"

/* Prototypes.  */

#define m2expr_c
#include "m2assert.h"
#include "m2builtins.h"
#include "m2convert.h"
#include "m2decl.h"
#include "m2expr.h"
#include "m2options.h"
#include "m2range.h"
#include "m2statement.h"
#include "m2tree.h"
#include "m2treelib.h"
#include "m2type.h"
#include "m2linemap.h"

static void m2expr_checkRealOverflow (location_t location, enum tree_code code,
                                      tree result);
static tree checkWholeNegateOverflow (location_t location, tree i, tree lowest,
                                      tree min, tree max);
// static tree m2expr_Build4LogicalAnd (location_t location, tree a, tree b,
// tree c, tree d);
static tree m2expr_Build4LogicalOr (location_t location, tree a, tree b,
                                    tree c, tree d);
static tree m2expr_Build4TruthOrIf (location_t location, tree a, tree b,
                                    tree c, tree d);
static tree m2expr_Build4TruthAndIf (location_t location, tree a, tree b,
                                     tree c, tree d);

static int label_count = 0;
static GTY (()) tree set_full_complement;

/* CompareTrees returns -1 if e1 < e2, 0 if e1 == e2, and 1 if e1 > e2.  */

int
m2expr_CompareTrees (tree e1, tree e2)
{
  return tree_int_cst_compare (m2expr_FoldAndStrip (e1),
                               m2expr_FoldAndStrip (e2));
}

/* FoldAndStrip return expression, t, after it has been folded (if
   possible).  */

tree
m2expr_FoldAndStrip (tree t)
{
  if (t != NULL)
    {
      t = fold (t);
      if (TREE_CODE (t) == CONST_DECL)
        return m2expr_FoldAndStrip (DECL_INITIAL (t));
    }

  return t;
}

/* StringLength returns an unsigned int which is the length of, string.  */

unsigned int
m2expr_StringLength (tree string)
{
  return TREE_STRING_LENGTH (string);
}

/* CheckAddressToCardinal if op is a pointer convert it to the ADDRESS type.  */

static tree
CheckAddressToCardinal (location_t location, tree op)
{
  if (m2type_IsAddress (TREE_TYPE (op)))
    return m2convert_BuildConvert (location, m2type_GetCardinalAddressType (),
                                   op, false);
  return op;
}

/* BuildTruthAndIf return true if a && b.  Retain order left to right.  */

static tree
m2expr_BuildTruthAndIf (location_t location, tree a, tree b)
{
  return m2expr_build_binary_op (location, TRUTH_ANDIF_EXPR, a, b, false);
}

/* BuildTruthOrIf return true if a || b.  Retain order left to right.  */

static tree
m2expr_BuildTruthOrIf (location_t location, tree a, tree b)
{
  return m2expr_build_binary_op (location, TRUTH_ORIF_EXPR, a, b, false);
}

/* BuildTruthNotIf inverts the boolean value of expr and returns the result.  */

static tree
m2expr_BuildTruthNot (location_t location, tree expr)
{
  return m2expr_build_unary_op (location, TRUTH_NOT_EXPR, expr, false);
}

/* BuildPostInc builds a post increment tree, the second operand is
   always one.  */

static tree
m2expr_BuildPostInc (location_t location, tree op)
{
  return m2expr_BuildAdd (location, op, build_int_cst (TREE_TYPE (op), 1), false);
}

/* BuildPostDec builds a post decrement tree, the second operand is
   always one.  */

static tree
m2expr_BuildPostDec (location_t location, tree op)
{
  return m2expr_BuildSub (location, op, build_int_cst (TREE_TYPE (op), 1), false);
}

/* BuildAddCheck builds an addition tree.  */

tree
m2expr_BuildAddCheck (location_t location, tree op1, tree op2, tree lowest,
                      tree min, tree max)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op_check (location, PLUS_EXPR, op1, op2, false,
                                    lowest, min, max);
  return m2expr_FoldAndStrip (t);
}

/* BuildAdd builds an addition tree.  */

tree
m2expr_BuildAdd (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, PLUS_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildSubCheck builds a subtraction tree.  */

tree
m2expr_BuildSubCheck (location_t location, tree op1, tree op2, tree lowest,
                      tree min, tree max)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op_check (location, MINUS_EXPR, op1, op2, false,
                                    lowest, min, max);
  return m2expr_FoldAndStrip (t);
}

/* BuildSub builds a subtraction tree.  */

tree
m2expr_BuildSub (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, MINUS_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildDivTrunc builds a trunc division tree.  */

tree
m2expr_BuildDivTrunc (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, TRUNC_DIV_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildDivTruncCheck builds a trunc division tree.  */

tree
m2expr_BuildDivTruncCheck (location_t location, tree op1, tree op2, tree lowest,
			   tree min, tree max)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op_check (location, TRUNC_DIV_EXPR, op1, op2, false,
				    lowest, min, max);
  return m2expr_FoldAndStrip (t);
}

/* BuildModTruncCheck builds a trunc modulus tree.  */

tree
m2expr_BuildModTruncCheck (location_t location, tree op1, tree op2, tree lowest,
			   tree min, tree max)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op_check (location, TRUNC_MOD_EXPR, op1, op2, false,
				    lowest, min, max);
  return m2expr_FoldAndStrip (t);
}

/* BuildModTrunc builds a trunc modulus tree.  */

tree
m2expr_BuildModTrunc (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, TRUNC_MOD_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildModCeilCheck builds a ceil modulus tree.  */

tree
m2expr_BuildModCeilCheck (location_t location, tree op1, tree op2, tree lowest,
			  tree min, tree max)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op_check (location, CEIL_MOD_EXPR, op1, op2, false,
				    lowest, min, max);
  return m2expr_FoldAndStrip (t);
}

/* BuildModFloorCheck builds a trunc modulus tree.  */

tree
m2expr_BuildModFloorCheck (location_t location, tree op1, tree op2, tree lowest,
			   tree min, tree max)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op_check (location, FLOOR_MOD_EXPR, op1, op2, false,
				    lowest, min, max);
  return m2expr_FoldAndStrip (t);
}

/* BuildDivCeil builds a ceil division tree.  */

tree
m2expr_BuildDivCeil (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, CEIL_DIV_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildDivCeilCheck builds a check ceil division tree.  */

tree
m2expr_BuildDivCeilCheck (location_t location, tree op1, tree op2, tree lowest,
			  tree min, tree max)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op_check (location, CEIL_DIV_EXPR, op1, op2, false,
				    lowest, min, max);
  return m2expr_FoldAndStrip (t);
}

/* BuildModCeil builds a ceil modulus tree.  */

tree
m2expr_BuildModCeil (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, CEIL_MOD_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildDivFloor builds a floor division tree.  */

tree
m2expr_BuildDivFloor (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, FLOOR_DIV_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildDivFloorCheck builds a check floor division tree.  */

tree
m2expr_BuildDivFloorCheck (location_t location, tree op1, tree op2, tree lowest,
			   tree min, tree max)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op_check (location, FLOOR_DIV_EXPR, op1, op2, false,
				    lowest, min, max);
  return m2expr_FoldAndStrip (t);
}

/* BuildRDiv builds a division tree (this should only be used for
   REAL and COMPLEX types and NEVER for integer based types).  */

tree
m2expr_BuildRDiv (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  t = m2expr_build_binary_op (location, RDIV_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildModFloor builds a modulus tree.  */

tree
m2expr_BuildModFloor (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op (location, FLOOR_MOD_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildLSL builds and returns tree (op1 << op2).  */

tree
m2expr_BuildLSL (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  t = m2expr_build_binary_op (location, LSHIFT_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildLSR builds and returns tree (op1 >> op2).  */

tree
m2expr_BuildLSR (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  t = m2expr_build_binary_op (location, RSHIFT_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* createUniqueLabel returns a unique label which has been alloc'ed.  */

static char *
createUniqueLabel (void)
{
  int size, i;
  char *label;

  label_count++;
  i = label_count;
  size = strlen (".LSHIFT") + 2;
  while (i > 0)
    {
      i /= 10;
      size++;
    }
  label = (char *)ggc_alloc_atomic (size);
  sprintf (label, ".LSHIFT%d", label_count);
  return label;
}

/* BuildLogicalShift builds the ISO Modula-2 SHIFT operator for a
   fundamental data type.  */

void
m2expr_BuildLogicalShift (location_t location, tree op1, tree op2, tree op3,
                          tree nBits ATTRIBUTE_UNUSED, bool needconvert)
{
  tree res;

  m2assert_AssertLocation (location);
  op2 = m2expr_FoldAndStrip (op2);
  op3 = m2expr_FoldAndStrip (op3);
  if (TREE_CODE (op3) == INTEGER_CST)
    {
      op2 = m2convert_ToWord (location, op2);
      if (tree_int_cst_sgn (op3) < 0)
        res = m2expr_BuildLSR (
            location, op2,
            m2convert_ToWord (location,
                              m2expr_BuildNegate (location, op3, needconvert)),
            needconvert);
      else
        res = m2expr_BuildLSL (location, op2, m2convert_ToWord (location, op3),
                               needconvert);
      res = m2convert_BuildConvert (
          location, m2tree_skip_type_decl (TREE_TYPE (op1)), res, false);
      m2statement_BuildAssignmentTree (location, op1, res);
    }
  else
    {
      char *labelElseName = createUniqueLabel ();
      char *labelEndName = createUniqueLabel ();
      tree is_less = m2expr_BuildLessThan (location,
                                           m2convert_ToInteger (location, op3),
                                           m2expr_GetIntegerZero (location));

      m2statement_DoJump (location, is_less, NULL, labelElseName);
      op2 = m2convert_ToWord (location, op2);
      op3 = m2convert_ToWord (location, op3);
      res = m2expr_BuildLSL (location, op2, op3, needconvert);
      res = m2convert_BuildConvert (
          location, m2tree_skip_type_decl (TREE_TYPE (op1)), res, false);
      m2statement_BuildAssignmentTree (location, op1, res);
      m2statement_BuildGoto (location, labelEndName);
      m2statement_DeclareLabel (location, labelElseName);
      res = m2expr_BuildLSR (location, op2,
                             m2expr_BuildNegate (location, op3, needconvert),
                             needconvert);
      res = m2convert_BuildConvert (
          location, m2tree_skip_type_decl (TREE_TYPE (op1)), res, false);
      m2statement_BuildAssignmentTree (location, op1, res);
      m2statement_DeclareLabel (location, labelEndName);
    }
}

/* BuildLRL builds and returns tree (op1 rotate left by op2 bits).  */

tree
m2expr_BuildLRL (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  t = m2expr_build_binary_op (location, LROTATE_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildLRR builds and returns tree (op1 rotate right by op2 bits).  */

tree
m2expr_BuildLRR (location_t location, tree op1, tree op2, bool needconvert)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  t = m2expr_build_binary_op (location, RROTATE_EXPR, op1, op2, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* m2expr_BuildMask returns a tree for the mask of a set of nBits.
   It assumes nBits is <= TSIZE (WORD).  */

tree
m2expr_BuildMask (location_t location, tree nBits, bool needconvert)
{
  tree mask = m2expr_BuildLSL (location, m2expr_GetIntegerOne (location),
                               nBits, needconvert);
  m2assert_AssertLocation (location);
  return m2expr_BuildSub (location, mask, m2expr_GetIntegerOne (location),
                          needconvert);
}

/* m2expr_BuildLRotate returns a tree in which op1 has been left
   rotated by nBits.  It assumes nBits is <= TSIZE (WORD).  */

tree
m2expr_BuildLRotate (location_t location, tree op1, tree nBits,
                     bool needconvert)
{
  tree t;

  op1 = m2expr_FoldAndStrip (op1);
  nBits = m2expr_FoldAndStrip (nBits);
  t = m2expr_build_binary_op (location, LROTATE_EXPR, op1, nBits, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* m2expr_BuildRRotate returns a tree in which op1 has been left
   rotated by nBits.  It assumes nBits is <= TSIZE (WORD).  */

tree
m2expr_BuildRRotate (location_t location, tree op1, tree nBits,
                     bool needconvert)
{
  tree t;

  op1 = m2expr_FoldAndStrip (op1);
  nBits = m2expr_FoldAndStrip (nBits);
  t = m2expr_build_binary_op (location, RROTATE_EXPR, op1, nBits, needconvert);
  return m2expr_FoldAndStrip (t);
}

/* BuildLRLn builds and returns tree (op1 rotate left by op2 bits) it
   rotates a set of size, nBits.  */

tree
m2expr_BuildLRLn (location_t location, tree op1, tree op2, tree nBits,
                  bool needconvert)
{
  tree op2min;

  m2assert_AssertLocation (location);

  /* Ensure we wrap the rotate.  */

  op2min = m2expr_BuildModTrunc (
      location, m2convert_ToCardinal (location, op2),
      m2convert_ToCardinal (location, nBits), needconvert);

  /* Optimize if we are we going to rotate a TSIZE(BITSET) set.  */

  if (m2expr_CompareTrees (
          m2decl_BuildIntegerConstant (m2decl_GetBitsPerBitset ()), nBits)
      == 0)
    return m2expr_BuildLRotate (location, op1, op2min, needconvert);
  else
    {
      tree mask = m2expr_BuildMask (location, nBits, needconvert);
      tree left, right;

      /* Make absolutely sure there are no high order bits lying around.  */

      op1 = m2expr_BuildLogicalAnd (location, op1, mask, needconvert);
      left = m2expr_BuildLSL (location, op1, op2min, needconvert);
      left = m2expr_BuildLogicalAnd (location, left, mask, needconvert);
      right = m2expr_BuildLSR (
          location, op1,
          m2expr_BuildSub (location, m2convert_ToCardinal (location, nBits),
                           op2min, needconvert),
          needconvert);
      return m2expr_BuildLogicalOr (location, left, right, needconvert);
    }
}

/* BuildLRRn builds and returns tree (op1 rotate right by op2 bits).
   It rotates a set of size, nBits.  */

tree
m2expr_BuildLRRn (location_t location, tree op1, tree op2, tree nBits,
                  bool needconvert)
{
  tree op2min;

  m2assert_AssertLocation (location);

  /* Ensure we wrap the rotate.  */

  op2min = m2expr_BuildModTrunc (
      location, m2convert_ToCardinal (location, op2),
      m2convert_ToCardinal (location, nBits), needconvert);
  /* Optimize if we are we going to rotate a TSIZE(BITSET) set.  */

  if (m2expr_CompareTrees (
          m2decl_BuildIntegerConstant (m2decl_GetBitsPerBitset ()), nBits)
      == 0)
    return m2expr_BuildRRotate (location, op1, op2min, needconvert);
  else
    {
      tree mask = m2expr_BuildMask (location, nBits, needconvert);
      tree left, right;

      /* Make absolutely sure there are no high order bits lying around.  */

      op1 = m2expr_BuildLogicalAnd (location, op1, mask, needconvert);
      right = m2expr_BuildLSR (location, op1, op2min, needconvert);
      left = m2expr_BuildLSL (
          location, op1,
          m2expr_BuildSub (location, m2convert_ToCardinal (location, nBits),
                           op2min, needconvert),
          needconvert);
      left = m2expr_BuildLogicalAnd (location, left, mask, needconvert);
      return m2expr_BuildLogicalOr (location, left, right, needconvert);
    }
}

/* BuildLogicalRotate build the ISO Modula-2 ROTATE operator for a
   fundamental data type.  */

void
m2expr_BuildLogicalRotate (location_t location, tree op1, tree op2, tree op3,
                           tree nBits, bool needconvert)
{
  tree res;

  m2assert_AssertLocation (location);
  op2 = m2expr_FoldAndStrip (op2);
  op3 = m2expr_FoldAndStrip (op3);
  if (TREE_CODE (op3) == INTEGER_CST)
    {
      if (tree_int_cst_sgn (op3) < 0)
        res = m2expr_BuildLRRn (
            location, op2, m2expr_BuildNegate (location, op3, needconvert),
            nBits, needconvert);
      else
        res = m2expr_BuildLRLn (location, op2, op3, nBits, needconvert);
      m2statement_BuildAssignmentTree (location, op1, res);
    }
  else
    {
      char *labelElseName = createUniqueLabel ();
      char *labelEndName = createUniqueLabel ();
      tree is_less = m2expr_BuildLessThan (location,
                                           m2convert_ToInteger (location, op3),
                                           m2expr_GetIntegerZero (location));

      m2statement_DoJump (location, is_less, NULL, labelElseName);
      res = m2expr_BuildLRLn (location, op2, op3, nBits, needconvert);
      m2statement_BuildAssignmentTree (location, op1, res);
      m2statement_BuildGoto (location, labelEndName);
      m2statement_DeclareLabel (location, labelElseName);
      res = m2expr_BuildLRRn (location, op2,
                              m2expr_BuildNegate (location, op3, needconvert),
                              nBits, needconvert);
      m2statement_BuildAssignmentTree (location, op1, res);
      m2statement_DeclareLabel (location, labelEndName);
    }
}

/* buildUnboundedArrayOf construct an unbounded struct and returns
   the gcc tree.  The two fields of the structure are initialized to
   contentsPtr and high.  */

static tree
buildUnboundedArrayOf (tree unbounded, tree contentsPtr, tree high)
{
  tree fields = TYPE_FIELDS (unbounded);
  tree field_list = NULL_TREE;
  tree constructor;

  field_list = tree_cons (fields, contentsPtr, field_list);
  fields = TREE_CHAIN (fields);

  field_list = tree_cons (fields, high, field_list);

  constructor = build_constructor_from_list (unbounded, nreverse (field_list));
  TREE_CONSTANT (constructor) = 0;
  TREE_STATIC (constructor) = 0;

  return constructor;
}

/* BuildBinarySetDo if the size of the set is <= TSIZE(WORD) then op1
   := binop(op2, op3) else call m2rtsprocedure(op1, op2, op3).  */

void
m2expr_BuildBinarySetDo (location_t location, tree settype, tree op1, tree op2,
                         tree op3, void (*binop) (location_t, tree, tree, tree,
                                                  tree, bool),
                         bool is_op1lvalue, bool is_op2lvalue, bool is_op3lvalue,
                         tree nBits, tree unbounded, tree varproc,
                         tree leftproc, tree rightproc)
{
  tree size = m2expr_GetSizeOf (location, settype);
  bool is_const = false;
  bool is_left = false;

  m2assert_AssertLocation (location);

  ASSERT_BOOL (is_op1lvalue);
  ASSERT_BOOL (is_op2lvalue);
  ASSERT_BOOL (is_op3lvalue);

  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    /* Small set size <= TSIZE(WORD).  */
    (*binop) (location,
              m2treelib_get_rvalue (location, op1, settype, is_op1lvalue),
              m2treelib_get_rvalue (location, op2, settype, is_op2lvalue),
              m2treelib_get_rvalue (location, op3, settype, is_op3lvalue),
              nBits, false);
  else
    {
      tree result;
      tree high = m2expr_BuildSub (
          location,
          m2convert_ToCardinal (
              location,
              m2expr_BuildDivTrunc (
                  location, size,
                  m2expr_GetSizeOf (location, m2type_GetBitsetType ()),
                  false)),
          m2expr_GetCardinalOne (location), false);

      /* If op3 is constant then make op3 positive and remember which
      direction we are shifting.  */

      op3 = m2tree_skip_const_decl (op3);
      if (TREE_CODE (op3) == INTEGER_CST)
        {
          is_const = true;
          if (tree_int_cst_sgn (op3) < 0)
            op3 = m2expr_BuildNegate (location, op3, false);
          else
            is_left = true;
          op3 = m2convert_BuildConvert (location, m2type_GetM2CardinalType (),
                                        op3, false);
        }

      /* These parameters must match the prototypes of the procedures:
	 ShiftLeft, ShiftRight, ShiftVal, RotateLeft, RotateRight, RotateVal
	 inside gm2-iso/SYSTEM.mod.  */

      /* Remember we must build the parameters in reverse.  */

      /* Parameter 4 amount.  */
      m2statement_BuildParam (
          location,
          m2convert_BuildConvert (
              location, m2type_GetM2IntegerType (),
              m2treelib_get_rvalue (location, op3,
                                    m2tree_skip_type_decl (TREE_TYPE (op3)),
                                    is_op3lvalue),
              false));

      /* Parameter 3 nBits.  */
      m2statement_BuildParam (
          location,
          m2convert_BuildConvert (location, m2type_GetM2CardinalType (),
                                  m2expr_FoldAndStrip (nBits), false));

      /* Parameter 2 destination set.  */
      m2statement_BuildParam (
          location,
          buildUnboundedArrayOf (
              unbounded,
              m2treelib_get_set_address (location, op1, is_op1lvalue), high));

      /* Parameter 1 source set.  */
      m2statement_BuildParam (
          location,
          buildUnboundedArrayOf (
              unbounded,
              m2treelib_get_set_address (location, op2, is_op2lvalue), high));

      /* Now call the appropriate procedure inside SYSTEM.mod.  */
      if (is_const)
        if (is_left)
          result = m2statement_BuildProcedureCallTree (location, leftproc,
                                                       NULL_TREE);
        else
          result = m2statement_BuildProcedureCallTree (location, rightproc,
                                                       NULL_TREE);
      else
        result = m2statement_BuildProcedureCallTree (location, varproc,
                                                     NULL_TREE);
      add_stmt (location, result);
    }
}

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language requires
   to be a constant expression.  */

void
m2expr_ConstantExpressionWarning (tree value)
{
  if ((TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
       || TREE_CODE (value) == FIXED_CST || TREE_CODE (value) == VECTOR_CST
       || TREE_CODE (value) == COMPLEX_CST)
      && TREE_OVERFLOW (value))
    pedwarn (input_location, OPT_Woverflow, "overflow in constant expression");
}

/* TreeOverflow return true if the contant expression, t, has caused
   an overflow.  No error message or warning is emitted and no
   modification is made to, t.  */

bool
m2expr_TreeOverflow (tree t)
{
  if ((TREE_CODE (t) == INTEGER_CST
       || (TREE_CODE (t) == COMPLEX_CST
           && TREE_CODE (TREE_REALPART (t)) == INTEGER_CST))
      && TREE_OVERFLOW (t))
    return true;
  else if ((TREE_CODE (t) == REAL_CST
            || (TREE_CODE (t) == COMPLEX_CST
                && TREE_CODE (TREE_REALPART (t)) == REAL_CST))
           && TREE_OVERFLOW (t))
    return true;
  else
    return false;
}

/* RemoveOverflow if tree, t, is a constant expression it removes any
   overflow flag and returns, t.  */

tree
m2expr_RemoveOverflow (tree t)
{
  if (TREE_CODE (t) == INTEGER_CST
      || (TREE_CODE (t) == COMPLEX_CST
          && TREE_CODE (TREE_REALPART (t)) == INTEGER_CST))
    TREE_OVERFLOW (t) = 0;
  else if (TREE_CODE (t) == REAL_CST
           || (TREE_CODE (t) == COMPLEX_CST
               && TREE_CODE (TREE_REALPART (t)) == REAL_CST))
    TREE_OVERFLOW (t) = 0;
  return t;
}

/* BuildCoerce return a tree containing the expression, expr, after
   it has been coersed to, type.  */

tree
m2expr_BuildCoerce (location_t location, tree des, tree type, tree expr)
{
  tree copy = copy_node (expr);
  TREE_TYPE (copy) = type;

  m2assert_AssertLocation (location);

  return m2treelib_build_modify_expr (location, des, NOP_EXPR, copy);
}

/* BuildTrunc return an integer expression from a REAL or LONGREAL op1.  */

tree
m2expr_BuildTrunc (tree op1)
{
  return convert_to_integer (m2type_GetIntegerType (),
                             m2expr_FoldAndStrip (op1));
}

/* checkUnaryWholeOverflow decide if we can check this unary expression.  */

tree
m2expr_checkUnaryWholeOverflow (location_t location, enum tree_code code,
                                tree arg, tree lowest, tree min, tree max)
{
  if (M2Options_GetWholeValueCheck () && (min != NULL))
    {
      lowest = m2tree_skip_type_decl (lowest);
      arg = fold_convert_loc (location, lowest, arg);

      switch (code)
        {
        case NEGATE_EXPR:
          return checkWholeNegateOverflow (location, arg, lowest, min, max);
        default:
	  return NULL;
        }
    }
  return NULL;
}

/* build_unary_op return a unary tree node.  */

tree
m2expr_build_unary_op_check (location_t location, enum tree_code code,
                             tree arg, tree lowest, tree min, tree max)
{
  tree argtype = TREE_TYPE (arg);
  tree result;
  tree check = NULL;

  m2assert_AssertLocation (location);

  arg = m2expr_FoldAndStrip (arg);

  if ((TREE_CODE (argtype) != REAL_TYPE) && (min != NULL))
    check = m2expr_checkUnaryWholeOverflow (location, code, arg, lowest, min, max);

  result = build1 (code, argtype, arg);
  protected_set_expr_location (result, location);

  if (check != NULL)
    result = build2 (COMPOUND_EXPR, argtype, check, result);

  if (TREE_CODE (argtype) == REAL_TYPE)
    m2expr_checkRealOverflow (location, code, result);

  return m2expr_FoldAndStrip (result);
}

/* build_unary_op return a unary tree node.  */

tree
m2expr_build_unary_op (location_t location, enum tree_code code, tree arg,
                       int flag ATTRIBUTE_UNUSED)
{
  tree argtype = TREE_TYPE (arg);
  tree result;

  m2assert_AssertLocation (location);

  arg = m2expr_FoldAndStrip (arg);
  result = build1 (code, argtype, arg);
  protected_set_expr_location (result, location);

  return m2expr_FoldAndStrip (result);
}

/* build_binary_op is a heavily pruned version of the one found in
   c-typeck.cc.  The Modula-2 expression rules are much more restricted
   than C.  */

tree
build_binary_op (location_t location, enum tree_code code, tree op1, tree op2,
                 int convert ATTRIBUTE_UNUSED)
{
  tree type1 = TREE_TYPE (op1);
  tree result;

  m2assert_AssertLocation (location);

  /* Strip NON_LVALUE_EXPRs, etc., since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (op1);
  STRIP_TYPE_NOPS (op2);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  result = build2 (code, type1, op1, op2);
  protected_set_expr_location (result, location);

  return m2expr_FoldAndStrip (result);
}

/* BuildLessThanZero - returns a tree containing (< value 0).  It
   checks the min and max value to ensure that the test can be safely
   achieved and will short circuit the result otherwise.  */

tree
m2expr_BuildLessThanZero (location_t location, tree value, tree type, tree min,
                          tree max)
{
  if (m2expr_CompareTrees (min, m2expr_GetIntegerZero (location)) >= 0)
    /* min is greater than or equal to zero therefore value will always
       be >= 0.  */
    return m2type_GetBooleanFalse ();
  else if (m2expr_CompareTrees (max, m2expr_GetIntegerZero (location)) == -1)
    /* max is less than zero therefore value will always be < 0.  */
    return m2type_GetBooleanTrue ();
  /* We now know 0 lies in the range min..max so we can safely cast
     zero to type.  */
  return m2expr_BuildLessThan (
      location, value,
      fold_convert_loc (location, type, m2expr_GetIntegerZero (location)));
}

/* BuildGreaterThanZero - returns a tree containing (> value 0).  It
   checks the min and max value to ensure that the test can be safely
   achieved and will short circuit the result otherwise.  */

tree
m2expr_BuildGreaterThanZero (location_t location, tree value, tree type,
                             tree min, tree max)
{
  if (m2expr_CompareTrees (min, m2expr_GetIntegerZero (location)) == 1)
    /* min is greater than zero therefore value will always be > 0.  */
    return m2type_GetBooleanTrue ();
  else if (m2expr_CompareTrees (max, m2expr_GetIntegerZero (location)) <= 0)
    /* max is less than or equal to zero therefore value will always be
       <= 0.  */
    return m2type_GetBooleanFalse ();
  /* We now know 0 lies in the range min..max so we can safely cast
     zero to type.  */
  return m2expr_BuildGreaterThan (
      location, value,
      fold_convert_loc (location, type, m2expr_GetIntegerZero (location)));
}

/* BuildEqualToZero - returns a tree containing (= value 0).  It
   checks the min and max value to ensure that the test can be safely
   achieved and will short circuit the result otherwise.  */

tree
m2expr_BuildEqualToZero (location_t location, tree value, tree type, tree min,
                         tree max)
{
  if (m2expr_CompareTrees (min, m2expr_GetIntegerZero (location)) == 1)
    /* min is greater than zero therefore value will always be > 0.  */
    return m2type_GetBooleanFalse ();
  else if (m2expr_CompareTrees (max, m2expr_GetIntegerZero (location)) < 0)
    /* max is less than or equal to zero therefore value will always be <
       0.  */
    return m2type_GetBooleanFalse ();
  /* We now know 0 lies in the range min..max so we can safely cast
     zero to type.  */
  return m2expr_BuildEqualTo (
      location, value,
      fold_convert_loc (location, type, m2expr_GetIntegerZero (location)));
}

/* BuildNotEqualToZero - returns a tree containing (# value 0).  It
   checks the min and max value to ensure that the test can be safely
   achieved and will short circuit the result otherwise.  */

tree
m2expr_BuildNotEqualToZero (location_t location, tree value, tree type,
                            tree min, tree max)
{
  if (m2expr_CompareTrees (min, m2expr_GetIntegerZero (location)) == 1)
    /* min is greater than zero therefore value will always be true.  */
    return m2type_GetBooleanTrue ();
  else if (m2expr_CompareTrees (max, m2expr_GetIntegerZero (location)) < 0)
    /* max is less than or equal to zero therefore value will always be
       true.  */
    return m2type_GetBooleanTrue ();
  /* We now know 0 lies in the range min..max so we can safely cast
     zero to type.  */
  return m2expr_BuildNotEqualTo (
      location, value,
      fold_convert_loc (location, type, m2expr_GetIntegerZero (location)));
}


/* BuildGreaterThanOrEqualZero - returns a tree containing (>= value 0).  It
   checks the min and max value to ensure that the test can be safely
   achieved and will short circuit the result otherwise.  */

tree
m2expr_BuildGreaterThanOrEqualZero (location_t location, tree value, tree type,
				    tree min, tree max)
{
  if (m2expr_CompareTrees (min, m2expr_GetIntegerZero (location)) >= 0)
    /* min is greater than or equal to zero therefore value will always be >= 0.  */
    return m2type_GetBooleanTrue ();
  else if (m2expr_CompareTrees (max, m2expr_GetIntegerZero (location)) < 0)
    /* max is less than zero therefore value will always be < 0.  */
    return m2type_GetBooleanFalse ();
  /* We now know 0 lies in the range min..max so we can safely cast
     zero to type.  */
  return m2expr_BuildGreaterThan (
      location, value,
      fold_convert_loc (location, type, m2expr_GetIntegerZero (location)));
}


/* BuildLessThanOrEqualZero - returns a tree containing (<= value 0).  It
   checks the min and max value to ensure that the test can be safely
   achieved and will short circuit the result otherwise.  */

tree
m2expr_BuildLessThanOrEqualZero (location_t location, tree value, tree type,
				 tree min, tree max)
{
  if (m2expr_CompareTrees (min, m2expr_GetIntegerZero (location)) > 0)
    /* min is greater than zero therefore value will always be > 0.  */
    return m2type_GetBooleanFalse ();
  else if (m2expr_CompareTrees (max, m2expr_GetIntegerZero (location)) <= 0)
    /* max is less than or equal to zero therefore value will always be <= 0.  */
    return m2type_GetBooleanTrue ();
  /* We now know 0 lies in the range min..max so we can safely cast
     zero to type.  */
  return m2expr_BuildLessThanOrEqual (
      location, value,
      fold_convert_loc (location, type, m2expr_GetIntegerZero (location)));
}


/* get_current_function_name, return the name of the current function if
   it currently exists.  NULL is returned if we are not inside a function.  */

static const char *
get_current_function_name (void)
{
  if (current_function_decl != NULL
      && (DECL_NAME (current_function_decl) != NULL)
      && (IDENTIFIER_POINTER (DECL_NAME (current_function_decl)) != NULL))
    return IDENTIFIER_POINTER (DECL_NAME (current_function_decl));
  return NULL;
}

/* checkWholeNegateOverflow - check to see whether -arg will overflow
   an integer.

PROCEDURE sneg (i: INTEGER) ;
BEGIN
   IF i = MIN(INTEGER)
   THEN
      'integer overflow'
   END
END sneg ;

general purpose subrange type, i, is currently legal, min is
   MIN(type) and max is MAX(type).

PROCEDURE sneg (i: type) ;
BEGIN
   max := MAX (type) ;
   min := MIN (type) ;
   (* cannot overflow if i is 0 *)
   IF (i#0) AND
     (* will overflow if entire range is positive.  *)
     ((min >= 0) OR
      (* will overflow if entire range is negative.  *)
      (max <= 0) OR
      (* c7 and c8 and c9 and c10 -> c17 more units positive.  *)
      ((min < 0) AND (max > 0) AND ((min + max) > 0) AND (i > -min)) OR
      (* c11 and c12 and c13 and c14 -> c18 more units negative.  *)
      ((min < 0) AND (max > 0) AND ((min + max) < 0) AND (i < -max)))
   THEN
      'type overflow'
   END
END sneg ; */

static tree
checkWholeNegateOverflow (location_t location,
			  tree i, tree type, tree min,
                          tree max)
{
  tree a1
      = m2expr_BuildNotEqualToZero (location, i, type, min, max); /* i # 0.  */
  tree c1 = m2expr_BuildGreaterThanZero (location, min, type, min,
                                         max); /* min > 0.  */
  tree c2 = m2expr_BuildEqualToZero (location, min, type, min,
                                     max); /* min == 0.  */
  tree c4 = m2expr_BuildLessThanZero (location, max, type, min,
                                      max); /* max < 0.  */
  tree c5 = m2expr_BuildEqualToZero (location, max, type, min,
                                     max); /* max == 0.  */
  tree c7 = m2expr_BuildLessThanZero (location, min, type, min,
                                      max); /* min < 0.  */
  tree c8 = m2expr_BuildGreaterThanZero (location, max, type, min,
                                         max); /* max > 0.  */
  tree c9 = m2expr_BuildGreaterThanZero (
      location, m2expr_BuildAdd (location, min, max, false), type, min,
      max); /* min + max > 0.  */
  tree c10 = m2expr_BuildGreaterThan (
      location, i, m2expr_BuildNegate (location, min, false)); /* i > -min.  */
  tree c11 = m2expr_BuildLessThanZero (
      location, m2expr_BuildAdd (location, min, max, false), type, min,
      max); /* min + max < 0.  */
  tree c12 = m2expr_BuildLessThan (
      location, i, m2expr_BuildNegate (location, max, false)); /* i < -max.  */

  tree b1 = m2expr_BuildTruthOrIf (location, c1, c2);
  tree b2 = m2expr_BuildTruthOrIf (location, c8, c5);
  tree o1 = m2expr_BuildTruthAndIf (location, b1, b2);

  tree b3 = m2expr_BuildTruthOrIf (location, c7, c2);
  tree b4 = m2expr_BuildTruthOrIf (location, c4, c5);
  tree o2 = m2expr_BuildTruthAndIf (location, b3, b4);

  tree o3 = m2expr_Build4TruthAndIf (location, c7, c8, c9, c10);
  tree o4 = m2expr_Build4TruthAndIf (location, c7, c8, c11, c12);

  tree a2 = m2expr_Build4TruthOrIf (location, o1, o2, o3, o4);
  tree condition
      = m2expr_FoldAndStrip (m2expr_BuildTruthAndIf (location, a1, a2));

  tree t = M2Range_BuildIfCallWholeHandlerLoc (location, condition,
					       get_current_function_name (),
               "whole value unary minus will cause range overflow");
  return t;
}

/* checkWholeAddOverflow - check to see whether op1 + op2 will
   overflow an integer.

PROCEDURE sadd (i, j: INTEGER) ;
BEGIN
   IF ((j>0) AND (i > MAX(INTEGER)-j)) OR ((j<0) AND (i < MIN(INTEGER)-j))
   THEN
      'signed addition overflow'
   END
END sadd.  */

static tree
checkWholeAddOverflow (location_t location, tree i, tree j, tree lowest,
                       tree min, tree max)
{
  tree j_gt_zero = m2expr_BuildGreaterThanZero (location, j, lowest, min, max);
  tree i_gt_max_sub_j = m2expr_BuildGreaterThan (
      location, i, m2expr_BuildSub (location, max, j, false));
  tree j_lt_zero = m2expr_BuildLessThanZero (location, j, lowest, min, max);
  tree i_lt_min_sub_j = m2expr_BuildLessThan (location, i,
					      m2expr_BuildSub (location, min, j, false));
  tree lhs_or = m2expr_FoldAndStrip (m2expr_BuildTruthAndIf (location, j_gt_zero, i_gt_max_sub_j));
  tree rhs_or = m2expr_FoldAndStrip (m2expr_BuildTruthAndIf (location, j_lt_zero, i_lt_min_sub_j));
  tree condition
      = m2expr_FoldAndStrip (m2expr_BuildTruthOrIf (location, lhs_or, rhs_or));
  tree result = M2Range_BuildIfCallWholeHandlerLoc (location, condition,
					       get_current_function_name (),
               "whole value addition will cause a range overflow");
  return result;
}

/* checkWholeSubOverflow - check to see whether op1 - op2 will
   overflow an integer.

PROCEDURE ssub (i, j: INTEGER) ;
BEGIN
   IF ((j>0) AND (i < MIN(INTEGER)+j)) OR ((j<0) AND (i > MAX(INTEGER)+j))
   THEN
      'signed subtraction overflow'
   END
END ssub.  */

static tree
checkWholeSubOverflow (location_t location, tree i, tree j, tree lowest,
                       tree min, tree max)
{
  tree c1 = m2expr_BuildGreaterThanZero (location, j, lowest, min, max);
  tree c2 = m2expr_BuildLessThan (location, i,
                                  m2expr_BuildAdd (location, min, j, false));
  tree c3 = m2expr_BuildLessThanZero (location, j, lowest, min, max);
  tree c4 = m2expr_BuildGreaterThan (location, i,
				     m2expr_BuildAdd (location, max, j, false));
  tree c5 = m2expr_FoldAndStrip (m2expr_BuildTruthAndIf (location, c1, c2));
  tree c6 = m2expr_FoldAndStrip (m2expr_BuildTruthAndIf (location, c3, c4));
  tree condition
      = m2expr_FoldAndStrip (m2expr_BuildTruthOrIf (location, c5, c6));
  tree t = M2Range_BuildIfCallWholeHandlerLoc (location, condition,
					       get_current_function_name (),
               "whole value subtraction will cause a range overflow");
  return t;
}

/* Build4TruthAndIf - return true if a && b && c && d.  Retain order left to
 * right.  */

static tree
m2expr_Build4TruthAndIf (location_t location, tree a, tree b, tree c, tree d)
{
  tree t1 = m2expr_FoldAndStrip (m2expr_BuildTruthAndIf (location, a, b));
  tree t2 = m2expr_FoldAndStrip (m2expr_BuildTruthAndIf (location, t1, c));
  return m2expr_FoldAndStrip (m2expr_BuildTruthAndIf (location, t2, d));
}

/* Build3TruthAndIf - return true if a && b && c.  Retain order left to right.
 */

static tree
m2expr_Build3TruthAndIf (location_t location, tree op1, tree op2, tree op3)
{
  tree t = m2expr_FoldAndStrip (m2expr_BuildTruthAndIf (location, op1, op2));
  return m2expr_FoldAndStrip (m2expr_BuildTruthAndIf (location, t, op3));
}

/* Build3TruthOrIf - return true if a || b || c.  Retain order left to right.
 */

static tree
m2expr_Build3TruthOrIf (location_t location, tree op1, tree op2, tree op3)
{
  tree t = m2expr_FoldAndStrip (m2expr_BuildTruthOrIf (location, op1, op2));
  return m2expr_FoldAndStrip (m2expr_BuildTruthOrIf (location, t, op3));
}

/* Build4TruthOrIf - return true if op1 || op2 || op3 || op4.  Retain order
   left to right.  */

static tree
m2expr_Build4TruthOrIf (location_t location, tree op1, tree op2, tree op3,
                        tree op4)
{
  tree t1 = m2expr_FoldAndStrip (m2expr_BuildTruthOrIf (location, op1, op2));
  tree t2 = m2expr_FoldAndStrip (m2expr_BuildTruthOrIf (location, t1, op3));
  return m2expr_FoldAndStrip (m2expr_BuildTruthOrIf (location, t2, op4));
}

/* Build4LogicalOr - return true if op1 || op2 || op3 || op4.  */

static tree
m2expr_Build4LogicalOr (location_t location, tree op1, tree op2, tree op3,
                        tree op4)
{
  tree t1 = m2expr_FoldAndStrip (
      m2expr_BuildLogicalOr (location, op1, op2, false));
  tree t2
      = m2expr_FoldAndStrip (m2expr_BuildLogicalOr (location, t1, op3, false));
  return m2expr_FoldAndStrip (
      m2expr_BuildLogicalOr (location, t2, op4, false));
}

/* checkWholeMultOverflow - check to see whether i * j will overflow
   an integer.

PROCEDURE smult (lhs, rhs: INTEGER) ;
BEGIN
   IF ((lhs > 0) AND (rhs > 0) AND (lhs > max DIV rhs)) OR
      ((lhs > 0) AND (rhs < 0) AND (rhs < min DIV lhs)) OR
      ((lhs < 0) AND (rhs > 0) AND (lhs < min DIV rhs)) OR
      ((lhs < 0) AND (rhs < 0) AND (lhs < max DIV rhs))
   THEN
      error ('signed multiplication overflow')
   END
END smult ;

 if ((c1 && c3 && c4)
   || (c1 && c5 && c6)
   || (c2 && c3 && c7)
   || (c2 && c5 && c8))
   error ('signed subtraction overflow').  */

static tree
testWholeMultOverflow (location_t location, tree lhs, tree rhs,
		       tree lowest, tree min, tree max)
{
  tree c1 = m2expr_BuildGreaterThanZero (location, lhs, lowest, min, max);
  tree c2 = m2expr_BuildLessThanZero (location, lhs, lowest, min, max);

  tree c3 = m2expr_BuildGreaterThanZero (location, rhs, lowest, min, max);
  tree c4 = m2expr_BuildGreaterThan (
      location, lhs, m2expr_BuildDivTrunc (location, max, rhs, false));

  tree c5 = m2expr_BuildLessThanZero (location, rhs, lowest, min, max);
  tree c6 = m2expr_BuildLessThan (
      location, rhs, m2expr_BuildDivTrunc (location, min, lhs, false));
  tree c7 = m2expr_BuildLessThan (
      location, lhs, m2expr_BuildDivTrunc (location, min, rhs, false));
  tree c8 = m2expr_BuildLessThan (
      location, lhs, m2expr_BuildDivTrunc (location, max, rhs, false));

  tree c9 = m2expr_Build3TruthAndIf (location, c1, c3, c4);
  tree c10 = m2expr_Build3TruthAndIf (location, c1, c5, c6);
  tree c11 = m2expr_Build3TruthAndIf (location, c2, c3, c7);
  tree c12 = m2expr_Build3TruthAndIf (location, c2, c5, c8);

  tree condition = m2expr_Build4LogicalOr (location, c9, c10, c11, c12);
  return condition;
}


static tree
checkWholeMultOverflow (location_t location, tree i, tree j, tree lowest,
                        tree min, tree max)
{
  tree condition = testWholeMultOverflow (location, i, j, lowest, min, max);
  tree result = M2Range_BuildIfCallWholeHandlerLoc (location, condition,
						    get_current_function_name (),
               "whole value multiplication will cause a range overflow");
  return result;
}


static tree
divMinUnderflow (location_t location, tree value, tree lowest, tree min, tree max)
{
  tree min2 = m2expr_BuildMult (location, min, min, false);
  tree rhs = m2expr_BuildGreaterThanOrEqual (location, value, min2);
  tree lhs = testWholeMultOverflow (location, min, min, lowest, min, max);
  return m2expr_BuildTruthAndIf (location, lhs, rhs);
}

/*
   divexpr - returns true if a DIV_TRUNC b will overflow.
 */

/* checkWholeDivOverflow - check to see whether i DIV_TRUNC j will overflow
   an integer.  The Modula-2 implementation of the GCC trees follows:

PROCEDURE divtruncexpr (a, b: INTEGER) : BOOLEAN ;
BEGIN
   (* Firstly catch division by 0.  *)
   RETURN ((b = 0) OR
           (* Case 2 range is always negative.  *)
           (* In which case a division will be illegal as result will be positive.  *)
           (max < 0) OR
           (* Case 1 both min / max are positive, check for underflow.  *)
           ((min >= 0) AND (max >= 0) AND (multMinOverflow (b) OR (a < b * min))) OR
           (* Case 1 both min / max are positive, check for overflow.  *)
           ((min >= 0) AND (max >= 0) AND (divMinUnderflow (a) OR (b > a DIV min))) OR
           (* Case 3 mixed range, need to check underflow.  *)
           ((min < 0) AND (max >= 0) AND (a < 0) AND (b < 0) AND (b >= a DIV min)) OR
           ((min < 0) AND (max >= 0) AND (a < 0) AND (b > 0) AND (b <= a DIV max)) OR
           ((min < 0) AND (max >= 0) AND (a >= 0) AND (b < 0) AND (a DIV b < min)))
END divtruncexpr ;

s1 -> a DIV min
s2 -> a DIV max
s3 -> a DIV b

b4 -> (min >= 0) AND (max >= 0)
b5 -> (min < 0) AND (max >= 0)
a_lt_b_mult_min -> (a < b * min)
b_mult_min_overflow -> testWholeMultOverflow (location, b, min, lowest, min, max)
b6 -> (b_mult_min_overflow OR a_lt_b_mult_min)
b_gt_s1 -> (b > s1)
a_div_min_overflow -> divMinUnderflow (location, a, min, lowest, min, max)
b7 -> (a_div_min_overflow OR b_gt_s1)
b8 -> (a < 0)
b9 -> (b < 0)
b10 -> (b > 0)
b11 -> (b >= s1)
b12 -> (b <= s2)
b13 -> (s3 < min)
b14 -> a >= 0

c1 -> (b = 0)
c2 -> (max < 0)
c3 -> (b4 AND b6)
c4 -> (b4 AND b7)
c5 -> (b5 AND b8 AND b9 AND b11)
c6 -> (b5 AND b8 AND b10 AND b12)
c7 -> (b5 AND b14 AND b9 AND b13)

 if (c1 || c2 || c3 || c4 || c5 || c6 || c7)
   error ('signed div trunc overflow').  */

static tree
checkWholeDivTruncOverflow (location_t location, tree i, tree j, tree lowest,
			    tree min, tree max)
{
  tree b4a = m2expr_BuildGreaterThanOrEqualZero (location, min, lowest, min, max);
  tree b4b = m2expr_BuildGreaterThanOrEqualZero (location, max, lowest, min, max);
  tree b4 = m2expr_BuildTruthAndIf (location, b4a, b4b);
  tree b5a = m2expr_BuildLessThanZero (location, min, lowest, min, max);
  tree b5 = m2expr_BuildTruthAndIf (location, b5a, b4b);
  tree c1 = m2expr_BuildEqualToZero (location, j, lowest, min, max);
  tree c2 = m2expr_BuildLessThanZero (location, max, lowest, min, max);
  tree i_lt_j_mult_min = m2expr_BuildLessThan (location, i, m2expr_BuildMult (location, j, min, false));
  tree j_mult_min_overflow = testWholeMultOverflow (location, j, min, lowest, min, max);
  tree b6 = m2expr_BuildTruthOrIf (location, j_mult_min_overflow, i_lt_j_mult_min);
  tree c3 = m2expr_BuildTruthAndIf (location, b4, b6);
  tree s1 = m2expr_BuildDivTrunc (location, i, min, false);
  tree s2 = m2expr_BuildDivTrunc (location, i, max, false);
  tree s3 = m2expr_BuildDivTrunc (location, i, j, false);

  tree j_gt_s1 = m2expr_BuildGreaterThan (location, j, s1);
  tree i_div_min_overflow = divMinUnderflow (location, i, lowest, min, max);
  tree b7 = m2expr_BuildTruthOrIf (location, i_div_min_overflow, j_gt_s1);
  tree c4 = m2expr_BuildTruthAndIf (location, b4, b7);
  tree b8 = m2expr_BuildLessThanZero (location, i, lowest, min, max);
  tree b9 = m2expr_BuildLessThanZero (location, j, lowest, min, max);
  tree b10 = m2expr_BuildGreaterThanZero (location, j, lowest, min, max);
  tree b11 = m2expr_BuildGreaterThanOrEqual (location, j, s1);
  tree b12 = m2expr_BuildLessThanOrEqual (location, j, s2);
  tree b13 = m2expr_BuildLessThan (location, s3, min);
  tree b14 = m2expr_BuildGreaterThanOrEqualZero (location, i, lowest, min, max);
  tree c5 = m2expr_Build4TruthAndIf (location, b5, b8, b9, b11);
  tree c6 = m2expr_Build4TruthAndIf (location, b5, b8, b10, b12);
  tree c7 = m2expr_Build4TruthAndIf (location, b5, b14, b9, b13);
  tree c8 = m2expr_Build4TruthOrIf (location, c1, c2, c3, c4);
  tree condition = m2expr_Build4TruthOrIf (location, c5, c6, c7, c8);
  tree t = M2Range_BuildIfCallWholeHandlerLoc (location, condition,
					       get_current_function_name (),
               "whole value truncated division will cause a range overflow");
  return t;
}

#if 0
(*
   divexpr - returns true if a DIV_CEIL b will overflow.
 *)

(* checkWholeDivCeilOverflow - check to see whether i DIV_CEIL j will overflow
   an integer.  *)

PROCEDURE divceilexpr (i, j: INTEGER) : BOOLEAN ;
BEGIN
   RETURN ((j = 0) OR     (* division by zero.  *)
           (maxT < 0) OR  (* both inputs are < 0 and max is < 0,
                             therefore error.  *)
           ((i # 0) AND   (* first operand is legally zero,
                             result is also legally zero.  *)
            divCeilOverflowCases (i, j)))
END divceilexpr ;


(*
   divCeilOverflowCases - precondition:  i, j are in range values.
                          postcondition:  true is returned if i divceil will
                                          result in an overflow/underflow.
*)

PROCEDURE divCeilOverflowCases (i, j: INTEGER) : BOOLEAN ;
BEGIN
   RETURN (((i > 0) AND (j > 0) AND divCeilOverflowPosPos (i, j)) OR
           ((i < 0) AND (j < 0) AND divCeilOverflowNegNeg (i, j)) OR
           ((i > 0) AND (j < 0) AND divCeilOverflowPosNeg (i, j)) OR
           ((i < 0) AND (j > 0) AND divCeilOverflowNegPos (i, j)))
END divCeilOverflowCases ;


(*
   divCeilOverflowPosPos - precondition:  i, j are legal and are both >= 0.
                           postcondition:  true is returned if i divceil will
                                           result in an overflow/underflow.
*)

PROCEDURE divCeilOverflowPosPos (i, j: INTEGER) : BOOLEAN ;
BEGIN
   RETURN (((i MOD j = 0) AND (i < j * minT)) OR
           (((i MOD j # 0) AND (i < j * minT + 1))))
END divCeilOverflowPosPos ;


(*
   divCeilOverflowNegNeg - precondition:  i, j are in range values and both < 0.
                           postcondition:  true is returned if i divceil will
                                           result in an overflow/underflow.
*)

PROCEDURE divCeilOverflowNegNeg (i, j: INTEGER) : BOOLEAN ;
BEGIN
   RETURN ((maxT <= 0) OR           (* signs will cause overflow.  *)
           (* check for underflow.  *)
           ((ABS (i) MOD ABS (j) = 0) AND (i >= j * minT)) OR
           ((ABS (i) MOD ABS (j) # 0) AND (i >= j * minT - 1)) OR
           (* check for overflow.  *)
           (((ABS (i) MOD maxT) = 0) AND (ABS (i) DIV maxT > ABS (j))) OR
           (((ABS (i) MOD maxT) # 0) AND (ABS (i) DIV maxT > ABS (j) + 1)))
END divCeilOverflowNegNeg ;


(*
   divCeilOverflowNegPos - precondition:  i, j are in range values.  i < 0, j >= 0.
                           postcondition:  true is returned if i divceil will
                                           result in an overflow/underflow.
*)

PROCEDURE divCeilOverflowNegPos (i, j: INTEGER) : BOOLEAN ;
BEGIN
   (* easier than might be initially expected.  We know minT < 0 and maxT > 0.
      We know the result will be negative and therefore we only need to test
      against minT.  *)
   RETURN (((ABS (i) MOD j = 0) AND (i < j * minT)) OR
           ((ABS (i) MOD j # 0) AND (i < j * minT - 1)))
END divCeilOverflowNegPos ;


(*
   divCeilOverflowPosNeg - precondition:  i, j are in range values.  i >= 0, j < 0.
                           postcondition:  true is returned if i divceil will
                                           result in an overflow/underflow.
*)

PROCEDURE divCeilOverflowPosNeg (i, j: INTEGER) : BOOLEAN ;
BEGIN
   (* easier than might be initially expected.  We know minT < 0 and maxT > 0.
      We know the result will be negative and therefore we only need to test
      against minT.  *)
   RETURN (((i MOD ABS (j) = 0) AND (i > j * minT)) OR
           ((i MOD ABS (j) # 0) AND (i > j * minT - 1)))
END divCeilOverflowPosNeg ;
#endif

/* divCeilOverflowPosPos, precondition:  lhs, rhs are legal and are both >= 0.
   Postcondition:  TRUE is returned if lhs divceil rhs will result
   in an overflow/underflow.

   A handbuilt expression of trees implementing:

   RETURN (((lhs MOD rhs = 0) AND (min >= 0) AND (lhs < rhs * min)) OR       (* check for underflow, no remainder.   *)
                                                  lhs_lt_rhs_mult_min
           (((lhs MOD rhs # 0) AND (lhs < rhs * min + 1))))   (* check for underflow with remainder.  *)
                                   ((lhs > min) AND (lhs - 1 > rhs * min))
                                                  lhs_gt_rhs_mult_min

   a -> (lhs MOD rhs = 0) AND (lhs < rhs * min)
   b -> (lhs MOD rhs # 0) AND (lhs < rhs * min + 1)
   RETURN a OR b.  */

static tree
divCeilOverflowPosPos (location_t location, tree i, tree j, tree lowest,
		       tree min, tree max)
{
  tree i_mod_j = m2expr_BuildModTrunc (location, i, j, false);
  tree i_mod_j_eq_zero = m2expr_BuildEqualToZero (location, i_mod_j, lowest, min, max);
  tree i_mod_j_ne_zero = m2expr_BuildNotEqualToZero (location, i_mod_j, lowest, min, max);
  tree j_min = m2expr_BuildMult (location, j, min, false);
  tree j_min_1 = m2expr_BuildAdd (location, j_min, m2expr_GetIntegerOne (location), false);
  tree i_lt_j_min = m2expr_BuildLessThan (location, i, j_min);
  tree i_lt_j_min_1 = m2expr_BuildLessThan (location, i, j_min_1);
  tree a = m2expr_BuildTruthAndIf (location, i_mod_j_eq_zero, i_lt_j_min);
  tree b = m2expr_BuildTruthAndIf (location, i_mod_j_ne_zero, i_lt_j_min_1);
  return m2expr_BuildTruthOrIf (location, a, b);
}


/* divCeilOverflowPosNeg precondition:  i, j are in range values and i >=0, j < 0.
   Postcondition:  TRUE is returned if i divceil j will result in an
   overflow/underflow.

   A handbuilt expression of trees implementing:

   RETURN (((i MOD ABS (j) = 0) AND (i > j * min)) OR
           ((i MOD ABS (j) # 0) AND (i > j * min - 1)))

   abs_j -> (ABS (j))
   i_mod_abs_j -> (i MOD abs_j)
   i_mod_abs_j_eq_0 -> (i_mod_abs_j = 0)
   i_mod_abs_j_ne_0 -> (i_mod_abs_j # 0)
   j_mult_min -> (j * min)
   j_mult_min_1 -> (j_mult_min - 1)
   i_gt_j_mult_min -> (i > j_mult_min)
   i_gt_j_mult_min_1 -> (i > j_mult_min_1)
   a -> (i_mod_abs_j_eq_0 AND i_gt_j_mult_min)
   b -> (i_mod_abs_j_ne_0 AND i_gt_j_mult_min_1)
   c -> (a OR b).  */

static tree
divCeilOverflowPosNeg (location_t location, tree i, tree j, tree lowest, tree min, tree max)
{
  tree abs_j = m2expr_BuildAbs (location, j);
  tree i_mod_abs_j = m2expr_BuildModFloor (location, i, abs_j, false);
  tree i_mod_abs_j_eq_0 = m2expr_BuildEqualToZero (location, i_mod_abs_j, lowest, min, max);
  tree i_mod_abs_j_ne_0 = m2expr_BuildNotEqualToZero (location, i_mod_abs_j, lowest, min, max);
  tree j_mult_min = m2expr_BuildMult (location, j, min, false);
  tree j_mult_min_1 = m2expr_BuildPostDec (location, j_mult_min);
  tree i_gt_j_mult_min = m2expr_BuildGreaterThan (location, i, j_mult_min);
  tree i_gt_j_mult_min_1 = m2expr_BuildGreaterThan (location, i, j_mult_min_1);
  tree a = m2expr_BuildTruthAndIf (location, i_mod_abs_j_eq_0, i_gt_j_mult_min);
  tree b = m2expr_BuildTruthAndIf (location, i_mod_abs_j_ne_0, i_gt_j_mult_min_1);
  tree c = m2expr_BuildTruthOrIf (location, a, b);
  return c;
}


/* divCeilOverflowNegPos precondition:  i, j are in range values and i < 0, j >= 0.
   Postcondition:  TRUE is returned if i divceil j will result in an
   overflow/underflow.

   A handbuilt expression of trees implementing:

   RETURN (((ABS (i) MOD j = 0) AND (i < j * min)) OR
           ((ABS (i) MOD j # 0) AND (i < j * min - 1)))

   abs_i -> (ABS (i))
   abs_i_mod_j -> (abs_i MOD j)
   abs_i_mod_j_eq_0 -> (abs_i_mod_j = 0)
   abs_i_mod_j_ne_0 -> (abs_i_mod_j # 0)
   j_mult_min -> (j * min)
   j_mult_min_1 -> (j_mult_min - 1)
   i_lt_j_mult_min -> (i < j_mult_min)
   i_lt_j_mult_min_1 -> (i < j_mult_min_1)
   a = (abs_i_mod_j_eq_0 AND i_lt_j_mult_min)
   b = (abs_i_mod_j_ne_0 AND i_lt_j_mult_min_1)
   c -> (a OR b).  */

static tree
divCeilOverflowNegPos (location_t location, tree i, tree j, tree lowest, tree min, tree max)
{
  tree abs_i = m2expr_BuildAbs (location, i);
  tree abs_i_mod_j = m2expr_BuildModFloor (location, abs_i, j, false);
  tree abs_i_mod_j_eq_0 = m2expr_BuildEqualToZero (location, abs_i_mod_j, lowest, min, max);
  tree abs_i_mod_j_ne_0 = m2expr_BuildNotEqualToZero (location, abs_i_mod_j, lowest, min, max);
  tree j_mult_min = m2expr_BuildMult (location, j, min, false);
  tree j_mult_min_1 = m2expr_BuildPostDec (location, j_mult_min);
  tree i_lt_j_mult_min = m2expr_BuildLessThan (location, i, j_mult_min);
  tree i_lt_j_mult_min_1 = m2expr_BuildLessThan (location, i, j_mult_min_1);
  tree a = m2expr_BuildTruthAndIf (location, abs_i_mod_j_eq_0, i_lt_j_mult_min);
  tree b = m2expr_BuildTruthAndIf (location, abs_i_mod_j_ne_0, i_lt_j_mult_min_1);
  tree c = m2expr_BuildTruthOrIf (location, a, b);
  return c;
}


/* divCeilOverflowNegNeg precondition:  i, j are in range values and both < 0.
   Postcondition:  TRUE is returned if i divceil j will result in an
   overflow/underflow.

   A handbuilt expression of trees implementing:

   RETURN ((max <= 0) OR           (* signs will cause overflow.  *)
           (* check for underflow.  *)
           ((ABS (i) MOD ABS (j) = 0) AND (i >= j * min)) OR
           ((ABS (i) MOD ABS (j) # 0) AND (i >= j * min - 1)) OR
           (* check for overflow.  *)
           (((ABS (i) MOD max) = 0) AND (ABS (i) DIV max > ABS (j))) OR
           (((ABS (i) MOD max) # 0) AND (ABS (i) DIV max > ABS (j) + 1)))

  max_lte_0 -> (max <= 0)
  abs_i -> (ABS (i))
  abs_j -> (ABS (j))
  abs_i_mod_abs_j -> (abs_i MOD abs_j)
  abs_i_mod_abs_j_eq_0 -> (abs_i_mod_abs_j = 0)
  abs_i_mod_abs_j_ne_0 -> (abs_i_mod_abs_j # 0)
  j_mult_min -> (j * min)
  j_mult_min_1 -> (j_mult_min - 1)
  i_ge_j_mult_min -> (i >= j_mult_min)
  i_ge_j_mult_min_1 -> (i >= j_mult_min_1)
  abs_i_mod_max -> (abs_i mod max)
  abs_i_div_max -> (abs_i DIVfloor max)
  abs_j_1 -> (abs_j + 1)
  abs_i_mod_max_eq_0 -> (abs_i_mod_max = 0)
  abs_i_mod_max_ne_0 -> (abs_i_mod_max # 0)
  abs_i_div_max_gt_abs_j -> (abs_i_div_max > abs_j)
  abs_i_div_max_gt_abs_j_1 -> (abs_i_div_max > abs_j_1)

  a -> (abs_i_mod_abs_j_eq_0 AND i_ge_j_mult_min)
  b -> (abs_i_mod_abs_j_ne_0 AND i_ge_j_mult_min_1)
  c -> (abs_i_mod_max_eq_0 AND abs_i_div_max_gt_abs_j)
  d -> (abs_i_mod_max_ne_0 AND abs_i_div_max_gt_abs_j_1)
  e -> (a OR b OR c OR d)
  return max_lte_0 OR e.  */

static tree
divCeilOverflowNegNeg (location_t location, tree i, tree j, tree lowest,
		       tree min, tree max)
{
  tree max_lte_0 = m2expr_BuildLessThanOrEqualZero (location, max, lowest, min, max);
  tree abs_i = m2expr_BuildAbs (location, i);
  tree abs_j = m2expr_BuildAbs (location, j);
  tree abs_i_mod_abs_j = m2expr_BuildModFloor (location, abs_i, abs_j, false);
  tree abs_i_mod_abs_j_eq_0 = m2expr_BuildEqualToZero (location, abs_i_mod_abs_j,
						       lowest, min, max);
  tree abs_i_mod_abs_j_ne_0 = m2expr_BuildNotEqualToZero (location, abs_i_mod_abs_j,
							  lowest, min, max);
  tree j_mult_min = m2expr_BuildMult (location, j, min, false);
  tree j_mult_min_1 = m2expr_BuildPostDec (location, j_mult_min);
  tree i_ge_j_mult_min = m2expr_BuildGreaterThanOrEqual (location, i, j_mult_min);
  tree i_ge_j_mult_min_1 = m2expr_BuildGreaterThanOrEqual (location, i, j_mult_min_1);
  tree abs_i_mod_max = m2expr_BuildModFloor (location, abs_i, max, false);
  tree abs_i_div_max = m2expr_BuildDivFloor (location, abs_i, max, false);
  tree abs_j_1 = m2expr_BuildPostInc (location, abs_j);
  tree abs_i_mod_max_eq_0 = m2expr_BuildEqualToZero (location, abs_i_mod_max, lowest, min, max);
  tree abs_i_mod_max_ne_0 = m2expr_BuildNotEqualToZero (location, abs_i_mod_max, lowest, min, max);
  tree abs_i_div_max_gt_abs_j = m2expr_BuildGreaterThan (location,  abs_i_div_max, abs_j);
  tree abs_i_div_max_gt_abs_j_1 = m2expr_BuildGreaterThan (location, abs_i_div_max, abs_j_1);

  tree a = m2expr_BuildTruthAndIf (location, abs_i_mod_abs_j_eq_0, i_ge_j_mult_min);
  tree b = m2expr_BuildTruthAndIf (location, abs_i_mod_abs_j_ne_0, i_ge_j_mult_min_1);
  tree c = m2expr_BuildTruthAndIf (location, abs_i_mod_max_eq_0, abs_i_div_max_gt_abs_j);
  tree d = m2expr_BuildTruthAndIf (location, abs_i_mod_max_ne_0, abs_i_div_max_gt_abs_j_1);
  tree e = m2expr_Build4TruthOrIf (location, a, b, c, d);
  return m2expr_BuildTruthOrIf (location, max_lte_0, e);
}


/* divCeilOverflowCases, precondition:  i, j are in range values.
   Postcondition:  TRUE is returned if i divceil will result in an
   overflow/underflow.

   A handbuilt expression of trees implementing:

   RETURN (((i > 0) AND (j > 0) AND divCeilOverflowPosPos (i, j)) OR
           ((i < 0) AND (j < 0) AND divCeilOverflowNegNeg (i, j)) OR
           ((i > 0) AND (j < 0) AND divCeilOverflowPosNeg (i, j)) OR
           ((i < 0) AND (j > 0) AND divCeilOverflowNegPos (i, j)))

   a -> ((i > 0) AND (j > 0) AND divCeilOverflowPosPos (i, j))
   b -> ((i < 0) AND (j < 0) AND divCeilOverflowNegNeg (i, j))
   c -> ((i > 0) AND (j < 0) AND divCeilOverflowPosNeg (i, j))
   d -> ((i < 0) AND (j > 0) AND divCeilOverflowNegPos (i, j))

   RETURN a AND b AND c AND d.  */

static tree
divCeilOverflowCases (location_t location, tree i, tree j, tree lowest,
		      tree min, tree max)
{
  tree i_gt_zero = m2expr_BuildGreaterThanZero (location, i, lowest, min, max);
  tree j_gt_zero = m2expr_BuildGreaterThanZero (location, j, lowest, min, max);
  tree i_lt_zero = m2expr_BuildLessThanZero (location, i, lowest, min, max);
  tree j_lt_zero = m2expr_BuildLessThanZero (location, j, lowest, min, max);
  tree a = m2expr_Build3TruthAndIf (location, i_gt_zero, j_gt_zero,
				    divCeilOverflowPosPos (location, i, j, lowest, min, max));
  tree b = m2expr_Build3TruthAndIf (location, i_lt_zero, j_lt_zero,
				    divCeilOverflowNegNeg (location, i, j, lowest, min, max));
  tree c = m2expr_Build3TruthAndIf (location, i_gt_zero, j_lt_zero,
				    divCeilOverflowPosNeg (location, i, j, lowest, min, max));
  tree d = m2expr_Build3TruthAndIf (location, i_lt_zero, j_gt_zero,
				    divCeilOverflowNegPos (location, i, j, lowest, min, max));
  return m2expr_Build4TruthOrIf (location, a, b, c, d);
}


/* checkWholeDivCeilOverflow check to see whether i DIV_CEIL j will overflow
   an integer.  A handbuilt expression of trees implementing:

   RETURN ((j = 0) OR     (* division by zero.  *)
           (maxT < 0) OR  (* both inputs are < 0 and max is < 0,
                             therefore error.  *)
           ((i # 0) AND   (* first operand is legally zero,
                             result is also legally zero.  *)
            divCeilOverflowCases (i, j)))

   using the following subexpressions:

   j_eq_zero -> (j == 0)
   max_lt_zero -> (max < 0)
   i_ne_zero -> (i # 0).  */

static tree
checkWholeDivCeilOverflow (location_t location, tree i, tree j, tree lowest,
			   tree min, tree max)
{
  tree j_eq_zero = m2expr_BuildEqualToZero (location, j, lowest, min, max);
  tree max_lt_zero = m2expr_BuildLessThanZero (location, max, lowest, min, max);
  tree i_ne_zero = m2expr_BuildNotEqualToZero (location, i, lowest, min, max);
  tree j_lt_zero;
  tree rhs = m2expr_BuildTruthAndIf (location,
				     i_ne_zero,
				     divCeilOverflowCases (location,
							   i, j, lowest, min, max));

  if (M2Options_GetISO ())
    j_lt_zero = m2expr_FoldAndStrip (m2expr_BuildLessThanZero (location, j, lowest, min, max));
  else
    j_lt_zero = m2expr_GetIntegerZero (location);
  j_eq_zero = m2expr_FoldAndStrip (j_eq_zero);
  max_lt_zero = m2expr_FoldAndStrip (max_lt_zero);
  i_ne_zero = m2expr_FoldAndStrip (i_ne_zero);
  rhs = m2expr_FoldAndStrip (rhs);

  tree condition = m2expr_Build4TruthOrIf (location, j_eq_zero, max_lt_zero, rhs, j_lt_zero);
  tree t = M2Range_BuildIfCallWholeHandlerLoc (location, condition,
					       get_current_function_name (),
               "whole value ceil division will cause a range overflow");
  return t;
}


/* checkWholeModTruncOverflow, the GCC tree.def defines TRUNC_MOD_EXPR to return
   the remainder which has the same sign as the dividend.  In ISO Modula-2 the
   divisor must never be negative (or zero).  The pseudo code for implementing these
   checks is given below:

   IF j = 0
   THEN
      RETURN TRUE   (* division by zero.  *)
   ELSIF j < 0
   THEN
      RETURN TRUE   (* modulus and division by negative (rhs) not allowed in ISO Modula-2.  *)
   ELSIF i = 0
   THEN
      RETURN FALSE  (* must be legal as result is same as operand.  *)
   ELSIF i > 0
   THEN
      (* test for:  i MOD j < minT  *)
      IF j > i
      THEN
         RETURN FALSE
      END ;
      RETURN i - ((i DIV j) * j) < minT
   ELSIF i < 0
   THEN
      (* the result will always be positive and less than i, given that j is less than zero
         we know that minT must be < 0 as well and therefore the result of i MOD j will
         never underflow.  *)
      RETURN FALSE
   END ;
   RETURN FALSE

   which can be converted into a large expression:

   RETURN (j = 0) OR ((j < 0) AND ISO) OR
          ((i # 0) AND (j <= i) AND (i - ((i DIVtrunc j) * j) < minT)

   and into GCC trees:

   c1 ->  (j = 0)
   c2 ->  (j < 0)  (* only called from ISO or PIM4 or -fpositive-mod-floor  *)
   c3 ->  (i # 0)
   c4 ->  (j <= i)
   c6 ->  (i DIVtrunc j)
   c7 ->  (i - (c6 * j))
   c5 ->  c7 < minT

   t -> (c1 OR c2 OR
        (c3 AND c4 AND c5)).  */

static tree
checkWholeModTruncOverflow (location_t location, tree i, tree j, tree lowest,
			    tree min, tree max)
{
  tree c1 = m2expr_BuildEqualToZero (location, j, lowest, min, max);
  tree c2 = m2expr_BuildLessThanZero (location, j, lowest, min, max);
  tree c3 = m2expr_BuildNotEqualToZero (location, i, lowest, min, max);
  tree c4 = m2expr_BuildLessThanOrEqual (location, j, i);
  tree c6 = m2expr_BuildDivTrunc (location, i, j, false);
  tree c7 = m2expr_BuildSub (location, i, m2expr_BuildMult (location, c6, j, false), false);
  tree c5 = m2expr_BuildLessThan (location, c7, min);
  tree c8 = m2expr_Build3TruthAndIf (location, c3, c4, c5);
  tree condition = m2expr_Build3TruthOrIf (location, c1, c2, c8);
  tree t = M2Range_BuildIfCallWholeHandlerLoc (location, condition,
					       get_current_function_name (),
               "whole value trunc modulus will cause a range overflow");
  return t;
}


/* checkWholeModCeilOverflow, the GCC tree.def defines CEIL_MOD_EXPR to return
   the remainder which has the same opposite of the divisor.  In gm2 this is
   only called when the divisor is negative.  The pseudo code for implementing
   these checks is given below:

   IF j = 0
   THEN
      RETURN TRUE   (* division by zero.  *)
   END ;
   t := i - j * divceil (i, j) ;
   printf ("t = %d, i = %d, j = %d, %d / %d = %d\n",
           t, i, j, i, j, divceil (i, j));
   RETURN NOT ((t >= minT) AND (t <= maxT))

   which can be converted into the expression:

   t := i - j * divceil (i, j) ;
   RETURN (j = 0) OR (NOT ((t >= minT) AND (t <= maxT)))

   and into GCC trees:

   c1 ->  (j = 0)
   c2 ->  (i - j)
   c3 ->  (i DIVceil j)
   t ->   (c2 * c3)
   c4 ->  (t >= minT)
   c5 ->  (t <= maxT)
   c6 ->  (c4 AND c5)
   c7 ->  (NOT c6)
   c8 ->  (c1 OR c7)
   return c8.  */

static tree
checkWholeModCeilOverflow (location_t location,
			   tree i, tree j, tree lowest,
			   tree min, tree max)
{
  tree c1 = m2expr_BuildEqualToZero (location, j, lowest, min, max);
  tree c2 = m2expr_BuildSub (location, i, j, false);
  tree c3 = m2expr_BuildDivCeil (location, i, j, false);
  tree t  = m2expr_BuildMult (location, c2, c3, false);
  tree c4 = m2expr_BuildGreaterThanOrEqual (location, t, min);
  tree c5 = m2expr_BuildLessThanOrEqual (location, t, max);
  tree c6 = m2expr_BuildTruthAndIf (location, c4, c5);
  tree c7 = m2expr_BuildTruthNot (location, c6);
  tree condition = m2expr_BuildTruthOrIf (location, c1, c7);
  tree s = M2Range_BuildIfCallWholeHandlerLoc (location, condition,
					       get_current_function_name (),
               "whole value ceil modulus will cause a range overflow");
  return s;
}


/* checkWholeModFloorOverflow, the GCC tree.def defines FLOOR_MOD_EXPR to return
   the remainder which has the same sign as the divisor.  In gm2 this is
   only called when the divisor is positive.  The pseudo code for implementing
   these checks is given below:

   IF j = 0
   THEN
      RETURN TRUE   (* division by zero.  *)
   END ;
   t := i - j * divfloor (i, j) ;
   printf ("t = %d, i = %d, j = %d, %d / %d = %d\n",
           t, i, j, i, j, divfloor (i, j));
   RETURN NOT ((t >= minT) AND (t <= maxT))

   which can be converted into the expression:

   t := i - j * divfloor (i, j) ;
   RETURN (j = 0) OR (NOT ((t >= minT) AND (t <= maxT)))

   and into GCC trees:

   c1 ->  (j = 0)
   c2 ->  (i - j)
   c3 ->  (i DIVfloor j)
   t ->   (c2 * c3)
   c4 ->  (t >= minT)
   c5 ->  (t <= maxT)
   c6 ->  (c4 AND c5)
   c7 ->  (NOT c6)
   c8 ->  (c1 OR c7)
   return c8.  */

static tree
checkWholeModFloorOverflow (location_t location,
			    tree i, tree j, tree lowest,
			    tree min, tree max)
{
  tree c1 = m2expr_BuildEqualToZero (location, j, lowest, min, max);
  tree c2 = m2expr_BuildSub (location, i, j, false);
  tree c3 = m2expr_BuildDivFloor (location, i, j, false);
  tree t  = m2expr_BuildMult (location, c2, c3, false);
  tree c4 = m2expr_BuildGreaterThanOrEqual (location, t, min);
  tree c5 = m2expr_BuildLessThanOrEqual (location, t, max);
  tree c6 = m2expr_BuildTruthAndIf (location, c4, c5);
  tree c7 = m2expr_BuildTruthNot (location, c6);
  tree condition = m2expr_BuildTruthOrIf (location, c1, c7);
  tree s = M2Range_BuildIfCallWholeHandlerLoc (location, condition,
					       get_current_function_name (),
               "whole value floor modulus will cause a range overflow");
  return s;
}


#if 0
/*  The following is a Modula-2 implementation of the C tree node code
    this code has been hand translated into GCC trees.  */

(*
   divFloorOverflow2 - returns true if an overflow will occur
                       if i divfloor j is performed.
*)

PROCEDURE divFloorOverflow (i, j: INTEGER) : BOOLEAN ;
BEGIN
   RETURN ((j = 0) OR     (* division by zero.  *)
           (maxT < 0) OR  (* both inputs are < 0 and max is < 0,
                             therefore error.  *)
                          (* --fixme-- remember here to also check
                             if ISO M2 dialect and j < 0
                             which will also generate an error.  *)
           ((i # 0) AND   (* first operand is legally zero,
                             result is also legally zero.  *)
            divFloorOverflowCases (i, j)))
END divFloorOverflow ;


(*
   divFloorOverflowCases - precondition:  i, j are in range values.
                           postcondition:  true is returned if i divfloor will
                                           result in an overflow/underflow.
*)

PROCEDURE divFloorOverflowCases (i, j: INTEGER) : BOOLEAN ;
BEGIN
   RETURN (((i > 0) AND (j > 0) AND divFloorOverflowPosPos (i, j)) OR
           ((i < 0) AND (j < 0) AND divFloorOverflowNegNeg (i, j)) OR
           ((i > 0) AND (j < 0) AND divFloorOverflowPosNeg (i, j)) OR
           ((i < 0) AND (j > 0) AND divFloorOverflowNegPos (i, j)))
END divFloorOverflowCases ;


(*
   divFloorOverflowPosPos - precondition:  lhs, rhs are legal and are both >= 0.
                            postcondition:  true is returned if lhs divfloor rhs will
                                            result in an overflow/underflow.
*)

PROCEDURE divFloorOverflowPosPos (lhs, rhs: INTEGER) : BOOLEAN ;
BEGIN
   RETURN multMinOverflow (rhs) OR (lhs < rhs * min)
END divFloorOverflowPosPos ;


(*
   divFloorOverflowNegNeg - precondition:  i, j are in range values and both < 0.
                            postcondition:  true is returned if i divfloor will
                                            result in an overflow/underflow.
*)

PROCEDURE divFloorOverflowNegNeg (i, j: INTEGER) : BOOLEAN ;
BEGIN
   RETURN ((maxT <= 0) OR           (* signs will cause overflow.  *)
           (* check for underflow.  *)
           (i >= j * minT) OR
           (* check for overflow.  *)
           (ABS (i) DIV maxT > ABS (j)))
END divFloorOverflowNegNeg ;


(*
   divFloorOverflowNegPos - precondition:  i, j are in range values.  i < 0, j >= 0.
                            postcondition:  true is returned if i divfloor will
                                            result in an overflow/underflow.
*)

PROCEDURE divFloorOverflowNegPos (i, j: INTEGER) : BOOLEAN ;
BEGIN
   (* easier than might be initially expected.  We know minT < 0 and maxT > 0.
      We know the result will be negative and therefore we only need to test
      against minT.  *)
   RETURN i < j * minT
END divFloorOverflowNegPos ;


(*
   divFloorOverflowPosNeg - precondition:  i, j are in range values.  i >= 0, j < 0.
                           postcondition:  true is returned if i divfloor will
                                           result in an overflow/underflow.
*)

PROCEDURE divFloorOverflowPosNeg (i, j: INTEGER) : BOOLEAN ;
BEGIN
   (* easier than might be initially expected.  We know minT < 0 and maxT > 0.
      We know the result will be negative and therefore we only need to test
      against minT.  *)
   RETURN i >= j * minT - j  (* is safer than i > j * minT -1 *)
END divFloorOverflowPosNeg ;
#endif


/* divFloorOverflowPosPos, precondition:  i, j are legal and are both >= 0.
   Postcondition:  true is returned if i divfloor will result in an overflow/underflow.

   A handbuilt expression of trees implementing:

   RETURN i < j * min

   j_mult_min -> (j * min)
   RETURN i < j_mult_min.  */

static tree
divFloorOverflowPosPos (location_t location, tree i, tree j, tree min)
{
  tree j_mult_min = m2expr_BuildMult (location, j, min, false);
  tree i_lt_j_mult_min = m2expr_BuildLessThan (location, i, j_mult_min);
  return i_lt_j_mult_min;
}


/* divFloorOverflowNegNeg precondition:  i, j are in range values and both < 0.
   Postcondition:  true is returned if i divfloor j will result in an
   overflow/underflow.

   A handbuilt expression of trees implementing:

   RETURN ((maxT <= 0) OR           (* signs will cause overflow.  *)
           (* check for underflow.  *)
           (i >= j * min) OR
           (* check for overflow.  *)
           (ABS (i) DIV max > ABS (j)))

  max_lte_0 -> (max <= 0)
  abs_i -> (ABS (i))
  abs_j -> (ABS (j))
  j_mult_min -> (j * min)
  i_ge_j_mult_min -> (i >= j_mult_min)
  abs_i_div_max -> (abs_i divfloor max)
  abs_i_div_max_gt_abs_j -> (abs_i_div_max > abs_j)

  return max_lte_0 OR
         i_ge_j_mult_min OR
         abs_i_div_max_gt_abs_j.  */

static tree
divFloorOverflowNegNeg (location_t location, tree i, tree j, tree lowest,
			tree min, tree max)
{
  tree max_lte_0 = m2expr_BuildLessThanOrEqualZero (location, max, lowest, min, max);
  tree abs_i = m2expr_BuildAbs (location, i);
  tree abs_j = m2expr_BuildAbs (location, j);
  tree j_mult_min = m2expr_BuildMult (location, j, min, false);
  tree i_ge_j_mult_min = m2expr_BuildGreaterThanOrEqual (location, i, j_mult_min);
  tree abs_i_div_max = m2expr_BuildDivFloor (location, abs_i, max, false);
  tree abs_i_div_max_gt_abs_j = m2expr_BuildGreaterThan (location,  abs_i_div_max, abs_j);

  return m2expr_Build3TruthOrIf (location, max_lte_0, i_ge_j_mult_min, abs_i_div_max_gt_abs_j);
}


/* divFloorOverflowPosNeg precondition:  i, j are in range values and i >=0, j < 0.
   Postcondition:  true is returned if i divfloor j will result in an
   overflow/underflow.

   A handbuilt expression of trees implementing:

   RETURN i >= j * min - j  (* is safer than i > j * min -1 *)

   j_mult_min -> (j * min)
   j_mult_min_sub_j -> (j_mult_min - j)
   i_ge_j_mult_min_sub_j -> (i >= j_mult_min_sub_j)

   return i_ge_j_mult_min_sub_j.  */

static tree
divFloorOverflowPosNeg (location_t location, tree i, tree j, tree min)
{
  tree j_mult_min = m2expr_BuildMult (location, j, min, false);
  tree j_mult_min_sub_j = m2expr_BuildSub (location, j_mult_min, j, false);
  tree i_ge_j_mult_min_sub_j = m2expr_BuildGreaterThanOrEqual (location, i, j_mult_min_sub_j);
  return i_ge_j_mult_min_sub_j;
}


/* divFloorOverflowNegPos precondition:  i, j are in range values and i < 0, j > 0.
   Postcondition:  true is returned if i divfloor j will result in an
   overflow/underflow.

   A handbuilt expression of trees implementing:

   RETURN i < j * min

   j_mult_min -> (j * min)
   RETURN i < j_mult_min.  */

static tree
divFloorOverflowNegPos (location_t location, tree i, tree j, tree min)
{
  tree j_mult_min = m2expr_BuildMult (location, j, min, false);
  tree i_lt_j_mult_min = m2expr_BuildLessThan (location, i, j_mult_min);
  return i_lt_j_mult_min;
}


/* divFloorOverflowCases, precondition:  i, j are in range values.
   Postcondition:  true is returned if i divfloor will result in an
   overflow/underflow.

   A handbuilt expression of trees implementing:

   RETURN (((i > 0) AND (j > 0) AND divFloorOverflowPosPos (i, j)) OR
           ((i < 0) AND (j < 0) AND divFloorOverflowNegNeg (i, j)) OR
           ((i > 0) AND (j < 0) AND divFloorOverflowPosNeg (i, j)) OR
           ((i < 0) AND (j > 0) AND divFloorOverflowNegPos (i, j)))

   a -> ((i > 0) AND (j > 0) AND divFloorOverflowPosPos (i, j))
   b -> ((i < 0) AND (j < 0) AND divFloorOverflowNegNeg (i, j))
   c -> ((i > 0) AND (j < 0) AND divFloorOverflowPosNeg (i, j))
   d -> ((i < 0) AND (j > 0) AND divFloorOverflowNegPos (i, j))

   RETURN a AND b AND c AND d.  */

static tree
divFloorOverflowCases (location_t location, tree i, tree j, tree lowest,
		      tree min, tree max)
{
  tree i_gt_zero = m2expr_BuildGreaterThanZero (location, i, lowest, min, max);
  tree j_gt_zero = m2expr_BuildGreaterThanZero (location, j, lowest, min, max);
  tree i_lt_zero = m2expr_BuildLessThanZero (location, i, lowest, min, max);
  tree j_lt_zero = m2expr_BuildLessThanZero (location, j, lowest, min, max);
  tree a = m2expr_Build3TruthAndIf (location, i_gt_zero, j_gt_zero,
				    divFloorOverflowPosPos (location, i, j, min));
  tree b = m2expr_Build3TruthAndIf (location, i_lt_zero, j_lt_zero,
				    divFloorOverflowNegNeg (location, i, j, lowest, min, max));
  tree c = m2expr_Build3TruthAndIf (location, i_gt_zero, j_lt_zero,
				    divFloorOverflowPosNeg (location, i, j, min));
  tree d = m2expr_Build3TruthAndIf (location, i_lt_zero, j_gt_zero,
				    divFloorOverflowNegPos (location, i, j, min));
  return m2expr_Build4TruthOrIf (location, a, b, c, d);
}


/* checkWholeDivFloorOverflow check to see whether i DIV_FLOOR j will overflow
   an integer.  A handbuilt expression of trees implementing:

   RETURN ((j = 0) OR     (* division by zero.  *)
           (maxT < 0) OR  (* both inputs are < 0 and max is < 0,
                             therefore error.  *)
                          (* we also check
                             if ISO M2 dialect and j < 0
                             which will also generate an error.  *)
           ((i # 0) AND   (* first operand is legally zero,
                             result is also legally zero.  *)
            divFloorOverflowCases (i, j)))

  using the following subexpressions:

   j_eq_zero -> (j == 0)
   max_lt_zero -> (max < 0)
   i_ne_zero -> (i # 0).  */

static tree
checkWholeDivFloorOverflow (location_t location, tree i, tree j, tree lowest,
			    tree min, tree max)
{
  tree j_eq_zero = m2expr_BuildEqualToZero (location, j, lowest, min, max);
  tree max_lt_zero = m2expr_BuildLessThanZero (location, max, lowest, min, max);
  tree i_ne_zero = m2expr_BuildNotEqualToZero (location, i, lowest, min, max);
  tree j_lt_zero;
  tree rhs = m2expr_BuildTruthAndIf (location,
				     i_ne_zero,
				     divFloorOverflowCases (location,
							    i, j, lowest, min, max));

  if (M2Options_GetISO ())
    /* ISO Modula-2 raises an exception if the right hand operand is < 0.  */
    j_lt_zero = m2expr_FoldAndStrip (m2expr_BuildLessThanZero (location, j, lowest, min, max));
  else
    j_lt_zero = m2expr_GetIntegerZero (location);
  j_eq_zero = m2expr_FoldAndStrip (j_eq_zero);
  max_lt_zero = m2expr_FoldAndStrip (max_lt_zero);
  i_ne_zero = m2expr_FoldAndStrip (i_ne_zero);
  rhs = m2expr_FoldAndStrip (rhs);

  tree condition = m2expr_Build4TruthOrIf (location, j_eq_zero, max_lt_zero, rhs, j_lt_zero);
  tree t = M2Range_BuildIfCallWholeHandlerLoc (location, condition,
					       get_current_function_name (),
               "whole value floor division will cause a range overflow");
  return t;
}

/* checkWholeOverflow check to see if the binary operators will overflow
   ordinal types.  */

static tree
m2expr_checkWholeOverflow (location_t location, enum tree_code code, tree op1,
                           tree op2, tree lowest, tree min, tree max)
{
  if (M2Options_GetWholeValueCheck () && (min != NULL))
    {
      lowest = m2tree_skip_type_decl (lowest);
      op1 = fold_convert_loc (location, lowest, op1);
      op2 = fold_convert_loc (location, lowest, op2);

      switch (code)
        {
        case PLUS_EXPR:
          return checkWholeAddOverflow (location, op1, op2, lowest, min, max);
        case MINUS_EXPR:
          return checkWholeSubOverflow (location, op1, op2, lowest, min, max);
        case MULT_EXPR:
          return checkWholeMultOverflow (location, op1, op2, lowest, min, max);
	case TRUNC_DIV_EXPR:
	  return checkWholeDivTruncOverflow (location, op1, op2, lowest, min, max);
	case CEIL_DIV_EXPR:
	  return checkWholeDivCeilOverflow (location, op1, op2, lowest, min, max);
	case FLOOR_DIV_EXPR:
	  return checkWholeDivFloorOverflow (location, op1, op2, lowest, min, max);
	case TRUNC_MOD_EXPR:
	  return checkWholeModTruncOverflow (location, op1, op2, lowest, min, max);
	case CEIL_MOD_EXPR:
	  return checkWholeModCeilOverflow (location, op1, op2, lowest, min, max);
	case FLOOR_MOD_EXPR:
	  return checkWholeModFloorOverflow (location, op1, op2, lowest, min, max);
        default:
	  return NULL;
        }
    }
  return NULL;
}

/* checkRealOverflow if we have enabled real value checking then
   generate an overflow check appropriate to the tree code being used.  */

static void
m2expr_checkRealOverflow (location_t location, enum tree_code code,
                          tree result)
{
  if (M2Options_GetFloatValueCheck ())
    {
      tree condition = m2expr_BuildEqualTo (
          location, m2builtins_BuiltInIsfinite (location, result),
          m2expr_GetIntegerZero (location));
      switch (code)
        {
        case PLUS_EXPR:
          m2type_AddStatement (location,
                               M2Range_BuildIfCallRealHandlerLoc (
				   location, condition,
				   get_current_function_name (),
		   "floating point + has caused an overflow"));
          break;
        case MINUS_EXPR:
          m2type_AddStatement (location,
                               M2Range_BuildIfCallRealHandlerLoc (
                                   location, condition,
				   get_current_function_name (),
		   "floating point - has caused an overflow"));
          break;
        case RDIV_EXPR:
        case FLOOR_DIV_EXPR:
        case CEIL_DIV_EXPR:
        case TRUNC_DIV_EXPR:
          m2type_AddStatement (location,
                               M2Range_BuildIfCallRealHandlerLoc (
                                   location, condition,
				   get_current_function_name (),
		   "floating point / has caused an overflow"));
          break;
        case MULT_EXPR:
          m2type_AddStatement (location,
                               M2Range_BuildIfCallRealHandlerLoc (
                                   location, condition,
				   get_current_function_name (),
		   "floating point * has caused an overflow"));
          break;
        case NEGATE_EXPR:
          m2type_AddStatement (
              location, M2Range_BuildIfCallRealHandlerLoc (
                            location, condition,
			    get_current_function_name (),
		   "floating point unary - has caused an overflow"));
        default:
          break;
        }
    }
}

/* build_binary_op, a wrapper for the lower level build_binary_op
   above.  */

tree
m2expr_build_binary_op_check (location_t location, enum tree_code code,
                              tree op1, tree op2, bool needconvert, tree lowest,
                              tree min, tree max)
{
  tree type1, type2, result;
  tree check = NULL;

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  type1 = m2tree_skip_type_decl (TREE_TYPE (op1));
  type2 = m2tree_skip_type_decl (TREE_TYPE (op2));

  m2assert_AssertLocation (location);

  if (code == PLUS_EXPR)
    {
      if (POINTER_TYPE_P (type1))
        {
          op2 = fold_convert_loc (location, sizetype, unshare_expr (op2));
          return fold_build2_loc (location, POINTER_PLUS_EXPR, TREE_TYPE (op1),
                                  op1, op2);
        }
      else if (POINTER_TYPE_P (type2))
        {
          op1 = fold_convert_loc (location, sizetype, unshare_expr (op1));
          return fold_build2_loc (location, POINTER_PLUS_EXPR, TREE_TYPE (op2),
                                  op2, op1);
        }
    }
  if (code == MINUS_EXPR)
    {
      if (POINTER_TYPE_P (type1))
        {
          op2 = fold_convert_loc (location, sizetype, unshare_expr (op2));
          op2 = fold_build1_loc (location, NEGATE_EXPR, sizetype, op2);
          return fold_build2_loc (location, POINTER_PLUS_EXPR, TREE_TYPE (op1),
                                  op1, op2);
        }
      else if (POINTER_TYPE_P (type2))
        {
          op2 = fold_convert_loc (location, sizetype, unshare_expr (op2));
          op2 = fold_build1_loc (location, NEGATE_EXPR, sizetype, op2);
          op1 = fold_convert_loc (location, sizetype, unshare_expr (op1));
          return fold_build2_loc (location, POINTER_PLUS_EXPR, TREE_TYPE (op2),
                                  op2, op1);
        }
    }

  if ((code != LSHIFT_EXPR) && (code != RSHIFT_EXPR) && (code != LROTATE_EXPR)
      && (code == RROTATE_EXPR))
    if (type1 != type2)
      error_at (location, "not expecting different types to binary operator");

  if ((TREE_CODE (type1) != REAL_TYPE) && (min != NULL))
    check = m2expr_checkWholeOverflow (location, code, op1, op2, lowest, min, max);

  result = build_binary_op (location, code, op1, op2, needconvert);
  if (check != NULL)
    result = build2 (COMPOUND_EXPR, TREE_TYPE (result), check, result);

  if (TREE_CODE (type1) == REAL_TYPE)
    m2expr_checkRealOverflow (location, code, result);
  return result;
}

/* build_binary_op, a wrapper for the lower level build_binary_op
   above.  */

tree
m2expr_build_binary_op (location_t location, enum tree_code code, tree op1,
                        tree op2, int convert)
{
  return m2expr_build_binary_op_check (location, code, op1, op2, convert, NULL,
                                       NULL, NULL);
}

/* BuildAddAddress return an expression op1+op2 where op1 is a
   pointer type and op2 is not a pointer type.  */

tree
m2expr_BuildAddAddress (location_t location, tree op1, tree op2)
{
  tree type1, type2;

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  type1 = m2tree_skip_type_decl (TREE_TYPE (op1));
  type2 = m2tree_skip_type_decl (TREE_TYPE (op2));

  m2assert_AssertLocation (location);
  ASSERT_CONDITION (POINTER_TYPE_P (type1));
  ASSERT_CONDITION (!POINTER_TYPE_P (type2));

  op2 = fold_convert_loc (location, sizetype, unshare_expr (op2));
  return fold_build2_loc (location, POINTER_PLUS_EXPR, TREE_TYPE (op1),
                          m2expr_FoldAndStrip (op1),
                          m2expr_FoldAndStrip (op2));
}

/* BuildNegateCheck builds a negate tree.  */

tree
m2expr_BuildNegateCheck (location_t location, tree arg, tree lowest, tree min,
                         tree max)
{
  tree t;

  m2assert_AssertLocation (location);

  arg = m2expr_FoldAndStrip (arg);
  arg = CheckAddressToCardinal (location, arg);

  t = m2expr_build_unary_op_check (location, NEGATE_EXPR, arg, lowest, min,
                                   max);
  return m2expr_FoldAndStrip (t);
}

/* BuildNegate build a negate expression and returns the tree.  */

tree
m2expr_BuildNegate (location_t location, tree op1, bool needconvert)
{
  m2assert_AssertLocation (location);
  op1 = m2expr_FoldAndStrip (op1);
  op1 = CheckAddressToCardinal (location, op1);

  return m2expr_build_unary_op (location, NEGATE_EXPR, op1, needconvert);
}

/* BuildSetNegate build a set negate expression and returns the tree.  */

tree
m2expr_BuildSetNegate (location_t location, tree op1, bool needconvert)
{
  m2assert_AssertLocation (location);

  return m2expr_build_binary_op (
      location, BIT_XOR_EXPR,
      m2convert_BuildConvert (location, m2type_GetWordType (),
                              m2expr_FoldAndStrip (op1), false),
      set_full_complement, needconvert);
}

/* BuildMult build a multiplication tree.  */

tree
m2expr_BuildMult (location_t location, tree op1, tree op2, bool needconvert)
{
  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  m2assert_AssertLocation (location);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  return m2expr_build_binary_op (location, MULT_EXPR, op1, op2, needconvert);
}

/* BuildMultCheck builds a multiplication tree.  */

tree
m2expr_BuildMultCheck (location_t location, tree op1, tree op2, tree lowest,
                       tree min, tree max)
{
  tree t;

  m2assert_AssertLocation (location);

  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);

  op1 = CheckAddressToCardinal (location, op1);
  op2 = CheckAddressToCardinal (location, op2);

  t = m2expr_build_binary_op_check (location, MULT_EXPR, op1, op2, false,
                                    lowest, min, max);
  return m2expr_FoldAndStrip (t);
}

/* testLimits return the number of bits required to represent:
   min..max if it matches the, type.  Otherwise NULL_TREE is returned.  */

static tree
testLimits (location_t location, tree type, tree min, tree max)
{
  m2assert_AssertLocation (location);

  if ((m2expr_CompareTrees (TYPE_MAX_VALUE (type), max) == 0)
      && (m2expr_CompareTrees (TYPE_MIN_VALUE (type), min) == 0))
    return m2expr_BuildMult (location, m2expr_GetSizeOf (location, type),
                             m2decl_BuildIntegerConstant (BITS_PER_UNIT),
                             false);
  return NULL_TREE;
}

/* noBitsRequired return the number of bits required to contain, values.  */

static tree
noBitsRequired (tree values)
{
  int bits = tree_floor_log2 (values);

  if (integer_pow2p (values))
    return m2decl_BuildIntegerConstant (bits + 1);
  else
    return m2decl_BuildIntegerConstant (bits + 1);
}

/* getMax return the result of max(a, b).  */

static tree
getMax (tree a, tree b)
{
  if (m2expr_CompareTrees (a, b) > 0)
    return a;
  else
    return b;
}

/* calcNbits return the smallest number of bits required to
   represent: min..max.  */

static tree
calcNbits (location_t location, tree min, tree max)
{
  int negative = false;
  tree t = testLimits (location, m2type_GetIntegerType (), min, max);

  m2assert_AssertLocation (location);

  if (t == NULL)
    t = testLimits (location, m2type_GetCardinalType (), min, max);

  if (t == NULL)
    {
      if (m2expr_CompareTrees (min, m2expr_GetIntegerZero (location)) < 0)
        {
          min = m2expr_BuildAdd (location, min,
                                 m2expr_GetIntegerOne (location), false);
          min = fold (m2expr_BuildNegate (location, min, false));
          negative = true;
        }
      if (m2expr_CompareTrees (max, m2expr_GetIntegerZero (location)) < 0)
        {
          max = fold (m2expr_BuildNegate (location, max, false));
          negative = true;
        }
      t = noBitsRequired (getMax (min, max));
      if (negative)
        t = m2expr_BuildAdd (location, t, m2expr_GetIntegerOne (location),
                             false);
    }
  return t;
}

/* BuildTBitSize return the minimum number of bits to represent, type.  */

tree
m2expr_BuildTBitSize (location_t location, tree type)
{
  enum tree_code code = TREE_CODE (type);
  tree min;
  tree max;
  m2assert_AssertLocation (location);

  switch (code)
    {

    case TYPE_DECL:
      return m2expr_BuildTBitSize (location, TREE_TYPE (type));
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      max = m2convert_BuildConvert (location, m2type_GetIntegerType (),
                                    TYPE_MAX_VALUE (type), false);
      min = m2convert_BuildConvert (location, m2type_GetIntegerType (),
                                    TYPE_MIN_VALUE (type), false);
      return calcNbits (location, min, max);
    case BOOLEAN_TYPE:
      return m2expr_GetIntegerOne (location);
    default:
      return m2expr_BuildMult (location, m2expr_GetSizeOf (location, type),
                               m2decl_BuildIntegerConstant (BITS_PER_UNIT),
                               false);
    }
}

/* BuildSize build a SIZE function expression and returns the tree.  */

tree
m2expr_BuildSize (location_t location, tree op1,
                  bool needconvert ATTRIBUTE_UNUSED)
{
  m2assert_AssertLocation (location);
  return m2expr_GetSizeOf (location, op1);
}

/* BuildAddr return an expression which calculates the address of op1
   and returns the tree.  If use_generic is true then create a generic
   pointer type.  */

tree
m2expr_BuildAddr (location_t location, tree op1, bool use_generic)
{
  tree type = m2tree_skip_type_decl (TREE_TYPE (op1));
  tree ptrType = build_pointer_type (type);
  tree result;

  m2assert_AssertLocation (location);

  if (!gm2_mark_addressable (op1))
    error_at (location, "cannot take the address of this expression");

  if (use_generic)
    result = build1 (ADDR_EXPR, m2type_GetPointerType (), op1);
  else
    result = build1 (ADDR_EXPR, ptrType, op1);
  protected_set_expr_location (result, location);
  return result;
}

/* BuildOffset1 build and return an expression containing the number
   of bytes the field is offset from the start of the record structure.
   This function is the same as the above, except that it derives the
   record from the field and then calls BuildOffset.  */

tree
m2expr_BuildOffset1 (location_t location, tree field,
                     bool needconvert ATTRIBUTE_UNUSED)
{
  m2assert_AssertLocation (location);
  return m2expr_BuildOffset (location, DECL_CONTEXT (field), field,
                             needconvert);
}

/* determinePenultimateField return the field associated with the
   DECL_CONTEXT (field) within a record or varient.  The record, is a
   record/varient but it maybe an outer nested record to the field that
   we are searching.  Ie:

   record = RECORD x: CARDINAL ; y: RECORD field: CARDINAL ; END END ;

   determinePenultimateField (record, field) returns, y.  We are
   assurred that the chain of records leading to field will be unique as
   they are built on the fly to implement varient records.  */

static tree
determinePenultimateField (tree record, tree field)
{
  tree fieldlist = TYPE_FIELDS (record);
  tree x, r;

  for (x = fieldlist; x; x = TREE_CHAIN (x))
    {
      if (DECL_CONTEXT (field) == TREE_TYPE (x))
        return x;
      switch (TREE_CODE (TREE_TYPE (x)))
        {
        case RECORD_TYPE:
        case UNION_TYPE:
          r = determinePenultimateField (TREE_TYPE (x), field);
          if (r != NULL)
            return r;
          break;
        default:
          break;
        }
    }
  return NULL_TREE;
}

/* BuildOffset builds an expression containing the number of bytes
the field is offset from the start of the record structure.  The
expression is returned.  */

tree
m2expr_BuildOffset (location_t location, tree record, tree field,
                    bool needconvert ATTRIBUTE_UNUSED)
{
  m2assert_AssertLocation (location);

  if (DECL_CONTEXT (field) == record)
    return m2convert_BuildConvert (
        location, m2type_GetIntegerType (),
        m2expr_BuildAdd (
            location, DECL_FIELD_OFFSET (field),
            m2expr_BuildDivTrunc (location, DECL_FIELD_BIT_OFFSET (field),
                                  m2decl_BuildIntegerConstant (BITS_PER_UNIT),
                                  false),
            false),
        false);
  else
    {
      tree r1 = DECL_CONTEXT (field);
      tree r2 = determinePenultimateField (record, field);
      return m2convert_BuildConvert (
          location, m2type_GetIntegerType (),
          m2expr_BuildAdd (
              location, m2expr_BuildOffset (location, r1, field, needconvert),
              m2expr_BuildOffset (location, record, r2, needconvert), false),
          false);
    }
}

/* BuildLogicalOrAddress build a logical or expressions and return the tree. */

tree
m2expr_BuildLogicalOrAddress (location_t location, tree op1, tree op2,
                              bool needconvert)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, BIT_IOR_EXPR, op1, op2,
                                 needconvert);
}

/* BuildLogicalOr build a logical or expressions and return the tree.  */

tree
m2expr_BuildLogicalOr (location_t location, tree op1, tree op2,
                       bool needconvert)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (
      location, BIT_IOR_EXPR,
      m2convert_BuildConvert (location, m2type_GetWordType (), op1, false),
      m2convert_BuildConvert (location, m2type_GetWordType (), op2, false),
      needconvert);
}

/* BuildLogicalAnd build a logical and expression and return the tree.  */

tree
m2expr_BuildLogicalAnd (location_t location, tree op1, tree op2,
                        bool needconvert)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (
      location, BIT_AND_EXPR,
      m2convert_BuildConvert (location, m2type_GetWordType (), op1, false),
      m2convert_BuildConvert (location, m2type_GetWordType (), op2, false),
      needconvert);
}

/* BuildSymmetricalDifference build a logical xor expression and return the
 * tree.  */

tree
m2expr_BuildSymmetricDifference (location_t location, tree op1, tree op2,
                                 bool needconvert)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (
      location, BIT_XOR_EXPR,
      m2convert_BuildConvert (location, m2type_GetWordType (), op1, false),
      m2convert_BuildConvert (location, m2type_GetWordType (), op2, false),
      needconvert);
}

/* BuildLogicalDifference build a logical difference expression and
return the tree.  (op1 and (not op2)).  */

tree
m2expr_BuildLogicalDifference (location_t location, tree op1, tree op2,
                               bool needconvert)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (
      location, BIT_AND_EXPR,
      m2convert_BuildConvert (location, m2type_GetWordType (), op1, false),
      m2expr_BuildSetNegate (location, op2, needconvert), needconvert);
}

/* base_type returns the base type of an ordinal subrange, or the
type itself if it is not a subrange.  */

static tree
base_type (tree type)
{
  if (type == error_mark_node)
    return error_mark_node;

  /* Check for ordinal subranges.  */
  if (m2tree_IsOrdinal (type) && TREE_TYPE (type))
    type = TREE_TYPE (type);
  return TYPE_MAIN_VARIANT (type);
}

/* boolean_enum_to_unsigned convert a BOOLEAN_TYPE, t, or
   ENUMERAL_TYPE to an unsigned type.  */

static tree
boolean_enum_to_unsigned (location_t location, tree t)
{
  tree type = TREE_TYPE (t);

  if (TREE_CODE (base_type (type)) == BOOLEAN_TYPE)
    return m2convert_BuildConvert (location, unsigned_type_node, t, false);
  else if (TREE_CODE (base_type (type)) == ENUMERAL_TYPE)
    return m2convert_BuildConvert (location, unsigned_type_node, t, false);
  else
    return t;
}

/* check_for_comparison check to see if, op, is of type, badType.  If
   so then it returns op after it has been cast to, goodType.  op will
   be an array so we take the address and cast the contents.  */

static tree
check_for_comparison (location_t location, tree op, tree badType,
                      tree goodType)
{
  m2assert_AssertLocation (location);
  if (m2tree_skip_type_decl (TREE_TYPE (op)) == badType)
    /* Cannot compare array contents in m2expr_build_binary_op.  */
    return m2expr_BuildIndirect (
        location, m2expr_BuildAddr (location, op, false), goodType);
  return op;
}

/* convert_for_comparison return a tree which can be used as an
   argument during a comparison.  */

static tree
convert_for_comparison (location_t location, tree op)
{
  m2assert_AssertLocation (location);
  op = boolean_enum_to_unsigned (location, op);

  op = check_for_comparison (location, op, m2type_GetISOWordType (),
                             m2type_GetWordType ());
  op = check_for_comparison (location, op, m2type_GetM2Word16 (),
                             m2type_GetM2Cardinal16 ());
  op = check_for_comparison (location, op, m2type_GetM2Word32 (),
                             m2type_GetM2Cardinal32 ());
  op = check_for_comparison (location, op, m2type_GetM2Word64 (),
                             m2type_GetM2Cardinal64 ());

  return op;
}

/* BuildLessThan return a tree which computes <.  */

tree
m2expr_BuildLessThan (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (
      location, LT_EXPR, boolean_enum_to_unsigned (location, op1),
      boolean_enum_to_unsigned (location, op2), true);
}

/* BuildGreaterThan return a tree which computes >.  */

tree
m2expr_BuildGreaterThan (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (
      location, GT_EXPR, boolean_enum_to_unsigned (location, op1),
      boolean_enum_to_unsigned (location, op2), true);
}

/* BuildLessThanOrEqual return a tree which computes <.  */

tree
m2expr_BuildLessThanOrEqual (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (
      location, LE_EXPR, boolean_enum_to_unsigned (location, op1),
      boolean_enum_to_unsigned (location, op2), true);
}

/* BuildGreaterThanOrEqual return a tree which computes >=.  */

tree
m2expr_BuildGreaterThanOrEqual (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (
      location, GE_EXPR, boolean_enum_to_unsigned (location, op1),
      boolean_enum_to_unsigned (location, op2), true);
}

/* BuildEqualTo return a tree which computes =.  */

tree
m2expr_BuildEqualTo (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, EQ_EXPR,
                                 convert_for_comparison (location, op1),
                                 convert_for_comparison (location, op2), true);
}

/* BuildEqualNotTo return a tree which computes #.  */

tree
m2expr_BuildNotEqualTo (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_build_binary_op (location, NE_EXPR,
                                 convert_for_comparison (location, op1),
                                 convert_for_comparison (location, op2), true);
}

/* BuildIsSuperset return a tree which computes:  op1 & op2 == op2.  */

tree
m2expr_BuildIsSuperset (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_BuildEqualTo (
      location, op2, m2expr_BuildLogicalAnd (location, op1, op2, false));
}

/* BuildIsNotSuperset return a tree which computes: op1 & op2 != op2.  */

tree
m2expr_BuildIsNotSuperset (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_BuildNotEqualTo (
      location, op2, m2expr_BuildLogicalAnd (location, op1, op2, false));
}

/* BuildIsSubset return a tree which computes:  op1 & op2 == op1.  */

tree
m2expr_BuildIsSubset (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_BuildEqualTo (
      location, op1, m2expr_BuildLogicalAnd (location, op1, op2, false));
}

/* BuildIsNotSubset return a tree which computes: op1 & op2 != op1.  */

tree
m2expr_BuildIsNotSubset (location_t location, tree op1, tree op2)
{
  m2assert_AssertLocation (location);
  return m2expr_BuildNotEqualTo (
      location, op1, m2expr_BuildLogicalAnd (location, op1, op2, false));
}

/* BuildIfConstInVar generates: if constel in varset then goto label.  */

void
m2expr_BuildIfConstInVar (location_t location, tree type, tree varset,
                          tree constel, bool is_lvalue, int fieldno,
                          char *label)
{
  tree size = m2expr_GetSizeOf (location, type);
  m2assert_AssertLocation (location);

  ASSERT_BOOL (is_lvalue);
  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    /* Small set size <= TSIZE(WORD).  */
    m2treelib_do_jump_if_bit (
        location, NE_EXPR,
        m2treelib_get_rvalue (location, varset, type, is_lvalue), constel,
        label);
  else
    {
      tree fieldlist = TYPE_FIELDS (type);
      tree field;

      for (field = fieldlist; (field != NULL) && (fieldno > 0);
           field = TREE_CHAIN (field))
        fieldno--;

      m2treelib_do_jump_if_bit (
          location, NE_EXPR,
          m2treelib_get_set_field_rhs (location, varset, field), constel,
          label);
    }
}

/* BuildIfConstInVar generates: if not (constel in varset) then goto label.  */

void
m2expr_BuildIfNotConstInVar (location_t location, tree type, tree varset,
                             tree constel, bool is_lvalue, int fieldno,
                             char *label)
{
  tree size = m2expr_GetSizeOf (location, type);

  m2assert_AssertLocation (location);

  ASSERT_BOOL (is_lvalue);
  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    /* Small set size <= TSIZE(WORD).  */
    m2treelib_do_jump_if_bit (
        location, EQ_EXPR,
        m2treelib_get_rvalue (location, varset, type, is_lvalue), constel,
        label);
  else
    {
      tree fieldlist = TYPE_FIELDS (type);
      tree field;

      for (field = fieldlist; (field != NULL) && (fieldno > 0);
           field = TREE_CHAIN (field))
        fieldno--;

      m2treelib_do_jump_if_bit (
          location, EQ_EXPR,
          m2treelib_get_set_field_rhs (location, varset, field), constel,
          label);
    }
}

/* BuildIfVarInVar generates: if varel in varset then goto label.  */

void
m2expr_BuildIfVarInVar (location_t location, tree type, tree varset,
                        tree varel, bool is_lvalue, tree low,
                        tree high ATTRIBUTE_UNUSED, char *label)
{
  tree size = m2expr_GetSizeOf (location, type);
  /* Calculate the index from the first bit, ie bit 0 represents low value.  */
  tree index = m2expr_BuildSub (
      location, m2convert_BuildConvert (location, m2type_GetIntegerType (),
                                        varel, false),
      m2convert_BuildConvert (location, m2type_GetIntegerType (), low, false),
      false);

  m2assert_AssertLocation (location);

  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    /* Small set size <= TSIZE(WORD).  */
    m2treelib_do_jump_if_bit (
        location, NE_EXPR,
        m2treelib_get_rvalue (location, varset, type, is_lvalue), index,
        label);
  else
    {
      tree p1 = m2treelib_get_set_address (location, varset, is_lvalue);
      /* Which word do we need to fetch?  */
      tree word_index = m2expr_FoldAndStrip (m2expr_BuildDivTrunc (
          location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE),
          false));
      /* Calculate the bit in this word.  */
      tree offset_into_word = m2expr_FoldAndStrip (m2expr_BuildModTrunc (
          location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE),
          false));
      tree p2 = m2expr_FoldAndStrip (m2expr_BuildMult (
          location, word_index,
          m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT), false));

      /* Calculate the address of the word we are interested in.  */
      p1 = m2expr_BuildAddAddress (location,
                                   m2convert_convertToPtr (location, p1), p2);

      /* Fetch the word, extract the bit and test for != 0.  */
      m2treelib_do_jump_if_bit (
          location, NE_EXPR,
          m2expr_BuildIndirect (location, p1, m2type_GetBitsetType ()),
          offset_into_word, label);
    }
}

/* BuildIfNotVarInVar generates: if not (varel in varset) then goto label.  */

void
m2expr_BuildIfNotVarInVar (location_t location, tree type, tree varset,
                           tree varel, bool is_lvalue, tree low,
                           tree high ATTRIBUTE_UNUSED, char *label)
{
  tree size = m2expr_GetSizeOf (location, type);
  /* Calculate the index from the first bit, ie bit 0 represents low value.  */
  tree index = m2expr_BuildSub (
      location, m2convert_BuildConvert (location, m2type_GetIntegerType (),
                                        m2expr_FoldAndStrip (varel), false),
      m2convert_BuildConvert (location, m2type_GetIntegerType (),
                              m2expr_FoldAndStrip (low), false),
      false);

  index = m2expr_FoldAndStrip (index);
  m2assert_AssertLocation (location);

  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    /* Small set size <= TSIZE(WORD).  */
    m2treelib_do_jump_if_bit (
        location, EQ_EXPR,
        m2treelib_get_rvalue (location, varset, type, is_lvalue), index,
        label);
  else
    {
      tree p1 = m2treelib_get_set_address (location, varset, is_lvalue);
      /* Calculate the index from the first bit.  */

      /* Which word do we need to fetch?  */
      tree word_index = m2expr_FoldAndStrip (m2expr_BuildDivTrunc (
          location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE),
          false));
      /* Calculate the bit in this word.  */
      tree offset_into_word = m2expr_FoldAndStrip (m2expr_BuildModTrunc (
          location, index, m2decl_BuildIntegerConstant (SET_WORD_SIZE),
          false));
      tree p2 = m2expr_FoldAndStrip (m2expr_BuildMult (
          location, word_index,
          m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT), false));

      /* Calculate the address of the word we are interested in.  */
      p1 = m2expr_BuildAddAddress (location, p1, p2);

      /* Fetch the word, extract the bit and test for == 0.  */
      m2treelib_do_jump_if_bit (
          location, EQ_EXPR,
          m2expr_BuildIndirect (location, p1, m2type_GetBitsetType ()),
          offset_into_word, label);
    }
}

/* BuildForeachWordInSetDoIfExpr foreach word in set, type, compute
   the expression, expr, and if true goto label.  */

void
m2expr_BuildForeachWordInSetDoIfExpr (location_t location, tree type, tree op1,
                                      tree op2, bool is_op1lvalue,
                                      bool is_op2lvalue, bool is_op1const,
                                      bool is_op2const,
                                      tree (*expr) (location_t, tree, tree),
                                      char *label)
{
  tree p1 = m2treelib_get_set_address_if_var (location, op1, is_op1lvalue,
                                              is_op1const);
  tree p2 = m2treelib_get_set_address_if_var (location, op2, is_op2lvalue,
                                              is_op2const);
  unsigned int fieldNo = 0;
  tree field1 = m2treelib_get_field_no (type, op1, is_op1const, fieldNo);
  tree field2 = m2treelib_get_field_no (type, op2, is_op2const, fieldNo);

  m2assert_AssertLocation (location);
  ASSERT_CONDITION (TREE_CODE (TREE_TYPE (op1)) == RECORD_TYPE);
  ASSERT_CONDITION (TREE_CODE (TREE_TYPE (op2)) == RECORD_TYPE);

  while (field1 != NULL && field2 != NULL)
    {
      m2statement_DoJump (
          location,
          (*expr) (location,
                   m2treelib_get_set_value (location, p1, field1, is_op1const,
                                            is_op1lvalue, op1, fieldNo),
                   m2treelib_get_set_value (location, p2, field2, is_op2const,
                                            is_op2lvalue, op2, fieldNo)),
          NULL, label);
      fieldNo++;
      field1 = m2treelib_get_field_no (type, op1, is_op1const, fieldNo);
      field2 = m2treelib_get_field_no (type, op2, is_op2const, fieldNo);
    }
}

/* BuildIfInRangeGoto returns a tree containing if var is in the
   range low..high then goto label.  */

void
m2expr_BuildIfInRangeGoto (location_t location, tree var, tree low, tree high,
                           char *label)
{
  m2assert_AssertLocation (location);

  if (m2expr_CompareTrees (low, high) == 0)
    m2statement_DoJump (location, m2expr_BuildEqualTo (location, var, low),
                        NULL, label);
  else
    m2statement_DoJump (
        location,
        m2expr_build_binary_op (
            location, TRUTH_ANDIF_EXPR,
            m2expr_BuildGreaterThanOrEqual (location, var, low),
            m2expr_BuildLessThanOrEqual (location, var, high), false),
        NULL, label);
}

/* BuildIfNotInRangeGoto returns a tree containing if var is not in
   the range low..high then goto label.  */

void
m2expr_BuildIfNotInRangeGoto (location_t location, tree var, tree low,
                              tree high, char *label)
{
  m2assert_AssertLocation (location);

  if (m2expr_CompareTrees (low, high) == 0)
    m2statement_DoJump (location, m2expr_BuildNotEqualTo (location, var, low),
                        NULL, label);
  else
    m2statement_DoJump (
        location, m2expr_build_binary_op (
                      location, TRUTH_ORIF_EXPR,
                      m2expr_BuildLessThan (location, var, low),
                      m2expr_BuildGreaterThan (location, var, high), false),
        NULL, label);
}

/* BuildArray - returns a tree which accesses array[index] given,
   lowIndice.  */

tree
m2expr_BuildArray (location_t location, tree type, tree array, tree index,
                   tree low_indice)
{
  tree array_type = m2tree_skip_type_decl (TREE_TYPE (array));
  tree index_type = TYPE_DOMAIN (array_type);
  type = m2tree_skip_type_decl (type);
// ASSERT_CONDITION (low_indice == TYPE_MIN_VALUE (index_type));

  low_indice
      = m2convert_BuildConvert (location, index_type, low_indice, false);
  return build4_loc (location, ARRAY_REF, type, array, index, low_indice,
                     NULL_TREE);
}

/* BuildComponentRef - build a component reference tree which
   accesses record.field.  If field does not belong to record it
   calls BuildComponentRef on the penultimate field.  */

tree
m2expr_BuildComponentRef (location_t location, tree record, tree field)
{
  tree recordType = m2tree_skip_reference_type (
      m2tree_skip_type_decl (TREE_TYPE (record)));

  if (DECL_CONTEXT (field) == recordType)
    return build3 (COMPONENT_REF, TREE_TYPE (field), record, field, NULL_TREE);
  else
    {
      tree f = determinePenultimateField (recordType, field);
      return m2expr_BuildComponentRef (
          location, m2expr_BuildComponentRef (location, record, f), field);
    }
}

/* BuildIndirect - build: (*target) given that the object to be
   copied is of, type.  */

tree
m2expr_BuildIndirect (location_t location ATTRIBUTE_UNUSED, tree target,
                      tree type)
{
  /* Note that the second argument to build1 is:

     TYPE_QUALS is a list of modifiers such as const or volatile to apply
     to the pointer type, represented as identifiers.

     it also determines the type of arithmetic and size of the object to
     be indirectly moved.  */

  tree t1 = m2tree_skip_type_decl (type);
  tree t2 = build_pointer_type (t1);

  m2assert_AssertLocation (location);

  return build1 (INDIRECT_REF, t1,
                 m2convert_BuildConvert (location, t2, target, false));
}

/* IsTrue - returns true if, t, is known to be true.  */

bool
m2expr_IsTrue (tree t)
{
  return (m2expr_FoldAndStrip (t) == m2type_GetBooleanTrue ());
}

/* IsFalse - returns false if, t, is known to be false.  */

bool
m2expr_IsFalse (tree t)
{
  return (m2expr_FoldAndStrip (t) == m2type_GetBooleanFalse ());
}

/* AreConstantsEqual - maps onto tree.cc (tree_int_cst_equal).  It
   returns true if the value of e1 is the same as e2.  */

bool
m2expr_AreConstantsEqual (tree e1, tree e2)
{
  return tree_int_cst_equal (e1, e2) != 0;
}

/* AreRealOrComplexConstantsEqual - returns true if constants, e1 and
   e2 are equal according to IEEE rules.  This does not perform bit
   equivalence for example IEEE states that -0 == 0 and NaN != NaN.  */

bool
m2expr_AreRealOrComplexConstantsEqual (tree e1, tree e2)
{
  if (TREE_CODE (e1) == COMPLEX_CST)
    return (m2expr_AreRealOrComplexConstantsEqual (TREE_REALPART (e1),
                                                   TREE_REALPART (e2))
            && m2expr_AreRealOrComplexConstantsEqual (TREE_IMAGPART (e1),
                                                      TREE_IMAGPART (e2)));
  else
    return real_compare (EQ_EXPR, &TREE_REAL_CST (e1), &TREE_REAL_CST (e2));
}

/* DetermineSign, returns -1 if e<0 0 if e==0 1 if e>0
   an unsigned constant will never return -1.  */

int
m2expr_DetermineSign (tree e)
{
  return tree_int_cst_sgn (e);
}

/* Similar to build_int_2 () but allows you to specify the type of
   the integer constant that you are creating.  */

static tree
build_int_2_type (HOST_WIDE_INT low, HOST_WIDE_INT hi, tree type)
{
  tree value;
  HOST_WIDE_INT ival[3];

  ival[0] = low;
  ival[1] = hi;
  ival[2] = 0;

  widest_int wval = widest_int::from_array (ival, 3);
  value = wide_int_to_tree (type, wval);

  return value;
}

/* BuildCap - builds the Modula-2 function CAP(t) and returns the
   result in a gcc Tree.  */

tree
m2expr_BuildCap (location_t location, tree t)
{
  tree tt;
  tree out_of_range, less_than, greater_than, translated;

  m2assert_AssertLocation (location);

  t = fold (t);
  if (t == error_mark_node)
    return error_mark_node;

  tt = TREE_TYPE (t);

  t = fold (convert (m2type_GetM2CharType (), t));

  if (TREE_CODE (tt) == INTEGER_TYPE)
    {
      less_than = fold (m2expr_build_binary_op (
          location, LT_EXPR, t,
          build_int_2_type ('a', 0, m2type_GetM2CharType ()), 0));
      greater_than = fold (m2expr_build_binary_op (
          location, GT_EXPR, t,
          build_int_2_type ('z', 0, m2type_GetM2CharType ()), 0));
      out_of_range = fold (m2expr_build_binary_op (
          location, TRUTH_ORIF_EXPR, less_than, greater_than, 0));

      translated = fold (convert (
          m2type_GetM2CharType (),
          m2expr_build_binary_op (
              location, MINUS_EXPR, t,
              build_int_2_type ('a' - 'A', 0, m2type_GetM2CharType ()), 0)));

      return fold_build3 (COND_EXPR, m2type_GetM2CharType (), out_of_range, t,
                          translated);
    }

  error_at (location,
            "argument to CAP is not a constant or variable of type CHAR");
  return error_mark_node;
}

/* BuildDivM2 if iso or pim4 then all modulus results are positive
   and the results from the division are rounded to the floor otherwise
   use BuildDivTrunc.  */

tree
m2expr_BuildDivM2 (location_t location, tree op1, tree op2,
                   bool needsconvert)
{
  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);
  ASSERT_CONDITION (TREE_TYPE (op1) == TREE_TYPE (op2));
  /* If iso or pim4 then build and return ((op2 < 0) ? (op1
     divceil op2) : (op1 divfloor op2)) otherwise use divtrunc.  */
  if (M2Options_GetPIM4 () || M2Options_GetISO ()
      || M2Options_GetPositiveModFloor ())
    return fold_build3 (
        COND_EXPR, TREE_TYPE (op1),
        m2expr_BuildLessThan (
            location, op2,
            m2convert_BuildConvert (location, TREE_TYPE (op2),
                                    m2expr_GetIntegerZero (location), false)),
        m2expr_BuildDivCeil (location, op1, op2, needsconvert),
        m2expr_BuildDivFloor (location, op1, op2, needsconvert));
  else
    return m2expr_BuildDivTrunc (location, op1, op2, needsconvert);
}

/* BuildDivM2Check - build and
   return ((op2 < 0) ? (op1 divtrunc op2) : (op1 divfloor op2))
   when -fiso, -fpim4 or -fpositive-mod-floor-div is present else
   return op1 div trunc op2.  Use the checking div equivalents.  */

tree
m2expr_BuildDivM2Check (location_t location, tree op1, tree op2,
			tree lowest, tree min, tree max)
{
  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);
  ASSERT_CONDITION (TREE_TYPE (op1) == TREE_TYPE (op2));
  if (M2Options_GetISO ()
      || M2Options_GetPIM4 () || M2Options_GetPositiveModFloor ())
    return fold_build3 (
        COND_EXPR, TREE_TYPE (op1),
        m2expr_BuildLessThan (
            location, op2,
            m2convert_BuildConvert (location, TREE_TYPE (op2),
                                    m2expr_GetIntegerZero (location), false)),
        m2expr_BuildDivCeilCheck (location, op1, op2, lowest, min, max),
        m2expr_BuildDivFloorCheck (location, op1, op2, lowest, min, max));
  else
    return m2expr_BuildDivTruncCheck (location, op1, op2, lowest, min, max);
}

static
tree
m2expr_BuildISOModM2Check (location_t location,
			   tree op1, tree op2, tree lowest, tree min, tree max)
{
  tree cond = m2expr_BuildLessThan (location, op2,
				    m2convert_BuildConvert (location, TREE_TYPE (op2),
							    m2expr_GetIntegerZero (location), false));

  /* Return the result of the modulus.  */
  return fold_build3 (COND_EXPR, TREE_TYPE (op1), cond,
		      /* op2 < 0.  */
		      m2expr_BuildModCeilCheck (location, op1, op2, lowest, min, max),
		      /* op2 >= 0.  */
		      m2expr_BuildModFloorCheck (location, op1, op2, lowest, min, max));
}


/* BuildModM2Check if iso or pim4 then build and return ((op2 < 0) ? (op1
   modceil op2) :  (op1 modfloor op2)) otherwise use modtrunc.
   Use the checking mod equivalents.  */

tree
m2expr_BuildModM2Check (location_t location, tree op1, tree op2,
			tree lowest, tree min, tree max)
{
  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);
  ASSERT_CONDITION (TREE_TYPE (op1) == TREE_TYPE (op2));
  if (M2Options_GetPIM4 () || M2Options_GetISO ()
      || M2Options_GetPositiveModFloor ())
    return m2expr_BuildISOModM2Check (location, op1, op2, lowest, min, max);
  else
    return m2expr_BuildModTruncCheck (location, op1, op2, lowest, min, max);
}

/* BuildModM2 if iso or pim4 then build and return ((op2 < 0) ? (op1
   modceil op2) : (op1 modfloor op2)) otherwise use modtrunc.  */

tree
m2expr_BuildModM2 (location_t location, tree op1, tree op2,
                   bool needsconvert)
{
  op1 = m2expr_FoldAndStrip (op1);
  op2 = m2expr_FoldAndStrip (op2);
  ASSERT_CONDITION (TREE_TYPE (op1) == TREE_TYPE (op2));
  if (M2Options_GetPIM4 () || M2Options_GetISO ()
      || M2Options_GetPositiveModFloor ())
    return fold_build3 (
        COND_EXPR, TREE_TYPE (op1),
        m2expr_BuildLessThan (
            location, op2,
            m2convert_BuildConvert (location, TREE_TYPE (op2),
                                    m2expr_GetIntegerZero (location), false)),
        m2expr_BuildModCeil (location, op1, op2, needsconvert),
        m2expr_BuildModFloor (location, op1, op2, needsconvert));
  else
    return m2expr_BuildModTrunc (location, op1, op2, needsconvert);
}

/* BuildAbs build the Modula-2 function ABS(t) and return the result
   in a gcc Tree.  */

tree
m2expr_BuildAbs (location_t location, tree t)
{
  m2assert_AssertLocation (location);

  return m2expr_build_unary_op (location, ABS_EXPR, t, 0);
}

/* BuildRe build an expression for the function RE.  */

tree
m2expr_BuildRe (tree op1)
{
  op1 = m2expr_FoldAndStrip (op1);
  if (TREE_CODE (op1) == COMPLEX_CST)
    return fold_build1 (REALPART_EXPR, TREE_TYPE (TREE_TYPE (op1)), op1);
  else
    return build1 (REALPART_EXPR, TREE_TYPE (TREE_TYPE (op1)), op1);
}

/* BuildIm build an expression for the function IM.  */

tree
m2expr_BuildIm (tree op1)
{
  op1 = m2expr_FoldAndStrip (op1);
  if (TREE_CODE (op1) == COMPLEX_CST)
    return fold_build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (op1)), op1);
  else
    return build1 (IMAGPART_EXPR, TREE_TYPE (TREE_TYPE (op1)), op1);
}

/* BuildCmplx build an expression for the function CMPLX.  */

tree
m2expr_BuildCmplx (location_t location, tree type, tree real, tree imag)
{
  tree scalor;
  real = m2expr_FoldAndStrip (real);
  imag = m2expr_FoldAndStrip (imag);
  type = m2tree_skip_type_decl (type);
  scalor = TREE_TYPE (type);

  if (scalor != TREE_TYPE (real))
    real = m2convert_BuildConvert (location, scalor, real, false);
  if (scalor != TREE_TYPE (imag))
    imag = m2convert_BuildConvert (location, scalor, imag, false);

  if ((TREE_CODE (real) == REAL_CST) && (TREE_CODE (imag) == REAL_CST))
    return build_complex (type, real, imag);
  else
    return build2 (COMPLEX_EXPR, type, real, imag);
}

/* BuildBinaryForeachWordDo implements the large set operators.  Each
   word of the set can be calculated by binop.  This function runs along
   each word of the large set invoking the binop.  */

void
m2expr_BuildBinaryForeachWordDo (location_t location, tree type, tree op1,
                                 tree op2, tree op3,
                                 tree (*binop) (location_t, tree, tree, bool),
                                 bool is_op1lvalue, bool is_op2lvalue,
                                 bool is_op3lvalue, bool is_op1const,
                                 bool is_op2const, bool is_op3const)
{
  tree size = m2expr_GetSizeOf (location, type);

  m2assert_AssertLocation (location);

  ASSERT_BOOL (is_op1lvalue);
  ASSERT_BOOL (is_op2lvalue);
  ASSERT_BOOL (is_op3lvalue);
  ASSERT_BOOL (is_op1const);
  ASSERT_BOOL (is_op2const);
  ASSERT_BOOL (is_op3const);
  if (m2expr_CompareTrees (
          size, m2decl_BuildIntegerConstant (SET_WORD_SIZE / BITS_PER_UNIT))
      <= 0)
    /* Small set size <= TSIZE(WORD).  */
    m2statement_BuildAssignmentTree (
        location, m2treelib_get_rvalue (location, op1, type, is_op1lvalue),
        (*binop) (
            location, m2treelib_get_rvalue (location, op2, type, is_op2lvalue),
            m2treelib_get_rvalue (location, op3, type, is_op3lvalue), false));
  else
    {
      /* Large set size > TSIZE(WORD).  */

      tree p2 = m2treelib_get_set_address_if_var (location, op2, is_op2lvalue,
                                                  is_op2const);
      tree p3 = m2treelib_get_set_address_if_var (location, op3, is_op3lvalue,
                                                  is_op3const);
      unsigned int fieldNo = 0;
      tree field1 = m2treelib_get_field_no (type, op1, is_op1const, fieldNo);
      tree field2 = m2treelib_get_field_no (type, op2, is_op2const, fieldNo);
      tree field3 = m2treelib_get_field_no (type, op3, is_op3const, fieldNo);

      if (is_op1const)
	m2linemap_internal_error_at (
            location,
            "not expecting operand1 to be a constant set");

      while (field1 != NULL && field2 != NULL && field3 != NULL)
        {
          m2statement_BuildAssignmentTree (
              location, m2treelib_get_set_field_des (location, op1, field1),
              (*binop) (
                  location,
                  m2treelib_get_set_value (location, p2, field2, is_op2const,
                                           is_op2lvalue, op2, fieldNo),
                  m2treelib_get_set_value (location, p3, field3, is_op3const,
                                           is_op3lvalue, op3, fieldNo),
                  false));
          fieldNo++;
          field1 = m2treelib_get_field_no (type, op1, is_op1const, fieldNo);
          field2 = m2treelib_get_field_no (type, op2, is_op2const, fieldNo);
          field3 = m2treelib_get_field_no (type, op3, is_op3const, fieldNo);
        }
    }
}

/* Append DIGIT to NUM, a number of PRECISION bits being read in base
   BASE.  */

static int
append_digit (location_t location,
	      unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high,
              unsigned int digit, unsigned int base)
{
  unsigned int shift;
  int overflow;
  HOST_WIDE_INT add_high, res_high, test_high;
  unsigned HOST_WIDE_INT add_low, res_low, test_low;

  switch (base)
    {

    case 2:
      shift = 1;
      break;
    case 8:
      shift = 3;
      break;
    case 10:
      shift = 3;
      break;
    case 16:
      shift = 4;
      break;

    default:
      shift = 3;
      m2linemap_internal_error_at (location,
				   "not expecting this base value for a constant");
    }

  /* Multiply by 2, 8 or 16.  Catching this overflow here means we
     don't need to worry about add_high overflowing.  */
  if (((*high) >> (INT_TYPE_SIZE - shift)) == 0)
    overflow = false;
  else
    overflow = true;

  res_high = *high << shift;
  res_low = *low << shift;
  res_high |= (*low) >> (INT_TYPE_SIZE - shift);

  if (base == 10)
    {
      add_low = (*low) << 1;
      add_high = ((*high) << 1) + ((*low) >> (INT_TYPE_SIZE - 1));
    }
  else
    add_high = add_low = 0;

  test_low = add_low + digit;
  if (test_low < add_low)
    add_high++;
  add_low += digit;

  test_low = res_low + add_low;
  if (test_low < res_low)
    add_high++;
  test_high = res_high + add_high;
  if (test_high < res_high)
    overflow = true;

  *low = res_low + add_low;
  *high = res_high + add_high;

  return overflow;
}

/* interpret_integer convert an integer constant into two integer
   constants.  Heavily borrowed from gcc/cppexp.cc.  */

int
m2expr_interpret_integer (location_t location, const char *str, unsigned int base,
                          unsigned HOST_WIDE_INT *low, HOST_WIDE_INT *high)
{
  unsigned const char *p, *end;
  int overflow = false;
  int len;

  *low = 0;
  *high = 0;
  p = (unsigned const char *)str;
  len = strlen (str);
  end = p + len;

  /* Common case of a single digit.  */
  if (len == 1)
    *low = p[0] - '0';
  else
    {
      unsigned int c = 0;

      /* We can add a digit to numbers strictly less than this without
	 needing the precision and slowness of double integers.  */

      unsigned HOST_WIDE_INT max = ~(unsigned HOST_WIDE_INT)0;
      max = (max - base + 1) / base + 1;

      for (; p < end; p++)
        {
          c = *p;

          if (ISDIGIT (c) || (base == 16 && ISXDIGIT (c)))
            c = hex_value (c);
          else
            return overflow;

          /* Strict inequality for when max is set to zero.  */
          if (*low < max)
            *low = (*low) * base + c;
          else
            {
              overflow = append_digit (location, low, high, c, base);
              max = 0;  /* From now on we always use append_digit.  */
            }
        }
    }
  return overflow;
}

/* Append DIGIT to NUM, a number of PRECISION bits being read in base
   BASE.  */

static int
append_m2_digit (location_t location,
		 unsigned int *low, int *high, unsigned int digit,
                 unsigned int base, bool *needsUnsigned)
{
  unsigned int shift;
  bool overflow;
  int add_high, res_high, test_high;
  unsigned int add_low, res_low, test_low;
  unsigned int add_uhigh, res_uhigh, test_uhigh;

  switch (base)
    {

    case 2:
      shift = 1;
      break;
    case 8:
      shift = 3;
      break;
    case 10:
      shift = 3;
      break;
    case 16:
      shift = 4;
      break;

    default:
      shift = 3;
      m2linemap_internal_error_at (location,
				   "not expecting this base value for a constant");
    }

  /* Multiply by 2, 8 or 16.  Catching this overflow here means we
     don't need to worry about add_high overflowing.  */
  if (((*high) >> (INT_TYPE_SIZE - shift)) == 0)
    overflow = false;
  else
    overflow = true;

  res_high = *high << shift;
  res_low = *low << shift;
  res_high |= (*low) >> (INT_TYPE_SIZE - shift);

  if (base == 10)
    {
      add_low = (*low) << 1;
      add_high = ((*high) << 1) + ((*low) >> (INT_TYPE_SIZE - 1));
    }
  else
    add_high = add_low = 0;

  test_low = add_low + digit;
  if (test_low < add_low)
    add_high++;
  add_low += digit;

  test_low = res_low + add_low;
  if (test_low < res_low)
    add_high++;
  test_high = res_high + add_high;
  if (test_high < res_high)
    {
      res_uhigh = res_high;
      add_uhigh = add_high;
      test_uhigh = res_uhigh + add_uhigh;
      if (test_uhigh < res_uhigh)
	overflow = true;
      else
	*needsUnsigned = true;
    }

  *low = res_low + add_low;
  *high = res_high + add_high;

  return overflow;
}

/* interpret_m2_integer convert an integer constant into two integer
   constants.  Heavily borrowed from gcc/cppexp.cc.  Note that this is a
   copy of the above code except that it uses `int' rather than
   HOST_WIDE_INT to allow gm2 to determine what Modula-2 base type to
   use for this constant and it also sets needsLong and needsUnsigned
   if an overflow can be avoided by using these techniques.  */

int
m2expr_interpret_m2_integer (location_t location,
			     const char *str, unsigned int base,
                             unsigned int *low, int *high,
			     bool *needsLong, bool *needsUnsigned)
{
  const unsigned char *p, *end;
  int len;
  *needsLong = false;
  *needsUnsigned = false;

  *low = 0;
  *high = 0;
  p = (unsigned const char *)str;
  len = strlen (str);
  end = p + len;

  /* Common case of a single digit.  */
  if (len == 1)
    *low = p[0] - '0';
  else
    {
      unsigned int c = 0;

      /* We can add a digit to numbers strictly less than this without
	 needing the precision and slowness of double integers.  */

      unsigned int max = ~(unsigned int)0;
      max = (max - base + 1) / base + 1;

      for (; p < end; p++)
        {
          c = *p;

          if (ISDIGIT (c) || (base == 16 && ISXDIGIT (c)))
            c = hex_value (c);
          else
            return false;  /* End of string and no overflow found.  */

          /* Strict inequality for when max is set to zero.  */
          if (*low < max)
            *low = (*low) * base + c;
          else
            {
	      *needsLong = true;
	      if (append_m2_digit (location,
				   low, high, c, base,
				   needsUnsigned))
		return true;  /* We have overflowed so bail out.  */
              max = 0;  /* From now on we always use append_digit.  */
            }
        }
    }
  return false;
}

/* GetSizeOfInBits return the number of bits used to contain, type.  */

tree
m2expr_GetSizeOfInBits (tree type)
{
  enum tree_code code = TREE_CODE (type);

  if (code == FUNCTION_TYPE)
    return m2expr_GetSizeOfInBits (ptr_type_node);

  if (code == VOID_TYPE)
    {
      error ("%qs applied to a void type", "sizeof");
      return size_one_node;
    }

  if (code == VAR_DECL)
    return m2expr_GetSizeOfInBits (TREE_TYPE (type));

  if (code == PARM_DECL)
    return m2expr_GetSizeOfInBits (TREE_TYPE (type));

  if (code == TYPE_DECL)
    return m2expr_GetSizeOfInBits (TREE_TYPE (type));

  if (code == COMPONENT_REF)
    return m2expr_GetSizeOfInBits (TREE_TYPE (type));

  if (code == ERROR_MARK)
    return size_one_node;

  if (!COMPLETE_TYPE_P (type))
    {
      error ("%qs applied to an incomplete type", "sizeof");
      return size_zero_node;
    }

  return m2decl_BuildIntegerConstant (TYPE_PRECISION (type));
}

/* GetSizeOf taken from c-typeck.cc (c_sizeof).  */

tree
m2expr_GetSizeOf (location_t location, tree type)
{
  enum tree_code code = TREE_CODE (type);
  m2assert_AssertLocation (location);

  if (code == FUNCTION_TYPE)
    return m2expr_GetSizeOf (location, m2type_GetPointerType ());

  if (code == VOID_TYPE)
    return size_one_node;

  if (code == VAR_DECL)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (code == PARM_DECL)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (code == TYPE_DECL)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (code == ERROR_MARK)
    return size_one_node;

  if (code == CONSTRUCTOR)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (code == FIELD_DECL)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (code == COMPONENT_REF)
    return m2expr_GetSizeOf (location, TREE_TYPE (type));

  if (!COMPLETE_TYPE_P (type))
    {
      error_at (location, "%qs applied to an incomplete type", "SIZE");
      return size_zero_node;
    }

  /* Convert in case a char is more than one unit.  */
  return size_binop_loc (
      location, CEIL_DIV_EXPR, TYPE_SIZE_UNIT (type),
      size_int (TYPE_PRECISION (char_type_node) / BITS_PER_UNIT));
}

tree
m2expr_GetIntegerZero (location_t location ATTRIBUTE_UNUSED)
{
  return integer_zero_node;
}

tree
m2expr_GetIntegerOne (location_t location ATTRIBUTE_UNUSED)
{
  return integer_one_node;
}

tree
m2expr_GetCardinalOne (location_t location)
{
  return m2convert_ToCardinal (location, integer_one_node);
}

tree
m2expr_GetCardinalZero (location_t location)
{
  return m2convert_ToCardinal (location, integer_zero_node);
}

tree
m2expr_GetWordZero (location_t location)
{
  return m2convert_ToWord (location, integer_zero_node);
}

tree
m2expr_GetWordOne (location_t location)
{
  return m2convert_ToWord (location, integer_one_node);
}

tree
m2expr_GetPointerZero (location_t location)
{
  return m2convert_convertToPtr (location, integer_zero_node);
}

tree
m2expr_GetPointerOne (location_t location)
{
  return m2convert_convertToPtr (location, integer_one_node);
}

/* build_set_full_complement return a word size value with all bits
set to one.  */

static tree
build_set_full_complement (location_t location)
{
  tree value = integer_zero_node;
  int i;

  m2assert_AssertLocation (location);

  for (i = 0; i < SET_WORD_SIZE; i++)
    {
      value = m2expr_BuildLogicalOr (
          location, value,
          m2expr_BuildLSL (
              location, m2expr_GetWordOne (location),
              m2convert_BuildConvert (location, m2type_GetWordType (),
                                      m2decl_BuildIntegerConstant (i), false),
              false),
          false);
    }
  return value;
}


/* GetCstInteger return the integer value of the cst tree.  */

int
m2expr_GetCstInteger (tree cst)
{
  return TREE_INT_CST_LOW (cst);
}


/* init initialise this module.  */

void
m2expr_init (location_t location)
{
  m2assert_AssertLocation (location);

  set_full_complement = build_set_full_complement (location);
}

#include "gt-m2-m2expr.h"
