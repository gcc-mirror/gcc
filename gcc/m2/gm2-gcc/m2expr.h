/* m2expr.h header file for m2expr.cc.

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

#if !defined(m2expr_h)
#define m2expr_h
#if defined(m2expr_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2expr_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* !m2expr_c.  */


EXTERN char m2expr_CSTIntToChar (tree t);
EXTERN char *m2expr_CSTIntToString (tree t);
EXTERN bool m2expr_StrToWideInt (location_t location, const char *str, unsigned int base,
				 widest_int &wval, bool issueError);
EXTERN void m2expr_BuildBinaryForeachWordDo (
    location_t location, tree type, tree op1, tree op2, tree op3,
    tree (*binop) (location_t, tree, tree, bool), bool is_op1lvalue,
    bool is_op2lvalue, bool is_op3lvalue, bool is_op1const, bool is_op2const,
    bool is_op3const);
EXTERN tree m2expr_BuildCmplx (location_t location, tree type, tree real,
                               tree imag);
EXTERN tree m2expr_BuildIm (tree op1);
EXTERN tree m2expr_BuildRe (tree op1);
EXTERN tree m2expr_BuildAbs (location_t location, tree t);
EXTERN tree m2expr_BuildCap (location_t location, tree t);
EXTERN int m2expr_DetermineSign (tree e);
EXTERN bool m2expr_AreRealOrComplexConstantsEqual (tree e1, tree e2);
EXTERN bool m2expr_AreConstantsEqual (tree e1, tree e2);
EXTERN bool m2expr_IsFalse (tree t);
EXTERN bool m2expr_IsTrue (tree t);
EXTERN tree m2expr_BuildIndirect (location_t location, tree target, tree type);
EXTERN tree m2expr_BuildComponentRef (location_t location, tree record,
                                      tree field);
EXTERN tree m2expr_BuildArray (location_t location, tree type, tree array,
                               tree index, tree lowIndice);
EXTERN void m2expr_BuildIfNotInRangeGoto (location_t location, tree var,
                                          tree low, tree high, char *label);
EXTERN void m2expr_BuildIfInRangeGoto (location_t location, tree var, tree low,
                                       tree high, char *label);
EXTERN void m2expr_BuildForeachWordInSetDoIfExpr (
    location_t location, tree type, tree op1, tree op2, bool is_op1lvalue,
    bool is_op2lvalue, bool is_op1const, bool is_op2const,
    tree (*expr) (location_t, tree, tree), char *label);
EXTERN void m2expr_BuildIfNotVarInVar (location_t location, tree type,
                                       tree varset, tree varel, bool is_lvalue,
                                       tree low, tree high ATTRIBUTE_UNUSED,
                                       char *label);
EXTERN void m2expr_BuildIfVarInVar (location_t location, tree type,
                                    tree varset, tree varel, bool is_lvalue,
                                    tree low, tree high ATTRIBUTE_UNUSED,
                                    char *label);
EXTERN void m2expr_BuildIfNotConstInVar (location_t location, tree type,
                                         tree varset, tree constel,
                                         bool is_lvalue, int fieldno,
                                         char *label);
EXTERN void m2expr_BuildIfConstInVar (location_t location, tree type,
                                      tree varset, tree constel, bool is_lvalue,
                                      int fieldno, char *label);
EXTERN tree m2expr_BuildIsNotSubset (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildIsSubset (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildIsNotSuperset (location_t location, tree op1,
                                       tree op2);
EXTERN tree m2expr_BuildIsSuperset (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildNotEqualTo (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildEqualTo (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildGreaterThanOrEqual (location_t location, tree op1,
                                            tree op2);
EXTERN tree m2expr_BuildLessThanOrEqual (location_t location, tree op1,
                                         tree op2);
EXTERN tree m2expr_BuildGreaterThan (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildLessThan (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildLogicalDifference (location_t location, tree op1,
                                           tree op2, bool needconvert);
EXTERN tree m2expr_BuildSymmetricDifference (location_t location, tree op1,
                                             tree op2, bool needconvert);
EXTERN tree m2expr_BuildLogicalAnd (location_t location, tree op1, tree op2,
                                    bool needconvert);
EXTERN tree m2expr_BuildLogicalOr (location_t location, tree op1, tree op2,
                                   bool needconvert);
EXTERN tree m2expr_BuildLogicalOrAddress (location_t location, tree op1,
                                          tree op2, bool needconvert);
EXTERN tree m2expr_BuildOffset (location_t location, tree record, tree field,
                                bool needconvert ATTRIBUTE_UNUSED);
EXTERN tree m2expr_BuildOffset1 (location_t location, tree field,
                                 bool needconvert ATTRIBUTE_UNUSED);
EXTERN tree m2expr_BuildAddr (location_t location, tree op1, bool needconvert);
EXTERN tree m2expr_BuildSize (location_t location, tree op1,
                              bool needconvert ATTRIBUTE_UNUSED);
EXTERN tree m2expr_BuildTBitSize (location_t location, tree type);
EXTERN tree m2expr_BuildSetNegate (location_t location, tree op1,
                                   bool needconvert);
EXTERN tree m2expr_BuildNegate (location_t location, tree op1,
                                bool needconvert);
EXTERN tree m2expr_BuildNegateCheck (location_t location, tree arg,
                                     tree lowest, tree min, tree max);
EXTERN tree m2expr_BuildTrunc (tree op1);
EXTERN tree m2expr_BuildCoerce (location_t location, tree des, tree type,
                                tree expr);
EXTERN tree m2expr_RemoveOverflow (tree t);
EXTERN bool m2expr_TreeOverflow (tree t);

EXTERN unsigned int m2expr_StringLength (tree string);
EXTERN tree m2expr_FoldAndStrip (tree t);
EXTERN int m2expr_interpret_integer (location_t location,
				     const char *str, unsigned int base,
                                     unsigned HOST_WIDE_INT *low,
                                     HOST_WIDE_INT *high);
EXTERN int m2expr_interpret_m2_integer (location_t location,
					const char *str, unsigned int base,
                                        unsigned int *low, int *high,
					bool *needsLong, bool *needsUnsigned);

EXTERN tree m2expr_BuildAddCheck (location_t location, tree op1, tree op2,
                                  tree lowest, tree min, tree max);
EXTERN tree m2expr_BuildSubCheck (location_t location, tree op1, tree op2,
                                  tree lowest, tree min, tree max);
EXTERN tree m2expr_BuildMultCheck (location_t location, tree op1, tree op2,
                                   tree lowest, tree min, tree max);

EXTERN tree m2expr_BuildAdd (location_t location, tree op1, tree op2,
                             bool needconvert);
EXTERN tree m2expr_BuildSub (location_t location, tree op1, tree op2,
                             bool needconvert);
EXTERN tree m2expr_BuildDivTrunc (location_t location, tree op1, tree op2,
                                  bool needconvert);
EXTERN tree m2expr_BuildDivTruncCheck (location_t location, tree op1, tree op2,
				       tree lowest, tree min, tree max);
EXTERN tree m2expr_BuildModTrunc (location_t location, tree op1, tree op2,
                                  bool needconvert);

EXTERN tree m2expr_BuildDivCeil (location_t location, tree op1, tree op2,
                                 bool needconvert);
EXTERN tree m2expr_BuildModCeil (location_t location, tree op1, tree op2,
                                 bool needconvert);

EXTERN tree m2expr_BuildDivFloor (location_t location, tree op1, tree op2,
                                  bool needconvert);
EXTERN tree m2expr_BuildModFloor (location_t location, tree op1, tree op2,
                                  bool needconvert);

EXTERN tree m2expr_BuildDivM2 (location_t location, tree op1, tree op2,
                               bool needsconvert);
EXTERN tree m2expr_BuildModM2 (location_t location, tree op1, tree op2,
                               bool needsconvert);
EXTERN tree m2expr_BuildDivM2Check (location_t location, tree op1, tree op2,
			            tree lowest, tree min, tree max);

EXTERN tree m2expr_BuildModM2Check (location_t location, tree op1, tree op2,
                                  tree lowest, tree min, tree max);

EXTERN tree m2expr_BuildLSL (location_t location, tree op1, tree op2,
                             bool needconvert);

EXTERN tree m2expr_BuildLSR (location_t location, tree op1, tree op2,
                             bool needconvert);

EXTERN void m2expr_BuildLogicalShift (location_t location, tree op1, tree op2,
                                      tree op3, tree nBits ATTRIBUTE_UNUSED,
                                      bool needconvert);

EXTERN tree m2expr_BuildLRL (location_t location, tree op1, tree op2,
                             bool needconvert);

EXTERN tree m2expr_BuildLRR (location_t location, tree op1, tree op2,
                             bool needconvert);
EXTERN tree m2expr_BuildMult (location_t location, tree op1, tree op2,
                              bool needconvert);

EXTERN tree m2expr_BuildRRotate (location_t location, tree op1, tree nBits,
                                 bool needconvert);
EXTERN tree m2expr_BuildLRotate (location_t location, tree op1, tree nBits,
                                 bool needconvert);

EXTERN tree m2expr_BuildMask (location_t location, tree nBits,
                              bool needconvert);
EXTERN tree m2expr_BuildLRLn (location_t location, tree op1, tree op2,
                              tree nBits, bool needconvert);
EXTERN tree m2expr_BuildLRRn (location_t location, tree op1, tree op2,
                              tree nBits, bool needconvert);
EXTERN void m2expr_BuildLogicalRotate (location_t location, tree op1, tree op2,
                                       tree op3, tree nBits, bool needconvert);
EXTERN void m2expr_BuildBinarySetDo (
    location_t location, tree settype, tree op1, tree op2, tree op3,
    void (*binop) (location_t, tree, tree, tree, tree, bool), bool is_op1lvalue,
    bool is_op2lvalue, bool is_op3lvalue, tree nBits, tree unbounded,
    tree varproc, tree leftproc, tree rightproc);

EXTERN tree m2expr_GetSizeOf (location_t location, tree type);
EXTERN tree m2expr_GetSizeOfInBits (tree type);

EXTERN tree m2expr_GetCardinalZero (location_t location);
EXTERN tree m2expr_GetCardinalOne (location_t location);
EXTERN tree m2expr_GetIntegerZero (location_t location);
EXTERN tree m2expr_GetIntegerOne (location_t location);
EXTERN tree m2expr_GetWordZero (location_t location);
EXTERN tree m2expr_GetWordOne (location_t location);
EXTERN tree m2expr_GetPointerZero (location_t location);
EXTERN tree m2expr_GetPointerOne (location_t location);

EXTERN int m2expr_CompareTrees (tree e1, tree e2);
EXTERN tree m2expr_build_unary_op (location_t location ATTRIBUTE_UNUSED,
                                   enum tree_code code, tree arg,
                                   int flag ATTRIBUTE_UNUSED);
EXTERN tree m2expr_build_binary_op (location_t location, enum tree_code code,
                                    tree op1, tree op2, int convert);
EXTERN tree m2expr_build_binary_op_check (location_t location,
                                          enum tree_code code, tree op1,
                                          tree op2, bool needconvert,
                                          tree lowest, tree min, tree max);
EXTERN void m2expr_ConstantExpressionWarning (tree value);
EXTERN tree m2expr_BuildAddAddress (location_t location, tree op1, tree op2);
EXTERN tree m2expr_BuildRDiv (location_t location, tree op1, tree op2,
                              bool needconvert);
EXTERN tree m2expr_BuildCondIfExpression (tree condition, tree type,
					  tree left, tree right);
EXTERN int m2expr_GetCstInteger (tree cst);
EXTERN tree m2expr_calcNbits (location_t location, tree min, tree max);
EXTERN bool m2expr_OverflowZType (location_t location, const char *str,
				  unsigned int base, bool issueError);
EXTERN tree m2expr_BuildSystemTBitSize (location_t location, tree type);
EXTERN void m2expr_init (location_t location);

#undef EXTERN
#endif  /* m2expr_h.  */
