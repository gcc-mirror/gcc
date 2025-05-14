/* m2type.h header file for m2type.cc.

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

#if !defined(m2type_h)
#define m2type_h
#if defined(m2type_c)
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN
#endif /* !__GNUG__.  */
#else /* !m2type_c.  */
#if defined(__GNUG__)
#define EXTERN extern "C"
#else /* !__GNUG__.  */
#define EXTERN extern
#endif /* !__GNUG__.  */
#endif /* !m2type_c.  */

#include <stdbool.h>

typedef void *m2type_Constructor;

EXTERN bool m2type_ValueInTypeRange (tree type, tree value);
EXTERN bool m2type_ExceedsTypeRange (tree type, tree low, tree high);
EXTERN bool m2type_ValueOutOfTypeRange (tree type, tree value);
EXTERN bool m2type_WithinTypeRange (tree type, tree low, tree high);
EXTERN tree m2type_BuildStartArrayType (tree index_type, tree elt_type,
                                        int type);
EXTERN void m2type_PutArrayType (tree array, tree type);
EXTERN tree m2type_BuildEndArrayType (tree arraytype, tree elementtype,
                                      tree indextype, int type);
EXTERN tree m2type_GetArrayNoOfElements (location_t location, tree arraytype);
EXTERN tree m2type_BuildArrayIndexType (tree low, tree high);
EXTERN void m2type_GarbageCollect (void);
EXTERN void m2type_MarkFunctionReferenced (tree f);
EXTERN void m2type_AddStatement (location_t location, tree t);
EXTERN tree m2type_BuildNumberOfArrayElements (location_t location,
                                               tree arrayType);
EXTERN tree m2type_BuildPackedFieldRecord (location_t location, char *name,
                                           tree fieldtype);
EXTERN tree m2type_SetRecordFieldOffset (tree field, tree byteOffset,
                                         tree bitOffset, tree fieldtype,
                                         tree nbits);
EXTERN tree m2type_SetTypePacked (tree node);
EXTERN tree m2type_SetDeclPacked (tree node);
EXTERN tree m2type_SetAlignment (tree node, tree align);
EXTERN tree m2type_BuildEndRecord (location_t location, tree record,
                                   tree fieldlist, bool isPacked);
EXTERN tree m2type_AddStringToTreeList (tree list, tree string);
EXTERN tree m2type_ChainOnParamValue (tree list, tree name, tree str,
                                      tree value);
EXTERN tree m2type_ChainOn (tree t1, tree t2);
EXTERN tree m2type_BuildFieldRecord (location_t location, char *name,
                                     tree type);
EXTERN tree m2type_BuildStartFieldRecord (location_t location, char *name,
                                          tree type);
EXTERN tree m2type_BuildEndFieldVarient (location_t location,
                                         tree varientField, tree varientList,
                                         bool isPacked);
EXTERN tree m2type_BuildStartFieldVarient (location_t location, char *name);
EXTERN tree m2type_BuildEndVarient (location_t location, tree varientField,
                                    tree varientList, bool isPacked);
EXTERN tree m2type_BuildStartVarient (location_t location, char *name);
EXTERN tree m2type_BuildStartUnion (location_t location, char *name);
EXTERN tree m2type_BuildStartRecord (location_t location, char *name);
EXTERN tree m2type_RealToTree (char *name);
EXTERN tree m2type_BuildArrayStringConstructor (location_t location,
                                                tree arrayType, tree str,
                                                tree length);

EXTERN tree m2type_GetM2CharType (void);
EXTERN tree m2type_GetM2IntegerType (void);
EXTERN tree m2type_GetM2ShortRealType (void);
EXTERN tree m2type_GetM2RealType (void);
EXTERN tree m2type_GetM2LongRealType (void);
EXTERN tree m2type_GetM2LongIntType (void);
EXTERN tree m2type_GetM2LongCardType (void);
EXTERN tree m2type_GetM2ShortIntType (void);
EXTERN tree m2type_GetShortIntType (void);
EXTERN tree m2type_GetM2ShortCardType (void);
EXTERN tree m2type_GetShortCardType (void);
EXTERN tree m2type_GetISOWordType (void);
EXTERN tree m2type_GetISOByteType (void);
EXTERN tree m2type_GetISOLocType (void);
EXTERN tree m2type_GetM2Integer8 (void);
EXTERN tree m2type_GetM2Integer16 (void);
EXTERN tree m2type_GetM2Integer32 (void);
EXTERN tree m2type_GetM2Integer64 (void);
EXTERN tree m2type_GetM2Cardinal8 (void);
EXTERN tree m2type_GetM2Cardinal16 (void);
EXTERN tree m2type_GetM2Cardinal32 (void);
EXTERN tree m2type_GetM2Cardinal64 (void);
EXTERN tree m2type_GetM2Word16 (void);
EXTERN tree m2type_GetM2Word32 (void);
EXTERN tree m2type_GetM2Word64 (void);
EXTERN tree m2type_GetM2Bitset8 (void);
EXTERN tree m2type_GetM2Bitset16 (void);
EXTERN tree m2type_GetM2Bitset32 (void);
EXTERN tree m2type_GetM2Real32 (void);
EXTERN tree m2type_GetM2Real64 (void);
EXTERN tree m2type_GetM2Real96 (void);
EXTERN tree m2type_GetM2Real128 (void);
EXTERN tree m2type_GetM2Complex32 (void);
EXTERN tree m2type_GetM2Complex64 (void);
EXTERN tree m2type_GetM2Complex96 (void);
EXTERN tree m2type_GetM2Complex128 (void);
EXTERN tree m2type_GetM2ShortComplexType (void);
EXTERN tree m2type_GetM2LongComplexType (void);
EXTERN tree m2type_GetM2ComplexType (void);
EXTERN tree m2type_GetShortCardType (void);
EXTERN tree m2type_GetProcType (void);
EXTERN tree m2type_GetCSizeTType (void);
EXTERN tree m2type_GetCSSizeTType (void);
EXTERN tree m2type_GetCOffTType (void);
EXTERN tree m2type_GetM2CType (void);

EXTERN tree m2type_GetBitsetType (void);
EXTERN tree m2type_GetM2CardinalType (void);
EXTERN tree m2type_GetWordType (void);
EXTERN tree m2type_GetIntegerType (void);
EXTERN tree m2type_GetCardinalType (void);
EXTERN tree m2type_GetPointerType (void);
EXTERN tree m2type_GetLongIntType (void);
EXTERN tree m2type_GetShortRealType (void);
EXTERN tree m2type_GetLongRealType (void);
EXTERN tree m2type_GetRealType (void);
EXTERN tree m2type_GetBitnumType (void);
EXTERN tree m2type_GetVoidType (void);
EXTERN tree m2type_GetByteType (void);
EXTERN tree m2type_GetCharType (void);
EXTERN tree m2type_GetPackedBooleanType (void);
EXTERN tree m2type_GetBooleanTrue (void);
EXTERN tree m2type_GetBooleanFalse (void);
EXTERN tree m2type_GetBooleanType (void);
EXTERN tree m2type_BuildSmallestTypeRange (location_t location, tree low,
                                           tree high);
EXTERN tree m2type_BuildSetTypeFromSubrange (location_t location, char *name,
                                             tree subrangeType, tree lowval,
                                             tree highval, bool ispacked);
EXTERN int m2type_GetBitsPerBitset (void);
EXTERN tree m2type_GetM2RType (void);
EXTERN tree m2type_GetM2ZType (void);

EXTERN tree m2type_DeclareKnownType (location_t location, char *name,
                                     tree type);
EXTERN tree m2type_GetTreeType (tree type);
EXTERN tree m2type_BuildEndFunctionType (tree func, tree type,
                                         bool uses_varargs);
EXTERN tree m2type_BuildStartFunctionType (
    location_t location ATTRIBUTE_UNUSED, char *name ATTRIBUTE_UNUSED);
EXTERN void m2type_InitFunctionTypeParameters (void);
EXTERN tree m2type_BuildVariableArrayAndDeclare (location_t location,
                                                 tree elementtype, tree high,
                                                 char *name, tree scope);
EXTERN void m2type_InitSystemTypes (location_t location, int loc);
EXTERN void m2type_InitBaseTypes (location_t location);
EXTERN tree m2type_BuildStartType (location_t location, char *name, tree type);
EXTERN tree m2type_BuildEndType (location_t location, tree type);
EXTERN tree m2type_GetDefaultType (location_t location, char *name, tree type);
EXTERN tree m2type_GetMinFrom (location_t location, tree type);
EXTERN tree m2type_GetMaxFrom (location_t location, tree type);
EXTERN void m2type_BuildTypeDeclaration (location_t location, tree type);
EXTERN tree m2type_BuildStartEnumeration (location_t location, char *name,
                                          bool ispacked);
EXTERN tree m2type_BuildEndEnumeration (location_t location, tree enumtype,
                                        tree enumvalues);
EXTERN tree m2type_BuildEnumerator (location_t location, char *name,
                                    tree value, tree *enumvalues);
EXTERN tree m2type_BuildPointerType (tree totype);
EXTERN tree m2type_BuildConstPointerType (tree totype);
EXTERN tree m2type_BuildSetType (location_t location, char *name, tree type,
                                 tree lowval, tree highval, bool ispacked);
EXTERN void *m2type_BuildStartSetConstructor (tree type);
EXTERN void m2type_BuildSetConstructorElement (void *p, tree value);
EXTERN tree m2type_BuildEndSetConstructor (void *p);
EXTERN void *m2type_BuildStartRecordConstructor (tree type);
EXTERN tree m2type_BuildEndRecordConstructor (void *p);
EXTERN void m2type_BuildRecordConstructorElement (void *p, tree value);
EXTERN void *m2type_BuildStartArrayConstructor (tree type);
EXTERN tree m2type_BuildEndArrayConstructor (void *p);
EXTERN void m2type_BuildArrayConstructorElement (void *p, tree value,
                                                 tree indice);
EXTERN tree m2type_BuildCharConstant (location_t location, const char *string);
EXTERN tree m2type_BuildCharConstantChar (location_t location, char ch);
EXTERN tree m2type_BuildSubrangeType (location_t location, char *name,
                                      tree type, tree lowval, tree highval);
EXTERN tree m2type_gm2_unsigned_type (tree type);
EXTERN tree m2type_gm2_signed_type (tree type);
EXTERN tree m2type_gm2_signed_or_unsigned_type (int unsignedp, tree type);
EXTERN tree m2type_gm2_type_for_size (unsigned int bits, int unsignedp);
EXTERN tree m2type_BuildProcTypeParameterDeclaration (location_t location,
                                                      tree type,
                                                      bool isreference);
EXTERN bool m2type_IsAddress (tree type);
EXTERN tree m2type_GetCardinalAddressType (void);
EXTERN bool m2type_SameRealType (tree a, tree b);
EXTERN bool m2type_IsGccStrictTypeEquivalent (tree left, tree right);

#undef EXTERN
#endif /* m2type_h  */
