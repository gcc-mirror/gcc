/* m2type.c provides an interface to GCC type trees.

Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

#define m2type_c
#include "m2assert.h"
#include "m2block.h"
#include "m2builtins.h"
#include "m2convert.h"
#include "m2decl.h"
#include "m2except.h"
#include "m2expr.h"
#include "m2linemap.h"
#include "m2tree.h"
#include "m2treelib.h"
#include "m2type.h"

#undef USE_BOOLEAN
static int broken_set_debugging_info = TRUE;


struct GTY (()) struct_constructor
{
  /* constructor_type, the type that we are constructing.  */
  tree GTY ((skip (""))) constructor_type;
  /* constructor_fields, the list of fields belonging to
     constructor_type.  Used by SET and RECORD constructors.  */
  tree GTY ((skip (""))) constructor_fields;
  /* constructor_element_list, the list of constants used by SET and
     RECORD constructors.  */
  tree GTY ((skip (""))) constructor_element_list;
  /* constructor_elements, used by an ARRAY initializer all elements
     are held in reverse order.  */
  vec<constructor_elt, va_gc> *constructor_elements;
  /* level, the next level down in the constructor stack.  */
  struct struct_constructor *level;
};

static GTY (()) struct struct_constructor *top_constructor = NULL;

typedef struct GTY (()) array_desc
{
  int type;
  tree index;
  tree array;
  struct array_desc *next;
} array_desc;

static GTY (()) array_desc *list_of_arrays = NULL;
/* Used in BuildStartFunctionType.  */
static GTY (()) tree param_type_list;

static GTY (()) tree proc_type_node;
static GTY (()) tree bitset_type_node;
static GTY (()) tree bitnum_type_node;
static GTY (()) tree m2_char_type_node;
static GTY (()) tree m2_integer_type_node;
static GTY (()) tree m2_cardinal_type_node;
static GTY (()) tree m2_short_real_type_node;
static GTY (()) tree m2_real_type_node;
static GTY (()) tree m2_long_real_type_node;
static GTY (()) tree m2_long_int_type_node;
static GTY (()) tree m2_long_card_type_node;
static GTY (()) tree m2_short_int_type_node;
static GTY (()) tree m2_short_card_type_node;
static GTY (()) tree m2_z_type_node;
static GTY (()) tree m2_iso_loc_type_node;
static GTY (()) tree m2_iso_byte_type_node;
static GTY (()) tree m2_iso_word_type_node;
static GTY (()) tree m2_integer8_type_node;
static GTY (()) tree m2_integer16_type_node;
static GTY (()) tree m2_integer32_type_node;
static GTY (()) tree m2_integer64_type_node;
static GTY (()) tree m2_cardinal8_type_node;
static GTY (()) tree m2_cardinal16_type_node;
static GTY (()) tree m2_cardinal32_type_node;
static GTY (()) tree m2_cardinal64_type_node;
static GTY (()) tree m2_word16_type_node;
static GTY (()) tree m2_word32_type_node;
static GTY (()) tree m2_word64_type_node;
static GTY (()) tree m2_bitset8_type_node;
static GTY (()) tree m2_bitset16_type_node;
static GTY (()) tree m2_bitset32_type_node;
static GTY (()) tree m2_real32_type_node;
static GTY (()) tree m2_real64_type_node;
static GTY (()) tree m2_real96_type_node;
static GTY (()) tree m2_real128_type_node;
static GTY (()) tree m2_complex_type_node;
static GTY (()) tree m2_long_complex_type_node;
static GTY (()) tree m2_short_complex_type_node;
static GTY (()) tree m2_c_type_node;
static GTY (()) tree m2_complex32_type_node;
static GTY (()) tree m2_complex64_type_node;
static GTY (()) tree m2_complex96_type_node;
static GTY (()) tree m2_complex128_type_node;
static GTY (()) tree m2_packed_boolean_type_node;
static GTY (()) tree m2_cardinal_address_type_node;

/* gm2_canonicalize_array - returns a unique array node based on,
   index_type, and, type.  */

static tree
gm2_canonicalize_array (tree index_type, int type)
{
  array_desc *l = list_of_arrays;

  while (l != NULL)
    {
      if (l->type == type && l->index == index_type)
        return l->array;
      else
        l = l->next;
    }
  l = ggc_alloc<array_desc> ();
  l->next = list_of_arrays;
  l->type = type;
  l->index = index_type;
  l->array = make_node (ARRAY_TYPE);
  TREE_TYPE (l->array) = NULL_TREE;
  TYPE_DOMAIN (l->array) = index_type;
  list_of_arrays = l;
  return l->array;
}

/* BuildStartArrayType - creates an array with an indextype and
   elttype.  The front end symbol, type, is also passed to allow the
   gccgm2 to return the canonical edition of the array type even if
   the GCC elttype is NULL_TREE.  */

tree
m2type_BuildStartArrayType (tree index_type, tree elt_type, int type)
{
  tree t;

  elt_type = m2tree_skip_type_decl (elt_type);
  ASSERT_CONDITION (index_type != NULL_TREE);
  if (elt_type == NULL_TREE)
    {
      /* cannot use GCC canonicalization routines yet, so we use our front
         end version based on the front end type.  */
      return gm2_canonicalize_array (index_type, type);
    }
  t = gm2_canonicalize_array (index_type, type);
  if (TREE_TYPE (t) == NULL_TREE)
    TREE_TYPE (t) = elt_type;
  else
    ASSERT_CONDITION (TREE_TYPE (t) == elt_type);

  return t;
}

/* PutArrayType - */

void
m2type_PutArrayType (tree array, tree type)
{
  TREE_TYPE (array) = m2tree_skip_type_decl (type);
}

/* gccgm2_GetArrayNoOfElements - returns the number of elements in,
   arraytype.  */

tree
m2type_GetArrayNoOfElements (location_t location, tree arraytype)
{
  tree index_type = TYPE_DOMAIN (m2tree_skip_type_decl (arraytype));
  tree min = TYPE_MIN_VALUE (index_type);
  tree max = TYPE_MAX_VALUE (index_type);

  m2assert_AssertLocation (location);
  return m2expr_FoldAndStrip (m2expr_BuildSub (location, max, min, FALSE));
}

/* gm2_finish_build_array_type - complete building the partially
   created array type, arrayType.  The arrayType is now known to be
   declared as: ARRAY index_type OF elt_type.  There will only ever
   be one gcc tree type for this array definition.  The third
   parameter, type, is a front end type and this is necessary so that
   the canonicalization creates unique array types for each type.  */

static tree
gm2_finish_build_array_type (tree arrayType, tree elt_type, tree index_type,
                             int type)
{
  tree old = arrayType;

  elt_type = m2tree_skip_type_decl (elt_type);
  ASSERT_CONDITION (index_type != NULL_TREE);
  if (TREE_CODE (elt_type) == FUNCTION_TYPE)
    {
      error ("arrays of functions are not meaningful");
      elt_type = integer_type_node;
    }

  TREE_TYPE (arrayType) = elt_type;
  TYPE_DOMAIN (arrayType) = index_type;

  arrayType = gm2_canonicalize_array (index_type, type);
  if (arrayType != old)
    internal_error (
        "[%s:%d]:array declaration canonicalization has failed",
        __FILE__, __LINE__);

  if (!COMPLETE_TYPE_P (arrayType))
    layout_type (arrayType);
  return arrayType;
}

/* BuildEndArrayType - returns a type which is an array indexed by
   IndexType and which has ElementType elements.  */

tree
m2type_BuildEndArrayType (tree arraytype, tree elementtype, tree indextype,
                          int type)
{
  elementtype = m2tree_skip_type_decl (elementtype);
  ASSERT (indextype == TYPE_DOMAIN (arraytype), indextype);

  if (TREE_CODE (elementtype) == FUNCTION_TYPE)
    return gm2_finish_build_array_type (arraytype, ptr_type_node, indextype,
                                        type);
  else
    return gm2_finish_build_array_type (
        arraytype, m2tree_skip_type_decl (elementtype), indextype, type);
}

/* gm2_build_array_type - returns a type which is an array indexed by
   IndexType and which has ElementType elements.  */

static tree
gm2_build_array_type (tree elementtype, tree indextype, int fetype)
{
  tree arrayType = m2type_BuildStartArrayType (indextype, elementtype, fetype);
  return m2type_BuildEndArrayType (arrayType, elementtype, indextype, fetype);
}

/* ValueInTypeRange returns TRUE if the constant, value, lies within
the range of, type.  */

int
m2type_ValueInTypeRange (tree type, tree value)
{
  tree low_type = m2tree_skip_type_decl (type);
  tree min_value = TYPE_MIN_VALUE (low_type);
  tree max_value = TYPE_MAX_VALUE (low_type);

  value = m2expr_FoldAndStrip (value);
  return ((tree_int_cst_compare (min_value, value) <= 0)
          && (tree_int_cst_compare (value, max_value) <= 0));
}

/* ValueOutOfTypeRange returns TRUE if the constant, value, exceeds
the range of, type.  */

int
m2type_ValueOutOfTypeRange (tree type, tree value)
{
  return (!m2type_ValueInTypeRange (type, value));
}

/* ExceedsTypeRange return TRUE if low or high exceed the range of, type.  */

int
m2type_ExceedsTypeRange (tree type, tree low, tree high)
{
  return (m2type_ValueOutOfTypeRange (type, low)
          || m2type_ValueOutOfTypeRange (type, high));
}

/* WithinTypeRange return TRUE if low and high are within the range of, type.
 */

int
m2type_WithinTypeRange (tree type, tree low, tree high)
{
  return (m2type_ValueInTypeRange (type, low)
          && m2type_ValueInTypeRange (type, high));
}

/* BuildArrayIndexType - creates an integer index which accesses an
   array.  low and high are the min, max elements of the array.  GCC
   insists we access an array with an integer indice.  */

tree
m2type_BuildArrayIndexType (tree low, tree high)
{
  tree sizelow = convert (m2type_GetIntegerType (), m2expr_FoldAndStrip (low));
  tree sizehigh
      = convert (m2type_GetIntegerType (), m2expr_FoldAndStrip (high));

  if (m2expr_TreeOverflow (sizelow))
    error ("low bound for the array is outside the ztype limits");
  if (m2expr_TreeOverflow (sizehigh))
    error ("high bound for the array is outside the ztype limits");

  return build_range_type (m2type_GetIntegerType (),
                           m2expr_FoldAndStrip (sizelow),
                           m2expr_FoldAndStrip (sizehigh));
}

/* build_m2_type_node_by_array - builds a ISO Modula-2 word type from
   ARRAY [low..high] OF arrayType.  this matches the front end data
   type, fetype, which is only used during canonicalization.  */

static tree
build_m2_type_node_by_array (tree arrayType, tree low, tree high, int fetype)
{
  return gm2_build_array_type (arrayType,
                               m2type_BuildArrayIndexType (low, high), fetype);
}

/* build_m2_word16_type_node - build an ISO 16 bit word as an ARRAY
   [0..1] OF loc.  */

static tree
build_m2_word16_type_node (location_t location, int loc)
{
  return build_m2_type_node_by_array (m2type_GetISOLocType (),
                                      m2expr_GetIntegerZero (location),
                                      m2expr_GetIntegerOne (location), loc);
}

/* build_m2_word32_type_node - build an ISO 32 bit word as an ARRAY
   [0..3] OF loc.  */

static tree
build_m2_word32_type_node (location_t location, int loc)
{
  return build_m2_type_node_by_array (m2type_GetISOLocType (),
                                      m2expr_GetIntegerZero (location),
                                      m2decl_BuildIntegerConstant (3), loc);
}

/* build_m2_word64_type_node - build an ISO 32 bit word as an ARRAY
   [0..7] OF loc.  */

static tree
build_m2_word64_type_node (location_t location, int loc)
{
  return build_m2_type_node_by_array (m2type_GetISOLocType (),
                                      m2expr_GetIntegerZero (location),
                                      m2decl_BuildIntegerConstant (7), loc);
}

/* GetM2Complex32 - return the fixed size complex type.  */

tree
m2type_GetM2Complex32 (void)
{
  return m2_complex32_type_node;
}

/* GetM2Complex64 - return the fixed size complex type.  */

tree
m2type_GetM2Complex64 (void)
{
  return m2_complex64_type_node;
}

/* GetM2Complex96 - return the fixed size complex type.  */

tree
m2type_GetM2Complex96 (void)
{
  return m2_complex96_type_node;
}

/* GetM2Complex128 - return the fixed size complex type.  */

tree
m2type_GetM2Complex128 (void)
{
  return m2_complex128_type_node;
}

/* GetM2CType - a test function.  */

tree
m2type_GetM2CType (void)
{
  return m2_c_type_node;
}

/* GetM2ShortComplexType - return the short complex type.  */

tree
m2type_GetM2ShortComplexType (void)
{
  return m2_short_complex_type_node;
}

/* GetM2LongComplexType - return the long complex type.  */

tree
m2type_GetM2LongComplexType (void)
{
  return m2_long_complex_type_node;
}

/* GetM2ComplexType - return the complex type.  */

tree
m2type_GetM2ComplexType (void)
{
  return m2_complex_type_node;
}

/* GetM2Real128 - return the real 128 bit type.  */

tree
m2type_GetM2Real128 (void)
{
  return m2_real128_type_node;
}

/* GetM2Real96 - return the real 96 bit type.  */

tree
m2type_GetM2Real96 (void)
{
  return m2_real96_type_node;
}

/* GetM2Real64 - return the real 64 bit type.  */

tree
m2type_GetM2Real64 (void)
{
  return m2_real64_type_node;
}

/* GetM2Real32 - return the real 32 bit type.  */

tree
m2type_GetM2Real32 (void)
{
  return m2_real32_type_node;
}

/* GetM2Bitset32 - return the bitset 32 bit type.  */

tree
m2type_GetM2Bitset32 (void)
{
  return m2_bitset32_type_node;
}

/* GetM2Bitset16 - return the bitset 16 bit type.  */

tree
m2type_GetM2Bitset16 (void)
{
  return m2_bitset16_type_node;
}

/* GetM2Bitset8 - return the bitset 8 bit type.  */

tree
m2type_GetM2Bitset8 (void)
{
  return m2_bitset8_type_node;
}

/* GetM2Word64 - return the word 64 bit type.  */

tree
m2type_GetM2Word64 (void)
{
  return m2_word64_type_node;
}

/* GetM2Word32 - return the word 32 bit type.  */

tree
m2type_GetM2Word32 (void)
{
  return m2_word32_type_node;
}

/* GetM2Word16 - return the word 16 bit type.  */

tree
m2type_GetM2Word16 (void)
{
  return m2_word16_type_node;
}

/* GetM2Cardinal64 - return the cardinal 64 bit type.  */

tree
m2type_GetM2Cardinal64 (void)
{
  return m2_cardinal64_type_node;
}

/* GetM2Cardinal32 - return the cardinal 32 bit type.  */

tree
m2type_GetM2Cardinal32 (void)
{
  return m2_cardinal32_type_node;
}

/* GetM2Cardinal16 - return the cardinal 16 bit type.  */

tree
m2type_GetM2Cardinal16 (void)
{
  return m2_cardinal16_type_node;
}

/* GetM2Cardinal8 - return the cardinal 8 bit type.  */

tree
m2type_GetM2Cardinal8 (void)
{
  return m2_cardinal8_type_node;
}

/* GetM2Integer64 - return the integer 64 bit type.  */

tree
m2type_GetM2Integer64 (void)
{
  return m2_integer64_type_node;
}

/* GetM2Integer32 - return the integer 32 bit type.  */

tree
m2type_GetM2Integer32 (void)
{
  return m2_integer32_type_node;
}

/* GetM2Integer16 - return the integer 16 bit type.  */

tree
m2type_GetM2Integer16 (void)
{
  return m2_integer16_type_node;
}

/* GetM2Integer8 - return the integer 8 bit type.  */

tree
m2type_GetM2Integer8 (void)
{
  return m2_integer8_type_node;
}

/* GetM2RType - return the ISO R data type, the longest real
   datatype.  */

tree
m2type_GetM2RType (void)
{
  return long_double_type_node;
}

/* GetM2ZType - return the ISO Z data type, the longest int datatype.  */

tree
m2type_GetM2ZType (void)
{
  return m2_z_type_node;
}

/* GetShortCardType - return the C short unsigned data type.  */

tree
m2type_GetShortCardType (void)
{
  return short_unsigned_type_node;
}

/* GetM2ShortCardType - return the m2 short cardinal data type.  */

tree
m2type_GetM2ShortCardType (void)
{
  return m2_short_card_type_node;
}

/* GetShortIntType - return the C short int data type.  */

tree
m2type_GetShortIntType (void)
{
  return short_integer_type_node;
}

/* GetM2ShortIntType - return the m2 short integer data type.  */

tree
m2type_GetM2ShortIntType (void)
{
  return m2_short_int_type_node;
}

/* GetM2LongCardType - return the m2 long cardinal data type.  */

tree
m2type_GetM2LongCardType (void)
{
  return m2_long_card_type_node;
}

/* GetM2LongIntType - return the m2 long integer data type.  */

tree
m2type_GetM2LongIntType (void)
{
  return m2_long_int_type_node;
}

/* GetM2LongRealType - return the m2 long real data type.  */

tree
m2type_GetM2LongRealType (void)
{
  return m2_long_real_type_node;
}

/* GetM2RealType - return the m2 real data type.  */

tree
m2type_GetM2RealType (void)
{
  return m2_real_type_node;
}

/* GetM2ShortRealType - return the m2 short real data type.  */

tree
m2type_GetM2ShortRealType (void)
{
  return m2_short_real_type_node;
}

/* GetM2CardinalType - return the m2 cardinal data type.  */

tree
m2type_GetM2CardinalType (void)
{
  return m2_cardinal_type_node;
}

/* GetM2IntegerType - return the m2 integer data type.  */

tree
m2type_GetM2IntegerType (void)
{
  return m2_integer_type_node;
}

/* GetM2CharType - return the m2 char data type.  */

tree
m2type_GetM2CharType (void)
{
  return m2_char_type_node;
}

/* GetProcType - return the m2 proc data type.  */

tree
m2type_GetProcType (void)
{
  return proc_type_node;
}

/* GetISOWordType - return the m2 iso word data type.  */

tree
m2type_GetISOWordType (void)
{
  return m2_iso_word_type_node;
}

/* GetISOByteType - return the m2 iso byte data type.  */

tree
m2type_GetISOByteType (void)
{
  return m2_iso_byte_type_node;
}

/* GetISOLocType - return the m2 loc word data type.  */

tree
m2type_GetISOLocType (void)
{
  return m2_iso_loc_type_node;
}

/* --fixme-- section  check these gaius - the function names are misleading
 * --fixme-- once the compiler is building correctly.  */
/* GetWordType - return the C unsigned data type.  */

tree
m2type_GetWordType (void)
{
  return unsigned_type_node;
}

/* GetLongIntType - return the C long int data type.  */

tree
m2type_GetLongIntType (void)
{
  return long_integer_type_node;
}

/* GetShortRealType - return the C float data type.  */

tree
m2type_GetShortRealType (void)
{
  return float_type_node;
}

/* GetLongRealType - return the C long double data type.  */

tree
m2type_GetLongRealType (void)
{
  return long_double_type_node;
}

/* GetRealType - */

tree
m2type_GetRealType (void)
{
  return double_type_node;
}
/* end of --fixme-- section.  */

/* GetBitnumType - return the ISO bitnum type.  */

tree
m2type_GetBitnumType (void)
{
  return bitnum_type_node;
}

/* GetBitsetType - return the bitset type.  */

tree
m2type_GetBitsetType (void)
{
  return bitset_type_node;
}

/* GetCardinalType - return the cardinal type.  */

tree
m2type_GetCardinalType (void)
{
  return unsigned_type_node;
}

/* GetPointerType - return the GCC ptr type node.  Equivalent to
   (void *).  */

tree
m2type_GetPointerType (void)
{
  return ptr_type_node;
}

/* GetVoidType - return the C void type.  */

tree
m2type_GetVoidType (void)
{
  return void_type_node;
}

/* GetByteType - return the byte type node.  */

tree
m2type_GetByteType (void)
{
  return unsigned_char_type_node;
}

/* GetCharType - return the char type node.  */

tree
m2type_GetCharType (void)
{
  return char_type_node;
}

/* GetIntegerType - return the integer type node.  */

tree
m2type_GetIntegerType (void)
{
  return integer_type_node;
}

/* GetCSizeTType - return a type representing, size_t on this system.  */

tree
m2type_GetCSizeTType (void)
{
  return sizetype;
}

/* GetCSSizeTType - return a type representing, size_t on this
   system.  */

tree
m2type_GetCSSizeTType (void)
{
  return ssizetype;
}

/* GetPackedBooleanType - return the packed boolean data type node.  */

tree
m2type_GetPackedBooleanType (void)
{
  return m2_packed_boolean_type_node;
}

/* GetBooleanTrue - */

tree
m2type_GetBooleanTrue (void)
{
#if defined(USE_BOOLEAN)
  return boolean_true_node;
#else /* !USE_BOOLEAN  */
  return m2expr_GetIntegerOne (m2linemap_BuiltinsLocation ());
#endif /* !USE_BOOLEAN  */
}

/* GetBooleanFalse - */

tree
m2type_GetBooleanFalse (void)
{
#if defined(USE_BOOLEAN)
  return boolean_false_node;
#else /* !USE_BOOLEAN  */
  return m2expr_GetIntegerZero (m2linemap_BuiltinsLocation ());
#endif /* !USE_BOOLEAN  */
}

/* GetBooleanType - */

tree
m2type_GetBooleanType (void)
{
#if defined(USE_BOOLEAN)
  return boolean_type_node;
#else /* !USE_BOOLEAN  */
  return integer_type_node;
#endif /* !USE_BOOLEAN  */
}

/* GetCardinalAddressType - returns the internal data type for
   computing binary arithmetic upon the ADDRESS datatype.  */

tree
m2type_GetCardinalAddressType (void)
{
  return m2_cardinal_address_type_node;
}

/* noBitsRequired - returns the number of bits required to contain,
   values.  How many bits are required to represent all numbers
   between: 0..values-1 */

static tree
noBitsRequired (tree values)
{
  int bits = tree_floor_log2 (values);

  if (integer_pow2p (values))
    /* remember we start counting from zero.  */
    return m2decl_BuildIntegerConstant (bits);
  else
    return m2decl_BuildIntegerConstant (bits + 1);
}

#if 0
/* build_set_type - creates a set type from the, domain, [low..high].
   The values low..high all have type, range_type.  */

static tree
build_set_type (tree domain, tree range_type, int allow_void, int ispacked)
{
  tree type;

  if (!m2tree_IsOrdinal (domain)
      && !(allow_void && TREE_CODE (domain) == VOID_TYPE))
    {
      error ("set base type must be an ordinal type");
      return NULL;
    }

  if (TYPE_SIZE (range_type) == 0)
    layout_type (range_type);

  if (TYPE_SIZE (domain) == 0)
    layout_type (domain);

  type = make_node (SET_TYPE);
  TREE_TYPE (type) = range_type;
  TYPE_DOMAIN (type) = domain;
  TYPE_PACKED (type) = ispacked;

  return type;
}


/* convert_type_to_range - does the conversion and copies the range
   type */

static tree
convert_type_to_range (tree type)
{
  tree min, max;
  tree itype;

  if (!m2tree_IsOrdinal (type))
    {
      error ("ordinal type expected");
      return error_mark_node;
    }

  min = TYPE_MIN_VALUE (type);
  max = TYPE_MAX_VALUE (type);

  if (TREE_TYPE (min) != TREE_TYPE (max))
    {
      error ("range limits are not of the same type");
      return error_mark_node;
    }

  itype = build_range_type (TREE_TYPE (min), min, max);

  if (TREE_TYPE (type) == NULL_TREE)
    {
      layout_type (type);
      TREE_TYPE (itype) = type;
    }
  else
    {
      layout_type (TREE_TYPE (type));
      TREE_TYPE (itype) = TREE_TYPE (type);
    }

  layout_type (itype);
  return itype;
}
#endif

/* build_bitset_type - builds the type BITSET which is exported from
   SYSTEM.  It also builds BITNUM (the subrange from which BITSET is
   created).  */

static tree
build_bitset_type (location_t location)
{
  m2assert_AssertLocation (location);
  bitnum_type_node = build_range_type (
      m2tree_skip_type_decl (m2type_GetCardinalType ()),
      m2decl_BuildIntegerConstant (0),
      m2decl_BuildIntegerConstant (m2decl_GetBitsPerBitset () - 1));
  layout_type (bitnum_type_node);

#if 1
  if (broken_set_debugging_info)
    return unsigned_type_node;
#endif

  ASSERT ((COMPLETE_TYPE_P (bitnum_type_node)), bitnum_type_node);

  return m2type_BuildSetTypeFromSubrange (
      location, NULL, bitnum_type_node, m2decl_BuildIntegerConstant (0),
      m2decl_BuildIntegerConstant (m2decl_GetBitsPerBitset () - 1), FALSE);
}

/* BuildSetTypeFromSubrange - constructs a set type from a
   subrangeType.  */

tree
m2type_BuildSetTypeFromSubrange (location_t location, char *name,
                                 tree subrangeType, tree lowval, tree highval, int ispacked)
{
  m2assert_AssertLocation (location);
  lowval = m2expr_FoldAndStrip (lowval);
  highval = m2expr_FoldAndStrip (highval);

#if 0
  if (broken_set_debugging_info)
    return unsigned_type_node;
  else
#endif
    if (ispacked)
    {
      tree noelements = m2expr_BuildAdd (
	  location, m2expr_BuildSub (location, highval, lowval, FALSE),
          integer_one_node, FALSE);
      highval = m2expr_FoldAndStrip (m2expr_BuildSub (
            location, m2expr_BuildLSL (location, m2expr_GetWordOne (location),
                                       noelements, FALSE),
            m2expr_GetIntegerOne (location), FALSE));
      lowval = m2expr_GetIntegerZero (location);
      return m2type_BuildSmallestTypeRange (location, lowval, highval);
    }
  else
    return unsigned_type_node;
}

/* build_m2_size_set_type - build and return a set type with,
   precision, bits.  */

static tree
build_m2_size_set_type (location_t location, int precision)
{
  tree bitnum_type_node
      = build_range_type (m2tree_skip_type_decl (m2type_GetCardinalType ()),
                          m2decl_BuildIntegerConstant (0),
                          m2decl_BuildIntegerConstant (precision - 1));
  layout_type (bitnum_type_node);
  m2assert_AssertLocation (location);

  if (broken_set_debugging_info)
    return unsigned_type_node;

  ASSERT ((COMPLETE_TYPE_P (bitnum_type_node)), bitnum_type_node);

  return m2type_BuildSetTypeFromSubrange (
      location, NULL, bitnum_type_node, m2decl_BuildIntegerConstant (0),
      m2decl_BuildIntegerConstant (precision - 1), FALSE);
}

/* build_m2_specific_size_type - build a specific data type matching
   number of bits, precision, whether it, is_signed.  It creates a
   set type if base == SET_TYPE or returns the already created real,
   if REAL_TYPE is specified.  */

static tree
build_m2_specific_size_type (location_t location, enum tree_code base,
                             int precision, int is_signed)
{
  tree c;

  m2assert_AssertLocation (location);

  c = make_node (base);
  TYPE_PRECISION (c) = precision;

  if (base == REAL_TYPE)
    {
      if (!float_mode_for_size (TYPE_PRECISION (c)).exists ())
        return NULL;
      layout_type (c);
    }
  else if (base == SET_TYPE)
    return build_m2_size_set_type (location, precision);
  else
    {
      TYPE_SIZE (c) = 0;

      if (is_signed)
        {
          fixup_signed_type (c);
          TYPE_UNSIGNED (c) = FALSE;
        }
      else
        {
          fixup_unsigned_type (c);
          TYPE_UNSIGNED (c) = TRUE;
        }
    }

  return c;
}

/* BuildSmallestTypeRange - returns the smallest INTEGER_TYPE which
   is sufficient to contain values: low..high.  */

tree
m2type_BuildSmallestTypeRange (location_t location, tree low, tree high)
{
  tree bits;

  m2assert_AssertLocation (location);
  low = fold (low);
  high = fold (high);
  bits = fold (noBitsRequired (
      m2expr_BuildAdd (location, m2expr_BuildSub (location, high, low, FALSE),
                       m2expr_GetIntegerOne (location), FALSE)));
  return build_m2_specific_size_type (location, INTEGER_TYPE,
                                      TREE_INT_CST_LOW (bits),
                                      tree_int_cst_sgn (low) < 0);
}

/* GetTreeType - returns TREE_TYPE (t).  */

tree
m2type_GetTreeType (tree t)
{
  return TREE_TYPE (t);
}

/* finish_build_pointer_type - finish building a POINTER_TYPE node.
   necessary to solve self references in procedure types.  */

/* code taken from tree.c:build_pointer_type_for_mode.  */

static tree
finish_build_pointer_type (tree t, tree to_type, enum machine_mode mode,
                           bool can_alias_all)
{
  TREE_TYPE (t) = to_type;
  SET_TYPE_MODE (t, mode);
  TYPE_REF_CAN_ALIAS_ALL (t) = can_alias_all;
  TYPE_NEXT_PTR_TO (t) = TYPE_POINTER_TO (to_type);
  TYPE_POINTER_TO (to_type) = t;

  /* Lay out the type.  */
  /* layout_type (t);  */
  layout_type (t);

  return t;
}

/* BuildParameterDeclaration - creates and returns one parameter
   from, name, and, type.  It appends this parameter to the internal
   param_type_list.  */

tree
m2type_BuildProcTypeParameterDeclaration (location_t location, tree type,
                                          int isreference)
{
  m2assert_AssertLocation (location);
  ASSERT_BOOL (isreference);
  type = m2tree_skip_type_decl (type);
  if (isreference)
    type = build_reference_type (type);

  param_type_list = tree_cons (NULL_TREE, type, param_type_list);
  return type;
}

/* BuildEndFunctionType - build a function type which would return a,
   value.  The arguments have been created by
   BuildParameterDeclaration.  */

tree
m2type_BuildEndFunctionType (tree func, tree return_type, int uses_varargs)
{
  tree last;

  if (return_type == NULL_TREE)
    return_type = void_type_node;
  else
    return_type = m2tree_skip_type_decl (return_type);

  if (uses_varargs)
    {
      if (param_type_list != NULL_TREE)
        {
          param_type_list = nreverse (param_type_list);
          last = param_type_list;
          param_type_list = nreverse (param_type_list);
          gcc_assert (last != void_list_node);
        }
    }
  else if (param_type_list == NULL_TREE)
    param_type_list = void_list_node;
  else
    {
      param_type_list = nreverse (param_type_list);
      last = param_type_list;
      param_type_list = nreverse (param_type_list);
      TREE_CHAIN (last) = void_list_node;
    }
  param_type_list = build_function_type (return_type, param_type_list);

  func = finish_build_pointer_type (func, param_type_list, ptr_mode, false);
  TYPE_SIZE (func) = 0;
  layout_type (func);
  return func;
}

/* BuildStartFunctionType - creates a pointer type, necessary to
   create a function type.  */

tree
m2type_BuildStartFunctionType (location_t location ATTRIBUTE_UNUSED,
                               char *name ATTRIBUTE_UNUSED)
{
  tree n = make_node (POINTER_TYPE);

  m2assert_AssertLocation (location);
  return n;
}

/* InitFunctionTypeParameters - resets the current function type
   parameter list.  */

void
m2type_InitFunctionTypeParameters (void)
{
  param_type_list = NULL_TREE;
}

/* gm2_finish_decl - */

static void
gm2_finish_decl (location_t location, tree decl)
{
  tree type = TREE_TYPE (decl);
  int was_incomplete = (DECL_SIZE (decl) == 0);

  m2assert_AssertLocation (location);
  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (DECL_SIZE (decl) == 0 && TREE_TYPE (decl) != error_mark_node
          && COMPLETE_TYPE_P (TREE_TYPE (decl)))
        layout_decl (decl, 0);

      if (DECL_SIZE (decl) == 0
          /* Don't give an error if we already gave one earlier.  */
          && TREE_TYPE (decl) != error_mark_node)
        {
          error_at (location, "storage size of %q+D isn%'t known", decl);
          TREE_TYPE (decl) = error_mark_node;
        }

      if ((DECL_EXTERNAL (decl) || TREE_STATIC (decl))
          && DECL_SIZE (decl) != 0)
        {
          if (TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST)
            m2expr_ConstantExpressionWarning (DECL_SIZE (decl));
          else
            error_at (location, "storage size of %q+D isn%'t constant", decl);
        }

      if (TREE_USED (type))
        TREE_USED (decl) = 1;
    }

  /* Output the assembler code and/or RTL code for variables and
  functions, unless the type is an undefined structure or union.  If
  not, it will get done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL)
    {
      if (DECL_FILE_SCOPE_P (decl))
        {
          if (DECL_INITIAL (decl) == NULL_TREE
              || DECL_INITIAL (decl) == error_mark_node)

            /* Don't output anything when a tentative file-scope definition is
            seen.  But at end of compilation, do output code for them.  */
            DECL_DEFER_OUTPUT (decl) = 1;
          rest_of_decl_compilation (decl, true, 0);
        }

      if (!DECL_FILE_SCOPE_P (decl))
        {

          /* Recompute the RTL of a local array now if it used to be an
          incomplete type.  */
          if (was_incomplete && !TREE_STATIC (decl) && !DECL_EXTERNAL (decl))
            {
              /* If we used it already as memory, it must stay in memory.  */
              TREE_ADDRESSABLE (decl) = TREE_USED (decl);
              /* If it's still incomplete now, no init will save it.  */
              if (DECL_SIZE (decl) == 0)
                DECL_INITIAL (decl) = 0;
            }
        }
    }

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      if (!DECL_FILE_SCOPE_P (decl)
          && variably_modified_type_p (TREE_TYPE (decl), NULL_TREE))
        m2block_pushDecl (build_stmt (location, DECL_EXPR, decl));

      rest_of_decl_compilation (decl, DECL_FILE_SCOPE_P (decl), 0);
    }
}

/* BuildVariableArrayAndDeclare - creates a variable length array.
   high is the maximum legal elements (which is a runtime variable).
   This creates and array index, array type and local variable.  */

tree
m2type_BuildVariableArrayAndDeclare (location_t location, tree elementtype,
                                     tree high, char *name, tree scope)
{
  tree indextype = build_index_type (variable_size (high));
  tree arraytype = build_array_type (elementtype, indextype);
  tree id = get_identifier (name);
  tree decl;

  m2assert_AssertLocation (location);
  decl = build_decl (location, VAR_DECL, id, arraytype);

  DECL_EXTERNAL (decl) = FALSE;
  TREE_PUBLIC (decl) = TRUE;
  DECL_CONTEXT (decl) = scope;
  TREE_USED (arraytype) = TRUE;
  TREE_USED (decl) = TRUE;

  m2block_pushDecl (decl);

  gm2_finish_decl (location, indextype);
  gm2_finish_decl (location, arraytype);
  add_stmt (location, build_stmt (location, DECL_EXPR, decl));

  return decl;
}

static tree
build_m2_iso_word_node (location_t location, int loc)
{
  tree c;

  m2assert_AssertLocation (location);
  /* Define `WORD' as specified in ISO m2

WORD = ARRAY [0..SizeOfWord / SizeOfLoc] OF LOC ; */

  if (m2decl_GetBitsPerInt () == BITS_PER_UNIT)
    c = m2type_GetISOLocType ();
  else
    c = gm2_build_array_type (
        m2type_GetISOLocType (),
        m2type_BuildArrayIndexType (
            m2expr_GetIntegerZero (location),
            (m2expr_BuildSub (location,
                              m2decl_BuildIntegerConstant (
                                  m2decl_GetBitsPerInt () / BITS_PER_UNIT),
                              m2expr_GetIntegerOne (location), FALSE))),
        loc);
  return c;
}

static tree
build_m2_iso_byte_node (location_t location, int loc)
{
  tree c;

  /* Define `BYTE' as specified in ISO m2

BYTE = ARRAY [0..SizeOfByte / SizeOfLoc] OF LOC ; */

  if (BITS_PER_UNIT == 8)
    c = m2type_GetISOLocType ();
  else
    c = gm2_build_array_type (
        m2type_GetISOLocType (),
        m2type_BuildArrayIndexType (
            m2expr_GetIntegerZero (location),
            m2decl_BuildIntegerConstant (BITS_PER_UNIT / 8)),
        loc);
  return c;
}

/* m2type_InitSystemTypes - initialise loc and word derivatives.  */

void
m2type_InitSystemTypes (location_t location, int loc)
{
  m2assert_AssertLocation (location);

  m2_iso_word_type_node = build_m2_iso_word_node (location, loc);
  m2_iso_byte_type_node = build_m2_iso_byte_node (location, loc);

  m2_word16_type_node = build_m2_word16_type_node (location, loc);
  m2_word32_type_node = build_m2_word32_type_node (location, loc);
  m2_word64_type_node = build_m2_word64_type_node (location, loc);
}

static tree
build_m2_integer_node (void)
{
  return m2type_GetIntegerType ();
}

static tree
build_m2_cardinal_node (void)
{
  return m2type_GetCardinalType ();
}

static tree
build_m2_char_node (void)
{
  tree c;

  /* Define `CHAR', to be an unsigned char.  */

  c = make_unsigned_type (CHAR_TYPE_SIZE);
  layout_type (c);
  return c;
}

static tree
build_m2_short_real_node (void)
{
  tree c;

  /* Define `REAL'.  */

  c = make_node (REAL_TYPE);
  TYPE_PRECISION (c) = FLOAT_TYPE_SIZE;
  layout_type (c);

  return c;
}

static tree
build_m2_real_node (void)
{
  tree c;

  /* Define `REAL'.  */

  c = make_node (REAL_TYPE);
  TYPE_PRECISION (c) = DOUBLE_TYPE_SIZE;
  layout_type (c);

  return c;
}

static tree
build_m2_long_real_node (void)
{
  tree c;

  /* Define `LONGREAL'.  */

  c = make_node (REAL_TYPE);
  TYPE_PRECISION (c) = LONG_DOUBLE_TYPE_SIZE;
  layout_type (c);

  return c;
}

static tree
build_m2_long_int_node (void)
{
  tree c;

  /* Define `LONGINT'.  */

  c = make_signed_type (LONG_LONG_TYPE_SIZE);
  layout_type (c);

  return c;
}

static tree
build_m2_long_card_node (void)
{
  tree c;

  /* Define `LONGCARD'.  */

  c = make_unsigned_type (LONG_LONG_TYPE_SIZE);
  layout_type (c);

  return c;
}

static tree
build_m2_short_int_node (void)
{
  tree c;

  /* Define `SHORTINT'.  */

  c = make_signed_type (SHORT_TYPE_SIZE);
  layout_type (c);

  return c;
}

static tree
build_m2_short_card_node (void)
{
  tree c;

  /* Define `SHORTCARD'.  */

  c = make_unsigned_type (SHORT_TYPE_SIZE);
  layout_type (c);

  return c;
}

static tree
build_m2_iso_loc_node (void)
{
  tree c;

  /* Define `LOC' as specified in ISO m2.  */

  c = make_node (INTEGER_TYPE);
  TYPE_PRECISION (c) = BITS_PER_UNIT;
  TYPE_SIZE (c) = 0;

  fixup_unsigned_type (c);
  TYPE_UNSIGNED (c) = 1;

  return c;
}

static tree
build_m2_integer8_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, INTEGER_TYPE, 8, TRUE);
}

static tree
build_m2_integer16_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, INTEGER_TYPE, 16, TRUE);
}

static tree
build_m2_integer32_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, INTEGER_TYPE, 32, TRUE);
}

static tree
build_m2_integer64_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, INTEGER_TYPE, 64, TRUE);
}

static tree
build_m2_cardinal8_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, INTEGER_TYPE, 8, FALSE);
}

static tree
build_m2_cardinal16_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, INTEGER_TYPE, 16, FALSE);
}

static tree
build_m2_cardinal32_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, INTEGER_TYPE, 32, FALSE);
}

static tree
build_m2_cardinal64_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, INTEGER_TYPE, 64, FALSE);
}

static tree
build_m2_bitset8_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  if (broken_set_debugging_info)
    return build_m2_specific_size_type (location, INTEGER_TYPE, 8, FALSE);
  else
    return build_m2_specific_size_type (location, SET_TYPE, 8, FALSE);
}

static tree
build_m2_bitset16_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  if (broken_set_debugging_info)
    return build_m2_specific_size_type (location, INTEGER_TYPE, 16, FALSE);
  else
    return build_m2_specific_size_type (location, SET_TYPE, 16, FALSE);
}

static tree
build_m2_bitset32_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  if (broken_set_debugging_info)
    return build_m2_specific_size_type (location, INTEGER_TYPE, 32, FALSE);
  else
    return build_m2_specific_size_type (location, SET_TYPE, 32, FALSE);
}

static tree
build_m2_real32_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, REAL_TYPE, 32, TRUE);
}

static tree
build_m2_real64_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, REAL_TYPE, 64, TRUE);
}

static tree
build_m2_real96_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, REAL_TYPE, 96, TRUE);
}

static tree
build_m2_real128_type_node (location_t location)
{
  m2assert_AssertLocation (location);
  return build_m2_specific_size_type (location, REAL_TYPE, 128, TRUE);
}

static tree
build_m2_complex_type_from (tree scalar_type)
{
  tree new_type;

  if (scalar_type == NULL)
    return NULL;
  if (scalar_type == float_type_node)
    return complex_float_type_node;
  if (scalar_type == double_type_node)
    return complex_double_type_node;
  if (scalar_type == long_double_type_node)
    return complex_long_double_type_node;

  new_type = make_node (COMPLEX_TYPE);
  TREE_TYPE (new_type) = scalar_type;
  layout_type (new_type);
  return new_type;
}

static tree
build_m2_complex_type_node (void)
{
  return build_m2_complex_type_from (m2_real_type_node);
}

static tree
build_m2_long_complex_type_node (void)
{
  return build_m2_complex_type_from (m2_long_real_type_node);
}

static tree
build_m2_short_complex_type_node (void)
{
  return build_m2_complex_type_from (m2_short_real_type_node);
}

static tree
build_m2_complex32_type_node (void)
{
  return build_m2_complex_type_from (m2_real32_type_node);
}

static tree
build_m2_complex64_type_node (void)
{
  return build_m2_complex_type_from (m2_real64_type_node);
}

static tree
build_m2_complex96_type_node (void)
{
  return build_m2_complex_type_from (m2_real96_type_node);
}

static tree
build_m2_complex128_type_node (void)
{
  return build_m2_complex_type_from (m2_real128_type_node);
}

static tree
build_m2_cardinal_address_type_node (location_t location)
{
  tree size = size_in_bytes (ptr_type_node);
  int bits = TREE_INT_CST_LOW (size) * BITS_PER_UNIT;

  return build_m2_specific_size_type (location, INTEGER_TYPE, bits, FALSE);
}

/* InitBaseTypes - */

void
m2type_InitBaseTypes (location_t location)
{
  m2assert_AssertLocation (location);
  m2block_init ();

  ptr_type_node = build_pointer_type (void_type_node);

  proc_type_node
      = build_pointer_type (build_function_type (void_type_node, NULL_TREE));

  bitset_type_node = build_bitset_type (location);
  m2_char_type_node = build_m2_char_node ();
  m2_integer_type_node = build_m2_integer_node ();
  m2_cardinal_type_node = build_m2_cardinal_node ();
  m2_short_real_type_node = build_m2_short_real_node ();
  m2_real_type_node = build_m2_real_node ();
  m2_long_real_type_node = build_m2_long_real_node ();
  m2_long_int_type_node = build_m2_long_int_node ();
  m2_long_card_type_node = build_m2_long_card_node ();
  m2_short_int_type_node = build_m2_short_int_node ();
  m2_short_card_type_node = build_m2_short_card_node ();
  m2_z_type_node = build_m2_long_int_node ();
  m2_integer8_type_node = build_m2_integer8_type_node (location);
  m2_integer16_type_node = build_m2_integer16_type_node (location);
  m2_integer32_type_node = build_m2_integer32_type_node (location);
  m2_integer64_type_node = build_m2_integer64_type_node (location);
  m2_cardinal8_type_node = build_m2_cardinal8_type_node (location);
  m2_cardinal16_type_node = build_m2_cardinal16_type_node (location);
  m2_cardinal32_type_node = build_m2_cardinal32_type_node (location);
  m2_cardinal64_type_node = build_m2_cardinal64_type_node (location);
  m2_bitset8_type_node = build_m2_bitset8_type_node (location);
  m2_bitset16_type_node = build_m2_bitset16_type_node (location);
  m2_bitset32_type_node = build_m2_bitset32_type_node (location);
  m2_real32_type_node = build_m2_real32_type_node (location);
  m2_real64_type_node = build_m2_real64_type_node (location);
  m2_real96_type_node = build_m2_real96_type_node (location);
  m2_real128_type_node = build_m2_real128_type_node (location);
  m2_complex_type_node = build_m2_complex_type_node ();
  m2_long_complex_type_node = build_m2_long_complex_type_node ();
  m2_short_complex_type_node = build_m2_short_complex_type_node ();
  m2_c_type_node = build_m2_long_complex_type_node ();
  m2_complex32_type_node = build_m2_complex32_type_node ();
  m2_complex64_type_node = build_m2_complex64_type_node ();
  m2_complex96_type_node = build_m2_complex96_type_node ();
  m2_complex128_type_node = build_m2_complex128_type_node ();
  m2_iso_loc_type_node = build_m2_iso_loc_node ();

  m2_cardinal_address_type_node
      = build_m2_cardinal_address_type_node (location);

  m2_packed_boolean_type_node = build_nonstandard_integer_type (1, TRUE);

  m2builtins_init (location);
  m2except_InitExceptions (location);
  m2expr_init (location);
}

/* BuildStartType - given a, type, with a, name, return a GCC
   declaration of this type.  TYPE name = foo ;

the type, foo, maybe a partially created type (which has yet to be
   'gm2_finish_decl'ed.  */

tree
m2type_BuildStartType (location_t location, char *name, tree type)
{
  tree id = get_identifier (name);
  tree decl, tem;

  m2assert_AssertLocation (location);
  ASSERT (m2tree_is_type (type), type);
  type = m2tree_skip_type_decl (type);
  decl = build_decl (location, TYPE_DECL, id, type);

  tem = m2block_pushDecl (decl);
  ASSERT (tem == decl, decl);
  ASSERT (m2tree_is_type (decl), decl);

  return tem;
}

/* BuildEndType - finish declaring, type, and return, type.  */

tree
m2type_BuildEndType (location_t location, tree type)
{
  m2assert_AssertLocation (location);
  layout_type (TREE_TYPE (type));
  gm2_finish_decl (location, type);
  return type;
}

/* DeclareKnownType - given a, type, with a, name, return a GCC
   declaration of this type.  TYPE name = foo ; */

tree
m2type_DeclareKnownType (location_t location, char *name, tree type)
{
  m2assert_AssertLocation (location);
  return m2type_BuildEndType (location,
                              m2type_BuildStartType (location, name, type));
}

/* GetDefaultType - given a, type, with a, name, return a GCC
   declaration of this type.  Checks to see whether the type name has
   already been declared as a default type and if so it returns this
   declaration.  Otherwise it declares the type.  In Modula-2 this is
   equivalent to:

TYPE name = type ;

We need this function as the initialization to gccgm2.c will declare
   C default types and _some_ M2 default types.  */

tree
m2type_GetDefaultType (location_t location, char *name, tree type)
{
  tree id = maybe_get_identifier (name);

  m2assert_AssertLocation (location);
  if (id == NULL)
    {
      tree prev = type;
      tree t;

      while (prev != NULL)
        {
          if (TYPE_NAME (prev) == NULL)
            TYPE_NAME (prev) = get_identifier (name);
          prev = TREE_TYPE (prev);
        }
      t = m2type_DeclareKnownType (location, name, type);
      return t;
    }
  else
    return id;
}

tree
do_min_real (tree type)
{
  REAL_VALUE_TYPE r;
  char buf[128];
  enum machine_mode mode = TYPE_MODE (type);

  get_max_float (REAL_MODE_FORMAT (mode), buf, sizeof (buf), false);
  real_from_string (&r, buf);
  return build1 (NEGATE_EXPR, type, build_real (type, r));
}

/* GetMinFrom - given a, type, return a constant representing the
   minimum legal value.  */

tree
m2type_GetMinFrom (location_t location, tree type)
{
  m2assert_AssertLocation (location);

  if (type == m2_real_type_node || type == m2type_GetRealType ())
    return do_min_real (type);
  if (type == m2_long_real_type_node || type == m2type_GetLongRealType ())
    return do_min_real (type);
  if (type == m2_short_real_type_node || type == m2type_GetShortRealType ())
    return do_min_real (type);
  if (type == ptr_type_node)
    return m2expr_GetPointerZero (location);

  return TYPE_MIN_VALUE (m2tree_skip_type_decl (type));
}

tree
do_max_real (tree type)
{
  REAL_VALUE_TYPE r;
  char buf[128];
  enum machine_mode mode = TYPE_MODE (type);

  get_max_float (REAL_MODE_FORMAT (mode), buf, sizeof (buf), false);
  real_from_string (&r, buf);
  return build_real (type, r);
}

/* GetMaxFrom - given a, type, return a constant representing the
   maximum legal value.  */

tree
m2type_GetMaxFrom (location_t location, tree type)
{
  m2assert_AssertLocation (location);

  if (type == m2_real_type_node || type == m2type_GetRealType ())
    return do_max_real (type);
  if (type == m2_long_real_type_node || type == m2type_GetLongRealType ())
    return do_max_real (type);
  if (type == m2_short_real_type_node || type == m2type_GetShortRealType ())
    return do_max_real (type);
  if (type == ptr_type_node)
    return fold (m2expr_BuildSub (location, m2expr_GetPointerZero (location),
                                  m2expr_GetPointerOne (location), FALSE));

  return TYPE_MAX_VALUE (m2tree_skip_type_decl (type));
}

/* BuildTypeDeclaration - adds the, type, to the current statement
   list.  */

void
m2type_BuildTypeDeclaration (location_t location, tree type)
{
  enum tree_code code = TREE_CODE (type);

  m2assert_AssertLocation (location);
  if (code == TYPE_DECL || code == RECORD_TYPE || code == POINTER_TYPE)
    {
      m2block_pushDecl (build_decl (location, TYPE_DECL, NULL, type));
    }
  else if (code == VAR_DECL)
    {
      m2type_BuildTypeDeclaration (location, TREE_TYPE (type));
      m2block_pushDecl (
          build_stmt (location, DECL_EXPR,
                      type)); /* is this safe?  --fixme-- gaius got here.  */
    }

#if 0
    add_stmt (build_stmt (location, DECL_EXPR, type));
    add_stmt (build_stmt (location, DECL_EXPR, build_decl (location, TYPE_DECL, NULL, type)));
#endif
}

/* Begin compiling the definition of an enumeration type.  NAME is
   its name (or null if anonymous).  Returns the type object, as yet
   incomplete.  Also records info about it so that build_enumerator may
   be used to declare the individual values as they are read.  */

static tree
gm2_start_enum (location_t location, tree name, int ispacked)
{
  tree enumtype = make_node (ENUMERAL_TYPE);

  m2assert_AssertLocation (location);
  if (TYPE_VALUES (enumtype) != 0)
    {
      /* This enum is a named one that has been declared already.  */
      error_at (location, "redeclaration of enum %qs",
                IDENTIFIER_POINTER (name));

      /* Completely replace its old definition.  The old enumerators remain
      defined, however.  */
      TYPE_VALUES (enumtype) = 0;
    }

  TYPE_PACKED (enumtype) = ispacked;
  TREE_TYPE (enumtype) = m2type_GetIntegerType ();

  /* This is required as rest_of_type_compilation will use this field
     when called from gm2_finish_enum.

     Create a fake NULL-named TYPE_DECL node whose TREE_TYPE will be the
     tagged type we just added to the current scope.  This fake NULL-named
     TYPE_DECL node helps dwarfout.c to know when it needs to output a
     representation of a tagged type, and it also gives us a convenient
     place to record the "scope start" address for the tagged type.  */

  TYPE_STUB_DECL (enumtype) = m2block_pushDecl (
      build_decl (location, TYPE_DECL, NULL_TREE, enumtype));

  return enumtype;
}

/* After processing and defining all the values of an enumeration
   type, install their decls in the enumeration type and finish it off.
   ENUMTYPE is the type object, VALUES a list of decl-value pairs, and
   ATTRIBUTES are the specified attributes.  Returns ENUMTYPE.  */

static tree
gm2_finish_enum (location_t location, tree enumtype, tree values)
{
  tree pair, tem;
  tree minnode = 0, maxnode = 0;
  int precision;
  signop sign;

  /* Calculate the maximum value of any enumerator in this type.  */

  if (values == error_mark_node)
    minnode = maxnode = integer_zero_node;
  else
    {
      minnode = maxnode = TREE_VALUE (values);
      for (pair = TREE_CHAIN (values); pair; pair = TREE_CHAIN (pair))
        {
          tree value = TREE_VALUE (pair);
          if (tree_int_cst_lt (maxnode, value))
            maxnode = value;
          if (tree_int_cst_lt (value, minnode))
            minnode = value;
        }
    }

  /* Construct the final type of this enumeration.  It is the same as
     one of the integral types - the narrowest one that fits, except that
     normally we only go as narrow as int - and signed iff any of the
     values are negative.  */
  sign = (tree_int_cst_sgn (minnode) >= 0) ? UNSIGNED : SIGNED;
  precision = MAX (tree_int_cst_min_precision (minnode, sign),
                   tree_int_cst_min_precision (maxnode, sign));

  if (precision > TYPE_PRECISION (integer_type_node))
    {
      warning (0, "enumeration values exceed range of integer");
      tem = long_long_integer_type_node;
    }
  else if (TYPE_PACKED (enumtype))
    tem = m2type_BuildSmallestTypeRange (location, minnode, maxnode);
  else
    tem = sign == UNSIGNED ? unsigned_type_node : integer_type_node;

  TYPE_MIN_VALUE (enumtype) = TYPE_MIN_VALUE (tem);
  TYPE_MAX_VALUE (enumtype) = TYPE_MAX_VALUE (tem);
  TYPE_UNSIGNED (enumtype) = TYPE_UNSIGNED (tem);
  TYPE_SIZE (enumtype) = 0;

  /* If the precision of the type was specific with an attribute and it
     was too small, give an error.  Otherwise, use it.  */
  if (TYPE_PRECISION (enumtype))
    {
      if (precision > TYPE_PRECISION (enumtype))
        error ("specified mode too small for enumeral values");
    }
  else
    TYPE_PRECISION (enumtype) = TYPE_PRECISION (tem);

  layout_type (enumtype);

  if (values != error_mark_node)
    {

      /* Change the type of the enumerators to be the enum type.  We need
         to do this irrespective of the size of the enum, for proper type
         checking.  Replace the DECL_INITIALs of the enumerators, and the
         value slots of the list, with copies that have the enum type; they
         cannot be modified in place because they may be shared (e.g.
         integer_zero_node) Finally, change the purpose slots to point to the
         names of the decls.  */
      for (pair = values; pair; pair = TREE_CHAIN (pair))
        {
          tree enu = TREE_PURPOSE (pair);
          tree ini = DECL_INITIAL (enu);

          TREE_TYPE (enu) = enumtype;

          if (TREE_TYPE (ini) != integer_type_node)
            ini = convert (enumtype, ini);

          DECL_INITIAL (enu) = ini;
          TREE_PURPOSE (pair) = DECL_NAME (enu);
          TREE_VALUE (pair) = ini;
        }

      TYPE_VALUES (enumtype) = values;
    }

  /* Fix up all variant types of this enum type.  */
  for (tem = TYPE_MAIN_VARIANT (enumtype); tem; tem = TYPE_NEXT_VARIANT (tem))
    {
      if (tem == enumtype)
        continue;
      TYPE_VALUES (tem) = TYPE_VALUES (enumtype);
      TYPE_MIN_VALUE (tem) = TYPE_MIN_VALUE (enumtype);
      TYPE_MAX_VALUE (tem) = TYPE_MAX_VALUE (enumtype);
      TYPE_SIZE (tem) = TYPE_SIZE (enumtype);
      TYPE_SIZE_UNIT (tem) = TYPE_SIZE_UNIT (enumtype);
      SET_TYPE_MODE (tem, TYPE_MODE (enumtype));
      TYPE_PRECISION (tem) = TYPE_PRECISION (enumtype);
      SET_TYPE_ALIGN (tem, TYPE_ALIGN (enumtype));
      TYPE_USER_ALIGN (tem) = TYPE_USER_ALIGN (enumtype);
      TYPE_UNSIGNED (tem) = TYPE_UNSIGNED (enumtype);
      TYPE_LANG_SPECIFIC (tem) = TYPE_LANG_SPECIFIC (enumtype);
    }

  /* Finish debugging output for this type.  */
  rest_of_type_compilation (enumtype, m2block_toplevel ());
  return enumtype;
}

/* BuildStartEnumeration - create an enumerated type in gcc.  */

tree
m2type_BuildStartEnumeration (location_t location, char *name, int ispacked)
{
  tree id;

  m2assert_AssertLocation (location);
  if ((name == NULL) || (strcmp (name, "") == 0))
    id = NULL_TREE;
  else
    id = get_identifier (name);

  return gm2_start_enum (location, id, ispacked);
}

/* BuildEndEnumeration - finish building the enumeration, it uses the
   enum list, enumvalues, and returns a enumeration type tree.  */

tree
m2type_BuildEndEnumeration (location_t location, tree enumtype,
                            tree enumvalues)
{
  tree finished ATTRIBUTE_UNUSED
      = gm2_finish_enum (location, enumtype, enumvalues);
  return enumtype;
}

/* Build and install a CONST_DECL for one value of the current
   enumeration type (one that was begun with start_enum).  Return a
   tree-list containing the CONST_DECL and its value.  Assignment of
   sequential values by default is handled here.  */

static tree
gm2_build_enumerator (location_t location, tree name, tree value)
{
  tree decl, type;

  m2assert_AssertLocation (location);
  /* Remove no-op casts from the value.  */
  if (value)
    STRIP_TYPE_NOPS (value);

  /* Now create a declaration for the enum value name.  */

  type = TREE_TYPE (value);

  decl = build_decl (location, CONST_DECL, name, type);
  DECL_INITIAL (decl) = convert (type, value);
  m2block_pushDecl (decl);

  return tree_cons (decl, value, NULL_TREE);
}

/* BuildEnumerator - build an enumerator and add it to the,
   enumvalues, list.  It returns a copy of the value.  --fixme-- why
   do this?  */

tree
m2type_BuildEnumerator (location_t location, char *name, tree value,
                        tree *enumvalues)
{
  tree id = get_identifier (name);
  tree copy_of_value = copy_node (value);
  tree gccenum = gm2_build_enumerator (location, id, copy_of_value);

  m2assert_AssertLocation (location);
  /* choose copy_of_value for enum value.  */
  *enumvalues = chainon (gccenum, *enumvalues);
  return copy_of_value;
}

/* BuildPointerType - returns a type which is a pointer to, totype.  */

tree
m2type_BuildPointerType (tree totype)
{
  return build_pointer_type (m2tree_skip_type_decl (totype));
}

/* BuildConstPointerType - returns a type which is a const pointer
   to, totype.  */

tree
m2type_BuildConstPointerType (tree totype)
{
  tree t = build_pointer_type (m2tree_skip_type_decl (totype));
  TYPE_READONLY (t) = TRUE;
  return t;
}

/* BuildSetType - creates a SET OF [lowval..highval].  */

tree
m2type_BuildSetType (location_t location, char *name, tree type, tree lowval,
                     tree highval, int ispacked)
{
  tree range = build_range_type (m2tree_skip_type_decl (type),
                                 m2expr_FoldAndStrip (lowval),
                                 m2expr_FoldAndStrip (highval));

  TYPE_PACKED (range) = ispacked;
  m2assert_AssertLocation (location);
  return m2type_BuildSetTypeFromSubrange (location, name, range,
                                          m2expr_FoldAndStrip (lowval),
                                          m2expr_FoldAndStrip (highval),
					  ispacked);
}

/* push_constructor - returns a new compound constructor frame.  */

static struct struct_constructor *
push_constructor (void)
{
  struct struct_constructor *p = ggc_alloc<struct_constructor> ();

  p->level = top_constructor;
  top_constructor = p;
  return p;
}

/* pop_constructor - throws away the top constructor frame on the
   stack.  */

static void
pop_constructor (struct struct_constructor *p)
{
  ASSERT_CONDITION (p
                    == top_constructor); /* p should be the top_constructor.  */
  top_constructor = top_constructor->level;
}

/* BuildStartSetConstructor - starts to create a set constant.
   Remember that type is really a record type.  */

void *
m2type_BuildStartSetConstructor (tree type)
{
  struct struct_constructor *p = push_constructor ();

  type = m2tree_skip_type_decl (type);
  layout_type (type);
  p->constructor_type = type;
  p->constructor_fields = TYPE_FIELDS (type);
  p->constructor_element_list = NULL_TREE;
  vec_alloc (p->constructor_elements, 1);
  return (void *)p;
}

/* BuildSetConstructorElement - adds, value, to the
   constructor_element_list.  */

void
m2type_BuildSetConstructorElement (void *p, tree value)
{
  struct struct_constructor *c = (struct struct_constructor *)p;

  if (value == NULL_TREE)
    {
      internal_error ("set type cannot be initialized with a NULL_TREE");
      return;
    }

  if (c->constructor_fields == NULL)
    {
      internal_error ("set type does not take another integer value");
      return;
    }

  c->constructor_element_list
      = tree_cons (c->constructor_fields, value, c->constructor_element_list);
  c->constructor_fields = TREE_CHAIN (c->constructor_fields);
}

/* BuildEndSetConstructor - finishes building a set constant.  */

tree
m2type_BuildEndSetConstructor (void *p)
{
  tree constructor;
  tree link;
  struct struct_constructor *c = (struct struct_constructor *)p;

  for (link = c->constructor_element_list; link; link = TREE_CHAIN (link))
    {
      tree field = TREE_PURPOSE (link);
      DECL_SIZE (field) = bitsize_int (SET_WORD_SIZE);
      DECL_BIT_FIELD (field) = 1;
    }

  constructor = build_constructor_from_list (
      c->constructor_type, nreverse (c->constructor_element_list));
  TREE_CONSTANT (constructor) = 1;
  TREE_STATIC (constructor) = 1;

  pop_constructor (c);

  return constructor;
}

/* BuildStartRecordConstructor - initializes a record compound
   constructor frame.  */

void *
m2type_BuildStartRecordConstructor (tree type)
{
  struct struct_constructor *p = push_constructor ();

  type = m2tree_skip_type_decl (type);
  layout_type (type);
  p->constructor_type = type;
  p->constructor_fields = TYPE_FIELDS (type);
  p->constructor_element_list = NULL_TREE;
  vec_alloc (p->constructor_elements, 1);
  return (void *)p;
}

/* BuildEndRecordConstructor - returns a tree containing the record
   compound literal.  */

tree
m2type_BuildEndRecordConstructor (void *p)
{
  struct struct_constructor *c = (struct struct_constructor *)p;
  tree constructor = build_constructor_from_list (
      c->constructor_type, nreverse (c->constructor_element_list));
  TREE_CONSTANT (constructor) = 1;
  TREE_STATIC (constructor) = 1;

  pop_constructor (c);

  return constructor;
}

/* BuildRecordConstructorElement - adds, value, to the
   constructor_element_list.  */

void
m2type_BuildRecordConstructorElement (void *p, tree value)
{
  m2type_BuildSetConstructorElement (p, value);
}

/* BuildStartArrayConstructor - initializes an array compound
   constructor frame.  */

void *
m2type_BuildStartArrayConstructor (tree type)
{
  struct struct_constructor *p = push_constructor ();

  type = m2tree_skip_type_decl (type);
  layout_type (type);
  p->constructor_type = type;
  p->constructor_fields = TREE_TYPE (type);
  p->constructor_element_list = NULL_TREE;
  vec_alloc (p->constructor_elements, 1);
  return (void *)p;
}

/* BuildEndArrayConstructor - returns a tree containing the array
   compound literal.  */

tree
m2type_BuildEndArrayConstructor (void *p)
{
  struct struct_constructor *c = (struct struct_constructor *)p;
  tree constructor;

  constructor
      = build_constructor (c->constructor_type, c->constructor_elements);
  TREE_CONSTANT (constructor) = TRUE;
  TREE_STATIC (constructor) = TRUE;

  pop_constructor (c);

  return constructor;
}

/* BuildArrayConstructorElement - adds, value, to the
   constructor_element_list.  */

void
m2type_BuildArrayConstructorElement (void *p, tree value, tree indice)
{
  struct struct_constructor *c = (struct struct_constructor *)p;
  constructor_elt celt;

  if (value == NULL_TREE)
    {
      internal_error ("array cannot be initialized with a NULL_TREE");
      return;
    }

  if (c->constructor_fields == NULL_TREE)
    {
      internal_error ("array type must be initialized");
      return;
    }

  if (c->constructor_fields != TREE_TYPE (value))
    {
      internal_error (
          "array element value must be the same type as its declaration");
      return;
    }

  celt.index = indice;
  celt.value = value;
  vec_safe_push (c->constructor_elements, celt);
}

/* BuildArrayStringConstructor - creates an array constructor for,
   arrayType, consisting of the character elements defined by, str,
   of, length, characters.  */

tree
m2type_BuildArrayStringConstructor (location_t location, tree arrayType,
                                    tree str, tree length)
{
  tree n;
  tree val;
  int i = 0;
  const char *p = TREE_STRING_POINTER (str);
  tree type = m2tree_skip_type_decl (TREE_TYPE (arrayType));
  struct struct_constructor *c
      = (struct struct_constructor *)m2type_BuildStartArrayConstructor (
          arrayType);
  char nul[1];
  int len = strlen (p);

  nul[0] = (char)0;

  m2assert_AssertLocation (location);
  n = m2expr_GetIntegerZero (location);
  while (m2expr_CompareTrees (n, length) < 0)
    {
      if (i < len)
        val = m2convert_BuildConvert (
            location, type, m2type_BuildCharConstant (location, &p[i]), FALSE);
      else
        val = m2type_BuildCharConstant (location, &nul[0]);
      m2type_BuildArrayConstructorElement (c, val, n);
      i += 1;
      n = m2expr_BuildAdd (location, n, m2expr_GetIntegerOne (location),
                           FALSE);
    }
  return m2type_BuildEndArrayConstructor (c);
}

/* BuildSubrangeType - creates a subrange of, type, with, lowval,
   highval.  */

tree
m2type_BuildSubrangeType (location_t location, char *name, tree type,
                          tree lowval, tree highval)
{
  tree range_type;

  m2assert_AssertLocation (location);
  type = m2tree_skip_type_decl (type);

  lowval = m2expr_FoldAndStrip (lowval);
  highval = m2expr_FoldAndStrip (highval);

  if (m2expr_TreeOverflow (lowval))
    error ("low bound for the subrange has overflowed");
  if (m2expr_TreeOverflow (highval))
    error ("high bound for the subrange has overflowed");

  /* First build a type with the base range.  */
  range_type = build_range_type (type, TYPE_MIN_VALUE (type),
				 TYPE_MAX_VALUE (type));

  TYPE_UNSIGNED (range_type) = TYPE_UNSIGNED (type);
#if 0
  /* Then set the actual range.  */
  SET_TYPE_RM_MIN_VALUE (range_type, lowval);
  SET_TYPE_RM_MAX_VALUE (range_type, highval);
#endif

  if ((name != NULL) && (strcmp (name, "") != 0))
    {
      /* declared as TYPE foo = [x..y];  */
      range_type = m2type_DeclareKnownType (location, name, range_type);
      layout_type (m2tree_skip_type_decl (range_type));
    }

  return range_type;
}

/* BuildCharConstantChar - creates a character constant given a character, ch.  */

tree
m2type_BuildCharConstantChar (location_t location, char ch)
{
  tree id = build_int_cst (char_type_node, (int) ch);
  id = m2convert_BuildConvert (location, m2type_GetM2CharType (), id, FALSE);
  return m2block_RememberConstant (id);
}

/* BuildCharConstant - creates a character constant given a, string.  */

tree
m2type_BuildCharConstant (location_t location, const char *string)
{
  return m2type_BuildCharConstantChar (location, string[0]);
}

/* RealToTree - convert a real number into a Tree.  */

tree
m2type_RealToTree (char *name)
{
  return build_real (
      m2type_GetLongRealType (),
      REAL_VALUE_ATOF (name, TYPE_MODE (m2type_GetLongRealType ())));
}

/* gm2_start_struct - start to create a struct.  */

static tree
gm2_start_struct (location_t location, enum tree_code code, char *name)
{
  tree s = make_node (code);
  tree id;

  m2assert_AssertLocation (location);
  if ((name == NULL) || (strcmp (name, "") == 0))
    id = NULL_TREE;
  else
    id = get_identifier (name);

  TYPE_PACKED (s) = FALSE; /* this is set TRUE later if necessary.  */

  m2block_pushDecl (build_decl (location, TYPE_DECL, id, s));
  return s;
}

/* BuildStartRecord - return a RECORD tree.  */

tree
m2type_BuildStartRecord (location_t location, char *name)
{
  m2assert_AssertLocation (location);
  return gm2_start_struct (location, RECORD_TYPE, name);
}

/* BuildStartUnion - return a union tree.  */

tree
m2type_BuildStartUnion (location_t location, char *name)
{
  m2assert_AssertLocation (location);
  return gm2_start_struct (location, UNION_TYPE, name);
}

/* m2type_BuildStartVarient - builds a varient record.  It creates a
   record field which has a, name, and whose type is a union.  */

tree
m2type_BuildStartVarient (location_t location, char *name)
{
  tree varient = m2type_BuildStartUnion (location, name);
  tree field = m2type_BuildStartFieldRecord (location, name, varient);
  m2assert_AssertLocation (location);
  return field;
}

/* m2type_BuildEndVarient - finish the varientField by calling
   decl_finish and also finish the type of varientField (which is a
   union).  */

tree
m2type_BuildEndVarient (location_t location, tree varientField,
                        tree varientList, int isPacked)
{
  tree varient = TREE_TYPE (varientField);
  m2assert_AssertLocation (location);
  varient = m2type_BuildEndRecord (location, varient, varientList, isPacked);
  gm2_finish_decl (location, varientField);
  return varientField;
}

/* m2type_BuildStartFieldVarient - builds a field varient record.  It
   creates a record field which has a, name, and whose type is a
   record.  */

tree
m2type_BuildStartFieldVarient (location_t location, char *name)
{
  tree record = m2type_BuildStartRecord (location, name);
  tree field = m2type_BuildStartFieldRecord (location, name, record);
  m2assert_AssertLocation (location);
  return field;
}

/* BuildEndRecord - a heavily pruned finish_struct from c-decl.c.  It
   sets the context for each field to, t, propagates isPacked
   throughout the fields in the structure.  */

tree
m2type_BuildEndRecord (location_t location, tree record, tree fieldlist,
                       int isPacked)
{
  tree x, d;

  m2assert_AssertLocation (location);

  /* If this type was previously laid out as a forward reference, make
     sure we lay it out again.  */

  TYPE_SIZE (record) = 0;

  /* Install struct as DECL_CONTEXT of each field decl.  Also process
     specified field sizes, found in the DECL_INITIAL, storing 0 there
     after the type has been changed to precision equal to its width,
     rather than the precision of the specified standard type.  (Correct
     layout requires the original type to have been preserved until now).  */

  for (x = fieldlist; x; x = TREE_CHAIN (x))
    {
      DECL_CONTEXT (x) = record;

      if (TYPE_PACKED (record) && TYPE_ALIGN (TREE_TYPE (x)) > BITS_PER_UNIT)
        DECL_PACKED (x) = 1;

      if (isPacked)
        {
          DECL_PACKED (x) = 1;
          DECL_BIT_FIELD (x) = 1;
        }
    }

  /* Now we have the nearly final fieldlist.  Record it, then lay out
     the structure or union (including the fields).  */

  TYPE_FIELDS (record) = fieldlist;
  layout_type (record);

  /* Now we have the truly final field list.  Store it in this type and
     in the variants.  */

  ASSERT_CONDITION (TYPE_FIELDS (record)
                    == fieldlist); /* --fixme-- remove this.  */
  TYPE_FIELDS (record)
      = fieldlist; /* and this once the condition always passes.  */

  for (x = TYPE_MAIN_VARIANT (record); x; x = TYPE_NEXT_VARIANT (x))
    {
      TYPE_FIELDS (x) = TYPE_FIELDS (record);
      TYPE_LANG_SPECIFIC (x) = TYPE_LANG_SPECIFIC (record);
      SET_TYPE_ALIGN (x, TYPE_ALIGN (record));
      TYPE_USER_ALIGN (x) = TYPE_USER_ALIGN (record);
    }

  d = build_decl (location, TYPE_DECL, NULL, record);
  TYPE_STUB_DECL (record) = d;

  /* Finish debugging output for this type.  This must be done after we have
   * called build_decl.  */
  rest_of_type_compilation (record, m2block_toplevel ());

  return record;
}

/* m2type_BuildEndFieldVarient - finish the varientField by calling
   decl_finish and also finish the type of varientField (which is a
   record).  */

tree
m2type_BuildEndFieldVarient (location_t location, tree varientField,
                             tree varientList, int isPacked)
{
  tree record = TREE_TYPE (varientField);

  m2assert_AssertLocation (location);
  record = m2type_BuildEndRecord (location, record, varientList, isPacked);
  gm2_finish_decl (location, varientField);
  return varientField;
}

/* m2type_BuildStartFieldRecord - starts building a field record.  It
   returns the field which must be completed by calling
   gm2_finish_decl.  */

tree
m2type_BuildStartFieldRecord (location_t location, char *name, tree type)
{
  tree field, declarator;

  m2assert_AssertLocation (location);
  if ((name == NULL) || (strcmp (name, "") == 0))
    declarator = NULL_TREE;
  else
    declarator = get_identifier (name);

  field = build_decl (location, FIELD_DECL, declarator,
                      m2tree_skip_type_decl (type));
  return field;
}

/* Build a record field with name (name maybe NULL), returning the
   new field declaration, FIELD_DECL.

   This is done during the parsing of the struct declaration.  The
   FIELD_DECL nodes are chained together and the lot of them are
   ultimately passed to `build_struct' to make the RECORD_TYPE node.  */

tree
m2type_BuildFieldRecord (location_t location, char *name, tree type)
{
  tree field = m2type_BuildStartFieldRecord (location, name, type);

  m2assert_AssertLocation (location);
  gm2_finish_decl (location, field);
  return field;
}

/* ChainOn - interface so that Modula-2 can also create chains of
   declarations.  */

tree
m2type_ChainOn (tree t1, tree t2)
{
  return chainon (t1, t2);
}

/* ChainOnParamValue - adds a list node {{name, str}, value} into the
   tree list.  */

tree
m2type_ChainOnParamValue (tree list, tree name, tree str, tree value)
{
  return chainon (list, build_tree_list (build_tree_list (name, str), value));
}

/* AddStringToTreeList - adds, string, to list.  */

tree
m2type_AddStringToTreeList (tree list, tree string)
{
  return tree_cons (NULL_TREE, string, list);
}

/* SetAlignment - sets the alignment of a, node, to, align.  It
   duplicates the, node, and sets the alignment to prevent alignment
   effecting behaviour elsewhere.  */

tree
m2type_SetAlignment (tree node, tree align)
{
  tree type = NULL_TREE;
  tree decl = NULL_TREE;
  int is_type;
  int i;

  if (DECL_P (node))
    {
      decl = node;
      is_type = (TREE_CODE (node) == TYPE_DECL);
      type = TREE_TYPE (decl);
    }
  else if (TYPE_P (node))
    {
      is_type = 1;
      type = node;
    }

  if (TREE_CODE (align) != INTEGER_CST)
    error ("requested alignment is not a constant");
  else if ((i = tree_log2 (align)) == -1)
    error ("requested alignment is not a power of 2");
  else if (i > HOST_BITS_PER_INT - 2)
    error ("requested alignment is too large");
  else if (is_type)
    {

      /* If we have a TYPE_DECL, then copy the type, so that we don't
         accidentally modify a builtin type.  See pushdecl.  */
      if (decl && TREE_TYPE (decl) != error_mark_node
          && DECL_ORIGINAL_TYPE (decl) == NULL_TREE)
        {
          tree tt = TREE_TYPE (decl);
          type = build_variant_type_copy (type);
          DECL_ORIGINAL_TYPE (decl) = tt;
          TYPE_NAME (type) = decl;
          TREE_USED (type) = TREE_USED (decl);
          TREE_TYPE (decl) = type;
        }

      SET_TYPE_ALIGN (type, (1 << i) * BITS_PER_UNIT);
      TYPE_USER_ALIGN (type) = 1;

      if (decl)
        {
          SET_DECL_ALIGN (decl, (1 << i) * BITS_PER_UNIT);
          DECL_USER_ALIGN (decl) = 1;
        }
    }
  else if (TREE_CODE (decl) != VAR_DECL && TREE_CODE (decl) != FIELD_DECL)
    error ("alignment may not be specified for %q+D", decl);
  else
    {
      SET_DECL_ALIGN (decl, (1 << i) * BITS_PER_UNIT);
      DECL_USER_ALIGN (decl) = 1;
    }
  return node;
}

/* SetDeclPacked - sets the packed bit in decl TREE, node.  It
   returns the node.  */

tree
m2type_SetDeclPacked (tree node)
{
  DECL_PACKED (node) = 1;
  return node;
}

/* SetTypePacked - sets the packed bit in type TREE, node.  It
   returns the node.  */

tree
m2type_SetTypePacked (tree node)
{
  TYPE_PACKED (node) = 1;
  return node;
}

/* SetRecordFieldOffset - returns field after the byteOffset and
   bitOffset has been applied to it.  */

tree
m2type_SetRecordFieldOffset (tree field, tree byteOffset, tree bitOffset,
                             tree fieldtype, tree nbits)
{
  DECL_FIELD_OFFSET (field) = byteOffset;
  DECL_FIELD_BIT_OFFSET (field) = bitOffset;
  TREE_TYPE (field) = m2tree_skip_type_decl (fieldtype);
  DECL_SIZE (field) = bitsize_int (TREE_INT_CST_LOW (nbits));
  return field;
}

/* BuildPackedFieldRecord - builds a packed field record of, name,
   and, fieldtype.  */

tree
m2type_BuildPackedFieldRecord (location_t location, char *name, tree fieldtype)
{
  m2assert_AssertLocation (location);
  return m2type_BuildFieldRecord (location, name, fieldtype);
}

/* BuildNumberOfArrayElements - returns the number of elements in an
   arrayType.  */

tree
m2type_BuildNumberOfArrayElements (location_t location, tree arrayType)
{
  tree index = TYPE_DOMAIN (arrayType);
  tree high = TYPE_MAX_VALUE (index);
  tree low = TYPE_MIN_VALUE (index);
  tree elements = m2expr_BuildAdd (
      location, m2expr_BuildSub (location, high, low, FALSE),
      m2expr_GetIntegerOne (location), FALSE);
  m2assert_AssertLocation (location);
  return elements;
}

/* AddStatement - maps onto add_stmt.  */

void
m2type_AddStatement (location_t location, tree t)
{
  if (t != NULL_TREE)
    add_stmt (location, t);
}

/* MarkFunctionReferenced - marks a function as referenced.  */

void
m2type_MarkFunctionReferenced (tree f)
{
  if (f != NULL_TREE)
    if (TREE_CODE (f) == FUNCTION_DECL)
      mark_decl_referenced (f);
}

/* GarbageCollect - force gcc to garbage collect.  */

void
m2type_GarbageCollect (void)
{
  ggc_collect ();
}

/* gm2_type_for_size - return an integer type with BITS bits of
   precision, that is unsigned if UNSIGNEDP is nonzero, otherwise
   signed.  */

tree
m2type_gm2_type_for_size (unsigned int bits, int unsignedp)
{
  if (bits == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (bits == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (bits == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (bits == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (bits == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
                      : long_long_integer_type_node);

  if (bits <= TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (bits <= TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (bits <= TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (bits <= TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

  return 0;
}

/* gm2_unsigned_type - return an unsigned type the same as TYPE in
   other respects.  */

tree
m2type_gm2_unsigned_type (tree type)
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == signed_char_type_node || type1 == char_type_node)
    return unsigned_char_type_node;
  if (type1 == integer_type_node)
    return unsigned_type_node;
  if (type1 == short_integer_type_node)
    return short_unsigned_type_node;
  if (type1 == long_integer_type_node)
    return long_unsigned_type_node;
  if (type1 == long_long_integer_type_node)
    return long_long_unsigned_type_node;

#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == intTI_type_node)
    return unsigned_intTI_type_node;
#endif
  if (type1 == intDI_type_node)
    return unsigned_intDI_type_node;
  if (type1 == intSI_type_node)
    return unsigned_intSI_type_node;
  if (type1 == intHI_type_node)
    return unsigned_intHI_type_node;
  if (type1 == intQI_type_node)
    return unsigned_intQI_type_node;

  return m2type_gm2_signed_or_unsigned_type (TRUE, type);
}

/* gm2_signed_type - return a signed type the same as TYPE in other
   respects.  */

tree
m2type_gm2_signed_type (tree type)
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == unsigned_char_type_node || type1 == char_type_node)
    return signed_char_type_node;
  if (type1 == unsigned_type_node)
    return integer_type_node;
  if (type1 == short_unsigned_type_node)
    return short_integer_type_node;
  if (type1 == long_unsigned_type_node)
    return long_integer_type_node;
  if (type1 == long_long_unsigned_type_node)
    return long_long_integer_type_node;

#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == unsigned_intTI_type_node)
    return intTI_type_node;
#endif
  if (type1 == unsigned_intDI_type_node)
    return intDI_type_node;
  if (type1 == unsigned_intSI_type_node)
    return intSI_type_node;
  if (type1 == unsigned_intHI_type_node)
    return intHI_type_node;
  if (type1 == unsigned_intQI_type_node)
    return intQI_type_node;

  return m2type_gm2_signed_or_unsigned_type (FALSE, type);
}

/* check_type - if the precision of baseType and type are the same
   then return true and set the signed or unsigned type in result
   else return false.  */

static int
check_type (tree baseType, tree type, int unsignedp, tree baseu, tree bases,
            tree *result)
{
  if (TYPE_PRECISION (baseType) == TYPE_PRECISION (type))
    {
      if (unsignedp)
        *result = baseu;
      else
        *result = bases;
      return TRUE;
    }
  return FALSE;
}

/* gm2_signed_or_unsigned_type - return a type the same as TYPE
   except unsigned or signed according to UNSIGNEDP.  */

tree
m2type_gm2_signed_or_unsigned_type (int unsignedp, tree type)
{
  tree result;

  if (!INTEGRAL_TYPE_P (type) || TYPE_UNSIGNED (type) == unsignedp)
    return type;

  /* For INTEGER_TYPEs we must check the precision as well, so as to
     yield correct results for bit-field types.  */

  if (check_type (signed_char_type_node, type, unsignedp,
                  unsigned_char_type_node, signed_char_type_node, &result))
    return result;
  if (check_type (integer_type_node, type, unsignedp, unsigned_type_node,
                  integer_type_node, &result))
    return result;
  if (check_type (short_integer_type_node, type, unsignedp,
                  short_unsigned_type_node, short_integer_type_node, &result))
    return result;
  if (check_type (long_integer_type_node, type, unsignedp,
                  long_unsigned_type_node, long_integer_type_node, &result))
    return result;
  if (check_type (long_long_integer_type_node, type, unsignedp,
                  long_long_unsigned_type_node, long_long_integer_type_node,
                  &result))
    return result;

#if HOST_BITS_PER_WIDE_INT >= 64
  if (check_type (intTI_type_node, type, unsignedp, unsigned_intTI_type_node,
                  intTI_type_node, &result))
    return result;
#endif
  if (check_type (intDI_type_node, type, unsignedp, unsigned_intDI_type_node,
                  intDI_type_node, &result))
    return result;
  if (check_type (intSI_type_node, type, unsignedp, unsigned_intSI_type_node,
                  intSI_type_node, &result))
    return result;
  if (check_type (intHI_type_node, type, unsignedp, unsigned_intHI_type_node,
                  intHI_type_node, &result))
    return result;
  if (check_type (intQI_type_node, type, unsignedp, unsigned_intQI_type_node,
                  intQI_type_node, &result))
    return result;
#undef TYPE_OK

  return type;
}

/* IsAddress - returns TRUE if the type is an ADDRESS.  */

int
m2type_IsAddress (tree type)
{
  return type == ptr_type_node;
}

#include "gt-m2-m2type.h"
