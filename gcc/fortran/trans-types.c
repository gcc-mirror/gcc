/* Backend support for Fortran 95 basic types and derived types.
   Copyright (C) 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* trans-types.c -- gfortran backend types */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include <stdio.h>
#include "ggc.h"
#include "toplev.h"
#include <assert.h>
#include "gfortran.h"
#include "trans.h"
#include "trans-types.h"
#include "trans-const.h"


#if (GFC_MAX_DIMENSIONS < 10)
#define GFC_RANK_DIGITS 1
#define GFC_RANK_PRINTF_FORMAT "%01d"
#elif (GFC_MAX_DIMENSIONS < 100)
#define GFC_RANK_DIGITS 2
#define GFC_RANK_PRINTF_FORMAT "%02d"
#else
#error If you really need >99 dimensions, continue the sequence above...
#endif

static tree gfc_get_derived_type (gfc_symbol * derived);

tree gfc_type_nodes[NUM_F95_TYPES];

tree gfc_array_index_type;
tree pvoid_type_node;
tree ppvoid_type_node;
tree pchar_type_node;

static GTY(()) tree gfc_desc_dim_type = NULL;

static GTY(()) tree gfc_max_array_element_size;

/* Create the backend type nodes. We map them to their
   equivalent C type, at least for now.  We also give
   names to the types here, and we push them in the
   global binding level context.*/

void
gfc_init_types (void)
{
  unsigned n;
  unsigned HOST_WIDE_INT hi;
  unsigned HOST_WIDE_INT lo;

  /* Name the types.  */
#define PUSH_TYPE(name, node) \
  pushdecl (build_decl (TYPE_DECL, get_identifier (name), node))

  gfc_int1_type_node = signed_char_type_node;
  PUSH_TYPE ("int1", gfc_int1_type_node);
  gfc_int2_type_node = short_integer_type_node;
  PUSH_TYPE ("int2", gfc_int2_type_node);
  gfc_int4_type_node = gfc_type_for_size (32, 0 /*unsigned */ );
  PUSH_TYPE ("int4", gfc_int4_type_node);
  gfc_int8_type_node = gfc_type_for_size (64, 0 /*unsigned */ );
  PUSH_TYPE ("int8", gfc_int8_type_node);
#if (GFC_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  gfc_int16_type_node = gfc_type_for_size (128, 0 /*unsigned */ );
  PUSH_TYPE ("int16", gfc_int16_type_node);
#endif

  gfc_real4_type_node = float_type_node;
  PUSH_TYPE ("real4", gfc_real4_type_node);
  gfc_real8_type_node = double_type_node;
  PUSH_TYPE ("real8", gfc_real8_type_node);
#if (GFC_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  /* Hmm, this will not work. Ref. g77 */
  gfc_real16_type_node = long_double_type_node;
  PUSH_TYPE ("real16", gfc_real16_type_node);
#endif

  gfc_complex4_type_node = complex_float_type_node;
  PUSH_TYPE ("complex4", gfc_complex4_type_node);
  gfc_complex8_type_node = complex_double_type_node;
  PUSH_TYPE ("complex8", gfc_complex8_type_node);
#if (GFC_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  /* Hmm, this will not work. Ref. g77 */
  gfc_complex16_type_node = complex_long_double_type_node;
  PUSH_TYPE ("complex16", gfc_complex16_type_node);
#endif

  gfc_logical1_type_node = make_node (BOOLEAN_TYPE);
  TYPE_PRECISION (gfc_logical1_type_node) = 8;
  fixup_unsigned_type (gfc_logical1_type_node);
  PUSH_TYPE ("logical1", gfc_logical1_type_node);
  gfc_logical2_type_node = make_node (BOOLEAN_TYPE);
  TYPE_PRECISION (gfc_logical2_type_node) = 16;
  fixup_unsigned_type (gfc_logical2_type_node);
  PUSH_TYPE ("logical2", gfc_logical2_type_node);
  gfc_logical4_type_node = make_node (BOOLEAN_TYPE);
  TYPE_PRECISION (gfc_logical4_type_node) = 32;
  fixup_unsigned_type (gfc_logical4_type_node);
  PUSH_TYPE ("logical4", gfc_logical4_type_node);
  gfc_logical8_type_node = make_node (BOOLEAN_TYPE);
  TYPE_PRECISION (gfc_logical8_type_node) = 64;
  fixup_unsigned_type (gfc_logical8_type_node);
  PUSH_TYPE ("logical8", gfc_logical8_type_node);
#if (GFC_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
  gfc_logical16_type_node = make_node (BOOLEAN_TYPE);
  TYPE_PRECISION (gfc_logical16_type_node) = 128;
  fixup_unsigned_type (gfc_logical16_type_node);
  PUSH_TYPE ("logical16", gfc_logical16_type_node);
#endif

  gfc_character1_type_node = build_type_variant (signed_char_type_node, 0, 0);
  PUSH_TYPE ("char", gfc_character1_type_node);

  PUSH_TYPE ("byte", unsigned_char_type_node);
  PUSH_TYPE ("void", void_type_node);

  /* DBX debugging output gets upset if these aren't set.  */
  if (!TYPE_NAME (integer_type_node))
    PUSH_TYPE ("c_integer", integer_type_node);
  if (!TYPE_NAME (char_type_node))
    PUSH_TYPE ("c_char", char_type_node);
#undef PUSH_TYPE

  pvoid_type_node = build_pointer_type (void_type_node);
  ppvoid_type_node = build_pointer_type (pvoid_type_node);
  pchar_type_node = build_pointer_type (gfc_character1_type_node);

  gfc_index_integer_kind = TYPE_PRECISION (long_unsigned_type_node) / 8;
  gfc_array_index_type = gfc_get_int_type (gfc_index_integer_kind);

  /* The maximum array element size that can be handled is determined
     by the number of bits available to store this field in the array
     descriptor.  */

  n = TREE_INT_CST_LOW (TYPE_SIZE (gfc_array_index_type))
      - GFC_DTYPE_SIZE_SHIFT;

  if (n > sizeof (HOST_WIDE_INT) * 8)
    {
      lo = ~(unsigned HOST_WIDE_INT) 0;
      hi = lo >> (sizeof (HOST_WIDE_INT) * 16 - n);
    }
  else
    {
      hi = 0;
      lo = (~(unsigned HOST_WIDE_INT) 0) >> (sizeof (HOST_WIDE_INT) * 8 - n);
    }
  gfc_max_array_element_size = build_int_2 (lo, hi);
  TREE_TYPE (gfc_max_array_element_size) = long_unsigned_type_node;

  size_type_node = gfc_array_index_type;
  boolean_type_node = gfc_get_logical_type (gfc_default_logical_kind ());

  boolean_true_node = build_int_2 (1, 0);
  TREE_TYPE (boolean_true_node) = boolean_type_node;
  boolean_false_node = build_int_2 (0, 0);
  TREE_TYPE (boolean_false_node) = boolean_type_node;
}

/* Get a type node for an integer kind */

tree
gfc_get_int_type (int kind)
{
  switch (kind)
    {
    case 1:
      return (gfc_int1_type_node);
    case 2:
      return (gfc_int2_type_node);
    case 4:
      return (gfc_int4_type_node);
    case 8:
      return (gfc_int8_type_node);
#if (GFC_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    case 16:
      return (95 _int16_type_node);
#endif
    default:
      fatal_error ("integer kind=%d not available", kind);
    }
}

/* Get a type node for a real kind */

tree
gfc_get_real_type (int kind)
{
  switch (kind)
    {
    case 4:
      return (gfc_real4_type_node);
    case 8:
      return (gfc_real8_type_node);
#if (GFC_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    case 16:
      return (gfc_real16_type_node);
#endif
    default:
      fatal_error ("real kind=%d not available", kind);
    }
}

/* Get a type node for a complex kind */

tree
gfc_get_complex_type (int kind)
{
  switch (kind)
    {
    case 4:
      return (gfc_complex4_type_node);
    case 8:
      return (gfc_complex8_type_node);
#if (GFC_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    case 16:
      return (gfc_complex16_type_node);
#endif
    default:
      fatal_error ("complex kind=%d not available", kind);
    }
}

/* Get a type node for a logical kind */

tree
gfc_get_logical_type (int kind)
{
  switch (kind)
    {
    case 1:
      return (gfc_logical1_type_node);
    case 2:
      return (gfc_logical2_type_node);
    case 4:
      return (gfc_logical4_type_node);
    case 8:
      return (gfc_logical8_type_node);
#if (GFC_USE_TYPES16 && (HOST_BITS_PER_WIDE_INT >= 64))
    case 16:
      return (gfc_logical16_type_node);
#endif
    default:
      fatal_error ("logical kind=%d not available", kind);
    }
}

/* Get a type node for a character kind.  */

tree
gfc_get_character_type (int kind, gfc_charlen * cl)
{
  tree base;
  tree type;
  tree len;
  tree bounds;

  switch (kind)
    {
    case 1:
      base = gfc_character1_type_node;
      break;

    default:
      fatal_error ("character kind=%d not available", kind);
    }

  len = (cl == 0) ? NULL_TREE : cl->backend_decl;

  bounds = build_range_type (gfc_array_index_type, integer_one_node, len);
  type = build_array_type (base, bounds);
  TYPE_STRING_FLAG (type) = 1;

  return type;
}

/* Covert a basic type.  This will be an array for character types.  */

tree
gfc_typenode_for_spec (gfc_typespec * spec)
{
  tree basetype;

  switch (spec->type)
    {
    case BT_UNKNOWN:
      abort ();
      break;

    case BT_INTEGER:
      basetype = gfc_get_int_type (spec->kind);
      break;

    case BT_REAL:
      basetype = gfc_get_real_type (spec->kind);
      break;

    case BT_COMPLEX:
      basetype = gfc_get_complex_type (spec->kind);
      break;

    case BT_LOGICAL:
      basetype = gfc_get_logical_type (spec->kind);
      break;

    case BT_CHARACTER:
      basetype = gfc_get_character_type (spec->kind, spec->cl);
      break;

    case BT_DERIVED:
      basetype = gfc_get_derived_type (spec->derived);
      break;

    default:
      abort ();
      break;
    }
  return basetype;
}

/* Build an INT_CST for constant expressions, otherwise return NULL_TREE.  */

static tree
gfc_conv_array_bound (gfc_expr * expr)
{
  /* If expr is an integer constant, return that.  */
  if (expr != NULL && expr->expr_type == EXPR_CONSTANT)
    return gfc_conv_mpz_to_tree (expr->value.integer, gfc_index_integer_kind);

  /* Otherwise return NULL.  */
  return NULL_TREE;
}

tree
gfc_get_element_type (tree type)
{
  tree element;

  if (GFC_ARRAY_TYPE_P (type))
    {
      if (TREE_CODE (type) == POINTER_TYPE)
        type = TREE_TYPE (type);
      assert (TREE_CODE (type) == ARRAY_TYPE);
      element = TREE_TYPE (type);
    }
  else
    {
      assert (GFC_DESCRIPTOR_TYPE_P (type));
      element = TREE_TYPE (TYPE_FIELDS (type));

      assert (TREE_CODE (element) == POINTER_TYPE);
      element = TREE_TYPE (element);

      assert (TREE_CODE (element) == ARRAY_TYPE);
      element = TREE_TYPE (element);
    }

  return element;
}

/* Build an array. This function is called from gfc_sym_type().
   Actually returns array descriptor type.

   Format of array descriptors is as follows:

    struct gfc_array_descriptor
    {
      array *data
      index offset;
      index dtype;
      struct descriptor_dimension dimension[N_DIM];
    }

    struct descriptor_dimension
    {
      index stride;
      index lbound;
      index ubound;
    }

   Translation code should use gfc_conv_descriptor_* rather than accessing
   the descriptor directly. Any changes to the array descriptor type will
   require changes in gfc_conv_descriptor_* and gfc_build_array_initializer.

   This is represented internally as a RECORD_TYPE. The index nodes are
   gfc_array_index_type and the data node is a pointer to the data. See below
   for the handling of character types.

   The dtype member is formatted as follows:
    rank = dtype & GFC_DTYPE_RANK_MASK // 3 bits
    type = (dtype & GFC_DTYPE_TYPE_MASK) >> GFC_DTYPE_TYPE_SHIFT // 3 bits
    size = dtype >> GFC_DTYPE_SIZE_SHIFT

   I originally used nested ARRAY_TYPE nodes to represent arrays, but this
   generated poor code for assumed/deferred size arrays.  These require
   use of PLACEHOLDER_EXPR/WITH_RECORD_EXPR, which isn't part of the GENERIC
   grammar.  Also, there is no way to explicitly set the array stride, so
   all data must be packed(1).  I've tried to mark all the functions which
   would require modification with a GCC ARRAYS comment.

   The data component points to the first element in the array.
   The offset field is the position of the origin of the array
   (ie element (0, 0 ...)).  This may be outsite the bounds of the array.

   An element is accessed by
   data[offset + index0*stride0 + index1*stride1 + index2*stride2]
   This gives good performance as the computation does not involve the
   bounds of the array.  For packed arrays, this is optimized further by
   substituting the known strides.

   This system has one problem: all array bounds must be withing 2^31 elements
   of the origin (2^63 on 64-bit machines).  For example
   integer, dimension (80000:90000, 80000:90000, 2) :: array
   may not work properly on 32-bit machines because 80000*80000 > 2^31, so
   the calculation for stride02 would overflow.  This may still work, but
   I haven't checked, and it relies on the overflow doing the right thing.

   The way to fix this problem is to access alements as follows:
   data[(index0-lbound0)*stride0 + (index1-lbound1)*stride1]
   Obviously this is much slower.  I will make this a compile time option,
   something like -fsmall-array-offsets.  Mixing code compiled with and without
   this switch will work.

   (1) This can be worked around by modifying the upper bound of the previous
   dimension.  This requires extra fields in the descriptor (both real_ubound
   and fake_ubound).  In tree.def there is mention of TYPE_SEP, which
   may allow us to do this.  However I can't find mention of this anywhere
   else.
 */


/* Returns true if the array sym does not require a descriptor.  */

int
gfc_is_nodesc_array (gfc_symbol * sym)
{
  assert (sym->attr.dimension);

  /* We only want local arrays.  */
  if (sym->attr.pointer || sym->attr.allocatable)
    return 0;

  if (sym->attr.dummy)
    {
      if (sym->as->type != AS_ASSUMED_SHAPE)
        return 1;
      else
        return 0;
    }

  if (sym->attr.result || sym->attr.function)
    return 0;

  if (sym->attr.pointer || sym->attr.allocatable)
    return 0;

  assert (sym->as->type == AS_EXPLICIT);

  return 1;
}

static tree
gfc_build_array_type (tree type, gfc_array_spec * as)
{
  tree lbound[GFC_MAX_DIMENSIONS];
  tree ubound[GFC_MAX_DIMENSIONS];
  int n;

  for (n = 0; n < as->rank; n++)
    {
      /* Create expressions for the known bounds of the array.  */
      if (as->type == AS_ASSUMED_SHAPE && as->lower[n] == NULL)
        lbound[n] = integer_one_node;
      else
        lbound[n] = gfc_conv_array_bound (as->lower[n]);
      ubound[n] = gfc_conv_array_bound (as->upper[n]);
    }

  return gfc_get_array_type_bounds (type, as->rank, lbound, ubound, 0);
}

/* Returns the struct descriptor_dimension type.  */

static tree
gfc_get_desc_dim_type (void)
{
  tree type;
  tree decl;
  tree fieldlist;

  if (gfc_desc_dim_type)
    return gfc_desc_dim_type;

  /* Build the type node.  */
  type = make_node (RECORD_TYPE);

  TYPE_NAME (type) = get_identifier ("descriptor_dimension");
  TYPE_PACKED (type) = 1;

  /* Consists of the stride, lbound and ubound members.  */
  decl = build_decl (FIELD_DECL,
		     get_identifier ("stride"), gfc_array_index_type);
  DECL_CONTEXT (decl) = type;
  fieldlist = decl;

  decl = build_decl (FIELD_DECL,
		     get_identifier ("lbound"), gfc_array_index_type);
  DECL_CONTEXT (decl) = type;
  fieldlist = chainon (fieldlist, decl);

  decl = build_decl (FIELD_DECL,
		     get_identifier ("ubound"), gfc_array_index_type);
  DECL_CONTEXT (decl) = type;
  fieldlist = chainon (fieldlist, decl);

  /* Finish off the type.  */
  TYPE_FIELDS (type) = fieldlist;

  gfc_finish_type (type);

  gfc_desc_dim_type = type;
  return type;
}

static tree
gfc_get_dtype (tree type, int rank)
{
  tree size;
  int n;
  HOST_WIDE_INT i;
  tree tmp;
  tree dtype;

  if (GFC_DESCRIPTOR_TYPE_P (type) || GFC_ARRAY_TYPE_P (type))
    return (GFC_TYPE_ARRAY_DTYPE (type));

  /* TODO: Correctly identify LOGICAL types.  */
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
      n = GFC_DTYPE_INTEGER;
      break;

    case BOOLEAN_TYPE:
      n = GFC_DTYPE_LOGICAL;
      break;

    case REAL_TYPE:
      n = GFC_DTYPE_REAL;
      break;

    case COMPLEX_TYPE:
      n = GFC_DTYPE_COMPLEX;
      break;

    /* Arrays have already been dealt with.  */
    case RECORD_TYPE:
      n = GFC_DTYPE_DERIVED;
      break;

    case ARRAY_TYPE:
      n = GFC_DTYPE_CHARACTER;
      break;

    default:
      abort ();
    }

  assert (rank <= GFC_DTYPE_RANK_MASK);
  size = TYPE_SIZE_UNIT (type);
    
  i = rank | (n << GFC_DTYPE_TYPE_SHIFT);
  if (size && INTEGER_CST_P (size))
    {
      if (tree_int_cst_lt (gfc_max_array_element_size, size))
	internal_error ("Array element size too big");

      i += TREE_INT_CST_LOW (size) << GFC_DTYPE_SIZE_SHIFT;
    }
  dtype = build_int_2 (i, 0);
  TREE_TYPE (dtype) = gfc_array_index_type;

  if (size && !INTEGER_CST_P (size))
    {
      tmp = build_int_2 (GFC_DTYPE_SIZE_SHIFT, 0);
      TREE_TYPE (tmp) = gfc_array_index_type;
      tmp  = fold (build (LSHIFT_EXPR, gfc_array_index_type, size, tmp));
      dtype = fold (build (PLUS_EXPR, gfc_array_index_type, tmp, dtype));
    }
  /* If we don't know the size we leave it as zero.  This should never happen
     for anything that is actually used.  */
  /* TODO: Check this is actually true, particularly when repacking
     assumed size parameters.  */

  return dtype;
}


/* Build an array type for use without a descriptor.  Valid values of packed
   are 0=no, 1=partial, 2=full, 3=static.  */

tree
gfc_get_nodesc_array_type (tree etype, gfc_array_spec * as, int packed)
{
  tree range;
  tree type;
  tree tmp;
  int n;
  int known_stride;
  int known_offset;
  mpz_t offset;
  mpz_t stride;
  mpz_t delta;
  gfc_expr *expr;

  mpz_init_set_ui (offset, 0);
  mpz_init_set_ui (stride, 1);
  mpz_init (delta);

  /* We don't use build_array_type because this does not include include
     lang-specific information (ie. the bounds of the array) when checking
     for duplicates.  */
  type = make_node (ARRAY_TYPE);

  GFC_ARRAY_TYPE_P (type) = 1;
  TYPE_LANG_SPECIFIC (type) = (struct lang_type *)
    ggc_alloc_cleared (sizeof (struct lang_type));

  known_stride = (packed != 0);
  known_offset = 1;
  for (n = 0; n < as->rank; n++)
    {
      /* Fill in the stride and bound components of the type.  */
      if (known_stride)
	tmp =  gfc_conv_mpz_to_tree (stride, gfc_index_integer_kind);
      else
        tmp = NULL_TREE;
      GFC_TYPE_ARRAY_STRIDE (type, n) = tmp;

      expr = as->lower[n];
      if (expr->expr_type == EXPR_CONSTANT)
        {
          tmp = gfc_conv_mpz_to_tree (expr->value.integer,
                                  gfc_index_integer_kind);
        }
      else
        {
          known_stride = 0;
          tmp = NULL_TREE;
        }
      GFC_TYPE_ARRAY_LBOUND (type, n) = tmp;

      if (known_stride)
	{
          /* Calculate the offset.  */
          mpz_mul (delta, stride, as->lower[n]->value.integer);
          mpz_sub (offset, offset, delta);
	}
      else
	known_offset = 0;

      expr = as->upper[n];
      if (expr && expr->expr_type == EXPR_CONSTANT)
        {
	  tmp = gfc_conv_mpz_to_tree (expr->value.integer,
			          gfc_index_integer_kind);
        }
      else
        {
          tmp = NULL_TREE;
          known_stride = 0;
        }
      GFC_TYPE_ARRAY_UBOUND (type, n) = tmp;

      if (known_stride)
        {
          /* Calculate the stride.  */
          mpz_sub (delta, as->upper[n]->value.integer,
	           as->lower[n]->value.integer);
          mpz_add_ui (delta, delta, 1);
          mpz_mul (stride, stride, delta);
        }

      /* Only the first stride is known for partial packed arrays.  */
      if (packed < 2)
        known_stride = 0;
    }

  if (known_offset)
    {
      GFC_TYPE_ARRAY_OFFSET (type) =
        gfc_conv_mpz_to_tree (offset, gfc_index_integer_kind);
    }
  else
    GFC_TYPE_ARRAY_OFFSET (type) = NULL_TREE;

  if (known_stride)
    {
      GFC_TYPE_ARRAY_SIZE (type) =
        gfc_conv_mpz_to_tree (stride, gfc_index_integer_kind);
    }
  else
    GFC_TYPE_ARRAY_SIZE (type) = NULL_TREE;

  GFC_TYPE_ARRAY_DTYPE (type) = gfc_get_dtype (etype, as->rank);
  GFC_TYPE_ARRAY_RANK (type) = as->rank;
  range = build_range_type (gfc_array_index_type, integer_zero_node,
			    NULL_TREE);
  /* TODO: use main type if it is unbounded.  */
  GFC_TYPE_ARRAY_DATAPTR_TYPE (type) =
    build_pointer_type (build_array_type (etype, range));

  if (known_stride)
    {
      mpz_sub_ui (stride, stride, 1);
      range = gfc_conv_mpz_to_tree (stride, gfc_index_integer_kind);
    }
  else
    range = NULL_TREE;

  range = build_range_type (gfc_array_index_type, integer_zero_node, range);
  TYPE_DOMAIN (type) = range;

  build_pointer_type (etype);
  TREE_TYPE (type) = etype;

  layout_type (type);

  mpz_clear (offset);
  mpz_clear (stride);
  mpz_clear (delta);

  if (packed < 3 || !known_stride)
    {
      type = build_pointer_type (type);
      GFC_ARRAY_TYPE_P (type) = 1;
      TYPE_LANG_SPECIFIC (type) = TYPE_LANG_SPECIFIC (TREE_TYPE (type));
    }
  return type;
}


/* Build an array (descriptor) type with given bounds.  */

tree
gfc_get_array_type_bounds (tree etype, int dimen, tree * lbound,
			   tree * ubound, int packed)
{
  tree fat_type, fat_pointer_type;
  tree fieldlist;
  tree arraytype;
  tree decl;
  int n;
  char name[8 + GFC_RANK_DIGITS + GFC_MAX_SYMBOL_LEN];
  const char *typename;
  tree lower;
  tree upper;
  tree stride;
  tree tmp;

  /* Build the type node.  */
  fat_type = make_node (RECORD_TYPE);
  GFC_DESCRIPTOR_TYPE_P (fat_type) = 1;
  TYPE_LANG_SPECIFIC (fat_type) = (struct lang_type *)
    ggc_alloc_cleared (sizeof (struct lang_type));
  GFC_TYPE_ARRAY_RANK (fat_type) = dimen;
  GFC_TYPE_ARRAY_DTYPE (fat_type) = gfc_get_dtype (etype, dimen);

  tmp = TYPE_NAME (etype);
  if (tmp && TREE_CODE (tmp) == TYPE_DECL)
    tmp = DECL_NAME (tmp);
  if (tmp)
    typename = IDENTIFIER_POINTER (tmp);
  else
    typename = "unknown";

  sprintf (name, "array" GFC_RANK_PRINTF_FORMAT "_%.*s", dimen,
	   GFC_MAX_SYMBOL_LEN, typename);
  TYPE_NAME (fat_type) = get_identifier (name);
  TYPE_PACKED (fat_type) = 0;

  fat_pointer_type = build_pointer_type (fat_type);

  /* Build an array descriptor record type.  */
  if (packed != 0)
    stride = integer_one_node;
  else
    stride = NULL_TREE;

  for (n = 0; n < dimen; n++)
    {
      GFC_TYPE_ARRAY_STRIDE (fat_type, n) = stride;

      if (lbound)
	lower = lbound[n];
      else
	lower = NULL_TREE;

      if (lower != NULL_TREE)
	{
	  if (INTEGER_CST_P (lower))
	    GFC_TYPE_ARRAY_LBOUND (fat_type, n) = lower;
	  else
	    lower = NULL_TREE;
	}

      upper = ubound[n];
      if (upper != NULL_TREE)
	{
	  if (INTEGER_CST_P (upper))
	    GFC_TYPE_ARRAY_UBOUND (fat_type, n) = upper;
	  else
	    upper = NULL_TREE;
	}

      if (upper != NULL_TREE && lower != NULL_TREE && stride != NULL_TREE)
	{
	  tmp = fold (build (MINUS_EXPR, gfc_array_index_type, upper, lower));
	  tmp = fold (build (PLUS_EXPR, gfc_array_index_type, tmp,
			     integer_one_node));
	  stride =
	    fold (build (MULT_EXPR, gfc_array_index_type, tmp, stride));
	  /* Check the folding worked.  */
	  assert (INTEGER_CST_P (stride));
	}
      else
	stride = NULL_TREE;
    }
  GFC_TYPE_ARRAY_SIZE (fat_type) = stride;
  /* TODO: known offsets for descriptors.  */
  GFC_TYPE_ARRAY_OFFSET (fat_type) = NULL_TREE;

  /* We define data as an unknown size array. Much better than doing
     pointer arithmetic.  */
  arraytype =
    build_array_type (etype,
		      build_range_type (gfc_array_index_type,
					integer_zero_node, NULL_TREE));
  arraytype = build_pointer_type (arraytype);
  GFC_TYPE_ARRAY_DATAPTR_TYPE (fat_type) = arraytype;

  /* The pointer to the array data.  */
  decl = build_decl (FIELD_DECL, get_identifier ("data"), arraytype);

  DECL_CONTEXT (decl) = fat_type;
  /* Add the data member as the first element of the descriptor.  */
  fieldlist = decl;

  /* Add the base component.  */
  decl = build_decl (FIELD_DECL, get_identifier ("offset"),
		     gfc_array_index_type);
  DECL_CONTEXT (decl) = fat_type;
  fieldlist = chainon (fieldlist, decl);

  /* Add the dtype component.  */
  decl = build_decl (FIELD_DECL, get_identifier ("dtype"),
		     gfc_array_index_type);
  DECL_CONTEXT (decl) = fat_type;
  fieldlist = chainon (fieldlist, decl);

  /* Build the array type for the stride and bound components.  */
  arraytype =
    build_array_type (gfc_get_desc_dim_type (),
		      build_range_type (gfc_array_index_type,
					integer_zero_node,
					gfc_rank_cst[dimen - 1]));

  decl = build_decl (FIELD_DECL, get_identifier ("dim"), arraytype);
  DECL_CONTEXT (decl) = fat_type;
  DECL_INITIAL (decl) = NULL_TREE;
  fieldlist = chainon (fieldlist, decl);

  /* Finish off the type.  */
  TYPE_FIELDS (fat_type) = fieldlist;

  gfc_finish_type (fat_type);

  return fat_type;
}

/* Build a pointer type. This function is called from gfc_sym_type().  */

static tree
gfc_build_pointer_type (gfc_symbol * sym, tree type)
{
  /* Array pointer types aren't actualy pointers.  */
  if (sym->attr.dimension)
    return type;
  else
    return build_pointer_type (type);
}

/* Return the type for a symbol.  Special handling is required for character
   types to get the correct level of indirection.
   For functions return the return type.
   For subroutines return void_type_node.  */

tree
gfc_sym_type (gfc_symbol * sym)
{
  tree type;
  int byref;

  if (sym->attr.flavor == FL_PROCEDURE && !sym->attr.function)
    return void_type_node;

  if (sym->backend_decl)
    {
      if (sym->attr.function)
	return TREE_TYPE (TREE_TYPE (sym->backend_decl));
      else
	return TREE_TYPE (sym->backend_decl);
    }

  /* The frontend doesn't set all the attributes for a function with an
     explicit result value, so we use that instead when present.  */
  if (sym->attr.function && sym->result)
    sym = sym->result;

  type = gfc_typenode_for_spec (&sym->ts);

  if (sym->attr.dummy && !sym->attr.function)
    byref = 1;
  else
    byref = 0;

  if (sym->attr.dimension)
    {
      if (gfc_is_nodesc_array (sym))
        {
	  /* If this is a character argument of unknown length, just use the
	     base type.  */
	  if (sym->ts.type != BT_CHARACTER
	      || !(sym->attr.dummy || sym->attr.function || sym->attr.result)
	      || sym->ts.cl->backend_decl)
	    {
	      type = gfc_get_nodesc_array_type (type, sym->as,
						byref ? 2 : 3);
	      byref = 0;
	    }
        }
      else
	type = gfc_build_array_type (type, sym->as);
    }
  else
    {
      if (sym->attr.allocatable || sym->attr.pointer)
	type = gfc_build_pointer_type (sym, type);
    }

  /* We currently pass all parameters by reference.
     See f95_get_function_decl.  For dummy function parameters return the
     function type.  */
  if (byref)
    type = build_reference_type (type);

  return (type);
}

/* Layout and output debug info for a record type.  */

void
gfc_finish_type (tree type)
{
  tree decl;

  decl = build_decl (TYPE_DECL, NULL_TREE, type);
  TYPE_STUB_DECL (type) = decl;
  layout_type (type);
  rest_of_type_compilation (type, 1);
  rest_of_decl_compilation (decl, NULL, 1, 0);
}

/* Add a field of given NAME and TYPE to the context of a UNION_TYPE
   or RECORD_TYPE pointed to by STYPE.  The new field is chained
   to the fieldlist pointed to by FIELDLIST.

   Returns a pointer to the new field.  */

tree
gfc_add_field_to_struct (tree *fieldlist, tree context,
			 tree name, tree type)
{
  tree decl;

  decl = build_decl (FIELD_DECL, name, type);

  DECL_CONTEXT (decl) = context;
  DECL_INITIAL (decl) = 0;
  DECL_ALIGN (decl) = 0;
  DECL_USER_ALIGN (decl) = 0;
  TREE_CHAIN (decl) = NULL_TREE;
  *fieldlist = chainon (*fieldlist, decl);

  return decl;
}


/* Build a tree node for a derived type.  */

static tree
gfc_get_derived_type (gfc_symbol * derived)
{
  tree typenode, field, field_type, fieldlist;
  gfc_component *c;

  assert (derived && derived->attr.flavor == FL_DERIVED);

  /* derived->backend_decl != 0 means we saw it before, but its
  component's backend_decl may have not been built.  */
  if (derived->backend_decl)
    {
      /* Its component's backend_decl has been built.  */
      if (TYPE_FIELDS (derived->backend_decl))
        return derived->backend_decl;
      else
        typenode = derived->backend_decl;
    }
  else
    {
      /* We see this derived type first time, so build the type node.  */
      typenode = make_node (RECORD_TYPE);
      TYPE_NAME (typenode) = get_identifier (derived->name);
      TYPE_PACKED (typenode) = gfc_option.flag_pack_derived;
      derived->backend_decl = typenode;
    }

  /* Build the type member list. Install the newly created RECORD_TYPE
     node as DECL_CONTEXT of each FIELD_DECL.  */
  fieldlist = NULL_TREE;
  for (c = derived->components; c; c = c->next)
    {
      if (c->ts.type == BT_DERIVED && c->pointer)
        {
          if (c->ts.derived->backend_decl)
            field_type = c->ts.derived->backend_decl;
          else
            {
              /* Build the type node.  */
              field_type = make_node (RECORD_TYPE);
              TYPE_NAME (field_type) = get_identifier (c->ts.derived->name);
              TYPE_PACKED (field_type) = gfc_option.flag_pack_derived;
              c->ts.derived->backend_decl = field_type;
            }
        }
      else
	{
	  if (c->ts.type == BT_CHARACTER)
	    {
	      /* Evaluate the string length.  */
	      gfc_conv_const_charlen (c->ts.cl);
	      assert (c->ts.cl->backend_decl);
	    }

	  field_type = gfc_typenode_for_spec (&c->ts);
	}

      /* This returns an array descriptor type.  Initialisation may be
         required.  */
      if (c->dimension)
	{
	  if (c->pointer)
	    {
	      /* Pointers to arrays aren't actualy pointer types.  The
	         descriptors are seperate, but the data is common.  */
	      field_type = gfc_build_array_type (field_type, c->as);
	    }
	  else
	    field_type = gfc_get_nodesc_array_type (field_type, c->as, 3);
	}
      else if (c->pointer)
	field_type = build_pointer_type (field_type);

      field = gfc_add_field_to_struct (&fieldlist, typenode,
				       get_identifier (c->name),
				       field_type);

      DECL_PACKED (field) |= TYPE_PACKED (typenode);

      assert (!c->backend_decl);
      c->backend_decl = field;
    }

  /* Now we have the final fieldlist.  Record it, then lay out the
     derived type, including the fields.  */
  TYPE_FIELDS (typenode) = fieldlist;

  gfc_finish_type (typenode);

  derived->backend_decl = typenode;

  return typenode;
}

int
gfc_return_by_reference (gfc_symbol * sym)
{
  if (!sym->attr.function)
    return 0;

  assert (sym->attr.function);

  if (sym->result)
    sym = sym->result;

  if (sym->attr.dimension)
    return 1;

  if (sym->ts.type == BT_CHARACTER)
    return 1;

  if (sym->ts.type == BT_DERIVED)
    gfc_todo_error ("Returning derived types");
  /* Possibly return derived types by reference.  */
  return 0;
}


tree
gfc_get_function_type (gfc_symbol * sym)
{
  tree type;
  tree typelist;
  gfc_formal_arglist *f;
  gfc_symbol *arg;
  int nstr;
  int alternate_return;

  /* Make sure this symbol is a function or a subroutine.  */
  assert (sym->attr.flavor == FL_PROCEDURE);

  if (sym->backend_decl)
    return TREE_TYPE (sym->backend_decl);

  nstr = 0;
  alternate_return = 0;
  typelist = NULL_TREE;
  /* Some functions we use an extra parameter for the return value.  */
  if (gfc_return_by_reference (sym))
    {
      if (sym->result)
	arg = sym->result;
      else
	arg = sym;

      if (arg->ts.type == BT_CHARACTER)
	gfc_conv_const_charlen (arg->ts.cl);

      type = gfc_sym_type (arg);
      if (arg->ts.type == BT_DERIVED
	  || arg->attr.dimension
	  || arg->ts.type == BT_CHARACTER)
	type = build_reference_type (type);

      typelist = gfc_chainon_list (typelist, type);
      if (arg->ts.type == BT_CHARACTER)
	typelist = gfc_chainon_list (typelist, gfc_strlen_type_node);
    }

  /* Build the argument types for the function */
  for (f = sym->formal; f; f = f->next)
    {
      arg = f->sym;
      if (arg)
	{
	  /* Evaluate constant character lengths here so that they can be
	     included in the type.  */
	  if (arg->ts.type == BT_CHARACTER)
	    gfc_conv_const_charlen (arg->ts.cl);

	  if (arg->attr.flavor == FL_PROCEDURE)
	    {
	      type = gfc_get_function_type (arg);
	      type = build_pointer_type (type);
	    }
	  else
	    type = gfc_sym_type (arg);

	  /* Parameter Passing Convention

	     We currently pass all parameters by reference.
	     Parameters with INTENT(IN) could be passed by value.
	     The problem arises if a function is called via an implicit
	     prototype. In this situation the INTENT is not known.
	     For this reason all parameters to global functions must be
	     passed by reference.  Passing by value would potentialy
	     generate bad code.  Worse there would be no way of telling that
	     this code was bad, except that it would give incorrect results.

	     Contained procedures could pass by value as these are never
	     used without an explicit interface, and connot be passed as
	     actual parameters for a dummy procedure.  */
	  if (arg->ts.type == BT_CHARACTER)
            nstr++;
	  typelist = gfc_chainon_list (typelist, type);
	}
      else
        {
          if (sym->attr.subroutine)
            alternate_return = 1;
        }
    }

  /* Add hidden string length parameters.  */
  while (nstr--)
    typelist = gfc_chainon_list (typelist, gfc_strlen_type_node);

  typelist = gfc_chainon_list (typelist, void_type_node);

  if (alternate_return)
    type = integer_type_node;
  else if (!sym->attr.function || gfc_return_by_reference (sym))
    type = void_type_node;
  else
    type = gfc_sym_type (sym);

  type = build_function_type (type, typelist);

  return type;
}

/* Routines for getting integer type nodes */


/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
gfc_type_for_size (unsigned bits, int unsignedp)
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
/*TODO: We currently don't initialise this...
  if (bits == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
            : widest_integer_literal_type_node);*/

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

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */

tree
gfc_type_for_mode (enum machine_mode mode, int unsignedp)
{
  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (mode == TYPE_MODE (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node :
      long_long_integer_type_node;

/*TODO: see above
  if (mode == TYPE_MODE (widest_integer_literal_type_node))
    return unsignedp ? widest_unsigned_literal_type_node
                     : widest_integer_literal_type_node;
*/

  if (mode == QImode)
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (mode == HImode)
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (mode == SImode)
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (mode == DImode)
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

#if HOST_BITS_PER_WIDE_INT >= 64
  if (mode == TYPE_MODE (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (long_double_type_node))
    return long_double_type_node;

  if (mode == TYPE_MODE (build_pointer_type (char_type_node)))
    return build_pointer_type (char_type_node);

  if (mode == TYPE_MODE (build_pointer_type (integer_type_node)))
    return build_pointer_type (integer_type_node);

#ifdef VECTOR_MODE_SUPPORTED_P
  if (VECTOR_MODE_SUPPORTED_P (mode))
    {
      switch (mode)
	{
	case V16QImode:
	  return unsignedp ? unsigned_V16QI_type_node : V16QI_type_node;
	case V8HImode:
	  return unsignedp ? unsigned_V8HI_type_node : V8HI_type_node;
	case V4SImode:
	  return unsignedp ? unsigned_V4SI_type_node : V4SI_type_node;
	case V2DImode:
	  return unsignedp ? unsigned_V2DI_type_node : V2DI_type_node;
	case V2SImode:
	  return unsignedp ? unsigned_V2SI_type_node : V2SI_type_node;
	case V4HImode:
	  return unsignedp ? unsigned_V4HI_type_node : V4HI_type_node;
	case V8QImode:
	  return unsignedp ? unsigned_V8QI_type_node : V8QI_type_node;
	case V16SFmode:
	  return V16SF_type_node;
	case V4SFmode:
	  return V4SF_type_node;
	case V2SFmode:
	  return V2SF_type_node;
	case V2DFmode:
	  return V2DF_type_node;
	default:
	  break;
	}
    }
#endif

  return 0;
}

/* Return an unsigned type the same as TYPE in other respects.  */

tree
gfc_unsigned_type (tree type)
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
/*TODO :see others
  if (type1 == widest_integer_literal_type_node)
    return widest_unsigned_literal_type_node;
*/
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

  return gfc_signed_or_unsigned_type (1, type);
}

/* Return a signed type the same as TYPE in other respects.  */

tree
gfc_signed_type (tree type)
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
/*TODO: see others
  if (type1 == widest_unsigned_literal_type_node)
    return widest_integer_literal_type_node;
*/
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

  return gfc_signed_or_unsigned_type (0, type);
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
gfc_signed_or_unsigned_type (int unsignedp, tree type)
{
  if (!INTEGRAL_TYPE_P (type) || TYPE_UNSIGNED (type) == unsignedp)
    return type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
/*TODO: see others
  if (TYPE_PRECISION (type) == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
            : widest_integer_literal_type_node);
*/
#if HOST_BITS_PER_WIDE_INT >= 64
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  return type;
}

#include "gt-fortran-trans-types.h"
