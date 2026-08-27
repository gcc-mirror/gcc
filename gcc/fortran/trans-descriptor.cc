/* Copyright (C) 2002-2025 Free Software Foundation, Inc.

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
#include "tree.h"
#include "fold-const.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"
#include "trans-array.h"


/* Array descriptor low level access routines.
 ******************************************************************************/

/* Build expressions to access the members of an array descriptor.
   It's surprisingly easy to mess up here, so never access
   an array descriptor by "brute force", always use these
   functions.  This also avoids problems if we change the format
   of an array descriptor.

   To understand these magic numbers, look at the comments
   before gfc_build_array_type() in trans-types.cc.

   The code within these defines should be the only code which knows the format
   of an array descriptor.

   Any code just needing to read obtain the bounds of an array should use
   gfc_conv_array_* rather than the following functions as these will return
   know constant values, and work with arrays which do not have descriptors.

   Don't forget to #undef these!  */

#define DATA_FIELD 0
#define OFFSET_FIELD 1
#define DTYPE_FIELD 2
#define SPAN_FIELD 3
#define DIMENSION_FIELD 4
#define CAF_TOKEN_FIELD 5

#define STRIDE_SUBFIELD 0
#define LBOUND_SUBFIELD 1
#define UBOUND_SUBFIELD 2

#define GFC_DTYPE_ELEM_LEN 0
#define GFC_DTYPE_VERSION 1
#define GFC_DTYPE_RANK 2
#define GFC_DTYPE_TYPE 3
#define GFC_DTYPE_ATTRIBUTE 4


/* Get FIELD_IDX'th field in struct TYPE.  */

static tree
get_type_field (tree type, unsigned field_idx)
{
  tree field = gfc_advance_chain (TYPE_FIELDS (type), field_idx);
  gcc_assert (field != NULL_TREE);

  return field;
}


/* Return a reference to the FIELD_IDX-th field of the descriptor DESC.  */

static tree
gfc_get_descriptor_field (tree desc, unsigned field_idx)
{
  tree type = TREE_TYPE (desc);
  gcc_assert (GFC_DESCRIPTOR_TYPE_P (type));

  tree field = get_type_field (type, field_idx);

  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			  desc, field, NULL_TREE);
}


/* Return a reference to the data field of the array descriptor DESC.  */

static tree
conv_descriptor_data (tree desc)
{
  return gfc_get_descriptor_field (desc, DATA_FIELD);
}

/* This provides READ-ONLY access to the data field.  The field itself
   doesn't have the proper type.  */

tree
gfc_conv_descriptor_data_get (tree desc)
{
  tree type = TREE_TYPE (desc);
  if (TREE_CODE (type) == REFERENCE_TYPE)
    gcc_unreachable ();

  tree data = conv_descriptor_data (desc);
  return fold_convert (GFC_TYPE_ARRAY_DATAPTR_TYPE (type), data);
}

/* This provides WRITE access to the data field.  */

void
gfc_conv_descriptor_data_set (stmtblock_t *block, tree desc, tree value)
{
  tree data = conv_descriptor_data (desc);
  gfc_add_modify (block, data, fold_convert (TREE_TYPE (data), value));
}


/* Return a reference to the offset field of the array descriptor DESC.  */

static tree
conv_descriptor_offset (tree desc)
{
  tree field = gfc_get_descriptor_field (desc, OFFSET_FIELD);
  gcc_assert (TREE_TYPE (field) == gfc_array_index_type);
  return field;
}

/* Return the offset value of the array descriptor DESC.  */

tree
gfc_conv_descriptor_offset_get (tree desc)
{
  return conv_descriptor_offset (desc);
}

/* Add code to BLOCK assigning VALUE to the offset field of the array descriptor
   DESC.  */

void
gfc_conv_descriptor_offset_set (stmtblock_t *block, tree desc, tree value)
{
  tree t = conv_descriptor_offset (desc);
  gfc_add_modify (block, t, fold_convert (TREE_TYPE (t), value));
}


/* Return a reference to the dtype field of the array descriptor DESC.  */

static tree
conv_descriptor_dtype (tree desc)
{
  tree field = gfc_get_descriptor_field (desc, DTYPE_FIELD);
  gcc_assert (TREE_TYPE (field) == get_dtype_type_node ());
  return field;
}

/* Return the dtype value of the array descriptor DESC.  */

tree
gfc_conv_descriptor_dtype_get (tree desc)
{
  return conv_descriptor_dtype (desc);
}

/* Add code to BLOCK assigning VALUE to the dtype field of the array descriptor
   DESC.  */

void
gfc_conv_descriptor_dtype_set (stmtblock_t *block, tree desc, tree value)
{
  location_t loc = input_location;
  tree t = conv_descriptor_dtype (desc);
  gfc_add_modify_loc (loc, block, t,
		      fold_convert_loc (loc, TREE_TYPE (t), value));
}


/* Return a reference to the span field of the array descriptor DESC.  */

static tree
conv_descriptor_span (tree desc)
{
  tree field = gfc_get_descriptor_field (desc, SPAN_FIELD);
  gcc_assert (TREE_TYPE (field) == gfc_array_index_type);
  return field;
}

/* Return the span value of the array descriptor DESC.  */

tree
gfc_conv_descriptor_span_get (tree desc)
{
  return conv_descriptor_span (desc);
}

/* Add code to BLOCK assigning VALUE to the span field of the array descriptor
   DESC.  */

void
gfc_conv_descriptor_span_set (stmtblock_t *block, tree desc, tree value)
{
  tree t = conv_descriptor_span (desc);
  gfc_add_modify (block, t, fold_convert (TREE_TYPE (t), value));
}


/* Return a reference to the rank field of the array descriptor DESC.  */

static tree
conv_descriptor_rank (tree desc)
{
  tree tmp;
  tree dtype;

  dtype = conv_descriptor_dtype (desc);
  tmp = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (dtype)), GFC_DTYPE_RANK);
  gcc_assert (tmp != NULL_TREE
	      && TREE_TYPE (tmp) == gfc_array_dim_rank_type);
  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			  dtype, tmp, NULL_TREE);
}

/* Return the rank value of the array descriptor DESC.  */

tree
gfc_conv_descriptor_rank_get (tree desc)
{
  return conv_descriptor_rank (desc);
}

/* Add code to BLOCK assigning VALUE to the rank field of the array descriptor
   DESC.  */

void
gfc_conv_descriptor_rank_set (stmtblock_t *block, tree desc, tree value)
{
  location_t loc = input_location;
  tree t = conv_descriptor_rank (desc);
  gfc_add_modify_loc (loc, block, t,
		      fold_convert_loc (loc, TREE_TYPE (t), value));
}

/* Add code to BLOCK assigning VALUE to the rank field of the array descriptor
   DESC.  */

void
gfc_conv_descriptor_rank_set (stmtblock_t *block, tree desc, int value)
{
  gfc_conv_descriptor_rank_set (block, desc, gfc_rank_cst[value]);
}


/* Return a reference to the format version field of the array descriptor
   DESC.  */

static tree
conv_descriptor_version (tree desc)
{
  tree tmp;
  tree dtype;

  dtype = conv_descriptor_dtype (desc);
  tmp = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (dtype)), GFC_DTYPE_VERSION);
  gcc_assert (tmp != NULL_TREE
	      && TREE_TYPE (tmp) == integer_type_node);
  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			  dtype, tmp, NULL_TREE);
}

/* Return the format version value of the array descriptor DESC.  */

tree
gfc_conv_descriptor_version_get (tree desc)
{
  return conv_descriptor_version (desc);
}

/* Add code to BLOCK assigning VALUE to the format version field of the array
   descriptor DESC.  */

void
gfc_conv_descriptor_version_set (stmtblock_t *block, tree desc, tree value)
{
  location_t loc = input_location;
  tree t = conv_descriptor_version (desc);
  gfc_add_modify_loc (loc, block, t,
		      fold_convert_loc (loc, TREE_TYPE (t), value));
}


/* Return a reference to the element length field of the array descriptor
   DESC.  */

static tree
conv_descriptor_elem_len (tree desc)
{
  tree tmp;
  tree dtype;

  dtype = conv_descriptor_dtype (desc);
  tmp = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (dtype)),
			   GFC_DTYPE_ELEM_LEN);
  gcc_assert (tmp != NULL_TREE
	      && TREE_TYPE (tmp) == size_type_node);
  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			  dtype, tmp, NULL_TREE);
}

/* Return the element length value of the array descriptor DESC.  */

tree
gfc_conv_descriptor_elem_len_get (tree desc)
{
  return conv_descriptor_elem_len (desc);
}

/* Add code to BLOCK assigning VALUE to the element length field of the array
   descriptor DESC.  */

void
gfc_conv_descriptor_elem_len_set (stmtblock_t *block, tree desc, tree value)
{
  location_t loc = input_location;
  tree t = conv_descriptor_elem_len (desc);
  gfc_add_modify_loc (loc, block, t,
		      fold_convert_loc (loc, TREE_TYPE (t), value));
}


/* Return a reference to the type discriminator field of the array descriptor
   DESC.  */

static tree
conv_descriptor_type (tree desc)
{
  tree tmp;
  tree dtype;

  dtype = conv_descriptor_dtype (desc);
  tmp = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (dtype)), GFC_DTYPE_TYPE);
  gcc_assert (tmp!= NULL_TREE
	      && TREE_TYPE (tmp) == signed_char_type_node);
  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			  dtype, tmp, NULL_TREE);
}

/* Return the type discriminator value of the array descriptor DESC.  */

tree
gfc_conv_descriptor_type_get (tree desc)
{
  return conv_descriptor_type (desc);
}

/* Add code to BLOCK assigning VALUE to the type discriminator field of the
   array descriptor DESC.  */

void
gfc_conv_descriptor_type_set (stmtblock_t *block, tree desc, tree value)
{
  location_t loc = input_location;
  tree t = conv_descriptor_type (desc);
  gfc_add_modify_loc (loc, block, t,
		      fold_convert_loc (loc, TREE_TYPE (t), value));
}

/* Add code to BLOCK assigning VALUE to the type discriminator field of the
   array descriptor DESC.  */

void
gfc_conv_descriptor_type_set (stmtblock_t *block, tree desc, int value)
{
  tree type = TREE_TYPE (desc);
  gcc_assert (GFC_DESCRIPTOR_TYPE_P (type));

  tree dtype = get_type_field (type, DTYPE_FIELD);
  tree field = get_type_field (TREE_TYPE (dtype), GFC_DTYPE_TYPE);
  tree type_value = build_int_cst (TREE_TYPE (field), value);

  gfc_conv_descriptor_type_set (block, desc, type_value);
}

/* Return a statement assigning VALUE to the type discriminator field of the
   array descriptor DESC.  */

tree
gfc_conv_descriptor_type_set (tree desc, tree value)
{
  stmtblock_t block;

  gfc_init_block (&block);
  gfc_conv_descriptor_type_set (&block, desc, value);
  return gfc_finish_block (&block);
}

/* Return a statement assigning VALUE to the type discriminator field of the
   array descriptor DESC.  */

tree
gfc_conv_descriptor_type_set (tree desc, int value)
{
  stmtblock_t block;

  gfc_init_block (&block);
  gfc_conv_descriptor_type_set (&block, desc, value);
  return gfc_finish_block (&block);
}


/* Return a reference to the array of dimension descriptors of the array
   descriptor DESC.  */

tree
gfc_get_descriptor_dimension (tree desc)
{
  tree field = gfc_get_descriptor_field (desc, DIMENSION_FIELD);
  gcc_assert (TREE_CODE (TREE_TYPE (field)) == ARRAY_TYPE
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (field))) == RECORD_TYPE);
  return field;
}


/* Return a reference to the dimension descriptor for the (zero-based) dimension
   DIM of the array descriptor DESC.  */

static tree
conv_descriptor_dimension (tree desc, tree dim)
{
  tree tmp;

  tmp = gfc_get_descriptor_dimension (desc);

  return gfc_build_array_ref (tmp, dim, NULL_TREE, true);
}


/* Return a reference to the coarray token field of the array descriptor
   DESC.  */

tree
gfc_conv_descriptor_token (tree desc)
{
  gcc_assert (flag_coarray == GFC_FCOARRAY_LIB);
  tree field = gfc_get_descriptor_field (desc, CAF_TOKEN_FIELD);
  /* Should be a restricted pointer - except in the finalization wrapper.  */
  gcc_assert (TREE_TYPE (field) == prvoid_type_node
	      || TREE_TYPE (field) == pvoid_type_node);
  return field;
}

/* Add code to BLOCK assigning VALUE to the coarray token field of the array
   descriptor DESC.  */

void
gfc_conv_descriptor_token_set (stmtblock_t *block, tree desc, tree value)
{
  location_t loc = input_location;
  tree t = gfc_conv_descriptor_token (desc);
  gfc_add_modify_loc (loc, block, t,
		      fold_convert_loc (loc, TREE_TYPE (t), value));
}


/* Return a reference to the FIELD_IDX'th subfield of the dimension descriptor
   of the (zero-based) dimension DIM of the array descriptor DESC.  */

static tree
gfc_conv_descriptor_subfield (tree desc, tree dim, unsigned field_idx)
{
  tree tmp = conv_descriptor_dimension (desc, dim);
  tree field = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (tmp)), field_idx);
  gcc_assert (field != NULL_TREE);

  return fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			  tmp, field, NULL_TREE);
}


/* Return a reference to the stride field of the (zero-based) dimension DIM of
   the array descriptor DESC.  */

static tree
conv_descriptor_stride (tree desc, tree dim)
{
  tree field = gfc_conv_descriptor_subfield (desc, dim, STRIDE_SUBFIELD);
  gcc_assert (TREE_TYPE (field) == gfc_array_index_type);
  return field;
}

/* Return the stride value for the (zero-based) dimension DIM of the array
   descriptor DESC.  */

tree
gfc_conv_descriptor_stride_get (tree desc, tree dim)
{
  tree type = TREE_TYPE (desc);
  gcc_assert (GFC_DESCRIPTOR_TYPE_P (type));
  if (integer_zerop (dim)
      && (GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ALLOCATABLE
	  || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ASSUMED_SHAPE_CONT
	  || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ASSUMED_RANK_CONT
	  || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ASSUMED_RANK_ALLOCATABLE
	  || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ASSUMED_RANK_POINTER_CONT
	  || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_POINTER_CONT))
    return gfc_index_one_node;

  return conv_descriptor_stride (desc, dim);
}

/* Add code to BLOCK assigning VALUE to the stride field of the (zero-based)
   dimension DIM of the array descriptor DESC.  */

void
gfc_conv_descriptor_stride_set (stmtblock_t *block, tree desc,
				tree dim, tree value)
{
  tree t = conv_descriptor_stride (desc, dim);
  gfc_add_modify (block, t, fold_convert (TREE_TYPE (t), value));
}


/* Return a reference to the lower bound field of the (zero-based) dimension DIM
   of the array descriptor DESC.  */

static tree
conv_descriptor_lbound (tree desc, tree dim)
{
  tree field = gfc_conv_descriptor_subfield (desc, dim, LBOUND_SUBFIELD);
  gcc_assert (TREE_TYPE (field) == gfc_array_index_type);
  return field;
}

/* Return the lower bound value for the (zero-based) dimension DIM of the array
   descriptor DESC.  */

tree
gfc_conv_descriptor_lbound_get (tree desc, tree dim)
{
  return conv_descriptor_lbound (desc, dim);
}

/* Add code to BLOCK assigning VALUE to the lower bound field of the
   (zero-based) dimension DIM of the array descriptor DESC.  */

void
gfc_conv_descriptor_lbound_set (stmtblock_t *block, tree desc,
				tree dim, tree value)
{
  tree t = conv_descriptor_lbound (desc, dim);
  gfc_add_modify (block, t, fold_convert (TREE_TYPE (t), value));
}


/* Return a reference to the upper bound field of the (zero-based) dimension DIM
   of the array descriptor DESC.  */

static tree
conv_descriptor_ubound (tree desc, tree dim)
{
  tree field = gfc_conv_descriptor_subfield (desc, dim, UBOUND_SUBFIELD);
  gcc_assert (TREE_TYPE (field) == gfc_array_index_type);
  return field;
}

/* Return the upper bound value for the (zero-based) dimension DIM of the array
   descriptor DESC.  */

tree
gfc_conv_descriptor_ubound_get (tree desc, tree dim)
{
  return conv_descriptor_ubound (desc, dim);
}

/* Add code to BLOCK assigning VALUE to the upper bound field of the
   (zero-based) dimension DIM of the array descriptor DESC.  */

void
gfc_conv_descriptor_ubound_set (stmtblock_t *block, tree desc,
				tree dim, tree value)
{
  tree t = conv_descriptor_ubound (desc, dim);
  gfc_add_modify (block, t, fold_convert (TREE_TYPE (t), value));
}


/* Obtain offsets for trans-types.cc(gfc_get_array_descr_info).  */

void
gfc_get_descriptor_offsets_for_info (const_tree desc_type, tree *data_off,
				     tree *rank_off, tree *span_off,
				     tree *dim_off, tree *dim_size,
				     tree *stride_suboff, tree *lower_suboff,
				     tree *upper_suboff)
{
  tree field;
  tree type;

  type = TYPE_MAIN_VARIANT (desc_type);
  tree fields = TYPE_FIELDS (type);
  field = gfc_advance_chain (fields, DATA_FIELD);
  *data_off = byte_position (field);
  field = gfc_advance_chain (fields, DTYPE_FIELD);
  tree dtype_off = byte_position (field);
  type = TREE_TYPE (field);
  field = gfc_advance_chain (TYPE_FIELDS (type), GFC_DTYPE_RANK);
  tree rank_suboff = byte_position (field);
  *rank_off = fold_build2 (PLUS_EXPR, TREE_TYPE (dtype_off), dtype_off,
			   rank_suboff);
  field = gfc_advance_chain (fields, SPAN_FIELD);
  *span_off = byte_position (field);
  field = gfc_advance_chain (fields, DIMENSION_FIELD);
  *dim_off = byte_position (field);
  type = TREE_TYPE (TREE_TYPE (field));
  *dim_size = TYPE_SIZE_UNIT (type);
  field = gfc_advance_chain (TYPE_FIELDS (type), STRIDE_SUBFIELD);
  *stride_suboff = byte_position (field);
  field = gfc_advance_chain (TYPE_FIELDS (type), LBOUND_SUBFIELD);
  *lower_suboff = byte_position (field);
  field = gfc_advance_chain (TYPE_FIELDS (type), UBOUND_SUBFIELD);
  *upper_suboff = byte_position (field);
}


/* Array descriptor higher level routines.
 ******************************************************************************/

/* Return a constructor for a descriptor dtype with the caracteristics given by
   the arguments.  */

tree
gfc_build_dtype_constructor (tree size, int type, int rank)
{
  tree field;
  vec<constructor_elt, va_gc> *v = NULL;

  gcc_assert (size);

  STRIP_NOPS (size);
  size = fold_convert (size_type_node, size);
  tree dtype_type_node = get_dtype_type_node ();
  field = gfc_advance_chain (TYPE_FIELDS (dtype_type_node),
			     GFC_DTYPE_ELEM_LEN);
  CONSTRUCTOR_APPEND_ELT (v, field,
			  fold_convert (TREE_TYPE (field), size));
  field = gfc_advance_chain (TYPE_FIELDS (dtype_type_node),
			     GFC_DTYPE_VERSION);
  CONSTRUCTOR_APPEND_ELT (v, field,
			  build_zero_cst (TREE_TYPE (field)));

  field = gfc_advance_chain (TYPE_FIELDS (dtype_type_node),
			     GFC_DTYPE_RANK);
  if (rank >= 0)
    CONSTRUCTOR_APPEND_ELT (v, field,
			    build_int_cst (TREE_TYPE (field), rank));

  field = gfc_advance_chain (TYPE_FIELDS (dtype_type_node),
			     GFC_DTYPE_TYPE);
  CONSTRUCTOR_APPEND_ELT (v, field,
			  build_int_cst (TREE_TYPE (field), type));

  return build_constructor (dtype_type_node, v);
}


/* Build a null array descriptor constructor.  */

tree
gfc_build_null_descriptor (tree type)
{
  tree field;
  tree tmp;

  gcc_assert (GFC_DESCRIPTOR_TYPE_P (type));
  gcc_assert (DATA_FIELD == 0);
  field = TYPE_FIELDS (type);

  /* Set a NULL data pointer.  */
  tmp = build_constructor_single (type, field, null_pointer_node);
  TREE_CONSTANT (tmp) = 1;
  /* All other fields are ignored.  */

  return tmp;
}


/* Cleanup those #defines.  */

#undef DATA_FIELD
#undef OFFSET_FIELD
#undef DTYPE_FIELD
#undef SPAN_FIELD
#undef DIMENSION_FIELD
#undef CAF_TOKEN_FIELD

#undef STRIDE_SUBFIELD
#undef LBOUND_SUBFIELD
#undef UBOUND_SUBFIELD

#undef GFC_DTYPE_ELEM_LEN
#undef GFC_DTYPE_VERSION
#undef GFC_DTYPE_RANK
#undef GFC_DTYPE_TYPE
#undef GFC_DTYPE_ATTRIBUTE


/* For an array descriptor, get the total number of elements.  This is just
   the product of the extents along from_dim to to_dim.  */

static tree
gfc_conv_descriptor_size_1 (tree desc, int from_dim, int to_dim)
{
  tree res;
  int dim;

  res = gfc_index_one_node;

  for (dim = from_dim; dim < to_dim; ++dim)
    {
      tree lbound;
      tree ubound;
      tree extent;

      lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[dim]);
      ubound = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[dim]);

      extent = gfc_conv_array_extent_dim (lbound, ubound, NULL);
      res = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			     res, extent);
    }

  return res;
}


/* Full size of an array.  */

tree
gfc_conv_descriptor_size (tree desc, int rank)
{
  return gfc_conv_descriptor_size_1 (desc, 0, rank);
}


/* Size of a coarray for all dimensions but the last.  */

tree
gfc_conv_descriptor_cosize (tree desc, int rank, int corank)
{
  return gfc_conv_descriptor_size_1 (desc, rank, rank + corank - 1);
}


/* Modify a descriptor such that the lbound of a given dimension is the value
   specified.  This also updates ubound and offset accordingly.  */

void
gfc_conv_shift_descriptor_lbound (stmtblock_t* block, tree desc,
				  int dim, tree new_lbound)
{
  tree offs, ubound, lbound, stride;
  tree diff, offs_diff;

  new_lbound = fold_convert (gfc_array_index_type, new_lbound);

  offs = gfc_conv_descriptor_offset_get (desc);
  lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[dim]);
  ubound = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[dim]);
  stride = gfc_conv_descriptor_stride_get (desc, gfc_rank_cst[dim]);

  /* Get difference (new - old) by which to shift stuff.  */
  diff = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			  new_lbound, lbound);

  /* Shift ubound and offset accordingly.  This has to be done before
     updating the lbound, as they depend on the lbound expression!  */
  ubound = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			    ubound, diff);
  gfc_conv_descriptor_ubound_set (block, desc, gfc_rank_cst[dim], ubound);
  offs_diff = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			       diff, stride);
  offs = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			  offs, offs_diff);
  gfc_conv_descriptor_offset_set (block, desc, offs);

  /* Finally set lbound to value we want.  */
  gfc_conv_descriptor_lbound_set (block, desc, gfc_rank_cst[dim], new_lbound);
}


void
gfc_copy_descriptor (stmtblock_t *block, tree dst, tree src, int rank)
{
  int n;
  tree dim;
  tree tmp;
  tree tmp2;
  tree size;
  tree offset;

  offset = gfc_index_zero_node;

  /* Use memcpy to copy the descriptor.  The size is the minimum of
     the sizes of 'src' and 'dst'. This avoids a non-trivial conversion.  */
  tmp = TYPE_SIZE_UNIT (TREE_TYPE (src));
  tmp2 = TYPE_SIZE_UNIT (TREE_TYPE (dst));
  size = fold_build2_loc (input_location, MIN_EXPR,
			  TREE_TYPE (tmp), tmp, tmp2);
  tmp = builtin_decl_explicit (BUILT_IN_MEMCPY);
  tmp = build_call_expr_loc (input_location, tmp, 3,
			     gfc_build_addr_expr (NULL_TREE, dst),
			     gfc_build_addr_expr (NULL_TREE, src),
			     fold_convert (size_type_node, size));
  gfc_add_expr_to_block (block, tmp);

  /* Set the offset correctly.  */
  for (n = 0; n < rank; n++)
    {
      dim = gfc_rank_cst[n];
      tmp = gfc_conv_descriptor_lbound_get (src, dim);
      tmp2 = gfc_conv_descriptor_stride_get (src, dim);
      tmp = fold_build2_loc (input_location, MULT_EXPR, TREE_TYPE (tmp),
			     tmp, tmp2);
      offset = fold_build2_loc (input_location, MINUS_EXPR,
			TREE_TYPE (offset), offset, tmp);
      offset = gfc_evaluate_now (offset, block);
    }

  gfc_conv_descriptor_offset_set (block, dst, offset);
}


/* Extend the data in array DESC by EXTRA elements.  */

void
gfc_grow_array (stmtblock_t * pblock, tree desc, tree extra)
{
  tree arg0, arg1;
  tree tmp;
  tree size;
  tree ubound;

  if (integer_zerop (extra))
    return;

  ubound = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[0]);

  /* Add EXTRA to the upper bound.  */
  tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			 ubound, extra);
  gfc_conv_descriptor_ubound_set (pblock, desc, gfc_rank_cst[0], tmp);

  /* Get the value of the current data pointer.  */
  arg0 = gfc_conv_descriptor_data_get (desc);

  /* Calculate the new array size.  */
  size = TYPE_SIZE_UNIT (gfc_get_element_type (TREE_TYPE (desc)));
  tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			 ubound, gfc_index_one_node);
  arg1 = fold_build2_loc (input_location, MULT_EXPR, size_type_node,
			  fold_convert (size_type_node, tmp),
			  fold_convert (size_type_node, size));

  /* Call the realloc() function.  */
  tmp = gfc_call_realloc (pblock, arg0, arg1);
  gfc_conv_descriptor_data_set (pblock, desc, tmp);
}
