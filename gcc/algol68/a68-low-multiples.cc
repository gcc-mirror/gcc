/* Lowering routines for all things related to multiples.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"

/* Algol 68 multiples are multi-dimensional and dynamically sized. They have a
   static part and a dynamic part.  The static part is conformed by a
   "descriptor", which contains information about each of the dimensions, and a
   pointer to the actual elements stored in the multiple.  The dynamic part are
   the elements, which are stored in column order.  Both the descriptor and the
   elements may reside on the stack, data section, or the heap.  The mode of a
   multiple is a "row".

   Schematically, the descriptor contains:

      triplets%
        lb% ub% stride%
	...
      elements%
      elements_size%

   Where elements_size% is the size of the buffer pointed by elements%, in
   bytes.

   There is a triplet per dimension in the multiple.  The number of dimensions
   in a row mode is static and is determined at compile-time.

   The infomation stored for each triplet is:

     lb%     is the lower bound of the dimension.
     ub%     is the upper bound of the dimension.
     stride% is the stride of the dimension.

   The stride of each dimension is the number of bytes to skip in order to
   access the next element in that dimension.  They express the layout of the
   multiple in memory.

   Algol 68 multi-dimensional multiples are stored in row-major (generalized,
   lexicographical) order:

     [1:3,1:2]AMODE = ((e1, e2, e3),
                       (e4, e5, e6))

   is stored as:

          1  2  3
       1  e1 e2 e3      | stride 2S ->  stride 1S
       2  e4 e5 e6      v

   Where S is the size in bytes of a single element.  That means that for two
   dimensional multiples, the column stride is always 1S and the row stride is
   the column size.

   In general, given a mode with number of elements N1, N2, N3, ...:

     [N1,N2,N3...,Nn]AMODE

   the strides of the dimensions are:

     S1 = N2 * S2
     S2 = N3 * S3
     S3 = N4 * S4
     ...
     Si = N1 * N2 * ... * Ni-1

   Indexing is then performed by a dot-product of an element coordinate and the
   strides:

     (i1,i2,i3) . (S1,S2,S3) = offset + i1*S1 + i2*S2 + i3*S3 = index in elements array.

   Note that the number of elements in each dimension can be easily derived
   from the bounds and there is no need to store them explicitly, save for
   performance reasons.  Descriptors are bulky enough and often they they are
   stored on the stack, so we prefer to pay in performance and save in
   storage.  */

/* Return a tree with the yielding of SKIP for the given row mode, a
   multiple.  */

tree
a68_get_multiple_skip_tree (MOID_T *m)
{
  tree res = NULL_TREE;
  int dim = DIM (m);
  tree *lower_bounds = (tree *) xmalloc (sizeof (tree) * dim);
  tree *upper_bounds = (tree *) xmalloc (sizeof (tree) * dim);
  tree ssize_one_node = fold_convert (ssizetype, size_one_node);
  tree ssize_zero_node = fold_convert (ssizetype, size_zero_node);
  for (int i = 0; i < dim; ++i)
    {
      lower_bounds[i] = ssize_one_node;
      upper_bounds[i] = ssize_zero_node;
    }
  res = a68_row_value (CTYPE (m), dim,
		       build_int_cst (build_pointer_type (void_type_node), 0),
		       size_zero_node, /* elements_size */
		       lower_bounds, upper_bounds);
  free (lower_bounds);
  free (upper_bounds);
  return res;
}

/* Return the number of dimensions of the multiple EXP as an integer
   constant.  */

tree
a68_multiple_dimensions (tree exp)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (exp)));

  /* triplets% is the first field in the descriptor.  */
  tree triplets_field = TYPE_FIELDS (TREE_TYPE (exp));
  return array_type_nelts_top (TREE_TYPE (triplets_field));
}

/* Return an expression that evaluates to the total number of elements stored
   in a multiple as a sizetype.  */

tree
a68_multiple_num_elems (tree exp)
{
  /* We have to calculate the number of elements based on the dimension
     triplets in the array type.  The number of dimensions is known at compile
     time, so we don't really need a loop.  */

  tree num_dimensions_tree = a68_multiple_dimensions (exp);
  gcc_assert (TREE_CODE (num_dimensions_tree) == INTEGER_CST);
  int num_dimensions = tree_to_shwi (num_dimensions_tree);

  tree size = NULL_TREE;
  for (int dim = 0; dim < num_dimensions; ++dim)
    {
      tree size_dim = size_int (dim);
      tree lower_bound = a68_multiple_lower_bound (exp, size_dim);
      tree upper_bound = a68_multiple_upper_bound (exp, size_dim);
      tree dim_size = fold_build2 (PLUS_EXPR, sizetype,
				   fold_convert (sizetype, fold_build2 (MINUS_EXPR,
									ssizetype,
									upper_bound,
									lower_bound)),
				   size_one_node);

      if (size == NULL_TREE)
	size = dim_size;
      else
	size = fold_build2 (MULT_EXPR, sizetype, size, dim_size);
    }

  return size;
}

/* Return a size expression that evaluates to the total size, in bytes, of the
   elements stored in the multiple.  */

tree
a68_multiple_elements_size (tree exp)
{
  tree type = TREE_TYPE (exp);
  gcc_assert (A68_ROW_TYPE_P (type));

  /* elements_size% is the third field in the descriptor.  */
  tree elements_size_field = TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (type)));
  return fold_build3 (COMPONENT_REF, TREE_TYPE (elements_size_field),
		      exp, elements_size_field, NULL_TREE);
}

/* Return the triplet for dimension DIM in the multiple EXP.  */

static tree
multiple_triplet (tree exp, tree dim)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (exp)));

  /* triplets% is the first field in the descriptor.  */
  tree triplets_field = TYPE_FIELDS (TREE_TYPE (exp));
  tree triplets = fold_build3 (COMPONENT_REF,
			       TREE_TYPE (triplets_field),
			       exp,
			       triplets_field,
			       NULL_TREE);

  /* Get the triplet for the given dimension.  */
  return build4 (ARRAY_REF,
		 TREE_TYPE (TREE_TYPE (triplets)),
		 triplets,
		 dim,
		 NULL_TREE,
		 NULL_TREE);
}

/* Return the lower bound of dimension DIM of the multiple EXP.  The returned
   value is a ssizetype.  */

tree
a68_multiple_lower_bound (tree exp, tree dim)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (exp)));

  /* lb% is the first field in the triplet.  */
  tree triplet = multiple_triplet (exp, dim);
  tree lower_bound_field = TYPE_FIELDS (TREE_TYPE (triplet));
  return fold_build3 (COMPONENT_REF,
		      TREE_TYPE (lower_bound_field),
		      triplet,
		      lower_bound_field,
		      NULL_TREE);
}

/* Return an expression that sets the lower bound of dimension DIM of the
   multiple EXP to BOUND.  */

tree
a68_multiple_set_lower_bound (tree exp, tree dim, tree bound)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (exp)));
  return fold_build2 (MODIFY_EXPR,
		      TREE_TYPE (bound),
		      a68_multiple_lower_bound (exp, dim),
		      bound);
}

/* Return the upper bound of dimension DIM of the multiple EXP.  The returned
   value is a ssizetype.  */

tree
a68_multiple_upper_bound (tree exp, tree dim)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (exp)));

  /* ub% is the second field in the triplet.  */
  tree triplet = multiple_triplet (exp, dim);
  tree upper_bound_field = TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (triplet)));
  return fold_build3 (COMPONENT_REF,
		      TREE_TYPE (upper_bound_field),
		      triplet,
		      upper_bound_field,
		      NULL_TREE);
}

/* Return an expression that sets the upper bound of dimension DIM of the
   multiple EXP to BOUND.  */

tree
a68_multiple_set_upper_bound (tree exp, tree dim, tree bound)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (exp)));
  return fold_build2 (MODIFY_EXPR,
		      TREE_TYPE (bound),
		      a68_multiple_upper_bound (exp, dim),
		      bound);
}

/* Return the stride of dimension DIM of the multiple EXP.  */

tree
a68_multiple_stride (tree exp, tree dim)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (exp)));

  /* stride% is the third field in the triplet.  */
  tree triplet = multiple_triplet (exp, dim);
  tree stride_field = TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (triplet))));
  return fold_build3 (COMPONENT_REF,
		      TREE_TYPE (stride_field),
		      triplet,
		      stride_field,
		      NULL_TREE);
}

/* Return an expression that sets the stride of dimension DIM of the multiple
   EXP to STRIDE.

   STRIDE must be a sizetype.  */

tree
a68_multiple_set_stride (tree exp, tree dim, tree stride)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (exp)));
  return fold_build2 (MODIFY_EXPR,
		      TREE_TYPE (stride),
		      a68_multiple_stride (exp, dim),
		      stride);
}

/* Return the triplets of the multiple EXP.  */

tree
a68_multiple_triplets (tree exp)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (exp)));

  /* triplets% is the first field in the descriptor.  */
  tree triplets_field = TYPE_FIELDS (TREE_TYPE (exp));
  return fold_build3 (COMPONENT_REF,
		      TREE_TYPE (triplets_field),
		      exp,
		      triplets_field,
		      NULL_TREE);
}

/* Return the pointer to the elements of the multiple EXP.  */

tree
a68_multiple_elements (tree exp)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (exp)));

  /* elements% is the second field in the descriptor.  */
  tree elements_field = TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (exp)));
  return fold_build3 (COMPONENT_REF,
		      TREE_TYPE (elements_field),
		      exp,
		      elements_field,
		      NULL_TREE);
}

/* Return an expression that sets the elements% field of EXP to ELEMENTS.  */

tree
a68_multiple_set_elements (tree exp, tree elements)
{
  /* elements% is the second field in the descriptor.  */
  tree elements_field = TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (exp)));
  return fold_build2 (MODIFY_EXPR,
		      TREE_TYPE (elements_field),
		      fold_build3 (COMPONENT_REF,
				   TREE_TYPE (elements_field),
				   exp,
				   elements_field,
				   NULL_TREE),
		      elements);
}

/* Return an expression that sets the elements_size% field of EXP to
   ELEMENTS_SIZE, which must be a sizetype.  */

tree
a68_multiple_set_elements_size (tree exp, tree elements_size)
{
  /* elements_size% is the third field in the descriptor.  */
  tree elements_size_field = TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (TREE_TYPE (exp))));
  return fold_build2 (MODIFY_EXPR,
		      TREE_TYPE (elements_size_field),
		      fold_build3 (COMPONENT_REF,
				   TREE_TYPE (elements_size_field),
				   exp,
				   elements_size_field,
				   NULL_TREE),
		      elements_size);
}

/* Given two arrays of LOWER_BOUNDs and UPPER_BOUNDs corresponding to DIM
   dimensions of a multiple of type TYPE, fill in the strides in STRIDES, which
   is assumed to be a buffer big enough to hold DIM tree nodes.  The bounds
   shall be of type ssizetype, and the calculated strides are of type sizetype,
   i.e. unsigned.  */

void
a68_multiple_compute_strides (tree type, size_t dim,
			      tree *lower_bounds, tree *upper_bounds,
			      tree *strides)
{
  tree stride = size_in_bytes (a68_row_elements_type (type));
  for (ssize_t i = dim - 1; i >= 0; --i)
    {
      strides[i] = stride;

      /* Calculate the stride for the previous dimension.  */
      tree dim_num_elems
	= save_expr (fold_build2 (PLUS_EXPR,
				  sizetype,
				  fold_convert (sizetype,
						fold_build2 (MINUS_EXPR, ssizetype,
							     upper_bounds[i], lower_bounds[i])),
				  size_one_node));
      stride = fold_build2 (MULT_EXPR, sizetype, stride, dim_num_elems);
    }
}

/* Return a constructor for a multiple of row type TYPE, using TRIPLETS and
   ELEMENTS.  ELEMENTS_SIZE is the size in bytes of the memory pointed by
   ELEMENTS.  */

tree
a68_row_value_raw (tree type, tree triplets,
		   tree elements, tree elements_size)
{
  tree triplets_field;
  tree elements_field;
  tree elements_size_field;
  vec <constructor_elt, va_gc> *ce = NULL;

  gcc_assert (A68_ROW_TYPE_P (type));
  triplets_field = TYPE_FIELDS (type);
  elements_field = TREE_CHAIN (triplets_field);
  elements_size_field = TREE_CHAIN (elements_field);
  CONSTRUCTOR_APPEND_ELT (ce, triplets_field, triplets);
  CONSTRUCTOR_APPEND_ELT (ce, elements_field,
			  fold_build1 (CONVERT_EXPR ,TREE_TYPE (elements_field), elements));
  CONSTRUCTOR_APPEND_ELT (ce, elements_size_field, elements_size);
  return build_constructor (type, ce);
}

/* Return a constructor for a multiple of row type TYPE, of DIM dimensions and
   pointing to ELEMENTS.

   ELEMENTS_SIZE contains the size in bytes of the memory pointed by ELEMENTS.

   *LOWER_BOUND and *UPPER_BOUND are the bounds for the DIM dimensions.
*/

tree
a68_row_value (tree type, size_t dim,
	       tree elements, tree elements_size,
	       tree *lower_bound, tree *upper_bound)
{
  tree triplets_field;
  tree elements_field;
  tree elements_size_field;
  vec <constructor_elt, va_gc> *ce = NULL;

  gcc_assert (A68_ROW_TYPE_P (type));
  triplets_field = TYPE_FIELDS (type);
  elements_field = TREE_CHAIN (triplets_field);
  elements_size_field = TREE_CHAIN (elements_field);

  tree triplet_type = TREE_TYPE (TREE_TYPE (triplets_field));
  tree lower_bound_field = TYPE_FIELDS (triplet_type);
  tree upper_bound_field = TREE_CHAIN (TYPE_FIELDS (triplet_type));
  tree stride_field = TREE_CHAIN (TREE_CHAIN (TYPE_FIELDS (triplet_type)));

  /* Calculate strides.  */
  tree *strides = (tree *) xmalloc (sizeof (tree) * dim);
  a68_multiple_compute_strides (type, dim, lower_bound, upper_bound, strides);

  vec <constructor_elt, va_gc> *triplets_ce = NULL;
  for (size_t i = 0; i < dim; ++i)
    {
      CONSTRUCTOR_APPEND_ELT (triplets_ce,
			      size_int (i),
			      build_constructor_va (triplet_type,
						    3,
						    lower_bound_field, lower_bound[i],
						    upper_bound_field, upper_bound[i],
						    stride_field, strides[i]));
    }
  free (strides);
  CONSTRUCTOR_APPEND_ELT (ce, triplets_field,
			  build_constructor (TREE_TYPE (triplets_field), triplets_ce));
  CONSTRUCTOR_APPEND_ELT (ce, elements_field,
			  fold_build1 (CONVERT_EXPR, TREE_TYPE (elements_field), elements));
  CONSTRUCTOR_APPEND_ELT (ce, elements_size_field,
			  elements_size ? elements_size : size_zero_node);
  tree multiple = build_constructor (type, ce);
  return multiple;
}

/* Build a tree to slice a multiple given a set of indexes.

   P is the tree node corresponding to the slice.  It is used as the source of
   location information.

   MULTIPLE is the multiple value being sliced.  If SLICING_NAME is true, it
   means the slicing operation is for a name and therefore it must yield a
   name.

   INDEXES is a list of NUM_INDEXES indexes, which are units.
   NUM_INDEXES must match the dimension of the multiple.  */

tree
a68_multiple_slice (NODE_T *p,
		    tree multiple, bool slicing_name,
		    int num_indexes, tree *indexes)
{
  tree slice = NULL_TREE;
  tree bounds_check = NULL_TREE;

  multiple = save_expr (multiple);
  tree index = NULL_TREE;
  for (int idx = 0; idx < num_indexes; ++idx)
    {
      tree lower_bound = a68_multiple_lower_bound (multiple, size_int (idx));
      tree index_expr = save_expr (indexes[idx]);

      /* Do run-time bound checking if requested.  */
      if (OPTION_BOUNDS_CHECKING (&A68_JOB))
	{
	  tree upper_bound = a68_multiple_upper_bound (multiple, size_int (idx));
	  unsigned int lineno = NUMBER (LINE (INFO (p)));
	  const char *filename_str = FILENAME (LINE (INFO (p)));
	  tree filename = build_string_literal (strlen (filename_str) + 1,
						    filename_str);
	  tree call = a68_build_libcall (A68_LIBCALL_ARRAYBOUNDS,
					 void_type_node, 5,
					 filename,
					 build_int_cst (unsigned_type_node, lineno),
					 fold_convert (ssizetype, index_expr),
					 fold_convert (ssizetype, lower_bound),
					 fold_convert (ssizetype, upper_bound));
	  call = fold_build2 (COMPOUND_EXPR, a68_bool_type, call, boolean_false_node);

	  /* If LB > UB, the dimension contains no elements.
	     Otherwise, it must hold IDX >= LB && IDX <= UB */
	  tree dim_bounds_check = fold_build2 (TRUTH_AND_EXPR, sizetype,
					       fold_build2 (LE_EXPR, ssizetype,
							    lower_bound, upper_bound),
					       fold_build2 (TRUTH_AND_EXPR,
							    boolean_type_node,
							    fold_build2 (GE_EXPR, ssizetype,
									 fold_convert (ssizetype,
										       index_expr),
									 lower_bound),
							    fold_build2 (LE_EXPR, ssizetype,
									 fold_convert (ssizetype,
										       index_expr),
									 upper_bound)));
	  dim_bounds_check = fold_build2_loc (a68_get_node_location (p),
					      TRUTH_ORIF_EXPR,
					      ssizetype,
					      dim_bounds_check, call);

	  /* bounds_check_ok || call_runtime_error */
	  if (bounds_check == NULL_TREE)
	    bounds_check = dim_bounds_check;
	  else
	    bounds_check = fold_build2 (TRUTH_ANDIF_EXPR,
					ssizetype,
					bounds_check,
					dim_bounds_check);
	}

      /* Now add the effect of this dimension's subscript in the index.  Note
	 that the stride is expressed in bytes.  */
      tree stride = a68_multiple_stride (multiple, size_int (idx));
      tree adjusted_index
	= fold_convert (sizetype, fold_build2 (MINUS_EXPR, ssizetype,
					       fold_convert (ssizetype, index_expr),
					       lower_bound));
      tree term = fold_build2 (MULT_EXPR, sizetype,
			       adjusted_index, stride);
      if (index == NULL_TREE)
	index = term;
      else
	index = fold_build2 (PLUS_EXPR, sizetype,
			     index, term);
    }

  tree elements = a68_multiple_elements (multiple);
  tree element_pointer_type = TREE_TYPE (elements);
  tree element_type = TREE_TYPE (element_pointer_type);

  /* Now refer to the indexed element.  In case we are slicing a ref to a
     multiple, return the address of the element and not the element
     itself.  */
  tree element_address = fold_build2 (POINTER_PLUS_EXPR,
				      element_pointer_type,
				      elements,
				      index);
  if (slicing_name)
    slice = element_address;
  else
    slice = fold_build2 (MEM_REF,
			 element_type,
			 fold_build2 (POINTER_PLUS_EXPR,
				      element_pointer_type,
				      elements,
				      index),
			 fold_convert (element_pointer_type,
				       integer_zero_node));

  /* Prepend bounds checking code if necessary.  */
  if (bounds_check != NULL_TREE)
    {
      slice = fold_build2_loc (a68_get_node_location (p),
			       COMPOUND_EXPR,
			       TREE_TYPE (slice),
			       bounds_check,
			       slice);
    }

  return slice;
}

/* Auxiliary routine for a68_multiple_copy_elemens.  */

static tree
copy_multiple_dimension_elems (size_t dim, size_t num_dimensions,
			       tree to, tree from,
			       tree to_elements, tree from_elements,
			       tree *to_offset, tree *from_offset,
			       tree *indexes)
{
  tree element_pointer_type = TREE_TYPE (from_elements);
  tree element_type = TREE_TYPE (element_pointer_type);
  tree upb = a68_multiple_upper_bound (from, size_int (dim));

  char *name = xasprintf ("r" HOST_SIZE_T_PRINT_DEC "%%", (fmt_size_t) dim);
  indexes[dim] = a68_lower_tmpvar (name, ssizetype,
				   a68_multiple_lower_bound (from,
							     size_int (dim)));
  free (name);

  /* Loop body.  */
  a68_push_range (NULL);
  {
    /* if (indexes[dim] > upb) break; */
    a68_add_stmt (fold_build1 (EXIT_EXPR, void_type_node,
			       fold_build2 (GT_EXPR, size_type_node,
					    indexes[dim], upb)));

    /* Add this dimension's contribution to the offsets.  */
    tree index = fold_convert (sizetype,
			       fold_build2 (MINUS_EXPR, ssizetype,
					    upb, indexes[dim]));
    *to_offset = fold_build2 (PLUS_EXPR, sizetype,
			      *to_offset,
			      fold_build2 (MULT_EXPR, sizetype,
					   index,
					   a68_multiple_stride (to, size_int (dim))));
    *from_offset = fold_build2 (PLUS_EXPR, sizetype,
				*from_offset,
				fold_build2 (MULT_EXPR, sizetype,
					     index,
					     a68_multiple_stride (from, size_int (dim))));

    if (dim == num_dimensions - 1)
      {
	/* Most inner loop, copy one element.  */

	tree to_off = a68_lower_tmpvar ("to_offset%", sizetype, *to_offset);
	tree from_off = a68_lower_tmpvar ("from_offset%", sizetype, *from_offset);

	tree to_elem = fold_build2 (MEM_REF,
				    element_type,
				    fold_build2 (POINTER_PLUS_EXPR,
						 element_pointer_type,
						 to_elements,
						 to_off),
				    fold_convert (element_pointer_type,
						  integer_zero_node));
	tree from_elem = fold_build2 (MEM_REF,
				      element_type,
				      fold_build2 (POINTER_PLUS_EXPR,
						   element_pointer_type,
						   from_elements,
						   from_off),
				      fold_convert (element_pointer_type,
						    integer_zero_node));

	/* XXX
	   if may_overlap then modify only if dst_offset < src_offset */
	a68_add_stmt (fold_build2 (MODIFY_EXPR, element_type,
				   to_elem, from_elem));
      }
    else
      {
	a68_add_stmt (copy_multiple_dimension_elems (dim + 1, num_dimensions,
						     to, from,
						     to_elements, from_elements,
						     to_offset, from_offset,
						     indexes));
      }

    /* indexes[dim]++ */
    a68_add_stmt (fold_build2 (POSTINCREMENT_EXPR, ssizetype,
			       indexes[dim], ssize_int (1)));
  }
  tree loop_body = a68_pop_range ();

  return fold_build1 (LOOP_EXPR, void_type_node, loop_body);
}

/* Copy the elements of a given multiple (string) FROM to the multiple (string)
   TO.

   The dimensions and bounds of both multiples are supposed to match, and they
   are supposed to not be flat.

   XXX simple cases  with same strides may be done with a memcpy.
   XXX compile this into a support routine to reduce code size.  */

tree
a68_multiple_copy_elems (MOID_T *mode, tree to, tree from)
{
  gcc_assert (A68_ROW_TYPE_P (TREE_TYPE (to))
	      && A68_ROW_TYPE_P (TREE_TYPE (from)));

  /* Deflex modes as needed and determine dimension.  */
  if (IS_FLEX (mode))
    mode = SUB (mode);
  int num_dimensions = (mode == M_STRING ? 1 : DIM (mode));

  a68_push_range (NULL);
  to = a68_lower_tmpvar ("to%", TREE_TYPE (to), to);
  from = a68_lower_tmpvar ("from%", TREE_TYPE (from), from);
  tree from_elements = a68_multiple_elements (from);
  tree element_pointer_type = TREE_TYPE (from_elements);
  from_elements = a68_lower_tmpvar ("from_elements%", element_pointer_type,
				    from_elements);
  tree to_elements = a68_lower_tmpvar ("to_elements%", element_pointer_type,
				       a68_multiple_elements (to));

  tree *indexes = (tree *) xmalloc (num_dimensions * sizeof (tree));
  tree to_offset = size_zero_node;
  tree from_offset = size_zero_node;
  a68_add_stmt (copy_multiple_dimension_elems (0 /* dim */, num_dimensions,
					       to, from,
					       to_elements, from_elements,
					       &to_offset, &from_offset,
					       indexes));
  free (indexes);
  return a68_pop_range ();
}

/* Given a rows type, return the number of dimensions.  */

tree
a68_rows_dim (tree exp)
{
  gcc_assert (A68_ROWS_TYPE_P (TREE_TYPE (exp)));

  /* dim% is the first field in the rows struct.  */
  tree dim_field = TYPE_FIELDS (TREE_TYPE (exp));
  return fold_build3 (COMPONENT_REF,
		      TREE_TYPE (dim_field),
		      exp,
		      dim_field,
		      NULL_TREE);
}

/* Given a multiple value, create a rows value reflecting the multiple's
   dimensions and triplets.  */

tree
a68_rows_value (tree multiple)
{
  tree rows_type = CTYPE (M_ROWS);
  tree dim_field = TYPE_FIELDS (rows_type);
  tree triplets_field = TREE_CHAIN (dim_field);

  tree dimensions = save_expr (a68_multiple_dimensions (multiple));
  tree triplets = fold_build1 (ADDR_EXPR, TREE_TYPE (triplets_field),
			       a68_multiple_triplets (multiple));
  return build_constructor_va (rows_type, 2,
			       dim_field, dimensions,
			       triplets_field, triplets);
}

/* Given a rows value and a dimension number, return the upper bound or the
   lower of the given dimension.  The returned bound is a ssizetype.

   DIM must be a sizetype.  */

static tree
rows_lower_or_upper_bound (tree rows, tree dim, bool upper)
{
  tree rows_type = TREE_TYPE (rows);
  tree triplet_type = a68_triplet_type ();
  tree triplet_pointer_type = build_pointer_type (triplet_type);
  tree triplet_lb_field = TYPE_FIELDS (triplet_type);
  tree triplet_ub_field = TREE_CHAIN (TYPE_FIELDS (triplet_type));
  tree triplets_field = TREE_CHAIN (TYPE_FIELDS (rows_type));
  tree triplets = fold_build3 (COMPONENT_REF, triplet_pointer_type,
			       rows, triplets_field, NULL_TREE);
  tree triplet_offset = fold_build2 (MULT_EXPR, sizetype,
				     dim,
				     size_in_bytes (triplet_type));
  tree bound = fold_build3 (COMPONENT_REF, ssizetype,
			    fold_build1 (INDIRECT_REF, triplet_type,
					 fold_build2 (POINTER_PLUS_EXPR,
						      triplet_pointer_type,
						      triplets,
						      triplet_offset)),
			    upper ? triplet_ub_field : triplet_lb_field,
			    NULL_TREE);

  return bound;
}

/* Return the lower bound of dimension DIM of ROWS.  */

tree
a68_rows_lower_bound (tree rows, tree dim)
{
  return rows_lower_or_upper_bound (rows, dim, false);
}

/* Return the upper bound of dimension DIM of ROWS.  */

tree
a68_rows_upper_bound (tree rows, tree dim)
{
  return rows_lower_or_upper_bound (rows, dim, true);
}

/* Return a tree that checks that a given INDEX is correct given a multiple's
   bounds in a given rank DIM.

   If UPPER_BOUND is true then INDEX shall be less or equal than the multiple's
   upper bound.  Otherwise INDEX shall be bigger or equal than the multiple's
   lower bound.

   If the condition above doesn't hold then a call to a run-time function is
   performed: if UPPER_BOUND is true then ARRAYUPPERBOUND is called.  Otherwise
   ARRAYLOWERBOUND is called.  */

tree
a68_multiple_single_bound_check (NODE_T *p, tree dim,
				 tree multiple, tree index, bool upper_bound)
{
  index = save_expr (index);
  multiple = save_expr (multiple);

  tree bound = (upper_bound
		? a68_multiple_upper_bound (multiple, dim)
		: a68_multiple_lower_bound (multiple, dim));
  a68_libcall_fn libcall = (upper_bound
			    ? A68_LIBCALL_ARRAYUPPERBOUND
			    : A68_LIBCALL_ARRAYLOWERBOUND);

  /* Build the call to ARRAY*BOUNDS. */
  unsigned int lineno = NUMBER (LINE (INFO (p)));
  const char *filename_str = FILENAME (LINE (INFO (p)));
  tree filename = build_string_literal (strlen (filename_str) + 1,
					filename_str);
  tree call = a68_build_libcall (libcall,
				 void_type_node, 4,
				 filename,
				 build_int_cst (unsigned_type_node, lineno),
				 fold_convert (ssizetype, index),
				 fold_convert (ssizetype, bound));
  call = fold_build2 (COMPOUND_EXPR, a68_bool_type, call, boolean_false_node);

  tree bounds_check = fold_build2 (upper_bound ? LE_EXPR : GE_EXPR,
				   ssizetype,
				   fold_convert (ssizetype, index),
				   bound);
  return fold_build2_loc (a68_get_node_location (p),
			  TRUTH_ORIF_EXPR,
			  ssizetype,
			  bounds_check, call);
}

/* Return a tree that checks whether the given DIM is a valid dimension/rank of
   a boundable object with dimension BOUNDABLE_DIM.  If the provided DIM is not
   a valid dimention then a call to the run-time function ARRAYDIM is
   performed.

   BOUNDABLE_DIM and DIM must be of type sizetype.  They are both one-based.

   The parse tree node P is used as the source for the filename and line number
   passed to the run-time function.  */

static tree
a68_boundable_dim_check (NODE_T *p, tree boundable_dim, tree dim)
{
  boundable_dim = save_expr (boundable_dim);
  dim = save_expr (dim);

  /* Build the call to ARRAYDIM. */
  unsigned int lineno = NUMBER (LINE (INFO (p)));
  const char *filename_str = FILENAME (LINE (INFO (p)));
  tree filename = build_string_literal (strlen (filename_str) + 1,
					filename_str);
  tree call = a68_build_libcall (A68_LIBCALL_ARRAYDIM,
				 void_type_node, 4,
				 filename,
				 build_int_cst (unsigned_type_node, lineno),
				 boundable_dim, dim);
  call = fold_build2 (COMPOUND_EXPR, a68_bool_type, call, boolean_false_node);

  tree dim_check = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
				fold_build2 (GT_EXPR, boolean_type_node, dim, size_zero_node),
				fold_build2 (LE_EXPR, boolean_type_node, dim, boundable_dim));
  return fold_build2_loc (a68_get_node_location (p),
			  TRUTH_ORIF_EXPR,
			  ssizetype,
			  dim_check, call);
}

/* Return a tree that checks whether the given DIM is a valid dimension/rank of
   the given rows value ROWS.

   DIM is a sizetype.
   The parse tree node P is used as the source for the filename and line
   number.  */

tree
a68_rows_dim_check (NODE_T *p, tree rows, tree dim)
{
  return a68_boundable_dim_check (p, a68_rows_dim (rows), dim);
}

/* Return a tree that checks whether the given DIM is a valid dimension/rank of
   the given multiple value MULTIPLE.

   DIM is a sizetype.
   The parse tree node P is used as the source for the filename and line
   number.  */

tree
a68_multiple_dim_check (NODE_T *p, tree multiple, tree dim)
{
  return a68_boundable_dim_check (p, a68_multiple_dimensions (multiple), dim);
}

/* Return a tree that checks whether the given INDEX falls within the bounds of
   MULTIPLE in the rank DIM.  If the provided index is out of bounds then a
   call to the run-time function ARRAYBOUNDS is performed.

   DIM must be a sizetype.
   MULTIPLE must be a multiple value.
   INDEX must be a ssizetype.

   The parse tree node P is used as the source for the filename and line number
   passed to the run-time function.  */

tree
a68_multiple_bounds_check (NODE_T *p, tree dim,
			   tree multiple, tree index)
{
  index = save_expr (index);
  multiple = save_expr (multiple);

  tree upper_bound = a68_multiple_upper_bound (multiple, dim);
  tree lower_bound = a68_multiple_lower_bound (multiple, dim);

  /* Build the call to ARRAYBOUNDS. */
  unsigned int lineno = NUMBER (LINE (INFO (p)));
  const char *filename_str = FILENAME (LINE (INFO (p)));
  tree filename = build_string_literal (strlen (filename_str) + 1,
					filename_str);
  tree call = a68_build_libcall (A68_LIBCALL_ARRAYBOUNDS,
				 void_type_node, 5,
				 filename,
				 build_int_cst (unsigned_type_node, lineno),
				 fold_convert (ssizetype, index),
				 fold_convert (ssizetype, lower_bound),
				 fold_convert (ssizetype, upper_bound));
  call = fold_build2 (COMPOUND_EXPR, a68_bool_type, call, boolean_false_node);

  /* If LB > UB, the dimension contains no elements.
     Otherwise, it must hold IDX >= LB && IDX <= UB */
  tree bounds_check = fold_build2 (TRUTH_AND_EXPR, sizetype,
				   fold_build2 (LE_EXPR, ssizetype,
						lower_bound, upper_bound),
				   fold_build2 (TRUTH_AND_EXPR,
						boolean_type_node,
						fold_build2 (GE_EXPR, ssizetype,
							     fold_convert (ssizetype,
									   index),
							     lower_bound),
						fold_build2 (LE_EXPR, ssizetype,
							     fold_convert (ssizetype,
									   index),
							     upper_bound)));
  return fold_build2_loc (a68_get_node_location (p),
			  TRUTH_ORIF_EXPR,
			  ssizetype,
			  bounds_check, call);
}

/* Emit a run-time error if the bounds of M1 and M2 are not the same.  Both
   multiples are assumed to have the same type and therefore feature the same
   number of dimensions.  */

tree
a68_multiple_bounds_check_equal (NODE_T *p, tree m1, tree m2)
{
  m1 = save_expr (m1);
  m2 = save_expr (m2);

  /* First determine the rank of the multiples and check they match.  */
  tree m1_dimensions = a68_multiple_dimensions (m1);
  tree m2_dimensions = a68_multiple_dimensions (m2);
  gcc_assert (TREE_CODE (m1_dimensions) == INTEGER_CST
	      && TREE_CODE (m2_dimensions) == INTEGER_CST);

  int dim1 = tree_to_shwi (m1_dimensions);
  int dim2 = tree_to_shwi (m2_dimensions);
  gcc_assert (dim1 == dim2);

  a68_push_range (NULL /* VOID */);

  /* For each dimension, check that bounds are the same in both multiples.  */
  int i;
  for (i = 0; i < dim1; ++i)
    {
      tree dim_tree = build_int_cst (ssizetype, i);
      tree dim_plus_one = fold_build2 (PLUS_EXPR, ssizetype,
				       dim_tree,
				       fold_convert (ssizetype, size_one_node));

      tree lb1 = save_expr (a68_multiple_lower_bound (m1, dim_tree));
      tree lb2 = save_expr (a68_multiple_lower_bound (m2, dim_tree));

      tree ub1 = save_expr (a68_multiple_upper_bound (m1, dim_tree));
      tree ub2 = save_expr (a68_multiple_upper_bound (m2, dim_tree));

      tree bounds_equal = fold_build2 (TRUTH_AND_EXPR,
				       boolean_type_node,
				       fold_build2 (EQ_EXPR, boolean_type_node,
						    lb1, lb2),
				       fold_build2 (EQ_EXPR, boolean_type_node,
						    ub1, ub2));

      unsigned int lineno = NUMBER (LINE (INFO (p)));
      const char *filename_str = FILENAME (LINE (INFO (p)));
      tree filename = build_string_literal (strlen (filename_str) + 1,
					    filename_str);
      tree call = a68_build_libcall (A68_LIBCALL_ARRAYBOUNDSMISMATCH,
				     void_type_node, 7,
				     filename,
				     build_int_cst (unsigned_type_node, lineno),
				     dim_plus_one,
				     lb1, ub1, lb2, ub2);
      call = fold_build2 (COMPOUND_EXPR, boolean_type_node, call, boolean_false_node);

      tree check = fold_build2_loc (a68_get_node_location (p),
				    TRUTH_ORIF_EXPR, boolean_type_node,
				    bounds_equal,
				    call);
      a68_add_stmt (check);
    }

  return a68_pop_range ();
}

/* Allocate a multiple on the heap.

   M is the mode the multiple to allocate.
   DIM is the number of dimensions of the multiple.
   ELEMS is a pointer to the elements of the multiple.
   ELEMS_SIZE is the size in bytes of ELEMS.
   *LOWER_BOUND and *UPPER_BOUND are the bounds for the DIM dimensions.  */

tree
a68_row_malloc (tree type, int dim, tree elems, tree elems_size,
		tree *lower_bound, tree *upper_bound)
{
  tree ptr_to_type = build_pointer_type (type);

  a68_push_range (NULL);

  /* Allocate space for the descriptor.  */
  tree ptr_to_multiple = a68_lower_tmpvar ("ptr_to_multiple%", ptr_to_type,
					   a68_lower_malloc (type, size_in_bytes (type)));
  tree multiple = a68_row_value (type, dim,
				 elems, elems_size,
				 lower_bound, upper_bound);
  a68_add_stmt (fold_build2 (MODIFY_EXPR, void_type_node,
			     fold_build1 (INDIRECT_REF, type, ptr_to_multiple),
			     multiple));
  a68_add_stmt (ptr_to_multiple);
  tree res = a68_pop_range ();
  TREE_TYPE (res) = ptr_to_type;
  return res;
}
